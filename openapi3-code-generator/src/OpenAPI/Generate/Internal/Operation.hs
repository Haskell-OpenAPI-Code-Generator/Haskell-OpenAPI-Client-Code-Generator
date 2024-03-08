{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Helpers for the generation of the operation functions
module OpenAPI.Generate.Internal.Operation
  ( getResponseObject,
    getResponseSchema,
    defineOperationFunction,
    getParameterDescription,
    generateParameterTypeFromOperation,
    getParametersTypeForSignature,
    getParametersTypeForSignatureWithMonadTransformer,
    getOperationName,
    getOperationDescription,
    getBodySchemaFromOperation,
    generateParameterizedRequestPath,
    generateQueryParams,
    shouldGenerateRequestBody,
    RequestBodyDefinition (..),
    ParameterTypeDefinition (..),
    ParameterCardinality (..),
  )
where

import Control.Monad
import qualified Data.Aeson as Aeson
import qualified Data.Bifunctor as BF
import qualified Data.Char as Char
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.PprLib hiding ((<>))
import qualified Network.HTTP.Simple as HS
import qualified Network.HTTP.Types as HT
import qualified OpenAPI.Common as OC
import qualified OpenAPI.Generate.Doc as Doc
import OpenAPI.Generate.Internal.Util
import qualified OpenAPI.Generate.Model as Model
import qualified OpenAPI.Generate.ModelDependencies as Dep
import qualified OpenAPI.Generate.Monad as OAM
import qualified OpenAPI.Generate.OptParse as OAO
import OpenAPI.Generate.OptParse.Types
import qualified OpenAPI.Generate.Types as OAT
import qualified OpenAPI.Generate.Types.Schema as OAS

-- | Extracted request body information which can be used for code generation
data RequestBodyDefinition = RequestBodyDefinition
  { requestBodyDefinitionSchema :: OAT.Schema,
    requestBodyDefinitionEncoding :: OC.RequestBodyEncoding,
    requestBodyDefinitionRequired :: Bool
  }

-- | Defines the type of a parameter bundle including the information to access the specific parameters
data ParameterTypeDefinition = ParameterTypeDefinition
  { parameterTypeDefinitionType :: Q Type,
    parameterTypeDefinitionDoc :: Q Doc,
    parameterTypeDefinitionDependencies :: Dep.Models,
    parameterTypeDefinitionQueryParams :: [(Name, OAT.ParameterObject)],
    parameterTypeDefinitionPathParams :: [(Name, OAT.ParameterObject)]
  }

-- | Represents the number of (supported) parameters and the generated types which result of it
--
-- * No type is generated when no parameters are present
-- * Only the type of the parameter is generated if a single parameter is present
-- * A combined parameter type is generated for multiple parameters
data ParameterCardinality
  = NoParameters
  | SingleParameter (Q Type) Dep.ModelContentWithDependencies OAT.ParameterObject
  | MultipleParameters ParameterTypeDefinition

-- | Generates the parameter type for an operation. See 'ParameterCardinality' for further information.
generateParameterTypeFromOperation :: Text -> OAT.OperationObject -> OAM.Generator ParameterCardinality
generateParameterTypeFromOperation operationName = getParametersFromOperationConcrete >=> generateParameterType operationName

generateParameterType :: Text -> [(OAT.ParameterObject, [Text])] -> OAM.Generator ParameterCardinality
generateParameterType operationName parameters = OAM.nested "parameters" $ do
  maybeSchemas <- mapM (\(p, path) -> OAM.resetPath path $ getSchemaFromParameter p) parameters
  parametersSuffix <- OAM.getSetting OAO.settingParametersTypeSuffix
  let parametersWithSchemas =
        [ ((parameter, path), mergeDescriptionOfParameterWithSchema parameter schema)
          | ((parameter, path), Just schema) <-
              zip parameters maybeSchemas,
            OAT.parameterObjectIn parameter `elem` [OAT.QueryParameterObjectLocation, OAT.PathParameterObjectLocation]
        ]
      schemaName = operationName <> parametersSuffix
  when (length parameters > length parametersWithSchemas) $ OAM.logWarning "Parameters are only supported in query and path (skipping parameters in cookie and header)."
  case parametersWithSchemas of
    [] -> pure NoParameters
    [((parameter, path), schema)] -> do
      -- TODO disable fixed value generation for parameters
      (paramType, model) <- OAM.resetPath (path <> ["schema"]) $ Model.defineModelForSchemaNamed (schemaName <> uppercaseFirstText (OAT.parameterObjectName parameter)) schema
      pure $
        SingleParameter
          ( if OAT.parameterObjectRequired parameter
              then paramType
              else [t|Maybe $(paramType)|]
          )
          model
          parameter
    _ -> do
      properties <-
        mapM
          ( \((parameter, _), schema) -> do
              prefix <- getParameterLocationPrefix parameter
              pure (prefix <> uppercaseFirstText (OAT.parameterObjectName parameter), schema)
          )
          parametersWithSchemas
      let parametersWithNames = zip (fst <$> properties) (fst <$> parametersWithSchemas)
          requiredProperties =
            Set.fromList $
              fst <$> filter (OAT.parameterObjectRequired . fst . snd) parametersWithNames
      (parameterTypeDefinitionType, (parameterTypeDefinitionDoc, parameterTypeDefinitionDependencies)) <-
        -- Explicitly include fixed value properties since this is not
        -- a user defined object schema but one that is defined by the
        -- generator and is only here to make the usage easier.
        -- It should therefore not change the semantics.
        OAM.adjustSettings (\settings -> settings {OAO.settingFixedValueStrategy = FixedValueStrategyInclude}) $
          Model.defineModelForSchemaNamed schemaName $
            OAT.Concrete $
              OAS.defaultSchema {OAS.schemaObjectProperties = Map.fromList properties, OAS.schemaObjectRequired = requiredProperties}
      convertToCamelCase <- OAM.getSetting OAO.settingConvertToCamelCase
      let parametersWithPropertyNames = BF.bimap (haskellifyName convertToCamelCase False . (schemaName <>) . uppercaseFirstText) fst <$> parametersWithNames
          filterByType t = filter ((== t) . OAT.parameterObjectIn . snd) parametersWithPropertyNames
          parameterTypeDefinitionQueryParams = filterByType OAT.QueryParameterObjectLocation
          parameterTypeDefinitionPathParams = filterByType OAT.PathParameterObjectLocation
      pure $ MultipleParameters ParameterTypeDefinition {..}

mergeDescriptionOfParameterWithSchema :: OAT.ParameterObject -> OAS.Schema -> OAS.Schema
mergeDescriptionOfParameterWithSchema parameter (OAT.Concrete schema) =
  let parameterName = OAT.parameterObjectName parameter
      descriptionParameter = Maybe.maybeToList $ OAT.parameterObjectDescription parameter
      descriptionSchema = Maybe.maybeToList $ OAS.schemaObjectDescription schema
      mergedDescription = T.intercalate "\n\n" (("Represents the parameter named '" <> parameterName <> "'") : descriptionParameter <> descriptionSchema)
   in OAT.Concrete $ schema {OAS.schemaObjectDescription = Just mergedDescription}
mergeDescriptionOfParameterWithSchema _ schema = schema

getParameterLocationPrefix :: OAT.ParameterObject -> OAM.Generator Text
getParameterLocationPrefix =
  ( \case
      OAT.QueryParameterObjectLocation -> OAM.getSetting OAO.settingParameterQueryPrefix
      OAT.PathParameterObjectLocation -> OAM.getSetting OAO.settingParameterPathPrefix
      OAT.CookieParameterObjectLocation -> OAM.getSetting OAO.settingParameterCookiePrefix
      OAT.HeaderParameterObjectLocation -> OAM.getSetting OAO.settingParameterHeaderPrefix
  )
    . OAT.parameterObjectIn

-- | Extracts all parameters of an operation
--
-- Concrete objects are always added. References try to get resolved to a concrete object.
-- If this fails, the parameter is skipped and a warning gets produced.
getParametersFromOperationConcrete :: OAT.OperationObject -> OAM.Generator [(OAT.ParameterObject, [Text])]
getParametersFromOperationConcrete =
  OAM.nested "parameters"
    . fmap Maybe.catMaybes
    . mapM
      ( \case
          (i, OAT.Concrete p) -> do
            path <- OAM.appendToPath ["[" <> T.pack (show i) <> "]"]
            pure $ Just (p, path)
          (i, OAT.Reference ref) -> OAM.nested ("[" <> T.pack (show i) <> "]") $ do
            p <- OAM.getParameterReferenceM ref
            when (Maybe.isNothing p) $ OAM.logWarning $ "Reference " <> ref <> " to parameter could not be found and therefore will be skipped."
            let name = T.replace "#/components/parameters/" "" ref
            pure $ (,["components", "parameters", name]) <$> p
      )
    . zip ([0 ..] :: [Int])
    . OAT.operationObjectParameters

getSchemaFromParameterObjectSchema :: OAT.ParameterObjectSchema -> OAM.Generator (Maybe OAS.Schema)
getSchemaFromParameterObjectSchema (OAT.SimpleParameterObjectSchema OAT.SimpleParameterSchema {..}) = pure $ Just simpleParameterSchemaSchema
getSchemaFromParameterObjectSchema (OAT.ComplexParameterObjectSchema _) = OAM.nested "content" $ do
  OAM.logWarning "Complex parameter schemas are not supported and therefore will be skipped."
  pure Nothing

-- | Reads the schema from the parameter
getSchemaFromParameter :: OAT.ParameterObject -> OAM.Generator (Maybe OAS.Schema)
getSchemaFromParameter = getSchemaFromParameterObjectSchema . OAT.parameterObjectSchema

-- | Gets the Type definition dependent on the number of parameters/types
--   A monadic name for which its forall structure is defined outside
--   this function can be given
--
--   @
--     [t|OC.Configuration -> Int -> $(varT monadName) ($(responseType) $(responseInnerType))|]
--       = getParametersTypeForSignature [conT ''Int] (monadName)
--   @
getParametersTypeForSignature :: [Q Type] -> Name -> Name -> Q Type
getParametersTypeForSignature types responseTypeName monadName =
  createFunctionType
    ( [t|OC.Configuration|]
        : types
          <> [[t|$(varT monadName) (HS.Response $(varT responseTypeName))|]]
    )

-- | Same as 'getParametersTypeForSignature' but with the configuration in 'MR.ReaderT' instead of a parameter
getParametersTypeForSignatureWithMonadTransformer :: [Q Type] -> Name -> Name -> Q Type
getParametersTypeForSignatureWithMonadTransformer types responseTypeName monadName =
  createFunctionType
    ( types
        <> [[t|OC.ClientT $(varT monadName) (HS.Response $(varT responseTypeName))|]]
    )

createFunctionType :: [Q Type] -> Q Type
createFunctionType =
  foldr1
    (\t1 t2 -> [t|$t1 -> $t2|])

getParameterName :: OAT.ParameterObject -> OAM.Generator Name
getParameterName parameter = haskellifyNameM False $ OAT.parameterObjectName parameter

-- | Get a description of a parameter object (the name and if available the description from the specification)
getParameterDescription :: OAT.ParameterObject -> OAM.Generator Text
getParameterDescription parameter = do
  schema <- case getSchemaOfParameterObject parameter of
    Just schema -> Model.resolveSchemaReferenceWithoutWarning schema
    Nothing -> pure Nothing
  let name = OAT.parameterObjectName parameter
      description = maybe "" (": " <>) $ OAT.parameterObjectDescription parameter
      constraints = joinWith ", " $ Model.getConstraintDescriptionsOfSchema schema
  pure $ Doc.escapeText $ name <> description <> (if T.null constraints then "" else " | Constraints: " <> constraints)

getSchemaOfParameterObject :: OAT.ParameterObject -> Maybe OAS.Schema
getSchemaOfParameterObject parameterObject = case OAT.parameterObjectSchema parameterObject of
  (OAT.SimpleParameterObjectSchema OAT.SimpleParameterSchema {..}) -> Just simpleParameterSchemaSchema
  OAT.ComplexParameterObjectSchema _ -> Nothing

-- | Defines the body of an Operation function
--   The Operation function calls an generall HTTP function
--   all Parameters are arguments to the function
defineOperationFunction ::
  -- | Should the configuration be passed explicitly as parameter?
  Bool ->
  -- | How the function should be called
  Name ->
  -- | The parameters
  ParameterCardinality ->
  -- | The request path. It may contain placeholders in the form /my/{var}/path/
  Text ->
  -- | HTTP Method (POST,GET,etc.)
  Text ->
  -- | Schema of body
  Maybe RequestBodyDefinition ->
  -- | An expression used to transform the response from 'BS.ByteString' to the required response type.
  -- Note that the response is nested within a HTTP monad and an 'Either'.
  Q Exp ->
  -- | Function body definition in TH
  OAM.Generator (Q Doc)
defineOperationFunction useExplicitConfiguration fnName parameterCardinality requestPath method bodySchema responseTransformerExp = do
  let configName = mkName "config"
      paramName = mkName "parameters"
      bodyName = mkName "body"
  paraPattern <- case parameterCardinality of
    NoParameters -> pure []
    SingleParameter _ _ parameter -> do
      paramName' <- getParameterName parameter
      pure [varP paramName']
    MultipleParameters _ -> pure [varP paramName]
  (pathParameters, queryParameters) <- case parameterCardinality of
    NoParameters -> pure ([], [])
    SingleParameter _ _ parameter -> do
      paramName' <- getParameterName parameter
      let paramExpr = (varE paramName', parameter)
      pure $
        if OAT.parameterObjectIn parameter == OAT.PathParameterObjectLocation
          then ([paramExpr], [])
          else ([], [paramExpr])
    MultipleParameters paramDefinition ->
      let toParamExpr f = BF.first (\name -> [|$(varE name) $(varE paramName)|]) <$> f paramDefinition
       in pure (toParamExpr parameterTypeDefinitionPathParams, toParamExpr parameterTypeDefinitionQueryParams)
  let queryParameters' = generateQueryParams queryParameters
      request = generateParameterizedRequestPath pathParameters requestPath
      methodLit = stringE $ T.unpack method
      fnPatterns = if useExplicitConfiguration then varP configName : paraPattern else paraPattern
  generateBody <- shouldGenerateRequestBody bodySchema
  pure $
    ppr <$> case bodySchema of
      Just RequestBodyDefinition {..}
        | generateBody ->
            let encodeExpr =
                  varE $
                    case requestBodyDefinitionEncoding of
                      OC.RequestBodyEncodingFormData -> 'OC.RequestBodyEncodingFormData
                      OC.RequestBodyEncodingJSON -> 'OC.RequestBodyEncodingJSON
             in [d|
                  $(conP fnName $ fnPatterns <> [varP bodyName]) =
                    $responseTransformerExp
                      ( $( if useExplicitConfiguration
                             then [|OC.doBodyCallWithConfiguration $(varE configName)|]
                             else [|OC.doBodyCallWithConfigurationM|]
                         )
                          (T.toUpper $ T.pack $methodLit)
                          $(request)
                          $(queryParameters')
                          $(if requestBodyDefinitionRequired then [|Just $(varE bodyName)|] else varE bodyName)
                          $(encodeExpr)
                      )
                  |]
      _ ->
        [d|
          $(conP fnName fnPatterns) =
            $responseTransformerExp
              ( $( if useExplicitConfiguration
                     then [|OC.doCallWithConfiguration $(varE configName)|]
                     else [|OC.doCallWithConfigurationM|]
                 )
                  (T.toUpper $ T.pack $methodLit)
                  $(request)
                  $(queryParameters')
              )
          |]

-- | Checks if a request body should be generated based on the CLI options and if the body type is an empty object
shouldGenerateRequestBody :: Maybe RequestBodyDefinition -> OAM.Generator Bool
shouldGenerateRequestBody Nothing = pure False
shouldGenerateRequestBody (Just RequestBodyDefinition {..}) = do
  maybeSchema <- Model.resolveSchemaReferenceWithoutWarning requestBodyDefinitionSchema
  generateEmptyRequestBody <- OAM.getSetting OAO.settingGenerateOptionalEmptyRequestBody
  case maybeSchema of
    Just s
      | not generateEmptyRequestBody
          && not requestBodyDefinitionRequired
          && OAS.isSchemaEmpty s ->
          pure False
    _ -> pure True

-- | Extracts the request body schema from an operation and the encoding which should be used on the body data.
getBodySchemaFromOperation :: OAT.OperationObject -> OAM.Generator (Maybe RequestBodyDefinition, [Text])
getBodySchemaFromOperation operation = OAM.nested "requestBody" $ do
  requestBody <- getRequestBodyObject operation
  case requestBody of
    Just (body, path) -> OAM.resetPath path $ getRequestBodySchema body
    Nothing -> pure (Nothing, [])

getRequestBodySchema :: OAT.RequestBodyObject -> OAM.Generator (Maybe RequestBodyDefinition, [Text])
getRequestBodySchema body = OAM.nested "content" $ do
  let contentMap = OAT.requestBodyObjectContent body
      content = getValueByContentTypeIgnoringCharset "application/json" contentMap
      createRequestBodyDefinition encoding schema =
        Just $
          RequestBodyDefinition
            { requestBodyDefinitionSchema = schema,
              requestBodyDefinitionEncoding = encoding,
              requestBodyDefinitionRequired = OAT.requestBodyObjectRequired body
            }
  case content of
    Nothing ->
      let formContent = getValueByContentTypeIgnoringCharset "application/x-www-form-urlencoded" contentMap
       in case formContent of
            Nothing -> do
              OAM.logWarning "Only content type application/json and application/x-www-form-urlencoded is supported"
              pure (Nothing, [])
            Just media -> do
              path <- OAM.appendToPath ["application/x-www-form-urlencoded", "schema"]
              pure
                ( OAT.mediaTypeObjectSchema media
                    >>= createRequestBodyDefinition OC.RequestBodyEncodingFormData,
                  path
                )
    Just media -> do
      path <- OAM.appendToPath ["application/json", "schema"]
      pure
        ( OAT.mediaTypeObjectSchema media
            >>= createRequestBodyDefinition OC.RequestBodyEncodingJSON,
          path
        )

getRequestBodyObject :: OAT.OperationObject -> OAM.Generator (Maybe (OAT.RequestBodyObject, [Text]))
getRequestBodyObject operation =
  case OAT.operationObjectRequestBody operation of
    Nothing -> pure Nothing
    Just (OAT.Concrete p) -> do
      path <- OAM.getCurrentPath
      pure $ Just (p, path)
    Just (OAT.Reference ref) -> do
      p <- OAM.getRequestBodyReferenceM ref
      when (Maybe.isNothing p) $ OAM.logWarning $ "Reference '" <> ref <> "' to request body could not be found and therefore will be skipped."
      let name = T.replace "#/components/requestBodies/" "" ref
      pure $ (,["components", "requestBodies", name]) <$> p

-- | Extracts the response 'OAT.Schema' from a 'OAT.ResponseObject'.
--
-- A warning is logged if the response does not contain one of the supported media types.
getResponseSchema :: OAT.ResponseObject -> OAM.Generator (Maybe OAT.Schema, [Text])
getResponseSchema response = OAM.nested "content" $ do
  let contentMap = OAT.responseObjectContent response
      schema = getValueByContentTypeIgnoringCharset "application/json" contentMap >>= OAT.mediaTypeObjectSchema
  when (Maybe.isNothing schema && not (Map.null contentMap)) $ OAM.logWarning "Only content type application/json is supported for response bodies."
  path <- OAM.appendToPath ["application/json", "schema"]
  pure (schema, path)

getValueByContentTypeIgnoringCharset :: Text -> Map.Map Text OAT.MediaTypeObject -> Maybe OAT.MediaTypeObject
getValueByContentTypeIgnoringCharset contentType contentMap =
  case Map.lookup contentType contentMap of
    Just content -> Just content
    Nothing -> case Map.elems $ Map.filterWithKey (\key _ -> Maybe.listToMaybe (T.splitOn ";" key) == Just contentType) contentMap of
      [] -> Nothing
      content : _ -> Just content

-- | Resolve a possibly referenced response to a concrete value.
--
-- A warning is logged if the reference is not found.
getResponseObject :: OAT.Referencable OAT.ResponseObject -> OAM.Generator (Maybe (OAT.ResponseObject, [Text]))
getResponseObject (OAT.Concrete p) = do
  path <- OAM.getCurrentPath
  pure $ Just (p, path)
getResponseObject (OAT.Reference ref) = do
  p <- OAM.getResponseReferenceM ref
  when (Maybe.isNothing p) $ OAM.logWarning $ "Reference '" <> ref <> "' to response could not be found and therefore will be skipped."
  let name = T.replace "#/components/responses/" "" ref
  pure $ (,["components", "responses", name]) <$> p

-- | Generates query params in the form of [(Text,ByteString)]
generateQueryParams :: [(Q Exp, OAT.ParameterObject)] -> Q Exp
generateQueryParams [] = [|mempty|]
generateQueryParams x =
  listE
    . fmap
      ( \(var, param) ->
          let queryName = stringE $ T.unpack $ OAT.parameterObjectName param
              required = OAT.parameterObjectRequired param
              (maybeStyle, explode') = case OAT.parameterObjectSchema param of
                (OAT.SimpleParameterObjectSchema OAT.SimpleParameterSchema {..}) -> (simpleParameterSchemaStyle, simpleParameterSchemaExplode)
                OAT.ComplexParameterObjectSchema _ -> (Just "form", True)
              style' =
                stringE $
                  T.unpack $
                    Maybe.fromMaybe
                      ( case OAT.parameterObjectIn param of
                          OAT.QueryParameterObjectLocation -> "form"
                          OAT.HeaderParameterObjectLocation -> "simple"
                          OAT.PathParameterObjectLocation -> "simple"
                          OAT.CookieParameterObjectLocation -> "form"
                      )
                      maybeStyle
              expr =
                if required
                  then [|Just $ Aeson.toJSON $var|]
                  else [|Aeson.toJSON <$> $var|]
           in [|OC.QueryParameter (T.pack $queryName) $expr (T.pack $style') explode'|]
      )
    $ x

-- | Resolves placeholders in paths with dynamic expressions
--
--   "my/{var}/path" -> "my" ++ myVar ++ "/path"
--
--   If the placeholder is at the end or at the beginning an empty string gets appended
generateParameterizedRequestPath :: [(Q Exp, OAT.ParameterObject)] -> Text -> Q Exp
generateParameterizedRequestPath ((paramName, param) : xs) path =
  foldr1 (foldingFn paramName) partExpressiones
  where
    parts = Split.splitOn ("{" <> T.unpack (OAT.parameterObjectName param) <> "}") (T.unpack path)
    partExpressiones = generateParameterizedRequestPath xs . T.pack <$> parts
    foldingFn :: Q Exp -> Q Exp -> Q Exp -> Q Exp
    foldingFn var a b = [|$(a) <> OC.byteToText (HT.urlEncode True $ OC.textToByte $ OC.stringifyModel $var) <> $(b)|]
generateParameterizedRequestPath _ path = stringE $ T.unpack path

-- | Extracts a description from an 'OAT.OperationObject'.
-- If available, the description is used, the summary otherwise.
-- If neither is available, an empty description is used.
getOperationDescription :: OAT.OperationObject -> Text
getOperationDescription operation =
  Maybe.fromMaybe "" $
    Maybe.listToMaybe $
      Maybe.catMaybes
        [ OAT.operationObjectDescription operation,
          OAT.operationObjectSummary operation
        ]

-- | Constructs the name of an operation.
-- If an 'OAT.operationId' is available, this is the primary choice.
-- If it is not available, the id is constructed based on the request path and method.
getOperationName :: Text -> Text -> OAT.OperationObject -> OAM.Generator Name
getOperationName requestPath method operation =
  let operationId = OAT.operationObjectOperationId operation
      textName = Maybe.fromMaybe (T.map Char.toLower method <> requestPath) operationId
   in haskellifyNameM False textName
