{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Helpers for the generation of the operation functions
module OpenAPI.Generate.Internal.Operation
  ( getResponseObject,
    getResponseSchema,
    defineOperationFunction,
    getParameterDescription,
    getParameterType,
    getParametersTypeForSignature,
    getParametersTypeForSignatureWithMonadTransformer,
    getOperationName,
    getOperationDescription,
    getParametersFromOperationConcrete,
    getBodySchemaFromOperation,
    generateParameterizedRequestPath,
    generateQueryParams,
    RequestBodyDefinition (..),
  )
where

import Control.Monad
import qualified Control.Monad.Reader as MR
import qualified Data.ByteString.Char8 as B8
import qualified Data.Char as Char
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.PprLib hiding ((<>))
import qualified Network.HTTP.Simple as HS
import qualified Network.HTTP.Types as HT
import qualified OpenAPI.Common as OC
import qualified OpenAPI.Generate.Doc as Doc
import qualified OpenAPI.Generate.Flags as OAF
import OpenAPI.Generate.Internal.Util
import qualified OpenAPI.Generate.Model as Model
import qualified OpenAPI.Generate.Monad as OAM
import qualified OpenAPI.Generate.Types as OAT
import qualified OpenAPI.Generate.Types.Schema as OAS

-- | Extracted request body information which can be used for code generation
data RequestBodyDefinition
  = RequestBodyDefinition
      { schema :: OAT.Schema,
        encoding :: OC.RequestBodyEncoding,
        required :: Bool
      }

-- | wrapper for ambigious usage
getParametersFromOperationReference :: OAT.OperationObject -> [OAT.Referencable OAT.ParameterObject]
getParametersFromOperationReference = OAT.parameters

-- | wrapper for ambigious usage
getSchemaFromParameterInner :: OAT.ParameterObject -> OAT.ParameterObjectSchema
getSchemaFromParameterInner = OAT.schema

-- | wrapper for ambigious usage
getRequiredFromParameter :: OAT.ParameterObject -> Bool
getRequiredFromParameter = OAT.required

-- | wrapper for ambigious usage
getInFromParameterObject :: OAT.ParameterObject -> OAT.ParameterObjectLocation
getInFromParameterObject = OAT.in'

-- | Extracts all parameters of an operation
--
-- Concrete objects are always added. References try to get resolved to a concrete object.
-- If this fails, the parameter is skipped and a warning gets produced.
getParametersFromOperationConcrete :: OAT.OperationObject -> OAM.Generator [OAT.ParameterObject]
getParametersFromOperationConcrete =
  OAM.nested "parameters"
    . fmap Maybe.catMaybes
    . mapM
      ( \case
          OAT.Concrete p -> pure $ Just p
          OAT.Reference ref -> do
            p <- OAM.getParameterReferenceM ref
            if Maybe.isJust p
              then pure p
              else do
                OAM.logWarning $ "Reference " <> ref <> " to ParameterObject could not be found and therefore will be skipped."
                pure p
      )
    . getParametersFromOperationReference

-- | Reads the Schema from the ParameterObjectSchema
--   Only application/json or simple Object Schema is read
getSchemaFromParameterOuter :: OAT.ParameterObjectSchema -> OAS.SchemaObject
getSchemaFromParameterOuter OAT.SimpleParameterObjectSchema {..} = case schema of
  OAT.Concrete e -> e
  _ -> error "not yet implemented"
getSchemaFromParameterOuter _ = error "not yet implemented"

-- | Reads the Schema from the Parameter
getSchemaFromParameter :: OAT.ParameterObject -> OAS.SchemaObject
getSchemaFromParameter = getSchemaFromParameterOuter . getSchemaFromParameterInner

getNameFromParameter :: OAT.ParameterObject -> Text
getNameFromParameter = OAT.name

-- | Gets the Type definition dependent on the number of parameters/types
--   A monadic name for which its forall structure is defined outside
--   this function can be given
--
--   @
--     [t|OC.Configuration -> Int -> $(varT monadName) ($(responseType) $(responseInnerType))|]
--       = getParametersTypeForSignature [conT ''Int] (monadName)
--   @
getParametersTypeForSignature :: [Q Type] -> Name -> Name -> Name -> Q Type
getParametersTypeForSignature types responseTypeName monadName securitySchemeName =
  createFunctionType
    ( [t|OC.Configuration $(varT securitySchemeName)|]
        : types
        <> [[t|$(varT monadName) (Either HS.HttpException (HS.Response $(varT responseTypeName)))|]]
    )

-- | Same as 'getParametersTypeForSignature' but with the configuration in 'MR.ReaderT' instead of a parameter
getParametersTypeForSignatureWithMonadTransformer :: [Q Type] -> Name -> Name -> Name -> Q Type
getParametersTypeForSignatureWithMonadTransformer types responseTypeName monadName securitySchemeName =
  createFunctionType
    ( types
        <> [[t|MR.ReaderT (OC.Configuration $(varT securitySchemeName)) $(varT monadName) (Either HS.HttpException (HS.Response $(varT responseTypeName)))|]]
    )

createFunctionType :: [Q Type] -> Q Type
createFunctionType =
  foldr1
    (\t1 t2 -> [t|$t1 -> $t2|])

getParameterName :: OAT.ParameterObject -> OAM.Generator Name
getParameterName parameter = haskellifyNameM False $ getNameFromParameter parameter

-- | Get the type of a parameter depending on its schema type and the configuration options ('OAF.Flags').
-- If the parameter is not required, a 'Maybe' type is produced.
getParameterType :: OAF.Flags -> OAT.ParameterObject -> Q Type
getParameterType flags parameter =
  let paramType = varT $ Model.getSchemaType flags (getSchemaFromParameter parameter)
   in ( if getRequiredFromParameter parameter
          then paramType
          else [t|Maybe $(paramType)|]
      )

-- | Get a description of a parameter object (the name and if available the description from the specification)
getParameterDescription :: OAT.ParameterObject -> OAM.Generator Text
getParameterDescription parameter = do
  schema <- Model.resolveSchemaReferenceWithoutWarning $ OAT.schema (OAT.schema (parameter :: OAT.ParameterObject) :: OAT.ParameterObjectSchema)
  let name = OAT.name (parameter :: OAT.ParameterObject)
      description = maybe "" (": " <>) $ OAT.description (parameter :: OAT.ParameterObject)
      constraints = joinWith ", " $ Model.getConstraintDescriptionsOfSchema schema
  pure $ Doc.escapeText $ name <> description <> (if T.null constraints then "" else " | Constraints: " <> constraints)

-- | Defines the body of an Operation function
--   The Operation function calls an generall HTTP function
--   all Parameters are arguments to the function
defineOperationFunction ::
  -- | Should the configuration be passed explicitly as parameter?
  Bool ->
  -- | How the function should be called
  Name ->
  -- | The parameters
  [OAT.ParameterObject] ->
  -- | The request path. It may contain placeholders in the form /my/{var}/path/
  Text ->
  -- | HTTP Method (POST,GET,etc.)
  Text ->
  -- | Schema of body
  Maybe RequestBodyDefinition ->
  -- | An expression used to transform the response from 'B8.ByteString' to the required response type.
  -- Note that the response is nested within a HTTP monad and an 'Either'.
  Q Exp ->
  -- | Function body definition in TH
  OAM.Generator (Q Doc)
defineOperationFunction useExplicitConfiguration fnName params requestPath method bodySchema responseTransformerExp = do
  paramVarNames <- mapM getParameterName params
  let configArg = mkName "config"
      paraPattern = varP <$> paramVarNames
      fnPatterns = if useExplicitConfiguration then varP configArg : paraPattern else paraPattern
      namedParameters = zip paramVarNames params
      namedPathParameters = filter ((== OAT.PathParameterObjectLocation) . getInFromParameterObject . snd) namedParameters
      request = generateParameterizedRequestPath namedPathParameters requestPath
      namedQueryParameters = filter ((== OAT.QueryParameterObjectLocation) . getInFromParameterObject . snd) namedParameters
      queryParameters = generateQueryParams namedQueryParameters
      bodyName = mkName "body"
      methodLit = litE $ stringL $ T.unpack method
  pure $
    ppr <$> case bodySchema of
      Just RequestBodyDefinition {..} ->
        let encodeExpr =
              varE $
                case encoding of
                  OC.RequestBodyEncodingFormData -> 'OC.RequestBodyEncodingFormData
                  OC.RequestBodyEncodingJSON -> 'OC.RequestBodyEncodingJSON
         in [d|
              $(conP fnName $ fnPatterns <> [varP bodyName]) =
                $responseTransformerExp
                  ( $( if useExplicitConfiguration
                         then [|OC.doBodyCallWithConfiguration $(varE configArg)|]
                         else [|OC.doBodyCallWithConfigurationM|]
                     )
                    (T.toUpper $ T.pack $methodLit)
                    (T.pack $(request))
                    $(queryParameters)
                    $(if required then [|Just $(varE bodyName)|] else varE bodyName)
                    $(encodeExpr)
                  )
              |]
      Nothing ->
        [d|
          $(conP fnName fnPatterns) =
            $responseTransformerExp
              ( $( if useExplicitConfiguration
                     then [|OC.doCallWithConfiguration $(varE configArg)|]
                     else [|OC.doCallWithConfigurationM|]
                 )
                (T.toUpper $ T.pack $methodLit)
                (T.pack $(request))
                $(queryParameters)
              )
          |]

-- | Extracts the request body schema from an operation and the encoding which should be used on the body data.
getBodySchemaFromOperation :: OAT.OperationObject -> OAM.Generator (Maybe RequestBodyDefinition)
getBodySchemaFromOperation operation = do
  requestBody <- getRequestBodyObject operation
  case requestBody of
    Just body -> getRequestBodySchema body
    Nothing -> pure Nothing

getRequestBodyContent :: OAT.RequestBodyObject -> Map.Map Text OAT.MediaTypeObject
getRequestBodyContent = OAT.content

getSchemaFromMedia :: OAT.MediaTypeObject -> Maybe OAT.Schema
getSchemaFromMedia = OAT.schema

getRequestBodySchema :: OAT.RequestBodyObject -> OAM.Generator (Maybe RequestBodyDefinition)
getRequestBodySchema body =
  let content = Map.lookup "application/json" $ getRequestBodyContent body
      createRequestBodyDefinition encoding schema =
        Just $
          RequestBodyDefinition
            { schema = schema,
              encoding = encoding,
              required = OAT.required (body :: OAT.RequestBodyObject)
            }
   in case content of
        Nothing ->
          let formContent = Map.lookup "application/x-www-form-urlencoded" $ getRequestBodyContent body
           in case formContent of
                Nothing -> do
                  OAM.logWarning "Only content type application/json and application/x-www-form-urlencoded is supported"
                  pure Nothing
                Just media ->
                  pure $
                    getSchemaFromMedia media
                      >>= createRequestBodyDefinition OC.RequestBodyEncodingFormData
        Just media ->
          pure $
            getSchemaFromMedia media
              >>= createRequestBodyDefinition OC.RequestBodyEncodingJSON

getRequestBodyObject :: OAT.OperationObject -> OAM.Generator (Maybe OAT.RequestBodyObject)
getRequestBodyObject operation =
  case OAT.requestBody operation of
    Nothing -> pure Nothing
    Just (OAT.Concrete p) -> pure $ Just p
    Just (OAT.Reference ref) -> do
      p <- OAM.getRequestBodyReferenceM ref
      when (Maybe.isNothing p) $ OAM.logWarning $ "Reference " <> ref <> " to RequestBody could not be found and therefore will be skipped."
      pure p

-- | Extracts the response 'OAT.Schema' from a 'OAT.ResponseObject'.
--
-- A warning is logged if the response does not contain one of the supported media types.
getResponseSchema :: OAT.ResponseObject -> OAM.Generator (Maybe OAT.Schema)
getResponseSchema response = do
  let contentMap = OAT.content (response :: OAT.ResponseObject)
      schema = Map.lookup "application/json" contentMap >>= getSchemaFromMedia
  when (Maybe.isNothing schema && not (Map.null contentMap)) $ OAM.logWarning "Only content type application/json is supported for response bodies."
  pure schema

-- | Resolve a possibly referenced response to a concrete value.
--
-- A warning is logged if the reference is not found.
getResponseObject :: OAT.Referencable OAT.ResponseObject -> OAM.Generator (Maybe OAT.ResponseObject)
getResponseObject (OAT.Concrete p) = pure $ Just p
getResponseObject (OAT.Reference ref) = do
  p <- OAM.getResponseReferenceM ref
  when (Maybe.isNothing p) $ OAM.logWarning $ "Reference " <> ref <> " to response could not be found and therefore will be skipped."
  pure p

-- | Generates query params in the form of [(Text,ByteString)]
generateQueryParams :: [(Name, OAT.ParameterObject)] -> Q Exp
generateQueryParams ((name, param) : xs) =
  infixE (Just [|(T.pack queryName, $(expr))|]) (varE $ mkName ":") (Just $ generateQueryParams xs)
  where
    queryName = T.unpack $ getNameFromParameter param
    required = getRequiredFromParameter param
    expr =
      if required
        then [|$(varE $ mkName "GHC.Base.Just") $ OC.stringifyModel $(varE name)|]
        else [|OC.stringifyModel <$> $(varE name)|]
generateQueryParams _ = [|[]|]

-- | Resolves placeholders in paths with dynamic expressions
--
--   "my/{var}/path" -> "my" ++ myVar ++ "/path"
--
--   If the placeholder is at the end or at the beginning an empty string gets appended
generateParameterizedRequestPath :: [(Name, OAT.ParameterObject)] -> Text -> Q Exp
generateParameterizedRequestPath ((paramName, param) : xs) path =
  foldr1 (foldingFn paramName) partExpressiones
  where
    parts = Split.splitOn ("{" <> T.unpack (getNameFromParameter param) <> "}") (T.unpack path)
    partExpressiones = generateParameterizedRequestPath xs . T.pack <$> parts
    foldingFn :: Name -> Q Exp -> Q Exp -> Q Exp
    foldingFn var a b = [|$(a) ++ B8.unpack (HT.urlEncode True $ B8.pack $ OC.stringifyModel $(varE var)) ++ $(b)|]
generateParameterizedRequestPath _ path = litE (stringL $ T.unpack path)

-- | Extracts a description from an 'OAT.OperationObject'.
-- If available, the description is used, the summary otherwise.
-- If neither is available, an empty description is used.
getOperationDescription :: OAT.OperationObject -> Text
getOperationDescription operation =
  Maybe.fromMaybe "" $ Maybe.listToMaybe $
    Maybe.catMaybes
      [ OAT.description (operation :: OAT.OperationObject),
        OAT.summary (operation :: OAT.OperationObject)
      ]

-- | Constructs the name of an operation.
-- If an 'OAT.operationId' is available, this is the primary choice.
-- If it is not available, the id is constructed based on the request path and method.
getOperationName :: Text -> Text -> OAT.OperationObject -> OAM.Generator Name
getOperationName requestPath method operation =
  let operationId = OAT.operationId operation
      textName = Maybe.fromMaybe (T.map Char.toLower method <> requestPath) operationId
   in haskellifyNameM False textName
