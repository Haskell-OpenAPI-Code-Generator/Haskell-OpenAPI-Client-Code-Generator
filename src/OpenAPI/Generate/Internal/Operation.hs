{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenAPI.Generate.Internal.Operation where

import Control.Monad
import qualified Control.Monad.Reader as MR
import qualified Data.ByteString.Char8 as B8
import Data.Char
import qualified Data.List as DL
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.PprLib hiding ((<>))
import Language.Haskell.TH.Syntax
import qualified Network.HTTP.Simple as HS
import qualified Network.HTTP.Types as HT
import qualified OpenAPI.Common as OC
import qualified OpenAPI.Generate.Doc as Doc
import OpenAPI.Generate.Flags
import OpenAPI.Generate.Internal.Util
import qualified OpenAPI.Generate.Model as Model
import qualified OpenAPI.Generate.Monad as OAM
import qualified OpenAPI.Generate.Types as OAT
import qualified OpenAPI.Generate.Types.Schema as OAS
import qualified OpenAPI.Generate.Util as Util

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

getNameFromParameter :: OAT.ParameterObject -> T.Text
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

getParameterType :: Flags -> OAT.ParameterObject -> Q Type
getParameterType flags parameter =
  let paramType = varT $ getSchemaType flags (getSchemaFromParameter parameter)
   in ( if getRequiredFromParameter parameter
          then paramType
          else [t|Maybe $(paramType)|]
      )

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
  T.Text ->
  -- | HTTP Method (POST,GET,etc.)
  T.Text ->
  -- | Schema of body
  Maybe.Maybe (OAT.Schema, OC.RequestBodyEncoding) ->
  -- | The operation object
  OAT.OperationObject ->
  -- | An expression used to transform the response from 'B8.ByteString' to the required response type.
  -- Note that the response is nested within a HTTP monad and an 'Either'.
  Q Exp ->
  -- | Function body definition in TH
  OAM.Generator (Q Doc)
defineOperationFunction useExplicitConfiguration fnName params requestPath method bodySchema operation responseTransformerExp = do
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
  pure $
    ppr <$> case bodySchema of
      Just (_, encoding) ->
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
                    (T.toUpper method)
                    (T.pack $(request))
                    $(queryParameters)
                    $(varE bodyName)
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
                (T.toUpper method)
                (T.pack $(request))
                $(queryParameters)
              )
          |]

getBodySchemaFromOperation :: OAT.OperationObject -> OAM.Generator (Maybe (OAT.Schema, OC.RequestBodyEncoding))
getBodySchemaFromOperation operation = do
  requestBody <- getRequestBodyObject operation
  case requestBody of
    Just body -> getRequestBodySchema body
    Nothing -> pure Nothing

getRequestBodyContent :: OAT.RequestBodyObject -> Map.Map T.Text OAT.MediaTypeObject
getRequestBodyContent = OAT.content

getSchemaFromMedia :: OAT.MediaTypeObject -> Maybe OAT.Schema
getSchemaFromMedia = OAT.schema

getRequestBodySchema :: OAT.RequestBodyObject -> OAM.Generator (Maybe (OAT.Schema, OC.RequestBodyEncoding))
getRequestBodySchema body =
  let content = Map.lookup "application/json" $ getRequestBodyContent body
   in case content of
        Nothing ->
          let formContent = Map.lookup "application/x-www-form-urlencoded" $ getRequestBodyContent body
           in case formContent of
                Nothing -> do
                  OAM.logWarning "Only content type application/json and application/x-www-form-urlencoded is supported"
                  pure Nothing
                Just media ->
                  pure $ getSchemaFromMedia media >>= (\x -> Just (x, OC.RequestBodyEncodingFormData))
        Just media ->
          pure $ getSchemaFromMedia media >>= (\x -> Just (x, OC.RequestBodyEncodingJSON))

getRequestBodyObject :: OAT.OperationObject -> OAM.Generator (Maybe OAT.RequestBodyObject)
getRequestBodyObject operation =
  case OAT.requestBody operation of
    Nothing -> pure Nothing
    Just (OAT.Concrete p) -> pure $ Just p
    Just (OAT.Reference ref) -> do
      p <- OAM.getRequestBodyReferenceM ref
      when (Maybe.isNothing p) $ OAM.logWarning $ "Reference " <> ref <> " to RequestBody could not be found and therefore will be skipped."
      pure p

getResponseSchema :: OAT.ResponseObject -> OAM.Generator (Maybe OAT.Schema)
getResponseSchema response = do
  let contentMap = OAT.content (response :: OAT.ResponseObject)
      schema = Map.lookup "application/json" contentMap >>= getSchemaFromMedia
  when (Maybe.isNothing schema && not (Map.null contentMap)) $ OAM.logWarning "Only content type application/json is supported for response bodies."
  pure schema

getResponseObject :: OAT.Referencable OAT.ResponseObject -> OAM.Generator (Maybe OAT.ResponseObject)
getResponseObject (OAT.Concrete p) = pure $ Just p
getResponseObject (OAT.Reference ref) = do
  p <- OAM.getResponseReferenceM ref
  when (Maybe.isNothing p) $ OAM.logWarning $ "Reference " <> ref <> " to Response could not be found and therefore will be skipped."
  pure p

-- | Generates query params in the form of [(Text,ByteString)]
generateQueryParams :: [(Name, OAT.ParameterObject)] -> Q Exp
generateQueryParams ((name, param) : xs) =
  infixE (Just [|(queryName, $(expr))|]) (varE $ mkName ":") (Just $ generateQueryParams xs)
  where
    queryName = getNameFromParameter param
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
generateParameterizedRequestPath :: [(Name, OAT.ParameterObject)] -> T.Text -> Q Exp
generateParameterizedRequestPath ((paramName, param) : xs) path =
  foldr1 (foldingFn paramName) partExpressiones
  where
    parts = Split.splitOn ("{" <> T.unpack (getNameFromParameter param) <> "}") (T.unpack path)
    partExpressiones = generateParameterizedRequestPath xs . T.pack <$> parts
    foldingFn :: Name -> Q Exp -> Q Exp -> Q Exp
    foldingFn var a b = [|$(a) ++ B8.unpack (HT.urlEncode True $ B8.pack $ OC.stringifyModel $(varE var)) ++ $(b)|]
generateParameterizedRequestPath _ path = litE (stringL $ T.unpack path)

getOperationDescription :: OAT.OperationObject -> T.Text
getOperationDescription operation =
  Maybe.fromMaybe "No summary provided" $ OAT.summary (operation :: OAT.OperationObject)

getOperationName :: T.Text -> T.Text -> OAT.OperationObject -> OAM.Generator Name
getOperationName requestPath method operation =
  let operationId = OAT.operationId operation
      textName = Maybe.fromMaybe (T.map toLower method <> requestPath) operationId
   in haskellifyNameM False textName
