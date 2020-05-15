{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module OpenAPI.Generate.Operation
  ( defineOperationsForPath,
    ModuleDefinition,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Bifunctor as Bif
import qualified Data.ByteString.Char8 as B8
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.PprLib hiding ((<>))
import Language.Haskell.TH.Syntax
import qualified Network.HTTP.Simple as HS
import qualified OpenAPI.Common as OC
import qualified OpenAPI.Generate.Doc as Doc
import qualified OpenAPI.Generate.Flags as OAF
import OpenAPI.Generate.Internal.Model
import OpenAPI.Generate.Internal.Operation
import OpenAPI.Generate.Internal.Util
import qualified OpenAPI.Generate.Model as Model
import qualified OpenAPI.Generate.Monad as OAM
import qualified OpenAPI.Generate.Response as OAR
import qualified OpenAPI.Generate.Types as OAT

-- | Defines the operations for all paths and their methods
defineOperationsForPath :: String -> Text -> OAT.PathItemObject -> OAM.Generator (Q [ModuleDefinition])
defineOperationsForPath mainModuleName requestPath =
  OAM.nested requestPath
    . fmap sequence
    . mapM
      (uncurry (defineModuleForOperation mainModuleName requestPath))
    . ( \pathItemObject ->
          filterEmptyOperations
            [ ("GET", OAT.get pathItemObject),
              ("PUT", OAT.put pathItemObject),
              ("POST", OAT.post pathItemObject),
              ("DELETE", OAT.delete pathItemObject),
              ("OPTIONS", OAT.options pathItemObject),
              ("HEAD", OAT.head pathItemObject),
              ("PATCH", OAT.patch pathItemObject),
              ("TRACE", OAT.trace pathItemObject)
            ]
      )

-- | A path may define n methods
--   This function filters out the empy not defined methods
filterEmptyOperations :: [(Text, Maybe OAT.OperationObject)] -> [(Text, OAT.OperationObject)]
filterEmptyOperations xs = [(method, operation) | (method, Just operation) <- xs]

-- |
--  Defines an Operation for a Method and a Path
--  Uses an OperationObject
--
--  Returns a commented function definition and implementation in a Q Monad
defineModuleForOperation ::
  -- | The main module name passed via CLI options
  String ->
  -- | The path to the request (This is the key from the map of Operations)
  --  It may contain placeholder variables in the form of /my/{var}/path/
  T.Text ->
  -- | HTTP Method (GET,POST,etc)
  T.Text ->
  -- | The Operation Object
  OAT.OperationObject ->
  -- | commented function definition and implementation
  OAM.Generator (Q ModuleDefinition)
defineModuleForOperation mainModuleName requestPath method operation = OAM.nested method $ do
  operationIdName <- getOperationName requestPath method operation
  flags <- OAM.getFlags
  let operationIdNameRaw = mkName $ nameBase operationIdName <> "Raw"
      operationIdNameWithMonadTransformer = mkName $ nameBase operationIdName <> "M"
      operationIdNameRawWithMonadTransformer = mkName $ nameBase operationIdNameRaw <> "M"
      moduleName = haskellifyText (OAF.optConvertToCamelCase flags) True (T.pack $ show operationIdName)
      description = getOperationDescription operation
      monadName = mkName "m"
      securitySchemeName = mkName "s"
      appendToOperationName = ((T.pack $ nameBase operationIdName) <>)
      rawTransformation = [|id|]
   in OAM.nested method $ do
        OAM.logInfo $ "Generating operation with name: " <> T.pack (show operationIdName)
        params <- getParametersFromOperationConcrete operation
        bodySchema <- getBodySchemaFromOperation operation
        (responseTypeName, responseTransformerExp, responseBodyDefinitions) <- OAR.getResponseDefinitions operation appendToOperationName
        functionBody <- defineOperationFunction True operationIdName params requestPath method bodySchema operation responseTransformerExp
        functionBodyRaw <- defineOperationFunction True operationIdNameRaw params requestPath method bodySchema operation rawTransformation
        functionBodyWithMonadTransformer <- defineOperationFunction False operationIdNameWithMonadTransformer params requestPath method bodySchema operation responseTransformerExp
        functionBodyRawWithMonadTransformer <- defineOperationFunction False operationIdNameRawWithMonadTransformer params requestPath method bodySchema operation rawTransformation
        (bodyType, bodyDefinition) <- getBodyType bodySchema appendToOperationName
        let types = (getParameterType flags <$> params) <> bodyType
            fnType = getParametersTypeForSignature types responseTypeName monadName securitySchemeName
            fnTypeRaw = getParametersTypeForSignature types ''B8.ByteString monadName securitySchemeName
            fnTypeWithMonadTransformer = getParametersTypeForSignatureWithMonadTransformer types responseTypeName monadName securitySchemeName
            fnTypeRawWithMonadTransformer = getParametersTypeForSignatureWithMonadTransformer types ''B8.ByteString monadName securitySchemeName
            createFunSignature operationName fnType =
              ppr
                <$> sigD
                  operationName
                  ( forallT
                      [plainTV monadName, plainTV securitySchemeName]
                      (cxt [appT (conT ''OC.MonadHTTP) (varT monadName), appT (conT ''OC.SecurityScheme) (varT securitySchemeName)])
                      fnType
                  )
            fnSignature =
              createFunSignature
                operationIdName
                fnType
            fnSignatureRaw =
              createFunSignature
                operationIdNameRaw
                fnTypeRaw
            fnSignatureWithMonadTransformer =
              createFunSignature
                operationIdNameWithMonadTransformer
                fnTypeWithMonadTransformer
            fnSignatureRawWithMonadTransformer =
              createFunSignature
                operationIdNameRawWithMonadTransformer
                fnTypeRawWithMonadTransformer
        pure $
          ([moduleName],)
            . Doc.addOperationsModuleHeader mainModuleName moduleName
            . (Doc.generateHaddockComment description (T.toUpper method <> " " <> requestPath) $$)
            . ($$ text "")
            <$> ( ($$)
                    <$> ( ($$ text "")
                            . vcat
                            <$> sequence
                              [ fnSignature,
                                functionBody,
                                fnSignatureRaw,
                                functionBodyRaw,
                                fnSignatureWithMonadTransformer,
                                functionBodyWithMonadTransformer,
                                fnSignatureRawWithMonadTransformer,
                                functionBodyRawWithMonadTransformer,
                                bodyDefinition
                              ]
                        )
                      <*> responseBodyDefinitions
                )

getBodyType :: Maybe (OAT.Schema, OC.RequestBodyEncoding) -> (Text -> Text) -> OAM.Generator ([Q Type], Q Doc)
getBodyType Nothing _ = pure ([], Doc.emptyDoc)
getBodyType (Just (bodySchema, _)) appendToOperationName = do
  requestBodySuffix <- OAM.getFlag $ T.pack . OAF.optRequestBodyTypeSuffix
  Bif.bimap pure fst <$> Model.defineModelForSchemaNamed (appendToOperationName requestBodySuffix) bodySchema
