{-# LANGUAGE CPP #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}

-- | Contains the functionality to define operation functions for path items.
module OpenAPI.Generate.Operation
  ( defineOperationsForPath,
  )
where

import qualified Control.Applicative as Applicative
import Control.Monad
import qualified Data.Bifunctor as BF
import qualified Data.ByteString.Char8 as B8
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.PprLib hiding ((<>))
import qualified OpenAPI.Common as OC
import qualified OpenAPI.Generate.Doc as Doc
import OpenAPI.Generate.Internal.Operation
import OpenAPI.Generate.Internal.Util
import qualified OpenAPI.Generate.Model as Model
import qualified OpenAPI.Generate.ModelDependencies as Dep
import qualified OpenAPI.Generate.Monad as OAM
import qualified OpenAPI.Generate.OptParse as OAO
import qualified OpenAPI.Generate.Response as OAR
import qualified OpenAPI.Generate.Types as OAT

#if MIN_VERSION_template_haskell(2,17,0)
nameToTypeVariable :: Name -> Q (TyVarBndr Specificity)
nameToTypeVariable monadName = plainInvisTV monadName specifiedSpec
#else
nameToTypeVariable :: Name -> Q TyVarBndr
nameToTypeVariable monadName = pure $ plainTV monadName
#endif

-- | Defines the operations for all paths and their methods
defineOperationsForPath :: String -> Text -> OAT.PathItemObject -> OAM.Generator (Q [Dep.ModuleDefinition], Dep.Models)
defineOperationsForPath mainModuleName requestPath pathItemObject = OAM.nested requestPath $ do
  operationsToGenerate <- OAM.getSetting OAO.settingOperationsToGenerate
  fmap (BF.bimap sequence Set.unions)
    . mapAndUnzipM
      (uncurry (defineModuleForOperation mainModuleName requestPath))
    $ filterEmptyAndOmittedOperations
      operationsToGenerate
      [ ("GET", OAT.get pathItemObject),
        ("PUT", OAT.put pathItemObject),
        ("POST", OAT.post pathItemObject),
        ("DELETE", OAT.delete pathItemObject),
        ("OPTIONS", OAT.options pathItemObject),
        ("HEAD", OAT.head pathItemObject),
        ("PATCH", OAT.patch pathItemObject),
        ("TRACE", OAT.trace pathItemObject)
      ]

filterEmptyAndOmittedOperations :: [Text] -> [(Text, Maybe OAT.OperationObject)] -> [(Text, OAT.OperationObject)]
filterEmptyAndOmittedOperations operationsToGenerate xs =
  [ (method, operation)
    | (method, Just operation) <- xs,
      null operationsToGenerate || OAT.operationId operation `elem` fmap Just operationsToGenerate
  ]

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
  Text ->
  -- | HTTP Method (GET,POST,etc)
  Text ->
  -- | The Operation Object
  OAT.OperationObject ->
  -- | commented function definition and implementation
  OAM.Generator (Q Dep.ModuleDefinition, Dep.Models)
defineModuleForOperation mainModuleName requestPath method operation = OAM.nested method $ do
  operationIdName <- getOperationName requestPath method operation
  convertToCamelCase <- OAM.getSetting OAO.settingConvertToCamelCase
  let operationIdAsText = T.pack $ show operationIdName
      appendToOperationName = ((T.pack $ nameBase operationIdName) <>)
      moduleName = haskellifyText convertToCamelCase True operationIdAsText
  OAM.logInfo $ "Generating operation with name '" <> operationIdAsText <> "'"
  (bodySchema, bodyPath) <- getBodySchemaFromOperation operation
  (responseTypeName, responseTransformerExp, responseBodyDefinitions, responseBodyDependencies) <- OAR.getResponseDefinitions operation appendToOperationName
  (bodyType, (bodyDefinition, bodyDependencies)) <- OAM.resetPath bodyPath $ getBodyType bodySchema appendToOperationName
  parameterCardinality <- generateParameterTypeFromOperation operationIdAsText operation
  paramDescriptions <-
    (<> ["The request body to send" | not $ null bodyType])
      <$> ( case parameterCardinality of
              NoParameters -> pure []
              SingleParameter _ _ parameter -> pure <$> getParameterDescription parameter
              MultipleParameters _ -> pure ["Contains all available parameters of this operation (query and path parameters)"]
          )
  let (paramType, paramDoc, paramDependencies) = case parameterCardinality of
        NoParameters -> ([], Doc.emptyDoc, Set.empty)
        SingleParameter paramType' (doc, deps) _ -> ([paramType'], doc, deps)
        MultipleParameters paramDefinition ->
          ( [parameterTypeDefinitionType paramDefinition],
            parameterTypeDefinitionDoc paramDefinition,
            parameterTypeDefinitionDependencies paramDefinition
          )
      types = paramType <> bodyType
      monadName = mkName "m"
      createFunSignature operationName fnType' = do
        tv <- nameToTypeVariable monadName
        ppr
          <$> sigD
            operationName
            ( forallT
                [tv]
                (cxt [appT (conT ''OC.MonadHTTP) (varT monadName)])
                fnType'
            )
      methodAndPath = T.toUpper method <> " " <> requestPath
      operationNameAsString = nameBase operationIdName
      operationDescription = pure . Doc.generateHaddockComment . ("> " <> methodAndPath :) . ("" :)
      cartesianProduct = Applicative.liftA2 (,)
      addToName suffix = mkName . (<> suffix) . nameBase
      availableOperationCombinations =
        cartesianProduct
          [ (id, responseTransformerExp, responseTypeName),
            (addToName "Raw", [|id|], ''B8.ByteString)
          ]
          [ (id, False, getParametersTypeForSignatureWithMonadTransformer),
            (addToName "WithConfiguration", True, getParametersTypeForSignature)
          ]
      description = Doc.escapeText $ getOperationDescription operation
      comments =
        [ [operationDescription [description]],
          [paramDoc, bodyDefinition, responseBodyDefinitions, operationDescription ["The same as '" <> operationIdAsText <> "' but accepts an explicit configuration."]],
          [operationDescription ["The same as '" <> operationIdAsText <> "' but returns the raw 'Data.ByteString.Char8.ByteString'."]],
          [operationDescription ["The same as '" <> operationIdAsText <> "' but accepts an explicit configuration and returns the raw 'Data.ByteString.Char8.ByteString'."]]
        ]
  functionDefinitions <-
    mapM
      ( \((f1, transformExp, responseType), (f2, explicitConfiguration, getParameterType)) -> do
          let fnName = f1 . f2 $ operationIdName
              fnSignature = createFunSignature fnName $ getParameterType types responseType monadName
              addCommentsToFnSignature =
                ( `Doc.sideBySide`
                    Doc.sideComments
                      ((if explicitConfiguration then ("The configuration to use in the request" :) else id) $ paramDescriptions <> ["Monadic computation which returns the result of the operation"])
                )
                  . Doc.breakOnTokens ["->"]
          functionBody <- defineOperationFunction explicitConfiguration fnName parameterCardinality requestPath method bodySchema transformExp
          pure [fmap addCommentsToFnSignature fnSignature `Doc.appendDoc` functionBody]
      )
      availableOperationCombinations
  omitAdditionalFunctions <- OAM.getSetting OAO.settingOmitAdditionalOperationFunctions
  let content =
        concat $
          if omitAdditionalFunctions
            then
              zipWith
                (<>)
                [ [operationDescription [description]],
                  [paramDoc, bodyDefinition, responseBodyDefinitions]
                ]
                $ (<> [[Doc.emptyDoc]]) $
                  maybe [] pure $
                    Maybe.listToMaybe functionDefinitions
            else zipWith (<>) comments functionDefinitions
  OAM.logTrace $ T.intercalate ", " $ Set.toList $ Set.unions [paramDependencies, bodyDependencies, responseBodyDependencies]
  pure
    ( ([moduleName],)
        . Doc.addOperationsModuleHeader mainModuleName moduleName operationNameAsString
        . ($$ text "")
        <$> ( vcat
                <$> sequence content
            ),
      Set.unions [paramDependencies, bodyDependencies, responseBodyDependencies]
    )

getBodyType :: Maybe RequestBodyDefinition -> (Text -> Text) -> OAM.Generator ([Q Type], Dep.ModelContentWithDependencies)
getBodyType requestBody appendToOperationName = do
  generateBody <- shouldGenerateRequestBody requestBody
  case requestBody of
    Just RequestBodyDefinition {..} | generateBody -> do
      let transformType = pure . (if required then id else appT $ varT ''Maybe)
      requestBodySuffix <- OAM.getSetting OAO.settingRequestBodyTypeSuffix
      BF.first transformType <$> Model.defineModelForSchemaNamed (appendToOperationName requestBodySuffix) schema
    _ -> pure ([], (Doc.emptyDoc, Set.empty))
