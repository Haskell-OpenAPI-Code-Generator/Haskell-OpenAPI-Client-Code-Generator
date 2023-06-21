{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | This module contains the utilities to define the data types of the response type of an operation
module OpenAPI.Generate.Response
  ( getResponseDefinitions,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Either as Either
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Language.Haskell.TH
import Language.Haskell.TH.PprLib hiding ((<>))
import Language.Haskell.TH.Syntax
import qualified Network.HTTP.Client as HC
import qualified Network.HTTP.Types as HT
import qualified OpenAPI.Generate.Doc as Doc
import OpenAPI.Generate.Internal.Operation
import OpenAPI.Generate.Internal.Util
import qualified OpenAPI.Generate.Model as Model
import qualified OpenAPI.Generate.ModelDependencies as Dep
import qualified OpenAPI.Generate.Monad as OAM
import qualified OpenAPI.Generate.OptParse as OAO
import qualified OpenAPI.Generate.Types as OAT

#if !MIN_VERSION_template_haskell(2,17,0)
examineCode :: a -> a
examineCode = id
#endif

-- | Generates a response type with a constructor for all possible response types of the operation.
--
-- Always generates an error case which is used if no other case matches.
getResponseDefinitions ::
  -- | The operation to generate the response types for
  OAT.OperationObject ->
  -- | A function which appends the passed 'Text' to the operation name and returns it
  (Text -> Text) ->
  -- | Returns the name of the reponse data type, the response transformation function and the document containing
  -- the definitions of all response types.
  OAM.Generator (Name, Q Exp, Q Doc, Dep.Models)
getResponseDefinitions operation appendToOperationName = OAM.nested "responses" $ do
  convertToCamelCase <- OAM.getSetting OAO.settingConvertToCamelCase
  responseSuffix <- OAM.getSetting OAO.settingResponseTypeSuffix
  responseBodySuffix <- OAM.getSetting OAO.settingResponseBodyTypeSuffix
  let responsesObject = OAT.operationObjectResponses operation
      createBodyName = createResponseNameAsText convertToCamelCase appendToOperationName . (responseBodySuffix <>)
      createName = createResponseName convertToCamelCase appendToOperationName . (responseSuffix <>)
      responseName = createName ""
      responseReferenceCases = getStatusCodeResponseCases responsesObject <> getRangeResponseCases responsesObject
  responseCases <- resolveResponseReferences responseReferenceCases
  let responseDescriptions = getResponseDescription . (\(_, _, (r, _)) -> r) <$> responseCases
  schemas <- generateResponseCaseDefinitions createBodyName responseCases
  let dependencies = Set.unions $ snd . snd <$> Maybe.mapMaybe (\(_, _, x) -> x) schemas
  pure $
    (responseName,createResponseTransformerFn createName schemas,,dependencies) $
      vcat
        <$> sequence
          [ pure $
              Doc.generateHaddockComment
                [ "Represents a response of the operation '" <> appendToOperationName "" <> "'.",
                  "",
                  "The response constructor is chosen by the status code of the response. If no case matches (no specific case for the response code, no range case, no default case), '"
                    <> createResponseNameAsText convertToCamelCase appendToOperationName (responseSuffix <> errorSuffix)
                    <> "' is used."
                ],
            ( `Doc.sideBySide`
                (text "" $$ Doc.sideComments ("Means either no matching case available or a parse error" : responseDescriptions))
            )
              . Doc.reformatADT
              . ppr
              <$> dataD
                (cxt [])
                responseName
                []
                Nothing
                ( fmap
                    ( \(suffix, _, maybeSchema) ->
                        normalC
                          (createName suffix)
                          ( case maybeSchema of
                              Just (type', _) -> [bangType (bang noSourceUnpackedness noSourceStrictness) type']
                              Nothing -> []
                          )
                    )
                    ((errorSuffix, examineCode [||const True||], Just ([t|String|], (Doc.emptyDoc, Set.empty))) : schemas)
                )
                [derivClause Nothing [conT ''Show, conT ''Eq]],
            printSchemaDefinitions schemas
          ]

-- | First: suffix to append to the data constructor name
-- Second: an expression which can be used to determine if this case should be used in regard to the response status
-- Third: Reference or concrete response object
type ResponseReferenceCase = (Text, TExpQ (HT.Status -> Bool), OAT.Referencable OAT.ResponseObject)

-- | Same as @ResponseReferenceCase@ but with resolved reference
type ResponseCase = (Text, TExpQ (HT.Status -> Bool), (OAT.ResponseObject, [Text]))

-- | Same as @ResponseReferenceCase@ but with type definition
type ResponseCaseDefinition = (Text, TExpQ (HT.Status -> Bool), Maybe Model.TypeWithDeclaration)

-- | Suffix used for the error case
errorSuffix :: Text
errorSuffix = "Error"

-- | Create the name as 'Text' of the response type / data constructor based on a suffix
createResponseNameAsText :: Bool -> (Text -> Text) -> Text -> Text
createResponseNameAsText convertToCamelCase appendToOperationName = T.pack . haskellifyText convertToCamelCase True . appendToOperationName

-- | Create the name as 'Name' of the response type / data constructor based on a suffix
createResponseName :: Bool -> (Text -> Text) -> Text -> Name
createResponseName convertToCamelCase appendToOperationName = mkName . T.unpack . createResponseNameAsText convertToCamelCase appendToOperationName

-- | Generate the response cases which have a range instead of a single status code
getRangeResponseCases :: OAT.ResponsesObject -> [ResponseReferenceCase]
getRangeResponseCases responsesObject =
  Maybe.catMaybes
    [ ("1XX",examineCode [||HT.statusIsInformational||],) <$> OAT.responsesObjectRange1XX responsesObject,
      ("2XX",examineCode [||HT.statusIsSuccessful||],) <$> OAT.responsesObjectRange2XX responsesObject,
      ("3XX",examineCode [||HT.statusIsRedirection||],) <$> OAT.responsesObjectRange3XX responsesObject,
      ("4XX",examineCode [||HT.statusIsClientError||],) <$> OAT.responsesObjectRange4XX responsesObject,
      ("5XX",examineCode [||HT.statusIsServerError||],) <$> OAT.responsesObjectRange5XX responsesObject,
      ("Default",examineCode [||const True||],) <$> OAT.responsesObjectDefault responsesObject
    ]

-- | Generate the response cases based on the available status codes
getStatusCodeResponseCases :: OAT.ResponsesObject -> [ResponseReferenceCase]
getStatusCodeResponseCases =
  fmap (\(code, response) -> (T.pack $ show code, examineCode [||\status -> HT.statusCode status == code||], response))
    . Map.toList
    . OAT.responsesObjectPerStatusCode

-- | Resolve the references in response cases
--
-- Note: Discards the unresolved references and generates a log message for them
resolveResponseReferences :: [ResponseReferenceCase] -> OAM.Generator [ResponseCase]
resolveResponseReferences =
  fmap Maybe.catMaybes
    . mapM
      ( \(suffix, guard, response) ->
          fmap (suffix,guard,) <$> OAM.nested suffix (getResponseObject response)
      )

-- | Generate the response definitions
--
-- If no response schema is available for a case (or with an unsupported media type), an empty data constructor is used
generateResponseCaseDefinitions :: (Text -> Text) -> [ResponseCase] -> OAM.Generator [ResponseCaseDefinition]
generateResponseCaseDefinitions createBodyName =
  mapM
    ( \(suffix, guard, (r, path)) -> OAM.resetPath path $ do
        (responseSchema, path') <- getResponseSchema r
        (suffix,guard,) <$> mapM (OAM.resetPath path' . Model.defineModelForSchemaNamed (createBodyName suffix)) responseSchema
    )

-- | Prints the definitions of the different response case data types in 'Q'
printSchemaDefinitions :: [ResponseCaseDefinition] -> Q Doc
printSchemaDefinitions =
  fmap vcat
    . sequence
    . Maybe.mapMaybe (\(_, _, namedTypeDef) -> fmap (fst . snd) namedTypeDef)

-- | Creates a function as 'Q Exp' which can be used in the generated code to transform the response
createResponseTransformerFn :: (Text -> Name) -> [ResponseCaseDefinition] -> Q Exp
createResponseTransformerFn createName schemas =
  let responseArgName = mkName "response"
      bodyName = mkName "body"
      ifCases =
        multiIfE $
          fmap
            ( \(suffix, guard, maybeSchema) ->
                normalGE
                  [|$(unTypeQ guard) (HC.responseStatus $(varE responseArgName))|]
                  ( case maybeSchema of
                      Just (type', _) -> [|$(varE $ createName suffix) <$> (Aeson.eitherDecodeStrict $(varE bodyName) :: Either String $type')|]
                      Nothing -> [|Right $(varE $ createName suffix)|]
                  )
            )
            schemas
            <> [normalGE [|otherwise|] [|Left "Missing default response type"|]]
      transformLambda = lamE [varP responseArgName, varP bodyName] ifCases
   in [|fmap (\response -> fmap (Either.either $(varE $ createName errorSuffix) id . $transformLambda response) response)|]

getResponseDescription :: OAT.ResponseObject -> Text
getResponseDescription = Doc.escapeText . OAT.responseObjectDescription
