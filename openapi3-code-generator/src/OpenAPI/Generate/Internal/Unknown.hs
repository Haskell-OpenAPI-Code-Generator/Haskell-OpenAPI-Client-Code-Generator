{-# LANGUAGE OverloadedStrings #-}

module OpenAPI.Generate.Internal.Unknown
  ( warnAboutUnknownWhiteListedOrOpaqueSchemas,
    warnAboutUnknownOperations,
  )
where

import Control.Monad
import qualified Data.List as L
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified OpenAPI.Generate.Monad as OAM
import qualified OpenAPI.Generate.OptParse as OAO
import qualified OpenAPI.Generate.Types as OAT
import qualified OpenAPI.Generate.Types.Schema as OAS

-- | Warn about operations listed as CLI options which do not appear in the provided OpenAPI specification
warnAboutUnknownOperations :: [(Text, OAT.PathItemObject)] -> OAM.Generator ()
warnAboutUnknownOperations operationDefinitions = do
  let getAllOperationObjectsFromPathItemObject = ([OAT.get, OAT.put, OAT.post, OAT.delete, OAT.options, OAT.head, OAT.patch, OAT.trace] <*>) . pure
      operationIds =
        Maybe.mapMaybe OAT.operationId $
          Maybe.catMaybes $
            operationDefinitions >>= getAllOperationObjectsFromPathItemObject . snd
  operationsToGenerate <- OAM.getSetting OAO.settingOperationsToGenerate
  printWarningIfUnknown (\operationId proposedOptions -> "The operation '" <> operationId <> "' which is listed for generation does not appear in the provided OpenAPI specification. " <> proposedOptions) operationIds operationsToGenerate

-- | Warn about schemas listed as CLI options (white list or opaque schemas) which do not appear in the provided OpenAPI specification
warnAboutUnknownWhiteListedOrOpaqueSchemas :: [(Text, OAS.Schema)] -> OAM.Generator ()
warnAboutUnknownWhiteListedOrOpaqueSchemas schemaDefinitions = do
  let schemaNames = fst <$> schemaDefinitions
      printWarningIfUnknownWithTypeName typeName = printWarningIfUnknown (\name proposedOptions -> "The " <> typeName <> " '" <> name <> "' does not appear in the provided OpenAPI specification. " <> proposedOptions) schemaNames
  whiteListedSchemas <- OAM.getSetting OAO.settingWhiteListedSchemas
  opaqueSchemas <- OAM.getSetting OAO.settingOpaqueSchemas
  printWarningIfUnknownWithTypeName "white-listed schema" whiteListedSchemas
  printWarningIfUnknownWithTypeName "schema listed as opaque" opaqueSchemas

printWarningIfUnknown :: (Text -> Text -> Text) -> [Text] -> [Text] -> OAM.Generator ()
printWarningIfUnknown generateMessage namesFromSpecification =
  mapM_
    ( \name ->
        unless (name `elem` namesFromSpecification) $
          OAM.logWarning $ generateMessage name $ getProposedOptionsFromNameAndAvailableSchemas name namesFromSpecification
    )

getProposedOptionsFromNameAndAvailableSchemas :: Text -> [Text] -> Text
getProposedOptionsFromNameAndAvailableSchemas name = getProposedOptions . sortByLongestCommonSubstring name

sortByLongestCommonSubstring :: Text -> [Text] -> [Text]
sortByLongestCommonSubstring needle = fmap fst . L.sortOn snd . fmap (\x -> (x, -(longestCommonSubstringCount needle x)))

getProposedOptions :: [Text] -> Text
getProposedOptions [] = "Specification does not contain any."
getProposedOptions (x1 : x2 : x3 : _ : _) = getProposedOptions [x1, x2, x3]
getProposedOptions xs =
  let separator = "\n      "
   in "Did you mean one of following options?" <> separator <> T.intercalate separator xs

longestCommonSubstringCount :: Text -> Text -> Int
longestCommonSubstringCount x y =
  let getSetWithAllSubstrings = Set.fromList . (T.inits <=< T.tails) . T.toLower
      intersection = Set.intersection (getSetWithAllSubstrings x) (getSetWithAllSubstrings y)
   in if Set.null intersection then 0 else T.length (L.maximumBy (Ord.comparing T.length) intersection)
