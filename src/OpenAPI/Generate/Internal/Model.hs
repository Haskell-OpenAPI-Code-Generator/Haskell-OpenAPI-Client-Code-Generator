{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module OpenAPI.Generate.Internal.Model
  ( getModelModulesFromModelsWithDependencies,
    ModuleDefinition,
    Models,
    ModelContentWithDependencies,
    ModelWithDependencies,
  )
where

import qualified Data.Bifunctor as Bif
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import Language.Haskell.TH
import Language.Haskell.TH.PprLib hiding ((<>))
import qualified OpenAPI.Generate.Doc as Doc
import OpenAPI.Generate.Internal.Util

-- | A module definition with a name as a string list with the different module levels (e. g. ["OpenAPI", "Generate"] for OpenAPI.Generate)
-- and the 'Doc' representing the module content
type ModuleDefinition = ([String], Doc)

-- | A set of model names (typically used as dependency list)
type Models = Set.Set Text

-- | A tuple containing the content and the dependencies of a model
type ModelContentWithDependencies = (Q Doc, Models)

-- | Represents a model with a name, content and dependencies
type ModelWithDependencies = (Text, ModelContentWithDependencies)

typesModule :: String
typesModule = "Types"

cyclicTypesModule :: String
cyclicTypesModule = "CyclicTypes"

-- | Analyzes the dependencies of the provided models and splits them into modules.
-- All modules with cyclic dependencies (between each other or to itself) are put in a module named by 'cyclicTypesModule'.
getModelModulesFromModelsWithDependencies :: String -> [ModelWithDependencies] -> Q [ModuleDefinition]
getModelModulesFromModelsWithDependencies mainModuleName = createModelModules mainModuleName . extractCyclicModuleDependentModels

createModelModules :: String -> ([ModelWithDependencies], Q Doc) -> Q [ModuleDefinition]
createModelModules mainModuleName (models, cyclicModuleContentQ) = do
  let prependTypesModule = ((typesModule <> ".") <>) . T.unpack
  let prependMainModule = ((mainModuleName <> ".") <>)
  cyclicModuleContent <- cyclicModuleContentQ
  modules <-
    mapM
      ( \(modelName, (doc, dependencies)) ->
          ([typesModule, T.unpack modelName],)
            . Doc.addModelModuleHeader
              mainModuleName
              (prependTypesModule modelName)
              (prependTypesModule <$> Set.toList dependencies)
              <$> doc
      )
      models
  let modelModuleNames = fmap (joinWithPoint . fst) modules
  pure $
    ([typesModule], Doc.createModuleHeaderWithReexports (prependMainModule typesModule) $ fmap prependMainModule (cyclicTypesModule : modelModuleNames))
      : ([cyclicTypesModule], Doc.addModelModuleHeader mainModuleName cyclicTypesModule modelModuleNames cyclicModuleContent)
      : modules

extractCyclicModuleDependentModels :: [ModelWithDependencies] -> ([ModelWithDependencies], Q Doc)
extractCyclicModuleDependentModels models =
  let (cyclicModels, extractedModels) = extractUnidirectionallyDependentModels (models, [])
   in (extractedModels, vcat <$> mapM (fst . snd) cyclicModels)

extractUnidirectionallyDependentModels :: ([ModelWithDependencies], [ModelWithDependencies]) -> ([ModelWithDependencies], [ModelWithDependencies])
extractUnidirectionallyDependentModels (rest, extractedModels) =
  let extractedModelNames = Set.fromList $ fmap fst extractedModels
      (newExtractedModels, notExtractedModels) = List.partition ((`Set.isSubsetOf` extractedModelNames) . snd . snd) rest
   in if null newExtractedModels
        then (rest, extractedModels)
        else extractUnidirectionallyDependentModels (notExtractedModels, extractedModels <> newExtractedModels)
