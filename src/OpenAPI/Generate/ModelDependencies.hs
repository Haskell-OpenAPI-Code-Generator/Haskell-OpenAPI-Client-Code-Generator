{-# LANGUAGE OverloadedStrings #-}

-- | Functionality to split models into multiple modules according to their dependencies
module OpenAPI.Generate.ModelDependencies
  ( getModelModulesFromModelsWithDependencies,
    ModuleDefinition,
    Models,
    ModelContentWithDependencies,
    ModelWithDependencies,
  )
where

import Data.List
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Text (Text)
import Language.Haskell.TH
import Language.Haskell.TH.PprLib hiding ((<>))
import qualified OpenAPI.Generate.Doc as Doc
import OpenAPI.Generate.Internal.Util

-- | A module definition with a name as a string list with the different module levels (e. g. [\"OpenAPI\", \"Generate\"] for "OpenAPI.Generate")
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

-- | Analyzes the dependencies of the provided models and splits them into modules.
-- All models which would form an own module but only consist of a type alias are put in a module named by 'Doc.typeAliasModule'.
getModelModulesFromModelsWithDependencies :: String -> [ModelWithDependencies] -> Q [ModuleDefinition]
getModelModulesFromModelsWithDependencies mainModuleName models = do
  let prependTypesModule = ((typesModule <> ".") <>) . T.unpack
      prependMainModule = ((mainModuleName <> ".") <>)
  modelsWithResolvedContent <-
    mapM
      ( \(name, (contentQ, dependencies)) -> do
          content <- contentQ
          pure (name, (content, dependencies))
      )
      models
  let (typeAliasModels, modelsWithContent) = partition (\(_, (content, _)) -> isTypeAliasModule content) modelsWithResolvedContent
      (typeAliasModuleNames, typeAliasContent, typeAliasDependencies) =
        foldr
          ( \(name, (content, dependencies)) (names, allContent, allDependencies) ->
              (Set.insert name names, allContent $$ text "" $$ content, Set.union dependencies allDependencies)
          )
          (Set.empty, empty, Set.empty)
          typeAliasModels
      modules =
        fmap
          ( \(modelName, (doc, dependencies)) ->
              ( [typesModule, T.unpack modelName],
                Doc.addModelModuleHeader
                  mainModuleName
                  (prependTypesModule modelName)
                  (prependTypesModule <$> Set.toList (Set.difference dependencies $ Set.insert modelName typeAliasModuleNames))
                  ("Contains the types generated from the schema " <> T.unpack modelName)
                  doc
              )
          )
          modelsWithContent
      modelModuleNames = fmap (joinWithPoint . fst) modules
  pure $
    ( [typesModule],
      Doc.createModuleHeaderWithReexports
        (prependMainModule typesModule)
        (fmap prependMainModule (Doc.typeAliasModule : modelModuleNames))
        "Rexports all type modules (used in the operation modules)."
    )
      : ( [Doc.typeAliasModule],
          Doc.addModelModuleHeader
            mainModuleName
            Doc.typeAliasModule
            (prependTypesModule <$> Set.toList (Set.difference typeAliasDependencies typeAliasModuleNames))
            "Contains all types with cyclic dependencies (between each other or to itself)"
            typeAliasContent
        )
      : modules

isTypeAliasModule :: Doc -> Bool
isTypeAliasModule =
  all
    ( \l ->
        isPrefixOf "--" l
          || isPrefixOf "type" l
          || null l
    )
    . lines
    . show
