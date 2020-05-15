{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module : OpenAPI.Generate
--
-- Functionality to Generate Haskell Code out of an OpenAPI definition File
module OpenAPI.Generate where

import qualified Data.ByteString.Char8 as B8
import qualified Data.List.Split as Split
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml
import GHC.Generics
import Language.Haskell.TH
import Language.Haskell.TH.PprLib hiding ((<>))
import Language.Haskell.TH.Syntax
import qualified Network.HTTP.Simple as HS
import OpenAPI.Common as OC
import OpenAPI.Generate.Doc
import OpenAPI.Generate.Flags
import OpenAPI.Generate.Internal.Model
import OpenAPI.Generate.Model
import qualified OpenAPI.Generate.Monad as OAM
import qualified OpenAPI.Generate.Operation as OGO
import qualified OpenAPI.Generate.SecurityScheme as OAS
import OpenAPI.Generate.Types
import OpenAPI.Generate.Types.Schema as SchemaType
import System.Exit

-- | Decodes an OpenAPI File
decodeOpenApi :: String -> IO OpenApiSpecification
decodeOpenApi fileName = do
  res <- decodeFileEither fileName
  case res of
    Left exc -> die $ "Could not parse OpenAPI specification '" <> fileName <> "': " <> show exc
    Right o -> pure o

-- | Defines all the operations as functions and the common imports
defineOperations :: String -> OpenApiSpecification -> OAM.Generator (Q [OGO.ModuleDefinition])
defineOperations moduleName =
  OAM.nested "paths"
    . fmap
      ( fmap
          concat
          . sequence
      )
    . mapM (uncurry $ OGO.defineOperationsForPath moduleName)
    . Map.toList
    . paths

-- | Defines code for the server information
--   A 'defaultConfiguration' method
--   A 'defaultURL' method
defineServerInformation :: String -> OpenApiSpecification -> Q Doc
defineServerInformation moduleName spec =
  let servers' = (servers :: OpenApiSpecification -> [ServerObject]) spec
      defaultURL = getServerURL servers'
      defaultURLName = mkName "defaultURL"
      getServerURL = maybe "/" (url :: ServerObject -> Text) . listToMaybe
   in addConfigurationModuleHeader moduleName . ppr . concat
        <$> sequence
          [ [d|$(varP defaultURLName) = defaultURL|],
            [d|$(varP $ mkName "defaultConfiguration") = OC.Configuration $(varE defaultURLName) OC.AnonymousSecurityScheme|]
          ]

defineModelsFromSpec :: String -> OpenApiSpecification -> OAM.Generator (Q [ModuleDefinition])
defineModelsFromSpec moduleName spec =
  let schemaDefinitions = Map.toList $ schemas $ components spec
   in OAM.nested "schemas" $ do
        models <- mapM (uncurry defineModelForSchema) schemaDefinitions
        pure $ getModelModulesFromModelsWithDependencies moduleName models

defineSecuritySchemes :: String -> OpenApiSpecification -> OAM.Generator (Q Doc)
defineSecuritySchemes moduleName =
  OAM.nested "components"
    . fmap (fmap $ addSecuritySchemesModuleHeader moduleName)
    . OAS.defineSupportedSecuritySchemes
    . mapMaybe
      ( \(name, scheme) -> case scheme of
          Concrete s -> Just (name, s)
          Reference _ -> Nothing
      )
    . Map.toList
    . securitySchemes
    . components
