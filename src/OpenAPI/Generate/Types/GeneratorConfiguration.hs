{-# LANGUAGE OverloadedStrings #-}

module OpenAPI.Generate.Types.GeneratorConfiguration where

import Data.Text (Text)
import Data.Yaml

newtype GeneratorConfiguration = GeneratorConfiguration
  { -- | A list of schema names (exactly as they are named in the components.schemas section of the corresponding OpenAPI 3 specification)
    -- which are not further investigated while generating code from the specification.
    -- Only a type alias to 'Aeson.Value' is created for these schemas.
    generatorConfigurationOpaqueSchemas :: [Text]
  }
  deriving (Show, Eq)

instance FromJSON GeneratorConfiguration where
  parseJSON = withObject "GeneratorConfiguration" $ \o ->
    GeneratorConfiguration
      <$> o .:? "opaqueSchemas" .!= []
