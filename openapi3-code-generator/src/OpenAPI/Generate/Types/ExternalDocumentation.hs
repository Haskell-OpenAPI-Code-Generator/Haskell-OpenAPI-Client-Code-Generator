{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | For more information see http://spec.openapis.org/oas/v3.0.3#external-documentation-object
module OpenAPI.Generate.Types.ExternalDocumentation where

import Data.Text (Text)
import Data.Yaml
import GHC.Generics

data ExternalDocumentationObject = ExternalDocumentationObject
  { externalDocumentationObjectUrl :: Text,
    externalDocumentationObjectDescription :: Maybe Text
  }
  deriving (Show, Ord, Eq, Generic)

instance FromJSON ExternalDocumentationObject where
  parseJSON = withObject "ExternalDocumentationObject" $ \o ->
    ExternalDocumentationObject
      <$> o .: "url"
      <*> o .:? "description"
