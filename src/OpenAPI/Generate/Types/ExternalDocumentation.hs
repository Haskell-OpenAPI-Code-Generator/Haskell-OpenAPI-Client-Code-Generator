{-# LANGUAGE DeriveGeneric #-}

-- | For more information see http://spec.openapis.org/oas/v3.0.3#external-documentation-object
module OpenAPI.Generate.Types.ExternalDocumentation where

import Data.Text (Text)
import Data.Yaml
import GHC.Generics

data ExternalDocumentationObject
  = ExternalDocumentationObject
      { url :: Text,
        description :: Maybe Text
      }
  deriving (Show, Ord, Eq, Generic)

instance FromJSON ExternalDocumentationObject
