{-# LANGUAGE DeriveGeneric #-}

module OpenAPI.Generate.Types.Doc where

import Data.Text
import Data.Yaml
import GHC.Generics

data ExternalDocumentationObject
  = ExternalDocumentationObject
      { url :: Text,
        description :: Maybe Text
      }
  deriving (Show, Ord, Eq, Generic)

instance FromJSON ExternalDocumentationObject
