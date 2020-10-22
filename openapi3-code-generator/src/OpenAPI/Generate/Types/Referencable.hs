{-# LANGUAGE OverloadedStrings #-}

-- | Many fields in OpenAPI can be either a reference or a concrete object.
-- This module adds this capabilities.
--
-- For more information see http://spec.openapis.org/oas/v3.0.3#reference-object
module OpenAPI.Generate.Types.Referencable where

import Data.Text (Text)
import Data.Yaml

-- | Represents either a reference or a concrete value
data Referencable a
  = -- | A reference with the JSON reference string pointing to the referenced target
    Reference Text
  | -- | A concrete value which can be used directly
    Concrete a
  deriving (Show, Eq, Ord)

instance FromJSON a => FromJSON (Referencable a) where
  parseJSON (Object v) = do
    maybeReference <- v .:? "$ref"
    case maybeReference of
      (Just reference) -> pure (Reference reference)
      Nothing -> Concrete <$> parseJSON (Object v)
  parseJSON v = Concrete <$> parseJSON v
