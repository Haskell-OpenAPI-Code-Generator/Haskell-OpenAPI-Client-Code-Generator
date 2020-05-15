{-# LANGUAGE OverloadedStrings #-}

module OpenAPI.Generate.Types.Referencable where

import qualified Data.Text as T
import Data.Yaml

data Referencable a = Reference T.Text | Concrete a
  deriving (Show, Eq, Ord)

instance FromJSON a => FromJSON (Referencable a) where
  parseJSON (Object v) = do
    maybeReference <- v .:? "$ref"
    case maybeReference of
      (Just reference) -> pure (Reference reference)
      Nothing -> Concrete <$> parseJSON (Object v)
  parseJSON v = Concrete <$> parseJSON v
