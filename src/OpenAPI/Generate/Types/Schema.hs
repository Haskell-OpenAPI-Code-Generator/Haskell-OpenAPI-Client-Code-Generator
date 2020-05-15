{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This module specifies the data types from the OpenAPI specification 3.0.3 Schema
--
-- For more information see http://spec.openapis.org/oas/v3.0.3
-- and https://json-schema.org/
--
-- All names in this module correspond to the respective OpenAPI types
module OpenAPI.Generate.Types.Schema where

import Control.Applicative
import Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Scientific
import Data.Set as Set
import qualified Data.Text as T
import Data.Word
import Data.Yaml
import GHC.Generics
import OpenAPI.Generate.Types.Doc
import OpenAPI.Generate.Types.Referencable (Referencable)
import Text.Read (readMaybe)

type Schema = Referencable SchemaObject

data SchemaObject
  = SchemaObject
      { type' :: SchemaType,
        title :: Maybe T.Text,
        multipleOf :: Maybe Integer,
        maximum :: Maybe Float,
        exclusiveMaximum :: Maybe Float,
        minimum :: Maybe Float,
        exclusiveMinimum :: Maybe Float,
        maxLength :: Maybe Word,
        minLength :: Maybe Word,
        pattern' :: Maybe T.Text,
        maxItems :: Maybe Word,
        minItems :: Maybe Word,
        uniqueItems :: Maybe Bool,
        maxProperties :: Maybe Word,
        minProperties :: Maybe Word,
        required :: Set T.Text,
        enum :: Set Value,
        allOf :: Set Schema,
        oneOf :: Set Schema,
        anyOf :: Set Schema,
        not :: Maybe Schema,
        properties :: Map T.Text Schema,
        additionalProperties :: AdditionalProperties,
        description :: Maybe T.Text,
        format :: Maybe T.Text,
        -- default would have the same value type as restricted by
        -- the schema. Stripe only uses Text default values
        default' :: Maybe ConcreteValue,
        nullable :: Bool,
        discriminator :: Maybe DiscriminatorObject,
        readOnly :: Bool,
        writeOnly :: Bool,
        xml :: Maybe XMLObject,
        externalDocs :: Maybe ExternalDocumentationObject,
        -- not correct
        example :: Maybe T.Text,
        deprecated :: Bool,
        items :: Maybe Schema
      }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON SchemaObject where
  parseJSON = withObject "SchemaObject" $ \o ->
    SchemaObject
      <$> o .:? "type" .!= SchemaTypeObject
      <*> o .:? "title"
      <*> o .:? "multipleOf"
      <*> o .:? "maximum"
      <*> o .:? "exclusiveMaximum"
      <*> o .:? "minimum"
      <*> o .:? "exclusiveMinimum"
      <*> o .:? "maxLength"
      <*> o .:? "minLength"
      <*> o .:? "pattern"
      <*> o .:? "maxItems"
      <*> o .:? "minItems"
      <*> o .:? "uniqueItems"
      <*> o .:? "maxProperties"
      <*> o .:? "minProperties"
      <*> o .:? "required" .!= Set.empty
      <*> o .:? "enum" .!= Set.empty
      <*> o .:? "allOf" .!= Set.empty
      <*> o .:? "oneOf" .!= Set.empty
      <*> o .:? "anyOf" .!= Set.empty
      <*> o .:? "not"
      <*> o .:? "properties" .!= Map.empty
      <*> o .:? "additionalProperties" .!= HasAdditionalProperties
      <*> o .:? "description"
      <*> o .:? "format"
      <*> o .:? "default"
      <*> o .:? "nullable" .!= False
      <*> o .:? "discriminator"
      <*> o .:? "readOnly" .!= False
      <*> o .:? "writeOnly" .!= False
      <*> o .:? "xml"
      <*> o .:? "externalDocs"
      <*> o .:? "example"
      <*> o .:? "deprecated" .!= False
      <*> o .:? "items"

data SchemaType
  = SchemaTypeString
  | SchemaTypeNumber
  | SchemaTypeInteger
  | SchemaTypeBool
  | SchemaTypeObject
  | SchemaTypeArray
  deriving (Show, Eq, Ord, Generic)

instance FromJSON SchemaType where
  parseJSON (String "integer") = pure SchemaTypeInteger
  parseJSON (String "string") = pure SchemaTypeString
  parseJSON (String "number") = pure SchemaTypeNumber
  parseJSON (String "boolean") = pure SchemaTypeBool
  parseJSON (String "array") = pure SchemaTypeArray
  parseJSON (String "object") = pure SchemaTypeObject
  parseJSON (String x) = fail $ "Only types integer, string, number, bool, array and object are supported but got: " <> T.unpack x
  parseJSON _ = fail "type must be of type string"

data DiscriminatorObject
  = DiscriminatorObject
      { propertyName :: T.Text,
        mapping :: Map T.Text T.Text
      }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON DiscriminatorObject

-- So that Sets are possible
instance Ord Value where
  (Object a) `compare` (Object b) = compare a b
  (Array a) `compare` (Array b) = compare a b
  (String a) `compare` (String b) = compare a b
  (Number a) `compare` (Number b) = compare a b
  (Bool a) `compare` (Bool b) = compare a b
  Null `compare` Null = EQ
  (Object a) `compare` _ = GT
  _ `compare` (Object a) = LT
  (Array a) `compare` _ = GT
  _ `compare` (Array a) = LT
  (String a) `compare` _ = GT
  _ `compare` (String a) = LT
  (Number a) `compare` _ = GT
  _ `compare` (Number a) = LT
  (Bool a) `compare` _ = GT
  _ `compare` (Bool a) = LT

data ConcreteValue
  = StringDefaultValue T.Text
  | NumericDefaultValue Scientific
  | BoolDefaultValue Bool
  deriving (Show, Eq, Ord, Generic)

instance FromJSON ConcreteValue where
  parseJSON v@(String _) = StringDefaultValue <$> parseJSON v
  parseJSON v@(Number _) = NumericDefaultValue <$> parseJSON v
  parseJSON v@(Bool _) = BoolDefaultValue <$> parseJSON v
  parseJSON x = fail "only text and numeric values are supported for a default value as of now"

data AdditionalProperties
  = NoAdditionalProperties
  | HasAdditionalProperties
  | AdditionalPropertiesWithSchema Schema
  deriving (Show, Eq, Ord, Generic)

instance FromJSON AdditionalProperties where
  parseJSON (Bool False) = pure NoAdditionalProperties
  parseJSON (Bool True) = pure HasAdditionalProperties
  parseJSON v = AdditionalPropertiesWithSchema <$> parseJSON v

data XMLObject
  = XMLObject
      { name :: T.Text,
        namespace :: Maybe T.Text,
        prefix :: Maybe T.Text,
        attribute :: Bool,
        wrapped :: Bool
      }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON XMLObject where
  parseJSON = withObject "SchemaObject" $ \o ->
    XMLObject
      <$> o .: "name"
      <*> o .:? "namespace"
      <*> o .:? "prefix"
      <*> o .:? "attribute" .!= False
      <*> o .:? "wrapped" .!= False
