{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module specifies the data types from the OpenAPI specification 3.0.3 Schema
--
-- For more information see http://spec.openapis.org/oas/v3.0.3
-- and https://json-schema.org/
--
-- All names in this module correspond to the respective OpenAPI types
module OpenAPI.Generate.Types.Schema where

import qualified Data.Map as Map
import qualified Data.Scientific as Scientific
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml
import GHC.Generics
import OpenAPI.Generate.Types.ExternalDocumentation
import OpenAPI.Generate.Types.Referencable
import Prelude hiding (maximum, minimum, not)

type Schema = Referencable SchemaObject

data SchemaObject = SchemaObject
  { type' :: SchemaType,
    title :: Maybe Text,
    multipleOf :: Maybe Integer,
    maximum :: Maybe Float,
    exclusiveMaximum :: Bool,
    minimum :: Maybe Float,
    exclusiveMinimum :: Bool,
    maxLength :: Maybe Word,
    minLength :: Maybe Word,
    pattern' :: Maybe Text,
    maxItems :: Maybe Word,
    minItems :: Maybe Word,
    uniqueItems :: Bool,
    maxProperties :: Maybe Word,
    minProperties :: Maybe Word,
    required :: Set Text,
    enum :: [Value],
    allOf :: [Schema],
    oneOf :: [Schema],
    anyOf :: [Schema],
    not :: Maybe Schema,
    properties :: Map.Map Text Schema,
    additionalProperties :: AdditionalProperties,
    description :: Maybe Text,
    format :: Maybe Text,
    -- default would have the same value type as restricted by
    -- the schema. Stripe only uses Text default values
    default' :: Maybe ConcreteValue,
    nullable :: Bool,
    discriminator :: Maybe DiscriminatorObject,
    readOnly :: Bool,
    writeOnly :: Bool,
    xml :: Maybe XMLObject,
    externalDocs :: Maybe ExternalDocumentationObject,
    example :: Maybe Value,
    deprecated :: Bool,
    items :: Maybe Schema
  }
  deriving (Show, Eq, Generic)

instance FromJSON SchemaObject where
  parseJSON = withObject "SchemaObject" $ \o ->
    SchemaObject
      <$> o .:? "type" .!= SchemaTypeObject
      <*> o .:? "title"
      <*> o .:? "multipleOf"
      <*> o .:? "maximum"
      <*> o .:? "exclusiveMaximum" .!= False
      <*> o .:? "minimum"
      <*> o .:? "exclusiveMinimum" .!= False
      <*> o .:? "maxLength"
      <*> o .:? "minLength"
      <*> o .:? "pattern"
      <*> o .:? "maxItems"
      <*> o .:? "minItems"
      <*> o .:? "uniqueItems" .!= False
      <*> o .:? "maxProperties"
      <*> o .:? "minProperties"
      <*> o .:? "required" .!= Set.empty
      <*> o .:? "enum" .!= []
      <*> o .:? "allOf" .!= []
      <*> o .:? "oneOf" .!= []
      <*> o .:? "anyOf" .!= []
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

defaultSchema :: SchemaObject
defaultSchema =
  SchemaObject
    { type' = SchemaTypeObject,
      title = Nothing,
      multipleOf = Nothing,
      maximum = Nothing,
      exclusiveMaximum = False,
      minimum = Nothing,
      exclusiveMinimum = False,
      maxLength = Nothing,
      minLength = Nothing,
      pattern' = Nothing,
      maxItems = Nothing,
      minItems = Nothing,
      uniqueItems = False,
      maxProperties = Nothing,
      minProperties = Nothing,
      required = Set.empty,
      enum = [],
      allOf = [],
      oneOf = [],
      anyOf = [],
      not = Nothing,
      properties = Map.empty,
      additionalProperties = HasAdditionalProperties,
      OpenAPI.Generate.Types.Schema.description = Nothing,
      format = Nothing,
      default' = Nothing,
      nullable = False,
      discriminator = Nothing,
      readOnly = False,
      writeOnly = False,
      xml = Nothing,
      externalDocs = Nothing,
      example = Nothing,
      deprecated = False,
      items = Nothing
    }

-- | Checks if the given schema is an empty object schema (without properties)
isSchemaEmpty :: SchemaObject -> Bool
isSchemaEmpty s =
  SchemaTypeObject == type' s
    && Map.null (properties s)
    && null (allOf s)
    && null (oneOf s)
    && null (anyOf s)

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
  parseJSON (String x) = fail $ "Only types integer, string, number, boolean, array and object are supported but got: " <> T.unpack x
  parseJSON _ = fail "type must be of type string"

data DiscriminatorObject = DiscriminatorObject
  { propertyName :: Text,
    mapping :: Map.Map Text Text
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON DiscriminatorObject where
  parseJSON = withObject "DiscriminatorObject" $ \o ->
    DiscriminatorObject
      <$> o .: "propertyName"
      <*> o .:? "mapping" .!= Map.empty

data ConcreteValue
  = StringDefaultValue Text
  | NumericDefaultValue Scientific.Scientific
  | BoolDefaultValue Bool
  | OtherDefaultValue Value
  deriving (Show, Eq, Generic)

instance FromJSON ConcreteValue where
  parseJSON v@(String _) = StringDefaultValue <$> parseJSON v
  parseJSON v@(Number _) = NumericDefaultValue <$> parseJSON v
  parseJSON v@(Bool _) = BoolDefaultValue <$> parseJSON v
  parseJSON v = pure $ OtherDefaultValue v

data AdditionalProperties
  = NoAdditionalProperties
  | HasAdditionalProperties
  | AdditionalPropertiesWithSchema Schema
  deriving (Show, Eq, Generic)

instance FromJSON AdditionalProperties where
  parseJSON (Bool False) = pure NoAdditionalProperties
  parseJSON (Bool True) = pure HasAdditionalProperties
  parseJSON v = AdditionalPropertiesWithSchema <$> parseJSON v

data XMLObject = XMLObject
  { name :: Maybe Text,
    namespace :: Maybe Text,
    prefix :: Maybe Text,
    attribute :: Bool,
    wrapped :: Bool
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON XMLObject where
  parseJSON = withObject "SchemaObject" $ \o ->
    XMLObject
      <$> o .:? "name"
      <*> o .:? "namespace"
      <*> o .:? "prefix"
      <*> o .:? "attribute" .!= False
      <*> o .:? "wrapped" .!= False
