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
  { schemaObjectType :: SchemaType,
    schemaObjectTitle :: Maybe Text,
    schemaObjectMultipleOf :: Maybe Integer,
    schemaObjectMaximum :: Maybe Float,
    schemaObjectExclusiveMaximum :: Bool,
    schemaObjectMinimum :: Maybe Float,
    schemaObjectExclusiveMinimum :: Bool,
    schemaObjectMaxLength :: Maybe Word,
    schemaObjectMinLength :: Maybe Word,
    schemaObjectPattern :: Maybe Text,
    schemaObjectMaxItems :: Maybe Word,
    schemaObjectMinItems :: Maybe Word,
    schemaObjectUniqueItems :: Bool,
    schemaObjectMaxProperties :: Maybe Word,
    schemaObjectMinProperties :: Maybe Word,
    schemaObjectRequired :: Set Text,
    schemaObjectEnum :: [Value],
    schemaObjectAllOf :: [Schema],
    schemaObjectOneOf :: [Schema],
    schemaObjectAnyOf :: [Schema],
    schemaObjectNot :: Maybe Schema,
    schemaObjectProperties :: Map.Map Text Schema,
    schemaObjectAdditionalProperties :: AdditionalProperties,
    schemaObjectDescription :: Maybe Text,
    schemaObjectFormat :: Maybe Text,
    -- default would have the same value type as restricted by
    -- the schema. Stripe only uses Text default values
    schemaObjectDefault :: Maybe ConcreteValue,
    schemaObjectNullable :: Bool,
    schemaObjectDiscriminator :: Maybe DiscriminatorObject,
    schemaObjectReadOnly :: Bool,
    schemaObjectWriteOnly :: Bool,
    schemaObjectXml :: Maybe XMLObject,
    schemaObjectExternalDocs :: Maybe ExternalDocumentationObject,
    schemaObjectExample :: Maybe Value,
    schemaObjectDeprecated :: Bool,
    schemaObjectItems :: Maybe Schema
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
    { schemaObjectType = SchemaTypeObject,
      schemaObjectTitle = Nothing,
      schemaObjectMultipleOf = Nothing,
      schemaObjectMaximum = Nothing,
      schemaObjectExclusiveMaximum = False,
      schemaObjectMinimum = Nothing,
      schemaObjectExclusiveMinimum = False,
      schemaObjectMaxLength = Nothing,
      schemaObjectMinLength = Nothing,
      schemaObjectPattern = Nothing,
      schemaObjectMaxItems = Nothing,
      schemaObjectMinItems = Nothing,
      schemaObjectUniqueItems = False,
      schemaObjectMaxProperties = Nothing,
      schemaObjectMinProperties = Nothing,
      schemaObjectRequired = Set.empty,
      schemaObjectEnum = [],
      schemaObjectAllOf = [],
      schemaObjectOneOf = [],
      schemaObjectAnyOf = [],
      schemaObjectNot = Nothing,
      schemaObjectProperties = Map.empty,
      schemaObjectAdditionalProperties = HasAdditionalProperties,
      schemaObjectDescription = Nothing,
      schemaObjectFormat = Nothing,
      schemaObjectDefault = Nothing,
      schemaObjectNullable = False,
      schemaObjectDiscriminator = Nothing,
      schemaObjectReadOnly = False,
      schemaObjectWriteOnly = False,
      schemaObjectXml = Nothing,
      schemaObjectExternalDocs = Nothing,
      schemaObjectExample = Nothing,
      schemaObjectDeprecated = False,
      schemaObjectItems = Nothing
    }

-- | Checks if the given schema is an empty object schema (without properties)
isSchemaEmpty :: SchemaObject -> Bool
isSchemaEmpty s =
  SchemaTypeObject == schemaObjectType s
    && Map.null (schemaObjectProperties s)
    && null (schemaObjectAllOf s)
    && null (schemaObjectOneOf s)
    && null (schemaObjectAnyOf s)

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
  { discriminatorObjectPropertyName :: Text,
    discriminatorObjectMapping :: Map.Map Text Text
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
  { xMLObjectName :: Maybe Text,
    xMLObjectNamespace :: Maybe Text,
    xMLObjectPrefix :: Maybe Text,
    xMLObjectAttribute :: Bool,
    xMLObjectWrapped :: Bool
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
