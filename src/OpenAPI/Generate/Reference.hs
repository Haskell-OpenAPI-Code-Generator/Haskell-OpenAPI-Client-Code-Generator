{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Contains function to resolve references within the OpenAPI specification
module OpenAPI.Generate.Reference where

import Control.Monad
import Data.Bifunctor
import qualified Data.Map as Map
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics
import OpenAPI.Generate.Types
import OpenAPI.Generate.Types.Schema

-- | Represents all types the 'ReferenceMap' can hold
data ComponentReference
  = SchemaReference SchemaObject
  | ResponseReference ResponseObject
  | ParameterReference ParameterObject
  | ExampleReference ExampleObject
  | RequestBodyReference RequestBodyObject
  | HeaderReference HeaderObject
  | SecuritySchemeReference SecuritySchemeObject
  deriving (Show, Eq, Generic)

-- | A lookup table for references within the OpenAPI specification
type ReferenceMap = Map.Map Text ComponentReference

-- | Creates a 'ReferenceMap' from an 'OpenApiSpecification' containing all elements within components.
-- It does not capture possibly referenced locations anywhere else in the specification.
buildReferenceMap :: OpenApiSpecification -> ReferenceMap
buildReferenceMap =
  Map.fromList
    . ( \o ->
          buildReferencesForComponentType "schemas" SchemaReference (schemas o)
            <> buildReferencesForComponentType "responses" ResponseReference (responses (o :: ComponentsObject))
            <> buildReferencesForComponentType "parameters" ParameterReference (parameters (o :: ComponentsObject))
            <> buildReferencesForComponentType "examples" ExampleReference (examples (o :: ComponentsObject))
            <> buildReferencesForComponentType "requestBodies" RequestBodyReference (requestBodies o)
            <> buildReferencesForComponentType "headers" HeaderReference (headers (o :: ComponentsObject))
            <> buildReferencesForComponentType "securitySchemes" SecuritySchemeReference (securitySchemes o)
      )
    . components

-- | Maps the subtypes of components to the entries of the 'ReferenceMap' and filters references (the lookup table should only contain concrete values).
buildReferencesForComponentType ::
  Text ->
  (a -> ComponentReference) ->
  Map.Map Text (Referencable a) ->
  [(Text, ComponentReference)]
buildReferencesForComponentType componentName constructor =
  fmap (first (("#/components/" <> componentName <> "/") <>))
    . mapMaybe (convertReferencableToReference constructor)
    . Map.toList

convertReferencableToReference ::
  (a -> ComponentReference) ->
  (Text, Referencable a) ->
  Maybe (Text, ComponentReference)
convertReferencableToReference constructor (name, Concrete object) = Just (name, constructor object)
convertReferencableToReference _ (_, Reference _) = Nothing

getReference :: Text -> ReferenceMap -> Maybe ComponentReference
getReference = Map.lookup

createReferenceLookup :: (ComponentReference -> Maybe a) -> Text -> ReferenceMap -> Maybe a
createReferenceLookup conversionFn key = getReference key >=> conversionFn

-- | Resolve a 'SchemaObject' reference from a 'ReferenceMap'
getSchemaReference :: Text -> ReferenceMap -> Maybe SchemaObject
getSchemaReference = createReferenceLookup $ \case
  SchemaReference r -> Just r
  _ -> Nothing

-- | Resolve a 'ResponseObject' reference from a 'ReferenceMap'
getResponseReference :: Text -> ReferenceMap -> Maybe ResponseObject
getResponseReference = createReferenceLookup $ \case
  ResponseReference r -> Just r
  _ -> Nothing

-- | Resolve a 'ParameterObject' reference from a 'ReferenceMap'
getParameterReference :: Text -> ReferenceMap -> Maybe ParameterObject
getParameterReference = createReferenceLookup $ \case
  ParameterReference r -> Just r
  _ -> Nothing

-- | Resolve a 'ExampleObject' reference from a 'ReferenceMap'
getExampleReference :: Text -> ReferenceMap -> Maybe ExampleObject
getExampleReference = createReferenceLookup $ \case
  ExampleReference r -> Just r
  _ -> Nothing

-- | Resolve a 'RequestBodyObject' reference from a 'ReferenceMap'
getRequestBodyReference :: Text -> ReferenceMap -> Maybe RequestBodyObject
getRequestBodyReference = createReferenceLookup $ \case
  RequestBodyReference r -> Just r
  _ -> Nothing

-- | Resolve a 'HeaderObject' reference from a 'ReferenceMap'
getHeaderReference :: Text -> ReferenceMap -> Maybe HeaderObject
getHeaderReference = createReferenceLookup $ \case
  HeaderReference r -> Just r
  _ -> Nothing

-- | Resolve a 'SecuritySchemeObject' reference from a 'ReferenceMap'
getSecuritySchemeReference :: Text -> ReferenceMap -> Maybe SecuritySchemeObject
getSecuritySchemeReference = createReferenceLookup $ \case
  SecuritySchemeReference r -> Just r
  _ -> Nothing
