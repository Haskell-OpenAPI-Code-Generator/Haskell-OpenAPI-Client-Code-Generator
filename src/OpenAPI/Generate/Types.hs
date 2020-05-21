{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

-- | This module specifies the data types from the OpenAPI specification 3.0.3
--
-- For more information see http://spec.openapis.org/oas/v3.0.3
--
-- All names in this module correspond to the respective OpenAPI types
module OpenAPI.Generate.Types
  ( module OpenAPI.Generate.Types.ExternalDocumentation,
    module OpenAPI.Generate.Types.Referencable,
    module OpenAPI.Generate.Types.Schema,
    module OpenAPI.Generate.Types,
  )
where

import qualified Data.HashMap.Strict as HMap
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml
import GHC.Generics
import OpenAPI.Generate.Types.ExternalDocumentation
import OpenAPI.Generate.Types.Referencable
import OpenAPI.Generate.Types.Schema (Schema)
import Text.Read (readMaybe)

data OpenApiSpecification
  = OpenApiSpecification
      { openapi :: Text,
        info :: InfoObject,
        servers :: [ServerObject],
        paths :: PathsObject,
        components :: ComponentsObject,
        security :: [SecurityRequirementObject],
        tags :: [TagObject],
        externalDocs :: Maybe ExternalDocumentationObject
      }
  deriving (Show, Eq, Generic)

instance FromJSON OpenApiSpecification where
  parseJSON = withObject "OpenApiSpecification" $ \o ->
    OpenApiSpecification
      <$> o .: "openapi"
      <*> o .: "info"
      <*> o .:? "servers" .!= []
      <*> o .: "paths"
      <*> o .:? "components"
        .!= ComponentsObject
          { schemas = Map.empty,
            responses = Map.empty,
            parameters = Map.empty,
            examples = Map.empty,
            requestBodies = Map.empty,
            headers = Map.empty,
            securitySchemes = Map.empty
          }
      <*> o .:? "security" .!= []
      <*> o .:? "tags" .!= []
      <*> o .:? "externalDocs"

data InfoObject
  = InfoObject
      { title :: Text,
        description :: Maybe Text,
        termsOfService :: Maybe Text,
        contact :: Maybe ContactObject,
        license :: Maybe LicenseObject,
        version :: Text
      }
  deriving (Show, Eq, Generic)

instance FromJSON InfoObject

data ContactObject
  = ContactObject
      { name :: Maybe Text,
        url :: Maybe Text,
        email :: Maybe Text
      }
  deriving (Show, Eq, Generic)

instance FromJSON ContactObject

data LicenseObject
  = LicenseObject
      { name :: Text,
        url :: Maybe Text
      }
  deriving (Show, Eq, Generic)

instance FromJSON LicenseObject

type PathsObject = Map.Map Text PathItemObject

data PathItemObject
  = PathItemObject
      { ref :: Maybe Text,
        summary :: Maybe Text,
        description :: Maybe Text,
        get :: Maybe OperationObject,
        put :: Maybe OperationObject,
        post :: Maybe OperationObject,
        delete :: Maybe OperationObject,
        options :: Maybe OperationObject,
        head :: Maybe OperationObject,
        patch :: Maybe OperationObject,
        trace :: Maybe OperationObject,
        servers :: [ServerObject],
        parameters :: [Referencable ParameterObject]
      }
  deriving (Show, Eq, Generic)

instance FromJSON PathItemObject where
  parseJSON = withObject "PathItemObject" $ \o ->
    PathItemObject
      <$> o .:? "ref"
      <*> o .:? "summary"
      <*> o .:? "description"
      <*> o .:? "get"
      <*> o .:? "put"
      <*> o .:? "post"
      <*> o .:? "delete"
      <*> o .:? "options"
      <*> o .:? "head"
      <*> o .:? "patch"
      <*> o .:? "trace"
      <*> o .:? "servers" .!= []
      <*> o .:? "parameters" .!= []

data OperationObject
  = OperationObject
      { tags :: [Text],
        summary :: Maybe Text,
        description :: Maybe Text,
        externalDocs :: Maybe ExternalDocumentationObject,
        operationId :: Maybe Text,
        parameters :: [Referencable ParameterObject],
        requestBody :: Maybe (Referencable RequestBodyObject),
        responses :: ResponsesObject,
        deprecated :: Bool,
        security :: [SecurityRequirementObject],
        servers :: [ServerObject]
        -- callbacks (http://spec.openapis.org/oas/v3.0.3#operation-object) are omitted because they are not needed
      }
  deriving (Show, Eq, Generic)

instance FromJSON OperationObject where
  parseJSON = withObject "OperationObject" $ \o ->
    OperationObject
      <$> o .:? "tags" .!= []
      <*> o .:? "summary"
      <*> o .:? "description"
      <*> o .:? "externalDocs"
      <*> o .:? "operationId"
      <*> o .:? "parameters" .!= []
      <*> o .:? "requestBody"
      <*> o .: "responses"
      <*> o .:? "deprecated" .!= False
      <*> o .:? "security" .!= []
      <*> o .:? "servers" .!= []

type SecurityRequirementObject = Map.Map Text [Text]

data RequestBodyObject
  = RequestBodyObject
      { content :: Map.Map Text MediaTypeObject,
        description :: Maybe Text,
        required :: Bool
      }
  deriving (Show, Eq, Generic)

instance FromJSON RequestBodyObject where
  parseJSON = withObject "RequestBodyObject" $ \o ->
    RequestBodyObject
      <$> o .: "content"
      <*> o .:? "description"
      <*> o .:? "required" .!= False

data MediaTypeObject
  = MediaTypeObject
      { schema :: Maybe Schema,
        example :: Maybe Value,
        examples :: Map.Map Text (Referencable ExampleObject),
        encoding :: Map.Map Text EncodingObject
      }
  deriving (Show, Eq, Generic)

instance FromJSON MediaTypeObject where
  parseJSON = withObject "MediaTypeObject" $ \o ->
    MediaTypeObject
      <$> o .:? "schema"
      <*> o .:? "example"
      <*> o .:? "examples" .!= Map.empty
      <*> o .:? "encoding" .!= Map.empty

data ExampleObject
  = ExampleObject
      { summary :: Maybe Text,
        description :: Maybe Text,
        value :: Maybe Value, -- value and externalValue are mutually exclusive, maybe this should be encoded in this data type
        externalValue :: Maybe Text
      }
  deriving (Show, Eq, Generic)

instance FromJSON ExampleObject

data EncodingObject
  = EncodingObject
      { contentType :: Maybe Text,
        headers :: Map.Map Text (Referencable HeaderObject),
        style :: Maybe Text,
        explode :: Bool,
        allowReserved :: Bool
      }
  deriving (Show, Eq, Generic)

instance FromJSON EncodingObject where
  parseJSON = withObject "EncodingObject" $ \o ->
    EncodingObject
      <$> o .:? "contentType"
      <*> o .:? "headers" .!= Map.empty
      <*> o .:? "style"
      <*> o .:? "explode" .!= True
      <*> o .:? "allowReserved" .!= False

data ResponsesObject
  = ResponsesObject
      { default' :: Maybe (Referencable ResponseObject),
        range1XX :: Maybe (Referencable ResponseObject),
        range2XX :: Maybe (Referencable ResponseObject),
        range3XX :: Maybe (Referencable ResponseObject),
        range4XX :: Maybe (Referencable ResponseObject),
        range5XX :: Maybe (Referencable ResponseObject),
        perStatusCode :: Map.Map Int (Referencable ResponseObject)
      }
  deriving (Show, Eq, Generic)

instance FromJSON ResponsesObject where
  parseJSON = withObject "ResponsesObject" $ \o ->
    ResponsesObject
      <$> o .:? "default"
      <*> o .:? "1XX"
      <*> o .:? "2XX"
      <*> o .:? "3XX"
      <*> o .:? "4XX"
      <*> o .:? "5XX"
      <*> mapM
        parseJSON
        ( Map.fromList
            . filter (\(code, _) -> code >= 100 && code < 600)
            . Maybe.mapMaybe
              ( \(code, response) -> fmap (,response) . readMaybe . T.unpack $ code
              )
            $ HMap.toList o
        )

data ResponseObject
  = ResponseObject
      { description :: Text,
        headers :: Map.Map Text (Referencable HeaderObject),
        content :: Map.Map Text MediaTypeObject
        -- links (http://spec.openapis.org/oas/v3.0.3#fixed-fields-14) are omitted because they are not needed
      }
  deriving (Show, Eq, Generic)

instance FromJSON ResponseObject where
  parseJSON = withObject "ResponseObject" $ \o ->
    ResponseObject
      <$> o .: "description"
      <*> o .:? "headers" .!= Map.empty
      <*> o .:? "content" .!= Map.empty

data ServerObject
  = ServerObject
      { url :: Text,
        description :: Maybe Text,
        variables :: Map.Map Text ServerVariableObject
      }
  deriving (Show, Eq, Generic)

instance FromJSON ServerObject where
  parseJSON = withObject "ServerObject" $ \o ->
    ServerObject
      <$> o .: "url"
      <*> o .:? "description"
      <*> o .:? "variables" .!= Map.empty

data ServerVariableObject
  = ServerVariableObject
      { enum :: [Text],
        default' :: Text,
        description :: Maybe Text
      }
  deriving (Show, Eq, Generic)

instance FromJSON ServerVariableObject where
  parseJSON = withObject "ServerVariableObject" $ \o ->
    ServerVariableObject
      <$> o .: "enum"
      <*> o .: "default"
      <*> o .:? "description"

data ParameterObject
  = ParameterObject
      { name :: Text,
        in' :: ParameterObjectLocation,
        description :: Maybe Text,
        required :: Bool,
        deprecated :: Bool,
        allowEmptyValue :: Bool,
        schema :: ParameterObjectSchema
      }
  deriving (Show, Eq, Generic)

instance FromJSON ParameterObject where
  parseJSON = withObject "ParameterObject" $ \o ->
    ParameterObject
      <$> o .: "name"
      <*> o .: "in"
      <*> o .:? "description"
      <*> o .:? "required" .!= False
      <*> o .:? "deprecated" .!= False
      <*> o .:? "allowEmptyValue" .!= False
      <*> parseJSON (Object o)

data ParameterObjectLocation = QueryParameterObjectLocation | HeaderParameterObjectLocation | PathParameterObjectLocation | CookieParameterObjectLocation
  deriving (Show, Eq, Generic)

instance FromJSON ParameterObjectLocation where
  parseJSON = withText "ParameterObjectLocation" $ \case
    "query" -> pure QueryParameterObjectLocation
    "header" -> pure HeaderParameterObjectLocation
    "path" -> pure PathParameterObjectLocation
    "cookie" -> pure CookieParameterObjectLocation
    _ -> fail "A ParameterObject must have a value of 'query', 'header', 'path' or 'cookie' in the property 'in'."

data ParameterObjectSchema
  = SimpleParameterObjectSchema
      { style :: Maybe Text,
        explode :: Bool,
        allowReserved :: Bool,
        schema :: Schema,
        example :: Maybe Value,
        examples :: Map.Map Text (Referencable ExampleObject)
      }
  | ComplexParameterObjectSchema (Map.Map Text MediaTypeObject)
  deriving (Show, Eq, Generic)

instance FromJSON ParameterObjectSchema where
  parseJSON = withObject "ParameterObjectSchema" $ \o -> do
    maybeSchema <- o .:? "schema"
    maybeContent <- o .:? "content"
    case (maybeSchema, maybeContent) of
      (Just schema', Nothing) ->
        SimpleParameterObjectSchema
          <$> o .:? "style"
            <*> o .:? "explode" .!= True
            <*> o .:? "allowReserved" .!= False
            <*> pure schema'
            <*> o .:? "example"
            <*> o .:? "examples" .!= Map.empty
      (Nothing, Just content') -> pure $ ComplexParameterObjectSchema content'
      (Just _, Just _) -> fail "ParameterObject (http://spec.openapis.org/oas/v3.0.3#parameter-object) only allows one of the properties schema and content."
      (Nothing, Nothing) -> fail "ParameterObject (http://spec.openapis.org/oas/v3.0.3#parameter-object) requires one of the properties schema and content to be present."

newtype HeaderObject = HeaderObject ParameterObject
  deriving (Show, Eq, Generic)

instance FromJSON HeaderObject where
  parseJSON = withObject "HeaderObject" $ \o ->
    HeaderObject
      <$> ( ParameterObject "name MUST NOT be specified, it is given in the corresponding headers map" HeaderParameterObjectLocation
              <$> o .:? "description"
              <*> o .:? "required" .!= False
              <*> o .:? "deprecated" .!= False
              <*> o .:? "allowEmptyValue" .!= False
              <*> parseJSON (Object o)
          )

data ComponentsObject
  = ComponentsObject
      { schemas :: Map.Map Text Schema,
        responses :: Map.Map Text (Referencable ResponseObject),
        parameters :: Map.Map Text (Referencable ParameterObject),
        examples :: Map.Map Text (Referencable ExampleObject),
        requestBodies :: Map.Map Text (Referencable RequestBodyObject),
        headers :: Map.Map Text (Referencable HeaderObject),
        securitySchemes :: Map.Map Text (Referencable SecuritySchemeObject)
        -- links and callbacks are omitted because they are not supported in the generator
      }
  deriving (Show, Eq, Generic)

instance FromJSON ComponentsObject where
  parseJSON = withObject "ComponentsObject" $ \o ->
    ComponentsObject
      <$> o .:? "schemas" .!= Map.empty
      <*> o .:? "responses" .!= Map.empty
      <*> o .:? "parameters" .!= Map.empty
      <*> o .:? "examples" .!= Map.empty
      <*> o .:? "requestBodies" .!= Map.empty
      <*> o .:? "headers" .!= Map.empty
      <*> o .:? "securitySchemes" .!= Map.empty

data SecuritySchemeObject
  = ApiKeySecuritySchemeObject ApiKeySecurityScheme
  | HttpSecuritySchemeObject HttpSecurityScheme
  | OAuth2SecuritySchemeObject OAuth2SecurityScheme
  | OpenIdConnectSecuritySchemeObject OpenIdConnectSecurityScheme
  deriving (Show, Eq, Generic)

instance FromJSON SecuritySchemeObject where
  parseJSON = withObject "SecuritySchemeObject" $ \o -> do
    type' <- o .: "type"
    case (type' :: Text) of
      "apiKey" -> ApiKeySecuritySchemeObject <$> parseJSON (Object o)
      "http" -> HttpSecuritySchemeObject <$> parseJSON (Object o)
      "oauth2" -> OAuth2SecuritySchemeObject <$> parseJSON (Object o)
      "openIdConnect" -> OpenIdConnectSecuritySchemeObject <$> parseJSON (Object o)
      _ -> fail "A SecuritySchemeObject must have a value of 'apiKey', 'http', 'oauth2' or 'openIdConnect' in the property 'type'."

data ApiKeySecurityScheme
  = ApiKeySecurityScheme
      { description :: Maybe Text,
        name :: Text,
        in' :: ApiKeySecuritySchemeLocation
      }
  deriving (Show, Eq, Generic)

instance FromJSON ApiKeySecurityScheme where
  parseJSON = withObject "ApiKeySecurityScheme" $ \o ->
    ApiKeySecurityScheme
      <$> o .:? "description"
      <*> o .: "name"
      <*> o .: "in"

data ApiKeySecuritySchemeLocation = QueryApiKeySecuritySchemeLocation | HeaderApiKeySecuritySchemeLocation | CookieApiKeySecuritySchemeLocation
  deriving (Show, Eq, Generic)

instance FromJSON ApiKeySecuritySchemeLocation where
  parseJSON = withText "ApiKeySecuritySchemeLocation" $ \case
    "query" -> pure QueryApiKeySecuritySchemeLocation
    "header" -> pure HeaderApiKeySecuritySchemeLocation
    "cookie" -> pure CookieApiKeySecuritySchemeLocation
    _ -> fail "A SecuritySchemeObject with type 'apiKey' must have a value of 'query', 'header' or 'cookie' in the property 'in'."

data HttpSecurityScheme
  = HttpSecurityScheme
      { description :: Maybe Text,
        scheme :: Text,
        bearerFormat :: Maybe Text
      }
  deriving (Show, Eq, Generic)

instance FromJSON HttpSecurityScheme

data OAuth2SecurityScheme
  = OAuth2SecurityScheme
      { description :: Maybe Text,
        flows :: OAuthFlowsObject
      }
  deriving (Show, Eq, Generic)

instance FromJSON OAuth2SecurityScheme

data OAuthFlowsObject
  = OAuthFlowsObject
      { implicit :: Maybe OAuthFlowObject,
        password :: Maybe OAuthFlowObject,
        clientCredentials :: Maybe OAuthFlowObject,
        authorizationCode :: Maybe OAuthFlowObject
      }
  deriving (Show, Eq, Generic)

instance FromJSON OAuthFlowsObject

data OAuthFlowObject
  = OAuthFlowObject
      { authorizationUrl :: Maybe Text, -- applies only to implicit and authorizationCode
        tokenUrl :: Maybe Text, -- applies only to password, clientCredentials and authorizationCode
        refreshUrl :: Maybe Text,
        scopes :: Map.Map Text Text
      }
  deriving (Show, Eq, Generic)

instance FromJSON OAuthFlowObject

data OpenIdConnectSecurityScheme
  = OpenIdConnectSecurityScheme
      { description :: Maybe Text,
        openIdConnectUrl :: Text
      }
  deriving (Show, Eq, Generic)

instance FromJSON OpenIdConnectSecurityScheme

data TagObject
  = TagObject
      { name :: Text,
        description :: Maybe Text,
        externalDocs :: Maybe ExternalDocumentationObject
      }
  deriving (Show, Eq, Generic)

instance FromJSON TagObject
