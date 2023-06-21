{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml
import GHC.Generics
import OpenAPI.Common (jsonObjectToList)
import OpenAPI.Generate.Types.ExternalDocumentation
import OpenAPI.Generate.Types.Referencable
import OpenAPI.Generate.Types.Schema (Schema)
import Text.Read (readMaybe)

data OpenApiSpecification = OpenApiSpecification
  { openApiSpecificationOpenapi :: Text,
    openApiSpecificationInfo :: InfoObject,
    openApiSpecificationServers :: [ServerObject],
    openApiSpecificationPaths :: PathsObject,
    openApiSpecificationComponents :: ComponentsObject,
    openApiSpecificationSecurity :: [SecurityRequirementObject],
    openApiSpecificationTags :: [TagObject],
    openApiSpecificationExternalDocs :: Maybe ExternalDocumentationObject
  }
  deriving (Show, Eq, Generic)

instance FromJSON OpenApiSpecification where
  parseJSON = withObject "OpenApiSpecification" $ \o ->
    OpenApiSpecification
      <$> o .: "openapi"
      <*> o .: "info"
      <*> o .:? "servers" .!= []
      <*> o .: "paths"
      <*> o
        .:? "components"
        .!= ComponentsObject
          { componentsObjectSchemas = Map.empty,
            componentsObjectResponses = Map.empty,
            componentsObjectParameters = Map.empty,
            componentsObjectExamples = Map.empty,
            componentsObjectRequestBodies = Map.empty,
            componentsObjectHeaders = Map.empty,
            componentsObjectSecuritySchemes = Map.empty
          }
      <*> o .:? "security" .!= []
      <*> o .:? "tags" .!= []
      <*> o .:? "externalDocs"

data InfoObject = InfoObject
  { infoObjectTitle :: Text,
    infoObjectDescription :: Maybe Text,
    infoObjectTermsOfService :: Maybe Text,
    infoObjectContact :: Maybe ContactObject,
    infoObjectLicense :: Maybe LicenseObject,
    infoObjectVersion :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON InfoObject where
  parseJSON = withObject "InfoObject" $ \o ->
    InfoObject
      <$> o .: "title"
      <*> o .:? "description"
      <*> o .:? "termsOfService"
      <*> o .:? "contact"
      <*> o .:? "license"
      <*> o .: "version"

data ContactObject = ContactObject
  { contactObjectName :: Maybe Text,
    contactObjectUrl :: Maybe Text,
    contactObjectEmail :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ContactObject where
  parseJSON = withObject "ContactObject" $ \o ->
    ContactObject
      <$> o .:? "name"
      <*> o .:? "url"
      <*> o .:? "email"

data LicenseObject = LicenseObject
  { licenseObjectName :: Text,
    licenseObjectUrl :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON LicenseObject where
  parseJSON = withObject "LicenseObject" $ \o ->
    LicenseObject
      <$> o .: "name"
      <*> o .:? "url"

type PathsObject = Map.Map Text PathItemObject

data PathItemObject = PathItemObject
  { pathItemObjectRef :: Maybe Text,
    pathItemObjectSummary :: Maybe Text,
    pathItemObjectDescription :: Maybe Text,
    pathItemObjectGet :: Maybe OperationObject,
    pathItemObjectPut :: Maybe OperationObject,
    pathItemObjectPost :: Maybe OperationObject,
    pathItemObjectDelete :: Maybe OperationObject,
    pathItemObjectOptions :: Maybe OperationObject,
    pathItemObjectHead :: Maybe OperationObject,
    pathItemObjectPatch :: Maybe OperationObject,
    pathItemObjectTrace :: Maybe OperationObject,
    pathItemObjectServers :: [ServerObject],
    pathItemObjectParameters :: [Referencable ParameterObject]
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

data OperationObject = OperationObject
  { operationObjectTags :: [Text],
    operationObjectSummary :: Maybe Text,
    operationObjectDescription :: Maybe Text,
    operationObjectExternalDocs :: Maybe ExternalDocumentationObject,
    operationObjectOperationId :: Maybe Text,
    operationObjectParameters :: [Referencable ParameterObject],
    operationObjectRequestBody :: Maybe (Referencable RequestBodyObject),
    operationObjectResponses :: ResponsesObject,
    operationObjectDeprecated :: Bool,
    operationObjectSecurity :: [SecurityRequirementObject],
    operationObjectServers :: [ServerObject]
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

data RequestBodyObject = RequestBodyObject
  { requestBodyObjectContent :: Map.Map Text MediaTypeObject,
    requestBodyObjectDescription :: Maybe Text,
    requestBodyObjectRequired :: Bool
  }
  deriving (Show, Eq, Generic)

instance FromJSON RequestBodyObject where
  parseJSON = withObject "RequestBodyObject" $ \o ->
    RequestBodyObject
      <$> o .: "content"
      <*> o .:? "description"
      <*> o .:? "required" .!= False

data MediaTypeObject = MediaTypeObject
  { mediaTypeObjectSchema :: Maybe Schema,
    mediaTypeObjectExample :: Maybe Value,
    mediaTypeObjectExamples :: Map.Map Text (Referencable ExampleObject),
    mediaTypeObjectEncoding :: Map.Map Text EncodingObject
  }
  deriving (Show, Eq, Generic)

instance FromJSON MediaTypeObject where
  parseJSON = withObject "MediaTypeObject" $ \o ->
    MediaTypeObject
      <$> o .:? "schema"
      <*> o .:? "example"
      <*> o .:? "examples" .!= Map.empty
      <*> o .:? "encoding" .!= Map.empty

data ExampleObject = ExampleObject
  { exampleObjectSummary :: Maybe Text,
    exampleObjectDescription :: Maybe Text,
    exampleObjectValue :: Maybe Value, -- value and externalValue are mutually exclusive, maybe this should be encoded in this data type
    exampleObjectExternalValue :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ExampleObject where
  parseJSON = withObject "ExampleObject" $ \o ->
    ExampleObject
      <$> o .:? "summary"
      <*> o .:? "description"
      <*> o .:? "value"
      <*> o .:? "externalValue"

data EncodingObject = EncodingObject
  { encodingObjectContentType :: Maybe Text,
    encodingObjectHeaders :: Map.Map Text (Referencable HeaderObject),
    encodingObjectStyle :: Maybe Text,
    encodingObjectExplode :: Bool,
    encodingObjectAllowReserved :: Bool
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

data ResponsesObject = ResponsesObject
  { responsesObjectDefault :: Maybe (Referencable ResponseObject),
    responsesObjectRange1XX :: Maybe (Referencable ResponseObject),
    responsesObjectRange2XX :: Maybe (Referencable ResponseObject),
    responsesObjectRange3XX :: Maybe (Referencable ResponseObject),
    responsesObjectRange4XX :: Maybe (Referencable ResponseObject),
    responsesObjectRange5XX :: Maybe (Referencable ResponseObject),
    responsesObjectPerStatusCode :: Map.Map Int (Referencable ResponseObject)
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
            $ jsonObjectToList o
        )

data ResponseObject = ResponseObject
  { responseObjectDescription :: Text,
    responseObjectHeaders :: Map.Map Text (Referencable HeaderObject),
    responseObjectContent :: Map.Map Text MediaTypeObject
    -- links (http://spec.openapis.org/oas/v3.0.3#fixed-fields-14) are omitted because they are not needed
  }
  deriving (Show, Eq, Generic)

instance FromJSON ResponseObject where
  parseJSON = withObject "ResponseObject" $ \o ->
    ResponseObject
      <$> o .: "description"
      <*> o .:? "headers" .!= Map.empty
      <*> o .:? "content" .!= Map.empty

data ServerObject = ServerObject
  { serverObjectUrl :: Text,
    serverObjectDescription :: Maybe Text,
    serverObjectVariables :: Map.Map Text ServerVariableObject
  }
  deriving (Show, Eq, Generic)

instance FromJSON ServerObject where
  parseJSON = withObject "ServerObject" $ \o ->
    ServerObject
      <$> o .: "url"
      <*> o .:? "description"
      <*> o .:? "variables" .!= Map.empty

data ServerVariableObject = ServerVariableObject
  { serverVariableObjectEnum :: [Text],
    serverVariableObjectDefault :: Text,
    serverVariableObjectDescription :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON ServerVariableObject where
  parseJSON = withObject "ServerVariableObject" $ \o ->
    ServerVariableObject
      <$> o .:? "enum" .!= []
      <*> o .: "default"
      <*> o .:? "description"

data ParameterObject = ParameterObject
  { parameterObjectName :: Text,
    parameterObjectIn :: ParameterObjectLocation,
    parameterObjectDescription :: Maybe Text,
    parameterObjectRequired :: Bool,
    parameterObjectDeprecated :: Bool,
    parameterObjectAllowEmptyValue :: Bool,
    parameterObjectSchema :: ParameterObjectSchema
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

data ParameterObjectLocation
  = QueryParameterObjectLocation
  | HeaderParameterObjectLocation
  | PathParameterObjectLocation
  | CookieParameterObjectLocation
  deriving (Show, Eq, Generic)

instance FromJSON ParameterObjectLocation where
  parseJSON = withText "ParameterObjectLocation" $ \case
    "query" -> pure QueryParameterObjectLocation
    "header" -> pure HeaderParameterObjectLocation
    "path" -> pure PathParameterObjectLocation
    "cookie" -> pure CookieParameterObjectLocation
    _ -> fail "A ParameterObject must have a value of 'query', 'header', 'path' or 'cookie' in the property 'in'."

data ParameterObjectSchema
  = SimpleParameterObjectSchema SimpleParameterSchema
  | ComplexParameterObjectSchema (Map.Map Text MediaTypeObject)
  deriving (Show, Eq, Generic)

instance FromJSON ParameterObjectSchema where
  parseJSON = withObject "ParameterObjectSchema" $ \o -> do
    maybeSchema <- o .:? "schema"
    maybeContent <- o .:? "content"
    case (maybeSchema :: Maybe Schema, maybeContent) of
      (Just _, Nothing) -> SimpleParameterObjectSchema <$> parseJSON (Object o)
      (Nothing, Just content') -> pure $ ComplexParameterObjectSchema content'
      (Just _, Just _) -> fail "ParameterObject (http://spec.openapis.org/oas/v3.0.3#parameter-object) only allows one of the properties schema and content."
      (Nothing, Nothing) -> fail "ParameterObject (http://spec.openapis.org/oas/v3.0.3#parameter-object) requires one of the properties schema and content to be present."

data SimpleParameterSchema = SimpleParameterSchema
  { simpleParameterSchemaStyle :: Maybe Text,
    simpleParameterSchemaExplode :: Bool,
    simpleParameterSchemaAllowReserved :: Bool,
    simpleParameterSchemaSchema :: Schema,
    simpleParameterSchemaExample :: Maybe Value,
    simpleParameterSchemaExamples :: Map.Map Text (Referencable ExampleObject)
  }
  deriving (Show, Eq, Generic)

instance FromJSON SimpleParameterSchema where
  parseJSON = withObject "SimpleParameterSchema" $ \o -> do
    maybeStyle <- o .:? "style"
    SimpleParameterSchema
      <$> o .:? "style"
      <*> o .:? "explode" .!= ((maybeStyle :: Maybe Text) == Just "form") -- The default value is true for form and false otherwise (http://spec.openapis.org/oas/v3.0.3#parameterExplode)
      <*> o .:? "allowReserved" .!= False
      <*> o .: "schema"
      <*> o .:? "example"
      <*> o .:? "examples" .!= Map.empty

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

data ComponentsObject = ComponentsObject
  { componentsObjectSchemas :: Map.Map Text Schema,
    componentsObjectResponses :: Map.Map Text (Referencable ResponseObject),
    componentsObjectParameters :: Map.Map Text (Referencable ParameterObject),
    componentsObjectExamples :: Map.Map Text (Referencable ExampleObject),
    componentsObjectRequestBodies :: Map.Map Text (Referencable RequestBodyObject),
    componentsObjectHeaders :: Map.Map Text (Referencable HeaderObject),
    componentsObjectSecuritySchemes :: Map.Map Text (Referencable SecuritySchemeObject)
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

data ApiKeySecurityScheme = ApiKeySecurityScheme
  { apiKeySecuritySchemeDescription :: Maybe Text,
    apiKeySecuritySchemeName :: Text,
    apiKeySecuritySchemeIn :: ApiKeySecuritySchemeLocation
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

data HttpSecurityScheme = HttpSecurityScheme
  { httpSecuritySchemeDescription :: Maybe Text,
    httpSecuritySchemeScheme :: Text,
    httpSecuritySchemeBearerFormat :: Maybe Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON HttpSecurityScheme where
  parseJSON = withObject "HttpSecurityScheme" $ \o ->
    HttpSecurityScheme
      <$> o .:? "description"
      <*> o .: "scheme"
      <*> o .:? "bearerFormat"

data OAuth2SecurityScheme = OAuth2SecurityScheme
  { oAuth2SecuritySchemeDescription :: Maybe Text,
    oAuth2SecuritySchemeFlows :: OAuthFlowsObject
  }
  deriving (Show, Eq, Generic)

instance FromJSON OAuth2SecurityScheme where
  parseJSON = withObject "OAuth2SecurityScheme" $ \o ->
    OAuth2SecurityScheme
      <$> o .:? "description"
      <*> o .: "flows"

data OAuthFlowsObject = OAuthFlowsObject
  { oAuthFlowsObjectImplicit :: Maybe OAuthFlowObject,
    oAuthFlowsObjectPassword :: Maybe OAuthFlowObject,
    oAuthFlowsObjectClientCredentials :: Maybe OAuthFlowObject,
    oAuthFlowsObjectAuthorizationCode :: Maybe OAuthFlowObject
  }
  deriving (Show, Eq, Generic)

instance FromJSON OAuthFlowsObject where
  parseJSON = withObject "OAuthFlowsObject" $ \o ->
    OAuthFlowsObject
      <$> o .:? "implicit"
      <*> o .:? "password"
      <*> o .:? "clientCredentials"
      <*> o .:? "authorizationCode"

data OAuthFlowObject = OAuthFlowObject
  { oAuthFlowObjectAuthorizationUrl :: Maybe Text, -- applies only to implicit and authorizationCode
    oAuthFlowObjectTokenUrl :: Maybe Text, -- applies only to password, clientCredentials and authorizationCode
    oAuthFlowObjectRefreshUrl :: Maybe Text,
    oAuthFlowObjectScopes :: Map.Map Text Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON OAuthFlowObject where
  parseJSON = withObject "OAuthFlowObject" $ \o ->
    OAuthFlowObject
      <$> o .:? "authorizationUrl"
      <*> o .:? "tokenUrl"
      <*> o .:? "refreshUrl"
      <*> o .: "scopes"

data OpenIdConnectSecurityScheme = OpenIdConnectSecurityScheme
  { openIdConnectSecuritySchemeDescription :: Maybe Text,
    openIdConnectSecuritySchemeOpenIdConnectUrl :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON OpenIdConnectSecurityScheme where
  parseJSON = withObject "OpenIdConnectSecurityScheme" $ \o ->
    OpenIdConnectSecurityScheme
      <$> o .:? "description"
      <*> o .: "openIdConnectUrl"

data TagObject = TagObject
  { tagObjectName :: Text,
    tagObjectDescription :: Maybe Text,
    tagObjectExternalDocs :: Maybe ExternalDocumentationObject
  }
  deriving (Show, Eq, Generic)

instance FromJSON TagObject where
  parseJSON = withObject "TagObject" $ \o ->
    TagObject
      <$> o .: "name"
      <*> o .:? "description"
      <*> o .:? "externalDocs"
