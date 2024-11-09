{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenAPI.Generate.OptParse.Configuration
  ( Configuration (..),
    getConfiguration,
  )
where

import Autodocodec
import Autodocodec.Yaml (readYamlConfigFile)
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import qualified Data.Text as T
import qualified OpenAPI.Generate.Log as OAL
import OpenAPI.Generate.OptParse.Types
import Path.IO

data Configuration = Configuration
  { configOpenApiSpecification :: !(Maybe Text),
    configOutputDir :: !(Maybe Text),
    configPackageName :: !(Maybe Text),
    configModuleName :: !(Maybe Text),
    configLogLevel :: !(Maybe OAL.LogSeverity),
    configForce :: !(Maybe Bool),
    configIncremental :: !(Maybe Bool),
    configDryRun :: !(Maybe Bool),
    configDoNotGenerateStackProject :: !(Maybe Bool),
    configGenerateNixFiles :: !(Maybe Bool),
    configOmitAdditionalOperationFunctions :: !(Maybe Bool),
    configGenerateOptionalEmptyRequestBody :: !(Maybe Bool),
    configUseNumberedVariantConstructors :: !(Maybe Bool),
    configUseFloatWithArbitraryPrecision :: !(Maybe Bool),
    configUseIntWithArbitraryPrecision :: !(Maybe Bool),
    configUseDateTypesAsString :: !(Maybe Bool),
    configConvertToCamelCase :: !(Maybe Bool),
    configPropertyTypeSuffix :: !(Maybe Text),
    configResponseTypeSuffix :: !(Maybe Text),
    configResponseBodyTypeSuffix :: !(Maybe Text),
    configRequestBodyTypeSuffix :: !(Maybe Text),
    configArrayItemTypeSuffix :: !(Maybe Text),
    configNonNullableTypeSuffix :: !(Maybe Text),
    configParametersTypeSuffix :: !(Maybe Text),
    configParameterQueryPrefix :: !(Maybe Text),
    configParameterPathPrefix :: !(Maybe Text),
    configParameterCookiePrefix :: !(Maybe Text),
    configParameterHeaderPrefix :: !(Maybe Text),
    configOperationsToGenerate :: !(Maybe [Text]),
    configOpaqueSchemas :: !(Maybe [Text]),
    configWhiteListedSchemas :: !(Maybe [Text]),
    configOutputAllSchemas :: !(Maybe Bool),
    configFixedValueStrategy :: !(Maybe FixedValueStrategy),
    configUseSingleFieldNames :: !(Maybe Bool)
  }
  deriving stock (Show, Eq)
  deriving (FromJSON, ToJSON) via (Autodocodec Configuration)

instance HasCodec Configuration where
  codec =
    object "Configuration" $
      Configuration
        <$> optionalField "specification" "The OpenAPI 3 specification file which code should be generated for." .= configOpenApiSpecification
        <*> optionalField "outputDir" "The directory where the generated output is stored." .= configOutputDir
        <*> optionalField "packageName" "Name of the stack project" .= configPackageName
        <*> optionalField "moduleName" "Name of the module" .= configModuleName
        <*> optionalField "logLevel" "Set the minium log level (e. g. WARN to only print warnings and errors). Possible values: TRACE, INFO, WARN, ERROR" .= configLogLevel
        <*> optionalField "force" "Overwrite output directory without question" .= configForce
        <*> optionalField "incremental" "Only write new/changed files. Does not need --force flag to overwrite files." .= configIncremental
        <*> optionalField "dryRun" "Do not generate the output files but only print the generated code" .= configDryRun
        <*> optionalField "doNotGenerateStackProject" "Do not generate a stack project alongside the raw Haskell files" .= configDoNotGenerateStackProject
        <*> optionalField "generateNixFiles" "Generate Nix files alongside the raw Haskell files" .= configGenerateNixFiles
        <*> optionalField "omitAdditionalOperationFunctions" "Omit the additional operation functions, which are: with explicit configuration and raw variants (returning the plain ByteString) for both with and without explicit configuration" .= configOmitAdditionalOperationFunctions
        <*> optionalField "generateOptionalEmptyRequestBody" "Force the generator to create types for empty request bodies which are optional (e. g. no properties and required equals false)" .= configGenerateOptionalEmptyRequestBody
        <*> optionalField "useNumberedVariantConstructors" "Use numbered data constructors (e. g. Variant1, Variant 2, etc.) for one-of types" .= configUseNumberedVariantConstructors
        <*> optionalField "useFloatWithArbitraryPrecision" "Use Data.Scientific instead of Double to support arbitary number precision" .= configUseFloatWithArbitraryPrecision
        <*> optionalField "useIntWithArbitraryPrecision" "Use 'Integer' instead of 'Int' to support arbitrary number precision" .= configUseIntWithArbitraryPrecision
        <*> optionalField "useDateTypesAsString" "Convert strings formatted as date / date-time to date types" .= configUseDateTypesAsString
        <*> optionalField "convertToCamelCase" "Convert names to CamelCase instead of using names which are as close as possible to the names provided in the specification" .= configConvertToCamelCase
        <*> optionalField "propertyTypeSuffix" "Add a suffix to property types to prevent naming conflicts" .= configPropertyTypeSuffix
        <*> optionalField "responseTypeSuffix" "The suffix which is added to the response data types" .= configResponseTypeSuffix
        <*> optionalField "responseBodyTypeSuffix" "The suffix which is added to the response body data types" .= configResponseBodyTypeSuffix
        <*> optionalField "requestBodyTypeSuffix" "The suffix which is added to the request body data types" .= configRequestBodyTypeSuffix
        <*> optionalField "arrayItemTypeSuffix" "The suffix which is added to the item type of an array. This is only applied to item types of top level array types which an alias is generated for." .= configArrayItemTypeSuffix
        <*> optionalField "nonNullableTypeSuffix" "The suffix which is added to the non-nullable part of a nullable type. This is only applied to top level nullable schemas as they are the only ones which need to be referencable by name." .= configNonNullableTypeSuffix
        <*> optionalField "parametersTypeSuffix" "The suffix which is added to the parameters type of operations" .= configParametersTypeSuffix
        <*> optionalField "parameterQueryPrefix" "The prefix which is added to query parameters" .= configParameterQueryPrefix
        <*> optionalField "parameterPathPrefix" "The prefix which is added to path parameters" .= configParameterPathPrefix
        <*> optionalField "parameterCookiePrefix" "The prefix which is added to cookie parameters" .= configParameterCookiePrefix
        <*> optionalField "parameterHeaderPrefix" "The prefix which is added to header parameters" .= configParameterHeaderPrefix
        <*> optionalField "operationsToGenerate" "If not all operations should be generated, this option can be used to specify all of them which should be generated. The value has to correspond to the value in the 'operationId' field in the OpenAPI 3 specification." .= configOperationsToGenerate
        <*> optionalField "opaqueSchemas" "A list of schema names (exactly as they are named in the components.schemas section of the corresponding OpenAPI 3 specification) which are not further investigated while generating code from the specification. Only a type alias to 'Aeson.Value' is created for these schemas." .= configOpaqueSchemas
        <*> optionalField "whiteListedSchemas" "A list of schema names (exactly as they are named in the components.schemas section of the corresponding OpenAPI 3 specification) which need to be generated. For all other schemas only a type alias to 'Aeson.Value' is created." .= configWhiteListedSchemas
        <*> optionalField "outputAllSchemas" "Output all component schemas" .= configOutputAllSchemas
        <*> optionalField "fixedValueStrategy" "In OpenAPI 3, fixed values can be defined as an enum with only one allowed value. If such a constant value is encountered as a required property of an object, the generator excludes this property by default ('exclude' strategy) and adds the value in the 'ToJSON' instance and expects the value to be there in the 'FromJSON' instance. This setting allows to change this behavior by including all fixed value fields instead ('include' strategy), i.e. just not trying to do anything smart." .= configFixedValueStrategy
        <*> optionalField "useSingleFieldNames" "Instead of numbering oneof branches as OneOfN, name oneof branches after a single field where possible" .= configUseSingleFieldNames

getConfiguration :: Text -> IO (Maybe Configuration)
getConfiguration path = resolveFile' (T.unpack path) >>= readYamlConfigFile
