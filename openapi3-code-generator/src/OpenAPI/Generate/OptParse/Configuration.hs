{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module OpenAPI.Generate.OptParse.Configuration
  ( Configuration (..),
    getConfiguration,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml
import qualified OpenAPI.Generate.Log as OAL
import OpenAPI.Generate.OptParse.Flags
import Path
import Path.IO
import YamlParse.Applicative as YamlParse

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
    configParametersTypeSuffix :: !(Maybe Text),
    configParameterQueryPrefix :: !(Maybe Text),
    configParameterPathPrefix :: !(Maybe Text),
    configParameterCookiePrefix :: !(Maybe Text),
    configParameterHeaderPrefix :: !(Maybe Text),
    configOperationsToGenerate :: !(Maybe [Text]),
    configOpaqueSchemas :: !(Maybe [Text]),
    configWhiteListedSchemas :: !(Maybe [Text])
  }
  deriving (Show, Eq)

instance FromJSON Configuration where
  parseJSON = viaYamlSchema

instance YamlSchema Configuration where
  yamlSchema =
    objectParser "Configuration" $
      Configuration
        <$> optionalField "specification" "The OpenAPI 3 specification file which code should be generated for."
        <*> optionalField "outputDir" "The directory where the generated output is stored."
        <*> optionalField "packageName" "Name of the stack project"
        <*> optionalField "moduleName" "Name of the module"
        <*> optionalField "logLevel" "Set the minium log level (e. g. WARN to only print warnings and errors). Possible values: TRACE, INFO, WARN, ERROR"
        <*> optionalField "force" "Overwrite output directory without question"
        <*> optionalField "incremental" "Only write new/changed files. Does not need --force flag to overwrite files."
        <*> optionalField "dryRun" "Do not generate the output files but only print the generated code"
        <*> optionalField "doNotGenerateStackProject" "Do not generate a stack project alongside the raw Haskell files"
        <*> optionalField "generateNixFiles" "Generate Nix files alongside the raw Haskell files"
        <*> optionalField "omitAdditionalOperationFunctions" "Omit the additional operation functions, which are: with explicit configuration and raw variants (returning the plain ByteString) for both with and without explicit configuration"
        <*> optionalField "generateOptionalEmptyRequestBody" "Force the generator to create types for empty request bodies which are optional (e. g. no properties and required equals false)"
        <*> optionalField "useNumberedVariantConstructors" "Use numbered data constructors (e. g. Variant1, Variant 2, etc.) for one-of types"
        <*> optionalField "useFloatWithArbitraryPrecision" "Use Data.Scientific instead of Double to support arbitary number precision"
        <*> optionalField "useIntWithArbitraryPrecision" "Use 'Integer' instead of 'Int' to support arbitrary number precision"
        <*> optionalField "useDateTypesAsString" "Convert strings formatted as date / date-time to date types"
        <*> optionalField "convertToCamelCase" "Convert names to CamelCase instead of using names which are as close as possible to the names provided in the specification"
        <*> optionalField "propertyTypeSuffix" "Add a suffix to property types to prevent naming conflicts"
        <*> optionalField "responseTypeSuffix" "The suffix which is added to the response data types"
        <*> optionalField "responseBodyTypeSuffix" "The suffix which is added to the response body data types"
        <*> optionalField "requestBodyTypeSuffix" "The suffix which is added to the request body data types"
        <*> optionalField "parametersTypeSuffix" "The suffix which is added to the parameters type of operations"
        <*> optionalField "parameterQueryPrefix" "The prefix which is added to query parameters"
        <*> optionalField "parameterPathPrefix" "The prefix which is added to path parameters"
        <*> optionalField "parameterCookiePrefix" "The prefix which is added to cookie parameters"
        <*> optionalField "parameterHeaderPrefix" "The prefix which is added to header parameters"
        <*> optionalField "operationsToGenerate" "If not all operations should be generated, this option can be used to specify all of them which should be generated. The value has to correspond to the value in the 'operationId' field in the OpenAPI 3 specification."
        <*> optionalField "opaqueSchemas" "A list of schema names (exactly as they are named in the components.schemas section of the corresponding OpenAPI 3 specification) which are not further investigated while generating code from the specification. Only a type alias to 'Aeson.Value' is created for these schemas."
        <*> optionalField "whiteListedSchemas" "A list of schema names (exactly as they are named in the components.schemas section of the corresponding OpenAPI 3 specification) which need to be generated. For all other schemas only a type alias to 'Aeson.Value' is created."

getConfiguration :: Flags -> IO (Maybe Configuration)
getConfiguration Flags {..} =
  case flagConfiguration of
    Nothing -> parseRelFile "openapi-configuration.yml" >>= YamlParse.readConfigFile
    Just cf -> do
      afp <- resolveFile' $ T.unpack cf
      YamlParse.readConfigFile afp
