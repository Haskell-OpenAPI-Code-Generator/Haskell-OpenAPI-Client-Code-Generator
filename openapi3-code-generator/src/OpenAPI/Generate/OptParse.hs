{-# LANGUAGE OverloadedStrings #-}

-- | This module defines the available command line flags and their default values.
module OpenAPI.Generate.OptParse
  ( Options (..),
    Flags (..),
    parseOptions,
    parseFlags,
  )
where

import Data.Text (Text)
import qualified OpenAPI.Generate.Log as OAL
import Options.Applicative

data Options = Options
  { optSpecification :: Text,
    optFlags :: Flags
  }
  deriving (Show, Eq)

parseOptions :: Parser Options
parseOptions =
  Options
    <$> strArgument
      ( mconcat
          [ metavar "FILENAME",
            help "The OpenAPI 3 specification file which code should be generated for."
          ]
      )
    <*> parseFlags

-- | The options passed to the generator via CLI
data Flags = Flags
  { -- | The directory where the generated output is stored.
    flagOutputDir :: Text,
    -- | The path to a configuration file which allows to configure the generation
    flagGeneratorConfigurationFile :: Maybe Text,
    -- | Name of the stack project
    flagPackageName :: Text,
    -- | Name of the module
    flagModuleName :: Text,
    -- | The minimum log level to output
    flagLogLevel :: OAL.LogSeverity,
    -- | Overwrite output directory without question
    flagForce :: Bool,
    -- | Only write new/changed files
    flagIncremental :: Bool,
    -- | Do not generate the output files but only print the generated code
    flagDryRun :: Bool,
    -- | Do not generate a stack project alongside the raw Haskell files
    flagDoNotGenerateStackProject :: Bool,
    -- | Omit the additional operation functions, which are: with explicit configuration and raw variants (returning the plain ByteString) for both with and without explicit configuration
    flagOmitAdditionalOperationFunctions :: Bool,
    -- | Force the generator to create types for empty request bodies which are optional (e. g. no properties and required equals false)
    flagGenerateOptionalEmptyRequestBody :: Bool,
    -- | Use numbered data constructors (e. g. Variant1, Variant 2, etc.) for one-of types
    flagUseNumberedVariantConstructors :: Bool,
    -- | Use Data.Scientific instead of Double to support arbitrary number precision
    flagUseFloatWithArbitraryPrecision :: Bool,
    -- | Use 'Integer' instead of 'Int' to support arbitrary number precision
    flagUseIntWithArbitraryPrecision :: Bool,
    -- | Convert strings formatted as date / date-time to date types
    flagUseDateTypesAsString :: Bool,
    -- | Convert names to CamelCase instead of using names which are as close as possible to the names provided in the specification
    flagConvertToCamelCase :: Bool,
    -- | Add a suffix to property types to prevent naming conflicts
    flagPropertyTypeSuffix :: Text,
    -- | The suffix which is added to the response data types
    flagResponseTypeSuffix :: Text,
    -- | The suffix which is added to the response body data types
    flagResponseBodyTypeSuffix :: Text,
    -- | The suffix which is added to the request body data types
    flagRequestBodyTypeSuffix :: Text,
    -- | The suffix which is added to the parameters type of operations
    flagParametersTypeSuffix :: Text,
    -- | The prefix which is added to query parameters
    flagParameterQueryPrefix :: Text,
    -- | The prefix which is added to path parameters
    flagParameterPathPrefix :: Text,
    -- | The prefix which is added to cookie parameters
    flagParameterCookiePrefix :: Text,
    -- | The prefix which is added to header parameters
    flagParameterHeaderPrefix :: Text,
    -- | The operations to generate (if empty all operations are generated)
    flagOperationsToGenerate :: [Text]
  }
  deriving (Show, Eq)

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> parseFlagOutputDir
    <*> parseFlagGeneratorConfigurationFile
    <*> parseFlagPackageName
    <*> parseFlagModuleName
    <*> parseFlagLogLevel
    <*> parseFlagForce
    <*> parseFlagIncremental
    <*> parseFlagDryRun
    <*> parseFlagDoNotGenerateStackProject
    <*> parseFlagOmitAdditionalOperationFunctions
    <*> parseFlagGenerateOptionalEmptyRequestBody
    <*> parseFlagUseNumberedVariantConstructors
    <*> parseFlagUseFloatWithArbitraryPrecision
    <*> parseFlagUseIntWithArbitraryPrecision
    <*> parseFlagUseDateTypesAsString
    <*> parseFlagConvertToCamelCase
    <*> parseFlagPropertyTypeSuffix
    <*> parseFlagResponseTypeSuffix
    <*> parseFlagResponseBodyTypeSuffix
    <*> parseFlagRequestBodyTypeSuffix
    <*> parseFlagParametersTypeSuffix
    <*> parseFlagParameterQueryPrefix
    <*> parseFlagParameterPathPrefix
    <*> parseFlagParameterCookiePrefix
    <*> parseFlagParameterHeaderPrefix
    <*> parseFlagOperationsToGenerate

parseFlagOutputDir :: Parser Text
parseFlagOutputDir =
  strOption $
    mconcat
      [ metavar "OUTDIR",
        help "The directory where the generated output is stored.",
        long "output-dir",
        short 'o',
        value "out",
        showDefault
      ]

parseFlagGeneratorConfigurationFile :: Parser (Maybe Text)
parseFlagGeneratorConfigurationFile =
  optional $
    strOption $
      mconcat
        [ metavar "FILEPATH",
          help "The path to a configuration file which allows to configure the generation. Should be a .yaml file. For an example see https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator/blob/master/example-generator-configuration.yaml.",
          long "generator-configuration"
        ]

parseFlagPackageName :: Parser Text
parseFlagPackageName =
  strOption $
    mconcat
      [ metavar "PACKAGE",
        help "Name of the stack project",
        long "package-name",
        value "openapi",
        showDefault
      ]

parseFlagModuleName :: Parser Text
parseFlagModuleName =
  strOption $
    mconcat
      [ metavar "MODULE",
        help "Name of the module",
        long "module-name",
        value "OpenAPI",
        showDefault
      ]

parseFlagLogLevel :: Parser OAL.LogSeverity
parseFlagLogLevel =
  option auto $
    mconcat
      [ metavar "LOGLEVEL",
        help "Set the minium log level (e. g. WARN to only print warnings and errors). Possible values: TRACE, INFO, WARN, ERROR",
        long "log-level",
        value OAL.InfoSeverity,
        showDefault,
        completeWith ["TRACE", "INFO", "WARN", "ERROR"]
      ]

parseFlagForce :: Parser Bool
parseFlagForce =
  switch $
    mconcat
      [ help "Overwrite output directory without question",
        long "force",
        short 'f'
      ]

parseFlagIncremental :: Parser Bool
parseFlagIncremental =
  switch $
    mconcat
      [ help "Only write new/changed files. Does not need --force flag to overwrite files.",
        long "incremental",
        short 'i'
      ]

parseFlagDryRun :: Parser Bool
parseFlagDryRun =
  switch $
    mconcat
      [ help "Do not generate the output files but only print the generated code",
        long "dry-run"
      ]

parseFlagDoNotGenerateStackProject :: Parser Bool
parseFlagDoNotGenerateStackProject =
  switch $
    mconcat
      [ help "Do not generate a stack project alongside the raw Haskell files",
        long "do-not-generate-stack-project"
      ]

parseFlagOmitAdditionalOperationFunctions :: Parser Bool
parseFlagOmitAdditionalOperationFunctions =
  switch $
    mconcat
      [ help "Omit the additional operation functions, which are: with explicit configuration and raw variants (returning the plain ByteString) for both with and without explicit configuration",
        long "omit-additional-operation-functions"
      ]

parseFlagGenerateOptionalEmptyRequestBody :: Parser Bool
parseFlagGenerateOptionalEmptyRequestBody =
  switch $
    mconcat
      [ help "Force the generator to create types for empty request bodies which are optional (e. g. no properties and required equals false)",
        long "generate-optional-empty-request-body"
      ]

parseFlagUseNumberedVariantConstructors :: Parser Bool
parseFlagUseNumberedVariantConstructors =
  switch $
    mconcat
      [ help "Use numbered data constructors (e. g. Variant1, Variant 2, etc.) for one-of types",
        long "use-numbered-variant-constructors"
      ]

parseFlagUseFloatWithArbitraryPrecision :: Parser Bool
parseFlagUseFloatWithArbitraryPrecision =
  switch $
    mconcat
      [ help "Use Data.Scientific instead of Double to support arbitary number precision",
        long "use-float-with-arbitrary-precision"
      ]

parseFlagUseIntWithArbitraryPrecision :: Parser Bool
parseFlagUseIntWithArbitraryPrecision =
  switch $
    mconcat
      [ help "Use 'Integer' instead of 'Int' to support arbitrary number precision",
        long "use-int-with-arbitrary-precision"
      ]

parseFlagUseDateTypesAsString :: Parser Bool
parseFlagUseDateTypesAsString =
  switch $
    mconcat
      [ help "Convert strings formatted as date / date-time to date types",
        long "use-date-types-as-string"
      ]

parseFlagConvertToCamelCase :: Parser Bool
parseFlagConvertToCamelCase =
  switch $
    mconcat
      [ help "Convert names to CamelCase instead of using names which are as close as possible to the names provided in the specification",
        long "convert-to-camel-case"
      ]

parseFlagPropertyTypeSuffix :: Parser Text
parseFlagPropertyTypeSuffix =
  strOption $
    mconcat
      [ metavar "SUFFIX",
        help "Add a suffix to property types to prevent naming conflicts",
        long "property-type-suffix",
        value ""
      ]

parseFlagResponseTypeSuffix :: Parser Text
parseFlagResponseTypeSuffix =
  strOption $
    mconcat
      [ metavar "SUFFIX",
        help "The suffix which is added to the response data types",
        long "response-type-suffix",
        value "Response",
        showDefault
      ]

parseFlagResponseBodyTypeSuffix :: Parser Text
parseFlagResponseBodyTypeSuffix =
  strOption $
    mconcat
      [ metavar "SUFFIX",
        help "The suffix which is added to the response body data types",
        long "response-body-type-suffix",
        value "ResponseBody",
        showDefault
      ]

parseFlagRequestBodyTypeSuffix :: Parser Text
parseFlagRequestBodyTypeSuffix =
  strOption $
    mconcat
      [ metavar "SUFFIX",
        help "The suffix which is added to the request body data types",
        long "request-body-type-suffix",
        value "RequestBody",
        showDefault
      ]

parseFlagParametersTypeSuffix :: Parser Text
parseFlagParametersTypeSuffix =
  strOption $
    mconcat
      [ metavar "SUFFIX",
        help "The suffix which is added to the parameters type of operations",
        long "parameters-type-suffix",
        value "Parameters",
        showDefault
      ]

parseFlagParameterQueryPrefix :: Parser Text
parseFlagParameterQueryPrefix =
  strOption $
    mconcat
      [ metavar "PREFIX",
        help "The prefix which is added to query parameters",
        long "parameter-query-prefix",
        value "query",
        showDefault
      ]

parseFlagParameterPathPrefix :: Parser Text
parseFlagParameterPathPrefix =
  strOption $
    mconcat
      [ metavar "PREFIX",
        help "The prefix which is added to path parameters",
        long "parameter-path-prefix",
        value "path",
        showDefault
      ]

parseFlagParameterCookiePrefix :: Parser Text
parseFlagParameterCookiePrefix =
  strOption $
    mconcat
      [ metavar "PREFIX",
        help "The prefix which is added to cookie parameters",
        long "parameter-cookie-prefix",
        value "cookie",
        showDefault
      ]

parseFlagParameterHeaderPrefix :: Parser Text
parseFlagParameterHeaderPrefix =
  strOption $
    mconcat
      [ metavar "PREFIX",
        help "The prefix which is added to header parameters",
        long "parameter-header-prefix",
        value "header",
        showDefault
      ]

parseFlagOperationsToGenerate :: Parser [Text]
parseFlagOperationsToGenerate =
  many $
    strOption $
      mconcat
        [ metavar "OPERATIONID",
          help "If not all operations should be generated, this option can be used to specify all of them which should be generated. The value has to correspond to the value in the 'operationId' field in the OpenAPI 3 specification.",
          long "operation-to-generate"
        ]
