{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenAPI.Generate.OptParse.Flags
  ( Flags (..),
    parseFlags,
  )
where

import Data.Text (Text)
import qualified OpenAPI.Generate.Log as OAL
import OpenAPI.Generate.OptParse.Types
import Options.Applicative

data Flags = Flags
  { flagConfiguration :: !(Maybe Text),
    flagOpenApiSpecification :: !(Maybe Text),
    flagOutputDir :: !(Maybe Text),
    flagPackageName :: !(Maybe Text),
    flagModuleName :: !(Maybe Text),
    flagLogLevel :: !(Maybe OAL.LogSeverity),
    flagForce :: !(Maybe Bool),
    flagIncremental :: !(Maybe Bool),
    flagDryRun :: !(Maybe Bool),
    flagDoNotGenerateStackProject :: !(Maybe Bool),
    flagGenerateNixFiles :: !(Maybe Bool),
    flagOmitAdditionalOperationFunctions :: !(Maybe Bool),
    flagGenerateOptionalEmptyRequestBody :: !(Maybe Bool),
    flagUseNumberedVariantConstructors :: !(Maybe Bool),
    flagUseFloatWithArbitraryPrecision :: !(Maybe Bool),
    flagUseIntWithArbitraryPrecision :: !(Maybe Bool),
    flagUseDateTypesAsString :: !(Maybe Bool),
    flagConvertToCamelCase :: !(Maybe Bool),
    flagPropertyTypeSuffix :: !(Maybe Text),
    flagResponseTypeSuffix :: !(Maybe Text),
    flagResponseBodyTypeSuffix :: !(Maybe Text),
    flagRequestBodyTypeSuffix :: !(Maybe Text),
    flagArrayItemTypeSuffix :: !(Maybe Text),
    flagNonNullableTypeSuffix :: !(Maybe Text),
    flagParametersTypeSuffix :: !(Maybe Text),
    flagParameterQueryPrefix :: !(Maybe Text),
    flagParameterPathPrefix :: !(Maybe Text),
    flagParameterCookiePrefix :: !(Maybe Text),
    flagParameterHeaderPrefix :: !(Maybe Text),
    flagOperationsToGenerate :: !(Maybe [Text]),
    flagOpaqueSchemas :: !(Maybe [Text]),
    flagWhiteListedSchemas :: !(Maybe [Text]),
    flagFixedValueStrategy :: !(Maybe FixedValueStrategy)
  }
  deriving (Show, Eq)

parseFlags :: Parser Flags
parseFlags =
  Flags
    <$> parseFlagConfiguration
    <*> parseFlagOpenApiSpecification
    <*> parseFlagOutputDir
    <*> parseFlagPackageName
    <*> parseFlagModuleName
    <*> parseFlagLogLevel
    <*> parseFlagForce
    <*> parseFlagIncremental
    <*> parseFlagDryRun
    <*> parseFlagDoNotGenerateStackProject
    <*> parseFlagGenerateNixFiles
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
    <*> parseFlagArrayItemTypeSuffix
    <*> parseFlagNonNullableTypeSuffix
    <*> parseFlagParametersTypeSuffix
    <*> parseFlagParameterQueryPrefix
    <*> parseFlagParameterPathPrefix
    <*> parseFlagParameterCookiePrefix
    <*> parseFlagParameterHeaderPrefix
    <*> parseFlagOperationsToGenerate
    <*> parseFlagOpaqueSchemas
    <*> parseFlagWhiteListedSchemas
    <*> parseFlagFixedValueStrategy

parseFlagConfiguration :: Parser (Maybe Text)
parseFlagConfiguration =
  optional $
    strOption $
      mconcat
        [ metavar "FILEPATH",
          help "The path to a configuration file which allows to configure the generation. Should be a YAML file (default: 'openapi-configuration.yml'). For an example see https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator/blob/master/openapi-configuration.yml.",
          long "configuration"
        ]

parseFlagOpenApiSpecification :: Parser (Maybe Text)
parseFlagOpenApiSpecification =
  let helpText = "The OpenAPI 3 specification file which code should be generated for (default: 'openapi-specification.yml')."
      varName = "FILEPATH"
   in optional $
        strArgument
          ( mconcat
              [ metavar varName,
                help helpText
              ]
          )
          <|> strOption
            ( mconcat
                [ metavar varName,
                  help helpText,
                  long "specification"
                ]
            )

parseFlagOutputDir :: Parser (Maybe Text)
parseFlagOutputDir =
  optional $
    strOption $
      mconcat
        [ metavar "OUTDIR",
          help "The directory where the generated output is stored (default: 'out').",
          long "output-dir",
          short 'o'
        ]

parseFlagPackageName :: Parser (Maybe Text)
parseFlagPackageName =
  optional $
    strOption $
      mconcat
        [ metavar "PACKAGE",
          help "Name of the stack project (default: 'openapi')",
          long "package-name"
        ]

parseFlagModuleName :: Parser (Maybe Text)
parseFlagModuleName =
  optional $
    strOption $
      mconcat
        [ metavar "MODULE",
          help "Name of the module (default: 'OpenAPI')",
          long "module-name"
        ]

parseFlagLogLevel :: Parser (Maybe OAL.LogSeverity)
parseFlagLogLevel =
  optional $
    option auto $
      mconcat
        [ metavar "LOGLEVEL",
          help "Set the minium log level (e. g. WARN to only print warnings and errors). Possible values: TRACE, INFO, WARN, ERROR (default: 'INFO')",
          long "log-level",
          completeWith ["TRACE", "INFO", "WARN", "ERROR"]
        ]

boolReader :: ReadM Bool
boolReader =
  maybeReader
    ( \case
        "true" -> Just True
        "True" -> Just True
        "false" -> Just False
        "False" -> Just False
        _ -> Nothing
    )

booleanFlag :: String -> String -> Maybe Char -> Parser (Maybe Bool)
booleanFlag helpText longName shortName =
  optional $
    flag'
      True
      ( mconcat
          [ help $ helpText <> " Use without argument (e. g. --" <> longName <> " to set the flag to true or use it with --" <> longName <> "=false to set it to false (or true). The equal sign is required for the parser.",
            long longName,
            maybe mempty short shortName
          ]
      )
      <|> option
        boolReader
        ( mconcat
            [ long longName,
              maybe mempty short shortName,
              internal
            ]
        )

parseFlagForce :: Parser (Maybe Bool)
parseFlagForce = booleanFlag "Overwrite output directory without question." "force" (Just 'f')

parseFlagIncremental :: Parser (Maybe Bool)
parseFlagIncremental = booleanFlag "Only write new/changed files. Does not need --force flag to overwrite files." "incremental" (Just 'i')

parseFlagDryRun :: Parser (Maybe Bool)
parseFlagDryRun = booleanFlag "Do not generate the output files but only print the generated code." "dry-run" Nothing

parseFlagDoNotGenerateStackProject :: Parser (Maybe Bool)
parseFlagDoNotGenerateStackProject = booleanFlag "Do not generate a stack project alongside the raw Haskell files." "do-not-generate-stack-project" Nothing

parseFlagGenerateNixFiles :: Parser (Maybe Bool)
parseFlagGenerateNixFiles = booleanFlag "Generate Nix files alongside the raw Haskell files." "generate-nix-files" Nothing

parseFlagOmitAdditionalOperationFunctions :: Parser (Maybe Bool)
parseFlagOmitAdditionalOperationFunctions = booleanFlag "Omit the additional operation functions, which are: with explicit configuration and raw variants (returning the plain ByteString) for both with and without explicit configuration." "omit-additional-operation-functions" Nothing

parseFlagGenerateOptionalEmptyRequestBody :: Parser (Maybe Bool)
parseFlagGenerateOptionalEmptyRequestBody = booleanFlag "Force the generator to create types for empty request bodies which are optional (e. g. no properties and required equals false)." "generate-optional-empty-request-body" Nothing

parseFlagUseNumberedVariantConstructors :: Parser (Maybe Bool)
parseFlagUseNumberedVariantConstructors = booleanFlag "Use numbered data constructors (e. g. Variant1, Variant 2, etc.) for one-of types." "use-numbered-variant-constructors" Nothing

parseFlagUseFloatWithArbitraryPrecision :: Parser (Maybe Bool)
parseFlagUseFloatWithArbitraryPrecision = booleanFlag "Use Data.Scientific instead of Double to support arbitary number precision." "use-float-with-arbitrary-precision" Nothing

parseFlagUseIntWithArbitraryPrecision :: Parser (Maybe Bool)
parseFlagUseIntWithArbitraryPrecision = booleanFlag "Use 'Integer' instead of 'Int' to support arbitrary number precision." "use-int-with-arbitrary-precision" Nothing

parseFlagUseDateTypesAsString :: Parser (Maybe Bool)
parseFlagUseDateTypesAsString = booleanFlag "Convert strings formatted as date / date-time to date types." "use-date-types-as-string" Nothing

parseFlagConvertToCamelCase :: Parser (Maybe Bool)
parseFlagConvertToCamelCase = booleanFlag "Convert names to CamelCase instead of using names which are as close as possible to the names provided in the specification." "convert-to-camel-case" Nothing

parseFlagPropertyTypeSuffix :: Parser (Maybe Text)
parseFlagPropertyTypeSuffix =
  optional $
    strOption $
      mconcat
        [ metavar "SUFFIX",
          help "Add a suffix to property types to prevent naming conflicts (default: '')",
          long "property-type-suffix"
        ]

parseFlagResponseTypeSuffix :: Parser (Maybe Text)
parseFlagResponseTypeSuffix =
  optional $
    strOption $
      mconcat
        [ metavar "SUFFIX",
          help "The suffix which is added to the response data types (default: 'Response')",
          long "response-type-suffix"
        ]

parseFlagResponseBodyTypeSuffix :: Parser (Maybe Text)
parseFlagResponseBodyTypeSuffix =
  optional $
    strOption $
      mconcat
        [ metavar "SUFFIX",
          help "The suffix which is added to the response body data types (default: 'ResponseBody')",
          long "response-body-type-suffix"
        ]

parseFlagRequestBodyTypeSuffix :: Parser (Maybe Text)
parseFlagRequestBodyTypeSuffix =
  optional $
    strOption $
      mconcat
        [ metavar "SUFFIX",
          help "The suffix which is added to the request body data types (default: 'RequestBody')",
          long "request-body-type-suffix"
        ]

parseFlagArrayItemTypeSuffix :: Parser (Maybe Text)
parseFlagArrayItemTypeSuffix =
  optional $
    strOption $
      mconcat
        [ metavar "SUFFIX",
          help "The suffix which is added to the item type of an array (default: 'Item'). This is only applied to item types of top level array types which an alias is generated for.",
          long "array-item-type-suffix"
        ]

parseFlagNonNullableTypeSuffix :: Parser (Maybe Text)
parseFlagNonNullableTypeSuffix =
  optional $
    strOption $
      mconcat
        [ metavar "SUFFIX",
          help "The suffix which is added to the non-nullable part of a nullable type (default: 'NonNullable'). This is only applied to top level nullable schemas as they are the only ones which need to be referencable by name.",
          long "non-nullable-type-suffix"
        ]

parseFlagParametersTypeSuffix :: Parser (Maybe Text)
parseFlagParametersTypeSuffix =
  optional $
    strOption $
      mconcat
        [ metavar "SUFFIX",
          help "The suffix which is added to the parameters type of operations (default: 'Parameters')",
          long "parameters-type-suffix"
        ]

parseFlagParameterQueryPrefix :: Parser (Maybe Text)
parseFlagParameterQueryPrefix =
  optional $
    strOption $
      mconcat
        [ metavar "PREFIX",
          help "The prefix which is added to query parameters (default: 'query')",
          long "parameter-query-prefix"
        ]

parseFlagParameterPathPrefix :: Parser (Maybe Text)
parseFlagParameterPathPrefix =
  optional $
    strOption $
      mconcat
        [ metavar "PREFIX",
          help "The prefix which is added to path parameters (default: 'path')",
          long "parameter-path-prefix"
        ]

parseFlagParameterCookiePrefix :: Parser (Maybe Text)
parseFlagParameterCookiePrefix =
  optional $
    strOption $
      mconcat
        [ metavar "PREFIX",
          help "The prefix which is added to cookie parameters (default: 'cookie')",
          long "parameter-cookie-prefix"
        ]

parseFlagParameterHeaderPrefix :: Parser (Maybe Text)
parseFlagParameterHeaderPrefix =
  optional $
    strOption $
      mconcat
        [ metavar "PREFIX",
          help "The prefix which is added to header parameters (default: 'header')",
          long "parameter-header-prefix"
        ]

parseFlagOperationsToGenerate :: Parser (Maybe [Text])
parseFlagOperationsToGenerate =
  optional $
    some $
      strOption $
        mconcat
          [ metavar "OPERATIONID",
            help "If not all operations should be generated, this option can be used to specify all of them which should be generated. The value has to correspond to the value in the 'operationId' field in the OpenAPI 3 specification.",
            long "operation-to-generate"
          ]

parseFlagOpaqueSchemas :: Parser (Maybe [Text])
parseFlagOpaqueSchemas =
  optional $
    some $
      strOption $
        mconcat
          [ metavar "SCHEMA",
            help "A list of schema names (exactly as they are named in the components.schemas section of the corresponding OpenAPI 3 specification) which are not further investigated while generating code from the specification. Only a type alias to 'Aeson.Value' is created for these schemas.",
            long "opaque-schema"
          ]

parseFlagWhiteListedSchemas :: Parser (Maybe [Text])
parseFlagWhiteListedSchemas =
  optional $
    some $
      strOption $
        mconcat
          [ metavar "SCHEMA",
            help "A list of schema names (exactly as they are named in the components.schemas section of the corresponding OpenAPI 3 specification) which need to be generated. For all other schemas only a type alias to 'Aeson.Value' is created.",
            long "white-listed-schema"
          ]

parseFlagFixedValueStrategy :: Parser (Maybe FixedValueStrategy)
parseFlagFixedValueStrategy =
  optional $
    option auto $
      mconcat
        [ metavar "STRATEGY",
          help "In OpenAPI 3, fixed values can be defined as an enum with only one allowed value. If such a constant value is encountered as a required property of an object, the generator excludes this property by default ('exclude' strategy) and adds the value in the 'ToJSON' instance and expects the value to be there in the 'FromJSON' instance. This setting allows to change this behavior by including all fixed value fields instead ('include' strategy), i.e. just not trying to do anything smart (default: 'exclude').",
          long "fixed-value-strategy"
        ]
