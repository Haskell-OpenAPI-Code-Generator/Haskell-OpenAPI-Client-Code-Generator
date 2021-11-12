{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | This module defines the settings and their default values.
module OpenAPI.Generate.OptParse
  ( Settings (..),
    getSettings,
  )
where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified OpenAPI.Generate.Log as OAL
import OpenAPI.Generate.OptParse.Configuration
import OpenAPI.Generate.OptParse.Flags
import Options.Applicative
import Options.Applicative.Help (string)
import Path.IO
import Path.Posix
import System.Exit
import YamlParse.Applicative as YamlParse

getSettings :: IO Settings
getSettings = do
  flags <- getFlags
  let configurationFilePath = fromMaybe "openapi-configuration.yml" $ flagConfiguration flags
  config <- getConfiguration configurationFilePath
  unless (isJust config || isNothing (flagConfiguration flags)) $ die $ "Could not read configuration file: " <> T.unpack configurationFilePath
  combineToSettings flags config configurationFilePath

data Settings = Settings
  { -- | The OpenAPI 3 specification file which code should be generated for.
    settingOpenApiSpecification :: !Text,
    -- | The directory where the generated output is stored.
    settingOutputDir :: !Text,
    -- | Name of the stack project
    settingPackageName :: !Text,
    -- | Name of the module
    settingModuleName :: !Text,
    -- | The minimum log level to output
    settingLogLevel :: !OAL.LogSeverity,
    -- | Overwrite output directory without question
    settingForce :: !Bool,
    -- | Only write new/changed files
    settingIncremental :: !Bool,
    -- | Do not generate the output files but only print the generated code
    settingDryRun :: !Bool,
    -- | Do not generate a stack project alongside the raw Haskell files
    settingDoNotGenerateStackProject :: !Bool,
    -- | Generate Nix files (default.nix and shell.nix)
    settingGenerateNixFiles :: !Bool,
    -- | Omit the additional operation functions, which are: with explicit configuration and raw variants (returning the plain ByteString) for both with and without explicit configuration
    settingOmitAdditionalOperationFunctions :: !Bool,
    -- | Force the generator to create types for empty request bodies which are optional (e. g. no properties and required equals false)
    settingGenerateOptionalEmptyRequestBody :: !Bool,
    -- | Use numbered data constructors (e. g. Variant1, Variant 2, etc.) for one-of types
    settingUseNumberedVariantConstructors :: !Bool,
    -- | Use Data.Scientific instead of Double to support arbitrary number precision
    settingUseFloatWithArbitraryPrecision :: !Bool,
    -- | Use 'Integer' instead of 'Int' to support arbitrary number precision
    settingUseIntWithArbitraryPrecision :: !Bool,
    -- | Convert strings formatted as date / date-time to date types
    settingUseDateTypesAsString :: !Bool,
    -- | Convert names to CamelCase instead of using names which are as close as possible to the names provided in the specification
    settingConvertToCamelCase :: !Bool,
    -- | Add a suffix to property types to prevent naming conflicts
    settingPropertyTypeSuffix :: !Text,
    -- | The suffix which is added to the response data types
    settingResponseTypeSuffix :: !Text,
    -- | The suffix which is added to the response body data types
    settingResponseBodyTypeSuffix :: !Text,
    -- | The suffix which is added to the request body data types
    settingRequestBodyTypeSuffix :: !Text,
    -- | The suffix which is added to the item type of an array. This is only applied to item types of top level array types which an alias is generated for.
    settingArrayItemTypeSuffix :: !Text,
    -- | The suffix which is added to the parameters type of operations
    settingParametersTypeSuffix :: !Text,
    -- | The prefix which is added to query parameters
    settingParameterQueryPrefix :: !Text,
    -- | The prefix which is added to path parameters
    settingParameterPathPrefix :: !Text,
    -- | The prefix which is added to cookie parameters
    settingParameterCookiePrefix :: !Text,
    -- | The prefix which is added to header parameters
    settingParameterHeaderPrefix :: !Text,
    -- | The operations to generate (if empty all operations are generated)
    settingOperationsToGenerate :: ![Text],
    -- | A list of schema names (exactly as they are named in the components.schemas section of the corresponding OpenAPI 3 specification)
    -- which are not further investigated while generating code from the specification.
    -- Only a type alias to 'Aeson.Value' is created for these schemas.
    settingOpaqueSchemas :: ![Text],
    -- | A list of schema names (exactly as they are named in the components.schemas section of the corresponding OpenAPI 3 specification)
    -- which need to be generated.
    -- For all other schemas only a type alias to 'Aeson.Value' is created.
    settingWhiteListedSchemas :: ![Text]
  }
  deriving (Show, Eq)

combineToSettings :: Flags -> Maybe Configuration -> Text -> IO Settings
combineToSettings Flags {..} mConf configurationFilePath = do
  let resolveRelativeToConfiguration = \case
        Just filePath -> do
          configurationDirectory <- resolveFile' $ T.unpack configurationFilePath
          file <- resolveFile (parent configurationDirectory) $ T.unpack filePath
          pure $ Just $ T.pack $ toFilePath file
        _ -> pure Nothing
  mConfigOpenApiSpecification <- resolveRelativeToConfiguration $ mc configOpenApiSpecification
  mConfigOutputDir <- resolveRelativeToConfiguration $ mc configOutputDir
  let settingOpenApiSpecification = fromMaybe "openapi-specification.yml" $ flagOpenApiSpecification <|> mConfigOpenApiSpecification
      settingOutputDir = fromMaybe "out" $ flagOutputDir <|> mConfigOutputDir
      settingPackageName = fromMaybe "openapi" $ flagPackageName <|> mc configPackageName
      settingModuleName = fromMaybe "OpenAPI" $ flagModuleName <|> mc configModuleName
      settingLogLevel = fromMaybe OAL.InfoSeverity $ flagLogLevel <|> mc configLogLevel
      settingForce = fromMaybe False $ flagForce <|> mc configForce
      settingIncremental = fromMaybe False $ flagIncremental <|> mc configIncremental
      settingDryRun = fromMaybe False $ flagDryRun <|> mc configDryRun
      settingDoNotGenerateStackProject = fromMaybe False $ flagDoNotGenerateStackProject <|> mc configDoNotGenerateStackProject
      settingGenerateNixFiles = fromMaybe False $ flagGenerateNixFiles <|> mc configGenerateNixFiles
      settingOmitAdditionalOperationFunctions = fromMaybe False $ flagOmitAdditionalOperationFunctions <|> mc configOmitAdditionalOperationFunctions
      settingGenerateOptionalEmptyRequestBody = fromMaybe False $ flagGenerateOptionalEmptyRequestBody <|> mc configGenerateOptionalEmptyRequestBody
      settingUseNumberedVariantConstructors = fromMaybe False $ flagUseNumberedVariantConstructors <|> mc configUseNumberedVariantConstructors
      settingUseFloatWithArbitraryPrecision = fromMaybe False $ flagUseFloatWithArbitraryPrecision <|> mc configUseFloatWithArbitraryPrecision
      settingUseIntWithArbitraryPrecision = fromMaybe False $ flagUseIntWithArbitraryPrecision <|> mc configUseIntWithArbitraryPrecision
      settingUseDateTypesAsString = fromMaybe False $ flagUseDateTypesAsString <|> mc configUseDateTypesAsString
      settingConvertToCamelCase = fromMaybe False $ flagConvertToCamelCase <|> mc configConvertToCamelCase
      settingPropertyTypeSuffix = fromMaybe "" $ flagPropertyTypeSuffix <|> mc configPropertyTypeSuffix
      settingResponseTypeSuffix = fromMaybe "Response" $ flagResponseTypeSuffix <|> mc configResponseTypeSuffix
      settingResponseBodyTypeSuffix = fromMaybe "ResponseBody" $ flagResponseBodyTypeSuffix <|> mc configResponseBodyTypeSuffix
      settingRequestBodyTypeSuffix = fromMaybe "RequestBody" $ flagRequestBodyTypeSuffix <|> mc configRequestBodyTypeSuffix
      settingArrayItemTypeSuffix = fromMaybe "Item" $ flagArrayItemTypeSuffix <|> mc configArrayItemTypeSuffix
      settingParametersTypeSuffix = fromMaybe "Parameters" $ flagParametersTypeSuffix <|> mc configParametersTypeSuffix
      settingParameterQueryPrefix = fromMaybe "query" $ flagParameterQueryPrefix <|> mc configParameterQueryPrefix
      settingParameterPathPrefix = fromMaybe "path" $ flagParameterPathPrefix <|> mc configParameterPathPrefix
      settingParameterCookiePrefix = fromMaybe "cookie" $ flagParameterCookiePrefix <|> mc configParameterCookiePrefix
      settingParameterHeaderPrefix = fromMaybe "header" $ flagParameterHeaderPrefix <|> mc configParameterHeaderPrefix
      settingOperationsToGenerate = fromMaybe [] $ flagOperationsToGenerate <|> mc configOperationsToGenerate
      settingOpaqueSchemas = fromMaybe [] $ flagOpaqueSchemas <|> mc configOpaqueSchemas
      settingWhiteListedSchemas = fromMaybe [] $ flagWhiteListedSchemas <|> mc configWhiteListedSchemas

  pure Settings {..}
  where
    mc :: (Configuration -> Maybe a) -> Maybe a
    mc f = mConf >>= f

getFlags :: IO Flags
getFlags = customExecParser prefs_ flagsParser

prefs_ :: ParserPrefs
prefs_ =
  defaultPrefs
    { prefShowHelpOnError = True,
      prefShowHelpOnEmpty = True
    }

flagsParser :: ParserInfo Flags
flagsParser =
  info
    (helper <*> parseFlags)
    ( fullDesc <> footerDoc (Just $ string footerStr)
        <> progDesc "This tool can be used to generate Haskell code from OpenAPI 3 specifications. For more information see https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator."
        <> header "Generate Haskell code from OpenAPI 3 specifications"
    )
  where
    footerStr =
      unlines
        [ "Configuration file format:",
          "",
          T.unpack (YamlParse.prettyColourisedSchemaDoc @Configuration)
        ]
