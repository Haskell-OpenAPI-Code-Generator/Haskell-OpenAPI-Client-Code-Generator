-- | This module defines the available command line flags and their default values.
module OpenAPI.Generate.Flags where

import Options

-- | The options passed to the generator via CLI
data Flags
  = Flags
      { -- | The directory where the generated output is stored.
        optOutputDir :: String,
        -- | Generate a stack project alongside the raw Haskell files
        optGenerateStackProject :: Bool,
        -- | Do not generate the output files but only print the generated code
        optDryRun :: Bool,
        -- | Overwrite output directory without question
        optForce :: Bool,
        -- | Name of the stack project
        optPackageName :: String,
        -- | Name of the module
        optModuleName :: String,
        -- | Use Data.Scientific instead of Double to support arbitrary number precision
        optUseFloatWithArbitraryPrecision :: Bool,
        -- | Use 'Integer' instead of 'Int' to support arbitrary number precision
        optUseIntWithArbitraryPrecision :: Bool,
        -- | Convert strings formatted as date / date-time to date types
        optUseDateTypesAsString :: Bool,
        -- | Convert names to CamelCase instead of using names which are as close as possible to the names provided in the specification
        optConvertToCamelCase :: Bool,
        -- | Add a suffix to property types to prevent naming conflicts
        optPropertyTypeSuffix :: String,
        -- | The suffix which is added to the response data types
        optResponseTypeSuffix :: String,
        -- | The suffix which is added to the response body data types
        optResponseBodyTypeSuffix :: String,
        -- | The suffix which is added to the request body data types
        optRequestBodyTypeSuffix :: String,
        -- | The suffix which is added to the parameters type of operations
        optParametersTypeSuffix :: String,
        -- | The prefix which is added to query parameters
        optParameterQueryPrefix :: String,
        -- | The prefix which is added to path parameters
        optParameterPathPrefix :: String,
        -- | The prefix which is added to cookie parameters
        optParameterCookiePrefix :: String,
        -- | The prefix which is added to header parameters
        optParameterHeaderPrefix :: String,
        -- | Use numbered data constructors (e. g. Variant1, Variant 2, etc.) for one-of types
        optUseNumberedVariantConstructors :: Bool
      }
  deriving (Show, Eq)

instance Options Flags where
  defineOptions =
    Flags
      <$> simpleOption
        "output-dir"
        (optOutputDir defaultFlags)
        "The directory where the generated output is stored."
      <*> simpleOption
        "generate-stack-project"
        (optGenerateStackProject defaultFlags)
        "Generate a stack project alongside the raw Haskell files"
      <*> simpleOption
        "dry-run"
        (optDryRun defaultFlags)
        "Do not generate the output files but only print the generated code"
      <*> simpleOption
        "force"
        (optForce defaultFlags)
        "Overwrite output directory without question"
      <*> simpleOption
        "package-name"
        (optPackageName defaultFlags)
        "Name of the stack project"
      <*> simpleOption
        "module-name"
        (optModuleName defaultFlags)
        "Name of the module"
      <*> simpleOption
        "use-float-with-arbitrary-precision"
        (optUseFloatWithArbitraryPrecision defaultFlags)
        "Use Data.Scientific instead of Double to support arbitary number precision"
      <*> simpleOption
        "use-int-with-arbitrary-precision"
        (optUseIntWithArbitraryPrecision defaultFlags)
        "Use 'Integer' instead of 'Int' to support arbitrary number precision"
      <*> simpleOption
        "use-date-types-as-string"
        (optUseDateTypesAsString defaultFlags)
        "Convert strings formatted as date / date-time to date types"
      <*> simpleOption
        "convert-to-camel-case"
        (optConvertToCamelCase defaultFlags)
        "Convert names to CamelCase instead of using names which are as close as possible to the names provided in the specification"
      <*> simpleOption
        "property-type-suffix"
        (optPropertyTypeSuffix defaultFlags)
        "Add a suffix to property types to prevent naming conflicts"
      <*> simpleOption
        "response-type-suffix"
        (optResponseTypeSuffix defaultFlags)
        "The suffix which is added to the response data types"
      <*> simpleOption
        "response-body-type-suffix"
        (optResponseBodyTypeSuffix defaultFlags)
        "The suffix which is added to the response body data types"
      <*> simpleOption
        "request-body-type-suffix"
        (optRequestBodyTypeSuffix defaultFlags)
        "The suffix which is added to the request body data types"
      <*> simpleOption
        "parameters-type-suffix"
        (optParametersTypeSuffix defaultFlags)
        "The suffix which is added to the parameters type of operations"
      <*> simpleOption
        "parameter-query-prefix"
        (optParameterQueryPrefix defaultFlags)
        "The prefix which is added to query parameters"
      <*> simpleOption
        "parameter-path-prefix"
        (optParameterPathPrefix defaultFlags)
        "The prefix which is added to path parameters"
      <*> simpleOption
        "parameter-cookie-prefix"
        (optParameterCookiePrefix defaultFlags)
        "The prefix which is added to cookie parameters"
      <*> simpleOption
        "parameter-header-prefix"
        (optParameterHeaderPrefix defaultFlags)
        "The prefix which is added to header parameters"
      <*> simpleOption
        "use-numbered-variant-constructors"
        (optUseNumberedVariantConstructors defaultFlags)
        "Use numbered data constructors (e. g. Variant1, Variant 2, etc.) for one-of types"

-- | Default flags which can be used for tests and as a reference which values actually are used
defaultFlags :: Flags
defaultFlags =
  Flags
    { optOutputDir = "out",
      optGenerateStackProject = True,
      optDryRun = False,
      optForce = False,
      optPackageName = "openapi",
      optModuleName = "OpenAPI",
      optUseFloatWithArbitraryPrecision = False,
      optUseIntWithArbitraryPrecision = False,
      optUseDateTypesAsString = False,
      optConvertToCamelCase = False,
      optPropertyTypeSuffix = "",
      optResponseTypeSuffix = "Response",
      optResponseBodyTypeSuffix = "ResponseBody",
      optRequestBodyTypeSuffix = "RequestBody",
      optParametersTypeSuffix = "Parameters",
      optParameterQueryPrefix = "query",
      optParameterPathPrefix = "path",
      optParameterCookiePrefix = "cookie",
      optParameterHeaderPrefix = "header",
      optUseNumberedVariantConstructors = False
    }
