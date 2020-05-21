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
        -- | Use Data.Scientific instead of Double to support arbitary number precision
        optUseFloatWithArbitraryPrecision :: Bool,
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
      optUseDateTypesAsString = False,
      optConvertToCamelCase = False,
      optPropertyTypeSuffix = "",
      optResponseTypeSuffix = "Response",
      optResponseBodyTypeSuffix = "ResponseBody",
      optRequestBodyTypeSuffix = "RequestBody",
      optUseNumberedVariantConstructors = False
    }
