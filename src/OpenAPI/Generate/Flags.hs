module OpenAPI.Generate.Flags where

import Options

data Flags
  = Flags
      { optOutputDir :: String,
        optGenerateStackProject :: Bool,
        optDryRun :: Bool,
        optForce :: Bool,
        optPackageName :: String,
        optModuleName :: String,
        optUseFloatWithArbitraryPrecision :: Bool,
        optUseDateTypesAsString :: Bool,
        optConvertToCamelCase :: Bool,
        optPropertyTypeSuffix :: String,
        optResponseTypeSuffix :: String,
        optResponseBodyTypeSuffix :: String,
        optRequestBodyTypeSuffix :: String
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
        "Should a stack project be generated alongside the Haskell files?"
      <*> simpleOption
        "dry-run"
        (optDryRun defaultFlags)
        "Do not generate the output files but only print the generated code."
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
        "Name of the Module"
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
      optRequestBodyTypeSuffix = "RequestBody"
    }
