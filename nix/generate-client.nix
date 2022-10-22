{ lib
, stdenv
, haskellPackages
, openapi3-code-generator
}:
{ name
, src ? null
, configFile ? null
, extraFlags ? [ ]
}:

with lib;
let
  extraFlagsStr = concatStringsSep " " extraFlags;
  specificationArgument = optionalString (! builtins.isNull src) src;
  configFileFlag = optionalString (! builtins.isNull configFile) "--configuration ${configFile}";
  generatedCode = stdenv.mkDerivation {
    name = "generated-${name}";
    buildInputs = [
      openapi3-code-generator
    ];
    buildCommand = ''
      # To make sure that we don't get issues with encodings
      export LANG=C.utf8
      export LC_ALL=C.utf8

      set -x

      openapi3-code-generator-exe ${specificationArgument} \
        --output-dir "$out" \
        ${configFileFlag} \
        ${extraFlagsStr}

      set +x
    '';
  };
  generatedPackage = haskellPackages.callPackage ("${generatedCode}/default.nix") { };
in
{
  code = generatedCode;
  package = generatedPackage;
}
