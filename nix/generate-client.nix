{ pkgs
, openapi3-code-generator ? pkgs.openapi3-code-generator
}:
{ name
, src ? null
, configFile ? null
, packageName ? name
, moduleName ? null
, extraFlags ? [ ]
, operations ? [ ] # Empty list means "All operations"
, schemas ? [ ] # if 'operations' is also empty, this means "All schemas"
}:

with pkgs.lib;
with pkgs.haskell.lib;
let
  extraFlagsStr = concatStringsSep " " extraFlags;
  specificationArgument = optionalString (! builtins.isNull src) src;
  omitFlag = optionalString (schemas != [ ] && operations != [ ]) "--omit-additional-operation-functions";
  operationFlag = operation: "--operation-to-generate '${operation}'";
  operationsFlags = concatStringsSep " " (map operationFlag operations);
  schemaFlag = schema: "--white-listed-schema '${schema}'";
  schemasFlags = concatStringsSep " " (map schemaFlag schemas);
  configFileFlag = optionalString (! builtins.isNull configFile) "--configuration ${configFile}";
  moduleNameFlag = optionalString (! builtins.isNull moduleName) "--module-name ${moduleName}";
  packageNameFlag = optionalString (! builtins.isNull packageName) "--package-name ${packageName}";
  generatedCode = pkgs.stdenv.mkDerivation {
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
        ${moduleNameFlag} \
        ${packageNameFlag} \
        ${extraFlagsStr} ${omitFlag} ${operationsFlags} ${schemasFlags}

      set +x
    '';
  };
  generatedPackage = dontHaddock (disableLibraryProfiling (pkgs.haskellPackages.callPackage ("${generatedCode}/default.nix") { }));
in
{
  code = generatedCode;
  package = generatedPackage;
}
