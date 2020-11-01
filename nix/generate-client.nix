{ pkgs ? import ./pkgs.nix
}:
{ name
, src
, packageName ? name
, moduleName
, extraFlags ? [ ]
, operations ? [ ] # Empty list means "All operations"
, schemas ? [ ] # if 'operations' is also empty, this means "All schemas"
}:

with pkgs.lib;
with pkgs.haskell.lib;
let
  localPkgs = import ./pkgs.nix;
  extraFlagsStr = concatStringsSep " " extraFlags;
  omitFlag = optionalString (schemas != [] && operations != []) "--omit-additional-operation-functions";
  operationFlag = operation: "--operation-to-generate '${operation}'";
  operationsFlags = concatStringsSep " " (map operationFlag operations);
  schemaFlag = schema: "--white-listed-schema '${schema}'";
  schemasFlags = concatStringsSep " " (map schemaFlag schemas);
  generatedCode = pkgs.stdenv.mkDerivation {
    name = "generated-${name}";
    buildInputs = [
      localPkgs.haskellPackages.openapi3-code-generator
    ];
    buildCommand = ''
      # To make sure that we don't get issues with encodings
      export LANG=C.utf8
      export LC_ALL=C.utf8

      set -x

      openapi3-code-generator-exe ${src}  \
        --module-name "${moduleName}" \
        --package-name "${packageName}" \
        --output-dir "$out" \
        ${extraFlagsStr} ${omitFlag} ${operationsFlags} ${schemasFlags}

      set +x
    '';
  };
  generatedPackage = dontHaddock (disableLibraryProfiling (pkgs.haskellPackages.callCabal2nix name generatedCode { }));
in
{
  code = generatedCode;
  package = generatedPackage;
}
