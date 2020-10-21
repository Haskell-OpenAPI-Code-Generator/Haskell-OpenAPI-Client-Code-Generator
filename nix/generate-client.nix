{ pkgs ? import ./pkgs.nix
}:
{ name
, src
, packageName ? name
, moduleName
, extraFlags ? [ ]
, operations ? [ ] # Empty list means "All operations"
}:

with pkgs.lib;
with pkgs.haskell.lib;
let
  localPkgs = import ./pkgs.nix;
  extraFlagsStr = concatStringsSep "\\\n" extraFlags;
  operationFlag = operation: "--operation-to-generate \"${operation}\" \\";
  operationsFlags = optionalString (operations != [ ]) ''
    --omit-additional-operation-functions \
    ${concatStringsSep "\n" (map operationFlag operations)}
  '';
  generatedCode = pkgs.stdenv.mkDerivation {
    name = "generated-${name}";
    buildInputs = [
      localPkgs.haskellPackages.openapi3-code-generator
    ];
    buildCommand = ''
      # To make sure that we don't get issues with encodings
      export LANG=C.utf8
      export LC_ALL=C.utf8

      openapi3-code-generator-exe ${src}  \
        --module-name "${moduleName}" \
        --package-name "${packageName}" \
        --output-dir "$out" \
        ${extraFlagsStr} \
        \
        ${operationsFlags}
    '';
  };
  generatedPackage = dontHaddock (disableLibraryProfiling (pkgs.haskellPackages.callCabal2nix name generatedCode { }));
in
{
  code = generatedCode;
  package = generatedPackage;
}
