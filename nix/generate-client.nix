{ lib
, stdenv
, openapi3-code-generator
, cabal2nix
}:
{ name
, src
, configFile ? null
, extraFlags ? [ ]
}:

stdenv.mkDerivation {
  name = "generated-${name}";
  inherit src;
  buildInputs = [ openapi3-code-generator cabal2nix ];
  buildCommand = ''
    # To make sure that we don't get issues with encodings
    export LANG=C.utf8
    export LC_ALL=C.utf8

    set -x

    openapi3-code-generator-exe $src \
      --output-dir "$out" ${lib.optionalString (!builtins.isNull configFile) "--configuration ${configFile}"} \
      ${lib.concatStringsSep " " extraFlags}
    cd $out
    cabal2nix . > default.nix

    set +x
  '';
}
