let
  pkgs = import ./nix/pkgs.nix;
in
pkgs.haskellPackages.openapi3-code-generator
