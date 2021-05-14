let
  pkgs = import ./nix/pkgs.nix;
  nix-pre-commit-hooks = import (builtins.fetchTarball "https://github.com/cachix/pre-commit-hooks.nix/tarball/master");
in
{
  openapi3-code-generator = pkgs.haskellPackages.openapi3-code-generator;
  pre-commit-check = nix-pre-commit-hooks.run {
    src = ./.;
    hooks = {
      ormolu = {
        enable = true;
        excludes = [ ".circleci/golden" "example" ];
      };
      hlint = {
        enable = true;
        excludes = [ ".circleci/golden" "example" ];
      };
      nixpkgs-fmt.enable = true;
    };
  };
}
