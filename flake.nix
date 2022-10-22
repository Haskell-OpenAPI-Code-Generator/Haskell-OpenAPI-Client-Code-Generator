{
  description = "openapi-code-generator";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity?ref=flake";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec?ref=flake";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text?ref=flake";
    safe-coloured-text.flake = false;
    sydtest.url = "github:NorfairKing/sydtest?ref=flake";
    sydtest.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , flake-utils
    , pre-commit-hooks
    , validity
    , safe-coloured-text
    , sydtest
    , autodocodec
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ]
      (system:
      let
        pkgsFor = nixpkgs: import nixpkgs {
          inherit system;
          overlays = [
            self.overlays.${system}
            (import (autodocodec + "/nix/overlay.nix"))
            (import (safe-coloured-text + "/nix/overlay.nix"))
            (import (sydtest + "/nix/overlay.nix"))
            (import (validity + "/nix/overlay.nix"))
          ];
        };
        pkgs = pkgsFor nixpkgs;

      in
      {
        overlays = import ./nix/overlay.nix;
        packages.release = pkgs.openapi3-code-generator;
        packages.default = self.packages.${system}.release;
        checks = {
          test-system-1 = pkgs.testSystem1;
          test-system-2 = pkgs.testSystem2;
          test-system-3 = pkgs.testSystem3;
          test-golden = pkgs.testGolden;
          test-golden-generate = pkgs.testGoldenGenerate;
          pre-commit = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              nixpkgs-fmt = {
                enable = true;
                excludes = [
                  "example"
                  "testing/golden-output"
                  ".*/default.nix"
                ];
              };
              hlint = {
                enable = true;
                excludes = [
                  "example"
                  "testing/golden-output"
                ];
              };
              ormolu = {
                enable = true;
                excludes = [
                  "example"
                  "testing/golden-output"
                ];
              };

              cabal2nix = {
                enable = true;
                excludes = [
                  "example"
                  "testing/golden-output"
                ];
              };
            };
          };
        };
        devShells.default = pkgs.haskellPackages.shellFor {
          name = "openapi-code-generator-shell";
          packages = (p:
            [ p.openapi3-code-generator ]
          );
          withHoogle = true;
          doBenchmark = true;
          buildInputs = with pkgs; [
            niv
            zlib
            cabal-install
          ] ++ (with pre-commit-hooks;
            [
              hlint
              hpack
              nixpkgs-fmt
              ormolu
              cabal2nix
            ]);
          shellHook = self.checks.${system}.pre-commit.shellHook;
        };
      });
}
