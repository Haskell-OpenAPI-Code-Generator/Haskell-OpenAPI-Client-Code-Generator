{
  description = "openapi-code-generator";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-23.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , validity
    , safe-coloured-text
    , sydtest
    , autodocodec
    }:
    let
      system = "x86_64-linux";
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
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = pkgs.openapi3-code-generator;
      checks.${system} =
        let tests = import ./nix/tests.nix { inherit pkgs; };
        in
        {
          inherit (tests)
            testSystem1
            testSystem2
            testSystem3
            testGolden
            testGoldenGenerate;
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
      devShells.${system}.default = pkgs.haskellPackages.shellFor {
        name = "openapi-code-generator-shell";
        packages = (p:
          [ p.openapi3-code-generator ]
        );
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; [
          zlib
          cabal-install
        ]) ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook;
      };
    };
}
