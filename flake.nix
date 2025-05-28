{
  description = "openapi-code-generator";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-25.05";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , flake-utils
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        overlays = [
          self.overlays.${system}
        ];
      };
      pkgs = pkgsFor nixpkgs;

    in
    {
      overlays = import ./nix/overlay.nix;
      packages.default = pkgs.openapi3-code-generator;
      checks =
        let tests = import ./nix/tests.nix { inherit pkgs; };
        in
        {
          inherit (tests)
            testSystem1
            testSystem2
            testSystem3
            testGolden
            testGoldenGenerate
            exampleGenerate
            testExample;
          pre-commit = pre-commit-hooks.lib.${system}.run {
            src = ./.;
            hooks = {
              nixpkgs-fmt = {
                enable = true;
                excludes = [
                  "example/generatedCode"
                  "testing/golden-output"
                  ".*/default.nix"
                ];
              };
              hlint = {
                enable = true;
                excludes = [
                  "example/generatedCode"
                  "testing/golden-output"
                ];
              };
              ormolu = {
                enable = true;
                excludes = [
                  "example/generatedCode"
                  "testing/golden-output"
                ];
              };

              cabal2nix = {
                enable = true;
                excludes = [
                  "example/generatedCode"
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
        buildInputs = (with pkgs; [
          zlib
          cabal-install
          stack
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
    });
}
