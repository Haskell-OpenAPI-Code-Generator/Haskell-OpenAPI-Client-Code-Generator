let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit = import ./nix/pre-commit.nix { inherit sources; };
in
{
  "pre-commit-hooks" = pre-commit.check;
  "shell" = pkgs.symlinkJoin {
    name = "shell";
    paths = (import ./shell.nix { inherit sources pkgs pre-commit; }).buildInputs;
  };
  "build" = pkgs.haskellPackages.openapi3-code-generator;
  "test-system-1" = pkgs.testSystem1;
  "test-system-2" = pkgs.testSystem2;
  "test-system-3" = pkgs.testSystem3;
  "test-golden" = pkgs.testGolden;
  "test-golden-generate" = pkgs.testGoldenGenerate;
  "all-checks" = pkgs.symlinkJoin {
    name = "all-checks";
    paths = [ pkgs.testSystem1 pkgs.testSystem2 pkgs.testSystem3 pkgs.testGolden ];
  };
}
