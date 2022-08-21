{ sources ? import ./sources.nix }:
let
  nix-pre-commit-hooks = import sources.pre-commit-hooks;
in
{
  tools = with nix-pre-commit-hooks; [
    hlint
    hpack
    nixpkgs-fmt
    ormolu
  ];
  check = nix-pre-commit-hooks.run {
    src = ../.;
    hooks = {
      nixpkgs-fmt = {
        enable = true;
        excludes = [
          "example"
          ".circleci"
          "golden-output"
        ];
      };
      hlint = {
        enable = true;
        excludes = [
          "example"
          ".circleci"
          "golden-output"
        ];
      };
      ormolu = {
        enable = true;
        excludes = [
          "example"
          ".circleci"
          "golden-output"
        ];
      };
    };
  };
}
