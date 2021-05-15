let
  sources = import ./sources.nix;
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
        ];
      };
      hlint = {
        enable = true;
        excludes = [
          "example"
          ".circleci"
        ];
      };
      ormolu = {
        enable = true;
        excludes = [
          "example"
          ".circleci"
        ];
      };
    };
  };
}
