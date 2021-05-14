(import <nixpkgs> { }).mkShell {
  inherit ((import ./default.nix).pre-commit-check) shellHook;
}
