let
  sources = import ./sources.nix;
  pkgsv = import sources.nixpkgs;
  niv-overlay = final: previous: { niv = (import sources.niv { }).niv; };
  gitignore-src-overlay = final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; };
  ourPkgs =
    pkgsv {
      overlays =
        [
          niv-overlay
          gitignore-src-overlay
          (import ./overlay.nix)
        ];
    };
in
ourPkgs
