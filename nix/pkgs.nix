let
  pkgsv = import (import ./nixpkgs.nix);
  pkgs = pkgsv { };
  ourPkgs =
    pkgsv {
      overlays =
        [
          (import ./gitignore-src.nix)
          (import ./overlay.nix)
        ];
    };
in
ourPkgs
