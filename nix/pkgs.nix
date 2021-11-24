let
  sources = import ./sources.nix;
  pkgsv = import sources.nixpkgs;
  ourPkgs =
    pkgsv {
      overlays =
        [
          (import (sources.autodocodec + "/nix/overlay.nix"))
          (import (sources.safe-coloured-text + "/nix/overlay.nix"))
          (final: previous: { niv = (import sources.niv { }).niv; })
          (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
          (import ./overlay.nix)
        ];
    };
in
ourPkgs
