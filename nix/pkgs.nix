{ sources ? import ./sources.nix
}:
let
  pkgsv = import sources.nixpkgs;
  ourPkgs =
    pkgsv {
      overlays =
        [
          (final: previous: { inherit (import sources.gitignore { inherit (final) lib; }) gitignoreSource; })
          (final: previous: { niv = (import sources.niv { }).niv; })
          (import (sources.autodocodec + "/nix/overlay.nix"))
          (import (sources.safe-coloured-text + "/nix/overlay.nix"))
          (import (sources.validity + "/nix/overlay.nix"))
          (import ./overlay.nix)
        ];
    };
in
ourPkgs
