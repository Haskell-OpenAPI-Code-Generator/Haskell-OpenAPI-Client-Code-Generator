{ sources ? import ./nix/sources.nix
, pkgs ? import ./nix/pkgs.nix { inherit sources; }
, pre-commit ? import ./nix/pre-commit.nix { inherit sources; }
}:
pkgs.haskell.lib.buildStackProject {
  name = "Haskell-OpenAPI-Client-Code-Generator-shell";
  buildInputs = with pkgs; [
    niv
    zlib
    cachix
  ] ++ pre-commit.tools;
  shellHook = ''
    export TMPDIR=/tmp
    ${pre-commit.check.shellHook}
  '';
}
