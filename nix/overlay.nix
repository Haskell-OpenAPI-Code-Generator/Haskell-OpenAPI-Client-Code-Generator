final: previous:
with final.lib;
with final.haskell.lib;
let
in
{
  generateOpenAPIClient = import ./generate-client.nix { pkgs = final; };
  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions
            (
              old.overrides or (
                _:
                _:
                { }
              )
            )
            (
              self: super:
                {
                  openapi3-code-generator = buildStrictly (final.haskellPackages.callCabal2nixWithOptions "openapi3-code-generator" (final.gitignoreSource ../openapi3-code-generator) "--no-hpack" { });
                }
            );
      }
    );
}
