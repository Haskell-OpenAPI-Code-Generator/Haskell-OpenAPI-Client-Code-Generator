final: prev:
with final.lib;
with final.haskell.lib;
{
  generateOpenAPIClient = final.callPackage ./generate-client.nix { };
  openapi3-code-generator = (justStaticExecutables final.haskellPackages.openapi3-code-generator).overrideAttrs (old: {
    passthru = (old.passthru or { }) // {
      generateOpenAPIClient = final.generateOpenAPIClient;
    };
  });
  haskellPackages = prev.haskellPackages.override (old: {
    overrides = final.lib.composeExtensions (old.overrides or (_: _: { })) (
      self: super: {
        openapi3-code-generator = buildStrictly (self.callPackage ../openapi3-code-generator { });
      }
    );
  });
}
