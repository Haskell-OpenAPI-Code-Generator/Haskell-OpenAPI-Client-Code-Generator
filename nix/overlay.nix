final: previous:
with final.lib;
with final.haskell.lib;
let
  openAPICodeGeneratorRepo = builtins.fetchGit {
    url = "https://github.com/Haskell-OpenAPI-Code-Generator/Haskell-OpenAPI-Client-Code-Generator";
    rev = "c3133bde91430990ff9c03c19a015713ae291e2f";
    ref = "master";
  };
  openAPICodeGenerator = dontHaddock (disableLibraryProfiling (final.haskellPackages.callCabal2nix "openapi3-code-generator" (final.gitignoreSource ../.) { }));
  generatedStripeCode = final.stdenv.mkDerivation {
    name = "generated-stripe-code";
    buildInputs = [
      openAPICodeGenerator
      final.tree
    ];
    buildCommand = ''
      # To make sure that we don't get issues with encodings
      export LANG=C.utf8
      export LC_ALL=C.utf8

      ${../scripts/generate-stripe.sh} ${openAPICodeGeneratorRepo} $out
    '';
  };
  generatedStripePackage = dontHaddock (disableLibraryProfiling (final.haskellPackages.callCabal2nix "stripe-api" generatedStripeCode { }));

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
                  openapi3-code-generator = openAPICodeGenerator;
                }
            );
      }
    );
}
