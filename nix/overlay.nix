final: prev:
with final.lib;
with final.haskell.lib;
let
  generateOpenAPIClient = import ./generate-client.nix { pkgs = final; };
  generateCode = { fileName, extraFlags ? [ ] }:
    let
      name = builtins.replaceStrings [ "_" ] [ "-" ] (strings.removeSuffix ".yaml" (strings.removeSuffix ".json" (strings.removeSuffix ".yml" fileName)));
    in
    {
      name = name;
      path = (generateOpenAPIClient {
        name = "openapi";
        src = ../specifications + "/${fileName}";
        extraFlags = extraFlags;
      }).code;
    };
  goldenTestCode = generateCode { fileName = "z_complex_self_made_example.yml"; };
  codeForSpecsLevelOne = [
    (generateCode { fileName = "google-payment.json"; })
    (generateCode { fileName = "hetzner.json"; })
    (generateCode { fileName = "official-api-with-examples.yaml"; })
    (generateCode { fileName = "official-callback-example.yaml"; })
    (generateCode { fileName = "official-link-example.yaml"; })
    (generateCode { fileName = "official-petstore-expanded.yaml"; })
    (generateCode { fileName = "official-uspto.yaml"; })
    (generateCode { fileName = "petstore-expanded.json"; })
    (generateCode { fileName = "selenium.yaml"; extraFlags = [ "--response-type-suffix=\"Response'\"" ]; })
    (generateCode { fileName = "spot_api.yml"; extraFlags = [ "--opaque-schema=\"aggTrade\"" ]; })
    (generateCode { fileName = "uber.json"; })
    (generateCode { fileName = "z_complex_self_made_example.yml"; })
  ];
  codeForSpecsLevelTwo = [
    (generateCode { fileName = "official-petstore.yaml"; })
    (generateCode { fileName = "petstore-running-example.yaml"; })
    (generateCode { fileName = "stripe-api.yml"; extraFlags = [ "--property-type-suffix=\"'\"" "--convert-to-camel-case" ]; })
  ];
  codeForSpecsLevelThree = [
    (generateCode { fileName = "petstore-running-example.yaml"; })
  ];
  haskellPackages =
    prev.haskellPackages.override (
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
                  openapi3-code-generator = buildStrictly (final.haskellPackages.callCabal2nixWithOptions "openapi3-code-generator" ../openapi3-code-generator "--no-hpack" { });
                }
            );
      }
    );
in
{
  generateOpenAPIClient = import ./generate-client.nix { pkgs = final; };
  openapi3-code-generator = justStaticExecutables final.haskellPackages.openapi3-code-generator;
  haskellPackages = haskellPackages;
  mockServer = disableOptimization (disableLibraryProfiling (haskellPackages.callCabal2nix "level-3-mock-server" (../testing/level3/mock-server) { }));
  testSystem1 = final.symlinkJoin {
    name = "test-system-1";
    paths = (builtins.map
      (pkg:
        disableOptimization (disableLibraryProfiling (haskellPackages.callCabal2nix "${pkg.name}-compiled" pkg.path { }))
      )
      codeForSpecsLevelOne);
  };
  testSystem2 = final.symlinkJoin {
    name = "test-system-2";
    paths = (builtins.map
      (pkg:
        let
          openapi = disableOptimization (disableLibraryProfiling (haskellPackages.callCabal2nix "${pkg.name}-compiled" pkg.path { }));
          base = disableOptimization (disableLibraryProfiling (haskellPackages.callCabal2nix "level2-base" ../testing/level2/level2-base { openapi = openapi; }));
        in
        disableOptimization (disableLibraryProfiling (haskellPackages.callCabal2nix "${pkg.name}-tests" (../testing/level2 + "/${pkg.name}") { level2-base = base; openapi = openapi; }))
      )
      codeForSpecsLevelTwo);
  };
  testSystem3 = final.symlinkJoin {
    name = "test-system-3";
    paths = (builtins.map
      (pkg:
        let
          openapi = disableOptimization (disableLibraryProfiling (haskellPackages.callCabal2nix "${pkg.name}-compiled" pkg.path { }));
        in
        overrideCabal (disableOptimization (disableLibraryProfiling (haskellPackages.callCabal2nix "${pkg.name}-tests" (../testing/level3 + "/${pkg.name}") { openapi = openapi; }))) (old: {
          preCheck = (old.preCheck or "") + ''
            ${final.killall}/bin/killall -r mock-server-exe || true
            ${final.mockServer}/bin/mock-server-exe &
          '';
          postCheck = (old.postCheck or "") + ''
            ${final.killall}/bin/killall -r mock-server-exe
          '';
        })
      )
      codeForSpecsLevelThree);
  };
  testGolden = final.stdenv.mkDerivation {
    name = "test-golden";
    src = ../testing/golden-output;

    buildCommand = ''
      diff -r ${goldenTestCode.path} $src
      mkdir -p $out
      echo "Golden test success" > $out/success
    '';
  };
  testGoldenGenerate = final.stdenv.mkDerivation {
    name = "test-golden-generate";
    buildCommand = ''
      cp -R ${goldenTestCode.path} $out
    '';
  };
}
