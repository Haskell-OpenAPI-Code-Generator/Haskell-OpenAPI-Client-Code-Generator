{ pkgs }:
with pkgs.lib;
with pkgs.haskell.lib;
let
  generateCode = { fileName, extraFlags ? [ ] }:
    let
      name = builtins.replaceStrings [ "_" ] [ "-" ] (strings.removeSuffix ".yaml" (strings.removeSuffix ".json" (strings.removeSuffix ".yml" fileName)));
    in
    {
      name = name;
      path = (pkgs.generateOpenAPIClient {
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
  mockServer = disableOptimization (disableLibraryProfiling (pkgs.haskellPackages.callCabal2nix "level-3-mock-server" (../testing/level3/mock-server) { }));
in
{
  testSystem1 = pkgs.symlinkJoin {
    name = "test-system-1";
    paths = (builtins.map
      (pkg:
        disableOptimization (disableLibraryProfiling (pkgs.haskellPackages.callCabal2nix "${pkg.name}-compiled" pkg.path { }))
      )
      codeForSpecsLevelOne);
  };
  testSystem2 = pkgs.symlinkJoin {
    name = "test-system-2";
    paths = (builtins.map
      (pkg:
        let
          openapi = disableOptimization (disableLibraryProfiling (pkgs.haskellPackages.callCabal2nix "${pkg.name}-compiled" pkg.path { }));
          base = disableOptimization (disableLibraryProfiling (pkgs.haskellPackages.callCabal2nix "level2-base" ../testing/level2/level2-base { openapi = openapi; }));
        in
        disableOptimization (disableLibraryProfiling (pkgs.haskellPackages.callCabal2nix "${pkg.name}-tests" (../testing/level2 + "/${pkg.name}") { level2-base = base; openapi = openapi; }))
      )
      codeForSpecsLevelTwo);
  };
  testSystem3 = pkgs.symlinkJoin {
    name = "test-system-3";
    paths = (builtins.map
      (pkg:
        let
          openapi = disableOptimization (disableLibraryProfiling (pkgs.haskellPackages.callCabal2nix "${pkg.name}-compiled" pkg.path { }));
        in
        overrideCabal (disableOptimization (disableLibraryProfiling (pkgs.haskellPackages.callCabal2nix "${pkg.name}-tests" (../testing/level3 + "/${pkg.name}") { openapi = openapi; }))) (old: {
          preCheck = (old.preCheck or "") + ''
            ${pkgs.killall}/bin/killall -r mock-server-exe || true
            ${mockServer}/bin/mock-server-exe &
          '';
          postCheck = (old.postCheck or "") + ''
            ${pkgs.killall}/bin/killall -r mock-server-exe
          '';
        })
      )
      codeForSpecsLevelThree);
  };
  testGolden = pkgs.stdenv.mkDerivation {
    name = "test-golden";
    src = ../testing/golden-output;

    buildCommand = ''
      diff -r ${goldenTestCode.path} $src
      mkdir -p $out
      echo "Golden test success" > $out/success
    '';
  };
  testGoldenGenerate = pkgs.stdenv.mkDerivation {
    name = "test-golden-generate";
    buildCommand = ''
      cp -R ${goldenTestCode.path} $out
    '';
  };
}
