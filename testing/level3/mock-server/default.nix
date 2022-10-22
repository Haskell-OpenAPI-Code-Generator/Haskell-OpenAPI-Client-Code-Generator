{ mkDerivation, aeson, base, lib, servant-server
, unordered-containers, wai, warp
}:
mkDerivation {
  pname = "mock-server";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base servant-server unordered-containers wai warp
  ];
  executableHaskellDepends = [
    aeson base servant-server unordered-containers wai warp
  ];
  license = lib.licenses.mit;
}
