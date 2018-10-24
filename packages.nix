{ mkDerivation, aeson, base, containers, freer-simple, servant
, servant-server, stdenv, tagged, text, wai, warp
}:
mkDerivation {
  pname = "freer-experiment";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers freer-simple servant servant-server tagged
    text wai warp
  ];
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.bsd3;
}
