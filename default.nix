{ mkDerivation, aeson, aeson-pretty, base, bytestring
, case-insensitive, connection, containers, exceptions
, http-conduit, http-types, HUnit, mtl, network, parsec, stdenv
, transformers
}:
mkDerivation {
  pname = "icicli";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-pretty base bytestring case-insensitive connection
    containers exceptions http-conduit http-types
  ];
  executableHaskellDepends = [
    base containers mtl network parsec transformers
  ];
  testHaskellDepends = [ aeson base bytestring HUnit ];
  homepage = "https://github.com/kuznero/icicli#readme";
  description = "Icinga 2 terminal client";
  license = stdenv.lib.licenses.bsd3;
}
