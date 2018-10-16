{ mkDerivation, aeson, base, bytestring, log-classifier, servant
, servant-server, stdenv, universum, wai-extra, warp
}:
mkDerivation {
  pname = "web";
  version = "1.6.0.0";
  src = ./web/.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring log-classifier servant servant-server
    universum wai-extra warp
  ];
  homepage = "https://github.com/input-output-hk/log-classifier#readme";
  description = "Log classifier web executable";
  license = stdenv.lib.licenses.mit;
}
