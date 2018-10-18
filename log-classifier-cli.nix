{ mkDerivation, base, log-classifier, optparse-applicative, stdenv
, time, universum
}:
mkDerivation {
  pname = "cli";
  version = "1.6.0.0";
  src = ./cli/.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base log-classifier optparse-applicative time universum
  ];
  homepage = "https://github.com/input-output-hk/log-classifier#readme";
  description = "Log classifier CLI executable";
  license = stdenv.lib.licenses.mit;
}
