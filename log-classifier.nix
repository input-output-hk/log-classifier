{ mkDerivation, aeson, attoparsec, base, bytestring, concurrency
, containers, dejafu, directory, hspec, hspec-contrib, http-conduit
, hunit-dejafu, monad-control, mtl, QuickCheck, resource-pool
, safe-exceptions, sqlite-simple, stdenv, text, time
, transformers-base, universum, unliftio, zip-archive
}:
mkDerivation {
  pname = "log-classifier";
  version = "1.6.0.0";
  src = ./classifier/.;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring concurrency containers directory
    http-conduit monad-control mtl QuickCheck resource-pool
    safe-exceptions sqlite-simple text time transformers-base universum
    unliftio zip-archive
  ];
  testHaskellDepends = [
    aeson base bytestring concurrency containers dejafu hspec
    hspec-contrib http-conduit hunit-dejafu QuickCheck sqlite-simple
    text time universum unliftio
  ];
  homepage = "https://github.com/input-output-hk/log-classifier#readme";
  description = "Log classifier for a Cardano node";
  license = stdenv.lib.licenses.mit;
}
