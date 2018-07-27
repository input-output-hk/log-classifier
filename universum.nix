{ mkDerivation, base, bytestring, containers, criterion, deepseq
, doctest, ghc-prim, Glob, hashable, hedgehog, microlens
, microlens-mtl, mtl, safe-exceptions, semigroups, stdenv, stm
, tasty, tasty-hedgehog, text, text-format, transformers
, type-operators, unordered-containers, utf8-string, vector
}:
mkDerivation {
  pname = "universum";
  version = "1.1.0";
  sha256 = "177635a009f38edb8fc764bf9504077fb41af033c49b2a0ae2c725b55a9a2f4c";
  libraryHaskellDepends = [
    base bytestring containers deepseq ghc-prim hashable microlens
    microlens-mtl mtl safe-exceptions stm text text-format transformers
    type-operators unordered-containers utf8-string vector
  ];
  testHaskellDepends = [
    base bytestring doctest Glob hedgehog tasty tasty-hedgehog text
    utf8-string
  ];
  benchmarkHaskellDepends = [
    base containers criterion deepseq hashable mtl semigroups text
    unordered-containers
  ];
  homepage = "https://github.com/serokell/universum";
  description = "Custom prelude used in Serokell";
  license = stdenv.lib.licenses.mit;
}
