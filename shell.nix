{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, array, attoparsec, base, bytestring
      , containers, directory, generics-sop, hspec, http-conduit
      , monad-control, mtl, optparse-applicative, QuickCheck, reflection
      , regex-tdfa, resource-pool, sqlite-simple, stdenv, text, time
      , transformers-base, universum, zip-archive
      }:
      mkDerivation {
        pname = "log-classifier";
        version = "1.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson array attoparsec base bytestring containers directory
          generics-sop http-conduit monad-control mtl optparse-applicative
          QuickCheck reflection regex-tdfa resource-pool sqlite-simple text
          time transformers-base universum zip-archive
        ];
        executableHaskellDepends = [ base universum ];
        testHaskellDepends = [
          aeson array attoparsec base bytestring containers hspec
          http-conduit mtl QuickCheck reflection regex-tdfa text universum
          zip-archive
        ];
        homepage = "https://github.com/input-output-hk/log-classifier#readme";
        description = "Log classifier for a Cardano node";
        license = stdenv.lib.licenses.mit;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
