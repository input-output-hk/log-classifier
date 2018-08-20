let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.ghc822.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          universum =
            haskellPackagesNew.callPackage ./universum.nix { };
          weeder = haskellPackagesNew.callPackage ./weeder.nix { };
        };
      };
    };
  };

  lib = import ./lib.nix;

  pkgs = import lib.fetchNixPkgs {inherit config; };
in
  pkgs.haskellPackages.cabal2nix 
