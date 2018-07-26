let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          universum =
            haskellPackagesNew.callPackage ./universum.nix { };
        };
      };
    };
  };

  lib = import ./lib.nix;

  pkgs = import lib.fetchNixPkgs {inherit config; };
in
  pkgs.haskell.packages.ghc822.callPackage ./cabal2nix.nix {}
