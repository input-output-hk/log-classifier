let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.ghc822.override {
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
  pkgs.haskellPackages.callPackage ./cabal2nix.nix {}
