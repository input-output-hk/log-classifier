let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.ghc822.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          universum =
            haskellPackagesNew.callHackage "universum" "1.1.0" {};
        };
      };
    };
  };

  lib = import ./lib.nix;

  pkgs = import lib.fetchNixPkgs {inherit config; };
in
  pkgs.haskellPackages.callCabal2nix "log-classifier" ./. {} 
