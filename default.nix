let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.ghc822.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          universum =
            pkgs.haskell.lib.dontCheck
              (haskellPackagesNew.callHackage "universum" "1.1.0" {});
          log-classifier =
            (haskellPackagesNew.callCabal2nix "log-classifier" ./. {});
        };
      };
    };
  };

  lib = import ./lib.nix;

  pkgs = import lib.fetchNixPkgs {inherit config; };
in
  haskell.lib.buildStackProject {
    inherit ghc;
    name = "myEnv";
    buildInputs = [ glpk pcre ];
    };
