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
  pkgs = (import <nixpkgs> {inherit config; });
in
    pkgs.haskellPackages.lib.buildStackProject {
    name = "log-classifier";
    buildInputs = [ pkgs.zlib ];
    }
