let
  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.ghc822.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          universum           = pkgs.haskell.lib.dontCheck (haskellPackagesNew.callHackage "universum" "1.1.0" {});
          concurrent-output   = haskellPackagesNew.callPackage ./concurrent-output.nix { };
          # because of http://hackage.haskell.org/package/concurrent-output
          stm                 = haskellPackagesNew.callPackage ./stm.nix { };
          log-classifier      = (haskellPackagesNew.callCabal2nix "log-classifier" ./. {});
        };
      };
    };
  };
in
  { pkgs ? import <nixpkgs> {inherit config; }}:
  pkgs.haskell.lib.buildStackProject {
    name = "log-classifier";
    buildInputs = with pkgs; [ unzip zlib gmp ];
    LANG = "en_US.UTF-8";
  }
