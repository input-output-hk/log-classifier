let

  config = {
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.ghc822.override {
        overrides = haskellPackagesNew: haskellPackagesOld: rec {
          universum           = pkgs.haskell.lib.dontCheck (haskellPackagesNew.callHackage "universum" "1.1.0" {});
          concurrent-output   = pkgs.haskell.lib.dontCheck (haskellPackagesNew.callPackage ./concurrent-output.nix {});
          # because of http://hackage.haskell.org/package/concurrent-output
          stm                 = pkgs.haskell.lib.dontCheck (haskellPackagesNew.callPackage ./stm.nix {});

          # LIB
          log-classifier      = haskellPackagesNew.callPackage ./log-classifier.nix { };
          log-classifier-web  = haskellPackagesNew.callPackage ./log-classifier-web.nix { };

        };
      };
    };
  };

  # pinning
  fetchNixPkgs  = import ./fetchNixpkgs.nix (builtins.fromJSON (builtins.readFile ./nixpkgs-src.json));
  pkgs          = import fetchNixPkgs { inherit config; };

in
  { log-classifier-web = pkgs.haskellPackages.log-classifier-web;
  }
