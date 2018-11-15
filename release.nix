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
  nixpkgs = builtins.fetchTarball {
    url    = "https://github.com/NixOS/nixpkgs/archive/069bf7aee30faf7b3ed773cfae2154d761b2d6c2.tar.gz";
    sha256 = "1c44vjb60fw2r8ck8yqwkj1w4288wixi59c6w1vazjixa79mvjvg";
  };

  pkgs = import nixpkgs { inherit config; };

in
  { log-classifier-web = pkgs.haskellPackages.log-classifier-web;
  }
