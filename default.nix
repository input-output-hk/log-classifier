let lib = import ./lib.nix;

    pkgs = import lib.fetchNixPkgs {};
in
  pkgs.haskellPackages.callPackage ./cabal2nix.nix {}
