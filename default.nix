let lib = import ./lib.nix;

    pkgs = import lib.fetchNixPkgs {};
in
  pkgs.haskell.packages.ghc822.callPackage ./cabal2nix.nix {}
