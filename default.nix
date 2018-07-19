let lib = import ./lib.nix;

let hsPkgs = haskell.packages.ghc822;
in
  fetchNixPkgs.callPackage ./cabal2nix.nix {}
