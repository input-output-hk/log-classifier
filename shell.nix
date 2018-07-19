with import ((import ./lib.nix).fetchNixPkgs) { };

let
  hsPkgs = haskell.packages.ghc822;
in
  haskell.lib.buildStackProject {
     name = "log-classifier";
     ghc = hsPkgs.ghc;
     buildInputs = [
       zlib unzip openssh autoreconfHook openssl
       gmp rocksdb git bsdiff ncurses
       hsPkgs.happy hsPkgs.cpphs lzma
       perl bash
     # cabal-install and stack pull in lots of dependencies on OSX so skip them
     # See https://github.com/NixOS/nixpkgs/issues/21200
     ] ++ (lib.optionals stdenv.isLinux [ cabal-install stack ])
       ++ (lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [ Cocoa CoreServices libcxx libiconv ]));
}
