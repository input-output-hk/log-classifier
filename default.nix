with import <nixpkgs> {};

let
  ghc = haskellPackages.ghcWithPackages (ps: with ps; [aeson array attoparsec bytestring containers http-conduit
                                                       mtl optparse-applicative regex-tdfa reflection universum zip-archive]);
in runCommand "log-classifier" { buildInputs = [ ghc haskellPackages.ghcid ]; } ''
  cp -r ${builtins.fetchGit ./.} src
  chmod -R +w src
  cd src
  mkdir -p $out/bin/
  ghc Zendesk.hs -o $out/bin/log-classifier -Wall
''
