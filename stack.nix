{ mkDerivation, aeson, annotated-wl-pprint, ansi-terminal, async
, attoparsec, base, base64-bytestring, bindings-uname, bytestring
, Cabal, conduit, conduit-extra, containers, cryptonite
, cryptonite-conduit, deepseq, directory, echo, exceptions, extra
, file-embed, filelock, filepath, fsnotify, generic-deriving
, gitrev, hackage-security, hashable, hpack, hpc, hspec
, http-client, http-client-tls, http-conduit, http-types, memory
, microlens, mintty, monad-logger, mono-traversable, mtl, mustache
, neat-interpolation, network-uri, open-browser
, optparse-applicative, optparse-simple, path, path-io, persistent
, persistent-sqlite, persistent-template, pretty, primitive
, process, project-template, QuickCheck, regex-applicative-text
, resourcet, retry, rio, semigroups, smallcheck, split, stdenv, stm
, store, store-core, streaming-commons, tar, template-haskell
, temporary, text, text-metrics, th-reify-many, time, tls
, transformers, typed-process, unicode-transforms, unix
, unix-compat, unliftio, unordered-containers, vector, yaml
, zip-archive, zlib
}:
mkDerivation {
  pname = "stack";
  version = "1.7.1";
  sha256 = "19c4f2e02975bb797a339cfe2893c9e1f40241a910da45be34c5c2f05d62329f";
  revision = "8";
  editedCabalFile = "0l6fnradzx4wf5q5zm1yv3zj8mragvzjk472pz3jsvk6iam6ss2c";
  isLibrary = true;
  isExecutable = true;
  setupHaskellDepends = [ base Cabal filepath ];
  libraryHaskellDepends = [
    aeson annotated-wl-pprint ansi-terminal async attoparsec base
    base64-bytestring bindings-uname bytestring Cabal conduit
    conduit-extra containers cryptonite cryptonite-conduit deepseq
    directory echo exceptions extra file-embed filelock filepath
    fsnotify generic-deriving hackage-security hashable hpack hpc
    http-client http-client-tls http-conduit http-types memory
    microlens mintty monad-logger mono-traversable mtl mustache
    neat-interpolation network-uri open-browser optparse-applicative
    path path-io persistent persistent-sqlite persistent-template
    pretty primitive process project-template regex-applicative-text
    resourcet retry rio semigroups split stm store store-core
    streaming-commons tar template-haskell temporary text text-metrics
    th-reify-many time tls transformers typed-process
    unicode-transforms unix unix-compat unliftio unordered-containers
    vector yaml zip-archive zlib
  ];
  executableHaskellDepends = [
    aeson annotated-wl-pprint ansi-terminal async attoparsec base
    base64-bytestring bindings-uname bytestring Cabal conduit
    conduit-extra containers cryptonite cryptonite-conduit deepseq
    directory echo exceptions extra file-embed filelock filepath
    fsnotify generic-deriving gitrev hackage-security hashable hpack
    hpc http-client http-client-tls http-conduit http-types memory
    microlens mintty monad-logger mono-traversable mtl mustache
    neat-interpolation network-uri open-browser optparse-applicative
    optparse-simple path path-io persistent persistent-sqlite
    persistent-template pretty primitive process project-template
    regex-applicative-text resourcet retry rio semigroups split stm
    store store-core streaming-commons tar template-haskell temporary
    text text-metrics th-reify-many time tls transformers typed-process
    unicode-transforms unix unix-compat unliftio unordered-containers
    vector yaml zip-archive zlib
  ];
  testHaskellDepends = [
    aeson annotated-wl-pprint ansi-terminal async attoparsec base
    base64-bytestring bindings-uname bytestring Cabal conduit
    conduit-extra containers cryptonite cryptonite-conduit deepseq
    directory echo exceptions extra file-embed filelock filepath
    fsnotify generic-deriving hackage-security hashable hpack hpc hspec
    http-client http-client-tls http-conduit http-types memory
    microlens mintty monad-logger mono-traversable mtl mustache
    neat-interpolation network-uri open-browser optparse-applicative
    path path-io persistent persistent-sqlite persistent-template
    pretty primitive process project-template QuickCheck
    regex-applicative-text resourcet retry rio semigroups smallcheck
    split stm store store-core streaming-commons tar template-haskell
    temporary text text-metrics th-reify-many time tls transformers
    typed-process unicode-transforms unix unix-compat unliftio
    unordered-containers vector yaml zip-archive zlib
  ];
  doCheck = false;
  preCheck = "export HOME=$TMPDIR";
  postInstall = ''
    exe=$out/bin/stack
    mkdir -p $out/share/bash-completion/completions
    $exe --bash-completion-script $exe >$out/share/bash-completion/completions/stack
  '';
  homepage = "http://haskellstack.org";
  description = "The Haskell Tool Stack";
  license = stdenv.lib.licenses.bsd3;
}
