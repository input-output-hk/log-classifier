{ mkDerivation, ansi-terminal, async, base, directory, exceptions
, fetchgit, process, stdenv, stm, terminal-size, text, transformers
, unix
}:
mkDerivation {
  pname = "concurrent-output";
  version = "1.10.7";
  src = fetchgit {
    url = "git://git.joeyh.name/concurrent-output.git";
    sha256 = "05i723i6cw98qfwqsz6d85s9ybibnlvzn6jym3q4pwkx9aj1dzlz";
    rev = "65ceb9bcbc600f1207ba309458fd31ed96642994";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    ansi-terminal async base directory exceptions process stm
    terminal-size text transformers unix
  ];
  description = "Ungarble output from several threads or commands";
  license = stdenv.lib.licenses.bsd2;
}
