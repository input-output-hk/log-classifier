# cabal2nix https://github.com/haskell/stm --revision "v2.4.5.1" > stm.nix
{ mkDerivation, array, base, fetchgit, stdenv }:
mkDerivation {
  pname = "stm";
  version = "2.4.5.1";
  src = fetchgit {
    url = "https://github.com/haskell/stm";
    sha256 = "1wvzc6yx4lnvv2nc9zn5q4g99h5cxpj5ywpks2blzbl6nysjzbkj";
    rev = "3722e83b92e947d6715153811f16c505c05a54f1";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ array base ];
  homepage = "https://wiki.haskell.org/Software_transactional_memory";
  description = "Software Transactional Memory";
  license = stdenv.lib.licenses.bsd3;
}
