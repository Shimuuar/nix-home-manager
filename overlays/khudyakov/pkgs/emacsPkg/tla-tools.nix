{ stdenv, fetchgit }:

stdenv.mkDerivation {
  name    = "tla-tools";
  version = "20190529.2050";
  src = fetchgit {
    url = "https://github.com/mrc/tla-tools.git";
    rev    = "7942288d707ed52ae911d1387414d04e8f83a43a";
    sha256 = "sha256:1cadv0hl9gnwqx0j1lc11hg4qyncw5ldl9sf52fp5asqb5kgfzls";
  };
  buildInputs = [ stdenv ];
  buildPhase  = "";
  installPhase = ''
    mkdir $out
    cp -rv * $out
    '';
}
