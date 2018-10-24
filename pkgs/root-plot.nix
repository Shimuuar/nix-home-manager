{ stdenv, fetchhg, root, boost, bison, flex }:
stdenv.mkDerivation {
  name    = "root-plot";
  version = 0.1;
  src = fetchhg {
    url = "https://bitbucket.org/Shimuuar/root-plot";
    rev = "608b3ad9cbd82bc267f9ef1497f8480c76c763e0";
  };
  nativeBuildInputs = [ bison flex ];
  buildInputs       = [ stdenv root boost ];
  makeFlags         = [ "INSTALLDIR=$(out)" ];
}
