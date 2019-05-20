{ stdenv, fetchhg, root, boost, bison, flex }:
stdenv.mkDerivation {
  name    = "root-plot";
  version = 0.1;
  src = fetchhg {
    url = "https://bitbucket.org/Shimuuar/root-plot";
    rev = "9333e13c4e7cf3ece65675ecf75c46033bfd3b62";
  };
  nativeBuildInputs = [ bison flex ];
  buildInputs       = [ stdenv root boost ];
  makeFlags         = [ "INSTALLDIR=$(out)" "-j4" ];
  postInstall       = ./root-wrapper.sh;
}
