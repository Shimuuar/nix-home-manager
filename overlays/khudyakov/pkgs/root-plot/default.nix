{ stdenv, fetchgit, root, boost, bison, flex }:
stdenv.mkDerivation {
  name    = "root-plot";
  version = 0.1;
  src = fetchgit {
    url    = "https://Shimuuar@bitbucket.org/Shimuuar/root-plot.git";
    rev    = "38299765689e0217fd4c3bab17b832f992c47e5c";
    sha256 = "183kf6gvpng6gb8kkfnhscr7vv04wb2wxif7vgjsv4y2qy55kdyi";
  };
  nativeBuildInputs = [ bison flex ];
  buildInputs       = [ stdenv root boost ];
  makeFlags         = [ "INSTALLDIR=$(out)" "-j4" ];
  postInstall       = ./root-wrapper.sh;
}
