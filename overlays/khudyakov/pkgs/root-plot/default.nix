{ stdenv, fetchgit, root, boost, bison, flex }:
stdenv.mkDerivation {
  name    = "root-plot";
  version = 0.1;
  src = builtins.fetchGit {
    url = "https://Shimuuar@bitbucket.org/Shimuuar/root-plot.git";
    rev = "ea425d1e3f5764b77f5615f03b01c95be0873fca";
    ref = "master";
  };
  nativeBuildInputs = [ bison flex ];
  buildInputs       = [ stdenv root boost ];
  makeFlags         = [ "INSTALLDIR=$(out)" "-j4" ];
  postInstall       = ./root-wrapper.sh;
}
