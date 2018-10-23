{ stdenv, fetchhg, root, boost, bison, flex }:
stdenv.mkDerivation {
  name    = "root-plot";
  version = 0.1;
  src = fetchhg {
    url = "https://bitbucket.org/Shimuuar/root-plot";
    rev = "52c28721e762ff04abc448c10baf9e9b0120fa57";
  };
  nativeBuildInputs = [ bison flex ];
  buildInputs       = [ stdenv root boost ];
}
