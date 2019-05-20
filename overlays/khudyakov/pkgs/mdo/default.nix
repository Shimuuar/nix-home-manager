{stdenv, fetchhg}:
stdenv.mkDerivation {
  name = "mdo-0.1";
  src  = fetchhg {
    url = "https://bitbucket.org/Shimuuar/config";
    rev = "b473cc39777675e45fc32f5b0f28ac99f6113c20";
  };
  builder = ./builder.sh;
}
