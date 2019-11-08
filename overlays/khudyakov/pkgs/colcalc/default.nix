{stdenv, fetchhg, python27, which}:
stdenv.mkDerivation {
  name = "colcalc";
  src  = fetchhg {
    url = "https://bitbucket.org/Shimuuar/colcalc";
    rev = "d1c38700c7f971e00f3a16b1f68f62b27a411404";
  };
  nativeBuildInputs = [ python27 which ];
  builder     = ./builder.sh;
}
