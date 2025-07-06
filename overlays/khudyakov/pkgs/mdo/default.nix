{stdenv}:
stdenv.mkDerivation {
  name    = "mdo-0.1";
  src     = ../../../../config/util;
  builder = ./builder.sh;
}
