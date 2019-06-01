{ stdenv, fetchurl }:

stdenv.mkDerivation {
  name = "polymode";
  version = "20190529.2050";
  src = fetchurl {
    url    = "https://melpa.org/packages/polymode-20190529.2050.tar";
    sha256 = "1wn330vl4v795j5ixiw9dy4makgrxfndm3abwbsijx2iww77fpd3";
  };
  buildInputs = [ stdenv ];
  buildPhase  = "";
  installPhase = ''
    mkdir $out
    cp -rv * $out
    '';
}
