{ stdenv, fetchurl }:

stdenv.mkDerivation {
  name = "polymode";
  version = "20190529.2050";
  src = fetchurl {
    url    = "https://melpa.org/packages/polymode-20190529.2050.tar";
    sha256 = "0qhbrm38v0b6ff0cr0wxdc7jg1c7jyh0cs570d31j3mbgy704w57";
  };
  buildInputs = [ stdenv ];
  buildPhase  = "";
  installPhase = ''
    mkdir $out
    cp -rv * $out
    '';
}
