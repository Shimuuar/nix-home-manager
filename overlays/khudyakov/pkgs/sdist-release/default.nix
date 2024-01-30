{ stdenv, fetchgit }:
stdenv.mkDerivation {
  name    = "sdist-release";
  version = 0.1;
  src     = fetchgit {
    url    = "https://github.com/Shimuuar/sdist-release.git";
    rev    = "14d9fd9f8e8703acaf39a0a2076452fa0ae67fe3";
    sha256 = "sha256-zU+PJQ4v7O156zGw3VW0g2RIYQSKDB8pwOesFnFSvMQ=";
  };
  buildPhase = "";
  installPhase = ''
    mkdir -p $out/bin
    install sdist-release $out/bin
    '';
}
