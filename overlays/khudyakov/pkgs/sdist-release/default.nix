{ stdenv, fetchgit }:
stdenv.mkDerivation {
  name    = "sdist-release";
  version = 0.1;
  src     = fetchgit {
    url    = "https://github.com/Shimuuar/sdist-release.git";
    rev    = "2c1e6ea8fa4a5cddc307f72511f81084b97313a0";
    sha256 = "sha256:0h3ggbprwgyghcc2aypglnr6xcs00nk0m6nzvyk3hw0cq6jraqnk";
  };
  buildPhase = "";
  installPhase = ''
    mkdir -p $out/bin
    install sdist-release $out/bin
    '';
}
