{stdenv}:
stdenv.mkDerivation {
  name = "mdo-0.1";
  src  = builtins.fetchGit {
    url = "https://github.com/Shimuuar/config.git";
    ref = "master";
    rev = "ec750c063ccaac7a1a0d4a7bc88af44cd8ab0986";
  };
  builder = ./builder.sh;
}
