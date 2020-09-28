{ mkDerivation, base, process, stdenv, tagsoup }:
mkDerivation {
  pname   = "arxiv-get";
  version = "0.1";
  src = builtins.fetchGit {
    url = "https://github.com/Shimuuar/arxiv-get.git";
    rev = "e379a18e25c675fef20494539a530041076804c6";
    ref = "master";
  };
  isLibrary    = false;
  isExecutable = true;
  executableHaskellDepends = [ base process tagsoup ];
  description = "Small utility to download papers from arXiv.org and give them meaningful names";
  license = stdenv.lib.licenses.bsd3;
}
