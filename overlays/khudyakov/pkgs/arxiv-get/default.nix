{ mkDerivation, base, process, lib, tagsoup }:
mkDerivation {
  pname   = "arxiv-get";
  version = "0.1";
  src = builtins.fetchGit {
    url = "https://github.com/Shimuuar/arxiv-get.git";
    rev = "e3e25507edeaf99576b0519ac385afe28ae026df";
    ref = "master";
  };
  isLibrary    = false;
  isExecutable = true;
  executableHaskellDepends = [ base process tagsoup ];
  description = "Small utility to download papers from arXiv.org and give them meaningful names";
  license = lib.licenses.bsd3;
}
