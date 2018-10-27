{ mkDerivation, fetchhg, base, process, stdenv, tagsoup }:
mkDerivation {
  pname   = "arxiv-get";
  version = "0.1";
  src = fetchhg {
    url    = "https://bitbucket.org/Shimuuar/arxiv-get";
    rev    = "6769ecb3c4e2c0a3413e056e40912f532bbd8c6b";
    sha256 = "1fpqgqwa0aj6bqvbb4ia30fjvmkmqpybj541542brqzd3ijna76c";
  };
  isLibrary    = false;
  isExecutable = true;
  executableHaskellDepends = [ base process tagsoup ];
  description = "Small utility to download papers from arXiv.org and give them meaningful names";
  license = stdenv.lib.licenses.bsd3;
}
