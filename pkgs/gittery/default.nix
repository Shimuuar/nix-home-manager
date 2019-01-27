{ mkDerivation, fetchgitPrivate
, aeson, base, directory, filepath, hostname
, optparse-applicative, process, stdenv, text, transformers
, unordered-containers, yaml
}:
mkDerivation {
  pname = "gittery";
  version = "0.1";
  src = fetchgitPrivate {
    url    = "git@bitbucket.org:Shimuuar/gittery.git";
    rev    = "94a22db3f1141e4be1d3f465b9106e1da9e05004";
    sha256 = "0iyhgg1mhhar6l9fy2c59cgzyla6aayn2vl97yin3wiw28i683jf";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base directory filepath hostname optparse-applicative process
    text transformers unordered-containers yaml
  ];
  description = "Tool for managing multiple repositories";
  license = stdenv.lib.licenses.bsd3;
}
