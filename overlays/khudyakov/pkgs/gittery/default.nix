{ mkDerivation
, aeson, base, directory, filepath, hostname, containers
, optparse-applicative, process, stdenv, text, transformers
, unordered-containers, yaml
}:
mkDerivation {
  pname = "gittery";
  version = "0.1";
  src = builtins.fetchGit {
    url    = "git@bitbucket.org:Shimuuar/gittery.git";
    rev    = "5e23356110ddda5cf2dcfa1893a0ed27e4fdf4aa";
    ref    = "master";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base directory filepath hostname optparse-applicative process
    text transformers unordered-containers yaml containers
  ];
  description = "Tool for managing multiple repositories";
  license = stdenv.lib.licenses.bsd3;
}
