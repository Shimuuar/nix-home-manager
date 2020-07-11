{ mkDerivation, fetchgit
, aeson, base, directory, filepath, hostname, containers
, optparse-applicative, process, stdenv, text, transformers
, unordered-containers, yaml
}:
mkDerivation {
  pname = "gittery";
  version = "0.1";
  src = fetchgit {
    url    = "git@bitbucket.org:Shimuuar/gittery.git";
    rev    = "5e23356110ddda5cf2dcfa1893a0ed27e4fdf4aa";
    sha256 = "1yv14nk0aingam0ba788kwqr7fma1zyp08x4py1zkf0mqmiyk315";
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
