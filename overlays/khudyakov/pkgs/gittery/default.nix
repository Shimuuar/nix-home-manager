{ mkDerivation, aeson, ansi-terminal, base, containers, directory
, filepath, hostname, lib, optparse-applicative, text, transformers
, typed-process, unordered-containers, yaml
}:
mkDerivation {
  pname = "gittery";
  version = "0.1";
  src = builtins.fetchGit {
    url    = "ssh://git@sepulcarium.org/home/git/GIT/private-projects/gittery";
    rev    = "f37e9c1a17af7e3ceba6a94dd2e229148189940c";
    ref    = "master";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson ansi-terminal base containers directory filepath hostname
    optparse-applicative text transformers typed-process
    unordered-containers yaml
  ];
  description = "Tool for managing multiple repositories";
  license = lib.licenses.bsd3;
  mainProgram = "gittery";
}
