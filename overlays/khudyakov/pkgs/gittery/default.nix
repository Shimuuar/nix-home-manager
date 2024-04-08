{ mkDerivation, aeson, ansi-terminal, base, containers, directory
, filepath, hostname, lib, optparse-applicative, text, transformers
, typed-process, unordered-containers, yaml
}:
mkDerivation {
  pname = "gittery";
  version = "0.1";
  src = builtins.fetchGit {
    url    = "ssh://git@sepulcarium.org/home/git/GIT/private-projects/gittery";
    rev    = "6d2f13bd406d882222d73dd11dfc982e01f58ca5";
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
