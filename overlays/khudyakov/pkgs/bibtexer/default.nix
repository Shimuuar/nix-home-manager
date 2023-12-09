{ mkDerivation, base, bibtex, directory, filepath, lib
, optparse-applicative, parsec, text, typed-process, terminfo
}:
mkDerivation {
  pname = "bibtexer";
  version = "0.1";
  src = builtins.fetchGit {
    url = "ssh://git@sepulcarium.org/home/git/GIT/private-projects/bibtexer/";
    rev = "9919f35096e9f2ef886711ed356e70632c8db86f";
    ref = "master";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bibtex directory filepath optparse-applicative parsec text
    typed-process terminfo
  ];
  description = "Small utility keeping bibligrpahy and org-roam in sync";
  license = lib.licenses.bsd3;
  mainProgram = "bibtexer";
}
