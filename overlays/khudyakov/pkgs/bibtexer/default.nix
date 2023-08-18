{ mkDerivation, base, bibtex, directory, filepath, lib
, optparse-applicative, parsec, text, typed-process
}:
mkDerivation {
  pname = "bibtexer";
  version = "0.1";
  src = builtins.fetchGit {
    url = "ssh://git@sepulcarium.org/home/git/GIT/private-projects/bibtexer/";
    rev = "a0e239543c0e1bb626a17796ae5179ca2dec6429";
    ref = "master";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bibtex directory filepath optparse-applicative parsec text
    typed-process
  ];
  description = "Small utility keeping bibligrpahy and org-roam in sync";
  license = lib.licenses.bsd3;
  mainProgram = "bibtexer";
}
