{ mkDerivation, fetchgitPrivate
, aeson, base, directory, filepath, hostname, containers
, optparse-applicative, process, stdenv, text, transformers
, unordered-containers, yaml
}:
mkDerivation {
  pname = "gittery";
  version = "0.1";
  src = fetchgitPrivate {
    url    = "git@bitbucket.org:Shimuuar/gittery.git";
    rev    = "ed2d25303d06b1c59a774eee4e777f4e7852e62d";
    sha256 = "1hxl5sf6za1h69c6vcyhnif8yl5llv7r0wn5m9a66hg6s9kg877z";
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
