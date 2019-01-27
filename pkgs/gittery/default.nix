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
    rev    = "c8ad05602a371a46029947b0ee1e242b81ec2b3f";
    sha256 = "06r5mp1lfap1czlay597f95ghiyk5czrb6gkz3i3m4yxvkkcfnda";
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
