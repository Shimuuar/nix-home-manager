{ mkDerivation, fetchgitPrivate, async, base, bytestring, directory
, http-media, network, optparse-applicative, servant
, servant-server, servant-websockets, stdenv, stm, wai, warp
, websockets
}:
mkDerivation {
  pname = "plotly-server";
  version = "0.1";
  src = fetchgitPrivate {
    url    = "git@bitbucket.org:Shimuuar/plotly-server.git";
    rev    = "03c18bd01271613c059183f9a1625041159a359e";
    sha256 = "1yysxh4avckm9hkx7n1swsqb7ilzrc9623sbxcjbgz83zgykx5sk";
  };
  isLibrary = false;
  isExecutable = true;
  enableSeparateDataOutput = true;
  executableHaskellDepends = [
    async base bytestring directory http-media network
    optparse-applicative servant servant-server servant-websockets stm
    wai warp websockets
  ];
  homepage = "http://bitbucket.org/Shimuuar/plotly-server";
  description = "Server for drawing and updating plotly plots in browser";
  license = stdenv.lib.licenses.bsd3;
}
