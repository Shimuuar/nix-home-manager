# with import <nixpkgs>{};
{ fetchFromGitHub, lib, buildGoPackage }:

buildGoPackage rec {
  name = "drone-cli-110";
  version = "1.1.0";
  revision = "v${version}";
  goPackagePath = "github.com/drone/drone-cli";

  goDeps= ./deps.nix;

  src = fetchFromGitHub {
    owner = "drone";
    repo = "drone-cli";
    rev = revision;
    sha256 = "1qpp7klh1dzicjbfvb2gkrnl001nr1lpqr8x0s9ybbhqgh3lrlak";
  };

  meta = with lib; {
    maintainers = with maintainers; [ bricewge ];
    license = licenses.asl20;
    description = "Command line client for the Drone continuous integration server.";
  };
}
