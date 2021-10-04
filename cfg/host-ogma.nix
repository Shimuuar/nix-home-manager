{ config, pkgs, ... }:
let
  # Repository with config files
  cfg = ../config;
in
{
  imports = [
    ./common.nix
    #
    ./desktop.nix
    ./haskell.nix
    ./idea.nix
    ./cli.nix
    ./extra-cli.nix
  ];
  #
  fonts.fontconfig.enable = true;
  #
  extra-param.composeKey    = "ralt";
  extra-param.extraXSession = ''
    # Turn on num lock
    ${pkgs.numlockx}/bin/numlockx
    # Run tunnel to dagda (Deluge)
    while : ; do
      ssh -oBatchMode=yes -N -L 127.0.0.2:58846:127.0.0.1:58846 192.168.1.4
      sleep 30
    done &
    # Run tunnel to dagda (syncthing)
    while : ; do
      ssh -oBatchMode=yes -N -L 127.0.0.2:8384:127.0.0.1:8384 192.168.1.4
      sleep 30
    done &
    '';
}
