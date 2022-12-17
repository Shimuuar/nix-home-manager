{ config, pkgs, ... }:
let
  # Repository with config files
  cfg = ../config;
in
{
  imports = [
    ./cfg/common.nix
    #
    ./cfg/cli.nix
    ./cfg/extra-cli.nix
    ./cfg/desktop.nix
    ./cfg/haskell.nix
    #./cfg/idea.nix
  ];
  home.stateVersion  = "22.11";
  home.username      = "alexey";
  home.homeDirectory = "/home/alexey";
  #
  fonts.fontconfig.enable = true;
  #
  extra-param.composeKey    = "ralt";
  extra-param.extraXSession = ''
    # Turn on num lock
    ${pkgs.numlockx}/bin/numlockx
    '';
  # ----------------------------------------------------------------
  # SSH tunnels
  systemd.user.services = {
    # Tunnel for deluge
    tunnel-deluge = {
      Unit.Description = "SSH tunnel for deluge";
      Install.WantedBy = [ "default.target" ];
      Service = {
        ExecStart  = "${pkgs.openssh}/bin/ssh -oBatchMode=yes -N -L 127.0.0.2:58846:127.0.0.1:58846 192.168.1.4";
        Restart    = "always";
        RestartSec = "30s";
      };
    };
    # Tunnel for syncthing API on dagda
    tunnel-syncthing = {
      Unit.Description = "SSH tunnel for web API on dagda";
      Install.WantedBy = [ "default.target" ];
      Service = {
        ExecStart  = "${pkgs.openssh}/bin/ssh -oBatchMode=yes -N -L 127.0.0.2:8384:127.0.0.1:8384 192.168.1.4";
        Restart    = "always";
        RestartSec = "30s";
      };
    };
  };
}
