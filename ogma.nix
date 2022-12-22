{ config, lib, pkgs, ... }:
let
  # Repository with config files
  cfg = ../config;
  # Function to make SSH tunnels
  makeTunnel = { magic, host, description, enable ? true }:
    { Unit.Description = description;
      Install.WantedBy = if enable then [ "default.target" ] else lib.mkForce [];
      Service = {
        ExecStart  = "${pkgs.openssh}/bin/ssh -oBatchMode=yes -N -L ${magic} ${host}";
        Restart    = "always";
        RestartSec = "30s";
      };
    };
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
    ${pkgs.sonata}/bin/sonata     &
    '';
  # ----------------------------------------------------------------
  # SSH tunnels
  systemd.user.services = {
    tunnel-deluge = makeTunnel {
      description = "SSH tunnel for deluge";
      magic       = "127.0.0.2:58846:127.0.0.1:58846";
      host        = "192.168.1.4";
    };
    tunnel-syncthing = makeTunnel {
      description = "SSH tunnel for syncthing's web API on dagda";
      magic       = "127.0.0.2:8384:127.0.0.1:8384";
      host        = "192.168.1.4";
    };
    tunnel-oka-wiki = makeTunnel {
      description = "SSH tunnel OKA wiki";
      magic       = "127.100.0.1:8080:istra.ihep.su:80";
      host        = "oka01.ihep.su";
      enable      = false;
    };
    tunnel-okavme = makeTunnel {
      description = "SSH tunnel for OKA slow control";
      magic       = "127.100.0.2:8080:okavme.ihep.su:80";
      host        = "oka01.ihep.su";
      enable      = false;
    };
  };
  # ----------------------------------------------------------------
  # Mount for SSHFS
  systemd.user.mounts = {
    "home-alexey-sshfs-dagda" = {
      Unit.Description = "SSHFS to dagda's nixos";
      Install.WantedBy = [ "default.target" ];
      Mount = {
        What    = "root@192.168.1.4:/etc/nixos";
        Where   = "/home/alexey/sshfs/dagda";
        Type    = "fuse.sshfs";
        Options = "_netdev,noauto,users,idmap=user,x-systemd-automount";
      };
    };
  };
  systemd.user.mounts = {
    "home-alexey-sshfs-aengus" = {
      Unit.Description = "SSHFS to dagda's nixos";
      Install.WantedBy = [ "default.target" ];
      Mount = {
        What    = "root@192.168.1.6:/etc/nixos";
        Where   = "/home/alexey/sshfs/aengus";
        Type    = "fuse.sshfs";
        Options = "_netdev,noauto,users,idmap=user,x-systemd-automount";
      };
    };
  };
}
