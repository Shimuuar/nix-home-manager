{ config, pkgs, ... }:
let
  # Repository with config files
  cfg = ../config;
in
{
  imports = [
    ./cfg/common.nix
    ./cfg/cli.nix
    ./cfg/extra-cli.nix
    #
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
  extra-param = {
    has-battery = true;
    composeKey  = "menu";
    extraXSession = ''
      # Programs
      xfce4-power-manager &
      '';
  };
}
