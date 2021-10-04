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
  extra-param.composeKey    = "menu";
  extra-param.extraXSession = ''
    # Programs
    xfce4-power-manager &
    '';
}
