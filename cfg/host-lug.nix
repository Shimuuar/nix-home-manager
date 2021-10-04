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

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  #
  # FIXME
  fonts.fontconfig.enable = false;

  # ---- X session ----

  extra-param.composeKey = "menu";
  extra-param.extraXSession = ''
    # Programs
    xfce4-power-manager &
    '';
}
