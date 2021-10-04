{ config, pkgs, ... }:
let
  # Repository with config files
  cfg = ../config;
in
{
  imports = [
    ./common.nix
    #
    ./haskell.nix
    ./cli.nix
  ];
  extra-param.extraBashProfile = ''
    . /Users/alexeykhudyakov/.nix-profile/etc/profile.d/nix.sh
    '';
}
