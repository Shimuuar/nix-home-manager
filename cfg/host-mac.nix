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
  extra-param.isMac = true;
  programs.bash.profileExtra = ''
    # Set up nix
    . /Users/alexeykhudyakov/.nix-profile/etc/profile.d/nix.sh
    # Set up locales
    export LANG=en_US.UTF-8
    export LC_ALL=en_US.UTF-8
    '';
}
