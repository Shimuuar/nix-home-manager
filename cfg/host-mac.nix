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
  programs.bash.profileExtra = ''
    . /Users/alexeykhudyakov/.nix-profile/etc/profile.d/nix.sh
    # We want to use proper bash and not one supplied by apple
    '';
}
