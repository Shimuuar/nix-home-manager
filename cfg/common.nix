{ config, pkgs, ... }:
{
  imports = [
    ../modules/screen.nix
    ../modules/emacsPkg.nix
    ../modules/xdgUserDirs.nix
    ../modules/gdb.nix
    ../modules/haskeline.nix
    ../modules/aspell.nix
    ../modules/lxpanel.nix
    ../modules/extra-param.nix
  ];
}
