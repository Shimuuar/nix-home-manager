{ config, lib, pkgs, ... }:

with lib;
let
  cfg   = config.programs.emacsPkg;
  mkPkg = p: {
    name  = ".emacs.d/lisp/${p.name}";
    value = {source = "${p}";};
  };
in
{
  options = {
    programs.emacsPkg = {
      enable = mkEnableOption "Extra emacs packages";

      packages = mkOption {
        default = [];
        description = "List of packages to be placed to into ~/.emacs.d/lisp";
      };
    };
  };

  config = mkIf cfg.enable {
    home.file = builtins.listToAttrs (builtins.map mkPkg cfg.packages);
  };
}
