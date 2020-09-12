{ config, lib, pkgs, ... }:

with lib;
let
  cfg   = config.programs.gdb;
in
{
  options = {
    programs.gdb = {
      enable = mkEnableOption "Extra gdb packages";
      gdbinit = mkOption {
        type        = types.str;
        default     = "";
        description = ".gdbinit";
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.gdb ];
    home.file.".gdbinit".text = config.programs.gdb.gdbinit;
  };
}
