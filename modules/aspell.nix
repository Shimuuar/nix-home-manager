{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.programs.aspell;

in
{
  options = {
    programs.aspell = {
      enable = mkOption {
        type = types.bool;
        default = false;
        defaultText = "false";
        description = ''
          Whether to install aspell
        '';
      };

      dicts  = mkOption {
        default = _: [];
        description = ''
          Which dictionaries should be installed
        '';      };

      config = mkOption {
        type = types.str;
        default = {};
        description = ''
          Configuration written to
          <filename>~/.aspell.conf</filename>
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.aspell ] ++ cfg.dicts pkgs.aspellDicts;
    home.file.".aspell.conf".text = cfg.config;
  };
}
