{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.programs.screen2;
in
{
  options = {
    programs.screen2 = {
      enable = mkOption {
        type = types.bool;
        default = false;
        defaultText = "false";
        description = ''
          Whether to enable GNU screen
        '';
      };

      settings = mkOption {
        type = types.str;
        default = {};
        description = ''
          Configuration written to
          <filename>~/.screenrc</filename>
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.screen ];
    home.file.".screenrc".text = config.programs.screen2.settings;
  };
}
