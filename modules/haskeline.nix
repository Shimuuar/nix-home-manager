{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.programs.screen;

in
{
  options = {
    programs.haskeline = {
      enable = mkOption {
        type = types.bool;
        default = false;
        defaultText = "false";
        description = ''
          Whether to enable haskeline config
        '';
      };

      config = mkOption {
        type = types.string;
        default = {};
        description = ''
          Configuration written to
          <filename>~/.haskeline</filename>
        '';
      };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ pkgs.screen ];
    home.file.".haskeline".text = config.programs.haskeline.config;
  };
}
