{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.programs.extra-param;
in
{
  options = {
    extra-param = {
      # fontconfig = mkOption {
      #   type        = types.bool;
      #   default     = false;
      #   defaultText = "false";
      #   description = ''
      #     Whether to install aspell
      #   '';
      # };

      # dicts  = mkOption {
      #   default = _: [];
      #   description = ''
      #     Which dictionaries should be installed
      #   '';      };

      composeKey = mkOption {
        type        = types.str;
        default     = "ralt";
        description = "Key used as X compose";
      };

      extraXSession = mkOption {
        type        = types.str;
        default     = "";
        description = "Extra bits to insert into .xsession file";
      };
      
      extraBashProfile = mkOption {
        type = types.str;
        default = "";
        description = ''
          Extra bits to add to bash profile
          '';
      };
    };
  };

  config = {};
}
