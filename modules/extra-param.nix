{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.programs.extra-param;
in
{
  options = {
    extra-param = {
      has-battery = mkOption {
        type        = types.bool;
        default     = false;
        description = "Whether computer has battery (used for GUI)";
      };

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

      isMac = mkOption {
        type        = types.bool;
        default     = false;
        description = "Whether we're working on mac";
      };
    };
  };

  config = {};
}
