{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.programs.extra-param;
in
{
  options = {
    extra-param = {
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
      
    };
  };

  config = {};
}
