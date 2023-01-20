{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.programs.lxpanel;
  renderConfigLine= k: v: "  ${k} = ${v}";
  renderConfig = c: ''
      Config {
        ${builtins.concatStringsSep "\n" (builtins.map (k: renderConfigLine k c."${k}") (builtins.attrNames c))}
      }
    '';
  renderPlugin = plug: ''
    Plugin {
      type = ${plug.type}
      ${renderConfig plug.config or {}}
    }
    '';
in
{
  options.programs.lxpanel = {
    enable = mkOption {
      type = types.bool;
      default = false;
      defaultText = "false";
      description = ''
        Whether to install lxpanel
        '';
      };

    edge = mkOption {
      type        = types.enum ["top" "bottom" "left" "right"];
      default     = "bottom";
      description = "Position of panel";
    };

    plugins = mkOption {
      default = [];
    };
  };
  ## ----------------
  config = mkIf cfg.enable {
    home.packages = [ pkgs.lxpanel ];

    xdg.configFile."lxpanel/default/config".text = ''
      [Command]
      '';
    xdg.configFile."lxpanel/default/panels/panel".text = ''
      Global {
        edge = ${cfg.edge}
      }
      ${builtins.concatStringsSep "\n" (builtins.map renderPlugin (builtins.filter (x: (x!=null)) cfg.plugins))}
      '';
  };

}
