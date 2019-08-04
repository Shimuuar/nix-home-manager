args@{config, pkgs, ...}:
let
  params = {
    X = {
      composeKey = "ralt";
      extraPrograms = ''
        systemctl --user start ssh-agent
        ${pkgs.lxpanel}/bin/lxpanel &
        '';
    };
  };
in import ./cfg params args

      


