args@{config, pkgs, ...}:
let
  params = {
    bash = {
      profileExtra = ''
        export EDITOR=nano
        '';
    };
    X = {
      composeKey    = "menu";
      extraPrograms = ''
        # Programs
        xfce4-power-manager &
        '';
    };
    fontconfig = true;
  };
in import ./cfg params args
