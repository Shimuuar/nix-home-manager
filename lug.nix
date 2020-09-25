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
        xmobar ${./config/xmobarrc}              &
        stalonetray -c ${./config/stalonetrayrc} &
        xfce4-power-manager
        '';
    };
    fontconfig = true;
  };
in import ./cfg params args
