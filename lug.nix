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
        xmobar      &
        stalonetray &
        '';
    };
    fontconfig = true;
  };
in import ./cfg params args
