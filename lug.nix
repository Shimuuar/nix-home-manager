args@{config, pkgs, ...}:
let
  params = {
    bash = {
      profileExtra = ''
        if [ -e /home/alexey/.nix-profile/etc/profile.d/nix.sh ]; then
          . /home/alexey/.nix-profile/etc/profile.d/nix.sh;
        fi
        export NIX_PATH=$HOME/.nix-defexpr/channels:$NIX_PATH
        export NIX_PATH=ssh-auth-sock=$SSH_AUTH_SOCK:$NIX_PATH
        export NIX_PATH=ssh-config-file=/etc/ssh/ssh_config:$NIX_PATH
        '';
    };
    X = {
      composeKey = "menu";
      extraPrograms = ''
        nm-applet           &
        xfce4-panel         &
        xfce4-power-manager &
        xfce4-volumed       &
        '';
    };
  };
in import ./cfg params args
