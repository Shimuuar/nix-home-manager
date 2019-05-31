args@{config, pkgs, ...}:
let
  params = {
    bash = {
      profileExtra = ''
        export EDITOR=nano

        if [ -e /home/alexey/.nix-profile/etc/profile.d/nix.sh ]; then
          . /home/alexey/.nix-profile/etc/profile.d/nix.sh;
        fi
        # See https://github.com/NixOS/nixpkgs/issues/38991
        export LOCALE_ARCHIVE_2_27=$(nix-build --no-out-link "<nixpkgs>" -A glibcLocales)/lib/locale/locale-archive
        #
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
    fontconfig = true;
  };
in import ./cfg params args
