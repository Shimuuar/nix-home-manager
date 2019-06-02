


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
        export LOCALE_ARCHIVE_2_27=${pkgs.glibcLocales}/lib/locale/locale-archive
        #
        export NIX_PATH=$HOME/.nix-defexpr/channels:$NIX_PATH
        export NIX_PATH=ssh-auth-sock=$SSH_AUTH_SOCK:$NIX_PATH
        export NIX_PATH=ssh-config-file=/etc/ssh/ssh_config:$NIX_PATH

        # Set up GLX for nix
        export LIBGL_DRIVERS_PATH=/usr/lib/x86_64-linux-gnu/dri/
        export LD_LIBRARY_PATH=$XDG_RUNTIME_DIR/GLX
        env | grep LD
        if [ ! -d $XDG_RUNTIME_DIR/GLX ]; then
          mkdir $XDG_RUNTIME_DIR/GLX
          ${pkgs.bash}/bin/bash ${pkgs.writeTextFile {
            name = "lug-glx.sh";
            text = builtins.readFile ./lug-glx.sh;
          }}
        fi
        '';
    };
    X = {
      composeKey = "menu";
      extraPrograms = ''
        # Somehow LD_LIBRARY_PATH gets unset
        export LD_LIBRARY_PATH=$XDG_RUNTIME_DIR/GLX
        # Programs
        nm-applet           &
        xfce4-panel         &
        xfce4-power-manager &
        xfce4-volumed       &
        '';
    };
    fontconfig = true;
  };
in import ./cfg params args
