{ config, pkgs, ... }:
let
  # Repository with config files
  cfg = ../config;
  # Wrapped telegram. It doesn't like XDG_CURRENT_DESKTOP set
  telegram-wrapped = pkgs.stdenv.mkDerivation {
    name    = "telegram-desktop";
    builder = pkgs.writeScript "telegram-builder" ''
      ${pkgs.coreutils}/bin/mkdir -p $out/bin
      ${pkgs.coreutils}/bin/cat > $out/bin/telegram-wrapped <<EOF
      #!${pkgs.stdenv.shell}
      unset XDG_CURRENT_DESKTOP
      ${pkgs.tdesktop}/bin/telegram-desktop "\$@"
      EOF
      ${pkgs.coreutils}/bin/chmod +x $out/bin/telegram-wrapped
      '';
  };
  # Make chromium browser with custom config
  makeChromium = name: pkgs.writeShellScriptBin ("chromium-" + name) ''
    CFGDIR=''${XDG_CONFIG_HOME-''${HOME}/.config}
    exec ${pkgs.chromium}/bin/chromium --user-data-dir="$CFGDIR"/chromium-${name} "$@"
    '';
  chromium-temp = pkgs.writeScriptBin "chromium-temp" ''
    #!/bin/sh
    CFGDIR=$(${pkgs.coreutils}/bin/mktemp -d ''${XDG_RUNTIME_DIR-/tmp}/chromium-XXXXXXXX)
    CLEANUP="rm -rf $CFGDIR"
    trap "$CLEANUP" SIGINT SIGTERM
    ${pkgs.chromium}/bin/chromium --user-data-dir=$CFGDIR $@
    $CLEANUP
    '';
in
{
    home.packages = with pkgs; [
      # ----------------
      # Window magament
      dmenu
      xdotool
      wmctrl
      spectacle
      libnotify
      xclip
      # ----------------
      # GUI programs
      arandr
      chromium
      chromium-temp
      (makeChromium "ru")
      (makeChromium "alfa")
      (makeChromium "metamask")
      darktable
      djview
      deluge
      evince
      firefox
      inkscape
      krusader
      gimp
      nomacs
      gwenview
      libreoffice
      sonata
      kid3
      telegram-wrapped
      element-desktop
      thunderbird
      wireshark
      xournal
      xorg.xkill
      xorg.xdpyinfo
      xterm
      freecad
      # Cannot be built with python3.12
      #cura
      # ----------------
      # KDE
      breeze-icons
      hicolor-icon-theme
      kcharselect
      plasma5Packages.dolphin
      plasma5Packages.dolphin-plugins
      plasma5Packages.kate
      plasma5Packages.konsole
      plasma5Packages.okular
      plasma5Packages.systemsettings
      plasma5Packages.breeze-gtk
      plasma5Packages.breeze-qt5
      plasma5Packages.breeze-grub
      plasma5Packages.breeze-plymouth
      plasma5Packages.oxygen
      plasma5Packages.plasma-integration
      plasma5Packages.oxygen
      # ----------------
      # media
      feh
      mpv
      pavucontrol
      mpc-cli
      # ----------------
      # Fonts
      unifont
      terminus_font
      terminus_font_ttf
      # ----------------
      stalonetray
      xmobar
      networkmanagerapplet
      xfce.xfce4-power-manager
    ];
  # ----
  xdgUserDirs = {
    enable      = true;
    desktop     = "$HOME/Desktop";
    documents   = "$HOME/Desktop/Documents";
    download    = "$HOME/Desktop/Downloads";
    music       = "$HOME/Desktop/Music";
    pictures    = "$HOME/Desktop/Pictures";
    publicshare = "$HOME/Desktop/Public";
    templates   = "$HOME/Desktop/Templates";
    videos      = "$HOME/Desktop/Videos";
  };
  # GTK
  # FIXME: doesn't work due to dconf errors
#  gtk = {
#    enable = true;
#    iconTheme = {
#      name    = "Breeze-Dark";
#      package = pkgs.breeze-icons;
#    };
#    theme = {
#      name    = "Breeze-Dark";
#      package = pkgs.breeze-gtk;
#    };
#  };
  # Qt
  qt = {
    enable = true;
    style  = {
      name    = "Breeze-Dark";
      package = pkgs.breeze-qt5;
    };
  };
  # ----
  programs.lxpanel = {
    enable  = true;
    edge    = "top";
    plugins = [
      { type = "deskno"; }
      { type = "taskbar";
        config = {
          ShowAllDesks="0";
        };
      }
      (if config.extra-param.has-battery
       then
         { type   = "batt";
           config = { HideIfNoBattery = "1"; };
         }
       else
         null
      )
      { type = "cpufreq"; }
      { type = "thermal"; }
      { type = "tray";    }
      { type = "cpu";     }
      { type = "dclock";  }
    ];
  };
  # ----
  services.syncthing = {
    enable = true;
  };
  # ----
  services.gpg-agent = {
    enable = true;
  };
  # ----
  # Surprisingly nm-applet doesn't work without notification service
  services.dunst = {
    enable = true;
    settings = {
      global = {
        geometry           = "400x20-30+50";
        frame_color        = "#eceff1";
        font               = "Droid Sans 12";
        padding            = 2;
        horizontal_padding = 5;
        frame_width        = 1;
        separator_heights  = 4;
      };
      urgency_normal = {
        background = "#37474f";
        foreground = "#eceff1";
        timeout    = 10;
      };
    };
  };
  xsession = {
    enable    = true;
    # Gtk & Qt needed handholding to make use of XCompose and Qt5
    # specifically does need explicit instructions to find XCompose
    # file.
    initExtra = ''
      # X
      xrdb ${cfg}/X/Xresources
      bash ${cfg}/util/x-set-keyboard ${config.extra-param.composeKey}
      export GTK_IM_MODULE=xim
      export QT_IM_MODULE=xim
      export XCOMPOSEFILE=${cfg}/X/XCompose
      export XDG_CURRENT_DESKTOP=kde
      xset +fp ${pkgs.terminus_font}/share/fonts/terminus
      # Machine-specific programs
      lxpanel &
      ${config.extra-param.extraXSession}
      # Programs
      ${pkgs.firefox}/bin/firefox                                         &
      ${pkgs.thunderbird}/bin/thunderbird                                 &
      telegram-wrapped                                                    &
      element-desktop                                                     &
      ${pkgs.udiskie}/bin/udiskie --tray                                  &
      emacs --name emacs-todo --eval '(load-file "${cfg}/emacs-todo.el")' &
      nix-shell -p 'texlive.combine {inherit (texlive) scheme-medium;}'   \
        --run "emacs --name emacs-zettel" &
      if which spotify &> /dev/null; then
         (sleep 4 && spotify) &
      fi
      # Wallpapers
      while : ; do
        python ${cfg}/util/set-random-wallpaper
        sleep 1800
      done &
      '';
    windowManager.xmonad = {
      enable                 = true;
      enableContribAndExtras = true;
      config               = null; # Used for debugging configuration
      # config                 = "${cfg}/X/xmonad.hs";
    };
  };
}
