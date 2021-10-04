{ config, pkgs, ... }:
let
  # Wrapped telegram. It oesn;t like XDG_CURRENT_DESKTOP set
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
    # ----------------
    # GUI programs
    arandr
    chromium
    chromium-temp
    darktable
    djview
    deluge
    evince
    firefox
    krusader
    gimp
    nomacs
    gwenview
    libreoffice
    telegram-wrapped
    thunderbird
    wireshark
    xournal
    xorg.xkill
    xorg.xdpyinfo
    xterm
    freecad
    cura
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
    # ----------------
    # Fonts
    unifont
    terminus_font
    terminus_font_ttf
    # ----------------
    stalonetray
    xmobar
    gnome3.networkmanagerapplet
    xfce.xfce4-power-manager
    ];
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
      { type = "batt";
        config = {
          HideIfNoBattery = "1";
        };
      }
      { type = "cpufreq"; }
      { type = "thermal"; }
      { type = "tray";    }
      { type = "cpu";     }
      { type = "dclock";  }
    ];
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
}
