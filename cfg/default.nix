params:
{ config, pkgs, ... }:
let
  # Repository with config files
  cfg = ../config;
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
   imports = [
     ../modules/screen.nix
     ../modules/emacsPkg.nix
     ../modules/xdgUserDirs.nix
     ../modules/gdb.nix
     ../modules/haskeline.nix
     ../modules/aspell.nix
     ../modules/lxpanel.nix
     ./desktop.nix
   ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  # Generic list of programs
  home.packages = with pkgs; [
    # ----------------
    # My utils
    arxiv-get
    gittery
    mdo
    # colcalc
    root-plot
    # ----------------
    # CLI utils
    ag
    crudini
    convmv
    diffstat
    enca
    fortune
    ghostscript
    gnupg
    graphviz
    haskellPackages.git-annex
    hledger
    hledger-ui
    htop
    imagemagick
    inetutils
    mc
    lz4
    jq
    mercurial
    p7zip
    pdfgrep
    pdftk
    postgresql_12
    python3Packages.ipython
    rlwrap
    rustup
    sqlite-interactive
    sshfs
    pwntools
    sysstat
    usbutils
    youtube-dl
    zip
    # ----------------
    # Devtools
    pypi2nix
    rr
    gcc
    # ----------------
    # Haskell tools
    cabal-install
    cabal2nix
    ghc
    haskellPackages.ghc-prof-flamegraph
    haskellPackages.graphmod
    haskellPackages.hasktags
    haskellPackages.hasktags
    haskellPackages.stack
    haskellPackages.weeder
    hlint
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
    idea.idea-community
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
  #
  fonts.fontconfig.enable = params.fontconfig or false;
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
  programs.aspell = {
    enable = true;
    dicts  = a: with a; [ en ru ];
    # See discussion: https://github.com/NixOS/nixpkgs/issues/1000
    config = "data-dir /home/alexey/.nix-profile/lib/aspell";
  };
  # ----
  programs.bash = {
    enable       = true;
    bashrcExtra  = builtins.readFile "${cfg}/bashrc.sh";
    profileExtra = params.bash.profileExtra or "";
  };
  # ----
  programs.screen = {
    enable   = true;
    settings = builtins.readFile "${cfg}/screenrc";
  };
  # ----
  programs.emacs = {
    enable = true;
    extraPackages = epkg : with epkg; [
      undo-tree
      browse-kill-ring
      # Modes
      go-mode
      rust-mode
      haskell-mode
      hledger-mode
      julia-mode
      kotlin-mode
      markdown-mode
      nix-mode
      scala-mode
      yaml-mode
      toml-mode
      # Tools
      org-roam
      org-roam-bibtex
      org-ref
      helm
      magit
      deft
    ];
  };
  programs.emacsPkg = {
    enable   = true;
    packages = [
    ];
  };
  # ----
  programs.ssh = {
    enable = true;
    matchBlocks = {
      "oka04.ihep.su" = {
        user         = "khudyakov";
        proxyJump    = "istrad.ihep.su";
        extraOptions = {
          "KexAlgorithms" = "+diffie-hellman-group1-sha1";
        };
      };
      "oka01.ihep.su" = {
        user         = "khudyakov";
        proxyJump    = "sepulcarium.org";
      };
      "oka02.ihep.su" = {
        user         = "khudyakov";
        proxyJump    = "sepulcarium.org";
      };
      "oka03.ihep.su" = {
        user         = "khudyakov";
        proxyJump    = "sepulcarium.org";
      };
    };
  };
  # ----
  programs.git = {
    enable      = true;
    userName    = "Alexey Khudyakov";
    userEmail   = "alexey.skladnoy@gmail.com";
    package     = pkgs.gitAndTools.gitFull;
    extraConfig = {
      core = { quotePath = false; };
      pull = { ff = "only"; };
      #
      merge             = { tool = "kdiff3"; };
      mergetool.kdiff3  = { path = "${pkgs.kdiff3}/bin/kdiff3"; };
      mergetool.meld    = { path = "${pkgs.meld}/bin/meld"; };
      init.defaultBranch = "master";
      #
      filter.nbstripout = {
        clean    = "${pkgs.nbstripout}/bin/nbstripout";
        smudge   = "cat";
        required = true;
      };
    };
  };
  # ----
  programs.mercurial = {
    enable      = true;
    userName    = "Alexey Khudyakov";
    userEmail   = "alexey.skladnoy@gmail.com";
    extraConfig = ''
    [extensions]
    record   =
    hgk      =
    hggit    =
    color    =
    pager    =
    '';
  };
  # ----
  programs.haskeline = {
    enable = true;
    config = ''
      historyDuplicates: IgnoreAll
      '';
  };
  # ----
  programs.gdb = {
    enable  = true;
    gdbinit = ''
      set auto-load safe-path /
      set disassembly-flavor intel

      define ghcR1
        printf "R1: 0x%016lx\n", $rbx
        x *$rbx
      end
      define ghcR2
        printf "R2: 0x%016lx\n", $r14
        x *$r14
      end
      define ghcR3
        printf "R3: 0x%016lx\n", $rsi
        x *$rsi
      end
      define ghcR4
        printf "R4: 0x%016lx\n", $rdi
        x *$rdi
      end
      define ghcR5
        printf "R5: 0x%016lx\n", $r8
        x *$r8
      end
      define ghcR6
        printf "R6: 0x%016lx\n", $r9
        x *$r9
      end
      '';
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
  # ---- X session ----
  xsession = {
    enable    = true;
    # Gtk & Qt needed handholding to make use of XCompose and Qt5
    # specifically does need explicit instructions to find XCompose
    # file.
    initExtra = ''
      # X
      xrdb ${cfg}/X/Xresources
      bash ${cfg}/util/x-set-keyboard ${params.X.composeKey}
      export GTK_IM_MODULE=xim
      export QT_IM_MODULE=xim
      export XCOMPOSEFILE=${cfg}/X/XCompose
      export XDG_CURRENT_DESKTOP=kde
      xset +fp ${pkgs.terminus_font}/share/fonts/terminus
      # Machine-specific programs
      lxpanel &
      ${params.X.extraPrograms or ""}
      # Programs
      ${pkgs.firefox}/bin/firefox                                         &
      ${pkgs.thunderbird}/bin/thunderbird                                 &
      ${telegram-wrapped}/bin/telegram-wrapped                            &
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
      # config               = null; # Used for debugging configuration
      config                 = "${cfg}/X/xmonad.hs";
    };
  };
}
