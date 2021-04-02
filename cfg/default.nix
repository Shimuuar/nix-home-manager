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
    gimp
    nomacs
    gwenview
    libreoffice
    telegram-wrapped
    thunderbird
    wireshark
    xournal
    xorg.xkill
    xterm
    freecad
    cura
    # ----------------
    # KDE
    breeze-icons
    hicolor-icon-theme
    kcharselect
    kdeApplications.dolphin
    kdeApplications.dolphin-plugins
    kdeApplications.kate
#    kdeApplications.konqueror
    kdeApplications.konsole
    kdeApplications.okular
    plasma5.systemsettings
    plasma5.breeze-gtk
    plasma5.breeze-qt5
    plasma5.breeze-grub
    plasma5.breeze-plymouth
    plasma5.oxygen
    plasma5.plasma-integration
    plasma5.oxygen
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
      # Tools
      magit
    ];
  };
  programs.emacsPkg = {
    enable   = true;
    packages = [
    ];
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
  # Surprisingly nm-apple doesn't work without notification service
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
