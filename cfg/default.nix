params:
{ config, pkgs, ... }:
let
  # Repository with config files
  cfg = pkgs.fetchhg {
    url = "https://bitbucket.org/Shimuuar/config";
    rev = "0295f6eb89384a3680bf1e16a84dee697e4aac35";
  };
in
{
   imports = [
     ../modules/screen.nix
     ../modules/emacsPkg.nix
     ../modules/xdgUserDirs.nix
     ../modules/gdb.nix
     ../modules/haskeline.nix
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
    colcalc
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
    lz4
    jq
    mercurial
    p7zip
    pdfgrep
    pdftk
    postgresql_12
    python3Packages.ipython
    rlwrap
    sqlite
    sshfs
    sysstat
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
    gwenview
    libreoffice
    tdesktop
    thunderbird
    wireshark
    xournal
    xorg.xkill
    xterm
    # ----------------
    # KDE
    breeze-icons
    hicolor-icon-theme
    kcharselect
    kdeApplications.dolphin
    kdeApplications.dolphin-plugins
    kdeApplications.kate
    kdeApplications.konqueror
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
      merge             = { tool = "kdiff3"; };
      mergetool.kdiff3  = { path = "${pkgs.kdiff3}/bin/kdiff3"; };
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
      ${params.X.extraPrograms or ""}
      # Programs
      ${pkgs.firefox}/bin/firefox &
      ${pkgs.thunderbird}/bin/thunderbird &
      XDG_CURRENT_DESKTOP= ${pkgs.tdesktop}/bin/telegram-desktop &
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
