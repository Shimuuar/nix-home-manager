params:
{ config, pkgs, ... }:
let
  # Repository with config files
  cfg = pkgs.fetchhg {
    url = "https://bitbucket.org/Shimuuar/config";
    rev = "f78de939fa73a0b2f3862d414bdae79d22597e98";
  };
in
{
   imports = [
     ../modules/screen.nix
     ../modules/emacsPkg.nix
   ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  # Generic list of programs
  home.packages = with pkgs; [
    khudyakovEnv
    # CLI utils
    ag
    crudini
    diffstat
    fortune
    ghostscript
    gnupg
    graphviz
    haskellPackages.git-annex
    imagemagick
    jq
    mercurial
    pdftk
    postgresql
    python36Packages.ipython
    rlwrap
    sqlite
    sshfs
    zip
    # Devtools
    pypi2nix
    haskellPackages.hasktags
    # Window magament
    dmenu
    xdotool
    spectacle
    # GUI programs
    arandr
    chromium
    darktable
    deluge
    dolphin
    firefox
    gwenview
    evince
    gimp
    wireshark
    kate
    konsole
    kdeApplications.konqueror
    libreoffice
    okular
    tdesktop
    thunderbird
    xournal
    xterm
    # media
    feh
    mpv
    pavucontrol
    # Fonts
    unifont
    terminus_font
    terminus_font_ttf
  ];

  #
  fonts.fontconfig.enableProfileFonts = params.fontconfig or false;
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
      nix-mode
      julia-mode
      haskell-mode
      markdown-mode
      yaml-mode
      # Tools
      magit
    ];
  };
  programs.emacsPkg = {
    enable   = true;
    packages = [
      pkgs.emacsPkg.polymode
      pkgs.emacsPkg.tla-tools
    ];
  };
  # ----
  programs.git = {
    enable      = true;
    userName    = "Alexey Khudyakov";
    userEmail   = "alexey.skladnoy@gmail.com";
    package     = pkgs.gitAndTools.gitFull;
    extraConfig = ''
      [filter "nbstripout"]
        clean    = ${pkgs.nbstripout}/bin/nbstripout
        smudge   = cat
        required
      [merge]
        tool = kdiff3
      [mergetool "kdiff3"]
        path = ${pkgs.kdiff3}/bin/kdiff3
      '';
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
    color    =
    pager    =
    '';
  };

  # ---- X session ----
  xsession = {
    enable    = true;
    # Gtk & Qt needed handholding to make use of XCompose but Qt5 does
    # need explicit instructions to find XCompose file.
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
