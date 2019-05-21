params:
{ config, pkgs, ... }:
let
  # Repository with config files
  cfg = pkgs.fetchhg {
    url = "https://bitbucket.org/Shimuuar/config";
    rev = "0953aa0974a350664070084330c2f06b992243f8";
  };
in
{
   imports = [
     ../modules/screen.nix
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
  ];

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
      nix-mode
      julia-mode
      haskell-mode
      markdown-mode
      yaml-mode
    ];
  };
  # ----
  programs.git = {
    enable      = true;
    userName    = "Alexey Khudyakov";
    userEmail   = "alexey.skladnoy@gmail.com";
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