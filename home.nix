{ config, pkgs, ... }:
let
  # Repository with config files
  cfg = pkgs.fetchhg {
    url = "https://bitbucket.org/Shimuuar/config";
    rev = "b401c40cd3d0e4b1efa142f2f193af9d7a857218";
  };
in
{
   imports = [
     ./modules/screen.nix
   ];

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  # Generic list of programs
  home.packages = with pkgs; [
    # CLI utils
    ag
    crudini
    diffstat
    fortune
    ghostscript
    gnupg
    haskellPackages.git-annex
    haskellPackages.hasktags
    imagemagick
    jq
    graphviz
    mercurial
    pdftk
    postgresql
    python36Packages.ipython
    rlwrap
    sqlite
    sshfs
    zip
    # Devtools
    binutils-unwrapped
    pypi2nix
    python27Packages.pip
    python36Packages.pip
    # Window magament
    dmenu
    lxpanel
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
    kdiff3
    kdeApplications.konqueror
    libreoffice
    okular
    qpdfview
    tdesktop
    thunderbird
    xournal
    # media
    feh
    mpv
    pavucontrol
  ];

  # ----
  programs.bash = {
    enable      = true;
    bashrcExtra = builtins.readFile "${cfg}/bashrc.sh";
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
      bash ${cfg}/util/x-set-keyboard ralt
      export GTK_IM_MODULE=xim
      export QT_IM_MODULE=xim
      export XCOMPOSEFILE=${cfg}/X/XCompose
      export XDG_CURRENT_DESKTOP=kde
      xset +fp ${pkgs.terminus_font}/share/fonts/terminus
      # Sessions
      systemctl --user start ssh-agent
      # Programs
      lxpanel &
      firefox &
      XDG_CURRENT_DESKTOP= telegram-desktop &
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
