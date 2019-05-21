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
    khudyakovEnv
    # CLI utils
    ag
    crudini
    diffstat
    fortune
    ghostscript
    gnupg
    haskellPackages.git-annex
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
    pypi2nix
    haskellPackages.hasktags
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
    kdeApplications.konqueror
    libreoffice
    okular
    qpdfview
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
    profileExtra = ''
      if [ -e /home/alexey/.nix-profile/etc/profile.d/nix.sh ]; then
        . /home/alexey/.nix-profile/etc/profile.d/nix.sh;
      fi
      export NIX_PATH=$HOME/.nix-defexpr/channels:$NIX_PATH
      export NIX_PATH=ssh-auth-sock=$SSH_AUTH_SOCK:$NIX_PATH
      export NIX_PATH=ssh-config-file=/etc/ssh/ssh_config:$NIX_PATH
      '';
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
      bash ${cfg}/util/x-set-keyboard menu
      export GTK_IM_MODULE=xim
      export QT_IM_MODULE=xim
      export XCOMPOSEFILE=${cfg}/X/XCompose
      export XDG_CURRENT_DESKTOP=kde
      xset +fp ${pkgs.terminus_font}/share/fonts/terminus
      # Sessions
      systemctl --user start ssh-agent
      # Desktop stuff
      nm-applet          &
      xfce4-panel         &
      xfce4-power-manager &
      xfce4-volumed       &
      # Programs
      ${pkgs.lxpanel}/bin/lxpanel &
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
