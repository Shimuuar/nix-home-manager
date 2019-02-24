{ config, pkgs, ... }:
let
  cfg = pkgs.fetchhg {
    url = "https://bitbucket.org/Shimuuar/config";
    rev = "90a2ebb4f090ea05dac276c990b0bda9e5a7f9fd";
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
    # General utils
    ag
    fortune
    haskellPackages.git-annex
    mercurial
    # Window magament
    dmenu
    lxpanel
    spectacle
    xorg.xev
    xorg.xmodmap
    xfontsel
    gnome3.adwaita-icon-theme
    # KDE
    kate
    konsole
    krusader
    okular
    # GUI programs   
    qpdfview
    tdesktop
    firefox
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
    initExtra = ''
      # X
      [ -L ~/.XCompose ] || ln -sf ${cfg}/X/XCompose ~/.XCompose
      xrdb ${cfg}/X/Xresources
      # Session programs
      eval $(ssh-agent)
      # Programs
      lxpanel &
      # Wallpapers
      while : ; do
        python ${cfg}/util/set-random-wallpaper
        sleep 1800
      done &
      '';
    windowManager.xmonad = {
      enable                 = true;
      enableContribAndExtras = true;
      config                 = "${cfg}/X/xmonad.hs";
    };
  };
}
