{ config, pkgs, ... }:
let
  cfg = pkgs.fetchhg {
    url = "https://bitbucket.org/Shimuuar/config";
    rev = "b440a34c55ebf99d549e5c5705b28e04e8f869ad";
  };
in
{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
  # Generic list of programs  
  home.packages = with pkgs; [
    # General utils
    fortune
    ag
    mercurial
    # KDE
    okular
    konsole
    #
    firefox
    # media
    mpv
    feh
    pavucontrol
  ];
  
  # ----
  programs.bash = {
    enable      = true;
    bashrcExtra = builtins.readFile "${cfg}/bashrc.sh";
  };
  # ----
  programs.emacs = {
    enable = true;
    extraPackages = epkg : with epkg; [
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

}
