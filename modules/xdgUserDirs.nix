{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.xdgUserDirs;

in
{
  options = {
    xdgUserDirs = {
      enable = mkEnableOption "Customization of XDG user directories";

      locale = mkOption {
        type = types.str;
        default = "en_US";
      };
      
      desktop = mkOption {
        type = types.str;
        default = "$HOME/Desktop";
      };
      documents = mkOption {
        type = types.str;
        default = "$HOME/Documents";
      };
      download = mkOption {
        type = types.str;
        default = "$HOME/Downloads";
      };
      music = mkOption {
        type = types.str;
        default = "$HOME/Music";
      };
      pictures = mkOption {
        type = types.str;
        default = "$HOME/Pictures";
      };
      publicshare = mkOption {
        type = types.str;
        default = "$HOME/Public";
      };
      templates = mkOption {
        type = types.str;
        default = "$HOME/Templates";
      };
      videos = mkOption {
        type = types.str;
        default = "$HOME/Videos";
      };
    };
  };

  config = mkIf cfg.enable {
    xdg.configFile."user-dirs.dirs".text = ''
      XDG_DESKTOP_DIR="${cfg.desktop}"
      XDG_DOCUMENTS_DIR="${cfg.documents}"
      XDG_DOWNLOAD_DIR="${cfg.download}"
      XDG_MUSIC_DIR="${cfg.music}"
      XDG_PICTURES_DIR="${cfg.pictures}"
      XDG_PUBLICSHARE_DIR="${cfg.publicshare}"
      XDG_TEMPLATES_DIR="${cfg.templates}"
      XDG_VIDEOS_DIR="${cfg.videos}"
      '';

    xdg.configFile."user-dirs.locale".text = cfg.locale;
  };
}
