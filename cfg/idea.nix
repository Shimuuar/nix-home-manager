{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    idea.idea-community
  ];
}
