{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    # ----------------
    # My utils
    arxiv-get
    gittery
    mdo
    # colcalc
    root-plot
    # CLI
    haskellPackages.git-annex
    hledger
    hledger-ui
  ];
}
