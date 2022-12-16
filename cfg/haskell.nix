{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    # ----------------
    # Haskell tools
    cabal-install
    cabal2nix
    (haskellPackages.ghcWithPackages(p: with p; [xmonad xmonad-contrib]))
    haskellPackages.ghc-prof-flamegraph
    haskellPackages.graphmod
    haskellPackages.hasktags
    haskellPackages.stack
    hlint
  ];
}
