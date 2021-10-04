{ config, pkgs, ... }:
{
  home.packages = with pkgs; [
    # ----------------
    # Haskell tools
    cabal-install
    cabal2nix
    ghc
    haskellPackages.ghc-prof-flamegraph
    haskellPackages.graphmod
    haskellPackages.hasktags
    haskellPackages.stack
    hlint
  ];
}
