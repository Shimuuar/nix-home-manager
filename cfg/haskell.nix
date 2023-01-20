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
    # nix-overlay
    (haskellPackages.callCabal2nix "nix-pkgs-generator" (builtins.fetchGit
      { url = "https://github.com/Shimuuar/nix-pkgs-generator.git";
        ref = "cabalize";
        rev = "2a1e8dcc07c5b6b8c998e4da46e2c1b1642eb4f9";
      }) {})
  ];
}
