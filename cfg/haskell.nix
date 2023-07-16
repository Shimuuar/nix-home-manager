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
        ref = "split-config";
        rev = "fb758f409cd4c6815dbcafdbda967b41ce2e73dd";
      }) {})
  ];
}
