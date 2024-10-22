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
        ref = "jailbreak";
        rev = "c50c34890cecc1749f91f0a88f6f045529441d78";
      }) {})
  ];
}
