pkgs:
pkgs.writeScriptBin "ghc-bash" ''
  #!${pkgs.stdenv.shell}
  EXPR="haskellPackages.ghcWithPackages(p: with p; [$@])"
  exec nix-shell -p "$EXPR"
  ''
