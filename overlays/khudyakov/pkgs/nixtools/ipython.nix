pkgs:
pkgs.writeScriptBin "ipython-with" ''
  #!${pkgs.stdenv.shell}
  EXPR="python3.withPackages(p: with p; [$@])"
  exec nix-shell -p "$EXPR" --run ipython
  ''
