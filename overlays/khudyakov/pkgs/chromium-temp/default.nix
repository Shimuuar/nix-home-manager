pkgs:
pkgs.writeScriptBin "chromium-temp" ''
  #!/bin/sh

  CFGDIR=$(mktemp -d ''${XDG_RUNTIME_DIR-/tmp}/chromium-XXXXXXXX)
  CLEANUP="rm -rf $CFGDIR"
  trap "$CLEANUP" SIGINT SIGTERM
  ${pkgs.chromium}/bin/chromium --user-data-dir=$CFGDIR $@
  $CLEANUP
  ''
