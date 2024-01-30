pkgs:
pkgs.writeScriptBin "chromium-ru" ''
  #!/bin/sh
  CFGDIR=$${XDG_CONFIG_HOME-$${HOME}/.config}
  exec ${pkgs.chromium}/bin/chromium --user-data-dir=$CFGDIR $@
  ''
