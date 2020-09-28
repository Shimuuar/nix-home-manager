args@{config, pkgs, ...}:
let
  params = {
    X = {
      composeKey = "ralt";
      extraPrograms = ''
        systemctl --user start ssh-agent
        ${pkgs.lxpanel}/bin/lxpanel &
        # Run tunnel to dagda (Deluge)
        while : ; do
          ssh -oBatchMode=yes -N -L 127.0.0.2:58846:127.0.0.1:58846 192.168.1.4
          sleep 30
        done &
        # Run tunnel to dagda (syncthing)
        while : ; do
          ssh -oBatchMode=yes -N -L 127.0.0.2:8384:127.0.0.1:8384 192.168.1.4
          sleep 30
        done &
        '';
    };
  };
in import ./cfg params args

      


