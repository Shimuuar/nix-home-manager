args@{config, pkgs, ...}:
let
  params = {
    X = {
      composeKey = "ralt";
      extraPrograms = ''
        # Turn on num lock
        ${pkgs.numlockx}/bin/numlockx
        xrandr \
          --output HDMI-A-0 --mode 1920x1080 --pos 0x0 --rotate normal --scale 2x2 \
          --output HDMI-A-1 --mode 3840x2160 --pos 3840x0
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
