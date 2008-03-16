#!/bin/sh

# Read resources
xrdb .XResourses

# Download wallpaper from APOD
apod_get_wallpaper.sh &

# Run bars
xbar | dzen2 -ta r  -w 896  -x 128 &
stalonetray -bg '#000' --icon-size 16 --geometry 128x18+0+0 &

# Run window manager 
/usr/bin/xmonad
