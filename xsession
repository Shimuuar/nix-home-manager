#!/bin/sh

# Read resources
xrdb .XResourses

# Download wallpaper from APOD
apod_get_wallpaper.sh &

# Run bars
xbar_mpd  &
xbar_main &

# Run window manager 
/usr/bin/xmonad
