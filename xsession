#!/bin/sh

# Read resources
xrdb .XResourses

# Download wallpaper from APOD
apod_get_wallpaper.sh &

# Run window manager 
/usr/bin/xmonad
