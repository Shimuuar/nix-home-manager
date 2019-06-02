#!/bin/sh

set -e

if [ "$1" == "" ]; then
    exit 1
fi

# Symlink GLX libraries
ln -s /usr/lib/x86_64-linux-gnu/libGLX_indirect.so.0
ln -s /usr/lib/x86_64-linux-gnu/libGLX_mesa.so.0    
ln -s /usr/lib/x86_64-linux-gnu/libGLX.so.0         
# Add dependencies
mdo ln -s -- $(ldd * | sed '/=>/!d; s/ (.*//; s/^.*=> //' | sort | uniq)
# Add DRI
mdo ln -s -- $(ls /usr/lib/x86_64-linux-gnu/libdrm_*)
ln -s /usr/lib/x86_64-linux-gnu/libpciaccess.so.0
ln -s /usr/lib/x86_64-linux-gnu/libstdc++.so.6
