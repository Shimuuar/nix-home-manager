#!/bin/sh

# apod_get_wallpaper (c) 2008 Alexey Khudyakov <alexey skladnoy {at} gmail com>

# Simple script which downloads wallpaper from APOD
# (http://antwrp.gsfc.nasa.gov/apod) and draws it on root window
#
# Not for people with expensive internet. One picture is about 1 MB.


# Directory where APOD files lie 
APOD_DIR=${HOME}/.share/apod
# Wallpaper 
APOD_PAPER=${APOD_DIR}/wallpaper.jpg
# Description
APOD_DESCR=${APOD_DIR}/description

# Flags
DO_DOWNLOAD=

## Test for APOD directory existence
if [ ! -d $APOD_DIR ]; then
    mkdir -p $APOD_DIR
fi

## If wallpaper is not created today - set download flag
if [ ! -f $APOD_PAPER ]; then
    DO_DOWNLOAD=yes
elif [  $(date +%Y-%m-%d) != $(stat --format="%y" $APOD_PAPER | cut -c 1-10) ]; then
    DO_DOWNLOAD=yes
fi

## If not downloaded - download
if [ "x$DO_DOWNLOAD" != "x" ]; then
    TMP=$(mktemp)
    wget http://antwrp.gsfc.nasa.gov/apod/ -O $TMP

    # Make explanation 
    cat $TMP | 
    sed -e '1,/<b> Explanation: <\/b>/ d' \
        -e '/<center>/,$ d' |
    tr "\n" ' ' |
    sed -r \
        -e 's,< */a *>,,g' \
        -e 's/<a[^>]*>//g' \
        -e 's/ + / /g' \
        -e 's/^ *//' > $APOD_DESCR
    echo >> $APOD_DESCR

    # Extract and download wallpaper    
    ( echo -n http://antwrp.gsfc.nasa.gov/ 
        sed $TMP -r \
            -e '1,/<IMG SRC=/ !d' \
            -e '/<h1>/,$ !d' \
            -e '/<a.*image\// !d' \
            -e 's/.*href="(.*)".*/\1/'  ) |
    wget -i - -O - |
      convert - -resize 1024x768 $APOD_PAPER
    
    rm -rf $TMP
fi

# Show it!
display -backdrop -background '#A0A0A0' \
    -window root $APOD_PAPER
