#!/bin/sh

# apod_get_wallpaper (c) 2008 Alexey Khudyakov <alexey skladnoy {at} gmail com>

# Display description of APOD


# Directory where APOD files lie 
APOD_DIR=${HOME}/.share/apod
# Description
APOD_DESCR=${APOD_DIR}/description

if [ -f $APOD_DESCR ]; then 
    TMP=$(mktemp)
    # Format text 
    fmt -w 85 $APOD_DESCR | sed -e 's/^/^p\(10\)/' > $TMP
    # Count lines
    N_LINES=$(wc -l < $TMP)

    # Some hardcoded display parameters
    (
        echo APOD
        cat $TMP
    ) |
    dzen2 -p 60 -l $((N_LINES+1)) \
        -e 'onstart=uncollapse;button1=exit:0' \
        -w 624 -x 200 -y 200
    
    rm -rf $TMP
fi