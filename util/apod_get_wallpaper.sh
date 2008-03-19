#!/bin/sh

# apod_get_wallpaper (c) 2008 Alexey Khudyakov <alexey skladnoy {at} gmail com>

# Permission is hereby granted, free of charge, to any person
# obtaining a copy of this software and associated documentation files
# (the "Software"), to deal in the Software without restriction,
# including without limitation the rights to use, copy, modify, merge,
# publish, distribute, sublicense, and/or sell copies of the Software,
# and to permit persons to whom the Software is furnished to do so,
# subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.


# Simple script which downloads wallpaper from APOD
# (http:/apod.nasa.gov/apod) and draws it on root window


APOD_DIR=${HOME}/.share/apod         # Directory where APOD files lie 
APOD_LINK=${APOD_DIR}/link           # Link from which wallpaper was downloaded
APOD_PAPER=${APOD_DIR}/wallpaper.jpg # Wallpaper 
APOD_DESCR=${APOD_DIR}/description   # Description
APOD_HOST=http://apod.nasa.gov       # Web host of APOD 

# Test for APOD directory existence
[ -d $APOD_DIR ] || mkdir -p $APOD_DIR

# Download apod page for parsing 
TMP=$(mktemp)
#TMP=~/qqq/wall/apod
wget -q ${APOD_HOST}/apod/ -O - | \
    awk '/^ *$/ { print; print; }  /[^ ]/ { printf("%s", $0); }' > $TMP 

# Make explanation 
sed -r $TMP \
    -e '1,/Explanation:/ {/Credit/ !d; s#</b>.*##; s/.*<b> *//;}' \
    -e '/<script.*>/,$ d' \
    -e 's#</?[^>]*>##g' | 
sed -e '3,$ {/^ *$/ d}' | fmt > $APOD_DESCR

# Extract picture link
LINK=${APOD_HOST}/$(sed $TMP -r -e '/<IMG/ {s/.*<a href="(image[^"]*)".*/\1/; p}; d')

# Download picture if needed 
if  [ ! -f $APOD_PAPER  -o  ! -f $APOD_LINK  -o  "$1" = force ] || \
    [ "$(cat $APOD_LINK)" != $LINK ] 
then
    echo $LINK > $APOD_LINK
    wget  $LINK -O - | convert - -resize 1024x768 $APOD_PAPER
fi
rm -rf $TMP

# Show it!
display -backdrop -window root $APOD_PAPER
