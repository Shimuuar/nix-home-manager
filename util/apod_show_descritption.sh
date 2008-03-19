#!/bin/sh

# apod_show_description (c) 2008 Alexey Khudyakov <alexey skladnoy {at} gmail com>

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


# Display description of APOD


APOD_DIR=${HOME}/.share/apod        # Directory where APOD files lie 
APOD_DESCR=${APOD_DIR}/description  # Description

if [ -f $APOD_DESCR ]; then
    N_LINES=$(wc -l < $APOD_DESCR)

    fmt -w 85 $APOD_DESCR | 
    sed -e '3,$ s/^/^p(10)/' | 
    dzen2 -p 60 -l $N_LINES \
        -w 640 -x 192 -y 200 -bg '#444' \
        -e 'onstart=uncollapse;button1=exit:0' 
fi
