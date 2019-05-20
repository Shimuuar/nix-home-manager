#/usr/bin/env bash
#
# We need to set environment variables in order to allow cling to find
# *.pcm & some of includes. It foolishly try to find them in the current
# directory

cd $out
mkdir bin-wrapped
mv bin/rt-plot bin-wrapped/
cat > bin/rt-plot <<EOF
#!/bin/bash

export ROOT_INCLUDE_PATH=$out/include/root-plot/cling
export LD_LIBRARY_PATH=$out/lib:$LD_LIBRARY_PATH
exec $out/bin-wrapped/rt-plot "\$@"
EOF
chmod 755 bin/rt-plot
