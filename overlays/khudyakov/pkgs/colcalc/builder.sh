source $stdenv/setup

mkdir -p $out/bin
cp -v $src/colcalc $out/bin
# postInstall refuses to run. OK we'll hack it around
sed -i -e "1s;.*;#!$(which python2);" $out/bin/colcalc
