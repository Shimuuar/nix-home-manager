source $stdenv/setup

cp $src/{Makefile,mdo.c} .
make mdo
mkdir -p $out/bin
install mdo $out/bin/mdo
