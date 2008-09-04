#!/bin/sh
#
# Script to install configs using symlinks

function fail() { echo "$@" >&2; exit 1; }
function bad_place() { fail "Must be launched in config directory"; }
function config_exist() { fail "Config already exists"; }

function install_script()
{
    SRC=$(/bin/pwd)/"$1"
    TARGET="$2"

    [ -L "$TARGET" -a "$(readlink -f "$TARGET")" = "$SRC" ] && \
	echo "\`$1' already installed" && return 0
    [ -e "$TARGET" ] && \
	echo "Warning: \`$1 exists. Remove it to install." && return 1
    ln -s "$SRC" "$TARGET"
}

function install()
{
    while [ ! -z "$1" ]; do
	case "$1" in
	    bash)   install_script bashrc.sh ~/.bashrc   ;;
	    screen) install_script screenrc  ~/.screenrc ;;
	    emacs)
		mkdir -p ~/.emacs.d/lisp
		install_script emacs.el ~/.emacs
		install_script emacs.d  ~/.emacs.d/lisp-personal
		;;
	    *) echo Unknown directive "$1";;
	esac
	shift
    done
}

## ================================================================
# Test that we are sitting in right place
[ -f install.sh   -a \
    -f emacs.el   -a  -d emacs.d  -a\
    -f bashrc.sh  -a \
    -f screenrc       ] || bad_place


if [ ${#@} == 0 ]; then
    install emacs bash screen
else
    install "$@"
fi
