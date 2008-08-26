#!/bin/sh
#
# Script to install configs using symlinks

function fail() { echo "$@" >&2; exit 1; }
function bad_place() { fail "Must be launched in config directory"; }
function config_exist() { fail "Config already exists"; }

# Test that we are sitting in right place
[ -f install.sh ] || bad_place
[ -f emacs.el   ] || bad_place
[ -f bashrc.sh  ] || bad_place
[ -d emacs.d    ] || bad_place

# Check that to configs already in place
[ -f ~/.bashrc  ] && config_exist
[ -f ~/.emacs   ] && config_exist
[ -d ~/.emacs.d ] && config_exist

# Create symlinks
ln -s ${PWD}/bashrc.sh ~/.bashrc
ln -s ${PWD}/emacs.el  ~/.emacs
mkdir -p ~/.emacs.d/lisp
ln -s ${PWD}/emacs.d   ~/.emacs.d/lisp-personald
