

## ~/.bashrc: executed by bash(1) for non-login shells.
## see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
## for examples

## ---------------------------------------------------------
## Pathes
## ---------------------------------------------------------
# Prepend path to envvar if it's not there already
function prepend_to() {
    local name="$1"
    local var=$(eval echo \$"{$1}")
    if [ x${var} = 'x' ]; then
	# Envvar is empty export it
	eval export $name="$2"
    elif echo $var | tr : '\n' | grep -qE "^$2$"; then
	# It's already there
	:
    else
	# Let's prepend it
	eval export $name="$2":$var
    fi
}

prepend_to PATH ${HOME}/opt/bin
prepend_to PATH ${HOME}/.cabal/bin
export PYTHONPATH=${HOME}/opt/python/lib$(getconf LONG_BIT | sed 's/32//')/python$(python -V 2>&1 | egrep -o '2\.[0-9]')/site-packages
## ----------------

## If not running interactively, don't do anything:
[ -z "$PS1" ] && return
## Use custom settings (this file is intended for use on multiple boxes)
[ -f ~/.bashrc.local ] && source ~/.bashrc.local

## ---------------------------------------------------------
## Shell options
## ---------------------------------------------------------
# check the window size after each command 
shopt -s checkwinsize
# Extended globbing
shopt -s extglob
## -----------------


## ---------------------------------------------------------
## Programable completion
## ---------------------------------------------------------
# Make cd use only directories
complete -d cd
## -----------------


## ---------------------------------------------------------
## History control
## ---------------------------------------------------------
## Ignore and erase duplicate commands 
export HISTCONTROL=ignoredups:erasedups:ingnorespace
## Ignore particular commands
export HISTIGNORE=ls:[fb]g
## -----------------


## ---------------------------------------------------------
## Fancy prompt
## ---------------------------------------------------------
function truncate_pwd
{
    if [ "$?" = 0 ]; then
	PS1_COL=42
    else
	PS1_COL=41
    fi
    ## Truncate $PWD to 20 last letters if too long 
    newPWD="${PWD/#$HOME/~}"
    local pwdmaxlen=20
    if [ ${#newPWD} -gt $pwdmaxlen ]; then
        newPWD=".+${newPWD: -$pwdmaxlen}"
    fi
}

MY_NAME=${MY_NAME-$(hostname)}
PROMPT_COMMAND=truncate_pwd
case "$TERM" in
    xterm*|rxvt*)
        PS1="\[\033[\${PS1_COL}m\]\[\033[30m\][${MY_NAME}:\${?}]\${newPWD}\\[\033[00m\] \$ " ;;
    screen*)
        PS1="\[\033[\${PS1_COL}m\]\[\033[30m\]@{${MY_NAME}:\${?}}\${newPWD}\\[\033[00m\] \$ " ;;
    *)
        PS1="[\u@\h]:\${newPWD}\\$ " ;;
esac
## -----------------

## ---------------------------------------------------------
## Navigation and file operations
## ---------------------------------------------------------
# Set pager I like most ;-)
export PAGER=less
alias mo="less -R"

# ls coloring & aliases 
if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
fi
alias l='ls -1'
alias ll='ls -lF'
alias la='ls -A'
alias lla='ls -lA'

# rm safety belt 
alias rm='rm -i'

# cat text in different encodings
alias catwin='iconv -f cp1251 -t utf8'
alias catkoi='iconv -f koi8-r -t utf8'
alias catdos='iconv -f cp866 -t utf8'
alias catlatin1='iconv -f latin1 -t utf8'
# Convert from DOS to unix line breaks
alias dos2unix='tr -d "\r"'
# Remove spaces from file names (Requires sane rename)
alias space2_='rename -v "s/ *- */-/g; s/ /_/g; s/_-_/-/g"'

# nice grep coloring
alias grep='grep --color'
# grepisms
alias envv="env | grep"
alias pss="ps -ef | grep"
alias dpkgg="dpkg -l | grep -i"

# Process structure 
alias psme="ps -u $(whoami) --forest"
suicide() { kill $(ps -u $(whoami) -o pid | grep -v PID); }
# Disk usage 
alias dum='du --max-depth=1'
# Shorthand for jobs 
alias j=jobs
# Shorthand for wc -l
alias wcl='wc -l'
# Make which shows notification on completion
function make-notify () { make "$@" && notify-send "Make done" || notify-send "Make failed"; }
# IPython with math functions
alias mpython='ipython ~/.config/mpython.py'

# apt-aliases
alias apt-search='apt-cache search'
alias apt-show='aptitude show'
alias apt-source='apt-get source'
alias yum-avail='yum list available | grep'
alias yum-installed='yum list installed | grep'

# Console emacs
alias cemacs="emacs -nw"

# LaTeX aliases. It shoud die on errors and do not bug me
alias    latex='latex    < /dev/null'
alias pdflatex='pdflatex < /dev/null'

# Configure and build python packages
alias python-setup="[ -f setup.py ] && python setup.py build && python setup.py install --prefix=${HOME}/opt/python"
##-----------------


## ---------------------------------------------------------
## Useful functions 
## ---------------------------------------------------------
# Calculate things ([p]rint) (borrowed from GDB)
function p() {  echo $@ | bc -l;  }
# View colored source code
function vsrc() { highlight "$@" -A | less -R; }
# Make dir and cd to it
function mkcd() { mkdir -p "$1" && cd "$1";  }
# Kill all processes owned by me
function suicide() { kill $(ps -u $(whoami) | grep -Eo '^ *[0-9]+'); }
## VCS shorcuts and goodies
# Subversion
function svn-diff()  {  svn diff "$@" | colordiff | tryless; } # Colored diff
function svn-gdiff() {  svn diff "$@" | kompare -o -; }        # Diff in kompare
# Mercurial  
function hg-diff()  {  hg diff "$@" | colordiff | tryless; } # Colored diff 
function hg-gdiff() {  hg diff "$@" | kompare -o -; } # View diff in kompare
function hg-qdiff() {  hg qdiff "$@" | colordiff | tryless; } # Colored diff for queues
function hg-prune() {  # Remove all files not under version control
    hg st -un | sed "s:^:$(hg root)/:" | while read q; do rm -rfv "$q"; done
}
function hg-clean() { # Remove *.{orig,rej} files
    hg st -un | sed "s:^:$(hg root)/:" | egrep '\.(rej|orig)$' | while read q; do rm -rfv "$q"; done
}
function hg-qexport { # export top patch in mercurial queue
    local name=$(hg qtop) || return 1
    hg export "$name" > "/tmp/${name}.patch" \
	&& echo "$name exported" \
	|| echo "Could not export $name"
}
# Darcs
function darcs-diff() { darcs diff -u "$@" | colordiff | less -R --quit-if-one-screenless; }
function darcs-what() { yes y | darcs send -o /dev/null; }

# Force removal of all packages
ghc-pkg-force-remove() {
    ghc-pkg unregister "$1"
    if [ $? != 0 ]; then
	for i in $( ghc-pkg unregister "$1" 2>&1 | sed -e 's/.*://; s/(.*//'); do
	    ghc-pkg unregister "$i"
	done
	ghc-pkg unregister "$1"
    fi
}

# Functionn to fetch and unpack gzipped tarball
function gettar() {
    cd $(wget "$1" -O - | tee $(expr match "$1" '.*/\([^/]*\)') | tar xzvf - | (head -1 ; cat > /dev/null))
}
# Function to fetch and unpack gzipped tarball (without leaving tarball behind)
function gettarc() {
    cd $(wget "$1" -O - | tar xzvf - | (head -1 ; cat > /dev/null))
}
# Download and unpack ZIP file
function getzip() {
    local name=$(expr match "$1" '.*/\([^/]*\)')
    wget -q "$1" -O "$name"
    unzip "$name"
}
# Download and unpack ZIP file without leaving it behind
function getzipc() {
    local TMP=$(mktemp)    || exit 1
    wget -q "$1" -O "$TMP" || (rm -f "$TMP"; exit 1)
    unzip "$TMP"           || (rm -f "$TMP"; exit 1)
}
# Convert SVG to PDF and EPS
svg2pdf() { inkscape "$1" --export-text-to-path --export-area-drawing --export-pdf ${1%.svg}.pdf; }
svg2eps() { inkscape "$1" --export-text-to-path --export-area-drawing --export-eps ${1%.svg}.eps; }
svg2png() { inkscape "$1" --export-text-to-path --export-area-drawing --export-dpi=${2-90} --export-png ${1%.svg}.png; }
# Cabalize haskell project
cabalize () {
    # Project name
    local name=$(pwd | sed -e 's@.*/@@')
    # Cabal file
    cat > "${name}.cabal" <<EOF
Name:           ${name}
Version:        0.1
Synopsis:       <<<SYNOPSIS>>>
Description:
  <<<DESCRIPTION>>>

Cabal-Version:  >= 1.6
License:        BSD3
License-File:   LICENSE
Author:         Aleksey Khudyakov <alexey.skladnoy@gmail.com>
Maintainer:     Aleksey Khudyakov <alexey.skladnoy@gmail.com>
Homepage:       http://bitbucket.org/Shimuuar/${name}
Category:       Data
Build-Type:     Simple

source-repository head
  type:     hg
  location: http://bitbucket.org/Shimuuar/${name}

Library
  Ghc-options:          -Wall
  Build-Depends:        base >=3 && <5
  Exposed-modules:      
EOF
    # Setup
    cat > Setup.hs <<EOF
import Distribution.Simple
main = defaultMain
EOF
    # BSD3 license
    cat ~/program/config/licenses/BSD3 > LICENSE
    # .hgignore if not present
    [ -f .hgignore ] || cat > .hgignore <<EOF
dist
EOF
}
## -----------------

## ---------------------------------------------------------
## Fortunes (pleasant reading)
## ---------------------------------------------------------
if which fortune &> /dev/null; then 
    echo ; echo ; fortune ; echo ; echo 
fi
