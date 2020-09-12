## ~/.bashrc: executed by bash(1) for non-login shells.
## see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
## for examples

## Use custom settings (this file is intended for computer-local settings)
[ -f $HOME/.bashrc.local ] && source ~/.bashrc.local
## ----------------

## If not running interactively, don't do anything:
[ -z "$PS1" ] && return

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
export HISTIGNORE='ls:[fb]g:sudo s2ram:sudo s2disk:sudo s2ram :sudo s2disk '
## -----------------


## ---------------------------------------------------------
## Fancy prompt
## ---------------------------------------------------------
function truncate_pwd()
{
    ## Set prompt color
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
# Recover names for cyrillic ZIP files
#   WARNING|!!! Could and will corrupt unsuspection names
function zip-recover-name() {
    convmv --notest -f cp1252 -t cp850 *
    convmv --notest -f cp866  -t utf8  *
}

# Convert from DOS to unix line breaks
alias dos2unix='tr -d "\r"'
# Remove spaces from file names (Requires sane rename)
alias space2_='rename -v "s/\s*-\s*/-/g; s/\s+/_/g; s/_-_/-/g"'

# nice grep coloring
alias grep='grep --color'
# grepisms
alias envv="env | grep"
alias pss="ps -ef | grep"
alias dpkgg="dpkg -l | grep -i"

# Process structure
alias psme="ps -u $(whoami) --forest"
# Disk usage
alias dum='du --max-depth=1'
# Shorthand for wc -l
alias wcl='wc -l'

# apt-aliases
alias apt-search='apt-cache search'
alias apt-show='aptitude show'
alias apt-source='apt-get source'

# Console emacs
alias cemacs="emacs -nw"

# LaTeX aliases. It shoud die on errors and do not bug me
alias    latex='latex    < /dev/null'
alias pdflatex='pdflatex < /dev/null'



## ---------------------------------------------------------
## Useful functions
## ---------------------------------------------------------
# Calculate things ([p]rint) (borrowed from GDB)
function p() {  echo $@ | bc -l;  }
# Make dir and cd to it
function mkcd() { mkdir -p "$1" && cd "$1";  }
# Kill all processes owned by me
function suicide() { kill $(ps -u $(whoami) | grep -Eo '^ *[0-9]+'); }

# Convert SVG to PDF and EPS
function svg2pdf() {
    inkscape "$1" --export-text-to-path --export-area-drawing --export-pdf ${1%.svg}.pdf;
}
function svg2eps() {
    inkscape "$1" --export-text-to-path --export-area-drawing --export-eps ${1%.svg}.eps;
}
function svg2png() {
    inkscape "$1" --export-text-to-path --export-area-drawing --export-dpi=${2-90} --export-png ${1%.svg}.png;
}
# Strip first page from papers downloaded from JSTOR
function dejstor() {
    if [ -z "$1" ]; then echo "No input file";  return 1; fi
    if [ -z "$2" ]; then echo "No output file"; return 1; fi
    pdftk "$1" cat 2-end output "$2"
}
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

Cabal-Version:  >= 1.10
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
  Default-Language:    Haskell2010
  Build-Depends:
    base >=3 && <5
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
    [ -f .gitignore ] || cat > .gitignore <<EOF
dist
dist-newstyle
.ghc.environment
.stack-work
TAGS
EOF
}


## ---------------------------------------------------------
## ghci shells
## ---------------------------------------------------------

ghci-with() {
    local EXPR="haskellPackages.ghcWithPackages(p: with p; [$@])"
    nix-shell -p "$EXPR" --run ghci
}

ghci-numeric() {
    ghci-with "statistics" "$@"
}

## ---------------------------------------------------------
## Fortunes (pleasant reading)
## ---------------------------------------------------------
if which fortune &> /dev/null; then
    echo ; echo ; fortune ; echo ; echo
fi
