
## ~/.bashrc: executed by bash(1) for non-login shells.
## see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
## for examples

## ---------------------------------------------------------
## Pathes
## ---------------------------------------------------------
export PATH=$PATH:$HOME/opt/bin
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
# Extendet globbing
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
    ## Truncate $PWD to 20 last letters if too long 
    newPWD="${PWD/#$HOME/~}"
    local pwdmaxlen=20
    if [ ${#newPWD} -gt $pwdmaxlen ]; then
        newPWD=".+${newPWD: -$pwdmaxlen}"
    fi

}

MY_NAME=${MY_NAME-$(whoami)}
PROMPT_COMMAND=truncate_pwd
case "$TERM" in
    xterm*|rxvt*)
        PS1="\[\033[43m\]\[\033[30m\][${MY_NAME}:\${?}]\${newPWD}\\[\033[00m\] \$ " ;;
    screen*)
        PS1="\[\033[43m\]\[\033[30m\]@{${MY_NAME}:\${?}}\${newPWD}\\[\033[00m\] \$ " ;;
    *)
        PS1="[\u@\h]:\${newPWD}\\$ " ;;
esac
## -----------------

## ---------------------------------------------------------
## Navigation and file operations
## ---------------------------------------------------------
# Set pager I like most ;-)
export PAGER='less -R'
alias mo=$PAGER

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
alias space2_='rename -v "s/ /_/g"'

# nice grep coloring
alias grep='grep --color'
# grepisms
alias envv="env | grep"
alias pss="ps -ef | grep"
alias dpkgg="dpkg -l | grep -i"

# One of plot types I use very often
alias hplot='gplot -histeps'
# Process structure 
alias psme="ps -u $(whoami) --forest" 
# Disk usage 
alias dum='du --max-depth=1'
# Shorthand for jobs 
alias j=jobs
# Shorthand for wc -l
alias wcl='wc -l'
# Man shorthand
alias ?='man'
# Make with include path
alias maki='make -I ~/.share/make'
# IPython with math functions
alias mpython='ipython ~/.config/mpython.py'
# Configure && build && install cabalized haskell package
alias ghc-cbi='runghc Setup.*hs configure --user --prefix=$HOME/opt && runghc Setup.*hs build && runghc Setup.*hs install'

# apt-aliases
alias apt-search='apt-cache search'
alias apt-show='aptitude show'
alias apt-source='apt-get source'
alias aptitude-up='sudo aptitude update && sudo aptitude'
# Console emacs
alias cemacs="emacs -nw"
##-----------------


## ---------------------------------------------------------
## Useful functions 
## ---------------------------------------------------------
# Calculate things ([p]rint)
function p() {  echo $@ | bc -l;  }
# Get word number 
function word() { awk "{ print $(echo $@ | sed -re 's/[0-9]+/$&/g; s/[0-9] /&,/g') }";  }
# View colored source code
function vsrc() { highlight "$@" -A | $PAGER;  }
# Make dir and cd to it
function mkcd() { mkdir -p "$1" && cd "$1";  }
# Unzip all zip files
function unzipall() { for i in "$@"; do unzip "$i"; done; }
# Repeat (shorthand for for loop) With implicit variables 
function irep() { while read i; do eval "$@" "$i"; done;  }
## VCS shorcuts and goodies
# Subversion
function svn-diff()  {  svn diff "$@" | colordiff;    } # Colored diff
function svn-gdiff() {  svn diff "$@" | kompare -o -; } # Diff in kompare
# Mercurial  
function hg-diff()  {  hg diff "$@" | colordiff;    } # Colored diff 
function hg-gdiff() {  hg diff "$@" | kompare -o -; } # View diff in kompare
function hg-qdiff() {  hg qdiff "$@" | colordiff;   } # Colored diff for queues
function hg-prune() {  # Remove all files not under version control
    # Needed to convert to absolute filenames 
    local root=$(hg root | sed 's,/,\\/,g')
    hg st -u -n | sed "s/^/$root\//" | while read q; do rm -rfv $q; done
}
# Download and unzip books from librusec
function librusecget() {
    hrefgrep "$1" | grep /download | \
	while read URL; do
	    local TMP=$(mktemp)
	    wget  "$URL" -O "$TMP" && unzip "$TMP" 
	    rm -rf "$TMP"
    done;
}
# Generate pdf from LaTeX file. DVI file and all auxillary TeX files are created in process.
function tex2pdf() {
    # LaTeX should be run twice for correct creation of ToC (if any)
    latex "$1"; latex "$1";
    dvips "${1%.tex}.dvi" -o - | ps2pdf - "${1%.tex}.pdf"
}
## -----------------


## ---------------------------------------------------------
## Fortunes (pleasant reading)
## ---------------------------------------------------------
if which fortune &> /dev/null; then 
    echo ; echo ; fortune ; echo ; echo 
fi
