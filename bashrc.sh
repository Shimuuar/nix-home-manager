## ~/.bashrc: executed by bash(1) for non-login shells.
## see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
## for examples

## If not running interactively, don't do anything:
[ -z "$PS1" ] && return

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
if [ -f ~/.bash.vars ]; then
    source ~/.bash.vars
else
    MY_NAME=$(whoami)
fi

function truncate_pwd
{
    ## Truncate $PWD to 20 last letters if too long 
    newPWD="${PWD/#$HOME/~}"
    local pwdmaxlen=20
    if [ ${#newPWD} -gt $pwdmaxlen ]; then
        newPWD=".+${newPWD: -$pwdmaxlen}"
    fi
}

PROMPT_COMMAND=truncate_pwd
case "$TERM" in
    xterm*|rxvt*)
        PS1="\[\033[33m\][${MY_NAME}:\${?}]\${newPWD}\\[\033[00m\] \$ " ;;
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
alias ll='ls -lF'
alias la='ls -A'
alias l='ls -1'

# rm safety belt 
alias rm='rm -i'

# Be-e-e-e-ep
alias beep='echo -en "\a"'

# cat text in windows encoding
alias catwin='iconv -f cp1251 -t utf8'
alias catkoi='iconv -f koi8-r -t utf8'

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
# Shorthand for jobs 
alias j=jobs
# Shorthand for wc -l
alias wcl='wc -l'
# Man shorthand
alias ?='man'

# apt-aliases
alias apt-search='apt-cache search'
alias apt-show='aptitude show'
alias apt-source='apt-get source'
alias aptitude-up='sudo aptitude update && sudo aptitude'

# emacs alias (while debian cannot fix emacs22 thing a have to fix it myself)
if ! which emacs &> /dev/null; then 
    alias emacs=emacs22
else
    echo 'PLEASE NOTE!'
    echo 'Debian fixed at last emacs22 symlink'
fi
##-----------------


## ---------------------------------------------------------
## Useful functions 
## ---------------------------------------------------------
# Calculate things ([p]rint)
function p() {
    echo $@ | bc -l 
}

# Get word number 
function word() {
    awk "{ print $(echo $@ | sed -re 's/[0-9]+/$&/g; s/[0-9] /&,/g') }"
}

# Gen n'th line from file 
function nline() {
    sed -e "$1 !d" $2
}

## VCS shorcuts and goodies
# Subversion 
function svn-diff() {  # Colored diff 
    svn diff $@ | colordiff
}
function svn-gdiff() { # view diff in kompare
    svn diff $@ | kompare -o -
}
# Mercurial
function hg-diff() {   # Colored diff 
    hg diff $@ | colordiff 
}
function hg-gdiff() {  # view diff in kompare
    hg diff $@ | kompare -o -
}
function hg-prune() {  # Remove all files not under version control
    # Needed to convert to absolute filenames 
    local root=$(hg root | sed 's,/,\\/,g')
    hg st -u -n | sed "s/^/$root\//" | xargs -d "\n" rm -rfv
}
## -----------------


## ---------------------------------------------------------
## Evironment variables
## ---------------------------------------------------------
export LD_LIBRARY_PATH="$HOME/lib"
export PYTHONPATH="$HOME/lib/python2.4/site-packages"
## -----------------

## ---------------------------------------------------------
## Fortunes (pleasant reading)
## ---------------------------------------------------------
if which fortune &> /dev/null; then 
    echo ; echo 
    fortune
    echo ; echo 
fi
