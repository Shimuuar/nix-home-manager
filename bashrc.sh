## ~/.bashrc: executed by bash(1) for non-login shells.
## see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
## for examples

## If not running interactively, don't do anything:
[ -z "$PS1" ] && return

## Don't put duplicate lines in the history and erase duplicate lines. 
## Ignore command which begins with space.
export HISTCONTROL=ignoredups:erasedups:ingnorespace

## check the window size after each command and, if necessary,
## update the values of LINES and COLUMNS.
shopt -s checkwinsize


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

PROMPT_COMMAND=truncate_pwd
case "$TERM" in
    xterm*)
        PS1="\[\033[33m\][Складной:\${?}]\${newPWD}\\[\033[00m\] $ " ;;
    *)
        PS1="[\u@\h]:\${newPWD}\\$ " ;;
esac

## =========================================================
## Navigation and file operations
## =========================================================
# Set pager I like most ;-)
export PAGER='less'
alias mo=$PAGER

## ls coloring & aliases 
if [ "$TERM" != "dumb" ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
fi
alias ll='ls -lF'
alias la='ls -A'
alias l='ls -1'

# rm safety belt 
alias rm='rm -i'

# cat text in windows encoding
alias catwin='iconv -f cp1251 -t utf8'
alias catkoi='iconv -f koi8-r -t utf8'

## nice grep coloring
alias grep='grep --color'
# grepisms
alias envv="env | grep"
alias pss="ps -ef | grep"
alias dpkgg="dpkg -l | grep -i"

# Process structure 
alias psme="ps -u $(whoami) --forest" 

# Disk usage 
alias dum='du --max-depth=1'

# apt-aliases
alias apt-search='apt-cache search'
alias apt-show='aptitude show'
alias aptitude-up='sudo aptitude update && sudo aptitude'

# Calculate things ([p]rint)
function p() {
    echo $@ | bc -l 
}

# Get word number 
function word() {
    for i in $@; do local promt+='$'$i', '; done
    awk '{print '${promt:0:${#promt}-2}'}'
}

# Gen n'th line from file 
function nline() {
    sed -e "$1 !d" $2
}

# VCS shorcuts and goodies
function svn-diff() {
    svn diff $@ | colordiff
}
function svn-gdiff() {
    svn diff $@ | kompare -o -
}
function hg-diff() {
    hg diff $@ | colordiff 
}
function hg-gdiff() {
    hg diff $@ | kompare -o -
}


export LD_LIBRARY_PATH="$HOME/lib"
export PYTHONPATH="$HOME/lib/python2.4/site-packages"


## ---------------------------------------------------------
## Fortunes (pleasant reading)
## ---------------------------------------------------------
echo
echo 
fortune
echo
echo 
