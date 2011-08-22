
.PHONY: all install \
	bash screen emacs X

all     : install
install : bash screen emacs X
	make -C util install
## ================================================================

# Bash
bash: 
	[ -L ~/.bashrc   ] || ln -sf ${PWD}/bashrc.sh ~/.bashrc
# GNU screen
screen:
	[ -L ~/.screenrc ] || ln -sf ${PWD}/screenrc  ~/.screenrc
# Emacs
emacs:
	mkdir -p ~/.emacs.d/lisp
	[ -L ~/.emacs                 ] || ln -sf ${PWD}/emacs.el ~/.emacs
	[ -d ~/.emacs.d/lisp-personal ] || ln -sf ${PWD}/emacs.d  ~/.emacs.d/lisp-personal
# X
X :
	mkdir -p ~/.xmonad
	[ -L ~/.xmonad/xmonad.hs ] || ln -sf ${PWD}/X/xmonad.hs     ~/.xmonad/xmonad.hs
	[ -L ~/.xsession         ] || ln -sf ${PWD}/X/xsession      ~/.xsession
	[ -L ~/.Xresources       ] || ln -sf ${PWD}/X/Xresources    ~/.Xresources 
	[ -L ~/.XCompose         ] || ln -sf ${PWD}/X/Xresources    ~/.XCompose
	[ -L ~/.conkyrc          ] || ln -sf ${PWD}/X/conky/conkyrc ~/.conkyrc
