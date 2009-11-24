
CC = gcc
CFLAGS = -Wall -Wextra -g -O2 -std=c99
PREFIX=${HOME}/opt/bin

# Executables
EXE = word
SCRIPTS = look_dictionary konqueror-newtab xterm-less sshmount

.PHONY: all clean install
all : ${EXE}
clean:
	rm -rf *.o ${EXE}
install: all
	cp ${EXE} ${PREFIX}/
	for i in ${SCRIPTS}; do ln -sf ${PWD}/$$i ${PREFIX}/$$i; done

word: word.c
	${CC} ${CFLAGS} -o $@ $<
