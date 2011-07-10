# Compiler config
CC     = gcc
CFLAGS = -Wall -Wextra -g -O2 -std=c99

PREFIX =${HOME}/opt/bin

# Executables
EXE     = word mdo tryless
SCRIPTS = caps2ctl

.PHONY: all clean install
all : ${EXE}
clean:
	rm -rf *.o ${EXE}
install: all
	install ${EXE}     ${PREFIX}/
	install ${SCRIPTS} ${PREFIX}/

word: word.c
	${CC} ${CFLAGS} -o $@ $<
mdo: mdo.c
	${CC} ${CFLAGS} -o $@ $<
tryless: tryless.c
	${CC} ${CFLAGS} -o $@ $<
