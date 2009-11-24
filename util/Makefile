
CC = gcc
CFLAGS = -Wall -Wextra -g -O2 -std=c99

# Executables
EXE = word

all : ${EXE}
clean:
	rm -rf *.o ${EXE}

word: word.c
	${CC} ${CFLAGS} -o $@ $<
