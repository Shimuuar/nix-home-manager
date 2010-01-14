#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>

#define N     20
#define CHUNK 4096

// Terminate execution if syscall failed
#define TRY(f) \
    do{ if( -1 == (f) ) fail(); } while(0)

void usage(void)
{
    printf("USAGE: tryless [file]\n"
           "    tryless read data from standard input and if it's sufficiently\n"
           "    small printf it directly to stdout. Otherwise it lauch pager\n");
    exit(1);
}

void fail(void)
{
    perror("tryless");
    exit(1);
}

int main(int argc, char** argv)
{
    size_t  nread  = 0;
    size_t  nalloc = 0;
    size_t  nlines = 0;
    ssize_t n;
    char* buffer = NULL;
    int pipefd[2];

    /* Check command line parameters */
    if( argc > 1 ) {
        if( !strcmp(argv[1], "-h") )
            usage();
        TRY( close(STDIN_FILENO) );
        TRY( open(argv[1], 0) );
    }

    /* Read first lines of data */
    do {
        /* Allocate memory if needed */
        if( nalloc - nread < CHUNK )
            buffer = realloc(buffer, nalloc+=CHUNK);
        n = read(STDIN_FILENO, buffer+nread, CHUNK);
        /* Count line breaks */
        for(int i = 0; i < n; i++) {
            if( buffer[nread + i] == '\n' )
                nlines++;
        }
        nread += n;
    } while( n != 0 && nlines < N);

    /* If input is big enough we need to spawn pager. */
    if( nlines >= N ) {
        TRY( pipe( pipefd ) );
        switch( fork() ) {
        case -1:
            fail();
        case 0:
            /* Child will supply data to parent via stdout*/
            TRY( close(pipefd[0]) );
            TRY( close(STDOUT_FILENO) );
            TRY( dup2(pipefd[1], STDOUT_FILENO) );
            TRY( close(pipefd[1]) );
            break;
        default:
            /* Parent will spawn pager and will read data through stdin */
            TRY( close(pipefd[1]) );
            TRY( close(STDIN_FILENO) );
            TRY( dup2(pipefd[0], STDIN_FILENO) );
            TRY( close(pipefd[0]) );
            execlp("less","less","-R",NULL);
            fail();
        }
    }
    /* Copy remaining data to stdout */
    write(STDOUT_FILENO, buffer, nread);
    while( (n = read(STDIN_FILENO, buffer, nalloc)) > 0 )
        write(STDOUT_FILENO, buffer, n);
    return 0;
}
