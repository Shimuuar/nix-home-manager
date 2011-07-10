
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#define NMAX     1024
#define AWK_STEP 1204

void failure(const char* msg)
{
    fprintf(stderr, "words: %s\n", msg);
    exit(1);
}

void help(void)
{
    fprintf(stderr,
            "USAGE: word {column numbers}\n"
            "    Select columns from stdin\n"
            "    Written in C as exersize\n"
           );
    exit(1);
}

char* awk_command(int n, int nw[])
{
    char* awkcmd;
    int   awklen;
    int   awkpos;

    /* Assemble awk command */
    awkcmd = (char*)malloc(AWK_STEP);
    awklen = AWK_STEP;
    awkpos = sprintf(awkcmd,"{ print ");
    int i = 0;
    while( i < n ) {
        const char* fmt = i == n-1 ? "$%i }" : "$%i,\" \",";
        int printed = snprintf(awkcmd+awkpos, awklen-awkpos, fmt, nw[i]);
        if( printed < awklen - awkpos - 1 ) {
            awkpos += printed;
            i      += 1;
        } else {
            awklen += AWK_STEP;
            awkcmd = realloc(awkcmd, awklen);
        }
    }
    return awkcmd;
}

int main(int argc, char** argv)
{
    int n = 0;
    int wordn[NMAX];
    int file_no = argc;

    /* Parse command line parameters */
    if( argc == 1 || !strcmp(argv[1],"-h") )
        help();
    for(int i = 1; i < argc; i++) {
        if( i >= NMAX )
            failure("too many columns selected > 1024");
        /* No more column numbers */
        if( !strcmp(argv[i], "--") ) {
            file_no = i + 1;
            break;
        }
        /* Column number */
        if( 0 == (wordn[n++] = atoi(argv[i])) )
            failure("Bad number");
    }

    /* Prepare command line parameter for awk */
    char** args = calloc(3 + (argc-file_no) ,sizeof(char*));
    args[0] = "awk";
    args[1] = awk_command(n, wordn);
    for(int i = file_no; i < argc; i++)
        args[2+i-file_no] = argv[i];
    args[2+argc-file_no] = NULL;
    /* Exec awk */
    execvp("awk", args);

    failure("cannot execute awk");
    return 1;
}
