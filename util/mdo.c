
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <unistd.h>
#include <sys/wait.h>

void usage(void)
{
    printf("Usage: mdo command -- [arguments] -- [after]\n");
    exit(1);
}

int main(int argc, char** argv)
{
    int start, end, len, i, status;
    char** command;

    /* Find both "--" */
    for(start = 1;         start < argc && strcmp(argv[start],"--"); start++);
    for(end   = start + 1; end   < argc && strcmp(argv[end],  "--"); end++);
    if( start == 1 )
        usage();
    /* Calculate length of command */
    len = start + (argc - end) + 2;
    command = (char**)malloc( len * sizeof(char*) );
    /* Write command */
    for(i = 1; i < start; i++)       /* Arguments before "--" */
        command[i] = argv[i];
    for(i = 1; i < argc - end; i++ ) /* Arguments after "--" */
        command[i+start] = argv[end+i];
    command[start + argc - end + 1] = NULL; /* Terminating NULL */
    /* execute commands */
    for(i = start+1; i < end; i++) {
        switch( fork() ) {
        case -1:                /* Cannot fork */
            perror("mdo");
            exit(1);
        case 0:                 /* Child */
            command[start] = argv[i];
            execvp(command[1], command+1);
            perror("mdo");
            exit(1);
        default:                /* Parent */
            wait(&status);
        }
    }
    return 0;
}
