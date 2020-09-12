
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

void wait_for_pool(int pool_size) {
    while( pool_size ) {
        int status;
        wait(&status);
        pool_size--;
    }
}

int main(int argc, char** argv)
{
    int start, end, len;
    char** command;
    int max_pool  = 1;
    int pool_size = 0;

    /* Parse command line parameters */
    if( argc > 1 && 0==strcmp(argv[1],"-j") ) {
        if( argc <= 2 )
            usage();
        max_pool = atoi(argv[2]);
        if( max_pool <= 0 )
            usage();
        argc -= 2;
        argv += 2;
    }

    /* Find both "--" */
    for(start = 1;         start < argc && strcmp(argv[start],"--"); start++);
    for(end   = start + 1; end   < argc && strcmp(argv[end],  "--"); end++);
    if( start == 1 )
        usage();
    /* Calculate length of command */
    len = start + (argc - end) + 2;
    command = (char**)malloc( len * sizeof(char*) );
    /* Write command */
    for(int i = 1; i < start; i++)       /* Arguments before "--" */
        command[i] = argv[i];
    for(int i = 1; i < argc - end; i++ ) /* Arguments after "--" */
        command[i+start] = argv[end+i];
    command[start + argc - end + 1] = NULL; /* Terminating NULL */
    /* execute commands */
    for(int i = start+1; i < end; i++) {
        switch( fork() ) {
        case -1:                /* Cannot fork */
            perror("mdo: cannot fork");
            wait_for_pool(pool_size);
            exit(1);
        case 0:                 /* Child */
            command[start] = argv[i];
            execvp(command[1], command+1);
            perror("mdo: cannot execute command: ");
            wait_for_pool(pool_size);
            exit(1);
        default:                /* Parent */
            pool_size++;
            if( pool_size >= max_pool ) {
                int status;
                wait(&status);
                pool_size--;
            }
        }
    }
    // Wait for remaining processes.
    wait_for_pool(pool_size);
    return 0;
}
