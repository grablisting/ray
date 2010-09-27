/* CS 352 -- Assignment #1
*
*   Sept 21, 2000,  Phil Nelson
*   Modified April 8, 2001
*   Modified January 6, 2003
*   Modified September 24, 2010, Ray Peters
*
*/

#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/wait.h>


/* Constants */

#define LINELEN 1024

/* Prototypes */

void processline (char *line);
char **arg_parse (char *line);
void print_args (char *msg, char **args);


/* Shell main */

int main (void) {

	char buffer [LINELEN];
	int len;

	while (1) {
		/* prompt and get line */
		fprintf (stderr, "%% ");
		if (fgets (buffer, LINELEN, stdin) != buffer)
			break;

		/* Get rid of \n at end of buffer. */
		len = strlen(buffer);
		if (buffer[len-1] == '\n')
			buffer[len-1] = 0;

		/* Run it ... */
		processline (buffer);
	}

	if (!feof(stdin))
		perror ("read");

	return 0;
}


void processline (char *line)
{
	pid_t	cpid;
	int		status;
	char	**args = arg_parse(line);
	
	print_args((char *)"processline args array", args);

	/* Start a new process to do the job. */
	cpid = fork();
	if (cpid < 0) {
		perror ("fork");
		return;
	}

	/* Check for who we are! */
	if (cpid == 0) {
		/* We are the child! */
		//execlp (line, line, (char *)0);
		execvp(args[0], args);
		
		perror ("exec");
		exit (127);
	}

	/* Have the parent wait for child to complete */
	if (wait (&status) < 0)
		perror ("wait");
	
	free (args);
}



char ** arg_parse (char *line)
{
	int		i;
	int		j;
	int		tokens = 0;
	char	*tokenPtr;
	char	**args;
	
	/* Skip over leading whitespace, and...
		init line[0] to first char of the first arg */
	line += strspn(line, " ");
	

	/* Token count found by pairing whitespace and character substrings */
	for (i = 0, j = strlen(line); i < j; tokens++) {
		/* Skip successive characters */
		i += strcspn(line+i, " ");

		/* Convert first trailing space to null terminator, continue */
		*(line + i) = 0;
		i++;

		/* Skip successive whitespaces */
		i += strspn(line+i, " ");
	}

	/* Allocate memory for tokens and one null terminator */
	args = (char **) malloc(sizeof(char *) * (tokens + 1));
	
	/* Save pointers to be returned */
	for (i = 0, tokenPtr = line; i <= tokens; i++) {
		args[i] = tokenPtr;
		tokenPtr += strcspn(tokenPtr, " ") + 1;
		tokenPtr += strspn(tokenPtr, " ");
	}

	/* Set last index to null */
	args[i] = NULL;
	
	return args;
}

void print_args (char *msg, char **args) {
	int i = 0;
	
	printf("\n%s \n", msg);
	
	for (i = 0; args[i] != 0; i++)
		printf("args[%i] = %s \n", i, args[i]);
	
	printf("\n\n");
}
