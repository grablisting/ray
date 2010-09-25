/* CS 352 -- Assignment #1
*
*   Sept 21, 2000,  Phil Nelson
*   Modified April 8, 2001
*   Modified January 6, 2003
*   Modified September 24, 2010, Ray Peters
*
*/

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

/* Shell main */

int main (void) {

	char	buffer [LINELEN];
	int		len;

	arg_parse(input);
	char	*input[] = "  arg1       hokay   bestArgnum2  test4  zesty tacos  ";

	return 0;

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
	pid_t  cpid;
	int    status;

	/* Start a new process to do the job. */
	cpid = fork();
	if (cpid < 0) {
		perror ("fork");
		return;
	}

	/* Check for who we are! */
	if (cpid == 0) {
		/* We are the child! */
		execlp (line, line, (char *)0);
		perror ("exec");
		exit (127);
	}

	/* Have the parent wait for child to complete */
	if (wait (&status) < 0)
		perror ("wait");
}


char **arg_parse (char *line)
{
	int counter;
	char *ip;
	/* char **args; */

	/* Skip leading whitespace */
	ip = line;

	if (*ip
		+ strspn(line, " ");

	printf("%s \n\n", ip);

	while (ip ?: )
	{
		counter++;
		ip = strchr(ip, ' ');
		*ip = 0;
		ip += strspn(ip, " ") + 1;
	}


	printf("%s \n\n", ip);


	/*

	ip += strspn(ip, " ");
	printf("\n\n-%c- \n\n", *ip);



	args = (char **)malloc(sizeof(char *) * counter);


	while (*ip != 0)
	{

	}
	printf("%c", *(ip++));
	args[i] = strchr(args[i-1], ' ');
	args[i] += strspn(args[i], " ");
	*/


	printf("\n\n");
	return 0;
}