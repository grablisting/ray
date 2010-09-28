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


void print_args (char **args);
char **arg_parse (char *linePtr);
char ** arg_parse_helper(char *linePtr, int count);


/* Shell main */

int main (void) {

	
	printf("TEST #1\n");
	arg_parse((char *)"Testing  trailing    spaces    ");
	
	printf("\n\nTEST #2\n");
	arg_parse((char *)"    Testing  leading    spaces");
	
	printf("\n\nTEST #3\n");
	arg_parse((char *)"    Testing both leading   and  trailing    spaces    ");
	
	printf("\n\nTEST #4\n");
	arg_parse((char *)"        ");
	
	return 0;
}

char ** arg_parse (char *line) {
	char ** args = arg_parse_helper(line, 0);
	//print_args(args);
	return args;
}

char ** arg_parse_helper(char *linePtr, int count) {
	char * i;
	char ** t;
	
	
	i = linePtr;
	while (*i == ' ') {
		*(i++) = 0;
	}

	
	if (*linePtr == 0) {
		printf("Found %i tokens; time to call malloc and return. \n", count);
		t = (char **) malloc(sizeof(char *) * count);
		t[count] = NULL;
	} else {
		t = arg_parse_helper(linePtr + strcspn(linePtr, " "), count + 1);
		t[count] = linePtr;
	}
	
	printf("Inserting pointer for token %i @: %s \n", count, linePtr);
	return t;
}


void print_args (char **args) {
	int i;
	
	printf("\n\n");
	
	for (i = 0; args[i] != 0; i++)
		printf("args[%i] = %s \n", i, args[i]);
	
	printf("\n\n");
}

