


//////////////////////////////////////////////////////////////
//
// file: substrng_c
//
// This program retrieves two strings from the user, then attempts to
// match the second string within the first string.
//
// Ray Peters
//
// May 21, 2010
//
//////////////////////////////////////////////////////////////
#include "cdecl.h"
#include "stdio.h"
#include "string.h"

int PRE_CDECL asm_main( char *, char * ) POST_CDECL;

int main()
{
  int ret_status;
  char parent[80] = "\0";	//Initialize all characters to null terminator
  char child[80] = "\0";	//""

  printf ("Please enter a PARENT string: ");
  gets(parent);

  printf ("Please enter a CHILD string: ");
  gets(child);

  ret_status = asm_main(parent, child);
  if (ret_status >= 0) {
    printf("Position of the first match: %d \n\n", ret_status);
  } else {
    printf("No match could be made.\n\n");
  }

  return ret_status;
}
