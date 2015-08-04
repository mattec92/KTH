#include <stdlib.h>
#include <stdio.h>
#include "tst.h"

#define SIZE 10
#define TIMES 30000

int main(int argc, char *argv[]){
  char *p;
  char *progname;

  if (argc > 0)
    progname = argv[0];
  else
    progname = "";

  MESSAGE("-- Test realloc() for unusual situations\n");

  MESSAGE("Allocate 17 bytes with realloc(NULL, 17)\n");
  p = realloc(NULL, 17);
  if (p == NULL)
    MESSAGE("*ERROR:Can't allocate 17 bytes. Must be bug in realloc()\n");

  MESSAGE("Write on allocated block\n");
  p[0] = p[16] = 17;
  MESSAGE("Increase block size with realloc(., 4711)\n");
  p = realloc(p, 4711);
  if (p == NULL)
    MESSAGE("* ERROR: Could not increase block size\n");
  if ( p[0] != 17 || p[16] != 17 )
    MESSAGE("* ERROR: Data destroyed during p = realloc(p, 4711)\n");

  MESSAGE("Write on allocated block\n");
  p[4710] = 47;
  MESSAGE("Decrease block size with realloc(., 17)\n");
  p = realloc(p, 17);
  if (p == NULL)
    MESSAGE("* ERROR: Could not decrease block size");
  else
    MESSAGE("Decreased block size\n");
  if ( p[0] != 17 || p[16] != 17 )
    MESSAGE("* ERROR: Data destroyed decreasing size with p = realloc(p, 17)\n");
  MESSAGE("Free block with realloc(., 0)\n");
  p = realloc(p, 0);
  if (p != NULL) 
    MESSAGE("realloc(p, 0) returns non null pointer\n");
  else
    MESSAGE("realloc(p, 0) returns null pointer\n");
  MESSAGE("Free pointer allocated with realloc(NULL, 0)\n");
  p = realloc(realloc(NULL, 0), 0);
  if (p != NULL) 
    MESSAGE("realloc(realloc(NULL, 0), 0) returned non null pointer\n");
  return 0;
}



