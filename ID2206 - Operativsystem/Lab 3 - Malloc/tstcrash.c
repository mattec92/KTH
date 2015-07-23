#include <sys/types.h>
#include <stdio.h>
#include "tst.h"
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>

char *progname;

void t_stack()
{
  char *message = ", * ERROR: Catched SIGSEGV in crash test\n";
  write(2,progname, strlen(progname) + 1);
  write(2,message, strlen(message) + 1);
  exit(1);
}

  
extern caddr_t sbrk(int);

int main(int argc, char *argv[]){
  
  static char arr[20];
  caddr_t highbreak, lowbreak;
  int add = 0;
  char *p, *q, *r, *t;
  
  signal( SIGSEGV, t_stack);         /* catch segmentation faults */

  if (argc > 0)
    progname = argv[0];
  else
    progname = "";

  MESSAGE("Curious on what I'm doing to spoil your life? read my sources!\n");
  /*
   * TEST 1
   * Try to deallocate memory that is beyond the end of allocated data.
   */
  MESSAGE("Foreign dynamic memory crash test\n");
  lowbreak = sbrk(256);
  highbreak = sbrk(0);
  MESSAGE("Freeing memory at lowbreak\n");
  free(lowbreak);
  MESSAGE("Freeing memory at highbreak\n");
  free(highbreak);
  fprintf(stderr, "%s, line %d: lowbreak = 0x%x, highbreak = 0x%x\n",
	  progname, __LINE__, (unsigned) lowbreak, (unsigned) highbreak);

  /*
   * TEST 2
   * Try to free a non-allocated-via-malloc structure.
   * Then check if that area is allocated by malloc.
   */
  MESSAGE("Foreign static memory crash test\n");
  free(arr);
  t = malloc(8);

  if ( t >= arr && t < arr + 20 ) 
    MESSAGE("* ERROR: Reusage of foreign memory\n");

  /*
   * TEST 3
   * Overwrite data between (m)allocated blocks.
   * Think on what will happen if control data resides there.
   */
  MESSAGE("Test fault recovery\n");
  p = malloc(16);
  q = malloc(16);
  r = malloc(16);
  if ( p < q && q < r ) add = 1;
  if ( r < q && q < p ) add = -1;
  if ( add != 0 ){
/*    MESSAGE("Overwrite crash test going on\n");*/
    for ( t = p; t <= r; t += add )
      *t = 'x';
  }
  free(p);
  free(q);
  free(r);
  MESSAGE("All tests passed. Trying to exit nicely.\n");
}
  
