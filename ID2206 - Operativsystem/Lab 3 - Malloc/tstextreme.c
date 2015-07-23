#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include "tst.h"
#include "brk.h"
#include <unistd.h>

#define SIZE 10
#define TIMES 10000

int main(int argc, char *argv[]){
  int i;
  char *p, *q, *r;
  size_t pagesize;
  void * lowbreak, * highbreak;
  char *progname;

  if (argc > 0)
    progname = argv[0];
  else
    progname = "";

  MESSAGE("-- This test will search for memory leaks\n");
  MESSAGE("At most 3.0x pages are allocated and recycled\n");

#ifdef MMAP
  lowbreak = endHeap();
#else
  lowbreak = (void *) sbrk(0);
#endif

  pagesize = sysconf(_SC_PAGESIZE);

  for(i = 0; i < TIMES; i++){
    p = malloc(pagesize);
    q = malloc(pagesize * 2 + 1);
    r = malloc(1);
    free(p);
    free(q);
    free(r);
  }

#ifdef MMAP
  highbreak = endHeap();
#else
  highbreak = (void *) sbrk(0);
#endif

  fprintf(stderr,"%s: Used memory in test: 0x%x (= %2.2f * pagesize)\n",
	  progname, (unsigned)(highbreak - lowbreak),
	  ( 1.0 * ( highbreak - lowbreak )) / pagesize);
  if ( highbreak - lowbreak > 7 * pagesize )
    MESSAGE("* ERROR: This malloc has a memory leak\n");
  return 0;
}



