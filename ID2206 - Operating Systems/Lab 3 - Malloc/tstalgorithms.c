/*
 * Description:
 *
 * Save the address of the twilight zone.
 * First phase - allocate a MAXPOSTS number of memory blocks each
 *               of random size but of MAXSIZE/2.
 *              Check that zero size the returned pointer was NULL.
 *              Check that for non-zero size the pointer is not NULL.
 *              Fill the beginning of each block with a float.
 *
 * Computer memory usage
 *
 * Second phase - realloc randomly blocks requesting for sizes that either
 *		  can shrink or dilate the original block.
 *		  null blocks are allcated if necessary.
 *
 * Compute current break and used memory.
 * $Log: tstalgorithms.c,v $
 * Revision 1.3  1993/12/01  14:34:41  luis
 * Checks that NULL is returned for 0 bytes allocation, and that
 * non-zero is returned for non-zero bytes (trivial, but some malloc's
 * do it ...).
 *
 * Revision 1.2  1993/11/10  14:36:06  luis
 * Bug fixed. It didn't check that malloc returned NULL when size is zero.
 * An attempt to write on a NULL address generated SIGSEGV.
 *
 * Revision 1.1  1993/11/10  10:19:22  luis
 * Initial revision
 *
 */
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include "malloc.h"
#include "tst.h"
#include "brk.h"

#define MAXPOSTS 2000
#define MAXSIZE  2048
#define MAXITERS 10000
#define ALLOCATED(i) (memPosts[i].size > 0)

typedef struct
{
  double *ptr;
  int size;
} allocpost;

allocpost memPosts[MAXPOSTS];

void calcMemUsage(int *max)
{
  int sum=0,i;
  for(i=0;i<MAXPOSTS;i++) sum += memPosts[i].size*sizeof(double);
  if(sum > *max) *max = sum;
}

int main(int argc, char *argv[])
{
  int i, maxMem=0;
  void *start, *end;
  char *progname;

  if (argc > 0)
    progname = argv[0];
  else
    progname = "";
  MESSAGE("-- This test checks malloc(), free() and realloc()\n");
  srand((unsigned int)time(NULL));

#ifdef MMAP
  start = endHeap();
#else
  start = (void *)sbrk(0);
#endif

  for(i=0;i<MAXPOSTS;i++)
    {
      memPosts[i].size = rand()%(MAXSIZE/2);
      memPosts[i].ptr = (double*) malloc(memPosts[i].size*sizeof(double));
      if ( memPosts[i].size == 0 &&  memPosts[i].ptr!= NULL )
/*	MESSAGE("* ERROR: malloc doesn't return NULL pointer on zero size\n");*/
		  ;
      else if( memPosts[i].size && memPosts[i].ptr == NULL ) {
	MESSAGE("* ERROR: malloc returned NULL on non-zero size request\n");
	  }
      else if( memPosts[i].ptr != NULL )
	memPosts[i].ptr[0]  = (double)3.14;
  }
  
  calcMemUsage(&maxMem);
  
  for(i=0;i<MAXITERS;i++)
    {
      int index;
      index = rand()%MAXPOSTS;
 
      if(ALLOCATED(index))
	{ 
	if(rand()%5 < 3)
	  {
	    if(memPosts[index].ptr[0] != (double)3.14)
	      MESSAGE("* ERROR: Corrupt memory handling\n");
	    memPosts[index].size = rand()%MAXSIZE;
	    memPosts[index].ptr = 
	      (double*) realloc(memPosts[index].ptr,
				memPosts[index].size*sizeof(double)); 
	    if(memPosts[index].size && memPosts[index].ptr[0] != (double)3.14)
	      MESSAGE("* ERROR: Corrupt memory handling\n");
	    if(memPosts[index].size) memPosts[index].ptr[0] = (double)3.14;
	  }
        else
	  {
	    if(memPosts[index].ptr[0] != (double)3.14)
	      MESSAGE("* ERROR: Corrupt memory handling\n");
            free(memPosts[index].ptr);
            memPosts[index].size = 0;
	  }
	}
      else 
	{
	  memPosts[index].size = rand()%MAXSIZE;
	  memPosts[index].ptr = (double*) malloc(memPosts[index].size*sizeof(double)); 
	  if(memPosts[index].size) memPosts[index].ptr[0] = (double)3.14;
	}
      calcMemUsage(&maxMem);
    }
#ifdef MMAP
  end = endHeap();
#else
  end = (void *) sbrk(0);
#endif

  fprintf(stderr,
	  "%s: Max memory allocated %d\n%s: Memory consumed %ld\n",
	  progname,
          maxMem,
	  progname,
	  (unsigned long)(end-start));
  return 0;
}


