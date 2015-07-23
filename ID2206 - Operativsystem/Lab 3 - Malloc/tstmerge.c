/* 

checks if merging of freed blocks seems to work correctly

Author: Robert Rönngren 050218

*/

#include <stdio.h>
#include <stdlib.h>
#include "brk.h"
#include <unistd.h>

#define SIZE 16384

int main(int argc, char * argv[])
{
  
  void *p1, *p2,  *oldBrk, *newBrk1, *newBrk2;
  unsigned long largeSize = SIZE;

#ifdef MMAP
    oldBrk = endHeap();
#else
    oldBrk = (void *) sbrk(0);
#endif

  p1 = (void *)malloc(1);

#ifdef MMAP
    newBrk1 = endHeap();
#else
    newBrk1 = (void *) sbrk(0);
#endif

  largeSize = ((unsigned long)(newBrk1-oldBrk) > SIZE ? (unsigned long)(newBrk1-oldBrk) : SIZE);

  free(p1);

  printf("-- Testing merging of deallocated large blocks ( >= %u bytes)\n", (unsigned)largeSize);

  p1 = (void *)malloc(largeSize);
  p2 = (void *)malloc(largeSize);

  if(p1 == NULL || p2 == NULL) printf("* ERROR: unable to allocate memory of size %u bytes\n", (unsigned) largeSize);

#ifdef MMAP
    newBrk1 = endHeap();
#else
    newBrk1 = (void *) sbrk(0);
#endif

  free(p1);
  free(p2);

  p1 = (void *)malloc(largeSize * 2);

  if(p1 == NULL) printf("* ERROR: unable to allocate memory of size %u bytes\n", (unsigned)largeSize);

#ifdef MMAP
    newBrk2 = endHeap();
#else
    newBrk2 = (void *) sbrk(0);
#endif

  if(((double)(newBrk2 - oldBrk))/((double)(newBrk1 - oldBrk)) > 1)
    printf("* ERROR: not good enough usage of memory (probably incorrect merging of deallocated memory blocks)\nYour implementation used %u bytes more than expected\n", ((unsigned)(newBrk2-newBrk1)));
  else
    printf("Test passed OK\n");
  exit(0);
}
