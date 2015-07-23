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
#include "malloc.h"
#include "tst.h"
#include "brk.h"
#include <time.h>
#include <sys/time.h>
#include <sys/types.h> /*definierar typen pid_t*/

#define MAXPOSTS 10000
#define MAXSIZE  2048
#define MAXITERS 10000
#define ALLOCATED(i) (memPosts[i].size > 0)

typedef struct {
	double *ptr;
	int size;
} allocpost;

allocpost memPosts[MAXPOSTS];

void calcMemUsage(int *max) {
	int sum=0,i;
	for(i=0;i<MAXPOSTS;i++) {
		sum += memPosts[i].size*sizeof(double);
	}
	if(sum > *max) {
		*max = sum;
	}
}

int main(int argc, char *argv[]) {
	int i, maxMem=0;
	void *start, *end;
	char *progname;
	struct timeval starttv; /*Används för att hämta ut timestamps*/
	struct timeval endtv; /*Används för att hämta ut timestamps*/

	if (argc > 0) {
		progname = argv[0];
	}
	else {
		progname = "";
	}
	MESSAGE("-- This test checks malloc(), free() and realloc()\n");
	if (argc > 1) {
		srand((unsigned int)argv[1]);
	}
	else {
		srand((unsigned int)time(NULL));	
	}


	start = (void *)sbrk(0);

	gettimeofday(&starttv, NULL); 
	for(i=0;i<MAXPOSTS;i++) {
		memPosts[i].size = rand()%(MAXSIZE/2);
#ifndef NO
		memPosts[i].ptr = (double*) malloc(memPosts[i].size*sizeof(double));
#endif
	}
	gettimeofday(&endtv, NULL); 
	/*printf("Time for malloc: %f\n", ((float)(endtv.tv_sec-starttv.tv_sec) + (float)(endtv.tv_usec-starttv.tv_usec)/1000000));*/
	printf("%f\n", ((float)(endtv.tv_sec-starttv.tv_sec) + (float)(endtv.tv_usec-starttv.tv_usec)/1000000));
	calcMemUsage(&maxMem);
	/*printf("Maxmem after malloc: %d\n\n", maxMem);*/
	printf("%d\n", maxMem);
	

	end = (void *) sbrk(0);

	/*printf("%s: Max memory allocated %d\n%s: Memory consumed %ld\n\n",
			progname,
			maxMem,
			progname,
			(unsigned long)(end-start));*/
	printf("%d\n%ld\n", maxMem, (unsigned long)(end-start));

	gettimeofday(&starttv, NULL);
	for(i=0;i<MAXITERS;i++) {
		int index;
		index = rand()%MAXPOSTS;

		if(ALLOCATED(index)) { 
			if(rand()%5 < 3) {
				memPosts[index].size = rand()%MAXSIZE;
#ifndef NO
				memPosts[index].ptr =  (double*) realloc(memPosts[index].ptr, memPosts[index].size*sizeof(double)); 
#endif
			}
		}
	}
	
	gettimeofday(&endtv, NULL); 
	/*printf("Time for realloc: %f\n", ((float)(endtv.tv_sec-starttv.tv_sec) + (float)(endtv.tv_usec-starttv.tv_usec)/1000000));*/
	printf("%f\n", ((float)(endtv.tv_sec-starttv.tv_sec) + (float)(endtv.tv_usec-starttv.tv_usec)/1000000));
	calcMemUsage(&maxMem);
	/*printf("Maxmem after realloc: %d\n\n", maxMem);*/
	printf("%d\n", maxMem);


	end = (void *) sbrk(0);

	
	gettimeofday(&starttv, NULL);
	for(i=0;i<MAXPOSTS;i++) {
#ifndef NO
		free(memPosts[i].ptr);
#endif
	}
	
	gettimeofday(&endtv, NULL); 
	/*printf("Time for free: %f\n", ((float)(endtv.tv_sec-starttv.tv_sec) + (float)(endtv.tv_usec-starttv.tv_usec)/1000000));*/
	printf("%f\n", ((float)(endtv.tv_sec-starttv.tv_sec) + (float)(endtv.tv_usec-starttv.tv_usec)/1000000));
	calcMemUsage(&maxMem);
	/*printf("Maxmem after free: %d\n\n", maxMem);*/
	printf("%d\n", maxMem);
	

	/*printf("%s: Max memory allocated %d\n%s: Memory consumed %ld\n",
			progname,
			maxMem,
			progname,
			(unsigned long)(end-start));
	printf("-----------------------------------\n");*/
	printf("%d\n%ld\n", maxMem, (unsigned long)(end-start));

	return 0;
}


