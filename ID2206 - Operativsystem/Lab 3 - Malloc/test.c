/*
 * Description - This is a program to test the functions malloc,
 * 				realloc and free for time and memory consumption.
 * 				
 * Usage - test [seed]
 * 		If seed is specified it will use this as start-seed for
 * 		random number generator, otherwise the seed is the current time.
 *
 * Save the address of the twilight zone.
 * First phase - allocate a MAXPOSTS number of memory blocks each
 *               of random size but of MAXSIZE/2.
 * 
 * Second phase - Realloc MAXITERS blocks of random size between
 * 				0 and MAXSIZE.
 * 				
 * Third phase - Free all blocks
 *
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
	struct timeval starttv;
	struct timeval endtv;

	if (argc > 1) {
		printf("Using seed: %s", argv[1]);
		srand(atoi(argv[1]));
	}
	else {
		srand((unsigned int)time(NULL));	
	}

#ifdef MMAP
	start = endHeap();
#else
	start = (void *)sbrk(0);
#endif

	gettimeofday(&starttv, NULL); 

	/*Alloc MAXPOSTS blocks of memory*/
	for(i=0;i<MAXPOSTS;i++) {
		memPosts[i].size = rand()%(MAXSIZE/2);
#ifndef NO
		memPosts[i].ptr = (double*) malloc(memPosts[i].size*sizeof(double));
#endif
	}

	gettimeofday(&endtv, NULL); 

#ifdef MMAP
	end = endHeap();
#else
	end = (void *) sbrk(0);
#endif

	printf("Time for malloc: %f\n", ((float)(endtv.tv_sec-starttv.tv_sec) + (float)(endtv.tv_usec-starttv.tv_usec)/1000000));
	calcMemUsage(&maxMem);
	printf("Maxmem after malloc: %d\n", maxMem);
	printf("Memory consumed after malloc %ld\n\n", (unsigned long)(end-start));

	gettimeofday(&starttv, NULL);

	/*Realloc MAXITERS blocks of memory*/
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

#ifdef MMAP
	end = endHeap();
#else
	end = (void *) sbrk(0);
#endif

	printf("Time for realloc: %f\n", ((float)(endtv.tv_sec-starttv.tv_sec) + (float)(endtv.tv_usec-starttv.tv_usec)/1000000));
	calcMemUsage(&maxMem);
	printf("Maxmem after realloc: %d\n", maxMem);
	printf("Memory consumed after realloc %ld\n\n", (unsigned long)(end-start));

	gettimeofday(&starttv, NULL);

	/*Free all blocks*/
	for(i=0;i<MAXPOSTS;i++) {
#ifndef NO
		memPosts[i].size = 0;
		free(memPosts[i].ptr);
#endif
	}

	gettimeofday(&endtv, NULL); 
	printf("Time for free: %f\n", ((float)(endtv.tv_sec-starttv.tv_sec) + (float)(endtv.tv_usec-starttv.tv_usec)/1000000));

	return 0;
}


