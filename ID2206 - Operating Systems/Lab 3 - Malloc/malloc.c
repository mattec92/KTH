/*
 * Description - This is a malloc implementation that contains the three
 * 				memory allocation functions malloc, realloc and free.
 * 				Usage is the same as for the functions in the standard library.
 * 				
 */

#include "brk.h"
#include <unistd.h>
#include <string.h> 
#include <errno.h> 
#include <sys/mman.h>
#include <stdlib.h>

#define STRATEGY_FIRST 1
#define STRATEGY_BEST 2
#define STRATEGY_WORST 3
#define STRATEGY_QUICK 4

#define SMALLEST 2								/*Smallest size of quicklist blocks*/

#ifndef MAP_ANONYMOUS							/*Some error wtih MAP_ANONYMOUS if not defined...*/
#define MAP_ANONYMOUS 32
#endif

#define NALLOC 512								/* minimum #units to request */

#ifndef NRQUICKLISTS							/*If number of quicklist is undefined, use 5*/
#define NRQUICKLISTS 5
#endif

#ifndef STRATEGY								/*If strategy is undefined, use quick*/
#define STRATEGY 4
#endif

typedef double Align;                           /* for alignment to long boundary */

union header {                                  /* block header */
	struct {
		union header *next;                     /* next block if on free list */
		unsigned size;                          /* size of this block  - what unit? */ 
	} s;
	Align x;                                 	/* force alignment of blocks */
};

typedef union header Header;

static Header base;                           	/* empty list to get started */
static Header *freep = NULL;              		/* start of free list */

static Header* quicklist[NRQUICKLISTS] = {0};	/*List for our quicklist memory blocks*/


/* free
 * 
 * free - put block ap in the free list 
 * 
 * void * ap - the block to free
 * 
 */

void free(void * ap)
{
	Header *bp, *p;

	if(ap == NULL) return;                           			/* Nothing to do */

	bp = (Header *) ap - 1;                               		/* point to block header */

	if (STRATEGY == STRATEGY_QUICK) {							/*Quick*/
		if (bp->s.size <= (SMALLEST << (NRQUICKLISTS - 1))) {	/*If block is within the range of blocks we have in quicklist*/
			int i;
			for (i = 0; bp->s.size > (SMALLEST << i); i++) {	/*Loop to find out which list to put the memory block in*/
			}
			bp->s.next = quicklist[i];							/*Add the block to appropriate list*/
			quicklist[i] = bp;
			return;
		}
	}

	for(p = freep; !(bp > p && bp < p->s.next); p = p->s.next)	/*Loop to find where in the list of free blocks to put block*/
		if(p >= p->s.next && (bp > p || bp < p->s.next))		/*If not looped whole list and block to free adress is larger than p*/
			break;                                            	/* freed block at atrt or end of arena */

	if(bp + bp->s.size == p->s.next) {                     		/* join to upper nb, if block to free ends at p's next block */
		bp->s.size += p->s.next->s.size;						/*Set pb's size to pb.size + p.size*/
		bp->s.next = p->s.next->s.next;							/*set bp's nextpointer to p's next,nextpointer*/
	}
	else
		bp->s.next = p->s.next;									/*Else, set bp's nextpointer to p's nextpointer*/
	if(p + p->s.size == bp) {                             		/* join to lower nbr, if a block ends right before bp */
		p->s.size += bp->s.size;								/*Add pb's size to p's size*/
		p->s.next = bp->s.next;									/*Set p's nextpointer to bp's nextpointer*/
	} else
		p->s.next = bp;											/*Else, set p's nextpointer to bp*/
	freep = p;													/*Set freepointer to p*/
}


#ifdef MMAP

static void * __endHeap = 0;

void * endHeap(void)
{
	if(__endHeap == 0) __endHeap = sbrk(0);
	return __endHeap;
}
#endif

/* morecore
 * 
 * morecore - ask system for more memory 
 * 
 * unsigned nu - size of block, in header-multiples
 */

static Header *morecore(unsigned nu)
{
	void *cp;
	Header *up;
#ifdef MMAP
	unsigned noPages;
	if(__endHeap == 0) __endHeap = sbrk(0);
#endif

	if(nu < NALLOC)
		nu = NALLOC;
#ifdef MMAP
	noPages = ((nu*sizeof(Header))-1)/getpagesize() + 1;
	cp = mmap(__endHeap, noPages*getpagesize(), PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANONYMOUS, -1, 0);
	nu = (noPages*getpagesize())/sizeof(Header);
	__endHeap += noPages*getpagesize();
#else
	cp = sbrk(nu*sizeof(Header));
#endif
	if(cp == (void *) -1){                                 /* no space at all */
		perror("failed to get more memory");
		return NULL;
	}
	up = (Header *) cp;
	up->s.size = nu;
	/*free((void *)(up+1));*/ /*Not compatible with quick-malloc*/
	return up;
}

/* malloc
 * 
 * malloc - Allocate a block of memory
 * 
 * size_t nbytes - number of bytes of block to allocate
 * 
 */

void * malloc(size_t nbytes) {
	Header *currentp, *prevp;
	Header * morecore(unsigned);
	unsigned nunits;

	Header *best = NULL, *prevbest = NULL;
	size_t value = (size_t) 0;

	if(nbytes == 0) { 										/*No memory needs to be allocated, return null*/
		return NULL;
	}

	nunits = (nbytes+sizeof(Header)-1)/sizeof(Header) +1; 	/*Number of header-sizes we need for the block*/

	if (STRATEGY == STRATEGY_QUICK) {						/*Quick*/
		int i;
		if (nunits <= (SMALLEST << (NRQUICKLISTS - 1))) {	/*If the requested block is smaller than the maxsize of the list*/
			for (i = 0; nunits > (SMALLEST << i); i++) {	/*Loop to find out which list to fetch memory block from*/
			}
			if (quicklist[i] != NULL) {						/*If there already exists a block, return it*/
				Header* header = quicklist[i];
				quicklist[i] = header->s.next;
				return (void *) (header + 1);
			}
			else {													/*Else, get blocks of correct size*/
				size_t size = (SMALLEST << i);						/*Size of each block to allocate*/
				size_t tomorecore = size;							/*Size of block to request from OS*/

				if (tomorecore < NALLOC) {							/*If the block to request is smaller than NALLOC, increase*/
					tomorecore = NALLOC;
				}
				Header* newblock = morecore(tomorecore);			/*Call morecore to get a new block form the OS*/

				if (newblock == NULL) {								/*If no memory left, return null*/
					return NULL;
				}

				int k;
				for (k = 0; k < tomorecore; k += size) {			/*Split the block into pieces of correct size*/
					newblock[k].s.size = size;						/*Set the correct size of the block*/
					free((void *) &(newblock[k+1]));				/*Free the block, putting it in the correct list*/
				}
				Header* toreturn = quicklist[i];					/*When done splitting, get the block to return*/
				quicklist[i] = toreturn->s.next;					/*Move the pointer to point at the next block*/
				return (void *) (toreturn + 1);						/*Return the data-area of the block*/
			}
		}
	}
	if ((prevp = freep) == NULL) {
		base.s.next = freep = prevp = &base;
		base.s.size = 0;
	}

	for(currentp = prevp->s.next;  ; prevp = currentp, currentp = currentp->s.next) {	/*Loop through the list of available blocks*/
		if(currentp->s.size >= nunits) {                           			/*Found block is big enough */
			if (STRATEGY == STRATEGY_FIRST || STRATEGY == STRATEGY_QUICK) {	/*First or big block in quick*/
				if (currentp->s.size == nunits) {                     		/*Block is exactly the right size*/
					prevp->s.next = currentp->s.next;
				}
				else {                                            		/*Block is bigger, allocate tail end */
					currentp->s.size -= nunits;
					currentp += currentp->s.size;
					currentp->s.size = nunits;
				}
				freep = prevp;
				return (void *)(currentp+1);
			}
			if (STRATEGY == STRATEGY_BEST) {
				if (best == NULL || currentp->s.size <= value) {         /*Block is smaller than the old best*/
					best = currentp;
					prevbest = prevp;
					value = currentp->s.size;
				}
			}
			if (STRATEGY == STRATEGY_WORST) {
				if (best == NULL || currentp->s.size > value) {			/*Block is bigger than the old best*/
					best = currentp;
					prevbest = prevp;
					value = currentp->s.size;
				}
			}
		}
		if(currentp == freep) {                                    				/* wrapped around free list */
			if ((STRATEGY == STRATEGY_BEST) || (STRATEGY == STRATEGY_WORST)) {
				if (best != NULL) {												/*If a best match is found*/
					currentp = best;
					prevp = prevbest;
					if (currentp->s.size == nunits) {							/*If exactly the right size*/
						prevp->s.next = currentp->s.next;
					}
					else {                                            			/*Block is bigger, allocate tail end */
						currentp->s.size -= nunits;								/*Resize head block*/
						currentp += currentp->s.size;							/*Move the pointer to the new start*/
						currentp->s.size = nunits;								/*Set the size of the new block*/
					}
					freep = prevp;
					return (void *)(currentp + 1);
				}
			}
			if((currentp = morecore(nunits)) == NULL) {				/*No block fits, call the OS for more*/
				return NULL;                                    	/*No available memory left left */
			}
			free((void *) (currentp+1));							/*Free the new memory block, so it will become available*/
		}
	}
}

/* realloc
 * 
 * realloc - Reallocate a block of memory
 * 
 * void * ptr - Pointer of block to reallocate
 * size_t size - Size of the new block
 * 
 */

void *realloc(void *ptr, size_t size) {
	if (ptr == NULL) {									/*if ptr = null, its a malloc*/
		return malloc(size);
	}
	if (size == 0) {									/*If new size = 0, its a free*/
		free(ptr);
		return NULL;
	}
	Header* h = ((Header*) ptr) - 1;					/*Get the header from the pointer*/
	size_t oldsize = (h->s.size - 1) * sizeof(Header);	/*Get the old size from the header*/
	void * newptr = malloc(size);						/*Allocate a memory block of the new size*/
	if (size < oldsize) {								/*If the new size is smaller than the old one, we will only copy*/
		oldsize = size;									/*untill the new size*/
	}
	memcpy(newptr, ptr, oldsize);						/*Do the copy*/
	free(ptr);											/*Free the old block*/
	return newptr;
}
