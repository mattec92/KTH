#ifndef _brk_h_
#define _brk_h_


#define MMAP      /* if defined use mmap() instead of sbrk/brk */

#ifdef MMAP
void * endHeap(void);
#endif

#ifndef	_UNISTD_H /* USE WITH CAUTION brk() and sbrk() have been removed from the POSIX standard, most systems implement them but the parameter types may vary */
extern int brk(void *);
extern void *sbrk( );
#endif

#endif /*_brk_h */
