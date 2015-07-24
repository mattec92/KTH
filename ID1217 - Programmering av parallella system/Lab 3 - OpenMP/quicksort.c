/*	Homework 3
	Mattias Cederlund
	mcede@kth.se

    Quicksort using OpenMP

    Usage with gcc (version 4.4 or higher required):
	
	COMPILE
    gcc -fopenmp -o quicksort quicksort.c
	
	RUN
    ./quicksort [size] [numWorkers]
	Parameter [size] is size of array
	Parameter [numWorkers] is number of worker threads
	If no parameters are used the program will run with 
	default parameters [size] = 5000000 and [numWorkers] = 12

	Tests ran at u-shell.csc because load was so high at ict-servers
	that it was causing big varieties in time.
	 
	Output from lscpu:
	CPU(s):                8
	On-line CPU(s) list:   0-7
	Thread(s) per core:    2
	Core(s) per socket:    4

	SIZE = 1000000
	1 thread  - 0.248994
	2 threads - 0.12804   - Speedup 1.945
	4 threads - 0.0714132 - Speedup 3.487
	8 threads - 0.0648078 - Speedup 3.842
	
	SIZE = 2000000
	1 thread  - 0.537157
	2 threads - 0.276962 - Speedup 1.939
	4 threads - 0.158447 - Speedup 3.390
	8 threads - 0.137578 - Speedup 3.904
	
	SIZE = 4000000
	1 thread  - 1.09457
	2 threads - 0.568864 - Speedup 1.924
	4 threads - 0.308295 - Speedup 3.550
	8 threads - 0.257502 - Speedup 4.251
	
	Conclusions:
	When increasing number of threads we do not get a linear increase
	in preformance because a large part of the execution is not done 
	in parallel. The first execution of Qsort only uses one thread.
	At recursion depth 1 we use two threads, and at depth 2 we use four.
	A large part of execution is done before we have enough tasks to use
	all available threads.



*/

#include <stdio.h>
#include <omp.h>

#define MAXSIZE 5000000 /* Maximum size of array*/
#define MAXWORKERS 12 /* Maximum amount of worker threads */

int size = MAXSIZE;
int vector[MAXSIZE];
double start_time, end_time; /* start and end times */
int numWorkers;

/* Regular quiksort algorithm, with the only exception that
 * the recursive step is done in parallel with openmp tasks
 */
void Qsort(int first, int last) {
  int pivot, i_pivot, temp, left, right;
  if (first >= last) return; // no need to sort
  // otherwise select a pivot
  i_pivot = (first + last) / 2;
  pivot = vector[i_pivot];
  left = first; 
  right = last;
  while (left <= right) {
    if (vector[left] > pivot) { // swap left element with right element
       temp = vector[left]; 
       vector[left] = vector[right]; 
       vector[right] = temp;
       if (right == i_pivot) {
        i_pivot = left;
       }
       right--;
    } 
    else { 
      left++;
    }
  }
  // place the pivot in its place (i.e. swap with right element)
  temp = vector[right];
  vector[right] = pivot;
  vector[i_pivot] = temp;
  // sort two sublists in parallel;

  /* The recursive steps in quicksort execution is implemented as separate tasks */
  #pragma omp task 
    Qsort(first, (right - 1));
  #pragma omp task 
    Qsort((right + 1), last);

}

int main(int argc, char *argv[]) {
  int i;

  /* determine size */
  size = (argc > 1) ? atoi(argv[1]) : MAXSIZE;
  if (size <= 0 || size > MAXSIZE)
    size = MAXSIZE;

  numWorkers = (argc > 2)? atoi(argv[2]) : MAXWORKERS;
  if (numWorkers > MAXWORKERS) numWorkers = MAXWORKERS;

  omp_set_num_threads(numWorkers);

  /* initialize and print the vector to be sorted */
  for (i = 0; i < size; i++)
  vector[i] = (int) random () % MAXSIZE;
#ifdef DEBUG
  printf ("initial vector: \n");
  for (i = 0; i < size; i++)
  printf (" %3d", vector[i]);
  printf ("\n");
#endif

  start_time = omp_get_wtime();

  /* call Qsort  */
  /* The sorting is done in a parallel region */
  #pragma omp parallel
  {
    /* But we only want to sort the list once, so the first call
	 * to Qsort is done only once thanks to the single parameter
	 */
    #pragma omp single 
      Qsort(0, (size - 1));
  }

  end_time = omp_get_wtime();

    /* check if the vector is sorted and print the sorted vector */
  for (i = 0; i < size - 1; i++)
  if (vector[i] > vector[i + 1]) {
    printf("The resulting vector is not sorted!\n");
    //return(1);
  }
#ifdef DEBUG
  printf ("sorted vector: \n");
  for (i = 0; i < size; i++)
    printf (" %3d", vector[i]);
  printf ("\n");
#endif

  printf("It took %g seconds\n", end_time - start_time);
}


