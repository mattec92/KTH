/* 	Homework 3
	Mattias Cederlund
	mcede@kth.se

	Matrix summation using OpenMP

	Usage with gcc (version 4.2 or higher required):
	
	COMPILE:
	gcc -fopenmp -o matrixSum-openmp matrixSum-openmp.c 
	
	RUN:
	./matrixSum-openmp [size] [numWorkers]
	Parameter [size] is size of matrix 
	Parameter [numWorkers] is number of worker threads
	If no parameters are used the program will run with 
	default parameters [size] = 13000 and [numWorkers] = 12

	Tests ran at u-shell.csc because load was so high at ict-servers
	that it was causing big varieties in time.

	Output from lscpu:
	CPU(s):                8
	On-line CPU(s) list:   0-7
	Thread(s) per core:    2
	Core(s) per socket:    4

	SIZE = 5000
	1 thread  - 0.157835
	2 threads - 0.0797712 - Speedup 1.978
	4 threads - 0.0397484 - Speedup 3.971
	8 threads - 0.0391951 - Speedup 4.027

	SIZE = 10000
	1 thread  - 0.629527
	2 threads - 0.31852  - Speedup 1.976
	4 threads - 0.158126 - Speedup 3.981
	8 threads - 0.156308 - Speedup 4.027

	SIZE = 13000
	1 thread  - 1.06238 
	2 threads - 0.537874 - Speedup 1.975 
	4 threads - 0.267035 - Speedup 3.978
	8 threads - 0.263675 - Speedup 4.029

	Conclusions: 
	At first it seems like a linear increase of performance
	but reaching 8 threads it flattens out. Maybe the threads have
	to wait longer for the critical sections or we get problem with
	false sharing, cach misses?

 */

#include <stdio.h>
#include <omp.h>

#define MAXSIZE 13000  /* maximum matrix size */
#define MAXWORKERS 12   /* maximum number of workers */

int numWorkers;
int size; 
int matrix[MAXSIZE][MAXSIZE];
double start_time, end_time;

void *Worker(void *);

/* read command line, initialize, and create threads */
int main(int argc, char *argv[]) {
	int i, j, max, min, imax, imin, jmax, jmin;
	long total = 0;
	/* read command line args if any */
	size = (argc > 1)? atoi(argv[1]) : MAXSIZE;
	numWorkers = (argc > 2)? atoi(argv[2]) : MAXWORKERS;
	if (size > MAXSIZE) size = MAXSIZE;
	if (numWorkers > MAXWORKERS) numWorkers = MAXWORKERS;

	omp_set_num_threads(numWorkers);

	/* initialize the matrix */
	for (i = 0; i < size; i++) {
		//  printf("[ ");
		for (j = 0; j < size; j++) {
			matrix[i][j] = rand()%99;
			//	  printf(" %d", matrix[i][j]);
		}
		//	  printf(" ]\n");
	}

	max = min = matrix[0][0];

	start_time = omp_get_wtime();
	/* Parallell region, for loop where local copies of total are reduced
	 * into a single variable total when the parallel region ends.
	 */
#pragma omp parallel for reduction (+:total) private(j)
	for (i = 0; i < size; i++)
		for (j = 0; j < size; j++){
			total += matrix[i][j];
			/* If max is larger than the current value, enter a critical section
			 * where we do the check again. If it still holds, update the shared
			 * variables max, imax, jmax. Same is for the minimum values case.
			 * We need the critical section so only one threadsess can write
			 * and update the max value, to ensure correctness.
			 */
			if (max < matrix[i][j]) {
#pragma omp critical
				{
					if (max < matrix[i][j]) {
						max = matrix[i][j];
						imax = i;
						jmax = j;
					}
				}
			}
			if (min > matrix[i][j]) {
#pragma omp critical
				{
					if (min > matrix[i][j]) {
						min = matrix[i][j];
						imin = i;
						jmin = j;
					}
				}
			}
		}
	// implicit barrier

	/* Comment about (b): As I understand it, the only time when we have
	 * multiple threads is inside the parallel section above. Therefore 
	 * it already is the master thread printing the result without any changes.
	 */
	end_time = omp_get_wtime();
	printf("Max(%d, %d): %d Min(%d, %d): %d\n", imax, jmax, max, imin, jmin, min);
	printf("the total is %li\n", total);
	printf("it took %g seconds\n", end_time - start_time);

}

