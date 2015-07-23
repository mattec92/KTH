/* THIS PROGRAM WILL NOT AND CANNOT BE MADE TO COMPILE WITH AN ANSI-STANDARD C-compiler */



#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <setjmp.h>
#include <signal.h>
#include <sys/signal.h>
#include <string.h>
#include <unistd.h>
#include "tst.h"



jmp_buf env;
char *progname;

void t_stack()
{
  char *message = ", * ERROR: Catched SIGSEGV in crash test\n";
  write(2,progname, strlen(progname) + 1);
  write(2,message, strlen(message) + 1);
  longjmp(env, 17);
/*  exit(1); */
}
  
void too_much(){
}

int main(int argc, char *argv[]){
  static char arr[20];
  caddr_t highbreak, lowbreak;
  int add = 0;
  char *p, *q, *r, *t;

  stack_t sigstk;
  struct sigaction act;

  if ( (sigstk.ss_sp = (char *) valloc( SIGSTKSZ) )== NULL ) {
    perror("sigaltstack");
    exit(-1);
  }
  sigstk.ss_size =  SIGSTKSZ;
  sigstk.ss_flags = 0;
  if (sigaltstack(&sigstk, (stack_t *)0) < 0) {
    perror("sigaltstack");
    exit(-1);
  }
  
  act.sa_handler = t_stack;
  act.sa_flags = SA_ONSTACK;
  if (sigaction(SIGSEGV, &act, NULL) != 0){
    perror("t_init: Could not set sigvec");
    exit(1);
  }

  if (sigaction(SIGBUS, &act, NULL) != 0){
    perror("t_init: Could not set sigvec");
    exit(1);
  }

  if (argc > 0)
    progname = argv[0];
  else
    progname = "";
  
  MESSAGE("Foreign static memory crash test\n");
  if ( setjmp(env) == 0 ) free(arr);
  if ( setjmp(env) == 0 ) t = malloc(8);

  if ( t >= arr && t < arr + 20 ) 
    MESSAGE("* ERROR: Reusage of foreign memory\n");

  MESSAGE("Foreign dynamic memory crash test\n");
  
  lowbreak = sbrk(256);

  highbreak = sbrk(0);
  
  fprintf(stderr, "%s, line %d: lowbreak = 0x%x, highbreak = 0x%x\n",
	  progname, __LINE__, (unsigned) lowbreak, (unsigned) highbreak);
  MESSAGE("Freeing memory at highbreak\n");
  if ( setjmp(env) == 0 )  free(highbreak);
  MESSAGE("Freeing memory at lowbreak\n");
  if ( setjmp(env) == 0 )  free(lowbreak);

  MESSAGE("This test will test fault recovery\n");

  p = malloc(16);
  q = malloc(16);
  r = malloc(16);
  
  if ( p < q && q < r ) add = 1;
  if ( r < q && q < p ) add = -1;
  if ( add != 0 ){
    MESSAGE("Overwrite crash test going on\n");
    for ( t = p; t <= r; t += add )
      *t = 'x';
  }

  if ( setjmp(env) == 0 ) {
    free(p);
    free(q);
    free(r);
  }
  MESSAGE("All tests passed. Trying to exit nicely ....\n");
  return 0;
}
