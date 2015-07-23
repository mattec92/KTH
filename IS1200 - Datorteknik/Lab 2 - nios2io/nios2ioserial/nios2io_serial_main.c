/*
 * Laboratory exercise Nios2io, Assignment 1
 * Main program file
 * Written by F Lundevall, 2008-11-25
 */

/*
 * The following functions are used in this file, but
 * are defined in other files. We must tell the compiler
 * about the types of parameters and return values for
 * these functions, so that the compiler can help us
 * with type-checking.
 */

void initfix_int( void );   /* Supplied by us at KTH. */

int checktimer( void );     /* Must be written by you. */
void init_timer( void );    /* Must be written by you. */
int in_charx( void );       /* Must be written by you. */
void out_char( int );       /* Must be written by you. */
void puttime_hex( int * );  /* Must be written by you. */
void tick( int * );         /* Must be written by you. */

/*
 * The mytime variable is our 4-digit representation
 * of time, counting minutes and seconds. The encoding
 * is NBCD (Natural Binary Coded Decimal, using 4 bits
 * for each decimal digit.
 * As a testing help, this variable is initialized to
 * 59 minutes 57 seconds. This means that the important
 * changeover from 59:59 to 00:00 will happen soon
 * after program startup.
 */
int mytime = 0x5957;

/*
 * This is the main() routine. In Nios2 systems, it
 * must be declared to return an integer. If you are
 * familiar with C programming in Unix-style systems,
 * you will observe that this main() has no input parameters;
 * specifically, the argc and argv parameters are not
 * used (and not relevant) in this particular system.
 */
int main()
{
  /* Declare local variable for serial echo. */
  register int current_char;

  /* The standard startup-code from Altera initializes
   * a lot of stuff that will disturb our own initialization,
   * and cause some of our input/output programs to fail.
   * The initfix_int function, supplied by us at KTH,
   * safely reverses those unwanted actions of the
   * standard startup-code. */
  initfix_int();

  /* Call your initialization function for Timer 1. */
  init_timer();
  
  /* This is the main loop, which will run forever. */
  while( 1 )
  {
    /* Check timer, call tick and puttime if necessary. */
    if( checktimer() == 0 )
    {
      tick( &mytime );
      puttime_hex( &mytime );
    }
    
    /* Check serial port uart0.
     * If there is a new character, echo the encoded version. */
    current_char = in_charx();
    if( current_char >= 0 ) out_char( current_char + 1 );
  }
     
  /* The following line is dead code that will never be executed.
   * It's there only to suppress a warning from the compiler,
   * about the main function (which is declared to return an int). */
  return( 0 );
}
