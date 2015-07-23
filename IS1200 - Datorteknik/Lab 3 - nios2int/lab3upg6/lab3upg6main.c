/*
 * Main C program for nios2int 2010
 * Assignment 6: Interrupts from
 * timer_1 and de2_pio_keys4
 */

/* Include header file for alt_irq_register() */
#include <sys/alt_irq.h>

/* Define the null pointer */
#define NULL_POINTER ( (void *) 0)

/* Define address for de2_pio_redled18 */
volatile int * const de2_pio_redled18_base = (volatile int *) 0x810;
/* The above means:
 * 
 * de2_pio_redled18_base is a pointer variable.
 * The pointer is allowed to point to a volatile int.
 * 
 * The keyword "volatile" means that this int must be read (with a load
 * instruction) every time the C program reads it, and written
 * (with a store instruction) every time the C program writes to it.
 * So the keyword "volatile" forbids the compiler to re-use old values
 * of the int. Re-using old values, instead of reading them again from memory,
 * is a very common optimization that the compiler can perform.
 * 
 * We initialize the pointer to 0x810.
 * 0x810 is an integer value, and not a pointer, so we must use a
 * type-cast to tell the compiler that we really know what we are doing.
 * The type-cast is the type-specification in parentheses.
 * The type in the cast must be the same as the type of de2_pio_redled18_base.
 * 
 * The keyword "const" means that we are not allowed to change this variable.
 * The C compiler will give an error message if we write C code that tries
 * to change a const. This can help you catch some common typing-mistakes.
 * 
 * The "const" keyword has been placed very carefully. We can not change
 * the pointer-variable itself, but we can change whatever value that
 * the pointer-variable points to. So we can write to address 0x810 
 * without any error-messages from the compiler, but we cannot change
 * de2_pio_redled18_base to point to something else.
 * 
 * Note: the "volatile" keyword has also been placed very carefully, so that
 * it is the int that becomes volatile and not something else.
 */

/* Define addresses etc for de2_pio_keys4 */
volatile int * const de2_pio_keys4_base    = (volatile int *) 0x840;
volatile int * const de2_pio_keys4_intmask = (volatile int *) 0x848;
volatile int * const de2_pio_keys4_edgecap = (volatile int *) 0x84c;
const int de2_pio_keys4_intindex = 2;
const int de2_pio_keys4_irqbit = 1 << 2;
/* de2_pio_keys4_irqbit 
 * is a bit-mask with a 1 in the bit with index 2 */

/* Define addresses etc for de2_pio_toggles18 */
volatile int * const de2_pio_toggles18_base    = (volatile int *) 0x850;
volatile int * const de2_pio_toggles18_intmask = (volatile int *) 0x858;
volatile int * const de2_pio_toggles18_edgecap = (volatile int *) 0x85c;
const int de2_pio_toggles18_intindex = 3;
const int de2_pio_toggles18_irqbit = 1 << 3;
/* de2_pio_toggles18_irqbit
 * is a bit-mask with a 1 in the bit with index 3 */

/* Define addresses etc for timer_1 */
volatile int * const timer_1_base        = (volatile int *) 0x920;
volatile int * const timer_1_status      = (volatile int *) 0x920; /* same as base */
volatile int * const timer_1_control     = (volatile int *) 0x924;
volatile int * const timer_1_period_low  = (volatile int *) 0x928;
volatile int * const timer_1_period_high = (volatile int *) 0x92c;
volatile int * const timer_1_snaplow     = (volatile int *) 0x930;
volatile int * const timer_1_snaphigh    = (volatile int *) 0x934;
const int timer_1_intindex = 10;
const int timer_1_irqbit = 1 << 10;
/* timer_1_irqbit
 * is a bit-mask with a 1 in the bit with index 10 */

/* Define address for de2_pio_hex_low28 */ 
volatile int *  de2_pio_hex_low28 = (volatile int *) 0x9f0;

/* Define address for de2_pio_greenled9 */
volatile int *  de2_pio_greenled9 = (volatile int *) 0xa10;

/* Define address for de2_uart_0 */
#define UART_0   ( (volatile int *) 0x860 )

/* Delay parameter for somedelay() */
#define DELAYPARAM (65535)

/* Delay parameter for bigdelay() */
#define BIGDELAYPARAM (33)

/* 
 * Define timeout count for timer_1
 * Use 4999999 for the simulator (six 9's - 0.1 seconds),
 * but 49999999 for the hardware (seven 9's - 1.0 seconds ).
 */
#define TIMER_1_TIMEOUT (4999999)

/* Define global variables. They are declared volatile,
 * since they are modified by interrupt handlers. */
volatile int mytime = 0x5957;   /* Time display */
volatile int myleds = 0;        /* Green LEDs (local copy) */

/* Declare those functions that are defined in other files. */
int initfix_int( void );        /* in initfix_int.c */
void puttime( volatile int * ); /* in puttime_uart_0.c */
void tick ( volatile int * );   /* in your file tick.s */

void out_char_uart_0( int c )
{
  /* Wait until transmitter is ready */
  while( (UART_0[2] & 0x40) == 0 );
  /* Now send character */
  UART_0[1] = c & 0xff;
}

/* This simple subroutine stalls
 * execution for a short while. */
void somedelay( void )
{
  int i = DELAYPARAM;
  while( (i = i - 1) > 0);
}

/* This simple subroutine stalls
 * execution for a long while. */
void bigdelay( void )
{
  int j = BIGDELAYPARAM;
  while( (j = j - 1) > 0) somedelay();
}

/*
 * The n2_fatal_error function is called for unexpected
 * conditions which most likely indicate a programming error
 * somewhere in this file. The function prints "FATAL ERROR"
 * using out_char_uart_0, lights an "Err" pattern on the
 * seven-segment display, and then enters an infinite loop.
 */
void n2_fatal_error()
{
/* Define the pattern to be sent to the seven-segment display. */
#define N2_FATAL_ERROR_HEX_PATTERN ( 0xcbd7ff )
  /* Define error message text to be printed. */
  static const char n2_fatal_error_text[] = "FATAL ERROR";
  /* Define pointer for pointing into the error message text. */
  register const char * cp = n2_fatal_error_text;

  /* Disable interrupts. The return value is the previous value
   * from the status register; since the n2_fatal_error function
   * never returns and never re-enables interrupts, this value
   * is discarded. To inform the compiler that we deliberately
   * ignore the return value, we cast it to void. */
  (void) alt_irq_disable_all();

  /* Send pattern to seven-segment display. */
  *de2_pio_hex_low28 = N2_FATAL_ERROR_HEX_PATTERN;
  /* Print the error message. */
  while( *cp )
  {
    out_char_uart_0( *cp );
    cp = cp + 1;
  }

  /* Stop and wait forever. */
  while( 1 );
}

/* 
 * Interrupt handler for de2_pio_keys4.
 * The parameters are ignored here, but are
 * required for correct compilation.
 * The type alt_u32 is an Altera-defined
 * unsigned integer type.
 * 
 * To help debugging interruptible interrupt-handlers,
 * this handler delays a long while when a key is pressed.
 * However, there is no delay when the key is released.
 * 
 * We keep a software copy of the LED value, since
 * the parallel output ports are not program-readable.
 * 
 * Example: we send out the value 1 on de2_pio_keys4,
 * by executing *DE2_PIO_KEYS4_BASE = 1;
 * Then we try to read the port by executing
 * int test_val = *DE2_PIO_KEYS4_BASE; // WRONG
 * The value of test_val is now undefined.
 * The port returns some bits which are not related
 * to the value we have written.
 * 
 * The software copy of the LED value
 * for this interrupt handler
 * is the global variable myleds, defined above.
 */
void irq_handler_keys( void * context, alt_u32 irqnum )
{
  /* Read edge capture register of the de2_pio_keys4 device. */
  int edges = *de2_pio_keys4_edgecap;
  /* Clear edge capture register - writing
   * any value clears all bits. */
  *de2_pio_keys4_edgecap = 0;
  /* If action on KEY0 */
  if( edges & 1 )
  {
    /* If KEY0 is pressed now */
    if( (*de2_pio_keys4_base & 1) == 0 )
    {
      /* Turn on green LED LEDG0
       * in software copy of LED bits. */
      myleds = myleds | 1;
      /* Copy software LED bits to actual LEDs. */
      *de2_pio_greenled9 = myleds;
          
      /* Print an upper-case 'D' using out_char_uart_0. */
      out_char_uart_0( 'D' );
      
      /* Wait a long while */
      bigdelay();
      /* Print a lower-case 'd' using out_char_uart_0. */
      out_char_uart_0( 'd' );
    }
    /* If KEY0 is released now */
    else if( (*de2_pio_keys4_base & 1) != 0 )
    {
      /* Turn off green LED LEDG0
       * in software copy of LED bits. */
      myleds = myleds & 0xffffe;

      /* Print an 'U' using out_char_uart_0. */
      out_char_uart_0( 'U' );
      /* Copy software LED bits to actual LEDs. */
      *de2_pio_greenled9 = myleds;
    }
  }
}

/*
 * Initialize de2_pio_keys4 for interrupts.
 */
void keysinit_int( void )
{
  /* Declare a temporary for checking return values
   * from system-calls and library functions. */
  register int ret_val_check;

  /* Disable interrupts for de2_pio_keys4,
   * if they were enabled.
   * 
   * The function alt_irq_disable returns an int,
   * which always has the value 0.
   * We use a type cast to void to tell the compiler
   * that we really want to ignore the return value. */
  (void) alt_irq_disable( de2_pio_keys4_intindex );
  
  /* Allow interrupts from KEY0 only. */
  *de2_pio_keys4_intmask = 1;
  
  /* Set up Altera's interrupt wrapper for
   * interrupts from the de2_pio_keys4 device.
   * The function alt_irq_register will enable
   * interrupts from de2_pio_keys4.
   * Return value is zero for success,
   * nonzero for failure. */
  ret_val_check = alt_irq_register( de2_pio_keys4_intindex,
                                    NULL_POINTER,
                                    irq_handler_keys );
  /* If there was an error, terminate the program. */                                 
  if( ret_val_check != 0 ) n2_fatal_error();
}

/* 
 * Interrupt handler for timer_1.
 * The parameters are ignored here, but are
 * required for correct compilation.
 * The type alt_u32 is an Altera-defined
 * unsigned integer type.
 */
void irq_handler_timer_1( void * context, alt_u32 irqnum )
{
  *timer_1_status = 0; /* Acknowledge interrupt */
  tick( &mytime );
  puttime( &mytime );
}

/*
 * Initialize timer_1 for regular interrupts,
 * once every timeout period.
 * The timeout period is defined above,
 * see definition of TIMER_1_TIMEOUT
 */
void timerinit_int( void )
{  
  /* Declare a local temporary variable
   * for checking return values
   * from system-calls and library functions. */
  register int ret_val_check;

  /* Disable interrupts for timer_1,
   * if they were enabled.
   * 
   * The function alt_irq_disable returns an int,
   * which always has the value 0.
   * We use a type cast to void to tell the compiler
   * that we really want to ignore the return value. */
  (void) alt_irq_disable( timer_1_intindex );

  *timer_1_period_low = TIMER_1_TIMEOUT & 0xffff;
  *timer_1_period_high = TIMER_1_TIMEOUT >> 16;
  *timer_1_control = 7;
  /* START bit (must always be a 1)
   * CONT bit (timer restarts on timeout)
   * ITO bit (interrupt on timeout) */
  
  /* Set up Altera's interrupt wrapper for
   * interrupts from the timer_1 device.
   * Return value is zero for success,
   * nonzero for failure. */
  ret_val_check = alt_irq_register( timer_1_intindex,
                                    NULL_POINTER,
                                    irq_handler_timer_1 );
  /* If there was an error, terminate the program. */                                 
  if( ret_val_check != 0 ) n2_fatal_error();                                   
}

void irq_handler_toggles(void* context, alt_u32 irqnum) {
   *de2_pio_redled18_base = 0x1;                //Sätt på första red-led
    out_char_uart_0(0x53);                      //Skriv ut S
    bigdelay();
    *de2_pio_redled18_base = 0x0;               //Stäng av red-leds
    out_char_uart_0(0x73);                      //Skriv ut s
}

void toggles_init() {
    void* null_pointer = (void*) 0;             //Skapa en nullpointer
    *de2_pio_toggles18_intmask = 0x1;           //Mask för vilka toggles som ska kunna ge int
    alt_irq_register(3, null_pointer, irq_handler_toggles); //Initiera interrupts för index 3,
}                                                           //Handlern är irq_handler_toggles

int main()
{
  /* Remove unwanted interrupts.
   * initfix_int is supplied by KTH.
   * A nonzero return value indicates failure. */
  if( initfix_int() != 0 ) n2_fatal_error();
  
  /* Initialize de2_pio_keys4 for
   * interrupts. */
  keysinit_int();
  
  /* Initialize timer_1 for
   * continuous timeout interrupts. */
  timerinit_int();
  toggles_init();
 
  /* Loop forever. */
  while( 1 )
  {
    out_char_uart_0('_'); /* print an underscore */
    
    /* Programmed delay between underscores.
     * Defined earlier in this file. */
    somedelay();
  }
}
