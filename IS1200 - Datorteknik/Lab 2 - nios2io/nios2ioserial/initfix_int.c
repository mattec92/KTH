/*
 * initfix_int.c
 *
 * Unregisters all interrupt handlers, except for
 * the JTAG UART and timer_0.
 */
 
#include <sys/alt_irq.h>

/* Allow interrupts for JTAG UART (number 0),
 * and for timer_0 (number 1) which is used for
 * the JTAG UART host-connection check.
 * 
 * Calling alt_irq_register with some IRQ number,
 * and the last argument set to 0 (NULL),
 * will disable interrupts for that IRQ number.
 * The initfix_int function calls alt_irq_register
 * this way for all IRQ numbers except number 0
 * (the IRQ number of the JTAG UART),
 * and 1 (the IRQ number of timer_0,
 * used for the JTAG UART host-connection check.
 * 
 * Note: to enable (rather than disable) an interrupt,
 * call alt_irq_register with the last argument set
 * to the address of an interrupt-handler function. */
 
#define LOWEST_IRQ_NUMBER (2)
#define HIGHEST_IRQ_NUMBER (16)

#define NULL ( (void *) 0)

/* initfix_int - unregister unused interrupt handlers
 * Returns 0 if successful, a nonzero value otherwise. */
int initfix_int( void )
{
  /* Declare a temporary for checking return values
   * from system-calls and library functions. */
  register int ret_val_check = 0;
  register int irq_number;
  for( irq_number = LOWEST_IRQ_NUMBER;
       irq_number < HIGHEST_IRQ_NUMBER;
       irq_number += 1 )
  {
    ret_val_check |= alt_irq_register( irq_number, NULL, NULL );
  }
  return( ret_val_check );
}
