#define HEX_LOW   ( (volatile int *) 0x9f0 )
#define HEX_HIGH  ( (volatile int *) 0xa00 )

/* out_char_uart_0 must be defined externally. */
void out_char_uart_0( int );

/*
 * This table translates into 7-bit patterns
 * suitable for a 7-segment display.
 * Negative logic is used: 0 for lit segments
 * and 1 for unlit segments.
 */
int trantab[16] = { 0x40, 0x79, 0x24, 0x30,
                    0x19, 0x12, 0x02, 0x78,
                    0x00, 0x10, 0x08, 0x03,
                    0x46, 0x21, 0x06, 0x0e };                    

/*
 * cvthex - translate to 7-segment digit
 */
int cvthex( int num )
{
  int tmp = num & 0xf;
  return( trantab[ tmp ] );
}

int hexasc( int num )
{
  int tmp = num & 0xf;
  if( tmp <= 9 ) tmp += '0';
  else tmp += ('A' - 10);
  return( tmp );
}

/*
 * puthex - send 4 digits to HEX_LOW
 * Uses cvthex for conversion
 */
void puthex( int val )
{
  /* Define temp for output value,
   * initialize to rightmost digit */
  int lcd = cvthex( val );
  /* Shift input */
  val >>= 4;
  /* Look up next digit in table and
   * add to output value */
  lcd |= cvthex( val ) << 7;
  /* Shift input */
  val >>= 4;
  /* Look up next digit in table and
   * add to output value */
  lcd |= cvthex( val ) << 14;
  /* Shift input */
  val >>= 4;
  /* Look up last digit in table and
   * add to output value */
  lcd |= cvthex( val ) << 21;
  /* Send value to hex display */
  *HEX_LOW = lcd;
}

/*
 * This is a special version of puttime,
 * that uses UART_0 instead of the
 * standard output.
 * Time is also displayed on the
 * 7-segment display.
 */
void puttime( int *tid )
{
  int tmp = *tid;
  /* Send time to HEX display */
  puthex( tmp );
  /* Send time to uart_0 */
  out_char_uart_0( hexasc( tmp >> 12 ) );
  out_char_uart_0( hexasc( tmp >>  8 ) );
  out_char_uart_0( ':' );
  out_char_uart_0( hexasc( tmp >>  4 ) );
  out_char_uart_0( hexasc( tmp       ) );
  /* Add a space character for readability */
  out_char_uart_0( ' ' );
}
