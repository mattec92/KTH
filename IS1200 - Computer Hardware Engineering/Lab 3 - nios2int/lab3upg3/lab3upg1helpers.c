/*
 * lab3upg1helpers.c - version 2010-02-22
 * 
 * Written by F Lundevall.
 * Copyright abandoned.
 * This file is in the public domain.
 */

/* Declare functions which are defined in other files,
 * or late in this file (after their first use). */
int nextprime( int );
void out_char_uart_0( int );

/* The sloppy declaration of nios2int_printf below
 * hides the variable number of arguments,
 * and the variable types of those arguments. */
void nios2int_printf();

int primesearch( int next )
{
  next = nextprime( next );   /* Produce a new prime. */
  nios2int_printf( "\n\rMain: %d is prime", next );
  return( next );
}

/*
 * ********************************************************
 * *** You don't have to study the code below this line ***
 * ********************************************************
 */

/*
 * nextprime
 * 
 * Return the first prime number larger than the integer
 * given as a parameter. The integer must be positive.
 */
#define PRIME_FALSE   0     /* Constant to help readability. */
#define PRIME_TRUE    1     /* Constant to help readability. */
int nextprime( int inval )
{
   register int perhapsprime = 0; /* Holds a tentative prime while we check it. */
   register int testfactor; /* Holds various factors for which we test perhapsprime. */
   register int found;      /* Flag, false until we find a prime. */

   if (inval < 3 )          /* Initial sanity check of parameter. */
   {
     if(inval <= 0) return(1);  /* Return 1 for zero or negative input. */
     if(inval == 1) return(2);  /* Easy special case. */
     if(inval == 2) return(3);  /* Easy special case. */
   }
   else
   {
     /* Testing an even number for primeness is pointless, since
      * all even numbers are divisible by 2. Therefore, we make sure
      * that perhapsprime is larger than the parameter, and odd. */
     perhapsprime = ( inval + 1 ) | 1 ;
   }
   /* While prime not found, loop. */
   for( found = PRIME_FALSE; found != PRIME_TRUE; perhapsprime += 2 )
   {
     /* Check factors from 3 up to perhapsprime/2. */
     for( testfactor = 3; testfactor <= (perhapsprime >> 1) + 1; testfactor += 1 )
     {
       found = PRIME_TRUE;      /* Assume we will find a prime. */
       if( (perhapsprime % testfactor) == 0 ) /* If testfactor divides perhapsprime... */
       {
         found = PRIME_FALSE;   /* ...then, perhapsprime was non-prime. */
         goto check_next_prime; /* Break the inner loop, go test a new perhapsprime. */
       }
     }
     check_next_prime:;         /* This label is used to break the inner loop. */
     if( found == PRIME_TRUE )  /* If the loop ended normally, we found a prime. */
     {
       return( perhapsprime );  /* Return the prime we found. */
     } 
   }
   return( perhapsprime );      /* When the loop ends, perhapsprime is a real prime. */
} 

/*
 * out_string_uart_0
 * 
 * Simple output routine, replaces printf()
 * for constant strings.
 * 
 * The argument is a pointer to an array of char.
 * The array can have any length, as long as there
 * is a trailing null-character at the end.
 * 
 * This routine calls out_char_uart_0 repeatedly,
 * to do the actual output.
 */
void out_string_uart_0( char * cp )
{
  while( *cp )
  {
    out_char_uart_0( *cp );
    cp += 1;
  }
}

/*
 * out_number_uart_0
 * 
 * Simple output routine, replaces printf()
 * for decimal numbers (%d conversion).
 * 
 * The integer argument is converted to a string
 * of digits representing the integer in decimal format.
 * The integer is considered signed, and a minus-sign
 * precedes the string of digits if the number is
 * negative. After conversion, the string is printed
 * using out_string_uart_0.
 * 
 * This routine will print a varying number of digits, from
 * one digit (for integers in the range 0 through 9) and up to
 * 10 digits and a leading minus-sign (for the largest negative
 * 32-bit integers).
 * 
 * If the integer has the special value
 * 100000...0 (that's 31 zeros), the number cannot be
 * negated. We check for this, and treat this as a special case.
 * If the integer has any other value, the sign is saved separately.
 * 
 * If the integer is negative, it is then converted to
 * its positive counterpart. We then use the positive
 * absolute value for conversion.
 * 
 * Conversion produces the least-significant digits first,
 * which is the reverse of the order in which we wish to
 * print the digits. We therefore store all digits in a buffer,
 * in ASCII form.
 * 
 * To avoid a separate step for reversing the contents of the buffer,
 * the buffer is initialized with an end-of-string marker at the
 * very end of the buffer. The digits produced by conversion are then
 * stored right-to-left in the buffer: starting with the position
 * immediately before the end-of-string marker and proceeding towards
 * the beginning of the buffer.
 * 
 * For this to work, the buffer size must of course be big enough
 * to hold the decimal representation of the largest possible integer,
 * and the minus sign, and the trailing end-of-string marker.
 * The value 24 for ITOA_BUFSIZ was selected to allow conversion of
 * 64-bit quantities; however, the size of an int on your current compiler
 * may not allow this straight away.
 */
#define ITOA_BUFSIZ ( 24 )
void out_number_uart_0( int num )
{
  register int i, sign;
  static char itoa_buffer[ ITOA_BUFSIZ ];
  static const char maxneg[] = "-2147483648";
  
  itoa_buffer[ ITOA_BUFSIZ - 1 ] = 0;   /* Insert the end-of-string marker. */
  sign = num;                           /* Save sign. */
  if( num < 0 && num - 1 > 0 )          /* Check for most negative integer */
  {
    for( i = 0; i < sizeof( maxneg ); i += 1 )
    itoa_buffer[ i + 1 ] = maxneg[ i ];
    i = 0;
  }
  else
  {
    if( num < 0 ) num = -num;           /* Make number positive. */
    i = ITOA_BUFSIZ - 2;                /* Location for first ASCII digit. */
    do {
      itoa_buffer[ i ] = num % 10 + '0';/* Insert next digit. */
      num = num / 10;                   /* Remove digit from number. */
      i -= 1;                           /* Move index to next empty position. */
    } while( num > 0 );
    if( sign < 0 )
    {
      itoa_buffer[ i ] = '-';
      i -= 1;
    }
  }
  /* Since the loop always sets the index i to the next empty position,
   * we must add 1 in order to return a pointer to the first occupied position. */
  out_string_uart_0( &itoa_buffer[ i + 1 ] );
}

/*
 * nios2int_hexasc
 * 
 * Supplementary routine for converting a 4-bit quantity
 * into an ASCII character representing the quantity in
 * hexadecimal form: 0 through 9, or A through F (for values
 * 10 and up).
 * 
 * This routine is used by out_hex_uart_0.
 */
static int nios2int_hexasc( int num )
{
  register int tmp = num & 15;
  if( tmp > 9 ) tmp += ( 'A' - 10 );
  else tmp += '0';
  return( tmp );
}
/*
 * out_hex_uart_0
 * 
 * Simple output routine, replaces printf()
 * for hexadecimal numbers (%x conversion).
 * 
 * This routine performs an implicit conversion of the
 * input into an unsigned integer. A sign is never printed.
 * Negative inputs are shown as the hexadecimal representation
 * of their bit patterns (in unsigned form).
 * 
 * This routine always prints eight digits; if the input
 * value is small, leading zeroes will be printed.
 */
void out_hex_uart_0( int num )
{
  out_char_uart_0( nios2int_hexasc( num >> 28 ) );
  out_char_uart_0( nios2int_hexasc( num >> 24 ) );
  out_char_uart_0( nios2int_hexasc( num >> 20 ) );
  out_char_uart_0( nios2int_hexasc( num >> 16 ) );
  out_char_uart_0( nios2int_hexasc( num >> 12 ) );
  out_char_uart_0( nios2int_hexasc( num >>  8 ) );
  out_char_uart_0( nios2int_hexasc( num >>  4 ) );
  out_char_uart_0( nios2int_hexasc( num       ) );
}
