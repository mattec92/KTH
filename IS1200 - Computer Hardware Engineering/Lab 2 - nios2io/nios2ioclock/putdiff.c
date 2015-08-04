/* File "putdiff.c"
 * Written by F Lundevall 2008-11-24.
 * Copyright abandoned. This file is in the public domain.
 */

#include <stdio.h>
 
/*
 * put_string
 * 
 * Simple output routine, replaces printf()
 * for constant strings.
 * 
 * The argument is a pointer to an array of char.
 * The array can have any length, as long as there
 * is a trailing null-character at the end.
 * 
 * This routine calls putchar repeatedly,
 * to do the actual output.
 */
void put_string( const char * cp )
{
  while( *cp )
  {
    putchar( *cp );
    cp += 1;
  }
}

/*
 * put_number
 * 
 * Simple output routine, replaces printf()
 * for decimal numbers (%d conversion).
 * 
 * This routine uses nonrestoring decimal division,
 * to avoid using the div instruction (which
 * works on the hardware but not in the simulator).
 */
void put_number( int num )
{
  register int i;
  register int currentdigit;
  register int notaleadingzeroanymore;
//  static const int maxpos = +2147483647;
  static const int nearmaxneg = -2147483647;
  static const char maxnegstr[] = "-2147483648";
  static const int powers[] = {     1000000000,
                                     100000000,
                                      10000000,
                                       1000000,
                                        100000,
                                         10000,
                                          1000,
                                           100,
                                            10,
                                             1,
                                             0 /* end marker */ };

  
  if( num < nearmaxneg )
  {
    put_string( maxnegstr );
    return;
  }
  
  if( num == 0 )
  {
    putchar( '0' );
    return;
  }
  
  if( num < 0 )
  {
    putchar( '-' );
    num = 0 - num;
  }
  
  notaleadingzeroanymore = 0;

  for( i = 0; powers[ i ] > 0; i = i + 1 )
  {
    if( num >= powers[ i ] ) notaleadingzeroanymore = 1;
    if( notaleadingzeroanymore )
    {
      for( currentdigit = '0'; num >= powers[ i ]; currentdigit += 1 )
        num -= powers[ i ];
      putchar( currentdigit );
    }
  }
}

void putdiff( int diff )
{
  putchar( 13 );
  putchar( 10 );
  put_string( "Time: " );
  put_number( diff );
  put_string( " cycles" );
}
