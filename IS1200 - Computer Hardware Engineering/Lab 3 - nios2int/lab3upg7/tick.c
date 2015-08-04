/*
 * tick.c - increment time in BCD format
 *
 * Copyright abandoned. This file is in the public domain.
 * Written by F Lundevall 2009-07-07 (first version).
 */

/*
 * tick expects a pointer to a memory location containing
 * BCD-coded time, and returns nothing.
 * 
 * Time is stored as: days - hours - minutes - seconds,
 * with two BCD digits for each number.
 */
void tick( unsigned int * timep )
{
  /* Get current value, store locally */
  register unsigned int t = * timep;
  t += 1; /* Increment local copy */
  
  /* If result was not a valid BCD-coded time, adjust now */

  if( (t & 0x0000000f) == 0x0000000a ) t += 0x00000006;
  if( (t & 0x000000f0) == 0x00000060 ) t += 0x000000a0;
  /* Seconds are now OK */

  if( (t & 0x00000f00) == 0x00000a00 ) t += 0x00000600;
  if( (t & 0x0000f000) == 0x00006000 ) t += 0x0000a000;
  /* Minutes are now OK */

  if( (t & 0x000f0000) == 0x000a0000 ) t += 0x00060000;
  if( (t & 0x00ff0000) == 0x00240000 ) t += 0x00dc0000;
  /* Hours are now OK */

  if( (t & 0x0f000000) == 0x0a000000 ) t += 0x06000000;
  if( (t & 0xf0000000) == 0xa0000000 ) t = 0;
  /* Days are now OK */

  * timep = t; /* Store new value */
}
