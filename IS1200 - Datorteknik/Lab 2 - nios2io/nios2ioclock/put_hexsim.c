/*
 * put_hexsim.c - simulate a seven-segment display with four digits,
 * using only the ASCII console.
 * 
 * Written by F Lundevall 2008-10-07, from an idea by William Sandqvist.
 * Copyright abandoned. This file is in the public domain.
 */

#include <stdio.h>

#define POSLEN 11
#define LEFTOFFSET 1
#define RIGHTOFFSET 7
#define LEFT 0
#define RIGHT 1

static void blankhoriz( char line[], int pos )
{
  register int i;
  for( i = pos * POSLEN; i < (pos * POSLEN) + 7; i += 1 )
    line[ i ] = ' ';
}

static void blankvert( char linea[], char lineb[], int leftright, int pos )
{
  register int i = pos * POSLEN;
  if( leftright == LEFT ) i += LEFTOFFSET; else i += RIGHTOFFSET;
  linea[ i ] = ' ';
  lineb[ i ] = ' ';
}
  
void put_hexsim( int value )
{
  char line1[] = "  *****      *****      *****      *****    ";
  char line2[] = " *     *    *     *    *     *    *     *   ";
  char line3[] = " *     *    *     *    *     *    *     *   ";
  char line4[] = "  *****      *****      *****      *****    ";
  char line5[] = " *     *    *     *    *     *    *     *   ";
  char line6[] = " *     *    *     *    *     *    *     *   ";
  char line7[] = "  *****      *****      *****      *****    ";

  if( value & 0x8000000 ) blankhoriz( line4, 0 );
  if( value & 0x4000000 ) blankvert( line2, line3, LEFT, 0 );
  if( value & 0x2000000 ) blankvert( line5, line6, LEFT, 0 );
  if( value & 0x1000000 ) blankhoriz( line7, 0 );
  if( value & 0x0800000 ) blankvert( line5, line6, RIGHT, 0 );
  if( value & 0x0400000 ) blankvert( line2, line3, RIGHT, 0 );
  if( value & 0x0200000 ) blankhoriz( line1, 0 );

  if( value & 0x0100000 ) blankhoriz( line4, 1 );
  if( value & 0x0080000 ) blankvert( line2, line3, LEFT, 1 );
  if( value & 0x0040000 ) blankvert( line5, line6, LEFT, 1 );
  if( value & 0x0020000 ) blankhoriz( line7, 1 );
  if( value & 0x0010000 ) blankvert( line5, line6, RIGHT, 1 );
  if( value & 0x0008000 ) blankvert( line2, line3, RIGHT, 1 );
  if( value & 0x0004000 ) blankhoriz( line1, 1 );

  if( value & 0x0002000 ) blankhoriz( line4, 2 );
  if( value & 0x0001000 ) blankvert( line2, line3, LEFT, 2 );
  if( value & 0x0000800 ) blankvert( line5, line6, LEFT, 2 );
  if( value & 0x0000400 ) blankhoriz( line7, 2 );
  if( value & 0x0000200 ) blankvert( line5, line6, RIGHT, 2 );
  if( value & 0x0000100 ) blankvert( line2, line3, RIGHT, 2 );
  if( value & 0x0000080 ) blankhoriz( line1, 2 );

  if( value & 0x0000040 ) blankhoriz( line4, 3 );
  if( value & 0x0000020 ) blankvert( line2, line3, LEFT, 3 );
  if( value & 0x0000010 ) blankvert( line5, line6, LEFT, 3 );
  if( value & 0x0000008 ) blankhoriz( line7, 3 );
  if( value & 0x0000004 ) blankvert( line5, line6, RIGHT, 3 );
  if( value & 0x0000002 ) blankvert( line2, line3, RIGHT, 3 );
  if( value & 0x0000001 ) blankhoriz( line1, 3 );
  
  printf( "\n" );
  printf( "%s\n", line1 );
  printf( "%s\n", line2 );
  printf( "%s\n", line3 );
  printf( "%s\n", line4 );
  printf( "%s\n", line5 );
  printf( "%s\n", line6 );
  printf( "%s\n", line7 );
}
