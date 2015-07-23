/*
 *  Author:
 *     Christian Schulte <cschulte@kth.se>
 *
 *  This is part of lab 2-2 for ID1218
 *
 *  This file contains the main program.
 *
 */

#include <iostream>
#include <cstring>
#include <cstdlib>

#include "digit.h"
#include "field.h"
#include "board.h"

#include "puzzle.h"

const puzzle examples[] = {
  {
    {0,0,0, 2,0,5, 0,0,0},
    {0,9,0, 0,0,0, 7,3,0},
    {0,0,2, 0,0,9, 0,6,0},
    
    {2,0,0, 0,0,0, 4,0,9},
    {0,0,0, 0,7,0, 0,0,0},
    {6,0,9, 0,0,0, 0,0,1},
    
    {0,8,0, 4,0,0, 1,0,0},
    {0,6,3, 0,0,0, 0,8,0},
    {0,0,0, 6,0,8, 0,0,0}
  },{
    {3,0,0, 9,0,4, 0,0,1},
    {0,0,2, 0,0,0, 4,0,0},
    {0,6,1, 0,0,0, 7,9,0},

    {6,0,0, 2,4,7, 0,0,5},
    {0,0,0, 0,0,0, 0,0,0},
    {2,0,0, 8,3,6, 0,0,4},
    
    {0,4,6, 0,0,0, 2,3,0},
    {0,0,9, 0,0,0, 6,0,0},
    {5,0,0, 3,0,9, 0,0,8}
  },{
    {0,0,0, 0,1,0, 0,0,0},
    {3,0,1, 4,0,0, 8,6,0},
    {9,0,0, 5,0,0, 2,0,0},
    
    {7,0,0, 1,6,0, 0,0,0},
    {0,2,0, 8,0,5, 0,1,0},
    {0,0,0, 0,9,7, 0,0,4},
    
    {0,0,3, 0,0,4, 0,0,6},
    {0,4,8, 0,0,6, 9,0,7},
    {0,0,0, 0,8,0, 0,0,0}
  },{
    // Fiendish puzzle April 21, 2005 Times London
    {0,0,4, 0,0,3, 0,7,0},
    {0,8,0, 0,7,0, 0,0,0},
    {0,7,0, 0,0,8, 2,0,5},
    
    {4,0,0, 0,0,0, 3,1,0},
    {9,0,0, 0,0,0, 0,0,8},
    {0,1,5, 0,0,0, 0,0,4},
    
    {1,0,6, 9,0,0, 0,3,0},
    {0,0,0, 0,2,0, 0,6,0},
    {0,2,0, 4,0,0, 5,0,0}
  },{
    // This one requires search
    {0,4,3, 0,8,0, 2,5,0},
    {6,0,0, 0,0,0, 0,0,0},
    {0,0,0, 0,0,1, 0,9,4},
    
    {9,0,0, 0,0,4, 0,7,0},
    {0,0,0, 6,0,8, 0,0,0},
    {0,1,0, 2,0,0, 0,0,3},
    
    {8,2,0, 5,0,0, 0,0,0},
    {0,0,0, 0,0,0, 0,0,5},
    {0,3,4, 0,9,0, 7,1,0}
  },{
    // Hard one from http://www.cs.mu.oz.au/671/proj3/node5.html
    {0,0,0, 0,0,3, 0,6,0},
    {0,0,0, 0,0,0, 0,1,0},
    {0,9,7, 5,0,0, 0,8,0},

    {0,0,0, 0,9,0, 2,0,0},
    {0,0,8, 0,7,0, 4,0,0},
    {0,0,3, 0,6,0, 0,0,0},

    {0,1,0, 0,0,2, 8,9,0},
    {0,4,0, 0,0,0, 0,0,0},
    {0,5,0, 1,0,0, 0,0,0}
  },{ // Puzzle 1 from http://www.sudoku.org.uk/bifurcation.htm
    {1,0,0, 9,0,7, 0,0,3},
    {0,8,0, 0,0,0, 0,7,0},
    {0,0,9, 0,0,0, 6,0,0},

    {0,0,7, 2,0,9, 4,0,0},
    {4,1,0, 0,0,0, 0,9,5},
    {0,0,8, 5,0,4, 3,0,0},

    {0,0,3, 0,0,0, 7,0,0},
    {0,5,0, 0,0,0, 0,4,0},
    {2,0,0, 8,0,6, 0,0,9}
  },{ // Puzzle 2 from http://www.sudoku.org.uk/bifurcation.htm
    {0,0,0, 3,0,2, 0,0,0},
    {0,5,0, 7,9,8, 0,3,0},
    {0,0,7, 0,0,0, 8,0,0},

    {0,0,8, 6,0,7, 3,0,0},
    {0,7,0, 0,0,0, 0,6,0},
    {0,0,3, 5,0,4, 1,0,0},

    {0,0,5, 0,0,0, 6,0,0},
    {0,2,0, 4,1,9, 0,5,0},
    {0,0,0, 8,0,6, 0,0,0}
  },{ // Puzzle 3 from http://www.sudoku.org.uk/bifurcation.htm
    {0,0,0, 8,0,0, 0,0,6},
    {0,0,1, 6,2,0, 4,3,0},
    {4,0,0, 0,7,1, 0,0,2},

    {0,0,7, 2,0,0, 0,8,0},
    {0,0,0, 0,1,0, 0,0,0},
    {0,1,0, 0,0,6, 2,0,0},

    {1,0,0, 7,3,0, 0,0,4},
    {0,2,6, 0,4,8, 1,0,0},
    {3,0,0, 0,0,5, 0,0,0}
  },{ // Puzzle 4 from http://www.sudoku.org.uk/bifurcation.htm
    {3,0,5, 0,0,4, 0,7,0},
    {0,7,0, 0,0,0, 0,0,1},
    {0,4,0, 9,0,0, 0,3,0},

    {4,0,0, 0,5,1, 0,0,6},
    {0,9,0, 0,0,0, 0,4,0},
    {2,0,0, 8,4,0, 0,0,7},

    {0,2,0, 0,0,7, 0,6,0},
    {8,0,0, 0,0,0, 0,9,0},
    {0,6,0, 4,0,0, 2,0,8}
  },{ // Puzzle 5 from http://www.sudoku.org.uk/bifurcation.htm
    {0,0,0, 7,0,0, 3,0,0},
    {0,6,0, 0,0,0, 5,7,0},
    {0,7,3, 8,0,0, 4,1,0},

    {0,0,9, 2,8,0, 0,0,0},
    {5,0,0, 0,0,0, 0,0,9},
    {0,0,0, 0,9,3, 6,0,0},

    {0,9,8, 0,0,7, 1,5,0},
    {0,5,4, 0,0,0, 0,6,0},
    {0,0,1, 0,0,9, 0,0,0}
  },{ // Puzzle 6 from http://www.sudoku.org.uk/bifurcation.htm
    {0,0,0, 6,0,0, 0,0,4},
    {0,3,0, 0,9,0, 0,2,0},
    {0,6,0, 8,0,0, 7,0,0},

    {0,0,5, 0,6,0, 0,0,1},
    {6,7,0, 3,0,1, 0,5,8},
    {9,0,0, 0,5,0, 4,0,0},

    {0,0,6, 0,0,3, 0,9,0},
    {0,1,0, 0,8,0, 0,6,0},
    {2,0,0, 0,0,6, 0,0,0}
  },{ // Puzzle 7 from http://www.sudoku.org.uk/bifurcation.htm
    {8,0,0, 0,0,1, 0,4,0},
    {2,0,6, 0,9,0, 0,1,0},
    {0,0,9, 0,0,6, 0,8,0},

    {1,2,4, 0,0,0, 0,0,9},
    {0,0,0, 0,0,0, 0,0,0},
    {9,0,0, 0,0,0, 8,2,4},

    {0,5,0, 4,0,0, 1,0,0},
    {0,8,0, 0,7,0, 2,0,5},
    {0,9,0, 5,0,0, 0,0,7}
  },{ // Puzzle 8 from http://www.sudoku.org.uk/bifurcation.htm
    {6,5,2, 0,4,8, 0,0,7},
    {0,7,0, 2,0,5, 4,0,0},
    {0,0,0, 0,0,0, 0,0,0},

    {0,6,4, 1,0,0, 0,7,0},
    {0,0,0, 0,8,0, 0,0,0},
    {0,8,0, 0,0,4, 5,6,0},

    {0,0,0, 0,0,0, 0,0,0},
    {0,0,8, 6,0,7, 0,2,0},
    {2,0,0, 8,9,0, 7,5,1}
  },{ // Puzzle 9 from http://www.sudoku.org.uk/bifurcation.htm
    {0,0,6, 0,0,2, 0,0,9},
    {1,0,0, 5,0,0, 0,2,0},
    {0,4,7, 3,0,6, 0,0,1},

    {0,0,0, 0,0,8, 0,4,0},
    {0,3,0, 0,0,0, 0,7,0},
    {0,1,0, 6,0,0, 0,0,0},

    {4,0,0, 8,0,3, 2,1,0},
    {0,6,0, 0,0,1, 0,0,4},
    {3,0,0, 4,0,0, 9,0,0}
  },{ // Puzzle 10 from http://www.sudoku.org.uk/bifurcation.htm
    {0,0,4, 0,5,0, 9,0,0},
    {0,0,0, 0,7,0, 0,0,6},
    {3,7,0, 0,0,0, 0,0,2},

    {0,0,9, 5,0,0, 0,8,0},
    {0,0,1, 2,0,4, 3,0,0},
    {0,6,0, 0,0,9, 2,0,0},

    {2,0,0, 0,0,0, 0,9,3},
    {1,0,0, 0,4,0, 0,0,0},
    {0,0,6, 0,2,0, 7,0,0}
  },{ // Puzzle 11 from http://www.sudoku.org.uk/bifurcation.htm
    {0,0,0, 0,3,0, 7,9,0},
    {3,0,0, 0,0,0, 0,0,5},
    {0,0,0, 4,0,7, 3,0,6},

    {0,5,3, 0,9,4, 0,7,0},
    {0,0,0, 0,7,0, 0,0,0},
    {0,1,0, 8,2,0, 6,4,0},

    {7,0,1, 9,0,8, 0,0,0},
    {8,0,0, 0,0,0, 0,0,1},
    {0,9,4, 0,1,0, 0,0,0}
  },{ // From http://www.sudoku.org.uk/discus/messages/29/51.html?1131034031
    {2,5,8, 1,0,4, 0,3,7},
    {9,3,6, 8,2,7, 5,1,4},
    {4,7,1, 5,3,0, 2,8,0},

    {7,1,5, 2,0,3, 0,4,0},
    {8,4,9, 6,7,5, 3,2,1},
    {3,6,2, 4,1,0, 0,7,5},

    {1,2,4, 9,0,0, 7,5,3},
    {5,9,3, 7,4,2, 1,6,8},
    {6,8,7, 3,5,1, 4,9,2}
  }
};

const unsigned int n_examples = sizeof(examples)/sizeof(puzzle);

int main(int argc, char* argv[]) {
  using namespace std;
  // How often to repeat solving
  int r = 1;
  // Which example to solve
  int e = 5;
  // Use copying or trailing?
  bool copy = true;

  // Parse comandline arguments
  int i = 1;
  while (i < argc) {
    if (!strcmp(argv[i],"-help") || !strcmp(argv[i],"--help")) {
      cerr << "Options for sudoku:" << endl
	   << "\t-copy (default)" << endl
	   << "\t\tuse copying for search" << endl
	   << "\t-trail" << endl
	   << "\t\tuse trailing for search" << endl
	   << "\t-repeat (unsigned int) default: " << r << endl
	   << "\t\trepeat solving process that often" << endl
	   << "\t(unsigned int) as last argument, default: " << e << endl
	   << "\t\twhich example to solve" << endl;
      return 0;
    } else if (!strcmp(argv[i],"-copy")) {
      copy = true;
    } else if (!strcmp(argv[i],"-trail")) {
      copy = false;
    } else if (!strcmp(argv[i],"-repeat")) {
      if (++i == argc) {
	cerr << "Missing argument for -repeat" << endl;
	return 1;
      }
      r = atoi(argv[i]);
    } else {
      e = atoi(argv[i]);
      if ((e < 0) || (e >= n_examples)) {
	cerr << "Illegal example number given" << endl;
	return 1;
      }
    }
    i++;
  }

  // Only print solution if no repeat is used
  bool print = (r == 1);

  while (r-- > 0)
    if (copy) {
      // Create board on heap
      Board* b = new Board;;
	//cout << *b << endl;
      if (!b->setup(examples[e])) {
	cerr << "Puzzle specification invalid" << endl;
	return 1;
      }
	//cout << "Skapat board" << endl;
	//cout << *b << endl;
      // Search for solution
      Board* s = b->search();
	//cout << "Sökt klart" << endl;
	//cout << *s << endl;
      // Delete board if neccessary
      if (s != b)
	delete b;
      if (s == NULL) {
	cerr << "Puzzle has no solution" << endl;
	return 1;
      } else {
	if (print)
	  cout << *s << endl;
	delete s;
      }
    } else {
      // Create board
      Board b;
      if (!b.setup(examples[e])) {
	cerr << "Puzzle specification invalid" << endl;
	return 1;
      }
      // Create trail
      Trail t;
      // Search for solution
      if (b.search(t)) {
	if (print)
	  cout << b << endl;
      } else {
	cerr << "Puzzle has no solution" << endl;
	return 1;
      }
    }
	//while(true){}
  return 0;
}



