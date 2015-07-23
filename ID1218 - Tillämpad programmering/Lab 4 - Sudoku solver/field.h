/*
 *  Author:
 *     Christian Schulte <cschulte@kth.se>
 *
 *  This is part of lab 2-2 for ID1218
 *
 *  This file contains a simple implementation of sets of
 *  digits between 1 and 9, called fields.
 *
 */

#ifndef __SUDOKU_FIELD_H__
#define __SUDOKU_FIELD_H__

#include <iostream>
#include <cassert>

#include "digit.h"
using namespace std;

class Field {
private:
  // Use integers for a bitset
  unsigned int _digits;
  // Number of digits in bitset
  unsigned int _size;
public:
  // Initialize with all digits between 1 and 9 included
  Field(void)
		: _digits((1 << 1) | (1 << 2) | (1 << 3) |
	      (1 << 4) | (1 << 5) | (1 << 6) |
	      (1 << 7) | (1 << 8) | (1 << 9)), _size(9) {}

	//: _digits(111111111), _size() {}
  // Return size of digit set (number of digits in set)
  unsigned int size(void) const {

	// FILL IN***
    return _size;

  }
	int test(void) {
		return _digits;
	}
  // Test whether digit set is empty
  bool empty(void) const {
	
	// FILL IN***
    return _size == 0; 

  }
  // Test whether set is assigned (that is, single digit left)
	//Kolla om bitset har endast en etta
  bool assigned(void) const {

    // FILL IN
	bool foundFirst = false;
	//cout << "Digits " << _digits << endl;
	for (int i = 1; i < 10; i++) {
		if ( (foundFirst == false) && ((_digits & (1 << i)) != 0) ) {
			foundFirst = true;
			//std::cout << "First 1 on spot " << i << std::endl; //Sker hela tiden???
		}
		else if (((_digits & (1 << i)) != 0) && foundFirst == true) {
			//cout << "Found second" << endl;
			return false;
		}
	}
	if (foundFirst) {
 		return true;
	}
	return false;
  }
  // Test whether digit d is included in set
  bool in(digit d) const {
    assert((d >= 1) && (d <= 9));

	// FILL IN***
    return (_digits & (1 << d)) != 0;

  }
  // Return digit to which the set is assigned
	//Returnera position av ettan om endast en etta
  digit value(void) const {
    assert(assigned());

    // FILL IN
	for (int i = 1; i <= 9; i++) {
		if (((_digits & (1 << i)) != 0)) {
			return static_cast<digit>(i);
		}
	}
  }

  // Print digits still included
  void print(std::ostream& os) const;

  // Remove digit d from set (d must be still included)
  void prune(digit d) {
    assert(in(d));

	// FILL IN***
    _digits  &= ~(1 << d);
	_size--;

  }
  // Assign field to digit d (d must be still included)
  void assign(digit d) {
    assert(in(d));

	// FILL IN***
	_digits = (1 << d);
	_size = 1;
	//std::cout << _digits << std::endl;

  }

};

// Print field
inline std::ostream&
operator<<(std::ostream& os, const Field& f) {
  f.print(os); return os;
}

#endif

