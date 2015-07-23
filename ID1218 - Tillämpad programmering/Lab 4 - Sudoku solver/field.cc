/*
 *  Author:
 *     Christian Schulte <cschulte@kth.se>
 *
 *  This is part of lab 2-2 for ID1218
 *
 *  This file contains the non inline member functions
 *  for fields.
 *
 */

#include "field.h"
using namespace std;

// Print digits still included
void Field::print(std::ostream& os) const {

  // FILL IN
	if (value() != 0) {
		cout << value() << endl;
	}

}
