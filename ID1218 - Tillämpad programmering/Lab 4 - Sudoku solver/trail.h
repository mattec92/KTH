/*
 *  Author:
 *     Christian Schulte <cschulte@kth.se>
 *
 *  This is part of lab 2-2 for ID1218
 *
 *  This file contains an implementation of a trail for
 *  trailing fields.
 *
 */

#ifndef __SUDOKU_TRAIL_H__
#define __SUDOKU_TRAIL_H__

#include "field.h"

class TrailEntry {
 private:
  // Location of field
  Field* _loc;
  // Old value of field
  Field  _old;
  // Next trail entry
  TrailEntry* _next;
 public:
  // Create an entry for a field
  TrailEntry(Field& f, TrailEntry* n)
    : 

    // FILL IN (Initializer list!)
	_loc(&f), _old(f), _next(n)
	

    {}
  // Create an entry for a mark
  TrailEntry(TrailEntry* n)
    : 

    // FILL IN (Initializer list!)
	_loc(NULL), _next(n)

    {}

  // Test whether entry is a mark
  bool ismark(void) const {

    // FILL IN
	if (_loc == NULL) {
		return true;
	}
	else {
		return false;
	}

  }
  // Undo the field
  void undo(void) const {

    // FILL IN
	if (!ismark()) {
		*_loc = _old;
		//cout << "Skriver " << endl;
	}

  }
  // Get next entry
  TrailEntry* next(void) const {

    // FILL IN
	return _next;

  }
	int value(void) {
		return _old.test();
	}
};

class Trail {
 private:
  // Top of trail
  TrailEntry* tot;
 public:
  // Initialize
  Trail(void) : tot(NULL) {}
  // Trail a field by pushing
  void trail(Field& f) {

    // FILL IN
	tot = new TrailEntry(f, tot);
	//cout << "Skapar ny trail-entry, tot= " << tot << " Next= " << tot->next() << "Value: " << tot->value() <<  endl;

  }
  // Push a mark on the trail
  void mark(void) {

    // FILL IN
	tot = new TrailEntry(tot);
	//cout << "Skapar mark, tot= " << tot << " Next= " << tot->next() << endl;

  }
  // Undo trail until next mark and pop mark
  void undo(void) {

    // FILL IN
	while (tot != NULL) {
		//cout << "tot= " << tot << " next= " << tot->next() << endl;
		if (tot->ismark()) {
		//cout << "Tot is mark, one search-assign undo'd" << endl;
			delete tot;
			tot = tot->next();
			break;
		}
		tot->undo();
		delete tot;
		tot = tot->next();
		
	}

  }
  // Delete all trail entries
  ~Trail(void) {

    // FILL IN
	while(tot != NULL) {
		delete tot;
		tot = tot->next();
	}

  }
};

#endif

