/*
 *  Author:
 *     Christian Schulte <cschulte@kth.se>
 *
 *  This is part of lab 2-2 for ID1218
 *
 *  This file contains the class definition for a board.
 *
 */

#ifndef __SUDOKU_BOARD_H__
#define __SUDOKU_BOARD_H__

#include <iostream>

#include "digit.h"
#include "field.h"
#include "trail.h"
#include "puzzle.h"
#include <algorithm>
using namespace std;

class Board {
private:
  Field* fs[9][9]; // All fields on board
public:
  // Initialize board
  Board(void) {
    // Uses the default constructor of Field!
	for (int i = 0; i < 9; i++) {
		for (int j = 0; j < 9; j++) {
			fs[i][j] = new Field();
		}
	}
  }
  // Copy board
  Board(const Board& b) {
    
    // FILL IN 
//	copy(b.fs, b.fs + sizeof(b.fs), fs);
	for (int i = 0; i < 9; i++) {
		for (int j = 0; j < 9; j++) {
			fs[i][j] = new Field(*b.fs[i][j]);
		}
	}

  }
  // Assignment operator for board
  Board& operator=(const Board& b) {

    // FILL IN
	if (this != &b) {
		delete fs;
		copy(b.fs, b.fs + sizeof(b.fs), fs);
	}
	return *this;
  }

  // Setup a board for a given puzzle
  //   - returns false if no solution
  bool setup(const puzzle& p);

  // Print board
  void print(std::ostream& os) const;

  /*
   * Operations without trail
   *
   */

  // Assign field (fx,fy) to digit d
  //   - returns false if no solution
  bool assign(int fx, int fy, digit d) {

    // FILL IN
	//cout << "FS= " << fs[fx][fy] << endl;
	if (fs[fx][fy]->in(d)) {
		if (fs[fx][fy]->assigned()) {
			//cout << "Already assigned"<< endl;
			if (fs[fx][fy]->value() == d) {
				return true;
			}
		}
		else {
			fs[fx][fy]->assign(d);
			//cout << "Assigned? " << fs[fx][fy]->assigned() << " " << fs[fx][fy]->test()<< endl;
			propagate(fx, fy);
			return true;
		}
	}
	else {
		return false;
	}
  }

  // Prune digit d from field (fx,fy)
  //   - returns false if no solution
  bool prune(int fx, int fy, digit d) {

    // FILL IN
	if (fs[fx][fy]->in(d)) {
		fs[fx][fy]->prune(d);
		if (fs[fx][fy]->assigned()) { 
			propagate(fx, fy);
			return true;
		}
	}
	else {
		return false;
	}

  }

  // Propagate that field (fx,fy) has been assigned
  //   - returns false if no solution
  bool propagate(int fx, int fy);

  // Search for a solution
  //   - returns NULL if no solution found
  Board* search(void);

  /*
   * Operations with trail
   *
   */

  // Assign field (fx,fy) to digit d
  //   - returns false if no solution
  bool assign(int fx, int fy, digit d, Trail& t) {

    // FILL IN
	if (fs[fx][fy]->in(d)) {
		if (fs[fx][fy]->assigned()) {
			//cout << "Already assigned"<< endl;
			if (fs[fx][fy]->value() == d) {
				return true;
			}
		}
		else {
			t.trail(*fs[fx][fy]);
			//cout << "Assigned " << static_cast<int(d) << " to " << fx << "|" << fy << endl;
			fs[fx][fy]->assign(d);
			//cout << "Assigned? " << fs[fx][fy]->assigned() << " " << fs[fx][fy]->test()<< endl;
			propagate(fx, fy, t);
			return true;
		}
	}
	else {
		return false;
	}

  }

  // Prune digit d from field (fx,fy)
  //   - returns false if no solution
  bool prune(int fx, int fy, digit d, Trail& t) {

    // FILL IN
	if (fs[fx][fy]->in(d)) {
		t.trail(*fs[fx][fy]);
		fs[fx][fy]->prune(d);
		if (fs[fx][fy]->assigned()) { 
			propagate(fx, fy, t);
			return true;
		}
	}
	else {
		return false;
	}

  }

  // Propagate that field (fx,fy) has been assigned
  //   - returns false if no solution
  bool propagate(int fx, int fy, Trail& t);

  // Search for a solution
  //   - returns false if no solution found
  bool search(Trail& t);
	//DESTRUKTOR
	~Board(void) {
		for (int i = 0; i < 9; i++) {
			for (int k = 0; k < 9; k++) {
				delete fs[i][k];
			}
		}
	}
};

// Print board
inline std::ostream&
operator<<(std::ostream& os, const Board& b) {
  b.print(os); return os;
}

#endif

