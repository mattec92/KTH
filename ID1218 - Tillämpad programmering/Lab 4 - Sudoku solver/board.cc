/*
 *  Author:
 *     Christian Schulte <cschulte@kth.se>
 *     Mattias Cederlund (FILL IN's)
 *
 *  This is part of lab 2-2 for ID1218
 *
 *  This file contains the non inline member functions for a board.
 *
 */

#include "board.h"

// Print board
void Board::print(std::ostream& os) const {

  // FILL IN
	cout << "-------------------------";
	for (int i = 0; i < 9; i++) {
		cout << "\n| ";
		for (int j = 0; j < 9; j++) {
			if (fs[i][j]->assigned()) {
				cout << static_cast<int>(fs[i][j]->value()) << " ";
			}
			else {
				cout << "0 ";
			}
			if ((j % 3) == 2) {
				cout << "| ";
			}
		}
		if ((i % 3) == 2) {
			cout << "\n-------------------------";
		}
	}

}
// Setup a board for a given puzzle
bool Board::setup(const puzzle& p) {
	for (int x=0; x<9; x++) {
		for (int y=0; y<9; y++) {
		//cout << "Value " << static_cast<int>(fs[x][y]->value()) << endl;
		//cout << "Value= " << fs[x][y]->test() << " Assigned= " << fs[x][y]->assigned() << endl;
			if (p[x][y] != 0) {
				//cout << "Assigning " << static_cast<int>(p[x][y]) << endl;
				if (!assign(x,y,p[x][y])) {
					return false;
				}
			}
		}
	}	
	return true;
}

/*
 * Operations without trail
 *
 */

// Propagate that field (fx,fy) has been assigned
bool Board::propagate(int fx, int fy) {

	// FILL IN
	digit value = fs[fx][fy]->value();
	for (int i = 0; i < 9; i++) {
		if (i != fx) {
			prune(i, fy, value);
		}
		if (i != fy) {
			prune(fx, i, value);
		}
	}
	int col = fx % 3;
	int row = fy % 3;
	for (int i = 0; i < 3; i++) {
		for (int k = 0; k < 3; k++) {
			if (fx-col+k != fx && fy-row+i != fy) {
				prune(fx-col+k, fy-row+i, value);
			}
		}
	}
	return true;

}

// Search for a solution, returns NULL if no solution found
Board* Board::search(void) {

  // FILL IN
	//cout << *this << endl;
	//cout << "Startar search" << endl;
	for (int i = 0; i < 9; i++) {
		for (int j = 0; j < 9; j++) {
			//cout << "Change of position to " << i << "|" << j << endl;
			if (!fs[i][j]->assigned()) {
				for (int k = 1; k < 10; k++) {
					Board* b = new Board(*this);
					if (b->fs[i][j]->in(k)) {
						if (b->assign(i, j, k)) {
							//cout << "Assigning " << k << endl;
							Board* solution = b->search();
							if (solution) {
								if (solution != b) {
									delete b;
								}
								return solution;
							}
						}
					}
					delete b;
				}
				return NULL;
			}
		}
	}
	return this;

}


/*
 * Operations with trail
 *
 */

// Propagate that field (fx,fy) has been assigned
bool Board::propagate(int fx, int fy, Trail& t) {

  // FILL IN
	digit value = fs[fx][fy]->value();
	for (int i = 0; i < 9; i++) {
		if (i != fx) {
			prune(i, fy, value, t);
		}
		if (i != fy) {
			prune(fx, i, value, t);
		}
	}
	int col = fx % 3;
	int row = fy % 3;
	for (int i = 0; i < 3; i++) {
		for (int k = 0; k < 3; k++) {
			if (fx-col+k != fx && fy-row+i != fy) {
				prune(fx-col+k, fy-row+i, value, t);
			}
		}
	}
	return true;

}

// Search for a solution, returns false if no solution found
bool Board::search(Trail& t) {
bool ass;
  // FILL IN
	//cout << *this << endl;
	//cout << "Startar search" << endl;
	for (int i = 0; i < 9; i++) {
		for (int j = 0; j < 9; j++) {
			//cout << "Change of position to " << i << "|" << j << endl;
			if (!fs[i][j]->assigned()) {
				for (int k = 1; k < 10; k++) {
					if (fs[i][j]->in(k)) {
						t.mark();
						if (ass = assign(i, j, k, t)) {
							//cout << "Assigned " << k << " to " << i << "|" << j << " True=" << ass << endl;
							//cout << "_digits @ position: " << fs[i][j]->test() << endl;
							//cout << *this << endl;
							bool solution = search(t);
							if (solution) {
								return solution;
							}
							else {
								//cout << "Gör undo" << endl;
								t.undo();
							}
						}
					}
				}
				return false;
			}
		}
	}
	return true;
}


