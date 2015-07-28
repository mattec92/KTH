
//
//  queens.cpp
//  id2204-assignment1
//
//  Created by Mattias Cederlund on 08/04/15.
//  Copyright (c) 2015 Mattias Cederlund. All rights reserved.
//

/*
 Testing propagator strength of distinct on example puzzle index 0:
 
 ICL_DEF:
    propagations: 1893
	nodes:        214
	failures:     103
	restarts:     0
	no-goods:     0
	peak depth:   11
 
 ICL_VAL
    propagations: 1893
	nodes:        214
	failures:     103
	restarts:     0
	no-goods:     0
	peak depth:   11
 
 ICL_BND
    propagations: 599
	nodes:        23
	failures:     10
	restarts:     0
	no-goods:     0
	peak depth:   5
 
 ICL_DOM
	propagations: 169
	nodes:        1
	failures:     0
	restarts:     0
	no-goods:     0
	peak depth:   0

 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>
#include "A1.cpp"

using namespace Gecode;

class Sudoku : public Script {
public:
    IntVarArray l;
    int puzzleIndex;
    
    //9 * 9 variables, allowed values 1 through 9.
    Sudoku(const SizeOptions& opt) : Script(opt), l(*this, 9 * 9, 1, 9) {
        puzzleIndex = opt.size();
        
        //Matrix to make use of int array easier.
        Matrix<IntVarArray> m(l, 9, 9);
        
        //Values in a row must be distinct.
        for (int i = 0; i < 9; i++) {
            distinct(*this, m.row(i), ICL_DOM);
        }
        
        //Values in a column must be distinct.
        for (int i = 0; i < 9; i++) {
            distinct(*this, m.col(i), ICL_DOM);
        }
        
        //Values in a square must be distinct.
        for (int i = 0; i < 9; i += 3) {
            for (int j = 0; j < 9; j += 3) {
                distinct(*this, m.slice(i, i + 3, j, j + 3), ICL_DOM);
            }
        }
        
        //Fill in values of given puzzle.
        for (int i = 0; i < 9; i++) {
            for (int j = 0; j < 9; j++) {
                int value = examples[puzzleIndex][i][j];
                if (value > 0) {
                    rel(*this, m(j, i), IRT_EQ, value);
                }
            }
        }
        
        // Branch over the matrix.
        branch(*this, l, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    }
    
    // Constructor for cloning
    Sudoku(bool share, Sudoku& s) : Script(share,s) {
        l.update(*this, share, s.l);
    }
    
    // Perform copying during cloning
    virtual Space* copy(bool share) {
        return new Sudoku(share,*this);
    }
    
    // Print solution
    virtual void print(std::ostream& os) const {
        for (int i = 0; i < 9*9; i++) {
            if (i != 0 && i % (3 * 9) == 0) {
                std::cout << std::endl << "---------------------" << std::endl;
            }
            else if (i % 9 == 0) {
                std::cout << std::endl;
            }
            else if (i % 3 == 0) {
                std::cout << "| ";
            }
            
            std::cout << l[i] << " ";
        }
        std::cout << std::endl;
        std::cout << std::endl;
    }
    
};


int main(int argc, char* argv[]) {
    SizeOptions opt("Sudoku");
    opt.iterations(500);
    opt.size(0); //Size parameter represents index of example sudoku to be solved.
    
    opt.parse(argc,argv);
    Script::run<Sudoku,DFS,SizeOptions>(opt);
    return 0;
}

