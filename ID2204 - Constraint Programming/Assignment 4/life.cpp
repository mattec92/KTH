//
//  life.cpp
//  id2204-assignment1
//
//  Created by Mattias Cederlund on 03/06/15.
//  Copyright (c) 2015 Mattias Cederlund. All rights reserved.
//

/*
 Solutions found for n = 8 (before optimization):
 
 Printout of solution:
 0 0 | 0 0 0 0 0 0 0 0 | 0 0
 0 0 | 0 0 0 0 0 0 0 0 | 0 0
 ---------------------------
 0 0 | 1 1 1 1 0 1 1 0 | 0 0
 0 0 | 1 0 0 1 0 1 1 0 | 0 0
 0 0 | 1 0 0 1 0 0 0 0 | 0 0
 0 0 | 1 1 0 1 0 1 1 0 | 0 0
 0 0 | 1 0 0 1 0 1 0 1 | 0 0
 0 0 | 1 0 1 1 0 0 0 1 | 0 0
 0 0 | 1 0 1 0 0 1 0 1 | 0 0
 0 0 | 1 1 0 0 1 1 1 0 | 0 0
 ---------------------------
 0 0 | 0 0 0 0 0 0 0 0 | 0 0
 0 0 | 0 0 0 0 0 0 0 0 | 0 0
 Live count: 34
 
 Printout of solution:
 0 0 | 0 0 0 0 0 0 0 0 | 0 0
 0 0 | 0 0 0 0 0 0 0 0 | 0 0
 ---------------------------
 0 0 | 1 1 1 1 0 1 1 0 | 0 0
 0 0 | 1 0 0 1 0 1 0 1 | 0 0
 0 0 | 1 0 0 1 0 1 0 1 | 0 0
 0 0 | 1 1 0 1 0 1 1 0 | 0 0
 0 0 | 1 0 0 1 0 0 0 0 | 0 0
 0 0 | 1 0 1 0 1 1 1 0 | 0 0
 0 0 | 1 0 1 0 1 0 0 1 | 0 0
 0 0 | 1 1 0 0 0 1 1 1 | 0 0
 ---------------------------
 0 0 | 0 0 0 0 0 0 0 0 | 0 0
 0 0 | 0 0 0 0 0 0 0 0 | 0 0
 Live count: 35
 
 Printout of solution:
 0 0 | 0 0 0 0 0 0 0 0 | 0 0
 0 0 | 0 0 0 0 0 0 0 0 | 0 0
 ---------------------------
 0 0 | 1 1 1 1 0 1 1 0 | 0 0
 0 0 | 1 0 0 1 0 1 0 1 | 0 0
 0 0 | 1 0 0 1 0 1 0 1 | 0 0
 0 0 | 1 1 0 1 0 1 0 1 | 0 0
 0 0 | 1 0 0 1 0 1 0 1 | 0 0
 0 0 | 1 0 1 1 0 1 0 1 | 0 0
 0 0 | 1 0 0 1 0 1 0 1 | 0 0
 0 0 | 1 1 1 0 0 0 1 1 | 0 0
 ---------------------------
 0 0 | 0 0 0 0 0 0 0 0 | 0 0
 0 0 | 0 0 0 0 0 0 0 0 | 0 0
 Live count: 37
 
 Printout of solution:
 0 0 | 0 0 0 0 0 0 0 0 | 0 0
 0 0 | 0 0 0 0 0 0 0 0 | 0 0
 ---------------------------
 0 0 | 1 1 1 1 1 1 1 1 | 0 0
 0 0 | 1 0 0 0 0 0 0 1 | 0 0
 0 0 | 1 0 1 1 1 1 0 1 | 0 0
 0 0 | 1 0 1 0 0 1 0 1 | 0 0
 0 0 | 1 0 1 0 1 0 0 1 | 0 0
 0 0 | 1 0 1 0 1 1 0 1 | 0 0
 0 0 | 1 0 1 0 1 0 0 1 | 0 0
 0 0 | 1 1 0 0 0 1 1 1 | 0 0
 ---------------------------
 0 0 | 0 0 0 0 0 0 0 0 | 0 0
 0 0 | 0 0 0 0 0 0 0 0 | 0 0
 Live count: 38
 
 Printout of solution:
 0 0 | 0 0 0 0 0 0 0 0 | 0 0
 0 0 | 0 0 0 0 0 0 0 0 | 0 0
 ---------------------------
 0 0 | 1 1 1 1 1 1 1 1 | 0 0
 0 0 | 1 0 0 0 0 0 0 1 | 0 0
 0 0 | 1 0 1 1 1 1 0 1 | 0 0
 0 0 | 1 0 1 0 0 1 0 1 | 0 0
 0 0 | 1 0 1 0 0 1 0 1 | 0 0
 0 0 | 1 0 1 1 1 1 0 1 | 0 0
 0 0 | 1 0 0 0 0 0 0 1 | 0 0
 0 0 | 1 1 1 1 1 1 1 1 | 0 0
 ---------------------------
 0 0 | 0 0 0 0 0 0 0 0 | 0 0
 0 0 | 0 0 0 0 0 0 0 0 | 0 0
 Live count: 40
 
 
 Initial
 propagators: 512
 branchers:   1
 
 Summary
 runtime:      27.270 (27270.500 ms)
 solutions:    5
 propagations: 142298699
 nodes:        1977180
 failures:     988585
 restarts:     0
 no-goods:     0
 peak depth:   37
 
 
 Solutions found for n = 9 (after optimization):
 
 Note: Running the optimized version for n = 8 wont give as good solutions.
 The best solution found then is density 38.
 To enable/disable optimization, set USE_OPTIMIZATION appropriately.
 
 Printout of solution:
 0 0 | 0 0 0 0 0 0 0 0 0 | 0 0
 0 0 | 0 0 0 0 0 0 0 0 0 | 0 0
 -----------------------------
 0 0 | 1 1 1 1 0 1 1 1 1 | 0 0
 0 0 | 1 0 0 1 0 1 0 0 1 | 0 0
 0 0 | 1 0 0 1 0 1 0 0 1 | 0 0
 0 0 | 1 1 0 1 0 1 1 0 1 | 0 0
 0 0 | 1 0 0 1 0 1 0 0 1 | 0 0
 0 0 | 1 0 1 1 0 1 0 0 1 | 0 0
 0 0 | 1 0 1 0 0 1 1 0 1 | 0 0
 0 0 | 1 0 1 0 0 1 0 0 1 | 0 0
 0 0 | 1 1 0 0 0 0 1 1 1 | 0 0
 -----------------------------
 0 0 | 0 0 0 0 0 0 0 0 0 | 0 0
 0 0 | 0 0 0 0 0 0 0 0 0 | 0 0
 Live count: 45
 
 Printout of solution:
 0 0 | 0 0 0 0 0 0 0 0 0 | 0 0
 0 0 | 0 0 0 0 0 0 0 0 0 | 0 0
 -----------------------------
 0 0 | 1 1 1 1 0 1 1 1 1 | 0 0
 0 0 | 1 0 0 1 0 1 0 0 1 | 0 0
 0 0 | 1 0 0 1 0 0 1 0 1 | 0 0
 0 0 | 1 1 0 1 1 0 1 0 1 | 0 0
 0 0 | 1 0 0 1 0 0 1 0 1 | 0 0
 0 0 | 1 0 1 0 0 1 1 0 1 | 0 0
 0 0 | 1 0 1 1 0 1 0 0 1 | 0 0
 0 0 | 1 0 1 0 0 1 0 1 0 | 0 0
 0 0 | 1 1 0 0 1 1 0 1 1 | 0 0
 -----------------------------
 0 0 | 0 0 0 0 0 0 0 0 0 | 0 0
 0 0 | 0 0 0 0 0 0 0 0 0 | 0 0
 Live count: 46
 
 Printout of solution:
 0 0 | 0 0 0 0 0 0 0 0 0 | 0 0
 0 0 | 0 0 0 0 0 0 0 0 0 | 0 0
 -----------------------------
 0 0 | 1 1 1 1 0 1 1 1 1 | 0 0
 0 0 | 1 0 0 1 0 1 0 0 1 | 0 0
 0 0 | 1 0 0 1 0 1 0 0 1 | 0 0
 0 0 | 1 1 0 1 0 1 1 0 1 | 0 0
 0 0 | 1 0 0 1 0 1 0 0 1 | 0 0
 0 0 | 1 0 1 1 0 1 0 1 1 | 0 0
 0 0 | 1 0 1 0 0 1 0 1 0 | 0 0
 0 0 | 1 0 0 1 0 1 0 0 1 | 0 0
 0 0 | 1 1 1 1 0 1 1 1 1 | 0 0
 -----------------------------
 0 0 | 0 0 0 0 0 0 0 0 0 | 0 0
 0 0 | 0 0 0 0 0 0 0 0 0 | 0 0
 Live count: 48
 
 
 Initial
 propagators: 657
 branchers:   1
 
 Summary
 runtime:      27.444 (27444.454 ms)
 solutions:    3
 propagations: 122739451
 nodes:        1589121
 failures:     794555
 restarts:     0
 no-goods:     0
 peak depth:   52
 
 */

#include <stdio.h>
#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

#define USE_OPTIMIZATION true

using namespace Gecode;

int n2; //Using n2 as variable name because of naming collision...

class Life : public Script {
public:
    IntVarArray cells;
    IntVarArray threeByThrees;
    
    Life(const SizeOptions& opt) : Script(opt), cells(*this, (opt.size() + 4) * (opt.size() + 4), 0 , 1),
    threeByThrees(*this, ((opt.size() / 3) * (opt.size() / 3)), 0, 6) {
        
        Matrix<IntVarArray> cellMatrix(cells, (opt.size() + 4), (opt.size() + 4));
        
        //Ensure border of 2 is set to dead.
        count(*this, cellMatrix.row(0), 1, IRT_EQ, 0);
        count(*this, cellMatrix.row(1), 1, IRT_EQ, 0);
        count(*this, cellMatrix.col(0), 1, IRT_EQ, 0);
        count(*this, cellMatrix.col(1), 1, IRT_EQ, 0);
        count(*this, cellMatrix.row(n2 + 4 - 1), 1, IRT_EQ, 0);
        count(*this, cellMatrix.row(n2 + 4 - 2), 1, IRT_EQ, 0);
        count(*this, cellMatrix.col(n2 + 4 - 1), 1, IRT_EQ, 0);
        count(*this, cellMatrix.col(n2 + 4 - 2), 1, IRT_EQ, 0);
        
        //Index variable for the three by three array.
        int threeByThreeIndex = 0;
        
        //Loop through all cells.
        for (int i = 2; i < n2 + 2; i++) {
            for (int j = 2; j < n2 + 2; j++) {
                //Put all neighbours in a separate array.
                IntVarArray neighbours(*this, 8, 0, 1);
                neighbours[0] = cellMatrix(i - 1, j - 1);   //Top left
                neighbours[1] = cellMatrix(i - 1, j);       //Above
                neighbours[2] = cellMatrix(i - 1, j + 1);   //Top right
                neighbours[3] = cellMatrix(i, j - 1);       //Left
                neighbours[4] = cellMatrix(i, j + 1);       //Right
                neighbours[5] = cellMatrix(i + 1, j - 1);   //Bottom left
                neighbours[6] = cellMatrix(i + 1, j);       //Below
                neighbours[7] = cellMatrix(i + 1, j + 1);   //Bottom right
                
                //If cell is alive, sum of neighbours should be 2 or 3 to stay alive.
                //If cell is dead, sum of neighbours should not be 3 to stay dead.
                rel(*this, ((cellMatrix(i, j) == 1 && sum(neighbours) >=2 && sum(neighbours) <= 3)) ||
                    (cellMatrix(i, j) == 0 && sum(neighbours) != 3));
                
                if (USE_OPTIMIZATION) {
                    //Find 3x3 squares, enforce a maximum of 6 live cells in the square.
                    if (i % 3 == 2 && j % 3 == 2 && i < n2 && j < n2) {
                        rel(*this, threeByThrees[threeByThreeIndex++] == sum(cellMatrix.slice(i, i + 3, j, j + 3)));
                    }
                }
            }
        }
        
        //Branch over cells, try high values first.
        branch(*this, cells, INT_VAR_NONE(), INT_VAL_MAX());
    }
    
    //Constrain solutions to have more alive cells than the previous.
    virtual void constrain(const Space& b) {
        const Life& life = static_cast<const Life&>(b);
        
        rel(*this, sum(cells) > sum(life.cells));
        
        if (USE_OPTIMIZATION) {
            rel(*this, sum(threeByThrees) > sum(life.threeByThrees));
        }
    }
    
    // Constructor for cloning
    Life(bool share, Life& s) : Script(share,s) {
        cells.update(*this, share, s.cells);
        
        if (USE_OPTIMIZATION) {
            threeByThrees.update(*this, share, s.threeByThrees);
        }
    }
    
    // Perform copying during cloning
    virtual Space* copy(bool share) {
        return new Life(share, *this);
    }
    
    // Print solution
    virtual void print(std::ostream& os) const {
        Matrix<IntVarArray> cellMatrix(cells, (n2 + 4), (n2 + 4));
        
        std::cout << "Printout of solution:" << std::endl;
        
        int liveCount;
        
        for (int i = 0; i < n2 + 4; i++) {
            for (int j = 0; j < n2 + 4; j++) {
                if (j == 0 && (i == 2 || i == n2 + 2)) {
                    for (int k = 0; k < n2 + 4; k++) {
                        std::cout << "--";
                    }
                    std::cout << "---" << std::endl;
                }
                if (j == 2 || j == (n2 + 2)) {
                    std::cout << "| ";
                }
                std::cout << cellMatrix(i, j) << " ";
                
                liveCount+= cellMatrix(i, j).val();
            }
            std::cout << std::endl;
        }
        
        std::cout << "Live count: " << liveCount << std::endl << std::endl;
    }
    
};


int main(int argc, char* argv[]) {
    SizeOptions opt("Life");
    opt.solutions(0);
    opt.iterations(1);
    opt.size(9); //Size of problem
    n2 = opt.size();
    
    opt.parse(argc,argv);
    Script::run<Life,BAB,SizeOptions>(opt);
    return 0;
}
