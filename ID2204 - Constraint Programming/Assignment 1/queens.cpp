
//
//  queens.cpp
//  id2204-assignment1
//
//  Created by Mattias Cederlund on 08/04/15.
//  Copyright (c) 2015 Mattias Cederlund. All rights reserved.
//

/*
 
 NOTE
    To run the n-queens solver, you need to uncomment the main function in this file 
    and comment out the one in sudoku.cpp.
 
 Branching
    branch(*this, q, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
        runtime:      0.001 (1.911 ms)
        solutions:    1
        propagations: 1189
        nodes:        53
        failures:     21
        restarts:     0
        no-goods:     0
        peak depth:   16
 
    branch(*this, q, INT_VAR_SIZE_MIN(), INT_VAL_MAX());
        runtime:      0.004 (4.266 ms)
        solutions:    1
        propagations: 1233
        nodes:        45
        failures:     21
        restarts:     0
        no-goods:     0
        peak depth:   5
 
    branch(*this, q, INT_VAR_RND(rand()), INT_VAL_RND(rand()));
        runtime:      0.003 (3.939 ms)
        solutions:    1
        propagations: 403
        nodes:        12
        failures:     3
        restarts:     0
        no-goods:     0
        peak depth:   7
 
    branch(*this, q, INT_VAR_RND(rand()), INT_VAL_MAX());
        runtime:      0.004 (4.253 ms)
        solutions:    1
        propagations: 388
        nodes:        14
        failures:     5
        restarts:     0
        no-goods:     0
        peak depth:   4
 
 Because there the chance of a variables correct value is 0 rather than 1, 
 one would think this would give a better solution, but they seem quite like each other.
 
 Using random branching decreased the size of the search tree and the number
 of propagations, yet the execution time was larger.
 
 As a last attempt I tried with random varaible and maximum value, which decreased both number of propagations
 and the peak depth slightly, but not much.
 
 
 Pros compared to "standard model"
    Less values per variable.
    Less constraints, (6n + 1) (With at least 2 overlapping in current implementation), instead of 3/2*n*(n-1).
 
 
 Cons compared to "standard model"
    Higher number of variables.
    Number of combinations a lot higher, 2^(n^2) instead of n^n;
 
 */

#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

class Queens : public Script {
public:
    // Position of queens on boards
    IntVarArray q;
    int n;

    //n * n variables, allowed values 0 and 1.
    Queens(const SizeOptions& opt) : Script(opt), q(*this, opt.size() * opt.size(), 0, 1) {
        n = opt.size();
      
        //Matrix representation of the board.
        Matrix<IntVarArray> m(q, opt.size(), opt.size());
        
        //Total number of queens must be n.
        count(*this, q, 1, IRT_EQ, n);
      
        //Only one queen per row.
        for (int i = 0; i < n; i++) {
            count(*this, m.row(i), 1, IRT_EQ, 1);
        }
        
        //Only one queen per column.
        for (int i = 0; i < n; i++) {
            count(*this, m.col(i), 1, IRT_EQ, 1);
        }
        
        //Only one queen per diagonal.
        IntVarArray diagonal(*this, n);
        
        std::cout << "Left side, going down diagonals" << std::endl;
        //Diagonals, left side, going down
        for (int i = 0; i < n; i++) {
            std::cout << "New diagonal" << std::endl;
            
            //Set whole diagonal array to 0s.
            for (int k = 0; k < n; k++) {
                diagonal[k] = IntVar(*this, 0, 0);
            }
            
            for (int j = 0; j < n - i; j++) {
                std::cout << "(" << i + j << "," << j << ")" << std::endl;
                diagonal[j] = m(i + j, j);
            }
            
            count(*this, diagonal, 1, IRT_LQ, 1);
        }
        
        std::cout << "Left side, going up diagonals" << std::endl;
        //Diagonals, left side, going up
        for (int i = 0; i < n; i++) {
            std::cout << "New diagonal" << std::endl;
            
            //Set whole diagonal array to 0s.
            for (int k = 0; k < n; k++) {
                diagonal[k] = IntVar(*this, 0, 0);
            }

            for (int j = 0; j <= i; j++) {
                std::cout << "(" << i - j << "," << j << ")" << std::endl;
                diagonal[j] = m(i - j, j);
            }
            
            count(*this, diagonal, 1, IRT_LQ, 1);
        }
        
        std::cout << "Right side, going down diagonals" << std::endl;
        //Diagonals, right side, going down
        for (int i = 0; i < n; i++) {
            std::cout << "New diagonal" << std::endl;
            
            //Set whole diagonal array to 0s.
            for (int k = 0; k < n; k++) {
                diagonal[k] = IntVar(*this, 0, 0);
            }
            
            for (int j = (n - 1); j >= i; j--) {
                std::cout << "(" << (n - 1) + i - j << "," << j << ")" << std::endl;
                diagonal[j] = m((n - 1) + i - j, j);
            }
            
            count(*this, diagonal, 1, IRT_LQ, 1);
        }
        
        std::cout << "Right side, going up diagonals" << std::endl;
        //Diagonals, right side, going up
        for (int i = 0; i < n; i++) {
            std::cout << "New diagonal" << std::endl;
            
            //Set whole diagonal array to 0s.
            for (int k = 0; k < n; k++) {
                diagonal[k] = IntVar(*this, 0, 0);
            }
            
            for (int j = (n - 1) - i; j < n; j++) {
                std::cout << "(" << i + j - (n - 1) << "," << j << ")" << std::endl;
                diagonal[j] = m(i + j - (n - 1), j);
            }
            
            count(*this, diagonal, 1, IRT_LQ, 1);
        }
      
        // Branch over the matrix.
        branch(*this, q, INT_VAR_SIZE_MIN(), INT_VAL_MIN());
    }

    // Constructor for cloning \a s
    Queens(bool share, Queens& s) : Script(share,s) {
        q.update(*this, share, s.q);
    }

    // Perform copying during cloning
    virtual Space* copy(bool share) {
        return new Queens(share,*this);
    }

    // Print solution
    virtual void print(std::ostream& os) const {
        std::cout << "Sometimes the board wont get printed, lets hope it will this time." << std::endl << std::endl;
        
        for (int i = 0; i < n * n; i++) {
            if (i != 0 && i % n == 0) {
                std::cout << std::endl;
            }
            std::cout << q[i] << " ";
        }
        
        std::cout << std::endl;
    }
    
};


int main(int argc, char* argv[]) {
    SizeOptions opt("Queens");
    opt.iterations(500);
    opt.size(8); //Size parameter size of chess board.
 
    opt.parse(argc,argv);
    Script::run<Queens,DFS,SizeOptions>(opt);
    return 0;
}

