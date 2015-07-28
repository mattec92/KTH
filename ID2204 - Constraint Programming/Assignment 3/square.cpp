//
//  square.cpp
//  id2204-assignment1
//
//  Created by Mattias Cederlund on 04/05/15.
//  Copyright (c) 2015 Mattias Cederlund. All rights reserved.
//

#include <stdio.h>
#include <gecode/driver.hh>
#include <gecode/int.hh>
#include <gecode/minimodel.hh>

using namespace Gecode;

int n;

class Square : public Script {
public:
    IntVar size;
    IntVarArray xCoords;
    IntVarArray yCoords;
    
    // Function returning the size of square at index i. (Largest square first)
    static int sizeOfSquare(int i) {
        return n - i;
    }
    
    //Returns the disallowed placement (gap) for square of given size.
    static int getDisallowedPlacement(int squareSize) {
        int disallowedPlacement = -1;
        
        if (squareSize == 2) {
            disallowedPlacement = 2;
        }
        else if (squareSize <= 3) {
            disallowedPlacement = 3;
        }
        else if (squareSize <= 4) {
            disallowedPlacement = 2;
        }
        else if (squareSize <= 8) {
            disallowedPlacement = 3;
        }
        else if (squareSize <= 11) {
            disallowedPlacement = 4;
        }
        else if (squareSize <= 17) {
            disallowedPlacement = 5;
        }
        else if (squareSize <= 21) {
            disallowedPlacement = 6;
        }
        else if (squareSize <= 29) {
            disallowedPlacement = 7;
        }
        else if (squareSize <= 34) {
            disallowedPlacement = 8;
        }
        else if (squareSize <= 44) {
            disallowedPlacement = 9;
        }
        else if (squareSize <= 45) {
            disallowedPlacement = 10;
        }
        
        return disallowedPlacement;
    }
    
    // n, nr of boxes, variables are from 0 to n. Should deduct index of array here.
    Square(const SizeOptions& opt) : Script(opt), xCoords(*this, opt.size()), yCoords(*this, opt.size()) {
        
        // Size must be at least n + (n - 1). Otherwise we cant fit both the largest and second largest.
        int min = n + (n - 1);
        
        // Max size of square is when all squares are on a single line.
        int max = 0;
        
        for (int i = 0; i < n; i++) {
            max += i;
        }
        
        // Initialize size with min and max values.
        size = IntVar(*this, min, max);
        
        // Initialize coordinate variables, values from 0 to max size.
        for (int i = 0; i < n; i++) {
            xCoords[i] = IntVar(*this, 0, max);
            yCoords[i] = IntVar(*this, 0, max);
        }
        
        // Constraints so squares wont escape its containing square.
        for (int i = 0; i < n; i++) {
            rel(*this, xCoords[i] + sizeOfSquare(i) <= size);
            rel(*this, yCoords[i] + sizeOfSquare(i) <= size);
        }
        
        // Constraints for non overlapping
        for (int i = 0; i < (n - 1); i++) {
            for (int j = i + 1; j < (n - 1); j++) { //Avoid some redundant constraints by initiating j = i + 1. Also never compare the square to itself.
                rel(*this, xCoords[i] + sizeOfSquare(i) <= xCoords[j] ||
                           xCoords[j] + sizeOfSquare(j) <= xCoords[i] ||
                           yCoords[i] + sizeOfSquare(i) <= yCoords[j] ||
                           yCoords[j] + sizeOfSquare(j) <= yCoords[i]);
            }
        }
        
        // Constraints for column/row consistency
        IntArgs squareSizes(n - 1);
        
        for (int i = 0; i < (n - 1); i++) {
            squareSizes[i] = sizeOfSquare(i);
        }
        
        // Loop through all columns
        for (int col = 0; col < size.max(); col++) {
            BoolVarArgs isCoveringCol(*this, (n - 1), 0, 1);
            // Loop through all squares
            for (int i = 0; i < (n - 1); i++) {
                // Coord of square inbetween column index - size + 1 and column index.
                dom(*this, xCoords[i], col - sizeOfSquare(i) + 1, col, isCoveringCol[i]);
            }
            //Get the size of squares covering column and add them. Must be less or equal to size of containing square.
            linear(*this, squareSizes, isCoveringCol, IRT_LQ, size);
        }
        
        // Loop through all rows
        for (int row = 0; row < size.max(); row++) {
            BoolVarArgs isCoveringRow(*this, (n - 1), 0, 1);
            // Loop through all squares
            for (int i = 0; i < (n - 1); i++) {
                dom(*this, yCoords[i], row - sizeOfSquare(i) + 1, row, isCoveringRow[i]);
            }
            linear(*this, squareSizes, isCoveringRow, IRT_LQ, size);
        }
        
        // Additional constraints
        
        // Problem decomposition - Size of container must be at least as large as the sum of all squares.
        rel(*this, (size * size) >= (n * (n + 1) * (2 * n + 1)) / 6);
        
        // Symmetry removal - Restrict coordinate of largest square to the first corner.
        rel(*this, xCoords[0] <= ((size - n) / 2));
        rel(*this, yCoords[0] <= ((size - n) / 2));
        rel(*this, yCoords[0] <= xCoords[0]);
        
        // Empty strip dominance, only disallowing placements away from top and left border.
        for (int i = 0; i < n; i++) {
            int disallowedPlacement = getDisallowedPlacement(sizeOfSquare(i));
            
            if (disallowedPlacement > 0) {
                rel(*this, xCoords[i], IRT_NQ, disallowedPlacement);
                rel(*this, yCoords[i], IRT_NQ, disallowedPlacement);
            }
        }
        
        // TODO: Forbidden gaps between squares
        
        // Ignoring size 1 squares
        // Not adding constaints for non overlapping or row/column consistency with the size 1 square. Using (n - 1) instead of n.
        
        // Branching, branch on size first.
        branch(*this, size, INT_VAL_MIN());
        //First assign x coordinates, then y coordinates.
        branch(*this, xCoords, INT_VAR_NONE(), INT_VAL_MIN());
        //INT_VAR_NONE = Choose first unassigned value, from smaller to larger squares.
        //INT_VAL_MIN = Try to place from left to right, and top to bottom.
        branch(*this, yCoords, INT_VAR_NONE(), INT_VAL_MIN());
    }
    
    // Constructor for cloning
    Square(bool share, Square& s) : Script(share,s) {
        size.update(*this, share, s.size);
        xCoords.update(*this, share, s.xCoords);
        yCoords.update(*this, share, s.yCoords);
    }
    
    // Perform copying during cloning
    virtual Space* copy(bool share) {
        return new Square(share, *this);
    }
    
    // Print solution
    virtual void print(std::ostream& os) const {
         std::cout << "Size of containing square: " << size << std::endl;
         std::cout << "Coordinates of squares: " << std::endl;
         for (int i = 0; i < n; i++) {
             std::cout << "(" << xCoords[i] << "," << yCoords[i] << ") size: " << (n - i) << std::endl;
         }
        std::cout << "Square of size 1 will use first available placement. Coordinate here is wrong." << std::endl;
        
        //TODO: Pretty print
    }
    
};


 int main(int argc, char* argv[]) {
     SizeOptions opt("Square");
     opt.iterations(1);
     opt.size(10); //Size parameter, number of squares to fit.
     n = opt.size(); //Setting n here so we can use it in the static sizeOfSquare function.
 
     opt.parse(argc,argv);
     Script::run<Square,DFS,SizeOptions>(opt);
     return 0;
 }
 