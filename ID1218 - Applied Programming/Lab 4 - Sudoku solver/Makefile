#
# Settings for compiler
#

CXX		= g++
CXXFLAGS	= -g
#CXXFLAGS	= -O2 -g -DNDEBUG
LDFLAGS		= 

#
# Other settings
#
RMF      	= rm -f

#
# Object files needed
#
SRC		= field.cc board.cc sudoku.cc
OBJ		= $(SRC:%.cc=%.o)

#
# Dependencies
#

all: sudoku.exe

field.o:	digit.h field.h
board.o:	digit.h field.h trail.h board.h puzzle.h
sudoku.o:	digit.h field.h trail.h board.h puzzle.h

sudoku.exe: $(OBJ)
	$(CXX) $(CXXFLAGS) $(OBJ) -o $@ $(LDFLAGS)

%.o: %.cc
	$(CXX) $(CXXFLAGS) -c $< -o $@