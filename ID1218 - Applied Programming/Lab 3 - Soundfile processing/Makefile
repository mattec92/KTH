#
# Settings for compiler
#

CXX		= g++
CXXFLAGS	= -g
LDFLAGS		= -L/usr/local/lib -lsndfile

#
# Other settings
#
RMF      	= rm -f

#
# Programs to be created...
# 
PROGSRC 	= normalize.cc
PROGEXE		= $(PROGSRC:%.cc=%.exe)

all: $(PROGEXE)

%.exe: %.cc sndreader.h sndwriter.h
	$(CXX) $(CXXFLAGS) $< -o $@ $(LDFLAGS)

