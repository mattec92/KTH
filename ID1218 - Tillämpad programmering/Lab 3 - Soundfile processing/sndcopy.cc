/*
 *  Author:
 *     Christian Schulte <cschulte@kth.se>
 *
 *  This is part of lab 2-1 for ID1218.
 *
 *  This is just a small example for handling commandline
 *  arguments and how to use the SndReader and SndWriter
 *  classes.
 *
 */

#include <iostream>
#include "sndreader.h"
#include "sndwriter.h"

using namespace std;

int main(int argc, const char** argv) {
  /*
   * Check whether we have exactly two arguments, argc
   * is number of arguments plus one!
   *
   */
  if (argc != 3) {
    cerr << "Usage: sndcopy <infile> <outfile>" << endl;
    return 1;
  }

  // The arguments are stored at argv[1] and argv[2]
  const char* i_name = argv[1];
  const char* o_name = argv[2];
  cout << "Copying " 
       << i_name << " to " 
       << o_name << "..." << endl;

  // Create reader
  SndReader r(i_name);
  // Create writer
  SndWriter w(o_name, r.samplerate(), r.channels());

  // Same basic info on input file
  cout << "\tSamplerate: " << r.samplerate() << endl
       << "\tChannels:   " << r.channels() << endl
       << "\tFrames:     " << r.frames() << endl;


  do {
    // For all channels do...
    for (int c = 0; c < r.channels(); c++) {
      // Read sample
      double s = r.sample(c);
      // Write sample
      w.sample(c,s);
    }
    // Move to next sample for writing
    w.next();
  } while (r.next());


  // Close the files
  r.close();
  w.close();

  return 0;
}
