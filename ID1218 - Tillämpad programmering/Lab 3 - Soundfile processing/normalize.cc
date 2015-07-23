#include <iostream>
#include "sndreader.h"
#include "sndwriter.h"
#include "stdlib.h"

extern "C" {
#include <sndfile.h>
}

int main (int argc, const char** argv) {
	if (argc != 2) {
		    std::cout << "Usage: sndinfo <filename> " << std::endl;
    return 1;
	}
	const char* filename = argv[1];
	SndReader r(filename);
	SndWriter w("wavfiles/normalized.wav", r.samplerate(), r.channels());
	double highest = 0;
	while (r.next()) {
		for (int c = 0; c < r.channels(); c++) {
			if (r.sample(c) > highest) {
				highest = r.sample(c);
			} 
			else if (r.sample(c) < -highest) {
				highest = -r.sample(c);
			}
		}
	}
	r.seek_start();
		while (r.next()) {
		for (int c = 0; c < r.channels(); c++) {
			double s = r.sample(c);
			w.sample(c,s/highest);
		}
		w.next();
	}
	r.close();
	w.close();
	return 0;
}




