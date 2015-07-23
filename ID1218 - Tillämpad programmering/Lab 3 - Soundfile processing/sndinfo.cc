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
	std::cout << "Filename: " << filename << std::endl;
	std::cout << "Samplerate: " << r.samplerate() << std::endl;
	std::cout << "Number of channels: " << r.channels() << std::endl;
	std::cout << "Number of samples: " << r.channels()*r.frames() << std::endl;
	int time = r.frames()/r.samplerate();
	int hours = time/3600;
	int minutes = (time%3600)/60;
	int seconds = ((time%3600)%60);
	std::cout << "Length (h/m/s): " << hours << ":" << minutes << ":" << seconds << std::endl;
	
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
	r.close();
	std::cout << "Highest volume level: " << highest << std::endl;
	return 0;
}
