/*
 *  Author:
 *     Christian Schulte <cschulte@kth.se>
 *
 *  This is part of lab 2-1 for ID1218.
 *
 *  This file contains the SndReader class. Please search for
 *       <<COMPLETE HERE>>
 *  At these places you have to add some code!
 *
 */

#include <iostream>

// Import a C header file
extern "C" {
#include <sndfile.h>
}

#ifndef __SNDREADER_H__
#define __SNDREADER_H__

const int r_buffersize = 8192;

class SndReader {
 private:
  SNDFILE*   file;   // Internal information on file
  SF_INFO    info;   // Accessible file information
  double*    buffer; // For buffering samples
  sf_count_t cur;    // Position of current frame in buffer
  sf_count_t end;    // End position of frames


  // Refresh the buffer and set "cur" and "end" accordingly!
  void refresh_buffer() {
    /*
     * sf_readf_double tries to fill the entire buffer with
     * "r_buffersize" frames and returns the actual number of 
     * frames returned. However, if there are less than
     * "r_buffersize" frames available for reading, it returns
     * a number which is less than "r_buffersize".
     *
     */
    end = sf_readf_double(file, buffer, r_buffersize);
    /*
     * This returned number tells us how many frames are actually
     * in the buffer: it is stored in "end". We continue at the
     * first frame: that's frame number zero.
     *
     */
    cur = 0;
    /*
     * Keep in mind that "end" might also be equal to zero: telling
     * us that we have reached the end of the file!
     *
     */
  }

 public:

  SndReader(const char* filename) 
    : buffer(NULL) {
    // Open the file for reading
    file = sf_open(filename, SFM_READ, &info);
    // Do poor error handling...
    if (file == NULL) {
      std::cout << "Opening file " << filename << " failed" 
		<< std::endl;
      return;
    }
    // Allocate buffer capable of storing all samples, that is
    buffer = new double[r_buffersize*info.channels];
    refresh_buffer();
  }


  // Return the total number of frames
  sf_count_t frames() const {
    return info.frames;
  }


  // Return the number of channels
  int channels() const {
    return info.channels;
  }


  // Return the samplerate
  int samplerate() const {
    return info.samplerate;
  }


  // Return the current sample from the current frame for channel c
  double sample(int c) const {
    return buffer[cur*channels()+c];
  }


  // Moves to next frame, returns false, iff no more frames available
  bool next() {
    /*
     * In order to complete, you have to advance cur and check whether
     * the buffer needs to be refreshed. Also, do not forget to return
     * true or false dependening on whether the end of the file has been 
     * reached.
     *
     */
	cur++;
	if (cur < end) {
		return true;	
	}
	refresh_buffer();
	if (end == 0) {
		return false;
	}
	else {
		return true;
	}
  }

  void seek(sf_count_t f) {
    // Moves to frame f
    sf_seek(file, f, SEEK_SET);
    // Also refresh the buffer accordingly
    refresh_buffer();
  }


  void seek_start() {
    // Goes to first frame
    seek(0);
  }


  void seek_end() {
    // Goes to the last frame
    seek(frames()-1);
  }


  void close() {
    sf_close(file);
  }


  ~SndReader() {
    delete [] buffer;
  } 


 private:
  // Disallow copying and assignment
  SndReader(const SndReader&);
  SndReader& operator=(const SndReader&);
};

#endif


