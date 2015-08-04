/*
 *  Author:
 *     Christian Schulte <cschulte@kth.se>
 *
 *  This is part of lab 2-1 for ID1218.
 *
 *  This file contains the SndReader class.
 *
 */

#include <iostream>

extern "C" {
#include <sndfile.h>
}

#ifndef __SNDWRITER_H__
#define __SNDWRITER_H__

const int w_buffersize = 8192;

class SndWriter {
 private:
  SNDFILE*   file;   // Internal information on file
  SF_INFO    info;   // Accessible file information
  double*    buffer; // For buffering samples
  sf_count_t cur;    // Position of current frame in buffer
  sf_count_t end;    // End position of frames

 public:

  SndWriter(const char* filename, int samplerate, int channels) 
    : buffer(NULL) {
    /*
     * Setup the format, we insist on WAV with 16 bits samplesize
     *
     */
    info.samplerate = samplerate;
    info.channels   = channels;
    info.format     = SF_FORMAT_WAV | SF_FORMAT_PCM_16;
    /*
     * Open the file. Do poor error handling!
     *
     */
    file = sf_open(filename, SFM_WRITE, &info);
    if (file == NULL) {
      std::cout << "Opening file " << filename << " failed" 
		<< std::endl;
      return;
    }
    buffer = new double[w_buffersize*info.channels];
    // Start writing at location zero
    cur = 0;
    end = w_buffersize;
  }


  // Return the number of channels
  int channels() const {
    return info.channels;
  }


  // Return the samplerate
  int samplerate() const {
    return info.samplerate;
  }


  // Write the current sample from the current frame for channel
  void sample(int c, double s) {
    buffer[cur*channels()+c] = s;
  }


  // Move to next frame
  void next() {
    cur++;
    if (cur < end)
      return;
    // Buffer full, write buffer
    sf_writef_double(file, buffer, w_buffersize);
    // Start at zero again
    cur = 0;
  }


  // Write yet unwritten frames
  void close() {
    if (cur > 0)
      sf_writef_double(file, buffer, cur);
    sf_close(file);
  }


  ~SndWriter() {
    // Delete the buffer
    delete [] buffer;
  } 

 private:
  // Disallow copying and assignment
  SndWriter(const SndWriter&);
  SndWriter& operator=(const SndWriter&);
};

#endif


