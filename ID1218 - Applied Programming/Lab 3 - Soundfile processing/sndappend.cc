#include <iostream>
#include "sndreader.h"
#include "sndwriter.h"
#include "stdlib.h"
#include <vector>

extern "C" {
#include <sndfile.h>
}

using namespace std;

int main(int argc, const char** argv) {
	int n = 100;
	int sr = 0;
	int noc = 0;
	double jump;
	vector<double>* vec = new vector<double>;
	//Gå igenom argumenten
	for (int i = 1; i <= argc; i++) { 
		if (i + 1 <= argc) {
			//Om -gap påträffas, läs in n, vilket är nästa efterföljande argument.
			if (string(argv[i]) == "-gap") { 
				n = atoi(argv[i+1]);
				i++;
				cout << "-gap found, n = " << n << endl;
			}
			//Om sr = 0, dvs om första fil
			else if (sr == 0) { 
				SndReader temp(argv[i]);
				//Sätt samplerate och number of channels som supportas
				sr = temp.samplerate(); 
				cout << "SR = " << sr << endl;
				noc = temp.channels();
				temp.close();
			}
		}
	}
	cout << "Skapar writer" << endl;
	//Skapa writer
	SndWriter w("appended.wav", sr, noc); 
	//Gå igenom alla argument
	for (int i = 1; i <= argc; i++) { 
		if (i + 1 <= argc) {
			cout << "Argument nr i = " << i << endl;
			//Om -gap påträffas, hoppa fram 1
			if (string(argv[i]) == "-gap") { 
				i++;
			}
			//Om inte -gap
			else if (argv[i] != "-gap") { 
				//Skapa reader för filen
				SndReader r(argv[i]); 
				cout << "Skapar reader för " << argv[i] << endl;
				//Om det finns sparade värden från slutet av föregående fil
				if ((*vec).size() > 0) { 
					cout << "Skapar övergång vec-size =" << (*vec).size() << endl;
					//Gå igenom alla channels
					for (int c = 0; c < noc; c++) { 
						cout << "Saved value = " << (*vec)[c] << " New value = " << r.sample(c) << endl;
						//Beräkna storlek på jump beroende på om sparade är positiv eller negativ
						if ((*vec)[c] >= 0) {
							jump = -((*vec)[c]-r.sample(c))/(sr*n/1000); 
							cout << "Jump saved positive = " << jump << " " << sr*n/1000 << endl;
						}
						else {
							jump = (r.sample(c)-(*vec)[c])/(sr*n/1000);	
							cout << "Jump saved negative = " << jump << " " << sr*n/1000 << endl;
						}
						//Loopa rätt antal hopp
						for (int i = 0; i < sr*n/1000; i++) { 
							//Skriv ny frame med gamla värdet gånger antal hopp
							w.sample(c,(*vec)[c]+i*jump); 
							w.next();
						}
					}
					//Töm vectorn på gamla värden.
					(*vec).clear();
				}
				//Om sr och noc matchar
				if (r.samplerate() == sr && r.channels() == noc) { 
					//Gå igenom alla samples och skriv till nya filen
					while (r.next()) { 
						for (int c = 0; c < r.channels(); c++) {
							double s = r.sample(c);
							w.sample(c,s);
						}
						w.next();
					}
					//Gå till sista framen
					r.seek_end(); 
					for (int c = 0; c < noc; c++) {
						//Spara ner värdet för alla channels till en vector
						(*vec).push_back(r.sample(c)); 
					}
				}
				//Om samplerate och antal channels inte matchar, returna med felmeddelande.
				else { 
					std::cout << "Files samplerate or number of channel does not match!" << std::endl;
					return 1;
				}
				//Stäng readern.
				r.close();
			}
		}
	}
	//Stäng writern
	w.close();
	return 0;




}
