
#ifndef _tst_h_
#define _tst_h_

#ifndef MESSAGE
#define MESSAGE(foo) fprintf(stderr,"%s, line %d: %s", progname, __LINE__, foo)
#endif

#ifndef NULL
#define NULL 0
#endif

#include "brk.h"
#include <unistd.h>

#endif
