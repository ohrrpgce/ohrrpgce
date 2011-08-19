/* OHRRPGCE - low level file interface layer
 * Copyright 2011. Please read LICENSE.txt for GNU GPL details and disclaimer of liability
 */

#ifndef FILELAYER_H
#define FILELAYER_H

#include "fb/fb_stub.h"
#include <string>

using namespace std;


struct FileInfo {
	string name;
};

extern "C" {

  typedef FBCALL int (*FnStringPredicate)(FBSTRING *filename);

  // common.bas
  void debugc(char *msg, int errorlevel);

  void set_OPEN_hook_filter(FnStringPredicate lumpfile_filter);

}


#endif // FILELAYER_H
