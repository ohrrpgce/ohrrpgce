/* OHRRPGCE - low level file interface layer
 * Copyright 2011. Please read LICENSE.txt for GNU GPL details and disclaimer of liability
 */

#ifndef FILELAYER_H
#define FILELAYER_H

#include "fb/fb_stub.h"
#include <string>
#include "os.h"

using namespace std;


struct FileInfo {
	string name;
	bool dirty;

	FileInfo() : dirty(false) {};
};

extern "C" {

  typedef FBCALL int (*FnStringPredicate)(FBSTRING *filename);

  void set_OPEN_hook_filter(FnStringPredicate lumpfile_filter);
  void set_lump_updates_channel(IPCChannel channel);

}


#endif // FILELAYER_H
