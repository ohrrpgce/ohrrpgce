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
  typedef FBCALL int (*FnOpenCallback)(FBSTRING *filename, int writable);

  void send_lump_modified_msg(const char *filename);
  int copyfile(FBSTRING *source, FBSTRING *destination);
  void set_OPEN_hook_filter(FnOpenCallback lumpfile_filter, int lump_writes_allowed);
  void set_lump_updates_channel(IPCChannel channel);

}


#endif // FILELAYER_H
