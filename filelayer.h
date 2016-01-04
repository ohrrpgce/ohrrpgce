/* OHRRPGCE - low level file interface layer
 * Copyright 2011. Please read LICENSE.txt for GNU GPL details and disclaimer of liability
 */

#ifndef FILELAYER_H
#define FILELAYER_H

#include "fb/fb_stub.h"
#include <string>
#include "os.h"
#include "common.h"

using namespace std;


struct FileInfo {
	string name;
	bool dirty;

	FileInfo() : dirty(false) {};
};

extern "C" {

	typedef FBCALL boolint (*FnStringPredicate)(FBSTRING *filename);
	typedef FBCALL boolint (*FnOpenCallback)(FBSTRING *filename, boolint writable);

	void send_lump_modified_msg(const char *filename);
	boolint copyfile(FBSTRING *source, FBSTRING *destination);

	void set_OPEN_hook(FnOpenCallback lumpfile_filter, boolint lump_writes_allowed, IPCChannel *channel);
	void clear_OPEN_hook();

}


#endif // FILELAYER_H
