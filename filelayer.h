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

// NOTE: Duplicated in lumpfile.bi
enum OPENBits {
	// FOR RANDOM (fixed sized records) not supported. Use load/storerecord() instead.
	FOR_BINARY =        0x0010000,  // default
	FOR_INPUT =         0x0020000,
	FOR_OUTPUT =        0x0040000,
	FOR_APPEND =        0x0080000,
	FOR_MASK =          0x00F0000,
	//For files, ACCESS ANY means try READ_WRITE, failing that use READ.
	//Which sounds like a misfeature to me, so let's default to ACCESS_READ_WRITE instead.
	ACCESS_ANY =        0x0100000,
	ACCESS_READ =       0x0200000,
	ACCESS_WRITE =      0x0400000,
	ACCESS_READ_WRITE = 0x0800000,  // default
	ACCESS_MASK =       0x0F00000,
	// Not implemented yet for hooked files, so no point using these
	ENCODING_ASCII =    0x1000000,  // default
	ENCODING_UTF8 =     0x2000000,
	ENCODING_UTF16 =    0x4000000,
	ENCODING_UTF32 =    0x8000000,
	ENCODING_MASK =     0xF000000,
	// LOCK not supported... in fact it's not even properly supported by FB!
	// However it could be added (since we already have file locking implemented in os.bi)
	// if it were useful.
	// LEN (record length) not supported.
};

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

	int OPENFILE(FBSTRING *filename, enum OPENBits openbits, int &fh);
}


#endif // FILELAYER_H
