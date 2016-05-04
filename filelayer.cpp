/* OHRRPGCE - low level file interface layer
 * Copyright 2011. Please read LICENSE.txt for GNU GPL details and disclaimer of liability
 */

//fb_stub.h (included by filelayer.h) MUST be included first, to ensure fb_off_t is 64 bit
#include "filelayer.h"
#include <cstdio>
#include <cassert>
#include <map>
#include "common.h"

// When quitting FB closes all files from within a destructor, so globals may have already
// been deleted. So it would be a bad idea to define openfiles as an object instead of pointer.
map<FB_FILE *, FileInfo> *openfiles;
typedef map<FB_FILE *, FileInfo>::iterator openfiles_iterator_t;

IPCChannel *lump_updates_channel;

bool lock_lumps = false;
bool allow_lump_writes;

// The function that tells whether to hook a file open
FnOpenCallback pfnLumpfileFilter;


const char *trimpath(const char *filename) {
	const char *p = filename, *ret = filename;
	while (*p) {
		if (ispathsep(*p))
			ret = p + 1;
		p++;
	}
	return ret;
}

void send_lump_modified_msg(const char *filename) {
	if (lump_updates_channel == NULL || *lump_updates_channel == NULL_CHANNEL)
		return;
	string buf = string("M ") + trimpath(filename) + "\n";
	channel_write(lump_updates_channel, buf.c_str(), buf.size());
	if (*lump_updates_channel == NULL_CHANNEL)
		//Automatically shut down the show
		clear_OPEN_hook();
}

int file_wrapper_close(FB_FILE *handle) {
	assert(openfiles->count(handle));
	FileInfo &info = (*openfiles)[handle];
	//debuginfo("closing %s, read-lock:%d write-lock:%d", info.name.c_str(), test_locked(info.name.c_str(), 0), test_locked(info.name.c_str(), 1));
	if (info.dirty && !allow_lump_writes) {
		// It's not really a great idea to call debug in here,
		// because we've been called from inside the rtlib, so
		// the global rtlib mutex is held. Luckily, FB uses recursive
		// mutexes, meaning the same thread can lock one multiple times.
		// Worse, if the user quits, FB will close all files,
		// calling us recursively! That's actually ok...
		info.dirty = false;  // Prevent recursion into debug()
		debug(errPromptBug, "Illegally wrote to protected file %s", info.name.c_str());
	}
	if (info.dirty) {
		//fprintf(stderr, "%s was dirty\n", info.name.c_str());
		send_lump_modified_msg(info.name.c_str());
	}
	//debuginfo("unlocking %s", info.name.c_str());
	unlock_file((FILE *)handle->opaque);  // Only needed on Windows
	openfiles->erase(handle);

	return fb_DevFileClose(handle);
}

int file_wrapper_seek(FB_FILE *handle, fb_off_t offset, int whence) {
	// Nothing here
	return fb_DevFileSeek(handle, offset, whence);
}

int file_wrapper_tell(FB_FILE *handle, fb_off_t *pOffset) {
	// Nothing here
	return fb_DevFileTell(handle, pOffset);
}

int file_wrapper_read(FB_FILE *handle, void *value, size_t *pValuelen) {
	// Nothing here
	return fb_DevFileRead(handle, value, pValuelen);
}

int file_wrapper_write(FB_FILE *handle, const void *value, size_t valuelen) {
	assert(openfiles->count(handle));
	FileInfo &info = (*openfiles)[handle];
	info.dirty = true;

	return fb_DevFileWrite(handle, value, valuelen);
}

// Modified version of hooks_dev_table in libfb_dev_file_open.c
static FB_FILE_HOOKS lumpfile_hooks = {
	fb_DevFileEof,
	file_wrapper_close,
	file_wrapper_seek,
	file_wrapper_tell,
	file_wrapper_read,
	fb_DevFileReadWstr,
	file_wrapper_write,
	fb_DevFileWriteWstr,
	fb_DevFileLock,
	fb_DevFileUnlock,
	fb_DevFileReadLine,
	fb_DevFileReadLineWstr,
	NULL,
	fb_DevFileFlush
};

void dump_openfiles() {
	debug(errDebug, "%d open files:", (int)openfiles->size());
	for (openfiles_iterator_t it = openfiles->begin(); it != openfiles->end(); ++it) {
		const char *fname = it->second.name.c_str();
		debug(errDebug, " %p (%s)", it->first, fname);
		if (lock_lumps)
			debug(errDebug, "   read-lock:%d write-lock:%d", test_locked(fname, 0), test_locked(fname, 1));
	}
}

// Replacement for fb_DevFileOpen().
int lump_file_opener(FB_FILE *handle, const char *filename, size_t filename_len) {
	//fprintf(stderr, "opening %p (%s).\n", handle, filename);

	// Just let the default file opener handle it (it does quite a lot of stuff, actually),
	// and then patch the file hooks table with wrappers
	int ret = fb_DevFileOpen(handle, filename, filename_len);
	// Note: fb_DevFileOpen changes FB_FILE_ACCESS_ANY to actual access state
	if (ret) return ret;

	handle->hooks = &lumpfile_hooks;
	assert(openfiles->count(handle) == 0);
	FileInfo &info = (*openfiles)[handle];
	info.name = string(filename);
	if (lock_lumps) {
		if (handle->access & FB_FILE_ACCESS_WRITE) {
			//debuginfo("write-locking %s", filename);
			lock_file_for_write((FILE *)handle->opaque, 1000);
		} else {
			//debuginfo("read-locking %s", filename);
			lock_file_for_read((FILE *)handle->opaque, 1000);
		}
		//debuginfo("read-lock:%d write-lock:%d", test_locked(filename, 0), test_locked(filename, 1));
	}
	return 0;
}

// This is a replacement for fb_FileOpen/fb_FileOpenEncod which is what plain OPEN with
// or without an ENCODING argument is translated to in -lang fb.
// Note that calling this function acts like the functional form OPEN(): when compiled with -exx
// it doesn't cause the program to abort if there's an error.
// Return 0 on success, 1 on error, 2 if file not found (FB_RTERROR_FILENOTFOUND)
FBCALL int OPENFILE(FBSTRING *filename, enum OPENBits openbits, int &fnum) {
	unsigned int mode, access;
	FB_FILE_ENCOD encod;

	switch(openbits & FOR_MASK) {
		case 0:  // Default
		case FOR_BINARY:
			mode = FB_FILE_MODE_BINARY;
			break;
		case FOR_INPUT:
			mode = FB_FILE_MODE_INPUT;
			break;
		case FOR_OUTPUT:
			mode = FB_FILE_MODE_OUTPUT;
			break;
		case FOR_APPEND:
			mode = FB_FILE_MODE_APPEND;
			break;
		default:
			fatal_error("OPENFILE: bad flags %x", openbits);
			return 1;
	}

	switch(openbits & ACCESS_MASK) {
		case ACCESS_ANY:
			access = FB_FILE_ACCESS_ANY;
			break;
		case ACCESS_READ:
			access = FB_FILE_ACCESS_READ;
			break;
		case ACCESS_WRITE:
			access = FB_FILE_ACCESS_WRITE;
			break;
		case 0:  // Default (in contrast to FB, which defaults to ANY)
		case ACCESS_READ_WRITE:
			access = FB_FILE_ACCESS_READWRITE;
			break;
		default:
			fatal_error("OPENFILE: bad flags %x", openbits);
			return 1;
	}

	switch(openbits & ENCODING_MASK) {
		case 0:  // Default
		case ENCODING_ASCII:
			encod = FB_FILE_ENCOD_ASCII;
			break;
		case ENCODING_UTF8:
			encod = FB_FILE_ENCOD_UTF8;
			break;
		case ENCODING_UTF16:
			encod = FB_FILE_ENCOD_UTF16;
			break;
		case ENCODING_UTF32:
			encod = FB_FILE_ENCOD_UTF32;
			break;
		default:
			fatal_error("OPENFILE: bad flags %x", openbits);
			return 1;
	}

	if ((fnum = fb_FileFree()) == 0) {
		fatal_error("OPEN_lump: too many open files");
		return 1;
	}

	FnFileOpen fnOpen;
	// This is the correct test
	//bool writable = access != FB_FILE_ACCESS_READ;
	// This tests only for explicit ACCESS WRITE, which is much more likely to be an error
	bool writable = openbits & (ACCESS_WRITE | ACCESS_READ_WRITE);
	if (pfnLumpfileFilter && pfnLumpfileFilter(filename, writable ? -1 : 0)) {
		if (encod != FB_FILE_ENCOD_ASCII) {
			fatal_error("OPENFILE: ENCODING not implemented for hooked files");
			return 1;
		}
		fnOpen = lump_file_opener;
	} else {
		if (encod == FB_FILE_ENCOD_ASCII)
			fnOpen = fb_DevFileOpen;
		else
			fnOpen = fb_DevFileOpenEncod;
	}

	return fb_FileOpenVfsEx(FB_FILE_TO_HANDLE(fnum), filename, mode, access,
				FB_FILE_LOCK_SHARED, 0, encod, fnOpen);
}

// A replacement for FB's filecopy which sends modification messages and deals with open files
// NOTE: return values are opposite to FileCopy (true for success)
boolint copyfile(FBSTRING *source, FBSTRING *destination) {
	int ret = copy_file_replacing(source->data, destination->data);
	if (ret && pfnLumpfileFilter && pfnLumpfileFilter(destination, -1)) {
		send_lump_modified_msg(destination->data);
	}
	return ret;
}

void set_OPEN_hook(FnOpenCallback lumpfile_filter, boolint lump_writes_allowed, IPCChannel *channel) {
	if (!openfiles)
		openfiles = new map<FB_FILE *, FileInfo>;
	pfnLumpfileFilter = lumpfile_filter;
#ifndef _WIN32
	lock_lumps = true;
#endif
	allow_lump_writes = lump_writes_allowed;
	lump_updates_channel = channel;
}

void clear_OPEN_hook() {
	lock_lumps = false;
	allow_lump_writes = true;
	lump_updates_channel = NULL_CHANNEL;
}
