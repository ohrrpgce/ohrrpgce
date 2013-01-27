/* OHRRPGCE - low level file interface layer
 * Copyright 2011. Please read LICENSE.txt for GNU GPL details and disclaimer of liability
 */

//fb_stub.h (included by filelayer.h) MUST be included first, to ensure fb_off_t is 64 bit
#include "filelayer.h"
#include <cstdio>
#include <cassert>
#include <map>
#include "common.h"

map<FB_FILE *, FileInfo> openfiles;
typedef map<FB_FILE *, FileInfo>::iterator openfiles_iterator_t;

IPCChannel *lump_updates_channel;

bool lock_lumps = false;
bool allow_lump_writes;

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
	assert(openfiles.count(handle));
	FileInfo &info = openfiles[handle];
	//debuginfo("closing %s, read-lock:%d write-lock:%d", info.name.c_str(), test_locked(info.name.c_str(), 0), test_locked(info.name.c_str(), 1));
	if (info.dirty && !allow_lump_writes) {
		// It's not really safe to call debug in here
		debug(errBug, "illegally wrote to %s", info.name.c_str());
	}
	if (info.dirty) {
		//fprintf(stderr, "%s was dirty\n", info.name.c_str());
		send_lump_modified_msg(info.name.c_str());
	}
	//debuginfo("unlocking %s", info.name.c_str());
	unlock_file((FILE *)handle->opaque);  // Only needed on Windows
	openfiles.erase(handle);

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
	assert(openfiles.count(handle));
	FileInfo &info = openfiles[handle];
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
	debug(errDebug, "%d open files:", (int)openfiles.size());
	for (openfiles_iterator_t it = openfiles.begin(); it != openfiles.end(); ++it) {
		const char *fname = it->second.name.c_str();
		debug(errDebug, " %p (%s)", it->first, fname);
		if (lock_lumps)
			debug(errDebug, "   read-lock:%d write-lock:%d", test_locked(fname, 0), test_locked(fname, 1));
	}
}

int lump_file_opener(FB_FILE *handle, const char *filename, size_t filename_len) {
	//fprintf(stderr, "opening %p (%s).\n", handle, filename);

	// Just let the default file opener handle it (it does quite a lot of stuff, actually),
	// and then patch the file hooks table with wrappers
	int ret = fb_DevFileOpen(handle, filename, filename_len);
	// Note: fb_DevFileOpen changes FB_FILE_ACCESS_ANY to actual access state
	if (ret) return ret;

	handle->hooks = &lumpfile_hooks;
	assert(openfiles.count(handle) == 0);
	FileInfo &info = openfiles[handle];
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

FnOpenCallback pfnLumpfileFilter;

// This is called on each (regular) OPEN, to optionally override the file opener function
FBCALL int OPEN_hook(FBSTRING *filename,
                     unsigned open_mode,
                     unsigned access_mode,
                     unsigned lock_mode,
                     int rec_len,
                     FnFileOpen *pfnFileOpen) {
	// It's safe to call a FB function in this context: fb_FileOpen[Ex]
	// (in libfb_file_open.c) seems to be specifically designed in this way: the state
	// the runtime library has not been modified (e.g. locked) at this point.

	// This is the correct test
	//bool writable = access_mode & FB_FILE_ACCESS_WRITE || accessmode == FB_FILE_ACCESS_ANY;
	// This tests only for explicit ACCESS WRITE, which is much more likely to be an error
	bool writable = access_mode & FB_FILE_ACCESS_WRITE;
	if (pfnLumpfileFilter(filename, writable ? -1 : 0))
		*pfnFileOpen = lump_file_opener;
	return 0;  // Success. We don't know any FB error codes, and we don't want to use them anyway
}

// A replacement for FB's filecopy which sends modification messages and deals with open files
// NOTE: return values are opposite to FileCopy (true for success)
int copyfile(FBSTRING *source, FBSTRING *destination) {
	if (pfnLumpfileFilter && pfnLumpfileFilter(destination, -1)) {
		int ret = copy_file_replacing(source->data, destination->data);
		if (ret)
			send_lump_modified_msg(destination->data);
		return ret;
	}
	return fb_FileCopy(source->data, destination->data);
}

void set_OPEN_hook(FnOpenCallback lumpfile_filter, int lump_writes_allowed, IPCChannel *channel) {
	pfnLumpfileFilter = lumpfile_filter;
	__fb_ctx.pfnDevOpenHook = OPEN_hook;
#ifndef _WIN32
	lock_lumps = true;
#endif
	allow_lump_writes = lump_writes_allowed;
	lump_updates_channel = channel;
}

void clear_OPEN_hook() {
	__fb_ctx.pfnDevOpenHook = NULL;
	lock_lumps = false;
	allow_lump_writes = true;
	lump_updates_channel = NULL_CHANNEL;
}
