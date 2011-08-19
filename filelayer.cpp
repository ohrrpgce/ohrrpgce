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

IPCChannel lump_updates_channel;

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

int file_wrapper_close(FB_FILE *handle) {
	assert(openfiles.count(handle));
	FileInfo &info = openfiles[handle];
	//debuginfo("closing %s, read-lock:%d write-lock:%d", info.name.c_str(), test_locked(info.name.c_str(), 0), test_locked(info.name.c_str(), 1));
	if (info.dirty && !allow_lump_writes) {
		// It's not really safe to call debug in here
		debug(3, "ENGINE BUG: illegally wrote to %s", info.name.c_str());
	}
	if (info.dirty && lump_updates_channel != NULL_CHANNEL) {
		//fprintf(stderr, "%s was dirty\n", info.name.c_str());
		char buf[256];
		int len = snprintf(buf, 256, "M %s\n", trimpath(info.name.c_str()));
		if (len > 255) {
			len = 255;
			buf[254] = '\n';
		}
		channel_write(lump_updates_channel, buf, len);
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
	debug(2, "%d open files:", openfiles.size());
	for (openfiles_iterator_t it = openfiles.begin(); it != openfiles.end(); ++it) {
		const char *fname = it->second.name.c_str();
		debug(2, " %p (%s)", it->first, fname);
                if (lock_lumps)
			debug(2, "   read-lock:%d write-lock:%d", test_locked(fname, 0), test_locked(fname, 1));
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

void set_OPEN_hook_filter(FnOpenCallback lumpfile_filter, int lump_writes_allowed) {
	pfnLumpfileFilter = lumpfile_filter;
	__fb_ctx.pfnDevOpenHook = OPEN_hook;
	lock_lumps = true;
	allow_lump_writes = lump_writes_allowed;
}

void set_lump_updates_channel(IPCChannel channel) {
	lump_updates_channel = channel;
}
