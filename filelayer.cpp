/* OHRRPGCE - low level file interface layer
 * Copyright 2011. Please read LICENSE.txt for GNU GPL details and disclaimer of liability
 */

//fb_stub.h (included by filelayer.h) MUST be included first, to ensure fb_off_t is 64 bit
#include "filelayer.h"
#include <cstdio>
#include <cassert>
#include <map>

map<FB_FILE *, FileInfo> openfiles;
typedef map<FB_FILE *, FileInfo>::iterator openfiles_iterator_t;

IPCChannel lump_updates_channel;


int file_wrapper_close(FB_FILE *handle) {
	assert(openfiles.count(handle));
	FileInfo &info = openfiles[handle];
	//fprintf(stderr, "closing %s\n", info.name.c_str());
	if (info.dirty && lump_updates_channel != NULL_CHANNEL) {
		//fprintf(stderr, "%s was dirty\n", info.name.c_str());
		char buf[256];
		int len = snprintf(buf, 256, "M %s\n", info.name.c_str());
		if (len > 255) len = 255;
		channel_write(lump_updates_channel, buf, len);
	}
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
	fprintf(stderr, "%d open files:\n", openfiles.size());
	for (openfiles_iterator_t it = openfiles.begin(); it != openfiles.end(); ++it) {
		fprintf(stderr, " %p (%s)\n", it->first, it->second.name.c_str());
	}
}

int lump_file_opener(FB_FILE *handle, const char *filename, size_t filename_len) {
	//fprintf(stderr, "openning %p (%s).\n", handle, filename);

	// Just let the default file opener handle it (it does quite a lot of stuff, actually),
	// and then patch the file hooks table with wrappers
	int ret = fb_DevFileOpen(handle, filename, filename_len);
	if (ret) return ret;

	handle->hooks = &lumpfile_hooks;
	assert(openfiles.count(handle) == 0);
	FileInfo &info = openfiles[handle];
	info.name = string(filename);
	return 0;
}

FnStringPredicate pfnLumpfileFilter;

// This is called on each (regular) OPEN, to optionally override the file opener function
FBCALL int OPEN_hook(FBSTRING *filename,
                     unsigned open_mode,
                     unsigned access_mode,
                     unsigned lock_mode,
                     int rec_len,
                     FnFileOpen *pfnFileOpen) {
	// I believe that it's safe to call a FB function in this context: fb_FileOpen[Ex]
	// (in libfb_file_open.c) seems to be specifically designed in this way: the state
	// the runtime library has not been modified (e.g. locked) at this point.
	if (pfnLumpfileFilter(filename))
		*pfnFileOpen = lump_file_opener;
	return 0;  // Success. We don't know any FB error codes, and we don't want to use them anyway
}

void set_OPEN_hook_filter(FnStringPredicate lumpfile_filter) {
	pfnLumpfileFilter = lumpfile_filter;
	__fb_ctx.pfnDevOpenHook = OPEN_hook;
}

void set_lump_updates_channel(IPCChannel channel) {
	lump_updates_channel = channel;
}
