#include "fb/fb_stub.h"

// common.bas
void debugc(char *msg, int errorlevel);

typedef FBCALL int (*FnStringPredicate)(FBSTRING *filename);


int file_wrapper_close(struct _FB_FILE *handle) {
	// Nothing here
	return fb_DevFileClose(handle);  
}

int file_wrapper_seek(struct _FB_FILE *handle, fb_off_t offset, int whence) {
	// Nothing here
	return fb_DevFileSeek(handle, offset, whence);
}

int file_wrapper_tell(struct _FB_FILE *handle, fb_off_t *pOffset) {
	// Nothing here
	return fb_DevFileTell(handle, pOffset);
}

int file_wrapper_read(struct _FB_FILE *handle, void *value, size_t *pValuelen) {
	// Nothing here
	return fb_DevFileRead(handle, value, pValuelen);
}

int file_wrapper_write(struct _FB_FILE *handle, const void *value, size_t valuelen) {
	// Nothing here
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


int lump_file_opener(struct _FB_FILE *handle, const char *filename, size_t filename_len) {
	// Just let the default file opener handle it (it does quite a lot of stuff, actually),
	// and then patch the file hooks table with wrappers
	int ret = fb_DevFileOpen(handle, filename, filename_len);
	if (ret) return ret;

	handle->hooks = &lumpfile_hooks;
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

