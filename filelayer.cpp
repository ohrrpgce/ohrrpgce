/* OHRRPGCE - low level file interface layer
 * Copyright 2011. Please read LICENSE.txt for GNU GPL details and disclaimer of liability
 */

//fb_stub.h MUST be included first, to ensure fb_off_t is 64 bit
#include "fb/fb_stub.h"
#include "filelayer.hpp"
#include <cstdio>
#include <cassert>
#include <map>
#include <cstring>
#include "misc.h"
#include "errno.h"
#include "mutex.hpp"

// This array stores information about any open file that was opened using OPENFILE.
// Indexed by index into FB's __fb_ctx.fileTB, NOT by file number as returned by FREEFILE.
// When quitting FB closes all files from within a destructor, so globals may have already
// been deleted. So it would be a bad idea to define openfiles as array of file-scope objects instead of pointers.
FileInfo *openfiles[FB_MAX_FILES];

// Mutex for creating or deleting entries in openfiles (if there's a risk another thread
// might close a file) - ONLY needed for dump_openfiles, because it's assumed a thread
// will never attempt to close a file while another is using it normally.
// So this is mutex is a bit ridiculous really.
// (Note that all FB file functions themselves will acquire FB_LOCK.
// And note that the -exx signal handler will not be called until after
// builtin functions like SEEK return, so no chance to deadlock because of that.)
mutex openfiles_mutex(true);  // Non-destructing mutex

IPCChannel *lump_updates_channel;

bool lock_lumps = false;
bool allow_lump_writes = true;

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

static FileInfo *&get_fileinfo(int fnum) {
	return openfiles[fnum + (FB_RESERVED_FILES - 1)];
}

static FileInfo *&get_fileinfo(FB_FILE *handle) {
	// equivalent to openfiles[handle - __fb_ctx.fileTB];
	return get_fileinfo(FB_FILE_FROM_HANDLE(handle));
}

// Unlike the other wrappers, this one is used for non-hooked files too,
// so that we can maintain openfiles[].
int file_wrapper_close(FB_FILE *handle) {
	FileInfo *&infop = get_fileinfo(handle);
	// infop will be NULL if this file was not opened with OPENFILE: see HACK below
	if (infop) {
		//debuginfo("closing %s, read-lock:%d write-lock:%d", info.name.c_str(), test_locked(info.name.c_str(), 0), test_locked(info.name.c_str(), 1));
		if (infop->hooked) {
			if (infop->dirty) {
				//fprintf(stderr, "%s was dirty\n", info.name.c_str());
				send_lump_modified_msg(infop->name.c_str());
			}
			//debuginfo("unlocking %s", info.name.c_str());
			unlock_file((FILE *)handle->opaque);  // Only needed on Windows
		}
		openfiles_mutex.lock();
		delete infop;
		infop = NULL;
		openfiles_mutex.unlock();
	}

	return fb_DevFileClose(handle);
}

int file_wrapper_seek(FB_FILE *handle, fb_off_t offset, int whence) {
	// Nothing here yet
	return fb_DevFileSeek(handle, offset, whence);
}

int file_wrapper_tell(FB_FILE *handle, fb_off_t *pOffset) {
	// Nothing here yet
	return fb_DevFileTell(handle, pOffset);
}

int file_wrapper_read(FB_FILE *handle, void *value, size_t *pValuelen) {
	// Nothing here yet
	return fb_DevFileRead(handle, value, pValuelen);
}

int file_wrapper_write(FB_FILE *handle, const void *value, size_t valuelen) {
	FileInfo &info = *get_fileinfo(handle);
	assert(&info);
	if (!allow_lump_writes) {
		// It's not really a great idea to call debug in here,
		// because we've been called from inside the rtlib, so
		// the global rtlib mutex is held. Luckily, FB uses recursive
		// mutexes, meaning the same thread can lock one multiple times.
		if (!info.reported_error) {
			// Setting this flag does not prevent recursion, since debug can open
			// a new file. We rely on the hook filter not hooking ?_debug.txt
			info.reported_error = true;
			debug(errShowBug, "Tried to write to protected file %s", info.name.c_str());
		}
		return 1;
	} else {
		info.dirty = true;
		return fb_DevFileWrite(handle, value, valuelen);
	}
}

// Modified version of hooks_dev_table in libfb_dev_file_open.c
static FB_FILE_HOOKS lumpfile_hooks = {
	fb_DevFileEof,
	file_wrapper_close,
	fb_DevFileSeek,       //file_wrapper_seek,
	fb_DevFileTell,       //file_wrapper_tell,
	fb_DevFileRead,       //file_wrapper_read,
	fb_DevFileReadWstr,
	file_wrapper_write,
	fb_DevFileWriteWstr,  // Ought to intercept this
	fb_DevFileLock,
	fb_DevFileUnlock,
	fb_DevFileReadLine,
	fb_DevFileReadLineWstr,
	NULL,
	fb_DevFileFlush
};

void dump_openfiles() {
	int numopen = 0;
	for (int fidx = 0; fidx < FB_MAX_FILES; fidx++) {
		openfiles_mutex.lock();
		if (!openfiles[fidx]) {
			openfiles_mutex.unlock();
			continue;
		}
		FileInfo info = *openfiles[fidx];  // Copy so can release lock
		openfiles_mutex.unlock();

		numopen++;
		const char *fname = info.name.c_str();
		debug(errDebug, " %d (%s) hooked=%d dirty=%d error=%d",
		      fidx + (FB_RESERVED_FILES - 1), fname, info.hooked, info.dirty, info.reported_error);
		if (lock_lumps)
			debug(errDebug, "   read-lock:%d write-lock:%d", test_locked(fname, 0), test_locked(fname, 1));
	}
	debug(errDebug, "%d open OPENFILE files", numopen);
}

// Replacement for fb_DevFileOpen().
// (Actually, I don't see why we need this - can't we just switch the hooks and lock the file inside OPENFILE?
// Almost nothing happens in-between.
int lump_file_opener(FB_FILE *handle, const char *filename, size_t filename_len) {
	//fprintf(stderr, "opening %p (%s).\n", handle, filename);

	// Just let the default file opener handle it (it does quite a lot of stuff, actually),
	// and then patch the file hooks table with wrappers
	int ret = fb_DevFileOpen(handle, filename, filename_len);
	// Note: fb_DevFileOpen changes FB_FILE_ACCESS_ANY to actual access state
	if (ret) return ret;

	handle->hooks = &lumpfile_hooks;

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
// Returns an FBErrorEnum: fberrOK/0 on success, fberrILLEGAL_CALL/1 on error,
// fberrNOTFOUND/2 if file not found.
// (This enum and constants are named differently in C, eg. FB_RTERROR_FILENOTFOUND.)
// Also sets fnum to 0 if the file couldn't be opened.
FB_RTERROR OPENFILE(FBSTRING *filename, enum OPENBits openbits, int &fnum) {
	unsigned int mode, access;
	FB_FILE_ENCOD encod;

	fnum = 0;

	if (!filename || !filename->data) {
		debug(errBug, "OPENFILE: empty filename");
		return FB_RTERROR_ILLEGALFUNCTIONCALL;
	}

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
			debug(errShowBug, "OPENFILE: bad flags (bad FOR): %x", openbits);
			return FB_RTERROR_ILLEGALFUNCTIONCALL;
	}

	if ((openbits & FOR_MASK) != FOR_BINARY && (openbits & ACCESS_MASK)) {
		debug(errShowBug, "OPENFILE: bad flags (ACCESS_* only valid with FOR_BINARY): %x", openbits);
		return FB_RTERROR_ILLEGALFUNCTIONCALL;
	}

	switch(openbits & ACCESS_MASK) {
		case ACCESS_ANY:
			// Try to open for writing, then for reading if that fails
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
			debug(errShowBug, "OPENFILE: bad flags (bad ACCESS): %x", openbits);
			return FB_RTERROR_ILLEGALFUNCTIONCALL;
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
			debug(errShowBug, "OPENFILE: bad flags (bad ENCODING): %x", openbits);
			return FB_RTERROR_ILLEGALFUNCTIONCALL;
	}

	FnFileOpen fnOpen;

	// Test for explicitly opening for writing (but ignore opening for
	// read+write, as that's the default).
	// pfnLumpfileFilter is responsible for showing an error if allow_lump_writes = NO.
	bool explicit_write = openbits & (ACCESS_WRITE | ACCESS_READ_WRITE | FOR_OUTPUT | FOR_APPEND);

	// Create a temp copy of filename, so that the filter function can modify it
	FBSTRING file_to_open;
	init_fbstring_copy(&file_to_open, filename);

	FilterActionEnum action = DONT_HOOK;
	if (pfnLumpfileFilter)
		action = pfnLumpfileFilter(&file_to_open, explicit_write ? -1 : 0, allow_lump_writes ? -1 : 0);

	if (action == HOOK) {
		if (!allow_lump_writes) {
			// If we implicitly asked for writing, then reduce to read access.
			access = FB_FILE_ACCESS_READ;
		}
		if (encod != FB_FILE_ENCOD_ASCII) {
			debug(errShowBug, "OPENFILE: ENCODING not implemented for hooked files");
			return FB_RTERROR_ILLEGALFUNCTIONCALL;
		}
		fnOpen = lump_file_opener;
	} else if (action == DONT_HOOK) {
		if (encod == FB_FILE_ENCOD_ASCII)
			fnOpen = fb_DevFileOpen;
		else
			fnOpen = fb_DevFileOpenEncod;
	} else if (action == DENY) {
		return FB_RTERROR_ILLEGALFUNCTIONCALL;
	} else if (action == HIDE) {
		return FB_RTERROR_FILENOTFOUND;
	} else {
		fatal_error("OPENFILE: Invalid action returned by filter function");
		return FB_RTERROR_ILLEGALFUNCTIONCALL;
	}

	// Avoid race condition with other threads opening a file, either using OPENFILE
	// or FREEFILE/OPEN. This race condition is a flaw in FB.
	// Luckily the global FB lock is recursive (lockable multiple times), so we
	// can use it to prevent other threads from calling FREEFILE.
	FB_LOCK();

	if ((fnum = fb_FileFree()) == 0) {
		FB_UNLOCK();
		debug(errShowError, "OPENFILE: too many open files");
		return FB_RTERROR_ILLEGALFUNCTIONCALL;
	}

	FileInfo *&infop = get_fileinfo(fnum);
	assert(!infop);
	openfiles_mutex.lock();
	infop = new FileInfo();
	infop->name = filename->data;
	infop->hooked = (action == HOOK);
	openfiles_mutex.unlock();

	errno = 0;
	FB_FILE *handle = FB_FILE_TO_HANDLE(fnum);
	int ret = fb_FileOpenVfsEx(handle, &file_to_open, mode, access,
	                           FB_FILE_LOCK_SHARED, 0, encod, fnOpen);
	int C_err = errno;

	FB_UNLOCK();

	const char *cfilename = (filename && filename->data) ? filename->data : "";  // Valid empty string
	if (ret != FB_RTERROR_OK && ret != FB_RTERROR_FILENOTFOUND) {
		debug(errError, "OPENFILE(%s, 0x%x)=%d: %s", cfilename,
		      openbits, ret, strerror(C_err));
	}
	if (ret != FB_RTERROR_OK && (openbits & OR_ERROR)) {
		debug(errShowError, "Couldn't open file %s: %s", cfilename, strerror(C_err));
	}
	if (ret != FB_RTERROR_OK) {
		fnum = 0;
		openfiles_mutex.lock();
		delete infop;
		infop = NULL;
		openfiles_mutex.unlock();
	} else {
		// HACK: hook CLOSE for all files, by permanently modifying FB's internal
		// file hooks tables, so that we can be sure that FileInfo will get deleted.
		// This will affect files opened with OPEN instead of OPENFILE too!
		if (handle->hooks && handle->hooks->pfnClose == fb_DevFileClose)
			handle->hooks->pfnClose = file_wrapper_close;
	}
	delete_fbstring(&file_to_open);

	return (FB_RTERROR)ret;
}


// A replacement for FB's filecopy which sends modification messages and deals with open files
// NOTE: return values are opposite to FileCopy (true for success)
boolint copyfile(FBSTRING *source, FBSTRING *destination) {
	FilterActionEnum action = DONT_HOOK;
	if (pfnLumpfileFilter)
		action = pfnLumpfileFilter(destination, -1, allow_lump_writes ? -1 : 0);
	if (action == DENY) {
		// The filter ought to have already shown an error
		debug(errError, "copyfile(%s, %s) denied by filter", source->data, destination->data);
		return 0;
	}
	int ret = copy_file_replacing(source->data, destination->data);
	if (ret && action == HOOK)
		send_lump_modified_msg(destination->data);
	return ret;
}

// Rename a file, while respecting the filter/hook fnction.
// Returns true for success.
// Warning! rename() is quite different on Windows and Unix. Call the
// local_file_move wrapper in util.bas instead; this is a lower level function.
boolint renamefile(FBSTRING *source, FBSTRING *destination) {
	FilterActionEnum actionsrc = DONT_HOOK, actiondest = DONT_HOOK;
	if (pfnLumpfileFilter) {
		actionsrc = pfnLumpfileFilter(source, -1, allow_lump_writes ? -1 : 0);
		actiondest = pfnLumpfileFilter(destination, -1, allow_lump_writes ? -1 : 0);
	}
	if (actionsrc == DENY || actiondest == DENY) {
		// The filter ought to have already shown an error
		debug(errError, "renamefile(%s, %s) denied by filter", source->data, destination->data);
		return 0;
	}
	if (rename(source->data, destination->data)) {
		dump_openfiles();  // On Windows rename() typically fails because the file is open
		debug(errShowError, "rename(%s, %s) failed: %s", source->data, destination->data, strerror(errno));
		return 0;
	}
	if (actionsrc == HOOK)
		send_lump_modified_msg(source->data);
	if (actiondest == HOOK)
		send_lump_modified_msg(destination->data);
	return -1;
}

// TODO: there's no reason to pass lump_writes_allowed; instead the filter function
// ought to return whether a file should be write-protection.
void set_OPEN_hook(FnOpenCallback lumpfile_filter, boolint lump_writes_allowed, IPCChannel *channel) {
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

// Lookup the name of a file from its handle (as returned by FREEFILE/OPENFILE).
// Only works if it was opened with OPENFILE, otherwise returns dummy filename.
// This is just intended for debugging.
FBSTRING *get_filename(int fnum) {
	FileInfo *infop = get_fileinfo(fnum);
	FBSTRING ret;
	init_fbstring(&ret, infop ? infop->name.c_str() : "<Non-OPENFILE file>");
	return return_fbstring(&ret);
}
