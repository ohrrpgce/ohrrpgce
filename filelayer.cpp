/* OHRRPGCE - low level file interface layer
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
 * Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
 */

//fb_stub.h MUST be included first, to ensure fb_off_t is 64 bit
#include "fb/fb_stub.h"
#include "filelayer.hpp"
#include <cstdio>
#include <cassert>
#include <map>
#include <cstring>
#include <errno.h>
#include "errorlog.h"
#include "mutex.hpp"
#include "lumpfile.h"

#define OPENDBG(...)
//#define OPENDBG(...) printf(__VA_ARGS__)
//#define OPENDBG(...) printf("%s: ", __FUNCTION__), printf(__VA_ARGS__)

// Max value returned by FREEFILE. Equal to 255.
#define MAX_FNUM FB_MAX_FILES - FB_RESERVED_FILES

// This array stores information about any open file that was opened using OPENFILE.
// Indexed by file number as returned by FREEFILE, not by the index used internally in rtlib
// to index __fb_ctx.fileTB (which is offset by FB_RESERVED_FILES). So openfiles[0] is never used.
// When quitting FB closes all files from within a destructor, so globals may have already
// been deleted. So it would be a bad idea to define openfiles as array of file-scope objects instead of pointers.
FileInfo *openfiles[MAX_FNUM + 1];

// Mutex for modifying entries in openfiles (if there's a risk another
// thread might close a file) or num_lazy_files.
// Not used for reading from openfiles in the course of normal file usage because
// it's assumed no more than one thread will use the same file number at once.
// (Note that all FB file functions themselves will acquire FB_LOCK.
// And note that the -exx signal handler will not be called until after
// builtin functions like SEEK return, so no chance to deadlock because of that.)
mutex openfiles_mutex(true);  // Non-destructing mutex

// Number of lazyclose'd files
int num_lazy_files = 0;

IPCChannel *lump_updates_channel;

bool lock_hooked_files = false;
bool allow_lump_writes = true;
bool allow_lazyclose = true;

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

// This is now pointless
static FileInfo *&get_fileinfo(int fnum) {
	return openfiles[fnum];
}

static FileInfo *&get_fileinfo(FB_FILE *handle) {
	return get_fileinfo(FB_FILE_FROM_HANDLE(handle));
}

// Called when a hooked file is opened or reused and lock_hooked_files==true
void lock_hooked_file(FB_FILE *handle, const char *filename) {
	if (handle->access & FB_FILE_ACCESS_WRITE) {
		//debuginfo("write-locking %s", filename);
		lock_file_for_write((FILE *)handle->opaque, filename, 1000);
	} else {
		//debuginfo("read-locking %s", filename);
		lock_file_for_read((FILE *)handle->opaque, filename, 1000);
	}
	//debuginfo("locks: can-read:%d  can-write:%d", 1 - test_locked(filename, 0), 1 - test_locked(filename, 1));
}

// Called when a hooked file is closed or lazyclosed
void closing_hooked_file(FB_FILE *handle, FileInfo *infop) {
	if (lock_hooked_files) {
		//debuginfo("unlocking %s", infop->name.c_str());
		unlock_file((FILE *)handle->opaque);  // Only needed on Windows
	}
	if (infop->dirty) {
		//fprintf(stderr, "%s was dirty\n", infop->name.c_str());
		send_lump_modified_msg(infop->name.c_str());
	}
}

// Unlike the other wrappers, this one is used for non-hooked files too,
// so that we can maintain openfiles[].
int file_wrapper_close(FB_FILE *handle) {
	int res = 0;
	// Don't need to hold openfiles_mutex to read from array
	FileInfo *infop = get_fileinfo(handle);
	// infop will be NULL if this file was not opened with OPENFILE: see HACK below
	if (infop) {
		OPENDBG("closing %s, locks: can-read:%d  can-write:%d\n", infop->name.c_str(),
		        1 - test_locked(infop->name.c_str(), 0), 1 - test_locked(infop->name.c_str(), 1));
		// If there are files lazy open when quitting we would hit this message.
		if (!infop->in_use) {
			debug(errBug, "Double close (close after lazyclose) of %s", infop->name.c_str());
			res = FB_RTERROR_ILLEGALFUNCTIONCALL;
			// If we return nonzero, fb_FileClose won't mark the file as closed, do it ourselves...
			// (just so filetest can keep going)
			handle->hooks = NULL;
		} else if (infop->hooked)
			closing_hooked_file(handle, infop);
		openfiles_mutex.lock();
		get_fileinfo(handle) = NULL;
		openfiles_mutex.unlock();
		delete infop;
	}

	return fb_DevFileClose(handle) || res;
}

/*
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
*/

int file_wrapper_write(FB_FILE *handle, const void *value, size_t valuelen) {
	FileInfo *infop = get_fileinfo(handle);
	assert(infop);
	if (!allow_lump_writes) {
		// It's not really a great idea to call debug in here,
		// because we've been called from inside the rtlib, so
		// the global rtlib mutex is held. Luckily, FB uses recursive
		// mutexes, meaning the same thread can lock one multiple times.
		if (!infop->reported_error) {
			// Setting this flag does not prevent recursion, since debug can open
			// a new file. We rely on the hook filter not hooking ?_debug.txt
			infop->reported_error = true;
			debug(errShowBug, "Tried to write to protected file %s", infop->name.c_str());
		}
		return 1;
	} else {
		infop->dirty = true;
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
	for (int fnum = 0; fnum <= MAX_FNUM; fnum++) {
		openfiles_mutex.lock();
		if (!openfiles[fnum]) {
			openfiles_mutex.unlock();
			continue;
		}
		FileInfo info = *openfiles[fnum];  // Copy so can release lock and call debug()
		openfiles_mutex.unlock();

		numopen++;
		const char *fname = info.name.c_str();
		debug(errDebug, " fnum=%d (%s) hooked=%d dirty=%d error=%d",
		      fnum, fname, info.hooked, info.dirty, info.reported_error);
		if (lock_hooked_files)
			debug(errDebug, "  locks: can-read:%d  can-write:%d", 1 - test_locked(fname, 0), 1 - test_locked(fname, 1));
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

	if (lock_hooked_files)
		lock_hooked_file(handle, filename);
	return 0;
}

// Check whether a file is already open, if so do everything needed to reset it, and return fnum.
// Returns 0 if not found.
static int try_reuse_open_file(const char* filename, enum OPENBits openbits) {
	// (Only need to lock to search openfiles on Alpha CPUs which do dependent-load reordering)
	bool found = false;
	int fnum;
	for (fnum = 1; fnum <= MAX_FNUM; fnum++) {
		FileInfo *info = get_fileinfo(fnum);
		if (info && info->name == filename) {
			// We can only reuse the file if it's not already in use!
			// Otherwise the two users wouldn't have independent file positions.
			// Of course, opening a file twice at once is almost certainly a mistake,
			// at least until we start to do file I/O in threads.
			if (info->in_use) {
				debug(errBug, "Opening %s twice! Probably a mistake", filename);
				return 0;
			}

			// Don't reuse the file if the openbits differ; not worth the trouble of
			// deciding whether to reuse, and also I've seen on Windows that if we have
			// the same file open twice, fflush one of them, LOF (which does fseek(fh,
			// 0, SEEK_END), ftell) on the other returns the wrong length!
			// So filetest will fail without this check.
			if (info->openbits != openbits) {
				OPENDBG("Closing lazy file %d to reopen with new openbits %x: %s\n", fnum, openbits, filename);
				openfiles_mutex.lock();
				info->in_use = true;  // Silence warning
				num_lazy_files--;
				openfiles_mutex.unlock();
				fb_FileClose(fnum);
				return 0;
			}

			found = true;
			break;
		}
	}
	if (!found)
		return 0;

	openfiles_mutex.lock();
	FileInfo *info = get_fileinfo(fnum);
	if (!info || info->in_use) {
		openfiles_mutex.unlock();
		return 0;  // Lost a race
	}
	info->in_use = true;
	num_lazy_files--;
	openfiles_mutex.unlock();

	if ((openbits & ACCESS_BITMASK) == ACCESS_WRITE) {
		OPENDBG("Closing lazy file to reopen for write %d %s\n", fnum, filename);
		// We could just truncate the file to zero, but we don't currently have a function for that.
		// So reopen.
		fb_FileClose(fnum);
		return 0;
	}

	OPENDBG("reusing %d = %s write %d\n", fnum, filename, openbits & (ACCESS_WRITE|ACCESS_READ_WRITE|FOR_OUTPUT|FOR_APPEND));

	FB_FILE *handle = FB_FILE_TO_HANDLE(fnum);
	if (handle->mode != FB_FILE_MODE_APPEND) {
		// Seek back to start
		if (fb_FileSeek(fnum, 1)) {
			debug(errError, "OPENFILE: couldn't reset file position when reusing open file %s", filename);
			fb_FileClose(fnum);
			return 0;
		}
	}

	if (info->hooked && lock_hooked_files)
		lock_hooked_file(handle, filename);

	// I found the most horrific thing in the rtlib source...  EOF for
	// FOR BINARY files doesn't actually check EOF, it compares the
	// file position to whatever the file length was when the file
	// was opened!!
	/* This might make EOF slightly less buggy
	if (handle->mode == FB_FILE_MODE_BINARY) {
		handle->size = fb_FileSize(fnum);  // LOF
	}
	*/

	return fnum;
}

// This is a replacement for fb_FileOpen/fb_FileOpenEncod which is what plain OPEN with
// or without an ENCODING argument is translated to in -lang fb.
// Note that calling this function acts like the functional form OPEN(): when compiled with -exx
// it doesn't cause the program to abort if there's an error.
// Returns an FBErrorEnum: fberrOK/0 on success, fberrILLEGAL_CALL/1 on error,
// fberrNOTFOUND/2 if file not found.
// (This enum and constants are named differently in C, eg. FB_RTERROR_FILENOTFOUND.)
// Also sets fnum to 0 if the file couldn't be opened.
FB_RTERROR OPENFILE(FBSTRING *filename, enum OPENBits openbits, int *fnum) {
	unsigned int mode, access;
	FB_FILE_ENCOD encod;

	*fnum = 0;

	if (!filename || !filename->data) {
		debug(errBug, "OPENFILE: empty filename");
		return FB_RTERROR_ILLEGALFUNCTIONCALL;
	}

	switch(openbits & FOR_BITMASK) {
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

	if ((openbits & FOR_BITMASK) != FOR_BINARY && (openbits & ACCESS_BITMASK)) {
		debug(errShowBug, "OPENFILE: bad flags (ACCESS_* only valid with FOR_BINARY): %x", openbits);
		return FB_RTERROR_ILLEGALFUNCTIONCALL;
	}

	switch(openbits & ACCESS_BITMASK) {
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

	switch(openbits & ENCODING_BITMASK) {
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

	// Skip calling the OPEN hook before trying to reopen a file; instead we
	// close all lazyclosed files when the hook changes.
	*fnum = try_reuse_open_file(filename->data, (OPENBits)(openbits & SAVE_OPENBITS_MASK));
	if (*fnum > 0) {
		// Add to log of recent files? Probably slow to do that 10000 times.
		//log_openfile(filename->data);
		return FB_RTERROR_OK;
	}

	FnFileOpen fnOpen;

	// Test for explicitly opening for writing (but ignore opening for
	// read+write, as that's the default).
	// pfnLumpfileFilter is responsible for showing an error if allow_lump_writes = NO.
	bool explicit_write = openbits & (ACCESS_WRITE | ACCESS_READ_WRITE | FOR_OUTPUT | FOR_APPEND);

	// Create a temp copy of filename, so that the filter function can modify it (we ignore modifications)
	FBSTRING file_to_open;
	init_fbstring_copy(&file_to_open, filename);

	FilterActionEnum action = DONT_HOOK;
	if (pfnLumpfileFilter)
		action = pfnLumpfileFilter(&file_to_open, explicit_write ? YES : NO, allow_lump_writes ? YES : NO);

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

	if ((*fnum = fb_FileFree()) == 0) {
		FB_UNLOCK();
		debug(errShowError, "OPENFILE: too many open files");  // That's not going to log...
		return FB_RTERROR_ILLEGALFUNCTIONCALL;
	}
	assert(!openfiles[*fnum]);

	OPENDBG("open without reuse %d %s write %d\n", *fnum, filename->data, explicit_write);

	errno = 0;
	FB_FILE *handle = FB_FILE_TO_HANDLE(*fnum);
	int ret = fb_FileOpenVfsEx(handle, &file_to_open, mode, access,
	                           FB_FILE_LOCK_SHARED, 0, encod, fnOpen);
	int C_err = errno;

	if (ret == FB_RTERROR_OK) {
		FileInfo *infop = new FileInfo();
		infop->in_use = true;
		infop->name = filename->data;
		infop->hooked = (action == HOOK);
		infop->openbits = (OPENBits)(openbits & SAVE_OPENBITS_MASK);
		openfiles_mutex.lock();
		openfiles[*fnum] = infop;
		openfiles_mutex.unlock();
	}

	FB_UNLOCK();

	if (ret != FB_RTERROR_OK && ret != FB_RTERROR_FILENOTFOUND) {
		debug(errError, "OPENFILE(%s, 0x%x)=%d: %s", filename->data,
		      openbits, ret, strerror(C_err));
	}
	if (ret != FB_RTERROR_OK && (openbits & OR_ERROR)) {
		debug(errShowError, "Couldn't open file %s: %s", filename->data, strerror(C_err));
	}
	if (ret != FB_RTERROR_OK) {
		*fnum = 0;
	} else {
		// Add to log of recent files
		log_openfile(filename->data);

		// HACK: hook CLOSE for all files, by permanently modifying FB's internal
		// file hooks tables, so that we can be sure that FileInfo will get deleted.
		// This will affect files opened with OPEN instead of OPENFILE too!
		if (handle->hooks && handle->hooks->pfnClose == fb_DevFileClose)
			handle->hooks->pfnClose = file_wrapper_close;
	}
	delete_fbstring(&file_to_open);

	return (FB_RTERROR)ret;
}

// Replacement for CLOSE which temporarily leaves the file open so it can be reused by OPENFILE
// Call close_lazy_files to ensure closed.
// It's OK to lazyclose files opened for writing (they will flushed, unlocked if needed, and
// lump modified messages sent).
// After you've called lazyclose it's an error to call close (because the file number might already
// be closed and reused!)
FB_RTERROR lazyclose(int fnum) {
	if (!allow_lazyclose)
		return (FB_RTERROR)fb_FileClose(fnum);

	// Let fb_FileClose handle special or invalid fnums (shouldn't happen!)
	if (fnum <= 0 || fnum > MAX_FNUM || !get_fileinfo(fnum)) {
		OPENDBG("lazyclose rejected %d\n", fnum);
		return (FB_RTERROR)fb_FileClose(fnum);
	}

	FileInfo *info = get_fileinfo(fnum);
	if (!info->in_use) {
		debug(errBug, "Double [lazy]close of %s", info->name.c_str());
		return FB_RTERROR_ILLEGALFUNCTIONCALL;
	}

	OPENDBG("lazyclose %d %s hooked=%d dirty=%d num_lazy_files=%d\n", fnum, info->name.c_str(), info->hooked, info->dirty, num_lazy_files);

	// No point using an LRU list, just close all the files occasionally!
	// (Race conditions reading num_lazy_files here aren't harmful.)
	if (num_lazy_files >= 4)
		close_lazy_files();

	FB_FILE *handle = FB_FILE_TO_HANDLE(fnum);

	// Possibly flush
	bool needflush = info->dirty;
	if (!info->hooked) {
		// Only hooked files have an accurate dirty flag. Flush any other opened for writing.
		bool readonly = (info->openbits & FOR_BITMASK) == FOR_INPUT ||
	                        (info->openbits & ACCESS_BITMASK) == ACCESS_READ;
		if (!readonly)
			needflush = true;
	}
	if (needflush) {
		// Or fb_DevFileFlush(handle) or handle->hooks->pfnFlush(handle), but
		// that does extra locking. Also, FB 1.08 finally exposes that as FLUSH
		fflush((FILE *)handle->opaque);
	}

	// Unlock, send message if dirty
	if (info->hooked)
		closing_hooked_file(handle, info);

	openfiles_mutex.lock();
	info->in_use = false;
	num_lazy_files++;
	info->dirty = false;
	openfiles_mutex.unlock();

	return FB_RTERROR_OK;
}

// Really close all files that are lazyclose'd.
// Shouldn't be needed before quitting, because FB closes all files itself.
void close_lazy_files() {
	if (num_lazy_files == 0)
		return;

	OPENDBG("close_lazy_files\n");
	openfiles_mutex.lock();
	for (int fnum = 0; fnum <= MAX_FNUM; fnum++) {
		FileInfo *info = get_fileinfo(fnum);
		if (info && info->in_use == false) {
			info->in_use = true;  // Prevent any other thread from reopening it
			num_lazy_files--;
			openfiles_mutex.unlock();
			OPENDBG("  closing %d %s\n", fnum, info->name.c_str());
			// This will call file_wrapper_close which deletes openfiles[fnum]
			fb_FileClose(fnum);
			openfiles_mutex.lock();
		}
	}
	openfiles_mutex.unlock();
	if (num_lazy_files) {
		OPENDBG(" ...but num_lazy_files=%d!\n", num_lazy_files);
	}
}


// A replacement for FB's filecopy which sends modification messages and deals with open files
// NOTE: return values are opposite to FileCopy (true for success)
boolint copyfile(FBSTRING *source, FBSTRING *destination) {
	FilterActionEnum action = DONT_HOOK;
	if (pfnLumpfileFilter)
		action = pfnLumpfileFilter(destination, YES, allow_lump_writes ? YES : NO);
	if (action == DENY) {
		// The filter ought to have already shown an error
		debug(errError, "copyfile(%s, %s) denied by filter", source->data, destination->data);
		return NO;
	}
	close_lazy_files();  // Probably not necessary
	int ret = copy_file_replacing(source->data, destination->data);
	if (ret && action == HOOK)
		send_lump_modified_msg(destination->data);
	return ret;
}

// Rename and/or move a file, while respecting the filter/hook function
// (sending lump modification messages and doing error reporting).
// Returns true for success. (Note: returns true if fell back to copy+delete
// but the delete failed).
// The new and old locations must be on the same filesystem, so only move
// between "nearby" locations (e.g. into a subdirectory)!
// NOTE: Call os_shell_move to move between filesystems.
boolint renamefile(FBSTRING *source, FBSTRING *destination) {
	FilterActionEnum actionsrc = DONT_HOOK, actiondest = DONT_HOOK;
	if (pfnLumpfileFilter) {
		actionsrc = pfnLumpfileFilter(source, YES, allow_lump_writes ? YES : NO);
		actiondest = pfnLumpfileFilter(destination, YES, allow_lump_writes ? YES : NO);
	}
	if (actionsrc == DENY || actiondest == DENY) {
		// The filter ought to have already shown an error
		debug(errError, "renamefile(%s, %s) denied by filter", source->data, destination->data);
		return NO;
	}
	close_lazy_files();

	// rename() is quite different on Windows and Unix, use a wrapper
	// (BTW, FB's NAME is translated directly to a rename() call)
	if (os_rename(source->data, destination->data) == NO) {
		// os_rename already showed/logged the error
		//dump_openfiles();
		return NO;
	}
	if (actionsrc == HOOK)
		send_lump_modified_msg(source->data);
	if (actiondest == HOOK)
		send_lump_modified_msg(destination->data);
	return YES;
}

// TODO: there's no reason to pass lump_writes_allowed; instead the filter function
// ought to return whether a file should be write-protected.
void set_OPEN_hook(FnOpenCallback lumpfile_filter, boolint lump_writes_allowed, boolint lazyclose_allowed, IPCChannel *channel) {
	pfnLumpfileFilter = lumpfile_filter;
#ifndef _WIN32
	lock_hooked_files = true;
#endif
	allow_lump_writes = !!lump_writes_allowed;
	allow_lazyclose = !!lazyclose_allowed;
	lump_updates_channel = channel;
	// lazyclosed files will have stale .hooked values
	close_lazy_files();
}

void clear_OPEN_hook() {
	lock_hooked_files = false;
	allow_lump_writes = true;
	allow_lazyclose = true;
	lump_updates_channel = NULL;
}

// Lookup the name of a file from its handle (as returned by FREEFILE/OPENFILE).
// Only works if it was opened with OPENFILE, otherwise returns dummy filename.
// This is just intended for debugging.
FBSTRING *get_fb_filename(int fnum) {
	FileInfo *infop = get_fileinfo(fnum);
	FBSTRING ret;
	init_fbstring(&ret, infop ? infop->name.c_str() : "<Non-OPENFILE file>");
	return return_fbstring(&ret);
}


/******************************************************************************/
// Abstraction layer for reading data files, either virtual files embedded into
// the executable or normal external files accessed via C FILE streams.  Only
// supports reading from embedded files, though writing external ones is OK. See
// also the similar BufferedFile wrapper in lumpfile, built over FB files, which
// only supports writing. Should probably merge this with the unused LumpFile
// class and add FB-like Get and Put functions.

struct VFile {
	enum {
		CFILE,
		MEM,
	} type;
	union {
		FILE *cfile;
		struct {
			int position;
			const char *data;
			int length;
		};
	};
};

// Implementation of the --list-embeds cmdline option
// Prints to both stdout and *debug.txt so that you can see it on Windows
void list_embedded_files() {
	EmbeddedFileInfo *info;
	for (info = embedded_files_table; ; info++) {
		if (!info->path)
			return;
		printf("%s  len=%d\n", info->path, info->length);
		debug(errDebug, "%s  len=%d\n", info->path, info->length);
	}
}

// Returns 0 if the paths are the same, ignoring differences in path seps
static int pathcmp(const char *path1, const char *path2) {
	while (true) {
		char c1 = *path1++, c2 = *path2++;
		if (c1 == '\\') c1 = '/';
		if (c2 == '\\') c2 = '/';
		if (c1 != c2) return 1;
		if (!c1) return 0;
	}
}

// Check whether a file is embedded in the executable, if so return its info.
// path should exclude res:// prefix.
EmbeddedFileInfo *find_embedded_file(const char *path) {
	EmbeddedFileInfo *info;
	for (info = embedded_files_table; ; info++) {
		if (!info->path)
			return NULL;
		// Use path-separator-insensitive comparison because of cross-compiles
		if (!pathcmp(info->path, path))
			return info;
	}
}

// Write an embedded file out to dump_path. Returns true on success.
bool dump_embedded_file(const char *embedded_path, const char *dump_path) {
	EmbeddedFileInfo *embedded = find_embedded_file(embedded_path);
	if (!embedded) {
		// This function is called from the -dump-embed commandline arg,
		// so better to write to stderr than the debug log
		fprintf(stderr, "No such embed: %s\n", embedded_path);
		return false;
	}
	FILE *cfile = fopen(dump_path, "wb");
	if (!cfile) return false;
	// Returns 1 if whole file written
	bool ret = fwrite(embedded->data, embedded->length, 1, cfile);
	fclose(cfile);
	return ret;
}

// Open either an arbitrary external file (relative to the current directory),
// or if path begins with "res://", an embedded data file, e.g.
// res://sourceslices/default_item_screen.slice
VFile *vfopen(const char *path, const char *mode) {
	VFile *ret;
	if (strncmp(path, "res://", 6) == 0) {
		if (strcmp(mode, "r") && strcmp(mode, "rb")) {
			debug(errShowBug, "vfopen: unsupported mode %s", mode);
			return NULL;
		}

		EmbeddedFileInfo *info = find_embedded_file(path + 6);  // Trim res://
		if (!info) {
			debug(errError, "vfopen: %s not found", path);  //Maybe count as a bug?
			return NULL;
		}

		ret = new VFile;
		ret->type = VFile::MEM;
		ret->position = 0;
		ret->data = info->data;
		ret->length = info->length;
	} else {
		FILE *cfile = fopen(path, mode);
		if (!cfile) return NULL;
		ret = new VFile;
		ret->type = VFile::CFILE;
		ret->cfile = cfile;
	}
	return ret;
}

void vfclose(VFile *file) {
	if (file->type == VFile::CFILE)
		fclose(file->cfile);
	delete file;
}

unsigned int vfread(void *restrict ptr, unsigned int size, unsigned int nmemb, VFile *file) {
	if (file->type == VFile::CFILE)
		return fread(ptr, size, nmemb, file->cfile);
	else {
		ssize_t bytes = size * nmemb;
		size_t ret = nmemb;
		if (bytes > file->length - file->position) {
			bytes = file->length - file->position;
			ret = bytes / size;
		}
		memcpy(ptr, file->data + file->position, bytes);
		file->position += bytes;
		return ret;
	}
}

int vfgetc(VFile *file) {
	if (file->type == VFile::CFILE)
		return fgetc(file->cfile);
	else {
		if (file->position == file->length)
			return EOF;
		return (unsigned char)file->data[file->position++];
	}
}

unsigned int vfwrite(const void *restrict ptr, unsigned int size, unsigned int nmemb, VFile *file) {
	if (file->type == VFile::CFILE)
		return fwrite(ptr, size, nmemb, file->cfile);
	else {
		showbug("Can't use vfwrite on memory buffers");
		return 0;
	}
}

size_t vfseek(VFile *file, ssize_t offset, int whence) {
	if (file->type == VFile::CFILE)
		return fseek(file->cfile, offset, whence);
	else {
		if (whence == SEEK_SET)
			file->position = offset;
		else if (whence == SEEK_CUR)
			file->position += offset;
		else if (whence == SEEK_END)
			file->position = file->length + offset;
		file->position = min(max(file->position, 0), file->length);
		return file->position;
	}
}

size_t vftell(VFile *file) {
	if (file->type == VFile::CFILE)
		return ftell(file->cfile);
	else
		return file->position;
}

size_t vflength(VFile *file) {
	if (file->type == VFile::MEM)
		return file->length;
	size_t pos = vftell(file);
	vfseek(file, 0, SEEK_END);
	size_t len = vftell(file);
	vfseek(file, pos, SEEK_SET);
	return len;
}

