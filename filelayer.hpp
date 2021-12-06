/* OHRRPGCE - low level file interface layer
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
 * Please read LICENSE.txt for GPL License details and disclaimer of liability
 */

#ifndef FILELAYER_H
#define FILELAYER_H

#include "config.h"
#include "fb/fb_stub.h"
#ifdef __cplusplus
	#include <string>
	using namespace std;
#endif
#include "os.h"


// NOTE: Duplicated in lumpfile.bi
enum OPENBits {
	OR_ERROR =          0x0000001,  // Show an error message (showerror) if the file can't be opened
	// FOR RANDOM (fixed sized records) not supported. Use load/storerecord() instead.
	FOR_BINARY =        0x0010000,  // default
	FOR_INPUT =         0x0020000,
	FOR_OUTPUT =        0x0040000,
	FOR_APPEND =        0x0080000,
	FOR_BITMASK =       0x00F0000,
	//For files, ACCESS ANY means try READ_WRITE, failing that use READ.
	//Which sounds like a misfeature to me, so let's default to ACCESS_READ_WRITE instead.
	ACCESS_ANY =        0x0100000,
	ACCESS_READ =       0x0200000,
	ACCESS_WRITE =      0x0400000,
	ACCESS_READ_WRITE = 0x0800000,  // default
	ACCESS_BITMASK =    0x0F00000,
	// Not implemented yet for hooked files, so no point using these
	ENCODING_ASCII =    0x1000000,  // default
	ENCODING_UTF8 =     0x2000000,
	ENCODING_UTF16 =    0x4000000,
	ENCODING_UTF32 =    0x8000000,
	ENCODING_BITMASK =  0xF000000,
	// LOCK not supported... in fact it's not even properly supported by FB!
	// However it could be added (since we already have file locking implemented in os.bi)
	// if it were useful.
	// LEN (record length) not supported.

	SAVE_OPENBITS_MASK = (FOR_BITMASK | ACCESS_BITMASK | ENCODING_BITMASK),
};


enum FilterActionEnum {
	HOOK = 1,     // Open and hook it
	DONT_HOOK = 2,// Open but don't hook it
	DENY = 3,     // Don't open the file, return illegal function call error
	HIDE = 4,     // Don't open the file, return file not found
};

#ifdef __cplusplus
struct FileInfo {
	string name;
	bool hooked;          // Send lump modified messages if dirty, and is locked, if locking enabled.
	bool dirty;           // File has been written to
	bool reported_error;  // Don't show more than one error
	enum OPENBits openbits;  // Mode/access/encoding bits it was opened with; other bits excluded.
	int in_use;           // Hasn't been lazyclosed

	FileInfo() : hooked(false), dirty(false), reported_error(false), openbits(), in_use(false) {};
};

extern "C" {
#endif

	typedef FBCALL enum FilterActionEnum (*FnOpenCallback)(FBSTRING *filename, boolint writable, boolint writes_allowed);

	void send_lump_modified_msg(const char *filename);
	boolint copyfile(FBSTRING *source, FBSTRING *destination);
	boolint renamefile(FBSTRING *source, FBSTRING *destination);

	void set_OPEN_hook(FnOpenCallback lumpfile_filter, boolint lump_writes_allowed, boolint lazyclose_allowed, IPCChannel *channel);
	void clear_OPEN_hook();

	FB_RTERROR OPENFILE(FBSTRING *filename, enum OPENBits openbits, int *fnum);
	FB_RTERROR lazyclose(int fnum);
	void close_lazy_files();

	FBSTRING *get_fb_filename(int fnum);

	// Embedded data files

	typedef struct EmbeddedFileInfo {
		const char *path;
		const char *data;
		int length;
	} EmbeddedFileInfo;
	extern EmbeddedFileInfo *embedded_files_table;

	void list_embedded_files();
	EmbeddedFileInfo *find_embedded_file(const char *path);

	// Abstraction layer for reading data files, embedded or not

	typedef struct VFile VFile;

	VFile *vfopen(const char *path, const char *mode);
	void vfclose(VFile *file);
	unsigned int vfread(void *restrict ptr, unsigned int size, unsigned int nmemb, VFile *file);
	int vfgetc(VFile *file);
	unsigned int vfwrite(const void *restrict ptr, unsigned int size, unsigned int nmemb, VFile *file);
	size_t vfseek(VFile *file, ssize_t offset, int whence);
	size_t vftell(VFile *file);

#ifdef __cplusplus
}
#endif

#endif // FILELAYER_H
