/*
 * miscc.c - Misc functions written in C
 *
 * Please read LICENSE.txt for GPL License details and disclaimer of liability
 */

//fb_stub.h MUST be included first, to ensure fb_off_t is 64 bit
#include "fb/fb_stub.h"
#include <errno.h>
#include <stdarg.h>
#include <string.h>
#include "common.h"


//Trying to read errno from FB is unlikely to even link, because it's normally a macro, so this has be in C
char *get_sys_err_string() {
	return strerror(errno);
}

void (*debug_hook)(enum ErrorLevel errorlevel, const char *msg) = debugc;

// This is for the benefit of testing tools (vectortest)
void set_debug_hook(void (*new_debug_hook)(enum ErrorLevel errorlevel, const char *msg)) {
	if (new_debug_hook)
		debug_hook = new_debug_hook;
	else
		debug_hook = debugc;
}

void _throw_error(enum ErrorLevel errorlevel, const char *srcfile, int linenum, const char *msg, ...) {
	va_list vl;
	va_start(vl, msg);
	char buf[256];
	buf[255] = '\0';
	int emitted = 0;
	if (srcfile)
		emitted = snprintf(buf, 255, "On line %d in %s: ", linenum, srcfile);
	vsnprintf(buf + emitted, 255 - emitted, msg, vl);
	va_end(vl);
	debug_hook(errorlevel, buf);
	/*
	if (errorlevel >= 5) {
		// Ah, what the heck, shouldn't run, but I already wrote it (NULLs indicate no RESUME support)
		void (*handler)() = fb_ErrorThrowAt(linenum, srcfile, NULL, NULL);
		handler();
	}
	*/
}

// Initialise an FBSTRING to a C string
// *fbstr is assumed to be garbage
void init_fbstring(FBSTRING *fbstr, char *cstr) {
	fb_StrInit(fbstr, -1, cstr, strlen(cstr), 0);
}

// Set an existing FBSTRING to a C string
// *fbstr must already initialised!
void set_fbstring(FBSTRING *fbstr, char *cstr) {
	fb_StrAssign(fbstr, -1, cstr, strlen(cstr), 0);
}

// Use this function to return a FB string from C.
// This allocates a temporary descriptor which can be returned.
// (The original string should not be freed.)
FBSTRING *return_fbstring(FBSTRING *fbstr) {
	return fb_StrAllocTempResult(fbstr);
}

// A returnable empty string. The result doesn't
// need to be passed through return_fbstring()
FBSTRING *empty_fbstring() {
	return &__fb_ctx.null_desc;
}

#define ROT(a, b) ((a << b) | (a >> (32 - b)))

// Quite fast hash, ported from fb2c++ (as strihash,
// original was case insensitive) which I wrote and tested myself.
// Actually it turns out this can distribute nonideally for non-text,
// proving it really was a bad idea.
// strp may be NULL iif length is 0
uint32_t stringhash(unsigned char *strp, int length) {
	uint32_t hash = 0xbaad1dea;
	int extra_bytes = length & 3;

	length /= 4;
	while (length) {
		hash += *(uint32_t *)strp;
		strp += 4;
		hash = (hash << 5) - hash;  // * 31
		hash ^= ROT(hash, 19);
		length -= 1;
	}

	if (extra_bytes) {
		if (extra_bytes == 3)
			hash += *(uint32_t *)strp & 0xffffff;
		else if (extra_bytes == 2)
			hash += *(uint32_t *)strp & 0xffff;
		else if (extra_bytes == 1)
			hash += *strp;
		hash = (hash << 5) - hash;  // * 31
		hash ^= ROT(hash, 19);
	}

	//No need to be too thorough, will get rehashed if needed anyway
	hash += ROT(hash, 2);
	hash ^= ROT(hash, 27);
	hash += ROT(hash, 16);
	return hash;
}
