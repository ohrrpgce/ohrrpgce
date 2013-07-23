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

// Set an FB string to a C string
// *fbstr is assumed to be garbage
void init_fbstring(FBSTRING *fbstr, char *cstr) {
	fb_StrInit(fbstr, -1, cstr, strlen(cstr), 0);
}

// Initialise an FB string to a C string
// *fbstr is assumed to be already initialised
void set_fbstring(FBSTRING *fbstr, char *cstr) {
	fb_StrAssign(fbstr, -1, cstr, strlen(cstr), 0);
}
