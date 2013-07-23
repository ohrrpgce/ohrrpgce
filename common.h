/* OHRRPGCE
 * This does NOT correspond to common.bi/common.bas
 * Copyright 2011. Please read LICENSE.txt for GNU GPL details and disclaimer of liability
 */

#ifndef COMMON_H
#define COMMON_H

//fb_stub.h MUST be included first, to ensure fb_off_t is 64 bit
#include "fb/fb_stub.h"
#include "errorlevel.h"
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif


#if defined(_WIN32) || defined(WIN32)
# define SLASH '\\'
# define ispathsep(chr) ((chr) == '/' || (chr) == '\\')
#else
# define SLASH '/'
# define ispathsep(chr) ((chr) == '/')
#endif


/* Several other C/C++ compilers, like Comeau C++, also have good gcc compatibility. Change this.
   Apparently the Intel compiler defines __GNUC__ */
#if defined(__GNUC__) || defined(__IBMC__) || defined(__INTEL_COMPILER)
# define pure __attribute__ ((__pure__))
# define format_chk(fmt_arg) __attribute__ ((__format__ (__printf__, fmt_arg, fmt_arg + 1)))
# define noreturn __attribute__ ((__noreturn__))
# define warn_unused_result __attribute__ ((__warn_unused_result__))
#else
# define pure
# define format_chk(fmt_arg)
# define noreturn
//# define inline
#endif

// Escape a filename for use in a shell in a way suitable for this OS.
// Returns a malloc'd string buffer
char *escape_filenamec (const char *filename);

const char *trimpath(const char *filename);

// in common.bas
void debugc(enum ErrorLevel errorlevel, const char *msg);

// libfb.a
void (*fb_ErrorThrowAt(int line_num, const char *mod_name, void *res_label, void *resnext_label))(void) noreturn;

// in miscc.c
void _throw_error(enum ErrorLevel errorlevel, const char *srcfile, int linenum, const char *msg, ...) format_chk(4);
extern void (*debug_hook)(enum ErrorLevel errorlevel, const char *msg);
void set_debug_hook(void (*new_debug_hook)(enum ErrorLevel errorlevel, const char *msg));

#define debug(errorlevel, ...) _throw_error(errorlevel, NULL, 0, __VA_ARGS__)
#define debuginfo(...) _throw_error(errInfo, NULL, 0, __VA_ARGS__)
#define throw_error(...) _throw_error(errFatalBug, __FILE__, __LINE__, __VA_ARGS__)
#define fatal_error(...) _throw_error(errFatal, __FILE__, __LINE__, __VA_ARGS__)

void init_fbstring(FBSTRING *fbstr, char *cstr);
void set_fbstring(FBSTRING *fbstr, char *cstr);


#ifdef __cplusplus
}
#endif

#endif
