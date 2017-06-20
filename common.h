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

// For alloca declaration
#ifdef _WIN32
#include <malloc.h>
#elif defined(__gnu_linux__)
// Doesn't exist in BSD
#include <alloca.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* I will use boolint in declarations of C/C++ functions where we would like to use
   bool (C/C++) or boolean (FB), but shouldn't, to support FB pre-1.04. So instead,
   use boolint on both sides, to show intention but prevent accidental C/C++ bool usage.
*/
typedef int boolint;

#ifdef _MSC_VER
 // TODO: bool is only available when compiling as C++, otherwise need typedef it...
#else
# include <stdbool.h>
#endif

#if defined(_WIN32) || defined(WIN32)
# define SLASH '\\'
# define ispathsep(chr) ((chr) == '/' || (chr) == '\\')
#else
# define SLASH '/'
# define ispathsep(chr) ((chr) == '/')
#endif

// __has_attribute is supported since gcc 5.0 and clang 2.9. That's very recent
// but I don't think we care if the attributes accidentally don't get used.
# ifndef __has_attribute
#  define __has_attribute(x) 0
# endif

// GCC is missing __has_builtin, at least in 5.4
#ifndef __has_builtin
# define __has_builtin(x) 0
#endif

// Can't rely on __has_builtin. Overflow-checked builtins introduced in GCC 5.0, and also in clang
#if  __GNUC__ >= 5 || (__has_builtin(__builtin_smul_overflow) && __has_builtin(__builtin_sadd_overflow))
# define has_overflow_builtins
#endif

// pure function: do not modify global memory, but may read it (including ptr args)
#if __has_attribute(pure)
# define pure __attribute__ ((__pure__))
#else
# define warn_unused_result
#endif

// _noreturn: does not return. Not the same as C++11 [[noreturn]], which can't be applied to function pointers.
#if __has_attribute(noreturn)
# define _noreturn __attribute__ ((__noreturn__))
#else
# define _noreturn
#endif

#ifdef _MSC_VER
# define restrict __restrict
#endif
#ifdef __cplusplus
  // restrict is not a keyword, but GCC accepts __restrict and __restrict__
# define restrict __restrict
#endif

// warn_unused_result: like [[nodiscard]] in C++11
#if __has_attribute(warn_unused_result)
# define warn_unused_result __attribute__ ((__warn_unused_result__))
#else
# define warn_unused_result
#endif

#if __has_attribute(format)
# define format_chk(fmt_arg) __attribute__ ((__format__ (__printf__, fmt_arg, fmt_arg + 1)))
#else
# define format_chk(fmt_arg)
#endif

// Escape a filename for use in a shell in a way suitable for this OS.
// Returns a malloc'd string buffer
char *escape_filenamec (const char *filename);

const char *trimpath(const char *filename);

// in common.bas
void debugc(enum ErrorLevel errorlevel, const char *msg);

// libfb.a
void _noreturn (*fb_ErrorThrowAt(int line_num, const char *mod_name, void *res_label, void *resnext_label))(void);

// in miscc.c
void _throw_error(enum ErrorLevel errorlevel, const char *srcfile, int linenum, const char *msg, ...) format_chk(4);
extern void (*debug_hook)(enum ErrorLevel errorlevel, const char *msg);
void set_debug_hook(void (*new_debug_hook)(enum ErrorLevel errorlevel, const char *msg));

#define debug(errorlevel, ...) _throw_error(errorlevel, NULL, 0, __VA_ARGS__)
#define debuginfo(...) _throw_error(errInfo, NULL, 0, __VA_ARGS__)
#define throw_error(...) _throw_error(errFatalBug, __FILE__, __LINE__, __VA_ARGS__)
#define fatal_error(...) _throw_error(errFatal, __FILE__, __LINE__, __VA_ARGS__)

void init_fbstring(FBSTRING *fbstr, const char *cstr);
void init_fbstring_copy(FBSTRING *fbstr, FBSTRING *src);
void set_fbstring(FBSTRING *fbstr, const char *cstr);
FBSTRING *return_fbstring(FBSTRING *fbstr);
FBSTRING *empty_fbstring();
void delete_fbstring(FBSTRING *str);

uint32_t stringhash(const unsigned char *strp, int length);

#ifdef __cplusplus
}
#endif

#endif
