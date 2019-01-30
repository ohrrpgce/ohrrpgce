
// Defining CONFIG_H breaks some MSVC++ or Windows header!
#ifndef CONFIG_H__
#define CONFIG_H__

//fb_stub.h MUST be included first, to ensure fb_off_t is 64 bit
#include "fb/fb_stub.h"
#include "errorlevel.h"
#include <stdint.h>
#include <stdlib.h>  // For __MINGW64_VERSION_MAJOR

// For Windows (changes declarations in windows.h from ANSI to UTF16)
#define UNICODE 1
#ifndef _UNICODE
# define _UNICODE 1
#endif

#if (defined(unix) || defined(__unix__)) && !defined(__APPLE__) && !defined(__ANDROID__)
# define USE_X11 1
#endif

#ifdef __MINGW64_VERSION_MAJOR
// This is mingw-w64, not mingw or anything else
# define IS_MINGW_W64
#elif defined(__MINGW32__)
// This is mingw, not mingw-w64
# define IS_MINGW
#endif

#ifdef __cplusplus
extern "C" {
#endif


/****** Cross-platform workarounds ******/


// For alloca declaration
#ifdef _WIN32
#include <malloc.h>
#elif defined(__gnu_linux__)
// Doesn't exist in BSD
#include <alloca.h>
#endif


#ifdef _MSC_VER
 /* Microsoft C++ */

 #define DLLEXPORT __declspec(dllexport)

#else
 /* standard C++ compiler/MinGW/MinGW-w64 */

 #define DLLEXPORT

 #ifndef __cdecl
  // #define __cdecl __attribute__((__cdecl__))
  #define __cdecl
 #endif

 /* Replacements for Microsoft extensions (no guarantees about correctness) */
 /* Recent versions of MinGW-w64 declare these, so check for that */
 #ifdef __MINGW32__  // Defined by MinGW and MinGW-w64
  #include <_mingw.h>
 #endif
 #ifndef MINGW_HAS_SECURE_API
  #define memcpy_s(dest, destsize, src, count)  memcpy(dest, src, count)
  #define strcpy_s(dest, destsize, src)  strcpy(dest, src)
  #define wcstombs_s(pReturnValue, mbstr, sizeInBytes, wcstr, count) \
    ((*(pReturnValue) = wcstombs(mbstr, wcstr, count), (*(int *)(pReturnValue) == -1) ? EINVAL : 0))
  #define mbstowcs_s(pReturnValue, wcstr, sizeInWords, mbstr, count) \
    ((*(pReturnValue) = mbstowcs(wcstr, mbstr, count), (*(int *)(pReturnValue) == -1) ? EINVAL : 0))
 #endif

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


/************* Attributes ***************/


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

#ifdef __cplusplus
}
#endif

#endif
