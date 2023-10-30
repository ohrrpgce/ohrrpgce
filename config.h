/* OHRRPGCE - Configuration/platform specific/important macros
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
 * Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
 */

// Defining CONFIG_H breaks some MSVC++ or Windows header!
#ifndef CONFIG_H__
#define CONFIG_H__

#ifdef __MINGW32__  // MinGW or MinGW-w64
// This causes MinGW and MinGW-w64 to switch to C99-compliant printf- and scanf-family
// functions (causes __USE_MINGW_ANSI_STDIO to be defined). See below.
// According to https://svn.python.org/projects/python/trunk/configure
// defining _POSIX_C_SOURCE causes lots of broken stuff on virtually every Unix, so avoid it.
# define _POSIX_C_SOURCE 200809L
#endif

//fb_stub.h MUST be included first, to ensure fb_off_t is 64 bit
#include "fb/fb_stub.h"
#include <stdint.h>
#include <stdlib.h>  // For __MINGW64_VERSION_MAJOR

#define YES -1
#define NO  0

// For Windows (changes declarations in windows.h from ANSI to UTF16)
/*
#define UNICODE 1
#ifndef _UNICODE
# define _UNICODE 1
#endif
*/

#if defined(__EMSCRIPTEN__) || defined(HOST_FB_BLACKBOX)
# define MINIMAL_OS
#endif

#if defined(MINIMAL_OS)
# define NO_TLS
#endif

#if (defined(unix) || defined(__unix__)) && !defined(__APPLE__) && !defined(__ANDROID__) && !defined(__EMSCRIPTEN__) && !defined(NO_X11)
# define USE_X11
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

 // MS only implemented standards-compliant [v]snprintf in VC++ 2015/Win10! So
 // much for caring about security! _[v]snprintf is available but is NOT
 // equivalent to [v]snprintf: if the buffer is too short it doesn't add a null
 // byte, and returns -1 instead of the required buffer size.
 // NOTE: MinGW provides replacements for all printf and scanf functions,
 // to use standard formatting codes. I'm not aware whether there are other
 // behaviour changes aside from [v]snprintf.
 #if _MSC_VER < 1900
  // Defined in lib/msvcrt_compat.c
  int c99_vsnprintf(char *outBuf, size_t size, const char *format, va_list ap);
  int c99_snprintf(char *outBuf, size_t size, const char *format, ...);
  #define vsnprintf c99_vsnprintf
  #define snprintf c99_snprintf
 #endif

 // VC++: disable warnings for not using *_s functions, many of those _s
 // functions have their own alternative security problems, and we should avoid
 // them because they aren't available in MinGW.
 // We can use them even on older Windows, because VC++-compiled code doesn't
 // use the system msvcrt.dll (in fact, gfx_directx.dll statically links the CRT)
 #pragma warning(disable:4996)

#else
 /* standard C++ compiler/MinGW/MinGW-w64 */

 #define DLLEXPORT

 #ifndef __cdecl
  // #define __cdecl __attribute__((__cdecl__))
  #define __cdecl
 #endif

 #if defined(IS_MINGW) || defined(IS_MINGW_W64)
  // As noted above, [v]snprintf in msvcrt.dll is non-standard before Windows 10
  // (VC++ 2015).  Luckily mingw[w-64] provide replacements. vsnprintf is
  // redirected to __mingw_vsnprintf if this is defined, which mingw does by
  // default but mingw-w64 doesn't.
  // But apparently MinGW no longer want people to define __USE_MINGW_ANSI_STDIO
  // (although that was what they explicitly told people to do in the past),
  // so we instead #define _POSIX_SOURCE, above.
  /*
  #ifndef __USE_MINGW_ANSI_STDIO
   #define __USE_MINGW_ANSI_STDIO 1
  #endif
  */
 #endif

 /* Microsoft secure _s extension functions are not present in older versions of
    msvcrt.dll (I think they are Vista+), so we can't use them even on Windows.
    However, recent versions of MinGW-w64 declare them #ifdef
    MINGW_HAS_SECURE_API, which is declared by default. So disable that to avoid
    accidents. */
 #ifdef __MINGW32__  // Defined by MinGW and MinGW-w64
  #include <_mingw.h>
  #undef MINGW_HAS_SECURE_API
 #endif
 // Replacements (no guarantees about correctness). Avoid use.
 /*
 #define memcpy_s(dest, destsize, src, count)  memcpy(dest, src, count)
 #define strcpy_s(dest, destsize, src)  strcpy(dest, src)
 #define wcstombs_s(pReturnValue, mbstr, sizeInBytes, wcstr, count) \
   ((*(pReturnValue) = wcstombs(mbstr, wcstr, count), (*(int *)(pReturnValue) == -1) ? EINVAL : 0))
 #define mbstowcs_s(pReturnValue, wcstr, sizeInWords, mbstr, count) \
   ((*(pReturnValue) = mbstowcs(wcstr, mbstr, count), (*(int *)(pReturnValue) == -1) ? EINVAL : 0))
 */

#endif


/* I will use boolint in declarations of C/C++ functions where we would like to use
   bool (C/C++) or boolean (FB), but shouldn't, to support FB pre-1.04. So instead,
   use boolint on both sides, to show intention but prevent accidental C/C++ bool usage.
*/
typedef int boolint;

#ifndef __cplusplus
  // When compiling C, MSVC only has _Bool and stdbool.h since VS 2015.
  // (Visual C++ supports bool in C++ since VS 5.0 (1997))
  #if !defined(_MSC_VER) || _MSC_VER >= 1800
    #include <stdbool.h>
  #elif defined(_MSC_VER)
    typedef uint8_t bool;
    #define false 0
    #define true 1
  #endif
#endif

#if defined(_WIN32) || defined(WIN32)
# define SLASH '\\'
# define ispathsep(chr) ((chr) == '/' || (chr) == '\\')
#else
# define SLASH '/'
# define ispathsep(chr) ((chr) == '/')
#endif


/************* Attributes ***************/


// __has_attribute is supported since gcc 5.0 and clang 2.9. That's recent
// but I don't think we care if the attributes accidentally don't get used.
# ifndef __has_attribute
#  define __has_attribute(x) 0
# endif

// GCC only added __has_builtin in GCC 10
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
# define pure
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

// warn_if_result_unused: like [[nodiscard]] in C++11
#if __has_attribute(warn_unused_result)
# define warn_if_result_unused __attribute__ ((__warn_unused_result__))
#else
# define warn_if_result_unused
#endif

#if __has_attribute(format)
// Under MinGW, depending on __USE_MINGW_ANSI_STDIO (printf provided by mingw or
// msvcrt) printf accepts different format codes; __MINGW_PRINTF_FORMAT is set
// to the correct style.
# ifdef __MINGW_PRINTF_FORMAT
#  define format_chk(fmt_arg) __attribute__ ((__format__ (__MINGW_PRINTF_FORMAT, fmt_arg, fmt_arg + 1)))
# else
#  define format_chk(fmt_arg) __attribute__ ((__format__ (__printf__, fmt_arg, fmt_arg + 1)))
# endif
#else
# define format_chk(fmt_arg)
#endif

// disable_ASan: put in front of a function definition to not instrument it when compiling
// with AddressSanitizer (scons asan=1)
#if defined(__clang__) || defined (__GNUC__)
# define disable_ASan __attribute__ ((no_sanitize_address))
#else
# define disable_ASan
#endif

#ifdef __cplusplus
}
#endif

#endif
