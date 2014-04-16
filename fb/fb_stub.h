/*
 *  This is a stripped down version of FB's src/rtlib/fb.h
 *
 *  libfb - FreeBASIC's runtime library
 *	Copyright (C) 2004-2010 The FreeBASIC development team.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *  As a special exception, the copyright holders of this library give
 *  you permission to link this library with independent modules to
 *  produce an executable, regardless of the license terms of these
 *  independent modules, and to copy and distribute the resulting
 *  executable under terms of your choice, provided that you also meet,
 *  for each linked independent module, the terms and conditions of the
 *  license of that module. An independent module is a module which is
 *  not derived from or based on this library. If you modify this library,
 *  you may extend this exception to your version of the library, but
 *  you are not obligated to do so. If you do not wish to do so, delete
 *  this exception statement from your version.
 */

#ifndef __FB_H__
#define __FB_H__

#ifdef __cplusplus
extern "C" {
#endif

/* Must be included before any system headers due to certain #defines */
#include "fb_config.h"


    /* =================================================================
     * RTLIB configuration
     * ================================================================= */

    /** Defines the ASCII code that indicates a two-byte key code.
     *
     * A two-byte key code will be returned by GET on SCRN: or INKEY$.
     */
#define FB_EXT_CHAR           ((char)255)

    /** Maximum number of temporary string descriptors.
     */
#define FB_STR_TMPDESCRIPTORS 256

    /** Maximum number of array dimensions.
     */
#define FB_MAXDIMENSIONS      8

    /** Maximum number of temporary array descriptors.
     */
#define FB_ARRAY_TMPDESCRIPTORS (FB_STR_TMPDESCRIPTORS / 4)

    /** The padding width (for PRINT ,).
     */
#define FB_TAB_WIDTH          14

    /** Screen width returned by default when native console function failed.
     *
     * This is required when an applications output is redirected.
     */
#define FB_SCRN_DEFAULT_WIDTH  80

    /** Screen height returned by default when native console function failed.
     *
     * This is required when an applications output is redirected.
     */
#define FB_SCRN_DEFAULT_HEIGHT 25

    /** Number of reserved file handles.
     *
     * Index        Usage:
     * 0            SCRN:
     * 1            LPT1:
     */
#define FB_RESERVED_FILES     2

    /** Maximum number of file handles.
     */
#define FB_MAX_FILES          (FB_RESERVED_FILES + 255)

    /** File buffer size (for buffered read ?).
     */
#define FB_FILE_BUFSIZE       8192

    /** Max length to allocated for a temporary buffer on stack
     */
#define FB_LOCALBUFF_MAXLEN	  32768

    /* =================================================================
     * RTLIB default values
     * ================================================================= */

    /** BASIC's TRUE value.
     */
#define FB_TRUE -1

    /** BASIC's FALSE value.
     */
#define FB_FALSE 0

    /** FALSE value for pre C99.
     */
#ifndef FALSE
#define FALSE    0
#endif

    /** TRUE value for pre C99.
     */
#ifndef TRUE
#define TRUE    1
#endif

    /** NULL value for pre C99.
     */
#ifndef NULL
#define NULL     0
#endif

#ifndef FB_LOCK
    /** Acquire a global semaphore (recursive mutex).
     */
# define FB_LOCK()
#endif
#ifndef FB_UNLOCK
    /** Release a global semaphore (recursive mutex).
     */
# define FB_UNLOCK()
#endif

#ifndef FB_TLSENTRY
    /** Define a TLS (Thread local storage) slot.
     */
# define FB_TLSENTRY uintptr_t
#endif

#ifndef FB_TLSALLOC
# define FB_TLSALLOC(key) key = NULL
#endif

#ifndef FB_TLSFREE
# define FB_TLSFREE(key) key = NULL
#endif

#ifndef FB_TLSSET
    /** Set the value of a TLS (Thread local storage) slot.
     */
# define FB_TLSSET(key,value) key = (FB_TLSENTRY)value
#endif
#ifndef FB_TLSGET
    /** Get the value from a TLS (Thread local storage) slot.
     */
# define FB_TLSGET(key) key
#endif

#ifndef FB_THREADID
# define FB_THREADID int
#endif

#ifndef FB_BINARY_NEWLINE
    /** The "NEW LINE" string required for printer I/O
     *
     * The printer always requires both CR and LF.
     */
#define FB_BINARY_NEWLINE "\r\n"
#define FB_BINARY_NEWLINE_WSTR _LC("\r\n")
#endif

#ifndef FB_NEWLINE
    /** The "NEW LINE" character used for all I/O.
     *
     * This is LF here because FB relies on the C RTL which only knows
     * LF as line-end character.
     */
#define FB_NEWLINE "\n"
#define FB_NEWLINE_WSTR _LC("\n")
#endif

#ifndef FB_LL_FMTMOD
    /** LONG LONG format modifier.
     *
     * This is the default "long long" format modifier for use with the
     * *printf functions.
     */
#define FB_LL_FMTMOD "ll"
#endif


/* The following has been added to replace dependance on fb_unix.h, fb_win32.h etc (bad idea?) */

#if defined HOST_WIN32

	#include <io.h>
	#include <stdio.h>

	// Copied from rtlib/win32/fb_win32.h

	#ifdef HOST_X86
	#define FBCALL __stdcall
	#else
	#define FBCALL
	#endif

	#ifdef HOST_CYGWIN
	typedef off_t fb_off_t;
	#else
	/* MinGW-w64 recognizes -D_FILE_OFFSET_BITS=64, but MinGW does not, so we
	can't be sure that ftello() really maps to the 64bit version...
	so we have to do it manually. */
	typedef long long fb_off_t;
	#define fseeko(stream, offset, whence) fseeko64(stream, offset, whence)
	#define ftello(stream)                 ftello64(stream)
	#endif

#elif defined HOST_UNIX

	#include <sys/types.h>

	// Copied from rtlib/unix/fb_unix.h

	#define FBCALL

	/* Relying on -D_FILE_OFFSET_BITS=64 to transparently remap to off64_t */
	#if !defined _FILE_OFFSET_BITS || _FILE_OFFSET_BITS != 64
	#error Expected _FILE_OFFSET_BITS=64
	#endif
	typedef off_t fb_off_t;

#else
	#error "XBOX and DOS not supported by the OHRRPGCE"
#endif


#define FB_WCHAR char

#include "fb_string.h"
#include "fb_file.h"
#include "fb_device.h"

typedef struct FB_RTLIB_CTX_ {
	int 			argc;
	char 			**argv;
	FBSTRING 		null_desc;
	char 			*error_msg;
	FnDevOpenHook	pfnDevOpenHook;
  /* And some other stuff... but not going to include all other headers
	FB_HOOKSTB		hooks;
	FB_TLSENTRY 	tls_ctxtb[FB_TLSKEYS];
	FB_FILE 		fileTB[FB_MAX_FILES];
	int				do_file_reset;
	int				lang;
  */
} FB_RTLIB_CTX;

extern FB_RTLIB_CTX __fb_ctx;

#ifdef __cplusplus
}
#endif

#endif /*__FB_H__*/
