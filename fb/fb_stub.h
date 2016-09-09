/*
 *  This is a stripped down version of FB 1.04's src/rtlib/fb.h
 *  with some other files merged in and some changes.
 *  See readme.txt
 */

#ifndef __FB_H__
#define __FB_H__

#ifdef __cplusplus
extern "C" {
#endif

#ifndef FBCVERSION
#error FBCVERSION must be defined.
#endif

#define ENABLE_MT

/* Must be included before any system headers due to certain #defines */
#include "fb_config.h"


#include <stdarg.h>
#include <stdio.h>
#include <stdint.h>


#define FB_TRUE (-1)
#define FB_FALSE 0

#ifndef FALSE
#define FALSE 0
#endif
#ifndef TRUE
#define TRUE 1
#endif
#ifndef NULL
#define NULL 0
#endif

/* Defines the ASCII code that indicates a two-byte key code.
   A two-byte key code will be returned by GET on SCRN: or INKEY$. */
#define FB_EXT_CHAR           ((char)255)

/* Maximum number of temporary string descriptors. */
#define FB_STR_TMPDESCRIPTORS 256

/* Maximum number of array dimensions. */
#define FB_MAXDIMENSIONS      8

/* Maximum number of temporary array descriptors. */
#define FB_ARRAY_TMPDESCRIPTORS (FB_STR_TMPDESCRIPTORS / 4)

/* The padding width (for PRINT ,). */
#define FB_TAB_WIDTH          14

#if FB_TAB_WIDTH == 8
#define FB_NATIVE_TAB 1
#endif

/* Screen width/height returned by default when native console function failed.
   This is required when an applications output is redirected. */
#define FB_SCRN_DEFAULT_WIDTH  80
#define FB_SCRN_DEFAULT_HEIGHT 25

/* Default colors for console color() function */
#define FB_COLOR_FG_DEFAULT   0x1
#define FB_COLOR_BG_DEFAULT   0x2

/* Number of reserved file handles. 0: SCRN, 1: LPT1 */
#define FB_RESERVED_FILES     2

/* Maximum number of file handles. */
#define FB_MAX_FILES          (FB_RESERVED_FILES + 255)

/* File buffer size (for buffered read?). */
#define FB_FILE_BUFSIZE       8192

/* Max length to allocated for a temporary buffer on stack */
#define FB_LOCALBUFF_MAXLEN   32768

#ifndef HOST_WIN32
	/* Maximum path length for Non-Win32 targets. For Win32 targets, this
	   value will be set automatically by windows.h. */
	#define MAX_PATH    1024
#endif

/* Convert char to int without sign-extension. */
#define FB_CHAR_TO_INT(ch)  ((int) ((unsigned) (unsigned char) (ch)))


/* The following has been added to replace dependance on fb_unix.h, fb_win32.h etc (bad idea?) */

#if defined HOST_WIN32

	#include <io.h>

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

	// Copied from rtlib/unix/fb_unix.h

	#include <unistd.h>

	#define FBCALL

	/* Relying on -D_FILE_OFFSET_BITS=64 to transparently remap to off64_t */
	#if !defined _FILE_OFFSET_BITS || _FILE_OFFSET_BITS != 64
	#error Expected _FILE_OFFSET_BITS=64
	#endif
	typedef off_t fb_off_t;

#else
	#error "XBOX and DOS not supported by the OHRRPGCE"
#endif


FBCALL void fb_Lock( void );
FBCALL void fb_Unlock( void );
FBCALL void fb_StrLock( void );
FBCALL void fb_StrUnlock( void );
FBCALL void fb_GraphicsLock  ( void );
FBCALL void fb_GraphicsUnlock( void );
#define FB_LOCK()      fb_Lock()
#define FB_UNLOCK()    fb_Unlock()
#define FB_STRLOCK()   fb_StrLock()
#define FB_STRUNLOCK() fb_StrUnlock()
#define FB_GRAPHICS_LOCK()   fb_GraphicsLock()
#define FB_GRAPHICS_UNLOCK() fb_GraphicsUnlock()


#define FB_WCHAR char
typedef uint32_t UTF_32;
typedef uint16_t UTF_16;
typedef uint8_t  UTF_8;


/* internal lists */
typedef struct _FB_LISTELEM {
    struct _FB_LISTELEM    *prev;
    struct _FB_LISTELEM    *next;
} FB_LISTELEM;

typedef struct _FB_LIST {
    int                cnt;      /* Number of used elements */
    FB_LISTELEM        *head;    /* First used element */
    FB_LISTELEM        *tail;    /* Last used element */
    FB_LISTELEM        *fhead;   /* First free element */
} FB_LIST;


#include "fb_array.h"
#include "fb_string.h"
#include "fb_file.h"
#include "fb_device.h"

typedef FBCALL int (*FnDummy)();

// Although the signatures of these functions have sometimes changed, the contents of
// this struct hasn't changed since 2007 (as of 20160504)
typedef struct FB_HOOKSTB {
	FnDummy inkeyproc;
	FnDummy getkeyproc;
	FnDummy keyhitproc;
	FnDummy clsproc;
	FnDummy colorproc;
	FnDummy locateproc;
	FnDummy widthproc;
	FnDummy getxproc;
	FnDummy getyproc;
	FnDummy getxyproc;
	FnDummy getsizeproc;
	FnDummy printbuffproc;
	FnDummy printbuffwproc;
	FnDummy readstrproc;
	FnDummy multikeyproc;
	FnDummy getmouseproc;
	FnDummy setmouseproc;
	FnDummy inproc;
	FnDummy outproc;
	FnDummy viewupdateproc;
	FnDummy lineinputproc;
	FnDummy lineinputwproc;
	FnDummy readxyproc;
	FnDummy sleepproc;
	FnDummy isredirproc;
	FnDummy pagecopyproc;
	FnDummy pagesetproc;
} FB_HOOKSTB;

#if FBCVERSION < 240
	typedef uintptr_t FB_TLSENTRY;  // Platform dependent, e.g. pthread_key_t on Unix
	#define FB_TLSKEYS 5
#endif

// This is also fairly stable. We need this to access the file table, fileTB.
typedef struct FB_RTLIB_CTX_ {
	int             argc;
	char          **argv;
	FBSTRING        null_desc;
	char           *errmsg;
#if FBCVERSION < 240
	FnDummy         pfnDevOpenHook;
	FB_TLSENTRY     tls_ctxtb[FB_TLSKEYS];
#endif
	FB_HOOKSTB      hooks;
	FB_FILE         fileTB[FB_MAX_FILES];
	/* We don't care about anything after this point */
	int             do_file_reset;
	int             lang;
#if FBCVERSION >= 1020
	void          (*exit_gfxlib2)(void);
#endif
} FB_RTLIB_CTX;

extern FB_RTLIB_CTX __fb_ctx;


#ifdef __cplusplus
}
#endif

#endif /*__FB_H__*/
