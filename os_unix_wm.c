/* OHRRPGCE - Unix versions of OS-specific window manager routines
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
 * Please read LICENSE.txt for GPL License details and disclaimer of liability
 *
 * This file contains OS-specific routines which should only be linked into
 * Game and Custom, not the commandline utils; namely, GUI/WM-related stuff.
 */

//fb_stub.h MUST be included first, to ensure fb_off_t is 64 bit
#include "fb/fb_stub.h"
#include "config.h"
#include <stdlib.h>

#ifdef USE_X11
#include <X11/Xlib.h>
#define USE_XINERAMA
#ifdef USE_XINERAMA
#include <X11/extensions/Xinerama.h>
#endif
#endif

#ifdef __APPLE__
// For CoreGraphics (part of Carbon)
//#include <CoreGraphics/CGDirectDisplay.h>
// Apparently you can't include CoreGraphics directly in OS 10.7 and older;
// so include ApplicationServices which includes CoreGraphics
#include <ApplicationServices/ApplicationServices.h>
#endif

#include "misc.h"
#include "os.h"


//==========================================================================================
//                                          OSX
//==========================================================================================

#ifdef __APPLE__

void os_get_screen_size(int *wide, int *high) {
	*wide = *high = 0;

	// Return the size of the main display (or the largest mirrored screen of the
	// main display), which is the one that is used when fullscreening.
	// OSX 10.0 or later
	uint32_t numDisplays;
	CGDirectDisplayID displays[1];
	CGError err = CGGetActiveDisplayList(1, displays, &numDisplays);
	if (err) {
		debug(errError, "CGGetActiveDisplayList failed: %d", err);
		return;
	}
	CGRect rect = CGDisplayBounds(displays[0]);
	*wide = rect.size.width;
	*high = rect.size.height;
}


//==========================================================================================
//                                          X11
//==========================================================================================


#elif defined(USE_X11)

int PrintXError(Display *dpy, XErrorEvent *event, FILE *fp);

static int X_fatal_error_handler(Display *dpl) {
	debugc(errDie, "Xlib fatal IO error handler called!");
	/* not reached */
	return 0;
}

static int X_error_handler(Display *dpl, XErrorEvent *event) {
	// Print the error to a buffer and log it and print to stderr, like
	// Xlib errors normally are
	char *buf;
	size_t bufsize;
	FILE *fbuf = open_memstream(&buf, &bufsize);
	fprintf(fbuf, "Xlib error handler called:\n");
	PrintXError(dpl, event, fbuf);
	fclose(fbuf);
	fputs(buf, stderr);
	debugc(errError, buf);
	free(buf);
	return 0;  //ignored
}

void set_X11_error_handlers() {
	// The default Xlib error handler prints the error to stderr and calls
	// exit(). That sucks, it would mean that e.g. if something goes wrong
	// when the backend tries to create the window we can't try again with
	// other settings or backend. So set our own handler.
	XSetErrorHandler(X_error_handler);

	// We can't stop Xlib from killing the program if the connection to
	// the X server is lost, or other fatal error, but we can at least
	// try to shut down cleanly
	XSetIOErrorHandler(X_fatal_error_handler);
}

// BUG: if you have multiple displays and are using Xinerama (likely) this returns
// the size of the whole desktop (bounding box of all displays) rather than the main display.
void os_get_screen_size(int *wide, int *high) {
	Display *display;
	display = XOpenDisplay(NULL);  // uses display indicated by $DISPLAY env var
	if (!display) {
		debug(errError, "get_screen_size: XOpenDisplay failed");
		*wide = *high = 0;
		return;
	}

	int screen = DefaultScreen(display);
	*wide = DisplayWidth(display, screen);
	*high = DisplayHeight(display, screen);

#ifdef USE_XINERAMA
	// This fixes the above mentioned bug, but I don't really want to add
	// another dependency that's only useful if using gfx_sdl or gfx_fb,
	// which you'd only likely do when you can't use SDL2. And even SDL2 can
	// be compiled without Xinerama.
	// (Requires Xinerama library be linked.)
	int screennum = 0;
	XineramaScreenInfo *screenarray = XineramaQueryScreens(display, &screennum);
	if (screenarray) {
		//debuginfo("Got %d screens", screennum);
		*wide = screenarray[0].width;
		*high = screenarray[0].height;
		XFree(screenarray);
	}
#endif

	XCloseDisplay(display);
}

#else

// Not implemented, will fallback to gfx_get_screen_size
void os_get_screen_size(int *wide, int *high) {
	*wide = *high = 0;
}

#endif
