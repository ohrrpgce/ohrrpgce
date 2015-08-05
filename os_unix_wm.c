// This file contains Unix-specific OS routines which should only be linked into
// Game and Custom, not the commandline utils, namely X11 stuff.
// (However this module is linked on all Unices, including OSX, not just ones using X11)
//
// Please read LICENSE.txt for GNU GPL License details and disclaimer of liability


#if !defined(__APPLE__) && !defined(__ANDROID__)
#define X_WINDOWS 1
#endif

//fb_stub.h MUST be included first, to ensure fb_off_t is 64 bit
#include "fb/fb_stub.h"

#ifdef X_WINDOWS
#include <X11/Xlib.h>
#endif

#include "common.h"
#include "os.h"


//==========================================================================================
//                                          X11
//==========================================================================================


#ifdef X_WINDOWS

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
	XCloseDisplay(display);
}

#else

// Not implemented, will fallback to gfx_get_screen_size
void os_get_screen_size(int *wide, int *high) {
	*wide = *high = 0;
}

#endif
