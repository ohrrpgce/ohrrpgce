// This file contains Unix-specific OS routines which should only be linked into
// Game and Custom, not the commandline utils, namely X11 stuff.
// (However this module is linked on all Unices, including OSX, not just ones using X11)
//
// Please read LICENSE.txt for GNU GPL License details and disclaimer of liability

//fb_stub.h MUST be included first, to ensure fb_off_t is 64 bit
#include "fb/fb_stub.h"
#include "config.h"

#ifdef USE_X11
#include <X11/Xlib.h>
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
