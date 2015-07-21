//OHHRPGCE COMMON - Windows-specific routines which require C implementations
//Please read LICENSE.txt for GNU GPL License details and disclaimer of liability

//fb_stub.h MUST be included first, to ensure fb_off_t is 64 bit
#include "fb/fb_stub.h"

#include "windows.h"

#include <locale.h>
#include "os.h"
#include "common.h"

// In os_windows.bas
FBSTRING *get_windows_error (int errcode);


void init_runtime() {
	// Needed for mbstowcs
	if (!setlocale(LC_ALL, "")) {
		// This will actually end up in ?_debug_archive.txt; see init_runtime in os_unix.c
		debug(errError, "setlocale failed");
	}
}

// (This could have been written in os_windows.bas and there's no special reason it isn't)
void os_get_screen_size(int *wide, int *high) {
	//*wide = *high = 0;
	// This gets the size of the primary monitor
	*wide = GetSystemMetrics(SM_CXSCREEN);
	*high = GetSystemMetrics(SM_CYSCREEN);
	debug(errInfo, "get_screen_size: true screen size %dx%d", *wide, *high);

	// This retrieves the size of the 'work area' on the primary monitor,
	// which is the part of the screen not obscured by taskbar and similar toolbars
	RECT rect;
	if (!SystemParametersInfo(SPI_GETWORKAREA, 0, &rect, 0)) {
		FBSTRING *errstr = get_windows_error(GetLastError());
		debug(errError, "get_screen_size failed: %s", errstr->data);
		delete_fbstring(errstr);
		return;
	}
	*wide = rect.right - rect.left;
	*high = rect.bottom - rect.top;
}
