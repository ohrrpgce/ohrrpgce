//gfx.old.h
//started 10/22/09
//old dll backend interface

#ifndef GFX_OLD_H
#define GFX_OLD_H

#include "config.h"

//gfx_*

extern "C" {

struct WindowState
{
	int focused;
	int minimised;
};

//terminate_signal_handler is a pointer to post_terminate_signal, for dynamically linked graphics backends.
//windowicon is platform specific: name of the icon resource on Windows, no meaning yet elsewhere
DLLEXPORT int gfx_init(void (__cdecl *terminate_signal_handler)(void), const char* windowicon, char* info_buffer, int info_buffer_size);
DLLEXPORT void gfx_close();		//put it back how we found it

DLLEXPORT int gfx_getversion();

DLLEXPORT void gfx_showpage(unsigned char *raw, int w, int h); //the main event
DLLEXPORT void gfx_setpal(unsigned int *pal); //set colour palette, DWORD where colors ordered b,g,r,a
DLLEXPORT int gfx_screenshot(const char* fname);
DLLEXPORT void gfx_setwindowed(int iswindow);
DLLEXPORT void gfx_windowtitle(const char* title);
DLLEXPORT WindowState* gfx_getwindowstate();

//gfx_setoption recieves an option name and the following option which may or may not be a related argument
//returns 0 if unrecognised, 1 if recognised but arg is ignored, 2 if arg is gobbled
DLLEXPORT int gfx_setoption(const char* opt, const char* arg);
DLLEXPORT const char* gfx_describe_options();

DLLEXPORT void io_init();
DLLEXPORT void io_pollkeyevents();

DLLEXPORT void io_waitprocessing();

DLLEXPORT void io_keybits(int* keybdarray);
DLLEXPORT void io_updatekeys(int *keybd);
DLLEXPORT void io_mousebits(int& mx, int& my, int& mwheel, int& mbuttons, int& mclicks);
DLLEXPORT int io_setmousevisibility(int visible);
DLLEXPORT void io_getmouse(int& mx, int& my, int& mwheel, int& mbuttons);
DLLEXPORT void io_setmouse(int x, int y);
DLLEXPORT void io_mouserect(int xmin, int xmax, int ymin, int ymax);
DLLEXPORT int io_readjoysane(int, int&, int&, int&);

} //extern "C"

#endif
