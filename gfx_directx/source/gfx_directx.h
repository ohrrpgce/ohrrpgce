//gfx_directx.h
//started 10/22/09
//provides directx 9.0c shader enabled graphics backend for the ohrrpgce

#ifndef GFX_DIRECTX_H
#define GFX_DIRECTX_H

//gfx_*
#define EXPORTDLL __declspec(dllexport)

extern "C" {

struct WindowState
{
	int focused;
	int minimised;
};

//terminate_signal_handler is a pointer to post_terminate_signal, for dynamically linked graphics backends.
//windowicon is platform specific: name of the icon resource on Windows, no meaning yet elsewhere
EXPORTDLL int gfx_init(void (__cdecl *terminate_signal_handler)(void) , const char* windowicon, char* info_buffer, int info_buffer_size);
EXPORTDLL void gfx_close();		//put it back how we found it

EXPORTDLL int gfx_getversion();

EXPORTDLL void gfx_showpage(unsigned char *raw, int w, int h); //the main event
EXPORTDLL void gfx_setpal(unsigned int *pal); //set colour palette, DWORD where colors ordered b,g,r,a
EXPORTDLL int gfx_screenshot(const char* fname);
EXPORTDLL void gfx_setwindowed(int iswindow);
EXPORTDLL void gfx_windowtitle(const char* title);
EXPORTDLL WindowState* gfx_getwindowstate();

//gfx_setoption recieves an option name and the following option which may or may not be a related argument
//returns 0 if unrecognised, 1 if recognised but arg is ignored, 2 if arg is gobbled
EXPORTDLL int gfx_setoption(const char* opt, const char* arg);
EXPORTDLL const char* gfx_describe_options();

EXPORTDLL void io_init();
EXPORTDLL void io_pollkeyevents();

//EXPORTDLL void io_waitprocessing();

//EXPORTDLL void io_keybits(int* keybdarray);
EXPORTDLL void io_updatekeys(int *keybd);
//EXPORTDLL void io_mousebits(int& mx, int& my, int& mwheel, int& mbuttons, int& mclicks);
EXPORTDLL int io_setmousevisibility(int visible);
EXPORTDLL void io_getmouse(int& mx, int& my, int& mwheel, int& mbuttons);
EXPORTDLL void io_setmouse(int x, int y);
EXPORTDLL void io_mouserect(int xmin, int xmax, int ymin, int ymax);
EXPORTDLL int io_readjoysane(int, int&, int&, int&);

} //extern "C"

#endif