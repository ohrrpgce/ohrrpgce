/* optional source file for linking this backend statically */

#include "gfx.old.h"

extern "C" {

extern int (*Gfx_init)(void (__cdecl *terminate_signal_handler)(void), const char* windowicon, char* info_buffer, int info_buffer_size);
extern void (*Gfx_close)();		
extern int (*Gfx_getversion)();
extern void (*Gfx_showpage)(unsigned char *raw, int w, int h); 
extern void (*Gfx_setpal)(unsigned int *pal); 
extern int (*Gfx_screenshot)(const char* fname);
extern void (*Gfx_setwindowed)(int iswindow);
extern void (*Gfx_windowtitle)(const char* title);
extern WindowState*(* Gfx_getwindowstate)();
extern int (*Gfx_setoption)(const char* opt, const char* arg);
extern const char* (*Gfx_describe_options)();
extern void (*Io_init)();
extern void (*Io_pollkeyevents)();
extern void (*Io_waitprocessing)();
extern void (*Io_keybits)(int* keybdarray);
extern void (*Io_updatekeys)(int *keybd);
extern void (*Io_mousebits)(int& mx, int& my, int& mwheel, int& mbuttons, int& mclicks);
extern int (*Io_setmousevisibility)(int visible);
extern void (*Io_getmouse)(int& mx, int& my, int& mwheel, int& mbuttons);
extern void (*Io_setmouse)(int x, int y);
extern void (*Io_mouserect)(int xmin, int xmax, int ymin, int ymax);
extern int (*Io_readjoysane)(int, int&, int&, int&);

int gfx_sdlpp_setprocptrs() {
  Gfx_init = gfx_init;
  Gfx_close = gfx_close;
  Gfx_getversion = gfx_getversion;
  Gfx_showpage = gfx_showpage;
  Gfx_setpal = gfx_setpal;
  Gfx_screenshot = gfx_screenshot;
  Gfx_setwindowed = gfx_setwindowed;
  Gfx_windowtitle = gfx_windowtitle;
  Gfx_getwindowstate = gfx_getwindowstate;
  Gfx_setoption = gfx_setoption;
  Gfx_describe_options = gfx_describe_options;
  Io_init = io_init;
  Io_pollkeyevents = io_pollkeyevents;
  Io_waitprocessing = io_waitprocessing;
  Io_keybits = io_keybits;
  Io_updatekeys = io_updatekeys;
  Io_mousebits = io_mousebits;
  Io_setmousevisibility = io_setmousevisibility;
  Io_getmouse = io_getmouse;
  Io_setmouse = io_setmouse;
  Io_mouserect = io_mouserect;
  Io_readjoysane = io_readjoysane;

  return true;
}

}
