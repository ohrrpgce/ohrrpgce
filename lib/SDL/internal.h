#pragma once

#include "../../config.h"

#define SDL_arraysize(array)    (sizeof(array)/sizeof(array[0]))

#ifdef USE_X11

#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

Atom X11_GetSDLCutBufferClipboardType(Display *display);

extern bool selection_waiting;

#endif
