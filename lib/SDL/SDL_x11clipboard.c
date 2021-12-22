/*
  X11 Clipboard routines -- adapted from SDL 2 for the OHRRPGCE.
  Copyright 2017. This file is distributed under the original license, as
  follows, rather than the OHRRPGCE's license.
  Simple DirectMedia Layer
  Copyright (C) 1997-2017 Sam Lantinga <slouken@libsdl.org>

  This software is provided 'as-is', without any express or implied
  warranty.  In no event will the authors be held liable for any damages
  arising from the use of this software.

  Permission is granted to anyone to use this software for any purpose,
  including commercial applications, and to alter it and redistribute it
  freely, subject to the following restrictions:

  1. The origin of this software must not be misrepresented; you must not
     claim that you wrote the original software. If you use this software
     in a product, an acknowledgment in the product documentation would be
     appreciated but is not required.
  2. Altered source versions must be plainly marked as such, and must not be
     misrepresented as being the original software.
  3. This notice may not be removed or altered from any source distribution.
*/

#include <limits.h> /* For INT_MAX */
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>

#include "internal.h"
#include "../../errorlog.h"

void fb_Sleep(int msecs);
double fb_Timer();

/* If you don't support UTF-8, you might use XA_STRING here */
#define TEXT_FORMAT XInternAtom(display, "UTF8_STRING", False)
//#define TEXT_FORMAT XA_STRING


/* We use our own cut-buffer for intermediate storage instead of  
   XA_CUT_BUFFER0 because their use isn't really defined for holding UTF8. */ 
Atom
X11_GetSDLCutBufferClipboardType(Display *display)
{
    return XInternAtom(display, "SDL_CUTBUFFER", False);
}

int
X11_SetClipboardText(Display *display, Window window, const char *text)
{
    Atom format;
    Atom XA_CLIPBOARD = XInternAtom(display, "CLIPBOARD", 0);

    if (window == None) {
        debug(errError, "X11_SetClipboardText: Couldn't find a window to own the selection");
        return -1;
    }

    /* Save the selection on the root window */
    format = TEXT_FORMAT;
    XChangeProperty(display, DefaultRootWindow(display),
        X11_GetSDLCutBufferClipboardType(display), format, 8, PropModeReplace,
        (const unsigned char *)text, strlen(text));

    if (XA_CLIPBOARD != None &&
        XGetSelectionOwner(display, XA_CLIPBOARD) != window) {
        XSetSelectionOwner(display, XA_CLIPBOARD, window, CurrentTime);
    }

    if (XGetSelectionOwner(display, XA_PRIMARY) != window) {
        XSetSelectionOwner(display, XA_PRIMARY, window, CurrentTime);
    }
    return 0;
}

char *
X11_GetClipboardText(Display *display, Window window, void (*event_loop_callback)())
{
    Atom format;
    Window owner;
    Atom selection;
    Atom seln_type;
    int seln_format;
    unsigned long nbytes;
    unsigned long overflow;
    unsigned char *src;
    char *text;
    Atom XA_CLIPBOARD = XInternAtom(display, "CLIPBOARD", 0);
    if (XA_CLIPBOARD == None) {
        debug(errError, "X11_GetClipboardText: Couldn't access X clipboard");
        return strdup("");
    }

    text = NULL;

    format = TEXT_FORMAT;
    owner = XGetSelectionOwner(display, XA_CLIPBOARD);
    if (owner == None) {
        /* Fall back to ancient X10 cut-buffers which do not support UTF8 strings*/
        owner = DefaultRootWindow(display);
        selection = XA_CUT_BUFFER0;
        format = XA_STRING;
    } else if (owner == window) {
        owner = DefaultRootWindow(display);
        selection = X11_GetSDLCutBufferClipboardType(display);
    } else {
        /* Request that the selection owner copy the data to our window */
        owner = window;
        selection = XInternAtom(display, "SDL_SELECTION", False);
        XConvertSelection(display, XA_CLIPBOARD, format, selection, owner,
            CurrentTime);

        // Wait for an SelectionNotify event
        if (!event_loop_callback) {
            fb_Sleep(40);
        } else {
            /* When using synergy on Linux and when data has been put in the clipboard
               on the remote (Windows anyway) machine then selection_waiting may never
               be set to False. Time out after a while. */
            double waitStart = fb_Timer();
            selection_waiting = true;
            while (selection_waiting) {
                event_loop_callback();
                /* Wait 300ms for a clipboard response. */
                if (fb_Timer() - waitStart > 0.3) {
                    selection_waiting = false;
                    debug(errDebug, "X11_GetClipboardText: Clipboard timeout");
                    /* We need to set the clipboard text so that next time we won't
                       timeout, otherwise we will hang on every call to this function. */
                    X11_SetClipboardText(display, window, "");
                    return strdup("");
                }
                fb_Sleep(5);
            }
        }
    }

    if (XGetWindowProperty(display, owner, selection, 0, INT_MAX/4, False,
            format, &seln_type, &seln_format, &nbytes, &overflow, &src)
            == Success) {
        if (seln_type == format) {
            text = (char *)malloc(nbytes+1);
            if (text) {
                memcpy(text, src, nbytes);
                text[nbytes] = '\0';
            }
        }
        XFree(src);
    }

    if (!text) {
        text = strdup("");
    }

    return text;
}

bool
X11_HasClipboardText(Display *display, Window window, void (*event_loop_callback)())
{
    bool result = false;
    char *text = X11_GetClipboardText(display, window, event_loop_callback);
    if (text) {
        result = text[0] != '\0';
        free(text);
    }
    return result;
}
