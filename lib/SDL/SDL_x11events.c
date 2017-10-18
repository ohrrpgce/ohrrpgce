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

bool selection_waiting = false;


// Originally part of X11_DispatchEvent() in SDL
void
X11_HandleClipboardEvent(Display *display, XEvent *xevent)
{
#if 0
    printf("type = %d display = %d window = %d\n",
           xevent->type, xevent->xany.display, xevent->xany.window);
#endif

    switch (xevent->type) {

    /* Copy the selection from our own CUTBUFFER to the requested property */
    case SelectionRequest: {
            XSelectionRequestEvent *req;
            XEvent sevent = {};
            int seln_format;
            unsigned long nbytes;
            unsigned long overflow;
            unsigned char *seln_data;

            req = &xevent->xselectionrequest;
#ifdef DEBUG_XEVENTS
            printf("SelectionRequest (requestor = %ld, target = %ld)\n",
                req->requestor, req->target);
#endif

            sevent.xany.type = SelectionNotify;
            sevent.xselection.selection = req->selection;
            sevent.xselection.target = None;
            sevent.xselection.property = None;
            sevent.xselection.requestor = req->requestor;
            sevent.xselection.time = req->time;

            if (XGetWindowProperty(display, DefaultRootWindow(display),
                    X11_GetSDLCutBufferClipboardType(display), 0, INT_MAX/4, False, req->target,
                    &sevent.xselection.target, &seln_format, &nbytes,
                    &overflow, &seln_data) == Success) {
                Atom XA_TARGETS = XInternAtom(display, "TARGETS", 0);
                if (sevent.xselection.target == req->target) {
                    XChangeProperty(display, req->requestor, req->property,
                        sevent.xselection.target, seln_format, PropModeReplace,
                        seln_data, nbytes);
                    sevent.xselection.property = req->property;
                } else if (XA_TARGETS == req->target) {
                    Atom SupportedFormats[] = { XA_TARGETS, sevent.xselection.target };
                    XChangeProperty(display, req->requestor, req->property,
                        XA_ATOM, 32, PropModeReplace,
                        (unsigned char*)SupportedFormats,
                        SDL_arraysize(SupportedFormats));
                    sevent.xselection.property = req->property;
                    sevent.xselection.target = XA_TARGETS;
                }
                XFree(seln_data);
            }
            XSendEvent(display, req->requestor, False, 0, &sevent);
            XSync(display, False);
        }
        break;

    case SelectionNotify: {
#ifdef DEBUG_XEVENTS
            printf("SelectionNotify (requestor = %ld, target = %ld)\n",
                xevent->xselection.requestor, xevent->xselection.target);
#endif

#if 0
            // Support for dragging-and-dropping
            //SDL_WindowData *data = ...
            Atom target = xevent->xselection.target;
            if (target == data->xdnd_req) {
                /* read data */
                SDL_x11Prop p;
                ReadProperty(&p, display, data->xwindow, XInternAtom(display, "PRIMARY", 0));

                if (p.format == 8) {
                    /* !!! FIXME: don't use strtok here. It's not reentrant and not in SDL_stdinc. */
                    char* name = XGetAtomName(display, target);
                    char *token = strtok((char *) p.data, "\r\n");
                    while (token != NULL) {
                        if (strcmp("text/plain", name)==0) {
                            SDL_SendDropText(data->window, token);
                        } else if (strcmp("text/uri-list", name)==0) {
                            char *fn = URIToLocal(token);
                            if (fn) {
                                SDL_SendDropFile(data->window, fn);
                            }
                        }
                        token = strtok(NULL, "\r\n");
                    }
                    SDL_SendDropComplete(data->window);
                }
                XFree(p.data);

                /* send reply */
                XClientMessageEvent m;
                memset(&m, 0, sizeof(XClientMessageEvent));
                m.type = ClientMessage;
                m.display = display;
                m.window = data->xdnd_source;
                m.message_type = XInternAtom(display, "XdndFinished", 0);
                m.format = 32;
                m.data.l[0] = data->xwindow;
                m.data.l[1] = 1;
                m.data.l[2] = XInternAtom(display, "XdndActionCopy", 0);
                XSendEvent(display, data->xdnd_source, False, NoEventMask, (XEvent*)&m);

                XSync(display, False);

            } else {
                selection_waiting = false;
            }
#else
            selection_waiting = false;
#endif
        }
        break;

    case SelectionClear: {
            Atom XA_CLIPBOARD = XInternAtom(display, "CLIPBOARD", 0);

            if (xevent->xselectionclear.selection == XA_PRIMARY ||
                (XA_CLIPBOARD != None && xevent->xselectionclear.selection == XA_CLIPBOARD)) {
                //SDL_SendClipboardUpdate();
            }
        }
        break;

    default:{
#ifdef DEBUG_XEVENTS
            printf("window %p: Unhandled event %d\n", data, xevent->type);
#endif
        }
        break;
    }
}
