/*
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

static void
X11_DispatchEvent(_THIS)
{
    SDL_VideoData *videodata = (SDL_VideoData *) _this->driverdata;
    Display *display;
    SDL_WindowData *data;
    XEvent xevent;
    int orig_event_type;
    KeyCode orig_keycode;
    XClientMessageEvent m;
    int i;

    if (!videodata) {
        return;
    }
    display = videodata->display;

    SDL_zero(xevent);           /* valgrind fix. --ryan. */
    X11_XNextEvent(display, &xevent);

#if 0
    printf("type = %d display = %d window = %d\n",
           xevent.type, xevent.xany.display, xevent.xany.window);
#endif

    data = NULL;
    if (videodata && videodata->windowlist) {
        for (i = 0; i < videodata->numwindows; ++i) {
            if ((videodata->windowlist[i] != NULL) &&
                (videodata->windowlist[i]->xwindow == xevent.xany.window)) {
                data = videodata->windowlist[i];
                break;
            }
        }
    }

    switch (xevent.type) {

    /* Copy the selection from our own CUTBUFFER to the requested property */
    case SelectionRequest: {
            XSelectionRequestEvent *req;
            XEvent sevent;
            int seln_format;
            unsigned long nbytes;
            unsigned long overflow;
            unsigned char *seln_data;

            req = &xevent.xselectionrequest;
#ifdef DEBUG_XEVENTS
            printf("window %p: SelectionRequest (requestor = %ld, target = %ld)\n", data,
                req->requestor, req->target);
#endif

            SDL_zero(sevent);
            sevent.xany.type = SelectionNotify;
            sevent.xselection.selection = req->selection;
            sevent.xselection.target = None;
            sevent.xselection.property = None;
            sevent.xselection.requestor = req->requestor;
            sevent.xselection.time = req->time;

            if (X11_XGetWindowProperty(display, DefaultRootWindow(display),
                    X11_GetSDLCutBufferClipboardType(display), 0, INT_MAX/4, False, req->target,
                    &sevent.xselection.target, &seln_format, &nbytes,
                    &overflow, &seln_data) == Success) {
                Atom XA_TARGETS = X11_XInternAtom(display, "TARGETS", 0);
                if (sevent.xselection.target == req->target) {
                    X11_XChangeProperty(display, req->requestor, req->property,
                        sevent.xselection.target, seln_format, PropModeReplace,
                        seln_data, nbytes);
                    sevent.xselection.property = req->property;
                } else if (XA_TARGETS == req->target) {
                    Atom SupportedFormats[] = { XA_TARGETS, sevent.xselection.target };
                    X11_XChangeProperty(display, req->requestor, req->property,
                        XA_ATOM, 32, PropModeReplace,
                        (unsigned char*)SupportedFormats,
                        SDL_arraysize(SupportedFormats));
                    sevent.xselection.property = req->property;
                    sevent.xselection.target = XA_TARGETS;
                }
                X11_XFree(seln_data);
            }
            X11_XSendEvent(display, req->requestor, False, 0, &sevent);
            X11_XSync(display, False);
        }
        break;

    case SelectionNotify: {
            Atom target = xevent.xselection.target;
#ifdef DEBUG_XEVENTS
            printf("window %p: SelectionNotify (requestor = %ld, target = %ld)\n", data,
                xevent.xselection.requestor, xevent.xselection.target);
#endif
            if (target == data->xdnd_req) {
                /* read data */
                SDL_x11Prop p;
                X11_ReadProperty(&p, display, data->xwindow, videodata->PRIMARY);

                if (p.format == 8) {
                    /* !!! FIXME: don't use strtok here. It's not reentrant and not in SDL_stdinc. */
                    char* name = X11_XGetAtomName(display, target);
                    char *token = strtok((char *) p.data, "\r\n");
                    while (token != NULL) {
                        if (SDL_strcmp("text/plain", name)==0) {
                            SDL_SendDropText(data->window, token);
                        } else if (SDL_strcmp("text/uri-list", name)==0) {
                            char *fn = X11_URIToLocal(token);
                            if (fn) {
                                SDL_SendDropFile(data->window, fn);
                            }
                        }
                        token = strtok(NULL, "\r\n");
                    }
                    SDL_SendDropComplete(data->window);
                }
                X11_XFree(p.data);

                /* send reply */
                SDL_memset(&m, 0, sizeof(XClientMessageEvent));
                m.type = ClientMessage;
                m.display = display;
                m.window = data->xdnd_source;
                m.message_type = videodata->XdndFinished;
                m.format = 32;
                m.data.l[0] = data->xwindow;
                m.data.l[1] = 1;
                m.data.l[2] = videodata->XdndActionCopy;
                X11_XSendEvent(display, data->xdnd_source, False, NoEventMask, (XEvent*)&m);

                X11_XSync(display, False);

            } else {
                videodata->selection_waiting = SDL_FALSE;
            }
        }
        break;

    case SelectionClear: {
            Atom XA_CLIPBOARD = X11_XInternAtom(display, "CLIPBOARD", 0);

            if (xevent.xselectionclear.selection == XA_PRIMARY ||
                (XA_CLIPBOARD != None && xevent.xselectionclear.selection == XA_CLIPBOARD)) {
                SDL_SendClipboardUpdate();
            }
        }
        break;

    default:{
#ifdef DEBUG_XEVENTS
            printf("window %p: Unhandled event %d\n", data, xevent.type);
#endif
        }
        break;
    }
}
