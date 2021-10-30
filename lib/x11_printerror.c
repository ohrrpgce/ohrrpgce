/* This code is adapted from part of:
 *	XlibInt.c - Internal support routines for the C subroutine
 *	interface library (Xlib) to the X Window System Protocol V11.0.
 * xorg-libX11 git commit 2911c39cecd63
 */

/*
Copyright 2018  Ralph Versteegen
Copyright 1985, 1986, 1987, 1998  The Open Group
Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation.
The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.
Except as contained in this notice, the name of The Open Group shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from The Open Group.
*/

#include <X11/Xlib.h>
#include <stdio.h>
#include <string.h>


/* Map a major op >= 128 to the name of its extension. See also:
   https://www.x.org/wiki/Development/Documentation/Protocol/OpCodes/
*/
char *opcode_to_extension(int opcode, FILE *fp) {
    static char buffer[64];

    Display *display;
    display = XOpenDisplay(NULL);  // uses display indicated by $DISPLAY
    if (!display) {
        fprintf(fp, "XOpenDisplay failed\n");
        return NULL;
    }

    int listn = 0;
    char **list = XListExtensions(display, &listn);
    if (!list || !listn) {
        fprintf(fp, "XListExtensions failed\n");
        XCloseDisplay(display);
        return NULL;
    }

    char *ret = NULL;

    for (int i = 0; i < listn; i++) {
        XExtCodes codes;
	if (!XQueryExtension(display, list[i],
		&codes.major_opcode, &codes.first_event,
                             &codes.first_error)) {
            fprintf(stderr, "XQueryExtension failed\n");
        }
        if (codes.major_opcode == opcode) {
            ret = buffer;
            snprintf(buffer, 64, "%s", list[i]);
            break;
        }
    }
    XCloseDisplay(display);

    return ret;
}

/* Xlib's default error handler calls internal function _XPrintDefaultError
   and then calls exit(). This function is a nearly functionally identical
   adapted version of _XPrintDefaultError that doesn't depend on Xlib internals.
*/
void PrintXError(
    Display *dpy,
    XErrorEvent *event,
    FILE *fp)
{
    char buffer[BUFSIZ];
    char mesg[BUFSIZ];
    char number[32];
    const char *mtype = "XlibMessage";
    const char *extname = NULL;
    XGetErrorText(dpy, event->error_code, buffer, BUFSIZ);
    XGetErrorDatabaseText(dpy, mtype, "XError", "X Error", mesg, BUFSIZ);
    (void) fprintf(fp, "%s:  %s\n  ", mesg, buffer);
    XGetErrorDatabaseText(dpy, mtype, "MajorCode", "Request Major code %d",
	mesg, BUFSIZ);
    (void) fprintf(fp, mesg, event->request_code);
    if (event->request_code < 128) {
	snprintf(number, sizeof(number), "%d", event->request_code);
	XGetErrorDatabaseText(dpy, "XRequest", number, "", buffer, BUFSIZ);
    } else {
        extname = opcode_to_extension(event->request_code, fp);
	if (extname) {
	    strncpy(buffer, extname, BUFSIZ - 1);
	    buffer[BUFSIZ - 1] = '\0';
        } else
	    buffer[0] = '\0';
    }
    (void) fprintf(fp, " (%s)\n", buffer);
    if (event->request_code >= 128) {
	XGetErrorDatabaseText(dpy, mtype, "MinorCode", "Request Minor code %d",
			      mesg, BUFSIZ);
	fputs("  ", fp);
	(void) fprintf(fp, mesg, event->minor_code);
	if (extname) {
	    snprintf(mesg, sizeof(mesg), "%s.%d", extname, event->minor_code);
	    XGetErrorDatabaseText(dpy, "XRequest", mesg, "", buffer, BUFSIZ);
	    (void) fprintf(fp, " (%s)", buffer);
	}

	fputs("\n", fp);
    }
    if (event->error_code >= 128) {
        /* The following code is almost identical to the implementation of
           XGetErrorText, except that calls XGetErrorDatabaseText for an
           XProtoError instead of XlibMessage string. The XlibMessage strings
           include a printf %lx argument while the XProtoError error string
           don't, and are worded differently.
           See https://cgit.freedesktop.org/xorg/lib/libX11/tree/src/XErrorDB
           And XGetErrorText has already been called above, so no point doing
           more than just printing event->resourceid.
        */
	/* kludge, try to find the extension that caused it */
        /*
	buffer[0] = '\0';
	for (ext = dpy->ext_procs; ext; ext = ext->next) {
	    if (ext->error_string)
		(*ext->error_string)(dpy, event->error_code, &ext->codes,
				     buffer, BUFSIZ);
	    if (buffer[0]) {
		bext = ext;
		break;
	    }
	    if (ext->codes.first_error &&
		ext->codes.first_error < (int)event->error_code &&
		(!bext || ext->codes.first_error > bext->codes.first_error))
		bext = ext;
	}
	if (bext)
	    snprintf(buffer, sizeof(buffer), "%s.%d", bext->name,
                     event->error_code - bext->codes.first_error);
	else
        */
	    strcpy(buffer, "Value");
	XGetErrorDatabaseText(dpy, mtype, buffer, "", mesg, BUFSIZ);
	if (mesg[0]) {
	    fputs("  ", fp);
	    (void) fprintf(fp, mesg, event->resourceid);
	    fputs("\n", fp);
	}
	/* let extensions try to print the values */
	/* We can't do this
	for (ext = dpy->ext_procs; ext; ext = ext->next) {
	    if (ext->error_values)
		(*ext->error_values)(dpy, event, fp);
	}
        */
    } else if ((event->error_code == BadWindow) ||
	       (event->error_code == BadPixmap) ||
	       (event->error_code == BadCursor) ||
	       (event->error_code == BadFont) ||
	       (event->error_code == BadDrawable) ||
	       (event->error_code == BadColor) ||
	       (event->error_code == BadGC) ||
	       (event->error_code == BadIDChoice) ||
	       (event->error_code == BadValue) ||
	       (event->error_code == BadAtom)) {
	if (event->error_code == BadValue)
	    XGetErrorDatabaseText(dpy, mtype, "Value", "Value 0x%x",
				  mesg, BUFSIZ);
	else if (event->error_code == BadAtom)
	    XGetErrorDatabaseText(dpy, mtype, "AtomID", "AtomID 0x%x",
				  mesg, BUFSIZ);
	else
	    XGetErrorDatabaseText(dpy, mtype, "ResourceID", "ResourceID 0x%x",
				  mesg, BUFSIZ);
	fputs("  ", fp);
	(void) fprintf(fp, mesg, event->resourceid);
	fputs("\n", fp);
    }
    XGetErrorDatabaseText(dpy, mtype, "ErrorSerial", "Error Serial #%d",
			  mesg, BUFSIZ);
    fputs("  ", fp);
    (void) fprintf(fp, mesg, event->serial);
    /* We can't do this
    XGetErrorDatabaseText(dpy, mtype, "CurrentSerial", "Current Serial #%lld",
			  mesg, BUFSIZ);
    fputs("\n  ", fp);
    (void) fprintf(fp, mesg, (unsigned long long)(X_DPY_GET_REQUEST(dpy)));
    */
    fputs("\n", fp);
}
