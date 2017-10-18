#pragma once

#include once "X11/Xlib.bi"

extern "C"

declare function X11_SetClipboardText(byval display as Display ptr, byval window as Window, byval text as const zstring ptr) as long
declare function X11_GetClipboardText(byval display as Display ptr, byval window as Window) as zstring ptr
declare function X11_HasClipboardText(byval display as Display ptr, byval window as Window) as bool
declare sub X11_HandleClipboardEvent(byval display as Display ptr, byval xevent as XEvent ptr)

end extern
