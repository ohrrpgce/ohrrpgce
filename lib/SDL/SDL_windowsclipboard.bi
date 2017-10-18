#pragma once

extern "C"

#include "config.bi"
include_windows_bi()

declare function WIN_SetClipboardText(byval hWindow as HWND, byval text as const zstring ptr) as long
declare function WIN_GetClipboardText(byval hWindow as HWND) as zstring ptr
declare function WIN_HasClipboardText(byval hWindow as HWND) as bool

end extern
