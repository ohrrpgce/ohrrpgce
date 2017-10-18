#pragma once

extern "C"

#define _SDL_cocoaclipboard_h
declare function Cocoa_SetClipboardText(byval text as const zstring ptr) as long
declare function Cocoa_GetClipboardText() as zstring ptr
declare function Cocoa_HasClipboardText() as SDL_bool

end extern
