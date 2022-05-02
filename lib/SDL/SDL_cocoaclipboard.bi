#pragma once

extern "C"

declare function Cocoa_SetClipboardText(byval text as const zstring ptr) as long
declare function Cocoa_GetClipboardText() as zstring ptr
declare function Cocoa_HasClipboardText() as boolean

end extern
