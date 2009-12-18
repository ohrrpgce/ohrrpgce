'' External graphics and IO functions

#include "udts.bi"

extern "C"

type WindowState
	focused as integer
	minimised as integer
end type

'terminate_signal_handler is a pointer to post_terminate_signal, for dynamically linked graphics backends.
'windowicon is platform specific: name of the icon resource on Windows, no meaning yet elsewhere
'(unrelated to gfx_init)
declare sub gfx_backend_init (byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr)

extern gfx_init as function (byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr, byval info_buffer as zstring ptr, byval info_buffer_size as integer) as integer
extern gfx_close as sub ()

'API version. Must return 1
extern gfx_getversion as function () as integer

extern gfx_showpage as sub (byval raw as ubyte ptr, byval w as integer, byval h as integer)

'set colour palette. May reuse last raw pointer to showpage, so you may not change it!
extern gfx_setpal as sub (byval pal as RGBcolor ptr)
extern gfx_screenshot as function (byval fname as zstring ptr) as integer
extern gfx_setwindowed as sub (byval iswindow as integer)
extern gfx_windowtitle as sub (byval title as zstring ptr)
extern gfx_getwindowstate as function () as WindowState ptr

'gfx_setoption recieves an option name and the following option which may or may not be a related argument
'returns 0 if unrecognised, 1 if recognised but arg is ignored, 2 if arg is gobbled
extern gfx_setoption as function (byval opt as zstring ptr, byval arg as zstring ptr) as integer
extern gfx_describe_options as function () as zstring ptr

extern io_init as sub ()

'(optional) called in loops where gfx_showpage is not.
extern io_pollkeyevents as sub ()

'(optional) called every 10ms during waits
extern io_waitprocessing as sub ()

'one of io_keybits or io_updatekeys, and one of io_mousebits or io_getmouse is required.

'(optional) Primary keyboard state function. Get keypress events (since last call) and keyboard state:
'bit 0: key down, bit 1 and 2: keypress since last call
extern io_keybits as sub (keybdarray as integer ptr)

'(optional, thread safe) Get current up/down state of each key. Only used by the polling thread, not needed otherwise
'set bit 3 (8) on each key if current down
extern io_updatekeys as sub (byval keybd as integer ptr)

'(optional) Primary mouse state function
'mbuttons is bitmask of currently down keys OR new clicks, mclicks is bitmask ofnew clicks since last call.
'left, right, middle buttons are bits 0, 1, 2
'Mouse wheel not commonly implemented yet.
extern io_mousebits as sub (byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer, byref mclicks as integer)

'(optional, thread safe) same as io_mouse bits.
extern io_getmouse as sub (byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer)
extern io_setmouse as sub (byval x as integer, byval y as integer)

extern io_setmousevisibility as sub (byval visible as integer)

'call io_mouserect(-1, -1, -1, -1) to disable clipping
extern io_mouserect as sub (byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
extern io_readjoysane as function (byval as integer, byref as integer, byref as integer, byref as integer) as integer


' functions in allmodex.bas

'Call on window or application close request event. Maybe ought to guarantee backend won't be reentered
'Allowed to be called from another thread.
declare sub post_terminate_signal ()

declare sub io_amx_keybits (keybdarray as integer ptr)
declare sub io_amx_mousebits (byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer, byref mclicks as integer)

' functions in blit.c

declare sub blitohr(byval spr as Frame ptr, byval destspr as Frame ptr, byval pal as Palette16 ptr, byval startoffset as integer, byval startx as integer, byval starty as integer, byval endx as integer, byval endy as integer, byval trans as integer)
declare sub blitohrscaled(byval spr as Frame ptr, byval destspr as Frame ptr, byval pal as Palette16 ptr, byval x as integer, byval y as integer, byval startx as integer, byval starty as integer, byval endx as integer, byval endy as integer, byval trans as integer, byval scale as integer)

declare sub smoothzoomblit_8_to_8bit(byval rptr as ubyte ptr, byval dptr as ubyte ptr, byval w as integer, byval h as integer, byval pitch as integer, byval zoom as integer, byval smooth as integer)
declare sub smoothzoomblit_8_to_32bit(byval rptr as ubyte ptr, byval dptr as ubyte ptr, byval w as integer, byval h as integer, byval pitch as integer, byval zoom as integer, byval smooth as integer, byval pal as integer ptr)

end extern
