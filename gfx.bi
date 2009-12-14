'' External graphics and IO functions

#include "udts.bi"

#ifdef EXTERN_GFX
' I would just always declare everything as external, however
' FB angrily complains that we have to also stick EXTERN blocks in each backend .bas as well!
extern "C"
#endif

type WindowState
	focused as integer
	minimised as integer
end type

'terminate_signal_handler is a pointer to post_terminate_signal, for dynamically linked graphics backends.
'windowicon is platform specific: name of the icon resource on Windows, no meaning yet elsewhere
declare sub gfx_init (byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr)
declare sub gfx_close ()		'put it back how we found it
declare sub gfx_showpage (byval raw as ubyte ptr, byval w as integer, byval h as integer) 'the main event
declare sub gfx_setpal (byval pal as RGBcolor ptr) 'set colour palette. May reuse last raw pointer to showpage, so you may not change it!
declare function gfx_screenshot (byval fname as zstring ptr) as integer
declare sub gfx_setwindowed (byval iswindow as integer)
declare sub gfx_windowtitle (byval title as zstring ptr)
declare function gfx_getwindowstate () as WindowState ptr

'gfx_setoption recieves an option name and the following option which may or may not be a related argument
'returns 0 if unrecognised, 1 if recognised but arg is ignored, 2 if arg is gobbled
declare function gfx_setoption(byval opt as zstring ptr, byval arg as zstring ptr) as integer
declare function gfx_describe_options () as zstring ptr

declare sub io_init ()
declare sub io_pollkeyevents ()

'Primary keyboard state function. Get keypress events (since last call) and keyboard state:
'bit 0: key down, bit 1 and 2: keypress since last call
declare sub io_keybits (keybdarray as integer ptr)

'Get current up/down state of each key. Only used by the polling thread, not needed otherwise
'set bit 3 (8) on each key if current down
declare sub io_updatekeys (byval keybd as integer ptr)

'Primary mouse state function
'mbuttons is currently down keys, mclicks is new clicks since last call. left, right, middle buttons are bits 0, 1, 2
declare sub io_mousebits (byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer, byref mclicks as integer)

declare sub io_setmousevisibility (byval visible as integer)
declare sub io_getmouse (byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer)
declare sub io_setmouse (byval x as integer, byval y as integer)
'call io_mouserect(-1, -1, -1, -1) to disable clipping
declare sub io_mouserect (byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
declare function io_readjoysane (byval as integer, byref as integer, byref as integer, byref as integer) as integer

#ifdef EXTERN_GFX
end extern
#endif

extern "C"

' functions in allmodex.bas

'Call on window or application close request event. Maybe ought to guarantee backend won't be reentered
'Allowed to be called from another thread.
declare sub post_terminate_signal ()

' functions in blit.c

declare sub blitohr(byval spr as Frame ptr, byval destspr as Frame ptr, byval pal as Palette16 ptr, byval startoffset as integer, byval startx as integer, byval starty as integer, byval endx as integer, byval endy as integer, byval trans as integer)
declare sub blitohrscaled(byval spr as Frame ptr, byval destspr as Frame ptr, byval pal as Palette16 ptr, byval x as integer, byval y as integer, byval startx as integer, byval starty as integer, byval endx as integer, byval endy as integer, byval trans as integer, byval scale as integer)

declare sub smoothzoomblit_8_to_8bit(byval rptr as ubyte ptr, byval dptr as ubyte ptr, byval w as integer, byval h as integer, byval pitch as integer, byval zoom as integer, byval smooth as integer)
declare sub smoothzoomblit_8_to_32bit(byval rptr as ubyte ptr, byval dptr as ubyte ptr, byval w as integer, byval h as integer, byval pitch as integer, byval zoom as integer, byval smooth as integer, byval pal as integer ptr)

end extern
