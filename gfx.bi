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

declare sub gfx_init ()		'initilization
declare sub gfx_close ()		'put it back how we found it
declare sub gfx_showpage (byval raw as ubyte ptr, byval w as integer, byval h as integer) 'the main event
declare sub gfx_setpal (byval pal as RGBcolor ptr) 'set colour palette. May reuse last raw pointer to showpage, so you may not change it!
declare function gfx_screenshot (byval fname as zstring ptr) as integer
declare sub gfx_setwindowed (byval iswindow as integer)
declare sub gfx_togglewindowed ()  'to be removed
declare sub gfx_windowtitle (byval title as zstring ptr)
declare function gfx_getwindowstate () as WindowState ptr

'gfx_setoption recieves an option name and the following option which may or may not be a related argument
'returns 0 if unrecognised, 1 if recognised but arg is ignored, 2 if arg is gobbled
declare function gfx_setoption(byval opt as zstring ptr, byval arg as zstring ptr) as integer
declare function gfx_describe_options () as zstring ptr

declare sub io_init ()
declare sub io_pollkeyevents ()
declare sub io_updatekeys (byval keybd as integer ptr)
declare sub io_setmousevisibility (byval visible as integer)
declare sub io_getmouse (byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer)
declare sub io_setmouse (byval x as integer, byval y as integer)
'declare sub io_mouserect (byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
declare function io_readjoysane (byval as integer, byref as integer, byref as integer, byref as integer) as integer

#ifdef EXTERN_GFX
end extern
#endif

' functions in allmodex.bas

'Call on window or application close request event. Maybe ought to guarantee backend won't be reentered
'Allowed to be called from another thread.
declare sub post_terminate_signal cdecl ()

' functions in gfxsubs.bas

declare sub smoothzoomblit_8_to_8bit(byval rptr as ubyte ptr, byval dptr as ubyte ptr, byval w as integer, byval h as integer, byval pitch as integer, byval zoom as integer, byval smooth as integer)
declare sub smoothzoomblit_8_to_32bit(byval rptr as ubyte ptr, byval dptr as ubyte ptr, byval w as integer, byval h as integer, byval pitch as integer, byval zoom as integer, byval smooth as integer, byval pal as integer ptr)
