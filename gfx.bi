'' External graphics and IO functions

#include "udts.bi"

' gfx_*

#ifdef EXTERN_GFX
' I would just always declare everything as external, however
' FB angrily complains that we have to also stick EXTERN blocks in each backend .bas as well!
extern "C"
#endif

declare sub gfx_init ()		'initilization
declare sub gfx_close ()		'put it back how we found it
declare sub gfx_showpage (byval raw as ubyte ptr, byval w as integer, byval h as integer) 'the main event
declare sub gfx_setpal (byval pal as RGBcolor ptr) 'set colour palette
declare function gfx_screenshot (fname as zstring ptr) as integer
declare sub gfx_setwindowed (byval iswindow as integer)
declare sub gfx_togglewindowed ()
declare sub gfx_windowtitle (title as zstring ptr)
declare sub gfx_setoption (opt as zstring ptr, byval value as integer = -1)
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

' gfxsubs

declare sub smoothzoomblit_8bit(byval rptr as ubyte ptr, byval dptr as ubyte ptr, byval zoom as integer, byval smooth as integer)
declare sub smoothzoomblit_32bit(byval rptr as ubyte ptr, byval dptr as ubyte ptr, byval zoom as integer, byval smooth as integer, byval pal as integer ptr)
declare sub smoothzoomblit_anybit(byval rptr as ubyte ptr, byval dptr as ubyte ptr, byval zoom as integer, byval smooth as integer, byval bpp as integer, byval pitch as integer)
