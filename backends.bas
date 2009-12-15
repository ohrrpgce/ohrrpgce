'OHRRPGCE COMMON - Runtime backend loading routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#include "compat.bi"
#include "gfx.bi"

extern "C"

dim gfx_close as sub ()
dim gfx_showpage as sub (byval raw as ubyte ptr, byval w as integer, byval h as integer)
dim gfx_setpal as sub (byval pal as RGBcolor ptr)
dim gfx_screenshot as function (byval fname as zstring ptr) as integer
dim gfx_setwindowed as sub (byval iswindow as integer)
dim gfx_windowtitle as sub (byval title as zstring ptr)
dim gfx_getwindowstate as function () as WindowState ptr
dim gfx_setoption as function (byval opt as zstring ptr, byval arg as zstring ptr) as integer
dim gfx_describe_options as function () as zstring ptr
dim io_init as sub ()
dim io_pollkeyevents as sub ()
dim io_keybits as sub (keybdarray as integer ptr)
dim io_updatekeys as sub (byval keybd as integer ptr)
dim io_mousebits as sub (byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer, byref mclicks as integer)
dim io_setmousevisibility as sub (byval visible as integer)
dim io_getmouse as sub (byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer)
dim io_setmouse as sub (byval x as integer, byval y as integer)
dim io_mouserect as sub (byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
dim io_readjoysane as function (byval as integer, byref as integer, byref as integer, byref as integer) as integer

declare sub gfx_fb_init(byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr)
declare sub gfx_sdl_init(byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr)
declare sub gfx_alleg_init(byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr)
dim shared gfx_directx_init as sub (byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr)

dim shared gfx_directx as any ptr


' gfx_directx needs updating
sub io_dummy_mouserect (byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
end sub

#ifdef GFX_DIRECTX_BACKEND

function load_gfx_directx() as integer
	gfx_directx = dylibload("gfx_directx.dll")
	if gfx_directx = 0 then return 0

	'TODO: verify version

	gfx_directx_init = dylibsymbol(gfx_directx, "gfx_init")
	gfx_close = dylibsymbol(gfx_directx, "gfx_close")
	gfx_showpage = dylibsymbol(gfx_directx, "gfx_showpage")
	gfx_setpal = dylibsymbol(gfx_directx, "gfx_setpal")
	gfx_screenshot = dylibsymbol(gfx_directx, "gfx_screenshot")
	gfx_setwindowed = dylibsymbol(gfx_directx, "gfx_setwindowed")
	gfx_windowtitle = dylibsymbol(gfx_directx, "gfx_windowtitle")
	gfx_getwindowstate = dylibsymbol(gfx_directx, "gfx_getwindowstate")
	gfx_setoption = dylibsymbol(gfx_directx, "gfx_setoption")
	gfx_describe_options = dylibsymbol(gfx_directx, "gfx_describe_options")
	io_init = dylibsymbol(gfx_directx, "io_init")
	io_pollkeyevents = dylibsymbol(gfx_directx, "io_pollkeyevents")
	io_keybits = @io_amx_keybits
	io_updatekeys = dylibsymbol(gfx_directx, "io_updatekeys")
	io_mousebits = @io_amx_mousebits
	io_setmousevisibility = dylibsymbol(gfx_directx, "io_setmousevisibility")
	io_getmouse = dylibsymbol(gfx_directx, "io_getmouse")
	io_setmouse = dylibsymbol(gfx_directx, "io_setmouse")
	io_mouserect = @io_dummy_mouserect
	io_readjoysane = dylibsymbol(gfx_directx, "io_readjoysane")
	
	return 1
end function

#endif

sub gfx_init(byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr)
	#ifdef GFX_FB_BACKEND
		gfx_fb_init(terminate_signal_handler, windowicon)
		exit sub
	#endif
	#ifdef GFX_SDL_BACKEND
		gfx_sdl_init(terminate_signal_handler, windowicon)
		exit sub
	#endif
	#ifdef GFX_DIRECTX_BACKEND
		if load_gfx_directx then
			gfx_directx_init(terminate_signal_handler, windowicon)
			exit sub
		end if
	#endif
	#ifdef GFX_ALLEG_BACKEND
		gfx_alleg_init(terminate_signal_handler, windowicon)
		exit sub
	#endif
	print "No graphics backend"
	system
end sub


end extern
