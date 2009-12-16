'OHRRPGCE COMMON - Runtime backend loading routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#include "compat.bi"
#include "common.bi"
#include "gfx.bi"

extern "C"

dim gfx_close as sub ()
dim gfx_getversion as function () as integer
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

dim as string gfxbackend, musicbackend
dim as string gfxbackendinfo, musicbackendinfo

declare function gfx_alleg_init(byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr, byval info_buffer as zstring ptr, byval info_buffer_size as integer) as integer
declare function gfx_fb_init(byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr, byval info_buffer as zstring ptr, byval info_buffer_size as integer) as integer
declare function gfx_sdl_init(byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr, byval info_buffer as zstring ptr, byval info_buffer_size as integer) as integer

declare function gfx_alleg_setprocptrs() as integer
declare function gfx_directx_setprocptrs() as integer
declare sub gfx_directx_unload()
declare function gfx_fb_setprocptrs() as integer
declare function gfx_sdl_setprocptrs() as integer
declare function gfx_load(onlyfirst as integer = NO) as integer

type GfxBackendStuff
	'FB doesn't allow initialising UDTs containing var-length strings
	name as string * 7
	load as function () as integer
	unload as sub ()
	init as function (byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr, byval info_buffer as zstring ptr, byval info_buffer_size as integer) as integer
end type

#ifdef GFX_ALLEG_BACKEND
dim shared as GfxBackendStuff alleg_stuff = ("alleg", @gfx_alleg_setprocptrs, NULL, @gfx_alleg_init)
#endif
#ifdef GFX_DIRECTX_BACKEND
dim shared as GfxBackendStuff directx_stuff = ("directx", @gfx_directx_setprocptrs, @gfx_directx_unload, NULL)
#endif
#ifdef GFX_FB_BACKEND
dim shared as GfxBackendStuff fb_stuff = ("fb", @gfx_fb_setprocptrs, NULL, @gfx_fb_init)
#endif
#ifdef GFX_SDL_BACKEND
dim shared as GfxBackendStuff sdl_stuff = ("sdl", @gfx_sdl_setprocptrs, NULL, @gfx_sdl_init)
#endif


'you can't initialise arrays with addresses because FB considers them nonconstant!!
'plus the extern block nonsense, oh what a mess
end extern
dim shared gfx_choices() as GfxBackendStuff ptr
'sets up pointers to *_stuff variables, in some build-dependent order
GFX_CHOICES_INIT
extern "C"

dim shared gfx_directx as any ptr
dim shared gfx_loaded as integer = 0
dim shared queue_error as string  'queue up errors until it's possible to actually display them (TODO: not implemented)

#ifdef GFX_DIRECTX_BACKEND

function gfx_directx_setprocptrs() as integer
	if gfx_directx <> 0 then return 1

	gfx_directx = dylibload("gfx_directx.dll")
	if gfx_directx = 0 then return 0

	gfx_getversion = dylibsymbol(gfx_directx, "gfx_getversion")
	dim as integer apiver = -1
	if gfx_getversion <> NULL then apiver = gfx_getversion()
	if apiver <> 1 then
		queue_error = "Bad gfx_version API version: expected 1, got " & apiver
		debug queue_error
		dylibfree(gfx_directx)
		gfx_directx = NULL
		return 0
	end if

	directx_stuff.init = dylibsymbol(gfx_directx, "gfx_init")
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
	io_mouserect = dylibsymbol(gfx_directx, "io_mouserect")
	io_readjoysane = dylibsymbol(gfx_directx, "io_readjoysane")
	
	return 1
end function

sub gfx_directx_unload()
	if gfx_directx then
		dylibfree(gfx_directx)
		gfx_directx = NULL
	end if
end sub

#endif

sub prefer_backend(b as GfxBackendStuff ptr)
	for i as integer = ubound(gfx_choices) - 1 to 0 step -1
		if gfx_choices(i + 1) = b then swap gfx_choices(i), gfx_choices(i + 1)
	next
end sub

function backends_setoption(opt as string, arg as string) as integer
	'general backend options
	'gfx should be the first option
	if opt = "gfx" then 
		dim unsupported as integer = NO
		dim failed as integer = NO
		if arg = "alleg" or arg = "allegro" then
			#ifdef GFX_ALLEG_BACKEND
				prefer_backend(@alleg_stuff)
				if gfx_load(YES) then return 2
				failed = YES
			#endif
			unsupported = YES
		elseif arg = "directx" then
			#ifdef GFX_DIRECTX_BACKEND
				prefer_backend(@directx_stuff)
				if gfx_load(YES) then return 2
				failed = YES
			#endif
			unsupported = YES
		elseif arg = "fb" then
			#ifdef GFX_FB_BACKEND
				prefer_backend(@fb_stuff)
				if gfx_load(YES) then return 2
				failed = YES
			#endif
			unsupported = YES
		elseif arg = "sdl" then
			#ifdef GFX_SDL_BACKEND
				prefer_backend(@sdl_stuff)
				if gfx_load(YES) then return 2
				failed = YES
			#endif
			unsupported = YES
		else
			display_help_string """" + arg + """ is not a valid graphics backend"
		end if
		if failed then
			display_help_string "gfx_" + arg + " could not be loaded!"
		elseif unsupported then
			display_help_string "gfx_" + arg + " support is not enabled in this build"
		end if
		return 2
	else
		'after any -gfx is processed, should load the backend to send it the remain options
		gfx_load
		if opt = "w" or opt = "windowed" then
			gfx_setwindowed(1)
			return 1
		elseif opt = "f" or opt = "fullscreen" then
			gfx_setwindowed(0)
			return 1
		end if
	end if
	return 0
end function

'onlyfirst: only try the most prefered. Returns 1 on success
function gfx_load(onlyfirst as integer) as integer
	if gfx_loaded then return 1 'hmm
	for i as integer = 0 to ubound(gfx_choices)
		if gfx_choices(i)->load() then
			gfx_loaded = YES
			gfxbackendinfo = "gfx_" + gfx_choices(i)->name
			gfxbackend = gfx_choices(i)->name
			return 1
		end if
		if onlyfirst then return 0
	next
	display_help_string "could not load any graphic backend!"
	return 0
end function

sub gfx_init(byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr)
	for i as integer = 0 to ubound(gfx_choices)
		with *gfx_choices(i)
			if .load() then
				dim info_buffer as zstring * 256
				debuginfo "Initialising gfx_" + .name + "..."
				gfxbackendinfo = "gfx_" + .name
				gfxbackend = .name
				if .init(terminate_signal_handler, windowicon, @info_buffer, 256) = 0 then 
					if .unload then .unload()
					queue_error = info_buffer
					debug queue_error
				else
					gfxbackendinfo += info_buffer
					debuginfo gfxbackendinfo
					exit sub
				end if
			end if
		end with
	next

	display_help_string "No working graphic backend!"
	system
end sub

end extern

'initialise the music backend name because it's static, yet music_init
'might not be called until Import Music menu
musicbackend = MUSIC_BACKEND
musicbackendinfo = "music_" + MUSIC_BACKEND
