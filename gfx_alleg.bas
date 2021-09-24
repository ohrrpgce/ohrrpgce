'OHRRPGCE - Allegro graphics backend
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

'' NOTE: This backend is unmaintained. Currently at least the following don't work:
'' -resolutions other than 320x200
'' -changing zoom (no cmdline switch, so can't work at runtime either)
'' -hiding or showing the mouse cursor


#include "config.bi"
#include "backends.bi"
#include "gfx.bi"
#include "common.bi"
#undef Font
#undef readkey
#undef bitmap
#undef ellipse
#undef num_joysticks
#include "allegro.bi"
#include "scancodes.bi"

dim shared init_gfx as bool = NO
dim shared screenbuf as BITMAP ptr = null

dim shared mouse_hidden as bool = NO
dim shared baroffset as integer = 0
dim shared windowed as bool = YES
dim shared bpp as integer = 8
dim shared alpal(255) as RGB

'Translate an allegro scancode into a normal one
dim shared scantrans(0 to 127) as integer => { _
	0, scA, scB, scC, scD, scE, scF, scG, _
	scH, scI, scJ, scK, scL, scM, scN, scO, _
	scP, scQ, scR, scS, scT, scU, scV, scW, _
	scX, scY, scZ, sc0, sc1, sc2, sc3, sc4, _
	sc5, sc6, sc7, sc8, sc9, scNumpad0, scNumpad1, scNumpad2, _
	scNumpad3, scNumpad4, scNumpad5, scNumpad6, scNumpad7, scNumpad8, scNumpad9, scF1, _
	scF2, scF3, scF4, scF5, scF6, scF7, scF8, scF9, _
	scF10, scF11, scF12, scEsc, scTilde, scMinus, scEquals, scBackspace, _
	scTab, scLeftBrace, scRightBrace, scEnter, scColon, scQuote, scBackslash, 0, _
	scComma, scPeriod, scSlash, scSpace, scInsert, scDelete, scHome, scEnd, _
	scPageup, scPagedown, scLeft, scRight, scUp, scDown, scNumpadSlash, scNumpadAsterisk, _
	scNumpadMinus, scNumpadPlus, scNumpadPeriod, scNumpadEnter, scPrintScreen, scPause, 0, 0, _
	0, 0, 0, scAtSign, scCircumflex, 0, 0, scLeftShift, _
	scRightShift, scLeftCtrl, scRightCtrl, scLeftAlt, scRightAlt, scLeftWinLogo, scRightWinLogo, scContext, _
	scScrollLock, scNumlock, scCapslock, 0, 0, 0, 0, 0, _
	0, 0, 0, 0, 0, 0, 0, 0 _
}

extern "C"

declare sub gfx_alleg_set_screen_mode()


function gfx_alleg_init(byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr, byval info_buffer as zstring ptr, byval info_buffer_size as integer) as integer
	if init_gfx = NO then
		#ifdef USE_X11
			'Xlib will kill the program if most errors occur, such as if OpenGL on the machine is broken
			'so the window can't be created. We need to install an error handler to prevent that
			set_X11_error_handlers
		#endif

		if allegro_initialised = NO then
			allegro_init()
			allegro_initialised = YES
		end if
		snprintf(info_buffer, info_buffer_size, "%s", allegro_id)

		gfx_alleg_set_screen_mode

		install_keyboard
		install_mouse
		if windowed then
			unscare_mouse
		else
			scare_mouse
		end if
		set_window_close_hook(@post_terminate_signal)

		init_gfx = YES
	end if
	return 1
end function

local sub gfx_alleg_set_screen_mode()
	set_color_depth(bpp)
	if windowed then
		set_gfx_mode(GFX_AUTODETECT_WINDOWED, 640, 400, 0, 0)
		baroffset = 0
	else
		set_gfx_mode(GFX_AUTODETECT_FULLSCREEN, 640, 480, 0, 0)
		baroffset = 40
	end if
	clear_bitmap(screen)

	if screenbuf <> null then
		destroy_bitmap(screenbuf)
	end if
	'only create this once, to save a bit of time
	screenbuf = create_bitmap(320, 200)
end sub

sub gfx_alleg_close
	if screenbuf <> null then
		destroy_bitmap(screenbuf)
		screenbuf = null
	end if
end sub

function gfx_alleg_getversion() as integer
	return 1
end function

function gfx_alleg_showpage(byval raw as ubyte ptr, byval w as integer, byval h as integer) as integer
'takes a pointer to raw 8-bit data at 320x200 (w, h ignored)
	dim as integer x, y

	if w <> 320 or h <> 200 then
		debug "gfx_alleg_showpage: only 320x200 implemented"
		return 1
	end if

	for y = 0 to 199
		for x = 0 to 319
			if bpp = 8 then
				putpixel(screenbuf, x, y, *raw)
				raw += 1
			else
				putpixel(screenbuf, x, y, *cast(int32 ptr, raw))
				raw += 4
			end if
		next
	next

	stretch_blit(screenbuf, screen, 0, 0, 320, 200, 0, baroffset, 640, 400)
	return 0
end function

sub gfx_alleg_setpal(byval pal as RGBcolor ptr)
	'In 8 bit colour depth, allegro uses 6 bit colour components in the palette
	for i as integer = 0 to 255
		alpal(i).r = pal[i].r \ 4
		alpal(i).g = pal[i].g \ 4
		alpal(i).b = pal[i].b \ 4
	next

	set_palette(@alpal(0))
end sub

function gfx_alleg_present(byval surfaceIn as Surface ptr, byval pal as RGBPalette ptr) as integer
	dim newbpp as integer = iif(surfaceIn->format = SF_8bit, 8, 24)
	if bpp <> newbpp then
		bpp = newbpp
		gfx_alleg_set_screen_mode
	end if

	if pal then
		gfx_alleg_setpal @pal->col(0)
	end if
	return gfx_alleg_showpage(surfaceIn->pPaletteData, surfaceIn->width, surfaceIn->height)
end function

function gfx_alleg_screenshot(byval fname as zstring ptr) as integer
	gfx_alleg_screenshot = 0
end function

sub gfx_alleg_setwindowed(byval iswindow as bool)
	if iswindow then iswindow = YES  'Only 1 "true" value
	if iswindow = windowed then exit sub

	windowed = iswindow

	if init_gfx then
		gfx_alleg_set_screen_mode
		set_palette(@alpal(0))
	end if
end sub

sub gfx_alleg_windowtitle(byval title as zstring ptr)
	if init_gfx then
 		set_window_title(title)
 	end if
end sub

function gfx_alleg_getwindowstate() as WindowState ptr
	static state as WindowState
	state.structsize = WINDOWSTATE_SZ
	'Maybe allegro 4 does support some of the following; I didn't even check
	state.focused = YES  'Don't know
	state.minimised = NO  'Don't know
	state.fullscreen = (windowed = NO)
	state.mouse_over = YES  'Don't know
	state.zoom = 2
	state.windowsize = XY(640, 400)
	return @state
end function

function gfx_alleg_setoption(byval opt as zstring ptr, byval arg as zstring ptr) as integer
'handle command-line options in a generic way, so that they
'can be ignored or supported as the library permits.
	'unrecognised
	return 0
end function

function gfx_alleg_describe_options() as zstring ptr
'No options are supported by this backend
 return @""
end function

'------------- IO Functions --------------
sub io_alleg_init
end sub

sub io_alleg_updatekeys(byval keybd as KeyBits ptr)
	dim a as integer
	for a as KBScancode = 0 to &h7f
		if key(a) then
			keybd[scantrans(a)] = keybd[scantrans(a)] or 8
		end if
	next

	keybd[scShift] or= (keybd[scLeftShift] or keybd[scRightShift]) and 8
	keybd[scUnfilteredAlt] or= (keybd[scLeftAlt] or keybd[scRightAlt]) and 8
	keybd[scCtrl] or= (keybd[scLeftCtrl] or keybd[scRightCtrl]) and 8

	'Note: Pause reports NumLock for me, just like fbgfx

	'FIXME: This crashes inside X11, because io_updatekeys is called from
	'the polling thread rather than the main thread.
	if key(KEY_ENTER) andalso (key_shifts and KB_ALT_FLAG) then
		if windowed = NO then
			gfx_alleg_setwindowed(1)
		else
			gfx_alleg_setwindowed(0)
		end if
		post_event(eventFullscreened, windowed = NO)
	end if
end sub

SUB io_alleg_show_virtual_keyboard()
	'Does nothing on platforms that have real keyboards
END SUB

SUB io_alleg_hide_virtual_keyboard()
	'Does nothing on platforms that have real keyboards
END SUB

sub io_alleg_setmousevisibility(byval visibility as CursorVisibility)
	dim vis as bool
	if visibility = cursorDefault then
		if windowed = YES then vis = NO else vis = YES
	elseif visibility = cursorVisible then
		vis = YES
	else
		vis = NO
	end if

	'who know why this check is here
	if vis = YES and mouse_hidden = YES then
 		unscare_mouse()
		mouse_hidden = NO
 	end if
	if vis = YES and mouse_hidden = NO then
 		scare_mouse()
		mouse_hidden = YES
 	end if
end sub

sub io_alleg_getmouse(byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer)
	mx = mouse_x \ 2		'allegro screen is double res
	my = (mouse_y \ 2) - baroffset	'and centred
	mwheel = 120 * mouse_z          'I have no idea what scale allegro uses for the mouse wheel
	mbuttons = mouse_b
end sub

sub io_alleg_setmouse(byval x as integer, byval y as integer)
	position_mouse(x * 2, y * 2 + baroffset)
end sub

sub io_alleg_mouserect(byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
'doesn't seem to work fullscreen, no idea why not. Height of mouse cursor?
'FIXME: doesn't seem to restrict to the window at all.
 	set_mouse_range(xmin * 2, ymin * 2 + baroffset, xmax * 2 + 1, ymax * 2 + 1 + baroffset)
end sub

function gfx_alleg_setprocptrs() as integer
	gfx_init = @gfx_alleg_init
	gfx_close = @gfx_alleg_close
	gfx_getversion = @gfx_alleg_getversion
	gfx_present = @gfx_alleg_present
	gfx_setpal = @gfx_alleg_setpal
	gfx_screenshot = @gfx_alleg_screenshot
	gfx_setwindowed = @gfx_alleg_setwindowed
	gfx_windowtitle = @gfx_alleg_windowtitle
	gfx_getwindowstate = @gfx_alleg_getwindowstate
	gfx_setoption = @gfx_alleg_setoption
	gfx_describe_options = @gfx_alleg_describe_options
	io_init = @io_alleg_init
	io_keybits = @io_amx_keybits
	io_updatekeys = @io_alleg_updatekeys
	io_show_virtual_keyboard = @io_alleg_show_virtual_keyboard
	io_hide_virtual_keyboard = @io_alleg_hide_virtual_keyboard
	io_mousebits = @io_amx_mousebits
	io_setmousevisibility = @io_alleg_setmousevisibility
	io_getmouse = @io_alleg_getmouse
	io_setmouse = @io_alleg_setmouse
	io_mouserect = @io_alleg_mouserect

	return 1
end function

end extern
