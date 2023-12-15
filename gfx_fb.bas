'OHRRPGCE - FreeBASIC gfxlib-based graphics backend
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#include "config.bi"

#ifdef USE_X11
	#include "lib/SDL/SDL_x11clipboard.bi"
	#undef font

#if __FB_VER_MAJOR__ = 1 and __FB_VER_MINOR__ <= 5
	' HACK: FB 1.05 and earlier don't have a way to get the Display ptr, only the Window
	' But the Window ptr happens to be the first member of fb_x11
	type X11DRIVER
		display as Display ptr
		'rest omitted
	end type
	extern fb_x11 alias "fb_x11" as X11DRIVER
#endif

#elseif defined(__FB_WIN32__)
	include_windows_bi()
	#include "lib/SDL/SDL_windowsclipboard.bi"
#elseif defined(__FB_DARWIN__)
	#include "lib/SDL/SDL_cocoaclipboard.bi"
#endif

#include "util.bi"
#include "fbgfx.bi"
#include "surface.bi"
#include "gfx.bi"
#include "allmodex.bi"
#include "common.bi"

'Use the FB namespace for the types and constants from fbgfx
USING FB

#ifndef GFX_SCREEN_EXIT
 'Added in FB 1.08
 const as integer GFX_SCREEN_EXIT = &h80000000l
#endif


'a public rtlib function that they seem to have forgotten to expose to FB programs?
declare function fb_KeyHit alias "fb_KeyHit" () as long

extern "C"

'subs only used internally
declare sub calculate_and_set_screen_res(fullscreen as bool)
declare sub update_mouse_visibility()

dim shared nogfx as bool = NO   'Run as a console program only
dim shared noinput as bool = NO  'Only when nogfx: don't accept key input from stdin either (which freezes if it isn't a tty)
dim shared screen_buffer_offset as XYPair  'Position of the image on the window/screen, in screen pixels
dim shared window_state as WindowState
dim shared want_toggle_fullscreen as bool  'Alt-Enter pressed, change pending
dim shared init_gfx as bool = NO
'defaults are 2x zoom and 640x400 in 8-bit
dim shared zoom as integer = 2
dim shared remember_windowed_zoom as integer = 2  'What zoom was before entering fullscreen
dim shared framesize as XYPair = (320, 200) 'The resolution supplied by the engine
dim shared screensize as XYPair            'The size of the window/fullscreen resolution
dim shared screenpitch as fb_integer       'Bytes per screen row
dim shared depth as integer = 8            '8 or 32
dim shared smooth as integer = 0  '0 or 1
dim shared mouseclipped as bool = NO
dim shared mouse_visibility as CursorVisibility = cursorDefault
dim shared remember_windowtitle as string
dim shared as integer mxmin = -1, mxmax = -1, mymin = -1, mymax = -1
dim shared inputtext as string
dim shared extrakeys(127) as KeyBits

'Internal palette for 32-bit mode
dim shared truepal(255) as RGBcolor


'Don't bother with gfxmutex in this function, as polling thread hasn't started
function gfx_fb_init(byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr, byval info_buffer as zstring ptr, byval info_buffer_size as integer) as integer
	if init_gfx then return 0

	if nogfx then
		*info_buffer = "nogfx"
	else
		#ifdef USE_X11
			'Xlib will kill the program if most errors occur, such as if OpenGL on the machine is broken
			'so the window can't be created. We need to install an error handler to prevent that
			set_X11_error_handlers
		#endif

		dim bpp as fb_integer 'bits, not bytes. see, bits is b, bytes is B
		dim refreshrate as fb_integer
		dim driver as string
		dim as fb_integer w, h
		'Poll the size of the desktop (entire desktop across multiple displays), etc
		screeninfo w, h, bpp, , , refreshrate, driver
		debuginfo "gfx_fb: desktop size=" & w & "*" & h & " screen bitdepth=" & bpp & " refreshrate=" & refreshrate

		calculate_and_set_screen_res window_state.fullscreen
		screenset 1, 0    'Only want one FB video page
		screeninfo w, h, bpp, , , refreshrate, driver
		*info_buffer = MID(bpp & "bpp, " & refreshrate & "Hz, " & driver & " driver", 1, info_buffer_size)
	end if
	init_gfx = YES
	window_state.structsize = WINDOWSTATE_SZ
	window_state.focused = YES
	window_state.minimised = NO
	window_state.maximised = NO
	'This may be set by gfx_setwindowed before gfx_init
	'window_state.fullscreen = NO
	window_state.mouse_over = NO
	return 1
end function

'calculate_and_set_screen_res should be called instead of this
local sub gfx_fb_screenres()
	if nogfx then exit sub
	debuginfo "screenres " & screensize & " depth=" & depth & " fullscreen=" & window_state.fullscreen
	'GFX_NO_SWITCH: disable alt+enter to change to/from fullscreen, so we can handle it ourselves,
	'changing screensize at the same time. Otherwise, fbgfx often fails to switch to fullscreen,
	'I think it only accepts screenmodes with the same width as passed to screenres?
	if window_state.fullscreen = YES then
		screenres screensize.x, screensize.y, depth, 1, GFX_FULLSCREEN + GFX_NO_SWITCH
	else
		screenres screensize.x, screensize.y, depth, 1, GFX_WINDOWED + GFX_NO_SWITCH
	end if
	'Get new screenpitch
	'The int64 version of screeninfo doesn't have optional w,h args, so pass w,h so fbc
	'can pick whichever overload is suitable
	dim dummy as fb_integer
	screeninfo dummy, dummy, , , screenpitch
	'hook_fb_End is a kludge that works by setting the gfxlib2 exit hook,
	'which screenres sets, so have to reset it afterwards. See miscc.c
	hook_fb_End()
	update_mouse_visibility()
end sub

private sub update_mouse_visibility
	dim vis as integer  '0 or 1
	if mouse_visibility = cursorDefault then
		vis = iif(window_state.fullscreen, 0, 1)
	elseif mouse_visibility = cursorVisible then
		vis = 1
	else
		vis = 0
	end if
	setmouse  , , vis
end sub

'Recreate the window if needed, while possibly switching to/from fullscreen
local sub update_screen_mode_no_lock(to_fullscreen as bool = -2)
	if to_fullscreen = -2 then to_fullscreen = window_state.fullscreen
	calculate_and_set_screen_res to_fullscreen
	windowtitle remember_windowtitle
	'Palette must be re-set
	if depth = 8 then
		for i as integer = 0 to 255
			with truepal(i)
				palette i, .r, .g, .b
			end with
		next
	end if
end sub

sub gfx_fb_update_screen_mode()
	if init_gfx then
		GFX_ENTER
		update_screen_mode_no_lock
		GFX_EXIT
	end if
end sub

sub gfx_fb_close
	init_gfx = NO
	if nogfx then exit sub
	GFX_ENTER
	'GFX_SCREEN_EXIT prevents fbgfx from resetting and clearing the console
	screen 0, , , GFX_SCREEN_EXIT
	GFX_EXIT
end sub

function gfx_fb_getversion() as integer
	return 1
end function

'Called with gfxmutex held
sub gfx_fb_setpal(byval pal as RGBcolor ptr)
	dim as integer i
	if depth = 8 then
		for i = 0 to 255
			palette i, pal[i].r, pal[i].g, pal[i].b
		next
	end if
	'copy the palette, both for 32bit colour mode and when changing
	'res requires resetting the palette
	memcpy @truepal(0), pal, 256 * sizeof(RGBcolor)
	'FIXME: If running in 32 bitdepth, this does not update the page "live", like the 8-bit version
	'so fades won't working, and there's no way to force an
	'update from here at the moment because the screen buffer is not
	'accessible.
end sub

'Called with gfxmutex held
function gfx_fb_present(byval surfaceIn as Surface ptr, byval pal as RGBPalette ptr) as integer
	dim ret as integer = 0
	if nogfx then return 0

	dim newdepth as integer = iif(surfaceIn->format = .SF_32bit, 32, 8)
	if newdepth <> depth orelse surfaceIn->size <> framesize then
		depth = newdepth
		framesize = surfaceIn->size
		update_screen_mode_no_lock
	end if

	screenlock
	dim as integer bytespp = depth \ 8
	dim as ubyte ptr screenpixels = screenptr + screen_buffer_offset.x * bytespp + screen_buffer_offset.y * screenpitch

	with *surfaceIn
		if .format = SF_8bit then
			gfx_fb_setpal(cast(RGBcolor ptr, @pal->col(0)))

			if depth = 8 then
				smoothzoomblit_8_to_8bit(.pPaletteData, screenpixels, .size, screenpitch, zoom, smooth)
			elseif depth = 32 then
				smoothzoomblit_8_to_32bit(.pPaletteData, cast(uint32 ptr, screenpixels), .size, screenpitch \ 4, zoom, smooth, @truepal(0))
			end if
		else  '32 bit
			if depth = 32 then
				smoothzoomblit_32_to_32bit(.pColorData, cast(uint32 ptr, screenpixels), .size, screenpitch \ 4, zoom, smooth)
			else
				ret = 1
			end if
		end if
	end with

	screenunlock
	flip

	return ret
end function

function gfx_fb_screenshot(byval fname as zstring ptr) as integer
	gfx_fb_screenshot = 0
end function

sub gfx_fb_setwindowed(byval iswindow as integer)
	dim wantfullscreen as bool = iif(iswindow, NO, YES) 'only 1 "true" value
	if window_state.fullscreen = wantfullscreen then exit sub

	if debugging_io then debuginfo "setwindowed fullscreen=" & wantfullscreen

	if wantfullscreen = NO then zoom = remember_windowed_zoom
	GFX_ENTER
	update_screen_mode_no_lock wantfullscreen
	GFX_EXIT
end sub

'Called with gfxmutex held
sub gfx_fb_windowtitle(byval title as zstring ptr)
	remember_windowtitle = *title
	windowtitle *title
end sub

function gfx_fb_getwindowstate() as WindowState ptr
	window_state.windowsize = screensize
	window_state.zoom = zoom
	return @window_state
end function

sub gfx_fb_get_screen_size(wide as integer ptr, high as integer ptr)
	dim as ssize_t wide_, high_  'for 64 bit builds
	'This returns the size of the display on which our window is, as desired, not the whole desktop size.
	'However, if we've fullscreened it returns the fullscreen resolution.
	GFX_ENTER
	ScreenControl GET_DESKTOP_SIZE, wide_, high_
	GFX_EXIT
	*wide = wide_
	*high = high_
end sub

function gfx_fb_supports_variable_resolution() as bool
	'Yes, but we don't support setting the window to resizable, and probably never will.
	return YES
end function

sub gfx_fb_get_settings(byref settings as GfxSettings)
	settings.upscaler = smooth   '0/1
end sub

sub gfx_fb_set_settings(settings as GfxSettings)
	smooth = settings.upscaler
end sub

function gfx_fb_setoption(byval opt as zstring ptr, byval arg as zstring ptr) as integer
'handle command-line options in a generic way, so that they
'can be ignored or supported as the library permits.
'This version supports
'	zoom (1, 2*, ..., 16),
'	smooth (0*, 1)
'Changing mode after window already created isn't well tested!
	dim as integer value = str2int(*arg, -1)
	dim as integer ret = 0
	dim as bool screen_mode_changed = NO
	if *opt = "zoom" or *opt = "z" then
		if value >= 1 and value <= 16 then
			if window_state.fullscreen then
				'We are using a computed zoom, ignore the request
				remember_windowed_zoom = value
			else
				zoom = value
				screen_mode_changed = YES
			end if
		end if
		ret = 1
	elseif *opt = "smooth" or *opt = "s" then
		if value = 1 or value = -1 then  'arg optional
			smooth = 1
		else
			smooth = 0
		end if
		ret = 1
	elseif *opt = "nogfx" then
		debug "nogfx"
		nogfx = YES
		ret = 1
	elseif *opt = "noinput" then
		debug "noinput"
		noinput = YES
		ret = 1
	end if
	'all these take an optional numeric argument, so gobble the arg if it is
	'a number, whether or not it was valid
	if ret = 1 and parse_int(*arg) then ret = 2

	if screen_mode_changed then
		gfx_fb_update_screen_mode
	end if

	return ret
end function

'Figure out what size the window/fullscreen resolution should be, and then recreate the window if needed
sub calculate_and_set_screen_res(fullscreen as bool)
	if fullscreen = NO then
		screensize = framesize * zoom
		screen_buffer_offset = 0
	else
		'Search for a fullscreen resolution that has near-native aspect ratio,
		'and is large enough but no larger than needed, together with a zoom level.

		'Find the native aspect ratio, by getting the largest available resolution,
		'which should be the last one returned from screenlist. We can't just
		'call screeninfo or screencontrol, because if we're fullscreen they return
		'the fullscreen resolution.
		dim modesize as XYPair
		dim mode as integer = screenlist(depth)
		while mode
			modesize = XY(hiword(mode), loword(mode))
			mode = screenlist
		wend
		dim native_aspect_ratio as double
		if modesize.y then native_aspect_ratio = modesize.x / modesize.y

		'debug "max screen size " & modesize & " ratio " & native_aspect_ratio & " zoom " & zoom

		screensize = 0
		dim best_ratio as double = 999
		dim best_cover as double = 0
		dim best_zoom as integer = 0

		mode = screenlist(depth)
		while mode
			modesize = XY(hiword(mode), loword(mode))
			dim ratio as double = modesize.x / modesize.y

			'Try multiple zoom levels
			for tryzoom as integer = 1 to 4
				dim imagesize as XYPair = framesize * tryzoom

				if modesize >= imagesize then
					'screenlist returns resolutions in increasing size, so don't
					'mark a mode best unless it's an improvement
					dim cover as double = (imagesize.x * imagesize.y) / (modesize.x * modesize.y)
					'debug "zoom " & tryzoom & " ratio " & ratio & " dist " & abs(ratio - native_aspect_ratio) & " cover " & cover
					if abs(ratio - native_aspect_ratio) <= abs(best_ratio - native_aspect_ratio) + 0.025 andalso _
					   cover >= best_cover + 0.01 then
						'debug "---best"
						screensize = modesize
						best_zoom = tryzoom
						best_ratio = ratio
						best_cover = cover
					end if
				end if
			next
			mode = screenlist()
		wend
		if best_zoom = 0 then
			debug "failed to find fullscreen mode"
			calculate_and_set_screen_res(NO)
			exit sub
		end if

		remember_windowed_zoom = zoom
		zoom = best_zoom

		screen_buffer_offset = (screensize - framesize * zoom) \ 2
	end if

	if init_gfx then
		'Check whether the new screen mode actually differs from existing
		dim as fb_integer w, h, bpp
		screeninfo w, h, bpp
		if XY(w, h) = screensize andalso bpp = depth andalso window_state.fullscreen = fullscreen then
			'No, it doesn't
			exit sub
		end if
	end if

	window_state.fullscreen = fullscreen
	gfx_fb_screenres
end sub

function gfx_fb_describe_options() as zstring ptr
	return @"-z -zoom [1...16]   Scale screen to 1,2, ... up to 16x normal size (2x default)" LINE_END _
	        "-s -smooth          Enable smoothing filter for zoom modes (default off)" LINE_END _
		"-nogfx              (Unix only) Don't create a window, commandline only. Combine with --print." LINE_END _
		"-noinput            (Unix only) Combine with --nogfx to not read key input, allows running without a tty."
end function

'------------- IO Functions --------------
sub io_fb_init
end sub

sub process_key_event(e as Event, byval value as KeyBits)
	'NOTE: numpad 5 seems to be broken on Windows, events for that key have scancode = 0 regardless of numlock state!

	'On linux, Pause, PrintScreen, and WindowsKey keypresses send events with scancode 0, and multikey shows nothing.

	select case e.scancode
		case scHome to scPageUp, scLeft to scRight, scEnd to scDelete
			'If numlock is on, then when a numerical/period numpad key is pressed the key will
			'generate ascii 0-9 or ., and can be differentiated from arrow and home, etc., keys.
			'Otherwise they both return e.ascii = 0
			if e.ascii then
				extrakeys(e.scancode - scHome + scNumpad7) = value
			else
				extrakeys(e.scancode) = value
			end if
	end select
end sub

private sub debug_key_event(e as Event, eventname as zstring ptr)
	if debugging_io then
		debuginfo *eventname & " scan=" & e.scancode & " (" & scancodename(e.scancode) & ") ascii=" & e.ascii
	end if
end sub

sub process_events()
	if noinput then exit sub

	static last_enter_state as integer
	dim e as Event
	while ScreenEvent(@e)
		'unhide the mouse when the window loses focus
		if e.type = EVENT_WINDOW_LOST_FOCUS then
			if debugging_io then debuginfo "EVENT_WINDOW_LOST_FOCUS"
			setmouse , , 1
			window_state.focused = NO
		elseif e.type = EVENT_WINDOW_GOT_FOCUS then
			if debugging_io then debuginfo "EVENT_WINDOW_GOT_FOCUS"
			update_mouse_visibility()
			window_state.focused = YES
		elseif e.type = EVENT_KEY_PRESS then
			debug_key_event(e, "EVENT_KEY_PRESS")
			if e.ascii <> 0 then inputtext += chr(e.ascii)
			process_key_event(e, 8)
		elseif e.type = EVENT_KEY_REPEAT then
			debug_key_event(e, "EVENT_KEY_REPEAT")
			if e.ascii <> 0 then inputtext += chr(e.ascii)
		elseif e.type = EVENT_KEY_RELEASE then
			debug_key_event(e, "EVENT_KEY_RELEASE")
			process_key_event(e, 0)
		else
			if debugging_io then debuginfo "EVENT " & e.type & " " & e.z
		end if
	wend

	'Don't eat memory if io_textinput is never called
	inputtext = RIGHT(inputtext, 128)

	'the polling thread ought to ensure that these are caught timeously
	'inkey does not seem to be threadsafe (bug 790)
	'if inkey = chr(255) + "k" then post_terminate_signal
	while fb_keyhit
		dim a as integer = getkey
		'there are two different getkey values that cause fb_GfxInkey to return "\255k"
		if a = &h100 or a = &h6bff then post_terminate_signal
	wend
	if multikey(SC_ALT) andalso multikey(SC_F4) then post_terminate_signal

	'Handle alt-enter; we've disabled fbgfx's builtin handling
	'(The window maximise button should be disabled)
	if multikey(SC_ALT) andalso multikey(SC_ENTER) andalso last_enter_state = 0 then
		'debug "ALT ENTER " & main_thread_in_gfx_backend
		'Can't call gfx_fb_setwindowed here, since we might not be on the
		'main thread, so delay it
		want_toggle_fullscreen = YES
	end if
	last_enter_state = multikey(SC_ENTER)
end sub

function gfx_fb_get_resize(byref ret as XYPair) as bool
	'Kludge: this seems the easiest place to act on alt-enter
	'(We can't do this inside gfx_present, because gfxmutex (GFX_ENTER)
	'is held in there, and obtained in gfx_fb_setwindowed; and
	'io_pollkeyevents, io_waitprocessing aren't always called)
	if want_toggle_fullscreen then
		want_toggle_fullscreen = NO
		'Note the inversion: If fullscreen, go windowed
		gfx_fb_setwindowed window_state.fullscreen
		post_event(eventFullscreened, window_state.fullscreen)
	end if
	return NO
end function

sub io_fb_pollkeyevents()
	'not really needed by this backend
	GFX_ENTER
	process_events()
	GFX_EXIT
end sub

sub io_fb_waitprocessing()
	'not needed by this backend
end sub

#ifdef __FB_WIN32__
'vkey: winapi virtual keycode
'is_toggle: read toggle state of the key
local sub vkey_to_keybits(vkey as integer, is_toggle as bool, byref key as KeyBits)
	dim mask as integer = iif(is_toggle, 1, &h8000)
	key = (key and not 8) or iif(GetKeyState(vkey) and mask, 8, 0)
end sub
#endif

'Called with gfxmutex held
sub io_fb_updatekeys(byval keybd as KeyBits ptr)
	process_events()

	for key as KBScancode = 0 to 127
		select case key
			'Pressing PrntScr causes multikey(0) to return true
			case 1 to scHome - 1, scNumpadMinus, scNumpadPlus, scDelete + 1 to scContext
				if multikey(key) then
					keybd[key] or= 8
				end if

			case scNumpad5
				'Events for this key are broken, luckily on X11 can fall back to this.
				'This doesn't work on Windows, but we handle Numpad5 there below.
				if multikey(76) then  'Not an OHR scancode
					keybd[key] or= 8
				end if

			case scHome to scDelete, scNumpad7 to scNumpadPeriod
				keybd[key] or= extrakeys(key)

			'other keys are undetectable with fbgfx!
		end select
	next

	'fbgfx reports separate shift keys, but combined alt and ctrl keys

	keybd[scShift] or= (keybd[scLeftShift] or keybd[scRightShift]) and 8
	keybd[scLeftAlt] or= keybd[scUnfilteredAlt] and 8
	keybd[scRightAlt] or= keybd[scUnfilteredAlt] and 8
	keybd[scLeftCtrl] or= keybd[scCtrl] and 8
	keybd[scRightCtrl] or= keybd[scCtrl] and 8
	'scMeta unsupported

	'Some other keys are also indistinguishable, and are mirrored
	keybd[scNumpadSlash] or= keybd[scSlash] and 8
	keybd[scNumpadEnter] or= keybd[scEnter] and 8
	keybd[scPrintScreen] or= keybd[scNumpadAsterisk] and 8
	keybd[scPause] or= keybd[scNumlock] and 8

	#ifdef __FB_WIN32__
		'Lots of keys broken in fbgfx, but at least on Windows it's easy to
		'workaround the problem by calling the winapi.
		'ScrollLock works in fbgfx, but it doesn't act as a toggle.
		vkey_to_keybits VK_SCROLL, YES, keybd[scScrollLock]
		'Numpad5 broken on Windows, OK on X11
		'(VK_NUMPAD5 only appears presses when numlock is on, but that's consistent)
		vkey_to_keybits VK_NUMPAD5, NO, keybd[scNumpad5]
		'None of the following work properly on any OS
		vkey_to_keybits VK_NUMLOCK, YES, keybd[scNumlock]
		vkey_to_keybits VK_SNAPSHOT, NO, keybd[scPrintScreen]
		'This doesn't work well, it misses most keypresses. Pause is weird.
		vkey_to_keybits VK_PAUSE, NO, keybd[scPause]
	#endif
end sub

sub io_fb_textinput (byval buf as wstring ptr, byval bufsize as integer)
	dim buflen as integer = bufsize \ 2 - 1
	*buf = LEFT(inputtext, buflen)
	inputtext = MID(inputtext, buflen)
end sub

SUB io_fb_show_virtual_keyboard()
	'Does nothing on platforms that have real keyboards
END SUB

SUB io_fb_hide_virtual_keyboard()
	'Does nothing on platforms that have real keyboards
END SUB

sub io_fb_setmousevisibility(visibility as CursorVisibility)
	GFX_ENTER
	mouse_visibility = visibility
	update_mouse_visibility()
	GFX_EXIT
end sub

'Called with gfxmutex held
sub io_fb_getmouse(byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer)
	static lastpos as XYPair, lastwheel as integer = 0, lastbuttons as integer = 0
	dim as integer dmx, dmy, dw, db, remx, remy
	if getmouse(dmx, dmy, dw, db) = 0 then
		'mouse is inside window
		window_state.mouse_over = YES
		if mouseclipped then
			remx = dmx
			remy = dmy
			dmx = bound(dmx, mxmin, mxmax)
			dmy = bound(dmy, mymin, mymax)
			'calling setmouse at the same position rapidly causes the mouse to crawl
			if remx <> dmx or remy <> dmy then setmouse dmx, dmy
		end if
		lastpos = (XY(dmx, dmy) - screen_buffer_offset) \ zoom
		lastwheel = 120 * dw
		lastbuttons = db
	else
		window_state.mouse_over = NO
	end if
	'(At least on X11) when dragging off the window we may see first OFFSCREEN position instead of
	'last onscreen, due to fbgfx freezing mouse input fractionally late.
	'All other gfx backends clamp the return value to the window bounds, so do the same here.
	mx = bound(lastpos.x, 0, framesize.w - 1)
	my = bound(lastpos.y, 0, framesize.h - 1)
	mwheel = lastwheel
	mbuttons = lastbuttons
end sub

'Called with gfxmutex held
sub io_fb_setmouse(byval x as integer, byval y as integer)
	dim mpos as XYPair = XY(x, y) * zoom + screen_buffer_offset
	setmouse(mpos.x, mpos.y)
end sub

'Called with gfxmutex held
sub io_fb_mouserect(byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
	mxmin = screen_buffer_offset.x + xmin * zoom
	mxmax = screen_buffer_offset.x + xmax * zoom + zoom - 1
	mymin = screen_buffer_offset.y + ymin * zoom
	mymax = screen_buffer_offset.y + ymax * zoom + zoom - 1
	if xmin >= 0 then
		'enable clipping
		mouseclipped = YES
		setmouse  , , , 1
	else
		'disable clipping
		mouseclipped = NO
		setmouse  , , , 0
	end if
end sub

'Called with gfxmutex held
function io_fb_get_joystick_state(byval joynum as integer, byval state as IOJoystickState ptr) as integer
	if window_state.focused = NO then return 3  'Not focused

	if joynum > 15 then return 1  'Out of range (fbgfx only supports 16 joysticks)
	static joystick_counter as integer
	'state.instance_id mapping. Used to tell report whether this is a new joystick or not
	static joystick_ids(15) as integer
	static joystick_infos(15) as JoystickInfo

	'Axes which are not present are set to -1000.
	'Those which are present aren't necessarily consecutive on Windows, since there fbgfx
	'sets the axes like so (accessed with plain winapi joyGetPosEx and joyGetDevCaps):
	' 0: X
	' 1: Y
	' 2: Z
	' 3: R
	' 4: U
	' 5: V
	' 6,7: POV hat (direction converted by fbgfx to possible values {-1,0,1} for each axis)
	'On linux, fbgfx reads events from /dev/input/js# and /dev/js#, which seem to
	'treat POV hats as extra axes, not necessarily starting at 6, so no way to distinguish them.
	dim ax(7) as single
	dim as size_t button_bits
	if getjoystick(joynum, button_bits, ax(0), ax(1), ax(2), ax(3), ax(4), ax(5), ax(6), ax(7)) then
		'getjoystick returns 1 on failure if the joystick can't be opened
		if joystick_ids(joynum) then
			joystick_ids(joynum) = 0
			return 2  'Joystick lost
		else
			return 1  'No joystick
		end if
	end if

	dim byref info as JoystickInfo = joystick_infos(joynum)
	state->info = @info
	memset @info, 0, sizeof(info)
	info.structsize = JOYSTICKINFO_SZ

	for i as integer = 0 to 7
		if ax(i) <> -1000 then
			if debugging_io then debuginfo "ax " & i & " " & ax(i)
#ifdef __FB_WIN32__
			if i >= 6 then
				'POV hat (see comment above)
				if ax(i) then
					dim bitnum as integer = 2*(i-6) + iif(ax(i) > 0, 1, 0)
					state->hats(0) or= 1 shl bitnum
				end if
				info.num_hats = 1
				continue for
			end if
#endif
			state->axes(info.num_axes) = 1000 * ax(i)
			info.num_axes += 1
		end if
	next

	state->buttons_down = button_bits

	if joystick_ids(joynum) = 0 then
		joystick_counter += 1
		joystick_ids(joynum) = joystick_counter
		return -1  'Acquired joystick
	else
		return 0  'Success
	end if
end function

sub io_fb_set_clipboard_text(text as zstring ptr)  'ustring
	if text = NULL then text = @""
	#ifdef USE_X11
		' Not supported. For the selection to work, we would need to handle SelectionRequest
		' X11 events, but FB discards these and doesn't forward them. We would have to do peeking
		' of the X11 event queue.
		exit sub
	#elseif defined(__FB_WIN32__)
		dim hwnd as ssize_t 'as HWND
		GFX_ENTER
		screencontrol GET_WINDOW_HANDLE, hwnd
		GFX_EXIT
		WIN_SetClipboardText(cast(HWND, hwnd), text)
	#elseif defined(__FB_DARWIN__)
		Cocoa_SetClipboardText(text)
	#endif
end sub

function io_fb_get_clipboard_text() as zstring ptr  'ustring
	dim ret as zstring ptr
	#ifdef USE_X11
		dim wndw as Window
		dim displayi as ssize_t
		dim display as Display ptr
		GFX_ENTER
		screencontrol GET_WINDOW_HANDLE, wndw, displayi
		GFX_EXIT
		#if __FB_VER_MAJOR__ = 1 and __FB_VER_MINOR__ <= 5
			'Getting display via GET_WINDOW_HANDLE is only in FB 1.06.0
			display = fb_x11.display
		#else
			display = cptr(Display ptr, displayi)
		#endif
		ret = X11_GetClipboardText(display, wndw, NULL)  'No callback, just waits 40ms
	#elseif defined(__FB_WIN32__)
		dim hwnd as ssize_t 'as HWND
		GFX_ENTER
		screencontrol GET_WINDOW_HANDLE, hwnd
		GFX_EXIT
		ret = WIN_GetClipboardText(cast(HWND, hwnd))
	#elseif defined(__FB_DARWIN__)
		ret = Cocoa_GetClipboardText()
	#endif
	return ret
end function

function gfx_fb_setprocptrs() as integer
	gfx_init = @gfx_fb_init
	gfx_close = @gfx_fb_close
	gfx_getversion = @gfx_fb_getversion
	gfx_setpal = @gfx_fb_setpal
	gfx_screenshot = @gfx_fb_screenshot
	gfx_setwindowed = @gfx_fb_setwindowed
	gfx_windowtitle = @gfx_fb_windowtitle
	gfx_getwindowstate = @gfx_fb_getwindowstate
	gfx_get_screen_size = @gfx_fb_get_screen_size
	gfx_supports_variable_resolution = @gfx_fb_supports_variable_resolution
	gfx_get_resize = @gfx_fb_get_resize
	gfx_get_settings = @gfx_fb_get_settings
	gfx_set_settings = @gfx_fb_set_settings
	gfx_setoption = @gfx_fb_setoption
	gfx_describe_options = @gfx_fb_describe_options
	io_init = @io_fb_init
	io_pollkeyevents = @io_fb_pollkeyevents
	io_waitprocessing = @io_fb_waitprocessing
	io_keybits = @io_amx_keybits
	io_updatekeys = @io_fb_updatekeys
	io_textinput = @io_fb_textinput
	io_get_clipboard_text = @io_fb_get_clipboard_text
	io_set_clipboard_text = @io_fb_set_clipboard_text
	io_show_virtual_keyboard = @io_fb_show_virtual_keyboard
	io_hide_virtual_keyboard = @io_fb_hide_virtual_keyboard
	io_mousebits = @io_amx_mousebits
	io_setmousevisibility = @io_fb_setmousevisibility
	io_getmouse = @io_fb_getmouse
	io_setmouse = @io_fb_setmouse
	io_mouserect = @io_fb_mouserect
	io_get_joystick_state = @io_fb_get_joystick_state

	'new render API
	gfx_present = @gfx_fb_present

	return 1
end function

end extern
