'OHRRPGCE - FreeBASIC gfxlib-based graphics backend
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#include "config.bi"

#ifdef USE_X11
	#include "lib/SDL/SDL_x11clipboard.bi"
	#undef font

	' HACK: FB 1.05 and earlier don't have a way to get the Display ptr, only the Window
	' But the Window ptr happens to be the first member of fb_x11
	type X11DRIVER
		display as Display ptr
		'rest omitted
	end type
	extern fb_x11 alias "fb_x11" as X11DRIVER

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

'a public rtlib function that they seem to have forgotten to expose to FB programs?
declare function fb_KeyHit alias "fb_KeyHit" () as long

extern "C"

'subs only used internally
declare sub gfx_fb_screenres()		'set screen res, etc
declare sub calculate_screen_res()
declare sub update_mouse_visibility()


'border required to fit standard 4:3 screen at zoom 1
#define BORDER 20

dim shared screen_buffer_offset as integer = 0
dim shared window_state as WindowState
dim shared init_gfx as bool = NO
'defaults are 2x zoom and 640x400 in 8-bit
dim shared zoom as integer = 2
dim shared screenmodex as integer = 640
dim shared screenmodey as integer = 400
dim shared bordered as integer = 0  '0 or 1
dim shared depth as integer = 32   '0 means use native
dim shared smooth as integer = 0  '0 or 1
dim shared mouseclipped as bool = NO
dim shared mouse_visibility as CursorVisibility = cursorDefault
dim shared remember_windowtitle as string
dim shared as integer mxmin = -1, mxmax = -1, mymin = -1, mymax = -1
dim shared inputtext as string
dim shared extrakeys(127) as KeyBits

'Internal palette for 32-bit mode
dim shared truepal(255) as RGBcolor


function gfx_fb_init(byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr, byval info_buffer as zstring ptr, byval info_buffer_size as integer) as integer
	if init_gfx = NO then
		#ifdef USE_X11
			'Xlib will kill the program if most errors occur, such as if OpenGL on the machine is broken
			'so the window can't be created. We need to install an error handler to prevent that
			set_X11_error_handlers
		#endif

		dim bpp as fb_integer 'bits, not bytes. see, bits is b, bytes is B
		dim refreshrate as fb_integer
		dim driver as string
		dim as fb_integer w, h
		'Poll the size of the screen
		screeninfo w, h, bpp, , , refreshrate, driver
		debuginfo "gfx_fb: native screensize=" & w & "*" & h & " bitdepth=" & bpp & " refreshrate=" & refreshrate
		if depth = 0 then depth = iif(bpp = 24, 32, bpp)

		calculate_screen_res
		gfx_fb_screenres
		screenset 1, 0    'Only want one FB video page
		init_gfx = YES
		screeninfo w, h, bpp, , , refreshrate, driver
		*info_buffer = MID(bpp & "bpp, " & refreshrate & "Hz, " & driver & " driver", 1, info_buffer_size)
	end if
	window_state.structsize = WINDOWSTATE_SZ
	window_state.focused = YES
	window_state.minimised = NO
	window_state.fullscreen = NO
	window_state.mouse_over = NO
	return 1
end function

local sub gfx_fb_screenres
	if window_state.fullscreen = YES then
		screenres screenmodex, screenmodey, depth, 1, GFX_FULLSCREEN
	else
		screenres screenmodex, screenmodey, depth, 1, GFX_WINDOWED
	end if
	'hook_fb_End is a kludge that works by setting the gfxlib2 exit hook,
	'which screenres sets, so have to reset it afterwards. See miscc.c
	hook_fb_End()
	update_mouse_visibility()
end sub

private sub update_mouse_visibility
	dim vis as integer  '0 or 1
	if mouse_visibility = cursorDefault then
		' window_state.fullscreen is an approximation (see process_events()),
		' and because it's so unreliable we need a constant default.
#ifdef IS_GAME
		vis = 0
#else
		vis = 1
#endif
		'if window_state.fullscreen = YES then vis = 0 else vis = 1
	elseif mouse_visibility = cursorVisible then
		vis = 1
	else
		vis = 0
	end if
	setmouse  , , vis
end sub

sub gfx_fb_update_screen_mode()
	if init_gfx then
		GFX_ENTER
		calculate_screen_res
		gfx_fb_screenres
		windowtitle remember_windowtitle
		'Palette must be re-set
		if depth = 8 then
			for i as integer = 0 to 255
				with truepal(i)
					palette i, .r, .g, .b
				end with
			next
		end if
		GFX_EXIT
	end if
end sub

sub gfx_fb_close
	screen 0
	init_gfx = NO
end sub

function gfx_fb_getversion() as integer
	return 1
end function

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

function gfx_fb_present(byval surfaceIn as Surface ptr, byval pal as RGBPalette ptr) as integer
'320x200 Surfaces supported only!
	dim ret as integer = 0

	if surfaceIn->format = .SF_32bit and depth <> 32 then
		debuginfo "gfx_fb_present: switching to 32 bit mode"
		depth = 32
		gfx_fb_screenres
	end if

	screenlock
	dim as ubyte ptr screenpixels = screenptr + (screen_buffer_offset * 320 * zoom)

	with *surfaceIn
		if .format = SF_8bit then
			gfx_fb_setpal(cast(RGBcolor ptr, @pal->col(0)))

			if depth = 8 then
				smoothzoomblit_8_to_8bit(.pPaletteData, screenpixels, .size, .width * zoom, zoom, smooth)
			elseif depth = 32 then
				smoothzoomblit_8_to_32bit(.pPaletteData, cast(uint32 ptr, screenpixels), .size, .width * zoom, zoom, smooth, @truepal(0))
			end if
		else  '32 bit
			if depth = 32 then
				smoothzoomblit_32_to_32bit(.pColorData, cast(uint32 ptr, screenpixels), .size, .width * zoom, zoom, smooth)
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

	window_state.fullscreen = wantfullscreen
	gfx_fb_update_screen_mode
end sub

sub gfx_fb_windowtitle(byval title as zstring ptr)
	remember_windowtitle = *title
	windowtitle *title
end sub

function gfx_fb_getwindowstate() as WindowState ptr
	window_state.windowsize = XY(screenmodex, screenmodey)
	window_state.zoom = zoom
	return @window_state
end function

sub gfx_fb_get_screen_size(wide as integer ptr, high as integer ptr)
	dim as ssize_t wide_, high_  'for 64 bit builds
	ScreenControl GET_DESKTOP_SIZE, wide_, high_
	*wide = wide_
	*high = high_
end sub


function gfx_fb_setoption(byval opt as zstring ptr, byval arg as zstring ptr) as integer
'handle command-line options in a generic way, so that they
'can be ignored or supported as the library permits.
'This version supports
'	zoom (1, 2*, ..., 16),
'	depth (8*, 32),
'	border (0*, 1)
'	smooth (0*, 1)
'Changing mode after window already created isn't well tested!
	dim as integer value = str2int(*arg, -1)
	dim as integer ret = 0
	dim as bool screen_mode_changed = NO
	if *opt = "zoom" or *opt = "z" then
		if value >= 1 and value <= 16 then
			zoom = value
		end if
		screen_mode_changed = YES
		ret = 1
	elseif *opt = "depth" or *opt = "d" then
		if value = 24 or value = 32 then
			depth = 32  '24 would screw things up
		else
			depth = 8
		end if
		screen_mode_changed = YES
		ret = 1
	elseif *opt = "border" or *opt = "b" then
		if value = 1 or value = -1 then  'arg optional
			bordered = 1
		else
			bordered = 0
		end if
		screen_mode_changed = YES
		ret = 1
	elseif *opt = "smooth" or *opt = "s" then
		if value = 1 or value = -1 then  'arg optional
			smooth = 1
		else
			smooth = 0
		end if
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

sub calculate_screen_res()
	'FIXME: this is an utter mess
	'FIXME: fullscreen doesn't work if the zoom results in an odd resolution like 960x600.
	'calculate mode
	if zoom = 1 then
		if depth = 8 then
			screenmodex = 320
			screenmodey = 200 + (bordered * BORDER * zoom)
		else
			'only bordered is supported in 24-bit it seems
			bordered = 1
			screenmodex = 320
			screenmodey = 240
		end if
	elseif zoom = 2 then
		screenmodex = 640
		screenmodey = 400 + (bordered * BORDER * zoom)
	elseif zoom >= 3 then
		bordered = 0 ' bordered mode is not supported
		screen_buffer_offset = 0
		screenmodex = 320 * zoom
		screenmodey = 200 * zoom
	end if
	'calculate offset
	if bordered = 1 and zoom < 3 then screen_buffer_offset = (BORDER / 2) * zoom
end sub

function gfx_fb_describe_options() as zstring ptr
	return @"-z -zoom [1...16]   Scale screen to 1,2, ... up to 16x normal size (2x default)" LINE_END _
	        "-b -border [0|1]    Add a letterbox border (default off)" LINE_END _
	        "-d -depth [8|32]    Set color bit-depth (default 8-bit)" LINE_END _
	        "-s -smooth          Enable smoothing filter for zoom modes (default off)"
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
'	static last_enter_state as integer
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

	'Try to catch fullscreening to reduce mouse visibility confusion. However, fullscreening
	'with the window button is not considered.
	'FB's X11 backend in effect doesn't report key events for alt+enter, nor will you ever see both
	'pressed at once with multikey, because the moment that they are, FB (the X11 backend anyway)
	'toggles fullscreen, resetting the state.
	'And FB has no way to check whether we're fullscreen.
/'	if multikey(SC_ALT) andalso multikey(SC_ENTER) andalso last_enter_state = 0 then
		window_state.fullscreen xor= YES
		post_event(eventFullscreened, window_state.fullscreen)
		update_mouse_visibility()
	end if
	last_enter_state = multikey(SC_ENTER)
'/
end sub

sub io_fb_pollkeyevents()
	'not really needed by this backend
	process_events()
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
	mouse_visibility = visibility
	update_mouse_visibility()
end sub

sub io_fb_getmouse(byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer)
	'FIXME: this is broken in fullscreen -z 2, with the Y position offset from the true, even if
	'FB accurately knows that it's running in fullscreen.
	static as integer lastx = 0, lasty = 0, lastwheel = 0, lastbuttons = 0
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
		dmx = dmx \ zoom
		dmy = (dmy \ zoom) - screen_buffer_offset
		lastx = dmx
		lasty = dmy
		lastwheel = 120 * dw
		lastbuttons = db
	else
		window_state.mouse_over = NO
	end if
	mx = lastx
	my = lasty
	mwheel = lastwheel
	mbuttons = lastbuttons
end sub

sub io_fb_setmouse(byval x as integer, byval y as integer)
	setmouse(x * zoom, y * zoom + screen_buffer_offset)
end sub

sub io_fb_mouserect(byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
	mxmin = xmin * zoom
	mxmax = xmax * zoom + zoom - 1
	mymin = (ymin + screen_buffer_offset) * zoom
	mymax = (ymax + screen_buffer_offset) * zoom + zoom - 1
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
		screencontrol GET_WINDOW_HANDLE, hwnd
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
		screencontrol GET_WINDOW_HANDLE, wndw, displayi
		display = cptr(Display ptr, displayi)
		'Getting display via GET_WINDOW_HANDLE is only in FB 1.06.0
		display = fb_x11.display
		ret = X11_GetClipboardText(display, wndw, NULL)  'No callback, just waits 40ms
	#elseif defined(__FB_WIN32__)
		dim hwnd as ssize_t 'as HWND
		screencontrol GET_WINDOW_HANDLE, hwnd
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
