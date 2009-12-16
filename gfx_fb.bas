''
'' gfx_fb.bas - External graphics functions implemented in FB's
''				built-in gfxlib. Multi-option version.
''
'' part of OHRRPGCE - see elsewhere for license details
''

option explicit

#include "fbgfx.bi"
#include "gfx.bi"
#include "compat.bi"  'for LINE_END
#include "common.bi"

#include once "crt.bi"
#undef abort
#undef strlen

'a public rtlib function that they seem to have forgotten to expose to FB programs?
declare function fb_KeyHit alias "fb_KeyHit" () as integer
'OK, now this one is in manual, but it's not exposed either! WTH
declare function fb_Getkey alias "fb_Getkey" () as integer

extern "C"

'subs only used internally
declare sub gfx_fb_screenres()		'set screen res, etc
declare sub calculate_screen_res()

'border required to fit standard 4:3 screen at zoom 1
#define BORDER 20

dim shared screen_buffer_offset as integer = 0
dim shared windowed as integer = 1
dim shared init_gfx as integer = 0
'defaults are 2x zoom and 640x400 in 8-bit
dim shared zoom as integer = 2
dim shared screenmodex as integer = 640
dim shared screenmodey as integer = 400
dim shared bordered as integer = 0
dim shared depth as integer = 8
dim shared smooth as integer = 0
dim shared mouseclipped as integer = 0
dim shared rememmvis as integer = 1
dim shared as integer mxmin = -1, mxmax = -1, mymin = -1, mymax = -1

'internal palette for 32-bit mode, with RGB colour components packed into a int
dim shared truepal(255) as integer


function gfx_fb_init(byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr) as integer
	if init_gfx = 0 then
		calculate_screen_res
		gfx_fb_screenres
		screenset 1, 0
		init_gfx = 1
		dim bpp as integer 'bits, not bytes. see, bits is b, bytes is B
		dim driver as string
		screeninfo , , bpp, , , , driver
		gfxbackendinfo += ", " & bpp & "bpp, " & driver & " driver"
	end if
	return 1
end function

sub gfx_fb_screenres
	if windowed = 0 then
		screenres screenmodex, screenmodey, depth, 1, GFX_FULLSCREEN
		setmouse , , 0
	else
		screenres screenmodex, screenmodey, depth, 1, GFX_WINDOWED
		setmouse , , rememmvis
	end if
end sub

sub gfx_fb_close
end sub

sub gfx_fb_showpage(byval raw as ubyte ptr, byval w as integer, byval h as integer)
'takes a pointer to raw 8-bit data at 320x200 (don't claim that anything else is supported)
	screenlock

	dim as ubyte ptr sptr = screenptr + (screen_buffer_offset * 320 * zoom)

	if depth = 8 then
		smoothzoomblit_8_to_8bit(raw, sptr, w, h, w * zoom, zoom, smooth)
	elseif depth = 32 then
		smoothzoomblit_8_to_32bit(raw, sptr, w, h, w * zoom, zoom, smooth, @truepal(0))
	else
		debug "gfx_fb_showpage: depth " & depth
	end if

	screenunlock
	flip
end sub

sub gfx_fb_setpal(byval pal as RGBcolor ptr)
	dim as integer i
	if depth = 8 then
		for i = 0 to 255
			palette i, pal[i].r, pal[i].g, pal[i].b
		next
	end if
	'copy the palette, both for 32bit colour mode and when changing
	'res requires resetting the palette
	for i = 0 to 255
		truepal(i) = RGB(pal[i].r, pal[i].g, pal[i].b)
	next
	'This does not update the page "live", like the 8-bit version
	'so fades aren't working, and there's no way to force an
	'update from here at the moment because the screen buffer is not
	'accessible.
end sub

function gfx_fb_screenshot(byval fname as zstring ptr) as integer
	gfx_fb_screenshot = 0
end function

sub gfx_fb_setwindowed(byval iswindow as integer)
	if iswindow <> 0 then iswindow = 1 'only 1 "true" value
	if iswindow = windowed then exit sub

	windowed = iswindow

	if init_gfx = 1 then
		dim i as integer
		gfx_fb_screenres
		'palette must be re-set
		if depth = 8 then
			for i = 0 to 255
				palette i, (truepal(i) and &hFF0000) shr 16, (truepal(i) and &hFF00) shr 8, truepal(i) and &hFF
			next
		end if
	end if
end sub

sub gfx_fb_windowtitle(byval title as zstring ptr)
	if len(title) = 0 then
		windowtitle ""
	else
		windowtitle *title
	end if
end sub

function gfx_fb_getwindowstate() as WindowState ptr
	return 0
end function

function gfx_fb_setoption(byval opt as zstring ptr, byval arg as zstring ptr) as integer
'handle command-line options in a generic way, so that they
'can be ignored or supported as the library permits.
'This version supports
'	zoom (1, 2*, 3, 4),
'	depth (8*, 32),
'	border (0*, 1)
'	smooth (0*, 1)
'only before gfx has been initialised
	dim as integer value = str2int(*arg, -1)
	dim as integer ret = 0
	if init_gfx = 0 then
		if *opt = "zoom" or *opt = "z" then
			if value >= 1 and value <= 4 then
				zoom = value
			end if
			ret = 1
		elseif *opt = "depth" or *opt = "d" then
			if value = 24 or value = 32 then
				depth = value
			else
				depth = 8
			end if
			ret = 1
		elseif *opt = "border" or *opt = "b" then
			if value = 1 or value = -1 then  'arg optional
				bordered = 1
			else
				bordered = 0
			end if
			ret = 1
		elseif *opt = "smooth" or *opt = "s" then
			if value = 1 or value = -1 then  'arg optional
				smooth = 1
			else
				smooth = 0
			end if
			ret = 1
		end if
		'all these take numeric arguments, so gobble the arg if it is
		'a number, whether or not it was valid
		if ret = 1 and is_int(*arg) then ret = 2
	end if

	return ret
end function

sub calculate_screen_res()
	'FIXME: this is an utter mess
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
	elseif zoom = 3 then
		bordered = 0 ' bordered mode is not supported in 3x zoom
		screen_buffer_offset = 0
		screenmodex = 960
		screenmodey = 600
	elseif zoom = 4 then
		bordered = 0 ' bordered mode is not supported in 3x zoom
		screen_buffer_offset = 0
		screenmodex = 1280
		screenmodey = 800
	end if
	'calculate offset
	if bordered = 1 and zoom < 3 then screen_buffer_offset = (BORDER / 2) * zoom
end sub

function gfx_fb_describe_options() as zstring ptr
	return @"-z -zoom [1|2|3|4]  Scale screen to 1,2,3 or 4x normal size (2x default)" LINE_END _
	        "-b -border [0|1]    Add a letterbox border (default off)" LINE_END _
	        "-d -depth [8|32]    Set color bit-depth (default 8-bit)" LINE_END _
	        "-s -smooth          Enable smoothing filter for zoom modes (default off)"
end function

'------------- IO Functions --------------
sub io_fb_init
	'setmouse , , 0 'hide mouse
end sub

sub process_events()
'	static last_enter_state as integer
	dim e as Event
	while ScreenEvent(@e)
		'unhide the mouse when the window loses focus
		if e.type = EVENT_WINDOW_LOST_FOCUS then
			setmouse , , 1
		end if
		if e.type = EVENT_WINDOW_GOT_FOCUS then
			if windowed then
				setmouse , , rememmvis
			else
				setmouse , , 0
			end if
		end if
	wend


	'the polling thread ought to ensure that these are caught timeously
	'inkey does not seem to be threadsafe (bug 790)
	'if inkey = chr(255) + "k" then post_terminate_signal
	while fb_keyhit
		dim a as integer = fb_getkey
		'there are two different fb_getkey values that cause fb_GfxInkey to return "\255k"
		if a = &h100 or a = &h6bff then post_terminate_signal
	wend
	if multikey(SC_ALT) andalso multikey(SC_F4) then post_terminate_signal

	'Try to catch fullscreening to reduce mouse visibility confusion. However, fullscreening
	'with the window button is not considered.
	'We can't directly detect alt+enter because FB doesn't report key events when alt is held
	'extremely insensitive, useless.
/'	if multikey(SC_ALT) andalso multikey(SC_ENTER) andalso last_enter_state = 0 then
		windowed xor= 1
		if windowed then
			setmouse , , rememmvis
		else
			setmouse , , 0
		end if
	end if
	last_enter_state = multikey(SC_ENTER)
'/
end sub

sub io_fb_pollkeyevents()
	'not really needed by this backend
	process_events()
end sub

sub io_fb_updatekeys(byval keybd as integer ptr)
	process_events()

	for a as integer = 0 to &h7f
		if multikey(a) then
			keybd[a] = keybd[a] or 8
		end if
	next
end sub

sub io_fb_setmousevisibility(byval visible as integer)
	'Note that 'windowed' is an approximation - see process_events()
	rememmvis = iif(visible, 1, 0)
	if windowed then
		setmouse , , rememmvis
	else
		setmouse , , 0
	end if
end sub

sub io_fb_getmouse(mx as integer, my as integer, mwheel as integer, mbuttons as integer)
	static as integer lastx = 0, lasty = 0, lastwheel = 0, lastbuttons = 0
	dim as integer dmx, dmy, dw, db, remx, remy
	if getmouse(dmx, dmy, dw, db) = 0 then
		'mouse is inside window
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
		lastwheel = dw
		lastbuttons = db
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
		mouseclipped = 1
		setmouse  , , , 1
	else
		'disable clipping
		mouseclipped = 0
		setmouse  , , , 0
	end if
end sub

function io_fb_readjoysane(byval joynum as integer, byref button as integer, byref x as integer, byref y as integer) as integer
	dim as single xa, ya
	if getjoystick(joynum, button, xa, ya) then 'returns 1 on failure
		return 0
	end if

	x = int(xa * 100)
	y = int(ya * 100)
	'if abs(x) > 10 then debug "X = " + str(x)
	return 1
end function

function gfx_fb_setprocptrs() as integer
	gfx_close = @gfx_fb_close
	gfx_showpage = @gfx_fb_showpage
	gfx_setpal = @gfx_fb_setpal
	gfx_screenshot = @gfx_fb_screenshot
	gfx_setwindowed = @gfx_fb_setwindowed
	gfx_windowtitle = @gfx_fb_windowtitle
	gfx_getwindowstate = @gfx_fb_getwindowstate
	gfx_setoption = @gfx_fb_setoption
	gfx_describe_options = @gfx_fb_describe_options
	io_init = @io_fb_init
	io_pollkeyevents = @io_fb_pollkeyevents
	io_keybits = @io_amx_keybits
	io_updatekeys = @io_fb_updatekeys
	io_mousebits = @io_amx_mousebits
	io_setmousevisibility = @io_fb_setmousevisibility
	io_getmouse = @io_fb_getmouse
	io_setmouse = @io_fb_setmouse
	io_mouserect = @io_fb_mouserect
	io_readjoysane = @io_fb_readjoysane

	return 1
end function

end extern
