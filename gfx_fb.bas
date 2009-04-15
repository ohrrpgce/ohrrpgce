''
'' gfx_fb.bas - External graphics functions implemented in FB's
''				built-in gfxlib. Multi-option version.
''
'' part of OHRRPGCE - see elsewhere for license details
''

option explicit

#include "fbgfx.bi"
#include "gfx.bi"

#include once "crt.bi"
#undef abort
#undef strlen

'subs only used internally
declare sub gfx_screenres()		'set screen res, etc

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

'internal palette for 32-bit mode, with RGB colour components packed into a int
dim shared truepal(255) as integer

declare sub debug(s$)

'Note, init is called before the browser is shown, and close is
'called when an RPG is exited, they will usually be run more than
'once. Perhaps there is also call for once-only routines outside
'the main loop?
sub gfx_init
	if init_gfx = 0 then
		gfx_screenres
		screenset 1, 0
		init_gfx = 1
	end if
end sub

sub gfx_screenres
	if windowed = 0 then
		screenres screenmodex, screenmodey, depth, 1, GFX_FULLSCREEN
	else
		screenres screenmodex, screenmodey, depth, 1, GFX_WINDOWED
	end if
end sub

sub gfx_close
end sub

sub gfx_showpage(byval raw as ubyte ptr)
'takes a pointer to raw 8-bit data at 320x200
	dim rptr as ubyte ptr
	dim as integer mult = 1
	dim as integer w, h, i, j
	dim as integer fx, fy, p0, p1, p2, p3, p4, pstep'for 2x/3x filtering

	screenlock

	for i = 2 to zoom
		mult = mult shl 8 + 1
	next

	rptr = raw

	if depth = 8 then
		dim sptr as ubyte ptr
		sptr = screenptr + (screen_buffer_offset * 320 * zoom)

		if zoom = 1 then
			memcpy sptr, rptr, 320 * 200
		else
			for h = 0 to 200 - 1
				for w = 320 / 4 - 1 to 0 step -1
					'could just multiple by &h1010101, but FB produces truly daft code for multiplication by constants
					*cast(integer ptr, sptr) = rptr[0] * mult
					sptr += zoom
					*cast(integer ptr, sptr) = rptr[1] * mult
					sptr += zoom
					*cast(integer ptr, sptr) = rptr[2] * mult
					sptr += zoom
					*cast(integer ptr, sptr) = rptr[3] * mult
					sptr += zoom
					rptr += 4
				next
				'repeat row zoom times
				for i = 2 to zoom
					memcpy sptr, sptr - 320 * zoom, 320 * zoom
					sptr += 320 * zoom
				next
			next
		end if
		if smooth = 1 and (zoom = 2 or zoom = 3) then
			'added for 2x/3x filtering
			if screenmodex > 640 then pstep = 1 else pstep = 2
			sptr = screenptr + (screen_buffer_offset * 320 * zoom)
			dim as ubyte ptr sptr1, sptr2, sptr3
			for fy = 1 to (screenmodey - 2) step pstep
				sptr1 = sptr + ((fy - 1) * screenmodex) + 1  '(1,0)
				sptr2 = sptr1 + screenmodex '(1,1)
				sptr3 = sptr2 + screenmodex '(1,2)
				for fx = (screenmodex - 2) to 1 step -1
					'p0=point(fx,fy)
					'p1=point(fx-1,fy-1)'nw
					'p2=point(fx+1,fy-1)'ne
					'p3=point(fx+1,fy+1)'se
					'p4=point(fx-1,fy+1)'sw
					'if p1 = p3 then p0 = p1
					'if p2 = p4 then p0 = p2
					if sptr1[1] = sptr3[-1] then
						sptr2[0] = sptr1[1]
					else
						if sptr1[-1] = sptr3[1] then sptr2[0] = sptr1[-1]
					end if
					'pset(fx,fy),p0
					sptr1 += 1
					sptr2 += 1
					sptr3 += 1
				next fx
			next fy
		end if
	else
		'true colour
		dim xptr as integer ptr
		dim pixel as integer
		xptr = screenptr
		xptr += (screen_buffer_offset * 320 * zoom)
		for h = 0 to 200 - 1
			'repeat row zoom times
			for w = 0 to 320 - 1
				'get colour
				pixel = truepal(*rptr)
				'zoom sptrs for each rptr
				for j = zoom to 1 step -1
					*xptr = pixel
					xptr += 1
				next
				rptr += 1
			next
			for i = 2 to zoom
				memcpy xptr, xptr - 320 * zoom, 4 * 320 * zoom
				xptr += 320 * zoom
			next
		next
		if smooth = 1 and (zoom = 2 or zoom = 3) then
			'this is duplicated from the 8-bit smoothing code because there is no
			'way to write this code as a function that would accept both ubyte ptr and integer ptr
			'added for 2x/3x filtering
			if screenmodex > 640 then pstep = 1 else pstep = 2
			xptr = screenptr + (screen_buffer_offset * 320 * zoom)
			dim as integer ptr xptr1, xptr2, xptr3
			for fy = 1 to (screenmodey - 2) step pstep
				xptr1 = xptr + ((fy - 1) * screenmodex) + 1  '(1,0)
				xptr2 = xptr1 + screenmodex '(1,1)
				xptr3 = xptr2 + screenmodex '(1,2)
				for fx = (screenmodex - 2) to 1 step -1
					'p0=point(fx,fy)
					'p1=point(fx-1,fy-1)'nw
					'p2=point(fx+1,fy-1)'ne
					'p3=point(fx+1,fy+1)'se
					'p4=point(fx-1,fy+1)'sw
					'if p1 = p3 then p0 = p1
					'if p2 = p4 then p0 = p2
					if xptr1[1] = xptr3[-1] then
						xptr2[0] = xptr1[1]
					else
						if xptr1[-1] = xptr3[1] then xptr2[0] = xptr1[-1]
					end if
					'pset(fx,fy),p0
					xptr1 += 1
					xptr2 += 1
					xptr3 += 1
				next fx
			next fy
		end if
	end if
	screenunlock
	flip

end sub

sub gfx_setpal(pal() as RGBcolor)
	dim as integer i
	if depth = 8 then
		for i = 0 to 255
			palette i, pal(i).r, pal(i).g, pal(i).b
		next
	end if
	'copy the palette, both for 32bit colour mode and when changing
	'res requires resetting the palette
	for i = 0 to 255
		truepal(i) = RGB(pal(i).r, pal(i).g, pal(i).b)
	next
	'This does not update the page "live", like the 8-bit version
	'so fades aren't working, and there's no way to force an
	'update from here at the moment because the screen buffer is not
	'accessible.
end sub

function gfx_screenshot(fname as string, byval page as integer) as integer
	gfx_screenshot = 0
end function

sub gfx_setwindowed(byval iswindow as integer)
	if iswindow <> 0 then iswindow = 1 'only 1 "true" value
	if iswindow = windowed then exit sub

	windowed = iswindow

	if init_gfx = 1 then
		dim i as integer
		gfx_screenres
        'palette must be re-set
		if depth = 8 then
			for i = 0 to 255
				palette i, (truepal(i) and &hFF0000) shr 16, (truepal(i) and &hFF00) shr 8, truepal(i) and &hFF
			next
		end if
	end if
end sub

sub gfx_togglewindowed()
	if windowed = 0 then
		gfx_setwindowed(1)
	else
		gfx_setwindowed(0)
	end if
end sub

sub gfx_windowtitle(title as string)
	if len(title) = 0 then
		windowtitle ""
	else
		windowtitle title
	end if
end sub

sub gfx_setoption(opt as string, byval value as integer = -1)
'handle command-line options in a generic way, so that they
'can be ignored or supported as the library permits.
'This version supports
'	zoom (1, 2*, 3),
'	depth (8*, 24),
'	border (0*, 1)
'	smooth (0*, 1)
'only before gfx has been initialised

	if init_gfx = 0 then
		if opt = "zoom" then
			'default zoom is 2, 1 is the only other valid value
			if value = 1 then
				zoom = 1
			elseif value = 3 then
				zoom = 3
			else
				zoom = 2
			end if
		elseif opt = "depth" then
			if value = 24 or value = 32 then
				depth = value
			else
				depth = 8
			end if
		elseif opt = "border" then
			if value = 1 then
				bordered = 1
			else
				bordered = 0
			end if
		elseif opt = "smooth" then
			if value = 1 then
				smooth = 1
			else
				smooth = 0
			end if
		end if
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
		elseif zoom = 3 then
			bordered = 0 ' bordered mode is not supported in 3x zoom
			screen_buffer_offset = 0
			screenmodex = 960
			screenmodey = 600
		else
			screenmodex = 640
			screenmodey = 400 + (bordered * BORDER * zoom)
		end if
		'calculate offset
		if bordered = 1 and zoom <> 3 then screen_buffer_offset = (BORDER / 2) * zoom
	end if

end sub

function gfx_describe_options() as string
 dim s as string
 dim lineend as string
#ifdef __FB_LINUX__
 lineend = chr(10)
#else
 lineend = chr(13) & chr(10)
#endif
 s =     "-z -zoom [1|2|3]    Scale screen to 1x 2x or 3x normal size (2x default)" & lineend
 s = s & "-b -border [0|1]    Add a letterbox border (default off)" & lineend
 s = s & "-d -depth [8|24|32] Set color bit-depth (default 8-bit)" & lineend
 s = s & "-s -smooth          Enable smoothing filter for zoom modes (default off)"
 return s
end function

'------------- IO Functions --------------
sub io_init
	setmouse(0, 0, 0) 'hide mouse
end sub

sub io_pollkeyevents()
	'not needed by this backend
end sub

sub io_updatekeys(keybd() as integer)
	dim as integer a
	for a = 0 to &h7f
		if multikey(a) then
			keybd(a) = keybd(a) or 8
		end if
	next
end sub

function io_enablemouse() as integer
'returns 0 if mouse okay
	'This fails if mouse is outside window, so just always return 0
' 	dim as integer mx, my, mw, mb
' 	getmouse(mx, my, mw, mb)
' 	if (mb = -1) then	'no mouse if button = -1
' 		io_enablemouse = -1
' 		'debug "No mouse detected"
' 		exit function
' 	end if
	io_enablemouse = 0
end function

sub io_getmouse(mx as integer, my as integer, mwheel as integer, mbuttons as integer)
	static as integer lastx = 0, lasty = 0, lastwheel = 0, lastbuttons = 0
	dim as integer dmx, dmy, dw, db
	if getmouse(dmx, dmy, dw, db) = 0 then
		'mouse is inside window
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

sub io_setmouse(byval x as integer, byval y as integer)
	setmouse(x * zoom, y * zoom + screen_buffer_offset)
end sub

sub io_mouserect(byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
	'nothing to do
end sub

function io_readjoy(joybuf() as integer, byval joynum as integer) as integer

	dim x as single,y as single,button as integer

	if getjoystick(joynum,button,x,y) then 'returns 1 on failure
	  return 0
	end if

	'otherwise...
	joybuf(0) = int(x * 100) 'x is between -1 and 1
	joybuf(1) = int(y * 100) 'ditto
	joybuf(2) = (button AND 1) = 0 '0 = pressed, not 0 = unpressed (why???)
	joybuf(3) = (button AND 2) = 0 'ditto
	'if abs(joybuf(0)) > 10 then debug "X = " + str(joybuf(0))
	return 1

end function

function io_readjoysane(byval joynum as integer, byref button as integer, byref x as integer, byref y as integer) as integer
	dim as single xa, ya
	if getjoystick(joynum,button,xa,ya) then 'returns 1 on failure
		return 0
	end if

	x = int(xa * 100)
	y = int(ya * 100)
	'if abs(x) > 10 then debug "X = " + str(x)
  return 1

end function
