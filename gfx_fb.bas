'' 
'' gfx_fb.bas - External graphics functions implemented in FB's
''				built-in gfxlib. Multi-option version.
''
'' part of OHRRPGCE - see elsewhere for license details
''

option explicit

#include gfx.bi

'border required to fit standard 4:3 screen at zoom 1
#define BORDER 20

dim shared offset as integer = 0
dim shared windowed as integer = 0
dim shared init_gfx as integer = 0
'defaults are 2x zoom and 640x400 in 8-bit (mode 17)
dim shared zoom as integer = 2
dim shared screenmode as integer = 17
dim shared bordered as integer = 0
dim shared depth as integer = 8

'internal palette for 24-bit mode, with 0-255 RGB instead of 0-63 BGR
dim shared truepal(255) as integer

declare sub debug(s$)

'Note, init is called before the browser is shown, and close is
'called when an RPG is exited, they will usually be run more than
'once. Perhaps there is also call for once-only routines outside
'the main loop?
sub gfx_init
	if init_gfx = 0 then
		if windowed = 0 then
			screen screenmode, depth, 1, 1
		else
			screen screenmode, depth,1
		end if
		screenset 0, 0
		init_gfx = 1
	end if
end sub

sub gfx_close
end sub

sub gfx_showpage(byval raw as ubyte ptr)
'takes a pointer to raw 8-bit data at 320x200
	dim rptr as ubyte ptr
	dim as integer w, h, i, j
	
	screensync
	screenlock
	if depth = 8 then
		dim sptr as ubyte ptr
		sptr = screenptr
		sptr += (offset * 320 * zoom)
		for h = 0 to 200 - 1
			'repeat row zoom times
			for i = 0 to zoom - 1
				'set start of row
				rptr = raw + (320 * h)
				for w = 0 to 320 - 1
					'zoom sptrs for each rptr
					for j = 0 to zoom - 1
						*sptr = *rptr
						sptr += 1
					next
					rptr += 1
				next
			next
		next
	else
		'true colour
		dim xptr as integer ptr
		dim pixel as integer
		xptr = screenptr
		xptr += (offset * 320 * zoom)
		for h = 0 to 200 - 1
			'repeat row zoom times
			for i = 0 to zoom - 1
				'set start of row
				rptr = raw + (320 * h)
				for w = 0 to 320 - 1
					'get colour
					pixel = truepal(*rptr)
					'zoom sptrs for each rptr
					for j = 0 to zoom - 1
						*xptr = pixel
						xptr += 1
					next
					rptr += 1
				next
			next
		next
	end if
	screenunlock
	
end sub

sub gfx_setpal(pal() as integer)
'NOTE: component colour values are 0-63 not 0-255
'Format is BGR, packed within 1 integer, which may not be that
'useful. Should it be 768 bytes instead of 256 ints?
	if depth = 8 then
		palette using pal
	else
		'set truecolour palette with scaled colour values
		dim as integer r, g, b, i
		for i = 0 to 255
			r = pal(i) and &hff
			g = (pal(i) and &hff00) shr 8
			b = (pal(i) and &hff0000) shr 16
			truepal(i) = RGB(r * 4, g * 4, b * 4)
		next
		'This does not update the page "live", like the 8-bit version
		'so fades aren't working, and there's no way to force an
		'update from here at the moment because the screen buffer is not
		'accessible.
	end if
end sub

function gfx_screenshot(fname as string, byval page as integer) as integer
	gfx_screenshot = 0
end function

sub gfx_setwindowed(byval iswindow as integer)
	if iswindow <> 0 then iswindow = 1 'only 1 "true" value
	if iswindow = windowed then exit sub
	
	windowed = iswindow
	
	if init_gfx = 1 then
		dim pal(255) as integer
		if depth = 8 then palette get using pal
		if windowed = 0 then
			screen screenmode, depth, 1, 1
		else
			screen screenmode, depth, 1
		end if
		if depth = 8 then palette using pal		
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
'	zoom (1, 2*, 4), 
'	depth (8*, 24), 
'	border (0*, 1)
'only before gfx has been initialised

	if init_gfx = 0 then
		if opt = "zoom" then
			'default zoom is 2, 1 is the only other valid value
			if value = 1 then
				zoom = 1
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
		end if
		'calculate mode
		if zoom = 1 then
			if depth = 8 then
				if bordered = 1 then
					screenmode = 14
				else 
					screenmode = 13
				end if
			else
				'only bordered is supported in 24-bit it seems
				bordered = 1
				screenmode = 14
			end if
		else
			if bordered = 1 then
				screenmode = 18
			else
				screenmode = 17
			end if
		end if
		'calculate offset
		if bordered = 1 then offset = BORDER * zoom
	end if
	
end sub

'------------- IO Functions --------------
sub io_init
	setmouse(0, 0, 0) 'hide mouse
end sub

function io_keypressed(byval scancode as integer)
'the contract of this function is basically the same as multikey
'in this case it's just a wrapper, but multikey only works with
'the built-in gfxlib
	io_keypressed = multikey(scancode)
end function

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
	dim as integer dmx, dmy
	getmouse(dmx, dmy, mwheel, mbuttons)
	mx = dmx \ zoom
	my = (dmy \ zoom) - offset
end sub

sub io_setmouse(byval x as integer, byval y as integer)
	setmouse(x * zoom, y * zoom + offset)
end sub

sub io_mouserect(byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
	'nothing to do
end sub

function io_readjoy(joybuf() as integer, byval joynum as integer) as integer
	'don't know
	io_readjoy = 0
end function