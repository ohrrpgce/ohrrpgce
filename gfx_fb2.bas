'' 
'' gfx_fb2.bas - External graphics functions implemented in FB's
''				 built-in gfxlib. Res-doubled version, 640x400.
''
'' part of OHRRPGCE - see elsewhere for license details
''

option explicit

#include gfx.bi

dim shared offset as integer = 0
dim shared windowed as integer = 0
dim shared init_gfx as integer = 0

declare sub debug(s$)

'Note, init is called before the browser is shown, and close is
'called when an RPG is exited, they will usually be run more than
'once. Perhaps there is also call for once-only routines outside
'the main loop?
sub gfx_init
	'mode 17 is 640x400, 18 is 640x480
	if init_gfx = 0 then
		if windowed = 0 then
			screen 17, , 1, 1
		else
			screen 17, ,1
		end if
		screenset 0, 0
		init_gfx = 1
	end if
end sub

sub gfx_close
end sub

sub gfx_showpage(byval raw as ubyte ptr)
'takes a pointer to raw 8-bit data at 320x200
	dim sptr as ubyte ptr
	dim rptr as ubyte ptr
	dim as integer w, h, i
	
	screenlock
	sptr = screenptr
	for h = 0 to 200 - 1
		'repeat row twice
		for i = 0 to 1
			'set start of row
			rptr = raw + (320 * h)
			for w = 0 to 320 - 1
				'2 sptrs for each rptr
				*sptr = *rptr
				sptr += 1
				*sptr = *rptr
				sptr += 1
				rptr += 1
			next
		next
	next
	screenunlock
	
end sub

sub gfx_setpal(pal() as integer)
'NOTE: component colour values are 0-63 not 0-255
'Format is BGR, packed within 1 integer, which may not be that
'useful. Should it be 768 bytes instead of 256 ints?
	palette using pal
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
		palette get using pal
		if windowed = 0 then
			screen 17, , 1, 1
		else
			screen 17, , 1
		end if
		palette using pal		
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
	mx = dmx \ 2
	my = (dmy \ 2) - offset
end sub

sub io_setmouse(byval x as integer, byval y as integer)
	setmouse(x * 2, y * 2 + offset)
end sub

sub io_mouserect(byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
	'nothing to do
end sub

function io_readjoy(joybuf() as integer, byval joynum as integer) as integer
	'don't know
	io_readjoy = 0
end function