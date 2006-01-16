'' 
'' gfx_fb.bas - External graphics functions implemented in FB's
''				built-in gfxlib.
''
'' part of OHRRPGCE - see elsewhere for license details
''

option explicit

#include gfx.bi

dim shared windowed as integer = 1
dim shared init_gfx as integer = 0

'Note, init is called before the browser is shown, and close is
'called when an RPG is exited, they will usually be run more than
'once. Perhaps there is also call for once-only routines outside
'the main loop?
sub gfx_init
	'screen 13, , , 1 for fullscreen
	if init_gfx = 0 then
		if windowed = 0 then
			screen 13, , 1, 1
		else
			screen 13, ,1
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
	dim i as integer
	
	screenlock
	sptr = screenptr
	for i = 0 to (320 * 200) - 1
		sptr[i] = raw[i]
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
			screen 13, , 1, 1
		else
			screen 13, , 1
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
	dim as integer mx, my, mw, mb
	getmouse(mx, my, mw, mb)
	if (mb = -1) then	'no mouse if button = -1
		io_enablemouse = -1
		exit function
	end if
	io_enablemouse = 0
end function

sub io_getmouse(mx as integer, my as integer, mwheel as integer, mbuttons as integer)
	getmouse(mx, my, mwheel, mbuttons)
end sub

sub io_setmouse(byval x as integer, byval y as integer)
	setmouse(x, y)
end sub

sub io_mouserect(byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
	'nothing to do
end sub

function io_readjoy(joybuf() as integer, byval joynum as integer) as integer
	'don't know
	io_readjoy = 0
end function