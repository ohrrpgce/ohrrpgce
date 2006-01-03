'' 
'' gfx_fb.bas - External graphics functions implemented in FB's
''				built-in gfxlib.
''
'' part of OHRRPGCE - see elsewhere for license details
''

option explicit

#include gfx.bi

'Note, init is called before the browser is shown, and close is
'called when an RPG is exited, they will usually be run more than
'once. Perhaps there is also call for once-only routines outside
'the main loop?
sub gfx_init
	'screen 13, , , 1 for fullscreen
	screen 13, ,1
	screenset 0, 0
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

function io_keypressed(byval scancode as integer)
'the contract of this function is basically the same as multikey
'in this case it's just a wrapper, but multikey only works with
'the built-in gfxlib
	io_keypressed = multikey(scancode)
end function
