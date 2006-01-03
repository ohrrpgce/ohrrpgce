'' 
'' gfx_alleg.bas - External graphics functions implemented in Allegro
''
'' part of OHRRPGCE - see elsewhere for license details
''

option explicit

#include "allegro.bi"
#include "gfx.bi"

dim shared init_gfx = 0
dim shared screenbuf as BITMAP ptr = null

'Scancodes are different, need a translation
dim shared keytrans(0 to 127) as integer => { _
	0, KEY_ESC, KEY_1, KEY_2, KEY_3, KEY_4, KEY_5, KEY_6, _
	KEY_7, KEY_8, KEY_9, KEY_0, KEY_MINUS, KEY_EQUALS, KEY_BACKSPACE, KEY_TAB, _
	KEY_Q, KEY_W, KEY_E, KEY_R, KEY_T, KEY_Y, KEY_U, KEY_I, _
	KEY_O, KEY_P, KEY_OPENBRACE, KEY_CLOSEBRACE, KEY_ENTER, KEY_LCONTROL, KEY_A, KEY_S, _
	KEY_D, KEY_F, KEY_G, KEY_H, KEY_J, KEY_K, KEY_L, KEY_COLON, _
	KEY_QUOTE, KEY_TILDE, KEY_LSHIFT, KEY_BACKSLASH, KEY_Z, KEY_X, KEY_C, KEY_V, _
	KEY_B, KEY_N, KEY_M, KEY_COMMA, KEY_STOP, KEY_SLASH, KEY_RSHIFT, KEY_ASTERISK, _
	KEY_ALT, KEY_SPACE, KEY_CAPSLOCK, KEY_F1, KEY_F2, KEY_F3, KEY_F4, KEY_F5, _
	KEY_F6, KEY_F7, KEY_F8, KEY_F9, KEY_F10, KEY_NUMLOCK, KEY_SCRLOCK, KEY_HOME, _
	KEY_UP, KEY_PGUP, 0, KEY_LEFT, 0, KEY_RIGHT, KEY_PLUS_PAD, KEY_END, _
	KEY_DOWN, KEY_PGDN, KEY_INSERT, KEY_DEL, 0, 0, 0, KEY_F11, _
	KEY_F12, 0, 0, 0, 0, 0, 0, 0, _
	0, 0, 0, 0, 0, 0, 0, 0, _
	0, 0, 0, 0, 0, 0, 0, 0, _
	0, 0, 0, 0, 0, 0, 0, 0, _
	0, 0, 0, 0, 0, KEY_LWIN, KEY_RWIN, KEY_MENU _
}

'Note, init is called before the browser is shown, and close is
'called when an RPG is exited, they will usually be run more than
'once. Perhaps there is also call for once-only routines outside
'the main loop?
sub gfx_init
	if init_gfx = 0 then
		allegro_init()
	
		set_color_depth(8)
		set_gfx_mode(GFX_AUTODETECT, 640, 480, 0, 0)
		clear_bitmap(screen)
		
		install_keyboard
		
		init_gfx = 1
	end if
end sub

sub gfx_close
	if screenbuf <> null then
		destroy_bitmap(screenbuf)
		screenbuf = null
	end if
end sub

sub gfx_showpage(byval raw as ubyte ptr)
'takes a pointer to raw 8-bit data at 320x200
	dim as integer x, y

	if screenbuf = null then
		'only create this once, to save a bit of time	
		screenbuf = create_bitmap(320, 200)
	end if
	
	for y = 0 to 199
		for x = 0 to 319
			putpixel8(screenbuf, x, y, *raw)
			raw += 1
		next
	next
	
	stretch_blit(screenbuf, screen, 0, 0, 320, 200, 0, 40, 640, 400)
	
end sub

sub gfx_setpal(pal() as integer)
'NOTE: component colour values are 0-63 not 0-255
'Format is BGR, packed within 1 integer, which may not be that
'useful. Should it be 768 bytes instead of 256 ints?
	dim alpal(255) as RGB
	dim as integer i
	
	for i = 0 to 255
		alpal(i).r = pal(i) and &hff
		alpal(i).g = (pal(i) and &hff00) shr 8
		alpal(i).b = (pal(i) and &hff0000) shr 16
	next
	
	set_palette(@alpal(0))
end sub

function io_keypressed(byval scancode as integer)
	io_keypressed = key(keytrans(scancode))
end function
