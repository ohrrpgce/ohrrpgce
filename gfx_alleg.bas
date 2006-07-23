'' 
'' gfx_alleg.bas - External graphics functions implemented in Allegro
''
'' part of OHRRPGCE - see elsewhere for license details
''

option explicit

#include "allegro.bi"
#include "gfx.bi"
#include "scancodes.bi"

declare sub debug(s$)

dim shared init_gfx as integer = 0
dim shared screenbuf as BITMAP ptr = null

dim shared mouse_hidden as integer = 0
dim shared offset as integer = 0
dim shared windowed as integer = 0
dim shared alpal(255) as RGB

'Translate an OHR scancode to an Allegro one
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
	scPageup, scPagedown, scLeft, scRight, scUp, scDown, scSlash, scNumpadAsterix, _
	scMinus, scPlus, scPeriod, scEnter, 0, 0, 0, 0, _
	0, 0, 0, scAtSign, scCircumflex, 0, 0, scLeftShift, _
	scRightShift, scCtrl, scCtrl, scAlt, scAlt, scLeftWinLogo, scRightWinLogo, scContext, _
	scScrollLock, scNumlock, scCapslock, 0, 0, 0, 0, 0, _
	0, 0, 0, 0, 0, 0, 0, 0 _
}

'Note, init is called before the browser is shown, and close is
'called when an RPG is exited, they will usually be run more than
'once. Perhaps there is also call for once-only routines outside
'the main loop?
sub gfx_init
	if init_gfx = 0 then
		allegro_init()
	
		set_color_depth(8)
		if windowed <> 0 then
			set_gfx_mode(GFX_AUTODETECT_WINDOWED, 640, 400, 0, 0)
			offset = 0
		else
			set_gfx_mode(GFX_AUTODETECT_FULLSCREEN, 640, 480, 0, 0)
			offset = 40
		end if
		clear_bitmap(screen)
		
		install_keyboard
		install_mouse
		
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
#if __FB_VERSION__ <= "0.16"
			_putpixel(screenbuf, x, y, *raw)
#else
			putpixel8(screenbuf, x, y, *raw)
#endif
			raw += 1
		next
	next
	
	stretch_blit(screenbuf, screen, 0, 0, 320, 200, 0, offset, 640, 400)
	
end sub

sub gfx_setpal(pal() as integer)
'NOTE: component colour values are 0-63 not 0-255
'Format is BGR, packed within 1 integer, which may not be that
'useful. Should it be 768 bytes instead of 256 ints?
	dim as integer i
	
	for i = 0 to 255
		alpal(i).r = pal(i) and &hff
		alpal(i).g = (pal(i) and &hff00) shr 8
		alpal(i).b = (pal(i) and &hff0000) shr 16
	next
	
	set_palette(@alpal(0))
end sub

function gfx_screenshot(fname as string, byval page as integer) as integer
	gfx_screenshot = 0
end function

sub gfx_setwindowed(byval iswindow as integer)
	if iswindow <> 0 then iswindow = 1 'only 1 "true" value
	if iswindow = windowed then exit sub
	
	windowed = iswindow
	
	if init_gfx = 1 then
		if windowed <> 0 then
			set_gfx_mode(GFX_AUTODETECT_WINDOWED, 640, 400, 0, 0)
			offset = 0
		else
			set_gfx_mode(GFX_AUTODETECT_FULLSCREEN, 640, 480, 0, 0)
			offset = 40
		end if
		set_palette(@alpal(0))		
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
	if init_gfx = 1 then
 		set_window_title(strptr(title))	
 	end if
end sub

sub gfx_setoption(opt as string, byval value as integer = -1)
'handle command-line options in a generic way, so that they
'can be ignored or supported as the library permits.
end sub

'------------- IO Functions --------------
sub io_init
' 	'mostly handled above
' 	if mouse_hidden = 0 then
' 		scare_mouse() 'hide mouse
' 		mouse_hidden = 1
' 	end if
end sub

sub io_updatekeys(keybd() as integer)
	dim a as integer
	for a = 0 to &h7f
		if key(a) then
			keybd(scantrans(a)) = keybd(scantrans(a)) or 4
		end if
	next
end sub

function io_keypressed(byval sccode as integer)
	io_keypressed = key(keytrans(sccode))
end function

function io_enablemouse() as integer
'returns 0 if mouse okay
' 	if mouse_hidden = 1 then
' 		unscare_mouse()
' 		mouse_hidden = 0
' 	end if
	io_enablemouse = 0
end function

sub io_getmouse(mx as integer, my as integer, mwheel as integer, mbuttons as integer)
	mx = mouse_x \ 2		'allegro screen is double res
	my = (mouse_y \ 2) - offset	'and centred
	mwheel = mouse_z
	mbuttons = mouse_b
end sub

sub io_setmouse(byval x as integer, byval y as integer)
	position_mouse(x * 2, y * 2 + offset)
end sub

sub io_mouserect(byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
'doesn't seem to work fullscreen, no idea why not. Height of mouse cursor?
' 	set_mouse_range(xmin * 2, ymin * 2 + offset, xmax * 2 + 1, ymax * 2 + 1 + offset)
end sub

function io_readjoy(joybuf() as integer, byval joynum as integer) as integer
	'don't know
	io_readjoy = 0
end function

function io_readjoysane(byval joynum as integer, byref button as integer, byref x as integer, byref y as integer) as integer

  'don't know either
	return 0
	
end function
