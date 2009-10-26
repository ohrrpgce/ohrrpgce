'' 
'' gfx_alleg.bas - External graphics functions implemented in Allegro
''
'' part of OHRRPGCE - see elsewhere for license details
''

option explicit

#include "gfx.bi"
#undef Font
#include "allegro.bi"
#include "scancodes.bi"

declare sub debug(s$)

dim shared init_gfx as integer = 0
dim shared screenbuf as BITMAP ptr = null

dim shared mouse_hidden as integer = 0
dim shared baroffset as integer = 0
dim shared windowed as integer = 1
dim shared alpal(255) as RGB

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
			baroffset = 0
		else
			set_gfx_mode(GFX_AUTODETECT_FULLSCREEN, 640, 480, 0, 0)
			baroffset = 40
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

sub gfx_showpage(byval raw as ubyte ptr, byval w as integer, byval h as integer)
'takes a pointer to raw 8-bit data at 320x200 (w, h ignored)
	dim as integer x, y

	if screenbuf = null then
		'only create this once, to save a bit of time	
		screenbuf = create_bitmap(320, 200)
	end if
	
	for y = 0 to 199
		for x = 0 to 319
			#IFDEF putpixel8
			putpixel8(screenbuf, x, y, *raw)
			#ELSE
			putpixel(screenbuf, x, y, *raw)
			#ENDIF
			raw += 1
		next
	next
	
	stretch_blit(screenbuf, screen, 0, 0, 320, 200, 0, baroffset, 640, 400)
	
end sub

sub gfx_setpal(byval pal as RGBcolor ptr)
'In 8 bit colour depth, allegro uses 6 bit colour components in the palette
	dim as integer i
	
	for i = 0 to 255
		alpal(i).r = pal[i].r \ 4
		alpal(i).g = pal[i].g \ 4
		alpal(i).b = pal[i].b \ 4
	next
	
	set_palette(@alpal(0))
end sub

function gfx_screenshot(fname as zstring ptr) as integer
	gfx_screenshot = 0
end function

sub gfx_setwindowed(byval iswindow as integer)
	if iswindow <> 0 then iswindow = 1 'only 1 "true" value
	if iswindow = windowed then exit sub
	
	windowed = iswindow
	
	if init_gfx = 1 then
		if windowed <> 0 then
			set_gfx_mode(GFX_AUTODETECT_WINDOWED, 640, 400, 0, 0)
			baroffset = 0
		else
			set_gfx_mode(GFX_AUTODETECT_FULLSCREEN, 640, 480, 0, 0)
			baroffset = 40
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

sub gfx_windowtitle(title as zstring ptr)
	if init_gfx = 1 then
 		set_window_title(title)
 	end if
end sub

sub gfx_setoption(opt as zstring ptr, byval value as integer = -1)
'handle command-line options in a generic way, so that they
'can be ignored or supported as the library permits.
end sub

function gfx_describe_options() as zstring ptr
'No options are supported by this backend
 return @""
end function

'------------- IO Functions --------------
sub io_init
' 	'mostly handled above
' 	if mouse_hidden = 0 then
' 		scare_mouse() 'hide mouse
' 		mouse_hidden = 1
' 	end if
end sub

sub io_pollkeyevents()
	'not needed by this backend
end sub

sub io_updatekeys(byval keybd as integer ptr)
	dim a as integer
	for a = 0 to &h7f
		if key(a) then
			keybd[scantrans(a)] = keybd[scantrans(a)] or 8
		end if
	next
end sub

sub io_setmousevisibility(byval visible as integer)
'who know why this check is here
 	if visible <> 0 and mouse_hidden = 1 then
 		unscare_mouse()
 		mouse_hidden = 0
 	end if
 	if visible = 0 and mouse_hidden = 0 then
 		scare_mouse()
 		mouse_hidden = 1
 	end if
end sub

sub io_getmouse(mx as integer, my as integer, mwheel as integer, mbuttons as integer)
	mx = mouse_x \ 2		'allegro screen is double res
	my = (mouse_y \ 2) - baroffset	'and centred
	mwheel = mouse_z
	mbuttons = mouse_b
end sub

sub io_setmouse(byval x as integer, byval y as integer)
	position_mouse(x * 2, y * 2 + baroffset)
end sub

'sub io_mouserect(byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
'doesn't seem to work fullscreen, no idea why not. Height of mouse cursor?
' 	set_mouse_range(xmin * 2, ymin * 2 + baroffset, xmax * 2 + 1, ymax * 2 + 1 + baroffset)
'end sub

function io_readjoysane(byval joynum as integer, byref button as integer, byref x as integer, byref y as integer) as integer
	'don't know
	return 0
end function
