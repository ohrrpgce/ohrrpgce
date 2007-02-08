''
'' gfx_sdl.bas - External graphics functions implemented in SDL
''
'' part of OHRRPGCE - see LICENCE.txt for GPL
''

option explicit

#include "crt.bi"
#include "gfx.bi"
#include once "sdl_common.bi"

DECLARE SUB gfx_sdl_set_screen_mode()
DECLARE SUB gfx_sdl_update_screen()

DIM SHARED zoom AS INTEGER = 2
DIM SHARED screensurface AS SDL_Surface PTR = NULL
DIM SHARED screenbuffer AS SDL_Surface PTR = NULL
DIM SHARED windowedmode AS INTEGER = -1
DIM SHARED keystate AS Uint8 PTR = NULL
DIM SHARED sdljoystick AS SDL_Joystick PTR = NULL
DIM SHARED sdlpalette(0 TO 255) AS SDL_Color
DIM SHARED source_rect AS SDL_Rect
DIM SHARED dest_rect AS SDL_Rect

'Translate an OHR scancode to an SDL one 
'FIXME: keytrans is useless and can be removed once io_keypressed is removed
DIM SHARED keytrans(0 to 127) AS INTEGER => { _
	0, SDLK_ESCAPE, SDLK_1, SDLK_2, SDLK_3, SDLK_4, SDLK_5, SDLK_6, _
	SDLK_7, SDLK_8, SDLK_9, SDLK_0, SDLK_MINUS, SDLK_EQUALS, SDLK_BACKSPACE, SDLK_TAB, _
	SDLK_Q, SDLK_W, SDLK_E, SDLK_R, SDLK_T, SDLK_Y, SDLK_U, SDLK_I, _
	SDLK_O, SDLK_P, SDLK_LEFTBRACKET, SDLK_RIGHTBRACKET, SDLK_RETURN, SDLK_LCTRL, SDLK_A, SDLK_S, _
	SDLK_D, SDLK_F, SDLK_G, SDLK_H, SDLK_J, SDLK_K, SDLK_L, SDLK_SEMICOLON, _
	SDLK_QUOTE, SDLK_BACKQUOTE, SDLK_LSHIFT, SDLK_BACKSLASH, SDLK_Z, SDLK_X, SDLK_C, SDLK_V, _
	SDLK_B, SDLK_N, SDLK_M, SDLK_COMMA, SDLK_PERIOD, SDLK_SLASH, SDLK_RSHIFT, SDLK_ASTERISK, _
	SDLK_LALT, SDLK_SPACE, SDLK_CAPSLOCK, SDLK_F1, SDLK_F2, SDLK_F3, SDLK_F4, SDLK_F5, _
	SDLK_F6, SDLK_F7, SDLK_F8, SDLK_F9, SDLK_F10, SDLK_NUMLOCK, SDLK_SCROLLOCK, SDLK_HOME, _
	SDLK_UP, SDLK_PAGEUP, 0, SDLK_LEFT, 0, SDLK_RIGHT, SDLK_KP_PLUS, SDLK_END, _
	SDLK_DOWN, SDLK_PAGEDOWN, SDLK_INSERT, SDLK_DELETE, 0, 0, 0, SDLK_F11, _
	SDLK_F12, 0, 0, 0, 0, 0, 0, 0, _
	0, 0, 0, 0, 0, 0, 0, 0, _
	0, 0, 0, 0, 0, 0, 0, 0, _
	0, 0, 0, 0, 0, 0, 0, 0, _
	0, 0, 0, 0, 0, SDLK_UNKNOWN, SDLK_UNKNOWN, SDLK_UNKNOWN _
}

'Translate SDL scancodes into a OHR scancodes
DIM SHARED scantrans(0 to 322) AS INTEGER
scantrans(SDLK_UNKNOWN) = 0
scantrans(SDLK_BACKSPACE) = 14
scantrans(SDLK_TAB) = 15
scantrans(SDLK_CLEAR) = 0
scantrans(SDLK_RETURN) = 28
scantrans(SDLK_PAUSE) = 0
scantrans(SDLK_ESCAPE) = 1
scantrans(SDLK_SPACE) = 57
scantrans(SDLK_EXCLAIM) = 2
scantrans(SDLK_QUOTEDBL) = 40
scantrans(SDLK_HASH) = 4
scantrans(SDLK_DOLLAR) = 5
scantrans(SDLK_AMPERSAND) = 8
scantrans(SDLK_QUOTE) = 40
scantrans(SDLK_LEFTPAREN) = 10
scantrans(SDLK_RIGHTPAREN) = 11
scantrans(SDLK_ASTERISK) = 9
scantrans(SDLK_PLUS) = 13
scantrans(SDLK_COMMA) = 51
scantrans(SDLK_MINUS) = 12
scantrans(SDLK_PERIOD) = 52
scantrans(SDLK_SLASH) = 53
scantrans(SDLK_0) = 11
scantrans(SDLK_1) = 2
scantrans(SDLK_2) = 3
scantrans(SDLK_3) = 4
scantrans(SDLK_4) = 5
scantrans(SDLK_5) = 6
scantrans(SDLK_6) = 7
scantrans(SDLK_7) = 8
scantrans(SDLK_8) = 9
scantrans(SDLK_9) = 10
scantrans(SDLK_COLON) = 39
scantrans(SDLK_SEMICOLON) = 39
scantrans(SDLK_LESS) = 51
scantrans(SDLK_EQUALS) = 13
scantrans(SDLK_GREATER) = 52
scantrans(SDLK_QUESTION) = 53
scantrans(SDLK_AT) = 3
scantrans(SDLK_LEFTBRACKET) = 26
scantrans(SDLK_BACKSLASH) = 43
scantrans(SDLK_RIGHTBRACKET) = 27
scantrans(SDLK_CARET) = 7
scantrans(SDLK_UNDERSCORE) = 12
scantrans(SDLK_BACKQUOTE) = 41
scantrans(SDLK_a) = 30
scantrans(SDLK_b) = 48
scantrans(SDLK_c) = 46
scantrans(SDLK_d) = 32
scantrans(SDLK_e) = 18
scantrans(SDLK_f) = 33
scantrans(SDLK_g) = 34
scantrans(SDLK_h) = 35
scantrans(SDLK_i) = 23
scantrans(SDLK_j) = 36
scantrans(SDLK_k) = 37
scantrans(SDLK_l) = 38
scantrans(SDLK_m) = 50
scantrans(SDLK_n) = 49
scantrans(SDLK_o) = 24
scantrans(SDLK_p) = 25
scantrans(SDLK_q) = 16
scantrans(SDLK_r) = 19
scantrans(SDLK_s) = 31
scantrans(SDLK_t) = 20
scantrans(SDLK_u) = 22
scantrans(SDLK_v) = 47
scantrans(SDLK_w) = 17
scantrans(SDLK_x) = 45
scantrans(SDLK_y) = 21
scantrans(SDLK_z) = 44
scantrans(SDLK_DELETE) = 83
scantrans(SDLK_KP0) = 82
scantrans(SDLK_KP1) = 79
scantrans(SDLK_KP2) = 80
scantrans(SDLK_KP3) = 81
scantrans(SDLK_KP4) = 75
scantrans(SDLK_KP5) = 76
scantrans(SDLK_KP6) = 77
scantrans(SDLK_KP7) = 71
scantrans(SDLK_KP8) = 72
scantrans(SDLK_KP9) = 73
scantrans(SDLK_KP_PERIOD) = 83
scantrans(SDLK_KP_DIVIDE) = 83
scantrans(SDLK_KP_MULTIPLY) = 55
scantrans(SDLK_KP_MINUS) = 74
scantrans(SDLK_KP_PLUS) = 78
scantrans(SDLK_KP_ENTER) = 28
scantrans(SDLK_KP_EQUALS) = 13
scantrans(SDLK_UP) = 72
scantrans(SDLK_DOWN) = 80
scantrans(SDLK_RIGHT) = 77
scantrans(SDLK_LEFT) = 75
scantrans(SDLK_INSERT) = 82
scantrans(SDLK_HOME) = 71
scantrans(SDLK_END) = 79
scantrans(SDLK_PAGEUP) = 73
scantrans(SDLK_PAGEDOWN) = 81
scantrans(SDLK_F1) = 59
scantrans(SDLK_F2) = 60
scantrans(SDLK_F3) = 61
scantrans(SDLK_F4) = 62
scantrans(SDLK_F5) = 63
scantrans(SDLK_F6) = 64
scantrans(SDLK_F7) = 65
scantrans(SDLK_F8) = 66
scantrans(SDLK_F9) = 67
scantrans(SDLK_F10) = 68
scantrans(SDLK_F11) = 87
scantrans(SDLK_F12) = 89
scantrans(SDLK_F13) = 0
scantrans(SDLK_F14) = 0
scantrans(SDLK_F15) = 0
scantrans(SDLK_NUMLOCK) = 69
scantrans(SDLK_CAPSLOCK) = 58
scantrans(SDLK_SCROLLOCK) = 70
scantrans(SDLK_RSHIFT) = 54
scantrans(SDLK_LSHIFT) = 42
scantrans(SDLK_RCTRL) = 29
scantrans(SDLK_LCTRL) = 29
scantrans(SDLK_RALT) = 56
scantrans(SDLK_LALT) = 56
scantrans(SDLK_RMETA) = 0
scantrans(SDLK_LMETA) = 0
scantrans(SDLK_LSUPER) = 0
scantrans(SDLK_RSUPER) = 0
scantrans(SDLK_MODE) = 0
scantrans(SDLK_COMPOSE) = 0
scantrans(SDLK_HELP) = 0
scantrans(SDLK_PRINT) = 0
scantrans(SDLK_SYSREQ) = 0
scantrans(SDLK_BREAK) = 0
scantrans(SDLK_MENU) = 0
scantrans(SDLK_POWER) = 0
scantrans(SDLK_EURO) = 0
scantrans(SDLK_UNDO) = 0

SUB gfx_init()
  IF sdl_init_done = 0 THEN
    SDL_Init(SDL_INIT_VIDEO or SDL_INIT_AUDIO)
    sdl_init_done = -1
    WITH source_rect
      .x = 0
      .y = 0
      .w = 320
      .h = 200
    END WITH
    gfx_sdl_set_screen_mode()
  END IF
END SUB

SUB gfx_sdl_set_screen_mode()
  DIM flags AS Uint32 = 0
  IF windowedmode = 0 THEN
    flags = flags OR SDL_FULLSCREEN
    SDL_ShowCursor(0)
  END IF
  screensurface = SDL_SetVideoMode(320 * zoom, 200 * zoom, 0, flags)
  IF screensurface = NULL THEN
    print "Failed to allocate display"
    SYSTEM
  END IF
  WITH dest_rect
    .x = 0
    .y = 0
    .w = 320 * zoom
    .h = 200 * zoom
  END WITH
END SUB

SUB gfx_close()
  IF screenbuffer <> NULL THEN SDL_FreeSurface(screenbuffer)
  IF sdljoystick <> NULL THEN SDL_JoystickClose(sdljoystick)
  DIM i AS INTEGER
  SDL_VideoQuit()
  SDL_Quit()
END SUB

SUB gfx_showpage(byval raw as ubyte ptr)
  'takes a pointer to raw 8-bit data at 320x200
  IF screenbuffer = NULL THEN
    screenbuffer = SDL_CreateRGBSurfaceFrom(raw, 320 * zoom, 200 * zoom, 8, 320, &hff000000, &h00ff0000, &h0000ff00, &h000000ff)
    IF screenbuffer = NULL THEN
      print "Failed to allocate screen buffer " & 320 * zoom & "x" & 200 * zoom
      SYSTEM
    END IF
  END IF
  gfx_sdl_update_screen()
END SUB

SUB gfx_sdl_update_screen()
  IF screenbuffer <> NULL and screensurface <> NULL THEN
    SDL_SetColors(screenbuffer, @sdlpalette(0), 0, 256)
    SDL_BlitSurface(screenbuffer, @source_rect, screensurface, @dest_rect)
    IF zoom > 1 THEN
      DIM AS INTEGER x, y, b, z, pitch, bpp, pixels
      DIM pixelptr AS ubyte PTR
      pitch = screensurface->pitch
      bpp = screensurface->format->BytesPerPixel
      DIM zoombuffer(pitch) AS ubyte
      SDL_LockSurface(screensurface)
      pixelptr = screensurface->pixels
      FOR y = 199 TO 0 STEP -1
       FOR x = 0 TO 319
         FOR z = 0 TO zoom - 1
           FOR b = 0 TO bpp - 1
             zoombuffer((x * zoom + z) * bpp + b) = pixelptr[y * pitch + (x * bpp + b)]
           NEXT b
         NEXT z
       NEXT x
       FOR z = 0 TO zoom - 1
         memcpy pixelptr + (y * zoom + z) * pitch, @zoombuffer(0), pitch
       NEXT z
      NEXT y
      SDL_UnlockSurface(screensurface)
    END IF
    SDL_Flip(screensurface)
    SDL_PumpEvents()
  END IF
END SUB

SUB gfx_setpal(pal() as RGBcolor)
  DIM i AS INTEGER
  FOR i = 0 TO 255
    sdlpalette(i).r = pal(i).r
    sdlpalette(i).g = pal(i).g
    sdlpalette(i).b = pal(i).b
  NEXT
  gfx_sdl_update_screen()
END SUB

FUNCTION gfx_screenshot(fname as string, byval page as integer) as integer
  'FIXME: what is the purpose of this?
  gfx_screenshot = 0
END FUNCTION

SUB gfx_setwindowed(byval iswindow as integer)
  IF iswindow = 0 THEN
    windowedmode = 0
  ELSE
    windowedmode = -1
  END IF
  gfx_sdl_set_screen_mode()
END SUB

SUB gfx_togglewindowed()
  gfx_setwindowed(windowedmode XOR -1)
END SUB

SUB gfx_windowtitle(title as string)
  IF sdl_init_done then
    SDL_WM_SetCaption(strptr(title), strptr(title))	
  END IF
END SUB

SUB gfx_setoption(opt as string, byval value as integer = -1)
  IF opt = "zoom" THEN
    IF value >= 1 AND value <= 4 THEN
      zoom = value
      IF sdl_init_done THEN
        gfx_sdl_set_screen_mode()
      END IF
    END IF
  END IF
END SUB

SUB io_init
  'nothing needed at the moment...
END SUB

SUB io_updatekeys(keybd() as integer)
  DIM a AS INTEGER
  keystate = SDL_GetKeyState(NULL)
  FOR a = 0 TO 322
    IF keystate[a] THEN
      'print "OHRkey=" & scantrans(a) & " SDLkey=" & a & " " & *SDL_GetKeyName(a)
      IF scantrans(a) THEN
        keybd(scantrans(a)) = keybd(scantrans(a)) OR 4
      END IF
    END IF
  NEXT
END SUB

FUNCTION io_keypressed(byval sccode as integer)
  'FIXME: this is not called anywhere. remove from all backends
  keystate = SDL_GetKeyState(NULL)
  io_keypressed = keystate[keytrans(sccode)]
END FUNCTION

FUNCTION io_enablemouse() as integer
  'FIXME: this is unneeded. It was originally for DOS mouse init voodoo
  io_enablemouse = 0
  SDL_ShowCursor(0)
END FUNCTION

SUB io_getmouse(mx as integer, my as integer, mwheel as integer, mbuttons as integer)
  DIM x AS INTEGER
  DIM y AS INTEGER
  DIM buttons AS Uint8
  buttons = SDL_GetMouseState(@x, @y)
	
  IF x < 0 THEN
    'mouse is outside window
    mx = -1
    my = -1
  ELSE
    mx = x \ zoom
    my = y \ zoom
    mbuttons = 0
    'FIXME: why can't I get the right and middle buttons correct? they seem inverted no matter what I do...
    IF SDL_BUTTON_LEFT AND buttons THEN mbuttons = mbuttons OR 1
    IF SDL_BUTTON_RIGHT AND buttons THEN mbuttons = mbuttons OR 2
    IF SDL_BUTTON_MIDDLE AND buttons THEN mbuttons = mbuttons OR 4
  END IF
END SUB

SUB io_setmouse(byval x as integer, byval y as integer)
  SDL_WarpMouse x, y
END SUB

SUB io_mouserect(byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
  'FIXME: not implemented yet
END SUB

FUNCTION io_readjoy(joybuf() as integer, byval joynum as integer) as integer
  'FIXME: this could be removed from all backends, and replaced with a backcompat wrapper around io_readjoysane in allmodex.bas
  'FIXME: only bothers to support the first joystick
  IF SDL_NumJoysticks() = 0 THEN RETURN 0
  IF sdljoystick = NULL THEN
    sdljoystick = SDL_JoystickOpen(0)
  END IF
  SDL_JoystickUpdate()
  joybuf(0) = SDL_JoystickGetAxis(sdljoystick, 0)
  joybuf(1) = SDL_JoystickGetAxis(sdljoystick, 1)
  joybuf(2) = SDL_JoystickGetButton(sdljoystick, 0)
  joybuf(3) = SDL_JoystickGetButton(sdljoystick, 1)
  RETURN 1
END FUNCTION

FUNCTION io_readjoysane(byval joynum as integer, byref button as integer, byref x as integer, byref y as integer) as integer
  'FIXME: yetmore.bas should NOT be calling this directly! allmodex.bas need to wrap this like it does all other backend functions
  'FIXME: only bothers to support the first joystick
  IF SDL_NumJoysticks() = 0 THEN RETURN 0
  IF sdljoystick = NULL THEN
    sdljoystick = SDL_JoystickOpen(0)
  END IF
  button = 0
  IF SDL_JoystickGetButton(sdljoystick, 0) THEN button = button AND 1
  IF SDL_JoystickGetButton(sdljoystick, 1) THEN button = button AND 2
  x = SDL_JoystickGetAxis(sdljoystick, 0)
  y = SDL_JoystickGetAxis(sdljoystick, 1)
END FUNCTION

