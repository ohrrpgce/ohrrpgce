''
'' gfx_sdl.bas - External graphics functions implemented in SDL
''
'' part of OHRRPGCE - see LICENCE.txt for GPL
''

option explicit

#include "gfx.bi"
#include once "sdl_common.bi"

DIM SHARED zoom AS INTEGER = 1
DIM SHARED screensurface AS SDL_Surface PTR = NULL
DIM SHARED screenbuffer AS SDL_Surface PTR = NULL
DIM SHARED screenrect AS SDL_Rect
DIM SHARED windowedmode AS INTEGER = -1
DIM SHARED keystate AS Uint8 PTR = NULL
DIM SHARED sdljoystick AS SDL_Joystick PTR = NULL
DIM SHARED sdlpalette(256) AS SDL_Color PTR

SUB gfx_init()
  IF sdl_init_done = 0 THEN
    SDL_Init(SDL_INIT_VIDEO or SDL_INIT_AUDIO)
    sdl_init_done = -1
    screensurface = SDL_SetVideoMode(320 * zoom, 200 * zoom, 0, 0)
    IF screensurface = NULL THEN
      print "Failed to allocate display"
      SYSTEM
    END IF
    windowedmode = -1
    WITH screenrect
      .x = 0
      .y = 0
      .w = 320 * zoom
      .h = 200 * zoom
    END WITH
  END IF
END SUB

SUB gfx_close()
  IF screenbuffer <> NULL THEN SDL_FreeSurface(screenbuffer)
  IF sdljoystick <> NULL THEN SDL_JoystickClose(sdljoystick)
  DIM i AS INTEGER
  FOR i = 0 to 255
    IF sdlpalette(i) = NULL THEN DEALLOCATE(sdlpalette(i))
  NEXT i
  SDL_VideoQuit()
  SDL_Quit()
END SUB

SUB gfx_showpage(byval raw as ubyte ptr)
  'takes a pointer to raw 8-bit data at 320x200
  IF screenbuffer = NULL THEN
    screenbuffer = SDL_CreateRGBSurfaceFrom(raw, 320, 200, 8, 320, &hff000000, &h00ff0000, &h0000ff00, &h000000ff)
    IF screenbuffer = NULL THEN
      print "Failed to allocate screen buffer"
      SYSTEM
    END IF
  END IF
  SDL_SetColors(screenbuffer, sdlpalette(0), 0, 256)
  SDL_BlitSurface(screenbuffer, @screenrect, screensurface, @screenrect)
END SUB

SUB gfx_setpal(pal() as RGBcolor)
  DIM i AS INTEGER
  FOR i = 0 TO 255
    IF sdlpalette(i) = NULL THEN
      sdlpalette(i) = ALLOCATE(SIZEOF(SDL_Color))
    END IF
    sdlpalette(i)->r = pal(i).r
    sdlpalette(i)->g = pal(i).g
    sdlpalette(i)->b = pal(i).b
  NEXT
END SUB

FUNCTION gfx_screenshot(fname as string, byval page as integer) as integer
  'FIXME: what is the purpose of this?
  gfx_screenshot = 0
END FUNCTION

SUB gfx_setwindowed(byval iswindow as integer)
  DIM flags AS Uint32 = 0
  IF iswindow = 0 THEN
    windowedmode = 0
    flags = flags OR SDL_FULLSCREEN
  ELSE
    windowedmode = -1
  END IF
  screensurface = SDL_SetVideoMode(320 * zoom, 200 * zoom, 0, flags)
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
  'ignore all options for now...
END SUB

SUB io_init
  'nothing needed at the moment...
END SUB

SUB io_updatekeys(keybd() as integer)
  DIM a AS INTEGER
  keystate = SDL_GetKeyState(NULL)
  FOR a = 0 TO &h7f
    IF keystate[a] THEN
      keybd(a) = keybd(a) OR 4
    END IF
  NEXT
END SUB

FUNCTION io_keypressed(byval sccode as integer)
  keystate = SDL_GetKeyState(NULL)
  io_keypressed = keystate[sccode]
END FUNCTION

FUNCTION io_enablemouse() as integer
  'FIXME: this is unneeded. It was originally for DOS mouse init voodoo
  io_enablemouse = 0
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
    mbuttons = buttons AND (SDL_BUTTON_LEFT OR SDL_BUTTON_RIGHT OR SDL_BUTTON_MIDDLE)
  END IF
END SUB

SUB io_setmouse(byval x as integer, byval y as integer)
  SDL_WarpMouse x, y
END SUB

SUB io_mouserect(byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
  'FIXME: not implemented yet
END SUB

FUNCTION io_readjoy(joybuf() as integer, byval joynum as integer) as integer
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
  'TODO: this does not belong in the backend at all. It should be a wrapper around io_readjoy in allmodex.bas
  RETURN 0
END FUNCTION

