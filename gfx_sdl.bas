''
'' gfx_sdl.bas - External graphics functions implemented in SDL
''
'' Part of the OHRRPGCE - See LICENSE.txt for GNU GPL License details and disclaimer of liability
''

#ifdef LANG_DEPRECATED
 #define __langtok #lang
 __langtok "deprecated"
 OPTION STATIC
 OPTION EXPLICIT
#endif

#include "config.bi"
#include "crt.bi"
#include "gfx.bi"
#include "gfx_newRenderPlan.bi"
#include "common.bi"
#include "scancodes.bi"
'#define NEED_SDL_GETENV
#include "SDL\SDL.bi"
/'
#ifdef __FB_WIN32__
'to load the window from resource file
include_windows_bi()
#endif
'/

'Not extern C
EXTERN running_as_slave AS INTEGER

EXTERN "C"

#IFDEF __FB_ANDROID__
'This function shows/hides the sdl virtual gamepad
declare sub SDL_ANDROID_SetScreenKeyboardShown (byval shown as integer)
'This function toggles the display of the android virtual keyboard
declare function SDL_ANDROID_ToggleScreenKeyboardWithoutTextInput() as integer 'always returns 1
declare function SDL_ANDROID_IsScreenKeyboardShown() as bool
#ENDIF

'why is this missing from crt.bi?
DECLARE FUNCTION putenv (byval as zstring ptr) as integer

'DECLARE FUNCTION SDL_putenv cdecl alias "SDL_putenv" (byval variable as zstring ptr) as integer
'DECLARE FUNCTION SDL_getenv cdecl alias "SDL_getenv" (byval name as zstring ptr) as zstring ptr

DECLARE FUNCTION gfx_sdl_set_screen_mode(byval bitdepth as integer = 0) as integer
DECLARE SUB gfx_sdl_set_zoom(byval value as integer)
DECLARE SUB gfx_sdl_8bit_update_screen()
DECLARE SUB update_state()
DECLARE FUNCTION update_mouse() as integer
DECLARE SUB set_forced_mouse_clipping(byval newvalue as integer)
DECLARE SUB internal_set_mouserect(byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)

#IFDEF __FB_DARWIN__

'--These wrapper functions in mac/SDLMain.m call various Cocoa methods
DECLARE SUB sdlCocoaHide()
DECLARE SUB sdlCocoaHideOthers()
DECLARE SUB sdlCocoaMinimise()

#ENDIF

DIM SHARED zoom AS INTEGER = 2
DIM SHARED zoom_has_been_changed AS INTEGER = NO
DIM SHARED remember_zoom AS INTEGER = -1   'We may change the zoom when fullscreening, so remember it
DIM SHARED smooth AS INTEGER = 0
DIM SHARED screensurface AS SDL_Surface PTR = NULL
DIM SHARED screenbuffer AS SDL_Surface PTR = NULL
DIM SHARED windowedmode AS BOOL = YES
DIM SHARED resizable AS INTEGER = NO
DIM SHARED resizerequested AS INTEGER = NO
DIM SHARED resizerequest AS XYPair
DIM SHARED remember_windowtitle AS STRING
DIM SHARED rememmvis AS INTEGER = 1
DIM SHARED keystate AS Uint8 PTR = NULL
DIM SHARED joystickhandles(7) AS SDL_Joystick PTR
DIM SHARED sdlpalette(0 TO 255) AS SDL_Color
DIM SHARED framesize AS XYPair
DIM SHARED dest_rect AS SDL_Rect
DIM SHARED mouseclipped AS INTEGER = NO   'Whether we are ACTUALLY clipped
DIM SHARED forced_mouse_clipping AS INTEGER = NO
'These were the args to the last call to io_mouserect
DIM SHARED remember_mouserect AS RectPoints = ((-1, -1), (-1, -1))
'These are the actual zoomed clip bounds
DIM SHARED AS INTEGER mxmin = -1, mxmax = -1, mymin = -1, mymax = -1
DIM SHARED AS INTEGER privatemx, privatemy, lastmx, lastmy
DIM SHARED keybdstate(127) AS INTEGER  '"real"time keyboard array
DIM SHARED input_buffer AS WSTRING * 128
DIM SHARED mouseclicks AS INTEGER

END EXTERN 'weirdness
'Translate SDL scancodes into a OHR scancodes
DIM SHARED scantrans(0 to 322) AS INTEGER
scantrans(SDLK_UNKNOWN) = 0
scantrans(SDLK_BACKSPACE) = scBackspace
scantrans(SDLK_TAB) = scTab
scantrans(SDLK_CLEAR) = 0
scantrans(SDLK_RETURN) = scEnter
scantrans(SDLK_PAUSE) = scPause
scantrans(SDLK_ESCAPE) = scEsc
scantrans(SDLK_SPACE) = scSpace
scantrans(SDLK_EXCLAIM) = scExclamation
scantrans(SDLK_QUOTEDBL) = scQuote
scantrans(SDLK_HASH) = scHash
scantrans(SDLK_DOLLAR) = scDollarSign
scantrans(SDLK_AMPERSAND) = scAmpersand
scantrans(SDLK_QUOTE) = scQuote
scantrans(SDLK_LEFTPAREN) = scLeftParenthesis
scantrans(SDLK_RIGHTPAREN) = scRightParenthesis
scantrans(SDLK_ASTERISK) = scAsterisk
scantrans(SDLK_PLUS) = scPlus
scantrans(SDLK_COMMA) = scComma
scantrans(SDLK_MINUS) = scMinus
scantrans(SDLK_PERIOD) = scPeriod
scantrans(SDLK_SLASH) = scSlash
scantrans(SDLK_0) = sc0
scantrans(SDLK_1) = sc1
scantrans(SDLK_2) = sc2
scantrans(SDLK_3) = sc3
scantrans(SDLK_4) = sc4
scantrans(SDLK_5) = sc5
scantrans(SDLK_6) = sc6
scantrans(SDLK_7) = sc7
scantrans(SDLK_8) = sc8
scantrans(SDLK_9) = sc9
scantrans(SDLK_COLON) = scColon
scantrans(SDLK_SEMICOLON) = scSemicolon
scantrans(SDLK_LESS) = scLeftCaret
scantrans(SDLK_EQUALS) = scEquals
scantrans(SDLK_GREATER) = scRightCaret
scantrans(SDLK_QUESTION) = scQuestionMark
scantrans(SDLK_AT) = scAtSign
scantrans(SDLK_LEFTBRACKET) = scLeftBracket
scantrans(SDLK_BACKSLASH) = scBackslash
scantrans(SDLK_RIGHTBRACKET) = scRightBracket
scantrans(SDLK_CARET) = scCircumflex
scantrans(SDLK_UNDERSCORE) = scUnderscore
scantrans(SDLK_BACKQUOTE) = scBackquote
scantrans(SDLK_a) = scA
scantrans(SDLK_b) = scB
scantrans(SDLK_c) = scC
scantrans(SDLK_d) = scD
scantrans(SDLK_e) = scE
scantrans(SDLK_f) = scF
scantrans(SDLK_g) = scG
scantrans(SDLK_h) = scH
scantrans(SDLK_i) = scI
scantrans(SDLK_j) = scJ
scantrans(SDLK_k) = scK
scantrans(SDLK_l) = scL
scantrans(SDLK_m) = scM
scantrans(SDLK_n) = scN
scantrans(SDLK_o) = scO
scantrans(SDLK_p) = scP
scantrans(SDLK_q) = scQ
scantrans(SDLK_r) = scR
scantrans(SDLK_s) = scS
scantrans(SDLK_t) = scT
scantrans(SDLK_u) = scU
scantrans(SDLK_v) = scV
scantrans(SDLK_w) = scW
scantrans(SDLK_x) = scX
scantrans(SDLK_y) = scY
scantrans(SDLK_z) = scZ
scantrans(SDLK_DELETE) = scDelete
scantrans(SDLK_KP0) = scNumpad0
scantrans(SDLK_KP1) = scNumpad1
scantrans(SDLK_KP2) = scNumpad2
scantrans(SDLK_KP3) = scNumpad3
scantrans(SDLK_KP4) = scNumpad4
scantrans(SDLK_KP5) = scNumpad5
scantrans(SDLK_KP6) = scNumpad6
scantrans(SDLK_KP7) = scNumpad7
scantrans(SDLK_KP8) = scNumpad8
scantrans(SDLK_KP9) = scNumpad9
scantrans(SDLK_KP_PERIOD) = scNumpadPeriod
scantrans(SDLK_KP_DIVIDE) = scNumpadSlash
scantrans(SDLK_KP_MULTIPLY) = scNumpadAsterisk
scantrans(SDLK_KP_MINUS) = scNumpadMinus
scantrans(SDLK_KP_PLUS) = scNumpadPlus
scantrans(SDLK_KP_ENTER) = scNumpadEnter
scantrans(SDLK_KP_EQUALS) = scEquals
scantrans(SDLK_UP) = scUp
scantrans(SDLK_DOWN) = scDown
scantrans(SDLK_RIGHT) = scRight
scantrans(SDLK_LEFT) = scLeft
scantrans(SDLK_INSERT) = scInsert
scantrans(SDLK_HOME) = scHome
scantrans(SDLK_END) = scEnd
scantrans(SDLK_PAGEUP) = scPageup
scantrans(SDLK_PAGEDOWN) = scPagedown
scantrans(SDLK_F1) = scF1
scantrans(SDLK_F2) = scF2
scantrans(SDLK_F3) = scF3
scantrans(SDLK_F4) = scF4
scantrans(SDLK_F5) = scF5
scantrans(SDLK_F6) = scF6
scantrans(SDLK_F7) = scF7
scantrans(SDLK_F8) = scF8
scantrans(SDLK_F9) = scF9
scantrans(SDLK_F10) = scF10
scantrans(SDLK_F11) = scF11
scantrans(SDLK_F12) = scF12
scantrans(SDLK_F13) = scF13
scantrans(SDLK_F14) = scF14
scantrans(SDLK_F15) = scF15
scantrans(SDLK_NUMLOCK) = scNumlock
scantrans(SDLK_CAPSLOCK) = scCapslock
scantrans(SDLK_SCROLLOCK) = scScrollLock
scantrans(SDLK_RSHIFT) = scRightShift
scantrans(SDLK_LSHIFT) = scLeftShift
scantrans(SDLK_RCTRL) = scRightCtrl
scantrans(SDLK_LCTRL) = scLeftCtrl
scantrans(SDLK_RALT) = scRightAlt
scantrans(SDLK_LALT) = scLeftAlt
scantrans(SDLK_RMETA) = scRightCommand
scantrans(SDLK_LMETA) = scLeftCommand
scantrans(SDLK_LSUPER) = scLeftWinLogo
scantrans(SDLK_RSUPER) = scRightWinLogo
scantrans(SDLK_MODE) = 0
scantrans(SDLK_COMPOSE) = 0
scantrans(SDLK_HELP) = 0
scantrans(SDLK_PRINT) = scPrintScreen
scantrans(SDLK_SYSREQ) = scPrintScreen
scantrans(SDLK_BREAK) = scPause
scantrans(SDLK_MENU) = scContext
scantrans(SDLK_POWER) = 0
scantrans(SDLK_EURO) = 0
scantrans(SDLK_UNDO) = 0
EXTERN "C"


FUNCTION gfx_sdl_init(byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr, byval info_buffer as zstring ptr, byval info_buffer_size as integer) as integer
/' Trying to load the resource as a SDL_Surface, Unfinished - the winapi has lost me
#ifdef __FB_WIN32__
  DIM as HBITMAP iconh
  DIM as BITMAP iconbmp
  iconh = cast(HBITMAP, LoadImage(NULL, windowicon, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION))
  GetObject(iconh, sizeof(iconbmp), @iconbmp);
#endif
'/
  'starting with svn revision 3964 custom actually supports capslock
  'as a toggle, so we no longer want to treat it like a regular key.
  'that is why these following lines are commented out

  ''disable capslock/numlock/pause special keypress behaviour
  'putenv("SDL_DISABLE_LOCK_KEYS=1") 'SDL 1.2.14
  'putenv("SDL_NO_LOCK_KEYS=1")      'SDL SVN between 1.2.13 and 1.2.14

  IF running_as_slave = NO THEN   'Don't display the window straight on top of Custom's
    putenv("SDL_VIDEO_CENTERED=1")
  ELSE
    putenv("SDL_VIDEO_CENTERED=0")
    putenv("SDL_VIDEO_WINDOW_POS=5,5")
  END IF

  DIM ver as SDL_version ptr = SDL_Linked_Version()
  *info_buffer = MID("SDL " & ver->major & "." & ver->minor & "." & ver->patch, 1, info_buffer_size)
  IF SDL_WasInit(0) = 0 THEN
    IF SDL_Init(SDL_INIT_VIDEO OR SDL_INIT_JOYSTICK) THEN
      *info_buffer = MID("Can't start SDL (video): " & *SDL_GetError & LINE_END & *info_buffer, 1, info_buffer_size)
      RETURN 0
    END IF
  ELSEIF SDL_WasInit(SDL_INIT_VIDEO) = 0 THEN
    IF SDL_InitSubSystem(SDL_INIT_VIDEO) THEN
      *info_buffer = MID("Can't start SDL video subsys: " & *SDL_GetError & LINE_END & *info_buffer, 1, info_buffer_size)
      RETURN 0
    END IF
  END IF
  SDL_EnableKeyRepeat(400, 50)

  *info_buffer = *info_buffer & " (" & SDL_NumJoysticks() & " joysticks) Driver:"
  SDL_VideoDriverName(info_buffer + LEN(*info_buffer), info_buffer_size - LEN(*info_buffer))

  framesize.w = 320
  framesize.h = 200
  RETURN gfx_sdl_set_screen_mode()
END FUNCTION

'Not used on any platforms at the moment, but could be useful on several
SUB select_zoom_automatically(byval w as integer, byval h as integer)
  ' DIM info as SDL_VideoInfo ptr
  ' info = SDL_GetVideoInfo()
  ' IF info = NULL THEN
  '   debug "SDL_GetVideoInfo failed: " & *SDL_GetError()
  '   EXIT SUB
  ' END IF
  zoom = small(w \ framesize.w, h \ framesize.h)
  zoom = large(zoom, 1)
  debuginfo "gfx_sdl: selected zoom = " & zoom
END SUB

FUNCTION gfx_sdl_set_screen_mode(byval bitdepth as integer = 0) as integer
  DIM flags AS Uint32 = 0
  IF resizable THEN flags = flags OR SDL_RESIZABLE
  IF windowedmode = NO THEN
    flags = flags OR SDL_FULLSCREEN
  END IF
#IFDEF __FB_DARWIN__
  IF SDL_WasInit(SDL_INIT_VIDEO) THEN
    SDL_QuitSubSystem(SDL_INIT_VIDEO)
    IF SDL_InitSubSystem(SDL_INIT_VIDEO) THEN
      debug "Can't start SDL video subsys (resize): " & *SDL_GetError
    END IF
  END IF
  'Force clipping in fullscreen, and undo when leaving
  set_forced_mouse_clipping (windowedmode = NO)
#ENDIF
#IFDEF __FB_ANDROID__
  'If fullscreen mode is requested on android, use the minimum size
  'and it should be stretched
  screensurface = SDL_SetVideoMode(320, 200, bitdepth, flags)
  IF screensurface = NULL THEN
    debug "Failed to open display (bitdepth = " & bitdepth & ", flags = " & flags & "): " & *SDL_GetError()
    RETURN 0
  END IF
  debuginfo "gfx_sdl: screen size is " & screensurface->w & "*" & screensurface->h
  zoom = 1
  WITH dest_rect
    .x = 0
    .y = 0
    .w = framesize.w * zoom
    .h = framesize.h * zoom
  END WITH
#ELSE
  'Start with initial zoom and repeatedly decrease it if it is too large
  '(This is necessary to run in fullscreen in OSX IIRC)
  DO
    WITH dest_rect
      .x = 0
      .y = 0
      .w = framesize.w * zoom
      .h = framesize.h * zoom
    END WITH
    screensurface = SDL_SetVideoMode(dest_rect.w, dest_rect.h, bitdepth, flags)
    IF screensurface = NULL THEN
      'This crude hack won't work for everyone if the SDL error messages are internationalised...
      IF zoom > 1 ANDALSO strstr(SDL_GetError(), "No video mode large enough") THEN
        debug "Failed to open display (windowed = " & windowedmode & ") (retrying with smaller zoom): " & *SDL_GetError
        IF remember_zoom = -1 THEN
          remember_zoom = zoom
        END IF
        zoom -= 1
        CONTINUE DO
      END IF
      debug "Failed to open display (windowed = " & windowedmode & "): " & *SDL_GetError
      RETURN 0
    END IF
    EXIT DO
  LOOP
#ENDIF
  SDL_WM_SetCaption(remember_windowtitle, remember_windowtitle)
  IF windowedmode = NO THEN
    SDL_ShowCursor(0)
  ELSE
    SDL_ShowCursor(rememmvis)
  END IF
  RETURN 1
END FUNCTION

SUB gfx_sdl_close()
  IF SDL_WasInit(SDL_INIT_VIDEO) THEN
    IF screenbuffer <> NULL THEN SDL_FreeSurface(screenbuffer)
    FOR i as integer = 0 TO small(SDL_NumJoysticks(), 8) - 1
      IF joystickhandles(i) <> NULL THEN SDL_JoystickClose(joystickhandles(i))
      joystickhandles(i) = NULL
    NEXT
    SDL_QuitSubSystem(SDL_INIT_VIDEO)
    IF SDL_WasInit(0) = 0 THEN
      SDL_Quit()
    END IF
  END IF
END SUB

FUNCTION gfx_sdl_getversion() as integer
  RETURN 1
END FUNCTION

FUNCTION gfx_sdl_present_internal(byval raw as any ptr, byval w as integer, byval h as integer, byval bitdepth as integer) as integer

  'variable resolution handling
  IF framesize.w <> w OR framesize.h <> h THEN
    framesize.w = w
    framesize.h = h
    'A bitdepth of 0 indicates 'same as previous, otherwise default (native)'. Not sure if it's best to use
    'a native or 8 bit screen surface when we're drawing 8 bit; simply going to preserve the status quo for now
    gfx_sdl_set_screen_mode(IIF(bitdepth = 8, 0, bitdepth))
    IF screenbuffer THEN
      SDL_FreeSurface(screenbuffer)
      screenbuffer = NULL
    END IF
  END IF

  IF bitdepth = 8 THEN

    'We may either blit to screensurface (doing 8 bit -> display pixel format conversion) first
    'and then smoothzoom, with smoothzoomblit_anybit
    'Or smoothzoom first, with smoothzoomblit_8_to_8bit, and then blit to screensurface

    IF screenbuffer ANDALSO (screenbuffer->w <> w * zoom OR screenbuffer->h <> h * zoom) THEN
      SDL_FreeSurface(screenbuffer)
      screenbuffer = NULL
    END IF

    IF screenbuffer = NULL THEN
      screenbuffer = SDL_CreateRGBSurface(SDL_SWSURFACE, w * zoom, h * zoom, 8, 0,0,0,0)
    END IF
    'screenbuffer = SDL_CreateRGBSurfaceFrom(raw, w, h, 8, w, 0,0,0,0)
    IF screenbuffer = NULL THEN
      debug "gfx_sdl_present_internal: Failed to allocate page wrapping surface, " & *SDL_GetError
      SYSTEM
    END IF

    smoothzoomblit_8_to_8bit(raw, screenbuffer->pixels, w, h, screenbuffer->pitch, zoom, smooth)
    gfx_sdl_8bit_update_screen()

  ELSE
    '32 bit surface

    IF screensurface->format->BitsPerPixel <> 32 THEN
      gfx_sdl_set_screen_mode(32)
    END IF
    IF screensurface = NULL THEN
      debug "gfx_sdl_present_internal: no screen!"
      RETURN 1
    END IF

    'smoothzoomblit takes the pitch in pixels, not bytes!
    smoothzoomblit_32_to_32bit(raw, cast(uinteger ptr, screensurface->pixels), w, h, screensurface->pitch \ 4, zoom, smooth)
    IF SDL_Flip(screensurface) THEN
      debug "gfx_sdl_present_internal: SDL_Flip failed: " & *SDL_GetError
    END IF
    update_state()
  END IF

  RETURN 0
END FUNCTION

FUNCTION gfx_sdl_present(byval surfaceIn as Surface ptr, byval pal as BackendPalette ptr) as integer
  WITH *surfaceIn
    IF .format = SF_8bit AND pal <> NULL THEN
      FOR i as integer = 0 TO 255
        sdlpalette(i).r = pal->col(i).r
        sdlpalette(i).g = pal->col(i).g
        sdlpalette(i).b = pal->col(i).b
      NEXT
    END IF
    RETURN gfx_sdl_present_internal(.pColorData, .width, .height, IIF(.format = SF_8bit, 8, 32))
  END WITH
END FUNCTION

SUB gfx_sdl_showpage(byval raw as ubyte ptr, byval w as integer, byval h as integer)
  'takes a pointer to a raw 8-bit image, with pitch = w
  gfx_sdl_present_internal(raw, w, h, 8)
END SUB

'Update the screen image and palette
SUB gfx_sdl_8bit_update_screen()
  IF screenbuffer <> NULL and screensurface <> NULL THEN
    IF SDL_SetColors(screenbuffer, @sdlpalette(0), 0, 256) = 0 THEN
      debug "gfx_sdl_8bit_update_screen: SDL_SetColors failed: " & *SDL_GetError
    END IF
    IF SDL_BlitSurface(screenbuffer, NULL, screensurface, @dest_rect) THEN
      debug "gfx_sdl_8bit_update_screen: SDL_BlitSurface failed: " & *SDL_GetError
    END IF
    IF SDL_Flip(screensurface) THEN
      debug "gfx_sdl_8bit_update_screen: SDL_Flip failed: " & *SDL_GetError
    END IF
    update_state()
  END IF
END SUB

SUB gfx_sdl_setpal(byval pal as RGBcolor ptr)
  DIM i AS INTEGER
  FOR i = 0 TO 255
    sdlpalette(i).r = pal[i].r
    sdlpalette(i).g = pal[i].g
    sdlpalette(i).b = pal[i].b
  NEXT
  gfx_sdl_8bit_update_screen()
END SUB

FUNCTION gfx_sdl_screenshot(byval fname as zstring ptr) as integer
  gfx_sdl_screenshot = 0
END FUNCTION

SUB gfx_sdl_setwindowed(byval iswindow as integer)
#IFDEF __FB_DARWIN__
  IF iswindow = NO THEN
    'Zoom 3 or 4 look better in fullscreen, so change to one of those temporarily
    IF zoom <= 2 AND zoom_has_been_changed = NO THEN
      remember_zoom = zoom
      zoom = 3
    END IF
  ELSE
    'Change zoom back?
    IF remember_zoom <> -1 AND zoom_has_been_changed = NO THEN
      zoom = remember_zoom
    END IF
  END IF
#ENDIF
  IF iswindow = 0 THEN
    windowedmode = NO
  ELSE
    windowedmode = YES
  END IF
  gfx_sdl_set_screen_mode()
  IF screensurface = NULL THEN
   'Attempt to fallback
   windowedmode XOR= YES
   IF remember_zoom <> -1 THEN
     zoom = remember_zoom
   END IF
   DIM remem_error as string = *SDL_GetError
   gfx_sdl_set_screen_mode()
   IF screensurface THEN
     notification "Could not toggle fullscreen mode: " & remem_error
   ELSE
     debugc errDie, "gfx_sdl: Could not recover after toggling fullscreen mode failed"
   END IF
  END IF
END SUB

SUB gfx_sdl_windowtitle(byval title as zstring ptr)
  IF SDL_WasInit(SDL_INIT_VIDEO) then
    SDL_WM_SetCaption(title, title)
  END IF
  remember_windowtitle = *title
END SUB

FUNCTION gfx_sdl_getwindowstate() as WindowState ptr
  STATIC state as WindowState
  DIM temp as integer = SDL_GetAppState()
  state.focused = (temp AND SDL_APPINPUTFOCUS) <> 0
  state.minimised = (temp AND SDL_APPACTIVE) = 0
  RETURN @state
END FUNCTION

SUB gfx_sdl_setresizable(byval able as integer)
  resizable = able
  gfx_sdl_set_screen_mode()
END SUB

FUNCTION gfx_sdl_getresize(byref ret as XYPair) as integer
  IF resizerequested THEN
    ret = resizerequest
    resizerequested = NO
    RETURN YES
  END IF
  RETURN NO
END FUNCTION

SUB gfx_sdl_set_zoom(byval value as integer)
  IF value >= 1 AND value <= 16 AND value <> zoom THEN
    zoom = value
    zoom_has_been_changed = YES
    IF SDL_WasInit(SDL_INIT_VIDEO) THEN
      gfx_sdl_set_screen_mode()
    END IF

    'Update the clip rectangle
    'It would probably be easier to just store the non-zoomed clipped rect (mxmin, etc)
    WITH remember_mouserect
      IF .p1.x <> -1 THEN
        internal_set_mouserect .p1.x, .p2.x, .p1.y, .p2.y
      ELSEIF forced_mouse_clipping THEN
        internal_set_mouserect 0, framesize.w - 1, 0, framesize.h - 1
      END IF
    END WITH
  END IF
END SUB

FUNCTION gfx_sdl_setoption(byval opt as zstring ptr, byval arg as zstring ptr) as integer
  DIM ret as integer = 0
  DIM value as integer = str2int(*arg, -1)
  IF *opt = "zoom" or *opt = "z" THEN
    gfx_sdl_set_zoom(value)
    ret = 1
  ELSEIF *opt = "smooth" OR *opt = "s" THEN
    IF value = 1 OR value = -1 THEN  'arg optional (-1)
      smooth = 1
    ELSE
      smooth = 0
    END IF
    ret = 1
  END IF
  'globble numerical args even if invalid
  IF ret = 1 AND is_int(*arg) THEN ret = 2
  RETURN ret
END FUNCTION

FUNCTION gfx_sdl_describe_options() as zstring ptr
  return @"-z -zoom [1...16]   Scale screen to 1,2, ... up to 16x normal size (2x default)" LINE_END _
          "-s -smooth          Enable smoothing filter for zoom modes (default off)"
END FUNCTION

SUB io_sdl_init
  'nothing needed at the moment...
END SUB

SUB keycombos_logic(evnt as SDL_Event)
  'Check for platform-dependent key combinations

  IF evnt.key.keysym.mod_ AND KMOD_ALT THEN
    IF evnt.key.keysym.sym = SDLK_RETURN THEN  'alt-enter (not processed normally when using SDL)
      gfx_sdl_setwindowed(windowedmode XOR YES)
    END IF
    IF evnt.key.keysym.sym = SDLK_F4 THEN  'alt-F4
      post_terminate_signal
    END IF
  END IF

#IFDEF __FB_DARWIN__
  'We have to handle menu item key combinations here: SDLMain.m only handles the case that you actually click on them

  IF evnt.key.keysym.mod_ AND KMOD_META THEN  'Command key
    IF evnt.key.keysym.sym = SDLK_m THEN
      sdlCocoaMinimise()
    END IF
    IF evnt.key.keysym.sym = SDLK_h THEN
      IF evnt.key.keysym.mod_ AND KMOD_SHIFT THEN
        sdlCocoaHideOthers()  'Cmd-Shift-H
      ELSE
        sdlCocoaHide()  'Cmd-H
      END IF
    END IF
    IF evnt.key.keysym.sym = SDLK_q THEN
      post_terminate_signal
    END IF
    IF evnt.key.keysym.sym = SDLK_f THEN
      gfx_sdl_setwindowed(windowedmode XOR YES)
    END IF
    'SDL doesn't actually seem to send SDLK_QUESTION...
    IF evnt.key.keysym.sym = SDLK_SLASH AND evnt.key.keysym.mod_ AND KMOD_SHIFT THEN
      keybdstate(scF1) = 2
    END IF
    FOR i as integer = 1 TO 4
      IF evnt.key.keysym.sym = SDLK_0 + i THEN
        gfx_sdl_set_zoom(i)
      END IF
    NEXT
  END IF
#ENDIF

END SUB

SUB gfx_sdl_process_events()
'The SDL event queue only holds 128 events, after which SDL_QuitEvents will be lost
'Of course, we might actually like to do something with some of the other events
  DIM evnt as SDL_Event

  WHILE SDL_PeepEvents(@evnt, 1, SDL_GETEVENT, SDL_ALLEVENTS)
    SELECT CASE evnt.type
      CASE SDL_EXIT
        post_terminate_signal
      CASE SDL_KEYDOWN
        keycombos_logic(evnt)
        DIM AS INTEGER key = scantrans(evnt.key.keysym.sym)
        IF LEN(input_buffer) >= 127 THEN input_buffer = RIGHT(input_buffer, 126)
        input_buffer += WCHR(evnt.key.keysym.unicode_)
        'lowest bit is now set in io_keybits, from SDL_GetKeyState
        'IF key THEN keybdstate(key) = 3
        IF key THEN keybdstate(key) = 2
        'debuginfo "key down: " & evnt.key.keysym.sym & " -> " & key & " U " & evnt.key.keysym.unicode_
      CASE SDL_KEYUP
        DIM AS INTEGER key = scantrans(evnt.key.keysym.sym)
        IF key THEN keybdstate(key) AND= NOT 1
      CASE SDL_MOUSEBUTTONDOWN
        'note SDL_GetMouseState is still used, while SDL_GetKeyState isn't
        mouseclicks OR= SDL_BUTTON(evnt.button.button)
      CASE SDL_ACTIVEEVENT
        'debug "SDL_ACTIVEEVENT " & evnt.active.state
        IF evnt.active.state AND SDL_APPINPUTFOCUS THEN
          IF evnt.active.gain = 0 THEN
            SDL_ShowCursor(1)
            IF mouseclipped = 1 THEN
              SDL_WarpMouse privatemx, privatemy
              SDL_PumpEvents
            END IF
          ELSE
            IF windowedmode THEN
              SDL_ShowCursor(rememmvis)
            ELSE
              SDL_ShowCursor(0)
            END IF
            IF mouseclipped = 1 THEN
              SDL_GetMouseState(@privatemx, @privatemy)
              lastmx = privatemx
              lastmy = privatemy
              'SDL_WarpMouse screensurface->w \ 2, screensurface->h \ 2
              'SDL_PumpEvents
              'lastmx = screensurface->w \ 2
              'lastmy = screensurface->h \ 2
            END IF
          END IF
        END IF
      CASE SDL_VIDEORESIZE
        'debug "SDL_VIDEORESIZE: w=" & evnt.resize.w & " h=" & evnt.resize.h
        IF resizable THEN
          resizerequested = YES
          resizerequest.w = evnt.resize.w / zoom
          resizerequest.h = evnt.resize.h / zoom
        END IF
    END SELECT
  WEND
END SUB

'may only be called from the main thread
SUB update_state()
  SDL_PumpEvents()
  update_mouse()
  gfx_sdl_process_events()
END SUB

SUB io_sdl_pollkeyevents()
  'might need to redraw the screen if exposed
  IF SDL_Flip(screensurface) THEN
    debug "pollkeyevents: SDL_Flip failed: " & *SDL_GetError
  END IF
  update_state()
END SUB

SUB io_sdl_waitprocessing()
  update_state()
END SUB

SUB io_sdl_keybits (BYVAL keybdarray as integer ptr)
  FOR a as integer = 0 TO &h7f
    keybdarray[a] = keybdstate(a)
    keybdstate(a) = keybdstate(a) and 1
  NEXT

  'calling SHELL on Windows when not compiled with -s console seems to cause SDL to not send
  'key up events for currently held keys, so we have to abandon the events-only scheme
  'FIXME: this workaround did not work, so now we can un-abandon events-only
  DIM keystate AS UINT8 PTR = NULL
  keystate = SDL_GetKeyState(NULL)
  FOR a as integer = 0 TO 322
    IF keystate[a] THEN
      'debug "OHRkey=" & scantrans(a) & " SDLkey=" & a & " " & *SDL_GetKeyName(a)
      IF scantrans(a) THEN
        keybdarray[scantrans(a)] OR= 1
      END IF
    END IF
  NEXT

  keybdarray[scShift] = keybdarray[scLeftShift] OR keybdarray[scRightShift]
  keybdarray[scUnfilteredAlt] = keybdarray[scLeftAlt] OR keybdarray[scRightAlt]
  keybdarray[scCtrl] = keybdarray[scLeftCtrl] OR keybdarray[scRightCtrl]
END SUB

SUB io_sdl_updatekeys(byval keybd as integer ptr)
  'supports io_keybits instead
END SUB

'Enabling unicode will cause combining keys to go dead on X11 (on non-US
'layouts that have them). This usually means certain punctuation keys such as '
SUB io_sdl_enable_textinput (byval enable as integer)
  SDL_EnableUNICODE(IIF(enable, 1, 0))
END SUB

SUB io_sdl_textinput (byval buf as wstring ptr, byval bufsize as integer)
  'Both FB and SDL only support UCS2, which doesn't have variable len wchars.
  DIM buflen as integer = bufsize \ 2 - 1
  *buf = LEFT(input_buffer, buflen)
  input_buffer = MID(input_buffer, buflen)
END SUB

SUB io_sdl_show_virtual_keyboard()
 'Does nothing on platforms that have real keyboards
#IFDEF __FB_ANDROID__
 if not SDL_ANDROID_IsScreenKeyboardShown() then
  SDL_ANDROID_ToggleScreenKeyboardWithoutTextInput()
 end if
#ENDIF
END SUB

SUB io_sdl_hide_virtual_keyboard()
 'Does nothing on platforms that have real keyboards
#IFDEF __FB_ANDROID__
 if SDL_ANDROID_IsScreenKeyboardShown() then
  SDL_ANDROID_ToggleScreenKeyboardWithoutTextInput()
 end if
#ENDIF
END SUB

SUB io_sdl_show_virtual_gamepad()
 'Does nothing on other platforms
#IFDEF __FB_ANDROID__
 SDL_ANDROID_SetScreenKeyboardShown(YES)
#ENDIF
END SUB

SUB io_sdl_hide_virtual_gamepad()
 'Does nothing on other platforms
#IFDEF __FB_ANDROID__
 'This does not work for some reason
 SDL_ANDROID_SetScreenKeyboardShown(NO)
#ENDIF
END SUB

SUB io_sdl_setmousevisibility(byval visible as integer)
  rememmvis = iif(visible, 1, 0)
  SDL_ShowCursor(iif(windowedmode, rememmvis, 0))
END SUB

'Change from SDL to OHR mouse button numbering (swap middle and right)
FUNCTION fix_buttons(byval buttons as integer) as integer
  DIM mbuttons as integer = 0
  IF SDL_BUTTON(SDL_BUTTON_LEFT) AND buttons THEN mbuttons = mbuttons OR 1
  IF SDL_BUTTON(SDL_BUTTON_RIGHT) AND buttons THEN mbuttons = mbuttons OR 2
  IF SDL_BUTTON(SDL_BUTTON_MIDDLE) AND buttons THEN mbuttons = mbuttons OR 4
  RETURN mbuttons
END FUNCTION

FUNCTION update_mouse() as integer
  DIM x AS INTEGER
  DIM y AS INTEGER
  DIM buttons AS Uint8

  buttons = SDL_GetMouseState(@x, @y)
  IF SDL_GetAppState() AND SDL_APPINPUTFOCUS THEN
    IF mouseclipped THEN
      'Not moving the mouse back to the centre of the window rapidly is widely recommended, but I haven't seen (nor looked for) evidence that it's bad.
      'Implemented only due to attempting to fix eventually unrelated problem. Possibly beneficial to keep
      'debug "mousestate " & x & " " & y & " (" & lastmx & " " & lastmy & ")"
      privatemx += x - lastmx
      privatemy += y - lastmy
      IF x < 3 * screensurface->w \ 8 OR x > 5 * screensurface->w \ 8 OR _
         y < 3 * screensurface->h \ 8 OR y > 5 * screensurface->h \ 8 THEN
        SDL_WarpMouse screensurface->w \ 2, screensurface->h \ 2
        'Required after warping the mouse for it to take effect. Discovered with much blood, sweat, and murderous rage
        SDL_PumpEvents
        lastmx = screensurface->w \ 2
        lastmy = screensurface->h \ 2
        'debug "warped"
      ELSE
        lastmx = x
        lastmy = y
      END IF
      privatemx = bound(privatemx, mxmin, mxmax)
      privatemy = bound(privatemy, mymin, mymax)
    ELSE
      privatemx = x
      privatemy = y
    END IF
  END IF
  RETURN buttons
END FUNCTION

SUB io_sdl_mousebits (byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer, byref mclicks as integer)
  DIM buttons as integer
  buttons = update_mouse()
  mx = privatemx \ zoom
  my = privatemy \ zoom

  mclicks = fix_buttons(mouseclicks)
  mbuttons = fix_buttons(buttons or mouseclicks)
  mouseclicks = 0
END SUB

SUB io_sdl_getmouse(byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer)
  'supports io_mousebits instead
END SUB

SUB io_sdl_setmouse(byval x as integer, byval y as integer)
  IF mouseclipped THEN
    privatemx = x * zoom
    privatemy = y * zoom
    'IF SDL_GetAppState() AND SDL_APPINPUTFOCUS THEN
    '  SDL_WarpMouse screensurface->w \ 2, screensurface->h \ 2
    'END IF
  ELSE
    IF SDL_GetAppState() AND SDL_APPINPUTFOCUS THEN
      SDL_WarpMouse x * zoom, y * zoom
      SDL_PumpEvents
    END IF
  END IF
END SUB

SUB internal_set_mouserect(byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
  IF mouseclipped = 0 AND (xmin >= 0) THEN
    'enter clipping mode
    'SDL_WM_GrabInput causes most WM key combinations to be blocked, which I find unacceptable, so instead
    'we stick the mouse at the centre of the window. It's a very common hack.
    mouseclipped = 1
    SDL_GetMouseState(@privatemx, @privatemy)
    IF SDL_GetAppState() AND SDL_APPINPUTFOCUS THEN
      SDL_WarpMouse screensurface->w \ 2, screensurface->h \ 2
      SDL_PumpEvents
    END IF
    lastmx = screensurface->w \ 2
    lastmy = screensurface->h \ 2
  ELSEIF mouseclipped = 1 AND (xmin = -1) THEN
    'exit clipping mode
    mouseclipped = 0
    SDL_WarpMouse privatemx, privatemy
  END IF
  mxmin = xmin * zoom
  mxmax = xmax * zoom + zoom - 1
  mymin = ymin * zoom
  mymax = ymax * zoom + zoom - 1
END SUB

'This turns forced mouse clipping on or off
SUB set_forced_mouse_clipping(byval newvalue as integer)
  newvalue = (newvalue <> 0)
  IF newvalue <> forced_mouse_clipping THEN
    forced_mouse_clipping = newvalue
    IF forced_mouse_clipping THEN
      IF mouseclipped = 0 THEN
        internal_set_mouserect 0, framesize.w - 1, 0, framesize.h - 1
      END IF
      'If already clipped: nothing to be done
    ELSE
      WITH remember_mouserect
        internal_set_mouserect .p1.x, .p2.x, .p1.y, .p2.y
      END WITH
    END IF
  END IF
END SUB

SUB io_sdl_mouserect(byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
  WITH remember_mouserect
    .p1.x = xmin
    .p1.y = ymin
    .p2.x = xmax
    .p2.y = ymax
  END WITH
  IF forced_mouse_clipping AND xmin = -1 THEN
    'Remember that we are now meant to be unclipped, but clip to the window
    internal_set_mouserect 0, framesize.w - 1, 0, framesize.h - 1
  ELSE
    internal_set_mouserect xmin, xmax, ymin, ymax
  END IF
END SUB

FUNCTION io_sdl_readjoysane(byval joynum as integer, byref button as integer, byref x as integer, byref y as integer) as integer
  IF joynum < 0 OR SDL_NumJoysticks() < joynum + 1 THEN RETURN 0
  IF joystickhandles(joynum) = NULL THEN
    joystickhandles(joynum) = SDL_JoystickOpen(joynum)
    IF joystickhandles(joynum) = NULL THEN
      debug "Couldn't open joystick " & joynum & ": " & *SDL_GetError
      RETURN 0
    END IF
  END IF
  SDL_JoystickUpdate() 'should this be here? moved from io_sdl_readjoy
  button = 0
  FOR i as integer = 0 TO SDL_JoystickNumButtons(joystickhandles(joynum)) - 1
    IF SDL_JoystickGetButton(joystickhandles(joynum), i) THEN button = button OR (1 SHL i)
  NEXT
  'SDL_JoystickGetAxis returns a value from -32768 to 32767
  x = SDL_JoystickGetAxis(joystickhandles(joynum), 0) / 32768.0 * 100
  y = SDL_JoystickGetAxis(joystickhandles(joynum), 1) / 32768.0 * 100
  'debug "x=" & x & " y=" & y & " button=" & button
  RETURN 1
END FUNCTION

FUNCTION gfx_sdl_setprocptrs() as integer
  gfx_init = @gfx_sdl_init
  gfx_close = @gfx_sdl_close
  gfx_getversion = @gfx_sdl_getversion
  gfx_showpage = @gfx_sdl_showpage
  gfx_setpal = @gfx_sdl_setpal
  gfx_screenshot = @gfx_sdl_screenshot
  gfx_setwindowed = @gfx_sdl_setwindowed
  gfx_windowtitle = @gfx_sdl_windowtitle
  gfx_getwindowstate = @gfx_sdl_getwindowstate
  gfx_getresize = @gfx_sdl_getresize
  gfx_setresizable = @gfx_sdl_setresizable
  gfx_setoption = @gfx_sdl_setoption
  gfx_describe_options = @gfx_sdl_describe_options
  io_init = @io_sdl_init
  io_pollkeyevents = @io_sdl_pollkeyevents
  io_waitprocessing = @io_sdl_waitprocessing
  io_keybits = @io_sdl_keybits
  io_updatekeys = @io_sdl_updatekeys
  io_enable_textinput = @io_sdl_enable_textinput
  io_textinput = @io_sdl_textinput
  io_show_virtual_keyboard = @io_sdl_show_virtual_keyboard
  io_hide_virtual_keyboard = @io_sdl_hide_virtual_keyboard
  io_mousebits = @io_sdl_mousebits
  io_setmousevisibility = @io_sdl_setmousevisibility
  io_getmouse = @io_sdl_getmouse
  io_setmouse = @io_sdl_setmouse
  io_mouserect = @io_sdl_mouserect
  io_readjoysane = @io_sdl_readjoysane

  gfx_present = @gfx_sdl_present

  RETURN 1
END FUNCTION

END EXTERN
