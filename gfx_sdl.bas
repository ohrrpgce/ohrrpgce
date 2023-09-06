'OHRRPGCE - SDL 1.2 graphics backend
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#include "config.bi"

#ifdef __FB_WIN32__
	'Headers such as SDL.bi include windows.bi; we have to include it first
        'to do the necessary conflict prevention.
	include_windows_bi()
#endif

#include "crt.bi"
#include "gfx.bi"
#include "surface.bi"
#include "common.bi"
#include "allmodex.bi"  'For set_scale_factor
#include "scancodes.bi"
'#define NEED_SDL_GETENV

#ifdef __FB_UNIX__
	'In FB >= 1.04 SDL.bi includes Xlib.bi; fix a conflict
	#undef font
#endif

#include "SDL\SDL.bi"

#ifdef USE_X11
#include "lib/SDL/SDL_x11clipboard.bi"
#endif

#ifdef __FB_WIN32__
#include "lib/SDL/SDL_windowsclipboard.bi"
#endif

#ifdef __FB_DARWIN__
#include "lib/SDL/SDL_cocoaclipboard.bi"
#endif


EXTERN "C"

#define USE_SDL
#include "gfx_sdl_common.bi"

#IFDEF __FB_ANDROID__
'This function shows/hides the sdl virtual gamepad
declare sub SDL_ANDROID_SetScreenKeyboardShown (byval shown as integer)
'This function toggles the display of the android virtual keyboard. always returns 1 no matter what
declare function SDL_ANDROID_ToggleScreenKeyboardWithoutTextInput() as integer 
'WARNING: SDL_ANDROID_IsScreenKeyboardShown seems unreliable. Don't use it! It is only declared here to document its existance. see the virtual_keyboard_shown variable instead
declare function SDL_ANDROID_IsScreenKeyboardShown() as bool
'This function brings up the keyboard to edit an existing piece of text. Key events will not be sent, but instead the buffer will be modified (in real time?). Always returns 1.
declare function SDL_ANDROID_GetScreenKeyboardTextInput(textBuf as zstring ptr, textBufSize as integer) as integer
'Like above this function brings up the keyboard to edit an existing piece of text (it doesn't hide it!). Afterwards, fake key events will be sent to type out the final text.
'Bug: meant to send backspace keypresses, but it doesn't.
'Bug: if any key is down when this is called, it'll get stuck.
'Warning: previousText is limited to length 254
'Always returns 1.
declare function SDL_ANDROID_ToggleScreenKeyboardTextInput(previousText as zstring ptr) as integer
declare function SDL_ANDROID_IsRunningOnConsole () as bool
declare function SDL_ANDROID_IsRunningOnOUYA () as bool
declare sub SDL_ANDROID_set_java_gamepad_keymap(byval A as integer, byval B as integer, byval C as integer, byval X as integer, byval Y as integer, byval Z as integer, byval L1 as integer, byval R1 as integer, byval L2 as integer, byval R2 as integer, byval LT as integer, byval RT as integer)
declare sub SDL_ANDROID_set_ouya_gamepad_keymap(byval player as integer, byval udpad as integer, byval rdpad as integer, byval ldpad as integer, byval ddpad as integer, byval O as integer, byval A as integer, byval U as integer, byval Y as integer, byval L1 as integer, byval R1 as integer, byval L2 as integer, byval R2 as integer, byval LT as integer, byval RT as integer)
declare function SDL_ANDROID_SetScreenKeyboardButtonKey(byval buttonId as integer, byval key as integer) as integer
declare function SDL_ANDROID_SetScreenKeyboardButtonDisable(byval buttonId as integer, byval disable as bool) as integer
declare function SDL_ANDROID_SetKeymapKeyMultitouchGesture(byval buttonId as integer, byval key as integer) as integer
declare sub SDL_ANDROID_SetOUYADeveloperId (byval devId as zstring ptr)
declare sub SDL_ANDROID_OUYAPurchaseRequest (byval identifier as zstring ptr, byval keyDer as zstring ptr, byval keyDerSize as integer)
declare function SDL_ANDROID_OUYAPurchaseIsReady () as bool
declare function SDL_ANDROID_OUYAPurchaseSucceeded () as bool
declare sub SDL_ANDROID_OUYAReceiptsRequest (byval keyDer as zstring ptr, byval keyDerSize as integer)
declare function SDL_ANDROID_OUYAReceiptsAreReady () as bool
declare function SDL_ANDROID_OUYAReceiptsResult () as zstring ptr
#ENDIF

DECLARE FUNCTION putenv (byval as zstring ptr) as integer
#IFNDEF __FB_WIN32__
'Doens't work on Windows. There we do putenv with a null string
DECLARE FUNCTION unsetenv (byval as zstring ptr) as integer
#ENDIF

'DECLARE FUNCTION SDL_putenv cdecl alias "SDL_putenv" (byval variable as zstring ptr) as integer
'DECLARE FUNCTION SDL_getenv cdecl alias "SDL_getenv" (byval name as zstring ptr) as zstring ptr

DECLARE SUB gfx_sdl_recenter_window_hint()
DECLARE FUNCTION gfx_sdl_set_screen_mode(bitdepth as integer = 0, quiet as bool = NO) as bool
DECLARE SUB gfx_sdl_set_zoom(value as integer, change_windowsize as bool)
DECLARE SUB gfx_sdl_8bit_update_screen()
DECLARE SUB update_state()
DECLARE FUNCTION update_mouse() as integer
DECLARE SUB update_mouse_visibility()
DECLARE SUB set_forced_mouse_clipping(byval newvalue as bool)
DECLARE SUB update_mouserect()
DECLARE SUB internal_disable_virtual_gamepad()
DECLARE FUNCTION scOHR2SDL(byval ohr_scancode as integer, byval default_sdl_scancode as integer=0) as integer
DECLARE FUNCTION load_wminfo() as bool
DECLARE SUB quit_video_subsystem()


#IFDEF __FB_DARWIN__

'--These wrapper functions in mac/SDLMain.m call various Cocoa methods
DECLARE SUB sdlCocoaHide()
DECLARE SUB sdlCocoaHideOthers()
DECLARE SUB sdlCocoaMinimise()

#ENDIF

DIM SHARED libsdl_handle as any ptr  'Not used, only to match gfx_sdl2
DIM SHARED zoom as integer = 2
DIM SHARED zoom_has_been_changed as bool = NO
DIM SHARED remember_zoom as integer = -1   'We may change the zoom when fullscreening, so remember it
DIM SHARED smooth as integer = 0
DIM SHARED screensurface as SDL_Surface ptr = NULL  'The output surface (the window)
DIM SHARED screenbuffer as SDL_Surface ptr = NULL  'Drawn to instead of screensurface if format conversion needed
DIM SHARED screensurface_is_RGBColor as bool  'screensurface format is same pixel format as RGBColor
DIM SHARED wminfo as SDL_SysWMinfo   'Must call load_wminfo() to load this global
DIM SHARED windowedmode as bool = YES
DIM SHARED screen_size as XYPair     'Size of the desktop/monitor (virtual size)
DIM SHARED lastwindowsize as XYPair  'Size of screensurface
DIM SHARED resizable as bool = NO
DIM SHARED resize_requested as bool = NO
DIM SHARED resize_request as XYPair
DIM SHARED force_video_reset as bool = NO
'(This used to be set to true on OSX, due to problems years ago without it, but
'it's harmful with SDL 1.2.14 and OS 10.8.5)
DIM SHARED always_force_video_reset as bool = NO
DIM SHARED remember_windowtitle as string
DIM SHARED remember_enable_textinput as bool = NO
DIM SHARED mouse_visibility as CursorVisibility = cursorDefault
DIM SHARED sdlpalette(0 TO 255) as SDL_Color
DIM SHARED framesize as XYPair = XY(320, 200)
DIM SHARED dest_rect as SDL_Rect
DIM SHARED mouseclipped as bool = NO   'Whether we are ACTUALLY clipped
DIM SHARED forced_mouse_clipping as bool = NO
'These were the args to the last call to io_mouserect
DIM SHARED remember_mouserect as RectPoints = ((-1, -1), (-1, -1))
'These are the actual zoomed clip bounds
DIM SHARED as integer mxmin = -1, mxmax = -1, mymin = -1, mymax = -1
DIM SHARED as int32 privatemx, privatemy, lastmx, lastmy
DIM SHARED keybdstate(127) as KeyBits  '"real"time keyboard array. See io_sdl_keybits for docs.
DIM SHARED input_buffer as wstring * 128
DIM SHARED mouseclicks as integer    'Bitmask of mouse buttons clicked (SDL order, not OHR), since last io_mousebits
DIM SHARED mousewheel as integer     'Position of the wheel. A multiple of 120
DIM SHARED virtual_keyboard_shown as bool = NO
DIM SHARED allow_virtual_gamepad as bool = YES
DIM SHARED safe_zone_margin as single = 0.0
#IFDEF __FB_ANDROID__
DIM SHARED warped_mouse as bool = NO  'No mouse movement since last SDL_WarpMouse?
#ENDIF


END EXTERN ' Can't put assignment statements in an extern block

'Translate SDL scancodes into a OHR scancodes
'Of course, scancodes can only be correctly mapped to OHR scancodes on a US keyboard.
'SDL scancodes say what's the unmodified character on a key. For example
'on a German keyboard the +/*/~ key is SDLK_PLUS, gets mapped to
'scPlus, which is the same as scEquals, so you get = when you press
'it.
'If there is no ASCII equivalent character, the key has a SDLK_WORLD_## scancode.

DIM SHARED scantrans(0 to SDLK_LAST - 1) as integer
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
scantrans(SDLK_MODE) = scRightAlt   'Alt Gr, but treat it as alt
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


FUNCTION gfx_sdl_getversion() as integer
  RETURN 1
END FUNCTION

FUNCTION gfx_sdl_init(byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr, byval info_buffer as zstring ptr, byval info_buffer_size as integer) as integer
/' Trying to load the resource as a SDL_Surface, Unfinished - the winapi has lost me
#ifdef __FB_WIN32__
  DIM as HBITMAP iconh
  DIM as BITMAP iconbmp
  iconh = cast(HBITMAP, LoadImage(NULL, windowicon, IMAGE_BITMAP, 0, 0, LR_CREATEDIBSECTION))
  GetObject(iconh, sizeof(iconbmp), @iconbmp);
#endif
'/

  #ifdef USE_X11
    'Xlib will kill the program if most errors occur, such as if OpenGL on the machine is broken
    'so the window can't be created. We need to install an error handler to prevent that
    set_X11_error_handlers
  #endif

  'starting with svn revision 3964 custom actually supports capslock
  'as a toggle, so we no longer want to treat it like a regular key.
  'that is why these following lines are commented out

  ''disable capslock/numlock/pause special keypress behaviour
  'putenv("SDL_DISABLE_LOCK_KEYS=1") 'SDL 1.2.14
  'putenv("SDL_NO_LOCK_KEYS=1")      'SDL SVN between 1.2.13 and 1.2.14

  gfx_sdl_recenter_window_hint()

#ifndef IS_GAME
  'By default SDL prevents screensaver (new in SDL 1.2.10)
  putenv("SDL_VIDEO_ALLOW_SCREENSAVER=1")
#endif

  DIM ver as const SDL_version ptr = SDL_Linked_Version()
  *info_buffer = MID("SDL " & ver->major & "." & ver->minor & "." & ver->patch, 1, info_buffer_size)

  DIM video_already_init as bool = (SDL_WasInit(SDL_INIT_VIDEO) <> 0)

  IF SDL_Init(SDL_INIT_VIDEO OR SDL_INIT_JOYSTICK) THEN
    *info_buffer = MID("Can't start SDL (video): " & *SDL_GetError & LINE_END & *info_buffer, 1, info_buffer_size)
    RETURN 0
  END IF

  IF video_already_init = NO THEN
    'Get resolution of the screen, must be done before opening a window,
    'as after that this gives the size of the window instead.
    DIM videoinfo as const SDL_VideoInfo ptr = SDL_GetVideoInfo()
    IF videoinfo = NULL THEN
      debug "SDL_GetVideoInfo failed: " & *SDL_GetError()
    ELSE
      screen_size = XY(videoinfo->current_w, videoinfo->current_h)
      debuginfo "SDL: screen size " & screen_size.wh
    END IF
  END IF
  ' This enables key repeat both for text input and for keys. We only
  ' want it for text input (only with --native-keybd), and otherwise filter out
  ' repeat keypresses.
  ' However, we still get key repeats, apparently from Windows, even if SDL
  ' keyrepeat is disabled (see SDL_KEYDOWN handling).
  SDL_EnableKeyRepeat(400, 50)

  'Clear keyboard state because if we re-initialise the backend (switch backend)
  'some key-up events can easily get lost
  'However, there's also a very rare problem after switching to gfx_sdl,
  'where all keys are dead. Seen once each by TMC (Linux?) and Gaplan (Windows).
  'Also doesn't fix sdl -> fb -> sdl typically crashing on linux/X11/KDE (when starting up with sdl)
  memset(@keybdstate(0), 0, (UBOUND(keybdstate) + 1) * SIZEOF(keybdstate(0)))

  ' Needed for X11 clipboard
  SDL_EventState(SDL_SYSWMEVENT, SDL_ENABLE)

  *info_buffer = *info_buffer & " (" & SDL_NumJoysticks() & " joysticks) Driver:"
  SDL_VideoDriverName(info_buffer + LEN(*info_buffer), info_buffer_size - LEN(*info_buffer))

#IFDEF __FB_ANDROID__
  IF SDL_ANDROID_IsRunningOnConsole() THEN
    debuginfo "Running on a console, disable the virtual gamepad"
    internal_disable_virtual_gamepad
  ELSE
    debuginfo "Not running on a console, leave the virtual gamepad visible"
  END IF
#ENDIF

  RETURN gfx_sdl_set_screen_mode()
END FUNCTION

'Create or modify the window with SDL_SetVideoMode, which is necessary to change
'nearly anything, such as window size. Returns true on success.
LOCAL FUNCTION gfx_sdl_set_screen_mode(bitdepth as integer = 0, quiet as bool = NO) as bool
  DIM flags as Uint32 = 0
  IF resizable THEN flags = flags OR SDL_RESIZABLE
  IF windowedmode = NO THEN
    flags = flags OR SDL_FULLSCREEN
  END IF
  IF always_force_video_reset OR force_video_reset THEN
    'Sometimes need to quit and reinit the video subsystem for changes to take effect
    force_video_reset = NO
    IF SDL_WasInit(SDL_INIT_VIDEO) THEN
      quit_video_subsystem()
      IF SDL_InitSubSystem(SDL_INIT_VIDEO) THEN
        debug "Can't start SDL video subsys (resize): " & *SDL_GetError
      END IF
    END IF
  END IF

  DIM creating_window as bool  'As opposed to recreating
  IF screensurface = NULL THEN creating_window = YES

#IFDEF __FB_ANDROID__
  'On Android, the requested screen size will be stretched.
  'We also want the option of a margin around the edges for
  'when the game is being played on a TV that needs safe zones

  IF smooth THEN
   'smoothing is enabled, use default zoom of 2 (or the zoom specified on the command line)
  ELSE
   'smoothing is disabled, force zoom to 1 on Android
   zoom = 1
  END IF
  
  DIM android_screen_size as XYPair
  android_screen_size.x = (framesize.w + INT(CDBL(framesize.w) * (safe_zone_margin * 2.0))) * zoom
  android_screen_size.y = (framesize.h + INT(CDBL(framesize.h) * (safe_zone_margin * 2.0))) * zoom
  screensurface = SDL_SetVideoMode(android_screen_size.x, android_screen_size.y, bitdepth, flags)
  IF screensurface = NULL THEN
    debug "Failed to open display (bitdepth = " & bitdepth & ", flags = " & flags & "): " & *SDL_GetError()
    RETURN NO
  END IF
  debuginfo "gfx_sdl: screen size is " & screensurface->w & "*" & screensurface->h
  WITH dest_rect
    .x = INT(CDBL(framesize.w) * safe_zone_margin) * zoom
    .y = INT(CDBL(framesize.h) * safe_zone_margin) * zoom
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
    IF quiet = NO THEN
      debuginfo "setvideomode zoom=" & zoom & " w*h = " & framesize*zoom & " resizable=" & resizable & " windowed=" & windowedmode
    END IF
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
      RETURN NO
    END IF
    EXIT DO
  LOOP
  'Don't recenter the window as the user resizes it
  '  putenv("SDL_VIDEO_CENTERED=0") does not work because SDL only tests whether the variable is defined
  'Note: on OSX unfortunately SDL will always recenter the window if its resizability changes, and the only
  'way to override that is to set SDL_VIDEO_WINDOW_POS.
#IFDEF __FB_WIN32__
  putenv("SDL_VIDEO_CENTERED=")
#ELSE
  unsetenv("SDL_VIDEO_CENTERED")
#ENDIF

#ENDIF  ' Not __FB_ANDROID__

  'Workaround for apparent SDL bug (see gfx_sdl_present_internal)
  lastwindowsize = XY(screensurface->w, screensurface->h)

  'Sanity check fullscreen state
  DIM is_windowed as bool = ((screensurface->flags AND SDL_FULLSCREEN) = 0)
  IF windowedmode <> is_windowed THEN
    debuginfo "SDL_SetVideoMode failed to set windowedmode=" & windowedmode
    windowedmode = is_windowed
    quiet = NO
  END IF

  WITH *screensurface->format
    screensurface_is_RGBColor = (.Rmask = RGB_Rmask andalso .Gmask = RGB_Gmask andalso .Bmask = RGB_Bmask)

    IF quiet = NO THEN
      debuginfo strprintf("gfx_sdl: created screensurface size=%d*%d depth=%d flags=%x R=%x G=%x B=%x A=%x is_RGBcolor=%d", _
                          screensurface->w, screensurface->h, .BitsPerPixel, screensurface->flags, _
                          .Rmask, .Gmask, .Bmask, .Amask, screensurface_is_RGBColor)

    END IF
  END WITH

#IFDEF __FB_DARWIN__
  ' SDL on OSX forgets the Unicode input state after a setvideomode
  SDL_EnableUNICODE(IIF(remember_enable_textinput, 1, 0))
#ENDIF

  'There was an annoying pulseaudio bug on my system, where the engine tended to crash when
  'the window was resized. Helgrind revealed a race condition in pa_context_set_name,
  'called from SDL_WM_SetCaption. So don't call it unless necessary because it hasn't been
  'called before. (Actually even then, it's may not be necessary?)
  IF creating_window THEN
    SDL_WM_SetCaption(remember_windowtitle, remember_windowtitle)
  END IF

  update_mouse_visibility()
  'Update the clip rectangle
  update_mouserect()
  RETURN YES
END FUNCTION

LOCAL SUB quit_video_subsystem()
  IF screenbuffer <> NULL THEN SDL_FreeSurface(screenbuffer)
  screensurface = NULL
  screenbuffer = NULL
  SDL_QuitSubSystem(SDL_INIT_VIDEO)
END SUB

'Ensure screenbuffer exists and has this size/bitdepth
LOCAL SUB create_or_update_screenbuffer(size as XYPair, bitdepth as integer)
  IF screenbuffer THEN
    IF XY(screenbuffer->w, screenbuffer->h) <> size ORELSE screenbuffer->format->BitsPerPixel <> bitdepth THEN
      SDL_FreeSurface(screenbuffer)
      screenbuffer = NULL
    END IF
  END IF

  IF screenbuffer = NULL THEN
    'These masks are ignored if bitdepth=8
    screenbuffer = SDL_CreateRGBSurface(SDL_SWSURFACE, size.w, size.h, bitdepth, RGB_Rmask, RGB_Gmask, RGB_Bmask, 0)
    IF screenbuffer = NULL THEN
      debugc errDie, "gfx_sdl_present_internal: Failed to allocate screenbuffer, " & *SDL_GetError
    END IF
  END IF
END SUB

FUNCTION gfx_sdl_present_internal(byval raw as any ptr, byval imagesz as XYPair, byval bitdepth as integer) as integer
  'debuginfo "gfx_sdl_present_internal(w=" & w & ", h=" & h & ", bitdepth=" & bitdepth & ")"

  'variable resolution handling
  '(We also test the size of screensurface, because sometimes SDL resizes the
  'window without telling us with an event! This happens on X11 when dragging
  'the window, if we try to change window size mid-drag. In that case we try to
  'override the change, and don't set resize_requested.)
  '(screensurface may be NULL after switching back to gfx_sdl, I'm not sure why!)
  DIM windowsize as XYPair = XY(screensurface->w, screensurface->h)
  IF screensurface = NULL ORELSE framesize <> imagesz ORELSE windowsize <> framesize * zoom ORELSE windowsize <> lastwindowsize THEN
    'debuginfo "gfx_sdl_present_internal: framesize changing from " & framesize & " to " & imagesz
    framesize = imagesz
    'A bitdepth of 0 indicates 'same as previous, otherwise default (native)'. Not sure if it's best to use
    'a native or 8 bit screen surface when we're drawing 8 bit; simply going to preserve the status quo for now.
    'Silence debug output here, or we get a raft of resize messages when resizing the window w/ the mouse
    IF gfx_sdl_set_screen_mode(IIF(bitdepth = 8, 0, bitdepth), YES) = NO THEN RETURN 1
    IF screenbuffer THEN
      SDL_FreeSurface(screenbuffer)
      screenbuffer = NULL
    END IF
  END IF

  'Warning: Should not rely on zoom, imagesz, screenbuffer and screensurface (actual window size)
  'matching even after above block because zoom might be "out of date". I'm not aware of any case in
  'which that actually happens, but easily possible. E.g. changing zoom can cause a resize request
  'which the engine might not have processed yet.  bitdepth=8 path is OK due to the SDL_BlitSurface
  'in gfx_sdl_8bit_update_screen, bitdepth=32 path has a safety check.

  IF bitdepth = 8 THEN
    'We could conceivably either blit to screensurface (doing 8 bit -> display pixel format conversion) first
    'and then smooth it (since it's also a SW surface)
    'Or what we actually do: smoothzoom first to screenbuffer, with smoothzoomblit_8_to_8bit, and then blit to screensurface

    create_or_update_screenbuffer imagesz * zoom, 8

    smoothzoomblit_8_to_8bit(raw, screenbuffer->pixels, imagesz, screenbuffer->pitch, zoom, smooth)
    gfx_sdl_8bit_update_screen()

  ELSE
    '32 bit surface

    IF screensurface->format->BitsPerPixel <= 8 THEN  'Might get 16bit surface if that's all the HW supports?
      gfx_sdl_set_screen_mode(32)
    END IF
    IF screensurface = NULL THEN
      debug "gfx_sdl_present_internal: no screen!"
      RETURN 1
    END IF

    'Safety check to avoid a crash (see Warning above)
    IF screensurface->w < imagesz.w * zoom ORELSE screensurface->h < imagesz.h * zoom THEN
    'IF XY(screensurface->w, screensurface->h) \ zoom <> imagesz THEN
      debug "gfx_sdl_present_internal: bad zoom " & zoom & " imagesz " & imagesz & " screen size " & XY(screensurface->w, screensurface->h) & ", skipping draw"
      RETURN 1
    END IF

    IF screensurface_is_RGBColor THEN
      'Can avoid an extra copy to screenbuffer

      'smoothzoomblit takes the pitch in pixels, not bytes!
      smoothzoomblit_32_to_32bit(cast(RGBcolor ptr, raw), cast(uint32 ptr, screensurface->pixels), imagesz, screensurface->pitch \ 4, zoom, smooth)
    ELSE
      'Need to get SDL to convert from RGBColor to screen format
      create_or_update_screenbuffer imagesz * zoom, 32

      smoothzoomblit_32_to_32bit(cast(RGBcolor ptr, raw), cast(uint32 ptr, screenbuffer->pixels), imagesz, screenbuffer->pitch \ 4, zoom, smooth)

      IF SDL_BlitSurface(screenbuffer, NULL, screensurface, NULL) THEN
        debug "gfx_sdl_8bit_update_screen: SDL_BlitSurface failed: " & *SDL_GetError
      END IF
    END If

    IF SDL_Flip(screensurface) THEN
      debug "gfx_sdl_present_internal: SDL_Flip failed: " & *SDL_GetError
    END IF
    update_state()
  END IF

  RETURN 0
END FUNCTION

FUNCTION gfx_sdl_present(byval surfaceIn as Surface ptr, byval pal as RGBPalette ptr) as integer
  WITH *surfaceIn
    IF .format = SF_8bit AND pal <> NULL THEN
      FOR i as integer = 0 TO 255
        sdlpalette(i).r = pal->col(i).r
        sdlpalette(i).g = pal->col(i).g
        sdlpalette(i).b = pal->col(i).b
      NEXT
    END IF
    RETURN gfx_sdl_present_internal(.pColorData, .size, IIF(.format = SF_8bit, 8, 32))
  END WITH
END FUNCTION

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
  DIM i as integer
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

SUB gfx_sdl_setwindowed(byval towindowed as bool)
#IFDEF __FB_DARWIN__
  IF towindowed = NO THEN
    'Low resolution looks bad in fullscreen, so change zoom temporarily
    IF zoom_has_been_changed = NO THEN
      remember_zoom = zoom
      zoom = large(zoom, 4)  'Rather crude
    END IF
  ELSE
    'Change zoom back?
    IF remember_zoom <> -1 AND zoom_has_been_changed = NO THEN
      zoom = remember_zoom
    END IF
  END IF
#ENDIF
  IF towindowed = 0 THEN
    windowedmode = NO
  ELSE
    windowedmode = YES
  END IF
  gfx_sdl_set_screen_mode()
  IF screensurface = NULL THEN
   debuginfo "setwindowed: fallback to previous zoom"
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
  state.structsize = WINDOWSTATE_SZ
  DIM temp as integer = SDL_GetAppState()
  state.focused = (temp AND SDL_APPINPUTFOCUS) <> 0
  state.minimised = (temp AND SDL_APPACTIVE) = 0
  state.fullscreen = (windowedmode = 0)
  state.mouse_over = (temp AND SDL_APPMOUSEFOCUS) <> 0
  state.windowsize = XY(screensurface->w, screensurface->h)
  state.zoom = zoom
  'SDL 1.2 has no way to check whether the window is maximised aside from
  'using the raw WM window handle, or guessing from the window size
  state.maximised = (screensurface->w = screen_size.w)
  RETURN @state
END FUNCTION

SUB gfx_sdl_get_screen_size(wide as integer ptr, high as integer ptr)
  'SDL only lets you check screen resolution before you've created a window.
  *wide = screen_size.w
  *high = screen_size.h
END SUB

FUNCTION gfx_sdl_supports_variable_resolution() as bool
  'Safe even in fullscreen, I think
  RETURN YES
END FUNCTION

FUNCTION gfx_sdl_vsync_supported() as bool
  #IFDEF __FB_DARWIN__
    ' OSX always has vsync, and drawing the screen will block until vsync, so this needs
    ' special treatment (as opposed to most other WMs which also do vsync compositing)
    RETURN YES
  #ELSE
    RETURN NO
  #ENDIF
END FUNCTION

FUNCTION gfx_sdl_set_resizable(byval enable as bool, min_width as integer, min_height as integer) as bool
  'Ignore minimum width and height.
  'See SDL_VIDEORESIZE handling for discussing of enforcing min window size.

  resizable = enable
  gfx_sdl_set_screen_mode()
  IF screensurface THEN
    RETURN (screensurface->flags AND SDL_RESIZABLE) <> 0
  END IF
  RETURN NO
END FUNCTION

FUNCTION gfx_sdl_get_resize(byref ret as XYPair) as bool
  IF resize_requested THEN
    ret = resize_request
    resize_requested = NO
    RETURN YES
  END IF
  RETURN NO
END FUNCTION

'Interesting behaviour: under X11+KDE, if the window doesn't go over the screen edges and is resized
'larger (SDL_SetVideoMode), then it will automatically be moved to fit onscreen (if you DON'T ask for recenter).
SUB gfx_sdl_recenter_window_hint()
  'Takes effect at the next SDL_SetVideoMode call, and it's then removed
  debuginfo "recenter_window_hint()"

  ' SDL_VIDEO_CENTERED has no effect on Mac (Quartz backend); the window is always
  ' centred unless SDL_VIDEO_WINDOW_POS is in effect.

  'IF running_under_Custom = NO THEN   'Don't display the window straight on top of Custom's
    putenv("SDL_VIDEO_CENTERED=1")
  'END IF

#IFDEF __FB_WIN32__
  'Under Windows SDL_VIDEO_CENTERED only has an effect when the window is recreated, which happens if
  'the resolution (and probably other settings) change. So force recreating by quitting and restarting
  'the video subsystem
  force_video_reset = YES
#ENDIF
END SUB

'change_windowsize = NO: Results in (eventually) changing framesize (set_scale_factor())
'change_windowsize = YES: Change the actual window size instead of framesize.
SUB gfx_sdl_set_zoom(newzoom as integer, change_windowsize as bool)
  IF newzoom >= 1 AND newzoom <= 16 AND newzoom <> zoom THEN
    zoom = newzoom
    IF screensurface = NULL THEN EXIT SUB
    zoom_has_been_changed = YES
    debuginfo "set_zoom change_windowsize=" & change_windowsize & ", zoom=" & zoom _
              & " old size = " & XY(screensurface->w, screensurface->h)
    IF change_windowsize THEN
      gfx_sdl_recenter_window_hint()  'Recenter because the window might go off the screen edge.
      'Unlike gfx_sdl_set_window_size, we immediately recreate the window, because framesize
      'is not expected to change on the next present. But ideally we wouldn't do this.
      gfx_sdl_set_screen_mode()
    ELSE
      'Keep window size the same
      resize_request.w = screensurface->w \ zoom
      resize_request.h = screensurface->h \ zoom
      resize_requested = YES
    END IF
  END IF
END SUB

'Change the window size and scale at the same time. an alternative to calling gfx_present with a resized frame.
'(Unlike gfx_sdl2_set_window_size, it doesn't immediately resize the window, but waits for the next gfx_present.
'This way is possibly more correct.)
SUB gfx_sdl_set_window_size (byval newframesize as XYPair = XY(-1,-1), newzoom as integer = -1)
  IF newframesize.w <= 0 THEN newframesize = framesize
  IF newzoom < 1 ORELSE newzoom > 16 THEN newzoom = zoom

  IF newframesize <> framesize THEN
    framesize = newframesize
    resize_request = newframesize
    resize_requested = YES
  END IF

  IF newzoom <> zoom THEN
    zoom = newzoom
    gfx_sdl_recenter_window_hint()  'Recenter because it's pretty ugly to go from centered to uncentered
  END IF

  IF newzoom <> zoom ORELSE newframesize <> framesize THEN
    debuginfo "set_window_size " & newframesize & ", zoom=" & newzoom
    'gfx_sdl_set_screen_mode()
  END IF
END SUB

FUNCTION gfx_sdl_setoption(byval opt as zstring ptr, byval arg as zstring ptr) as integer
  'debuginfo "gfx_sdl_setoption " & *opt & " " & *arg
  DIM ret as integer = 0
  DIM value as integer = str2int(*arg, -1)
  IF *opt = "zoom" or *opt = "z" THEN
    gfx_sdl_set_zoom(value, YES)
    ret = 1
  ELSEIF *opt = "smooth" OR *opt = "s" THEN
    IF value = 1 OR value = -1 THEN  'arg optional (-1)
      smooth = 1
    ELSE
      smooth = 0
    END IF
    ret = 1
  ELSEIF *opt = "reset-videomode" THEN
    always_force_video_reset = YES
    ret = 1
  END IF
  'all these take an optional numeric argument, so gobble the arg if it is
  'a number, whether or not it was valid
  IF ret = 1 AND parse_int(*arg) THEN ret = 2
  RETURN ret
END FUNCTION

FUNCTION gfx_sdl_describe_options() as zstring ptr
  return @"-z -zoom [1...16]   Scale screen to 1,2, ... up to 16x normal size (2x default)" LINE_END _
          "-s -smooth          Enable smoothing filter for zoom modes (default off)" LINE_END _
          "-reset-videomode    Reset SDL video subsys when changing video mode; may work around problems"
END FUNCTION

FUNCTION gfx_sdl_get_safe_zone_margin() as single
 RETURN safe_zone_margin
END FUNCTION

SUB gfx_sdl_set_safe_zone_margin(margin as single)
 safe_zone_margin = margin
 gfx_sdl_set_screen_mode()
END SUB

FUNCTION gfx_sdl_supports_safe_zone_margin() as bool
#IFDEF __FB_ANDROID__
 RETURN YES
#ELSE
 RETURN NO
#ENDIF
END FUNCTION

SUB gfx_sdl_ouya_purchase_request(dev_id as string, identifier as string, key_der as string)
#IFDEF __FB_ANDROID__
 SDL_ANDROID_SetOUYADeveloperId(dev_id)
 SDL_ANDROID_OUYAPurchaseRequest(identifier, key_der, LEN(key_der))
#ENDIF
END SUB

FUNCTION gfx_sdl_ouya_purchase_is_ready() as bool
#IFDEF __FB_ANDROID__
 RETURN SDL_ANDROID_OUYAPurchaseIsReady() <> 0
#ENDIF
 RETURN YES
END FUNCTION

FUNCTION gfx_sdl_ouya_purchase_succeeded() as bool
#IFDEF __FB_ANDROID__
 RETURN SDL_ANDROID_OUYAPurchaseSucceeded() <> 0
#ENDIF
 RETURN NO
END FUNCTION

SUB gfx_sdl_ouya_receipts_request(dev_id as string, key_der as string)
debuginfo "gfx_sdl_ouya_receipts_request"
#IFDEF __FB_ANDROID__
 SDL_ANDROID_SetOUYADeveloperId(dev_id)
 SDL_ANDROID_OUYAReceiptsRequest(key_der, LEN(key_der))
#ENDIF
END SUB

FUNCTION gfx_sdl_ouya_receipts_are_ready() as bool
#IFDEF __FB_ANDROID__
 RETURN SDL_ANDROID_OUYAReceiptsAreReady() <> 0
#ENDIF
 RETURN YES
END FUNCTION

FUNCTION gfx_sdl_ouya_receipts_result() as string
#IFDEF __FB_ANDROID__
 DIM zresult as zstring ptr
 zresult = SDL_ANDROID_OUYAReceiptsResult()
 DIM result as string = *zresult
 RETURN result
#ENDIF
 RETURN ""
END FUNCTION

SUB io_sdl_init

#IFDEF __FB_ANDROID__
 'Disable all four multitouch gestures, because they are kind of a mess
 'and it is virtually impossible to do two-finger drags (right mouse drags)
 'without accidentaly triggering them at random.
 
 DIM key as integer = 311 ' This is equivalent to SDLK_LSUPER which we use
                          ' because it should do absolutely nothing on
                          ' an android device. (using 0 to disable does
                          ' not work because it is interpreted as ESC)
  
 FOR gesture_id as integer = 0 to 3 
  SDL_ANDROID_SetKeymapKeyMultitouchGesture(gesture_id, key)
 NEXT gesture_id
#ENDIF
END SUB

LOCAL SUB keycombos_logic(evnt as SDL_Event)
  'Check for platform-dependent key combinations

  IF evnt.key.keysym.mod_ AND KMOD_ALT THEN
    IF evnt.key.keysym.sym = SDLK_RETURN THEN  'alt-enter (not processed normally when using SDL)
      gfx_sdl_setwindowed(windowedmode XOR YES)
      post_event(eventFullscreened, windowedmode = NO)
    END IF
    IF evnt.key.keysym.sym = SDLK_F4 THEN  'alt-F4
      post_terminate_signal
    END IF
  END IF

#IFDEF __FB_DARWIN__
  'We have to handle menu item key combinations here: SDLMain.m only handles the case that you actually click on them
  '(many of those actually generate an SDL keypress event, which is then handled here)
  'Note: these can NOT be handled in allmodex because the modifier keys won't appear to be pressed there,
  'no events are generated to fake those keypresses.

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
      post_event(eventFullscreened, windowedmode = NO)
      ' Includes Cmd+F to fullscreen
    END IF
    'SDL doesn't actually seem to send SDLK_QUESTION...
    IF evnt.key.keysym.sym = SDLK_SLASH AND evnt.key.keysym.mod_ AND KMOD_SHIFT THEN
      keybdstate(scF1) = 2
    END IF
    FOR i as integer = 1 TO 4
      IF evnt.key.keysym.sym = SDLK_0 + i THEN
        #IFDEF IS_CUSTOM
          set_scale_factor i, NO
        #ELSE
          set_scale_factor i, YES
        #ENDIF
      END IF
    NEXT
  END IF
#ENDIF

END SUB

SUB gfx_sdl_process_events()
'The SDL event queue only holds 128 events, after which SDL_QuitEvents will be lost
'Of course, we might actually like to do something with some of the other events
  DIM evnt as SDL_Event
  'I assume this uses SDL_PeepEvents instead of SDL_PollEvent because the latter calls SDL_PumpEvents
  WHILE SDL_PeepEvents(@evnt, 1, SDL_GETEVENT, SDL_ALLEVENTS)
    SELECT CASE evnt.type
      CASE SDL_QUIT_
        IF debugging_io THEN
          debuginfo "SDL_QUIT"
        END IF
        post_terminate_signal
      CASE SDL_KEYDOWN
        keycombos_logic(evnt)
        DIM as integer key = scantrans(evnt.key.keysym.sym)
        IF LEN(input_buffer) >= 127 THEN input_buffer = RIGHT(input_buffer, 126)
        input_buffer += WCHR(evnt.key.keysym.unicode_)
        IF debugging_io THEN
          debuginfo "SDL_KEYDOWN " & evnt.key.keysym.sym & " -> scan=" & key & " (" & scancodename(key) & ") char=" & evnt.key.keysym.unicode_ & " prev_keystate=" & keybdstate(key)
        END IF
        IF key THEN
          'Filter out key repeats (key already down, or we just saw a keyup):
          'On Windows (XP at least) we get key repeats even if we don't enable
          'SDL's key repeats, but with a much longer initial delay than the SDL ones.
          'SDL repeats keys by sending extra KEYDOWNs, while Windows sends keyup-keydown
          'pairs. Unfortunately for some reason we don't always get the keydown until
          'the next tick, so that it doesn't get filtered out.
          'gfx_fb suffers the same problem.
          IF keybdstate(key) = 0 THEN keybdstate(key) OR= 2  'new keypress
          keybdstate(key) OR= 1  'key down
        END IF
      CASE SDL_KEYUP
        DIM as integer key = scantrans(evnt.key.keysym.sym)
        IF debugging_io THEN
          debuginfo "SDL_KEYUP " & evnt.key.keysym.sym & " -> scan=" & key & " (" & scancodename(key) & ") char=" & evnt.key.keysym.unicode_ & " prev_keystate=" & keybdstate(key)
        END IF
        'Clear 2nd bit (new keypress) and turn on 3rd bit (keyup)
        IF key THEN keybdstate(key) = (keybdstate(key) AND 2) OR 4
      CASE SDL_MOUSEBUTTONDOWN
        'note SDL_GetMouseState is still used, while SDL_GetKeyState isn't
        'Interestingly, although (on Linux/X11) SDL doesn't report mouse motion events
        'if the window isn't focused, it does report mouse wheel button events
        '(other buttons focus the window).
        WITH evnt.button
          mouseclicks OR= SDL_BUTTON(.button)
          IF .button = SDL_BUTTON_WHEELUP THEN mousewheel += 120
          IF .button = SDL_BUTTON_WHEELDOWN THEN mousewheel -= 120
          IF debugging_io THEN
            debuginfo "SDL_MOUSEBUTTONDOWN mouse " & .which & " button " & .button & " at " & .x & "," & .y
          END IF
        END WITH
      CASE SDL_MOUSEBUTTONUP
        WITH evnt.button
          IF debugging_io THEN
            debuginfo "SDL_MOUSEBUTTONUP   mouse " & .which & " button " & .button & " at " & .x & "," & .y
          END IF
        END WITH

#IFDEF __FB_ANDROID__
      'SDL_WarpMouse doesn't work, this is part of workaround
      CASE SDL_MOUSEMOTION : warped_mouse = NO
#ENDIF

      CASE SDL_ACTIVEEVENT
        IF evnt.active.state AND SDL_APPINPUTFOCUS THEN
          IF debugging_io THEN
            debuginfo "SDL_ACTIVEEVENT state=" & evnt.active.state & " gain=" & evnt.active.gain
          END IF
          IF evnt.active.gain = 0 THEN
            SDL_ShowCursor(1)
            IF mouseclipped THEN
              SDL_WarpMouse privatemx, privatemy
              SDL_PumpEvents
            END IF
          ELSE
            update_mouse_visibility()
            IF mouseclipped THEN
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
        IF debugging_io THEN
          debuginfo "SDL_VIDEORESIZE: w=" & evnt.resize.w & " h=" & evnt.resize.h
        END IF
        IF resizable THEN
          'Round upwards
          resize_request.w = (evnt.resize.w + zoom - 1) \ zoom
          resize_request.h = (evnt.resize.h + zoom - 1) \ zoom
          IF framesize <> resize_request THEN
            'On Windows (XP), changing the window size causes an SDL_VIDEORESIZE event
            'to be sent with the size you just set... this would produce annoying overlay
            'messages in screen_size_update() if we don't filter them out.
            resize_requested = YES
          END IF
          'Nothing happens until the engine calls gfx_get_resize,
          'changes its internal window size (windowsize) as a result,
          'and starts pushing Frames with the new size to gfx_present.

          'Calling SDL_SetVideoMode changes the window size.  Unfortunately it's not possible
          'to reliably override a user resize event with a different window size, at least with
          'X11+KDE, because the window size isn't changed by SDL_SetVideoMode while the user is
          'still dragging the window, and as far as I can tell there is no way to tell what the
          'actual window size is, or whether the user still has the mouse button down while
          'resizing (it isn't reported); usually they do hold it down until after they've
          'finished moving their mouse.  One possibility would be to hook into X11, or to do
          'some delayed SDL_SetVideoMode calls.

          'Similarly, there's an SDL bug (I've only seen it on X11+KDE), where sometimes a
          'split second after changing the window size it will revert to its original size
          'if the window is resizable.
        END IF

      CASE SDL_SYSWMEVENT
        'On X11, it's necessary to handle SelectionRequest events in order to be able
        'to "copy to the clipboard" (there is no global clipboard; we declare we have the
        'current selection and then other applications can request it).
        WITH *evnt.syswm.msg
          #IFDEF USE_X11
            IF .subsystem = SDL_SYSWM_X11 THEN
              DIM xeventp as XEvent ptr = @.event.xevent
              IF load_wminfo() THEN
                WITH wminfo.info.x11
                  .lock_func()
                  X11_HandleClipboardEvent(.display, xeventp)
                  .unlock_func()
                END WITH
              END IF
            END IF
          #ENDIF
        END WITH
    END SELECT
  WEND
END SUB

'may only be called from the main thread
LOCAL SUB update_state()
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

LOCAL SUB keymod_to_keybdstate(modstate as integer, key as KBScancode)
  keybdstate(key) = (keybdstate(key) AND 6) OR IIF(modstate, 1, 0)
END SUB

SUB io_sdl_keybits (byval keybdarray as KeyBits ptr)
  'keybdarray bits:
  ' bit 0 - key down
  ' bit 1 - new keypress event
  'keybdstate bits:
  ' bit 0 - key down
  ' bit 1 - new keypress event
  ' bit 2 - keyup event

  'Get numlock and capslock state
  #IFDEF __FB_WIN32__
    'On Windows, if numlock and capslock are on when the program starts, then SDL 1.2
    'reports them as off, and keeps them inverted from the real state from then on.
    'Work around this bug by using the winapi to get the real state.
    '(Note: we can't use SDL_SetModState() to fix the state because that triggers
    'another bug, it confuses SDL_PrivateKeyboard() into dropping all numlock/capslock
    'events and the keys become stuck)
    keymod_to_keybdstate GetKeyState(VK_NUMLOCK) AND 1, scNumlock
    keymod_to_keybdstate GetKeyState(VK_CAPITAL) AND 1, scCapslock
  #ELSE
    'On X11, similar problem except the state is always off to begin with, then
    'corrects after the first capslock/numlock keypress.
    'It's not an SDL bug, it's our bug - we initialise keybdstate to zero.
    'Could move the following code to gfx_sdl_init, but this is more robust.
    '(Note: gfx_sdl2 has code identical to the following, but for a different reason!)
    DIM kmod as integer = SDL_GetModState()
    keymod_to_keybdstate kmod AND KMOD_NUM,  scNumlock
    keymod_to_keybdstate kmod AND KMOD_CAPS, scCapslock
  #ENDIF

  DIM msg as string
  FOR a as KBScancode = 0 TO &h7f
    keybdstate(a) = keybdstate(a) and 3  'Clear key-up bit
    keybdarray[a] = keybdstate(a)
    IF debugging_io ANDALSO keybdarray[a] THEN
      msg &= "  key[" & a & "](" & scancodename(a) & ")=" & keybdarray[a]
    END IF
    keybdstate(a) = keybdstate(a) and 1  'Clear new-keypress bit
  NEXT
  IF LEN(msg) THEN debuginfo "io_sdl_keybits returning:" & msg

  keybdarray[scShift] = keybdarray[scLeftShift] OR keybdarray[scRightShift]
  keybdarray[scUnfilteredAlt] = keybdarray[scLeftAlt] OR keybdarray[scRightAlt]
  keybdarray[scCtrl] = keybdarray[scLeftCtrl] OR keybdarray[scRightCtrl]
END SUB

SUB io_sdl_updatekeys(byval keybd as KeyBits ptr)
  'supports io_keybits instead
END SUB

'Enabling unicode will cause combining keys to go dead on X11 (on non-US
'layouts that have them). This usually means certain punctuation keys such as '
'On both X11 and Windows, disabling unicode input means SDL_KEYDOWN events
'don't report the character value (.unicode_).
SUB io_sdl_enable_textinput (byval enable as integer)
  DIM oldstate as integer
  oldstate = SDL_EnableUNICODE(IIF(enable, 1, 0))
  remember_enable_textinput = enable  ' Needed only because of an SDL bug on OSX
  IF debugging_io THEN
    debuginfo "SDL_EnableUNICODE(" & enable & ") = " & oldstate & " (prev state)"
  END IF
END SUB

SUB io_sdl_textinput (byval buf as wstring ptr, byval bufsize as integer)
  'Both FB and SDL only support UCS2, which doesn't have variable len wchars.
  DIM buflen as integer = bufsize \ 2 - 1
  *buf = LEFT(input_buffer, buflen)
  IF debugging_io ANDALSO LEN(*buf) THEN
    debuginfo "io_sdl_textinput: " & *buf
  END IF
  input_buffer = MID(input_buffer, buflen)
END SUB

SUB io_sdl_show_virtual_keyboard()
 'Does nothing on platforms that have real keyboards
#IFDEF __FB_ANDROID__
 if not virtual_keyboard_shown then
  SDL_ANDROID_ToggleScreenKeyboardWithoutTextInput()
  virtual_keyboard_shown = YES
 end if
#ENDIF
END SUB

SUB io_sdl_hide_virtual_keyboard()
 'Does nothing on platforms that have real keyboards
#IFDEF __FB_ANDROID__
 if virtual_keyboard_shown then
  SDL_ANDROID_ToggleScreenKeyboardWithoutTextInput()
  virtual_keyboard_shown = NO
 end if
#ENDIF
END SUB

SUB io_sdl_show_virtual_gamepad()
 'Does nothing on other platforms
#IFDEF __FB_ANDROID__
 if allow_virtual_gamepad then
  SDL_ANDROID_SetScreenKeyboardShown(YES)
 else
  debuginfo "io_sdl_show_virtual_gamepad was supressed because of a previous call to internal_disable_virtual_gamepad"
 end if
#ENDIF
END SUB

SUB io_sdl_hide_virtual_gamepad()
 'Does nothing on other platforms
#IFDEF __FB_ANDROID__
 SDL_ANDROID_SetScreenKeyboardShown(NO)
#ENDIF
END SUB

LOCAL SUB internal_disable_virtual_gamepad()
 'Does nothing on other platforms
#IFDEF __FB_ANDROID__
 io_sdl_hide_virtual_gamepad
 allow_virtual_gamepad = NO
#ENDIF
END SUB

SUB io_sdl_remap_android_gamepad(byval player as integer, gp as GamePadMap)
'Does nothing on non-android
#IFDEF __FB_ANDROID__
 SELECT CASE player
  CASE 0
   SDL_ANDROID_set_java_gamepad_keymap ( _
    scOHR2SDL(gp.A, SDLK_RETURN), _
    scOHR2SDL(gp.B, SDLK_ESCAPE), _
    0, _
    scOHR2SDL(gp.X, SDLK_ESCAPE), _
    scOHR2SDL(gp.Y, SDLK_ESCAPE), _
    0, _
    scOHR2SDL(gp.L1, SDLK_PAGEUP), _
    scOHR2SDL(gp.R1, SDLK_PAGEDOWN), _
    scOHR2SDL(gp.L2, SDLK_HOME), _
    scOHR2SDL(gp.R2, SDLK_END), _
    0, 0)
  CASE 1 TO 3
    SDL_ANDROID_set_ouya_gamepad_keymap ( _
    player, _
    scOHR2SDL(gp.Ud, SDLK_UP), _
    scOHR2SDL(gp.Rd, SDLK_RIGHT), _
    scOHR2SDL(gp.Dd, SDLK_DOWN), _
    scOHR2SDL(gp.Ld, SDLK_LEFT), _
    scOHR2SDL(gp.A, SDLK_RETURN), _
    scOHR2SDL(gp.B, SDLK_ESCAPE), _
    scOHR2SDL(gp.X, SDLK_ESCAPE), _
    scOHR2SDL(gp.Y, SDLK_ESCAPE), _
    scOHR2SDL(gp.L1, SDLK_PAGEUP), _
    scOHR2SDL(gp.R1, SDLK_PAGEDOWN), _
    scOHR2SDL(gp.L2, SDLK_HOME), _
    scOHR2SDL(gp.R2, SDLK_END), _
    0, 0)
  CASE ELSE
   debug "WARNING: io_sdl_remap_android_gamepad: invalid player number " & player
 END SELECT
#ENDIF
END SUB

SUB io_sdl_remap_touchscreen_button(byval button_id as integer, byval ohr_scancode as integer)
'Pass a scancode of 0 to disabled/hide the button
'Does nothing on non-android
#IFDEF __FB_ANDROID__
 SDL_ANDROID_SetScreenKeyboardButtonDisable(button_id, (ohr_scancode = 0))
 SDL_ANDROID_SetScreenKeyboardButtonKey(button_id, scOHR2SDL(ohr_scancode, 0))
#ENDIF
END SUB

FUNCTION io_sdl_running_on_console() as bool
#IFDEF __FB_ANDROID__
 RETURN SDL_ANDROID_IsRunningOnConsole()
#ENDIF
 RETURN NO
END FUNCTION

FUNCTION io_sdl_running_on_ouya() as bool
#IFDEF __FB_ANDROID__
 RETURN SDL_ANDROID_IsRunningOnOUYA()
#ENDIF
 RETURN NO
END FUNCTION

LOCAL SUB update_mouse_visibility()
  DIM vis as integer
  IF mouse_visibility = cursorDefault THEN
    IF windowedmode THEN vis = 1 ELSE vis = 0
  ELSEIF mouse_visibility = cursorVisible THEN
    vis = 1
  ELSE
    vis = 0
  END IF
  SDL_ShowCursor(vis)
#IFDEF __FB_DARWIN__
  'Force clipping in fullscreen, and undo when leaving, because you
  'can move the cursor to the screen edge, where it will be visible
  'regardless of whether SDL_ShowCursor is used.
  set_forced_mouse_clipping (windowedmode = NO AND vis = 0)
#ENDIF
END SUB

SUB io_sdl_setmousevisibility(visibility as CursorVisibility)
  mouse_visibility = visibility
  update_mouse_visibility()
END SUB

'Change from SDL to OHR mouse button numbering (swap middle and right)
LOCAL FUNCTION fix_buttons(byval buttons as integer) as integer
  DIM mbuttons as integer = 0
  IF SDL_BUTTON(SDL_BUTTON_LEFT) AND buttons THEN mbuttons = mbuttons OR mouseLeft
  IF SDL_BUTTON(SDL_BUTTON_RIGHT) AND buttons THEN mbuttons = mbuttons OR mouseRight
  IF SDL_BUTTON(SDL_BUTTON_MIDDLE) AND buttons THEN mbuttons = mbuttons OR mouseMiddle
  RETURN mbuttons
END FUNCTION

' Returns currently down mouse buttons, in SDL order, not OHR order
LOCAL FUNCTION update_mouse() as integer
  DIM x as int32
  DIM y as int32
  DIM buttons as Uint8

  buttons = SDL_GetMouseState(@x, @y)
#IFDEF __FB_ANDROID__
  'SDL_WarpMouse doesn't work, so reimplement it: don't update mouse position until it's moved/clicked
  IF buttons <> 0 THEN warped_mouse = NO
  IF warped_mouse THEN RETURN buttons
#ENDIF
  IF SDL_GetAppState() AND SDL_APPINPUTFOCUS THEN
    IF mouseclipped THEN
      'Not moving the mouse back to the centre of the window rapidly is widely recommended, but I haven't seen (nor looked for) evidence that it's bad.
      'Implemented only due to attempting to fix eventually unrelated problem. Possibly beneficial to keep
      'debuginfo "gfx_sdl: mousestate " & x & " " & y & " (" & lastmx & " " & lastmy & ")"  'Very spammy
      privatemx += x - lastmx
      privatemy += y - lastmy
      IF x < 3 * screensurface->w \ 8 OR x > 5 * screensurface->w \ 8 OR _
         y < 3 * screensurface->h \ 8 OR y > 5 * screensurface->h \ 8 THEN
        SDL_WarpMouse screensurface->w \ 2, screensurface->h \ 2
        'Required after warping the mouse for it to take effect. Discovered with much blood, sweat, and murderous rage
        SDL_PumpEvents
        lastmx = screensurface->w \ 2
        lastmy = screensurface->h \ 2
        IF debugging_io THEN
          debuginfo "gfx_sdl: clipped mouse warped"
        END IF
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

  mwheel = mousewheel
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
#IFDEF __FB_ANDROID__
      ' SDL_WarpMouse doesn't work
      warped_mouse = YES
      privatemx = x * zoom
      privatemy = y * zoom
#ENDIF
#IFDEF __FB_DARWIN__
      ' SDL Mac bug (SDL 1.2.14, OS 10.8.5): if the cursor is off the window
      ' when SDL_WarpMouse is called then the mouse gets moved onto the window,
      ' but SDL forgets to hide the cursor if it was previously requested, and further,
      ' SDL_ShowCursor(0) does nothing because SDL thinks it's already hidden.
      ' So call SDL_ShowCursor twice in a row as workaround.
      SDL_ShowCursor(1)
      update_mouse_visibility()
#ENDIF
    END IF
  END IF
END SUB

LOCAL SUB internal_set_mouserect(rect as RectPoints)
  IF mouseclipped = NO AND (rect.p1.x >= 0) THEN
    'enter clipping mode
    'SDL_WM_GrabInput causes most WM key combinations to be blocked, which I find unacceptable, so instead
    'we stick the mouse at the centre of the window. It's a very common hack.
    mouseclipped = YES
    SDL_GetMouseState(@privatemx, @privatemy)
    IF SDL_GetAppState() AND SDL_APPINPUTFOCUS THEN
      SDL_WarpMouse screensurface->w \ 2, screensurface->h \ 2
      SDL_PumpEvents
    END IF
    lastmx = screensurface->w \ 2
    lastmy = screensurface->h \ 2
  ELSEIF mouseclipped = YES AND (rect.p1.x = -1) THEN
    'exit clipping mode
    mouseclipped = NO
    SDL_WarpMouse privatemx, privatemy
  END IF
  mxmin = rect.p1.x * zoom
  mxmax = rect.p2.x * zoom + zoom - 1
  mymin = rect.p1.y * zoom
  mymax = rect.p2.y * zoom + zoom - 1
END SUB

'Update the mouse clip rectangle, either because it changed (or was enabled/disabled) or the window size changed
LOCAL SUB update_mouserect()
  IF remember_mouserect.p1.x > -1 THEN
    internal_set_mouserect remember_mouserect
  ELSEIF forced_mouse_clipping THEN
    'We're now meant to be unclipped, but clip to the window
    internal_set_mouserect TYPE<RectPoints>((0, 0), framesize.w - 1, framesize.h - 1)
  ELSE
    'Unclipped: remember_mouserect == ((-1,-1),(-1,-1))
    internal_set_mouserect remember_mouserect
  END IF
END SUB

'This turns forced mouse clipping on or off.
'Used only on Mac in fullscreen to work around an SDL bug!
LOCAL SUB set_forced_mouse_clipping(byval newvalue as bool)
  newvalue = (newvalue <> 0)
  IF newvalue <> forced_mouse_clipping THEN
    forced_mouse_clipping = newvalue
    update_mouserect
  END IF
END SUB

SUB io_sdl_mouserect(byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
  'Should we clamp the rect?
  remember_mouserect = TYPE<RectPoints>((xmin, ymin), (xmax, ymax))
  update_mouserect
END SUB

LOCAL FUNCTION scOHR2SDL(byval ohr_scancode as integer, byval default_sdl_scancode as integer=0) as integer
 'Convert an OHR scancode into an SDL scancode
 '(the reverse can be accomplished just by using the scantrans array)
 IF ohr_scancode = 0 THEN RETURN default_sdl_scancode
 FOR i as integer = 0 TO UBOUND(scantrans)
  IF scantrans(i) = ohr_scancode THEN RETURN i
 NEXT i
 RETURN 0
END FUNCTION

'Loads the wminfo global, returns success
LOCAL FUNCTION load_wminfo() as bool
  SDL_VERSION_(@wminfo.version)
  IF SDL_GetWMInfo(@wminfo) <> 1 THEN RETURN NO
  #IFDEF USE_X11
    IF wminfo.subsystem <> SDL_SYSWM_X11 THEN RETURN NO
  #ENDIF
  'Other platforms don't have the equivalent of subsystem
  RETURN YES
END FUNCTION

SUB io_sdl_set_clipboard_text(text as zstring ptr)  'ustring
  IF text = NULL THEN text = @""
  #IFDEF USE_X11
    IF load_wminfo() = NO THEN EXIT SUB
    WITH wminfo.info.x11
      .lock_func()
      X11_SetClipboardText(.display, .window, text)
      .unlock_func()
    END WITH
  #ELSEIF DEFINED(__FB_WIN32__)
    IF load_wminfo() = NO THEN EXIT SUB
    WIN_SetClipboardText(wminfo.window, text)
  #ELSEIF DEFINED(__FB_DARWIN__)
    Cocoa_SetClipboardText(text)
  #ENDIF
END SUB

#IFDEF USE_X11
LOCAL SUB check_events()
  WITH wminfo.info.x11
    .unlock_func()
    update_state()
    .lock_func()
  END WITH
END SUB
#ENDIF

FUNCTION io_sdl_get_clipboard_text() as zstring ptr  'ustring
  DIM ret as zstring ptr
  #IFDEF USE_X11
    IF load_wminfo() = NO THEN RETURN NULL
    WITH wminfo.info.x11
      .lock_func()
      ret = X11_GetClipboardText(.display, .window, @check_events)
      .unlock_func()
    END WITH
  #ELSEIF DEFINED(__FB_WIN32__)
    IF load_wminfo() = NO THEN RETURN NULL
    ret = WIN_GetClipboardText(wminfo.window)
  #ELSEIF DEFINED(__FB_DARWIN__)
    ret = Cocoa_GetClipboardText()
  #ENDIF
  RETURN ret
END FUNCTION

FUNCTION gfx_sdl_setprocptrs() as integer
  gfx_init = @gfx_sdl_init
  gfx_close = @gfx_sdl_close
  gfx_getversion = @gfx_sdl_getversion
  gfx_setpal = @gfx_sdl_setpal
  gfx_screenshot = @gfx_sdl_screenshot
  gfx_setwindowed = @gfx_sdl_setwindowed
  gfx_windowtitle = @gfx_sdl_windowtitle
  gfx_getwindowstate = @gfx_sdl_getwindowstate
  gfx_get_screen_size = @gfx_sdl_get_screen_size
  gfx_set_window_size = @gfx_sdl_set_window_size
  gfx_supports_variable_resolution = @gfx_sdl_supports_variable_resolution
  gfx_vsync_supported = @gfx_sdl_vsync_supported
  gfx_get_resize = @gfx_sdl_get_resize
  gfx_set_resizable = @gfx_sdl_set_resizable
  gfx_recenter_window_hint = @gfx_sdl_recenter_window_hint
  gfx_setoption = @gfx_sdl_setoption
  gfx_describe_options = @gfx_sdl_describe_options
  gfx_get_safe_zone_margin = @gfx_sdl_get_safe_zone_margin
  gfx_set_safe_zone_margin = @gfx_sdl_set_safe_zone_margin
  gfx_supports_safe_zone_margin = @gfx_sdl_supports_safe_zone_margin
  gfx_ouya_purchase_request = @gfx_sdl_ouya_purchase_request
  gfx_ouya_purchase_is_ready = @gfx_sdl_ouya_purchase_is_ready
  gfx_ouya_purchase_succeeded = @gfx_sdl_ouya_purchase_succeeded
  gfx_ouya_receipts_request = @gfx_sdl_ouya_receipts_request
  gfx_ouya_receipts_are_ready = @gfx_sdl_ouya_receipts_are_ready
  gfx_ouya_receipts_result = @gfx_sdl_ouya_receipts_result
  io_init = @io_sdl_init
  io_pollkeyevents = @io_sdl_pollkeyevents
  io_waitprocessing = @io_sdl_waitprocessing
  io_keybits = @io_sdl_keybits
  io_updatekeys = @io_sdl_updatekeys
  io_enable_textinput = @io_sdl_enable_textinput
  io_textinput = @io_sdl_textinput
  io_get_clipboard_text = @io_sdl_get_clipboard_text
  io_set_clipboard_text = @io_sdl_set_clipboard_text
  io_show_virtual_keyboard = @io_sdl_show_virtual_keyboard
  io_hide_virtual_keyboard = @io_sdl_hide_virtual_keyboard
  io_show_virtual_gamepad = @io_sdl_show_virtual_gamepad
  io_hide_virtual_gamepad = @io_sdl_hide_virtual_gamepad
  io_remap_android_gamepad = @io_sdl_remap_android_gamepad
  io_remap_touchscreen_button = @io_sdl_remap_touchscreen_button
  io_running_on_console = @io_sdl_running_on_console
  io_running_on_ouya = @io_sdl_running_on_ouya
  io_mousebits = @io_sdl_mousebits
  io_setmousevisibility = @io_sdl_setmousevisibility
  io_getmouse = @io_sdl_getmouse
  io_setmouse = @io_sdl_setmouse
  io_mouserect = @io_sdl_mouserect
  io_get_joystick_state = @io_sdl_get_joystick_state

  gfx_present = @gfx_sdl_present

  RETURN 1
END FUNCTION


#include "gfx_sdl_common.bas"

END EXTERN
