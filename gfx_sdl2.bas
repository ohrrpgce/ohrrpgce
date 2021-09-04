'OHRRPGCE - SDL 2 graphics bakend
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#include "config.bi"

#ifdef __FB_WIN32__
	'In FB >= 1.04 SDL.bi includes windows.bi; we have to include it first to do the necessary conflict prevention
	include_windows_bi()
#endif

#include "crt.bi"
#include "gfx.bi"
#include "surface.bi"
#include "common.bi"
#include "allmodex.bi"  'For set_scale_factor
#include "unicode.bi"
#include "scancodes.bi"
'#define NEED_SDL_GETENV

#ifdef __FB_UNIX__
	'In FB >= 1.04 SDL.bi includes Xlib.bi; fix a conflict
	#undef font
#endif

#include "SDL2\SDL.bi"

EXTERN "C"

#define KMOD_META  KMOD_GUI  'Renamed in SDL2

'Older FB releases (before 1.06?) only have headers for SDL 2.0.3 (Mar 2014). Declaring these is
'simpler than requiring a recent FB. Whether they're in FB's headers doesn't tell whether they're on
'the system, but you can compile with "scons sdl203=1" to not depend on any of the following.
'(Might actually support SDL 2.0.1/2 too; I didn't check)
#ifndef SDL_GameControllerFromInstanceID
  'SDL 2.0.4+ (Jan 2016)
  declare function SDL_GameControllerFromInstanceID(byval joyid as SDL_JoystickID) as SDL_GameController ptr
#endif
#ifndef SDL_CaptureMouse
  'SDL 2.0.4+
  declare function SDL_CaptureMouse(byval enabled as SDL_bool) as long
#endif
#ifndef SDL_GetDisplayUsableBounds
  'SDL 2.0.5+ (Oct 2016)
  declare function SDL_GetDisplayUsableBounds(byval displayIndex as long, byval rect as SDL_Rect ptr) as long
#endif
#ifndef SDL_RenderSetIntegerScale
  'SDL 2.0.5+
  declare function SDL_RenderSetIntegerScale(byval renderer as SDL_Renderer ptr, byval enable as SDL_bool) as long
#endif
#ifndef SDL_SetWindowResizable
  'SDL 2.0.5+
  declare sub SDL_SetWindowResizable(byval window as SDL_Window ptr, byval resizable as SDL_bool)
#endif
#ifndef SDL_JoystickGetDeviceInstanceID
  'SDL 2.0.6+ (Sept 2017). Not used
  declare function SDL_JoystickGetDeviceInstanceID(byval device_index as long) as SDL_JoystickID
#endif


#define USE_SDL2
#include "gfx_sdl_common.bi"

#IFDEF __FB_ANDROID__
'This function shows/hides the sdl virtual gamepad
declare sub SDL_ANDROID_SetScreenKeyboardShown (byval shown as integer)
'This function toggles the display of the android virtual keyboard. always returns 1 no matter what
declare function SDL_ANDROID_ToggleScreenKeyboardWithoutTextInput() as integer 
'WARNING: SDL_ANDROID_IsScreenKeyboardShown seems unreliable. Don't use it! It is only declared here to document its existance. see the virtual_keyboard_shown variable instead
declare function SDL_ANDROID_IsScreenKeyboardShown() as bool
declare function SDL_ANDROID_IsRunningOnConsole () as bool
declare function SDL_ANDROID_IsRunningOnOUYA () as bool
declare sub SDL_ANDROID_set_java_gamepad_keymap(byval A as integer, byval B as integer, byval C as integer, byval X as integer, byval Y as integer, byval Z as integer, byval L1 as integer, byval R1 as integer, byval L2 as integer, byval R2 as integer, byval LT as integer, byval RT as integer)
declare sub SDL_ANDROID_set_ouya_gamepad_keymap(byval player as integer, byval udpad as integer, byval rdpad as integer, byval ldpad as integer, byval ddpad as integer, byval O as integer, byval A as integer, byval U as integer, byval Y as integer, byval L1 as integer, byval R1 as integer, byval L2 as integer, byval R2 as integer, byval LT as integer, byval RT as integer)
declare function SDL_ANDROID_SetScreenKeyboardButtonKey(byval buttonId as integer, byval key as integer) as integer
declare function SDL_ANDROID_SetScreenKeyboardButtonDisable(byval buttonId as integer, byval disable as bool) as integer
declare sub SDL_ANDROID_SetOUYADeveloperId (byval devId as zstring ptr)
declare sub SDL_ANDROID_OUYAPurchaseRequest (byval identifier as zstring ptr, byval keyDer as zstring ptr, byval keyDerSize as integer)
declare function SDL_ANDROID_OUYAPurchaseIsReady () as bool
declare function SDL_ANDROID_OUYAPurchaseSucceeded () as bool
declare sub SDL_ANDROID_OUYAReceiptsRequest (byval keyDer as zstring ptr, byval keyDerSize as integer)
declare function SDL_ANDROID_OUYAReceiptsAreReady () as bool
declare function SDL_ANDROID_OUYAReceiptsResult () as zstring ptr
#ENDIF

DECLARE FUNCTION recreate_window(byval bitdepth as integer = 0) as bool
DECLARE FUNCTION recreate_screen_texture() as bool
DECLARE FUNCTION get_buffersize() as XYPair
DECLARE SUB set_viewport()
DECLARE FUNCTION present_internal2(srcsurf as SDL_Surface ptr, raw as any ptr, imagesz as XYPair, pitch as integer, bitdepth as integer) as bool
DECLARE SUB update_state()
DECLARE FUNCTION update_mouse() as integer
DECLARE SUB update_mouse_visibility()
DECLARE SUB set_forced_mouse_clipping(byval newvalue as bool)
DECLARE SUB internal_set_mouserect(rect as RectPoints)
DECLARE SUB internal_disable_virtual_gamepad()
DECLARE FUNCTION scOHR2SDL(byval ohr_scancode as KBScancode, byval default_sdl_scancode as integer=0) as integer

DECLARE SUB log_error(failed_call as zstring ptr, funcname as zstring ptr)
#define CheckOK(condition, otherwise...)  IF condition THEN log_error(#condition, __FUNCTION__) : otherwise

DIM SHARED zoom as integer = 2  'Size of a pixel
DIM SHARED smooth_zoom as integer = 2  'Amount to zoom before applying smoothing
DIM SHARED smooth as integer = 0  'Smoothing mode (0 or 1)
DIM SHARED mainwindow as SDL_Window ptr = NULL
DIM SHARED mainrenderer as SDL_Renderer ptr = NULL
DIM SHARED maintexture as SDL_Texture ptr = NULL

DIM SHARED screenbuffer as SDL_Surface ptr = NULL
DIM SHARED last_bitdepth as integer   'Bitdepth of the last gfx_present call

DIM SHARED windowedmode as bool = YES  'Windowed rather than fullscreen? (Should we trust this, or call SDL_GetWindowFlags?)
DIM SHARED resizable as bool = NO
DIM SHARED resize_requested as bool = NO
DIM SHARED resize_request as XYPair
DIM SHARED min_window_resolution as XYPair = XY(10, 10)  'Used only if 'resizable' true. Excludes zoom factor.
DIM SHARED recenter_window_hint as bool = NO
DIM SHARED remember_windowtitle as string
DIM SHARED mouse_visibility as CursorVisibility = cursorDefault
DIM SHARED sdlpalette as SDL_Palette ptr
DIM SHARED framesize as XYPair       'Size of the unscaled image
DIM SHARED mouseclipped as bool = NO   'Whether we are ACTUALLY clipped
DIM SHARED forced_mouse_clipping as bool = NO
DIM SHARED remember_mousebounds as RectPoints = ((-1, -1), (-1, -1)) 'Args at the last call to io_mouserect
DIM SHARED mousebounds as RectPoints = ((-1, -1), (-1, -1)) 'These are the actual clip bounds, in window coords
DIM SHARED privatempos as XYPair     'Mouse position in window coords
DIM SHARED keybdstate(127) as KeyBits  '"real"time keyboard array. See io_sdl2_keybits for docs.
DIM SHARED input_buffer as ustring
DIM SHARED mouseclicks as integer    'Bitmask of mouse buttons clicked (SDL order, not OHR), since last io_mousebits
DIM SHARED mousewheel as integer     'Position of the wheel. A multiple of 120
DIM SHARED virtual_keyboard_shown as bool = NO
DIM SHARED allow_virtual_gamepad as bool = YES
DIM SHARED safe_zone_margin as single = 0.0

END EXTERN ' Can't put assignment statements in an extern block

'Translate SDL scancodes into a OHR scancodes
'Of course, scancodes can only be correctly mapped to OHR scancodes on a US keyboard.
'SDL scancodes say what's the unmodified character on a key. For example
'on a German keyboard the +/*/~ key is SDLK_PLUS, gets mapped to
'scPlus, which is the same as scEquals, so you get = when you press
'it.
'If there is no ASCII equivalent character, the key has a SDLK_WORLD_## scancode.

DIM SHARED scantrans(0 to SDL_NUM_SCANCODES) as KBScancode
scantrans(SDL_SCANCODE_UNKNOWN) = 0
scantrans(SDL_SCANCODE_BACKSPACE) = scBackspace
scantrans(SDL_SCANCODE_TAB) = scTab
scantrans(SDL_SCANCODE_CLEAR) = 0
scantrans(SDL_SCANCODE_RETURN) = scEnter
scantrans(SDL_SCANCODE_PAUSE) = scPause
scantrans(SDL_SCANCODE_ESCAPE) = scEsc
scantrans(SDL_SCANCODE_SPACE) = scSpace
scantrans(SDL_SCANCODE_APOSTROPHE) = scQuote
scantrans(SDL_SCANCODE_COMMA) = scComma
scantrans(SDL_SCANCODE_PERIOD) = scPeriod
scantrans(SDL_SCANCODE_SLASH) = scSlash
scantrans(SDL_SCANCODE_0) = sc0
scantrans(SDL_SCANCODE_1) = sc1
scantrans(SDL_SCANCODE_2) = sc2
scantrans(SDL_SCANCODE_3) = sc3
scantrans(SDL_SCANCODE_4) = sc4
scantrans(SDL_SCANCODE_5) = sc5
scantrans(SDL_SCANCODE_6) = sc6
scantrans(SDL_SCANCODE_7) = sc7
scantrans(SDL_SCANCODE_8) = sc8
scantrans(SDL_SCANCODE_9) = sc9
scantrans(SDL_SCANCODE_SEMICOLON) = scSemicolon
scantrans(SDL_SCANCODE_EQUALS) = scEquals
scantrans(SDL_SCANCODE_LEFTBRACKET) = scLeftBracket
scantrans(SDL_SCANCODE_BACKSLASH) = scBackslash
scantrans(SDL_SCANCODE_RIGHTBRACKET) = scRightBracket
scantrans(SDL_SCANCODE_MINUS) = scMinus
scantrans(SDL_SCANCODE_GRAVE) = scBackquote
scantrans(SDL_SCANCODE_a) = scA
scantrans(SDL_SCANCODE_b) = scB
scantrans(SDL_SCANCODE_c) = scC
scantrans(SDL_SCANCODE_d) = scD
scantrans(SDL_SCANCODE_e) = scE
scantrans(SDL_SCANCODE_f) = scF
scantrans(SDL_SCANCODE_g) = scG
scantrans(SDL_SCANCODE_h) = scH
scantrans(SDL_SCANCODE_i) = scI
scantrans(SDL_SCANCODE_j) = scJ
scantrans(SDL_SCANCODE_k) = scK
scantrans(SDL_SCANCODE_l) = scL
scantrans(SDL_SCANCODE_m) = scM
scantrans(SDL_SCANCODE_n) = scN
scantrans(SDL_SCANCODE_o) = scO
scantrans(SDL_SCANCODE_p) = scP
scantrans(SDL_SCANCODE_q) = scQ
scantrans(SDL_SCANCODE_r) = scR
scantrans(SDL_SCANCODE_s) = scS
scantrans(SDL_SCANCODE_t) = scT
scantrans(SDL_SCANCODE_u) = scU
scantrans(SDL_SCANCODE_v) = scV
scantrans(SDL_SCANCODE_w) = scW
scantrans(SDL_SCANCODE_x) = scX
scantrans(SDL_SCANCODE_y) = scY
scantrans(SDL_SCANCODE_z) = scZ
scantrans(SDL_SCANCODE_DELETE) = scDelete
scantrans(SDL_SCANCODE_KP_0) = scNumpad0
scantrans(SDL_SCANCODE_KP_1) = scNumpad1
scantrans(SDL_SCANCODE_KP_2) = scNumpad2
scantrans(SDL_SCANCODE_KP_3) = scNumpad3
scantrans(SDL_SCANCODE_KP_4) = scNumpad4
scantrans(SDL_SCANCODE_KP_5) = scNumpad5
scantrans(SDL_SCANCODE_KP_6) = scNumpad6
scantrans(SDL_SCANCODE_KP_7) = scNumpad7
scantrans(SDL_SCANCODE_KP_8) = scNumpad8
scantrans(SDL_SCANCODE_KP_9) = scNumpad9
scantrans(SDL_SCANCODE_KP_PERIOD) = scNumpadPeriod
scantrans(SDL_SCANCODE_KP_DIVIDE) = scNumpadSlash
scantrans(SDL_SCANCODE_KP_MULTIPLY) = scNumpadAsterisk
scantrans(SDL_SCANCODE_KP_MINUS) = scNumpadMinus
scantrans(SDL_SCANCODE_KP_PLUS) = scNumpadPlus
scantrans(SDL_SCANCODE_KP_ENTER) = scNumpadEnter
scantrans(SDL_SCANCODE_KP_EQUALS) = scEquals
scantrans(SDL_SCANCODE_UP) = scUp
scantrans(SDL_SCANCODE_DOWN) = scDown
scantrans(SDL_SCANCODE_RIGHT) = scRight
scantrans(SDL_SCANCODE_LEFT) = scLeft
scantrans(SDL_SCANCODE_INSERT) = scInsert
scantrans(SDL_SCANCODE_HOME) = scHome
scantrans(SDL_SCANCODE_END) = scEnd
scantrans(SDL_SCANCODE_PAGEUP) = scPageup
scantrans(SDL_SCANCODE_PAGEDOWN) = scPagedown
scantrans(SDL_SCANCODE_F1) = scF1
scantrans(SDL_SCANCODE_F2) = scF2
scantrans(SDL_SCANCODE_F3) = scF3
scantrans(SDL_SCANCODE_F4) = scF4
scantrans(SDL_SCANCODE_F5) = scF5
scantrans(SDL_SCANCODE_F6) = scF6
scantrans(SDL_SCANCODE_F7) = scF7
scantrans(SDL_SCANCODE_F8) = scF8
scantrans(SDL_SCANCODE_F9) = scF9
scantrans(SDL_SCANCODE_F10) = scF10
scantrans(SDL_SCANCODE_F11) = scF11
scantrans(SDL_SCANCODE_F12) = scF12
scantrans(SDL_SCANCODE_F13) = scF13
scantrans(SDL_SCANCODE_F14) = scF14
scantrans(SDL_SCANCODE_F15) = scF15
scantrans(SDL_SCANCODE_NUMLOCKCLEAR) = scNumlock  'Clear key on Macs
scantrans(SDL_SCANCODE_CAPSLOCK) = scCapslock
scantrans(SDL_SCANCODE_SCROLLLOCK) = scScrollLock
scantrans(SDL_SCANCODE_RSHIFT) = scRightShift
scantrans(SDL_SCANCODE_LSHIFT) = scLeftShift
scantrans(SDL_SCANCODE_RCTRL) = scRightCtrl
scantrans(SDL_SCANCODE_LCTRL) = scLeftCtrl
scantrans(SDL_SCANCODE_RALT) = scRightAlt
scantrans(SDL_SCANCODE_LALT) = scLeftAlt
scantrans(SDL_SCANCODE_RGUI) = scRightMeta
scantrans(SDL_SCANCODE_LGUI) = scLeftMeta
scantrans(SDL_SCANCODE_MODE) = scRightAlt   'Possibly (probably not) Alt Gr? So treat it as alt
scantrans(SDL_SCANCODE_HELP) = 0
scantrans(SDL_SCANCODE_PRINTSCREEN) = scPrintScreen
scantrans(SDL_SCANCODE_SYSREQ) = scPrintScreen
scantrans(SDL_SCANCODE_PAUSE) = scPause
scantrans(SDL_SCANCODE_MENU) = scContext
scantrans(SDL_SCANCODE_APPLICATION) = scContext
scantrans(SDL_SCANCODE_POWER) = 0
scantrans(SDL_SCANCODE_UNDO) = 0
EXTERN "C"


PRIVATE SUB log_error(failed_call as zstring ptr, funcname as zstring ptr)
  debugerror *funcname & " " & *failed_call & ": " & *SDL_GetError()
END SUB

FUNCTION gfx_sdl2_init(byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr, byval info_buffer as zstring ptr, byval info_buffer_size as integer) as integer

  #ifdef USE_X11
    'Xlib will kill the program if most errors occur, such as if OpenGL on the machine is broken
    'so the window can't be created. We need to install an error handler to prevent that
    set_X11_error_handlers
  #endif

  'Not needed, seems to work without
  'SDL_SetHint(SDL_HINT_WINDOWS_INTRESOURCE_ICON, windowicon)
  #ifndef IS_GAME
    'By default SDL prevents the screensaver (new in SDL 2.0.2)
    SDL_SetHint(SDL_HINT_VIDEO_ALLOW_SCREENSAVER, "1")
  #endif
  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, "nearest")
  'By default SDL disables VM compositing in order to allow higher framerates, but this is causes problems under KWin (KDE). (SDL 2.0.8+)
  SDL_SetHint("SDL_VIDEO_X11_NET_WM_BYPASS_COMPOSITOR", "0")
  'We don't need shaders
  'NOTE: commented out, because this caused the window to be stuck white under X11. It used to work fine in
  'SDL 2.0.10 but is broken in SDL 2.0.12.
  'SDL_SetHint(SDL_HINT_RENDER_OPENGL_SHADERS, "0")
  'I guess this sets the render driver?
  'SDL_SetHint(SDL_HINT_FRAMEBUFFER_ACCELERATION, "0") 'software, opengl, direct3d, opengles2, opengles, metal
  'Maybe want to set SDL_HINT_VIDEO_X11_NET_WM_PING off, since we detect hung scripts ourselves?
  'Make Ctrl-click on Mac send a right-click event
  SDL_SetHint(SDL_HINT_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK, "1")
  'IMEs should provide their own UIs for inputting characters, as we don't handle SDL_TEXTEDITING events
  SDL_SetHint("SDL_IME_INTERNAL_EDITING", "1")  'SDL_HINT_IME_INTERNAL_EDITING not in old FB headers
  'Return key on on-screen keyboard acts as 'done'
  SDL_SetHint("SDL_RETURN_KEY_HIDES_IME", "1")  'SDL_HINT_RETURN_KEY_HIDES_IME not in FB's header yet
  'Don't minimise the window if it loses focus while fullscreen - it's annoying if you
  'have multiple monitors and you move the mouse to another monitor
  SDL_SetHint(SDL_HINT_VIDEO_MINIMIZE_ON_FOCUS_LOSS, "0")
  'This controls whether SDL will wait for vsync (causing the framerate to cap to 60fps), or
  'to triple buffer (adding a frame of latency) - only some drivers
  'SDL_SetHint("SDL_VIDEO_DOUBLE_BUFFER", "1")  'SDL_HINT_VIDEO_DOUBLE_BUFFER not in FB's header yet

  'Possibly useful in future:
  'SDL_SetHint(SDL_HINT_RENDER_LOGICAL_SIZE_MODE, "overscan")  'Causes left/right of screen to be clipped instead of letterboxing

  'To receive controller updates while in the background, SDL_HINT_JOYSTICK_ALLOW_BACKGROUND_EVENTS

  DIM ver as SDL_version
  SDL_GetVersion(@ver)
  DIM ret as string
  ret = "SDL " & ver.major & "." & ver.minor & "." & ver.patch

  DIM video_already_init as bool = (SDL_WasInit(SDL_INIT_VIDEO) <> 0)

  IF SDL_Init(SDL_INIT_VIDEO OR SDL_INIT_JOYSTICK OR SDL_INIT_GAMECONTROLLER) THEN
    ret = "Can't start SDL (gfx_sdl2): " & *SDL_GetError() & !"\n" & ret
    *info_buffer = LEFT(ret, info_buffer_size)
    RETURN 0
  END IF

  'Clear keyboard state because if we re-initialise the backend (switch backend)
  'some key-up events can easily get lost
  memset(@keybdstate(0), 0, (UBOUND(keybdstate) + 1) * SIZEOF(keybdstate(0)))

  'Enable controller events, so don't have to call SDL_GameControllerUpdate
  SDL_GameControllerEventState(SDL_ENABLE)

  ret &= " (" & SDL_NumJoysticks() & " joysticks) Driver:" & *SDL_GetCurrentVideoDriver() & " (Drivers:"
  FOR i as integer = 0 TO SDL_GetNumVideoDrivers() - 1
    ret &= " " & *SDL_GetVideoDriver(i)
  NEXT
  ret &= ")"

  framesize.w = 320
  framesize.h = 200

  sdlpalette = SDL_AllocPalette(256)
  CheckOK(sdlpalette = NULL, RETURN 0)

#IFDEF __FB_ANDROID__
  IF SDL_ANDROID_IsRunningOnConsole() THEN
    debuginfo "Running on a console, disable the virtual gamepad"
    internal_disable_virtual_gamepad
  ELSE
    debuginfo "Not running on a console, leave the virtual gamepad visible"
  END IF
#ENDIF

  *info_buffer = LEFT(ret, info_buffer_size)
  RETURN recreate_window()
END FUNCTION

LOCAL FUNCTION recreate_window(byval bitdepth as integer = 0) as bool
  IF mainrenderer THEN SDL_DestroyRenderer(mainrenderer)  'Also destroys textures
  mainrenderer = NULL
  maintexture = NULL
  IF mainwindow THEN SDL_DestroyWindow(mainwindow)
  mainwindow = NULL

  DIM flags as Uint32 = 0
  IF resizable THEN flags = flags OR SDL_WINDOW_RESIZABLE
  IF windowedmode = NO THEN
    'TODO: when "true fullscreen" is used and you quit from fullscreen using alt-F4, on linux/KDE at least,
    'the screen doesn't restore to its original resolution. So need to return to windowed mode when
    'quitting gfx_sdl2
    'flags = flags OR SDL_WINDOW_FULLSCREEN
    ' This means don't change the resolution, instead create a fullscreen window, like gfx_directx
    flags = flags OR SDL_WINDOW_FULLSCREEN_DESKTOP
  END IF

  DIM windowpos as integer
  IF recenter_window_hint ANDALSO running_as_slave = NO THEN   'Don't display the window straight on top of Custom's
    windowpos = SDL_WINDOWPOS_CENTERED
  ELSE
    windowpos = SDL_WINDOWPOS_UNDEFINED
  END IF
  recenter_window_hint = NO

  'Start with initial zoom and repeatedly decrease it if it is too large
  '(This is necessary to run in fullscreen in OSX IIRC)
  DO
    DIM windowsize as XYPair = framesize * zoom
    debuginfo "setvideomode zoom=" & zoom & " w*h = " & windowsize
    mainwindow = SDL_CreateWindow(remember_windowtitle, windowpos, windowpos, _
                                  windowsize.w, windowsize.h, flags)
    IF mainwindow = NULL THEN
      'This crude hack won't work for everyone if the SDL error messages are internationalised...
      IF zoom > 1 ANDALSO strstr(SDL_GetError(), "No video mode large enough") THEN
        debug "Failed to open display (windowed = " & windowedmode & ") (retrying with smaller zoom): " & *SDL_GetError
        zoom -= 1
        CONTINUE DO
      END IF
      debug "Failed to open display (windowed = " & windowedmode & "): " & *SDL_GetError
      RETURN 0
    END IF
    EXIT DO
  LOOP

  mainrenderer = SDL_CreateRenderer(mainwindow, -1, SDL_RENDERER_PRESENTVSYNC)
  ' Don't kill the program yet; we might be able to switch to a different backend
  IF mainrenderer = NULL THEN
    log_error("SDL_CreateRenderer failed; falling back to software renderer", "")
    'Doing SDL_SetHint(SDL_HINT_FRAMEBUFFER_ACCELERATION, "software") or "0" instead didn't help to
    'recover from a broken X11 OpenGL implementation
    SDL_SetHint(SDL_HINT_RENDER_DRIVER, "software")
    mainrenderer = SDL_CreateRenderer(mainwindow, -1, SDL_RENDERER_PRESENTVSYNC)
    CheckOK(mainrenderer = NULL, RETURN 0)
  END IF

  #IFNDEF SDL_203
    'Whether to stick to integer scaling amounts when using SDL_RenderSetLogicalSize. SDL 2.0.5+
    'SDL_RenderSetIntegerScale(mainrenderer, NO)
  #ENDIF

  set_viewport

  IF recreate_screen_texture() = NO THEN RETURN 0

/'
  WITH *mainwindow->format
   debuginfo "gfx_sdl2: created mainwindow size=" & mainwindow->w & "*" & mainwindow->h _
             & " depth=" & .BitsPerPixel & " flags=0x" & HEX(mainwindow->flags) _
             & " R=0x" & hex(.Rmask) & " G=0x" & hex(.Gmask) & " B=0x" & hex(.Bmask)
   'FIXME: should handle the screen surface not being BGRA, or ask SDL for a surface in that encoding
  END WITH
'/

  update_mouse_visibility()
  RETURN 1
END FUNCTION

LOCAL FUNCTION recreate_screen_texture() as bool
  IF mainrenderer = NULL THEN RETURN NO  'Called before backend init

  IF maintexture THEN SDL_DestroyTexture(maintexture)
  DIM buffersize as XYPair = get_buffersize
  maintexture = SDL_CreateTexture(mainrenderer, _
                               SDL_PIXELFORMAT_ARGB8888, _
                               SDL_TEXTUREACCESS_STREAMING, _
                               buffersize.w, buffersize.h)
  CheckOK(maintexture = NULL, RETURN NO)
  RETURN YES
END FUNCTION

LOCAL FUNCTION get_buffersize() as XYPair
  DIM buffer_zoom as integer = IIF(smooth, smooth_zoom, 1)
  RETURN framesize * buffer_zoom
END FUNCTION

'Set/update the part of the window/screen on which to draw the frame, including scaling and letterboxing.
LOCAL SUB set_viewport()
  IF windowedmode = NO THEN
    'Fullscreen: Ask SDL to scale, center and letterbox automatically.
    '(Aspect ratio is always preserved. SDL_RenderSetIntegerScale is optional)
    'But this will lead to ugly wobbling while resizing a window.
    DIM buffersize as XYPair = get_buffersize
    SDL_RenderSetLogicalSize(mainrenderer, buffersize.w, buffersize.h)
  ELSE
    'No centering while windowed, fixed scale amount.
    '(There's no need to use SDL_RenderSetScale)
    DIM rect as SDL_Rect
    rect.w = zoom * framesize.w
    rect.h = zoom * framesize.h
    'Calling SDL_RenderSetLogicalSize overrides previous SDL_RenderSetViewport,
    'but calling SDL_RenderSetViewport does not turn off SDL_RenderSetLogicalSize automatically.
    SDL_RenderSetLogicalSize(mainrenderer, 0, 0)
    SDL_RenderSetViewport(mainrenderer, @rect)
  END IF
END SUB

'Note that gfx_sdl2_set_window_size wraps this.
LOCAL SUB set_window_size(newframesize as XYPair, newzoom as integer)
  framesize = newframesize
  zoom = newzoom
  smooth_zoom = IIF(newzoom > 4, 3, newzoom)

  IF debugging_io THEN
    debuginfo "set_window_size " & newframesize & " x" & newzoom
  END IF

  IF resizable THEN
    DIM minsize as XYPair = min_window_resolution * zoom
    SDL_SetWindowMinimumSize(mainwindow, minsize.w, minsize.h)
  END IF

  IF mainwindow THEN
    'TODO: this doesn't work if fullscreen
    '(FIXME: If you open debug menu in-game, resize the window, press alt-enter to fullscreen
    'and then alt-enter back to windowed, the window won't return to original size.
    'Maybe the cause is here.)
    SDL_SetWindowSize(mainwindow, zoom * framesize.w, zoom * framesize.h)
    set_viewport
    recreate_screen_texture
  END IF
END SUB

LOCAL SUB quit_video_subsystem()
  IF mainrenderer THEN SDL_DestroyRenderer(mainrenderer)  'Also destroys textures
  mainrenderer = NULL
  maintexture = NULL
  IF mainwindow THEN SDL_DestroyWindow(mainwindow)
  mainwindow = NULL
  IF screenbuffer THEN SDL_FreeSurface(screenbuffer)
  screenbuffer = NULL
  IF sdlpalette THEN SDL_FreePalette(sdlpalette)
  sdlpalette = NULL
  SDL_QuitSubSystem(SDL_INIT_VIDEO)
END SUB

FUNCTION gfx_sdl2_getversion() as integer
  RETURN 1
END FUNCTION

'Handles smoothing and changes to the frame size, then calls present_internal2
'to update the screen
LOCAL FUNCTION present_internal(raw as any ptr, imagesz as XYPair, bitdepth as integer) as integer
  'debuginfo "gfx_sdl2_present_internal(" & imagesz & ", bitdepth=" & bitdepth & ")"

  last_bitdepth = bitdepth

  'variable resolution handling
  IF framesize <> imagesz THEN
    'debuginfo "gfx_sdl2_present_internal: framesize changing from " & framesize & " to " & imagesz
    set_window_size(imagesz, zoom)
  END IF

  DIM pitch as integer = imagesz.w * IIF(bitdepth = 32, 4, 1)

  'This is zoom ratio from raw to screenbuffer. Unrelated to window zoom.
  DIM buffer_zoom as integer = IIF(smooth, smooth_zoom, 1)
  DIM buffersize as XYPair = get_buffersize

  IF smooth ORELSE bitdepth = 8 THEN
    ' We need screenbuffer. So check it exists and is the right size

    IF screenbuffer THEN
      IF XY(screenbuffer->w, screenbuffer->h) <> buffersize ORELSE _
          screenbuffer->format->BitsPerPixel <> bitdepth THEN
        SDL_FreeSurface(screenbuffer)
        screenbuffer = NULL
      END IF
    END IF

    IF screenbuffer = NULL THEN
      IF bitdepth = 32 THEN
        'screenbuffer = SDL_CreateRGBSurfaceFrom(raw, w, h, 8, w, 0,0,0,0)

        'screenbuffer = SDL_CreateRGBSurfaceWithFormat(0, buffersize.w, buffersize.h, 32, SDL_PIXELFORMAT_ARGB8888)
        screenbuffer = SDL_CreateRGBSurface(0, buffersize.w, buffersize.h, bitdepth, &h00ff0000, &h0000ff00, &h000000ff, &hff000000)
      ELSE
        screenbuffer = SDL_CreateRGBSurface(0, buffersize.w, buffersize.h, bitdepth, 0,0,0,0)
      END IF
    END IF

    IF screenbuffer = NULL THEN
      debugc errDie, "present_internal: Failed to allocate page wrapping surface, " & *SDL_GetError()
    END IF
  END IF

  IF smooth THEN
    ' Intermediate step: do an enlarged blit to a surface and then do smoothing

    IF bitdepth = 8 THEN
      smoothzoomblit_8_to_8bit(raw, screenbuffer->pixels, imagesz, screenbuffer->pitch, buffer_zoom, smooth)
    ELSE
      '32 bit surface
      'smoothzoomblit takes the pitch in pixels, not bytes!
      smoothzoomblit_32_to_32bit(cast(RGBcolor ptr, raw), cast(uint32 ptr, screenbuffer->pixels), imagesz, screenbuffer->pitch \ 4, buffer_zoom, smooth)
    END IF

    raw = screenbuffer->pixels
    pitch = screenbuffer->pitch

  ELSEIF bitdepth = 8 THEN
    'Need to make a copy of the input, in case gfx_setpal is called

    'Copy over
    'smoothzoomblit_8_to_8bit(raw, screenbuffer->pixels, imagesz, screenbuffer->pitch, 1, smooth)
    SDL_ConvertPixels(imagesz.w, imagesz.h, SDL_PIXELFORMAT_INDEX8, raw, pitch, SDL_PIXELFORMAT_INDEX8, screenbuffer->pixels, screenbuffer->pitch)

  ELSE
    ' Can copy directly to maintexture: screenbuffer is not used
  END IF

  RETURN present_internal2(screenbuffer, raw, buffersize, pitch, bitdepth)
END FUNCTION

'Updates the screen. Assumes all size changes have been handled.
'If bitdepth=8 then srcsurf is used, otherwise raw is used, and is a block of
'pixels in SDL_PIXELFORMAT_ARGB8888 with the given pitch.
'The surface or block of pixels must be the same size as maintexture.
LOCAL FUNCTION present_internal2(srcsurf as SDL_Surface ptr, raw as any ptr, imagesz as XYPair, pitch as integer, bitdepth as integer) as bool
  DIM ret as bool = YES

  DIM as integer texw, texh
  DIM texpixels as any ptr
  DIM texpitch as integer
  SDL_QueryTexture(maintexture, NULL, NULL, @texw, @texh)
  CheckOK(SDL_LockTexture(maintexture, NULL, @texpixels, @texpitch), RETURN NO)

  IF bitdepth = 8 THEN
    'SDL2 has two different ways to specify a pixel format:
    ' struct SDL_PixelFormat - the struct used by SDL_Surfaces. Very flexible, includes an SDL_Palette*
    ' enum SDL_PixelFormatEnum - available texture formats.
    'Conversion functions:
    ' SDL_ConvertPixels - convert raw pixel buffer from one SDL_PixelFormatEnum to another
    ' SDL_ConvertSurface - copy of a Surface converted to a SDL_PixelFormat
    ' SDL_ConvertSurfaceFormat - copy of a Surface converted to a SDL_PixelFormatEnum
    ' SDL_BlitSurface - between Surfaces. Does a conversion
    'Also relevant:
    ' SDL_AllocFormat - Get a SDL_PixelFormat from a SDL_PixelFormatEnum
    ' SDL_SetSurfacePalette - Modify's a Surface's SDL_PixelFormat
    ' SDL_CreateRGBSurfaceFrom - A Surface wrapping an existing pixel buffer, defined by masks
    ' SDL_CreateRGBSurfaceWithFormatFrom - A Surface wrapping an existing pixel buffer, defined by SDL_PixelFormatEnum.

    'So can't use SDL_ConvertPixels as it doesn't support a palette.

    CheckOK(SDL_SetSurfacePalette(srcsurf, sdlpalette))

    DIM destsurf as SDL_Surface ptr
    'Avoid SDL_CreateRGBSurfaceWithFormatFrom because it's SDL 2.0.5+
    'destsurf = SDL_CreateRGBSurfaceWithFormatFrom(texpixels, texw, texh, 32, texpitch, SDL_PIXELFORMAT_ARGB8888)
    destsurf = SDL_CreateRGBSurfaceFrom(texpixels, texw, texh, 32, texpitch, &h00ff0000, &h0000ff00, &h000000ff, &hff000000)
    CheckOK(destsurf = NULL)

    CheckOK(SDL_BlitSurface(srcsurf, NULL, destsurf, NULL), ret = NO)
    '? texw, texh, srcsurf->w, srcsurf->h, imagew, imageh

    SDL_FreeSurface(destsurf)
  ELSE

    'Formats are the same, so this will be a simple copy
    CheckOK(SDL_ConvertPixels(texw, texh, SDL_PIXELFORMAT_ARGB8888, raw, pitch, SDL_PIXELFORMAT_ARGB8888, texpixels, texpitch), ret = NO)
    'CheckOK(SDL_UpdateTexture(maintexture, NULL, raw, pitch), ret = NO)
  END IF

  SDL_UnlockTexture(maintexture)

  'Clearing the screen first is necessary in fullscreen, when the window size may not match the maintexture size
  '(this clears the black bars)
  SDL_RenderClear(mainrenderer)
  'DIM dstrect as SDL_Rect = (0, 0, framesize.w * zoom, framesize.h * zoom) 'imagew, imageh
  CheckOK(SDL_RenderCopy(mainrenderer, maintexture, NULL, NULL /'@dstrect'/), ret = NO)
  SDL_RenderPresent(mainrenderer)

  update_state()

  RETURN ret
END FUNCTION

'Copies an RGBColor[256] array to sdlpalette
LOCAL SUB set_palette(pal as RGBColor ptr)
  DIM cols(255) as SDL_Color
  FOR i as integer = 0 TO 255
    cols(i).r = pal[i].r
    cols(i).g = pal[i].g
    cols(i).b = pal[i].b
  NEXT
  SDL_SetPaletteColors(sdlpalette, @cols(0), 0, 256)
END SUB

SUB gfx_sdl2_setpal(byval pal as RGBcolor ptr)
  IF last_bitdepth = 8 THEN
    set_palette pal
    'Re-render the contents of screenbuffer
    present_internal2(screenbuffer, NULL, XY(screenbuffer->w, screenbuffer->h), screenbuffer->pitch, 8)
  ELSE
    debuginfo "gfx_sdl2_setpal called after a 32bit present"
  END IF
  update_state()
END SUB

FUNCTION gfx_sdl2_present(byval surfaceIn as Surface ptr, byval pal as RGBPalette ptr) as integer
  WITH *surfaceIn
    IF .format = SF_8bit AND pal <> NULL THEN
      set_palette @pal->col(0)
    END IF
    DIM ret as integer
    ret = present_internal(.pColorData, .size, IIF(.format = SF_8bit, 8, 32))
    update_state()
    RETURN ret
  END WITH
END FUNCTION

FUNCTION gfx_sdl2_screenshot(byval fname as zstring ptr) as integer
  gfx_sdl2_screenshot = 0
END FUNCTION

SUB gfx_sdl2_setwindowed(byval towindowed as bool)
  DIM flags as int32 = 0
  IF towindowed = NO THEN flags = SDL_WINDOW_FULLSCREEN_DESKTOP
  IF SDL_SetWindowFullscreen(mainwindow, flags) THEN
    showerror "Could not toggle fullscreen mode: " & *SDL_GetError()
    EXIT SUB
  END IF
  windowedmode = towindowed
  IF debugging_io THEN debuginfo "setwindowed " & towindowed
  'TODO: call gfx_sdl2_set_resizable here, since that doesn't work on fullscreen windows?

  'Turn on or off scaling/centering/letterboxing
  set_viewport
END SUB

SUB gfx_sdl2_windowtitle(byval title as zstring ptr)
  IF SDL_WasInit(SDL_INIT_VIDEO) then
    SDL_SetWindowTitle(mainwindow, title)
  END IF
  remember_windowtitle = *title
END SUB

FUNCTION gfx_sdl2_getwindowstate() as WindowState ptr
  STATIC state as WindowState
  state.structsize = WINDOWSTATE_SZ
  DIM flags as uint32 = SDL_GetWindowFlags(mainwindow)
  'TODO: what about SDL_WINDOW_SHOWN/SDL_WINDOW_HIDDEN?
  state.focused = (flags AND SDL_WINDOW_INPUT_FOCUS) <> 0
  state.minimised = (flags AND SDL_WINDOW_MINIMIZED) = 0
  state.fullscreen = (flags AND (SDL_WINDOW_FULLSCREEN OR SDL_WINDOW_FULLSCREEN_DESKTOP)) <> 0
  state.mouse_over = (flags AND SDL_WINDOW_MOUSE_FOCUS) <> 0
  SDL_GetWindowSize(mainwindow, @state.windowsize.w, @state.windowsize.h)
  state.zoom = zoom
  RETURN @state
END FUNCTION

SUB gfx_sdl2_get_screen_size(wide as integer ptr, high as integer ptr)
  'Query the first display.
  DIM rect as SDL_Rect
#IFNDEF SDL_203
  'SDL_GetDisplayUsableBounds excludes area for taskbar, OSX menubar, dock, etc.,
  'but is SDL 2.0.5+
  IF SDL_GetDisplayUsableBounds(0, @rect) THEN
#ELSE
  IF SDL_GetDisplayBounds(0, @rect) THEN
#ENDIF
    debug "SDL_GetDisplayUsableBounds: " & *SDL_GetError()
    *wide = 0
    *high = 0
  ELSE
    *wide = rect.w
    *high = rect.h
  END IF
END SUB

FUNCTION gfx_sdl2_supports_variable_resolution() as bool
  'Safe even in fullscreen, I think
  RETURN YES
END FUNCTION

FUNCTION gfx_sdl2_vsync_supported() as bool
  #IFDEF __FB_DARWIN__
    ' OSX always has vsync, and drawing the screen will block until vsync, so this needs
    ' special treatment (as opposed to most other WMs which also do vsync compositing)
    RETURN YES
  #ELSE
    'FIXME: this is usually wrong
    RETURN NO
  #ENDIF
END FUNCTION

FUNCTION gfx_sdl2_set_resizable(enable as bool, min_width as integer, min_height as integer) as bool
  resizable = enable
  IF mainwindow = NULL THEN RETURN resizable

  'The min window size may (depending on OS?) still be enforced (even on
  'SDL_SetWindowSize) even if we disable resizing, so have to change it so setting a
  'small zoom or frame size works. But passing zero width/height to
  'SDL_SetWindowMinimumSize is invalid, so set to something tiny.
  DIM minsize as XYPair
  IF enable THEN
    min_window_resolution = large(XY(5, 5), XY(min_width, min_height))
    minsize = min_window_resolution * zoom
  ELSE
    minsize = XY(10, 10)
  END IF
  'debuginfo "SDL_SetWindowMinimumSize " & minsize
  SDL_SetWindowMinimumSize(mainwindow, minsize.w, minsize.h)

  'Note: Can't change resizability of a fullscreen window
  'Argh, SDL_SetWindowResizable was only added in SDL 2.0.5 (Oct 2016)
  #IFNDEF SDL_203
    SDL_SetWindowResizable(mainwindow, resizable)
  #ELSE
    recreate_window()
  #ENDIF
  RETURN resizable
END FUNCTION

FUNCTION gfx_sdl2_get_resize(byref ret as XYPair) as bool
  IF resize_requested THEN
    ret = resize_request
    resize_requested = NO
    RETURN YES
  END IF
  RETURN NO
END FUNCTION

'Interesting behaviour: under X11+KDE+SDL 1.2, if the window doesn't go over the screen edges and is resized
'larger (SDL_SetVideoMode), then it will automatically be moved to fit onscreen (if you DON'T ask for recenter).
SUB gfx_sdl2_recenter_window_hint()
  'Takes effect at the next SDL_SetVideoMode call, and it's then removed
  debuginfo "recenter_window_hint()"
  recenter_window_hint = YES
END SUB

'This is the new API for changing window size, an alternative to calling gfx_present with a resized frame.
'Unlike gfx_present, it causes the window to resize but doesn't repaint it yet.
SUB gfx_sdl2_set_window_size (byval newframesize as XYPair, newzoom as integer)
  IF newzoom >= 1 AND newzoom <= 16 AND newzoom <> zoom THEN
    'debuginfo "set_window_size " & newframesize & ", zoom=" & newzoom

    IF newframesize <> framesize THEN
      resize_request = newframesize
      resize_requested = YES
      'debuginfo " (resize_requested)"
    END IF

    gfx_sdl2_recenter_window_hint()  'Recenter because the window might go off the screen edge.

    set_window_size(newframesize, newzoom)

    'Update the clip rectangle
    'It would probably be easier to just store the non-zoomed clipped rect (mxmin, etc)
    IF remember_mousebounds.p1.x <> -1 THEN
      internal_set_mouserect remember_mousebounds
    ELSEIF forced_mouse_clipping THEN
      internal_set_mouserect TYPE<RectPoints>((0, 0), framesize.w - 1, framesize.h - 1)
    END IF
  END IF
END SUB

FUNCTION gfx_sdl2_setoption(byval opt as zstring ptr, byval arg as zstring ptr) as integer
  DIM ret as integer = 0
  DIM value as integer = str2int(*arg, -1)
  '"zoomonly" is not supported, we support gfx_sdl2_set_window_size instead.
  IF *opt = "zoom" or *opt = "z" THEN
    gfx_sdl2_set_window_size(framesize, value)
    ret = 1
  ELSEIF *opt = "smooth" OR *opt = "s" THEN
    IF value = 1 OR value = -1 THEN  'arg optional (-1)
      smooth = 1
    ELSE
      smooth = 0
    END IF
    ret = 1
  END IF
  'all these take an optional numeric argument, so gobble the arg if it is
  'a number, whether or not it was valid
  IF ret = 1 AND parse_int(*arg) THEN ret = 2
  RETURN ret
END FUNCTION

FUNCTION gfx_sdl2_describe_options() as zstring ptr
  return @"-z -zoom [1...16]   Scale screen to 1,2, ... up to 16x normal size (2x default)" LINE_END _
          "-s -smooth          Enable smoothing filter for zoom modes (default off)"
END FUNCTION

FUNCTION gfx_sdl2_get_safe_zone_margin() as single
 RETURN safe_zone_margin
END FUNCTION

SUB gfx_sdl2_set_safe_zone_margin(margin as single)
 'FIXME: Not implemented!
 safe_zone_margin = margin
END SUB

FUNCTION gfx_sdl2_supports_safe_zone_margin() as bool
#IFDEF __FB_ANDROID__
 RETURN YES
#ELSE
 RETURN NO
#ENDIF
END FUNCTION

SUB gfx_sdl2_ouya_purchase_request(dev_id as string, identifier as string, key_der as string)
#IFDEF __FB_ANDROID__
 SDL_ANDROID_SetOUYADeveloperId(dev_id)
 SDL_ANDROID_OUYAPurchaseRequest(identifier, key_der, LEN(key_der))
#ENDIF
END SUB

FUNCTION gfx_sdl2_ouya_purchase_is_ready() as bool
#IFDEF __FB_ANDROID__
 RETURN SDL_ANDROID_OUYAPurchaseIsReady() <> 0
#ENDIF
 RETURN YES
END FUNCTION

FUNCTION gfx_sdl2_ouya_purchase_succeeded() as bool
#IFDEF __FB_ANDROID__
 RETURN SDL_ANDROID_OUYAPurchaseSucceeded() <> 0
#ENDIF
 RETURN NO
END FUNCTION

SUB gfx_sdl2_ouya_receipts_request(dev_id as string, key_der as string)
debuginfo "gfx_sdl2_ouya_receipts_request"
#IFDEF __FB_ANDROID__
 SDL_ANDROID_SetOUYADeveloperId(dev_id)
 SDL_ANDROID_OUYAReceiptsRequest(key_der, LEN(key_der))
#ENDIF
END SUB

FUNCTION gfx_sdl2_ouya_receipts_are_ready() as bool
#IFDEF __FB_ANDROID__
 RETURN SDL_ANDROID_OUYAReceiptsAreReady() <> 0
#ENDIF
 RETURN YES
END FUNCTION

FUNCTION gfx_sdl2_ouya_receipts_result() as string
#IFDEF __FB_ANDROID__
 DIM zresult as zstring ptr
 zresult = SDL_ANDROID_OUYAReceiptsResult()
 DIM result as string = *zresult
 RETURN result
#ENDIF
 RETURN ""
END FUNCTION

SUB io_sdl2_init
  'nothing needed at the moment...
END SUB

LOCAL SUB keycombos_logic(evnt as SDL_Event)
  'Check for platform-dependent key combinations

  IF evnt.key.keysym.mod_ AND KMOD_ALT THEN
    IF evnt.key.keysym.sym = SDLK_RETURN THEN  'alt-enter (not processed normally when using SDL)
      gfx_sdl2_setwindowed(windowedmode XOR YES)
      post_event(eventFullscreened, windowedmode = NO)
    END IF
    IF evnt.key.keysym.sym = SDLK_F4 THEN  'alt-F4
      post_terminate_signal
    END IF
  END IF

#IFDEF __FB_DARWIN__
  'Unlike SDL 1.2, shortcuts likes Cmd-Q, Cmd-M, Cmd-H and Cmd-Shift-H (Quit, Minimise, Hide, Hide Others)
  'which are attached to items in the menu bar just work without us having to do anything.

  IF evnt.key.keysym.mod_ AND KMOD_META THEN  'Command key
    'The shortcut (in the menubar) for fullscreen is Ctrl-Cmd-F, but also
    'support Cmd-F, which is what gfx_sdl uses.
    IF evnt.key.keysym.sym = SDLK_f THEN
      gfx_sdl2_setwindowed(windowedmode XOR YES)
      post_event(eventFullscreened, windowedmode = NO)
    END IF
    'SDL doesn't actually seem to send SDLK_QUESTION...
    'FIXME: this doesn't work properly, the Apple menu is opened,
    'and only when it closes does this get sent... but then the
    'keys get stuck!!
    IF evnt.key.keysym.sym = SDLK_SLASH AND evnt.key.keysym.mod_ AND KMOD_SHIFT THEN
      keybdstate(scF1) = 2
    END IF
    '(Note: gfx_sdl2 doesn't support set_scale_factor yet)
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

SUB gfx_sdl2_process_events()
  'I assume this uses SDL_PeepEvents instead of SDL_PollEvent because the latter calls SDL_PumpEvents
  DIM evnt as SDL_Event
  WHILE SDL_PeepEvents(@evnt, 1, SDL_GETEVENT, SDL_FIRSTEVENT, SDL_LASTEVENT)
    SELECT CASE evnt.type
      CASE SDL_QUIT_
        IF debugging_io THEN
          debuginfo "SDL_QUIT"
        END IF
        post_terminate_signal
      CASE SDL_KEYDOWN
        keycombos_logic(evnt)
        DIM as integer key = scantrans(evnt.key.keysym.scancode)
        IF debugging_io THEN
          debuginfo "SDL_KEYDOWN scan=" & evnt.key.keysym.scancode & " key=" & evnt.key.keysym.sym & " -> ohr=" & key & " (" & scancodename(key) & ") prev_keystate=" & keybdstate(key)
        END IF
        IF key ANDALSO evnt.key.repeat = 0 THEN
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
        DIM as integer key = scantrans(evnt.key.keysym.scancode)
        IF debugging_io THEN
          debuginfo "SDL_KEYUP scan=" & evnt.key.keysym.scancode & " key=" & evnt.key.keysym.sym & " -> ohr=" & key & " (" & scancodename(key) & ") prev_keystate=" & keybdstate(key)
        END IF
        'Clear 2nd bit (new keypress) and turn on 3rd bit (keyup)
        IF key THEN keybdstate(key) = (keybdstate(key) AND 2) OR 4
      CASE SDL_TEXTINPUT
        input_buffer += evnt.text.text  'UTF8

      CASE SDL_CONTROLLERDEVICEADDED
        IF debugging_io THEN
          debuginfo "SDL_CONTROLLERDEVICEADDED joynum=" & evnt.cdevice.which
        END IF

      CASE SDL_CONTROLLERDEVICEREMOVED
        IF debugging_io THEN
          debuginfo "SDL_CONTROLLERDEVICEREMOVED instance_id=" & evnt.cdevice.which
        END IF

      CASE SDL_JOYDEVICEADDED
        IF debugging_io THEN
          debuginfo "SDL_JOYDEVICEADDED joynum=" & evnt.jdevice.which & " " & SDL_JoystickNameForIndex(evnt.jdevice.which)
          '     & " instance_id=" & SDL_JoystickGetDeviceInstanceID(evnt.jdevice.which)
        END IF

      CASE SDL_JOYDEVICEREMOVED
        IF debugging_io THEN
          debuginfo "SDL_JOYDEVICEREMOVED instance_id=" & evnt.jdevice.which
        END IF

      CASE SDL_CONTROLLERBUTTONDOWN
        DIM btn as integer = evnt.cbutton.button
        DIM ok as bool = sdl2_joy_button_press(btn, evnt.cbutton.which)
        IF debugging_io THEN
          debuginfo "SDL_CONTROLLERBUTTONDOWN instance_id=" & evnt.cbutton.which & " sdlbtn=" & evnt.cbutton.button & " ok=" & ok
        END IF

      CASE SDL_JOYBUTTONDOWN
        DIM btn as integer = evnt.jbutton.button
        DIM ok as bool
        DIM joynum as integer = instance_to_joynum(evnt.jbutton.which)
        IF joynum >= 0 ANDALSO joystickinfo(joynum).have_bindings = NO THEN
          'Only process buttons for joysticks not recognised as gamepads, they're handled by SDL_CONTROLLERBUTTONDOWN
          ok = sdl2_joy_button_press(btn, evnt.jbutton.which)
        END IF
        IF debugging_io THEN
          debuginfo "SDL_JOYBUTTONDOWN instance_id=" & evnt.jbutton.which & " joynum=" & joynum & " sdlbtn=" & evnt.jbutton.button & " ok=" & ok
        END IF

      CASE SDL_MOUSEBUTTONDOWN
        'note SDL_GetMouseState is still used, while SDL_GetKeyState isn't
        'Interestingly, although (on Linux/X11) SDL doesn't report mouse motion events
        'if the window isn't focused, it does report mouse wheel button events
        '(other buttons focus the window).

        #IFNDEF SDL_203
         SDL_CaptureMouse(YES)  'So that dragging off the window reports positions outside the window
        #ENDIF
        WITH evnt.button
          mouseclicks OR= SDL_BUTTON(.button)
          IF debugging_io THEN
            debuginfo "SDL_MOUSEBUTTONDOWN mouse " & .which & " button " & .button & " at " & XY(.x, .y)
          END IF
        END WITH
      CASE SDL_MOUSEBUTTONUP
        'In order to wait until all buttons are up, we end mouse capture in update_mouse
        WITH evnt.button
          IF debugging_io THEN
            debuginfo "SDL_MOUSEBUTTONUP   mouse " & .which & " button " & .button & " at " & XY(.x, .y)
          END IF
        END WITH

      CASE SDL_MOUSEWHEEL
        IF debugging_io THEN
          debuginfo "SDL_MOUSEWHEEL " & evnt.wheel.x & "," & evnt.wheel.y & " mouse=" & evnt.wheel.which
          'SDL 2.0.4+:  & " dir=" & evnt.wheel.direction
        END IF
        'I'm surprised that SDL reports only 1 or -1 per wheel click... how does it reports wheels with
        'higher resolutions?
        mousewheel += evnt.wheel.y * 120
        'TODO: report evnt.wheel.x too

      CASE SDL_WINDOWEVENT
        IF debugging_io THEN
          debuginfo "SDL_WINDOWEVENT event=" & evnt.window.event
        END IF
        IF evnt.window.event = SDL_WINDOWEVENT_ENTER THEN
          'Gained mouse focus
          /'
          IF evnt.active.gain = 0 THEN
            SDL_ShowCursor(1)
          ELSE
            update_mouse_visibility()
          END IF
          '/
        END IF

        IF evnt.window.event = SDL_WINDOWEVENT_RESIZED THEN
          'This event is delivered when the window size is changed by the user/WM
          'rather than because we changed it (unlike SDL_WINDOWEVENT_SIZE_CHANGED)
          IF debugging_io THEN
            debuginfo "SDL_WINDOWEVENT_RESIZED: w=" & evnt.window.data1 & " h=" & evnt.window.data2
          END IF

          'The viewport is automatically updated when window is resized. In fullscreen
          '(SDL_RenderSetLogicalSize in use) that's good, but when windowed, the viewport
          'resets to cover the window, causing the image to stretch. Undo that
          IF windowedmode THEN set_viewport

          IF resizable THEN
            'Round upwards
            resize_request.w = (evnt.window.data1 + zoom - 1) \ zoom
            resize_request.h = (evnt.window.data2 + zoom - 1) \ zoom

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
          END IF
        END IF
    END SELECT
  WEND
END SUB

'may only be called from the main thread
LOCAL SUB update_state()
  SDL_PumpEvents()
  update_mouse()
  gfx_sdl2_process_events()
END SUB

SUB io_sdl2_pollkeyevents()
  'might need to redraw the screen if exposed
/'
  IF SDL_Flip(mainwindow) THEN
    debug "pollkeyevents: SDL_Flip failed: " & *SDL_GetError
  END IF
'/
  update_state()
END SUB

SUB io_sdl2_waitprocessing()
  update_state()
END SUB

LOCAL SUB keymod_to_keybdstate(modstate as integer, key as KBScancode)
  keybdstate(key) = (keybdstate(key) AND 6) OR IIF(modstate, 1, 0)
END SUB

SUB io_sdl2_keybits (byval keybdarray as KeyBits ptr)
  'keybdarray bits:
  ' bit 0 - key down
  ' bit 1 - new keypress event
  'keybdstate bits:
  ' bit 0 - key down
  ' bit 1 - new keypress event
  ' bit 2 - keyup event

  'In SDL2, unlike SDL 1.2 (unless SDL_DISABLE_LOCK_KEYS is set), the *lock
  'keys act like normal keys instead of telling whether the respective lock is on.
  '(Pause/Break still doesn't act as a normal key).
  'Maybe we should just report modifier state separately from button state, the same
  'way SDL2 does it.
  DIM kmod as SDL_Keymod = SDL_GetModState()
  keymod_to_keybdstate kmod AND KMOD_NUM,  scNumlock
  keymod_to_keybdstate kmod AND KMOD_CAPS, scCapslock
  'scScrollLock: No way to check scoll lock state?

  DIM msg as string
  FOR a as KBScancode = 0 TO &h7f
    keybdstate(a) = keybdstate(a) and 3  'Clear key-up bit
    keybdarray[a] = keybdstate(a)
    IF debugging_io ANDALSO keybdarray[a] THEN
      msg &= "  key[" & a & "](" & scancodename(a) & ")=" & keybdarray[a]
    END IF
    keybdstate(a) = keybdstate(a) and 1  'Clear new-keypress bit
  NEXT
  IF LEN(msg) THEN debuginfo "io_sdl2_keybits returning:" & msg

  keybdarray[scShift] = keybdarray[scLeftShift] OR keybdarray[scRightShift]
  keybdarray[scUnfilteredAlt] = keybdarray[scLeftAlt] OR keybdarray[scRightAlt]
  keybdarray[scCtrl] = keybdarray[scLeftCtrl] OR keybdarray[scRightCtrl]
END SUB

SUB io_sdl2_updatekeys(byval keybd as KeyBits ptr)
  'supports io_keybits instead
END SUB

'Enabling unicode will cause combining keys to go dead on X11 (on non-US
'layouts that have them). This usually means certain punctuation keys such as '
'On both X11 and Windows, disabling unicode input means SDL_KEYDOWN events
'don't report the character value (.unicode_).
SUB io_sdl2_enable_textinput (byval enable as integer)
END SUB

SUB io_sdl2_textinput (byval buf as wstring ptr, byval bufsize as integer)
  DIM out as wstring ptr = utf8_decode(@input_buffer[0])
  IF out = NULL THEN
    debug "io_sdl2_textinput: utf8_decode failed"
  ELSE
    *buf = LEFT(*out, bufsize)
    DEALLOCATE out
  END IF
  input_buffer = ""
END SUB

SUB io_sdl2_show_virtual_keyboard()
 'Does nothing on platforms that have real keyboards
#IFDEF __FB_ANDROID__
 if not virtual_keyboard_shown then
  SDL_ANDROID_ToggleScreenKeyboardWithoutTextInput()
  virtual_keyboard_shown = YES
 end if
#ENDIF
END SUB

SUB io_sdl2_hide_virtual_keyboard()
 'Does nothing on platforms that have real keyboards
#IFDEF __FB_ANDROID__
 if virtual_keyboard_shown then
  SDL_ANDROID_ToggleScreenKeyboardWithoutTextInput()
  virtual_keyboard_shown = NO
 end if
#ENDIF
END SUB

SUB io_sdl2_show_virtual_gamepad()
 'Does nothing on other platforms
#IFDEF __FB_ANDROID__
 if allow_virtual_gamepad then
  SDL_ANDROID_SetScreenKeyboardShown(YES)
 else
  debuginfo "io_sdl2_show_virtual_gamepad was supressed because of a previous call to internal_disable_virtual_gamepad"
 end if
#ENDIF
END SUB

SUB io_sdl2_hide_virtual_gamepad()
 'Does nothing on other platforms
#IFDEF __FB_ANDROID__
 SDL_ANDROID_SetScreenKeyboardShown(NO)
#ENDIF
END SUB

LOCAL SUB internal_disable_virtual_gamepad()
 'Does nothing on other platforms
#IFDEF __FB_ANDROID__
 io_sdl2_hide_virtual_gamepad
 allow_virtual_gamepad = NO
#ENDIF
END SUB

SUB io_sdl2_remap_android_gamepad(byval player as integer, gp as GamePadMap)
'Does nothing on non-android
#IFDEF __FB_ANDROID__
 SELECT CASE player
  CASE 0
   SDL_ANDROID_set_java_gamepad_keymap ( _
    scOHR2SDL(gp.A, SDL_SCANCODE_RETURN), _
    scOHR2SDL(gp.B, SDL_SCANCODE_ESCAPE), _
    0, _
    scOHR2SDL(gp.X, SDL_SCANCODE_ESCAPE), _
    scOHR2SDL(gp.Y, SDL_SCANCODE_ESCAPE), _
    0, _
    scOHR2SDL(gp.L1, SDL_SCANCODE_PAGEUP), _
    scOHR2SDL(gp.R1, SDL_SCANCODE_PAGEDOWN), _
    scOHR2SDL(gp.L2, SDL_SCANCODE_HOME), _
    scOHR2SDL(gp.R2, SDL_SCANCODE_END), _
    0, 0)
  CASE 1 TO 3
    SDL_ANDROID_set_ouya_gamepad_keymap ( _
    player, _
    scOHR2SDL(gp.Ud, SDL_SCANCODE_UP), _
    scOHR2SDL(gp.Rd, SDL_SCANCODE_RIGHT), _
    scOHR2SDL(gp.Dd, SDL_SCANCODE_DOWN), _
    scOHR2SDL(gp.Ld, SDL_SCANCODE_LEFT), _
    scOHR2SDL(gp.A, SDL_SCANCODE_RETURN), _
    scOHR2SDL(gp.B, SDL_SCANCODE_ESCAPE), _
    scOHR2SDL(gp.X, SDL_SCANCODE_ESCAPE), _
    scOHR2SDL(gp.Y, SDL_SCANCODE_ESCAPE), _
    scOHR2SDL(gp.L1, SDL_SCANCODE_PAGEUP), _
    scOHR2SDL(gp.R1, SDL_SCANCODE_PAGEDOWN), _
    scOHR2SDL(gp.L2, SDL_SCANCODE_HOME), _
    scOHR2SDL(gp.R2, SDL_SCANCODE_END), _
    0, 0)
  CASE ELSE
   debug "WARNING: io_sdl2_remap_android_gamepad: invalid player number " & player
 END SELECT
#ENDIF
END SUB

SUB io_sdl2_remap_touchscreen_button(byval button_id as integer, byval ohr_scancode as integer)
'Pass a scancode of 0 to disabled/hide the button
'Does nothing on non-android
#IFDEF __FB_ANDROID__
 SDL_ANDROID_SetScreenKeyboardButtonDisable(button_id, (ohr_scancode = 0))
 SDL_ANDROID_SetScreenKeyboardButtonKey(button_id, scOHR2SDL(ohr_scancode, 0))
#ENDIF
END SUB

FUNCTION io_sdl2_running_on_console() as bool
#IFDEF __FB_ANDROID__
 RETURN SDL_ANDROID_IsRunningOnConsole()
#ENDIF
 RETURN NO
END FUNCTION

FUNCTION io_sdl2_running_on_ouya() as bool
#IFDEF __FB_ANDROID__
 RETURN SDL_ANDROID_IsRunningOnOUYA()
#ENDIF
 RETURN NO
END FUNCTION

PRIVATE SUB update_mouse_visibility()
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

SUB io_sdl2_setmousevisibility(visibility as CursorVisibility)
  mouse_visibility = visibility
  update_mouse_visibility()
END SUB

'Get the origin of the displayed image, in unscaled window coordinates,
'and the actual zoom ratio in use (may differ from 'zoom')
SUB get_image_origin_and_ratio(byref origin as XYPair, byref ratio as double)
  DIM windowsz as XYPair
  SDL_GetWindowSize(mainwindow, @windowsz.w, @windowsz.h)
  ' Subtract for the origin position since when the window is fullscreened and
  ' not resizable window the image will be centred on the screen.
  DIM xratio as double = windowsz.w / framesize.w
  DIM yratio as double = windowsz.h / framesize.h
  ratio = small(xratio, yratio)
  origin = (windowsz - framesize * ratio) / 2
END SUB

'Convert a position on the client area of the window (e.g. as returned by SDL_GetMouseState)
'to position in the original unscaled image
'TODO: do SDL_GetWindowSize and SDL_GetMouseState return in screen coords or pixel coords?
'Not clear, but they don't agree, this will bread on high-DPI displays
FUNCTION windowpos_to_pixelpos(windowpos as XYPair, clamp as bool) as XYPair
  DIM origin as XYPair
  DIM ratio as double
  get_image_origin_and_ratio origin, ratio
  DIM pixelpos as XYPair
  'Should to use INT here (the floor function), NOT CINT, which is round-to-nearest,
  'rounding x.5 towards even, making cursor movement un-smooth at pixel-scale.
  pixelpos.x = INT((windowpos.x - origin.x) / ratio)
  pixelpos.y = INT((windowpos.y - origin.y) / ratio)
  IF clamp THEN
    pixelpos.x = bound(pixelpos.x, 0, framesize.w - 1)
    pixelpos.y = bound(pixelpos.y, 0, framesize.h - 1)
  END IF
  RETURN pixelpos
END FUNCTION

'Convert a position on the original unscaled image to position in the client area of the window
'TODO: do SDL_GetWindowSize and SDL_GetMouseState return in screen coords or pixel coords?
'Not clear, but they don't agree, this will bread on high-DPI displays
FUNCTION pixelpos_to_windowpos(pixelpos as XYPair) as XYPair
  DIM origin as XYPair
  DIM ratio as double
  get_image_origin_and_ratio origin, ratio

  RETURN origin + pixelpos * ratio + ratio / 2
END FUNCTION

'Change from SDL to OHR mouse button numbering (swap middle and right)
PRIVATE FUNCTION fix_buttons(byval buttons as integer) as integer
  DIM mbuttons as integer = 0
  IF SDL_BUTTON(SDL_BUTTON_LEFT) AND buttons THEN mbuttons = mbuttons OR mouseLeft
  IF SDL_BUTTON(SDL_BUTTON_RIGHT) AND buttons THEN mbuttons = mbuttons OR mouseRight
  IF SDL_BUTTON(SDL_BUTTON_MIDDLE) AND buttons THEN mbuttons = mbuttons OR mouseMiddle
  RETURN mbuttons
END FUNCTION

' Returns currently down mouse buttons, in SDL order, not OHR order
' TODO: report mouse position when over the window edge
LOCAL FUNCTION update_mouse() as integer
  DIM x as int32
  DIM y as int32
  DIM buttons as int32

  'TODO: do these functions return in screen coords or pixel coords? Not clear,
  'but it makes a difference on high-DPI displays

  IF SDL_GetWindowFlags(mainwindow) AND SDL_WINDOW_MOUSE_FOCUS THEN
    IF mouseclipped THEN
      buttons = SDL_GetRelativeMouseState(@x, @y)
      'debuginfo "gfx_sdl2: relativemousestate " & x & " " & y
      privatempos.x = bound(privatempos.x + x, mousebounds.p1.x, mousebounds.p2.x)
      privatempos.y = bound(privatempos.y + y, mousebounds.p1.y, mousebounds.p2.y)
    ELSE
      buttons = SDL_GetMouseState(@privatempos.x, @privatempos.y)
    END IF
  END IF
  #IFNDEF SDL_203
    IF buttons = 0 THEN SDL_CaptureMouse(NO)  'Any mouse drag ended
  #ENDIF
  RETURN buttons
END FUNCTION

SUB io_sdl2_mousebits (byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer, byref mclicks as integer)
  DIM buttons as integer
  buttons = update_mouse()

  DIM pixelpos as XYPair = windowpos_to_pixelpos(privatempos, YES)  'clamp=YES
  '?"mouse at " & privatempos & "(window), " & pixelpos & "(px)
  mx = pixelpos.x
  my = pixelpos.y
  mwheel = mousewheel
  mclicks = fix_buttons(mouseclicks)
  mbuttons = fix_buttons(buttons or mouseclicks)
  mouseclicks = 0
END SUB

SUB io_sdl2_getmouse(byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer)
  'supports io_mousebits instead
END SUB

SUB io_sdl2_setmouse(byval x as integer, byval y as integer)
  DIM windowpos as XYPair = pixelpos_to_windowpos(XY(x, y))
  '?"warp mouse to " & XY(x,y) & "(px) -> " & windowpos & "(window)"
  IF mouseclipped THEN
    privatempos = windowpos
  ELSE
    IF SDL_GetWindowFlags(mainwindow) AND SDL_WINDOW_INPUT_FOCUS THEN
      SDL_WarpMouseInWindow mainwindow, windowpos.x, windowpos.y
      SDL_PumpEvents  'Needed for SDL_WarpMouse to work?
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
  'FIXME: enabling clipping causes the mouse position to change.
  'In SDL 1.2 SDL_WM_GrabInput causes most WM key combinations to be blocked
  'Now in SDL 2, keyboard is not grabbed by default (see SDL_HINT_GRAB_KEYBOARD),
  'but I assume switching to relative mouse mode is effectively grabbing anyway.
  IF mouseclipped = NO ANDALSO (rect.p1.x >= 0) THEN
    'enter clipping mode
    mouseclipped = YES
    SDL_GetMouseState(@privatempos.x, @privatempos.y)
    SDL_SetRelativeMouseMode YES
  ELSEIF mouseclipped = YES ANDALSO (rect.p1.x = -1) THEN
    'exit clipping mode
    mouseclipped = NO
    SDL_SetRelativeMouseMode NO
    SDL_WarpMouseInWindow mainwindow, privatempos.x, privatempos.y
  END IF
  mousebounds.p1 = pixelpos_to_windowpos(rect.p1)
  mousebounds.p2 = pixelpos_to_windowpos(rect.p2)
END SUB

'This turns forced mouse clipping on or off
LOCAL SUB set_forced_mouse_clipping(byval newvalue as bool)
  newvalue = (newvalue <> 0)
  IF newvalue <> forced_mouse_clipping THEN
    forced_mouse_clipping = newvalue
    IF forced_mouse_clipping THEN
      IF mouseclipped = NO THEN
        internal_set_mouserect TYPE<RectPoints>((0, 0), framesize.w - 1, framesize.h - 1)
      END IF
      'If already clipped: nothing to be done
    ELSE
      internal_set_mouserect remember_mousebounds
    END IF
  END IF
END SUB

SUB io_sdl2_mouserect(byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
  DIM rect as RectPoints = ((xmin, ymin), (xmax, ymax))
  remember_mousebounds = rect
  IF forced_mouse_clipping AND xmin = -1 THEN
    'Remember that we are now meant to be unclipped, but clip to the window
    internal_set_mouserect TYPE<RectPoints>((0, 0), framesize.w - 1, framesize.h - 1)
  ELSE
    internal_set_mouserect rect
  END IF
END SUB

PRIVATE FUNCTION scOHR2SDL(byval ohr_scancode as KBScancode, byval default_sdl_scancode as integer=0) as integer
 'Convert an OHR scancode into an SDL scancode
 '(the reverse can be accomplished just by using the scantrans array)
 IF ohr_scancode = 0 THEN RETURN default_sdl_scancode
 FOR i as integer = 0 TO UBOUND(scantrans)
  IF scantrans(i) = ohr_scancode THEN RETURN i
 NEXT i
 RETURN 0
END FUNCTION

SUB io_sdl2_set_clipboard_text(text as zstring ptr)  'ustring
  CheckOK(SDL_SetClipboardText(text))
END SUB

FUNCTION io_sdl2_get_clipboard_text() as zstring ptr  'ustring
  RETURN SDL_GetClipboardText()
END FUNCTION

FUNCTION gfx_sdl2_setprocptrs() as integer
  gfx_init = @gfx_sdl2_init
  gfx_close = @gfx_sdl2_close
  gfx_getversion = @gfx_sdl2_getversion
  gfx_setpal = @gfx_sdl2_setpal
  gfx_screenshot = @gfx_sdl2_screenshot
  gfx_setwindowed = @gfx_sdl2_setwindowed
  gfx_windowtitle = @gfx_sdl2_windowtitle
  gfx_getwindowstate = @gfx_sdl2_getwindowstate
  gfx_get_screen_size = @gfx_sdl2_get_screen_size
  gfx_set_window_size = @gfx_sdl2_set_window_size
  gfx_supports_variable_resolution = @gfx_sdl2_supports_variable_resolution
  gfx_vsync_supported = @gfx_sdl2_vsync_supported
  gfx_get_resize = @gfx_sdl2_get_resize
  gfx_set_resizable = @gfx_sdl2_set_resizable
  gfx_recenter_window_hint = @gfx_sdl2_recenter_window_hint
  gfx_setoption = @gfx_sdl2_setoption
  gfx_describe_options = @gfx_sdl2_describe_options
  gfx_get_safe_zone_margin = @gfx_sdl2_get_safe_zone_margin
  gfx_set_safe_zone_margin = @gfx_sdl2_set_safe_zone_margin
  gfx_supports_safe_zone_margin = @gfx_sdl2_supports_safe_zone_margin
  gfx_ouya_purchase_request = @gfx_sdl2_ouya_purchase_request
  gfx_ouya_purchase_is_ready = @gfx_sdl2_ouya_purchase_is_ready
  gfx_ouya_purchase_succeeded = @gfx_sdl2_ouya_purchase_succeeded
  gfx_ouya_receipts_request = @gfx_sdl2_ouya_receipts_request
  gfx_ouya_receipts_are_ready = @gfx_sdl2_ouya_receipts_are_ready
  gfx_ouya_receipts_result = @gfx_sdl2_ouya_receipts_result
  io_init = @io_sdl2_init
  io_pollkeyevents = @io_sdl2_pollkeyevents
  io_waitprocessing = @io_sdl2_waitprocessing
  io_keybits = @io_sdl2_keybits
  io_updatekeys = @io_sdl2_updatekeys
  io_enable_textinput = @io_sdl2_enable_textinput
  io_textinput = @io_sdl2_textinput
  io_get_clipboard_text = @io_sdl2_get_clipboard_text
  io_set_clipboard_text = @io_sdl2_set_clipboard_text
  io_show_virtual_keyboard = @io_sdl2_show_virtual_keyboard
  io_hide_virtual_keyboard = @io_sdl2_hide_virtual_keyboard
  io_show_virtual_gamepad = @io_sdl2_show_virtual_gamepad
  io_hide_virtual_gamepad = @io_sdl2_hide_virtual_gamepad
  io_remap_android_gamepad = @io_sdl2_remap_android_gamepad
  io_remap_touchscreen_button = @io_sdl2_remap_touchscreen_button
  io_running_on_console = @io_sdl2_running_on_console
  io_running_on_ouya = @io_sdl2_running_on_ouya
  io_mousebits = @io_sdl2_mousebits
  io_setmousevisibility = @io_sdl2_setmousevisibility
  io_getmouse = @io_sdl2_getmouse
  io_setmouse = @io_sdl2_setmouse
  io_mouserect = @io_sdl2_mouserect
  io_get_joystick_state = @io_sdl2_get_joystick_state

  gfx_present = @gfx_sdl2_present

  RETURN 1
END FUNCTION


#include "gfx_sdl_common.bas"

END EXTERN
