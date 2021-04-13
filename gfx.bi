'OHRRPGCE - Graphics/IO backend API
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#IFNDEF GFX_BI
#DEFINE GFX_BI

' NOTE:
'  If a function is marked as optional, that means it may do nothing (either unimplemented
'  or irrelevant to the backend) and that a backend doesn't need to set the function pointer:
'  instead the pointer must be valid and set to a default in set_default_gfx_function_ptrs.
'  On the other hand "(optional, ptr may be NULL)", means the default is a NULL pointer.
'  Possibly-NULL function pointers are very much discouraged. They're used as a simple way
'  to indicate whether the function is implemented, or where calling an unimplemented function
'  would be an unnecessary performance hit.

#include "const.bi"
#include "backends.bi"
#include "surface.bi"
#include "util.bi"  'For XYPair

#define CURRENT_GFX_API_VERSION 2
' This is used by dynamically linked backends to report their compatibility
' (returned by gfx_getversion).
' Increment this when a change means that both the backend and the engine can't
' support previous versions.
' It isn't incremented when an optional feature is added to the API,
' (such as by increasing WINDOWSTATE_SZ), and might not be incremented if
' the engine can detect incompatible backends by missing functions.
' 1 - Original version, but there were actually multiple incompatible breaks
' 2 - Changes to Surface (inc. addition of pitch)


extern "C"

'==================================== Types ===================================

' Forward declarations
type FrameFwd as Frame
type Palette16Fwd as Palette16

type WindowState
	structsize as integer  'number of members in the struct, set to WINDOWSTATE_SZ
	focused as boolint
	minimised as boolint
	fullscreen as boolint
	unused as integer      'Obsolete, used to be user_toggled_fullscreen
	mouse_over as boolint
	windowsize as XYPair   'The actual size (client area), not the resolution seen by the engine
	zoom as integer
end type
#define WINDOWSTATE_SZ 8

type JoystickInfo
	'All of this data is optional.
	structsize as integer    'Number of members in the struct, set to JOYSTICKINFO_SZ by backend. Always at least 9
	instance_id as integer   'Uniquely identifies a joystick. (Some backends may not be to track
	                         'correctly if there are multiple joysticks plugged in at once).
	                         'Unplugging and replugging a joystick may assign a new ID.
	model_guid(15) as ubyte  'Identifies the model of hardware. Provided by winapi and SDL2
	name as zstring * 80     'E.g. concatenation of manufacturer name and product name, if both available
	have_bindings as boolint 'True if the backend could map buttons/axes to OHR ones (joyA etc) itself
	'Note: if have_bindings, then num_buttons and num_axes may be less than the last button/axis reported
	'in IOJoystickState, so num_buttons/num_axes/num_hats may be ignored.
	num_buttons as integer   'At most 32. 0 if not known.
	num_axes as integer      'At most 8.
	num_hats as integer      'At most 4.
	num_balls as integer     'I don't actually expect we will ever use this - backend should just report balls as axes
	'joytype as integer       'SDL_JoystickType and SDL_GameControllerType
end type
#define JOYSTICKINFO_SZ 9

const AXIS_LIMIT = 1000

type IOJoystickState
	structsize as integer    'Number of members in the struct, set to IOJOYSTICKSTATE_SZ by both engine and backend.
				 'Always at least 6.
	buttons_down as uinteger 'Whether each button is currently down. Starting from bit 0, unlike JoyButton
	                         'If info->have_bindings is true, then buttons are numbered according to JoyButton,
				 '(offset by -1 to start from 0), e.g. bit 0 is joyA, bit 1 is joyB
	buttons_new as uinteger  '(Optional) Whether a new keypress has happened for each button, since last poll
	axes(7) as integer       'Values from -AXIS_LIMIT to AXIS_LIMIT
	                         'If info->have_bindings is true, then axes are numbered according to JoyAxis.
	hats(3) as integer       'Length 4 bitvector: left=1, right=2, up=4, down=8
	info as JoystickInfo ptr '(Optional - can be NULL) This pointer must remain valid until backend shutdown
end type
#define IOJOYSTICKSTATE_SZ 6


type GamePadMap
	'For passing OHR scancodes to io_remap_android_gamepad
	Ud as integer
	Rd as integer
	Dd as integer
	Ld as integer
	A as integer
	B as integer
	X as integer
	Y as integer
	L1 as integer
	R1 as integer
	L2 as integer
	R2 as integer
end type

' This is used in a C interface, so need to force 32 bit type (enums may be 64 bit).
' -1 for compatibility with old io_setmousevisibility.
type CursorVisibility as integer
enum
	cursorHidden = 0   'Cursor always hidden
	cursorVisible = -1 'Cursor always shown, except on touch screens
	cursorDefault = -2 'Cursor shown when windowed, hidden in fullscreen
end enum

type EventEnum as integer
enum
	eventTerminate = 0        'Window or application close request event
	eventFullscreened = 1     'Windowed/fullscreen state changed by WM/user. arg1 is new fullscreen state
end enum

'Allowed to be called from another thread.
'Maybe ought to guarantee backend won't be reentered.
'Return value is INT_MIN if the event wasn't understood, and event-specific but generally 0 if it was.
type FnEventHandler as function (event as EventEnum, arg1 as intptr_t = 0, arg2 as intptr_t = 0) as integer


'============================== Engine Functions ==============================

'The following are in allmodex.bas, called by backend.
'Allowed to be called from another thread.
'Maybe ought to guarantee backend won't be reentered.

'Used by backend to send events to the engine. GfxInitData.PostEvent is a pointer to it.
'See FnEventHandler.
declare function post_event(event as EventEnum, arg1 as intptr_t = 0, arg2 as intptr_t = 0) as integer
'Call on window or application close request event (redundant to post_event)
declare sub post_terminate_signal ()


'============================== gfx Backend API ===============================

'(Obsolete, still supported but replaced by gfx_initialize)
'terminate_signal_handler is a pointer to post_terminate_signal, for dynamically linked graphics backends.
'windowicon is platform specific: name of the icon resource on Windows, no meaning yet elsewhere
extern Gfx_init as function (byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr, byval info_buffer as zstring ptr, byval info_buffer_size as integer) as integer
extern Gfx_close as sub ()

'(optional, ptr may be NULL)
extern Gfx_setdebugfunc as sub (byval debugc as sub cdecl (byval errorlevel as ErrorLevelEnum, byval message as zstring ptr))

' Returns CURRENT_GFX_API_VERSION (see above)
' Only used by dynamically linked, not compiled-in backends.
extern Gfx_getversion as function () as integer

' Tell backend to display an 8-bit or 32-bit screen buffer.
' pPalette is required for 8-bit Surfaces and ignored for 32-bit ones.
' Returns 0 on success
extern Gfx_present as function ( byval pSurfaceIn as Surface ptr, byval pPalette as RGBPalette ptr ) as integer

' Change colour palette. If the last gfx_present was 8-bit it will be
' redisplayed, otherwise this does nothing.
extern Gfx_setpal as sub (byval pal as RGBcolor ptr)

extern Gfx_screenshot as function (byval fname as zstring ptr) as integer
extern Gfx_setwindowed as sub (byval iswindow as integer)
extern Gfx_windowtitle as sub (byval title as zstring ptr)
extern Gfx_getwindowstate as function () as WindowState ptr
'(optional) Size in pixels of the (primary) monitor. Sets values to 0 if can't retrieve it.
'If possible, tries to exclude size of WM tool/taskbars.
'NOTE: call get_screen_size instead of this.
extern Gfx_get_screen_size as sub (wide as integer ptr, high as integer ptr)

'(optional, ptr may be NULL) Set the window size and scale/zoom at the same time.
'This is the new way of changing window size, supported by gfx_sdl2. It's an alternative
'to calling gfx_present with a new frame, which is still preferred because it can repaint the
'window at the same time.
'This will count as a resize request, causing gfx_get_resize() to return the new resolution.
extern Gfx_set_window_size as sub (byval newsize as XYPair, newzoom as integer)

'(optional) Returns whether the resolution can be changed to something other than 320x200 (via gfx_present)
'(This doesn't imply that gfx_set_resizable is supported)
'Returns false if the backend hasn't been updated or there are other constraints.
extern Gfx_supports_variable_resolution as function () as bool
'(optional) If a window resize was requested, returns true and sets ret. Otherwise must not modify ret.
extern Gfx_get_resize as function (byref ret as XYPair) as bool
'(optional) Enable or disable window resizing by the user, and optionally specify minimum window width/height
'(not including the zoom factor), but width/height may not be supported!
'If enable=NO then min_width/height are ignored. Also ignored if equal to 0.
'Returns new resizability state: false if the backend doesn't support it.
extern Gfx_set_resizable as function (enable as bool, min_width as integer = 0, min_height as integer = 0) as bool
'(optional) At the next gfx_present call, recentering the window would be a good idea.
'Called when starting a game.
extern Gfx_recenter_window_hint as sub ()
'(optional) Whether vsync is supported
extern Gfx_vsync_supported as function () as bool

'gfx_setoption recieves an option name and the following option which may or may not be a related argument
'returns 0 if unrecognised, 1 if recognised but arg is ignored, 2 if arg is gobbled
extern Gfx_setoption as function (byval opt as zstring ptr, byval arg as zstring ptr) as integer
extern Gfx_describe_options as function () as zstring ptr

'(optional, ptr may be NULL, gfx_console only)
extern Gfx_printchar as sub (byval ch as integer, byval x as integer, byval y as integer, byval col as integer)

'(optional)
extern Gfx_get_safe_zone_margin as function () as single
'(optional)
extern Gfx_set_safe_zone_margin as sub (byval margin as single)
'(optional)
extern Gfx_supports_safe_zone_margin as function () as bool

'(optional)
extern Gfx_ouya_purchase_request as sub(dev_id as string, identifier as string, key_der as string)
'(optional)
extern Gfx_ouya_purchase_is_ready as function () as bool
'(optional)
extern Gfx_ouya_purchase_succeeded as function () as bool
'(optional)
extern Gfx_ouya_receipts_request as sub (dev_id as string, key_der as string)
'(optional)
extern Gfx_ouya_receipts_are_ready as function () as bool
'(optional)
extern Gfx_ouya_receipts_result as function () as string


'=============================== io Backend API ===============================


'KeyBits
'The state of a key/button. In different contexts the bits get used differently:
'the io backend API only uses certain bits; they are stored differently in KeyArray,
'and keyval modifies the bits before returning them.
'1:  key currently down
'2:  keypress event (either new keypress or key repeat) - Note: keypresses
'    due to key repeat get added by keyval, they aren't stored in KeyArray
'4:  new keypress
'8:  set by io_updatekeys() only, normally 0 (not stored in KeyArray or anywhere else)
type KeyBits as integer


extern Io_init as sub ()

'(optional) called in loops where gfx_present is not.
extern Io_pollkeyevents as sub ()

'(optional) called every 5ms during waits
extern Io_waitprocessing as sub ()

'one of io_keybits or io_updatekeys, and one of io_mousebits or io_getmouse is required.

'(optional) Primary keyboard state function. Get keypress events (since last call) and keyboard state:
'bit 0: key down, bit 1: keypress since last call, must clear all other bits
'Length 128 array.
extern Io_keybits as sub (byval keybdarray as KeyBits ptr)

'(optional, must be thread safe) Get current up/down state of each key. Only used by the polling thread, not needed otherwise
'set bit 3 (8) on each key if current down, should not modify the other bits!
extern Io_updatekeys as sub (byval keybd as KeyBits ptr)

'(optional) Enable or disable text input methods, possibly causing some keys to go dead
'(stop reporting keypresses). See each backend or setkeys in allmodex.bas for details.
extern Io_enable_textinput as sub (byval enable as integer)

'(optional, ptr might be NULL) Get the inputted text since the last call, in UCS2 encoded unicode
extern Io_textinput as sub (byval buf as wstring ptr, byval bufsize as integer)

'(optional) Returns a UTF8 string, or either "" or NULL if the clipboard is unavailable or doesn't contain text
'The result must be DEALLOCATE'd!
extern Io_get_clipboard_text as function () as zstring ptr
'(optional) Sets the OS clipboard to a UTF8 string
extern Io_set_clipboard_text as sub (text as zstring ptr)

'(optional) Display the virtual keyboard, but only for platforms like Android that need it
extern Io_show_virtual_keyboard as sub ()
'(optional) Hide the virtual keyboard, but only for platforms like Android that need it
extern Io_hide_virtual_keyboard as sub ()
'(optional) Display the virtual gamepad, but only for platforms like Android that need it
extern Io_show_virtual_gamepad as sub ()
'(optional) Hide the virtual gamepad, but only for platforms like Android that need it
extern Io_hide_virtual_gamepad as sub ()
'(optional) Runtime remapping of android controller buttons
extern Io_remap_android_gamepad as sub (byval player as integer, gp as GamePadMap)
'(optional) Runtime remapping of touchscreen virtual gamepad buttons
extern Io_remap_touchscreen_button as sub (byval button_id as integer, byval ohr_scancode as integer)
'(optional) Return true if the running device is a console (currently only supports OUYA, GameStick, Fire-TV)
extern Io_running_on_console as function () as bool
'(optional) Return true if the running device is an OUYA (determines if OUYA billing APIs can be attempted)
extern Io_running_on_ouya as function () as bool

'(optional) Primary mouse state function
'All of the arguments should be overwritten with new values:
'mbuttons is bitmask of currently down keys OR new clicks, mclicks is bitmask of new clicks since last call.
'left, right, middle buttons are bits 0, 1, 2
'mwheel is the mouse wheel position, not the wheel delta. It should be 120 per
'dedent ('tick', 15 degree rotation), but not necessarily a multiple of 120 for
'a free-scrolling mouse (e.g. touch interface). Increases rotating away from you.
extern Io_mousebits as sub (byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer, byref mclicks as integer)

'(optional, must be thread safe) same as Io_mouse bits.
extern Io_getmouse as sub (byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer)

extern Io_setmouse as sub (byval x as integer, byval y as integer)
extern Io_setmousevisibility as sub (byval visibility as CursorVisibility)

'call io_mouserect(-1, -1, -1, -1) to disable clipping
extern Io_mouserect as sub (byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)

'(optional, ptr may be NULL)
'Poll state of a joystick. (Old)
'Returns nonzero on success, 0 if the joystick can't be read.
'buttons is a bitvector of up to 32 buttons (starting from bit 0, unlike JoyButton)
'jx/jy are the first two axes, in the range from -100 to 100
extern Io_readjoysane as function (byval joynum as integer, byref buttons as uinteger, byref jx as integer, byref jy as integer) as integer

'(optional, ptr may be NULL)
'Poll state of a joystick. (New)
'state is wiped clean before being handed to the backend.
'Returns > 0 on an error.
' -1: acquired new joystick
'  0: success
'  1: couldn't open/joystick index is out of range
'  2: previously acquired joystick is gone and should be dropped
'  3: we don't have input focus currently (temporarily can't be read)
extern Io_get_joystick_state as function (byval joynum as integer, byval state as IOJoystickState ptr) as integer


'=========================== Backend API wrappers =============================
' functions in allmodex.bas

declare sub Io_amx_keybits (byval keybdarray as KeyBits ptr)
declare sub Io_amx_mousebits (byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer, byref mclicks as integer)


'=============================== Blitting API =================================
' functions in blit.c

declare sub blitohr(byval spr as FrameFwd ptr, byval destspr as FrameFwd ptr, byval pal as Palette16Fwd ptr, byval startoffset as int32, byval startx as int32, byval starty as int32, byval endx as int32, byval endy as int32, byval trans as boolint, byref opts as DrawOptions)
declare sub blitohrscaled(byval spr as FrameFwd ptr, byval destspr as FrameFwd ptr, byval pal as Palette16Fwd ptr, byval x as int32, byval y as int32, byval startx as int32, byval starty as int32, byval endx as int32, byval endy as int32, byval trans as boolint, byref opts as DrawOptions)

declare sub smoothzoomblit_8_to_8bit(byval srcbuffer as ubyte ptr, byval destbuffer as ubyte ptr, byval size as XYPair, byval pitch as int32, byval zoom as int32, byval smooth as int32, byval dummypal as RGBcolor ptr = 0)
declare sub smoothzoomblit_8_to_32bit(byval srcbuffer as ubyte ptr, byval destbuffer as uint32 ptr, byval size as XYPair, byval pitch as int32, byval zoom as int32, byval smooth as int32, byval pal as RGBcolor ptr)
declare sub smoothzoomblit_32_to_32bit(byval srcbuffer as RGBcolor ptr, byval destbuffer as uint32 ptr, byval size as XYPair, byval pitch as int32, byval zoom as int32, byval smooth as int32, byval dummypal as RGBcolor ptr = 0)

end extern

#ENDIF
