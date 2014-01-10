'' External graphics and IO functions

#IFNDEF GFX_BI
#DEFINE GFX_BI

' NOTE:
'  If a function is marked as optional, that means it may do nothing (either unimplemented
'  to irrelevant to the backend) and that a backend doesn't need to set the function pointer:
'  instead the pointer must be valid and set to a default in set_default_gfx_function_ptrs.
'  On the other hand "(optional, ptr may be NULL)", means the default is a NULL pointer.
'  Possibly-NULL function pointers are very much discouraged. They're used as a simple way
'  to indicate whether the function is implemented, or where calling an unimplemented function
'  would be an unnecessary performance hit.

#include "udts.bi"
#include "const.bi"

extern "C"

type WindowState
	structsize as integer  'number of members in the struct, currently 3 (New API only!)
	focused as integer
	minimised as integer
end type
#define WINDOWSTATE_SZ 3

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

'terminate_signal_handler is a pointer to post_terminate_signal, for dynamically linked graphics backends.
'windowicon is platform specific: name of the icon resource on Windows, no meaning yet elsewhere
'(unrelated to gfx_init)
declare sub Gfx_backend_init (byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr)

extern Gfx_init as function (byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr, byval info_buffer as zstring ptr, byval info_buffer_size as integer) as integer
extern Gfx_close as sub ()

'(optional, ptr may be NULL)
extern Gfx_setdebugfunc as sub (byval debugc as sub cdecl (byval errorlevel as ErrorLevelEnum, byval message as zstring ptr))

'(optional, ptr may be NULL) Returns a bitfield, eg. bit 1 on if supports API version 1.
'Not used by compiled-in backends. Dynamically linked backends must support version 1.
extern Gfx_getversion as function () as integer

extern Gfx_showpage as sub (byval raw as ubyte ptr, byval w as integer, byval h as integer)

'set colour palette. May reuse last raw pointer to showpage, so you may not change it!
extern Gfx_setpal as sub (byval pal as RGBcolor ptr)
extern Gfx_screenshot as function (byval fname as zstring ptr) as integer
extern Gfx_setwindowed as sub (byval iswindow as integer)
extern Gfx_windowtitle as sub (byval title as zstring ptr)
extern Gfx_getwindowstate as function () as WindowState ptr

'(optional) Returns whether the resolution can be changed to something other than 320x200 (via gfx_showpage)
'(This doesn't imply that gfx_set_resizable is supported)
'Returns false if the backend hasn't been updated or there are other constraints.
extern Gfx_supports_variable_resolution as function () as bool
'(optional) If a window resize was requested, returns true and sets ret. Otherwise must not modify ret.
extern Gfx_get_resize as function (byref ret as XYPair) as bool
'(optional) Enable or disable window resizing by the user, and optionally specify minimum window width/height.
'Returns new resizability state: false if the backend doesn't support it.
'Minimum window width/height may not work!
extern Gfx_set_resizable as function (enable as bool, min_width as integer, min_height as integer) as bool
'(optional) At the next gfx_showpage call, recentering the window would be a good idea.
'Called when starting a game.
extern Gfx_recenter_window_hint as sub ()

'gfx_setoption recieves an option name and the following option which may or may not be a related argument
'returns 0 if unrecognised, 1 if recognised but arg is ignored, 2 if arg is gobbled
extern Gfx_setoption as function (byval opt as zstring ptr, byval arg as zstring ptr) as integer
extern Gfx_describe_options as function () as zstring ptr

'(optional, ptr may be NULL, gfx_console only)
extern Gfx_printchar as sub (byval ch as integer, byval x as integer, byval y as integer)

'(optional)
extern Gfx_get_safe_zone_margin as function () as single
extern Gfx_set_safe_zone_margin as sub (byval margin as single)
extern Gfx_supports_safe_zone_margin as function () as bool

extern Gfx_ouya_purchase_request as sub(dev_id as string, identifier as string, key_der as string)
extern Gfx_ouya_purchase_is_ready as function () as bool
extern Gfx_ouya_purchase_succeeded as function () as bool
extern Gfx_ouya_receipts_request as sub (dev_id as string, key_der as string)
extern Gfx_ouya_receipts_are_ready as function () as bool
extern Gfx_ouya_receipts_result as function () as string

extern Io_init as sub ()

'(optional) called in loops where gfx_showpage is not.
extern Io_pollkeyevents as sub ()

'(optional) called every 5ms during waits
extern Io_waitprocessing as sub ()

'one of io_keybits or io_updatekeys, and one of io_mousebits or io_getmouse is required.

'(optional) Primary keyboard state function. Get keypress events (since last call) and keyboard state:
'bit 0: key down, bit 1: keypress since last call, must clear all other bits
'Length 128 array.
extern Io_keybits as sub (byval keybdarray as integer ptr)

'(optional, must be thread safe) Get current up/down state of each key. Only used by the polling thread, not needed otherwise
'set bit 3 (8) on each key if current down, should not modify the other bits!
extern Io_updatekeys as sub (byval keybd as integer ptr)

'(optional) Enable or disable text input methods, possibly causing some keys to go dead
'(stop reporting keypresses). See each backend or setkeys in allmodex.bas for details.
extern Io_enable_textinput as sub (byval enable as integer)

'(optional, ptr might be NULL) Get the inputted text since the last call, in UCS2 encoded unicode
extern Io_textinput as sub (byval buf as wstring ptr, byval bufsize as integer)

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
'(optional) Return true if the running device is a console (currently only supports OUYA)
extern Io_running_on_console as function () as bool

'(optional) Primary mouse state function
'mbuttons is bitmask of currently down keys OR new clicks, mclicks is bitmask ofnew clicks since last call.
'left, right, middle buttons are bits 0, 1, 2
'Mouse wheel not commonly implemented yet.
extern Io_mousebits as sub (byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer, byref mclicks as integer)

'(optional, must be thread safe) same as Io_mouse bits.
extern Io_getmouse as sub (byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer)

extern Io_setmouse as sub (byval x as integer, byval y as integer)
extern Io_setmousevisibility as sub (byval visible as integer)

'call io_mouserect(-1, -1, -1, -1) to disable clipping
extern Io_mouserect as sub (byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
extern Io_readjoysane as function (byval as integer, byref as integer, byref as integer, byref as integer) as integer


' functions in allmodex.bas

'Call on window or application close request event. Maybe ought to guarantee backend won't be reentered
'Allowed to be called from another thread.
declare sub post_terminate_signal ()

declare sub Io_amx_keybits (byval keybdarray as integer ptr)
declare sub Io_amx_mousebits (byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer, byref mclicks as integer)

' functions in blit.c

declare sub blitohr(byval spr as Frame ptr, byval destspr as Frame ptr, byval pal as Palette16 ptr, byval startoffset as int32, byval startx as int32, byval starty as int32, byval endx as int32, byval endy as int32, byval trans as int32)
declare sub blitohrscaled(byval spr as Frame ptr, byval destspr as Frame ptr, byval pal as Palette16 ptr, byval x as int32, byval y as int32, byval startx as int32, byval starty as int32, byval endx as int32, byval endy as int32, byval trans as int32, byval scale as int32)

declare sub smoothzoomblit_8_to_8bit(byval srcbuffer as ubyte ptr, byval destbuffer as ubyte ptr, byval w as int32, byval h as int32, byval pitch as int32, byval zoom as int32, byval smooth as int32)
declare sub smoothzoomblit_8_to_32bit(byval srcbuffer as ubyte ptr, byval destbuffer as uint32 ptr, byval w as int32, byval h as int32, byval pitch as int32, byval zoom as int32, byval smooth as int32, byval pal as int32 ptr)
declare sub smoothzoomblit_32_to_32bit(byval srcbuffer as uint32 ptr, byval destbuffer as uint32 ptr, byval w as int32, byval h as int32, byval pitch as int32, byval zoom as int32, byval smooth as int32)

end extern

#ENDIF
