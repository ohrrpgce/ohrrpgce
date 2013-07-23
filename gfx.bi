'' External graphics and IO functions

#include "udts.bi"
#include "const.bi"

extern "C"

type WindowState
	structsize as integer  'number of members in the struct, currently 3 (New API only!)
	focused as integer
	minimised as integer
end type
#define WINDOWSTATE_SZ 3

'terminate_signal_handler is a pointer to post_terminate_signal, for dynamically linked graphics backends.
'windowicon is platform specific: name of the icon resource on Windows, no meaning yet elsewhere
'(unrelated to gfx_init)
declare sub Gfx_backend_init (byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr)

extern Gfx_init as function (byval terminate_signal_handler as sub cdecl (), byval windowicon as zstring ptr, byval info_buffer as zstring ptr, byval info_buffer_size as integer) as integer
extern Gfx_close as sub ()

'(optional, ptr may be NULL)
extern Gfx_setdebugfunc as sub (byval debugc as sub cdecl (byval errorlevel as ErrorLevelEnum, byval message as zstring ptr))

'API version. Must return 1
extern Gfx_getversion as function () as integer

extern Gfx_showpage as sub (byval raw as ubyte ptr, byval w as integer, byval h as integer)

'set colour palette. May reuse last raw pointer to showpage, so you may not change it!
extern Gfx_setpal as sub (byval pal as RGBcolor ptr)
extern Gfx_screenshot as function (byval fname as zstring ptr) as integer
extern Gfx_setwindowed as sub (byval iswindow as integer)
extern Gfx_windowtitle as sub (byval title as zstring ptr)
extern Gfx_getwindowstate as function () as WindowState ptr

'(optional, temporary/experimental)
extern Gfx_getresize as function (byref ret as XYPair) as integer
extern Gfx_setresizable as sub (byval able as integer)

'gfx_setoption recieves an option name and the following option which may or may not be a related argument
'returns 0 if unrecognised, 1 if recognised but arg is ignored, 2 if arg is gobbled
extern Gfx_setoption as function (byval opt as zstring ptr, byval arg as zstring ptr) as integer
extern Gfx_describe_options as function () as zstring ptr

'(optional, ptr may be NULL, gfx_console only)
extern Gfx_printchar as sub (byval ch as integer, byval x as integer, byval y as integer)

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
extern Io_remap_android_gamepad as sub (byval A as integer, byval B as integer, byval X as integer, byval Y as integer, byval L1 as integer, byval R1 as integer, byval L2 as integer, byval R2 as integer)

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

declare sub blitohr(byval spr as Frame ptr, byval destspr as Frame ptr, byval pal as Palette16 ptr, byval startoffset as integer, byval startx as integer, byval starty as integer, byval endx as integer, byval endy as integer, byval trans as integer)
declare sub blitohrscaled(byval spr as Frame ptr, byval destspr as Frame ptr, byval pal as Palette16 ptr, byval x as integer, byval y as integer, byval startx as integer, byval starty as integer, byval endx as integer, byval endy as integer, byval trans as integer, byval scale as integer)

declare sub smoothzoomblit_8_to_8bit(byval srcbuffer as ubyte ptr, byval destbuffer as ubyte ptr, byval w as integer, byval h as integer, byval pitch as integer, byval zoom as integer, byval smooth as integer)
declare sub smoothzoomblit_8_to_32bit(byval srcbuffer as ubyte ptr, byval destbuffer as uinteger ptr, byval w as integer, byval h as integer, byval pitch as integer, byval zoom as integer, byval smooth as integer, byval pal as integer ptr)
declare sub smoothzoomblit_32_to_32bit(byval srcbuffer as uinteger ptr, byval destbuffer as uinteger ptr, byval w as integer, byval h as integer, byval pitch as integer, byval zoom as integer, byval smooth as integer)

end extern
