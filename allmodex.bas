'OHRRPGCE - the graphics, audio and user input library!
'(C) Copyright 1997-2017 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
'
'This module is completely bool-clean (bool always used when appropriate)

#include "config.bi"
#include "crt/string.bi"
#include "crt/limits.bi"
#include "string.bi"
#include "cmdline.bi"
#include "common.bi"
#include "allmodex.bi"
#include "gfx.bi"
#include "surface.bi"
#include "matrixMath.bi"
#include "lib/gif.bi"
#include "lib/lodepng.bi"
#include "lib/ujpeg.bi"
#include "lib/jo_jpeg.bi"
#include "music.bi"
#include "reload.bi"
#include "util.bi"
#include "const.bi"
#include "uiconst.bi"
#include "slices.bi"
#include "loading.bi"
#include "steam.bi"

using Reload

#ifdef IS_GAME
	#include "game.bi"  'For exit_gracefully
#endif

#ifdef IS_CUSTOM
	#include "cglobals.bi"  'For channel_to_Game
#endif

#ifdef __FB_ANDROID__
'This is gfx_sdl specific, of course, but a lot of the stuff in our fork of the android fork
'of SDL 1.2 would more make sense to live in totally separate java files, which is something we will
'want to do to support SDL 2 on Android.
extern "C"
	'Return value is always 1
	declare function SDL_ANDROID_EmailFiles(address as zstring ptr, subject as zstring ptr, message as zstring ptr, file1 as zstring ptr = NULL, file2 as zstring ptr = NULL, file3 as zstring ptr = NULL) as integer
end extern
#endif


'Note: While non-refcounted frames work (at last check), it's not used anywhere, and you most probably do not need it
'NOREFC is also used to indicate uncached Palette16's. Note Palette16's are NOT refcounted in the way as frames
const NOREFC = -1234
const FREEDREFC = -4321

type XYPair_node 	'only used for floodfill
	x as integer
	y as integer
	nextnode as XYPair_node ptr
end type


'----------- Local functions ----------

declare sub _frame_copyctor cdecl(dest as Frame ptr ptr, src as Frame ptr ptr)
declare sub init_frame_with_surface(ret as Frame ptr, surf as Surface ptr)
declare sub reload_global_animations(def_anim as SpriteSet ptr, sprtype as SpriteType)

declare sub frame_draw_internal(src as Frame ptr, masterpal() as RGBcolor, pal as Palette16 ptr = NULL, x as integer, y as integer, trans as bool = YES, dest as Frame ptr, opts as DrawOptions = def_drawoptions)
declare sub draw_clipped(src as Frame ptr, pal as Palette16 ptr = NULL, x as integer, y as integer, trans as bool = YES, dest as Frame ptr, opts as DrawOptions)
declare sub draw_clipped_scaled(src as Frame ptr, pal as Palette16 ptr = NULL, x as integer, y as integer, trans as bool = YES, dest as Frame ptr, opts as DrawOptions)
declare sub draw_clipped_surf(src as Surface ptr, master_pal as RGBcolor ptr, pal as Palette16 ptr = NULL, x as integer, y as integer, trans as bool = YES, dest as Surface ptr, opts as DrawOptions = def_drawoptions)

'declare sub grabrect(page as integer, x as integer, y as integer, w as integer, h as integer, ibuf as ubyte ptr, tbuf as ubyte ptr = 0)
declare function write_bmp_header(filen as string, w as integer, h as integer, bitdepth as integer) as integer
declare function decode_bmp_bitmask(mask as uint32) as integer
declare sub loadbmp32(bf as integer, surf as Surface ptr, infohd as BITMAPV3INFOHEADER)
declare sub loadbmp24(bf as integer, surf as Surface ptr)
declare sub loadbmp8(bf as integer, fr as Frame ptr)
declare sub loadbmp4(bf as integer, fr as Frame ptr)
declare sub loadbmp1(bf as integer, fr as Frame ptr)
declare sub loadbmprle8(bf as integer, fr as Frame ptr)
declare sub loadbmprle4(bf as integer, fr as Frame ptr)

declare function next_unused_screenshot_filename() as string
declare sub snapshot_check(page as integer = -1)

declare function calcblock(tmap as TileMap, x as integer, y as integer, overheadmode as integer, pmapptr as TileMap ptr) as integer

declare function compatpage_internal(pageframe as Frame ptr) as Frame ptr

declare sub screen_size_update ()
declare sub masterpal_changed()

declare sub pollingthread(as any ptr)
declare sub keystate_convert_bit3_to_keybits(keystate() as KeyBits)
declare function read_inputtext () as string
declare sub update_mouse_state ()

declare sub load_replay_header ()
declare sub record_input_tick ()
declare sub replay_input_tick ()
declare sub read_replay_length ()

declare function draw_allmodex_recordable_overlays (page as integer) as bool
declare function draw_allmodex_overlays (page as integer) as bool
declare sub show_replay_overlay()
declare sub hide_overlays ()
declare sub update_fps_counter (skipped as bool)
declare sub allmodex_controls ()
declare sub replay_controls ()

declare function hexptr(p as any ptr) as string

declare sub Palette16_delete(f as Palette16 ptr ptr)
declare function palette16_load_pal_single(fh as integer) as Palette16 ptr


#define POINT_CLIPPED(x, y) ((x) < cliprect.l orelse (x) > cliprect.r orelse (y) < cliprect.t orelse (y) > cliprect.b)

#define FRAMEPIXEL(x, y, fr) fr->image[fr->pitch * (y) + (x)]

' In a function, pass return value on error
' NULL .image ptr indicates that either the Frame is Surface-backed or it's corrupt,
' so might as well check that instead of ->surf. This is misnamed: doesn't allow 8-bit surfaces.
#define CHECK_FRAME_8BIT(fr, retwhat...) FAIL_IF((fr)->image = NULL, " NULL Frame.image", retwhat)



'------------ Global variables ------------

dim modex_initialised as bool = NO
dim vpages() as Frame ptr
dim vpagesp as Frame ptr ptr  'points to vpages(0) for debugging: fbc outputs typeless debugging symbol
dim shared default_page_bitdepth as integer = 8  '8 or 32. Affects allocatepage only, set by switch_to_*bit_vpages()
dim faded_in as bool = YES     'NO when screen is faded to a color, YES when faded to a palette
dim faded_to_color as RGBcolor 'If faded_in=NO, the color the screen is faded to

'Whether the player has at any point toggled fullscreen/windowed in some low-level way
'like alt+enter or window buttons.
dim user_toggled_fullscreen as bool = NO

'The -input-debug cmdline option: causes gfx backends to print info about events and other user/OS input
dim debugging_io as bool = NO

'Amount of time (in seconds) that the user has been actively using the program. Stops counting if no input
dim active_seconds as double
'Seconds without input after which to stop increasing time
dim idle_time_threshold as double = 30.
'When last input arrived
dim shared last_active_time as double

dim def_drawoptions as DrawOptions

redim fonts(fontLAST) as Font ptr

'Toggles 0-1 every time dowait is called
dim global_tog as integer

'Convert scancodes to text; Enter does not insert newline!
'This array is a global instead of an internal detail because it's used by charpicker and the font editor
'to work out key mapping for the extended characters. Would be nice if it weren't needed.
'FIXME: discover why this array is filled with empty values on Android
'key2text(0,*): no modifiers
'key2text(1,*): shift
'key2text(2,*): alt
'key2text(3,*): alt+shift
dim key2text(3,53) as string*1 => { _
	{"", "", "1","2","3","4","5","6","7","8","9","0","-","=","","","q","w","e","r","t","y","u","i","o","p","[","]","","","a","s","d","f","g","h","j","k","l",";","'","`","","\","z","x","c","v","b","n","m",",",".","/"}, _
	{"", "", "!","@","#","$","%","^","&","*","(",")","_","+","","","Q","W","E","R","T","Y","U","I","O","P","{","}","","","A","S","D","F","G","H","J","K","L",":","""","~","","|","Z","X","C","V","B","N","M","<",">","?"}, _
	{"", "", !"\130",!"\131",!"\132",!"\133",!"\134",!"\135",!"\136",!"\137",!"\138",!"\139",!"\140",!"\141","","",!"\142",!"\143",!"\144",!"\145",!"\146",!"\147",!"\148",!"\149",!"\150",!"\151",!"\152",!"\153","","",!"\154",!"\155",!"\156",!"\157",!"\158",!"\159",!"\160",!"\161",!"\162",!"\163",!"\164",!"\165","",!"\166",!"\167",!"\168",!"\169",!"\170",!"\171",!"\172",!"\173",!"\174",!"\175",!"\176"}, _
	{"", "", !"\177",!"\178",!"\179",!"\180",!"\181",!"\182",!"\183",!"\184",!"\185",!"\186",!"\187",!"\188","","",!"\189",!"\190",!"\191",!"\192",!"\193",!"\194",!"\195",!"\196",!"\197",!"\198",!"\199",!"\200","","",!"\201",!"\202",!"\203",!"\204",!"\205",!"\206",!"\207",!"\208",!"\209",!"\210",!"\211",!"\212","",!"\213",!"\214",!"\215",!"\216",!"\217",!"\218",!"\219",!"\220",!"\221",!"\222",!"\223"} _
}
' Translate scancodes scNumpadSlash and up to ASCII.
' Again, Enter is skipped.
' *, -, + are missing, since their scancodes aren't contiguous with the others.
dim shared numpad2text(...) as string*1 => {"7","8","9","","4","5","6","","1","2","3","0","."}

' Frame type table
DEFINE_VECTOR_OF_TYPE_COMMON(Frame ptr, Frame_ptr, @_frame_copyctor, @frame_unload)


'--------- Module shared variables ---------

'For each vpage() element, this records whether it shouldn't be resized when the window size changes (normally is)
'(Not fully implemented, as it seems it would only benefit textbox_appearance_editor)
'dim shared fixedsize_vpages() as bool

dim shared tlsKeyClipRect as TLSKey

'The current internal size of the window (takes effect at next setvispage).
'Should only be modified via set_resolution and unlock_resolution
dim shared windowsize as XYPair = (320, 200)
'Remember gfx scale/zoom when no gfx backend is initialised (default to not changing it)
'Don't use this otherwise! Call gfx_getwindowstate instead.
dim shared remember_scale as integer = -1
'Minimum window size; can't resize width or height below this. Default to (0,0): no bound
'Also equal to (0,0) when window isn't resizeable.
dim shared minwinsize as XYPair
dim shared resizing_requested as bool = NO  'Whether we want the window to be resizable, even if unsupported
dim shared resizing_enabled as bool = NO    'Current backend state (maybe this should be in WindowState instead)

'State for drawing maps (I wish we didn't have any global state)
dim shared bordertile as integer
'Tileset animation states
dim shared anim1 as integer
dim shared anim2 as integer

type SkippedFrame
	page as integer = -1

	declare sub drop()
	declare sub show()
end type

dim shared waittime as double
dim shared flagtime as double = 0.0
dim shared setwait_called as bool
dim shared tickcount as integer = 0
dim use_speed_control as bool = YES
dim shared ms_per_frame as integer = 55     'This is only used by the animation system, not the framerate control
dim shared requested_framerate as double    'Set by last setwait
dim shared base_fps_multiplier as double = 1.0 'Doesn't include effect of shift+tab
dim shared fps_multiplier as double = 1.0   'Effect speed multiplier, affects all setwait/dowaits
dim max_display_fps as integer = 60         'Skip frames if drawing more than this.
dim shared frame_index as integer           'Count number of setvispage calls
dim shared skipped_frame as SkippedFrame    'Records the last setvispage call if it was frameskipped.

dim shared last_setvispage as integer = -1  'Records the last setvispage. -1 if none.
                                            'Virtually always vpage; in fact using anything other than vpage
                                            'would cause a lot of functions like multichoice to glitch.
                                            'Don't use this directly; call getvispage instead!

dim shared log_slow as bool = NO            'Enable spammy debug_if_slow logging

'If this is defined, the effect of vsync on Macs is simulated, where gfx_present
'blocks until vsync occurs. Other OSes might do the same, but generally don't.
'Note: this simulates vsync at 60fps regardless of max_display_fps
'#define SIMULATE_BLOCKING_VSYNC

type InputStateFwd as InputState

' Shared by KeyboardState and JoystickState, this holds down/triggered/new-press
' state of an array of keys/buttons.
type KeyArray extends Object
	keys(any) as KeyBits                'State of each key
	key_down_ms(any) as integer         'ms each key has been down
	arrow_key_down_ms as integer        'Max ms that any arrow key has been down
	controls(any) as ControlKey         'Mapping from scancodes to controls
	                                    'The reason that controls() is in this UDT is so that we can
	                                    'replay input which has controls set up differently.

	' Redim the arrays. (Not a constructor, because that's a nuisance for globals)
	declare sub init(maxkey as integer)
	declare abstract sub init_controls()
	declare abstract function is_arrow_key(key as integer) as bool
	declare sub update_keydown_times(inputst as InputStateFwd)
	'In following, key is a KBScancode or JoyButton depending on subclass
	declare function key_repeating(key as integer, repeat_wait as integer, repeat_rate as integer, inputst as InputStateFwd) as KeyBits
	declare abstract function keyval(key as integer, repeat_wait as integer = 0, repeat_rate as integer = 0, inputst as InputStateFwd) as KeyBits
	declare abstract function anykey(inputst as InputStateFwd) as KeyBits
	declare function controlkey(key as KBScancode, inputst as InputStateFwd, repeat_wait as integer = 0, repeat_rate as integer = 0) as KeyBits
	declare sub clearkeys()
end type

type KeyboardState extends KeyArray
	delayed_alt_keydown as bool = NO    'Whether have delayed reporting an ALT keypress
	inputtext as string

	declare constructor()
	declare sub init_controls()
	declare sub reset()
	declare sub update_keybits()
	declare function is_arrow_key(key as KBScancode) as bool
	declare function numpad_alias_key(key as KBScancode) as KBScancode
	declare function keyval(key as KBScancode, repeat_wait as integer = 0, repeat_rate as integer = 0, inputst as InputStateFwd) as KeyBits
	declare function anykey(inputst as InputStateFwd) as KeyBits
end type

dim shared last_setkeys_time as double      'Used to compute real_input.elapsed_ms
dim shared inputtext_enabled as bool = NO   'Whether to fetch real_input.kb.inputtext, not applied to replay_input.kb
dim shared remap_numpad as bool = YES       'If YES, then when numlock is off remap numpad .0-9 to arrows/home/etc

'If true, assume a US keyboard layout (use get_ascii_inputtext instead of calling io_enable_textinput + io_textinput).
'Used as workaround for #1064.
dim shared disable_native_text_input as bool = NO
'True if disable_native_text_input set by a commandline or config option
dim shared overrode_native_text_input as bool = NO

dim shared joysticks_globally_disabled as bool = NO

type JoystickState extends KeyArray
	state as IOJoystickState

	' Configuration
	axis_threshold as integer = AXIS_LIMIT / 2

	declare constructor()
	declare sub init_controls()
	declare sub update_keybits(joynum as integer)
	declare function is_arrow_key(key as JoyButton) as bool
	declare function keyval(key as JoyButton, repeat_wait as integer = 0, repeat_rate as integer = 0, inputst as InputStateFwd) as KeyBits
	declare function anykey(inputst as InputStateFwd) as KeyBits
end type

' Keyboard and joystick state which is separate for recording and replaying.
' (In future will include mouse too, once record/replay is implemented for mouse)
type InputState
	'Shared between keyboard and joysticks
	elapsed_ms as integer               'Time since last setkeys call (used by key_repeating)
	repeat_wait as integer = 500        'ms before keys start to repeat
	repeat_rate as integer = 55         'repeat interval, in ms

	kb as KeyboardState
	joys(3) as JoystickState

	declare function controlkey(key as KBScancode, repeat_wait as integer = 0, repeat_rate as integer = 0) as KeyBits
end type

dim shared real_input as InputState         'Always contains real state even if replaying
dim shared replay_input as InputState       'Contains replayed input state while replaying, else unused
                                            'NOTE! Recording/replaying joysticks not implemented yet!

'Singleton type
type ReplayState
	active as bool             'Currently replaying input and not paused
	paused as bool             'While paused, keyval, etc, act on real_input.kb.
	filename as string         'Used only for error messages.
	file as integer = -1       'File handle
	tick as integer = -1       'Counts number of ticks we've replayed
	fpos as integer            'Debugging only: File offset of the tick chunk
	nexttick as integer = -1   'If we read the next tickcount from the file before it's needed
	                           'it's stored here. Otherwise -1.
	next_tick_ms as integer = 55 'Next tick milliseconds read before it's needed.
	debug as bool = NO         'Set to YES by editing this line; maybe add a commandline option
	length_ticks as integer    'Length in ticks (max tick num)
	length_ms as integer       'Approximate length of the replay, in milliseconds
	play_position_ms as integer 'Approximate position in replay in ms (calculated in same way as length_ms)
	repeat_count as integer    'Number of times to repeat the playback
	repeats_done as integer    'Number of repeats already finished.
end type

'Singleton type for recording input.
type RecordState
	file as integer = -1       'File handle
	active as bool             'Currently recording input and not paused.
	paused as bool             'While paused, calls to setkeys don't affect recording.
	tick as integer = -1       'Tick number, starting from zero.
	debug as bool = NO         'Set to YES by editing this line; maybe add a commandline option
	last_kb as KeyboardState   'Keyboard state during previous recorded tick
end type

dim shared replay as ReplayState
dim shared record as RecordState
dim shared macrofile as string

'Abstract base class for recording a video (eg .gif)
type VideoRecorder extends Object
	declare virtual property active() as bool
	declare abstract sub stop()
	declare abstract sub record_frame(fr as Frame ptr, pal() as RGBcolor)
end type

type GIFRecorder extends VideoRecorder
	'active as bool
	writer as GifWriter
	fname as string
	secondscreen as string           'When recording combined editor+player .gif: path to player screen file
	last_frame_end_time as double    'Nominal time when the delay for the last frame we wrote ends

	declare constructor(outfile as string, secondscreen as string = "")
	declare property active() as bool
	declare sub stop()
	declare sub record_frame(fr as Frame ptr, pal() as RGBcolor)
	declare function calc_delay() as integer
end type

'Class for saving a screenshot to a certain file every frame
type ScreenForwarder extends VideoRecorder
	fname as string

	declare constructor(outfile as string)
	declare property active() as bool
	declare sub stop()
	declare sub record_frame(fr as Frame ptr, pal() as RGBcolor)
end type

dim shared recordvid as VideoRecorder ptr

dim shared gif_max_fps as integer = 30
dim shared screenshot_record_overlays as bool = NO
dim shared gif_show_keys_overlay as bool   'Whether to display pressed keys overlay while recording a gif
dim shared show_mouse_overlay as bool      'Whether to draw mouse location overlay (regardless of gif recording)

dim shared loaded_screenshot_settings as bool = NO
dim shared screenshot_format as string
dim shared use_gfx_screenshot as bool

dim shared closerequest as bool = NO     'It has been requested to close the program.

dim gfxmutex as any ptr                  '(Global) Coordinates access to globals and gfx backend with the polling thread
dim main_thread_in_gfx_backend as bool   '(Global) Whether the main thread has acquired gfxmutex.

'State variables for the pollingthread
type PollingThreadState
	threadptr as any ptr          'id of the polling thread
	wantquit as bool              'signal the polling thread to quit
	keybdstate(scLAST) as KeyBits '"real"time keyboard array
	mousebuttons as integer
	mouselastbuttons as integer
end type
dim shared pollthread as PollingThreadState

'State of the mouse (set when setkeys is called), includes persistent state
dim shared mouse_state as MouseInfo
dim shared last_mouse_wheel as integer   'mouse_state.wheel at previous update_mouse_state call.
dim shared cursorvisibility as CursorVisibility = cursorDefault

dim shared textfg as integer
dim shared textbg as integer

'Master palette copies internal to allmodex.
'(Length 257 because they double as RGBPalettes)
'master() is usually equal to these two, but does not take effect until setpal or fadein is called.
'displaypal is used for display (including screenshots and gifs), while curmasterpal is for drawing.
'curmasterpal is used for colors drawn to a 32-bit vpage, and for nearcolor lookups when drawing
'to a 8-bit vpage (e.g. drawing with blending), and for exporting.
'In 32-bit mode, displaypal is mever used; in 8-bit mode, displaypal gets faded in and out.
dim shared displaypal(0 to 256) as RGBcolor   'Current display palette; in 8-bit mode includes screen fades
extern "C"
dim shared curmasterpal(0 to 256) as RGBcolor 'Palette at last setpal/fadein, excludes any screen fades
end extern

dim shared updatepal as bool             'setpal called, load new palette at next setvispage
dim shared nearcolor_kdtree as GifKDTree ptr  'Use for fast nearest-color lookups into curmasterpal()

dim shared fps_draw_frames as integer = 0 'Frames drawn since fps_time_start
dim shared fps_real_frames as integer = 0 'Frames sent to gfx backend since fps_time_start
dim shared fps_time_start as double = 0.0
dim shared draw_fps as double             'Current measured frame draw rate, per second
dim shared real_fps as double             'Current measured frame display rate, per second
dim shared overlay_showfps as integer = 0 'Draw on overlay? 0 (off), 1 (real fps), or 2 (draw fps)

dim shared overlays_enabled as bool = YES 'Whether to draw overlays in general
dim shared overlay_message as string      'Message to display on screen
dim shared overlay_hide_time as double    'Time at which to hide it
dim shared overlay_replay_display as bool

MAKETYPE_DoubleList(SpriteCacheEntry)
MAKETYPE_DListItem(SpriteCacheEntry)
type SpriteCacheEntry
	'cachelist used only if object is a member of sprcacheB
	cacheB as DListItem(SpriteCacheEntry)
	hash as integer   'Used as HashTable hash/key
	p as Frame ptr
	cost as integer
	Bcached as bool
end type

CONST SPRITE_CACHE_MULT = 1000000
#define SPRITE_CACHE_KEY(sprtype, record) (sprtype * SPRITE_CACHE_MULT + record)
' Record number used for the dummy SpriteSet holding global animations
const SPRITE_CACHE_GLOBAL_ANIMS = 999999

dim shared sprcache as HashTable
dim shared sprcacheB as DoubleList(SpriteCacheEntry)
dim shared sprcacheB_used as integer    'number of slots full
'dim shared as integer cachehit, cachemiss

dim shared mouse_grab_requested as bool = NO
dim shared mouse_grab_nested_pauses as integer = 0
dim shared mouse_grab_scrolllock_overridden as bool = NO
dim shared remember_mouse_grab as RectPoints

dim shared remember_title as string       'The window title

dim shared global_sfx_volume as single = 1.



'==========================================================================================
'                                Initialisation and shutdown
'==========================================================================================


' Initialise anything in this module that's independent from the gfx backend
local sub modex_init()
	tlsKeyClipRect = tls_alloc_key()
	gfxmutex = mutexcreate

	'Just to ensure nearcolor_kdtree isn't NULL. curmasterpal() is probably empty
	masterpal_changed

	palette16_reload_cache   'read data/defaultgfx/ohrrpgce.pal

	redim vpages(3)
	'redim fixedsize_vpages(3)  'Initially all NO
	vpagesp = @vpages(0)
	for i as integer = 0 to 3
		vpages(i) = frame_new(windowsize.w, windowsize.h, , YES)
	next
	'other vpages slots are for temporary pages
	'They are currently still used in the tileset editor, importmxs,
	'and mapedit_linkdoors.
	'Except for the first two, they're assumed to be the same size as pages 0/1.

	sprcache.construct()
	dlist_construct(sprcacheB.generic, offsetof(SpriteCacheEntry, cacheB))
	sprcacheB_used = 0

	' TODO: tmpdir is shared by all instances of Custom, but when that is fixed this can be removed
	macrofile = tmpdir & "macro" & get_process_id() & ".ohrkeys"
end sub

' This is called from init_preferred_gfx_backend before gfx_init/Initialize. gfxbackend is set.
' Note that it can be called repeatedly with different backends.
sub before_gfx_backend_init()
	'Tell the backend what resolution/scale to initialise at
	if gfx_set_window_size then
		gfx_set_window_size(windowsize, remember_scale)
	else
		' Backends that don't support gfx_set_window_size, which don't
		' support non-320x200 anyway.  Actually just gfx_directx
		' supports changing zoom; gfx_alleg/console don't.
		gfx_setoption("zoom", str(remember_scale))
	end if

	'This for when switching to gfx_directx; sdl/sdl2/fb can read the global
	if debugging_io then gfx_setoption("input-debug", "")
end sub

' Initialise stuff specific to the backend (this is called after a successful gfx_init(),
' but not after an unsuccessful one, so isn't called as often as before_backend_init())
local sub after_gfx_backend_init()
	'Polling thread variables
	pollthread.wantquit = NO
	pollthread.mouselastbuttons = 0
	pollthread.mousebuttons = 0

	if wantpollingthread then
		debuginfo "Starting IO polling thread"
		pollthread.threadptr = threadcreate(@pollingthread)
	end if

	io_init()

	if overrode_native_text_input = NO then
		disable_native_text_input = NO
		#ifdef USE_X11
			if gfxbackend = "sdl" then
				'As a workaround for bug #1064, we disable native text input by default
				'on X11 (Linux/BSD) when using gfx_sdl, avoiding SDL_EnableUNICODE
				disable_native_text_input = YES
			end if
		#endif
	end if
	debuginfo "disable_native_text_input=" & disable_native_text_input

	'gfx_fb has bad numpad support and already remaps the numpad anyway,
	'so our remapping isn't useful. Also it doesn't report numlock state,
	'so remapping would cause text input "4" and scLeft at the same time.
	if gfxbackend = "fb" then remap_numpad = NO

	fps_time_start = TIMER
	fps_draw_frames = 0
	fps_real_frames = 0

	if gfx_supports_variable_resolution() = NO then
		debuginfo "Resolution changing not supported"
		resizing_enabled = NO
		windowsize = XY(320, 200)
		'In case we're called from switch_gfx, resize video pages
		screen_size_update
	else
		if resizing_requested then
			resizing_enabled = gfx_set_resizable(YES, minwinsize.w, minwinsize.h)
		else
			resizing_enabled = gfx_set_resizable(NO, 0, 0)
		end if
	end if
end sub

' Initialise this module and backends, create a window
' (set_resolution can be called before this to set initial resolution. Other functions generally can't!)
sub setmodex()
	modex_init()
	'Select and initialise a graphics/io backend; calls before_gfx_backend_init()
	init_preferred_gfx_backend()
	after_gfx_backend_init()

	modex_initialised = YES
end sub

' Cleans up anything in this module which is independent of the graphics backend
local sub modex_quit()
	stop_recording_input
	stop_recording_video

	for i as integer = 0 to ubound(vpages)
		frame_unload(@vpages(i))
	next
	for i as integer = 0 to ubound(fonts)
		font_unload(@fonts(i))
	next

	sprcache.destruct()
	'debug "cachehit = " & cachehit & " mis == " & cachemiss

	releasestack
	safekill macrofile

	mutexdestroy gfxmutex
	tls_free_key(tlsKeyClipRect)  'Leaking the ClipState, don't care
end sub

' Cleans up everything that ought to be done before calling gfx_close()
local sub before_gfx_backend_quit()
	'clean up io stuff
	if pollthread.threadptr then
		pollthread.wantquit = YES
		threadwait pollthread.threadptr
		pollthread.threadptr = NULL
	end if

	skipped_frame.drop()

	flush_gfx_config_settings
end sub

' Deinitialise this module and backends, destroy the window
sub restoremode()
	if modex_initialised = NO then exit sub
	if main_thread_in_gfx_backend then
		'This can happen when quitting from crash handler. Likely to deadlock
		debug "skipping gfx_close"
		exit sub
	end if
	modex_initialised = NO

	debuginfo "Closing gfx backend & allmodex..."
	before_gfx_backend_quit()
	gfx_close()
	modex_quit()
	debuginfo "...done"
end sub

' Switch to a different gfx backend
sub switch_gfx(backendname as string)
	debuginfo "switch_gfx " & backendname

	before_gfx_backend_quit()
	'This will call before_gfx_backend_init()
	switch_gfx_backend(backendname)
	after_gfx_backend_init()

	' Re-apply settings (this is very incomplete)
	setwindowtitle remember_title
	io_setmousevisibility(cursorvisibility)
end sub

'Force config settings to be reloaded, since they may be game- or backend-specific
sub flush_gfx_config_settings()
	loaded_screenshot_settings = NO
end sub

sub settemporarywindowtitle (title as string)
	'just like setwindowtitle but does not memorize the title
	GFX_ENTER
	gfx_windowtitle(title)
	GFX_EXIT
end sub

sub setwindowtitle (title as string)
	remember_title = title
	GFX_ENTER
	gfx_windowtitle(title)
	GFX_EXIT
end sub

function allmodex_setoption(opt as string, arg as string) as integer
	if opt = "no-native-kbd" then
		disable_native_text_input = YES
		overrode_native_text_input = YES
		debuginfo "Native text input disabled"
		return 1
	elseif opt = "native-kbd" then
		disable_native_text_input = NO
		overrode_native_text_input = YES
		debuginfo "Native text input enabled"
		return 1
	elseif opt = "runfast" then
		debuginfo "Running without speed control"
		use_speed_control = NO
		return 1
	elseif opt = "maxfps" then
		dim fps as integer = str2int(arg, -1)
		if fps > 0 then
			max_display_fps = fps
			return 2
		else
			display_help_string "--maxfps: invalid fps"
			return 1
		end if
	elseif opt = "giffps" then
		dim fps as integer = str2int(arg, -1)
		if fps > 0 then
			gif_max_fps = fps
			return 2
		else
			display_help_string "--giffps: invalid fps"
			return 1
		end if
	elseif opt = "recordoverlays" then
		screenshot_record_overlays = YES
		return 1
	elseif opt = "hideoverlays" then
		overlays_enabled = NO
		return 1
	elseif opt = "recordinput" then
		dim fname as string = absolute_with_orig_path(arg)
		if fileiswriteable(fname) then
			start_recording_input fname
			return 2 'arg used
		else
			display_help_string "input cannot be recorded to """ & fname & """ because the file is not writeable." & LINE_END
			return 1
		end if
	elseif opt = "replayinput" then
		dim fname as string = absolute_with_orig_path(arg)
		if fileisreadable(fname) then
			start_replaying_input fname
			return 2 'arg used
		else
			display_help_string "input cannot be replayed from """ & fname & """ because the file is not readable." & LINE_END
			return 1
		end if
	elseif opt = "nojoy" then
		debuginfo "Joystick/gamepad disabled by -nojoy"
		joysticks_globally_disabled = YES
		return 1 'arg not used
	elseif opt = "nonumpad" then
		debuginfo "Numpad remapping disabled by -nonumpad"
		remap_numpad = NO
		return 1 'arg not used
	elseif opt = "showkeys" then
		gif_show_keys_overlay = YES
		return 1
	elseif opt = "showmouse" then
		show_mouse_overlay = YES
		return 1
	elseif opt = "input-debug" orelse opt = "debug-input" then
		debugging_io = YES
		'sdl/sdl2/fb access the debugging_io global, but gfx_directx does not
		if gfx_setoption then
			gfx_setoption(cstring(opt), "")  'Ignore result
		end if
		return 1
	elseif opt = "logslow" then
		log_slow = YES
		return 1
	end if
end function

property VideoRecorder.active() as bool
	return NO
end property

sub stop_recording_video()
	if recordvid then
		recordvid->stop()
		delete recordvid
	end if
	recordvid = NULL
end sub


'==========================================================================================
'                                        Video pages
'==========================================================================================


' Convert all videopages to 32 bit. Preserves their content
sub switch_to_32bit_vpages ()
	if default_page_bitdepth = 32 then exit sub
	default_page_bitdepth = 32
	for i as integer = 0 to ubound(vpages)
		'Skip duplicated ('holdscreen') pages
		if vpages(i) andalso vpages(i)->fixeddepth = 0 then
			if vpages(i)->isview = NO then
				frame_convert_to_32bit vpages(i), curmasterpal()
				'Any view onto the page will now be invalid (containing an invalid ptr)
			else
				'Hack: assume the page is a compatpage (view of vpage) and i > vpage
				frame_assign @vpages(i), compatpage_internal(vpages(vpage))
			end if
		end if
	next
end sub

' Convert all videopages to 8 bit Frames (not backed by Surfaces).
' WIPES their contents!
sub switch_to_8bit_vpages ()
	if default_page_bitdepth = 8 then exit sub
	default_page_bitdepth = 8
	for i as integer = 0 to ubound(vpages)
		'Skip duplicated ('holdscreen') pages
		if vpages(i) andalso vpages(i)->fixeddepth = 0 then
			if vpages(i)->isview = NO then
				'frame_assign @vpages(i), frame_new(vpages(i)->w, vpages(i)->h)
				'Safer to use this, as it keeps extra state like .noresize
				frame_drop_surface vpages(i)
				clearpage i
			else
				'Hack: assume the page is a compatpage (view of vpage) and i > vpage
				frame_assign @vpages(i), compatpage_internal(vpages(vpage))
			end if
		end if
	next
end sub

'Returns whether pages are now 32-bit color
function toggle_32bit_vpages () as bool
	if vpages_are_32bit then
		switch_to_8bit_vpages
		show_overlay_message "Switched to 8-bit color", 1.2
		return NO
	else
		switch_to_32bit_vpages
		show_overlay_message "Switched to 32-bit color", 1.2
		return YES
	end if
end function

function vpages_are_32bit () as bool
	return default_page_bitdepth = 32
end function

sub freepage (page as integer)
	if page < 0 orelse page > ubound(vpages) orelse vpages(page) = NULL then
		showbug "Tried to free unallocated/invalid page " & page
		exit sub
	end if

	frame_unload(@vpages(page))
end sub

'Adds a Frame ptr to vpages(), returning its index.
function registerpage (spr as Frame ptr) as integer
	if spr->refcount <> NOREFC then	spr->refcount += 1
	for i as integer = 0 to ubound(vpages)
		if vpages(i) = NULL then
			vpages(i) = spr
			' Mark as fixed size, so it won't be resized when the window resizes.
			'fixedsize_vpages(i) = YES
			return i
		end if
	next

	redim preserve vpages(ubound(vpages) + 1)
	vpagesp = @vpages(0)
	vpages(ubound(vpages)) = spr
	'redim preserve fixedsize_vpages(ubound(vpages) + 1)
	'fixedsize_vpages(ubound(vpages)) = YES
	return ubound(vpages)
end function

'Create a new video page and return its index.
'bitdepth: 8 for a regular Frame, 32 for a 32-bit Surface-backed page, or -1 to use the default
'Note: the page is filled with color 0, unlike clearpage, which defaults to uiBackground!
function allocatepage(w as integer = -1, h as integer = -1, bitdepth as integer = -1) as integer
	if w < 0 then w = windowsize.w
	if h < 0 then h = windowsize.h
	if bitdepth < 0 then bitdepth = default_page_bitdepth
	if bitdepth <> 8 and bitdepth <> 32 then
		showbug "allocatepage: Bad bitdepth " & bitdepth
	end if
	dim fr as Frame ptr = frame_new(w, h, , YES, , bitdepth = 32)

	dim ret as integer = registerpage(fr)
	frame_unload(@fr) 'we're not hanging onto it, vpages() is

	return ret
end function

'creates a copy of a page, registering it (must be freed)
'The copy will be unaffected by switch_to_8/32bit_vpages so that it is preserved.
function duplicatepage (page as integer) as integer
	dim fr as Frame ptr = frame_duplicate(vpages(page))
	fr->fixeddepth = 1  'Preserve contents instead of swapping from 8<->32 bit
	dim ret as integer = registerpage(fr)
	frame_unload(@fr) 'we're not hanging onto it, vpages() is
	return ret
end function

'Copy contents of one page onto another
'should copying to a page of different size resize that page?
sub copypage (src as integer, dest as integer)
	'if vpages(src)->w <> vpages(dest)->w or vpages(src)->h <> vpages(dest)->h then
	'	debug "warning, copied to page of unequal size"
	'end if
	frame_draw vpages(src), , 0, 0, NO, vpages(dest)
end sub

sub clearpage (page as integer, colour as integer = -1)
	if colour = -1 then colour = uilook(uiBackground)
	frame_clear vpages(page), colour
end sub

'The contents are either trimmed or extended with colour uilook(uiBackground).
sub resizepage (page as integer, w as integer, h as integer)
	BUG_IF(vpages(page) = NULL, "NULL page")
	frame_assign @vpages(page), frame_resized(vpages(page), w, h, 0, 0, uilook(uiBackground))
end sub

local function compatpage_internal(pageframe as Frame ptr) as Frame ptr
	return frame_new_view(vpages(vpage), (vpages(vpage)->w - 320) / 2, (vpages(vpage)->h - 200) / 2, 320, 200)
end function

'Return a video page which is a view on vpage that is 320x200 (or smaller) and centred.
'In order to use this, draw to the returned page, but call setvispage(vpage).
'Do not swap dpage and vpage!
'WARNING: if a menu using compatpage calls another one that does swap dpage and
'vpage, things will break 50% of the time!
function compatpage() as integer
	dim fakepage as integer
	dim centreview as Frame ptr
	centreview = compatpage_internal(vpages(vpage))
	fakepage = registerpage(centreview)
	frame_unload @centreview
	return fakepage
end function


'==========================================================================================
'                                   Resolution changing
'==========================================================================================


'First check if the window was resized by the user,
'then if windowsize has changed (possibly by a call to unlock_resolution/set_resolution)
'resize all videopages (except compatpages) and the Screen slice to the new window size.
'The videopages are either trimmed or extended with colour 0.
local sub screen_size_update ()
	'Modifies windowsize to requested size if user or possibly something else
	'(e.g. get_set_window_size) tried to resize. Does nothing else.
	if gfx_get_resize(windowsize) then
		'debuginfo "User window resize to " & windowsize.wh
		show_overlay_message windowsize.w & " x " & windowsize.h, 0.7
	end if

	'Clamping windowsize to the minwinsize here means trying to override user
	'resizes (specific to the case where the backend doesn't support giving the WM
	'a min size hint).
	'However unfortunately gfx_sdl can't reliably override it, at least with X11+KDE,
	'because the window size can't be changed while the user is still dragging the window
	'frame.
	'So just accept whatever the backend says the actual window size is.
	'windowsize.w = large(windowsize.w, minwinsize.w)
	'windowsize.h = large(windowsize.h, minwinsize.h)

	dim oldvpages(ubound(vpages)) as Frame ptr
	for page as integer = 0 to ubound(vpages)
		oldvpages(page) = vpages(page)
	next
	'oldvpages pointers will be invalidated

	'Resize dpage and vpage (I think it's better to hardcode 0 & 1 rather
	'than using dpage and vpage variables in case the later are temporarily changed)

	'Update size of all real pages. I think it's better to do so to all pages rather
	'than just page 0 and 1, as other pages are generally used as 'holdpages'.
	'The alternative is to update all menus using holdpages to clear the screen
	'before copying the holdpage over.
	'All pages which are not meant to be the same size as the screen
	'currently don't persist to the next frame.
	for page as integer = 0 to ubound(vpages)
		dim vp as Frame ptr = vpages(page)
		if vp andalso vp->isview = NO andalso vp->noresize = NO then
			if vp->w <> windowsize.w or vp->h <> windowsize.h then
				'debug "screen_size_update: resizing page " & page & " -> " & windowsize.wh
				resizepage page, windowsize.w, windowsize.h
			end if
		end if
	next

	'Scan for compatpages (we're assuming all views are compatpages, which isn't true in
	'general, but currently true when setvispage is called) and replace each with a new view
	'onto the center of the same page if it changed.
	for page as integer = 0 to ubound(vpages)
		if vpages(page) andalso vpages(page)->isview then
			for page2 as integer = 0 to ubound(oldvpages)
				if vpages(page)->base = oldvpages(page2) and vpages(page2) <> oldvpages(page2) then
					'debug "screen_size_update: updating view page " & page & " to new compatpage onto " & page2
					frame_unload @vpages(page)
					vpages(page) = compatpage_internal(vpages(page2))
					exit for
				end if
			next
			'If no match found, do nothing
		end if
	next

	'Update the size of the Screen slice.
	'This removes the need to call UpdateScreenSlice in all menus, but you can
	'still call it to find out if the size changed.
	UpdateScreenSlice NO  'clear_changed_flag=NO
end sub

'Set the size of a video page and keep it from being resized as the window size changes.
'TODO: delete this after the tileset editor and importmxs stop using video pages 2 and 3
sub lock_page_size(page as integer, w as integer, h as integer)
	resizepage page, w, h
	vpages(page)->noresize = 1
end sub

'Revert a video page to following the size of the window
'TODO: delete this after the tileset editor and importmxs stop using video pages 2 and 3
sub unlock_page_size(page as integer)
	resizepage page, windowsize.w, windowsize.h
	vpages(page)->noresize = 0
end sub

'Makes the window resizable, and sets a minimum size.
'Whenever the window is resized all videopages (except compatpages) are resized to match.
'Returns true if was actually successful in making the window resizable, false
'if backend doesn't support it. (Note: won't return false just because we're fullscreen)
function unlock_resolution (min_w as integer, min_h as integer) as bool
	resizing_requested = YES
	minwinsize = XY(min_w, min_h)
	if gfx_supports_variable_resolution() = NO then
		resizing_enabled = NO
		return NO
	end if
	debuginfo "unlock_resolution(" & minwinsize & ")"
	resizing_enabled = gfx_set_resizable(YES, minwinsize.w, minwinsize.h)
	windowsize.w = large(windowsize.w, minwinsize.w)
	windowsize.h = large(windowsize.h, minwinsize.h)
	screen_size_update  'Update page size
	return resizing_enabled
end function

'Disable window resizing.
sub lock_resolution ()
	debuginfo "lock_resolution()"
	resizing_requested = NO
	resizing_enabled = gfx_set_resizable(NO, 0, 0)  'Hard to imagine this could return YES
	minwinsize = XY(0, 0)
end sub

'Returns whether unlock_resolution was called to make the window to be resizable,
'regardless of whether the backend supports it.
function resolution_unlocked () as bool
	return resizing_requested
end function

'Set the window size, if possible, subject to min size bound. Doesn't modify resizability state.
'This will resize all videopages (except compatpages) to the new window size.
sub set_resolution (w as integer, h as integer)
	if gfx_supports_variable_resolution andalso gfx_supports_variable_resolution() = NO then
		exit sub
	end if
	debuginfo "set_resolution " & w & "*" & h
	windowsize.w = large(w, minwinsize.w)
	windowsize.h = large(h, minwinsize.h)
	if modex_initialised = NO then
		'We will tell the backend what resolution to initialise at in before_gfx_backend_init
		exit sub
	end if
	'Ignore any pending resize request
	gfx_get_resize(XY(0,0))
	'Update page size
	screen_size_update
	'Tell the gfx backend about the new page size. If we delayed this then a following
	'call to set_scale_factor would change scale and recenter window using wrong window size,
	'requiring manual recenter.
	'TODO: not ideal, should tell backend about size and scale at same time.
	setvispage vpage, NO
end sub

'The current internal window size in pixels (actual window updated at next setvispage)
function get_resolution() as XYPair
	return windowsize
end function

'Get resolution of the (primary) monitor. On Windows, this excludes size of the taskbar.
sub get_screen_size (byref screenwidth as integer, byref screenheight as integer)
	screenwidth = 0
	screenheight = 0
	if gfxbackend = "sdl2" then
		'Prefer gfx_sdl2 because it has a dedicated API for getting the usable size
		'(excluding taskbar, etc) of the main display. (gfx_directx is also good.)
		gfx_get_screen_size(@screenwidth, @screenheight)
	end if
	if screenwidth <= 0 or screenheight <= 0 then
		'On Windows and Mac os_get_screen_size also returns the usable
		'size of the main display.
		os_get_screen_size(@screenwidth, @screenheight)
	end if
	if screenwidth <= 0 or screenheight <= 0 then
		'gfx_sdl is particularly bad, reports resolution of the whole desktop
		'at init time rather than the current size.
		debuginfo "Falling back to gfx_get_screen_size"
		gfx_get_screen_size(@screenwidth, @screenheight)
	end if
	debuginfo "Desktop resolution: " & screenwidth & "*" & screenheight
end sub

'Set the size that a pixel appears on the screen (i.e. the zoom), while windowed.
'Usually no effect if fullscreened, until switching back to windowed.
'Supported by all backends except gfx_alleg.
'If change_windowsize = YES, this changes the window size and keeps the same resolution,
'otherwise it changes the resolution and keeps the same window size
sub set_scale_factor (scale as integer, change_windowsize as bool = YES)
	'gfx_sdl and gfx_fb, which use blit.c scaling, are limited to 1x-16x
	scale = bound(scale, 1, 16)
	debuginfo "Setting graphics scaling to x" & scale & " change_windowsize=" & change_windowsize

	'If we don't have a gfx backend yet need to store it in an otherwise redundant global
	remember_scale = scale
	if modex_initialised = NO then
		'We effectively force change_windowsize = YES
		exit sub
	end if

	dim changed_zoom as bool = NO
	if change_windowsize = NO then
		dim winstate as WindowState ptr = gfx_getwindowstate()
		if winstate->structsize >= 8 THEN  'winstate->windowsize valid
			debuginfo " ...current window size " & winstate->windowsize
			dim newresolution as XYPair = winstate->windowsize \ scale
			dim toosmall as bool = (newresolution < XY(320, 200))
			newresolution = large(newresolution, XY(320, 200))
			if gfx_set_window_size then
				'Set both resolution and scale at the same time if the gfx backend supports it
				gfx_set_window_size(newresolution, scale)
				changed_zoom = YES
			elseif toosmall then
				debuginfo " ...too small, increasing res"
				'Don't allow a tiny resolution: change both resolution and window size
				set_resolution newresolution.w, newresolution.h
				change_windowsize = YES
			end if
		end if
	end if

	if change_windowsize then
		if gfx_set_window_size then
			gfx_set_window_size( , scale)
			changed_zoom = YES
		end if
	end if

	if changed_zoom = NO then
		' Backends that don't support gfx_set_window_size... actually just gfx_directx.
		' gfx_alleg/console don't allow changing zoom either
		gfx_setoption("zoom", str(scale))
	end if

	'The resolution might have changed size (probably only if change_windowsize=NO)
	'so update to avoid a one-tick flicker.
	screen_size_update
end sub

'Returns true if successfully queries the fullscreen state, in which case 'fullscreen' is set.
'(Note: gfx_fb doesn't know for certain whether it's fullscreen; can't catch alt+enter.
function try_check_fullscreen(byref fullscreen as bool) as bool
	dim winstate as WindowState ptr = gfx_getwindowstate()
	if winstate andalso winstate->structsize >= 4 then
		fullscreen = winstate->fullscreen
		return YES
	end if
	return NO
end function

function supports_fullscreen_well () as bool
	'Return YES if we should show the fullscreen/windowed menu options
	'and obey a game's fullscreen/windowed setting.
	'Note: even if this returns false, you can still try to fullscreen using alt-tab
	'or the --fullscreen arg and it might be supported.
	if running_on_desktop() = NO then
		return NO
	end if
#IFDEF __GNU_LINUX__
	' At least for me with KDE 4 and xfce4, fbgfx gives horrible results,
	' turning off my 2nd monitor and lots of garbage and desktop resolution changing,
	' and often gets stuck with a fullscreen black screen, or ignoring all input
	' and becoming unquitable from inside X11.
	' SDL 1.2 does something milder (causing the 2nd monitor to switch to mirrored)
	' but only when the window size is smaller than the desktop.
	' So probably the solution in gfx_sdl is to set the requested resolution to
	' be equal to the desktop resolution and add black bars.
	if gfxbackend = "fb" then
		return NO
	end if
#ENDIF
	return YES
end function


'==========================================================================================
'                                   setvispage and Fading
'==========================================================================================

declare sub present_internal_frame(drawpage as integer)
declare sub present_internal_surface(drawpage as integer)

sub SkippedFrame.drop()
	'if page >= 0 then freepage page
	page = -1
end sub

' If the last setvispage was skipped, display it
sub SkippedFrame.show ()
	' Note: setvispage will call SkippedFrame.drop() after displaying the page
	if page > -1 then
		setvispage page, NO
	end if
end sub

'Decide whether to skip a frame, in order to meet max_display_fps limit
function should_skip_frame() as bool
	static lastframe as double
	'Make sure we still draw under --runfast
	if timer > lastframe + 1. / max_display_fps then
		lastframe = timer
		return NO
	end if

	'How many times will setvispage be called per non-skipped frame?
	dim frames_per_gfx_present as double = requested_framerate / max_display_fps
	if frames_per_gfx_present <= 1 then return NO  'No need to skip

	'Per frames_per_gfx_present frames, draw one frame and skip the rest
	frame_index and= INT_MAX  'Avoid overflow to negative
	if fmod(frame_index, frames_per_gfx_present) > 1 then
		return YES
	end if
	lastframe = timer

	'Maybe we should have an option to also skip frames if we're running at
	'100% cpu, although that will only save a little time because we still
	'draw each frame.
end function

' The last/currently displayed  videopage (or a substitute: guaranteed to be valid)
function getvispage() as integer
	if last_setvispage >= 0 andalso last_setvispage <= ubound(vpages) _
	   andalso vpages(last_setvispage) then
		return last_setvispage
	end if
	return vpage
end function

'Display a videopage. May modify the page!
'Also resizes all videopages to match the window size
'skippable: if true, allowed to frameskip this frame at high framerates
sub setvispage (page as integer, skippable as bool = YES)
	' Remember last page
	last_setvispage = page

	' Drop frames to reduce CPU usage if FPS too high
	frame_index += 1
	if skippable andalso should_skip_frame() then
		skipped_frame.drop()
		skipped_frame.page = page
		' To be really cautious we could save a copy, but because page should
		' not get modified until it's time to draw the next frame, this isn't really needed.
		'skipped_frame.page = duplicatepage(page)
		update_fps_counter YES
		exit sub
	end if
	update_fps_counter NO

	dim starttime as double = timer
	if gfx_supports_variable_resolution() = NO then
		'Safety check. We must stick to 320x200, otherwise the backend could crash.
		'In future backends should be updated to accept other sizes even if they only support 320x200
		'(Actually gfx_directx appears to accept other sizes, but I can't test)
		if vpages(page)->w <> 320 or vpages(page)->h <> 200 then
			resizepage page, 320, 200
			showbug "setvispage: page was not 320x200 even though gfx backend forbade it"
		end if
	end if

	' The page to which to draw overlays, and display.
	dim drawpage as integer
	if vpages_are_32bit() andalso faded_in = NO then
		' In 32-bit mode, we always draw to the vpage using curmasterpal (not faded out),
		' so that we have something to fade back in if a fade happens.
		' This means we need to hide the fact that the vpage palette isn't actually faded out.
		drawpage = allocatepage(vpages(page)->w, vpages(page)->h)
		gfx_surfaceFill(faded_to_color.col, NULL, vpages(drawpage)->surf)
	else
		' We could skip this duplication if there are no overlays to draw. But even at 60fps
		' it's not significant: in my test, at 1920x1080 and 2x zoom, this duplicatepage
		' is only 0.5% of runtime.
		drawpage = duplicatepage(page)
	end if

	'Draw those overlays that are always recorded in .gifs/screenshots
	draw_allmodex_recordable_overlays drawpage

	if screenshot_record_overlays = YES then
		draw_allmodex_overlays drawpage
	end if

	'F12 for screenshots handled here (uses real_keyval)
	snapshot_check drawpage
	if recordvid then
		recordvid->record_frame vpages(drawpage), displaypal()
	end if

	if screenshot_record_overlays = NO then
		draw_allmodex_overlays drawpage
	end if

	starttime -= timer  'Stop timer
	dim starttime2 as double = timer

	'fb_gfx may deadlock if it collides with the polling thread because of
	'FB bug https://sourceforge.net/p/fbc/bugs/885/
	GFX_ENTER

	if vpages(page)->surf then
		present_internal_surface drawpage
	else
		present_internal_frame drawpage
	end if

	GFX_EXIT

	#ifdef SIMULATE_BLOCKING_VSYNC
		'For testing: simulate gfx_present blocking until vsync.
		dim vsynctime as double, nowtime as double = timer
		vsynctime = 1e3 * ((1. / 60) - fmod(nowtime, (1. / 60)))
		sleep vsynctime
		'?"sleep " & vsynctime & "... slept " & 1e3 * (timer - nowtime)
	#endif

	' This gets triggered a lot under Win XP because the program freezes while moving
	' the window (in all backends, although in gfx_fb it freezes readmouse instead)
	if log_slow then debug_if_slow(starttime2, 0.008, "gfx_present")
	starttime += timer  'Restart timer

	freepage drawpage

	skipped_frame.drop()  'Delay dropping old frame; skipped_frame.show() might have called us

	'After presenting the page this is a good time to check for window size changes and
	'resize the videopages as needed before the next frame is rendered.
	screen_size_update
	if log_slow then debug_if_slow(starttime, 0.005, "")
end sub

'setvispage internal function for presenting a regular Frame page on the screen
local sub present_internal_frame(drawpage as integer)
	dim surf as Surface ptr
	if gfx_surfaceCreateFrameView(vpages(drawpage), @surf) then return

	dim surface_pal as RGBPalette ptr
	if surf->format = SF_8bit then
		' Need to provide a palette
		surface_pal = masterpal_to_gfxpal(displaypal())
		'gfx_paletteFromRGB(@displaypal(0), @surface_pal)
	end if

	gfx_present(surf, surface_pal)
	updatepal = NO  'We just did

	gfx_paletteDestroy(@surface_pal)
	gfx_surfaceDestroy(@surf)
end sub

'setvispage internal function for presenting a Surface-backed page on the screen
local sub present_internal_surface(drawpage as integer)
	dim drawsurf as Surface ptr = vpages(drawpage)->surf

	dim surface_pal as RGBPalette ptr
	if drawsurf->format = SF_8bit then
		' Need to provide a palette
		surface_pal = masterpal_to_gfxpal(displaypal())
		'gfx_paletteFromRGB(@displaypal(0), @surface_pal)
	end if

	gfx_present(drawsurf, surface_pal)
	updatepal = NO  'We just did

	gfx_paletteDestroy(@surface_pal)
end sub

local sub masterpal_changed()
	delete_KDTree(nearcolor_kdtree)
	'Build tree which excludes color 0 (callers to nearcolor_fast rely on this)
	nearcolor_kdtree = make_KDTree_for_palette(@curmasterpal(0), 8, 1)
	memset(@nearcolor_cache(0), 0, ubound(nearcolor_cache) + 1)
end sub

'For checking whether master() has changed since last setpal/setdrawpal/fadein
function masterpal_has_changed(pal() as RGBcolor) as bool
	return memcmp(@pal(0), @curmasterpal(0), 256 * SIZEOF(RGBcolor)) <> 0
end function

'Change the palette used for drawing, but don't end a fade out yet.
'This should be called before drawing the screen and calling fadein/fadetopal,
'unless you're fading back in the same palette that you faded out from.
'(It's also mostly necessary only in 32-bit mode)
sub setdrawpal(pal() as RGBcolor)
	memcpy(@curmasterpal(0), @pal(0), 256 * SIZEOF(RGBcolor))
	masterpal_changed
end sub

'Switch to a palette immediately without a fade in. Ends a fade out.
'(Unlike how it used to work, only takes effect at the next setvispage call or at start of fadein)
sub setpal(pal() as RGBcolor)
	setdrawpal pal()
	memcpy(@displaypal(0), @pal(0), 256 * SIZEOF(RGBcolor))
	updatepal = YES
	faded_in = YES
end sub

'Immediately change to a faded out state.
'The difference from calling setpal is:
'-preserves curmasterpal, for nearcolor & transparency effects
'-Sets faded_in = NO, not YES
'The difference from calling fadetocolor with fadems=0 is just that it doesn't
'immediately redraw the screen.
sub setpal_to_color(col as RGBcolor = TYPE(0))
	col.a = 255
	faded_to_color = col
	if vpages_are_32bit() = NO then
		for i as integer = 0 to 255
			displaypal(i) = col
		next
	end if
	'Do not update curmasterpal
	updatepal = YES
	faded_in = NO
end sub

' A gfx_setpal wrapper which may perform frameskipping to limit fps
local sub maybe_do_gfx_setpal()
	frame_index += 1
	if should_skip_frame() then
		update_fps_counter YES
		updatepal = YES
		exit sub
	end if
	update_fps_counter NO
	updatepal = NO

	GFX_ENTER
	gfx_setpal(@displaypal(0))
	GFX_EXIT
end sub

'A fade in or out.
'This modifies displaypal() only in 8-bit color mode
'pal() is used only in 8-bit mode. fadecol is used only in 32-bit mode
'so don't support fading between two master palettes!
local sub fadetopal_internal(pal() as RGBcolor, col as RGBcolor, fadems as integer, fading_in as bool)
	dim vispage as integer = getvispage()
	dim is32bit as bool = vpages_are_32bit()
	dim was_faded_in as bool = faded_in
	dim prev_fade_color as RGBcolor = faded_to_color

	skipped_frame.show()  'If we frame-skipped last frame, better show it

	if updatepal then
		if is32bit = NO then maybe_do_gfx_setpal
		if recordvid then
			recordvid->record_frame vpages(vispage), displaypal()
		end if
	end if

	dim holdscreen as integer
	dim startpal(255) as RGBcolor

	if is32bit then
		holdscreen = duplicatepage(vispage)
	else
		'This will be equal to curmasterpal unless we're faded out
		memcpy(@startpal(0), @displaypal(0), 256 * SIZEOF(RGBcolor))
	end if

	dim ticks as integer = large(1, fadems / 16.67)
	for tick as integer = 1 to ticks
		setwait 16.67
		'Use a symmetric cubic smoothing function. The slope is at a
		'minimum at x=0 and x=1, where it's 1/2 of the linear
		'interpolation slope, and it's at a maximum at x=1/2.
		dim x as double = tick / ticks
		dim fraction as double = x / 2 + 3 * x*x / 2 - x*x*x

		if is32bit then
			'Draw a transparent rect over vispage
			if fading_in then fraction = 1 - fraction
			if was_faded_in andalso fading_in then fraction = 0  'noop
			if was_faded_in = NO andalso fading_in = NO then
				'Fading between two colors
				gfx_surfaceFill(prev_fade_color.col, NULL, vpages(vispage)->surf)
			else
				copypage holdscreen, vispage
			end if
			trans_rectangle vpages(vispage), XYWH(0,0,rWidth,rHeight), col, fraction
			faded_in = YES  'Needed to stop setvispage from doing its own fade handling
			setvispage vispage
		else
			'Don't modify vispage, instead modify displaypal and redisplay with that
			for j as integer = 0 to 255
				displaypal(j).r = pal(j).r * fraction + startpal(j).r * (1 - fraction)
				displaypal(j).g = pal(j).g * fraction + startpal(j).g * (1 - fraction)
				displaypal(j).b = pal(j).b * fraction + startpal(j).b * (1 - fraction)
			next
			maybe_do_gfx_setpal
		end if

		if tick mod 3 = 0 then
			' We're assuming that the page hasn't been modified since the last setvispage
			if recordvid then
				recordvid->record_frame vpages(vispage), displaypal()
			end if
		end if

		dowait
	next

	if is32bit then
		copypage holdscreen, vispage
		freepage holdscreen
	end if

	'This function was probably called in the middle of timed loop, call
	'setwait to avoid "dowait called without setwait" warnings
	setwait 0
end sub

sub fadetocolor(col as RGBcolor, fadems as integer = 500)
	col.a = 255
	dim pal(255) as RGBcolor
	for j as integer = 0 to 255
		pal(j) = col
	next
	fadetopal_internal pal(), col, fadems, NO

	faded_in = NO
	faded_to_color = col

	'In 8-bit mode, displaypal() is now equal to pal()/col. In 32-bit mode, not modified
	'Do not update curmasterpal, it contains non-faded palette
end sub

'NOTE: In 32-bit mode we don't support fading between two master palettes, only out and back in to the original palette!
sub fadetopal(pal() as RGBcolor, fadems as integer = 500)
	fadetopal_internal pal(), faded_to_color, fadems, YES

	faded_in = YES

	memcpy(@displaypal(0), @pal(0), 256 * SIZEOF(RGBcolor))
	'If fadetopal/fadein is used, it means setpal wasn't called, so we need to replicate
	'the other thing setpal does: update curmasterpal
	memcpy(@curmasterpal(0), @pal(0), 256 * SIZEOF(RGBcolor))
	masterpal_changed
end sub

'Blend between two video pages... unless the screen is faded out, in which case blends in to newpage, ignoring oldpage
sub fadetopage(oldpage as integer, newpage as integer, fadems as integer = 500)
	dim was_faded_in as bool = faded_in
	dim prev_fade_color as RGBcolor = faded_to_color

	skipped_frame.show()  'If we frame-skipped last frame, better show it

	if updatepal then
		if vpages_are_32bit() = NO then maybe_do_gfx_setpal
		if recordvid then
			recordvid->record_frame vpages(getvispage()), displaypal()
		end if
	end if

	dim drawpage as integer = allocatepage(vpages(oldpage)->w, vpages(oldpage)->h)
	dim drawopts as DrawOptions
	drawopts.with_blending = YES

	dim ticks as integer = large(1, fadems / 16.67)
	for tick as integer = 1 to ticks
		setwait 16.67
		'Use a symmetric cubic smoothing function. The slope is at a
		'minimum at x=0 and x=1, where it's 1/2 of the linear
		'interpolation slope, and it's at a maximum at x=1/2.
		dim x as double = tick / ticks
		drawopts.opacity = x / 2 + 3 * x*x / 2 - x*x*x

		if was_faded_in then
			copypage oldpage, drawpage
		else
			if vpages_are_32bit() then
				gfx_surfaceFill(faded_to_color.col, NULL, vpages(drawpage)->surf)
			else
				clearpage drawpage, nearcolor_master(faded_to_color)
			end if
		end if
		frame_draw vpages(newpage), , 0, 0, NO, drawpage, drawopts
		faded_in = YES
		setvispage drawpage

		if tick mod 3 = 0 then
			' We're assuming that the page hasn't been modified since the last setvispage
			if recordvid then
				recordvid->record_frame vpages(drawpage), displaypal()
			end if
		end if

		dowait
	next

	copypage newpage, vpage
	freepage drawpage

	'This function was probably called in the middle of timed loop, call
	'setwait to avoid "dowait called without setwait" warnings
	setwait 0
end sub


'==========================================================================================
'                                     Waits/Framerate
'==========================================================================================


'Set number of milliseconds from now when the next call to dowait returns.
'This number is treated as a desired framewait, so actual target wait varies from 0.5-1.5x requested.
'ms:     number of milliseconds
'flagms: if nonzero, is a count in milliseconds for the secondary timer, whether this has triggered
'        is accessed as the return value from dowait.
sub setwait (ms as double, flagms as double = 0)
	if use_speed_control = NO then ms = 0.001
	ms /= fps_multiplier
	'flagms /= fps_multiplier
	requested_framerate = 1000. / ms
	dim thetime as double = timer
	dim target as double
	target = bound(waittime + ms / 1000, thetime + 0.5 * ms / 1000, thetime + 1.5 * ms / 1000)
	/'
	if thetime > waittime + 0.001 then
		debuginfo strprintf("Missed setwait by %.1fms. Waiting %.1fms", _
				    1e3 * (thetime - waittime), 1e3 * (target - thetime))
	end if
	'/
	waittime = target
	if flagms <= 0 then
		flagms = ms
	end if
	if thetime > flagtime then
		flagtime = bound(flagtime + flagms / 1000, thetime + 0.0165, thetime + 1.5 * flagms / 1000)
	end if
	setwait_called = YES
end sub

'Returns seconds left until the deadline set by the last setwait. Will
'be negative if it's already been missed.
function setwait_time_remaining() as double
	return waittime - timer
end function

' Returns number of dowait calls
function get_tickcount() as integer
	return tickcount
end function

function dowait () as bool
'wait until alarm time set in setwait()
'returns true if the flag time has passed (since the last time it was passed)
'In freebasic, sleep is in 1000ths, and a value of less than 100 will not
'be exited by a keypress, so sleep for 5ms until timer > waittime.
	tickcount += 1
	global_tog XOR= 1
	dim starttime as double = timer
	do while timer <= waittime - 0.0005
		io_waitprocessing()
		Steam.run_frame
		sleep bound((waittime - timer) * 1000, 1, 5)
	loop
	' dowait might be called after waittime has already passed, ignore that
        ' (the time printed is the unwanted delay).
	' On Windows FB sleep calls winapi Sleep(), which has a default of 15.6ms, adjustable
	' with timeBeginPeriod(). 15.6ms is very coarse for 60fps games, so we probably
	' should request a higher frequency. (Also, Win XP rounds the sleep period up to the
	' following tick, while Win 7+ rounds it down, although that probably makes no
	' difference due to the avoid while loop. See
	' https://randomascii.wordpress.com/2013/04/02/sleep-variation-investigated/
	' If there's a long delay here it's because the system is busy; not interesting.
	if log_slow then debug_if_slow(large(starttime, waittime), 0.1, "")
	if setwait_called then
		setwait_called = NO
	else
		'debuginfo "dowait called without setwait"
	end if
	return timer >= flagtime
end function


'==========================================================================================
'                                           Music
'==========================================================================================


sub setupmusic
	music_init
	sound_init
	musicbackendinfo = music_get_info
	debuginfo musicbackendinfo
end sub

sub closemusic ()
	debuginfo "Closing music backend..."
	music_close
	sound_close
	debuginfo "...done"
end sub

sub loadsong (songname as string)
	music_play(songname, getmusictype(songname))
end sub

'Doesn't work in SDL_mixer for MIDI music, so avoid
'sub pausesong ()
'	music_pause()
'end sub
'
'sub resumesong ()
'	music_resume
'end sub

function get_music_volume () as single
	return music_getvolume
end function

sub set_music_volume (vol as single)
	music_setvolume(vol)
end sub


'==========================================================================================
'                                      Sound effects
'==========================================================================================


' loopcount N to play N+1 times, -1 to loop forever
' See set_sfx_volume for description of volume_mult.
sub playsfx (num as integer, loopcount as integer = 0, volume_mult as single = 1.)
	dim slot as integer
	' If already loaded can reuse without reloading.
	' TODO: However this preempts it if still playing; shouldn't force that
	' NOTE: backends vary, music_sdl does nothing if too many sfx playing,
	' music_audiere has no limit.
	slot = sound_slot_with_id(num)
	if slot = -1 then
		slot = sound_load(find_sfx_lump(num), num)
		if slot = -1 then exit sub
	end if
	'debug "playsfx volume_mult=" & volume_mult & " global_sfx_volume " & global_sfx_volume
	sound_play(slot, loopcount, volume_mult * global_sfx_volume)
	IF_PTR(sound_slotdata(slot))->original_volume = volume_mult
end sub

sub resetsfx ()
	' Stops playback and unloads cached sound effects
	sound_reset
end sub

sub stopsfx (num as integer)
	dim slot as integer
	slot = sound_slot_with_id(num)
	if slot = -1 then exit sub
	sound_stop(slot)
end sub

sub pausesfx (num as integer)
	dim slot as integer
	slot = sound_slot_with_id(num)
	if slot = -1 then exit sub
	sound_pause(slot)
end sub

' This returns the actual effective sfx volume 0. - 1., combining all volume
' settings and any fade effects the backend might be doing (nothing like
' that is implemented yet).
function effective_sfx_volume (num as integer) as single
	dim slot as integer
	slot = sound_slot_with_id(num)
	if slot = -1 then return 0.
	return sound_getvolume(slot)
end function

/'  Is this needed?
function get_sfx_volume (num as integer) as single
	dim slot as integer
	slot = sound_slot_with_id(num)
	if slot = -1 then return 0.
	return sound_getslot(slot)->original_volume
end function
'/

' Set the volume of a sfx to some multiple of its default volume,
' which is the global sfx volume * the volume adjustment defined in Custom
sub set_sfx_volume (num as integer, volume_mult as single)
	dim slot as integer
	slot = sound_slot_with_id(num)
	if slot = -1 then exit sub
	sound_setvolume(slot, volume_mult * global_sfx_volume)
	IF_PTR(sound_slotdata(slot))->original_volume = volume_mult
end sub

' Set the global volume multiplier for sound effects.
' The backends only support a max volume of 1.0,
' but the global volume can be set higher, amplifying
' any sfx with a volume less than 1.0.
sub set_global_sfx_volume (volume as single)
	global_sfx_volume = volume
	' Update all SFX
	for slot as integer = 0 to sound_lastslot()
		dim slotdata as SFXCommonData ptr
		slotdata = sound_slotdata(slot)
		if slotdata = 0 then continue for
		'debug "set_global_sfx_volume: refresh volume for " _
		'      & slotdata->effectID & " to " & (slotdata->original_volume * global_sfx_volume)
		sound_setvolume slot, slotdata->original_volume * global_sfx_volume
	next
end sub

function get_global_sfx_volume () as single
	return global_sfx_volume
end function

' Only used by Custom's importing interface
sub freesfx (num as integer)
	sound_free(num)
end sub

function sfxisplaying(num as integer) as bool
	dim slot as integer
	slot = sound_slot_with_id(num)
	if slot = -1 then return NO
	return sound_playing(slot)
end function


'==========================================================================================
'                                      Keyboard input
'==========================================================================================

'======================================= keyval API =======================================

'Read keyboard/joystick input for either a single player or player 0 (default/all).
'Reads replayed state, if any, unless real_keys = YES
'key:    any scancode, including cc* constants and joystick buttons
'player: 0 for any input device, 1 is first joystick + keyboard, 2-4 are other joysticks
'check_keyboard: pass false to ignore keyboard input
'
'except for possibly certain special keys (like capslock), each key reports 3 bits:
'
'bit 0: key was down at the last setkeys call
'bit 1: keypress event (either new keypress, or key-repeat) during last setkey-setkey interval
'bit 2: new keypress during last setkey-setkey interval
'
'Note: Alt/Ctrl keys may behave strangely with gfx_fb:
'You won't see Left/Right keypresses even when scAlt/scCtrl is pressed, so do not
'check "keyval(scLeftAlt) > 0 or keyval(scRightAlt) > 0" instead of "keyval(scAlt) > 0"
function player_keyval(key as KBScancode, player as integer = 0, repeat_wait as integer = 0, repeat_rate as integer = 0, check_keyboard as bool = YES, real_keys as bool = NO) as KeyBits
	BUG_IF(player < 0, "Invalid player " & player, 0)
	BUG_IF(key < scKEYVAL_FIRST orelse key > scKEYVAL_LAST, "bad scancode " & key, 0)

	dim ret as KeyBits

	if player = 0 then
		'Merge all inputs
		for player = 1 to num_joysticks()
			ret or= player_keyval(key, player, repeat_wait, repeat_rate, check_keyboard, real_keys)
		next
		return ret
	end if

	dim inputst as InputState ptr
	if replay.active andalso real_keys = NO then
		inputst = @replay_input
	else
		inputst = @real_input
	end if

	dim joynum as integer = player - 1
	if joynum > ubound(inputst->joys) then return 0

	if key < 0 then  'Control key
		ret = inputst->joys(joynum).controlkey(key, *inputst, repeat_wait, repeat_rate)
		if player = 1 andalso check_keyboard then
			'In future, ignore any keys mapped to other players?
			ret or= inputst->kb.controlkey(key, *inputst, repeat_wait, repeat_rate)
		end if
	elseif key <= scLAST then  'Keyboard key
		if player = 1 andalso check_keyboard then
			ret = inputst->kb.keyval(key, repeat_wait, repeat_rate, *inputst)
		end if
	else  'Joystick button
		dim button as integer = keybd_to_joy_scancode(key)  '0 if invalid
		ret = inputst->joys(joynum).keyval(button, repeat_wait, repeat_rate, *inputst)
	end if
	return ret
end function

'This keyval() variant always returns the real input, rather than replayed input
function real_keyval (key as KBScancode) as KeyBits
	return player_keyval(key, 0, , , , YES)
end function

function keyval (key as KBScancode) as KeyBits
	'Using a wrapper rather than an alias for player_keyval has the advantage of
	'decreasing executable size, since this function is called in zillions of places!
	return player_keyval(key, 0)
end function

'Simple keyval() variant with modified key repeat rate (in milliseconds), and no repeat delay
function slowkey (key as KBScancode, ms as integer) as bool
	return player_keyval(key, 0, ms, ms) > 1
end function

sub setkeyrepeat (repeat_wait as integer = 500, repeat_rate as integer = 55)
	' Not actually used anywhere yet, but give the replay and real states
	' separate repeat rates to avoid desync issues
	dim inputst as InputState ptr = iif(replay.active, @replay_input, @real_input)
	inputst->repeat_wait = repeat_wait
	inputst->repeat_rate = repeat_rate
end sub

'Translate a sc* constant to a joy* constant
function keybd_to_joy_scancode(key as KBScancode) as JoyButton
	ERROR_IF(key < scJoyButton1 orelse key > scJoyLAST, "Bad scancode " & key, 0)
	return key - scJoyOFFSET
end function


'=============================== keyval implementation ====================================

'Return numpad scancode that's an alias to 'key'
local function KeyboardState.numpad_alias_key(key as KBScancode) as KBScancode
	if remap_numpad = NO then return 0
	if (this.keys(scNumLock) and 1) xor (this.keys(scShift) and 1) then
		return 0
	end if
	select case key
		case scLeft:           return scNumpad4
		case scRight:          return scNumpad6
		case scUp:             return scNumpad8
		case scDown:           return scNumpad2
		case scHome:           return scNumpad7
		case scEnd:            return scNumpad1
		case scPageUp:         return scNumpad9
		case scPageDown:       return scNumpad3
		case scDelete:         return scNumpadPeriod
		case scInsert:         return scNumpad0
		'Skip - + (already handled in intgrabber) * / Enter (already handled by AnyEnter)
		'(no good reason to do so).
	end select
	return 0
end function

function KeyboardState.anykey(inputst as InputState) as KeyBits
	dim ret as KeyBits
	for key as KBScancode = 0 to scLAST
		select case key
			case scNumLock, scCapsLock, scScrollLock
			case else
				ret or= this.keyval(key, , , inputst)
		end select
	next
	return ret
end function

'This doesn't check all joystick buttons, only ones mapped in controls()
'semi-intentionally, so you can ignore stuck keys or uncentered sticks.
function JoystickState.anykey(inputst as InputState) as KeyBits
	dim ret as KeyBits
	for idx as integer = 0 to ubound(this.controls)
		ret or= this.keyval(this.controls(idx).scancode, , , inputst)
	next
	'for button as JoyButton = joyButton1 to joyLAST
	'	ret or= this.keyval(button, repeat_wait, repeat_rate, inputst)
	'next
	return ret
end function

'Calculate value of a control key for one device, bitwise-ORing all keys mapped to it.
'cc should be ccFIRST <= cc < 0
function KeyArray.controlkey (cc as KBScancode, inputst as InputState, repeat_wait as integer = 0, repeat_rate as integer = 0) as KeyBits
	if cc = ccAny then
		'Note: repeat_wait and repeat_rate are ignored
		return this.anykey(inputst)
	end if

	dim ret as KeyBits
	for idx as integer = 0 to ubound(this.controls)
		with this.controls(idx)
			if cc = .ckey then
				ret or= this.keyval(.scancode, repeat_wait, repeat_rate, inputst)
			end if
		end with
	next
	return ret
end function

'Get state of a real keyboard key: cc* and joy* scancodes not supported
function KeyboardState.keyval(key as KBScancode, repeat_wait as integer = 0, repeat_rate as integer = 0, inputst as InputState) as KeyBits
	dim check_repeat as bool = YES

	'if key = scAlt then
		'alt can repeat (probably a bad idea not to), but only if nothing else has been pressed
		'for i as KBScancode = 1 to scLAST
		'	if this.keys(i) > 1 then check_repeat = NO
		'next
		'if delayed_alt_keydown = NO then check_repeat = NO
	'end if

	'Don't fire repeat presses for special toggle keys (note: these aren't actually
	'toggle keys in all backends, eg. gfx_fb)
	if key = scNumlock orelse key = scCapslock orelse key = scScrolllock then check_repeat = NO

	if check_repeat then
		dim ret as KeyBits
		ret = this.key_repeating(key, repeat_wait, repeat_rate, inputst)
		'Wait, there's more! When numlock is off, numpad keys double as other keys; is 'key' one?
		dim key2 as KBScancode = this.numpad_alias_key(key)
		if key2 then
			ret or= this.key_repeating(key2, repeat_wait, repeat_rate, inputst)
		end if
		return ret
	else
		return this.keys(key)
		'Num/caps/scrolllock don't have any alias keys on the numpad
	end if
end function

function JoystickState.keyval (key as JoyButton, repeat_wait as integer = 0, repeat_rate as integer = 0, inputst as InputState) as KeyBits
	return this.key_repeating(key, repeat_wait, repeat_rate, inputst)
end function

'Return state of a key plus key repeat bit. (Should only be called from keyval)
'repeat_wait and repeat_rate can override inputst settings.
function KeyArray.key_repeating(key as integer, repeat_wait as integer, repeat_rate as integer, inputst as InputState) as KeyBits
	dim result as KeyBits = keys(key)

	if result and 1 then
		'Check key repeat

		if repeat_wait = 0 then repeat_wait = inputst.repeat_wait
		if repeat_rate = 0 then repeat_rate = inputst.repeat_rate

		dim down_ms as integer
		'Ensure arrow keys/buttons repeat on the same tick
		down_ms = iif(is_arrow_key(key), arrow_key_down_ms, key_down_ms(key))

		if down_ms >= repeat_wait then
			'Keypress event at "wait + i * rate" ms after keydown
			dim temp as integer = down_ms - repeat_wait
			if temp \ repeat_rate > (temp - inputst.elapsed_ms) \ repeat_rate then
				result or= 2
			end if
		end if
	end if
	return result
end function


'======================================= clearkey =========================================

'Erase a keypress event from the keyboard state, and optionally cancel key repeat. Does not affect key-down state.
'FIXME: have to clear numpad keys too, if remapped!
'NOTE: clearing scShift/Alt/Ctrl doesn't clear scLeft/RightShift/Alt/Ctrl. That's probably fine.
sub clearkey(k as KBScancode, clear_key_repeat as bool = YES)
	dim inputst as InputState ptr = iif(replay.active, @replay_input, @real_input)
	inputst->kb.keys(k) and= 1
	if clear_key_repeat then
		inputst->kb.key_down_ms(k) = 0
	end if
end sub

'Erase a new keypress bit and optionally cancel key repeat from the real keyboard state,
'even if replaying recorded input.
sub real_clearkey(k as KBScancode, clear_key_repeat as bool = YES)
	real_input.kb.keys(k) and= 1
	if clear_key_repeat then
		real_input.kb.key_down_ms(k) = 0
	end if
end sub

'Erase all new keypress bits and cancel key repeat. Does not affect key-down state.
sub KeyArray.clearkeys()
	for scancode as integer = 0 to ubound(keys)
		keys(scancode) and= 1
	next
	flusharray key_down_ms()
end sub

'Clear keypress events for all keyboard keys and joystick buttons, including cancelling
'key repeat, and clear mouse clicks. Doesn't change the 'down' state of keys/buttons.
'Note: an alternative is to call setkeys, which will also wipe & update the "new keypress" bits
sub clearkeys()
	dim inputst as InputState ptr = iif(replay.active, @replay_input, @real_input)
	inputst->kb.clearkeys()
	for joynum as integer = 0 to ubound(inputst->joys)
		inputst->joys(joynum).clearkeys()
	next
	mouse_state.clearclick(mouseLeft)
	mouse_state.clearclick(mouseRight)
	mouse_state.clearclick(mouseMiddle)
end sub


'====================================== Text Input ========================================

' Get text input by assuming a US keyboard layout and reading scancodes rather than using the io backend.
' Also supports alt- combinations for the high 128 characters
' Always returns real input, even if replaying input.
function get_ascii_inputtext () as string
	dim shift as integer = 0
	dim ret as string

	if real_keyval(scCtrl) > 0 then return ""

	if real_keyval(scShift) and 1 then shift += 1
	if real_keyval(scAlt) and 1 then shift += 2   'for characters 128 and up

	for i as integer = 0 to 53
		dim effective_shift as integer = shift
		if shift <= 1 andalso real_keyval(scCapsLock) > 0 then
			select case i
				case scQ to scP, scA to scL, scZ to scM
					effective_shift xor= 1
			end select
		end if
		if real_keyval(i) > 1 then
			ret &= key2text(effective_shift, i)
		end if
	next i

	' Space and numpad are missing from key2text
	if real_keyval(scSpace) > 1 then ret &= " "

	' Note: On Windows, if numlock is on and you press Shift and 0-9 or . on the numpad
	' then Shift will appear unpressed! Backend independent, happens in Win XP and 10.
	' Not a keyboard artifact.
	' We do this even if remap_numpad = NO, because that's how native text input works.
	if (real_keyval(scNumLock) and 1) xor (shift and 1) then
		'NOTE: When NumLock is off, numpad is mapped to Left, Home, etc, by keyval
		'but when it's on we *don't* map it to 1, +, etc!  That's because we want
		'the option of having numpad separate. And you should ideally be reading
		'text input rather than checking sc1, etc.
		if real_keyval(scNumpadAsterisk) > 1 then ret &= "*"
		if real_keyval(scNumpadMinus) > 1 then ret &= "-"
		if real_keyval(scNumpadPlus) > 1 then ret &= "+"
		' (Bug: gfx_fb reports both scSlash and scNumpadSlash)
		if gfxbackend <> "fb" and real_keyval(scNumpadSlash) > 1 then ret &= "/"

		' (Bug: gfx_fb on Windows never reports scNumpad5 at all!)
		for i as integer = 0 to ubound(numpad2text)
			if real_keyval(scNumpad7 + i) > 1 then
				ret &= numpad2text(i)
			end if
		next
	end if
	' Note, OSes differ on when they report text input from numpad keys (this seems
	' to be backend-independent):
	' X11 (both FB and SDL): when numlock XOR shift is pressed
	' Windows (FB, SDL, directx): only when numlock on and shift not pressed
	' (Also, on Windows, status of numlock is buggy: for gfx_sdl and gfx_directx,
	' after user turns it off, state doesn't update until next keypress,
	' while gfx_fb doesn't report it at all)

	return ret
end function

' Returns text input from the backend since the last call.
' Always returns real input, even if replaying input.
local function read_inputtext () as string
	if disable_native_text_input then
		return get_ascii_inputtext()
	end if

	'AFAIK, this is will still work on all platforms except X11 with SDL
	'even if inputtext was not enabled; however you'll get a warning when
	'getinputtext is called.
	dim w_in as wstring * 64
	if io_textinput then io_textinput(w_in, 64)

	'OK, so here's the hack: one of the alt keys (could be either) might be used
	'as a 'shift' or compose key, but if it's not, we want to support the old
	'method of entering extended characters (128 and up) using it. This will
	'backfire if the key face/base characters aren't ASCII

	dim force_native_input as bool = NO

	for i as integer = 0 to len(w_in) - 1
		if w_in[i] > 127 then force_native_input = YES
	next

	if force_native_input = NO andalso real_keyval(scAlt) and 1 then
		'Throw away w_in
		return get_ascii_inputtext()
	end if


	dim as integer icons_low, icons_high
	if get_font_type(current_font()) = ftypeLatin1 then
		icons_low = 127
		icons_high = 160
	else
		icons_low = 127
		icons_high = 255
	end if

	if io_textinput then
		'if len(w_in) then print #fh, "input :" & w_in
		' Now we need to convert from unicode to the game's character set (7-bit ascii or Latin-1)
		dim ret as string = ""
		dim force_shift as bool = NO
		for i as integer = 0 to len(w_in) - 1
			if w_in[i] > 255 then
				select case w_in[i]
					case &hF700 to &hF746:
						'Ignore Mac unicode for arrow keys, pgup+pgdown,
						' delete, misc other keys. I don't know if the
						' upper bound of &hF746 is high enough, but it
						' blocks all the keys I could find on my keyboard.
						' --James
						continue for
					case 304:
						'Ignore COMBINING MACRON on most platforms, but
						'use it to shift the next char on Android
#IFDEF __FB_ANDROID__
						force_shift = YES
#ENDIF
						continue for
				end select
				'debug "unicode char " & w_in[i]
				ret += "?"
			elseif w_in[i] = 127 then
				'Delete (only sent on OSX). Ignore; we use scancodes instead.
			elseif w_in[i] >= icons_low and w_in[i] <= icons_high then
				ret += "?"
			elseif w_in[i] < 32 then
				'Control character. What a waste of 8-bit code-space!
				'Note that we ignore newlines... because we've always done it that way
			else
				dim ch as string = chr(w_in[i])
				if force_shift then
					force_shift = NO
					ch = UCASE(ch)
					select case ch
						'FIXME: it would be better to loop through the key2text array
						'here, but it fails to initialize on Android
						'(But both this and key2text assume US keyboard so really shouldn't
						'be doing this. Is SDL 1.2 text input on Android just plain broken?)
						case "1": ch = "!"
						case "2": ch = "@"
						case "3": ch = "#"
						case "4": ch = "$"
						case "5": ch = "%"
						case "6": ch = "^"
						case "7": ch = "&"
						case "8": ch = "*"
						case "9": ch = "("
						case "0": ch = ")"
						case "-": ch = "_"
						case "=": ch = "+"
						case "[": ch = "{"
						case "]": ch = "}"
						case ";": ch = ":"
						case "'": ch = """"
						case "`": ch = "~"
						case "\": ch = "|"
						case ",": ch = "<"
						case ".": ch = ">"
						case "/": ch = "?"
					end select
				end if
				ret += ch
			end if
		next
		return ret
	else
		return get_ascii_inputtext()
	end if
end function

'If using gfx_sdl and gfx_directx this is Latin-1, while gfx_fb doesn't currently support even that
function getinputtext () as string
	if replay.active then
		return replay_input.kb.inputtext
	end if

	if disable_native_text_input = NO then
		'Only show this message if getinputtext is called incorrectly twice in a row,
		'to filter out instances when a menu with inputtext disabled exits back to
		'one that expects it enabled, and getinputtext is called before the next call to setkeys.
		static last_call_was_bad as bool = NO
		if inputtext_enabled = NO and last_call_was_bad then
			debuginfo "getinputtext: not enabled"
		end if
		last_call_was_bad = (inputtext_enabled = NO)
	end if

	return real_input.kb.inputtext
end function


'==========================================================================================
'                              Checking/waiting for keypresses
'==========================================================================================


'Checks the keyboard and optionally joystick for keypress events.
'trigger_level: 1 to trigger on a held key,
'               2 to trigger on keypress (inc. repeat)
'               4 to trigger only on new keypress.
'Returns scancode if one is found, 0 otherwise.
'Use this instead of looping over all keys, to make sure alt filtering and joysticks work
function anykeypressed (checkjoystick as bool = YES, checkmouse as bool = YES, trigger_level as KeyBits = 2) as KBScancode
	for i as KBScancode = 0 to scLAST
		'check scAlt only, so Alt-filtering (see setkeys) works
		if i = scLeftAlt or i = scRightAlt or i = scUnfilteredAlt then continue for
		' Ignore capslock and numlock because they always appear pressed when on,
		' and it doesn't really matter if they doesn't work for 'press a key' prompts.
		' To be on the safe said, ignore scroll lock too. Though with gfx_sdl,
		' on Windows howing down scrolllock causes SDL to report key_up/key_down
		' wait every tick, while on linux it seems to behave like a normal key
		if i = scCapsLock or i = scNumLock or i = scScrollLock then continue for

		if keyval(i) >= trigger_level then
			return i
		end if
	next

	if checkjoystick then
		'Note that keyval(ccAny) only checks joystick buttons mapped to controls
		for key as KBScancode = scJoyButton1 to scJoyLAST
			if keyval(key) >= trigger_level then
				return key
			end if
		next
	end if

	if checkmouse then
		'If trigger_level=1, check both release or currently down, eg. to ensure
		'waitforkeyrelease hides mouse button release.
		dim bitvec as integer = mouse_state.release
		if trigger_level = 1 then bitvec or= mouse_state.buttons
		for button as integer = 0 to 15
			if bitvec and (1 shl button) then
				return scMouseLeft + button
			end if
		next button
	end if
end function

'Waits for a new keyboard key, mouse or joystick button press. Clears the keypress and returns the scancode.
'If wait_for_resize = YES, also returns scResize if the window was resized.
'By default waits for a new keypress, adjust with trigger_level
function waitforanykey (wait_for_resize as bool = NO, trigger_level as KeyBits = 4) as KBScancode
	dim key as KBScancode
	dim sleepjoymouse as integer = 5
	dim remem_speed_control as bool = use_speed_control
	dim original_resolution as XYPair = windowsize
	use_speed_control = YES
	skipped_frame.show()  'If we frame-skipped last frame, better show it
	setkeys
	do
		setwait 55, 200
		io_pollkeyevents()
		setkeys
		key = anykeypressed(sleepjoymouse = 0, sleepjoymouse = 0, trigger_level)
		if key then
			snapshot_check  'In case F12 pressed, otherwise it wouldn't work
			setkeys  'Clear the keypress
			use_speed_control = remem_speed_control
			return key
		end if
		if sleepjoymouse > 0 then
			'Delay before checking joystick so user has time to return
			'stick to center, and delay mouse because mouse button might be pressed
			'when called, and releasing it counts as input.
			sleepjoymouse -= 1
		end if
		if wait_for_resize andalso windowsize <> original_resolution then
			use_speed_control = remem_speed_control
			return scResize
		end if
		if dowait then
			' Redraw the screen occasionally in case something like an overlay is drawn
			setvispage getvispage
		end if
	loop
end function

'Wait for all keys, and joystick and mouse buttons to be released
sub waitforkeyrelease ()
	setkeys
	'anykeypressed checks scAlt instead of scUnfilteredAlt
	while anykeypressed(YES, YES, 1) or keyval(scUnfilteredAlt)
		if getquitflag() then exit sub
		io_pollkeyevents()
		setwait 15
		setkeys
		dowait
	wend
end sub

'Without changing the results of keyval or readmouse, check whether a key has been pressed,
'mouse button clicked, or window close requested since the last call to setkeys.
'NOTE: any such keypresses or mouse clicks are lost! This is OK for the current purposes
'NOTE: This checks the real keyboard state while replaying input.
function interrupting_keypress () as bool
	dim starttime as double = timer
	dim ret as bool = NO

	io_pollkeyevents()

	dim keybd_dummy(scLAST) as KeyBits
	dim mouse as MouseInfo

	'Note: This use of gfxmutex is necessary even if FB bug 885 gets fixed (see r642 & r708)
	GFX_ENTER
	io_keybits(@keybd_dummy(0))
	io_mousebits(mouse.x, mouse.y, mouse.wheel, mouse.buttons, mouse.clicks)
	GFX_EXIT

	debug_if_slow(starttime, 0.005, "")

	' Check for attempt to quit program
	if keybd_dummy(scPageup) > 0 and keybd_dummy(scPagedown) > 0 and keybd_dummy(scEsc) > 1 then closerequest = YES
	if closerequest then
#ifdef IS_GAME
		exit_gracefully()
#else
		ret = YES
#endif
	end if

	for i as KBScancode = 0 to scLAST
		'Check for new keypresses
		if keybd_dummy(i) and 2 then ret = YES
	next

	if mouse.clicks then ret = YES

	if ret then
		'Crap, this is going to desync the replay since the result of interrupting_keypress isn't recorded
		'(No problem if paused)
		if record.active then
			stop_recording_input "Recording ended by interrupting keypress"
		end if
		if replay.active then
			stop_replaying_input "Replay ended by interrupting keypress"
		end if
	end if

	return ret
end function


'==========================================================================================
'                               setkeys (update input state)
'==========================================================================================


sub KeyArray.init(maxkey as integer)
	redim keys(maxkey)
	redim key_down_ms(maxkey)
	init_controls()
end sub

constructor KeyboardState()
	init(scLAST)
end constructor

sub KeyboardState.reset()
	init(ubound(keys))
	delayed_alt_keydown = NO
	inputtext = ""
end sub

constructor JoystickState()
	init(joyLAST)
end constructor


'================================== Key mappings ==========================================

sub KeyboardState.init_controls()
	redim controls(12)
	controls(0)  = TYPE(scUp,     ccUp)
	controls(1)  = TYPE(scDown,   ccDown)
	controls(2)  = TYPE(scLeft,   ccLeft)
	controls(3)  = TYPE(scRight,  ccRight)
	#ifdef IS_GAME
	controls(4)  = TYPE(scCtrl,   ccUse)  'Wiped by set_basic_key_mappings
	#endif
	controls(5)  = TYPE(scSpace,  ccUse)
	controls(6)  = TYPE(scEnter,  ccUse)
	#ifdef IS_GAME
	controls(7)  = TYPE(scAlt,    ccMenu)  'Wiped by set_basic_key_mappings
	controls(8)  = TYPE(scAlt,    ccCancel)  'Wiped by set_basic_key_mappings
	#endif
	controls(9)  = TYPE(scEsc,    ccMenu)
	controls(10) = TYPE(scEsc,    ccCancel)
	controls(11) = TYPE(scEsc,    ccFlee)
	controls(12) = TYPE(scTab,    ccFlee)  'Who knew?
end sub

sub JoystickState.init_controls()
	redim controls(8)
	controls(0) = TYPE(joyUp,       ccUp)
	controls(1) = TYPE(joyDown,     ccDown)
	controls(2) = TYPE(joyLeft,     ccLeft)
	controls(3) = TYPE(joyRight,    ccRight)
	controls(4) = TYPE(joyA,        ccUse)
	controls(5) = TYPE(joyB,        ccCancel)
	controls(6) = TYPE(joyB,        ccMenu)
	controls(7) = TYPE(joyStart,    ccMenu)
	controls(8) = TYPE(joyB,        ccRun)

	'Typically the first four buttons will be A/B/X/Y buttons, but not always in that order.
	'So previously we used to map buttons 3 and 4 to use/cancel, but that's a nuiscance for scripted controls.
	' controls(7) = TYPE(joyButton3,  ccUse)
	' controls(8) = TYPE(joyButton4,  ccMenu)
	' controls(9) = TYPE(joyButton4,  ccRun)
end sub

'Remove all key mappings to or from a scancode/controlcode (cc* constant).
'Reads replayed state, if any (real_keys = NO)
'key:     either a cc* constant (all key mappings to that cc will be removed)
'         or an ordinary KBScancode or JoyButton (any mapping from that key/button will be removed)
'joynum:  -2 for keyboard, -1 for any joystick, 0-3 for single joystick (similar to player_keyval())
sub delete_key_mappings(key as integer, joynum as integer = -2)
	dim inputst as InputState ptr = iif(replay.active, @replay_input, @real_input)

	'Maybe this device-specific stuff is overengineering, don't need it yet...
	dim device as KeyArray ptr
	if joynum = -2 then
		device = @inputst->kb
	elseif joynum = -1 then
		for joynum = 0 to ubound(inputst->joys)
			delete_key_mappings key, joynum
		next
		exit sub
	else
		if joynum < 0 orelse joynum > ubound(inputst->joys) then exit sub
		device = @inputst->joys(joynum)
	end if

	for i as integer = 0 to ubound(device->controls)
		with device->controls(i)
			if .ckey = key orelse .scancode = key then .ckey = 0
		end with
	next
end sub

'Return the current (keyboard) key bindings
'TODO: only keyboard, not joystick mappings, as currently we have no use for that
sub get_key_mappings(controls() as ControlKey)
	dim inputst as InputState ptr = iif(replay.active, @replay_input, @real_input)
	redim controls(ubound(inputst->kb.controls))
	for i as integer = 0 to ubound(controls)
		controls(i) = inputst->kb.controls(i)
	next
end sub

'Overwrite the current (keyboard) key bindings
'TODO: see above
sub set_key_mappings(controls() as ControlKey)
	dim inputst as InputState ptr = iif(replay.active, @replay_input, @real_input)
	redim inputst->kb.controls(ubound(controls))
	for i as integer = 0 to ubound(controls)
		inputst->kb.controls(i) = controls(i)
	next
end sub

'Sets up the keyboard key mappings the way Custom does (unlike Game, which allows Ctrl for Use
'and Alt for Cancel)
sub set_basic_key_mappings()
	dim inputst as InputState ptr = iif(replay.active, @replay_input, @real_input)

	inputst->kb.init_controls()
	delete_key_mappings scCtrl
	delete_key_mappings scAlt
end sub


'============================ Update KB/joys from backend =================================

'Poll io backend to update key state bits, and then handle all special scancodes.
'keybd() should be dimmed at least (0 to scLAST)
sub KeyboardState.update_keybits()
	dim winstate as WindowState ptr
	winstate = gfx_getwindowstate()

	GFX_ENTER
	io_keybits(@keys(0))
	GFX_EXIT

	'State of keys(0 to scLAST) at this point:
	'bit 0: key currently down
	'bit 1: key down since last io_keybits call
	'bit 2: zero

	'debug "raw scEnter = " & keys(scEnter) & " scAlt = " & keys(scAlt)

	'DELETEME (after a lag period): This is a temporary fix for gfx_directx not knowing about scShift
	'(or any other of the new scancodes, but none of the rest matter much (maybe
	'scPause) since there are no games that use them).
	'(Ignore bit 2, because that isn't set yet)
	if ((keys(scLeftShift) or keys(scRightShift)) and 3) <> (keys(scShift) and 3) then
		keys(scShift) = keys(scLeftShift) or keys(scRightShift)
	end if
	'TODO: Actually, wouldn't it make more sense to set all the combined scancodes here instead of
	'duplicating that in all backends?
	'These two scancodes are set here instead of in backends...
	keys(scAnyEnter) = keys(scEnter) or keys(scNumpadEnter)
	keys(scMeta) = keys(scLeftMeta) or keys(scRightMeta)

	'Backends don't know about scAlt, only scUnfilteredAlt
	keys(scAlt) = keys(scUnfilteredAlt)

	'Don't fire ctrl presses when alt down due to large number of WM shortcuts containing ctrl+alt
	'(Testing delayed_alt_keydown is just a hack to add one tick delay after alt up,
	'which is absolutely required)
	if (keys(scAlt) and 1) or delayed_alt_keydown then

		if keys(scEnter) and 6 then
			keys(scEnter) and= 1
			delayed_alt_keydown = NO
		end if

		keys(scCtrl) and= 1
		keys(scLeftCtrl) and= 1
		keys(scRightCtrl) and= 1
	end if

	'Calculate new "new keypress" bit (bit 2)
	for key as KBScancode = 0 to scLAST
		keys(key) and= 3
		if key = scAlt then
			'Special behaviour for alt, to ignore pesky WM shortcuts like alt+tab, alt+enter:
			'Wait until alt has been released, without losing focus, before
			'causing a key-down event.
			'Also, special case for alt+enter, since that doesn't remove focus

			'Note: this is only for scAlt, not scLeftAlt, scRightAlt, which aren't used by
			'the engine, only by games. Maybe those shoudl be blocked too
			'Note: currently keyval causes key-repeat events for alt if delayed_alt_keydown = YES

			if keys(scAlt) and 2 then
				if delayed_alt_keydown = NO then
					keys(scAlt) -= 2
				end if
				delayed_alt_keydown = YES
			end if

			/'
			for scancode as integer = 0 to scLAST
				if scancode <> scUnfilteredAlt and scancode <> scAlt and scancode <> scLeftAlt and scancode <> scRightAlt and (keys(scancode) and 1) then
					delayed_alt_keydown = NO
				end if
			next
			'/
			if winstate andalso winstate->focused = NO then
				delayed_alt_keydown = NO
			end if

			if (keys(scAlt) and 1) = 0 andalso delayed_alt_keydown then
				keys(scAlt) or= 6
				delayed_alt_keydown = NO
			end if

		'elseif key = scCtrl or key = scLeftCtrl or key = scRightCtrl then

		else
			'Duplicate bit 1 to bit 2
			 keys(key) or= (keys(key) and 2) shl 1
		end if
	next

end sub

sub joy_axes_to_buttons(jx as integer, jy as integer, byref keyup as KeyBits, byref keydown as KeyBits, byref keyleft as KeyBits, byref keyright as KeyBits, axis_threshold as integer)

	'Instead of treating X and Y separately, which would divide the range of possible values
	'into quadrants, we divide it into a circular dead zone and 8 surrounding sectors
	dim as double angle, norm
	norm = sqr(jx ^ 2 + jy ^ 2)
	angle = atan2(-jy, jx) * 6 / 3.14159265  'range -6.0 - 6.0, 0 is right, 3 is up, -3 is down
	'if jx or jy then ? strprintf("%d,%d -> norm %f ang %f",jx, jy, norm, angle)
	if norm >= axis_threshold then
		if angle > 1  andalso angle < 5  then keyup    or= 8
		if angle > -5 andalso angle < -1 then keydown  or= 8
		if angle > -2 andalso angle < 2  then keyright or= 8
		if angle > 4  orelse  angle < -4 then keyleft  or= 8
	end if
end sub

'This is similar to io_keybits for a single joystick:
'it updates a JoystickState with currently-down and new-keypress bits.
'Basically it does a similar thing to the pollingthread, emulating new-keypress
'bits since the backend may not report them.
sub JoystickState.update_keybits(joynum as integer)
	if joysticks_globally_disabled then exit sub
	memset(@state, 0, SIZEOF(state))
	state.structsize = IOJOYSTICKSTATE_SZ


	dim starttime as double = timer
	GFX_ENTER
	if io_get_joystick_state then
		dim ret as integer
		ret = io_get_joystick_state(joynum, @state)
		if ret > 0 then
			'Failed to read/not present. Continue, wiping keys()
		end if
	elseif io_readjoysane then
		dim as integer jx, jy
		if io_readjoysane(joynum, state.buttons_down, jx, jy) = 0 then
			'Failed to read/not present. Continue, wiping keys()
			'(Warning: if gfx_directx can't read a joystick, it is removed and the others
			'are renumbered)
		else
			'io_readjoysane reports -100 to 100, not -1000 to 1000
			state.axes(axisX) = jx * AXIS_LIMIT / 100
			state.axes(axisY) = jy * AXIS_LIMIT / 100
		end if
	else
		'Backend doesn't support joysticks! Continue, wiping keys()
	end if
	GFX_EXIT
	debug_if_slow(starttime, 0.01, joynum)

	' Unless the gfx backend reports state.buttons_new (only gfx_sdl2),
	' it only tells us which buttons are currently down,
	' like io_updatekeys, not which have new keypresses, like io_keybits,
	' so this is similar to the former (as handled in pollingthread).

	' Clear bits 1 (keypress event) and 2 (new keypress), leave bit 0 (key down)
	for scancode as JoyButton = 0 to ubound(keys)
		keys(scancode) and= 1
	next

	' Set pressed buttons

	' Map axes 0, 1 to dpad buttons
	if prefbit(53) = NO then ' "!Map joystick (left) stick to dpad"
		joy_axes_to_buttons state.axes(axisX), state.axes(axisY), keys(joyUp), _
				    keys(joyDown), keys(joyLeft), keys(joyRight), axis_threshold
	end if
	' Convenience buttons for using right thumbstick
	joy_axes_to_buttons state.axes(axisRightX), state.axes(axisRightY), keys(joyRStickUp), _
			    keys(joyRStickDown), keys(joyRStickLeft), keys(joyRStickRight), axis_threshold

	if state.info andalso state.info->have_bindings then
		' These two trigger buttons are reported as axes, not buttons, by XInput and SDL2
		if state.axes(axisL2) > axis_threshold then keys(joyL2) or= 8
		if state.axes(axisR2) > axis_threshold then keys(joyR2) or= 8
	else
		' If we don't have button bindings also map the first hat as the dpad, which is univerally
		' correct for gamepads.
		' (E.g. on this here PSX controller with thumbsticks, using a usb adaptor, the
		' dpad reports as axes 0/1 with analog off, and as hat 0 (or dpad under SDL2) with analog on)
		for bitn as integer = 0 to 3
			if state.hats(0) and (1 shl bitn) then keys(joyLeft + bitn) or= 8
		next
	end if

	for btn as integer = 0 to 31
		if state.buttons_down and (1 shl btn) then
			keys(joyButton1 + btn) or= 8
		end if
	next

	' Convert those bits we just set into KeyBits bits 0 & 1 (detecting new keypresses)
	keystate_convert_bit3_to_keybits(keys())

	' Duplicate bit 1 (key event) to bit 2 (new keypress)
	for scancode as JoyButton = 0 to ubound(keys)
		dim byref key as KeyBits = keys(scancode)
		key = (key and 3) or ((key and 2) shl 1)
	next
	' Add in explicit new-keypress (mirror to key-event) bits from the backend, if reported
	for btn as integer = 0 to 31
		if state.buttons_new and (1 shl btn) then
			keys(joyButton1 + btn) or= 6
		end if
	next
end sub

function KeyboardState.is_arrow_key (key as KBScancode) as bool
	return (key = scLeft orelse key = scRight orelse key = scUp orelse key = scDown)
end function

function JoystickState.is_arrow_key (key as JoyButton) as bool
	'Any of joy[RStick]Up/Down/Left/Right
	return (key >= joyLeft andalso key <= joyRStickDown)
end function

' Updates kbstate.key_down_ms and arrow_key_down_ms
sub KeyArray.update_keydown_times (inputst as InputState)
	arrow_key_down_ms = 0

	for key as KBScancode = 0 to ubound(keys)
		if (keys(key) and 4) or (keys(key) and 1) = 0 then
			key_down_ms(key) = 0
		end if
		if keys(key) and 1 then
			key_down_ms(key) += inputst.elapsed_ms
		end if

		if is_arrow_key(key) then
			arrow_key_down_ms = large(arrow_key_down_ms, key_down_ms(key))
		end if
	next
end sub

sub setkeys (enable_inputtext as bool = NO)
'Updates the keyboard state to reflect new keypresses
'since the last call, also clears all keypress events (except key-is-down)
'
'Also calls allmodex_controls() to handle key hooks which work everywhere.
'
'enable_inputtext needs to be true for getinputtext to work;
'however there is a one tick delay before coming into effect.
'Passing enable_inputtext may cause certain "combining" keys to stop reporting
'key presses. Currently this only happens with gfx_sdl on X11 (it is an X11
'limitation). And it probably only effects punctuation keys such as ' or ~
'(naturally those keys could be anywhere, but a good rule of thumb seems to be
'to avoid QWERTY punctuation keys)
'For more, see http://en.wikipedia.org/wiki/Dead_key
'
'Note that key repeat is NOT added to kb.keys() (it's done by "post-processing" in keyval)

	dim starttime as double = timer

	if replay.active = NO and disable_native_text_input = NO then
		if enable_inputtext then enable_inputtext = YES
		if inputtext_enabled <> enable_inputtext then
			inputtext_enabled = enable_inputtext
			io_enable_textinput(inputtext_enabled)
		end if
	end if

	'While playing back a recording we still poll for keyboard
	'input, but this goes in the separate real_input.kb.keys() array so it's
	'invisible to the game.

	dim time_passed as double = TIMER - last_setkeys_time
	real_input.elapsed_ms = bound(1000 * time_passed, 0, 255)
	last_setkeys_time = TIMER

	' Get real joystick state. Do this before keyboard, because
	' setkeys_update_keybd will call map_joystick_to_keys
	for joynum as integer = 0 to ubound(real_input.joys)
		real_input.joys(joynum).update_keybits joynum
		real_input.joys(joynum).update_keydown_times real_input
	next

	' Get real keyboard state
	real_input.kb.update_keybits
	real_input.kb.update_keydown_times real_input
	real_input.kb.inputtext = read_inputtext()

	if replay.active then
		' Updates replay_input.kb.keys(), .kb.inputtext, .elapsed_ms
		' FUTURE: updates replay_input.joys
		replay_input_tick

		' Updates kb.key_down_ms()
		replay_input.kb.update_keydown_times replay_input

		' Update replay_input.joys().key_down_ms()
		for joynum as integer = 0 to ubound(replay_input.joys)
			replay_input.joys(joynum).update_keydown_times(replay_input)
		next
	end if

	'Taking a screenshot with gfx_directx is very slow, so avoid timing that
	if log_slow then debug_if_slow(starttime, 0.005, replay.active)

	'Handle special keys, possibly clear or add keypresses. Might recursively call setkeys.
	allmodex_controls()

	' Record input, after filtering of keys by allmodex_controls.
	if record.active then
		record_input_tick ()
	end if

	' Call io_mousebits
	update_mouse_state()

	' Update active_seconds, if have been active within some interval
	if anykeypressed() THEN
		last_active_time = last_setkeys_time
	end if
	if last_setkeys_time < last_active_time + idle_time_threshold then
		active_seconds += time_passed
	end if

	' Custom/Game-specific global controls, done last so that there can't be interference
	#if defined(IS_GAME) or defined(IS_CUSTOM)
		static entered as bool
		if entered = NO then
			entered = YES
			global_setkeys_hook
			entered = NO
		end if
	#endif
end sub

sub setquitflag (newstate as bool = YES)
	closerequest = newstate
end sub

function getquitflag () as bool
	return closerequest
end function

' This callback is used by backends, possibly from another thread.
' Returns INT_MIN if the event was not understood, otherwise return value is event-dependent.
function post_event cdecl (event as EventEnum, arg1 as intptr_t = 0, arg2 as intptr_t = 0) as integer
	select case event
	case eventTerminate
		closerequest = YES
		return 0
	case eventFullscreened
		'arg1 is the new state
		user_toggled_fullscreen = YES
		return 0
	end select
	debuginfo "post_event: unknown event " & event & " " & arg1 & " " & arg2
	return INT_MIN
end function

sub post_terminate_signal cdecl ()
	closerequest = YES
end sub


'==========================================================================================
'                                          Mouse
'==========================================================================================


function havemouse() as bool
	'atm, all backends support the mouse, or don't know
	'Tip: It's much more useful to check readmouse().active instead.
	return YES
end function

' Cause mouse cursor to be always hidden
sub hidemousecursor ()
	io_setmousevisibility(cursorHidden)
	cursorvisibility = cursorHidden
end sub

' Cause mouse cursor to be always visible, except on touchscreen devices
sub showmousecursor ()
	io_setmousevisibility(cursorVisible)
	cursorvisibility = cursorVisible
end sub

' Use when the mouse is not in use:
' Hide the mouse cursor in fullscreen, and show it when windowed.
sub defaultmousecursor ()
	io_setmousevisibility(cursorDefault)
	cursorvisibility = cursorDefault
end sub

sub setcursorvisibility (state as CursorVisibility)
	select case state
	case cursorVisible, cursorHidden, cursorDefault
		io_setmousevisibility(state)
		cursorvisibility = state
	case else
		showbug "Bad setcursorvisibility(" & state & ") call"
	end select
end sub

function getcursorvisibility () as CursorVisibility
	return cursorvisibility
end function

local sub check_for_released_mouse_button(buttonnum as MouseButton)
	if (mouse_state.last_buttons and buttonnum) andalso (mouse_state.buttons and buttonnum) = 0 then
		'If the button was released since the last tick, turn on .release
		mouse_state.release or= buttonnum
	else
		'All the rest of the time, .release should be off
		mouse_state.release and= not buttonnum
	end if
end sub

' Called from setkeys to update the internal mouse state
sub update_mouse_state ()
	dim starttime as double = timer

	mouse_state.lastpos = mouse_state.pos

	mouse_state.last_buttons = mouse_state.buttons

	GFX_ENTER   'Just in case
	io_mousebits(mouse_state.x, mouse_state.y, mouse_state.wheel, mouse_state.buttons, mouse_state.clicks)
	GFX_EXIT

	'Ignore mouse clicks that focus the window. If you clicked, it's already
	'focused, so we consider the previous focus state instead.
	'(Not necessary for gfx_sdl2, already filters those clicks out.)
	static prev_focus_state as bool
	dim window_state as WindowState ptr = gfx_getwindowstate()
	if mouse_state.buttons = 0 then
		prev_focus_state = window_state->focused
	elseif prev_focus_state = NO then
		mouse_state.buttons = 0
		mouse_state.clicks = 0
		mouse_state.release = 0
		mouse_state.last_buttons = 0
	end if

	for button as integer = 0 to 15
		check_for_released_mouse_button(1 shl button)
	next

	if (mouse_state.buttons and mouseLeft) orelse (mouse_state.release and mouseLeft) then
		mouse_state.left_click_age += 1
	else
		mouse_state.left_click_age = 0
	end if

	mouse_state.wheel *= -1
	mouse_state.wheel_delta = mouse_state.wheel - last_mouse_wheel
	mouse_state.wheel_clicks = mouse_state.wheel \ 120 - last_mouse_wheel \ 120
	last_mouse_wheel = mouse_state.wheel

	'In the following, "offscreen" includes over the window decorations or
	'whenever the window doesn't have mouse focus even if mouse is over it.
	'
	'==Mouse position while offscreen, when NOT dragging from inside the window==
	'
	'gfx_sdl/alleg:
	'         Return last onscreen position when the mouse is offscreen.
	'gfx_sdl2:Returns last onscreen position when the mouse is outside the window.
	'         Returns real position if the mouse is over the window even if it
	'         it isn't focused.
	'gfx_fb:  (At least on X11) May return first OFFSCREEN position instead of
	'         last onscreen, due to freezing mouse input fractionally late.
	'         gfx_getwindowstate->mouse_over always false when the window doesn't
	'         have focus.
	'directx: Unknown.
	'
	'==Mouse buttons & wheel while offscreen==
	'
	'gfx_fb:  If you release a mouse button offscreen, it becomes stuck until
	'         either the mouse moves back onscreen or the window loses focus.
	'         [wheel scrolls offscreen are registered when you move back onscreen?
	'         I can't reproduce that anymore.]
	'gfx_alleg: button state continues to work offscreen but wheel scrolls are not registered
	'gfx_sdl/sdl2: Doesn't report buttons offscreen (unless dragging). Wheel movement
	'         is reported if the mouse is over the window, even if it's not focused.
	'         SDL 1.2 doesn't know about the OS's wheel speed setting.
	'directx: Unknown.

	mouse_state.moved = mouse_state.lastpos <> mouse_state.pos

	dim diff as XYPair = mouse_state.lastpos - mouse_state.pos
	mouse_state.moved_dist = sqrt(diff.x * diff.x + diff.y * diff.y)

	mouse_state.active = window_state->mouse_over and window_state->focused

	'==Behaviour of clicking and dragging from inside the window to outside==
	'
	'gfx_sdl/fb: mouse.active is false while dragging off the window
	'gfx_sdl2: mouse.active is true while dragging off the window
	'gfx_fb:  Mouse input goes dead while outside until moved back into window.
	'         (So mouse.dragging doesn't become false until back over window.)
	'gfx_sdl/sdl2: Mouse acts as if clipped to the window while button is down and
	'         button state continues to be reported, until button is released.
	'directx: Mouse is truely clipped to the window while button is down.
	'         (So mouse.active is true while dragging off the window?)
	'gfx_alleg:Unknown.

	if mouse_state.dragging then
		'Test whether drag ended
		if (mouse_state.clicks and mouse_state.dragging) orelse (mouse_state.buttons and mouse_state.dragging) = 0 then
			mouse_state.dragging = 0
			'Preserve .clickstart so that you can see what the drag was upon release
		else
			mouse_state.drag_dist += mouse_state.moved_dist
		end if
	else
		'Dragging is only tracked for a single button at a time, and clickstart is not updated
		'while dragging either. So we may now test for new drags or clicks.
		for button as integer = 0 to 15
			dim mask as MouseButton = 1 shl button
			if mouse_state.clicks and mask then
				'Do not flag as dragging until the second tick
				mouse_state.clickstart = mouse_state.pos
			elseif mouse_state.buttons and mask then
				'Button still down
				mouse_state.dragging = mask
				exit for
			end if
		next
		'Note that we delay zeroing this until the tick after a drag ends
		mouse_state.drag_dist = 0
	end if

	' If you released a mouse grab (mouserect) and then click on the
	' window, resume the mouse grab. (This is, and should be, called even if someone else
	' also called pause_mouserect)
	if mouse_state.clicks <> 0 andalso mouse_grab_scrolllock_overridden then
		mouse_grab_scrolllock_overridden = NO
		resume_mouserect
	end if

	if log_slow then debug_if_slow(starttime, 0.005, mouse_state.clicks)
end sub

' Get the state of the mouse at the last setkeys call (or after putmouse, mouserect).
' So make sure you call this AFTER setkeys.
function readmouse () byref as MouseInfo
	return mouse_state
end function

sub MouseInfo.clearclick(button as MouseButton)
	clicks and= not button
	release and= not button
	' Cancel for good measure, but not really needed
	dragging and= not button
end sub

sub movemouse (x as integer, y as integer)
	'Maybe we shouldn't cause the mouse to jump back to our window if it's not over it? (mouse_state.active)
	GFX_ENTER
	io_setmouse(x, y)
	GFX_EXIT

	' Don't call io_mousebits to get the new state, since that will cause clicks and movements to get lost,
	' and is difficult to support in .ohrkeys.
	mouse_state.x = x
	mouse_state.y = y
end sub

'Restrict the mouse to a rectangle (xmax, ymax are inclusive).
'Call mouserect(-1, -1, -1, -1) to end.
sub mouserect (xmin as integer, xmax as integer, ymin as integer, ymax as integer)
	dim norect as bool = (xmin = -1 and xmax = -1 and ymin = -1 and ymax = -1)

	if norect then
		mouse_grab_requested = NO
	else
		remember_mouse_grab = TYPE<RectPoints>(XY(xmin, ymin), XY(xmax, ymax))
		mouse_grab_requested = YES
		'Nested mouserects are not supported.
		mouse_grab_nested_pauses = 0
	end if

	' Set window title to tell the player about scrolllock to escape mouse-grab
	' gfx_directx does this itself, including handling scroll lock
	if gfxbackend <> "directx" then
		if norect then
			settemporarywindowtitle remember_title
		else
#IFDEF __FB_DARWIN__
			settemporarywindowtitle remember_title & " (F14 to free mouse)"
#ELSE
			settemporarywindowtitle remember_title & " (ScrlLock to free mouse)"
#ENDIF
		end if
	end if

	GFX_ENTER
	io_mouserect(xmin, xmax, ymin, ymax)
	GFX_EXIT

	if norect = NO then
		' Don't call io_mousebits to get the new state, since that will cause clicks and movements to get lost,
		' and is difficult to support in .ohrkeys.
		mouse_state.x = bound(mouse_state.x, xmin, xmax)
		mouse_state.y = bound(mouse_state.y, ymin, ymax)
	end if
end sub

'If mouserect is in effect, free the mouse. Nestable; resume_mouserect should be called the
'same number of times (you can't nest mouserect calls, however).
'Note this does not pause mouse input or show the mouse cursor.
sub pause_mouserect
	if mouse_grab_requested then
		mouserect -1, -1, -1, -1
		mouse_grab_requested = YES
		mouse_grab_nested_pauses += 1
	end if
end sub

'Undoes one call to pause_mouserect
sub resume_mouserect
	if mouse_grab_requested then
		mouse_grab_nested_pauses -= 1
		if mouse_grab_nested_pauses <= 0 then
			mouse_grab_nested_pauses = 0
			with remember_mouse_grab
				mouserect .p1.x, .p2.x, .p1.y, .p2.y
			end with
		end if
	end if
end sub


'==========================================================================================
'                                    Extra Joystick API
'==========================================================================================

'This is also, currently, the number of players
'TODO: this always returns 4!
function num_joysticks () as integer
	dim inputst as InputState ptr = iif(replay.active, @replay_input, @real_input)
	return ubound(inputst->joys) + 1
end function

'Returns a value from -1000 to 1000
'player: 0 means merge input from all joysticks together; 1-4 is an individual joystick
function joystick_axis (axis as integer, player as integer = 0) as integer
	dim inputst as InputState ptr = iif(replay.active, @replay_input, @real_input)

	if player = 0 then  'Merge
		dim ret as integer
		for player = 1 to num_joysticks()
			dim value as integer = joystick_axis(axis, player)
			if abs(value) > abs(ret) then ret = value
		next
		return ret
	end if

	dim joynum as integer = player - 1
	if joynum < 0 or joynum > ubound(inputst->joys) then return 0  'Not an error
	dim byref joy as JoystickState = inputst->joys(joynum)

	if axis < 0 orelse axis > ubound(joy.state.axes) then return 0
	'info ptr will be NULL if not supported by backend, or not even called yet
	if joy.state.info andalso axis >= joy.state.info->num_axes then return 0
	return joy.state.axes(axis)
end function

'Can return NULL
'Player 0 just returns info on the first joystick (player 1)
function joystick_info (player as integer) as JoystickInfo ptr
	dim inputst as InputState ptr = iif(replay.active, @replay_input, @real_input)
	dim joynum as integer = large(0, player - 1)
	if joynum > ubound(inputst->joys) then return NULL  'Not an error
	return inputst->joys(joynum).state.info
end function

/' Not used yet
sub disable_joystick_input ()
	joysticks_globally_disabled = YES
end sub

sub enable_joystick_input ()
	joysticks_globally_disabled = NO
end sub
'/

'==========================================================================================
'                       Compat layer for old graphics backend IO API
'==========================================================================================
' These functions are used to supplement gfx backends not supporting
' io_mousebits or io_keybits.

'io_keybits implementation provided via the polling thread
'This is called while gfxmutex is held
sub io_amx_keybits cdecl (keybdarray as KeyBits ptr)
	for a as KBScancode = 0 to scLAST
		keybdarray[a] = pollthread.keybdstate(a)
		pollthread.keybdstate(a) and= 1
	next
end sub

'io_mousebits implementation provided via the polling thread
'This is called while gfxmutex is held
sub io_amx_mousebits cdecl (byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer, byref mclicks as integer)
	with pollthread
		'get the mouse state one last time, for good measure
		io_getmouse(mx, my, mwheel, mbuttons)
		mclicks = .mousebuttons or (mbuttons and not .mouselastbuttons)
		.mouselastbuttons = mbuttons
		.mousebuttons = 0
		mbuttons = mbuttons or mclicks
	end with
end sub

'Input: a key array with bit 3 (1<<3 == 8) set for currently pressed keys (from io_updatekeys)
'       and bit 0 set for keys pressed last tick
'Output: key array with only bits 0 and 1 set (like io_keybits)
local sub keystate_convert_bit3_to_keybits(keystate() as KeyBits)
	for scancode as integer = 0 to ubound(keystate)
		dim byref key as KeyBits = keystate(scancode)
		if (key and 9) = 8 then
			'Key is pressed, wasn't pressed last time. This is a new keypress
			key or= 2
		end if
		'move the bit (clearing it) that io_updatekeys sets from 8 to 1
		key = (key and 2) or ((key shr 3) and 1)
	next
end sub

local sub pollingthread(unused as any ptr)
	with pollthread
		while .wantquit = NO
			mutexlock gfxmutex

			dim starttime as double = timer

			'Sets bit 3 (1<<3 == 8) for currently pressed keys
			io_updatekeys(@.keybdstate(0))
			if log_slow then debug_if_slow(starttime, 0.005, "io_updatekeys")
			starttime = timer

			'Convert from io_updatekeys bits (bit 3) to io_keybits bits (bits 0 and 1)
			keystate_convert_bit3_to_keybits(.keybdstate())

			dim as integer dummy, buttons
			io_getmouse(dummy, dummy, dummy, buttons)
			.mousebuttons = .mousebuttons or (buttons and not .mouselastbuttons)
			.mouselastbuttons = buttons

			mutexunlock gfxmutex

			if log_slow then debug_if_slow(starttime, 0.005, "io_getmouse")

			sleep 15
		wend
	end with
end sub


'==========================================================================================
'                              Special overlays and controls
'==========================================================================================


'Called from setkeys. This handles keypresses which are global throughout the engine.
'(Note that backends also have some hooks, especially gfx_sdl.bas for OSX-specific stuff)
local sub allmodex_controls()
	dim ctrl as KeyBits = real_keyval(scCtrl)
	dim shift as KeyBits = real_keyval(scShift)
	dim ctrlshift as Keybits = ctrl or shift

	'Check to see if the backend has received a request
	'to close the window (eg. clicking the window frame's X).
	'This form of input isn't recorded, but the ESCs fired in Custom will be recorded,
	'so there's no need to check the recorded key state for pageup+pagedown+esc
	if real_keyval(scPageup) > 0 andalso real_keyval(scPagedown) > 0 andalso real_keyval(scEsc) > 1 then
		closerequest = YES
	end if

#ifdef IS_CUSTOM
	'Fire ESC keypresses to exit every menu
	if closerequest then
		if replay.active or replay.paused then
			stop_replaying_input "Replay ended by quit request"
		end if
		real_input.kb.keys(scEsc) = 7
	end if
#elseif defined(IS_GAME)
	'Quick abort (could probably do better, just moving this here for now)
	if closerequest then
		exit_gracefully()
	end if
#endif

	if shift > 0 andalso real_keyval(scTab) > 0 then
		' Crash the program! For testing
		if real_keyval(scF3) > 1 then
			*cast(integer ptr, &hff) = 42
		end if

		' A breakpoint. If not running under gdb, this will terminate the program
		if real_keyval(scF4) > 1 then
			interrupt_self ()
		end if

		if real_keyval(scF5) > 1 then
			fatalerror "User hit Tab-Shift-F5"
		end if

		if real_keyval(scF6) > 1 then
			dim x as integer ptr
			*x = 42  'In -exx builds, FB throws an error rather than SIGSEGV
		end if
	end if

	if ctrlshift > 0 andalso (real_keyval(scF7) and 4) then
		gfx_backend_menu
	end if

	'Ctrl-Shift-N: toggle numpad remapping
	if ctrl > 0 andalso shift > 0 andalso (real_keyval(scN) and 4) then
		remap_numpad xor= YES
		show_overlay_message "Numpad remapping " & onoroff(remap_numpad xor YES)
	end if

	'Ctrl/Shift-F8: Open debug log
	if ctrlshift > 0 andalso (real_keyval(scF8) and 4) then
		open_document log_dir & *app_log_filename
	end if

	' F12 screenshots are handled in setvispage, not here.

	' Ctrl/Shift+F12 to start/stop recording a .gif
	if ctrlshift > 0 andalso (real_keyval(scF12) and 4) then
		toggle_recording_gif
	end if

	if ctrl > 0 andalso real_keyval(scTilde) and 4 then
		toggle_fps_display
	end if

	fps_multiplier = base_fps_multiplier
	if shift > 0 and real_keyval(scTab) > 0 then  'speed up while held down
		fps_multiplier *= 6.
	end if

	if replay.active then replay_controls()

	if ctrlshift > 0 and real_keyval(scF11) > 1 then
		real_clearkey(scF11)
		macro_controls()
	end if

	'This is a pause that doesn't show up in recorded input
	if (replay.active or record.active) andalso real_keyval(scPause) > 1 then
		real_clearkey(scPause)
		pause_replaying_input
		pause_recording_input
		notification "Replaying/recording is PAUSED"
		resume_replaying_input
		resume_recording_input
	end if

	'Some debug keys for working on resolution independence
	if keyval(scShift) > 0 and keyval(sc1) > 0 then
		if keyval(scRightBrace) > 1 then
			set_resolution windowsize.w + 10, windowsize.h + 10
		end if
		if keyval(scLeftBrace) > 1 then
			set_resolution windowsize.w - 10, windowsize.h - 10
		end if
		if keyval(scR) > 1 then
			'Note: there's also an option in the F8 menu in-game.
			resizing_enabled = gfx_set_resizable(resizing_enabled xor YES, minwinsize.w, minwinsize.h)
		end if
	end if

	'gfx_directx handles ScollLock to pause mouse grab itself
	if mouse_grab_requested andalso mouse_grab_nested_pauses <= 0 andalso gfxbackend <> "directx" then
#IFDEF __FB_DARWIN__
		if keyval(scF14) > 1 then
			clearkey(scF14)
#ELSE
		if keyval(scScrollLock) > 1 then
			clearkey(scScrollLock)
#ENDIF
			pause_mouserect
			mouse_grab_scrolllock_overridden = YES
		end if
	end if
end sub

'Show the menu that comes up when pressing ESC while replaying
local sub replay_menu ()
	dim menu(...) as string = {"Resume Replay", "End Replay"}
	dim choice as integer
	pause_replaying_input
	ensure_normal_palette
	dim previous_speed as double = base_fps_multiplier
	base_fps_multiplier = 1.
	choice = multichoice("Stop replaying recorded input?", menu(), 0, 0)
	if choice = 0 then
		base_fps_multiplier = previous_speed
                resume_replaying_input
	elseif choice = 1 then
		stop_replaying_input "Playback cancelled."
	end if
	restore_previous_palette
end sub

'Controls available while replaying input.
'Called from inside setkeys; but it's OK to call setkeys from here if
'pause_replaying_input is called first. If FB had co-routines, this would be implemented as one.
local sub replay_controls ()
	'We call show_help which calls setkeys which calls us.
	static reentering as bool = NO
	BUG_IF(reentering, "Reentry shouldn't happen")
	reentering = YES

	if real_keyval(scF1) > 1 then
		dim remem as bool = overlay_replay_display
		pause_replaying_input()
		hide_overlays()
		base_fps_multiplier = 1.
		show_help("share_replay")
		setkeys
		overlay_replay_display = remem
		resume_replaying_input()
	end if
	if real_keyval(scSpace) > 1 then
		overlay_replay_display xor= YES
	end if
	if real_keyval(ccCancel) > 1 then
		replay_menu
	end if
	'Also scPause, handled in setkeys because it affects record too.

	if real_keyval(ccLeft) > 1 then
		base_fps_multiplier *= 0.5
		show_replay_overlay()
	end if
	if real_keyval(ccRight) > 1 then
		base_fps_multiplier *= 2
		show_replay_overlay()
	end if
	base_fps_multiplier = bound(base_fps_multiplier, 0.5^3, 2.^9)

	reentering = NO
end sub

' Menu of options for playback/recording of macros
local sub macro_menu ()
	pause_replaying_input
	pause_recording_input
	ensure_normal_palette
	dim holdscreen as integer = allocatepage
	copypage vpage, holdscreen

	dim choice as integer = 3  'Default to playback
	do
		'browse() and inputfilename() clobber vpage
		copypage holdscreen, vpage
		fuzzyrect 0, 0, , , uilook(uiBackground), vpage, 40

		redim menu(2) as string
		menu(0) = "Cancel"
		menu(1) = "Load macro from file"
		menu(2) = "Start recording macro"
		if isfile(macrofile) then
			redim preserve menu(5)
			menu(3) = "Play back last recorded macro"
			menu(4) = "Play back last recorded macro # times"
			menu(5) = "Save last recorded macro to file"
		end if

		dim msg as string
		msg = !"Macro Recording & Replay\n(See F1 help file for information.)"
		if ubound(menu) < 3 then
			msg += !"\nNo macro recorded yet."
		end if
		choice = multichoice(msg, menu(), choice, 0, "share_macro_menu")
		if choice = 1 then
			dim macfile as string
			macfile = browse(browseAny, "", "*.ohrkeys")
			if len(macfile) then
				if not copyfile(macfile, macrofile) THEN
					showerror "Couldn't make a copy of " & macfile
				end if
			end if
			continue do
		elseif choice = 2 then
			show_overlay_message "Recording macro, Shift/Ctrl+F11 to stop", 2.
			start_recording_input macrofile
		elseif choice = 3 then
			show_overlay_message "Replaying macro"
			start_replaying_input macrofile
		elseif choice = 4 then
			dim repeats as string
			prompt_for_string repeats, "Number of macro repetitions?"
			dim repeat_count as integer = str2int(repeats, -1)
			if repeat_count <= 0 then
				exit sub
			end if
			show_overlay_message "Replaying macro " & replay.repeat_count & " time(s)"
			start_replaying_input macrofile, repeat_count
		elseif choice = 5 then
			dim macfile as string
			macfile = inputfilename("Input a filename to save to", ".ohrkeys", "", "")
			'setkeys
			if len(macfile) then
				if not copyfile(macrofile, macfile + ".ohrkeys") THEN
					showerror "Couldn't write to " & macfile & ".ohrkeys"
				end if
			end if
			continue do
		end if
		exit do
	loop

	copypage holdscreen, vpage
	freepage holdscreen
	restore_previous_palette
	resume_replaying_input
	resume_recording_input
end sub

'Handles Ctrl+F11 key for macro recording and replay.
'Called from inside setkeys, but it's OK to call setkeys from here as we disallow reentry.
'This can also be called from the in-game debug menu.
sub macro_controls ()
	static reentering as bool = NO
	if reentering then exit sub
	reentering = YES
	if record.active then
		stop_recording_input "Recorded macro, Shift/Ctrl+F11 to play", errInfo
	elseif replay.active then
		show_overlay_message "Ended macro playback early", 2.
		stop_replaying_input
	else
		macro_menu
	end if
	reentering = NO
end sub

'Display a message above everything else; by default doesn't appear in screenshots.
'Intended for use here in allmodex, but pragmaticlly, can be used in Custom too.
'Note that in-game, you should set gam.showtext/gam.showtext_ticks instead.
sub show_overlay_message (msg as string, seconds as double = 3.)
	overlay_message = msg
	overlay_hide_time = timer + seconds
	overlay_replay_display = NO
end sub

function overlay_message_visible () as bool
	return len(overlay_message) > 0 and overlay_hide_time > timer
end function

'Show the overlay for replaying input
local sub show_replay_overlay ()
	overlay_replay_display = YES
end sub

local sub hide_overlays ()
	overlay_message = ""
	overlay_replay_display = NO
end sub

local function ms_to_string (ms as integer) as string
	return seconds2str(cint(ms * 0.001), "%h:%M:%S")
end function

sub toggle_fps_display ()
	overlay_showfps = (overlay_showfps + 1) MOD 3
end sub

' Called every time a frame is drawn.
' skipped: true if this frame was frameskipped.
local sub update_fps_counter (skipped as bool)
	fps_draw_frames += 1
	if not skipped then
		fps_real_frames += 1
	end if
	dim nowtime as double = timer
	if nowtime > fps_time_start + 1 then
		draw_fps = fps_draw_frames / (nowtime - fps_time_start)
		real_fps = fps_real_frames / (nowtime - fps_time_start)
		fps_time_start = nowtime
		fps_draw_frames = 0
		fps_real_frames = 0
	end if
end sub

'Draw crosshairs at the mouse position plus left/right buttons; used by --showmouse cmdline option
sub draw_basic_mouse_cursor (page as integer)
	with mouse_state
		dim col as integer = uilook(uiSelectedItem + global_tog)
		rectangle .x - 4, .y, 9, 1, col, page
		rectangle .x, .y - 4, 1, 9, col, page
		if .buttons and mouseLeft then
			rectangle .x - 3, .y - 3, 3, 3, col, page
		end if
		if .buttons and mouseRight then
			rectangle .x + 1, .y - 3, 3, 3, col, page
		end if
	end with
end sub

'Draw stuff on top of the video page about to be shown; specially those things
'that are included in .gifs/screenshots even without --recordoverlays
'Returns true if something was drawn.
local function draw_allmodex_recordable_overlays (page as integer) as bool
	dim dirty as bool = NO

	if show_mouse_overlay then
		draw_basic_mouse_cursor page
		dirty = YES
	end if

	if gif_show_keys_overlay andalso recordvid andalso recordvid->active then
		' Build up two strings describing keypresses, so that modifiers like LShift
		' are sorted to the front.
		dim as string modifiers, keys
		static recent_key_cooldowns(ubound(real_input.kb.keys)) as integer 'cooldown ticks
		dim inputst as InputState ptr = iif(replay.active, @replay_input, @real_input)
		with inputst->kb
			for idx as KBScancode = 0 to ubound(.keys)
				'If the key was down for only a short time, wait a little
				'before the display of the key disappears
				const DISPLAY_MS = 400
				dim byref cooldown as integer = recent_key_cooldowns(idx)
				if .keys(idx) > 0 then
					cooldown = DISPLAY_MS - .key_down_ms(idx)  'Total display time DISPLAY_MS
				else
					cooldown -= inputst->elapsed_ms
				end if
				if cooldown < 0 then cooldown = 0
				if cooldown = 0 andalso .keys(idx) = 0 then continue for

				'TODO: Would be nice to show "Left" instead of "Numpad 4" if
				'numlock is off and that's what it's acting as.
				dim keyname as string = scancodename(idx)
				if idx <> scLeft andalso idx <> scRight then
					replacestr keyname, "Left", "L"  'Shorten the name of modifiers
					replacestr keyname, "Right", "R"
					replacestr keyname, " ", ""
				end if
				if .keys(idx) = 0 then
					'In cooldown period, show darker text because key isn't down
					keyname = fgcol_text(keyname, uilook(uiMenuItem))
				end if

				select case idx
				case scLeftShift, scRightShift, scLeftAlt, scRightAlt, scLeftCtrl, scRightCtrl
					modifiers &= " " & keyname
				case scShift, scAlt, scUnfilteredAlt, scCtrl, scAnyEnter
					'Ignore these duplicates
				case scNumLock, scCapsLock, scScrollLock
					'May appear pressed continuously
				case else
					keys &= " " & keyname
				end select
			next idx
		end with
		dim keysmsg as string = trim(modifiers & keys)
		if len(keysmsg) then
			rectangle pRight, pTop, textwidth(keysmsg) + 2, 10, uilook(uiBackground), page
			edgeprint keysmsg, pRight - 1, pTop, uilook(uiText), page, YES  'withtags=YES
			dirty = YES
		end if
	end if

	return dirty
end function

'Draw stuff on top of the video page about to be shown.
'Returns true if something was drawn.
local function draw_allmodex_overlays (page as integer) as bool
	if overlays_enabled = NO then return NO

	dim dirty as bool = NO

	if overlay_showfps then
		dim fpsstring as string
		if overlay_showfps = 2 then
			fpsstring = "Draw:" & format(draw_fps, "0.0") & " FPS"
		else
			fpsstring = "Display:" & format(real_fps, "0.0") & " FPS"
		end if
		' Move the FPS a little to the left, because on OSX+gfx_sdl the handle for resizable
		' windows is drawn in the bottom right corner by SDL (not the OS).
		edgeprint fpsstring, pRight - 14, iif(overlay_replay_display, pTop, pBottom), uilook(uiText), page
		dirty = YES
	end if

	if overlay_replay_display then
		overlay_hide_time = 0.  'Hides any other message
		dim repeat_str as string
		if replay.repeat_count > 1 then
			repeat_str = "#" & (1 + replay.repeats_done) & "/" & replay.repeat_count
		end if
		overlay_message = "Pos: " & ms_to_string(replay.play_position_ms) & "/" & ms_to_string(replay.length_ms) & _
		     "  " & rpad(replay.tick & "/" & replay.length_ticks, " ", 9) & repeat_str & _
		     !"\nSpeed: " & rpad(fps_multiplier & "x", " ", 5) & "FPS:" & format(draw_fps, "0.0") & " [F1 for help]"
	elseif overlay_hide_time < timer then
		overlay_message = ""
	end if

	if len(overlay_message) then
		basic_textbox overlay_message, uilook(uiText), page, rBottom + ancBottom - 2, , YES
		dirty = YES
	end if

	' For chasing Surface memory leaks
	'edgeprint gfx_debugSurfaces_SW() & " surfaces", pLeft, pBottom, uilook(uiText), page
	'dirty = YES

	return dirty
end function


'==========================================================================================
'                                  Recording and replay
'==========================================================================================


sub start_recording_input (filename as string)
	if replay.active or replay.paused then
		debug "Can't record input because already replaying input!"
		exit sub
	end if
	if isfile(filename) then
		debug "Replacing the input recording that already existed at """ & filename & """"
	end if
	record.constructor()  'Clear data
	if openfile(filename, for_binary + access_write, record.file) then
		stop_recording_input "Couldn't open " & filename
		record.file = -1
		exit sub
	end if
	dim header as string = "OHRRPGCEkeys"
	put #record.file,, header
	dim ohrkey_ver as integer = 4
	put #record.file,, ohrkey_ver
	dim seed as double = TIMER * 1e9
	reseed_prng seed
	put #record.file,, seed
	record.active = YES
	debuginfo "Recording keyboard input to: """ & filename & """"
end sub

sub stop_recording_input (msg as string="", errorlevel as ErrorLevelEnum = errError)
	if msg <> "" then
		debugc errorlevel, msg
		show_overlay_message msg
	end if
	if record.active or record.paused then
		close #record.file
		record.active = NO
		record.paused = NO
		debuginfo "STOP recording input"
	end if
end sub

' While recording is paused you can call setkeys without updating the recorded state.
' The keyboard state before pausing is restored when resuming, so it's safe to pause
' and resume recording anywhere.
sub pause_recording_input
	if record.active then
		record.active = NO
		record.paused = YES
		record.last_kb = real_input.kb
	end if
end sub

sub resume_recording_input
	if record.paused then
		record.active = YES
		record.paused = NO
		real_input.kb = record.last_kb
	end if
end sub

' Start replaying again from the beginning, used for loop
sub restart_replaying_input ()
	replay.tick = -1
	replay.nexttick = -1
	replay.play_position_ms = 0
	seek replay.file, 1
	load_replay_header()
end sub

sub start_replaying_input (filename as string, num_repeats as integer = 1)
	if record.active or record.paused then
		debug "Can't replay input because already recording input!"
		exit sub
	end if
	replay.constructor()     'Reset  (TODO: does this leak the filename string?)
	replay.filename = filename
	replay_input.kb.reset()
	if openfile(filename, for_binary + access_read, replay.file) then
		stop_replaying_input "Couldn't open " & filename
		replay.file = -1
		exit sub
	end if
	replay.active = YES
	replay.repeat_count = num_repeats
	load_replay_header()
end sub

sub load_replay_header ()
	dim header as string = STRING(12, 0)
	GET #replay.file,, header
	if header <> "OHRRPGCEkeys" then
		stop_replaying_input "No OHRRPGCEkeys header in """ & replay.filename & """"
		exit sub
	end if
	dim ohrkey_ver as integer = -1
	GET #replay.file,, ohrkey_ver
	if ohrkey_ver <> 4 then
		stop_replaying_input "Unknown ohrkey version code " & ohrkey_ver & " in """ & replay.filename & """. Only know how to understand version 4"
		exit sub
	end if
	dim seed as double
	GET #replay.file,, seed
	reseed_prng seed
	debuginfo "Replaying keyboard input from: """ & replay.filename & """"
	read_replay_length()
	if replay.repeats_done = 0 then
		show_replay_overlay()
	end if
end sub

sub stop_replaying_input (msg as string="", errorlevel as ErrorLevelEnum = errError)
	if msg <> "" then
		debugc errorlevel, msg
		show_overlay_message msg
	end if
	if replay.active or replay.paused then
		close #replay.file
		replay.file = -1
		replay.active = NO
		replay.paused = NO
		debugc errorlevel, "STOP replaying input"
		use_speed_control = YES
	end if
	' Cancel any speedup
	base_fps_multiplier = 1.
end sub

' While replay is paused you can call setkeys without changing the replay state,
' and keyval, etc, return the real state of the keyboard.
' (Safe to try pausing/resuming when not replaying)
sub pause_replaying_input
        ' The replay state is preserved in replay_input.kb, so pausing and resuming is easy.
	if replay.active then
		replay.active = NO
		replay.paused = YES
	end if
end sub

sub resume_replaying_input
	if replay.paused then
		replay.active = YES
		replay.paused = NO
	end if
end sub

sub record_input_tick ()
	record.tick += 1
	dim presses as ubyte = 0
	dim keys_down as integer = 0
	for i as KBScancode = 0 to scLAST
		if real_input.kb.keys(i) <> record.last_kb.keys(i) then
			presses += 1
		end if
		if real_input.kb.keys(i) then keys_down += 1  'must record elapsed_ms
	next i
	if presses = 0 andalso keys_down = 0 andalso len(real_input.kb.inputtext) = 0 then exit sub

	dim debugstr as string
	if record.debug then debugstr = "L:" & (SEEK(record.file) - 1) & " T:" & record.tick & " ms:" & real_input.elapsed_ms & " ("

	put #record.file,, record.tick
	put #record.file,, cubyte(real_input.elapsed_ms)
	put #record.file,, presses

	for i as ubyte = 0 to scLAST
		if real_input.kb.keys(i) <> record.last_kb.keys(i) then
			PUT #record.file,, i
			PUT #record.file,, cubyte(real_input.kb.keys(i))
			if record.debug then debugstr &= " " & scancodename(i, YES) & "=" & real_input.kb.keys(i)
		end if
	next i
	'Currently inputtext is Latin-1, format will need changing in future
	put #record.file,, cubyte(len(real_input.kb.inputtext))
	put #record.file,, real_input.kb.inputtext
	if record.debug then
		debugstr &= " )"
		if len(real_input.kb.inputtext) then debugstr &= " input: '" & real_input.kb.inputtext & "'"
		debuginfo debugstr
	end if
	record.last_kb = real_input.kb
end sub

' Scan the replay file to find its length, setting replay.length_ms and replay.length_ticks
' Assumes replay.file is at start of the data stream.
local sub read_replay_length ()
	dim as integer tick, nexttick
	dim as ubyte tick_ms = 55, presses, input_len
	dim initial_pos as integer = seek(replay.file)
	replay.length_ms = 0

	do
		get #replay.file,, nexttick
		if eof(replay.file) then exit do
		if nexttick < tick then
			visible_debug "Replay corrupt: tick " & replay.nexttick & " occurs after " & tick
			exit do
		end if

		' Assume any skipped ticks are the same length as the next one, seems to give a vastly better
		' estimate than using the previous tick.
		' (This could be way off, some ticks are 0ms or 255+ms)
		get #replay.file,, tick_ms
		replay.length_ms += tick_ms * (nexttick - tick)
		' if (nexttick - tick) > 1 and (tick_ms < 50 or tick_ms > 60) then
		' 	debug "dubious tick_ms estimate " & tick_ms & " at " & tick & " for " & (nexttick - tick) & " ticks"
		' end if

		tick = nexttick
		get #replay.file,, presses
		if presses > scLAST + 1 then
			visible_debug "Replay corrupt: presses=" & presses
			exit do
		end if

		seek #replay.file, seek(replay.file) + 2 * presses
		GET #replay.file,, input_len
		if input_len then
			seek #replay.file, seek(replay.file) + input_len
		end if
	loop
	replay.length_ticks = tick
	seek #replay.file, initial_pos
end sub

sub replay_input_tick ()
	replay.tick += 1
	do
		if EOF(replay.file) then
			replay.repeats_done += 1
			'show_overlay_message "Finished replay " & replay.repeats_done & " of " & replay.repeat_count

			if replay.repeats_done >= replay.repeat_count then
				stop_replaying_input "The end of the playback file was reached.", errInfo
				exit sub
			else
				restart_replaying_input
			end if
		end if

		'Check whether it's time to play the next recorded tick in the replay file
		'(ticks on which nothing happened aren't saved)
		if replay.nexttick = -1 then
			replay.fpos = seek(replay.file) - 1
			GET #replay.file,, replay.nexttick
			' Grab the next tick_ms already, because for some reason it gives far more accurate .play_position_ms estimation
			dim tick_ms as ubyte
			GET #replay.file,, tick_ms
			replay.next_tick_ms = tick_ms
		end if
		if replay.nexttick < replay.tick then
			debug "input replay late for tick " & replay.nexttick & " (" & replay.nexttick - replay.tick & ")"
		elseif replay.nexttick > replay.tick then
			'debug "saving replay input tick " & replay.nexttick & " until its time has come (+" & replay.nexttick - replay.tick & ")"
			for i as KBScancode = 0 to scLAST
				'Check for a corrupt file
				if replay_input.kb.keys(i) then
					' There ought to be a tick in the input file so that we can set setkeys_elapsed_ms correctly
					debug "bad recorded key input: key " & i & " is down, but expected tick " & replay.tick & " is missing"
					exit for
				end if
			next
			' Otherwise, this doesn't matter as it won't be used
			replay_input.elapsed_ms = 1
			' Increment how much we've played so far - not actual play time but at same rate as the .length_ms estimate
			replay.play_position_ms += replay.next_tick_ms
			replay_input.kb.inputtext = ""
			exit sub
		end if

		replay_input.elapsed_ms = replay.next_tick_ms
		replay.play_position_ms += replay.next_tick_ms

		dim presses as ubyte
		GET #replay.file,, presses
		if presses > scLAST + 1 then
			stop_replaying_input "input replay tick " & replay.nexttick & " has invalid number of keypresses " & presses
			exit sub
		end if

		dim as string info
		if replay.debug then
			info = "L:" & replay.fpos & " T:" & replay.nexttick & " ms:" & replay_input.elapsed_ms & " ("
		end if

		dim key as ubyte
		dim keybits as ubyte
		for i as integer = 1 to presses
			GET #replay.file,, key
			GET #replay.file,, keybits
			replay_input.kb.keys(key) = keybits
			if replay.debug then info &= " " & scancodename(key) & "=" & keybits
		next i
		if replay.debug then info &= " )"
		dim input_len as ubyte
		GET #replay.file,, input_len
		if input_len then
			'Currently inputtext is Latin-1, format will need changing in future
			replay_input.kb.inputtext = space(input_len)
			GET #replay.file,, replay_input.kb.inputtext
			if replay.debug then info &= " input: '" & replay_input.kb.inputtext & "'"
		else
			replay_input.kb.inputtext = ""
		end if

		if replay.debug then debuginfo info

		'In case the replay somehow became out of sync, keep looping until we catch up
		'(Probably hopeless though)
		if replay.nexttick = replay.tick then
			replay.nexttick = -1
			exit sub
		end if
		replay.nexttick = -1
	loop
end sub


'==========================================================================================
'                                      Map rendering
'==========================================================================================


function readblock (map as TileMap, x as integer, y as integer, default as integer = 112343211) as integer
	if x < 0 orelse x >= map.wide orelse y < 0 orelse y >= map.high then
		if default <> 112343211 then return default
		onetime_debug errShowBug, "illegal readblock call " & x & " " & y
		exit function
	end if
	return map.data[x + y * map.wide]
end function

sub writeblock (map as TileMap, x as integer, y as integer, v as integer)
	if x < 0 orelse x >= map.wide orelse y < 0 orelse y >= map.high then
		onetime_debug errShowBug, "illegal writeblock call " & x & " " & y & " " & v
		exit sub
	end if
	map.data[x + y * map.wide] = v
end sub

'Calculate which tile to display
local function calcblock (tmap as TileMap, x as integer, y as integer, overheadmode as integer, pmapptr as TileMap ptr) as integer
'returns -1 to draw no tile
'overheadmode = 0 : ignore overhead tile bit; draw normally;
'overheadmode = 1 : draw non overhead tiles only (to avoid double draw)
'overheadmode = 2 : draw overhead tiles only
	dim block as integer

	'check bounds
	if bordertile = -1 then
		'wrap
		while y < 0
			y = y + tmap.high
		wend
		while y >= tmap.high
			y = y - tmap.high
		wend
		while x < 0
			x = x + tmap.wide
		wend
		while x >= tmap.wide
			x = x - tmap.wide
		wend
	else
		if (y < 0) or (y >= tmap.high) or (x < 0) or (x >= tmap.wide) then
			if tmap.layernum = 0 and overheadmode <= 1 then
				'only draw the border tile once!
				return bordertile
			else
				return -1
			end if
		end if
	end if

	block = readblock(tmap, x, y)

	if block = 0 and tmap.layernum > 0 then  'This could be an argument, maybe we could get rid of layernum
		return -1
	end if

	if overheadmode > 0 then
		if pmapptr = NULL then
			showbug "calcblock: overheadmode but passmap ptr is NULL"
			block = -1
		elseif x >= pmapptr->wide or y >= pmapptr->high then
			'Impossible if the passmap is the same size
			if overheadmode = 2 then block = -1
		elseif ((readblock(*pmapptr, x, y) and passOverhead) <> 0) xor (overheadmode = 2) then
			block = -1
		end if
	end if

	return block
end function

'Given a tile number, possibly animated, translate it to the static tile to display
function translate_animated_tile(todraw as integer) as integer
	if todraw >= 208 then
		return (todraw - 48 + anim2) mod 160
	elseif todraw >= 160 then
		return (todraw + anim1) mod 160
	else
		return todraw
	end if
end function

sub drawmap (tmap as TileMap, x as integer, y as integer, tileset as TilesetData ptr, p as integer, trans as bool = NO, overheadmode as integer = 0, pmapptr as TileMap ptr = NULL, ystart as integer = 0, yheight as integer = -1, pal as Palette16 ptr = NULL, opts as DrawOptions = def_drawoptions)
	setanim tileset
	drawmap tmap, x, y, tileset->spr, p, trans, overheadmode, pmapptr, ystart, yheight, , pal, opts
end sub

sub drawmap (tmap as TileMap, x as integer, y as integer, tilesetsprite as Frame ptr, p as integer, trans as bool = NO, overheadmode as integer = 0, pmapptr as TileMap ptr = NULL, ystart as integer = 0, yheight as integer = -1, largetileset as bool = NO, pal as Palette16 ptr = NULL, opts as DrawOptions = def_drawoptions)
'Draw a single map layer; see overload below for args.
'ystart is the distance from the top to start drawing, yheight the number of lines. yheight=-1 indicates extend to bottom of screen
'There are no options in the X direction because they've never been used, and I don't forsee them being (can use Frames or slices instead)
	dim mapview as Frame ptr
	'Drawing onto mapview will reset the cliprect, so save it. Also we need to shift the cliprect by ystart.
	'TODO: it would be more efficient to shrink mapview to the cliprect. Then we could also avoid having
	'to clip each individual tile.
	dim saveclip as ClipState = get_cliprect(vpages(p))
	mapview = frame_new_view(vpages(p), 0, ystart, vpages(p)->w, iif(yheight = -1, vpages(p)->h, yheight))
	setclip saveclip.l, saveclip.t - ystart, saveclip.r, saveclip.b - ystart, mapview
	drawmap tmap, x, y, tilesetsprite, mapview, trans, overheadmode, pmapptr, largetileset, pal, opts
	frame_unload @mapview
	get_cliprect() = saveclip
end sub

sub drawmap (tmap as TileMap, x as integer, y as integer, tilesetsprite as Frame ptr, dest as Frame ptr, trans as bool = NO, overheadmode as integer = 0, pmapptr as TileMap ptr = NULL, largetileset as bool = NO, pal as Palette16 ptr = NULL, opts as DrawOptions = def_drawoptions)
'Draw a single map layer.
'This version of drawmap paints over the entire dest Frame given to it.
'x and y are the camera position at the top left corner of the Frame, not
'the position at which the top left of the map is drawn: this is the OPPOSITE
'to all other drawing commands!
'trans : Whether color 0 is transparent; doesn't affect treatment of tile 0.
'overheadmode = 0 : draw all tiles normally
'overheadmode = 1 : draw non overhead tiles only (to avoid double draw)
'overheadmode = 2 : draw overhead tiles only
'largetileset : A hack which disables tile animation, instead using tilesets with 256 tiles
'opts : Note that DrawOptions.scale is not yet supported

	dim sptr as ubyte ptr
	dim plane as integer

	dim ypos as integer
	dim xpos as integer
	dim xstart as integer
	dim yoff as integer
	dim xoff as integer
	dim calc as integer
	dim ty as integer
	dim tx as integer
	dim todraw as integer
	dim tileframe as Frame

	get_cliprect(dest)  'Set clipping Frame

	'copied from the asm
	ypos = y \ 20
	calc = y mod 20
	if calc < 0 then	'adjust for negative coords
		calc = calc + 20
		ypos = ypos - 1
	end if
	yoff = -calc

	xpos = x \ 20
	calc = x mod 20
	if calc < 0 then
		calc = calc + 20
		xpos = xpos - 1
	end if
	xoff = -calc
	xstart = xpos

	tileframe.refcount = NOREFC
	tileframe.w = 20
	tileframe.h = 20
	tileframe.pitch = 20

	ty = yoff
	while ty < dest->h
		tx = xoff
		xpos = xstart
		while tx < dest->w
			todraw = calcblock(tmap, xpos, ypos, overheadmode, pmapptr)
			if largetileset = NO then
				todraw = translate_animated_tile(todraw)
			end if

			'get the tile
			if (todraw >= 0) then
				tileframe.image = tilesetsprite->image + todraw * 20 * 20
				if tilesetsprite->mask then 'just in case it happens some day
					tileframe.mask = tilesetsprite->mask + todraw * 20 * 20
				else
					tileframe.mask = NULL
				end if

				'draw it on the map
				frame_draw_internal(@tileframe, curmasterpal(), pal, tx, ty, trans, dest, opts)
			end if

			tx = tx + 20
			xpos = xpos + 1
		wend
		ty = ty + 20
		ypos = ypos + 1
	wend
end sub

'Set tile animation state for drawmap... yuck
sub setanim (cycle1 as integer, cycle2 as integer)
	anim1 = cycle1
	anim2 = cycle2
end sub

sub setanim (tileset as TilesetData ptr)
	anim1 = tileset->tastuf(0) +  tileset->anim(0).cycle
	anim2 = tileset->tastuf(20) + tileset->anim(1).cycle
end sub

'-2: draw nothing beyond the map edge
'-1: wrap map
'0+: draw this tile beyong the map edge (but only when drawing layer 0)
sub setoutside (defaulttile as integer)
	bordertile = defaulttile
end sub

' Draws all map layers at a single tile coordinate. Used for drawing the minimap.
' Respects setoutside. Changes the setanim (current tileset animation) state.
sub draw_layers_at_tile(composed_tile as Frame ptr, tiles as TileMap ptr vector, tilesets as TilesetData ptr vector, tx as integer, ty as integer, pmapptr as TileMap ptr = NULL)
	BUG_IF(v_len(tiles) <> v_len(tilesets), "mismatched vectors")
	for idx as integer = 0 to v_len(tiles) - 1
		'It's possible that layer <> idx if for example drawing a minimap of a single map layer
		dim layer as integer = tiles[idx]->layernum
		setanim tilesets[idx]
		with *tilesets[idx]
			dim todraw as integer = calcblock(*tiles[idx], tx, ty, 0, 0)
			if todraw < 0 then continue for
			todraw = translate_animated_tile(todraw)

			frame_draw .spr, , 0, -todraw * 20, (layer > 0), composed_tile

			'Note: readblock() must be called with a default for OOB reads, because
			'a number of ancient .rpgs have passmaps that are 2 rows shorter than the tilemap
			if layer = 0 andalso pmapptr andalso (readblock(*pmapptr, tx, ty, 0) and passOverhead) then
				' If an overhead tile, return just the layer 0 tile
				exit for
			end if
		end with
	next
end sub


'==========================================================================================
'                                     Old allmodex IO
'==========================================================================================

sub storemxs (fil as string, record as integer, fr as Frame ptr)
'saves a screen page to a file. Doesn't support non-320x200 pages
	dim f as integer
	dim as integer x, y
	dim sptr as ubyte ptr
	dim plane as integer

	CHECK_FRAME_8BIT(fr)

	if openfile(fil, for_binary + access_read_write, f) then exit sub

	'skip to index
	seek #f, (record*64000) + 1 'will this work with write access?

	'modex format, 4 planes
	for plane = 0 to 3
		for y = 0 to 199
			sptr = fr->image + fr->pitch * y + plane

			for x = 0 to (80 - 1) '1/4 of a row
				put #f, , *sptr
				sptr = sptr + 4
			next
		next
	next

	close #f
end sub

'For compatibility: load into an existing Frame.
'NOTE: Don't use this in new code. It bypasses the cache. Use frame_load
sub loadmxs (filen as string, record as integer, dest as Frame ptr)
	dim temp as Frame ptr
	temp = frame_load_mxs(filen, record)
	frame_clear dest
	if temp then
		frame_draw temp, , 0, 0, NO, dest
		frame_unload @temp
	end if
end sub

'Loads a 320x200 mode X format page from a file.
'This should probably only be called directly when loading from file outside an .rpg,
'otherwise use frame_load.
function frame_load_mxs (filen as string, record as integer) as Frame ptr
	dim starttime as double = timer
	dim fh as integer
	dim as integer x, y
	dim sptr as ubyte ptr
	dim plane as integer
	dim dest as Frame ptr

	'Return blank Frame on failure
	dest = frame_new(320, 200, , YES)

	if record < 0 then
		debugc errBug, "frame_load_mxs: attempted to read a negative record number " & record
		return dest
	end if
	if openfile(filen, for_binary + access_read, fh) then
		debugerror "frame_load_mxs: Couldn't open " & filen
		return dest
	end if

	if lof(fh) < (record + 1) * 64000 then
		debugerror "frame_load_mxs: wanted page " & record & "; " & filen & " is only " & lof(fh) & " bytes"
		lazyclose fh
		return dest
	end if

	'skip to index
	seek #fh, (record*64000) + 1

	dim quarter_row(79) as ubyte

	'modex format, 4 planes
	for plane = 0 to 3
		for y = 0 to 200 - 1
			sptr = dest->image + dest->pitch * y + plane

			'1/4 of a row
			get #fh, , quarter_row()
			for x = 0 to 80 - 1
				sptr[x * 4] = quarter_row(x)
			next
		next
	next

	lazyclose fh
	debug_if_slow(starttime, 0.1, filen)
	return dest
end function


'==========================================================================================
'                                   Graphics primitives
'==========================================================================================


'No clipping!!
'c is either an 8 bit or 32 bit RGBColor value
sub putpixel (spr as Frame ptr, x as integer, y as integer, c as integer)
	if x < 0 orelse x >= spr->w orelse y < 0 orelse y >= spr->h then
		exit sub
	end if
	if spr->image then
		FRAMEPIXEL(x, y, spr) = c
		exit sub
	end if
	ERROR_IF(spr->surf = NULL, "NULL image and surface")
	with *spr->surf
		ERROR_IF(.format <> SF_32bit, "surf isn't 32bit")
		cast(integer ptr, .pColorData)[.pitch * y + x] = c
	end with
end sub

sub putpixel (x as integer, y as integer, c as integer, p as integer)
	dim byref cliprect as ClipState = get_cliprect(vpages(p))
	if POINT_CLIPPED(x, y) then
		'debug "attempt to putpixel off-screen " & x & "," & y & "=" & c & " on page " & p
		exit sub
	end if

	putpixel vpages(p), x, y, c
end sub

'Returns either an 8 bit, or 32 bit RGBColor value
function readpixel (spr as Frame ptr, x as integer, y as integer) as integer
	if x < 0 orelse x >= spr->w orelse y < 0 orelse y >= spr->h then
		return -1
	end if

	if spr->image then
		return FRAMEPIXEL(x, y, spr)
	end if
	ERROR_IF(spr->surf = NULL, "NULL image and surface", -1)
	with *spr->surf
		ERROR_IF(.format <> SF_32bit, "surf isn't 32bit", -1)
		return cast(integer ptr, .pColorData)[.pitch * y + x]
	end with
end function

function readpixel (x as integer, y as integer, p as integer) as integer
	dim byref cliprect as ClipState = get_cliprect(vpages(p))

	if POINT_CLIPPED(x, y) then
		'debug "attempt to readpixel off-screen " & x & "," & y & " on page " & p
		return -1
	end if
	return readpixel(vpages(p), x, y)
end function

sub drawbox (x as RelPos, y as RelPos, w as RelPos, h as RelPos, col as integer, thickness as integer = 1, p as integer)
	drawbox vpages(p), x, y, w, h, col, thickness
end sub

'Draw a hollow box, with given edge thickness
'Increasing the thickness causes the lines to fatten towards the interior of the
'box; w/h is always the external size - meaning x+w and y+h are exclusive, while
'x and y are inclusive
sub drawbox (dest as Frame ptr, x as RelPos, y as RelPos, w as RelPos, h as RelPos, col as integer, thickness as integer = 1)
	w = relative_pos(w, dest->w)
	h = relative_pos(h, dest->h)

	if w < 0 then x = x + w + 1: w = -w
	if h < 0 then y = y + h + 1: h = -h

	if w = 0 or h = 0 then exit sub

	x = relative_pos(x, dest->w, w)
	y = relative_pos(y, dest->h, h)

	dim as integer thickx = small(thickness, w), thicky = small(thickness, h)

	rectangle dest, x, y, w, thicky, col
	IF h > thicky THEN
		rectangle dest, x, y + h - thicky, w, thicky, col
	end IF
	rectangle dest, x, y, thickx, h, col
	IF w > thickx THEN
		rectangle dest, x + w - thickx, y, thickx, h, col
	end IF
end sub

'Draw a box in perspective, its top 'offset' from its bottom; 12 lines in total
'Note, thickness doesn't affect the four angled lines
sub drawcube(dest as Frame ptr, rect as RectType, off as XYPair, col as integer, thickness as integer = 1)
	drawbox dest, rect.x, rect.y, rect.wide, rect.high, col, thickness
	dim shifted as RectType = rect + off
	drawbox dest, shifted.x, shifted.y, shifted.wide, shifted.high, col, thickness

	dim br as XYPair = rect.topleft + rect.size - 1  'bottom-right
	drawline dest, rect.x, rect.y, rect.x + off.x, rect.y + off.y, col
	drawline dest, rect.x, br.y,   rect.x + off.x, br.y + off.y,   col
	drawline dest, br.x,   rect.y, br.x + off.x,   rect.y + off.y, col
	drawline dest, br.x,   br.y,   br.x + off.x,   br.y + off.y,   col
end sub

' This function is slightly different from drawbox/rectangle, in that draws boxes with
' width/height 0 as width/height 1 instead of not at all.
' color is the main highlight color; if -1, use default
' FIXME: this function doesn't respect clipping!
sub drawants(dest as Frame ptr, x as RelPos, y as RelPos, wide as RelPos, high as RelPos, color as integer = -1)
	if color = -1 then color = uilook(uiText)
	dim as integer color2 = uilook(uiBackground)
	if dest->surf andalso dest->surf->format = SF_32bit then
		'putpixel takes BGRA, not master palette index
		color = curmasterpal(color).col
		color2 = curmasterpal(color2).col
	end if

	' Decode relative positions/sizes to absolute
	wide = relative_pos(wide, dest->w)
	high = relative_pos(high, dest->h)
	x = relative_pos(x, dest->w, wide)
	y = relative_pos(y, dest->h, high)

	if wide < 0 then x = x + wide + 1: wide = -wide
	if high < 0 then y = y + high + 1: high = -high

	'if wide <= 0 or high <= 0 then exit sub

	dim col as integer
	'--Draw verticals
	for idx as integer = 0 to large(high - 1, 0)
		select case (idx + x + y + tickcount) mod 3
			case 0: continue for
			case 1: col = color
			case 2: col = color2
		end select
		putpixel dest, x, y + idx, col
		if wide > 0 then
			putpixel dest, x + wide - 1, y + idx, col
		end if
	next idx
	'--Draw horizontals
	for idx as integer = 0 to large(wide - 1, 0)
		select case (idx + x + y + tickcount) mod 3
			case 0: continue for
			case 1: col = color
			case 2: col = color2
		end select
		putpixel dest, x + idx, y, col
		if high > 0 then
			putpixel dest, x + idx, y + high - 1, col
		end if
	next idx
end sub

'Ensure rect is contained within the cliprect and has non-negative width/height.
'Used by rectangle, trans_rectangle, fuzzyrect.
'x/y_start tells how many pixels the x/y location was moved by.
sub clip_rectangle_draw(dest as Frame ptr, byref rect as RelRectType, byref x_start as integer = 0, byref y_start as integer = 0)
	dim byref cliprect as ClipState = get_cliprect(dest)

	' Decode relative positions/sizes to absolute
	rect.wide = relative_pos(rect.wide, dest->w)
	rect.high = relative_pos(rect.high, dest->h)
	rect.x = relative_pos(rect.x, dest->w, rect.wide)
	rect.y = relative_pos(rect.y, dest->h, rect.high)

	if rect.wide < 0 then
		rect.x = rect.x + rect.wide + 1
		rect.wide = -rect.wide
	end if
	if rect.high < 0 then
		rect.y = rect.y + rect.high + 1
		rect.high = -rect.high
	end if

	'clip
	if rect.x + rect.wide > cliprect.r then rect.wide = (cliprect.r - rect.x) + 1
	if rect.y + rect.high > cliprect.b then rect.high = (cliprect.b - rect.y) + 1
	if rect.x < cliprect.l then
		x_start = cliprect.l - rect.x
		rect.wide -= x_start
		rect.x = cliprect.l
	end if
	if rect.y < cliprect.t then
		y_start = cliprect.t - rect.y
		rect.high -= y_start
		rect.y = cliprect.t
	end if
end sub

'Draw a transparent rectangle. 8- or 32-bit
'alpha is 0 for transparent, 1. for opaque
sub trans_rectangle(dest as Frame ptr, byval rect as RelRectType, byval col as RGBcolor, alpha as double)
	'gfx_surfaceFillAlpha and frame_new_view also clip to the Frame bounds,
	'but we need to clip by the cliprect anyway.
	clip_rectangle_draw dest, rect
	if rect.wide <= 0 orelse rect.high <= 0 then exit sub

	if dest->surf then
		BUG_IF(dest->surf->format <> SF_32bit, "8-bit Surface backed Frame not supported")
		dim srect as SurfaceRect = (rect.x, rect.y, rect.x + rect.wide - 1, rect.y + rect.high - 1)
		gfx_surfaceFillAlpha(col, alpha, @srect, dest->surf)
	else
		dim pal as Palette16 ptr = palette16_new_identity(256)
		Palette16_mix_n_match pal, col, alpha, mixBlend
		'Drawing onto the view Frame will reset the cliprect
		dim saveclip as ClipState = get_cliprect()
		'Draw a piece of the dest frame onto itself, effectively remapping by pal.
		dim viewfr as Frame ptr = frame_new_view(dest, rect.x, rect.y, rect.wide, rect.high)
		frame_draw viewfr, pal, 0, 0, NO, viewfr
		frame_unload @viewfr
		palette16_unload @pal
		get_cliprect() = saveclip
	end if
end sub

sub rectangle (x as RelPos, y as RelPos, w as RelPos, h as RelPos, c as integer, p as integer)
	rectangle vpages(p), x, y, w, h, c
end sub

sub rectangle (fr as Frame Ptr, x_ as RelPos, y_ as RelPos, w_ as RelPos, h_ as RelPos, c as integer)
	rectangle fr, XYWH(x_, y_, w_, h_), c
end sub

'Draw a solid rectangle (use drawbox for a hollow one)
'Top/left edges are inclusive, bottom/right are exclusive
sub rectangle (fr as Frame Ptr, byval rect as RelRectType, c as integer)
	clip_rectangle_draw fr, rect
	if rect.wide <= 0 orelse rect.high <= 0 then exit sub

	if fr->surf then
		dim srect as SurfaceRect = (rect.x, rect.y, rect.x + rect.wide - 1, rect.y + rect.high - 1)
		dim col as uint32 = c
		if fr->surf->format = SF_32bit then
			col = curmasterpal(c).col
		end if
		gfx_surfaceFill(col, @srect, fr->surf)
	else
		dim sptr as ubyte ptr = fr->image + (rect.y * fr->pitch) + rect.x
		while rect.high > 0
			memset(sptr, c, rect.wide)
			sptr += fr->pitch
			rect.high -= 1
		wend
	end if
end sub

sub fuzzyrect (x as RelPos, y as RelPos, w as RelPos = rWidth, h as RelPos = rHeight, c as integer, p as integer, fuzzfactor as integer = 50, stationary as bool = NO, zoom as integer = 1, offset as integer = 0)
	fuzzyrect vpages(p), XYWH(x, y, w, h), c, fuzzfactor, stationary, zoom, offset
end sub

'Draw a dithered rectangle
'Top/left edges are inclusive, bottom/right are exclusive
'stationary (originally "match_pattern"):
'  By default if you draw two fuzzy rectangles touching, the patterns
'  might not match up. Specify YES to force them to match up, but then
'  changing x,y will not make the pattern appear to shift.
'zoom:
'  Amount to scale up the pattern by (size of each pixel)
'offset:
'  Used for animating scrolling patterns by offseting them; non-negative
'  (see draw_background()).
sub fuzzyrect (fr as Frame Ptr, byval rect as RelRectType, c as integer, fuzzfactor as integer = 50, stationary as bool = NO, zoom as integer = 1, offset as integer = 0)
	'How many magic constants could you wish for?
	'These were half generated via magic formulas, and half hand picked (with magic criteria)
	static grain_table(50) as integer = {_
	                    50, 46, 42, 38, 38, 40, 41, 39, 26, 38, 30, 36, _
	                    42, 31, 39, 38, 41, 26, 27, 28, 40, 35, 35, 31, _
	                    39, 50, 41, 30, 29, 28, 45, 37, 24, 43, 23, 42, _
	                    21, 28, 11, 16, 20, 22, 18, 17, 19, 32, 17, 16, _
	                    15, 14, 50}

	fuzzfactor = bound(fuzzfactor, 1, 99)
	zoom = large(zoom, 1)

	dim as integer x_start = 0, y_start = 0
	clip_rectangle_draw fr, rect, x_start, y_start
	if rect.wide <= 0 orelse rect.high <= 0 then exit sub

	if stationary then
		x_start = rect.x
		y_start = rect.y
	end if
	x_start += offset
	y_start += offset

	dim grain as integer
	if fuzzfactor <= 50 then
		grain = grain_table(fuzzfactor)
	else
		grain = grain_table(100 - fuzzfactor)
	end if
	'if w = 99 then grain = h mod 100  'for hand picking

	'startr is the initial value of r, multiplied by zoom, at the top-left of the rect,
	'and the start of every line thereafter
	dim startr as integer = 0
	'These +1's are unneeded, they are only here to make the results identical to previous versions
	startr = (((x_start \ zoom) + 1) * fuzzfactor + ((y_start \ zoom) + 1) * grain) mod 100
	x_start = x_start mod zoom
	y_start = y_start mod zoom

	'Get image pointer
	dim sptr as ubyte ptr
	dim pitch as integer
	dim pixelbytes as integer = 1
	dim pixformat as SurfaceFormat
	if fr->image then
		sptr = fr->image
		pitch = fr->pitch
		pixformat = SF_8bit
	elseif fr->surf then
		sptr = fr->surf->pRawData
		pitch = fr->surf->pitch
		pixformat = fr->surf->format
		if pixformat = SF_32bit then
			pitch *= 4
			pixelbytes = 4
			c = curmasterpal(c).col
		end if
	else
		showbug "fuzzyrect: bad dest Frame"
		exit sub
	end if
	sptr += rect.y * pitch + rect.x * pixelbytes

	while rect.high > 0
		dim r as integer = startr
		if r < fuzzfactor then r += 100

		y_start += 1
		if y_start = zoom then
			startr = (startr + grain) mod 100
			y_start = 0
		end if

		'Whether we are currently drawing, or not
		dim drawpix as bool = r >= 100
		if drawpix then r -= 100

		'Number of times left to draw 'drawpix' on this row
		dim repeats as integer = zoom - x_start

		for i as integer = 0 to rect.wide - 1
			if drawpix then
				if pixformat = SF_8bit then
					sptr[i] = c
				else
					cast(int32 ptr, sptr)[i] = c
				end if
			end if
			if repeats = 1 then
				'Advance to next pixel
				repeats = zoom
				r += fuzzfactor
				drawpix = (r >= 100)
				if drawpix then r -= 100
			else
				repeats -= 1
			end if
		next
		rect.high -= 1
		sptr += pitch
	wend
end sub

'Draw a fuzzy rect over the whole clipping rect (normally, the whole screen) except for the given rectangle.
sub antifuzzyrect(fr as Frame Ptr, rect as RelRectType, col as integer, fuzzfactor as integer = 50, zoom as integer = 1)
	'Top 3 ninths
	fuzzyrect fr, XYWH(0, 0, 999999, rect.y), col, fuzzfactor, YES, zoom
	'Left ninth
	fuzzyrect fr, XYWH(0, rect.y, rect.x, rect.high), col, fuzzfactor, YES, zoom
	'Right ninth
	fuzzyrect fr, XYWH(rect.x + rect.wide, rect.y, 999999, rect.high), col, fuzzfactor, YES, zoom
	'Bottom 3 ninths
	fuzzyrect fr, XYWH(0, rect.y + rect.high, 999999, 999999), col, fuzzfactor, YES, zoom
end sub

'Draw either a rectangle or a scrolling chequer pattern.
'bgcolor is either between 0 and 255 (a colour), bgChequerScroll (a scrolling chequered
'background), or bgChequer (a non-scrolling chequered background)
'chequer_scroll is a counter variable which the calling function should increment once per tick.
'(If chequer_scroll isn't provided, than bgChequerScroll acts like bgChequer.)
'wide and high default to the whole dest Frame.
sub draw_background (dest as Frame ptr, bgcolor as bgType = bgChequerScroll, byref chequer_scroll as integer = 0, x as RelPos = 0, y as RelPos = 0, wide as RelPos = rWidth, high as RelPos = rHeight)
	const zoom = 3  'Chequer pattern zoom, fixed
	const rate = 4  'ticks per pixel scrolled, fixed
	'static chequer_scroll as integer
	'chequer_scroll = POSMOD(chequer_scroll, (zoom * rate * 6))

	wide = relative_pos(wide, dest->w)
	high = relative_pos(high, dest->h)
	x = relative_pos(x, dest->w, wide)
	y = relative_pos(y, dest->h, high)

	if bgcolor >= 0 then
		rectangle dest, x, y, wide, high, bgcolor
	else
		dim offset as integer = 0
		if bgcolor = bgChequerScroll then offset = chequer_scroll \ rate
		rectangle dest, XYWH(x, y, wide, high), uilook(uiBackground)
		fuzzyrect dest, XYWH(x, y, wide, high), uilook(uiDisabledItem), 25, , zoom, offset
	end if
end sub

sub drawline (x1 as integer, y1 as integer, x2 as integer, y2 as integer, c as integer, p as integer, dash_cycle as integer = 0, dash_len as integer = 0)
	drawline vpages(p), x1, y1, x2, y2, c, dash_cycle, dash_len
end sub

'dash_cycle:
'    If nonzero, draw dots/dashes. The cycle length is the number of
'    pixels from the start of one dash to the next one. Should be >= 2.
'dash_len:
'    Dash length in pixels. Shoudl be < dash_cycle.
sub drawline (dest as Frame ptr, x1 as integer, y1 as integer, x2 as integer, y2 as integer, c as integer, dash_cycle as integer = 0, dash_len as integer = 0)
	'Uses Bresenham's algorithm

	dim byref cliprect as ClipState = get_cliprect(dest)

	if y1 > y2 then
		'swap ends, we only draw downwards
		swap y1, y2
		swap x1, x2
	end if

	dim as integer stepX, stepY

	if x2 > x1 then
		stepX = 1
	elseif x2 < x1 then
		stepX = -1
	else
		stepX = 0
	end if

	if y2 > y1 then
		stepY = 1
	else
		stepY = 0
	end if

	'If the line is mostly-horizontal, then the 'major' direction
	'is X and the minor is Y.
	'All the deltas are fractions of a pixel scaled to integers
	'by multiplying by 2*deltaMAJOR

	dim as integer deltaX, deltaY

	deltax = abs(x2 - x1)
	deltay = y2 - y1  'is positive due to above swap

	dim as integer delta    'Accumulated fraction of a pixel error

	dim as integer delta_add, delta_sub
	dim as integer length, majorstep, minorstep

	if deltaX > deltaY then
		length = deltaX
		delta_add = 2*deltaY
		delta_sub = 2*deltaX
		minorstep = stepY * dest->pitch
		majorstep = stepX
	else
		length = deltaY
		delta_add = 2*deltaX
		delta_sub = 2*deltaY
		minorstep = stepX
		majorstep = stepY * dest->pitch
	end if
	delta = -delta_sub \ 2  'Start at the center of a pixel

	/'
	'Perform clipping (not correct/finished)
	dim itstart as integer
	if y1 < cliprect.t then
		if y2 < cliprect.t then exit sub  'Ensures delta_add & delta_sub > 0
		if deltaX > deltaY then
			delta += (cliprect.t - y1) * delta_add
			itstart = delta \ delta_add
			delta = delta mod delta_sub
		else
			itstart = cliprect.t - y1
			delta += itstart * delta_add
			x1 += stepX * (delta mod delta_sub)
			delta = delta mod delta_sub
			if delta > 0 then
				x1 += stepX
				'sptr += minorstep
				delta -= delta_sub
			end if
		end if
		y1 = cliprect.t
	end if
	'/

	dim sptr as ubyte ptr
	dim sptr32 as RGBcolor ptr
	dim is32bit as bool
	if dest->image then
		sptr = dest->image + (y1 * dest->pitch) + x1
		is32bit = NO
	elseif dest->surf then
		ERROR_IF(dest->surf->format <> SF_32bit, "surf not 32bit")
		ERROR_IF(dest->surf->pitch <> dest->pitch, "mismatched pitch")
		sptr32 = dest->surf->pColorData + (y1 * dest->surf->pitch) + x1
		sptr = cast(ubyte ptr, sptr32)
		minorstep *= 4
		majorstep *= 4
		c = curmasterpal(c).col
		is32bit = YES
	else
		showbug "drawline: bad Frame"
		exit sub
	end if

	dim dash_accum as integer

	for it as integer = 0 to length
		if POINT_CLIPPED(x1, y1) = NO then
			if dash_cycle = 0 then
				if is32bit then
					*cast(integer ptr, sptr) = c
				else
					*sptr = c
				end if
			else
				if dash_accum < dash_len then
					if is32bit then
						*cast(integer ptr, sptr) = c
					else
						*sptr = c
					end if
				end if
				dash_accum += 1
				if dash_accum = dash_cycle then dash_accum = 0
			end if
		end if
		delta += delta_add
		if delta > 0 then
			sptr += minorstep
			delta -= delta_sub
			if deltaX > deltaY then y1 += stepY else x1 += stepX
		end if
		sptr += majorstep
		if deltaX > deltaY then x1 += stepX else y1 += stepY
	next
end sub

sub paintat (dest as Frame ptr, x as integer, y as integer, c as integer)
'a floodfill.
	dim tcol as integer
	dim queue as XYPair_node ptr = null
	dim tail as XYPair_node ptr = null
	dim as integer w, e		'x coords west and east
	dim i as integer
	dim tnode as XYPair_node ptr = null

	CHECK_FRAME_8BIT(dest)

	dim byref cliprect as ClipState = get_cliprect(dest)
	if POINT_CLIPPED(x, y) then exit sub

	tcol = readpixel(dest, x, y)	'get target colour

	'prevent infinite loop if you fill with the same colour
	if tcol = c then exit sub

	queue = callocate(sizeof(XYPair_node))
	queue->x = x
	queue->y = y
	queue->nextnode = null
	tail = queue

	'we only let coordinates within the clip bounds get onto the queue, so there's no need to check them

	do
		if FRAMEPIXEL(queue->x, queue->y, dest) = tcol then
			FRAMEPIXEL(queue->x, queue->y, dest) = c
			w = queue->x
			e = queue->x
			'find western limit
			while w > cliprect.l and FRAMEPIXEL(w-1, queue->y, dest) = tcol
				w -= 1
				FRAMEPIXEL(w, queue->y, dest) = c
			wend
			'find eastern limit
			while e < cliprect.r and FRAMEPIXEL(e+1, queue->y, dest) = tcol
				e += 1
				FRAMEPIXEL(e, queue->y, dest) = c
			wend
			'add bordering XYPair_nodes
			for i = w to e
				if queue->y > cliprect.t then
					'north
					if FRAMEPIXEL(i, queue->y-1, dest) = tcol then
						tail->nextnode = callocate(sizeof(XYPair_node))
						tail = tail->nextnode
						tail->x = i
						tail->y = queue->y-1
						tail->nextnode = null
					end if
				end if
				if queue->y < cliprect.b then
					'south
					if FRAMEPIXEL(i, queue->y+1, dest) = tcol then
						tail->nextnode = callocate(sizeof(XYPair_node))
						tail = tail->nextnode
						tail->x = i
						tail->y = queue->y+1
						tail->nextnode = null
					end if
				end if
			next
		end if

		'advance queue pointer, and delete behind us
		tnode = queue
		queue = queue->nextnode
		deallocate(tnode)

	loop while queue <> null
	'should only exit when queue has caught up with tail

end sub

sub ellipse (fr as Frame ptr, x as double, y as double, radius as double, col as integer, fillcol as integer = -1, semiminor as double = 0.0, angle as double = 0.0)
'radius is the semimajor axis if the ellipse is not a circle
'angle is the angle of the semimajor axis to the x axis, in radians counter-clockwise

	if fr->surf andalso fr->surf->format = SF_32bit then
		'putpixel takes BGRA, not master palette index
		col = curmasterpal(col).col
		fillcol = curmasterpal(fillcol).col
	end if

	dim byref cliprect as ClipState = get_cliprect(fr)

	'x,y is the pixel to centre the ellipse at - that is, the centre of that pixel, so add half a pixel to
	'radius to put the perimeter halfway between two pixels
	x += 0.5
	y += 0.5
	radius += 0.5
	if semiminor = 0.0 then
		semiminor = radius
	else
		semiminor += 0.5
	end if

	dim as double ypart
	ypart = fmod(y, 1.0) - 0.5  'Here we add in the fact that we test for intercepts with a line offset 0.5 pixels

	dim as double sin_2, cos_2, sincos
	sin_2 = sin(-angle) ^ 2
	cos_2 = cos(-angle) ^ 2
	sincos = sin(-angle) * cos(-angle)

	'Coefficients of the general conic quadratic equation Ax^2 + Bxy + Cy^2 + Dx + Ey + F = 0  (D,E = 0)
	'Aprime, Cprime are of the unrotated version
	dim as double Aprime, Cprime
	Aprime = 1.0 / radius ^ 2
	Cprime = 1.0 / semiminor ^ 2

	dim as double A, B, C, F
	A = Aprime * cos_2 + Cprime * sin_2
	B = 2 * (Cprime - Aprime) * sincos
	C = Aprime * sin_2 + Cprime * cos_2
	F = -1.0

	dim as integer xstart = 999999999, xend = -999999999, lastxstart = 999999999, lastxend = -999999999, xs, yi, ys, maxr = large(radius, semiminor) + 1

	for yi = maxr to -maxr step -1
		'Note yi is cartesian coordinates, with the centre of the ellipsis at the origin, NOT screen coordinates!
		'xs, ys are in screen coordinates
		ys = int(y) - yi
		if ys < cliprect.t - 1 or ys > cliprect.b + 1 then continue for

		'Fix y (scanline) and solve for x using quadratic formula (coefficients:)
		dim as double qf_a, qf_b, qf_c
		qf_a = A
		qf_b = B * (yi + ypart)
		qf_c = C * (yi + ypart) ^ 2 + F

		dim as double discrim
		discrim = qf_b^2 - 4.0 * qf_a * qf_c
		if discrim >= 0.0 then
			discrim = sqr(discrim)

			'This algorithm is very sensitive to which way XXX.5 is rounded (normally towards even)...
			xstart = -int(-(x + (-qf_b - discrim) / (2.0 * qf_a) - 0.5))  'ceil(x-0.5), ie. round 0.5 down
			xend = int(x + (-qf_b + discrim) / (2.0 * qf_a) - 0.5)  'floor(x-0.5), ie. round 0.5 up, and subtract 1

			if xstart > xend then  'No pixel centres on this scanline lie inside the ellipse
				if lastxstart <> 999999999 then
					xend = xstart  'We've already started drawing, so must draw at least one pixel
				end if
			end if
		end if

		'Reconsider the previous scanline
		for xs = lastxstart to xstart - 1
			putpixel(fr, xs, ys - 1, col)
		next
		for xs = xend + 1 to lastxend
			putpixel(fr, xs, ys - 1, col)
		next

		dim canskip as bool = YES
		for xs = xstart to xend
			putpixel(fr, xs, ys, col)
			if canskip andalso xs >= lastxstart - 1 then
				'Draw the bare minimum number of pixels (some of these might be needed, but won't know until next scanline)
				dim jumpto as integer = small(xend - 1, lastxend)
				if fillcol <> -1 then
					for xs = xs + 1 to jumpto
						putpixel(fr, xs, ys, fillcol)
					next
				end if
				xs = jumpto
				canskip = NO  'Skipping more than once causes infinite loops
			end if
		next
		lastxstart = xstart
		lastxend = xend
		if discrim >= 0 then xend = xstart - 1  'To draw the last scanline, in the next loop
	next
end sub


'==========================================================================================
'                                  Palette manipulation
'==========================================================================================


'Replaces one colour with another, OR if swapcols is true, swaps the two colours.
sub replacecolor (fr as Frame ptr, c_old as integer, c_new as integer, swapcols as bool = NO)
	CHECK_FRAME_8BIT(fr)
	dim byref cliprect as ClipState = get_cliprect(fr)

	for yi as integer = cliprect.t to cliprect.b
		dim sptr as ubyte ptr = fr->image + (yi * fr->pitch)
		for xi as integer = cliprect.l to cliprect.r
			if sptr[xi] = c_old then
				sptr[xi] = c_new
			elseif swapcols and (sptr[xi] = c_new) then
				sptr[xi] = c_old
			end if
		next
	next
end sub

sub swapcolors(fr as Frame ptr, col1 as integer, col2 as integer)
	replacecolor fr, col1, col2, YES
end sub

'Changes a Frame in-place, applying a remapping
sub remap_to_palette (fr as Frame ptr, pal as Palette16 ptr)
	CHECK_FRAME_8BIT(fr)
	dim byref cliprect as ClipState = get_cliprect(fr)

	for y as integer = cliprect.t to cliprect.b
		for x as integer = cliprect.l to cliprect.r
			FRAMEPIXEL(x, y, fr) = pal->col(FRAMEPIXEL(x, y, fr))
		next
	next
end sub

sub remap_to_palette (fr as Frame ptr, palmapping() as integer)
	dim pal as Palette16 ptr = Palette16_new_from_indices(palmapping())
	remap_to_palette fr, pal
	Palette16_unload @pal
end sub

' Count the number of occurrences of a color in a Frame (just the clipped region)
function countcolor (fr as Frame ptr, col as integer) as integer
	CHECK_FRAME_8BIT(fr, 0)
	dim byref cliprect as ClipState = get_cliprect(fr)

	dim ret as integer = 0
	for yi as integer = cliprect.t to cliprect.b
		for xi as integer = cliprect.l to cliprect.r
			if FRAMEPIXEL(xi, yi, fr) = col then ret += 1
		next
	next
	return ret
end function

'Loads into pal() a 256-color palette from a 16x16 image (normally 24/32-bit)
'so, pixel (0,0) holds colour 0, (0,1) has colour 16, and (15,15) has colour 255
sub palette_from_16x16_image (filename as string, pal() as RGBcolor)
	dim surf as Surface ptr
	surf = image_import_as_surface(filename, YES)
	if surf = 0 then exit sub

	if surf->width <> 16 or surf->height <> 16 then
		showerror "Can't load palette from " & filename & ": not 16x16"
	else
		dim idx as integer
		for y as integer = 0 to 15
			for x as integer = 0 to 15
				pal(idx) = surf->pColorData[x + y * surf->pitch]
				idx += 1
			next
		next
	end if

	gfx_surfaceDestroy(@surf)
end sub


'==========================================================================================
'                                      Text routines
'==========================================================================================


#define TEXTDBG(message) if state.debug then ? message

function get_font(fontnum as integer, show_err as bool = NO) as Font ptr
	if fontnum < 0 orelse fontnum > ubound(fonts) orelse fonts(fontnum) = null then
		if show_err then
			showbug "invalid font num " & fontnum
		end if
		return fonts(0)
	else
		return fonts(fontnum)
	end if
end function

'Parses a code like ${Foo123} into action (eg 'FOO') and arg (eg 123) and find closing }.
'Pass a string, a 0-based offset of the start of the contents (eg. after "${"),
'and action and arg pointer, to fill with the parse results. (Action in UPPERCASE)
'Returns 0 for an invalidly formed tag, otherwise the (0-based) offset of the closing }.
function parse_tag(z as string, offset as integer, byref action as string, arg as int32 ptr) as integer
	dim closebrace as integer = instr((offset + 2) + 1, z, "}") - 1
	if closebrace <> -1 then
		z[closebrace] = 0
		dim ret as bool = split_str_int(@z[offset], action, *arg)
		z[closebrace] = asc("}")
		action = ucase(action)
		if ret then return closebrace
	end if
	return 0
end function

'Used for iterating over the valid text markup tags (e.g. "${K4}") in a string.
'Considers markup valid only when render_text would (ignores ones with invalid parameters and embed codes).
'offset is input/output, tagend is output.
'On the first call, pass in offset = 1 (it's a 1-based position).
'If there is another tag at or after offset, returns true and sets offset to the
'start of the tag and tagend to one past end (1-based).
'Returns false when no more, and doesn't set offset/tagend.
function next_text_markup(text as string, byref offset as integer, byref tagend as integer) as bool
	while offset <= len(text) - 3
		if text[offset - 1] = asc("$") andalso text[offset] = asc("{") then
			dim action as string
			dim intarg as int32
			tagend = parse_tag(text, (offset - 1) + 2, action, @intarg)
			debug "at " & offset & " tagend " & (tagend + 1)
			if tagend = 0 then return NO
			tagend += 2  'Convert from 0- to 1-based index, and move 1 past closing }
			dim ok as bool = NO

			if action = "F" then  'Font
				'Assume won't have null ptrs in font()
				ok = (intarg >= -1 andalso intarg <= ubound(fonts))
			elseif action = "K" then  'Foreground colour
				ok = (intarg <= 255)
			elseif action = "KB" then  'Background colour
				ok = (intarg <= 255)
			elseif action = "KP" then  'Font palette
				ok = (intarg >= 0 andalso intarg <= gen(genMaxPal))
			elseif action = "LM" then
				ok = YES
			elseif action = "RM" then
				ok = YES
			end if

			if ok then return YES
			offset = tagend
			continue while
		end if
		offset += 1
	wend
	return NO
end function

'FIXME: refactor, making use of OO which we can now use
type PrintStrState
	'Public members (may set before passing to render_text)
	as Font ptr thefont
	as long fgcolor          'Used when resetting localpal. May be -1 for none
	as long bgcolor          'Only used if not_transparent
	as bool not_transparent  'Force non-transparency of layer 1
	'as bool debug           'Print debug statements (also need to uncomment this and TEXTDBG lines)

	'Internal members
	as Font ptr initial_font    'Used when resetting thefont
	as long leftmargin
	as long rightmargin
	union
		as XYPair pos
		type
			as long x, y
		end type
	end union
	as long startx
	as long charnum

	'Internal members used only if drawing, as opposed to laying out/measuring
	as Palette16 ptr localpal  'NULL if not initialised
	as long initial_fgcolor  'Used when resetting fgcolor
	as long initial_bgcolor  'Used when resetting bgcolor
	as bool initial_not_trans 'Used when resetting bgcolor

	declare constructor()
	declare constructor(rhs as PrintStrState)
	declare destructor()
	declare sub duplicate_from(rhs as PrintStrState)
end type

' Need a default ctor just because there is a copy ctor
constructor PrintStrState()
end constructor

constructor PrintStrState(rhs as PrintStrState)
	memcpy(@this, @rhs, sizeof(PrintStrState))
	if localpal then
		this.localpal->refcount += 1
	end if
end constructor

'Unlike the copy ctor, this duplicates the palette instead of incrementing refcount.
sub PrintStrState.duplicate_from(rhs as PrintStrState)
	memcpy(@this, @rhs, sizeof(PrintStrState))
	if localpal then
		this.localpal = Palette16_duplicate(rhs.localpal)
	end if
end sub

destructor PrintStrState()
	Palette16_unload @localpal
end destructor

'Special signalling characters
#define tcmdFirst      15
#define tcmdState      15  'Special argument format (6 bytes)
'All following tcmds must have one argument
#define tcmdPalette    16  '1 argument
#define tcmdFont       17  '1 argument: the font number (possibly -1)
#define tcmdLastWithArg 17
'All following tcmds must have zero arguments
#define tcmdRepalette  18  'Call build_text_palette
#define tcmdLast       18

'Invisible argument: state. (member should not be . prefixed, unfortunately)
'Modifies state, and appends a control sequence to the string outbuf to duplicate the change
'Note: in order to support members that are less than 4 bytes (eg palette colours) some hackery is done, and
'members greater than 4 bytes aren't supported
#macro UPDATE_STATE(outbuf, member, value)
	'Ugh! FB doesn't allow sizeof in #if conditions!
	#if typeof(state.member) <> integer and typeof(state.member) <> long
		#error "UPDATE_STATE: bad member type"
	#endif
	outbuf += CHR(tcmdState) & "      "
	*Cast(short ptr, @outbuf[len(outbuf) - 6]) = Offsetof(PrintStrState, member)
	*Cast(long ptr, @outbuf[len(outbuf) - 4]) = Cast(long, value)
	state.member = value
#endmacro

'Interprets a control sequence (at 0-based offset ch in outbuf) written by UPDATE_STATE,
'modifying a member of state.
#define READ_MEMBER(state, outbuf, ch) _
	/' dim offset as long = *Cast(short ptr, @outbuf[ch + 1]) '/ _
	/' dim newval as long = *Cast(long ptr, @outbuf[ch + 3]) '/ _
	*Cast(long ptr, Cast(byte ptr, @state) + *Cast(short ptr, @outbuf[ch + 1])) = _
		*Cast(long ptr, @outbuf[ch + 3]) : _
	ch += 6  '7 bytes in total, assume inside FOR loop that increments ch

#define APPEND_CMD0(outbuf, cmd_id) _
	outbuf += CHR(cmd_id)

#define APPEND_CMD1(outbuf, cmd_id, value) _
	outbuf += CHR(cmd_id) & "    " : _
	*Cast(long ptr, @outbuf[len(outbuf) - 4]) = Cast(long, value)

#define READ_VALUE(variable, outbuf, ch) _
	variable = *Cast(long ptr, @outbuf[ch + 1]) : _
	ch += 4  '5 bytes in total, assume inside FOR loop that increments ch

'Processes starting from z[state.charnum] until the end of the line, returning a string
'which describes a line fragment. It contains printing characters plus command sequences
'for modifying state. state is passed byval (upon wrapping we would have to undo changes
'to the state, which is too hard).
'endchar is 0 based, and exclusive - normally len(z). FIXME: endchar appears broken
'We also compute the line_height (height of the tallest font on the line) and the line_width
'of the line fragment. You have to know the line height before you can know the y
'coordinate of each character on the line.
'Updates to .x, .y are not written because they can be recreated from the character stream,
'nor is .charnum for printing characters (unless updatecharnum is true) because it's too
'expensive. However, .x, .y and .charnum are updated at the end.
'If updatecharnum is true, it is updated only when .charnum jumps; you still need to
'increment after every printing character yourself.
local function layout_line_fragment(z as string, endchar as integer, byval state as PrintStrState, byref line_width as integer, byref line_height as integer, wide as integer, withtags as bool, withnewlines as bool, updatecharnum as bool = NO) as string
	dim lastspace as integer = -1
	dim lastspace_x as integer
	dim lastspace_outbuf_len as integer
	dim lastspace_line_height as integer
	dim endchar_x as integer             'state.x at endchar
	dim endchar_outbuf_len as integer = 999999  'Length of outbuf at endchar
	dim ch as integer                    'We use this instead of modifying .charnum
	dim visible_chars as integer         'Number non-control chars we will return
	dim outbuf as string
	'Appending characters one at a time to outbuf is slow, so we delay it.
	'chars_to_add counts the number of delayed characters
	dim chars_to_add as integer = 0

        'Avoid overflow
	if state.rightmargin = INT_MAX then state.rightmargin = 999999

	with state
		'TEXTDBG("layout '" & z & "' from " & .charnum & " at " & .x & "," & .y)
		line_height = .thefont->line_h
		for ch = .charnum to len(z) - 1
			'We keep going past endchar until the end of the line, to figure out where to linebreak
			if ch >= endchar andalso endchar_outbuf_len = 999999 then
				'If the final character is a newline (and maybe other cases?), or if endchar
				'isn't len(z), then we need to record this.
				'We might skip over ch = endchar because it's in the middle of markup.
				'TEXTDBG("hit endchar, x=" & .x)
				endchar_x = .x
				endchar_outbuf_len = len(outbuf) + chars_to_add
			end if

			if z[ch] = 10 and withnewlines then  'newline
				'TEXTDBG("add " & chars_to_add & " chars before " & ch & " : '" & Mid(z, 1 + ch - chars_to_add, chars_to_add) & "'")
				outbuf += Mid(z, 1 + ch - chars_to_add, chars_to_add)
				chars_to_add = 0
				'Skip past the newline character, but don't add to outbuf
				ch += 1
				if ch - 1 >= endchar then
					'FIXME: If the final character is a newline, we don't add a blank line.
					'But text slices do! We should probably do the same here, e.g. removing
					'this if block (and much more work).
					'However, it's difficult to change that, due to other functions depending
					'this one.

					'FIXME: On the other hand when wrapping with wide=8, currently we will add
					'a blank line every time we encounter a space, but text slices don't! We
					'shouldn't add spaces.
					outbuf = left(outbuf, endchar_outbuf_len)
					line_width = endchar_x
					UPDATE_STATE(outbuf, x, endchar_x)
				else
					line_width = .x
					UPDATE_STATE(outbuf, x, .startx)
				end if
				'Purposefully past endchar
				UPDATE_STATE(outbuf, charnum, ch)
				'Reset margins for next paragraph? No.
				'UPDATE_STATE(outbuf, leftmargin, 0)
				'UPDATE_STATE(outbuf, rightmargin, wide)
				return outbuf
			elseif z[ch] = 8 then ' ^H, hide tag
				if z[ch + 1] = asc("{") then
					dim closebrace as integer = instr((ch + 2) + 1, z, "}") - 1
					if closebrace <> -1 then
						'Add delayed characters first
						'TEXTDBG("add " & chars_to_add & " chars before " & ch & " : '" & Mid(z, 1 + ch - chars_to_add, chars_to_add) & "'")

						outbuf += Mid(z, 1 + ch - chars_to_add, chars_to_add)
						chars_to_add = 0
						ch = closebrace
						if updatecharnum then
							UPDATE_STATE(outbuf, charnum, ch + 1)
						end if
						continue for
					end if
				end if
			elseif z[ch] >= tcmdFirst and z[ch] <= tcmdLast then ' special signalling characters. Not allowed! (FIXME: delete this)
				'TEXTDBG("add " & chars_to_add & " chars before " & ch & " : '" & Mid(z, 1 + ch - chars_to_add, chars_to_add) & "'")

				outbuf += Mid(z, 1 + ch - chars_to_add, chars_to_add)
				chars_to_add = 0
				ch += 1	 'skip
				if updatecharnum then
					UPDATE_STATE(outbuf, charnum, ch + 1)
				end if
				continue for
			elseif z[ch] = asc("$") then
				if withtags and z[ch + 1] = asc("{") then
					dim action as string
					dim intarg as int32

					dim closebrace as integer = parse_tag(z, ch + 2, action, @intarg)
					if closebrace then
						'Add delayed characters first
						'TEXTDBG("add " & chars_to_add & " chars before " & ch & " : '" & Mid(z, 1 + ch - chars_to_add, chars_to_add) & "'")

						outbuf += Mid(z, 1 + ch - chars_to_add, chars_to_add)
						chars_to_add = 0
						if action = "F" then
							'Font
							'Let's preserve the position offset when changing fonts. That way, plain text in
							'the middle of edgetext is also offset +1,+1, so that it lines up visually with it
							'.pos += fonts(intarg)->offset - .thefont->offset
							if intarg >= -1 andalso intarg <= ubound(fonts) then
								if intarg = -1 then
									'UPDATE_STATE(outbuf, thefont, .initial_font)
									.thefont = .initial_font
								elseif fonts(intarg) then
									'UPDATE_STATE(outbuf, thefont, fonts(intarg))
									.thefont = fonts(intarg)
								else
									goto badtexttag
								end if
								APPEND_CMD1(outbuf, tcmdFont, intarg)
								line_height = large(line_height, .thefont->line_h)
							else
								goto badtexttag
							end if
						elseif action = "K" then
							'Foreground colour
							dim col as integer
							if intarg <= -1 then
								col = .initial_fgcolor
							elseif intarg <= 255 THEN
								col = intarg
							else
								goto badtexttag
							end if
							'UPDATE_STATE(outbuf, localpal.col(1), col)
							UPDATE_STATE(outbuf, fgcolor, col)
							APPEND_CMD0(outbuf, tcmdRepalette)
							'No need to update localpal here by calling build_text_palette
						elseif action = "KB" then
							'Background colour
							dim col as integer
							if intarg <= -1 then
								col = .initial_bgcolor
								if .not_transparent <> .initial_not_trans then
									UPDATE_STATE(outbuf, not_transparent, .initial_not_trans)
								end if
							elseif intarg <= 255 THEN
								col = intarg
								if .not_transparent = NO then
									UPDATE_STATE(outbuf, not_transparent, YES)
								end if
							else
								goto badtexttag
							end if
							'UPDATE_STATE(outbuf, localpal.col(0), col)
							UPDATE_STATE(outbuf, bgcolor, col)
							APPEND_CMD0(outbuf, tcmdRepalette)
							'No need to update localpal here by calling build_text_palette
						elseif action = "KP" then
							'Font palette
							if intarg >= 0 and intarg <= gen(genMaxPal) then
								APPEND_CMD1(outbuf, tcmdPalette, intarg)
								'No need up update palette or fgcolor here
								'(don't want to duplicate that logic here)
							else
								goto badtexttag
							end if
						elseif action = "LM" then
							UPDATE_STATE(outbuf, leftmargin, intarg)
						elseif action = "RM" then
							UPDATE_STATE(outbuf, rightmargin, wide - intarg)
						else
							goto badtexttag
						end if
						ch = closebrace
						if updatecharnum then
							UPDATE_STATE(outbuf, charnum, ch + 1)
						end if
						continue for
					end if

					badtexttag:
				end if
			elseif z[ch] = asc(" ") then
				lastspace = ch
				lastspace_outbuf_len = len(outbuf) + chars_to_add
				lastspace_x = .x
				lastspace_line_height = line_height
			end if

			.x += .thefont->w(z[ch])
			if .x > .startx + .rightmargin then
				'TEXTDBG("rm = " & .rightmargin & " lm = " & .leftmargin)
				if lastspace > -1 and .x - lastspace_x < 3 * (.rightmargin - .leftmargin) \ 5 then
					'Split at the last space

					if chars_to_add then
						'TEXTDBG("add " & chars_to_add & " chars before " & ch & " : '" & Mid(z, 1 + ch - chars_to_add, chars_to_add) & "'")

						outbuf += Mid(z, 1 + ch - chars_to_add, chars_to_add)
					end if
					outbuf = left(outbuf, small(endchar_outbuf_len, lastspace_outbuf_len))
					if lastspace < endchar then
						line_width = lastspace_x
						UPDATE_STATE(outbuf, x, .startx + .leftmargin)
					else
						line_width = endchar_x
					end if
					line_height = lastspace_line_height
					UPDATE_STATE(outbuf, charnum, lastspace + 1)

					return outbuf
				else
					'Split the word instead, it would just look ugly to break the line
					if visible_chars = 0 then
						'Always output at least one character
						chars_to_add += 1
						ch += 1
					end if
					exit for
				end if
			end if

			'Add this character to outbuf. But not immediately.
			chars_to_add += 1
			visible_chars += 1
		next

		'Hit end of text, or splitting word
		if chars_to_add then
			'TEXTDBG("add " & chars_to_add & " chars before " & ch & " : '" & Mid(z, 1 + ch - chars_to_add, chars_to_add) & "'")
			outbuf += Mid(z, 1 + ch - chars_to_add, chars_to_add)
		end if
		'Set final x and charnum, and trim off outbuf anything parsed after endchar
		if endchar_outbuf_len = 999999 then 'ch <= endchar then
			'Didn't reach endchar
			'TEXTDBG("exiting layout_line_fragment, ch = " & ch & ", .x = " & .x)
			line_width = .x
			UPDATE_STATE(outbuf, x, .startx + .leftmargin)
		else
			'Reached endchar and continued
			'TEXTDBG("exiting layout_line_fragment, ch = " & ch & ", endchar_x = " & endchar_x)
			outbuf = left(outbuf, endchar_outbuf_len)
			line_width = endchar_x
			UPDATE_STATE(outbuf, x, endchar_x)
		end if
		UPDATE_STATE(outbuf, charnum, ch)
		'Preserve .leftmargin and .rightmargin

		return outbuf
	end with
end function

'Build state.localpal
sub build_text_palette(byref state as PrintStrState, srcpal as Palette16 ptr)
	with state
		if state.localpal = NULL then
			state.localpal = Palette16_new()
		end if
		if srcpal then
			memcpy(@.localpal->col(0), @srcpal->col(0), srcpal->numcolors)
			.localpal->numcolors = srcpal->numcolors
		end if
		.localpal->col(0) = .bgcolor
		if .fgcolor > -1 then
			.localpal->col(1) = .fgcolor
		end if
		if srcpal = NULL and .fgcolor = -1 then
			debug "render_text: Drawing a font without a palette or foreground colour!"
		end if
		'TEXTDBG("build_text_palette: bg = " & .bgcolor & " fg = "& .fgcolor & " outline = " & .thefont->outline_col)
		'Outline colours are a hack, hopefully temp.
		if .thefont->outline_col > 0 then
			.localpal->col(.thefont->outline_col) = uilook(uiOutline)
		end if
	end with
end sub

'Processes a parsed line, updating the state passed to it, and also optionally draws one of the layers (if reallydraw)
sub draw_line_fragment(dest as Frame ptr, byref state as PrintStrState, layer as integer, parsed_line as string, reallydraw as bool)
	dim arg as integer
	dim as Frame charframe
	charframe.mask = NULL
	charframe.refcount = NOREFC

	dim byref cliprect as ClipState = get_cliprect()

	with state
		'TEXTDBG("draw frag: x=" & .x & " y=" & .y & " char=" & .charnum & " reallydraw=" & reallydraw & " layer=" & layer)
		for ch as integer = 0 to len(parsed_line) - 1
			if parsed_line[ch] = tcmdState then
				'Control sequence. Make a change to state, and move ch past the sequence
				READ_MEMBER(state, parsed_line, ch)

			elseif parsed_line[ch] = tcmdFont then
				READ_VALUE(arg, parsed_line, ch)
				if arg >= -1 andalso arg <= ubound(fonts) then
					if arg = -1 then
						'UPDATE_STATE(outbuf, thefont, .initial_font)
						.thefont = .initial_font
					elseif fonts(arg) then
						'UPDATE_STATE(outbuf, thefont, fonts(arg))
						.thefont = fonts(arg)
					else
						'This should be impossible, because layout_line_fragment has already checked this
						showbug "draw_line_fragment: NULL font!"
					end if
				else
					'This should be impossible, because layout_line_fragment has already checked this
					showbug "draw_line_fragment: invalid font!"
				end if
				if reallydraw then
					'In case .fgcolor == -1 and .thefont->pal == NULL. Palette changes are per-font,
					'so reset the colour.
					if .fgcolor = -1 then .fgcolor = .initial_fgcolor
					'We rebuild the local palette using either the font's palette or from scratch
					build_text_palette state, .thefont->pal
				end if

			elseif parsed_line[ch] = tcmdPalette then
				READ_VALUE(arg, parsed_line, ch)
				if reallydraw then
					dim pal as Palette16 ptr
					pal = Palette16_load(arg)
					if pal then
						'Palettes override the foreground colour (but not background or outline)
						.fgcolor = -1
						build_text_palette state, pal
						Palette16_unload @pal
					end if
					'FIXME: in fact pal should be kept around, for tcmdRepalette
				end if

			elseif parsed_line[ch] = tcmdRepalette then
				if reallydraw then
					'FIXME: if we want to support switching to a non-font palette, then
					'that palette should be stored in state and used here
					build_text_palette state, .thefont->pal
				end if

			else
				'Draw a character

				'Fun hack! Console support
				if layer = 1 and gfx_printchar <> NULL then
					gfx_printchar(parsed_line[ch], .x, .y, .fgcolor)
				end if

				'Print one character past the end of the line
				if reallydraw and .x <= cliprect.r then
					if .thefont->layers(layer) <> NULL then
						with .thefont->layers(layer)->chdata(parsed_line[ch])
							charframe.image = state.thefont->layers(layer)->spr->image + .offset
							charframe.w = .w
							charframe.h = .h
							charframe.pitch = .w
'debug " <" & (state.x + .offx) & "," & (state.y + .offy) & ">"
							dim trans as bool = YES
							'FIXME: why do we only allow 1-layer fonts to be non transparent?
							'(2-layer fonts would need layer 0 to be opaque)
							'ALSO, this would stuff up ${KB#} on 2-layer fonts
							if layer = 1 and state.not_transparent then trans = NO
							frame_draw_internal(@charframe, curmasterpal(), state.localpal, state.x + .offx, state.y + .offy - state.thefont->line_h, trans, dest)
						end with
					end if
				end if

				'Note: do not use charframe.w, that's just the width of the sprite
				.x += .thefont->w(parsed_line[ch])
			end if
		next
	end with
end sub


'Draw a string. You will normally want to use one of the friendlier overloads for this,
'probably the most complicated function in the engine.
'
'Arguments:
'
'Pass in a reference to a (fresh!!) PrintStrState object with .thefont and .fgcolor set
'.fgcolor can be -1 for no colour (just use font palette).
'.not_transparent and .bgcolor (only used if .not_transparent) may also be set
'
'At least one of <s>pal and</s> the (current) font pal and .fgcolor must be not NULL/-1.
'This can be ensured by starting with either a palette or a .fgcolor!=-1
'FIXME: pal is currently disabled; palette handling needs rewriting.
'
'endchar shouldn't be used; currently broken?
'
'If withtags is false then no tags are processed.
'If withtags is true, the follow "basic texttags" are processed:
'  (These will change!)
'-${F#}  changes to font # or return to initial font if # == -1
'-${K#}  changes foreground/first colour, or return to initial colour if # == -1
'        (Note that this does disable the foreground colour, unless the initial fg colour was -1!)
'-${KB#} changes the background colour, and turns on not_transparent.
'        Specify -1 to restore previous background colour and transparency
'        FIXME: ${KB0} does NOT switch to transparency, but an initial bgcol of 0 IS transparent!
'-${KP#} changes to palette # (-1 is invalid) (Maybe should make ${F-1} return to the default)
'        (Note, palette changes are per-font, and expire when the font changes)
'-${LM#} sets left margin for the current line, in pixels
'-${RM#} sets right margin for the current line, in pixels
'Purposefully no way to set background colour.
'Unrecognised and invalid basic texttags are printed as normal.
'ASCII character 8 can be used to hide texttags by overwriting the $, like so: \008{X#}
'
'Clipping and wrapping:
'If you specify a page width (the default is "infinite"), then text automatically wraps according
'to current margins. Otherwise there is no limit on the right (not even the edge of the screen).
'xpos is the left limit, and xpos+wide is the right limit from which margins are measured (inwards).
'Drawn text is NOT clipped to this region, use setclip or frame_new_view for that.
'This region may be larger than the clip area.
'If withnewlines is true, then newlines (ASCII character 10) are respected
'instead of printed as normal characters.
'
'If you want to skip some number of lines, you should clip, and draw some number of pixels
'above the clipping rectangle.
'
sub render_text (dest as Frame ptr, byref state as PrintStrState, text as string, endchar as integer = 999999, xpos as RelPos, ypos as RelPos, wide as RelPos = 999999, pal as Palette16 ptr = NULL, withtags as bool = YES, withnewlines as bool = YES)
', cached_state as PrintStrStatePtr = NULL, use_cached_state as bool = YES)

'static tog as integer = 0
'tog xor= 1
'dim t as double = timer

	BUG_IF(dest = null, "NULL dest")

	dim byref cliprect as ClipState = get_cliprect(dest)

	'check bounds skipped because this is now quite hard to tell (checked in draw_clipped)

	'TEXTDBG("printstr '" & text & "' (len=" & len(text) & ") wide = " & wide & " tags=" & withtags & " nl=" & withnewlines)

	wide = relative_pos(wide, dest->w)

	' Only pre-compute the text dimensions if required for anchoring, as it's quite expensive
	dim as AlignType xanchor, yanchor, xshow, yshow
	RelPos_decode xpos, 0, 0, xanchor, xshow
	RelPos_decode ypos, 0, 0, yanchor, yshow
	dim finalsize as StringSize
	if xanchor <> alignLeft or yanchor <> alignLeft or xshow <> alignCenter or yshow <> alignCenter then
		text_layout_dimensions @finalsize, text, endchar, , wide, state.thefont, withtags, withnewlines
	end if

	with state
		/'
		if cached_state <> NULL and use_cached_state then
			state = *cached_state
			cached_state = NULL
		else
		'/
			'if pal then
			'	build_text_palette state, pal
			'else
				build_text_palette state, .thefont->pal
			'end if
			.initial_font = .thefont
			.initial_fgcolor = .fgcolor
			.initial_bgcolor = .bgcolor
			.initial_not_trans = .not_transparent
			.charnum = 0
			.pos = relative_pos(XY(xpos, ypos), dest->size, finalsize.size) + .thefont->offset
			.startx = .x
			'Margins are measured relative to xpos
			.leftmargin = 0
			.rightmargin = wide
		'end if

		dim as bool visibleline  'Draw this line of text?

		'We have to process both layers, even if the current font has only one layer,
		'in case the string switches to a font that has two!
		'That's why we use two copies of state.
		'Make sure to use a separate Palette16.
		dim prev_state as PrintStrState
		prev_state.duplicate_from(state)
		dim prev_parse as string
		dim prev_visible as bool
		dim draw_layer1 as bool = NO  'Don't draw on first loop

		if endchar > len(text) then endchar = len(text)
		do
			dim line_height as integer
			dim parsed_line as string = layout_line_fragment(text, endchar, state, 0, line_height, wide, withtags, withnewlines)
			'TEXTDBG("parsed: " + parsed_line)
			'Print at least one extra line above and below the visible region, in case the
			'characters are big (we only approximate this policy, with the current font height)
			visibleline = (.y + line_height > cliprect.t - .thefont->char_h AND .y < cliprect.b + .thefont->char_h)
			'if tog then visibleline = NO
			'debug "vis: " & visibleline

			'FIXME: state caching was meant to kick in after the first visible line of text, not here;
			'however need to rethink how it should work
/'
			if cached_state then
				*cached_state = state
				cached_state = NULL  'Don't save again
			end if
'/
			.y += line_height

			'Update state while drawing layer 0 (if visible)
			draw_line_fragment(dest, state, 0, parsed_line, visibleline)

			if draw_layer1 then
				'Now update prev_state (to the beginning of THIS line) while drawing layer 1
				'for the previous line. Afterwards, prev_state will be identical to state
				'as it was at the start of this loop.
				draw_line_fragment(dest, prev_state, 1, prev_parse, prev_visible)
				'TEXTDBG("prev.charnum=" & prev_state.charnum)
				if prev_state.charnum >= endchar then /'debug "text end" :'/ exit do
				if prev_state.y > cliprect.b + prev_state.thefont->char_h then exit do
			end if
			draw_layer1 = YES
			prev_parse = parsed_line
			prev_visible = visibleline
			prev_state.y += line_height
		loop
	end with
	't = timer - t
	'debug "prinstr" & tog & " len " & len(text) & " in " & t*1000 & "ms"
end sub

'Calculate size of part of a block of text when drawn, returned in retsize
'endchar and endline can be used to trim the string.
'endchar is the number of chars (bytes), not a 1-based string position!
'NOTE: Edged font has width 1 pixel more than Plain font, due to .offset.x.
sub text_layout_dimensions (retsize as StringSize ptr, z as string, endchar as integer = 999999, endline as integer = 999999, wide as integer = 999999, fontp as Font ptr, withtags as bool = YES, withnewlines as bool = YES)
	'debug "[text_layout_dimensions] endchar=" & endchar
	dim state as PrintStrState
	with state
		'.localpal/?gcolor/initial_?gcolor/transparency non-initialised
		.thefont = fontp
		.initial_font = .thefont
		.charnum = 0
		.pos = .thefont->offset
		'Margins are measured relative to xpos
		.leftmargin = 0
		.rightmargin = wide

		dim maxwidth as integer = 0
		dim line_width as integer = 0
		dim line_height as integer = 0
		retsize->lines = 0

		if endchar > len(z) then endchar = len(z)
		while .charnum < len(z)
			if .charnum > endchar orelse retsize->lines >= endline then exit while
			'If .charnum = endchar, the last line is zero length, but should be included.
			'(That sounds wrong. Doesn't it actually mean endchar points at a newline?)
			'.charnum won't advance, so need extra check to prevent infinite loop!
			dim exitloop as bool = (.charnum = endchar)
			dim parsed_line as string = layout_line_fragment(z, endchar, state, line_width, line_height, wide, withtags, withnewlines)
			retsize->lines += 1
			'TEXTDBG("parsed a line, line_width =" & line_width)
			maxwidth = large(maxwidth, line_width)

			'if .debug then edgeprint STR(line_width), pRight, .y, 10, vpage

			'Update state
			.y += line_height
			draw_line_fragment(NULL, state, 0, parsed_line, NO)  'reallydraw=NO
			'TEXTDBG("now " & .charnum & " at " & .pos)
			if exitloop then exit while
		wend

		'layout_line_fragment sets .charnum to the beginning of the next line. It's a 0-based
		'index. Instead we return it as a 1-based index to the end of the current line
		'(char on which the line wraps).
		retsize->lineend = .charnum
		retsize->size = XY(maxwidth, .y)
		retsize->lastw = line_width
		retsize->lasth = line_height
		retsize->finalfont = .thefont
		'debug "[/text_layout_dimensions] charnum=" & .charnum
	end with
end sub

'Returns the length in pixels of the longest line of a *non-autowrapped* string.
function textwidth(text as string, fontnum as integer = fontPlain, withtags as bool = YES, withnewlines as bool = YES) as integer
	dim retsize as StringSize
	text_layout_dimensions @retsize, text, , , , get_font(fontnum), withtags, withnewlines
	return retsize.size.w
end function

'Returns the width and height of an autowrapped string.
'Specify the wrapping width; 'wide' might include rWidth for the width of the screen
'(which is what the page arg is for).
function textsize(text as string, wide as RelPos = rWidth, fontnum as integer = fontPlain, withtags as bool = YES, page as integer = -1) as XYPair
	if page = -1 then page = vpage
	wide = relative_pos(wide, vpages(page)->w)
	dim retsize as StringSize
	text_layout_dimensions @retsize, text, , , wide, get_font(fontnum), withtags, YES
	return retsize.size
end function

'Returns the default height of a line of text of a certain font.
'Warning: this currently returns 10 for 8x8 fonts, because that's what text slices use. Sigh.
'However standardmenu (calc_menu_rect) by default uses 9 for fontEdged and 8 for fontPlain
'and draw_menu by default uses 10. Nonstandard menus use 8-10.
function lineheight(fontnum as integer = fontEdged) as integer
	return get_font(fontnum, YES)->line_h
end function

'Pixel size of a character in a font
function charsize(char as integer, fontp as Font ptr) as XYPair
	dim w as integer
	if char <= 0 then   'This happens when we index 1 past the end of the string
		w = fontp->w(ASC(" "))  'Dummy value
	else
		w = fontp->w(char)
	end if
	return XY(w, fontp->char_h)
end function

function charsize(char as integer, fontnum as integer) as XYPair
	return charsize(char, get_font(fontnum))
end function

'Calculate the position at which a certain character in a block of text will be drawn
'FIXME: this returns the wrong position for a space/newline at the end of a line (it
'returns the start of the next line).
'To fix, render_text (or maybe layout_line_fragment?) needs to be changed.
sub find_text_char_position(retsize as StringCharPos ptr, text as string, charnum as integer, wide as RelPos = rWidth, fontnum as integer = fontPlain, withtags as bool = YES, page as integer = -1)
	if page = -1 then page = vpage
	wide = relative_pos(wide, vpages(page)->w)
	dim size as StringSize
	text_layout_dimensions @size, text, charnum, , wide, get_font(fontnum), withtags, YES
	with *retsize
		.charnum = charnum
		.exacthit = YES   'Maybe return NO if it's at the end of the line?
		.pos.x = size.lastw
		.pos.y = size.size.h - size.lasth
		.size = charsize(iif(charnum >= len(text), 0, text[charnum]), size.finalfont)
		.lineh = size.lasth
	end with
end sub

'Calculate character position in string from pixel position.
'draw_pos is where the text was drawn... redundant to subtracting that out of seekpt,
'but in future we might cache render_text state.
'NOTE: draw_pos is NOT a RelPosXY, unlike render_text! Because we don't know the size of the dest Frame.
sub find_point_in_text (retsize as StringCharPos ptr, seekpt as XYPair, z as string, wide as integer = 999999, draw_pos as XYPair = XY(0,0), fontnum as integer, withtags as bool = YES, withnewlines as bool = YES)
	dim state as PrintStrState
	with state
		'.localpal/?gcolor/initial_?gcolor/transparency non-initialised
		.thefont = get_font(fontnum)
		.initial_font = .thefont
		.charnum = 0
		.pos = draw_pos + .thefont->offset
		'.pos = relative_pos(draw_pos, destfr->size, finalsize.size) + .thefont->offset
		.startx = .x

		'Margins are measured relative to draw_pos.x
		.leftmargin = 0
		.rightmargin = wide
		'if left(z,11) = "${K15}Press" then .debug = YES

		dim delayedmatch as bool = NO
		dim line_width as integer
		dim line_height as integer
		dim arg as integer

		retsize->exacthit = NO

		while .charnum < len(z)
			dim parsed_line as string = layout_line_fragment(z, len(z), state, line_width, line_height, wide, withtags, withnewlines, YES)
			.y += line_height
			'.y now points to 1 pixel past the bottom of the line fragment

			'Update state
			'Note: .charnum is character in original text, ch is character in parser output
			for ch as integer = 0 to len(parsed_line) - 1
				dim char as integer = parsed_line[ch]
				if char = tcmdState then
					'Make a change to the state
					'TEXTDBG("READ_MEMBER: ch=" & ch & " charnum=" & .charnum)
					READ_MEMBER(state, parsed_line, ch)
				elseif char >= tcmdFirst andalso char <= tcmdLast then
					'TEXTDBG("CMD(" & char & "): ch=" & ch & " charnum=" & .charnum)
					if char <= tcmdLastWithArg then
						READ_VALUE(arg, parsed_line, ch)
						'TEXTDBG("READ_VALUE: arg=" & arg)
						if char = tcmdFont then
							.thefont = fonts(arg)
						end if
					end if
					'All other commands: ignore
				else
					'TEXTDBG("CHAR(" & char & " " & CHR(char) & ") ch=" & ch & " charnum = " & .charnum & " x = " & .x)
					dim w as integer = .thefont->w(char)
					'Draw a character
					if delayedmatch then
						'retsize->w = w
						exit while
					end if
					.x += w
					if .y > seekpt.y andalso .x > seekpt.x then
						'TEXTDBG("HIT w/ x=" & .x & " ch=" & ch & " charnum=" & .charnum)
						'retsize->w = w
						retsize->exacthit = YES
						.x -= w
						exit while
					end if
					.charnum += 1
				end if
			next

			'TEXTDBG("After parsing: charnum = " & .charnum & " line_width = " & line_width & " x = " & .x)

			if .y > seekpt.y then
				'Position was off the (right-hand) end of the line
				if .charnum > 0 then
					dim lastchar as ubyte = z[.charnum - 1]
					if lastchar = 32 or (lastchar = 10 andalso withnewlines) then
						'This point is actually on a space/newline, which was
						'not added to parsed_string. So don't delay.
						retsize->exacthit = YES
						.x = line_width
						.charnum -= 1
						exit while
					end if
				end if
				delayedmatch = YES
				'TEXTDBG("FIND IN: delayed")
			end if
		wend

		retsize->charnum = .charnum
		retsize->pos.x = .x
		retsize->pos.y = .y - .thefont->line_h
		retsize->size = charsize(z[.charnum], .thefont)  '.charnum = len(z) is OK
		retsize->lineh = line_height
	end with
end sub

'the old printstr -- no autowrapping
sub printstr (text as string, x as RelPos, y as RelPos, page as integer, withtags as bool = NO, fontnum as integer = fontPlain)
	dim state as PrintStrState
	state.thefont = get_font(fontnum)
	if textbg <> 0 then state.not_transparent = YES
	state.bgcolor = textbg
	state.fgcolor = textfg

	render_text (vpages(page), state, text, , x, y, , , withtags, NO)
end sub

'this doesn't autowrap either
sub edgeprint (text as string, x as RelPos, y as RelPos, col as integer, page as integer, withtags as bool = NO, withnewlines as bool = NO)
	'preserve the old behaviour (edgeprint used to call textcolor)
	textfg = col
	textbg = 0

	dim state as PrintStrState
	state.thefont = fonts(fontEdged)
	state.fgcolor = col

	render_text (vpages(page), state, text, , x, y, , , withtags, withnewlines)
end sub

'A flexible edgeprint/printstr replacement.
'Either specify the colour, or omit it and use textcolor().
'Wraps the text at 'wide'; pass "rWidth - x" to wrap at the right edge of the screen.
sub wrapprint (text as string, x as RelPos, y as RelPos, col as integer = -1, page as integer, wide as RelPos = rWidth, withtags as bool = YES, fontnum as integer = fontEdged)
	dim state as PrintStrState
	state.thefont = fonts(fontnum)
	if col = -1 then
		state.fgcolor = textfg
		state.bgcolor = textbg
		if textbg <> 0 then state.not_transparent = YES
	else
		state.fgcolor = col
		state.bgcolor = 0
	end if
	render_text (vpages(page), state, text, , x, y, wide, , withtags, YES)
end sub

'Like wrapprint except (optionally, by default) a transparent rectangle is drawn
'behind the text.
'TODO: this is a temporary solution, this ought to be handled by the standard
'text drawing functions, and there ought to be a markup code to enable it.
sub wrapprintbg (text as string, x as RelPos, y as RelPos, col as integer = -1, page as integer, drawbg as bool = YES, wrapx as RelPos = rWidth, withtags as bool = YES, fontnum as integer = fontEdged)
	if drawbg then
		trans_rectangle vpages(page), TYPE(x, y, textwidth(text), 10), master(uilook(uiBackground)), 0.55
	end if
	wrapprint text, x, y, col, page, wrapx, withtags, fontnum
end sub

sub textcolor (fg as integer, bg as integer)
	textfg = fg
	textbg = bg
end sub

function fgcol_text(text as string, colour as integer) as string
	return "${K" & colour & "}" & text & "${K-1}"
end function

function bgcol_text(text as string, colour as integer) as string
	return "${KB" & colour & "}" & text & "${KB-1}"
end function

'Remove all the valid text markup (not embed codes) like ${K-1} from a string.
function remove_markup(text as string) as string
	dim offset as integer = 1
	dim tagend as integer
	dim ret as string
	do
		dim last as integer = offset
		if next_text_markup(text, offset, tagend) = NO then
			ret &= mid(text, last)
			return ret
		end if
		ret &= mid(text, last, offset - last)
		offset = tagend  'Skip over tag
	loop
end function

'Remove everything except valid text markup (not embed codes) from a string.
'Useful for skipping over text but getting the same effects
function just_markup(text as string) as string
	dim offset as integer = 1
	dim tagend as integer
	dim ret as string
	do
		if next_text_markup(text, offset, tagend) = NO then return ret
		ret &= mid(text, offset, tagend - offset)
		offset = tagend  'Skip over tag
	loop
end function


'==========================================================================================
'                                           Fonts
'==========================================================================================


constructor FontLayer()
	refcount = 1
end constructor

constructor FontLayer(src as FontLayer ptr)
	memcpy(@this, src, sizeof(FontLayer))  'Copy chdata
	spr = frame_duplicate(src->spr)
	refcount = 1
end constructor

'Decrement refcount, and null out the ptr
sub fontlayer_unload (layerpp as FontLayer ptr ptr)
	BUG_IF(layerpp = NULL, "NULL ptr")
	if *layerpp then
		(*layerpp)->refcount -= 1
		if (*layerpp)->refcount <= 0 then
			delete *layerpp
		end if
		*layerpp = NULL
	end if
end sub

destructor FontLayer()
	frame_unload @spr
end destructor


constructor Font()
end constructor

'A copy of a Font which shares the layers with the original Font
constructor Font(src as Font ptr)
	memcpy(@this, src, sizeof(Font))
	for idx as integer = 0 to 1
		if layers(idx) then layers(idx)->refcount += 1
	next
end constructor

'This deletes a Font object pointed to by a pointer. It's OK to call on a ptr to a NULL ptr
sub font_unload (fontpp as Font ptr ptr)
	BUG_IF(fontpp = NULL, "NULL font")
	dim fontp as Font ptr = *fontpp
	if fontp = null then exit sub
	delete fontp
	*fontpp = NULL
end sub

destructor Font()
	for i as integer = 0 to 1
		fontlayer_unload @layers(i)
	next
	Palette16_unload @pal
end destructor

'Create a version of a font with an outline around each character (in a new palette colour)
function font_create_edged (basefont as Font ptr) as Font ptr
	BUG_IF(basefont = NULL, "NULL font", NULL)
	BUG_IF(basefont->layers(1) = NULL, "blank font", NULL)
	CHECK_FRAME_8BIT(basefont->layers(1)->spr, NULL)

	dim newfont as Font ptr = new Font()

	newfont->layers(0) = new FontLayer()
	'Share layer 1
	newfont->layers(1) = basefont->layers(1)
	newfont->layers(1)->refcount += 1

	dim size as integer
	'since you can only WITH one thing at a time
	dim bchr as FontChar ptr
	bchr = @basefont->layers(1)->chdata(0)

	dim as integer ch

	for ch = 0 to 255
		newfont->w(ch) = basefont->w(ch)

		with newfont->layers(0)->chdata(ch)
			.offset = size
			.offx = bchr->offx - 1
			.offy = bchr->offy - 1
			.w = bchr->w + 2
			.h = bchr->h + 2
			size += .w * .h
		end with
		bchr += 1
	next

	'This is a hack; create a size*1 size frame, which we use as a buffer for pixel data
	newfont->layers(0)->spr = frame_new(size, 1, , YES)

	newfont->char_h = basefont->char_h + 2
	newfont->line_h = basefont->line_h  'This is for backcompat with text slices...
	newfont->offset = basefont->offset
	newfont->cols = basefont->cols
	if basefont->outline_col = 0 then
		'Doesn't already have an outline colour
		newfont->cols += 1
		newfont->outline_col = newfont->cols
	else
		newfont->outline_col = basefont->outline_col
	end if

	'Stuff currently hardcoded to keep edged font working as before
	newfont->offset.x = 1
	newfont->offset.y = 1

	'dim as ubyte ptr maskp = basefont->layers(0)->spr->mask
	dim as ubyte ptr sptr
	dim as ubyte ptr srcptr = newfont->layers(1)->spr->image
	dim as integer x, y

	for ch = 0 to 255
		with newfont->layers(0)->chdata(ch)
			sptr = newfont->layers(0)->spr->image + .offset + .w + 1
			for y = 1 to .h - 2
				for x = 1 to .w - 2
					if *srcptr then
						sptr[-.w + 0] = newfont->outline_col
						sptr[  0 - 1] = newfont->outline_col
						sptr[  0 + 1] = newfont->outline_col
						sptr[ .w + 0] = newfont->outline_col
					end if
					'if *sptr = 0 then *maskp = 0 else *maskp = &hff
					sptr += 1
					srcptr += 1
					'maskp += 8
				next
				sptr += 2
			next
		end with
	next

	return newfont
end function

'Create a version of a font with a drop shadow (in a new palette colour)
function font_create_shadowed (basefont as Font ptr, xdrop as integer = 1, ydrop as integer = 1) as Font ptr
	BUG_IF(basefont = NULL, "NULL font", NULL)
	BUG_IF(basefont->layers(1) = NULL, "blank font", NULL)
	CHECK_FRAME_8BIT(basefont->layers(1)->spr, NULL)

	dim newfont as Font ptr = new Font(basefont)

	'Layer 0 is a copy of layer 1 from the old font
	fontlayer_unload @newfont->layers(0)
	newfont->layers(0) = new FontLayer(basefont->layers(1))
	'Layer 1 is shared with the base font

	if newfont->outline_col = 0 then
		'Doesn't already have an outline colour
		newfont->cols += 1
		newfont->outline_col = newfont->cols
	end if

	for ch as integer = 0 to 255
		with newfont->layers(0)->chdata(ch)
			.offx += xdrop
			.offy += ydrop
		end with
	next

	with *newfont->layers(0)->spr
		for i as integer = 0 to .w * .h - 1
			if .image[i] then
				.image[i] = newfont->outline_col
			end if
		next
	end with

	return newfont
end function

function font_loadold1bit (fontdata as ubyte ptr) as Font ptr
	dim newfont as Font ptr = new Font()

	newfont->layers(1) = new FontLayer()
	newfont->layers(1)->spr = frame_new(8, 256 * 8)
	newfont->char_h = 8
	newfont->line_h = 10  'I would have said 9, but this is what was used in text slices
	newfont->offset.x = 0
	newfont->offset.y = 0
	newfont->cols = 1
	newfont->outline_col = 0  'None

	'dim as ubyte ptr maskp = newfont->layers(1)->spr->mask
	dim as ubyte ptr sptr = newfont->layers(1)->spr->image

	dim as integer ch, x, y
	dim as integer fi 'font index
	dim as integer fstep

	for ch = 0 to 255
		newfont->w(ch) = 8
		with newfont->layers(1)->chdata(ch)
			.w = 8
			.h = 8
			.offset = 64 * ch
		end with

		'find fontdata index, bearing in mind that the data is stored
		'2-bytes at a time in 4-byte integers, due to QB->FB quirks,
		'and fontdata itself is a byte pointer. Because there are
		'always 8 bytes per character, we will always use exactly 4
		'ints, or 16 bytes, making the initial calc pretty simple.
		fi = ch * 16
		'fi = ch * 8	'index to fontdata
		fstep = 1 'used because our indexing is messed up, see above
		for x = 0 to 7
			for y = 0 to 7
				*sptr = (fontdata[fi] shr y) and 1
				'if *sptr = 0 then *maskp = 0 else *maskp = &hff
				sptr += 8
				'maskp += 8
			next
			fi = fi + fstep
			fstep = iif(fstep = 1, 3, 1) 'uneven steps due to 2->4 byte thunk
			sptr += 1 - 8 * 8
			'maskp += 1 - 8 * 8
		next
		sptr += 8 * 8 - 8
		'maskp += 8 * 8 - 8
	next

	return newfont
end function

'Load each character from an individual BMP in a directory, falling back to some other
'font for missing BMPs
'This function is for testing purposes only, and will be removed unless this shows some use:
'uses hardcoded values
function font_loadbmps (directory as string, fallback as Font ptr = null) as Font ptr
	dim newfont as Font ptr = new Font()

	newfont->layers(0) = null
	newfont->layers(1) = new FontLayer()
	'Hacky: start by allocating 4096 pixels, expand as needed
	newfont->layers(1)->spr = frame_new(1, 4096)
	newfont->cols = 1  'hardcoded
	newfont->outline_col = 0  'None

	dim maxheight as integer
	if fallback then
		maxheight = fallback->char_h
		newfont->offset.x = fallback->offset.x
		newfont->offset.y = fallback->offset.y
		newfont->cols = fallback->cols
	end if

	dim as ubyte ptr image = newfont->layers(1)->spr->image
	dim as ubyte ptr sptr
	dim as integer size = 0
	dim as integer i
	dim f as string
	dim tempfr as Frame ptr
	dim bchr as FontChar ptr
	if fallback andalso fallback->layers(1) then
		bchr = @fallback->layers(1)->chdata(0)
	end if

	for i = 0 to 255
		with newfont->layers(1)->chdata(i)
			f = finddatafile(directory & SLASH & i & ".bmp", NO)
			if isfile(f) then
				'FIXME: awful stuff
				tempfr = image_import_as_frame_raw(f)  ', master())

				.offset = size
				.offx = 0
				.offy = 0
				.w = tempfr->w
				.h = tempfr->h
				if .h > maxheight then maxheight = .h
				newfont->w(i) = .w
				size += .w * .h
				image = reallocate(image, size)
				sptr = image + .offset
				memcpy(sptr, tempfr->image, .w * .h)
				frame_unload @tempfr
			else
				if bchr = NULL then
					visible_debug "font_loadbmps: " & i & ".bmp missing and fallback font not provided"
					font_unload @newfont
					return null
				end if

				.offset = size
				.offx = bchr->offx
				.offy = bchr->offy
				.w = bchr->w
				.h = bchr->h
				newfont->w(i) = .w
				size += .w * .h
				image = reallocate(image, size)
				memcpy(image + .offset, fallback->layers(1)->spr->image + bchr->offset, .w * .h)
			end if
		end with

		bchr += 1
	next

	newfont->layers(1)->spr->image = image
	newfont->char_h = maxheight
	newfont->line_h = maxheight + 2

	return newfont
end function

'Load a font from an image which contains all 256 characters in a 16x16 grid (all characters the same size)
function font_load_16x16 (filename as string) as Font ptr
	dim image as Frame ptr
	image = image_import_as_frame_raw(filename)
	FAIL_IF(image = NULL, "couldn't load file", NULL)

	if image->w MOD 16 ORELSE image->h MOD 16 then
		debug "font_load_16x16: " & filename & ": bad dimensions " & image->size
		frame_unload @image
		return null
	end if

	dim newfont as Font ptr = new Font()

	dim as integer charw, charh
	charw = image->w \ 16
	charh = image->h \ 16
	newfont->char_h = charh
	newfont->line_h = charh + 2
	newfont->offset.x = 0
	newfont->offset.y = 0
	newfont->outline_col = 0  'None
	newfont->layers(0) = null
	newfont->layers(1) = new FontLayer()

	'"Linearise" the characters. In future this will be unnecessary
	newfont->layers(1)->spr = frame_new(charw, charh * 256)

	dim as integer size = 0

	for i as integer = 0 to 255
		with newfont->layers(1)->chdata(i)
			.offset = size
			.offx = 0
			.offy = 0
			.w = charw
			.h = charh
			newfont->w(i) = .w
			size += .w * .h
			dim tempview as Frame ptr
			tempview = frame_new_view(image, charw * (i mod 16), charh * (i \ 16), charw, charh)
			'setclip , charh * i, , charh * (i + 1) - 1, newfont->layers(1)->spr
			frame_draw tempview, , 0, charh * i, NO, newfont->layers(1)->spr
			frame_unload @tempview
		end with
	next

	'Find number of used colours
	newfont->cols = 0
	dim as ubyte ptr imptr = image->image
	for i as integer = 0 to image->pitch * image->h - 1
		if imptr[i] > newfont->cols then newfont->cols = imptr[i]
	next

	frame_unload @image
	return newfont
end function

sub setfont (ohf_font() as integer)
	font_unload @fonts(fontPlain)
	font_unload @fonts(fontEdged)
	font_unload @fonts(fontShadow)
	fonts(fontPlain) = font_loadold1bit(cast(ubyte ptr, @ohf_font(0)))
	fonts(fontEdged) = font_create_edged(fonts(fontPlain))
	fonts(fontShadow) = font_create_shadowed(fonts(fontPlain), 1, 2)
end sub

sub set_builtin_font (ohf_font() as integer)
	font_unload @fonts(fontBuiltinPlain)
	font_unload @fonts(fontBuiltinEdged)
	fonts(fontBuiltinPlain) = font_loadold1bit(cast(ubyte ptr, @ohf_font(0)))
	fonts(fontBuiltinEdged) = font_create_edged(fonts(fontBuiltinPlain))
end sub

'NOTE: the following two functions are for the old style fonts, they will
'be removed when switching to the new system supporting unicode fonts

'These old style fonts store the type of the font in first integer (part of character
'0). The default "Latin-1.ohf" and "OHRRPGCE Default.ohf" fonts are marked as Latin 1, so
'any font derived from them will be too (ability to change the type only added in Callipygous)

function get_font_type (ohf_font() as integer) as fontTypeEnum
	if ohf_font(0) <> ftypeASCII andalso ohf_font(0) <> ftypeLatin1 then
		showerror "Unknown font type ID " & ohf_font(0)
		return ftypeASCII
	end if
	return ohf_font(0)
end function

sub set_font_type (ohf_font() as integer, ty as fontTypeEnum)
	BUG_IF(ty <> ftypeASCII andalso ty <> ftypeLatin1, "bad type " & ty)
	ohf_font(0) = ty
end sub


'==========================================================================================
'                                       BMP routines
'==========================================================================================


sub surface_export_bmp (f as string, surf as Surface Ptr, maspal() as RGBcolor)
	if surf->format = SF_32bit then
		surface_export_bmp24(f, surf)
	elseif surf->base_frame then
		frame_export_bmp8(f, surf->base_frame, maspal())
	else
		showbug "surface_export_bmp: SF_8bit not supported"
	end if
end sub

sub surface_export_bmp24 (f as string, surf as Surface Ptr)
	dim argb as RGBQUAD
	dim as integer of, y, i, skipbytes
	dim as RGBcolor ptr sptr
	dim as ubyte buf(3)

	if surf->format <> SF_32bit then
		showbug "surface_export_bmp24 got 8bit Surface"
		exit sub
	end if

	of = write_bmp_header(f, surf->width, surf->height, 24)
	if of = -1 then exit sub

	skipbytes = 4 - (surf->width * 3 mod 4)
	if skipbytes = 4 then skipbytes = 0
	sptr = surf->pColorData + (surf->height - 1) * surf->pitch
	for y = surf->height - 1 to 0 step -1
		'put is possibly the most screwed up FB builtin; the use of the fput wrapper soothes the soul
		for x as integer = 0 to surf->width - 1
			fput(of, , @sptr[x], 3)
		next
		sptr -= surf->pitch
		'pad to 4-byte boundary
		fput(of, , @buf(0), skipbytes)
	next

	close #of
end sub

sub frame_export_bmp8 (f as string, fr as Frame Ptr, maspal() as RGBcolor)
	dim argb as RGBQUAD
	dim as integer of, y, i, skipbytes
	dim as ubyte ptr sptr

	CHECK_FRAME_8BIT(fr)

	of = write_bmp_header(f, fr->w, fr->h, 8)
	if of = -1 then exit sub

	for i = 0 to 255
		argb.rgbRed = maspal(i).r
		argb.rgbGreen = maspal(i).g
		argb.rgbBlue = maspal(i).b
		put #of, , argb
	next

	skipbytes = 4 - (fr->w mod 4)
	if skipbytes = 4 then skipbytes = 0
	sptr = fr->image + (fr->h - 1) * fr->pitch
	for y = fr->h - 1 to 0 step -1
		'put is possibly the most screwed up FB builtin; the use of the fput wrapper soothes the soul
		fput(of, , sptr, fr->w) 'equivalent to "put #of, , *sptr, fr->w"
		sptr -= fr->pitch
		'write some interesting dummy data
		fput(of, , fr->image, skipbytes)
	next

	close #of
end sub

'Write a 4-bit BMP; pal should have at most 16 colours in use
sub frame_export_bmp4 (f as string, fr as Frame Ptr, maspal() as RGBcolor, pal as Palette16 ptr)
	dim argb as RGBQUAD
	dim as integer of, x, y, i, skipbytes
	dim as ubyte ptr sptr
	dim as ubyte pix

	CHECK_FRAME_8BIT(fr)

	of = write_bmp_header(f, fr->w, fr->h, 4)
	if of = -1 then exit sub

	for i = 0 to 15
		argb.rgbRed = maspal(pal->col(i)).r
		argb.rgbGreen = maspal(pal->col(i)).g
		argb.rgbBlue = maspal(pal->col(i)).b
		put #of, , argb
	next

	'Each row must be a multiple of 4 bytes
	skipbytes = 4 - ((fr->w + 1) \ 2) mod 4
	if skipbytes = 4 then skipbytes = 0

	sptr = fr->image + (fr->h - 1) * fr->pitch
	for y = fr->h - 1 to 0 step -1
		for x = 0 to fr->w - 1
			if (x and 1) = 0 then
				pix = sptr[x] shl 4
			else
				pix or= sptr[x]
				put #of, , pix
			end if
		next
		if fr->w mod 2 then
			put #of, , pix
		end if
		sptr -= fr->pitch
		'write some interesting dummy data
		fput(of, , fr->image, skipbytes)
	next

	close #of
end sub

' Generic 4/8/24-bit BMP export
sub frame_export_bmp (fname as string, fr as Frame ptr, maspal() as RGBcolor, pal as Palette16 ptr = NULL)
	if pal then
		frame_export_bmp4 fname, fr, maspal(), pal
	elseif fr->surf then
		surface_export_bmp fname, fr->surf, maspal()
	else
		frame_export_bmp8 fname, fr, maspal()
	end if
end sub

'Creates a new file and writes the bmp headers to it.
'Returns a file handle, or -1 on error.
local function write_bmp_header(filen as string, w as integer, h as integer, bitdepth as integer) as integer
	dim header as BITMAPFILEHEADER
	dim info as BITMAPINFOHEADER

	dim as integer of, imagesize, imageoff

	imagesize = ((w * bitdepth + 31) \ 32) * 4 * h
	imageoff = 54
	if bitdepth <= 8 then
		'Palette in front of the image data
		imageoff += (1 shl bitdepth) * 4
	end if

	header.bfType = 19778
	header.bfSize = imageoff + imagesize
	header.bfReserved1 = 0
	header.bfReserved2 = 0
	header.bfOffBits = imageoff

	info.biSize = 40
	info.biWidth = w
	info.biHeight = h
	info.biPlanes = 1
	info.biBitCount = bitdepth
	info.biCompression = BI_RGB
	info.biSizeImage = imagesize
	info.biXPelsPerMeter = &hB12
	info.biYPelsPerMeter = &hB12
	if bitdepth <= 8 then
		info.biClrUsed = 1 shl bitdepth
		info.biClrImportant = 1 shl bitdepth
	end if

	if openfile(filen, for_binary + access_write, of) then  'Truncate
		debugerror "write_bmp_header: couldn't open " & filen
		return -1
	end if

	put #of, , header
	put #of, , info

	return of
end function

'Open a BMP file, read its headers, and return a file handle (>= 0),
'or -1 if invalid, or -2 if unsupported.
'Only 1, 4, 8, 24, and 32 bit BMPs are accepted
'Afterwards, the file is positioned at the start of the palette, if there is one
function open_bmp_and_read_header(bmp as string, byref header as BITMAPFILEHEADER, byref info as BITMAPV3INFOHEADER, byref errmsg as string = "") as integer
	dim bf as integer
	if openfile(bmp, for_binary + access_read, bf) then
		debug "open_bmp_and_read_header: couldn't open " & bmp
		errmsg = "Couldn't open file"
		return -1
	end if

	get #bf, , header
	if header.bfType <> 19778 then
		close #bf
		errmsg = "Is not a BMP file"
		debuginfo bmp & ": " & errmsg
		return -1
	end if

	dim bisize as integer
	get #bf, , bisize
	seek #bf, seek(bf) - 4

	if biSize = 12 then
		'debuginfo "Ancient BMP2 file"
		dim info_old as BITMAPCOREHEADER
		get #bf, , info_old
		info.biSize = biSize
		info.biCompression = BI_RGB
		info.biBitCount = info_old.bcBitCount
		info.biWidth = info_old.bcWidth
		info.biHeight = info_old.bcHeight
	elseif biSize < 40 then
		close #bf
		errmsg = "Unsupported DIB header size " & biSize
		debuginfo bmp & ": " & errmsg
		return -2
	else
		'A BITMAPINFOHEADER or one of its extensions
		get #bf, , info
		if biSize >= 56 then
			'BITMAPV3INFOHEADER or one of its extensions
			'We don't support any of those extension features but none of them are important
		elseif biSize = 52 then
			'BITMAPV2INFOHEADER, alpha bitmask doesn't exist
			info.biAlphaMask = 0
		else
			'Assumably BITMAPINFOHEADER
			info.biRedMask = 0
			info.biGreenMask = 0
			info.biBlueMask = 0
			info.biAlphaMask = 0
		end if
	end if

	if info.biClrUsed <= 0 and info.biBitCount <= 8 then
		info.biClrUsed = 1 shl info.biBitCount
	end if

	'debuginfo bmp & " header size: " & bisize & " size: " & info.biWidth & "*" & info.biHeight & " bitdepth: " & info.biBitCount & " compression: " & info.biCompression & " colors: " & info.biClrUsed

	select case info.biBitCount
		case 1, 4, 8, 24, 32
		case else
			close #bf
			errmsg = "Unsupported bitdepth " & info.biBitCount
			debuginfo bmp & ": " & errmsg
			if info.biBitCount = 2 or info.biBitcount = 16 then
				return -2
			else
				'Invalid
				return -1
			end if
	end select

	if (info.biCompression = BI_RLE4 and info.biBitCount <> 4) or (info.biCompression = BI_RLE8 and info.biBitCount <> 8) then
		close #bf
		errmsg = "Invalid compression scheme " & info.biCompression & " in " & info.biBitCount & "bpp BMP"
		debuginfo bmp & ": " & errmsg
		return -1
	end if

	if info.biCompression = BI_BITFIELDS and info.biBitCount = 32 then
		'16 bit (but not 24 bit) BMPs can also use BI_BITFIELDS, but we don't support them.
		'Check whether the bitmasks are simple 8 bit masks, aside from the alpha
		'mask, which can be 0 (not present)
		if decode_bmp_bitmask(info.biRedMask) = -1 or _
		   decode_bmp_bitmask(info.biGreenMask) = -1 or _
		   decode_bmp_bitmask(info.biBlueMask) = -1 or _
		   (info.biAlphaMask <> 0 and decode_bmp_bitmask(info.biAlphaMask) = -1) then
			close #bf
			errmsg = "Unsupported BMP RGBA bitmasks " & _
			     HEX(info.biRedMask) & " " & _
			     HEX(info.biGreenMask) & " " & _
			     HEX(info.biBlueMask) & " " & _
			     HEX(info.biAlphaMask)
			debuginfo bmp & ": " & errmsg
			return -2
		end if
	elseif info.biCompression <> BI_RGB and info.biCompression <> BI_RLE4 and info.biCompression <> BI_RLE8 then
		close #bf
		errmsg = "Unsupported compression scheme " & info.biCompression & " in " & info.biBitCount & "-bit BMP"
		debuginfo bmp & ": " & errmsg
		return -2
	end if

	if info.biHeight < 0 then
		'A negative height indicates that the image is not stored upside-down. Unimplemented
		close #bf
		errmsg = "Unsupported non-flipped image"
		debuginfo bmp & ": " & errmsg
		return -2
	end if

	'Seek to palette
	'(some extra data might sit between the header and the palette only if the compression is BI_BITFIELDS
	seek #bf, 1 + sizeof(BITMAPFILEHEADER) + biSize

	return bf
end function

'Loads any supported .bmp file as a Surface, returning NULL on error.
'always_32bit: load paletted BMPs as 32 bit Surfaces instead of 8-bit ones
'(in the latter case, you have to load the palette yourself).
'The alpha channel if any is ignored
function surface_import_bmp(bmp as string, always_32bit as bool) as Surface ptr
	dim header as BITMAPFILEHEADER
	dim info as BITMAPV3INFOHEADER
	dim bf as integer

	bf = open_bmp_and_read_header(bmp, header, info)
	if bf <= -1 then return 0

	'navigate to the beginning of the bitmap data
	seek #bf, header.bfOffBits + 1

	dim ret as Surface ptr

	if info.biBitCount < 24 then
		dim paletted as Frame ptr
		paletted = frame_import_bmp_raw(bmp)  'Opens the file a second time
		if paletted then
			if always_32bit then
				dim bmppal(255) as RGBcolor
				loadbmppal(bmp, bmppal())
				' Convert it to 32bit
				ret = frame_to_surface32(paletted, bmppal())
			else
				' Keep 8-bit. We don't load the palette
				gfx_surfaceCreateFrameView(paletted, @ret)  'Increments refcount
			end if
			frame_unload @paletted
		end if
	else
		gfx_surfaceCreate(info.biWidth, info.biHeight, SF_32bit, SU_Staging, @ret)
		if info.biBitCount = 24 then
			loadbmp24(bf, ret)
		elseif info.biBitCount = 32 then
			loadbmp32(bf, ret, info)
		end if
	end if

	close #bf
	return ret
end function

function frame_import_bmp_raw(bmp as string) as Frame ptr
'load a 1-, 4- or 8-bit .BMP, ignoring the palette
	dim header as BITMAPFILEHEADER
	dim info as BITMAPV3INFOHEADER
	dim bf as integer
	dim ret as Frame ptr

	bf = open_bmp_and_read_header(bmp, header, info)
	if bf <= -1 then return 0

	if info.biBitCount > 8 then
		close #bf
		showbug "frame_import_bmp_raw should not have been called!"
		return 0
	end if

	'use header offset to get to data
	seek #bf, header.bfOffBits + 1

	ret = frame_new(info.biWidth, info.biHeight, , YES)

	if info.biBitCount = 1 then
		loadbmp1(bf, ret)
	elseif info.biBitCount = 4 then
		'call one of two loaders depending on compression
		if info.biCompression = BI_RGB then
			loadbmp4(bf, ret)
		elseif info.biCompression = BI_RLE4 then
			frame_clear(ret)
			loadbmprle4(bf, ret)
		else
			debug "frame_import_bmp_raw should not have been called, bad 4-bit compression"
		end if
	else
		if info.biCompression = BI_RGB then
			loadbmp8(bf, ret)
		elseif info.biCompression = BI_RLE8 then
			frame_clear(ret)
			loadbmprle8(bf, ret)
		else
			debug "frame_import_bmp_raw should not have been called, bad 8-bit compression"
		end if
	end if

	close #bf
	return ret
end function

'Given a mask with 8 consecutive bits such as &hff00 returns the number of zero
'bits to the right of the bits. Returns -1 if the mask isn't of this form.
local function decode_bmp_bitmask(mask as uint32) as integer
	for shift as integer = 0 to 24
		if mask shr shift = &hFF then
			return shift
		end if
	next
	return -1
end function

'Takes an open file handle pointing at start of pixel data and an already sized Surface to load into
local sub loadbmp32(bf as integer, surf as Surface ptr, infohd as BITMAPV3INFOHEADER)
	dim bitspix as uint32
	dim quadpix as RGBQUAD
	dim sptr as RGBcolor ptr
	dim tempcol as RGBcolor
	dim as integer rshift, gshift, bshift, ashift
	tempcol.a = 255  'Opaque

	if infohd.biCompression = BI_BITFIELDS then
		' The bitmasks have already been verified to be supported, except
		' alpha might be missing
		rshift = decode_bmp_bitmask(infohd.biRedMask)
		gshift = decode_bmp_bitmask(infohd.biGreenMask)
		bshift = decode_bmp_bitmask(infohd.biBlueMask)
		ashift = decode_bmp_bitmask(infohd.biAlphaMask)
	end if

	for y as integer = surf->height - 1 to 0 step -1
		sptr = surf->pColorData + y * surf->pitch
		for x as integer = 0 to surf->width - 1
			if infohd.biCompression = BI_BITFIELDS then
				get #bf, , bitspix
				tempcol.r = bitspix shr rshift
				tempcol.g = bitspix shr gshift
				tempcol.b = bitspix shr bshift
				if ashift <> -1 then
					tempcol.a = bitspix shr ashift
				end if
				*sptr = tempcol
			else
				'Layout of RGBQUAD and RGBcolor are the same
				get #bf, , quadpix
				*sptr = *cast(RGBcolor ptr, @quadpix)
			end if
			sptr += 1
		next
	next
end sub

'Takes an open file handle pointing at start of pixel data and an already sized Surface to load into
local sub loadbmp24(bf as integer, surf as Surface ptr)
	dim pix as RGBTRIPLE
	dim ub as ubyte
	dim sptr as RGBcolor ptr
	dim pad as integer

	'data lines are padded to 32-bit boundaries
	pad = 4 - ((surf->width * 3) mod 4)
	if pad = 4 then	pad = 0

	for y as integer = surf->height - 1 to 0 step -1
		sptr = surf->pColorData + y * surf->pitch
		for x as integer = 0 to surf->width - 1
			get #bf, , pix
			sptr->r = pix.rgbtRed
			sptr->g = pix.rgbtGreen
			sptr->b = pix.rgbtBlue
			sptr->a = 255
			sptr += 1
		next
		'padding to dword boundary
		for w as integer = 0 to pad-1
			get #bf, , ub
		next
	next
end sub

local sub loadbmp8(bf as integer, fr as Frame ptr)
'takes an open file handle and an already size Frame pointer, should only be called within loadbmp
	dim ub as ubyte
	dim as integer w, h
	dim sptr as ubyte ptr
	dim pad as integer

	pad = 4 - (fr->w mod 4)
	if pad = 4 then	pad = 0

	for h = fr->h - 1 to 0 step -1
		sptr = fr->image + h * fr->pitch
		for w = 0 to fr->w - 1
			'read the data
			get #bf, , ub
			*sptr = ub
			sptr += 1
		next

		'padding to dword boundary
		for w = 0 to pad-1
			get #bf, , ub
		next
	next
end sub

local sub loadbmp4(bf as integer, fr as Frame ptr)
'takes an open file handle and an already size Frame pointer, should only be called within loadbmp
	dim ub as ubyte
	dim as integer w, h
	dim sptr as ubyte ptr
	dim pad as integer

	dim numbytes as integer = (fr->w + 1) \ 2  'per line
	pad = 4 - (numbytes mod 4)
	if pad = 4 then pad = 0

	for h = fr->h - 1 to 0 step -1
		sptr = fr->image + h * fr->pitch
		for w = 0 to fr->w - 1
			if (w and 1) = 0 then
				'read the data
				get #bf, , ub
				*sptr = (ub and &hf0) shr 4
			else
				'2nd nybble in byte
				*sptr = ub and &h0f
			end if
			sptr += 1
		next

		'padding to dword boundary
		for w = 0 to pad - 1
			get #bf, , ub
		next
	next
end sub

local sub loadbmprle4(bf as integer, fr as Frame ptr)
'takes an open file handle and an already size Frame pointer, should only be called within loadbmp
	dim pix as ubyte
	dim ub as ubyte
	dim as integer w, h
	dim i as integer
	dim as ubyte bval, v1, v2

	w = 0
	h = fr->h - 1

	'read bytes until we're done
	while not eof(bf)
		'get command byte
		get #bf, , ub
		select case ub
			case 0	'special, check next byte
				get #bf, , ub
				select case ub
					case 0		'end of line
						w = 0
						h -= 1
					case 1		'end of bitmap
						exit while
					case 2 		'delta (how can this ever be used?)
						get #bf, , ub
						w = w + ub
						get #bf, , ub
						h = h + ub
					case else	'absolute mode
						for i = 1 to ub
							if i and 1 then
								get #bf, , pix
								bval = (pix and &hf0) shr 4
							else
								bval = pix and &h0f
							end if
							putpixel(fr, w, h, bval)
							w += 1
						next
						if (ub mod 4 = 1) or (ub mod 4 = 2) then
							get #bf, , ub 'pad to word bound
						end if
				end select
			case else	'run-length
				get #bf, , pix	'2 colours
				v1 = (pix and &hf0) shr 4
				v2 = pix and &h0f

				for i = 1 to ub
					if i and 1 then
						bval = v1
					else
						bval = v2
					end if
					putpixel(fr, w, h, bval)
					w += 1
				next
		end select
	wend

end sub

local sub loadbmprle8(bf as integer, fr as Frame ptr)
'takes an open file handle and an already size Frame pointer, should only be called within loadbmp
	dim pix as ubyte
	dim ub as ubyte
	dim as integer w, h
	dim i as integer
	dim as ubyte bval

	w = 0
	h = fr->h - 1

	'read bytes until we're done
	while not eof(bf)
		'get command byte
		get #bf, , ub
		select case ub
			case 0	'special, check next byte
				get #bf, , ub
				select case ub
					case 0		'end of line
						w = 0
						h -= 1
					case 1		'end of bitmap
						exit while
					case 2 		'delta (how can this ever be used?)
						get #bf, , ub
						w = w + ub
						get #bf, , ub
						h = h + ub
					case else	'absolute mode
						for i = 1 to ub
							get #bf, , pix
							putpixel(fr, w, h, pix)
							w += 1
						next
						if ub mod 2 then
							get #bf, , ub 'pad to word boundary
						end if
				end select
			case else	'run-length
				get #bf, , pix

				for i = 1 to ub
					putpixel(fr, w, h, pix)
					w += 1
				next
		end select
	wend

end sub

local sub loadbmp1(bf as integer, fr as Frame ptr)
'takes an open file handle and an already sized Frame pointer, should only be called within loadbmp
	dim ub as ubyte
	dim as integer w, h
	dim sptr as ubyte ptr
	dim pad as integer

	dim numbytes as integer = (fr->w + 7) \ 8  'per line
	pad = 4 - (numbytes mod 4)
	if pad = 4 then	pad = 0

	for h = fr->h - 1 to 0 step -1
		sptr = fr->image + h * fr->pitch
		for w = 0 to fr->w - 1
			if (w mod 8) = 0 then
				get #bf, , ub
			end if
			*sptr = ub shr 7
			ub = ub shl 1
			sptr += 1
		next

		'padding to dword boundary
		for w = 0 to pad - 1
			get #bf, , ub
		next
	next
end sub

'Loads the palette of a 1-bit, 4-bit or 8-bit bmp into pal().
'Returns the number of bits, or 0 if the file can't be read.
'Ignores alpha channel
function loadbmppal (f as string, pal() as RGBcolor) as integer
	dim header as BITMAPFILEHEADER
	dim info as BITMAPV3INFOHEADER
	dim col3 as RGBTRIPLE
	dim col4 as RGBQUAD
	dim bf as integer
	dim i as integer

	bf = open_bmp_and_read_header(f, header, info)
	if bf <= -1 then return 0

	for i = 0 to ubound(pal)
		pal(i).r = 0
		pal(i).g = 0
		pal(i).b = 0
	next

	'debug "loadbmppal(" & f & "): table at " & (seek(bf) - 1) & " = " & hex(seek(bf) - 1)
	if info.biBitCount <= 8 then
		for i = 0 to (1 shl info.biBitCount) - 1
			if info.biSize = 12 then  'BITMAPCOREHEADER
				get #bf, , col3
				pal(i).r = col3.rgbtRed
				pal(i).g = col3.rgbtGreen
				pal(i).b = col3.rgbtBlue
				pal(i).a = 255
			else
				get #bf, , col4
				pal(i).r = col4.rgbRed
				pal(i).g = col4.rgbGreen
				pal(i).b = col4.rgbBlue
				' Some BMP documentation states col4.rgbReserved "must be zero"
				pal(i).a = 255
			end if
		next
	else
		debugc errBug, "loadbmppal shouldn't have been called!"
	end if
	close #bf
	return info.biBitCount
end function

'Returns 0 if invalid, otherwise fills 'info' and returns 1 if valid but unsupported, 2 if supported
function bmpinfo (f as string, byref info as BITMAPV3INFOHEADER, byref errmsg as string = "") as integer
	dim header as BITMAPFILEHEADER
	dim bf as integer

	bf = open_bmp_and_read_header(f, header, info, errmsg)
	if bf = -1 then return 0
	if bf = -2 then return 1
	close #bf
	return 2
end function

sub bmpinfo (filename as string, byref iminfo as ImageFileInfo)
	iminfo.imagetype = imBMP
	dim bmpd as BitmapV3InfoHeader
	dim support as integer = bmpinfo(filename, bmpd, iminfo.error)
	iminfo.supported = (support = 2)
	iminfo.valid = (support >= 1)
	iminfo.size.w = bmpd.biWidth
	iminfo.size.h = bmpd.biHeight
	iminfo.bpp = bmpd.biBitCount
	iminfo.paletted = (iminfo.bpp <= 8)
	'It's also possible for the palette to define an alpha for each color
	iminfo.alpha = (iminfo.bpp = 32)
end sub


'==========================================================================================
'                                          PNG
'==========================================================================================

private function lodepngerr(errornum as integer, funcname as zstring ptr) as integer
	if errornum then
		debuginfo *funcname & ": error " & errornum & " " & *lodepng_error_text(errornum)
	end if
	return errornum
end function

#define PNGCHKERR(funccall)  lodepngerr(funccall, @__FUNCTION__)

sub pnginfo (filename as string, byref iminfo as ImageFileInfo)
	iminfo.imagetype = imPNG

	'lodepng_inspect only inspects the PNG header, which is always 33 bytes. Load into memory
	dim header(32) as byte
	' if PNGCHKERR(lodepng_buffer_file(@header(0), 33, strptr(filename))) then
	' 	exit sub
	' end if
	dim fh as integer
	if openfile(filename, for_binary + access_read, fh) then
		debug "Couldn't open " & filename
		exit sub
	end if
	get #fh, , header()
	close fh

	dim state as LodePNGState
	dim errornum as integer
	errornum = lodepng_inspect(@iminfo.size.w, @iminfo.size.h, @state, @header(0), 33)
	if PNGCHKERR(errornum) then
		iminfo.error = *lodepng_error_text(errornum)
		exit sub
	end if
	iminfo.valid = YES
	iminfo.supported = YES

	'Look at color mode info.
	'lodepng_inspect doesn't read other chunks, including PLTE, so
	'can't tell us how large the palette is. It can tell us the color key though
	'EDIT: lodepng_inspect_chunk has been added, which would now allow loading the palette
	dim cinfo as LodePNGColorMode ptr = @state.info_png.color
	iminfo.bpp = lodepng_get_bpp(cinfo)
	if lodepng_is_palette_type(cinfo) then
		'Excludes non-paletted 8-bit greyscale images
		iminfo.paletted = YES
	end if
	if lodepng_can_have_alpha(cinfo) then 'lodepng_is_alpha_type(cinfo) then
		'Image type with wholy or partially transparent pixels
		'(Either RGBA or grey+alpha or has a colorkey, or has a palette with alpha)
		iminfo.alpha = YES
	end if

	lodepng_state_cleanup(@state)
end sub

'Import a paletted PNG as a Frame, and load the palette into pal().
'Returns NULL if not paletted.
function frame_import_paletted_png(filename as string, pal() as RGBcolor) as Frame ptr
	dim ret as Frame ptr
	dim pixelbuf as byte ptr
	dim size as XYPair

	dim filebuf as byte ptr
	dim filebufsize as size_t
	log_openfile filename
	if PNGCHKERR(lodepng_load_file(@filebuf, @filebufsize, strptr(filename))) then
		deallocate filebuf
		return NULL
	end if

	dim state as LodePNGState
	lodepng_state_init(@state)
	' The type of image we want to read
	state.info_raw.colortype = LCT_PALETTE
	state.info_raw.bitdepth = 8

	if PNGCHKERR(lodepng_decode(@pixelbuf, @size.w, @size.h, @state, filebuf, filebufsize)) = 0 then
		ret = frame_new(size.w, size.h)
		if ret then
			memcpy(ret->image, pixelbuf, size.w * size.h)
		end if

		with state.info_png.color
			'redim pal(.palettesize - 1)
			for cidx as integer = 0 to .palettesize - 1
				'Oddly although PNG supports 1..16 bit color depth, palettes are always 8-bit
				pal(cidx).r = .palette[cidx * 4 + 0]
				pal(cidx).g = .palette[cidx * 4 + 1]
				pal(cidx).b = .palette[cidx * 4 + 2]
				pal(cidx).a = .palette[cidx * 4 + 3]  '255=opaque
			next
			for cidx as integer = .palettesize to ubound(pal)
				pal(cidx).col = 0
			next
		end with
	end if

	deallocate filebuf
	deallocate pixelbuf
	lodepng_state_cleanup(@state)

	return ret
end function

'Loads any supported .png file as a Surface, returning NULL on error.
'always_32bit: load paletted PNGs as 32 bit Surfaces instead of 8-bit ones
'(in the latter case, you have to load the palette yourself).
'The alpha channel if any is ignored, as is the palette if always_32bit=NO.
function surface_import_png(filename as string, always_32bit as bool) as Surface ptr

	'Calling pnginfo, which means we open the file twice, is a lot less work than this...
	/'
	dim bufsize as size_t
	'Read the file into memory
	if PNGCHKERR(lodepng_load_file(@buf, @bufsize, strptr(filename)) then return NULL
	'etc, see lodepng_decode_memory() for other steps
	'/

	dim iminfo as ImageFileInfo
	pnginfo filename, iminfo
	if iminfo.supported = NO then return NULL

	dim ret as Surface ptr

	if iminfo.paletted = NO orelse always_32bit then
		dim buf as byte ptr
		dim size as XYPair
		log_openfile filename
		if PNGCHKERR(lodepng_decode_file(@buf, @size.w, @size.h, strptr(filename), LCT_RGB, 8)) then
			return NULL
		end if

		'Convert RGB to BGRA
		ret = surface32_from_pixels(buf, size.w, size.h, PIXFMT_RGB)
		deallocate buf
	else
		'The palette is ignored
		dim pal(255) as RGBColor
		dim fr as Frame ptr = frame_import_paletted_png(filename, pal())
		gfx_surfaceCreateFrameView(fr, @ret)  'Increments refcount
		frame_unload @fr
	end if

	return ret
end function

'Write a Surface to a .png file.
'8-bit Surfaces:  preserves palette indices. pal is optional.
'32-bit Surfaces: masterpal() and pal and the alpha channel are ignored.
'                 The output .png will be paletted if the input has <= 256 colors.
'compress is 0, 1, 2, higher values are slower but compress better.
function surface_export_png(surf as Surface ptr, filename as string, masterpal() as RGBcolor, pal as Palette16 ptr = NULL, compress as integer = 1) as bool
	dim filebuf as byte ptr
	dim filebufsize as size_t
	dim pixelbuf as byte ptr

	dim state as LodePNGState
	lodepng_state_init(@state)
	state.info_raw.bitdepth = 8
	state.info_png.color.bitdepth = 8

	'LodePNG's default is 2048. 8192 and above are much slower, because they're size-optimised
	state.encoder.zlibsettings.windowsize = iif(compress <= 0, 512, iif(compress >= 2, 32768, 4096))

	if surf->format = SF_8bit then
		state.info_raw.colortype = LCT_PALETTE
		state.info_png.color.colortype = LCT_PALETTE

		' When writing a paletted image, to preserve palette indices for
		' re-import, disallow LodePNG from shuffling the palette to get
		' a lower bitdepth (eg writing as monochrome images as 2-bit)
		state.encoder.auto_convert = 0

		lodepng_palette_clear(@state.info_png.color)
		dim ncols as integer = iif(pal, pal->numcolors, 256)
		for cidx as integer = 0 to ncols - 1
			with masterpal(iif(pal, pal->col(cidx), cidx))
				lodepng_palette_add(@state.info_png.color, .r, .g, .b, 255)
			end with
		next
		lodepng_color_mode_copy(@state.info_raw, @state.info_png.color)

		if ncols <= 16 then
			state.info_png.color.bitdepth = 4  'Only for better compression
		end if

		pixelbuf = surf->pPaletteData
	else
		state.info_raw.colortype = LCT_RGB  'LCT_RGBA
		'state.info_png.color.colortype unspecified, will be auto-selected

		'BGRA to RGB
		pixelbuf = surface32_to_pixels(surf, PIXFMT_RGB)
	end if

	'Encode and write to file
	lodepng_encode(@filebuf, @filebufsize, pixelbuf, surf->width, surf->height, @state)

	if state.error = 0 then
		state.error = lodepng_save_file(filebuf, filebufsize, strptr(filename))
	end if
	PNGCHKERR(state.error)
	dim ret as bool = (state.error = 0)

	'? (surf->width * surf->height) & " pix in " & CINT(1e6 * time) & !"us  \twinsize = " & state.encoder.zlibsettings.windowsize & "  fsize " & filebufsize

	'Cleanup
	lodepng_state_cleanup(@state)
	deallocate filebuf
	if surf->format = SF_32bit then
		deallocate pixelbuf
	end if

	return ret
end function

'Write a Frame to a paletted .png file, preserving palette indices. pal is optional.
'compress: amount of compression (affects speed), 0, 1 or 2
function frame_export_png(fr as Frame ptr, filename as string, masterpal() as RGBcolor, pal as Palette16 ptr = NULL, compress as integer = 1) as bool
	dim surf as Surface ptr
	if gfx_surfaceCreateFrameView(fr, @surf) then return NO

	dim ret as bool
	ret = surface_export_png(surf, filename, masterpal(), pal, compress)

	gfx_surfaceDestroy(@surf)
	return ret
end function

#undef PNGCHKERR


'==========================================================================================
'                                         JPEG
'==========================================================================================


'Caches result of find_helper_app("jpegtran")
function get_jpegtran() as string
	static checked as bool
	static ret as string

	if checked = NO then
		checked = YES
		'First check without attempting to download, so that we can show
		'a helpful message before the download prompt
		ret = find_helper_app("jpegtran")
		if len(ret) = 0 then
			visible_debug "Can only read progressive JPEGs using the jpegtran program (part of libjpeg-progs)"
			ret = find_helper_app("jpegtran", YES, "http://jpegclub.org/jpegtran.zip")
		end if
	end if
	return ret
end function

sub jpeginfo (filename as string, byref iminfo as ImageFileInfo)
	iminfo.imagetype = imJPEG

	dim jpeg as ujImage
	jpeg = ujCreate()
	ujDisableDecoding(jpeg)
	log_openfile filename
	ujDecodeFile(jpeg, strptr(filename))
	dim errcode as integer = ujGetError()

	dim bad as bool = (ujIsValid(jpeg) = 0)
	if errcode = UJ_PROGRESSIVE then
		if len(get_jpegtran()) then bad = NO
	end if

	if bad then  'invalid or not supported
		if errcode = UJ_NO_JPEG then
			iminfo.error = "Not a JPEG file"
		elseif errcode = UJ_IO_ERROR then
			iminfo.error = "Could not read file, IO error"
		elseif errcode = UJ_PROGRESSIVE then
			iminfo.valid = YES
			iminfo.error = "Progressive, jpegtran not installed"
		else
			if errcode = UJ_UNSUPPORTED or errcode >= UJ_UNKNOWN_SEGM then
				'Other unsupported features or extensions
				iminfo.valid = YES
			end if
			iminfo.error = "Error code " & hex(errcode)
		end if

	else
		if errcode = UJ_PROGRESSIVE then
			iminfo.info = "Progressive"
		end if
		iminfo.valid = YES
		iminfo.supported = YES
		iminfo.size.w = ujGetWidth(jpeg)
		iminfo.size.h = ujGetHeight(jpeg)
		iminfo.bpp = iif(ujIsColor(jpeg), 24, 8)
	end if

	ujFree(jpeg)
end sub

'Attempt to losslessly turn a progressive or arithmetically coded JPEG to a
'baseline JPEG which can be read using uJPEG, using the jpegtran tool, which is
'part of the libjpeg-progs suite of tools.
'jpegtran will strip out metadata blocks, except for comments, unless "-copy all" is given.
'Returns a temp filename or "".
function jpeg_convert_to_baseline(filename as string) as string
	debuginfo "jpeg_convert_to_baseline"
	dim jpegtran as string = get_jpegtran()
	if len(jpegtran) = 0 then return ""

	dim outfile as string = tmpdir & "jpegtran_" & trimpath(filename)
	safe_shell jpegtran & " -outfile " & escape_filename(outfile) & " " & escape_filename(filename)
	if not isfile(outfile) then
		debug "jpegtran failed"
		return ""
	end if

	return outfile
end function

'Loads any supported JPEG file as a Surface, returning NULL on error.
function surface_import_jpeg(filename as string) as Surface ptr
	dim jpeg as ujImage
	jpeg = ujCreate()
	log_openfile filename
	ujDecodeFile(jpeg, strptr(filename))

	if ujGetError() = UJ_PROGRESSIVE then
		'uJPEG doesn't support progressive JPEGs, but they can be
		'losslessly translated to baseline JPEGs
		filename = jpeg_convert_to_baseline(filename)
		if len(filename) = 0 then return NULL
		ujDecodeFile(jpeg, strptr(filename))
		killfile filename
	end if

	dim errcode as integer = ujGetError()
	if errcode <> UJ_OK orelse ujIsValid(jpeg) = 0 then
		debug "ujDecodeFile error " & errcode & " in " & filename
		ujFree(jpeg)
		return NULL
	end if

	dim pixformat as PixelFormat
	pixformat = iif(ujIsColor(jpeg), PIXFMT_RGB, PIXFMT_GREY)
	dim size as XYPair = (ujGetWidth(jpeg), ujGetHeight(jpeg))

	dim ret as Surface ptr

	dim buf as byte ptr
	buf = ujGetImage(jpeg, NULL)
	if buf = NULL then
		debug "ujGetImage error " & ujGetError() & " importing " & filename
	else
		'Need to convert RGB to our BGRA
		ret = surface32_from_pixels(buf, size.w, size.h, pixformat)
	end if

	ujFree(jpeg)

	return ret
end function

function surface_export_jpeg(surf as Surface ptr, filename as string, quality as integer = 95) as bool
	BUG_IF(surf->format = SF_8bit, "8-bit surfaces not supported", NO) 'TODO. Use frame_export_jpeg instead.
	BUG_IF(surf->width <> surf->pitch, "Unsupported image pitch", NO)

	dim pixelbuf as byte ptr
	'BGRA to RGB
	pixelbuf = surface32_to_pixels(surf, PIXFMT_RGB)

	dim ret as bool = YES
	if jo_write_jpg(strptr(filename), pixelbuf, surf->width, surf->height, 3, quality) = 0 then
		'Only other possible error condition is a zero size image or null ptr
		debug "Couldn't write to " & filename
		ret = NO
	end if

	deallocate pixelbuf
	return ret
end function

'Write a Frame to a paletted .png file, preserving palette indices. pal is optional.
function frame_export_jpeg(fr as Frame ptr, filename as string, masterpal() as RGBcolor, pal as Palette16 ptr = NULL, quality as integer = 95) as bool
	dim surf as Surface ptr
	surf = frame_to_surface32(fr, masterpal(), pal)
	if surf = NULL then return NO

	dim ret as bool
	ret = surface_export_jpeg(surf, filename, quality)

	gfx_surfaceDestroy(@surf)
	return ret
end function


'==========================================================================================
'                               Generic image file interface
'==========================================================================================


'Indexed by ImageFileTypes
dim shared image_type_strings(...) as zstring ptr = {@"Invalid", @"BMP", @"GIF", @"PNG", @"JPEG"}

function image_file_type (filename as string) as ImageFileTypes
	select case lcase(justextension(filename))
		case "bmp" : return imBMP
		case "png" : return imPNG
		case "gif" : return imGIF
		case "jpg", "jpeg" : return imJPEG
	end select
	return imUnknown
end function

function image_read_info (filename as string) as ImageFileInfo
	dim ret as ImageFileInfo

	ret.imagetype = image_file_type(filename)
	if ret.imagetype = imBMP then
		bmpinfo filename, ret
	elseif ret.imagetype = imPNG then
		pnginfo filename, ret
	elseif ret.imagetype = imJPEG then
		jpeginfo filename, ret
	else
		ret.error = "File extension not recognised"   'Shouldn't happen
	end if

	ret.imagetype_name = *image_type_strings(ret.imagetype)

	dim info as string
	if ret.valid = NO then
		info = "Invalid "
	elseif ret.supported = NO then
		info = "Unsupported "
	end if
	info &= ret.imagetype_name

	if ret.supported orelse ret.size.w > 0 then
		info &= ", " & ret.size.wh & " pixels, " & ret.bpp & "-bit color"
		if ret.size.w > maxFrameSize orelse ret.size.h > maxFrameSize then
			ret.supported = NO
			if ret.error = "" then ret.error = "Too large!"
		end if
	end if
	if ret.valid then
		if ret.alpha then info &= ", alpha"
		if ret.paletted then info &= ", paletted"
	end if
	if len(ret.info) then info &= ", " & ret.info
	ret.info = info

	return ret
end function

'Loads the palette of a <= 8-bit image file into pal().
'Returns the number of bits, or 0 if the file can't be read or isn't paletted.
function image_load_palette (filename as string, pal() as RGBcolor) as integer
	select case image_file_type(filename)
		case imBMP
			return loadbmppal(filename, pal())
		case imPNG
			'<s>LodePNG doesn't have a way to read just the palette without the image</s>
			'EDIT: loadpng_inspect_chunk was added, which can now do that. But it doesn't
			'look like it's worth the time to switch to that.
			dim fr as Frame ptr
			fr = frame_import_paletted_png(filename, pal())
			if fr = 0 then
				return 0
			else
				frame_unload @fr
				return 8  'Don't care
			end if
		case else
			debug "load_image_palette: Unrecognised: " & filename
			return 0
	end select
end function

'Loads any supported image file as a Surface, returning NULL on error.
'always_32bit: load paletted images as 32 bit Surfaces instead of 8-bit ones
'(in the latter case, you have to load the palette yourself).
'The alpha channel if any is ignored
function image_import_as_surface(filename as string, always_32bit as bool) as Surface ptr
	select case image_file_type(filename)
		case imBMP
			return surface_import_bmp(filename, always_32bit)
		case imPNG
			return surface_import_png(filename, always_32bit)
		case imJPEG
			return surface_import_jpeg(filename)
		case else
			debug "image_import_as_surface: Unrecognised: " & filename
			return NULL
	end select
end function

'Loads a 24/32-bit image as 8-bit Frame.
'pal() is an output if options.compute_palette=YES, otherwise an input.
'See quantize_surface() for full documentation.
'It doesn't make sense to call this on paletted images, as it's unnecessarily very slow.
'If there is an alpha channel, fully transparent pixels are mapped to index 0.
function image_import_as_frame_quantized(filename as string, pal() as RGBcolor, options as QuantizeOptions = TYPE(0, -1)) as Frame ptr
	dim surf as Surface ptr
	surf = image_import_as_surface(filename, YES)
	if surf = NULL then return NULL
	return quantize_surface(surf, pal(), options)
end function

'Load a paletted image as a Frame, and load the palette into pal(), a length-256 array.
'An error to call for non-paletted images.
function image_import_as_frame_paletted (filename as string, pal() as RGBColor) as Frame ptr
	select case image_file_type(filename)
		case imBMP
			dim ret as Frame ptr
			ret = frame_import_bmp_raw(filename)
			if ret then
				loadbmppal(filename, pal())
			end if
			return ret
		case imPNG
			return frame_import_paletted_png(filename, pal())
		case else
			showerror "image_import_as_frame_paletted: invalid image type " & filename
			return NULL
	end select
end function

'Load a paletted image as a Frame and a Palette16 mapped into master() using nearest-matching; returns success.
'Deletes any existing pointers in ret.
'Unlike image_import_as_frame_8bit() this preserves the original colour indices.
'ret.pal will have at least 16 colors, possibly up to 256, but enough to cover all pixel values,
'but is NULL when the image is unpaletted.
'defaultpal is used for breaking nearest-match ties.
'Alpha values in image palette ignored.
function image_import_as_frame_and_palette16 (byref ret as GraphicPair, filename as string, defaultpal as Palette16 ptr = NULL) as bool
	unload_sprite_and_pal ret

	dim info as ImageFileInfo
	info = image_read_info(filename)
	if info.supported = NO then return NULL ' Unreadable, invalid, or unsupported

	if info.paletted then
		dim imgpal(255) as RGBColor
		ret.sprite = image_import_as_frame_paletted(filename, imgpal())
		if ret.sprite = NULL then return NO

		'Map from impsprite colors to master pal indices
		dim ncols as integer = 1 shl info.bpp
		if ncols < 16 then ncols = 16
		dim palmapping(ncols - 1) as integer
		if defaultpal then
			'Put color index hints in palmapping(), which are used if they are an exact match.
			for i as integer = 0 TO small(ncols, defaultpal->numcolors) - 1
				palmapping(i) = defaultpal->col(i)
			next
		end if
		find_palette_mapping(imgpal(), curmasterpal(), palmapping())

		ret.pal = Palette16_new_from_indices(palmapping())
	else
		ret.sprite = image_import_as_frame_quantized(filename, curmasterpal())
		if ret.sprite = NULL then return NO
		'Leave ret.pal blank, we don't need it
		'ret.pal = Palette16_new_identity(256)
	end if
	return YES
end function

'Load a paletted image as a Frame, ignoring the palette. An error to call for non-paletted images.
function image_import_as_frame_raw (filename as string) as Frame ptr
	dim pal(255) as RGBColor
	return image_import_as_frame_paletted(filename, pal())
end function

'Loads any image as an (optionally transparent) 8-bit Frame (ie. with no Palette16),
'remapped to the given master palette. Returns NULL on error.
'Nonpaletted images will have RGB pixels equal to the 'transparency' color (transparency.a should be 0)
'mapped to masterpal() index 0 (by default nothing); 'keep_col0' is ignored.
'Also, in images with an alpha channel, fully transparent pixels are mapped to index 0.
'Paletted images get palette index 0 mapped to color 0 if 'keep_col0' is true,
'otherwise they have no color 0 pixels; 'transparency' is ignored.
function image_import_as_frame_8bit(filename as string, masterpal() as RGBcolor, keep_col0 as bool = YES, byval transparency as RGBcolor = TYPE(-1)) as Frame ptr
	dim info as ImageFileInfo
	info = image_read_info(filename)
	if info.supported = NO then return NULL ' Unreadable, invalid, or unsupported

	if info.paletted then
		dim ret as Frame ptr
		dim imgpal(255) as RGBColor
		ret = image_import_as_frame_paletted(filename, imgpal())
		if ret = NULL then return NULL

		' Drop the palette, remapping to the master palette
		' (Can't use frame_draw, since we have an array instead of a Palette16)
		dim palindices(255) as integer
		find_palette_mapping(imgpal(), masterpal(), palindices(), 1)
		if keep_col0 then
			palindices(0) = 0
		end if
		remap_to_palette ret, palindices()

		return ret
	else
		dim options as QuantizeOptions = (1, transparency)
		return image_import_as_frame_quantized(filename, masterpal(), options)
	end if
end function

'Returns a Frame backed by a 32-bit Surface
function image_import_as_frame_32bit(filename as string) as Frame ptr
	dim surf as Surface ptr
	surf = image_import_as_surface(filename, YES)
	if surf = NULL then return NULL
	dim ret as Frame ptr
	ret = frame_with_surface(surf)
	gfx_surfaceDestroy(@surf)
	return ret
end function

'Output file format is determined from the filename.
sub frame_export_image (fr as Frame ptr, filename as string, masterpal() as RGBcolor, pal as Palette16 ptr = NULL)
	select case image_file_type(filename)
		case imBMP
			frame_export_bmp filename, fr, masterpal(), pal
		case imPNG
			frame_export_png fr, filename, masterpal(), pal
		case imGIF
			frame_export_gif fr, filename, masterpal(), pal, NO  'transparent = NO
		case imJPEG
			frame_export_jpeg fr, filename, masterpal(), pal
		'Update load_screenshot_settings when adding more formats
		case else
			debug "Can't write image: unknown or unsupported file extension: " & filename
	end select
end sub

'Export a 32-bit Surface. Output file format is determined from the filename.
sub surface_export_image (surf as Surface ptr, filename as string)
	select case image_file_type(filename)
		case imBMP
			'Supports only 8bit Surfaces with backing Frame
			surface_export_bmp filename, surf, curmasterpal()
		case imPNG
			'Supports 8bit Surfaces
			surface_export_png surf, filename, curmasterpal() ' masterpal(), pal
		case imGIF
			'Doesn't support 8bit
			surface_export_gif surf, filename
		case imJPEG
			'Doesn't support 8bit
			surface_export_jpeg surf, filename
		case else
			debug "Can't write image: unknown or unsupported file extension: " & filename
	end select
end sub

'==========================================================================================
'                                   Image quantization
'==========================================================================================

'Returns a non-negative integer which is 0 if both colors in a color table are the same
function color_distance(pal() as RGBcolor, index1 as integer, index2 as integer) as integer
	with pal(index1)
		dim as integer rdif, bdif, gdif, rmean
		rmean = (.r + pal(index2).r) shr 1
		rdif = .r - pal(index2).r
		gdif = .g - pal(index2).g
		bdif = .b - pal(index2).b
		'Formula taken from https://www.compuphase.com/cmetric.htm
		return (((512 + rmean)*rdif*rdif) shr 8) + 4*gdif*gdif + (((767-rmean)*bdif*bdif) shr 8)
	end with
end function

function nearcolor(pal() as RGBcolor, red as integer, green as integer, blue as integer, firstindex as integer = 0, indexhint as integer = -1, avoidcol as integer = -1) as ubyte
'Figure out nearest palette colour in range [firstindex..255] using an approximate perceptual color distance
'A perfect match against pal(indexhint) is tried first
'Never returns avoidcol or any other color the same as it.
	dim as integer i, diff, best, save, rdif, bdif, gdif, cappedred, rmean

	if indexhint > -1 and indexhint <= ubound(pal) and indexhint >= firstindex then
		with pal(indexhint)
			if red = .r and green = .g and blue = .b then return indexhint
		end with
	end if

	if red < 0 then
		cappedred = 0
	elseif red > 255 then
		cappedred = 255
	else
		cappedred = red
	end if

	dim avoidrgb as RGBcolor
	if avoidcol > -1 then avoidrgb = pal(avoidcol)
	best = 1000000
	save = 0
	for i = firstindex to 255
		with pal(i)
			if avoidcol > -1 andalso .col = avoidrgb.col then continue for
			rmean = (cappedred + .r) shr 1
			rdif = red - .r
			gdif = green - .g
			bdif = blue - .b
		end with
		'Formula taken from https://www.compuphase.com/cmetric.htm
		'It is an interpolation between
		'diff = 3*rdif*rdif + 4*gdif*gdif + 2*bdif*bdif
		' and
		'diff = 2*rdif*rdif + 4*gdif*gdif + 3*bdif*bdif
		diff = (((512 + rmean)*rdif*rdif) shr 8) + 4*gdif*gdif + (((767-rmean)*bdif*bdif) shr 8)

		if diff = 0 then
			'early out on direct hit
			save = i
			exit for
		end if
		if diff < best then
			save = i
			best = diff
		end if
	next

	return save
end function

function nearcolor(pal() as RGBcolor, index as integer, firstindex as integer = 0) as ubyte
	with pal(index)
		return nearcolor(pal(), .r, .g, .b, firstindex)
	end with
end function

extern "C"

function nearcolor_master(byval col as RGBcolor, firstindex as integer = 0) as ubyte
	return nearcolor(curmasterpal(), col.r, col.g, col.b, firstindex)
end function

'Find the nearest color in the current palette (curmasterpal(), set by setpal). Alpha ignored.
'This may produce slightly worse results than nearcolor because it uses a slightly different
'color distance function. However it's over 10x faster. (Try nearcolor_faster if you need more.)
'This never returns color 0 (firstindex=1).
function nearcolor_fast(byval col as RGBcolor) as ubyte
	return query_KDTree(nearcolor_kdtree, col)
end function

end extern

'Version which supports out-of-bounds r/g/b values. Note that this behaves
'differently to nearcolor, which can search for a color "bluer than blue".
'This never returns color 0 (firstindex=1).
function nearcolor_fast(r as integer, g as integer, b as integer) as ubyte
	dim col as RGBcolor = any
	col.b = iif(b > 255, 255, iif(b < 0, 0, b))
	col.g = iif(g > 255, 255, iif(g < 0, 0, g))
	col.r = iif(r > 255, 255, iif(r < 0, 0, r))
	return query_KDTree(nearcolor_kdtree, col)
end function


'Find the nearest match palette mapping from inputpal() into
'the master palette masterpal(), and store it in mapping(), an array of masterpal() indices.
'Alpha values ignored.
'mapping() may contain initial values, used as hints which are used if an exact match.
'Pass firstindex = 1 to prevent anything from getting mapped to colour 0.
sub find_palette_mapping(inputpal() as RGBcolor, masterpal() as RGBcolor, mapping() as integer, firstindex as integer = 0)
	for i as integer = 0 to small(ubound(mapping), ubound(inputpal))
		with inputpal(i)
			mapping(i) = nearcolor(masterpal(), .r, .g, .b, firstindex, mapping(i))
		end with
	next
end sub

declare sub quantize_surface_threshold(surf as Surface ptr, ret as Frame ptr, pal() as RGBcolor, options as QuantizeOptions, quantizing as bool)

'Convert a 32 bit Surface to a paletted Frame.
'Frees surf.
'If options.compute_palette=NO (the default) the palette to use should be given as pal(),
'otherwise a suitable palette is computed and returned in pal().
'Only colours options.firstindex..255 in pal() are used.
'Any pixels with alpha=0 are mapped to 0; otherwise alpha is ignored.
'Optionally, any RGB colour matching options.transparency gets mapped to index 0 (by default none);
'the Surface's alpha is ignored and options.transparency.a must be 0 or it won't be matched.
function quantize_surface(byref surf as Surface ptr, pal() as RGBcolor, options as QuantizeOptions) as Frame ptr
	if surf->format <> SF_32bit then
		showbug "quantize_surface only works on 32 bit Surfaces (bad image_import_as_frame_quantized call?)"
		gfx_surfaceDestroy(@surf)
		return NULL
	end if

	dim ret as Frame ptr
	ret = frame_new(surf->width, surf->height)

	if options.dither orelse options.compute_palette then
		if surf->pitch <> surf->width or ret->pitch <> surf->width then
			showbug "Can't call dither_image due to pitch mismatch"
		else
			'We can set the max error to 0 to disable dithering
			dim maxerr as integer = iif(options.dither, options.dither_maxerror, 0)

			dither_image(surf->pColorData, surf->width, surf->height, ret->image, _
				     options.compute_palette, @pal(0), 8, options.firstindex, maxerr)
			'Handle options.transparency
			quantize_surface_threshold(surf, ret, pal(), options, NO)
		end if
	else
		'This is not the same as options.dither_maxerror = 0, because it
		'uses nearcolor, which is slower but maybe slightly better results,
		'compared to the less "perceptual" comparison done in lib/gif.h.
		quantize_surface_threshold(surf, ret, pal(), options, YES)
	end if

	gfx_surfaceDestroy(@surf)
	return ret
end function

'If quantizing=YES, converting the image to 8 bit, otherwise only post-processing result from dither_image
'to handle options.transparency.
local sub quantize_surface_threshold(surf as Surface ptr, ret as Frame ptr, pal() as RGBcolor, options as QuantizeOptions, quantizing as bool)
	dim inptr as RGBcolor ptr
	dim outptr as ubyte ptr
	for y as integer = 0 to surf->height - 1
		inptr = surf->pColorData + y * surf->pitch
		outptr = ret->image + y * ret->pitch
		for x as integer = 0 to surf->width - 1
			' Ignore alpha
			if (inptr->col and &h00ffffff) = options.transparency.col then
				*outptr = 0
			elseif inptr->a = 0 then
				*outptr = 0
			elseif quantizing then
				if options.to_master then
					*outptr = nearcolor_fast(*inptr)  'Never 0
				else
					*outptr = nearcolor(pal(), inptr->r, inptr->g, inptr->b, options.firstindex)
				end if
			end if
			inptr += 1
			outptr += 1
		next
	next
end sub


'==========================================================================================
'                                           GIF
'==========================================================================================

'Number of bits needed for this many colors.
function bitdepth_needed(colors as integer) as integer
	dim bits as integer = 0
	colors -= 1
	while colors > 0
		bits += 1
		colors shr= 1
	wend
	return bits
end function

' Create a GifPalette from either the master palette or a Palette16 mapped onto
' a master palette, as needed for calling lib/gif.bi functions directly
sub GifPalette_from_pal (byref gpal as GifPalette, masterpal() as RGBcolor, pal as Palette16 ptr = NULL)
	if pal then
		' Color 0 used for transparency by gif encoder so the palette's
		' color 0 gets remapped to the nearest match, so if possible add
		' an extra color to end of palette to provide exact match
		dim palcolors as integer = pal->numcolors
		if pal->numcolors < 256 then
			palcolors += 1
		end if
		gpal.bitDepth = bitdepth_needed(palcolors)

		for idx as integer = 0 to palcolors - 1
			' The extra color, if any, is color 0
			dim masteridx as integer = pal->col(iif(idx = pal->numcolors, 0, idx))
			gpal.colors(idx) = masterpal(masteridx)
		next
	else
		' Again color 0 will be remapped, but with 256 colours to choose from there's likely
		' to be a good match
		gpal.bitDepth = 8
		for idx as integer = 0 to 255
			gpal.colors(idx) = masterpal(idx)
		next
	end if
end sub

' Export a 32 bit Surface as a single-frame .gif (alpha ignored)
sub surface_export_gif (surf as Surface Ptr, fname as string, dither as bool = NO)
	FAIL_IF(surf = NULL, "NULL Surface")
	FAIL_IF(surf->format <> SF_32bit, "8bit Surface")
	FAIL_IF(surf->pitch <> surf->width, "pitch doesn't match width")

	dim writer as GifWriter
	if GifBegin(@writer, fopen(fname, "wb"), surf->width, surf->height, 0, NO, NULL) = NO then
		debug "GifWriter(" & fname & ") failed"
	elseif GifWriteFrame(@writer, surf->pColorData, surf->width, surf->height, 0, 8, iif(dither, 1, 0)) = NO then
		debug "GifWriteFrame failed"
	elseif GifEnd(@writer) = NO then
		debug "GifEnd failed"
	end if
end sub

' Output a single-frame .gif. Ignores mask.
sub frame_export_gif (fr as Frame Ptr, fname as string, maspal() as RGBcolor, pal as Palette16 ptr = NULL, transparent as bool = NO)
	if fr->surf then
		surface_export_gif fr->surf, fname
		exit sub
	end if
	BUG_IF(fr->pitch <> fr->w, "pitch doesn't match width")

	dim writer as GifWriter
	dim gifpal as GifPalette
	GifPalette_from_pal gifpal, maspal(), pal
	if GifBegin(@writer, fopen(fname, "wb"), fr->w, fr->h, 0, iif(transparent, 1, 0), @gifpal) = NO then
		debug "GifWriter(" & fname & ") failed"
	elseif GifWriteFrame8(@writer, fr->image, fr->w, fr->h, 0, NULL) = NO then
		debug "GifWriteFrame8 failed"
	elseif GifEnd(@writer) = NO then
		debug "GifEnd failed"
	end if
end sub


'==========================================================================================
'                                           GIFRecorder
'==========================================================================================


property GIFRecorder.active() as bool
	return writer.f <> NULL
end property

'Returns time delay in hundreds of a second to be used for next frame
'(We have to say how long the frame will be displayed when we write it, rather than
'just telling how long the last frame was on-screen for.)
function GIFRecorder.calc_delay() as integer
	' Predict the time that this frame will be shown via the setwait timer.
	' But the actual next setvispage might happen after or before that
	' (if there are multiple setvispage calls before dowait).
	dim as double next_frame_time = waittime
	'next_next_frame_time = waittime + 1 / requested_framerate

	if gif_max_fps > 0 andalso next_frame_time - last_frame_end_time < 1. / gif_max_fps then
		' Wait until some more time has passed
		return 0
	end if

	dim ret as integer
	ret = (next_frame_time - last_frame_end_time) * 100
	if ret <= 0 then
		' In this case there's no point writing the frame, but this should be rare
		return 0
	end if

	' Instead of doing last_frame_end_time = waittime, this accumulates
	' the parts less than 0.01s, to avoid rounding error
	last_frame_end_time += ret * 0.01
	return ret
end function

sub start_recording_gif(secondscreen as string = "")
	stop_recording_video()
	recordvid = new GIFRecorder(absolute_path(next_unused_screenshot_filename() + ".gif"), secondscreen)
end sub

constructor GIFRecorder(outfile as string, secondscreen as string = "")
	dim gifpal as GifPalette
	' Use curmasterpal() rather than actual palette (displaypal()), because
	' displaypal() is affected by fades. We want the master palette,
	' because that's likely to be the palette for most frames.
	GifPalette_from_pal gifpal, curmasterpal()
	this.fname = outfile
	this.secondscreen = secondscreen
	dim file as FILE ptr = fopen(this.fname, "wb")
	if GifBegin(@this.writer, file, vpages(vpage)->w, vpages(vpage)->h, 6, NO, @gifpal) then
		show_overlay_message "Shft/Ctrl-F12 to stop recording", 1.
		this.last_frame_end_time = timer
		debuginfo "Starting to record to " & outfile
	else
		show_overlay_message "Can't record, GifBegin failed"
		debug "GifBegin failed"
	end if
end constructor

sub GIFRecorder.stop()
	if not this.active then exit sub

	if len(this.secondscreen) then
		#ifdef IS_CUSTOM
			debug "Asking Game to stop writing to " & this.secondscreen
			channel_write_line(channel_to_Game, "SCREEN STOP")
		#endif
		safekill this.secondscreen  'Both Game and Custom will attempt to delete the file
	end if

	if GifEnd(@this.writer) = NO then
		show_overlay_message "Recording failed"
		safekill this.fname
		exit sub
	end if
	dim msg as string = "Recorded " & trimpath(this.fname)

	' Compress it using gifsicle, if available
	dim gifsicle as string = find_helper_app("gifsicle")
	if len(gifsicle) then
		debuginfo "Compressing " & this.fname & " with gifsicle; size before = " & filelen(this.fname)
		dim handle as ProcessHandle
		handle = open_process(gifsicle, "-O2 " & escape_filename(this.fname) & " -o " & escape_filename(this.fname), NO, NO)
		if handle = 0 then
			debug "open_process " & gifsicle & " failed"
		else
			msg += " (Compressing...)"
		end if
		cleanup_process(@handle)
	end if

	show_overlay_message msg, 1.2
end sub

function recording_gif() as bool
	return recordvid andalso recordvid->active andalso *recordvid is GIFRecorder
end function

'Perform the effect of pressing Shift/Ctrl-F12: start or stop recording a gif
sub toggle_recording_gif()
	if recordvid andalso recordvid->active then
		stop_recording_video
	else
		start_recording_gif
	end if
end sub

local sub _gif_pitch_fail(what as string)
	showbug "Can't record gif from " & what & " with extra pitch"
	'This will cause the following GifWriteFrame* call to fail
	recordvid->stop()
end sub

'Stack two images, one of them loaded from a file; our on top and other underneath
local function combined_screen(our as Frame ptr, our_pal() as RGBcolor, other_path as string) as Frame ptr
	'Since the editor and player palettes might be different, or one might be running
	'at 32 bitdepth, easiest to always convert to 32bit.
	dim other as Surface ptr
	if real_isfile(other_path) then  'Avoid errors
		other = image_import_as_surface(other_path, YES)  'always_32bit=YES
	end if
	if other = NULL then
		'Maybe the game has quit, or hasn't started yet.
		'The bottom of the image will simply be blank while this is the case.
		debuginfo "combined_screen: couldn't read file"
		return NULL
	end if

	dim ret as Frame ptr
	ret = frame_new(large(our->w, other->width), our->h + other->height, 1, NO, NO, YES)
	frame_clear ret, uilook(uiBackground)
	frame_draw our, our_pal(), , 0, 0, NO, ret
	dim other_fr as Frame ptr = frame_with_surface(other)  'TODO: get rid of this
	frame_draw other_fr, , 0, our->h, NO, ret
	frame_unload @other_fr

	gfx_surfaceDestroy(@other)
	return ret
end function

' Called with every frame that should be included in any ongoing gif recording
sub GIFRecorder.record_frame(fr as Frame ptr, pal() as RGBcolor)
	if this.active = NO then exit sub

	dim delay as integer = this.calc_delay()
	if delay <= 0 then exit sub

	dim ret as bool
	dim bits as integer
	dim combined as Frame ptr

	if len(this.secondscreen) then
		combined = combined_screen(fr, pal(), this.secondscreen)
		if combined then fr = combined
	end if

	dim sf as Surface ptr = fr->surf

	if sf andalso sf->format = SF_32bit then
		bits = 32
		if sf->width <> sf->pitch then _gif_pitch_fail "32-bit Surface"
		ret = GifWriteFrame(@this.writer, sf->pColorData, sf->width, sf->height, delay, 8, NO)
	else
		' 8-bit Surface-backed Frames and regular Frames.
		bits = 8
		dim gifpal as GifPalette
		GifPalette_from_pal gifpal, pal()
		if sf andalso sf->format = SF_8bit then
			if sf->width <> sf->pitch then _gif_pitch_fail "8-bit Surface"
			ret = GifWriteFrame8(@this.writer, sf->pPaletteData, sf->width, sf->height, delay, @gifpal)
		else
			if fr->w <> fr->pitch then _gif_pitch_fail "Frame"
			ret = GifWriteFrame8(@this.writer, fr->image, fr->w, fr->h, delay, @gifpal)
		end if
	end if
	if ret = NO then
		' On a write failure, this.active will already be set to false
		show_overlay_message "Recording failed (GifWriteFrame " & bits & ")"
		debug "GifWriteFrame failed, bits = " & bits
	end if
	frame_unload @combined
end sub


'==========================================================================================
'                                       Screenshots
'==========================================================================================


'All extensions that might be used for screenshots or recordings (including by gfx_screenshot)
dim shared as string*5 screenshot_exts(...) => {".bmp", ".png", ".jpg", ".jpeg", ".dds", ".gif"}
dim shared as string screenshot_dir

local sub load_screenshot_settings()
	loaded_screenshot_settings = YES

	dim temp as string = "." & lcase(read_config_str("gfx.screenshot_format", "png"))
	if temp = ".bmp" orelse temp = ".png" orelse temp = ".gif" orelse temp = ".jpg" orelse temp = ".jpeg" then
		screenshot_format = temp
	else
		debug "Unrecognised/unsupported screenshot_format in config file: " & temp
		screenshot_format = ".png"
	end if

	use_gfx_screenshot = read_config_bool("gfx.gfx_" & gfxbackend & ".backend_screenshot", YES)

	screenshot_dir = read_config_str("gfx.screenshot_dir", "")
end sub

'Save a screenshot.
'basename: overrides the path/filename, default to gamename####.ext. Should NOT include the extension,
'    since the gfx backend can decide that.
'page: defaults to last setvispage
'message: if true, announces the file was saved.
'Returns the filename it was saved to, with extension
function screenshot (basename as string = "", page as integer = -1, message as bool = YES) as string
	if loaded_screenshot_settings = NO then
		load_screenshot_settings
	end if

	dim ret as string
	if len(basename) = 0 then
		basename = next_unused_screenshot_filename()
	end if
	'try external first
	if page <> -1 orelse use_gfx_screenshot = NO orelse gfx_screenshot(basename) = 0 then
		'otherwise save it ourselves
		ret = basename & screenshot_format
		if page = -1 then page = getvispage()
		frame_export_image(vpages(page), ret, displaypal())
	else
		' gfx_screenshot succeeded:
		' The reason for this for loop is that we don't know what extension the gfx backend
		' might save the screenshot as; have to search for it.
		for i as integer = 0 to ubound(screenshot_exts)
			dim tmp as string = basename & screenshot_exts(i)
			if isfile(tmp) then
				ret = tmp
				exit for
			end if
		next
	end if

	if message then
		show_overlay_message "Saved screenshot " & text_right(ret, 150), 1.5
	end if
	return ret
end function

sub bmp_screenshot(basename as string)
	'This is for when you explicitly want a bmp screenshot, and NOT the preferred
	'screenshot type used by the current gfx backend
	frame_export_bmp(basename & ".bmp", vpages(getvispage), displaypal())
end sub

' Find an available screenshot name in screenshot_dir, or current directory if that's not set.
' Returns filename without extension, and ensures it doesn't collide regardless of the
' extension selected from screenshot_extns.
local function next_unused_screenshot_filename() as string
	static search_start as integer
	static search_gamename as string

	dim as string ret
	dim as string gamename = game_fname
	if gamename = "" then
		' If we haven't loaded a game yet
		gamename = "ohrrpgce"
	end if
	if len(screenshot_dir) then
		gamename = screenshot_dir + SLASH + gamename
	end if

	' Reset search_start counter if needed
	if search_gamename <> gamename then
		search_gamename = gamename
		search_start = 0
	end if

	for n as integer = search_start to 99999
		ret = gamename + right("0000" & n, 4)
		'checking curdir, which is export directory
		for i as integer = 0 to ubound(screenshot_exts)
			if isfile(ret + screenshot_exts(i)) then continue for, for
		next
		search_start = n
		return ret
	next
	return ret  'This won't be reached
end function

'Take a single screenshot if F12 is pressed.
'Holding down F12 takes a screenshot each frame, however besides
'the first, they're saved to the temporary directory until key repeat kicks in, and then
'moved, in order to 'debounce' F12 if you only press it for a short while.
'(Hmm, now that we can record gifs directly, it probably makes sense to remove the ability to hold F12)
'NOTE: global variables like tmpdir can change between calls, have to be lenient
local sub snapshot_check(page as integer = -1)
	static as string backlog()
	' The following are just for the overlay message
	static as integer num_screenshots_taken
	static as string first_screenshot

	dim as integer n, F12bits
	dim as string shot

	F12bits = real_keyval(scF12)

	if F12bits = 0 then
		' If key repeat never occurred then delete the backlog.
		for n = 0 to ubound(backlog)
			'debug "killing " & backlog(n)
			safekill backlog(n)
		next
		erase backlog
		' Tell what we did
		if num_screenshots_taken = 1 then
			show_overlay_message "Saved screenshot " & first_screenshot, 1.5
		elseif num_screenshots_taken > 1 then
			show_overlay_message "Saved " & first_screenshot & " and " & (num_screenshots_taken - 1) & " more", 1.5
		end if
		num_screenshots_taken = 0
	elseif real_keyval(scCtrl) = 0 andalso real_keyval(scShift) = 0 then  'Not Shift/Ctrl-F12 to record a gif

		if F12bits = 1 then
			' Take a screenshot, but maybe delete it later
			shot = tmpdir & get_process_id() & "_tempscreen" & (ubound(backlog) + 1)
			a_append(backlog(), screenshot(shot, page, NO))  'message=NO
			'debug "temp save " & backlog(ubound(backlog))
		else
			' Key repeat has kicked in, so move our backlog of screenshots to the visible location.
			for n = 0 to ubound(backlog)
				shot = next_unused_screenshot_filename() & "." & justextension(backlog(n))
				'debug "moving " & backlog(n) & " to " & shot
				'Might be on a different filesystem, can't use renamefile
				os_shell_move backlog(n), shot
				num_screenshots_taken += 1
			next
			erase backlog

			' Take the new screenshot
			dim temp as string = screenshot( , page, NO)  'message=NO
			'debug "saved " & temp
			if num_screenshots_taken = 0 then
				first_screenshot = text_right(temp, 150)
			end if
			num_screenshots_taken += 1
		end if
		'debug "screen " & shot
	end if

	' This is in case this sub is called more than once before setkeys is called.
	' Normally setkeys happens at the beginning of a tick and setvispage at the end,
	' so this does no damage.
	' Clear 'new keypress' bit, but not key repeat.
	real_clearkey(scF12, NO)
end sub


'==========================================================================================
'                                    Screen forwarding
'==========================================================================================


constructor ScreenForwarder(outfile as string)
	fname = outfile
end constructor

property ScreenForwarder.active() as bool
	return LEN(this.fname) <> 0
end property

sub ScreenForwarder.stop()
	if this.active then
		safekill this.fname
		this.fname = ""
	end if
end sub

sub ScreenForwarder.record_frame(fr as Frame ptr, pal() as RGBcolor)
	frame_export_image(vpages(getvispage), this.fname, pal())
end sub

sub start_forwarding_screen(outfile as string)
	stop_recording_video
	recordvid = new ScreenForwarder(outfile)
end sub


'==========================================================================================
'                                 Graphics render clipping
'==========================================================================================


'NOTE: there is only one set of clipping values, shared globally for
'all drawing operations. The cliprect is stored in thread-local storage (TLS)
'so that it's possible for multiple threads to draw to Frames.
'The frame argument to setclip() is used to determine the allowed range of clipping values.

'Guaranteed to always return the same result on the same thread.
'Also ensures that the cliprect is for the given Frame, if given.
function get_cliprect(fr as Frame ptr = NULL) byref as ClipState
	'Without TLS:
	' static cliprect as ClipState
	' if fr then setclip , , , , fr
	' return cliprect

	dim cliprectp as ClipState ptr = cast(ClipState ptr, tls_get(tlsKeyClipRect))
	if cliprectp = NULL then
		cliprectp = new ClipState
		tls_set(tlsKeyClipRect, cliprectp)
	end if

	if fr andalso cliprectp->frame <> fr then
		cliprectp->frame = fr
		cliprectp->l = 0
		cliprectp->t = 0
		cliprectp->r = fr->w - 1
		cliprectp->b = fr->h - 1
	end if
	return *cliprectp
end function

'Set the bounds used by most Frame drawing functions.
'setclip (or get_cliprect(fr)) must be called to reset the clip bounds whenever the Frame being drawn to
'changes, to ensure clip bounds are valid.
sub setclip(l as integer = 0, t as integer = 0, r as integer = 999999, b as integer = 999999, fr as Frame ptr = 0)
	dim byref cliprect as ClipState = get_cliprect()
	if fr <> 0 then cliprect.frame = fr
	if cliprect.frame = 0 then
		showbug "Trying to setclip with no Frame"
		exit sub
	end if
	with *cliprect.frame
		cliprect.l = bound(l, 0, .w) '.w valid, prevents any drawing
		cliprect.t = bound(t, 0, .h)
		cliprect.r = bound(r, 0, .w - 1)
		cliprect.b = bound(b, 0, .h - 1)
	end with
end sub

'Shrinks clipping area, never grows it
'Returns true if the next cliprect still has nonzero size
function shrinkclip(l as integer = 0, t as integer = 0, r as integer = 999999, b as integer = 999999, fr as Frame ptr = 0) as bool
	dim byref cliprect as ClipState = get_cliprect()
	if fr andalso cliprect.frame <> fr then
		cliprect.frame = fr
		cliprect.l = 0
		cliprect.t = 0
		cliprect.r = fr->w - 1
		cliprect.b = fr->h - 1
	end if
	BUG_IF(cliprect.frame = 0 andalso fr = 0, "Trying to shrinkclip with no Frame", NO)
	with *cliprect.frame
		cliprect.l = bound(large(cliprect.l, l), 0, .w) '.w valid, prevents any drawing
		cliprect.t = bound(large(cliprect.t, t), 0, .h)
		cliprect.r = bound(small(cliprect.r, r), 0, .w - 1)
		cliprect.b = bound(small(cliprect.b, b), 0, .h - 1)
	end with

	return cliprect.r >= cliprect.l andalso cliprect.b >= cliprect.t
end function

'Blit a Frame with setclip clipping.
'trans: draw transparently, either using ->mask if available, or otherwise use colour 0 as transparent
'warning! Make sure setclip/get_cliprect has been called before calling this
'write_mask:
'    If the destination has a mask, sets the mask for the destination rectangle
'    equal to the mask (or color-key) for the source rectangle. Does not OR them.
local sub draw_clipped(src as Frame ptr, pal as Palette16 ptr = NULL, x as integer, y as integer, trans as bool = YES, dest as Frame ptr, opts as DrawOptions)
	dim as integer startx, starty, endx, endy
	dim as integer srcoffset

	startx = x
	endx = x + src->w - 1
	starty = y
	endy = y + src->h - 1

	dim byref cliprect as ClipState = get_cliprect()

	if startx < cliprect.l then
		srcoffset = (cliprect.l - startx)
		startx = cliprect.l
	end if

	if starty < cliprect.t then
		srcoffset += (cliprect.t - starty) * src->pitch
		starty = cliprect.t
	end if

	if endx > cliprect.r then
		endx = cliprect.r
	end if

	if endy > cliprect.b then
		endy = cliprect.b
	end if

	if starty > endy or startx > endx then exit sub

	blitohr(src, dest, pal, srcoffset, startx, starty, endx, endy, trans, opts)
end sub

' Blit a Frame with setclip clipping and opts->scale <> 1. opts can not be NULL!
local sub draw_clipped_scaled(src as Frame ptr, pal as Palette16 ptr = NULL, x as integer, y as integer, trans as bool = YES, dest as Frame ptr, opts as DrawOptions)
	dim byref cliprect as ClipState = get_cliprect()
	dim as integer sxfrom, sxto, syfrom, syto

	sxfrom = large(cliprect.l, x)
	sxto = small(cliprect.r, x + (src->w * opts.scale) - 1)

	syfrom = large(cliprect.t, y)
	syto = small(cliprect.b, y + (src->h * opts.scale) - 1)

	blitohrscaled (src, dest, pal, x, y, sxfrom, syfrom, sxto, syto, trans, opts)
end sub

' Blit a Surface with setclip clipping.
local sub draw_clipped_surf(src as Surface ptr, master_pal as RGBcolor ptr, pal as Palette16 ptr = NULL, x as integer, y as integer, trans as bool, dest as Surface ptr, opts as DrawOptions)

	dim byref cliprect as ClipState = get_cliprect()

	' It's OK for the src and dest rects to have negative size or be off
	' the edge of src/dest, because gfx_surfaceCopy properly clips them.
	dim srcRect as SurfaceRect = (0, 0, src->width - 1, src->height - 1)

	if x < cliprect.l then
		srcRect.left = cliprect.l - x
		x = cliprect.l
	end if

	if y < cliprect.t then
		srcRect.top = cliprect.t - y
		y = cliprect.t
	end if

	dim destRect as SurfaceRect = (x, y, cliprect.r, cliprect.b)

	opts.color_key0 = trans  'Clobbers def_drawoptions.color_key0
	if gfx_surfaceCopy(@srcRect, src, master_pal, pal, @destRect, dest, opts) then
		debug "gfx_surfaceCopy error"
	end if
	def_drawoptions.color_key0 = NO
end sub


'==========================================================================================
'                                   Sprite (Frame) cache
'==========================================================================================


'not to be used outside of the sprite functions
declare sub frame_delete_members(f as Frame ptr)
declare sub frame_freemem(f as Frame ptr)
declare sub spriteset_freemem(sprset as SpriteSet ptr)
'Assumes pitch == w
declare sub frame_add_mask(fr as Frame ptr, clr as bool = NO)


'The sprite cache holds Frame ptrs, which may also be Frame arrays and SpriteSets. Since
'each SpriteSet is associated with a unique Frame array, we don't need a separate cache
'for SpriteSets. SpriteSet data can be loaded after and linked to the cached Frame array
'if it was initially not loaded as a SpriteSet.

'The sprite cache, which is a HashTable (sprcache) containing all loaded sprites, is split in
'two: the A cache containing currently in-use sprites (which is not explicitly tracked), and
'the B cache holding those not in use, which is a LRU list 'sprcacheB' which holds a maximum
'of SPRCACHEB_SZ entries.
'The number/size of in-use sprites is not limited, and does not count towards the B cache
'unless COMBINED_SPRCACHE_LIMIT is defined. It should be left undefined when memory usage
'is not actually important.

'I couldn't find any algorithms for inequal cost caching so invented my own: sprite size is
'measured in 'base size' units, and instead of being added to the head of the LRU list,
'sprites are moved a number of places back from the head equal to their size. This is probably
'an unnecessary complication over LRU, but it's fun.

CONST SPRCACHE_BASE_SZ = 4096  'bytes
#IFDEF LOWMEM
 'Up to 8MB, including in-use sprites
 CONST SPRCACHEB_SZ = 2048  'in SPRITE_BASE_SZ units
 #DEFINE COMBINED_SPRCACHE_LIMIT 1
#ELSE
 'Max cache size of 32MB, but actual limit will be less due to items smaller than 4KB
 CONST SPRCACHEB_SZ = 8192  'in SPRITE_BASE_SZ units
#ENDIF

#if 0
        'Enable this to help track down sprite leaks.
        'Set TRACE_SPRITE to the particular spriteset you want to trace
        #define TRACE_SPRITE  SPRITE_CACHE_KEY(sprTypeWalkabout, 1)  'walkabout set 1
        #macro TRACE_CACHE(fr, msg)
                if fr->cacheentry andalso fr->cacheentry->hash = TRACE_SPRITE then
                        debug msg ", spr " & TRACE_SPRITE & " refc=" & fr->refcount
                end if
        #endmacro
#else
        #define TRACE_CACHE(fr, msg)
#endif

' removes a sprite from the cache, and frees it.
local sub sprite_remove_cache(entry as SpriteCacheEntry ptr)
	TRACE_CACHE(entry->p, "freeing from cache")
	dlist_remove(sprcacheB.generic, entry)
	sprcache.remove(entry->hash)
	#ifdef COMBINED_SPRCACHE_LIMIT
		sprcacheB_used -= entry->cost
	#else
		if entry->Bcached then
			sprcacheB_used -= entry->cost
		end if
	#endif
	if entry->p->refcount <> 1 then
		debugc errBug, "sprite cache leak/invalid sprite_remove_cache(): " & entry->hash & " " & frame_describe(entry->p)
		'Leak instead of deleting the Frame, to avoid crashes
	else
		entry->p->cacheentry = NULL  'help to detect double free
		frame_freemem(entry->p)
	end if
	delete entry
end sub

'Free some sprites from the end of the B cache
'Returns true if enough space was freed
local function sprite_cacheB_shrink(amount as integer) as bool
	sprite_cacheB_shrink = (amount <= SPRCACHEB_SZ)
	if sprcacheB_used + amount <= SPRCACHEB_SZ then exit function

	dim as SpriteCacheEntry ptr pt, prevpt
	pt = sprcacheB.last
	while pt
		prevpt = pt->cacheB.prev
		sprite_remove_cache(pt)
		if sprcacheB_used + amount <= SPRCACHEB_SZ then exit function
		pt = prevpt
	wend
end function

sub sprite_empty_cache_range(minkey as integer, maxkey as integer, leakmsg as string)
	dim iterstate as uinteger = 0
	dim as SpriteCacheEntry ptr pt, nextpt

	nextpt = NULL
	pt = sprcache.iter(iterstate, nextpt)
	while pt
		nextpt = sprcache.iter(iterstate, pt)
		if pt->hash >= minkey andalso pt->hash <= maxkey then
			sprite_remove_cache(pt)
		end if
		pt = nextpt
	wend
end sub

'Unlike sprite_empty_cache, this reloads (in-use) sprites from file, without changing the pointers
'to them. Any sprite that's not actually in use is removed from the cache as it's unnecessary to reload.
local sub sprite_update_cache_range(minkey as integer, maxkey as integer)
	dim iterstate as uinteger = 0
	dim as SpriteCacheEntry ptr pt, nextpt

	nextpt = NULL
	pt = sprcache.iter(iterstate, nextpt)
	while pt
		nextpt = sprcache.iter(iterstate, pt)

		if pt->hash < minkey or pt->hash > maxkey then
			pt = nextpt
			continue while
		end if

		'recall that the cache counts as a reference
		if pt->p->refcount <> 1 then
			dim sprtype as integer = pt->hash \ SPRITE_CACHE_MULT
			dim record as integer = pt->hash mod SPRITE_CACHE_MULT

			if record = SPRITE_CACHE_GLOBAL_ANIMS then
				'Unlike normal SpriteSets, the one holding the default animations must
				'be updated inplace because others point to it.
				reload_global_animations(pt->p->sprset, sprtype)
			else

				dim newframe as Frame ptr
				newframe = frame_load_uncached(sprtype, record)

				if newframe <> NULL then
					dim numframes as integer = newframe->arraylen
					if newframe->arraylen <> pt->p->arraylen then
						'Unfortunately, this error will occur if you change the number
						'of frames in the spriteset editor. Only thing we can do about it is
						'try to unload all affected Frames before updating the cache.
						showbug "sprite_update_cache: number of frames changed for sprite " & pt->hash
						numframes = small(numframes, pt->p->arraylen)
					end if

					'Transplant the data from the new Frame into the old Frame, so that no
					'pointers need to be updated. pt (the SpriteCacheEntry) doesn't need to
					'to be modified at all

					dim refcount as integer = pt->p->refcount
					dim wantmask as bool = (pt->p->mask <> NULL)
					'Remove the host's previous organs (deletes SpriteSet)
					frame_delete_members pt->p
					'Insert the new organs
					memcpy(pt->p, newframe, sizeof(Frame) * numframes)
					'Having removed everything from the donor, dispose of it
					Deallocate(newframe)
					'Fix the bits we just clobbered
					pt->p->cached = 1
					pt->p->refcount = refcount
					pt->p->cacheentry = pt
					if pt->p->sprset then
						'We DON'T do the same trick with SpriteSets.
						'You mustn't hold onto SpriteSet ptrs when gfx are reloaded, they will become invalid!
						'Update cross-link
						pt->p->sprset->frames = pt->p
					end if
					'Make sure we don't crash if we were using a mask (might be the wrong mask though)
					if wantmask then frame_add_mask pt->p

				end if
			end if
		else
			'Don't bother if not in use
			sprite_remove_cache(pt)
		end if
		pt = nextpt
	wend
end sub

'Reload all graphics of certain type
sub sprite_update_cache(sprtype as SpriteType)
	sprite_update_cache_range(SPRITE_CACHE_MULT * sprtype, SPRITE_CACHE_MULT * (sprtype + 1) - 1)
	if sprtype = sprTypeTileset then
		sprite_update_cache sprTypeTilesetStrip
	end if
end sub

'Attempt to completely empty the sprite cache, detecting memory leaks
'By default, remove everything. With an argument: remove specific sprite type,
'or with two: remove a specific spriteset
sub sprite_empty_cache(sprtype as SpriteType = sprTypeInvalid, setnum as integer = -1)
	if sprtype = sprTypeInvalid then
		sprite_empty_cache_range(INT_MIN, INT_MAX, "leaked sprite ")
		if sprcacheB_used <> 0 or sprcache.numitems <> 0 then
			debug "sprite_empty_cache: corruption: sprcacheB_used=" & sprcacheB_used & " items=" & sprcache.numitems
		end if
	elseif setnum < 0 then
		sprite_empty_cache_range(SPRITE_CACHE_MULT * sprtype, SPRITE_CACHE_MULT * (sprtype + 1) - 1, "leaked sprite ")
	else
		dim which as integer = SPRITE_CACHE_MULT * sprtype + setnum
		sprite_empty_cache_range(which, which, "leaked sprite ")
	end if
end sub

sub sprite_debug_cache()
	debug "==sprcache=="
	dim iterstate as integer = 0
	dim pt as SpriteCacheEntry ptr = NULL

	while sprcache.iter(iterstate, pt)
		debug pt->hash & " cost=" & pt->cost & " : " & frame_describe(pt->p)
	wend

	debug "==sprcacheB== (used units = " & sprcacheB_used & "/" & SPRCACHEB_SZ & ")"
	pt = sprcacheB.first
	while pt
		debug pt->hash & " cost=" & pt->cost & " : " & frame_describe(pt->p)
		pt = pt->cacheB.next
	wend
end sub

'a sprite has no references, move it to the B cache
local sub sprite_to_B_cache(entry as SpriteCacheEntry ptr)
	dim pt as SpriteCacheEntry ptr

	if sprite_cacheB_shrink(entry->cost) = NO then
		'fringe case: bigger than the max cache size
		sprite_remove_cache(entry)
		exit sub
	end if

	'apply size penalty
	pt = sprcacheB.first
	dim tobepaid as integer = entry->cost
	while pt
		tobepaid -= pt->cost
		if tobepaid <= 0 then exit while
		pt = pt->cacheB.next
	wend
	dlist_insertat(sprcacheB.generic, pt, entry)
	entry->Bcached = YES
	#ifndef COMBINED_SPRCACHE_LIMIT
		sprcacheB_used += entry->cost
	#endif
end sub

' move a sprite out of the B cache
local sub sprite_from_B_cache(entry as SpriteCacheEntry ptr)
	dlist_remove(sprcacheB.generic, entry)
	entry->Bcached = NO
	#ifndef COMBINED_SPRCACHE_LIMIT
		sprcacheB_used -= entry->cost
	#endif
end sub

' search cache, update as required if found
local function sprite_fetch_from_cache(sprtype as SpriteType, record as integer) as Frame ptr
	dim entry as SpriteCacheEntry ptr
	entry = sprcache.get(SPRITE_CACHE_KEY(sprtype, record))

	if entry then
		'cachehit += 1
		if entry->Bcached then
			sprite_from_B_cache(entry)
		end if
		entry->p->refcount += 1
		return entry->p
	end if
	return NULL
end function

' adds a newly loaded frame to the cache with a given type/record
local sub sprite_add_cache(sprtype as SpriteType, record as integer, p as Frame ptr)
	if p = 0 then exit sub

	dim entry as SpriteCacheEntry ptr
	entry = new SpriteCacheEntry

	entry->hash = SPRITE_CACHE_KEY(sprtype, record)
	entry->p = p
	entry->cost = (p->w * p->h * p->arraylen) \ SPRCACHE_BASE_SZ + 1
	'leave entry->cacheB unlinked
	entry->Bcached = NO

	'the cache counts as a reference, but only to the head element of an array!!
	p->cached = 1
	p->refcount += 1
	p->cacheentry = entry
	sprcache.add(entry->hash, entry)

	#ifdef COMBINED_SPRCACHE_LIMIT
		sprcacheB_used += entry->cost
	#endif
end sub

local sub _cache_sprtype(byref doc as DocPtr, sprtype as SpriteType)
	for record as integer = 0 TO sprite_sizes(sprtype).lastrec()
		dim fr as Frame ptr
		fr = sprite_fetch_from_cache(sprtype, record)
		if fr = NULL then
			if doc = NULL then
				doc = rgfx_open(sprtype, NO, optNoDelay)
				if doc = NULL then exit sub
			end if
			fr = rgfx_load_spriteset(doc, sprtype, record, YES)
			if fr then
				sprite_add_cache(sprtype, record, fr)
			end if
		end if
		frame_unload @fr
	next
	if doc then
		load_global_animations sprtype, doc  'Doesn't have to be freed.
	end if
end sub

'Equivalent to loading and freeing all graphics of one type, but vastly faster.
'Does nothing if graphics haven't been converted to rgfx.
'sprtype can sprTypeBackdrop too, but not sprTypeTileset
'TODO: we should just hold the .rgfx files open instead and this will become obsolete
sub cache_all_spritesets(sprtype as SpriteType)
	BUG_IF(sprtype > sprTypeEnemy, "Must be an rgfx sprite type")
	'dim starttime as double = timer
	dim doc as DocPtr
	'FIXME: the sprite cache doesn't know that each enemy sprite has two sprtypes
	if sprtype = sprTypeEnemy then
		_cache_sprtype doc, sprTypeSmallEnemy
		_cache_sprtype doc, sprTypeMediumEnemy
		_cache_sprtype doc, sprTypeLargeEnemy
	else
		_cache_sprtype doc, sprtype
	end if
	FreeDocument doc
	'debuginfo "sprtype " & sprtype & " cached in " & cint((timer - starttime) * 1e3) & "ms"
end sub


'==========================================================================================
'                                          Frames
'==========================================================================================


'Create a blank Frame or array of Frames. No SpriteSet created!
'By default not initialised; pass clr=YES to initialise to 0
'with_surface32: if true, create a 32-it Surface-backed Frame.
'no_alloc: ignore this; internal use only.
function frame_new(w as integer, h as integer, frames as integer = 1, clr as bool = NO, wantmask as bool = NO, with_surface32 as bool = NO, no_alloc as bool = NO) as Frame ptr
	if w < 1 or h < 1 or frames < 1 then
		showbug "frame_new: bad size " & XY(w,h).wh & "*" & frames
		return 0
	end if
	if with_surface32 then
		if wantmask then
			'8-bit surfaces can have masks, but not 32-bit ones (no form
			'of transparency is implemented for them yet!)
			showbug "frame_new: mask and backing surface mututally exclusive"
		end if
	end if

	dim ret as Frame ptr
	'this hack was Mike's idea, not mine!
	ret = callocate(sizeof(Frame) * frames)

	'no memory? shucks.
	if ret = 0 then
		showerror "Could not create sprite frames, no memory"
		return 0
	end if

	for i as integer = 0 to frames - 1
		with ret[i]
			'the caller to frame_new is considered to have a ref to the head; and the head to have a ref to each other elem
			'so set each refcount to 1
			.refcount = 1
			.arraylen = frames
			'By default, use contiguous frameids
			.frameid = i
			if i > 0 then .arrayelem = 1
			.defpal = -1
			.w = w
			.h = h
			.pitch = w
			.mask = NULL
			if no_alloc then
			elseif with_surface32 then
				if gfx_surfaceCreate(w, h, SF_32bit, SU_Staging, @.surf) then
					frame_freemem(ret)
					return NULL
				end if
				if clr then
					gfx_surfaceFill(curmasterpal(0).col, NULL, .surf)
				end if
			else
				if clr then
					.image = callocate(.pitch * h)
					if wantmask then .mask = callocate(.pitch * h)
				else
					.image = allocate(.pitch * h)
					if wantmask then .mask = allocate(.pitch * h)
				end if

				if .image = 0 or (.mask = 0 and wantmask <> NO) then
					showerror "Could not allocate sprite frames/surfaces"
					'well, I don't really see the point freeing memory, but who knows...
					frame_freemem(ret)
					return NULL
				end if
			end if
		end with
	next
	return ret
end function

'Create a frame which is a view onto part of a larger frame
'Can return a zero-size view. Seems to work, but not yet sure that all operations will work correctly on such a frame.
function frame_new_view(spr as Frame ptr, x as integer, y as integer, w as integer, h as integer) as Frame ptr
	dim ret as Frame ptr = callocate(sizeof(Frame))

	if ret = 0 then
		showerror "Could not create sprite view, no memory"
		return 0
	end if

	if x < 0 then w -= -x: x = 0
	if y < 0 then h -= -y: y = 0
	with *ret
		.w = bound(w, 0, spr->w - x)
		.h = bound(h, 0, spr->h - y)
		if x >= spr->w or y >= spr->h or .w = 0 or .h = 0 then
			'this might help to keep things slightly saner
			.w = 0
			.h = 0
		end if
		.pitch = spr->pitch

		if spr->surf then
			'.surf must be destroyed (refcount decremented) when this Frame is unloaded
			if gfx_surfaceCreateView(spr->surf, x, y, .w, .h, @.surf) then
				deallocate ret
				return NULL
			end if
		else
			'These must not be freed when this Frame is unloaded
			.image = spr->image + .pitch * y + x
			if spr->mask then
				.mask = spr->mask + .pitch * y + x
			end if
		end if
		.refcount = 1
		.arraylen = 1 'at the moment not actually used anywhere on sprites with isview = 1
		.defpal = -1
		.isview = 1
		'we point .base at the 'root' frame which really owns these pixel buffer(s)
		if spr->isview then
			.base = spr->base
		else
			.base = spr
		end if
		if .base->refcount <> NOREFC then .base->refcount += 1
	end with
	return ret
end function

' Returns a Frame which is backed by a Surface.
' Unload/Destroy both the Frame and the Surface: increments refcount for the Surface!
' Note: normally it makes no sense to call this on a Surface that is itself
' a view of a Frame
function frame_with_surface(surf as Surface ptr) as Frame ptr
	if surf = NULL then return NULL
	dim ret as Frame ptr = frame_new(1, 1, 1, , , , YES)  'no_alloc = YES. Dummy size
	init_frame_with_surface ret, surf
	return ret
end function

'ret is a Frame without image/mask allocated
local sub init_frame_with_surface(ret as Frame ptr, surf as Surface ptr)
	surf = gfx_surfaceReference(surf)
	with *ret
		.surf = surf
		.w = surf->width
		.h = surf->height
		.pitch = surf->pitch
		'image and mask are Null
	end with
end sub

' Creates an (independent) 32 bit Surface which is a copy of a Frame.
' This is not the same as gfx_surfaceCreateFrameView, which creates a Surface which
' is just a view of a Frame (and is a temporary hack!)
function frame_to_surface32(fr as Frame ptr, masterpal() as RGBcolor, pal as Palette16 ptr = NULL) as Surface ptr
	if fr->surf then
		debug "frame_to_surface32 called on a Surface-backed Frame"
		if fr->surf->format = SF_8bit then
			showbug "Converting Frame w/ 8bit Surface to 32bit Surface unimplemented"
			return NULL
		end if
		return gfx_surfaceReference(fr->surf)
	end if

	dim surf as Surface ptr
	if gfx_surfaceCreate(fr->w, fr->h, SF_32bit, SU_Staging, @surf) then
		return NULL
	end if
	dim wrapper as Frame ptr  'yuck
	wrapper = frame_with_surface(surf)
	dim trans as bool = NO
	dim opts as DrawOptions
	if fr->mask then
		'32bit Surfaces don't have transparency yet, but if the Frame has
		'a mask at least make transparent pixels end up as rgb 0,0,0
		trans = YES
		opts.write_mask = YES
	end if
	frame_draw fr, masterpal(), pal, 0, 0, trans, wrapper, opts
	frame_unload @wrapper
	return surf
end function

' Turn a regular Frame into a 32-bit Surface-backed Frame.
' Content is preserved.
' Warning: this is a dangerous implementation. Any view onto fr will become invalid
sub frame_convert_to_32bit(fr as Frame ptr, masterpal() as RGBcolor, pal as Palette16 ptr = NULL)
	BUG_IF(fr->cached, "refusing to clobber cached Frame")
	fr->surf = frame_to_surface32(fr, masterpal(), pal)

	deallocate(fr->image)
	fr->image = NULL
	deallocate(fr->mask)
	fr->mask = NULL
end sub

' Turn Surface-backed Frame back to a regular Frame. Content IS WIPED if it was a 32-bit Surface!
sub frame_drop_surface(fr as Frame ptr)
	if fr->surf then
		if fr->image = NULL then  'Should always be true
			if fr->surf->format = SF_8bit then
				fr->image = allocate(fr->pitch * fr->h)
				memcpy(fr->image, fr->surf->pPaletteData, fr->pitch * fr->h)
				if fr->surf->pMaskData then
					fr->mask = allocate(fr->pitch * fr->h)
					memcpy(fr->mask, fr->surf->pMaskData, fr->pitch * fr->h)
				end if
			else
				fr->image = callocate(fr->pitch * fr->h)
			end if
		end if
		gfx_surfaceDestroy(@fr->surf)
	end if
end sub

local sub frame_delete_members(f as Frame ptr)
	if f->arrayelem then debug "can't free arrayelem!": exit sub
	for i as integer = 0 to f->arraylen - 1
		deallocate(f[i].image)  'May be NULL
		f[i].image = NULL
		deallocate(f[i].mask)  'May be NULL
		f[i].mask = NULL
		if f[i].surf then gfx_surfaceDestroy(@f[i].surf)
		f[i].refcount = FREEDREFC  'help to detect double free
	next
	if f->sprset then
		delete f->sprset
		f->sprset = NULL
	end if
end sub

' unconditionally frees a sprite from memory.
' You should never need to call this: use frame_unload
' Should only be called on the head of an array (and not a view, obv)!
' Warning: not all code calls frame_freemem to free sprites! Grrr!
local sub frame_freemem(f as Frame ptr)
	if f = 0 then exit sub
	frame_delete_members f
	deallocate(f)
end sub

' Duplicates a Frame array to a vector of individual Frame ptrs. Doesn't deref input.
' (TODO: this function is temporary; Frame arrays should be replaced with SpriteSets)
function frame_array_to_vector(frames as Frame ptr) as Frame ptr vector
	dim ret as Frame ptr vector
	v_new ret
	for idx as integer = 0 TO frames->arraylen - 1
		v_append ret, frame_duplicate(@frames[idx])
	next
	return ret
end function

' Duplicates a vector of Frames, which must all have the same size and mask/no mask,
' as a single Frame array. Doesn't free input.
' (TODO: this function is temporary; Frame arrays should be replaced with SpriteSets)
function frame_vector_to_array(frames as Frame ptr vector) as Frame ptr
	if frames = NULL then return NULL
	dim ret as Frame ptr
	ret = frame_new(frames[0]->w, frames[0]->h, v_len(frames), , frames[0]->mask <> NULL)

	for idx as integer = 0 TO v_len(frames) - 1
		dim opts as DrawOptions
		opts.write_mask = YES
		frame_draw frames[idx], , 0, 0, NO, @ret[idx], opts
		ret[idx].frameid = frames[idx]->frameid
	next
	return ret
end function

'================================ Loading & Saving Frames =================================

'Public:
' Loads a 4-bit or 8-bit sprite/backdrop/tileset from the appropriate game lump, *with caching*.
' For 4-bit sprites it will return a pointer to the first frame, and subsequent frames
' will be immediately after it in memory. (This is a hack, and will probably be removed)
' For tilesets, the tileset will already be reordered as needed.
' Note: if no game has been loaded, the lump doesn't exist, or the record is out of range,
' may return either NULL or a blank Frame!
function frame_load(sprtype as SpriteType, record as integer) as Frame ptr
	dim ret as Frame ptr = sprite_fetch_from_cache(sprtype, record)
	if ret then
		TRACE_CACHE(ret, "frame_load from cache")
		return ret
	end if
	ret = frame_load_uncached(sprtype, record)
	if ret then
		sprite_add_cache(sprtype, record, ret)
		TRACE_CACHE(ret, "frame_load from file")
	end if
	return ret
end function

'If an extension, must exclude the '.'
function graphics_file(lumpname_or_extn as string) as string
	dim as bool fullname = instr(lumpname_or_extn, ".") > 0
	if len(game) = 0 then
		' Haven't loaded a game, fallback to the engine's default graphics
		' (Note that this won't exist when running a distributed game.exe)
		dim gfxdir as string = finddatadir("defaultgfx", NO)  'error_if_missing=NO
		if len(gfxdir) = 0 then
			return ""
		end if
		if fullname then
			return gfxdir & SLASH & lumpname_or_extn
		else
			return gfxdir & SLASH "ohrrpgce." & lumpname_or_extn
		end if
	end if
	if fullname then
		return workingdir & SLASH & lumpname_or_extn
	else
		return game & "." & lumpname_or_extn
	end if
end function

' Loads a 4-bit or 8-bit sprite/backdrop/tileset from the appropriate game lump. See frame_load.
function frame_load_uncached(sprtype as SpriteType, record as integer) as Frame ptr
	if sprtype < sprTypeFirstLoadable orelse sprtype > sprTypeLastLoadable orelse record < 0 then
		debugc errBug, "frame_load: invalid type=" & sprtype & " and rec=" & record
		return 0
	end if

	dim ret as Frame ptr
	dim sprset as SpriteSet ptr
	dim starttime as double = timer

	if sprtype = sprTypeTileset or sprtype = sprTypeTilesetStrip then
		dim mxs as Frame ptr
		'Returns a blank Frame on error
		mxs = frame_load_mxs(graphics_file("til"), record)
		if mxs = NULL then return NULL
		if sprtype = sprTypeTilesetStrip then
			ret = mxs_frame_to_tileset(mxs)
			frame_unload @mxs
		else
			ret = mxs
		end if
	else
		ret = rgfx_load_spriteset(sprtype, record, NO)

		if ret then
			'OK
		elseif sprtype = sprTypeBackdrop then
			'Returns a blank Frame on error
			ret = frame_load_mxs(graphics_file("mxs"), record)
			if ret then
				sprset = new SpriteSet(ret)  'Attaches to ret
			end if
		else
			with sprite_sizes(sprtype)
				'debug "loading " & sprtype & "  " & record
				'cachemiss += 1
				ret = frame_load_4bit(graphics_file("pt" & sprtype), record, .frames, .size.w, .size.h)
				if ret = NULL then
					ret = frame_new(.size.w, .size.h, .frames, YES)
				end if
			end with
			if ret then
				initialise_backcompat_pt_frameids ret, sprtype
				sprset = new SpriteSet(ret)  'Attaches to ret
				sprset->global_animations = load_global_animations(sprtype)
			end if
		end if
	end if

	debug_if_slow(starttime, 0.1, sprtype & "," & record)
	return ret
end function

' You can use this to load a .pt?-format 4-bit sprite from some non-standard location.
' No code does this. Does not use a cache.
' It will return a pointer to the first frame (of num frames), and subsequent frames
' will be immediately after it in memory. (This is a hack, and will probably be removed)
function frame_load_4bit(filen as string, rec as integer, numframes as integer, wid as integer, hei as integer) as Frame ptr
	dim ret as Frame ptr

	dim frsize as integer = wid * hei / 2
	dim recsize as integer = frsize * numframes

	dim fh as integer
	if openfile(filen, for_binary + access_read, fh) then
		debugerror "frame_load_4bit: could not open " & filen
		return 0
	end if

	ret = frame_new(wid, hei, numframes)
	if ret = 0 then
		close #fh
		return 0
	end if

	'find the right sprite (remember, it's base-1)
	seek #fh, recsize * rec + 1

	dim framenum as integer, x as integer, y as integer, z as ubyte

	'pixels stored in columns, 2 pixels/byte
	for framenum = 0 to numframes - 1
		with ret[framenum]
			for x = 0 to wid - 1
				for y = 0 to hei - 1
					'pull up two pixels
					get #fh, , z

					'the high nybble is the first pixel
					.image[y * wid + x] = (z SHR 4)

					y += 1

					'and the low nybble is the second one
					.image[y * wid + x] = z AND 15
				next
			next
		end with
	next

	lazyclose fh
	return ret
end function

declare sub write_frame_node(fr as Frame ptr, fs_node as Node ptr, bits as integer)
declare sub read_frame_node(fr as Frame ptr, fr_node as Node ptr, bitdepth as integer, byref lastid as integer)

'Appends a new "frameset" child node storing an array of Frames and returns it.
'TODO: Doesn't save metadata about palette or master palette
'TODO: Doesn't save mask, but we don't have any need to serialise masks at the moment
function frameset_to_node(fr as Frame ptr, parent as Node ptr) as Node ptr
	if fr->arrayelem then
		showbug "frameset_to_node: not first Frame in array"
		return NULL
	end if

	dim as Node ptr fs_node
	fs_node = AppendChildNode(parent, "frameset")
	AppendChildNode(fs_node, "w", fr->w)
	AppendChildNode(fs_node, "h", fr->h)
	AppendChildNode(fs_node, "format", 0)

	if fr->mask then
		debug "WARNING: frameset_to_node can't save masks"
	end if

	'"bits" gives the format of the "image" node; whether this Frame
	'is a 4 or 8 bit sprite is unknown (and would be stored separately)
	dim bits as integer = 8
	if fr->surf then
		if fr->surf->format = SF_32bit then
			bits = 32
		end if
	end if
	AppendChildNode(fs_node, "bits", bits)

	for idx as integer = 0 to fr->arraylen - 1
		write_frame_node(@fr[idx], fs_node, bits)
	next

	return fs_node
end function

'Write a single Frame in an array as a "frame" node
local sub write_frame_node(fr as Frame ptr, fs_node as Node ptr, bits as integer)
	dim as Node ptr frame_node, image_node
	frame_node = AppendChildNode(fs_node, "frame")
	AppendChildNode(frame_node, "id", fr->frameid)

	image_node = AppendChildNode(frame_node, "image")
	'Allocate uninitialised memory
	SetContent(image_node, NULL, fr->w * fr->h * (bits \ 8))
	dim imdata as byte ptr = GetZString(image_node)

	if fr->surf then
		dim surf as Surface ptr = fr->surf
		dim rowbytes as integer = surf->width * bits \ 8
		dim pitchbytes as integer = surf->pitch * bits \ 8
		for y as integer = 0 TO surf->height - 1
			memcpy(imdata + y * rowbytes, cast(byte ptr, surf->pRawData) + y * pitchbytes, rowbytes)
		next
	else
		for y as integer = 0 TO fr->h - 1
			memcpy(imdata + y * fr->w, fr->image + y * fr->pitch, fr->w)
		next
	end if
end sub

'Loads an array of Frames from a "frameset" node
function frameset_from_node(fs_node as Node ptr) as Frame ptr
	dim as integer dataformat = GetChildNodeInt(fs_node, "format")
	dim as integer bitdepth = GetChildNodeInt(fs_node, "bits", 8)
	dim as XYPair size
	size.w = GetChildNodeInt(fs_node, "w")
	size.h = GetChildNodeInt(fs_node, "h")

	dim as integer frames = 0
	dim as Node ptr ch = FirstChild(fs_node, "frame")
	while ch
		frames += 1
		ch = NextSibling(ch, "frame")
	wend

	ERROR_IF(dataformat <> 0, "unsupported data format " & dataformat, NULL)
	ERROR_IF(frames = 0, "no frames!", NULL)
	ERROR_IF(size.w <= 0 orelse size.h <= 0 orelse size.w > maxFrameSize orelse size.h > maxFrameSize, _
		 "bad size " & size, NULL)

	dim fr as Frame ptr
	if bitdepth = 8 then
		fr = frame_new(size.w, size.h, frames)
	elseif bitdepth = 32 then
		fr = frame_new(size.w, size.h, frames, , , , YES)  'no_alloc = YES
	else
		showerror "frameset_from_node: Unsupported graphics bitdepth " & bitdepth
		return NULL
	end if
	if fr = NULL then return NULL  'Should already have shown an error

	dim index as integer = 0
	dim lastid as integer = -1
	dim fr_node as Node ptr = FirstChild(fs_node, "frame")
	while fr_node
		read_frame_node(@fr[index], fr_node, bitdepth, lastid)
		fr_node = NextSibling(fr_node, "frame")
		index += 1
	wend

	return fr
end function

'Loads a single "frame" node in a frameset
'lastid: frameid for previous Frame
local sub read_frame_node(fr as Frame ptr, fr_node as Node ptr, bitdepth as integer, byref lastid as integer)
	fr->frameid = GetChildNodeInt(fr_node, "id", fr->frameid)
	ERROR_IF(fr->frameid <= lastid, "corrupt .rgfx file; frameids not in order: " & fr->frameid & " follows " & lastid)
	lastid = fr->frameid

	dim image_node as NodePtr = GetChildByName(fr_node, "image")
	dim imdata as ubyte ptr = GetZString(image_node)
	dim imlen as integer = GetZStringSize(image_node)
	if imdata = NULL orelse imlen <> fr->w * fr->h * bitdepth \ 8 then
		showerror "frame_from_node: Couldn't load image; data missing or bad length (" & imlen & " for " & fr->size & ", bitdepth=" & bitdepth & ")"
		exit sub
	end if

	if bitdepth = 8 then
		memcpy(fr->image, imdata, fr->w * fr->h)
	elseif bitdepth = 32 then
		dim surf as Surface ptr
		if gfx_surfaceCreate(fr->w, fr->h, SF_32bit, SU_Staging, @surf) then
			exit sub
		end if
		memcpy(surf->pColorData, imdata, fr->w * fr->h * 4)
		init_frame_with_surface(fr, surf)
		gfx_surfaceDestroy(@surf)
	end if
end sub


'==========================================================================================

'Public:
' Releases a reference to a sprite and nulls the pointer.
' If it is refcounted, decrements the refcount, otherwise it is freed immediately.
' A note on frame arrays: you may pass around pointers to frames in it (call frame_reference
' on them) and then unload them, but no memory will be freed until the head pointer refcount reaches 0.
' The head element will have 1 extra refcount if the frame array is in the cache. Each of the non-head
' elements also have 1 refcount, indicating that they are 'in use' by the head element,
' but this is just for feel-good book keeping
' (cdecl so that it can be used in the Frame ptr vector typetable)
sub frame_unload cdecl(ppfr as Frame ptr ptr)
	if ppfr = 0 then exit sub
	dim fr as Frame ptr = *ppfr
	*ppfr = 0
	if fr = 0 then exit sub

	dim byref cliprect as ClipState = get_cliprect()
	if cliprect.frame = fr then cliprect.frame = 0

	with *fr
		if .refcount = FREEDREFC then
			debug frame_describe(fr) & " already freed!"
			exit sub
		end if
		'Theoretically possible to have an un-refcounted Frame/SpriteSet which uses refcounted default animations
		if .sprset andalso .sprset->global_animations then
			spriteset_unload @.sprset->global_animations
		end if
		if .refcount = NOREFC then
			exit sub
		end if
		.refcount -= 1
		TRACE_CACHE(fr, "frame_unload")

		if .refcount < 0 then debugc errBug, frame_describe(fr) & " has refcount " & .refcount
		'if cached, can free two references at once
		if (.refcount - .cached) <= 0 then
			TRACE_CACHE(fr, "now unused")

			if .arrayelem then
				'this should not happen, because each arrayelem gets an extra refcount
				debug "arrayelem with refcount = " & .refcount
				exit sub
			end if
			if .isview then
				if .surf then
					'View onto surf, so decrement its refcount
					gfx_surfaceDestroy(@.surf)
				end if
				frame_unload @.base
				deallocate(fr)
			else
				for i as integer = 1 to .arraylen - 1
					if fr[i].refcount <> 1 then
						debug frame_describe(@fr[i]) & " array elem freed with bad refcount"
					end if
				next
				if .cached then
					sprite_to_B_cache(fr->cacheentry)
				else
					'Frees .surf
					frame_freemem(fr)
				end if
			end if
		end if
	end with
end sub

'Takes a 320x200 Frame and produces a 20x3200 Frame in the format expected of tilesets:
'linear series of 20x20 tiles - better cache behaviour, but I haven't tested whether it
'has a significant performance effect.
function mxs_frame_to_tileset(spr as Frame ptr) as Frame ptr
	CHECK_FRAME_8BIT(spr, NULL)

	dim tileset as Frame ptr
	tileset = frame_new(20, 20 * 160)

	dim as ubyte ptr sptr = tileset->image
	dim as ubyte ptr srcp
	dim tilex as integer
	dim tiley as integer
	dim px as integer
	dim py as integer

	for tiley = 0 to 9
		for tilex = 0 to 15
			srcp = spr->image + tilex * 20 + tiley * 320 * 20
			for py = 0 to 19
				for px = 0 to 19
					*sptr = *srcp
					sptr += 1
					srcp += 1
				next
				srcp += 320 - 20
			next
		next
	next
	return tileset
end function

function hexptr(p as any ptr) as string
	return hex(cast(intptr_t, p))
end function

function frame_describe(p as Frame ptr) as string
	if p = 0 then return "'(null)'"
	dim temp as string
	if p->sprset then temp = p->sprset->describe()
	return "'(0x" & hexptr(p) & ") " & p->arraylen & "*" & p->size.wh _
	       & " offset=" & p->offset  & " img=0x" & hexptr(p->image) _
	       & " msk=0x" & hexptr(p->mask) & " pitch=" & p->pitch & " cached=" & p->cached & " aelem=" _
	       & p->arrayelem & " view=" & p->isview & " base=0x" & hexptr(p->base) & " refc=" & p->refcount & "' " _
	       & temp
end function

'this is mostly just a gimmick
function frame_is_valid(p as Frame ptr) as bool
	if p = 0 then return NO
	dim ret as bool = YES

	if p->refcount <> NOREFC and p->refcount <= 0 then ret = NO

	'this is an arbitrary test, and in theory, could cause a false-negative, but I can't concieve of 100 thousand references to the same sprite.
	if p->refcount > 100000 then ret = NO

	if p->w < 0 or p->h < 0 then ret = NO
	if p->pitch < p->w then ret = NO

	if p->surf then
		if p->image <> 0 orelse p->mask <> 0 then ret = NO
	else
		if p->image = 0 then ret = NO
	end if

	'Patterns used by Windows and Linux to scrub memory
	if cint(p->mask) = &hBAADF00D or cint(p->image) = &hBAADF00D then ret = NO
	if cint(p->mask) = &hFEEEFEEE or cint(p->image) = &hFEEEFEEE then ret = NO

	if ret = NO then
		showbug "Invalid sprite " & frame_describe(p)
		'if we get here, we are probably doomed, but this might be a recovery
		if p->cacheentry then sprite_remove_cache(p->cacheentry)
	end if
	return ret
end function

'Add a mask
'clr: is true, blank mask (entirely transparent), otherwise copy image
local sub frame_add_mask(fr as Frame ptr, clr as bool = NO)
	CHECK_FRAME_8BIT(fr)
	if fr->mask then exit sub
	if clr = NO then
		fr->mask = allocate(fr->pitch * fr->h)
		memcpy(fr->mask, fr->image, fr->pitch * fr->h)
	else
		fr->mask = callocate(fr->pitch * fr->h)
	end if
end sub

'for a copy you intend to modify. Otherwise use frame_reference
'clr: if true, return a new blank Frame with the same size.
'note: does not copy frame arrays, only single frames
function frame_duplicate(p as Frame ptr, clr as bool = NO, addmask as bool = NO) as Frame ptr
	dim ret as Frame ptr

	if p = 0 then return 0

	if p->surf then
		BUG_IF(clr orelse addmask, "clr/addmask unimplemented for Surfaces", NULL)  'TODO: we don't need it yet
		dim surf as Surface ptr = surface_duplicate(p->surf)
		ret = frame_with_surface(surf)
		ret->offset = p->offset
		gfx_surfaceDestroy(@surf)  'Decrement extra reference
		return ret
	end if

	ret = callocate(sizeof(Frame))
	if ret = 0 then return 0

	ret->w = p->w
	ret->h = p->h
	ret->pitch = p->w
	ret->offset = p->offset
	ret->refcount = 1
	ret->image = 0
	ret->mask = 0
	ret->arraylen = 1
	ret->frameid = p->frameid
	ret->defpal = p->defpal

	if p->image then
		if clr = 0 then
			ret->image = allocate(ret->w * ret->h)
			if p->w = p->pitch then
				'a little optimisation (we know ret->w == ret->pitch)
				memcpy(ret->image, p->image, ret->w * ret->h)
			else
				for i as integer = 0 to ret->h - 1
					memcpy(ret->image + i * ret->pitch, p->image + i * p->pitch, ret->w)
				next
			end if
		else
			ret->image = callocate(ret->w * ret->h)
		end if
	end if
	if p->mask then
		if clr = 0 then
			ret->mask = allocate(ret->w * ret->h)
			if p->w = p->pitch then
				'a little optimisation (we know ret->w == ret->pitch)
				memcpy(ret->mask, p->mask, ret->w * ret->h)
			else
				for i as integer = 0 to ret->h - 1
					memcpy(ret->mask + i * ret->pitch, p->mask + i * p->pitch, ret->w)
				next
			end if
		else
			ret->mask = callocate(ret->w * ret->h)
		end if
	elseif addmask then
		frame_add_mask ret, clr
	end if

	return ret
end function

function frame_reference cdecl(p as Frame ptr) as Frame ptr
	if p = 0 then return 0
	if p->refcount = NOREFC then
		'showbug "tried to reference a non-refcounted sprite!"
	else
		p->refcount += 1
		TRACE_CACHE(p, "frame_reference")
	end if
	return p
end function

' This is a convenience function to set a Frame ptr variable, CHANGING the
' Frame ptr it contains. Useful because many frame functions are not in-place.
' (Use frame_draw with trans=NO, write_mask=YES to set the contents of one Frame
' equal to another. There is no way to do so while changing the Frame size
' (it could be implemented, but only for Frames with no views onto them).
sub frame_assign cdecl(ptr_to_replace as Frame ptr ptr, new_value as Frame ptr)
	frame_unload ptr_to_replace
	*ptr_to_replace = new_value
end sub

' See frame_assign.
sub surface_assign cdecl(ptr_to_replace as Surface ptr ptr, new_value as Surface ptr)
	if *ptr_to_replace then gfx_surfaceDestroy(ptr_to_replace)
	*ptr_to_replace = new_value
end sub

' This is for the Frame ptr vector typetable. Ignore.
local sub _frame_copyctor cdecl(dest as Frame ptr ptr, src as Frame ptr ptr)
	*dest = frame_reference(*src)
end sub

constructor DrawOptions(scale as integer = 1)
	this.scale = scale
end constructor

'Public:
' Draws a sprite to a page/dest.
' trans: if true, color 0 is transparent. (If the Frame has a mask, mask = 0 is transparent instead)
sub frame_draw(src as Frame ptr, pal as Palette16 ptr = NULL, x as RelPos, y as RelPos, trans as bool = YES, page as integer, opts as DrawOptions = def_drawoptions)
	frame_draw src, curmasterpal(), pal, x, y, trans, vpages(page), opts
end sub

sub frame_draw(src as Frame ptr, pal as Palette16 ptr = NULL, x as RelPos, y as RelPos, trans as bool = YES, dest as Frame ptr, opts as DrawOptions = def_drawoptions)
	frame_draw src, curmasterpal(), pal, x, y, trans, dest, opts
end sub

' Explicitly specify the master palette to use - it is only used if the src is 8-bit
' and the dest is 32-bit.
sub frame_draw overload (src as Frame ptr, masterpal() as RGBcolor, pal as Palette16 ptr = NULL, x as RelPos, y as RelPos, trans as bool = YES, dest as Frame ptr, opts as DrawOptions = def_drawoptions)
	BUG_IF(src = NULL orelse dest = NULL, "trying to draw from/to null frame")
	get_cliprect(dest)  'Set clipping Frame

	x = relative_pos(x, dest->w, src->w)
	y = relative_pos(y, dest->h, src->h)
	x += src->offset.x * opts.scale
	y += src->offset.y * opts.scale

	frame_draw_internal src, masterpal(), pal, x, y, trans, dest, opts
end sub

private function surface_shim(src as Frame ptr, temp_surface as Surface ptr) as Surface ptr
	if src->surf then return src->surf
	'Correct but slower:
	'dim src_surface as Surface ptr
	'if gfx_surfaceCreateFrameView(src, @src_surface) then return
	'Kludgy but faster, avoid a slow allocation:
	if surfaceFrameShim(src, temp_surface) then return NULL
	return temp_surface
end function

local sub frame_draw_internal(src as Frame ptr, masterpal() as RGBcolor, pal as Palette16 ptr = NULL, x as integer, y as integer, trans as bool = YES, dest as Frame ptr, opts as DrawOptions = def_drawoptions)

	if (src->surf andalso src->surf->format <> SF_8bit) orelse _
	   (dest->surf andalso dest->surf->format <> SF_8bit) then
		' Have to use gfx_surfaceCopy, so translate everything to Surfaces

		BUG_IF(opts.scale <> 1, "scale not supported with 32-bit Frames")

		dim as Surface tempsrc_surface = any, tempdest_surface = any
		dim src_surface as Surface ptr = surface_shim(src, @tempsrc_surface)
		dim dest_surface as Surface ptr = surface_shim(dest, @tempdest_surface)
		if src_surface = 0 orelse dest_surface = 0 then return

		/'
		dim master_pal as RGBPalette ptr
		if src_surface->format = SF_8bit then
			' From 8 -> 32 bit
			' This is slow, performs an allocation and copy!
			' (Can use masterpal_to_gfxpal instead)
			if gfx_paletteFromRGB(@masterpal(0), @master_pal) then
				debug "gfx_paletteFromRGB failed"
				goto cleanup
			end if
		end if
		'/

		draw_clipped_surf src_surface, @masterpal(0), pal, x, y, trans, dest_surface, opts

		/'
	cleanup:
		if master_pal then
			gfx_paletteDestroy(@master_pal)
		end if
		if src->surf = NULL then
			gfx_surfaceDestroy(@src_surface)
		end if
		'/
	else
		if opts.scale <> 1 then
			BUG_IF(src->surf orelse dest->surf, "32-bit Frames don't support scale<>1")
			draw_clipped_scaled src, pal, x, y, trans, dest, opts
		else
			draw_clipped src, pal, x, y, trans, dest, opts
		end if
	end if
end sub

' Draw a Frame with position and transformation specified by an AffineTransform.
' Supports 8 & 32-bit Frames, including alpha channels. (Respects opts.alpha_channel.)
' Supports opts.with_blending, opts.blend_mode, and opts.argbModifier in addition to opts.opacity.
' Does not support masks on 8-bit Frames, or color_key0 on 32-bit Frames.
' Does not support opts.alpha_channel=NO when using opacity/argbModifier.a or vertex alpha.
' Optionally, can pass in an array of 4 colours (clockwise from bottomleft) to interpolate
' colour (and alpha) modulation across the image.
sub frame_draw_transformed(src as Frame ptr, masterpal() as RGBcolor, pal as Palette16 ptr = NULL, transf as AffineTransform, trans as bool = YES, dest as Frame ptr, opts as DrawOptions = def_drawoptions, vertex_cols as RGBcolor ptr = NULL)
	dim vertices(3) as VertexPT
	'Clockwise from bottom-left
	vertices(0).tex.u = 0
	vertices(0).tex.v = 1
	vertices(1).tex.u = 0
	vertices(1).tex.v = 0
	vertices(2).tex.u = 1
	vertices(2).tex.v = 0
	vertices(3).tex.u = 1
	vertices(3).tex.v = 1
	with transf
		'Shift vertices slightly to avoid almost-horizontal or -vertical edges
		'cutting through going exactly through a row/column of pixel centers,
		'which causes artifacts (not a rasterizer bug, will happen in OpenGL too)
		vertices(0).pos = .bottomleft - 0.01
		vertices(1).pos = .topleft - 0.01
		vertices(2).pos = .topright - 0.01
		'vertices(3).pos = .bottomright - 0.01
		vertices(3).pos = XYF(.bottomleft.x + (.topright.x - .topleft.x), .bottomleft.y + (.topright.y - .topleft.y))
	end with

	'Get Surface shims around Frames as needed
	dim as Surface tempsrc_surface = any, tempdest_surface = any
	dim src_surface as Surface ptr = surface_shim(src, @tempsrc_surface)
	dim dest_surface as Surface ptr = surface_shim(dest, @tempdest_surface)
	if src_surface = 0 orelse dest_surface = 0 then return

	'Convert from pal (which may be NULL) to a 256-color palette
	dim gfxpal as RGBPalette ptr = unrollPalette16(pal, @masterpal(0))

	dim byref cliprect as ClipState = get_cliprect(dest)
	dim destrect as SurfaceRect = (cliprect.l, cliprect.t, cliprect.r, cliprect.b)

	opts.color_key0 = trans  'Clobbers def_drawoptions.color_key0

	if opts.with_blending andalso opts.argbModifier.col <> -1 andalso vertex_cols = NULL then
		'gfx_renderQuadTexture doesn't support argbModifier
		static white(3) as RGBcolor = {(-1), (-1), (-1), (-1)}
		vertex_cols = @white(0)
		'(Possible optimisation: if only argbModifier.a is set, set opts.opacity instead)
	end if

	if vertex_cols then
		dim ptcvertices(3) as VertexPTC
		for i as integer = 0 to 3
			ptcvertices(i).tex = vertices(i).tex
			ptcvertices(i).pos = vertices(i).pos
			ptcvertices(i).col = vertex_cols[i]
		next
		gfx_renderQuadTextureColor(@ptcvertices(0), src_surface, gfxpal, @destrect, dest_surface, @opts)
	else
		gfx_renderQuadTexture(@vertices(0), src_surface, gfxpal, @destrect, dest_surface, @opts)
	end if
	def_drawoptions.color_key0 = NO
end sub

' Draw a paralleogram with a colour gradient between its corners.
' Supports opts.with_blending, opts.blend_mode, and opts.argbModifier in addition to opts.opacity
' opts.alpha_channel ignored.
sub rectangle_transformed(cols() as RGBcolor, transf as AffineTransform, dest as Frame ptr, opts as DrawOptions = def_drawoptions)
	BUG_IF(ubound(cols) <> 3, "expect 4 colors")

	dim vertices(3) as VertexPC
	'Clockwise from bottom-left
	with transf
		vertices(0).pos = .bottomleft - 0.01
		vertices(1).pos = .topleft - 0.01
		vertices(2).pos = .topright - 0.01
		'vertices(3).pos = .bottomright - 0.01
		vertices(3).pos = XYF(.bottomleft.x + (.topright.x - .topleft.x), .bottomleft.y + (.topright.y - .topleft.y))
	end with
	for i as integer = 0 to 3
		vertices(i).col = cols(i)
	next

	'Get Surface shims around Frames as needed
	dim as Surface tempdest_surface = any
	dim dest_surface as Surface ptr = surface_shim(dest, @tempdest_surface)
	if dest_surface = 0 then return

	dim byref cliprect as ClipState = get_cliprect(dest)
	dim destrect as SurfaceRect = (cliprect.l, cliprect.t, cliprect.r, cliprect.b)

	gfx_renderQuadColor(@vertices(0), @destrect, dest_surface, @opts)
end sub

'Calculate an AffineTransform of a slice of given 'size' by first stretching by 'zoom',
'then rotating `angle` degrees clockwise about `center` (defaults of center of `size`),
'then translating by `pos`. (Does NOT support RelPosXY)
'This can achieve any affine transform.
'(The Float2 args are passed byref but not modified)
sub rotozoom_transform(byref result as AffineTransform, size as XYPair, center as Float2 ptr = NULL, pos as Float2, angle as double, zoom as Float2)
	dim _center as Float2 = any
	if center = NULL then
		_center = XYF(size.x / 2, size.y / 2)
		center = @_center
	end if
	dim vertices(3) as Float2
	vec2GenerateCorners @vertices(0), 4, size, *center
	dim matrix as Float3x3
	matrixLocalTransform @matrix, angle * -M_PI / 180, zoom, pos
	'Only first 3 vertices
	vec2Transform @result.vertices(0), 3, @vertices(0), 3, matrix
end sub

'Return a copy of a single Frame or a Frame array, each frame clipped or extended.
'Extended portions are filled with bgcol.
'Can also be used to scroll (does not wrap around)
'Turns an 8-bit Surface-backed Frame into a regular Frame, and works on 32-bit Surface-backed ones too.
'Like all functions that return new Frames, the new Frame doesn't have a SpriteSet ptr.
function frame_resized(spr as Frame ptr, wide as integer, high as integer, shiftx as integer = 0, shifty as integer = 0, bgcol as integer = 0) as Frame ptr
	dim as Frame ptr ret
	dim with_surface32 as bool = (spr->surf <> NULL andalso spr->surf->format = SF_32bit)
	ret = frame_new(wide, high, spr->arraylen, NO, (spr->mask <> NULL), with_surface32)
	dim opts as DrawOptions
	opts.write_mask = YES
	for fridx as integer = 0 to spr->arraylen - 1
		frame_clear @ret[fridx], bgcol
		frame_draw @spr[fridx], NULL, shiftx, shifty, NO, @ret[fridx], opts
		ret[fridx].frameid = spr[fridx].frameid
	next
	return ret
end function

'Scale a Frame to given size. Returns a 32-bit Surface-backed Frame.
'masterpal() only used if src is 8-bit. pal can be NULL.
function frame_scaled32(src as Frame ptr, wide as integer, high as integer, masterpal() as RGBcolor, pal as Palette16 ptr = NULL) as Frame ptr
	dim as Surface ptr src_surface, temp
	if src->surf then
		src_surface = src->surf
	else
		src_surface = frame_to_surface32(src, masterpal(), pal)
	end if
	temp = surface_scale(src_surface, wide, high)
	if src->surf = NULL then
		gfx_surfaceDestroy(@src_surface)
	end if
	dim ret as Frame ptr = frame_with_surface(temp)
	gfx_surfaceDestroy(@temp)
	return ret
end function

'Public:
' Returns a copy of a sprite in the midst of a given fade or distort effect.
' This only supports a subset of effects; frame_draw_dissolved should normally be used instead.
' Note that the result has a mask, which is very unusual for Frames.
' tlength is the desired length of the transition (in any time units you please),
' t is the number of elasped time units. style is the specific transition.
function frame_dissolved(spr as Frame ptr, tlength as integer, t as integer, style as integer) as Frame ptr
	CHECK_FRAME_8BIT(spr, NULL)

	'Return a blank sprite of same size
	'(Note that Vapourise and Phase Out aren't blank on t==tlength, while others are, unless tlength=0
	if t > tlength then return frame_duplicate(spr, YES)
	'Return copy. (Actually Melt otherwise has very slight distortion on frame 0.)
	if t <= 0 then return frame_duplicate(spr)

	'by default, sprites use colourkey transparency instead of masks.
	'We could easily not use a mask here, but by using one, this function can be called on 8-bit graphics
	'too; just in case you ever want to fade out a backdrop or something?
	dim startblank as bool = (style = 8 or style = 9 or style = 11)
	dim cpy as Frame ptr
	cpy = frame_duplicate(spr, startblank, YES) 'addmask = YES
	if cpy = 0 then return 0

	dim as integer i, j, sx, sy, tog

	select case style
		case 0 'scattered pixel dissolve
			dim prng_state as unsigned integer = cpy->w * tlength

			dim cutoff as unsigned integer = 2 ^ 20 * t / (tlength - 0.5)

			for sy = 0 to cpy->h - 1
				dim mptr as ubyte ptr = @cpy->mask[sy * cpy->pitch]
				for sx = 0 to cpy->w - 1
					prng_state = (prng_state * 1103515245 + 12345)
					if (prng_state shr 12) < cutoff then
						mptr[sx] = 0
					end if
				next
			next

		case 1 'crossfade
			'interesting idea: could maybe replace all this with calls to generalised fuzzyrect
			dim m as integer = cpy->w * cpy->h * t * 2 / tlength
			dim mptr as ubyte ptr
			dim xoroff as integer = 0
			if t > tlength / 2 then
				'after halfway mark: checker whole sprite, then checker the remaining (with tog xor'd 1)
				for sy = 0 to cpy->h - 1
					mptr = cpy->mask + sy * cpy->pitch
					tog = sy and 1
					for sx = 0 to cpy->w - 1
						tog = tog xor 1
						if tog then mptr[sx] = 0
					next
				next
				xoroff = 1
				m = cpy->w * cpy->h * (t - tlength / 2) * 2 / tlength
			end if
			'checker the first m pixels of the sprite
			for sy = 0 to cpy->h - 1
				mptr = cpy->mask + sy * cpy->pitch
				tog = (sy and 1) xor xoroff
				for sx = 0 to cpy->w - 1
					tog = tog xor 1
					if tog then mptr[sx] = 0
					m -= 1
					if m <= 0 then exit for, for
				next
			next
		case 2 'diagonal vanish
			i = cpy->w * t * 2 / tlength
			j = i
			for sy = 0 to i
				j = i - sy
				if sy >= cpy->h then exit for
				for sx = 0 to j
					if sx >= cpy->w then exit for
					cpy->mask[sy * cpy->pitch + sx] = 0
				next
			next
		case 3 'sink into ground
			dim fall as integer = cpy->h * t / tlength
			for sy = cpy->h - 1 to 0 step -1
				if sy < fall then
					memset(cpy->mask + sy * cpy->pitch, 0, cpy->w)
				else
					memcpy(cpy->image + sy * cpy->pitch, cpy->image + (sy - fall) * cpy->pitch, cpy->w)
					memcpy(cpy->mask + sy * cpy->pitch, cpy->mask + (sy - fall) * cpy->pitch, cpy->w)
				end if
			next
		case 4 'squash
			for i = cpy->h - 1 to 0 step -1
				dim desty as integer = cpy->h * (t / tlength) + i * (1 - t / tlength)
				desty = bound(desty, 0, cpy->h - 1)
				if desty > i then
					memcpy(cpy->image + desty * cpy->pitch, cpy->image + i * cpy->pitch, cpy->w)
					memcpy(cpy->mask + desty * cpy->pitch, cpy->mask + i * cpy->pitch, cpy->w)
					memset(cpy->mask + i * cpy->pitch, 0, cpy->w)
				end if
			next
		case 5 'melt
			'height and meltmap are fixed point, with 8 bit fractional parts
			'(an attempt to speed up this dissolve, which is 10x slower than most of the others!)
			'the current height of each column above the base of the frame
			dim height(-1 to cpy->w) as integer
			dim meltmap(cpy->h - 1) as integer

			for i = 0 to cpy->h - 1
				'Gompertz sigmoid function, exp(-exp(-x))
				'this is very close to 1 when k <= -1.5
				'and very close to 0 when k >= 1.5
				'so decreases down to 0 with increasing i (height) and t
				'meltmap(i) = exp(-exp(-7 + 8.5*(t/tlength) + (-cpy->h + i))) * 256
				meltmap(i) = exp(-exp(-8 + 10*(t/tlength) + 6*(i/cpy->h))) * 256
			next

			dim poffset as integer = (cpy->h - 1) * cpy->pitch
			dim destoff as integer

			for sy = cpy->h - 1 to 0 step -1
				for sx = 0 to cpy->w - 1
					destoff = (cpy->h - 1 - (height(sx) shr 8)) * cpy->pitch + sx

					'if sx = 24 then
						'debug sy & " mask=" & cpy->mask[poffset + sx] & " h=" & height(sx)/256 & " dest=" & (destoff\cpy->pitch) & "   " & t/tlength
					'end if

					'potentially destoff = poffset + sx
					dim temp as integer = cpy->mask[poffset + sx]
					cpy->mask[poffset + sx] = 0
					cpy->image[destoff] = cpy->image[poffset + sx]
					cpy->mask[destoff] = temp

					if temp then
						height(sx) += meltmap(height(sx) shr 8)
					else
						'empty spaces melt quicker, for flop down of hanging swords,etc
						'height(sx) += meltmap(height(sx)) * (1 - t/tlength)
						'height(sx) += meltmap((height(sx) shr 8) + 16)
						height(sx) += meltmap(sy)
					end if
				next
				poffset -= cpy->pitch

				'mix together adjacent heights so that hanging pieces don't easily disconnect
				height(-1) = height(0)
				height(cpy->w) = height(cpy->w - 1)
				for sx = (sy mod 3) to cpy->w - 1 step 3
					height(sx) = height(sx - 1) \ 4 + height(sx) \ 2 + height(sx + 1) \ 4
				next
			next
		case 6 'vapourise
			'vapoury is the bottommost vapourised row
			dim vapoury as integer = (cpy->h - 1) * (t / tlength)
			dim vspeed as integer = large(cint(cpy->h / tlength), 1)
			for sx = 0 to cpy->w - 1
				dim chunklength as integer = randint(vspeed + 5)
				for i = -2 to 9999
					if rando() < 0.3 then exit for
				next

				dim fragy as integer = large(vapoury - large(i, 0) - (chunklength - 1), 0)
				'position to copy fragment from
				dim chunkoffset as integer = large(vapoury - (chunklength - 1), 0) * cpy->pitch + sx

				dim poffset as integer = sx
				for sy = 0 to vapoury
					if sy >= fragy and chunklength <> 0 then
						cpy->image[poffset] = cpy->image[chunkoffset]
						cpy->mask[poffset] = cpy->mask[chunkoffset]
						chunkoffset += cpy->pitch
						chunklength -= 1
					else
						cpy->mask[poffset] = 0
					end if
					poffset += cpy->pitch
				next
			next
		case 7 'phase out
			dim fall as integer = 1 + (cpy->h - 2) * (t / tlength)  'range 1 to cpy->h-1
			'blank out top of sprite
			for sy = 0 to fall - 2
				memset(cpy->mask + sy * cpy->pitch, 0, cpy->w)
			next

			for sx = 0 to cpy->w - 1
				dim poffset as integer = sx + fall * cpy->pitch

				'we stretch the two pixels at the vapour-front up some factor
				dim beamc1 as integer = -1
				dim beamc2 as integer = -1
				if cpy->mask[poffset] then beamc1 = cpy->image[poffset]
				if cpy->mask[poffset - cpy->pitch] then beamc2 = cpy->image[poffset - cpy->pitch]

				if beamc1 = -1 then continue for
				for sy = fall to large(fall - 10, 0) step -1
					cpy->image[poffset] = beamc1
					cpy->mask[poffset] = 1
					poffset -= cpy->pitch
				next
				if beamc2 = -1 then continue for
				for sy = sy to large(sy - 10, 0) step -1
					cpy->image[poffset] = beamc2
					cpy->mask[poffset] = 1
					poffset -= cpy->pitch
				next
			next
		case 8 'squeeze (horizontal squash)
			dim destx(spr->w - 1) as integer
			for sx = 0 to spr->w - 1
				destx(sx) = sx * (1 - t / tlength) + 0.5 * (spr->w - 1) * (t / tlength)
			next
			for sy = 0 to spr->h - 1
				dim destimage as ubyte ptr = cpy->image + sy * cpy->pitch
				dim destmask as ubyte ptr = cpy->mask + sy * cpy->pitch
				dim srcmask as ubyte ptr = iif(spr->mask, spr->mask, spr->image)
				dim poffset as integer = sy * cpy->pitch
				for sx = 0 to spr->w - 1
					destimage[destx(sx)] = spr->image[poffset]
					destmask[destx(sx)] = srcmask[poffset]
					poffset += 1
				next
			next
		case 9 'shrink (horizontal+vertical squash) centered on bottom center
			dim destx(spr->w - 1) as integer
			for sx = 0 to spr->w - 1
				destx(sx) = sx * (1 - t / tlength) + 0.5 * (spr->w - 1) * (t / tlength)
			next
			for sy = 0 to spr->h - 1
				dim desty as integer = sy * (1 - t / tlength) + (spr->h - 1) * (t / tlength)
				dim destimage as ubyte ptr = cpy->image + desty * cpy->pitch
				dim destmask as ubyte ptr = cpy->mask + desty * cpy->pitch
				dim srcmask as ubyte ptr = iif(spr->mask, spr->mask, spr->image)
				dim poffset as integer = sy * cpy->pitch
				for sx = 0 to spr->w - 1
					destimage[destx(sx)] = spr->image[poffset]
					destmask[destx(sx)] = srcmask[poffset]
					poffset += 1
				next
			next
		case 10 'flicker
			dim state as integer = 0
			dim ctr as integer  'percent
			for i = 0 to t
				dim cutoff as integer = 60 * (1 - i / tlength) + 25 * (i / tlength)
				dim inc as integer = 60 * i / tlength
				ctr += inc
				if ctr > cutoff then
					i += ctr \ cutoff  'length of gaps increases
					if i > t then state = 1
					ctr = ctr mod 100
				end if
			next
			if state then frame_clear(cpy)
		case 11 'shrink (horizontal+vertical squash) centered
			dim destx(spr->w - 1) as integer
			for sx = 0 to spr->w - 1
				destx(sx) = sx * (1 - t / tlength) + 0.5 * (spr->w - 1) * (t / tlength)
			next
			for sy = 0 to spr->h - 1
				dim desty as integer = sy * (1 - t / tlength) + 0.5 * (spr->h - 1) * (t / tlength)
				dim destimage as ubyte ptr = cpy->image + desty * cpy->pitch
				dim destmask as ubyte ptr = cpy->mask + desty * cpy->pitch
				dim srcmask as ubyte ptr = iif(spr->mask, spr->mask, spr->image)
				dim poffset as integer = sy * cpy->pitch
				for sx = 0 to spr->w - 1
					destimage[destx(sx)] = spr->image[poffset]
					destmask[destx(sx)] = srcmask[poffset]
					poffset += 1
				next
			next
		case else
			debug "frame_dissolved: unsupported effect " & style
	end select

	return cpy
end function

'Draw a Frame with a dissolve/distort effect.
'This supports all effects, unlike frame_dissolved.
sub frame_draw_dissolved (src as Frame ptr, pal as Palette16 ptr = NULL, x as RelPos, y as RelPos, trans as bool = YES, dest as Frame ptr, opts as DrawOptions = def_drawoptions, tlength as integer, tick as integer, style as integer)

	'Trivial cases
	if tick > tlength then exit sub
	if tick <= 0 then
		frame_draw src, pal, x, y, trans, dest, opts
		exit sub
	end if

	dim fadeopts as DrawOptions
	fadeopts.with_blending = YES
	fadeopts.blend_mode = blendModeNormal

	dim max_opacity as double = 1.
	if opts.with_blending then
		'We respect the baseline opts.opacity and blend_mode
		max_opacity = opts.opacity
		fadeopts.blend_mode = opts.blend_mode
	end if

	'Should just pass other drawoptions on, though the combination hasn't necessarily been tested
	'(FIXME: when opts.scale <> 1., we need to adjust dissolves that offset the sprite)
	fadeopts.scale = opts.scale
	fadeopts.write_mask = opts.write_mask

	dim tfrac as double = tick / tlength

	select case style
		case 12  'Fade out
			fadeopts.opacity = max_opacity * (1 - tfrac)
			frame_draw src, pal, x, y, trans, dest, fadeopts

		case 13  'Ghost fade
			'Fade out (normal blending) 100% to 0% at half time
			dim t_to_halfway as double = 1. - tick / (tlength / 2.)  'Goes from 1 to -1
			if t_to_halfway > 0. then
				fadeopts.opacity = max_opacity * t_to_halfway
				frame_draw src, pal, x, y, trans, dest, fadeopts
			end if
			'...while also fade in (add blending) from 0% to 100% at half time to 0%
			fadeopts.opacity = max_opacity * (1 - abs(t_to_halfway))
			fadeopts.blend_mode = blendModeAdd
			frame_draw src, pal, x, y, trans, dest, fadeopts

		case 14  'Fade to white
			FAIL_IF(pal = NULL, "Fade through white needs palette")

			dim white as RGBcolor
			if fadeopts.blend_mode = blendModeMultiply then
				'Special case, fading to white would do nothing, so fade to black
			else
				white.col = &hffffffff
			end if
			dim fadepal as Palette16 ptr = palette16_duplicate(pal)
			Palette16_mix_n_match fadepal, white, small(1., tfrac * 2), mixBlend

			fadeopts.opacity = max_opacity * bound(2. - tfrac * 2, 0., 1.)
			frame_draw src, fadepal, x, y, trans, dest, fadeopts
			palette16_unload @fadepal

		case 15, 16  'Puff, Fade Up
			'Zoom out at same time as fading out
			dim as double zoomx = 1., zoomy = 1.
			if style = 15 then
				zoomx = 1. + 0.6 * tfrac ^ 0.5
				zoomy = zoomx
			else
				zoomy = 1. + 1.2 * tfrac ^ 2
			end if

			dim scaled as Frame ptr = frame_rotozoom(src, pal, 0, zoomx, zoomy)

			'Recenter
			fadeopts.opacity = 1 - tfrac
			if style = 15 then
				x += (src->w - scaled->w) / 2
				y += 3 * (src->h - scaled->h) / 4
				fadeopts.opacity ^= 2
			else
				y += (src->h - scaled->h)
			end if
			fadeopts.opacity *= max_opacity
			frame_draw scaled, pal, x, y, trans, dest, fadeopts
			frame_unload @scaled

		case 17  'Blip
			dim as double zoomx = 1., zoomy = 1.
			zoomx = (1 - tfrac) ^ 2
			zoomy = 2. - (1 - tfrac ^ 2)

			dim scaled as Frame ptr = frame_rotozoom(src, pal, 0, zoomx, zoomy)
			x += (src->w - scaled->w) / 2
			y += 5 * (src->h - scaled->h) / 4  'move upwards slightly
			frame_draw scaled, pal, x, y, trans, dest, opts
			frame_unload @scaled

		case else
			dim dissolved as Frame ptr
			dissolved = frame_dissolved(src, tlength, tick, style)
			frame_draw dissolved, pal, x, y, trans, dest, opts
			frame_unload @dissolved
	end select
end sub

'Warning: this is used ONLY for battle appear/death animations, it is NOT used by
'dissolving slices set to default dissolve length! (They use (w+h)/10)
function default_dissolve_time(style as integer, w as integer, h as integer) as integer
	select case style
		case 4, 6, 7, 8, 9, 11, 15, 16
			'squash, vapourise, phase out, squeeze, shrink, shrink centered, puff, fade up
			return w / 5
		case 12  'fade
			return w / 4
		case 13, 14  'ghost fade, fade to white
			return w / 3
		case 17  'blip
			return 9
		case else
			return w / 2
	end select
end function

'Returns a scaled+rotated copy.
'See also rotozoom_transform + frame_draw_transformed.
'Note: Frame masks are not supported, so can't rotate a dissolved sprite
function frame_rotozoom(src as Frame ptr, pal as Palette16 ptr = NULL, angle as double, zoomx as double, zoomy as double, smooth as integer = 0) as Frame ptr
	dim as Surface ptr in_surf, out_surf
	dim as Surface temp_surf = any
	if smooth > 0 andalso vpages_are_32bit then
		'Going to perform smoothing
		'Can't do any smoothing with an 8-bit input Surface, since the rotozoomer
		'doesn't do 8->32 bit like frame_draw can.
		in_surf = frame_to_surface32(src, curmasterpal(), pal)
	else
		'Correct but slower:
		'if gfx_surfaceCreateFrameView(src, @in_surf) then return NULL
		'Kludgy but faster, avoid a slow allocation:
		in_surf = @temp_surf
		if surfaceFrameShim(src, in_surf) then return NULL
		'We can call gfx_surfaceDestroy on in_surf, it will do nothing.
	end if
	if smooth = 2 andalso vpages_are_32bit then
		'surface_scale does much better smoothing for zoom levels < 100% (neglible
		'difference at zoom > 100%), but it's slower and doesn't support rotation.
		'Dest size axes must be >= 1
		out_surf = surface_scale(in_surf, large(1., src->w * zoomx), large(1., src->h * zoomy))
	else
		'Bilinear interpolation or no smoothing
		'Negate rotation so angle is clockwise
		out_surf = rotozoomSurface(in_surf, angle, zoomx, zoomy, smooth)
	end if
	BUG_IF(out_surf = NULL, "rotozoom returned NULL", NULL)
	dim ret as Frame ptr = frame_with_surface(out_surf)
	gfx_surfaceDestroy(@in_surf)
	gfx_surfaceDestroy(@out_surf)
	return ret
end function

'Used by frame_flip_horiz and frame_flip_vert
local sub flip_image(pixels as ubyte ptr, d1len as integer, d1stride as integer, d2len as integer, d2stride as integer)
	for x1 as integer = 0 to d1len - 1
		dim as ubyte ptr pixelp = pixels + x1 * d1stride
		for offset as integer = (d2len - 1) * d2stride to 0 step -2 * d2stride
			dim as ubyte temp = pixelp[0]
			pixelp[0] = pixelp[offset]
			pixelp[offset] = temp
			pixelp += d2stride
		next
	next
end sub

'not-in-place isometric transformation of a pixel buffer
'dimensions/strides of source is taken from src, but srcpixels specifies the actual pixel buffer
'destorigin points to the pixel in the destination buffer where the pixel at the (top left) origin should be put
local sub transform_image(src as Frame ptr, srcpixels as ubyte ptr, destorigin as ubyte ptr, d1stride as integer, d2stride as integer)
	for y as integer = 0 to src->h - 1
		dim as ubyte ptr sptr = srcpixels + y * src->pitch
		dim as ubyte ptr dptr = destorigin + y * d1stride
		for x as integer = 0 to src->w - 1
			*dptr = sptr[x]
			dptr += d2stride
		next
	next
end sub

'Public:
' flips a sprite horizontally. In place: you are only allowed to do this on sprites with no other references
sub frame_flip_horiz(spr as Frame ptr)
	if spr = 0 then exit sub
	CHECK_FRAME_8BIT(spr)
	BUG_IF(spr->refcount > 1, "illegal when refc>1")

	flip_image(spr->image, spr->h, spr->pitch, spr->w, 1)
	if spr->mask then
		flip_image(spr->mask, spr->h, spr->pitch, spr->w, 1)
	end if
end sub

'Public:
' flips a sprite vertically. In place: you are only allowed to do this on sprites with no other references
sub frame_flip_vert(spr as Frame ptr)
	if spr = 0 then exit sub
	CHECK_FRAME_8BIT(spr)
	BUG_IF(spr->refcount > 1, "illegal when refc>1")

	flip_image(spr->image, spr->w, 1, spr->h, spr->pitch)
	if spr->mask then
		flip_image(spr->mask, spr->w, 1, spr->h, spr->pitch)
	end if
end sub

'90 degree (anticlockwise) rotation.
'Unlike flipping functions, not inplace!
function frame_rotated_90(spr as Frame ptr) as Frame ptr
	if spr = 0 then return NULL
	CHECK_FRAME_8BIT(spr, NULL)

	dim ret as Frame ptr = frame_new(spr->h, spr->w, 1, (spr->mask <> NULL))

	'top left corner transformed to bottom left corner
	transform_image(spr, spr->image, ret->image + ret->pitch * (ret->h - 1), 1, -ret->pitch)

	if spr->mask <> NULL then
		transform_image(spr, spr->mask, ret->mask + ret->pitch * (ret->h - 1), 1, -ret->pitch)
	end if

	return ret
end function

'270 degree (anticlockwise) rotation, ie 90 degrees clockwise.
'Unlike flipping functions, not inplace!
function frame_rotated_270(spr as Frame ptr) as Frame ptr
	if spr = 0 then return NULL
	CHECK_FRAME_8BIT(spr, NULL)

	dim ret as Frame ptr = frame_new(spr->h, spr->w, 1, (spr->mask <> NULL))

	'top left corner transformed to top right corner
	transform_image(spr, spr->image, ret->image + (ret->w - 1), -1, ret->pitch)

	if spr->mask <> NULL then
		transform_image(spr, spr->mask, ret->mask + (ret->w - 1), -1, ret->pitch)
	end if

	return ret
end function

'Note that we clear masks to transparent! I'm not sure if this is best (not currently used anywhere), but notice that
'frame_duplicate with clr=1 does the same
sub frame_clear(spr as Frame ptr, colour as integer = 0)
	if spr->surf then
		gfx_surfaceFill(curmasterpal(colour).col, NULL, spr->surf)
		exit sub
	end if
	if spr->image then
		if spr->w = spr->pitch then
			memset(spr->image, colour, spr->w * spr->h)
		else
			for i as integer = 0 to spr->h - 1
				memset(spr->image + i * spr->pitch, colour, spr->w)
			next
		end if
	end if
	if spr->mask then
		if spr->w = spr->pitch then
			memset(spr->mask, 0, spr->w * spr->h)
		else
			for i as integer = 0 to spr->h - 1
				memset(spr->mask + i * spr->pitch, 0, spr->w)
			next
		end if
	end if
end sub

'Warning: this code is rotting; don't assume ->mask is used, etc. Anyway the whole thing should be replaced with a memmove call or two.
' function frame_scroll(spr as Frame ptr, h as integer = 0, v as integer = 0, wrap as bool = NO, direct as bool = NO) as Frame ptr
'	CHECK_FRAME_8BIT(spr, NULL)
'
' 	dim ret as Frame ptr, x as integer, y as integer
'
' 	ret = frame_clear(spr, -1)
'
' 	'first scroll horizontally
'
' 	if h <> 0 then
' 		if h > 0 then
' 			for y = 0 to spr->h - 1
' 				for x = spr->w - 1 to h step -1
' 					ret->image[y * spr->h + x] = spr->image[y * spr->h - h + x]
' 					ret->mask[y * spr->h + x] = spr->mask[y * spr->h - h + x]
' 				next
' 			next
' 			if wrap then
' 				for y = 0 to spr->h - 1
' 					for x = 0 to h - 1
' 						ret->image[y * spr->h + x] = spr->image[y * spr->h + (x + spr->w - h)]
' 						ret->mask[y * spr->h + x] = spr->mask[y * spr->h + (x + spr->w - h)]
' 					next
' 				next
' 			end if
' 		else if h < 0 then
' 			for y = 0 to spr->h - 1
' 				for x = 0 to abs(h) - 1
' 					ret->image[y * spr->h + x] = spr->image[y * spr->h - h + x]
' 					ret->mask[y * spr->h + x] = spr->mask[y * spr->h - h + x]
' 				next
' 			next
' 			if wrap then
' 				for y = 0 to spr->h - 1
' 					for x = abs(h) to spr->w - 1
' 						ret->image[y * spr->h - h + x] = spr->image[y * spr->h + x]
' 						ret->mask[y * spr->h - h + x] = spr->mask[y * spr->h + x]
' 					next
' 				next
' 			end if
' 		end if
' 	end if
'
' 	'then scroll vertically
'
' 	if v <> 0 then
'
' 	end if
'
' 	if direct then
' 		deallocate(spr->image)
' 		deallocate(spr->mask)
' 		spr->image = ret->image
' 		spr->mask = ret->mask
' 		ret->image = 0
' 		ret->mask = 0
' 		sprite_delete(@ret)
' 		return spr
' 	else
' 		return ret
' 	end if
' end function

/'
local sub grabrect(page as integer, x as integer, y as integer, w as integer, h as integer, ibuf as ubyte ptr, tbuf as ubyte ptr = 0)
'this isn't used anywhere anymore, was used to grab tiles from the tileset videopage before loadtileset
'maybe some possible future use?
'ibuf should be pre-allocated
	dim sptr as ubyte ptr
	dim as integer i, j, px, py, l

	if ibuf = null then exit sub
	CHECK_FRAME_8BIT(vpages(page))

	sptr = vpages(page)->image

	py = y
	for i = 0 to h-1
		px = x
		for j = 0 to w-1
			l = i * w + j
			'ignore clip rect, but check screen bounds
			if not (px < 0 or px >= vpages(page)->w or py < 0 or py >= vpages(page)->h) then
				ibuf[l] = sptr[(py * vpages(page)->pitch) + px]
				if tbuf then
					if ibuf[l] = 0 then tbuf[l] = &hff else tbuf[l] = 0
				end if
			else
				ibuf[l] = 0
				tbuf[l] = 0
			end if
			px += 1
		next
		py += 1
	next

end sub
'/


'==========================================================================================
'                                        Palette16
'==========================================================================================


'Note that the palette cache works completely differently to the sprite cache:
'we simply load all palettes into memory. Each cache entry will have refcount 1 if it's unused.
redim shared palcache() as Palette16 ptr

local sub Palette16_delete(f as Palette16 ptr ptr)
	if f = 0 then exit sub
	if *f = 0 then exit sub
	(*f)->refcount = FREEDREFC  'help detect double frees
	delete *f
	*f = 0
end sub

'Completely empty the Palette16 cache
local sub Palette16_empty_cache()
	for idx as integer = 0 to ubound(palcache)
		'Palettes in the cache but unused have refc=1
		if palcache(idx) andalso palcache(idx)->refcount <> 1 then
			debugc errBug, "Palette16 leak/bad refc: " & palette16_describe(palcache(idx))
			palcache(idx) = NULL
		else
			Palette16_delete(@palcache(idx))
		end if
	next
	erase palcache
end sub

'Create a new palette which is not connected to any data file
function Palette16_new(numcolors as integer = 16) as Palette16 ptr
	dim ret as Palette16 ptr
	ret = new Palette16
	ret->numcolors = numcolors
	ret->refcount = 1
	ret->palnum = -1  'Uncached
	return ret
end function

function Palette16_new_identity(numcolors as integer = 16) as Palette16 ptr
	dim ret as Palette16 ptr = Palette16_new(numcolors)
	for cidx as integer = 0 TO numcolors - 1
		ret->col(cidx) = cidx
	next
	return ret
end function

'pal() is an array of master palette indices, to convert into a Palette16
function Palette16_new_from_indices(pal() as integer) as Palette16 ptr
	BUG_IF(ubound(pal) > 255, "Palette indices pal() too long!", NULL)
	dim ret as Palette16 ptr = Palette16_new(ubound(pal) + 1)
	for idx as integer = 0 to ubound(pal)
		ret->col(idx) = pal(idx)
	next
	return ret
end function

'Loads and returns a palette from the current game (resolving -1 to default palette),
'returning a blank palette if it didn't exist.
'(Note that the blank palette isn't put in the cache, so if that palette is later
'added to the game, it won't auto-update.)
'autotype, spr: spriteset type and id, for default palette lookup.
function Palette16_load(num as integer, autotype as SpriteType = sprTypeInvalid, spr as integer = 0, expect_exists as bool = YES) as Palette16 ptr
	if num <= -1 then
		if autotype = sprTypeInvalid then
			return 0
		end if
		num = getdefaultpal(autotype, spr)
		'Returns num = -1 if the defpal file is missing
	end if

	if num >= 0 andalso num <= ubound(palcache) then
		palcache(num)->refcount += 1
		return palcache(num)
	end if

	if num >= 0 andalso expect_exists then
		' Only bother to warn if a specific palette failed to load.
		' Avoids debug noise when default palette load fails because of a non-existant defpal file
		debug "failed to load palette " & num
	end if
	' Is it a problem that this isn't put in the cache, so you can load
	' this palette multiple times and get different ptrs?
	return Palette16_new()
end function

'Open a .PAL file and pass back file handle, number of palettes, header size.
'Returns true on success.
local function open_pal_and_read_header(fname as string, byref fh as integer, byref numpalettes as integer, byref headersize as integer) as bool
	if openfile(fname, for_binary + access_read + or_error, fh) then return NO

	dim mag as short
	get #fh, 1, mag
	if mag = 4444 then
		' File is in new file format
		headersize = 16
		get #fh, , mag
		numpalettes = mag + 1
		if LOF(fh) <> 16 + 16 * numpalettes then
			debug "Mismatched .pal header (last=" & mag & ") and size " & LOF(fh)
		end if
	else
		' .pal file is still in ancient BSAVE format, with exactly 100
		' palettes. This shouldn't happen because upgrade() upgrades it.
		' Skip 7-byte BSAVE header.
		headersize = 7
		numpalettes = 100
	end if
	return YES
end function

'Load all palettes into the cache that aren't already cached.
'(Using during live-previewing because it leaves in-use entries alone)
local sub palette16_fill_cache()
	dim fh as integer
	dim numpalettes as integer
	dim headersize as integer
	'If we haven't loaded a game, and data/defaultgfx isn't available, then there's nothing to load
	dim fname as string = graphics_file("pal")
	if len(fname) = 0 then exit sub
	if open_pal_and_read_header(fname, fh, numpalettes, headersize) = NO then exit sub

	seek #fh, 1 + headersize

	if numpalettes >= 1 then
		redim preserve palcache(large(ubound(palcache), numpalettes - 1))
		for idx as integer = 0 to numpalettes - 1
			if palcache(idx) = NULL then
				seek #fh, 1 + headersize + 16 * idx
				palcache(idx) = palette16_load_pal_single(fh)
				palcache(idx)->palnum = idx
			end if
		next
	end if
	lazyclose fh
end sub

'Load all palettes into the cache, throwing out old cache if any.
sub palette16_reload_cache()
	dim starttime as double = timer
	palette16_empty_cache
	palette16_fill_cache
	debug_if_slow(starttime, 0.1, "")
end sub

'Loads and returns a palette from a .pal file. num can not be -1.
'Returns NULL if the palette doesn't exist!
function palette16_load_pal_uncached(fil as string, num as integer) as Palette16 ptr
	BUG_IF(num < 0, "negative pal num", NULL)

	dim fh as integer
	dim numpalettes as integer
	dim headersize as integer
	if open_pal_and_read_header(fil, fh, numpalettes, headersize) = NO then return NULL

	dim ret as Palette16 ptr
	if num <= numpalettes - 1 then
		seek #fh, 1 + headersize + 16 * num
		ret = palette16_load_pal_single(fh)
	end if
	lazyclose fh
	return ret
end function

local function palette16_load_pal_single(fh as integer) as Palette16 ptr
	dim ret as Palette16 ptr = Palette16_new(16)
	for idx as integer = 0 to 15
		dim byt as ubyte
		get #fh, , byt
		ret->col(idx) = byt
	next
	return ret
end function

sub Palette16_unload(palptr as Palette16 ptr ptr)
	if palptr = 0 then exit sub
	dim pal as Palette16 ptr = *palptr
	if pal = 0 then exit sub
	if pal->refcount > 0 then
		pal->refcount -= 1
	end if
	if pal->refcount <= 0 then
		if pal->refcount < 0 orelse pal->palnum >= 0 then
			'Cached palettes should never reach refc=0
			showbug "Too many frees of " & palette16_describe(pal)
		end if
		if pal->palnum < 0 then
			'Uncached palettes should be deleted when they are unloaded
			Palette16_delete(palptr)
		end if
	end if
	*palptr = 0
end sub

function Palette16_duplicate(pal as Palette16 ptr) as Palette16 ptr
	dim ret as Palette16 ptr = palette16_new(pal->numcolors)
	for i as integer = 0 to ubound(pal->col)
		ret->col(i) = pal->col(i)
	next
	'Result is uncached.
	return ret
end function

'Update a palette in the cache (or add to the cache if missing) even while possibly in use.
'(Won't update localpal in a cached PrintStrState... but PrintStrState caching isn't implemented yet)
sub Palette16_update_cache(record as integer)
	dim as Palette16 ptr oldpal, newpal

	if record > ubound(palcache) then
		palette16_fill_cache
	else
		oldpal = palcache(record)
		newpal = palette16_load_pal_uncached(graphics_file("pal"), record)

		'copy to old palette structure
		dim as integer oldrefcount = oldpal->refcount
		memcpy(oldpal, newpal, sizeof(Palette16))
		oldpal->refcount = oldrefcount
		oldpal->palnum = record

		Palette16_delete(@newpal)
	end if
end sub

function Palette16_describe(pal as Palette16 ptr) as string
	if pal = 0 then return "'(null)'"
	dim temp as string
	temp = strprintf("<Palette16 num=%d numcolors=%d refc=%d ", pal->palnum, pal->numcolors, pal->refcount)
	for idx as integer = 0 to pal->numcolors - 1
		if idx then temp &= ","
		temp &= hex(pal->col(idx))
	next
	return temp & ">"
end function

'Modifies a palette in-place, changing each color according to method, e.g. greyscale.
'Calculated using the master palette ignoring screen fades.
sub Palette16_transform_n_match(pal as Palette16 ptr, method as ColorOperator)
	for idx as integer = 0 to pal->numcolors - 1
		dim as integer r, g, b, temp
		with curmasterpal(pal->col(idx))
			if method = copLuminance then
				'Best choice for converting to grey
				r = .r * 0.3 + .g * 0.59 + .b * 0.11
				g = r
				b = r
			elseif method = copValue then
				'Just the value component of HSV, converted to grey
				r = iif(.r > .g, .r, .g)
				r = iif(r > .b, r, .b)
				g = r
				b = r
			elseif method = copTintValue then
				'Like copValue, but a produces a grey better suited for
				'being tinted using Palette16_mix_n_match:
				'only return 255 for pure white for better distrinctions,
				'and don't return 0 to allow tinting black.
				temp = iif(.r > .g, .r, .g)
				temp = iif(temp > .b, temp, .b)
				temp = (temp * 4 + .r + .g + .b + 255) \ 8
				r = temp
				g = temp
				b = temp
			end if

		end with
		pal->col(idx) = nearcolor_fast(r, g, b)  'Never 0
	next
end sub

'Modifies a palette in-place, tinting it with a color
'Calculated using the master palette ignoring screen fades.
sub Palette16_mix_n_match(pal as Palette16 ptr, byval col as RGBcolor, colfrac as double, method as ColorMixMethod, scale as double = 1.0)
	for idx as integer = 0 to pal->numcolors - 1
		dim as integer mixr, mixg, mixb
		with curmasterpal(pal->col(idx))
			if method = mixBlend then
				mixr = scale * .r * (1 - colfrac) + col.r * colfrac
				mixg = scale * .g * (1 - colfrac) + col.g * colfrac
				mixb = scale * .b * (1 - colfrac) + col.b * colfrac
			elseif method = mixMult then
				dim nonmult as double = 255 * (1 - colfrac)
				mixr = scale * .r * (nonmult + col.r * colfrac) / 255
				mixg = scale * .g * (nonmult + col.g * colfrac) / 255
				mixb = scale * .b * (nonmult + col.b * colfrac) / 255
			end if
		end with
		pal->col(idx) = nearcolor_fast(mixr, mixg, mixb)  'Never 0
	next
end sub

'This is a faster alternative to gfx_paletteFromRGB. The result doesn't need to
'be deallocated with gfx_paletteDestroy (it's a noop).
'Very similar to surfaceFrameShim
'This exists mostly as a stub, in future we might just swap the whole engine to
'RGBPalette instead of RGBcolor FB arrays to avoid conversions.
function masterpal_to_gfxpal(pal() as RGBcolor) as RGBPalette ptr
	BUG_IF(UBOUND(pal) < 256, "pal() should be length 257", NULL)
	dim ret as RGBPalette ptr = cast(RGBPalette ptr, @pal(0))
	ret->from_backend = NO
	return ret
end function

'==========================================================================================
'                            SpriteSet/Animation/SpriteState
'==========================================================================================

' Number of loops/non-forwards branches that can occur in an animation without a
' wait before it's considered to be stuck in an infinite loop.
CONST ANIMATION_LOOPLIMIT = 10

' Short names used for listing an animation
redim anim_op_names(animOpLAST) as string
anim_op_names(animOpWait) =      "wait"
anim_op_names(animOpWaitMS) =    "wait"
anim_op_names(animOpFrame) =     "frame"
anim_op_names(animOpRepeat) =    "repeat"
anim_op_names(animOpSetOffset) = "set offset"
anim_op_names(animOpRelOffset) = "add offset"

 ' Short names used for RELOAD serialisation
redim anim_op_node_names(animOpLAST) as string
anim_op_node_names(animOpWait) =      "wait"
anim_op_node_names(animOpWaitMS) =    "waitms"
anim_op_node_names(animOpFrame) =     "frame"
anim_op_node_names(animOpRepeat) =    "repeat"
anim_op_node_names(animOpSetOffset) = "setoffset"
anim_op_node_names(animOpRelOffset) = "addoffset"

' Descriptive captions
redim anim_op_fullnames(animOpLAST) as string
anim_op_fullnames(animOpWait) =      "Wait (num frames)"
anim_op_fullnames(animOpWaitMS) =    "Wait (seconds)"
anim_op_fullnames(animOpFrame) =     "Set frame"
anim_op_fullnames(animOpRepeat) =    "Repeat animation"
anim_op_fullnames(animOpSetOffset) = "Move to offset (unimp)"
anim_op_fullnames(animOpRelOffset) = "Add to offset (unimp)"

sub set_animation_framerate(ms as integer)
	' We bound to 16-200 because set_speedcontrol does the same thing
	ms_per_frame = bound(ms, 16, 200)
end sub

function ms_to_frames(ms as integer) as integer
	return large(1, INT(ms / ms_per_frame))
end function

function frames_to_ms(frames as integer) as integer
	return frames * ms_per_frame
end function

'Find a frame in a frameset, returning frame index.
'If fail = NO, then return the nearest match if the frame doesn't exist. Otherwise return -1.
'The nearest match is the previous frameid that exists
'frameset must be the first Frame in the frameset
function frameid_to_frame(frameset as Frame ptr, frameid as integer, fail as bool = NO) as integer
	dim nearest as integer = 0
	for idx as integer = 0 to frameset->arraylen - 1
		dim thisid as integer = frameset[idx].frameid
		if thisid = frameid then return idx
		if thisid < frameid then nearest = idx
	next
	if fail then return -1
	return nearest
end function

sub FrameGroupInfo.set(frameid as integer, name as string, default_num as integer)
	this.frameid = frameid
	this.name = name
	this.default_num = default_num
end sub

' This should only be called from within allmodex
constructor SpriteSet(frameset as Frame ptr)
	BUG_IF(frameset = NULL orelse frameset->arrayelem, "need first Frame in array")
	'redim animations(0 to -1)
	frames = frameset
	frameset->sprset = @this
end constructor

function SpriteSet.num_frames() as integer
	return frames->arraylen
end function

'Create a SpriteSet for a Frame if it doesn't have one
function spriteset_for_frame(fr as Frame ptr) as SpriteSet ptr
	if fr->sprset then return fr->sprset
	return new SpriteSet(fr)
end function

'A dummy SpriteSet
function empty_spriteset() as SpriteSet ptr
	dim fr as Frame ptr = frame_new(1, 1, 1)
	return new SpriteSet(fr)
end function

local function load_global_animations_uncached(sprtype as SpriteType) as SpriteSet ptr
	dim rgfxdoc as Doc ptr
	rgfxdoc = rgfx_open(sprtype, NO)
	if rgfxdoc = NULL then
		return default_global_animations(sprtype)
	end if
	dim ret as SpriteSet ptr
	ret = rgfx_load_global_animations(rgfxdoc)
	FreeDocument rgfxdoc
	return ret
end function

'Returns a dummy SpriteSet which contains the global (default) animations for a sprtype,
'loaded from the cache, or from rgfx or the defaults if missing.
'Use spriteset_unload to free the result.
'If rgfxdoc is already open you can optionally pass it to avoid reloading.
function load_global_animations(sprtype as SpriteType, rgfxdoc as Doc ptr = NULL) as SpriteSet ptr
	dim cached as Frame ptr
	cached = sprite_fetch_from_cache(sprtype, SPRITE_CACHE_GLOBAL_ANIMS)
	if cached then return cached->sprset

	dim ret as SpriteSet ptr
	if rgfxdoc then
		ret = rgfx_load_global_animations(rgfxdoc)
	else
		ret = load_global_animations_uncached(sprtype)
	end if
	if ret then
		sprite_add_cache(sprtype, SPRITE_CACHE_GLOBAL_ANIMS, ret->frames)
	end if
	return ret
end function

'Called when updating the sprite cache. Updates a SpriteSet in-place.
'Variant on rgfx_load_global_animations.
local sub reload_global_animations(def_anim as SpriteSet ptr, sprtype as SpriteType)
	dim rgfxdoc as Doc ptr = rgfx_open(sprtype, YES)
	FAIL_IF(rgfxdoc = NULL, "failed")
	'This overwrites the existing animations
	load_animations_node(DocumentRoot(rgfxdoc), def_anim)
	FreeDocument rgfxdoc
end sub

' Load a spriteset from file, or return a reference if already cached.
' WARNING: Holding onto a SpriteSet ptr in Game while live previewing would currently crash if it's reloaded!
' This increments the refcount, use spriteset_unload to decrement it, NOT 'DELETE'.
function spriteset_load(ptno as SpriteType, record as integer) as SpriteSet ptr
	' frame_load will load a Frame array with a corresponding SpriteSet
	dim frameset as Frame ptr
	frameset = frame_load(ptno, record)
	if frameset = NULL then return NULL
	return frameset->sprset
end function

' Used to decrement refcount if was loaded with spriteset_load
' (no need to call this when using frame_load and accessing Frame.sprset).
sub spriteset_unload(ss as SpriteSet ptr ptr)
	'a SpriteSet and its Frame array are never unloaded separately;
	'frame_unload is responsible for all refcounting and unloading
	if ss = NULL ORELSE *ss = NULL then exit sub
	dim temp as Frame ptr = (*ss)->frames
	frame_unload @temp
	*ss = NULL
end sub

' Increment refcount.
sub SpriteSet.reference()
	if frames then frame_reference frames
end sub

function SpriteSet.describe() as string
	return "spriteset:<" & num_frames & " frames: 0x" & hexptr(frames) _
	       & ", " & ubound(animations) & " animations>"
end function

' Searches for an animation with a certain name, or NULL if there
' are no animations with that name.
' variantname is either just the name of the animation, or the
' name plus a variant separated by a space, like "walk upleft".
' The variant is optional, and the nearest match is picked amongst animations
' which match the name:
'  - prefer variant as specified
'  - then prefer an animation with blank variant
'  - then prefer the first animation (with that name)
function SpriteSet.find_animation(variantname as string) as Animation ptr
	dim as string name, variant
	dim spacepos as integer = instr(variantname, " ")
	if spacepos then
		name = left(variantname, spacepos - 1)
		variant = mid(variantname, spacepos + 1)
	else
		name = variantname
	end if

	dim best_match as Animation ptr
	for idx as integer = 0 to ubound(animations)
		if animations(idx).name = name then
			' Right name, check how good the match is
			if animations(idx).variant = variant then
				return @animations(idx)        'Exact match
			elseif len(animations(idx).variant) then
				best_match = @animations(idx)  'Prefer nonvariant animations
			elseif best_match = NULL then
				best_match = @animations(idx)  'Otherwise, default to the first variant
			end if
		end if
	next
	return best_match
end function

' Append a new blank animation and return pointer
function SpriteSet.new_animation(name as string = "", variant as string = "") as Animation ptr
	redim preserve animations(ubound(animations) + 1)
	dim ret as Animation ptr = @animations(ubound(animations))
	ret->name = name
	ret->variant = variant
	return ret
end function


constructor Animation()
end constructor

constructor Animation(name as string, variant as string = "")
	this.name = name
	this.variant = variant
end constructor

sub Animation.append(optype as AnimOpType, arg1 as integer = 0, arg2 as integer = 0)
	redim preserve ops(ubound(ops) + 1)
	with ops(ubound(ops))
		.type = optype
		.arg1 = arg1
		.arg2 = arg2
	end with
end sub


constructor SpriteState(sprset as SpriteSet ptr)
	ss = sprset
	ss->reference()  'Inc refcount, because dec it in destructor
	frame_num = 0
end constructor

constructor SpriteState(ptno as SpriteType, record as integer)
	ss = spriteset_load(ptno, record)
	frame_num = 0
end constructor

destructor SpriteState()
	spriteset_unload @ss
end destructor

' Lookup an animation and start it. See SpriteSet.find_animation() for documentation
' of variantname (animation name plus optional variant).
' Normally an animation specifies how many times it loops (unimplemented), or ends in Repeat
' to loop forever. loopcount <> 0 overrides this, giving a fixed number of
' times to play, or < 0 to repeat forever
sub SpriteState.start_animation(variantname as string, loopcount as integer = 0)
	anim_wait = 0
	anim_step = 0
	anim_loop = loopcount
	anim_looplimit = ANIMATION_LOOPLIMIT
	anim = ss->find_animation(variantname)
end sub

function SpriteState.cur_frame() as Frame ptr
	if ss = NULL then return NULL
	if frame_num < 0 or frame_num >= ss->num_frames then return NULL
	return @ss->frames[frame_num]
end function

' Advance time until the next wait, skipping the current one, and returns number of ms that the wait was for.
' Returns -1 and does nothing if not waiting, -2 on error.
' The return value ought to be independent of ms_per_frame
' Note: any time already spent on the current wait is ignored.
function SpriteState.skip_wait() as integer
	if anim = NULL then return -2
	' Look at the current op instead of anim_wait, because it might be a wait
	' which we haven't looked at yet.
	with anim->ops(anim_step)
		if .type <> animOpWait and .type <> animOpWaitMS then
			return -1
		end if
		dim ret as integer = .arg1
		anim_wait = ms_to_frames(ret)
		if animate() = NO then ret = -2  ' Until next wait
		return ret
	end with
end function

' Advance the animation by one op.
' Returns true on success, false on an error.
' Does not check for infinite loops; caller must do that.
function SpriteState.animate_step() as bool
	if anim = NULL then return NO

	' This condition only If the animation doesn't end up looping, re
	if anim_step > ubound(anim->ops) then
		debuginfo "anim done"
		anim_looplimit -= 1
		' anim_loop = 0 means default number of loops
		if anim_loop = 0 or anim_loop = 1 then
			anim = NULL
			return YES
		end if
		if anim_loop > 0 then anim_loop -= 1
		anim_step = 0
	end if

	with anim->ops(anim_step)
		select case .type
			case animOpWait, animOpWaitMS
				' These two opcodes are identical, differing only in how
				' they are treated by the editor
				anim_wait += 1
				if anim_wait > ms_to_frames(.arg1) then
					anim_wait = 0
				else
					anim_looplimit = ANIMATION_LOOPLIMIT  'Reset
					return YES
				end if
			case animOpFrame
				/'
				if .arg1 >= ss->num_frames then
					debug "Animation '" & anim->name & "': illegal frame number " & .arg1
					anim = NULL
					return NO
				end if
				'/
				frame_num = frameid_to_frame(ss->frames, .arg1)
			case animOpRepeat
				' If a loop count was specified when playing the animation,
				' then only loop that many times, otherwise repeat forever
				if anim_loop > 0 then
					anim_loop -= 1
					if anim_loop = 0 then
						anim = NULL
						return YES
					end if
				end if
				anim_step = 0
				anim_looplimit -= 1
				return YES
			case animOpSetOffset
				offset.x = .arg1
				offset.y = .arg2
			case animOpRelOffset
				offset.x += .arg1
				offset.y += .arg2
			case else
				debug "bad animation opcode " & .type & " in '" & anim->name & "'"
				anim = NULL
				return NO
		end select
	end with
	anim_step += 1
	return YES
end function

' Advance time by one tick. True on success or finished (anim is now NULL!), false on an error/infinite loop
function SpriteState.animate() as bool
	if anim = NULL then return NO

	while anim_looplimit > 0
		if animate_step() = NO then return NO  'stop on error
		if anim_wait > 0 then return YES  'stop if waiting
		if anim = NULL then return YES  'stop if finished animating
	wend

	' Exceeded the loop limit
	debug "animation '" & anim->name & "' got stuck in an infinite loop"
	anim = NULL
	return NO
end function

/'
sub SpriteState.draw(x as integer, y as integer, trans as bool = YES, page as integer)
	dim as integer realx, realy
	realx = x + offset.x
	realy = y + offset.y
	frame_draw(cur_frame(), pal, realx, realy, trans, page)
end sub
'/

'==========================================================================================
'                           Platform specific wrapper functions
'==========================================================================================


sub show_virtual_keyboard()
	'Does nothing on platforms that have real keyboards
	debuginfo "show_virtual_keyboard"
	io_show_virtual_keyboard()
end sub

sub hide_virtual_keyboard()
	'Does nothing on platforms that have real keyboards
	debuginfo "hide_virtual_keyboard"
	io_hide_virtual_keyboard()
end sub

sub show_virtual_gamepad()
	'Does nothing on platforms that have real keyboards
	io_show_virtual_gamepad()
end sub

sub hide_virtual_gamepad()
	'Does nothing on platforms that have real keyboards
	io_hide_virtual_gamepad()
end sub

sub remap_android_gamepad(player as integer, gp as GamePadMap)
	'Does nothing on non-android non-ouya platforms
	'debuginfo "remap_android_gamepad " & gp.Ud & " " & gp.Rd & " " & gp.Dd & " " & gp.Ld & " " & gp.A & " " & gp.B & " " & gp.X & " " & gp.Y & " " & gp.L1 & " " & gp.R1 & " " & gp.L2 & " " & gp.R2
	io_remap_android_gamepad(player, gp)
end sub

sub remap_touchscreen_button (button_id as integer, ohr_scancode as integer)
	'Does nothing on platforms without touch screens
	'debuginfo "remap_android_gamepad " & button_id & " " & ohr_scancode
	io_remap_touchscreen_button(button_id, ohr_scancode)
end sub

function running_on_desktop() as bool
#IFDEF __FB_ANDROID__
	return NO
#ELSE
	return YES
#ENDIF
end function

function running_on_console() as bool
	'Currently supports OUYA, GameStick, Fire-TV
#IFDEF __FB_ANDROID__
	static cached as bool = NO
	static cached_result as bool
	if not cached then
		cached_result = io_running_on_console()
		cached = YES
	end if
	return cached_result
#ELSE
	return NO
#ENDIF
end function

function running_on_ouya() as bool
'Only use this for things that strictly require OUYA, like the OUYA store
#IFDEF __FB_ANDROID__
	static cached as bool = NO
	static cached_result as bool
	if not cached then
		cached_result = io_running_on_ouya()
		cached = YES
	end if
	return cached_result
#ELSE
	return NO
#ENDIF
end function

function running_on_mobile() as bool
#IFDEF __FB_ANDROID__
	'--return true for all Android except OUYA
	static cached as bool = NO
	static cached_result as bool
	if not cached then
		cached_result = NOT io_running_on_console()
		cached = YES
	end if
	return cached_result
#ELSE
	return NO
#ENDIF
end function

function get_safe_zone_margin () as integer
	'--returns and integer from 0 to 10 representing the percentage
	' of the screen edges reserved for TV safe zones. Only returns non-zero
	' values on backends that support this feature.
	dim margin as integer = int(gfx_get_safe_zone_margin() * 100)
	return large(0, small(10, margin))
end function

sub set_safe_zone_margin (margin as integer)
	'the margin argument is an integer from 0 to 10 representing
	' the percentage of the screen edges reserved for TV safe zones.
	' this has no effect on backends that don't support this feature.
	margin = bound(margin, 0, 10)
	gfx_set_safe_zone_margin(margin / 100)
end sub

function supports_safe_zone_margin () as bool
	'Returns YES if the current backend supports safe zone margins
	return gfx_supports_safe_zone_margin()
end function

sub ouya_purchase_request (dev_id as string, identifier as string, key_der as string)
	'Only works on OUYA. Should do nothing on other platforms
	debug "ouya_purchase_request for product " & identifier
	gfx_ouya_purchase_request(dev_id, identifier, key_der)
end sub

function ouya_purchase_is_ready () as bool
	'Wait until the OUYA store has replied. Always return YES on other platforms
	return gfx_ouya_purchase_is_ready()
end function

function ouya_purchase_succeeded () as bool
	'Returns YES if the OUYA purchase was completed successfully.
	'Always returns NO on other platforms
	return gfx_ouya_purchase_succeeded()
end function

sub ouya_receipts_request (dev_id as string, key_der as string)
	'Start a request for reciepts. They may take some time.
	'Does nothing if the platform is not OUYA
	gfx_ouya_receipts_request(dev_id, key_der)
end sub

function ouya_receipts_are_ready () as bool
	'Wait until the OUYA store has replied. Always return YES on other platforms
	return gfx_ouya_receipts_are_ready ()
end function

function ouya_receipts_result () as string
	'Returns a newline delimited list of OUYA product identifiers that
	'have already been purchased.
	'Always returns "" on other platforms
	return gfx_ouya_receipts_result()
end function

sub email_files(address as string, subject as string, message as string, file1 as zstring ptr = NULL, file2 as zstring ptr = NULL, file3 as zstring ptr = NULL)
	debuginfo "Emailing " & *file1 & " " & *file2 & " " & *file3 & " to " & address
	debuginfo " subject: '" & subject & "' body: '" & message & "'"
	#ifdef __FB_ANDROID__
		' Omitted files should be NULL, not "".
		if len(*file1) = 0 then file1 = NULL
		if len(*file2) = 0 then file2 = NULL
		if len(*file3) = 0 then file3 = NULL
		SDL_ANDROID_EmailFiles(address, subject, message, file1, file2, file3)
	#else
		debug "email_files only supported on Android"
	#endif
end sub
