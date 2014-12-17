'The OHRRPGCE graphics, audio and user input library!
'Please read LICENSE.txt for GNU GPL license details and disclaimer of liability
'
'This module is completely bool-clean (bool always used when appropriate)

#include "config.bi"
#include "crt/limits.bi"
#include "common.bi"
#include "allmodex.bi"
#include "gfx.bi"
#include "gfx_newRenderPlan.bi"
#include "music.bi"
#include "reload.bi"
#include "util.bi"
#include "const.bi"
#include "uiconst.bi"

using Reload

#ifdef IS_GAME
declare sub exitprogram (byval need_fade_out as bool, byval errorout as integer = 0)
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

declare sub drawohr(byval src as Frame ptr, byval dest as Frame ptr, byval pal as Palette16 ptr = null, byval x as integer, byval y as integer, byval trans as bool = YES)
'declare sub grabrect(byval page as integer, byval x as integer, byval y as integer, byval w as integer, byval h as integer, ibuf as ubyte ptr, tbuf as ubyte ptr = 0)
declare function write_bmp_header(f as string, byval w as integer, byval h as integer, byval bitdepth as integer) as integer
declare function decode_bmp_bitmask(mask as uint32) as integer
declare sub loadbmp32(byval bf as integer, byval surf as Surface ptr, infohd as BITMAPV3INFOHEADER)
declare sub loadbmp24(byval bf as integer, byval surf as Surface ptr)
declare sub loadbmp8(byval bf as integer, byval fr as Frame ptr)
declare sub loadbmp4(byval bf as integer, byval fr as Frame ptr)
declare sub loadbmp1(byval bf as integer, byval fr as Frame ptr)
declare sub loadbmprle8(byval bf as integer, byval fr as Frame ptr)
declare sub loadbmprle4(byval bf as integer, byval fr as Frame ptr)
declare function quantise_surface(surf as Surface ptr, pal() as RGBcolor, firstindex as integer, options as integer = 0) as Frame ptr

declare sub snapshot_check

declare function calcblock(tmap as TileMap, byval x as integer, byval y as integer, byval overheadmode as integer, pmapptr as TileMap ptr) as integer

declare sub font_unload(byval font as Font ptr)

declare sub pollingthread(byval as any ptr)
declare sub update_inputtext ()

declare sub record_input_tick ()
declare sub replay_input_tick ()

declare function hexptr(p as any ptr) as string


'------------ Global variables ------------

dim modex_initialised as bool = NO
dim vpages() as Frame ptr
dim vpagesp as Frame ptr ptr  'points to vpages(0) for debugging: fbc outputs typeless debugging symbol
dim disable_native_text_input as bool = NO
redim fonts(3) as Font

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

'--------- Module shared variables ---------

'For each vpage() element, this records whether it shouldn't be resized when the window size changes (normally is)
'(Not fully implemented, as it seems it would only benefit textbox_appearance_editor)
'dim shared fixedsize_vpages() as bool
dim shared wrkpage as integer  'used by some legacy modex functions. Usually points at clippedframe
dim shared clippedframe as Frame ptr  'used to track which Frame the clips are set for.
dim shared as integer clipl, clipt, clipr, clipb 'drawable area on clippedframe; right, bottom margins are excluded

'The current internal size of the window (takes effect at next setvispage).
'Should only be modified via set_resolution and unlock_resolution
dim shared windowsize as XYPair = (320, 200)
'Minimum window size; can't resize width or height below this. Default to (0,0): no bound
dim shared minwinsize as XYPair
dim shared resizing_enabled as bool = NO  'keeps track of backend state

'storeset/loadset stuff
dim shared bptr as integer ptr	' buffer
dim shared bsize as integer  'record size, in BYTES
dim shared bpage as integer  'Ye Olde Garbage

dim shared bordertile as integer

dim shared anim1 as integer
dim shared anim2 as integer

dim shared waittime as double
dim shared flagtime as double = 0.0
dim shared setwait_called as bool
dim shared tickcount as integer = 0
dim shared use_speed_control as bool = YES

dim shared last_setkeys_time as double
dim shared setkeys_elapsed_ms as integer       'Time since last setkeys call (used by keyval)
dim shared keybd(-1 to 127) as integer         'keyval array
dim shared key_down_ms(-1 to 127) as integer   'ms each key has been down
dim shared last_keybd(127) as integer          'used only for input recording
dim shared keyrepeatwait as integer = 500
dim shared keyrepeatrate as integer = 55
dim shared diagonalhack as integer
dim shared delayed_alt_keydown as bool = NO
dim shared inputtext as string
dim shared inputtext_enabled as bool = NO

dim shared rec_input as bool = NO
dim shared rec_input_file as integer     'file handle
dim shared play_input as bool = NO
dim shared play_input_file as integer    'file handle
dim shared replaytick as integer
dim shared debug_replay as bool = NO     'set to YES by editing this line; maybe add a commandline option

dim shared closerequest as integer = NO

dim shared keybdmutex as any ptr         'controls access to keybdstate(), mouseflags, mouselastflags, and various backend functions
dim shared keybdthread as any ptr        'id of the polling thread
dim shared endpollthread as bool         'signal the polling thread to quit
dim shared keybdstate(127) as integer    '"real"time keyboard array
dim shared mouseflags as integer
dim shared mouselastflags as integer
dim shared cursorvisible as bool = YES

'State saved from one readmouse call to the next
dim shared mouse_lastpos as XYPair       'Position at last readmouse call
dim shared mouse_clickstart as XYPair
dim shared mouse_dragmask as integer
dim shared mouse_moved_since_setkeys as bool
dim shared mouse_clicks_since_setkeys as integer

dim shared textfg as integer
dim shared textbg as integer

dim shared intpal(0 to 255) as RGBcolor	 'current palette
dim shared updatepal as bool             'setpal called, load new palette at next setvispage

dim shared fpsframes as integer = 0
dim shared fpstime as double = 0.0
dim shared fpsstring as string
dim shared showfps as bool = NO

MAKETYPE_DoubleList(SpriteCacheEntry)
MAKETYPE_DListItem(SpriteCacheEntry)
'WARNING: don't add strings to this
type SpriteCacheEntry
	'cachelist used only if object is a member of sprcacheB
	cacheB as DListItem(SpriteCacheEntry)
	hashed as HashedItem
	p as frame ptr
	cost as integer
	Bcached as bool
end type

dim shared sprcache as HashTable
dim shared sprcacheB as DoubleList(SpriteCacheEntry)
dim shared sprcacheB_used as integer    'number of slots full
'dim shared as integer cachehit, cachemiss

dim shared mouse_grab_requested as bool = NO
dim shared mouse_grab_overridden as bool = NO
dim shared remember_mouse_grab(3) as integer = {-1, -1, -1, -1}

dim shared remember_title as string


'==========================================================================================
'                                Initialisation and shutdown
'==========================================================================================


sub modex_init()
	'initialise software gfx library

	redim vpages(3)
	'redim fixedsize_vpages(3)  'Initially all NO
	vpagesp = @vpages(0)
	for i as integer = 0 to 3
		vpages(i) = frame_new(320, 200, , YES)
	next
	'other vpages slots are for temporary pages

	setclip , , , , 0

	hash_construct(sprcache, offsetof(SpriteCacheEntry, hashed))
	dlist_construct(sprcacheB.generic, offsetof(SpriteCacheEntry, cacheB))
	sprcacheB_used = 0
end sub

sub setmodex()
	modex_init()

	'initialise graphics and io

	gfx_backend_init(@post_terminate_signal, "FB_PROGRAM_ICON")

	'init vars
	for i as integer = 0 to 127
		keybd(i) = 0
		keybdstate(i) = 0
		key_down_ms(i) = 0
	next
	endpollthread = NO
	mouselastflags = 0
	mouseflags = 0

	keybdmutex = mutexcreate
	if wantpollingthread then
		keybdthread = threadcreate(@pollingthread)
	end if

	io_init()
	'mouserect(-1,-1,-1,-1)

	fpstime = TIMER
	fpsframes = 0
	fpsstring = ""

	if gfx_supports_variable_resolution() = NO then
		debuginfo "Resolution changing not supported"
	end if

	modex_initialised = YES
end sub

sub modex_quit()
	'clean up software gfx library

	for i as integer = 0 to ubound(vpages)
		frame_unload(@vpages(i))
	next
	for i as integer = 0 to ubound(fonts)
		font_unload(@fonts(i))
	next

	hash_destruct(sprcache)
	'debug "cachehit = " & cachehit & " mis == " & cachemiss

	releasestack
end sub

sub restoremode()
	if modex_initialised = NO then exit sub
	modex_initialised = NO

	'clean up io stuff
	if keybdthread then
		endpollthread = YES
		threadwait keybdthread
		keybdthread = 0
	end if
	mutexdestroy keybdmutex

	gfx_close()

	modex_quit
end sub

sub mersenne_twister (byval seed as double)
	IF play_input ORELSE rec_input THEN exit sub 'Seeding not allowed in play/record modes
	RANDOMIZE seed, 3
	debuginfo "mersenne_twister seed=" & seed
end sub

sub settemporarywindowtitle (title as string)
	'just like setwindowtitle but does not memorize the title
	mutexlock keybdmutex
	gfx_windowtitle(title)
	mutexunlock keybdmutex
end sub

sub setwindowtitle (title as string)
	remember_title = title
	mutexlock keybdmutex
	gfx_windowtitle(title)
	mutexunlock keybdmutex
end sub


'==========================================================================================
'                                        Video pages
'==========================================================================================


sub freepage (byval page as integer)
	if page < 0 orelse page > ubound(vpages) orelse vpages(page) = NULL then
		debug "Tried to free unallocated/invalid page " & page
		exit sub
	end if

	if page = wrkpage then wrkpage = 0 'no setclip call: legacy wrkpage code doesn't even use clips anyway
	frame_unload(@vpages(page))
end sub

'Adds a Frame ptr to vpages(), returning its index.
function registerpage (byval spr as Frame ptr) as integer
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

function allocatepage(byval w as integer = -1, byval h as integer = -1) as integer
	if w < 0 then w = windowsize.w
	if h < 0 then h = windowsize.h
	dim fr as Frame ptr = frame_new(w, h, , YES)

	dim ret as integer = registerpage(fr)
	frame_unload(@fr) 'we're not hanging onto it, vpages() is

	return ret
end function

'creates a copy of a page, registering it (must be freed)
function duplicatepage (byval page as integer) as integer
	dim fr as Frame ptr = frame_duplicate(vpages(page))
	dim ret as integer = registerpage(fr)
	frame_unload(@fr) 'we're not hanging onto it, vpages() is
	return ret
end function

'Copy contents of one page onto another
'should copying to a page of different size resize that page?
sub copypage (byval src as integer, byval dest as integer)
	'if vpages(src)->w <> vpages(dest)->w or vpages(src)->h <> vpages(dest)->h then
	'	debug "warning, copied to page of unequal size"
	'end if
	frame_draw vpages(src), , 0, 0, , NO, vpages(dest)
end sub

sub clearpage (byval page as integer, byval colour as integer = -1)
	if colour = -1 then colour = uilook(uiBackground)
	frame_clear vpages(page), colour
end sub

'The contents are either trimmed or extended with colour 0.
sub resizepage (page as integer, w as integer, h as integer)
	if vpages(page) = NULL then
		showerror "resizepage called with null ptr"
		exit sub
	end if
	dim newpage as Frame ptr
	newpage = frame_new(w, h, , YES)
	frame_draw vpages(page), NULL, 0, 0, 1, 0, newpage
	frame_unload @vpages(page)
	vpages(page) = newpage
end sub

private function compatpage_internal(pageframe as Frame ptr) as Frame ptr
	return frame_new_view(vpages(vpage), (vpages(vpage)->w - 320) / 2, (vpages(vpage)->h - 200) / 2, 320, 200)
end function

'Return a video page which is a view on vpage hat is 320x200 (or smaller) and centred.
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
'resize all videopages (except compatpages) to the new window size.
'The videopages are either trimmed or extended with colour 0.
private sub screen_size_update ()
	'Changes windowsize if user tried to resize, otherwise does nothing
	if gfx_get_resize(windowsize) then
		'debuginfo "User window resize to " & windowsize.w & "*" & windowsize.h
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
		if vpages(page) andalso vpages(page)->isview = NO then
			if vpages(page)->w <> windowsize.w or vpages(page)->h <> windowsize.h then
				'debug "screen_size_update: resizing page " & page & " -> " & windowsize.w & "*" & windowsize.h
				resizepage page, windowsize.w, windowsize.h
			end if
		end if
	next

	'Scan for compatpages (we're assuming all views are compatpages, which isn't true in
	'general, but currently true when setvispage is called) and replace each with a new view
	'onto the same page if it changed.
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
end sub

'Makes the window resizeable, and sets a minimum size.
'Whenever the window is resized all videopages (except compatpages) are resized to match.
sub unlock_resolution (byval min_w as integer, byval min_h as integer)
	minwinsize.w = min_w
	minwinsize.h = min_h
	if gfx_supports_variable_resolution() = NO then
		exit sub
	end if
	debuginfo "unlock_resolution " & min_w & "*" & min_h
	resizing_enabled = gfx_set_resizable(YES, minwinsize.w, minwinsize.h)
	windowsize.w = large(windowsize.w, minwinsize.w)
	windowsize.h = large(windowsize.h, minwinsize.h)
	screen_size_update
end sub

'Disable window resizing.
sub lock_resolution ()
	resizing_enabled = gfx_set_resizable(NO, 0, 0)
end sub

'Set the window size, if possible, subject to min size bound. Doesn't modify resizability state.
'This will resize all videopages (except compatpages) to the new window size.
sub set_resolution (byval w as integer, byval h as integer)
	if gfx_supports_variable_resolution() = NO then
		exit sub
	end if
	debuginfo "set_resolution " & w & "*" & h
	windowsize.w = large(w, minwinsize.w)
	windowsize.h = large(h, minwinsize.h)
	screen_size_update
end sub

'The current internal window size in pixels (actual window updated at next setvispage)
function get_resolution_w() as integer
	return windowsize.w
end function

'The current internal window size in pixels (actual window updated at next setvispage)
function get_resolution_h() as integer
	return windowsize.h
end function


'==========================================================================================
'                                   setvispage and Fading
'==========================================================================================


'Display a videopage. May modify the page!
'Also resizes all videopages to match the window size
sub setvispage (byval page as integer)
	dim starttime as double = timer
	dim starttime2 as double
	if gfx_supports_variable_resolution() = NO then
		'Safety check. We must stick to 320x200, otherwise the backend could crash.
		'In future backends should be updated to accept other sizes even if they only support 320x200
		'(Actually gfx_directx appears to accept other sizes, but I can't test)
		if vpages(page)->w <> 320 or vpages(page)->h <> 200 then
			resizepage page, 320, 200
			showerror "setvispage: page was not 320x200 even though gfx backend forbade it"
		end if
	end if

	with *vpages(page)
		fpsframes += 1
		if timer > fpstime + 1 then
			fpsstring = "fps:" & INT(10 * fpsframes / (timer - fpstime)) / 10
			fpstime = timer
			fpsframes = 0
		end if
		if showfps then
			'NOTE: this is bad if displaying a page other than vpage/dpage!
			edgeprint fpsstring, vpages(page)->w - 65, vpages(page)->h - 10, uilook(uiText), page
		end if
		'rectangle .w - 6, .h - 6, 6, 6, uilook(uiText), page

		'the fb backend may freeze up if it collides with the polling thread
		mutexlock keybdmutex
		if updatepal then
			starttime2 = timer
			gfx_setpal(@intpal(0))
			debug_if_slow(starttime2, 0.05, "gfx_setpal")
			updatepal = NO
		end if
		starttime2 = timer
		gfx_showpage(.image, .w, .h)
		debug_if_slow(starttime2, 0.1, "gfx_showpage")
		mutexunlock keybdmutex
	end with

	'After presenting the page this is a good time to check for window size changes and
	'resize the videopages as needed before the next frame is rendered.
	screen_size_update
	debug_if_slow(starttime, 0.2, "")
end sub

sub setpal(pal() as RGBcolor)
	memcpy(@intpal(0), @pal(0), 256 * SIZEOF(RGBcolor))

	updatepal = YES
end sub

sub fadeto (byval red as integer, byval green as integer, byval blue as integer)
	dim i as integer
	dim j as integer
	dim diff as integer

	if updatepal then
		mutexlock keybdmutex
		gfx_setpal(@intpal(0))
		mutexunlock keybdmutex
		updatepal = NO
	end if

	for i = 1 to 32
		setwait 17 ' aim to complete fade in 550ms
		for j = 0 to 255
			'red
			diff = intpal(j).r - red
			if diff > 0 then
				intpal(j).r -= iif(diff >= 8, 8, diff)
			elseif diff < 0 then
				intpal(j).r -= iif(diff <= -8, -8, diff)
			end if
			'green
			diff = intpal(j).g - green
			if diff > 0 then
				intpal(j).g -= iif(diff >= 8, 8, diff)
			elseif diff < 0 then
				intpal(j).g -= iif(diff <= -8, -8, diff)
			end if
			'blue
			diff = intpal(j).b - blue
			if diff > 0 then
				intpal(j).b -= iif(diff >= 8, 8, diff)
			elseif diff < 0 then
				intpal(j).b -= iif(diff <= -8, -8, diff)
			end if
		next
		mutexlock keybdmutex
		gfx_setpal(@intpal(0))
		mutexunlock keybdmutex
		dowait
	next
	'Make sure the palette gets set on the final pass

	'This function was probably called in the middle of timed loop, call
	'setwait to avoid "dowait without setwait" warnings
	setwait 0
end sub

sub fadetopal (pal() as RGBcolor)
	dim i as integer
	dim j as integer
	dim diff as integer

	if updatepal then
		mutexlock keybdmutex
		gfx_setpal(@intpal(0))
		mutexunlock keybdmutex
		updatepal = NO
	end if

	for i = 1 to 32
		setwait 17 ' aim to complete fade in 550ms
		for j = 0 to 255
			'red
			diff = intpal(j).r - pal(j).r
			if diff > 0 then
				intpal(j).r -= iif(diff >= 8, 8, diff)
			elseif diff < 0 then
				intpal(j).r -= iif(diff <= -8, -8, diff)
			end if
			'green
			diff = intpal(j).g - pal(j).g
			if diff > 0 then
				intpal(j).g -= iif(diff >= 8, 8, diff)
			elseif diff < 0 then
				intpal(j).g -= iif(diff <= -8, -8, diff)
			end if
			'blue
				diff = intpal(j).b - pal(j).b
			if diff > 0 then
				intpal(j).b -= iif(diff >= 8, 8, diff)
			elseif diff < 0 then
				intpal(j).b -= iif(diff <= -8, -8, diff)
			end if
		next
		mutexlock keybdmutex
		gfx_setpal(@intpal(0))
		mutexunlock keybdmutex
		dowait
	next

	'This function was probably called in the middle of timed loop, call
	'setwait to avoid "dowait without setwait" warnings
	setwait 0
end sub

#define POINT_CLIPPED(x, y) ((x) < clipl orelse (x) > clipr orelse (y) < clipt orelse (y) > clipb)

#define PAGEPIXEL(x, y, p) vpages(p)->image[vpages(p)->pitch * (y) + (x)]
#define FRAMEPIXEL(x, y, fr) fr->image[fr->pitch * (y) + (x)]


'==========================================================================================
'                                     Waits/Framerate
'==========================================================================================


sub enable_speed_control(byval setting as bool = YES)
	use_speed_control = setting
end sub

sub setwait (byval t as integer, byval flagt as integer = 0)
't is a value in milliseconds which, in the original, is used to set the event
'frequency and is also used to set the wait time, but the resolution of the
'dos timer means that the latter is always truncated to the last multiple of
'55 milliseconds. We won't do this anymore. Try to make the target framerate.
'flagt, if nonzero, is a count in milliseconds for the secondary timer, which is
'accessed as the return value from dowait.
	if use_speed_control = NO then exit sub
	'Min wait: 60fps, max wait: 1.5x requested
	waittime = bound(waittime + t / 1000, timer + 0.017, timer + t / 667)
	if flagt = 0 then
		flagt = t
	end if
	if timer > flagtime then
		flagtime = bound(flagtime + flagt / 1000, timer + 0.017, timer + flagt / 667)
	end if
	setwait_called = YES
end sub

function get_tickcount() as integer
	return tickcount
end function

function dowait () as bool
'wait until alarm time set in setwait()
'returns true if the flag time has passed (since the last time it was passed)
'In freebasic, sleep is in 1000ths, and a value of less than 100 will not
'be exited by a keypress, so sleep for 5ms until timer > waittime.
	if use_speed_control = NO then tickcount += 1 : return YES
	dim i as integer
	do while timer <= waittime
		i = bound((waittime - timer) * 1000, 1, 5)
		sleep i
		io_waitprocessing()
	loop
	if setwait_called then
		setwait_called = NO
	else
		debug "dowait called without setwait"
	end if
	tickcount += 1
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
	music_close
	sound_close
end sub

sub resetsfx ()
	sound_reset
end sub

sub loadsong (f as string)
	'check for extension
	dim ext as string
	dim songname as string
	dim songtype as MusicFormatEnum

	songname = f
	songtype = getmusictype(f)

	music_play(songname, songtype)
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

sub set_music_volume (byval vol as single)
	music_setvolume(vol)
end sub

function getmusictype (file as string) as MusicFormatEnum
	if file = "" then
		'no further checking for blank names
		return 0
	end if

	if isdir(file) OR right(file, 1) = SLASH then
		'no further checking if this is a directory
		return 0
	end if

	dim ext as string, chk as integer
	ext = lcase(justextension(file))

	'special case
	if str(cint(ext)) = ext then return FORMAT_BAM

	select case ext
	case "bam"
		chk = FORMAT_BAM
	case "mid"
		chk = FORMAT_MIDI
	case "xm"
		chk = FORMAT_XM
	case "it"
		chk = FORMAT_IT
	case "wav"
		chk = FORMAT_WAV
	case "ogg"
		chk = FORMAT_OGG
	case "mp3"
		chk = FORMAT_MP3
	case "s3m"
		chk = FORMAT_S3M
	case "mod"
		chk = FORMAT_MOD
	case else
		debug "unknown format: " & file & " - " & ext
		chk = 0
	end select

	return chk
end function


'==========================================================================================
'                                      Sound effects
'==========================================================================================


function isawav(fi as string) as bool
	if not isfile(fi) then return NO 'duhhhhhh

#define ID(a,b,c,d) asc(a) SHL 0 + asc(b) SHL 8 + asc(c) SHL 16 + asc(d) SHL 24
	dim _RIFF as integer = ID("R","I","F","F") 'these are the "signatures" of a
	dim _WAVE as integer = ID("W","A","V","E") 'wave file. RIFF is the format,
	'dim _fmt_ as integer = ID("f","m","t"," ") 'WAVE is the type, and fmt_ and
	'dim _data as integer = ID("d","a","t","a") 'data are the chunks
#undef ID

	dim chnk_ID as integer
	dim chnk_size as integer
	dim fh as integer = freefile
	open fi for binary access read as #fh

	get #fh, , chnk_ID
	if chnk_ID <> _RIFF then
		close #fh
		return NO 'not even a RIFF file
	end if

	get #fh, , chnk_size 'don't care

	get #fh, , chnk_ID

	if chnk_ID <> _WAVE then
		close #fh
		return NO 'not a WAVE file, pffft
	end if

	'is this good enough? meh, sure.
	close #fh
	return YES
end function

sub playsfx (byval num as integer, byval l as integer=0)
	sound_play(num, l)
end sub

sub stopsfx (byval num as integer)
	sound_stop (num)
end sub

sub pausesfx (byval num as integer)
	sound_pause(num)
end sub

sub freesfx (byval num as integer)
	sound_free(num)
end sub

function sfxisplaying(byval num as integer) as bool
	return sound_playing(num)
end function


'==========================================================================================
'                                      Keyboard input
'==========================================================================================

function keyval (byval a as integer, byval repeat_wait as integer = 0, byval repeat_rate as integer = 0) as integer
'except for special keys (like -1), each key reports 3 bits:
'
'bit 0: key was down at the last setkeys call
'bit 1: keypress event (either new keypress, or key-repeat) during last setkey-setkey interval
'bit 2: new keypress during last setkey-setkey interval
'
'Note: Alt/Ctrl keys may behave strangely with gfx_fb (and old gfx_directx):
'You won't see Left/Right keypresses even when scAlt/scCtrl is pressed, so do not
'check "keyval(scLeftAlt) > 0 or keyval(scRightAlt) > 0" instead of "keyval(scAlt) > 0"

	dim result as integer = keybd(a)
	if a >= 0 then
		if repeat_wait = 0 then repeat_wait = keyrepeatwait
		if repeat_rate = 0 then repeat_rate = keyrepeatrate

		'awful hack to avoid arrow keys firing alternatively when not pressed at the same time:
		'save state of the first arrow key you query
		dim arrowkey as bool = NO
		if a = scLeft or a = scRight or a = scUp or a = scDown then arrowkey = YES
		if arrowkey and diagonalhack <> -1 then return (result and 5) or (diagonalhack and keybd(a) > 0)

		if key_down_ms(a) >= repeat_wait then
			dim check_repeat as bool = YES

			'if a = scAlt then
				'alt can repeat (probably a bad idea not to), but only if nothing else has been pressed
				'for i as integer = 1 to &h7f
				'	if keybd(i) > 1 then check_repeat = NO
				'next
				'if delayed_alt_keydown = NO then check_repeat = NO
			'end if

			'Don't fire repeat presses for special toggle keys (note: these aren't actually
			'toggle keys in all backends, eg. gfx_fb)
			if a = scNumlock or a = scCapslock or a = scScrolllock then check_repeat = NO

			if check_repeat then
				'Keypress event at "wait + i * rate" ms after keydown
				dim temp as integer = key_down_ms(a) - repeat_wait
				if temp \ repeat_rate > (temp - setkeys_elapsed_ms) \ repeat_rate then result or= 2
			end if
			if arrowkey then diagonalhack = result and 2
		end if
	end if
	return result
end function

sub setkeyrepeat (byval repeat_wait as integer = 500, byval repeat_rate as integer = 55)
	keyrepeatwait = repeat_wait
	keyrepeatrate = repeat_rate
end sub

function get_ascii_inputtext () as string
	dim shift as integer = 0
	dim ret as string

	if keyval(scCtrl) > 0 then return ""

	if keyval(scShift) and 1 then shift += 1
	if keyval(scAlt) and 1 then shift += 2   'for characters 128 and up

	for i as integer = 0 to 53
		dim effective_shift as integer = shift
		if shift <= 1 andalso keyval(scCapsLock) > 0 then
			select case i
				case scQ to scP, scA to scL, scZ to scM
					effective_shift xor= 1
			end select
		end if
		if keyval(i) > 1 then
			ret &= key2text(effective_shift, i)
		end if
	next i

	'Space missing from key2text
	if keyval(scSpace) > 1 then ret &= " "

	return ret
end function

private sub update_inputtext ()
	if disable_native_text_input then
		inputtext = get_ascii_inputtext()
		exit sub
	end if

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

	if force_native_input = NO andalso keyval(scAlt) and 1 then
		'Throw away w_in
		inputtext = get_ascii_inputtext()
		exit sub
	end if

	if io_textinput then
		'if len(w_in) then print #fh, "input :" & w_in
		'convert to ascii
		inputtext = ""
		DIM force_shift as bool = NO
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
				inputtext += "?"
			elseif (w_in[i] < 32) or (w_in[i] >= &h7F and w_in[i] <= &hA0) then
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
				inputtext += ch
			end if
		next
	else
		inputtext = get_ascii_inputtext()
	end if
end sub

'If using gfx_sdl and gfx_directx this is Latin-1, while gfx_fb doesn't currently support even that
function getinputtext () as string
	'Only show this message if getinputtext is called incorrectly twice in a row,
	'to filter out instances when a menu with inputtext disabled exits back to
	'one that expects it enabled, and getinputtext is called before the next call to setkeys.
	static last_call_was_bad as bool = NO
	if inputtext_enabled = NO and last_call_was_bad then
		debuginfo "getinputtext: not enabled"
	end if
	last_call_was_bad = (inputtext_enabled = NO)

	return inputtext
end function

'Checks the keyboard and optionally joystick for keypress events.
'Returns scancode if one is found, 0 otherwise.
'Use this instead of looping over all keys, to make sure alt filtering and joysticks work
function anykeypressed (byval checkjoystick as bool = YES) as integer
	dim as integer joybutton, joyx, joyy

	for i as integer = 0 to &h7f
		'check scAlt only, so Alt-filtering (see setkeys) works
		if i = scLeftAlt or i = scRightAlt or i = scUnfilteredAlt then continue for
		if keyval(i) > 1 then
			return i
		end if
	next
	if checkjoystick then
		if io_readjoysane(0, joybutton, joyx, joyy) then
			for i as integer = 16 to 1 step -1
				if joybutton and (i ^ 2) then return 127 + i
			next i
		end if
	end if
end function

'Returns a scancode or joystick button scancode
function waitforanykey () as integer
	dim as integer key, sleepjoy = 3

	setkeys
	do
		setwait 40
		io_pollkeyevents()
		setkeys
		key = anykeypressed(sleepjoy = 0)
		if key then
			return key
		end if
		if sleepjoy > 0 then
			sleepjoy -= 1
		end if
		dowait
	loop
end function

'Without changing the results of keyval or readmouse, check whether a key has been pressed,
'mouse button clicked, or window close requested since the last call to setkeys.
'NOTE: any such keypresses are lost! This is OK for the current purposes
function interrupting_keypress () as bool
	io_pollkeyevents()

	dim keybd_dummy(-1 to 127) as integer
	dim mouse as MouseInfo

	mutexlock keybdmutex
	io_keybits(@keybd_dummy(0))
	io_mousebits(mouse.x, mouse.y, mouse.wheel, mouse.buttons, mouse.clicks)
	mutexunlock keybdmutex

	if closerequest then
		'closerequest = NO
		keybd_dummy(-1) = 1
	end if
	if keybd_dummy(scPageup) > 0 and keybd_dummy(scPagedown) > 0 and keybd_dummy(scEsc) > 1 then keybd_dummy(-1) = 1

	'Quick abort (could probably do better, just moving this here for now)
	if keybd_dummy(-1) then
#ifdef IS_GAME
		'uncomment for slice debugging
		'DestroyGameSlices YES
		exitprogram NO
#else
		return YES
#endif
	end if

	for i as integer = 0 to 127
		'Check for new keypresses
		if keybd_dummy(i) and 2 then return YES
	next

	if mouse.clicks then return YES

	return NO
end function

sub setkeys_update_keybd
	dim winstate as WindowState ptr
	winstate = gfx_getwindowstate()

	mutexlock keybdmutex
	io_keybits(@keybd(0))
	mutexunlock keybdmutex

	'State of keybd(0 to 127) at this point:
	'bit 0: key currently down
	'bit 1: key down since last io_keybits call
	'bit 2: zero
	'(keybd(-1) is special)

	'debug "raw scEnter = " & keybd(scEnter) & " scAlt = " & keybd(scAlt)

	'DELETEME (after a lag period): This is a temporary fix for gfx_directx not knowing about scShift
	'(or any other of the new scancodes, but none of the rest matter much (maybe
	'scPause) since there are no games that use them).
	'(Ignore bit 2, because that isn't set yet)
	if ((keybd(scLeftShift) or keybd(scRightShift)) and 3) <> (keybd(scShift) and 3) then
		keybd(scShift) = keybd(scLeftShift) or keybd(scRightShift)
	end if

	'Backends don't know about scAlt, only scUnfilteredAlt
	keybd(scAlt) = keybd(scUnfilteredAlt)

	'Don't fire ctrl presses when alt down due to large number of WM shortcuts containing ctrl+alt
	'(Testing delayed_alt_keydown is just a hack to add one tick delay after alt up,
	'which is absolutely required)
	if (keybd(scAlt) and 1) or delayed_alt_keydown then

		if keybd(scEnter) and 6 then
			keybd(scEnter) and= 1
			delayed_alt_keydown = NO
		end if

		keybd(scCtrl) and= 1
		keybd(scLeftCtrl) and= 1
		keybd(scRightCtrl) and= 1
	end if

	'Calculate new "new keypress" bit (bit 2)
	for a as integer = 0 to &h7f
		keybd(a) and= 3
		if a = scAlt then
			'Special behaviour for alt, to ignore pesky WM shortcuts like alt+tab, alt+enter:
			'Wait until alt has been released, without losing focus, before
			'causing a key-down event.
			'Also, special case for alt+enter, since that doesn't remove focus

			'Note: this is only for scAlt, not scLeftAlt, scRightAlt, which aren't used by
			'the engine, only by games. Maybe those shoudl be blocked too
			'Note: currently keyval causes key-repeat events for alt if delayed_alt_keydown = YES

			if keybd(scAlt) and 2 then
				if delayed_alt_keydown = NO then
					keybd(scAlt) -= 2
				end if
				delayed_alt_keydown = YES
			end if

			/'
			for scancode as integer = 0 to &h7f
				if scancode <> scUnfilteredAlt and scancode <> scAlt and scancode <> scLeftAlt and scancode <> scRightAlt and (keybd(scancode) and 1) then
					delayed_alt_keydown = NO
				end if
			next
			'/
			if winstate andalso winstate->focused = NO then
				delayed_alt_keydown = NO
			end if

			if (keybd(scAlt) and 1) = 0 andalso delayed_alt_keydown then
				keybd(scAlt) or= 6
				delayed_alt_keydown = NO
			end if

		'elseif a = scCtrl or a = scLeftCtrl or a = scRightCtrl then

		else
			'Duplicate bit 1 to bit 2
			 keybd(a) or= (keybd(a) and 2) shl 1
		end if
	next

end sub

sub update_keydown_times ()
	for a as integer = 0 to &h7f
		if (keybd(a) and 4) or (keybd(a) and 1) = 0 then
			key_down_ms(a) = 0
		end if
		if keybd(a) and 1 then
			key_down_ms(a) += setkeys_elapsed_ms
		end if
	next
end sub

sub setkeys (byval enable_inputtext as bool = NO)
'Updates the keybd array (which keyval() wraps) to reflect new keypresses
'since the last call, also clears all keypress events (except key-is-down)
'
'Also the place for low-level key hooks that work everywhere
'(Note that backends also have some hooks, especially gfx_sdl.bas for OSX-
'specific stuff)
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
'Note that key repeat is NOT added to keybd (it's done by "post-processing" in keyval)

	dim starttime as double = timer

	if enable_inputtext then enable_inputtext = YES
	if inputtext_enabled <> enable_inputtext then
		inputtext_enabled = enable_inputtext
		io_enable_textinput(inputtext_enabled)
	end if

	if play_input then
		'Updates keybd(), setkeys_elapsed_ms, inputtext
		replay_input_tick ()

		update_keydown_times ()
	else
		setkeys_update_keybd ()

		setkeys_elapsed_ms = bound(1000 * (TIMER - last_setkeys_time), 0, 255)
		last_setkeys_time = TIMER

		update_keydown_times ()

		'AFAIK, this is will still work on all platforms except X11 with SDL
		'even if inputtext was not enabled; however you'll get a warning when
		'getinputtext is called. So we call this just so that making that error
		'isn't too annoying (you'll still notice it)
		update_inputtext()

		if rec_input then
			record_input_tick ()
		end if
	end if

	'reset arrow key fire state
	diagonalhack = -1

	'Check to see if the backend has received a request
	'to close the window (eg. clicking the X), set the magic keyboard
	'index -1 if so. It can only be unset with clearkey.
	if closerequest then
		closerequest = NO
		keybd(-1) = 1
	end if
	if keybd(scPageup) > 0 and keybd(scPagedown) > 0 and keybd(scEsc) > 1 then keybd(-1) = 1

#ifdef IS_CUSTOM
	if keybd(-1) then keybd(scEsc) = 7
#elseif defined(IS_GAME)
	'Quick abort (could probably do better, just moving this here for now)
	if keyval(-1) then
		'uncomment for slice debugging
		'DestroyGameSlices YES
		exitprogram NO
	end if
#endif

	'Taking a screenshot with gfx_directx is very slow, so avoid timing that
	debug_if_slow(starttime, 0.005, play_input)

	'F12 for screenshots handled here
	snapshot_check

	if keyval(scCtrl) > 0 and keyval(scTilde) and 4 then
		showfps xor= 1
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
			resizing_enabled = gfx_set_resizable(resizing_enabled xor YES, minwinsize.w, minwinsize.h)
		end if
	end if

	if mouse_grab_requested then
#IFDEF __FB_DARWIN__
		if keyval(scF14) > 1 then
			clearkey(scF14)
#ELSE
		if keyval(scScrollLock) > 1 then
			clearkey(scScrollLock)
#endIF
			mouserect -1, -1, -1, -1
			mouse_grab_requested = YES
			mouse_grab_overridden = YES
		end if
	end if
	
	mouse_moved_since_setkeys = NO
	mouse_clicks_since_setkeys = 0
end sub

sub clearkey(byval k as integer)
	keybd(k) = 0
	if k >= 0 then
		key_down_ms(k) = 0
	end if
end sub

'Set keyval(-1) on. So ugly
sub setquitflag ()
	keybd(-1) = 1
end sub

sub post_terminate_signal cdecl ()
	closerequest = YES
end sub


'==========================================================================================
'                                     Mouse and joystick
'==========================================================================================


function havemouse() as bool
'atm, all backends support the mouse, or don't know
	 return YES
end function

sub hidemousecursor ()
	io_setmousevisibility(0)
	cursorvisible = NO
end sub

sub unhidemousecursor ()
	io_setmousevisibility(-1)
	io_mouserect(-1, -1, -1, -1)
	cursorvisible = YES
end sub

function mousecursorvisible () as bool
	return cursorvisible
end function

function readmouse () as MouseInfo
	dim starttime as double = timer
	dim info as MouseInfo

	mutexlock keybdmutex   'is this necessary?
	io_mousebits(info.x, info.y, info.wheel, info.buttons, info.clicks)
	mutexunlock keybdmutex

	'gfx_fb/sdl/alleg return last onscreen position when the mouse is offscreen
	'gfx_fb: If you release a mouse button offscreen, it becomes stuck (FB bug)
	'        wheel scrolls offscreen are registered when you move back onscreen
	'gfx_alleg: button state continues to work offscreen but wheel scrolls are not registered
	'gfx_sdl: button state works offscreen. wheel state not implemented yet

	if mouse_dragmask then
		'Test whether drag ended
		if (info.clicks and mouse_dragmask) orelse (info.buttons and mouse_dragmask) = 0 then
			mouse_dragmask = 0
			mouse_clickstart = TYPE<XYPair>(0, 0)
		end if
	end if

	if mouse_dragmask = 0 then
		'Dragging is only tracked for a single button at a time, and clickstart is not updated
		'while dragging either. So we may now test for new drags or clicks.
		for i as integer = 0 to 2
			dim mask as integer = 2 ^ i
			if info.clicks and mask then
				'Do not flag as dragging until the second tick
				mouse_clickstart = TYPE<XYPair>(info.x, info.y)
			elseif info.buttons and mask then
				'left mouse button down, but no new click this tick
				mouse_dragmask = mask
				exit for
			end if
		next
	end if

	info.moved = (mouse_lastpos.x <> info.x OR mouse_lastpos.y <> info.y)
	mouse_lastpos = Type(info.x, info.y)
	info.dragging = mouse_dragmask
	info.clickstart = mouse_clickstart
	
	if mouse_moved_since_setkeys ORELSE info.moved then
		mouse_moved_since_setkeys = YES
		info.movedtick = YES
	end if
	
	if mouse_clicks_since_setkeys ORELSE info.clicks then
		mouse_clicks_since_setkeys OR= info.clicks
		info.clickstick = mouse_clicks_since_setkeys
	end if

	if info.clicks <> 0 then
		if mouse_grab_requested andalso mouse_grab_overridden then
			mouserect remember_mouse_grab(0), remember_mouse_grab(1), remember_mouse_grab(2), remember_mouse_grab(3)
		end if
	end if

	debug_if_slow(starttime, 0.005, info.clicks)
	return info
end function

sub movemouse (byval x as integer, byval y as integer)
	io_setmouse(x, y)
end sub

sub mouserect (byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)
	if gfxbackend = "fb" or gfxbackend = "sdl" then
		if xmin = -1 and xmax = -1 and ymin = -1 and ymax = -1 then
			mouse_grab_requested = NO
			settemporarywindowtitle remember_title
		else
			remember_mouse_grab(0) = xmin
			remember_mouse_grab(1) = xmax
			remember_mouse_grab(2) = ymin
			remember_mouse_grab(3) = ymax
			mouse_grab_requested = YES
			mouse_grab_overridden = NO
#IFDEF __FB_DARWIN__
			settemporarywindowtitle remember_title & " (F14 to free mouse)"
#ELSE
			settemporarywindowtitle remember_title & " (ScrlLock to free mouse)"
#endIF
		end if
	end if
	mutexlock keybdmutex
	io_mouserect(xmin, xmax, ymin, ymax)
	mutexunlock keybdmutex
end sub

function readjoy (joybuf() as integer, byval jnum as integer) as bool
'Return false if joystick is not present, or true if joystick is present
'jnum is the joystick to read (QB implementation supports 0 and 1)
'joybuf(0) = Analog X axis (scaled to -100 to 100)
'joybuf(1) = Analog Y axis
'joybuf(2) = button 1: 0=pressed nonzero=not pressed
'joybuf(3) = button 2: 0=pressed nonzero=not pressed
'Other values in joybuf() should be preserved.
'If X and Y axis are not analog,
'  upward motion when joybuf(0) < joybuf(9)
'  down motion when joybuf(0) > joybuf(10)
'  left motion when joybuf(1) < joybuf(11)
'  right motion when joybuf(1) > joybuf(12)
	dim as integer buttons, x, y
	if io_readjoysane(jnum, buttons, x, y) = 0 then return 0

	joybuf(0) = x
	joybuf(1) = y
	joybuf(2) = (buttons AND 1) = 0 '0 = pressed, not 0 = unpressed (why???)
	joybuf(3) = (buttons AND 2) = 0 'ditto
	return -1
end function

function readjoy (byval joynum as integer, byref buttons as integer, byref x as integer, byref y as integer) as bool
	return io_readjoysane(joynum, buttons, x, y)
end function


'==========================================================================================
'                       Compat layer for old graphics backend IO API
'==========================================================================================


'these are wrappers provided by the polling thread
sub io_amx_keybits cdecl (byval keybdarray as integer ptr)
	for a as integer = 0 to &h7f
		keybdarray[a] = keybdstate(a)
		keybdstate(a) = keybdstate(a) and 1
	next
end sub

sub io_amx_mousebits cdecl (byref mx as integer, byref my as integer, byref mwheel as integer, byref mbuttons as integer, byref mclicks as integer)
	'get the mouse state one last time, for good measure
	io_getmouse(mx, my, mwheel, mbuttons)
	mclicks = mouseflags or (mbuttons and not mouselastflags)
	mouselastflags = mbuttons
	mouseflags = 0
	mbuttons = mbuttons or mclicks
end sub

private sub pollingthread(byval unused as any ptr)
	dim as integer a, dummy, buttons

	while endpollthread = NO
		mutexlock keybdmutex

		io_updatekeys(@keybdstate(0))
		'set key state for every key
		'highest scancode in fbgfx.bi is &h79, no point overdoing it
		for a = 0 to &h7f
			if keybdstate(a) and 8 then
				'decide whether to set the 'new key' bit, otherwise the keystate is preserved
				if (keybdstate(a) and 1) = 0 then
					'this is a new keypress
					keybdstate(a) = keybdstate(a) or 2
				end if
			end if
			'move the bit (clearing it) that io_updatekeys sets from 8 to 1
			keybdstate(a) = (keybdstate(a) and 2) or ((keybdstate(a) shr 3) and 1)
		next

		io_getmouse(dummy, dummy, dummy, buttons)
		mouseflags = mouseflags or (buttons and not mouselastflags)
		mouselastflags = buttons

		mutexunlock keybdmutex

		'25ms was found to be sufficient
		sleep 25
	wend
end sub


'==========================================================================================
'                                  Recording and replay
'==========================================================================================


sub start_recording_input (filename as string)
	if play_input then
		debug "Can't record input because already replaying input!"
		exit sub
	end if
	if isfile(filename) then
		debug "Replacing the input recording that already existed at """ & filename & """"
		safekill filename
	end if
	rec_input_file = FREEFILE
	open filename for binary access write as #rec_input_file
	dim header as string = "OHRRPGCEkeys"
	PUT #rec_input_file,, header
	dim ohrkey_ver as integer = 4
	PUT #rec_input_file,, ohrkey_ver
	dim seed as double = TIMER
	RANDOMIZE seed, 3
	PUT #rec_input_file,, seed
	rec_input = YES
	debuginfo "Recording keyboard input to: """ & filename & """"
	for i as integer = 0 to ubound(last_keybd)
		last_keybd(i) = 0
	next i
end sub

sub stop_recording_input ()
	if rec_input then
		close #rec_input_file
		rec_input = NO
		debuginfo "STOP recording input"
	end if
end sub

sub start_replaying_input (filename as string)
	if rec_input then
		debug "Can't replay input because already recording input!"
		exit sub
	end if
	play_input_file = FREEFILE
	open filename for binary access read as #play_input_file
	play_input = YES
	dim header as string = STRING(12, 0)
	GET #play_input_file,, header
	if header <> "OHRRPGCEkeys" then
		stop_replaying_input "No OHRRPGCEkeys header in """ & filename & """"
		exit sub
	end if
	dim ohrkey_ver as integer = -1
	GET #play_input_file,, ohrkey_ver
	if ohrkey_ver <> 4 then
		stop_replaying_input "Unknown ohrkey version code " & ohrkey_ver & " in """ & filename & """. Only know how to understand version 4"
		exit sub
	end if
	dim seed as double
	GET #play_input_file,, seed
	RANDOMIZE seed, 3
	debuginfo "Replaying keyboard input from: """ & filename & """"
	replaytick = -1
	for i as integer = 0 to ubound(keybd)
		keybd(i) = 0
	next i
end sub

sub stop_replaying_input (msg as string="", byval errorlevel as ErrorLevelEnum = errError)
	if msg <> "" then
		debugc errorlevel, msg
	end if
	if play_input then
		close #play_input_file
		play_input = NO
		debugc errorlevel, "STOP replaying input"
	end if
end sub

sub record_input_tick ()
	static tick as integer = -1
	tick += 1
	dim presses as ubyte = 0
	dim keys_down as integer = 0
	for i as integer = 0 to ubound(keybd)
		if keybd(i) <> last_keybd(i) then
			presses += 1
		end if
		if keybd(i) then keys_down += 1  'must record setkeys_elapsed_ms
	next i
	if presses = 0 and keys_down = 0 and len(inputtext) = 0 then exit sub
	PUT #rec_input_file,, tick
	PUT #rec_input_file,, cubyte(setkeys_elapsed_ms)
	PUT #rec_input_file,, presses
	for i as ubyte = 0 to ubound(keybd)
		if keybd(i) <> last_keybd(i) then
			PUT #rec_input_file,, i
			PUT #rec_input_file,, cubyte(keybd(i))
			last_keybd(i) = keybd(i)
		end if
	next i
	'Currently inputtext is Latin-1, format will need changing in future
	PUT #rec_input_file,, cubyte(len(inputtext))
	PUT #rec_input_file,, inputtext
end sub

sub replay_input_tick ()
	static tick as integer = -1
	tick += 1
	do
		if EOF(play_input_file) then
			stop_replaying_input "The end of the input playback file was reached.", errInfo
			exit sub
		end if
		dim fpos as integer = LOC(play_input_file)
		if replaytick = -1 then
			GET #play_input_file,, replaytick
		end if
		if replaytick < tick and replaytick <> -1 then
			debug "input replay late for tick " & replaytick & " (" & replaytick - tick & ")"
		elseif replaytick > tick then
			'debug "saving replay input tick " & replaytick & " until its time has come (+" & replaytick - tick & ")"
			for i as integer = 0 to 127
				if keybd(i) then
					' There ought to be a tick in the input file so that we can set setkeys_elapsed_ms correctly
					debug "bad recorded key input: key " & i & " is down, but expected tick " & tick & " is missing"
					exit for
				end if
			next
			' Otherwise, this doesn't matter as it won't be used
			setkeys_elapsed_ms = 1
			inputtext = ""
			exit sub
		end if

		dim tick_ms as ubyte
		GET #play_input_file,, tick_ms
		setkeys_elapsed_ms = tick_ms
		dim presses as ubyte
		GET #play_input_file,, presses
		if presses < 0 orelse presses > ubound(keybd) + 1 then
			stop_replaying_input "input replay tick " & replaytick & " has invalid number of keypresses " & presses
			exit sub
		end if

		dim as string info
		if debug_replay then
			info = "L:" & fpos & " T:" & replaytick & " ms:" & setkeys_elapsed_ms & " ("
		end if

		dim key as ubyte
		dim kb as ubyte
		for i as integer = 1 to presses
			GET #play_input_file,, key
			GET #play_input_file,, kb
			keybd(key) = kb
			if debug_replay then info &= " " & scancodename(key) & "=" & kb
		next i
		info &= " )"
		dim input_len as ubyte
		GET #play_input_file,, input_len
		if input_len then
			'Currently inputtext is Latin-1, format will need changing in future
			inputtext = space(input_len)
			GET #play_input_file,, inputtext
			if debug_replay then info &= " input: '" & inputtext & "'"
		else
			inputtext = ""
		end if

		if debug_replay then debuginfo info

		'In case the replay somehow became out of sync, keep looping
		'(Probably hopeless though)
		if replaytick = tick then
			replaytick = -1
			exit sub
		end if
		replaytick = -1
	loop
end sub


'==========================================================================================
'                                      Map rendering
'==========================================================================================


function readblock (map as TileMap, byval x as integer, byval y as integer) as integer
	if x < 0 OR x >= map.wide OR y < 0 OR y >= map.high then
		debug "illegal readblock call " & x & " " & y
		exit function
	end if
	return map.data[x + y * map.wide]
end function

sub writeblock (map as TileMap, byval x as integer, byval y as integer, byval v as integer)
	if x < 0 OR x >= map.wide OR y < 0 OR y >= map.high then
		debug "illegal writeblock call " & x & " " & y
		exit sub
	end if
	map.data[x + y * map.wide] = v
end sub

private function calcblock (tmap as TileMap, byval x as integer, byval y as integer, byval overheadmode as integer, pmapptr as TileMap ptr) as integer
'returns -1 to draw no tile
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
			debugc errPromptBug, "NULL passmap ptr"
			block = -1
		elseif x >= pmapptr->wide or y >= pmapptr->high then
			if overheadmode = 2 then block = -1
		elseif ((readblock(*pmapptr, x, y) and passOverhead) <> 0) xor (overheadmode = 2) then
			block = -1
		end if
	end if

	return block
end function

sub drawmap (tmap as TileMap, byval x as integer, byval y as integer, byval tileset as TilesetData ptr, byval p as integer, byval trans as bool = NO, byval overheadmode as integer = 0, byval pmapptr as TileMap ptr = NULL, byval ystart as integer = 0, byval yheight as integer = -1)
	'overrides setanim
	anim1 = tileset->tastuf(0) + tileset->anim(0).cycle
	anim2 = tileset->tastuf(20) + tileset->anim(1).cycle
	drawmap tmap, x, y, tileset->spr, p, trans, overheadmode, pmapptr, ystart, yheight
end sub

sub drawmap (tmap as TileMap, byval x as integer, byval y as integer, byval tilesetsprite as Frame ptr, byval p as integer, byval trans as bool = NO, byval overheadmode as integer = 0, byval pmapptr as TileMap ptr = NULL, byval ystart as integer = 0, byval yheight as integer = -1, byval largetileset as bool = NO)
'ystart is the distance from the top to start drawing, yheight the number of lines. yheight=-1 indicates extend to bottom of screen
'There are no options in the X direction because they've never been used, and I don't forsee them being (can use Frames or slices instead)
	dim mapview as Frame ptr
	mapview = frame_new_view(vpages(p), 0, ystart, vpages(p)->w, iif(yheight = -1, vpages(p)->h, yheight))
	drawmap tmap, x, y, tilesetsprite, mapview, trans, overheadmode, pmapptr, largetileset
	frame_unload @mapview
end sub

sub drawmap (tmap as TileMap, byval x as integer, byval y as integer, byval tilesetsprite as Frame ptr, byval dest as Frame ptr, byval trans as bool = NO, byval overheadmode as integer = 0, byval pmapptr as TileMap ptr = NULL, byval largetileset as bool = NO)
'This version of drawmap paints over the entire dest Frame given to it.
'x and y are the camera position at the top left corner of the Frame, not
'the position at which the top left of the map is drawn: this is the OPPOSITE
'to all other drawing commands!
'overheadmode = 0 : draw all tiles normally
'overheadmode = 1 : draw non overhead tiles only (to avoid double draw)
'overheadmode = 2 : draw overhead tiles only
'largetileset : A hack which disables tile animation, instead using tilesets with 256 tiles

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
	dim tileframe as frame

	if clippedframe <> dest then
		setclip , , , , dest
	end if

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

	tileframe.w = 20
	tileframe.h = 20
	tileframe.pitch = 20

	ty = yoff
	while ty < dest->h
		tx = xoff
		xpos = xstart
		while tx < dest->w
			todraw = calcblock(tmap, xpos, ypos, overheadmode, pmapptr)
			if (largetileset = NO ANDALSO todraw >= 160) then
				if (todraw > 207) then
					todraw = (todraw - 48 + anim2) MOD 160
				else
					todraw = (todraw + anim1) MOD 160
				end if
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
				drawohr(@tileframe, dest, , tx, ty, trans)
			end if

			tx = tx + 20
			xpos = xpos + 1
		wend
		ty = ty + 20
		ypos = ypos + 1
	wend
end sub

sub setanim (byval cycle1 as integer, byval cycle2 as integer)
	anim1 = cycle1
	anim2 = cycle2
end sub

sub setoutside (byval defaulttile as integer)
	bordertile = defaulttile
end sub


'==========================================================================================
'                                 Old graphics API wrappers
'==========================================================================================


sub drawsprite (pic() as integer, byval picoff as integer, pal() as integer, byval po as integer, byval x as integer, byval y as integer, byval page as integer, byval trans as bool = YES)
'draw sprite from pic(picoff) onto page using pal() starting at po
	drawspritex(pic(), picoff, pal(), po, x, y, page, 1, trans)
end sub

sub bigsprite (pic() as integer, pal() as integer, byval p as integer, byval x as integer, byval y as integer, byval page as integer, byval trans as bool = YES)
	drawspritex(pic(), 0, pal(), p, x, y, page, 2, trans)
end sub

sub hugesprite (pic() as integer, pal() as integer, byval p as integer, byval x as integer, byval y as integer, byval page as integer, byval trans as bool = YES)
	drawspritex(pic(), 0, pal(), p, x, y, page, 4, trans)
end sub

function Palette16_new_from_buffer(pal() as integer, byval po as integer) as Palette16 ptr
	dim ret as Palette16 ptr
	dim word as integer
	ret = allocate(sizeof(Palette16))

	for i as integer = 0 to 15
		'palettes are interleaved like everything else
		word = pal((po + i) \ 2)	' get color from palette
		if (po + i) mod 2 = 1 then
			ret->col(i) = (word and &hff00) shr 8
		else
			ret->col(i) = word and &hff
		end if
	next
	return ret
end function

'Convert a (deprecated) pixel array representation of a 4 bit sprite to a Frame
function frame_new_from_buffer(pic() as integer, byval picoff as integer) as Frame ptr
	dim sw as integer
	dim sh as integer
	dim hspr as frame ptr
	dim dspr as ubyte ptr
	dim nib as integer
	dim i as integer
	dim spix as integer
	dim row as integer

	sw = pic(picoff)
	sh = pic(picoff+1)
	picoff = picoff + 2

	hspr = frame_new(sw, sh)
	dspr = hspr->image

	'now do the pixels
	'pixels are in columns, so this might not be the best way to do it
	'maybe just drawing straight to the screen would be easier
	nib = 0
	row = 0
	for i = 0 to (sw * sh) - 1
		select case nib			' 2 bytes = 4 nibbles in each int
			case 0
				spix = (pic(picoff) and &hf000) shr 12
			case 1
				spix = (pic(picoff) and &h0f00) shr 8
			case 2
				spix = (pic(picoff) and &hf0) shr 4
			case 3
				spix = pic(picoff) and &h0f
				picoff = picoff + 1
		end select
		*dspr = spix				' set image pixel
		dspr = dspr + sw
		row = row + 1
		if (row >= sh) then	'ugh
			dspr = dspr - (sw * sh)
			dspr = dspr + 1
			row = 0
		end if
		nib = nib + 1
		nib = nib and 3
	next
	return hspr
end function

sub drawspritex (pic() as integer, byval picoff as integer, pal() as integer, byval po as integer, byval x as integer, byval y as integer, byval page as integer, byval scale as integer, byval trans as bool = YES)
'draw sprite scaled, used for drawsprite(x1), bigsprite(x2) and hugesprite(x4)
	if clippedframe <> vpages(page) then
		setclip , , , , page
	end if

	'convert the buffer into a Frame
	dim hspr as frame ptr
	dim hpal as Palette16 ptr
	hspr = frame_new_from_buffer(pic(), picoff)
	hpal = Palette16_new_from_buffer(pal(), po)

	'now draw the image
	frame_draw(hspr, hpal, x, y, scale, trans, page)
	'what a waste
	frame_unload(@hspr)
	deallocate(hpal)
end sub

sub wardsprite (pic() as integer, byval picoff as integer, pal() as integer, byval po as integer, byval x as integer, byval y as integer, byval page as integer, byval trans as bool = YES)
'this just draws the sprite mirrored
'the coords are still top-left
	dim sw as integer
	dim sh as integer
	dim hspr as frame ptr
	dim dspr as ubyte ptr
	dim nib as integer
	dim i as integer
	dim spix as integer
	dim pix as integer
	dim row as integer

	if clippedframe <> vpages(page) then
		setclip , , , , page
	end if

	sw = pic(picoff)
	sh = pic(picoff+1)
	picoff = picoff + 2

	hspr = frame_new(sw, sh)
	dspr = hspr->image
	dspr = dspr + sw - 1 'jump to last column

	'now do the pixels
	'pixels are in columns, so this might not be the best way to do it
	'maybe just drawing straight to the screen would be easier
	nib = 0
	row = 0
	for i = 0 to (sw * sh) - 1
		select case nib			' 2 bytes = 4 nibbles in each int
			case 0
				spix = (pic(picoff) and &hf000) shr 12
			case 1
				spix = (pic(picoff) and &h0f00) shr 8
			case 2
				spix = (pic(picoff) and &hf0) shr 4
			case 3
				spix = pic(picoff) and &h0f
				picoff = picoff + 1
		end select
		if spix = 0 and trans then
			pix = 0					' transparent
		else
			'palettes are interleaved like everything else
			pix = pal((po + spix) \ 2)	' get color from palette
			if (po + spix) mod 2 = 1 then
				pix = (pix and &hff00) shr 8
			else
				pix = pix and &hff
			end if
		end if
		*dspr = pix				' set image pixel
		dspr = dspr + sw
		row = row + 1
		if (row >= sh) then	'ugh
			dspr = dspr - (sw * sh)
			dspr = dspr - 1		' right to left for wardsprite
			row = 0
		end if
		nib = nib + 1
		nib = nib and 3
	next

	'now draw the image
	frame_draw(hspr, NULL, x, y, , trans, page)

	frame_unload(@hspr)
end sub

sub stosprite (pic() as integer, byval picoff as integer, byval x as integer, byval y as integer, byval page as integer)
'This is the opposite of loadsprite, ie store raw sprite data in screen p
'starting at x, y.
	dim i as integer
	dim poff as integer
	dim toggle as integer
	dim sbytes as integer
	dim h as integer
	dim w as integer

	if clippedframe <> vpages(page) then
		setclip , , , , page
	end if

	poff = picoff
	h = pic(poff)
	w = pic(poff + 1)
	poff += 2
	sbytes = ((w * h) + 1) \ 2	'only 4 bits per pixel

	y += x \ 320
	x = x mod 320

	'copy from passed int buffer, with 2 bytes per int as usual
	toggle = 0
	for i = 0 to sbytes - 1
		if toggle = 0 then
			PAGEPIXEL(x, y, page) = (pic(poff) and &hff00) shr 8
			toggle = 1
		else
			PAGEPIXEL(x, y, page) = pic(poff) and &hff
			toggle = 0
			poff += 1
		end if
		x += 1
		if x = 320 then
			y += 1
			x = 0
		end if
	next

end sub

sub loadsprite (pic() as integer, byval picoff as integer, byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval page as integer)
'reads sprite from given page into pic(), starting at picoff
	dim i as integer
	dim poff as integer
	dim toggle as integer
	dim sbytes as integer
	dim temp as integer

	if clippedframe <> vpages(page) then
		setclip , , , , page
	end if

	sbytes = ((w * h) + 1) \ 2	'only 4 bits per pixel

	y += x \ 320
	x = x mod 320

	'copy to passed int buffer, with 2 bytes per int as usual
	toggle = 0
	poff = picoff
	pic(poff) = w			'these are 4byte ints, not compat w. orig.
	pic(poff+1) = h
	poff += 2
	for i = 0 to sbytes - 1
		temp = PAGEPIXEL(x, y, page)
		if toggle = 0 then
			pic(poff) = temp shl 8
		else
			pic(poff) = pic(poff) or temp
			poff += 1
		end if
		toggle xor= 1
		x += 1
		if x = 320 then
			y += 1
			x = 0
		end if
	next

end sub

sub getsprite (pic() as integer, byval picoff as integer, byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval page as integer)
'This reads a rectangular region of a screen page into sprite buffer array pic() at picoff
'It assumes that all the pixels it encounters will be colors 0-15 of the master palette
'even though those colors will certainly be mapped to some other 16 color palette when drawn
	dim as ubyte ptr sbase, sptr
	dim nyb as integer = 0
	dim p as integer = 0
	dim as integer sw, sh

	'store width and height
	p = picoff
	pic(p) = w
	p += 1
	pic(p) = h
	p += 1

	'find start of image
	sbase = vpages(page)->image + (vpages(page)->pitch * y) + x

	'pixels are stored in columns for the sprites (argh)
	for sh = 0 to small(w, vpages(page)->w)  - 1
		sptr = sbase
		for sw = 0 to small(h, vpages(page)->h) - 1
			select case nyb
				case 0
					pic(p) = (*sptr and &h0f) shl 12
				case 1
					pic(p) = pic(p) or ((*sptr and &h0f) shl 8)
				case 2
					pic(p) = pic(p) or ((*sptr and &h0f) shl 4)
				case 3
					pic(p) = pic(p) or (*sptr and &h0f)
					p += 1
			end select
			sptr += vpages(page)->pitch
			nyb += 1
			nyb = nyb and &h03
		next
		sbase = sbase + 1 'next col
	next

end sub


'==========================================================================================
'                                     Old allmodex IO
'==========================================================================================


sub storeset (fil as string, byval i as integer, byval l as integer)
' i = index, l = line (only if reading from screen buffer)
	dim starttime as double = timer
	dim f as integer
	dim idx as integer
	dim bi as integer
	dim ub as ubyte
	dim toggle as integer
	dim sptr as ubyte ptr

	if NOT fileiswriteable(fil) then exit sub
	f = freefile
	open fil for binary access read write as #f

	seek #f, (i*bsize) + 1 'does this work properly with write?
	'this is a horrible hack to get 2 bytes per integer, even though
	'they are 4 bytes long in FB
	bi = 0
	toggle = 0
	if bpage >= 0 then
		'read from screen
		sptr = vpages(wrkpage)->image
		sptr = sptr + (vpages(wrkpage)->pitch * l)
		idx = bsize
		while idx > vpages(wrkpage)->w
			fput(f, , sptr, vpages(wrkpage)->w)
			idx -= vpages(wrkpage)->w
			sptr += vpages(wrkpage)->pitch
		wend
		fput(f, , sptr, idx)
	else
		'debug "buffer size to read = " + str(bsize)
		for idx = 0 to bsize - 1 ' this will be slow
			if toggle = 0 then
				ub = bptr[bi] and &hff
				toggle = 1
			else
				ub = (bptr[bi] and &hff00) shr 8
				toggle = 0
				bi = bi + 1
			end if
			put #f, , ub
		next
	end if

	close #f
	debug_if_slow(starttime, 0.1, fil)
end sub

sub loadset (fil as string, byval i as integer, byval l as integer)
' i = index, l = line (only if reading to screen buffer)
'Obsolete, use loadrecord instead
'Note: This is extremely slow when reading past end of file because fread buffering internal stuff
	dim starttime as double = timer
	dim f as integer
	dim idx as integer
	dim bi as integer
	dim ub as ubyte
	dim toggle as integer
	dim sptr as ubyte ptr

	if NOT fileisreadable(fil) then exit sub
	if i < 0 then debug "loadset: attempt to read index " & i & " of """ & fil & """": exit sub
	f = freefile
	open fil for binary access read as #f

	seek #f, (i*bsize) + 1
	'this is a horrible hack to get 2 bytes per integer, even though
	'they are 4 bytes long in FB
	bi = 0
	toggle = 0
	if bpage >= 0 then
		'read to screen
		sptr = vpages(wrkpage)->image
		sptr = sptr + (vpages(wrkpage)->pitch * l)
		idx = bsize
		while idx > vpages(wrkpage)->w
			fget(f, , sptr, vpages(wrkpage)->w)
			idx -= vpages(wrkpage)->w
			sptr += vpages(wrkpage)->pitch
		wend
		fget(f, , sptr, idx)
	else
		'debug "buffer size to read = " + str(bsize)
		for idx = 0 to bsize - 1 ' this will be slow
			get #f, , ub
			if toggle = 0 then
				bptr[bi] = ub
				toggle = 1
			else
				bptr[bi] = bptr[bi] or (ub shl 8)
				'check sign
				if (bptr[bi] and &h8000) > 0 then
					bptr[bi] = bptr[bi] or &hffff0000 'make -ve
				end if
				toggle = 0
				bi = bi + 1
			end if
		next
	end if

	close #f
	debug_if_slow(starttime, 0.1, fil)
end sub

'b is in BYTES
sub setpicstuf (buf() as integer, byval b as integer, byval p as integer)
	if p >= 0 then
		if clippedframe <> vpages(p) then
			setclip , , , , p
		end if
	end if

	bptr = @buf(0) 'doesn't really work well with FB
	bsize = b
	bpage = p
end sub

sub storemxs (fil as string, byval record as integer, byval fr as Frame ptr)
'saves a screen page to a file. Doesn't support non-320x200 pages
	dim f as integer
	dim as integer x, y
	dim sptr as ubyte ptr
	dim plane as integer

	if NOT fileiswriteable(fil) then exit sub
	f = freefile
	open fil for binary access read write as #f

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

function loadmxs (fil as string, byval record as integer, byval dest as Frame ptr = NULL) as Frame ptr
'Loads a 320x200 mode X format page from a file.
'You may optionally pass in existing frame to load into (unnecessary functionality)
	dim starttime as double = timer
	dim f as integer
	dim as integer x, y
	dim sptr as ubyte ptr
	dim plane as integer

	if dest then
		dim temp as Frame ptr
		temp = loadmxs(fil, record)
		frame_clear dest
		if temp then
			frame_draw temp, , 0, 0, , NO, dest
			frame_unload @temp
		end if
		return dest
	end if
	dest = frame_new(320, 200)

	if NOT fileisreadable(fil) then return 0
	if record < 0 then
		debug "loadmxs: attempted to read a negative record number " & record
		return dest
	end if
	f = freefile
	open fil for binary access read as #f

	if lof(f) < (record + 1) * 64000 then
		debug "loadpage: wanted page " & record & "; " & fil & " is only " & lof(f) & " bytes"
		close #f
		return dest
	end if

	'skip to index
	seek #f, (record*64000) + 1

        dim quarter_row(79) as ubyte

	'modex format, 4 planes
	for plane = 0 to 3
		for y = 0 to 200 - 1
			sptr = dest->image + dest->pitch * y + plane

			'1/4 of a row
                        get #f, , quarter_row()
			for x = 0 to 80 - 1
                                sptr[x * 4] = quarter_row(x)
			next
		next
	next

	close #f
	debug_if_slow(starttime, 0.1, fil)
	return dest
end function


'==========================================================================================
'                                   Graphics primitives
'==========================================================================================


'No clipping!!
sub putpixel (byval spr as Frame ptr, byval x as integer, byval y as integer, byval c as integer)
	if x < 0 orelse x >= spr->w orelse y < 0 orelse y >= spr->h then
		exit sub
	end if

	FRAMEPIXEL(x, y, spr) = c
end sub

sub putpixel (byval x as integer, byval y as integer, byval c as integer, byval p as integer)
	if clippedframe <> vpages(p) then
		setclip , , , , p
	end if

	if POINT_CLIPPED(x, y) then
		'debug "attempt to putpixel off-screen " & x & "," & y & "=" & c & " on page " & p
		exit sub
	end if

	PAGEPIXEL(x, y, p) = c
end sub

function readpixel (byval spr as Frame ptr, byval x as integer, byval y as integer) as integer
	if x < 0 orelse x >= spr->w orelse y < 0 orelse y >= spr->h then
		exit function
	end if

	return FRAMEPIXEL(x, y, spr)
end function

function readpixel (byval x as integer, byval y as integer, byval p as integer) as integer
	if clippedframe <> vpages(p) then
		setclip , , , , p
	end if

	if POINT_CLIPPED(x, y) then
		debug "attempt to readpixel off-screen " & x & "," & y & " on page " & p
		return 0
	end if

	return PAGEPIXEL(x, y, p)
end function

sub drawbox (byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval col as integer, byval thickness as integer = 1, byval p as integer)
	drawbox vpages(p), x, y, w, h, col, thickness
end sub

'Draw a hollow box, with given edge thickness
sub drawbox (byval dest as Frame ptr, byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval col as integer, byval thickness as integer = 1)
	if w < 0 then x = x + w + 1: w = -w
	if h < 0 then y = y + h + 1: h = -h

	if w = 0 or h = 0 then exit sub

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

sub rectangle (byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval c as integer, byval p as integer)
	rectangle vpages(p), x, y, w, h, c
end sub

sub rectangle (byval fr as Frame Ptr, byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval c as integer)
	if clippedframe <> fr then
		setclip , , , , fr
	end if

	if fr = 0 then debug "rectangle null ptr": exit sub

	if w < 0 then x = x + w + 1: w = -w
	if h < 0 then y = y + h + 1: h = -h

	'clip
	if x + w > clipr then w = (clipr - x) + 1
	if y + h > clipb then h = (clipb - y) + 1
	if x < clipl then w -= (clipl - x) : x = clipl
	if y < clipt then h -= (clipt - y) : y = clipt

	if w <= 0 or h <= 0 then exit sub

	dim sptr as ubyte ptr = fr->image + (y * fr->pitch) + x
	while h > 0
		memset(sptr, c, w)
		sptr += fr->pitch
		h -= 1
	wend
end sub

sub fuzzyrect (byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval c as integer, byval p as integer, byval fuzzfactor as integer = 50)
	fuzzyrect vpages(p), x, y, w, h, c, fuzzfactor
end sub

sub fuzzyrect (byval fr as Frame Ptr, byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval c as integer, byval fuzzfactor as integer = 50)
	'How many magic constants could you wish for?
	'These were half generated via magic formulas, and half hand picked (with magic criteria)
	static grain_table(50) as integer = {_
	                    50, 46, 42, 38, 38, 40, 41, 39, 26, 38, 30, 36, _
	                    42, 31, 39, 38, 41, 26, 27, 28, 40, 35, 35, 31, _
	                    39, 50, 41, 30, 29, 28, 45, 37, 24, 43, 23, 42, _
	                    21, 28, 11, 16, 20, 22, 18, 17, 19, 32, 17, 16, _
	                    15, 14, 50}

	if clippedframe <> fr then
		setclip , , , , fr
	end if

	if fuzzfactor <= 0 then fuzzfactor = 1
	dim grain as integer
	dim r as integer = 0
	dim startr as integer = 0

	if fuzzfactor <= 50 then grain = grain_table(fuzzfactor) else grain = grain_table(100 - fuzzfactor)
	'if w = 99 then grain = h mod 100  'for hand picking

	if w < 0 then x = x + w + 1: w = -w
	if h < 0 then y = y + h + 1: h = -h

	'clip
	if x + w > clipr then w = (clipr - x) + 1
	if y + h > clipb then h = (clipb - y) + 1
	if x < clipl then
		startr += (clipl - x) * fuzzfactor
		w -= (clipl - x)
		x = clipl
	end if
	if y < clipt then
		startr += (clipt - y) * grain
		h -= (clipt - y)
		y = clipt
	end if

	if w <= 0 or h <= 0 then exit sub

	dim sptr as ubyte ptr = fr->image + (y * fr->pitch) + x
	while h > 0
		startr = (startr + grain) mod 100
		r = startr
		for i as integer = 0 to w-1
			r += fuzzfactor
			if r >= 100 then
				sptr[i] = c
				r -= 100
			end if
		next
		h -= 1
		sptr += fr->pitch
	wend
end sub

'Draw either a rectangle or a scrolling chequer pattern.
'bgcolor is either between 0 and 255 (a colour), -1 (a scrolling chequered
'background), or -2 (a non-scrolling chequered background)
'chequer_scroll is a counter variable which the calling function should increment once per tick.
sub draw_background (x as integer, y as integer, wide as integer, high as integer, bgcolor as integer, byref chequer_scroll as integer, dest as Frame ptr)
	const zoom = 3  'Chequer pattern zoom, fixed
	const rate = 4  'ticks per pixel scrolled, fixed
	'static chequer_scroll as integer
	chequer_scroll = POSMOD(chequer_scroll, (zoom * rate * 2))

	if bgcolor >= 0 then
		rectangle dest, x, y, wide, high, bgcolor
	else
		dim bg_chequer as Frame Ptr
		bg_chequer = frame_new(wide / zoom + 2, high / zoom + 2)
		frame_clear bg_chequer, uilook(uiBackground)
		fuzzyrect bg_chequer, 0, 0, bg_chequer->w, bg_chequer->h, uilook(uiDisabledItem)
		dim offset as integer = 0
		if bgcolor = -1 then offset = chequer_scroll \ rate
		dim oldclip as ClipState
		saveclip oldclip
		shrinkclip x, y, x + wide - 1, y + high - 1, dest
		frame_draw bg_chequer, NULL, x - offset, y - offset, zoom, NO, dest
		loadclip oldclip
		frame_unload @bg_chequer
	end if
end sub

sub drawline (byval x1 as integer, byval y1 as integer, byval x2 as integer, byval y2 as integer, byval c as integer, byval p as integer)
	drawline vpages(p), x1, y1, x2, y2, c
end sub

sub drawline (byval dest as Frame ptr, byval x1 as integer, byval y1 as integer, byval x2 as integer, byval y2 as integer, byval c as integer)
'uses Bresenham's run-length slice algorithm
	dim as integer xdiff,ydiff
	dim as integer xdirection       'direction of X travel from top to bottom point (1 or -1)
	dim as integer minlength        'minimum length of a line strip
	dim as integer startLength      'length of start strip (approx half 'minLength' to balance line)
	dim as integer runLength        'current run-length to be used (minLength or minLength+1)
	dim as integer endLength        'length of end of line strip (usually same as startLength)

	dim as integer instep           'xdirection or 320 (inner loop)
	dim as integer outstep          'xdirection or 320 (outer loop)
	dim as integer shortaxis        'outer loop control
	dim as integer longaxis

	dim as integer errorterm        'when to draw an extra pixel
	dim as integer erroradd         'add to errorTerm for each strip drawn
	dim as integer errorsub         'subtract from errorterm when triggered

	dim sptr as ubyte ptr

'Macro to simplify code
#macro DRAW_SLICE(a)
	for i as integer = 0 to a-1
		*sptr = c
		sptr += instep
	next
#endmacro

	if clippedframe <> dest then
		setclip , , , , dest
	end if

	if POINT_CLIPPED(x1, y1) orelse POINT_CLIPPED(x2, y2) then
		debug "drawline: outside clipping"
		exit sub
	end if

	if y1 > y2 then
		'swap ends, we only draw downwards
		swap y1, y2
		swap x1, x2
	end if

	'point to start
	sptr = dest->image + (y1 * dest->pitch) + x1

	xdiff = x2 - x1
	ydiff = y2 - y1

	if xDiff < 0 then
		'right to left
		xdiff = -xdiff
		xdirection = -1
	else
		xdirection = 1
	end if

	'special case for vertical
	if xdiff = 0 then
		instep = dest->pitch
		DRAW_SLICE(ydiff+1)
		exit sub
	end if

	'and for horizontal
	if ydiff = 0 then
		instep = xdirection
		DRAW_SLICE(xdiff+1)
		exit sub
	end if

	'and also for pure diagonals
	if xdiff = ydiff then
		instep = dest->pitch + xdirection
		DRAW_SLICE(ydiff+1)
		exit sub
	end if

	'now the actual bresenham
	if xdiff > ydiff then
		longaxis = xdiff
		shortaxis = ydiff

		instep = xdirection
		outstep = dest->pitch
	else
		'other way round, draw vertical slices
		longaxis = ydiff
		shortaxis = xdiff

		instep = dest->pitch
		outstep = xdirection
	end if

	'calculate stuff
	minlength = longaxis \ shortaxis
	erroradd = (longaxis mod shortaxis) * 2
	errorsub = shortaxis * 2

	'errorTerm must be initialized properly since first pixel
	'is about in the center of a strip ... not the start
	errorterm = (erroradd \ 2) - errorsub

	startLength = (minLength \ 2) + 1
	endLength = startlength 'half +1 of normal strip length

	'If the minimum strip length is even
	if (minLength and 1) <> 0 then
		errorterm += shortaxis 'adjust errorTerm
	else
		'If the line had no remainder (x&yDiff divided evenly)
		if erroradd = 0 then
			startLength -= 1 'leave out extra start pixel
		end if
	end if

	'draw the start strip
	DRAW_SLICE(startlength)
	sptr += outstep

	'draw the middle strips
	for j as integer = 1 to shortaxis - 1
		runLength = minLength
		errorTerm += erroradd

		if errorTerm > 0 then
			errorTerm -= errorsub
			runLength += 1
		end if

		DRAW_SLICE(runlength)
		sptr += outstep
	next

	DRAW_SLICE(endlength)
end sub

sub paintat (byval dest as Frame ptr, byval x as integer, byval y as integer, byval c as integer)
'a floodfill.
	dim tcol as integer
	dim queue as XYPair_node ptr = null
	dim tail as XYPair_node ptr = null
	dim as integer w, e		'x coords west and east
	dim i as integer
	dim tnode as XYPair_node ptr = null

	if clippedframe <> dest then
		setclip , , , , dest
	end if

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
			while w > clipl and FRAMEPIXEL(w-1, queue->y, dest) = tcol
				w -= 1
				FRAMEPIXEL(w, queue->y, dest) = c
			wend
			'find eastern limit
			while e < clipr and FRAMEPIXEL(e+1, queue->y, dest) = tcol
				e += 1
				FRAMEPIXEL(e, queue->y, dest) = c
			wend
			'add bordering XYPair_nodes
			for i = w to e
				if queue->y > clipt then
					'north
					if FRAMEPIXEL(i, queue->y-1, dest) = tcol then
						tail->nextnode = callocate(sizeof(XYPair_node))
						tail = tail->nextnode
						tail->x = i
						tail->y = queue->y-1
						tail->nextnode = null
					end if
				end if
				if queue->y < clipb then
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

sub ellipse (byval fr as Frame ptr, byval x as double, byval y as double, byval radius as double, byval col as integer, byval fillcol as integer, byval semiminor as double = 0.0, byval angle as double = 0.0)
'radius is the semimajor axis if the ellipse is not a circle
'angle is the angle of the semimajor axis to the x axis, in radians counter-clockwise

	if clippedframe <> fr then
		setclip , , , , fr
	end if

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
		if ys < clipt - 1 or ys > clipb + 1 then continue for

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

'Replaces one colour with another within a rectangular region.
'Specifying the region is optional (all four args x,y,w,h must be given if any of them are)
'w and h may be negative to 'grow' a rectangle from the opposite edge
sub replacecolor (byval fr as Frame ptr, byval c_old as integer, byval c_new as integer, byval x as integer = -1, byval y as integer = -1, byval w as integer = -1, byval h as integer = -1)
	if clippedframe <> fr then
		setclip , , , , fr
	end if

	if (x and y and w and h) = -1 then
		'Default to whole clipped region
		x = clipl
		y = clipt
		w = clipr - clipl + 1
		h = clipb - clipt + 1
	else
		if w < 0 then x = x + w + 1: w = -w
		if h < 0 then y = y + h + 1: h = -h

		'clip
		if x + w > clipr then w = (clipr - x) + 1
		if y + h > clipb then h = (clipb - y) + 1
		if x < clipl then w -= (clipl - x) : x = clipl
		if y < clipt then h -= (clipt - y) : y = clipt
	end if

	if w <= 0 or h <= 0 then exit sub

	dim as integer xi, yi
	for yi = y to y + h - 1
		dim sptr as ubyte ptr = fr->image + (yi * fr->pitch)
		for xi = x to x + w - 1
			if sptr[xi] = c_old then sptr[xi] = c_new
		next
	next
end sub


'==========================================================================================
'                                      Text routines
'==========================================================================================


'Pass a string, a 0-based offset of the start of the tag (it is assumed the first two characters have already
'been matched as ${ or \8{ as desired), and action and arg pointers, to fill with the parse results. (Action in UPPERCASE)
'Returns 0 for an invalidly formed tag, otherwise the (0-based) offset of the closing }.
function parse_tag(z as string, byval offset as integer, byval action as string ptr, byval arg as int32 ptr) as integer
	dim closebrace as integer = INSTR((offset + 4) + 1, z, "}") - 1
	if closebrace <> -1 then
		*action = ""
		dim j as integer
		for j = 2 to 5
			if isalpha(z[offset + j]) then
				*action += CHR(toupper(z[offset + j]))
			else
				exit for
			end if
		next

		'dim strarg as string = MID(z, offset + j + 1, closebrace - (offset + j))
		'*arg = str2int(strarg)

		'The C standard lib seems a tad more practical than BASIC's (watch out though, scanf will stab you in the back if it sees a chance)
		dim brace as byte
		if isspace(z[offset + j]) orelse sscanf(@z[offset + j], "%d%c", arg, @brace) <> 2 orelse brace <> asc("}") then
			*action = ""
			return 0
		end if
		return closebrace
	end if
	return 0
end function

'FIXME: refactor, making use of OO which we can now use
type PrintStrState
	'Public members (may set before passing to render_text)
	as Font ptr thefont
	as long fgcolor          'Used when resetting localpal. May be -1 for none
	as long bgcolor          'Only used if not_transparent
	as bool32 not_transparent  'Force non-transparency of layer 1

	'Internal members
	as Font ptr initial_font    'Used when resetting thefont
	as long leftmargin
	as long rightmargin
	as long x
	as long y
	as long startx
	as long charnum

	'Internal members used only if drawing, as opposed to laying out/measuring
	as Palette16 localpal
	as long initial_fgcolor  'Used when resetting fgcolor
	as long initial_bgcolor  'Used when resetting bgcolor
	as bool32 initial_not_trans 'Used when resetting bgcolor
end type

'Special signalling characters
#define tcmdFirst      15
#define tcmdState      15
#define tcmdPalette    16
#define tcmdRepalette  17
#define tcmdFont       18  '1 argument: the font number (possibly -1)
#define tcmdLast       18

'Invisible argument: state. (member should not be . prefixed, unfortunately)
'Modifies state, and appends a control sequence to the string outbuf to duplicate the change
'Assumes 32 bit.
'Note: in order to support members that are less than 4 bytes (eg palette colours) some hackery is done, and
'members greater than 4 bytes aren't supported
#macro UPDATE_STATE(outbuf, member, value)
	'Ugh! FB doesn't allow sizeof in #if conditions!
	#if typeof(state.member) <> long
		#error "UPDATE_STATE: bad member type"
	#endif
	outbuf += CHR(tcmdState) & "      "
	*Cast(short ptr, @outbuf[len(outbuf) - 6]) = Offsetof(PrintStrState, member)
	*Cast(long ptr, @outbuf[len(outbuf) - 4]) = Cast(long, value)
	state.member = value
#endmacro

'Interprets a control sequence (at 0-based offset ch in outbuf) written by UPDATE_STATE,
'modifying state.
#define MODIFY_STATE(state, outbuf, ch) _
	/' dim offset as long = *Cast(short ptr, @outbuf[ch + 1]) '/ _
	/' dim newval as long = *Cast(long ptr, @outbuf[ch + 3]) '/ _
	*Cast(long ptr, Cast(byte ptr, @state) + *Cast(short ptr, @outbuf[ch + 1])) = _
		*Cast(long ptr, @outbuf[ch + 3]) : _
	ch += 6

#define APPEND_CMD0(outbuf, cmd_id) _
	outbuf += CHR(cmd_id)

#define APPEND_CMD1(outbuf, cmd_id, value) _
	outbuf += CHR(cmd_id) & "    " : _
	*Cast(long ptr, @outbuf[len(outbuf) - 4]) = Cast(long, value)

#define READ_CMD(outbuf, ch, variable) _
	variable = *Cast(long ptr, @outbuf[ch + 1]) : _
	ch += 4

'Processes starting from z[state.charnum] until the end of the line, returning a string
'which describes a line fragment. It contains printing characters plus command sequences
'for modifying state. state is passed byval (upon wrapping we would have to undo changes
'to the state, which is too hard).
'endchar is 0 based, and exclusive - normally len(z). FIXME: endchar appears broken
'We also compute the height (height of the tallest font on the line) and the right edge
'(max_x) of the line fragment. You have to know the line height before you can know the y
'coordinate of each character on the line.
'Updates to .x, .y are not written because they can be recreated from the character
'stream, and .charnum is not written (unless updatecharnum is true) because it's too
'expensive. However, .x, .y and .charnum are updated at the end.
'If updatecharnum is true, it is updated only when .charnum jumps; you still need to
'increment after every printing character yourself.
private function layout_line_fragment(z as string, byval endchar as integer, byval state as PrintStrState, byref line_width as integer, byref line_height as integer, byval pagewidth as integer, byval withtags as bool, byval withnewlines as bool, byval updatecharnum as bool = NO) as string
	dim lastspace as integer = -1
	dim lastspace_x as integer
	dim lastspace_outbuf_len as integer
	dim lastspace_line_height as integer
	dim endchar_x as integer             'x at endchar
	dim endchar_outbuf_len as integer = 999999  'Length of outbuf at endchar
	dim ch as integer                    'We use this instead of modifying .charnum
	dim visible_chars as integer         'Number non-control chars we will return
	dim outbuf as string
	'Appending characters one at a time to outbuf is slow, so we delay it.
	'chars_to_add counts the number of delayed characters
	dim chars_to_add as integer = 0

	with state
'debug "layout '" & z & "' from " & .charnum & " at " & .x & "," & .y
		line_height = .thefont->h
		for ch = .charnum to len(z) - 1
			if ch = endchar then  'FIXME: This never happens
'debug "hit endchar"
				endchar_x = .x
				endchar_outbuf_len = len(outbuf) + chars_to_add
			end if

			if z[ch] = 10 and withnewlines then  'newline
'debug "add " & chars_to_add & " chars before " & ch & " : '" & Mid(z, 1 + ch - chars_to_add, chars_to_add) & "'"
				outbuf += Mid(z, 1 + ch - chars_to_add, chars_to_add)
				chars_to_add = 0
				'Skip past the newline character, but don't add to outbuf
				ch += 1
				if ch >= endchar then
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
				'UPDATE_STATE(outbuf, rightmargin, pagewidth)
				return outbuf
			elseif z[ch] = 8 then ' ^H, hide tag
				if z[ch + 1] = asc("{") then
					dim closebrace as integer = instr((ch + 2) + 1, z, "}") - 1
					if closebrace <> -1 then
						'Add delayed characters first
'debug "add " & chars_to_add & " chars before " & ch & " : '" & Mid(z, 1 + ch - chars_to_add, chars_to_add) & "'"

						outbuf += Mid(z, 1 + ch - chars_to_add, chars_to_add)
						chars_to_add = 0
						ch = closebrace
						if updatecharnum then
							UPDATE_STATE(outbuf, charnum, ch)
						end if
						continue for
					end if
				end if
			elseif z[ch] >= tcmdFirst and z[ch] <= tcmdLast then ' special signalling characters. Not allowed! (FIXME: delete this)
'debug "add " & chars_to_add & " chars before " & ch & " : '" & Mid(z, 1 + ch - chars_to_add, chars_to_add) & "'"

				outbuf += Mid(z, 1 + ch - chars_to_add, chars_to_add)
				chars_to_add = 0
				ch += 1	 'skip
				if updatecharnum then
					UPDATE_STATE(outbuf, charnum, ch)
				end if
				continue for
			elseif z[ch] = asc("$") then
				if withtags and z[ch + 1] = asc("{") then
					dim action as string
					dim intarg as int32

					dim closebrace as integer = parse_tag(z, ch, @action, @intarg)
					if closebrace then
						'Add delayed characters first
'debug "add " & chars_to_add & " chars before " & ch & " : '" & Mid(z, 1 + ch - chars_to_add, chars_to_add) & "'"

						outbuf += Mid(z, 1 + ch - chars_to_add, chars_to_add)
						chars_to_add = 0
						if action = "F" then
							'Font
							'Let's preserve the position offset when changing fonts. That way, plain text in
							'the middle of edgetext is also offset +1,+1, so that it lines up visually with it
							'.x += fonts(intarg).offset.x - .thefont->offset.x
							'.y += fonts(intarg).offset.y - .thefont->offset.y
							if intarg >= -1 andalso intarg <= ubound(fonts) then
								if intarg = -1 then
									'UPDATE_STATE(outbuf, thefont, .initial_font)
									.thefont = .initial_font
								elseif fonts(intarg).initialised then
									'UPDATE_STATE(outbuf, thefont, @fonts(intarg))
									.thefont = @fonts(intarg)
								else
									goto badtexttag
								end if
								APPEND_CMD1(outbuf, tcmdFont, intarg) '.thefont)
								line_height = large(line_height, .thefont->h)
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
							UPDATE_STATE(outbuf, rightmargin, pagewidth - intarg)
						else
							goto badtexttag
						end if
						ch = closebrace
						if updatecharnum then
							UPDATE_STATE(outbuf, charnum, ch)
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
'debug "rm = " & .rightmargin & " lm = " & .leftmargin
				if lastspace > -1 and .x - lastspace_x < (.rightmargin - .leftmargin) \ 2 then
					'Split at the last space

					if chars_to_add then
'debug "add " & chars_to_add & " chars before " & ch & " : '" & Mid(z, 1 + ch - chars_to_add, chars_to_add) & "'"

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
'debug "add " & chars_to_add & " chars before " & ch & " : '" & Mid(z, 1 + ch - chars_to_add, chars_to_add) & "'"
			outbuf += Mid(z, 1 + ch - chars_to_add, chars_to_add)
		end if
		'Why do we always set x and charnum at the end of the string?
		if ch <= endchar then
'debug "exiting layout_line_fragment, ch = " & ch & ", .x = " & .x
			line_width = .x
			UPDATE_STATE(outbuf, x, .startx + .leftmargin)
		else
'debug "exiting layout_line_fragment, ch = " & ch & ", endchar_x = " & endchar_x
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
sub build_text_palette(byref state as PrintStrState, byval srcpal as Palette16 ptr)
	with state
		if srcpal then
			memcpy(@.localpal, srcpal, sizeof(Palette16))
		end if
		.localpal.col(0) = .bgcolor
		if .fgcolor > -1 then
			.localpal.col(1) = .fgcolor
		end if
		if srcpal = NULL and .fgcolor = -1 then
			debug "render_text: Drawing a font without a palette or foreground colour!"
		end if
'debug "build_text_palette: bg = " & .bgcolor & " fg = "& .fgcolor & " outline = " & .thefont->outline_col
		'Outline colours are a hack, hopefully temp.
		if .thefont->outline_col > -1 then
			.localpal.col(.thefont->outline_col) = uilook(uiOutline)
		end if
	end with
end sub

'Processes a parsed line, updating the state passed to it, and also optionally draws one of the layers (if reallydraw)
sub draw_line_fragment(byval dest as Frame ptr, byref state as PrintStrState, byval layer as integer, parsed_line as string, byval reallydraw as bool)
	dim arg as integer
	dim as Frame charframe
	charframe.mask = NULL

	with state
'debug "draw frag: x=" & .x & " y=" & .y & " char=" & .charnum & " reallydraw=" & reallydraw & " layer=" & layer
		for ch as integer = 0 to len(parsed_line) - 1
			if parsed_line[ch] = tcmdState then
				'Control sequence. Make a change to state, and move ch past the sequence
				MODIFY_STATE(state, parsed_line, ch)

			elseif parsed_line[ch] = tcmdFont then
				READ_CMD(parsed_line, ch, arg)
				if arg >= -1 andalso arg <= ubound(fonts) then
					if arg = -1 then
						'UPDATE_STATE(outbuf, thefont, .initial_font)
						.thefont = .initial_font
					elseif fonts(arg).initialised then
						'UPDATE_STATE(outbuf, thefont, @fonts(arg))
						.thefont = @fonts(arg)
					else
						'This should be impossible, because layout_line_fragment has already checked this
						debugc errPromptBug, "draw_line_fragment: font not initialised!"
					end if
				else
					'This should be impossible, because layout_line_fragment has already checked this
					debugc errPromptBug, "draw_line_fragment: invalid font!"
				end if
				if reallydraw then
					'In case .fgcolor == -1 and .thefont->pal == NULL. Palette changes are per-font,
					'so reset the colour.
					if .fgcolor = -1 then .fgcolor = .initial_fgcolor
					'We rebuild the local palette using either the font's palette or from scratch
					build_text_palette state, .thefont->pal
				end if

			elseif parsed_line[ch] = tcmdPalette then
				READ_CMD(parsed_line, ch, arg)
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
				if reallydraw and .x <= clipr then
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
							drawohr(@charframe, dest, @state.localpal, state.x + .offx, state.y + .offy - state.thefont->h, trans)
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
' ${F#}  changes to font # or return to initial font if # == -1
' ${K#}  changes foreground/first colour, or return to initial colour if # == -1
'        (Note that this does disable the foreground colour, unless the initial fg colour was -1!)
' ${KB#} changes the background colour, and turns on not_transparent.
'        Specify -1 to restore previous background colour and transparency
' ${KP#} changes to palette # (-1 is invalid) (Maybe should make ${F-1} return to the default)
'        (Note, palette changes are per-font, and expire when the font changes)
' ${LM#} sets left margin for the current line, in pixels
' ${RM#} sets right margin for the current line, in pixels
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
sub render_text (byval dest as Frame ptr, byref state as PrintStrState, text as string, byval endchar as integer = 999999, byval xpos as integer, byval ypos as integer, byval wide as integer = 999999, byval pal as Palette16 ptr = NULL, byval withtags as bool = YES, byval withnewlines as bool = YES)
', byval cached_state as PrintStrStatePtr = NULL, byval use_cached_state as bool = YES)

'static tog as integer = 0
'tog xor= 1
'dim t as double = timer

	if dest = null then debug "printstr: NULL dest" : exit sub

	if clippedframe <> dest then
		setclip , , , , dest
	end if

	'check bounds skipped because this is now quite hard to tell (checked in drawohr)

'debug "printstr '" & text & "' (len=" & len(text) & ") wide = " & wide & " tags=" & withtags & " nl=" & withnewlines

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
			.x = xpos + .thefont->offset.x
			.y = ypos + .thefont->offset.y
			.startx = .x
			'Margins are measured relative to xpos
			.leftmargin = 0
			.rightmargin = wide
		'end if

		dim as bool visibleline  'Draw this line of text?

		'We have to process both layers, even if the current font has only one layer,
		'in case the string switches to a font that has two!
		dim prev_state as PrintStrState = state
		dim prev_parse as string
		dim prev_visible as bool
		dim draw_layer1 as bool = NO  'Don't draw on first loop

		if endchar > len(text) then endchar = len(text)
		do
			dim line_height as integer
			dim parsed_line as string = layout_line_fragment(text, endchar, state, 0, line_height, wide, withtags, withnewlines)
'debug "parsed: " + parsed_line
			'Print at least one extra line above and below the visible region, in case the
			'characters are big (we only approximate this policy, with the current font height)
			visibleline = (.y + line_height > clipt - .thefont->h AND .y < clipb + .thefont->h)
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
'debug "prev.charnum=" & prev_state.charnum
				if prev_state.charnum >= endchar then /'debug "text end" :'/ exit do
				if prev_state.y > clipb + prev_state.thefont->h then exit do
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
sub text_layout_dimensions (byval retsize as StringSize ptr, z as string, byval endchar as integer = 999999, byval maxlines as integer = 999999, byval wide as integer = 999999, byval fontnum as integer, byval withtags as bool = YES, byval withnewlines as bool = YES)
'debug "DIMEN char " & endchar
	dim state as PrintStrState
	with state
		'.localpal/?gcolor/initial_?gcolor/transparency non-initialised
		.thefont = @fonts(fontnum)
		.initial_font = .thefont
		.charnum = 0
		.x = .thefont->offset.x
		.y = .thefont->offset.y
		'Margins are measured relative to xpos
		.leftmargin = 0
		.rightmargin = wide

		dim maxwidth as integer = 0
		dim line_width as integer = 0
		dim line_height as integer = 0
		retsize->lines = 0

		if endchar > len(z) then endchar = len(z)
		while .charnum < len(z)
			if .charnum > endchar then exit while
			'If .charnum = endchar, the last line is zero length, but should be included.
			'.charnum won't advance, so need extra check to prevent infinite loop!
			dim exitloop as bool = (.charnum = endchar)
			dim parsed_line as string = layout_line_fragment(z, endchar, state, line_width, line_height, wide, withtags, withnewlines)
			retsize->lines += 1
'debug "parsed a line, line_width =" & line_width
			maxwidth = large(maxwidth, line_width)

			'Update state
			.y += line_height
			draw_line_fragment(NULL, state, 0, parsed_line, NO)
'debug "now " & .charnum & " at " & .x & "," & .y
			if exitloop then exit while
		wend

		retsize->endchar = .charnum
		retsize->w = maxwidth
		retsize->h = .y
		retsize->lastw = line_width
		retsize->lasth = line_height
		retsize->finalfont = .thefont
'debug "end DIM  char=" & .charnum
	end with
end sub

'Returns the length in pixels of the longest line of a *non-autowrapped* string.
function textwidth(z as string, byval fontnum as integer = 0, byval withtags as bool = YES, byval withnewlines as bool = YES) as integer
	dim retsize as StringSize
	text_layout_dimensions @retsize, z, len(z), , , fontnum, withtags, withnewlines
'debug "width of '" & z & "' is "
	return retsize.w
end function

'xpos and ypos passed to use same cached state
sub find_point_in_text (byval retsize as StringCharPos ptr, byval seekx as integer, byval seeky as integer, z as string, byval wide as integer = 999999, byval xpos as integer = 0, byval ypos as integer = 0, byval fontnum as integer, byval withtags as bool = YES, byval withnewlines as bool = YES)

	dim state as PrintStrState
	with state
		'.localpal/?gcolor/initial_?gcolor/transparency non-initialised
		.thefont = @fonts(fontnum)
		.initial_font = .thefont
		.charnum = 0
		.x = xpos + .thefont->offset.x
		.y = ypos + .thefont->offset.y
		'Margins are measured relative to xpos
		.leftmargin = 0
		.rightmargin = wide

		dim delayedmatch as bool = NO
		dim line_width as integer
		dim line_height as integer
		dim arg as integer

		retsize->exacthit = NO
		'retsize->w = .thefont->h  'Default for if we go off the end of the text

		while .charnum < len(z)
			dim parsed_line as string = layout_line_fragment(z, len(z), state, line_width, line_height, wide, withtags, withnewlines, YES)
			.y += line_height
			'.y now points to 1 pixel past the bottom of the line fragment

			'Update state
			for ch as integer = 0 to len(parsed_line) - 1
				if parsed_line[ch] = tcmdState then
					'Make a change to the state
					.charnum += 1   'FIXME: this looks wrong
					MODIFY_STATE(state, parsed_line, ch)
				elseif parsed_line[ch] = tcmdFont then
					READ_CMD(parsed_line, ch, arg)
					.thefont = cast(Font ptr, arg)
					'.thefont = @fonts(arg)
				elseif parsed_line[ch] = tcmdPalette then
					READ_CMD(parsed_line, ch, arg)
				else

					dim w as integer = .thefont->w(parsed_line[ch])
					'Draw a character
					if delayedmatch then
						'retsize->w = w
						exit while
					end if
					.x += w
					if .y > seeky and .x > seekx then
'debug "FIND IN: hit w/ x = " & .x
						'retsize->w = w
						retsize->exacthit = YES
						.x -= w
						exit while
					end if
					.charnum += 1
				end if
			next

			if .y > seeky then
				'Position was off the end of the line
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
'debug "FIND IN: delayed"
			end if
		wend

		retsize->charnum = .charnum
		retsize->x = .x
		retsize->y = .y - .thefont->h
		retsize->h = .thefont->h
		retsize->lineh = line_height
	end with
end sub

'A flexible printstr for enduser code without weird font, pal arguments
sub printstr (byval dest as Frame ptr, s as string, byval x as integer, byval y as integer, byval wide as integer = 999999, byval fontnum as integer, byval withtags as bool = YES, byval withnewlines as bool = YES)
	dim state as PrintStrState
	state.thefont = @fonts(fontnum)
	if textbg <> 0 then state.not_transparent = YES
	state.bgcolor = textbg
	state.fgcolor = textfg

	render_text (dest, state, s, , x, y, wide, , withtags, withnewlines)
end sub

'the old printstr -- no autowrapping
sub printstr (s as string, byval x as integer, byval y as integer, byval p as integer, byval withtags as bool = NO)
	dim state as PrintStrState
	state.thefont = @fonts(0)
	if textbg <> 0 then state.not_transparent = YES
	state.bgcolor = textbg
	state.fgcolor = textfg

	render_text (vpages(p), state, s, , x, y, , , withtags, NO)
end sub

'this doesn't autowrap either
sub edgeprint (s as string, byval x as integer, byval y as integer, byval c as integer, byval p as integer, byval withtags as bool = NO, byval withnewlines as bool = NO)
	'preserve the old behaviour (edgeprint used to call textcolor)
	textfg = c
	textbg = 0

	dim state as PrintStrState
	state.thefont = @fonts(1)
	state.fgcolor = c

	render_text (vpages(p), state, s, , x, y, , , withtags, withnewlines)
end sub

sub textcolor (byval fg as integer, byval bg as integer)
	textfg = fg
	textbg = bg
end sub

function fgcol_text(text as string, byval colour as integer) as string
	return "${K" & colour & "}" & text & "${K-1}"
end function

function bgcol_text(text as string, byval colour as integer) as string
	return "${KB" & colour & "}" & text & "${KB-1}"
end function


'==========================================================================================
'                                           Fonts
'==========================================================================================


'This sub doesn't actually delete the Font object
sub font_unload (byval font as Font ptr)
	if font = null then exit sub

	for i as integer = 0 to 1
		if font->layers(i) then
			font->layers(i)->refcount -= 1
			if font->layers(i)->refcount <= 0 then
				frame_unload @font->layers(i)->spr
				deallocate(font->layers(i))
			end if
			font->layers(i) = NULL
		end if
	next

	Palette16_unload @font->pal
	memset(font, 0, sizeof(Font))
	font->pal_id = -1
	font->outline_col = -1
	'font->cols = 0
	'font->offset.x = 0
	'font->offset.y = 0
end sub

'Doesn't create a Frame
private function fontlayer_new () as FontLayer ptr
	dim ret as FontLayer ptr
	ret = callocate(sizeof(FontLayer))
	ret->refcount = 1
	return ret
end function

private function fontlayer_duplicate (byval srclayer as FontLayer ptr) as FontLayer ptr
	dim ret as FontLayer ptr
	ret = callocate(sizeof(FontLayer))
	memcpy(ret, srclayer, sizeof(FontLayer))
	ret->spr = frame_duplicate(srclayer->spr)
	ret->refcount = 1
	return ret
end function

'Create a version of a font with an outline around each character (in a new palette colour)
sub font_create_edged (byval newfont as Font ptr, byval basefont as Font ptr)
	if newfont = null then exit sub

	if basefont = null then
		debug "createedgefont wasn't passed a font!"
		exit sub
	end if
	if basefont->layers(1) = null then
		debug "createedgefont was passed a blank font!"
		exit sub
	end if

	'We support newfont == basefont by writing to a temporary Font to begin with
	dim font as Font ptr = callocate(sizeof(Font))
	'font_unload font

	font->layers(0) = fontlayer_new()
	'Share layer 1
	font->layers(1) = basefont->layers(1)
	font->layers(1)->refcount += 1

	dim size as integer
	'since you can only WITH one thing at a time
	dim bchr as FontChar ptr
	bchr = @basefont->layers(1)->chdata(0)

	dim as integer ch

	for ch = 0 to 255
		font->w(ch) = basefont->w(ch)

		with font->layers(0)->chdata(ch)
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
	font->layers(0)->spr = frame_new(size, 1, , YES)

	font->h = basefont->h  '+ 2
	font->offset = basefont->offset
	font->cols = basefont->cols + 1
	font->outline_col = font->cols
	font->initialised = YES

	'Stuff currently hardcoded to keep edged font working as before
	font->offset.x = 1
	font->offset.y = 1
	'font->h += 2

	'dim as ubyte ptr maskp = basefont->layers(0)->spr->mask
	dim as ubyte ptr sptr
	dim as ubyte ptr srcptr = font->layers(1)->spr->image
	dim as integer x, y

	for ch = 0 to 255
		with font->layers(0)->chdata(ch)
			sptr = font->layers(0)->spr->image + .offset + .w + 1
			for y = 1 to .h - 2
				for x = 1 to .w - 2
					if *srcptr then
						sptr[-.w + 0] = font->outline_col
						sptr[  0 - 1] = font->outline_col
						sptr[  0 + 1] = font->outline_col
						sptr[ .w + 0] = font->outline_col
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

	font_unload newfont
	memcpy(newfont, font, sizeof(Font))
	deallocate(font)
end sub

'Create a version of a font with a drop shadow (in a new palette colour)
sub font_create_shadowed (byval newfont as Font ptr, byval basefont as Font ptr, byval xdrop as integer = 1, byval ydrop as integer = 1)
	if newfont = null then exit sub

	if basefont = null then
		debug "createshadowfont wasn't passed a font!"
		exit sub
	end if
	if basefont->layers(1) = null then
		debug "createshadowfont was passed a blank font!"
		exit sub
	end if

	'We support newfont == basefont by writing to a temporary Font to begin with
	dim font as Font ptr = callocate(sizeof(Font))
	'font_unload font

	memcpy(font, basefont, sizeof(Font))

	'Copy layer 1 from the old font to layer 0 of the new
	font->layers(0) = fontlayer_duplicate(basefont->layers(1))

	'Share layer 1 with the base font
	font->layers(1)->refcount += 1

	font->cols += 1
	font->outline_col = font->cols
	font->initialised = YES

	for ch as integer = 0 to 255
		with font->layers(0)->chdata(ch)
			.offx += xdrop
			.offy += ydrop
		end with
	next

	with *font->layers(0)->spr
		for i as integer = 0 to .w * .h - 1
			if .image[i] then
				.image[i] = font->outline_col
			end if
		next
	end with

	font_unload newfont
	memcpy(newfont, font, sizeof(Font))
	deallocate(font)
end sub

sub font_loadold1bit (byval font as Font ptr, byval fontdata as ubyte ptr)
	if font = null then exit sub
	font_unload font

	font->layers(1) = fontlayer_new()
	font->layers(1)->spr = frame_new(8, 256 * 8)
	font->h = 10  'I would have said 9, but this is what was used in text slices
	font->offset.x = 0
	font->offset.y = 0
	font->cols = 1
	font->initialised = YES

	'dim as ubyte ptr maskp = font->layers(1)->spr->mask
	dim as ubyte ptr sptr = font->layers(1)->spr->image

	dim as integer ch, x, y
	dim as integer fi 'font index
	dim as integer fstep

	for ch = 0 to 255
		font->w(ch) = 8
		with font->layers(1)->chdata(ch)
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
#IFDEF __FB_64BIT__
			fstep = iif(fstep = 1, 7, 1) 'uneven steps due to 2->8 byte thunk
#ELSE
			fstep = iif(fstep = 1, 3, 1) 'uneven steps due to 2->4 byte thunk
#ENDIF
			sptr += 1 - 8 * 8
			'maskp += 1 - 8 * 8
		next
		sptr += 8 * 8 - 8
		'maskp += 8 * 8 - 8
	next
end sub

'Load each character from an individual BMP in a directory, falling back to some other
'font for missing BMPs
'This sub is for testing purposes only, and will be removed unless this shows some use:
'uses hardcoded values
sub font_loadbmps (byval newfont as Font ptr, directory as string, byval fallback as Font ptr = null)
	if newfont = null then exit sub
	'We support fallback == font by writing to a temporary Font to begin with
	dim font as Font ptr = callocate(sizeof(Font))
	'font_unload font

	font->layers(0) = null
	font->layers(1) = fontlayer_new()
	'Hacky: start by allocating 4096 pixels, expand as needed
	font->layers(1)->spr = frame_new(1, 4096)
	font->cols = 1  'hardcoded
	font->initialised = YES

	dim maxheight as integer
	if fallback then
		maxheight = fallback->h
		font->offset.x = fallback->offset.x
		font->offset.y = fallback->offset.y
		font->cols = fallback->cols
	end if

	dim as ubyte ptr image = font->layers(1)->spr->image
	dim as ubyte ptr sptr
	dim as integer size = 0
	dim as integer i
	dim f as string
	dim tempfr as Frame ptr
	dim bchr as FontChar ptr
	bchr = @fallback->layers(1)->chdata(0)

	for i = 0 to 255
		with font->layers(1)->chdata(i)
			f = finddatafile(directory & SLASH & i & ".bmp")
			if isfile(f) then
				'FIXME: awful stuff
				tempfr = frame_import_bmp_raw(f)  ', master())

				.offset = size
				.offx = 0
				.offy = 0
				.w = tempfr->w
				.h = tempfr->h
				if .h > maxheight then maxheight = .h
				font->w(i) = .w
				size += .w * .h
				image = reallocate(image, size)
				sptr = image + .offset
				memcpy(sptr, tempfr->image, .w * .h)
				frame_unload @tempfr
			else
				if fallback = null ORELSE fallback->layers(1) = null then
					debug "font_loadbmps: fallback font not provided"
					font_unload font
					deallocate(font)
					'font_unload newfont
					exit sub
				end if

				.offset = size
				.offx = bchr->offx
				.offy = bchr->offy
				.w = bchr->w
				.h = bchr->h
				font->w(i) = .w
				size += .w * .h
				image = reallocate(image, size)
				memcpy(image + .offset, fallback->layers(1)->spr->image + bchr->offset, .w * .h)
			end if
		end with

		bchr += 1
	next

	font->layers(1)->spr->image = image
	font->h = maxheight

	font_unload newfont
	memcpy(newfont, font, sizeof(Font))
	deallocate(font)
end sub

'Load a font from a BMP which contains all 256 characters in a 16x16 grid (all characters the same size)
sub font_loadbmp_16x16 (byval font as Font ptr, filename as string)
	if font = null then exit sub

	dim bmp as Frame ptr
	bmp = frame_import_bmp_raw(filename)

	if bmp = NULL then
		debug "font_loadbmp_16x16: couldn't load " & filename
		exit sub
	end if

	if bmp->w MOD 16 OR bmp->h MOD 16 then
		debug "font_loadbmp_16x16: " & filename & ": bad dimensions " & bmp->w & "*" & bmp->h
		exit sub
	end if

	font_unload font

	dim as integer charw, charh
	charw = bmp->w \ 16
	charh = bmp->h \ 16
	font->h = charh
	font->offset.x = 0
	font->offset.y = 0
	font->initialised = YES
	font->layers(0) = null
	font->layers(1) = fontlayer_new()

	'"Linearise" the characters. In future this will be unnecessary
	font->layers(1)->spr = frame_new(charw, charh * 256)

	dim as integer size = 0

	for i as integer = 0 to 255
		with font->layers(1)->chdata(i)
			.offset = size
			.offx = 0
			.offy = 0
			.w = charw
			.h = charh
			font->w(i) = .w
			size += .w * .h
			dim tempview as Frame ptr
			tempview = frame_new_view(bmp, charw * (i MOD 16), charh * (i \ 16), charw, charh)
			'setclip , charh * i, , charh * (i + 1) - 1, font->layers(1)->spr
			frame_draw tempview, , 0, charh * i, 1, NO, font->layers(1)->spr
			frame_unload @tempview
		end with
	next

	'Find number of used colours
	font->cols = 0
	dim as ubyte ptr image = bmp->image
	for i as integer = 0 to bmp->pitch * bmp->h - 1
		if image[i] > font->cols then font->cols = image[i]
	next

	frame_unload @bmp
end sub

sub setfont (f() as integer)
	font_loadold1bit(@fonts(0), cast(ubyte ptr, @f(0)))
	font_create_edged(@fonts(1), @fonts(0))
end sub

'NOTE: the following two functions are for the old style fonts, they will
'be removed when switching to the new system supporting unicode fonts

'These old style fonts store the type of the font in first integer (part of character
'0). The default "Latin-1.ohf" and "OHRRPGCE Default.ohf" fonts are marked as Latin 1, so
'any font derived from them will be too (ability to change the type only added in Callipygous)

function get_font_type (font() as integer) as fontTypeEnum
	if font(0) <> ftypeASCII and font(0) <> ftypeLatin1 then
		debugc errPromptBug, "Unknown font type ID " & font(0)
		return ftypeASCII
	end if
	return font(0)
end function

sub set_font_type (font() as integer, ty as fontTypeEnum)
	if ty <> ftypeASCII and ty <> ftypeLatin1 then
		debugc errPromptBug, "set_font_type: bad type " & ty
	end if
	font(0) = ty
end sub


'==========================================================================================
'                                       BMP routines
'==========================================================================================
'other formats are probably quite simple
'with Allegro or SDL or FreeImage, but we'll stick to this for now.


sub surface_export_bmp (f as string, byval surf as Surface Ptr, maspal() as RGBcolor)
	if surf->format = SF_32bit then
		surface_export_bmp24(f, surf)
	else
		'A wrapper
		dim fr as Frame
		fr.w = surf->width
		fr.h = surf->height
		fr.pitch = surf->width
		fr.image = surf->pPaletteData
		fr.mask = surf->pPaletteData
		frame_export_bmp8(f, @fr, maspal())
	end if
end sub

sub surface_export_bmp24 (f as string, byval surf as Surface Ptr)
	dim argb as RGBQUAD
	dim as integer of, y, i, skipbytes
	dim as RGBcolor ptr sptr
	dim as ubyte buf(3)

	of = write_bmp_header(f, surf->width, surf->height, 24)
	if of = -1 then exit sub

	skipbytes = 4 - (surf->width * 3 mod 4)
	if skipbytes = 4 then skipbytes = 0
	sptr = surf->pColorData + (surf->height - 1) * surf->width
	for y = surf->height - 1 to 0 step -1
		'put is possibly the most screwed up FB builtin; the use of the fput wrapper soothes the soul
		for x as integer = 0 to surf->width - 1
			fput(of, , @sptr[x], 3)
		next
		sptr -= surf->width
		'pad to 4-byte boundary
		fput(of, , @buf(0), skipbytes)
	next

	close #of
end sub

sub frame_export_bmp8 (f as string, byval fr as Frame Ptr, maspal() as RGBcolor)
	dim argb as RGBQUAD
	dim as integer of, y, i, skipbytes
	dim as ubyte ptr sptr

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

sub frame_export_bmp4 (f as string, byval fr as Frame Ptr, maspal() as RGBcolor, byval pal as Palette16 ptr)
	dim argb as RGBQUAD
	dim as integer of, x, y, i, skipbytes
	dim as ubyte ptr sptr
	dim as ubyte pix

	of = write_bmp_header(f, fr->w, fr->h, 4)
	if of = -1 then exit sub

	for i = 0 to 15
		argb.rgbRed = maspal(pal->col(i)).r
		argb.rgbGreen = maspal(pal->col(i)).g
		argb.rgbBlue = maspal(pal->col(i)).b
		put #of, , argb
	next

	skipbytes = 4 - ((fr->w / 2) mod 4)
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

'Creates a new file and writes the bmp headers to it.
'Returns a file handle, or -1 on error.
private function write_bmp_header(f as string, byval w as integer, byval h as integer, byval bitdepth as integer) as integer
	dim header as BITMAPFILEHEADER
	dim info as BITMAPINFOHEADER

	dim as integer of, imagesize, imageoff

	imagesize = ((w * bitdepth + 31) \ 32) * 4 * h
	imageoff = 54
	if bitdepth <= 8 then
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
	info.biClrUsed = 1 shl bitdepth
	info.biClrImportant = 1 shl bitdepth

	safekill f
	of = freefile
	if open(f for binary access write as #of) then
		debug "write_bmp_header: couldn't open " & f
		return -1
	end if

	put #of, , header
	put #of, , info

	return of
end function

'Open a BMP file, read its headers, and return a file handle.
'Only 1, 4, 8, 24, and 32 bit BMPs are accepted
'Afterwards, the file is positioned at the start of the palette, if there is one
'Returns -1 is invalid, -2 if unsupported
function open_bmp_and_read_header(bmp as string, byref header as BITMAPFILEHEADER, byref info as BITMAPV3INFOHEADER) as integer
	dim bf as integer = freefile
	if open(bmp for binary access read as #bf) then
		debuginfo "open_bmp_and_read_header: couldn't open " & bmp
		return -1
	end if

	get #bf, , header
	if header.bfType <> 19778 then
		close #bf
		debuginfo bmp & " is not a valid BMP file"
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
		debuginfo "Unsupported DIB header size " & biSize & " in " & bmp
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
			debuginfo "Unsupported bitdepth " & info.biBitCount & " in " & bmp
			if info.biBitCount = 2 or info.biBitcount = 16 then
				return -2
			else
				'Invalid
				return -1
			end if
	end select

	if (info.biCompression = BI_RLE4 and info.biBitCount <> 4) or (info.biCompression = BI_RLE8 and info.biBitCount <> 8) then
		close #bf
		debuginfo "Invalid compression scheme " & info.biCompression & " in " & info.biBitCount & "bpp BMP " & bmp
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
			debuginfo "Unsupported BMP RGBA bitmasks " & _
			     HEX(info.biRedMask) & " " & _
			     HEX(info.biGreenMask) & " " & _
			     HEX(info.biBlueMask) & " " & _
			     HEX(info.biAlphaMask) & _
			     " in 32-bit " & bmp
			return -2
		end if
	elseif info.biCompression <> BI_RGB and info.biCompression <> BI_RLE4 and info.biCompression <> BI_RLE8 then
		close #bf
		debuginfo "Unsupported BMP compression scheme " & info.biCompression & " in " & info.biBitCount & "-bit BMP " & bmp
		return -2
	end if

	if info.biHeight < 0 then
		'A negative height indicates that the image is not stored upside-down. Unimplemented
		close #bf
		debuginfo "Unsupported non-flipped image in " & bmp
		return -2
	end if	

	'Seek to palette
	'(some extra data might sit between the header and the palette only if the compression is BI_BITFIELDS
	seek #bf, 1 + sizeof(BITMAPFILEHEADER) + biSize

	return bf
end function


function frame_import_bmp24_or_32(bmp as string, pal() as RGBcolor, firstindex as integer = 0, options as integer = 0) as Frame ptr
'loads and palettises the 24-bit or 32-bit bitmap bmp, mapped to palette pal()
'The alpha channel if any is ignored
'Pass firstindex = 1 to prevent anything from getting mapped to colour 0.

	dim header as BITMAPFILEHEADER
	dim info as BITMAPV3INFOHEADER
	dim bf as integer

	bf = open_bmp_and_read_header(bmp, header, info)
	if bf <= -1 then return 0

	if info.biBitCount <> 24 and info.biBitCount <> 32 then
		debugc errPromptBug, "frame_import_bmp24_or_32 should not have been called!"
		close #bf
		return NULL
	end if

	'navigate to the beginning of the bitmap data
	seek #bf, header.bfOffBits + 1

	dim surf as Surface ptr
	gfx_surfaceCreate(info.biWidth, info.biHeight, SF_32bit, SU_Staging, @surf)

	if info.biBitCount = 24 then
		loadbmp24(bf, surf)
	elseif info.biBitCount = 32 then
		loadbmp32(bf, surf, info)
	end if

	dim ret as Frame ptr
	ret = quantise_surface(surf, pal(), firstindex, options)

	close #bf
	return ret
end function

sub bitmap2pal (bmp as string, pal() as RGBcolor)
'loads the 24/32-bit 16x16 palette bitmap bmp into palette pal()
'so, pixel (0,0) holds colour 0, (0,1) has colour 16, and (15,15) has colour 255
	dim header as BITMAPFILEHEADER
	dim info as BITMAPV3INFOHEADER
	dim col as RGBTRIPLE
	dim bf as integer
	dim dummy as ubyte
	dim as integer w, h

	bf = open_bmp_and_read_header(bmp, header, info)
	if bf <= -1 then exit sub

	if info.biBitCount < 24 OR info.biWidth <> 16 OR info.biHeight <> 16 then
		close #bf
		debug "bitmap2pal should not have been called!"
		exit sub
	end if

	'navigate to the beginning of the bitmap data
	seek #bf, header.bfOffBits + 1

	for h = 15 to 0 step -1
		for w = 0 to 15
			'read the data
			get #bf, , col
			pal(h * 16 + w).r = col.rgbtRed
			pal(h * 16 + w).g = col.rgbtGreen
			pal(h * 16 + w).b = col.rgbtBlue
		next
		if info.biBitCount = 32 then
			get #bf, , dummy
		end if
	next

	close #bf
end sub

function frame_import_bmp_raw(bmp as string) as Frame ptr
'load a 1-, 4- or 8-bit .BMP, ignoring the palette
	dim header as BITMAPFILEHEADER
	dim info as BITMAPV3INFOHEADER
	dim bf as integer
	dim ret as frame ptr

	bf = open_bmp_and_read_header(bmp, header, info)
	if bf <= -1 then return 0

	if info.biBitCount > 8 then
		close #bf
		debugc errPromptBug, "frame_import_bmp_raw should not have been called!"
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
private function decode_bmp_bitmask(mask as uint32) as integer
	for shift as integer = 0 to 24
		if mask shr shift = &hFF then
			return shift
		end if
	next
	return -1
end function

'Takes an open file handle pointing at start of pixel data and an already sized Surface to load into
private sub loadbmp32(byval bf as integer, byval surf as Surface ptr, infohd as BITMAPV3INFOHEADER)
	dim bitspix as uint32
	dim quadpix as RGBQUAD
	dim sptr as RGBcolor ptr
	dim tempcol as RGBcolor
	dim as integer rshift, gshift, bshift, ashift
	tempcol.a = 0

	if infohd.biCompression = BI_BITFIELDS then
		' The bitmasks have already been verified to be supported, except
		' alpha might be missing
		rshift = decode_bmp_bitmask(infohd.biRedMask)
		gshift = decode_bmp_bitmask(infohd.biGreenMask)
		bshift = decode_bmp_bitmask(infohd.biBlueMask)
		ashift = decode_bmp_bitmask(infohd.biAlphaMask)
	end if

	for y as integer = surf->height - 1 to 0 step -1
		sptr = surf->pColorData + y * surf->width
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
END SUB

'Takes an open file handle pointing at start of pixel data and an already sized Surface to load into
private sub loadbmp24(byval bf as integer, byval surf as Surface ptr)
	dim pix as RGBTRIPLE
	dim ub as ubyte
	dim sptr as RGBcolor ptr
	dim pad as integer

	'data lines are padded to 32-bit boundaries
	pad = 4 - ((surf->width * 3) mod 4)
	if pad = 4 then	pad = 0

	for y as integer = surf->height - 1 to 0 step -1
		sptr = surf->pColorData + y * surf->width
		for x as integer = 0 to surf->width - 1
			get #bf, , pix
			'First 3 bytes of RGBTRIPLE are the same as RGBcolor
			*sptr = *cast(RGBcolor ptr, @pix)
			'We haven't yet defined whether opaque is 0 or 255
			sptr->a = 0
			sptr += 1
		next
		'padding to dword boundary
		for w as integer = 0 to pad-1
			get #bf, , ub
		next
	next
end sub

private sub loadbmp8(byval bf as integer, byval fr as Frame ptr)
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

private sub loadbmp4(byval bf as integer, byval fr as Frame ptr)
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

private sub loadbmprle4(byval bf as integer, byval fr as Frame ptr)
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

PRIVATE SUB loadbmprle8(byval bf as integer, byval fr as Frame ptr)
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

private sub loadbmp1(byval bf as integer, byval fr as Frame ptr)
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

function loadbmppal (f as string, pal() as RGBcolor) as integer
'loads the palette of a 1-bit, 4-bit or 8-bit bmp into pal
'returns the number of bits
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
			else
				get #bf, , col4
				pal(i).r = col4.rgbRed
				pal(i).g = col4.rgbGreen
				pal(i).b = col4.rgbBlue
			end if
		next
	else
		debugc errBug, "loadbmppal shouldn't have been called!"
	end if
	close #bf
	return info.biBitCount
end function

sub convertbmppal (f as string, mpal() as RGBcolor, pal() as integer, firstindex as integer = 0)
'Find the nearest match palette mapping from a 1/4/8 bit bmp f to
'the master palette mpal(), and store it in pal(), an array of mpal() indices.
'pal() may contain initial values, used as hints which are used if an exact match.
'Pass firstindex = 1 to prevent anything from getting mapped to colour 0.
	dim bitdepth as integer
	dim cols(255) as RGBcolor

	bitdepth = loadbmppal(f, cols())
	if bitdepth = 0 then exit sub

	for i as integer = 0 to small(UBOUND(pal), (1 SHL bitdepth) - 1)
		pal(i) = nearcolor(mpal(), cols(i).r, cols(i).g, cols(i).b, firstindex, pal(i))
	next
end sub

'Returns 0 if invalid, otherwise fills 'info' and returns 1 if valid but unsupported, 2 if supported
function bmpinfo (f as string, byref info as BITMAPV3INFOHEADER) as integer
	dim header as BITMAPFILEHEADER
	dim bf as integer

	bf = open_bmp_and_read_header(f, header, info)
	if bf = -1 then return 0
	if bf = -2 then return 1
	close #bf
	return 2
end function

'Returns a non-negative integer which is 0 if both colors in a color table are the same
function color_distance(pal() as RGBcolor, byval index1 as integer, byval index2 as integer) as integer
	with pal(index1)
		dim as integer rdif, bdif, gdif
		rdif = .r - pal(index2).r
		gdif = .g - pal(index2).g
		bdif = .b - pal(index2).b
		return rdif*rdif + gdif*gdif + bdif*bdif
	end with
end function

function nearcolor(pal() as RGBcolor, byval red as ubyte, byval green as ubyte, byval blue as ubyte, byval firstindex as integer = 0, byval indexhint as integer = -1) as ubyte
'Figure out nearest palette colour in range [firstindex..255] using Euclidean distance
'A perfect match against pal(indexhint) is tried first
	dim as integer i, diff, best, save, rdif, bdif, gdif

	if indexhint > -1 and indexhint <= UBOUND(pal) then
		with pal(indexhint)
			if red = .r and green = .g and blue = .b then return indexhint
		end with
	end if

	best = 1000000
	save = 0
	for i = firstindex to 255
		rdif = red - pal(i).r
		gdif = green - pal(i).g
		bdif = blue - pal(i).b
		'diff = abs(rdif) + abs(gdif) + abs(bdif)
		diff = rdif*rdif + gdif*gdif + bdif*bdif
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

	nearcolor = save
end function

function nearcolor(pal() as RGBcolor, byval index as integer, byval firstindex as integer = 0) as ubyte
	with pal(index)
		return nearcolor(pal(), .r, .g, .b, firstindex)
	end with
end function

'Convert a 32 bit Surface to a paletted Frame.
'Frees surf.
'Only colours firstindex..255 in pal() are used
private function quantise_surface(surf as Surface ptr, pal() as RGBcolor, firstindex as integer, options as integer = 0) as Frame ptr
	dim ret as Frame ptr
	ret = frame_new(surf->width, surf->height)

	dim inptr as RGBcolor ptr
	dim outptr as ubyte ptr
	for y as integer = 0 to surf->height - 1
		inptr = surf->pColorData + y * surf->width
		outptr = ret->image + y * ret->pitch
		for x as integer = 0 to surf->width - 1
			*outptr = nearcolor(pal(), inptr->r, inptr->g, inptr->b, firstindex)
			inptr += 1
			outptr += 1
		next
	next
	gfx_surfaceDestroy(surf)
	return ret
end function


'==========================================================================================
'                                        Screenshots
'==========================================================================================


sub screenshot (f as string)
	'try external first
	if gfx_screenshot(f) = 0 then
		'otherwise save it ourselves
		frame_export_bmp8(f & ".bmp", vpages(vpage), intpal())
	end if
end sub

sub bmp_screenshot(f as string)
	'This is for when you explicitly want a bmp screenshot, and NOT the preferred
	'screenshot type used by the current gfx backend
	frame_export_bmp8(f & ".bmp", vpages(vpage), intpal())
end sub

private sub snapshot_check
'The best of both worlds. Holding down F12 takes a screenshot each frame, however besides
'the first, they're saved to the temporary directory until key repeat kicks in, and then
'moved, to prevent littering
'NOTE: global variables like tmpdir can change between calls, have to be lenient
	static as string*4 image_exts(3) => {".bmp", ".png", ".jpg", ".dds"}
	static as string backlog()
	initialize_static_dynamic_array(backlog)
	static as integer backlog_num

	dim as integer n, i

	if keyval(scF12) = 0 then
		'delete the backlog
		for n = 1 to ubound(backlog)
			'debug "killing " & backlog(n)
			safekill backlog(n)
		next
		redim backlog(0)
		backlog_num = 0
	else
		dim as string shot
		dim as string gamename = trimextension(trimpath(sourcerpg))
		if gamename = "" then
			gamename = "ohrrpgce"
		end if

		for n = backlog_num to 9999
			shot = gamename + right("000" & n, 4)
			'checking curdir, which is export directory
			for i = 0 to ubound(image_exts)
				if isfile(shot + image_exts(i)) then continue for, for
			next
			exit for
		next
		backlog_num = n + 1

		if keyval(scF12) = 1 then
			shot = tmpdir + shot
			screenshot shot
			for i = 0 to ubound(image_exts)
				if isfile(shot + image_exts(i)) then str_array_append(backlog(), shot + image_exts(i))
			next
		else
			screenshot shot
			'move our backlog of screenshots to the visible location
			for n = 1 to ubound(backlog)
				'debug "moving " & backlog(n) & " to " & curdir + slash + trimpath(backlog(n))
				local_file_move backlog(n), curdir + slash + trimpath(backlog(n))
			next
			redim backlog(0)
		end if
		'debug "screen " & shot
	end if
end sub


'==========================================================================================
'                                 Graphics render clipping
'==========================================================================================


'NOTE: there is only one set of clipping values, shared globally for
'all drawing operations... this is probably a bad thing, but that is how
'it works. The frame or page argument to setclip() is used to determine
'the allowed range of clipping values.

'Set the bounds used by various (not quite all?) video page drawing functions.
'setclip must be called to reset the clip bounds whenever the wrkpage changes, to ensure
'that they are valid (the video page dimensions might differ).
'Aside from tracking which page the clips are for, some legacy code actually uses wrkpage,
'these should be removed.
sub setclip(byval l as integer = 0, byval t as integer = 0, byval r as integer = 999999, byval b as integer = 999999, byval page as integer)
	wrkpage = page
	setclip l, t, r, b, vpages(wrkpage)
end sub

'more modern version
'would call this directly everywhere, but don't want to break that edge case that actually needs wrkpage set
sub setclip(byval l as integer = 0, byval t as integer = 0, byval r as integer = 999999, byval b as integer = 999999, byval fr as Frame ptr = 0)
	if fr <> 0 then clippedframe = fr
	with *clippedframe
		clipl = bound(l, 0, .w) '.w valid, prevents any drawing
		clipt = bound(t, 0, .h)
		clipr = bound(r, 0, .w - 1)
		clipb = bound(b, 0, .h - 1)
	end with
end sub

'Shrinks clipping area, never grows it
sub shrinkclip(byval l as integer = 0, byval t as integer = 0, byval r as integer = 999999, byval b as integer = 999999, byval fr as Frame ptr)
	if clippedframe <> fr then
		clippedframe = fr
		clipl = 0
		clipt = 0
		clipr = 999999
		clipb = 999999
	end if
	with *clippedframe
		clipl = bound(large(clipl, l), 0, .w) '.w valid, prevents any drawing
		clipt = bound(large(clipt, t), 0, .h)
		clipr = bound(small(clipr, r), 0, .w - 1)
		clipb = bound(small(clipb, b), 0, .h - 1)
	end with
end sub

sub saveclip(byref buf as ClipState)
	buf.whichframe = clippedframe
	buf.clipr = clipr
	buf.clipl = clipl
	buf.clipt = clipt
	buf.clipb = clipb
end sub

sub loadclip(byref buf as ClipState)
	clippedframe = buf.whichframe
	clipr = buf.clipr
	clipl = buf.clipl
	clipt = buf.clipt
	clipb = buf.clipb
end sub

'trans: draw transparently, either using ->mask if available, or otherwise use colour 0 as transparent
'warning! Make sure setclip has been called before calling this
private sub drawohr(byval src as Frame ptr, byval dest as Frame ptr, byval pal as Palette16 ptr = null, byval x as integer, byval y as integer, byval trans as bool = YES)
	dim as integer startx, starty, endx, endy
	dim as integer srcoffset

	startx = x
	endx = x + src->w - 1
	starty = y
	endy = y + src->h - 1

	if startx < clipl then
		srcoffset = (clipl - startx)
		startx = clipl
	end if

	if starty < clipt then
		srcoffset += (clipt - starty) * src->pitch
		starty = clipt
	end if

	if endx > clipr then
		endx = clipr
	end if

	if endy > clipb then
		endy = clipb
	end if

	if starty > endy or startx > endx then exit sub

	blitohr(src, dest, pal, srcoffset, startx, starty, endx, endy, trans)
end sub


'==========================================================================================
'                                   Sprite (Frame) cache
'==========================================================================================


'not to be used outside of the sprite functions
declare sub frame_delete_members(byval f as frame ptr)
declare sub frame_freemem(byval f as frame ptr)
declare sub Palette16_delete(byval f as Palette16 ptr ptr)
declare sub spriteset_freemem(byval sprset as SpriteSet ptr)
'Assumes pitch == w
declare sub frame_add_mask(byval fr as frame ptr, byval clr as bool = NO)


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
CONST SPRCACHEB_SZ = 256  'in SPRITE_BASE_SZ units
'#DEFINE COMBINED_SPRCACHE_LIMIT 1


' removes a sprite from the cache, and frees it.
private sub sprite_remove_cache(byval entry as SpriteCacheEntry ptr)
	if entry->p->refcount <> 1 then
		debug "error: invalidly uncaching sprite " & entry->hashed.hash & " " & frame_describe(entry->p)
	end if
	dlist_remove(sprcacheB.generic, entry)
	hash_remove(sprcache, entry)
	entry->p->cacheentry = NULL  'help to detect double free
	frame_freemem(entry->p)
	#ifdef COMBINED_SPRCACHE_LIMIT
		sprcacheB_used -= entry->cost
	#else
		if entry->Bcached then
			sprcacheB_used -= entry->cost
		end if
	#endif
	deallocate(entry)
end sub

'Free some sprites from the end of the B cache
'Returns true if enough space was freed
private function sprite_cacheB_shrink(byval amount as integer) as bool
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

sub sprite_purge_cache(byval minkey as integer, byval maxkey as integer, leakmsg as string, byval freeleaks as bool = NO)
	dim iterstate as integer = 0
	dim as SpriteCacheEntry ptr pt, nextpt

	nextpt = NULL
	pt = hash_iter(sprcache, iterstate, nextpt)
	while pt
		nextpt = hash_iter(sprcache, iterstate, nextpt)
		'recall that the cache counts as a reference
		if pt->p->refcount <> 1 then
			debug "warning: " & leakmsg & pt->hashed.hash & " with " & pt->p->refcount & " references"
			if freeleaks then sprite_remove_cache(pt)
		else
			sprite_remove_cache(pt)
		end if
		pt = nextpt
	wend
end sub

'Unlike sprite_purge_cache, this reloads (in-use) sprites from file, without changing the pointers
'to them. Any sprite that's not actually in use is removed from the cache as it's unnecessary to reload.
sub sprite_update_cache(byval minkey as integer, byval maxkey as integer)
	dim iterstate as integer = 0
	dim as SpriteCacheEntry ptr pt, nextpt

	nextpt = NULL
	pt = hash_iter(sprcache, iterstate, nextpt)
	while pt
		nextpt = hash_iter(sprcache, iterstate, nextpt)

		if pt->hashed.hash < minkey or pt->hashed.hash > maxkey then
			pt = nextpt
			continue while
		end if

		'recall that the cache counts as a reference
		if pt->p->refcount <> 1 then
			dim newframe as Frame ptr = NULL

			if pt->hashed.hash >= 100000000 then
				'Tileset

				dim mxs as Frame ptr
				mxs = loadmxs(game + ".til", pt->hashed.hash - 100000000)
				if mxs <> NULL then newframe = frame_to_tileset(mxs)
				frame_unload @mxs
			else
				'.PT# file

				dim ptno as integer = pt->hashed.hash \ 1000000
				dim rec as integer = pt->hashed.hash MOD 1000000
				with sprite_sizes(ptno)
					newframe = frame_load(game + ".pt" & ptno, rec, .frames, .size.w, .size.h)
				end with
			end if

			if newframe <> NULL then
				if newframe->arraylen <> pt->p->arraylen then
					fatalerror "sprite_update_cache: wrong number of frames!"
				else
					'Transplant the data from the new Frame into the old Frame, so that no
					'pointers need to be updated. pt (the SpriteCacheEntry) doesn't need to
					'to be modified at all

					dim refcount as integer = pt->p->refcount
					dim wantmask as bool = (pt->p->mask <> NULL)
					'Remove the host's previous organs
					frame_delete_members pt->p
					'Insert the new organs
					memcpy(pt->p, newframe, sizeof(Frame) * newframe->arraylen)
					'Having removed everything from the donor, dispose of it
					Deallocate(newframe)
					'Fix the bits we just clobbered
					pt->p->cached = 1
					pt->p->refcount = refcount
					pt->p->cacheentry = pt
					'Make sure we don't crash if we were using a mask (might be the wrong mask though)
					if wantmask then frame_add_mask pt->p

				end if
			end if
		else
			sprite_remove_cache(pt)
		end if
		pt = nextpt
	wend
end sub

'Reload all graphics from one .PT# file
sub sprite_update_cache_pt(byval ptno as integer)
	sprite_update_cache(1000000 * ptno, 1000000 * (ptno + 1) - 1)
end sub

'Reload all tilesets
sub sprite_update_cache_tilesets()
	sprite_update_cache(100000000, 100000000 + 32767)
end sub

'Attempt to completely empty the sprite cache, detecting memory leaks
sub sprite_empty_cache()
	sprite_purge_cache(INT_MIN, INT_MAX, "leaked sprite ")
	if sprcacheB_used <> 0 or sprcache.numitems <> 0 then
		debug "sprite_empty_cache: corruption: sprcacheB_used=" & sprcacheB_used & " items=" & sprcache.numitems
	end if
end sub

'removes all tilesets from the cache
sub tileset_empty_cache()
	sprite_purge_cache (100000000, 110000000, "could not purge tileset ")
end sub

sub sprite_debug_cache()
	debug "==sprcache=="
	dim iterstate as integer = 0
	dim pt as SpriteCacheEntry ptr = NULL

	while hash_iter(sprcache, iterstate, pt)
		debug pt->hashed.hash & " cost=" & pt->cost & " : " & frame_describe(pt->p)
	wend

	debug "==sprcacheB== (used units = " & sprcacheB_used & "/" & SPRCACHEB_SZ & ")"
	pt = sprcacheB.first
	while pt
		debug pt->hashed.hash & " cost=" & pt->cost & " : " & frame_describe(pt->p)
		pt = pt->cacheB.next
	wend
end sub

'a sprite has no references, move it to the B cache
private sub sprite_to_B_cache(byval entry as SpriteCacheEntry ptr)
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
private sub sprite_from_B_cache(byval entry as SpriteCacheEntry ptr)
	dlist_remove(sprcacheB.generic, entry)
	entry->Bcached = NO
	#ifndef COMBINED_SPRCACHE_LIMIT
		sprcacheB_used -= entry->cost
	#endif
end sub

' search cache, update as required if found
private function sprite_fetch_from_cache(byval key as integer) as Frame ptr
	dim entry as SpriteCacheEntry ptr

	entry = hash_find(sprcache, key)

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

' adds a newly loaded frame to the cache with a given key
private sub sprite_add_cache(byval key as integer, byval p as frame ptr)
	if p = 0 then exit sub

	dim entry as SpriteCacheEntry ptr
	entry = callocate(sizeof(SpriteCacheEntry))

	entry->hashed.hash = key
	entry->p = p
	entry->cost = (p->w * p->h * p->arraylen) \ SPRCACHE_BASE_SZ + 1
	'leave entry->cacheB unlinked
	entry->Bcached = NO

	'the cache counts as a reference, but only to the head element of an array!!
	p->cached = 1
	p->refcount += 1
	p->cacheentry = entry
	hash_add(sprcache, entry)

	#ifdef COMBINED_SPRCACHE_LIMIT
		sprcacheB_used += entry->cost
	#endif
end sub


'==========================================================================================
'                                          Frames
'==========================================================================================


'Create a blank Frame.
'By default not initialised; pass clr=YES to initialise to 0
function frame_new(byval w as integer, byval h as integer, byval frames as integer = 1, byval clr as bool = NO, byval wantmask as bool = NO) as Frame ptr
	if w < 1 or h < 1 or frames < 1 then
		debugc errPromptBug, "frame_new: bad size " & w & "*" & h & "*" & frames
		return 0
	end if

	dim ret as frame ptr
	'this hack was Mike's idea, not mine!
	ret = callocate(sizeof(Frame) * frames)

	'no memory? shucks.
	if ret = 0 then
		debug "Could not create sprite frames, no memory"
		return 0
	end if

	dim as integer i, j
	for i = 0 to frames - 1
		with ret[i]
			'the caller to frame_new is considered to have a ref to the head; and the head to have a ref to each other elem
			'so set each refcount to 1
			.refcount = 1
			.arraylen = frames
			if i > 0 then .arrayelem = 1
			.w = w
			.h = h
			.pitch = w '+ 10  'test pitch conversion work
			.mask = NULL
			if clr then
				.image = callocate(.pitch * h)
				if wantmask then .mask = callocate(.pitch * h)
			else
				.image = allocate(.pitch * h)
				if wantmask then .mask = allocate(.pitch * h)
			end if

			if .image = 0 or (.mask = 0 and wantmask <> NO) then
				debug "Could not allocate sprite frames, no memory"
				'well, I don't really see the point freeing memory, but who knows...
				frame_freemem(ret)
				return 0
			end if
		end with
	next
	return ret
end function

'Create a frame which is a view onto part of a larger frame
'Can return a zero-size view. Seems to work, but not yet sure that all operations will work correctly on such a frame.
function frame_new_view(byval spr as Frame ptr, byval x as integer, byval y as integer, byval w as integer, byval h as integer) as Frame ptr
	dim ret as frame ptr = callocate(sizeof(Frame))

	if ret = 0 then
		debug "Could not create sprite view, no memory"
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
		.image = spr->image + .pitch * y + x
		if spr->mask then
			.mask = spr->mask + .pitch * y + x
		end if
		.refcount = 1
		.arraylen = 1 'at the moment not actually used anywhere on sprites with isview = 1
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

private sub frame_delete_members(byval f as frame ptr)
	if f->arrayelem then debug "can't free arrayelem!": exit sub
	for i as integer = 0 to f->arraylen - 1
		deallocate(f[i].image)
		deallocate(f[i].mask)
		f[i].image = NULL
		f[i].mask = NULL
		f[i].refcount = FREEDREFC  'help to detect double free
	next
	'spriteset_freemem also calls frame_freemem
	if f->sprset then
		f->sprset->frames = NULL
		spriteset_freemem f->sprset
	end if
end sub

' unconditionally frees a sprite from memory.
' You should never need to call this: use frame_unload
' Should only be called on the head of an array (and not a view, obv)!
' Warning: not all code calls frame_freemem to free sprites! Grrr!
private sub frame_freemem(byval f as frame ptr)
	if f = 0 then exit sub
	frame_delete_members f
	deallocate(f)
end sub

'Public:
' Loads a 4-bit sprite (stored in columns (2/byte)) from one of the .pt? files, with caching.
' It will return a pointer to the first frame, and subsequent frames
' will be immediately after it in memory. (This is a hack, and will probably be removed)
function frame_load(byval ptno as integer, byval rec as integer) as frame ptr
	dim starttime as double = timer
	dim ret as Frame ptr
	dim key as integer = ptno * 1000000 + rec

	if ptno < 0 or rec < 0 then
		debug "frame_load: invalid ptno=" & ptno & " and rec=" & rec
		return 0
	end if

	ret = sprite_fetch_from_cache(key)
	if ret then return ret

	with sprite_sizes(ptno)
		'debug "loading " & ptno & "  " & rec
		'cachemiss += 1
		ret = frame_load(game + ".pt" & ptno, rec, .frames, .size.w, .size.h)
	end with

	if ret then sprite_add_cache(key, ret)
	debug_if_slow(starttime, 0.1, key)
	return ret
end function

function tileset_load(byval num as integer) as Frame ptr
	dim ret as Frame ptr
	dim key as integer = 100000000 + num

	ret = sprite_fetch_from_cache(key)
	if ret then return ret

	'debug "loading tileset" & ptno & "  " & rec
	'cachemiss += 1

	dim mxs as Frame ptr
	mxs = loadmxs(game + ".til", num)
	if mxs = NULL then return NULL
	ret = frame_to_tileset(mxs)
	frame_unload @mxs

	if ret then sprite_add_cache(key, ret)
	return ret
end function

' You can use this to load a .pt?-format 4-bit sprite from some non-standard location.
' No code does this. Does not use a cache.
' It will return a pointer to the first frame (of num frames), and subsequent frames
' will be immediately after it in memory. (This is a hack, and will probably be removed)
function frame_load(fi as string, byval rec as integer, byval num as integer, byval wid as integer, byval hei as integer) as frame ptr
	dim ret as frame ptr

	'first, we do a bit of math:
	dim frsize as integer = wid * hei / 2
	dim recsize as integer = frsize * num

	'make sure the file is real
	if not isfile(fi) then
		debug "frame_load: can't read " + fi
		return 0
	end if

	'now, we can load the sprite
	dim f as integer = freefile

	'open() returns 0 for success
	if open(fi for binary access read as #f) then
		debug "sprites: could not open " & fi
		return 0
	end if

	'if we get here, we can assume that all's well, and allocate the memory
	ret = frame_new(wid, hei, num)

	if ret = 0 then
		close #f
		return 0
	end if

	'find the right sprite (remember, it's base-1)
	seek #f, recsize * rec + 1

	dim i as integer, x as integer, y as integer, z as ubyte

	for i = 0 to num - 1
		with ret[i]
			'although it's a four-bit sprite, it IS an 8-bit bitmap.

			for x = 0 to wid - 1
				for y = 0 to hei - 1
					'pull up two pixels
					get #f,,z

					'the high nybble is the first pixel
					.image[y * wid + x] = (z SHR 4)

					y+=1

					'and the low nybble is the second one
					.image[y * wid + x] = z AND 15

					'it is worth mentioning that sprites are stored in columns, not rows
				next
			next
		end with
	next

	close #f

	return ret
end function

'Appends a new "frame" child node
'TODO: Assumes the frame is 8 bit, and doesn't save metadata about palette or master palette
'TODO: Doesn't save mask, but we don't have any need to serialise masks at the moment
function frame_to_node(fr as Frame ptr, parent as NodePtr) as NodePtr
	dim as NodePtr frame_node, image_node
	frame_node = AppendChildNode(parent, "frame")
	AppendChildNode(frame_node, "w", fr->w)
	AppendChildNode(frame_node, "h", fr->h)
	'"bits" gives the format of the "image" node; whether this Frame
	'is a 4 or 8 bit sprite is unknown (and would be stored separately)
	AppendChildNode(frame_node, "bits", 8)

	image_node = AppendChildNode(frame_node, "image")
	'Allocate uninitialised memory
	SetContent(image_node, NULL, fr->w * fr->h)
	dim imdata as ubyte ptr = GetZString(image_node)
	for y as integer = 0 TO fr->h - 1
		memcpy(imdata + y * fr->w, fr->image + y * fr->pitch, fr->w)
	next

	return frame_node
end function

'Loads a Frame from a "frame" node (node name not enforced)
function frame_from_node(node as NodePtr) as Frame ptr
	dim as integer bitdepth = GetChildNodeInt(node, "bits", 8)
	dim as integer w = GetChildNodeInt(node, "w"), h = GetChildNodeInt(node, "h")
	if bitdepth <> 8 then
		debugc errPromptError, "frame_from_node: Unsupported graphics bitdepth " & bitdepth
		return NULL
	end if
	dim fr as Frame ptr
	fr = frame_new(w, h)
	if fr = NULL then
		'If the width or height was bad then an error already shown
		return NULL
	end if

	dim image_node as NodePtr = GetChildByName(node, "image")
	dim imdata as ubyte ptr = GetZString(image_node)
	dim imlen as integer = GetZStringSize(image_node)
	if imdata = NULL OR imlen < w * h then
		debugc errPromptError, "frame_from_node: Couldn't load image; data is short (" & imlen & " for " & w & "*" & h & ")"
		return NULL
	end if
	memcpy(fr->image, imdata, w * h)
	return fr
end function

'Public:
' Releases a reference to a sprite and nulls the pointer.
' If it is refcounted, decrements the refcount, otherwise it is freed immediately.
' A note on frame arrays: you may pass around pointers to frames in it (call frame_reference
' on them) and then unload them, but no memory will be freed until the head pointer refcount reaches 0.
' The head element will have 1 extra refcount if the frame array is in the cache. Each of the non-head
' elements also have 1 refcount, indicating that they are 'in use' by the head element,
' but this is just for feel-good book keeping
sub frame_unload(byval p as frame ptr ptr)
	if p = 0 then exit sub
	if *p = 0 then exit sub

	if clippedframe = *p then clippedframe = 0
	with **p
		if .refcount <> NOREFC then
			if .refcount = FREEDREFC then
				debug frame_describe(*p) & " already freed!"
				*p = 0
				exit sub
			end if
			.refcount -= 1
			if .refcount < 0 then debug frame_describe(*p) & " has refcount " & .refcount
		end if
		'if cached, can free two references at once
		if (.refcount - .cached) <= 0 then
			if .arrayelem then
				'this should not happen, because each arrayelem gets an extra refcount
				debug "arrayelem with refcount = " & .refcount
				exit sub
			end if
			if .isview then
				frame_unload @.base
				deallocate(*p)
			else
				for i as integer = 1 to .arraylen - 1
					if (*p)[i].refcount <> 1 then
						debug frame_describe(*p + i) & " array elem freed with bad refcount"
					end if
				next
				if .cached then sprite_to_B_cache((*p)->cacheentry) else frame_freemem(*p)
			end if
		end if
	end with
	*p = 0
end sub

function frame_to_tileset(byval spr as Frame ptr) as Frame ptr
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
	return hex(cast(unsigned integer, p))
end function

function frame_describe(byval p as frame ptr) as string
	if p = 0 then return "'(null)'"
	dim temp as string
	if p->sprset then
		temp = "spriteset:<" & p->sprset->numframes & " frames: 0x" & hexptr(p->sprset->frames) _
		       & "," & p->sprset->numanimations & " animations: 0x" & hexptr(p->sprset->animations) & ">"
	end if
	return "'(0x" & hexptr(p) & ") " & p->arraylen & "x" & p->w & "x" & p->h & " img=0x" & hexptr(p->image) _
	       & " msk=0x" & hexptr(p->mask) & " pitch=" & p->pitch & " cached=" & p->cached & " aelem=" _
	       & p->arrayelem & " view=" & p->isview & " base=0x" & hexptr(p->base) & " refc=" & p->refcount & "' " _
	       & temp
end function

'this is mostly just a gimmick
function frame_is_valid(byval p as frame ptr) as bool
	if p = 0 then return NO
	dim ret as bool = YES

	if p->refcount <> NOREFC and p->refcount <= 0 then ret = NO

	'this is an arbitrary test, and in theory, could cause a false-negative, but I can't concieve of 100 thousand references to the same sprite.
	if p->refcount > 100000 then ret = NO

	if p->w < 0 or p->h < 0 then ret = NO
	if p->pitch < p->w then ret = NO

	if p->image = 0 then ret = NO

	'Patterns used by Windows and Linux to scrub memory
	if cint(p->mask) = &hBAADF00D or cint(p->image) = &hBAADF00D then ret = NO
	if cint(p->mask) = &hFEEEFEEE or cint(p->image) = &hFEEEFEEE then ret = NO

	if ret = NO then
		debugc errBug, "Invalid sprite " & frame_describe(p)
		'if we get here, we are probably doomed, but this might be a recovery
		if p->cacheentry then sprite_remove_cache(p->cacheentry)
	end if
	return ret
end function

'Add a mask. NOTE: Only valid on Frames with pitch == w!
'clr: is true, blank mask, otherwise copy image
private sub frame_add_mask(byval fr as frame ptr, byval clr as bool = NO)
	if fr->mask then exit sub
	if clr = NO then
		fr->mask = allocate(fr->w * fr->h)
		'we can just copy .image in one go, since we just ensured it's contiguous
		memcpy(fr->mask, fr->image, fr->w * fr->h)
	else
		fr->mask = callocate(fr->w * fr->h)
	end if
end sub

'for a copy you intend to modify. Otherwise use frame_reference
'note: does not copy frame arrays, only single frames
function frame_duplicate(byval p as frame ptr, byval clr as bool = NO, byval addmask as bool = NO) as frame ptr
	dim ret as frame ptr, i as integer

	if p = 0 then return 0

	ret = callocate(sizeof(frame))

	if ret = 0 then return 0

	ret->w = p->w
	ret->h = p->h
	ret->pitch = p->w
	ret->refcount = 1
	ret->image = 0
	ret->mask = 0
	ret->arraylen = 1

	if p->image then
		if clr = 0 then
			ret->image = allocate(ret->w * ret->h)
			if p->w = p->pitch then
				'a little optimisation (we know ret->w == ret->pitch)
				memcpy(ret->image, p->image, ret->w * ret->h)
			else
				for i = 0 to ret->h - 1
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
				for i = 0 to ret->h - 1
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

function frame_reference(byval p as frame ptr) as frame ptr
	if p = 0 then return 0
	if p->refcount = NOREFC then
		debug "tried to reference a non-refcounted sprite!"
	else
		p->refcount += 1
	end if
	return p
end function

'Public:
' draws a sprite to a page. scale must be greater than or equal to 1. if trans is false, the
' mask will be wholly ignored. Just like drawohr, masks are optional, otherwise use colourkey 0
sub frame_draw(byval src as frame ptr, byval pal as Palette16 ptr = NULL, byval x as integer, byval y as integer, byval scale as integer = 1, byval trans as bool = YES, byval page as integer)
	if src = 0 then
		debug "trying to draw null frame"
		exit sub
	end if

	frame_draw src, pal, x, y, scale, trans, vpages(page)
end sub

sub frame_draw(byval src as Frame ptr, byval pal as Palette16 ptr = NULL, byval x as integer, byval y as integer, byval scale as integer = 1, byval trans as bool = YES, byval dest as Frame ptr)
	if dest <> clippedframe then
		setclip , , , , dest
	end if

	if scale = 1 then
		drawohr src, dest, pal, x, y, trans
		exit sub
	end if

	dim as integer sxfrom, sxto, syfrom, syto

	sxfrom = large(clipl, x)
	sxto = small(clipr, x + (src->w * scale) - 1)

	syfrom = large(clipt, y)
	syto = small(clipb, y + (src->h * scale) - 1)

	blitohrscaled (src, dest, pal, x, y, sxfrom, syfrom, sxto, syto, trans, scale)
end sub

'Public:
' Returns a (copy of the) sprite (any bitdepth) in the midst of a given fade out.
' tlength is the desired length of the transition (in any time units you please),
' t is the number of elasped time units. style is the specific transition.
function frame_dissolved(byval spr as frame ptr, byval tlength as integer, byval t as integer, byval style as integer) as frame ptr
	if t > tlength then return frame_duplicate(spr, YES)

	'by default, sprites use colourkey transparency instead of masks.
	'We could easily not use a mask here, but by using one, this function can be called on 8-bit graphics
	'too; just in case you ever want to fade out a backdrop or something?
	dim startblank as integer = (style = 8 or style = 9)
	dim cpy as frame ptr
	cpy = frame_duplicate(spr, startblank, 1)
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
		case 9 'shrink (horizontal+vertical squash)
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
	end select

	return cpy
end function

function default_dissolve_time(byval style as integer, byval w as integer, byval h as integer) as integer
	'squash, vapourise, phase out, squeeze
	if style = 4 or style = 6 or style = 7 or style = 8 or style = 9 then
		return w / 5
	else
		return w / 2
	end if
end function

'Used by frame_flip_horiz and frame_flip_vert
private sub flip_image(byval pixels as ubyte ptr, byval d1len as integer, byval d1stride as integer, byval d2len as integer, byval d2stride as integer)
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
private sub transform_image(byval src as Frame ptr, byval srcpixels as ubyte ptr, byval destorigin as ubyte ptr, byval d1stride as integer, byval d2stride as integer)
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
sub frame_flip_horiz(byval spr as frame ptr)
	if spr = 0 then exit sub

	if spr->refcount > 1 then
		debug "illegal hflip on " & frame_describe(spr)
		exit sub
	end if

	flip_image(spr->image, spr->h, spr->pitch, spr->w, 1)
	if spr->mask then
		flip_image(spr->mask, spr->h, spr->pitch, spr->w, 1)
	end if
end sub

'Public:
' flips a sprite vertically. In place: you are only allowed to do this on sprites with no other references
sub frame_flip_vert(byval spr as frame ptr)
	if spr = 0 then exit sub

	if spr->refcount > 1 then
		debug "illegal vflip on " & frame_describe(spr)
		exit sub
	end if

	flip_image(spr->image, spr->w, 1, spr->h, spr->pitch)
	if spr->mask then
		flip_image(spr->mask, spr->w, 1, spr->h, spr->pitch)
	end if
end sub

'90 degree (anticlockwise) rotation. Unlike flipping functions, non-destructive!
function frame_rotated_90(byval spr as Frame ptr) as Frame ptr
	if spr = 0 then return NULL

	dim ret as Frame ptr = frame_new(spr->h, spr->w, 1, (spr->mask <> NULL))

	'top left corner transformed to bottom left corner
	transform_image(spr, spr->image, ret->image + ret->pitch * (ret->h - 1), 1, -ret->pitch)

	if spr->mask <> NULL then
		transform_image(spr, spr->mask, ret->mask + ret->pitch * (ret->h - 1), 1, -ret->pitch)
	end if

	return ret
end function

'270 degree (anticlockwise) rotation. Unlike flipping functions, non-destructive!
function frame_rotated_270(byval spr as Frame ptr) as Frame ptr
	if spr = 0 then return NULL

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
sub frame_clear(byval spr as frame ptr, byval colour as integer = 0)
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

sub frame_swap_colors(byval spr as Frame ptr, byval col1 as integer, byval col2 as integer)
	for xx as integer = 0 to spr->w - 1
		for yy as integer = 0 to spr->h - 1
			if readpixel(spr, xx, yy) = col1 then
				putpixel spr, xx, yy, col2
			elseif readpixel(spr, xx, yy) = col2 then
				putpixel spr, xx, yy, col1
			end if
		next
	next
end sub

'Warning: this code is rotting; don't assume ->mask is used, etc. Anyway the whole thing should be replaced with a memmove call or two.
' function frame_scroll(byval spr as frame ptr, byval h as integer = 0, byval v as integer = 0, byval wrap as bool = NO, byval direct as bool = NO) as frame ptr

' 	dim ret as frame ptr, x as integer, y as integer
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
private sub grabrect(byval page as integer, byval x as integer, byval y as integer, byval w as integer, byval h as integer, ibuf as ubyte ptr, tbuf as ubyte ptr = 0)
'this isn't used anywhere anymore, was used to grab tiles from the tileset videopage before loadtileset
'maybe some possible future use?
'ibuf should be pre-allocated
	dim sptr as ubyte ptr
	dim as integer i, j, px, py, l

	if ibuf = null then exit sub

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


'This should be replaced with a real hash
'Note that the palette cache works completely differently to the sprite cache,
'and the palette refcounting system too!

type Palette16Cache
	s as string
	p as Palette16 ptr
end type


redim shared palcache(50) as Palette16Cache

sub Palette16_delete(byval f as Palette16 ptr ptr)
	if f = 0 then exit sub
	if *f = 0 then exit sub
	(*f)->refcount = FREEDREFC  'help detect double frees
	deallocate(*f)
	*f = 0
end sub

'Completely empty the Palette16 cache
'palettes aren't uncached either when they hit 0 references
sub Palette16_empty_cache()
	dim i as integer
	for i = 0 to ubound(palcache)
		with palcache(i)
			if .p <> 0 then
				'debug "warning: leaked palette: " & .s & " with " & .p->refcount & " references"
				Palette16_delete(@.p)
			'elseif .s <> "" then
				'debug "warning: phantom cached palette " & .s
			end if
			.s = ""
		end with
	next
end sub

function Palette16_find_cache(s as string) as Palette16Cache ptr
	dim i as integer
	for i = 0 to ubound(palcache)
		if palcache(i).s = s then return @palcache(i)
	next
	return NULL
end function

sub Palette16_add_cache(s as string, byval p as Palette16 ptr, byval fr as integer = 0)
	if p = 0 then exit sub
	if p->refcount = NOREFC then
		'sanity check
		debug "Tried to add a non-refcounted Palette16 to the palette cache!"
		exit sub
	end if

	dim as integer i, sec = -1
	for i = fr to ubound(palcache)
		with palcache(i)
			if .s = "" then
				.s = s
				.p = p
				exit sub
			elseif .p->refcount <= 0 then
				sec = i
			end if
		end with
	next

	if sec > 0 then
		Palette16_delete(@palcache(sec).p)
		palcache(sec).s = s
		palcache(sec).p = p
		exit sub
	end if

	'no room? pah.
	redim preserve palcache(ubound(palcache) * 1.3 + 5)

	Palette16_add_cache(s, p, i)
end sub

function Palette16_new() as Palette16 ptr
	dim ret as Palette16 ptr
	'--create a new palette which is not connected to any data file
	ret = callocate(sizeof(Palette16))
	'--noncached palettes should be deleted when they are unloaded
	ret->refcount = NOREFC
	return ret
end function

'autotype: spriteset
function Palette16_load(byval num as integer, byval autotype as integer = 0, byval spr as integer = 0) as Palette16 ptr
	dim as Palette16 ptr ret = Palette16_load(game + ".pal", num, autotype, spr)
	if ret = 0 then
		if num >= 0 then
			' Only bother to warn if a specific palette failed to load.
			' Avoids debug noise when default palette load fails because of a non-existant defpal file
			debug "failed to load palette " & num
		end if
	end if
	return ret
end function

function Palette16_load(fil as string, byval num as integer, byval autotype as integer = 0, byval spr as integer = 0) as Palette16 ptr
	dim starttime as double = timer
	dim hashstring as string
	dim cache as Palette16Cache ptr
	if num > -1 then
		hashstring = trimpath(fil) & "#" & num
	else
		num = getdefaultpal(autotype, spr)
		if num <> -1 then
			hashstring = trimpath(fil) & "#" & num
		else
			return 0
		end if
	end if

	'debug "Loading: " & hashstring
	cache = Palette16_find_cache(hashstring)

	if cache <> 0 then
		cache->p->refcount += 1
		return cache->p
	end if

	if not isfile(fil) then return 0

	dim fh as integer = freefile

	if open(fil for binary access read as #fh) then return 0

	dim mag as short

	get #fh, 1, mag

	if mag = 4444 then
		get #fh, , mag
		if num > mag then
			close #fh
			return 0
		end if

		seek #fh, 17 + 16 * num
	else
		seek #fh, 8 + 16 * num
	end if

	dim ret as Palette16 ptr
	ret = callocate(sizeof(Palette16))
	if ret = 0 then
		close #fh
		debug "Could not create palette, no memory"
		return 0
	end if

	'see, it's "mag"ic, since it's used for so many things
	for mag = 0 to 15
		get #fh, , ret->col(mag)
	next
	ret->refcount = 1

	close #fh

	Palette16_add_cache(hashstring, ret)

	'dim d as string
	'd = hex(ret->col(0))
	'for mag = 1 to 15
	'	d &= "," & hex(ret->col(mag))
	'next

	'debug d

	debug_if_slow(starttime, 0.1, fil)
	return ret
end function

sub Palette16_unload(byval p as Palette16 ptr ptr)
	if p = 0 then exit sub
	if *p = 0 then exit sub
	if (*p)->refcount = NOREFC then
		'--noncached palettes should be deleted when they are unloaded
		Palette16_delete(p)
	else
		(*p)->refcount -= 1
		'debug "unloading palette (" & ((*p)->refcount) & " more copies!)"
		'Don't delete: it stays in the cache. Unlike the sprite cache, the much simpler
		'palette cache doesn't count as a reference
	end if
	*p = 0
end sub

'update a .pal-loaded palette even while in use elsewhere.
'(Won't update localpal in a cached PrintStrState... but caching isn't implemented yet)
sub Palette16_update_cache(fil as string, byval num as integer)
	dim oldpal as Palette16 ptr
	dim hashstring as string
	dim cache as Palette16Cache ptr

	hashstring = trimpath(fil) & "#" & num
	cache = Palette16_find_cache(hashstring)

	if cache then
		oldpal = cache->p

		'force a reload, creating a temporary new palette
		cache->s = ""
		cache->p = NULL
		Palette16_load(num)
		cache = Palette16_find_cache(hashstring)

		'copy to old palette structure
		dim as integer oldrefcount = oldpal->refcount
		memcpy(oldpal, cache->p, sizeof(Palette16))
		oldpal->refcount = oldrefcount
		'this sub is silly
		Palette16_delete(@cache->p)
		cache->p = oldpal
	end if
end sub


'==========================================================================================
'                            SpriteSet and SpriteState routines
'==========================================================================================


function spriteset_load_from_pt(byval ptno as integer, byval rec as integer) as SpriteSet ptr
	dim frameset as Frame ptr
	frameset = frame_load(ptno, rec)
	if frameset = NULL then return NULL

	if frameset->sprset = NULL then
		'this Frame array was previously loaded using frame_load; add SpriteSet data
		frameset->sprset = new SpriteSet
		with *frameset->sprset
			.numframes = sprite_sizes(ptno).frames
			.frames = frameset
			'TODO: should pt? records have default animations?
		end with
	end if

	return frameset->sprset
end function

private sub spriteset_freemem(byval sprset as SpriteSet ptr)
	'frame_freemem also calls spriteset_freemem
	if sprset->frames then
		sprset->frames->sprset = NULL
		frame_freemem sprset->frames
	end if
	deallocate sprset->animations
	delete sprset
end sub

sub spriteset_unload(byref ss as SpriteSet ptr)
	'a SpriteSet and its Frame array are never unloaded separately;
	'frame_unload is responsible for all refcounting and unloading
	if ss = NULL then exit sub
	dim temp as Frame ptr = ss->frames
	frame_unload @temp
	ss = NULL
end sub

function sprite_load(byval ptno as integer, byval rec as integer, byval palno as integer = -1) as SpriteState ptr
	dim sprset as SpriteSet ptr = spriteset_load_from_pt(ptno, rec)
	if sprset = NULL then return NULL

	dim ret as SpriteState ptr
	ret = allocate(sizeof(SpriteState))
	with *ret
		.set = sprset
		.pal = Palette16_load(palno, ptno, rec)
		.frame_id = 0
		.curframe = @sprset->frames[.frame_id]
	end with
	return ret
end function

sub sprite_unload(byval spr as SpriteState ptr ptr)
	if spr = NULL then exit sub
	if *spr = NULL then exit sub
	spriteset_unload((*spr)->set)
	Palette16_unload(@(*spr)->pal)
	deallocate *spr
	*spr = NULL
end sub

'loop is number of times to play, or <=0 for infinite
sub sprite_play_animation(spr as SpriteState ptr, anim_name as string, byval loopcount as integer = 1)
	spr->anim_wait = 0
	spr->anim_step = 0
	spr->anim_loop = loopcount - 1
	dim animp as Animation ptr = spr->set->animations
	for i as integer = 0 to spr->set->numanimations - 1
		if animp->name = anim_name then
			spr->anim = animp
			exit sub
		end if
		animp += 1
	next
	debug "Could not find animation '" & anim_name & "'"
end sub

sub sprite_animate(spr as SpriteState ptr)
	with *spr
		if .anim = NULL then exit sub

		dim looplimit as integer = 40
		do
			if .anim_step >= .anim->numitems then
				if .anim_loop = 0 then
					.anim = NULL
					exit sub
				end if
				if .anim_loop > 0 then .anim_loop -= 1
				.anim_step = 0
			end if

			dim op as AnimationOp ptr = @.anim->ops[.anim_step]
			select case op->type
				case animOpWait
					.anim_wait += 1
					if .anim_wait > op->arg1 then
						.anim_wait = 0
					else
						exit do
					end if
				case animOpFrame
					if op->arg1 >= .set->numframes then
						debug "Animation '" & .anim->name & "': illegal frame number " & op->arg1
						.anim = NULL
						exit sub
					end if
					.frame_id = op->arg1
					.curframe = @.set->frames[.frame_id]
				case animOpSetOffset
					.offset.x = op->arg1
					.offset.y = op->arg2
				case animOpRelOffset
					.offset.x += op->arg1
					.offset.y += op->arg2
				case else
					debug "bad animation opcode " & op->type & " in '" & .anim->name & "'"
					.anim = NULL
					exit sub
			end select
			.anim_step += 1

			looplimit -= 1
			if looplimit = 0 then .anim = NULL: exit do
		loop
	end with
end sub

sub sprite_draw(spr as SpriteState ptr, byval x as integer, byval y as integer, byval scale as integer = 1, byval trans as bool = YES, byval page as integer)
	dim as integer realx, realy
	realx = x + spr->curframe->offset.x + spr->offset.x
	realy = y + spr->curframe->offset.y + spr->offset.y
	frame_draw(spr->curframe, spr->pal, realx, realy, scale, trans, page)
end sub


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

sub remap_android_gamepad(byval player as integer, gp as GamePadMap)
	'Does nothing on non-android non-ouya platforms
	'debuginfo "remap_android_gamepad " & gp.Ud & " " & gp.Rd & " " & gp.Dd & " " & gp.Ld & " " & gp.A & " " & gp.B & " " & gp.X & " " & gp.Y & " " & gp.L1 & " " & gp.R1 & " " & gp.L2 & " " & gp.R2
	io_remap_android_gamepad(player, gp)
end sub

sub remap_touchscreen_button (byval button_id as integer, byval ohr_scancode as integer)
	'Does nothing on platforms without touch screens
	'debuginfo "remap_android_gamepad " & button_id & " " & ohr_scancode
	io_remap_touchscreen_button(button_id, ohr_scancode)
end sub

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

sub set_safe_zone_margin (byval margin as integer)
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

