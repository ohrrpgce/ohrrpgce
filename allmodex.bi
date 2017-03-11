'Allmodex FreeBasic Library header

#IFNDEF ALLMODEX_BI
#DEFINE ALLMODEX_BI

#include "config.bi"
#include "const.bi"
#include "reload.bi"
#include "bitmap.bi"
#include "gfx.bi"
#include "gfx_newRenderPlan.bi"
#include "lib/gif.bi"
#include "music.bi"


'WARNING: don't add strings to this
Type Palette16
	col(15) as ubyte 'indices into the master palette
	refcount as int32 'private. This is not like Frame.refcount, it is used by the palette cache.
End Type

Type SpriteCacheEntryFwd as SpriteCacheEntry
Type SpriteSetFwd as SpriteSet

'An 8 bit, single frame of a sprite.
'Don't forget to update definition in allmodex.h when changing this!!
'As a rather ugly hack (TODO: remove), arrays of Frames are sometimes (for sprite sets) allocated contiguously,
'with each having pointers to separate .image and .mask buffers. All will initially have .refcount = 1,
'.arraylen set to the length of the array, and all but first will have .arrayelem = YES.
'WARNING: don't add strings to this
Type Frame
	w as int32
	h as int32
	offset as XYPair   'Draw offset from the position passed to frame_draw. Used by frame_dissolve
	pitch as int32     'pixel (x,y) is at .image[.x + .pitch * .y]; mask and image pitch are the same!
	image as ubyte ptr
	mask as ubyte ptr
	refcount as int32  'see frame_unload in particular for documentation
	arraylen as int32  'how many frames were contiguously allocated in this frame array
	base as Frame ptr    'if a view, the Frame which actually owns this memory
	cacheentry as SpriteCacheEntryFwd ptr
	cached:1 as int32  '(not set for views onto cached sprites) integer, NOT bool!
	arrayelem:1 as int32  'not the first frame in a frame array
	isview:1 as int32
	noresize:1 as int32  '(Video pages only.) Don't resize this page to the window size

	sprset as SpriteSetFwd ptr  'if not NULL, this Frame array is part of a SpriteSet which
                                    'will need to be freed at the same time
End Type

' You can declare vectors of type "Frame ptr vector".
' When you remove a Frame ptr from the vector, frame_unload is called (decrementing the refcount),
' so likewise when you add a Frame ptr frame_reference is called to increment the refcount.
' ** WARNING: This means you have to call frame_unload to decrement the refcount after appending it! **
DECLARE_VECTOR_OF_TYPE(Frame ptr, Frame_ptr)

Type GraphicPair
	sprite as Frame ptr
	pal as Palette16 ptr
End Type


'==========================================================================================
'                              Video pages and video mode

DECLARE SUB setmodex ()
DECLARE SUB restoremode ()
DECLARE FUNCTION allmodex_setoption(opt as string, arg as string) as integer
DECLARE SUB mersenne_twister (byval seed as double)

DECLARE FUNCTION allocatepage(byval w as integer = -1, byval h as integer = -1) as integer
DECLARE FUNCTION duplicatepage (byval page as integer) as integer
DECLARE SUB freepage (byval page as integer)
DECLARE FUNCTION registerpage (byval spr as Frame ptr) as integer
DECLARE SUB copypage (byval src as integer, byval dest as integer)
DECLARE SUB clearpage (byval page as integer, byval colour as integer = -1)
DECLARE SUB resizepage (page as integer, w as integer, h as integer)
DECLARE FUNCTION compatpage() as integer
DECLARE SUB lock_page_size (page as integer, w as integer, h as integer)
DECLARE SUB unlock_page_size (page as integer)

DECLARE SUB unlock_resolution (byval min_w as integer, byval min_h as integer)
DECLARE SUB lock_resolution ()
DECLARE FUNCTION resolution_unlocked () as bool
DECLARE SUB set_resolution (byval w as integer, byval h as integer)
DECLARE FUNCTION get_resolution () as XYPair
DECLARE SUB get_screen_size (byref screenwidth as integer, byref screenheight as integer)
DECLARE SUB set_scale_factor (scale as integer)

DECLARE FUNCTION supports_fullscreen_well () as bool
DECLARE FUNCTION try_check_fullscreen(byref fullscreen as bool) as bool
DECLARE FUNCTION check_user_toggled_fullscreen() as bool

DECLARE SUB setvispage (page as integer, skippable as bool = YES)
DECLARE SUB setvissurface (surface as Surface ptr, skippable as bool = YES)
DECLARE SUB setwindowtitle (title as string)
DECLARE SUB setpal (pal() as RGBcolor)
DECLARE SUB fadeto (byval red as integer, byval green as integer, byval blue as integer)
DECLARE SUB fadetopal (pal() as RGBcolor)


'==========================================================================================
'                                        Maps

Type TileAnimState
  cycle as integer 'Current tile offset (tile to show)
  pt as integer    'Step number of the next step in the animation
  skip as integer  'Number of ticks left in current wait
End Type

Type TilesetData
  num as integer
  spr as Frame ptr
  anim(1) as TileAnimState
  tastuf(40) as integer
End Type

'*** Requires construction + destruction ***
Type TileMap
  wide as integer
  high as integer
  data as ubyte ptr
  layernum as integer
End Type

DECLARE FUNCTION readblock (map as TileMap, byval x as integer, byval y as integer, byval default as integer = 112343211) as integer
DECLARE SUB writeblock (map as TileMap, byval x as integer, byval y as integer, byval v as integer)

DECLARE SUB drawmap OVERLOAD (tmap as TileMap, byval x as integer, byval y as integer, byval tileset as TilesetData ptr, byval p as integer, byval trans as bool = NO, byval overheadmode as integer = 0, byval pmapptr as TileMap ptr = NULL, byval ystart as integer = 0, byval yheight as integer = -1)
DECLARE SUB drawmap OVERLOAD (tmap as TileMap, byval x as integer, byval y as integer, byval tilesetsprite as Frame ptr, byval p as integer, byval trans as bool = NO, byval overheadmode as integer = 0, byval pmapptr as TileMap ptr = NULL, byval ystart as integer = 0, byval yheight as integer = -1, byval largetileset as bool = NO)
DECLARE SUB drawmap OVERLOAD (tmap as TileMap, byval x as integer, byval y as integer, byval tilesetsprite as Frame ptr, byval dest as Frame ptr, byval trans as bool = NO, byval overheadmode as integer = 0, byval pmapptr as TileMap ptr = NULL, byval largetileset as bool = NO)

DECLARE SUB setanim (byval cycle1 as integer, byval cycle2 as integer)
DECLARE SUB setoutside (byval defaulttile as integer)


'==========================================================================================
'                                     Drawing

Enum bgType
	bgFIRST = -2
	bgChequer = -2       'Non-scrolling chequered pattern
	bgChequerScroll = -1 'Scrolling chequered pattern
	'0 - 255 are master palette colors
End Enum

DECLARE SUB drawbox OVERLOAD (x as RelPos, y as RelPos, w as RelPos, h as RelPos, col as integer, thickness as integer = 1, p as integer)
DECLARE SUB drawbox OVERLOAD (dest as Frame ptr, x as RelPos, y as RelPos, w as RelPos, h as RelPos, col as integer, thickness as integer = 1)
DECLARE sub drawants(dest as Frame ptr, x as RelPos, y as RelPos, wide as RelPos, high as RelPos, color as integer = -1)
DECLARE SUB rectangle OVERLOAD (x as RelPos, y as RelPos, w as RelPos, h as RelPos, c as integer, p as integer)
DECLARE SUB rectangle OVERLOAD (fr as Frame Ptr, x as RelPos, y as RelPos, w as RelPos, h as RelPos, c as integer)
DECLARE SUB fuzzyrect OVERLOAD (x as RelPos, y as RelPos, w as RelPos = rWidth, h as RelPos = rHeight, c as integer, p as integer, fuzzfactor as integer = 50)
DECLARE SUB fuzzyrect OVERLOAD (fr as Frame Ptr, x as RelPos, y as RelPos, w as RelPos = rWidth, h as RelPos = rHeight, c as integer, fuzzfactor as integer = 50)
DECLARE SUB draw_background (dest as Frame ptr, bgcolor as bgType = bgChequerScroll, byref chequer_scroll as integer = 0, x as RelPos = 0, y as RelPos = 0, wide as RelPos = rWidth, high as RelPos = rHeight)

Type ClipState
	whichframe as Frame ptr
	clipl as integer
	clipr as integer
	clipt as integer
	clipb as integer
End Type

'NOTE: clipping values are global.
DECLARE SUB setclip (byval l as integer = 0, byval t as integer = 0, byval r as integer = 999999, byval b as integer = 999999, byval fr as Frame ptr = 0)
DECLARE SUB shrinkclip(byval l as integer = 0, byval t as integer = 0, byval r as integer = 999999, byval b as integer = 999999, byval fr as Frame ptr)
DECLARE SUB saveclip(byref buf as ClipState)
DECLARE SUB loadclip(byref buf as ClipState)


DECLARE SUB drawspritex OVERLOAD (pic() as integer, byval picoff as integer, pal as Palette16 ptr, byval x as integer, byval y as integer, byval page as integer, byval scale as integer=1, byval trans as bool = YES)
DECLARE SUB drawspritex OVERLOAD (pic() as integer, byval picoff as integer, pal() as integer, byval po as integer, byval x as integer, byval y as integer, byval page as integer, byval scale as integer = 1, byval trans as bool = YES)
DECLARE SUB drawsprite (pic() as integer, byval picoff as integer, pal() as integer, byval po as integer, byval x as integer, byval y as integer, byval page as integer, byval trans as bool = YES)
DECLARE SUB wardsprite (pic() as integer, byval picoff as integer, pal() as integer, byval po as integer, byval x as integer, byval y as integer, byval page as integer, byval trans as bool = YES)
DECLARE SUB getsprite (pic() as integer, byval picoff as integer, byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval page as integer)
DECLARE SUB stosprite (pic() as integer, byval picoff as integer, byval x as integer, byval y as integer, byval page as integer)
DECLARE SUB loadsprite (pic() as integer, byval picoff as integer, byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval page as integer)
DECLARE SUB bigsprite  (pic() as integer, pal() as integer, byval p as integer, byval x as integer, byval y as integer, byval page as integer, byval trans as bool = YES)
DECLARE SUB hugesprite (pic() as integer, pal() as integer, byval p as integer, byval x as integer, byval y as integer, byval page as integer, byval trans as bool = YES)


DECLARE SUB putpixel OVERLOAD (byval spr as Frame ptr, byval x as integer, byval y as integer, byval c as integer)
DECLARE SUB putpixel OVERLOAD (byval x as integer, byval y as integer, byval c as integer, byval p as integer)
DECLARE FUNCTION readpixel OVERLOAD (byval spr as Frame ptr, byval x as integer, byval y as integer) as integer
DECLARE FUNCTION readpixel OVERLOAD (byval x as integer, byval y as integer, byval p as integer) as integer
DECLARE SUB drawline OVERLOAD (byval dest as Frame ptr, byval x1 as integer, byval y1 as integer, byval x2 as integer, byval y2 as integer, byval c as integer)
DECLARE SUB drawline OVERLOAD (byval x1 as integer, byval y1 as integer, byval x2 as integer, byval y2 as integer, byval c as integer, byval p as integer)
DECLARE SUB paintat (byval dest as Frame ptr, byval x as integer, byval y as integer, byval c as integer)
DECLARE SUB ellipse (byval fr as Frame ptr, byval x as double, byval y as double, byval radius as double, byval c as integer, byval fillcol as integer = -1, byval semiminor as double = 0.0, byval angle as double = 0.0)
DECLARE SUB replacecolor (byval fr as Frame ptr, byval c_old as integer, byval c_new as integer, byval x as integer = -1, byval y as integer = -1, byval w as integer = -1, byval h as integer = -1)

'==========================================================================================

DECLARE SUB storemxs (fil as string, byval record as integer, byval fr as Frame ptr)
DECLARE SUB loadmxs (filen as string, record as integer, dest as Frame ptr)
DECLARE FUNCTION mxs_frame_to_tileset(spr as Frame ptr) as Frame ptr

'==========================================================================================

DECLARE SUB setwait (byval ms as double, byval flagms as double = 0)
DECLARE FUNCTION dowait () as bool
DECLARE SUB enable_speed_control(byval setting as bool = YES)
DECLARE FUNCTION get_tickcount() as integer

'==========================================================================================
'                               Fonts and text rendering

CONST fontPlain = 0
CONST fontEdged = 1
CONST fontShadow = 2

Type FontChar
	offset as integer  'offset into spr->image
	offx as byte   'pixel offsets
	offy as byte
	w as byte      'size of sprite
	h as byte
End Type

'WARNING: don't add strings to this
Type FontLayer
	spr as Frame ptr
	refcount as integer
	chdata(255) as FontChar
End Type

Type Font
	layers(1) as FontLayer ptr	'single layer fonts should use sprite(1) only
	w(255) as integer	'width of each character
	h as integer		'height of a line
	offset as XYPair	'added to coordinates when printing
	cols as integer		'number of used colours, not including colour 0 (transparency), so at most 15
	pal as Palette16 ptr    '(Default) palette template to use, or NULL if this font is unpaletted (foreground colour only)
	outline_col as integer  'palette entry (1 to .cols) which should be filled with uiOutline, or 0 for none.
End Type

'text_layout_dimensions returns this struct
Type StringSize
	h as integer         'Height (in pixels)
	w as integer         'Greatest width of any line
	endchar as integer   'For when maxlines is specified: one character past last line
	lastw as integer     'Width of last line fragment
	lasth as integer     'Height of last line fragment
	lines as integer     'Number of lines (always at least 1)   FIXME:  not true
	finalfont as Font ptr
End Type

Type StringCharPos
	charnum as integer   'offset in string; equal to len(text) if off the end
	exacthit as bool     'whether actually on this character, or just the nearest (eg. off end of line)
	x as integer         'position is in screen coordinates
	y as integer
	'w as integer        'Size of the selected character (do we really need this?)
	h as integer
	lineh as integer     'height of containing line fragment
End Type

Type PrintStrStatePtr as PrintStrState ptr

DECLARE FUNCTION parse_tag(z as string, byval offset as integer, byval action as string ptr, byval arg as int32 ptr) as integer

DECLARE SUB printstr (text as string, x as RelPos, y as RelPos, page as integer, withtags as bool = NO, fontnum as integer = fontPlain)
DECLARE SUB edgeprint (text as string, x as RelPos, y as RelPos, col as integer, page as integer, withtags as bool = NO, withnewlines as bool = NO)
DECLARE SUB wrapprint (text as string, x as RelPos, y as RelPos, col as integer = -1, page as integer, wrapx as RelPos = rWidth, withtags as bool = YES, fontnum as integer = fontEdged)
DECLARE SUB textcolor (byval fg as integer, byval bg as integer)

DECLARE SUB text_layout_dimensions (retsize as StringSize ptr, z as string, endchar as integer = 999999, maxlines as integer = 999999, wide as integer = 999999, fontp as Font ptr, withtags as bool = YES, withnewlines as bool = YES)
DECLARE FUNCTION textwidth(text as string, fontnum as integer = 0, withtags as bool = YES, withnewlines as bool = YES) as integer
DECLARE FUNCTION textsize(text as string, wide as RelPos, fontnum as integer = 0, withtags as bool = YES, page as integer = -1) as XYPair

DECLARE SUB find_point_in_text (byval retsize as StringCharPos ptr, byval seekx as integer, byval seeky as integer, z as string, byval wide as integer = 999999, byval xpos as integer = 0, byval ypos as integer = 0, byval fontnum as integer, byval withtags as bool = YES, byval withnewlines as bool = YES)

DECLARE FUNCTION fgcol_text (text as string, byval colour as integer) as string
DECLARE FUNCTION bgcol_text (text as string, byval colour as integer) as string

DECLARE FUNCTION get_font(fontnum as integer, show_err as bool = NO) as Font ptr

DECLARE SUB setfont (f() as integer)
DECLARE FUNCTION get_font_type (font() as integer) as fontTypeEnum
DECLARE SUB set_font_type (font() as integer, ty as fontTypeEnum)
DECLARE SUB font_unload (fontpp as Font ptr ptr)
DECLARE FUNCTION font_create_edged (basefont as Font ptr) as Font ptr
DECLARE FUNCTION font_create_shadowed (basefont as Font ptr, xdrop as integer = 1, ydrop as integer = 1) as Font ptr
DECLARE FUNCTION font_loadbmps (directory as string, fallback as Font ptr = null) as Font ptr
DECLARE FUNCTION font_loadbmp_16x16 (filename as string) as Font ptr

'==========================================================================================
'                                    BMPs/GIFs/screenshots

DECLARE FUNCTION screenshot (f as string = "") as string
DECLARE SUB bmp_screenshot(f as string)
DECLARE SUB toggle_recording_gif()

DECLARE SUB frame_export_bmp4 (f as string, byval fr as Frame Ptr, maspal() as RGBcolor, byval pal as Palette16 ptr)
DECLARE SUB frame_export_bmp8 (f as string, byval fr as Frame Ptr, maspal() as RGBcolor)
DECLARE SUB surface_export_bmp24 (f as string, byval surf as Surface Ptr)
DECLARE FUNCTION frame_import_bmp24_or_32(bmp as string, pal() as RGBcolor, firstindex as integer = 0, options as integer = 0) as Frame ptr
DECLARE FUNCTION frame_import_bmp_raw(bmp as string) as Frame ptr
DECLARE SUB bitmap2pal (bmp as string, pal() as RGBcolor)
DECLARE FUNCTION loadbmppal (f as string, pal() as RGBcolor) as integer
DECLARE SUB convertbmppal (f as string, mpal() as RGBcolor, pal() as integer, firstindex as integer = 0)
DECLARE FUNCTION color_distance(pal() as RGBcolor, byval index1 as integer, byval index2 as integer) as integer
DECLARE FUNCTION nearcolor OVERLOAD (pal() as RGBcolor, byval red as ubyte, byval green as ubyte, byval blue as ubyte, byval firstindex as integer = 0, byval indexhint as integer = -1) as ubyte
DECLARE FUNCTION nearcolor OVERLOAD (pal() as RGBcolor, byval index as integer, byval firstindex as integer = 0) as ubyte
DECLARE FUNCTION bmpinfo (f as string, byref dat as BitmapV3InfoHeader) as integer

DECLARE SUB GifPalette_from_pal (byref gpal as GifPalette, masterpal() as RGBcolor, pal as Palette16 ptr = NULL)
DECLARE SUB frame_export_gif (fr as Frame Ptr, fname as string, maspal() as RGBcolor, pal as Palette16 ptr = NULL, transparent as bool = NO)


'==========================================================================================
'                                          Input

DECLARE FUNCTION real_keyval(byval a as integer, byval repeat_wait as integer = 0, byval repeat_rate as integer = 0) as integer
DECLARE FUNCTION keyval (byval a as integer, byval repeat_wait as integer = 0, byval repeat_rate as integer = 0, real_keys as bool = NO) as integer
DECLARE FUNCTION getinputtext () as string
DECLARE FUNCTION interrupting_keypress () as bool
DECLARE FUNCTION anykeypressed (byval checkjoystick as bool = YES, trigger_level as integer = 1) as integer
DECLARE FUNCTION waitforanykey () as integer
DECLARE SUB setkeyrepeat (byval repeat_wait as integer = 500, byval repeat_rate as integer = 55)
DECLARE SUB setkeys (byval enable_inputtext as bool = NO)
DECLARE SUB real_clearkey (byval k as integer)
DECLARE SUB clearkey (byval k as integer)
DECLARE SUB real_clear_newkeypress(k as integer)
DECLARE SUB clear_newkeypress(k as integer)
DECLARE SUB setquitflag (newstate as bool = YES)
DECLARE FUNCTION getquitflag () as bool
#DEFINE slowkey(key, ms) (keyval((key), (ms), (ms)) > 1)

DECLARE SUB start_recording_input (filename as string)
DECLARE SUB stop_recording_input (msg as string="", byval errorlevel as ErrorLevelEnum = errError)
DECLARE SUB start_replaying_input (filename as string, num_repeats as integer = 1)
DECLARE SUB stop_replaying_input (msg as string="", byval errorlevel as ErrorLevelEnum = errError)
DECLARE SUB pause_replaying_input
DECLARE SUB resume_replaying_input
DECLARE SUB pause_recording_input
DECLARE SUB resume_recording_input

DECLARE SUB macro_controls ()

Type MouseInfo
	x as integer
	y as integer
	moved as bool         'Whether mouse has moved since last readmouse call.
	movedtick as bool     'Whether mouse has moved since the last setkeys call
	clicks as integer     'Button down event since last readmouse call; MouseButton bitvector (see scancodes.bi)
	clickstick as integer 'Button down since the last setkeys call
	buttons as integer    'Buttons currently down OR clicked; MouseButton bitvector
	wheel as integer      'Wheel movement since last tick; NOT SUPPORTED ON ALL BACKENDS
	dragging as integer   'MouseButton bitvector, but only one button at once can be dragged.
	                      'A dragged button is one held down for at least 2 ticks. 
	                      'So on the first tick, you see click=button=true, dragging=false
	                      'And on the subsequent ticks, you see dragging=button=true, click=false
	clickstart as XYPair  'Mouse position at start of click/drag (Note: no backend currently
	                      'supports reporting the position of click, so currently equal to .x/.y)
End Type

DECLARE FUNCTION havemouse () as bool
DECLARE SUB hidemousecursor ()
DECLARE SUB showmousecursor ()
DECLARE SUB defaultmousecursor ()
DECLARE SUB setcursorvisibility (state as CursorVisibility)
DECLARE FUNCTION getcursorvisibility () as CursorVisibility
DECLARE FUNCTION readmouse () as MouseInfo
DECLARE SUB movemouse (byval x as integer, byval y as integer)
DECLARE SUB mouserect (byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)

DECLARE FUNCTION readjoy OVERLOAD (joybuf() as integer, byval jnum as integer) as bool
DECLARE FUNCTION readjoy (byval joynum as integer, byref buttons as integer, byref x as integer, byref y as integer) as bool


'==========================================================================================
'                                  Music and Sound effects

DECLARE SUB setupmusic
DECLARE SUB closemusic ()
DECLARE SUB loadsong (f as string)
'DECLARE SUB pausesong ()
'DECLARE SUB resumesong ()
DECLARE FUNCTION get_music_volume () as single
DECLARE SUB set_music_volume (byval vol as single)

DECLARE FUNCTION isawav(fi as string) as bool
DECLARE FUNCTION soundfile (sfxnum as integer) as string
DECLARE FUNCTION getmusictype (file as string) as MusicFormatEnum

DECLARE SUB resetsfx ()
DECLARE SUB playsfx (num as integer, loopcount as integer = 0, volume_mult as single = 1.0)
DECLARE SUB stopsfx (num as integer)
DECLARE SUB pausesfx (num as integer)
DECLARE SUB freesfx (num as integer)
DECLARE FUNCTION sfxisplaying (num as integer) as bool
DECLARE FUNCTION effective_sfx_volume (num as integer) as single
DECLARE SUB set_sfx_volume (num as integer, volume_mult as single)
DECLARE SUB set_global_sfx_volume (volume as single)
DECLARE FUNCTION get_global_sfx_volume () as single


'==========================================================================================
'                                          Frame

declare function frame_new(byval w as integer, byval h as integer, byval frames as integer = 1, byval clr as bool = NO, byval wantmask as bool = NO) as Frame ptr
declare function frame_new_view(byval spr as Frame ptr, byval x as integer, byval y as integer, byval w as integer, byval h as integer) as Frame ptr
declare function frame_new_from_buffer(pic() as integer, byval picoff as integer = 0) as Frame ptr
declare sub frame_to_buffer(spr as Frame ptr, pic() as integer)
declare function frame_load(sprtype as SpriteType, record as integer) as Frame ptr
declare function frame_load_4bit(filen as string, record as integer, numframes as integer, wid as integer, hei as integer) as Frame ptr
declare function frame_load_mxs(filen as string, record as integer) as Frame ptr
declare function frame_to_node(fr as Frame ptr, parent as Reload.NodePtr) as Reload.NodePtr
declare function frame_from_node(node as Reload.NodePtr) as Frame ptr
extern "C" 
declare function frame_reference (byval p as frame ptr) as frame ptr
declare sub frame_assign(ptr_to_replace as Frame ptr ptr, new_value as Frame ptr)
declare sub frame_unload (byval p as frame ptr ptr)
end extern
declare sub frame_draw overload (src as frame ptr, pal as Palette16 ptr = NULL, x as RelPos, y as RelPos, scale as integer = 1, trans as bool = YES, page as integer, write_mask as bool = NO)
declare sub frame_draw overload (src as Frame ptr, pal as Palette16 ptr = NULL, x as RelPos, y as RelPos, scale as integer = 1, trans as bool = YES, dest as Frame ptr, write_mask as bool = NO)
declare sub frame_draw overload (src as Frame ptr, masterpal() as RGBcolor, x as RelPos, y as RelPos, trans as bool = YES, dest as Surface ptr)
declare function frame_dissolved(byval spr as frame ptr, byval tlength as integer, byval t as integer, byval style as integer) as frame ptr
declare function default_dissolve_time(byval style as integer, byval w as integer, byval h as integer) as integer
declare sub frame_flip_horiz(byval spr as frame ptr)
declare sub frame_flip_vert(byval spr as frame ptr)
declare function frame_rotated_90(byval spr as Frame ptr) as Frame ptr
declare function frame_rotated_270(byval spr as Frame ptr) as Frame ptr
declare function frame_duplicate(p as Frame ptr, clr as bool = NO, addmask as bool = NO) as Frame ptr
declare function frame_resized(spr as Frame ptr, wide as integer, high as integer, shiftx as integer = 0, shifty as integer = 0, bgcol as integer = 0) as Frame ptr
declare sub frame_clear(byval spr as frame ptr, byval colour as integer = 0)
declare sub frame_swap_colors(byval spr as Frame ptr, byval col1 as integer, byval col2 as integer)
declare sub sprite_empty_cache(sprtype as SpriteType = -1)
declare sub sprite_update_cache(sprtype as SpriteType)
declare sub tileset_empty_cache()
declare function frame_is_valid(byval p as frame ptr) as bool
declare sub sprite_debug_cache()
declare function frame_describe(byval p as frame ptr) as string

'==========================================================================================
'                                       Palette16

declare function palette16_new() as palette16 ptr
declare function palette16_new_from_buffer(pal() as integer, byval po as integer) as Palette16 ptr
declare function palette16_load overload(num as integer, autotype as SpriteType = 0, spr as integer = 0, default_blank as bool = YES) as palette16 ptr
declare function palette16_load overload(fil as string, num as integer, autotype as SpriteType = 0, spr as integer = 0) as palette16 ptr
declare sub palette16_unload(byval p as palette16 ptr ptr)
declare function palette16_duplicate(pal as Palette16 ptr) as Palette16 ptr
declare sub palette16_empty_cache()
declare sub palette16_update_cache(fil as string, byval num as integer)


'==========================================================================================
'                                 SpriteSets and Animations

Enum AnimOpType
	animOpWait      = 0 '(ms)
	animOpWaitMS    = 1 '(ms)
	animOpFrame     = 2 '(framenum)
	animOpRepeat    = 3  '()     Start the animation over
	animOpSetOffset = 4 '(x,y)
	animOpRelOffset	= 5'(x,y)
	animOpLAST      = 5
End Enum

extern anim_op_names() as string  ' Short names used for display and debug
extern anim_op_fullnames() as string  ' Descriptive captions used in editor

Type AnimationOp
	type as AnimOpType
	arg1 as integer
	arg2 as integer
End Type

Type Animation
	name as string
	variant as string
	'numitems as integer
	ops(any) as AnimationOp

	declare constructor()
	declare constructor(name as string, variant as string = "")

	declare sub append(type as AnimOpType, arg1 as integer = 0, arg2 as integer = 0)
End Type

declare sub set_animation_framerate(ms as integer)
declare function ms_to_frames(ms as integer) as integer
declare function frames_to_ms(frames as integer) as integer

Type SpriteSet
	animations(any) as Animation
	num_frames as integer  'redundant to frames->arraylen
	frames as Frame ptr
	'uses refcount from frames

	declare Constructor(frameset as Frame ptr)

	declare sub reference()
	declare function describe() as string
	declare function find_animation(variantname as string) as Animation ptr
	declare function new_animation(name as string = "", variant as string = "") as Animation ptr
End Type

declare function spriteset_load(ptno as SpriteType, record as integer) as SpriteSet ptr
declare sub spriteset_unload(ss as SpriteSet ptr ptr)

' The animation state of a SpriteSet instance
Type SpriteState
	ss as SpriteSet ptr
	frame_num as integer
	anim as Animation ptr      'The currently playing animation or NULL (Not owned)
	anim_step as integer       'Current op index in the current animation
	anim_wait as integer       'Equal to 0 if not waiting, otherwise the number of ticks into the wait.
	anim_loop as integer       '-1:infinite, 0<:number of times to play after current
	anim_looplimit as integer  '(Private) Number of looping ops remaining before
	                           'infinite loop protection is triggered.
	offset as XYPair

	declare constructor(sprset as SpriteSet ptr)
	declare constructor(ptno as SpriteType, record as integer)
	declare destructor()

	declare sub start_animation(name as string, loopcount as integer = 0)
	declare function cur_frame() as Frame ptr

	' Three ways to advance the animation:
	' Advance time by one tick
	declare function animate() as bool
	' Advance time until the next wait
	declare function skip_wait() as integer
	' Advance by one animation op
	declare function animate_step() as bool
End Type


'==========================================================================================
'                                        Platforms

declare sub show_virtual_keyboard()
declare sub hide_virtual_keyboard()
declare sub show_virtual_gamepad()
declare sub hide_virtual_gamepad()

declare sub remap_android_gamepad(byval player as integer, gp as GamePadMap)
declare sub remap_touchscreen_button (byval button_id as integer, byval ohr_scancode as integer)

declare function running_on_desktop() as bool
declare function running_on_console() as bool
declare function running_on_mobile() as bool
declare function running_on_ouya() as bool 'Only use this for things that strictly require OUYA, like the OUYA store

declare sub ouya_purchase_request (dev_id as string, identifier as string, key_der as string)
declare function ouya_purchase_is_ready () as bool
declare function ouya_purchase_succeeded () as bool

declare sub ouya_receipts_request (dev_id as string, key_der as string)
declare function ouya_receipts_are_ready () as bool
declare function ouya_receipts_result () as string

declare function get_safe_zone_margin () as integer
declare sub set_safe_zone_margin (byval margin as integer)
declare function supports_safe_zone_margin () as bool

declare sub email_files(address as string, subject as string, message as string, file1 as zstring ptr = NULL, file2 as zstring ptr = NULL, file3 as zstring ptr = NULL)


'==========================================================================================
'                                         Globals

extern keybdmutex as any ptr
extern modex_initialised as bool
extern vpages() as Frame ptr
extern vpagesp as Frame ptr ptr
extern key2text(3,53) as string*1
extern fonts() as Font ptr
extern global_tog as integer
extern showfps as bool
extern max_display_fps as integer

#ENDIF
