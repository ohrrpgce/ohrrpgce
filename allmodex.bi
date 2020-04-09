'Allmodex FreeBasic Library header

#IFNDEF ALLMODEX_BI
#DEFINE ALLMODEX_BI

#include "config.bi"
#include "const.bi"
#include "reload.bi"
#include "bitmap.bi"
#include "gfx.bi"
#include "surface.bi"
#include "lib/gif.bi"
#include "music.bi"


'This Type is misnamed. But currently, a Palette16 virtually always has numcolors=16
Type Palette16
	numcolors as int32
	refcount as int32 'Always >= 1 (palcache counts as a reference). Can not be NOREFC.
	palnum as int32   '>= 0: numbered palette, cached. -1: not loaded from file, uncached.
	col(255) as ubyte 'indices into the master palette
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
	Union
		Type
			w as int32
			h as int32
		End Type
		size as XYPair
	End Union

	offset as XYPair   'Draw offset from the position passed to frame_draw. Not used yet.
	pitch as int32     'pixel (x,y) is at .image[.x + .pitch * .y]; mask and image pitch are the same!
	image as ubyte ptr 'Pointer to top-left corner. NULL if and onlf it Surface-backed.
	mask as ubyte ptr  'Same shape as image. If not NULL, nonzero bytes in mask are opaque, rather
	                   'than nonzero bytes in image. Most Frames don't have a mask.
	refcount as int32  'see frame_unload in particular for documentation
			   'Must be NOREFC if allocated on stack rather than heap (frame_load/etc)
	arraylen as int32  'how many frames were contiguously allocated in this frame array
	frameid as int32   'Used by frames in a frameset (always in increasing order): alternative to frame number
	base as Frame ptr    'if a view, the Frame which actually owns this memory
	cacheentry as SpriteCacheEntryFwd ptr  'First Frame in array only
	cached:1 as int32  '(not set for views onto cached sprites) integer, NOT bool! First Frame in array only.
	arrayelem:1 as int32  'not the first frame in a Frame array
	isview:1 as int32  'View of another Frame (which might be backed by a Surface, in which we will be
	                   'backed by a Surface too, created with gfx_surfaceCreateView).
	                   'Aside from that, this is NOT true for Surface-backed Frames which aren't views!
	                   'If this is a view, then 'image' and 'mask' mustn't be freed, but 'surf' must be.
	noresize:1 as int32  '(Video pages only.) Don't resize this page to the window size

	surf as Surface ptr  'If not NULL, this is a Surface-backed Frame, and image/mask are NULL,
	                     'but all other members are correct (including .pitch), and match the Surface.
	                     '(View of a WHOLE Surface.) Holds a single reference to surf.

	sprset as SpriteSetFwd ptr  'if not NULL, this Frame array is part of a SpriteSet which
                                    'will need to be freed at the same time
				    'First Frame in array only.
	defpal as int32    'Default palette or -1 if not loaded. Only set on first frame of array!
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

'Information for a specific group of frames (frames xx, 1xx, 2xx, etc), typically shared between
'all spritesets of a certain type
Type FrameGroupInfo
	frameid as integer
	name as string
	default_num as integer  'Number of frames to create in new spritesets, 0 for none

	DECLARE SUB set(frameid as integer, name as string, default_num as integer)
End Type

'==========================================================================================
'                              Video pages and video mode

DECLARE SUB setmodex ()
DECLARE SUB restoremode ()
DECLARE SUB switch_gfx (backendname as string)
DECLARE FUNCTION allmodex_setoption(opt as string, arg as string) as integer
DECLARE SUB flush_gfx_config_settings ()

DECLARE SUB switch_to_32bit_vpages ()
DECLARE SUB switch_to_8bit_vpages ()
DECLARE FUNCTION vpages_are_32bit () as bool
DECLARE FUNCTION allocatepage(w as integer = -1, h as integer = -1, bitdepth as integer = -1) as integer
DECLARE FUNCTION duplicatepage (page as integer) as integer
DECLARE SUB freepage (page as integer)
DECLARE FUNCTION registerpage (spr as Frame ptr) as integer
DECLARE SUB copypage (src as integer, dest as integer)
DECLARE SUB clearpage (page as integer, colour as integer = -1)
DECLARE SUB resizepage (page as integer, w as integer, h as integer)
DECLARE FUNCTION compatpage() as integer
DECLARE SUB lock_page_size (page as integer, w as integer, h as integer)
DECLARE SUB unlock_page_size (page as integer)

DECLARE SUB unlock_resolution (min_w as integer, min_h as integer)
DECLARE SUB lock_resolution ()
DECLARE FUNCTION resolution_unlocked () as bool
DECLARE SUB set_resolution (w as integer, h as integer)
DECLARE FUNCTION get_resolution () as XYPair
DECLARE SUB get_screen_size (byref screenwidth as integer, byref screenheight as integer)
DECLARE SUB set_scale_factor (scale as integer, change_windowsize as bool = YES)
DECLARE SUB toggle_fps_display ()

DECLARE FUNCTION supports_fullscreen_well () as bool
DECLARE FUNCTION try_check_fullscreen(byref fullscreen as bool) as bool

DECLARE SUB setvispage (page as integer, skippable as bool = YES)
DECLARE FUNCTION getvispage () as integer
DECLARE SUB setwindowtitle (title as string)
DECLARE SUB setpal (pal() as RGBcolor)
DECLARE SUB fadeto (red as integer, green as integer, blue as integer)
DECLARE SUB fadetopal (pal() as RGBcolor)

DECLARE SUB show_overlay_message(msg as string, seconds as double = 3.)
DECLARE FUNCTION overlay_message_visible () as bool

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
  Union
    Type
      wide as integer
      high as integer
    End Type
    size as XYPair
  End Union
  data as ubyte ptr
  layernum as integer
End Type

DECLARE FUNCTION readblock (map as TileMap, x as integer, y as integer, default as integer = 112343211) as integer
DECLARE SUB writeblock (map as TileMap, x as integer, y as integer, v as integer)

DECLARE SUB drawmap OVERLOAD (tmap as TileMap, x as integer, y as integer, tileset as TilesetData ptr, p as integer, trans as bool = NO, overheadmode as integer = 0, pmapptr as TileMap ptr = NULL, ystart as integer = 0, yheight as integer = -1, pal as Palette16 ptr = NULL, opts as DrawOptions = def_drawoptions)
DECLARE SUB drawmap OVERLOAD (tmap as TileMap, x as integer, y as integer, tilesetsprite as Frame ptr, p as integer, trans as bool = NO, overheadmode as integer = 0, pmapptr as TileMap ptr = NULL, ystart as integer = 0, yheight as integer = -1, largetileset as bool = NO, pal as Palette16 ptr = NULL, opts as DrawOptions = def_drawoptions)
DECLARE SUB drawmap OVERLOAD (tmap as TileMap, x as integer, y as integer, tilesetsprite as Frame ptr, dest as Frame ptr, trans as bool = NO, overheadmode as integer = 0, pmapptr as TileMap ptr = NULL, largetileset as bool = NO, pal as Palette16 ptr = NULL, opts as DrawOptions = def_drawoptions)
DECLARE SUB draw_layers_at_tile(composed_tile as Frame ptr, tiles as TileMap ptr vector, tilesets as TilesetData ptr vector, tx as integer, ty as integer, pmapptr as TileMap ptr = NULL)

DECLARE SUB setanim OVERLOAD (cycle1 as integer, cycle2 as integer)
DECLARE sub setanim OVERLOAD (tileset as TilesetData ptr)
DECLARE SUB setoutside (defaulttile as integer)


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
DECLARE SUB drawcube (dest as Frame ptr, rect as RectType, off as XYPair, col as integer, thickness as integer = 1)
DECLARE sub drawants (dest as Frame ptr, x as RelPos, y as RelPos, wide as RelPos, high as RelPos, color as integer = -1)
DECLARE SUB rectangle OVERLOAD (x as RelPos, y as RelPos, w as RelPos, h as RelPos, c as integer, p as integer)
DECLARE SUB rectangle OVERLOAD (fr as Frame Ptr, x as RelPos, y as RelPos, w as RelPos, h as RelPos, c as integer)
DECLARE SUB trans_rectangle (dest as Frame ptr, byval rect as RectType, byval col as RGBcolor, alpha as double)
DECLARE SUB fuzzyrect OVERLOAD (x as RelPos, y as RelPos, w as RelPos = rWidth, h as RelPos = rHeight, c as integer, p as integer, fuzzfactor as integer = 50, stationary as bool = NO, zoom as integer = 1, offset as integer = 0)
DECLARE SUB fuzzyrect OVERLOAD (fr as Frame Ptr, x as RelPos, y as RelPos, w as RelPos = rWidth, h as RelPos = rHeight, c as integer, fuzzfactor as integer = 50, stationary as bool = NO, zoom as integer = 1, offset as integer = 0)
DECLARE SUB antifuzzyrect(fr as Frame Ptr, rect as RectType, col as integer, fuzzfactor as integer = 50, zoom as integer = 1)

DECLARE SUB draw_background (dest as Frame ptr, bgcolor as bgType = bgChequerScroll, byref chequer_scroll as integer = 0, x as RelPos = 0, y as RelPos = 0, wide as RelPos = rWidth, high as RelPos = rHeight)

Type ClipState
	'Which Frame the clips are set for
	frame as Frame ptr
	'Drawable area on clippedframe; right & bottom edges are INCLUSIVE!
	l as integer
	t as integer
	r as integer
	b as integer
End Type

'NOTE: clipping values are global.
DECLARE SUB setclip(l as integer = 0, t as integer = 0, r as integer = 999999, b as integer = 999999, fr as Frame ptr = 0)
DECLARE FUNCTION shrinkclip(l as integer = 0, t as integer = 0, r as integer = 999999, b as integer = 999999, fr as Frame ptr = 0) as bool
DECLARE FUNCTION get_cliprect(fr as Frame ptr = NULL) BYREF as ClipState

DECLARE SUB putpixel OVERLOAD (spr as Frame ptr, x as integer, y as integer, c as integer)
DECLARE SUB putpixel OVERLOAD (x as integer, y as integer, c as integer, p as integer)
DECLARE FUNCTION readpixel OVERLOAD (spr as Frame ptr, x as integer, y as integer) as integer
DECLARE FUNCTION readpixel OVERLOAD (x as integer, y as integer, p as integer) as integer
DECLARE SUB drawline OVERLOAD (dest as Frame ptr, x1 as integer, y1 as integer, x2 as integer, y2 as integer, c as integer, dash_cycle as integer = 0, dash_len as integer = 0)
DECLARE SUB drawline OVERLOAD (x1 as integer, y1 as integer, x2 as integer, y2 as integer, c as integer, p as integer, dash_cycle as integer = 0, dash_len as integer = 0)

DECLARE SUB paintat (dest as Frame ptr, x as integer, y as integer, c as integer)
DECLARE SUB ellipse (fr as Frame ptr, x as double, y as double, radius as double, c as integer, fillcol as integer = -1, semiminor as double = 0.0, angle as double = 0.0)
DECLARE SUB replacecolor (fr as Frame ptr, c_old as integer, c_new as integer, swapcols as bool = NO)
DECLARE SUB swapcolors(fr as Frame ptr, col1 as integer, col2 as integer)
DECLARE FUNCTION countcolor (fr as Frame ptr, col as integer) as integer
DECLARE SUB remap_to_palette OVERLOAD (fr as Frame ptr, pal as Palette16 ptr)
DECLARE SUB remap_to_palette OVERLOAD (fr as Frame ptr, palmapping() as integer)

'==========================================================================================

DECLARE SUB storemxs (fil as string, record as integer, fr as Frame ptr)
DECLARE SUB loadmxs (filen as string, record as integer, dest as Frame ptr)
DECLARE FUNCTION mxs_frame_to_tileset(spr as Frame ptr) as Frame ptr

'==========================================================================================

DECLARE SUB setwait (ms as double, flagms as double = 0)
DECLARE FUNCTION dowait () as bool
DECLARE FUNCTION setwait_time_remaining() as double
DECLARE FUNCTION get_tickcount() as integer

'==========================================================================================
'                               Fonts and text rendering

CONST fontPlain = 0
CONST fontEdged = 1
CONST fontShadow = 2

Type FontChar
	offset as integer  'offset into spr->image
	offx as byte       'Amount to offset the image when drawing, in pixels
	offy as byte
	w as byte          'Size of sprite
	h as byte
End Type

'A FontLayer can be shared between multiple fonts
Type FontLayer
	spr as Frame ptr        'FIXME: this is meant to be an atlas/spritesheet, but it's actually a
	                        'concatenation of pixel buffers for each character. Needs changing for backend accel
	refcount as integer
	chdata(255) as FontChar

	declare constructor()
	declare constructor(src as FontLayer ptr)
	declare destructor()
End Type

Type Font
	layers(1) as FontLayer ptr	'single layer fonts should use sprite(1) only
	w(255) as integer	'width of each character
	h as integer		'height of a line
	offset as XYPair	'added to coordinates when printing
	cols as integer		'number of used colours, not including colour 0 (transparency), so at most 15
	pal as Palette16 ptr    '(Default) palette template to use, or NULL if this font is unpaletted (foreground colour only)
	outline_col as integer  'palette entry (1 to .cols) which should be filled with uiOutline, or 0 for none.

	declare constructor()
	declare constructor(src as Font ptr)
	'Recommmended to call the font_unload wrapper instead of using delete directly
	declare destructor()
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
	pos as XYPair        'position is in screen coordinates
	'w as integer        'Size of the selected character (do we really need this?)
	h as integer
	lineh as integer     'height of containing line fragment
End Type

Type PrintStrStatePtr as PrintStrState ptr

DECLARE FUNCTION parse_tag (z as string, offset as integer, byref action as string, arg as int32 ptr) as integer

DECLARE SUB printstr (text as string, x as RelPos, y as RelPos, page as integer, withtags as bool = NO, fontnum as integer = fontPlain)
DECLARE SUB edgeprint (text as string, x as RelPos, y as RelPos, col as integer, page as integer, withtags as bool = NO, withnewlines as bool = NO)
DECLARE SUB wrapprint (text as string, x as RelPos, y as RelPos, col as integer = -1, page as integer, wrapx as RelPos = rWidth, withtags as bool = YES, fontnum as integer = fontEdged)
DECLARE SUB wrapprintbg (text as string, x as RelPos, y as RelPos, col as integer = -1, page as integer, drawbg as bool = YES, wrapx as RelPos = rWidth, withtags as bool = YES, fontnum as integer = fontEdged)
DECLARE SUB textcolor (fg as integer, bg as integer)

DECLARE SUB text_layout_dimensions (retsize as StringSize ptr, z as string, endchar as integer = 999999, maxlines as integer = 999999, wide as integer = 999999, fontp as Font ptr, withtags as bool = YES, withnewlines as bool = YES)
DECLARE FUNCTION textwidth(text as string, fontnum as integer = fontPlain, withtags as bool = YES, withnewlines as bool = YES) as integer
DECLARE FUNCTION textsize(text as string, wide as RelPos = rWidth, fontnum as integer = fontPlain, withtags as bool = YES, page as integer = -1) as XYPair
DECLARE FUNCTION lineheight(fontnum as integer = fontEdged) as integer

DECLARE SUB find_text_char_position(retsize as StringCharPos ptr, text as string, charnum as integer, wide as RelPos = rWidth, fontnum as integer = fontPlain, withtags as bool = YES, page as integer = -1)
DECLARE SUB find_point_in_text (retsize as StringCharPos ptr, seekx as integer, seeky as integer, z as string, wide as integer = 999999, xpos as integer = 0, ypos as integer = 0, fontnum as integer, withtags as bool = YES, withnewlines as bool = YES)

DECLARE FUNCTION fgcol_text (text as string, colour as integer) as string
DECLARE FUNCTION bgcol_text (text as string, colour as integer) as string

DECLARE FUNCTION get_font(fontnum as integer, show_err as bool = NO) as Font ptr

DECLARE SUB setfont (f() as integer)
DECLARE FUNCTION get_font_type (font() as integer) as fontTypeEnum
DECLARE SUB set_font_type (font() as integer, ty as fontTypeEnum)
DECLARE SUB font_unload (fontpp as Font ptr ptr)
DECLARE FUNCTION font_create_edged (basefont as Font ptr) as Font ptr
DECLARE FUNCTION font_create_shadowed (basefont as Font ptr, xdrop as integer = 1, ydrop as integer = 1) as Font ptr
DECLARE FUNCTION font_loadbmps (directory as string, fallback as Font ptr = null) as Font ptr
DECLARE FUNCTION font_load_16x16 (filename as string) as Font ptr

'==========================================================================================
'                                    BMPs/GIFs/screenshots

' Options to quantize_surface
TYPE QuantizeOptions
	firstindex as integer    'Pass firstindex = 1 to prevent anything from getting mapped to colour 0. (Default 0)
	transparency as RGBcolor 'Color to map to 0 (should have .a=0) (Default -1, meaning none)
	dither as bool
	dither_maxerror as integer 'How much dithering to do (kGifMaxAccumError); 0 means no dither. (Default 50)
	compute_palette as bool  'If true, pal() is output rather than input!
	to_intpal as bool        'pal() is intpal() (set by last setpal call). Allows use of nearcolor_fast
	                         '(currently only implemented if dither=NO compute_palette=NO).
	                         'firstindex will be ignored! (Always taken as 0. Maybe should be changed.).
END TYPE

ENUM ImageFileTypes
	imUnknown   'File extension not recognised
	imBMP
	imGIF
	imPNG
	imJPEG
	'Update image_type_strings when changing this
END ENUM

TYPE ImageFileInfo
	imagetype as ImageFileTypes
	imagetype_name as string
	supported as bool   'Can be loaded
	valid as bool       'Appears to valid, regardless of being supported
	paletted as bool    'Has a palette. Check this, not bpp!
	alpha as bool       'Has alpha channel or color key
	                    '(Warning: is false for PNGs with palette w/ transparent colors)
	info as string      'Description, whether supported or not
	error as string     'If not supported, error message describing problem
	size as XYPair
	bpp as integer      'Bits per pixel
END TYPE

DECLARE FUNCTION image_read_info (filename as string) as ImageFileInfo
DECLARE FUNCTION image_load_palette (filename as string, pal() as RGBcolor) as integer
DECLARE FUNCTION image_import_as_frame_raw (filename as string) as Frame ptr
DECLARE FUNCTION image_import_as_frame_paletted (filename as string, pal() as RGBColor) as Frame ptr
DECLARE FUNCTION image_import_as_frame_quantized (bmp as string, pal() as RGBcolor, options as QuantizeOptions = TYPE(0, -1)) as Frame ptr
DECLARE FUNCTION image_import_as_frame_8bit (filename as string, masterpal() as RGBcolor, keep_col0 as bool = YES, byval transparency as RGBcolor = TYPE(-1)) as Frame ptr
DECLARE FUNCTION image_import_as_frame_32bit (filename as string) as Frame ptr

DECLARE FUNCTION image_import_as_surface (bmp as string, always_32bit as bool) as Surface ptr
DECLARE SUB frame_export_image (fr as Frame ptr, filename as string, masterpal() as RGBcolor, pal as Palette16 ptr = NULL)
DECLARE SUB surface_export_image (surf as Surface ptr, filename as string)

DECLARE SUB palette_from_16x16_image (filename as string, pal() as RGBcolor)

DECLARE FUNCTION screenshot(basename as string = "") as string
DECLARE SUB bmp_screenshot(basename as string)
DECLARE SUB start_recording_gif(secondscreen as string = "")
DECLARE FUNCTION recording_gif() as bool
DECLARE SUB toggle_recording_gif()
DECLARE SUB start_forwarding_screen(outfile as string)
DECLARE SUB stop_recording_video()

'Note order of Frame/Surface ptr and filename varies!

DECLARE SUB frame_export_bmp4 (f as string, fr as Frame Ptr, maspal() as RGBcolor, pal as Palette16 ptr)
DECLARE SUB frame_export_bmp8 (f as string, fr as Frame Ptr, maspal() as RGBcolor)
DECLARE SUB surface_export_bmp24 (f as string, surf as Surface Ptr)
DECLARE SUB frame_export_bmp (fname as string, fr as Frame ptr, maspal() as RGBcolor, pal as Palette16 ptr = NULL)

DECLARE FUNCTION frame_import_bmp_raw(bmp as string) as Frame ptr

DECLARE FUNCTION surface_export_jpeg(surf as Surface ptr, filename as string, quality as integer = 95) as bool
DECLARE FUNCTION frame_export_jpeg(fr as Frame ptr, filename as string, masterpal() as RGBcolor, pal as Palette16 ptr = NULL, quality as integer = 95) as bool

'Read BMP info or palette
DECLARE FUNCTION loadbmppal (f as string, pal() as RGBcolor) as integer
DECLARE FUNCTION bmpinfo OVERLOAD (f as string, byref dat as BitmapV3InfoHeader, byref errmsg as string = "") as integer
DECLARE SUB bmpinfo OVERLOAD (filename as string, byref iminfo as ImageFileInfo)

'Color matching
DECLARE FUNCTION color_distance(pal() as RGBcolor, index1 as integer, index2 as integer) as integer
DECLARE FUNCTION nearcolor OVERLOAD (pal() as RGBcolor, red as integer, green as integer, blue as integer, firstindex as integer = 0, indexhint as integer = -1, avoidcol as integer = -1) as ubyte
DECLARE FUNCTION nearcolor OVERLOAD (pal() as RGBcolor, index as integer, firstindex as integer = 0) as ubyte
EXTERN "C"
DECLARE FUNCTION nearcolor_fast OVERLOAD (byval col as RGBcolor) as ubyte
END EXTERN
DECLARE FUNCTION nearcolor_fast OVERLOAD (r as integer, g as integer, b as integer) as ubyte
DECLARE SUB find_palette_mapping (inputpal() as RGBcolor, masterpal() as RGBcolor, mapping() as integer, firstindex as integer = 0)
DECLARE FUNCTION quantize_surface(byref surf as Surface ptr, pal() as RGBcolor, options as QuantizeOptions) as Frame ptr
'firstindex is optional
#DEFINE findrgb(r, g, b, firstindex...)  nearcolor(master(), r, g, b, firstindex)

'Export .gifs
DECLARE SUB GifPalette_from_pal (byref gpal as GifPalette, masterpal() as RGBcolor, pal as Palette16 ptr = NULL)
DECLARE SUB surface_export_gif (surf as Surface Ptr, fname as string, dither as bool = NO)
DECLARE SUB frame_export_gif (fr as Frame Ptr, fname as string, maspal() as RGBcolor, pal as Palette16 ptr = NULL, transparent as bool = NO)


'==========================================================================================
'                                          Input

DECLARE FUNCTION keyval_ex (key as KBScancode, repeat_wait as integer = 0, repeat_rate as integer = 0, real_keys as bool = NO) as KeyBits
DECLARE FUNCTION real_keyval (key as KBScancode) as KeyBits
DECLARE FUNCTION keyval (key as KBScancode) as KeyBits
DECLARE FUNCTION carray alias "KEYVAL" (key as KBScancode) as KeyBits
DECLARE FUNCTION slowkey (key as KBScancode, ms as integer) as bool
DECLARE FUNCTION getinputtext () as string
DECLARE FUNCTION interrupting_keypress () as bool
DECLARE FUNCTION anykeypressed (checkjoystick as bool = YES, checkmouse as bool = YES, trigger_level as KeyBits = 2) as KBScancode
DECLARE FUNCTION waitforanykey (wait_for_resize as bool = NO) as KBScancode
DECLARE SUB waitforkeyrelease ()
DECLARE SUB setkeyrepeat (repeat_wait as integer = 500, repeat_rate as integer = 55)
DECLARE SUB setkeys (enable_inputtext as bool = NO)
DECLARE SUB real_clearkey (key as KBScancode, clear_key_repeat as bool = YES)
DECLARE SUB clearkey (key as KBScancode, clear_key_repeat as bool = YES)
DECLARE SUB clearkeys ()

DECLARE FUNCTION joykeyval (key as JoyScancode, joynum as integer = 0, repeat_wait as integer = 0, repeat_rate as integer = 0, real_keys as bool = NO) as KeyBits
DECLARE FUNCTION joystick_axis (axis as integer, joynum as integer = 0) as integer
DECLARE FUNCTION joystick_info (joynum as integer) as JoystickInfo ptr
DECLARE FUNCTION keybd_to_joy_scancode (key as KBScancode) as JoyScancode
DECLARE FUNCTION num_joysticks () as integer
DECLARE SUB disable_joystick_input()
DECLARE SUB enable_joystick_input() 'defaults to enabled, so this doesn't necessarily need to be called

DECLARE SUB setquitflag (newstate as bool = YES)
DECLARE FUNCTION getquitflag () as bool

DECLARE SUB start_recording_input (filename as string)
DECLARE SUB stop_recording_input (msg as string="", errorlevel as ErrorLevelEnum = errError)
DECLARE SUB start_replaying_input (filename as string, num_repeats as integer = 1)
DECLARE SUB stop_replaying_input (msg as string="", errorlevel as ErrorLevelEnum = errError)
DECLARE SUB pause_replaying_input
DECLARE SUB resume_replaying_input
DECLARE SUB pause_recording_input
DECLARE SUB resume_recording_input

DECLARE SUB macro_controls ()

' This SUB is implemented in Game/Custom and called from allmodex.
DECLARE SUB global_setkeys_hook ()

Type MouseInfo
	Union
		Type
			'Note: position may be wrong if .active is NO.
			'See comments in update_mouse_state
			x as integer
			y as integer
		End Type
		pos as XYPair
	End Union
	lastpos as XYPair     'pos, last tick
	moved as bool         'Whether mouse has moved since the last setkeys call
	moved_dist as integer 'Distance moved since last tick
	active as bool        'Is over the window and the window is focused
	clicks as integer     'Button down since the last setkeys call; MouseButton bitvector (see scancodes.bi)
	buttons as integer    'Buttons currently down OR clicked; MouseButton bitvector
	release as integer    'Buttons that were released last click; MouseButton bitvector
	last_buttons as integer 'used when calculating .release; MouseButton bitvector
	wheel as integer      'Wheel position. Each dedent/click is 120
	wheel_delta as integer  'Change in .wheel since last setkeys
	wheel_clicks as integer 'Multiples of 120 that .wheel has changed since last setkeys (NOT .wheel_delta\120)
		                'If the wheel is moved very slowly, wheel_delta\120 may be always 0.
	dragging as integer   'MouseButton bitvector, but only one button at once can be dragged.
	                      'A dragged button is one held down for at least 2 ticks.
	                      'So on the first tick, you see click=button=true, dragging=false
	                      'And on the subsequent ticks, you see dragging=button=true, click=false
			      '(TODO: rename to holding)
	drag_dist as integer  'The total distance the mouse has been dragged; check to see whether
	                      'actually dragging rather than holding
	clickstart as XYPair  'Mouse position at start of click/drag (Note: no backend currently
	                      'supports reporting the position of click, so currently equal to .x/.y)
	left_click_age as integer 'Keeps track of how many ticks the left button has been held down.
	                      'includes both when the .buttons bit is set and when the .release bit is set

	DECLARE SUB clearclick(button as MouseButton)
End Type

DECLARE FUNCTION havemouse () as bool
DECLARE SUB hidemousecursor ()
DECLARE SUB showmousecursor ()
DECLARE SUB defaultmousecursor ()
DECLARE SUB setcursorvisibility (state as CursorVisibility)
DECLARE FUNCTION getcursorvisibility () as CursorVisibility
DECLARE FUNCTION readmouse () byref as MouseInfo
DECLARE SUB movemouse (x as integer, y as integer)
DECLARE SUB mouserect (xmin as integer, xmax as integer, ymin as integer, ymax as integer)


'==========================================================================================
'                                  Music and Sound effects

DECLARE SUB setupmusic
DECLARE SUB closemusic ()
DECLARE SUB loadsong (f as string)
'DECLARE SUB pausesong ()
'DECLARE SUB resumesong ()
DECLARE FUNCTION get_music_volume () as single
DECLARE SUB set_music_volume (vol as single)

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

declare function graphics_file(filename_or_extn as string) as string

declare function frame_new(w as integer, h as integer, frames as integer = 1, clr as bool = NO, wantmask as bool = NO, with_surface32 as bool = NO, no_alloc as bool = NO) as Frame ptr
declare function frame_new_view(spr as Frame ptr, x as integer, y as integer, w as integer, h as integer) as Frame ptr
declare function frame_load(sprtype as SpriteType, record as integer) as Frame ptr
declare function frame_load_uncached(sprtype as SpriteType, record as integer) as Frame ptr
declare function frame_load_4bit(filen as string, record as integer, numframes as integer, wid as integer, hei as integer) as Frame ptr
declare function frame_load_mxs(filen as string, record as integer) as Frame ptr
declare function frameset_to_node(fr as Frame ptr, parent as Reload.NodePtr) as Reload.NodePtr
declare function frameset_from_node(node as Reload.NodePtr) as Frame ptr
declare function frameid_to_frame(frameset as Frame ptr, frameid as integer, fail as bool = NO) as integer
extern "C"
declare function frame_reference (p as Frame ptr) as Frame ptr
declare sub frame_assign(ptr_to_replace as Frame ptr ptr, new_value as Frame ptr)
declare sub frame_unload (p as Frame ptr ptr)
end extern
declare sub frame_draw overload (src as Frame ptr, pal as Palette16 ptr = NULL, x as RelPos, y as RelPos, trans as bool = YES, page as integer, opts as DrawOptions = def_drawoptions)
declare sub frame_draw overload (src as Frame ptr, pal as Palette16 ptr = NULL, x as RelPos, y as RelPos, trans as bool = YES, dest as Frame ptr, opts as DrawOptions = def_drawoptions)
declare sub frame_draw overload (src as Frame ptr, masterpal() as RGBcolor, pal as Palette16 ptr = NULL, x as RelPos, y as RelPos, trans as bool = YES, dest as Frame ptr, opts as DrawOptions = def_drawoptions)
declare function frame_dissolved(spr as Frame ptr, tlength as integer, t as integer, style as integer) as Frame ptr
declare sub frame_draw_dissolved (src as Frame ptr, pal as Palette16 ptr = NULL, x as RelPos, y as RelPos, trans as bool = YES, dest as Frame ptr, opts as DrawOptions = def_drawoptions, tlength as integer, tick as integer, style as integer)
declare function default_dissolve_time(style as integer, w as integer, h as integer) as integer
declare sub frame_flip_horiz(spr as Frame ptr)
declare sub frame_flip_vert(spr as Frame ptr)
declare function frame_rotated_90(spr as Frame ptr) as Frame ptr
declare function frame_rotated_270(spr as Frame ptr) as Frame ptr
declare function frame_duplicate(p as Frame ptr, clr as bool = NO, addmask as bool = NO) as Frame ptr
declare function frame_resized(spr as Frame ptr, wide as integer, high as integer, shiftx as integer = 0, shifty as integer = 0, bgcol as integer = 0) as Frame ptr
declare function frame_scaled32(src as Frame ptr, wide as integer, high as integer, masterpal() as RGBcolor, pal as Palette16 ptr = NULL) as Frame ptr
declare sub frame_clear(spr as Frame ptr, colour as integer = 0)
declare sub sprite_empty_cache(sprtype as SpriteType = sprTypeInvalid, setnum as integer = -1)
declare sub sprite_update_cache(sprtype as SpriteType)
declare sub cache_all_spritesets(sprtype as SpriteType)
declare sub tileset_empty_cache()
declare function frame_is_valid(p as Frame ptr) as bool
declare sub sprite_debug_cache()
declare function frame_describe(p as Frame ptr) as string

declare function frame_to_surface32(fr as Frame ptr, masterpal() as RGBcolor, pal as Palette16 ptr = NULL) as Surface ptr
declare sub frame_convert_to_32bit(fr as Frame ptr, masterpal() as RGBcolor, pal as Palette16 ptr = NULL)
declare sub frame_drop_surface(fr as Frame ptr)
declare function frame_with_surface(surf as Surface ptr) as Frame ptr


'==========================================================================================
'                                       Palette16

enum ColorMixMethod
	mixBlend
	mixMult
end enum

enum ColorOperator
	copLuminance
	copValue
	copTintValue

	copGreyscale = copLuminance
end enum

declare function palette16_new(numcolors as integer = 16) as Palette16 ptr
declare function Palette16_new_identity(numcolors as integer = 16) as Palette16 ptr
declare function palette16_new_from_indices(pal() as integer) as Palette16 ptr
declare function palette16_load(num as integer, autotype as SpriteType = sprTypeInvalid, spr as integer = 0, expect_exists as bool = YES) as Palette16 ptr
declare function palette16_load_pal_uncached(fil as string, num as integer) as Palette16 ptr
declare sub palette16_unload(p as Palette16 ptr ptr)
declare function palette16_duplicate(pal as Palette16 ptr) as Palette16 ptr
declare sub palette16_reload_cache()
declare sub palette16_update_cache(num as integer)
declare function palette16_describe(pal as Palette16 ptr) as string
declare sub Palette16_transform_n_match(pal as Palette16 ptr, method as ColorOperator)
declare sub palette16_mix_n_match(pal as Palette16 ptr, byval col as RGBcolor, colfrac as double, method as ColorMixMethod, scale as double = 1.0)


'==========================================================================================
'                                 SpriteSets and Animations

Enum AnimOpType
	animOpWait      = 0 '(ms)
	animOpWaitMS    = 1 '(ms)
	animOpFrame     = 2 '(frameid)
	animOpRepeat    = 3  '()     Start the animation over
	animOpSetOffset = 4 '(x,y)
	animOpRelOffset = 5 '(x,y)
	animOpLAST      = 5
End Enum

extern anim_op_names() as string      ' Short names used for display and debug
extern anim_op_node_names() as string ' Short names used for RELOAD serialisation
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
	frames as Frame ptr    'Does NOT count as a reference
	'uses refcount from frames
	global_animations as SpriteSet ptr  'The default animations for sprites of this type. May be NULL
	                                    '(This counts as a reference)
	'This is private!
	declare constructor(frameset as Frame ptr)

	declare function num_frames() as integer
	declare sub reference()
	declare function describe() as string
	declare function find_animation(variantname as string) as Animation ptr
	declare function new_animation(name as string = "", variant as string = "") as Animation ptr
End Type

declare function spriteset_load(ptno as SpriteType, record as integer) as SpriteSet ptr
declare sub spriteset_unload(ss as SpriteSet ptr ptr)
declare function spriteset_for_frame(fr as Frame ptr) as SpriteSet ptr
declare function empty_spriteset() as SpriteSet ptr
declare function load_global_animations(sprtype as SpriteType, rgfxdoc as Reload.DocPtr = NULL) as SpriteSet ptr

declare function frame_array_to_vector(frames as Frame ptr) as Frame ptr vector
declare function frame_vector_to_array(frames as Frame ptr vector) as Frame ptr

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

declare sub remap_android_gamepad(player as integer, gp as GamePadMap)
declare sub remap_touchscreen_button (button_id as integer, ohr_scancode as integer)

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
declare sub set_safe_zone_margin (margin as integer)
declare function supports_safe_zone_margin () as bool

declare sub email_files(address as string, subject as string, message as string, file1 as zstring ptr = NULL, file2 as zstring ptr = NULL, file3 as zstring ptr = NULL)


'==========================================================================================



'Use these macros to avoid deadlocking on gfxmutex due to reentering allmodex from an
'exception handler, if it's already held by the main thread. Not used by the polling thread.
#macro GFX_ENTER
	if main_thread_in_gfx_backend then
		debugc errBug, "GFX_ENTER reentered!"
		'Try to recover by skipping whatever we were about to do
		goto skip_gfx
	end if
	main_thread_in_gfx_backend = YES
	mutexlock gfxmutex
#endmacro
#macro GFX_EXIT
	mutexunlock gfxmutex
	main_thread_in_gfx_backend = NO
	skip_gfx:
#endmacro


'==========================================================================================
'                                         Globals

extern gfxmutex as any ptr
extern main_thread_in_gfx_backend as bool
extern modex_initialised as bool
extern vpages() as Frame ptr
extern vpagesp as Frame ptr ptr
extern key2text(3,53) as string*1
extern fonts() as Font ptr
extern global_tog as integer
extern max_display_fps as integer
extern use_speed_control as bool
extern user_toggled_fullscreen as bool
extern active_seconds as double
extern idle_time_threshold as double
extern joysticks_globally_disabled as bool
extern "C"
extern pintpal as RGBcolor ptr
end extern

#ENDIF
