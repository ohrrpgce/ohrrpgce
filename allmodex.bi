'Allmodex FreeBasic Library header

#IFNDEF ALLMODEX_BI
#DEFINE ALLMODEX_BI

#include "udts.bi"
#include "reload.bi"
#include "config.bi"
#include "bitmap.bi"
#include "file.bi"   'FB header
#include "lumpfile.bi"
#include "gfx.bi"
#include "gfx_newRenderPlan.bi"
#include "music.bi"


'Library routines
DECLARE SUB modex_init ()
DECLARE SUB setmodex ()
DECLARE SUB modex_quit ()
DECLARE SUB restoremode ()
DECLARE SUB mersenne_twister (byval seed as double)

DECLARE FUNCTION allocatepage(byval w as integer = -1, byval h as integer = -1) as integer
DECLARE FUNCTION duplicatepage (byval page as integer) as integer
DECLARE SUB freepage (byval page as integer)
DECLARE FUNCTION registerpage (byval spr as Frame ptr) as integer
DECLARE SUB copypage (byval src as integer, byval dest as integer)
DECLARE SUB clearpage (byval page as integer, byval colour as integer = -1)
DECLARE SUB resizepage (page as integer, w as integer, h as integer)
DECLARE FUNCTION compatpage() as integer

DECLARE SUB unlock_resolution (byval min_w as integer, byval min_h as integer)
DECLARE SUB lock_resolution ()
DECLARE SUB set_resolution (byval w as integer, byval h as integer)
DECLARE FUNCTION get_resolution_w () as integer
DECLARE FUNCTION get_resolution_h () as integer

DECLARE SUB setvispage (byval page as integer)
DECLARE SUB setwindowtitle (title as string)
DECLARE SUB setpal (pal() as RGBcolor)
DECLARE SUB fadeto (byval red as integer, byval green as integer, byval blue as integer)
DECLARE SUB fadetopal (pal() as RGBcolor)

DECLARE FUNCTION frame_to_tileset(byval spr as frame ptr) as frame ptr
DECLARE FUNCTION tileset_load(byval num as integer) as Frame ptr

DECLARE FUNCTION readblock (map as TileMap, byval x as integer, byval y as integer) as integer
DECLARE SUB writeblock (map as TileMap, byval x as integer, byval y as integer, byval v as integer)

DECLARE SUB drawmap OVERLOAD (tmap as TileMap, byval x as integer, byval y as integer, byval tileset as TilesetData ptr, byval p as integer, byval trans as bool = NO, byval overheadmode as integer = 0, byval pmapptr as TileMap ptr = NULL, byval ystart as integer = 0, byval yheight as integer = -1)
DECLARE SUB drawmap OVERLOAD (tmap as TileMap, byval x as integer, byval y as integer, byval tilesetsprite as Frame ptr, byval p as integer, byval trans as bool = NO, byval overheadmode as integer = 0, byval pmapptr as TileMap ptr = NULL, byval ystart as integer = 0, byval yheight as integer = -1, byval largetileset as bool = NO)
DECLARE SUB drawmap OVERLOAD (tmap as TileMap, byval x as integer, byval y as integer, byval tilesetsprite as Frame ptr, byval dest as Frame ptr, byval trans as bool = NO, byval overheadmode as integer = 0, byval pmapptr as TileMap ptr = NULL, byval largetileset as bool = NO)

DECLARE SUB setanim (byval cycle1 as integer, byval cycle2 as integer)
DECLARE SUB setoutside (byval defaulttile as integer)

'--box drawing
DECLARE SUB drawbox OVERLOAD (byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval col as integer, byval thickness as integer = 1, byval p as integer)
DECLARE SUB drawbox OVERLOAD (byval dest as Frame ptr, byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval col as integer, byval thickness as integer = 1)
DECLARE SUB rectangle OVERLOAD (byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval c as integer, byval p as integer)
DECLARE SUB rectangle OVERLOAD (byval fr as Frame Ptr, byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval c as integer)
DECLARE SUB fuzzyrect OVERLOAD (byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval c as integer, byval p as integer, byval fuzzfactor as integer = 50)
DECLARE SUB fuzzyrect OVERLOAD (byval fr as Frame Ptr, byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval c as integer, byval fuzzfactor as integer = 50)
DECLARE SUB draw_background (x as integer, y as integer, wide as integer, high as integer, bgcolor as integer, byref chequer_scroll as integer, dest as Frame ptr)


'NOTE: clipping values are global.
DECLARE SUB setclip OVERLOAD (byval l as integer = 0, byval t as integer = 0, byval r as integer = 999999, byval b as integer = 999999, byval fr as Frame ptr = 0)
DECLARE SUB setclip (byval l as integer = 0, byval t as integer = 0, byval r as integer = 999999, byval b as integer = 999999, byval page as integer)
DECLARE SUB shrinkclip(byval l as integer = 0, byval t as integer = 0, byval r as integer = 999999, byval b as integer = 999999, byval fr as Frame ptr)
DECLARE SUB saveclip(byref buf as ClipState)
DECLARE SUB loadclip(byref buf as ClipState)
DECLARE SUB drawspritex (pic() as integer, byval picoff as integer, pal() as integer, byval po as integer, byval x as integer, byval y as integer, byval page as integer, byval scale as integer=1, byval trans as bool = YES)
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

DECLARE SUB storemxs (fil as string, byval record as integer, byval fr as Frame ptr)
DECLARE FUNCTION loadmxs (fil as string, byval record as integer, byval dest as Frame ptr = 0) as Frame ptr

DECLARE SUB setwait (byval t as integer, byval flagt as integer = 0)
DECLARE FUNCTION dowait () as bool
DECLARE SUB enable_speed_control(byval setting as bool = YES)
DECLARE FUNCTION get_tickcount() as integer

DECLARE FUNCTION parse_tag(z as string, byval offset as integer, byval action as string ptr, byval arg as int32 ptr) as integer

TYPE PrintStrStatePtr as PrintStrState Ptr

DECLARE SUB text_layout_dimensions (byval retsize as StringSize ptr, z as string, byval endchar as integer = 999999, byval maxlines as integer = 999999, byval wide as integer = 999999, byval fontnum as integer, byval withtags as bool = YES, byval withnewlines as bool = YES)
DECLARE SUB printstr OVERLOAD (byval dest as Frame ptr, s as string, byval x as integer, byval y as integer, byval wide as integer = 999999, byval fontnum as integer, byval withtags as bool = YES, byval withnewlines as bool = YES)
DECLARE SUB printstr OVERLOAD (s as string, byval x as integer, byval y as integer, byval p as integer, byval withtags as bool = NO)
DECLARE SUB edgeprint (s as string, byval x as integer, byval y as integer, byval c as integer, byval p as integer, byval withtags as bool = NO, byval withnewlines as bool = NO)
DECLARE SUB textcolor (byval fg as integer, byval bg as integer)

DECLARE FUNCTION textwidth (z as string, byval fontnum as integer = 0, byval withtags as bool = YES, byval withnewlines as bool = YES) as integer

DECLARE SUB find_point_in_text (byval retsize as StringCharPos ptr, byval seekx as integer, byval seeky as integer, z as string, byval wide as integer = 999999, byval xpos as integer = 0, byval ypos as integer = 0, byval fontnum as integer, byval withtags as bool = YES, byval withnewlines as bool = YES)

DECLARE FUNCTION fgcol_text (text as string, byval colour as integer) as string
DECLARE FUNCTION bgcol_text (text as string, byval colour as integer) as string

DECLARE SUB setfont (f() as integer)
DECLARE FUNCTION get_font_type (font() as integer) as fontTypeEnum
DECLARE SUB set_font_type (font() as integer, ty as fontTypeEnum)
DECLARE SUB font_create_edged (byval font as Font ptr, byval basefont as Font ptr)
DECLARE SUB font_create_shadowed (byval font as Font ptr, byval basefont as Font ptr, byval xdrop as integer = 1, byval ydrop as integer = 1)
DECLARE SUB font_loadbmps (byval font as Font ptr, directory as string, byval fallback as Font ptr = null)
DECLARE SUB font_loadbmp_16x16 (byval font as Font ptr, filename as string)

DECLARE SUB storeset (fil as string, byval i as integer, byval l as integer)
DECLARE SUB loadset (fil as string, byval i as integer, byval l as integer)
DECLARE SUB setpicstuf (buf() as integer, byval b as integer, byval p as integer)

DECLARE SUB setupmusic
DECLARE SUB closemusic ()
DECLARE SUB loadsong (f as string)
'DECLARE SUB pausesong ()
'DECLARE SUB resumesong ()
DECLARE FUNCTION get_music_volume () as single
DECLARE SUB set_music_volume (byval vol as single)

DECLARE SUB screenshot (f as string)
DECLARE SUB bmp_screenshot(f as string)
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

DECLARE FUNCTION isawav(fi as string) as bool

DECLARE FUNCTION keyval (byval a as integer, byval repeat_wait as integer = 0, byval repeat_rate as integer = 0) as integer
DECLARE FUNCTION getinputtext () as string
DECLARE FUNCTION interrupting_keypress () as bool
DECLARE FUNCTION anykeypressed (byval checkjoystick as bool = YES, trigger_level as integer = 0) as integer
DECLARE FUNCTION waitforanykey () as integer
DECLARE SUB setkeyrepeat (byval repeat_wait as integer = 500, byval repeat_rate as integer = 55)
DECLARE SUB setkeys (byval enable_inputtext as bool = NO)
DECLARE SUB clearkey (byval k as integer)
DECLARE SUB setquitflag ()
#DEFINE slowkey(key, ms) (keyval((key), (ms), (ms)) > 1)

DECLARE SUB start_recording_input (filename as string)
DECLARE SUB stop_recording_input ()
DECLARE SUB start_replaying_input (filename as string)
DECLARE SUB stop_replaying_input (msg as string="", byval errorlevel as ErrorLevelEnum = errError)

DECLARE FUNCTION havemouse () as bool
DECLARE SUB hidemousecursor ()
DECLARE SUB unhidemousecursor ()
DECLARE FUNCTION mousecursorvisible () as bool
DECLARE FUNCTION readmouse () as MouseInfo
DECLARE SUB movemouse (byval x as integer, byval y as integer)
DECLARE SUB mouserect (byval xmin as integer, byval xmax as integer, byval ymin as integer, byval ymax as integer)

DECLARE FUNCTION readjoy OVERLOAD (joybuf() as integer, byval jnum as integer) as bool
DECLARE FUNCTION readjoy (byval joynum as integer, byref buttons as integer, byref x as integer, byref y as integer) as bool

DECLARE SUB resetsfx ()
DECLARE SUB playsfx (byval num as integer, byval l as integer=0) 'l is loop count. -1 for infinite loop
DECLARE SUB stopsfx (byval num as integer)
DECLARE SUB pausesfx (byval num as integer)
DECLARE SUB freesfx (byval num as integer) ' only used by custom's importing interface
DECLARE FUNCTION sfxisplaying (byval num as integer) as bool
DECLARE FUNCTION getmusictype (file as string) as MusicFormatEnum
'DECLARE SUB getsfxvol (byval num as integer)
'DECLARE SUB setsfxvol (byval num as integer, byval vol as integer)

'DECLARE FUNCTION getsoundvol () as integer
'DECLARE SUB setsoundvol (byval vol)

'new sprite functions
declare function frame_new(byval w as integer, byval h as integer, byval frames as integer = 1, byval clr as bool = NO, byval wantmask as bool = NO) as Frame ptr
declare function frame_new_view(byval spr as Frame ptr, byval x as integer, byval y as integer, byval w as integer, byval h as integer) as Frame ptr
declare function frame_new_from_buffer(pic() as integer, byval picoff as integer) as Frame ptr
declare function frame_load overload (byval ptno as integer, byval rec as integer) as frame ptr
declare function frame_load(as string, byval as integer, byval as integer , byval as integer, byval as integer) as frame ptr
declare function frame_to_node(fr as Frame ptr, parent as Reload.NodePtr) as Reload.NodePtr
declare function frame_from_node(node as Reload.NodePtr) as Frame ptr
declare function frame_reference(byval p as frame ptr) as frame ptr
declare sub frame_unload(byval p as frame ptr ptr)
declare sub frame_draw overload (byval src as frame ptr, byval pal as Palette16 ptr = NULL, byval x as integer, byval y as integer, byval scale as integer = 1, byval trans as bool = YES, byval page as integer)
declare sub frame_draw(byval src as Frame ptr, byval pal as Palette16 ptr = NULL, byval x as integer, byval y as integer, byval scale as integer = 1, byval trans as bool = YES, byval dest as Frame ptr)
declare function frame_dissolved(byval spr as frame ptr, byval tlength as integer, byval t as integer, byval style as integer) as frame ptr
declare function default_dissolve_time(byval style as integer, byval w as integer, byval h as integer) as integer
declare sub frame_flip_horiz(byval spr as frame ptr)
declare sub frame_flip_vert(byval spr as frame ptr)
declare function frame_rotated_90(byval spr as Frame ptr) as Frame ptr
declare function frame_rotated_270(byval spr as Frame ptr) as Frame ptr
declare function frame_duplicate(byval p as frame ptr, byval clr as integer = 0, byval addmask as integer = 0) as frame ptr
declare sub frame_clear(byval spr as frame ptr, byval colour as integer = 0)
declare sub frame_swap_colors(byval spr as Frame ptr, byval col1 as integer, byval col2 as integer)
declare sub sprite_empty_cache()
declare sub sprite_update_cache_pt(byval ptno as integer)
declare sub sprite_update_cache_tilesets()
declare sub tileset_empty_cache()
declare function frame_is_valid(byval p as frame ptr) as bool
declare sub sprite_debug_cache()
declare function frame_describe(byval p as frame ptr) as string

declare function palette16_new() as palette16 ptr
declare function palette16_new_from_buffer(pal() as integer, byval po as integer) as Palette16 ptr
declare function palette16_load overload (byval num as integer, byval autotype as integer = 0, byval spr as integer = 0) as palette16 ptr
declare function palette16_load(fil as string, byval num as integer, byval autotype as integer = 0, byval spr as integer = 0) as palette16 ptr
declare sub palette16_unload(byval p as palette16 ptr ptr)
declare sub palette16_empty_cache()
declare sub palette16_update_cache(fil as string, byval num as integer)

declare sub show_virtual_keyboard()
declare sub hide_virtual_keyboard()
declare sub show_virtual_gamepad()
declare sub hide_virtual_gamepad()

declare sub remap_android_gamepad(byval player as integer, gp as GamePadMap)
declare sub remap_touchscreen_button (byval button_id as integer, byval ohr_scancode as integer)

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

'globals
extern modex_initialised as bool
extern vpages() as Frame ptr
extern vpagesp as Frame ptr ptr
extern key2text(3,53) as string*1
extern disable_native_text_input as integer
extern fonts() as Font

#ENDIF
