'Allmodex FreeBasic Library header

#IFNDEF ALLMODEX_BI
#DEFINE ALLMODEX_BI

#include "udts.bi"
#include "compat.bi"
#IFNDEF BITMAP
 'windows.bi may have been included
 #include "bitmap.bi"
#ENDIF
#include "file.bi"   'FB header
#include "lumpfile.bi"

'Library routines
DECLARE SUB setmodex ()
DECLARE SUB restoremode ()
DECLARE SUB setwindowtitle (title as string)
DECLARE FUNCTION allocatepage() as integer
DECLARE SUB freepage (BYVAL page as integer)
DECLARE FUNCTION registerpage (BYVAL spr as Frame ptr) as integer
DECLARE SUB copypage (BYVAL page1 as integer, BYVAL page2 as integer, BYVAL y as integer = 0, BYVAL top as integer = 0, BYVAL bottom as integer = 199)
DECLARE SUB clearpage (BYVAL page as integer, BYVAL colour as integer = -1, BYVAL top as integer = 0, BYVAL bottom as integer = 199)
DECLARE SUB setvispage (BYVAL page as integer)
DECLARE SUB setpal (pal() as RGBcolor)
DECLARE SUB fadeto (BYVAL red as integer, BYVAL green as integer, BYVAL blue as integer)
DECLARE SUB fadetopal (pal() as RGBcolor)
DECLARE FUNCTION sprite_to_tileset(BYVAL spr as frame ptr) as frame ptr
DECLARE FUNCTION tileset_load(BYVAL num as integer) as Frame ptr
DECLARE SUB setmapdata (array() as integer, pas() as integer, BYVAL t as integer, BYVAL b as integer)
DECLARE SUB setmapblock (BYVAL x as integer, BYVAL y as integer, byval l as integer, BYVAL v as integer)
DECLARE FUNCTION readmapblock (BYVAL x as integer, BYVAL y as integer, byval l as integer) as integer
DECLARE SUB setpassblock (BYVAL x as integer, BYVAL y as integer, BYVAL v as integer)
DECLARE FUNCTION readpassblock (BYVAL x as integer, BYVAL y as integer) as integer
DECLARE SUB setclip (BYVAL l as integer = 0, BYVAL t as integer = 0, BYVAL r as integer = 9999, BYVAL b as integer = 9999, BYVAL page as integer = -1)
DECLARE SUB drawmap overload (BYVAL x as integer, BYVAL y as integer, BYVAL l as integer, BYVAL t as integer, BYVAL tileset as TilesetData ptr, BYVAL p as integer, byval trans as integer = 0)
DECLARE SUB drawmap (BYVAL x as integer, BYVAL y as integer, BYVAL l as integer, BYVAL t as integer, BYVAL tilesetsprite as Frame ptr, BYVAL p as integer, byval trans as integer = 0)
DECLARE SUB setanim (BYVAL cycle1 as integer, BYVAL cycle2 as integer)
DECLARE SUB setoutside (BYVAL defaulttile as integer)
DECLARE SUB drawspritex (pic() as integer, BYVAL picoff as integer, pal() as integer, BYVAL po as integer, BYVAL x as integer, BYVAL y as integer, BYVAL page as integer, byval scale as integer=1, BYVAL trans as integer = -1)
DECLARE SUB drawsprite (pic() as integer, BYVAL picoff as integer, pal() as integer, BYVAL po as integer, BYVAL x as integer, BYVAL y as integer, BYVAL page as integer, BYVAL trans as integer = -1)
DECLARE SUB wardsprite (pic() as integer, BYVAL picoff as integer, pal() as integer, BYVAL po as integer, BYVAL x as integer, BYVAL y as integer, BYVAL page as integer, BYVAL trans as integer = -1)
DECLARE SUB getsprite (pic() as integer, BYVAL picoff as integer, BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL page as integer)
DECLARE SUB stosprite (pic() as integer, BYVAL picoff as integer, BYVAL x as integer, BYVAL y as integer, BYVAL page as integer)
DECLARE SUB loadsprite (pic() as integer, BYVAL picoff as integer, BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL page as integer)
DECLARE SUB bigsprite  (pic() as integer, pal() as integer, BYVAL p as integer, BYVAL x as integer, BYVAL y as integer, BYVAL page as integer, BYVAL trans as integer = -1)
DECLARE SUB hugesprite (pic() as integer, pal() as integer, BYVAL p as integer, BYVAL x as integer, BYVAL y as integer, BYVAL page as integer, BYVAL trans as integer = -1)
DECLARE SUB putpixel OVERLOAD (BYVAL spr as Frame ptr, BYVAL x as integer, BYVAL y as integer, BYVAL c as integer)
DECLARE SUB putpixel (BYVAL x as integer, BYVAL y as integer, BYVAL c as integer, BYVAL p as integer)
DECLARE FUNCTION readpixel OVERLOAD (BYVAL spr as Frame ptr, BYVAL x as integer, BYVAL y as integer) as integer
DECLARE FUNCTION readpixel (BYVAL x as integer, BYVAL y as integer, BYVAL p as integer) as integer
DECLARE SUB rectangle (BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL c as integer, BYVAL p as integer)
DECLARE SUB fuzzyrect (BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL c as integer, BYVAL p as integer)
DECLARE SUB drawline (BYVAL x1 as integer, BYVAL y1 as integer, BYVAL x2 as integer, BYVAL y2 as integer, BYVAL c as integer, BYVAL p as integer)
DECLARE SUB paintat (BYVAL x as integer, BYVAL y as integer, BYVAL c as integer, BYVAL page as integer)
DECLARE SUB storemxs (fil as string, BYVAL record as integer, BYVAL fr as Frame ptr)
DECLARE FUNCTION loadmxs (fil as string, BYVAL record as integer, BYVAL dest as Frame ptr = 0) as Frame ptr
DECLARE SUB setwait (BYVAL t as integer, BYVAL flagt as integer = 0)
DECLARE FUNCTION dowait () as integer
DECLARE SUB printstr OVERLOAD (s as string, BYVAL startx as integer, BYVAL y as integer, BYREF f as Font, BYREF pal as Palette16, BYVAL p as integer)
DECLARE SUB printstr (s as string, BYVAL x as integer, BYVAL y as integer, BYVAL p as integer)
DECLARE SUB edgeprint (s as string, BYVAL x as integer, BYVAL y as integer, BYVAL c as integer, BYVAL p as integer)
DECLARE SUB textcolor (BYVAL f as integer, BYVAL b as integer)
DECLARE SUB setfont (f() as integer)
DECLARE SUB setbit (b() as integer, BYVAL w as integer, BYVAL b as integer, BYVAL v as integer)
DECLARE FUNCTION readbit (b() as integer, BYVAL w as integer, BYVAL b as integer) as integer
DECLARE SUB storeset (fil as string, BYVAL i as integer, BYVAL l as integer)
DECLARE SUB loadset (fil as string, BYVAL i as integer, BYVAL l as integer)
DECLARE SUB setpicstuf (buf() as integer, BYVAL b as integer, BYVAL p as integer)
DECLARE SUB fixspriterecord (buf() as integer, w as integer, h as integer)
DECLARE SUB findfiles (fmask as string, BYVAL attrib as integer, outfile as string)
DECLARE FUNCTION isfile (n as string) as integer
DECLARE FUNCTION isdir (sDir as string) as integer
DECLARE FUNCTION is_absolute_path (sDir as string) as integer
DECLARE FUNCTION drivelist (d() as string) as integer
DECLARE FUNCTION drivelabel (drive as string) as string
DECLARE FUNCTION isremovable (drive as string) as integer
DECLARE FUNCTION hasmedia (drive as string) as integer
DECLARE SUB setupmusic
DECLARE SUB closemusic ()
DECLARE SUB loadsong (f as string)
DECLARE SUB pausesong ()
DECLARE SUB resumesong ()
DECLARE FUNCTION getfmvol () as integer
DECLARE SUB setfmvol (BYVAL vol as integer)
DECLARE SUB screenshot (f as string)
DECLARE SUB sprite_export_bmp4 (f$, byval fr as Frame Ptr, maspal() as RGBcolor, byval pal as Palette16 ptr)
DECLARE SUB sprite_export_bmp8 (f$, byval fr as Frame Ptr, maspal() as RGBcolor)
DECLARE FUNCTION sprite_import_bmp24(bmp as string, pal() as RGBcolor) as Frame ptr
DECLARE FUNCTION sprite_import_bmp_raw(bmp as string) as Frame ptr
DECLARE SUB bitmap2pal (bmp as string, pal() as RGBcolor)
DECLARE FUNCTION loadbmppal (f as string, pal() as RGBcolor) as integer
DECLARE SUB convertbmppal (f as string, mpal() as RGBcolor, pal() as integer, BYVAL o as integer)
DECLARE FUNCTION nearcolor(pal() as RGBcolor, byval red as ubyte, byval green as ubyte, byval blue as ubyte) as ubyte
DECLARE FUNCTION bmpinfo (f as string, byref dat as BitmapInfoHeader) as integer
DECLARE SUB array2str (arr() as integer, BYVAL o as integer, s as string)
DECLARE SUB str2array (s as string, arr() as integer, BYVAL o as integer)
DECLARE SUB setupstack ()
DECLARE SUB pushw (BYVAL word as integer)
DECLARE FUNCTION popw () as integer
DECLARE SUB pushdw (BYVAL word as integer)
DECLARE FUNCTION popdw () as integer
DECLARE SUB releasestack ()
DECLARE FUNCTION stackpos () as integer
DECLARE FUNCTION readstackdw (BYVAL off as integer) as integer
DECLARE SUB drawbox(BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL c as integer, BYVAL p as integer)
DECLARE FUNCTION isawav(fi as string) as integer
DECLARE FUNCTION fileisreadable(f as string) as integer
DECLARE FUNCTION fileiswriteable(f as string) as integer
DECLARE FUNCTION diriswriteable(d as string) as integer

DECLARE FUNCTION keyval (BYVAL a as integer, BYVAL rwait as integer = 0, BYVAL rrate as integer = 0) as integer
DECLARE FUNCTION getkey () as integer
DECLARE FUNCTION waitforanykey (modkeys as integer = -1) as integer
DECLARE SUB setkeyrepeat (rwait as integer = 8, rrate as integer = 1)
DECLARE SUB setkeys ()
DECLARE SUB clearkey (byval k as integer)
DECLARE FUNCTION havemouse () as integer
DECLARE SUB hidemousecursor ()
DECLARE SUB unhidemousecursor ()
DECLARE SUB readmouse (mbuf() as integer)
DECLARE SUB movemouse (BYVAL x as integer, BYVAL y as integer)
DECLARE SUB mouserect (BYVAL xmin as integer, BYVAL xmax as integer, BYVAL ymin as integer, BYVAL ymax as integer)
DECLARE FUNCTION readjoy OVERLOAD (joybuf() as integer, BYVAL jnum as integer) as integer
DECLARE FUNCTION readjoy (BYVAL joynum as integer, BYREF buttons as integer, BYREF x as integer, BYREF y as integer) as integer
#DEFINE slowkey(key, fraction) (keyval((key), (fraction), (fraction)) > 1)

DECLARE SUB resetsfx ()
DECLARE SUB playsfx (BYVAL num as integer, BYVAL l as integer=0) 'l is loop count. -1 for infinite loop
DECLARE SUB stopsfx (BYVAL num as integer)
DECLARE SUB pausesfx (BYVAL num as integer)
DECLARE SUB freesfx (BYVAL num as integer) ' only used by custom's importing interface
DECLARE FUNCTION sfxisplaying (BYVAL num as integer) as integer
DECLARE FUNCTION getmusictype (file as string) as integer
'DECLARE SUB getsfxvol (BYVAL num as integer)
'DECLARE SUB setsfxvol (BYVAL num as integer, BYVAL vol as integer)

'DECLARE FUNCTION getsoundvol () as integer
'DECLARE SUB setsoundvol (BYVAL vol)

'new sprite functions
declare function sprite_new(byval w as integer, byval h as integer, byval frames as integer = 1, byval clr as integer = NO, byval wantmask as integer = NO) as Frame ptr
declare function sprite_new_view(byval spr as Frame ptr, byval x as integer, byval y as integer, byval w as integer, byval h as integer) as Frame ptr
declare function sprite_load overload (byval ptno as integer, byval rec as integer) as frame ptr
declare function sprite_load(byval as string, byval as integer, byval as integer , byval as integer, byval as integer) as frame ptr
declare function sprite_reference(byval p as frame ptr) as frame ptr
declare sub sprite_unload(byval p as frame ptr ptr)
declare sub sprite_draw(byval spr as frame ptr, Byval pal as Palette16 ptr, Byval x as integer, Byval y as integer, Byval scale as integer = 1, Byval trans as integer = -1, byval page as integer)
declare function sprite_dissolved(byval spr as frame ptr, byval tlength as integer, byval t as integer, byval style as integer) as frame ptr
declare sub sprite_flip_horiz(byval spr as frame ptr)
declare sub sprite_flip_vert(byval spr as frame ptr)
declare function sprite_duplicate(byval p as frame ptr, byval clr as integer = 0, byval addmask as integer = 0) as frame ptr
declare sub sprite_clear(byval spr as frame ptr)
declare sub sprite_empty_cache()
declare function sprite_is_valid(byval p as frame ptr) as integer
declare sub sprite_debug_cache()
declare function sprite_describe(byval p as frame ptr) as string

declare function palette16_new() as palette16 ptr
declare function palette16_load overload (byval num as integer, byval autotype as integer = 0, byval spr as integer = 0) as palette16 ptr
declare function palette16_load(byval fil as string, byval num as integer, byval autotype as integer = 0, byval spr as integer = 0) as palette16 ptr
declare sub palette16_unload(byval p as palette16 ptr ptr)
declare sub palette16_empty_cache()
declare sub palette16_update_cache(fil as string, byval num as integer)

'globals
extern vpages(0 to 15) as Frame ptr

#ENDIF
