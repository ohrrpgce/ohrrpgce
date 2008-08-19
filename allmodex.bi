'Allmodex FreeBasic Library header

#IFNDEF ALLMODEX_BI
#DEFINE ALLMODEX_BI

#include "udts.bi"
#include "compat.bi"

'Library routines
DECLARE SUB setmodex ()
DECLARE SUB restoremode ()
DECLARE FUNCTION allocatepage() as integer
DECLARE SUB freepage (BYVAL page as integer)
DECLARE SUB copypage (BYVAL page1, BYVAL page2, BYVAL y = 0, BYVAL top = 0, BYVAL bottom = 199)
DECLARE SUB clearpage (BYVAL page, BYVAL top = 0, BYVAL bottom = 199, BYVAL colour = 0)
DECLARE SUB setvispage (BYVAL page)
DECLARE SUB setpal (pal() as RGBcolor)
DECLARE SUB fadeto (BYVAL red, BYVAL green, BYVAL blue)
DECLARE SUB fadetopal (pal() as RGBcolor)
DECLARE SUB loadtileset (BYREF tileset as Frame ptr, BYVAL page)
DECLARE SUB unloadtileset (BYREF tileset as Frame ptr)
DECLARE SUB setmapdata (array(), pas(), BYVAL t, BYVAL b)
DECLARE SUB setmapblock (BYVAL x, BYVAL y, byval l, BYVAL v)
DECLARE FUNCTION readmapblock (BYVAL x, BYVAL y, byval l)
DECLARE SUB setpassblock (BYVAL x, BYVAL y, BYVAL v)
DECLARE FUNCTION readpassblock (BYVAL x, BYVAL y)
DECLARE SUB drawmap overload (BYVAL x, BYVAL y, BYVAL l, BYVAL t, BYVAL tileset as TilesetData ptr, BYVAL p, byval trans as integer = 0)
DECLARE SUB drawmap (BYVAL x, BYVAL y, BYVAL l, BYVAL t, BYVAL tilesetsprite as Frame ptr, BYVAL p, byval trans as integer = 0)
DECLARE SUB setanim (BYVAL cycle1, BYVAL cycle2)
DECLARE SUB setoutside (BYVAL defaulttile)
DECLARE SUB drawsprite (pic(), BYVAL picoff, pal(), BYVAL po, BYVAL x, BYVAL y, BYVAL page, BYVAL trans = -1)
DECLARE SUB wardsprite (pic(), BYVAL picoff, pal(), BYVAL po, BYVAL x, BYVAL y, BYVAL page, BYVAL trans = -1)
DECLARE SUB getsprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL page)
DECLARE SUB stosprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB loadsprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL page)
DECLARE SUB bigsprite (pic(), pal(), BYVAL p, BYVAL x, BYVAL y, BYVAL page, BYVAL trans = -1)
DECLARE SUB hugesprite (pic(), pal(), BYVAL p, BYVAL x, BYVAL y, BYVAL page, BYVAL trans = -1)
DECLARE SUB putpixel (BYVAL x, BYVAL y, BYVAL c, BYVAL p)
DECLARE FUNCTION readpixel (BYVAL x, BYVAL y, BYVAL p)
DECLARE SUB rectangle (BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL c, BYVAL p)
DECLARE SUB fuzzyrect (BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL c, BYVAL p)
DECLARE SUB drawline (BYVAL x1, BYVAL y1, BYVAL x2, BYVAL y2, BYVAL c, BYVAL p)
DECLARE SUB paintat (BYVAL x, BYVAL y, BYVAL c, BYVAL page, buf(), BYVAL max)
DECLARE SUB storepage (fil$, BYVAL i, BYVAL p)
DECLARE SUB loadpage (fil$, BYVAL i, BYVAL p)
DECLARE SUB setwait (BYVAL t, BYVAL flagt = 0)
DECLARE FUNCTION dowait () as integer
DECLARE SUB printstr (s$, BYVAL x, BYVAL y, BYVAL p)
DECLARE SUB textcolor (BYVAL f, BYVAL b)
DECLARE SUB setfont (f())
DECLARE SUB setbit (b(), BYVAL w, BYVAL b, BYVAL v)
DECLARE FUNCTION readbit (b(), BYVAL w, BYVAL b)
DECLARE SUB storeset (fil$, BYVAL i, BYVAL l)
DECLARE SUB loadset (fil$, BYVAL i, BYVAL l)
DECLARE SUB setpicstuf (buf(), BYVAL b, BYVAL p)
DECLARE FUNCTION loadrecord overload (buf(), fh, recordsize, record = -1)
DECLARE FUNCTION loadrecord overload (buf(), filename$, recordsize, record = 0)
DECLARE SUB storerecord overload (buf(), fh, recordsize, record = -1)
DECLARE SUB storerecord overload (buf(), filename$, recordsize, record = 0)
DECLARE SUB fixspriterecord (buf(), w, h)
DECLARE SUB bitmap2page (pal() as RGBcolor, bmp$, BYVAL p)
DECLARE SUB findfiles overload(fmask$, BYVAL attrib, outfile$, buf())
DECLARE SUB findfiles (fmask$, BYVAL attrib, outfile$)
DECLARE SUB lumpfiles (listf$, lump$, path$)
DECLARE SUB unlump(lump$, ulpath$)
DECLARE SUB unlumpfile(lump$, fmask$, path$)
DECLARE FUNCTION islumpfile (lump$, fmask$)
DECLARE FUNCTION isfile (n$)
DECLARE FUNCTION isdir (sDir$)
DECLARE FUNCTION drivelist (d$())
DECLARE FUNCTION drivelabel$ (drive$)
DECLARE FUNCTION isremovable (drive$)
DECLARE FUNCTION hasmedia (drive$)
DECLARE SUB setupmusic
DECLARE SUB closemusic ()
DECLARE SUB loadsong (f$)
DECLARE SUB pausesong ()
DECLARE SUB resumesong ()
DECLARE SUB fademusic (BYVAL vol)
DECLARE FUNCTION getfmvol ()
DECLARE SUB setfmvol (BYVAL vol)
DECLARE SUB copyfile (s$, d$)
DECLARE SUB screenshot (f$, BYVAL p, maspal() as RGBcolor)
DECLARE SUB loadbmp (f$, BYVAL x, BYVAL y, BYVAL p)
DECLARE SUB bitmap2pal (bmp$, pal() as RGBcolor)
DECLARE FUNCTION loadbmppal (f$, pal() as RGBcolor)
DECLARE SUB convertbmppal (f$, mpal() as RGBcolor, pal(), BYVAL o)
DECLARE FUNCTION nearcolor(pal() as RGBcolor, byval red as ubyte, byval green as ubyte, byval blue as ubyte) as ubyte
DECLARE FUNCTION bmpinfo (f$, dat())
DECLARE SUB array2str (arr(), BYVAL o, s$)
DECLARE SUB str2array (s$, arr(), BYVAL o)
DECLARE SUB setupstack ()
DECLARE SUB pushw (BYVAL word)
DECLARE FUNCTION popw ()
DECLARE SUB pushdw (BYVAL word)
DECLARE FUNCTION popdw ()
DECLARE SUB releasestack ()
DECLARE FUNCTION stackpos ()
DECLARE FUNCTION readstackdw (BYVAL off)
DECLARE SUB drawbox(BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL c, BYVAL p)
DECLARE FUNCTION isawav(fi$)
DECLARE FUNCTION fileisreadable(f$)
DECLARE FUNCTION fileiswriteable(f$)

DECLARE FUNCTION keyval (BYVAL a, BYVAL rwait = 0, BYVAL rrate = 0) as integer
DECLARE FUNCTION getkey ()
DECLARE FUNCTION waitforanykey (modkeys = -1) as integer
DECLARE SUB setkeyrepeat (rwait as integer = 8, rrate as integer = 1)
DECLARE SUB setkeys ()
DECLARE SUB clearkey (byval k as integer)
DECLARE FUNCTION setmouse (mbuf())
DECLARE SUB readmouse (mbuf())
DECLARE SUB movemouse (BYVAL x, BYVAL y)
DECLARE SUB mouserect (BYVAL xmin, BYVAL xmax, BYVAL ymin, BYVAL ymax)
DECLARE FUNCTION readjoy (joybuf(), BYVAL jnum)
#DEFINE slowkey(key, fraction) (keyval((key), (fraction), (fraction)) > 1)

DECLARE SUB playsfx (BYVAL num, BYVAL l=0) 'l is loop count. -1 for infinite loop
DECLARE SUB stopsfx (BYVAL num)
DECLARE SUB pausesfx (BYVAL num)
DECLARE SUB freesfx (BYVAL num) ' only used by custom's importing interface
DECLARE FUNCTION sfxisplaying (BYVAL num)
DECLARE FUNCTION getmusictype (file$)
'DECLARE SUB getsfxvol (BYVAL num)
'DECLARE SUB setsfxvol (BYVAL num, BYVAL vol)

'DECLARE FUNCTION getsoundvol ()
'DECLARE SUB setsoundvol (BYVAL vol)

'new sprite functions
declare function sprite_load(byval as string, byval as integer, byval as integer , byval as integer, byval as integer) as frame ptr
declare sub sprite_unload(byval p as frame ptr ptr)
declare sub sprite_draw(byval spr as frame ptr, Byval pal as Palette16 ptr, Byval x as integer, Byval y as integer, Byval scale as integer = 1, Byval trans as integer = -1, byval page as integer)
declare function sprite_dissolve(byval spr as frame ptr, byval tim as integer, byval p as integer, byval style as integer = 0, byval direct as integer = 0) as frame ptr
declare function sprite_flip_horiz(byval spr as frame ptr, byval direct as integer = 0) as frame ptr
declare function sprite_flip_vert(byval spr as frame ptr, byval direct as integer = 0) as frame ptr
declare function sprite_duplicate(byval p as frame ptr, byval clr as integer = 0) as frame ptr
declare sub sprite_clear(byval spr as frame ptr)
declare sub sprite_empty_cache()
declare function sprite_is_valid(byval p as frame ptr) as integer
declare sub sprite_crash_invalid(byval p as frame ptr)

declare function palette16_load(byval fil as string, byval num as integer, byval autotype as integer = 0, byval spr as integer = 0) as palette16 ptr
declare sub palette16_unload(byval p as palette16 ptr ptr)
declare sub palette16_empty_cache()

#ENDIF
