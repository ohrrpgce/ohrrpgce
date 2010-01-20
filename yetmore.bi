#ifndef YETMORE_BI
#define YETMORE_BI

DECLARE SUB add_rem_swap_lock_hero (box AS TextBox, stat() as integer)
DECLARE FUNCTION checksaveslot (slot as integer) as integer
DECLARE SUB erasesaveslot (slot as integer)
DECLARE SUB doihavebits
DECLARE SUB embedtext (text as string, limit as integer=0)
DECLARE SUB scriptstat (id as integer, stat() as integer)
DECLARE SUB forceparty (stat() as integer)
DECLARE FUNCTION gethighbyte (n as integer) as integer
DECLARE FUNCTION getnpcref (seekid as integer, offset as integer) as integer
DECLARE SUB greyscalepal
DECLARE FUNCTION herobyrank (slot as integer) as integer
DECLARE SUB initgame
DECLARE SUB interpolatecat
DECLARE SUB npcplot
DECLARE SUB onkeyscript (scriptnum as integer)
DECLARE FUNCTION partybyrank (slot as integer) as integer
DECLARE FUNCTION playtime (d as integer, h as integer, m as integer) as string
DECLARE SUB playtimer
DECLARE FUNCTION rankincaterpillar (heroid as integer) as integer
DECLARE FUNCTION readfoemap (x as integer, y as integer, fh as integer) as integer
DECLARE SUB scriptadvanced (id as integer)
DECLARE SUB scriptmisc (id as integer)
DECLARE SUB scriptnpc (id as integer)
DECLARE SUB setdebugpan
DECLARE SUB templockexplain
DECLARE SUB tweakpalette
DECLARE FUNCTION vehiclestuff () as integer
DECLARE FUNCTION vehpass (n as integer, tile as integer, default as integer) as integer
DECLARE SUB vishero (stat() as integer)
DECLARE SUB wrapaheadxy (x as integer, y as integer, direction as integer, distance as integer, unitsize as integer)
DECLARE SUB cropposition (BYREF x as integer, BYREF y as integer, unitsize as integer)
DECLARE FUNCTION wrappass (x as integer, y as integer, xgo as integer, ygo as integer, isveh as integer) as integer
DECLARE FUNCTION wrapcollision (xa as integer, ya as integer, xgoa as integer, ygoa as integer, xb as integer, yb as integer, xgob as integer, ygob as integer) as integer
DECLARE FUNCTION wraptouch (x1 as integer, y1 as integer, x2 as integer, y2 as integer, distance as integer) as integer
DECLARE SUB wrappedsong (songnumber as integer)
DECLARE SUB stopsong
DECLARE SUB wrapxy (x as integer, y as integer, wide as integer, high as integer)
DECLARE FUNCTION backcompat_sound_id (id AS INTEGER) as integer
DECLARE SUB loadsay (box_id as integer)
DECLARE SUB load_text_box_portrait (BYREF box AS TextBox, BYREF gfx AS GraphicPair)
DECLARE FUNCTION valid_plotslice(byval handle as integer, errlev as integer=5) as integer
DECLARE FUNCTION valid_plotsprite(byval handle as integer) as integer
DECLARE FUNCTION valid_plotrect(byval handle as integer) as integer
DECLARE FUNCTION valid_plottextslice(byval handle as integer) as integer
DECLARE FUNCTION valid_resizeable_slice(byval handle as integer, byval ignore_fill as integer=NO) as integer
DECLARE FUNCTION create_plotslice_handle(byval sl as Slice Ptr) AS INTEGER
DECLARE FUNCTION find_plotslice_handle(BYVAL sl AS Slice Ptr) AS INTEGER
DECLARE FUNCTION load_sprite_plotslice(BYVAL spritetype AS INTEGER, BYVAL record AS INTEGER, BYVAL pal AS INTEGER=-1) AS INTEGER
DECLARE SUB change_sprite_plotslice(BYVAL handle AS INTEGER, BYVAL spritetype AS INTEGER, BYVAL record AS INTEGER, BYVAL pal AS INTEGER=-1, BYVAL frame AS INTEGER=-1, BYVAL fliph AS INTEGER=-2, BYVAL flipv AS INTEGER=-2)
DECLARE SUB change_rect_plotslice(BYVAL handle AS INTEGER, BYVAL style AS INTEGER=-2, BYVAL bgcol AS INTEGER=-1, BYVAL fgcol AS INTEGER=-1, BYVAL border AS INTEGER=-3, BYVAL translucent AS RectTransTypes=transUndef)
DECLARE FUNCTION valid_spriteslice_dat(BYVAL sl AS Slice Ptr) AS INTEGER
DECLARE SUB vehscramble(BYREF mode_val AS INTEGER, BYVAL trigger_cleanup AS INTEGER, BYVAL targx AS INTEGER, BYVAL targy AS INTEGER, BYREF result AS INTEGER)

#endif
