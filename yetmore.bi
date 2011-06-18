#ifndef YETMORE_BI
#define YETMORE_BI

DECLARE SUB add_rem_swap_lock_hero (box AS TextBox)
DECLARE FUNCTION checksaveslot (slot as integer) as integer
DECLARE SUB erasesaveslot (slot as integer)
DECLARE SUB doihavebits
DECLARE SUB embedtext (text as string, limit as integer=0)
DECLARE SUB scriptstat (id as integer)
DECLARE SUB forceparty ()
DECLARE FUNCTION gethighbyte (n as integer) as integer
DECLARE FUNCTION getnpcref (seekid as integer, offset as integer) as integer
DECLARE FUNCTION get_valid_npc (BYVAL seekid as integer, BYVAL errlvl as integer = 5) as integer
DECLARE FUNCTION get_valid_npc_id (BYVAL seekid as integer, BYVAL errlvl as integer = 5) as integer
DECLARE SUB greyscalepal
DECLARE FUNCTION herobyrank (slot as integer) as integer
DECLARE SUB interpolatecat
DECLARE SUB onkeyscript (scriptnum as integer)
DECLARE FUNCTION partybyrank (slot as integer) as integer
DECLARE FUNCTION playtime (d as integer, h as integer, m as integer) as string
DECLARE SUB playtimer
DECLARE FUNCTION rankincaterpillar (heroid as integer) as integer
DECLARE FUNCTION readfoemap (x as integer, y as integer, fh as integer) as integer
DECLARE SUB scriptadvanced (id as integer)
DECLARE SUB scriptmisc (byval id as integer)
DECLARE SUB scriptnpc (byval id as integer)
DECLARE SUB setdebugpan
DECLARE SUB tweakpalette (byval r as integer, byval g as integer, byval b as integer, byval first as integer = 0, byval last as integer = 255)
DECLARE SUB update_vehicle_state ()
DECLARE FUNCTION vehpass (byval n as integer, byval tile as integer, byval default as integer) as integer
DECLARE SUB vehicle_graceful_dismount ()
DECLARE SUB vishero ()
DECLARE SUB visnpc ()
DECLARE SUB set_walkabout_sprite (byval cont as Slice Ptr, byval pic as integer=-1, byval pal as integer=-2)
DECLARE SUB set_walkabout_frame (byval cont as Slice Ptr, byval frame as integer)
DECLARE SUB set_walkabout_vis (byval cont as Slice Ptr, byval vis as integer)
DECLARE SUB wrapaheadxy (byref x as integer, byref y as integer, byval direction as integer, byval distance as integer, byval unitsize as integer)
DECLARE SUB cropposition (byref x as integer, byref y as integer, byval unitsize as integer)
DECLARE FUNCTION wrappass (byval x as integer, byval y as integer, byref xgo as integer, byref ygo as integer, byval isveh as integer) as integer
DECLARE FUNCTION wrapzonetest (BYVAL zone as integer, BYVAL x as integer, BYVAL y as integer, BYVAL xgo as integer, BYVAL ygo as integer) as integer
DECLARE FUNCTION wrapcollision (byval xa as integer, byval ya as integer, byval xgoa as integer, byval ygoa as integer, byval xb as integer, byval yb as integer, byval xgob as integer, byval ygob as integer) as integer
DECLARE FUNCTION wraptouch (byval x1 as integer, byval y1 as integer, byval x2 as integer, byval y2 as integer, byval distance as integer) as integer
DECLARE SUB wrappedsong (BYVAL songnumber as integer)
DECLARE SUB stopsong
DECLARE SUB wrapxy (BYREF x AS INTEGER, BYREF y AS INTEGER, BYVAL wide AS INTEGER, BYVAL high AS INTEGER)
DECLARE FUNCTION backcompat_sound_id (BYVAL id AS INTEGER) as integer
DECLARE SUB loadsay (byval box_id as integer)
DECLARE SUB load_text_box_portrait (BYREF box AS TextBox, BYREF gfx AS GraphicPair)
DECLARE FUNCTION valid_plotslice(byval handle as integer, errlev as integer=5) as integer
DECLARE FUNCTION valid_plotsprite(byval handle as integer) as integer
DECLARE FUNCTION valid_plotrect(byval handle as integer) as integer
DECLARE FUNCTION valid_plottextslice(byval handle as integer) as integer
DECLARE FUNCTION valid_plotgridslice(byval handle as integer) as integer
DECLARE FUNCTION valid_resizeable_slice(byval handle as integer, byval ignore_fill as integer=NO) as integer
DECLARE FUNCTION create_plotslice_handle(byval sl as Slice Ptr) AS INTEGER
DECLARE FUNCTION find_plotslice_handle(BYVAL sl AS Slice Ptr) AS INTEGER
DECLARE FUNCTION load_sprite_plotslice(BYVAL spritetype AS INTEGER, BYVAL record AS INTEGER, BYVAL pal AS INTEGER=-2) AS INTEGER
DECLARE SUB replace_sprite_plotslice(BYVAL handle AS INTEGER, BYVAL spritetype AS INTEGER, BYVAL record AS INTEGER, BYVAL pal AS INTEGER=-2)
DECLARE SUB change_rect_plotslice(BYVAL handle AS INTEGER, BYVAL style AS INTEGER=-2, BYVAL bgcol AS INTEGER=-1, BYVAL fgcol AS INTEGER=-1, BYVAL border AS INTEGER=-3, BYVAL translucent AS RectTransTypes=transUndef)
DECLARE FUNCTION valid_spriteslice_dat(BYVAL sl AS Slice Ptr) AS INTEGER
DECLARE SUB vehscramble(BYREF mode_val AS INTEGER, BYVAL trigger_cleanup AS INTEGER, BYVAL targx AS INTEGER, BYVAL targy AS INTEGER)

#endif
