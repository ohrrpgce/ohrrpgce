#ifndef YETMORE_BI
#define YETMORE_BI

DECLARE FUNCTION checksaveslot (slot as integer) as integer
DECLARE SUB erasesaveslot (slot as integer)

DECLARE SUB embedtext (text as string, byval limit as integer = 0)
DECLARE FUNCTION embed_text_codes (text_in as string, byval callback as ANY Ptr=0, byval arg0 as ANY ptr=0, byval arg1 as ANY ptr=0, byval arg2 as ANY ptr=0) as string

DECLARE SUB forceparty ()
DECLARE FUNCTION herobyrank (byval slot as integer) as integer
DECLARE FUNCTION rank_to_party_slot (byval rank as integer) as integer
DECLARE FUNCTION party_slot_to_rank (byval slot as integer) as integer
DECLARE FUNCTION rankincaterpillar (byval heroid as integer) as integer
DECLARE SUB interpolatecat

DECLARE SUB onkeyscript (byval scriptnum as integer)

DECLARE SUB process_wait_conditions ()
DECLARE SUB sfunctions (byval cmdid as integer)

DECLARE FUNCTION playtime (byval d as integer, byval h as integer, byval m as integer) as string
DECLARE SUB playtimer

DECLARE SUB update_vehicle_state ()
DECLARE FUNCTION vehpass (byval n as integer, byval tile as integer, byval default as integer) as integer
DECLARE SUB vehicle_graceful_dismount ()
DECLARE FUNCTION vehscramble(byval targx as integer, byval targy as integer) as bool

DECLARE SUB vishero ()
DECLARE SUB visnpc ()
DECLARE SUB set_walkabout_sprite (byval cont as Slice Ptr, byval pic as integer=-1, byval pal as integer=-2)
DECLARE SUB set_walkabout_frame (byval cont as Slice Ptr, byval direction as integer, byval frame as integer)
DECLARE SUB set_walkabout_vis (byval cont as Slice Ptr, byval vis as integer)
DECLARE SUB wrapaheadxy (byref x as integer, byref y as integer, byval direction as integer, byval distance as integer, byval unitsize as integer)
DECLARE SUB cropposition (byref x as integer, byref y as integer, byval unitsize as integer)
DECLARE FUNCTION wrappass (byval x as integer, byval y as integer, byref xgo as integer, byref ygo as integer, byval isveh as integer) as integer
DECLARE FUNCTION wrapzonecheck (byval zone as integer, byval x as integer, byval y as integer, byval xgo as integer, byval ygo as integer) as integer
DECLARE FUNCTION wrapcollision (byval xa as integer, byval ya as integer, byval xgoa as integer, byval ygoa as integer, byval xb as integer, byval yb as integer, byval xgob as integer, byval ygob as integer) as integer
DECLARE FUNCTION wraptouch (byval x1 as integer, byval y1 as integer, byval x2 as integer, byval y2 as integer, byval distance as integer) as integer
DECLARE SUB wrapxy (byref x as integer, byref y as integer, byval wide as integer, byval high as integer)

DECLARE SUB wrappedsong (byval songnumber as integer)
DECLARE SUB stopsong
DECLARE FUNCTION backcompat_sound_id (byval id as integer) as integer

DECLARE SUB loadsay (byval box_id as integer)

DECLARE FUNCTION getnpcref (byval seekid as integer, byval offset as integer) as integer
DECLARE FUNCTION get_valid_npc (byval seekid as integer, byval errlvl as scriptErrEnum = serrBadOp) as integer
DECLARE FUNCTION get_valid_npc_id (byval seekid as integer, byval errlvl as scriptErrEnum = serrBadOp) as integer

DECLARE FUNCTION valid_plotslice(byval handle as integer, byval errlev as scriptErrEnum = serrBadOp) as integer
DECLARE FUNCTION valid_plotsprite(byval handle as integer) as integer
DECLARE FUNCTION valid_plotrect(byval handle as integer) as integer
DECLARE FUNCTION valid_plottextslice(byval handle as integer) as integer
DECLARE FUNCTION valid_plotgridslice(byval handle as integer) as integer
DECLARE FUNCTION valid_plotselectslice(byval handle as integer) as integer
DECLARE FUNCTION valid_plotscrollslice(byval handle as integer) as integer
DECLARE FUNCTION valid_resizeable_slice(byval handle as integer, byval horiz_fill_ok as bool=NO, byval vert_fill_ok as bool=NO) as integer
DECLARE FUNCTION create_plotslice_handle(byval sl as Slice Ptr) as integer
DECLARE FUNCTION find_plotslice_handle(byval sl as Slice Ptr) as integer
DECLARE SUB set_plotslice_handle(byval sl as Slice Ptr, handle as integer)
DECLARE FUNCTION load_sprite_plotslice(byval spritetype as SpriteType, byval record as integer, byval pal as integer=-2) as integer
DECLARE SUB replace_sprite_plotslice(byval handle as integer, byval spritetype as SpriteType, byval record as integer, byval pal as integer=-2)
DECLARE SUB change_rect_plotslice(byval handle as integer, byval style as integer=-2, byval bgcol as integer=-99, byval fgcol as integer=-99, byval border as integer=-3, byval translucent as RectTransTypes=transUndef)
DECLARE FUNCTION valid_spriteslice_dat(byval sl as Slice Ptr) as integer

DECLARE FUNCTION find_menu_id (byval id as integer) as integer
DECLARE FUNCTION find_menu_handle (byval handle as integer) as integer
DECLARE FUNCTION find_menu_item_handle_in_menuslot (byval handle as integer, byval menuslot as integer) as integer
DECLARE FUNCTION find_menu_item_handle (byval handle as integer, byref found_in_menuslot as integer) as integer
DECLARE FUNCTION assign_menu_item_handle (byref mi as menudefitem) as integer
DECLARE FUNCTION assign_menu_handles (byref menu as menudef) as integer
DECLARE FUNCTION menu_item_handle_by_slot(byval menuslot as integer, byval mislot as integer, byval visible_only as integer=yes) as integer
DECLARE FUNCTION find_menu_item_slot_by_string(byval menuslot as integer, s as string, byval mislot as integer=0, byval visible_only as integer=yes) as integer

DECLARE FUNCTION valid_item_slot(byval item_slot as integer) as integer
DECLARE FUNCTION valid_item(byval itemid as integer) as integer
DECLARE FUNCTION valid_hero_party(byval who as integer, byval minimum as integer=0) as integer
DECLARE FUNCTION really_valid_hero_party(byval who as integer, byval maxslot as integer=40, byval errlvl as scriptErrEnum = serrBadOp) as integer
DECLARE FUNCTION valid_stat(byval statid as integer) as integer
DECLARE FUNCTION valid_menuslot(byval menuslot as integer) as integer
DECLARE FUNCTION valid_menuslot_and_mislot(byval menuslot as integer, byval mislot as integer) as integer
DECLARE FUNCTION valid_plotstr(byval n as integer, byval errlvl as scriptErrEnum = serrBound) as integer
DECLARE FUNCTION valid_formation(byval form as integer) as integer
DECLARE FUNCTION valid_formation_slot(byval form as integer, byval slot as integer) as integer
DECLARE FUNCTION valid_zone(byval id as integer) as integer
DECLARE FUNCTION valid_door(byval id as integer) as integer
DECLARE FUNCTION valid_map_layer(layer as integer, byval errlvl as scriptErrEnum = serrBadOp) as bool
DECLARE FUNCTION valid_tile_pos(byval x as integer, byval y as integer) as integer

DECLARE SUB greyscalepal
DECLARE SUB tweakpalette (byval r as integer, byval g as integer, byval b as integer, byval first as integer = 0, byval last as integer = 255)
DECLARE SUB write_checkpoint ()

#endif
