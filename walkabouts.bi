#ifndef WALKABOUTS_BI
#define WALKABOUTS_BI

#include "slices.bi"

DECLARE SUB set_walkabout_sprite (byval cont as Slice Ptr, byval pic as integer=-1, byval pal as integer=-2)
DECLARE SUB set_walkabout_frame (byval cont as Slice Ptr, byval direction as DirNum, byval frame as integer)
DECLARE SUB set_walkabout_vis (byval cont as Slice Ptr, byval vis as bool)
DECLARE FUNCTION create_walkabout_slices(byval parent as Slice Ptr) as Slice Ptr
DECLARE SUB create_walkabout_shadow (byval walkabout_cont as Slice Ptr)
DECLARE SUB delete_walkabout_shadow (byval walkabout_cont as Slice Ptr)
DECLARE SUB reset_npc_graphics ()
DECLARE SUB change_npc_def_sprite (byval npc_id as NPCTypeID, byval walkabout_sprite_id as integer)
DECLARE SUB change_npc_def_pal (byval npc_id as NPCTypeID, byval palette_id as integer)
DECLARE SUB vishero ()
DECLARE SUB visnpc ()
DECLARE SUB update_walkabout_slices()
DECLARE SUB update_walkabout_hero_slices()
DECLARE SUB update_walkabout_npc_slices()
DECLARE SUB update_walkabout_pos (byval walkabout_cont as slice ptr, byval x as integer, byval y as integer, byval z as integer)

DECLARE SUB reparent_hero_slices()
DECLARE SUB orphan_hero_slices()
DECLARE SUB reparent_npc_slices()
DECLARE SUB orphan_npc_slices()
DECLARE FUNCTION hero_layer() as Slice Ptr
DECLARE FUNCTION npc_layer() as Slice Ptr

DECLARE FUNCTION npc_at_spot(tilepos as XYPair, byval copynum as integer=0) as NPCIndex
DECLARE FUNCTION count_npcs_at_spot(tilepos as XYPair) as integer
DECLARE FUNCTION npc_at_pixel(pixelpos as XYPair, byval copynum as integer=0, allow_disabled as bool=NO) as NPCIndex
DECLARE FUNCTION hero_at_pixel(pixelpos as XYPair) as integer

'Movement/collision/wrapping
DECLARE FUNCTION movdivis (byval xygo as integer) as bool
DECLARE FUNCTION cropmovement (byref pos as XYPair, byref xygo as XYPair) as bool
DECLARE FUNCTION framewalkabout (byval mappos as XYPair, byref screenpos as XYPair, byval mapsize as XYPair, wrapmode as MapEdgeModeEnum, margin as integer = -1) as bool
DECLARE SUB cropposition (byref x as integer, byref y as integer, byval unitsize as integer)
DECLARE FUNCTION wrappass (x as integer, y as integer, byref xgo as integer, byref ygo as integer, isveh as bool, ignore_passmap as bool = NO) as bool
DECLARE FUNCTION check_wallmap_collision (byval startpos as XYPair, byref pos as XYPair, byval size as XYPair, byval go as XYPair, isveh as bool, walls_over_edges as bool = YES) as integer
DECLARE FUNCTION sliding_wallmap_collision (byval startpos as XYPair, byref pos as XYPair, byval size as XYPair, byval xygo as XYPair, isveh as bool, walls_over_edges as bool = YES, friction as integer = 100) as integer
DECLARE FUNCTION wrapzonecheck (byval zone as integer, byval pos as XYPair, byval xygo as XYPair) as bool
DECLARE FUNCTION wrapcollision (byval posa as XYPair, byval xygoa as XYPair, byval posb as XYPair, byval xygob as XYPair) as bool
DECLARE FUNCTION wraptouch (byval pos1 as XYPair, byval pos2 as XYPair, byval distance as integer) as bool
DECLARE FUNCTION check_wall_edges(tilex as integer, tiley as integer, direction as DirNum, isveh as bool = NO, walls_over_edges as bool = YES, ignore_passmap as bool = NO) as bool

DECLARE SUB aheadxy OVERLOAD (byref x as integer, byref y as integer, byval direction as DirNum, byval distance as integer)
DECLARE SUB wrapxy OVERLOAD (byref x as integer, byref y as integer, byval unitsize as integer = 1)
DECLARE SUB wrapaheadxy OVERLOAD (byref x as integer, byref y as integer, byval direction as DirNum, byval distance as integer, byval unitsize as integer)

DECLARE SUB aheadxy OVERLOAD (byref p as XYPair, byval direction as DirNum, byval distance as integer)
DECLARE SUB wrapxy OVERLOAD (byref p as XYPair, byval unitsize as integer = 1)
DECLARE SUB wrapaheadxy OVERLOAD (byref p as XYPair, byval direction as DirNum, byval distance as integer, byval unitsize as integer)

DECLARE FUNCTION walkrotate(byval d as DirNum, byval rota as integer, byval amount as integer=1) as DirNum

DECLARE FUNCTION xypair_direction_to (src_v as XYPair, dest_v as XYPair, default as DirNum = -1) as DirNum

DECLARE SUB cancel_npc_walk(npci as NPCInst)
DECLARE SUB cancel_hero_walk(byval rank as integer)

'Vehicles
DECLARE FUNCTION vehicle_is_animating() as bool
DECLARE SUB reset_vehicle(vstate as VehicleState)
DECLARE SUB reload_vehicle()
DECLARE SUB dump_vehicle_state()
DECLARE SUB forcedismount ()
DECLARE SUB update_vehicle_state ()
DECLARE FUNCTION vehpass (byval n as integer, byval tile as integer, byval default as integer) as integer
DECLARE SUB vehicle_graceful_dismount ()
DECLARE FUNCTION vehscramble(byval target as XYPair) as bool
DECLARE SUB try_mount_vehicle(vehid as integer, npci as NPCIndex, force_mount as bool = NO)
DECLARE SUB forcemountvehicle (byval npcnum as NPCIndex)

#endif
