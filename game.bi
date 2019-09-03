'OHRRPGCE - game.bi
'(C) Copyright 1997-2017 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#IFNDEF GAME_BI
#DEFINE GAME_BI

#INCLUDE "game_udts.bi"
#INCLUDE "const.bi"
#INCLUDE "pathfinding.bi"

declare sub prepare_map (byval afterbat as bool=NO, byval afterload as bool=NO)
declare sub displayall()

declare sub gmap_updates()
declare sub loadmap_gmap(byval mapnum as integer)
declare sub loadmap_npcl(byval mapnum as integer)
declare sub loadmap_npcd(byval mapnum as integer)
declare sub loadmap_tilemap(byval mapnum as integer)
declare sub loadmap_passmap(byval mapnum as integer)
declare sub loadmap_foemap(byval mapnum as integer)
declare sub loadmap_zonemap(byval mapnum as integer)
declare sub loadmap_bitmask (byval mapnum as integer, byval loadmask as integer)

declare sub menusound(byval s as integer)
declare sub usemenusounds (byval deckey as integer = ccUp, byval inckey as integer = ccDown)
declare sub dotimer(timercontext as TimerContextEnum)
declare function dotimerbattle() as integer

declare function add_menu (byval record as integer, byval allow_duplicate as bool=NO) as integer
declare sub remove_menu (byval slot as integer, byval run_on_close as bool=YES)
declare sub bring_menu_forward (byval slot as integer)
declare function normal_controls_disabled () as bool
declare function menus_allow_gameplay () as bool
declare function menus_allow_player () as bool
declare sub player_menu_keys ()
declare sub update_menu_items ()
declare function update_menu_item (mi as MenuDefItem) as bool
declare sub tag_updates (npc_visibility as bool=YES)
declare function game_usemenu (state as MenuState, menu as MenuDef) as bool
declare function allowed_to_open_main_menu () as bool
declare function random_formation (byval set as integer) as integer
DECLARE FUNCTION activate_menu_item(mi as MenuDefItem, byval menuslot as integer) as bool

DECLARE SUB loadsay (byval box_id as integer)
DECLARE SUB init_text_box_slices(txt as TextBoxState)
DECLARE SUB cleanup_text_box ()
DECLARE SUB advance_text_box ()
DECLARE SUB add_rem_swap_lock_hero (box as TextBox)
DECLARE SUB delete_save_load_game (box as TextBox)
DECLARE FUNCTION immediate_showtextbox() as bool

DECLARE SUB SetupGameSlices
DECLARE SUB SetupMapSlices(byval to_max as integer)
DECLARE SUB DestroyGameSlices(dumpdebug as bool = NO)
DECLARE SUB recreate_map_slices()
DECLARE SUB refresh_map_slice()
DECLARE SUB refresh_map_slice_tilesets()
DECLARE SUB refresh_walkabout_layer_sort()
DECLARE SUB update_map_slices_for_new_tilemap()
DECLARE SUB cleanup_game_slices ()

DECLARE SUB save_game_config()
DECLARE SUB exit_gracefully(need_fade_out as bool = NO)

DECLARE FUNCTION is_rpg(path as string) as bool
DECLARE FUNCTION is_rpgdir(path as string) as bool
DECLARE FUNCTION select_rpg_or_rpgdir(path as string) as bool
DECLARE FUNCTION seek_rpg_or_rpgdir_and_select_it(where as string, gamename as string) as bool

DECLARE FUNCTION usenpc(byval cause as integer, byval npcnum as NPCIndex) as bool

DECLARE SUB forceparty ()
DECLARE FUNCTION findhero (byval id as integer, byval direction as integer = 1, errlvl as scriptErrEnum = serrIgnore) as integer
DECLARE FUNCTION first_used_slot_in_party() as integer
DECLARE FUNCTION first_free_slot_in_party() as integer
DECLARE FUNCTION first_free_slot_in_active_party() as integer
DECLARE FUNCTION first_free_slot_in_reserve_party() as integer
DECLARE FUNCTION free_slots_in_party() as integer
DECLARE FUNCTION party_size () as integer
DECLARE FUNCTION active_party_size () as integer
DECLARE FUNCTION caterpillar_size () as integer
'See also liveherocount
DECLARE FUNCTION active_party_slots() as integer
DECLARE FUNCTION last_active_party_slot() as integer 'FIXME: use this everywhere!
DECLARE FUNCTION is_active_party_slot(byval slot as integer) as integer
DECLARE FUNCTION loop_active_party_slot(byval slot as integer, byval direction as integer=1) as integer

DECLARE SUB queue_fade_in (delay as integer = 0, script_overridable as bool = NO)
DECLARE SUB check_for_queued_fade_in ()

DECLARE FUNCTION find_door (byval tilepos as XYPair) as integer
DECLARE FUNCTION find_doorlink (byref thisdoorlink as doorlink, byval door_id as integer, byval map_id as integer=-1) as bool

DECLARE SUB update_hero_zones (byval who as integer)
DECLARE SUB update_npc_zones (byval npcref as integer)
DECLARE SUB process_zone_eachstep_triggers (who as string, byval zones as integer vector)
DECLARE SUB process_zone_entry_triggers (who as string, byval oldzones as integer vector, byval newzones as integer vector)

DECLARE SUB cleanup_other_temp_files ()
DECLARE SUB refresh_keepalive_file ()
DECLARE FUNCTION read_keepalive_as_days (keepalive_file as string) as integer
DECLARE FUNCTION guess_age_by_tmpdir_name(dirname as string) as integer

DECLARE SUB email_save_to_developer(save_slot as integer = -1, prefix as string="", subject as string = "", body as string = "")
DECLARE SUB debug_menu()

DECLARE SUB update_virtual_gamepad_display()
DECLARE FUNCTION calc_virtual_gamepad_state() as bool
DECLARE SUB a_script_wants_keys()

DECLARE FUNCTION top_menu_allows_controls() as bool

Enum WalkaboutCollisionType
  collideNone = 0
  collideWall = 1  'Including edge of the map
  collideMoveZone = 2
  collideAvoidZone = 3
  collideNPC = 4
  collideHero = 5
End Enum

DECLARE FUNCTION npc_collision_check OVERLOAD (npci as NPCInst, byval direction as DirNum, byref collision_type as WalkaboutCollisionType=collideNone, byval npc_ccache as NPCCollisionCache Ptr=0) as bool
DECLARE FUNCTION npc_collision_check OVERLOAD (npci as NPCInst, npcdata as NPCType, byval direction as DirNum, byref collision_type as WalkaboutCollisionType=collideNone, byval npc_ccache as NPCCollisionCache Ptr=0) as bool
DECLARE FUNCTION npc_collision_check OVERLOAD (npci as NPCInst, npcdata as NPCType, byval xgo as integer, byval ygo as integer, byref collision_type as WalkaboutCollisionType=collideNone, byval npc_ccache as NPCCollisionCache Ptr=0) as bool
DECLARE FUNCTION npc_collision_check_at(npci as NPCInst, tile as XYPair, byval direction as DirNum, byref collision_type as WalkaboutCollisionType=collideNone, byval npc_ccache as NPCCollisionCache Ptr=0) as bool
DECLARE FUNCTION npc_collision_check_npcs_and_heroes(npci as NPCInst, byval direction as DirNum) as bool
DECLARE FUNCTION npc_collision_check_walls_and_zones(npci as NPCInst, byval direction as DirNum) as bool
DECLARE FUNCTION npc_collision_check_at_walls_and_zones(npci as NPCInst, tile as XYPair, byval direction as DirNum) as bool

DECLARE FUNCTION hero_collision_check OVERLOAD (byval rank as integer, byval xgo as integer, byval ygo as integer, byref collision_type as WalkaboutCollisionType=collideNone, byval npc_ccache as NPCCollisionCache Ptr=0) as bool
DECLARE FUNCTION hero_collision_check OVERLOAD (byval rank as integer, byval direction as DirNum, byref collision_type as WalkaboutCollisionType=collideNone, byval npc_ccache as NPCCollisionCache Ptr=0) as bool
DECLARE FUNCTION hero_collision_check_at(byval rank as integer, tile as XYPair, byval direction as DirNum, byref collision_type as WalkaboutCollisionType=collideNone, byval npc_ccache as NPCCollisionCache Ptr=0) as bool

DECLARE FUNCTION herox(byval rank as integer) byref as integer
DECLARE FUNCTION heroy(byval rank as integer) byref as integer
DECLARE FUNCTION heropos(byval rank as integer) byref as XYPair
DECLARE FUNCTION heroz(byval rank as integer) byref as integer
DECLARE FUNCTION herodir(byval rank as integer) byref as DirNum
DECLARE FUNCTION herotx(byval rank as integer) as integer
DECLARE FUNCTION heroty(byval rank as integer) as integer
DECLARE FUNCTION herotpos(byval rank as integer) as XYPair

DECLARE SUB change_hero_speed(byval rank as integer, byval new_speed as integer)
DECLARE SUB interpolatecat (byval old_speed as integer = -1)
DECLARE SUB resetcaterpillar ()
DECLARE SUB resetcaterpillar_for_one_hero (byval rank as integer, byval newx as integer, byval newy as integer)

DECLARE SUB cancel_npc_movement_override (npci as NPCInst)

DECLARE FUNCTION caterpillar_is_suspended() as bool 
DECLARE FUNCTION player_is_suspended() as bool 

DECLARE FUNCTION user_triggered_main_menu() as bool

DECLARE FUNCTION hero_is_pathfinding(byval rank as integer) as bool
DECLARE SUB cancel_hero_pathfinding(byval rank as integer, byval user_only as bool=NO)
DECLARE SUB path_hero_to_tile(byval rank as integer, dest as XYPair, byval stop_after_stillticks as integer=0)
DECLARE SUB path_hero_to_npc(byval rank as integer, byval npc as NPCIndex, byval stop_when_npc_reached as bool, byval stop_after_stillticks as integer=0)
DECLARE SUB user_trigger_hero_pathfinding()
DECLARE SUB update_hero_pathfinding(byval rank as integer)
DECLARE SUB update_hero_pathfinding_menu_queue()
DECLARE SUB display_hero_pathfinding (byval rank as integer)
DECLARE SUB clear_hero_pathfinding_display(byval rank as integer)
DECLARE SUB update_hero_pathfinding_display(byval tile as XYpair, byval rank as integer=0)
DECLARE SUB check_pathfinding_for_map_change()

DECLARE SUB heromove_walk_ahead(byval rank as integer)

#ENDIF
