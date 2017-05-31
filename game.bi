'OHRRPGCE - game.bi
'(C) Copyright 1997-2017 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#IFNDEF GAME_BI
#DEFINE GAME_BI

#INCLUDE "game_udts.bi"
#INCLUDE "const.bi"

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
declare sub usemenusounds (byval deckey as integer = scUp, byval inckey as integer = scDown)
declare sub dotimer(timercontext as TimerContextEnum)
declare function dotimerbattle() as integer

declare function add_menu (byval record as integer, byval allow_duplicate as bool=NO) as integer
declare sub remove_menu (byval slot as integer, byval run_on_close as bool=YES)
declare sub bring_menu_forward (byval slot as integer)
declare function menus_allow_gameplay () as bool
declare function menus_allow_player () as bool
declare sub player_menu_keys ()
declare sub check_menu_tags ()
declare sub tag_updates (npc_visibility as bool=YES)
declare function game_usemenu (state as MenuState) as bool
declare function allowed_to_open_main_menu () as bool
declare function random_formation (byval set as integer) as integer
DECLARE FUNCTION activate_menu_item(mi as MenuDefItem, byval menuslot as integer) as bool

DECLARE SUB loadsay (byval box_id as integer)
DECLARE SUB init_text_box_slices(txt as TextBoxState)
DECLARE SUB cleanup_text_box ()
DECLARE SUB advance_text_box ()
DECLARE SUB add_rem_swap_lock_hero (box as TextBox)
DECLARE FUNCTION immediate_showtextbox() as bool

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

DECLARE SUB usenpc(byval cause as integer, byval npcnum as integer)

DECLARE SUB forceparty ()
DECLARE FUNCTION first_free_slot_in_party() as integer
DECLARE FUNCTION first_free_slot_in_active_party() as integer
DECLARE FUNCTION first_free_slot_in_reserve_party() as integer
DECLARE FUNCTION free_slots_in_party() as integer

DECLARE SUB queue_fade_in (delay as integer = 0, script_overridable as bool = NO)
DECLARE SUB check_for_queued_fade_in ()

DECLARE FUNCTION find_door (byval tilex as integer, byval tiley as integer) as integer
DECLARE FUNCTION find_doorlink (byval door_id as integer) as integer

DECLARE SUB update_hero_zones (byval who as integer)
DECLARE SUB update_npc_zones (byval npcref as integer)
DECLARE SUB process_zone_eachstep_triggers (who as string, byval zones as integer vector)
DECLARE SUB process_zone_entry_triggers (who as string, byval oldzones as integer vector, byval newzones as integer vector)

DECLARE SUB cleanup_other_temp_files ()
DECLARE SUB refresh_keepalive_file ()
DECLARE FUNCTION read_keepalive_as_days (keepalive_file as string) as integer
DECLARE FUNCTION guess_age_by_tmpdir_name(dirname as string) as integer

DECLARE SUB email_save_to_developer(save_slot as integer = -1, subject as string = "", body as string = "")
DECLARE SUB debug_menu()

DECLARE SUB update_virtual_gamepad_display()
DECLARE FUNCTION calc_virtual_gamepad_state() as bool
DECLARE SUB a_script_wants_keys()

DECLARE FUNCTION last_active_party_slot() as integer 'FIXME: use this everywhere!
DECLARE FUNCTION is_active_party_slot(byval slot as integer) as integer
DECLARE FUNCTION active_party_size() as integer
DECLARE FUNCTION loop_active_party_slot(byval slot as integer, byval direction as integer=1) as integer

DECLARE FUNCTION top_menu_allows_controls() as bool

Enum WalkaboutCollisionType
  collideNone = 0
  collideWall = 1
  collideMoveZone = 2
  collideAvoidZone = 3
  collideNPC = 4
  collideHero = 5
End Enum

DECLARE FUNCTION npc_collision_check OVERLOAD (npci as NPCInst, byval direction as integer, byref collision_type as WalkaboutCollisionType=collideNone) as bool
DECLARE FUNCTION npc_collision_check OVERLOAD (npci as NPCInst, npcdata as NPCType, byval direction as integer, byref collision_type as WalkaboutCollisionType=collideNone) as bool
DECLARE FUNCTION npc_collision_check OVERLOAD (npci as NPCInst, npcdata as NPCType, byval xgo as integer, byval ygo as integer, byref collision_type as WalkaboutCollisionType=collideNone) as bool
DECLARE FUNCTION npc_collision_check_at(npci as NPCInst, tile as XYPair, byval direction as integer, byref collision_type as WalkaboutCollisionType=collideNone) as bool
DECLARE FUNCTION npc_collision_check_walls_and_zones(npci as NPCInst, byval direction as integer) as bool
DECLARE FUNCTION npc_collision_check_at_walls_and_zones(npci as NPCInst, tile as XYPair, byval direction as integer) as bool

DECLARE FUNCTION herox(byval rank as integer) byref as integer
DECLARE FUNCTION heroy(byval rank as integer) byref as integer
DECLARE FUNCTION heroz(byval rank as integer) byref as integer
DECLARE FUNCTION herodir(byval rank as integer) byref as integer
DECLARE FUNCTION herotx(byval rank as integer) as integer
DECLARE FUNCTION heroty(byval rank as integer) as integer

DECLARE SUB change_hero_speed(byval rank as integer, byval new_speed as integer)
DECLARE SUB interpolatecat (byval old_speed as integer = -1)
DECLARE SUB resetcaterpillar ()
DECLARE SUB resetcaterpillar_for_one_hero (byval rank as integer, byval newx as integer, byval newy as integer)

#ENDIF
