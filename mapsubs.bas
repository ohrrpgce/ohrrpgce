'OHRRPGCE CUSTOM - Map editor and map picker
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
#include "config.bi"
#include "const.bi"
#include "udts.bi"
#include "custom_udts.bi"

'---------------------------- External subs and functions --------------------------------

DECLARE SUB npcdef (st as MapEditState, npc_img() as GraphicPair, gmap() as integer, zmap as ZoneMap)
DECLARE SUB tile_anim_draw_range(tastuf() as integer, byval taset as integer, byval page as integer)


'---------------------------- Local subs/functions & types -------------------------------

DECLARE SUB make_map_picker_menu (topmenu() as string, state as MenuState)
DECLARE SUB mapeditor (byval mapnum as integer)
DECLARE FUNCTION addmaphow () as integer

DECLARE SUB loadpasdefaults (byref defaults as integer vector, tilesetnum as integer)

DECLARE SUB fill_map_area(st as MapEditState, byval x as integer, byval y as integer, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, reader as FnReader)
DECLARE SUB fill_with_other_area(st as MapEditState, byval x as integer, byval y as integer, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, reader as FnReader)

DECLARE FUNCTION mapedit_npc_at_spot(st as MapEditState) as integer
DECLARE FUNCTION mapedit_on_screen(st as MapEditState, byval x as integer, byval y as integer) as integer
DECLARE SUB mapedit_focus_camera(st as MapEditState, byval x as integer, byval y as integer)

DECLARE FUNCTION mapedit_npc_instance_count(st as MapEditState, byval id as integer) as integer

'Undo
DECLARE SUB add_change_step(byref changelist as MapEditUndoTile vector, byval x as integer, byval y as integer, byval value as integer, byval mapid as integer)
DECLARE SUB add_undo_step(st as MapEditState, byval x as integer, byval y as integer, byval oldvalue as integer, byval mapid as integer)
DECLARE FUNCTION undo_stroke(st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, byval redo as bool = NO) as MapEditUndoTile vector
DECLARE FUNCTION redo_stroke(st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap) as MapEditUndoTile vector
DECLARE SUB undo_stroke_internal(st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, byref changelist as MapEditUndoTile vector, byval redo as bool = NO)
DECLARE SUB undo_preview(st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap)
DECLARE SUB mapedit_throw_away_history(st as MapEditState)
DECLARE SUB mapedit_show_undo_change(st as MapEditState, byval undostroke as MapEditUndoTile vector)

'Mark+Clone
DECLARE FUNCTION create_changelist(st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, visible() as integer, gmap() as integer, rect as RectType) as MapEditUndoTile vector
DECLARE SUB apply_changelist(st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, visible() as integer, gmap() as integer, byref changelist as MapEditUndoTile vector, offset as XYPair)

'Zones
DECLARE SUB draw_zone_tileset(byval zonetileset as Frame ptr)
DECLARE SUB draw_zone_tileset2(byval zonetileset as Frame ptr)
DECLARE SUB draw_zone_tileset3(byval zonetileset as Frame ptr)
DECLARE SUB mapedit_doZoneHinting(st as MapEditState, zmap as ZoneMap)
DECLARE SUB zonemenu_add_zone (byref zonemenu as SimpleMenuItem vector, zonecolours() as integer, byval info as ZoneInfo ptr)
DECLARE FUNCTION mapedit_try_assign_colour_to_zone(byval id as integer, zonecolours() as integer, viszonelist() as integer) as integer
DECLARE SUB mapedit_update_visible_zones (st as MapEditState, byref zonemenu as SimpleMenuItem vector, zonemenustate as MenuState, zmap as ZoneMap, lockedzonelist() as integer)
DECLARE SUB mapedit_edit_zoneinfo(st as MapEditState, zmap as ZoneMap)
DECLARE SUB mapedit_zonespam(st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap)
DECLARE SUB draw_zone_minimap(st as MapEditState, tmap as TileMap, byval bitnum as integer, byval col as integer)

ENUM LayerMenuItemType
  ltOther
  ltPreviousMenu
  ltDefaultTileset
  ltLayerName
  ltLayerTileset
  ltLayerEnabled
END ENUM

TYPE LayerMenuItem  'EXTENDS BasicMenuItem
  'members copied from BasicMenuItem
  text as string
  col as integer
  bgcol as integer
  unselectable as integer
  disabled as integer

  'new members
  role as LayerMenuItemType
  layernum as integer    '-1 if not a layer
  gmapindex as integer   '-1 if enabled/visibility choice rather than tileset choice
END TYPE

DECLARE_VECTOR_OF_TYPE(LayerMenuItem, LayerMenuItem)
DEFINE_VECTOR_OF_CLASS(LayerMenuItem, LayerMenuItem)

DECLARE FUNCTION LayerIsVisible(vis() as integer, byval l as integer) as bool
DECLARE FUNCTION LayerIsEnabled(gmap() as integer, byval l as integer) as bool
DECLARE SUB SetLayerVisible(vis() as integer, byval l as integer, byval v as bool)
DECLARE SUB SetLayerEnabled(gmap() as integer, byval l as integer, byval v as bool)
DECLARE SUB ToggleLayerVisible(vis() as integer, byval l as integer)
DECLARE SUB ToggleLayerEnabled(vis() as integer, byval l as integer)

DECLARE SUB DrawDoorPair(st as MapEditState, byval linknum as integer, map() as TileMap, pass as TileMap, doors() as door, link() as doorlink, gmap() as integer)

DECLARE SUB calculatepassblock(st as MapEditState, x as integer, y as integer, map() as TileMap, pass as TileMap)

DECLARE SUB resizemapmenu (st as MapEditState, map() as TileMap, byref rs as MapResizeState)
DECLARE SUB resizetiledata OVERLOAD (tmap as TileMap, rs as MapResizeState, byref yout as integer, page as integer)
DECLARE SUB resizetiledata OVERLOAD (tmaps() as TileMap, rs as MapResizeState, byref yout as integer, page as integer)
DECLARE SUB resizetiledata OVERLOAD (tmap as TileMap, x_off as integer, y_off as integer, new_width as integer, new_height as integer, byref yout as integer, page as integer)

DECLARE SUB update_npc_graphics(st as MapEditState, npc_img() as GraphicPair)
DECLARE SUB update_tilepicker(st as MapEditState)
DECLARE SUB verify_map_size (st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, mapname as string)
DECLARE SUB fix_tilemaps(map() as TileMap)
DECLARE SUB mapedit_loadmap (st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, gmap() as integer, visible() as integer, doors() as Door, link() as DoorLink, mapname as string)
DECLARE SUB mapedit_load_tilesets (st as MapEditState, map() as TileMap, gmap() as integer)
DECLARE SUB mapedit_savemap (st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, gmap() as integer, doors() as Door, link() as DoorLink, mapname as string)
DECLARE SUB new_blank_map (st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, gmap() as integer, doors() as Door, link() as DoorLink)
DECLARE SUB mapedit_addmap()
DECLARE SUB mapedit_resize(st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, gmap() as integer, doors() as Door, link() as DoorLink, mapname as string)
DECLARE SUB mapedit_delete(st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, gmap() as integer, doors() as Door, link() as DoorLink, npc_img() as GraphicPair, mapname as string)
DECLARE SUB link_one_door(st as MapEditState, linknum as integer, link() as DoorLink, doors() as Door, map() as TileMap, pass as TileMap, gmap() as integer)
DECLARE SUB mapedit_linkdoors (st as MapEditState, map() as TileMap, pass as TileMap, gmap() as integer, doors() as Door, link() as DoorLink)
DECLARE FUNCTION mapedit_pick_layer(st as MapEditState, gmap() as integer, map() as TileMap, message as string, other_option as string = "") as integer
DECLARE SUB mapedit_layers (st as MapEditState, gmap() as integer, visible() as integer, map() as TileMap)
DECLARE SUB mapedit_makelayermenu(st as MapEditState, byref menu as LayerMenuItem vector, state as MenuState, gmap() as integer, visible() as integer, map() as TileMap, byval resetpt as bool, byval selectedlayer as integer = 0)
DECLARE SUB mapedit_copy_layer(st as MapEditState, map() as TileMap, gmap() as integer, byval src as integer, byval dest as integer)
DECLARE SUB mapedit_append_new_layers(st as MapEditState, map() as TileMap, vis() as integer, gmap() as integer, howmany as integer)
DECLARE SUB mapedit_insert_new_layer(st as MapEditState, map() as TileMap, vis() as integer, gmap() as integer, byval where as integer)
DECLARE SUB mapedit_delete_layer(st as MapEditState, map() as TileMap, vis() as integer, gmap() as integer, byval which as integer)
DECLARE SUB mapedit_swap_layers(st as MapEditState, map() as TileMap, vis() as integer, gmap() as integer, byval l1 as integer, byval l2 as integer)
DECLARE SUB mapedit_gmapdata(st as MapEditState, gmap() as integer, zmap as ZoneMap)
DECLARE SUB mapedit_draw_icon(st as MapEditState, icon as string, byval x as integer, byval y as integer, byval highlight as bool = NO)
DECLARE SUB mapedit_list_npcs_by_tile (st as MapEditState)

DECLARE SUB mapedit_import_export(st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, gmap() as integer, visible() as integer, doors() as Door, link() as DoorLink, mapname as string)

DECLARE FUNCTION door_exists (doors() as Door, id as integer) as bool
DECLARE SUB set_door_exists (doors() as Door, id as integer, value as bool)
DECLARE FUNCTION find_last_used_doorlink(link() as DoorLink) as integer
DECLARE FUNCTION find_door_at_spot (x as integer, y as integer, doors() as Door) as integer
DECLARE FUNCTION find_first_free_door (doors() as Door) as integer
DECLARE FUNCTION find_first_doorlink_by_door(doornum as integer, link() as DoorLink) as integer

DECLARE SUB resize_rezoom_mini_map(st as MapEditState, byref rs as MapResizeState, map() as TileMap)
DECLARE SUB show_minimap(byref map as MapEditState, map() as TileMap)
DECLARE SUB mapedit_pickblock(st as MapEditState)
DECLARE SUB resize_buildmenu(byref rs as MapResizeState)
DECLARE SUB resize_dimchange(st as MapEditState, byref rs as MapResizeState, map() as TileMap)
DECLARE SUB resize_correct_width(st as MapEditState, byref rs as MapResizeState, map() as TileMap)
DECLARE SUB resize_correct_height(st as MapEditState, byref rs as MapResizeState, map() as TileMap)

#include "allmodex.bi"
#include "common.bi"
#include "customsubs.bi"
#include "cglobals.bi"

#include "scrconst.bi"
#include "loading.bi"

DEFINE_VECTOR_OF_TYPE(MapEditUndoTile, MapEditUndoTile)
DEFINE_VECTOR_VECTOR_OF(MapEditUndoTile, MapEditUndoTile)


'==========================================================================================
'                                    Map listing menu
'==========================================================================================


FUNCTION addmaphow () as integer
'--Return values
'  -2  =Cancel
'  -1  =New blank
'  >=0 =Copy

DIM menu(2) as string
DIM maptocopy as integer = 0
DIM state as MenuState
state.last = UBOUND(menu)
state.size = 24

state.need_update = YES
setkeys YES
DO
 setwait 55
 setkeys YES
 IF keyval(scESC) > 1 THEN
  '--return cancel
  RETURN -2
 END IF
 IF keyval(scF1) > 1 THEN show_help "add_map_how"
 usemenu state
 IF state.pt = 2 THEN
  IF intgrabber(maptocopy, 0, gen(genMaxMap)) THEN state.need_update = YES
 END IF
 IF enter_space_click(state) THEN
  SELECT CASE state.pt
   CASE 0 ' cancel
    RETURN -2
   CASE 1 ' blank
    RETURN -1
   CASE 2 ' copy
    RETURN maptocopy
  END SELECT
 END IF
 IF state.need_update THEN
  state.need_update = NO
  menu(0) = "Cancel"
  menu(1) = "New Blank Map"
  menu(2) = "Copy of map " & maptocopy & " " & getmapname(maptocopy)
 END IF
 clearpage vpage
 standardmenu menu(), state, 0, 0, vpage

 setvispage vpage
 dowait
LOOP
END FUNCTION

SUB make_map_picker_menu(topmenu() as string, state as MenuState)
 REDIM topmenu(0)
 topmenu(0) = "Return to Main Menu"
 FOR i as integer = 0 TO gen(genMaxMap)
  str_array_append topmenu(), "Map " & i & ": " + getmapname(i)
 NEXT
 str_array_append topmenu(), "Add a New Map"

 state.size = 24
 state.last = UBOUND(topmenu)
END SUB

SUB map_picker ()
 DIM topmenu() as string
 DIM topmenu_display() as string
 DIM state as MenuState
 state.spacing = 8
 state.autosize = YES
 DIM selectst as SelectTypeState
 
 make_map_picker_menu topmenu(), state

 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "mapedit_choose_map"
  usemenu state
  IF select_by_typing(selectst) THEN
   select_on_word_boundary_excluding topmenu(), selectst, state, "map"
  END IF

  IF enter_space_click(state) THEN
   IF state.pt = 0 THEN EXIT DO
   IF state.pt > 0 AND state.pt <= gen(genMaxMap) + 1 THEN
    mapeditor state.pt - 1
    make_map_picker_menu topmenu(), state  'User could delete map
   ELSEIF state.pt = gen(genMaxMap) + 2 THEN
    mapedit_addmap
    make_map_picker_menu topmenu(), state
   END IF
  END IF

  clearpage vpage

  REDIM topmenu_display(LBOUND(topmenu) TO UBOUND(topmenu)) as string
  highlight_menu_typing_selection topmenu(), topmenu_display(), selectst, state
  standardmenu topmenu_display(), state, 0, 0, vpage
  setvispage vpage
  dowait
 LOOP
END SUB


'==========================================================================================
'                                         Brushes
'==========================================================================================


'Note dummy arguments: all brush functions should have the same signature.
SUB tilebrush (st as MapEditState, byval x as integer, byval y as integer, byval tile as integer = -1, byval layer as integer = -1, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap)
 IF tile = -1 THEN tile = st.tool_value
 IF layer = -1 THEN layer = st.layer
 DIM oldval as integer = readblock(map(layer), x, y)
 IF oldval <> tile THEN
  add_undo_step st, x, y, oldval, mapIDLayer + layer
  writeblock map(layer), x, y, tile
 END IF
 IF st.defpass THEN calculatepassblock st, x, y, map(), pass
END SUB

'Note dummy arguments: all brush functions should have the same signature
SUB wallbrush (st as MapEditState, byval x as integer, byval y as integer, byval tile as integer = -1, byval extraarg as integer = -1, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap)
 IF tile = -1 THEN tile = st.tool_value
 DIM oldval as integer = readblock(pass, x, y)
 IF oldval <> tile THEN
  add_undo_step st, x, y, oldval, mapIDPass
  writeblock pass, x, y, tile
 END IF
END SUB

'Like wallbrush, but only sets the bits specified in st.wallmap_mask
'Note dummy arguments: all brush functions should have the same signature
SUB wallbitbrush (st as MapEditState, byval x as integer, byval y as integer, byval walls as integer = -1, byval extraarg as integer = -1, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap)
 IF walls = -1 THEN walls = st.tool_value
 DIM oldwalls as integer = readblock(pass, x, y)
 walls = (oldwalls AND NOT st.wallmap_mask) OR walls
 IF oldwalls <> walls THEN
  add_undo_step st, x, y, oldwalls, mapIDPass
  writeblock pass, x, y, walls
 END IF
END SUB

'Note dummy arguments: all brush functions should have the same signature
SUB foebrush (st as MapEditState, byval x as integer, byval y as integer, byval foe as integer = -1, byval extraarg as integer = -1, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap)
 IF foe = -1 THEN foe = st.tool_value
 DIM oldval as integer = readblock(emap, x, y)
 IF oldval <> foe THEN
  add_undo_step st, x, y, oldval, mapIDFoe
  writeblock emap, x, y, foe
 END IF
END SUB

'Note dummy arguments: all brush functions should have the same signature
SUB zonebrush (st as MapEditState, byval x as integer, byval y as integer, byval value as integer = -1, byval zone as integer = -1, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap)
 IF value = -1 THEN value = st.tool_value
 IF zone = -1 THEN zone = st.cur_zone
 DIM new_stroke as bool = st.new_stroke  'Modified by add_undo_step
 DIM oldval as integer = CheckZoneAtTile(zmap, zone, x, y)
 IF (oldval = 0) <> (value = 0) THEN
  add_undo_step st, x, y, oldval, mapIDZone + zone
  IF value = 0 THEN
   UnsetZoneTile zmap, zone, x, y
  ELSE
   IF SetZoneTile(zmap, zone, x, y) = NO THEN
    IF new_stroke THEN
     pop_warning "You have already placed this tile in 15 other zones, and that is the maximum supported. Sorry!"
    END IF
   END IF
  END IF
  st.zones_needupdate = YES
 END IF
END SUB

'Note dummy arguments: all brush functions should have the same signature
'Values allowed: 0 to 255
SUB tempbrush (st as MapEditState, byval x as integer, byval y as integer, byval tile as integer = -1, byval extraarg as integer = -1, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap)
 writeblock st.temptilemap, x, y, tile
END SUB


'==========================================================================================
'                                         Readers
'==========================================================================================


'Note dummy arguments: all reader functions should have the same signature
FUNCTION tilereader (st as MapEditState, byval x as integer, byval y as integer, byval layer as integer = -1, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap) as integer
 IF layer = -1 THEN layer = st.layer
 RETURN readblock(map(layer), x, y)
END FUNCTION

'Note dummy arguments: all reader functions should have the same signature
FUNCTION foereader (st as MapEditState, byval x as integer, byval y as integer, byval extraarg as integer = -1, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap) as integer
 RETURN readblock(emap, x, y)
END FUNCTION

'Note dummy arguments: all reader functions should have the same signature
FUNCTION zonereader (st as MapEditState, byval x as integer, byval y as integer, byval zone as integer = -1, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap) as integer
 IF zone = -1 THEN zone = st.cur_zone
 RETURN CheckZoneAtTile(zmap, zone, x, y)
END FUNCTION

'Note dummy arguments: all reader functions should have the same signature
FUNCTION tempreader (st as MapEditState, byval x as integer, byval y as integer, byval extraarg as integer = -1, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap) as integer
 RETURN readblock(st.temptilemap, x, y)
END FUNCTION


'==========================================================================================
'                               Main SUB (toplevel menu)
'==========================================================================================

FUNCTION mapedit_npc_instance_count(st as MapEditState, byval id as integer) as integer
 DIM num as integer = 0
 FOR i as integer = 0 to UBOUND(st.npc_inst)
  IF ABS(st.npc_inst(i).id) - 1 = id THEN num += 1
 NEXT i
 RETURN num
END FUNCTION

SUB mapeditor (byval mapnum as integer)
STATIC remember_menu_pt as integer = 0

DIM st as MapEditState
DIM modenames(5) as string
DIM gmap(dimbinsize(binMAP)) as integer
DIM pal16(288) as integer
DIM npcnum(max_npc_defs - 1) as integer
DIM her as HeroDef
DIM hero_gfx as GraphicPair

REDIM doors(99) as door
REDIM link(199) as doorlink

st.mapnum = mapnum
st.editmode = 0
st.seteditmode = -1
DIM mode_tools_map(zone_mode, 10) as integer = { _
   {draw_tool, box_tool, fill_tool, replace_tool, mark_tool, clone_tool, -1}, _       'tile_mode
   {draw_tool, box_tool, paint_tool, mark_tool, clone_tool, -1}, _                    'pass_mode
   {-1}, _                                                                            'door_mode
   {-1}, _                                                                            'npc_mode
   {draw_tool, box_tool, fill_tool, replace_tool, paint_tool, mark_tool, clone_tool, -1}, _ 'foe_mode
   {draw_tool, box_tool, fill_tool, paint_tool, mark_tool, clone_tool, -1} _          'zone_mode
}
DIM mode_tools as integer vector
v_new mode_tools
DIM toolsbar_available as integer  'Whether you can select the current tool
DIM drawing_allowed as integer     'Whether you can actually draw

REDIM lockedzonelist(-1 TO -1) as integer 'The zones chosen to be always displayed. At most 8 (index 0 onwards, start at -1 for fake zero-length arrays) 
DIM gauze_ticker as integer = 0  'for hidden zones animation
'The floating menu that displays a list of zones. These are created and updated in mapedit_update_visible_zones
DIM zonemenu as SimpleMenuItem vector
DIM zonemenustate as MenuState
DIM zone_delete_tool as integer  'Whether Space should add or remove tiles

DIM npczone_needupdate as integer

DIM as integer jiggle(maplayerMax \ 16)
DIM as integer visible(maplayerMax \ 16) = {-1} 'used as bitsets: all layers visible

'npcdef assumes that npc_img is sized (0 TO max_npc_defs - 1), just like st.npc_def()
DIM npc_img(max_npc_defs - 1) as GraphicPair

REDIM map(0) as TileMap ' dummy empty map data, will be resized later
DIM pass as TileMap
DIM emap as TileMap
DIM zmap as ZoneMap

st.secondary_undo_buffer = NULL
v_new st.history
st.history_step = 0
st.new_stroke = YES

DIM mapname as string

'Some temporary variables
DIM as integer temp, doorid, doorlinkid

'--create a palette for the cursor
st.cursor.pal = palette16_new()
'set the colors that actually get used
st.cursor.pal->col(1) = uilook(uiText)
st.cursor.pal->col(2) = uilook(uiMenuItem)

'--create cursor
' the colors here are actually offsets into the 16-color palette.
' see the st.cursor.pal construction above
st.cursor.sprite = frame_new(20, 20, 2, YES)

rectangle st.cursor.sprite, 0, 0, 20, 20, 1
rectangle st.cursor.sprite, 1, 1, 18, 18, 0
rectangle st.cursor.sprite, 2, 2, 16, 16, 2
rectangle st.cursor.sprite, 3, 3, 14, 14, 0

rectangle st.cursor.sprite + 1, 0, 0, 20, 20, 1
rectangle st.cursor.sprite + 1, 1, 1, 18, 18, 0
rectangle st.cursor.sprite + 1, 3, 3, 14, 14, 2
rectangle st.cursor.sprite + 1, 4, 4, 12, 12, 0

'--These tilesets indicate up to 8 zones at once
'--create three alternative zone tilemaps, I can't decide!
DIM zonetileset(2) as Frame ptr
zonetileset(0) = frame_new(20, 20 * 256, , YES)  'large tilesets
zonetileset(1) = frame_new(20, 20 * 256, , YES)
zonetileset(2) = frame_new(20, 20 * 256, , YES)
draw_zone_tileset zonetileset(0)
draw_zone_tileset2 zonetileset(1)
draw_zone_tileset3 zonetileset(2)
'frame_export_bmp8 "zt3.bmp", zonetileset(2), master()

DIM overlaytileset as Frame ptr
overlaytileset = frame_new(20, 20 * 160, , YES)
fuzzyrect overlaytileset, 0, 1*20, 20, 20, uilook(uiHighlight)  'Zone edit mode, and NPC movement zone
fuzzyrect overlaytileset, 0, 2*20, 20, 20, uilook(uiHighlight2) 'NPC avoidance zone
fuzzyrect overlaytileset, 0, 3*20, 20, 20, uilook(uiHighlight2) 'NPC avoidance zone (overriding movement zone)
rectangle overlaytileset, 0, 6*20, 20, 20, uilook(uiDisabledItem)  '???

'Tiles 10 - 15 are for the 'hidden zone' animation. I think it's easier on the eyes than 2 frame flickering.
'Leave tiles 10-12 blank
FOR i as integer = 1 TO 3
 'fuzzyrect overlaytileset, 0, (12 + i)*20, 20, 20, uilook(uiDisabledItem), 5 * i
 fuzzyrect overlaytileset, 0, (12 + i)*20, 20, 20, boxlook(15-i).edgecol, 5 * i
NEXT

'Plenty of tiles left for other purposes

'Note that most of this array is empty
DIM toolinfo(NUM_TOOLS) as ToolInfoType
WITH toolinfo(draw_tool)
 .name = "Draw"
 .icon = "D"  'CHR(3)
 .shortcut = scD
END WITH
WITH toolinfo(box_tool)
 .name = "Box"
 .icon = "B"  'CHR(4)
 .shortcut = scB
END WITH
WITH toolinfo(fill_tool)
 .name = "Fill"
 .icon = "F"
 .shortcut = scF
END WITH
WITH toolinfo(replace_tool)
 .name = "Replace"
 .icon = "R"
 .shortcut = scR
END WITH
WITH toolinfo(paint_tool)
 .name = "Paint Tilemap"
 .icon = "P"
 .shortcut = scP
END WITH
WITH toolinfo(mark_tool)
 .name = "Mark (Copy)"
 .icon = "M"
 .shortcut = scM
END WITH
WITH toolinfo(clone_tool)
 .name = "Clone (Paste)"
 .icon = "C"
 .shortcut = scC
END WITH

'--load hero graphics--
loadherodata her, 0
load_sprite_and_pal hero_gfx, 4, her.walk_sprite, her.walk_sprite_pal


modenames(0) = "Picture Mode"
modenames(1) = "Passability Mode"
modenames(2) = "Door Placement Mode"
modenames(3) = "NPC Placement Mode"
modenames(4) = "Foe Mapping Mode"
modenames(5) = "Zone Mapping Mode"

'Due to laziness, the menubar shows all 256 tiles (including animated tiles),
'while the tileset tile picker hides unused animated tile patterns.
'See mapedit_pickblock_setup_tileset.
cleantilemap st.menubar, 256, 1
FOR i as integer = 0 TO 255
 writeblock st.menubar, i, 0, i
NEXT
st.zoneminimap = NULL

mapedit_loadmap st, map(), pass, emap, zmap, gmap(), visible(), doors(), link(), mapname

update_npc_graphics st, npc_img()

st.x = 0
st.y = 0
st.mapx = 0
st.mapy = 0
st.layer = 0
st.cur_zone = 1
st.cur_zinfo = GetZoneInfo(zmap, st.cur_zone)
st.clone_merge = YES

st.cur_door = find_first_free_door(doors())

DIM mapeditmenu(15) as string
DIM mapeditmenu_display(15) as string

mapeditmenu(0) = "Return to Map Menu"
mapeditmenu(1) = "Edit General Map Data..."
mapeditmenu(2) = "Resize Map..."
mapeditmenu(3) = "Layers and Tilesets..."
mapeditmenu(4) = "Edit NPCs..."
mapeditmenu(5) = "Edit Tilemap..."
mapeditmenu(6) = "Edit Wallmap..."
mapeditmenu(7) = "Place Doors..."
mapeditmenu(8) = "Place NPCs..."
mapeditmenu(9) = "Edit Foemap..."
mapeditmenu(10) = "Edit Zones..."
mapeditmenu(11) = "Link Doors..."
mapeditmenu(12) = "Erase Map Data"
mapeditmenu(13) = "Import/Export Tilemap..."
mapeditmenu(14) = "Re-load Default Passability"
mapeditmenu(15) = "Map name:"

DIM selectst as SelectTypeState
st.menustate.size = 24
st.menustate.last = UBOUND(mapeditmenu)
st.menustate.pt = remember_menu_pt  'preserved from any other maps for convenience
st.menustate.need_update = YES

setkeys YES
DO
 setwait 55
 setkeys YES
 IF keyval(scESC) > 1 THEN
  mapedit_savemap st, map(), pass, emap, zmap, gmap(), doors(), link(), mapname
  EXIT DO
 END IF
 IF keyval(scF1) > 1 THEN show_help "mapedit_menu"
 usemenu st.menustate
 IF st.menustate.pt = 15 AND selectst.query = "" THEN
  strgrabber mapname, 39
  st.menustate.need_update = YES
 ELSEIF select_by_typing(selectst) THEN
  select_on_word_boundary mapeditmenu(), selectst, st.menustate
 END IF

 IF enter_space_click(st.menustate) THEN
  SELECT CASE st.menustate.pt
   CASE 0
    mapedit_savemap st, map(), pass, emap, zmap, gmap(), doors(), link(), mapname
    EXIT DO
   CASE 1
    mapedit_gmapdata st, gmap(), zmap
   CASE 2
    mapedit_resize st, map(), pass, emap, zmap, gmap(), doors(), link(), mapname
   CASE 3
    mapedit_layers st, gmap(), visible(), map()
   CASE 4
    'This may change st.num_npc_defs, delete NPC instances, and write npc definitions to disk
    npcdef st, npc_img(), gmap(), zmap
   CASE 5 TO 10
    st.seteditmode = st.menustate.pt - 5
    GOSUB mapping
   CASE 11
    mapedit_savemap st, map(), pass, emap, zmap, gmap(), doors(), link(), mapname
    mapedit_linkdoors st, map(), pass, gmap(), doors(), link()
   CASE 12
    mapedit_delete st, map(), pass, emap, zmap, gmap(), doors(), link(), npc_img(), mapname
    IF st.mapnum > gen(genMaxMap) THEN
     'This was the last map, and it was deleted instead of blanked
     EXIT DO
    END IF
   CASE 13
    mapedit_import_export st, map(), pass, emap, zmap, gmap(), visible(), doors(), link(), mapname
   CASE 14
    '--reload default passability
    IF yesno("Set default passability for whole map, overwriting your wallmap? Don't worry, you can undo this by hitting Ctrl+Z in any editing mode", NO, NO) THEN
     FOR tx as integer = 0 TO st.wide - 1
      FOR ty as integer = 0 TO st.high - 1
       calculatepassblock st, tx, ty, map(), pass
      NEXT ty
     NEXT tx
    END IF
  END SELECT
  IF slave_channel <> NULL_CHANNEL THEN     'If live previewing, give quick feedback
   mapedit_savemap st, map(), pass, emap, zmap, gmap(), doors(), link(), mapname
  END IF
 END IF

 IF st.menustate.need_update THEN
  mapeditmenu(15) = "Map name:" + mapname
  st.menustate.need_update = NO
 END IF

 clearpage vpage
 highlight_menu_typing_selection mapeditmenu(), mapeditmenu_display(), selectst, st.menustate
 standardmenu mapeditmenu_display(), st.menustate, 0, 0, vpage
 setvispage vpage
 dowait
LOOP

'---------------------------------- CLEANUP CODE -------------------------------------

'Unload NPC graphics
FOR i as integer = 0 TO UBOUND(npc_img)
 WITH npc_img(i)
  IF .sprite THEN frame_unload(@.sprite)
  IF .pal THEN palette16_unload(@.pal)
 END WITH
NEXT i

unloadmaptilesets st.tilesets()
unloadtilemap st.menubar
unloadtilemaps map()
unloadtilemap pass
unloadtilemap emap
deletezonemap zmap
v_free st.history
IF st.secondary_undo_buffer THEN debugc errPromptBug, "mapedit cleanup: secondary_undo_buffer exists!"
v_free st.cloned
unloadtilemap st.zoneviewmap
unloadtilemap st.zoneoverlaymap
v_free st.defaultwalls
unload_sprite_and_pal st.cursor
unload_sprite_and_pal hero_gfx
frame_unload @zonetileset(0)
frame_unload @zonetileset(1)
frame_unload @zonetileset(2)
frame_unload @overlaytileset
frame_unload @st.zoneminimap
v_free mode_tools
v_free zonemenu

remember_menu_pt = st.menustate.pt  'preserve for other maps
EXIT SUB


'==========================================================================================
'                                   Map editor Proper
'==========================================================================================


mapping:
clearpage 2

st.reset_tool = YES
st.defpass = YES
IF readbit(gen(), genBits, 15) THEN st.defpass = NO ' option to default the defaults to OFF
st.autoshow_zones = YES
st.showzonehints = YES
zonemenustate.pt = -1  'Properly initialised in mapedit_update_visible_zones
st.zones_needupdate = YES
npczone_needupdate = YES
DIM tog as integer
DIM slowtog as integer
DIM chequer_scroll as integer

setkeys
DO
 setwait 55, 300
 setkeys
 tog = tog XOR 1
 chequer_scroll += 1
 gauze_ticker = (gauze_ticker + 1) MOD 50  '10 frames, 5 ticks a frame
 st.message_ticks = large(0, st.message_ticks - 1) 

 IF keyval(scESC) > 1 THEN EXIT DO
 IF keyval(scCtrl) = 0 AND keyval(scAlt) = 0 THEN
  FOR i as integer = tile_mode TO zone_mode
   IF keyval(scF2 + i) > 1 THEN st.seteditmode = i
  NEXT
 END IF

 ' This can be modified to override/augment the normal key for activating the current tool
 ' if it is a 'drawing' tool (Currently used just for wallmap bit keys)
 DIM use_draw_tool as bool = (keyval(scSpace) AND 4) > 0

 IF st.new_stroke = NO AND keyval(scSpace) = 0 THEN
  'Yes, a bit of a hack, not sure what a more rigourous test would be
  st.new_stroke = YES
 END IF

 IF st.seteditmode > -1 THEN
  st.editmode = st.seteditmode
  st.seteditmode = -1

  'Set available tools
  v_resize mode_tools, 0
  DIM tools_i as integer = 0
  WHILE mode_tools_map(st.editmode, tools_i) <> -1
   v_append mode_tools, mode_tools_map(st.editmode, tools_i)
   tools_i += 1
  WEND
  toolsbar_available = v_len(mode_tools) <> 0
  drawing_allowed = YES  'if there are any tools, that is

  st.brush = NULL
  st.reader = NULL
  SELECT CASE st.editmode
   CASE tile_mode
    st.brush = @tilebrush
    st.reader = @tilereader
   CASE pass_mode
    st.brush = @wallbitbrush
   CASE door_mode
   CASE npc_mode
    npczone_needupdate = YES
   CASE foe_mode
    st.brush = @foebrush
    st.reader = @foereader
   CASE zone_mode
    st.brush = @zonebrush
    st.reader = @zonereader
    IF st.zonesubmode = zone_view_mode THEN
     toolsbar_available = NO  'No normal tool switching in view mode
     st.tool = draw_tool
     drawing_allowed = NO
    END IF
    st.zones_needupdate = YES
  END SELECT

  'Reset tool
  IF v_len(mode_tools) = 0 THEN
   st.tool = -1  'None
  ELSEIF v_find(mode_tools, st.tool) = -1 THEN
   st.tool = mode_tools[0]
  END IF
  st.reset_tool = YES
  st.tool_hold = NO
 END IF

 IF keyval(scCtrl) > 0 AND keyval(scS) > 1 THEN
  'Instant save, mostly for live previewing, but maybe you're paranoid...
  mapedit_savemap st, map(), pass, emap, zmap, gmap(), doors(), link(), mapname
 END IF

 IF st.editmode = tile_mode OR st.tool = paint_tool THEN
  'Paint tool is affected by the current map layer, so that's important
  IF keyval(scPageup) > 1 ORELSE (keyval(scCTRL) > 0 ANDALSO keyval(scPeriod) > 1) THEN
   IF UBOUND(map) = 0 THEN
    st.message = "No more layers; press Ctrl+L to add one"
    st.message_ticks = 15
   END IF
   FOR i as integer = st.layer + 1 TO UBOUND(map)
    IF layerisenabled(gmap(), i) THEN
     st.layer = i
     update_tilepicker st
     EXIT FOR
    END IF
   NEXT i
  END IF
  IF keyval(scPageDown) > 1 ORELSE (keyval(scCTRL) > 0 ANDALSO keyval(scComma) > 1) THEN
   FOR i as integer = st.layer - 1 TO 0 STEP -1
    IF layerisenabled(gmap(), i) THEN
     st.layer = i
     update_tilepicker st
     EXIT FOR
    END IF
   NEXT
  END IF
 END IF

 IF keyval(scCtrl) > 0 AND keyval(scL) > 1 THEN
  mapedit_layers st, gmap(), visible(), map()  'ctrl-L
  update_tilepicker st
 END IF
 IF keyval(scTab) > 1 THEN st.tiny XOR= YES
 IF keyval(scTilde) > 1 AND keyval(scAlt) = 0 THEN show_minimap st, map()
 IF keyval(scCtrl) > 0 AND keyval(scBackspace) > 1 THEN
   'delete tile
   FOR i as integer = 0 TO UBOUND(map)
    tilebrush st, st.x, st.y, 0, i, map(), pass, emap, zmap
   NEXT i
   'delete passability
   wallbrush st, st.x, st.y, 0, , map(), pass, emap, zmap
   'delete foemap
   foebrush st, st.x, st.y, 0, , map(), pass, emap, zmap
   'delete NPC
   FOR i as integer = 0 TO 299
    WITH st.npc_inst(i)
     IF .id > 0 THEN
      IF .x = st.x * 20 AND .y = st.y * 20 THEN .id = 0
     END IF
    END WITH
   NEXT i
   'delete door
   doorid = find_door_at_spot(st.x, st.y, doors())
   IF doorid >= 0 THEN
    set_door_exists(doors(), doorid, NO)
   END IF
   'zones not deleted
 END IF
 IF keyval(scCtrl) > 0 AND keyval(scH) > 1 THEN 'Ctrl+H for hero start position
  gen(genStartMap) = st.mapnum
  gen(genStartX) = st.x
  gen(genStartY) = st.y
 END IF
 IF keyval(scCtrl) > 0 AND keyval(scD) > 1 THEN st.defpass = st.defpass XOR YES

 SELECT CASE st.editmode
  '---TILEMODE------
  CASE tile_mode
   IF keyval(scCtrl) = 0 AND keyval(scF1) > 1 THEN show_help "mapedit_tilemap"

   IF keyval(scEnter) > 1 THEN mapedit_pickblock st
   IF keyval(scG) > 1 THEN 'grab tile
    st.usetile(st.layer) = readblock(map(st.layer), st.x, st.y)
    update_tilepicker st
   END IF
   IF keyval(scCtrl) = 0 THEN
    IF keyval(scComma) > 1 AND st.usetile(st.layer) > 0 THEN
     st.usetile(st.layer) = st.usetile(st.layer) - 1
     update_tilepicker st
    END IF
    IF keyval(scPeriod) > 1 AND st.usetile(st.layer) < st.menubar.wide - 1 THEN
     st.usetile(st.layer) = st.usetile(st.layer) + 1
     update_tilepicker st
    END IF
   END IF
   st.tool_value = st.usetile(st.layer)

   IF keyval(scCtrl) > 0 AND keyval(scJ) > 1 THEN
    setbit jiggle(), 0, st.layer, (readbit(jiggle(), 0, st.layer) XOR 1)
   END IF
   FOR i as integer = 0 TO 1
    'Try to animate a tile
    IF keyval(sc1 + i) > 1 THEN 'animate tile
     DIM as integer oldtile, newtile
     newtile = -1
     oldtile = readblock(map(st.layer), st.x, st.y)
     'Returns -1 if can't be done
     newtile = tile_anim_animate_tile(oldtile, i, st.tilesets(st.layer)->tastuf())
     IF newtile = oldtile THEN
      'It was already animated with this pattern, so instead toggle to a non-animated tile
      newtile = tile_anim_deanimate_tile(oldtile, st.tilesets(st.layer)->tastuf())
     END IF
     IF newtile >= 0 THEN
      IF keyval(scCtrl) = 0 THEN
       tilebrush st, st.x, st.y, newtile, , map(), pass, emap, zmap
      ELSE
       'Like Replace tool, except that can't place animated tiles
       FOR tx as integer = 0 TO st.wide - 1
        FOR ty as integer = 0 TO st.high - 1
         IF readblock(map(st.layer), tx, ty) = oldtile THEN tilebrush st, tx, ty, newtile, , map(), pass, emap, zmap
        NEXT ty
       NEXT tx
      END IF
     END IF
    END IF
   NEXT i

   '#IFNDEF __UNIX__
    'common WM keys
    FOR i as integer = 0 TO UBOUND(map)
     IF keyval(scCtrl) > 0 AND keyval(scF1 + i) > 1 THEN
      clearkey(scF1 + i)
      IF layerisenabled(gmap(), i) THEN togglelayervisible(visible(), i)
     END IF
    NEXT
   '#ENDIF

   FOR i as integer = 1 TO small(maplayerMax, 10)
    IF keyval(scAlt) > 0 AND keyval(sc1 + (i - 1)) > 1 THEN
     clearkey(sc1 + i)
     togglelayerenabled(gmap(), i)
     IF layerisenabled(gmap(), i) THEN
      IF i > UBOUND(map) THEN
       DIM howmany as integer
       howmany = i - UBOUND(map)
       IF yesno("Layer " & i & " doesn't exist yet. Create " & iif_string(howmany = 1, "a new map layer?", howmany & " new map layers?")) THEN
        mapedit_append_new_layers st, map(), visible(), gmap(), howmany
       END IF
      END IF
     ELSE
      IF st.layer = i THEN
       DO UNTIL layerisenabled(gmap(), st.layer)
        st.layer -= 1
        update_tilepicker st
       LOOP
      END IF
     END IF
    END IF
   NEXT

   IF keyval(scAlt) > 0 AND keyval(scTilde) > 1 THEN
    togglelayervisible(visible(), st.layer)
   END IF

   '---PASSMODE-------
  CASE pass_mode
   IF keyval(scCtrl) = 0 AND keyval(scF1) > 1 THEN show_help "mapedit_wallmap"

   'st.tool_value is passmap mode is a little bit different; it's
   'determined on the fly by which key (space, H, etc) you press and
   'what's under the cursor. The only tool which doesn't have
   'st.tool_value determined on the fly is Ctrl+W, as well as holding
   'space down and drawing a line. That just remembers whatever the
   'last tool_value was, which is OK. +/- for changing this remembered
   'value is still supported though undocumented.
   IF st.reset_tool THEN st.tool_value = 15  'default
   IF keyval(scPlus) > 1 OR keyval(scMinus) > 1 THEN
    st.tool_value = IIF(st.tool_value, 0, 15)
   END IF

   DIM pass_overtile as integer = readblock(pass, st.x, st.y)
   IF keyval(scSpace) AND 4 THEN
    st.wallmap_mask = 255  'Remove all other bits
    IF (pass_overtile AND 15) = 0 THEN st.tool_value = 15
    IF (pass_overtile AND 15) = 15 THEN st.tool_value = 0
    IF (pass_overtile AND 15) > 0 AND (pass_overtile AND 15) < 15 THEN st.tool_value = 0
   END IF

   DIM wallbit as integer = 0
   IF keyval(scCtrl) > 0 THEN
    IF keyval(scUp) > 1    THEN wallbit = passNorthWall
    IF keyval(scRight) > 1 THEN wallbit = passEastWall
    IF keyval(scDown) > 1  THEN wallbit = passSouthWall
    IF keyval(scLeft) > 1  THEN wallbit = passWestWall
    IF keyval(scA) > 1 THEN wallbit = passVehA
    IF keyval(scB) > 1 THEN wallbit = passVehB
   ELSE
    IF keyval(scH) > 1 THEN wallbit = passHarm
    IF keyval(scO) > 1 THEN wallbit = passOverhead
   END IF
   IF wallbit THEN
    ' Examine the tile under the cursor, toggle the bit on that tile, and perform the
    ' same action on all other selected tiles
    st.wallmap_mask = wallbit
    st.tool_value = (pass_overtile AND wallbit) XOR wallbit
    use_draw_tool = YES
   END IF

   '---DOORMODE-----
  CASE door_mode
   IF keyval(scCtrl) = 0 AND keyval(scF1) > 1 THEN show_help "mapedit_door_placement"
   IF keyval(scEnter) > 1 THEN ' enter to link a door
    doorid = find_door_at_spot(st.x, st.y, doors())
    IF doorid >= 0 THEN
     'Save currently-worked-on map data
     mapedit_savemap st, map(), pass, emap, zmap, gmap(), doors(), link(), mapname
     doorlinkid = find_first_doorlink_by_door(doorid, link())
     IF doorlinkid >= 0 THEN
      link_one_door st, doorlinkid, link(), doors(), map(), pass, gmap()
     ELSE
      doorlinkid = find_last_used_doorlink(link()) + 1
      IF doorlinkid >= 0 AND doorlinkid <= UBOUND(link) THEN
       link(doorlinkid).source = doorid
       link_one_door st, doorlinkid, link(), doors(), map(), pass, gmap()
      END IF
     END IF
    END IF
   END IF
   IF intgrabber(st.cur_door, 0, UBOUND(doors), scLeftCaret, scRightCaret) THEN
    IF door_exists(doors(), st.cur_door) THEN
     st.x = doors(st.cur_door).x
     st.y = doors(st.cur_door).y - 1
     mapedit_focus_camera st, st.x, st.y
    END IF
   END IF
   IF keyval(scSpace) > 1 THEN ' space to place a door
    doorid = find_door_at_spot(st.x, st.y, doors())
    IF doorid >= 0 THEN
     'clear an existing door
     set_door_exists(doors(), doorid, NO)
    ELSE
     'Either move the current door if it exists, or create it
     doors(st.cur_door).x = st.x
     doors(st.cur_door).y = st.y + 1
     IF door_exists(doors(), st.cur_door) = NO THEN
      'If creating a new door, automatically get an unused door ID, ready for the next placement
      set_door_exists(doors(), st.cur_door, YES)
      st.cur_door = find_first_free_door(doors())
     END IF
    END IF
   END IF
   IF keyval(scDelete) > 1 THEN
    doorid = find_door_at_spot(st.x, st.y, doors())
    IF doorid >= 0 THEN
     set_door_exists(doors(), doorid, NO)
    END IF
   END IF

   '---NPCMODE------
  CASE npc_mode
   IF keyval(scCtrl) = 0 AND keyval(scF1) > 1 THEN show_help "mapedit_npc_placement"
   IF keyval(scDelete) > 1 THEN
    FOR i as integer = 0 TO 299
     WITH st.npc_inst(i)
      IF .id > 0 THEN
       IF .x = st.x * 20 AND .y = st.y * 20 THEN .id = 0
      END IF
     END WITH
    NEXT i
   END IF
   IF keyval(scEnter) > 1 THEN
    mapedit_list_npcs_by_tile st
   END IF
   st.npc_d = -1
   IF keyval(scCtrl) > 0 OR keyval(scSpace) > 1 THEN
    IF slowkey(scUp, 660)    THEN st.npc_d = 0
    IF slowkey(scRight, 660) THEN st.npc_d = 1
    IF slowkey(scDown, 660)  THEN st.npc_d = 2
    IF slowkey(scLeft, 660)  THEN st.npc_d = 3
   END IF
   IF keyval(scSpace) > 1 OR st.npc_d > -1 THEN
    temp = 0
    IF st.npc_d = -1 THEN
     DIM npci as integer = mapedit_npc_at_spot(st)
     IF npci > -1 THEN
      WITH st.npc_inst(npci)
       .id = 0
       .x = 0
       .y = 0
       .dir = 0
       temp = 1
      END WITH
     END IF
    END IF
    IF st.npc_d = -1 THEN st.npc_d = 2
    IF temp = 0 THEN
     temp = -1
     FOR i as integer = 299 TO 0 STEP -1
      IF st.npc_inst(i).id = 0 THEN temp = i
     NEXT i
     IF temp >= 0 THEN
      st.npc_inst(temp).x = st.x * 20
      st.npc_inst(temp).y = st.y * 20
      st.npc_inst(temp).id = st.cur_npc + 1
      st.npc_inst(temp).dir = st.npc_d
     END IF
    END IF
   END IF
   intgrabber(st.cur_npc, 0, st.num_npc_defs - 1, scLeftCaret, scRightCaret)

   '---FOEMODE--------
  CASE foe_mode
   IF keyval(scCtrl) = 0 AND keyval(scF1) > 1 THEN show_help "mapedit_foemap"
   intgrabber(st.cur_foe, 0, 255, scLeftCaret, scRightCaret)
   IF keyval(scG) > 1 THEN st.cur_foe = readblock(emap, st.x, st.y)
   st.tool_value = st.cur_foe

   '---ZONEMODE--------
  CASE zone_mode
   IF keyval(scCtrl) = 0 AND keyval(scF1) > 1 THEN
    IF st.zonesubmode THEN show_help "mapedit_zonemap_view" ELSE show_help "mapedit_zonemap_edit"
   END IF
   IF keyval(scCtrl) = 0 THEN
    IF keyval(scZ) > 1 THEN
     st.zonesubmode = st.zonesubmode XOR 1
     toolsbar_available = (st.zonesubmode = zone_edit_mode)
     drawing_allowed = (st.zonesubmode = zone_edit_mode)
     IF st.zonesubmode = zone_view_mode THEN st.tool = draw_tool
     st.zones_needupdate = YES
    END IF
    IF keyval(scE) > 1 THEN
     mapedit_edit_zoneinfo st, zmap
     st.zones_needupdate = YES  'st.cur_zone might change, amongst other things
    END IF
   END IF
   IF st.reset_tool THEN st.tool_value = YES
   IF st.tool = draw_tool ANDALSO (keyval(scSpace) AND 4) THEN 'drawing, new keypress: pick value intelligently
    st.tool_value = CheckZoneAtTile(zmap, st.cur_zone, st.x, st.y) XOR YES
   END IF
   IF st.tool <> draw_tool ANDALSO (keyval(scPlus) > 1 OR keyval(scMinus) > 1) THEN
    st.tool_value XOR= YES
   END IF
   IF st.zonesubmode = zone_edit_mode THEN

    '--Tiling/editing mode
    st.zones_needupdate OR= intgrabber(st.cur_zone, 1, 9999, scLeftCaret, scRightCaret)
    IF st.tool <> paint_tool THEN
     'Using the paint tool enables pageup/pagedown for changing selected map layer
     st.zones_needupdate OR= keygrabber(st.cur_zone, 1, 9999, scPageDown, scPageUp)
    END IF
    st.cur_zinfo = GetZoneInfo(zmap, st.cur_zone)
    IF keyval(scQ) > 1 AND keyval(scCtrl) > 0 THEN
     DebugZoneMap zmap, st.x, st.y
     ''paint a whole lot of tiles over the map randomly
     'mapedit_zonespam st, map(), pass, emap, zmap
     'st.zones_needupdate = YES
    END IF

   ELSE
    '--Multizone view
    usemenu zonemenustate, cast(BasicMenuItem vector, zonemenu), scLeftCaret, scRightCaret
    IF zonemenustate.pt > -1 THEN
     st.cur_zone = zonemenu[zonemenustate.pt].dat
     st.cur_zinfo = GetZoneInfo(zmap, st.cur_zone)
     IF keyval(scL) > 1 THEN  'Lock/Unlock
      IF int_array_find(lockedzonelist(), st.cur_zone) > -1 THEN
       int_array_remove(lockedzonelist(), st.cur_zone)
      ELSEIF UBOUND(lockedzonelist) + 1 < 8 THEN
       int_array_append(lockedzonelist(), st.cur_zone)
       st.cur_zinfo->hidden = NO  'Doesn't make sense for a zone to be hidden and locked
      END IF
      st.zones_needupdate = YES
     END IF
     IF keyval(scH) > 1 THEN
      st.cur_zinfo->hidden XOR= YES
      int_array_remove(lockedzonelist(), st.cur_zone)  'Doesn't make sense for a zone to be hidden and locked
      st.zones_needupdate = YES
     END IF
    END IF
    'You may draw if you lock the zone first to avoid weird graphical glitches
    drawing_allowed = (int_array_find(lockedzonelist(), st.cur_zone) > -1)
    IF keyval(scA) > 1 THEN  'Autoshow zones
     st.autoshow_zones XOR= YES
     st.zones_needupdate = YES
    END IF
    IF keyval(scS) > 1 THEN  'Show other zones
     st.showzonehints XOR= YES
    END IF
    IF keyval(scT) > 1 THEN  'Let the user choose the tileset used to display zones in multi-view
     st.zoneviewtileset = (st.zoneviewtileset + 1) MOD 3
    END IF
   END IF
   '--done input-modes-------
 END SELECT
 st.reset_tool = NO   'The above SELECT block is responsible for doing resetting

 'Size of the map viewport
 DIM mapviewsize as XYPair
 mapviewsize.w = vpages(dpage)->w
 mapviewsize.h = vpages(dpage)->h - 20  '20 pixels for menubar

 'Position of the map viewport
 DIM maprect as RectPoints
 maprect.p1 = Type(0, 20)
 maprect.p2 = Type(vpages(dpage)->w, vpages(dpage)->h)

 '--general purpose controls----
 st.oldpos.x = st.x
 st.oldpos.y = st.y
 IF keyval(scLeftShift) > 0 OR keyval(scRightShift) > 0 THEN
  st.rate.x = 8
  st.rate.y = 5
 ELSE
  st.rate.x = 1
  st.rate.y = 1
 END IF
 IF keyval(scAlt) = 0 AND keyval(scCtrl) = 0 THEN
  'Move cursor position
  IF slowkey(scUp, 110) THEN st.y = large(st.y - st.rate.y, 0)
  IF slowkey(scDown, 110) THEN st.y = small(st.y + st.rate.y, st.high - 1)
  IF slowkey(scLeft, 110) THEN st.x = large(st.x - st.rate.x, 0)
  IF slowkey(scRight, 110) THEN st.x = small(st.x + st.rate.x, st.wide - 1)
  st.mapx = bound(st.mapx, (st.x + 1) * 20 - mapviewsize.w, st.x * 20)
  st.mapy = bound(st.mapy, (st.y + 1) * 20 - mapviewsize.h, st.y * 20)
 END IF
 IF keyval(scAlt) > 0 AND keyval(scCtrl) = 0 THEN
  'Move camera position
  st.oldrel.x = st.x - st.mapx / 20
  st.oldrel.y = st.y - st.mapy / 20
  IF slowkey(scUp, 110) THEN st.mapy = large(st.mapy - 20 * st.rate.y, 0)
  IF slowkey(scDown, 110) THEN st.mapy = small(st.mapy + 20 * st.rate.y, st.high * 20 - mapviewsize.h)
  IF slowkey(scLeft, 110) THEN st.mapx = large(st.mapx - 20 * st.rate.x, 0)
  IF slowkey(scRight, 110) THEN st.mapx = small(st.mapx + 20 * st.rate.x, st.wide * 20 - mapviewsize.w)
  st.mapx = large(0, st.mapx)
  st.mapy = large(0, st.mapy)
  st.x = st.mapx / 20 + st.oldrel.x
  st.y = st.mapy / 20 + st.oldrel.y
 END IF
 st.moved = (st.oldpos.x <> st.x OR st.oldpos.y <> st.y)

 ' 'Actual size of the map viewport showing the map
 ' '(Since the window can be larger than the map, have to clamp it)
 ' DIM maprect as RectType
 ' maprect.topleft = Type(0, 20)
 ' maprect.wide = small(st.wide * 20 - st.mapx, mapviewsize.w)  
 ' maprect.high = small(st.high * 20 - st.mapy, mapviewsize.h)

 '--Tools
 IF drawing_allowed AND v_len(mode_tools) > 0 THEN
  '--Select tool
  IF toolsbar_available THEN
   FOR i as integer = 0 TO v_len(mode_tools) - 1
    IF keyval(scCtrl) = 0 AND keyval(toolinfo(mode_tools[i]).shortcut) > 1 THEN
     st.tool = mode_tools[i]
     st.reset_tool = YES
     st.tool_hold = NO
    END IF
   NEXT
  END IF

  'These two are basically tools

  IF keyval(scDelete) > 0 THEN
   st.wallmap_mask = 255   'Special case needed for wallmap mode
   st.brush(st, st.x, st.y, 0, , map(), pass, emap, zmap)
  END IF

  IF keyval(scW) > 1 AND keyval(scCtrl) > 0 THEN  'Ctrl+W  Paint the window/screen
   FOR tx as integer = 0 TO 15
    FOR ty as integer = 0 TO 8
     st.brush(st, st.mapx \ 20 + tx, st.mapy \ 20 + ty, st.tool_value, , map(), pass, emap, zmap)
    NEXT ty
   NEXT tx
  END IF

  SELECT CASE st.tool
   CASE draw_tool
    'IF keyval(scSpace) > 0 AND (st.new_stroke OR st.moved) THEN
    IF use_draw_tool OR (keyval(scSpace) > 0 AND st.moved) THEN
     '(No need to reapply brush until the cursor moves)
     st.brush(st, st.x, st.y, st.tool_value, , map(), pass, emap, zmap)
    END IF

   CASE box_tool
    IF st.tool_hold THEN
     IF use_draw_tool THEN
      'We have two corners
      st.tool_hold = NO
      FOR tx as integer = small(st.tool_hold_pos.x, st.x) TO large(st.tool_hold_pos.x, st.x)
       FOR ty as integer = small(st.tool_hold_pos.y, st.y) TO large(st.tool_hold_pos.y, st.y)
        st.brush(st, tx, ty, st.tool_value, , map(), pass, emap, zmap)
       NEXT
      NEXT
     END IF
    ELSE
     IF keyval(scSpace) AND 4 THEN  'new keypress
      st.tool_hold = YES
      st.tool_hold_pos = TYPE(st.x, st.y)
     END IF
    END IF

   CASE fill_tool
    IF use_draw_tool THEN
     fill_map_area st, st.x, st.y, map(), pass, emap, zmap, st.reader
    END IF

   CASE paint_tool
    IF use_draw_tool THEN
     fill_with_other_area st, st.x, st.y, map(), pass, emap, zmap, @tilereader
    END IF

   CASE replace_tool
    IF use_draw_tool THEN
     DIM replace_old as integer
     replace_old = st.reader(st, st.x, st.y, , map(), pass, emap, zmap)
     FOR ty as integer = 0 to st.high - 1
      FOR tx as integer = 0 to st.wide - 1
       IF st.reader(st, tx, ty, , map(), pass, emap, zmap) = replace_old THEN
        st.brush(st, tx, ty, st.tool_value, , map(), pass, emap, zmap)
       END IF
      NEXT tx
     NEXT ty
    END IF

   CASE mark_tool
    IF keyval(scSpace) AND 4 THEN  'new keypress
     IF st.tool_hold THEN
      'We have two corners
      st.tool_hold = NO
      DIM select_rect as RectType
      corners_to_rect_inclusive TYPE(st.x, st.y), st.tool_hold_pos, select_rect

      v_free st.cloned
      st.cloned = create_changelist(st, map(), pass, emap, zmap, visible(), gmap(), select_rect)
      st.clone_offset.x = select_rect.wide \ 2
      st.clone_offset.y = select_rect.high \ 2
      st.clone_size.w = select_rect.wide
      st.clone_size.h = select_rect.high
      st.tool = clone_tool
      st.multitile_draw_brush = NO  'Normal clone tool behaviour
     ELSE
      st.tool_hold = YES
      st.tool_hold_pos = TYPE(st.x, st.y)
     END IF
    END IF

   CASE clone_tool
    IF st.cloned = NULL THEN
     st.tool = mark_tool
    ELSE
     IF use_draw_tool OR (keyval(scSpace) > 0 AND st.moved) THEN
      apply_changelist st, map(), pass, emap, zmap, visible(), gmap(), st.cloned, TYPE(st.x - st.clone_offset.x, st.y - st.clone_offset.y)
     END IF
     IF keyval(scCtrl) > 0 AND keyval(scM) > 1 THEN
      st.clone_merge XOR= YES
     END IF
    END IF

  END SELECT
 END IF

 '--Undo/Redo
 'IF v_len(mode_tools) THEN
 DIM stroke as MapEditUndoTile vector = NULL
 IF keyval(scCtrl) > 0 AND keyval(scZ) > 1 THEN
  stroke = undo_stroke(st, map(), pass, emap, zmap)
 END IF
 IF keyval(scCtrl) > 0 AND keyval(scY) > 1 THEN
  stroke = redo_stroke(st, map(), pass, emap, zmap)
 END IF
 IF stroke THEN mapedit_show_undo_change st, stroke
 'END IF

 '--Apply temporary previews to the map
 '(Creating the secondary undo buffer causes edits to be undoable with undo_preview)
 IF st.secondary_undo_buffer THEN debugc errPromptBug, "mapedit preview: secondary_undo_buffer already exists!"
 IF st.tool = clone_tool AND st.cloned <> NULL THEN
  v_new st.secondary_undo_buffer
  apply_changelist st, map(), pass, emap, zmap, visible(), gmap(), st.cloned, TYPE(st.x - st.clone_offset.x, st.y - st.clone_offset.y)
 END IF
 IF st.editmode = tile_mode AND st.tool = draw_tool THEN
  IF slowtog AND keyval(scSpace) = 0 THEN
   v_new st.secondary_undo_buffer
   tilebrush(st, st.x, st.y, st.tool_value, , map(), pass, emap, zmap)
  END IF
 END IF

 'NOTE: There should be no use of brushes below this point!!

 '--Zones update logic, here because it needs access to 'moved'
 IF st.editmode = zone_mode THEN
  IF st.zonesubmode = zone_edit_mode THEN
   IF st.zones_needupdate THEN
    CleanTilemap st.zoneoverlaymap, st.wide, st.high
    ZoneToTilemap zmap, st.zoneoverlaymap, st.cur_zone, 0
   END IF
  ELSE
   IF st.zones_needupdate OR st.moved THEN
    'Rebuilds zonemenu and st.zoneviewmap based on selected tile and lockedzonelist() 
    mapedit_update_visible_zones st, zonemenu, zonemenustate, zmap, lockedzonelist()
   END IF
  END IF

  'Generate minimap
  IF st.zonesubmode = zone_edit_mode THEN
   draw_zone_minimap st, st.zoneoverlaymap, 0, uilook(uiGold)
  ELSE
   DIM bitnum as integer = int_array_find(st.zonecolours(), st.cur_zone)
   IF bitnum <> -1 THEN
    draw_zone_minimap st, st.zoneviewmap, bitnum, uilook(uiGold)
   END IF
  END IF

  st.zones_needupdate = NO
 END IF

 st.last_pos = TYPE(st.x, st.y)

 '--Draw Screen
 clearpage dpage

 '--draw map
 animatetilesets st.tilesets()
 FOR i as integer = 0 TO UBOUND(map)
  IF layerisvisible(visible(), i) AND layerisenabled(gmap(), i) THEN
   st.jig.x = 0
   st.jig.y = 0
   IF readbit(jiggle(), 0, i) AND tog THEN
    st.jig.x = 0
    IF (i mod 8) >= 1 AND (i mod 8) <= 3 THEN st.jig.x = 1
    IF (i mod 8) >= 5 THEN st.jig.x = -1
    st.jig.y = 0
    IF (i mod 8) <= 1 OR (i mod 8) = 7 THEN st.jig.y = -1
    IF (i mod 8) >= 3 AND (i mod 8) <= 5 THEN st.jig.y = 1
    st.jig.x *= i \ 8 + 1
    st.jig.y *= i \ 8 + 1
   END IF
   drawmap map(i), st.mapx + st.jig.x, st.mapy + st.jig.y, st.tilesets(i), dpage, iif(i = 0, 0, 1), iif(i = 0, 1, 0), @pass, 20
  END IF
 NEXT
 IF layerisvisible(visible(), 0) AND layerisenabled(gmap(), 0) THEN
  IF readbit(jiggle(), 0, 0) AND tog THEN
   drawmap map(0), st.mapx, st.mapy - 1, st.tilesets(0), dpage, 0, 2, @pass, 20
  ELSE
   drawmap map(0), st.mapx, st.mapy, st.tilesets(0), dpage, 0, 2, @pass, 20
  END IF
 END IF

 '--hero start location display--
 IF gen(genStartMap) = st.mapnum THEN
  IF gen(genStartX) >= st.mapx \ 20 AND gen(genStartX) <= (st.mapx + mapviewsize.w) \ 20 AND gen(genStartY) >= st.mapy \ 20 AND gen(genStartY) <= (st.mapy + mapviewsize.h) \ 20 THEN
   frame_draw hero_gfx.sprite + 4, hero_gfx.pal, gen(genStartX) * 20 - st.mapx, gen(genStartY) * 20 + 20 - st.mapy, , , dpage
   textcolor uilook(uiText), 0
   printstr "Hero", gen(genStartX) * 20 - st.mapx, gen(genStartY) * 20 + 30 - st.mapy, dpage
  END IF
 END IF

 '--point out overhead tiles so that you can see what's wrong if you accidentally use them
 IF st.editmode = tile_mode AND UBOUND(map) > 0 THEN
  textcolor uilook(uiSelectedItem + tog), 0
  FOR o as integer = 0 TO mapviewsize.h \ 20 + 1
   FOR i as integer = 0 TO mapviewsize.w \ 20 + 1
    IF (st.mapx \ 20) + i < st.wide ANDALSO (st.mapy \ 20) + o < st.high THEN
     DIM pass_overtile as integer = readblock(pass, (st.mapx \ 20) + i, (st.mapy \ 20) + o)
     DIM pixelx as integer = (st.mapx \ 20 + i) * 20 - st.mapx
     DIM pixely as integer = (st.mapy \ 20 + o) * 20 - st.mapy
     IF (pass_overtile AND passOverhead) THEN printstr "O", pixelx + 10, pixely + 30, dpage
    END IF
   NEXT i
  NEXT o
 END IF

 '--show passmode
 IF st.editmode = pass_mode THEN
  FOR o as integer = 0 TO mapviewsize.h \ 20 + 1
   FOR i as integer = 0 TO mapviewsize.w \ 20 + 1
    IF (st.mapx \ 20) + i < st.wide ANDALSO (st.mapy \ 20) + o < st.high THEN
     DIM pass_overtile as integer = readblock(pass, (st.mapx \ 20) + i, (st.mapy \ 20) + o)
     DIM pixelx as integer = (st.mapx \ 20 + i) * 20 - st.mapx
     DIM pixely as integer = (st.mapy \ 20 + o) * 20 - st.mapy
     IF (pass_overtile AND passNorthWall) THEN rectangle pixelx     , pixely + 20, 20, 3, uilook(uiMenuItem + tog), dpage
     IF (pass_overtile AND passEastWall)  THEN rectangle pixelx + 17, pixely + 20, 3, 20, uilook(uiMenuItem + tog), dpage
     IF (pass_overtile AND passSouthWall) THEN rectangle pixelx     , pixely + 37, 20, 3, uilook(uiMenuItem + tog), dpage
     IF (pass_overtile AND passWestWall)  THEN rectangle pixelx     , pixely + 20, 3, 20, uilook(uiMenuItem + tog), dpage
     textcolor uilook(uiSelectedItem + tog), 0
     IF (pass_overtile AND passVehA) THEN printstr "A", pixelx, pixely + 20, dpage
     IF (pass_overtile AND passVehB) THEN printstr "B", pixelx + 10, pixely + 20, dpage
     IF (pass_overtile AND passHarm) THEN printstr "H", pixelx, pixely + 30, dpage
     IF (pass_overtile AND passOverhead) THEN printstr "O", pixelx + 10, pixely + 30, dpage
    END IF
   NEXT i
  NEXT o
 END IF
 
 '--door display--
 IF st.editmode = door_mode THEN
  textcolor uilook(uiBackground), 0
  FOR i as integer = 0 TO 99
   IF doors(i).x >= st.mapx \ 20 AND doors(i).x <= (st.mapx + mapviewsize.w) \ 20 AND doors(i).y > st.mapy \ 20 AND doors(i).y <= (st.mapy + mapviewsize.h) \ 20 + 1 AND door_exists(doors(), i) THEN
    rectangle doors(i).x * 20 - st.mapx, doors(i).y * 20 - st.mapy, 20, 20, uilook(uiSelectedItem + tog), dpage
    printstr STR(i), doors(i).x * 20 - st.mapx + 10 - (4 * LEN(STR(i))), doors(i).y * 20 - st.mapy + 6, dpage
   END IF
  NEXT
 END IF

 '--npc display--
 IF st.editmode = npc_mode THEN
  '--Determine restriction zone to display (Ugh this is pretty ugly)
  DIM oldzone as integer = st.cur_npc_zone
  DIM oldwallzone as integer = st.cur_npc_wall_zone
  st.cur_npc_zone = 0
  st.cur_npc_wall_zone = 0
  DIM npci as integer = mapedit_npc_at_spot(st)
  IF npci > -1 THEN
   WITH st.npc_def(st.npc_inst(npci).id - 1)
    IF .defaultzone = -1 THEN
     st.cur_npc_zone = 0
    ELSEIF .defaultzone = 0 THEN
     st.cur_npc_zone = gmap(32)
    ELSE
     st.cur_npc_zone = .defaultzone
    END IF
    IF .defaultwallzone = -1 THEN
     st.cur_npc_wall_zone = 0
    ELSEIF .defaultwallzone = 0 THEN
     st.cur_npc_wall_zone = gmap(33)
    ELSE
     st.cur_npc_wall_zone = .defaultwallzone
    END IF
   END WITH
  END IF
  IF oldzone <> st.cur_npc_zone OR oldwallzone <> st.cur_npc_wall_zone OR npczone_needupdate THEN
   CleanTilemap st.zoneoverlaymap, st.wide, st.high
   IF st.cur_npc_zone > 0 THEN
    ZoneToTilemap zmap, st.zoneoverlaymap, st.cur_npc_zone, 0
   END IF
   IF st.cur_npc_wall_zone > 0 THEN
    ZoneToTilemap zmap, st.zoneoverlaymap, st.cur_npc_wall_zone, 1
   END IF
   npczone_needupdate = NO
   'We're reusing st.zoneoverlaymap
   st.zones_needupdate = YES
  END IF
  '--Draw NPC zones
  drawmap st.zoneoverlaymap, st.mapx, st.mapy, overlaytileset, dpage, YES, , , 20

  '--Draw npcs
  FOR i as integer = 0 TO UBOUND(npcnum)
   npcnum(i) = 0
  NEXT
  st.walk = (st.walk + 1) MOD 4
  FOR i as integer = 0 TO 299
   WITH st.npc_inst(i)
    IF .id > 0 THEN
     ' +/-20 Y must be for the footoffset
     IF .x >= st.mapx AND .x < st.mapx + mapviewsize.w AND .y >= st.mapy - 20 AND .y < st.mapy + mapviewsize.h + 20 THEN
      DIM image as GraphicPair = npc_img(.id - 1)
      frame_draw image.sprite + (2 * .dir) + st.walk \ 2, image.pal, .x - st.mapx, .y + 20 - st.mapy + gmap(11), 1, -1, dpage
      textcolor uilook(uiSelectedItem + tog), 0
      printstr STR(.id - 1), .x - st.mapx, .y + 20 - st.mapy + 3, dpage
      printstr STR(npcnum(.id - 1)), .x - st.mapx, .y + 20 - st.mapy + 12, dpage
     END IF
     npcnum(.id - 1) += 1
    END IF
   END WITH
  NEXT
 END IF

 '--show foemap--
 IF st.editmode = foe_mode THEN
  textcolor uilook(uiSelectedItem + tog), 0
  FOR i as integer = 0 TO mapviewsize.w \ 20 + 1
   FOR o as integer = 0 TO mapviewsize.h \ 20 + 1
    IF (st.mapx \ 20) + i < st.wide ANDALSO (st.mapy \ 20) + o < st.high THEN
     temp = readblock(emap, st.mapx \ 20 + i, st.mapy \ 20 + o)
     DIM pixelx as integer = (st.mapx \ 20 + i) * 20 - st.mapx
     DIM pixely as integer = (st.mapy \ 20 + o) * 20 - st.mapy
     IF temp > 0 THEN printstr STR(temp), pixelx - ((temp < 10) * 5), pixely + 26, dpage
    END IF
   NEXT o
  NEXT i
 END IF

 '--show zones
 IF st.editmode = zone_mode THEN
  IF st.zonesubmode = zone_edit_mode THEN
   'Draw a single zone
   drawmap st.zoneoverlaymap, st.mapx, st.mapy, overlaytileset, dpage, YES, , , 20
  ELSE
   'Draw all zones on this tile
   drawmap st.zoneviewmap, st.mapx, st.mapy, zonetileset(st.zoneviewtileset), dpage, YES, , , 20, , YES
   IF st.showzonehints THEN
    'Overlay 'hints' at hidden zones
    setanim ABS(gauze_ticker \ 5 - 4), 0
    drawmap st.zoneoverlaymap, st.mapx, st.mapy, overlaytileset, dpage, YES, , , 20
   END IF
  END IF
 END IF
 
 '--tools overlays
 SELECT CASE st.tool
  CASE box_tool, mark_tool
   IF st.tool_hold THEN
    'Just draw a cheap rectangle on the screen, because I'm lazy. Drawing something different
    'for different brushes is non-trivial, and besides, how should layers work?
    DIM as RectType select_rect
    corners_to_rect_inclusive TYPE(st.x, st.y), st.tool_hold_pos, select_rect
    drawbox select_rect.x * 20 - st.mapx, select_rect.y * 20 - st.mapy + 20, _
            select_rect.wide * 20, select_rect.high * 20, _
            uilook(uiHighlight + tog), 4, dpage
   END IF

  CASE clone_tool
   DIM as RectType clone_box
   clone_box = TYPE(st.x - st.clone_offset.x, st.y - st.clone_offset.y, st.clone_size.w, st.clone_size.h)
   drawbox clone_box.x * 20 - st.mapx, clone_box.y * 20 - st.mapy + 20, _
           clone_box.wide * 20, clone_box.high * 20, _
           uilook(uiHighlight + tog), 1, dpage
 END SELECT


 '--Draw menubar (includes tileset preview)
 IF st.editmode = tile_mode THEN
  'This is to draw tile 0 as fully transparent on layer > 0
  st.menubar.layernum = st.layer
  draw_background 0, 0, vpages(dpage)->w - 40, 20, IIF(st.layer > 0, -1, 0), chequer_scroll, vpages(dpage)
  drawmap st.menubar, st.menubarstart(st.layer) * 20, 0, st.tilesets(st.layer), dpage, YES, , , 0, 20
  'Don't show (black out) the last two tiles on the menubar, because they
  'are overlaid too much by the icons.
  rectangle vpages(dpage)->w - 40, 0, 40, 20, uilook(uiBackground), dpage
 ELSE
  rectangle 0, 0, vpages(dpage)->w, 20, uilook(uiBackground), dpage
 END IF
 rectangle 0, 19, vpages(dpage)->w, 1, uilook(uiText), dpage

 '--position finder--
 IF st.tiny THEN
  fuzzyrect 0, 35, st.wide, st.high, uilook(uiHighlight), dpage
  rectangle st.mapx \ 20, (st.mapy \ 20) + 35, 16, 9, uilook(uiDescription), dpage
  IF st.editmode = zone_mode THEN
   frame_draw st.zoneminimap, NULL, 0, 35, , , dpage
  END IF
 END IF
 
 '--normal cursor--
 IF st.editmode <> npc_mode AND st.tool <> clone_tool THEN
  frame_draw st.cursor.sprite + tog, st.cursor.pal, (st.x * 20) - st.mapx, (st.y * 20) - st.mapy + 20, , , dpage
 END IF
 '--menubar cursor
 IF st.editmode = tile_mode THEN
  frame_draw st.cursor.sprite + tog, st.cursor.pal, ((st.usetile(st.layer) - st.menubarstart(st.layer)) * 20), 0, , , dpage
 END IF
 
 '--npc placement cursor--
 IF st.editmode = npc_mode THEN
  WITH npc_img(st.cur_npc)
   frame_draw .sprite + (2 * st.walk), .pal, st.x * 20 - st.mapx, st.y * 20 - st.mapy + 20 + gmap(11), 1, -1, dpage
  END WITH
  textcolor uilook(uiSelectedItem + tog), 0
  printstr STR(st.cur_npc), (st.x * 20) - st.mapx, (st.y * 20) - st.mapy + 28, dpage
  edgeprint npc_preview_text(st.npc_def(st.cur_npc)), 0, 0, uilook(uiText), dpage
  edgeprint mapedit_npc_instance_count(st, st.cur_npc) & " copies of NPC " & st.cur_npc & " on this map", 0, 10, uilook(uiText), dpage
 END IF
 
 textcolor uilook(uiSelectedItem + tog), 0 
 printstr "X " & st.x & "   Y " & st.y, 0, maprect.p2.y - 8, dpage
 textcolor uilook(uiText), 0
 printstr modenames(st.editmode), 0, 24, dpage

 '--Tool selection
 IF toolsbar_available THEN
  DIM toolbarpos as XYPair = TYPE(320 - 10 * v_len(mode_tools), 0)
  IF st.editmode = tile_mode THEN
   toolbarpos.y = 12
  END IF
  rectangle toolbarpos.x, toolbarpos.y, 10 * v_len(mode_tools), 8, uilook(uiBackground), dpage
  FOR i as integer = 0 TO v_len(mode_tools) - 1
   mapedit_draw_icon st, toolinfo(mode_tools[i]).icon, toolbarpos.x + i * 10, toolbarpos.y, (st.tool = mode_tools[i])
  NEXT
  DIM tmpstr as string
  IF st.tool = paint_tool THEN
   'Show the current layer if using the paint tool, since that depends on selected tilemap layer
   tmpstr = "Tool:Paint on " & hilite("Layer " & st.layer)
  ELSE
   tmpstr = "Tool: " & toolinfo(st.tool).name
  END IF
  textcolor uilook(uiText), 0 
  printstr tmpstr, xstring(tmpstr, toolbarpos.x, YES), toolbarpos.y + 10, dpage, YES
 ELSEIF st.editmode = zone_mode AND st.zonesubmode = zone_view_mode AND drawing_allowed THEN
  'Nasty
  textcolor uilook(uiText), 0 
  printstr "Tool: Draw", 320 - 81, 22, dpage
 END IF

 IF st.editmode = tile_mode THEN
  IF st.tool = mark_tool OR (st.tool = clone_tool AND st.multitile_draw_brush = NO) THEN
   'Hint that the current layer doesn't matter
   textcolor uilook(uiText), 0
  ELSE
   textcolor uilook(uiSelectedItem + tog), 0
  END IF
  DIM layername as string
  layername = "Layer " & st.layer
  IF layerisvisible(visible(), st.layer) = NO THEN layername &= " (invisible)"
  layername &= " " & read_map_layer_name(gmap(), st.layer)
  layername = RIGHT(layername, 40)
  printstr layername, 0, maprect.p2.y - 20, dpage
 END IF

 IF st.editmode = tile_mode OR st.tool = clone_tool THEN
  'Also show the default walls option when using clone tool as it is affected by them
  textcolor uilook(uiText), 0
  DIM defpass_msg as string = hilite("Ctrl+D: ")
  IF st.defpass = NO THEN defpass_msg &= "No "
  defpass_msg &= "Default Walls"
  printstr defpass_msg, maprect.p2.x - 196, maprect.p2.y - 8, dpage, YES
 END IF

 IF st.tool = clone_tool THEN
  textcolor uilook(uiText), 0
  printstr hilite("Ctrl+M: ") & iif_string(st.clone_merge, "Tile Merging On", "Tile Merging Off"), maprect.p2.x - 196, maprect.p2.y - 16, dpage, YES
 END IF

 IF st.editmode = door_mode THEN
  textcolor uilook(uiText), uilook(uiHighlight)
  printstr "Placing Door: " & st.cur_door, 0, 16, dpage
 END IF

 IF st.editmode = foe_mode THEN
  textcolor uilook(uiText), uilook(uiHighlight)
  printstr "Formation Set: " & st.cur_foe, 0, 16, dpage
 END IF

 IF st.editmode = zone_mode THEN
  DIM zoneselected as integer = YES
  textcolor uilook(uiText), 0
  IF st.zonesubmode = zone_edit_mode THEN
   SELECT CASE st.tool
    CASE draw_tool, mark_tool, clone_tool
    CASE ELSE
     printstr hilite("+") + "/" + hilite("-") + iif_string(st.tool_value, ": Adding tiles", ": Removing tiles"), 10, 6, dpage, YES
   END SELECT

   printstr "(" + hilite("Z") + ": Editing)", 140, 24, dpage, YES
  ELSE
   printstr "(" + hilite("Z") + ": Viewing)", 140, 24, dpage, YES
   IF zonemenustate.pt = -1 THEN zoneselected = NO
  END IF

  IF zoneselected THEN
   printstr hilite("Zone " & st.cur_zone) & " (" & st.cur_zinfo->numtiles & " tiles) " & st.cur_zinfo->name, 0, vpages(dpage)->h - 20, dpage, YES
  END IF

  IF st.zonesubmode = zone_edit_mode THEN
   '-- Edit mode

   IF st.tool <> clone_tool THEN
    'Don't overdraw "Default Walls"
    printstr hilite("E") + "dit zone info", 116, vpages(dpage)->h - 8, dpage, YES
   END IF

  ELSE
   '-- View mode

   printstr iif_string(st.autoshow_zones,"      ","Don't ") & hilite("A") + "utoshow zones  " _
            & iif_string(st.showzonehints,"      ","Don't ") & hilite("S") + "how other", 0, 5, dpage, YES

   IF zoneselected THEN
    DIM is_locked as integer = (int_array_find(lockedzonelist(), st.cur_zone) > -1)
    printstr hilite("E") + "dit/" _
             & iif_string(st.cur_zinfo->hidden,"un","") + hilite("H") + "ide/" _
             & iif_string(is_locked,"un","") + hilite("L") + "ock zone", 320 - 25*8, vpages(dpage)->h - 8, dpage, YES
   END IF

   'Draw zonemenu
   DIM pixel_width as integer = vpages(dpage)->w \ 3
   ' Where to put the menu
   DIM xpos as integer
   xpos = vpages(dpage)->w - pixel_width
   IF (st.x * 20) - st.mapx > xpos AND st.tiny = NO THEN
    xpos = 8
   END IF
   DIM zmenuopts as MenuOptions
   zmenuopts.edged = YES
   zmenuopts.wide = pixel_width
   standardmenu cast(BasicMenuItem vector, zonemenu), zonemenustate, xpos, 40, dpage, zmenuopts

   IF zonemenustate.pt > -1 THEN
    ' A little right arrow
    edgeprint CHR(26), xpos - 8, 40 + (zonemenustate.pt - zonemenustate.top)*9, uilook(uiText), dpage
   END IF

  END IF
 END IF

 '--Undo preview edits
 undo_preview st, map(), pass, emap, zmap

 '--Message
 IF st.message_ticks > 0 THEN
  centerbox vpages(dpage)->w \ 2, 11, LEN(st.message) * 8 + 20, 15, 1, dpage
  edgeprint st.message, vpages(dpage)->w \ 2 - LEN(st.message) * 4, 6, uilook(uiText), dpage, YES
 END IF

 SWAP vpage, dpage
 setvispage vpage
 IF dowait THEN slowtog XOR= 1
LOOP
st.message_ticks = 0
RETRACE '--end of mapping GOSUB block

END SUB


'==========================================================================================
'                                Zonemap display helpers
'==========================================================================================


SUB mapedit_list_npcs_by_tile (st as MapEditState)

 DIM dir_str(...) as string = {"north", "east", "south", "west"}

 DIM count as integer = 0

 REDIM menu(0) as string
 menu(0) = "Back to the map editor..."

 DIM s as string
 
 FOR i as integer = 0 to 299
  WITH st.npc_inst(i)
   IF .id > 0 THEN
    IF .x = st.x * 20 AND .y = st.y * 20 THEN
     s = "NPC ID=" & (.id - 1) & " facing " & dir_str(.dir)
     str_array_append menu(), s
     count += 1
    END IF
   END IF
  END WITH
 NEXT i

 DIM state as MenuState
 state.size = 20
 state.last = UBOUND(menu)

 setkeys
 DO
  setwait 55
  setkeys

  IF keyval(scF1) > 1 THEN show_help "mapedit_npcs_by_tile"
  IF keyval(scESC) > 1 THEN EXIT DO
  IF enter_space_click(state) THEN
   clearkey(scSpace)
   IF state.pt = 0 THEN EXIT DO
  END IF
  
  usemenu state

  edgeprint count & " NPCs at tile X=" & st.x & " Y=" & st.y, 0, 0, uilook(uiSelectedDisabled), dpage
  standardmenu menu(), state, 0, 10, dpage

  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
 
END SUB

FUNCTION mapedit_npc_at_spot(st as MapEditState) as integer
 FOR i as integer = 0 TO 299
  WITH st.npc_inst(i)
   IF .id > 0 THEN
    IF .x = st.x * 20 AND .y = st.y * 20 THEN RETURN i
   END IF
  END WITH
 NEXT i
 RETURN -1
END FUNCTION

'==========================================================================================

'This is a variant on spriteedit_draw_icon
SUB mapedit_draw_icon(st as MapEditState, icon as string, byval x as integer, byval y as integer, byval highlight as bool = NO)
 DIM bgcol as integer
 DIM fgcol as integer
 fgcol = uilook(uiMenuItem)
 bgcol = uilook(uiDisabledItem)
 IF highlight THEN
  fgcol = uilook(uiText)
  bgcol = uilook(uiMenuItem)
 END IF
 'IF ts.zonenum = areanum + 1 THEN bgcol = uilook(uiSelectedDisabled)
 textcolor fgcol, bgcol
 printstr icon, x, y, dpage
END SUB

SUB update_npc_graphics(st as MapEditState, npc_img() as GraphicPair)
 ' npc_img() may be sized larger than the number of NPC defs (st.num_npc_defs),
 ' if so, the extra graphics if any are freed
 FOR i as integer = 0 TO UBOUND(npc_img)
  WITH npc_img(i)
   IF .sprite THEN frame_unload @.sprite
   IF .pal THEN palette16_unload @.pal
   IF i <= st.num_npc_defs - 1 THEN
    .sprite = frame_load(4, st.npc_def(i).picture)
    .pal    = palette16_load(st.npc_def(i).palette, 4, st.npc_def(i).picture)
   END IF
  END WITH
 NEXT i
END SUB


'==========================================================================================
'                                Zonemap display helpers
'==========================================================================================


'Returns the colour chosen, from 0-7
FUNCTION mapedit_try_assign_colour_to_zone(byval id as integer, zonecolours() as integer, viszonelist() as integer) as integer
 STATIC zone_col_rotate as integer
 DIM idx as integer

 'note viszonelist(-1) is not used, but is 0, so does not interfere
 idx = int_array_find(viszonelist(), id)
 IF idx <> -1 THEN
  RETURN idx
 END IF

 'Success guaranteed
 int_array_append viszonelist(), id

 'First check whether we remember a colour
 idx = int_array_find(zonecolours(), id)
 IF idx <> -1 THEN
  zonecolours(idx) = id
  RETURN idx
 END IF

 'An unused colour?
 idx = int_array_find(zonecolours(), 0)
 IF idx <> -1 THEN
  zonecolours(idx) = id
  RETURN idx
 END IF

 'Deassign colour to some zone remembered but no longer visible (certain to find one)
 DO
  'Rotate the first colour checked, otherwise everything keeps getting colour 0
  zone_col_rotate = (zone_col_rotate + 1) MOD (UBOUND(zonecolours) + 1)

  IF int_array_find(viszonelist(), zonecolours(zone_col_rotate)) = -1 THEN
   zonecolours(zone_col_rotate) = id
   RETURN zone_col_rotate
  END IF
 LOOP
END FUNCTION

SUB zonemenu_add_zone (byref zonemenu as SimpleMenuItem vector, zonecolours() as integer, byval info as ZoneInfo ptr)
 IF info = NULL THEN
  debug "zonemenu_add_zone: NULL zone"
  EXIT SUB
 END IF

 DIM col as integer = int_array_find(zonecolours(), info->id)
 DIM extra as string
 IF info->hidden THEN
  extra = "(H)"
  col = -1
 END IF
 IF col = -1 THEN
  col = uilook(uiDisabledItem)
 ELSE
  col = boxlook(col).edgecol
 END IF
 IF info->name <> "" THEN extra += " " & info->name
 append_simplemenu_item zonemenu, "${K" & col & "}" & info->id & "${K" & uilook(uiText) & "}" & extra, , , info->id
END SUB

'Rebuilds zonemenu and st.zoneviewmap based on selected tile and lockedzonelist() 
SUB mapedit_update_visible_zones (st as MapEditState, byref zonemenu as SimpleMenuItem vector, zonemenustate as MenuState, zmap as ZoneMap, lockedzonelist() as integer)

 REDIM tilezonelist(-1 TO -1) as integer  'The zones at the current tile (index 0 onwards, start at -1 for fake zero-length arrays)
 REDIM viszonelist(-1 TO 0) as integer    'The currently displayed zones. At most 8. (index 0 onwards, start at -1 for fake zero-length arrays)
 DIM i as integer

 'Find the previous selection, so can move the cursor to something appropriate
 DIM oldpt_zone as integer = -1
 DIM oldpt_waslocked as bool = NO
 IF zonemenustate.pt <> -1 THEN
  oldpt_zone = zonemenu[zonemenustate.pt].dat
  'Search for "Zones here:", yeah, real ugly
  FOR i as integer = zonemenustate.pt TO v_len(zonemenu) - 1
   IF zonemenu[i].dat = 0 THEN oldpt_waslocked = YES
  NEXT
'  oldpt_waslocked = (zonemenustate.pt <= UBOUND(lockedzonelist) + 1)
 END IF

 GetZonesAtTile zmap, tilezonelist(), st.x, st.y

 'Decide upon visible zones

 REDIM viszonelist(-1 TO -1)
 FOR i as integer = 0 TO UBOUND(lockedzonelist)
  mapedit_try_assign_colour_to_zone lockedzonelist(i), st.zonecolours(), viszonelist()
 NEXT

 IF st.autoshow_zones THEN
  'Try to add some of the zones at this tile to the visible zone list

  'Assign remaining colours/patterns to some zones at this tile
  FOR i as integer = 0 TO UBOUND(tilezonelist)
   IF UBOUND(viszonelist) >= 7 THEN EXIT FOR
   IF GetZoneInfo(zmap, tilezonelist(i))->hidden THEN CONTINUE FOR
   mapedit_try_assign_colour_to_zone tilezonelist(i), st.zonecolours(), viszonelist()
  NEXT
 END IF

 'Rebuild the menu
 v_free zonemenu
 v_new zonemenu
 IF UBOUND(lockedzonelist) >= 0 THEN
  append_simplemenu_item zonemenu, "Locked zones:", YES, uilook(uiText)
 END IF
 FOR i as integer = 0 TO UBOUND(lockedzonelist)
  zonemenu_add_zone zonemenu, st.zonecolours(), GetZoneInfo(zmap, lockedzonelist(i))
 NEXT

 append_simplemenu_item zonemenu, iif_string(UBOUND(tilezonelist) >= 0, "Zones here:", "No zones here"), YES, uilook(uiText)
 DIM tileliststart as integer = v_len(zonemenu)
 FOR i as integer = 0 TO UBOUND(tilezonelist)
  zonemenu_add_zone zonemenu, st.zonecolours(), GetZoneInfo(zmap, tilezonelist(i))
 NEXT

 zonemenustate.size = 14
 'sets .pt to something valid, or -1 if nothing selectable
 init_menu_state zonemenustate, cast(BasicMenuItem vector, zonemenu)

 'Pick a good selection automatically
 IF zonemenustate.pt <> -1 THEN
  IF oldpt_waslocked THEN
'   zonemenustate.pt = bound(zonemenustate.pt, 1, UBOUND(lockedzonelist) + 1)
  ELSE
   IF tileliststart < v_len(zonemenu) THEN
    zonemenustate.pt = tileliststart
    FOR i as integer = v_len(zonemenu) - 1 TO 0 STEP -1
     IF zonemenu[i].dat = oldpt_zone THEN zonemenustate.pt = i: EXIT FOR
     IF zonemenu[i].dat = 0 THEN EXIT FOR
    NEXT
   END IF
  END IF
 END IF

 'Update the zoneviewmap
 CleanTilemap st.zoneviewmap, st.wide, st.high
 FOR i as integer = 0 TO UBOUND(viszonelist)
  DIM colour as integer = int_array_find(st.zonecolours(), viszonelist(i))
  ZoneToTilemap zmap, st.zoneviewmap, viszonelist(i), colour
 NEXT
 'needs to be called after zoneviewmap is updated, to show hidden zones
 mapedit_doZoneHinting st, zmap

END SUB

SUB draw_zone_minimap(st as MapEditState, tmap as TileMap, byval bitnum as integer, byval col as integer)
 frame_unload @st.zoneminimap
 st.zoneminimap = frame_new(tmap.wide, tmap.high, , YES)

 DIM bitmask as integer = 1 SHL bitnum
 DIM tptr as byte ptr = tmap.data
 FOR y as integer = 0 TO tmap.high - 1
  FOR x as integer = 0 TO tmap.wide - 1
   IF *tptr AND bitmask THEN
    putpixel st.zoneminimap, x, y, col
   END IF
   tptr += 1
  NEXT
 NEXT
END SUB

SUB draw_zone_tileset(byval zonetileset as Frame ptr)
 ' This draws a bunch of lines across the tiles of a tileset, to indicate up to 8 overlapping zones at once
 ' zonetileset is a 256-tile tileset!! Each bit in the tile number indicates a different zone
 ' The zones are coloured with textbox border colours
 DIM as integer zone, tileno, offsetstart, lineoffset, i, onlyhalf
 FOR tileno = 0 TO 255
  FOR zone = 0 TO 7
   IF (tileno AND (1 SHL zone)) = 0 THEN CONTINUE FOR
   'In each direction the 5 lines (every 4 pixels) for a zone overlap with another zone; draw half of each
   'if they are both present
   onlyhalf = (zone >= 4) ANDALSO (tileno AND (1 SHL (zone - 4)))
   offsetstart = ((zone \ 2) * 2 + 1) MOD 4  '1, 1, 3, 3, 1, 1, 3, 3
   FOR lineoffset = offsetstart TO 19 STEP 4
    'Draw 5 lines across each tile
    IF zone AND 1 THEN
     'Horizontal
     IF onlyhalf THEN
      drawline zonetileset, 10, tileno*20 + lineoffset, 19, tileno*20 + lineoffset, boxlook(zone).edgecol
     ELSE
      drawline zonetileset, 0, tileno*20 + lineoffset, 19, tileno*20 + lineoffset, boxlook(zone).edgecol
     END IF
    ELSE
     'Vertical
     IF onlyhalf THEN
      drawline zonetileset, lineoffset, tileno*20 + 10, lineoffset, tileno*20 + 19, boxlook(zone).edgecol
     ELSE
      drawline zonetileset, lineoffset, tileno*20, lineoffset, tileno*20 + 19, boxlook(zone).edgecol
     END IF
    END IF
   NEXT
  NEXT
 NEXT
END SUB

SUB draw_diamond(byval fr as Frame ptr, byval x as integer, byval y as integer, byval c as integer)
  FOR yi as integer = 0 TO 4
   FOR xi as integer = 0 TO 4
    IF ABS(yi - 2) + ABS(xi - 2) <= 2 THEN putpixel fr, x + xi, y + yi, c
   NEXT
  NEXT
  putpixel fr, x + 2, y + 2, 0
END SUB

SUB draw_zone_tileset2(byval zonetileset as Frame ptr)
 ' Alternative tileset
 ' This draws a bunch of dots across the tiles of a tileset, to indicate up to 8 overlapping zones at once
 ' zonetileset is a 256-tile tileset!! Each bit in the tile number indicates a different zone
 ' The zones are coloured with textbox border colours
 DIM as integer zone, tileno, temp
 FOR tileno = 1 TO 255
  'corner pieces
  putpixel zonetileset, 0, tileno * 20, uilook(uiText)
  putpixel zonetileset, 1, tileno * 20, uilook(uiText)
  putpixel zonetileset, 0, tileno * 20 + 1, uilook(uiText)

  'dots
  FOR zone = 0 TO 7
   IF (tileno AND (1 SHL zone)) = 0 THEN CONTINUE FOR
   temp = zone * 2
   IF (zone \ 2) MOD 2 = 1 THEN temp += 1  '0, 2, 5, 7, 8, 10, 13, 15
   temp = 5 * temp
   draw_diamond zonetileset, (temp \ 20) * 5, tileno * 20 + temp MOD 20, boxlook(zone).edgecol
  NEXT
 NEXT
END SUB

SUB draw_zone_tileset3(byval zonetileset as Frame ptr)
 ' Alternative tileset
 ' zonetileset is a 256-tile tileset!! Each bit in the tile number indicates a different zone
 ' The zones are coloured with textbox border colours

 STATIC sectantx(7) as integer = {0, 6, 8, 6, 0, -6, -8, -6}
 STATIC sectanty(7) as integer = {-8, -6, 0, 6, 8, 6, 0, -6}
 STATIC centrex(7) as integer = {1, 1, 1, 1, 0, 0, 0, 0}
 STATIC centrey(7) as integer = {0, 0, 1, 1, 1, 1, 0, 0}

 DIM as integer zone, tileno, temp, safecol
 'Pick an unused colour
 WHILE 1
  safecol = 1 + randint(254)
  FOR zone = 0 TO 7
   IF safecol = boxlook(zone).edgecol THEN CONTINUE WHILE
  NEXT
  EXIT WHILE
 WEND

 FOR tileno = 1 TO 255
  FOR zone = 0 TO 7
   IF (tileno AND (1 SHL zone)) = 0 THEN CONTINUE FOR

   dim as integer x1, y1, x2, y2, x3, y3  'coordinates of corners of the sectant
   x1 = 9 + sectantx(zone)  + centrex(zone)
   y1 = tileno*20 + 9 + sectanty(zone)  + centrey(zone)
   x2 = 9 + centrex(zone)
   y2 = tileno*20 + 9 + centrey(zone)
   x3 = 9 + sectantx((zone + 1) MOD 8)  + centrex(zone)
   y3 = tileno*20 + 9 + sectanty((zone + 1) MOD 8)  + centrey(zone)
' debug "tile " & tileno & " z " & zone & ":" & x1 & "," & y1 & " " & x3 & "," & y3

   drawline zonetileset, x2, y2, x1, y1, safecol
   drawline zonetileset, x2, y2, x3, y3, safecol
   ellipse zonetileset, 9.5, tileno*20 + 9.5, 9, (safecol AND 2) XOR 1  'Doesn't matter what colour, as long as not safecol or 0

   paintat zonetileset, (x1 + x2 + x3)/3, (y1 + y2 + y3)/3, safecol  'Merge with the lines
   paintat zonetileset, (x1 + x2 + x3)/3, (y1 + y2 + y3)/3, boxlook(zone).edgecol
  NEXT
 NEXT
 replacecolor zonetileset, (safecol AND 2) XOR 1, 0
END SUB

'Paints the zoneoverlaymap to show tiles with nonvisible zones
'It may be a good idea to not show hidden zones, unfortunately that would be difficult/really slow
SUB mapedit_doZoneHinting(st as MapEditState, zmap as ZoneMap)
  CleanTilemap st.zoneoverlaymap, st.zoneviewmap.wide, st.zoneviewmap.high
  WITH zmap
    DIM as integer x, y
    FOR y = 0 TO .high - 1
      DIM bitvectors as ushort ptr = @.bitmap[y * .wide]
      DIM tileptr0 as ubyte ptr = @st.zoneviewmap.data[y * .wide]
      DIM tileptr1 as ubyte ptr = @st.zoneoverlaymap.data[y * .wide]
      FOR x = 0 TO .wide - 1
        'IF tileptr0[x] = 0 ANDALSO tileptr1[x] = 0 ANDALSO bitvectors[x] <> 0 THEN
        IF bitcount(bitvectors[x] AND NOT (1 SHL 15)) > bitcount(tileptr0[x]) THEN
'        IF tileptr0[x] = 0 ANDALSO bitvectors[x] <> 0 THEN
'        IF tileptr0[x] = 0 OR st.zoneviewtileset = 1 THEN
         'Show a fuzzy animation
         tileptr1[x] = 170
        END IF
      NEXT
    NEXT
  END WITH
END SUB

'For debugging. Paint a whole lot of tiles over the map randomly for the current zone,
'so that we have something to look at.
SUB mapedit_zonespam(st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap)
 DIM t as double = TIMER
 DIM as integer x, y, i, temp, count = st.cur_zinfo->numtiles
 FOR i as integer = 0 TO INT((1 + rando()) * zmap.high / 8)
  y = randint(zmap.high)
  temp = randint(zmap.wide)
  FOR x = temp TO small(temp + 12, zmap.wide - 1)
   zonebrush st, x, y, 1, , map(), pass, emap, zmap
  NEXT
 NEXT

 t = TIMER - t
 count = st.cur_zinfo->numtiles - count
 debug "zonespam: spammed " & count & " tiles, " & (1000 * t / count) & "ms/tile"
END SUB


'==========================================================================================
'                                    Zone info editor
'==========================================================================================


SUB mapedit_edit_zoneinfo(st as MapEditState, zmap as ZoneMap)
 'We could first build sorted list of zones, and only show those that actually exist?

 DIM menu(6) as string
 DIM menu_display(6) as string
 DIM enabled(6) as bool
 flusharray enabled(), -1, YES

 DIM selectst as SelectTypeState
 DIM state as MenuState
 state.last = UBOUND(menu)
 state.size = 24
 state.need_update = YES

 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "mapedit_zone_edit"
  usemenu state, enabled()
  DIM enable_strgrabber as bool = NO
  IF state.pt = 3 AND selectst.query = "" THEN enable_strgrabber = YES
  IF enable_strgrabber = NO ANDALSO select_by_typing(selectst, NO) THEN
   select_on_word_boundary menu(), selectst, state
  END IF

  SELECT CASE state.pt
   CASE 0
    IF enter_space_click(state) THEN EXIT DO
   CASE 1
    IF intgrabber(st.cur_zone, 1, 9999) THEN
     state.need_update = YES
     st.cur_zinfo = GetZoneInfo(zmap, st.cur_zone)
    END IF
   CASE 3
    IF enable_strgrabber ANDALSO strgrabber(st.cur_zinfo->name, 35) THEN state.need_update = YES
   CASE 4 TO 6
    IF intgrabber(st.cur_zinfo->extra(state.pt - 4), -2147483648, 2147483647) THEN state.need_update = YES
  END SELECT

  IF state.need_update THEN
   state.need_update = NO

   menu(0) = "Previous Menu"
   menu(1) = CHR(27) & "Zone " & st.cur_zone & CHR(26)
   menu(2) = " Contains " & st.cur_zinfo->numtiles & " tiles"
   enabled(2) = NO
   menu(3) = "Name:" & st.cur_zinfo->name
   FOR i as integer = 0 TO 2
    menu(4 + i) = "Extra data " & i & ":" & st.cur_zinfo->extra(i)
   NEXT
  END IF

  clearpage vpage
  highlight_menu_typing_selection menu(), menu_display(), selectst, state
  standardmenu menu_display(), state, 0, 0, vpage
  setvispage vpage
  dowait
 LOOP
 
END SUB


'==========================================================================================
'                                  General map data menu
'==========================================================================================


'Whether the user should have the option to pick an edge tile.
'Also true if the map is smaller than the screen and the camera is set to crop.
FUNCTION mapedit_need_default_edge_tile(st as MapEditState, gmap() as integer) as bool
 IF gmap(5) = 2 THEN RETURN YES  'Use default edge tile
 IF gmap(5) = 0 THEN  'Crop camera
  IF gen(genResolutionX) > st.wide * 20 ORELSE gen(genResolutionY) > st.high * 20 THEN RETURN YES
 END IF
 RETURN NO
END FUNCTION

SUB mapedit_gmapdata_buildmenu(st as MapEditState, byref menu as SimpleMenuItem vector, gdidx() as integer, midx() as integer, gmap() as integer, zmap as ZoneMap)

 v_new menu
 REDIM gdidx(23)
 gdidx(0)  = -1: append_simplemenu_item menu, "Previous Menu"
 gdidx(1)  = 1:  append_simplemenu_item menu, "Ambient Music: "
 gdidx(2)  = 2:  append_simplemenu_item menu, "Minimap Available: "
 gdidx(3)  = 3:  append_simplemenu_item menu, "Save Anywhere: "
 gdidx(4)  = 18: append_simplemenu_item menu, "Tile Data: "
 gdidx(5)  = 17: append_simplemenu_item menu, "NPC Data: "
 gdidx(6)  = 32: append_simplemenu_item menu, "Default NPC Move Zone: "
 gdidx(7)  = 33: append_simplemenu_item menu, "Default NPC Avoid Zone: "

 gdidx(8)  = -1: append_simplemenu_item menu, "Display:", YES, uilook(uiText)

 gdidx(9)  = 11: append_simplemenu_item menu, "Foot Offset: "
 gdidx(10) = 16: append_simplemenu_item menu, "Walkabout Layering: "
 gdidx(11) = 4:  append_simplemenu_item menu, "Display Map Name: "
 gdidx(12) = 10: append_simplemenu_item menu, "Harm-Tile Flash: "   'flash colour drawn here
 gdidx(13) = 9:  append_simplemenu_item menu, "Harm-Tile Damage: "
 gdidx(14) = 5:  append_simplemenu_item menu, "Map Edge Mode: " 
 gdidx(15) = 6:  append_simplemenu_item menu, "Default Edge Tile: " 'edge tile drawn here

 'Add an extra gap, for the edge tile preview
 gdidx(16) = -1: append_simplemenu_item menu, "", YES

 gdidx(17) = -1: append_simplemenu_item menu, "Scripts:", YES, uilook(uiText)
 gdidx(18) = 7:  append_simplemenu_item menu, "Autorun Script: "
 gdidx(19) = 8:  append_simplemenu_item menu, "Autorun Script Argument: "
 gdidx(20) = 12: append_simplemenu_item menu, "After-Battle Script: "
 gdidx(21) = 13: append_simplemenu_item menu, "Instead-of-Battle Script: "
 gdidx(22) = 14: append_simplemenu_item menu, "Each-Step Script: "
 gdidx(23) = 15: append_simplemenu_item menu, "On-Keypress Script: "

 IF UBOUND(gdidx) + 1 <> v_len(menu) THEN debugc errFatalBug, "Wrong gdidx length!"
 invert_permutation gdidx(), midx()

 ' Music
 IF gmap(1) = 0 THEN
  menu[midx(1)].text &= "-silence-"
 ELSEIF gmap(1) = -1 THEN
  menu[midx(1)].text &= "-same as previous map-"
 ELSE
  menu[midx(1)].text &= (gmap(1) - 1) & " " & getsongname(gmap(1) - 1)
 END IF
 ' Minimap available and save anywhere
 menu[midx(2)].text &= yesorno(gmap(2))
 menu[midx(3)].text &= yesorno(gmap(3))
 ' Show map name
 IF gmap(4) = 0 THEN
  menu[midx(4)].text &= "NO"
 ELSE
  menu[midx(4)].text &= gmap(4) & " ticks"
 END IF
 ' Map edge mode
 SELECT CASE gmap(5)
  CASE 0
   menu[midx(5)].text &= "Crop"
  CASE 1
   menu[midx(5)].text &= "Wrap"
  CASE 2
   menu[midx(5)].text &= "use default edge tile"
 END SELECT
 ' Default edge tile
 IF mapedit_need_default_edge_tile(st, gmap()) THEN
  menu[midx(6)].text &= gmap(6)
 ELSE
  menu[midx(6)].text &= "N/A"
 END IF
 ' Scripts
 menu[midx(7)].text &= scriptname(gmap(7))
 FOR i as integer = 12 TO 15
  menu[midx(i)].text &= scriptname(gmap(i))
 NEXT
 ' Autorun script argument
 IF gmap(7) = 0 THEN
  menu[midx(8)].text &= "N/A"
 ELSE
  menu[midx(8)].text &= gmap(8)
 END IF
 ' Harm tile damage
 menu[midx(9)].text &= gmap(9)
 ' Harm tile flash
 IF gmap(10) = 0 THEN
  menu[midx(10)].text &= "none"
 ELSE
  menu[midx(10)].text &= gmap(10)
 END IF
 ' Foot offset
 SELECT CASE gmap(11)
  CASE 0
   menu[midx(11)].text &= "none"
  CASE IS < 0
   menu[midx(11)].text &= "up " & ABS(gmap(11)) & " pixels"
  CASE IS > 0
   menu[midx(11)].text &= "down " & gmap(11) & " pixels"
 END SELECT
 ' Hero/npc draw order
 SELECT CASE gmap(16)
  CASE 0: menu[midx(16)].text &= "Heroes over NPCs"
  CASE 1: menu[midx(16)].text &= "NPCs over Heroes"
  CASE 2: menu[midx(16)].text &= "Together (recommended)"
 END SELECT
 ' NPC and Tile data saving
 FOR i as integer = 17 TO 18
  SELECT CASE gmap(i)
   CASE 0
    menu[midx(i)].text &= "Don't save state when leaving"
   CASE 1
    menu[midx(i)].text &= "Remember state when leaving"
   CASE 2
    menu[midx(i)].text &= "Ignore saved state, load anew"
  END SELECT
 NEXT
 ' Default zones
 FOR i as integer = 32 TO 33
  IF gmap(i) = 0 THEN
   menu[midx(i)].text &= "None"
  ELSE
   menu[midx(i)].text &= gmap(i) & " " & GetZoneInfo(zmap, gmap(i))->name
  END IF
 NEXT
END SUB

SUB mapedit_gmapdata(st as MapEditState, gmap() as integer, zmap as ZoneMap)
 DIM gdidx() as integer   'Index in gmap()
 DIM menu as SimpleMenuItem vector
 DIM menu_display as SimpleMenuItem vector

 'Maps gmap() index to menu() index
 DIM midx(dimbinsize(binMAP)) as integer

 mapedit_gmapdata_buildmenu st, menu, gdidx(), midx(), gmap(), zmap

 'These are indexed by *gmap index*, not by menu item index!
 DIM gdmax(dimbinsize(binMAP)) as integer
 DIM gdmin(dimbinsize(binMAP)) as integer
 gdmax(1) = gen(genMaxSong) + 1:  gdmin(1) = -1
 gdmax(2) = 1:                    gdmin(2) = 0
 gdmax(3) = 1:                    gdmin(3) = 0
 gdmax(4) = 255:                  gdmin(4) = 0
 gdmax(5) = 2:                    gdmin(5) = 0
 gdmax(6) = 255:                  gdmin(6) = 0
 gdmax(7) = 32767:                gdmin(7) = 0
 gdmax(8) = 32767:                gdmin(8) = -32767
 gdmax(9) = 32767:                gdmin(9) = -32767
 gdmax(10) = 255:                 gdmin(10) = 0
 gdmax(11) = 20:                  gdmin(11) = -20
 gdmax(12) = 32767:               gdmin(12) = 0
 gdmax(13) = 32767:               gdmin(13) = 0
 gdmax(14) = 32767:               gdmin(14) = 0
 gdmax(15) = 32767:               gdmin(15) = 0
 gdmax(16) = 2:                   gdmin(16) = 0
 gdmax(17) = 2:                   gdmin(17) = 0
 gdmax(18) = 2:                   gdmin(18) = 0
 gdmax(32) = 9999:                gdmin(32) = 0
 gdmax(33) = 9999:                gdmin(33) = 0

 DIM selectst as SelectTypeState
 DIM state as MenuState
 state.pt = 0
 state.last = v_len(menu) - 1
 state.size = 24

 'Clamp data to bounds (only the gmap() indices edited in this menu)
 FOR i as integer = 0 TO UBOUND(gdidx)
  'Safety-bounding of gmap data, prevents crashes in cases of corruption
  DIM idx as integer = gdidx(i)
  IF idx >= 0 THEN
   IF NOT in_bound(gmap(idx), gdmin(idx), gdmax(idx)) THEN
    debugc errError, "Map " & st.mapnum & "'s general data corrupt (or unsupported); clamping gmap(" & idx & ") = " & gmap(idx) & " -> 0"
    gmap(idx) = 0
   END IF
  END IF
 NEXT i

 'A sample map of a single tile, used to preview the default edge tile
 DIM sampmap as TileMap
 cleantilemap sampmap, 1, 1
 
 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(scESC) > 1 OR (state.pt = 0 AND enter_space_click(state)) THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "general_map_data"
  usemenu state, cast(BasicMenuItem vector, menu)
  DIM idx as integer = gdidx(state.pt)
  SELECT CASE idx
   CASE -1
   CASE 1 'music
    IF zintgrabber(gmap(idx), gdmin(idx) - 1, gdmax(idx) - 1) THEN 'song is optional
     music_stop
     state.need_update = YES
    END IF
    IF enter_space_click(state) THEN
     IF gmap(idx) > 0 THEN playsongnum gmap(idx) - 1
    END IF
   CASE 7, 12 TO 15 'scripts
    IF enter_space_click(state) THEN
     scriptbrowse(gmap(idx), plottrigger, "plotscript")
     state.need_update = YES
    ELSEIF scrintgrabber(gmap(idx), 0, 0, scLeft, scRight, 1, plottrigger) THEN
     state.need_update = YES
    END IF
   CASE 10 'Harm tile color
    state.need_update OR= intgrabber(gmap(idx), gdmin(idx), gdmax(idx))
    IF enter_space_click(state) THEN
     gmap(idx) = color_browser_256(gmap(idx))
    END IF
   CASE ELSE 'all other gmap data are simple integers
    state.need_update OR= intgrabber(gmap(idx), gdmin(idx), gdmax(idx))
  END SELECT

  IF state.need_update THEN
   mapedit_gmapdata_buildmenu st, menu, gdidx(), midx(), gmap(), zmap
   state.need_update = NO
  END IF

  IF select_by_typing(selectst, NO) THEN
   select_on_word_boundary cast(BasicMenuItem vector, menu), selectst, state
  END IF

  '--Draw screen
  clearpage dpage
  highlight_menu_typing_selection cast(BasicMenuItem vector, menu), cast(BasicMenuItem vector, menu_display), selectst, state
  standardmenu cast(BasicMenuItem vector, menu_display), state, 0, 0, dpage
  IF gmap(10) THEN
   'Harm tile flash color preview
   rectangle 4 + 8 * LEN(menu[midx(10)].text), 8 * midx(10), 8, 8, gmap(10), dpage
  END IF
  IF mapedit_need_default_edge_tile(st, gmap()) THEN
   'Show default edge tile
   writeblock sampmap, 0, 0, gmap(6)
   DIM tilepos as XYPair = (8 + 8 * LEN(menu[midx(6)].text), 8 * midx(6))
   DIM tileview as Frame ptr
   tileview = frame_new_view(vpages(dpage), tilepos.x, tilepos.y, 20, 20)
   drawmap sampmap, 0, 0, st.tilesets(0)->spr, tileview ', NO, 0, NULL, NO
   frame_unload @tileview
  END IF
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 music_stop
 unloadtilemap sampmap
 v_free menu
 v_free menu_display
END SUB


'==========================================================================================
'                                      Layers menus
'==========================================================================================


FUNCTION mapedit_pick_layer(st as MapEditState, gmap() as integer, map() as TileMap, message as string, other_option as string = "") as integer
 'Gives the user a prompt to select a map layer, returns layer number, or -2
 'if cancelled.
 'If other_option isn't empty it appears as an alternative option at the top, returning -1 if selected.
 DIM offset as integer = 0
 IF LEN(other_option) > 0 THEN offset = 1
 DIM options(UBOUND(map) + offset) as string
 IF LEN(other_option) > 0 THEN options(0) = other_option
 FOR layerno as integer = 0 TO UBOUND(map)
  options(offset + layerno) = "Layer " & layerno & " " & read_map_layer_name(gmap(), layerno)
 NEXT
 DIM ret as integer
 ret = multichoice(message, options(), 0, -2)
 IF ret >= 0 THEN ret -= offset
 RETURN ret
END FUNCTION

SUB mapedit_layers (st as MapEditState, gmap() as integer, visible() as integer, map() as TileMap)
 DIM state as MenuState
 DIM menuopts as MenuOptions
 menuopts.edged = YES
 menuopts.showright = YES
 menuopts.fullscreen_scrollbar = YES
 DIM menu as LayerMenuItem vector
 
 DIM layerno as integer
 DIM fakelayerno as integer  'the selected layer, treating NPCs/Heroes as a layer
 DIM resetpt as bool
 DIM col as integer
 DIM tileset as integer

 state.top = 0
 state.size = 19

 mapedit_makelayermenu st, menu, state, gmap(), visible(), map(), YES, st.layer

 DO 
  setwait 55
  setkeys YES

  layerno = menu[state.pt].layernum
  fakelayerno = layerno
  IF fakelayerno >= gmap(31) THEN fakelayerno += 1
  'Warning: gen(31) (#layers below heroes/npcs) might be larger than the number of layers

  IF keyval(scESC) > 1 THEN clearkey(scESC): EXIT DO
  IF keyval(scF1) > 1 THEN show_help "mapedit_layers"
  IF (keyval(scPlus) > 1 OR keyval(scNumpadPlus) > 1) AND UBOUND(map) < maplayerMax THEN
   DIM layer_to_copy as integer
   layer_to_copy = mapedit_pick_layer(st, gmap(), map(), "Copy an existing layer?", "No; new blank layer")
   IF layer_to_copy >= -1 THEN
    'Didn't cancel
    IF layerno = -1 THEN
     'No layer selected; just add new one to end
     mapedit_append_new_layers st, map(), visible(), gmap(), 1
     layerno = UBOUND(map)
     resetpt = NO
    ELSE
     'Insert after.
     'When gmap(31) is greater than actual number of layers we are "filling up" to old default of 2 under
     IF layerno < gmap(31) AND UBOUND(map) + 1 >= gmap(31) THEN gmap(31) += 1
     IF layer_to_copy > layerno THEN layer_to_copy += 1
     mapedit_insert_new_layer st, map(), visible(), gmap(), layerno + 1
     layerno += 1
     resetpt = YES
    END IF
    IF layer_to_copy >= 0 THEN mapedit_copy_layer(st, map(), gmap(), layer_to_copy, layerno)
    state.need_update = YES
   END IF
  END IF
  IF (keyval(scDelete) > 1 OR keyval(scMinus) > 1 OR keyval(scNumpadMinus) > 1) ANDALSO UBOUND(map) > 0 ANDALSO layerno >= 0 THEN
   DIM layername as string
   layername = read_map_layer_name(gmap(), layerno)
   IF LEN(layername) THEN layername = " " & layername
   IF yesno("Really delete layer " & layerno & layername & "?", NO) THEN
    IF layerno < gmap(31) THEN gmap(31) = large(gmap(31) - 1, 1)
    mapedit_delete_layer st, map(), visible(), gmap(), layerno
    st.layer = small(st.layer, UBOUND(map))
    layerno = small(layerno, UBOUND(map))
    resetpt = YES
    state.need_update = YES
   END IF
  END IF
  IF keyval(scShift) > 0 THEN
   'Moving layers up or down: state.pt needs to be updated afterwards, which happens
   'in mapedit_makelayermenu with resetpt = YES

   IF keyval(scUp) > 1 AND fakelayerno > 0 THEN
    IF fakelayerno = gmap(31) + 1 THEN
     'swapping with NPC/Hero layers
     gmap(31) += 1
    ELSE
     mapedit_swap_layers st, map(), visible(), gmap(), layerno, layerno - 1
     layerno -= 1
    END IF
    resetpt = YES
    state.need_update = YES
   END IF
   'UBOUND(map) or UBOUND(map) + 1 is the maximum fakelayerno (can't adjust overhead tiles either)
   IF keyval(scDown) > 1 THEN
    IF layerno = 0 AND UBOUND(map) > 0 THEN
     'can't move npcs/heroes below layer 0, so swap with 2nd layer instead
     mapedit_swap_layers st, map(), visible(), gmap(), layerno, layerno + 1
     layerno += 1     
    ELSEIF layerno > 0 THEN
     IF layerno = small(gmap(31) - 1, UBOUND(map)) THEN  'gmap(31) may be larger
      'swapping with NPC/Hero layers
      gmap(31) = layerno
     ELSEIF layerno < UBOUND(map) THEN
      mapedit_swap_layers st, map(), visible(), gmap(), layerno, layerno + 1
      layerno += 1
     END IF
    END IF
    resetpt = YES
    state.need_update = YES
   END IF

  ELSE
   'Normal controls
   IF usemenu(state, cast(BasicMenuItem vector, menu)) THEN
    state.need_update = YES
    layerno = menu[state.pt].layernum
   END IF
  END IF

  IF resetpt = NO THEN
   SELECT CASE menu[state.pt].role
    CASE ltPreviousMenu
     IF enter_space_click(state) THEN
      EXIT DO
     END IF
    CASE ltDefaultTileset
     IF intgrabber(gmap(0), 0, gen(genMaxTile)) THEN
      state.need_update = YES
     END IF
    CASE ltLayerName
     DIM tempname as string
     tempname = read_map_layer_name(gmap(), layerno)
     IF strgrabber(tempname, 40) THEN
      state.need_update = YES
     END IF
     write_map_layer_name(gmap(), layerno, tempname)
    CASE ltLayerTileset
     clearkey(scPlus)
     clearkey(scNumpadPlus)
     clearkey(scMinus)
     clearkey(scNumpadMinus)
     IF zintgrabber(gmap(menu[state.pt].gmapindex), -1, gen(genMaxTile)) THEN
      tileset = gmap(menu[state.pt].gmapindex) - 1
      IF tileset = -1 THEN tileset = gmap(0)
      loadtilesetdata st.tilesets(), layerno, tileset
      state.need_update = YES
     END IF
    CASE ltLayerEnabled
     IF enter_space_click(state) THEN
      ToggleLayerEnabled(gmap(), layerno)
      state.need_update = YES
     END IF
     IF layerisenabled(gmap(), layerno) AND (keyval(scLeft) > 1 OR keyval(scRight) > 1) THEN
      ToggleLayerVisible(visible(), layerno)
      state.need_update = YES
     END IF
   END SELECT
  END IF

  IF state.need_update THEN
   state.need_update = NO
   mapedit_makelayermenu st, menu, state, gmap(), visible(), map(), resetpt, layerno
   resetpt = NO
  END IF

  copypage 2, dpage
  standardmenu cast(BasicMenuItem vector, menu), state, 0, 0, dpage, menuopts

  DIM liney as integer = 190
  edgeprint "SHIFT+arrows to move layers, - to delete", 0, liney, uilook(uiText), dpage
  liney -= 10
  IF UBOUND(map) < maplayerMax THEN
   IF layerno > -1 THEN
    edgeprint "+ to add a new layer after this one", 0, liney, uilook(uiText), dpage
   ELSE
    edgeprint "+ to add a new layer", 0, liney, uilook(uiText), dpage
   END IF
   liney -= 10
  END IF
  WITH menu[state.pt]
   IF .role = ltLayerName THEN
    edgeprint "Type to name this layer", 0, liney, uilook(uiText), dpage
    liney -= 10
   ELSEIF .role = ltLayerEnabled AND .layernum <> 0 THEN
    edgeprint "ENTER to disable/enable", 0, liney, uilook(uiText), dpage
    liney -= 10
   END IF
  END WITH

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 mapedit_load_tilesets st, map(), gmap()
 IF layerisenabled(gmap(), st.layer) = 0 THEN st.layer = 0

END SUB

'Create all the menu items for a single layer
SUB mapedit_makelayermenu_layer(st as MapEditState, byref menu as LayerMenuItem vector, gmap() as integer, visible() as integer, byref slot as integer, byval layer as integer, byref needdefault as integer)

 menu[slot].role = ltLayerName
 'menu[slot].unselectable = YES
 menu[slot].text = "Tile layer " & layer & " " & read_map_layer_name(gmap(), layer)
 menu[slot].layernum = layer
 slot += 1

 IF layerisenabled(gmap(), layer) THEN
  IF layerisvisible(visible(), layer) THEN
   menu[slot].text = " Enabled (" & CHR(27) & "Visible in editor" & CHR(26) & ")"
   menu[slot - 1].col = uilook(uiDescription)
  ELSE
   menu[slot].text = " Enabled (" & CHR(27) & "Invisible in editor" & CHR(26) & ")"
   menu[slot - 1].col = uilook(uiDisabledItem)
  END IF
 ELSE
  menu[slot].text = " Disabled in-game"
  menu[slot - 1].col = uilook(uiDisabledItem)
 END IF
 menu[slot].role = ltLayerEnabled
 menu[slot].layernum = layer
 slot += 1

 DIM layerindex as integer = layer_tileset_index(layer)

 IF gmap(layerindex) = 0 THEN
  menu[slot].text = " Tileset: Default"
  needdefault = YES
 ELSE
  menu[slot].text = " Tileset: " & gmap(layerindex) - 1
 END IF
 menu[slot].role = ltLayerTileset
 menu[slot].layernum = layer
 menu[slot].gmapindex = layerindex
 slot += 1
END SUB

SUB mapedit_makelayermenu(st as MapEditState, byref menu as LayerMenuItem vector, state as MenuState, gmap() as integer, visible() as integer, map() as TileMap, byval resetpt as bool, byval selectedlayer as integer = 0)
 DIM remember_selection_type as LayerMenuItemType
 IF menu THEN
  remember_selection_type = menu[state.pt].role
 ELSE
  'On building the menu for the first time
  remember_selection_type = ltLayerName
 END IF

 v_free menu
 'Yuck, FIXME: append menu items normally instead
 v_new menu, 1 + 3 * (UBOUND(map) + 1) + 2 + IIF(gmap(16) = 2, 1, 2)
 state.last = v_len(menu) - 1
 FOR i as integer = 0 TO v_len(menu) - 1
  menu[i].unselectable = NO
  menu[i].col = uilook(uiMenuItem)
  menu[i].layernum = -1
  menu[i].gmapindex = -1
 NEXT i
 menu[0].text = "Go back"
 menu[0].role = ltPreviousMenu
 menu[1].text = "Default tileset: "
 menu[1].role = ltDefaultTileset
 
 DIM needdefault as integer = NO
 
 DIM slot as integer = 2
 FOR i as integer = 0 TO small(UBOUND(map), gmap(31) - 1)
  mapedit_makelayermenu_layer st, menu, gmap(), visible(), slot, i, needdefault
 NEXT

 IF gmap(16) = 2 THEN '--keep heroes and NPCs together
  menu[slot].unselectable = YES
  menu[slot].col = uilook(uiText)
  menu[slot].text = "Heroes & NPCs layer"
  slot += 1
 ELSE '--heroes and NPCs on different layers
  menu[slot].unselectable = YES
  menu[slot].col = uilook(uiText)
  slot += 1
  menu[slot].unselectable = YES
  menu[slot].col = uilook(uiText)
  slot += 1
  IF gmap(16) = 0 THEN
   menu[slot - 2].text = "NPCs layer"
   menu[slot - 1].text = "Heroes layer"
  ELSE
   menu[slot - 2].text = "Heroes layer"
   menu[slot - 1].text = "NPCs layer"
  END IF
 END IF

 FOR i as integer = gmap(31) TO UBOUND(map)
  mapedit_makelayermenu_layer st, menu, gmap(), visible(), slot, i, needdefault
 NEXT

 menu[slot].unselectable = YES
 menu[slot].col = uilook(uiText)
 menu[slot].text = "Tile layer 0 overhead tiles (obsolete)"
 slot += 1
 
 IF needdefault THEN
  menu[1].text += STR(gmap(0))
 ELSE
  menu[1].text += "(Not used)"
  menu[1].unselectable = YES
  menu[1].col = uilook(uiDisabledItem)
 END IF

 IF resetpt THEN
  state.pt = 0
  FOR i as integer = 0 TO v_len(menu) - 1
   IF menu[i].layernum = selectedlayer AND menu[i].role = remember_selection_type THEN
    state.pt = i
   END IF
  NEXT
  IF state.pt = 0 THEN debugc errBug, "Layer menu resetpt broken"
 END IF

 'Load the background for the menu on vpage 2
 DIM layerno as integer = menu[state.pt].layernum
 IF layerno > -1 AND menu[state.pt].gmapindex = -1 THEN
  'Layer menu item other than tileset selection. Preview map minimap
  clearpage 2
  DIM preview as Frame Ptr
  preview = createminimap(map(layerno), st.tilesets(layerno))
  frame_draw preview, NULL, 0, 0, , , 2
  frame_unload @preview
  fuzzyrect 0, 0, 320, 200, uilook(uiBackground), 2

 ELSE
  'Either preview tileset, or blank background

  DIM wanttileset as integer = -1
  IF state.pt = 1 THEN
   wanttileset = gmap(0)
  ELSEIF menu[state.pt].gmapindex > -1 THEN
   wanttileset = gmap(menu[state.pt].gmapindex) - 1
   IF wanttileset = -1 THEN wanttileset = gmap(0)
  END IF

  IF wanttileset = -1 THEN
   clearpage 2
  ELSE
   loadmxs game + ".til", wanttileset, vpages(2)
   fuzzyrect 0, 0, 320, 200, uilook(uiBackground), 2
  END IF
 END IF
END SUB


'==========================================================================================
'                                      Door functions
'==========================================================================================


FUNCTION door_exists (doors() as Door, id as integer) as bool
 RETURN readbit(doors(id).bits(), 0, 0) <> 0
END FUNCTION

SUB set_door_exists (doors() as Door, id as integer, value as bool)
 setbit(doors(id).bits(), 0, 0, value)
END SUB

FUNCTION find_door_at_spot (x as integer, y as integer, doors() as Door) as integer
 FOR i as integer = 0 TO UBOUND(doors)
  IF doors(i).x = x AND doors(i).y = y + 1 AND door_exists(doors(), i) THEN
   RETURN i
  END IF
 NEXT i
 RETURN -1
END FUNCTION

FUNCTION find_first_free_door (doors() as Door) as integer
 FOR i as integer = 0 TO UBOUND(doors)
  IF door_exists(doors(), i) = NO THEN RETURN i
 NEXT i
 RETURN UBOUND(doors)
END FUNCTION

FUNCTION find_first_doorlink_by_door(doornum as integer, link() as DoorLink) as integer
 FOR i as integer = 0 TO UBOUND(link)
  IF link(i).source = doornum THEN RETURN i
 NEXT i
 RETURN -1
END FUNCTION

FUNCTION find_last_used_doorlink(link() as DoorLink) as integer
 FOR i as integer = UBOUND(link) TO 0 STEP -1
  IF link(i).source >= 0 THEN RETURN i
 NEXT i
 RETURN -1
END FUNCTION


'==========================================================================================
'                            Creating, loading & saving maps
'==========================================================================================


'Adds a new map with ID gen(genMaxMap) + 1
SUB mapedit_addmap()
 'Temporary buffers for making the copy
 DIM st as MapEditState
 DIM gmap(dimbinsize(binMAP)) as integer
 REDIM doors(99) as door
 REDIM link(199) as doorlink
 REDIM map(0) as TileMap ' dummy empty map data, will be resized later
 DIM pass as TileMap
 DIM emap as TileMap
 DIM zmap as ZoneMap

 DIM copyname as string
 DIM visible(maplayerMax \ 16) as integer
 visible(0) = -1 'used as bitsets
 
 DIM how as integer
 how = addmaphow()
 '-- -2  =Cancel
 '-- -1  =New blank
 '-- >=0 =Copy
 IF how = -1 THEN
  gen(genMaxMap) += 1
  new_blank_map st, map(), pass, emap, zmap, gmap(), doors(), link()
  st.mapnum = gen(genMaxMap)
  mapedit_savemap st, map(), pass, emap, zmap, gmap(), doors(), link(), ""
 ELSEIF how >= 0 THEN
  gen(genMaxMap) += 1
  st.mapnum = how
  mapedit_loadmap st, map(), pass, emap, zmap, gmap(), visible(), doors(), link(), copyname
  st.mapnum = gen(genMaxMap)
  mapedit_savemap st, map(), pass, emap, zmap, gmap(), doors(), link(), copyname
 END IF
END SUB

SUB new_blank_map (st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, gmap() as integer, doors() as Door, link() as DoorLink)
 '--flush map buffers
 cleantilemaps map(), 64, 64, 1
 cleantilemap pass, 64, 64
 cleantilemap emap, 64, 64
 CleanZoneMap zmap, 64, 64
 flusharray gmap(), -1, 0
 gmap(16) = 2 'Walkabout Layering: Together
 gmap(31) = 1 'Walkabout layer above map layer 0
 CleanNPCL st.npc_inst()
 CleanNPCD st.npc_def()
 st.num_npc_defs = 1
 cleandoors doors()
 cleandoorlinks link()
 'Just in case
 mapedit_load_tilesets st, map(), gmap()
END SUB

SUB mapedit_loadmap (st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, gmap() as integer, visible() as integer, doors() as Door, link() as DoorLink, mapname as string)
 loadrecord gmap(), game & ".map", getbinsize(binMAP) \ 2, st.mapnum
 IF gmap(31) = 0 THEN gmap(31) = 2
 visible(maplayerMax \ 16) = -1   'default all layers to visible, if they're enabled too, of course
 loadtilemaps map(), maplumpname(st.mapnum, "t")
 loadtilemap pass, maplumpname(st.mapnum, "p")
 loadtilemap emap, maplumpname(st.mapnum, "e")
 IF isfile(maplumpname(st.mapnum, "z")) THEN
  LoadZoneMap zmap, maplumpname(st.mapnum, "z")
 ELSE
  CleanZoneMap zmap, map(0).wide, map(0).high
 END IF
 mapedit_load_tilesets st, map(), gmap()
 LoadNPCL maplumpname(st.mapnum, "l"), st.npc_inst()
 LoadNPCD_fixedlen maplumpname(st.mapnum, "n"), st.npc_def(), st.num_npc_defs
 deserdoors game & ".dox", doors(), st.mapnum
 deserdoorlinks maplumpname(st.mapnum, "d"), link()
 mapname = getmapname(st.mapnum)
 st.wide = map(0).wide
 st.high = map(0).high
 verify_map_size st, map(), pass, emap, zmap, mapname
END SUB

SUB mapedit_savemap (st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, gmap() as integer, doors() as Door, link() as DoorLink, mapname as string)
 storerecord gmap(), game & ".map", getbinsize(binMAP) / 2, st.mapnum
 savetilemaps map(), maplumpname(st.mapnum, "t")
 savetilemap pass, maplumpname(st.mapnum, "p")
 savetilemap emap, maplumpname(st.mapnum, "e")
 SaveZoneMap zmap, maplumpname(st.mapnum, "z")
 SaveNPCL maplumpname(st.mapnum, "l"), st.npc_inst()
 SaveNPCD_fixedlen maplumpname(st.mapnum, "n"), st.npc_def(), st.num_npc_defs
 serdoors game & ".dox", doors(), st.mapnum
 serdoorlinks maplumpname(st.mapnum, "d"), link()
 '--save map name
 DIM mapsave(39) as integer
 mapsave(0) = LEN(mapname)
 str2array LEFT(mapname, 39), mapsave(), 1
 storerecord mapsave(), game & ".mn", 40, st.mapnum
END SUB

SUB verify_map_size (st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, mapname as string)
 IF map(0).wide = pass.wide AND pass.wide = emap.wide AND zmap.wide = emap.wide AND map(0).high = pass.high AND pass.high = emap.high AND zmap.high = emap.high THEN EXIT SUB
 '--Map's X and Y do not match
 st.wide = map(0).wide
 st.high = map(0).high
 clearpage vpage
 DIM j as integer
 j = 0
 textcolor uilook(uiText), 0
 printstr "Map " & st.mapnum & ":" & mapname, 0, j * 8, vpage
 j += 2
 printstr "this map seems to be corrupted", 0, j * 8, vpage
 j += 2
 printstr " TileMap " & map(0).wide & "*" & map(0).high & " tiles, " & (UBOUND(map) + 1) & " layers", 0, j * 8, vpage: j += 1
 printstr " WallMap " & pass.wide & "*" & pass.high & " tiles", 0, j * 8, vpage: j += 1
 printstr " FoeMap " & emap.wide & "*" & emap.high & " tiles", 0, j * 8, vpage: j += 1
 printstr " ZoneMap " & zmap.wide & "*" & zmap.high & " tiles", 0, j * 8, vpage: j += 1
 j += 1
 printstr "Fixing to " & st.wide & "*" & st.high, 0, j * 8, vpage: j += 1
 'A map's size might be due to corruption, besides, the tilemap is far away the most important
 'st.wide = large(map(0).wide, large(pass.wide, emap.wide))
 'st.high = large(map(0).high, large(pass.high, emap.high))
 pass.wide = st.wide: pass.high = st.high
 pass.data = REALLOCATE(pass.data, st.wide * st.high)
 emap.wide = st.wide: emap.high = st.high
 emap.data = REALLOCATE(emap.data, st.wide * st.high)
 IF zmap.wide <> st.wide OR zmap.high <> st.high THEN
  'Zone maps are too tricky, just delete
  CleanZoneMap zmap, st.wide, st.high
 END IF
 'savetilemaps map(), maplumpname(st.mapnum, "t")
 savetilemap pass, maplumpname(st.mapnum, "p")
 savetilemap emap, maplumpname(st.mapnum, "e")
 SaveZoneMap zmap, maplumpname(st.mapnum, "z")
 'loadtilemaps map(), maplumpname(st.mapnum, "t")
 'loadtilemap pass, maplumpname(st.mapnum, "p")
 'loadtilemap emap, maplumpname(st.mapnum, "e")
 j += 1
 printstr "please report this error to", 0, j * 8, vpage: j += 1
 printstr "ohrrpgce@HamsterRepublic.com", 0, j * 8, vpage: j += 1
 setvispage vpage
 waitforanykey
END SUB

SUB mapedit_load_tilesets(st as MapEditState, map() as TileMap, gmap() as integer)
 loadmaptilesets st.tilesets(), gmap()
 v_new st.defaultwalls, UBOUND(map) + 1
 FOR i as integer = 0 TO UBOUND(map)
  loadpasdefaults st.defaultwalls[i], st.tilesets(i)->num
 NEXT
END SUB


'==========================================================================================
'                             Layer manipulation/rearrangement
'==========================================================================================


SUB fix_tilemaps(map() as TileMap)
 'Each tilemap in map() needs to know its index number in map(). This SUB updates that.
 FOR i as integer = 0 TO UBOUND(map)
  map(i).layernum = i
 NEXT
END SUB

SUB mapedit_swap_layers(st as MapEditState, map() as TileMap, vis() as integer, gmap() as integer, byval l1 as integer, byval l2 as integer)
 DIM as bool temp1, temp2
 SWAP map(l1), map(l2)
 fix_tilemaps map()
 SWAP st.usetile(l1), st.usetile(l2)
 SWAP st.menubarstart(l1), st.menubarstart(l2)
 SWAP gmap(layer_tileset_index(l1)), gmap(layer_tileset_index(l2))  'Tileset
 DIM as string name1, name2
 name1 = read_map_layer_name(gmap(), l1)
 name2 = read_map_layer_name(gmap(), l2)
 write_map_layer_name(gmap(), l1, name2)
 write_map_layer_name(gmap(), l2, name1)
 SWAP st.tilesets(l1), st.tilesets(l2)
 temp1 = layerisenabled(gmap(), l1)
 temp2 = layerisenabled(gmap(), l2)
 setlayerenabled(gmap(), l2, temp1)
 setlayerenabled(gmap(), l1, temp2)
 temp1 = layerisvisible(vis(), l1)
 temp2 = layerisvisible(vis(), l2)
 setlayervisible(vis(), l2, temp1)
 setlayervisible(vis(), l1, temp2)
 IF st.layer = l1 THEN
  st.layer = l2
 ELSEIF st.layer = l2 THEN
  st.layer = l1
 END IF
 mapedit_throw_away_history st
 v_free st.cloned
END SUB

SUB mapedit_copy_layer(st as MapEditState, map() as TileMap, gmap() as integer, byval src as integer, byval dest as integer)
 'Copy src layer over dest layer
 'Note: we don't throw away history because it's unnecessary if copy to a new layer
 'at the end; if this is ever used for overwriting an actual existing layer, the caller should
 'call throw away history and the clone buffer.
 gmap(layer_tileset_index(dest)) = gmap(layer_tileset_index(src))  'Tileset
 write_map_layer_name(gmap(), dest, "copy of " & src & " " & read_map_layer_name(gmap(), src))
 CopyTilemap map(dest), map(src)
END SUB

'Add blank map layers
SUB mapedit_append_new_layers(st as MapEditState, map() as TileMap, vis() as integer, gmap() as integer, howmany as integer)
 DIM old_maxlayer as integer = UBOUND(map)
 howmany = small(howmany, maplayerMax - UBOUND(map))
 REDIM PRESERVE map(UBOUND(map) + howmany)
 FOR i as integer = old_maxlayer + 1 to UBOUND(map)
  CleanTilemap map(i), map(0).wide, map(0).high, i
  SetLayerEnabled(gmap(), i, YES)
  SetLayerVisible(vis(), i, YES)
  gmap(layer_tileset_index(i)) = 0  'Tileset = default
  write_map_layer_name(gmap(), i, "")
 NEXT
 fix_tilemaps map()
 mapedit_load_tilesets st, map(), gmap()
END SUB

'where is the index the new layer is to be placed at
'If the new layer is added to the end, then undo history and the clone buffer are preserved
SUB mapedit_insert_new_layer(st as MapEditState, map() as TileMap, vis() as integer, gmap() as integer, where as integer)
 IF UBOUND(map) = maplayerMax THEN EXIT SUB

 'Add to end, then shuffle to correct spot
 mapedit_append_new_layers st, map(), vis(), gmap(), 1
 FOR i as integer = UBOUND(map) - 1 TO where STEP -1
  mapedit_swap_layers st, map(), vis(), gmap(), i, i + 1
 NEXT
 fix_tilemaps map()
 mapedit_load_tilesets st, map(), gmap()
END SUB

SUB mapedit_delete_layer(st as MapEditState, map() as TileMap, vis() as integer, gmap() as integer, byval which as integer)
 'doesn't reload tilesets or passability defaults, layers menu does that
 FOR i as integer = which TO UBOUND(map) - 1
  mapedit_swap_layers st, map(), vis(), gmap(), i, i + 1
 NEXT
 UnloadTilemap map(UBOUND(map))
 'currently (temporarily) tilesets for unused layers are still loaded, so reset to default
 gmap(layer_tileset_index(UBOUND(map))) = 0
 write_map_layer_name(gmap(), UBOUND(map), "")
 REDIM PRESERVE map(UBOUND(map) - 1)
 fix_tilemaps map()
 mapedit_throw_away_history st
 v_free st.cloned
END SUB


'==========================================================================================
'                                  Resizing & deleting
'==========================================================================================


SUB mapedit_resize(st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, gmap() as integer, doors() as Door, link() as DoorLink, mapname as string)
'sizemap:
 DIM rs as MapResizeState
 rs.rect.wide = 0
 rs.rect.high = 0
 rs.rect.x = 0
 rs.rect.y = 0
 'resizemapmenu both inits and deletes rs.menu
 resizemapmenu st, map(), rs
 IF rs.rect.wide = -1 THEN EXIT SUB

 clearpage 0
 clearpage 1
 
 DIM yout as integer = 0
 edgeprint "TILEMAP", 0, yout * 10, uilook(uiText), vpage: setvispage vpage: yout += 1
 resizetiledata map(), rs, yout, vpage
 edgeprint "PASSMAP", 0, yout * 10, uilook(uiText), vpage: setvispage vpage: yout += 1
 resizetiledata pass, rs, yout, vpage
 edgeprint "FOEMAP", 0, yout * 10, uilook(uiText), vpage: setvispage vpage: yout += 1
 resizetiledata emap, rs, yout, vpage
 edgeprint "ZONEMAP", 0, yout * 10, uilook(uiText), vpage: setvispage vpage: yout += 1
 SaveZoneMap zmap, tmpdir & "zresize.tmp", @rs.rect
 LoadZoneMap zmap, tmpdir & "zresize.tmp"
 edgeprint "Deleting Undo History", 0, yout * 10, uilook(uiText), vpage: setvispage vpage: yout += 1
 mapedit_throw_away_history st
 ' update SAV x/y offset in MAP lump
 gmap(20) += rs.rect.x * -1
 gmap(21) += rs.rect.y * -1
 ' update hero's starting position (if on current map)
 IF gen(genStartMap) = st.mapnum THEN
  gen(genStartX) += rs.rect.x * -1
  gen(genStartY) += rs.rect.y * -1 
 END IF
 st.wide = rs.rect.wide
 st.high = rs.rect.high
 '--reset map scroll position
 st.x = 0
 st.y = 0
 st.mapx = 0
 st.mapy = 0
 edgeprint "Aligning and truncating doors", 0, yout * 10, uilook(uiText), vpage: yout += 1
 DIM i as integer
 FOR i as integer = 0 TO 99
  doors(i).x -= rs.rect.x
  doors(i).y -= rs.rect.y
  IF doors(i).x < 0 OR doors(i).y < 0 OR doors(i).x >= st.wide OR doors(i).y >= st.high THEN
   set_door_exists(doors(), i, NO)
  END IF
 NEXT
 edgeprint "Aligning and truncating NPCs", 0, yout * 10, uilook(uiText), vpage: setvispage vpage: yout += 1
 FOR i as integer = 0 TO 299
  WITH st.npc_inst(i)
   .x -= rs.rect.x * 20
   .y -= rs.rect.y * 20
   IF .x < 0 OR .y < 0 OR .x >= st.wide * 20 OR .y >= st.high * 20 THEN
    .id = 0
   END IF
  END WITH
 NEXT i
 verify_map_size st, map(), pass, emap, zmap, mapname
END SUB

SUB mapedit_delete(st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, gmap() as integer, doors() as Door, link() as DoorLink, npc_img() as GraphicPair, mapname as string)
 REDIM options(6) as string
 options(0) = "Cancel!"
 options(1) = "Erase all map data"
 options(2) = "Erase tile data + doors + NPC instances"
 options(3) = "Erase NPC instances"
 options(4) = "Erase NPC instances + definitions"
 options(5) = "Erase doors"
 options(6) = "Erase doorlinks"
 IF st.mapnum = gen(genMaxMap) AND st.mapnum >= 1 THEN
  '--if this is the last map, then we can actually remove it entirely, rather than just blanking it
  str_array_append options(), "Delete map entirely"
 END IF
 DIM choice as integer = multichoice("Delete which map data?", options(), 0, 0, "mapedit_delete")
 IF choice >= 1 AND choice <= 6 THEN
  IF choice = 1 THEN  '--everything
   new_blank_map st, map(), pass, emap, zmap, gmap(), doors(), link()
   mapname = ""
   update_npc_graphics st, npc_img()
   mapedit_throw_away_history st
  ELSEIF choice = 2 THEN  '--just tile related data
   CleanTilemaps map(), st.wide, st.high, 1
   CleanTilemap pass, st.wide, st.high
   CleanTilemap emap, st.wide, st.high
   CleanZoneMap zmap, st.wide, st.high
   CleanNPCL st.npc_inst()
   CleanDoors doors()
   gmap(31) = 1 'Walkabout layer above map layer 0
   mapedit_throw_away_history st
  ELSEIF choice = 3 THEN
   CleanNPCL st.npc_inst()
  ELSEIF choice = 4 THEN
   CleanNPCL st.npc_inst()
   CleanNPCD st.npc_def()
   st.num_npc_defs = 1
   update_npc_graphics st, npc_img()
  ELSEIF choice = 5 THEN
   CleanDoors doors()
  ELSEIF choice = 6 THEN
   CleanDoorlinks link()
  END IF

  '--reset scroll position
  st.x = 0
  st.y = 0
  st.mapx = 0
  st.mapy = 0
  st.layer = 0

  mapedit_savemap st, map(), pass, emap, zmap, gmap(), doors(), link(), mapname
 END IF

 IF choice = 7 THEN
  gen(genMaxMap) -= 1
  safekill maplumpname(st.mapnum, "t")
  safekill maplumpname(st.mapnum, "p")
  safekill maplumpname(st.mapnum, "e")
  safekill maplumpname(st.mapnum, "l")
  safekill maplumpname(st.mapnum, "n")
  safekill maplumpname(st.mapnum, "d")
  safekill maplumpname(st.mapnum, "z")
  'Note .MAP and .MN are not truncated
  'Afterwards, the map editor exits
 END IF
END SUB

'==========================================================================================

SUB update_tilepicker(st as MapEditState)
 st.menubarstart(st.layer) = bound(st.menubarstart(st.layer), large(st.usetile(st.layer) - 13, 0), small(st.usetile(st.layer), st.menubar.wide - 14))
 IF st.tool = clone_tool AND st.multitile_draw_brush THEN
  'Clone tool behaves like Draw: upon changing the drawing tile (including when changing layer), reset
  st.tool = draw_tool
  v_free st.cloned
 END IF
END SUB


'==========================================================================================
'                                      Import/Export
'==========================================================================================


SUB mapedit_export_tilemaps(st as MapEditState)
 DIM outfile as string
 outfile = inputfilename("Export tilemap to which file?", ".tilemap", "", "input_file_export_tilemap")
 IF LEN(outfile) THEN
  copyfile maplumpname(st.mapnum, "t"), outfile + ".tilemap"
 END IF
END SUB

'Copy each imported tilemap to a new tilemap, assuming they are not necessarily
'the same size.
'If appending is false, then deletes existing layers, but doesn't change map size.
SUB mapedit_append_imported_tilemaps(st as MapEditState, map() as TileMap, newlayers() as TileMap, gmap() as integer, visible() as integer, appending as bool)
 DIM dest_layer as integer
 IF appending = NO THEN
  'Preserve the map size here; the resize menu is needed to change that.
  CleanTilemaps map(), st.wide, st.high, UBOUND(newlayers) + 1
  dest_layer = 0
 ELSE
  mapedit_append_new_layers st, map(), visible(), gmap(), 1
  dest_layer = UBOUND(map)
 END IF 
 DIM as integer src_layer, x, y
 FOR src_layer = 0 TO UBOUND(newlayers)
  FOR x = 0 TO small(map(0).wide, newlayers(src_layer).wide) - 1
   FOR y = 0 TO small(map(0).high, newlayers(src_layer).high) - 1
    writeblock(map(dest_layer), x, y, readblock(newlayers(src_layer), x, y))
   NEXT
  NEXT
  dest_layer += 1
 NEXT
 mapedit_load_tilesets st, map(), gmap()
END SUB

'Asks the user to browse for a .tilemap file, then handles the import
'appending: true if appending, false if replacing all existing
SUB mapedit_import_tilemaps(st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, gmap() as integer, visible() as integer, doors() as Door, link() as DoorLink, mapname as string, appending as bool)
 DIM infile as string
 infile = browse(12, "", "*.tilemap", tmpdir, NO, "browse_tilemaps")
 IF LEN(infile) = 0 THEN EXIT SUB

 REDIM newlayers(0) as TileMap
 IF LoadTilemaps(newlayers(), infile, YES) = NO THEN
  pop_warning "Bad file; could not load " & decode_filename(infile)
  EXIT SUB
 END IF

 '--- First we check whether there are too many map layers

 DIM menu_caption as string
 DIM choice as integer
 DIM num_old_layers as integer = UBOUND(map) + 1
 DIM num_new_layers as integer = UBOUND(newlayers) + 1
 IF appending = NO THEN num_old_layers = 0

 IF num_old_layers + num_new_layers > maplayerMax + 1 THEN
  menu_caption = "The new map would have " & (num_old_layers + num_new_layers) & " layers, but the maximum amount is " & (maplayerMax + 1) & "."
  REDIM menu_choices(0) as string
  menu_choices(0) = "Cancel"
  FOR layer as integer = 0 TO num_new_layers - 1
   str_array_append menu_choices(), "Append just layer " & layer & " from the imported tilemap"
  NEXT
  choice = multichoice(menu_caption, menu_choices(), 1, 0)

  IF choice = 0 THEN
   UnloadTilemaps newlayers()
   EXIT SUB
  ELSE
   'keep a single layer
   SWAP newlayers(choice), newlayers(0)
   FOR i as integer = 1 TO UBOUND(newlayers)
    UnloadTilemap newlayers(i)
   NEXT
   REDIM PRESERVE newlayers(0)
  END IF
 END IF

 '--- Then we check the size and handle any problems

 DIM as integer newwide = newlayers(0).wide, newhigh = newlayers(0).high
 IF st.wide <> newwide OR st.high <> newhigh THEN
  REDIM menu_choices(2) as string

  menu_caption = "This tilemap is " & newwide & "*" & newhigh & " with " & (UBOUND(newlayers) + 1) & " layers, while the map is " & st.wide & "*" & st.high & " (with " & (UBOUND(newlayers) + 1) & " layers)."
  menu_caption = !"\nDo you want to resize (or shift) the map first?"
  menu_choices(0) = "Cancel"
  menu_choices(1) = "Let me resize the map"
  menu_choices(2) = "Crop/expand imported tilemaps as needed"

  choice = multichoice(menu_caption, menu_choices(), 1, 0, "mapedit_importing_wrong_size_tilemap")
  IF choice = 0 THEN
   UnloadTilemaps newlayers()
   EXIT SUB
  ELSEIF choice = 1 THEN
   'Goto resize menu
   mapedit_resize st, map(), pass, emap, zmap, gmap(), doors(), link(), mapname
   IF st.wide <> newwide OR st.high <> newhigh THEN  'Either cancelled or wrong size
    notification "Map still the wrong size, cancelling import"
    UnloadTilemaps newlayers()
    EXIT SUB
   END IF
  ELSEIF choice = 2 THEN
   'Crop/extend. This is handled automatically.
  END IF
 END IF

 mapedit_append_imported_tilemaps st, map(), newlayers(), gmap(), visible(), appending
 notification "Imported " & num_new_layers & " layers"

 UnloadTilemaps newlayers()
END SUB

'Overwrite a map layer, reading a tile per pixel of a .bmp
SUB bmp_to_layer(st as MapEditState, imgfile as string, map() as TileMap, layer as integer)
 DIM fr as Frame ptr = frame_import_bmp_raw(imgfile)
 IF fr = NULL THEN
  notification "Couldn't import map layer: loading .bmp failed"
  EXIT SUB
 END IF
 IF fr->h <> st.high OR fr->w <> st.wide THEN
  notification "This image is " & fr->w & "*" & fr->h & " while the map is " & st.wide & "*" & st.high & !".\n" _
               "Any extra portion will be clipped. Resize the map manually as desired."
 END IF
 FOR y as integer = 0 TO small(fr->h, st.high) - 1
  FOR x as integer = 0 TO small(fr->w, st.wide) - 1
   writeblock(map(layer), x, y, readpixel(fr, x, y))
  NEXT
 NEXT
 frame_unload @fr
END SUB

'Import a map layer from a paletted .bmp, one tile per pixel
SUB mapedit_import_bmp_tilemap(st as MapEditState, map() as TileMap, gmap() as integer, visible() as integer)
 pop_warning "Please select an 8-bit paletted .bmp file to import. Each pixel of the image will be mapped to a tile. " _
             !"The n-th colour in the palette becomes tile index n. The actual colours in the palette are ignored.\n" _
             !"This is useful mainly for defining the rough outline of your map.\n" _
             "You may find it easiest to draw the image without considering tile indices, then " _
             "remap them to the correct tiles with the 'Replace' tool."
 DIM imgfile as string
 'Want any bmp with bitdepth at most 8
 imgfile = browse(10, "", "*.bmp", tmpdir, 0, "browse_bmp_tilemap")
 DIM bmpd as BitmapV3InfoHeader
 IF LEN(imgfile) = 0 OR bmpinfo(imgfile, bmpd) <> 2 THEN EXIT SUB

 DIM blank_option as string = "New blank layer"
 IF UBOUND(map) >= maplayerMax THEN blank_option = ""   'remove option
 DIM layer as integer
 layer = mapedit_pick_layer(st, gmap(), map(), "Import as new layer or OVERWRITE existing layer?", blank_option)
 IF layer = -2 THEN EXIT SUB  'cancelled
 IF layer = -1 THEN  'new layer
  mapedit_append_new_layers st, map(), visible(), gmap(), 1
  layer = UBOUND(map)
 END IF

 bmp_to_layer st, imgfile, map(), layer
 notification "Imported layer " & layer
END SUB

'Pick a colour to represent each tile in a tileset: the average colour
SUB color_for_each_tile (tileset as TilesetData ptr, colors() as RGBcolor)
 FOR tile as integer = 0 TO 159
  DIM as double r, g, b
  FOR y as integer = 0 TO 19
   FOR x as integer = 0 TO 19
    DIM idx as integer
    idx = readpixel(tileset->spr, x, tile * 20 + y)
    r += master(idx).r ^ 1.7
    g += master(idx).g ^ 1.7
    b += master(idx).b ^ 1.7
   NEXT
  NEXT
  colors(tile).b = small(255., (b / 400) ^ (1 / 1.7))
  colors(tile).g = small(255., (g / 400) ^ (1 / 1.7))
  colors(tile).r = small(255., (r / 400) ^ (1 / 1.7))
  debug "tile " & tile & " g " & g & "->" & colors(tile).g
 NEXT
 'Then handle animated tiles by making them the same colour as the first tile in the animation
 FOR tile as integer = 160 TO 255
  DIM basetile as integer
  basetile = tile_anim_deanimate_tile(tile, tileset->tastuf())
  colors(tile) = colors(basetile)
 NEXT
END SUB

SUB layer_to_bmp(st as MapEditState, imgfile as string, map() as TileMap, layer as integer)
 DIM colors(255) as RGBcolor
 color_for_each_tile st.tilesets(layer), colors()
 DIM fr as Frame ptr
 fr = frame_new(st.wide, st.high, , YES)
 FOR y as integer = 0 TO st.high - 1
  FOR x as integer = 0 TO st.wide - 1
   putpixel fr, x, y, readblock(map(layer), x, y)
  NEXT
 NEXT
 frame_export_bmp8 imgfile, fr, colors()
 frame_unload @fr
END SUB

SUB mapedit_export_bmp_tilemap(st as MapEditState, map() as TileMap, gmap() as integer)
 DIM menu_caption as string
 menu_caption = "Please select a layer to export to an 8-bit paletted .bmp file. " _
                "Each pixel of the image will represent one tile: " _
                !"color n in the palette means tileset index n.\n" _
                !"This is useful mainly for modifying the large-scale structures of your map."
 REDIM menu_choices(0) as string
 menu_choices(0) = "Cancel"
 FOR layer as integer = 0 TO UBOUND(map)
  str_array_append menu_choices(), "Export map layer " & layer & " " & read_map_layer_name(gmap(), layer)
 NEXT
 DIM choice as integer
 choice = multichoice(menu_caption, menu_choices(), 1, 0)
 IF choice = 0 THEN EXIT SUB
 DIM outfile as string
 DIM defaultname as string
 defaultname = trimextension(trimpath(sourcerpg)) & " map " & st.mapnum & " layer " & (choice - 1) & " " & read_map_layer_name(gmap(), choice - 1)
 defaultname = TRIM(defaultname)  'If no layer name
 outfile = inputfilename("Export tilemap to which file?", ".bmp", "", "input_file_export_bmp_tilemap", defaultname)
 IF LEN(outfile) THEN layer_to_bmp st, outfile + ".bmp", map(), choice - 1
END SUB

SUB mapedit_import_export(st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, gmap() as integer, visible() as integer, doors() as Door, link() as DoorLink, mapname as string)
 DIM menu(5) as string
 menu(0) = "Previous menu"
 menu(1) = "Export tilemap"
 menu(2) = "Import tilemap, overwriting existing"
 menu(3) = "Import tilemap, as new layers"
 menu(4) = "Export map layer as pixel-a-tile BMP"
 menu(5) = "Import pixel-a-tile BMP as map layer"

 DIM state as menustate
 state.top = 0
 state.pt = 0
 state.last = UBOUND(menu)
 state.size = 11
 state.need_update = YES

 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "mapedit_importexport"
  usemenu state
  IF enter_or_space() THEN
   IF state.pt = 0 THEN EXIT DO
   IF state.pt = 1 THEN
    mapedit_export_tilemaps st
   END IF
   IF state.pt = 2 THEN
    IF yesno("Are you sure you want to DELETE your current map layers and import new ones?") THEN
     mapedit_import_tilemaps st, map(), pass, emap, zmap, gmap(), visible(), doors(), link(), mapname, NO
    END IF
   END IF
   IF state.pt = 3 THEN
    mapedit_import_tilemaps st, map(), pass, emap, zmap, gmap(), visible(), doors(), link(), mapname, YES
   END IF
   IF state.pt = 4 THEN
    mapedit_export_bmp_tilemap st, map(), gmap()
   END IF
   IF state.pt = 5 THEN
    mapedit_import_bmp_tilemap st, map(), gmap(), visible()
   END IF
  END IF

  clearpage vpage
  standardmenu menu(), state, 0, 0, vpage
  setvispage vpage
  dowait
 LOOP
END SUB


'==========================================================================================
'                                         Doors
'==========================================================================================


SUB mapedit_linkdoors (st as MapEditState, map() as TileMap, pass as TileMap, gmap() as integer, doors() as Door, link() as DoorLink)
 'Warning: map data should be saved before this SUB is called, as some of it's reloaded from file
 
 DIM state as MenuState
 state.top = 0
 state.pt = 0
 state.last = small(find_last_used_doorlink(link()) + 1, UBOUND(link))
 state.size = 11
 state.need_update = YES

 DIM menu_temp as string
 DIM col as integer

 setkeys
 DO
  setwait 55
  setkeys
  state.tog = state.tog XOR 1
  IF keyval(scESC) > 1 THEN
   serdoorlinks(maplumpname(st.mapnum, "d"), link())
   EXIT DO
  END IF
  IF keyval(scF1) > 1 THEN show_help "mapedit_linkdoors"
  IF usemenu(state) THEN state.need_update = YES
  IF enter_space_click(state) THEN
   IF state.pt = state.last AND link(state.pt).source = -1 THEN link(state.pt).source = 0
   link_one_door st, state.pt, link(), doors(), map(), pass, gmap()
   state.need_update = YES
   IF state.pt = state.last AND link(state.pt).source >= 0 THEN
    state.last = small(state.last + 1, UBOUND(link))
   END IF
  END IF
  IF state.need_update THEN
   state.need_update = NO
   DrawDoorPair st, state.pt, map(), pass, doors(), link(), gmap()
  END IF

  '--Draw screen
  copypage 2, dpage
  FOR i as integer = state.top TO small(state.top + state.size, state.last)
   col = uilook(uiMenuItem)
   IF state.pt = i THEN
    col = uilook(uiSelectedItem + state.tog)
    edgeboxstyle 0, 1 + (i - state.top) * 16, 280, 19, 0, dpage, YES, YES
   END IF

   IF link(i).source >= 0 THEN
    menu_temp = "Door " & link(i).source & " leads to door " & link(i).dest & " on map " & link(i).dest_map
    edgeprint menu_temp, 0, 2 + (i - state.top) * 16, col, dpage

    IF link(i).tag1 = 0 AND link(i).tag2 = 0 THEN
     menu_temp = "  all the time"
    ELSE
     menu_temp = "  only if tag "
     IF link(i).tag1 <> 0 THEN
      menu_temp += ABS(link(i).tag1) & " is " & sign_string(link(i).tag1, "OFF", "", "ON")
     END IF
     IF link(i).tag2 THEN
      IF link(i).tag1 THEN menu_temp += " and tag "
      menu_temp += ABS(link(i).tag2) & " is " & sign_string(link(i).tag2, "OFF", "", "ON")
     END IF
    END IF
    edgeprint menu_temp, 0, 10 + (i - state.top) * 16, col, dpage
   ELSEIF i = state.last THEN
    menu_temp = "Create a new doorlink..."
    edgeprint menu_temp, 0, 2 + (i - state.top) * 16, col, dpage
   ELSE
    menu_temp = "Unused Door link #" & i
    edgeprint menu_temp, 0, 2 + (i - state.top) * 16, col, dpage
   END IF
  NEXT i
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB

SUB link_one_door(st as MapEditState, linknum as integer, link() as DoorLink, doors() as Door, map() as TileMap, pass as TileMap, gmap() as integer)
 DIM ulim(4) as integer, llim(4) as integer
 ulim(0) = 99: llim(0) = -1
 ulim(1) = 99: llim(1) = 0
 ulim(2) = gen(genMaxMap): llim(2) = 0
 ulim(3) = max_tag(): llim(3) = -max_tag()
 ulim(4) = max_tag(): llim(4) = -max_tag()

 DIM menu(-1 TO 4) as string
 menu(-1) = "Go Back"
 menu(0) = "Entrance Door"
 menu(1) = "Exit Door"
 menu(2) = "Exit Map"
 menu(3) = "Require Tag"
 menu(4) = "Require Tag"
 
 DIM state as MenuState
 state.pt = -1
 state.top = -1
 state.size = 22
 state.first = LBOUND(menu)
 state.last = UBOUND(menu)
 
 DIM preview_delay as integer = 0
 DIM outmap as string
 outmap = getmapname(link(linknum).dest_map)
 DIM menu_temp as string
 DIM col as integer

 DrawDoorPair st, linknum, map(), pass, doors(), link(), gmap()

 setkeys
 DO
  setwait 55
  setkeys
  state.tog = state.tog XOR 1
  IF preview_delay > 0 THEN
   preview_delay -= 1
   IF preview_delay = 0 THEN DrawDoorPair st, linknum, map(), pass, doors(), link(), gmap()
  END IF
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "door_link_editor"
  usemenu state
  IF state.pt >= 0 THEN
   SELECT CASE state.pt
    CASE 0
     IF intgrabber(link(linknum).source, llim(state.pt), ulim(state.pt)) THEN preview_delay = 3
    CASE 1
     IF intgrabber(link(linknum).dest, llim(state.pt), ulim(state.pt)) THEN preview_delay = 3
    CASE 2
     IF intgrabber(link(linknum).dest_map, llim(state.pt), ulim(state.pt)) THEN
      preview_delay = 3
      outmap = getmapname(link(linknum).dest_map)
     END IF
    CASE 3
     tag_grabber link(linknum).tag1
    CASE 4
     tag_grabber link(linknum).tag2
    CASE ELSE
     '...
   END SELECT
  ELSE
   IF enter_space_click(state) THEN EXIT DO
  END IF
  '--Draw screen
  copypage 2, dpage
  rectangle 0, 99, 320, 2, uilook(uiSelectedDisabled + state.tog), dpage
  FOR i as integer = -1 TO 4
   menu_temp = ""
   SELECT CASE i
    CASE 0
     IF link(linknum).source >= 0 THEN
      menu_temp = STR(link(linknum).source)
     ELSE
      menu_temp = "Unused"
     END IF
    CASE 1
     menu_temp = STR(link(linknum).dest)
    CASE 2
     menu_temp = STR(link(linknum).dest_map)
    CASE 3
     menu_temp = tag_condition_caption(link(linknum).tag1, "", "Always")
    CASE 4
     menu_temp = tag_condition_caption(link(linknum).tag2, "", "Always")
   END SELECT
   col = uilook(uiMenuItem)
   IF state.pt = i THEN col = uilook(uiSelectedItem + state.tog)
   edgeprint menu(i) & " " & menu_temp, 1, 1 + (i + 1) * 10, col, dpage
  NEXT i
  edgeprint "ENTER", 275, 0, uilook(uiText), dpage
  edgeprint "EXIT", 283, 190, uilook(uiText), dpage
  edgeprint outmap, 0, 190, uilook(uiText), dpage
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB


'==========================================================================================

FUNCTION LayerIsVisible(vis() as integer, byval l as integer) as bool
 'debug "layer #" & l & " is: " & readbit(vis(), 0, l)
 RETURN xreadbit(vis(), l)
END FUNCTION

FUNCTION LayerIsEnabled(gmap() as integer, byval l as integer) as bool
 IF l <= 0 THEN RETURN YES
 'debug "layer #" & l & " is: " & readbit(gmap(), 19, l-1)
 RETURN xreadbit(gmap(), l - 1, 19)
END FUNCTION

SUB SetLayerVisible(vis() as integer, byval l as integer, byval v as bool)
 setbit(vis(), 0, l, v)
END SUB

SUB SetLayerEnabled(gmap() as integer, byval l as integer, byval v as bool)
 IF l <= 0 THEN EXIT SUB
 setbit(gmap(), 19, l - 1, v)
END SUB

SUB ToggleLayerVisible(vis() as integer, byval l as integer)
 setbit(vis(), 0, l, readbit(vis(), 0, l) xor 1)
END SUB

SUB ToggleLayerEnabled(gmap() as integer, byval l as integer)
 IF l <= 0 THEN EXIT SUB
 setbit(gmap(), 19, l - 1, readbit(gmap(), 19, l - 1) xor 1)
END SUB


'==========================================================================================

SUB DrawDoorPair(st as MapEditState, byval linknum as integer, map() as TileMap, pass as TileMap, doors() as door, link() as doorlink, gmap() as integer)
 DIM as integer dmx, dmy, i
 DIM as string caption
 DIM destdoor(99) as door
 DIM destmap as integer
 DIM gmap2(dimbinsize(binMAP)) as integer
 REDIM map2(0) as TileMap
 DIM pass2 as TileMap
 DIM tilesets2(maplayerMax) as TilesetData ptr
 
 clearpage 2
 IF link(linknum).source = -1 THEN EXIT SUB

 IF door_exists(doors(), link(linknum).source) THEN
  dmx = doors(link(linknum).source).x * 20 - 150
  dmy = doors(link(linknum).source).y * 20 - 65
  dmx = small(large(dmx, 0), map(0).wide * 20 - 320)
  dmy = small(large(dmy, 0), map(0).high * 20 - 100)
  FOR i as integer = 0 TO UBOUND(map)
   IF LayerIsEnabled(gmap(), i) THEN
    drawmap map(i), dmx, dmy, st.tilesets(i), 2, i <> 0, , , 0, 99
   END IF
  NEXT i
  IF LayerIsEnabled(gmap(), 0) THEN
   drawmap map(0), dmx, dmy, st.tilesets(0), 2, 0, 2, @pass, 0, 99
  END IF
  edgebox doors(link(linknum).source).x * 20 - dmx, doors(link(linknum).source).y * 20 - dmy - 20, 20, 20, uilook(uiMenuItem), uilook(uiBackground), 2
  textcolor uilook(uiBackground), 0
  caption = STR(link(linknum).source)
  printstr caption, doors(link(linknum).source).x * 20 - dmx + 10 - (4 * LEN(caption)), doors(link(linknum).source).y * 20 - dmy - 14, 2
 END IF
 '-----------------EXIT DOOR
 destmap = link(linknum).dest_map
 loadrecord gmap2(), game + ".map", getbinsize(binMAP) \ 2, destmap
 deserdoors game + ".dox", destdoor(), destmap
 LoadTilemaps map2(), maplumpname(destmap, "t")
 LoadTilemap pass2, maplumpname(destmap, "p")
 loadmaptilesets tilesets2(), gmap2()

 IF door_exists(destdoor(), link(linknum).dest) THEN
  dmx = destdoor(link(linknum).dest).x * 20 - 150
  dmy = destdoor(link(linknum).dest).y * 20 - 65
  dmx = small(large(dmx, 0), map2(0).wide * 20 - 320)
  dmy = small(large(dmy, 0), map2(0).high * 20 - 100)
  FOR i as integer = 0 TO UBOUND(map2)
   IF LayerIsEnabled(gmap2(), i) THEN
     drawmap map2(i), dmx, dmy, tilesets2(i), 2, i <> 0, , , 101
   END IF
  NEXT i
  IF LayerIsEnabled(gmap2(), 0) THEN
   drawmap map2(0), dmx, dmy, tilesets2(0), 2, 0, 2, @pass, 101
  END IF
  edgebox destdoor(link(linknum).dest).x * 20 - dmx, destdoor(link(linknum).dest).y * 20 - dmy + 80, 20, 20, uilook(uiMenuItem), uilook(uiBackground), 2
  textcolor uilook(uiBackground), 0
  caption = STR(link(linknum).dest)
  printstr caption, destdoor(link(linknum).dest).x * 20 - dmx + 10 - (4 * LEN(caption)), destdoor(link(linknum).dest).y * 20 - dmy + 86, 2
 END IF
 unloadtilemaps map2()
 unloadtilemap pass2
 unloadmaptilesets tilesets2()
END SUB

SUB calculatepassblock(st as MapEditState, x as integer, y as integer, map() as TileMap, pass as TileMap)
 DIM n as integer = 0
 DIM tilenum as integer
 FOR i as integer = 0 TO UBOUND(map)
  tilenum = readblock(map(i), x, y)
  IF i = 0 OR tilenum > 0 THEN
   n = n OR st.defaultwalls[i][tile_anim_deanimate_tile(tilenum, st.tilesets(i)->tastuf())]
  END IF
 NEXT i
 DIM oldval as integer = readblock(pass, x, y)
 IF oldval <> n THEN
  add_undo_step st, x, y, oldval, mapIDPass
  writeblock pass, x, y, n
 END IF
END SUB


'==========================================================================================
'                                      Map resizing
'==========================================================================================

' (Actual resize routine, mapedit_resize, is above)

SUB resizetiledata (tmap as TileMap, rs as MapResizeState, byref yout as integer, page as integer)
 resizetiledata tmap, rs.rect.x, rs.rect.y, rs.rect.wide, rs.rect.high, yout, page
END SUB

SUB resizetiledata (tmaps() as TileMap, rs as MapResizeState, byref yout as integer, page as integer)
 FOR i as integer = 0 TO UBOUND(tmaps)
  resizetiledata tmaps(i), rs.rect.x, rs.rect.y, rs.rect.wide, rs.rect.high, yout, page
 NEXT
END SUB

SUB resizetiledata (tmap as TileMap, x_off as integer, y_off as integer, new_width as integer, new_height as integer, byref yout as integer, page as integer)
 edgeprint "Resizing Map...", 0, yout * 10, uilook(uiText), page
 yout += 1
 setvispage page

 dim tmp as TileMap
 cleantilemap tmp, new_width, new_height
 tmp.layernum = tmap.layernum  'the unexpected ingredient!

 dim as integer x, y
 for x = large(x_off, 0) to small(tmap.wide, new_width + x_off) - 1
	for y = large(y_off, 0) to small(tmap.high, new_height + y_off) - 1
		'newarray((x - tempx) * tempw + (y - tempy) + 2) = tmp(x * st.wide + y + 2)
		writeblock(tmp, x - x_off, y - y_off, readblock(tmap, x, y))
	next
 next
 unloadtilemap tmap
 memcpy(@tmap, @tmp, sizeof(TileMap))
 'obviously don't free tmp
END SUB

SUB resizemapmenu (st as MapEditState, map() as TileMap, byref rs as MapResizeState)
 'returns the new size and offset in passed args, or -1 width to cancel

 ClearMenuData rs.menu
 WITH rs.menu
  .anchor.x = -1
  .anchor.y = 1
  .offset.x = -160
  .offset.y = 100
  .align = -1
  .no_box = YES
  .bordersize = -8
 END WITH
 append_menu_item rs.menu, "Cancel"
 FOR i as integer = 1 TO 6
  append_menu_item rs.menu, ""
 NEXT i
 rs.menu.items[5]->disabled = YES
 rs.menu.items[6]->disabled = YES

 DIM state as MenuState
 state.active = YES
 init_menu_state state, rs.menu
 state.pt = 1
 state.last = 4
 
 DIM incval as integer = 0
 DIM drawoff as XYPair
 
 rs.zoom = 0
 rs.oldsize.x = map(0).wide
 rs.oldsize.y = map(0).high
 rs.rect.wide = rs.oldsize.x
 rs.rect.high = rs.oldsize.y
 rs.rect.x = 0
 rs.rect.y = 0
 
 resize_rezoom_mini_map st, rs, map()
 resize_buildmenu rs
 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(scESC) > 1 THEN
   'Unlike every other menu, hitting ESC cancels changes, so confirm if changes were made
   IF (rs.rect.x = 0 AND rs.rect.y = 0 AND rs.rect.wide = rs.oldsize.x AND rs.rect.high = rs.oldsize.y) _
      ORELSE yesno("Cancel resize?", NO) THEN
    rs.rect.wide = -1
    rs.rect.high = -1
    EXIT DO
   END IF
  END IF
  IF keyval(scF1) > 1 THEN show_help "resize_map"
  usemenu state
  IF keyval(scLeftShift) > 0 OR keyval(scRightShift) > 0 THEN incval = 8 ELSE incval = 1
  SELECT CASE state.pt
   CASE 0
    IF keyval(scEnter) > 1 THEN
     rs.rect.wide = -1
     rs.rect.high = -1
     EXIT DO
    END IF
   CASE 1
    IF keyval(scLeft) > 0 THEN rs.rect.wide -= incval 
    IF keyval(scRight) > 0 THEN rs.rect.wide += incval
    resize_correct_width st, rs, map()
   CASE 2
    IF keyval(scLeft) > 0 THEN rs.rect.high -= incval 
    IF keyval(scRight) > 0 THEN rs.rect.high += incval
    resize_correct_height st, rs, map()
   CASE 3
    IF keyval(scLeft) > 0 THEN rs.rect.x -= incval: rs.rect.wide += incval
    IF keyval(scRight) > 0 THEN rs.rect.x += incval: rs.rect.wide -= incval
    resize_correct_width st, rs, map()
   CASE 4
    IF keyval(scLeft) > 0 THEN rs.rect.y -= incval: rs.rect.high += incval
    IF keyval(scRight) > 0 THEN rs.rect.y += incval: rs.rect.high -= incval
    resize_correct_height st, rs, map()
  END SELECT
  IF keyval(scEnter) > 1 THEN EXIT DO

  clearpage dpage
  drawoff.x = large(0, -rs.rect.x * rs.zoom)
  drawoff.y = large(0, -rs.rect.y * rs.zoom)
  frame_draw rs.minimap, NULL, drawoff.x, drawoff.y, 1, NO, dpage
  draw_menu rs.menu, state, dpage
  drawbox drawoff.x + rs.zoom * rs.rect.x, drawoff.y + rs.zoom * rs.rect.y, rs.zoom * rs.rect.wide, rs.zoom * rs.rect.high, 14 + state.tog, 1, dpage

  SWAP dpage, vpage
  setvispage vpage
  dowait
 LOOP
 frame_unload @(rs.minimap)
 ClearMenuData rs.menu

END SUB

SUB resize_correct_width(st as MapEditState, byref rs as MapResizeState, map() as TileMap)
 rs.rect.wide = bound(rs.rect.wide, 16, mapTilesMax)
 rs.rect.x = bound(rs.rect.x, -rs.rect.wide + 1, rs.oldsize.x - 1)
 WHILE rs.rect.high * rs.rect.wide > mapTilesMax AND rs.rect.high > 10
  rs.rect.high -= 1
 WEND
 resize_dimchange st, rs, map()
END SUB

SUB resize_correct_height(st as MapEditState, byref rs as MapResizeState, map() as TileMap)
 rs.rect.high = bound(rs.rect.high, 10, mapTilesMax)
 rs.rect.y = bound(rs.rect.y, -rs.rect.high + 1, rs.oldsize.y - 1)
 WHILE rs.rect.high * rs.rect.wide > mapTilesMax AND rs.rect.wide > 16
  rs.rect.wide -= 1
 WEND
 resize_dimchange st, rs, map()
END SUB

SUB resize_dimchange(st as MapEditState, byref rs as MapResizeState, map() as TileMap)
 WHILE rs.rect.high * rs.rect.wide > mapTilesMax
  rs.rect.high = large(rs.rect.high - 1, 10)
  rs.rect.wide = large(rs.rect.wide - 1, 16)
 WEND
 resize_rezoom_mini_map st, rs, map()
 resize_buildmenu rs
END SUB

SUB resize_buildmenu(byref rs as MapResizeState)
 rs.menu.items[1]->caption = "Width " & rs.oldsize.x & CHR(26) & rs.rect.wide
 rs.menu.items[2]->caption = "Height " & rs.oldsize.y & CHR(26) & rs.rect.high
 IF rs.rect.x > 0 THEN
  rs.menu.items[3]->caption = "Left edge: trim " & rs.rect.x & " tiles"
 ELSE
  rs.menu.items[3]->caption = "Left edge: add " & -rs.rect.x & " tiles"
 END IF
 IF rs.rect.y > 0 THEN
  rs.menu.items[4]->caption = "Top edge: trim " & rs.rect.y & " tiles"
 ELSE
  rs.menu.items[4]->caption = "Top edge: add " & -rs.rect.y & " tiles"
 END IF
 rs.menu.items[5]->caption = "Area " & (rs.oldsize.x * rs.oldsize.y) & CHR(26) & (rs.rect.high * rs.rect.wide)
 rs.menu.items[6]->caption = rs.zoom & "x zoom"
END SUB

SUB resize_rezoom_mini_map(st as MapEditState, byref rs as MapResizeState, map() as TileMap)
 DIM lastzoom as integer
 lastzoom = rs.zoom
 DIM as integer tw, th
 tw = large(rs.oldsize.x, rs.rect.x + rs.rect.wide) 'right most point
 IF rs.rect.x < 0 THEN tw -= rs.rect.x   'plus left most
 th = large(rs.oldsize.y, rs.rect.y + rs.rect.high)
 IF rs.rect.y < 0 THEN th -= rs.rect.y
 rs.zoom = bound(small(320 \ tw, 200 \ th), 1, 20)
 IF rs.zoom <> lastzoom THEN
  frame_unload @(rs.minimap)
  rs.minimap = createminimap(map(), st.tilesets(), rs.zoom)
 END IF
END SUB

SUB show_minimap(st as MapEditState, map() as TileMap)
 DIM minimap as Frame Ptr
 minimap = createminimap(map(), st.tilesets())

 clearpage vpage
 frame_draw minimap, NULL, 0, 0, 1, NO, vpage
 frame_unload @minimap

 edgeprint "Press Any Key", 0, 180, uilook(uiText), vpage
 setvispage vpage
 waitforanykey
END SUB


'==========================================================================================
'                                    Fill (paint) tool
'==========================================================================================


SUB fill_map_add_node(st as MapEditState, byval followTile as integer, byval oldTile as integer, byval x as integer, byval y as integer, byref head as integer, queue() as XYPair, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, reader as FnReader)
 IF (y < emap.high) AND (y >= 0) AND (x < emap.wide) AND (x >= 0) THEN  'emap is not special
  IF reader(st, x, y, , map(), pass, emap, zmap) = followTile THEN
   IF st.reader(st, x, y, , map(), pass, emap, zmap) = oldTile THEN   'Would be interesting to see whether this redundant check speeds or slows things
    queue(head).x = x
    queue(head).y = y
    head = (head + 1) MOD UBOUND(queue)
   END IF
  END IF
 END IF
END SUB

'tile fill (paint bucket) tool: iterate through all contiguous tiles
'
'do a breadth first search instead of using the stack; that's prone to overflow
'reader is a FnReader for a map on which the continuous regions is sought
SUB fill_map_area(st as MapEditState, byval x as integer, byval y as integer, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, reader as FnReader)
 DIM as integer oldtile, newTile, followTile
 oldTile = st.reader(st, x, y, , map(), pass, emap, zmap)
 followTile = reader(st, x, y, , map(), pass, emap, zmap)
 newTile = st.tool_value
 IF oldTile = newTile THEN EXIT SUB
 REDIM queue(250) as XYPair 'a circular buffer. We don't use the last element
 DIM as integer head, tail, i, oldend
 fill_map_add_node st, followTile, oldTile, x, y, head, queue(), map(), pass, emap, zmap, reader
 WHILE tail <> head
  'resizing inside fill_map_add_node would invalidate the WITH pointers, so make sure there's at least 4 empty slots
  IF (tail - head + UBOUND(queue)) MOD UBOUND(queue) <= 4 THEN
   oldend = UBOUND(queue)
   REDIM PRESERVE queue(UBOUND(queue) * 2)
   IF head < tail THEN
    FOR i as integer = 0 TO head - 1
     queue(oldend + i) = queue(i)
    NEXT
    head += oldend
   END IF
  END IF

  WITH queue(tail)
   IF st.reader(st, .x, .y, , map(), pass, emap, zmap) = oldTile THEN
    st.brush(st, .x, .y, newTile, , map(), pass, emap, zmap)

    fill_map_add_node st, followTile, oldTile, .x + 1, .y, head, queue(), map(), pass, emap, zmap, reader
    fill_map_add_node st, followTile, oldTile, .x - 1, .y, head, queue(), map(), pass, emap, zmap, reader
    fill_map_add_node st, followTile, oldTile, .x, .y + 1, head, queue(), map(), pass, emap, zmap, reader
    fill_map_add_node st, followTile, oldTile, .x, .y - 1, head, queue(), map(), pass, emap, zmap, reader
   END IF
  END WITH
  tail = (tail + 1) MOD UBOUND(queue)
 WEND
END SUB

'Finding a continuous area by reading from a map with the given reader,
'fill that area using st.brush and st.tool_value. This is done by first
'drawing a stencil to a temporary TileMap and then copying it
SUB fill_with_other_area(st as MapEditState, byval x as integer, byval y as integer, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, reader as FnReader)
 DIM oldbrush as FnBrush = st.brush
 DIM oldreader as FnReader = st.reader
 DIM oldvalue as integer = st.tool_value
 st.brush = @tempbrush
 st.reader = @tempreader
 st.tool_value = 1

 CleanTileMap st.temptilemap, st.wide, st.high

 fill_map_area st, x, y, map(), pass, emap, zmap, reader

 st.brush = oldbrush
 st.reader = oldreader
 st.tool_value = oldvalue

 FOR ty as integer = 0 TO st.high - 1
  FOR tx as integer = 0 TO st.wide - 1
   'IF tempreader(st, tx, ty, , map(), pass, emap, zmap) THEN
   IF readblock(st.temptilemap, tx, ty) THEN
    st.brush(st, tx, ty, st.tool_value, , map(), pass, emap, zmap)
   END IF
  NEXT
 NEXT
 UnloadTileMap st.temptilemap
END SUB


'==========================================================================================


SUB loadpasdefaults (byref defaults as integer vector, tilesetnum as integer)
 DIM buf(160) as integer
 v_new defaults, 160
 '--load defaults from tile set defaults file
 loadrecord buf(), workingdir & SLASH & "defpass.bin", 322 \ 2, tilesetnum
 '--enforce magic number and filesize
 IF buf(160) = 4444 THEN
  FOR i as integer = 0 TO 159
   defaults[i] = buf(i)
  NEXT
 ELSE
  'I wonder what this old unsupported file format was?
  debug "Unsupported default tile passability format"
 END IF
END SUB

SUB savepasdefaults (byref defaults as integer vector, tilesetnum as integer)
 DIM buf(160) as integer
 FOR i as integer = 0 TO 159
  buf(i) = defaults[i]
 NEXT  
 '--set magic number
 buf(160) = 4444
 '--write defaults into tile set defaults file
 storerecord buf(), workingdir & SLASH & "defpass.bin", 322 \ 2, tilesetnum
END SUB


'==========================================================================================
'                              Tile picker (tileset screen)
'==========================================================================================


'Create a clone brush from a section of the tileset
SUB mapedit_pick_tileset_rect(st as MapEditState, tilesetview as TileMap, corner1 as XYPair, corner2 as XYPair)
 DIM select_rect as RectType
 corners_to_rect_inclusive corner1, corner2, select_rect
 IF select_rect.wide = 1 AND select_rect.high = 1 THEN
  'No need to create a clone brush
  IF st.tool = clone_tool THEN st.tool = draw_tool
  EXIT SUB
 END IF

 st.tool = clone_tool
 st.multitile_draw_brush = YES  'Cause the clone tool to act like the draw tool (maybe best to have a separate tool)
 st.clone_offset.x = select_rect.wide \ 2
 st.clone_offset.y = select_rect.high \ 2
 st.clone_size.w = select_rect.wide
 st.clone_size.h = select_rect.high

 v_new st.cloned
 FOR xoff as integer = 0 TO select_rect.wide - 1
  DIM x as integer = xoff + select_rect.x
  FOR yoff as integer = 0 TO select_rect.high - 1
   DIM y as integer = yoff + select_rect.y
   DIM tile as integer = readblock(tilesetview, x, y)
   add_change_step st.cloned, xoff, yoff, tile, mapIDLayer + st.layer
  NEXT
 NEXT
END SUB

SUB mapedit_pickblock_setup_tileset(st as MapEditState, tilesetview as TileMap, tilesetdata as TilesetData ptr, tilepick as XYPair)
 tilesetview.layernum = 1
 cleantilemap tilesetview, 16, 16
 ' If the selected tile isn't found (because we hide animated tiles if the animation
 ' pattern is empty), default
 tilepick = TYPE(0, 0)

 'First 10 rows are regular tiles
 FOR i as integer = 0 TO 159
  writeblock tilesetview, i MOD 16, i \ 16, i
  IF i = st.usetile(st.layer) THEN tilepick = TYPE(i MOD 16, i \ 16)
 NEXT

 'Then draw the two animation pattern ranges if they're used,
 '3 rows each.
 DIM tiley as integer = 10
 FOR pattern as integer = 0 TO 1
  IF tile_anim_is_empty(pattern, tilesetdata->tastuf()) = NO THEN
   FOR i as integer = 0 TO 47
    DIM tileid as integer = 160 + 48 * pattern + i
    DIM tilepos as XYPair = (i MOD 16, tiley + i \ 16)
    writeblock tilesetview, tilepos.x, tilepos.y, tileid
    IF tileid = st.usetile(st.layer) THEN tilepick = tilepos
   NEXT
   tiley += 3
  END IF
 NEXT
 tilesetview.high = tiley 
END SUB

'Pick either a tile from the tileset or a rectangle.
'FIXME: if this were cleaned up to return a tile instead of modifying st.usetile, it could be called
'from the general map settings menu.
SUB mapedit_pickblock(st as MapEditState)
 DIM tilepick as XYPair  'Coordinates (in tiles) of the selected tile in tilesetview
 DIM dragging as bool = NO
 DIM chequer_scroll as integer
 DIM tog as integer
 DIM holdpos as XYPair
 DIM scrolly as integer = 0 'Y position in pixels of the camera/top of the screen
 DIM bgcolor as integer = 0
 IF st.layer > 0 THEN bgcolor = -1  'scrolling chequer pattern

 DIM tilesetdata as TilesetData ptr = st.tilesets(st.layer)

 'Show a view of selectable tiles. The order of the tiles and the size doesn't
 'have to be the same as the tileset. This is to allow more flexible tile animations
 'systems in future, and larger tilesets.
 DIM tilesetview as TileMap
 mapedit_pickblock_setup_tileset st, tilesetview, tilesetdata, tilepick

 'The animated tiles are very annoying, so hide them by default unless already selected
 DIM show_animated_tiles as bool = YES
 IF st.usetile(st.layer) < 160 THEN show_animated_tiles = NO
 DIM real_tilesetview_high as integer = tilesetview.high

 setkeys
 DO
  setwait 27, 55
  setkeys
  IF keyval(scESC) > 1 THEN
   update_tilepicker st
   EXIT DO
  END IF
  IF keyval(scF1) > 1 THEN show_help "mapedit_tilemap_picktile"

  IF (keyval(scEnter) > 1 OR keyval(scSpace) > 1) AND dragging = NO THEN
   'start drag-selecting a box
   dragging = YES
   holdpos = tilepick
  END IF
  IF keyval(scEnter) = 0 AND keyval(scSpace) = 0 AND dragging = YES THEN
   update_tilepicker st
   mapedit_pick_tileset_rect st, tilesetview, tilepick, holdpos
   EXIT DO
  END IF
  IF dragging = NO AND keyval(scA) > 1 THEN
   show_animated_tiles XOR= YES
  END IF

  tilesetview.high = IIF(show_animated_tiles, real_tilesetview_high, 10)

  DIM repeatms as integer = 80
  IF keyval(scShift) > 0 THEN repeatms = 40
  IF slowkey(scUp, repeatms) AND tilepick.y > 0 THEN tilepick.y -= 1
  IF slowkey(scDown, repeatms) AND tilepick.y < tilesetview.high - 1 THEN tilepick.y += 1
  IF slowkey(scLeft, repeatms) AND tilepick.x > 0 THEN tilepick.x -= 1
  IF slowkey(scRight, repeatms) AND tilepick.x < tilesetview.wide - 1 THEN tilepick.x += 1
  ' This is NOT tile ID, it's linearised index in tilesetview
  DIM tileoffset as integer = int_from_xy(tilepick, tilesetview.wide, tilesetview.high)
  IF slowkey(scComma, repeatms) AND tileoffset > 0 THEN tileoffset -= 1
  IF slowkey(scPeriod, repeatms) AND tileoffset < tilesetview.wide * tilesetview.high - 1 THEN tileoffset += 1
  tilepick = xy_from_int(tileoffset, tilesetview.wide, tilesetview.high)
  st.usetile(st.layer) = readblock(tilesetview, tilepick.x, tilepick.y)

  ' Keep the selected tile in view
  scrolly = bound(scrolly, tilepick.y * 20 + 30 - vpages(vpage)->h, tilepick.y * 20 - 10)
  ' Don't over-scroll
  scrolly = large(small(scrolly, tilesetview.high * 20 - vpages(vpage)->h), 0)

  'Draw screen
  draw_background 0, 0, vpages(vpage)->w, vpages(vpage)->h, bgcolor, chequer_scroll, vpages(vpage)
  drawmap tilesetview, 0, scrolly, tilesetdata, vpage, YES
  DIM as integer infoline_y = 0, infoline2_y = 10
  IF tilepick.y * 20 - scrolly < vpages(vpage)->h - 80 THEN
   infoline_y = vpages(vpage)->h - 10
   infoline2_y = vpages(vpage)->h - 20
  END IF
  edgeprint "Tile " & st.usetile(st.layer), 0, infoline_y, uilook(uiText), vpage
  IF st.usetile(st.layer) >= 160 THEN
   edgeprint "(Animation set " & ((st.usetile(st.layer) - 160) \ 48) & ")", 0, infoline2_y, uilook(uiText), vpage
  END IF
  DIM infotext as string = "Hold to select a rectangle"
  edgeprint infotext, vpages(vpage)->w - textwidth(infotext), infoline_y, uilook(uiText), vpage

  IF dragging THEN
   DIM select_rect as RectType
   corners_to_rect_inclusive tilepick, holdpos, select_rect
   drawbox select_rect.x * 20, select_rect.y * 20 - scrolly, _
           select_rect.wide * 20, select_rect.high * 20, _
           uilook(uiHighlight + tog), 2, vpage
  ELSE
   frame_draw st.cursor.sprite + tog, st.cursor.pal, tilepick.x * 20, tilepick.y * 20 - scrolly, , , vpage
  END IF
  setvispage vpage
  IF dowait THEN
   tog = tog XOR 1
   chequer_scroll += 1
   'Update tile animations
   cycletile tilesetdata->anim(), tilesetdata->tastuf()
  END IF
 LOOP
 unloadtilemap tilesetview
END SUB


'==========================================================================================
'                                      Undo History
'==========================================================================================


SUB mapedit_throw_away_history(st as MapEditState)
 v_resize st.history, 0
 st.history_step = 0
 st.history_size = 0
END SUB

SUB mapedit_show_undo_change(st as MapEditState, byval undostroke as MapEditUndoTile vector)
 DIM seen_change as integer = NO
 DIM cursorpos as MapEditUndoTile ptr = NULL

 FOR i as integer = 0 TO v_len(undostroke) - 1
  WITH undostroke[i]
   SELECT CASE .mapid
    CASE mapIDMetaCursor
     cursorpos = @undostroke[i]
    CASE mapIDMetaEditmode + tile_mode
     'Tiles are always visible, no need to change to tile mode
    CASE mapIDMetaEditmode + zone_mode
     st.seteditmode = zone_mode
     IF st.zonesubmode = zone_edit_mode THEN st.cur_zone = .value
    CASE mapIDMetaEditmode TO mapIDMetaEditmodeEND
     st.seteditmode = .mapid - mapIDMetaEditmode
    CASE ELSE
     IF seen_change = NO THEN seen_change = mapedit_on_screen(st, .x, .y)
   END SELECT
  END WITH
 NEXT

 IF seen_change = NO THEN
  'No visible change for the user? Then move to the cursor pos
  IF cursorpos THEN
   st.x = cursorpos->x
   st.y = cursorpos->y
   IF mapedit_on_screen(st, st.x, st.y) = NO THEN mapedit_focus_camera st, st.x, st.y
  END IF
 END IF
END SUB

'Note that changelist is byref
SUB add_change_step(byref changelist as MapEditUndoTile vector, byval x as integer, byval y as integer, byval value as integer, byval mapid as integer)
 WITH *v_expand(changelist)
  .x = x
  .y = y
  .value = value
  .mapid = mapid
 END WITH
END SUB

SUB add_undo_step(st as MapEditState, byval x as integer, byval y as integer, byval oldvalue as integer, byval mapid as integer)
 IF st.secondary_undo_buffer THEN
  'Used while previewing a change (pasting a stamp). Note that we don't write metadata
  add_change_step st.secondary_undo_buffer, x, y, oldvalue, mapid
  EXIT SUB
 END IF

 IF st.new_stroke THEN
  st.new_stroke = NO

  'Throw away any redo steps
  FOR i as integer = st.history_step TO v_len(st.history) - 1
   st.history_size -= v_len(st.history[i])
  NEXT
  v_resize st.history, st.history_step
  st.history_step += 1
  'debug "add_undo_step: set st.history_step to " & st.history_step

  'Limit size of undo history
  DIM trim_amount as integer = 0
  WHILE st.history_size > maxMapHistoryMem
   IF v_len(st.history) <= trim_amount THEN
    showerror "add_undo_step: garbage st.history_size = " & st.history_size
    st.history_size = 0
    EXIT WHILE
   END IF
   st.history_size -= v_len(st.history[trim_amount])
   trim_amount += 1
  WEND
  IF trim_amount THEN
   v_delete_slice st.history, 0, trim_amount
   st.history_step -= trim_amount
   'debug "add_undo_step: reduced history size to " & st.history_size & " by discarding " & trim_amount
   'debug "...now history_step=" & st.history_step & " out of " & v_len(st.history) 
  END IF

  v_expand st.history

  'Write meta data
  add_change_step st.history[st.history_step - 1], -1, -1, st.cur_zone, mapIDMetaEditmode + st.editmode
  add_change_step st.history[st.history_step - 1], st.x, st.y, 0, mapIDMetaCursor
 END IF

 add_change_step st.history[st.history_step - 1], x, y, oldvalue, mapid

 st.history_size += 1
END SUB

'A stroke is a group of tile brush applications
'Returns the stroke which was redone
FUNCTION redo_stroke(st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap) as MapEditUndoTile vector
 st.message_ticks = 20
 IF st.history_step = v_len(st.history) THEN
  st.message = "No more Redo history"
  RETURN NULL
 END IF

 RETURN undo_stroke(st, map(), pass, emap, zmap, YES)
END FUNCTION

'A stroke is a group of brush applications/steps
'Returns the stroke which was undone
FUNCTION undo_stroke(st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, byval redo as bool = NO) as MapEditUndoTile vector
 st.message_ticks = 20
 IF redo = NO THEN
  IF st.history_step = 0 THEN
   st.message = "No more Undo history"
   RETURN NULL
  END IF
  st.history_step -= 1
 END IF

 'debug "undo_stroke(" & redo & ")  history_step=" & st.history_step & " out of " & v_len(st.history)

 DIM undostroke as MapEditUndoTile vector = st.history[st.history_step]
 IF v_len(undostroke) = 0 THEN showerror "Strange... empty undo step. Probably harmless"
 undo_stroke_internal st, map(), pass, emap, zmap, undostroke, redo

 IF redo THEN st.history_step += 1
 st.message = CHR(27) + " " & st.history_step & " Undo steps | " & (v_len(st.history) - st.history_step) & " Redo steps " + CHR(26)

 RETURN undostroke
END FUNCTION

'Undoes the changes in st.secondary_undo_buffer
SUB undo_preview(st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap)
 IF st.secondary_undo_buffer THEN
  undo_stroke_internal st, map(), pass, emap, zmap, st.secondary_undo_buffer, NO
  v_free st.secondary_undo_buffer
 END IF
END SUB

'Either redo or undo an undo history changelist
SUB undo_stroke_internal(st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, byref changelist as MapEditUndoTile vector, byval redo as bool = NO)
 DIM undotile as MapEditUndoTile ptr
 'When undoing, start from the end, when redoing, start from the beginning.
 'Order is only important so that the number of zones per tile does not go over 15 at any point;
 'this stuff isn't affected by default passability.
 IF redo THEN
  undotile = @changelist[0]
 ELSE
  undotile = @changelist[v_len(changelist) - 1]
 END IF
 FOR i as integer = 0 TO v_len(changelist) - 1
  WITH *undotile
   DIM overwrite_value as integer
   IF .mapid >= mapIDLayer THEN
    overwrite_value = readblock(map(.mapid - mapIDLayer), .x, .y)
    writeblock map(.mapid - mapIDLayer), .x, .y, .value
   ELSEIF .mapid = mapIDPass THEN
    overwrite_value = readblock(pass, .x, .y)
    writeblock pass, .x, .y, .value
   ELSEIF .mapid = mapIDFoe THEN
    overwrite_value = readblock(emap, .x, .y)
    writeblock emap, .x, .y, .value
   ELSEIF .mapid >= mapIDZone THEN
    overwrite_value = CheckZoneAtTile(zmap, .mapid - mapIDZone, .x, .y)
    IF .value THEN
     IF SetZoneTile(zmap, .mapid - mapIDZone, .x, .y) = NO THEN
      showerror "SetZoneTile failed during undo: impossible!"
     END IF
    ELSE
     UnsetZoneTile zmap, .mapid - mapIDZone, .x, .y
    END IF
    st.zones_needupdate = YES
   ELSEIF .mapid >= mapIDMetaBEGIN THEN
    'Ignore meta data
    overwrite_value = .value
   ELSE
    showerror "Undo history is corrupt: unknown map id " & .mapid
   END IF

   'debug "   pos=" & .x & "," & .y & " mapid=" & .mapid & "  " & overwrite_value & " -> " & .value

   'Reverse history!
   .value = overwrite_value
  END WITH
  IF redo THEN undotile += 1 ELSE undotile -= 1
 NEXT
END SUB


'==========================================================================================
'                                     Mark + clone tool
'==========================================================================================


FUNCTION create_changelist(st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, visible() as integer, gmap() as integer, rect as RectType) as MapEditUndoTile vector
 DIM changelist as MapEditUndoTile vector
 v_new changelist
 DIM zones() as integer

 FOR xoff as integer = 0 TO rect.wide - 1
  DIM x as integer = xoff + rect.x
  IF NOT in_bound(x, 0, st.wide - 1) THEN CONTINUE FOR
  FOR yoff as integer = 0 TO rect.high - 1
   DIM y as integer = yoff + rect.y
   IF NOT in_bound(y, 0, st.high - 1) THEN CONTINUE FOR

   FOR layer as integer = 0 TO UBOUND(map)
    IF LayerIsEnabled(gmap(), layer) AND LayerIsVisible(visible(), layer) THEN
     IF readblock(map(layer), x, y) THEN
      add_change_step changelist, xoff, yoff, readblock(map(layer), x, y), mapIDLayer + layer
     END IF
    END IF
   NEXT
   IF readblock(pass, x, y) THEN
    add_change_step changelist, xoff, yoff, readblock(pass, x, y), mapIDPass
   END IF
   IF readblock(emap, x, y) THEN
    add_change_step changelist, xoff, yoff, readblock(emap, x, y), mapIDFoe
   END IF

   GetZonesAtTile zmap, zones(), x, y
   FOR ctr as integer = 0 TO UBOUND(zones)
    add_change_step changelist, xoff, yoff, 1, mapIDZone + zones(ctr)
   NEXT
  NEXT
 NEXT

 RETURN changelist
END FUNCTION

'Stamp a 'marked' brush onto the map, which is stored as a list of MapEditUndoTile edits
'offset is the difference between the cursor position (st.x/y) now and when the brush
'was saved (ie. the x/y coords stored in the changelist items).
SUB apply_changelist(st as MapEditState, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap, visible() as integer, gmap() as integer, byref changelist as MapEditUndoTile vector, offset as XYPair)
 'IF v_len(changelist) = 0 THEN showerror "Strange... empty undo step. Probably harmless"

 'debug "apply_changelist len " & v_len(changelist)

 IF st.clone_merge = NO THEN
  'First remove all existing tile data

  DIM as RectType clone_box
  clone_box = TYPE(st.x - st.clone_offset.x, st.y - st.clone_offset.y, st.clone_size.w, st.clone_size.h)

  FOR xoff as integer = 0 TO clone_box.wide - 1
   DIM x as integer = xoff + clone_box.x
   IF NOT in_bound(x, 0, st.wide - 1) THEN CONTINUE FOR
   FOR yoff as integer = 0 TO clone_box.high - 1
    DIM y as integer = yoff + clone_box.y
    IF NOT in_bound(y, 0, st.high - 1) THEN CONTINUE FOR

    'This is quite different to BACKSPACE tile clearing:
    'Clear visible tiles, walls, foemap, zones,
    'but not doors or NPCs
    FOR layer as integer = 0 TO UBOUND(map)
     IF LayerIsEnabled(gmap(), layer) AND LayerIsVisible(visible(), layer) THEN
      tilebrush st, x, y, 0, layer, map(), pass, emap, zmap
     END IF
    NEXT
    wallbrush st, x, y, 0, , map(), pass, emap, zmap
    foebrush st, x, y, 0, , map(), pass, emap, zmap
    DIM zones() as integer
    GetZonesAtTile zmap, zones(), x, y
    FOR ctr as integer = 0 TO UBOUND(zones)
     zonebrush st, x, y, 0, zones(ctr), map(), pass, emap, zmap
    NEXT
   NEXT
  NEXT
 END IF

 FOR i as integer = 0 TO v_len(changelist) - 1
  WITH changelist[i]
   DIM as integer x = .x + offset.x, y = .y + offset.y
   IF NOT in_bound(x, 0, st.wide - 1) THEN CONTINUE FOR
   IF NOT in_bound(y, 0, st.high - 1) THEN CONTINUE FOR

   IF .mapid >= mapIDLayer THEN
    DIM layer as integer = .mapid - mapIDLayer
    IF LayerIsEnabled(gmap(), layer) AND LayerIsVisible(visible(), layer) THEN
     tilebrush st, x, y, .value, layer, map(), pass, emap, zmap
    END IF
   ELSEIF .mapid = mapIDPass THEN
    IF st.defpass = NO THEN
     'Merge walls with existing ones
     DIM wallmask as integer = readblock(pass, x, y) OR .value
     wallbrush st, x, y, wallmask, , map(), pass, emap, zmap
    END IF
   ELSEIF .mapid = mapIDFoe THEN
    foebrush st, x, y, .value, , map(), pass, emap, zmap
   ELSEIF .mapid >= mapIDZone THEN
    'Effectively merges zones with existing ones
    zonebrush st, x, y, .value, (.mapid - mapIDZone), map(), pass, emap, zmap
   ELSEIF .mapid >= mapIDMetaBEGIN THEN
    'Ignore meta data
   ELSE
    showerror "Mark+clone stamp is corrupt: unknown map id " & .mapid
   END IF

   'debug "   pos=" & x & "," & y & " mapid=" & .mapid & "  value=" & .value
  END WITH
 NEXT
END SUB


'==========================================================================================


'Can a tile be seen? (Specifically, the centre of the tile)
FUNCTION mapedit_on_screen(st as MapEditState, byval x as integer, byval y as integer) as integer
 'Visible portion of the map
 DIM mapview as RectType
 mapview.x = st.mapx
 mapview.y = st.mapy
 mapview.wide = vpages(dpage)->w
 mapview.high = vpages(dpage)->h - 20  '20 pixels for menubar
 RETURN rect_collide_point(mapview, x * 20 + 10, y * 20 + 10)
END FUNCTION

'Center the camera on a tile
SUB mapedit_focus_camera(st as MapEditState, byval x as integer, byval y as integer)
 'Size of the map viewport
 DIM mapviewsize as XYPair
 mapviewsize.w = vpages(dpage)->w
 mapviewsize.h = vpages(dpage)->h - 20  '20 pixels for menubar
 st.mapx = bound(x * 20 - mapviewsize.w \ 2, 0, st.wide * 20 - mapviewsize.w)
 st.mapy = bound(y * 20 - mapviewsize.h \ 2, 0, st.high * 20 - mapviewsize.h)
END SUB
