'OHRRPGCE CUSTOM - Map editor and map picker
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
#include "config.bi"
#include "const.bi"
#include "udts.bi"
#include "custom.bi"
#include "allmodex.bi"
#include "common.bi"
#include "customsubs.bi"
#include "cglobals.bi"
#include "scrconst.bi"
#include "loading.bi"
#include "common_menus.bi"
#include "thingbrowser.bi"

CONST tilew = 20
CONST tileh = 20
DIM SHARED tilesize as XYPair = (20, 20)

'---------------------------- Local subs/functions & types -------------------------------

DECLARE SUB make_map_picker_menu (topmenu() as string, state as MenuState)
DECLARE SUB mapeditor (byval mapnum as integer)
DECLARE SUB mapeditor_mapping(st as MapEditState, mode_tools_map() as integer)

DECLARE SUB loadpasdefaults (byref defaults as integer vector, tilesetnum as integer)

DECLARE SUB fill_map_area(st as MapEditState, byval x as integer, byval y as integer, reader as FnReader)
DECLARE SUB fill_with_other_area(st as MapEditState, byval x as integer, byval y as integer, reader as FnReader)

DECLARE FUNCTION mapedit_layer_offset(st as MapEditState, i as integer) as XYPair
DECLARE SUB mapedit_draw_layer(st as MapEditState, layernum as integer, height as integer, overhead as bool = NO, pal as Palette16 ptr = NULL)

ENUM MouseAttention
 focusNowhere
 focusMap
 focusViewport  'Main part of the screen, but off the map edge
 focusToolbar   'The tool buttons
 focusTopbar
END ENUM

DECLARE FUNCTION mapedit_npc_at_spot(st as MapEditState, pos as XYPair) as integer
DECLARE FUNCTION mapedit_on_screen(st as MapEditState, byval x as integer, byval y as integer) as integer
DECLARE SUB mapedit_focus_camera(st as MapEditState, byval x as integer, byval y as integer)
DECLARE SUB mapedit_constrain_camera(st as MapEditState)
DECLARE FUNCTION map_to_screen OVERLOAD(st as MapEditState, map_pos as XYPair) as XYPair
DECLARE FUNCTION map_to_screen OVERLOAD(st as MapEditState, map_pos as RectType) as RectType
DECLARE FUNCTION screen_to_map(st as MapEditState, pos as XYPair) as XYPair
DECLARE FUNCTION mapedit_mouse_over_what(st as MapEditState) as MouseAttention

DECLARE SUB mapedit_draw_cursor(st as MapEditState)
DECLARE FUNCTION mapedit_tool_rect(st as MapEditState) as RectType
DECLARE FUNCTION tool_cube_offset(st as MapEditState) as XYPair
DECLARE FUNCTION toolbar_rect(st as MapEditState) as RectType

DECLARE FUNCTION layer_shadow_palette() as Palette16 ptr
DECLARE SUB mapedit_free_layer_palettes(st as MapEditState)
DECLARE SUB mapedit_update_layer_palettes(st as MapEditState)

DECLARE SUB mapedit_edit_npcdef (st as MapEditState, npcdata as NPCType)
DECLARE SUB npcdef_editor (st as MapEditState)
DECLARE FUNCTION mapedit_npc_instance_count(st as MapEditState, byval id as integer) as integer
DECLARE SUB npcdefedit_preview_npc(npcdata as NPCType, npc_img as GraphicPair, boxpreview as string, framenum as integer = 4)

'Undo
DECLARE SUB add_change_step(byref changelist as MapEditUndoTile vector, byval x as integer, byval y as integer, byval value as integer, byval mapid as integer)
DECLARE SUB add_undo_step(st as MapEditState, byval x as integer, byval y as integer, byval oldvalue as integer, byval mapid as integer)
DECLARE FUNCTION undo_stroke(st as MapEditState, byval redo as bool = NO) as MapEditUndoTile vector
DECLARE FUNCTION redo_stroke(st as MapEditState) as MapEditUndoTile vector
DECLARE SUB undo_stroke_internal(st as MapEditState, byref changelist as MapEditUndoTile vector, byval redo as bool = NO)
DECLARE SUB undo_preview(st as MapEditState)
DECLARE SUB mapedit_throw_away_history(st as MapEditState)
DECLARE SUB mapedit_show_undo_change(st as MapEditState, byval undostroke as MapEditUndoTile vector)

'Mark+Clone
DECLARE FUNCTION create_changelist(st as MapEditState, rect as RectType) as MapEditUndoTile vector
DECLARE SUB apply_changelist(st as MapEditState, byref changelist as MapEditUndoTile vector, offset as XYPair)

'Zones
DECLARE SUB draw_zone_tileset(byval zonetileset as Frame ptr)
DECLARE SUB draw_zone_tileset2(byval zonetileset as Frame ptr)
DECLARE SUB draw_zone_tileset3(byval zonetileset as Frame ptr)
DECLARE SUB mapedit_doZoneHinting(st as MapEditState)
DECLARE SUB zonemenu_add_zone (byref zonemenu as SimpleMenuItem vector, zonecolours() as integer, byval info as ZoneInfo ptr)
DECLARE FUNCTION mapedit_try_assign_colour_to_zone(byval id as integer, zonecolours() as integer, viszonelist() as integer) as integer
DECLARE SUB mapedit_update_visible_zones (st as MapEditState)
DECLARE SUB mapedit_edit_zoneinfo(st as MapEditState)
DECLARE SUB mapedit_zonespam(st as MapEditState)
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
DECLARE FUNCTION should_draw_layer(st as MapEditState, l as integer) as bool
DECLARE SUB set_layer(st as MapEditState, layer as integer)

DECLARE SUB DrawDoorPair(st as MapEditState, linknum as integer, page as integer)

DECLARE SUB calculatepassblock(st as MapEditState, x as integer, y as integer)

DECLARE SUB resizemapmenu (st as MapEditState, byref rs as MapResizeState)
DECLARE SUB resizetiledata OVERLOAD (tmap as TileMap, rs as MapResizeState, byref yout as integer, page as integer)
DECLARE SUB resizetiledata OVERLOAD (tmaps() as TileMap, rs as MapResizeState, byref yout as integer, page as integer)
DECLARE SUB resizetiledata OVERLOAD (tmap as TileMap, x_off as integer, y_off as integer, new_width as integer, new_height as integer, byref yout as integer, page as integer)

DECLARE SUB set_usetile(st as MapEditState, tile as integer)
DECLARE SUB update_tilepicker(st as MapEditState)
DECLARE SUB verify_map_size (st as MapEditState)
DECLARE SUB fix_tilemaps(map as MapData)
DECLARE SUB mapedit_loadmap (st as MapEditState, mapnum as integer)
DECLARE SUB mapedit_load_tilesets (st as MapEditState)
DECLARE SUB mapedit_savemap (st as MapEditState)
DECLARE SUB new_blank_map (st as MapEditState)
DECLARE SUB mapedit_addmap()
DECLARE SUB mapedit_resize(st as MapEditState)
DECLARE SUB mapedit_delete(st as MapEditState)
DECLARE SUB link_one_door(st as MapEditState, linknum as integer)
DECLARE SUB mapedit_linkdoors (st as MapEditState)
DECLARE FUNCTION mapedit_pick_layer(st as MapEditState, message as string, other_option as string = "") as integer
DECLARE SUB mapedit_layers (st as MapEditState)
DECLARE SUB mapedit_makelayermenu(st as MapEditState, byref menu as LayerMenuItem vector, state as MenuState, byval resetpt as bool, byval selectedlayer as integer = 0, byref layerpreview as Frame ptr)

DECLARE SUB mapedit_copy_layer(st as MapEditState, byval src as integer, byval dest as integer)
DECLARE SUB mapedit_append_new_layers(st as MapEditState, howmany as integer)
DECLARE SUB mapedit_insert_new_layer(st as MapEditState, byval where as integer)
DECLARE SUB mapedit_delete_layer(st as MapEditState, byval which as integer)
DECLARE SUB mapedit_swap_layers(st as MapEditState, byval l1 as integer, byval l2 as integer)
DECLARE SUB mapedit_gmapdata(st as MapEditState)
DECLARE SUB mapedit_draw_icon(st as MapEditState, icon as string, x as RelPos, y as RelPos, highlight as bool = NO)
DECLARE SUB mapedit_list_npcs_by_tile (st as MapEditState, pos as XYPair)

DECLARE SUB mapedit_import_export(st as MapEditState)

DECLARE FUNCTION door_exists (doors() as Door, id as integer) as bool
DECLARE SUB set_door_exists (doors() as Door, id as integer, value as bool)
DECLARE FUNCTION find_last_used_doorlink(link() as DoorLink) as integer
DECLARE FUNCTION find_door_at_spot (x as integer, y as integer, doors() as Door) as integer
DECLARE FUNCTION find_first_free_door (doors() as Door) as integer
DECLARE FUNCTION find_first_doorlink_by_door(doornum as integer, link() as DoorLink) as integer

DECLARE SUB resize_rezoom_mini_map(st as MapEditState, byref rs as MapResizeState)
DECLARE SUB show_minimap(byref map as MapEditState)
DECLARE SUB mapedit_pickblock(st as MapEditState)
DECLARE SUB resize_buildmenu(byref rs as MapResizeState)
DECLARE SUB resize_dimchange(st as MapEditState, byref rs as MapResizeState)
DECLARE SUB resize_correct_width(st as MapEditState, byref rs as MapResizeState)
DECLARE SUB resize_correct_height(st as MapEditState, byref rs as MapResizeState)


DEFINE_VECTOR_OF_TYPE(MapEditUndoTile, MapEditUndoTile)
DEFINE_VECTOR_VECTOR_OF(MapEditUndoTile, MapEditUndoTile)


'==========================================================================================
'                                    Map listing menu
'==========================================================================================

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
  draw_fullscreen_scrollbar state, 0, vpage

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
SUB tilebrush (st as MapEditState, byval x as integer, byval y as integer, byval tile as integer = -1, byval layer as integer = -1)
 IF tile = -1 THEN tile = st.tool_value
 IF layer = -1 THEN layer = st.layer
 DIM oldval as integer = readblock(st.map.tiles(layer), x, y)
 IF oldval <> tile THEN
  add_undo_step st, x, y, oldval, mapIDLayer + layer
  writeblock st.map.tiles(layer), x, y, tile
 END IF
 IF st.defpass THEN calculatepassblock st, x, y
END SUB

'Note dummy arguments: all brush functions should have the same signature
SUB wallbrush (st as MapEditState, byval x as integer, byval y as integer, byval tile as integer = -1, byval extraarg as integer = -1)
 IF tile = -1 THEN tile = st.tool_value
 DIM oldval as integer = readblock(st.map.pass, x, y)
 IF oldval <> tile THEN
  add_undo_step st, x, y, oldval, mapIDPass
  writeblock st.map.pass, x, y, tile
 END IF
END SUB

'Like wallbrush, but only sets the bits specified in st.wallmap_mask
'Note dummy arguments: all brush functions should have the same signature
SUB wallbitbrush (st as MapEditState, byval x as integer, byval y as integer, byval walls as integer = -1, byval extraarg as integer = -1)
 IF walls = -1 THEN walls = st.tool_value
 DIM oldwalls as integer = readblock(st.map.pass, x, y)
 walls = (oldwalls AND NOT st.wallmap_mask) OR walls
 IF oldwalls <> walls THEN
  add_undo_step st, x, y, oldwalls, mapIDPass
  writeblock st.map.pass, x, y, walls
 END IF
END SUB

'Note dummy arguments: all brush functions should have the same signature
SUB foebrush (st as MapEditState, byval x as integer, byval y as integer, byval foe as integer = -1, byval extraarg as integer = -1)
 IF foe = -1 THEN foe = st.tool_value
 DIM oldval as integer = readblock(st.map.foemap, x, y)
 IF oldval <> foe THEN
  add_undo_step st, x, y, oldval, mapIDFoe
  writeblock st.map.foemap, x, y, foe
 END IF
END SUB

'Note dummy arguments: all brush functions should have the same signature
'(Small exception: different value is use for 'default' for value, so value can be treated as a bool)
SUB zonebrush (st as MapEditState, byval x as integer, byval y as integer, byval value as integer = -9876, byval zone as integer = -1)
 IF value = -9876 THEN value = st.tool_value
 IF zone = -1 THEN zone = st.cur_zone
 DIM new_stroke as bool = st.new_stroke  'Modified by add_undo_step
 DIM oldval as integer = CheckZoneAtTile(st.map.zmap, zone, x, y)
 IF (oldval = 0) <> (value = 0) THEN
  add_undo_step st, x, y, oldval, mapIDZone + zone
  IF WriteZoneTile(st.map.zmap, zone, x, y, value) = NO THEN
   IF new_stroke THEN
    pop_warning "You have already placed this tile in 15 other zones, and that is the maximum supported. Sorry!"
   END IF
  END IF
  st.zones_needupdate = YES
 END IF
END SUB

'Note dummy arguments: all brush functions should have the same signature
'Values allowed: 0 to 255
SUB tempbrush (st as MapEditState, byval x as integer, byval y as integer, byval tile as integer = -1, byval extraarg as integer = -1)
 writeblock st.temptilemap, x, y, tile
END SUB


'==========================================================================================
'                                         Readers
'==========================================================================================


'Note dummy arguments: all reader functions should have the same signature
FUNCTION tilereader (st as MapEditState, byval x as integer, byval y as integer, byval layer as integer = -1) as integer
 IF layer = -1 THEN layer = st.layer
 RETURN readblock(st.map.tiles(layer), x, y)
END FUNCTION

'Note dummy arguments: all reader functions should have the same signature
FUNCTION foereader (st as MapEditState, byval x as integer, byval y as integer, byval extraarg as integer = -1) as integer
 RETURN readblock(st.map.foemap, x, y)
END FUNCTION

'Note dummy arguments: all reader functions should have the same signature
FUNCTION zonereader (st as MapEditState, byval x as integer, byval y as integer, byval zone as integer = -1) as integer
 IF zone = -1 THEN zone = st.cur_zone
 RETURN CheckZoneAtTile(st.map.zmap, zone, x, y)
END FUNCTION

'Note dummy arguments: all reader functions should have the same signature
FUNCTION tempreader (st as MapEditState, byval x as integer, byval y as integer, byval extraarg as integer = -1) as integer
 RETURN readblock(st.temptilemap, x, y)
END FUNCTION


'==========================================================================================
'                               Main SUB (toplevel menu)
'==========================================================================================

FUNCTION mapedit_npc_instance_count(st as MapEditState, byval id as integer) as integer
 DIM num as integer = 0
 FOR i as integer = 0 to UBOUND(st.map.npc)
  IF ABS(st.map.npc(i).id) - 1 = id THEN num += 1
 NEXT i
 RETURN num
END FUNCTION

SUB mapeditor (byval mapnum as integer)
STATIC remember_menu_pt as integer = 0

DIM st as MapEditState
DIM pal16(288) as integer
DIM npcnum() as integer
DIM her as HeroDef

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

v_new st.mode_tools

REDIM st.lockedzonelist(-1 TO -1) as integer 'The zones chosen to be always displayed. At most 8 (index 0 onwards, start at -1 for fake zero-length arrays) 
'The floating menu that displays a list of zones. These are created and updated in mapedit_update_visible_zones
DIM zone_delete_tool as integer  'Whether Space should add or remove tiles

flusharray st.visible(), , -1  'Mark all layers visible (when they're enabled)


st.secondary_undo_buffer = NULL
v_new st.history
st.history_step = 0
st.new_stroke = YES

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

DIM datafile as string = finddatafile("arrows2.bmp")  'Actually a walkabout set
IF LEN(datafile) THEN
 DIM arrowset as Frame ptr = frame_import_bmp_raw(datafile)
 FOR idx as integer = 0 TO 4
  st.arrow_icons(idx) = frame_resized(arrowset, 20, 20, idx * -20, 0)
 NEXT
 frame_unload @arrowset
END IF

mapedit_update_layer_palettes st

st.shadowpal = layer_shadow_palette()

'--These tilesets indicate up to 8 zones at once
'--create three alternative zone tilemaps, I can't decide!
st.zonetileset(0) = frame_new(20, 20 * 256, , YES)  'large tilesets
st.zonetileset(1) = frame_new(20, 20 * 256, , YES)
st.zonetileset(2) = frame_new(20, 20 * 256, , YES)
draw_zone_tileset st.zonetileset(0)
draw_zone_tileset2 st.zonetileset(1)
draw_zone_tileset3 st.zonetileset(2)
'frame_export_bmp8 "zt3.bmp", st.zonetileset(2), master()

st.overlaytileset = frame_new(20, 20 * 160, , YES)
fuzzyrect st.overlaytileset, 0, 1*20, 20, 20, uilook(uiHighlight)  'Zone edit mode, and NPC movement zone
fuzzyrect st.overlaytileset, 0, 2*20, 20, 20, uilook(uiHighlight2) 'NPC avoidance zone
fuzzyrect st.overlaytileset, 0, 3*20, 20, 20, uilook(uiHighlight2) 'NPC avoidance zone (overriding movement zone)
rectangle st.overlaytileset, 0, 6*20, 20, 20, uilook(uiDisabledItem)  '???

'Tiles 10 - 15 are for the 'hidden zone' animation. I think it's easier on the eyes than 2 frame flickering.
'Leave tiles 10-12 blank
FOR i as integer = 1 TO 3
 'fuzzyrect st.overlaytileset, 0, (12 + i)*20, 20, 20, uilook(uiDisabledItem), 5 * i
 fuzzyrect st.overlaytileset, 0, (12 + i)*20, 20, 20, boxlook(15-i).edgecol, 5 * i
NEXT

'Plenty of tiles left for other purposes

'Note that most of this array is empty
WITH st.toolinfo(draw_tool)
 .name = "Draw"
 .icon = "D"  'CHR(3)
 .shortcut = scD
END WITH
WITH st.toolinfo(box_tool)
 .name = "Box"
 .icon = "B"  'CHR(4)
 .shortcut = scB
END WITH
WITH st.toolinfo(fill_tool)
 .name = "Fill"
 .icon = "F"
 .shortcut = scF
END WITH
WITH st.toolinfo(replace_tool)
 .name = "Replace"
 .icon = "R"
 .shortcut = scR
END WITH
WITH st.toolinfo(paint_tool)
 .name = "Paint Tilemap"
 .icon = "P"
 .shortcut = scP
END WITH
WITH st.toolinfo(mark_tool)
 .name = "Mark (Copy)"
 .icon = "M"
 .shortcut = scM
END WITH
WITH st.toolinfo(clone_tool)
 .name = "Clone (Paste)"
 .icon = "C"
 .shortcut = scC
END WITH

'--load hero graphics--
loadherodata her, 0
load_sprite_and_pal st.hero_gfx, sprTypeWalkabout, her.walk_sprite, her.walk_sprite_pal


st.modenames(0) = "Picture Mode"
st.modenames(1) = "Passability Mode"
st.modenames(2) = "Door Placement Mode"
st.modenames(3) = "NPC Placement Mode"
st.modenames(4) = "Foe Mapping Mode"
st.modenames(5) = "Zone Mapping Mode"

'Due to laziness, the menubar shows all 256 tiles (including animated tiles),
'while the tileset tile picker hides unused animated tile patterns.
'See mapedit_pickblock_setup_tileset.
cleantilemap st.menubar, 256, 1
FOR i as integer = 0 TO 255
 writeblock st.menubar, i, 0, i
NEXT
st.zoneminimap = NULL

mapedit_loadmap st, mapnum

load_npc_graphics st.map.npc_def(), st.npc_img()

st.x = 0
st.y = 0
st.mapx = 0
st.mapy = 0
st.layer = 0
st.cur_zone = 1
st.cur_zinfo = GetZoneInfo(st.map.zmap, st.cur_zone)
st.clone_merge = YES
st.wallthickness = 2

st.cur_door = find_first_free_door(st.map.door())

DIM mapeditmenu(15) as string
DIM mapeditmenu_display(15) as string

mapeditmenu(0) = "Return to Map Menu"
mapeditmenu(1) = "Edit General Map Data..."
mapeditmenu(2) = "Resize Map..."
mapeditmenu(3) = "Layers and Tilesets..."
mapeditmenu(4) = "Edit Tilemap..."
mapeditmenu(5) = "Edit Wallmap..."
mapeditmenu(6) = "Place Doors..."
mapeditmenu(7) = "Link Doors..."
mapeditmenu(8) = "Edit NPCs..."
mapeditmenu(9) = "Place NPCs..."
mapeditmenu(10) = "Edit Foemap..."
mapeditmenu(11) = "Edit Zones..."
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
  mapedit_savemap st
  EXIT DO
 END IF
 IF keyval(scF1) > 1 THEN show_help "mapedit_menu"
 usemenu st.menustate
 IF st.menustate.pt = 15 AND selectst.query = "" THEN
  strgrabber st.map.name, 39
  st.menustate.need_update = YES
 ELSEIF select_by_typing(selectst) THEN
  select_on_word_boundary mapeditmenu(), selectst, st.menustate
 END IF

 IF enter_space_click(st.menustate) THEN
  SELECT CASE st.menustate.pt
   CASE 0
    mapedit_savemap st
    EXIT DO
   CASE 1
    mapedit_gmapdata st
   CASE 2
    mapedit_resize st
   CASE 3
    mapedit_layers st
   CASE 4 TO 6  'Tilemap, Wallmap, Doormap
    st.seteditmode = st.menustate.pt - 4
    mapeditor_mapping st, mode_tools_map()
   CASE 7
    mapedit_savemap st
    mapedit_linkdoors st
   CASE 8
    'This may delete NPC instances, and write npc definitions to disk
    npcdef_editor st
   CASE 9 TO 11  'Place NPCs, Foemap, Zonemap
    st.seteditmode = st.menustate.pt - 6
    mapeditor_mapping st, mode_tools_map()
   CASE 12
    mapedit_delete st
    IF st.map.id > gen(genMaxMap) THEN
     'This was the last map, and it was deleted instead of blanked
     EXIT DO
    END IF
   CASE 13
    mapedit_import_export st
   CASE 14
    '--reload default passability
    IF yesno("Set default passability for whole map, overwriting your wallmap? Don't worry, you can undo this by hitting Ctrl+Z in any editing mode", NO, NO) THEN
     FOR tx as integer = 0 TO st.map.wide - 1
      FOR ty as integer = 0 TO st.map.high - 1
       calculatepassblock st, tx, ty
      NEXT ty
     NEXT tx
    END IF
  END SELECT
  IF slave_channel <> NULL_CHANNEL THEN     'If live previewing, give quick feedback
   mapedit_savemap st
  END IF
 END IF

 IF st.menustate.need_update THEN
  mapeditmenu(15) = "Map name:" + st.map.name
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
FOR i as integer = 0 TO UBOUND(st.npc_img)
 WITH st.npc_img(i)
  IF .sprite THEN frame_unload(@.sprite)
  IF .pal THEN palette16_unload(@.pal)
 END WITH
NEXT i

unloadmaptilesets st.tilesets()
unloadtilemap st.menubar
v_free st.history
IF st.secondary_undo_buffer THEN debugc errPromptBug, "mapedit cleanup: secondary_undo_buffer exists!"
v_free st.cloned
unloadtilemap st.zoneviewmap
unloadtilemap st.zoneoverlaymap
v_free st.defaultwalls
unload_sprite_and_pal st.cursor
unload_sprite_and_pal st.hero_gfx
frame_unload @st.zonetileset(0)
frame_unload @st.zonetileset(1)
frame_unload @st.zonetileset(2)
frame_unload @st.overlaytileset
frame_unload @st.zoneminimap
FOR idx as integer = 0 TO 4
 frame_unload @st.arrow_icons(idx)
NEXT
mapedit_free_layer_palettes st
palette16_unload @st.shadowpal
v_free st.mode_tools
v_free st.zonemenu
' Contents of st.map are freed by destructor

remember_menu_pt = st.menustate.pt  'preserve for other maps
END SUB


'==========================================================================================
'                                   Map editor Proper
'==========================================================================================


SUB mapeditor_mapping(st as MapEditState, mode_tools_map() as integer)
clearpage 2

st.reset_tool = YES
st.defpass = YES
IF readbit(gen(), genBits, 15) THEN st.defpass = NO ' option to default the defaults to OFF
st.autoshow_zones = YES
st.showzonehints = YES
st.zonemenustate.pt = -1  'Properly initialised in mapedit_update_visible_zones
st.zones_needupdate = YES
st.npczone_needupdate = YES
DIM tog as integer
DIM slowtog as integer
DIM chequer_scroll as integer
DIM byref mouse as MouseInfo = readmouse
DIM mouse_attention as MouseAttention = focusNowhere  'What currently recieves mouse input

setkeys
DO
 setwait 55, 300
 setkeys
 tog = tog XOR 1
 chequer_scroll += 1
 st.gauze_ticker = (st.gauze_ticker + 1) MOD 50  '10 frames, 5 ticks a frame
 st.message_ticks = large(0, st.message_ticks - 1) 

 IF keyval(scESC) > 1 THEN EXIT DO
 IF keyval(scCtrl) = 0 AND keyval(scAlt) = 0 THEN
  FOR i as integer = tile_mode TO zone_mode
   IF keyval(scF2 + i) > 1 THEN st.seteditmode = i
  NEXT
 END IF

 DIM oldpos as XYPair = st.pos     'used when detecting cursor movement

 'A mouse click on the map (uses readmouse.release rather than .click, as you
 'should normally should) or Space keypress, which can activate a tool.
 '(The mouse component is added below)
 DIM tool_actkeypress as bool = (keyval(scSpace) AND 4) > 0
 'As above, but using .click instead of .release, for stuff that affects dragging.
 DIM tool_newkeypress as bool = (keyval(scSpace) AND 4) > 0
 'Space or left mouse button is down (and the map has focus)
 DIM tool_buttonpressed as bool = keyval(scSpace) > 0

 '--Check how to handle mouse input
 DIM mouse_over_tool as ToolIDs = no_tool
 DIM mouse_over as MouseAttention
 mouse_over = mapedit_mouse_over_what(st)
 IF mouse.buttons = 0 THEN mouse_attention = mouse_over
 DIM mouse_over_tile as XYPair
 IF mouse_over = focusMap THEN mouse_over_tile = screen_to_map(st, mouse.pos) \ tilesize

 'If you drag off an UI element, ignore mouse input
 IF mouse_attention = mouse_over THEN

  IF mouse_attention = focusMap THEN
   'Also, ignore mouse while skewing the map (started by holding down Ctrl)
   IF (st.mouse_skewing OR keyval(scCtrl)) = NO THEN
    'In-bounds

    'Move cursor to mouse position?
    'NPC edit mode is a special case, because placing the cursor over an NPC you
    'place is very annoying, so it handles this itself.
    IF st.editmode <> npc_mode THEN
     IF st.tool_hold OR (mouse.clicks AND mouseRight) OR (mouse.buttons AND mouseLeft) THEN
      st.pos = mouse_over_tile
     END IF
    END IF

    'Left mouse button can activate tools
    IF mouse.buttons AND mouseLeft THEN
     tool_buttonpressed = YES
    END IF
    IF mouse.clicks AND mouseLeft THEN
     tool_newkeypress = YES
    END IF
    IF mouse.release AND mouseLeft THEN
     tool_actkeypress = YES
    END IF
   END IF

  ELSEIF mouse_attention = focusToolbar THEN
   mouse_over_tool = st.mode_tools[(mouse.x - toolbar_rect(st).x) \ 10]

   IF mouse.release AND mouseLeft THEN  'Select a tool
    st.tool = mouse_over_tool
    st.reset_tool = YES
    st.tool_hold = NO
   END IF

  END IF
  'focusTopbar input is edit mode-specific, handled below
 END IF

 ' This can be modified to override/augment the normal key for activating the current tool
 ' if it is a 'drawing' tool (Currently used just for wallmap bit keys)
 DIM use_draw_tool as bool = tool_newkeypress

 IF st.new_stroke = NO AND keyval(scSpace) = 0 AND mouse.buttons = 0 THEN
  'Yes, a bit of a hack, not sure what a more rigourous test would be
  st.new_stroke = YES
 END IF

 IF st.seteditmode > -1 THEN
  st.editmode = st.seteditmode
  st.seteditmode = -1

  'Set available tools
  v_resize st.mode_tools, 0
  DIM tools_i as integer = 0
  WHILE mode_tools_map(st.editmode, tools_i) <> -1
   v_append st.mode_tools, mode_tools_map(st.editmode, tools_i)
   tools_i += 1
  WEND
  st.toolsbar_available = v_len(st.mode_tools) <> 0
  st.drawing_allowed = YES  'if there are any tools, that is

  st.brush = NULL
  st.reader = NULL
  SELECT CASE st.editmode
   CASE tile_mode
    st.brush = @tilebrush
    st.reader = @tilereader
   CASE pass_mode
    st.brush = @wallbitbrush
   CASE door_mode
    st.tool = select_tool
   CASE npc_mode
    st.npczone_needupdate = YES
    st.tool = npc_tool
   CASE foe_mode
    st.brush = @foebrush
    st.reader = @foereader
    st.tool = select_tool
   CASE zone_mode
    st.brush = @zonebrush
    st.reader = @zonereader
    IF st.zonesubmode = zone_view_mode THEN
     st.toolsbar_available = NO  'No normal tool switching in view mode
     st.tool = draw_tool
     st.drawing_allowed = NO
    END IF
    st.zones_needupdate = YES
  END SELECT

  'Reset tool (if the tool isn't fixed according to the mode in the above)
  IF v_len(st.mode_tools) ANDALSO v_find(st.mode_tools, st.tool) = -1 THEN
   st.tool = st.mode_tools[0]
  END IF
  st.reset_tool = YES
  st.tool_hold = NO
 END IF

 IF keyval(scCtrl) > 0 AND keyval(scS) > 1 THEN
  'Instant save, mostly for live previewing, but maybe you're paranoid...
  mapedit_savemap st
 END IF

 IF st.editmode = tile_mode OR st.tool = paint_tool THEN
  'Paint tool is affected by the current map layer, so that's important
  IF keyval(scPageup) > 1 ORELSE (keyval(scCTRL) > 0 ANDALSO keyval(scPeriod) > 1) THEN
   IF UBOUND(st.map.tiles) = 0 THEN
    st.message = "No more layers; press Ctrl+L to add one"
    st.message_ticks = 15
   END IF
   FOR i as integer = st.layer + 1 TO UBOUND(st.map.tiles)
    IF layerisenabled(st.map.gmap(), i) THEN
     set_layer st, i
     EXIT FOR
    END IF
   NEXT i
  END IF
  IF keyval(scPageDown) > 1 ORELSE (keyval(scCTRL) > 0 ANDALSO keyval(scComma) > 1) THEN
   FOR i as integer = st.layer - 1 TO 0 STEP -1
    IF layerisenabled(st.map.gmap(), i) THEN
     set_layer st, i
     EXIT FOR
    END IF
   NEXT
  END IF
 END IF

 IF keyval(scCtrl) > 0 AND keyval(scL) > 1 THEN
  mapedit_layers st  'ctrl-L
  update_tilepicker st
  mapedit_update_layer_palettes st
 END IF

 IF keyval(scT) > 1 THEN  'Tint
  st.layer_display_mode = loopvar(st.layer_display_mode, 0, layerDisplayNUM - 1)
  mapedit_update_layer_palettes st
 END IF

 IF keyval(scTab) > 1 THEN st.tiny XOR= YES
 IF keyval(scTilde) > 1 AND keyval(scAlt) = 0 THEN show_minimap st
 IF keyval(scCtrl) > 0 AND keyval(scBackspace) > 1 THEN
   'delete tile
   FOR i as integer = 0 TO UBOUND(st.map.tiles)
    tilebrush st, st.x, st.y, 0, i
   NEXT i
   'delete passability
   wallbrush st, st.x, st.y, 0
   'delete foemap
   foebrush st, st.x, st.y, 0
   'delete NPC
   FOR i as integer = 0 TO UBOUND(st.map.npc)
    WITH st.map.npc(i)
     IF .id > 0 THEN
      IF .x = st.x * 20 AND .y = st.y * 20 THEN .id = 0
     END IF
    END WITH
   NEXT i
   'delete door
   st.doorid = find_door_at_spot(st.x, st.y, st.map.door())
   IF st.doorid >= 0 THEN
    set_door_exists(st.map.door(), st.doorid, NO)
   END IF
   'zones not deleted
 END IF
 IF keyval(scCtrl) > 0 AND keyval(scH) > 1 THEN 'Ctrl+H for hero start position
  gen(genStartMap) = st.map.id
  gen(genStartX) = st.x
  gen(genStartY) = st.y
 END IF
 IF keyval(scCtrl) > 0 AND keyval(scD) > 1 THEN st.defpass = st.defpass XOR YES

 SELECT CASE st.editmode
  '---TILEMODE------
  CASE tile_mode
   IF keyval(scCtrl) = 0 AND keyval(scF1) > 1 THEN show_help "mapedit_tilemap"

   'Selecting a tile in the tileset...
   '...via top bar
   IF keyval(scAnyEnter) > 1 THEN mapedit_pickblock st
   IF mouse_attention = focusTopBar THEN
    IF mouse.buttons AND mouseLeft THEN
     set_usetile st, st.menubarstart(st.layer) + mouse.x \ tilew
    END IF
    IF mouse.release AND mouseRight THEN mapedit_pickblock st
   END IF
   '...via selecting from the map
   'G or right click twice to select (st.pos has already been updated so check oldpos)
   IF ((mouse.clicks AND mouseRight) ANDALSO mouse_attention = focusMap _
       ANDALSO st.pos = oldpos) OR keyval(scG) > 1 THEN 'grab tile
    set_usetile st, readblock(st.map.tiles(st.layer), st.x, st.y)
   END IF
   '...via scrolling
   IF keyval(scCtrl) = 0 THEN
    IF keyval(scComma) > 1 THEN set_usetile st, st.usetile(st.layer) - 1
    IF keyval(scPeriod) > 1 THEN set_usetile st, st.usetile(st.layer) + 1
   END IF
   IF mouse.buttons = 0 THEN
    set_usetile st, st.usetile(st.layer) + mouse.wheel_clicks
   END IF

   st.tool_value = st.usetile(st.layer)

   'Ctrl+J to jiggle (although the other layer visualisation works in any
   'edit mode...)
   IF keyval(scCtrl) > 0 AND keyval(scJ) > 1 THEN
    setbit st.jiggle(), 0, st.layer, (readbit(st.jiggle(), 0, st.layer) XOR 1)
   END IF

   'Try to animate a tile
   FOR i as integer = 0 TO 1
    IF keyval(sc1 + i) > 1 THEN 'animate tile
     DIM as integer oldtile, newtile
     newtile = -1
     oldtile = readblock(st.map.tiles(st.layer), st.x, st.y)
     'Returns -1 if can't be done
     newtile = tile_anim_animate_tile(oldtile, i, st.tilesets(st.layer)->tastuf())
     IF newtile = oldtile THEN
      'It was already animated with this pattern, so instead toggle to a non-animated tile
      newtile = tile_anim_deanimate_tile(oldtile, st.tilesets(st.layer)->tastuf())
     END IF
     IF newtile >= 0 THEN
      IF keyval(scCtrl) = 0 THEN
       tilebrush st, st.x, st.y, newtile
      ELSE
       'Like Replace tool, except that can't place animated tiles
       FOR tx as integer = 0 TO st.map.wide - 1
        FOR ty as integer = 0 TO st.map.high - 1
         IF readblock(st.map.tiles(st.layer), tx, ty) = oldtile THEN tilebrush st, tx, ty, newtile
        NEXT ty
       NEXT tx
      END IF
     END IF
    END IF
   NEXT i

   '#IFNDEF __FB_UNIX__
    'common WM keys
    FOR i as integer = 0 TO UBOUND(st.map.tiles)
     IF keyval(scCtrl) > 0 AND keyval(scF1 + i) > 1 THEN
      clearkey(scF1 + i)
      IF layerisenabled(st.map.gmap(), i) THEN togglelayervisible(st.visible(), i)
     END IF
    NEXT
   '#ENDIF

   IF mouse.buttons THEN
    set_layer st, st.layer + mouse.wheel_clicks
   END IF

   'Alt+number to toggle layer 1-10 enabled, Alt+Shift+number to toggle layer 11-15
   FOR i as integer = 1 TO small(maplayerMax, 20)
    DIM shift_ok as bool = IIF(i > 10, keyval(scShift) > 0, YES)
    DIM num_key as integer = sc1 + ((i - 1) MOD 10)
    IF keyval(scAlt) > 0 AND shift_ok AND keyval(num_key) > 1 THEN
     clearkey(num_key)
     togglelayerenabled(st.map.gmap(), i)
     IF layerisenabled(st.map.gmap(), i) THEN
      IF i > UBOUND(st.map.tiles) THEN
       DIM howmany as integer
       howmany = i - UBOUND(st.map.tiles)
       IF yesno("Layer " & i & " doesn't exist yet. Create " & _
                IIF(howmany = 1, "a new map layer?", howmany & " new map layers?")) THEN
        mapedit_append_new_layers st, howmany
        mapedit_update_layer_palettes st
       END IF
      END IF
     ELSE
      IF st.layer = i THEN
       DO UNTIL layerisenabled(st.map.gmap(), st.layer)
        set_layer st, st.layer - 1
       LOOP
      END IF
     END IF
    END IF
   NEXT

   IF keyval(scAlt) > 0 AND keyval(scTilde) > 1 THEN
    togglelayervisible(st.visible(), st.layer)
   END IF

   '---PASSMODE-------
  CASE pass_mode
   IF keyval(scCtrl) = 0 AND keyval(scF1) > 1 THEN show_help "mapedit_wallmap"

   IF keyval(scCtrl) = 0 AND keyval(scW) > 1 THEN  'One-way tiles
    zonebrush st, st.x, st.y, (CheckZoneAtTile(st.map.zmap, zoneOneWayExit, st.x, st.y) XOR YES), zoneOneWayExit
   END IF

   'st.tool_value in passmap mode is a little bit different; it's
   'determined on the fly by which key (space, H, etc) you press and
   'what's under the cursor. The only tool which doesn't have
   'st.tool_value determined on the fly is Ctrl+W, as well as holding
   'space down and drawing a line. That just remembers whatever the
   'last tool_value was, which is OK.
   IF st.reset_tool THEN st.tool_value = passAllWalls  '=15, default

   ' +/- to change the thickness walls are drawn at
   IF keyval(scPlus) > 1 OR keyval(scNumpadPlus) > 1 THEN
    st.wallthickness = small(st.wallthickness + 1, 5)
   END IF
   IF keyval(scMinus) > 1 OR keyval(scNumpadMinus) > 1 THEN
    st.wallthickness = large(st.wallthickness - 1, 0)
   END IF

   DIM pass_overtile as integer = readblock(st.map.pass, st.x, st.y)
   IF tool_newkeypress THEN
    st.wallmap_mask = 255  'Remove all other bits
    IF (pass_overtile AND 15) = 0 THEN st.tool_value = 15
    IF (pass_overtile AND 15) = 15 THEN st.tool_value = 0
    IF (pass_overtile AND 15) > 0 AND (pass_overtile AND 15) < 15 THEN st.tool_value = 0
   END IF

   DIM wallbit as integer = 0
   IF keyval(scCtrl) > 0 AND keyval(scShift) = 0 THEN
    IF keyval(scUp) > 1    THEN wallbit = passNorthWall
    IF keyval(scRight) > 1 THEN wallbit = passEastWall
    IF keyval(scDown) > 1  THEN wallbit = passSouthWall
    IF keyval(scLeft) > 1  THEN wallbit = passWestWall
    IF keyval(scA) > 1 THEN wallbit = passVehA
    IF keyval(scB) > 1 THEN wallbit = passVehB
   END IF
   IF keyval(scCtrl) = 0 THEN
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
   IF keyval(scAnyEnter) > 1 OR (mouse.release AND mouseRight) THEN ' enter/right click to link a door
    st.doorid = find_door_at_spot(st.x, st.y, st.map.door())
    IF st.doorid >= 0 THEN
     'Save currently-worked-on map data
     mapedit_savemap st
     DIM doorlinkid as integer = find_first_doorlink_by_door(st.doorid, st.map.doorlink())
     IF doorlinkid >= 0 THEN
      link_one_door st, doorlinkid
     ELSE
      doorlinkid = find_last_used_doorlink(st.map.doorlink()) + 1
      IF doorlinkid >= 0 AND doorlinkid <= UBOUND(st.map.doorlink) THEN
       st.map.doorlink(doorlinkid).source = st.doorid
       link_one_door st, doorlinkid
      END IF
     END IF
    END IF
   END IF
   IF intgrabber(st.cur_door, 0, UBOUND(st.map.door), scLeftCaret, scRightCaret, , , , wheelAlways) THEN
    IF door_exists(st.map.door(), st.cur_door) THEN
     st.x = st.map.door(st.cur_door).x
     st.y = st.map.door(st.cur_door).y - 1
     mapedit_focus_camera st, st.x, st.y
    END IF
   END IF
   IF tool_actkeypress THEN ' space/click to place a door
    st.doorid = find_door_at_spot(st.x, st.y, st.map.door())
    IF st.doorid >= 0 THEN
     'clear an existing door
     set_door_exists(st.map.door(), st.doorid, NO)
    ELSE
     'Either move the current door if it exists, or create it
     st.map.door(st.cur_door).x = st.x
     st.map.door(st.cur_door).y = st.y + 1
     IF door_exists(st.map.door(), st.cur_door) = NO THEN
      'If creating a new door, automatically get an unused door ID, ready for the next placement
      set_door_exists(st.map.door(), st.cur_door, YES)
      st.cur_door = find_first_free_door(st.map.door())
     END IF
    END IF
   END IF
   IF keyval(scDelete) > 1 THEN
    st.doorid = find_door_at_spot(st.x, st.y, st.map.door())
    IF st.doorid >= 0 THEN
     set_door_exists(st.map.door(), st.doorid, NO)
    END IF
   END IF

   '---NPCMODE------
  CASE npc_mode
   IF keyval(scCtrl) = 0 AND keyval(scF1) > 1 THEN show_help "mapedit_npc_placement"
   IF keyval(scDelete) > 1 THEN
    FOR i as integer = 0 TO 299
     WITH st.map.npc(i)
      IF .id > 0 THEN
       IF .pos = st.pos * 20 THEN .id = 0
      END IF
     END WITH
    NEXT i
   END IF

   'Mouse controls
   DIM spot as XYPair = st.pos
   DIM npc_d as DirNum = -1  'If not -1, create an NPC facing this direction
   IF mouse_attention = focusTopBar then
    IF mouse.release AND (mouseLeft OR mouseRight) THEN
     mapedit_edit_npcdef st, st.map.npc_def(st.cur_npc)
    END IF
   ELSEIF mouse_attention = focusMap then
    IF mouse.release AND mouseLeft THEN
     'As a kludge, we didn't move the cursor to the mouse, to avoid the
     'cursor overlapping the NPC which we will now place
     spot = mouse_over_tile
     DIM diff as XYPair = ABS(mouse.pos - mouse.clickstart)
     IF diff.x > 5 OR diff.y > 5 THEN
      'Click and drag to set an NPC's direction
      npc_d = xypair_to_direction(mouse.pos - mouse.clickstart)
     END IF
    END IF
    IF mouse.release AND mouseRight THEN
     IF mapedit_npc_at_spot(st, mouse_over_tile) > -1 THEN
      mapedit_list_npcs_by_tile st, mouse_over_tile
     ELSE
      st.pos = mouse_over_tile
     END IF
    END IF
   END IF

   'Keyboard
   IF keyval(scAnyEnter) > 1 THEN
    IF mapedit_npc_at_spot(st, spot) > -1 THEN
     mapedit_list_npcs_by_tile st, spot
    ELSE
     mapedit_edit_npcdef st, st.map.npc_def(st.cur_npc)
    END IF
   END IF
   'Note that pressing SPACE+arrow keys at the same time will place an NPC and
   'move the cursor: "RMZ-style NPC placement"
   IF (keyval(scCtrl) > 0 AND keyval(scShift) = 0) OR keyval(scSpace) > 1 THEN
    IF slowkey(scUp, 660)    THEN npc_d = 0
    IF slowkey(scRight, 660) THEN npc_d = 1
    IF slowkey(scDown, 660)  THEN npc_d = 2
    IF slowkey(scLeft, 660)  THEN npc_d = 3
   END IF

   'Place or delete an NPC
   IF tool_actkeypress OR npc_d > -1 THEN
    DIM npc_slot as integer = 0
    IF npc_d = -1 THEN
     DIM npci as integer = mapedit_npc_at_spot(st, spot)
     IF npci > -1 THEN
      WITH st.map.npc(npci)
       .id = 0
       .x = 0
       .y = 0
       .dir = 0
       npc_slot = 1
      END WITH
     END IF
    END IF
    IF npc_d = -1 THEN npc_d = 2
    IF npc_slot = 0 THEN
     npc_slot = -1
     FOR i as integer = 299 TO 0 STEP -1
      IF st.map.npc(i).id = 0 THEN npc_slot = i
     NEXT i
     IF npc_slot >= 0 THEN
      st.map.npc(npc_slot).pos = spot * 20
      st.map.npc(npc_slot).id = st.cur_npc + 1
      st.map.npc(npc_slot).dir = npc_d
     END IF
    END IF
   END IF
   intgrabber(st.cur_npc, 0, UBOUND(st.map.npc_def), scLeftCaret, scRightCaret, , , , wheelAlways)

   '---FOEMODE--------
  CASE foe_mode
   IF keyval(scCtrl) = 0 AND keyval(scF1) > 1 THEN show_help "mapedit_foemap"
   intgrabber(st.cur_foe, 0, 255, scLeftCaret, scRightCaret, , , , wheelAlways)
   IF keyval(scG) > 1 THEN st.cur_foe = readblock(st.map.foemap, st.x, st.y)
   st.tool_value = st.cur_foe

   '---ZONEMODE--------
  CASE zone_mode
   IF keyval(scCtrl) = 0 AND keyval(scF1) > 1 THEN
    IF st.zonesubmode THEN show_help "mapedit_zonemap_view" ELSE show_help "mapedit_zonemap_edit"
   END IF
   IF keyval(scCtrl) = 0 THEN
    IF keyval(scZ) > 1 THEN
     st.zonesubmode = st.zonesubmode XOR 1
     st.toolsbar_available = (st.zonesubmode = zone_edit_mode)
     st.drawing_allowed = (st.zonesubmode = zone_edit_mode)
     IF st.zonesubmode = zone_view_mode THEN st.tool = draw_tool
     st.zones_needupdate = YES
    END IF
    IF keyval(scE) > 1 THEN
     mapedit_edit_zoneinfo st
     st.zones_needupdate = YES  'st.cur_zone might change, amongst other things
    END IF
   END IF
   IF st.reset_tool THEN st.tool_value = YES
   IF st.tool = draw_tool ANDALSO tool_newkeypress THEN 'pick value intelligently
    st.tool_value = CheckZoneAtTile(st.map.zmap, st.cur_zone, st.x, st.y) XOR YES
   END IF
   IF st.tool <> draw_tool ANDALSO (keyval(scPlus) > 1 OR keyval(scMinus) > 1) THEN
    st.tool_value XOR= YES
   END IF
   IF st.zonesubmode = zone_edit_mode THEN

    '--Tiling/editing mode

    st.zones_needupdate OR= intgrabber(st.cur_zone, 1, zoneLASTREADABLE, scLeftCaret, scRightCaret, , , , wheelAlways)
    IF st.tool <> paint_tool THEN
     'Using the paint tool enables pageup/pagedown for changing selected map layer
     st.zones_needupdate OR= keygrabber(st.cur_zone, 1, zoneLASTREADABLE, scPageDown, scPageUp)
    END IF
    st.cur_zinfo = GetZoneInfo(st.map.zmap, st.cur_zone)
    IF keyval(scQ) > 1 AND keyval(scCtrl) > 0 THEN
     DebugZoneMap st.map.zmap, st.x, st.y
     ''paint a whole lot of tiles over the map randomly
     'mapedit_zonespam st
     'st.zones_needupdate = YES
    END IF

   ELSE
    '--Multizone view
    usemenu st.zonemenustate, cast(BasicMenuItem vector, st.zonemenu), scLeftCaret, scRightCaret
    IF st.zonemenustate.pt > -1 THEN
     st.cur_zone = st.zonemenu[st.zonemenustate.pt].dat
     st.cur_zinfo = GetZoneInfo(st.map.zmap, st.cur_zone)
     IF keyval(scL) > 1 THEN  'Lock/Unlock
      IF int_array_find(st.lockedzonelist(), st.cur_zone) > -1 THEN
       int_array_remove(st.lockedzonelist(), st.cur_zone)
      ELSEIF UBOUND(st.lockedzonelist) + 1 < 8 THEN
       int_array_append(st.lockedzonelist(), st.cur_zone)
       st.cur_zinfo->hidden = NO  'Doesn't make sense for a zone to be hidden and locked
      END IF
      st.zones_needupdate = YES
     END IF
     IF keyval(scH) > 1 THEN
      st.cur_zinfo->hidden XOR= YES
      int_array_remove(st.lockedzonelist(), st.cur_zone)  'Doesn't make sense for a zone to be hidden and locked
      st.zones_needupdate = YES
     END IF
    END IF
    'You may draw if you lock the zone first to avoid weird graphical glitches
    st.drawing_allowed = (int_array_find(st.lockedzonelist(), st.cur_zone) > -1)
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

 '--Camera and cursor movement
 '(Cursor movement with mouse was handled above, because we generally need to update
 'cursor position before handling clicks)

 DIM mouse_pan as bool
 IF mouse_attention = focusMap ANDALSO (mouse.dragging AND mouseRight) _
    ANDALSO st.mouse_skewing = NO THEN
  mouse_pan = YES
  st.camera += (mouse.pos - mouse.clickstart) \ 4
  mapedit_constrain_camera st
  'Don't ensure the cursor is on-screen
 ELSE
  'After finishing a pan, ensure cursor on the screen
  st.x = bound(st.x, (st.mapx + 19) \ 20, (st.mapx + mapviewsize.w) \ 20 - 1)
  st.y = bound(st.y, (st.mapy + 19) \ 20, (st.mapy + mapviewsize.h) \ 20 - 1)
 END IF

 'Keyboard camera+cursor controls
 DIM rate as XYPair = (1, 1)
 IF keyval(scShift) > 0 THEN rate = XY(8, 5)
 IF keyval(scAlt) = 0 AND keyval(scCtrl) = 0 THEN
  'Move cursor position
  IF slowkey(scUp, 110) THEN st.y = large(st.y - rate.y, 0)
  IF slowkey(scDown, 110) THEN st.y = small(st.y + rate.y, st.map.high - 1)
  IF slowkey(scLeft, 110) THEN st.x = large(st.x - rate.x, 0)
  IF slowkey(scRight, 110) THEN st.x = small(st.x + rate.x, st.map.wide - 1)
  IF mouse_pan = NO THEN  'Cursor can be offscreen while panning
   st.mapx = bound(st.mapx, (st.x + 1) * 20 - mapviewsize.w, st.x * 20)
   st.mapy = bound(st.mapy, (st.y + 1) * 20 - mapviewsize.h, st.y * 20)
  END IF
 END IF
 IF keyval(scAlt) > 0 AND keyval(scCtrl) = 0 THEN
  'Move camera position
  DIM oldrel as XYPair = st.pos - st.camera \ 20
  IF slowkey(scUp, 110) THEN st.mapy -= 20 * rate.y
  IF slowkey(scDown, 110) THEN st.mapy += 20 * rate.y
  IF slowkey(scLeft, 110) THEN st.mapx -= 20 * rate.x
  IF slowkey(scRight, 110) THEN st.mapx += 20 * rate.x
  mapedit_constrain_camera st
  st.pos = st.camera \ 20 + oldrel
 END IF
 st.moved = oldpos <> st.pos

 'Skew map layers (note that skew is measured in tenths of a pixel)
 IF (keyval(scCtrl) > 0 OR st.per_layer_skew <> 0) ANDALSO (mouse.dragging AND mouseRight) THEN
  'Record that we've started skewing the map with the mouse, so that we can
  'ignore this right-drag if Ctrl is released
  st.mouse_skewing = YES
 END IF
 IF keyval(scShift) > 0 AND keyval(scCtrl) > 0 THEN
  IF keyval(scLeft) > 0 THEN st.per_layer_skew.x -= 10
  IF keyval(scRight) > 0 THEN st.per_layer_skew.x += 10
  IF keyval(scUp) > 0 THEN st.per_layer_skew.y -= 10
  IF keyval(scDown) > 0 THEN st.per_layer_skew.y += 10
 ELSEIF st.mouse_skewing ANDALSO mouse.dragging AND mouseRight THEN
  DIM numlayers as integer = UBOUND(st.map.tiles) + 1  '+1 for overhead
  st.per_layer_skew = (mouse.pos - mouse.clickstart) '* 5 / numlayers
 ELSE
  'Reset once you let go of the right mouse button or shift+ctrl
  st.per_layer_skew = 0
  st.mouse_skewing = NO
 END IF


 ' 'Actual size of the map viewport showing the map
 ' '(Since the window can be larger than the map, have to clamp it)
 ' DIM maprect as RectType
 ' maprect.topleft = Type(0, 20)
 ' maprect.wide = small(st.map.wide * 20 - st.mapx, mapviewsize.w)  
 ' maprect.high = small(st.map.high * 20 - st.mapy, mapviewsize.h)

 '--Tools
 IF st.drawing_allowed AND v_len(st.mode_tools) > 0 THEN
  '--Select the tool
  IF st.toolsbar_available THEN
   FOR i as integer = 0 TO v_len(st.mode_tools) - 1
    IF keyval(scCtrl) = 0 AND keyval(st.toolinfo(st.mode_tools[i]).shortcut) > 1 THEN
     st.tool = st.mode_tools[i]
     st.reset_tool = YES
     st.tool_hold = NO
    END IF
   NEXT
  END IF

  'These two are basically tools

  IF keyval(scDelete) > 0 THEN
   st.wallmap_mask = 255   'Special case needed for wallmap mode
   st.brush(st, st.x, st.y, 0)
  END IF

  IF keyval(scW) > 1 AND keyval(scCtrl) > 0 THEN  'Ctrl+W  Paint the window/screen
   FOR tx as integer = 0 TO 15
    FOR ty as integer = 0 TO 8
     st.brush(st, st.mapx \ 20 + tx, st.mapy \ 20 + ty, st.tool_value)
    NEXT ty
   NEXT tx
  END IF

  SELECT CASE st.tool
   CASE draw_tool
    'IF keyval(scSpace) > 0 AND (st.new_stroke OR st.moved) THEN
    IF use_draw_tool OR (tool_buttonpressed AND st.moved) THEN
     '(No need to reapply brush until the cursor moves)
     st.brush(st, st.x, st.y, st.tool_value)
    END IF

   CASE box_tool
    IF st.tool_hold THEN
     IF use_draw_tool THEN
      'We have two corners
      st.tool_hold = NO
      FOR tx as integer = small(st.tool_hold_pos.x, st.x) TO large(st.tool_hold_pos.x, st.x)
       FOR ty as integer = small(st.tool_hold_pos.y, st.y) TO large(st.tool_hold_pos.y, st.y)
        st.brush(st, tx, ty, st.tool_value)
       NEXT
      NEXT
     END IF
    ELSE
     'TODO: allow dragging the cursor or mouse, rather than needing to click twice
     IF tool_newkeypress THEN
      st.tool_hold = YES
      st.tool_hold_pos = st.pos
     END IF
    END IF

   CASE fill_tool
    IF use_draw_tool THEN
     fill_map_area st, st.x, st.y, st.reader
    END IF

   CASE paint_tool
    IF use_draw_tool THEN
     fill_with_other_area st, st.x, st.y, @tilereader
    END IF

   CASE replace_tool
    IF use_draw_tool THEN
     DIM replace_old as integer
     replace_old = st.reader(st, st.x, st.y)
     FOR ty as integer = 0 to st.map.high - 1
      FOR tx as integer = 0 to st.map.wide - 1
       IF st.reader(st, tx, ty) = replace_old THEN
        st.brush(st, tx, ty, st.tool_value)
       END IF
      NEXT tx
     NEXT ty
    END IF

   CASE mark_tool
    IF tool_newkeypress THEN
     'Note that if using the mouse, you need to click twice, to can't drag to
     'select. Might want to change that, but it's the same as the sprite editor.
     IF st.tool_hold THEN
      'We have two corners
      st.tool_hold = NO
      DIM select_rect as RectType
      corners_to_rect_inclusive st.pos, st.tool_hold_pos, select_rect

      v_free st.cloned
      st.cloned = create_changelist(st, select_rect)
      st.clone_offset.x = select_rect.wide \ 2
      st.clone_offset.y = select_rect.high \ 2
      st.clone_size.w = select_rect.wide
      st.clone_size.h = select_rect.high
      st.tool = clone_tool
      st.multitile_draw_brush = NO  'Normal clone tool behaviour
     ELSE
      st.tool_hold = YES
      st.tool_hold_pos = st.pos
     END IF
    END IF

   CASE clone_tool
    IF st.cloned = NULL THEN
     st.tool = mark_tool
    ELSE
     IF use_draw_tool OR (tool_buttonpressed AND st.moved) THEN
      apply_changelist st, st.cloned, st.pos - st.clone_offset
     END IF
     IF keyval(scCtrl) > 0 AND keyval(scM) > 1 THEN
      st.clone_merge XOR= YES
     END IF
    END IF

  END SELECT
 END IF

 '--Undo/Redo
 'IF v_len(st.mode_tools) THEN
 DIM stroke as MapEditUndoTile vector = NULL
 IF keyval(scCtrl) > 0 AND keyval(scZ) > 1 THEN
  stroke = undo_stroke(st)
 END IF
 IF keyval(scCtrl) > 0 AND keyval(scY) > 1 THEN
  stroke = redo_stroke(st)
 END IF
 IF stroke THEN mapedit_show_undo_change st, stroke
 'END IF

 '--Apply temporary previews to the map
 '(Creating the secondary undo buffer causes edits to be undoable with undo_preview)
 IF st.secondary_undo_buffer THEN debugc errPromptBug, "mapedit preview: secondary_undo_buffer already exists!"
 IF st.tool = clone_tool AND st.cloned <> NULL THEN
  v_new st.secondary_undo_buffer
  apply_changelist st, st.cloned, st.pos - st.clone_offset
 END IF
 IF st.editmode = tile_mode AND st.tool = draw_tool THEN
  'TODO: some kind of equivalent for the mouse. When holding down right button?
  IF slowtog AND keyval(scSpace) = 0 THEN
   v_new st.secondary_undo_buffer
   tilebrush(st, st.x, st.y, st.tool_value)
  END IF
 END IF

 'NOTE: There should be no use of brushes below this point!!

 '--Zones update logic, here because it needs access to 'moved'
 IF st.editmode = zone_mode THEN
  IF st.zonesubmode = zone_edit_mode THEN
   IF st.zones_needupdate THEN
    CleanTilemap st.zoneoverlaymap, st.map.wide, st.map.high
    ZoneToTilemap st.map.zmap, st.zoneoverlaymap, st.cur_zone, 0
   END IF
  ELSE
   IF st.zones_needupdate OR st.moved THEN
    'Rebuilds st.zonemenu and st.zoneviewmap based on selected tile and st.lockedzonelist() 
    mapedit_update_visible_zones st
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

 st.last_pos = st.pos

 '--Draw Screen
 clearpage dpage

 '--draw map
 animatetilesets st.tilesets()

 'Draw map layers
 FOR i as integer = 0 TO UBOUND(st.map.tiles)
  IF should_draw_layer(st, i) THEN
   mapedit_draw_layer st, i, i, , st.layerpals(i)
  END IF

  IF i = 0 ANDALSO st.per_layer_skew <> 0 THEN
   'Draw the shadows on top of layer 0
   FOR layeri as integer = 1 TO UBOUND(st.map.tiles)
    IF should_draw_layer(st, layeri) THEN
     mapedit_draw_layer st, layeri, 0, , st.shadowpal
    END IF
   NEXT
   'And shadow of overhead tiles
   IF should_draw_layer(st, 0) THEN
    mapedit_draw_layer st, 0, 0, YES, st.shadowpal
   END IF
  END IF
 NEXT
 'Draw obsolete overhead tiles
 IF should_draw_layer(st, 0) THEN
  DIM height as integer = UBOUND(st.map.tiles) + 1
  mapedit_draw_layer st, 0, height, YES, st.layerpals(height)
 END IF

 '--hero start location display--
 IF gen(genStartMap) = st.map.id THEN
  IF gen(genStartX) >= st.mapx \ 20 AND gen(genStartX) <= (st.mapx + mapviewsize.w) \ 20 AND gen(genStartY) >= st.mapy \ 20 AND gen(genStartY) <= (st.mapy + mapviewsize.h) \ 20 THEN
   frame_draw st.hero_gfx.sprite + 4, st.hero_gfx.pal, gen(genStartX) * 20 - st.mapx, gen(genStartY) * 20 + 20 - st.mapy, , , dpage
   textcolor uilook(uiText), 0
   printstr "Hero", gen(genStartX) * 20 - st.mapx, gen(genStartY) * 20 + 30 - st.mapy, dpage
  END IF
 END IF

 '--point out overhead tiles so that you can see what's wrong if you accidentally use them
 IF st.editmode = tile_mode AND UBOUND(st.map.tiles) > 0 THEN
  textcolor uilook(uiSelectedItem + tog), 0
  FOR yidx as integer = 0 TO mapviewsize.h \ 20 + 1
   FOR xidx as integer = 0 TO mapviewsize.w \ 20 + 1
    DIM tilex as integer = (st.mapx \ 20) + xidx
    DIM tiley as integer = (st.mapy \ 20) + yidx
    IF tilex < st.map.wide ANDALSO tiley < st.map.high THEN
     DIM pass_overtile as integer = readblock(st.map.pass, tilex, tiley)
     DIM pixelx as integer = tilex * 20 - st.mapx
     DIM pixely as integer = tiley * 20 - st.mapy + 20  '20 for the top toolbar
     IF (pass_overtile AND passOverhead) THEN printstr "O", pixelx + 11, pixely + 11, dpage
    END IF
   NEXT xidx
  NEXT yidx
 END IF

 '--show passmap
 IF st.editmode = pass_mode THEN
  DIM col as integer = uilook(uiMenuItem + tog)
  FOR yidx as integer = 0 TO mapviewsize.h \ 20 + 1
   FOR xidx as integer = 0 TO mapviewsize.w \ 20 + 1
    DIM tilex as integer = (st.mapx \ 20) + xidx
    DIM tiley as integer = (st.mapy \ 20) + yidx
    IF tilex < st.map.wide ANDALSO tiley < st.map.high THEN
     DIM pass_overtile as integer = readblock(st.map.pass, tilex, tiley)
     DIM pixelx as integer = tilex * 20 - st.mapx
     DIM pixely as integer = tiley * 20 - st.mapy + 20  '20 for the top toolbar

     IF CheckZoneAtTile(st.map.zmap, zoneOneWayExit, tilex, tiley) THEN
      ' Draw arrows leaving this tile
      DIM temppal as Palette16
      temppal.col(1) = col

      ' Draw a circle in the center, to indicate the bit is set even if there are no walls
      IF (pass_overtile AND passAllWalls) = 0 THEN
       frame_draw st.arrow_icons(4), @temppal, pixelx, pixely, , , dpage
      END IF

      FOR direc as integer = 0 TO 3
       IF (pass_overtile AND (1 SHL direc)) THEN frame_draw st.arrow_icons(direc), @temppal, pixelx, pixely, , , dpage
      NEXT
     ELSEIF st.wallthickness > 0 THEN
      IF (pass_overtile AND passNorthWall) THEN rectangle pixelx     , pixely     , 20, st.wallthickness, col, dpage
      IF (pass_overtile AND passEastWall)  THEN rectangle pixelx + 19, pixely     , -st.wallthickness, 20, col, dpage
      IF (pass_overtile AND passSouthWall) THEN rectangle pixelx     , pixely + 19, 20, -st.wallthickness, col, dpage
      IF (pass_overtile AND passWestWall)  THEN rectangle pixelx     , pixely     , st.wallthickness, 20, col, dpage
     ELSEIF st.wallthickness = 0 THEN
      IF (pass_overtile AND passNorthWall) THEN drawants vpages(dpage), pixelx       , pixely       , 20, 1
      IF (pass_overtile AND passEastWall)  THEN drawants vpages(dpage), pixelx + 20-1, pixely       , 1, 20
      IF (pass_overtile AND passSouthWall) THEN drawants vpages(dpage), pixelx       , pixely + 20-1, 20, 1
      IF (pass_overtile AND passWestWall)  THEN drawants vpages(dpage), pixelx       , pixely       , 1, 20
     END IF

     textcolor uilook(uiSelectedItem + tog), 0
     ' Y positions
     IF (pass_overtile AND passVehA) THEN printstr "A", pixelx + 2, pixely + 1, dpage
     IF (pass_overtile AND passVehB) THEN printstr "B", pixelx + 11, pixely + 1, dpage
     IF (pass_overtile AND passHarm) THEN printstr "H", pixelx + 2, pixely + 11, dpage
     IF (pass_overtile AND passOverhead) THEN printstr "O", pixelx + 11, pixely + 11, dpage
    END IF
   NEXT xidx
  NEXT yidx
 END IF
 
 '--door display--
 IF st.editmode = door_mode THEN
  textcolor uilook(uiBackground), 0
  FOR i as integer = 0 TO UBOUND(st.map.door)
   WITH st.map.door(i)
    IF .x >= st.mapx \ 20 AND .x <= (st.mapx + mapviewsize.w) \ 20 AND _
       .y >  st.mapy \ 20 AND .y <= (st.mapy + mapviewsize.h) \ 20 + 1 AND _
       door_exists(st.map.door(), i) THEN
     rectangle .x * 20 - st.mapx, .y * 20 - st.mapy, 20, 20, uilook(uiSelectedItem + tog), dpage
     printstr STR(i), .x * 20 - st.mapx + 10 - (4 * LEN(STR(i))), .y * 20 - st.mapy + 6, dpage
    END IF
   END WITH
  NEXT
 END IF

 '--npc display--
 IF st.editmode = npc_mode THEN
  '--Determine restriction zone to display (Ugh this is pretty ugly)
  DIM oldzone as integer = st.cur_npc_zone
  DIM oldwallzone as integer = st.cur_npc_wall_zone
  st.cur_npc_zone = 0
  st.cur_npc_wall_zone = 0
  DIM npci as integer = mapedit_npc_at_spot(st, st.pos)
  IF npci > -1 THEN
   WITH st.map.npc_def(st.map.npc(npci).id - 1)
    IF .defaultzone = -1 THEN
     st.cur_npc_zone = 0
    ELSEIF .defaultzone = 0 THEN
     st.cur_npc_zone = st.map.gmap(32)
    ELSE
     st.cur_npc_zone = .defaultzone
    END IF
    IF .defaultwallzone = -1 THEN
     st.cur_npc_wall_zone = 0
    ELSEIF .defaultwallzone = 0 THEN
     st.cur_npc_wall_zone = st.map.gmap(33)
    ELSE
     st.cur_npc_wall_zone = .defaultwallzone
    END IF
   END WITH
  END IF
  IF oldzone <> st.cur_npc_zone OR oldwallzone <> st.cur_npc_wall_zone OR st.npczone_needupdate THEN
   CleanTilemap st.zoneoverlaymap, st.map.wide, st.map.high
   IF st.cur_npc_zone > 0 THEN
    ZoneToTilemap st.map.zmap, st.zoneoverlaymap, st.cur_npc_zone, 0
   END IF
   IF st.cur_npc_wall_zone > 0 THEN
    ZoneToTilemap st.map.zmap, st.zoneoverlaymap, st.cur_npc_wall_zone, 1
   END IF
   st.npczone_needupdate = NO
   'We're reusing st.zoneoverlaymap
   st.zones_needupdate = YES
  END IF
  '--Draw NPC zones
  drawmap st.zoneoverlaymap, st.mapx, st.mapy, st.overlaytileset, dpage, YES, , , 20

  '--Draw npcs
  REDIM npcnum(UBOUND(st.map.npc_def)) as integer  'Clear counts to 0
  st.walk = (st.walk + 1) MOD 4
  FOR i as integer = 0 TO UBOUND(st.map.npc)
   WITH st.map.npc(i)
    IF .id > 0 THEN
     ' +/-20 Y must be for the footoffset
     IF .x >= st.mapx AND .x < st.mapx + mapviewsize.w AND .y >= st.mapy - 20 AND .y < st.mapy + mapviewsize.h + 20 THEN
      DIM image as GraphicPair = st.npc_img(.id - 1)
      frame_draw image.sprite + (2 * .dir) + st.walk \ 2, image.pal, .x - st.mapx, .y + 20 - st.mapy + st.map.gmap(11), 1, -1, dpage
      DIM col as integer = uilook(uiSelectedItem + tog)
      edgeprint STR(.id - 1), .x - st.mapx, .y + 20 - st.mapy + 2, col, dpage
      edgeprint STR(npcnum(.id - 1)), .x - st.mapx, .y + 20 - st.mapy + 11, col, dpage
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
    IF (st.mapx \ 20) + i < st.map.wide ANDALSO (st.mapy \ 20) + o < st.map.high THEN
     DIM foe_val as integer = readblock(st.map.foemap, st.mapx \ 20 + i, st.mapy \ 20 + o)
     DIM pixelx as integer = (st.mapx \ 20 + i) * 20 - st.mapx
     DIM pixely as integer = (st.mapy \ 20 + o) * 20 - st.mapy
     IF foe_val > 0 THEN printstr STR(foe_val), pixelx - ((foe_val < 10) * 5), pixely + 26, dpage
    END IF
   NEXT o
  NEXT i
 END IF

 '--show zones
 IF st.editmode = zone_mode THEN
  IF st.zonesubmode = zone_edit_mode THEN
   'Draw a single zone
   drawmap st.zoneoverlaymap, st.mapx, st.mapy, st.overlaytileset, dpage, YES, , , 20
  ELSE
   'Draw all zones on this tile
   drawmap st.zoneviewmap, st.mapx, st.mapy, st.zonetileset(st.zoneviewtileset), dpage, YES, , , 20, , YES
   IF st.showzonehints THEN
    'Overlay 'hints' at hidden zones
    setanim ABS(st.gauze_ticker \ 5 - 4), 0
    drawmap st.zoneoverlaymap, st.mapx, st.mapy, st.overlaytileset, dpage, YES, , , 20
   END IF
  END IF
 END IF

 'Draw the cursor, including box, mark, and clone outlines, or NPC cursor
 mapedit_draw_cursor st

 '--Draw menubar (includes tileset preview)
 IF st.editmode = tile_mode THEN
  'This is to draw tile 0 as fully transparent on layer > 0
  st.menubar.layernum = st.layer
  draw_background vpages(dpage), IIF(st.layer > 0, bgChequerScroll, 0), chequer_scroll, 0, 0, rWidth - 40, 20
  drawmap st.menubar, st.menubarstart(st.layer) * 20, 0, st.tilesets(st.layer), dpage, YES, , , 0, 20
  'Don't show (black out) the last three tiles on the menubar, because they
  'are overlaid too much by the icons.
  rectangle pRight, 0, 60, 20, uilook(uiBackground), dpage
 ELSE
  rectangle 0, 0, rWidth, 20, uilook(uiBackground), dpage
 END IF
 rectangle 0, 19, rWidth, 1, uilook(uiText), dpage

 '--menubar cursor
 IF st.editmode = tile_mode THEN
  frame_draw st.cursor.sprite + tog, st.cursor.pal, ((st.usetile(st.layer) - st.menubarstart(st.layer)) * 20), 0, , , dpage
 END IF

 '--position finder--
 IF st.tiny THEN
  fuzzyrect 0, 35, st.map.wide, st.map.high, uilook(uiHighlight), dpage
  rectangle st.mapx \ 20, (st.mapy \ 20) + 35, 16, 9, uilook(uiDescription), dpage
  IF st.editmode = zone_mode THEN
   frame_draw st.zoneminimap, NULL, 0, 35, , , dpage
  END IF
 END IF

 '--npc info
 IF st.editmode = npc_mode THEN
  edgeprint npc_preview_text(st.map.npc_def(st.cur_npc)), 0, 0, uilook(uiText), dpage
  edgeprint mapedit_npc_instance_count(st, st.cur_npc) & " copies of " & CHR(27) & "NPC " & st.cur_npc & CHR(26) & " on this map", 0, 10, uilook(uiText), dpage
 END IF
 
 edgeprint "X " & st.x & "   Y " & st.y, 0, maprect.p2.y - 9, uilook(uiSelectedItem + tog), dpage
 edgeprint st.modenames(st.editmode), 0, 24, uilook(uiText), dpage

 '--Tool selection
 IF st.toolsbar_available THEN
  DIM toolbarpos as RectType = toolbar_rect(st)
  rectangle toolbarpos.x, toolbarpos.y, toolbarpos.wide, toolbarpos.high, uilook(uiBackground), dpage
  FOR i as integer = 0 TO v_len(st.mode_tools) - 1
   mapedit_draw_icon st, st.toolinfo(st.mode_tools[i]).icon, toolbarpos.x + i * 10, toolbarpos.y, (st.tool = st.mode_tools[i])
  NEXT
  DIM tmpstr as string
  IF mouse_over_tool <> no_tool THEN
   tmpstr = "Tool: " & st.toolinfo(mouse_over_tool).name
  ELSEIF st.tool = paint_tool THEN
   'Show the current layer if using the paint tool, since that depends on selected tilemap layer
   tmpstr = "Tool:Paint on " & hilite("Layer " & st.layer)
  ELSE
   tmpstr = "Tool: " & st.toolinfo(st.tool).name
  END IF
  edgeprint tmpstr, pRight, toolbarpos.y + 10, uilook(uiText), dpage, YES
 ELSEIF st.editmode = zone_mode AND st.zonesubmode = zone_view_mode AND st.drawing_allowed THEN
  'Nasty
  edgeprint "Tool: Draw", pRight, 22, uilook(uiText), dpage
 END IF

 IF st.editmode = tile_mode THEN
  DIM col as integer
  IF st.tool = mark_tool OR (st.tool = clone_tool AND st.multitile_draw_brush = NO) THEN
   'Hint that the current layer doesn't matter
   col = uilook(uiText)
  ELSE
   col = uilook(uiSelectedItem + tog)
  END IF
  DIM layername as string
  layername = "Layer " & st.layer
  IF layerisvisible(st.visible(), st.layer) = NO THEN layername &= " (invisible)"
  layername &= " " & read_map_layer_name(st.map.gmap(), st.layer)
  layername = RIGHT(layername, 40)
  edgeprint layername, 0, maprect.p2.y - 20, col, dpage
 END IF

 IF st.editmode = tile_mode OR st.tool = clone_tool THEN
  'Also show the default walls option when using clone tool as it is affected by them
  textcolor uilook(uiText), 0
  DIM defpass_msg as string = hilite("Ctrl+D: ")
  IF st.defpass = NO THEN defpass_msg &= "No "
  defpass_msg &= "Default Walls"
  printstr defpass_msg, maprect.p2.x - 196, maprect.p2.y - 8, dpage, YES
 END IF

 IF st.editmode = pass_mode THEN
  printstr ticklite("one`W`ay  `H`arm  vehicle Ctrl-`A B`"), 0, 0, dpage, YES
  printstr ticklite("`+`/`-`: thickness"), 0, 10, dpage, YES

  IF CheckZoneAtTile(st.map.zmap, zoneOneWayExit, st.x, st.y) THEN
   printstr hilite("W") + ": one-way walls", maprect.p2.x - 196, maprect.p2.y - 8, dpage, YES
  END IF
 END IF

 IF st.tool = clone_tool THEN
  textcolor uilook(uiText), 0
  printstr hilite("Ctrl+M: ") & IIF(st.clone_merge, "Tile Merging On", "Tile Merging Off"), maprect.p2.x - 196, maprect.p2.y - 16, dpage, YES
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
     printstr hilite("+") + "/" + hilite("-") + IIF(st.tool_value, ": Adding tiles", ": Removing tiles"), 10, 6, dpage, YES
   END SELECT

   edgeprint "(" + hilite("Z") + ": Editing)", 140, 24, uilook(uiText), dpage, YES
  ELSE
   edgeprint "(" + hilite("Z") + ": Viewing)", 140, 24, uilook(uiText), dpage, YES
   IF st.zonemenustate.pt = -1 THEN zoneselected = NO
  END IF

  IF zoneselected THEN
   edgeprint hilite("Zone " & st.cur_zone) & " (" & st.cur_zinfo->numtiles & " tiles) " & st.cur_zinfo->name, _
             0, rBottom - 20, uilook(uiText), dpage, YES
  END IF

  IF st.zonesubmode = zone_edit_mode THEN
   '-- Edit mode

   IF st.tool <> clone_tool THEN
    'Don't overdraw "Default Walls"
    printstr hilite("E") + "dit zone info", 116, pBottom, dpage, YES
   END IF

  ELSE
   '-- View mode

   printstr IIF(st.autoshow_zones,"      ","Don't ") & hilite("A") + "utoshow zones  " _
            & IIF(st.showzonehints,"      ","Don't ") & hilite("S") + "how other", 0, 5, dpage, YES

   IF zoneselected THEN
    DIM is_locked as integer = (int_array_find(st.lockedzonelist(), st.cur_zone) > -1)
    printstr hilite("E") + "dit/" _
             & IIF(st.cur_zinfo->hidden,"un","") + hilite("H") + "ide/" _
             & IIF(is_locked,"un","") + hilite("L") + "ock zone", pRight - 20, pBottom, dpage, YES
   END IF

   'Draw zonemenu
   DIM pixel_width as integer = vpages(dpage)->w \ 3
   ' Where to put the menu
   DIM as integer xpos, ypos = 40
   xpos = vpages(dpage)->w - pixel_width
   IF (st.x * 20) - st.mapx > xpos AND st.tiny = NO THEN
    xpos = 8
   END IF
   DIM zmenuopts as MenuOptions
   zmenuopts.edged = YES
   zmenuopts.wide = pixel_width
   zmenuopts.itemspacing = -1  'Squeeze so all zones can fit in at 320x200
   standardmenu cast(BasicMenuItem vector, st.zonemenu), st.zonemenustate, xpos, ypos, dpage, zmenuopts

   IF st.zonemenustate.pt > -1 THEN
    ' A little right arrow
    edgeprint CHR(26), xpos - 8, ypos + (st.zonemenustate.pt - st.zonemenustate.top) * st.zonemenustate.spacing, uilook(uiText), dpage
   END IF

  END IF
 END IF

 '--Undo preview edits
 undo_preview st

 '--Message
 IF st.message_ticks > 0 THEN
  basic_textbox st.message, , dpage, 11, , YES
 END IF

 SWAP vpage, dpage
 setvispage vpage
 update_tilepicker st  'Call in case the window size changed
 IF dowait THEN slowtog XOR= 1
LOOP
st.message_ticks = 0
END SUB

'==========================================================================================

'==========================================================================================
'                                Cursor and tool helpers

'Draw the appropriate cursor and box for the selected tool
SUB mapedit_draw_cursor(st as MapEditState)
 DIM as RectType tool_rect = map_to_screen(st, mapedit_tool_rect(st))
 SELECT CASE st.tool
  CASE box_tool, mark_tool
   IF st.tool_hold THEN
    'Just draw a cheap rectangle on the screen, because I'm lazy. Drawing something different
    'for different brushes is non-trivial, and besides, how should layers work?
    drawcube vpages(dpage), tool_rect, tool_cube_offset(st), uilook(uiHighlight + global_tog), 4
    EXIT SUB
   END IF
   'Otherwise, draw the default cursor

  CASE clone_tool
   drawcube vpages(dpage), tool_rect, tool_cube_offset(st), uilook(uiHighlight + global_tog)
   EXIT SUB

  CASE npc_tool
   'Don't draw a cursor
   WITH st.npc_img(st.cur_npc)
    frame_draw .sprite + (2 * st.walk), .pal, tool_rect.x, tool_rect.y + st.map.gmap(11), 1, -1, dpage
   END WITH
   edgeprint STR(st.cur_npc), tool_rect.x, tool_rect.y + 8, uilook(uiSelectedItem + global_tog), dpage
   EXIT SUB
 END SELECT

 'Normal cursor
 drawcube vpages(dpage), tool_rect, tool_cube_offset(st), uilook(uiMenuItem)
 tool_rect += st.per_layer_skew * st.layer / 10
 frame_draw st.cursor.sprite + global_tog, st.cursor.pal, tool_rect.x, tool_rect.y, , , dpage
END SUB

FUNCTION mapedit_tool_rect(st as MapEditState) as RectType
 SELECT CASE st.tool
  CASE box_tool, mark_tool
   IF st.tool_hold THEN
    'Just draw a cheap rectangle on the screen, because I'm lazy. Drawing something different
    'for different brushes is non-trivial, and besides, how should layers work?
    DIM as RectType select_rect
    corners_to_rect_inclusive st.pos, st.tool_hold_pos, select_rect
    RETURN select_rect * 20
   END IF
   'Otherwise, return the default

  CASE clone_tool
   DIM as RectType clone_box
   clone_box.topleft = st.pos - st.clone_offset
   clone_box.size = st.clone_size
   RETURN clone_box * 20
 END SELECT

 'Default
  DIM as RectType cursor_box
  cursor_box.topleft = st.pos * 20
  cursor_box.size = XY(20, 20)
  RETURN cursor_box
END FUNCTION

'Compute the offset arg to drawcube, when drawing the cursor
FUNCTION tool_cube_offset(st as MapEditState) as XYPair
 RETURN st.per_layer_skew * UBOUND(st.map.tiles) / 10
END FUNCTION

FUNCTION toolbar_rect(st as MapEditState) as RectType
 DIM ret as RectType
 ret.wide = 10 * v_len(st.mode_tools)
 ret.high = 8
 ret.x = vpages(dpage)->w - ret.wide
 IF st.editmode = tile_mode THEN
  ret.y = 12
 END IF
 RETURN ret
END FUNCTION


'==========================================================================================

PRIVATE SUB mapedit_list_npcs_by_tile_update (st as MapEditState, pos as XYPair, menu() as string, npcrefs() as integer)
 DIM dir_str(...) as string = {"north", "east", "south", "west"}

 REDIM npcrefs(0) as integer
 npcrefs(0) = -1
 REDIM menu(0) as string
 menu(0) = "Back to the map editor..."

 FOR i as integer = 0 TO UBOUND(st.map.npc)
  WITH st.map.npc(i)
   IF .id > 0 THEN
    IF .x = pos.x * 20 AND .y = pos.y * 20 THEN
     DIM s as string
     s = "NPC ID=" & (.id - 1) & " facing " & dir_str(.dir)
     str_array_append menu(), s
     int_array_append npcrefs(), i
    END IF
   END IF
  END WITH
 NEXT i
END SUB

'Menu which shows the NPCs at the specified tile, and allows you to edit them
SUB mapedit_list_npcs_by_tile (st as MapEditState, pos as XYPair)
 REDIM npcrefs(0) as integer  'Index of NPC in st.map.npc() for each menu item
 REDIM menu(0) as string
 DIM boxpreview as string
 DIM npcinst as NPCInst ptr = NULL  'Convenience ptrs
 DIM npcdef as NPCType ptr = NULL

 DIM state as MenuState
 state.size = 20
 state.need_update = YES

 setkeys
 DO
  setwait 55
  setkeys

  IF keyval(scF1) > 1 THEN show_help "mapedit_npcs_by_tile"
  IF keyval(scESC) > 1 THEN EXIT DO
  IF enter_space_click(state) THEN
   clearkey(scSpace)
   IF state.pt = 0 THEN
    EXIT DO
   ELSEIF npcdef THEN
    mapedit_edit_npcdef st, *npcdef
    state.need_update = YES
   END IF
  END IF

  state.need_update OR= usemenu(state)

  IF state.need_update THEN
   state.need_update = NO
   mapedit_list_npcs_by_tile_update st, pos, menu(), npcrefs()
   state.last = UBOUND(menu)
   npcinst = NULL
   npcdef = NULL
   IF state.pt > 0 AND state.pt <= UBOUND(npcrefs) THEN
    npcinst = @st.map.npc(npcrefs(state.pt))
    npcdef = @st.map.npc_def(npcinst->id - 1)
    boxpreview = npc_preview_text(*npcdef)
   END IF
  END IF

  clearpage dpage
  edgeprint UBOUND(npcrefs) & " NPCs at tile X=" & pos.x & " Y=" & pos.y, 0, 0, uilook(uiSelectedDisabled), dpage
  standardmenu menu(), state, 0, 10, dpage
  IF npcdef THEN
   edgeprint "Enter/Space/Click to edit", 0, pBottom - 21, uilook(uiSelectedDisabled), dpage
   'Display a frame in right direction
   npcdefedit_preview_npc *npcdef, st.npc_img(npcinst->id - 1), boxpreview, npcinst->dir * 2
  END IF

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB

FUNCTION mapedit_npc_at_spot(st as MapEditState, pos as XYPair) as integer
 FOR i as integer = 0 TO UBOUND(st.map.npc)
  WITH st.map.npc(i)
   IF .id > 0 THEN
    IF .x = pos.x * 20 AND .y = pos.y * 20 THEN RETURN i
   END IF
  END WITH
 NEXT i
 RETURN -1
END FUNCTION

'==========================================================================================

'This is a variant on spriteedit_draw_icon
SUB mapedit_draw_icon(st as MapEditState, icon as string, x as RelPos, y as RelPos, highlight as bool = NO)
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

SUB load_npc_graphics(npc_def() as NPCType, npc_img() as GraphicPair)
 ' Resizes and fills npc_img()
 FOR i as integer = 0 TO UBOUND(npc_img)
  unload_sprite_and_pal npc_img(i)
 NEXT
 REDIM npc_img(UBOUND(npc_def))
 FOR i as integer = 0 TO UBOUND(npc_def)
  load_sprite_and_pal npc_img(i), sprTypeWalkabout, npc_def(i).picture, npc_def(i).palette
 NEXT i
END SUB


'==========================================================================================


'The amount to offset the position of a map layer due to jiggling and other cues
FUNCTION mapedit_layer_offset(st as MapEditState, i as integer) as XYPair
 DIM offset as XYPair
 IF readbit(st.jiggle(), 0, i) AND global_tog THEN
  IF (i mod 8) >= 1 AND (i mod 8) <= 3 THEN offset.x = 1
  IF (i mod 8) >= 5 THEN offset.x = -1
  IF (i mod 8) <= 1 OR (i mod 8) = 7 THEN offset.y = -1
  IF (i mod 8) >= 3 AND (i mod 8) <= 5 THEN offset.y = 1
  offset.x *= i \ 8 + 1
  offset.y *= i \ 8 + 1
 END IF
 offset -= st.per_layer_skew * i / 10
 RETURN offset
END FUNCTION

'overhead: draw the overhead layer (layernum should be 0)
SUB mapedit_draw_layer(st as MapEditState, layernum as integer, height as integer, overhead as bool = NO, pal as Palette16 ptr = NULL)
 DIM pos as XYPair = st.camera + mapedit_layer_offset(st, height)
 DIM trans as bool
 DIM overheadmode as integer
 IF overhead THEN
  trans = NO
  overheadmode = 2
 ELSE
  trans = layernum > 0
  overheadmode = IIF(layernum = 0, 1, 0)
 END IF
 drawmap st.map.tiles(layernum), pos.x, pos.y, st.tilesets(layernum), dpage, _
         trans, overheadmode, @st.map.pass, 20, , pal
END SUB


'==========================================================================================
'                                     Layer palettes

FUNCTION layer_shadow_palette() as Palette16 ptr
 DIM ret as Palette16 ptr = palette16_new_identity(256)
 palette16_transform_n_match ret, copGreyscale
 DIM black as RGBcolor
 palette16_mix_n_match ret, black, 0.60, mixBlend  'Darken by 60%
 RETURN ret
END FUNCTION

SUB mapedit_free_layer_palettes(st as MapEditState)
 FOR idx as integer = 0 TO UBOUND(st.layerpals)
  palette16_unload @st.layerpals(idx)
 NEXT
END SUB

'Set layer palettes to tint according to layer no.
FUNCTION mapedit_layer_tint_palette(st as MapEditState, layer as integer, lastlayer as integer) as Palette16 ptr
 DIM as double hfrac, heightfrac = layer / lastlayer
 DIM col as RGBcolor
 ' Cyan - green - yellow - red - purple
 IF heightfrac < 1/4 THEN
  ' (Blueish) cyan to green
  hfrac = heightfrac * 3
  col.r = 0
  col.g = 196 + hfrac * 59
  col.b = (1 - hfrac) * 255
 ELSEIF heightfrac < 2/4 THEN
  ' Green to yellow
  hfrac = (heightfrac - 1/4) * 4
  col.r = hfrac * 255
  col.g = 255
  col.b = 0
 ELSEIF heightfrac < 3/4 THEN
  ' Yellow to red
  hfrac = (heightfrac - 2/4) * 4
  col.r = 255
  col.g = (1 - hfrac) * 255
  col.b = 0
 ELSE
  ' Red to purple
  hfrac = (heightfrac - 3/4) * 4
  col.r = 255
  col.g = 0
  col.b = hfrac * 255
 END IF
 DIM ret as Palette16 ptr = palette16_new_identity(256)
 'Convert to grey, taking the Value rather than Luminance,
 'which is far too dark, especially for greys
 palette16_transform_n_match ret, copTintValue   'copValue
 palette16_mix_n_match ret, col, 1, mixMult  'Tint
 RETURN ret
END FUNCTION

'Set layer palettes to highlight the selected one
FUNCTION mapedit_layer_highlight_palette(st as MapEditState, layer as integer, lastlayer as integer) as Palette16 ptr
 'For all layers except selected (including overhead for layer 0)
 IF st.layer = layer OR (st.layer = 0 AND layer = lastlayer) THEN RETURN NULL

 DIM ret as Palette16 ptr = palette16_new_identity(256)
 palette16_transform_n_match ret, copGreyscale
 'And pull a little towards a mid-grey, in case the tiles are already grey
 DIM grey as RGBcolor
 grey.col = &h505050
 palette16_mix_n_match ret, grey, 0.2, mixBlend
 RETURN ret
END FUNCTION

SUB mapedit_update_layer_palettes(st as MapEditState)
 'By default no palette
 mapedit_free_layer_palettes st

 DIM lastlayer as integer = UBOUND(st.map.tiles) + 1  '+1 for overhead
 FOR layer as integer = 0 TO lastlayer
  IF st.layer_display_mode = layerDisplayTinted THEN
   st.layerpals(layer) = mapedit_layer_tint_palette(st, layer, lastlayer)
  ELSEIF st.layer_display_mode = layerDisplayHighlight THEN
   st.layerpals(layer) = mapedit_layer_highlight_palette(st, layer, lastlayer)
  END IF
 NEXT
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

'Rebuilds zonemenu and st.zoneviewmap based on selected tile and st.lockedzonelist() 
SUB mapedit_update_visible_zones (st as MapEditState)

 REDIM tilezonelist(-1 TO -1) as integer  'The zones at the current tile (index 0 onwards, start at -1 for fake zero-length arrays)
 REDIM viszonelist(-1 TO 0) as integer    'The currently displayed zones. At most 8. (index 0 onwards, start at -1 for fake zero-length arrays)
 DIM i as integer

 'Find the previous selection, so can move the cursor to something appropriate
 DIM oldpt_zone as integer = -1
 DIM oldpt_waslocked as bool = NO
 IF st.zonemenustate.pt <> -1 THEN
  oldpt_zone = st.zonemenu[st.zonemenustate.pt].dat
  'Search for "Zones here:", yeah, real ugly
  FOR i as integer = st.zonemenustate.pt TO v_len(st.zonemenu) - 1
   IF st.zonemenu[i].dat = 0 THEN oldpt_waslocked = YES
  NEXT
'  oldpt_waslocked = (st.zonemenustate.pt <= UBOUND(st.lockedzonelist) + 1)
 END IF

 GetZonesAtTile st.map.zmap, tilezonelist(), st.x, st.y ', zoneLASTUSER

 'Decide upon visible zones

 REDIM viszonelist(-1 TO -1)
 FOR i as integer = 0 TO UBOUND(st.lockedzonelist)
  mapedit_try_assign_colour_to_zone st.lockedzonelist(i), st.zonecolours(), viszonelist()
 NEXT

 IF st.autoshow_zones THEN
  'Try to add some of the zones at this tile to the visible zone list

  'Assign remaining colours/patterns to some zones at this tile
  FOR i as integer = 0 TO UBOUND(tilezonelist)
   IF UBOUND(viszonelist) >= 7 THEN EXIT FOR
   IF GetZoneInfo(st.map.zmap, tilezonelist(i))->hidden THEN CONTINUE FOR
   mapedit_try_assign_colour_to_zone tilezonelist(i), st.zonecolours(), viszonelist()
  NEXT
 END IF

 'Rebuild the menu
 v_free st.zonemenu
 v_new st.zonemenu
 IF UBOUND(st.lockedzonelist) >= 0 THEN
  append_simplemenu_item st.zonemenu, "Locked zones:", YES, uilook(uiText)
 END IF
 FOR i as integer = 0 TO UBOUND(st.lockedzonelist)
  zonemenu_add_zone st.zonemenu, st.zonecolours(), GetZoneInfo(st.map.zmap, st.lockedzonelist(i))
 NEXT

 append_simplemenu_item st.zonemenu, IIF(UBOUND(tilezonelist) >= 0, "Zones here:", "No zones here"), YES, uilook(uiText)
 DIM tileliststart as integer = v_len(st.zonemenu)
 FOR i as integer = 0 TO UBOUND(tilezonelist)
  zonemenu_add_zone st.zonemenu, st.zonecolours(), GetZoneInfo(st.map.zmap, tilezonelist(i))
 NEXT

 st.zonemenustate.size = 15
 'sets .pt to something valid, or -1 if nothing selectable
 init_menu_state st.zonemenustate, cast(BasicMenuItem vector, st.zonemenu)

 'Pick a good selection automatically
 IF st.zonemenustate.pt <> -1 THEN
  IF oldpt_waslocked THEN
'   st.zonemenustate.pt = bound(st.zonemenustate.pt, 1, UBOUND(st.lockedzonelist) + 1)
  ELSE
   IF tileliststart < v_len(st.zonemenu) THEN
    st.zonemenustate.pt = tileliststart
    FOR i as integer = v_len(st.zonemenu) - 1 TO 0 STEP -1
     IF st.zonemenu[i].dat = oldpt_zone THEN st.zonemenustate.pt = i: EXIT FOR
     IF st.zonemenu[i].dat = 0 THEN EXIT FOR
    NEXT
   END IF
  END IF
 END IF

 'Update the zoneviewmap
 CleanTilemap st.zoneviewmap, st.map.wide, st.map.high
 FOR i as integer = 0 TO UBOUND(viszonelist)
  DIM colour as integer = int_array_find(st.zonecolours(), viszonelist(i))
  ZoneToTilemap st.map.zmap, st.zoneviewmap, viszonelist(i), colour
 NEXT
 'needs to be called after zoneviewmap is updated, to show hidden zones
 mapedit_doZoneHinting st

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
'What's happening here is that st.zoneviewmap is displaying up at 8
'selected zones, with the 8 bits of each tile indicating whether those 8 zones
'(recall that st.zoneviewmap is drawn with a special tileset with 256 tiles).
'So at each tile we just compare whether the number of set bits in the zonemap is
'greater than the number in st.zoneviewmap.
'It may be a good idea to not show hidden zones, unfortunately that would be difficult/really slow
SUB mapedit_doZoneHinting(st as MapEditState)
  CleanTilemap st.zoneoverlaymap, st.zoneviewmap.wide, st.zoneviewmap.high
  WITH st.map.zmap
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
SUB mapedit_zonespam(st as MapEditState)
 DIM t as double = TIMER
 DIM as integer x, y, i, temp, count = st.cur_zinfo->numtiles
 FOR i as integer = 0 TO INT((1 + rando()) * st.map.zmap.high / 8)
  y = randint(st.map.zmap.high)
  temp = randint(st.map.zmap.wide)
  FOR x = temp TO small(temp + 12, st.map.zmap.wide - 1)
   zonebrush st, x, y, 1
  NEXT
 NEXT

 t = TIMER - t
 count = st.cur_zinfo->numtiles - count
 debug "zonespam: spammed " & count & " tiles, " & (1000 * t / count) & "ms/tile"
END SUB


'==========================================================================================
'                                    Zone info editor
'==========================================================================================


SUB mapedit_edit_zoneinfo(st as MapEditState)
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
    IF intgrabber(st.cur_zone, 1, zoneLASTREADABLE) THEN
     state.need_update = YES
     st.cur_zinfo = GetZoneInfo(st.map.zmap, st.cur_zone)
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
FUNCTION need_default_edge_tile(map as MapData) as bool
 IF map.gmap(5) = 2 THEN RETURN YES  'Use default edge tile
 IF map.gmap(5) = 0 THEN  'Crop camera
  IF gen(genResolutionX) > map.wide * 20 ORELSE gen(genResolutionY) > map.high * 20 THEN RETURN YES
 END IF
 RETURN NO
END FUNCTION

'st.map.gmap() is passed as gmap(), for convenience.
SUB mapedit_gmapdata_buildmenu(st as MapEditState, byref menu as SimpleMenuItem vector, gmap() as integer, gdidx() as integer, midx() as integer, script_defaults() as integer)

 v_new menu
 REDIM gdidx(25)
 gdidx(0)  = -1: append_simplemenu_item menu, "Previous Menu"
 gdidx(1)  = 1:  append_simplemenu_item menu, "Ambient Music: "
 gdidx(2)  = 2:  append_simplemenu_item menu, "Minimap Available: "
 gdidx(3)  = 3:  append_simplemenu_item menu, "Save Anywhere: "
 gdidx(4)  = 18: append_simplemenu_item menu, "Tile Data: "
 gdidx(5)  = 17: append_simplemenu_item menu, "NPC Data: "
 gdidx(6)  = 32: append_simplemenu_item menu, "Default NPC Move Zone: "
 gdidx(7)  = 33: append_simplemenu_item menu, "Default NPC Avoid Zone: "
 gdidx(8)  = 378: append_simplemenu_item menu, "Default Pathfinding rules: "

 'Add an extra gap
 gdidx(9) = -1: append_simplemenu_item menu, "", YES
 gdidx(10)  = -1: append_simplemenu_item menu, "Display:", YES, uilook(uiText)

 gdidx(11)  = 11: append_simplemenu_item menu, "Foot Offset: "
 gdidx(12) = 16: append_simplemenu_item menu, "Walkabout Layering: "
 gdidx(13) = 4:  append_simplemenu_item menu, "Display Map Name: "
 gdidx(14) = 10: append_simplemenu_item menu, "Harm-Tile Flash: "   'flash colour drawn here
 gdidx(15) = 9:  append_simplemenu_item menu, "Harm-Tile Damage: "
 gdidx(16) = 5:  append_simplemenu_item menu, "Map Edge Mode: " 
 gdidx(17) = 6:  append_simplemenu_item menu, "Default Edge Tile: " 'edge tile drawn here

 'Add an extra gap, for the edge tile preview
 gdidx(18) = -1: append_simplemenu_item menu, "", YES

 gdidx(19) = -1: append_simplemenu_item menu, "Scripts:", YES, uilook(uiText)
 gdidx(20) = 7:  append_simplemenu_item menu, "Autorun Script: "
 gdidx(21) = 8:  append_simplemenu_item menu, "Autorun Script Argument: "
 gdidx(22) = 12: append_simplemenu_item menu, "After-Battle Script: "
 gdidx(23) = 13: append_simplemenu_item menu, "Instead-of-Battle Script: "
 gdidx(24) = 14: append_simplemenu_item menu, "Each-Step Script: "
 gdidx(25) = 15: append_simplemenu_item menu, "On-Keypress Script: "

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
 IF need_default_edge_tile(st.map) THEN
  menu[midx(6)].text &= gmap(6)
 ELSE
  menu[midx(6)].text &= "N/A"
 END IF

 ' Scripts
 menu[midx(7)].text  &= scriptname_default(gmap(7), script_defaults(7))
 FOR i as integer = 12 TO 15
  menu[midx(i)].text &= scriptname_default(gmap(i), script_defaults(i))
 NEXT
 ' Autorun script argument
 IF trigger_or_default(gmap(7), gen(genDefMapAutorunScript)) = 0 THEN
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
   menu[midx(i)].text &= gmap(i) & " " & GetZoneInfo(st.map.zmap, gmap(i))->name
  END IF
 NEXT
 ' Default Pathfinding rules
 SELECT CASE gmap(378) 
  CASE 0: menu[midx(378)].text &= "Default (NPCs Obstruct)"
  CASE 1: menu[midx(378)].text &= "NPCs Obstruct"
  CASE 2: menu[midx(378)].text &= "Ignore NPCs"
 END SELECT
END SUB

SUB mapedit_gmapdata(st as MapEditState)
 DIM gdidx() as integer   'Index in gmap()
 DIM menu as SimpleMenuItem vector
 DIM menu_display as SimpleMenuItem vector
 DIM menuopts as MenuOptions
 menuopts.scrollbar = YES
 DIM BYREF map as MapData = st.map

 DIM script_defaults(7 TO 15) as integer  'Only covers gmap indices used by scripts
 script_defaults(7) =  gen(genDefMapAutorunScript)
 script_defaults(12) = gen(genDefAfterBattleScript)
 script_defaults(13) = gen(genDefInsteadOfBattleScript)
 script_defaults(14) = gen(genDefEachStepScript)
 script_defaults(15) = gen(genDefOnKeypressScript)

 'Maps gmap() index to menu() index
 DIM midx(dimbinsize(binMAP)) as integer

 mapedit_gmapdata_buildmenu st, menu, map.gmap(), gdidx(), midx(), script_defaults()

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
 gdmax(32) = zoneLASTUSER:        gdmin(32) = 0
 gdmax(33) = zoneLASTUSER:        gdmin(33) = 0
 gdmax(378) = 2:                  gdmin(378) = 0

 DIM selectst as SelectTypeState
 DIM state as MenuState
 state.pt = 0
 state.last = v_len(menu) - 1
 state.autosize = YES

 'Clamp data to bounds (only the gmap() indices edited in this menu)
 FOR i as integer = 0 TO UBOUND(gdidx)
  'Safety-bounding of gmap data, prevents crashes in cases of corruption
  DIM idx as integer = gdidx(i)
  IF idx >= 0 THEN
   IF NOT in_bound(map.gmap(idx), gdmin(idx), gdmax(idx)) THEN
    debugc errError, "Map " & map.id & "'s general data corrupt (or unsupported); clamping gmap(" & idx & ") = " & map.gmap(idx) & " -> 0"
    map.gmap(idx) = 0
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
    IF zintgrabber(map.gmap(idx), gdmin(idx) - 1, gdmax(idx) - 1) THEN 'song is optional
     music_stop
     state.need_update = YES
    END IF
    IF enter_space_click(state) THEN
     IF map.gmap(idx) > 0 THEN playsongnum map.gmap(idx) - 1
    END IF
   CASE 7, 12 TO 15 'scripts
    IF enter_space_click(state) THEN
     scriptbrowse(map.gmap(idx), plottrigger, "plotscript", YES, script_defaults(idx))
     state.need_update = YES
    ELSEIF scrintgrabber(map.gmap(idx), -1, 0, scLeft, scRight, 1, plottrigger) THEN
     state.need_update = YES
    END IF
   CASE 10 'Harm tile color
    state.need_update OR= intgrabber(map.gmap(idx), gdmin(idx), gdmax(idx))
    IF enter_space_click(state) THEN
     map.gmap(idx) = color_browser_256(map.gmap(idx))
    END IF
   CASE ELSE 'all other gmap data are simple integers
    state.need_update OR= intgrabber(map.gmap(idx), gdmin(idx), gdmax(idx))
  END SELECT

  IF state.need_update THEN
   mapedit_gmapdata_buildmenu st, menu, st.map.gmap(), gdidx(), midx(), script_defaults()
   state.need_update = NO
  END IF

  IF select_by_typing(selectst, NO) THEN
   select_on_word_boundary cast(BasicMenuItem vector, menu), selectst, state
  END IF

  '--Draw screen
  clearpage dpage
  highlight_menu_typing_selection cast(BasicMenuItem vector, menu), cast(BasicMenuItem vector, menu_display), selectst, state
  standardmenu cast(BasicMenuItem vector, menu_display), state, 0, 0, dpage, menuopts
  IF map.gmap(10) THEN
   'Harm tile flash color preview
   rectangle 4 + 8 * LEN(menu[midx(10)].text), 8 * midx(10), 8, 8, map.gmap(10), dpage
  END IF
  IF need_default_edge_tile(st.map) THEN
   'Show default edge tile
   writeblock sampmap, 0, 0, map.gmap(6)
   DIM tilepos as XYPair = (8 + 8 * LEN(menu[midx(6)].text), 8 * (midx(6) - state.top))
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


FUNCTION mapedit_pick_layer(st as MapEditState, message as string, other_option as string = "") as integer
 'Gives the user a prompt to select a map layer, returns layer number, or -2
 'if cancelled.
 'If other_option isn't empty it appears as an alternative option at the top, returning -1 if selected.
 DIM offset as integer = 0
 IF LEN(other_option) > 0 THEN offset = 1
 DIM options(UBOUND(st.map.tiles) + offset) as string
 IF LEN(other_option) > 0 THEN options(0) = other_option
 FOR layerno as integer = 0 TO UBOUND(st.map.tiles)
  options(offset + layerno) = "Layer " & layerno & " " & read_map_layer_name(st.map.gmap(), layerno)
 NEXT
 DIM ret as integer
 ret = multichoice(message, options(), 0, -2)
 IF ret >= 0 THEN ret -= offset
 RETURN ret
END FUNCTION

SUB mapedit_layers (st as MapEditState)
 DIM byref map as MapData = st.map

 DIM state as MenuState
 DIM menuopts as MenuOptions
 menuopts.edged = YES
 menuopts.itemspacing = -1
 menuopts.showright = YES
 menuopts.fullscreen_scrollbar = YES
 DIM menu as LayerMenuItem vector
 
 DIM layerno as integer
 DIM fakelayerno as integer  'the selected layer, treating NPCs/Heroes as a layer
 DIM resetpt as bool
 DIM col as integer
 DIM tileset as integer
 DIM layerpreview as Frame ptr

 state.top = 0
 state.size = 19
 state.autosize = YES
 state.autosize_ignore_pixels = 26

 mapedit_makelayermenu st, menu, state, YES, st.layer, layerpreview

 DO 
  setwait 55
  setkeys YES

  layerno = menu[state.pt].layernum
  fakelayerno = layerno
  IF fakelayerno >= map.gmap(31) THEN fakelayerno += 1
  'Warning: gen(31) (#layers below heroes/npcs) might be larger than the number of layers

  IF keyval(scESC) > 1 THEN clearkey(scESC): EXIT DO
  IF keyval(scF1) > 1 THEN show_help "mapedit_layers"
  IF (keyval(scPlus) > 1 OR keyval(scNumpadPlus) > 1) AND UBOUND(map.tiles) < maplayerMax THEN
   DIM layer_to_copy as integer
   layer_to_copy = mapedit_pick_layer(st, "Copy an existing layer?", "No; new blank layer")
   IF layer_to_copy >= -1 THEN
    'Didn't cancel
    IF layerno = -1 THEN
     'No layer selected; just add new one to end
     mapedit_append_new_layers st, 1
     layerno = UBOUND(map.tiles)
     resetpt = NO
    ELSE
     'Insert after.
     'When gmap(31) is greater than actual number of layers we are "filling up" to old default of 2 under
     IF layerno < map.gmap(31) AND UBOUND(map.tiles) + 1 >= map.gmap(31) THEN map.gmap(31) += 1
     IF layer_to_copy > layerno THEN layer_to_copy += 1
     mapedit_insert_new_layer st, layerno + 1
     layerno += 1
     resetpt = YES
    END IF
    IF layer_to_copy >= 0 THEN mapedit_copy_layer(st, layer_to_copy, layerno)
    state.need_update = YES
   END IF
  END IF
  IF (keyval(scDelete) > 1 OR keyval(scMinus) > 1 OR keyval(scNumpadMinus) > 1) ANDALSO UBOUND(map.tiles) > 0 ANDALSO layerno >= 0 THEN
   DIM layername as string
   layername = read_map_layer_name(map.gmap(), layerno)
   IF LEN(layername) THEN layername = " " & layername
   IF yesno("Really delete layer " & layerno & layername & "?", NO) THEN
    IF layerno < map.gmap(31) THEN map.gmap(31) = large(map.gmap(31) - 1, 1)
    mapedit_delete_layer st, layerno
    st.layer = small(st.layer, UBOUND(map.tiles))
    layerno = small(layerno, UBOUND(map.tiles))
    resetpt = YES
    state.need_update = YES
   END IF
  END IF
  IF keyval(scShift) > 0 THEN
   'Moving layers up or down: state.pt needs to be updated afterwards, which happens
   'in mapedit_makelayermenu with resetpt = YES

   IF keyval(scUp) > 1 AND fakelayerno > 0 THEN
    IF fakelayerno = map.gmap(31) + 1 THEN
     'swapping with NPC/Hero layers
     map.gmap(31) += 1
    ELSE
     mapedit_swap_layers st, layerno, layerno - 1
     layerno -= 1
    END IF
    resetpt = YES
    state.need_update = YES
   END IF
   'UBOUND(map.tiles) or UBOUND(map.tiles) + 1 is the maximum fakelayerno (can't adjust overhead tiles either)
   IF keyval(scDown) > 1 THEN
    IF layerno = 0 AND UBOUND(map.tiles) > 0 THEN
     'can't move npcs/heroes below layer 0, so swap with 2nd layer instead
     mapedit_swap_layers st, layerno, layerno + 1
     layerno += 1     
    ELSEIF layerno > 0 THEN
     IF layerno = small(map.gmap(31) - 1, UBOUND(map.tiles)) THEN  'gmap(31) may be larger
      'swapping with NPC/Hero layers
      map.gmap(31) = layerno
     ELSEIF layerno < UBOUND(map.tiles) THEN
      mapedit_swap_layers st, layerno, layerno + 1
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
     IF intgrabber(map.gmap(0), 0, gen(genMaxTile)) THEN
      loadmaptilesets st.tilesets(), map.gmap()
      state.need_update = YES
     END IF
    CASE ltLayerName
     DIM tempname as string
     tempname = read_map_layer_name(map.gmap(), layerno)
     IF strgrabber(tempname, 40) THEN
      state.need_update = YES
     END IF
     write_map_layer_name(map.gmap(), layerno, tempname)
    CASE ltLayerTileset
     clearkey(scPlus)
     clearkey(scNumpadPlus)
     clearkey(scMinus)
     clearkey(scNumpadMinus)
     IF zintgrabber(map.gmap(menu[state.pt].gmapindex), -1, gen(genMaxTile)) THEN
      tileset = map.gmap(menu[state.pt].gmapindex) - 1
      IF tileset = -1 THEN tileset = map.gmap(0)
      loadtilesetdata st.tilesets(), layerno, tileset
      state.need_update = YES
     END IF
    CASE ltLayerEnabled
     IF enter_space_click(state) THEN
      ToggleLayerEnabled(map.gmap(), layerno)
      state.need_update = YES
     END IF
     IF layerisenabled(map.gmap(), layerno) AND (keyval(scLeft) > 1 OR keyval(scRight) > 1) THEN
      ToggleLayerVisible(st.visible(), layerno)
      state.need_update = YES
     END IF
   END SELECT
  END IF

  IF state.need_update THEN
   state.need_update = NO
   mapedit_makelayermenu st, menu, state, resetpt, layerno, layerpreview
   resetpt = NO
  END IF

  clearpage dpage
  IF layerpreview THEN
   'Shift over 6 pixels to avoid the scrollbar if any
   frame_draw layerpreview, , pRight + showLeft - 6, pTop, , NO, dpage
  END IF
  standardmenu cast(BasicMenuItem vector, menu), state, 0, 0, dpage, menuopts

  DIM liney as integer = vpages(dpage)->h - 10
  edgeprint "SHIFT+arrows to move layers, - to delete", 0, liney, uilook(uiText), dpage
  liney -= 10
  IF UBOUND(map.tiles) < maplayerMax THEN
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
 mapedit_load_tilesets st  'Reload default passability
 IF layerisenabled(map.gmap(), st.layer) = 0 THEN st.layer = 0
 v_free menu
 frame_unload @layerpreview

END SUB

'Create all the menu items for a single layer
SUB mapedit_makelayermenu_layer(st as MapEditState, byref menu as LayerMenuItem vector, byref slot as integer, byval layer as integer, byref needdefault as integer)

 menu[slot].role = ltLayerName
 'menu[slot].unselectable = YES
 menu[slot].text = "Tile layer " & layer & " " & read_map_layer_name(st.map.gmap(), layer)
 menu[slot].layernum = layer
 slot += 1

 IF layerisenabled(st.map.gmap(), layer) THEN
  IF layerisvisible(st.visible(), layer) THEN
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

 IF st.map.gmap(layerindex) = 0 THEN
  menu[slot].text = " Tileset: Default"
  needdefault = YES
 ELSE
  menu[slot].text = " Tileset: " & st.map.gmap(layerindex) - 1
 END IF
 menu[slot].role = ltLayerTileset
 menu[slot].layernum = layer
 menu[slot].gmapindex = layerindex
 slot += 1
END SUB

SUB mapedit_makelayermenu(st as MapEditState, byref menu as LayerMenuItem vector, state as MenuState, byval resetpt as bool, byval selectedlayer as integer = 0, byref layerpreview as Frame ptr)
 DIM remember_selection_type as LayerMenuItemType
 IF menu THEN
  remember_selection_type = menu[state.pt].role
 ELSE
  'On building the menu for the first time
  remember_selection_type = ltLayerName
 END IF

 v_free menu
 'Yuck, FIXME: append menu items normally instead
 v_new menu, 1 + 3 * (UBOUND(st.map.tiles) + 1) + 2 + IIF(st.map.gmap(16) = 2, 1, 2)
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
 FOR i as integer = 0 TO small(UBOUND(st.map.tiles), st.map.gmap(31) - 1)
  mapedit_makelayermenu_layer st, menu, slot, i, needdefault
 NEXT

 IF st.map.gmap(16) = 2 THEN '--keep heroes and NPCs together
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
  IF st.map.gmap(16) = 0 THEN
   menu[slot - 2].text = "NPCs layer"
   menu[slot - 1].text = "Heroes layer"
  ELSE
   menu[slot - 2].text = "Heroes layer"
   menu[slot - 1].text = "NPCs layer"
  END IF
 END IF

 FOR i as integer = st.map.gmap(31) TO UBOUND(st.map.tiles)
  mapedit_makelayermenu_layer st, menu, slot, i, needdefault
 NEXT

 menu[slot].unselectable = YES
 menu[slot].col = uilook(uiText)
 menu[slot].text = "Tile layer 0 overhead tiles (obsolete)"
 slot += 1
 
 IF needdefault THEN
  menu[1].text += STR(st.map.gmap(0))
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

 'Load the preview of the map layer or its tileset
 frame_unload @layerpreview
 DIM layerno as integer = menu[state.pt].layernum
 IF layerno > -1 AND menu[state.pt].gmapindex = -1 THEN
  'Layer menu item other than tileset selection. Preview map minimap
  layerpreview = createminimap(st.map.tiles(layerno), st.tilesets(layerno))

 ELSE
  'Either preview tileset, or blank background

  DIM wanttileset as integer = -1
  IF state.pt = 1 THEN
   wanttileset = st.map.gmap(0)
  ELSEIF menu[state.pt].gmapindex > -1 THEN
   wanttileset = st.map.gmap(menu[state.pt].gmapindex) - 1
   IF wanttileset = -1 THEN wanttileset = st.map.gmap(0)
  END IF

  IF wanttileset > -1 THEN
   layerpreview = frame_load_mxs(game + ".til", wanttileset)
  END IF
 END IF
 IF layerpreview THEN fuzzyrect layerpreview, 0, 0, , , uilook(uiBackground)
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

 DIM how as integer
 how = generic_add_new("map", gen(genMaxMap), @getmapname, "add_map_how")
 '-- -2  =Cancel
 '-- -1  =New blank
 '-- >=0 =Copy
 IF how = -1 THEN
  gen(genMaxMap) += 1
  new_blank_map st
  st.map.id = gen(genMaxMap)
  mapedit_savemap st
 ELSEIF how >= 0 THEN
  gen(genMaxMap) += 1
  mapedit_loadmap st, how
  st.map.id = gen(genMaxMap)
  mapedit_savemap st
 END IF
END SUB

SUB new_blank_map (st as MapEditState)
 st.map.name = ""
 st.map.wide = 64
 st.map.high = 64
 '--flush map buffers
 cleantilemaps st.map.tiles(), 64, 64, 1
 cleantilemap st.map.pass, 64, 64
 cleantilemap st.map.foemap, 64, 64
 CleanZoneMap st.map.zmap, 64, 64
 flusharray st.map.gmap(), -1, 0
 st.map.gmap(16) = 2 'Walkabout Layering: Together
 st.map.gmap(31) = 1 'Walkabout layer above map layer 0
 CleanNPCL st.map.npc()
 CleanNPCD st.map.npc_def()
 REDIM PRESERVE st.map.npc_def(0)
 cleandoors st.map.door()
 cleandoorlinks st.map.doorlink()
 'Just in case
 mapedit_load_tilesets st
END SUB

SUB mapedit_loadmap (st as MapEditState, mapnum as integer)
 st.map.load(mapnum)
 flusharray st.visible(), , -1  'Mark all layers visible (when they're enabled)
 mapedit_load_tilesets st
 verify_map_size st

 'Initialise default zone names
 DIM zinfo as ZoneInfo ptr = GetZoneInfo(st.map.zmap, zoneOneWayExit)
 IF LEN(zinfo->name) = 0 THEN zinfo->name = "One-Way walls (exit only)"
END SUB

SUB mapedit_savemap (st as MapEditState)
 storerecord st.map.gmap(), game & ".map", getbinsize(binMAP) / 2, st.map.id
 savetilemaps st.map.tiles(), maplumpname(st.map.id, "t")
 savetilemap st.map.pass, maplumpname(st.map.id, "p")
 savetilemap st.map.foemap, maplumpname(st.map.id, "e")
 SaveZoneMap st.map.zmap, maplumpname(st.map.id, "z")
 SaveNPCL maplumpname(st.map.id, "l"), st.map.npc()
 SaveNPCD maplumpname(st.map.id, "n"), st.map.npc_def()
 serdoors game & ".dox", st.map.door(), st.map.id
 serdoorlinks maplumpname(st.map.id, "d"), st.map.doorlink()
 '--save map name
 DIM mapsave(39) as integer
 mapsave(0) = LEN(st.map.name)
 str2array LEFT(st.map.name, 39), mapsave(), 1
 storerecord mapsave(), game & ".mn", 40, st.map.id
END SUB

SUB verify_map_size (st as MapEditState)
 WITH st.map
  DIM as integer w = .tiles(0).wide, h = .tiles(0).high
  IF .pass.wide = w AND .foemap.wide = w AND .zmap.wide = w AND _
     .pass.high = h AND .foemap.high = h AND .zmap.high = h THEN
   EXIT SUB
  END IF
  '--Map's X and Y do not match
  .wide = w
  .high = h
  clearpage vpage
  DIM j as integer
  j = 0
  textcolor uilook(uiText), 0
  printstr "Map " & .id & ":" & .name, 0, j * 8, vpage
  j += 2
  printstr "this map seems to be corrupted", 0, j * 8, vpage
  j += 2
  printstr " TileMap " & .tiles(0).wide & "*" & .tiles(0).high & " tiles, " & (UBOUND(.tiles) + 1) & " layers", 0, j * 8, vpage: j += 1
  printstr " WallMap " & .pass.wide & "*" & .pass.high & " tiles", 0, j * 8, vpage: j += 1
  printstr " FoeMap " & .foemap.wide & "*" & .foemap.high & " tiles", 0, j * 8, vpage: j += 1
  printstr " ZoneMap " & .zmap.wide & "*" & .zmap.high & " tiles", 0, j * 8, vpage: j += 1
  j += 1
  printstr "Fixing to " & .wide & "*" & .high, 0, j * 8, vpage: j += 1
  'A map's size might be due to corruption, besides, the tilemap is far away the most important
  '.wide = large(.tiles(0).wide, large(.pass.wide, .foemap.wide))
  '.high = large(.tiles(0).high, large(.pass.high, .foemap.high))
  .pass.wide = .wide
  .pass.high = .high
  .pass.data = REALLOCATE(.pass.data, .wide * .high)
  .foemap.wide = .wide
  .foemap.high = .high
  .foemap.data = REALLOCATE(.foemap.data, .wide * .high)
  IF .zmap.wide <> .wide OR .zmap.high <> .high THEN
   'Zone maps are too tricky, just delete
   CleanZoneMap .zmap, .wide, .high
  END IF
  'savetilemaps .tiles, maplumpname(.id, "t")
  savetilemap .pass, maplumpname(.id, "p")
  savetilemap .foemap, maplumpname(.id, "e")
  SaveZoneMap .zmap, maplumpname(.id, "z")
  'loadtilemaps .tiles, maplumpname(.id, "t")
  'loadtilemap .pass, maplumpname(.id, "p")
  'loadtilemap .foemap, maplumpname(.id, "e")
  j += 1
  printstr "please report this error to", 0, j * 8, vpage: j += 1
  printstr "ohrrpgce@HamsterRepublic.com", 0, j * 8, vpage: j += 1
  setvispage vpage
  waitforanykey
 END WITH
END SUB

SUB mapedit_load_tilesets(st as MapEditState)
 loadmaptilesets st.tilesets(), st.map.gmap()
 v_new st.defaultwalls, UBOUND(st.map.tiles) + 1
 FOR i as integer = 0 TO UBOUND(st.map.tiles)
  loadpasdefaults st.defaultwalls[i], st.tilesets(i)->num
 NEXT
END SUB


'==========================================================================================
'                             Layer manipulation/rearrangement
'==========================================================================================


SUB fix_tilemaps(map as MapData)
 'Each tilemap in map.tiles() needs to know its index number in map.tiles(). This SUB updates that.
 FOR i as integer = 0 TO UBOUND(map.tiles)
  map.tiles(i).layernum = i
 NEXT
END SUB

SUB mapedit_swap_layers(st as MapEditState, byval l1 as integer, byval l2 as integer)
 WITH st
  DIM as bool temp1, temp2
  SWAP .map.tiles(l1), .map.tiles(l2)
  fix_tilemaps .map
  SWAP .usetile(l1), .usetile(l2)
  SWAP .menubarstart(l1), .menubarstart(l2)
  SWAP .map.gmap(layer_tileset_index(l1)), .map.gmap(layer_tileset_index(l2))  'Tileset
  DIM as string name1, name2
  name1 = read_map_layer_name(.map.gmap(), l1)
  name2 = read_map_layer_name(.map.gmap(), l2)
  write_map_layer_name(.map.gmap(), l1, name2)
  write_map_layer_name(.map.gmap(), l2, name1)
  SWAP .tilesets(l1), .tilesets(l2)
  temp1 = layerisenabled(.map.gmap(), l1)
  temp2 = layerisenabled(.map.gmap(), l2)
  setlayerenabled(.map.gmap(), l2, temp1)
  setlayerenabled(.map.gmap(), l1, temp2)
  temp1 = layerisvisible(.visible(), l1)
  temp2 = layerisvisible(.visible(), l2)
  setlayervisible(.visible(), l2, temp1)
  setlayervisible(.visible(), l1, temp2)
  IF .layer = l1 THEN
   .layer = l2
  ELSEIF .layer = l2 THEN
   .layer = l1
  END IF
  mapedit_throw_away_history st
  v_free .cloned
 END WITH
END SUB

SUB mapedit_copy_layer(st as MapEditState, byval src as integer, byval dest as integer)
 'Copy src layer over dest layer
 'Note: we don't throw away history because it's unnecessary if copy to a new layer
 'at the end; if this is ever used for overwriting an actual existing layer, the caller should
 'call throw away history and the clone buffer.
 WITH st.map
  .gmap(layer_tileset_index(dest)) = .gmap(layer_tileset_index(src))  'Tileset
  write_map_layer_name(.gmap(), dest, "copy of " & src & " " & read_map_layer_name(.gmap(), src))
  CopyTilemap .tiles(dest), .tiles(src)
 END WITH
END SUB

'Add blank map layers
SUB mapedit_append_new_layers(st as MapEditState, howmany as integer)
 DIM old_maxlayer as integer = UBOUND(st.map.tiles)
 howmany = small(howmany, maplayerMax - UBOUND(st.map.tiles))
 REDIM PRESERVE st.map.tiles(UBOUND(st.map.tiles) + howmany)
 FOR i as integer = old_maxlayer + 1 to UBOUND(st.map.tiles)
  CleanTilemap st.map.tiles(i), st.map.wide, st.map.high, i
  SetLayerEnabled(st.map.gmap(), i, YES)
  SetLayerVisible(st.visible(), i, YES)
  st.map.gmap(layer_tileset_index(i)) = 0  'Tileset = default
  write_map_layer_name(st.map.gmap(), i, "")
 NEXT
 fix_tilemaps st.map
 mapedit_load_tilesets st
END SUB

'where is the index the new layer is to be placed at
'If the new layer is added to the end, then undo history and the clone buffer are preserved
SUB mapedit_insert_new_layer(st as MapEditState, where as integer)
 IF UBOUND(st.map.tiles) = maplayerMax THEN EXIT SUB

 'Add to end, then shuffle to correct spot
 mapedit_append_new_layers st, 1
 FOR i as integer = UBOUND(st.map.tiles) - 1 TO where STEP -1
  mapedit_swap_layers st, i, i + 1
 NEXT
 fix_tilemaps st.map
 mapedit_load_tilesets st
END SUB

SUB mapedit_delete_layer(st as MapEditState, byval which as integer)
 'doesn't reload tilesets or passability defaults, layers menu does that
 WITH st.map
  FOR i as integer = which TO UBOUND(.tiles) - 1
   mapedit_swap_layers st, i, i + 1
  NEXT
  UnloadTilemap .tiles(UBOUND(.tiles))
  'currently (temporarily) tilesets for unused layers are still loaded, so reset to default
  .gmap(layer_tileset_index(UBOUND(.tiles))) = 0
  write_map_layer_name(.gmap(), UBOUND(.tiles), "")
  REDIM PRESERVE .tiles(UBOUND(.tiles) - 1)
 END WITH
 fix_tilemaps st.map
 mapedit_throw_away_history st
 v_free st.cloned
END SUB


'==========================================================================================
'                                  Resizing & deleting
'==========================================================================================


SUB mapedit_resize(st as MapEditState)
'sizemap:
 DIM rs as MapResizeState
 rs.rect.wide = 0
 rs.rect.high = 0
 rs.rect.x = 0
 rs.rect.y = 0
 'resizemapmenu both inits and deletes rs.menu
 resizemapmenu st, rs
 IF rs.rect.wide = -1 THEN EXIT SUB

 clearpage 0
 clearpage 1
 
 DIM yout as integer = 0
 edgeprint "TILEMAP", 0, yout * 10, uilook(uiText), vpage: setvispage vpage: yout += 1
 resizetiledata st.map.tiles(), rs, yout, vpage
 edgeprint "PASSMAP", 0, yout * 10, uilook(uiText), vpage: setvispage vpage: yout += 1
 resizetiledata st.map.pass, rs, yout, vpage
 edgeprint "FOEMAP", 0, yout * 10, uilook(uiText), vpage: setvispage vpage: yout += 1
 resizetiledata st.map.foemap, rs, yout, vpage
 edgeprint "ZONEMAP", 0, yout * 10, uilook(uiText), vpage: setvispage vpage: yout += 1
 SaveZoneMap st.map.zmap, tmpdir & "zresize.tmp", @rs.rect
 LoadZoneMap st.map.zmap, tmpdir & "zresize.tmp"
 edgeprint "Deleting Undo History", 0, yout * 10, uilook(uiText), vpage: setvispage vpage: yout += 1
 mapedit_throw_away_history st
 ' update SAV x/y offset in MAP lump
 st.map.gmap(20) += rs.rect.x * -1
 st.map.gmap(21) += rs.rect.y * -1
 ' update hero's starting position (if on current map)
 IF gen(genStartMap) = st.map.id THEN
  gen(genStartX) += rs.rect.x * -1
  gen(genStartY) += rs.rect.y * -1 
 END IF
 st.map.wide = rs.rect.wide
 st.map.high = rs.rect.high
 '--reset map scroll position
 st.x = 0
 st.y = 0
 st.mapx = 0
 st.mapy = 0
 edgeprint "Aligning and truncating doors", 0, yout * 10, uilook(uiText), vpage: yout += 1
 FOR i as integer = 0 TO UBOUND(st.map.door)
  WITH st.map.door(i)
   .x -= rs.rect.x
   .y -= rs.rect.y
   IF .x < 0 OR .y < 0 OR .x >= st.map.wide OR .y >= st.map.high THEN
    set_door_exists(st.map.door(), i, NO)
   END IF
  END WITH
 NEXT
 edgeprint "Aligning and truncating NPCs", 0, yout * 10, uilook(uiText), vpage: setvispage vpage: yout += 1
 FOR i as integer = 0 TO UBOUND(st.map.npc)
  WITH st.map.npc(i)
   .x -= rs.rect.x * 20
   .y -= rs.rect.y * 20
   IF .x < 0 OR .y < 0 OR .x >= st.map.wide * 20 OR .y >= st.map.high * 20 THEN
    .id = 0
   END IF
  END WITH
 NEXT i
 verify_map_size st
END SUB

SUB mapedit_delete(st as MapEditState)
 REDIM options(6) as string
 options(0) = "Cancel!"
 options(1) = "Erase all map data"
 options(2) = "Erase tile data + doors + NPC instances"
 options(3) = "Erase NPC instances"
 options(4) = "Erase NPC instances + definitions"
 options(5) = "Erase doors"
 options(6) = "Erase doorlinks"
 IF st.map.id = gen(genMaxMap) AND st.map.id >= 1 THEN
  '--if this is the last map, then we can actually remove it entirely, rather than just blanking it
  str_array_append options(), "Delete map entirely"
 END IF
 DIM choice as integer = multichoice("Delete which map data?", options(), 0, 0, "mapedit_delete")
 IF choice >= 1 AND choice <= 6 THEN
  IF choice = 1 THEN  '--everything
   new_blank_map st
   load_npc_graphics st.map.npc_def(), st.npc_img()
   mapedit_throw_away_history st
  ELSEIF choice = 2 THEN  '--just tile related data
   CleanTilemaps st.map.tiles(), st.map.wide, st.map.high, 1
   CleanTilemap st.map.pass, st.map.wide, st.map.high
   CleanTilemap st.map.foemap, st.map.wide, st.map.high
   CleanZoneMap st.map.zmap, st.map.wide, st.map.high
   CleanNPCL st.map.npc()
   CleanDoors st.map.door()
   st.map.gmap(31) = 1 'Walkabout layer above map layer 0
   mapedit_throw_away_history st
  ELSEIF choice = 3 THEN
   CleanNPCL st.map.npc()
  ELSEIF choice = 4 THEN
   CleanNPCL st.map.npc()
   CleanNPCD st.map.npc_def()
   REDIM PRESERVE st.map.npc_def(0)
   load_npc_graphics st.map.npc_def(), st.npc_img()
  ELSEIF choice = 5 THEN
   CleanDoors st.map.door()
  ELSEIF choice = 6 THEN
   CleanDoorlinks st.map.doorlink()
  END IF

  '--reset scroll position
  st.x = 0
  st.y = 0
  st.mapx = 0
  st.mapy = 0
  st.layer = 0

  mapedit_savemap st
 END IF

 IF choice = 7 THEN
  gen(genMaxMap) -= 1
  safekill maplumpname(st.map.id, "t")
  safekill maplumpname(st.map.id, "p")
  safekill maplumpname(st.map.id, "e")
  safekill maplumpname(st.map.id, "l")
  safekill maplumpname(st.map.id, "n")
  safekill maplumpname(st.map.id, "d")
  safekill maplumpname(st.map.id, "z")
  'Note .MAP and .MN are not truncated
  'Afterwards, the map editor exits
 END IF
END SUB

'==========================================================================================

'Update the scroll position of the tilepicker in the top bar
SUB update_tilepicker(st as MapEditState)
 'Based on correct_menu_state.
 DIM byref top as integer = st.menubarstart(st.layer)
 DIM byref pt as integer = st.usetile(st.layer)
 'Menu width, as number of visible tiles (right 60 pixels is blacked out)
 DIM size as integer = (vpages(dpage)->w - 60) \ tilew - 1

 'Selected item must be visible, with a margin of 2 tiles to left and right
 '(this indirectly allows scrolling by clicking at the edges)
 top = large(small(top, pt - 2), pt - size + 2)
 top = large(small(top, (st.menubar.wide - 1) - size), 0)
END SUB

'Set the selected tile in the tileset. Not an error to try to set to something out-of-range.
SUB set_usetile(st as MapEditState, tile as integer)
 IF tile < 0 ORELSE tile >= st.menubar.wide THEN EXIT SUB
 IF st.usetile(st.layer) = tile THEN EXIT SUB
 st.usetile(st.layer) = tile
 update_tilepicker st

 IF st.tool = clone_tool AND st.multitile_draw_brush THEN
  'Clone tool behaves like Draw: upon changing the drawing tile (including when changing layer), reset
  st.tool = draw_tool
  v_free st.cloned
 END IF
END SUB

'Safely set the selected layer
SUB set_layer(st as MapEditState, layer as integer)
 IF layer < 0 OR layer > UBOUND(st.map.tiles) THEN EXIT SUB
 st.layer = layer
 update_tilepicker st
 mapedit_update_layer_palettes st
END SUB


'==========================================================================================
'                                      Import/Export
'==========================================================================================


SUB mapedit_export_tilemaps(st as MapEditState)
 DIM outfile as string
 outfile = inputfilename("Export tilemap to which file?", ".tilemap", "", "input_file_export_tilemap")
 IF LEN(outfile) THEN
  copyfile maplumpname(st.map.id, "t"), outfile + ".tilemap"
 END IF
END SUB

'Copy each imported tilemap to a new tilemap, assuming they are not necessarily
'the same size.
'If appending is false, then deletes existing layers, but doesn't change map size.
SUB mapedit_append_imported_tilemaps(st as MapEditState, newlayers() as TileMap, appending as bool)
 DIM dest_layer as integer
 IF appending = NO THEN
  'Preserve the map size here; the resize menu is needed to change that.
  CleanTilemaps st.map.tiles(), st.map.wide, st.map.high, UBOUND(newlayers) + 1
  dest_layer = 0
 ELSE
  mapedit_append_new_layers st, 1
  dest_layer = UBOUND(st.map.tiles)
 END IF 
 DIM as integer src_layer, x, y
 FOR src_layer = 0 TO UBOUND(newlayers)
  FOR x = 0 TO small(st.map.wide, newlayers(src_layer).wide) - 1
   FOR y = 0 TO small(st.map.high, newlayers(src_layer).high) - 1
    writeblock(st.map.tiles(dest_layer), x, y, readblock(newlayers(src_layer), x, y))
   NEXT
  NEXT
  dest_layer += 1
 NEXT
 mapedit_load_tilesets st
END SUB

'Asks the user to browse for a .tilemap file, then handles the import
'appending: true if appending, false if replacing all existing
SUB mapedit_import_tilemaps(st as MapEditState, appending as bool)
 DIM infile as string
 infile = browse(12, "", "*.tilemap", "browse_tilemaps")
 IF LEN(infile) = 0 THEN EXIT SUB

 REDIM newlayers(0) as TileMap
 IF LoadTilemaps(newlayers(), infile, YES) = NO THEN
  pop_warning "Bad file; could not load " & decode_filename(infile)
  EXIT SUB
 END IF

 '--- First we check whether there are too many map layers

 DIM menu_caption as string
 DIM choice as integer
 DIM num_old_layers as integer = UBOUND(st.map.tiles) + 1
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
 IF st.map.wide <> newwide OR st.map.high <> newhigh THEN
  REDIM menu_choices(2) as string

  menu_caption = "This tilemap is " & newwide & "*" & newhigh & " with " & (UBOUND(newlayers) + 1) & " layers, while the map is " & st.map.wide & "*" & st.map.high & " (with " & (UBOUND(newlayers) + 1) & " layers)."
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
   mapedit_resize st
   IF st.map.wide <> newwide OR st.map.high <> newhigh THEN  'Either cancelled or wrong size
    notification "Map still the wrong size, cancelling import"
    UnloadTilemaps newlayers()
    EXIT SUB
   END IF
  ELSEIF choice = 2 THEN
   'Crop/extend. This is handled automatically.
  END IF
 END IF

 mapedit_append_imported_tilemaps st, newlayers(), appending
 notification "Imported " & num_new_layers & " layers"

 UnloadTilemaps newlayers()
END SUB

'Overwrite a map layer, reading a tile per pixel of a .bmp
SUB bmp_to_layer(st as MapEditState, imgfile as string, layer as integer)
 DIM fr as Frame ptr = frame_import_bmp_raw(imgfile)
 IF fr = NULL THEN
  notification "Couldn't import map layer: loading .bmp failed"
  EXIT SUB
 END IF
 IF fr->h <> st.map.high OR fr->w <> st.map.wide THEN
  notification "This image is " & fr->w & "*" & fr->h & " while the map is " & st.map.wide & "*" & st.map.high & !".\n" _
               "Any extra portion will be clipped. Resize the map manually as desired."
 END IF
 FOR y as integer = 0 TO small(fr->h, st.map.high) - 1
  FOR x as integer = 0 TO small(fr->w, st.map.wide) - 1
   writeblock(st.map.tiles(layer), x, y, readpixel(fr, x, y))
  NEXT
 NEXT
 frame_unload @fr
END SUB

'Import a map layer from a paletted .bmp, one tile per pixel
SUB mapedit_import_bmp_tilemap(st as MapEditState)
 pop_warning "Please select an 8-bit paletted .bmp file to import. Each pixel of the image will be mapped to a tile. " _
             !"The n-th colour in the palette becomes tile index n. The actual colours in the palette are ignored.\n" _
             !"This is useful mainly for defining the rough outline of your map.\n" _
             "You may find it easiest to draw the image without considering tile indices, then " _
             "remap them to the correct tiles with the 'Replace' tool."
 DIM imgfile as string
 'Want any bmp with bitdepth at most 8
 imgfile = browse(10, "", "*.bmp", "browse_bmp_tilemap")
 DIM bmpd as BitmapV3InfoHeader
 IF LEN(imgfile) = 0 OR bmpinfo(imgfile, bmpd) <> 2 THEN EXIT SUB

 DIM blank_option as string = "New blank layer"
 IF UBOUND(st.map.tiles) >= maplayerMax THEN blank_option = ""   'remove option
 DIM layer as integer
 layer = mapedit_pick_layer(st, "Import as new layer or OVERWRITE existing layer?", blank_option)
 IF layer = -2 THEN EXIT SUB  'cancelled
 IF layer = -1 THEN  'new layer
  mapedit_append_new_layers st, 1
  layer = UBOUND(st.map.tiles)
 END IF

 bmp_to_layer st, imgfile, layer
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
    ' Perform gamma correction
    r += master(idx).r ^ 2.2
    g += master(idx).g ^ 2.2
    b += master(idx).b ^ 2.2
   NEXT
  NEXT
  colors(tile).b = small(255., (b / 400) ^ (1 / 2.2))
  colors(tile).g = small(255., (g / 400) ^ (1 / 2.2))
  colors(tile).r = small(255., (r / 400) ^ (1 / 2.2))
  'debug "tile " & tile & " g " & g & "->" & colors(tile).g
 NEXT
 'Then handle animated tiles by making them the same colour as the first tile in the animation
 FOR tile as integer = 160 TO 255
  DIM basetile as integer
  basetile = tile_anim_deanimate_tile(tile, tileset->tastuf())
  colors(tile) = colors(basetile)
 NEXT
END SUB

SUB layer_to_bmp(st as MapEditState, imgfile as string, layer as integer)
 DIM colors(255) as RGBcolor
 color_for_each_tile st.tilesets(layer), colors()
 DIM fr as Frame ptr
 fr = frame_new(st.map.wide, st.map.high, , YES)
 FOR y as integer = 0 TO st.map.high - 1
  FOR x as integer = 0 TO st.map.wide - 1
   putpixel fr, x, y, readblock(st.map.tiles(layer), x, y)
  NEXT
 NEXT
 frame_export_bmp8 imgfile, fr, colors()
 frame_unload @fr
END SUB

SUB mapedit_export_bmp_tilemap(st as MapEditState)
 DIM menu_caption as string
 menu_caption = "Please select a layer to export to an 8-bit paletted .bmp file. " _
                "Each pixel of the image will represent one tile: " _
                !"color n in the palette means tileset index n.\n" _
                !"This is useful mainly for modifying the large-scale structures of your map."
 REDIM menu_choices(0) as string
 menu_choices(0) = "Cancel"
 FOR layer as integer = 0 TO UBOUND(st.map.tiles)
  str_array_append menu_choices(), "Export map layer " & layer & " " & read_map_layer_name(st.map.gmap(), layer)
 NEXT
 DIM choice as integer
 choice = multichoice(menu_caption, menu_choices(), 1, 0)
 IF choice = 0 THEN EXIT SUB
 DIM outfile as string
 DIM defaultname as string
 defaultname = trimextension(trimpath(sourcerpg)) & " map " & st.map.id & " layer " & (choice - 1) & " " & read_map_layer_name(st.map.gmap(), choice - 1)
 defaultname = TRIM(defaultname)  'If no layer name
 outfile = inputfilename("Export tilemap to which file?", ".bmp", "", "input_file_export_bmp_tilemap", defaultname)
 IF LEN(outfile) THEN layer_to_bmp st, outfile + ".bmp", choice - 1
END SUB

'Simplified, used only when exporting a map image
SUB draw_all_npcs(st as MapEditState, dest as Frame ptr, including_conditional as bool)
 FOR i as integer = 0 TO UBOUND(st.map.npc)
  WITH st.map.npc(i)
   IF .id <= 0 THEN CONTINUE FOR
   WITH st.map.npc_def(.id - 1)
    IF including_conditional = NO ANDALSO (.tag1 ORELSE .tag2) THEN CONTINUE FOR
   END WITH
   DIM image as GraphicPair = st.npc_img(.id - 1)
   frame_draw image.sprite + (2 * .dir) + st.walk \ 2, image.pal, .x, .y + st.map.gmap(11), , , dest
  END WITH
 NEXT
END SUB

'Export the map at full resolution
SUB mapedit_export_map_image(st as MapEditState)
 DIM filename as string
 filename = inputfilename("Filename to save as?", ".bmp", "", "", _
                          trimextension(trimpath(sourcerpg)) & " map " & st.map.id & " " & st.map.name)
 IF LEN(filename) = 0 THEN EXIT SUB
 filename += ".bmp"
 DIM menu(2) as string = {"Only NPCs without tag conditions", "All", "None"}
 DIM npc_choice as integer = multichoice("Draw NPCs?", menu())
 IF npc_choice = -1 THEN EXIT SUB

 DIM dest as Frame ptr
 dest = frame_new(st.map.wide * 20, st.map.high * 20)

 'Need to make sure this has been called at least once
 animatetilesets st.tilesets()

 FOR layer as integer = 0 TO UBOUND(st.map.tiles)
  IF LayerIsEnabled(st.map.gmap(), layer) THEN
   setanim st.tilesets(layer)
   drawmap st.map.tiles(layer), 0, 0, st.tilesets(layer)->spr, dest, _
           layer > 0, IIF(layer > 0, 0, 1), @st.map.pass
  END IF
  'Draw NPCs?
  IF layer = small(st.map.gmap(31) - 1, UBOUND(st.map.tiles)) THEN
   IF npc_choice < 2 THEN draw_all_npcs st, dest, (npc_choice = 1)
  END IF
 NEXT
 IF LayerIsEnabled(st.map.gmap(), 0) THEN
   setanim st.tilesets(0)
   drawmap st.map.tiles(0), 0, 0, st.tilesets(0)->spr, dest, _
           NO, 2, @st.map.pass
 END IF

 frame_export_bmp8 filename, dest, master()
 frame_unload @dest
 show_overlay_message "Saved as " & trimpath(filename)
END SUB

SUB mapedit_import_export(st as MapEditState)
 DIM menu(6) as string
 menu(0) = "Previous menu"
 menu(1) = "Export tilemap"
 menu(2) = "Import tilemap, overwriting existing"
 menu(3) = "Import tilemap, as new layers"
 menu(4) = "Export map layer as pixel-a-tile BMP"
 menu(5) = "Import pixel-a-tile BMP as map layer"
 menu(6) = "Export full map image"

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
     mapedit_import_tilemaps st, NO
    END IF
   END IF
   IF state.pt = 3 THEN
    mapedit_import_tilemaps st, YES
   END IF
   IF state.pt = 4 THEN
    mapedit_export_bmp_tilemap st
   END IF
   IF state.pt = 5 THEN
    mapedit_import_bmp_tilemap st
   END IF
   IF state.pt = 6 THEN
    mapedit_export_map_image st
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


SUB mapedit_linkdoors (st as MapEditState)
 'Warning: map data should be saved before this SUB is called, as some of it's reloaded from file
 
 DIM state as MenuState
 state.top = 0
 state.pt = 0
 state.spacing = 18
 state.need_update = YES

 DIM last_resolution as XYPair
 DIM menu_temp as string
 DIM col as integer

 setkeys
 DO
  setwait 55
  setkeys
  state.tog = state.tog XOR 1
  IF keyval(scESC) > 1 THEN
   serdoorlinks(maplumpname(st.map.id, "d"), st.map.doorlink())
   EXIT DO
  END IF
  IF keyval(scF1) > 1 THEN show_help "mapedit_linkdoors"
  IF usemenu(state) THEN state.need_update = YES
  IF enter_space_click(state) THEN
   IF state.pt = state.last AND st.map.doorlink(state.pt).source = -1 THEN st.map.doorlink(state.pt).source = 0
   link_one_door st, state.pt
   state.need_update = YES
  END IF
  IF keyval(scDelete) > 1 OR keyval(scBackspace) > 1 THEN
   st.map.doorlink(state.pt).source = -1  ' Mark unused
   ' Shuffle the unused doorlink to the end, so that it will be hidden
   FOR idx as integer = state.pt TO UBOUND(st.map.doorlink) - 1
    SWAP st.map.doorlink(idx), st.map.doorlink(idx + 1)
   NEXT
   state.need_update = YES
  END IF
  IF last_resolution <> get_resolution() THEN state.need_update = YES
  IF state.need_update THEN
   state.need_update = NO
   state.last = small(find_last_used_doorlink(st.map.doorlink()) + 1, UBOUND(st.map.doorlink))
   recalc_menu_size state
   correct_menu_state state
   DrawDoorPair st, state.pt, 2
   last_resolution = get_resolution()
  END IF

  '--Draw screen
  copypage 2, dpage
  rectangle 0, rCenter, rWidth, 2, uilook(uiSelectedDisabled + state.tog), dpage
  draw_fullscreen_scrollbar state, 0, dpage
  FOR i as integer = state.top TO small(state.top + state.size, state.last)
   col = uilook(uiMenuItem)
   DIM ypos as integer = (i - state.top) * state.spacing
   IF state.pt = i THEN
    col = uilook(uiSelectedItem + state.tog)
    edgeboxstyle 0, 1 + ypos, 280, state.spacing, 0, dpage, YES, YES
   END IF

   WITH st.map.doorlink(i)
    IF .source >= 0 THEN
     menu_temp = "Door " & .source & " leads to door " & .dest & " on map " & .dest_map
     edgeprint menu_temp, 0, 2 + ypos, col, dpage

     IF .tag1 = 0 AND .tag2 = 0 THEN
      menu_temp = "  all the time"
     ELSE
      menu_temp = "  only if tag "
      IF .tag1 <> 0 THEN
       menu_temp += ABS(.tag1) & " is " & sign_string(.tag1, "OFF", "", "ON")
      END IF
      IF .tag2 THEN
       IF .tag1 THEN menu_temp += " and tag "
       menu_temp += ABS(.tag2) & " is " & sign_string(.tag2, "OFF", "", "ON")
      END IF
     END IF
     edgeprint menu_temp, 0, 10 + ypos, col, dpage
    ELSEIF i = state.last THEN
     menu_temp = "Create a new doorlink..."
     edgeprint menu_temp, 0, 2 + ypos, col, dpage
    ELSE
     menu_temp = "Unused Door link #" & i
     edgeprint menu_temp, 0, 2 + ypos, col, dpage
    END IF
   END WITH
  NEXT i
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB

SUB link_one_door(st as MapEditState, linknum as integer)
 DIM byref doorlink as DoorLink = st.map.doorlink(linknum)

 DIM ulim(4) as integer, llim(4) as integer
 ulim(0) = maxDoorsPerMap: llim(0) = -1
 ulim(1) = maxDoorsPerMap: llim(1) = 0
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
 outmap = getmapname(doorlink.dest_map)
 DIM menu_temp as string
 DIM col as integer

 DIM last_resolution as XYPair

 setkeys
 DO
  setwait 55
  setkeys
  state.tog = state.tog XOR 1
  preview_delay -= 1
  IF preview_delay = 0 OR last_resolution <> get_resolution() THEN
   last_resolution = get_resolution()
   DrawDoorPair st, linknum, 2
  END IF
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "door_link_editor"
  usemenu state
  IF state.pt >= 0 THEN
   SELECT CASE state.pt
    CASE 0
     IF intgrabber(doorlink.source, llim(state.pt), ulim(state.pt)) THEN preview_delay = 3
    CASE 1
     IF intgrabber(doorlink.dest, llim(state.pt), ulim(state.pt)) THEN preview_delay = 3
    CASE 2
     IF intgrabber(doorlink.dest_map, llim(state.pt), ulim(state.pt)) THEN
      preview_delay = 3
      outmap = getmapname(doorlink.dest_map)
     END IF
    CASE 3
     tag_grabber doorlink.tag1, state
    CASE 4
     tag_grabber doorlink.tag2, state
    CASE ELSE
     '...
   END SELECT
  ELSE
   IF enter_space_click(state) THEN EXIT DO
  END IF
  '--Draw screen
  copypage 2, dpage
  rectangle 0, rCenter, rWidth, 2, uilook(uiSelectedDisabled + state.tog), dpage
  FOR i as integer = -1 TO 4
   menu_temp = ""
   SELECT CASE i
    CASE 0
     IF doorlink.source >= 0 THEN
      menu_temp = STR(doorlink.source)
     ELSE
      menu_temp = "Unused"
     END IF
    CASE 1
     menu_temp = STR(doorlink.dest)
    CASE 2
     menu_temp = STR(doorlink.dest_map)
    CASE 3
     menu_temp = tag_condition_caption(doorlink.tag1, "", "Always")
    CASE 4
     menu_temp = tag_condition_caption(doorlink.tag2, "", "Always")
   END SELECT
   col = uilook(uiMenuItem)
   IF state.pt = i THEN col = uilook(uiSelectedItem + state.tog)
   edgeprint menu(i) & " " & menu_temp, 1, 1 + (i + 1) * 10, col, dpage
  NEXT i
  edgeprint "ENTER", pRight - 5, 0, uilook(uiText), dpage
  edgeprint "EXIT", pRight - 5, pBottom, uilook(uiText), dpage
  edgeprint outmap, 0, pBottom, uilook(uiText), dpage
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB


'==========================================================================================

'If a layer is marked visible but not enabled, it isn't drawn. Call should_draw_layer().
FUNCTION LayerIsVisible(vis() as integer, byval l as integer) as bool
 IF l < 0 OR l > maplayerMax THEN showerror "Bad map layer " & l
 'debug "layer #" & l & " is: " & readbit(vis(), 0, l)
 RETURN xreadbit(vis(), l)
END FUNCTION

'Whether a map layer is visible in-game
FUNCTION LayerIsEnabled(gmap() as integer, byval l as integer) as bool
 IF l < 0 OR l > maplayerMax THEN showerror "Bad map layer " & l
 IF l = 0 THEN RETURN YES
 'debug "layer #" & l & " is: " & readbit(gmap(), 19, l-1)
 RETURN xreadbit(gmap(), l - 1, 19)
END FUNCTION

'This layer is drawn, while editing.
FUNCTION should_draw_layer(st as MapEditState, l as integer) as bool
 RETURN layerisvisible(st.visible(), l) AND layerisenabled(st.map.gmap(), l)
END FUNCTION

SUB SetLayerVisible(vis() as integer, byval l as integer, byval v as bool)
 IF l < 0 OR l > maplayerMax THEN showerror "Bad map layer " & l
 setbit(vis(), 0, l, v)
END SUB

SUB SetLayerEnabled(gmap() as integer, byval l as integer, byval v as bool)
 IF l < 0 OR l > maplayerMax THEN showerror "Bad map layer " & l
 IF l = 0 THEN EXIT SUB
 setbit(gmap(), 19, l - 1, v)
END SUB

SUB ToggleLayerVisible(vis() as integer, byval l as integer)
 IF l < 0 OR l > maplayerMax THEN showerror "Bad map layer " & l
 setbit(vis(), 0, l, readbit(vis(), 0, l) XOR 1)
END SUB

SUB ToggleLayerEnabled(gmap() as integer, byval l as integer)
 IF l < 0 OR l > maplayerMax THEN showerror "Bad map layer " & l
 IF l = 0 THEN EXIT SUB
 setbit(gmap(), 19, l - 1, readbit(gmap(), 19, l - 1) XOR 1)
END SUB


'==========================================================================================

'Draws a preview of a certain door on a map to either the top or bottom half of a page
SUB DrawDoorPreview(map as MapData, tilesets() as TilesetData ptr, doornum as integer, top_half as bool, page as integer)
 DIM as integer dmx, dmy
 DIM as integer starty, view_width, view_height
 IF top_half THEN
  starty = 0
 ELSE
  starty = vpages(page)->h \ 2 + 2
 END IF
 view_width = vpages(page)->w
 view_height = vpages(page)->h \ 2

 IF door_exists(map.door(), doornum) THEN
  DIM byref thisdoor as Door = map.door(doornum)
  ' dmx/dmy gives the camera position (top left of viewport relative to the map)
  dmx = thisdoor.x * tilew       - (view_width - tilew) \ 2
  ' thisdoor.y is offset by 1
  dmy = (thisdoor.y - 1) * tileh - (view_height - tileh) \ 2
  ' Clamp to map edge if needed
  IF map.gmap(5) = 0 THEN  'Map edge mode = Crop
   dmx = small(large(dmx, 0), map.wide * tilew - view_width)
   dmy = small(large(dmy, 0), map.high * tileh - view_height)
  END IF
  set_map_edge_draw_mode map.gmap()
  FOR i as integer = 0 TO UBOUND(map.tiles)
   IF LayerIsEnabled(map.gmap(), i) THEN
    drawmap map.tiles(i), dmx, dmy, tilesets(i), page, i <> 0, , , starty, view_height
   END IF
  NEXT i
  IF LayerIsEnabled(map.gmap(), 0) THEN
   drawmap map.tiles(0), dmx, dmy, tilesets(0), page, 0, 2, @map.pass, starty, view_height
  END IF
  ' Position of the door on the screen
  DIM as integer door_drawx, door_drawy
  door_drawx = thisdoor.x * tilew - dmx
  door_drawy = (thisdoor.y - 1) * tileh - dmy + starty
  edgebox door_drawx, door_drawy, tilew, tileh, uilook(uiMenuItem), uilook(uiBackground), page
  textcolor uilook(uiBackground), 0
  DIM as string caption = STR(doornum)
  printstr caption, door_drawx + tilew \ 2 + ancCenter, door_drawy + tileh \ 2 + 1 + ancCenter, page
 ELSE
  textcolor uilook(uiDisabledItem), 0
  DIM as string caption = "(No such door)"
  printstr caption, pCentered, starty + view_height \ 2, page
 END IF
 
END SUB

' Draw preview of source and destination doors of a doorlink.
SUB DrawDoorPair(st as MapEditState, linknum as integer, page as integer)
 DIM byref doorlink as DoorLink = st.map.doorlink(linknum)

 clearpage page
 IF doorlink.source = -1 THEN EXIT SUB

 ' Entry door
 DrawDoorPreview st.map, st.tilesets(), doorlink.source, YES, page

 ' Exit door
 IF doorlink.dest_map = st.map.id THEN
  DrawDoorPreview st.map, st.tilesets(), doorlink.dest, NO, page
 ELSE
  DIM destmap as MapData
  DIM dest_tilesets(maplayerMax) as TilesetData ptr
  destmap.id = doorlink.dest_map
  loadrecord destmap.gmap(), game + ".map", getbinsize(binMAP) \ 2, destmap.id
  deserdoors game + ".dox", destmap.door(), destmap.id
  LoadTilemaps destmap.tiles(), maplumpname(destmap.id, "t")
  LoadTilemap destmap.pass, maplumpname(destmap.id, "p")
  loadmaptilesets dest_tilesets(), destmap.gmap()
  destmap.wide = destmap.pass.wide
  destmap.high = destmap.pass.high

  DrawDoorPreview destmap, dest_tilesets(), doorlink.dest, NO, page

  unloadmaptilesets dest_tilesets()
 END IF
END SUB

SUB calculatepassblock(st as MapEditState, x as integer, y as integer)
 DIM n as integer = 0
 DIM tilenum as integer
 FOR i as integer = 0 TO UBOUND(st.map.tiles)
  tilenum = readblock(st.map.tiles(i), x, y)
  IF i = 0 OR tilenum > 0 THEN
   n = n OR st.defaultwalls[i][tile_anim_deanimate_tile(tilenum, st.tilesets(i)->tastuf())]
  END IF
 NEXT i
 DIM oldval as integer = readblock(st.map.pass, x, y)
 IF oldval <> n THEN
  add_undo_step st, x, y, oldval, mapIDPass
  writeblock st.map.pass, x, y, n
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
 setvispage page, NO

 dim tmp as TileMap
 cleantilemap tmp, new_width, new_height
 tmp.layernum = tmap.layernum  'the unexpected ingredient!

 dim as integer x, y
 for x = large(x_off, 0) to small(tmap.wide, new_width + x_off) - 1
	for y = large(y_off, 0) to small(tmap.high, new_height + y_off) - 1
		'newarray((x - tempx) * tempw + (y - tempy) + 2) = tmp(x * st.map.wide + y + 2)
		writeblock(tmp, x - x_off, y - y_off, readblock(tmap, x, y))
	next
 next
 unloadtilemap tmap
 memcpy(@tmap, @tmp, sizeof(TileMap))
 'obviously don't free tmp
END SUB

SUB resizemapmenu (st as MapEditState, byref rs as MapResizeState)
 'returns the new size and offset in passed args, or -1 width to cancel

 InitLikeStandardMenu rs.menu
 WITH rs.menu
  .alignvert = alignBottom
  .anchorvert = alignBottom
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
 rs.oldsize.x = st.map.wide
 rs.oldsize.y = st.map.high
 rs.rect.wide = rs.oldsize.x
 rs.rect.high = rs.oldsize.y
 rs.rect.x = 0
 rs.rect.y = 0
 
 resize_rezoom_mini_map st, rs
 resize_buildmenu rs
 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(scESC) > 1 THEN
   'Unlike every other menu, hitting ESC cancels changes, so confirm if changes were made
   IF (rs.rect.x = 0 AND rs.rect.y = 0 AND rs.rect.wide = rs.oldsize.x AND rs.rect.high = rs.oldsize.y) _
      ORELSE yesno("Cancel resize?", NO, YES) THEN  'Must default to yes on ESC
    rs.rect.wide = -1
    rs.rect.high = -1
    EXIT DO
   END IF
  END IF
  IF keyval(scF1) > 1 THEN show_help "resize_map"
  usemenu state
  IF keyval(scShift) > 0 THEN incval = 8 ELSE incval = 1
  SELECT CASE state.pt
   CASE 0
    IF keyval(scAnyEnter) > 1 THEN
     rs.rect.wide = -1
     rs.rect.high = -1
     EXIT DO
    END IF
   CASE 1
    IF keyval(scLeft) > 0 THEN rs.rect.wide -= incval
    IF keyval(scRight) > 0 THEN rs.rect.wide += incval
    resize_correct_width st, rs
   CASE 2
    IF keyval(scLeft) > 0 THEN rs.rect.high -= incval
    IF keyval(scRight) > 0 THEN rs.rect.high += incval
    resize_correct_height st, rs
   CASE 3
    IF keyval(scLeft) > 0 THEN rs.rect.x -= incval: rs.rect.wide += incval
    IF keyval(scRight) > 0 THEN rs.rect.x += incval: rs.rect.wide -= incval
    resize_correct_width st, rs
   CASE 4
    IF keyval(scLeft) > 0 THEN rs.rect.y -= incval: rs.rect.high += incval
    IF keyval(scRight) > 0 THEN rs.rect.y += incval: rs.rect.high -= incval
    resize_correct_height st, rs
  END SELECT
  IF keyval(scAnyEnter) > 1 THEN EXIT DO

  clearpage dpage
  drawoff.x = large(0, -rs.rect.x * rs.zoom)
  drawoff.y = large(0, -rs.rect.y * rs.zoom)
  frame_draw rs.minimap, NULL, drawoff.x, drawoff.y, 1, NO, dpage
  drawbox drawoff.x + rs.zoom * rs.rect.x, drawoff.y + rs.zoom * rs.rect.y, rs.zoom * rs.rect.wide, rs.zoom * rs.rect.high, 14 + state.tog, 1, dpage
  draw_menu rs.menu, state, dpage

  SWAP dpage, vpage
  setvispage vpage
  dowait
 LOOP
 frame_unload @(rs.minimap)
 ClearMenuData rs.menu

END SUB

SUB resize_correct_width(st as MapEditState, byref rs as MapResizeState)
 rs.rect.wide = bound(rs.rect.wide, 16, mapTilesMax)
 rs.rect.x = bound(rs.rect.x, -rs.rect.wide + 1, rs.oldsize.x - 1)
 WHILE rs.rect.high * rs.rect.wide > mapTilesMax AND rs.rect.high > 10
  rs.rect.high -= 1
 WEND
 resize_dimchange st, rs
END SUB

SUB resize_correct_height(st as MapEditState, byref rs as MapResizeState)
 rs.rect.high = bound(rs.rect.high, 10, mapTilesMax)
 rs.rect.y = bound(rs.rect.y, -rs.rect.high + 1, rs.oldsize.y - 1)
 WHILE rs.rect.high * rs.rect.wide > mapTilesMax AND rs.rect.wide > 16
  rs.rect.wide -= 1
 WEND
 resize_dimchange st, rs
END SUB

SUB resize_dimchange(st as MapEditState, byref rs as MapResizeState)
 WHILE rs.rect.high * rs.rect.wide > mapTilesMax
  rs.rect.high = large(rs.rect.high - 1, 10)
  rs.rect.wide = large(rs.rect.wide - 1, 16)
 WEND
 resize_rezoom_mini_map st, rs
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

SUB resize_rezoom_mini_map(st as MapEditState, byref rs as MapResizeState)
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
  rs.minimap = createminimap(st.map.tiles(), st.tilesets(), @st.map.pass, rs.zoom)
 END IF
END SUB

SUB show_minimap(st as MapEditState)
 DIM minimap as Frame Ptr
 minimap = createminimap(st.map.tiles(), st.tilesets(), @st.map.pass)

 clearpage vpage
 frame_draw minimap, NULL, 0, 0, 1, NO, vpage
 frame_unload @minimap

 edgeprint "Press Any Key", pRight, pBottom, uilook(uiText), vpage
 setvispage vpage
 waitforanykey
END SUB


'==========================================================================================
'                                    Fill (paint) tool
'==========================================================================================


SUB fill_map_add_node(st as MapEditState, byval followTile as integer, byval oldTile as integer, byval x as integer, byval y as integer, byref head as integer, queue() as XYPair, reader as FnReader)
 IF (y < st.map.foemap.high) AND (y >= 0) AND (x < st.map.foemap.wide) AND (x >= 0) THEN  'st.map.foemap is not special
  IF reader(st, x, y) = followTile THEN
   IF st.reader(st, x, y) = oldTile THEN   'Would be interesting to see whether this redundant check speeds or slows things
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
SUB fill_map_area(st as MapEditState, byval x as integer, byval y as integer, reader as FnReader)
 DIM as integer oldtile, newTile, followTile
 oldTile = st.reader(st, x, y)
 followTile = reader(st, x, y)
 newTile = st.tool_value
 IF oldTile = newTile THEN EXIT SUB
 REDIM queue(250) as XYPair 'a circular buffer. We don't use the last element
 DIM as integer head, tail, i, oldend
 fill_map_add_node st, followTile, oldTile, x, y, head, queue(), reader
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
   IF st.reader(st, .x, .y) = oldTile THEN
    st.brush(st, .x, .y, newTile)

    fill_map_add_node st, followTile, oldTile, .x + 1, .y, head, queue(), reader
    fill_map_add_node st, followTile, oldTile, .x - 1, .y, head, queue(), reader
    fill_map_add_node st, followTile, oldTile, .x, .y + 1, head, queue(), reader
    fill_map_add_node st, followTile, oldTile, .x, .y - 1, head, queue(), reader
   END IF
  END WITH
  tail = (tail + 1) MOD UBOUND(queue)
 WEND
END SUB

'Finding a continuous area by reading from a map with the given reader,
'fill that area using st.brush and st.tool_value. This is done by first
'drawing a stencil to a temporary TileMap and then copying it
SUB fill_with_other_area(st as MapEditState, byval x as integer, byval y as integer, reader as FnReader)
 DIM oldbrush as FnBrush = st.brush
 DIM oldreader as FnReader = st.reader
 DIM oldvalue as integer = st.tool_value
 st.brush = @tempbrush
 st.reader = @tempreader
 st.tool_value = 1

 CleanTileMap st.temptilemap, st.map.wide, st.map.high

 fill_map_area st, x, y, reader

 st.brush = oldbrush
 st.reader = oldreader
 st.tool_value = oldvalue

 FOR ty as integer = 0 TO st.map.high - 1
  FOR tx as integer = 0 TO st.map.wide - 1
   'IF tempreader(st, tx, ty) THEN
   IF readblock(st.temptilemap, tx, ty) THEN
    st.brush(st, tx, ty, st.tool_value)
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
 DIM success as bool
 DIM partial_retval as bool
 success = loadrecord(buf(), workingdir & SLASH & "defpass.bin", 322 \ 2, tilesetnum, partial_retval)
 IF NOT success THEN
  IF partial_retval THEN
   debug "Partial record loaded, defpass.bin might be corrupted"
  ELSE
   'debuginfo "defpass.bin record " & tilesetnum & " does not exist, and that is okay"
   EXIT SUB
  END IF
 END IF
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
 tilepick = XY(0, 0)

 'First 10 rows are regular tiles
 FOR i as integer = 0 TO 159
  writeblock tilesetview, i MOD 16, i \ 16, i
  IF i = st.usetile(st.layer) THEN tilepick = XY(i MOD 16, i \ 16)
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
 DIM byref mouse as MouseInfo = readmouse
 DIM tilepick as XYPair  'Coordinates (in tiles) of the selected tile in tilesetview
 DIM dragging as bool = NO
 DIM chequer_scroll as integer
 DIM tog as integer
 DIM holdpos as XYPair
 DIM scrolly as integer = 0 'Y position in pixels of the camera/top of the screen
 DIM bgcolor as bgType = 0
 IF st.layer > 0 THEN bgcolor = bgChequerScroll

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
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "mapedit_tilemap_picktile"

  tilesetview.high = IIF(show_animated_tiles, real_tilesetview_high, 10)

  ' Cursor movement
  DIM repeatms as integer = 80
  IF keyval(scShift) > 0 THEN repeatms = 40
  IF slowkey(scUp, repeatms) AND tilepick.y > 0 THEN tilepick.y -= 1
  IF slowkey(scDown, repeatms) AND tilepick.y < tilesetview.high - 1 THEN tilepick.y += 1
  IF slowkey(scLeft, repeatms) AND tilepick.x > 0 THEN tilepick.x -= 1
  IF slowkey(scRight, repeatms) AND tilepick.x < tilesetview.wide - 1 THEN tilepick.x += 1
  IF mouse.buttons THEN
   IF mouse.x < tilesetview.wide * tilew AND mouse.y + scrolly < tilesetview.high * tileh THEN
    tilepick = (mouse.pos + XY(0, scrolly)) \ tilesize
   END IF
  END IF

  IF (keyval(scAnyEnter) > 1 OR keyval(scSpace) > 1 OR (mouse.clicks AND mouseLeft)) AND dragging = NO THEN
   'start drag-selecting a box
   dragging = YES
   holdpos = tilepick
  END IF
  IF keyval(scAnyEnter) = 0 AND keyval(scSpace) = 0 AND mouse.buttons = 0 AND dragging = YES THEN
   mapedit_pick_tileset_rect st, tilesetview, tilepick, holdpos
   EXIT DO
  END IF
  IF dragging = NO AND keyval(scA) > 1 THEN
   show_animated_tiles XOR= YES
  END IF

  ' This is NOT tile ID, it's linearised index in tilesetview
  DIM tileoffset as integer = int_from_xy(tilepick, tilesetview.wide, tilesetview.high)
  IF slowkey(scComma, repeatms) AND tileoffset > 0 THEN tileoffset -= 1
  IF slowkey(scPeriod, repeatms) AND tileoffset < tilesetview.wide * tilesetview.high - 1 THEN tileoffset += 1
  tilepick = xy_from_int(tileoffset, tilesetview.wide, tilesetview.high)
  set_usetile st, readblock(tilesetview, tilepick.x, tilepick.y)

  scrolly += mouse.wheel_delta \ 6
  ' Keep the selected tile in view
  scrolly = bound(scrolly, tilepick.y * 20 + 30 - vpages(vpage)->h, tilepick.y * 20 - 10)
  ' Don't over-scroll
  scrolly = large(small(scrolly, tilesetview.high * 20 - vpages(vpage)->h), 0)

  'Draw screen
  draw_background vpages(vpage), bgcolor, chequer_scroll
  drawmap tilesetview, 0, scrolly, tilesetdata, vpage, YES
  DIM as RelPos infoline_y = pTop
  IF tilepick.y * 20 - scrolly < vpages(vpage)->h - 80 THEN
   infoline_y = pBottom
  END IF
  DIM infotext as string
  infotext = "Tile " & st.usetile(st.layer)
  IF st.usetile(st.layer) >= 160 THEN
   infotext & = !"\nAnimation set " & ((st.usetile(st.layer) - 160) \ 48)
  END IF
  edgeprint infotext, 0, infoline_y, uilook(uiText), vpage, YES, YES
  infotext = " Hold/drag to select a rectangle"
  IF show_animated_tiles = NO THEN
   infotext &= !"\nA: show animated tiles"
  END IF
  edgeprint infotext, pRight, infoline_y, uilook(uiText), vpage, YES, YES

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
FUNCTION redo_stroke(st as MapEditState) as MapEditUndoTile vector
 st.message_ticks = 20
 IF st.history_step = v_len(st.history) THEN
  st.message = "No more Redo history"
  RETURN NULL
 END IF

 RETURN undo_stroke(st, YES)
END FUNCTION

'A stroke is a group of brush applications/steps
'Returns the stroke which was undone
FUNCTION undo_stroke(st as MapEditState, byval redo as bool = NO) as MapEditUndoTile vector
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
 undo_stroke_internal st, undostroke, redo

 IF redo THEN st.history_step += 1
 st.message = CHR(27) + " " & st.history_step & " Undo steps | " & (v_len(st.history) - st.history_step) & " Redo steps " + CHR(26)

 RETURN undostroke
END FUNCTION

'Undoes the changes in st.secondary_undo_buffer
SUB undo_preview(st as MapEditState)
 IF st.secondary_undo_buffer THEN
  undo_stroke_internal st, st.secondary_undo_buffer, NO
  v_free st.secondary_undo_buffer
 END IF
END SUB

'Either redo or undo an undo history changelist
SUB undo_stroke_internal(st as MapEditState, byref changelist as MapEditUndoTile vector, byval redo as bool = NO)
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
    overwrite_value = readblock(st.map.tiles(.mapid - mapIDLayer), .x, .y)
    writeblock st.map.tiles(.mapid - mapIDLayer), .x, .y, .value
   ELSEIF .mapid = mapIDPass THEN
    overwrite_value = readblock(st.map.pass, .x, .y)
    writeblock st.map.pass, .x, .y, .value
   ELSEIF .mapid = mapIDFoe THEN
    overwrite_value = readblock(st.map.foemap, .x, .y)
    writeblock st.map.foemap, .x, .y, .value
   ELSEIF .mapid >= mapIDZone THEN
    overwrite_value = CheckZoneAtTile(st.map.zmap, .mapid - mapIDZone, .x, .y)
    IF .value THEN
     IF SetZoneTile(st.map.zmap, .mapid - mapIDZone, .x, .y) = NO THEN
      showerror "SetZoneTile failed during undo: impossible!"
     END IF
    ELSE
     UnsetZoneTile st.map.zmap, .mapid - mapIDZone, .x, .y
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


FUNCTION create_changelist(st as MapEditState, rect as RectType) as MapEditUndoTile vector
 DIM changelist as MapEditUndoTile vector
 v_new changelist
 DIM zones() as integer

 FOR xoff as integer = 0 TO rect.wide - 1
  DIM x as integer = xoff + rect.x
  IF NOT in_bound(x, 0, st.map.wide - 1) THEN CONTINUE FOR
  FOR yoff as integer = 0 TO rect.high - 1
   DIM y as integer = yoff + rect.y
   IF NOT in_bound(y, 0, st.map.high - 1) THEN CONTINUE FOR

   FOR layer as integer = 0 TO UBOUND(st.map.tiles)
    IF LayerIsEnabled(st.map.gmap(), layer) AND LayerIsVisible(st.visible(), layer) THEN
     IF readblock(st.map.tiles(layer), x, y) THEN
      add_change_step changelist, xoff, yoff, readblock(st.map.tiles(layer), x, y), mapIDLayer + layer
     END IF
    END IF
   NEXT
   IF readblock(st.map.pass, x, y) THEN
    add_change_step changelist, xoff, yoff, readblock(st.map.pass, x, y), mapIDPass
   END IF
   IF readblock(st.map.foemap, x, y) THEN
    add_change_step changelist, xoff, yoff, readblock(st.map.foemap, x, y), mapIDFoe
   END IF

   GetZonesAtTile st.map.zmap, zones(), x, y
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
SUB apply_changelist(st as MapEditState, byref changelist as MapEditUndoTile vector, offset as XYPair)
 'IF v_len(changelist) = 0 THEN showerror "Strange... empty undo step. Probably harmless"

 'debug "apply_changelist len " & v_len(changelist)

 IF st.clone_merge = NO THEN
  'First remove all existing tile data

  DIM as RectType clone_box
  clone_box = TYPE(st.x - st.clone_offset.x, st.y - st.clone_offset.y, st.clone_size.w, st.clone_size.h)

  FOR xoff as integer = 0 TO clone_box.wide - 1
   DIM x as integer = xoff + clone_box.x
   IF NOT in_bound(x, 0, st.map.wide - 1) THEN CONTINUE FOR
   FOR yoff as integer = 0 TO clone_box.high - 1
    DIM y as integer = yoff + clone_box.y
    IF NOT in_bound(y, 0, st.map.high - 1) THEN CONTINUE FOR

    'This is quite different to BACKSPACE tile clearing:
    'Clear visible tiles, walls, foemap, zones,
    'but not doors or NPCs
    FOR layer as integer = 0 TO UBOUND(st.map.tiles)
     IF LayerIsEnabled(st.map.gmap(), layer) AND LayerIsVisible(st.visible(), layer) THEN
      tilebrush st, x, y, 0, layer
     END IF
    NEXT
    wallbrush st, x, y, 0
    foebrush st, x, y, 0
    DIM zones() as integer
    GetZonesAtTile st.map.zmap, zones(), x, y
    FOR ctr as integer = 0 TO UBOUND(zones)
     zonebrush st, x, y, 0, zones(ctr)
    NEXT
   NEXT
  NEXT
 END IF

 FOR i as integer = 0 TO v_len(changelist) - 1
  WITH changelist[i]
   DIM as integer x = .x + offset.x, y = .y + offset.y
   IF NOT in_bound(x, 0, st.map.wide - 1) THEN CONTINUE FOR
   IF NOT in_bound(y, 0, st.map.high - 1) THEN CONTINUE FOR

   IF .mapid >= mapIDLayer THEN
    DIM layer as integer = .mapid - mapIDLayer
    IF should_draw_layer(st, layer) THEN
     tilebrush st, x, y, .value, layer
    END IF
   ELSEIF .mapid = mapIDPass THEN
    IF st.defpass = NO THEN
     'Merge walls with existing ones
     DIM wallmask as integer = readblock(st.map.pass, x, y) OR .value
     wallbrush st, x, y, wallmask
    END IF
   ELSEIF .mapid = mapIDFoe THEN
    foebrush st, x, y, .value
   ELSEIF .mapid >= mapIDZone THEN
    'Effectively merges zones with existing ones
    zonebrush st, x, y, .value, (.mapid - mapIDZone)
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
'                                         Camera

'Translate a pixel position on the map to a pixel position on the screen
FUNCTION map_to_screen(st as MapEditState, map_pos as XYPair) as XYPair
 RETURN XY(map_pos.x - st.mapx, map_pos.y - st.mapy + 20)
END FUNCTION

FUNCTION map_to_screen(st as MapEditState, map_rect as RectType) as RectType
 RETURN TYPE(map_rect.x - st.mapx, map_rect.y - st.mapy + 20, map_rect.wide, map_rect.high)
END FUNCTION

'Translate a pixel position on the screen to a pixel position on the map;
'returns -1,-1 if off the map edge
FUNCTION screen_to_map(st as MapEditState, pos as XYPair) as XYPair
 DIM ret as XYPair = (pos.x + st.mapx, pos.y + st.mapy - 20)
 IF ret.x < 0 OR ret.y < 0 OR ret.x >= st.map.wide * tilew OR ret.y >= st.map.high * tileh THEN
  RETURN XY(-1, -1)
 END IF
 RETURN ret
END FUNCTION

FUNCTION mapedit_mouse_over_what(st as MapEditState) as MouseAttention
 DIM byref mouse as MouseInfo = readmouse
 IF mouse.active = NO THEN RETURN focusNowhere

 IF st.toolsbar_available ANDALSO rect_collide_point(toolbar_rect(st), mouse.pos) THEN
  RETURN focusToolbar
 ELSEIF mouse.y >= 20 THEN
  IF screen_to_map(st, mouse.pos).x < 0 THEN
   RETURN focusViewport
  ELSE
   RETURN focusMap
  END IF
 ELSE
  RETURN focusTopbar
 END IF
END FUNCTION

'Can a tile be seen? (Specifically, the centre of the tile)
FUNCTION mapedit_on_screen(st as MapEditState, byval x as integer, byval y as integer) as integer
 'Visible portion of the map
 DIM mapview as RectType
 mapview.x = st.mapx
 mapview.y = st.mapy
 mapview.wide = vpages(dpage)->w
 mapview.high = vpages(dpage)->h - 20  '20 pixels for menubar
 RETURN rect_collide_point(mapview, XY(x * 20 + 10, y * 20 + 10))
END FUNCTION

'Center the camera on a tile
SUB mapedit_focus_camera(st as MapEditState, byval x as integer, byval y as integer)
 'Size of the map viewport
 DIM mapviewsize as XYPair = vpages(dpage)->size
 mapviewsize.h -= 20  'For menubar
 st.mapx = bound(x * 20 - mapviewsize.w \ 2, 0, st.map.wide * 20 - mapviewsize.w)
 st.mapy = bound(y * 20 - mapviewsize.h \ 2, 0, st.map.high * 20 - mapviewsize.h)
END SUB

'Make sure the camera position is within map limits. Ignores cursor.
SUB mapedit_constrain_camera(st as MapEditState)
 DIM mapviewsize as XYPair = vpages(dpage)->size
 mapviewsize.h -= 20  'For menubar
 st.mapx = small(st.mapx, st.map.wide * 20 - mapviewsize.w)
 st.mapy = small(st.mapy, st.map.high * 20 - mapviewsize.h)
 st.mapx = large(st.mapx, 0)
 st.mapy = large(st.mapy, 0)
END SUB


'======================================= NPC Editor =======================================


' State of edit_npc (single NPC definition editor)
TYPE NPCEditState
 state as MenuState
 menu as SimpleMenuItem vector
 ' IDs to match up menu items with actions. Currently equal to .N data index, but treat as meaningless.
 itemids(any) as integer

 ' Preview current item/textbox/script/vechicle name
 itemname as string
 boxpreview as string
 scrname as string
 vehiclename as string

 DECLARE SUB menu_append(itemid as integer = -2, menuitem as string, unselectable as bool = NO)
END TYPE


SUB NPCEditState.menu_append(itemid as integer = -2, menuitem as string, unselectable as bool = NO)
 append_simplemenu_item menu, menuitem, unselectable, , itemid
 v_end(menu)[-1].disabled = unselectable
END SUB

FUNCTION editnpc_zone_caption(byval zoneid as integer, byval default as integer, zmap as ZoneMap) as string
 DIM caption as string
 IF zoneid = 0 THEN
  caption = " Map default:"
  zoneid = default
 ELSEIF zoneid = -1 THEN
  'We use -1 instead of 0 for None simply so that the default value
  '(including in existing games) is 'default'
  zoneid = 0
 END IF
 IF zoneid = 0 THEN
  caption += " None"
 ELSE
  caption += " " & zoneid & " " & GetZoneInfo(zmap, zoneid)->name
 END IF
 RETURN caption
END FUNCTION

SUB update_edit_npc (npcdata as NPCType, ed as NPCEditState, gmap() as integer, zmap as ZoneMap)
 REDIM ed.itemids(-1 TO -1)
 v_new ed.menu

 ed.menu_append -1, "Previous Menu"
 ed.menu_append  0, "Picture " & npcdata.picture
 ed.menu_append  1, "Palette " & defaultint(npcdata.palette)

 ed.menu_append   , "Movement", YES
 ed.menu_append  2, "Move Type = " & safe_caption(npc_movetypes(), npcdata.movetype, "movetype")
 IF npcdata.movetype = 15 THEN
  DIM obs_caption as string
  SELECT CASE npcdata.pathfinding_obstruction_mode
   CASE 0: obs_caption = "Default for this map"
    SELECT CASE gmap(378)
     CASE 0, 1: obs_caption &= " (NPCs Obstruct)"
     CASE 2: obs_caption &= " (Ignore NPCs)"
    END SELECT
   CASE 1: obs_caption = "NPCs Obstruct"
   CASE 2: obs_caption = "Ignore NPCs"
  END SELECT
  ed.menu_append 18, " Pathfinding rule: " & obs_caption
 END IF
 ed.menu_append  3, "Move Speed " & npcdata.speed
 ed.menu_append 15, "Movement Zone:" & editnpc_zone_caption(npcdata.defaultzone, gmap(32), zmap)
 ed.menu_append 16, "Avoidance Zone:" & editnpc_zone_caption(npcdata.defaultwallzone, gmap(33), zmap)
 ed.menu_append 17, "Ignore Passmap: " & yesorno(npcdata.ignore_passmap)
 ed.menu_append  7, "Pushability " & safe_caption(npc_pushtypes(), npcdata.pushtype, "pushtype")

 ed.menu_append   , "Activation by player:", YES
 ed.menu_append  8, "Activation: " & safe_caption(npc_usetypes(), npcdata.activation, "usetype")
 ed.menu_append  4, "Display Text " & zero_default(npcdata.textbox, "[None]")
 ed.menu_append  6, "Give Item: " & ed.itemname
 ed.menu_append 12, "Run Script: " & ed.scrname
 ed.menu_append 13, "Script Argument: " & IIF(npcdata.script, STR(npcdata.scriptarg), "N/A")
 ed.menu_append 14, "Vehicle: " & IIF(npcdata.vehicle > 0, ed.vehiclename, "No")
 ed.menu_append  5, "When Activated " & safe_caption(npc_facetypes(), npcdata.facetype, "facetype")
 IF npcdata.usetag THEN
  ed.menu_append 11, "Usable Only Once (onetime " & npcdata.usetag & ")"
 ELSE
  ed.menu_append 11, "Usable Repeatedly"
 END IF

 ed.menu_append   , "Appears when...:", YES
 ed.menu_append  9, "Appear if Tag " & tag_condition_caption(npcdata.tag1, "", "Always")
 ed.menu_append 10, "Appear if Tag " & tag_condition_caption(npcdata.tag2, "", "Always")

 ed.state.last = v_len(ed.menu) - 1
END SUB

' Editor for a single NPC definition
SUB edit_npc (npcdata as NPCType, gmap() as integer, zmap as ZoneMap)
 DIM ed as NPCEditState
 DIM menu_display as BasicMenuItem vector

 DIM walk as integer = 0
 DIM tog as integer = 0

 DIM selectst as SelectTypeState
 WITH ed.state
  .autosize = YES
  .autosize_ignore_pixels = 24
  .need_update = YES
 END WITH
 DIM menuopts as MenuOptions
 menuopts.fullscreen_scrollbar = YES

 DIM npc_img as GraphicPair
 load_sprite_and_pal npc_img, sprTypeWalkabout, npcdata.picture, npcdata.palette

 ed.itemname = load_item_name(npcdata.item, 0, 0)
 ed.boxpreview = textbox_preview_line(npcdata.textbox)
 ed.scrname = scriptname(npcdata.script)
 ed.vehiclename = load_vehicle_name(npcdata.vehicle - 1)

 update_edit_npc npcdata, ed, gmap(), zmap

 setkeys YES
 DO
  setwait 55
  setkeys YES
  tog = tog XOR 1
  IF npcdata.movetype > 0 THEN walk = (walk + 1) MOD 4
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "edit_npc"
  usemenu ed.state, cast(BasicMenuItem vector, ed.menu)
  DIM itemid as integer = ed.menu[ed.state.pt].dat
  SELECT CASE itemid
   CASE 0'--picture
    IF intgrabber(npcdata.picture, 0, gen(genMaxNPCPic)) THEN
     load_sprite_and_pal npc_img, sprTypeWalkabout, npcdata.picture, npcdata.palette
    END IF
    IF enter_space_click(ed.state) THEN
     DIM walkaboutb as WalkaboutSpriteBrowser
     npcdata.picture = walkaboutb.browse(npcdata.picture)
     load_sprite_and_pal npc_img, sprTypeWalkabout, npcdata.picture, npcdata.palette
    END IF
   CASE 1'--palette
    IF intgrabber(npcdata.palette, -1, gen(genMaxPal)) THEN
     load_sprite_and_pal npc_img, sprTypeWalkabout, npcdata.picture, npcdata.palette
    END IF
    IF enter_space_click(ed.state) THEN
     npcdata.palette = pal16browse(npcdata.palette, sprTypeWalkabout, npcdata.picture, YES)
     load_sprite_and_pal npc_img, sprTypeWalkabout, npcdata.picture, npcdata.palette
    END IF
   CASE 2
    intgrabber(npcdata.movetype, 0, ubound(npc_movetypes))
    IF enter_space_click(ed.state) THEN
     DIM constb as ArrayBrowser = ArrayBrowser(npc_movetypes(), "Movement Types")
     npcdata.movetype = constb.browse(npcdata.movetype)
    END IF
   CASE 3
    'yuck.
    IF npcdata.speed = 10 THEN npcdata.speed = 3
    intgrabber(npcdata.speed, 0, 5)
    IF npcdata.speed = 3 THEN npcdata.speed = 10
   CASE 4
    IF textboxgrabber(npcdata.textbox, ed.state) THEN
     ed.boxpreview = textbox_preview_line(npcdata.textbox)
    END IF
   CASE 5
    intgrabber(npcdata.facetype, 0, ubound(npc_facetypes))
    IF enter_space_click(ed.state) THEN
     DIM constb as ArrayBrowser = ArrayBrowser(npc_facetypes(), "Facing Types")
     npcdata.facetype = constb.browse(npcdata.facetype)
    END IF
   CASE 6
    IF intgrabber(npcdata.item, 0, gen(genMaxItem) + 1) THEN
     ed.itemname = load_item_name(npcdata.item, 0, 0)
    END IF
    IF enter_space_click(ed.state) THEN
     DIM itemb as ItemBrowser
     npcdata.item = itemb.browse(npcdata.item - 1, YES) + 1
     ed.itemname = load_item_name(npcdata.item, 0, 0)
    END IF
   CASE 7
    intgrabber(npcdata.pushtype, 0, ubound(npc_pushtypes))
    IF enter_space_click(ed.state) THEN
     DIM constb as ArrayBrowser = ArrayBrowser(npc_pushtypes(), "Pushing Types")
     npcdata.pushtype = constb.browse(npcdata.pushtype)
    END IF
   CASE 8
    intgrabber(npcdata.activation, 0, ubound(npc_usetypes))
    IF enter_space_click(ed.state) THEN
     DIM constb as ArrayBrowser = ArrayBrowser(npc_usetypes(), "Activation Types")
     npcdata.activation = constb.browse(npcdata.activation)
    END IF
   CASE 9'--tag conditionals
    tag_grabber npcdata.tag1, ed.state
   CASE 10'--tag conditionals
    tag_grabber npcdata.tag2, ed.state
   CASE 11'--one-time-use tag
    IF keyval(scLeft) > 1 OR keyval(scRight) > 1 OR enter_space_click(ed.state) THEN
     onetimetog npcdata.usetag
    END IF
   CASE 12'--script
    IF enter_space_click(ed.state) THEN
     ed.scrname = scriptbrowse(npcdata.script, plottrigger, "NPC use plotscript")
    ELSEIF scrintgrabber(npcdata.script, 0, 0, scLeft, scRight, 1, plottrigger) THEN
     ed.scrname = scriptname(npcdata.script)
    END IF
   CASE 13
    intgrabber(npcdata.scriptarg, -32768, 32767)
   CASE 14
    IF intgrabber(npcdata.vehicle, 0, gen(genMaxVehicle) + 1) THEN
     ed.vehiclename = load_vehicle_name(npcdata.vehicle - 1)
    END IF
   CASE 15
    intgrabber(npcdata.defaultzone, -1, 9999)
   CASE 16
    intgrabber(npcdata.defaultwallzone, -1, 9999)
   CASE 17
    intgrabber(npcdata.ignore_passmap, 0, 1)
    IF enter_space_click(ed.state) THEN npcdata.ignore_passmap XOR= 1
   CASE 18
    intgrabber(npcdata.pathfinding_obstruction_mode, 0, obmodeLAST)
   CASE -1' previous menu
    IF enter_space_click(ed.state) THEN EXIT DO
  END SELECT

  update_edit_npc npcdata, ed, gmap(), zmap

  IF select_by_typing(selectst, NO) THEN
   select_on_word_boundary cast(BasicMenuItem vector, ed.menu), selectst, ed.state
  END IF

  '--Draw screen
  clearpage dpage
  highlight_menu_typing_selection cast(BasicMenuItem vector, ed.menu), menu_display, selectst, ed.state
  standardmenu menu_display, ed.state, 0, 0, dpage, menuopts
  npcdefedit_preview_npc npcdata, npc_img, ed.boxpreview, 4 + (walk \ 2)

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP

 v_free ed.menu
 v_free menu_display

 unload_sprite_and_pal npc_img
END SUB

' Displays the NPC walkabout, tag conditions and textbox preview at the bottom of the screen
' (Default to displaying south1 frame)
SUB npcdefedit_preview_npc(npcdata as NPCType, npc_img as GraphicPair, boxpreview as string, framenum as integer = 4)
 edgebox pRight - 15, pBottom - 23, npc_img.sprite->w + 2, npc_img.sprite->h + 2, uilook(uiDisabledItem), uilook(uiText), dpage
 frame_draw npc_img.sprite + framenum, npc_img.pal, pRight - 16, pBottom - 24, 1, YES, dpage
 textcolor uilook(uiSelectedItem2), uiLook(uiHighlight)
 printstr boxpreview, 0, pBottom - 10, dpage
 textcolor uilook(uiSelectedItem2), 0
 printstr describe_two_tag_condition("Appears if", "Appears all the time", "Never appears!", YES, npcdata.tag1, npcdata.tag2), 0, pBottom, dpage
END SUB

'Wrapper around edit_npc to do the right thing
'(npcdata should be an element of st.map.npc_def())
SUB mapedit_edit_npcdef (st as MapEditState, npcdata as NPCType)
 'First save NPCs so that we can correctly search for unused one-time use tags (see onetimetog)
 SaveNPCD maplumpname(st.map.id, "n"), st.map.npc_def()
 edit_npc npcdata, st.map.gmap(), st.map.zmap
 load_npc_graphics st.map.npc_def(), st.npc_img()
END SUB


'----------------------------- Toplevel NPC Editor ----------------------------

SUB handle_npc_def_delete (npc() as NPCType, byval id as integer, npc_insts() as NPCInst)

 '--Count number of uses
 DIM as integer uses = 0
 FOR i as integer = 0 to UBOUND(npc_insts)
  IF npc_insts(i).id = id + 1 THEN uses += 1
 NEXT

 IF uses > 0 THEN
  IF yesno("There are " & uses & " copies of NPC ID " & id & " on the map. Are you sure you want to delete them?", NO, NO) = NO THEN EXIT SUB

  '--Delete instances of this ID
  FOR i as integer = 0 to UBOUND(npc_insts)
   IF npc_insts(i).id = id + 1 THEN npc_insts(i).id = 0
  NEXT
 END IF

 '--Wiping a definition clear, or completely deleting it?
 DIM as bool deleting = NO
 '--Can't delete ID 0; must always have at least one NPC
 IF id > 0 AND id = UBOUND(npc) THEN deleting = YES

 IF yesno(IIF(uses, "Done. ", "") & "Really " & IIF(deleting, "delete", "wipe clean") & " this NPC definition?", NO, NO) = NO THEN EXIT SUB

 CleanNPCDefinition npc(id)
 IF deleting THEN
  REDIM PRESERVE npc(UBOUND(npc) - 1)
 END IF

END SUB

'This is the top-level NPC editor menu (displays a list of NPCs)
SUB npcdef_editor (st as MapEditState)
DIM byref map as MapData = st.map

' Copied NPC buffer. can be used to copy NPC definitions also between maps
STATIC copied_npcdef as NPCType
STATIC have_copied_npcdef as bool = NO

DIM boxpreview(UBOUND(map.npc_def)) as string
DIM need_update_selected as bool = NO

DIM state as MenuState
state.top = -1
state.first = -1
state.spacing = 25
state.has_been_drawn = YES ' fake this because we don't call standardmenu()

state.last = UBOUND(map.npc_def)
'--Add "Add new NPC" option to end
state.last += 1

FOR i as integer = 0 TO UBOUND(map.npc_def)
 boxpreview(i) = npc_preview_text(map.npc_def(i))
NEXT i
setkeys YES
DO
 setwait 55
 setkeys YES
 state.size = vpages(dpage)->h \ 25 - 1
 state.tog = state.tog XOR 1
 IF keyval(scESC) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help "pick_npc_to_edit"
 intgrabber state.pt, -1, state.last, , , , NO  'use_clipboard=NO
 corners_to_rect XY(0,0), vpages(dpage)->size, state.rect
 usemenu state
 IF enter_space_click(state) THEN
  IF state.pt = -1 THEN
   EXIT DO
  ELSEIF state.pt = state.last THEN
   '--Add new NPC option
   REDIM PRESERVE map.npc_def(UBOUND(map.npc_def) + 1)
   CleanNPCDefinition map.npc_def(UBOUND(map.npc_def))
  ELSE
   '--An NPC
   mapedit_edit_npcdef st, map.npc_def(state.pt)
   setkeys
  END IF
  need_update_selected = YES
 END IF
 IF keyval(scPlus) > 1 THEN
  '--Fast add button (for people who really want ID 134 for a task)
  state.pt = UBOUND(map.npc_def) + 1
  REDIM PRESERVE map.npc_def(state.pt)
  CleanNPCDefinition map.npc_def(state.pt)
  need_update_selected = YES
 END IF
 IF keyval(scDelete) > 1 THEN
  '--This updates arrays, but not state.pt
  handle_npc_def_delete map.npc_def(), state.pt, map.npc()
  IF state.pt > UBOUND(map.npc_def) THEN
   '--Deleted last NPC def
   state.pt = UBOUND(map.npc_def)
  END IF
  need_update_selected = YES
 END IF
 'Copy/paste
 IF state.pt >= 0 ANDALSO state.pt < state.last THEN  'An NPC def is selected
  IF copy_keychord() THEN
   copied_npcdef = map.npc_def(state.pt)
   have_copied_npcdef = YES
  END IF
  IF paste_keychord() ANDALSO have_copied_npcdef THEN
   map.npc_def(state.pt) = copied_npcdef
   IF copied_npcdef.usetag THEN  'onetime tag
    IF twochoice("Copied NPC has a one-time-use tag set", "Replace the tag with a new one", _
                 "Use the same tag for the copy") <> 1 THEN
     'First save NPCs so that we can correctly search for unused one-time use tags (see onetimetog)
     SaveNPCD maplumpname(map.id, "n"), map.npc_def()
     onetimetog map.npc_def(state.pt).usetag
     onetimetog map.npc_def(state.pt).usetag
    END IF
   END IF
   need_update_selected = YES
  END IF
 END IF

 IF need_update_selected THEN
  '--Note not all, or even any, of these updates will be required in a given case
  load_npc_graphics map.npc_def(), st.npc_img()
  '--Update box preview line
  REDIM PRESERVE boxpreview(UBOUND(map.npc_def))
  boxpreview(state.pt) = npc_preview_text(map.npc_def(state.pt))
  '--Update menu size (including Add NPC option) and scroll up if needed
  state.last = UBOUND(map.npc_def) + 1
  correct_menu_state state

  need_update_selected = NO
 END IF

 clearpage dpage
 DIM textcol as integer
 DIM textbg as integer
 DIM y as integer
 FOR i as integer = state.top TO state.top + state.size
  IF i > state.last THEN EXIT FOR
  textcol = uilook(uiMenuItem)
  textbg = 0
  IF state.hover = i THEN textcol = uilook(uiMouseHoverItem)
  IF state.pt = i THEN textcol = uilook(uiSelectedItem + state.tog)
  textcolor textcol, textbg
  y = (i - state.top) * state.spacing
  'Selection box
  IF state.hover = i THEN fuzzyrect 0, y, rWidth, 22, uilook(uiDisabledItem), dpage, 25
  IF state.pt = i THEN edgebox 0, y, rWidth, 22, uilook(uiDisabledItem), uilook(uiMenuItem), dpage
  'Special menu items
  IF i = -1 THEN
   printstr "Previous Menu", 0, y + 5, dpage
  ELSEIF i = state.last THEN
   '--Add new NPC option
   printstr "Add new NPC", 0, y + 5, dpage
  ELSE
   '--An NPC
   printstr "" & i, 0, y + 5, dpage
   WITH st.npc_img(i)
    '--Down A frame
    frame_draw .sprite + 4, .pal, 32, (i - state.top) * 25, 1, -1, dpage
   END WITH
   textcol = uilook(uiMenuItem)
   textbg = uilook(uiHighlight)
   IF state.hover = i THEN textcol = uilook(uiMouseHoverItem)
   IF state.pt = i THEN textcol = uilook(uiText)
   textcolor textcol, textbg
   printstr boxpreview(i), 56, y + 5, dpage
  END IF
 NEXT i
 draw_fullscreen_scrollbar state, , dpage
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP

END SUB
