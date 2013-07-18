#IFNDEF CUSTOM_UDTS_BI
#DEFINE CUSTOM_UDTS_BI

'This file contains UDTs that only get used in custom mode, and not in game,
'so as to prevent them from cluttering up the global udts.bi file

#include "slices.bi"

ENUM ToolIDs
  'These are the tools available in the sprite, tile and map editors
  draw_tool
  box_tool
  line_tool
  fill_tool
  oval_tool
  airbrush_tool
  mark_tool
  clone_tool
  replace_tool
  scroll_tool
  SPRITEEDITOR_NUM_TOOLS

  paint_tool = SPRITEEDITOR_NUM_TOOLS
  NUM_TOOLS
END ENUM

TYPE SpriteEditStatic
  clonemarked as integer
  clonepos as XYPair
  clonebuf(2561) as integer 'Needs to be big enough for 2+w*h*sets/4 for the largest possible sprite set
  spriteclip(2561) as integer 'Needs to be big enough for 2+w*h*sets/4 for the largest possible sprite set
  clipsize as XYPair
  paste as integer
END TYPE

TYPE SpriteEditState
  '--sprite set state
  spritefile as string
  fileset as integer
  framenum as integer
  wide as integer
  high as integer
  perset as integer
  size as integer ' In bytes, two pixels per byte
  setsize as integer ' In bytes, two pixels per byte
  at_a_time as integer 'Number of sprite sets that fit on the browsing screen
  fullset as integer
 
  '--sprite editor state
  zoom as integer
  x as integer
  y as integer
  lastcpos as XYPair '.x/.y (cursor position) last tick
  lastpos as XYPair  'something totally different
  zonenum as integer
  zone as XYPair
  zonecursor as integer
  gotmouse as integer
  drawcursor as integer
  tool as integer
  curcolor as integer ' Index in master palette
  palindex as integer ' Index in 16 color palette
  hidemouse as integer
  airsize as integer
  mist as integer
  hold as integer
  holdpos as XYPair
  radius as double
  ellip_minoraxis as double '--For non-circular elipses. Not implemented yet
  ellip_angle as double
  undodepth as integer
  undoslot as integer
  undomax as integer
  didscroll as integer  'have scrolled since selecting the scroll tool
  delay as integer
  movespeed as integer
  readjust as integer
  adjustpos as XYPair
  previewpos as XYPair
  nulpal(8) as integer '--nulpal is used for getsprite and can go away once we convert to use Frame
  clippedpal as integer
END TYPE

TYPE TileCloneBuffer
  exists as integer
  buf(19,19) as UBYTE
  size as XYPair
  offset as XYPair
END TYPE

TYPE TileEditState
  tilesetnum as integer
  drawframe as Frame Ptr  '--Don't write to this! It's for display only
  x as integer
  y as integer
  lastcpos as XYPair  '.x/.y (cursor position) last tick
  tilex as integer  'on the tileset (measured in tiles)
  tiley as integer
  gotmouse as integer
  drawcursor as integer
  tool as integer
  curcolor as integer
  hidemouse as integer
  radius as double
  airsize as integer
  mist as integer
  undo as integer
  allowundo as integer
  zone as integer
  justpainted as integer
  hold as integer
  holdpos as XYPair
  cutfrom as integer
  cuttileset as integer
  canpaste as integer
  delay as integer
  readjust as integer
  adjustpos as XYPair
  didscroll as integer  'have scrolled since selecting the scroll tool
  defaultwalls as integer vector  'always length 160
END TYPE

TYPE HeroEditState
  changed as integer
  previewframe as integer
  battle    as GraphicPair
  walkabout as GraphicPair
  portrait  as GraphicPair
  preview_steps as integer
  preview_walk_direction as integer
  preview_walk_pos as XYPair
END TYPE

TYPE TextboxEditState
  id as integer
  portrait as GraphicPair
  search as string
END TYPE

TYPE TextboxConnectNode
  lines(2) as string
  id as integer 'ID of box or < 0 for script
  style as integer
  add as integer 'NO normally. YES if this is for adding a new box
END TYPE

ENUM MapEditMode
  tile_mode
  pass_mode
  door_mode
  npc_mode
  foe_mode
  zone_mode
END ENUM

ENUM ZoneEditSubmode
  zone_edit_mode
  zone_view_mode
END ENUM

'MapIDs used for undo steps
'FIXME:a bit of a mess, clean up later
ENUM MapID
  mapIDMetaBEGIN = -11
  mapIDMetaCursor = -11    'Stores cursor position at beginning of stroke
  mapIDMetaEditmode = -10  'to -1. .value is mode specific.
  mapIDMetaEditmodeEND = -1
  mapIDZone = 0   'to 9999
  mapIDPass = 10000
  mapIDFoe = 10001
  mapIDLayer = 10002  'to 10099
END ENUM

TYPE MapEditUndoTile
  x as ushort
  y as ushort
  value as short
  mapid as short  'as MapID
END TYPE

DECLARE_VECTOR_OF_TYPE(MapEditUndoTile, MapEditUndoTile)
DECLARE_VECTOR_OF_TYPE(MapEditUndoTile vector, MapEditUndoTile_vector)

TYPE MapEditStateFwd as MapEditState

TYPE FnBrush as SUB (st as MapEditStateFwd, byval x as integer, byval y as integer, byval value as integer = -1, byval extraarg as integer = -1, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap)
TYPE FnReader as FUNCTION (st as MapEditStateFwd, byval x as integer, byval y as integer, byval extraarg as integer = -1, map() as TileMap, pass as TileMap, emap as TileMap, zmap as ZoneMap) as integer

TYPE MapEditState
  'This NPC stuff shouldn't be here; this is the Editor state, not a map TYPE
  npc_def(max_npc_defs - 1) as NPCType
  num_npc_defs as integer
  npc_inst(299) as NPCInst

  mapnum as integer          'Map ID number
  editmode as integer        'ENUM MapEditMode
  seteditmode as integer     'Normally -1, set to an editmode to cause a switch
  x as integer               'Cursor position, in tiles
  y as integer
  mapx as integer            'Camera position (top left of viewable area), in pixels
  mapy as integer
  wide as integer            'Map size
  high as integer
  layer as integer
  defpass as bool            'Default passability ON/OFF
  cur_foe as integer         'Formation set selected for placement
  cur_npc as integer         'NPC ID selected for placement
  cur_door as integer        'Door number selected
  usetile(0 to maplayerMax) as integer  'Tile selected for each layer
  menubarstart(0 to maplayerMax) as integer
  menubar as TileMap
  tilesetview as TileMap
  cursor as GraphicPair
  tilesets(maplayerMax) as TilesetData ptr  'Tilesets is fixed size at the moment. It must always be at least as large as the number of layers on a map
  defaultwalls as integer vector vector  'indexed by layer (variable length) and then by tile (always 0-159)
  menustate as MenuState     'The top-level menu state
  temptilemap as TileMap     'A temporary TileMap. Normally remains uninitialised
  moved as integer          'used when detecting cursor movement
  walk as integer           'used for animating NPC walking
  
  'Most of these could become locals after mapping gosub is broken into real subs
  tiny as bool               'whether or not to show the tiny screen relative to map area
  anim_newtile as integer   'used when setting animated tiles
  anim_old as integer       'used when setting animated tiles
  npc_d as integer          'used when placing npcs
  oldpos as XYPair           'used when detecting cursor movement
  oldrel as XYPair           'used when detecting cursor movement
  rate as XYPair             'used for fast cursor movement when holding shift
  replace_old as integer    'used for replacing tiles
  jig as XYPair              'used for display jiggling
  wallmap_mask as integer    'used by wallbitsbrush

  message as string          'Message shown at the top of the screen
  message_ticks as integer   'Remaining ticks to display message

  'Tool stuff
  tool as integer            'Tool ID (index in toolinfo), or -1 if none (meaning none available)
  brush as FnBrush           'What to draw with
  reader as FnReader         'What to read with
  tool_value as integer      'Value (eg. tile) with which to draw. Should never be -1.
  reset_tool as bool         'When true, tool_value should be set to some default
  tool_hold as bool          'True if one coordinate has been selected
  tool_hold_pos as XYPair    'Held coordinate
  last_pos as XYPair         'Position of the cursor last tick
  new_stroke as bool         'True before beginning a new editing operation (group of brush() calls)
  history as MapEditUndoTile vector vector   'Vector of groups of tile edits
  history_size as integer    'Size of history, in number of MapEditUndoTiles (each is 8 bytes)
  history_step as integer    'In history, [0, history_step) are undos, and the rest are redos
  secondary_undo_buffer as MapEditUndoTile vector
                             'Usually NULL. If not, undo steps are added to this vector instead
                             'of the history. Used for previewing undoable changes.

  'Mark+clone
  cloned as MapEditUndoTile vector  'Cloned brush. NULL if none. Offsets are 0,0 at topleft of brush
  clone_offset as XYPair     'Handle point on the clone brush
  clone_size as XYPair       'Size of the cloned brush in tiles
  multitile_draw_brush as bool 'When true and clone tool is in use the clone brush is meant to act like
                               'the draw tool: changing map layer or .tool_value resets the tool to Draw

  'Zone stuff (st.editmode = zone_mode)
  zonesubmode as ZoneEditSubmode
  cur_zone as integer        'Zone ID selected for placement
  cur_zinfo as ZoneInfo ptr  '== GetZoneInfo(zonemaps, cur_zone)
  zones_needupdate as integer
  zoneviewmap as TileMap     'Each bit indicates one of 8 currently displayed zones
  zoneoverlaymap as TileMap  'For other overlays drawn by zonemode
  zoneminimap as Frame ptr   '1/20x zoomed view of cur_zone
  zoneviewtileset as integer 'Which of zonetileset() to use to draw zoneviewmap
  autoshow_zones as integer  'Zones at current tile become visible ("Autoshow zones")
  showzonehints as integer   'Display 'hints' where nonvisible zones are ("Show other")
  zonecolours(7) as integer  'The zone assigned to each colour, or 0. Includes "memories" of zones not currently displayed
  'Zone stuff (npc_mode)
  cur_npc_zone as integer    'Movement zone for currently selected NPC in NPC placer
  cur_npc_wall_zone as integer 'Avoidance zone for currently selected NPC in NPC placer

END TYPE

TYPE MapResizeState
  menu as MenuDef
  rect as RectType
  oldsize as XYPair
  zoom as integer
  minimap as Frame Ptr
END TYPE

TYPE AttackChainBrowserState
 root as Slice Ptr
 lbox as Slice Ptr
 rbox as Slice Ptr
 current as Slice Ptr
 after as MenuState
 before as MenuState
 chainfrom(50) as Slice Ptr 'FIXME: when FreeBasic types support resizeable arrays, this would be a great place to use one
 chainto(2) as Slice Ptr
 column as integer
 refresh as integer
 focused as Slice Ptr
 done as integer
END TYPE

TYPE ShopEditState
 id as integer
 st as MenuState
 name as string
 menu(24) as string
 shaded(24) as bool
 havestuf as bool
END TYPE

TYPE ShopStuffState
 st as MenuState
 thing as integer       'Hero/item ID number
 thingname as string
 default_thingname as string
 item_value as integer  'Garbage if thing is a hero
 menu(24) as string
 max(24) as integer
 min(24) as integer
END TYPE

TYPE MouseArea
  x as integer
  y as integer
  w as integer
  h as integer
  hidecursor as integer
END TYPE

TYPE ToolInfoType
  name as string
  icon as string
  shortcut as integer
  cursor as integer
  areanum as integer
END TYPE

#ENDIF
