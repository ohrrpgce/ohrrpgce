#IFNDEF CUSTOM_UDTS_BI
#DEFINE CUSTOM_UDTS_BI

'This file contains UDTs that only get used in custom mode, and not in game,
'so as to prevent them from cluttering up the global udts.bi file

#include "slices.bi"

ENUM PixelTool
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
  NUM_TOOLS
END ENUM

TYPE SpriteEditStatic
  clonemarked AS INTEGER
  clonebuf(2561) AS INTEGER 'Needs to be big enough for 2+w*h*sets/4 for the largest possible sprite set
  spriteclip(2561) AS INTEGER 'Needs to be big enough for 2+w*h*sets/4 for the largest possible sprite set
  clipsize AS XYPair
  paste AS INTEGER
END TYPE

TYPE SpriteEditState
  '--sprite set state
  spritefile AS STRING
  fileset AS INTEGER
  framenum AS INTEGER
  wide AS INTEGER
  high AS INTEGER
  perset AS INTEGER
  size AS INTEGER ' In bytes, two pixels per byte
  setsize AS INTEGER ' In bytes, two pixels per byte
  at_a_time AS INTEGER 'Number of sprite sets that fit on the browsing screen
  fullset AS INTEGER
 
  '--sprite editor state
  zoom AS INTEGER
  x AS INTEGER
  y AS INTEGER
  lastcpos AS XYPair '.x/.y (cursor position) last tick
  lastpos AS XYPair  'something totally different
  zonenum AS INTEGER
  zone AS XYPair
  zonecursor AS INTEGER
  gotmouse AS INTEGER
  drawcursor AS INTEGER
  tool AS INTEGER
  curcolor AS INTEGER ' Index in master palette
  palindex AS INTEGER ' Index in 16 color palette
  hidemouse AS INTEGER
  airsize AS INTEGER
  mist AS INTEGER
  hold AS INTEGER
  holdpos AS XYPair
  radius AS DOUBLE
  ellip_minoraxis AS DOUBLE '--For non-circular elipses. Not implemented yet
  ellip_angle AS DOUBLE
  undodepth AS INTEGER
  undoslot AS INTEGER
  undomax AS INTEGER
  didscroll AS INTEGER  'have scrolled since selecting the scroll tool
  delay AS INTEGER
  movespeed AS INTEGER
  readjust AS INTEGER
  adjustpos AS XYPair
  previewpos AS XYPair
  nulpal(8) AS INTEGER '--nulpal is used for getsprite and can go away once we convert to use Frame
  clippedpal AS INTEGER
END TYPE

TYPE TileCloneBuffer
  exists AS INTEGER
  buf(19,19) AS UBYTE
  size AS XYPair
END TYPE

TYPE TileEditState
  drawframe AS Frame Ptr  '--Don't write to this! It's for display only
  x AS INTEGER
  y AS INTEGER
  lastcpos AS XYPair  '.x/.y (cursor position) last tick
  tilex AS INTEGER  'on the tileset (measured in tiles)
  tiley AS INTEGER
  gotmouse AS INTEGER
  drawcursor AS INTEGER
  tool AS INTEGER
  curcolor AS INTEGER
  hidemouse AS INTEGER
  radius AS DOUBLE
  airsize AS INTEGER
  mist AS INTEGER
  undo AS INTEGER
  allowundo AS INTEGER
  zone AS INTEGER
  justpainted AS INTEGER
  hold AS INTEGER
  hox AS INTEGER
  hoy AS INTEGER
  cutfrom AS INTEGER
  cuttileset AS INTEGER
  canpaste AS INTEGER
  delay AS INTEGER
  readjust AS INTEGER
  adjustpos AS XYPair
  didscroll AS INTEGER  'have scrolled since selecting the scroll tool
  defaultwalls AS INTEGER VECTOR  'always length 160
END TYPE

TYPE HeroEditState
  changed AS INTEGER
  previewframe AS INTEGER
  battle    AS GraphicPair
  walkabout AS GraphicPair
  portrait  AS GraphicPair
  preview_steps AS INTEGER
  preview_walk_direction AS INTEGER
  preview_walk_pos AS XYPair
END TYPE

TYPE TextboxEditState
  id AS INTEGER
  portrait AS GraphicPair
  search AS STRING
END TYPE

TYPE TextboxConnectNode
  lines(2) AS STRING
  id AS INTEGER 'ID of box or < 0 for script
  style AS INTEGER
  add AS INTEGER 'NO normally. YES if this is for adding a new box
END TYPE

ENUM MapEditMode
  tile_mode
  pass_mode
  door_mode
  npc_mode
  foe_mode
  zone_mode
END ENUM

TYPE MapEditState
  'This NPC stuff shouldn't be here; this is the Editor state, not a map TYPE
  npc_def(max_npc_defs - 1) AS NPCType
  num_npc_defs AS INTEGER
  npc_inst(299) AS NPCInst

  tilepick AS XYPair  'Coordinates (in tiles) of the selected tile on the tile picker screen
  layer AS INTEGER
  defpass AS INTEGER  'Default passability ON/OFF
  cur_foe AS INTEGER  'Formation set selected for placement
  cur_npc AS INTEGER  'NPC ID selected for placement
  usetile(0 to maplayerMax) AS INTEGER  'Tile selected for each layer
  menubarstart(0 to maplayerMax) AS INTEGER
  menubar AS TileMap
  tilesetview AS TileMap
  cursor AS GraphicPair
  tilesets(maplayerMax) as TilesetData ptr  'Tilesets is fixed size at the moment. It must always be at least as large as the number of layers on a map
  defaultwalls AS INTEGER VECTOR VECTOR  'indexed by layer (variable length) and then by tile (always 0-159)
  menustate AS MenuState  'The top-level menu state

  'Zone stuff (zone_mode)
  zonesubmode AS INTEGER
  cur_zone AS INTEGER        'Zone ID selected for placement
  cur_zinfo AS ZoneInfo ptr  '== GetZoneInfo(zonemaps, cur_zone)
  zoneviewmap AS TileMap     'Each bit indicates one of 8 currently displayed zones
  zoneoverlaymap AS TileMap  'For other overlays drawn by zonemode
  zoneminimap AS Frame ptr   '1/20x zoomed view of cur_zone
  zoneviewtileset AS INTEGER 'Which of zonetileset() to use to draw zoneviewmap
  autoshow_zones AS INTEGER  'Zones at current tile become visible ("Autoshow zones")
  showzonehints AS INTEGER   'Display 'hints' where nonvisible zones are ("Show other")
  zonecolours(7) AS INTEGER  'The zone assigned to each colour, or 0. Includes "memories" of zones not currently displayed
  'Zone stuff (npc_mode)
  cur_npc_zone AS INTEGER    'Movement zone for currently selected NPC in NPC placer

END TYPE

TYPE MapResizeState
  menu AS MenuDef
  rect AS RectType
  oldsize AS XYPair
  zoom AS INTEGER
  minimap AS Frame Ptr
END TYPE

TYPE AttackChainBrowserState
 root AS Slice Ptr
 lbox AS Slice Ptr
 rbox AS Slice Ptr
 current AS Slice Ptr
 after AS MenuState
 before AS MenuState
 chainfrom(50) AS Slice Ptr 'FIXME: when FreeBasic types support resizeable arrays, this would be a great place to use one
 chainto(2) AS Slice Ptr
 column AS INTEGER
 refresh AS INTEGER
 focused AS Slice Ptr
 done AS INTEGER
END TYPE

#ENDIF
