'OHRRPGCE CUSTOM - Miscellaneous unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z

#include "compat.bi"
#include "const.bi"
#include "udts.bi"
#include "custom_udts.bi"

'external subs and functions
DECLARE SUB npcdef (npc() AS NPCType, npc_img() AS GraphicPair, BYREF num_npc_defs AS INTEGER, npc_insts() AS NPCInst)

'local subs and functions
DECLARE SUB make_map_picker_menu (topmenu() AS STRING, state AS MenuState)
DECLARE SUB mapeditor (BYVAL mapnum AS INTEGER)
DECLARE FUNCTION addmaphow () AS INTEGER
DECLARE FUNCTION animadjust% (tilenum%, tastuf%())
DECLARE SUB loadpasdefaults (array() AS INTEGER, tilesetnum AS INTEGER)
DECLARE SUB paint_map_area(st AS MapEditState, oldTile, x%, y%, map() AS TileMap, pass AS TileMap, defaults() AS DefArray, defpass%)

TYPE LayerMenuItem
 layernum AS INTEGER '-1 if not a layer
 gmapindex AS INTEGER '-1 if enabled/visibility choice rather than tileset choice
END TYPE

DECLARE Function LayerIsVisible(vis() as integer, byval l as integer) as integer
DECLARE Function LayerIsEnabled(gmap() as integer, byval l as integer) as integer
DECLARE Sub SetLayerVisible(vis() as integer, byval l as integer, byval v as integer)
DECLARE Sub SetLayerEnabled(gmap() as integer, byval l as integer, byval v as integer)
DECLARE Sub ToggleLayerVisible(vis() as integer, byval l as integer)
DECLARE Sub ToggleLayerEnabled(vis() as integer, byval l as integer)

DECLARE SUB DrawDoorPair(BYREF st AS MapEditState, curmap as integer, cur as integer, map() AS TileMap, pass AS TileMap, doors() as door, link() as doorlink, gmap())

DECLARE SUB calculatepassblock(BYREF st AS MapEditState, x AS INTEGER, y AS INTEGER, map() AS TileMap, pass AS TileMap, defaults() AS DefArray)

DECLARE SUB resizemapmenu (BYREF st AS MapEditState, map() AS TileMap, BYREF rs AS MapResizeState)
DECLARE SUB resizetiledata OVERLOAD (tmap AS TileMap, rs AS MapResizeState, BYREF yout AS INTEGER, page AS INTEGER)
DECLARE SUB resizetiledata OVERLOAD (tmaps() AS TileMap, rs AS MapResizeState, BYREF yout AS INTEGER, page AS INTEGER)
DECLARE SUB resizetiledata OVERLOAD (tmap AS TileMap, x_off AS INTEGER, y_off AS INTEGER, new_width AS INTEGER, new_height AS INTEGER, BYREF yout AS INTEGER, page AS INTEGER)

DECLARE SUB update_tilepicker(BYREF st AS MapEditState)
DECLARE SUB verify_map_size (mapnum AS INTEGER, BYREF wide AS INTEGER, BYREF high AS INTEGER, map() AS TileMap, pass AS TileMap, emap AS TileMap, mapname AS STRING)
DECLARE SUB add_more_layers(map() as TileMap, vis() AS INTEGER, gmap() AS INTEGER, BYVAL numlayers AS INTEGER)
DECLARE SUB fix_tilemaps(map() as TileMap)
DECLARE SUB mapedit_loadmap (BYREF st AS MapEditState, mapnum AS INTEGER, BYREF wide AS INTEGER, BYREF high AS INTEGER, map() AS TileMap, pass AS TileMap, emap AS TileMap, gmap() AS INTEGER, visible() AS INTEGER, doors() AS Door, link() AS DoorLink, defaults() AS DefArray, mapname AS STRING)
DECLARE SUB mapedit_savemap (BYREF st AS MapEditState, mapnum AS INTEGER, map() AS TileMap, pass AS TileMap, emap AS TileMap, gmap() AS INTEGER, doors() AS Door, link() AS DoorLink, mapname AS STRING)
DECLARE SUB new_blank_map (BYREF st AS MapEditState, map() AS TileMap, pass AS TileMap, emap AS TileMap, gmap() AS INTEGER, doors() AS Door, link() AS DoorLink)
DECLARE SUB mapedit_addmap()
DECLARE SUB mapedit_resize(BYREF st AS MapEditState, mapnum AS INTEGER, BYREF wide AS INTEGER, BYREF high AS INTEGER, BYREF x AS INTEGER, BYREF y AS INTEGER, BYREF mapx AS INTEGER, BYREF mapy AS INTEGER, map() AS TileMap, pass AS TileMap, emap AS TileMap, gmap() AS INTEGER, doors() AS Door, link() AS DoorLink, mapname AS STRING)
DECLARE SUB mapedit_delete(BYREF st AS MapEditState, mapnum AS INTEGER, BYREF wide AS INTEGER, BYREF high AS INTEGER, BYREF x AS INTEGER, BYREF y AS INTEGER, BYREF mapx AS INTEGER, BYREF mapy AS INTEGER, map() AS TileMap, pass AS TileMap, emap AS TileMap, doors() AS Door, link() AS DoorLink)
DECLARE SUB link_one_door(BYREF st AS MapEditState, mapnum AS INTEGER, linknum AS INTEGER, link() AS DoorLink, doors() AS Door, map() AS TileMap, pass AS TileMap, gmap() AS INTEGER)
DECLARE SUB mapedit_linkdoors (BYREF st AS MapEditState, mapnum AS INTEGER, map() AS TileMap, pass AS TileMap, emap AS TileMap, gmap() AS INTEGER, doors() AS Door, link() AS DoorLink, mapname AS STRING)
DECLARE SUB mapedit_layers (BYREF st AS MapEditState, gmap() AS INTEGER, visible() AS INTEGER, defaults() AS DefArray, map() AS TileMap)
DECLARE SUB mapedit_makelayermenu(BYREF st AS MapEditState, menu() AS SimpleMenu, state AS MenuState, gmap() AS INTEGER, BYREF currentset AS INTEGER, visible() AS INTEGER, map() AS TileMap, itemsinfo() AS LayerMenuItem, BYVAL resetpt AS INTEGER)
DECLARE SUB mapedit_insert_layer(BYREF st AS MapEditState, map() as TileMap, vis() AS INTEGER, gmap() AS INTEGER, BYVAL where AS INTEGER)
DECLARE SUB mapedit_delete_layer(BYREF st AS MapEditState, map() as TileMap, vis() AS INTEGER, gmap() AS INTEGER, BYVAL which AS INTEGER)
DECLARE SUB mapedit_swap_layers(BYREF st AS MapEditState, map() as TileMap, vis() AS INTEGER, gmap() AS INTEGER, BYVAL l1 AS INTEGER, BYVAL l2 AS INTEGER)
DECLARE SUB mapedit_gmapdata(BYREF st AS MapEditState, gmap() AS INTEGER)

DECLARE FUNCTION find_last_used_doorlink(link() AS DoorLink) AS INTEGER
DECLARE FUNCTION find_door_at_spot (x AS INTEGER, y AS INTEGER, doors() AS Door) AS INTEGER
DECLARE FUNCTION find_first_free_door (doors() AS Door) AS INTEGER
DECLARE FUNCTION find_first_doorlink_by_door(doornum AS INTEGER, link() AS DoorLink) AS INTEGER

DECLARE SUB resize_rezoom_mini_map(BYREF st AS MapEditState, BYREF rs AS MapResizeState, map() AS TileMap)
DECLARE SUB show_minimap(BYREF map AS MapEditState, map() AS TileMap)
DECLARE SUB mapedit_pickblock(BYREF st AS MapEditState)
DECLARE SUB resize_buildmenu(BYREF rs AS MapResizeState)
DECLARE SUB resize_dimchange(BYREF st AS MapEditState, BYREF rs AS MapResizeState, map() AS TileMap)
DECLARE SUB resize_correct_width(BYREF st AS MapEditState, BYREF rs AS MapResizeState, map() AS TileMap)
DECLARE SUB resize_correct_height(BYREF st AS MapEditState, BYREF rs AS MapResizeState, map() AS TileMap)

#include "allmodex.bi"
#include "common.bi"
#include "customsubs.bi"
#include "cglobals.bi"

#include "scrconst.bi"
#include "loading.bi"

REM $STATIC

FUNCTION addmaphow () AS INTEGER
'--Return values
'  -2  =Cancel
'  -1  =New blank
'  >=0 =Copy

DIM menu(2) AS STRING
DIM maptocopy AS INTEGER = 0
DIM state AS MenuState
state.last = UBOUND(menu)
state.size = 24

state.need_update = YES
setkeys
DO
 setwait 55
 setkeys
 IF keyval(scESC) > 1 THEN
  '--return cancel
  RETURN -2
 END IF
 IF keyval(scF1) > 1 THEN show_help "add_map_how"
 usemenu state
 IF state.pt = 2 THEN
  IF intgrabber(maptocopy, 0, gen(genMaxMap)) THEN state.need_update = YES
 END IF
 IF enter_or_space() THEN
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
  menu(2) = "Copy of map " & maptocopy & " " & getmapname$(maptocopy)
 END IF
 clearpage vpage
 standardmenu menu(), state, 0, 0, vpage
 setvispage vpage
 dowait
LOOP
END FUNCTION

FUNCTION animadjust (tilenum, tastuf())
'given a tile number and the tile-animation data,
'adjusts to make sure the tile is non-animated
pic = tilenum
IF pic >= 208 THEN pic = (pic - 208) + tastuf(20)
IF pic >= 160 THEN pic = (pic - 160) + tastuf(0)
animadjust = pic
END FUNCTION

SUB make_map_picker_menu(topmenu() AS STRING, state AS MenuState)
 REDIM topmenu(0)
 topmenu(0) = "Return to Main Menu"
 FOR i = 0 TO gen(genMaxMap)
  str_array_append topmenu(), "Map " + filenum(i) + ": " + getmapname(i)
 NEXT
 str_array_append topmenu(), "Add a New Map"

 state.size = 24
 state.last = UBOUND(topmenu)
END SUB

SUB map_picker ()
 DIM topmenu() AS STRING
 DIM state AS MenuState
 make_map_picker_menu topmenu(), state

 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "mapedit_choose_map"
  usemenu state

  IF enter_or_space() THEN
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
  standardmenu topmenu(), state, 0, 0, vpage
  setvispage vpage
  dowait
 LOOP
 clearpage 0
 clearpage 1
END SUB

SUB mapeditor (BYVAL mapnum AS INTEGER)
STATIC remember_menu_pt AS INTEGER = 0

DIM st AS MapEditState
DIM modenames(4) AS STRING, mapeditmenu(13) AS STRING, gmap(dimbinsize(binMAP)), pal16(288), npcnum(max_npc_defs - 1)
DIM her AS HeroDef
DIM defaults(maplayerMax) AS DefArray

REDIM doors(99) AS door, link(199) AS doorlink

DIM as integer jiggle(maplayerMax \ 16)
DIM as integer visible(maplayerMax \ 16) = {-1} 'used as bitsets: all layers visible

'FIXME: heroimg is a hack and should be replaced with a GraphicPair object
DIM heroimg(102), heropal(8)

'npcdef assumes that npc_img is sized (0 TO max_npc_defs - 1), just like st.npc()
DIM npc_img(max_npc_defs - 1) AS GraphicPair

REDIM map(0) AS TileMap ' dummy empty map data, will be resized later
DIM pass AS TileMap
DIM emap AS TileMap

DIM foe AS INTEGER = 0 ' Formation number for foemapping mode

DIM defpass_reload_confirm(1) AS STRING

textcolor uilook(uiText), 0

wide = 0: high = 0: nptr = 0
mapname$ = ""
DIM xtemp AS STRING, temp AS INTEGER

'--create a palette for the cursor
st.cursor.pal = palette16_new()
'set the colors that actually get used
st.cursor.pal->col(1) = uilook(uiText)
st.cursor.pal->col(2) = uilook(uiMenuItem)

'--create cursor
' the colors here are actually offsets into the 16-color palette.
' see the st.cursor.pal construction above
st.cursor.sprite = frame_new(20, 20, 2, YES)

rectangle 0, 0, 20, 20, 1, st.cursor.sprite
rectangle 1, 1, 18, 18, 0, st.cursor.sprite
rectangle 2, 2, 16, 16, 2, st.cursor.sprite
rectangle 3, 3, 14, 14, 0, st.cursor.sprite

rectangle 0, 0, 20, 20, 1, st.cursor.sprite + 1
rectangle 1, 1, 18, 18, 0, st.cursor.sprite + 1
rectangle 3, 3, 14, 14, 2, st.cursor.sprite + 1
rectangle 4, 4, 12, 12, 0, st.cursor.sprite + 1

modenames(0) = "Picture Mode"
modenames(1) = "Passability Mode"
modenames(2) = "Door Placement Mode"
modenames(3) = "NPC Placement Mode"
modenames(4) = "Foe Mapping Mode"

cleantilemap st.menubar, 160, 1
cleantilemap st.tilesetview, 16, 10
FOR i = 0 TO 159
 writeblock st.menubar, i, 0, i
 writeblock st.tilesetview, i MOD 16, i \ 16, i
NEXT

mapedit_loadmap st, mapnum, wide, high, map(), pass, emap, gmap(), visible(), doors(), link(), defaults(), mapname$

x = 0
y = 0
mapx = 0
mapy = 0
st.layer = 0
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
mapeditmenu(10) = "Link Doors..."
mapeditmenu(11) = "Erase Map Data"
mapeditmenu(12) = "Re-load Default Passability"
mapeditmenu(13) = "Map name:"

st.menustate.size = 24
st.menustate.last = UBOUND(mapeditmenu)
st.menustate.pt = remember_menu_pt  'preserved from any other maps for convenience

'--load NPC graphics--
FOR i = 0 TO st.num_npc_defs - 1
 'Load the picture and palette
 WITH npc_img(i)
  .sprite = frame_load(4, st.npc_def(i).picture)
  .pal    = palette16_load(st.npc_def(i).palette, 4, st.npc_def(i).picture)
 END WITH
NEXT i

setkeys
DO
 setwait 55
 setkeys
 tog = tog XOR 1
 IF keyval(scESC) > 1 THEN
  mapedit_savemap st, mapnum, map(), pass, emap, gmap(), doors(), link(), mapname$
  EXIT DO
 END IF
 IF keyval(scF1) > 1 THEN show_help "mapedit_menu"
 usemenu st.menustate
 IF enter_or_space() THEN
  SELECT CASE st.menustate.pt
   CASE 0
    mapedit_savemap st, mapnum, map(), pass, emap, gmap(), doors(), link(), mapname$
    EXIT DO
   CASE 1
    mapedit_gmapdata st, gmap()
   CASE 2
    mapedit_resize st, mapnum, wide, high, x, y, mapx, mapy, map(), pass, emap, gmap(), doors(), link(), mapname$
   CASE 3
    mapedit_layers st, gmap(), visible(), defaults(), map()
   CASE 4
    'This may change st.num_npc_defs, and delete NPC instances
    npcdef st.npc_def(), npc_img(), st.num_npc_defs, st.npc_inst()
   CASE 5 TO 9
    editmode = st.menustate.pt - 5
    GOSUB mapping
   CASE 10
    mapedit_linkdoors st, mapnum, map(), pass, emap, gmap(), doors(), link(), mapname$
   CASE 11
    mapedit_delete st, mapnum, wide, high, x, y, mapx, mapy, map(), pass, emap, doors(), link()
    IF mapnum > gen(genMaxMap) THEN
     'This was the last map, and it was deleted instead of blanked
     EXIT DO
    END IF
   CASE 12
    '--reload default passability
    defpass_reload_confirm(0) = "No, Nevermind. No passability changes"
    defpass_reload_confirm(1) = "Set default passability for whole map"
    IF sublist(defpass_reload_confirm(), "defpass_reload_confirm") = 1 THEN
     FOR tx = 0 TO wide - 1
      FOR ty = 0 TO high - 1
       calculatepassblock st, tx, ty, map(), pass, defaults()
      NEXT ty
     NEXT tx
    END IF
  END SELECT
 END IF
 IF st.menustate.pt = 13 THEN strgrabber mapname$, 39
 mapeditmenu(13) = "Map name:" + mapname$
 IF LEN(mapeditmenu(13)) > 40 THEN mapeditmenu(13) = mapname$
 
 clearpage vpage
 standardmenu mapeditmenu(), st.menustate, 0, 0, vpage
 setvispage vpage
 dowait
LOOP

''''''''''' MAP EDITOR CLEANUP CODE
'Unload NPC graphics
FOR i = 0 TO UBOUND(npc_img)
 WITH npc_img(i)
  IF .sprite THEN frame_unload(@.sprite)
  IF .pal THEN palette16_unload(@.pal)
 END WITH
NEXT i

unloadmaptilesets st.tilesets()
unloadtilemap st.menubar
unloadtilemap st.tilesetview
unloadtilemaps map()
unloadtilemap pass
unloadtilemap emap
frame_unload @(st.cursor.sprite)
palette16_unload @(st.cursor.pal)

clearpage 0
clearpage 1
clearpage 2
clearpage 3

remember_menu_pt = st.menustate.pt  'preserve for other maps
EXIT SUB


mapping:
clearpage 2
'--load hero graphics--
'FIXME this is a hack. heroimg() should be replaced with a Frame object
loadherodata @her, 0
loadrecord heroimg(), game + ".pt4", 100, her.walk_sprite * 8 + 4
fixspriterecord heroimg(), 20, 20
getpal16 heropal(), 0, her.walk_sprite_pal, 4, her.walk_sprite
defpass = 1
IF readbit(gen(), genBits, 15) THEN defpass = 0 ' option to default the defaults to OFF
doorid = 0
doorlinkid = 0

setkeys
DO
 setwait 55
 setkeys
 IF keyval(scESC) > 1 THEN EXIT DO
 if keyval(scCtrl) = 0 AND keyval(scAlt) = 0 then
  IF keyval(scF2) > 1 THEN
   editmode = 0
  END IF
  IF keyval(scF3) > 1 THEN
   editmode = 1
  END IF
  IF keyval(scF4) > 1 THEN
   editmode = 2
  END IF
  IF keyval(scF5) > 1 THEN
   editmode = 3
  END IF
  IF keyval(scF6) > 1 THEN
   editmode = 4
  END IF
 else
  for i = 1 to maplayerMax
   if keyval(scAlt) > 0 AND keyval(sc1 + (i - 1)) > 1 then
    clearkey(sc1 + i)
    togglelayerenabled(gmap(), i)
    if layerisenabled(gmap(), i) then
     if i > ubound(map) then
      temp = i - ubound(map)
      if yesno("Create " & iif_string(temp = 1, "a new map layer?", temp & " new map layers?")) then
       add_more_layers map(), visible(), gmap(), i
      end if
     end if
    else
     if st.layer = i then
      do until layerisenabled(gmap(), st.layer): st.layer -= 1: loop
     end if
    end if
   end if
  next
  #IFNDEF __UNIX__
  'common WM keys
  for i = 0 to ubound(map)
   if keyval(scCtrl) > 0 AND keyval(scF1 + i) > 1 then
    clearkey(scF1 + i)
    if layerisenabled(gmap(), i) then togglelayervisible(visible(), i)
   end if
  next
  #ENDIF
  
  if keyval(scTilde) > 1 then
   togglelayervisible(visible(), st.layer)
   clearkey(scTilde)
  end if
 end if
 
 IF keyval(scCtrl) > 0 AND keyval(scL) > 1 THEN mapedit_layers st, gmap(), visible(), defaults(), map()  'ctrl-L
 IF keyval(scTab) > 1 THEN tiny = tiny XOR 1
 IF keyval(scCtrl) > 0 AND keyval(scBackspace) > 1 THEN
   'delete tile
   FOR i = 0 TO UBOUND(map)
    writeblock map(i), x, y, 0
   NEXT i
   'delete passability
   writeblock pass, x, y, 0
   'delete foemap
   writeblock emap, x, y, 0
   'delete NPC
   FOR i = 0 TO 299
    WITH st.npc_inst(i)
     IF .id > 0 THEN
      IF .x = x * 20 AND .y = y * 20 THEN .id = 0
     END IF
    END WITH
   NEXT i
   'delete door
   doorid = find_door_at_spot(x, y, doors())
   IF doorid >= 0 THEN
    setbit doors(doorid).bits(), 0, 0, 1
   END IF
 END IF
 IF keyval(scCtrl) > 0 AND keyval(scH) > 1 THEN 'Ctrl+H for hero start position
  gen(genStartMap) = mapnum
  gen(genStartX) = x
  gen(genStartY) = y
 END IF
 SELECT CASE editmode
  '---TILEMODE------
  CASE 0
   IF keyval(scF1) > 1 THEN show_help "mapedit_tilemap"
   IF keyval(scF) > 1 AND keyval(scCtrl) > 0 THEN' Ctrl+F Fill screen
    FOR tx = 0 TO 15
     FOR ty = 0 TO 8
      writeblock map(st.layer), mapx \ 20 + tx, mapy \ 20 + ty, st.usetile(st.layer)
      IF defpass THEN calculatepassblock st, mapx \ 20 + tx, mapy \ 20 + ty, map(), pass, defaults()
     NEXT ty
    NEXT tx
   END IF
   IF keyval(scR) > 1 AND keyval(scCtrl) > 0 THEN' Ctrl+R to replace-all
    old = readblock(map(st.layer), x, y)
    FOR ty = 0 to map(st.layer).high - 1
     FOR tx = 0 to map(st.layer).wide - 1
      IF readblock(map(st.layer), tx, ty) = old THEN writeblock map(st.layer), tx, ty, st.usetile(st.layer)
     NEXT tx
    NEXT ty
   END IF
   IF keyval(scP) > 1 AND keyval(scCtrl) > 0 THEN' Ctrl+P to paint a continuous section of maptiles
    old = readblock(map(st.layer), x, y)
    paint_map_area st, old, x, y, map(), pass, defaults(), defpass
   END IF
   IF keyval(scCtrl) > 0 AND keyval(scJ) > 1 THEN
     setbit jiggle(), 0, st.layer, (readbit(jiggle(), 0, st.layer) XOR 1)
   END IF
   IF keyval(scTilde) > 1 THEN show_minimap st, map()
   IF keyval(scEnter) > 1 THEN mapedit_pickblock st
   IF keyval(scSpace) > 0 THEN
    writeblock map(st.layer), x, y, st.usetile(st.layer)
    IF defpass THEN calculatepassblock st, x, y, map(), pass, defaults()
   END IF
   IF keyval(scDelete) > 1 THEN 'delete
    writeblock map(st.layer), x, y, 0
   END IF
   IF keyval(scCapslock) > 1 THEN 'grab tile
    st.usetile(st.layer) = animadjust(readblock(map(st.layer), x, y), st.tilesets(st.layer)->tastuf())
    update_tilepicker st
   END IF
   IF keyval(scCtrl) > 0 AND keyval(scD) > 1 THEN defpass = defpass XOR 1   
   FOR i = 0 TO 1 
    IF keyval(sc1 + i) > 1 THEN 'animate tile
     newtile = -1
     old = readblock(map(st.layer), x, y)
     IF old >= 160 + i * 48 AND old < 160 + i * 48 + 48 THEN
      newtile = (old - (160 + (i * 48))) + st.tilesets(st.layer)->tastuf(i * 20)
     ELSEIF old >= st.tilesets(st.layer)->tastuf(i * 20) AND old < st.tilesets(st.layer)->tastuf(i * 20) + 48 THEN
      newtile = 160 + (i * 48) + (old - st.tilesets(st.layer)->tastuf(i * 20))
     END IF
     IF newtile >= 0 THEN
      IF keyval(scCtrl) = 0 THEN
       writeblock map(st.layer), x, y, newtile
      ELSE
       FOR tx = 0 TO wide - 1
        FOR ty = 0 TO high - 1
         IF readblock(map(st.layer), tx, ty) = old THEN writeblock map(st.layer), tx, ty, newtile
        NEXT ty
       NEXT tx
      END IF
     END IF
    END IF
   NEXT i
   IF keyval(scComma) > 1 AND st.usetile(st.layer) > 0 THEN
    st.usetile(st.layer) = st.usetile(st.layer) - 1
    update_tilepicker st
   END IF
   IF keyval(scPeriod) > 1 AND st.usetile(st.layer) < 159 THEN
    st.usetile(st.layer) = st.usetile(st.layer) + 1
    update_tilepicker st
   END IF
   '---PASSMODE-------
  CASE 1
   IF keyval(scF1) > 1 THEN show_help "mapedit_wallmap"
   over = readblock(pass, x, y)
   IF keyval(scSpace) > 1 AND (over AND 15) = 0 THEN writeblock pass, x, y, 15
   IF keyval(scSpace) > 1 AND (over AND 15) = 15 THEN writeblock pass, x, y, 0
   IF keyval(scSpace) > 1 AND (over AND 15) > 0 AND (over AND 15) < 15 THEN writeblock pass, x, y, 0
   IF keyval(scDelete) > 1 THEN 'delete
    writeblock pass, x, y, 0
   END IF
   IF keyval(scCtrl) > 0 THEN
    IF keyval(scUp) > 1 THEN writeblock pass, x, y, (over XOR 1)
    IF keyval(scRight) > 1 THEN writeblock pass, x, y, (over XOR 2)
    IF keyval(scDown) > 1 THEN writeblock pass, x, y, (over XOR 4)
    IF keyval(scLeft) > 1 THEN writeblock pass, x, y, (over XOR 8)
   END IF
   IF keyval(scA) > 1 THEN writeblock pass, x, y, (over XOR 16) 'vehicle A
   IF keyval(scB) > 1 THEN writeblock pass, x, y, (over XOR 32) 'vehicle B
   IF keyval(scH) > 1 THEN writeblock pass, x, y, (over XOR 64) 'harm tile
   IF keyval(scO) > 1 THEN writeblock pass, x, y, (over XOR 128)'overhead
   '---DOORMODE-----
  CASE 2
   IF keyval(scF1) > 1 THEN show_help "mapedit_door_placement"
   IF keyval(scEnter) > 1 THEN ' enter to link a door
    doorid = find_door_at_spot(x, y, doors())
    IF doorid >= 0 THEN
     'Save currently-worked-on map data
     mapedit_savemap st, mapnum, map(), pass, emap, gmap(), doors(), link(), mapname$
     doorlinkid = find_first_doorlink_by_door(doorid, link())
     IF doorlinkid >= 0 THEN
      link_one_door st, mapnum, doorlinkid, link(), doors(), map(), pass, gmap()
     ELSE
      doorlinkid = find_last_used_doorlink(link()) + 1
      IF doorlinkid >= 0 AND doorlinkid <= UBOUND(link) THEN
       link(doorlinkid).source = doorid
       link_one_door st, mapnum, doorlinkid, link(), doors(), map(), pass, gmap()
      END IF
     END IF
    END IF
   END IF
   IF keyval(scSpace) > 1 THEN ' space to place a door
    doorid = find_door_at_spot(x, y, doors())
    IF doorid >= 0 THEN
     'clear an existing door
     setbit doors(doorid).bits(), 0, 0, 0
    ELSE
     'place a new door
     doorid = find_first_free_door(doors())
     IF doorid >= 0 THEN
      doors(doorid).x = x
      doors(doorid).y = y + 1
      setbit doors(doorid).bits(), 0, 0, 1
     END IF
    END IF
   END IF
   IF keyval(scDelete) > 1 THEN
    doorid = find_door_at_spot(x, y, doors())
    IF doorid >= 0 THEN
     setbit doors(doorid).bits(), 0, 0, 0
    END IF
   END IF
   '---NPCMODE------
  CASE 3
   IF keyval(scF1) > 1 THEN show_help "mapedit_npc_placement"
   IF keyval(scDelete) > 1 THEN
    FOR i = 0 TO 299
     WITH st.npc_inst(i)
      IF .id > 0 THEN
       IF .x = x * 20 AND .y = y * 20 THEN .id = 0
      END IF
     END WITH
    NEXT i
   END IF
   nd = -1
   IF keyval(scCtrl) > 0 OR keyval(scSpace) > 1 THEN
    IF slowkey(scUp, 12)    THEN nd = 0
    IF slowkey(scRight, 12) THEN nd = 1
    IF slowkey(scDown, 12)  THEN nd = 2
    IF slowkey(scLeft, 12)  THEN nd = 3
   END IF
   IF keyval(scSpace) > 1 OR nd > -1 THEN
    temp = 0
    IF nd = -1 THEN
     FOR i = 0 TO 299
      WITH st.npc_inst(i)
       IF .id > 0 THEN
        IF .x = x * 20 AND .y = y * 20 THEN .id = 0: temp = 1
       END IF
      END WITH
     NEXT i
    END IF
    IF nd = -1 THEN nd = 2
    IF temp = 0 THEN
     temp = -1
     FOR i = 299 TO 0 STEP -1
      IF st.npc_inst(i).id = 0 THEN temp = i
     NEXT i
     IF temp >= 0 THEN
      st.npc_inst(temp).x = x * 20
      st.npc_inst(temp).y = y * 20
      st.npc_inst(temp).id = nptr + 1
      st.npc_inst(temp).dir = nd
     END IF
    END IF
   END IF
   intgrabber(nptr, 0, st.num_npc_defs - 1, 51, 52)
   '---FOEMODE--------
  CASE 4
   IF keyval(scF1) > 1 THEN show_help "mapedit_foemap"
   intgrabber(foe, 0, 255, 51, 52)
   IF keyval(scSpace) > 0 THEN
    writeblock emap, x, y, foe
   END IF
   IF keyval(scDelete) > 1 THEN
    writeblock emap, x, y, 0
   END IF
   IF keyval(scF) > 1 AND keyval(scCtrl) > 0 THEN
    FOR i = 0 TO 15
     FOR o = 0 TO 8
      writeblock emap, mapx \ 20 + i, mapy \ 20 + o, foe
     NEXT o
    NEXT i
   END IF
   IF keyval(scCapslock) > 1 THEN foe = readblock(emap, x, y)
   '--done input-modes-------
 END SELECT
 
 '--general purpose controls----
 IF keyval(scLeftShift) > 0 OR keyval(scRightShift) > 0 THEN
  xrate = 8
  yrate = 5
 ELSE
  xrate = 1
  yrate = 1
 END IF
 IF keyval(scAlt) = 0 AND keyval(scCtrl) = 0 THEN
  IF slowkey(scUp, 2) THEN y = large(y - yrate, 0): IF y < mapy \ 20 THEN mapy = y * 20
  IF slowkey(scDown, 2) THEN y = small(y + yrate, high - 1): IF y > mapy \ 20 + 8 THEN mapy = y * 20 - 160
  IF slowkey(scLeft, 2) THEN x = large(x - xrate, 0): IF x < mapx \ 20 THEN mapx = x * 20
  IF slowkey(scRight, 2) THEN x = small(x + xrate, wide - 1): IF x > mapx \ 20 + 15 THEN mapx = x * 20 - 300
 END IF
 IF keyval(scAlt) > 0 AND keyval(scCtrl) = 0 THEN
  oldrelx = x - mapx / 20
  oldrely = y - mapy / 20
  IF slowkey(scUp, 2) THEN mapy = large(mapy - 20 * yrate, 0)
  IF slowkey(scDown, 2) THEN mapy = small(mapy + 20 * yrate, high * 20 - 180)
  IF slowkey(scLeft, 2) THEN mapx = large(mapx - 20 * xrate, 0)
  IF slowkey(scRight, 2) THEN mapx = small(mapx + 20 * xrate, wide * 20 - 320)
  x = mapx / 20 + oldrelx
  y = mapy / 20 + oldrely
 END IF
 
 IF editmode = 0 THEN 'tilemode, uses layers
  IF keyval(scPageup) > 1 THEN
   FOR i = st.layer + 1 TO UBOUND(map)
    IF layerisenabled(gmap(), i) THEN
     st.layer = i
     setlayervisible(visible(), st.layer, 1)
     update_tilepicker st
     EXIT FOR
    END IF
   NEXT i
  END IF

  IF keyval(scPageDown) > 1 THEN
   FOR i = st.layer - 1 TO 0 STEP -1
    IF layerisenabled(gmap(), i) THEN
     st.layer = i
     setlayervisible(visible(), st.layer, 1)
     update_tilepicker st
     EXIT FOR
    END IF
   NEXT
  END IF
 END IF
 
 tog = tog XOR 1
 flash = loopvar(flash, 0, 3, 1)
 
 '--draw menubar
 IF editmode = 0 THEN
  setmapdata  , 0, 20
  drawmap st.menubar, st.menubarstart(st.layer) * 20, 0, 0, st.tilesets(st.layer), dpage
 ELSE
  rectangle 0, 0, 320, 20, uilook(uiBackground), dpage
 END IF
 
 '--draw map
 setmapdata @pass, 20
 animatetilesets st.tilesets()
 rectangle 0, 20, 320, 180, uilook(uiBackground), dpage
 for i = 0 to ubound(map)
 	if layerisvisible(visible(), i) AND layerisenabled(gmap(), i) then
		jigx = 0: jigy = 0
		if readbit(jiggle(), 0, i) and tog then
			jigx = 0
			if (i mod 8) >= 1 AND (i mod 8) <= 3 then jigx = 1
			if (i mod 8) >= 5 then jigx = -1
			jigy = 0
			if (i mod 8) <= 1 OR (i mod 8) = 7 then jigy = -1
			if (i mod 8) >= 3 AND (i mod 8) <= 5 then jigy = 1
			jigx *= i \ 8 + 1
			jigy *= i \ 8 + 1
		end if
		drawmap map(i), mapx + jigx, mapy + jigy, iif(i = 0, 1, 0), st.tilesets(i), dpage, iif(i = 0, 0, 1)
	end if
 next
 if layerisvisible(visible(), 0) AND layerisenabled(gmap(), 0) then
	if readbit(jiggle(), 0, 0) and tog then
		drawmap map(0), mapx, mapy - 1, 2, st.tilesets(0), dpage, 0
	else
		drawmap map(0), mapx, mapy, 2, st.tilesets(0), dpage, 0
	end if
 end if
 
 '--show passmode overlay
 IF editmode = 1 THEN
  FOR o = 0 TO 8
   FOR i = 0 TO 15
    over = readblock(pass, (mapx \ 20) + i, (mapy \ 20) + o)
    IF (over AND 1) THEN rectangle i * 20, o * 20 + 20, 20, 3, uilook(uiMenuItem + tog), dpage
    IF (over AND 2) THEN rectangle i * 20 + 17, o * 20 + 20, 3, 20, uilook(uiMenuItem + tog), dpage
    IF (over AND 4) THEN rectangle i * 20, o * 20 + 37, 20, 3, uilook(uiMenuItem + tog), dpage
    IF (over AND 8) THEN rectangle i * 20, o * 20 + 20, 3, 20, uilook(uiMenuItem + tog), dpage
    textcolor uilook(uiSelectedItem + tog), 0
    IF (over AND 16) THEN printstr "A", i * 20, o * 20 + 20, dpage
    IF (over AND 32) THEN printstr "B", i * 20 + 10, o * 20 + 20, dpage
    IF (over AND 64) THEN printstr "H", i * 20, o * 20 + 30, dpage
    IF (over AND 128) THEN printstr "O", i * 20 + 10, o * 20 + 30, dpage
   NEXT i
  NEXT o
 END IF
 
 '--door display--
 IF editmode = 2 THEN
  textcolor uilook(uiBackground), 0
  FOR i = 0 TO 99
   IF doors(i).x >= mapx \ 20 AND doors(i).x < mapx \ 20 + 16 AND doors(i).y > mapy \ 20 AND doors(i).y <= mapy \ 20 + 9 AND readbit(doors(i).bits(),0,0) = 1 THEN
    rectangle doors(i).x * 20 - mapx, doors(i).y * 20 - mapy, 20, 20, uilook(uiSelectedItem + tog), dpage
    printstr STR$(i), doors(i).x * 20 - mapx + 10 - (4 * LEN(STR$(i))), doors(i).y * 20 - mapy + 6, dpage
   END IF
  NEXT
 END IF

 '--hero start location display--
 IF gen(genStartMap) = mapnum THEN
  IF gen(genStartX) >= mapx \ 20 AND gen(genStartX) < mapx \ 20 + 16 AND gen(genStartY) > mapy \ 20 AND gen(genStartY) <= mapy \ 20 + 9 THEN
   drawsprite heroimg(), 0, heropal(), 0, gen(genStartX) * 20 - mapx, gen(genStartY) * 20 + 20 - mapy, dpage
   printstr "Hero", gen(genStartX) * 20 - mapx, gen(genStartY) * 20 + 30 - mapy, dpage
  END IF
 END IF

 '--npc display--
 IF editmode = 3 THEN
  FOR i = 0 TO UBOUND(npcnum)
   npcnum(i) = 0
  NEXT
  walk = walk + 1: IF walk > 3 THEN walk = 0
  FOR i = 0 TO 299
   WITH st.npc_inst(i)
    IF .id > 0 THEN
     IF .x >= mapx AND .x < mapx + 320 AND .y >= mapy AND .y < mapy + 200 THEN
      DIM image AS GraphicPair = npc_img(.id - 1)
      frame_draw image.sprite + (2 * .dir) + walk \ 2, image.pal, .x - mapx, .y + 20 - mapy, 1, -1, dpage
      textcolor uilook(uiSelectedItem + tog), 0
      printstr STR(.id - 1), .x - mapx, .y + 20 - mapy + 3, dpage
      printstr STR(npcnum(.id - 1)), .x - mapx, .y + 20 - mapy + 12, dpage
     END IF
     npcnum(.id - 1) += 1
    END IF
   END WITH
  NEXT
 END IF
 
 '--position finder--
 IF tiny = 1 THEN
  fuzzyrect 0, 20, wide, high, uilook(uiHighlight), dpage
  rectangle mapx / 20, (mapy / 20) + 20, 15, 9, uilook(uiDescription), dpage
 END IF
 
 '--normal cursor--
 IF editmode <> 3 THEN
  frame_draw st.cursor.sprite + tog, st.cursor.pal, (x * 20) - mapx, (y * 20) - mapy + 20, , , dpage
  IF editmode = 0 THEN
   frame_draw st.cursor.sprite + tog, st.cursor.pal, ((st.usetile(st.layer) - st.menubarstart(st.layer)) * 20), 0, , , dpage
  END IF
 END IF
 
 '--npc placement cursor--
 IF editmode = 3 THEN
  WITH npc_img(nptr)
   frame_draw .sprite + (2 * walk), .pal, x * 20 - mapx, y * 20 - mapy + 20, 1, -1, dpage
  END WITH
  textcolor uilook(uiSelectedItem + tog), 0
  xtemp = STR(nptr)
  printstr xtemp, (x * 20) - mapx, (y * 20) - mapy + 28, dpage
 END IF
 
 '--show foemap--
 IF editmode = 4 THEN
  textcolor uilook(uiSelectedItem + tog), 0
  FOR i = 0 TO 15
   FOR o = 0 TO 8
    temp = readblock(emap, mapx \ 20 + i, mapy \ 20 + o)
    IF temp > 0 THEN printstr STR$(temp), i * 20 - ((temp < 10) * 5), o * 20 + 26, dpage
   NEXT o
  NEXT i
 END IF
 
 textcolor uilook(uiSelectedItem + tog), 0
 IF editmode = 0 THEN
  printstr "Layer " & st.layer, 0, 180, dpage
 END IF
 printstr "X " & x & "   Y " & y, 0, 192, dpage
 rectangle 0, 19, 320, 1, uilook(uiText), dpage
 IF editmode = 0 THEN
  status$ = "Default Passability "
  IF defpass THEN status$ = status$ + "On" ELSE status$ = status$ + "Off"
  printstr status$, 124, 192, dpage
 END IF
 textcolor uilook(uiText), 0
 printstr modenames(editmode), 0, 24, dpage
 IF editmode = 4 THEN textcolor uilook(uiText), uilook(uiHighlight): printstr "Formation Set: " & foe, 0, 16, dpage
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
RETRACE '--end of mapping GOSUB block

END SUB

'======== FIXME: move this up as code gets cleaned up ===========
OPTION EXPLICIT

SUB mapedit_gmapdata(BYREF st AS MapEditState, gmap() AS INTEGER)
 DIM gdmenu(0 TO 18) AS STRING
 gdmenu(0) = "Previous Menu"
 gdmenu(1) = "Ambient Music:"
 gdmenu(2) = "Minimap Available:"
 gdmenu(3) = "Save Anywhere:"
 gdmenu(4) = "Display Map Name:"
 gdmenu(5) = "Map Edge Mode:"
 gdmenu(6) = "Default Edge Tile:"
 gdmenu(7) = "Autorun Script: "
 gdmenu(8) = "Autorun Script Argument:"
 gdmenu(9) = "Harm-Tile Damage:"
 gdmenu(10) = "Harm-Tile Flash:"
 gdmenu(11) = "Foot Offset:"
 gdmenu(12) = "After-Battle Script:"
 gdmenu(13) = "Instead-of-Battle Script:"
 gdmenu(14) = "Each-Step Script:"
 gdmenu(15) = "On-Keypress Script:"
 gdmenu(16) = "Walkabout Layering:"
 gdmenu(17) = "NPC Data:"
 gdmenu(18) = "Tile Data:"

 DIM gdmax(20) AS INTEGER,        gdmin(20) AS INTEGER
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
 gdmax(16) = 1:                   gdmin(16) = 0
 gdmax(17) = 2:                   gdmin(17) = 0
 gdmax(18) = 2:                   gdmin(18) = 0

 DIM state AS MenuState
 state.pt = 0
 state.last = UBOUND(gdmenu)
 state.size = 24

 DIM idx AS INTEGER
 DIM scri AS INTEGER
 DIM gmapscrof(5)
 gmapscrof(0) = 7  'autorun
 gmapscrof(1) = 12 'after-battle
 gmapscrof(2) = 13 'instead-of-battle
 gmapscrof(3) = 14 'each-step
 gmapscrof(4) = 15 'on-keypress

 DIM gmapscr(5) AS STRING
 FOR i AS INTEGER = 0 TO 4
  gmapscr(i) = scriptname(gmap(gmapscrof(i)), plottrigger)
 NEXT i

 'default out-of-bounds hero/npc draw order
 IF gmap(16) > 1 THEN gmap(16) = 0
 
 FOR i AS INTEGER = 1 TO UBOUND(gdmenu)
  'Safety-bounding of gmap data, prevents crashes in cases of corruption
  gmap(i) = bound(gmap(i), gdmin(i), gdmax(i))
 NEXT i

 'A sample map of a single tile, used to preview the default edge tile
 DIM sampmap AS TileMap
 cleantilemap sampmap, 1, 1
 
 DIM caption AS STRING
 
 setkeys
 clearpage dpage
 DO
  setwait 55
  setkeys
  state.tog = state.tog XOR 1
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "general_map_data"
  usemenu state
  SELECT CASE state.pt
   CASE 0
    IF enter_or_space() THEN EXIT DO
   CASE 1 'music
    zintgrabber(gmap(state.pt), gdmin(state.pt) - 1, gdmax(state.pt) - 1) 'song is optional
   CASE 7, 12 TO 15 'scripts
    IF state.pt = 7 THEN idx = 0 ELSE idx = state.pt - 11
    IF enter_or_space() THEN
     gmapscr(idx) = scriptbrowse_string(gmap(state.pt), plottrigger, "plotscript")
    ELSEIF scrintgrabber(gmap(state.pt), 0, 0, scLeft, scRight, 1, plottrigger) THEN
     gmapscr(idx) = scriptname(gmap(state.pt), plottrigger)
    END IF
   CASE 10' Harm tile color
    intgrabber gmap(state.pt), gdmin(state.pt), gdmax(state.pt)
    IF enter_or_space() THEN
     gmap(state.pt) = color_browser_256(gmap(state.pt))
    END IF
   CASE ELSE 'all other gmap data is simple integers
    intgrabber gmap(state.pt), gdmin(state.pt), gdmax(state.pt)
  END SELECT
  scri = 0
  FOR i AS INTEGER = 0 TO UBOUND(gdmenu)
   caption = ""
   SELECT CASE i
    CASE 1 'music
     IF gmap(1) = 0 THEN
      caption = "-silence-"
     ELSEIF gmap(1) = -1 THEN
      caption = "-same as previous map-"
     ELSE
      caption = (gmap(1) - 1) & " " & getsongname(gmap(1) - 1)
     END IF
    CASE 2, 3 'minimap available and save anywhere
     caption = yesorno(gmap(i))
    CASE 4 'show map name
     IF gmap(i) = 0 THEN caption = "NO" ELSE caption = gmap(i) & " ticks"
    CASE 5 'map edge mode
     SELECT CASE gmap(i)
      CASE 0
       caption = "Crop"
      CASE 1
       caption = "Wrap"
      CASE 2
       caption = "use default edge tile"
     END SELECT
    CASE 6 'default edge tile
     IF gmap(5) = 2 THEN
      caption = STR(gmap(i))
     ELSE
      caption = "N/A"
     END IF
    CASE 7, 12 TO 15 'scripts
     caption = gmapscr(scri)
     scri += 1
    CASE 8 'script argument
     IF gmap(7) = 0 THEN
      caption = "N/A"
     ELSE
      caption = STR(gmap(i))
     END IF
    CASE 9 'harm tile damage
     caption = STR(gmap(i))
    CASE 10 'harm tile flash
     IF gmap(i) = 0 THEN
      caption = "none"
     ELSE
      caption = STR(gmap(i))
     END IF
    CASE 11 'foot offset
     SELECT CASE gmap(i)
      CASE 0
       caption = "none"
      CASE IS < 0
       caption = "up " & ABS(gmap(i)) & " pixels"
      CASE IS > 0
       caption = "down " & gmap(i) & " pixels"
     END SELECT
    CASE 16 'hero/npc draw order
     IF gmap(i) = 1 THEN
      caption = "NPCs over Heroes"
     ELSE
      caption = "Heroes over NPCs"
     END IF
    CASE 17, 18 'NPC and Tile data saving
     SELECT CASE gmap(i)
      CASE 0
       caption = "Don't save state when leaving"
      CASE 1
       caption = "Remember state when leaving"
      CASE 2
       caption = "Ignore saved state, load anew"
     END SELECT
   END SELECT
   textcolor uilook(uiMenuItem), 0
   IF i = state.pt THEN textcolor uilook(uiSelectedItem + state.tog), 0
   printstr gdmenu(i) & " " & caption, 0, 8 * i, dpage
   IF i = 10 THEN
    'Harm tile flash color preview
    rectangle 4 + (8 * (LEN(gdmenu(i)) + 1 + LEN(caption))), 8 * i, 8, 8, gmap(i), dpage
   END IF
  NEXT
  IF gmap(5) = 2 THEN
   '--show default edge tile
   writeblock sampmap, 0, 0, gmap(6)
   setmapdata  , 180, 20
   drawmap sampmap, 0, 0, 0, st.tilesets(0), dpage
   rectangle 20, 180, 300, 20, uilook(uiBackground), dpage 'that's hacky
  END IF
 
  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
 unloadtilemap sampmap
END SUB

SUB mapedit_layers (BYREF st AS MapEditState, gmap() AS INTEGER, visible() AS INTEGER, defaults() AS DefArray, map() AS TileMap)
 DIM state AS MenuState
 REDIM menu(0) AS SimpleMenu
 REDIM itemsinfo(0) AS LayerMenuItem
 
 DIM layerno AS INTEGER
 DIM layering AS INTEGER
 DIM currentset AS INTEGER
 DIM resetpt AS INTEGER
 DIM col AS INTEGER
 DIM tileset AS INTEGER

 state.top = 0
 state.size = 18

 clearpage 2
 currentset = -1

 mapedit_makelayermenu st, menu(), state, gmap(), currentset, visible(), map(), itemsinfo(), YES

 DO 
  setwait 55
  setkeys
  state.tog = state.tog XOR 1

  layerno = itemsinfo(state.pt).layernum
  layering = layerno
  IF layering >= gmap(31) THEN layering += 1

  IF keyval(scESC) > 1 THEN clearkey(scESC): EXIT DO
  IF keyval(scF1) > 1 THEN show_help "mapedit_layers"
  IF (keyval(scPlus) > 1 OR keyval(scNumpadPlus) > 1) AND UBOUND(map) < maplayerMax THEN
   IF layerno = -1 THEN
    add_more_layers map(), visible(), gmap(), UBOUND(map) + 1
    st.layer = UBOUND(map)
    resetpt = YES
   ELSE
    'when gmap(31) is greater than actual number of layers we are "filling up" to old default of 2 under
    IF layerno < gmap(31) AND UBOUND(map) + 1 >= gmap(31) THEN gmap(31) += 1
    mapedit_insert_layer st, map(), visible(), gmap(), layerno + 1
    st.layer = layerno + 1
    resetpt = YES
   END IF
   currentset = -2
   state.need_update = YES
  END IF
  IF (keyval(scDelete) > 1 OR keyval(scMinus) > 1 OR keyval(scNumpadMinus) > 1) ANDALSO UBOUND(map) > 0 ANDALSO layerno >= 0 _
     ANDALSO yesno("Really delete layer " & layerno & "?", NO) THEN
   IF layerno < gmap(31) THEN gmap(31) = large(gmap(31) - 1, 1)
   mapedit_delete_layer st, map(), visible(), gmap(), layerno
   st.layer = small(layerno, UBOUND(map))
   resetpt = YES
   currentset = -2
   state.need_update = YES
  END IF
  IF keyval(scLeftShift) > 0 OR keyval(scRightShift) > 0 THEN
   IF keyval(scUp) > 1 AND layering > 0 THEN
    IF layering = gmap(31) + 1 THEN
     'swapping with NPC/Hero layers
     gmap(31) += 1
     st.layer = layerno
   ELSE
     mapedit_swap_layers st, map(), visible(), gmap(), layerno, layerno - 1
     st.layer = layerno - 1
    END IF
    resetpt = YES
    state.need_update = YES
   END IF
   'can't move npcs/heroes below layer 0.
   'UBOUND(map) + 1 is the maximum layering (can't adjust overhead tiles either)
   IF keyval(scDown) > 1 AND (gmap(31) > 1 OR layerno > 0) AND layering < UBOUND(map) + 1 THEN
    IF layering = gmap(31) - 1 THEN
     'swapping with NPC/Hero layers
     gmap(31) -= 1
     st.layer = layerno
    ELSE
     mapedit_swap_layers st, map(), visible(), gmap(), layerno, layerno + 1
     st.layer = layerno + 1
    END IF
    resetpt = YES
    state.need_update = YES
   END IF
  ELSE
   IF usemenu(state, menu()) THEN
    state.need_update = YES
   END IF
  END IF

  IF state.pt = 0 THEN
   IF enter_or_space() THEN
    EXIT DO
   END IF
  ELSEIF state.pt = 1 THEN
   intgrabber gmap(0), 0, gen(genMaxTile)
   state.need_update = YES
  ELSEIF layerno > -1 THEN
   IF itemsinfo(state.pt).gmapindex > -1 THEN
    clearkey(scPlus)
    clearkey(scNumpadPlus)
    clearkey(scMinus)
    clearkey(scNumpadMinus)
    IF zintgrabber(gmap(itemsinfo(state.pt).gmapindex), -1, gen(genMaxTile)) THEN
     tileset = gmap(itemsinfo(state.pt).gmapindex) - 1
     IF tileset = -1 THEN tileset = gmap(0)
     loadtilesetdata st.tilesets(), layerno, tileset
     state.need_update = YES
    END IF
   ELSE
    IF enter_or_space() THEN
     ToggleLayerEnabled(gmap(), layerno)
     state.need_update = YES
    END IF
    IF layerisenabled(gmap(), layerno) AND (keyval(scLeft) > 1 OR keyval(scRight) > 1) THEN
     ToggleLayerVisible(visible(), layerno)
     state.need_update = YES
    END IF
   END IF
  END IF

  IF state.need_update THEN
   state.need_update = NO
   mapedit_makelayermenu st, menu(), state, gmap(), currentset, visible(), map(), itemsinfo(), resetpt
   resetpt = NO
  END IF

  copypage 2, dpage

  IF UBOUND(map) < maplayerMax THEN
   IF layerno > -1 THEN
    edgeprint "+ to add a new layer after this one", 0, 180, uilook(uiText), dpage
   ELSE
    edgeprint "+ to add a new layer", 0, 180, uilook(uiText), dpage
   END IF
  END IF
  edgeprint "SHIFT+arrows to move layers, - to delete", 0, 190, uilook(uiText), dpage
  
  FOR i AS INTEGER = state.top TO state.top + state.size
   IF i <= state.last THEN
    col = menu(i).col
    IF state.pt = i THEN col = uilook(uiSelectedItem + state.tog)
    edgeprint menu(i).text, 0, (i - state.top) * 9, col, dpage
   END IF
  NEXT

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 clearpage dpage
 loadmaptilesets st.tilesets(), gmap()
 FOR i AS INTEGER = 0 TO UBOUND(map)
  loadpasdefaults defaults(i).a(), st.tilesets(i)->num
 NEXT
 'we don't care about the rest of the defaults array
 IF layerisenabled(gmap(), st.layer) = 0 THEN st.layer = 0

END SUB

SUB mapedit_makelayermenu_layer(BYREF st AS MapEditState, menu() AS SimpleMenu, gmap() AS INTEGER, visible() AS INTEGER, itemsinfo() AS LayerMenuItem, BYREF slot AS INTEGER, BYVAL layer AS INTEGER, BYREF needdefault AS INTEGER)

 menu(slot).enabled = NO
 menu(slot).text = "Tile layer " & layer
 slot += 1

 IF layerisenabled(gmap(), layer) THEN
  IF layerisvisible(visible(), layer) THEN
   menu(slot).text = " Enabled (" & CHR(27) & "Visible in editor" & CHR(26) & ")"
   menu(slot - 1).col = uilook(uiSelectedDisabled)
  ELSE
   menu(slot).text = " Enabled (" & CHR(27) & "Invisible in editor" & CHR(26) & ")"
   menu(slot - 1).col = uilook(uiDisabledItem)
  END IF
 ELSE
  menu(slot).text = " Disabled in-game"
  menu(slot - 1).col = uilook(uiDisabledItem)
 END IF
 itemsinfo(slot).layernum = layer
 slot += 1

 DIM layerindex AS INTEGER = layer_tileset_index(layer)

 IF gmap(layerindex) = 0 THEN
  menu(slot).text = " Tileset: Default"
  needdefault = YES
 ELSE
  menu(slot).text = " Tileset: " & gmap(layerindex) - 1
 END IF
 itemsinfo(slot).layernum = layer
 itemsinfo(slot).gmapindex = layerindex
 slot += 1
END SUB

SUB mapedit_makelayermenu(BYREF st AS MapEditState, menu() AS SimpleMenu, state AS MenuState, gmap() AS INTEGER, BYREF currentset AS INTEGER, visible() AS INTEGER, map() AS TileMap, itemsinfo() AS LayerMenuItem, BYVAL resetpt AS INTEGER)
 REDIM menu(1 + 3 * (UBOUND(map) + 1) + 3)
 REDIM itemsinfo(1 + 3 * (UBOUND(map) + 1) + 3)
 state.last = UBOUND(menu)
 FOR i AS INTEGER = 0 TO UBOUND(menu)
  menu(i).enabled = YES
  menu(i).col = uilook(uiMenuItem)
  itemsinfo(i).layernum = -1
  itemsinfo(i).gmapindex = -1
 NEXT i
 menu(0).text = "Go back"
 menu(1).text = "Default tileset: "
 
 DIM needdefault AS INTEGER = NO
 
 DIM slot AS INTEGER = 2
 FOR i AS INTEGER = 0 TO small(UBOUND(map), gmap(31) - 1)
  IF st.layer = i AND resetpt THEN state.pt = slot + 1
  mapedit_makelayermenu_layer st, menu(), gmap(), visible(), itemsinfo(), slot, i, needdefault
 NEXT

 menu(slot).enabled = NO
 menu(slot).col = uilook(uiSelectedDisabled)
 slot += 1
 menu(slot).enabled = NO
 menu(slot).col = uilook(uiSelectedDisabled)
 slot += 1
 IF gmap(16) = 0 THEN
  menu(slot - 2).text = "NPCs layer"
  menu(slot - 1).text = "Heroes layer"
 ELSE
  menu(slot - 2).text = "Heroes layer"
  menu(slot - 1).text = "NPCs layer"
 END IF   

 FOR i AS INTEGER = gmap(31) TO UBOUND(map)
  IF st.layer = i AND resetpt THEN state.pt = slot + 1
  mapedit_makelayermenu_layer st, menu(), gmap(), visible(), itemsinfo(), slot, i, needdefault
 NEXT

 menu(slot).enabled = NO
 menu(slot).col = uilook(uiSelectedDisabled)
 menu(slot).text = "Tile layer 0 overhead tiles (obsolete)"
 slot += 1
 
 IF needdefault THEN
  menu(1).text += STR(gmap(0))
 ELSE
  menu(1).text += "(Not used)"
  menu(1).enabled = NO
  menu(1).col = uilook(uiDisabledItem)
 END IF

 DIM layerno AS INTEGER = itemsinfo(state.pt).layernum
 DIM wantset AS INTEGER = -1
 IF state.pt = 1 THEN
  wantset = gmap(0)
 ELSEIF itemsinfo(state.pt).gmapindex > -1 THEN
  wantset = gmap(itemsinfo(state.pt).gmapindex) - 1
  IF wantset = -1 THEN wantset = gmap(0)
 ELSEIF layerno > -1 AND itemsinfo(state.pt).gmapindex = -1 THEN
  wantset = 1000000 + layerno
 END IF
 IF wantset <> currentset THEN
  IF wantset = -1 THEN
   clearpage 2
  ELSEIF wantset >= 1000000 THEN
   clearpage 2
   DIM preview AS Frame Ptr
   preview = createminimap(map(wantset - 1000000), st.tilesets(wantset - 1000000))
   frame_draw preview, NULL, 0, 0, , , 2
   frame_unload @preview
   'fuzzyrect 0, 0, 320, 200, uilook(uiBackground), 2
  ELSE
   loadmxs game + ".til", wantset, vpages(2)
   fuzzyrect 0, 0, 320, 200, uilook(uiBackground), 2
  END IF
  currentset = wantset
 END IF
 
END SUB

FUNCTION find_door_at_spot (x AS INTEGER, y AS INTEGER, doors() AS Door) AS INTEGER
 DIM i AS INTEGER
 FOR i = 0 TO UBOUND(doors)
  IF doors(i).x = x AND doors(i).y = y + 1 AND readbit(doors(i).bits(),0,0) = 1 THEN
   RETURN i
  END IF
 NEXT i
 RETURN -1
END FUNCTION

FUNCTION find_first_free_door (doors() AS Door) AS INTEGER
 DIM i AS INTEGER
 FOR i = 0 TO UBOUND(doors)
  IF readbit(doors(i).bits(), 0, 0) = 0 THEN
   RETURN i
  END IF
 NEXT i
 RETURN -1
END FUNCTION

FUNCTION find_first_doorlink_by_door(doornum AS INTEGER, link() AS DoorLink) AS INTEGER
 DIM i AS INTEGER
 FOR i = 0 TO UBOUND(link)
  IF link(i).source = doornum THEN RETURN i
 NEXT i
 RETURN -1
END FUNCTION

'Adds a new map with ID gen(genMaxMap) + 1
SUB mapedit_addmap()
 'Temporary buffers for making the copy
 DIM st AS MapEditState
 DIM gmap(dimbinsize(binMAP))
 REDIM doors(99) AS door, link(199) AS doorlink
 REDIM map(0) AS TileMap ' dummy empty map data, will be resized later
 DIM pass AS TileMap
 DIM emap AS TileMap

 DIM copyname AS STRING
 DIM copysize AS XYPair
 DIM visible(maplayerMax \ 16) AS INTEGER
 visible(0) = -1 'used as bitsets
 DIM defaults(maplayerMax) AS DefArray
 
 DIM how AS INTEGER
 how = addmaphow()
 '-- -2  =Cancel
 '-- -1  =New blank
 '-- >=0 =Copy
 IF how = -1 THEN
  gen(genMaxMap) += 1
  new_blank_map st, map(), pass, emap, gmap(), doors(), link()
  mapedit_savemap st, gen(genMaxMap), map(), pass, emap, gmap(), doors(), link(), ""
 ELSEIF how >= 0 THEN
  gen(genMaxMap) += 1
  mapedit_loadmap st, how, copysize.x, copysize.y, map(), pass, emap, gmap(), visible(), doors(), link(), defaults(), copyname
  mapedit_savemap st, gen(genMaxMap), map(), pass, emap, gmap(), doors(), link(), copyname
 END IF
END SUB

SUB new_blank_map (BYREF st AS MapEditState, map() AS TileMap, pass AS TileMap, emap AS TileMap, gmap() AS INTEGER, doors() AS Door, link() AS DoorLink)
 '--flush map buffers
 cleantilemaps map(), 64, 64, 1
 cleantilemap pass, 64, 64
 cleantilemap emap, 64, 64
 flusharray gmap(), -1, 0
 CleanNPCL st.npc_inst()
 CleanNPCD st.npc_def()
 st.num_npc_defs = 1
 cleandoors doors()
 cleandoorlinks link()
END SUB

SUB mapedit_loadmap (BYREF st AS MapEditState, mapnum AS INTEGER, BYREF wide AS INTEGER, BYREF high AS INTEGER, map() AS TileMap, pass AS TileMap, emap AS TileMap, gmap() AS INTEGER, visible() AS INTEGER, doors() AS Door, link() AS DoorLink, defaults() AS DefArray, mapname AS STRING)
 loadrecord gmap(), game & ".map", dimbinsize(binMAP), mapnum
 IF gmap(31) = 0 THEN gmap(31) = 2
 visible(maplayerMax \ 16) = -1   'default all layers to visible, if they're enabled too, of course
 loadtilemaps map(), maplumpname(mapnum, "t")
 loadtilemap pass, maplumpname(mapnum, "p")
 loadtilemap emap, maplumpname(mapnum, "e")
 loadmaptilesets st.tilesets(), gmap()
 FOR i AS INTEGER = 0 TO UBOUND(map)
  loadpasdefaults defaults(i).a(), st.tilesets(i)->num
 NEXT
 'rest of defaults does not need initialisation
 LoadNPCL maplumpname(mapnum, "l"), st.npc_inst()
 LoadNPCD_fixedlen maplumpname(mapnum, "n"), st.npc_def(), st.num_npc_defs
 deserdoors game & ".dox", doors(), mapnum
 deserdoorlinks maplumpname(mapnum, "d"), link()
 mapname = getmapname(mapnum)
 wide = map(0).wide
 high = map(0).high
 verify_map_size mapnum, wide, high, map(), pass, emap, mapname
END SUB

SUB mapedit_savemap (BYREF st AS MapEditState, mapnum AS INTEGER, map() AS TileMap, pass AS TileMap, emap AS TileMap, gmap() AS INTEGER, doors() AS Door, link() AS DoorLink, mapname AS STRING)
 storerecord gmap(), game & ".map", getbinsize(binMAP) / 2, mapnum
 savetilemaps map(), maplumpname(mapnum, "t")
 savetilemap pass, maplumpname(mapnum, "p")
 savetilemap emap, maplumpname(mapnum, "e")
 SaveNPCL maplumpname(mapnum, "l"), st.npc_inst()
 SaveNPCD_fixedlen maplumpname(mapnum, "n"), st.npc_def(), st.num_npc_defs
 serdoors game & ".dox", doors(), mapnum
 serdoorlinks maplumpname(mapnum, "d"), link()
 '--save map name
 DIM mapsave(39) AS INTEGER
 mapsave(0) = LEN(mapname)
 str2array LEFT(mapname, 39), mapsave(), 1
 storerecord mapsave(), game & ".mn", 40, mapnum
END SUB

SUB verify_map_size (mapnum AS INTEGER, BYREF wide AS INTEGER, BYREF high AS INTEGER, map() AS TileMap, pass AS TileMap, emap AS TileMap, mapname AS STRING)
 IF map(0).wide = pass.wide AND pass.wide = emap.wide AND map(0).high = pass.high AND pass.high = emap.high THEN EXIT SUB
 '--Map's X and Y do not match
 wide = map(0).wide
 high = map(0).high
 clearpage vpage
 DIM j AS INTEGER
 j = 0
 textcolor uilook(uiText), 0
 printstr "Map" & filenum(mapnum) & ":" & mapname, 0, j * 8, vpage
 j += 2
 printstr "this map seems to be corrupted", 0, j * 8, vpage
 j += 2
 printstr " TileMap " & map(0).wide & "*" & map(0).high & " tiles, " & UBOUND(map) & " layers", 0, j * 8, vpage: j += 1
 printstr " WallMap " & pass.wide & "*" & pass.high & " tiles", 0, j * 8, vpage: j += 1
 printstr " FoeMap " & emap.wide & "*" & emap.high & " tiles", 0, j * 8, vpage: j += 1
 j += 1
 printstr "Fixing to " & wide & "*" & high, 0, j * 8, vpage: j += 1
 'A map's size might be due to corruption, besides, the tilemap is far away the most important
 'wide = large(map(0).wide, large(pass.wide, emap.wide))
 'high = large(map(0).high, large(pass.high, emap.high))
 pass.wide = wide: pass.high = high
 pass.data = REALLOCATE(pass.data, wide * high)
 emap.wide = wide: emap.high = high
 emap.data = REALLOCATE(emap.data, wide * high)
 'savetilemaps map(), maplumpname(mapnum, "t")
 savetilemap pass, maplumpname(mapnum, "p")
 savetilemap emap, maplumpname(mapnum, "e")
 'loadtilemaps map(), maplumpname(mapnum, "t")
 'loadtilemap pass, maplumpname(mapnum, "p")
 'loadtilemap emap, maplumpname(mapnum, "e")
 j += 1
 printstr "please report this error to", 0, j * 8, vpage: j += 1
 printstr "ohrrpgce@HamsterRepublic.com", 0, j * 8, vpage: j += 1
 setvispage vpage
 waitforanykey
END SUB

SUB add_more_layers(map() as TileMap, vis() as integer, gmap() as integer, BYVAL numlayers as integer)
 DIM old_numlayers as integer = UBOUND(map)
 numlayers = bound(numlayers, UBOUND(map), maplayerMax + 1)
 REDIM PRESERVE map(numlayers)
 FOR i as integer = old_numlayers + 1 to numlayers
  CleanTilemap map(i), map(0).wide, map(0).high, i
  SetLayerEnabled(gmap(), i, YES)
  SetLayerVisible(vis(), i, YES)
  gmap(layer_tileset_index(i)) = 0
 NEXT
END SUB

SUB fix_tilemaps(map() as TileMap)
 FOR i as integer = 0 TO UBOUND(map)
  map(i).layernum = i
 NEXT
END SUB

SUB mapedit_swap_layers(BYREF st AS MapEditState, map() as TileMap, vis() as integer, gmap() as integer, BYVAL l1 as integer, BYVAL l2 as integer)
 DIM as integer temp1, temp2
 SWAP map(l1), map(l2)
 SWAP st.usetile(l1), st.usetile(l2)
 SWAP st.menubarstart(l1), st.menubarstart(l2)
 SWAP gmap(layer_tileset_index(l1)), gmap(layer_tileset_index(l2))
 SWAP st.tilesets(l1), st.tilesets(l2)
 temp1 = layerisenabled(gmap(), l1)
 temp2 = layerisenabled(gmap(), l2)
 setlayerenabled(gmap(), l2, temp1)
 setlayerenabled(gmap(), l1, temp2)
 temp1 = layerisvisible(vis(), l1)
 temp2 = layerisvisible(vis(), l2)
 setlayervisible(vis(), l2, temp1)
 setlayervisible(vis(), l1, temp2)
END SUB

SUB mapedit_insert_layer(BYREF st AS MapEditState, map() as TileMap, vis() as integer, gmap() as integer, BYVAL where as integer)
 'doesn't reload (all) tilesets or passability defaults, layers menu does that
 IF UBOUND(map) = maplayerMax THEN EXIT SUB

 REDIM PRESERVE map(UBOUND(map) + 1)
 CleanTilemap map(UBOUND(map)), map(0).wide, map(0).high
 setlayerenabled(gmap(), UBOUND(map), YES)
 setlayervisible(vis(), UBOUND(map), YES)
 gmap(layer_tileset_index(UBOUND(map))) = 0
 FOR i as integer = UBOUND(map) - 1 TO where STEP -1
  mapedit_swap_layers st, map(), vis(), gmap(), i, i + 1
 NEXT
 fix_tilemaps map()
END SUB

SUB mapedit_delete_layer(BYREF st AS MapEditState, map() as TileMap, vis() as integer, gmap() as integer, BYVAL which as integer)
 'doesn't reload tilesets or passability defaults, layers menu does that
 FOR i as integer = which TO UBOUND(map) - 1
  mapedit_swap_layers st, map(), vis(), gmap(), i, i + 1
 NEXT
 UnloadTilemap map(UBOUND(map))
 'currently (temporarily) tilesets for unused layers are still loaded, so reset to default
 gmap(layer_tileset_index(UBOUND(map))) = 0
 REDIM PRESERVE map(UBOUND(map) - 1)
 fix_tilemaps map()
END SUB

SUB mapedit_resize(BYREF st AS MapEditState, mapnum AS INTEGER, BYREF wide AS INTEGER, BYREF high AS INTEGER, BYREF x AS INTEGER, BYREF y AS INTEGER, BYREF mapx AS INTEGER, BYREF mapy AS INTEGER, map() AS TileMap, pass AS TileMap, emap AS TileMap, gmap() AS INTEGER, doors() AS Door, link() AS DoorLink, mapname AS STRING)
'sizemap:
 DIM rs AS MapResizeState
 rs.rect.wide = 0
 rs.rect.high = 0
 rs.rect.x = 0
 rs.rect.y = 0
 resizemapmenu st, map(), rs
 IF rs.rect.wide = -1 THEN EXIT SUB

 clearpage 0
 clearpage 1
 
 DIM yout AS INTEGER = 0
 edgeprint "TILEMAP", 0, yout * 10, uilook(uiText), vpage: setvispage vpage: yout += 1
 resizetiledata map(), rs, yout, vpage
 edgeprint "PASSMAP", 0, yout * 10, uilook(uiText), vpage: setvispage vpage: yout += 1
 resizetiledata pass, rs, yout, vpage
 edgeprint "FOEMAP", 0, yout * 10, uilook(uiText), vpage: setvispage vpage: yout += 1
 resizetiledata emap, rs, yout, vpage
 ' update SAV x/y offset in MAP lump
 gmap(20) += rs.rect.x * -1
 gmap(21) += rs.rect.y * -1
 ' update hero's starting position (if on current map)
 IF gen(genStartMap) = mapnum THEN
  gen(genStartX) += rs.rect.x * -1
  gen(genStartY) += rs.rect.y * -1 
 END IF
 wide = rs.rect.wide
 high = rs.rect.high
 '--reset map scroll position
 x = 0
 y = 0
 mapx = 0
 mapy = 0
 edgeprint "Aligning and truncating doors", 0, yout * 10, uilook(uiText), vpage: yout += 1
 DIM i AS INTEGER
 FOR i = 0 TO 99
  doors(i).x -= rs.rect.x
  doors(i).y -= rs.rect.y
  IF doors(i).x < 0 OR doors(i).y < 0 OR doors(i).x >= wide OR doors(i).y >= high THEN
   setbit(doors(i).bits(), 0, 0, 0)
  END IF
 NEXT
 edgeprint "Aligning and truncating NPCs", 0, yout * 10, uilook(uiText), vpage: setvispage vpage: yout += 1
 FOR i = 0 TO 299
  WITH st.npc_inst(i)
   .x -= rs.rect.x * 20
   .y -= rs.rect.y * 20
   IF .x < 0 OR .y < 0 OR .x >= wide * 20 OR .y >= high * 20 THEN
    .id = 0
   END IF
  END WITH
 NEXT i
 verify_map_size mapnum, wide, high, map(), pass, emap, mapname
END SUB

SUB mapedit_delete(BYREF st AS MapEditState, mapnum AS INTEGER, BYREF wide AS INTEGER, BYREF high AS INTEGER, BYREF x AS INTEGER, BYREF y AS INTEGER, BYREF mapx AS INTEGER, BYREF mapy AS INTEGER, map() AS TileMap, pass AS TileMap, emap AS TileMap, doors() AS Door, link() AS DoorLink)
 IF yesno("Delete this map?", NO) THEN
  printstr "Please Wait...", 0, 40, vpage
  setvispage vpage

  cleantilemaps map(), wide, high, 1
  cleantilemap pass, wide, high
  cleantilemap emap, wide, high
  CleanNPCL st.npc_inst()
  cleandoorlinks link()
  cleandoors doors()

  savetilemaps map(), maplumpname$(mapnum, "t")
  savetilemap pass, maplumpname$(mapnum, "p")
  savetilemap emap, maplumpname$(mapnum, "e")
  SaveNPCL maplumpname(mapnum, "l"), st.npc_inst()
  serdoorlinks maplumpname$(mapnum, "d"), link()
  serdoors game + ".dox", doors(), mapnum
  '--reset scroll position
  x = 0
  y = 0
  mapx = 0
  mapy = 0
  st.layer = 0
  '--if this is the last map, then actually remove it entirely, rather than just blanking it
  IF mapnum = gen(genMaxMap) THEN
   gen(genMaxMap) -= 1
   safekill maplumpname$(mapnum, "t")
   safekill maplumpname$(mapnum, "p")
   safekill maplumpname$(mapnum, "e")
   safekill maplumpname$(mapnum, "l")
   safekill maplumpname$(mapnum, "d")
  END IF
 END IF
END SUB

SUB update_tilepicker(BYREF st AS MapEditState)
 st.menubarstart(st.layer) = bound(st.menubarstart(st.layer), large(st.usetile(st.layer) - 14, 0), small(st.usetile(st.layer), 145))
 st.tilepick.y = st.usetile(st.layer) \ 16
 st.tilepick.x = st.usetile(st.layer) - (st.tilepick.y * 16)
END SUB

SUB mapedit_linkdoors (BYREF st AS MapEditState, mapnum AS INTEGER, map() AS TileMap, pass AS TileMap, emap AS TileMap, gmap() AS INTEGER, doors() AS Door, link() AS DoorLink, mapname AS STRING)
 mapedit_savemap st, mapnum, map(), pass, emap, gmap(), doors(), link(), mapname
 
 DIM state AS MenuState
 state.top = 0
 state.pt = 0
 state.last = small(find_last_used_doorlink(link()) + 1, UBOUND(link))
 state.size = 11
 state.need_update = YES

 DIM menu_temp AS STRING
 DIM col AS INTEGER

 setkeys
 DO
  setwait 55
  setkeys
  state.tog = state.tog XOR 1
  IF keyval(scESC) > 1 THEN
   serdoorlinks(maplumpname$(mapnum, "d"), link())
   EXIT DO
  END IF
  IF keyval(scF1) > 1 THEN show_help "mapedit_linkdoors"
  IF usemenu(state) THEN state.need_update = YES
  IF enter_or_space() THEN
   IF state.pt = state.last AND link(state.pt).source = -1 THEN link(state.pt).source = 0
   link_one_door st, mapnum, state.pt, link(), doors(), map(), pass, gmap()
   state.need_update = YES
   IF state.pt = state.last AND link(state.pt).source >= 0 THEN
    state.last = small(state.last + 1, UBOUND(link))
   END IF
  END IF
  IF state.need_update THEN
   state.need_update = NO
   DrawDoorPair st, mapnum, state.pt, map(), pass, doors(), link(), gmap()
  END IF
  FOR i AS INTEGER = state.top TO small(state.top + state.size, state.last)
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
      menu_temp += ABS(link(i).tag1) & " = " & iif(link(i).tag1 > 0, 1, 0)
     END IF
     IF link(i).tag2 THEN
      IF link(i).tag1 THEN menu_temp += " and tag "
      menu_temp += ABS(link(i).tag2) & " = " & iif(link(i).tag2 > 0, 1, 0)
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
  copypage 2, dpage
  dowait
 LOOP
END SUB

SUB link_one_door(BYREF st AS MapEditState, mapnum AS INTEGER, linknum AS INTEGER, link() AS DoorLink, doors() AS Door, map() AS TileMap, pass AS TileMap, gmap() AS INTEGER)
 DIM ulim(4) AS INTEGER, llim(4) AS INTEGER
 ulim(0) = 99: llim(0) = -1
 ulim(1) = 99: llim(1) = 0
 ulim(2) = gen(genMaxMap): llim(2) = 0
 ulim(3) = 999: llim(3) = -999
 ulim(4) = 999: llim(4) = -999

 DIM menu(-1 TO 4) AS STRING
 menu(-1) = "Go Back"
 menu(0) = "Entrance Door"
 menu(1) = "Exit Door"
 menu(2) = "Exit Map"
 menu(3) = "Require Tag"
 menu(4) = "Require Tag"
 
 DIM state AS MenuState
 state.pt = -1
 state.top = -1
 state.size = 22
 state.first = LBOUND(menu)
 state.last = UBOUND(menu)
 
 DIM preview_delay AS INTEGER = 0
 DIM outmap AS STRING
 outmap = getmapname$(link(linknum).dest_map)
 DIM menu_temp AS STRING
 DIM col AS INTEGER

 DrawDoorPair st, mapnum, linknum, map(), pass, doors(), link(), gmap()

 setkeys
 DO
  setwait 55
  setkeys
  state.tog = state.tog XOR 1
  IF preview_delay > 0 THEN
   preview_delay -= 1
   IF preview_delay = 0 THEN DrawDoorPair st, mapnum, linknum, map(), pass, doors(), link(), gmap()
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
      outmap = getmapname$(link(linknum).dest_map)
     END IF
    CASE 3
     tag_grabber link(linknum).tag1
    CASE 4
     tag_grabber link(linknum).tag2
    CASE ELSE
     '...
   END SELECT
  ELSE
   IF enter_or_space() THEN EXIT DO
  END IF
  rectangle 0, 100, 320, 2, uilook(uiSelectedDisabled + state.tog), dpage
  FOR i AS INTEGER = -1 TO 4
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
     menu_temp = tag_condition_caption(link(linknum).tag1, "", "No Tag Check")
    CASE 4
     menu_temp = tag_condition_caption(link(linknum).tag2, "", "No Tag Check")
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
  copypage 2, dpage
  dowait
 LOOP
END SUB

FUNCTION find_last_used_doorlink(link() AS DoorLink) AS INTEGER
 DIM i AS INTEGER
 FOR i = UBOUND(link) TO 0 STEP -1
  IF link(i).source >= 0 THEN RETURN i
 NEXT i
 RETURN -1
END FUNCTION

Function LayerIsVisible(vis() as integer, byval l as integer) as integer
	'debug "layer #" & l & " is: " & readbit(vis(), 0, l)
	return readbit(vis(), 0, l)
end function

Function LayerIsEnabled(gmap() as integer, byval l as integer) as integer
	if l <= 0 then return 1
	'debug "layer #" & l & " is: " & readbit(gmap(), 19, l-1)
	return readbit(gmap(), 19, l-1)
end function

Sub SetLayerVisible(vis() as integer, byval l as integer, byval v as integer)
	setbit(vis(), 0, l, v)
end sub

Sub SetLayerEnabled(gmap() as integer, byval l as integer, byval v as integer)
	if l <= 0 then exit sub
	setbit(gmap(), 19, l-1, v)
end sub

Sub ToggleLayerVisible(vis() as integer, byval l as integer)
	setbit(vis(), 0, l, readbit(vis(), 0, l) xor 1)
end sub

Sub ToggleLayerEnabled(gmap() as integer, byval l as integer)
	if l <= 0 then exit sub
	setbit(gmap(), 19, l - 1, readbit(gmap(), 19, l-1) xor 1)
end sub

SUB DrawDoorPair(BYREF st AS MapEditState, curmap as integer, cur as integer, map() AS TileMap, pass AS TileMap, doors() as door, link() as doorlink, gmap())
 DIM as integer dmx, dmy, i
 DIM as string caption
 DIM destdoor(99) as door
 DIM destmap AS INTEGER
 DIM gmap2(dimbinsize(binMAP))
 REDIM map2(0) AS TileMap
 DIM pass2 AS TileMap
 DIM tilesets2(maplayerMax) AS TilesetData ptr
 
 clearpage 2
 IF link(cur).source = -1 THEN EXIT SUB

 IF readbit(doors(link(cur).source).bits(),0,0) = 1 THEN
  dmx = doors(link(cur).source).x * 20 - 150
  dmy = doors(link(cur).source).y * 20 - 65
  dmx = small(large(dmx, 0), map(0).wide * 20 - 320)
  dmy = small(large(dmy, 0), map(0).high * 20 - 100)
  setmapdata @pass, 0, 99
  FOR i = 0 TO UBOUND(map)
   IF LayerIsEnabled(gmap(), i) THEN
     drawmap map(i), dmx, dmy, 0, st.tilesets(i), 2, i <> 0
   END IF
  NEXT i
  IF LayerIsEnabled(gmap(), 0) THEN
   drawmap map(0), dmx, dmy, 2, st.tilesets(0), 2, 0
  END IF
  edgebox doors(link(cur).source).x * 20 - dmx, doors(link(cur).source).y * 20 - dmy - 20, 20, 20, uilook(uiMenuItem), uilook(uiBackground), 2
  textcolor uilook(uiBackground), 0
  caption = STR(link(cur).source)
  printstr caption, doors(link(cur).source).x * 20 - dmx + 10 - (4 * LEN(caption)), doors(link(cur).source).y * 20 - dmy - 14, 2
 END IF
 '-----------------EXIT DOOR
 destmap = link(cur).dest_map
 loadrecord gmap2(), game + ".map", dimbinsize(binMAP), destmap
 deserdoors game + ".dox", destdoor(), destmap
 LoadTilemaps map2(), maplumpname$(destmap, "t")
 LoadTilemap pass2, maplumpname$(destmap, "p")
 loadmaptilesets tilesets2(), gmap2()

 IF readbit(destdoor(link(cur).dest).bits(),0,0) = 1 THEN
  dmx = destdoor(link(cur).dest).x * 20 - 150
  dmy = destdoor(link(cur).dest).y * 20 - 65
  dmx = small(large(dmx, 0), map2(0).wide * 20 - 320)
  dmy = small(large(dmy, 0), map2(0).high * 20 - 100)
  setmapdata @pass2, 101, 
  FOR i = 0 TO UBOUND(map2)
   IF LayerIsEnabled(gmap2(), i) THEN
     drawmap map2(i), dmx, dmy, 0, tilesets2(i), 2, i <> 0
   END IF
  NEXT i
  IF LayerIsEnabled(gmap2(), 0) THEN
   drawmap map2(0), dmx, dmy, 2, tilesets2(0), 2, 0
  END IF
  edgebox destdoor(link(cur).dest).x * 20 - dmx, destdoor(link(cur).dest).y * 20 - dmy + 80, 20, 20, uilook(uiMenuItem), uilook(uiBackground), 2
  textcolor uilook(uiBackground), 0
  caption = STR(link(cur).dest)
  printstr caption, destdoor(link(cur).dest).x * 20 - dmx + 10 - (4 * LEN(caption)), destdoor(link(cur).dest).y * 20 - dmy + 86, 2
 END IF
 unloadtilemaps map2()
 unloadtilemap pass2
 unloadmaptilesets tilesets2()
END SUB

SUB calculatepassblock(BYREF st AS MapEditState, x AS INTEGER, y AS INTEGER, map() AS TileMap, pass AS TileMap, defaults() AS DefArray)
 DIM n AS INTEGER = 0
 DIM tilenum AS INTEGER
 FOR i AS INTEGER = 0 TO UBOUND(map)
  tilenum = readblock(map(i), x, y)
  IF i = 0 OR tilenum > 0 THEN
   n = n OR defaults(i).a(animadjust(tilenum, st.tilesets(i)->tastuf()))
  END IF
 NEXT i
 writeblock pass, x, y, n
END SUB

SUB resizetiledata (tmap AS TileMap, rs AS MapResizeState, BYREF yout AS INTEGER, page AS INTEGER)
 resizetiledata tmap, rs.rect.x, rs.rect.y, rs.rect.wide, rs.rect.high, yout, page
END SUB

SUB resizetiledata (tmaps() AS TileMap, rs AS MapResizeState, BYREF yout AS INTEGER, page AS INTEGER)
 FOR i AS INTEGER = 0 TO UBOUND(tmaps)
  resizetiledata tmaps(i), rs.rect.x, rs.rect.y, rs.rect.wide, rs.rect.high, yout, page
 NEXT
END SUB

SUB resizetiledata (tmap AS TileMap, x_off AS INTEGER, y_off AS INTEGER, new_width AS INTEGER, new_height AS INTEGER, BYREF yout AS INTEGER, page AS INTEGER)
 edgeprint "Resizing Map...", 0, yout * 10, uilook(uiText), page
 yout += 1
 setvispage page

 dim tmp as TileMap
 cleantilemap tmp, new_width, new_height
 tmp.layernum = tmap.layernum  'the unexpected ingredient!

 dim as integer x, y
 for x = large(x_off, 0) to small(tmap.wide, new_width + x_off) - 1
	for y = large(y_off, 0) to small(tmap.high, new_height + y_off) - 1
		'newarray((x - tempx) * tempw + (y - tempy) + 2) = tmp(x * wide + y + 2)
		writeblock(tmp, x - x_off, y - y_off, readblock(tmap, x, y))
	next
 next
 unloadtilemap tmap
 memcpy(@tmap, @tmp, sizeof(TileMap))
 'obviously don't free tmp
END SUB

SUB resizemapmenu (BYREF st AS MapEditState, map() AS TileMap, BYREF rs AS MapResizeState)
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
 FOR i AS INTEGER = 1 TO 6
  append_menu_item rs.menu, ""
 NEXT i
 rs.menu.items[5]->disabled = YES
 rs.menu.items[6]->disabled = YES

 DIM state AS MenuState
 state.active = YES
 init_menu_state state, rs.menu
 state.pt = 1
 state.last = 4
 
 DIM incval AS INTEGER = 0
 DIM drawoff AS XYPair
 
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
      ORELSE yesno("Cancel resize?") THEN
    rs.rect.wide = -1
    rs.rect.high = -1
    EXIT DO
   END IF
  END IF
  IF keyval(scF1) > 1 THEN show_help "resize_map"
  usemenu state
  IF keyval(scAlt) > 0 THEN incval = 8 ELSE incval = 1
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
  drawbox drawoff.x + rs.zoom * rs.rect.x, drawoff.y + rs.zoom * rs.rect.y, rs.zoom * rs.rect.wide, rs.zoom * rs.rect.high, 14 + state.tog, dpage

  SWAP dpage, vpage
  setvispage vpage
  dowait
 LOOP
 frame_unload @(rs.minimap)
EXIT SUB

END SUB

SUB resize_correct_width(BYREF st AS MapEditState, BYREF rs AS MapResizeState, map() AS TileMap)
 rs.rect.wide = bound(rs.rect.wide, 16, mapTilesMax)
 rs.rect.x = bound(rs.rect.x, -rs.rect.wide + 1, rs.oldsize.x - 1)
 WHILE rs.rect.high * rs.rect.wide > mapTilesMax AND rs.rect.high > 10
  rs.rect.high -= 1
 WEND
 resize_dimchange st, rs, map()
END SUB

SUB resize_correct_height(BYREF st AS MapEditState, BYREF rs AS MapResizeState, map() AS TileMap)
 rs.rect.high = bound(rs.rect.high, 10, mapTilesMax)
 rs.rect.y = bound(rs.rect.y, -rs.rect.high + 1, rs.oldsize.y - 1)
 WHILE rs.rect.high * rs.rect.wide > mapTilesMax AND rs.rect.wide > 16
  rs.rect.wide -= 1
 WEND
 resize_dimchange st, rs, map()
END SUB

SUB resize_dimchange(BYREF st AS MapEditState, BYREF rs AS MapResizeState, map() AS TileMap)
 WHILE rs.rect.high * rs.rect.wide > mapTilesMax
  rs.rect.high = large(rs.rect.high - 1, 10)
  rs.rect.wide = large(rs.rect.wide - 1, 16)
 WEND
 resize_rezoom_mini_map st, rs, map()
 resize_buildmenu rs
END SUB

SUB resize_buildmenu(BYREF rs AS MapResizeState)
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

SUB resize_rezoom_mini_map(BYREF st AS MapEditState, BYREF rs AS MapResizeState, map() AS TileMap)
 DIM lastzoom AS INTEGER
 lastzoom = rs.zoom
 DIM AS INTEGER tw, th
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

SUB show_minimap(BYREF st AS MapEditState, map() AS TileMap)
 DIM minimap AS Frame Ptr
 minimap = createminimap(map(), st.tilesets())

 clearpage vpage
 frame_draw minimap, NULL, 0, 0, 1, NO, vpage
 frame_unload @minimap

 edgeprint "Press Any Key", 0, 180, uilook(uiText), vpage
 setvispage vpage
 waitforanykey
END SUB

SUB paint_map_add_node(BYREF tlayer AS TileMap, BYVAL oldTile, BYVAL x, BYVAL y, BYREF head, queue() AS XYPair)
 IF (y < tlayer.high) AND (y >= 0) AND (x < tlayer.wide) AND (x >= 0) THEN
  IF readblock(tlayer, x, y) = oldTile THEN
   queue(head).x = x
   queue(head).y = y
   head = (head + 1) MOD UBOUND(queue)
  END IF
 END IF
END SUB

'tile fill tool: iterate through all contiguous maptiles, changing if the area continues, and stopping if it is blocked by a different kind of maptile
'do a breadth first search instead of using the stack; that's prone to overflow
SUB paint_map_area(st AS MapEditState, oldTile, x, y, map() AS TileMap, pass AS TileMap, defaults() AS DefArray, defpass)
 IF oldTile = st.usetile(st.layer) THEN EXIT SUB
 REDIM queue(250) AS XYPair 'a circular buffer. We don't use the last element
 DIM AS INTEGER head, tail, i, oldend
 paint_map_add_node map(st.layer), oldTile, x, y, head, queue()
 WHILE tail <> head
  'resizing inside paint_map_add_node would invalidate the WITH pointers, so make sure there's at least 4 empty slots
  IF (tail - head + UBOUND(queue)) MOD UBOUND(queue) <= 4 THEN
   oldend = UBOUND(queue)
   REDIM PRESERVE queue(UBOUND(queue) * 2)
   IF head < tail THEN
    FOR i = 0 TO head - 1
     queue(oldend + i) = queue(i)
    NEXT
    head += oldend
   END IF
  END IF

  WITH queue(tail)
   IF readblock(map(st.layer), .x, .y) = oldTile THEN
    writeblock map(st.layer), .x, .y, st.usetile(st.layer)
    IF defpass THEN calculatepassblock st, .x, .y, map(), pass, defaults()
    paint_map_add_node map(st.layer), oldTile, .x + 1, .y, head, queue()
    paint_map_add_node map(st.layer), oldTile, .x - 1, .y, head, queue()
    paint_map_add_node map(st.layer), oldTile, .x, .y + 1, head, queue()
    paint_map_add_node map(st.layer), oldTile, .x, .y - 1, head, queue()
   END IF
  END WITH
  tail = (tail + 1) MOD UBOUND(queue)
 WEND
END SUB

SUB loadpasdefaults (array() AS INTEGER, tilesetnum AS INTEGER)
 flusharray array(), 160, 0
 '--load defaults from tile set defaults file
 setpicstuf array(), 322, -1
 loadset workingdir & SLASH & "defpass.bin", tilesetnum, 0
 '--enforce magic number and filesize
 IF array(160) <> 4444 THEN
  flusharray array(), 160, 0
 END IF
END SUB

SUB savepasdefaults (array() AS INTEGER, tilesetnum AS INTEGER)
'--set magic number
array(160) = 4444
'--write defaults into tile set defaults file
setpicstuf array(), 322, -1
storeset workingdir & SLASH & "defpass.bin", tilesetnum, 0
END SUB

SUB mapedit_pickblock(BYREF st AS MapEditState)
 DIM tog AS INTEGER = 0
 setkeys
 DO
  setwait 80
  setkeys
  IF keyval(scEnter) > 1 OR keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "mapedit_tilemap_picktile"
  IF (keyval(scUp) AND 5) AND st.tilepick.y > 0 THEN st.tilepick.y -= 1: st.usetile(st.layer) = st.usetile(st.layer) - 16
  IF (keyval(scDown) AND 5) AND st.tilepick.y < 9 THEN st.tilepick.y += 1: st.usetile(st.layer) = st.usetile(st.layer) + 16
  IF (keyval(scLeft) AND 5) AND st.tilepick.x > 0 THEN st.tilepick.x -= 1: st.usetile(st.layer) = st.usetile(st.layer) - 1
  IF (keyval(scRight) AND 5) AND st.tilepick.x < 15 THEN st.tilepick.x += 1: st.usetile(st.layer) = st.usetile(st.layer) + 1
  IF (keyval(scComma) AND 5) AND st.usetile(st.layer) > 0 THEN
   st.usetile(st.layer) -= 1
   st.tilepick.x -= 1
   IF st.tilepick.x < 0 THEN st.tilepick.x = 15: st.tilepick.y -= 1
  END IF
  IF (keyval(scPeriod) AND 5) AND st.usetile(st.layer) < 159 THEN
   st.usetile(st.layer) += 1
   st.tilepick.x += 1
   IF st.tilepick.x > 15 THEN st.tilepick.x = 0: st.tilepick.y += 1
  END IF
  tog = tog XOR 1
  setmapdata , 0
  drawmap st.tilesetview, 0, 0, 0, st.tilesets(st.layer), dpage
  frame_draw st.cursor.sprite + tog, st.cursor.pal, st.tilepick.x * 20, st.tilepick.y * 20, , , dpage
  ' copypage dpage, vpage
  setvispage dpage
  dowait
 LOOP
 update_tilepicker st
END SUB
