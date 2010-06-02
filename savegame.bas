'OHRRPGCE GAME - Saving and loading games
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z

#include "compat.bi"
#include "udts.bi"
#include "allmodex.bi"
#include "common.bi"
#include "gglobals.bi"
#include "const.bi"
#include "loading.bi"
#include "reload.bi"
#include "reloadext.bi"
#include "savegame.bi"

#include "menustuf.bi"
#include "yetmore.bi"

USING Reload
USING Reload.Ext

'--Local subs and functions

DECLARE SUB new_savegame (BYVAL slot AS INTEGER)
DECLARE SUB gamestate_to_reload(BYVAL node AS Reload.NodePtr)
DECLARE SUB gamestate_state_to_reload(BYVAL parent AS Reload.NodePtr)
DECLARE SUB gamestate_script_to_reload(BYVAL parent AS Reload.NodePtr)
DECLARE SUB gamestate_globals_to_reload(BYVAL parent AS Reload.NodePtr)
DECLARE SUB gamestate_maps_to_reload(BYVAL parent AS Reload.NodePtr)
DECLARE SUB gamestate_npcs_to_reload(BYVAL parent AS Reload.NodePtr, BYVAL map AS INTEGER)
DECLARE SUB gamestate_tags_to_reload(BYVAL parent AS Reload.NodePtr)
DECLARE SUB gamestate_onetime_to_reload(BYVAL parent AS Reload.NodePtr)
DECLARE SUB gamestate_party_to_reload(BYVAL parent AS Reload.NodePtr)
DECLARE SUB gamestate_spelllist_to_reload(hero_slot AS INTEGER, spell_list AS INTEGER, BYVAL parent AS Reload.NodePtr)
DECLARE SUB gamestate_inventory_to_reload(BYVAL parent AS Reload.NodePtr)
DECLARE SUB gamestate_shops_to_reload(BYVAL parent AS Reload.NodePtr)
DECLARE SUB gamestate_vehicle_to_reload(BYVAL parent AS Reload.NodePtr)

DECLARE SUB new_loadgame(BYVAL slot AS INTEGER)
DECLARE SUB gamestate_from_reload(BYVAL node AS Reload.NodePtr)
DECLARE SUB gamestate_state_from_reload(BYVAL parent AS Reload.NodePtr)
DECLARE SUB gamestate_script_from_reload(BYVAL parent AS Reload.NodePtr)
DECLARE SUB gamestate_globals_from_reload(BYVAL parent AS Reload.NodePtr)
DECLARE SUB gamestate_maps_from_reload(BYVAL parent AS Reload.NodePtr)
DECLARE SUB gamestate_npcs_from_reload(BYVAL parent AS Reload.NodePtr, BYVAL map AS INTEGER)
DECLARE SUB gamestate_tags_from_reload(BYVAL parent AS Reload.NodePtr)
DECLARE SUB gamestate_onetime_from_reload(BYVAL parent AS Reload.NodePtr)
DECLARE SUB gamestate_party_from_reload(BYVAL parent AS Reload.NodePtr)
DECLARE SUB gamestate_spelllist_from_reload(hero_slot AS INTEGER, spell_list AS INTEGER, BYVAL parent AS Reload.NodePtr)
DECLARE SUB gamestate_inventory_from_reload(BYVAL parent AS Reload.NodePtr)
DECLARE SUB gamestate_shops_from_reload(BYVAL parent AS Reload.NodePtr)
DECLARE SUB gamestate_vehicle_from_reload(BYVAL parent AS Reload.NodePtr)

DECLARE SUB rsav_warn (s AS STRING)
DECLARE SUB rsav_warn_wrong (expected AS STRING, BYVAL n AS Reload.NodePtr)

'--old save/load support
DECLARE SUB old_savegame (slot as integer)
DECLARE SUB old_saveglobalvars (slot as integer, first as integer, last as integer)
DECLARE SUB old_loadgame (slot as integer)
DECLARE SUB old_loadglobalvars (slot as integer, first as integer, last as integer)
DECLARE SUB old_get_save_slot_preview(BYVAL slot AS INTEGER, pv AS SaveSlotPreview)
DECLARE SUB show_load_index(z AS INTEGER, caption AS STRING, slot AS INTEGER=0)
DECLARE SUB rebuild_inventory_captions (invent() AS InventSlot)
DECLARE FUNCTION old_save_slot_used (BYVAL slot AS INTEGER) AS INTEGER
DECLARE SUB old_erase_save_slot (BYVAL slot AS INTEGER)
DECLARE FUNCTION old_count_used_save_slots() AS INTEGER

DIM SHARED old_savefile AS STRING
DIM SHARED savedir AS STRING

DIM SHARED current_save_slot AS INTEGER '--used in rsav_warn

REM $STATIC
OPTION EXPLICIT

'-----------------------------------------------------------------------

SUB init_save_system()
 '--set up savegame file
 old_savefile = trimextension(sourcerpg) + ".sav"
 #IFDEF __UNIX__
 IF NOT fileisreadable(old_savefile) THEN
  'for a systemwide linux install, save files go in the prefs dir
  old_savefile = prefsdir & SLASH & trimpath(old_savefile)
 END IF
 #ENDIF
 savedir = prefsdir & SLASH & "saves"
 IF NOT isdir(savedir) THEN MKDIR savedir
END SUB

SUB savegame (BYVAL slot AS INTEGER)
 old_savegame slot
 new_savegame slot
END SUB

SUB saveglobalvars (BYVAL slot AS INTEGER, BYVAL first AS INTEGER, BYVAL last AS INTEGER)
 old_saveglobalvars slot, first, last
 'new_saveglobalvars slot, first, last
END SUB

SUB loadgame (BYVAL slot AS INTEGER)
 '--Works under the assumption that resetgame has already been called.
 IF keyval(scLeftShift) > 0 OR keyval(scRightShift) > 0 THEN
  'test new loading if shift is held down when you load
  DIM filename AS STRING
  filename = savedir & SLASH & slot & ".rsav"
  IF isfile(filename) THEN
   debug "testing awesome new loadgame from " & filename
   new_loadgame slot
   EXIT SUB
  END IF
 END IF
 debug "loading from slot " & slot & " of boring old " & old_savefile
 old_loadgame slot
END SUB

SUB loadglobalvars (BYVAL slot AS INTEGER, BYVAL first AS INTEGER, BYVAL last AS INTEGER)
 old_loadglobalvars slot, first, last
END SUB

SUB get_save_slot_preview(BYVAL slot AS INTEGER, pv AS SaveSlotPreview)
 old_get_save_slot_preview slot, pv
END SUB

FUNCTION save_slot_used (BYVAL slot AS INTEGER) AS INTEGER
 RETURN old_save_slot_used(slot)
END FUNCTION

SUB erase_save_slot (BYVAL slot AS INTEGER)
 old_erase_save_slot slot
END SUB

FUNCTION count_used_save_slots() AS INTEGER
 RETURN old_count_used_save_slots()
END FUNCTION

'-----------------------------------------------------------------------

SUB new_savegame (BYVAL slot AS INTEGER)
 current_save_slot = slot
 
 DIM doc AS DocPtr
 doc = CreateDocument()
 
 DIM node AS NodePtr
 node = CreateNode(doc, "rsav")
 SetRootNode(doc, node)
 gamestate_to_reload node
 
 DIM filename AS STRING
 filename = savedir & SLASH & slot & ".rsav"
 SerializeBin filename, doc
 
 FreeDocument doc
 current_save_slot = -1
END SUB

SUB new_loadgame(BYVAL slot AS INTEGER)
 current_save_slot = slot
 
 DIM filename AS STRING
 filename = savedir & SLASH & slot & ".rsav"

 IF NOT isfile(filename) THEN
  debug "Save file missing: " & filename
  EXIT SUB
 END IF

 DIM doc AS DocPtr
 doc = LoadDocument(filename)
 
 DIM node AS NodePtr
 node = DocumentRoot(doc)
 
 gamestate_from_reload node
 
 FreeDocument doc
 current_save_slot = -1
END SUB

SUB rsav_warn (s AS STRING)
 debug "Save slot " & current_save_slot & ": " & s
END SUB

SUB rsav_warn_wrong (expected AS STRING, BYVAL n AS Reload.NodePtr)
 rsav_warn "expected " & expected & " but found " & NodeName(n)
END SUB

'-----------------------------------------------------------------------

SUB gamestate_from_reload(BYVAL node AS Reload.NodePtr)
 IF NodeName(node) <> "rsav" THEN rsav_warn "root node is not rsav"

 IF GetChildNodeInt(node, "ver") > 0 THEN
  'FIXME: this should be a user-visible pop-up warning
  rsav_warn "new save file on old game player. Some data might get lost"
 END IF 

 gamestate_state_from_reload node
 gamestate_script_from_reload node
 gamestate_maps_from_reload node
 gamestate_tags_from_reload node
 gamestate_onetime_from_reload node
 gamestate_party_from_reload node
 gamestate_inventory_from_reload node
 gamestate_shops_from_reload node
 gamestate_vehicle_from_reload node

END SUB

SUB gamestate_state_from_reload(BYVAL parent AS Reload.NodePtr)
 DIM node AS NodePtr
 node = GetChildByName(parent, "state")
 DIM ch AS NodePtr 'used for sub-containers
 DIM n AS NodePtr 'used for numbered containers
 DIM i AS INTEGER
 
 gam.map.id = GetChildNodeInt(node, "current_map")

 DIM map_offset AS XYPair
 map_offset = load_map_pos_save_offset(gam.map.id)

 ch = GetChildByName(node, "caterpillar")
 n = FirstChild(ch)
 DO WHILE n
  IF NodeName(n) = "hero" THEN
   i = GetInteger(n)
   SELECT CASE i
    CASE 0 TO 3
     catx(i * 5) = GetChildNodeInt(n, "x") + map_offset.x * 20
     caty(i * 5) = GetChildNodeInt(n, "y") + map_offset.y * 20
     catd(i * 5) = GetChildNodeInt(n, "d")
    CASE ELSE
     rsav_warn "invalid caterpillar hero index " & i
   END SELECT
  ELSE
   rsav_warn_wrong "hero", n
  END IF
  n = NextSibling(n)
 LOOP

 gam.random_battle_countdown = GetChildNodeInt(node, "random_battle_countdown") 

 ch = GetChildByName(node, "camera") 
 mapx = GetChildNodeInt(ch, "x")
 mapy = GetChildNodeInt(ch, "y")
 gen(genCamera) = GetChildNodeInt(ch, "mode")
 FOR i = 0 TO 3
  gen(genCamArg1 + i) = GetChildNodeInt(ch, "arg" & i+1)
 NEXT i
 
 gold = GetChildNodeInt(node, "gold")

 ch = GetChildByName(node, "playtime")
 gen(genDays) = GetChildNodeInt(ch, "days")
 gen(genHours) = GetChildNodeInt(ch, "hours")
 gen(genMinutes) = GetChildNodeInt(ch, "minutes")
 gen(genSeconds) = GetChildNodeInt(ch, "seconds")

 ch = GetChildByName(node, "textbox")
 gen(genTextboxBackdrop) = GetChildNodeInt(ch, "backdrop")

 ch = GetChildByName(node, "status_char")
 gen(genPoison) = GetChildNodeInt(ch, "poison")
 gen(genStun) = GetChildNodeInt(ch, "stun")
 gen(genMute) = GetChildNodeInt(ch, "mute")

 gen(genDamageCap) = GetChildNodeInt(node, "damage_cap")

 ch = GetChildByName(node, "stats")
 n = FirstChild(ch)
 DO WHILE n
  IF NodeName(n) = "stat" THEN
   i = GetInteger(n)
   SELECT CASE i
    CASE 0 TO 11
     gen(genStatCap + i) = GetChildNodeInt(n, "cap")
    CASE ELSE
     rsav_warn "invalid stat cap index " & i
   END SELECT
  ELSE
   rsav_warn_wrong "stat", n
  END IF
  n = NextSibling(n)
 LOOP

END SUB

SUB gamestate_script_from_reload(BYVAL parent AS Reload.NodePtr)

 DIM node AS NodePtr
 node = GetChildByName(parent, "script")
 DIM ch AS NodePtr 'used for sub-containers

 gamestate_globals_from_reload node

 IF readbit(gen(), genBits2, 2) = 0 THEN
  gen(genGameoverScript) = GetChildNodeInt(node, "gameover_script")
  gen(genLoadgameScript) = GetChildNodeInt(node, "loadgame_script")
 END IF
 
 ch = GetChildByName(node, "suspend")
 
 setbit gen(), genSuspendBits, 0, GetChildNodeExists(ch, "npcs")
 setbit gen(), genSuspendBits, 1, GetChildNodeExists(ch, "player")
 setbit gen(), genSuspendBits, 2, GetChildNodeExists(ch, "obstruction")
 setbit gen(), genSuspendBits, 3, GetChildNodeExists(ch, "herowalls")
 setbit gen(), genSuspendBits, 4, GetChildNodeExists(ch, "npcwalls")
 setbit gen(), genSuspendBits, 5, GetChildNodeExists(ch, "caterpillar")
 setbit gen(), genSuspendBits, 6, GetChildNodeExists(ch, "randomenemies")
 setbit gen(), genSuspendBits, 7, GetChildNodeExists(ch, "boxadvance")
 setbit gen(), genSuspendBits, 8, GetChildNodeExists(ch, "overlay")
 setbit gen(), genSuspendBits, 9, GetChildNodeExists(ch, "ambientmusic")
 
 gen(genScrBackdrop) = GetChildNodeInt(node, "backdrop")
END SUB

SUB gamestate_globals_from_reload(BYVAL parent AS Reload.NodePtr)

 DIM node AS NodePtr
 node = GetChildByName(parent, "globals")
 DIM n AS NodePtr 'used for numbered containers
 DIM i AS INTEGER

 n = FirstChild(node)
 DO WHILE n
  IF NodeName(n) = "global" THEN
   i = GetInteger(n)
   SELECT CASE i
    CASE 0 TO 4095
     global(i) = GetChildNodeInt(n, "int")
    CASE ELSE
     rsav_warn "invalid global id " & i
   END SELECT
  ELSE
   rsav_warn_wrong "global", n
  END IF
  n = NextSibling(n)
 LOOP
END SUB

SUB gamestate_maps_from_reload(BYVAL parent AS Reload.NodePtr)
 DIM node AS NodePtr
 node = GetChildByName(parent, "maps")
 DIM n AS NodePtr 'used for numbered containers
 DIM i AS INTEGER
 DIM loaded_current AS INTEGER = NO
 
 'FIXME: currently only supports saving the current map
 n = FirstChild(node)
 DO WHILE n
  IF NodeName(n) = "map" THEN
   i = GetInteger(n)
   IF i = gam.map.id THEN
    gamestate_npcs_from_reload n, gam.map.id
    loaded_current = YES
   END IF
  ELSE
   rsav_warn_wrong "map", n
  END IF
  n = NextSibling(n)
 LOOP

 IF loaded_current = NO THEN
  rsav_warn "couldn't find saved data for current map " & gam.map.id
 END IF
 
END SUB

SUB gamestate_npcs_from_reload(BYVAL parent AS Reload.NodePtr, BYVAL map AS INTEGER)
 DIM node AS NodePtr
 node = GetChildByName(parent, "npcs")
 DIM n AS NodePtr 'used for numbered containers
 DIM i AS INTEGER

 DIM map_offset AS XYPair
 map_offset = load_map_pos_save_offset(map)

 n = FirstChild(node)
 DO WHILE n
  IF NodeName(n) = "npc" THEN
   i = GetInteger(n)
   SELECT CASE i
    CASE 0 TO 299
     IF GetChildNodeExists(n, "id") THEN
      npc(i).id = GetChildNodeInt(n, "id") + 1
      IF GetChildNodeExists(n, "hidden") THEN npc(i).id = npc(i).id * -1
      npc(i).x = GetChildNodeInt(n, "x") + map_offset.x * 20
      npc(i).y = GetChildNodeInt(n, "y") + map_offset.y * 20
      npc(i).dir = GetChildNodeInt(n, "d")
      npc(i).frame = GetChildNodeInt(n, "fr")
      npc(i).xgo = GetChildNodeInt(n, "xgo")
      npc(i).ygo = GetChildNodeInt(n, "ygo")
     END IF
    CASE ELSE
     rsav_warn "invalid npc instance " & i
   END SELECT
  ELSE
   rsav_warn_wrong "npc", n
  END IF
  n = NextSibling(n)
 LOOP

END SUB

SUB gamestate_tags_from_reload(BYVAL parent AS Reload.NodePtr)
 DIM node AS NodePtr
 node = GetChildByName(parent, "tags")
 
 DIM count AS INTEGER
 count = GetChildNodeInt(node, "count")
 IF count > 1000 THEN
  rsav_warn "too many saved tags 1000 < " & count
  count = 1000
 END IF

 DIM buf(INT(count / 16)) AS INTEGER

 DIM ch AS NodePtr
 ch = GetChildByName(node, "data")
 LoadBitsetArray(ch, buf(), UBOUND(buf))
 
 FOR i AS INTEGER = 0 TO count - 1
  setbit tag(), 0, i, readbit(buf(), 0, i)
 NEXT i
 
END SUB

SUB gamestate_onetime_from_reload(BYVAL parent AS Reload.NodePtr)
 DIM node AS NodePtr
 node = GetChildByName(parent, "onetime")
 
 DIM count AS INTEGER
 count = GetChildNodeInt(node, "count")
 IF count > 1032 THEN
  rsav_warn "too many saved tags 1032 < " & count
  count = 1032
 END IF

 DIM buf(INT(count / 16)) AS INTEGER

 DIM ch AS NodePtr
 ch = GetChildByName(node, "data")
 LoadBitsetArray(ch, buf(), UBOUND(buf))
 
 FOR i AS INTEGER = 0 TO count - 1
  setbit tag(), 0, 1000 + i, readbit(buf(), 0, i)
 NEXT i
 
END SUB

SUB gamestate_party_from_reload(BYVAL parent AS Reload.NodePtr)
 DIM node AS NodePtr
 node = GetChildByName(parent, "party")
 DIM slot AS NodePtr
 DIM ch AS NodePtr 'used for sub-containers
 DIM n AS NodePtr 'used for numbered containers
 DIM i AS INTEGER
 
 slot = FirstChild(node)
 DO WHILE slot
  IF NodeName(slot) = "slot" THEN
   i = GetInteger(slot)
   SELECT CASE i
    CASE 0 TO 40
    
     IF GetChildNodeExists(slot, "id") THEN
      hero(i) = GetChildNodeInt(slot, "id") + 1
     END IF
     
     names(i) = GetChildNodeStr(slot, "name")
     
     IF GetChildNodeExists(slot, "locked") THEN
      setbit hmask(), 0, i, YES
     END IF

     ch = GetChildByName(slot, "stats")
     DIM j AS INTEGER
     n = FirstChild(ch)
     DO WHILE n
      IF NodeName(n) = "stat" THEN
       j = GetInteger(n)
       SELECT CASE j
        CASE 0 TO 11
         gam.hero(i).stat.cur.sta(j) = GetChildNodeInt(n, "cur")
         gam.hero(i).stat.max.sta(j) = GetChildNodeInt(n, "max")
        CASE ELSE
         rsav_warn "invalid stat id " & j
       END SELECT
      ELSE
       rsav_warn_wrong "stat", n
      END IF
      n = NextSibling(n)
     LOOP

     WITH gam.hero(i)
     
      .lev = GetChildNodeInt(slot, "lev")
      .lev_gain = GetChildNodeInt(slot, "lev_gain")
      exlev(i, 0) = GetChildNodeInt(slot, "exp")
      exlev(i, 1) = GetChildNodeInt(slot, "exp_next")

      ch = GetChildByName(slot, "wep")
      .wep_pic = GetChildNodeInt(ch, "pic")
      .wep_pal = GetChildNodeInt(ch, "pal")

      ch = GetChildByName(slot, "in_battle")
      .battle_pic = GetChildNodeInt(ch, "pic")
      .battle_pal = GetChildNodeInt(ch, "pal")

      ch = GetChildByName(slot, "walkabout")
      .pic = GetChildNodeInt(ch, "pic")
      .pal = GetChildNodeInt(ch, "pal")

     END WITH
     
     ch = GetChildByName(slot, "battle_menus")
     n = FirstChild(ch)
     DO WHILE n
      IF NodeName(n) = "menu" THEN
       j = GetInteger(n)
       SELECT CASE j
        CASE 0 TO 5
         IF GetChildNodeExists(n, "attack") THEN
          bmenu(i, j) = GetChildNodeInt(n, "attack") + 1
         ELSEIF GetChildNodeExists(n, "items") THEN
          bmenu(i, j) = -10
         ELSEIF GetChildNodeExists(n, "spells") THEN
          bmenu(i, j) = (GetChildNodeInt(n, "spells") + 1) * -1
         END IF
        CASE ELSE
         rsav_warn "invalid battle menu id " & j
       END SELECT
      ELSE
       rsav_warn_wrong "menu", n
      END IF
      n = NextSibling(n)
     LOOP
     
     ch = GetChildByName(slot, "spell_lists")
     n = FirstChild(ch)
     DO WHILE n
      IF NodeName(n) = "list" THEN
       j = GetInteger(n)
       SELECT CASE j
        CASE 0 TO 3
         gamestate_spelllist_from_reload(i, j, n)
        CASE ELSE
         rsav_warn "invalid spell list id " & j
       END SELECT
      ELSE
       rsav_warn_wrong "list", n
      END IF
      n = NextSibling(n)
     LOOP 

     ch = GetChildByName(slot, "level_mp")
     n = FirstChild(ch)
     DO WHILE n
      IF NodeName(n) = "lev" THEN
       j = GetInteger(n)
       SELECT CASE j
        CASE 0 TO 7
         lmp(i, j) = GetChildNodeInt(n, "val")
        CASE ELSE
         rsav_warn "invalid level mp slot " & j
       END SELECT
      ELSE
       rsav_warn_wrong "lev", n
      END IF
      n = NextSibling(n)
     LOOP 

     ch = GetChildByName(slot, "equipment")
     n = FirstChild(ch)
     DO WHILE n
      IF NodeName(n) = "equip" THEN
       j = GetInteger(n)
       SELECT CASE j
        CASE 0 TO 4
         eqstuf(i, j) = GetChildNodeInt(n, "item") + 1
        CASE ELSE
         rsav_warn "invalid equip slot " & j
       END SELECT
      ELSE
       rsav_warn_wrong "equip", n
      END IF
      n = NextSibling(n)
     LOOP 

     DIM bitbuf(4) AS INTEGER

     ch = GetChildByName(slot, "elements")
     n = FirstChild(ch)
     DO WHILE n
      j = GetInteger(n)
      SELECT CASE NodeName(n)
       CASE "weak":   setbit bitbuf(), 0, j, YES
       CASE "strong": setbit bitbuf(), 0, j + 8, YES
       CASE "absorb": setbit bitbuf(), 0, j + 16, YES
       CASE ELSE
        rsav_warn_wrong "weak/strong/absorb", n
      END SELECT
      n = NextSibling(n)
     LOOP 

     IF GetChildNodeExists(slot, "rename_on_add") THEN setbit bitbuf(), 0, 24, YES
     IF GetChildNodeExists(slot, "rename_on_status") THEN setbit bitbuf(), 0, 25, YES
     IF GetChildNodeExists(slot, "hide_empty_lists") THEN setbit bitbuf(), 0, 26, YES

     FOR j = 0 TO 4
      nativehbits(i, j) = bitbuf(j)
     NEXT j

    CASE ELSE
     rsav_warn "invalid hero party slot " & i
   END SELECT
  ELSE
   rsav_warn_wrong "slot", slot
  END IF
  slot = NextSibling(slot)
 LOOP
END SUB

SUB gamestate_spelllist_from_reload(hero_slot AS INTEGER, spell_list AS INTEGER, BYVAL parent AS Reload.NodePtr)
 DIM node AS NodePtr
 node = GetChildByName(parent, "spells")
 IF spell_list <> GetInteger(node) THEN
  rsav_warn "spell list id mismatch " & spell_list & "<>" & GetInteger(node)
 END IF
 
 DIM n AS NodePtr 'used for numbered containers
 DIM i AS INTEGER

 DIM atk_id AS INTEGER
 n = FirstChild(node)
 DO WHILE n
  IF NodeName(n) = "spell" THEN
   i = GetInteger(n)
   SELECT CASE i
    CASE 0 TO 23
     atk_id = GetChildNodeInt(n, "attack")
     spell(hero_slot, spell_list, i) = atk_id + 1
    CASE ELSE
     rsav_warn "invalid spell list slot " & i
   END SELECT
  ELSE
   rsav_warn_wrong "spell", n
  END IF
  n = NextSibling(n)
 LOOP
 
END SUB

SUB gamestate_inventory_from_reload(BYVAL parent AS Reload.NodePtr)
 DIM node AS NodePtr
 node = GetChildByName(parent, "inventory")
 DIM ch AS NodePtr 'used for sub-containers
 DIM n AS NodePtr 'used for numbered containers
 DIM i AS INTEGER
 
 gen(genMaxInventory) = GetChildNodeInt(node, "size")

 ch = GetChildByName(node, "slots")
 DIM last AS INTEGER
 last = small(inventoryMax, UBOUND(inventory))
 
 n = FirstChild(ch)
 DO WHILE n
  IF NodeName(n) = "slot" THEN
   i = GetInteger(n)
   SELECT CASE i
    CASE 0 TO last
     WITH inventory(i)
      .used = YES
      .id = GetChildNodeInt(n, "item")
      .num = GetChildNodeInt(n, "num")
     END WITH
    CASE ELSE
     rsav_warn "invalid inventory slot id " & i
   END SELECT
  ELSE
   rsav_warn_wrong "slot", n
  END IF
  n = NextSibling(n)
 LOOP
 
 rebuild_inventory_captions inventory()
 
END SUB

SUB gamestate_shops_from_reload(BYVAL parent AS Reload.NodePtr)
 DIM node AS NodePtr
 node = GetChildByName(parent, "shops")
 DIM ch AS NodePtr 'used for sub-containers
 DIM n AS NodePtr 'used for numbered containers
 DIM n2 AS NodePtr 'also used for numbered containers
 DIM i AS INTEGER
 DIM j AS INTEGER

 DIM shoptmp(19) AS INTEGER

 n = FirstChild(node)
 DO WHILE n
  IF NodeName(n) = "shop" THEN
   i = GetInteger(n)
   SELECT CASE i
    CASE 0 TO gen(genMaxShop)
     loadrecord shoptmp(), game & ".sho", 20, i
     ch = GetChildByName(n, "slots")
     
     n2 = FirstChild(ch)
     DO WHILE n2
      IF NodeName(n2) = "slot" THEN
       j = GetInteger(n2)
       SELECT CASE j
        CASE 0 TO shoptmp(16)
         gam.stock(i, j) = GetChildNodeInt(n2, "stock", -1)
        CASE ELSE
         rsav_warn "invalid shop " & i & " stuff slot " & j
       END SELECT
      ELSE
       rsav_warn_wrong "slot", n2
      END IF
      n2 = NextSibling(n2)
     LOOP
     
    CASE ELSE
     rsav_warn "invalid shop id " & i
   END SELECT
  ELSE
   rsav_warn_wrong "shop", n
  END IF
  n = NextSibling(n)
 LOOP
 
END SUB

SUB gamestate_vehicle_from_reload(BYVAL parent AS Reload.NodePtr)
 DIM node AS NodePtr
 node = GetChildByName(parent, "vehicle")
 DIM ch AS NodePtr 'used for sub-containers
 DIM n AS NodePtr 'used for numbered containers
 
 WITH vstate
 
  ch = GetChildByName(node, "state")
  .active    = GetChildNodeInt(ch, "active")
  .npc       = GetChildNodeInt(ch, "npc")
  .old_speed = GetChildNodeInt(ch, "old_speed")
  IF GetChildNodeExists(ch, "mounting")        THEN .mounting = YES
  IF GetChildNodeExists(ch, "rising")          THEN .rising = YES
  IF GetChildNodeExists(ch, "falling")         THEN .falling = YES
  IF GetChildNodeExists(ch, "init_dismount")   THEN .init_dismount = YES
  IF GetChildNodeExists(ch, "trigger_cleanup") THEN .trigger_cleanup = YES
  IF GetChildNodeExists(ch, "ahead")           THEN .ahead = YES

  WITH .dat
  
   ch = GetChildByName(node, "def")
   .speed = GetChildNodeInt(ch, "speed")

   IF GetChildNodeExists(ch, "pass_walls")            THEN .pass_walls = YES
   IF GetChildNodeExists(ch, "pass_npcs")             THEN .pass_npcs = YES
   IF GetChildNodeExists(ch, "enable_npc_activation") THEN .enable_npc_activation = YES
   IF GetChildNodeExists(ch, "enable_door_use")       THEN .enable_door_use = YES
   IF GetChildNodeExists(ch, "do_not_hide_leader")    THEN .do_not_hide_leader = YES
   IF GetChildNodeExists(ch, "do_not_hide_party")     THEN .do_not_hide_party = YES
   IF GetChildNodeExists(ch, "dismount_ahead")        THEN .dismount_ahead = YES
   IF GetChildNodeExists(ch, "pass_walls_while_dismounting") THEN .pass_walls_while_dismounting = YES
   IF GetChildNodeExists(ch, "disable_flying_shadow") THEN .disable_flying_shadow = YES

   .random_battles = GetChildNodeInt(ch, "random_battles")
   .use_button     = GetChildNodeInt(ch, "use_button")
   .menu_button    = GetChildNodeInt(ch, "menu_button")
   .riding_tag     = GetChildNodeInt(ch, "riding_tag")
   .on_mount       = GetChildNodeInt(ch, "on_mount")
   .on_dismount    = GetChildNodeInt(ch, "on_dismount")
   .override_walls = GetChildNodeInt(ch, "override_walls")
   .blocked_by     = GetChildNodeInt(ch, "blocked_by")
   .mount_from     = GetChildNodeInt(ch, "mount_from")
   .dismount_to    = GetChildNodeInt(ch, "dismount_to")
   .elevation      = GetChildNodeInt(ch, "elevation")
  
  END WITH
 END WITH
END SUB

'-----------------------------------------------------------------------

SUB gamestate_to_reload(BYVAL node AS Reload.NodePtr)
 'increment this to produce a warning message when
 'loading a new rsav file in an old game player
 SetChildNode(node, "ver", 0)
 
 gamestate_state_to_reload node
 gamestate_script_to_reload node
 gamestate_maps_to_reload node
 gamestate_tags_to_reload node
 gamestate_onetime_to_reload node
 gamestate_party_to_reload node
 gamestate_inventory_to_reload node
 gamestate_shops_to_reload node
 gamestate_vehicle_to_reload node
END SUB

SUB gamestate_state_to_reload(BYVAL parent AS Reload.NodePtr)
 DIM node AS NodePtr
 node = SetChildNode(parent, "state")
 DIM ch AS NodePtr 'used for sub-containers
 DIM n AS NodePtr 'used for numbered containers
 
 SetChildNode(node, "current_map", gam.map.id)

 DIM map_offset AS XYPair
 map_offset = load_map_pos_save_offset(gam.map.id)

 ch = SetChildNode(node, "caterpillar")
 FOR i AS INTEGER = 0 TO 3
  n = AppendChildNode(ch, "hero", i)
  SetChildNode(n, "x", catx(i * 5) - map_offset.x * 20)
  SetChildNode(n, "y", caty(i * 5) - map_offset.y * 20)
  SetChildNode(n, "d", catd(i * 5))
 NEXT i

 SetChildNode(node, "random_battle_countdown", gam.random_battle_countdown)
 
 ch = SetChildNode(node, "camera")
 SetChildNode(ch, "x", mapx)
 SetChildNode(ch, "y", mapy)
 SetChildNode(ch, "mode", gen(genCamera))
 FOR i AS INTEGER = 0 TO 3
  SetChildNode(ch, "arg" & i+1, gen(genCamArg1 + i))
 NEXT i
 
 SetChildNode(node, "gold", gold)

 ch = SetChildNode(node, "playtime")
 SetChildNode(ch, "days", gen(genDays))
 SetChildNode(ch, "hours", gen(genHours))
 SetChildNode(ch, "minutes", gen(genMinutes))
 SetChildNode(ch, "seconds", gen(genSeconds))

 ch = SetChildNode(node, "textbox")
 SetChildNode(ch, "backdrop", gen(genTextboxBackdrop))
 
 ch = SetChildNode(node, "status_char")
 SetChildNode(ch, "poison", gen(genPoison))
 SetChildNode(ch, "stun", gen(genStun))
 SetChildNode(ch, "mute", gen(genMute))

 SetChildNode(node, "damage_cap", gen(genDamageCap))

 ch = SetChildNode(node, "stats")
 FOR i AS INTEGER = 0 TO 11
  n = AppendChildNode(ch, "stat", i)
  SetChildNode(n, "cap", gen(genStatCap + i))
 NEXT i

END SUB

SUB gamestate_script_to_reload(BYVAL parent AS Reload.NodePtr)
 'FIXME: currently only stores a tiny bit of script state, but could store
 'a lot more in the future

 DIM node AS NodePtr
 node = SetChildNode(parent, "script")
 DIM ch AS NodePtr 'used for sub-containers
 DIM n AS NodePtr 'used for numbered containers

 gamestate_globals_to_reload node

 SetChildNode(node, "gameover_script", gen(genGameoverScript))
 SetChildNode(node, "loadgame_script", gen(genLoadgameScript))
 
 ch = SetChildNode(node, "suspend")
 IF xreadbit(gen(), 0, genSuspendBits) THEN SetChildNode(ch, "npcs")
 IF xreadbit(gen(), 1, genSuspendBits) THEN SetChildNode(ch, "player")
 IF xreadbit(gen(), 2, genSuspendBits) THEN SetChildNode(ch, "obstruction")
 IF xreadbit(gen(), 3, genSuspendBits) THEN SetChildNode(ch, "herowalls")
 IF xreadbit(gen(), 4, genSuspendBits) THEN SetChildNode(ch, "npcwalls")
 IF xreadbit(gen(), 5, genSuspendBits) THEN SetChildNode(ch, "caterpillar")
 IF xreadbit(gen(), 6, genSuspendBits) THEN SetChildNode(ch, "randomenemies")
 IF xreadbit(gen(), 7, genSuspendBits) THEN SetChildNode(ch, "boxadvance")
 IF xreadbit(gen(), 8, genSuspendBits) THEN SetChildNode(ch, "overlay")
 IF xreadbit(gen(), 9, genSuspendBits) THEN SetChildNode(ch, "ambientmusic")
 
 SetChildNode(node, "backdrop", gen(genScrBackdrop))
END SUB

SUB gamestate_globals_to_reload(BYVAL parent AS Reload.NodePtr)

 DIM node AS NodePtr
 node = SetChildNode(parent, "globals")
 DIM n AS NodePtr 'used for numbered containers

 FOR i AS INTEGER = 0 TO 4095
  IF global(i) <> 0 THEN
   n = AppendChildNode(node, "global", i)
   SetChildNode(n, "int", global(i))
  END IF
 NEXT i
END SUB

SUB gamestate_maps_to_reload(BYVAL parent AS Reload.NodePtr)
 DIM node AS NodePtr
 node = SetChildNode(parent, "maps")
 DIM n AS NodePtr 'used for numbered containers
 
 'FIXME: currently only supports saving the current map
 n = AppendChildNode(node, "map", gam.map.id)
 gamestate_npcs_to_reload n, gam.map.id
 
END SUB

SUB gamestate_npcs_to_reload(BYVAL parent AS Reload.NodePtr, BYVAL map AS INTEGER)
 DIM node AS NodePtr
 node = SetChildNode(parent, "npcs")
 DIM n AS NodePtr 'used for numbered containers

 DIM map_offset AS XYPair
 map_offset = load_map_pos_save_offset(map)

 FOR i AS INTEGER = 0 TO 299
  IF npc(i).id <> 0 THEN
   n = AppendChildNode(node, "npc", i)
   SetChildNode(n, "id", ABS(npc(i).id) - 1)
   IF npc(i).id < 0 THEN SetChildNode(n, "hidden")
   SetChildNode(n, "x", npc(i).x - map_offset.x * 20)
   SetChildNode(n, "y", npc(i).y - map_offset.y * 20)
   SetChildNode(n, "d", npc(i).dir)
   SetChildNode(n, "fr", npc(i).frame)
   IF npc(i).xgo THEN SetChildNode(n, "xgo", npc(i).xgo)
   IF npc(i).ygo THEN SetChildNode(n, "ygo", npc(i).ygo)
  END IF
 NEXT i
END SUB

SUB gamestate_tags_to_reload(BYVAL parent AS Reload.NodePtr)
 DIM node AS NodePtr
 node = SetChildNode(parent, "tags")
 
 DIM count AS INTEGER = 1000
 SetChildNode(node, "count", count)
 
 DIM buf(INT(count / 16)) AS INTEGER
 FOR i AS INTEGER = 0 TO count - 1
  setbit buf(), 0, i, readbit(tag(), 0, i)
 NEXT i
 
 DIM ch AS NodePtr
 ch = SetChildNode(node, "data")
 SaveBitsetArray(ch, buf(), UBOUND(buf))
END SUB

SUB gamestate_onetime_to_reload(BYVAL parent AS Reload.NodePtr)
 DIM node AS NodePtr
 node = SetChildNode(parent, "onetime")
 
 DIM count AS INTEGER = 1032
 SetChildNode(node, "count", count)
 
 DIM buf(INT(count / 16)) AS INTEGER
 FOR i AS INTEGER = 0 TO count - 1
  setbit buf(), 0, i, readbit(tag(), 0, 1000 + i)
 NEXT i
 
 DIM ch AS NodePtr
 ch = SetChildNode(node, "data")
 SaveBitsetArray(ch, buf(), UBOUND(buf))
END SUB

SUB gamestate_party_to_reload(BYVAL parent AS Reload.NodePtr)
 DIM node AS NodePtr
 node = SetChildNode(parent, "party")
 DIM slot AS NodePtr
 DIM ch AS NodePtr 'used for sub-containers
 DIM n AS NodePtr 'used for numbered containers
 
 FOR i AS INTEGER = 0 TO 40
  slot = AppendChildNode(node, "slot", i)
  
  IF hero(i) > 0 THEN SetChildNode(slot, "id", hero(i) - 1)
  SetChildNode(slot, "name", names(i))
  
  IF xreadbit(hmask(), i) THEN
   SetChildNode(slot, "locked")
  END IF
  
  ch = SetChildNode(slot, "stats")
  FOR j AS INTEGER = 0 TO 11
   WITH gam.hero(i).stat
    IF .cur.sta(j) <> 0 OR .max.sta(j) <> 0 THEN
     n = AppendChildNode(ch, "stat", j)
     SetChildNode(n, "cur", .cur.sta(j))
     SetChildNode(n, "max", .max.sta(j))
    END IF
   END WITH
  NEXT j
  
  SetChildNode(slot, "lev", gam.hero(i).lev)
  SetChildNode(slot, "lev_gain", gam.hero(i).lev_gain)
  SetChildNode(slot, "exp", exlev(i, 0))
  SetChildNode(slot, "exp_next", exlev(i, 1))
  
  
  ch = SetChildNode(slot, "wep")
  SetChildNode(ch, "pic", gam.hero(i).wep_pic)
  SetChildNode(ch, "pal", gam.hero(i).wep_pal)
  
  ch = SetChildNode(slot, "in_battle")
  SetChildNode(ch, "pic", gam.hero(i).battle_pic)
  SetChildNode(ch, "pal", gam.hero(i).battle_pal)

  ch = SetChildNode(slot, "walkabout")
  SetChildNode(ch, "pic", gam.hero(i).pic)
  SetChildNode(ch, "pal", gam.hero(i).pal)
  
  ch = SetChildNode(slot, "battle_menus")
  FOR j AS INTEGER = 0 TO 5
   n = AppendChildNode(ch, "menu", j)
   SELECT CASE bmenu(i, j)
    CASE IS > 0:
     '--attack from weapon
     SetChildNode(n, "attack", bmenu(i, j) - 1)
    CASE -10:
     '--items menu
     SetChildNode(n, "items")
    CASE -4 TO -1:
     '--spell list
     SetChildNode(n, "spells", ABS(bmenu(i, j)) - 1)
   END SELECT
  NEXT j
  
  ch = SetChildNode(slot, "spell_lists")
  FOR j AS INTEGER = 0 TO 3
   n = AppendChildNode(ch, "list", j)
   gamestate_spelllist_to_reload(i, j, n)
  NEXT j
  
  ch = SetChildNode(slot, "level_mp")
  FOR j AS INTEGER = 0 TO 7
   IF lmp(i, j) <> 0 THEN
    n = AppendChildNode(ch, "lev", j)
    SetChildNode(n, "val", lmp(i, j))
   END IF
  NEXT j

  ch = SetChildNode(slot, "equipment")
  FOR j AS INTEGER = 0 TO 4
   IF eqstuf(i, j) > 0 THEN
    n = AppendChildNode(ch, "equip", j)
    SetChildNode(n, "item", eqstuf(i, j) - 1)
   END IF
  NEXT j
  
  DIM bitbuf(4) AS INTEGER
  FOR j AS INTEGER = 0 TO 4
   bitbuf(j) = nativehbits(i, j)
  NEXT j
  
  ch = SetChildNode(slot, "elements")
  FOR j AS INTEGER = 0 TO 7
   IF xreadbit(bitbuf(), j) THEN AppendChildNode(ch, "weak", j)
   IF xreadbit(bitbuf(), 8 + j) THEN AppendChildNode(ch, "strong", j)
   IF xreadbit(bitbuf(), 16 + j) THEN AppendChildNode(ch, "absorb", j)
  NEXT j
  
  IF xreadbit(bitbuf(), 24) THEN SetChildNode(slot, "rename_on_add")
  IF xreadbit(bitbuf(), 25) THEN SetChildNode(slot, "rename_on_status")
  IF xreadbit(bitbuf(), 26) THEN SetChildNode(slot, "hide_empty_lists")
  
 NEXT i
END SUB

SUB gamestate_spelllist_to_reload(hero_slot AS INTEGER, spell_list AS INTEGER, BYVAL parent AS Reload.NodePtr)
 DIM node AS NodePtr
 node = SetChildNode(parent, "spells", spell_list)
 DIM n AS NodePtr 'used for numbered containers

 DIM atk_id AS INTEGER
 FOR i AS INTEGER = 0 TO 23
  atk_id = spell(hero_slot, spell_list, i) - 1
  IF atk_id >= 0 THEN
   n = AppendChildNode(node, "spell", i)
   SetChildNode(n, "attack", atk_id)
  END IF
 NEXT i
 
END SUB

SUB gamestate_inventory_to_reload(BYVAL parent AS Reload.NodePtr)
 DIM node AS NodePtr
 node = SetChildNode(parent, "inventory")
 DIM ch AS NodePtr 'used for sub-containers
 DIM n AS NodePtr 'used for numbered containers
 
 SetChildNode(node, "size", gen(genMaxInventory))

 ch = SetChildNode(node, "slots")
 DIM last AS INTEGER
 last = small(inventoryMax, UBOUND(inventory))
 FOR i AS INTEGER = 0 TO last
  WITH inventory(i)
   IF .used THEN
    n = AppendChildNode(ch, "slot", i)
    SetChildNode(n, "item", .id)
    SetChildNode(n, "num", .num)
   END IF
  END WITH
 NEXT i
END SUB

SUB gamestate_shops_to_reload(BYVAL parent AS Reload.NodePtr)
 DIM node AS NodePtr
 node = SetChildNode(parent, "shops")
 DIM ch AS NodePtr 'used for sub-containers
 DIM n AS NodePtr 'used for numbered containers
 DIM n2 AS NodePtr 'also used for numbered containers

 DIM shoptmp(19) AS INTEGER
 
 FOR i AS INTEGER = 0 TO gen(genMaxShop)
  n = AppendChildNode(node, "shop", i)
  loadrecord shoptmp(), game & ".sho", 20, i
  ch = SetChildNode(n, "slots")
  FOR j AS INTEGER = 0 TO shoptmp(16)
   IF gam.stock(i, j) >= 0 THEN
    n2 = AppendChildNode(ch, "slot", j)
    SetChildNode(n2, "stock", gam.stock(i, j))
   END IF
  NEXT j
 NEXT i
 
END SUB

SUB gamestate_vehicle_to_reload(BYVAL parent AS Reload.NodePtr)
 DIM node AS NodePtr
 node = SetChildNode(parent, "vehicle")
 DIM ch AS NodePtr 'used for sub-containers
 DIM n AS NodePtr 'used for numbered containers
 
 WITH vstate
 
  ch = SetChildNode(node, "state")
  SetChildNode(ch, "active", .active)
  SetChildNode(ch, "npc", .npc)
  SetChildNode(ch, "old_speed", .old_speed)
  IF .mounting        THEN SetChildNode(ch, "mounting")
  IF .rising          THEN SetChildNode(ch, "rising")
  IF .falling         THEN SetChildNode(ch, "falling")
  IF .init_dismount   THEN SetChildNode(ch, "init_dismount")
  IF .trigger_cleanup THEN SetChildNode(ch, "trigger_cleanup")
  IF .ahead           THEN SetChildNode(ch, "ahead")

  WITH .dat
  
   ch = SetChildNode(node, "def")
   SetChildNode(ch, "speed", .speed)
   IF .pass_walls                   THEN SetChildNode(ch, "pass_walls")
   IF .pass_npcs                    THEN SetChildNode(ch, "pass_npcs")
   IF .enable_npc_activation        THEN SetChildNode(ch, "enable_npc_activation")
   IF .enable_door_use              THEN SetChildNode(ch, "enable_door_use")
   IF .do_not_hide_leader           THEN SetChildNode(ch, "do_not_hide_leader")
   IF .do_not_hide_party            THEN SetChildNode(ch, "do_not_hide_party")
   IF .dismount_ahead               THEN SetChildNode(ch, "dismount_ahead")
   IF .pass_walls_while_dismounting THEN SetChildNode(ch, "pass_walls_while_dismounting")
   IF .disable_flying_shadow        THEN SetChildNode(ch, "disable_flying_shadow")

   SetChildNode(ch, "random_battles", .random_battles)
   SetChildNode(ch, "use_button",     .use_button)
   SetChildNode(ch, "menu_button",    .menu_button)
   SetChildNode(ch, "riding_tag",     .riding_tag)
   SetChildNode(ch, "on_mount",       .on_mount)
   SetChildNode(ch, "on_dismount",    .on_dismount)
   SetChildNode(ch, "override_walls", .override_walls)
   SetChildNode(ch, "blocked_by",     .blocked_by)
   SetChildNode(ch, "mount_from",     .mount_from)
   SetChildNode(ch, "dismount_to",    .dismount_to)
   SetChildNode(ch, "elevation",      .elevation)
  
  END WITH
 END WITH
END SUB

'-----------------------------------------------------------------------

SUB old_savegame (slot)

DIM gmaptmp(dimbinsize(binMAP))

DIM AS INTEGER i, j, o, z

'--FLUSH BUFFER---
FOR i = 0 TO 16000
 buffer(i) = 0
NEXT i

buffer(0) = 3        'SAVEGAME VERSION NUMBER
buffer(1) = gam.map.id
loadrecord gmaptmp(), game + ".map", getbinsize(binMAP) / 2, gam.map.id
buffer(2) = catx(0) - gmaptmp(20) * 20
buffer(3) = caty(0) - gmaptmp(21) * 20
buffer(4) = catd(0)
buffer(5) = gam.random_battle_countdown
buffer(6) = 0    'was leader
buffer(7) = mapx
buffer(8) = mapy

DIM gold_str AS STRING = STR(gold)
FOR i = 0 TO 24
 IF i < LEN(gold_str) THEN
  IF MID$(gold_str, i + 1, 1) <> "" THEN buffer(i + 9) = ASC(MID$(gold_str, i + 1, 1))
 ELSE
  buffer(i + 9) = 0
 END IF
NEXT i

z = 34
FOR i = 0 TO 500
 IF i <= 104 THEN
  buffer(z) = gen(i)
 ELSE
  buffer(z) = 0
 END IF
 z = z + 1
NEXT i
SerNPCL npc(), z, buffer(), 300, gmaptmp(20), gmaptmp(21)
z=z+1 'fix an old bug
FOR i = 0 TO 126
 buffer(z) = tag(i): z = z + 1
NEXT i
FOR i = 0 TO 40
 buffer(z) = hero(i): z = z + 1
NEXT i
FOR i = 0 TO 500
 '--placeholder for old useless a() buffer
 buffer(z) = 0: z = z + 1
NEXT i
FOR i = 0 TO 40
 FOR j = 0 TO 11
  buffer(z) = gam.hero(i).stat.cur.sta(j): z = z + 1
 NEXT j
 buffer(z) = gam.hero(i).lev: z = z + 1
 buffer(z) = gam.hero(i).wep_pic: z = z + 1
 FOR j = 0 TO 11
  buffer(z) = gam.hero(i).stat.max.sta(j): z = z + 1
 NEXT j
 buffer(z) = gam.hero(i).lev_gain: z = z + 1
 buffer(z) = gam.hero(i).wep_pal: z = z + 1
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 5
  buffer(z) = bmenu(i, o): z = z + 1
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 3
  FOR j = 0 TO 23
   buffer(z) = spell(i, o, j): z = z + 1
  NEXT j
  z = z + 1
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 7
  buffer(z) = lmp(i, o): z = z + 1
 NEXT o
NEXT i
DIM exp_str AS STRING
FOR i = 0 TO 40
 FOR o = 0 TO 1
  exp_str = STR$(exlev(i, o))
  FOR j = 0 TO 25
   IF j < LEN(exp_str) THEN
    IF MID$(exp_str, j + 1, 1) <> "" THEN buffer(z) = ASC(MID$(exp_str, j + 1, 1))
   ELSE
    buffer(z) = 0
   END IF
   z = z + 1
  NEXT j
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR j = 0 TO 16
  IF j < LEN(names(i)) THEN
   IF MID$(names(i), j + 1, 1) <> "" THEN buffer(z) = ASC(MID$(names(i), j + 1, 1))
  END IF
  z = z + 1
 NEXT j
NEXT i
'Store old-style 8-bit inventory (provides a little compatability for new SAV files with old game versions)
SerInventory8bit inventory(), z, buffer()
FOR i = 0 TO 40
 FOR o = 0 TO 4
  buffer(z) = eqstuf(i, o): z = z + 1
 NEXT o
NEXT i
'Store new 16-bit inventory (Only the first 100 elements fit into this buffer!)
SaveInventory16Bit inventory(), z, buffer(), 0, 99
setpicstuf buffer(), 30000, -1
DIM sg AS STRING = old_savefile
storeset sg, slot * 2, 0

'---RECORD 2

'--FLUSH BUFFER---
FOR i = 0 TO 16000
 buffer(i) = 0
NEXT i

z = 0

FOR i = 0 TO 99
 FOR o = 0 TO 49
  buffer(z) = gam.stock(i, o): z = z + 1
 NEXT o
NEXT i
FOR i = 0 TO 3
 buffer(z) = hmask(i): z = z + 1
NEXT i
FOR i = 1 TO 3
 buffer(z) = catx(i * 5) - gmaptmp(20) * 20: z = z + 1
 buffer(z) = caty(i * 5) - gmaptmp(21) * 20: z = z + 1
 buffer(z) = catd(i * 5): z = z + 1
NEXT i
'--bottom 16 bits of each global variable in 5013 - 6037
FOR i = 0 TO 1024
 buffer(z) = global(i): z = z + 1
NEXT i
'--vehicle data
WITH vstate
 buffer(z+0) = .active
 buffer(z+5) = .npc
 setbit buffer(), z+6, 0, .mounting
 setbit buffer(), z+6, 1, .rising
 setbit buffer(), z+6, 2, .falling
 setbit buffer(), z+6, 3, .init_dismount
 setbit buffer(), z+6, 4, .trigger_cleanup
 setbit buffer(), z+6, 5, .ahead
 buffer(z+7) = .old_speed
 WITH .dat
  buffer(z+8) = .speed 
  setbit buffer(), z+9, 0, .pass_walls
  setbit buffer(), z+9, 1, .pass_npcs
  setbit buffer(), z+9, 2, .enable_npc_activation
  setbit buffer(), z+9, 3, .enable_door_use
  setbit buffer(), z+9, 4, .do_not_hide_leader
  setbit buffer(), z+9, 5, .do_not_hide_party
  setbit buffer(), z+9, 6, .dismount_ahead
  setbit buffer(), z+9, 7, .pass_walls_while_dismounting
  setbit buffer(), z+9, 8, .disable_flying_shadow
  buffer(z+11) = .random_battles
  buffer(z+12) = .use_button
  buffer(z+13) = .menu_button
  buffer(z+14) = .riding_tag
  buffer(z+15) = .on_mount
  buffer(z+16) = .on_dismount
  buffer(z+17) = .override_walls
  buffer(z+18) = .blocked_by
  buffer(z+19) = .mount_from
  buffer(z+20) = .dismount_to
  buffer(z+21) = .elevation
 END WITH
END WITH
z += 22
'--picture and palette
buffer(z) = 4444: z = z + 1 'magic number
FOR i = 0 TO 40
 buffer(z) = gam.hero(i).battle_pic: z = z + 1
 buffer(z) = gam.hero(i).battle_pal: z = z + 1
 buffer(z) = gam.hero(i).def_wep: z = z + 1
 buffer(z) = gam.hero(i).pic: z = z + 1
 buffer(z) = gam.hero(i).pal: z = z + 1
 z = z + 1 'skip an int
NEXT i
'--native hero bitsets
buffer(z) = 4444: z = z + 1 'magic number
FOR i = 0 TO 40
 FOR o = 0 TO 4
  buffer(z) = nativehbits(i, o): z = z + 1
 NEXT o
NEXT i
'--top 16 bits of each global variable in 6513 - 7537
FOR i = 0 TO 1024
 buffer(z) = global(i) shr 16: z = z + 1
NEXT
'--3071 more globals in 7538 - 13679
FOR i = 1025 TO 4095
 buffer(z) = global(i): z = z + 1
 buffer(z) = global(i) shr 16: z = z + 1
NEXT i
'Store the rest of 16-bit inventory
IF inventoryMax <> 599 THEN debug "Warning: inventoryMax=" & inventoryMax & ", does not fit in old SAV format"
SaveInventory16Bit inventory(), z, buffer(), 100, 599

setpicstuf buffer(), 30000, -1
sg = old_savefile
storeset sg, slot * 2 + 1, 0

'See http://gilgamesh.hamsterrepublic.com/wiki/ohrrpgce/index.php/SAV for docs
END SUB

SUB old_saveglobalvars (slot, first, last)
DIM i AS INTEGER
DIM buf((last - first + 1) * 2) = ANY
DIM fh AS INTEGER = FREEFILE
OPEN old_savefile FOR BINARY AS #fh
IF first <= 1024 THEN
 'output first-final
 DIM final AS INTEGER = small(1024, last)
 FOR i = 0 TO final - first
  buf(i) = global(first + i)
 NEXT
 SEEK #fh, 60000 * slot + 2 * first + 40027  '20013 * 2 + 1
 storerecord buf(), fh, final - first + 1, -1
 FOR i = 0 TO final - first
  buf(i) = global(first + i) shr 16
 NEXT
 SEEK #fh, 60000 * slot + 2 * first + 43027  '21513 * 2 + 1
 storerecord buf(), fh, final - first + 1, -1
END IF
IF last >= 1025 THEN
 'output start-last
 DIM start AS INTEGER = large(1025, first)
 FOR i = 0 TO last - start
  buf(i * 2) = global(start + i)
  buf(i * 2 + 1) = global(start + i) shr 16
 NEXT
 SEEK #fh, 60000 * slot + 4 * (start - 1025) + 45077  '22538 * 2 + 1
 storerecord buf(), fh, (last - start + 1) * 2, -1
END IF
CLOSE #fh
END SUB

SUB old_loadgame (slot)
DIM gmaptmp(dimbinsize(binMAP))

DIM AS INTEGER i, j, o, z

'--return gen to defaults
xbload game + ".gen", gen(), "General data is missing from " + game

DIM sg AS STRING = old_savefile
setpicstuf buffer(), 30000, -1
loadset sg, slot * 2, 0

DIM savver AS INTEGER = buffer(0)
IF savver < 2 OR savver > 3 THEN EXIT SUB
gam.map.id = buffer(1)
loadrecord gmaptmp(), game + ".map", getbinsize(binMAP) / 2, gam.map.id
catx(0) = buffer(2) + gmaptmp(20) * 20
caty(0) = buffer(3) + gmaptmp(21) * 20
catd(0) = buffer(4)
gam.random_battle_countdown = buffer(5)
'leader = buffer(6)
mapx = buffer(7)
mapy = buffer(8)

DIM gold_str AS STRING = ""
FOR i = 0 TO 24
 IF buffer(i + 9) < 0 OR buffer(i + 9) > 255 THEN buffer(i + 9) = 0
 IF buffer(i + 9) > 0 THEN gold_str &= CHR(buffer(i + 9))
NEXT i
gold = str2int(gold_str)

z = 34
show_load_index z, "gen"
FOR i = 0 TO 500
 SELECT CASE i
  'Only certain gen() values should be read from the saved game.
  'See http://HamsterRepublic.com/ohrrpgce/index.php/GEN.html
  CASE 42, 57 'genGameoverScript and genLoadgameScript
   IF readbit(gen(), genBits2, 2) = 0 THEN
    gen(i) = buffer(z)
   END IF
  CASE 44 TO 54, 58, 60 TO 76, 85
   gen(i) = buffer(z)
 END SELECT
 z = z + 1
NEXT i

show_load_index z, "npcl"
DeserNPCL npc(),z,buffer(),300,gmaptmp(20),gmaptmp(21)
show_load_index z, "unused"
z=z+1 'fix an old bug

show_load_index z, "tags"
FOR i = 0 TO 126
 tag(i) = buffer(z): z = z + 1
NEXT i
show_load_index z, "heroes"
FOR i = 0 TO 40
 hero(i) = buffer(z): z = z + 1
NEXT i
show_load_index z, "unused a"
FOR i = 0 TO 500
 '--used to be the useless a() buffer
 z = z + 1
NEXT i
show_load_index z, "stats"
FOR i = 0 TO 40
 FOR j = 0 TO 11
  gam.hero(i).stat.cur.sta(j) = buffer(z): z = z + 1
 NEXT j
 gam.hero(i).lev = buffer(z): z = z + 1
 gam.hero(i).wep_pic = buffer(z): z = z + 1
 FOR j = 0 TO 11
  gam.hero(i).stat.max.sta(j) = buffer(z): z = z + 1
 NEXT j
 gam.hero(i).lev_gain = buffer(z): z = z + 1
 gam.hero(i).wep_pal = buffer(z): z = z + 1
NEXT i
show_load_index z, "bmenu"
FOR i = 0 TO 40
 FOR o = 0 TO 5
  bmenu(i, o) = buffer(z): z = z + 1
 NEXT o
NEXT i
show_load_index z, "spell"
FOR i = 0 TO 40
 FOR o = 0 TO 3
  FOR j = 0 TO 23
   spell(i, o, j) = buffer(z): z = z + 1
  NEXT j
  z = z + 1'--skip extra data
 NEXT o
NEXT i
show_load_index z, "lmp"
FOR i = 0 TO 40
 FOR o = 0 TO 7
  lmp(i, o) = buffer(z): z = z + 1
 NEXT o
NEXT i
show_load_index z, "exlev"
DIM exp_str AS STRING
FOR i = 0 TO 40
 FOR o = 0 TO 1
  exp_str = ""
  FOR j = 0 TO 25
   IF buffer(z) < 0 OR buffer(z) > 255 THEN buffer(z) = 0
   IF buffer(z) > 0 THEN exp_str &= CHR(buffer(z))
   z = z + 1
  NEXT j
  exlev(i, o) = str2int(exp_str)
 NEXT o
NEXT i
show_load_index z, "names"
FOR i = 0 TO 40
 names(i) = ""
 FOR j = 0 TO 16
  IF buffer(z) < 0 OR buffer(z) > 255 THEN buffer(z) = 0
  IF buffer(z) > 0 THEN names(i) &= CHR(buffer(z))
  z = z + 1
 NEXT j
NEXT i

show_load_index z, "inv_mode"
DIM inv_mode AS INTEGER
inv_mode = buffer(z)
show_load_index z, "inv 8bit"
IF inv_mode = 0 THEN ' Read 8-bit inventory data from old SAV files
 DeserInventory8Bit inventory(), z, buffer()
ELSE
 'Skip this section
 z = 14595
END IF

show_load_index z, "eqstuff"
FOR i = 0 TO 40
 FOR o = 0 TO 4
  eqstuf(i, o) = buffer(z): z = z + 1
 NEXT o
NEXT i

show_load_index z, "inv 16bit"
IF inv_mode = 1 THEN ' Read 16-bit inventory data from newer SAV files
 LoadInventory16Bit inventory(), z, buffer(), 0, 99
END IF
show_load_index z, "after inv 16bit"

'RECORD 2

setpicstuf buffer(), 30000, -1
loadset sg, slot * 2 + 1, 0

z = 0

show_load_index z, "stock", 1
FOR i = 0 TO 99
 FOR o = 0 TO 49
  gam.stock(i, o) = buffer(z): z = z + 1
 NEXT o
NEXT i
show_load_index z, "hmask", 1
FOR i = 0 TO 3
 hmask(i) = buffer(z): z = z + 1
NEXT i
show_load_index z, "cathero", 1
FOR i = 1 TO 3
 catx(i * 5) = buffer(z) + gmaptmp(20) * 20: z = z + 1
 caty(i * 5) = buffer(z) + gmaptmp(21) * 20: z = z + 1
 catd(i * 5) = buffer(z): z = z + 1
NEXT i
show_load_index z, "globals low", 1
FOR i = 0 TO 1024
 global(i) = (buffer(z) AND &hFFFF): z = z + 1
NEXT i
show_load_index z, "vstate", 1
WITH vstate
 .active = buffer(z+0) <> 0
 .npc    = buffer(z+5)
 .mounting        = xreadbit(buffer(), 0, z+6)
 .rising          = xreadbit(buffer(), 1, z+6)
 .falling         = xreadbit(buffer(), 2, z+6)
 .init_dismount   = xreadbit(buffer(), 3, z+6)
 .trigger_cleanup = xreadbit(buffer(), 4, z+6)
 .ahead           = xreadbit(buffer(), 5, z+6)
 .old_speed = buffer(z+7)
 WITH .dat
  .speed = buffer(z+8)
  .pass_walls = xreadbit(buffer(), 0, z+9)
  .pass_npcs  = xreadbit(buffer(), 1, z+9)
  .enable_npc_activation = xreadbit(buffer(), 2, z+9)
  .enable_door_use       = xreadbit(buffer(), 3, z+9)
  .do_not_hide_leader    = xreadbit(buffer(), 4, z+9)
  .do_not_hide_party     = xreadbit(buffer(), 5, z+9)
  .dismount_ahead        = xreadbit(buffer(), 6, z+9)
  .pass_walls_while_dismounting = xreadbit(buffer(), 7, z+9)
  .disable_flying_shadow        = xreadbit(buffer(), 8, z+9)
  .random_battles = buffer(z+11)
  .use_button     = buffer(z+12)
  .menu_button    = buffer(z+13)
  .riding_tag     = buffer(z+14)
  .on_mount       = buffer(z+15)
  .on_dismount    = buffer(z+16)
  .override_walls = buffer(z+17)
  .blocked_by     = buffer(z+18)
  .mount_from     = buffer(z+19)
  .dismount_to    = buffer(z+20)
  .elevation      = buffer(z+21)
 END WITH
END WITH
z += 22
'--picture and palette
show_load_index z, "picpal magic", 1
DIM picpalmagicnum AS INTEGER = buffer(z): z = z + 1
show_load_index z, "picpalwep", 1
FOR i = 0 TO 40
 IF picpalmagicnum = 4444 THEN
  gam.hero(i).battle_pic = buffer(z)
  gam.hero(i).battle_pal = buffer(z+1)
  gam.hero(i).def_wep = buffer(z+2)
  gam.hero(i).pic = buffer(z+3)
  gam.hero(i).pal = buffer(z+4)
 END IF
 z = z + 6
NEXT i
'native hero bitsets
show_load_index z, "hbit magic", 1
DIM nativebitmagicnum AS INTEGER = buffer(z): z = z + 1
show_load_index z, "hbits", 1
FOR i = 0 TO 40
 FOR o = 0 TO 4
  IF nativebitmagicnum = 4444 THEN nativehbits(i, o) = buffer(z)
  z = z + 1
 NEXT o
NEXT i
'top global variable bits
show_load_index z, "global high", 1
FOR i = 0 TO 1024
 global(i) or= buffer(z) shl 16: z = z + 1
NEXT i
show_load_index z, "global ext", 1
FOR i = 1025 TO 4095
 global(i) = buffer(z) and &hFFFF: z = z + 1
 global(i) or= buffer(z) shl 16: z = z + 1
NEXT i
show_load_index z, "inv 16bit ext", 1
IF inv_mode = 1 THEN ' Read 16-bit inventory data from newer SAV files
 IF inventoryMax <> 599 THEN debug "Warning: inventoryMax=" & inventoryMax & ", does not fit in old SAV format"
 LoadInventory16Bit inventory(), z, buffer(), 100, 599
ELSE
 'skip this section for old saves
 z = 29680 - 15000
END IF
show_load_index z, "unused", 1
rebuild_inventory_captions inventory()

'---BLOODY BACKWARD COMPATABILITY---
'fix doors...
IF savver = 2 THEN gen(genVersion) = 3

dim her as herodef

IF picpalmagicnum <> 4444 THEN
 '--fix appearance settings
 FOR i = 0 TO 40
  IF hero(i) > 0 THEN
   loadherodata @her, hero(i) - 1
   gam.hero(i).battle_pic = her.sprite
   gam.hero(i).battle_pal = her.sprite_pal
   gam.hero(i).pic = her.walk_sprite
   gam.hero(i).pal = her.walk_sprite_pal
   gam.hero(i).def_wep = her.def_weapon + 1'default weapon
  END IF
 NEXT i
END IF

IF nativebitmagicnum <> 4444 THEN
 '--fix native hero bits
 FOR i = 0 TO 40
  IF hero(i) > 0 THEN
   loadherodata @her, hero(i) - 1
   FOR i = 0 TO 2 '??
    nativehbits(i, i) = her.bits(i)
   NEXT i
  END IF
 NEXT i
END IF

'See http://gilgamesh.hamsterrepublic.com/wiki/ohrrpgce/index.php/SAV for docs
END SUB

SUB old_loadglobalvars (slot, first, last)
DIM i AS INTEGER
DIM buf((last - first + 1) * 2) = ANY
IF isfile(old_savefile) THEN
 DIM fh AS INTEGER = FREEFILE
 OPEN old_savefile FOR BINARY AS #fh

 IF first <= 1024 THEN
  'grab first-final
  DIM final AS INTEGER = small(1024, last)
  SEEK #fh, 60000 * slot + 2 * first + 40027  '20013 * 2 + 1
  loadrecord buf(), fh, final - first + 1, -1
  FOR i = 0 TO final - first
   global(first + i) = buf(i) and &hFFFF
  NEXT
  SEEK #fh, 60000 * slot + 2 * first + 43027  '21513 * 2 + 1
  loadrecord buf(), fh, final - first + 1, -1
  FOR i = 0 TO final - first
   global(first + i) or= buf(i) shl 16
  NEXT
 END IF
 IF last >= 1025 THEN
  'grab start-last
  DIM start AS INTEGER = large(1025, first)
  SEEK #fh, 60000 * slot + 4 * (start - 1025) + 45077  '22538 * 2 + 1
  loadrecord buf(), fh, (last - start + 1) * 2, -1
  FOR i = 0 TO last - start
   global(start + i) = buf(i * 2) and &hFFFF
   global(start + i) or= buf(i * 2 + 1) shl 16
  NEXT
 END IF

 CLOSE #fh
ELSE
 FOR i = first TO last
  global(i) = 0
 NEXT
END IF
END SUB

SUB show_load_index(z AS INTEGER, caption AS STRING, slot AS INTEGER=0)
 'debug "SAV:" & LEFT(caption & STRING(20, " "), 20) & " int=" & z + slot * 15000
END SUB

SUB rebuild_inventory_captions (invent() AS InventSlot)
 DIM i AS INTEGER
 FOR i = 0 TO inventoryMax
  update_inventory_caption i
 NEXT i
END SUB

SUB old_get_save_slot_preview(BYVAL slot AS INTEGER, pv AS SaveSlotPreview)

 setpicstuf buffer(), 30000, -1
 loadset old_savefile, slot * 2, 0
 
 IF buffer(0) <> 3 THEN
  '--currently only understands v3 binary sav format
  pv.valid = NO
  EXIT SUB
 END IF
 
 pv.valid = YES
 pv.cur_map = buffer(1)

 '-get stats
 DIM z AS INTEGER = 3305
 FOR i AS INTEGER = 0 TO 3
  FOR j AS INTEGER = 0 TO 11
   pv.hero(i).stat.cur.sta(j) = buffer(z): z += 1
  NEXT j
  pv.hero(i).lev = buffer(z): z += 1
  z += 1 'skip weapon pic because we could care less right now
  FOR j AS INTEGER = 0 TO 11
   pv.hero(i).stat.max.sta(j) = buffer(z): z += 1
  NEXT j
 NEXT i

 '--get play time
 z = 34 + 51
 pv.playtime = playtime(buffer(z), buffer(z + 1), buffer(z + 2))

 '--leader data
 DIM foundleader AS INTEGER = NO
 pv.leader_name = ""
 FOR o AS INTEGER = 0 TO 3
  '--load hero ID
  pv.hero_id(o) = buffer(2763 + o)
  '--leader name and level
  IF foundleader = NO AND pv.hero_id(o) > 0 THEN
   foundleader = YES
   FOR j AS INTEGER = 0 TO 15
    DIM k AS INTEGER = buffer(11259 + (o * 17) + j)
    IF k > 0 AND k < 255 THEN pv.leader_name &= CHR(k)
   NEXT j
   pv.leader_lev = pv.hero(o).lev
  END IF
 NEXT o

 '--load second record
 loadset old_savefile, slot * 2 + 1, 0

 z = 6060
 pv.use_saved_pics = NO
 IF buffer(z) = 4444 THEN pv.use_saved_pics = YES
 z += 1
 IF pv.use_saved_pics THEN
  FOR i AS INTEGER = 0 TO 3
   pv.hero(i).battle_pic = buffer(z)
   pv.hero(i).battle_pal = buffer(z+1)
   pv.hero(i).def_wep = buffer(z+2)
   pv.hero(i).pic = buffer(z+3)
   pv.hero(i).pal = buffer(z+4)
   z += 6
  NEXT i
 END IF

END SUB

FUNCTION old_save_slot_used (BYVAL slot AS INTEGER) AS INTEGER
 DIM AS SHORT saveversion
 DIM savh AS INTEGER = FREEFILE
 OPEN old_savefile FOR BINARY AS #savh
 GET #savh, 1 + 60000 * slot, saveversion
 CLOSE #savh
 RETURN (saveversion = 3)
END FUNCTION

SUB old_erase_save_slot (BYVAL slot AS INTEGER)
 DIM AS SHORT saveversion = 0
 IF fileisreadable(old_savefile) = NO THEN EXIT SUB
 DIM savh AS INTEGER = FREEFILE
 OPEN old_savefile FOR BINARY AS #savh
 IF LOF(savh) > 60000 * slot THEN
  PUT #savh, 1 + 60000 * slot, saveversion
 END IF
 CLOSE #savh
END SUB

FUNCTION old_count_used_save_slots() AS INTEGER
 DIM i AS INTEGER
 DIM n AS INTEGER
 DIM savver AS INTEGER
 n = 0
 setpicstuf buffer(), 30000, -1
 FOR i = 0 TO 3
  loadset old_savefile, i * 2, 0
  savver = buffer(0)
  IF savver = 3 THEN n += 1
 NEXT i
 RETURN n
END FUNCTION
