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

#include "menustuf.bi"

'--Local subs and functions

'--old save/load support
DECLARE SUB old_savegame (slot as integer, stat() as integer)
DECLARE SUB old_saveglobalvars (slot as integer, first as integer, last as integer)
DECLARE SUB old_loadgame (slot as integer, stat() as integer)
DECLARE SUB old_loadglobalvars (slot as integer, first as integer, last as integer)
DECLARE SUB show_load_index(z AS INTEGER, caption AS STRING, slot AS INTEGER=0)
DECLARE SUB rebuild_inventory_captions (invent() AS InventSlot)


REM $STATIC
OPTION EXPLICIT

'-----------------------------------------------------------------------

SUB savegame (slot, stat())
 old_savegame slot, stat()
END SUB

SUB saveglobalvars (slot, first, last)
 old_saveglobalvars slot, first, last
END SUB

SUB loadgame (slot, stat())
 old_loadgame slot, stat()
END SUB

SUB loadglobalvars (slot, first, last)
 old_loadglobalvars slot, first, last
END SUB

'-----------------------------------------------------------------------

SUB old_savegame (slot, stat())

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
 FOR o = 0 TO 1
  FOR j = 0 TO 13
   buffer(z) = stat(i, o, j): z = z + 1
  NEXT j
 NEXT o
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
DIM sg AS STRING = savefile
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
 FOR o = 0 TO 1
  FOR j = 14 TO 16
   buffer(z) = stat(i, o, j): z = z + 1
  NEXT j
 NEXT o
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
sg = savefile
storeset sg, slot * 2 + 1, 0

'See http://gilgamesh.hamsterrepublic.com/wiki/ohrrpgce/index.php/SAV for docs
END SUB

SUB old_saveglobalvars (slot, first, last)
DIM i AS INTEGER
DIM buf((last - first + 1) * 2) = ANY
DIM fh AS INTEGER = FREEFILE
OPEN savefile FOR BINARY AS #fh
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

SUB old_loadgame (slot, stat())
DIM gmaptmp(dimbinsize(binMAP))

DIM AS INTEGER i, j, o, z

'--return gen to defaults
xbload game + ".gen", gen(), "General data is missing from " + game

DIM sg AS STRING = savefile
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
 FOR o = 0 TO 1
  FOR j = 0 TO 13
   stat(i, o, j) = buffer(z): z = z + 1
  NEXT j
 NEXT o
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
 FOR o = 0 TO 1
  FOR j = 14 TO 16
   IF picpalmagicnum = 4444 THEN stat(i, o, j) = buffer(z)
   z = z + 1
  NEXT j
 NEXT o
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
   stat(i, 0, 14) = her.sprite
   stat(i, 0, 15) = her.sprite_pal
   stat(i, 1, 14) = her.walk_sprite
   stat(i, 1, 15) = her.walk_sprite_pal
   stat(i, 0, 16) = her.def_weapon + 1'default weapon
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
IF isfile(savefile) THEN
 DIM fh AS INTEGER = FREEFILE
 OPEN savefile FOR BINARY AS #fh

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
