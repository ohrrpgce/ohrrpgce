'OHRRPGCE GAME - Various unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z

#include "config.bi"
#include "udts.bi"
#include "misc.bi"
#include "allmodex.bi"
#include "common.bi"
#include "gglobals.bi"
#include "const.bi"
#include "scrconst.bi"
#include "uiconst.bi"
#include "loading.bi"
#include "slices.bi"
#include "hsinterpreter.bi"
#include "savegame.bi"

#include "game.bi"
#include "yetmore.bi"
#include "yetmore2.bi"
#include "moresubs.bi"
#include "menustuf.bi"
#include "bmodsubs.bi"

#IFDEF SCRIPTPROFILE
#include "string.bi" 'for format
#ENDIF

'--Local subs and functions
DECLARE SUB teleporttooltend (BYREF mini AS Frame Ptr, maptilesX() AS TileMap, tilesets2() AS TilesetData ptr, BYREF zoom, BYVAL map, BYREF mapsize AS XYPair, BYREF minisize AS XYPair, BYREF offset AS XYPair)

REM $STATIC

'who is the hero id + 1!
SUB addhero (who, slot, forcelevel=-1)
DIM wbuf(dimbinsize(binITM))
DIM her AS HeroDef

'--load hero's data
loadherodata @her, who - 1

'--load data of hero's default weapon
loaditemdata wbuf(), her.def_weapon

'--do level forcing
IF forcelevel >= 0 THEN her.def_level = forcelevel

'--do average level enforcement
IF her.def_level < 0 THEN her.def_level = averagelev

'--formally add hero
hero(slot) = who

'---MUST SET DEFAULT EQUIP---
wep = large(wbuf(48), 1)
FOR i = 0 TO 4
 eqstuf(slot, i) = 0
NEXT i
eqstuf(slot, 0) = her.def_weapon + 1

WITH gam.hero(slot).stat
 '--fill in stats
 FOR i = 0 TO 11
  .cur.sta(i) = atlevel(her.def_level, her.Lev0.sta(i), her.Lev99.sta(i)) + wbuf(54 + i)
  .max.sta(i) = .cur.sta(i)
 NEXT i
END WITH
'--weapon picture and palette
gam.hero(slot).wep_pic = wbuf(52)
gam.hero(slot).wep_pal = wbuf(53)

'--weapon attack
bmenu(slot, 0) = wep

'--clear spell lists
FOR i = 1 TO 5
 bmenu(slot, i) = 0
NEXT i

'--include spell lists that have names
o = 1
FOR i = 0 TO 3
 IF her.list_name(i) <> "" THEN bmenu(slot, o) = (i + 1) * -1: o = o + 1
NEXT i

'--add item list to the end
bmenu(slot, o) = -10

'--put spells in spell list
FOR i = 0 TO 3
 FOR o = 0 TO 23
  spell(slot, i, o) = 0
  IF her.spell_lists(i,o).attack > 0 AND her.spell_lists(i,o).learned - 1 <= her.def_level AND her.spell_lists(i,o).learned > 0 THEN spell(slot, i, o) = her.spell_lists(i,o).attack
 NEXT o
NEXT i

'--damage from elements
FOR i = 0 TO gen(genNumElements) - 1
 gam.hero(slot).elementals(i) = her.elementals(i)
NEXT

'--mutable hero bits (not actually mutable)
gam.hero(slot).rename_on_status = readbit(her.bits(), 0, 25)

'--reset levelmp
resetlmp slot, her.def_level

'--setup experience
gam.hero(slot).lev = her.def_level
gam.hero(slot).lev_gain = 0
gam.hero(slot).exp_cur = 0
gam.hero(slot).exp_next = exptolevel(her.def_level + 1)

'--clear learnmask slots (just to be really thorough)
FOR i = slot * 6 TO slot * 6 + 5
 learnmask(i) = 0
NEXT

'--heros are added unlocked
setbit hmask(), 0, who - 1, 0

'--appearance settings
' udts are self documenting
WITH gam.hero(slot)
 .battle_pic = her.sprite
 .battle_pal = her.sprite_pal
 .pic = her.walk_sprite
 .pal = her.walk_sprite_pal
 .def_wep = her.def_weapon + 1'default weapon
END WITH

'--read hero's name (doing this last for no real reason)
names(slot) = her.name
'--if renaming is permitted, do it
IF readbit(her.bits(), 0, 24) THEN
 '--add-hero rename is allowed
 renamehero slot, NO
END IF

'--update tags
party_change_updates
END SUB

FUNCTION atlevel (now as integer, a0 as integer, a99 as integer) as integer

'CLS : a = 80: b = 8500: PRINT : FOR i = 0 TO 99 STEP 5: PRINT i; " "; atlevel(i, a, b): LINE (640, i)-(640 - atlevel(i, a, b) / 100, i), 4: NEXT i

'atlevel = (.8 + now / 50) * now * ((a99 - a0) / 100) + a0 + .1
IF now < 0 THEN atlevel = 0: EXIT FUNCTION
atlevel = (.8 + now / 50) * now * ((a99 - a0) / 275.222) + a0 + .1

END FUNCTION

FUNCTION averagelev () as integer
average = 0
count = 0
FOR i = 0 TO 3
 IF hero(i) > 0 THEN average += gam.hero(i).lev: count += 1
NEXT i
IF count > 0 THEN average = average / count
averagelev = average
END FUNCTION

SUB calibrate

state = 0
state$ = "Center Joystick and Press Button"
midx = 0
midy = 0
button = 0
disabled = 10

setkeys
DO
 setwait speedcontrol
 setkeys
 tog = tog XOR 1
 IF keyval(scEsc) > 1 THEN EXIT DO
 FOR i = 0 TO 1
  IF readjoy(joy(), i) THEN EXIT FOR
 NEXT i
 SELECT CASE button
  CASE 0'no button
   IF joy(3) = 0 THEN joy(13) = 3: joy(14) = 2: button = 1
   IF joy(2) = 0 THEN joy(13) = 2: joy(14) = 3: button = 1
  CASE 1'button down
   IF joy(2) <> 0 AND joy(3) <> 0 THEN button = 2
  CASE 2
   button = 0
 END SELECT
 disabled = disabled - SGN(disabled)
 SELECT CASE state
  CASE 0
   IF (button = 2) AND (disabled = 0) THEN
    midx = joy(0)
    midy = joy(1)
    state$ = "Push UP and Press Button"
    tx = 160
    ty = 45
    state = 1
   END IF
  CASE 1
   IF button = 2 THEN
    joy(9) = joy(1) + (midy - joy(1)) * .33
    state$ = "Push DOWN and Press Button"
    ty = 155
    state = 2
   END IF
  CASE 2
   IF button = 2 THEN
    joy(10) = joy(1) - (joy(1) - midy) * .33
    state$ = "Push LEFT and Press Button"
    tx = 50
    ty = 110
    state = 3
   END IF
  CASE 3
   IF button = 2 THEN
    joy(11) = joy(0) + (midx - joy(0)) * .33
    state$ = "Push RIGHT and Press Button"
    tx = 260
    state = 4
   END IF
  CASE 4
   IF button = 2 THEN
    joy(12) = joy(0) - (joy(0) - midx) * .33
    state$ = "Press the USE button"
    state = 5
   END IF
  CASE 5
   IF button = 2 THEN
    disabled = 4
    state$ = ""
    state = 6
   END IF
  CASE 6
   IF disabled = 0 THEN
    writejoysettings
    gen(genJoy) = 1
    EXIT DO
   END IF
 END SELECT
 clearpage dpage
 centerbox 160, 100, 100, 80, 1, dpage
 centerbox 160, 100, 20, 20, 3, dpage
 IF state > 0 THEN
  centerbox 160 + (joy(0) - midx) * .1, 100 + (joy(1) - midy) * .1, 10, 10, 15, dpage
 END IF
 IF state > 0 AND state < 5 THEN
  edgeprint "This way!", tx - 36, ty - 5, uilook(uiDescription), dpage
 END IF
 edgeprint "Calibrate Joystick", 88, 8, uilook(uiText), dpage
 edgeprint state$, 160 - 4 * LEN(state$), 174, uilook(uiSelectedItem + tog), dpage
 jpos$ = "X=" + STR$(joy(0)) + " Y=" + STR$(joy(1))
 edgeprint jpos$, 160 - 4 * LEN(jpos$), 184, uilook(uiSelectedItem + tog), dpage
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP

END SUB

FUNCTION consumeitem (index as integer) as integer
'--subtracts one of an item at a location. If the item is depleted, returns true. If there are some of the item left, it returns false
'--argument is the inventory slot index, not the item ID
consumeitem = 0
inventory(index).num -= 1
IF inventory(index).num <= 0 THEN
 inventory(index).used = 0
 consumeitem = -1
END IF
update_inventory_caption index
END FUNCTION

FUNCTION countitem (it as integer) as integer
total = 0
FOR o = 0 TO last_inv_slot()
 IF inventory(o).used AND it - 1 = inventory(o).id THEN
  total += inventory(o).num
 END IF
NEXT o
countitem = total
END FUNCTION

SUB delitem (it as integer, amount as integer)
FOR o = 0 TO last_inv_slot()
 IF inventory(o).used AND it - 1 = inventory(o).id THEN
  IF inventory(o).num <= amount THEN
   amount -= inventory(o).num
   inventory(o).used = 0
  ELSE
   inventory(o).num -= amount
   amount = 0
  END IF
  update_inventory_caption o
  IF amount = 0 THEN EXIT FOR
 END IF
NEXT o
END SUB

SUB doswap (s, d)

'---swap hmask (bitsets which tell which heros are locked)
a = readbit(hmask(), 0, d)
setbit hmask(), 0, d, readbit(hmask(), 0, s)
setbit hmask(), 0, s, a

'---Hero index
SWAP hero(s), hero(d)

'---Battle menu
FOR i = 0 TO 5
 SWAP bmenu(s, i), bmenu(d, i)
NEXT i

'---Spell lists
FOR i = 0 TO 3
 FOR o = 0 TO 23
  SWAP spell(s, i, o), spell(d, i, o)
 NEXT o
NEXT i

'---hero state and stats
SWAP gam.hero(s), gam.hero(d)

'---Level-MP
FOR i = 0 TO 7
 SWAP lmp(s, i), lmp(d, i)
NEXT i

'--Learnt spells flags
FOR i = 0 TO 5
 SWAP learnmask(s * 6 + i), learnmask(d * 6 + i)
NEXT

'--name
SWAP names(s), names(d)

'---Equipment
FOR i = 0 TO 4
 SWAP eqstuf(s, i), eqstuf(d, i)
NEXT i

'--set tags, reload hero pictures and palettes, etc
party_change_updates

'hero(40), bmenu(40,5), spell(40,3,23), lmp(40,7), names(40), eqstuf(40,4)
END SUB

SUB drawsay ()
STATIC tog AS INTEGER
tog = tog XOR 1

IF txt.show_lines < 7 THEN
 txt.show_lines = txt.show_lines + 1
 '--play sounds for non-blank lines
 IF txt.show_lines > 1 THEN
  IF trim(txt.box.text(txt.show_lines)) <> "" THEN menusound gen(genTextboxLine)
 END IF
 '--note when the display of lines is done
 IF txt.show_lines >= 7 THEN
  txt.fully_shown = YES
 END IF
 '--update the slice to show the right number of lines
 DIM text_sl As Slice Ptr
 text_sl = LookupSlice(SL_TEXTBOX_TEXT, txt.sl)
 IF text_sl THEN
  DIM dat AS TextSliceData Ptr
  dat = text_sl->SliceData
  IF dat THEN
   dat->line_limit = txt.show_lines
   IF txt.fully_shown THEN
    dat->line_limit = 0
   END IF
  END IF
 END IF
END IF

IF txt.box.choice_enabled THEN
 '--Make the selected choice flash
 DIM choice_sl(1) AS Slice Ptr
 choice_sl(0) = LookupSlice(SL_TEXTBOX_CHOICE0)
 choice_sl(1) = LookupSlice(SL_TEXTBOX_CHOICE1)
 IF choice_sl(0) <> 0 AND choice_sl(1) <> 0 THEN
  FOR i = 0 TO 1
   col = uilook(uiMenuItem)
   IF txt.choice_cursor = i THEN col = uilook(uiSelectedItem + tog)
   ChangeTextSlice choice_sl(i), ,col
  NEXT i
 END IF
END IF
END SUB

SUB evalherotags ()
 DIM AS INTEGER i, id
 DIM leaderid AS INTEGER = -1
 FOR i = 3 TO 0 STEP -1
  IF hero(i) > 0 THEN leaderid = hero(i) - 1
 NEXT i

 FOR i = 0 TO small(gen(genMaxHero), UBOUND(herotags, 1)) '--for each available hero
  'unset all tags, including ones used on heroes not in the party 
  settag herotags(i).have_tag, NO
  settag herotags(i).alive_tag, NO
  settag herotags(i).leader_tag, NO
  settag herotags(i).active_tag, NO
 NEXT i
 'scan party
 FOR i = 0 TO UBOUND(hero)
  id = hero(i) - 1
  IF id >= 0 THEN
   settag herotags(id).have_tag, YES
   IF gam.hero(i).stat.cur.hp > 0 THEN settag herotags(id).alive_tag, YES
   IF id = leaderid THEN settag herotags(id).leader_tag, YES
   IF i < 4 THEN settag herotags(id).active_tag, YES
  END IF
 NEXT i
END SUB

'Call this after a change to the party
SUB party_change_updates
 evalherotags
 evalitemtags  'Because of items with 'actively equipped' tags
 vishero
 tag_updates
END SUB

SUB evalitemtags
 DIM AS INTEGER i, j, k, id

 FOR i = 0 TO maxMaxItems
  'clear all four tags
  settag itemtags(i).have_tag, NO
  settag itemtags(i).in_inventory_tag, NO
  settag itemtags(i).is_equipped_tag, NO
  settag itemtags(i).is_actively_equipped_tag, NO
 NEXT i

 'search inventory slots
 FOR j = 0 TO last_inv_slot()
  'get item ID
  id = inventory(j).id
  IF inventory(j).used THEN 'there is an item in this slot
   settag itemtags(id).have_tag, YES
   settag itemtags(id).in_inventory_tag, YES
  END IF
 NEXT j

 FOR j = 0 TO 40 'search hero list
  FOR k = 0 TO 4 'search equipment slots
   id = eqstuf(j, k) - 1
   IF id >= 0 THEN ' there is an item equipped in this slot
    settag itemtags(id).have_tag, YES
    settag itemtags(id).is_equipped_tag, YES
    IF j < 4 THEN settag itemtags(id).is_actively_equipped_tag, YES
   END IF
  NEXT k
 NEXT j
END SUB

FUNCTION findhero (who as integer, f as integer, l as integer, d as integer) as integer
result = -1
FOR i = f TO l STEP d
 IF hero(i) = who OR (who = -1 AND hero(i)) THEN result = i: EXIT FOR
NEXT i
findhero = result
END FUNCTION

SUB hero_swap_menu (iAll as integer)
'--Preserve background for display beneath the hero swapper
DIM page AS INTEGER
DIM holdscreen AS INTEGER
page = compatpage
holdscreen = allocatepage
copypage page, holdscreen

DIM swindex(40), swname(40) as string

swapme = -1
ecsr = -1
acsr = 0
top = 0
la = -1
numhero = 0
high = 0
wide = 0

GOSUB resetswap

IF hero(acsr) THEN info$ = names(acsr) ELSE info$ = ""

MenuSound gen(genAcceptSFX)
setkeys
DO
 setwait speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 IF carray(ccMenu) > 1 THEN
  IF swapme >= 0 THEN
   MenuSound gen(genCancelSFX)
   swapme = -1
  ELSE
   EXIT DO
  END IF
 END IF
 IF iAll THEN
  IF carray(ccUp) > 1 THEN
   MenuSound gen(genCursorSFX)
   IF ecsr < 0 THEN
    ecsr = la
    GOSUB refreshemenu
   ELSE
    ecsr = loopvar(ecsr, -1, la, -1)
    GOSUB refreshemenu
   END IF
  END IF
  IF carray(ccDown) > 1 THEN
   MenuSound gen(genCursorSFX)
   IF ecsr < 0 THEN
    ecsr = 0
    GOSUB refreshemenu
   ELSE
    ecsr = loopvar(ecsr, -1, la, 1)
    GOSUB refreshemenu
   END IF
  END IF
 END IF
 IF carray(ccLeft) > 1 AND ecsr < 0 THEN
  MenuSound gen(genCursorSFX)
  acsr = loopvar(acsr, 0, 3, -1)
  IF hero(acsr) AND ecsr < 0 THEN info$ = names(acsr) ELSE info$ = ""
 END IF
 IF carray(ccRight) > 1 AND ecsr < 0 THEN
  MenuSound gen(genCursorSFX)
  acsr = loopvar(acsr, 0, 3, 1)
  IF hero(acsr) AND ecsr < 0 THEN info$ = names(acsr) ELSE info$ = ""
 END IF
 IF carray(ccUse) > 1 THEN
  DO
  IF readbit(gen(), genBits2, 4) THEN
   '--If this bit is set, we refuse to reorder locked heroes
   IF readbit(hmask(), 0, acsr) ORELSE (swapme >= 0 ANDALSO readbit(hmask(), 0, swapme)) THEN
    MenuSound gen(genCancelSFX)
    EXIT DO
   END IF
  END IF
  IF swapme = -1 THEN
   MenuSound gen(genAcceptSFX)
   IF ecsr < 0 THEN
    swapme = acsr
   ELSE
    swapme = 4 + ecsr
   END IF
  ELSE
   MenuSound gen(genAcceptSFX)
   DO
    IF swapme < 4 THEN
     IF (numhero < 2 AND ecsr = la) OR (readbit(hmask(), 0, swapme) AND ecsr > -1) THEN EXIT DO
    ELSE
     IF swapme - 4 = la AND ecsr = -1 AND numhero < 2 THEN EXIT DO
     IF readbit(hmask(), 0, acsr) AND ecsr = -1 THEN EXIT DO
    END IF
    '---IDENTIFY DESTINATION---
    IF ecsr < 0 THEN
     temp = acsr
    ELSE
     temp = swindex(ecsr)
    END IF
    '---IDENTIFY SOURCE---
    IF swapme < 4 THEN
     temp2 = swapme
    ELSE
     temp2 = swindex(swapme - 4)
    END IF
    doswap temp, temp2
    swapme = -1
    GOSUB resetswap
    EXIT DO
   LOOP '--this loop just exists for convenient breaking with EXIT DO
  END IF
  EXIT DO
  LOOP '--this loop just exists for convenient breaking with EXIT DO
 END IF

 GOSUB showswapmenu
 setvispage vpage
 copypage holdscreen, page
 dowait
LOOP
FOR t = 4 TO 5: carray(t) = 0: NEXT t
MenuSound gen(genCancelSFX)
freepage page
freepage holdscreen

party_change_updates
EXIT SUB

refreshemenu:
IF ecsr < top THEN top = large(ecsr, 0)
IF ecsr > top + 7 THEN top = ecsr - 7
IF hero(acsr) AND ecsr < 0 THEN info$ = names(acsr) ELSE info$ = ""
RETRACE

'---DRAWS SWAP MENU AND CURRENT SELECTION----
showswapmenu:
centerbox 160, 66, 130, 38, 1, page
o = 0
FOR i = 0 TO 3
 IF i = swapme OR hero(i) > 0 THEN rectangle 105 + (30 * i), 60, 20, 20, uilook(uiTextBox), page
 IF hero(i) THEN
  '5th frame: down
  frame_draw herow(o).sprite + 4, herow(o).pal, 105 + i * 30, 60 + (i = swapme) * 6, 1, -1, page
  o = o + 1
 END IF
NEXT i
IF ecsr < 0 THEN edgeprint CHR$(24), 111 + 30 * acsr, 52, uilook(uiSelectedItem + tog), page
IF iAll THEN
 centerbox 160, 100 + small(high, 8) * 5, wide * 8 + 16, small(high, 8) * 10 + 10, 1, page
 FOR i = top TO small(top + 7, la)
  'Some of the colours are a bit bizarre, here, especially the time bar stuff below
  c = uilook(uiMenuItem)
  IF swapme = i + 4 THEN c = uilook(uiSelectedDisabled) '6
  IF ecsr = i THEN
   c = uilook(uiSelectedItem + tog)
   IF swapme = i + 4 THEN c = uilook(uiSelectedDisabled + tog) '6 + 8 * tog
  END IF
  IF swapme > -1 AND swapme < 4 THEN
   IF (numhero < 2 AND i = la) OR readbit(hmask(), 0, acsr) THEN c = uilook(uiTimeBar + ((ecsr = i) * tog)) '8 + ((ecsr = i) * tog)
  END IF
  edgeprint swname(i), xstring(swname(i), 160), 100 + (i - top) * 10, c, page
 NEXT i
END IF
IF LEN(info$) THEN
 centerbox 160, 44, (LEN(info$) + 2) * 8, 14, 1, page
 edgeprint info$, xstring(info$, 160), 39, uilook(uiText), page
END IF
RETRACE

'---MAPS OUT ONLY VALID SWAPABLE HEROS PLUS A BLANK-----
resetswap:
la = -1
wide = 0
FOR i = 4 TO 40
 IF readbit(hmask(), 0, i) = 0 AND hero(i) THEN
  la = la + 1
  swindex(la) = i
  swname(la) = names(i)
  wide = large(wide, LEN(swname(la)))
 END IF
NEXT i
la = la + 1
FOR i = 40 TO 4 STEP -1
 IF hero(i) = 0 THEN
  swindex(la) = i
  swname(la) = readglobalstring$(48, "-REMOVE-", 10)
  wide = large(wide, 7)
 END IF
NEXT i
high = small(8, la + 1)
numhero = 0
FOR i = 0 TO 3
 IF hero(i) > 0 THEN
  numhero = numhero + 1
 END IF
NEXT i
IF hero(acsr) AND ecsr < 0 THEN info$ = names(acsr) ELSE info$ = ""
RETRACE
END SUB

'Either pass a tag number and specify YES/NO, or pass just a tag number; +ve/-ve indicates value
SUB settag (BYVAL tagnum as integer, BYVAL value as integer = 4444)
 IF value <> 4444 THEN
  IF ABS(tagnum) > 1 THEN setbit tag(), 0, ABS(tagnum), value
 ELSEIF tagnum < -1 THEN
  setbit tag(), 0, ABS(tagnum), NO
 ELSEIF tagnum > 1 THEN
  setbit tag(), 0, tagnum, YES
 END IF
END SUB

FUNCTION istag (num as integer, zero as integer) as integer
IF num = 0 THEN RETURN zero 'why go through all that just to return defaults?
IF num = 1 THEN RETURN 0
IF num = -1 THEN RETURN -1
IF ABS(num) >= UBOUND(tag) * 16 + 16 THEN RETURN zero ' use default in case of an invalid tag

ret = readbit(tag(), 0, ABS(num)) 'raw bit: 0 or -1

IF num > 0 AND ret <> 0 THEN RETURN -1
IF num < 0 AND ret = 0 THEN RETURN -1
RETURN 0
END FUNCTION

SUB loaddoor (map)
IF gen(genVersion) < 2 THEN
 '--obsolete doors
ELSE
 '--THE RIGHT WAY--
 DeSerDoors(game + ".dox", gam.map.door(), map)
END IF
END SUB

SUB minimap (x, y)
 DIM mini AS Frame Ptr
 DIM zoom AS INTEGER = -1
 mini = createminimap(maptiles(), tilesets(), zoom)

 DIM minisize AS XYPair
 minisize.x = mini->w
 minisize.y = mini->h
 
 DIM offset AS XYPair
 offset.x = vpages(vpage)->w / 2 - minisize.x / 2
 offset.y = vpages(vpage)->h / 2 - minisize.y / 2

 edgeboxstyle offset.x - 2, offset.y - 2, minisize.x + 4, minisize.y + 4, 0, vpage
 frame_draw mini, NULL, offset.x, offset.y, 1, NO, vpage
 frame_unload @mini

 MenuSound gen(genAcceptSFX)

 DIM i AS INTEGER
 setkeys
 DO
  setwait speedcontrol
  setkeys
  tog = tog XOR 1
  playtimer
  control
  IF carray(ccUse) > 1 OR carray(ccMenu) > 1 THEN EXIT DO
  FOR i = 1 TO 99
   IF keyval(i) > 1 THEN EXIT DO
  NEXT i
  rectangle offset.x + (x / 20) * zoom, offset.y + (y / 20) * zoom, zoom, zoom, uilook(uiSelectedItem) * tog, vpage
  setvispage vpage
  dowait
 LOOP
 setkeys
 flusharray carray(), 7, 0
 MenuSound gen(genCancelSFX)
END SUB

FUNCTION teleporttool () as integer
 REDIM maptiles2(0) AS TileMap
 DIM tilesets2(maplayerMax) as TilesetData ptr
 DIM mini AS Frame Ptr
 DIM zoom AS INTEGER
 DIM i AS INTEGER

 DIM mapsize AS XYPair
 DIM minisize AS XYPair 'pixels
 DIM offset AS XYPair

 'Notice that we initially use the real map's maptiles(), rather than reloading them into maptiles2(),
 'but we DO reload the tilesets (they are cached anyway)
 teleporttooltend mini, maptiles(), tilesets2(), zoom, -1, mapsize, minisize, offset

 DIM dest AS XYPair
 dest.x = catx(0) \ 20
 dest.y = caty(0) \ 20

 DIM camera AS XYPair 'in pixels
 camera.x = bound(dest.x * zoom - minisize.x \ 2, 0, mapsize.x * zoom - minisize.x)
 camera.y = bound(dest.y * zoom - minisize.y \ 2, 0, mapsize.y * zoom - minisize.y)

 DIM menu(1) AS STRING
 menu(0) = "Teleport to map... " & gam.map.id & " " & getmapname$(gam.map.id)
 menu(1) = "Teleport to position... X = " & dest.x & " Y = " & dest.y

 DIM state AS MenuState
 state.pt = 1
 state.top = 0
 state.size = 22
 state.first = 0
 state.last = UBOUND(menu)

 DIM preview_delay AS INTEGER = 0
 DIM pickpoint AS INTEGER = NO
 DIM destmap AS INTEGER = gam.map.id


 teleporttool = 0

 copypage vpage, 3
 GOSUB redraw

 MenuSound gen(genAcceptSFX)
 setkeys
 DO
  setwait speedcontrol
  setkeys
  state.tog = state.tog XOR 1

  IF preview_delay > 0 THEN
   preview_delay -= 1
   IF preview_delay = 0 THEN
    teleporttooltend mini, maptiles2(), tilesets2(), zoom, destmap, mapsize, minisize, offset
    dest.x = small(dest.x, mapsize.x - 1)
    dest.y = small(dest.y, mapsize.y - 1)
    camera.x = bound(dest.x * zoom - minisize.x \ 2, 0, mapsize.x * zoom - minisize.x)
    camera.y = bound(dest.y * zoom - minisize.y \ 2, 0, mapsize.y * zoom - minisize.y)
    GOSUB redraw
   END IF
  END IF

  control
  IF pickpoint = NO THEN
   usemenu state
   IF carray(ccMenu) > 1 THEN  'cancel
    EXIT DO
   END IF
   IF state.pt = 0 THEN
    IF intgrabber(destmap, 0, gen(genMaxMap)) THEN
     preview_delay = 12
     menu(0) = "Map " & destmap & " " & getmapname$(destmap)
    END IF
   END IF
   IF enter_or_space() THEN pickpoint = YES
  ELSE
   IF carray(ccUse) > 1 THEN 'confirm and teleport
    IF gam.map.id <> destmap THEN teleporttool = -1
    gam.map.id = destmap
    catx(0) = dest.x * 20
    caty(0) = dest.y * 20
    EXIT DO
   END IF
   IF carray(ccMenu) > 1 THEN pickpoint = NO
   
   IF keyval(scLeftShift) > 0 OR keyval(scRightShift) > 0 THEN
    xrate = 8
    yrate = 5
   ELSE
    xrate = 1
    yrate = 1
   END IF

   IF slowkey(scUp, 1) THEN dest.y = large(dest.y - yrate, 0)
   IF slowkey(scDown, 1) THEN dest.y = small(dest.y + yrate, mapsize.y - 1)
   IF slowkey(scLeft, 1) THEN dest.x = large(dest.x - xrate, 0)
   IF slowkey(scRight, 1) THEN dest.x = small(dest.x + xrate, mapsize.x - 1)

   DIM temp AS XYPair = camera
   camera.x = bound(camera.x, (dest.x + 1) * zoom + 40 - minisize.x, dest.x * zoom - 40)  'follow dest
   camera.y = bound(camera.y, (dest.y + 1) * zoom + 30 - minisize.y, dest.y * zoom - 30)
   camera.x = bound(camera.x, 0, mapsize.x * zoom - minisize.x)  'bound to map edges
   camera.y = bound(camera.y, 0, mapsize.y * zoom - minisize.y)
   IF temp.x <> camera.x OR temp.y <> camera.y THEN GOSUB redraw

   menu(1) = "Position X = " & dest.x & " Y = " & dest.y
  END IF

  copypage vpage, dpage
  rectangle offset.x + dest.x * zoom - camera.x, offset.y + dest.y * zoom - camera.y, zoom, zoom, uilook(uiSelectedItem) * state.tog, dpage
  standardmenu menu(), state, 0, 182, dpage, YES, pickpoint=YES
  setvispage dpage
  dowait
 LOOP
 frame_unload @mini
 unloadtilemaps maptiles2()
 unloadmaptilesets tilesets2()
 setkeys
 flusharray carray(), 7, 0
 MenuSound gen(genCancelSFX)
 EXIT FUNCTION

redraw:
 'reblit the minimap to a spare page
 copypage 3, vpage
 edgeboxstyle offset.x - 2, offset.y - 2, minisize.x + 4, minisize.y + 4, 0, vpage
 frame_draw mini, NULL, offset.x - camera.x, offset.y - camera.y, 1, NO, vpage
 RETRACE

END FUNCTION

'map = -1 means don't load maptiles; already loaded (maptilesX() == maptiles() global)
SUB teleporttooltend (BYREF mini AS Frame Ptr, maptilesX() AS TileMap, tilesets2() AS TilesetData ptr, BYREF zoom, BYVAL map, BYREF mapsize AS XYPair, BYREF minisize AS XYPair, BYREF offset AS XYPair)
 IF map > -1 THEN
  DIM gmap2(dimbinsize(binMAP)) AS INTEGER
  loadrecord gmap2(), game + ".map", getbinsize(binMAP) \ 2, map
  loadtilemaps maptilesX(), maplumpname$(map, "t")
  loadmaptilesets tilesets2(), gmap2()
 ELSE
  loadmaptilesets tilesets2(), gmap()
 END IF
 'minimum zoom level to make tiles easy to pick
 zoom = bound(small(320 \ maptilesX(0).wide, 200 \ maptilesX(0).high), 5, 20)
 frame_unload @mini
 mini = createminimap(maptilesX(), tilesets2(), zoom)
 mapsize.x = maptilesX(0).wide
 mapsize.y = maptilesX(0).high
 offset.x = large(cint(160 - mini->w / 2), 0)
 offset.y = large(cint(100 - mini->h / 2), 0)
 minisize.x = 320 - offset.x * 2
 minisize.y = 200 - offset.y * 2
END SUB


FUNCTION movdivis (xygo as integer) as integer
IF (xygo \ 20) * 20 = xygo AND xygo <> 0 THEN
 movdivis = -1
ELSE
 movdivis = 0
END IF
END FUNCTION

FUNCTION onwho (caption as string, alone as integer) as integer

'-- pre-select the first hero
FOR i = 0 TO 3
 IF hero(i) > 0 THEN
  w = i
  EXIT FOR
 END IF
NEXT i

'-- if there is only one hero, return immediately
'--unless we are in alone-mode
IF alone = 0 AND herocount() <= 1 THEN onwho = w: setkeys: EXIT FUNCTION

menusound gen(genAcceptSFX)
copypage dpage, vpage
setvispage vpage
DIM page as integer = compatpage
setkeys
DO
 setwait speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 wtg = loopvar(wtg, 0, 3, 1)
 IF carray(ccMenu) > 1 THEN
  onwho = -1
  menusound gen(genCancelSFX)
  EXIT DO
 END IF
 IF carray(ccLeft) > 1 THEN
  DO: w = loopvar(w, 0, 3, -1): LOOP UNTIL hero(w) > 0
  menusound gen(genCursorSFX)
 END IF
 IF carray(ccRight) > 1 THEN
  DO: w = loopvar(w, 0, 3, 1): LOOP UNTIL hero(w) > 0
  menusound gen(genCursorSFX)
 END IF
 IF carray(ccUse) > 1 THEN onwho = w: EXIT DO
 centerbox 160, 100, 140, 52, 1, page
 o = 0
 FOR i = 0 TO 3
  IF hero(i) > 0 THEN
   wt = 0: IF w = i THEN wt = INT(wtg / 2)
   frame_draw herow(o).sprite + (2 * 2) + wt, herow(o).pal, 100 + i * 30, 100, 1, -1, page
   o = o + 1
  END IF
 NEXT i
 edgeprint CHR(25), 106 + w * 30, 90, uilook(uiSelectedItem + tog), page
 edgeprint caption, xstring(caption, 160), 80, uilook(uiText), page
 setvispage vpage
 dowait
LOOP
freepage page
setkeys
flusharray carray(), 7, 0

END FUNCTION

SUB readjoysettings

IF isfile(exepath$ + SLASH + "joyset.ini") THEN
 '--use joyset.ini
 fh = FREEFILE
 OPEN exepath$ + SLASH + "joyset.ini" FOR INPUT AS #fh
 safety = 0
 n$ = ""
 DO WHILE NOT EOF(fh) AND safety < 100
  LINE INPUT #fh, a$
  IF settingstring(a$, "UPTHRESH", n$) THEN
   joy(9) = str2int(n$)
  END IF
  IF settingstring(a$, "DOWNTHRESH", n$) THEN
   joy(10) = str2int(n$)
  END IF
  IF settingstring(a$, "LEFTTHRESH", n$) THEN
   joy(11) = str2int(n$)
  END IF
  IF settingstring(a$, "RIGHTTHRESH", n$) THEN
   joy(12) = str2int(n$)
  END IF
  IF settingstring(a$, "USEBUTTON", n$) THEN
   joy(13) = bound(str2int(n$) + 2, 2, 3)
  END IF
  IF settingstring(a$, "MENUBUTTON", n$) THEN
   joy(14) = bound(str2int(n$) + 2, 2, 3)
  END IF
  safety = safety + 1
 LOOP
 CLOSE #fh
 '--wait a little to make sure the buttons clear
 setwait speedcontrol
 dowait
 gen(genJoy) = 1
ELSE
 '--no joyset.ini file, must recalibrate
 'calibrate
END IF

END SUB

'If escapable is true, then ESC exits, otherwise it resets the entered name
SUB renamehero (byval who as integer, byval escapable as integer)

dim her as herodef
loadherodata @her, hero(who) - 1

limit = her.max_name_len
IF limit = 0 THEN limit = 16

prompt$ = readglobalstring$(137, "Name the Hero", 20)
spacer$ = STRING$(large(limit, LEN(names(who))), " ")
remember$ = names(who)
rememberjoycal = gen(genJoy)
gen(genJoy) = 1'--disable joystick calibration

DIM page as integer
page = compatpage
IF fadestate = 0 THEN
 setvispage vpage
 fadein
 needfadeout = 1
END IF

setkeys
DO
 setwait speedcontrol
 setkeys
 playtimer
 control
 centerbox 160, 100, 168, 32, 1, page
 IF carray(ccUse) > 1 AND keyval(scSpace) = 0 THEN EXIT DO
 IF carray(ccMenu) > 1 THEN
  names(who) = remember$
  IF escapable THEN EXIT DO
 END IF
 strgrabber names(who), limit
 edgeprint prompt$, xstring(prompt$, 160), 90, uilook(uiText), page
 textcolor uilook(uiHighlight), uiLook(uiHighlight)
 printstr spacer$, xstring(spacer$, 161), 101, page
 edgeprint names(who), xstring(names(who), 160), 100, uilook(uiMenuItem), page

 setvispage vpage
 dowait
LOOP
menusound gen(genAcceptSFX)
gen(genJoy) = rememberjoycal '-- restore joystick calibration setting

IF needfadeout = 1 THEN
 fadeout 0, 0, 0
END IF
freepage page

END SUB

SUB resetgame (scriptout$)
gam.map.id = 0
catx(0) = 0
caty(0) = 0
catz(0) = 0
catd(0) = 0
gam.random_battle_countdown = 0
'leader = 0
mapx = 0
mapy = 0
gold = 0
scriptout$ = ""
'--return gen to defaults
xbload game + ".gen", gen(), "General data is missing from " + sourcerpg

CleanNPCL npc(),300
flusharray tag(), 126, 0
flusharray hero(), 40, 0
FOR i = 0 TO 40
 FOR j = 0 TO 11
  gam.hero(i).stat.cur.sta(j) = 0
  gam.hero(i).stat.max.sta(j) = 0
 NEXT j
 gam.hero(i).lev = 0
 gam.hero(i).lev_gain = 0
 gam.hero(i).wep_pic = 0
 gam.hero(i).wep_pal = 0
 gam.hero(i).battle_pic = 0
 gam.hero(i).battle_pal = 0
 gam.hero(i).pic = 0
 gam.hero(i).pal = 0
 gam.hero(i).def_wep = 0
 gam.hero(i).rename_on_status = 0
 FOR j = 0 TO maxElements - 1
  gam.hero(i).elementals(j) = 1.0f
 NEXT
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 5
  bmenu(i, o) = 0
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 3
  FOR j = 0 TO 23
   spell(i, o, j) = 0
  NEXT j
 NEXT o
NEXT i
FOR i = 0 TO 40
 FOR o = 0 TO 7
  lmp(i, o) = 0
 NEXT o
NEXT i
FOR i = 0 TO 40
 gam.hero(i).exp_cur = 0
 gam.hero(i).exp_next = 0
NEXT i
FOR i = 0 TO 40
 names(i) = ""
NEXT i
CleanInventory(inventory())
FOR i = 0 TO 40
 FOR o = 0 TO 4
  eqstuf(i, o) = 0
 NEXT o
NEXT i

'RECORD 2 (applies only to saves)

FOR i = 0 TO 99
 FOR o = 0 TO 49
  gam.stock(i, o) = -1 'default of -1 means no limit
 NEXT o
NEXT i
flusharray hmask(), 3, 0
flusharray global(), 4095, 0
reset_vehicle vstate
cleanup_text_box
txt.showing = NO
txt.fully_shown = NO
txt.show_lines = 0

FOR i = 0 TO 31
 with plotstr(i)
  .s = ""
  .col = 15
  .BGCol = 0
  .X = 0
  .Y = 0
  .Bits = 0
 end with
NEXT i

FOR i = topmenu TO 0 STEP -1
 remove_menu i, NO
NEXT i

'delete temp files that are part of the game state
deletetemps

'doesn't unload scripts: not needed
killallscripts

FOR i = 0 TO UBOUND(herow)
 WITH herow(i)
  frame_unload(@.sprite)
  palette16_unload(@.pal)
 END WITH
NEXT

FOR i = 0 TO ubound(timers)
 WITH timers(i)
  .count = 0
  .speed = 0
  .ticks = 0
  .trigger = 0
  .flags = 0
  .st = 0
  .pause = 0
 END WITH
NEXT i

cleanup_game_slices()
'plotslices() should now be all zeroed out. Rather than checking, check slicedebug() (if enabled)
SliceDebugDump YES
SetupGameSlices
reset_game_state()

END SUB

SUB resetlmp (slot, lev)
FOR i = 0 TO 7
 lmp(slot, i) = 0
NEXT i
o = 0: j = 0
FOR i = 0 TO lev
 lmp(slot, o) = lmp(slot, o) + 1
 o = o + 1
 IF o > j THEN o = 0: j = j + 1
 IF j > 7 THEN j = 0
NEXT i
END SUB

FUNCTION runscript (id as integer, index as integer, newcall as integer, er as string, trigger as integer) as integer
'newcall: whether his script is triggered rather than called from a script, and
'therefore whether "no double-triggering" should take effect.

IF trigger <> 0 THEN n = decodetrigger(id, trigger) ELSE n = id

IF n = 0 THEN
 runscript = 2 '--quiet failure (though decodetrigger might have shown a scripterr)
 EXIT FUNCTION
END IF

IF index > 127 THEN
 runscript = 0 '--error
 scripterr "failed to load " + er$ + " script " & n & " " & scriptname(n) & ", interpreter overloaded", 6
 EXIT FUNCTION
END IF

IF newcall AND index > 0 THEN
 IF n = scrat(index - 1).id AND readbit(gen(), 101, 10) = 0 THEN
  'fail quietly
  '--scripterr "script " & n & " is already running"
  runscript = 2 '--quiet failure
  EXIT FUNCTION
 END IF
END IF

'--store current command data in scrat (used outside of the inner interpreter)
IF nowscript >= 0 THEN
 WITH scrat(nowscript)
  .curkind = curcmd->kind
  .curvalue = curcmd->value
  .curargc = curcmd->argc
 END WITH
END IF

WITH scrat(index)
 '-- Load the script (or return the reference if already loaded)
 .scr = loadscript(n)
 IF .scr = NULL THEN
  '--failed to load
  runscript = 0'--error
  scripterr "Failed to load " + er$ + " script " & n & " " & scriptname(n), 6
  EXIT FUNCTION
 END IF
 .scr->totaluse += 1
 scriptctr += 1
 .scr->lastuse = scriptctr
 'increment refcount once loading is successful

 'erase state, pointer, return value and depth, set id
 .state = stread
 .ptr = 0
 .ret = 0
 .depth = 0
 .id = n
 .scrdata = .scr->ptr
 .curargn = 0
 curcmd = cast(ScriptCommand ptr, .scrdata + .ptr) 'just in case it's needed before subread is run
 
 scrat(index + 1).heap = .heap + .scr->vars

 IF scrat(index + 1).heap > 2048 THEN
  runscript = 0'--error
  scripterr "failed to load " + er$ + " script " & n & " " & scriptname(n) & ", script heap overflow", 6
  EXIT FUNCTION
 END IF

 FOR i = 0 TO .scr->vars - 1
  heap(.heap + i) = 0
 NEXT i

 '--suspend the previous script...Why was I doing this?
 IF newcall AND index > 0 THEN
  scrat(index - 1).state *= -1
 END IF

 '--we are successful, so now its safe to increment this
 .scr->refcount += 1
 nowscript += 1

 'debug scriptname$(.id) & " in script(" & (.id MOD scriptTableSize) & "): totaluse = " & .scr->totaluse & " refc = " & .scr->refcount & " lastuse = " & .scr->lastuse
END WITH

#IFDEF SCRIPTPROFILE
IF insideinterpreter THEN 'we have nowscript > 0
 TIMER_STOP(scrat(nowscript - 1).scr->totaltime)
 scrat(nowscript).scr->entered += 1
 TIMER_START(scrat(nowscript).scr->totaltime)
END IF
#ENDIF

RETURN 1 '--success

END FUNCTION

FUNCTION loadscript (n as unsigned integer) as ScriptData ptr
 '-- script() is a hashtable with doubly linked lists as buckets, storing the loaded scripts

 DIM as ScriptData Ptr scrnode = script(n MOD scriptTableSize)
 WHILE scrnode
  IF scrnode->id = n THEN RETURN scrnode
  scrnode = scrnode->next
 WEND

 DIM thisscr as ScriptData ptr
 DIM temp as short

 '--load the script from file
 scriptfile$ = tmpdir & n & ".hsz"
 IF NOT isfile(scriptfile$) THEN
  scriptfile$ = tmpdir & n & ".hsx"
  IF NOT isfile(scriptfile$) THEN
   '--because TMC once suggested that preunlumping the .hsp lump would be a good way to reduce (SoJ) loading time
   scriptfile$ = workingdir & SLASH & n & ".hsx"
   IF NOT isfile(scriptfile$) THEN
    scripterr "script id " & n & " does not exist", 6
    RETURN NULL
   END IF
  END IF
 END IF

 f = FREEFILE
 OPEN scriptfile$ FOR BINARY AS #f

 'minimum length of a valid 16-bit .hsx
 IF LOF(f) < 10 THEN
  scripterr "script " & n & " corrupt (" & LOF(f) & " bytes)", 6
  CLOSE #f
  RETURN NULL
 END IF

 thisscr = callocate(sizeof(ScriptData))
 WITH *thisscr

  GET #f, 1, temp
  skip = temp

  IF skip < 4 THEN
   scripterr "script " & n & " is corrupt (header length " & skip & ")", 6
   CLOSE #f
   deallocate(thisscr)
   RETURN NULL
  END IF

  GET #f, 3, temp
  'some HSX files seem to have an illegal negative number of variables
  .vars = temp
  .vars = bound(.vars, 0, 256)
 
  IF skip >= 6 THEN
   GET #f, 5, temp
   .args = bound(temp, 0, .vars)
  ELSE
   .args = 999
  END IF

  IF skip >= 8 THEN
   GET #f, 7, temp
   scrformat = temp
  ELSE
   scrformat = 0
  END IF
  IF scrformat > 2 THEN
   scripterr "script " & n & " is in an unsupported format", 6
   CLOSE #f
   deallocate(thisscr)
   RETURN NULL
  END IF
  IF scrformat >= 1 THEN wordsize = 4 ELSE wordsize = 2

  IF skip >= 12 THEN
   GET #f, 9, .strtable
   IF .strtable THEN .strtable = (.strtable - skip) \ wordsize
  ELSEIF skip = 10 THEN
   GET #f, 9, temp
   IF temp THEN .strtable = (temp - skip) \ wordsize
  ELSE
   .strtable = 0
  END IF

  'set an arbitrary max script buffer size (scriptmemMax in const.bi), individual scripts must also obey
  .size = (LOF(f) - skip) \ wordsize
  IF .size > scriptmemMax THEN
   scripterr "Script " & n & " exceeds maximum size by " & .size * 100 \ scriptmemMax - 99 & "%", 6
   CLOSE #f
   deallocate(thisscr)
   RETURN NULL
  END IF

  IF .strtable < 0 OR .strtable > .size THEN
   scripterr "Script " & n & " corrupt; bad string table offset", 6
   CLOSE #f
   deallocate(thisscr)
   RETURN NULL
  END IF

  IF .size + totalscrmem > scriptmemMax OR numloadedscr = maxLoadedScripts THEN
   'debug "loadscript(" & n & " '" & scriptname(n) & "'): scriptbuf full; size = " & .size & " totalscrmem = " & totalscrmem & ", calling freescripts"
   freescripts(scriptmemMax - .size)
  END IF

  .ptr = allocate(.size * sizeof(integer))
  IF .ptr = 0 THEN
   scripterr "Could not allocate memory to load script", 6
   CLOSE #f
   deallocate(thisscr)
   RETURN NULL
  END IF

  IF wordsize = 2 THEN
   FOR i = skip TO LOF(f) - wordsize STEP wordsize
    GET #f, 1 + i, temp
    .ptr[(i - skip) \ 2] = temp
   NEXT
  ELSE
   GET #f, skip + 1, *.ptr, .size
  END IF
  CLOSE #f

  .id = n
  .refcount = 0
  .totaluse = 0
  .lastuse = 0
  .totaltime = 0.0
  .entered = 0
  numloadedscr += 1
  totalscrmem += .size
 END WITH

 'append to front of doubly linked list
 DIM as ScriptData Ptr Ptr scrnodeptr = @script(n MOD scriptTableSize)
 IF *scrnodeptr THEN
  'already a script there
  (*scrnodeptr)->backptr = @thisscr->next
 END IF
 thisscr->backptr = scrnodeptr 'this is for convenience of easier deleting (in freescripts)
 thisscr->next = *scrnodeptr
 *scrnodeptr = thisscr

 RETURN thisscr
END FUNCTION

SUB freescripts (mem)
'frees loaded scripts until at least totalscrmem <= mem (measured in 4-byte ints) (probably a lot lower)
'also makes sure numloadedscr <= maxLoadedScripts - 24
'call freescripts(0) to cleanup all scripts
'and, as a total hack, print profiling information on scripts if SCRIPTPROFILE is defined, in which
'case the buffers are so large that freescripts is only called when quitting

#IFDEF SCRIPTPROFILE
DIM timeroverhead as double
FOR i = 0 TO 999
 timeroverhead -= TIMER
 timeroverhead += TIMER
NEXT
timeroverhead /= 1000
#ENDIF

'give each script a score (the lower, the more likely to throw) and sort them
'this is roughly a least recently used list
TYPE ScriptListElmt
 p as ScriptData ptr
 score as integer
END TYPE
DIM LRUlist(maxLoadedScripts) as ScriptListElmt
listtail = -1

FOR i = 0 TO UBOUND(script)
 DIM as ScriptData Ptr scrp = script(i)
 WHILE scrp
  WITH *scrp
#IFDEF SCRIPTPROFILE
   'sort by total time
   .totaltime -= .entered * timeroverhead
   score = .totaltime * -10000
#ELSE
   'this formula has only been given some testing, and doesn't do all that well
   score = .lastuse - scriptctr
   score = iif(score > -400, score, -400) _
         + iif(.totaluse < 100, .totaluse, iif(.totaluse < 1700, 94 + .totaluse\16, 200)) _
         - .size \ (scriptmemMax \ 1024)
#ENDIF
  END WITH
  FOR j = listtail TO 0 STEP -1
   IF score >= LRUlist(j).score THEN EXIT FOR
   LRUlist(j + 1).p = LRUlist(j).p
   LRUlist(j + 1).score = LRUlist(j).score
  NEXT
  LRUlist(j + 1).p = scrp
  LRUlist(j + 1).score = score
  listtail += 1
  scrp = scrp->next
 WEND
NEXT


#IFDEF SCRIPTPROFILE
DIM entiretime as double
FOR i = 0 TO listtail
 entiretime += LRUlist(i).p->totaltime
NEXT

debug "script profiling information:"
debug "#switches is the number of times that the interpreter switched to that script"
debug "(switching time is relatively neglible and included to help determine"
debug "calls to other scripts, which are more expensive)"
debug "Total time recorded in interpreter: " & format(entiretime, "0.000") & "sec   (timer overhead = " & format(timeroverhead*1000000, "0.00") & "us)"
debug " %time        time    time/call      #calls   #switches  script name"
FOR i = 0 TO listtail
' debug i & ": " & LRUlist(i).p & " score = " & LRUlist(i).score
 WITH *LRUlist(i).p
  debug " " & format(100 * .totaltime / entiretime, "00.00") _
      & RIGHT(SPACE(9) & format(.totaltime*1000, "0"), 10) & "ms" _
      & RIGHT(SPACE(10) & format(.totaltime*1000000/.totaluse, "0"), 11) & "us" _
      & RIGHT(SPACE(11) & .totaluse, 12) _
      & RIGHT(SPACE(11) & .entered, 12) _
      & "  " & scriptname(.id) '& "  " & format(1000*(.totaltime + .entered * timeroverhead), "0.00")

'  debug "id = " & .id & " " & scriptname$(.id)
'  debug "refcount = " & .refcount
'  debug "totaluse = " & .totaluse
'  debug "lastuse = " & .lastuse
'  debug "size = " & .size
 END WITH
NEXT
#ENDIF 'SCRIPTPROFILE

'aim for at most 75% of memory limit
targetmem = mem
IF mem > scriptmemMax \ 2 THEN targetmem = mem * (1 - 0.5 * (mem - scriptmemMax \ 2) / scriptmemMax)

'debug "requested max mem = " & mem & ", target = " & targetmem

FOR i = 0 TO listtail
 WITH *LRUlist(i).p
  IF totalscrmem <= targetmem AND numloadedscr <= maxLoadedScripts - 24 THEN EXIT SUB

  'debug "deallocating " & .id & " " & scriptname(.id) & " size " & .size
  totalscrmem -= .size
  numloadedscr -= 1
  deallocate(.ptr)
  IF .refcount THEN
   FOR j = 0 TO nowscript
    IF scrat(j).scr = LRUlist(i).p THEN
     'debug "marking scrat(" & j & ") (id = " & scrat(j).id & ") unloaded"
     scrat(j).scr = NULL
     scrat(j).scrdata = NULL
    END IF
   NEXT
  END IF

  IF .next THEN
   .next->backptr = .backptr
  END IF
  *.backptr = .next
 END WITH
 deallocate(LRUlist(i).p)
NEXT

END SUB

FUNCTION commandname (byval id as integer) as string
 'cmd_default_names array
#include "scrcommands.bi"

 STATIC cache(32) as IntStrPair
 DIM as string ret
 ret = search_string_cache(cache(), id, game)
 IF ret <> "" THEN RETURN ret
 IF id >= 0 AND id <= UBOUND(cmd_default_names) THEN ret = cmd_default_names(id)
 IF ret = "" THEN ret = "cmd" & id

 DIM as short headersz, formatv, records, offset

 '--could check workingdir as well like we do in runscript; but doesn't seem necessary
 fh = FREEFILE
 IF OPEN(tmpdir + "commands.bin" FOR BINARY ACCESS READ AS fh) THEN
  add_string_cache cache(), id, ret
  RETURN ret
 END IF

 GET #fh, , headersz
 GET #fh, , formatv
 GET #fh, , records

 IF formatv > 0 OR id < 0 OR id >= records THEN
  CLOSE fh
  add_string_cache cache(), id, ret
  RETURN ret
 END IF

 GET #fh, 1 + headersz + 2 * id, offset

 IF offset = 0 THEN
  CLOSE fh
  add_string_cache cache(), id, ret
  RETURN ret
 END IF

 DIM rec(25) as short
 GET #fh, 1 + offset + 2, rec()
 ret = readbinstring(rec(), 0, 50)
 CLOSE fh
 add_string_cache cache(), id, ret
 RETURN ret
END FUNCTION

'errorlevel scheme:
'1: informative messages
'2: possibly suspicious operation, eg. re-freeing a slice
'3: suspicious operation on weak type or suspicious argument type (unimplemented)
'4: warning on auto-bound() argument  (suppressed in old games)
'5: bad argument/operation       (not suppressed by default)
'6: corrupt script data/unimplemented feature/interpreter can't continue
'7: impossible condition; engine bug
SUB scripterr (e AS STRING, errorlevel as integer = 5)
 'mechanism to handle scriptwatch throwing errors
 STATIC as integer recursivecall

 'this is the correct way to declare a dynamic static array. Also, you have to REDIM it before use!
 STATIC as integer ignorelist()
 REDIM PRESERVE ignorelist(UBOUND(ignorelist))

 DIM as string errtext()
 DIM as string scriptlocation
 DIM as integer scriptcmdhash

 debug "Scripterr(" & errorlevel & "): " + e

 IF errorlevel <= err_suppress_lvl THEN EXIT SUB
 IF nowscript >= 0 THEN
  scriptcmdhash = scrat(nowscript).id * 100000 + scrat(nowscript).ptr * 10 + scrat(nowscript).depth
  IF int_array_find(ignorelist(), scriptcmdhash) <> -1 THEN EXIT SUB
 END IF

 recursivecall += 1

 IF errorlevel = 6 THEN e = "Script data may be corrupt or unsupported:" + CHR(10) + e
 IF errorlevel >= 7 THEN e = "PLEASE REPORT THIS POSSIBLE ENGINE BUG" + CHR(10) + e

 IF nowscript < 0 THEN
  e = e + CHR(10) + CHR(10) + "Funny... no scripts running!"
 ELSE
  scriptlocation = scriptname(scrat(nowscript).id)
  FOR i as integer = nowscript - 1 TO 0 STEP -1
   IF scrat(i).state < 0 THEN EXIT FOR 'suspended: not part of the call chain
   scriptlocation = scriptname(scrat(i).id) + " -> " + scriptlocation
  NEXT
  IF LEN(scriptlocation) > 150 THEN scriptlocation = " ..." + RIGHT(scriptlocation, 150)
  e = e + CHR(10) + CHR(10) + "  Call chain (current script last):" + CHR(10) + scriptlocation
 END IF
 split(wordwrap(e, 38), errtext())

 DIM state AS MenuState
 state.pt = 0
 DIM menu AS MenuDef
 ClearMenuData menu
 menu.anchor.y = -1
 menu.offset.y = -100 + 38 + 10 * UBOUND(errtext) 'menus are always offset from the center of the screen
 menu.bordersize = -4

 append_menu_item menu, "Ignore"
 append_menu_item menu, "Don't display any more script errors"
 append_menu_item menu, "Set error suppression level to " & errorlevel
 append_menu_item menu, "Suppress errors from this source"
 append_menu_item menu, "Exit game (without saving)"
 IF recursivecall = 1 THEN  'don't reenter the debugger if possibly already inside!
  IF scrwatch <> 0 THEN
   append_menu_item menu, "Return to debugger"
   state.pt = 5
  ELSE
   append_menu_item menu, "Enter debugger"
  END IF
 END IF

 state.active = YES
 init_menu_state state, menu
 
 setpal master()
 setkeys
 DO
  setwait 55
  setkeys

  IF keyval(scEsc) > 1 THEN 'ignore
   EXIT DO 
  END IF

  IF enter_or_space() THEN
   SELECT CASE state.pt
    CASE 0 'ignore
     EXIT DO
    CASE 1 'hide errors (but not engine bugs)
     err_suppress_lvl = 6
     EXIT DO
    CASE 2 'hide some errors
     err_suppress_lvl = errorlevel
     EXIT DO
    CASE 3 'hide errors from this command
     int_array_append(ignorelist(), scriptcmdhash)
     EXIT DO
    CASE 4
     exitprogram 0
    CASE 5
     scrwatch = 2
     scriptwatcher scrwatch, 0 'clean mode, script state view mode
     EXIT DO
   END SELECT
  END IF
  
  usemenu state

  clearpage vpage

  centerbox 160, 12, 310, 15, 3, vpage
  textcolor uilook(uiText), 0
  IF errorlevel >= 7 THEN
   printstr "Impossible error/engine bug!", 160 - 28*4, 7, vpage
  ELSEIF errorlevel >= 4 THEN
   printstr "Script Error!", 160 - 13*4, 7, vpage
  ELSEIF errorlevel >= 2 THEN
   printstr "Script Warning", 160 - 14*4, 7, vpage
  ELSE
   printstr "Script Diagnostic", 160 - 17*4, 7, vpage
  END IF

  FOR i as integer = 0 TO UBOUND(errtext)
   printstr errtext(i), 8, 25 + 10 * i, vpage
  NEXT

  draw_menu menu, state, vpage

  IF state.pt = 5 THEN
   textcolor uilook(uiSelectedItem), 0 
   printstr "The debugger is a usability train-wreck!", 0, 184, vpage
   printstr "Press F1 inside the debugger to see help", 0, 192, vpage
  END IF
  setvispage vpage

  IF autotestmode THEN
    write_checkpoint
    exitprogram 0
  END IF

  dowait
 LOOP
 ClearMenuData menu
 setkeys
 recursivecall -= 1
END SUB

FUNCTION settingstring (searchee$, setting$, result$) as integer

' checks to see if searchee$ begins with setting$ =
' if so, sets result$ to the uppercased space-trimmed value that
' follows the = sign and returns true. If not found, returns false

settingstring = 0

IF UCASE$(LEFT$(searchee$, LEN(setting$) + 1)) = setting$ + "=" THEN
 result$ = UCASE$(LTRIM$(RTRIM$(MID$(searchee$, LEN(setting$) + 2, 32))))
 settingstring = -1
END IF

END FUNCTION

SUB shop (id, needf)

DIM storebuf(40), menu(10) AS STRING, menuid(10)
DIM sn AS STRING
DIM AS INTEGER i, autopick, w, temp, tog, inn, rsr, h, c, t, o
DIM page AS INTEGER
DIM holdscreen AS INTEGER
DIM st as MenuState

FOR i = 0 TO 7
 menuid(i) = i
NEXT i

menu(0) = readglobalstring$(70, "Buy", 10)
menu(1) = readglobalstring$(71, "Sell", 10)
menu(2) = readglobalstring$(73, "Hire", 10)
menu(3) = readglobalstring$(72, "Inn", 10)
menu(4) = readglobalstring$(63, "Equip", 10)
menu(5) = readglobalstring$(66, "Save", 10)
menu(6) = readglobalstring$(68, "Map", 10)
menu(7) = readglobalstring$(65, "Team", 10)

st.size = 22
st.pt = 0
st.last = -1
GOSUB initshop
IF st.last = -1 THEN EXIT SUB
IF st.last = 0 THEN autopick = 1
st.last += 1
menu(st.last) = readglobalstring$(74, "Exit", 10)
menuid(st.last) = 8

menusound gen(genAcceptSFX)

'--Preserve background for display beneath top-level shop menu
holdscreen = duplicatepage(vpage)

page = compatpage

setkeys
DO
 setwait speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 usemenu st
 usemenusounds
 IF carray(ccMenu) > 1 THEN menusound gen(genCancelSFX) : EXIT DO
 IF carray(ccUse) > 1 OR autopick THEN
  IF menuid(st.pt) = 8 THEN '--EXIT
   menusound gen(genCancelSFX)
   EXIT DO
  END IF
  IF menuid(st.pt) = 0 THEN '--BUY
   buystuff id, 0, storebuf()
  END IF
  IF menuid(st.pt) = 1 THEN '--SELL
   sellstuff id, storebuf()
  END IF
  IF menuid(st.pt) = 2 THEN '--HIRE
   buystuff id, 1, storebuf()
  END IF
  IF menuid(st.pt) = 6 THEN '--MAP
   minimap catx(0), caty(0)
  END IF
  IF menuid(st.pt) = 7 THEN '--TEAM
   hero_swap_menu 1
  END IF
  IF menuid(st.pt) = 4 THEN '--EQUIP
   w = onwho(readglobalstring$(108, "Equip Who?", 20), 0)
   IF w >= 0 THEN
    equip w
   END IF
  END IF
  IF menuid(st.pt) = 5 THEN '--SAVE
   temp = picksave(0)
   IF temp >= 0 THEN savegame temp
  END IF
  IF menuid(st.pt) = 3 THEN '--INN
   inn = 0
   IF useinn(inn, storebuf(18), needf, holdscreen) THEN
    IF inn = 0 THEN
     innRestore
    END IF
    IF storebuf(19) > 0 THEN
     '--Run animation for Inn
     rsr = runscript(storebuf(19), nowscript + 1, -1, "inn", plottrigger)
     IF rsr = 1 THEN
      EXIT DO
     END IF
    ELSE
     '--Inn has no script, do simple fade
     fadeout 0, 0, 80
     needf = 1
    END IF
   END IF
   copypage holdscreen, vpage
  END IF
  IF autopick THEN EXIT DO
 END IF
 h = (st.last + 2) * 10
 centerbox 160, 104 + (h * .5), 96, h, 1, page
 centerbox 160, 90, LEN(sn) * 8 + 8, 16, 1, page
 edgeprint sn, xstring(sn, 160), 85, uilook(uiText), page
 FOR i = 0 TO st.last
  c = uilook(uiMenuItem): IF st.pt = i THEN c = uilook(uiSelectedItem + tog)
  edgeprint menu(i), xstring(menu(i), 160), 109 + i * 10, c, page
 NEXT i
 setvispage vpage
 copypage holdscreen, vpage
 IF needf = 1 THEN needf = 0: fadein: setkeys
 IF needf > 1 THEN needf = needf - 1
 dowait
LOOP
FOR t = 4 TO 5: carray(t) = 0: NEXT t
freepage page
freepage holdscreen

evalitemtags
party_change_updates
EXIT SUB

initshop:
setpicstuf storebuf(), 40, -1
loadset game + ".sho", id, 0
sn = readbadbinstring$(storebuf(), 0, 15, 0)
o = 0
FOR i = 0 TO 7
 IF readbit(storebuf(), 17, i) THEN
  SWAP menu(i), menu(o)
  SWAP menuid(i), menuid(o)
  st.last = o
  o = o + 1
 END IF
NEXT i
RETRACE
END SUB

'holdscreen is a copy of vpage (not a compatpage)
FUNCTION useinn (inn as integer, price as integer, needf as integer, holdscreen as integer) as integer
DIM menu(1) AS STRING
DIM AS INTEGER i, y
DIM page as integer
page = compatpage

useinn = 0

menu(0) = readglobalstring$(49, "Pay", 10)
menu(1) = readglobalstring$(50, "Cancel", 10)
inncost$ = readglobalstring$(143, "THE INN COSTS", 20)
youhave$ = readglobalstring$(145, "You have", 20)
menusound gen(genAcceptSFX)
setkeys
DO
 setwait speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 IF carray(ccMenu) > 1 THEN
  inn = 1 '?? Remember cursor position maybe?
  menusound gen(genCancelSFX)
  EXIT DO
 END IF
 usemenusounds
 usemenu inn, 0, 0, 1, 2
 'alternatively
 IF carray(ccLeft) > 1 OR carray(ccRight) > 1 THEN
  menusound gen(genCursorSFX)
  inn = inn XOR 1
 END IF
 IF carray(ccUse) > 1 THEN
  IF inn = 0 AND gold >= price THEN
   gold = gold - price
   useinn = -1
   menusound gen(genAcceptSFX)
   EXIT DO
  ELSE
   menusound gen(genCancelSFX)
  END IF
  IF inn = 1 THEN EXIT DO
 END IF

 'Draw screen
 copypage holdscreen, vpage
 edgeboxstyle 0, 3, 218, herocount() * 10 + 4, 0, page
 y = 0
 FOR i = 0 TO 3
  IF hero(i) > 0 THEN
   col = uilook(uiText)
   edgeprint names(i), 128 - LEN(names(i)) * 8, 5 + y * 10, col, page
   edgeprint STR$(ABS(gam.hero(i).stat.cur.hp)) + "/" + STR$(ABS(gam.hero(i).stat.max.hp)), 136, 5 + y * 10, col, page
   y = y + 1
  END IF
 NEXT i
 centerfuz 160, 90, 200, 60, 1, page
 rectangle 130, 92, 60, 22, uilook(uiHighlight), page 'orig colour 20
 edgeprint inncost$ & " " & price & " " & readglobalstring(32, "Money"), 160 - LEN(inncost$ & price & " " & readglobalstring(32, "Money")) * 4, 70, uilook(uiText), page
 edgeprint youhave$ & " " & gold & " " & readglobalstring(32, "Money"), 160 - LEN(youhave$ & gold & " " & readglobalstring(32, "Money")) * 4, 80, uilook(uiText), page
 FOR i = 0 TO 1
  col = uilook(uiMenuItem): IF inn = i THEN col = uilook(uiSelectedItem + tog)
  edgeprint menu(i), 160 - LEN(menu(i)) * 4, 94 + i * 8, col, page
 NEXT i

 setvispage vpage
 IF needf = 1 THEN needf = 0: fadein: setkeys
 IF needf > 1 THEN needf = needf - 1
 dowait
LOOP
freepage page
party_change_updates
END FUNCTION

SUB tagdisplay
STATIC st AS menustate
st.size = 6
st.last = 1999

IF keyval(scCtrl) > 0 THEN
 IF keyval(scNumpadMinus) > 1 OR keyval(scMinus) > 1 THEN
  settag st.pt, NO
  tag_updates
 END IF
 IF keyval(scNumpadPlus) > 1 OR keyval(scPlus) > 1 THEN
  settag st.pt, YES
  tag_updates
 END IF
ELSE
 IF usemenu(st, scMinus, scPlus) = 0 THEN
  'little bit hacky...
  usemenu(st, scNumpadMinus, scNumpadPlus)
 END IF
END IF

fuzzyrect 0, 0, 208, (st.size + 1) * 10, uilook(uiOutline), dpage
FOR i = st.top TO st.top + st.size
 DIM temp as string = i & "  "
 SELECT CASE i
  CASE 0, 1
   temp += "Reserved Tag"
  CASE IS > 1
   temp += load_tag_name(i)
 END SELECT
 c = uilook(uiDisabledItem)
 IF istag(i, 0) THEN c = uilook(uiHighlight) 'hmm
 edgeprint temp, 16, (i - st.top) * 10, c, dpage
 IF i = st.pt THEN edgeprint "->", 0, (i - st.top) * 10, uilook(uiText), dpage
NEXT i
END SUB

SUB writejoysettings
fh = FREEFILE
OPEN exepath$ + SLASH + "joyset.ini" FOR OUTPUT AS #fh
PRINT #fh, "#Joystick/gamepad configuration"
PRINT #fh, "UPTHRESH=" + STR$(joy(9))
PRINT #fh, "DOWNTHRESH=" + STR$(joy(10))
PRINT #fh, "LEFTTHRESH=" + STR$(joy(11))
PRINT #fh, "RIGHTTHRESH=" + STR$(joy(12))
PRINT #fh, "USEBUTTON=" + STR$(joy(13) - 2)
PRINT #fh, "MENUBUTTON=" + STR$(joy(14 - 2))
CLOSE #fh
END SUB

FUNCTION herocount (last AS INTEGER = 3) AS INTEGER
'--differs from liveherocount() in that it does not care if they are alive
 DIM i AS INTEGER
 DIM count AS INTEGER
 count = 0
 FOR i = 0 TO last
  IF hero(i) > 0 THEN count += 1
 NEXT i
 RETURN count
END FUNCTION
