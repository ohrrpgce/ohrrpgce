'OHRRPGCE GAME - Various unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z

#include "udts.bi"
#include "compat.bi"
#include "allmodex.bi"
#include "common.bi"
#include "gglobals.bi"
#include "const.bi"
#include "scrconst.bi"
#include "uiconst.bi"
#include "loading.bi"
#include "slices.bi"

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
DECLARE SUB show_load_index(z AS INTEGER, caption AS STRING, slot AS INTEGER=0)
DECLARE SUB teleporttooltend (BYREF mini AS Frame Ptr, tilemap(), tilesets() AS TilesetData ptr, BYREF zoom, BYVAL map, BYREF mapsize AS XYPair, BYREF minisize AS XYPair, BYREF offset AS XYPair)
DECLARE SUB rebuild_inventory_captions (invent() AS InventSlot)

REM $STATIC

SUB addhero (who, slot, stat(), forcelevel=-1)
DIM wbuf(100), thishbits(4)

dim her as herodef

'--load hero's data
loadherodata @her, who - 1

'--load data of hero's default weapon
loaditemdata wbuf(), her.def_weapon

'--do level forcing
IF forcelevel >= 0 THEN her.def_level = forcelevel

'--do average level enforcement
IF her.def_level < 0 THEN her.def_level = averagelev(stat())

'--formally add hero
hero(slot) = who

'---MUST SET DEFAULT EQUIP---
wep = large(wbuf(48), 1)
FOR i = 0 TO 4
 eqstuf(slot, i) = 0
NEXT i
eqstuf(slot, 0) = her.def_weapon + 1

'--fill in stats
FOR i = 0 TO 11
 stat(slot, 0, i) = atlevel(her.def_level, her.Lev0.sta(i), her.Lev99.sta(i)) + wbuf(54 + i)
 stat(slot, 1, i) = stat(slot, 0, i)
NEXT i
'--weapon picture and palette
stat(slot, 0, 13) = wbuf(52)
stat(slot, 1, 13) = wbuf(53)

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

'--elemental bitsets
FOR i = 0 TO 2
 thishbits(i) = her.bits(i)
 nativehbits(slot, i) = her.bits(i)
NEXT i

'--reset levelmp
resetlmp slot, her.def_level

'--setup experience
stat(slot, 0, 12) = her.def_level
stat(slot, 1, 12) = 0
exlev(slot, 0) = 0
exlev(slot, 1) = exptolevel(her.def_level)

'--clear learnmask slots (just to be really thorough)
FOR i = slot * 6 TO slot * 6 + 5
 learnmask(i) = 0
NEXT

'--heros are added unlocked
setbit hmask(), 0, who - 1, 0

'--appearance settings
' udts are self documenting
stat(slot, 0, 14) = her.sprite
stat(slot, 0, 15) = her.sprite_pal
stat(slot, 1, 14) = her.walk_sprite
stat(slot, 1, 15) = her.walk_sprite_pal
stat(slot, 0, 16) = her.def_weapon + 1'default weapon

'--read hero's name (doing this last for no real reason)
names(slot) = her.name
'--if renaming is permitted, do it
IF readbit(thishbits(), 0, 24) THEN
 '--add-hero rename is allowed
 renamehero slot
END IF

END SUB

FUNCTION atlevel (now as integer, a0 as integer, a99 as integer) as integer

'CLS : a = 80: b = 8500: PRINT : FOR i = 0 TO 99 STEP 5: PRINT i; " "; atlevel(i, a, b): LINE (640, i)-(640 - atlevel(i, a, b) / 100, i), 4: NEXT i

'atlevel = (.8 + now / 50) * now * ((a99 - a0) / 100) + a0 + .1
IF now < 0 THEN atlevel = 0: EXIT FUNCTION
atlevel = (.8 + now / 50) * now * ((a99 - a0) / 275.222) + a0 + .1

END FUNCTION

FUNCTION averagelev (stat() as integer) as integer
average = 0
count = 0
FOR i = 0 TO 3
 IF hero(i) > 0 THEN average = average + stat(i, 0, 12): count = count + 1
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
 clearpage dpage
 dowait
LOOP

END SUB

FUNCTION consumeitem (index as integer) as integer
'--subtracts one of an item at a location. If the item is depleted, returns true. If there are some of the item left, it returns false
consumeitem = 0
inventory(index).num -= 1
IF inventory(index).num <= 0 THEN
 inventory(index).used = 0
 consumeitem = -1
END IF
itstr index
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
  itstr o
  IF amount = 0 THEN EXIT FOR
 END IF
NEXT o
END SUB

SUB doswap (s, d, stat())

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

'---hero stats
FOR i = 0 TO 1
 FOR o = 0 TO 16
  SWAP stat(s, i, o), stat(d, i, o)
 NEXT o
NEXT i

'---Level-MP
FOR i = 0 TO 7
 SWAP lmp(s, i), lmp(d, i)
NEXT i

'---Experience
FOR i = 0 TO 1
 SWAP exlev(s, i), exlev(d, i)
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

'---elemental bitsets
FOR i = 0 TO 4
 SWAP nativehbits(s, i), nativehbits(d, i)
NEXT i

'---reload hero pictures and palettes
vishero stat()

'hero(40), bmenu(40,5), spell(40,3,23), stat(40,1,13), lmp(40,7), exlev(40,1), names(40), eqstuf(40,4)
END SUB

SUB drawsay ()
STATIC tog AS INTEGER
tog = tog XOR 1

IF txt.show_lines < 7 THEN
 txt.show_lines = txt.show_lines + 1
 '--play sounds for non-blank lines
 IF txt.show_lines > 1 THEN
  IF trim(txt.box.text(txt.show_lines)) <> "" THEN menusound gen(genTextboxLetter)
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

SUB evalherotag (stat())

leader = -1
FOR i = 3 TO 0 STEP -1
 IF hero(i) > 0 THEN leader = hero(i) - 1
NEXT i

FOR i = 0 TO small(gen(genMaxHero), 59) '--for each available hero
 FOR j = 0 TO 3
  IF herobits(i, j) > 1 THEN setbit tag(), 0, herobits(i, j), 0
 NEXT j
 FOR j = 0 TO 40
  IF hero(j) - 1 = i THEN
   IF herobits(i, 0) > 1 THEN setbit tag(), 0, herobits(i, 0), 1 '---HAVE HERO
   IF herobits(i, 1) > 1 AND stat(j, 0, statHP) THEN setbit tag(), 0, herobits(i, 1), 1 '---IS ALIVE
   IF herobits(i, 2) > 1 AND i = leader THEN setbit tag(), 0, herobits(i, 2), 1 '---IS LEADER
   IF herobits(i, 3) > 1 AND j < 4 THEN setbit tag(), 0, herobits(i, 3), 1 '---IN PARTY
  END IF
 NEXT j
NEXT i

'292     have hero tag
'293     is alive tag
'294     is leader tag
'295     is in active party tag
END SUB

SUB evalitemtag

FOR i = 0 TO maxMaxItems
 'clear all four bits
 FOR j = 0 TO 3
  IF itembits(i, j) > 1 THEN setbit tag(), 0, itembits(i, j), 0
 NEXT j
NEXT i

'search inventory slots
FOR j = 0 TO last_inv_slot()
 'get item ID
 id = inventory(j).id
 IF inventory(j).used THEN 'there is an item in this slot
  IF itembits(id, 0) > 1 THEN setbit tag(), 0, itembits(id, 0), 1 'you have it
  IF itembits(id, 1) > 1 THEN setbit tag(), 0, itembits(id, 1), 1 'it is in your inventory
 END IF
NEXT j

FOR j = 0 TO 40 'search hero list
 FOR k = 0 TO 4 'search equipment slots
  id = eqstuf(j, k) - 1
  IF id >= 0 THEN ' there is an item equipped in this slot
   IF itembits(id, 0) > 1 THEN setbit tag(), 0, itembits(id, 0), 1 'you have it
   IF itembits(id, 2) > 1 THEN setbit tag(), 0, itembits(id, 2), 1 'it is equipped
   IF j < 4 AND itembits(id, 3) > 1 THEN setbit tag(), 0, itembits(id, 3), 1   'it is equipped by an active hero
  END IF
 NEXT k
NEXT j

'itembits(n,0)      when have tag
'itembits(n,1)      is in inventory
'itembits(n,2)      is equiped tag
'itembits(n,3)      is equiped by hero in active party

END SUB

FUNCTION findhero (who as integer, f as integer, l as integer, d as integer) as integer
result = -1
FOR i = f TO l STEP d
 IF hero(i) = who OR (who = -1 AND hero(i)) THEN result = i: EXIT FOR
NEXT i
findhero = result
END FUNCTION

SUB heroswap (iAll%, stat())
'--Preserve background for display beneath the hero swapper
holdscreen = allocatepage
copypage vpage, holdscreen

DIM swindex(40), swname$(40)

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
    doswap temp, temp2, stat()
    swapme = -1
    GOSUB resetswap
    EXIT DO
   LOOP
  END IF
 END IF

 GOSUB showswapmenu
 SWAP vpage, dpage
 setvispage vpage
 copypage holdscreen, dpage
 dowait
LOOP
FOR t = 4 TO 5: carray(t) = 0: NEXT t
MenuSound gen(genCancelSFX)
freepage holdscreen
EXIT SUB

refreshemenu:
IF ecsr < top THEN top = large(ecsr, 0)
IF ecsr > top + 7 THEN top = ecsr - 7
IF hero(acsr) AND ecsr < 0 THEN info$ = names(acsr) ELSE info$ = ""
RETRACE

'---DRAWS SWAP MENU AND CURRENT SELECTION----
showswapmenu:
centerbox 160, 66, 130, 38, 1, dpage
o = 0
FOR i = 0 TO 3
 IF i = swapme OR hero(i) > 0 THEN rectangle 105 + (30 * i), 60, 20, 20, uilook(uiTextBox), dpage
 IF hero(i) THEN
  '5th frame: down
  sprite_draw herow(o).sprite + 4, herow(o).pal, 105 + i * 30, 60 + (i = swapme) * 6, 1, -1, dpage
  o = o + 1
 END IF
NEXT i
IF ecsr < 0 THEN edgeprint CHR$(24), 111 + 30 * acsr, 52, uilook(uiSelectedItem + tog), dpage
IF iAll THEN
 centerbox 160, 100 + small(high, 8) * 5, wide * 8 + 16, small(high, 8) * 10 + 10, 1, dpage
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
  edgeprint swname$(i), xstring(swname$(i), 160), 100 + (i - top) * 10, c, dpage
 NEXT i
END IF
IF LEN(info$) THEN
 centerbox 160, 44, (LEN(info$) + 2) * 8, 14, 1, dpage
 edgeprint info$, xstring(info$, 160), 39, uilook(uiText), dpage
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
  swname$(la) = names(i)
  wide = large(wide, LEN(swname$(la)))
 END IF
NEXT i
la = la + 1
FOR i = 40 TO 4 STEP -1
 IF hero(i) = 0 THEN
  swindex(la) = i
  swname$(la) = readglobalstring$(48, "-REMOVE-", 10)
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

SUB show_load_index(z AS INTEGER, caption AS STRING, slot AS INTEGER=0)
 'debug "SAV:" & LEFT(caption & STRING(20, " "), 20) & " int=" & z + slot * 15000
END SUB

SUB loadgame (slot, stat())
DIM gmaptmp(dimbinsize(binMAP))

'--return gen to defaults
xbload game + ".gen", gen(), "General data is missing from " + game

sg$ = savefile
setpicstuf buffer(), 30000, -1
loadset sg$, slot * 2, 0

savver = buffer(0)
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

temp$ = ""
FOR i = 0 TO 24
 IF buffer(i + 9) < 0 OR buffer(i + 9) > 255 THEN buffer(i + 9) = 0
 IF buffer(i + 9) > 0 THEN temp$ = temp$ + CHR$(buffer(i + 9))
NEXT i
gold = str2int(temp$)

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
 dummy = buffer(z): z = z + 1
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
FOR i = 0 TO 40
 FOR o = 0 TO 1
  temp$ = ""
  FOR j = 0 TO 25
   IF buffer(z) < 0 OR buffer(z) > 255 THEN buffer(z) = 0
   IF buffer(z) > 0 THEN temp$ = temp$ + CHR$(buffer(z))
   z = z + 1
  NEXT j
  exlev(i, o) = str2int(temp$)
 NEXT o
NEXT i
show_load_index z, "names"
FOR i = 0 TO 40
 temp$ = ""
 FOR j = 0 TO 16
  IF buffer(z) < 0 OR buffer(z) > 255 THEN buffer(z) = 0
  IF buffer(z) > 0 THEN temp$ = temp$ + CHR$(buffer(z))
  z = z + 1
 NEXT j
 names(i) = temp$
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
loadset sg$, slot * 2 + 1, 0

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
picpalmagicnum = buffer(z): z = z + 1
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
nativebitmagicnum = buffer(z): z = z + 1
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
 FOR sl = 0 TO 40
  IF hero(sl) > 0 THEN
   loadherodata @her, hero(sl) - 1
   stat(sl, 0, 14) = her.sprite
   stat(sl, 0, 15) = her.sprite_pal
   stat(sl, 1, 14) = her.walk_sprite
   stat(sl, 1, 15) = her.walk_sprite_pal
   stat(sl, 0, 16) = her.def_weapon + 1'default weapon
  END IF
 NEXT sl
END IF

IF nativebitmagicnum <> 4444 THEN
 '--fix native hero bits
 FOR sl = 0 TO 40
  IF hero(sl) > 0 THEN
   loadherodata @her, hero(sl) - 1
   FOR i = 0 TO 2 '??
    nativehbits(sl, i) = her.bits(i)
   NEXT i
  END IF
 NEXT sl
END IF

'See http://gilgamesh.hamsterrepublic.com/wiki/ohrrpgce/index.php/SAV for docs
END SUB

SUB loadglobalvars (slot, first, last)
DIM buf((last - first + 1) * 2) = ANY
IF isfile$(savefile) THEN
 fh = FREEFILE
 OPEN savefile FOR BINARY AS #fh

 IF first <= 1024 THEN
  'grab first-final
  final = small(1024, last)
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
  start = large(1025, first)
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

SUB minimap (x, y, tilesets() as TilesetData ptr)
 DIM mini AS Frame Ptr
 DIM zoom AS INTEGER = -1
 mini = createminimap(scroll(), tilesets(), zoom)
 setmapdata scroll(), pass(), 0, 0

 DIM minisize AS XYPair
 minisize.x = mini->w
 minisize.y = mini->h
 
 DIM offset AS XYPair
 offset.x = 160 - minisize.x / 2
 offset.y = 100 - minisize.y / 2

 edgeboxstyle offset.x - 2, offset.y - 2, minisize.x + 4, minisize.y + 4, 0, vpage
 sprite_draw mini, NULL, offset.x, offset.y, 1, NO, vpage
 sprite_unload @mini

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

FUNCTION teleporttool (tilesets() as TilesetData ptr) as integer
 REDIM tilemap(2) AS INTEGER
 DIM mini AS Frame Ptr
 DIM zoom AS INTEGER
 DIM i AS INTEGER

 DIM mapsize AS XYPair
 DIM minisize AS XYPair 'pixels
 DIM offset AS XYPair

 'We don't bother reloading tilesets and tilemaps if not changing map
 teleporttooltend mini, scroll(), tilesets(), zoom, -1, mapsize, minisize, offset

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
    teleporttooltend mini, tilemap(), tilesets(), zoom, destmap, mapsize, minisize, offset
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
   IF carray(ccMenu) > 1 THEN
    loadmaptilesets tilesets(), gmap()
    refresh_map_slice_tilesets
    EXIT DO 'cancel
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
    FOR i = 0 TO 15
     catx(i) = dest.x * 20
     caty(i) = dest.y * 20
    NEXT
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
 sprite_unload @mini
 setkeys
 flusharray carray(), 7, 0
 MenuSound gen(genCancelSFX)
 setmapdata scroll(), pass(), 0, 0
 EXIT FUNCTION

redraw:
 'reblit the minimap to a spare page
 copypage 3, vpage
 edgeboxstyle offset.x - 2, offset.y - 2, minisize.x + 4, minisize.y + 4, 0, vpage
 sprite_draw mini, NULL, offset.x - camera.x, offset.y - camera.y, 1, NO, vpage
 RETRACE

END FUNCTION

SUB teleporttooltend (BYREF mini AS Frame Ptr, tilemap(), tilesets() AS TilesetData ptr, BYREF zoom, BYVAL map, BYREF mapsize AS XYPair, BYREF minisize AS XYPair, BYREF offset AS XYPair)
 IF map > -1 THEN
  DIM gmap2(dimbinsize(binMAP)) AS INTEGER
  loadtiledata maplumpname$(map, "t"), tilemap(), 3
  loadrecord gmap2(), game + ".map", dimbinsize(binMAP), map
  loadmaptilesets tilesets(), gmap2()
  refresh_map_slice_tilesets
  refresh_map_slice
 END IF
 'minimum zoom level to make tiles easy to pick
 zoom = bound(small(320 \ tilemap(0), 200 \ tilemap(1)), 5, 20)
 sprite_unload @mini
 mini = createminimap(tilemap(), tilesets(), zoom)
 mapsize.x = tilemap(0)
 mapsize.y = tilemap(1)
 offset.x = large(160 - mini->w / 2, 0)
 offset.y = large(100 - mini->h / 2, 0)
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
 centerbox 160, 100, 140, 52, 1, dpage
 o = 0
 FOR i = 0 TO 3
  IF hero(i) > 0 THEN
   wt = 0: IF w = i THEN wt = INT(wtg / 2)
   sprite_draw herow(o).sprite + (2 * 2) + wt, herow(o).pal, 100 + i * 30, 100, 1, -1, dpage
   o = o + 1
  END IF
 NEXT i
 edgeprint CHR(25), 106 + w * 30, 90, uilook(uiSelectedItem + tog), dpage
 edgeprint caption, xstring(caption, 160), 80, uilook(uiText), dpage
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP

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

FUNCTION readscriptvar (id as integer) as integer

SELECT CASE id
 CASE IS < 0 'local variable
  readscriptvar = heap(scrat(nowscript).heap + ABS(id) - 1)
 CASE 0 TO 4095 'global variable
  readscriptvar = global(id)
 CASE ELSE
  scripterr "Cannot read global " & id & ". Out of range", 4
END SELECT

END FUNCTION

SUB renamehero (who)

dim her as herodef
loadherodata @her, hero(who) - 1

limit = her.max_name_len
IF limit = 0 THEN limit = 16

prompt$ = readglobalstring$(137, "Name the Hero", 20)
spacer$ = STRING$(large(limit, LEN(names(who))), " ")
remember$ = names(who)
rememberjoycal = gen(genJoy)
gen(genJoy) = 1'--disable joystick calibration

copypage dpage, vpage
setvispage vpage
IF fadestate = 0 THEN
 fadein
 needfadeout = 1
END IF

setkeys
DO
 setwait speedcontrol
 setkeys
 playtimer
 control
 centerbox 160, 100, 168, 32, 1, dpage
 IF carray(ccUse) > 1 AND keyval(scSpace) = 0 THEN EXIT DO
 IF carray(ccMenu) > 1 THEN names(who) = remember$
 strgrabber names(who), limit
 edgeprint prompt$, xstring(prompt$, 160), 90, uilook(uiText), dpage
 textcolor uilook(uiHighlight), uiLook(uiHighlight)
 printstr spacer$, xstring(spacer$, 161), 101, dpage
 edgeprint names(who), xstring(names(who), 160), 100, uilook(uiMenuItem), dpage
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
menusound gen(genAcceptSFX)
gen(genJoy) = rememberjoycal '-- restore joystick calibration setting

IF needfadeout = 1 THEN
 fadeout 0, 0, 0
END IF

END SUB

SUB resetgame (stat(), scriptout$)
gam.map.id = 0
catx(0) = 0
caty(0) = 0
catd(0) = 0
gam.random_battle_countdown = 0
'leader = 0
mapx = 0
mapy = 0
gold = 0
scriptout$ = ""
'--return gen to defaults
xbload game + ".gen", gen(), "General data is missing from " + game

CleanNPCL npc(),300
flusharray tag(), 126, 0
flusharray hero(), 40, 0
FOR i = 0 TO 40
 FOR o = 0 TO 1
  FOR j = 0 TO 16
   stat(i, o, j) = 0
  NEXT j
 NEXT o
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
 FOR o = 0 TO 1
  exlev(i, o) = 0
 NEXT o
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
  gam.stock(i, o) = 0
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
 remove_menu i
NEXT i

'delete temp files that are part of the game state
deletetemps

'doesn't unload scripts: not needed
killallscripts

FOR i = 0 TO max_npc_defs
 WITH npcs(i)
  sprite_unload(@.sprite)
  palette16_unload(@.pal)
 END WITH
NEXT i
FOR i = 0 TO UBOUND(herow)
 WITH herow(i)
  sprite_unload(@.sprite)
  palette16_unload(@.pal)
 END WITH
NEXT

clear_box_border_cache

FOR i = 0 TO 15
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

DestroyGameSlices
'plotslices() should now be all zeroed out. Rather than checking, check slicedebug() (if enabled)
SliceDebugDump YES
SetupGameSlices

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

IF trigger <> 0 THEN n = decodetrigger(id, trigger) ELSE n = id

IF n = 0 THEN
 runscript = 2 '--quiet failure (though decodetrigger might have shown a scripterr)
 EXIT FUNCTION
END IF

IF index > 127 THEN
 scripterr "interpreter overloaded", 5
 runscript = 0 '--error
 scripterr "failed to load " + er$ + " script " & n & " " & scriptname(n), 5
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
  scripterr "Failed to load " + er$ + " script " & n & " " & scriptname(n), 5
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
 curcmd = cast(ScriptCommand ptr, .scrdata + .ptr) 'just in case it's needed before subread is run
 
 scrat(index + 1).heap = .heap + .scr->vars

 IF scrat(index + 1).heap > 2048 THEN
  scripterr "Script heap overflow", 5
  runscript = 0'--error
  scripterr "failed to load " + er$ + " script " & n & " " & scriptname(n), 5
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
    scripterr "script id " & n & " does not exist", 5
    RETURN NULL
   END IF
  END IF
 END IF

 thisscr = allocate(sizeof(ScriptData))
 WITH *thisscr

  f = FREEFILE
  OPEN scriptfile$ FOR BINARY AS #f

  GET #f, 1, temp
  skip = temp

  GET #f, 3, temp
  'some HSX files seem to have an illegal negative number of variables
  .vars = temp
  .vars = large(.vars, 0)
 
  IF skip >= 6 THEN
   GET #f, 5, temp
   .args = temp
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
   scripterr "script " & n & " is in an unsupported format", 5
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
   IF .strtable THEN .strtable = (temp - skip) \ wordsize
  ELSE
   .strtable = 0
  END IF

  'set an arbitrary max script buffer size (scriptmemMax in const.bi), individual scripts must also obey
  .size = (LOF(f) - skip) \ wordsize
  IF .size > scriptmemMax THEN
   scripterr "Script " & n & " exceeds maximum size by " & .size * 100 \ scriptmemMax - 99 & "%", 5
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
   scripterr "Could not allocate memory to load script", 5
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
 DIM as string filename
 lumpfile = tmpdir + "commands.bin"
 '--could check workingdir as well like we do in runscript; but doesn't seem necessary
 IF isfile(lumpfile) THEN
  
 ELSE
  RETURN "cmd" & id
 END IF

END FUNCTION

SUB savegame (slot, stat())

DIM gmaptmp(dimbinsize(binMAP))

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

temp$ = STR$(gold)
FOR i = 0 TO 24
 IF i < LEN(temp$) THEN
  IF MID$(temp$, i + 1, 1) <> "" THEN buffer(i + 9) = ASC(MID$(temp$, i + 1, 1))
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
FOR i = 0 TO 40
 FOR o = 0 TO 1
  temp$ = STR$(exlev(i, o))
  FOR j = 0 TO 25
   IF j < LEN(temp$) THEN
    IF MID$(temp$, j + 1, 1) <> "" THEN buffer(z) = ASC(MID$(temp$, j + 1, 1))
   ELSE
    buffer(z) = 0
   END IF
   z = z + 1
  NEXT j
 NEXT o
NEXT i
FOR i = 0 TO 40
 temp$ = names(i)
 FOR j = 0 TO 16
  IF j < LEN(temp$) THEN
   IF MID$(temp$, j + 1, 1) <> "" THEN buffer(z) = ASC(MID$(temp$, j + 1, 1))
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
sg$ = savefile
storeset sg$, slot * 2, 0

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
sg$ = savefile
storeset sg$, slot * 2 + 1, 0

'See http://gilgamesh.hamsterrepublic.com/wiki/ohrrpgce/index.php/SAV for docs
END SUB

SUB saveglobalvars (slot, first, last)
DIM buf((last - first + 1) * 2) = ANY
fh = FREEFILE
OPEN savefile FOR BINARY AS #fh
IF first <= 1024 THEN
 'output first-final
 final = small(1024, last)
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
 start = large(1025, first)
 FOR i = 0 TO last - start
  buf(i * 2) = global(start + i)
  buf(i * 2 + 1) = global(start + i) shr 16
 NEXT
 SEEK #fh, 60000 * slot + 4 * (start - 1025) + 45077  '22538 * 2 + 1
 storerecord buf(), fh, (last - start + 1) * 2, -1
END IF
CLOSE #fh
END SUB

'errorlevel scheme:
'1: informative messages
'2: suspicious operation on weak type or suspicious argument type (unimplemented)
'3: warning on auto-bound() argument  (suppressed in old games)
'4: bad argument/operation       (not suppressed by default)
'5: corrupt script data/unimplemented feature/interpreter can't continue
'6: impossible condition; engine bug
SUB scripterr (e AS STRING, errorlevel as integer = 4)
 STATIC as integer recursivecall
 DIM as string errtext()
 DIM as string scriptlocation

 debug "Scripterr(" & errorlevel & "): " + e

 IF errorlevel <= err_suppress_lvl THEN EXIT SUB
 recursivecall += 1

 IF errorlevel = 5 THEN e = "Script data may be corrupt or unsupported:" + CHR(10) + e
 IF errorlevel >= 6 THEN e = "An impossible error occurred. Please report this engine bug!:" + CHR(10) + e

 IF nowscript < 0 THEN
  e = e + CHR(10) + "Funny... no scripts running!"
 ELSE
  scriptlocation = scriptname(scrat(nowscript).id)
  FOR i as integer = nowscript - 1 TO 0 STEP -1
   IF scrat(i).state < 0 THEN EXIT FOR 'suspended: not part of the call chain
   scriptlocation = scriptname(scrat(i).id) + " -> " + scriptlocation
  NEXT
  IF LEN(scriptlocation) > 150 THEN scriptlocation = " ..." + RIGHT(scriptlocation, 150)
  e = e + CHR(10) + CHR(10) + " Call chain (current script last):" + CHR(10) + scriptlocation
 END IF
 split(wordwrap(e, 38), errtext())

 DIM state AS MenuState
 DIM menu AS MenuDef
 menu.anchor.y = -1
 menu.offset.y = -100 + 45 + 10 * UBOUND(errtext) 'menus are always offset from the center of the screen
 menu.bordersize = -4

 append_menu_item menu, "Ignore"
 append_menu_item menu, "Don't display any more script errors"
 append_menu_item menu, "Set error suppression level to " & errorlevel
 append_menu_item menu, "Exit game (without saving)"
 IF recursivecall = 1 THEN  'don't reenter the debugger if possibly already inside!
  append_menu_item menu, "Enter debugger"
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
     err_suppress_lvl = 5
     EXIT DO
    CASE 2 'hide some errors
     err_suppress_lvl = errorlevel
     EXIT DO
    CASE 3
     exitprogram 0
    CASE 4
     scriptwatcher 2, 0 'clean mode, script state view mode
     EXIT DO
   END SELECT
  END IF
  
  usemenu state

  clearpage vpage, iif(errorlevel >= 6, uilook(uiHighlight2), -1)

  centerbox 160, 15, 310, 20, 3, vpage
  textcolor uilook(uiText), 0
  printstr "Script Error!", 108, 10, vpage

  FOR i as integer = 0 TO UBOUND(errtext)
   printstr errtext(i), 8, 30 + 10 * i, vpage
  NEXT

  draw_menu menu, state, vpage

  IF state.pt = 4 THEN
   textcolor uilook(uiSelectedItem), 0 
   printstr "The debugger is a usability train-wreck!", 0, 184, vpage
   printstr "Press F1 inside the debugger to see help", 0, 192, vpage
  END IF
  setvispage vpage
  dowait
 LOOP
 setkeys
 recursivecall -= 1
END SUB

SUB scriptmath
SELECT CASE AS CONST curcmd->value
 CASE 0' random
  scriptret = retvals(0) + INT(RND * (1.0 + retvals(1) - retvals(0))) 'handles the case max-min = 2^32
 CASE 1' exponent
  scriptret = retvals(0) ^ retvals(1)
 CASE 2' modulus
  IF retvals(1) = 0 THEN
   scripterr "division by zero", 4
  ELSE
   scriptret = retvals(0) MOD retvals(1)
  END IF
 CASE 3' divide
  IF retvals(1) = 0 THEN
   scripterr "division by zero", 4
  ELSE
   scriptret = retvals(0) \ retvals(1)
  END IF
 CASE 4'multiply
  scriptret = retvals(0) * retvals(1)
 CASE 5'subtract
  scriptret = retvals(0) - retvals(1)
 CASE 6'add
  scriptret = retvals(0) + retvals(1)
 CASE 7'xor
  scriptret = retvals(0) XOR retvals(1)
 CASE 8'or
  scriptret = retvals(0) OR retvals(1)
 CASE 9'and
  scriptret = retvals(0) AND retvals(1)
 CASE 10'equal
  scriptret = (retvals(0) = retvals(1)) * -1
 CASE 11'not equal
  scriptret = (retvals(0) <> retvals(1)) * -1
 CASE 12'less than
  scriptret = (retvals(0) < retvals(1)) * -1
 CASE 13'greater than
  scriptret = (retvals(0) > retvals(1)) * -1
 CASE 14'less than or equal to
  scriptret = (retvals(0) <= retvals(1)) * -1
 CASE 15'greater than or equal to
  scriptret = (retvals(0) >= retvals(1)) * -1
 CASE 16'set variable
  writescriptvar retvals(0), retvals(1)
  scriptret = retvals(1)
 CASE 17'increment
  writescriptvar retvals(0), readscriptvar(retvals(0)) + retvals(1)
 CASE 18'decrement
  writescriptvar retvals(0), readscriptvar(retvals(0)) - retvals(1)
 CASE 19'not
  IF retvals(0) = 0 THEN
   scriptret = 1
  ELSE
   scriptret = 0
  END IF
 CASE 20'&&
  IF retvals(1) <> 0 THEN scriptret = 1 ELSE scriptret = 0
 CASE 21'||
  IF retvals(1) <> 0 THEN scriptret = 1 ELSE scriptret = 0
 CASE 22'^^
  IF retvals(0) <> 0 XOR retvals(1) <> 0 THEN scriptret = 1 ELSE scriptret = 0
 CASE ELSE
  scripterr "unsupported math", 5
END SELECT
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

SUB shop (id, needf, stat(), tilesets() AS TilesetData ptr)

DIM storebuf(40), menu$(10), menuid(10)

FOR i = 0 TO 7
 menuid(i) = i
NEXT i

menu$(0) = readglobalstring$(70, "Buy", 10)
menu$(1) = readglobalstring$(71, "Sell", 10)
menu$(2) = readglobalstring$(73, "Hire", 10)
menu$(3) = readglobalstring$(72, "Inn", 10)
menu$(4) = readglobalstring$(63, "Equip", 10)
menu$(5) = readglobalstring$(66, "Save", 10)
menu$(6) = readglobalstring$(68, "Map", 10)
menu$(7) = readglobalstring$(65, "Team", 10)

sn$ = ""
last = -1
GOSUB initshop
IF last = -1 THEN EXIT SUB
IF last = 0 THEN autopick = 1
last = last + 1: menu$(last) = readglobalstring$(74, "Exit", 10)

menusound gen(genAcceptSFX)

'--Preserve background for display beneath top-level shop menu
holdscreen = allocatepage
copypage vpage, holdscreen

setkeys
DO
 setwait speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 IF carray(ccUp) > 1 THEN pt = large(pt - 1, 0) : menusound gen(genCursorSFX)
 IF carray(ccDown) > 1 THEN pt = small(pt + 1, last) : menusound gen(genCursorSFX)
 IF carray(ccMenu) > 1 THEN menusound gen(genCancelSFX) : EXIT DO
 IF carray(ccUse) > 1 OR autopick THEN
  IF pt = last THEN menusound gen(genCancelSFX) : EXIT DO
  IF menuid(pt) = 0 THEN '--BUY
   buystuff id, 0, storebuf(), stat()
  END IF
  IF menuid(pt) = 1 THEN '--SELL
   sellstuff id, storebuf(), stat()
  END IF
  IF menuid(pt) = 2 THEN '--HIRE
   buystuff id, 1, storebuf(), stat()
  END IF
  IF menuid(pt) = 6 THEN '--MAP
   minimap catx(0), caty(0), tilesets()
  END IF
  IF menuid(pt) = 7 THEN '--TEAM
   heroswap 1, stat()
  END IF
  IF menuid(pt) = 4 THEN '--EQUIP
   w = onwho(readglobalstring$(108, "Equip Who?", 20), 0)
   IF w >= 0 THEN
    equip w, stat()
   END IF
  END IF
  IF menuid(pt) = 5 THEN '--SAVE
   temp = picksave(0)
   IF temp >= 0 THEN savegame temp, stat()
  END IF
  IF menuid(pt) = 3 THEN '--INN
   inn = 0
   IF useinn(inn, storebuf(18), needf, stat(), holdscreen) THEN
    IF inn = 0 THEN
     innRestore stat()
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
  END IF
  IF autopick THEN EXIT DO
 END IF
 h = (last + 2) * 10
 centerbox 160, 104 + (h * .5), 96, h, 1, dpage
 centerbox 160, 90, LEN(sn$) * 8 + 8, 16, 1, dpage
 edgeprint sn$, xstring(sn$, 160), 85, uilook(uiText), dpage
 FOR i = 0 TO last
  c = uilook(uiMenuItem): IF pt = i THEN c = uilook(uiSelectedItem + tog)
  edgeprint menu$(i), xstring(menu$(i), 160), 109 + i * 10, c, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage holdscreen, dpage
 IF needf = 1 THEN needf = 0: fadein: setkeys
 IF needf > 1 THEN needf = needf - 1
 dowait
LOOP
FOR t = 4 TO 5: carray(t) = 0: NEXT t
freepage holdscreen
EXIT SUB

initshop:
setpicstuf storebuf(), 40, -1
loadset game + ".sho", id, 0
sn$ = readbadbinstring$(storebuf(), 0, 15, 0)
o = 0
FOR i = 0 TO 7
 IF readbit(storebuf(), 17, i) THEN
  SWAP menu$(i), menu$(o)
  SWAP menuid(i), menuid(o)
  last = o
  o = o + 1
 END IF
NEXT i
RETRACE
END SUB

FUNCTION useinn (inn as integer, price as integer, needf as integer, stat() as integer, holdscreen as integer) as integer
DIM menu$(1)
DIM AS INTEGER i, y

useinn = 0

menu$(0) = readglobalstring$(49, "Pay", 10)
menu$(1) = readglobalstring$(50, "Cancel", 10)
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
 IF carray(ccUp) > 1 OR carray(ccDown) > 1 OR carray(ccLeft) > 1 OR carray(ccRight) > 1 THEN
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
 edgeboxstyle 0, 3, 218, herocount() * 10 + 4, 0, dpage
 y = 0
 FOR i = 0 TO 3
  IF hero(i) > 0 THEN
   col = uilook(uiText)
   edgeprint names(i), 128 - LEN(names(i)) * 8, 5 + y * 10, col, dpage
   edgeprint STR$(ABS(stat(i, 0, statHP))) + "/" + STR$(ABS(stat(i, 1, statHP))), 136, 5 + y * 10, col, dpage
   y = y + 1
  END IF
 NEXT i
 centerfuz 160, 90, 200, 60, 1, dpage
 rectangle 130, 92, 60, 22, uilook(uiHighlight), dpage 'orig colour 20
 edgeprint inncost$ & " " & price & " " & readglobalstring(32, "Money"), 160 - LEN(inncost$ & price & " " & readglobalstring(32, "Money")) * 4, 70, uilook(uiText), dpage
 edgeprint youhave$ & " " & gold & " " & readglobalstring(32, "Money"), 160 - LEN(youhave$ & gold & " " & readglobalstring(32, "Money")) * 4, 80, uilook(uiText), dpage
 FOR i = 0 TO 1
  col = uilook(uiMenuItem): IF inn = i THEN col = uilook(uiSelectedItem + tog)
  edgeprint menu$(i), 160 - LEN(menu$(i)) * 4, 94 + i * 8, col, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage holdscreen, dpage
 IF needf = 1 THEN needf = 0: fadein: setkeys
 IF needf > 1 THEN needf = needf - 1
 dowait
LOOP
END FUNCTION

SUB snapshot
pre$ = trimextension$(sourcerpg)

n = 0
DO
 num$ = "" & n
 WHILE LEN(num$) < 4 : num$ = "0" & num$: WEND
 shot$ = pre$ & num$ & ".bmp"
 IF isfile(shot$) = 0 THEN EXIT DO
 n = n + 1
LOOP UNTIL n > 9999

#IFDEF __FB_LINUX__
IF NOT fileiswriteable(shot$) THEN
  shot$ = ENVIRON$("HOME") + SLASH + trimpath$(shot$)
END IF
#ENDIF

screenshot shot$, vpage, master()

END SUB

SUB tagdisplay
STATIC pt, top

pt = large(pt, 0)

IF keyval(scNumpadMinus) > 1 OR keyval(scMinus) > 1 THEN
 '--minus
 IF keyval(scCtrl) > 0 THEN
  setbit tag(), 0, pt, 0
 ELSE
  pt = large(pt - 1, 0)
 END IF
END IF
IF keyval(scNumpadPlus) > 1 OR keyval(scEquals) > 1 THEN
 '--plus
 IF keyval(scCtrl) > 0 THEN
  setbit tag(), 0, pt, 1
 ELSE
  pt = small(pt + 1, 1999)
 END IF
END IF

top = bound(top, pt - 4, pt)

fuzzyrect 0, 0, 208, 50, uilook(uiOutline), dpage
FOR i = top TO top + 4
 temp$ = i & "  "
 SELECT CASE i
  CASE 0, 1
   temp$ = temp$ & "Reserved Tag"
  CASE IS > 1
   temp$ = temp$ & load_tag_name(i)
 END SELECT
 c = uilook(uiDisabledItem)
 IF istag(i, 0) THEN c = uilook(uiHighlight) 'hmm
 edgeprint temp$, 16, (i - top) * 10, c, dpage
 IF i = pt THEN edgeprint "->", 0, (i - top) * 10, uilook(uiText), dpage
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

SUB writescriptvar (BYVAL id, BYVAL newval)

SELECT CASE id
 CASE IS < 0 'local variable
  heap(scrat(nowscript).heap + ABS(id) - 1) = newval
 CASE 0 TO 4095 'global variable
  global(id) = newval
 CASE ELSE
  scripterr "Cannot write global " & id &  ". Out of range", 4
END SELECT

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

SUB rebuild_inventory_captions (invent() AS InventSlot)
 DIM i AS INTEGER
 FOR i = 0 TO inventoryMax
  itstr i
 NEXT i
END SUB
