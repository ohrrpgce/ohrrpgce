'OHRRPGCE GAME - Various unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#ifdef LANG_DEPRECATED
 #define __langtok #lang
 __langtok "deprecated"
 OPTION STATIC
 OPTION EXPLICIT
#endif

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
DECLARE SUB teleporttooltend (byref mini as Frame Ptr, maptilesX() as TileMap, tilesets2() as TilesetData ptr, byref zoom as integer, byval map as integer, byref mapsize as XYPair, byref minisize as XYPair, byref offset as XYPair)
DECLARE SUB inventory_overflow_handler(byval item_id as integer, byval numitems as integer)

'--Global variables

'These have to be in the same module as dequeue_scripts for some reason; looks like a FB bug.
REDIM scrqFirst() as QueuedScript
REDIM scrqBackcompat() as QueuedScript
REDIM scrqLast() as QueuedScript

'--Module local variables

'Used by trigger_script
DIM SHARED trigger_script_failure as integer


'who is the hero id + 1!
SUB addhero (byval who as integer, byval slot as integer, byval forcelevel as integer=-1)
DIM her as HeroDef

'--load hero's data
loadherodata her, who - 1

'--do level forcing
IF forcelevel >= 0 THEN her.def_level = forcelevel

'--do average level enforcement
IF her.def_level < 0 THEN her.def_level = averagelev

'--formally add hero
hero(slot) = who

'---MUST SET DEFAULT EQUIP---
FOR i as integer = 0 TO 4
 eqstuf(slot, i) = 0
NEXT i
eqstuf(slot, 0) = her.def_weapon + 1

'--fill in stats
WITH gam.hero(slot).stat
 FOR statnum as integer = 0 TO statLast
  .base.sta(statnum) = atlevel(her.def_level, her.Lev0.sta(statnum), her.LevMax.sta(statnum))
 NEXT
 recompute_hero_max_stats slot
 FOR statnum as integer = 0 TO statLast
  .cur.sta(statnum) = .max.sta(statnum)
 NEXT
END WITH

'--weapon picture and palette
DIM wbuf(dimbinsize(binITM)) as integer
loaditemdata wbuf(), her.def_weapon
gam.hero(slot).wep_pic = wbuf(52)
gam.hero(slot).wep_pal = wbuf(53)

'--put spells in spell list
FOR i as integer = 0 TO 3
 FOR o as integer = 0 TO 23
  spell(slot, i, o) = 0
  IF her.spell_lists(i,o).attack > 0 AND her.spell_lists(i,o).learned - 1 <= her.def_level AND her.spell_lists(i,o).learned > 0 THEN spell(slot, i, o) = her.spell_lists(i,o).attack
 NEXT o
NEXT i

'--damage from elements
FOR i as integer = 0 TO gen(genNumElements) - 1
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
FOR i as integer = slot * 6 TO slot * 6 + 5
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
 .def_wep = her.def_weapon + 1 'default weapon
 FOR i as integer = 0 to 1
  .hand_pos(i).x = her.hand_pos(i).x
  .hand_pos(i).y = her.hand_pos(i).y
 NEXT i
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

FUNCTION averagelev () as integer
 DIM average as integer = 0
 DIM count as integer = 0
 FOR i as integer = 0 TO 3
  IF hero(i) > 0 THEN average += gam.hero(i).lev: count += 1
 NEXT i
 IF count > 0 THEN average = average / count
 RETURN average
END FUNCTION

SUB calibrate

DIM state as integer = 0
DIM state_str as string = "Center Joystick and Press Button"
DIM midx as integer = 0
DIM midy as integer = 0
DIM button as integer = 0
DIM disabled as integer = 10
DIM tog as integer
DIM tx as integer
DIM ty as integer

setkeys
DO
 setwait speedcontrol
 setkeys
 tog = tog XOR 1
 IF keyval(scEsc) > 1 THEN EXIT DO
 FOR i as integer = 0 TO 1
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
 'button = IIF(keyval(scB) AND 4, 2, 0)  'simulate joystick button with B
 disabled = disabled - SGN(disabled)
 SELECT CASE state
  CASE 0
   IF (button = 2) AND (disabled = 0) THEN
    midx = joy(0)
    midy = joy(1)
    state_str = "Push UP and Press Button"
    tx = 160
    ty = 45
    state = 1
   END IF
  CASE 1
   IF button = 2 THEN
    joy(9) = joy(1) + (midy - joy(1)) * .33
    state_str = "Push DOWN and Press Button"
    ty = 155
    state = 2
   END IF
  CASE 2
   IF button = 2 THEN
    joy(10) = joy(1) - (joy(1) - midy) * .33
    state_str = "Push LEFT and Press Button"
    tx = 50
    ty = 110
    state = 3
   END IF
  CASE 3
   IF button = 2 THEN
    joy(11) = joy(0) + (midx - joy(0)) * .33
    state_str = "Push RIGHT and Press Button"
    tx = 260
    state = 4
   END IF
  CASE 4
   IF button = 2 THEN
    joy(12) = joy(0) - (joy(0) - midx) * .33
    state_str = "Press the USE button"
    state = 5
   END IF
  CASE 5
   IF button = 2 THEN
    disabled = 4
    state_str = ""
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
 edgeprint state_str, 160 - 4 * LEN(state_str), 174, uilook(uiSelectedItem + tog), dpage
 edgeprint "X=" & joy(0) & " Y=" & joy(1), 160 - 4 * LEN("X=" & joy(0) & " Y=" & joy(1)), 184, uilook(uiSelectedItem + tog), dpage
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP

END SUB

FUNCTION consumeitem (byval invslot as integer) as bool
 '--Subtracts one of an item at a location. If the item is depleted, returns true. If there are some of the item left, it returns false
 '--Argument is the inventory slot index, not the item ID
 consumeitem = 0
 inventory(invslot).num -= 1
 IF inventory(invslot).num <= 0 THEN
  inventory(invslot).used = NO
  consumeitem = -1
 END IF
 update_inventory_caption invslot
END FUNCTION

FUNCTION countitem (byval item_id as integer) as integer
 DIM total as integer = 0
 FOR o as integer = 0 TO last_inv_slot()
  IF inventory(o).used AND item_id = inventory(o).id THEN
   total += inventory(o).num
  END IF
 NEXT o
 RETURN total
END FUNCTION

SUB getitem (byval item_id as integer, byval num as integer=1)

 DIM numitems as integer = num
 DIM room as integer
 DIM stacksize as integer = get_item_stack_size(item_id)

 FOR i as integer = 0 TO last_inv_slot()
  ' Loop through all inventory slots looking for a slot that already
  ' contains the item we are adding. If found increment that slot
  room = stacksize - inventory(i).num
  IF inventory(i).used AND item_id = inventory(i).id AND room > 0 THEN
   IF room < numitems THEN
    inventory(i).num = stacksize
    update_inventory_caption i
    numitems -= room
   ELSE
    inventory(i).num += numitems
    update_inventory_caption i
    EXIT SUB
   END IF
  END IF
 NEXT i
 
 FOR i as integer = 0 TO last_inv_slot()
  'loop through each inventory slot looking for an empty slot to populate 
  IF inventory(i).used = 0 THEN
   inventory(i).used = -1
   inventory(i).id = item_id
   inventory(i).num = small(numitems, stacksize)
   numitems -= inventory(i).num
   update_inventory_caption i
   IF numitems = 0 THEN EXIT SUB
  END IF
 NEXT
 
 'No slot was found to put this item into!
 inventory_overflow_handler item_id, numitems
 
END SUB

SUB inventory_overflow_handler(byval item_id as integer, byval numitems as integer)
 debug "Didn't have room for " & readitemname(item_id) & "x" & numitems
END SUB

FUNCTION room_for_item (byval item_id as integer, byval num as integer = 1) as bool
 DIM room as integer
 DIM stacksize as integer = get_item_stack_size(item_id)

 FOR i as integer = 0 TO last_inv_slot()
  ' Loop through all inventory slots looking for a slot that already contains the item
  room = stacksize - inventory(i).num
  IF inventory(i).used AND item_id = inventory(i).id AND room > 0 THEN
   IF room >= num THEN
    RETURN YES
   END IF
   num -= room
  END IF
 NEXT
 FOR i as integer = 0 TO last_inv_slot()
  'loop through each inventory slot looking for an empty slot to populate 
  IF inventory(i).used = NO THEN
   IF num <= stacksize THEN
    RETURN YES
   END IF
   num -= stacksize
  END IF
 NEXT
 RETURN NO
END FUNCTION

SUB delitem (byval item_id as integer, byval amount as integer=1)
 FOR o as integer = 0 TO last_inv_slot()
  IF inventory(o).used AND item_id = inventory(o).id THEN
   IF inventory(o).num <= amount THEN
    amount -= inventory(o).num
    inventory(o).num = 0
    inventory(o).id = 0
    inventory(o).used = NO
   ELSE
    inventory(o).num -= amount
    amount = 0
   END IF
   update_inventory_caption o
   IF amount = 0 THEN EXIT FOR
  END IF
 NEXT o
END SUB

SUB doswap (byval s as integer, byval d as integer)

'---swap hmask (bitsets which tell which heros are locked)
DIM a as integer = readbit(hmask(), 0, d)
setbit hmask(), 0, d, readbit(hmask(), 0, s)
setbit hmask(), 0, s, a

'---Hero index
SWAP hero(s), hero(d)

'---Spell lists
FOR i as integer = 0 TO 3
 FOR o as integer = 0 TO 23
  SWAP spell(s, i, o), spell(d, i, o)
 NEXT o
NEXT i

'---hero state and stats
SWAP gam.hero(s), gam.hero(d)

'---Level-MP
FOR i as integer = 0 TO 7
 SWAP lmp(s, i), lmp(d, i)
NEXT i

'--Learnt spells flags
FOR i as integer = 0 TO 5
 SWAP learnmask(s * 6 + i), learnmask(d * 6 + i)
NEXT

'--name
SWAP names(s), names(d)

'---Equipment
FOR i as integer = 0 TO 4
 SWAP eqstuf(s, i), eqstuf(d, i)
NEXT i

'--set tags, reload hero pictures and palettes, etc
party_change_updates

'hero(40), spell(40,3,23), lmp(40,7), names(40), eqstuf(40,4)
END SUB

SUB update_textbox ()
STATIC tog as integer
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
  DIM dat as TextSliceData Ptr
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
 DIM choice_sl(1) as Slice Ptr
 choice_sl(0) = LookupSlice(SL_TEXTBOX_CHOICE0)
 choice_sl(1) = LookupSlice(SL_TEXTBOX_CHOICE1)
 DIM col as integer
 IF choice_sl(0) <> 0 AND choice_sl(1) <> 0 THEN
  FOR i as integer = 0 TO 1
   col = uilook(uiMenuItem)
   IF txt.choicestate.pt = i THEN col = uilook(uiSelectedItem + tog)
   ChangeTextSlice choice_sl(i), ,col
  NEXT i
 END IF
END IF
END SUB

SUB evalherotags ()
 DIM as integer id
 DIM leaderid as integer = herobyrank(0)

 FOR i as integer = 0 TO small(gen(genMaxHero), UBOUND(herotags, 1)) '--for each available hero
  'unset all tags, including ones used on heroes not in the party 
  settag herotags(i).have_tag, NO
  settag herotags(i).alive_tag, NO
  settag herotags(i).leader_tag, NO
  settag herotags(i).active_tag, NO
 NEXT i
 'scan party
 FOR i as integer = 0 TO UBOUND(hero)
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
 DIM as integer id

 FOR i as integer = 0 TO maxMaxItems
  'clear all four tags
  settag itemtags(i).have_tag, NO
  settag itemtags(i).in_inventory_tag, NO
  settag itemtags(i).is_equipped_tag, NO
  settag itemtags(i).is_actively_equipped_tag, NO
 NEXT i

 'search inventory slots
 FOR j as integer = 0 TO last_inv_slot()
  'get item ID
  id = inventory(j).id
  IF inventory(j).used THEN 'there is an item in this slot
   settag itemtags(id).have_tag, YES
   settag itemtags(id).in_inventory_tag, YES
  END IF
 NEXT j

 FOR j as integer = 0 TO 40 'search hero list
  FOR k as integer = 0 TO 4 'search equipment slots
   id = eqstuf(j, k) - 1
   IF id >= 0 THEN ' there is an item equipped in this slot
    settag itemtags(id).have_tag, YES
    settag itemtags(id).is_equipped_tag, YES
    IF j < 4 THEN settag itemtags(id).is_actively_equipped_tag, YES
   END IF
  NEXT k
 NEXT j
END SUB

FUNCTION findhero (byval who as integer, byval first as integer, byval last as integer, byval direction as integer) as integer
 FOR i as integer = first TO last STEP direction
  IF hero(i) = who ORELSE (who = -1 ANDALSO hero(i)) THEN
   RETURN i
  END IF
 NEXT i
 RETURN -1 'not found
END FUNCTION

SUB hero_swap_menu (byval iAll as integer)
'--Preserve background for display beneath the hero swapper
DIM page as integer
DIM holdscreen as integer
page = compatpage
holdscreen = allocatepage
copypage page, holdscreen

DIM swindex(40) as integer
DIM swname(40) as string

DIM info as string
DIM swapme as integer = -1
DIM ecsr as integer = -1
DIM acsr as integer = 0
DIM top as integer = 0
DIM la as integer = -1
DIM numhero as integer = 0
DIM high as integer = 0
DIM wide as integer = 0
DIM tog as integer
DIM swap1 as integer
DIM swap2 as integer
DIM cater_slot as integer
DIM menu_color as integer

GOSUB resetswap

IF hero(acsr) THEN info = names(acsr) ELSE info = ""

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
  IF hero(acsr) AND ecsr < 0 THEN info = names(acsr) ELSE info = ""
 END IF
 IF carray(ccRight) > 1 AND ecsr < 0 THEN
  MenuSound gen(genCursorSFX)
  acsr = loopvar(acsr, 0, 3, 1)
  IF hero(acsr) AND ecsr < 0 THEN info = names(acsr) ELSE info = ""
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
     swap1 = acsr
    ELSE
     swap1 = swindex(ecsr)
    END IF
    '---IDENTIFY SOURCE---
    IF swapme < 4 THEN
     swap2 = swapme
    ELSE
     swap2 = swindex(swapme - 4)
    END IF
    doswap swap1, swap2
    swapme = -1
    GOSUB resetswap
    EXIT DO
   LOOP '--this loop just exists for convenient breaking with EXIT DO
  END IF
  EXIT DO
  LOOP '--this loop just exists for convenient breaking with EXIT DO
 END IF

 copypage holdscreen, page
 GOSUB showswapmenu
 setvispage vpage
 dowait
LOOP
carray(4) = 0
carray(5) = 0
MenuSound gen(genCancelSFX)
freepage page
freepage holdscreen

party_change_updates
EXIT SUB

refreshemenu:
IF ecsr < top THEN top = large(ecsr, 0)
IF ecsr > top + 7 THEN top = ecsr - 7
IF hero(acsr) AND ecsr < 0 THEN info = names(acsr) ELSE info = ""
RETRACE

'---DRAWS SWAP MENU AND CURRENT SELECTION----
showswapmenu:
centerbox 160, 66, 130, 38, 1, page
cater_slot = 0
FOR i as integer = 0 TO 3
 IF i = swapme OR hero(i) > 0 THEN rectangle 105 + (30 * i), 60, 20, 20, uilook(uiTextBox), page
 IF hero(i) THEN
  set_walkabout_frame herow(cater_slot).sl, dirDown, 0
  DrawSliceAt LookupSlice(SL_WALKABOUT_SPRITE_COMPONENT, herow(cater_slot).sl), 105 + i * 30, 60 + (i = swapme) * 6, 20, 20, page, YES
  cater_slot += 1
 END IF
NEXT i
IF ecsr < 0 THEN edgeprint CHR(24), 111 + 30 * acsr, 52, uilook(uiSelectedItem + tog), page
IF iAll THEN
 centerbox 160, 100 + small(high, 8) * 5, wide * 8 + 16, small(high, 8) * 10 + 10, 1, page
 FOR i as integer = top TO small(top + 7, la)
  'Some of the colours are a bit bizarre, here, especially the time bar stuff below
  menu_color = uilook(uiMenuItem)
  IF swapme = i + 4 THEN menu_color = uilook(uiSelectedDisabled) '6
  IF ecsr = i THEN
   menu_color = uilook(uiSelectedItem + tog)
   IF swapme = i + 4 THEN menu_color = uilook(uiSelectedDisabled + tog) '6 + 8 * tog
  END IF
  IF swapme > -1 AND swapme < 4 THEN
   IF (numhero < 2 AND i = la) OR readbit(hmask(), 0, acsr) THEN menu_color = uilook(uiTimeBar + ((ecsr = i) * tog)) '8 + ((ecsr = i) * tog)
  END IF
  edgeprint swname(i), xstring(swname(i), 160), 100 + (i - top) * 10, menu_color, page
 NEXT i
END IF
IF LEN(info) THEN
 centerbox 160, 44, (LEN(info) + 2) * 8, 14, 1, page
 edgeprint info, xstring(info, 160), 39, uilook(uiText), page
END IF
RETRACE

'---MAPS OUT ONLY VALID SWAPABLE HEROS PLUS A BLANK-----
resetswap:
la = -1
wide = 0
FOR i as integer = 4 TO 40
 IF readbit(hmask(), 0, i) = 0 AND hero(i) THEN
  la = la + 1
  swindex(la) = i
  swname(la) = names(i)
  wide = large(wide, LEN(swname(la)))
 END IF
NEXT i
la = la + 1
FOR i as integer = 40 TO 4 STEP -1
 IF hero(i) = 0 THEN
  swindex(la) = i
  swname(la) = readglobalstring(48, "-REMOVE-", 10)
  wide = large(wide, 7)
 END IF
NEXT i
high = small(8, la + 1)
numhero = herocount()
IF hero(acsr) AND ecsr < 0 THEN info = names(acsr) ELSE info = ""
RETRACE
END SUB

'Either pass a tag number and specify YES/NO, or pass just a tag number; +ve/-ve indicates value
SUB settag (byval tagnum as integer, byval value as integer = 4444)
 IF ABS(tagnum) <= max_tag() THEN
  settag tag(), tagnum, value
 ELSE
  settag onetime(), tagnum - (max_tag()+1) * SGN(tagnum), value
 END IF
END SUB

FUNCTION istag (byval num as integer, byval zero as integer=NO) as integer
 IF ABS(num) <= max_tag() THEN
  RETURN istag(tag(), num, zero)
 ELSE
  RETURN istag(onetime(), num - (max_tag()+1) * SGN(num), zero)
 END IF
END FUNCTION

'Either pass a tag number and specify YES/NO, or pass just a tag number; +ve/-ve indicates value
SUB settag (tagbits() as integer, byval tagnum as integer, byval value as integer = 4444)
 IF value <> 4444 THEN
  IF ABS(tagnum) > 1 THEN setbit tagbits(), 0, ABS(tagnum), value
 ELSEIF tagnum < -1 THEN
  setbit tagbits(), 0, ABS(tagnum), NO
 ELSEIF tagnum > 1 THEN
  setbit tagbits(), 0, tagnum, YES
 END IF
END SUB

FUNCTION istag (tagbits() as integer, byval num as integer, byval zero as integer=NO) as integer
 IF num = 0 THEN RETURN zero 'why go through all that just to return defaults?
 IF num = 1 THEN RETURN 0
 IF num = -1 THEN RETURN -1
 IF ABS(num) >= UBOUND(tagbits) * 16 + 16 THEN RETURN zero ' use default in case of an invalid tag

 DIM ret as integer = readbit(tagbits(), 0, ABS(num)) 'raw bit: 0 or -1

 IF num > 0 AND ret <> 0 THEN RETURN -1
 IF num < 0 AND ret = 0 THEN RETURN -1
 RETURN 0
END FUNCTION

SUB loaddoor (byval map as integer)
IF gen(genVersion) < 2 THEN
 '--obsolete doors
ELSE
 '--THE RIGHT WAY--
 DeSerDoors(game + ".dox", gam.map.door(), map)
END IF
END SUB

SUB minimap (byval x as integer, byval y as integer)
 DIM mini as Frame Ptr
 DIM zoom as integer = -1
 mini = createminimap(maptiles(), tilesets(), zoom)

 DIM minisize as XYPair
 minisize.x = mini->w
 minisize.y = mini->h
 
 DIM offset as XYPair
 offset.x = vpages(vpage)->w / 2 - minisize.x / 2
 offset.y = vpages(vpage)->h / 2 - minisize.y / 2

 edgeboxstyle offset.x - 2, offset.y - 2, minisize.x + 4, minisize.y + 4, 0, vpage
 frame_draw mini, NULL, offset.x, offset.y, 1, NO, vpage
 frame_unload @mini

 MenuSound gen(genAcceptSFX)

 DIM tog as integer
 DIM i as integer
 setkeys
 DO
  setwait speedcontrol
  setkeys
  tog = tog XOR 1
  playtimer
  control
  IF anykeypressed THEN EXIT DO
  rectangle offset.x + (x / 20) * zoom, offset.y + (y / 20) * zoom, zoom, zoom, uilook(uiSelectedItem) * tog, vpage
  setvispage vpage
  dowait
 LOOP
 setkeys
 flusharray carray(), 7, 0
 MenuSound gen(genCancelSFX)
END SUB

FUNCTION teleporttool () as integer
 REDIM maptiles2(0) as TileMap
 DIM tilesets2(maplayerMax) as TilesetData ptr
 DIM mini as Frame Ptr
 DIM zoom as integer
 DIM i as integer

 DIM mapsize as XYPair
 DIM minisize as XYPair 'pixels
 DIM offset as XYPair

 'Notice that we initially use the real map's maptiles(), rather than reloading them into maptiles2(),
 'but we DO reload the tilesets (they are cached anyway)
 teleporttooltend mini, maptiles(), tilesets2(), zoom, -1, mapsize, minisize, offset

 DIM dest as XYPair
 dest.x = catx(0) \ 20
 dest.y = caty(0) \ 20

 DIM camera as XYPair 'in pixels
 camera.x = bound(dest.x * zoom - minisize.x \ 2, 0, mapsize.x * zoom - minisize.x)
 camera.y = bound(dest.y * zoom - minisize.y \ 2, 0, mapsize.y * zoom - minisize.y)

 DIM menu(1) as STRING
 menu(0) = "Teleport to map... " & gam.map.id & " " & getmapname(gam.map.id)
 menu(1) = "Teleport to position... X = " & dest.x & " Y = " & dest.y

 DIM state as MenuState
 state.pt = 1
 state.top = 0
 state.size = 22
 state.first = 0
 state.last = UBOUND(menu)
 DIM menuopts as MenuOptions
 menuopts.edged = YES

 DIM preview_delay as integer = 0
 DIM pickpoint as integer = NO
 DIM destmap as integer = gam.map.id
 DIM xrate as integer
 DIM yrate as integer


 teleporttool = 0

 copypage vpage, 3
 GOSUB redraw

 MenuSound gen(genAcceptSFX)
 setkeys
 DO
  setwait speedcontrol
  setkeys

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
     menu(0) = "Map " & destmap & " " & getmapname(destmap)
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

   IF slowkey(scUp, 45) THEN dest.y = large(dest.y - yrate, 0)
   IF slowkey(scDown, 45) THEN dest.y = small(dest.y + yrate, mapsize.y - 1)
   IF slowkey(scLeft, 45) THEN dest.x = large(dest.x - xrate, 0)
   IF slowkey(scRight, 45) THEN dest.x = small(dest.x + xrate, mapsize.x - 1)

   DIM temp as XYPair = camera
   camera.x = bound(camera.x, (dest.x + 1) * zoom + 40 - minisize.x, dest.x * zoom - 40)  'follow dest
   camera.y = bound(camera.y, (dest.y + 1) * zoom + 30 - minisize.y, dest.y * zoom - 30)
   camera.x = bound(camera.x, 0, mapsize.x * zoom - minisize.x)  'bound to map edges
   camera.y = bound(camera.y, 0, mapsize.y * zoom - minisize.y)
   IF temp.x <> camera.x OR temp.y <> camera.y THEN GOSUB redraw

   menu(1) = "Position X = " & dest.x & " Y = " & dest.y
  END IF

  copypage vpage, dpage
  rectangle offset.x + dest.x * zoom - camera.x, offset.y + dest.y * zoom - camera.y, zoom, zoom, uilook(uiSelectedItem) * state.tog, dpage
  state.active = (pickpoint = NO)
  standardmenu menu(), state, 0, 182, dpage, menuopts
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
SUB teleporttooltend (byref mini as Frame Ptr, maptilesX() as TileMap, tilesets2() as TilesetData ptr, byref zoom as integer, byval map as integer, byref mapsize as XYPair, byref minisize as XYPair, byref offset as XYPair)
 IF map > -1 THEN
  DIM gmap2(dimbinsize(binMAP)) as integer
  loadrecord gmap2(), game + ".map", getbinsize(binMAP) \ 2, map
  loadtilemaps maptilesX(), maplumpname(map, "t")
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


FUNCTION movdivis (byval xygo as integer) as integer
IF (xygo \ 20) * 20 = xygo AND xygo <> 0 THEN
 movdivis = -1
ELSE
 movdivis = 0
END IF
END FUNCTION

FUNCTION onwho (caption as string, byval alone as integer) as integer

'-- pre-select the first hero
DIM w as integer = rank_to_party_slot(0)

'-- if there is only one hero, return immediately
'--unless we are in alone-mode
IF alone = 0 AND herocount() <= 1 THEN onwho = w: setkeys: EXIT FUNCTION

menusound gen(genAcceptSFX)
copypage vpage, dpage
DIM page as integer = compatpage
DIM tog as integer
DIM wtg as integer
DIM wt as integer
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
 DIM cater_slot as integer = 0
 FOR party_slot as integer = 0 TO 3
  IF hero(party_slot) > 0 THEN
   IF w = party_slot THEN wt = wtg \ 2 ELSE wt = 0
   set_walkabout_frame herow(cater_slot).sl, dirDown, wt
   DrawSliceAt LookupSlice(SL_WALKABOUT_SPRITE_COMPONENT, herow(cater_slot).sl), 100 + party_slot * 30, 100, 20, 20, page, YES
   cater_slot += 1
  END IF
 NEXT
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

IF isfile(exepath & SLASH & "joyset.ini") THEN
 '--use joyset.ini
 DIM fh as integer = FREEFILE
 OPEN exepath & SLASH & "joyset.ini" FOR INPUT as #fh
 DIM safety as integer = 0
 DIM n as string = ""
 DIM a as string
 DO WHILE NOT EOF(fh) AND safety < 100
  LINE INPUT #fh, a
  IF settingstring(a, "UPTHRESH", n) THEN
   joy(9) = str2int(n)
  END IF
  IF settingstring(a, "DOWNTHRESH", n) THEN
   joy(10) = str2int(n)
  END IF
  IF settingstring(a, "LEFTTHRESH", n) THEN
   joy(11) = str2int(n)
  END IF
  IF settingstring(a, "RIGHTTHRESH", n) THEN
   joy(12) = str2int(n)
  END IF
  IF settingstring(a, "USEBUTTON", n) THEN
   joy(13) = bound(str2int(n) + 2, 2, 3)
  END IF
  IF settingstring(a, "MENUBUTTON", n) THEN
   joy(14) = bound(str2int(n) + 2, 2, 3)
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

DIM her as herodef
loadherodata her, hero(who) - 1

DIM limit as integer = her.max_name_len
IF limit = 0 THEN limit = 16

DIM prompt as string = readglobalstring(137, "Name the Hero", 20)
DIM spacer as string = STRING(large(limit, LEN(names(who))), " ")
DIM remember as string = names(who)

DIM need_fade_out as integer
DIM page as integer
page = compatpage
IF fadestate = 0 THEN
 setvispage vpage
 fadein
 need_fade_out = YES
END IF

show_virtual_keyboard

setkeys YES
DO
 setwait speedcontrol
 setkeys YES
 playtimer
 control
 centerbox 160, 100, 168, 32, 1, page
 IF carray(ccUse) > 1 AND keyval(scSpace) = 0 THEN EXIT DO
 IF carray(ccMenu) > 1 THEN
  names(who) = remember
  IF escapable THEN EXIT DO
 END IF
 strgrabber names(who), limit
 edgeprint prompt, xstring(prompt, 160), 90, uilook(uiText), page
 textcolor uilook(uiHighlight), uiLook(uiHighlight)
 printstr spacer, xstring(spacer, 161), 101, page
 edgeprint names(who), xstring(names(who), 160), 100, uilook(uiMenuItem), page

 setvispage vpage
 dowait
LOOP
menusound gen(genAcceptSFX)

hide_virtual_keyboard

IF need_fade_out THEN
 fadeout 0, 0, 0
END IF
freepage page

END SUB

SUB resetgame (scriptout as string)
gam.map.id = 0
catx(0) = 0
caty(0) = 0
catz(0) = 0
catd(0) = 0
gam.getinputtext_enabled = NO
gam.script_log.tick = 0
gam.script_log.wait_msg_repeats = 0
'leader = 0
mapx = 0
mapy = 0
gold = 0
scriptout = ""
'--return gen to defaults
xbload game + ".gen", gen(), "General data is missing from " + sourcerpg

CleanNPCL npc()
flusharray tag()
flusharray onetime()
flusharray hero(), 40, 0
FOR i as integer = 0 TO 40
 FOR j as integer = 0 TO statLast
  gam.hero(i).stat.cur.sta(j) = 0
  gam.hero(i).stat.max.sta(j) = 0
  gam.hero(i).stat.base.sta(j) = 0
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
 FOR j as integer = 0 TO maxElements - 1
  gam.hero(i).elementals(j) = 1.0f
 NEXT
 FOR j as integer = 0 to 1
  gam.hero(i).hand_pos(j).x = 0
  gam.hero(i).hand_pos(j).y = 0
 NEXT j
NEXT i
FOR i as integer = 0 TO 40
 FOR o as integer = 0 TO 3
  FOR j as integer = 0 TO 23
   spell(i, o, j) = 0
  NEXT j
 NEXT o
NEXT i
FOR i as integer = 0 TO 40
 FOR o as integer = 0 TO 7
  lmp(i, o) = 0
 NEXT o
NEXT i
FOR i as integer = 0 TO 40
 gam.hero(i).exp_cur = 0
 gam.hero(i).exp_next = 0
NEXT i
FOR i as integer = 0 TO 40
 names(i) = ""
NEXT i
CleanInventory(inventory())
FOR i as integer = 0 TO 40
 FOR o as integer = 0 TO 4
  eqstuf(i, o) = 0
 NEXT o
NEXT i

'RECORD 2 (applies only to saves)

FOR i as integer = 0 TO 99
 FOR o as integer = 0 TO 49
  gam.stock(i, o) = 0 'default of 0 means "unloaded". The actual value will be populated when you first visit the shop
 NEXT o
NEXT i
flusharray hmask()
flusharray global(), maxScriptGlobals, 0
reset_vehicle vstate
cleanup_text_box
txt.showing = NO
txt.fully_shown = NO
txt.show_lines = 0

FOR i as integer = 0 TO UBOUND(plotstr)
 WITH plotstr(i)
  .s = ""
  .col = uilook(uiText)
  .BGCol = 0
  .X = 0
  .Y = 0
  .Bits = 0
 END WITH
NEXT i

FOR i as integer = topmenu TO 0 STEP -1
 remove_menu i, NO
NEXT i

'delete temp files that are part of the game state
deletetemps

'doesn't unload scripts: not needed
killallscripts

IF running_as_slave THEN reload_scripts

FOR i as integer = 0 TO ubound(timers)
 WITH timers(i)
  .count = 0
  .speed = 0
  .ticks = 0
  .trigger = 0
  .flags = 0
  .st = 0
  .pause = NO
 END WITH
NEXT i

cleanup_game_slices()
'plotslices() should now be all zeroed out. Rather than checking, check slicedebug() (if enabled)
SliceDebugDump YES
SetupGameSlices
reset_game_state()

END SUB

SUB resetlmp (byval slot as integer, byval lev as integer)
 FOR i as integer = 0 TO 7
  lmp(slot, i) = 0
 NEXT i
 DIM o as integer = 0
 DIM j as integer = 0
 FOR i as integer = 0 TO lev
  lmp(slot, o) = lmp(slot, o) + 1
  o = o + 1
  IF o > j THEN o = 0: j = j + 1
  IF j > 7 THEN j = 0
 NEXT i
END SUB

SUB trigger_script (byval id as integer, byval double_trigger_check as bool, scripttype as string, trigger_loc as string, scrqueue() as QueuedScript, byval trigger as integer = plottrigger)
 'Add a script to one of the script queues, unless already inside the interpreter.
 'In that case, run immediately.
 'scrqueue should be one of the scrq* arrays
 'double_trigger_check: whether "no double-triggering" should take effect

 STATIC dummy_queued_script as QueuedScript

 IF insideinterpreter THEN
  DIM rsr as integer
  rsr = runscript(id, YES, double_trigger_check, scripttype, trigger)
  trigger_script_failure = (rsr <> 1)
  IF gam.script_log.enabled = NO THEN EXIT SUB

  'Can't call watched_script_triggered until after the trigger_script_args calls
  scrat(nowscript).watched = YES
  scrat(nowscript).state = sttriggered
  last_queued_script = @dummy_queued_script
 ELSE
  REDIM PRESERVE scrqueue(-1 TO UBOUND(scrqueue) + 1)
  last_queued_script = @scrqueue(UBOUND(scrqueue))
 END IF

 WITH *last_queued_script
  IF trigger <> 0 THEN id = decodetrigger(id)
  .id = id
  .scripttype = scripttype
  .log_line = scriptname(id) & "("
  .trigger_loc = trigger_loc
  .double_trigger_check = double_trigger_check
  .argc = 0
 END WITH
END SUB

SUB trigger_script_arg (byval argno as integer, byval value as integer, byval argname as zstring ptr = NULL)
 'Set one of the args for a script that was just triggered
 'Note that after calling trigger_script, script queuing can be in three states:
 'inside interpreter, trigger_script_failure = NO
 '    triggered a script which started immediately
 'inside interpreter, trigger_script_failure = YES
 '    triggered a script which there was an error starting
 'not inside interpreter:
 '    queued a script, can now set the arguments

 IF insideinterpreter THEN
  IF trigger_script_failure = NO THEN
   setScriptArg argno, value
  END IF
  IF gam.script_log.enabled = NO THEN EXIT SUB
 END IF

 WITH *last_queued_script
  IF argno > UBOUND(.args) THEN fatalerror "trigger_script_arg: args queue overflow"
  .args(argno) = value
  .argc = large(.argc, argno + 1)
  IF gam.script_log.enabled THEN
   IF argno <> 0 THEN .log_line += ", "
   IF argname THEN .log_line += *argname + "="
   .log_line &= value
  END IF
 END WITH
END SUB

PRIVATE SUB run_queued_script (script as QueuedScript)
 DIM rsr as integer
 rsr = runscript(script.id, YES, script.double_trigger_check, script.scripttype, 0)
 IF rsr = 1 THEN
  FOR argno as integer = 0 TO script.argc - 1
   setScriptArg argno, script.args(argno)
  NEXT
 END IF

 IF gam.script_log.enabled THEN watched_script_triggered script
END SUB

SUB run_queued_scripts
 'Load the queued scripts into the interpreter.
 'We have to call runscript in the reverse order, because we build the stack up from bottom

 FOR i as integer = UBOUND(scrqFirst) TO 0 STEP -1
  run_queued_script(scrqFirst(i))
 NEXT
 FOR i as integer = 0 TO UBOUND(scrqBackcompat)
  run_queued_script(scrqBackcompat(i))
 NEXT
 FOR i as integer = UBOUND(scrqLast) TO 0 STEP -1
  run_queued_script(scrqLast(i))
 NEXT

 dequeue_scripts
END SUB

SUB dequeue_scripts
 'Wipe the script queues
 last_queued_script = NULL
 REDIM scrqFirst(-1 TO -1)
 REDIM scrqBackcompat(-1 TO -1)
 REDIM scrqLast(-1 TO -1)
END SUB

SUB start_script_trigger_log
 gam.script_log.enabled = YES
 safekill gam.script_log.filename
 DIM fh as integer = FREEFILE
 IF OPEN(gam.script_log.filename FOR APPEND AS #fh) THEN
  notification "Could not open " & gam.script_log.filename & ". Script logging disabled."
  EXIT SUB
 END IF
 gam.script_log.enabled = YES

 print #fh, "Script trigger log for " & getdisplayname(trimpath(sourcerpg)) & ", " & DATE & " " & TIME
 print #fh,
 print #fh, "Solid lines '|' show triggered scripts which have already started running but are"
 print #fh, "waiting or paused due to either another script which was triggered (line to the right)"
 print #fh, "or while waiting for a script they called (not shown)."
 print #fh, "Dotted lines ':' show triggered scripts which have no even had a chance to start."
 print #fh,
 print #fh, " Symbols in front of script names:"
 print #fh, "+ -- A script was triggered (queued), possibly also started, possibly also finished" 
 print #fh, "! -- As above, but triggered as a side effect of something the script above it did,"
 print #fh, "     such as running ""close menu"", interrupting that script."
 print #fh, "     (Note: ! is used only if the command didn't cause an implicit 'wait')"
 print #fh, "* -- A queued script was started, possibly also finished" 
 print #fh, "- -- A previously started script finished"
 print #fh,
 CLOSE #fh
END SUB

SUB script_log_out (text as string)
 IF gam.script_log.enabled = NO THEN EXIT SUB
 DIM fh as integer = FREEFILE
 IF OPEN(gam.script_log.filename FOR APPEND AS #fh) THEN
  gam.script_log.enabled = NO
  EXIT SUB
 END IF
 #IFDEF __FB_WIN32__
  'FB opens files in binary mode...
  replacestr text, !"\n", !"\r\n"
 #ENDIF

 print #fh, text;
 CLOSE #fh
 gam.script_log.output_flag = YES
END SUB

FUNCTION script_log_indent (byval upto as integer = -1, byval spaces as integer = 11) as string
 DIM indent as string = SPACE(spaces)
 IF upto = -1 THEN upto = nowscript - 1
 FOR i as integer = 0 TO upto
  WITH scrat(i)
   IF .watched THEN
    IF .started THEN
     indent &= "| "
    ELSE
     indent &= ": "
    END IF
   END IF
  END WITH
 NEXT
 RETURN indent
END FUNCTION

'Called after runscript when running a script which should be watched
SUB watched_script_triggered(script as QueuedScript)
 scrat(nowscript).watched = YES
 IF gam.script_log.last_logged > -1 ANDALSO scrat(gam.script_log.last_logged).started = NO THEN
  script_log_out " (queued)"
 END IF

 DIM logline as string
 logline = !"\n" & script_log_indent()
 IF insideinterpreter THEN
  IF nowscript >= 1 ANDALSO scrat(nowscript - 1).state < 0 THEN
   'The previous script was suspended, therefore this script was triggered as
   'a side effect of something that script did, such as activate an NPC
   logline &= "!"
  ELSE
   'Called normally
   logline &= "\"
  END IF
 ELSE
  'Triggered normally
  logline &= "+"
 END IF

 logline &= script.log_line & ") " & script.scripttype & " script"
 IF LEN(script.trigger_loc) THEN
  logline &= ", " & script.trigger_loc
 END IF
 script_log_out logline

 gam.script_log.last_logged = nowscript

END SUB

'nowscript has been started and resumed and has .watched = YES
SUB watched_script_resumed
 IF gam.script_log.last_logged = nowscript THEN
  'nothing
 ELSEIF scrat(nowscript).started THEN
  'also nothing
 ELSE
  script_log_out !"\n" & script_log_indent() & "*" & scriptname(scrat(nowscript).id) & " started"
  gam.script_log.last_logged = nowscript
 END IF
 scrat(nowscript).started = YES
END SUB

'Called right before the current script terminates and has .watched = YES
SUB watched_script_finished
 DIM logline as string
 IF gam.script_log.last_logged = nowscript THEN
  script_log_out " ... finished"
 ELSE
  script_log_out !"\n" & script_log_indent() & "-" & scriptname(scrat(nowscript).id) & " finished"
 END IF

 gam.script_log.last_logged = -1
END SUB

'Call each tick if script logging is enabled
SUB script_log_tick
 WITH gam.script_log
  DIM doprint as integer = NO
  IF .output_flag THEN doprint = YES

  DIM wait_msg as string = ""
  IF nowscript > -1 THEN
   wait_msg = "waiting on " & commandname(scrat(nowscript).curvalue) & " in " & scriptname(scrat(nowscript).id)
   IF .last_wait_msg <> wait_msg THEN
    .last_wait_msg = wait_msg
    .wait_msg_repeats = 0
   END If
   .wait_msg_repeats += 1
   IF .wait_msg_repeats <= 3 THEN doprint = YES
   IF .wait_msg_repeats = 3 THEN wait_msg = "..."
  END IF

  IF doprint THEN
 '  script_log_out !"\n" & script_log_indent(nowscript) & "   <<tick " & .tick & ">>"
   DIM logline as string
   logline =  !"\ntick " & LEFT(RIGHT(STR(.tick), 5) & "     ", 6) & script_log_indent(nowscript, 0)
   IF LEN(wait_msg) THEN logline &= "     (" & wait_msg & ")"
   script_log_out logline
   .output_flag = NO

   .last_logged = -1
  END IF
 END WITH
END SUB

FUNCTION runscript (byval id as integer, byval newcall as integer, byval double_trigger_check as bool, byval scripttype as zstring ptr, byval trigger as integer) as integer
'newcall: whether his script is triggered rather than called from a script
'double_trigger_check: whether "no double-triggering" should take effect

DIM n as integer
IF trigger <> 0 THEN n = decodetrigger(id) ELSE n = id

IF n = 0 THEN
 runscript = 2 '--quiet failure (though decodetrigger might have shown a scripterr)
 EXIT FUNCTION
END IF

DIM index as integer = nowscript + 1

IF index >= maxScriptRunning THEN
 runscript = 0 '--error
 scripterr "failed to load " + *scripttype + " script " & n & " " & scriptname(n) & ", interpreter overloaded", 6
 EXIT FUNCTION
END IF

IF double_trigger_check AND index > 0 THEN
 IF n = scrat(index - 1).id AND readbit(gen(), genBits, 10) = 0 THEN
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
  scripterr "Failed to load " + *scripttype + " script " & n & " " & scriptname(n), 6
  EXIT FUNCTION
 END IF
 .scr->totaluse += 1
 scriptctr += 1
 .scr->lastuse = scriptctr
 'increment refcount once loading is successful

 'erase state, pointer, return value and depth, set id
 .state = ststart
 .ptr = 0
 .ret = 0
 .depth = 0
 .id = n
 .stackbase = -1
 .scrdata = .scr->ptr
 .watched = NO
 .started = NO
 .curargn = 0
 curcmd = cast(ScriptCommand ptr, .scrdata + .ptr) 'just in case it's needed before subread is run
 
 scrat(index + 1).heap = .heap + .scr->vars

 IF scrat(index + 1).heap > maxScriptHeap THEN
  runscript = 0'--error
  scripterr "failed to load " + *scripttype + " script " & n & " " & scriptname(n) & ", script heap overflow", 6
  EXIT FUNCTION
 END IF

 FOR i as integer = 0 TO .scr->vars - 1
  heap(.heap + i) = 0
 NEXT i

 '--suspend the previous script
 IF newcall AND index > 0 THEN
  scrat(index - 1).state *= -1
 END IF

 '--we are successful, so now its safe to increment this
 .scr->refcount += 1
 nowscript += 1

 'debug scriptname(.id) & " in script(" & (.id MOD scriptTableSize) & "): totaluse = " & .scr->totaluse & " refc = " & .scr->refcount & " lastuse = " & .scr->lastuse
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

FUNCTION loadscript (byval n as unsigned integer) as ScriptData ptr
 '-- script() is a hashtable with doubly linked lists as buckets, storing the loaded scripts

 DIM as ScriptData Ptr scrnode = script(n MOD scriptTableSize)
 WHILE scrnode
  IF scrnode->id = n THEN RETURN scrnode
  scrnode = scrnode->next
 WEND

 DIM thisscr as ScriptData ptr
 DIM shortvar as short

 '--load the script from file
 DIM scriptfile as string = tmpdir & n & ".hsz"
 IF NOT isfile(scriptfile) THEN
  scriptfile = tmpdir & n & ".hsx"
  IF NOT isfile(scriptfile) THEN
   '--because TMC once suggested that preunlumping the .hsp lump would be a good way to reduce (SoJ) loading time
   scriptfile = workingdir & SLASH & n & ".hsx"
   IF NOT isfile(scriptfile) THEN
    scripterr "script " & n & " " & scriptname(n) & " does not exist", 6
    RETURN NULL
   END IF
  END IF
 END IF

 DIM f as integer = FREEFILE
 OPEN scriptfile FOR BINARY as #f

 'minimum length of a valid 16-bit .hsx
 IF LOF(f) < 10 THEN
  scripterr "script " & n & " corrupt (too short: " & LOF(f) & " bytes)", 6
  CLOSE #f
  RETURN NULL
 END IF

 thisscr = callocate(sizeof(ScriptData))
 WITH *thisscr

  GET #f, 1, shortvar
  DIM skip as integer = shortvar

  IF skip < 4 THEN
   scripterr "script " & n & " is corrupt (header length " & skip & ")", 6
   CLOSE #f
   deallocate(thisscr)
   RETURN NULL
  END IF

  GET #f, 3, shortvar
  'some HSX files seem to have an illegal negative number of variables
  .vars = shortvar
  .vars = bound(.vars, 0, 256)
 
  IF skip >= 6 THEN
   GET #f, 5, shortvar
   .args = bound(shortvar, 0, .vars)
  ELSE
   .args = 999
  END IF

  DIM scrformat as integer
  IF skip >= 8 THEN
   GET #f, 7, shortvar
   scrformat = shortvar
  ELSE
   scrformat = 0
  END IF
  IF scrformat > CURRENT_HSZ_VERSION THEN
   scripterr "script " & n & " is in an unsupported format", 6
   CLOSE #f
   deallocate(thisscr)
   RETURN NULL
  END IF
  DIM wordsize as integer
  IF scrformat >= 1 THEN wordsize = 4 ELSE wordsize = 2

  IF skip >= 12 THEN
   GET #f, 9, .strtable
   IF .strtable THEN .strtable = (.strtable - skip) \ wordsize
  ELSEIF skip = 10 THEN
   GET #f, 9, shortvar
   IF shortvar THEN .strtable = (shortvar - skip) \ wordsize
  ELSE
   .strtable = 0
  END IF

  'set an arbitrary max script buffer size (scriptmemMax in const.bi), individual scripts must also obey
  .size = (LOF(f) - skip) \ wordsize
  IF .size > scriptmemMax THEN
   scripterr "Script " & n & " " & scriptname(n) & " exceeds maximum size by " & .size * 100 \ scriptmemMax - 99 & "%", 6
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
   FOR i as integer = skip TO LOF(f) - wordsize STEP wordsize
    GET #f, 1 + i, shortvar
    .ptr[(i - skip) \ 2] = shortvar
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

SUB freescripts (byval mem as integer)
'frees loaded scripts until at least totalscrmem <= mem (measured in 4-byte ints) (probably a lot lower)
'also makes sure numloadedscr <= maxLoadedScripts - 24
'call freescripts(0) to cleanup all scripts
'and, as a total hack, print profiling information on scripts if SCRIPTPROFILE is defined, in which
'case the buffers are so large that freescripts is only called when quitting

#IFDEF SCRIPTPROFILE
DIM timeroverhead as double
FOR i as integer = 0 TO 999
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
DIM listtail as integer = -1
DIM score as integer

DIM j as integer
FOR i as integer = 0 TO UBOUND(script)
 DIM scrp as ScriptData Ptr = script(i)
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
FOR i as integer = 0 TO listtail
 entiretime += LRUlist(i).p->totaltime
NEXT

debug "script profiling information:"
debug "#switches is the number of times that the interpreter switched to that script"
debug "(switching time is relatively neglible and included to help determine"
debug "calls to other scripts, which are more expensive)"
debug "Total time recorded in interpreter: " & format(entiretime, "0.000") & "sec   (timer overhead = " & format(timeroverhead*1000000, "0.00") & "us)"
debug " %time        time    time/call      #calls   #switches  script name"
FOR i as integer = 0 TO listtail
' debug i & ": " & LRUlist(i).p & " score = " & LRUlist(i).score
 WITH *LRUlist(i).p
  debug " " & format(100 * .totaltime / entiretime, "00.00") _
      & RIGHT(SPACE(9) & format(.totaltime*1000, "0"), 10) & "ms" _
      & RIGHT(SPACE(10) & format(.totaltime*1000000/.totaluse, "0"), 11) & "us" _
      & RIGHT(SPACE(11) & .totaluse, 12) _
      & RIGHT(SPACE(11) & .entered, 12) _
      & "  " & scriptname(.id) '& "  " & format(1000*(.totaltime + .entered * timeroverhead), "0.00")

'  debug "id = " & .id & " " & scriptname(.id)
'  debug "refcount = " & .refcount
'  debug "totaluse = " & .totaluse
'  debug "lastuse = " & .lastuse
'  debug "size = " & .size
 END WITH
NEXT
#ENDIF 'SCRIPTPROFILE

'aim for at most 75% of memory limit
DIM targetmem as integer = mem
IF mem > scriptmemMax \ 2 THEN targetmem = mem * (1 - 0.5 * (mem - scriptmemMax \ 2) / scriptmemMax)

'debug "requested max mem = " & mem & ", target = " & targetmem

FOR i as integer = 0 TO listtail
 IF totalscrmem <= targetmem AND numloadedscr <= maxLoadedScripts - 24 THEN EXIT SUB
 delete_scriptdata LRUlist(i).p
NEXT

END SUB

FUNCTION commandname (byval id as integer) as string
 'cmd_default_names array
#include "scrcommands.bi"

 STATIC cache(32) as IntStrPair
 DIM as string ret
 ret = search_string_cache(cache(), id, game_unique_id)
 IF ret <> "" THEN RETURN ret
 IF id >= 0 AND id <= UBOUND(cmd_default_names) THEN ret = cmd_default_names(id)
 IF ret = "" THEN ret = "cmd" & id

 DIM as short headersz, formatv, records, offset

 '--could check workingdir as well like we do in runscript; but doesn't seem necessary
 DIM fh as integer = FREEFILE
 IF OPEN(tmpdir + "commands.bin" FOR BINARY ACCESS READ as fh) THEN
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

FUNCTION script_call_chain (byval trim_front as integer = YES) as string
 IF nowscript < 0 THEN
  RETURN "Funny... no scripts running!"
 END IF

 DIM scriptlocation as string
 scriptlocation = scriptname(scrat(nowscript).id)
 FOR i as integer = nowscript - 1 TO 0 STEP -1
  IF scrat(i).state < 0 THEN EXIT FOR 'suspended: not part of the call chain
  scriptlocation = scriptname(scrat(i).id) + " -> " + scriptlocation
 NEXT
 IF trim_front AND LEN(scriptlocation) > 150 THEN scriptlocation = " ..." + RIGHT(scriptlocation, 150)
 RETURN "  Call chain (current script last):" + CHR(10) + scriptlocation
END FUNCTION

'For errorlevel scheme, see scriptErrEnum in const.bi
SUB scripterr (e as string, byval errorlevel as scriptErrEnum = serrBadOp)
 'mechanism to handle scriptwatch throwing errors
 STATIC as integer recursivecall

 'this is the correct way to declare a dynamic static array. Also, you have to REDIM it before use!
 STATIC as integer ignorelist()
 REDIM PRESERVE ignorelist(UBOUND(ignorelist))

 DIM as string errtext()
 DIM as integer scriptcmdhash

 debug "Scripterr(" & errorlevel & "): " + e

 IF errorlevel <= err_suppress_lvl THEN EXIT SUB
 IF nowscript >= 0 THEN
  scriptcmdhash = scrat(nowscript).id * 100000 + scrat(nowscript).ptr * 10 + scrat(nowscript).depth
  IF int_array_find(ignorelist(), scriptcmdhash) <> -1 THEN EXIT SUB
 END IF

 recursivecall += 1

 IF errorlevel = serrError THEN e = "Script data may be corrupt or unsupported:" + CHR(10) + e
 IF errorlevel >= serrBug THEN e = "PLEASE REPORT THIS POSSIBLE ENGINE BUG" + CHR(10) + e

 e = e + CHR(10) + CHR(10) + script_call_chain
 split(wordwrap(e, 38), errtext())

 DIM state as MenuState
 state.pt = 0
 DIM menu as MenuDef
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
  IF running_as_slave THEN append_menu_item menu, "Reload scripts"
 END IF

 state.active = YES
 init_menu_state state, menu

 'Modify master() because the script debugger or other menus may setpal 
 REDIM remember_master(255) as RGBcolor
 FOR i as integer = 0 TO 255
  remember_master(i) = master(i)
 NEXT
 loadpalette master(), gam.current_master_palette
 setpal master()

 setkeys
 DO
  setwait 55
  setkeys

  IF keyval(scEsc) > 1 THEN 'ignore
   EXIT DO 
  END IF

  IF keyval(scF1) > 1 THEN show_help("game_scripterr")

  IF enter_or_space() THEN
   SELECT CASE state.pt
    CASE 0 'ignore
    CASE 1 'hide errors (but not engine bugs)
     err_suppress_lvl = serrError
    CASE 2 'hide some errors
     err_suppress_lvl = errorlevel
    CASE 3 'hide errors from this command
     int_array_append(ignorelist(), scriptcmdhash)
    CASE 4
     debug "scripterr: User opted to quit"
     exitprogram NO
    CASE 5
     scrwatch = 2
     scriptwatcher scrwatch, 0 'clean mode, script state view mode
    CASE 6 'reload scripts
     reload_scripts
   END SELECT
   EXIT DO
  END IF
  
  usemenu state

  clearpage vpage

  centerbox 160, 12, 310, 15, 3, vpage
  textcolor uilook(uiText), 0
  IF errorlevel >= serrBug THEN
   printstr "Impossible error/engine bug!", 160 - 28*4, 7, vpage
  ELSEIF errorlevel >= serrBound THEN
   printstr "Script Error!", 160 - 13*4, 7, vpage
  ELSEIF errorlevel >= serrWarn THEN
   printstr "Script Warning", 160 - 14*4, 7, vpage
  ELSEIF errorlevel = serrInfo THEN
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
    exitprogram NO, 1
  END IF

  dowait
 LOOP
 ClearMenuData menu
 setkeys
 recursivecall -= 1

 FOR i as integer = 0 TO 255
  master(i) = remember_master(i)
 NEXT
 setpal master()
 next_interpreter_check_time = TIMER + scriptCheckDelay

 'Note: when we resume after a script error, the keyboard state changes, which might break a script
 'Not worth worrying about this.
END SUB

FUNCTION script_interrupt () as integer
 DIM as integer ret = NO
 DIM as string errtext()
 DIM as string msg

 msg = "A script may be stuck in an infinite loop. Press F1 for more help" + CHR(10) + CHR(10) + script_call_chain
 debug script_call_chain(NO)
 split(wordwrap(msg, 38), errtext())

 DIM state as MenuState
 state.pt = 0
 DIM menu as MenuDef
 ClearMenuData menu
 menu.anchor.y = -1
 menu.offset.y = -100 + 38 + 10 * UBOUND(errtext) 'menus are always offset from the center of the screen
 menu.bordersize = -4

 append_menu_item menu, "Continue running"
 'append_menu_item menu, "Exit the top-most script"
 append_menu_item menu, "Stop the script thread"
 append_menu_item menu, "Stop all scripts"
 append_menu_item menu, "Exit game"
 append_menu_item menu, "Enter script debugger"
 IF running_as_slave THEN append_menu_item menu, "Reload scripts"

 state.active = YES
 init_menu_state state, menu

 'Modify master() because the script debugger or other menus may setpal 
 REDIM remember_master(255) as RGBcolor
 FOR i as integer = 0 TO 255
  remember_master(i) = master(i)
 NEXT
 loadpalette master(), gam.current_master_palette
 setpal master()

 setkeys
 DO
  setwait 55
  setkeys

  IF keyval(scEsc) > 1 THEN 'continue
   EXIT DO 
  END IF

  IF keyval(scF1) > 1 THEN show_help("game_script_interrupt")

  IF enter_or_space() THEN
   SELECT CASE state.pt
    CASE 0 'continue
     ret = NO
    'CASE 1 'exit topmost  ... probably not too helpful
    ' killtopscript
    ' ret = YES
    CASE 1 'exit whole 'thread'
     killscriptthread
     ret = YES
    CASE 2 'kill everything
     killallscripts
     ret = YES
    CASE 3 'die
     debug "script_interrupt: User opted to quit"
     exitprogram NO
    CASE 4 'script debugger
     scrwatch = 2
     scriptwatcher scrwatch, 0 'clean mode, script state view mode
     ret = YES
    CASE 5 'reload scripts
     reload_scripts
     ret = NO
   END SELECT
   EXIT DO
  END IF
  
  usemenu state

  clearpage vpage

  centerbox 160, 12, 310, 15, 3, vpage
  textcolor uilook(uiText), 0
  printstr "A script is stuck", 160 - 17*4, 7, vpage

  FOR i as integer = 0 TO UBOUND(errtext)
   printstr errtext(i), 8, 25 + 10 * i, vpage
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
 ClearMenuData menu
 setkeys

 FOR i as integer = 0 TO 255
  master(i) = remember_master(i)
 NEXT
 setpal master()
 clearpage vpage
 setvispage vpage
 next_interpreter_check_time = TIMER + scriptCheckDelay

 'Note: when we resume after a script interruption, the keyboard state changes, which might break a script
 'Not worth worrying about this.
 RETURN ret
END FUNCTION

FUNCTION settingstring (searchee as string, setting as string, result as string) as integer

' checks to see if searchee begins with setting =
' if so, sets result to the uppercased space-trimmed value that
' follows the = sign and returns true. If not found, returns false

settingstring = 0

IF UCASE(LEFT(searchee, LEN(setting) + 1)) = setting + "=" THEN
 result = UCASE(LTRIM(RTRIM(MID(searchee, LEN(setting) + 2, 32))))
 settingstring = -1
END IF

END FUNCTION

SUB shop (byval id as integer)

DIM storebuf(40) as integer
DIM menu(10) as string
DIM menuid(10) as integer
DIM sn as string
DIM autopick as integer
DIM w as integer
DIM temp as integer
DIM tog as integer
DIM inn as integer
DIM rsr as integer
DIM h as integer
DIM c as integer
DIM t as integer
DIM o as integer
DIM page as integer
DIM holdscreen as integer
DIM st as MenuState

FOR i as integer = 0 TO 7
 menuid(i) = i
NEXT i

menu(0) = readglobalstring(70, "Buy", 10)
menu(1) = readglobalstring(71, "Sell", 10)
menu(2) = readglobalstring(73, "Hire", 10)
menu(3) = readglobalstring(72, "Inn", 10)
menu(4) = readglobalstring(63, "Equip", 10)
menu(5) = readglobalstring(66, "Save", 10)
menu(6) = readglobalstring(68, "Map", 10)
menu(7) = readglobalstring(65, "Team", 10)

st.size = 22
st.pt = 0
st.last = -1

'--initshop
setpicstuf storebuf(), 40, -1
loadset game + ".sho", id, 0
sn = readbadbinstring(storebuf(), 0, 15, 0)
o = 0
FOR i as integer = 0 TO 7
 IF readbit(storebuf(), 17, i) THEN
  SWAP menu(i), menu(o)
  SWAP menuid(i), menuid(o)
  st.last = o
  o = o + 1
 END IF
NEXT i

IF st.last = -1 THEN EXIT SUB
IF st.last = 0 THEN autopick = 1
st.last += 1
menu(st.last) = readglobalstring(74, "Exit", 10)
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
   w = onwho(readglobalstring(108, "Equip Who?", 20), 0)
   IF w >= 0 THEN
    equip w
   END IF
  END IF
  IF menuid(st.pt) = 5 THEN '--SAVE
   temp = picksave(0)
   IF temp >= 0 THEN savegame temp
  END IF
  IF menuid(st.pt) = 3 THEN '--INN
   IF useinn(storebuf(18), holdscreen) THEN
    innRestore
    IF storebuf(19) > 0 THEN
     '--Run animation for Inn
     trigger_script storebuf(19), NO, "inn", "ID " & id, scrqBackcompat()
     EXIT DO
    ELSE
     '--Inn has no script, do simple fade
     fadeout 0, 0, 80
     queue_fade_in
    END IF
   END IF
   copypage holdscreen, vpage
  END IF
  IF autopick THEN EXIT DO
 END IF
 copypage holdscreen, vpage
 h = (st.last + 2) * 10
 centerbox 160, 104 + (h * .5), 96, h, 1, page
 centerbox 160, 90, LEN(sn) * 8 + 8, 16, 1, page
 edgeprint sn, xstring(sn, 160), 85, uilook(uiText), page
 FOR i as integer = 0 TO st.last
  c = uilook(uiMenuItem): IF st.pt = i THEN c = uilook(uiSelectedItem + tog)
  edgeprint menu(i), xstring(menu(i), 160), 109 + i * 10, c, page
 NEXT i
 setvispage vpage
 check_for_queued_fade_in
 dowait
LOOP
FOR t = 4 TO 5: carray(t) = 0: NEXT t
freepage page
freepage holdscreen

evalitemtags
party_change_updates

END SUB

'holdscreen is a copy of vpage (not a compatpage)
FUNCTION useinn (byval price as integer, byval holdscreen as integer) as integer
DIM menu(1) as string
DIM page as integer
page = compatpage
DIM tog as integer

DIM state as MenuState
state.last = UBOUND(menu)
state.size = 2

useinn = 0 'default return value

menu(0) = readglobalstring(49, "Pay", 10)
menu(1) = readglobalstring(50, "Cancel", 10)
DIM inncost as string = readglobalstring(143, "THE INN COSTS", 20)
DIM youhave as string = readglobalstring(145, "You have", 20)
menusound gen(genAcceptSFX)
setkeys
DO
 setwait speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 IF carray(ccMenu) > 1 THEN
  menusound gen(genCancelSFX)
  EXIT DO
 END IF
 usemenusounds
 usemenu state
 'alternatively
 IF carray(ccLeft) > 1 OR carray(ccRight) > 1 THEN
  menusound gen(genCursorSFX)
  state.pt = state.pt XOR 1
 END IF
 IF carray(ccUse) > 1 THEN
  IF state.pt = 0 AND gold >= price THEN
   gold = gold - price
   useinn = -1
   menusound gen(genAcceptSFX)
   EXIT DO
  ELSE
   menusound gen(genCancelSFX)
  END IF
  IF state.pt = 1 THEN EXIT DO
 END IF

 'Draw screen
 copypage holdscreen, vpage
 edgeboxstyle 0, 3, 218, herocount() * 10 + 4, 0, page
 DIM y as integer = 0
 FOR i as integer = 0 TO 3
  IF hero(i) > 0 THEN
   DIM col as integer = uilook(uiText)
   edgeprint names(i), 128 - LEN(names(i)) * 8, 5 + y * 10, col, page
   edgeprint STR(gam.hero(i).stat.cur.hp) + "/" + STR(gam.hero(i).stat.max.hp), 136, 5 + y * 10, col, page
   y = y + 1
  END IF
 NEXT i
 centerfuz 160, 90, 200, 60, 1, page
 rectangle 130, 92, 60, 22, uilook(uiHighlight), page 'orig colour 20
 edgeprint inncost & " " & price & " " & readglobalstring(32, "Money"), 160 - LEN(inncost & price & " " & readglobalstring(32, "Money")) * 4, 70, uilook(uiText), page
 edgeprint youhave & " " & gold & " " & readglobalstring(32, "Money"), 160 - LEN(youhave & gold & " " & readglobalstring(32, "Money")) * 4, 80, uilook(uiText), page
 FOR i as integer = 0 TO 1
  DIM col as integer
  col = uilook(uiMenuItem)
  IF state.pt = i THEN col = uilook(uiSelectedItem + tog)
  edgeprint menu(i), 160 - LEN(menu(i)) * 4, 94 + i * 8, col, page
 NEXT i

 setvispage vpage
 check_for_queued_fade_in
 dowait
LOOP
freepage page
party_change_updates
END FUNCTION

SUB tagdisplay
DIM c as integer
STATIC st as menustate
st.size = 6
st.last = max_tag()

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
FOR i as integer = st.top TO st.top + st.size
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
 DIM fh as integer = FREEFILE
 OPEN exepath & SLASH & "joyset.ini" FOR OUTPUT as #fh
 PRINT #fh, "#Joystick/gamepad configuration"
 PRINT #fh, "UPTHRESH=" & joy(9)
 PRINT #fh, "DOWNTHRESH=" & joy(10)
 PRINT #fh, "LEFTTHRESH=" & joy(11)
 PRINT #fh, "RIGHTTHRESH=" & joy(12)
 PRINT #fh, "USEBUTTON=" & (joy(13) - 2)
 PRINT #fh, "MENUBUTTON=" & (joy(14) - 2)
 CLOSE #fh
END SUB

FUNCTION herocount (byval last as integer = 3) as integer
 '--differs from liveherocount() in that it does not care if they are alive
 DIM count as integer = 0
 FOR i as integer = 0 TO last
  IF hero(i) > 0 THEN count += 1
 NEXT i
 RETURN count
END FUNCTION

FUNCTION caterpillar_size () as integer
 'Returns the number of heroes on the map, regardless of whether caterpillar trailing is suspended
 IF readbit(gen(), genBits, 1) = 1 THEN RETURN herocount
 RETURN 1
END FUNCTION
