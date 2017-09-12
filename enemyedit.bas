'OHRRPGCE CUSTOM - Enemy Editor and Enemy/Hero Formation/Formation Set Editors
'(C) Copyright 1997-2017 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
#include "config.bi"
#include "const.bi"
#include "udts.bi"
#include "custom.bi"
#include "allmodex.bi"
#include "common.bi"
#include "loading.bi"
#include "customsubs.bi"
#include "slices.bi"
#include "thingbrowser.bi"
#include "cglobals.bi"

#include "uiconst.bi"
#include "scrconst.bi"

#include "flexmenu.bi"

'Defined in this file:

DECLARE FUNCTION enemy_edit_add_new (recbuf() as integer, preview_box as Slice ptr) as bool
DECLARE SUB enemy_edit_update_menu(byval recindex as integer, state as MenuState, recbuf() as integer, menu() as string, menuoff() as integer, menutype() as integer, menulimits() as integer, min() as integer, max() as integer, dispmenu() as string, workmenu() as integer, caption() as string, byref preview_sprite as Frame ptr, preview as Slice Ptr, byval dissolve_ticks as integer, byval EnLimSpawn as integer, byval EnLimAtk as integer, byval EnLimPic as integer, byval EnDatPic as integer, byval EnDatPal as integer, byval EnDatPicSize as integer)
DECLARE SUB enemy_edit_load(byval recnum as integer, recbuf() as integer, state as MenuState, caption() as string, byval EnCapElemResist as integer)
DECLARE SUB enemy_edit_pushmenu (state as MenuState, byref lastptr as integer, byref lasttop as integer, byref menudepth as bool)
DECLARE SUB enemy_edit_backmenu (state as MenuState, byval lastptr as integer, byval lasttop as integer, byref menudepth as bool, workmenu() as integer, mainMenu() as integer)

DECLARE SUB individual_formation_editor ()
DECLARE SUB formation_set_editor ()
DECLARE SUB draw_formation_slices OVERLOAD (eform as Formation, rootslice as Slice ptr, selected_slot as integer, page as integer)
DECLARE SUB draw_formation_slices OVERLOAD (eform as Formation, hform as HeroFormation, rootslice as Slice ptr, selected_slot as integer, page as integer, byval heromode as bool=NO)
DECLARE SUB load_formation_slices(ename() as string, form as Formation, rootslice as Slice ptr ptr)
DECLARE SUB hero_formation_editor ()

DECLARE SUB formation_set_editor_load_preview(state as MenuState, byref form_id as integer, formset as FormationSet, form as Formation, ename() as string, byref rootslice as Slice Ptr)

' Formation editor slice lookup codes
CONST SL_FORMEDITOR_BACKDROP = 100
CONST SL_FORMEDITOR_ENEMY = 200  '+0 to +7 for 8 slots
CONST SL_FORMEDITOR_LAST_ENEMY = 299  'End of range indicating an enemy slot
CONST SL_FORMEDITOR_CURSOR = 300
CONST SL_FORMEDITOR_HERO_AREA = 399  'container that holds heroes
CONST SL_FORMEDITOR_HERO = 400  '+0 to +3 for 4 slots


SUB update_enemy_editor_for_elementals(recbuf() as integer, caption() as string, byval EnCapElemResist as integer)
 FOR i as integer = 0 TO gen(genNumElements) - 1
  caption(EnCapElemResist + i) = format_percent(DeSerSingle(recbuf(), 239 + i*2))
 NEXT
END SUB

'recindex: which enemy to show. If -1, same as last time. If >= max, ask to add a new attack,
'(and exit and return -1 if cancelled).
'Otherwise, returns the enemy number we were last editing.
'Note: the enemy editor can be entered recursively!
FUNCTION enemy_editor (recindex as integer = -1) as integer

DIM elementnames() as string
getelementnames elementnames()

'-------------------------------------------------------------------------

'--bitsets
DIM ebit(64) as string

FOR i as integer = 32 TO 53
 ebit(i) = "" 'preferable to be blank, so we can hide it
NEXT i
ebit(54) = "Harmed by Cure"
ebit(55) = "MP Idiot"
ebit(56) = "Boss"
ebit(57) = "Unescapable"
ebit(58) = "Die Without Boss"
ebit(59) = "Flee instead of Die"
ebit(60) = "Untargetable by Enemies"
ebit(61) = "Untargetable by Heros"
ebit(62) = "Win battle even if alive"
ebit(63) = "Never flinch when attacked"
ebit(64) = "Ignored for ""Alone"" AI"

'-------------------------------------------------------------------------

'--record buffer
DIM recbuf(dimbinsize(binDT1)) as integer

CONST EnDatName = 0' to 16
CONST EnDatStealAvail = 17
CONST EnDatStealItem = 18
CONST EnDatStealItemP = 19
CONST EnDatStealRItem = 20
CONST EnDatStealRItemP = 21
CONST EnDatDissolve = 22
CONST EnDatDissolveTime = 23
CONST EnDatDeathSFX = 24
CONST EnDatCursorX = 25
CONST EnDatCursorY = 26
'27 to 52 unused
CONST EnDatPic = 53
CONST EnDatPal = 54
CONST EnDatPicSize = 55
CONST EnDatGold = 56
CONST EnDatExp = 57
CONST EnDatItem = 58
CONST EnDatItemP = 59
CONST EnDatRareItem = 60
CONST EnDatRareItemP = 61
CONST EnDatStat = 62' to 73
CONST EnDatBitset = 74' to 78
CONST EnDatSpawnDeath = 79
CONST EnDatSpawnNEDeath = 80
CONST EnDatSpawnAlone = 81
CONST EnDatSpawnNEHit = 82
CONST EnDatSpawnElement = 83' to 90
CONST EnDatSpawnNum = 91
CONST EnDatAtkNormal = 92' to 96
CONST EnDatAtkDesp = 97'   to 101
CONST EnDatAtkAlone = 102' to 106
CONST EnDatElemCtr = 107' to 114
CONST EnDatStatCtr = 115' to 126
CONST EnDatElemCtr2 = 127' to 182
CONST EnDatSpawnElement2 = 183' to 238
CONST EnDatElemResist = 239' to 366
CONST EnDatAtkBequest = 367
CONST EnDatNonElemCtr = 368

'-------------------------------------------------------------------------

DIM capindex as integer = 0
REDIM caption(-1 TO -1) as string
DIM max(26) as integer
DIM min(26) as integer
'Limit 0 is not used

CONST EnLimPic = 1
max(EnLimPic) = gen(genMaxEnemy1Pic) 'or 28 or 29. Must be updated!

CONST EnLimUInt = 2
max(EnLimUInt) = 32767

CONST EnLimPicSize = 3
max(EnLimPicSize) = 2
DIM EnCapPicSize as integer = capindex
addcaption caption(), capindex, "Small 34x34"
addcaption caption(), capindex, "Medium 50x50"
addcaption caption(), capindex, "Big 80x80"

CONST EnLimItem = 4
max(EnLimItem) = gen(genMaxItem)

CONST EnLimPercent = 5
max(EnLimPercent) = 100

CONST EnLimStat = 6' to 17
FOR i as integer = 0 TO statLast
 max(EnLimStat + i) = 32767  ' By default
NEXT
max(EnLimStat + statFocus) = 100
max(EnLimStat + statHitX) = 20

CONST EnLimSpawn = 18
max(EnLimSpawn) = gen(genMaxEnemy) + 1 'must be updated!

CONST EnLimSpawnNum = 19
max(EnLimSpawnNum) = 8

CONST EnLimAtk = 20
max(EnLimAtk) = gen(genMaxAttack) + 1 'Must be updated!

CONST EnLimStr16 = 21
max(EnLimStr16) = 16

CONST EnLimStealAvail = 22
min(EnLimStealAvail) = -1
max(EnLimStealAvail) = 1
addcaption caption(), capindex, "Disabled"
DIM EnCapStealAvail as integer = capindex
addcaption caption(), capindex, "Only one"
addcaption caption(), capindex, "Unlimited"

CONST EnLimPal16 = 23
max(EnLimPal16) = 32767
min(EnLimPal16) = -1

CONST EnLimDissolve = 24
min(EnLimDissolve) = 0
max(EnLimDissolve) = dissolveTypeMax + 1
DIM EnCapDissolve as integer = capindex
addcaption caption(), capindex, "Global Default"
FOR i as integer = 0 TO dissolveTypeMax
 addcaption caption(), capindex, dissolve_type_caption(i)
NEXT

CONST EnLimDissolveTime = 25
min(EnLimDissolveTime) = 0
max(EnLimDissolveTime) = 99

CONST EnLimDeathSFX = 26
min(EnLimDeathSFX) = -1
max(EnLimDeathSFX) = gen(genMaxSFX) + 1

DIM EnCapElemResist as integer = capindex
FOR i as integer = 0 TO gen(genNumElements) - 1
 addcaption caption(), capindex, ""  '--updated in update_enemy_editor_for_elementals
NEXT

'--next limit 27, remeber to update dim!

'-------------------------------------------------------------------------
'--menu content
DIM menu(265) as string
DIM menutype(265) as integer
DIM menuoff(265) as integer
DIM menulimits(265) as integer

CONST EnMenuBackAct = 0
menu(EnMenuBackAct) = "Previous Menu"
menutype(EnMenuBackAct) = 1

CONST EnMenuChooseAct = 1
menu(EnMenuChooseAct) = "Enemy"
menutype(EnMenuChooseAct) = 5

CONST EnMenuName = 2
menu(EnMenuName) = "Name:"
menutype(EnMenuName) = 4
menuoff(EnMenuName) = EnDatName
menulimits(EnMenuName) = EnLimStr16

CONST EnMenuAppearAct = 3
menu(EnMenuAppearAct) = "Appearance & Sounds..."
menutype(EnMenuAppearAct) = 1

CONST EnMenuRewardAct = 4
menu(EnMenuRewardAct) = "Rewards..."
menutype(EnMenuRewardAct) = 1

CONST EnMenuStatAct = 5
menu(EnMenuStatAct) = "Stats..."
menutype(EnMenuStatAct) = 1

CONST EnMenuBitsetAct = 6
menu(EnMenuBitsetAct) = "Bitsets..."
menutype(EnMenuBitsetAct) = 1

CONST EnMenuSpawnAct = 7
menu(EnMenuSpawnAct) = "Spawning..."
menutype(EnMenuSpawnAct) = 1

CONST EnMenuAtkAct = 8
menu(EnMenuAtkAct) = "Attacks..."
menutype(EnMenuAtkAct) = 1

CONST EnMenuPic = 9
menu(EnMenuPic) = "Picture:"
menutype(EnMenuPic) = 0
menuoff(EnMenuPic) = EnDatPic
menulimits(EnMenuPic) = EnLimPic

CONST EnMenuPal = 10
menu(EnMenuPal) = "Palette:"
menutype(EnMenuPal) = 12
menuoff(EnMenuPal) = EnDatPal
menulimits(EnMenuPal) = EnLimPal16

CONST EnMenuPicSize = 11
menu(EnMenuPicSize) = "Picture Size:"
menutype(EnMenuPicSize) = 2000 + EnCapPicSize
menuoff(EnMenuPicSize) = EnDatPicSize
menulimits(EnMenuPicSize) = EnLimPicSize

CONST EnMenuGold = 12
menu(EnMenuGold) = "Gold:"
menutype(EnMenuGold) = 0
menuoff(EnMenuGold) = EnDatGold
menulimits(EnMenuGold) = EnLimUInt

CONST EnMenuExp = 13
menu(EnMenuExp) = "Experience Points:"
menutype(EnMenuExp) = 0
menuoff(EnMenuExp) = EnDatExp
menulimits(EnMenuExp) = EnLimUInt

CONST EnMenuItem = 14
menu(EnMenuItem) = "Item:"
menutype(EnMenuItem) = 8
menuoff(EnMenuItem) = EnDatItem
menulimits(EnMenuItem) = EnLimItem

CONST EnMenuItemP = 15
menu(EnMenuItemP) = "Item%:"
menutype(EnMenuItemP) = 0
menuoff(EnMenuItemP) = EnDatItemP
menulimits(EnMenuItemP) = EnLimPercent

CONST EnMenuRareItem = 16
menu(EnMenuRareItem) = "Rare Item:"
menutype(EnMenuRareItem) = 8
menuoff(EnMenuRareItem) = EnDatRareItem
menulimits(EnMenuRareItem) = EnLimItem

CONST EnMenuRareItemP = 17
menu(EnMenuRareItemP) = "Rare Item%:"
menutype(EnMenuRareItemP) = 0
menuoff(EnMenuRareItemP) = EnDatRareItemP
menulimits(EnMenuRareItemP) = EnLimPercent

CONST EnMenuStat = 18' to 29
FOR i as integer = 0 TO 11
 menu(EnMenuStat + i) = statnames(i) + ":"
 menutype(EnMenuStat + i) = 0
 menuoff(EnMenuStat + i) = EnDatStat + i
 menulimits(EnMenuStat + i) = EnLimStat + i
NEXT i
menutype(EnMenuStat + 8) = 15 'Speed should show turn-time estimate

CONST EnMenuSpawnDeath = 30
menu(EnMenuSpawnDeath) = "Spawn on Death:"
menutype(EnMenuSpawnDeath) = 9
menuoff(EnMenuSpawnDeath) = EnDatSpawnDeath
menulimits(EnMenuSpawnDeath) = EnLimSpawn

CONST EnMenuSpawnNEDeath = 31
menu(EnMenuSpawnNEDeath) = "on Non-Elemental Death:"
menutype(EnMenuSpawnNEDeath) = 9
menuoff(EnMenuSpawnNEDeath) = EnDatSpawnNEDeath
menulimits(EnMenuSpawnNEDeath) = EnLimSpawn

CONST EnMenuSpawnAlone = 32
menu(EnMenuSpawnAlone) = "Spawn When Alone:"
menutype(EnMenuSpawnAlone) = 9
menuoff(EnMenuSpawnAlone) = EnDatSpawnAlone
menulimits(EnMenuSpawnAlone) = EnLimSpawn

CONST EnMenuSpawnNEHit = 33
menu(EnMenuSpawnNEHit) = "on Non-Elemental Hit:"
menutype(EnMenuSpawnNEHit) = 9
menuoff(EnMenuSpawnNEHit) = EnDatSpawnNEHit
menulimits(EnMenuSpawnNEHit) = EnLimSpawn

CONST EnMenuSpawnElement = 34' to 93
FOR i as integer = 0 TO gen(genNumElements) - 1
 menu(EnMenuSpawnElement + i) = "on " & elementnames(i) & " Hit:"
 menutype(EnMenuSpawnElement + i) = 9
 IF i < 8 THEN
  menuoff(EnMenuSpawnElement + i) = EnDatSpawnElement + i
 ELSE
  menuoff(EnMenuSpawnElement + i) = EnDatSpawnElement2 + (i - 8)
 END IF
 menulimits(EnMenuSpawnElement + i) = EnLimSpawn
NEXT i

CONST EnMenuSpawnNum = 98
menu(EnMenuSpawnNum) = "How Many to Spawn:"
menutype(EnMenuSpawnNum) = 0
menuoff(EnMenuSpawnNum) = EnDatSpawnNum
menulimits(EnMenuSpawnNum) = EnLimSpawnNum

CONST EnMenuAtkNormal = 99' to 103
FOR i as integer = 0 TO 4
 menu(EnMenuAtkNormal + i) = "Normal:"
 menutype(EnMenuAtkNormal + i) = 7
 menuoff(EnMenuAtkNormal + i) = EnDatAtkNormal + i
 menulimits(EnMenuAtkNormal + i) = EnLimAtk
NEXT i

CONST EnMenuAtkDesp = 104' to 108
FOR i as integer = 0 TO 4
 menu(EnMenuAtkDesp + i) = "Desperation:"
 menutype(EnMenuAtkDesp + i) = 7
 menuoff(EnMenuAtkDesp + i) = EnDatAtkDesp + i
 menulimits(EnMenuAtkDesp + i) = EnLimAtk
NEXT i

CONST EnMenuAtkAlone = 109' to 113
FOR i as integer = 0 TO 4
 menu(EnMenuAtkAlone + i) = "Alone:"
 menutype(EnMenuAtkAlone + i) = 7
 menuoff(EnMenuAtkAlone + i) = EnDatAtkAlone + i
 menulimits(EnMenuAtkAlone + i) = EnLimAtk
NEXT i

CONST EnMenuStealItem = 114
menu(EnMenuStealItem) = "Stealable Item:"
menutype(EnMenuStealItem) = 8
menuoff(EnMenuStealItem) = EnDatStealItem
menulimits(EnMenuStealItem) = EnLimItem

CONST EnMenuStealRItem = 115
menu(EnMenuStealRItem) = "Rare Stealable Item:"
menutype(EnMenuStealRItem) = 8
menuoff(EnMenuStealRItem) = EnDatStealRItem
menulimits(EnMenuStealRItem) = EnLimItem

CONST EnMenuStealItemP = 116
menu(EnMenuStealItemP) = "Steal Rate%:"
menutype(EnMenuStealItemP) = 0
menuoff(EnMenuStealItemP) = EnDatStealItemP
menulimits(EnMenuStealItemP) = EnLimPercent

CONST EnMenuStealRItemP = 117
menu(EnMenuStealRItemP) = "Rare Steal Rate%:"
menutype(EnMenuStealRItemP) = 0
menuoff(EnMenuStealRItemP) = EnDatStealRItemP
menulimits(EnMenuStealRItemP) = EnLimPercent

CONST EnMenuStealAvail = 118
menu(EnMenuStealAvail) = "Steal Availability:"
menutype(EnMenuStealAvail) = 2000 + EnCapStealAvail
menuoff(EnMenuStealAvail) = EnDatStealAvail
menulimits(EnMenuStealAvail) = EnLimStealAvail

CONST EnMenuDissolve = 119
menu(EnMenuDissolve) = "Death Animation:"
menutype(EnMenuDissolve) = 2000 + EnCapDissolve
menuoff(EnMenuDissolve) = EnDatDissolve
menulimits(EnMenuDissolve) = EnLimDissolve

CONST EnMenuDissolveTime = 120
menu(EnMenuDissolveTime) = "Death Animation ticks:"
menutype(EnMenuDissolveTime) = 13
menuoff(EnMenuDissolveTime) = EnDatDissolveTime
menulimits(EnMenuDissolveTime) = EnLimDissolveTime

CONST EnMenuDeathSFX = 121
menu(EnMenuDeathSFX) = "Death Sound Effect:"
menutype(EnMenuDeathSFX) = 14
menuoff(EnMenuDeathSFX) = EnDatDeathSFX
menulimits(EnMenuDeathSFX) = EnLimDeathSFX

CONST EnMenuCursorOffset = 122
menu(EnMenuCursorOffset) = "Cursor Offset..."
menutype(EnMenuCursorOffset) = 1

CONST EnMenuElemCtr = 123' to 186
FOR i as integer = 0 TO gen(genNumElements) - 1
 menu(EnMenuElemCtr + i) = "Counter element " & elementnames(i) & ":"
 menutype(EnMenuElemCtr + i) = 7
 IF i < 8 THEN
  menuoff(EnMenuElemCtr + i) = EnDatElemCtr + i
 ELSE
  menuoff(EnMenuElemCtr + i) = EnDatElemCtr2 + (i - 8)
 END IF
 menulimits(EnMenuElemCtr + i) = EnLimAtk
NEXT i

CONST EnMenuStatCtr = 187' to 198
FOR i as integer = 0 TO 11
 menu(EnMenuStatCtr + i) = "Counter damage to " & statnames(i) & ":"
 menutype(EnMenuStatCtr + i) = 7
 menuoff(EnMenuStatCtr + i) = EnDatStatCtr + i
 menulimits(EnMenuStatCtr + i) = EnLimAtk
NEXT i

CONST EnMenuElementalsAct = 199
menu(EnMenuElementalsAct) = "Elemental Resistances..."
menutype(EnMenuElementalsAct) = 1

CONST EnMenuElemDmg = 200' to 263
FOR i as integer = 0 TO gen(genNumElements) - 1
 menu(EnMenuElemDmg + i) = "Damage from " + rpad(elementnames(i), " ", 15) + ":"
 menutype(EnMenuElemDmg + i) = 5000 + EnCapElemResist + i  'percent_grabber
 menuoff(EnMenuElemDmg + i) = 239 + i*2 
NEXT

CONST EnMenuAtkBequest = 264
menu(EnMenuAtkBequest) = "On-Death Bequest Attack:"
menutype(EnMenuAtkBequest) = 7
menuoff(EnMenuAtkBequest) = EnDatAtkBequest
menulimits(EnMenuAtkBequest) = EnLimAtk

CONST EnMenuNonElemCtr = 265
menu(EnMenuNonElemCtr) = "Counter non-elemental attacks:"
menutype(EnMenuNonElemCtr) = 7
menuoff(EnMenuNonElemCtr) = EnDatNonElemCtr
menulimits(EnMenuNonElemCtr) = EnLimAtk

'-------------------------------------------------------------------------
'--menu structure
'WARNING: make these big enough to hold atkMenu when genNumElements is maxed out
DIM workmenu(93) as integer
DIM dispmenu(93) as string
DIM state as MenuState
state.autosize = YES
state.autosize_ignore_pixels = 12
state.need_update = YES

DIM mainMenu(9) as integer
mainMenu(0) = EnMenuBackAct
mainMenu(1) = EnMenuChooseAct
mainMenu(2) = EnMenuName
mainMenu(3) = EnMenuAppearAct
mainMenu(4) = EnMenuRewardAct
mainMenu(5) = EnMenuStatAct
mainMenu(6) = EnMenuBitsetAct
mainMenu(7) = EnMenuElementalsAct
mainMenu(8) = EnMenuSpawnAct
mainMenu(9) = EnMenuAtkAct

DIM appearMenu(7) as integer
appearMenu(0) = EnMenuBackAct
appearMenu(1) = EnMenuPicSize
appearMenu(2) = EnMenuPic
appearMenu(3) = EnMenuPal
appearMenu(4) = EnMenuDissolve
appearMenu(5) = EnMenuDissolveTime
appearMenu(6) = EnMenuDeathSFX
appearMenu(7) = EnMenuCursorOffset

DIM rewardMenu(11) as integer
rewardMenu(0) = EnMenuBackAct
rewardMenu(1) = EnMenuGold
rewardMenu(2) = EnMenuExp
rewardMenu(3) = EnMenuItem
rewardMenu(4) = EnMenuItemP
rewardMenu(5) = EnMenuRareItem
rewardMenu(6) = EnMenuRareItemP
rewardMenu(7) = EnMenuStealAvail
rewardMenu(8) = EnMenuStealItem
rewardMenu(9) = EnMenuStealItemP
rewardMenu(10) = EnMenuStealRItem
rewardMenu(11) = EnMenuStealRItemP

DIM statMenu(12) as integer
statMenu(0) = EnMenuBackAct
FOR i as integer = 0 TO 11
 statMenu(1 + i) = EnMenuStat + i
NEXT i

DIM spawnMenu(5 + gen(genNumElements)) as integer
spawnMenu(0) = EnMenuBackAct
spawnMenu(1) = EnMenuSpawnNum
spawnMenu(2) = EnMenuSpawnDeath
spawnMenu(3) = EnMenuSpawnNEDeath
spawnMenu(4) = EnMenuSpawnAlone
spawnMenu(5) = EnMenuSpawnNEHit
FOR i as integer = 0 TO gen(genNumElements) - 1
 spawnMenu(6 + i) = EnMenuSpawnElement + i
NEXT i

DIM atkMenu(29 + gen(genNumElements)) as integer
atkMenu(0) = EnMenuBackAct
FOR i as integer = 0 TO 4
 atkMenu(1 + i) = EnMenuAtkNormal + i
 atkMenu(6 + i) = EnMenuAtkDesp + i
 atkMenu(11 + i) = EnMenuAtkAlone + i
NEXT i
atkMenu(16) = EnMenuAtkBequest
FOR i as integer = 0 TO gen(genNumElements) - 1
 atkMenu(17 + i) = EnMenuElemCtr + i
NEXT i
atkMenu(17 + gen(genNumElements)) = EnMenuNonElemCtr
FOR i as integer = 0 TO 11
 atkMenu(18 + gen(genNumElements) + i) = EnMenuStatCtr + i
NEXT i

DIM elementalMenu(gen(genNumElements)) as integer
elementalMenu(0) = EnMenuBackAct
FOR i as integer = 0 TO gen(genNumElements) - 1
 elementalMenu(1 + i) = EnMenuElemDmg + i
NEXT i

DIM helpkey as string = "enemy"

'--Create the box that holds the preview
DIM preview_box as Slice Ptr
preview_box = NewSliceOfType(slRectangle)
ChangeRectangleSlice preview_box, ,uilook(uiDisabledItem), uilook(uiMenuItem), , transOpaque
'--Align the box in the bottom right
WITH *preview_box
 .X = -8
 .Y = -8
 .Width = 82
 .Height = 82
 .AnchorHoriz = 2
 .AlignHoriz = 2
 .AnchorVert = 2
 .AlignVert = 2
END WITH

'--Create the preview sprite. It will be updated before it is drawn.
DIM preview as Slice Ptr
preview = NewSliceOfType(slSprite, preview_box)
'--Align the sprite to the bottom center of the containing box
WITH *preview
 .Y = -1
 .AnchorHoriz = 1
 .AlignHoriz = 1
 .AnchorVert = 2
 .AlignVert = 2
END WITH

'--Need a copy of the sprite to call frame_dissolved on
DIM preview_sprite as Frame ptr

'--dissolve_ticks is >= 0 while playing a dissolve; > dissolve_time while during lag period afterwards
DIM as integer dissolve_time, dissolve_type, dissolve_ticks
dissolve_ticks = -1

'--default starting menu
setactivemenu workmenu(), mainMenu(), state
state.pt = 1  'Select <-Enemy ..-> line
state.size = 25

DIM menudepth as bool = NO
DIM lastptr as integer = 0
DIM lasttop as integer = 0

STATIC rememberindex as integer = -1   'Record to switch to with TAB
DIM show_name_ticks as integer = 0  'Number of ticks to show name (after switching record with TAB)

DIM remember_bit as integer = -1
DIM drawpreview as bool = YES

'Which enemy to show?
STATIC remember_recindex as integer = 0
IF recindex < 0 THEN
 recindex = remember_recindex
ELSE
 IF recindex > gen(genMaxEnemy) THEN
  IF enemy_edit_add_new(recbuf(), preview_box) THEN
   'Added a new record (blank or copy)
   saveenemydata recbuf(), recindex
   recindex = gen(genMaxEnemy) + 1
  ELSE
   DeleteSlice @preview_box
   RETURN -1
  END IF
 END IF
END IF

'load data here
enemy_edit_load recindex, recbuf(), state, caption(), EnCapElemResist

'------------------------------------------------------------------------
'--main loop

setkeys YES
DO
 setwait 55
 setkeys YES
 IF keyval(scESC) > 1 THEN
  IF menudepth THEN
   enemy_edit_backmenu state, lastptr, lasttop, menudepth, workmenu(), mainMenu()
   helpkey = "enemy"
   drawpreview = YES
  ELSE
   EXIT DO
  END IF
 END IF

 '--SHIFT+BACKSPACE
 IF cropafter_keycombo(workmenu(state.pt) = EnMenuChooseAct) THEN
  cropafter recindex, gen(genMaxEnemy), 0, game + ".dt1", getbinsize(binDT1)
 END IF

 usemenu state

 IF workmenu(state.pt) = EnMenuChooseAct OR (keyval(scAlt) > 0 and NOT isStringField(menutype(workmenu(state.pt)))) THEN
  DIM lastindex as integer = recindex
  IF intgrabber_with_addset(recindex, 0, gen(genMaxEnemy), 32767, "enemy") THEN
   saveenemydata recbuf(), lastindex
   IF recindex > gen(genMaxEnemy) THEN
    '--adding a new set
    IF enemy_edit_add_new(recbuf(), preview_box) THEN
     'Added a new record (blank or copy)
     saveenemydata recbuf(), recindex
    ELSE
     'cancelled add, reload the old last record
     recindex -= 1
     enemy_edit_load recindex, recbuf(), state, caption(), EnCapElemResist
    END IF
   ELSE
    enemy_edit_load recindex, recbuf(), state, caption(), EnCapElemResist
   END IF
   state.need_update = YES
  END IF
 END IF

 IF keyval(scF1) > 1 THEN show_help helpkey

 IF keyval(scTab) > 1 THEN
  IF keyval(scShift) > 0 THEN
   rememberindex = recindex
  ELSEIF rememberindex >= 0 AND rememberindex <= gen(genMaxEnemy) THEN
   saveenemydata recbuf(), recindex
   SWAP rememberindex, recindex
   enemy_edit_load recindex, recbuf(), state, caption(), EnCapElemResist
   show_name_ticks = 23
  END IF
 END IF

 IF enter_space_click(state) THEN
  DIM nowindex as integer = workmenu(state.pt)
  SELECT CASE menutype(nowindex)
   CASE 8 ' Item
    DIM itemb as ItemBrowser
    recbuf(menuoff(nowindex)) = itemb.browse(recbuf(menuoff(nowindex)))
    state.need_update = YES
   CASE 10 ' Item with offset
    DIM itemb as ItemBrowser
    recbuf(menuoff(nowindex)) = itemb.browse(recbuf(menuoff(nowindex)) - 1) + 1
    state.need_update = YES
  END SELECT
  SELECT CASE nowindex
   CASE EnMenuChooseAct
    'The <-Enemy #-> line; enter exits so that if we were called from another menu
    'it is easy to select an enemy and return to it.
    EXIT DO
   CASE EnMenuBackAct
    IF menudepth THEN
     enemy_edit_backmenu state, lastptr, lasttop, menudepth, workmenu(), mainMenu()
     helpkey = "enemy"
     drawpreview = YES
    ELSE
     EXIT DO
    END IF
   CASE EnMenuAppearAct
    enemy_edit_pushmenu state, lastptr, lasttop, menudepth
    setactivemenu workmenu(), appearMenu(), state
    helpkey = "enemy_appearance"
    state.need_update = YES
   CASE EnMenuRewardAct
    enemy_edit_pushmenu state, lastptr, lasttop, menudepth
    setactivemenu workmenu(), rewardMenu(), state
    helpkey = "enemy_rewards"
    state.need_update = YES
   CASE EnMenuStatAct
    enemy_edit_pushmenu state, lastptr, lasttop, menudepth
    setactivemenu workmenu(), statMenu(), state
    helpkey = "enemy_stats"
    state.need_update = YES
   CASE EnMenuSpawnAct
    enemy_edit_pushmenu state, lastptr, lasttop, menudepth
    setactivemenu workmenu(), spawnMenu(), state
    helpkey = "enemy_spawning"
    drawpreview = NO
    state.need_update = YES
   CASE EnMenuAtkAct
    enemy_edit_pushmenu state, lastptr, lasttop, menudepth
    setactivemenu workmenu(), atkMenu(), state
    helpkey = "enemy_attacks"
    drawpreview = NO
    state.need_update = YES
   CASE EnMenuElementalsAct
    enemy_edit_pushmenu state, lastptr, lasttop, menudepth
    setactivemenu workmenu(), elementalMenu(), state
    helpkey = "enemy_elementals"
    drawpreview = NO
    state.need_update = YES
   CASE EnMenuPal
    recbuf(EnDatPal) = pal16browse(recbuf(EnDatPal), recbuf(EnDatPicSize) + sprTypeSmallEnemy, recbuf(EnDatPic), YES)
    state.need_update = YES
   CASE EnMenuDeathSFX
    IF recbuf(EnDatDeathSFX) >= 1 THEN playsfx recbuf(EnDatDeathSFX) - 1
    IF recbuf(EnDatDeathSFX) = 0 THEN playsfx gen(genDefaultDeathSFX) - 1
   CASE EnMenuBitsetAct
    editbitset recbuf(), EnDatBitset, UBOUND(ebit), ebit(), "enemy_bitsets", remember_bit
   CASE EnMenuDissolve, EnMenuDissolveTime
    IF recbuf(EnDatDissolve) THEN dissolve_type = recbuf(EnDatDissolve) - 1 ELSE dissolve_type = gen(genEnemyDissolve)
    dissolve_time = recbuf(EnDatDissolveTime) 
    IF dissolve_time = 0 THEN dissolve_time = default_dissolve_time(dissolve_type, preview_sprite->w, preview_sprite->h)
    dissolve_ticks = 0
   CASE EnMenuCursorOffset
    '--temporarily move the preview image
    SetSliceParent(preview, SliceTable.Root)
    preview->AnchorVert = 1
    preview->AlignVert = 1
    WITH sprite_sizes(recbuf(EnDatPicSize) + sprTypeSmallEnemy)
     recbuf(EnDatCursorX) += .size.x / 2 '--offset relative to the top middle
     xy_position_on_slice preview, recbuf(EnDatCursorX), recbuf(EnDatCursorY), "Targetting Cursor Offset", "xy_target_cursor"
     recbuf(EnDatCursorX) -= .size.x / 2
    END WITH
    '--move the preview image back how it was before
    SetSliceParent(preview, preview_box)
    preview->AnchorVert = 2
    preview->AlignVert = 2
  END SELECT
 END IF

 IF keyval(scAlt) = 0 or isStringField(menutype(workmenu(state.pt))) THEN 'not pressing ALT, or not allowed to
  IF editflexmenu(state, workmenu(state.pt), menutype(), menuoff(), menulimits(), recbuf(), caption(), min(), max()) THEN
   state.need_update = YES
  END IF
 END IF

 IF flexmenu_handle_crossrefs(state, workmenu(state.pt), menutype(), menuoff(), recindex, recbuf(), NO) THEN
  'Reload this enemy in case it was changed in recursive call to the editor (in fact, this record might be deleted!)
  recindex = small(recindex, gen(genMaxEnemy))
  enemy_edit_load recindex, recbuf(), state, caption(), EnCapElemResist
  show_name_ticks = 23
  state.need_update = YES
 END IF

 IF dissolve_ticks >= 0 THEN
  dissolve_ticks += 1
  IF dissolve_ticks > dissolve_time + 15 THEN
   dissolve_ticks = -1
   state.need_update = YES
  ELSE
   IF dissolve_ticks <= dissolve_time THEN
    SetSpriteToFrame preview, frame_dissolved(preview_sprite, dissolve_time, dissolve_ticks, dissolve_type), , _
                     abs_pal_num(recbuf(EnDatPal), sprTypeSmallEnemy + recbuf(EnDatPicSize), recbuf(EnDatPic))
   END IF
  END IF
 END IF
 'lag time after fading out, to give a more realistic preview
 preview->Visible = (dissolve_ticks <= dissolve_time)

 IF state.need_update THEN
  state.need_update = NO
  enemy_edit_update_menu recindex, state, recbuf(), menu(), menuoff(), menutype(), menulimits(), min(), max(), dispmenu(), workmenu(), caption(), preview_sprite, preview, dissolve_ticks, EnLimSpawn, EnLimAtk, EnLimPic, EnDatPic, EnDatPal, EnDatPicSize
 END IF

 clearpage vpage
 IF drawpreview THEN
  DrawSlice preview_box, vpage
 END IF

 standardmenu dispmenu(), state, 0, 0, vpage
 draw_fullscreen_scrollbar state, , vpage
 IF keyval(scAlt) > 0 OR show_name_ticks > 0 THEN 'holding ALT or just pressed TAB
  show_name_ticks = large(0, show_name_ticks - 1)
  DIM tmpstr as string = readbadbinstring(recbuf(), EnDatName, 15, 0) & " " & recindex
  textcolor uilook(uiText), uilook(uiHighlight)
  printstr tmpstr, pRight, 0, vpage
 END IF
 edgeprint flexmenu_tooltip(workmenu(state.pt), menutype()), pLeft, pBottom, uilook(uiDisabledItem), vpage

 setvispage vpage
 dowait
LOOP

'--save what we were last working on
saveenemydata recbuf(), recindex

resetsfx
DeleteSlice @preview_box
frame_unload @preview_sprite

remember_recindex = recindex
RETURN recindex

END FUNCTION

SUB enemy_edit_backmenu (state as MenuState, byval lastptr as integer, byval lasttop as integer, byref menudepth as bool, workmenu() as integer, mainMenu() as integer)
 setactivemenu workmenu(), mainMenu(), state
 menudepth = NO
 state.pt = lastptr
 state.top = lasttop
 state.need_update = YES
END SUB

SUB enemy_edit_pushmenu (state as MenuState, byref lastptr as integer, byref lasttop as integer, byref menudepth as bool)
 lastptr = state.pt
 lasttop = state.top
 menudepth = YES
END SUB

SUB enemy_edit_load(byval recnum as integer, recbuf() as integer, state as MenuState, caption() as string, byval EnCapElemResist as integer)
 loadenemydata recbuf(), recnum
 update_enemy_editor_for_elementals recbuf(), caption(), EnCapElemResist
 state.need_update = YES
END SUB

SUB enemy_edit_update_menu(byval recindex as integer, state as MenuState, recbuf() as integer, menu() as string, menuoff() as integer, menutype() as integer, menulimits() as integer, min() as integer, max() as integer, dispmenu() as string, workmenu() as integer, caption() as string, byref preview_sprite as Frame ptr, preview as Slice Ptr, byval dissolve_ticks as integer, byval EnLimSpawn as integer, byval EnLimAtk as integer, byval EnLimPic as integer, byval EnDatPic as integer, byval EnDatPal as integer, byval EnDatPicSize as integer)

 '--in case new enemies/attacks have been added
 max(EnLimSpawn) = gen(genMaxEnemy) + 1
 max(EnLimAtk) = gen(genMaxAttack) + 1

 '--in case the PicSize has changed
 max(EnLimPic) = gen(genMaxEnemy1Pic + bound(recbuf(EnDatPicSize), 0, 2))
 
 '--re-enforce bounds, as they might have just changed
 enforceflexbounds menuoff(), menutype(), menulimits(), recbuf(), min(), max()
 
 updateflexmenu state.pt, dispmenu(), workmenu(), state.last, menu(), menutype(), menuoff(), menulimits(), recbuf(), caption(), max(), recindex
 
 '--stop sounds
 resetsfx
 '--update the picture and palette preview
 frame_unload @preview_sprite
 preview_sprite = frame_load(sprTypeSmallEnemy + recbuf(EnDatPicSize), recbuf(EnDatPic))
 dissolve_ticks = -1
 '--resets if dissolved
 ChangeSpriteSlice preview, sprTypeSmallEnemy + recbuf(EnDatPicSize), recbuf(EnDatPic), recbuf(EnDatPal), ,YES

END SUB

'Returns YES if a new record was added, or NO if cancelled.
'When YES, gen(genMaxEnemy) gets updated, and recbuf() will be populated with
'blank or cloned record, and unsaved! Previous contents are discarded.
'TODO: convert to generic_add_new
FUNCTION enemy_edit_add_new (recbuf() as integer, preview_box as Slice ptr) as bool
  DIM enemy as EnemyDef
  DIM menu(2) as string
  DIM enemytocopy as integer = 0
  DIM preview as Slice ptr = preview_box->FirstChild
  DIM state as MenuState
  state.last = UBOUND(menu)
  state.size = 24
  state.pt = 1

  state.need_update = YES
  setkeys
  DO
    setwait 55
    setkeys
    IF keyval(scESC) > 1 THEN setkeys : RETURN NO 'cancel
    IF keyval(scF1) > 1 THEN show_help "enemy_new"
    usemenu state
    IF state.pt = 2 THEN
      IF intgrabber(enemytocopy, 0, gen(genMaxEnemy)) THEN state.need_update = YES
    END IF
    IF state.need_update THEN
      state.need_update = NO
      loadenemydata recbuf(), enemytocopy
      loadenemydata enemy, enemytocopy
      ChangeSpriteSlice preview, 1 + enemy.size, enemy.pic, enemy.pal, , YES

      menu(0) = "Cancel"
      menu(1) = "New Blank Enemy"
      menu(2) = "Copy of Enemy " & enemytocopy & " " & enemy.name
    END IF
    IF enter_space_click(state) THEN
      setkeys
      SELECT CASE state.pt
        CASE 0 ' cancel
          RETURN NO
        CASE 1 ' blank
          gen(genMaxEnemy) += 1
          clearenemydata recbuf()
          RETURN YES
        CASE 2 ' copy
          gen(genMaxEnemy) += 1
          RETURN YES
      END SELECT
    END IF

    clearpage vpage
    standardmenu menu(), state, 20, 20, vpage
    IF state.pt = 2 THEN DrawSlice preview_box, vpage
    setvispage vpage
    dowait
  LOOP
END FUNCTION


'--------------------------------- Formation Editor ----------------------------


SUB formation_editor
 DIM menu(3) as string
 menu(0) = "Return to Main Menu"
 menu(1) = "Edit Individual Enemy Formations..."
 menu(2) = "Construct Formation Sets..."
 menu(3) = "Edit Hero Formations..."

 DIM state as MenuState
 state.size = 24
 state.last = UBOUND(menu)

 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "formation_main"
  usemenu state
  IF enter_space_click(state) THEN
   IF state.pt = 0 THEN EXIT DO
   IF state.pt = 1 THEN individual_formation_editor
   IF state.pt = 2 THEN formation_set_editor
   IF state.pt = 3 THEN hero_formation_editor
  END IF

  clearpage dpage
  standardmenu menu(), state, 0, 0, dpage

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB

SUB formation_set_editor
 DIM form as Formation
 DIM formset as FormationSet
 DIM set_id as integer = 1, form_id as integer
 DIM menu(23) as string
 DIM rootslice as Slice ptr
 DIM as string ename(7)
 DIM state as MenuState
 state.last = UBOUND(menu)
 state.size = 24
 DIM menuopts as MenuOptions
 menuopts.edged = YES
 menuopts.itemspacing = -1

 LoadFormationSet formset, set_id
 formation_set_editor_load_preview state, form_id, formset, form, ename(), rootslice

 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(scESC) > 1 THEN
   SaveFormationSet formset, set_id
   EXIT DO
  END IF
  IF keyval(scF1) > 1 THEN show_help "formation_sets"
  IF usemenu(state) THEN 
   formation_set_editor_load_preview state, form_id, formset, form, ename(), rootslice
  END IF
  IF enter_space_click(state) THEN
   IF state.pt = 0 THEN
    SaveFormationSet formset, set_id
    EXIT DO
   END IF
  END IF
  IF state.pt = 1 THEN
   DIM remember_id as integer = set_id
   IF intgrabber(set_id, 1, 255) THEN
    SaveFormationSet formset, remember_id
    LoadFormationSet formset, set_id
   END IF
  END IF
  IF state.pt = 2 THEN intgrabber formset.frequency, 0, 200
  IF state.pt = 3 THEN tag_grabber formset.tag, state
  IF state.pt >= 4 THEN
   IF intgrabber(formset.formations(state.pt - 4), -1, gen(genMaxFormation)) THEN
    formation_set_editor_load_preview state, form_id, formset, form, ename(), rootslice
   END IF
  END IF
  IF state.pt >= 4 AND form_id >= 0 THEN
   draw_formation_slices form, rootslice, -1, dpage
  ELSE
   clearpage dpage
  END IF
  menu(0) = "Previous Menu"
  menu(1) = CHR(27) & "Formation Set " & set_id & CHR(26)
  menu(2) = "Battle Frequency: " & formset.frequency & " (" & step_estimate(formset.frequency, 40, 160, "-", " steps") & ")"
  menu(3) = tag_condition_caption(formset.tag, "Only if tag", "No tag check")
  FOR i as integer = 0 TO 19
   IF formset.formations(i) = -1 THEN
    menu(4 + i) = "Empty"
   ELSE
    menu(4 + i) = "Formation " & formset.formations(i)
   END IF
  NEXT i

  standardmenu menu(), state, 0, 0, dpage, menuopts

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 DeleteSlice @rootslice
 EXIT SUB

END SUB

SUB formation_set_editor_load_preview(state as MenuState, byref form_id as integer, formset as FormationSet, form as Formation, ename() as string, byref rootslice as slice Ptr)
 IF state.pt >= 4 THEN
  '--have form selected
  form_id = formset.formations(state.pt - 4)
  IF form_id >= 0 THEN
   '--form not empty
   LoadFormation form, form_id
   load_formation_slices ename(), form, @rootslice
  END IF
 END IF
END SUB

SUB hero_formation_editor ()
 DIM hero_form_id as integer = 0
 DIM test_form_id as integer = 0
 DIM ename(7) as string
 DIM eform as Formation
 DIM hform as HeroFormation
 DIM default_hform as HeroFormation
 DIM rootslice as Slice ptr
 DIM as integer i
 DIM positioning_mode as bool = NO
 DIM as integer bgwait, bgctr

 LoadFormation eform, test_form_id
 load_formation_slices ename(), eform, @rootslice

 DIM menu(6) as string
 DIM state as MenuState
 state.pt = 0
 state.top = 0
 state.first = 0
 state.last = UBOUND(menu)
 state.size = 20
 DIM menuopts as MenuOptions
 menuopts.edged = YES

 CONST first_hero_item = 3
 'slot -1 indicates no enemy selected
 DIM slot as integer = state.pt - first_hero_item
 IF slot < 0 THEN slot = -1
 
 default_hero_formation default_hform
 load_hero_formation hform, hero_form_id

 setkeys
 DO
  setwait 55
  setkeys
  IF positioning_mode = YES THEN
   '--hero positioning mode
   IF keyval(scESC) > 1 OR enter_or_space() THEN setkeys: positioning_mode = NO
   IF keyval(scF1) > 1 THEN show_help "hero_formation_editor_placement"
   DIM as integer thiswidth = 0, thisheight = 0, movespeed = 1
   IF keyval(scShift) THEN movespeed = 8
   WITH hform.slots(slot)
    DIM hrect as Slice ptr = LookupSlice(SL_FORMEDITOR_HERO + slot, rootslice)
    IF hrect THEN
     thiswidth = hrect->Width
     thisheight = hrect->Height
    END IF
    IF keyval(scUp) > 0 THEN .pos.y -= movespeed
    IF keyval(scDown) > 0 THEN .pos.y += movespeed
    IF keyval(scLeft) > 0 THEN .pos.x -= movespeed
    IF keyval(scRight) > 0 THEN .pos.x += movespeed
    'Hero positions are the bottom center of the sprite
    .pos.x = bound(.pos.x, -500, gen(genResolutionX) + 500)
    .pos.y = bound(.pos.y, -500, gen(genResolutionY) + 500)
   END WITH
  END IF
  IF positioning_mode = NO THEN
   '--menu mode
   IF keyval(scESC) > 1 THEN
    EXIT DO
   END IF
   IF keyval(scF1) > 1 THEN show_help "hero_formation_editor"
   usemenu state
   slot = state.pt - first_hero_item
   IF slot < 0 THEN slot = -1

   IF enter_space_click(state) THEN
    IF state.pt = 0 THEN
     EXIT DO
    END IF
    IF slot <> -1 THEN 'a hero slot
     positioning_mode = YES
    END IF
   END IF
   IF slot <> -1 THEN
    IF keyval(scCtrl) > 0 ANDALSO keyval(scD) > 1 THEN
     'Revert to default
     hform.slots(slot).pos.x = default_hform.slots(slot).pos.x
     hform.slots(slot).pos.y = default_hform.slots(slot).pos.y
    END IF
   END IF
   IF state.pt = 2 THEN
    IF intgrabber(test_form_id, 0, gen(genMaxFormation)) THEN
     'Test with a different enemy formation
     LoadFormation eform, test_form_id
     load_formation_slices ename(), eform, @rootslice
     bgwait = 0
     bgctr = 0
    END IF
   END IF
   IF state.pt = 1 THEN '---SELECT A DIFFERENT HERO FORMATION
    DIM as integer remember_id = hero_form_id
    IF intgrabber_with_addset(hero_form_id, 0, last_hero_formation_id(), 32767, "hero formation") THEN
     save_hero_formation hform, remember_id
     load_hero_formation hform, hero_form_id
     save_hero_formation hform, hero_form_id
    END IF
   END IF'--DONE SELECTING DIFFERENT HERO FORMATION
  END IF

  ' Draw screen

  IF eform.background_frames > 1 AND eform.background_ticks > 0 THEN
   bgwait = (bgwait + 1) MOD eform.background_ticks   'FIXME: off-by-one bug here
   IF bgwait = 0 THEN
    bgctr = loopvar(bgctr, 0, eform.background_frames - 1, 1)
    DIM sl as Slice ptr = LookupSlice(SL_FORMEDITOR_BACKDROP, rootslice)
    ChangeSpriteSlice sl, , (eform.background + bgctr) MOD gen(genNumBackdrops)
   END IF
  END IF
  draw_formation_slices eform, hform, rootslice, slot, dpage, YES

  IF positioning_mode = NO THEN
   menu(0) = "Previous Menu"
   menu(1) = CHR(27) + "Hero Formation " & hero_form_id & CHR(26)
   menu(2) = "Preview Enemy Formation: " & test_form_id
   FOR i as integer = 0 TO 3
    menu(first_hero_item + i) = "Hero Slot " & i & "(x=" & hform.slots(i).pos.x & " y=" & hform.slots(i).pos.y & ")"
   NEXT i
   standardmenu menu(), state, 0, 0, dpage, menuopts
  END IF
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP

 save_hero_formation hform, hero_form_id
 DeleteSlice @rootslice
END SUB

SUB individual_formation_editor ()
 DIM form_id as integer = 0
 DIM form as Formation
 DIM ename(7) as string
 DIM rootslice as Slice ptr
 DIM as integer i
 DIM positioning_mode as bool = NO
 DIM as integer bgwait, bgctr

 LoadFormation form, form_id
 load_formation_slices ename(), form, @rootslice

 DIM menu(16) as string
 DIM state as MenuState
 state.pt = 0
 state.top = 0
 state.first = 0
 state.last = UBOUND(menu)
 state.size = 20
 DIM menuopts as MenuOptions
 menuopts.edged = YES

 CONST first_enemy_item = 9
 'slot -1 indicates no enemy selected
 DIM slot as integer = state.pt - first_enemy_item
 IF slot < 0 THEN slot = -1

 setkeys
 DO
  setwait 55
  setkeys
  IF positioning_mode = YES THEN
   '--enemy positioning mode
   IF keyval(scESC) > 1 OR enter_or_space() THEN setkeys: positioning_mode = NO
   IF keyval(scF1) > 1 THEN show_help "formation_editor_placement"
   DIM as integer movespeed = 1
   IF keyval(scShift) THEN movespeed = 8
   WITH form.slots(slot)
    DIM sprite as Slice ptr = LookupSlice(SL_FORMEDITOR_ENEMY + slot, rootslice)
    DIM size as XYPair
    IF sprite THEN size = SliceSize(sprite)
    ' Note that enemy positions are the top-left corner of the sprite
    ' (which needs to be changed)
    IF keyval(scUp) > 0 THEN .pos.y -= movespeed
    IF keyval(scDown) > 0 THEN .pos.y += movespeed
    IF keyval(scLeft) > 0 THEN .pos.x -= movespeed
    IF keyval(scRight) > 0 THEN .pos.x += movespeed
    ' FIXME: battles are still stuck at 320x200 for the moment, but switch to this later
    ' .pos.x = bound(.pos.x, -size.w\2, gen(genResolutionX) - size.w\2)
    ' .pos.y = bound(.pos.y, -size.h\2, gen(genResolutionY) - size.h\2)
    .pos.x = bound(.pos.x, -size.w\2, 320 - size.w\2)
    .pos.y = bound(.pos.y, -size.h\2, 200 - size.h\2)
   END WITH
  END IF
  IF positioning_mode = NO THEN
   '--menu mode
   IF keyval(scESC) > 1 THEN
    EXIT DO
   END IF
   IF keyval(scF1) > 1 THEN show_help "formation_editor"
   IF cropafter_keycombo(state.pt = 1) THEN cropafter form_id, gen(genMaxFormation), 0, game + ".for", 80
   usemenu state
   slot = state.pt - first_enemy_item
   IF slot < 0 THEN slot = -1

   IF enter_space_click(state) THEN
    IF state.pt = 0 THEN
     EXIT DO
    END IF
    IF state.pt = 5 THEN
     IF form.music >= 0 THEN playsongnum form.music
    END IF
    IF slot <> -1 THEN 'an enemy
     IF form.slots(slot).id >= 0 THEN positioning_mode = YES
    END IF
   END IF
   IF state.pt = 2 THEN
    IF intgrabber(form.background, 0, gen(genNumBackdrops) - 1) THEN
     bgwait = 0
     bgctr = 0
     load_formation_slices ename(), form, @rootslice
    END IF
   END IF
   IF state.pt = 3 THEN
    'IF intgrabber(form.background_frames, 1, 50) THEN
    DIM temp as integer = form.background_frames - 1
    IF xintgrabber(temp, 2, 50) THEN
     IF form.background_frames = 1 THEN form.background_ticks = 8  'default to 8 ticks because 1 tick can be really painful
     form.background_frames = temp + 1
     IF bgctr >= form.background_frames THEN
      bgctr = 0
      load_formation_slices ename(), form, @rootslice
     END IF
    END IF
   END IF
   IF state.pt = 4 THEN
    IF intgrabber(form.background_ticks, 0, 1000) THEN
     bgwait = 0
    END IF
   END IF
   IF state.pt = 5 THEN
    IF intgrabber(form.music, -2, gen(genMaxSong)) THEN
     music_stop
    END IF
   END IF
   IF state.pt = 6 THEN
    tag_set_grabber(form.victory_tag, state)
   END IF
   IF state.pt = 7 THEN
    intgrabber(form.death_action, -1, 0)
   END IF
   IF state.pt = 8 THEN
    intgrabber(form.hero_form, 0, last_hero_formation_id())
   END IF
   IF state.pt = 1 THEN '---SELECT A DIFFERENT FORMATION
    DIM as integer remember_id = form_id
    IF intgrabber_with_addset(form_id, 0, gen(genMaxFormation), 32767, "formation") THEN
     SaveFormation form, remember_id
     IF form_id > gen(genMaxFormation) THEN
      gen(genMaxFormation) = form_id
      ClearFormation form
      form.music = gen(genBatMus) - 1
      SaveFormation form, form_id
     END IF
     LoadFormation form, form_id
     load_formation_slices ename(), form, @rootslice
     bgwait = 0
     bgctr = 0
    END IF
   END IF'--DONE SELECTING DIFFERENT FORMATION
   IF slot <> -1 THEN
    WITH form.slots(slot)
     DIM oldenemy as integer = .id
     IF form.slots(slot).id >= 0 AND enter_space_click(state) THEN
      'Pressing enter should go to placement mode (handled above)
     ELSEIF enemygrabber(.id, state, 0, -1) THEN
      'This would treat the x/y position as being the bottom middle of enemies, which makes much more
      'sense, but that would change where enemies of different sizes are spawned in slots in existing games
      'See the Plan for battle formation improvements
      '.pos.x += w(slot) \ 2
      '.pos.y += h(slot)
      load_formation_slices ename(), form, @rootslice
      'default to middle of field
      IF oldenemy = -1 AND .pos.x = 0 AND .pos.y = 0 THEN
       .pos.x = 70
       .pos.y = 95
      END IF
      '.pos.x -= w(slot) \ 2
      '.pos.y -= h(slot)
     END IF
    END WITH
   END IF
  END IF

  ' Draw screen

  IF form.background_frames > 1 AND form.background_ticks > 0 THEN
   bgwait = (bgwait + 1) MOD form.background_ticks
   IF bgwait = 0 THEN
    bgctr = loopvar(bgctr, 0, form.background_frames - 1, 1)
    DIM sl as Slice ptr = LookupSlice(SL_FORMEDITOR_BACKDROP, rootslice)
    ChangeSpriteSlice sl, , (form.background + bgctr) MOD gen(genNumBackdrops)
   END IF
  END IF
  draw_formation_slices form, rootslice, slot, dpage

  IF positioning_mode = NO THEN
   menu(0) = "Previous Menu"
   menu(1) = CHR(27) + "Formation " & form_id & CHR(26)
   menu(2) = "Backdrop: " & form.background
   IF form.background_frames <= 1 THEN
    menu(3) = "Backdrop Animation: none"
    menu(4) = " Ticks per Backdrop Frame: -NA-"
   ELSE
    menu(3) = "Backdrop Animation: " & form.background_frames & " frames"
    menu(4) = " Ticks per Backdrop Frame: " & form.background_ticks
   END IF
   menu(5) = "Battle Music:"
   IF form.music = -2 THEN
     menu(5) &= " -same music as map-"
   ELSEIF form.music = -1 THEN
     menu(5) &= " -silence-"
   ELSEIF form.music >= 0 THEN
     menu(5) &= " " & form.music & " " & getsongname(form.music)
   END IF
   menu(6) = "Victory Tag: " & tag_choice_caption(form.victory_tag)
   menu(7) = "On Death: "
   IF form.death_action = 0 THEN
    menu(7) &= "gameover/death script"
   ELSEIF form.death_action = -1 THEN
    menu(7) &= "continue game"
   END IF
   menu(8) = "Hero Formation: " & form.hero_form

   FOR i as integer = 0 TO 7
    menu(first_enemy_item + i) = "Enemy:" + ename(i)
   NEXT i
   standardmenu menu(), state, 0, 0, dpage, menuopts
  END IF
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP

 SaveFormation form, form_id
 music_stop
 DeleteSlice @rootslice
END SUB

'Deletes previous rootslice if any, then creates a bunch of sprite slices for enemies
'(but doesn't position them: that's done in draw_formation_slices), and rectangles for
'hero positions.
'Also loads enemy names.
SUB load_formation_slices(ename() as string, form as Formation, rootslice as Slice ptr ptr)
 DIM sl as Slice ptr
 DeleteSlice rootslice

 ' Root is backdrop
 *rootslice = NewSliceOfType(slSprite)
 sl = *rootslice
 ChangeSpriteSlice sl, sprTypeBackdrop, form.background 
 sl->Lookup = SL_FORMEDITOR_BACKDROP
 
 'Hero Area
 DIM h_area as Slice Ptr
 h_area = NewSliceOfType(slContainer, *rootslice)
 WITH *(h_area)
  .Lookup = SL_FORMEDITOR_HERO_AREA
  .X = 240
  .Y = 82
  .Width = 56
  .Height = 100
 END WITH
 ' Heroes
 FOR i as integer = 0 TO 3
  sl = NewSliceOfType(slRectangle, h_area)
  sl->Lookup = SL_FORMEDITOR_HERO + i
  ChangeRectangleSlice sl, , boxlook(0).bgcol, boxlook(0).edgecol
  sl->AnchorHoriz = 1
  sl->AnchorVert = 2
  sl->X = i * 8 + 16 'overridden by hero formation
  sl->Y = i * 20 + 40 'overridden by hero formation
  sl->Width = 32
  sl->Height = 40
  'Break ties with heroes behind
  sl->Sorter = i
 NEXT

 ' Enemies
 FOR i as integer = 0 TO 7
  ename(i) = "-EMPTY-"
  IF form.slots(i).id >= 0 THEN
   DIM enemy as EnemyDef
   loadenemydata enemy, form.slots(i).id
   WITH enemy
    ename(i) = form.slots(i).id & ":" & .name
    sl = NewSliceOfType(slSprite, *rootslice)
    ChangeSpriteSlice sl, sprTypeSmallEnemy + bound(.size, 0, 2), .pic, .pal
    sl->Lookup = SL_FORMEDITOR_ENEMY + i
    sl->Sorter = 100 + i
   END WITH
  END IF
 NEXT i

 ' Cursor (defaults to invisible)
 sl = NewSliceOfType(slText, *rootslice)
 sl->AlignHoriz = 1  'mid
 sl->AnchorHoriz = 1  'mid
 sl->Lookup = SL_FORMEDITOR_CURSOR
END SUB

SUB draw_formation_slices(eform as Formation, rootslice as Slice ptr, selected_slot as integer, page as integer)
 DIM hform as HeroFormation
 load_hero_formation hform, eform.hero_form
 draw_formation_slices eform, hform, rootslice, selected_slot, page, NO
END SUB

SUB draw_formation_slices(eform as Formation, hform as HeroFormation, rootslice as Slice ptr, selected_slot as integer, page as integer, byval heromode as bool=NO)
 STATIC flash as integer
 flash = (flash + 1) MOD 256
 DIM cursorsl as Slice ptr = LookupSlice(SL_FORMEDITOR_CURSOR, rootslice)
 cursorsl->Visible = NO

 ' Set enemy positions (and maybe parent of cursor slice)
 DIM sl as Slice ptr = rootslice->FirstChild
 WHILE sl
  IF sl->Lookup >= SL_FORMEDITOR_ENEMY AND sl->Lookup <= SL_FORMEDITOR_LAST_ENEMY THEN
   'Is an enemy
   DIM enemy_slot as integer = sl->Lookup - SL_FORMEDITOR_ENEMY
   DIM fslot as FormationSlot ptr = @eform.slots(enemy_slot)
   IF fslot->id < 0 THEN debugc errPromptBug, "Formation enemy slice corresponds to an empty slot"
   sl->X = fslot->pos.x
   sl->Y = fslot->pos.y
   IF NOT heromode THEN
    IF enemy_slot = selected_slot AND cursorsl <> NULL THEN
     cursorsl->Visible = YES
     SetSliceParent cursorsl, sl
     ChangeTextSlice cursorsl, CHR(25), flash
    END IF
   END IF
  END IF
  sl = sl->NextSibling
 WEND
 
 ' Set hero positions (and maybe parent of cursor slice)
 DIM h_area as Slice ptr = LookupSlice(SL_FORMEDITOR_HERO_AREA, rootslice)
 DIM hrect as Slice Ptr
 FOR i as integer = 0 TO 3
  hrect = LookupSlice(SL_FORMEDITOR_HERO + i, h_area)
  hrect->X = hform.slots(i).pos.x
  hrect->Y = hform.slots(i).pos.y
  IF heromode THEN
   IF i = selected_slot AND cursorsl <> NULL THEN
    cursorsl->Visible = YES
    SetSliceParent cursorsl, hrect
    ChangeTextSlice cursorsl, CHR(25), flash
   END IF
  END IF
 NEXT i

 ' We don't want path-dependence of the sort order, because
 ' the real battle system display breaks ties in a certain way.
 ' This ensures ties broken in the same way (I hope)
 CustomSortChildSlices rootslice, NO  'wipevals=NO
 EdgeYSortChildSlices rootslice, 2  'edge=2 is bottom edge
 clearpage page
 DrawSlice rootslice, page
END SUB
