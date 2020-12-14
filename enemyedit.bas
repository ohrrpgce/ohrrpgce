'OHRRPGCE CUSTOM - Enemy Editor
'(C) Copyright 1997-2018 James Paige and Hamster Republic Productions
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
#include "bcommon.bi"

#include "uiconst.bi"
#include "scrconst.bi"

#include "flexmenu.bi"

'Defined in this file:

DECLARE FUNCTION enemy_edit_add_new (recbuf() as integer, preview_box as Slice ptr) as bool
DECLARE SUB enemy_edit_update_menu(byval recindex as integer, state as MenuState, recbuf() as integer, menu() as string, menuoff() as integer, menutype() as integer, menulimits() as integer, min() as integer, max() as integer, dispmenu() as string, workmenu() as integer, caption() as string, menucapoff() as integer, preview as Slice Ptr, rewards_preview as string, byval EnLimSpawn as integer, byval EnLimAtk as integer, byval EnLimPic as integer)
DECLARE FUNCTION start_preview_dissolve(preview as Slice ptr, dissolve_type as integer, dissolve_time as integer, dissolve_backwards as bool) as integer
DECLARE FUNCTION readenemyname OVERLOAD(recbuf() as integer) as string
DECLARE FUNCTION describe_item_chance(chance as ItemChance) as string
DECLARE SUB enemy_edit_load(byval recnum as integer, recbuf() as integer, state as MenuState, caption() as string, byval EnCapElemResist as integer)
DECLARE SUB enemy_edit_pushmenu (state as MenuState, byref lastptr as integer, byref lasttop as integer, byref menudepth as bool)
DECLARE SUB enemy_edit_backmenu (state as MenuState, byval lastptr as integer, byval lasttop as integer, byref menudepth as bool, workmenu() as integer, mainMenu() as integer)
DECLARE SUB enemy_usage_menu(byref enemyid as integer)

DIM SHARED remem_enemy_index as integer = -1   'Record to switch to with TAB
DIM SHARED show_name_ticks as integer = 0  'Number of ticks to show name (after switching record with TAB)


'Returns true if should swap remem_enemy_index and recindex
FUNCTION handle_tab_to_switch_enemies(recindex as integer) as bool
 IF keyval(scTab) > 1 THEN
  IF keyval(scShift) > 0 THEN
   remem_enemy_index = recindex
  ELSEIF remem_enemy_index >= 0 ANDALSO remem_enemy_index <= gen(genMaxEnemy) THEN
   show_name_ticks = 23
   RETURN YES
  END IF
 END IF
END FUNCTION

SUB update_enemy_editor_for_elementals(recbuf() as integer, caption() as string, byval EnCapElemResist as integer)
 FOR i as integer = 0 TO gen(genNumElements) - 1
  caption(EnCapElemResist + i) = format_percent(DeSerSingle(recbuf(), 239 + i*2))
 NEXT
END SUB

SUB enemy_editor_main ()
 IF read_config_bool("thingbrowser.enable_top_level", YES) THEN
  DIM b as EnemyBrowser
  b.browse(-1, , @enemy_editor)
 ELSE
  enemy_editor 0
 END IF
END SUB

FUNCTION enemy_picker (recindex as integer = -1) as integer
 DIM b as EnemyBrowser
 RETURN b.browse(recindex, , @enemy_editor, NO)
END FUNCTION

FUNCTION enemy_picker_or_none (recindex as integer = -1) as integer
 DIM b as EnemyBrowser
 RETURN b.browse(recindex - 1, YES , @enemy_editor, NO) + 1
END FUNCTION

CONST EnLimDeathSFX = 26

'recindex: which enemy to show. If -1, same as last time. If >= max, ask to add a new enemy,
'(and exit and return -1 if cancelled).
'Otherwise, returns the enemy number we were last editing.
'Note: the enemy editor can be entered recursively!
FUNCTION enemy_editor (recindex as integer = -1) as integer

DIM elementnames() as string
getelementnames elementnames()

'-------------------------------------------------------------------------

'--bitsets
DIM enemybits() as IntStrPair

a_append enemybits(), -1, ""
a_append enemybits(), -1, " Appearance"
a_append enemybits(), 59, "Flee instead of die"
a_append enemybits(), 63, "Never flinch when attacked"

a_append enemybits(), -1, ""
a_append enemybits(), -1, " Behavior"
a_append enemybits(), 55, "MP idiot"
a_append enemybits(), 58, "Die without boss"

a_append enemybits(), -1, ""
a_append enemybits(), -1, " Type"
a_append enemybits(), 64, "Ignored for ""Alone"" AI"
a_append enemybits(), 56, "Boss"
a_append enemybits(), 57, "Unescapable"
a_append enemybits(), 62, "Win battle even if alive"
a_append enemybits(), 61, "Untargetable by heroes & win even if alive"
a_append enemybits(), 60, "Untargetable by enemies"

a_append enemybits(), -1, ""
a_append enemybits(), -1, " Other"
a_append enemybits(), 54, "Harmed by cure"

a_append enemybits(), -1, "##"
a_append enemybits(), -1, "## Obsolete"
FOR i as integer = 0 TO 7
 DIM elname as string
 elname = IIF(i <= UBOUND(elementnames), elementnames(i), "element" & i)
 a_append enemybits(), i + 0,  "##Weak against " & elname & " (obsolete)"
 a_append enemybits(), i + 8,  "##Strong against " & elname & " (obsolete)"
 a_append enemybits(), i + 16, "##Absorbs " & elname & " (obsolete)"
 a_append enemybits(), i + 24, "##Enemy type " & readglobalstring(9 + i, "EnemyType" & i+1) & " (obsolete)"
NEXT


'-------------------------------------------------------------------------

'--record buffer
DIM recbuf(dimbinsize(binDT1)) as integer

'Copy/paste buffer
STATIC copy_recbuf(dimbinsize(binDT1)) as integer
STATIC have_copy as bool

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
CONST EnDatDissolveIn = 369
CONST EnDatDissolveInTime = 370
CONST EnDatSpawnAllElementsOnHit = 371

'-------------------------------------------------------------------------

DIM capindex as integer = 0
REDIM caption(-1 TO -1) as string
DIM max(29) as integer
DIM min(29) as integer
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
 max(EnLimStat + i) = 32767
NEXT

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

min(EnLimDeathSFX) = -1
max(EnLimDeathSFX) = gen(genMaxSFX) + 1

DIM EnCapElemResist as integer = capindex
FOR i as integer = 0 TO gen(genNumElements) - 1
 addcaption caption(), capindex, ""  '--updated in update_enemy_editor_for_elementals
NEXT

CONST EnLimDissolveIn = 27
min(EnLimDissolveIn) = 0
max(EnLimDissolveIn) = dissolveTypeMax + 1
DIM EnCapDissolveIn as integer = capindex
addcaption caption(), capindex, "Appear Instantly"
FOR i as integer = 0 TO dissolveTypeMax
 addcaption caption(), capindex, appear_type_caption(i)
NEXT

CONST EnLimDissolveInTime = 28
min(EnLimDissolveInTime) = 0
max(EnLimDissolveInTime) = 99

CONST EnLimSpawnAllElementsOnHit = 29
min(EnLimSpawnAllElementsOnHit) = 0
max(EnLimSpawnAllElementsOnHit) = 1
DIM EnCapSpawnAllElementsOnHit as integer = capindex
addcaption caption(), capindex, "Only first matching element"
addcaption caption(), capindex, "All of the above"

'--next limit 30, remember to update min() and max() dim!

'-------------------------------------------------------------------------
'--menu content
CONST MnuItems = 270
DIM menu(MnuItems) as string
DIM menutype(MnuItems) as integer
DIM menuoff(MnuItems) as integer
DIM menulimits(MnuItems) as integer
DIM menucapoff(MnuItems) as integer

CONST EnMenuInvalid = -1

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
menu(EnMenuGold) = money_name() & ":"
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
 menutype(EnMenuStat + i) = 8000 + i  'stat value
 menuoff(EnMenuStat + i) = EnDatStat + i
 menulimits(EnMenuStat + i) = EnLimStat + i
NEXT i

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
 menu(EnMenuElemDmg + i) = "Damage from " + rpad(elementnames(i), " ", 15, clipRight) + ":"
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

CONST EnMenuDissolveIn = 266
menu(EnMenuDissolveIn) = "Appear Animation:"
menutype(EnMenuDissolveIn) = 2000 + EnCapDissolveIn
menuoff(EnMenuDissolveIn) = EnDatDissolveIn
menulimits(EnMenuDissolveIn) = EnLimDissolveIn

CONST EnMenuDissolveInTime = 267
menu(EnMenuDissolveInTime) = "Appear Animation ticks:"
menutype(EnMenuDissolveInTime) = 13
menuoff(EnMenuDissolveInTime) = EnDatDissolveInTime
menulimits(EnMenuDissolveInTime) = EnLimDissolveInTime

CONST EnMenuSpawnAllElementsOnHit = 268
menu(EnMenuSpawnAllElementsOnHit) = "on Multi-Element Hit:"
menutype(EnMenuSpawnAllElementsOnHit) = 2000 + EnCapSpawnAllElementsOnHit
menuoff(EnMenuSpawnAllElementsOnHit) = EnDatSpawnAllElementsOnHit
menulimits(EnMenuSpawnAllElementsOnHit) = EnLimSpawnAllElementsOnHit

CONST EnMenuUsageAct = 269
menu(EnMenuUsageAct) = "Usage..."
menutype(EnMenuUsageAct) = 1

CONST EnMenuPreviewBackdrop = 270
menu(EnMenuPreviewBackdrop) = "Preview Background..."
menutype(EnMenuPreviewBackdrop) = 1

'Next is 271, don't forget to update MnuItems

'-------------------------------------------------------------------------
'--menu structure
'WARNING: make these big enough to hold atkMenu when genNumElements is maxed out
DIM workmenu(93) as integer
DIM dispmenu(93) as string
DIM state as MenuState
state.autosize = YES
state.autosize_ignore_pixels = 12
state.need_update = YES
DIM menuopts as MenuOptions
menuopts.fullscreen_scrollbar = YES

DIM mainMenu(10) as integer
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
mainMenu(10) = EnMenuUsageAct

DIM appearMenu(10) as integer
appearMenu(0) = EnMenuBackAct
appearMenu(1) = EnMenuPicSize
appearMenu(2) = EnMenuPic
appearMenu(3) = EnMenuPal
appearMenu(4) = EnMenuDissolve
appearMenu(5) = EnMenuDissolveTime
appearMenu(6) = EnMenuDissolveIn
appearMenu(7) = EnMenuDissolveInTime
appearMenu(8) = EnMenuDeathSFX
appearMenu(9) = EnMenuCursorOffset
appearMenu(10) = EnMenuPreviewBackdrop

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

DIM spawnMenu(6 + gen(genNumElements)) as integer
spawnMenu(0) = EnMenuBackAct
spawnMenu(1) = EnMenuSpawnNum
spawnMenu(2) = EnMenuSpawnDeath
spawnMenu(3) = EnMenuSpawnNEDeath
spawnMenu(4) = EnMenuSpawnAlone
spawnMenu(5) = EnMenuSpawnNEHit
FOR i as integer = 0 TO gen(genNumElements) - 1
 spawnMenu(6 + i) = EnMenuSpawnElement + i
NEXT i
spawnMenu(6 + gen(genNumElements)) = EnMenuSpawnAllElementsOnHit

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
 .AnchorHoriz = alignRight
 .AlignHoriz = alignRight
 .AnchorVert = alignBottom
 .AlignVert = alignBottom
END WITH

DIM clip_box as Slice Ptr
clip_box = NewSliceOfType(slContainer, preview_box)
clip_box->Clip = YES
clip_box->Fill = YES

'The preview backdrop + 1, or none if 0
DIM byref preview_backdrop_id as integer = gen(genPreviewBackdrop)
preview_backdrop_id = small(preview_backdrop_id, gen(genNumBackdrops) - 1)
DIM preview_backdrop as Slice Ptr
preview_backdrop = NewSliceOfType(slSprite, clip_box)
preview_backdrop->Visible = (preview_backdrop_id > 0)
IF preview_backdrop_id > 0 THEN
 ChangeSpriteSlice preview_backdrop, sprTypeBackdrop, preview_backdrop_id - 1
END IF
RealignSlice preview_backdrop, alignCenter, alignCenter, alignCenter, alignCenter

'--Create the preview sprite. It will be updated before it is drawn.
DIM preview as Slice Ptr
preview = NewSliceOfType(slSprite, preview_box)
'--Align the sprite to the bottom center of the containing box
WITH *preview
 .Lookup = SL_EDITOR_ENEMY_SPRITE
 .Y = -1
 .AnchorHoriz = alignCenter
 .AlignHoriz = alignCenter
 .AnchorVert = alignBottom
 .AlignVert = alignBottom
END WITH

DIM rewards_preview as string

'--dissolve_ticks is >= 0 and <= dissolve_stop_at while playing a dissolve;
'--continues past > dissolve_time during lag period afterwards.
DIM as integer dissolve_ticks, dissolve_stop_at
dissolve_ticks = -1

'--default starting menu
setactivemenu workmenu(), mainMenu(), state
state.pt = 1  'Select <-Enemy ..-> line
state.size = 25

DIM menudepth as bool = NO
DIM lastptr as integer = 0
DIM lasttop as integer = 0

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
   recindex = gen(genMaxEnemy)
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
 IF keyval(ccCancel) > 1 THEN
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
  cropafter recindex, gen(genMaxEnemy), game + ".dt1", getbinsize(binDT1)
 END IF

 'Copy-paste
 IF workmenu(state.pt) = EnMenuChooseAct THEN
  IF copy_keychord() THEN
   a_copy recbuf(), copy_recbuf()
   have_copy = YES
   show_overlay_message "Copied enemy", 1.2
  END IF
  IF have_copy ANDALSO paste_keychord() THEN
   IF yesno("Really overwrite this enemy by pasting " & readenemyname(copy_recbuf()) & "?") THEN
    a_copy copy_recbuf(), recbuf()
    state.need_update = YES
   END IF
  END IF
 END IF

 usemenu state

 'Change record (possibly anywhere if holding Alt)
 IF workmenu(state.pt) = EnMenuChooseAct OR (keyval(scAlt) > 0 and NOT isStringField(menutype(workmenu(state.pt)))) THEN
  DIM lastindex as integer = recindex
  IF intgrabber_with_addset(recindex, 0, gen(genMaxEnemy), 32767, "enemy") THEN
   saveenemydata recbuf(), lastindex
   IF recindex > gen(genMaxEnemy) THEN
    '--adding a new set
    IF enemy_edit_add_new(recbuf(), preview_box) THEN
     'Added a new record (blank or copy)
     saveenemydata recbuf(), recindex
     enemy_edit_load recindex, recbuf(), state, caption(), EnCapElemResist
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

 'Shift-Tab to remember index, Tab to swap to remembered index
 IF handle_tab_to_switch_enemies(recindex) THEN
  saveenemydata recbuf(), recindex
  SWAP remem_enemy_index, recindex
  enemy_edit_load recindex, recbuf(), state, caption(), EnCapElemResist
 END IF

 DIM nowindex as integer = EnMenuInvalid
 IF enter_space_click(state) THEN
  nowindex = workmenu(state.pt)
  SELECT CASE menutype(nowindex)
   CASE 8 ' Item
    recbuf(menuoff(nowindex)) = item_picker(recbuf(menuoff(nowindex)))
    max(EnLimItem) = gen(genMaxItem)
    state.need_update = YES
   CASE 10 ' Item with offset
    recbuf(menuoff(nowindex)) = item_picker_or_none(recbuf(menuoff(nowindex)))
    max(EnLimItem) = gen(genMaxItem)
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
   CASE EnMenuUsageAct
    'Save before-hand because we might enter other editor or recursively enter the enemy editor
    '(c.f. flexmenu_handle_crossrefs)
    saveenemydata recbuf(), recindex
    enemy_usage_menu recindex
    'recindex may have been changed, but we need to reload even if it wasn't, because might
    'have recursively edited this enemy
    enemy_edit_load recindex, recbuf(), state, caption(), EnCapElemResist
    state.need_update = YES
   CASE EnMenuPic
    DIM sprtype as SpriteType = recbuf(EnDatPicSize) + sprTypeSmallEnemy
    DIM enemyb as EnemySpriteBrowser = EnemySpriteBrowser(sprtype)
    recbuf(EnDatPic) = enemyb.browse(recbuf(EnDatPic))
    state.need_update = YES
   CASE EnMenuPal
    DIM sprtype as SpriteType = recbuf(EnDatPicSize) + sprTypeSmallEnemy
    recbuf(EnDatPal) = pal16browse(recbuf(EnDatPal), sprtype, recbuf(EnDatPic), YES)
    state.need_update = YES
   CASE EnMenuDeathSFX
    DIM old_sfx as integer = recbuf(EnDatDeathSFX)
    recbuf(EnDatDeathSFX) = sfx_picker_or_none(old_sfx)
    state.need_update = (recbuf(EnDatDeathSFX) <> old_sfx)
    IF recbuf(EnDatDeathSFX) = 0 THEN playsfx gen(genDefaultDeathSFX) - 1
   CASE EnMenuBitsetAct
    editbitset recbuf(), EnDatBitset, enemybits(), "enemy_bitsets", remember_bit
   CASE EnMenuCursorOffset
    'The offset is relative to the top-center edge, and horiz-flipped, because
    'originally the sprite was shown flipped.
    recbuf(EnDatCursorX) *= -1
    recbuf(EnDatCursorX) += preview->Size.x \ 2
    xy_position_on_sprite_slice preview, recbuf(EnDatCursorX), recbuf(EnDatCursorY), "Targetting Cursor Offset", "xy_target_cursor"
    recbuf(EnDatCursorX) -= preview->Size.x \ 2
    recbuf(EnDatCursorX) *= -1
   CASE EnMenuPreviewBackdrop
    'Pick preview backdrop
    DIM backdropb as BackdropSpriteBrowser
    preview_backdrop_id = backdropb.browse(preview_backdrop_id - 1, YES) + 1  'or_none=YES
    preview_backdrop->Visible = (preview_backdrop_id > 0)
    IF preview_backdrop_id > 0 THEN
     ChangeSpriteSlice preview_backdrop, sprTypeBackdrop, preview_backdrop_id - 1
    END IF
  END SELECT
 END IF

 IF keyval(scAlt) = 0 or isStringField(menutype(workmenu(state.pt))) THEN 'not pressing ALT, or not allowed to
  IF editflexmenu(state, workmenu(state.pt), menutype(), menuoff(), menulimits(), recbuf(), caption(), min(), max()) THEN
   state.need_update = YES
  END IF
 END IF

 'Some menu item enter_space_click actions have to happen after editflexmenu
 SELECT CASE nowindex  'Set if enter_space_click(state)
  CASE EnMenuDissolve, EnMenuDissolveTime
   DIM dissolve_type as integer = IIF(recbuf(EnDatDissolve), recbuf(EnDatDissolve) - 1, gen(genEnemyDissolve))
   dissolve_stop_at = start_preview_dissolve(preview, dissolve_type, recbuf(EnDatDissolveTime), NO)
   dissolve_ticks = 0
  CASE EnMenuDissolveIn, EnMenuDissolveInTime
   IF recbuf(EnDatDissolveIn) > 0 THEN  'Not 'Appears Instantly'
    dissolve_stop_at = start_preview_dissolve(preview, recbuf(EnDatDissolveIn) - 1, recbuf(EnDatDissolveInTime), YES)
    dissolve_ticks = 0
   END IF
 END SELECT

 'Debug key CTRL-B: edit all bitsets
 IF keyval(scCtrl) > 0 AND keyval(scB) > 1 THEN
  edit_all_bits recbuf(), EnDatBitset, enemybits(), 80, "enemy_bitsets", remember_bit
 END IF

 IF flexmenu_handle_crossrefs(state, workmenu(state.pt), menutype(), menuoff(), recindex, recbuf(), NO) THEN
  'Reload this enemy in case it was changed in recursive call to the editor (in fact, this record might be deleted!)
  recindex = small(recindex, gen(genMaxEnemy))
  enemy_edit_load recindex, recbuf(), state, caption(), EnCapElemResist
  show_name_ticks = 23
  state.need_update = YES
 END IF

 IF dissolve_ticks >= 0 THEN
  'Sprite would otherwise reappear instantly when the dissolve ends
  preview->Visible = SpriteSliceIsDissolving(preview)
  dissolve_ticks += 1
  IF dissolve_ticks >= dissolve_stop_at THEN
   dissolve_ticks = -1
   CancelSpriteSliceDissolve preview
  END IF
 ELSE
  preview->Visible = YES
 END IF

 IF state.need_update THEN
  state.need_update = NO
  enemy_edit_update_menu recindex, state, recbuf(), menu(), menuoff(), menutype(), menulimits(), min(), max(), dispmenu(), workmenu(), caption(), menucapoff(), preview, rewards_preview, EnLimSpawn, EnLimAtk, EnLimPic
 END IF

 clearpage vpage
 IF drawpreview THEN
  DrawSlice preview_box, vpage
 END IF

 IF helpkey = "enemy_rewards" THEN
  'Preview drops and steal chances
  wrapprint rewards_preview, 0, 112, uilook(uiMenuItem), vpage, , , fontPlain
 END IF

 standardmenu dispmenu(), state, 0, 0, vpage, menuopts
 draw_fullscreen_scrollbar state, , vpage
 IF keyval(scAlt) > 0 OR show_name_ticks > 0 THEN 'holding ALT or just pressed TAB
  show_name_ticks = large(0, show_name_ticks - 1)
  DIM tmpstr as string = readenemyname(recbuf()) & " " & recindex
  textcolor uilook(uiText), uilook(uiHighlight)
  printstr tmpstr, pRight, 0, vpage
 END IF
 nowindex = workmenu(state.pt)
 edgeprint flexmenu_tooltip(menutype(nowindex), recbuf(menuoff(nowindex))), pLeft, pBottom, uilook(uiDisabledItem), vpage

 setvispage vpage
 dowait
LOOP

'--save what we were last working on
saveenemydata recbuf(), recindex

resetsfx
DeleteSlice @preview_box

remember_recindex = recindex
RETURN recindex

END FUNCTION

'There's another overload of this in common.rbas
FUNCTION readenemyname(recbuf() as integer) as string
 RETURN readbadbinstring(recbuf(), 0, 16, 0)
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

SUB enemy_edit_update_menu(byval recindex as integer, state as MenuState, recbuf() as integer, menu() as string, menuoff() as integer, menutype() as integer, menulimits() as integer, min() as integer, max() as integer, dispmenu() as string, workmenu() as integer, caption() as string, menucapoff() as integer, preview as Slice Ptr, rewards_preview as string, byval EnLimSpawn as integer, byval EnLimAtk as integer, byval EnLimPic as integer)

 DIM enemy as EnemyDef
 convertenemydata recbuf(), enemy

 '--in case new enemies/attacks have been added
 max(EnLimSpawn) = gen(genMaxEnemy) + 1
 max(EnLimAtk) = gen(genMaxAttack) + 1
 max(EnLimDeathSFX) = gen(genMaxSFX) + 1

 '--in case the PicSize has changed
 max(EnLimPic) = gen(genMaxEnemy1Pic + bound(enemy.size, 0, 2))
 
 '--re-enforce bounds, as they might have just changed
 enforceflexbounds menuoff(), menutype(), menulimits(), recbuf(), min(), max()
 
 updateflexmenu state.pt, dispmenu(), workmenu(), state.last, menu(), menutype(), menuoff(), menulimits(), recbuf(), caption(), max(), recindex, menucapoff()
 
 '--stop sounds
 resetsfx
 '--update the picture and palette preview (also resets if dissolved)
 ChangeSpriteSlice preview, sprTypeSmallEnemy + enemy.size, enemy.pic, enemy.pal, ,YES

 '--Rewards preview
 rewards_preview = !"Drop chances:\n" & describe_item_chance(enemy.reward)
 IF enemy.steal.thievability >= 0 THEN  'Not Disabled
  rewards_preview &= !"\nSteal results:\n" & describe_item_chance(enemy.steal)
 END IF
END SUB

FUNCTION start_preview_dissolve(preview as Slice ptr, dissolve_type as integer, dissolve_time as integer, dissolve_backwards as bool) as integer
 IF dissolve_time = 0 THEN dissolve_time = default_dissolve_time(dissolve_type, preview->Size.w, preview->Size.h)
 DissolveSpriteSlice preview, dissolve_type, dissolve_time, 0, dissolve_backwards, YES
 DIM stop_at as integer = dissolve_time
 'Lag time after fading out, to give a more realistic preview
 IF NOT dissolve_backwards THEN stop_at += 12
 RETURN stop_at
END FUNCTION

'Return a string describing enemy drops or stealable item chances
FUNCTION describe_item_chance(chance as ItemChance) as string
 WITH chance
  DIM ret as string
  DIM rare_percent as double = (1 - .item_rate / 100) * .rare_item_rate
  DIM nothing_percent as double = 100 - .item_rate - rare_percent
  IF .item_rate > 0 THEN
   ret &= strprintf(" %3d   %%: %d ", .item_rate, .item) & readitemname(.item) & !"\n"
  END IF
  IF rare_percent > 0. THEN
   ret &= strprintf(" %6.2f%%: %d ", rare_percent, .rare_item) & readitemname(.rare_item) & !"\n"
  END IF
  IF nothing_percent THEN
   ret &= strprintf(!" %6.2f%%: nothing\n", nothing_percent)
  END IF
  RETURN ret
 END WITH
END FUNCTION

'Returns YES if a new record was added, or NO if cancelled.
'When YES, gen(genMaxEnemy) gets updated, and recbuf() will be populated with
'blank or cloned record, and unsaved! Previous contents are discarded.
'TODO: convert to generic_add_new
FUNCTION enemy_edit_add_new (recbuf() as integer, preview_box as Slice ptr) as bool
  DIM enemy as EnemyDef
  DIM menu(2) as string
  DIM enemytocopy as integer = 0
  DIM preview as Slice ptr = LookupSlice(SL_EDITOR_ENEMY_SPRITE, preview_box)
  DIM state as MenuState
  state.last = UBOUND(menu)
  state.size = 24
  state.pt = 1

  state.need_update = YES
  setkeys
  DO
    setwait 55
    setkeys
    IF keyval(ccCancel) > 1 THEN setkeys : RETURN NO 'cancel
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


'==============================================================================
'                              Enemy Usage Menu
'==============================================================================

TYPE EnemyUsageMenu EXTENDS ModularMenu
 enemyid as integer
 enemyname as string

 DECLARE SUB update()
 DECLARE FUNCTION check_spawn(eid as integer, enemy as EnemyDef, spawn as integer, description as string) as bool
 DECLARE SUB draw()
 DECLARE FUNCTION each_tick() as bool
END TYPE

'Count number of uses of each form set in a foemap
SUB tally_foemap(foemap as TileMap, occurrences() as integer, byref total_foe_tiles as integer)
 total_foe_tiles = 0
 FOR y as integer = 0 TO foemap.high - 1
  FOR x as integer = 0 TO foemap.wide - 1
   DIM foe as integer = readblock(foemap, x, y)
   occurrences(foe) += 1
   IF foe THEN total_foe_tiles += 1
  NEXT
 NEXT
END SUB

SUB EnemyUsageMenu.update()
 DIM enemy as EnemyDef
 loadenemydata enemy, this.enemyid
 this.enemyname = enemy.name
 this.title = "Uses of Enemy " & this.enemyid & " " & enemy.name

 DIM have_any as bool = NO

 add_item -2, , "Previous Menu"

 ' Scan for formations containing this enemy
 header " Formations"
 DIM form_counts(gen(genMaxFormation)) as integer  'Count of this enemy in each formation
 DIM form as Formation
 FOR formid as integer = 0 TO gen(genMaxFormation)
  LoadFormation form, formid
  DIM count as integer = 0
  FOR slotidx as integer = 0 TO UBOUND(form.slots)
   IF form.slots(slotidx).id = this.enemyid THEN count += 1
  NEXT
  IF count THEN
   have_any = YES
   add_item 0, formid, strprintf("%-3d: ", formid) & describe_formation(form)
   form_counts(formid) = count
  END IF
 NEXT
 IF have_any = NO THEN add_item -1, 0, "(None)"
 have_any = NO

 ' Scan formation sets
 header " Formation Sets"
 DIM formset_counts(maxMaxFormation) as integer  'Count of the enemy in the formations
 DIM formset_encounter_rate(maxMaxFormation) as double  'Rate of encounters per step
 DIM formset_avg_num(maxMaxFormation) as double  'Average number of the enemy in an encounter
 DIM formset as FormationSet
 FOR fsid as integer = 1 TO maxFormationSet
  LoadFormationSet formset, fsid
  DIM matching_forms as integer = 0  'Number of formations with this enemy
  DIM total_forms as integer = 0
  DIM count as integer = 0   'Total number of this enemy
  FOR slotidx as integer = 0 TO UBOUND(formset.formations)
   DIM form as integer = formset.formations(slotidx)
   IF form > -1 THEN
    count += form_counts(form)
    total_forms += 1
    IF form_counts(form) THEN matching_forms += 1
   END IF
  NEXT
  DIM enctr_rate as double = formset_freq_estimate(formset.frequency)
  IF count THEN
   have_any = YES
   DIM extra as string
   IF enctr_rate = 0 THEN extra = "  (Zero frequency!)"
   add_item 1, fsid, strprintf("%-3d: in %2d%% of formations (avg num %.1f)", _
                               fsid, CINT(100 * matching_forms / total_forms), count / total_forms) & extra
  END IF
  formset_encounter_rate(fsid) = enctr_rate
  formset_avg_num(fsid) = count / total_forms
  formset_counts(fsid) = count
 NEXT
 IF have_any = NO THEN add_item -1, 0, "(None)"
 have_any = NO

 header " Maps (foemaps)"
 add_item , , "Map |  % of  |   % of    | Chance to see | Avg num  | Form", NO, NO
 add_item , , "    | tiles  | foe-tiles | per foe-tile  | / battle | sets", NO, NO
 FOR mapid as integer = 0 TO gen(genMaxMap)
  'First count occurrences of each form set in the foemap
  DIM foemap as TileMap
  LoadTilemap foemap, maplumpname(mapid, "e")
  DIM occurrences(maxFormationSet) as integer  'Number of tiles on which each formation set occurs
  DIM total_foe_tiles as integer
  tally_foemap foemap, occurrences(), total_foe_tiles

  'Then compute statistics
  DIM matching_tiles as integer
  DIM enctr_rate as double  'Encounter rate
  DIM total_enctr_rate as double
  DIM avg_num as double
  DIM formsets as string
  FOR fsid as integer = 1 TO maxFormationSet
   DIM fsweight as double = formset_encounter_rate(fsid) * occurrences(fsid) / total_foe_tiles
   total_enctr_rate += fsweight

   IF formset_counts(fsid) ANDALSO occurrences(fsid) THEN
    matching_tiles += occurrences(fsid)
    enctr_rate += formset_encounter_rate(fsid) * occurrences(fsid)
    avg_num += formset_avg_num(fsid) * fsweight
    formsets &= fsid & " "
   END IF
  NEXT
  IF total_foe_tiles THEN enctr_rate /= total_foe_tiles
  IF total_enctr_rate THEN avg_num /= total_enctr_rate

  DIM percent_of_tiles as double = 100 * matching_tiles / (foemap.wide * foemap.high)
  DIM percent_of_foe_tiles as double = 100 * matching_tiles / total_foe_tiles

  IF matching_tiles THEN
   have_any = YES
   add_item 2, mapid, strprintf("%-3d | %5.1f%% | %8.1f%% | %12.2f%% | %8.3f | %s", _
                                mapid, percent_of_tiles, percent_of_foe_tiles, _
                                100 * enctr_rate, avg_num, @formsets[0])
  END IF
  UnloadTilemap foemap
 NEXT
 IF have_any = NO THEN add_item -1, 0, "(None)"
 have_any = NO

 header " Textboxes"
 DIM box as TextBox
 FOR boxid as integer = 0 TO gen(genMaxTextbox)
  LoadTextBox box, boxid
  IF form_counts(box.battle) ANDALSO box.battle_tag <> 0 ANDALSO box.battle_tag <> 1 THEN  'Tag not NEVER
   have_any = YES
   DIM taginfo as string
   IF box.battle_tag <> -1 THEN  'Tag not ALWAYS
    taginfo = " if " & tag_condition_caption(box.battle_tag, , "", "")
   END IF
   add_item 3, boxid, boxid & ": fight formation " & box.battle & taginfo
  END IF
 NEXT
 IF have_any = NO THEN add_item -1, 0, "(None)"
 have_any = NO

 DIM elementnames() as string
 getelementnames elementnames()

 header " Enemy Spawns"
 FOR eid as integer = 0 TO gen(genMaxEnemy)
  DIM enemy as EnemyDef
  loadenemydata enemy, eid

  have_any OR= check_spawn(eid, enemy, enemy.spawn.on_death, "On death")
  have_any OR= check_spawn(eid, enemy, enemy.spawn.non_elemental_death, "Non-elemental death")
  have_any OR= check_spawn(eid, enemy, enemy.spawn.when_alone, "When alone")
  have_any OR= check_spawn(eid, enemy, enemy.spawn.non_elemental_hit, "Non-elemental hit")
  FOR idx as integer = 0 TO gen(genNumElements) - 1
   have_any OR= check_spawn(eid, enemy, enemy.spawn.elemental_hit(idx), elementnames(idx) & " hit")
  NEXT
 NEXT
 IF have_any = NO THEN add_item -1, 0, "(None)"
 have_any = NO

 header " Attack Transmogrifications"
 FOR aid as integer = 0 TO gen(genMaxAttack)
  DIM attack as AttackData
  loadattackdata attack, aid
  IF attack.transmog.enemy = this.enemyid THEN
   have_any = YES
   add_item 5, aid, aid & " " & attack.name & " transmogs to " & this.enemyid & " " & this.enemyname
  END IF
 NEXT
 IF have_any = NO THEN add_item -1, 0, "(None)"
 have_any = NO

 this.state.last = UBOUND(this.menu)
END SUB

FUNCTION EnemyUsageMenu.check_spawn(eid as integer, enemy as EnemyDef, spawn as integer, description as string) as bool
 IF spawn - 1 = this.enemyid THEN
  'CHR(1) is 'x'
  add_item 4, eid, rpad(eid & " " & enemy.name, , 15, clipRight) & " " & enemy.spawn.how_many & CHR(1) & " " & description
  RETURN YES
 END IF
END FUNCTION

FUNCTION EnemyUsageMenu.each_tick() as bool
 DIM changed as bool

 IF keyval(scAlt) > 0 THEN
  changed OR= intgrabber(this.enemyid, 0, gen(genMaxEnemy))
 END IF

 'Shift-Tab to remember index, Tab to swap to remembered index
 IF handle_tab_to_switch_enemies(this.enemyid) THEN
  SWAP remem_enemy_index, this.enemyid
  changed = YES
 END IF

 IF enter_space_click(this.state) THEN
  DIM itemtype as integer = this.itemtypes(this.state.pt)
  DIM itemid as integer = this.itemids(this.state.pt)
  IF itemtype <> -1 THEN
   show_name_ticks = 23
   changed = YES
   SELECT CASE itemtype
    CASE -2 'Quit
     RETURN YES
    CASE 0  'Formation
     individual_formation_editor itemid
    CASE 1  'Formation set
     formation_set_editor itemid
    CASE 2  'Map
     mapeditor itemid
    CASE 3  'Textbox
     text_box_editor itemid
    CASE 4  'Enemy
     enemy_editor itemid
    CASE 5  'Attack
     attack_editor itemid
   END SELECT
  END IF
 END IF

 this.state.need_update = changed
END FUNCTION

SUB EnemyUsageMenu.draw()
 BASE.draw()
 IF keyval(scAlt) > 0 ORELSE show_name_ticks > 0 THEN 'holding ALT or just pressed TAB
  show_name_ticks = large(0, show_name_ticks - 1)
  textcolor uilook(uiText), uilook(uiHighlight)
  printstr this.enemyname & " " & this.enemyid, pRight, 0, vpage
 END IF
END SUB

'Modifies enemyid, e.g. using Alt to switch enemies
SUB enemy_usage_menu(byref enemyid as integer)
 DIM menu as EnemyUsageMenu
 menu.enemyid = enemyid
 menu.title = "(filled later)"
 menu.helpkey = "enemy_usage"
 menu.run()
 enemyid = menu.enemyid
END SUB


'==============================================================================
'                               Foemap Stats Menu
'==============================================================================

TYPE FoemapStatsMenu EXTENDS ModularMenu
 foemap as TileMap ptr
 enemies(any) as EnemyDef ptr  'Cache
 from_level as integer = 0
 to_level as integer = 1
 exp_mult_percent as integer = -1  '-1 is default, 0-100 is 0.00 to 1.00
 num_heroes as integer = 1

 DECLARE DESTRUCTOR()
 DECLARE SUB clear_cache()
 DECLARE SUB update()
 DECLARE FUNCTION each_tick() as bool
 DECLARE FUNCTION get_enemy(eid as integer) byref as EnemyDef
END TYPE

DESTRUCTOR FoemapStatsMenu()
 clear_cache
END DESTRUCTOR

SUB FoemapStatsMenu.clear_cache()
 FOR i as integer = 0 TO UBOUND(this.enemies)
  DELETE this.enemies(i)
 NEXT
 ERASE this.enemies
END SUB

'Load an EnemyDef with caching
FUNCTION FoemapStatsMenu.get_enemy(eid as integer) byref as EnemyDef
 REDIM PRESERVE this.enemies(gen(genMaxEnemy))
 DIM byref eptr as EnemyDef ptr = this.enemies(eid)
 IF eptr = NULL THEN
  eptr = NEW EnemyDef
  loadenemydata *eptr, eid
 END IF
 RETURN *eptr
END FUNCTION

SUB FoemapStatsMenu.update()
 ' First count occurrences of each form set in the foemap
 DIM occurrences(maxFormationSet) as integer  'Number of tiles on which each formation set occurs
 DIM total_foe_tiles as integer
 tally_foemap *this.foemap, occurrences(), total_foe_tiles

 DIM have_any as bool = NO

 add_item -2, , "Previous Menu"

 ' Show formation sets
 header " Formation Sets"
 add_item , , " ID |  % of  |   % of    | Steps / | Guessed %  |    Num   ", NO, NO
 add_item , , "    | tiles  | foe-tiles | battle  | of battles | formations", NO, NO
 DIM formsets(maxFormationSet) as FormationSet
 DIM fs_enctr_rates(maxFormationSet) as double 'Encounter chance per random foe-tile step
 DIM num_forms(maxFormationSet) as integer  'Number of formations in each formation set
 DIM fs_weights(maxFormationSet) as double 'Weight of each formation set in the combined stats (sums to 1.)

 ' Need to loop twice in order to calculate total_enctr_rate
 DIM total_enctr_rate as double
 FOR fsid as integer = 1 TO maxFormationSet
  IF occurrences(fsid) THEN
   LoadFormationSet formsets(fsid), fsid
   WITH formsets(fsid)
    fs_enctr_rates(fsid) = formset_freq_estimate(.frequency) * occurrences(fsid) / total_foe_tiles
    total_enctr_rate += fs_enctr_rates(fsid)
    'Count number of formations
    FOR idx as integer = 0 TO UBOUND(.formations)
     IF .formations(idx) >= 0 THEN num_forms(fsid) += 1
    NEXT
   END WITH
  END IF
 NEXT
 IF total_enctr_rate = 0 THEN total_enctr_rate = 1  'Avoid NaNs

 ' Then print
 FOR fsid as integer = 1 TO maxFormationSet
  IF occurrences(fsid) THEN
   WITH formsets(fsid)

    DIM percent_of_tiles as double = 100 * occurrences(fsid) / (this.foemap->wide * this.foemap->high)
    DIM percent_of_foe_tiles as double = 100 * occurrences(fsid) / total_foe_tiles
    fs_weights(fsid) = fs_enctr_rates(fsid) / total_enctr_rate
    DIM as string steps_str, enctr_str
    IF .frequency THEN
     steps_str = strprintf("%.1f", 1. / formset_freq_estimate(.frequency))
     DIM percent_of_encounters as double = 100 * fs_weights(fsid)
     enctr_str = strprintf("%9.1f%%", percent_of_encounters)
    ELSE
     steps_str = "never"
     enctr_str = "never"
    END IF

    have_any = YES
    add_item 0, fsid, strprintf("%3d | %5.1f%% | %8.1f%% | %7s | %10s | %5d", _
                                 fsid, percent_of_tiles, percent_of_foe_tiles, @steps_str[0], _
                                 @enctr_str[0], num_forms(fsid))
   END WITH
  END IF
 NEXT
 IF have_any = NO THEN add_item -1, 0, "(None)"
 have_any = NO

 header " Formations"
 add_item , , " ID |   XP   |  Gold  |   Enemies", NO, NO

 ' Get the combined list of formations (build form_weights)
 DIM form_weights(gen(genMaxFormation)) as double 'Weight of each formation in the combined stats
 DIM form_enctr_rates(gen(genMaxFormation)) as double 'Encounter chance per random foe-tile step
 DIM form_needed(gen(genMaxFormation)) as bool  'Need to load this formation (although form_weights may be 0)
 FOR fsid as integer = 1 TO maxFormationSet
  IF occurrences(fsid) ANDALSO num_forms(fsid) > 0 THEN
   FOR idx as integer = 0 TO UBOUND(formsets(fsid).formations)
    DIM formid as integer = formsets(fsid).formations(idx)
    IF formid >= 0 THEN
     form_weights(formid) += fs_weights(fsid) / num_forms(fsid)
     form_enctr_rates(formid) += fs_enctr_rates(fsid) / num_forms(fsid)
     form_needed(formid) = YES
    END IF
   NEXT
  END IF
 NEXT

 ' Print the formations and tally the enemies in each formation
 DIM enemy_formations(gen(genMaxEnemy)) as string  'List of formations containing
 DIM enemy_enctr_rates(gen(genMaxEnemy)) as double 'Chance of encounter containing this enemy per foe-tile
 DIM enemy_weights(gen(genMaxEnemy)) as double   'Avg number per encounter
 DIM form_exper(gen(genMaxFormation)) as integer 'Formation totals
 DIM form_gold(gen(genMaxFormation)) as integer
 FOR formid as integer = 0 TO gen(genMaxFormation)
  IF form_needed(formid) THEN
   DIM form as Formation
   LoadFormation form, formid

   DIM already_seen() as integer
   FOR slot as integer = 0 TO UBOUND(form.slots)
    WITH form.slots(slot)
     IF .id >= 0 THEN
      enemy_weights(.id) += form_weights(formid)
      DIM reward as EnemyRewardDef = get_enemy(.id).reward
      form_exper(formid) += reward.exper
      form_gold(formid) += reward.gold
      IF form_weights(formid) > 0 ANDALSO a_find(already_seen(), .id) = -1 THEN
       'First occurrence in this formation
       a_append already_seen(), .id
       enemy_formations(.id) &= formid & " "
       enemy_enctr_rates(.id) += form_enctr_rates(formid)
      END IF
     END IF
    END WITH
   NEXT

   have_any = YES
   add_item 1, formid, strprintf("%3d | %6d | %6d | ", formid, form_exper(formid), form_gold(formid)) _
                       & describe_formation(form)
  END IF
 NEXT
 IF have_any = NO THEN add_item -1, 0, "(None)"
 have_any = NO

 'Unfortunately need to loop again, because rewards are calculated enemies->formations->sets,
 'while encounter rates are calculated sets->formations->enemies
 DIM fs_exper(gen(genMaxFormation)) as double   'Average XP, gold reward for the formset
 DIM fs_gold(gen(genMaxFormation)) as double
 FOR fsid as integer = 1 TO maxFormationSet
  IF occurrences(fsid) ANDALSO num_forms(fsid) > 0 THEN
   FOR idx as integer = 0 TO UBOUND(formsets(fsid).formations)
    DIM formid as integer = formsets(fsid).formations(idx)
    IF formid >= 0 THEN
     fs_exper(fsid) += form_exper(formid) / num_forms(fsid)
     fs_gold(fsid) += form_gold(formid) / num_forms(fsid)
    END IF
   NEXT
  END IF
 NEXT

 'Expected items per encounter
 DIM drops(gen(genMaxItem)) as double
 DIM steals(gen(genMaxItem)) as double
 DIM unlimited_steal(gen(genMaxItem)) as bool  'At least one enemy allowing unlimited stealing
 'Total rewards for formations
 DIM xp(gen(genMaxFormation)) as integer
 DIM gold(gen(genMaxFormation)) as integer

 header " Enemies"
 add_item , , "               |  Steps /  |  % of   | Avg num  |    In", NO, NO
 add_item , , "               | encounter | battles | / battle | formations", NO, NO
 FOR eid as integer = 0 TO gen(genMaxEnemy)
  IF enemy_weights(eid) THEN
   DIM byref enemy as EnemyDef = get_enemy(eid)

   DIM enctr_percent as double = 100 * enemy_enctr_rates(eid) / total_enctr_rate
   DIM enctr_steps as double = 1. / enemy_enctr_rates(eid)

   have_any = YES
   DIM shortname as string = LEFT(eid & " " & enemy.name, 14)
   add_item 2, eid, strprintf("%-14s | %9.1f | %6.1f%% | %8.3f | %s", _
                              @shortname[0], enctr_steps, enctr_percent, enemy_weights(eid), _
                              @enemy_formations(eid)[0])

   WITH enemy.reward
    drops(.item) += enemy_weights(eid) * .item_rate / 100
    IF .item_rate < 100 THEN
     drops(.rare_item) += enemy_weights(eid) * (1. - .item_rate / 100) * .rare_item_rate / 100
    END IF
   END WITH

   WITH enemy.steal
    IF .thievability >= 0 THEN  'Not disabled
     IF .item_rate > 0 THEN
      unlimited_steal(.item) OR= (.thievability = 1)
     END IF
     steals(.item) += enemy_weights(eid) * .item_rate / 100
     IF .item_rate < 100 THEN
      IF .rare_item_rate > 0 THEN
       unlimited_steal(.rare_item) OR= (.thievability = 1)
      END IF
      steals(.rare_item) += enemy_weights(eid) * (1. - .item_rate / 100) * .rare_item_rate / 100
     END IF
    END IF
   END WITH
  END IF
 NEXT
 IF have_any = NO THEN add_item -1, 0, "(None)"

 header " Formation Set Rewards"
 have_any = NO
 add_item , , " ID |  Avg   |   Avg  | Battles for  | Steps for", NO, NO
 add_item , , strprintf(_
              "    |  gold  |   XP   | level %-2d->%2d | level %-2d->%2d", _
              this.from_level, this.to_level, this.from_level, this.to_level), NO, NO
 FOR fsid as integer = 1 TO maxFormationSet
  IF occurrences(fsid) THEN
   WITH formsets(fsid)
    'Calculate encounters and steps needed
    DIM enctrs_needed as double
    DIM as string enctr_str, steps_str
    DIM exp_mult as double = 0.2
    IF this.exp_mult_percent >= 0 THEN exp_mult = exp_mult_percent / 100.
    DIM xp_needed as integer
    xp_needed = total_exp_to_level(this.to_level, exp_mult) - total_exp_to_level(this.from_level, exp_mult)
    IF prefbit(30) = NO THEN  '"!Divide experience between heroes"
     xp_needed *= this.num_heroes
    END IF
    IF xp_needed <= 0 THEN
     enctr_str = "N/A"
     steps_str = "N/A"
    ELSEIF fs_exper(fsid) = 0. THEN
     enctr_str = "never"
     steps_str = "never"
    ELSE
     DIM enctrs_needed as double = xp_needed / fs_exper(fsid)
     enctr_str = strprintf("%12.1f", enctrs_needed)
     IF .frequency THEN
      steps_str = strprintf("%12.1f", enctrs_needed / formset_freq_estimate(.frequency))
     ELSE
      steps_str = "never"
     END IF
    END IF

    have_any = YES
    add_item 0, fsid, strprintf("%3d | %6.0f | %6.0f | %12s | %12s ", fsid, fs_gold(fsid), _
                                fs_exper(fsid), @enctr_str[0], @steps_str[0])
   END WITH
  END IF
 NEXT
 IF have_any = NO THEN add_item -1, 0, "(None)"

 IF have_any THEN
  'Editable inputs for the preview
  add_spacer
  add_item 10, , "From level: " & this.from_level
  add_item 11, , "To level: " & this.to_level
  DIM temp as string = "default"
  IF this.exp_mult_percent >= 0 THEN temp = strprintf("%.2f", this.exp_mult_percent / 100.)
  add_item 12, , "Experience curve: " & temp
  IF prefbit(30) = NO THEN  '"!Divide experience between heroes"
   add_item 13, , "For # heroes: " & this.num_heroes
  END IF
 END IF

 have_any = NO

 header " Items (drops)"
 add_item , , "             |  Avg num per battle", NO, NO
 FOR iid as integer = 0 TO gen(genMaxItem)
  IF drops(iid) > 0. THEN
   have_any = YES
   DIM itname as string = readitemname(iid)
   add_item 3, iid, strprintf("%3d %-8s | %5.3f", iid, @itname[0], drops(iid))
  END IF
 NEXT
 IF have_any = NO THEN add_item -1, 0, "(None)"
 have_any = NO

 header " Items (stealable)"
 add_item , , "             |  Avg num per battle", NO, NO
 DIM have_unlimited_steal as bool
 FOR iid as integer = 0 TO gen(genMaxItem)
  IF steals(iid) > 0. THEN
   have_any = YES
   DIM itname as string = readitemname(iid)
   add_item 3, iid, strprintf("%3d %-8s | %5.3f %c", iid, @itname[0], steals(iid), _
                              IIF(unlimited_steal(iid), ASC("+"), ASC(" ")))
   have_unlimited_steal OR= unlimited_steal(iid)
  END IF
 NEXT
 IF have_unlimited_steal THEN add_item -1, 0, "(+: unlimited stealing possible)"
 IF have_any = NO THEN add_item -1, 0, "(None)"
 have_any = NO

 this.state.last = UBOUND(this.menu)
END SUB

FUNCTION FoemapStatsMenu.each_tick() as bool
 DIM changed as bool

 DIM itemtype as integer = this.itemtypes(this.state.pt)
 DIM itemid as integer = this.itemids(this.state.pt)

 IF enter_space_click(this.state) THEN
  IF itemtype <> -1 THEN
   changed = YES
   SELECT CASE itemtype
    CASE -2 'Quit
     RETURN YES
    CASE 0  'Formation set
     formation_set_editor itemid
     this.clear_cache()  'Out of date after entering any editor
    CASE 1  'Formation
     individual_formation_editor itemid
     this.clear_cache()
    CASE 2  'Enemy
     enemy_editor itemid
     this.clear_cache()
    CASE 3  'Item
     individual_item_editor itemid
     this.clear_cache()
   END SELECT
  END IF
 END IF

 SELECT CASE itemtype
  CASE 10
   changed OR= intgrabber(this.from_level, 0, gen(genMaxLevel))
  CASE 11
   changed OR= intgrabber(this.to_level, 0, gen(genMaxLevel))
  CASE 12
   changed OR= intgrabber(this.exp_mult_percent, -1, 100)
  CASE 13
   changed OR= intgrabber(this.num_heroes, 1, 4)
 END SELECT

 this.state.need_update = changed
END FUNCTION

SUB foemap_stats_menu(foemap as TileMap, title as string)
 DIM menu as FoemapStatsMenu
 menu.foemap = @foemap
 menu.title = title
 menu.helpkey = "foemap_stats"
 menu.run()
 menu.clear_cache()
END SUB
