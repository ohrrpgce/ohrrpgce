'OHRRPGCE CUSTOM - Mostly menu-related routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
OPTION EXPLICIT
DEFINT A-Z
'$DYNAMIC
DECLARE SUB cropafter (index%, limit%, flushafter%, lump$, bytes%, prompt%)
DECLARE SUB clearallpages ()
DECLARE SUB enforceflexbounds (menuoff() AS INTEGER, menutype() AS INTEGER, menulimits() AS INTEGER, recbuf() AS INTEGER, min() AS INTEGER, max() AS INTEGER)
DECLARE SUB addcaption (caption() AS STRING, indexer AS INTEGER, cap AS STRING)
DECLARE FUNCTION editflexmenu (nowindex AS INTEGER, menutype() AS INTEGER, menuoff() AS INTEGER, menulimits() AS INTEGER, datablock() AS INTEGER, mintable() AS INTEGER, maxtable() AS INTEGER) AS INTEGER
DECLARE SUB updateflexmenu (mpointer AS INTEGER, nowmenu() AS STRING, nowdat() AS INTEGER, size AS INTEGER, menu() AS STRING, menutype() AS INTEGER, menuoff() AS INTEGER, menulimits() AS INTEGER, datablock() AS INTEGER, caption() AS STRING, maxtable() AS INTEGER, recindex AS INTEGER)
DECLARE SUB attackdata ()
DECLARE FUNCTION isStringField(mnu AS INTEGER)

#include "compat.bi"
#include "allmodex.bi"
#include "common.bi"
#include "customsubs.bi"
#include "cglobals.bi"

#include "const.bi"
#include "scrconst.bi"
#include "loading.bi"
#include "scancodes.bi"
#include "slices.bi"

DECLARE SUB menu_editor ()
DECLARE SUB update_menu_editor_menu(record, edmenu AS MenuDef, menu AS MenuDef)
DECLARE SUB update_detail_menu(detail AS MenuDef, mi AS MenuDefItem)
DECLARE SUB menu_editor_keys (state AS MenuState, mstate AS MenuState, menudata AS MenuDef, record, menu_set AS MenuSet)
DECLARE SUB menu_editor_menu_keys (mstate AS MenuState, dstate AS MenuState, menudata AS MenuDef, record AS INTEGER)
DECLARE SUB menu_editor_detail_keys(dstate AS MenuState, mstate AS MenuState, detail AS MenuDef, mi AS MenuDefItem)

DECLARE SUB setactivemenu (workmenu(), newmenu(), BYREF state AS MenuState)
DECLARE SUB flexmenu_skipper (BYREF state AS MenuState, workmenu(), menutype())

DECLARE SUB atk_edit_preview(BYVAL pattern AS INTEGER, sl AS Slice Ptr)
DECLARE SUB atk_edit_pushptr(state AS MenuState, laststate AS MenuState, BYREF menudepth AS INTEGER)
DECLARE SUB atk_edit_backptr(workmenu() AS INTEGER, mainMenu() AS INTEGER, state AS MenuState, laststate AS menustate, BYREF menudepth AS INTEGER)

REM $STATIC
SUB addcaption (caption() AS STRING, indexer, cap AS STRING)
IF indexer > UBOUND(caption) THEN
 fatalerror "caption(" + STR(indexer) + ") overflow on " + cap
ELSE
 caption(indexer) = cap
 indexer = indexer + 1
END IF
END SUB

SUB attackdata

clearallpages
DIM i AS INTEGER

'--bitsets

DIM atkbit(-1 TO 128) AS STRING

atkbit(0) = "Cure Instead of Harm"
atkbit(1) = "Divide Spread Damage"
atkbit(2) = "Absorb Damage"          'was bounceable!
atkbit(3) = "Unreversable Picture"
atkbit(4) = "Steal Item"

FOR i = 0 TO 7
 atkbit(i + 5) = readglobalstring(17 + i, "Elemental" & i+1) & " Damage" '05-12
 atkbit(i + 13) = "Bonus vs " & readglobalstring(9 + i, "EnemyType" & i+1) '13-20
 atkbit(i + 21) = "Fail vs " & readglobalstring(17 + i, "Elemental" & i+1) & " resistance" '21-28
 atkbit(i + 29) = "Fail vs " & readglobalstring(9 + i, "EnemyType" & i+1) '29-36
NEXT i

FOR i = 0 TO 7
 atkbit(i + 37) = "Cannot target enemy slot " & i
NEXT i
FOR i = 0 TO 3
 atkbit$(i + 45) = "Cannot target hero slot " & i
NEXT i

atkbit(49) = "Ignore attacker's extra hits"
atkbit(50) = "Erase rewards (Enemy target only)"
atkbit(51) = "Show damage without inflicting"
atkbit(52) = "Store Target"
atkbit(53) = "Delete Stored Target"
atkbit(54) = "Automatically choose target"
atkbit(55) = "Show attack name"
atkbit(56) = "Do not display Damage"
atkbit(57) = "Reset target stat to max before hit"
atkbit(58) = "Allow Cure to exceed maximum"
atkbit(59) = "Useable Outside of Battle"
atkbit(60) = "Damage " & statnames(statMP) & " (obsolete)"
atkbit(61) = "Do not randomize"
atkbit(62) = "Damage can be Zero"
atkbit(63) = "Cause heroes to run away"
atkbit(64) = "Mutable"
atkbit(65) = "Fail if target is poisoned"
atkbit(66) = "Fail if target is regened"
atkbit(67) = "Fail if target is stunned"
atkbit(68) = "Fail if target is muted"
atkbit(69) = "% based attacks damage instead of set"
atkbit(70) = "Check costs when used as a weapon"
atkbit(71) = "Do not chain if attack fails"
atkbit(72) = "Reset Poison register"
atkbit(73) = "Reset Regen register"
atkbit(74) = "Reset Stun register"
atkbit(75) = "Reset Mute register"
atkbit(76) = "Cancel target's attack"
atkbit(77) = "Can't be canceled by other attacks"
atkbit(78) = "Do not trigger spawning on hit"
atkbit(79) = "Do not trigger spawning on kill"
atkbit(80) = "Check costs when used as an item"
atkbit(81) = "Re-check costs after attack delay"
atkbit(82) = "Do not cause target to flinch"
'             ^---------------------------------------^
'               the amount of room you have (39 chars)

'--191 attack bits allowed in menu.
'--Data is split, See AtkDatBits and AtkDatBits2 for offsets

DIM atk_chain_bitset_names(1) AS STRING
atk_chain_bitset_names(0) = "Attacker must know chained attack"
atk_chain_bitset_names(1) = "Ignore chained attack's delay"

'----------------------------------------------------------
DIM recbuf(40 + curbinsize(0) / 2) '--stores the combined attack data from both .DT6 and ATTACK.BIN

CONST AtkDatPic = 0
CONST AtkDatPal = 1
CONST AtkDatAnimPattern = 2
CONST AtkDatTargClass = 3
CONST AtkDatTargSetting = 4
CONST AtkDatDamageEq = 5
CONST AtkDatAimEq = 6
CONST AtkDatBaseAtk = 7
CONST AtkDatMPCost = 8
CONST AtkDatHPCost = 9
CONST AtkDatMoneyCost = 10
CONST AtkDatExtraDamage = 11
CONST AtkDatChainTo = 12
CONST AtkDatChainRate = 13
CONST AtkDatAnimAttacker = 14
CONST AtkDatAnimAttack = 15
CONST AtkDatDelay = 16
CONST AtkDatHitX = 17
CONST AtkDatTargStat = 18
CONST AtkDatPreferTarg = 19
CONST AtkDatBitsets = 20' to 23
CONST AtkDatName = 24'to 35
CONST AtkDatCapTime = 36
CONST AtkDatCaption = 37'to 56
CONST AtkDatCaptDelay = 57
CONST AtkDatBaseDef = 58
CONST AtkDatTag = 59
CONST AtkDatTagIf = 60
CONST AtkDatTagAnd = 61
CONST AtkDatTag2 = 62
CONST AtkDatTagIf2 = 63
CONST AtkDatTagAnd2 = 64
CONST AtkDatBitsets2 = 65' to 72
CONST AtkDatDescription = 73'to 92
CONST AtkDatItem = 93', 95, 97
CONST AtkDatItemCost = 94', 96, 98
CONST AtkDatSoundEffect = 99
CONST AtkDatPrefTargStat = 100
CONST AtkDatChainMode = 101
CONST AtkDatChainVal1 = 102
CONST AtkDatChainVal2 = 103
CONST AtkDatChainBits = 104
CONST AtkDatElseChainTo = 105
CONST AtkDatElseChainMode = 106
CONST AtkDatElseChainRate = 107
CONST AtkDatElseChainVal1 = 108
CONST AtkDatElseChainVal2 = 109
CONST AtkDatElseChainBits = 110
CONST AtkDatInsteadChainTo = 111
CONST AtkDatInsteadChainMode = 112
CONST AtkDatInsteadChainRate = 113
CONST AtkDatInsteadChainVal1 = 114
CONST AtkDatInsteadChainVal2 = 115
CONST AtkDatInsteadChainBits = 116

'anything past this requires expanding the data


'----------------------------------------------------------
DIM capindex AS INTEGER = 0
DIM caption(151) AS STRING
DIM max(37), min(37)

'Limit(0) is not used

CONST AtkLimUInt = 8
max(AtkLimUInt) = 32767

CONST AtkLimInt = 9
max(AtkLimInt) = 32767
min(AtkLimInt) = -32767

CONST AtkLimStr10 = 10
max(AtkLimStr10) = 10

CONST AtkLimStr38 = 19
max(AtkLimStr38) = 38

CONST AtkLimPic = 1
max(AtkLimPic) = gen(32)

CONST AtkLimAnimPattern = 2
max(AtkLimAnimPattern) = 3
DIM AtkCapAnimPattern AS INTEGER = capindex
addcaption caption(), capindex, "Cycle Forward"
addcaption caption(), capindex, "Cycle Back"
addcaption caption(), capindex, "Oscillate"
addcaption caption(), capindex, "Random"

CONST AtkLimTargClass = 3
max(AtkLimTargClass) = 12
DIM AtkCapTargClass AS INTEGER = capindex
addcaption caption(), capindex, "Enemy"
addcaption caption(), capindex, "Ally"
addcaption caption(), capindex, "Self"
addcaption caption(), capindex, "All"
addcaption caption(), capindex, "Ally (Including Dead)"
addcaption caption(), capindex, "Ally Not Self"
addcaption caption(), capindex, "Revenge (last to hit attacker)"
addcaption caption(), capindex, "Revenge (whole battle)"
addcaption caption(), capindex, "Previous target"
addcaption caption(), capindex, "Recorded target"
addcaption caption(), capindex, "Dead Allies (hero only)"
addcaption caption(), capindex, "Thankvenge (last to cure attacker)"
addcaption caption(), capindex, "Thankvenge (whole battle)"

CONST AtkLimTargSetting = 4
max(AtkLimTargSetting) = 4
DIM AtkCapTargSetting AS INTEGER = capindex
addcaption caption(), capindex, "Focused"
addcaption caption(), capindex, "Spread"
addcaption caption(), capindex, "Optional Spread"
addcaption caption(), capindex, "Random Focus"
addcaption caption(), capindex, "First Target"

CONST AtkLimDamageEq = 5
max(AtkLimDamageEq) = 6
DIM AtkCapDamageEq AS INTEGER = capindex
addcaption caption(), capindex, "Normal: ATK - DEF*.5"
addcaption caption(), capindex, "Blunt: ATK*.8 - DEF*.1"
addcaption caption(), capindex, "Sharp: ATK*1.3 - DEF"
addcaption caption(), capindex, "Pure Damage"
addcaption caption(), capindex, "No Damage"
addcaption caption(), capindex, "Set = N% of Max"
addcaption caption(), capindex, "Set = N% of Current"

CONST AtkLimAimEq = 6
max(AtkLimAimEq) = 8
DIM AtkCapAimEq AS INTEGER = capindex
addcaption caption(), capindex, "Normal: " & statnames(statAim) & "*4 ~ " & statnames(statDodge)
addcaption caption(), capindex, "Poor: " & statnames(statAim) & "*2 ~ " & statnames(statDodge)
addcaption caption(), capindex, "Bad: " & statnames(statAim) & " ~ " & statnames(statDodge)
addcaption caption(), capindex, "Never Misses"
addcaption caption(), capindex, "Magic: " & statnames(statMagic) & " ~ " & statnames(statWill) & "*1.25"
addcaption caption(), capindex, "Percentage: " & statnames(statAim) & "% * " & statnames(statDodge) & "%"
addcaption caption(), capindex, "Percentage: " & statnames(statAim) & "%"
addcaption caption(), capindex, "Percentage: " & statnames(statMagic) & "% * " & statnames(statWill) & "%"
addcaption caption(), capindex, "Percentage: " & statnames(statMagic) & "%"

CONST AtkLimBaseAtk = 7
max(AtkLimBaseAtk) = 22 + (UBOUND(statnames) - 11)
DIM AtkCapBaseAtk AS INTEGER = capindex
addcaption caption(), capindex, statnames(statAtk)
addcaption caption(), capindex, statnames(statMagic)
addcaption caption(), capindex, statnames(statHP)
addcaption caption(), capindex, "Lost " & statnames(statHP)
addcaption caption(), capindex, "Random 0 to 999"
addcaption caption(), capindex, "100"
FOR i = 0 TO 11
 addcaption caption(), capindex, statnames(i)
NEXT
addcaption caption(), capindex, "previous attack"
addcaption caption(), capindex, "last damage to attacker"
addcaption caption(), capindex, "last damage to target"
addcaption caption(), capindex, "last cure to attacker"
addcaption caption(), capindex, "last cure to target"
FOR i = 12 TO UBOUND(statnames)
 addcaption caption(), capindex, statnames(i)
NEXT

CONST AtkLimExtraDamage = 11
max(AtkLimExtraDamage) = 1000
min(AtkLimExtraDamage) = -100

CONST AtkLimChainTo = 12
max(AtkLimChainTo) = gen(genMaxAttack) + 1'--must be updated!

CONST AtkLimChainRate = 13
max(AtkLimChainRate) = 100
min(AtkLimChainRate) = 0

CONST AtkLimAnimAttacker = 14
max(AtkLimAnimAttacker) = 8
DIM AtkCapAnimAttacker AS INTEGER = capindex
addcaption caption(), capindex, "Strike"
addcaption caption(), capindex, "Cast"
addcaption caption(), capindex, "Dash In"
addcaption caption(), capindex, "SpinStrike"
addcaption caption(), capindex, "Jump (chain to Land)"
addcaption caption(), capindex, "Land"
addcaption caption(), capindex, "Null"
addcaption caption(), capindex, "Standing Cast"
addcaption caption(), capindex, "Teleport"

CONST AtkLimAnimAttack = 15
max(AtkLimAnimAttack) = 10
DIM AtkCapAnimAttack AS INTEGER = capindex
addcaption caption(), capindex, "Normal"
addcaption caption(), capindex, "Projectile"
addcaption caption(), capindex, "Reverse Projectile"
addcaption caption(), capindex, "Drop"
addcaption caption(), capindex, "Ring"
addcaption caption(), capindex, "Wave"
addcaption caption(), capindex, "Scatter"
addcaption caption(), capindex, "Sequential Projectile"
addcaption caption(), capindex, "Meteor"
addcaption caption(), capindex, "Driveby"
addcaption caption(), capindex, "Null"

CONST AtkLimDelay = 16
max(AtkLimDelay) = 1000

CONST AtkLimHitX = 17
max(AtkLimHitX) = 20
min(AtkLimHitX) = 1

CONST AtkLimTargStat = 18
max(AtkLimTargStat) = 15 + (UBOUND(statnames) - 11)
DIM AtkCapTargStat AS INTEGER = capindex
FOR i = 0 TO 11
 addcaption caption(), capindex, statnames(i)
NEXT
addcaption caption(), capindex, "poison register"
addcaption caption(), capindex, "regen register"
addcaption caption(), capindex, "stun register"
addcaption caption(), capindex, "mute register"
FOR i = 12 TO UBOUND(statnames)
 addcaption caption(), capindex, statnames(i)
NEXT

CONST AtkLimCapTime = 20
max(AtkLimCapTime) = 16383
min(AtkLimCapTime) = -1
addcaption caption(), capindex, "Ticks"
DIM AtkCapCapTime AS INTEGER = capindex
addcaption caption(), capindex, "Full Duration of Attack"
addcaption caption(), capindex, "Not at All"

CONST AtkLimCaptDelay = 21
max(AtkLimCaptDelay) = 16383
min(AtkLimCaptDelay) = 0

CONST AtkLimBaseDef = 22
max(AtkLimBaseDef) = 1 + UBOUND(statnames)
DIM AtkCapBaseDef AS INTEGER = capindex
addcaption caption(), capindex, "Default"
FOR i = 0 TO UBOUND(statnames)
 addcaption caption(), capindex, statnames(i)
NEXT

CONST AtkLimTag = 23
max(AtkLimTag) = 1000
min(AtkLimTag) = -1000

CONST AtkLimTagIf = 24
max(AtkLimTagIf) = 4
DIM AtkCapTagIf AS INTEGER = capindex
addcaption caption(), capindex, "Never" '0
addcaption caption(), capindex, "Use"   '1
addcaption caption(), capindex, "Hit"   '2
addcaption caption(), capindex, "Miss"  '3
addcaption caption(), capindex, "Kill"  '4

CONST AtkLimTagAnd = 25
max(AtkLimTag) = 1000
min(AtkLimTag) = -1000

CONST AtkLimItem = 26
max(AtkLimItem) = gen(genMaxItem) + 1
min(AtkLimItem) = 0

CONST AtkLimSfx = 27
max(AtkLimSfx) = gen(genMaxSFX) + 1
min(AtkLimSfx) = 0

CONST AtkLimPal16 = 28
max(AtkLimPal16) = 32767
min(AtkLimPal16) = -1

CONST AtkLimPreferTarg = 29
max(AtkLimPreferTarg) = 8
min(AtkLimPreferTarg) = 0
DIM AtkCapPreferTarg AS INTEGER = capindex
addcaption caption(), capindex, "default"    '0
addcaption caption(), capindex, "first"      '1
addcaption caption(), capindex, "closest"    '2
addcaption caption(), capindex, "farthest"   '3
addcaption caption(), capindex, "random"     '4
addcaption caption(), capindex, "weakest"    '5
addcaption caption(), capindex, "strongest"  '6
addcaption caption(), capindex, "weakest%"   '7
addcaption caption(), capindex, "strongest%" '8

CONST AtkLimPrefTargStat = 30
max(AtkLimPrefTargStat) = 16
min(AtkLimPrefTargStat) = 0
DIM AtkCapPrefTargStat AS INTEGER = capindex
addcaption caption(), capindex, "same as target stat" '0
FOR i = 0 TO 11  '1 - 12
 addcaption caption(), capindex, statnames(i)
NEXT
addcaption caption(), capindex, "poison register"'13
addcaption caption(), capindex, "regen register" '14 
addcaption caption(), capindex, "stun register"  '15
addcaption caption(), capindex, "mute register"  '16
FOR i = 12 TO UBOUND(statnames) '17+
 addcaption caption(), capindex, statnames(i)
NEXT

CONST AtkLimChainMode = 31
max(AtkLimChainMode) = 5
DIM AtkCapChainMode AS INTEGER = capindex
addcaption caption(), capindex, "No special conditions" '0
addcaption caption(), capindex, "Tag Check"     '1
addcaption caption(), capindex, "Attacker stat > value" '2
addcaption caption(), capindex, "Attacker stat < value" '3
addcaption caption(), capindex, "Attacker stat > %"     '4
addcaption caption(), capindex, "Attacker stat < %"     '5

CONST AtkLimChainVal1 = 32
max(AtkLimChainVal1) = 0 '--updated by update_attack_editor_for_chain()
min(AtkLimChainVal1) = 0 '--updated by update_attack_editor_for_chain()

CONST AtkLimChainVal2 = 33
max(AtkLimChainVal2) = 0 '--updated by update_attack_editor_for_chain()
min(AtkLimChainVal2) = 0 '--updated by update_attack_editor_for_chain()

CONST AtkLimElseChainVal1 = 34
max(AtkLimElseChainVal1) = 0 '--updated by update_attack_editor_for_chain()
min(AtkLimElseChainVal1) = 0 '--updated by update_attack_editor_for_chain()

CONST AtkLimElseChainVal2 = 35
max(AtkLimElseChainVal2) = 0 '--updated by update_attack_editor_for_chain()
min(AtkLimElseChainVal2) = 0 '--updated by update_attack_editor_for_chain()

CONST AtkLimInsteadChainVal1 = 36
max(AtkLimInsteadChainVal1) = 0 '--updated by update_attack_editor_for_chain()
min(AtkLimInsteadChainVal1) = 0 '--updated by update_attack_editor_for_chain()

CONST AtkLimInsteadChainVal2 = 37
max(AtkLimInsteadChainVal2) = 0 '--updated by update_attack_editor_for_chain()
min(AtkLimInsteadChainVal2) = 0 '--updated by update_attack_editor_for_chain()

'next limit is 38 (remember to update the dim)

'----------------------------------------------------------------------
'--menu content
CONST MnuItems = 68
DIM menu(MnuItems) AS STRING, menutype(MnuItems), menuoff(MnuItems), menulimits(MnuItems)

CONST AtkBackAct = 0
menu(AtkBackAct) = "Previous Menu"
menutype(AtkBackAct) = 1

CONST AtkName = 1
menu(AtkName) = "Name:"
menutype(AtkName) = 6
menuoff(AtkName) = AtkDatName
menulimits(AtkName) = AtkLimStr10

CONST AtkAppearAct = 2
menu(AtkAppearAct) = "Appearance..."
menutype(AtkAppearAct) = 1

CONST AtkDmgAct = 3
menu(AtkDmgAct) = "Damage Settings..."
menutype(AtkDmgAct) = 1

CONST AtkTargAct = 4
menu(AtkTargAct) = "Target Settings..."
menutype(AtkTargAct) = 1

CONST AtkCostAct = 5
menu(AtkCostAct) = "Cost..."
menutype(AtkCostAct) = 1

CONST AtkChainAct = 6
menu(AtkChainAct) = "Chaining..."
menutype(AtkChainAct) = 1

CONST AtkBitAct = 7
menu(AtkBitAct) = "Bitsets..."
menutype(AtkBitAct) = 1

CONST AtkPic = 8
menu(AtkPic) = "Picture:"
menutype(AtkPic) = 0
menuoff(AtkPic) = AtkDatPic
menulimits(AtkPic) = AtkLimPic

CONST AtkPal = 9
menu(AtkPal) = "Palette:"
menutype(AtkPal) = 12
menuoff(AtkPal) = AtkDatPal
menulimits(AtkPal) = AtkLimPal16

CONST AtkAnimPattern = 10
menu(AtkAnimPattern) = "Animation Pattern:"
menutype(AtkAnimPattern) = 2000 + AtkCapAnimPattern
menuoff(AtkAnimPattern) = AtkDatAnimPattern
menulimits(AtkAnimPattern) = AtkLimAnimPattern

CONST AtkTargClass = 11
menu(AtkTargClass) = "Target Class:"
menutype(AtkTargClass) = 2000 + AtkCapTargClass
menuoff(AtkTargClass) = AtkDatTargClass
menulimits(AtkTargClass) = AtkLimTargClass

CONST AtkTargSetting = 12
menu(AtkTargSetting) = "Target Setting:"
menutype(AtkTargSetting) = 2000 + AtkCapTargSetting
menuoff(AtkTargSetting) = AtkDatTargSetting
menulimits(AtkTargSetting) = AtkLimTargSetting

CONST AtkChooseAct = 13
menu(AtkChooseAct) = "Attack"
menutype(AtkChooseAct) = 5

CONST AtkDamageEq = 14
menu(AtkDamageEq) = "Damage Math:"
menutype(AtkDamageEq) = 2000 + AtkCapDamageEq
menuoff(AtkDamageEq) = AtkDatDamageEq
menulimits(AtkDamageEq) = AtkLimDamageEq

CONST AtkAimEq = 15
menu(AtkAimEq) = "Aim Math:"
menutype(AtkAimEq) = 2000 + AtkCapAimEq
menuoff(AtkAimEq) = AtkDatAimEq
menulimits(AtkAimEq) = AtkLimAimEq

CONST AtkBaseAtk = 16
menu(AtkBaseAtk) = "Base ATK Stat:"
menutype(AtkBaseAtk) = 2000 + AtkCapBaseAtk
menuoff(AtkBaseAtk) = AtkDatBaseAtk
menulimits(AtkBaseAtk) = AtkLimBaseAtk

CONST AtkMPCost = 17
menu(AtkMPCost) = statnames(statMP) & " Cost:"
menutype(AtkMPCost) = 0
menuoff(AtkMPCost) = AtkDatMPCost
menulimits(AtkMPCost) = AtkLimInt

CONST AtkHPCost = 18
menu(AtkHPCost) = statnames(statHP) & " Cost:"
menutype(AtkHPCost) = 0
menuoff(AtkHPCost) = AtkDatHPCost
menulimits(AtkHPCost) = AtkLimInt

CONST AtkMoneyCost = 19
menu(AtkMoneyCost) = readglobalstring(32, "Money") & " Cost:"
menutype(AtkMoneyCost) = 0
menuoff(AtkMoneyCost) = AtkDatMoneyCost
menulimits(AtkMoneyCost) = AtkLimInt

CONST AtkExtraDamage = 20
menu(AtkExtraDamage) = "Extra Damage:"
menutype(AtkExtraDamage) = 17 'int%
menuoff(AtkExtraDamage) = AtkDatExtraDamage
menulimits(AtkExtraDamage) = AtkLimExtraDamage

CONST AtkChainTo = 21
menu(AtkChainTo) = "  Attack:"
menutype(AtkChainTo) = 7 '--special class for showing an attack name
menuoff(AtkChainTo) = AtkDatChainTo
menulimits(AtkChainTo) = AtkLimChainTo

CONST AtkChainRate = 22
menu(AtkChainRate) = "  Rate:"
menutype(AtkChainRate) = 17
menuoff(AtkChainRate) = AtkDatChainRate
menulimits(AtkChainRate) = AtkLimChainRate

CONST AtkAnimAttacker = 23
menu(AtkAnimAttacker) = "Attacker Animation:"
menutype(AtkAnimAttacker) = 2000 + AtkCapAnimAttacker
menuoff(AtkAnimAttacker) = AtkDatAnimAttacker
menulimits(AtkAnimAttacker) = AtkLimAnimAttacker

CONST AtkAnimAttack = 24
menu(AtkAnimAttack) = "Attack Animation:"
menutype(AtkAnimAttack) = 2000 + AtkCapAnimAttack
menuoff(AtkAnimAttack) = AtkDatAnimAttack
menulimits(AtkAnimAttack) = AtkLimAnimAttack

CONST AtkDelay = 25
menu(AtkDelay) = "Delay Before Attack:"
menutype(AtkDelay) = 19'ticks
menuoff(AtkDelay) = AtkDatDelay
menulimits(AtkDelay) = AtkLimDelay

CONST AtkHitX = 26
menu(AtkHitX) = "Number of Hits:"
menutype(AtkHitX) = 0
menuoff(AtkHitX) = AtkDatHitX
menulimits(AtkHitX) = AtkLimHitX

CONST AtkTargStat = 27
menu(AtkTargStat) = "Target Stat:"
menutype(AtkTargStat) = 2000 + AtkCapTargStat
menuoff(AtkTargStat) = AtkDatTargStat
menulimits(AtkTargStat) = AtkLimTargStat

CONST AtkCaption = 28
menu(AtkCaption) = "Caption:"
menutype(AtkCaption) = 3'goodstring
menuoff(AtkCaption) = AtkDatCaption
menulimits(AtkCaption) = AtkLimStr38

CONST AtkCapTime = 29
menu(AtkCapTime) = "Display Caption:"
menutype(AtkCapTime) = 3000 + AtkCapCapTime
menuoff(AtkCapTime) = AtkDatCapTime
menulimits(AtkCapTime) = AtkLimCapTime

CONST AtkCaptDelay = 30
menu(AtkCaptDelay) = "Delay Before Caption:"
menutype(AtkCaptDelay) = 19'ticks
menuoff(AtkCaptDelay) = AtkDatCaptDelay
menulimits(AtkCaptDelay) = AtkLimCaptDelay

CONST AtkBaseDef = 31
menu(AtkBaseDef) = "Base DEF Stat:"
menutype(AtkBaseDef) = 2000 + AtkCapBaseDef
menuoff(AtkBaseDef) = AtkDatBaseDef
menulimits(AtkBaseDef) = AtkLimBaseDef

CONST AtkTag = 32
menu(AtkTag) = "Set Tag"
menutype(AtkTag) = 2
menuoff(AtkTag) = AtkDatTag
menulimits(AtkTag) = AtkLimTag

CONST AtkTagIf = 33
menu(AtkTagIf) = "On"
menutype(AtkTagIf) = 2000 + AtkCapTagIf
menuoff(AtkTagIf) = AtkDatTagIf
menulimits(AtkTagIf) = AtkLimTagIf

CONST AtkTagAnd = 34
menu(AtkTagAnd) = "If Tag"
menutype(AtkTagAnd) = 2
menuoff(AtkTagAnd) = AtkDatTagAnd
menulimits(AtkTagAnd) = AtkLimTagAnd

CONST AtkTag2 = 35
menu(AtkTag2) = "Set Tag"
menutype(AtkTag2) = 2
menuoff(AtkTag2) = AtkDatTag2
menulimits(AtkTag2) = AtkLimTag

CONST AtkTagIf2 = 36
menu(AtkTagIf2) = "On"
menutype(AtkTagIf2) = 2000 + AtkCapTagIf
menuoff(AtkTagIf2) = AtkDatTagIf2
menulimits(AtkTagIf2) = AtkLimTagIf

CONST AtkTagAnd2 = 37
menu(AtkTagAnd2) = "If Tag"
menutype(AtkTagAnd2) = 2
menuoff(AtkTagAnd2) = AtkDatTagAnd2
menulimits(AtkTagAnd2) = AtkLimTagAnd

CONST AtkTagAct = 38
menu(AtkTagAct) = "Tags..."
menutype(AtkTagAct) = 1

CONST AtkDescription = 39
menu(AtkDescription) = "Description:"
menutype(AtkDescription) = 3
menuoff(AtkDescription) = AtkDatDescription
menulimits(AtkDescription) = AtkLimStr38

CONST AtkItem1 = 40
menu(AtkItem1) = "Item 1:"
menutype(AtkItem1) = 10
menuoff(AtkItem1) = AtkDatItem
menulimits(AtkItem1) = AtkLimItem

CONST AtkItemCost1 = 41
menu(AtkItemCost1) = "  Cost:"
menutype(AtkItemCost1) = 0
menuoff(AtkItemCost1) = AtkDatItemCost
menulimits(AtkItemCost1) = AtkLimInt

CONST AtkItem2 = 42
menu(AtkItem2) = "Item 2:"
menutype(AtkItem2) = 10
menuoff(AtkItem2) = AtkDatItem + 2
menulimits(AtkItem2) = AtkLimItem

CONST AtkItemCost2 = 43
menu(AtkItemCost2) = "  Cost:"
menutype(AtkItemCost2) = 0
menuoff(AtkItemCost2) = AtkDatItemCost + 2
menulimits(AtkItemCost2) = AtkLimInt

CONST AtkItem3 = 44
menu(AtkItem3) = "Item 3:"
menutype(AtkItem3) = 10
menuoff(AtkItem3) = AtkDatItem + 4
menulimits(AtkItem3) = AtkLimItem

CONST AtkItemCost3 = 45
menu(AtkItemCost3) = "  Cost:"
menutype(AtkItemCost3) = 0
menuoff(AtkItemCost3) = AtkDatItemCost + 4
menulimits(AtkItemCost3) = AtkLimInt

CONST AtkSoundEffect = 46
menu(AtkSoundEffect) = "Sound Effect:"
menutype(AtkSoundEffect) = 11
menuoff(AtkSoundEffect) = AtkDatSoundEffect
menulimits(AtkSoundEffect) = AtkLimSFX

CONST AtkPreferTarg = 47
menu(AtkPreferTarg) = "Prefer Target:"
menutype(AtkPreferTarg) = 2000 + AtkCapPreferTarg
menuoff(AtkPreferTarg) = AtkDatPreferTarg
menulimits(AtkPreferTarg) = AtkLimPreferTarg

CONST AtkPrefTargStat = 48
menu(AtkPrefTargStat) = "Weak/Strong Stat:"
menutype(AtkPrefTargStat) = 2000 + AtkCapPrefTargStat
menuoff(AtkPrefTargStat) = AtkDatPrefTargStat
menulimits(AtkPrefTargStat) = AtkLimPrefTargStat

CONST AtkChainMode = 49
menu(AtkChainMode) = "  Condition:"
menutype(AtkChainMode) = 2000 + AtkCapChainMode
menuoff(AtkChainMode) = AtkDatChainMode
menulimits(AtkChainMode) = AtkLimChainMode

CONST AtkChainVal1 = 50
menu(AtkChainVal1) = "" '--updated by update_attack_editor_for_chain()
menutype(AtkChainVal1) = 18 'skipper
menuoff(AtkChainVal1) = AtkDatChainVal1
menulimits(AtkChainVal1) = AtkLimChainVal1

CONST AtkChainVal2 = 51
menu(AtkChainVal2) = "" '--updated by update_attack_editor_for_chain()
menutype(AtkChainVal2) = 18 'skipper
menuoff(AtkChainVal2) = AtkDatChainVal2
menulimits(AtkChainVal2) = AtkLimChainVal2

CONST AtkChainBits = 52
menu(AtkChainBits) = "  option bitsets..."
menutype(AtkChainBits) = 1

CONST AtkElseChainTo = 53
menu(AtkElseChainTo) = "  Attack:"
menutype(AtkElseChainTo) = 7 '--special class for showing an attack name
menuoff(AtkElseChainTo) = AtkDatElseChainTo
menulimits(AtkElseChainTo) = AtkLimChainTo

CONST AtkElseChainRate = 54
menu(AtkElseChainRate) = "  Rate:"
menutype(AtkElseChainRate) = 17
menuoff(AtkElseChainRate) = AtkDatElseChainRate
menulimits(AtkElseChainRate) = AtkLimChainRate

CONST AtkElseChainMode = 55
menu(AtkElseChainMode) = "  Condition:"
menutype(AtkElseChainMode) = 2000 + AtkCapChainMode
menuoff(AtkElseChainMode) = AtkDatElseChainMode
menulimits(AtkElseChainMode) = AtkLimChainMode

CONST AtkElseChainVal1 = 56
menu(AtkElseChainVal1) = "" '--updated by update_attack_editor_for_chain()
menutype(AtkElseChainVal1) = 18'skipper
menuoff(AtkElseChainVal1) = AtkDatElseChainVal1
menulimits(AtkElseChainVal1) = AtkLimElseChainVal1

CONST AtkElseChainVal2 = 57
menu(AtkElseChainVal2) = "" '--updated by update_attack_editor_for_chain()
menutype(AtkElseChainVal2) = 18'skipper
menuoff(AtkElseChainVal2) = AtkDatElseChainVal2
menulimits(AtkElseChainVal2) = AtkLimElseChainVal2

CONST AtkElseChainBits = 58
menu(AtkElseChainBits) = "  Option bitsets..."
menutype(AtkElseChainBits) = 1

CONST AtkChainHeader = 59
menu(AtkChainHeader) = "[Regular Chain]"
menutype(AtkChainHeader) = 18'skipper

CONST AtkElseChainHeader = 60
menu(AtkElseChainHeader) = "[Else-Chain]"
menutype(AtkElseChainHeader) = 18'skipper

CONST AtkInsteadChainHeader = 61
menu(AtkInsteadChainHeader) = "[Instead-Chain]"
menutype(AtkInsteadChainHeader) = 18'skipper

CONST AtkInsteadChainTo = 62
menu(AtkInsteadChainTo) = "  Attack:"
menutype(AtkInsteadChainTo) = 7 '--special class for showing an attack name
menuoff(AtkInsteadChainTo) = AtkDatInsteadChainTo
menulimits(AtkInsteadChainTo) = AtkLimChainTo

CONST AtkInsteadChainRate = 63
menu(AtkInsteadChainRate) = "  Rate:"
menutype(AtkInsteadChainRate) = 17
menuoff(AtkInsteadChainRate) = AtkDatInsteadChainRate
menulimits(AtkInsteadChainRate) = AtkLimChainRate

CONST AtkInsteadChainMode = 64
menu(AtkInsteadChainMode) = "  Condition:"
menutype(AtkInsteadChainMode) = 2000 + AtkCapChainMode
menuoff(AtkInsteadChainMode) = AtkDatInsteadChainMode
menulimits(AtkInsteadChainMode) = AtkLimChainMode

CONST AtkInsteadChainVal1 = 65
menu(AtkInsteadChainVal1) = "" '--updated by update_attack_editor_for_chain()
menutype(AtkInsteadChainVal1) = 18'skipper
menuoff(AtkInsteadChainVal1) = AtkDatInsteadChainVal1
menulimits(AtkInsteadChainVal1) = AtkLimInsteadChainVal1

CONST AtkInsteadChainVal2 = 66
menu(AtkInsteadChainVal2) = "" '--updated by update_attack_editor_for_chain()
menutype(AtkInsteadChainVal2) = 18'skipper
menuoff(AtkInsteadChainVal2) = AtkDatInsteadChainVal2
menulimits(AtkInsteadChainVal2) = AtkLimInsteadChainVal2

CONST AtkInsteadChainBits = 67
menu(AtkInsteadChainBits) = "  Option bitsets..."
menutype(AtkInsteadChainBits) = 1

CONST AtkChainBrowserAct = 68
menu(AtkChainBrowserAct) = "Browse chain..."
menutype(AtkChainBrowserAct) = 1

'Next menu item is 69 (remember to update the dims)

'----------------------------------------------------------
'--menu structure
DIM workmenu(22), dispmenu(22) AS STRING
DIM state as MenuState
state.size = 22

DIM mainMenu(10)
mainMenu(0) = AtkBackAct
mainMenu(1) = AtkChooseAct
mainMenu(2) = AtkName
mainMenu(3) = AtkDescription
mainMenu(4) = AtkAppearAct
mainMenu(5) = AtkDmgAct
mainMenu(6) = AtkTargAct
mainMenu(7) = AtkCostAct
mainMenu(8) = AtkChainAct
mainMenu(9) = AtkBitAct
mainMenu(10) = AtkTagAct

DIM appearMenu(9)
appearMenu(0) = AtkBackAct
appearMenu(1) = AtkPic
appearMenu(2) = AtkPal
appearMenu(3) = AtkAnimAttack
appearMenu(4) = AtkAnimPattern
appearMenu(5) = AtkAnimAttacker
appearMenu(6) = AtkCaption
appearMenu(7) = AtkCapTime
appearMenu(8) = AtkCaptDelay
appearMenu(9) = AtkSoundEffect

DIM dmgMenu(8)
dmgMenu(0) = AtkBackAct
dmgMenu(1) = AtkDamageEq
dmgMenu(2) = AtkBaseAtk
dmgMenu(3) = AtkBaseDef
dmgMenu(4) = AtkTargStat
dmgMenu(5) = AtkExtraDamage
dmgMenu(6) = AtkAimEq
dmgMenu(7) = AtkHitX
dmgMenu(8) = AtkDelay

DIM targMenu(4)
targMenu(0) = AtkBackAct
targMenu(1) = AtkTargClass
targMenu(2) = AtkTargSetting
targMenu(3) = AtkPreferTarg
targMenu(4) = AtkPrefTargStat

DIM costMenu(9)
costMenu(0) = AtkBackAct
costMenu(1) = AtkMPCost
costMenu(2) = AtkHPCost
costMenu(3) = AtkMoneyCost
costMenu(4) = AtkItem1
costMenu(5) = AtkItemCost1
costMenu(6) = AtkItem2
costMenu(7) = AtkItemCost2
costMenu(8) = AtkItem3
costMenu(9) = AtkItemCost3

DIM chainMenu(22)
chainMenu(0) = AtkBackAct
chainMenu(1) = AtkChainBrowserAct
chainMenu(2) = AtkChainHeader
chainMenu(3) = AtkChainTo
chainMenu(4) = AtkChainRate
chainmenu(5) = AtkChainBits
chainMenu(6) = AtkChainMode
chainMenu(7) = AtkChainVal1
chainMenu(8) = AtkChainVal2
chainmenu(9) = AtkElseChainHeader
chainMenu(10) = AtkElseChainTo
chainMenu(11) = AtkElseChainRate
chainmenu(12) = AtkElseChainBits
chainMenu(13) = AtkElseChainMode
chainMenu(14) = AtkElseChainVal1
chainMenu(15) = AtkElseChainVal2
chainmenu(16) = AtkInsteadChainHeader
chainMenu(17) = AtkInsteadChainTo
chainMenu(18) = AtkInsteadChainRate
chainmenu(19) = AtkInsteadChainBits
chainMenu(20) = AtkInsteadChainMode
chainMenu(21) = AtkInsteadChainVal1
chainMenu(22) = AtkInsteadChainVal2

DIM tagMenu(6)
tagMenu(0) = AtkBackAct
tagMenu(1) = AtkTagIf
tagMenu(2) = AtkTagAnd
tagMenu(3) = AtkTag
tagMenu(4) = AtkTagIf2
tagMenu(5) = AtkTagAnd2
tagMenu(6) = AtkTag2

'--Create the box that holds the preview
DIM preview_box AS Slice Ptr
preview_box = NewSliceOfType(slRectangle)
ChangeRectangleSlice preview_box, ,uilook(uiDisabledItem), uilook(uiMenuItem), , transOpaque
'--Align the box in the bottom right
WITH *preview_box
 .X = -8
 .Y = -8
 .Width = 52
 .Height = 52
 .AnchorHoriz = 2
 .AlignHoriz = 2
 .AnchorVert = 2
 .AlignVert = 2
END WITH

'--Create the preview sprite. It will be updated before it is drawn.
DIM preview AS Slice Ptr
preview = NewSliceOfType(slSprite, preview_box)
'--Align the sprite to the center of the containing box
WITH *preview
 .AnchorHoriz = 1
 .AlignHoriz = 1
 .AnchorVert = 1
 .AlignVert = 1
END WITH

'--default starting menu
setactivemenu workmenu(), mainMenu(), state

DIM menudepth AS INTEGER = 0
DIM laststate AS MenuState
laststate.pt = 0
laststate.top = 0
DIM recindex AS INTEGER = 0
DIM lastindex AS INTEGER = 0
laststate.need_update = NO

'load data here
loadattackdata recbuf(), recindex
state.need_update = YES

DIM helpkey AS STRING = "attacks"
DIM tmpstr AS STRING

'------------------------------------------------------------------------
'--main loop

setkeys
DO
 setwait 55
 setkeys
 state.tog = state.tog XOR 1
 IF keyval(scESC) > 1 THEN
  IF menudepth = 1 THEN
   atk_edit_backptr workmenu(), mainMenu(), state, laststate, menudepth
   helpkey = "attacks"
  ELSE
   EXIT DO
  END IF
 END IF

 IF keyval(scF1) > 1 THEN show_help helpkey

 '--CTRL+BACKSPACE
 IF keyval(29) > 0 AND keyval(14) > 0 THEN
  cropafter recindex, gen(34), 0, game + ".dt6", 80, 1
  '--this is a hack to detect if it is safe to erase the extended data
  '--in the second file
  IF recindex = gen(34) THEN
   '--delete the end of attack.bin without the need to prompt
   cropafter recindex, gen(34), 0, workingdir + SLASH + "attack.bin", getbinsize(0), 0
  END IF
 END IF

 IF usemenu(state) THEN
  state.need_update = YES
  flexmenu_skipper state, workmenu(), menutype()
 END IF

 IF workmenu(state.pt) = AtkChooseAct OR (keyval(56) > 0 and NOT isStringField(menutype(workmenu(state.pt)))) THEN
  lastindex = recindex
  IF keyval(77) > 1 AND recindex = gen(genMaxAttack) AND recindex < 32767 THEN
   '--attempt to add a new set
   '--save current
   saveattackdata recbuf(), lastindex
   '--increment
   recindex = recindex + 1
   '--make sure we really have permission to increment
   IF needaddset(recindex, gen(genMaxAttack), "attack") THEN
    flusharray recbuf(), 39 + curbinsize(0) / 2, 0
    state.need_update = YES
   END IF
  ELSE
   IF intgrabber(recindex, 0, gen(genMaxAttack)) THEN
    saveattackdata recbuf(), lastindex
    loadattackdata recbuf(), recindex
    state.need_update = YES
   END IF
  END IF
 END IF

 IF enter_or_space() THEN
  SELECT CASE workmenu(state.pt)
   CASE AtkBackAct
    IF menudepth = 1 THEN
     atk_edit_backptr workmenu(), mainMenu(), state, laststate, menudepth
     helpkey = "attacks"
    ELSE
     EXIT DO
    END IF
   CASE AtkAppearAct
    atk_edit_pushptr state, laststate, menudepth
    setactivemenu workmenu(), appearMenu(), state
    helpkey = "attack_appearance"
   CASE AtkDmgAct
    atk_edit_pushptr state, laststate, menudepth
    setactivemenu workmenu(), dmgMenu(), state
    helpkey = "attack_damage"
   CASE AtkTargAct
    atk_edit_pushptr state, laststate, menudepth
    setactivemenu workmenu(), targMenu(), state
    helpkey = "attack_targetting"
   CASE AtkCostAct
    atk_edit_pushptr state, laststate, menudepth
    setactivemenu workmenu(), costMenu(), state
    helpkey = "attack_cost"
   CASE AtkChainAct
    atk_edit_pushptr state, laststate, menudepth
    setactivemenu workmenu(), chainMenu(), state
    helpkey = "attack_chaining"
   CASE AtkTagAct
    atk_edit_pushptr state, laststate, menudepth
    setactivemenu workmenu(), tagMenu(), state
    helpkey = "attack_tags"
   CASE AtkPal
    recbuf(AtkDatPal) = pal16browse(recbuf(AtkDatPal), 6, recbuf(AtkDatPic))
    state.need_update = YES
   CASE AtkBitAct
    'merge the two blocks of bitsets into the buffer
    FOR i = 0 TO 3
     buffer(i) = recbuf(AtkDatBitsets + i)
    NEXT i
    FOR i = 0 TO 7
     buffer(4 + i) = recbuf(AtkDatBitsets2 + i)
    NEXT i
    editbitset buffer(), 0, UBOUND(atkbit), atkbit()
    'split the buffer to the two bitset blocks
    FOR i = 0 TO 3
     recbuf(AtkDatBitsets + i) = buffer(i)
    NEXT i
    FOR i = 0 TO 7
     recbuf(AtkDatBitsets2 + i) = buffer(4 + i)
    NEXT i
   CASE AtkSoundEffect
    IF recbuf(AtkDatSoundEffect) > 0 THEN
     playsfx recbuf(AtkDatSoundEffect) - 1
    END IF
   CASE AtkChainBits
    editbitset recbuf(), AtkDatChainBits, UBOUND(atk_chain_bitset_names), atk_chain_bitset_names()
    state.need_update = YES
   CASE AtkElseChainBits
    editbitset recbuf(), AtkDatElseChainBits, UBOUND(atk_chain_bitset_names), atk_chain_bitset_names()
    state.need_update = YES
   CASE AtkInsteadChainBits
    editbitset recbuf(), AtkDatInsteadChainBits, UBOUND(atk_chain_bitset_names), atk_chain_bitset_names()
    state.need_update = YES
   CASE AtkChainBrowserAct
    saveattackdata recbuf(), recindex
    recindex = attack_chain_browser(recindex)
    loadattackdata recbuf(), recindex
    state.need_update = YES
  END SELECT
 END IF

 IF keyval(56) = 0 or isStringField(menutype(workmenu(state.pt))) THEN 'not pressing ALT, or not allowed to
  IF editflexmenu(workmenu(state.pt), menutype(), menuoff(), menulimits(), recbuf(), min(), max()) THEN
   state.need_update = YES
  END IF
 END IF

 IF state.need_update THEN
  '--in case new attacks have been added
  max(AtkLimChainTo) = gen(genMaxAttack) + 1
  '--in case chain mode has changed
  update_attack_editor_for_chain recbuf(AtkDatChainMode), menu(AtkChainVal1), max(AtkLimChainVal1), min(AtkLimChainVal1), menutype(AtkChainVal1), menu(AtkChainVal2), max(AtkLimChainVal2), min(AtkLimChainVal2), menutype(AtkChainVal2)
  update_attack_editor_for_chain recbuf(AtkDatElseChainMode), menu(AtkElseChainVal1), max(AtkLimElseChainVal1), min(AtkLimElseChainVal1), menutype(AtkElseChainVal1), menu(AtkElseChainVal2), max(AtkLimElseChainVal2), min(AtkLimElseChainVal2), menutype(AtkElseChainVal2)
  update_attack_editor_for_chain recbuf(AtkDatInsteadChainMode), menu(AtkInsteadChainVal1), max(AtkLimInsteadChainVal1), min(AtkLimInsteadChainVal1), menutype(AtkInsteadChainVal1), menu(AtkInsteadChainVal2), max(AtkLimInsteadChainVal2), min(AtkLimInsteadChainVal2), menutype(AtkInsteadChainVal2)
  '--re-enforce bounds, as they might have just changed
  enforceflexbounds menuoff(), menutype(), menulimits(), recbuf(), min(), max()
  '--fix caption attack caption duration
  caption(AtkCapCapTime - 1) = "ticks (" & seconds_estimate(recbuf(AtkDatCapTime)) & " sec)"
  '--percentage damage shows target stat
  caption(AtkCapDamageEq + 5) = caption(AtkCapTargStat + recbuf(AtkDatTargStat)) + " = " + STR(100 + recbuf(AtkDatExtraDamage)) + "% of Maximum"
  caption(AtkCapDamageEq + 6) = caption(AtkCapTargStat + recbuf(AtkDatTargStat)) + " = " + STR(100 + recbuf(AtkDatExtraDamage)) + "% of Current"
  updateflexmenu state.pt, dispmenu(), workmenu(), state.last, menu(), menutype(), menuoff(), menulimits(), recbuf(), caption(), max(), recindex
  '--update the picture and palette preview
  ChangeSpriteSlice preview, 6, recbuf(AtkDatPic), recbuf(AtkDatPal)
  '--done updating
  state.need_update = NO
 END IF
 atk_edit_preview recbuf(AtkDatAnimPattern), preview
 DrawSlice preview_box, dpage

 standardmenu dispmenu(), state, 0, 0, dpage
 IF keyval(56) > 0 THEN 'holding ALT
   tmpstr = readbadbinstring(recbuf(), AtkDatName, 10, 1) & " " & recindex
   textcolor uilook(uiText), uilook(uiHighlight)
   printstr tmpstr, 320 - LEN(tmpstr) * 8, 0, dpage
 END IF

 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 dowait
LOOP

'--save what we were last working on
saveattackdata recbuf(), recindex

clearallpages
DeleteSlice @preview_box

END SUB

SUB atk_edit_backptr(workmenu() AS INTEGER, mainMenu() AS INTEGER, state AS MenuState, laststate AS menustate, BYREF menudepth AS INTEGER)
 setactivemenu workmenu(), mainMenu(), state
 menudepth = 0
 state.pt = laststate.pt
 state.top = laststate.top
 state.need_update = YES
END SUB

SUB atk_edit_pushptr(state AS MenuState, laststate AS MenuState, BYREF menudepth AS INTEGER)
 laststate.pt = state.pt
 laststate.top = state.top
 menudepth = 1
END SUB

SUB atk_edit_preview(BYVAL pattern AS INTEGER, sl as Slice Ptr)
 STATIC anim0 AS INTEGER
 STATIC anim1 AS INTEGER
 anim0 = anim0 + 1
 IF anim0 > 3 THEN
  anim0 = 0
  IF pattern = 0 THEN anim1 = anim1 + 1: IF anim1 > 2 THEN anim1 = 0
  IF pattern = 1 THEN anim1 = anim1 - 1: IF anim1 < 0 THEN anim1 = 2
  IF pattern = 2 THEN anim1 = anim1 + 1: IF anim1 > 2 THEN anim1 = -1
  IF pattern = 3 THEN anim1 = INT(RND * 3)
 END IF
 ChangeSpriteSlice sl, , , ,ABS(anim1)
END SUB

FUNCTION editflexmenu (nowindex AS INTEGER, menutype() AS INTEGER, menuoff() AS INTEGER, menulimits() AS INTEGER, datablock() AS INTEGER, mintable() AS INTEGER, maxtable() AS INTEGER) AS INTEGER
'--returns true if data has changed, false it not

'nowindex is the index into the menu data of the currently selected menuitem
'menutype() holds the type of each menu element.
'           0=int
'           1=action (usually triggering a different menu)
'           2=set tag
'           3=string(bybyte)
'           4=badly stored string(by word)
'           5=chooser (not connected with data)
'           6=extra badly stored string(by word with gap)
'           7=attack number (offset!)
'           8=item number (not offset)
'           10=item number (offset!)
'           11=sound effect (offset)
'           12=defaultable positive int >=0 is int, -1 is "default"
'           13=Default zero int >0 is int, 0 is "default"
'           14=sound effect + 1 (0=default, -1=none)
'           15=speed (shows battle turn time estimate)
'           16=stat (numbered the same way as BattleStatsSingle.sta())
'           17=int with a % sign after it
'           18=skipper (caption which is skipped by the cursor)
'           19=ticks (with seconds estimate)
'           1000-1999=postcaptioned int (caption-start-offset=n-1000)
'                     (be careful about negatives!)
'           2000-2999=caption-only int (caption-start-offset=n-1000)
'                     (be careful about negatives!)
'           3000-3999=multi-state (uses caption index -1 too!)
'menuoff() is the offsets into the data block where each menu data is stored
'menulimits() is the offsets into the mintable() and maxtable() arrays
'datablock() holds the actual data
'mintable() is minimum integer values
'maxtable() is maximum int values and string limits

DIM changed AS INTEGER = 0
DIM s AS STRING

SELECT CASE menutype(nowindex)
 CASE 0, 8, 12 TO 17, 19, 1000 TO 3999' integers
  changed = intgrabber(datablock(menuoff(nowindex)), mintable(menulimits(nowindex)), maxtable(menulimits(nowindex)))
 CASE 7, 9 TO 11 'offset integers
  changed = zintgrabber(datablock(menuoff(nowindex)), mintable(menulimits(nowindex)) - 1, maxtable(menulimits(nowindex)) - 1)
 CASE 2' set tag
  changed = tag_grabber(datablock(menuoff(nowindex)), -999, 999)
 CASE 3' string
  s = readbinstring$(datablock(), menuoff(nowindex), maxtable(menulimits(nowindex)))
  IF strgrabber(s, maxtable(menulimits(nowindex))) THEN changed = 1
  writebinstring s, datablock(), menuoff(nowindex), maxtable(menulimits(nowindex))
 CASE 4' badly stored string
  s = readbadbinstring$(datablock(), menuoff(nowindex), maxtable(menulimits(nowindex)), 0)
  IF strgrabber(s, maxtable(menulimits(nowindex))) THEN changed = 1
  writebadbinstring s, datablock(), menuoff(nowindex), maxtable(menulimits(nowindex)), 0
 CASE 6' extra badly stored string
  s = readbadbinstring$(datablock(), menuoff(nowindex), maxtable(menulimits(nowindex)), 1)
  IF strgrabber(s, maxtable(menulimits(nowindex))) THEN changed = 1
  writebadbinstring s, datablock(), menuoff(nowindex), maxtable(menulimits(nowindex)), 1
END SELECT

RETURN changed

END FUNCTION

SUB enforceflexbounds (menuoff() AS INTEGER, menutype() AS INTEGER, menulimits() AS INTEGER, recbuf() AS INTEGER, min() AS INTEGER, max() AS INTEGER)

FOR i AS INTEGER = 0 TO UBOUND(menuoff)
 SELECT CASE menutype(i)
  CASE 0, 8, 12 TO 17, 1000 TO 3999
   '--bound ints
   IF menulimits(i) > 0 THEN
    '--only bound items that have real limits
    IF recbuf(menuoff(i)) < min(menulimits(i)) OR recbuf(menuoff(i)) > max(menulimits(i)) THEN
     '--detected out-of-range
     recbuf(menuoff(i)) = large(0, min(menulimits(i)))
    END IF
   END IF
 END SELECT
NEXT i

END SUB

SUB setactivemenu (workmenu(), newmenu(), BYREF state AS MenuState)
 DIM i AS INTEGER
 FOR i = 0 TO UBOUND(newmenu)
  workmenu(i) = newmenu(i)
 NEXT i
 state.pt = 0
 state.top = 0
 state.last = UBOUND(newmenu)
 state.need_update = YES
END SUB

SUB updateflexmenu (mpointer AS INTEGER, nowmenu() AS STRING, nowdat() AS INTEGER, size AS INTEGER, menu() AS STRING, menutype() AS INTEGER, menuoff() AS INTEGER, menulimits() AS INTEGER, datablock() AS INTEGER, caption() AS STRING, maxtable() AS INTEGER, recindex AS INTEGER)

'--generates a nowmenu subset from generic menu data

'nowmenu() contains the results. a menu ready to use with standardmenu
'nowdat() is a list of the indexes of which menu elements are currently on display
'size is the index of the last element in nowdat() and nowmenu()
'menu() holds all the available captions
'menutype() holds the type of each menu element.
'           0=int
'           1=action (usually triggering a different menu)
'           2=set tag
'           3=string(bybyte)
'           4=badly stored string(by word)
'           5=record chooser (not connected with data)
'           6=extra badly stored string(by word with gap)
'           7=attack number (offset)
'           8=item number (not offset)
'           9=enemy name (offset)
'           10=item name (offset)
'           11=sound effect (offset)
'           12=defaultable positive int >=0 is int, -1 is "default"
'           13=Default zero int >0 is int, 0 is "default"
'           14=sound effect + 1 (0=default, -1=none)
'           15=speed (shows battle turn time estimate)
'           16=stat (numbered the same way as BattleStatsSingle.sta())
'           17=int with a % sign after it
'           18=skipper (caption which is skipped by the cursor)
'           19=ticks (with seconds estimate)
'           1000-1999=postcaptioned int (caption-start-offset=n-1000)
'                     (be careful about negatives!)
'           2000-2999=caption-only int (caption-start-offset=n-1000)
'                     (be careful about negatives!)
'           3000-3999=Multi-state (0 and negatives are caption-only,
'                                  positive is postcaptioned. Captions are
'                                  numbered bass-ackwards )
'menuoff() tells us what index to look for the data for this menu item
'menulimits() is the offset to look in maxtable() for limits
'datablock() the actual data the menu represents
'caption() available captions for postcaptioned ints
'maxtable() used here only for max string lengths

DIM maxl AS INTEGER
DIM capnum AS INTEGER
DIM dat AS INTEGER
DIM i AS INTEGER
FOR i = 0 TO size
 dat = datablock(menuoff(nowdat(i)))
 nowmenu(i) = menu(nowdat(i))
 SELECT CASE menutype(nowdat(i))
  CASE 0 '--int
   nowmenu(i) = nowmenu(i) & " " & dat
  CASE 2 '--set tag
   nowmenu(i) = nowmenu(i) & " " & tag_condition_caption(dat, "", "NONE", "Tag 1 cannot be changed", "Tag 1 cannot be changed")
  CASE 3 '--goodstring
   maxl = maxtable(menulimits(nowdat(i)))
   nowmenu(i) = nowmenu(i) + readbinstring(datablock(), menuoff(nowdat(i)), maxl)
  CASE 4 '--badstring
   maxl = maxtable(menulimits(nowdat(i)))
   nowmenu(i) = nowmenu(i) + readbadbinstring(datablock(), menuoff(nowdat(i)), maxl, 0)
  CASE 5 '--record index
   nowmenu(i) = CHR(27) & nowmenu(i) & " " & recindex & CHR(26)
  CASE 6 '--extrabadstring
   maxl = maxtable(menulimits(nowdat(i)))
   nowmenu(i) = nowmenu(i) + readbadbinstring(datablock(), menuoff(nowdat(i)), maxl, 1)
  CASE 7 '--attack number
   IF dat <= 0 THEN
    nowmenu(i) = nowmenu(i) + " None"
   ELSE
    nowmenu(i) = nowmenu(i) & " " & (dat - 1)
    nowmenu(i) = nowmenu(i) + " " + readattackname(dat - 1)
   END IF
  CASE 8 '--item number
   nowmenu(i) = nowmenu(i) + " " + load_item_name(dat, 0, 1)
  CASE 9 '--enemy number
   IF dat <= 0 THEN
    nowmenu(i) = nowmenu(i) + " None"
   ELSE
    nowmenu(i) = nowmenu(i) & " " & (dat - 1)
    nowmenu(i) = nowmenu(i) + " " + readenemyname(dat - 1)
   END IF
  CASE 10 '--item number, offset
    nowmenu(i) = nowmenu(i) + " " + load_item_name(dat, 0, 0)
  CASE 11 '--sound effect number, offset
    IF dat <= 0 THEN
      nowmenu(i) = nowmenu(i) + " None"
    ELSE
      nowmenu(i) = nowmenu(i) + str(dat - 1 ) + " (" + getsfxname(dat - 1) + ")"
    END IF
  CASE 12 '--defaultable positive int
    nowmenu(i) = nowmenu(i) & " " & defaultint(dat)
  CASE 13 '--zero default int
    nowmenu(i) = nowmenu(i) & " " & zero_default(dat)
  CASE 14 '--sound effect number + 1 (0=default, -1=none)
    IF dat = 0 THEN
      nowmenu(i) = nowmenu(i) + " Default"
    ELSEIF dat < 0 THEN
      nowmenu(i) = nowmenu(i) + " None"
    ELSE
      nowmenu(i) = nowmenu(i) + str(dat - 1 ) + " (" + getsfxname(dat - 1) + ")"
    END IF
  CASE 15 '--speed (shows battle turn time estimate)
    nowmenu(i) = nowmenu(i) & " " & dat & " (1 turn each " & speed_estimate(dat) & ")"
  CASE 16 '--stat
    SELECT CASE dat
     CASE 0 TO 11
      nowmenu(i) = nowmenu(i) & " " & statnames(dat)
     CASE 12: nowmenu(i) = nowmenu(i) & " posion register"
     CASE 13: nowmenu(i) = nowmenu(i) & " regen register"
     CASE 14: nowmenu(i) = nowmenu(i) & " stun register"
     CASE 15: nowmenu(i) = nowmenu(i) & " mute register"
    END SELECT
  CASE 17 '--int%
   nowmenu(i) = nowmenu(i) & " " & dat & "%"
  CASE 18 '--skipper
   '--no change to caption
  CASE 19 '--ticks
   nowmenu(i) = nowmenu(i) & " " & dat & " ticks (" & seconds_estimate(dat) & " sec)"
  CASE 1000 TO 1999 '--captioned int
   capnum = menutype(nowdat(i)) - 1000
   nowmenu(i) = nowmenu(i) & " " & dat & " " & caption(capnum + dat)
  CASE 2000 TO 2999 '--caption-only int
   capnum = menutype(nowdat(i)) - 2000
   nowmenu(i) = nowmenu(i) + " " + caption(capnum + dat)
  CASE 3000 TO 3999 '--multistate
   capnum = menutype(nowdat(i)) - 3000
   IF dat > 0 THEN
    nowmenu(i) = nowmenu(i) & " " & dat & " " & caption(capnum - 1)
   ELSE
    nowmenu(i) = nowmenu(i) & " " & caption(capnum + ABS(dat))
   END IF
 END SELECT
 IF mpointer = i THEN
   nowmenu(i) = RIGHT(nowmenu(i), 40)
 END IF
NEXT i
END SUB

FUNCTION isStringField(mnu AS INTEGER)
  IF mnu = 3 OR mnu = 4 OR mnu = 6 THEN RETURN -1
  RETURN 0
END FUNCTION

SUB flexmenu_skipper (BYREF state AS MenuState, workmenu(), menutype())
 DIM loop_safety AS INTEGER = 0
 DO WHILE menutype(workmenu(state.pt)) = 18 '--skipper
  '--re-call usemenu in order to cause the keypress to be evaluated again
  usemenu state
  IF state.pt = state.first THEN EXIT DO
  IF state.pt = state.last THEN EXIT DO
  loop_safety += 1
  IF loop_safety > 50 THEN
   debug "loop safety problem in flexmenu_skipper"
   EXIT DO
  END IF
 LOOP
END SUB

'-----------------------------------------------------------------------

SUB menu_editor ()

DIM menu_set AS MenuSet
menu_set.menufile = workingdir & SLASH & "menus.bin"
menu_set.itemfile = workingdir & SLASH & "menuitem.bin"

DIM record AS INTEGER = 0

DIM state AS MenuState 'top level
state.active = YES
state.need_update = YES
DIM mstate AS MenuState 'menu
mstate.active = NO
mstate.need_update = YES
DIM dstate AS MenuState 'detail state
dstate.active = NO

DIM edmenu AS MenuDef
edmenu.align = -1
edmenu.anchor.x = -1
edmenu.anchor.y = -1
edmenu.offset.x = -160
edmenu.offset.y = -100
edmenu.boxstyle = 3
edmenu.translucent = YES
edmenu.min_chars = 38
DIM menudata AS MenuDef
LoadMenuData menu_set, menudata, record
DIM detail AS MenuDef
detail.align = -1
detail.anchor.x = -1
detail.anchor.y = 1
detail.offset.x = -152
detail.offset.y = 92
detail.min_chars = 36

DIM box_preview AS STRING = ""

setkeys
DO
 setwait 55
 setkeys
 
 IF state.active = NO THEN EXIT DO
 IF mstate.active = YES THEN
  menu_editor_menu_keys mstate, dstate, menudata, record
 ELSEIF dstate.active = YES THEN
  menu_editor_detail_keys dstate, mstate, detail, menudata.items(mstate.pt)
 ELSE
  menu_editor_keys state, mstate, menudata, record, menu_set
 END IF
 
 IF state.need_update THEN
  state.need_update = NO
  update_menu_editor_menu record, edmenu, menudata
  init_menu_state state, edmenu
  init_menu_state mstate, menudata
 END IF
 IF mstate.need_update THEN
  mstate.need_update = NO
  init_menu_state mstate, menudata
 END IF
 IF dstate.need_update THEN
  dstate.need_update = NO
  update_detail_menu detail, menudata.items(mstate.pt)
  init_menu_state dstate, detail
  WITH menudata.items(mstate.pt)
   IF .t = 3 THEN '--text box
    box_preview = textbox_preview_line(.sub_t)
   END IF
  END WITH
 END IF
 
 IF NOT mstate.active THEN draw_menu menudata, mstate, dpage
 IF NOT mstate.active AND NOT dstate.active THEN draw_menu edmenu, state, dpage
 IF mstate.active THEN
  draw_menu menudata, mstate, dpage
  edgeprint "ENTER to edit, Shift+Arrows to re-order", 0, 191, uilook(uiDisabledItem), dpage
  IF record = 0 THEN
   edgeprint "CTRL+R to reload default", 0, 181, uilook(uiDisabledItem), dpage
  END IF
 END IF
 IF dstate.active THEN
  draw_menu detail, dstate, dpage
  IF menudata.items(mstate.pt).t = 3 THEN '--textbox
   edgeprint box_preview, 0, 191, uilook(uiText), dpage
  END IF
 END IF
 
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
SaveMenuData menu_set, menudata, record

END SUB

SUB menu_editor_keys (state AS MenuState, mstate AS MenuState, menudata AS MenuDef, record, menu_set AS MenuSet)
 DIM saverecord AS INTEGER

 IF keyval(scESC) > 1 THEN state.active = NO
 IF keyval(scF1) > 1 THEN show_help "menu_editor_main"
 
 usemenu state
 
 SELECT CASE state.pt
  CASE 0
   IF enter_or_space() THEN
    state.active = NO
   END IF
  CASE 1
   saverecord = record
   IF keyval(77) > 1 AND record = gen(genMaxMenu) AND record < 32767 THEN
    '--attempt to add a new set
    '--save current
    SaveMenuData menu_set, menudata, record
    '--increment
    record = record + 1
    '--make sure we really have permission to increment
    IF needaddset(record, gen(genMaxMenu), "menu") THEN
     state.need_update = YES
    END IF
   ELSE
    IF intgrabber(record, 0, gen(genMaxMenu)) THEN
     state.need_update = YES
    END IF
   END IF
   IF state.need_update THEN
    SaveMenuData menu_set, menudata, saverecord
    LoadMenuData menu_set, menudata, record
    mstate.need_update = YES
   END IF
  CASE 2
   IF strgrabber(menudata.name, 20) THEN state.need_update = YES
  CASE 3
   IF enter_or_space() THEN
    mstate.active = YES
    mstate.need_update = YES
    menudata.edit_mode = YES
   END IF
  CASE 4
   IF intgrabber(menudata.boxstyle, 0, 14) THEN state.need_update = YES
  CASE 5
   IF intgrabber(menudata.textcolor, 0, 255) THEN state.need_update = YES
   IF enter_or_space() THEN
    menudata.textcolor = color_browser_256(menudata.textcolor)
    state.need_update = YES
   END IF
  CASE 6
   IF intgrabber(menudata.maxrows, 0, 20) THEN state.need_update = YES
  CASE 7
   IF enter_or_space() THEN
    edit_menu_bits menudata
   END IF
  CASE 8
   IF enter_or_space() THEN
    reposition_menu menudata, mstate
   END IF
  CASE 9
   IF enter_or_space() THEN
    reposition_anchor menudata, mstate
   END IF
  CASE 10 ' text align
   IF intgrabber(menudata.align, -1, 1) THEN state.need_update = YES
  CASE 11 ' Minimum width in chars
   IF intgrabber(menudata.min_chars, 0, 38) THEN state.need_update = YES
  CASE 12 ' Maximum width in chars
   IF intgrabber(menudata.max_chars, 0, 38) THEN state.need_update = YES
  CASE 13 ' border size
   IF intgrabber(menudata.bordersize, -100, 100) THEN state.need_update = YES
 END SELECT
END SUB

SUB menu_editor_menu_keys (mstate AS MenuState, dstate AS MenuState, menudata AS MenuDef, record AS INTEGER)
 DIM i AS INTEGER
 DIM elem AS INTEGER

 IF keyval(scESC) > 1 THEN
  mstate.active = NO
  menudata.edit_mode = NO
  EXIT SUB
 END IF
 IF keyval(scF1) > 1 THEN show_help "menu_editor_items"

 usemenu mstate

 IF mstate.pt >= 0 AND mstate.pt <= UBOUND(menudata.items) THEN
 WITH menudata.items(mstate.pt)
  IF .exists THEN
   strgrabber .caption, 38
   IF keyval(28) > 1 THEN '--Enter
    mstate.active = NO
    dstate.active = YES
    dstate.need_update = YES
   END IF
   IF keyval(83) > 1 THEN '-- Delete
    IF yesno("Delete this menu item?", NO) THEN
     ClearMenuItem menudata.items(mstate.pt)
     SortMenuItems menudata.items()
     mstate.need_update = YES
    END IF
   END IF
   IF keyval(42) > 0 OR keyval(54) > 0 THEN '--holding Shift
    IF keyval(72) > 1 AND mstate.pt < mstate.last - 1 THEN ' just went up
     'NOTE: Cursor will have already moved because of usemenu call above
     SWAP menudata.items(mstate.pt), menudata.items(mstate.pt + 1)
     mstate.need_update = YES
    END IF
    IF keyval(80) > 1 AND mstate.pt > mstate.first THEN ' just went down
     'NOTE: Cursor will have already moved because of usemenu call above
     SWAP menudata.items(mstate.pt), menudata.items(mstate.pt - 1)
     mstate.need_update = YES
    END IF
   END IF
  ELSE
   IF menudata.edit_mode = YES THEN
    'Selecting the item that appends new items
    IF enter_or_space() THEN
     .exists = YES
     mstate.active = NO
     mstate.need_update = YES
     dstate.active = YES
     dstate.need_update = YES
    END IF
   END IF
  END IF
 END WITH
 END IF' above block only runs with a valid mstate.pt

 IF record = 0 THEN
  IF keyval(29) > 0 AND keyval(19) > 1 THEN
   IF yesno("Reload the default main menu?") THEN
    ClearMenuData menudata
    create_default_menu menudata
    mstate.need_update = YES
   END IF
  END IF
 END IF
 
END SUB

SUB menu_editor_detail_keys(dstate AS MenuState, mstate AS MenuState, detail AS MenuDef, mi AS MenuDefItem)
 DIM max AS INTEGER

 IF keyval(scESC) > 1 THEN
  dstate.active = NO
  mstate.active = YES
  EXIT SUB
 END IF
 IF keyval(scF1) > 1 THEN show_help "menu_editor_item_details"

 usemenu dstate

 SELECT CASE dstate.pt
  CASE 0
   IF enter_or_space() THEN
    dstate.active = NO
    mstate.active = YES
    EXIT SUB
   END IF
  CASE 1: 'caption
   IF strgrabber(mi.caption, 38) THEN
    dstate.need_update = YES
   END IF
  CASE 2: 'type
   IF intgrabber(mi.t, 0, 4) THEN
    mi.sub_t = 0
    dstate.need_update = YES
   END IF
  CASE 3:
   SELECT CASE mi.t
    CASE 0: '--caption
     max = 1
    CASE 1: '--special
     max = 13
    CASE 2: '--menu
     max = gen(genMaxMenu)
    CASE 3: '--text box
     max = gen(genMaxTextBox)
   END SELECT
   IF mi.t = 4 THEN '--script
    IF scrintgrabber(mi.sub_t, 0, 0, 75, 77, 1, plottrigger) THEN dstate.need_update = YES
    IF enter_or_space() THEN
     scriptbrowse mi.sub_t, plottrigger, "Menu Item Script"
     dstate.need_update = YES
    END IF
   ELSE
    IF intgrabber(mi.sub_t, 0, max) THEN dstate.need_update = YES
   END IF
  CASE 4: 'conditional tag1
   IF tag_grabber(mi.tag1) THEN dstate.need_update = YES
  CASE 5: 'conditional tag2
   IF tag_grabber(mi.tag2) THEN dstate.need_update = YES
  CASE 6: 'set tag
   IF tag_grabber(mi.settag) THEN dstate.need_update = YES
  CASE 7: 'toggle tag
   IF tag_grabber(mi.togtag, 0) THEN dstate.need_update = YES
  CASE 8: ' bitsets
   IF enter_or_space() THEN
    edit_menu_item_bits mi
   END IF
  CASE 9 TO 11:
   IF intgrabber(mi.extra(dstate.pt - 9), -32767, 32767) THEN dstate.need_update = YES
 END SELECT

END SUB

SUB update_menu_editor_menu(record, edmenu AS MenuDef, menu AS MenuDef)
 DIM cap AS STRING
 ClearMenuItems edmenu
 
 append_menu_item edmenu, "Previous Menu"
 
 cap = "Menu " & record
 IF record = 0 THEN cap = cap & " (MAIN MENU)"
 append_menu_item edmenu, cap
 
 append_menu_item edmenu, "Name: " & menu.name
 append_menu_item edmenu, "Edit Items..."
 append_menu_item edmenu, "Background: " & menu.boxstyle
 append_menu_item edmenu, "Text color: " & zero_default(menu.textcolor)
 append_menu_item edmenu, "Max rows to display: " & zero_default(menu.maxrows)
 append_menu_item edmenu, "Edit Bitsets..."
 append_menu_item edmenu, "Reposition menu..."
 append_menu_item edmenu, "Change Anchor Point..."
 append_menu_item edmenu, "Text Align: " & sign_string(menu.align, "Left", "Center", "Right")
 append_menu_item edmenu, "Minimum width: " & zero_default(menu.min_chars, "Automatic")
 append_menu_item edmenu, "Maximum width: " & zero_default(menu.max_chars, "None")
 append_menu_item edmenu, "Border size: " & zero_default(menu.bordersize)
END SUB

SUB update_detail_menu(detail AS MenuDef, mi AS MenuDefItem)
 DIM i AS INTEGER
 DIM cap AS STRING
 DIM index AS INTEGER
 ClearMenuItems detail
 
 append_menu_item detail, "Go Back"
 
 cap = mi.caption
 IF LEN(cap) = 0 THEN cap = "[DEFAULT]"
 append_menu_item detail, "Caption: " & cap
 
 index = append_menu_item(detail, "Type")
 WITH detail.items(index)
  SELECT CASE mi.t
   CASE 0
    .caption = "Type: " & mi.t & " Caption"
   CASE 1
    .caption = "Type: " & mi.t & " Special screen"
   CASE 2
    .caption = "Type: " & mi.t & " Go to Menu"
   CASE 3
    .caption = "Type: " & mi.t & " Show text box"
   CASE 4
    .caption = "Type: " & mi.t & " Run script"
  END SELECT
 END WITH
 
 index = append_menu_item(detail, "Subtype: " & mi.sub_t)
 WITH detail.items(index)
  SELECT CASE mi.t
   CASE 0
    SELECT CASE mi.sub_t
     CASE 0: .caption = .caption & " Selectable"
     CASE 1: .caption = .caption & " Not Selectable"
    END SELECT
   CASE 1
    .caption = .caption & " " & get_special_menu_caption(mi.sub_t, YES)
   CASE 4
    .caption = "Subtype: " & scriptname$(mi.sub_t, plottrigger)
   CASE ELSE
   .caption = "Subtype: " & mi.sub_t
  END SELECT
 END WITH
 
 append_menu_item detail, tag_condition_caption(mi.tag1, "Enable if tag", "No tag check")
 append_menu_item detail, tag_condition_caption(mi.tag2, "Enable if tag", "No tag check")
 append_menu_item detail, tag_set_caption(mi.settag, "Set tag")
 append_menu_item detail, tag_toggle_caption(mi.togtag)
 append_menu_item detail, "Edit Bitsets..."
 FOR i = 0 TO 2
  append_menu_item detail, "Extra data " & i & ": " & mi.extra(i)
 NEXT i
END SUB

