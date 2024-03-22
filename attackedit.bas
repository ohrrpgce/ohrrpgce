'OHRRPGCE CUSTOM - Attack Editor, and generic flexmenu routines
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#include "config.bi"
#include "allmodex.bi"
#include "common.bi"
#include "customsubs.bi"
#include "cglobals.bi"
#include "flexmenu.bi"
#include "const.bi"
#include "loading.bi"
#include "custom.bi"
#include "thingbrowser.bi"
#include "bcommon.bi"
#include "sliceedit.bi"

'--Local SUBs
DECLARE FUNCTION atk_edit_add_new(recbuf() as integer, preview_box as Slice Ptr) as bool
DECLARE SUB atk_edit_load_attack(recbuf() as integer, attackid as integer, caption() as string, AtkCapFailConds as integer)
DECLARE SUB atk_edit_merge_bitsets(recbuf() as integer, tempbuf() as integer)
DECLARE SUB atk_edit_split_bitsets(recbuf() as integer, tempbuf() as integer)
DECLARE FUNCTION readattackname(recbuf() as integer) as string
DECLARE SUB update_attack_editor_for_fail_conds(recbuf() as integer, caption() as string, byval AtkCapFailConds as integer)
DECLARE SUB attack_editor_build_damage_menu(recbuf() as integer, menu() as string, menutype() as integer, caption() as string, menucapoff() as integer, workmenu() as integer, state as MenuState, dmgbit() as string, maskeddmgbit() as string, damagepreview as string)
DECLARE SUB attack_editor_build_appearance_menu(recbuf() as integer, workmenu() as integer, state as MenuState)
DECLARE SUB attack_editor_build_sounds_menu(recbuf() as integer, workmenu() as integer, state as MenuState)
DECLARE FUNCTION browse_base_attack_stat(byval base_num as integer) as integer

DECLARE SUB atk_edit_preview(byval pattern as integer, sl as Slice Ptr)
DECLARE SUB atk_edit_pushptr(state as MenuState, laststate as MenuState, byref menudepth as integer)
DECLARE SUB atk_edit_backptr(workmenu() as integer, mainMenu() as integer, state as MenuState, laststate as menustate, byref menudepth as integer)

DECLARE SUB attack_alignment_editor (byval attack_id as integer, byref xoff as integer, byref yoff as integer, byref halign as integer, byref valign as integer)

DECLARE SUB update_attack_editor_for_chain (byval mode as integer, byref caption1 as string, byref max1 as integer, byref min1 as integer, byref menutype1 as integer, byref caption2 as string, byref max2 as integer, byref min2 as integer, byref menutype2 as integer, rate as integer, stat as integer)
DECLARE FUNCTION attack_chain_browser (byval start_attack as integer) as integer

DECLARE FUNCTION create_attack_preview_dummy_slice(caption as string, byval parent as Slice Ptr) as Slice Ptr
DECLARE FUNCTION create_attack_preview_slice(caption as string, byval attack_id as integer, byval parent as Slice Ptr) as Slice Ptr
DECLARE SUB init_attack_chain_screen(byval attack_id as integer, state as AttackChainBrowserState)
DECLARE SUB attack_preview_slice_focus(byval sl as Slice Ptr)
DECLARE SUB attack_preview_slice_defocus(byval sl as Slice Ptr)


'--Globals
DIM counter_provoke_captions(provokeLAST) as zstring * 24 = { _
    "Default", "Always", "Never", "If attack hits", "If attack fails", "If attack misses", _
    "If attack doesn't hit", "If attack doesn't fail", "If attack doesn't miss" _
}

SUB addcaption (caption() as string, byref indexer as integer, cap as string)
 a_append caption(), cap
 indexer = UBOUND(caption) + 1
END SUB

'------------------------------ Attack Editor ----------------------------------

'--ID numbers for menu item definitions

CONST AtkBackAct = 0
CONST AtkName = 1
CONST AtkAppearAct = 2
CONST AtkDmgAct = 3
CONST AtkTargAct = 4
CONST AtkCostAct = 5
CONST AtkChainAct = 6
CONST AtkBitAct = 7
CONST AtkPic = 8
CONST AtkPal = 9
CONST AtkAnimPattern = 10
CONST AtkTargClass = 11
CONST AtkTargSetting = 12
CONST AtkChooseAct = 13
CONST AtkDamageEq = 14
CONST AtkAimEq = 15
CONST AtkBaseAtk = 16
CONST AtkMPCost = 17
CONST AtkHPCost = 18
CONST AtkMoneyCost = 19
CONST AtkExtraDamage = 20
CONST AtkChainTo = 21
CONST AtkChainRate = 22
CONST AtkAnimAttacker = 23
CONST AtkAnimAttack = 24
CONST AtkDelay = 25
CONST AtkHitX = 26
CONST AtkTargStat = 27
CONST AtkCaption = 28
CONST AtkCapTime = 29
CONST AtkCaptDelay = 30
CONST AtkBaseDef = 31
CONST AtkTag = 32
CONST AtkTagIf = 33
CONST AtkTagAnd = 34
CONST AtkTag2 = 35
CONST AtkTagIf2 = 36
CONST AtkTagAnd2 = 37
CONST AtkTagAct = 38
CONST AtkDescription = 39
CONST AtkItem1 = 40
CONST AtkItemCost1 = 41
CONST AtkItem2 = 42
CONST AtkItemCost2 = 43
CONST AtkItem3 = 44
CONST AtkItemCost3 = 45
CONST AtkSoundEffect = 46
CONST AtkPreferTarg = 47
CONST AtkPrefTargStat = 48
CONST AtkChainMode = 49
CONST AtkChainVal1 = 50
CONST AtkChainVal2 = 51
CONST AtkChainBits = 52
CONST AtkElseChainTo = 53
CONST AtkElseChainRate = 54
CONST AtkElseChainMode = 55
CONST AtkElseChainVal1 = 56
CONST AtkElseChainVal2 = 57
CONST AtkElseChainBits = 58
CONST AtkChainHeader = 59
CONST AtkElseChainHeader = 60
CONST AtkInsteadChainHeader = 61
CONST AtkInsteadChainTo = 62
CONST AtkInsteadChainRate = 63
CONST AtkInsteadChainMode = 64
CONST AtkInsteadChainVal1 = 65
CONST AtkInsteadChainVal2 = 66
CONST AtkInsteadChainBits = 67
CONST AtkChainBrowserAct = 68
CONST AtkLearnSoundEffect = 69
CONST AtkTransmogAct = 70
CONST AtkTransmogEnemy = 71
CONST AtkTransmogHp = 72
CONST AtkTransmogStats = 73
CONST AtkElementFailAct = 74
CONST AtkElementalFailHeader = 75
CONST AtkElementalFails = 76  ' to 139
CONST AtkElemBitAct = 140
CONST AtkDamageBitAct = 141
CONST AtkBlankMenuItem = 142  ' Generic blank skippable menu item
CONST AtkWepPic = 143
CONST AtkWepPal = 144
CONST AtkWepHand0 = 145
CONST AtkWepHand1 = 146
CONST AtkTurnDelay = 147
CONST AtkDramaticPause = 148
CONST AtkDamageColor = 149
CONST AtkTransmogRewards = 150
#define AtkCounterProvoke 151
CONST AtkTriggerElementalCounters = 152
CONST AtkMissSoundEffect = 153
CONST AtkFailSoundEffect = 154
CONST AtkStealFailSoundEffect = 155
CONST AtkAlignToTarget = 156
CONST AtkSoundsAct = 157
CONST AtkChangeControllable = 158
CONST AtkEffectsAct = 159
CONST AtkChangeTurncoat = 160
CONST AtkChangeDefector = 161
CONST AtkChangeFlipped = 162
CONST AtkSpawnEnemy = 163
CONST AtkChainOnFailOrMissBit = 164
CONST AtkMiscAct = 165
CONST AtkExtra0 = 166
CONST AtkExtra1 = 167
CONST AtkExtra2 = 168

'Next menu item is 165 (remember to update MnuItems)


'--Offsets in the attack data record (combined DT6 + ATTACK.BIN)

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
CONST AtkDatChainRate = 13 'See also the hacky usage of this value in updateflexmenu
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
CONST AtkDatLearnSoundEffect = 117
CONST AtkDatTransmogEnemy = 118
CONST AtkDatTransmogHp = 119
CONST AtkDatTransmogStats = 120
CONST AtkDatElementalFail = 121 'to 312
CONST AtkDatWepPic = 313
CONST AtkDatWepPal = 314
CONST AtkDatWepHand0X = 315
CONST AtkDatWepHand0Y = 316
CONST AtkDatWepHand1X = 317
CONST AtkDatWepHand1Y = 318
CONST AtkDatTurnDelay = 319
CONST AtkDatDramaticPause = 320
CONST AtkDatDamageColor = 338
CONST AtkDatTransmogRewards = 339
CONST AtkDatCounterProvoke = 340
CONST AtkDatMissSoundEffect = 341
CONST AtkDatFailSoundEffect = 342
CONST AtkDatStealFailSoundEffect = 343
CONST AtkDatXOffset = 344
CONST AtkDatYOffset = 345
CONST AtkDatHorizAlign = 346
CONST AtkDatVertAlign = 347
CONST AtkDatChangeControllable = 348
CONST AtkDatChangeTurncoat = 349
CONST AtkDatChangeDefector = 350
CONST AtkDatChangeFlipped = 351
CONST AtkDatSpawnEnemy = 352
CONST AtkDatExtra0 = 353
CONST AtkDatExtra1 = 354
CONST AtkDatExtra2 = 355

'anything past this requires expanding the data

SUB attack_editor_main ()
 IF read_config_bool("thingbrowser.enable_top_level", YES) THEN
  DIM attackb as AttackBrowser
  attackb.browse(-1, , @attack_editor)
 ELSE
  attack_editor 0
 END IF
END SUB

FUNCTION attack_picker (recindex as integer = -1) as integer
 DIM attackb as AttackBrowser
 RETURN attackb.browse(recindex, , @attack_editor, NO)
END FUNCTION

FUNCTION attack_picker_or_none (recindex as integer = -1) as integer
 DIM attackb as AttackBrowser
 RETURN attackb.browse(recindex - 1, YES , @attack_editor, NO) + 1
END FUNCTION

'recindex: which attack to show. If -1, same as last time. If >= max, ask to add a new attack,
'(and exit and return -1 if cancelled).
'Otherwise, returns the attack number we were last editing.
'Note: the attack editor can be entered recursively!
FUNCTION attack_editor (recindex as integer = -1) as integer

DIM i as integer

DIM elementnames() as string
getelementnames elementnames()

'--bitsets

REDIM atkbit() as IntStrPair

a_append atkbit(), -1, ""
a_append atkbit(), -1, " Appearance"
a_append atkbit(), 3,  "Unreversable picture"
a_append atkbit(), 55, "Show attack name"
a_append atkbit(), 56, "Don't display damage"
a_append atkbit(), 90, "Don't display ""Miss"""
a_append atkbit(), 91, "Don't display ""Fail"""
a_append atkbit(), 82, "Don't cause target to flinch"

a_append atkbit(), -1, ""
a_append atkbit(), -1, " Targetting"
a_append atkbit(), 54, "Automatically choose target"
FOR i = 0 TO 7
 a_append atkbit(), i + 37, "Cannot target enemy slot " & i
NEXT i
FOR i = 0 TO 3
 a_append atkbit(), i + 45, "Cannot target hero slot " & i
NEXT i

a_append atkbit(), -1, ""
a_append atkbit(), -1, " Effects: on Attacker"
a_append atkbit(), 52, "Store target"
a_append atkbit(), 53, "Delete stored targets"
a_append atkbit(), 92, "Always hide attacker (any attacker animation)"
a_append atkbit(), 93, "Always unhide attacker (any attacker animation)"

a_append atkbit(), -1, ""
a_append atkbit(), -1, " Effects: on Target"
a_append atkbit(), 4,  "Steal item"
a_append atkbit(), 50, "Erase rewards (Enemy targets only)"
a_append atkbit(), 72, "Reset Poison register"
a_append atkbit(), 73, "Reset Regen register"
a_append atkbit(), 74, "Reset Stun register"
a_append atkbit(), 75, "Reset Mute register"
a_append atkbit(), 76, "Cancel target's attack"
a_append atkbit(), 95, "Empty target's ready meter"
a_append atkbit(), 96, "Fill target's ready meter"

a_append atkbit(), -1, ""
a_append atkbit(), 77, "Can't be cancelled by other attacks"

a_append atkbit(), -1, ""
a_append atkbit(), -1, " Effects: Global"
a_append atkbit(), 85, "Force battle victory"
a_append atkbit(), 86, "Force battle loss & exit (no run animation)"
a_append atkbit(), 63, "Force battle loss & heroes run away"

a_append atkbit(), -1, ""
a_append atkbit(), -1, " Counter-effects"
a_append atkbit(), 78, "Don't trigger spawning on hit"
a_append atkbit(), 79, "Don't trigger spawning on kill"
a_append atkbit(), 87, "Don't trigger elemental counterattacks"  'Also in Spawning & Counterattacks menu
a_append atkbit(), 94, "Block actions as a counterattack in active mode"

a_append atkbit(), -1, ""
a_append atkbit(), -1, " Failure Conditions"
a_append atkbit(), 64, "Mutable"
a_append atkbit(), 65, "Fail if target is poisoned"
a_append atkbit(), 66, "Fail if target is regened"
a_append atkbit(), 67, "Fail if target is stunned"
a_append atkbit(), 68, "Fail if target is muted"

a_append atkbit(), -1, ""
a_append atkbit(), -1, " Costs"
a_append atkbit(), 70, "Check costs when used as a weapon"
a_append atkbit(), 80, "Check costs when used as an item"
a_append atkbit(), 81, "Re-check costs after attack delay"

a_append atkbit(), -1, ""
a_append atkbit(), -1, " Misc"
a_append atkbit(), 89, "!Useable in battle from spell lists"
a_append atkbit(), 59, "Useable outside battle from Spells menu"
a_append atkbit(), 84, "Delay doesn't block further actions"
a_append atkbit(), 71, "!Chain if attack misses or fails"  'Also in Spawning & Counterattacks menu
a_append atkbit(), 97, "Exclude from hero auto-battle"

DIM dmgbit(-1 TO 128) as string
DIM maskeddmgbit(-1 TO 128) as string  'Built in attack_editor_build_damage_menu

dmgbit(0)  = "Cure Instead of Harm"
dmgbit(1)  = "Divide Spread Damage"
dmgbit(2)  = "Absorb Damage"          'was bounceable!
dmgbit(49) = "Ignore attacker's extra hits"
dmgbit(51) = "Show damage without inflicting"
dmgbit(57) = "Reset target stat to max before hit"
dmgbit(58) = "Allow Cure to exceed maximum"
dmgbit(60) = "##Damage MP (obsolete)"
dmgbit(61) = "Do not randomize"
dmgbit(62) = "Damage can be Zero"
dmgbit(69) = "% based attacks damage instead of set"
dmgbit(83) = "Don't allow damage to exceed target stat"
dmgbit(88) = "Healing poison causes regen, and vice versa"


'--191 attack bits allowed in menu.
'--Data is split, See AtkDatBits and AtkDatBits2 for offsets


'These bits are edited separately
DIM elementbit(-1 TO 79) as string
FOR i = 0 TO small(15, gen(genNumElements) - 1)
 elementbit(i + 5) = elementnames(i) & " Damage"  'bits 5-20
NEXT i
FOR i = 16 TO gen(genNumElements) - 1
 elementbit((i - 16) + 32) = elementnames(i) & " Damage"  'bits 144-191 in the main bit array
NEXT i


DIM atk_chain_bitset_names(4) as string
atk_chain_bitset_names(0) = "Attacker must know chained attack"
atk_chain_bitset_names(1) = "Ignore chained attack's delay"
atk_chain_bitset_names(2) = "Delay doesn't block further actions"
atk_chain_bitset_names(3) = "Don't retarget if target is lost"
atk_chain_bitset_names(4) = "Invert condition"

'----------------------------------------------------------
DIM recbuf(40 + curbinsize(binATTACK) \ 2 - 1) as integer '--stores the combined attack data from both .DT6 and ATTACK.BIN

'Copy/paste buffer
STATIC copy_recbuf(40 + curbinsize(binATTACK) \ 2 - 1) as integer
STATIC have_copy as bool

CONST MnuItems = 168
DIM menu(MnuItems) as string
DIM menutype(MnuItems) as integer
DIM menuoff(MnuItems) as integer
DIM menulimits(MnuItems) as integer
DIM menucapoff(MnuItems) as integer
'----------------------------------------------------------

DIM capindex as integer = 0
REDIM caption(-1 TO -1) as string
DIM max(51) as integer
DIM min(51) as integer

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
max(AtkLimPic) = gen(genMaxAttackPic)

CONST AtkLimAnimPattern = 2
max(AtkLimAnimPattern) = 3
menucapoff(AtkAnimPattern) = capindex
addcaption caption(), capindex, "Cycle Forward"
addcaption caption(), capindex, "Cycle Back"
addcaption caption(), capindex, "Oscillate"
addcaption caption(), capindex, "Random"

CONST AtkLimTargClass = 3
max(AtkLimTargClass) = 17
menucapoff(AtkTargClass) = capindex
addcaption caption(), capindex, "Foe (not dead)"
addcaption caption(), capindex, "Ally (not dead)"
addcaption caption(), capindex, "Self"
addcaption caption(), capindex, "All (not dead)"
addcaption caption(), capindex, "Ally (including dead)"
addcaption caption(), capindex, "Ally Not Self"
addcaption caption(), capindex, "Revenge (last to hurt attacker)"
addcaption caption(), capindex, "Revenge (whole battle)"
addcaption caption(), capindex, "Previous target(s)"
addcaption caption(), capindex, "Recorded (stored) targets"
addcaption caption(), capindex, "Dead Ally (hero only)"
addcaption caption(), capindex, "Thankvenge (last to cure attacker)"
addcaption caption(), capindex, "Thankvenge (whole battle)"
addcaption caption(), capindex, "Counter (last to hit attacker)"
addcaption caption(), capindex, "All (including dead heroes)"
addcaption caption(), capindex, "Dead Foe (dead heroes only)"
addcaption caption(), capindex, "Foe (including dead heroes)"
addcaption caption(), capindex, "Most recently spawned by attack"

CONST AtkLimTargSetting = 4
max(AtkLimTargSetting) = 4
menucapoff(AtkTargSetting) = capindex
addcaption caption(), capindex, "Focused"
addcaption caption(), capindex, "Spread"
addcaption caption(), capindex, "Optional Spread"
addcaption caption(), capindex, "Random Roulette"
addcaption caption(), capindex, "First Target"

CONST AtkLimDamageEq = 5
max(AtkLimDamageEq) = 6
menucapoff(AtkDamageEq) = capindex
addcaption caption(), capindex, "Normal: ATK - DEF*.5"
addcaption caption(), capindex, "Blunt: ATK*.8 - DEF*.1"
addcaption caption(), capindex, "Sharp: ATK*1.3 - DEF"
addcaption caption(), capindex, "Pure Damage"
addcaption caption(), capindex, "No Damage"
addcaption caption(), capindex, "Set = N% of Max"
addcaption caption(), capindex, "Set = N% of Current"

CONST AtkLimAimEq = 6
max(AtkLimAimEq) = 8
menucapoff(AtkAimEq) = capindex
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
max(AtkLimBaseAtk) = 62
menucapoff(AtkBaseAtk) = capindex
addcaption caption(), capindex, statnames(statAtk) & " (attacker)"
addcaption caption(), capindex, statnames(statMagic) & " (attacker)"
addcaption caption(), capindex, statnames(statHP) & " (attacker)"
addcaption caption(), capindex, "Lost " & statnames(statHP) & " (attacker)"
addcaption caption(), capindex, "Random 0 to 999"
addcaption caption(), capindex, "100"
FOR i = 0 TO 11
 addcaption caption(), capindex, statnames(i) & " (attacker)"
NEXT
addcaption caption(), capindex, "Last damage by attacker"
addcaption caption(), capindex, "Last damage to attacker"
addcaption caption(), capindex, "Last damage to target"
addcaption caption(), capindex, "Last cure to attacker"
addcaption caption(), capindex, "Last cure to target"
FOR i = 0 TO 11
 addcaption caption(), capindex, statnames(i) & " (target)"
NEXT
FOR i = 0 TO 11
 addcaption caption(), capindex, "Max " & statnames(i) & " (attacker)"
NEXT
FOR i = 0 TO 11
 addcaption caption(), capindex, "Max " & statnames(i) & " (target)"
NEXT
addcaption caption(), capindex, "100 * number of targets"
addcaption caption(), capindex, "Lost MP (attacker)"
addcaption caption(), capindex, "Lost MP (target)"
addcaption caption(), capindex, "Lost HP (target)"

CONST AtkLimExtraDamage = 11
max(AtkLimExtraDamage) = 32767
min(AtkLimExtraDamage) = -100

CONST AtkLimChainTo = 12
max(AtkLimChainTo) = gen(genMaxAttack) + 1'--must be updated!

CONST AtkLimChainRate = 13
max(AtkLimChainRate) = 100
min(AtkLimChainRate) = 0

CONST AtkLimAnimAttacker = 14
max(AtkLimAnimAttacker) = 11
menucapoff(AtkAnimAttacker) = capindex
addcaption caption(), capindex, "Strike"
addcaption caption(), capindex, "Cast"
addcaption caption(), capindex, "Dash In"
addcaption caption(), capindex, "SpinStrike"
addcaption caption(), capindex, "Jump (hides)"
addcaption caption(), capindex, "Land (unhides)"
addcaption caption(), capindex, "Null"
addcaption caption(), capindex, "Standing Cast"
addcaption caption(), capindex, "Teleport"
addcaption caption(), capindex, "Standing Strike"
addcaption caption(), capindex, "Run and Hide (hides)"
addcaption caption(), capindex, "Run In (unhides)"

CONST AtkLimAnimAttack = 15
max(AtkLimAnimAttack) = 11
menucapoff(AtkAnimAttack) = capindex
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
addcaption caption(), capindex, "Screen Center"

CONST AtkLimDelay = 16
max(AtkLimDelay) = 1000
IF gen(genBattleMode) = 1 THEN  'Turn-based
 min(AtkLimDelay) = -1000
END IF

CONST AtkLimHitX = 17
max(AtkLimHitX) = 20
min(AtkLimHitX) = 1

CONST AtkLimTargStat = 18
max(AtkLimTargStat) = 15
menucapoff(AtkTargStat) = capindex
FOR i = 0 TO 11
 addcaption caption(), capindex, statnames(i)
NEXT
addcaption caption(), capindex, "poison register"
addcaption caption(), capindex, "regen register"
addcaption caption(), capindex, "stun register"
addcaption caption(), capindex, "mute register"

CONST AtkLimCapTime = 20
max(AtkLimCapTime) = 16383
min(AtkLimCapTime) = -1
addcaption caption(), capindex, "Ticks"  'Note: special-cased to add seconds estimate
menucapoff(AtkCapTime) = capindex
addcaption caption(), capindex, "Full Duration of Attack"
addcaption caption(), capindex, "Not at All"

CONST AtkLimCaptDelay = 21
max(AtkLimCaptDelay) = 16383
min(AtkLimCaptDelay) = 0

CONST AtkLimBaseDef = 22
max(AtkLimBaseDef) = 1 + UBOUND(statnames)
menucapoff(AtkBaseDef) = capindex
addcaption caption(), capindex, "Default"
FOR i = 0 TO UBOUND(statnames)
 addcaption caption(), capindex, statnames(i)
NEXT

CONST AtkLimTag = 23
max(AtkLimTag) = max_tag()
min(AtkLimTag) = -max_tag()

CONST AtkLimTagIf = 24
max(AtkLimTagIf) = 4
menucapoff(AtkTagIf) = capindex
'Indices are AttackTagConditionEnum
addcaption caption(), capindex, "Never:"    '0
addcaption caption(), capindex, "On Use:"   '1
addcaption caption(), capindex, "On Hit:"   '2
addcaption caption(), capindex, "On Miss:"  '3
addcaption caption(), capindex, "On Kill:"  '4

CONST AtkLimTagAnd = 25
max(AtkLimTag) = max_tag()
min(AtkLimTag) = -max_tag()

CONST AtkLimItem = 26
max(AtkLimItem) = gen(genMaxItem) + 1
min(AtkLimItem) = 0

CONST AtkLimSfx = 27
max(AtkLimSfx) = gen(genMaxSFX) + 1
min(AtkLimSfx) = 0  '0 is None

CONST AtkLimPal16 = 28
max(AtkLimPal16) = 32767
min(AtkLimPal16) = -1

CONST AtkLimPreferTarg = 29
max(AtkLimPreferTarg) = 8
min(AtkLimPreferTarg) = 0
menucapoff(AtkPreferTarg) = capindex
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
menucapoff(AtkPrefTargStat) = capindex
addcaption caption(), capindex, "same as target stat" '0
FOR i = 0 TO 15  '1 - 16
 addcaption caption(), capindex, battle_statnames(i)
NEXT

CONST AtkLimChainMode = 31
max(AtkLimChainMode) = 19
menucapoff(AtkChainMode) = capindex
addcaption caption(), capindex, "No special conditions" '0
addcaption caption(), capindex, "Tag Check"     '1
addcaption caption(), capindex, "Attacker stat > value" '2
addcaption caption(), capindex, "Attacker stat < value" '3
addcaption caption(), capindex, "Attacker stat > %"     '4
addcaption caption(), capindex, "Attacker stat < %"     '5
addcaption caption(), capindex, "Any target's stat > value" '6
addcaption caption(), capindex, "Any target's stat < value" '7
addcaption caption(), capindex, "Any target's stat > %"     '8
addcaption caption(), capindex, "Any target's stat < %"     '9
addcaption caption(), capindex, "All targets' stats > value" '10
addcaption caption(), capindex, "All targets' stats < value" '11
addcaption caption(), capindex, "All targets' stats > %"     '12
addcaption caption(), capindex, "All targets' stats < %"     '13
addcaption caption(), capindex, "All targets' stats > attacker stat" '14
addcaption caption(), capindex, "All targets' stats < attacker stat" '15
addcaption caption(), capindex, "Attacker stat < attacker stat" '16
addcaption caption(), capindex, "Stat-based chance (attacker)" '17
addcaption caption(), capindex, "Stat-based chance (max any target)" '18
addcaption caption(), capindex, "Stat-based chance (min all targets)" '19

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

CONST AtkLimTransmogStats = 38
max(AtkLimTransmogStats) = 4
min(AtkLimTransmogStats) = 0
menucapoff(AtkTransmogStats) = capindex
addcaption caption(), capindex, "keep current, copy max"  '0
addcaption caption(), capindex, "restore to new max"  '1
addcaption caption(), capindex, "preserve % of new max"   '2
addcaption caption(), capindex, "keep old current, limit to new max"  '3
addcaption caption(), capindex, "don't change current or max"  '4

CONST AtkLimTransmogEnemy = 39
max(AtkLimTransmogEnemy) = gen(genMaxEnemy) + 1 'Must be updated!
min(AtkLimTransmogEnemy) = 0

'Special case!
DIM AtkCapFailConds as integer = capindex
FOR i = 0 TO maxElements - 1
 addcaption caption(), capindex, " [No Condition]"
 addcaption caption(), capindex, "" '--updated by update_attack_editor_for_fail_conds()
NEXT

CONST AtkLimWepPic = 40
max(AtkLimWepPic) = gen(genMaxWeaponPic) + 1 ' the +1 is because 0 is used for "default"

CONST AtkLimTurnDelay = 41
max(AtkLimTurnDelay) = 1000
min(AtkLimTurnDelay) = 0

CONST AtkLimDramaticPause = 42
max(AtkLimDramaticPause) = 1000

CONST AtkLimColorIndex = 43
max(AtkLimColorIndex) = 255

CONST AtkLimTransmogRewards = 44
max(AtkLimTransmogRewards) = 1
min(AtkLimTransmogRewards) = 0
menucapoff(AtkTransmogRewards) = capindex
addcaption caption(), capindex, "Don't give rewards for old enemy"  '0
addcaption caption(), capindex, "Give rewards for old enemy"  '1

CONST AtkLimCounterProvoke = 45
max(AtkLimCounterProvoke) = provokeLAST
min(AtkLimCounterProvoke) = 0
menucapoff(AtkCounterProvoke) = capindex
FOR idx as integer = 0 TO UBOUND(counter_provoke_captions)
 addcaption caption(), capindex, counter_provoke_captions(idx)
NEXT

CONST AtkLimSfxOrDefault = 46
max(AtkLimSfxOrDefault) = gen(genMaxSFX) + 1
min(AtkLimSfxOrDefault) = -1  '0 is same as Hit sound, -1 is None

CONST AtkLimChangeControllable = 47
max(AtkLimChangeControllable) = 3
min(AtkLimChangeControllable) = 0
menucapoff(AtkChangeControllable) = capindex
addcaption caption(), capindex, "No change"  '0
addcaption caption(), capindex, "Controlled by Player"  '1
addcaption caption(), capindex, "Acts automatically"  '2
addcaption caption(), capindex, "Reset to default"  '3

CONST AtkLimChangeTurncoat = 48
max(AtkLimChangeTurncoat) = 3
min(AtkLimChangeTurncoat) = 0
menucapoff(AtkChangeTurncoat) = capindex
addcaption caption(), capindex, "No change"  '0
addcaption caption(), capindex, "Attacks allies"  '1
addcaption caption(), capindex, "Attacks foes"  '2
addcaption caption(), capindex, "Reset to default"  '3

CONST AtkLimChangeDefector = 49
max(AtkLimChangeDefector) = 3
min(AtkLimChangeDefector) = 0
menucapoff(AtkChangeDefector) = capindex
addcaption caption(), capindex, "No change"  '0
addcaption caption(), capindex, "Changes sides"  '1
addcaption caption(), capindex, "Remains with own side"  '2
addcaption caption(), capindex, "Reset to default"  '3

CONST AtkLimChangeFlipped = 50
max(AtkLimChangeFlipped) = 3
min(AtkLimChangeFlipped) = 0
menucapoff(AtkChangeFlipped) = capindex
addcaption caption(), capindex, "No change"  '0
addcaption caption(), capindex, "Flip Sprite"  '1
addcaption caption(), capindex, "Unflip Sprite"  '2
addcaption caption(), capindex, "Reset to default"  '3

CONST AtkLimSpawnEnemy = 51
max(AtkLimSpawnEnemy) = gen(genMaxEnemy) + 1 'Must be updated!
min(AtkLimSpawnEnemy) = 0

'next limit is 51 (remember to update the max() and min() dims)

'----------------------------------------------------------------------
'--menu content

menu(AtkBackAct) = "Previous Menu"
menutype(AtkBackAct) = 1

menu(AtkName) = "Name:"
menutype(AtkName) = 6
menuoff(AtkName) = AtkDatName
menulimits(AtkName) = AtkLimStr10

menu(AtkAppearAct) = "Appearance..."
menutype(AtkAppearAct) = 1

menu(AtkSoundsAct) = "Sounds..."
menutype(AtkSoundsAct) = 1

menu(AtkDmgAct) = "Damage Settings..."
menutype(AtkDmgAct) = 1

menu(AtkTargAct) = "Target and Aiming Settings..."
menutype(AtkTargAct) = 1

menu(AtkEffectsAct) = "Additional Effects..."
menutype(AtkEffectsAct) = 1

menu(AtkCostAct) = "Cost..."
menutype(AtkCostAct) = 1

menu(AtkChainAct) = "Chaining and Counterattacks..."
menutype(AtkChainAct) = 1

menu(AtkBitAct) = "Bitsets..."
menutype(AtkBitAct) = 1

menu(AtkPic) = "Picture:"
menutype(AtkPic) = 0
menuoff(AtkPic) = AtkDatPic
menulimits(AtkPic) = AtkLimPic

menu(AtkPal) = "Palette:"
menutype(AtkPal) = 12
menuoff(AtkPal) = AtkDatPal
menulimits(AtkPal) = AtkLimPal16

menu(AtkAnimPattern) = "Animation Pattern:"
menutype(AtkAnimPattern) = 2000 + menucapoff(AtkAnimPattern)
menuoff(AtkAnimPattern) = AtkDatAnimPattern
menulimits(AtkAnimPattern) = AtkLimAnimPattern

menu(AtkTargClass) = "Target Class:"
menutype(AtkTargClass) = 2000 + menucapoff(AtkTargClass)
menuoff(AtkTargClass) = AtkDatTargClass
menulimits(AtkTargClass) = AtkLimTargClass

menu(AtkTargSetting) = "Target Setting:"
menutype(AtkTargSetting) = 2000 + menucapoff(AtkTargSetting)
menuoff(AtkTargSetting) = AtkDatTargSetting
menulimits(AtkTargSetting) = AtkLimTargSetting

menu(AtkChooseAct) = "Attack"
menutype(AtkChooseAct) = 5

menu(AtkDamageEq) = "Damage Math:"
menutype(AtkDamageEq) = 2000 + menucapoff(AtkDamageEq)
menuoff(AtkDamageEq) = AtkDatDamageEq
menulimits(AtkDamageEq) = AtkLimDamageEq

menu(AtkAimEq) = "Aim Math:"
menutype(AtkAimEq) = 2000 + menucapoff(AtkAimEq)
menuoff(AtkAimEq) = AtkDatAimEq
menulimits(AtkAimEq) = AtkLimAimEq

menu(AtkBaseAtk) = "Base ATK Stat:"
menutype(AtkBaseAtk) = 2000 + menucapoff(AtkBaseAtk)
menuoff(AtkBaseAtk) = AtkDatBaseAtk
menulimits(AtkBaseAtk) = AtkLimBaseAtk

menu(AtkMPCost) = statnames(statMP) & " Cost:"
menutype(AtkMPCost) = 0
menuoff(AtkMPCost) = AtkDatMPCost
menulimits(AtkMPCost) = AtkLimInt

menu(AtkHPCost) = statnames(statHP) & " Cost:"
menutype(AtkHPCost) = 0
menuoff(AtkHPCost) = AtkDatHPCost
menulimits(AtkHPCost) = AtkLimInt

menu(AtkMoneyCost) = money_name() & " Cost:"
menutype(AtkMoneyCost) = 0
menuoff(AtkMoneyCost) = AtkDatMoneyCost
menulimits(AtkMoneyCost) = AtkLimInt

menu(AtkExtraDamage) = "Extra Damage:"
menutype(AtkExtraDamage) = 17 'int%
menuoff(AtkExtraDamage) = AtkDatExtraDamage
menulimits(AtkExtraDamage) = AtkLimExtraDamage

menu(AtkChainTo) = "  Attack:"
menutype(AtkChainTo) = 7 '--special class for showing an attack name
menuoff(AtkChainTo) = AtkDatChainTo
menulimits(AtkChainTo) = AtkLimChainTo

menu(AtkChainRate) = "  Rate:"
menutype(AtkChainRate) = 17
menuoff(AtkChainRate) = AtkDatChainRate
menulimits(AtkChainRate) = AtkLimChainRate

menu(AtkAnimAttacker) = "Attacker Animation:"
menutype(AtkAnimAttacker) = 2000 + menucapoff(AtkAnimAttacker)
menuoff(AtkAnimAttacker) = AtkDatAnimAttacker
menulimits(AtkAnimAttacker) = AtkLimAnimAttacker

menu(AtkAnimAttack) = "Attack Animation:"
menutype(AtkAnimAttack) = 2000 + menucapoff(AtkAnimAttack)
menuoff(AtkAnimAttack) = AtkDatAnimAttack
menulimits(AtkAnimAttack) = AtkLimAnimAttack

IF gen(genBattleMode) = 0 THEN  'Active-turn
 menu(AtkDelay) = "Delay Ticks Before Attack:"
 menutype(AtkDelay) = 19'ticks
ELSE
 menu(AtkDelay) = "Delay/Advance Attack:"
 menutype(AtkDelay) = 24 'attack delay
END IF
menuoff(AtkDelay) = AtkDatDelay
menulimits(AtkDelay) = AtkLimDelay

menu(AtkHitX) = "Number of Hits:"
menutype(AtkHitX) = 0
menuoff(AtkHitX) = AtkDatHitX
menulimits(AtkHitX) = AtkLimHitX

menu(AtkTargStat) = "Target Stat:"
menutype(AtkTargStat) = 2000 + menucapoff(AtkTargStat)
menuoff(AtkTargStat) = AtkDatTargStat
menulimits(AtkTargStat) = AtkLimTargStat

menu(AtkCaption) = "Caption:"
menutype(AtkCaption) = 3'goodstring
menuoff(AtkCaption) = AtkDatCaption
menulimits(AtkCaption) = AtkLimStr38

menu(AtkCapTime) = "Display Caption:"
menutype(AtkCapTime) = 3000 + menucapoff(AtkCapTime)
menuoff(AtkCapTime) = AtkDatCapTime
menulimits(AtkCapTime) = AtkLimCapTime

menu(AtkCaptDelay) = "Delay Before Caption:"
menutype(AtkCaptDelay) = 19'ticks
menuoff(AtkCaptDelay) = AtkDatCaptDelay
menulimits(AtkCaptDelay) = AtkLimCaptDelay

menu(AtkBaseDef) = "Base DEF Stat:"
menutype(AtkBaseDef) = 2000 + menucapoff(AtkBaseDef)
menuoff(AtkBaseDef) = AtkDatBaseDef
menulimits(AtkBaseDef) = AtkLimBaseDef

menu(AtkTag) = " Set Tag"
menutype(AtkTag) = 21
menuoff(AtkTag) = AtkDatTag
menulimits(AtkTag) = AtkLimTag

menu(AtkTagIf) = ""
menutype(AtkTagIf) = 2000 + menucapoff(AtkTagIf)
menuoff(AtkTagIf) = AtkDatTagIf
menulimits(AtkTagIf) = AtkLimTagIf

menu(AtkTagAnd) = " If Tag"
menutype(AtkTagAnd) = 2
menuoff(AtkTagAnd) = AtkDatTagAnd
menulimits(AtkTagAnd) = AtkLimTagAnd

menu(AtkTag2) = " Set Tag"
menutype(AtkTag2) = 21
menuoff(AtkTag2) = AtkDatTag2
menulimits(AtkTag2) = AtkLimTag

menu(AtkTagIf2) = ""
menutype(AtkTagIf2) = 2000 + menucapoff(AtkTagIf)
menuoff(AtkTagIf2) = AtkDatTagIf2
menulimits(AtkTagIf2) = AtkLimTagIf

menu(AtkTagAnd2) = " If Tag"
menutype(AtkTagAnd2) = 2
menuoff(AtkTagAnd2) = AtkDatTagAnd2
menulimits(AtkTagAnd2) = AtkLimTagAnd

menu(AtkTagAct) = "Tags..."
menutype(AtkTagAct) = 1

menu(AtkDescription) = "Description:"
menutype(AtkDescription) = 3
menuoff(AtkDescription) = AtkDatDescription
menulimits(AtkDescription) = AtkLimStr38

menu(AtkItem1) = "Item 1:"
menutype(AtkItem1) = 10
menuoff(AtkItem1) = AtkDatItem
menulimits(AtkItem1) = AtkLimItem

menu(AtkItemCost1) = "  Cost:"
menutype(AtkItemCost1) = 0
menuoff(AtkItemCost1) = AtkDatItemCost
menulimits(AtkItemCost1) = AtkLimInt

menu(AtkItem2) = "Item 2:"
menutype(AtkItem2) = 10
menuoff(AtkItem2) = AtkDatItem + 2
menulimits(AtkItem2) = AtkLimItem

menu(AtkItemCost2) = "  Cost:"
menutype(AtkItemCost2) = 0
menuoff(AtkItemCost2) = AtkDatItemCost + 2
menulimits(AtkItemCost2) = AtkLimInt

menu(AtkItem3) = "Item 3:"
menutype(AtkItem3) = 10
menuoff(AtkItem3) = AtkDatItem + 4
menulimits(AtkItem3) = AtkLimItem

menu(AtkItemCost3) = "  Cost:"
menutype(AtkItemCost3) = 0
menuoff(AtkItemCost3) = AtkDatItemCost + 4
menulimits(AtkItemCost3) = AtkLimInt

menu(AtkSoundEffect) = "Sound Effect:"  '"Hit/Default Sound Effect:"
menutype(AtkSoundEffect) = 11
menuoff(AtkSoundEffect) = AtkDatSoundEffect
menulimits(AtkSoundEffect) = AtkLimSFX

menu(AtkMissSoundEffect) = "Miss Sound Effect:"
menutype(AtkMissSoundEffect) = 27
menuoff(AtkMissSoundEffect) = AtkDatMissSoundEffect
menulimits(AtkMissSoundEffect) = AtkLimSfxOrDefault

menu(AtkFailSoundEffect) = "Fail Sound Effect:"
menutype(AtkFailSoundEffect) = 27
menuoff(AtkFailSoundEffect) = AtkDatFailSoundEffect
menulimits(AtkFailSoundEffect) = AtkLimSfxOrDefault

menu(AtkStealFailSoundEffect) = "Hit but Steal Failed Sound Effect:"
menutype(AtkStealFailSoundEffect) = 27
menuoff(AtkStealFailSoundEffect) = AtkDatStealFailSoundEffect
menulimits(AtkStealFailSoundEffect) = AtkLimSfxOrDefault

menu(AtkPreferTarg) = "Prefer Target:"
menutype(AtkPreferTarg) = 2000 + menucapoff(AtkPreferTarg)
menuoff(AtkPreferTarg) = AtkDatPreferTarg
menulimits(AtkPreferTarg) = AtkLimPreferTarg

menu(AtkPrefTargStat) = "Weak/Strong Stat:"
menutype(AtkPrefTargStat) = 2000 + menucapoff(AtkPrefTargStat)
menuoff(AtkPrefTargStat) = AtkDatPrefTargStat
menulimits(AtkPrefTargStat) = AtkLimPrefTargStat

menu(AtkChainMode) = "  Condition:"
menutype(AtkChainMode) = 2000 + menucapoff(AtkChainMode)
menuoff(AtkChainMode) = AtkDatChainMode
menulimits(AtkChainMode) = AtkLimChainMode

menu(AtkChainVal1) = "" '--updated by update_attack_editor_for_chain()
menutype(AtkChainVal1) = 18 'skipper
menuoff(AtkChainVal1) = AtkDatChainVal1
menulimits(AtkChainVal1) = AtkLimChainVal1

menu(AtkChainVal2) = "" '--updated by update_attack_editor_for_chain()
menutype(AtkChainVal2) = 18 'skipper
menuoff(AtkChainVal2) = AtkDatChainVal2
menulimits(AtkChainVal2) = AtkLimChainVal2

menu(AtkChainBits) = "  Option bitsets..."
menutype(AtkChainBits) = 1

menu(AtkElseChainTo) = "  Attack:"
menutype(AtkElseChainTo) = 7 '--special class for showing an attack name
menuoff(AtkElseChainTo) = AtkDatElseChainTo
menulimits(AtkElseChainTo) = AtkLimChainTo

menu(AtkElseChainRate) = "  Rate:"
menutype(AtkElseChainRate) = 20 'Hacky specific type
menuoff(AtkElseChainRate) = AtkDatElseChainRate
menulimits(AtkElseChainRate) = AtkLimChainRate

menu(AtkElseChainMode) = "  Condition:"
menutype(AtkElseChainMode) = 2000 + menucapoff(AtkChainMode)
menuoff(AtkElseChainMode) = AtkDatElseChainMode
menulimits(AtkElseChainMode) = AtkLimChainMode

menu(AtkElseChainVal1) = "" '--updated by update_attack_editor_for_chain()
menutype(AtkElseChainVal1) = 18'skipper
menuoff(AtkElseChainVal1) = AtkDatElseChainVal1
menulimits(AtkElseChainVal1) = AtkLimElseChainVal1

menu(AtkElseChainVal2) = "" '--updated by update_attack_editor_for_chain()
menutype(AtkElseChainVal2) = 18'skipper
menuoff(AtkElseChainVal2) = AtkDatElseChainVal2
menulimits(AtkElseChainVal2) = AtkLimElseChainVal2

menu(AtkElseChainBits) = "  Option bitsets..."
menutype(AtkElseChainBits) = 1

menu(AtkChainHeader) = "[Regular Chain]"
menutype(AtkChainHeader) = 18'skipper

menu(AtkElseChainHeader) = "[Else-Chain]"
menutype(AtkElseChainHeader) = 18'skipper

menu(AtkInsteadChainHeader) = "[Instead-Chain]"
menutype(AtkInsteadChainHeader) = 18'skipper

menu(AtkInsteadChainTo) = "  Attack:"
menutype(AtkInsteadChainTo) = 7 '--special class for showing an attack name
menuoff(AtkInsteadChainTo) = AtkDatInsteadChainTo
menulimits(AtkInsteadChainTo) = AtkLimChainTo

menu(AtkInsteadChainRate) = "  Rate:"
menutype(AtkInsteadChainRate) = 17
menuoff(AtkInsteadChainRate) = AtkDatInsteadChainRate
menulimits(AtkInsteadChainRate) = AtkLimChainRate

menu(AtkInsteadChainMode) = "  Condition:"
menutype(AtkInsteadChainMode) = 2000 + menucapoff(AtkChainMode)
menuoff(AtkInsteadChainMode) = AtkDatInsteadChainMode
menulimits(AtkInsteadChainMode) = AtkLimChainMode

menu(AtkInsteadChainVal1) = "" '--updated by update_attack_editor_for_chain()
menutype(AtkInsteadChainVal1) = 18'skipper
menuoff(AtkInsteadChainVal1) = AtkDatInsteadChainVal1
menulimits(AtkInsteadChainVal1) = AtkLimInsteadChainVal1

menu(AtkInsteadChainVal2) = "" '--updated by update_attack_editor_for_chain()
menutype(AtkInsteadChainVal2) = 18'skipper
menuoff(AtkInsteadChainVal2) = AtkDatInsteadChainVal2
menulimits(AtkInsteadChainVal2) = AtkLimInsteadChainVal2

menu(AtkInsteadChainBits) = "  Option bitsets..."
menutype(AtkInsteadChainBits) = 1

menu(AtkChainBrowserAct) = "Browse chain..."
menutype(AtkChainBrowserAct) = 1

menu(AtkLearnSoundEffect) = "Sound When Learned:"
menutype(AtkLearnSoundEffect) = 11
menuoff(AtkLearnSoundEffect) = AtkDatLearnSoundEffect
menulimits(AtkLearnSoundEffect) = AtkLimSFX

menu(AtkTransmogAct) = "Transmogrification..."
menutype(AtkTransmogAct) = 1

menu(AtkMiscAct) = "Misc..."
menutype(AtkMiscAct) = 1

menu(AtkTransmogEnemy) = "Enemy target becomes:"
menutype(AtkTransmogEnemy) = 9 'enemy name
menuoff(AtkTransmogEnemy) = AtkDatTransmogEnemy
menulimits(AtkTransmogEnemy) = AtkLimTransmogEnemy

menu(AtkTransmogHp) = "Health:"
menutype(AtkTransmogHp) = 2000 + menucapoff(AtkTransmogStats)
menuoff(AtkTransmogHp) = AtkDatTransmogHp
menulimits(AtkTransmogHp) = AtkLimTransmogStats

menu(AtkTransmogStats) = "Other stats:"
menutype(AtkTransmogStats) = 2000 + menucapoff(AtkTransmogStats)
menuoff(AtkTransmogStats) = AtkDatTransmogStats
menulimits(AtkTransmogStats) = AtkLimTransmogStats

menu(AtkTransmogRewards) = "Rewards:"
menutype(AtkTransmogRewards) = 2000 + menucapoff(AtkTransmogRewards)
menuoff(AtkTransmogRewards) = AtkDatTransmogRewards
menulimits(AtkTransmogRewards) = AtkLimTransmogRewards

menu(AtkCounterProvoke) = "Provoke counterattacks:"
menutype(AtkCounterProvoke) = menucapoff(AtkCounterProvoke)
menuoff(AtkCounterProvoke) = AtkDatCounterProvoke
menulimits(AtkCounterProvoke) = AtkLimCounterProvoke

menu(AtkTriggerElementalCounters) = " Never trigger elemental counterattacks:"
menutype(AtkTriggerElementalCounters) = 7000 + 87  'Attack bit 87

menu(AtkChainOnFailOrMissBit) = "!Chain if attack misses or fails:"
menutype(AtkChainOnFailOrMissBit) = 7000 + 71  'Attack bit 71, inverted

menu(AtkElementFailAct) = "Elemental failure conditions..."
menutype(AtkElementFailAct) = 1

menu(AtkElementalFailHeader) = "Fail when target's damage..."
menutype(AtkElementalFailHeader) = 18  'skip

FOR i = 0 TO small(maxElements, gen(genNumElements)) - 1
 menu(AtkElementalFails + i) = " from " + rpad(elementnames(i), " ", 15, clipRight)
 menutype(AtkElementalFails + i) = 4000 + AtkCapFailConds + i * 2  'percent_cond_grabber
 menuoff(AtkElementalFails + i) = AtkDatElementalFail + i * 3
NEXT

menu(AtkElemBitAct) = "Elemental bits..."
menutype(AtkElemBitAct) = 1

menu(AtkDamageBitAct) = "Damage bitsets..."
menutype(AtkDamageBitAct) = 1

menu(AtkBlankMenuItem) = ""
menutype(AtkBlankMenuItem) = 18  'skip

menu(AtkWepPic) = "Weapon Picture:"
menutype(AtkWepPic) = 26
menuoff(AtkWepPic) = AtkDatWepPic
menulimits(AtkWepPic) = AtkLimWepPic

menu(AtkWepPal) = "Weapon Palette:"
menutype(AtkWepPal) = 12
menuoff(AtkWepPal) = AtkDatWepPal
menulimits(AtkWepPal) = AtkLimPal16

menu(AtkWepHand0) = "Weapon handle for first frame"
menutype(AtkWepHand0) = 1

menu(AtkWepHand1) = "Weapon handle for second frame"
menutype(AtkWepHand1) = 1

menu(AtkTurnDelay) = "Delay Turns Before Attack:"
menutype(AtkTurnDelay) = 0
menuoff(AtkTurnDelay) = AtkDatTurnDelay
menulimits(AtkTurnDelay) = AtkLimTurnDelay

menu(AtkDramaticPause) = "Dramatic Pause Ticks:"
menutype(AtkDramaticPause) = 19'ticks
menuoff(AtkDramaticPause) = AtkDatDramaticPause
menulimits(AtkDramaticPause) = AtkLimDramaticPause

menu(AtkDamageColor) = "Damage Color:"
menutype(AtkDamageColor) = 23'color
menuoff(AtkDamageColor) = AtkDatDamageColor
menulimits(AtkDamageColor) = AtkLimColorIndex

menu(AtkAlignToTarget) = "Attack Animation Align to Target..."
menutype(AtkAlignToTarget) = 1

menu(AtkChangeControllable) = "Change Target Control:"
menutype(AtkChangeControllable) = 2000 + menucapoff(AtkChangeControllable)
menuoff(AtkChangeControllable) = AtkDatChangeControllable
menulimits(AtkChangeControllable) = AtkLimChangeControllable

menu(AtkChangeTurncoat) = "Make Target a Turncoat:"
menutype(AtkChangeTurncoat) = 2000 + menucapoff(AtkChangeTurncoat)
menuoff(AtkChangeTurncoat) = AtkDatChangeTurncoat
menulimits(AtkChangeTurncoat) = AtkLimChangeTurncoat

menu(AtkChangeDefector) = "Make Target a Defector:"
menutype(AtkChangeDefector) = 2000 + menucapoff(AtkChangeDefector)
menuoff(AtkChangeDefector) = AtkDatChangeDefector
menulimits(AtkChangeDefector) = AtkLimChangeDefector

menu(AtkChangeFlipped) = "Flip Target Sprite:"
menutype(AtkChangeFlipped) = 2000 + menucapoff(AtkChangeFlipped)
menuoff(AtkChangeFlipped) = AtkDatChangeFlipped
menulimits(AtkChangeFlipped) = AtkLimChangeFlipped

menu(AtkSpawnEnemy) = "Spawn Enemy (If Room):"
menutype(AtkSpawnEnemy) = 9 'enemy name
menuoff(AtkSpawnEnemy) = AtkDatSpawnEnemy
menulimits(AtkSpawnEnemy) = AtkLimSpawnEnemy

menu(AtkExtra0) = "Extra Data 0:"
menutype(AtkExtra0) = 0
menuoff(AtkExtra0) = AtkDatExtra0
menulimits(AtkExtra0) = AtkLimInt

menu(AtkExtra1) = "Extra Data 1:"
menutype(AtkExtra1) = 0
menuoff(AtkExtra1) = AtkDatExtra1
menulimits(AtkExtra1) = AtkLimInt

menu(AtkExtra2) = "Extra Data 2:"
menutype(AtkExtra2) = 0
menuoff(AtkExtra2) = AtkDatExtra2
menulimits(AtkExtra2) = AtkLimInt

'----------------------------------------------------------
'--menu structure
DIM workmenu(65) as integer
DIM dispmenu(65) as string
DIM state as MenuState
state.autosize = YES
state.autosize_ignore_pixels = 16
DIM menuopts as MenuOptions
menuopts.fullscreen_scrollbar = YES

DIM mainMenu(16) as integer
mainMenu(0) = AtkBackAct
mainMenu(1) = AtkChooseAct
mainMenu(2) = AtkName
mainMenu(3) = AtkDescription
mainMenu(4) = AtkAppearAct
mainMenu(5) = AtkSoundsAct
mainMenu(6) = AtkTargAct
mainMenu(7) = AtkDmgAct
mainMenu(8) = AtkCostAct
mainMenu(9) = AtkChainAct
mainMenu(10) = AtkBitAct
mainMenu(11) = AtkEffectsAct
mainMenu(12) = AtkElemBitAct
mainMenu(13) = AtkElementFailAct
mainMenu(14) = AtkTagAct
mainMenu(15) = AtkTransmogAct
mainMenu(16) = AtkMiscAct

DIM targMenu(5) as integer
targMenu(0) = AtkBackAct
targMenu(1) = AtkAimEq
targMenu(2) = AtkTargClass
targMenu(3) = AtkTargSetting
targMenu(4) = AtkPreferTarg
targMenu(5) = AtkPrefTargStat

DIM costMenu(9) as integer
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

DIM chainMenu(25) as integer
chainMenu(0) = AtkBackAct
chainMenu(1) = AtkChainBrowserAct
chainMenu(2) = AtkChainHeader
chainMenu(3) = AtkChainTo
chainMenu(4) = AtkChainRate
chainMenu(5) = AtkChainBits
chainMenu(6) = AtkChainMode
chainMenu(7) = AtkChainVal1
chainMenu(8) = AtkChainVal2
chainMenu(9) = AtkElseChainHeader
chainMenu(10) = AtkElseChainTo
chainMenu(11) = AtkElseChainRate
chainMenu(12) = AtkElseChainBits
chainMenu(13) = AtkElseChainMode
chainMenu(14) = AtkElseChainVal1
chainMenu(15) = AtkElseChainVal2
chainMenu(16) = AtkInsteadChainHeader
chainMenu(17) = AtkInsteadChainTo
chainMenu(18) = AtkInsteadChainRate
chainMenu(19) = AtkInsteadChainBits
chainMenu(20) = AtkInsteadChainMode
chainMenu(21) = AtkInsteadChainVal1
chainMenu(22) = AtkInsteadChainVal2
chainMenu(23) = AtkChainOnFailOrMissBit
chainMenu(24) = AtkCounterProvoke
chainMenu(25) = AtkTriggerElementalCounters

DIM tagMenu(6) as integer
tagMenu(0) = AtkBackAct
tagMenu(1) = AtkTagIf
tagMenu(2) = AtkTagAnd
tagMenu(3) = AtkTag
tagMenu(4) = AtkTagIf2
tagMenu(5) = AtkTagAnd2
tagMenu(6) = AtkTag2

DIM transmogMenu(4) as integer
transmogMenu(0) = AtkBackAct
transmogMenu(1) = AtkTransmogEnemy
transmogMenu(2) = AtkTransmogHp
transmogMenu(3) = AtkTransmogStats
transmogMenu(4) = AtkTransmogRewards

DIM elementFailMenu(gen(genNumElements) + 1) as integer
elementFailMenu(0) = AtkBackAct
elementFailMenu(1) = AtkElementalFailHeader
FOR i = 0 TO gen(genNumElements) - 1
 elementFailMenu(2 + i) = AtkElementalFails + i
NEXT

DIM effectsMenu(5) as integer
effectsMenu(0) = AtkBackAct
effectsMenu(1) = AtkChangeControllable
effectsMenu(2) = AtkChangeTurncoat
effectsMenu(3) = AtkChangeDefector
effectsMenu(4) = AtkChangeFlipped
effectsMenu(5) = AtkSpawnEnemy

DIM miscMenu(3) as integer
miscMenu(0) = AtkBackAct
miscMenu(1) = AtkExtra0
miscMenu(2) = AtkExtra1
miscMenu(3) = AtkExtra2

'--Create the box that holds the preview
DIM preview_box as Slice Ptr
preview_box = NewSliceOfType(slRectangle)
ChangeRectangleSlice preview_box, , uilook(uiDisabledItem), uilook(uiMenuItem), , transOpaque
'--Align the box in the bottom right
WITH *preview_box
 .X = -8
 .Y = -8
 .CoverChildren = YES
 .PaddingTop = 1
 .PaddingBottom = 1
 .PaddingLeft = 1
 .PaddingRight = 1
 .AnchorHoriz = alignRight
 .AlignHoriz = alignRight
 .AnchorVert = alignBottom
 .AlignVert = alignBottom
END WITH

'--Create the preview sprite. It will be updated before it is drawn.
DIM preview as Slice Ptr
preview = NewSliceOfType(slSprite, preview_box)
'--Align the sprite to the center of the containing box
WITH *preview
 .AnchorHoriz = alignCenter
 .AlignHoriz = alignCenter
 .AnchorVert = alignCenter
 .AlignVert = alignCenter
END WITH

'--Create the weapon preview sprite. It will be updated before it is drawn.
DIM weppreview as Slice Ptr
weppreview = NewSliceOfType(slSprite, preview_box)
'--Align the sprite to the top of the containing box
WITH *weppreview
 .Visible = NO
 .AnchorHoriz = alignCenter
 .AlignHoriz = alignCenter
 .AnchorVert = alignBottom
 .AlignVert = alignTop
END WITH

DIM damagepreview as string

'--default starting menu
setactivemenu workmenu(), mainMenu(), state
state.pt = 1  'Select <-Attack ..-> line
state.size = 25
state.autosize = YES

DIM selectable() as bool
flexmenu_update_selectable workmenu(), menutype(), selectable()

DIM menudepth as integer = 0
DIM laststate as MenuState
laststate.pt = 0
laststate.top = 0

laststate.need_update = NO

STATIC rememberindex as integer = -1   'Record to switch to with TAB
DIM show_name_ticks as integer = 0  'Number of ticks to show name (after switching record with TAB)

DIM remember_atk_bit as integer = -1
DIM remember_dmg_bit as integer = -1
DIM remember_elmt_bit as integer = -1
DIM drawpreview as bool = YES
STATIC warned_old_fail_bit as bool = NO

'Which attack to show?
STATIC remember_recindex as integer = 0
IF recindex < 0 THEN
 recindex = remember_recindex
ELSE
 IF recindex > gen(genMaxAttack) THEN
  IF atk_edit_add_new(recbuf(), preview_box) THEN
   'Added a new record (blank or copy)
   saveattackdata recbuf(), recindex
   recindex = gen(genMaxAttack)
  ELSE
   DeleteSlice @preview_box
   RETURN -1
  END IF
 END IF
END IF

'load data here
atk_edit_load_attack recbuf(), recindex, caption(), AtkCapFailConds
state.need_update = YES

'As a hack (I blame it on flexmenu itself which tries to be more "flexible" than possible),
'helpkey is used to tell us which submenu we're in
DIM helpkey as string = "attacks"
DIM tmpstr as string

'------------------------------------------------------------------------
'--main loop

setkeys YES
DO
 setwait 55
 setkeys YES
 IF keyval(ccCancel) > 1 THEN
  IF menudepth = 1 THEN
   atk_edit_backptr workmenu(), mainMenu(), state, laststate, menudepth
   flexmenu_update_selectable workmenu(), menutype(), selectable()
   helpkey = "attacks"
   drawpreview = YES
   damagepreview = ""
  ELSE
   EXIT DO
  END IF
 END IF

 IF keyval(scF1) > 1 THEN show_help helpkey

 '--SHIFT+BACKSPACE
 IF cropafter_keycombo(workmenu(state.pt) = AtkChooseAct) THEN
  IF cropafter(recindex, gen(genMaxAttack), game & ".dt6", 80) THEN
   'User confirmed the prompt, also crop attack.bin without prompt
   cropafter recindex, gen(genMaxAttack), workingdir & SLASH & "attack.bin", getbinsize(binATTACK), NO  'prompt=NO
  END IF
 END IF

 'Copy-paste
 IF workmenu(state.pt) = AtkChooseAct THEN
  IF copy_keychord() THEN
   a_copy recbuf(), copy_recbuf()
   have_copy = YES
   show_overlay_message "Copied attack", 1.2
  END IF
  IF have_copy ANDALSO paste_keychord() THEN
   IF yesno("Really overwrite this attack by pasting " & readattackname(copy_recbuf()) & "?") THEN
    a_copy copy_recbuf(), recbuf()
    state.need_update = YES
   END IF
  END IF
 END IF

 IF usemenu(state, selectable()) THEN
  state.need_update = YES
 END IF

 IF workmenu(state.pt) = AtkChooseAct OR (keyval(scAlt) > 0 and NOT isStringField(menutype(workmenu(state.pt)))) THEN
  DIM lastindex as integer = recindex
  IF intgrabber_with_addset(recindex, 0, gen(genMaxAttack), maxMaxAttacks, "attack") THEN
   saveattackdata recbuf(), lastindex
   IF recindex > gen(genMaxAttack) THEN
    IF atk_edit_add_new(recbuf(), preview_box) THEN
     'Added a new record (blank or copy)
     saveattackdata recbuf(), recindex
     'Reload, to set fail cond captions correctly
    ELSE
     'cancelled add, reload the old last record
     recindex -= 1
    END IF
   ELSE
    'Load attack
   END IF
   atk_edit_load_attack recbuf(), recindex, caption(), AtkCapFailConds
   state.need_update = YES
  END IF
 END IF

 IF keyval(scTab) > 1 THEN
  IF keyval(scShift) > 0 THEN
   rememberindex = recindex
  ELSEIF rememberindex >= 0 AND rememberindex <= gen(genMaxAttack) THEN
   saveattackdata recbuf(), recindex
   SWAP rememberindex, recindex
   atk_edit_load_attack recbuf(), recindex, caption(), AtkCapFailConds
   state.need_update = YES
   show_name_ticks = 23
  END IF
 END IF

 'Debug key CTRL-B: edit all bitsets
 IF keyval(scCtrl) > 0 AND keyval(scB) > 1 THEN
  DIM allbits(127) as string
  FOR i = 0 TO UBOUND(atkbit)
   IF atkbit(i).i >= 0 THEN allbits(atkbit(i).i) = atkbit(i).s
  NEXT
  FOR i = 0 TO UBOUND(dmgbit)
   IF LEN(dmgbit(i)) THEN allbits(i) = dmgbit(i)
  NEXT

  'Obsolete bits
  FOR i = 0 TO 7
   DIM elname as string
   elname = IIF(i <= UBOUND(elementnames), elementnames(i), "element" & i)
   allbits(i + 5) = "##" & elname & " Damage (obsolete)" '05-12
   allbits(i + 13) = "##Bonus vs " & readglobalstring(9 + i, "EnemyType (obsolete)" & i+1) '13-20
   allbits(i + 21) = "##Fail vs " & elname & " resistance (obsolete)" '21-28
   allbits(i + 29) = "##Fail vs " & readglobalstring(9 + i, "EnemyType" & i+1) & " (obsolete)" '29-36
  NEXT i

  atk_edit_merge_bitsets recbuf(), buffer()
  editbitset buffer(), 0, allbits(), "attack_bitsets", , , , , YES, YES  'show_index = show_all = YES
  atk_edit_split_bitsets recbuf(), buffer()
  state.need_update = YES
 END IF

 IF enter_space_click(state) THEN
  DIM nowindex as integer = workmenu(state.pt)
  SELECT CASE menutype(nowindex)
   CASE 8 ' Item
    recbuf(menuoff(nowindex)) = item_picker(recbuf(menuoff(nowindex)))
    max(AtkLimItem) = gen(genMaxItem) + 1
    state.need_update = YES
   CASE 10 ' Item with offset
    recbuf(menuoff(nowindex)) = item_picker_or_none(recbuf(menuoff(nowindex)))
    max(AtkLimItem) = gen(genMaxItem) + 1
    state.need_update = YES
  END SELECT
  SELECT CASE nowindex
   CASE AtkChooseAct
    'The <-Attack #-> line; enter exits so that if we were called from another menu
    'it is easy to select an attack and return to it.
    EXIT DO
   CASE AtkBackAct
    IF menudepth = 1 THEN
     atk_edit_backptr workmenu(), mainMenu(), state, laststate, menudepth
     helpkey = "attacks"
     drawpreview = YES
     damagepreview = ""
    ELSE
     EXIT DO
    END IF
   CASE AtkAppearAct
    'Special case
    atk_edit_pushptr state, laststate, menudepth
    state.pt = 0
    state.need_update = YES
    drawpreview = YES
    helpkey = "attack_appearance"
   CASE AtkSoundsAct
    'Special case
    atk_edit_pushptr state, laststate, menudepth
    state.pt = 0
    state.need_update = YES
    drawpreview = YES
    helpkey = "attack_sounds"
   CASE AtkDmgAct
    'Special case
    atk_edit_pushptr state, laststate, menudepth
    state.pt = 0
    state.need_update = YES
    helpkey = "attack_damage"
    drawpreview = NO
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
   CASE AtkElementFailAct
    atk_edit_pushptr state, laststate, menudepth
    setactivemenu workmenu(), elementFailMenu(), state
    helpkey = "attack_elementfail"
    IF prefbit(25) ANDALSO warned_old_fail_bit = NO THEN
     'Show warning about 'Simulate old fail vs. element resist bit'
     show_help "attack_warn_old_fail_bit"
     warned_old_fail_bit = YES
    END IF
    drawpreview = NO
   CASE AtkTagAct
    atk_edit_pushptr state, laststate, menudepth
    setactivemenu workmenu(), tagMenu(), state
    helpkey = "attack_tags"
   CASE AtkTransmogAct
    atk_edit_pushptr state, laststate, menudepth
    setactivemenu workmenu(), transmogMenu(), state
    helpkey = "attack_transmogrify"
   CASE AtkPic
    DIM attackb as AttackSpriteBrowser
    recbuf(AtkDatPic) = attackb.browse(recbuf(AtkDatPic))
    state.need_update = YES
   CASE AtkPal
    recbuf(AtkDatPal) = pal16browse(recbuf(AtkDatPal), sprTypeAttack, recbuf(AtkDatPic), YES)
    state.need_update = YES
   CASE AtkWepPic
    DIM weaponb as WeaponSpriteBrowser
    recbuf(AtkDatWepPic) = weaponb.browse(recbuf(AtkDatWepPic) - 1, YES) + 1
    state.need_update = YES
   CASE AtkWepPal
    IF recbuf(AtkDatWepPic) > 0 THEN
     recbuf(AtkDatWepPal) = pal16browse(recbuf(AtkDatWepPal), sprTypeAttack, recbuf(AtkDatWepPic), YES)
     state.need_update = YES
    END IF
   CASE AtkBitAct
    atk_edit_merge_bitsets recbuf(), buffer()
    editbitset buffer(), 0, atkbit(), "attack_bitsets", remember_atk_bit, , readattackname(recbuf()) & " general bitsets"
    atk_edit_split_bitsets recbuf(), buffer()
   CASE AtkDamageBitAct
    'Every time the user toggles a bit editbitset quits immediately, we refresh
    'the list of applicable bits, and reenter
    DIM editret as EditBitsetResult
    DO
     atk_edit_merge_bitsets recbuf(), buffer()
     editret = editbitset(buffer(), 0, maskeddmgbit(), "attack_damage_bitsets", remember_dmg_bit, YES, readattackname(recbuf()) & " damage bitsets")
     atk_edit_split_bitsets recbuf(), buffer()
     IF editret = edbitPickedBit THEN
      attack_editor_build_damage_menu recbuf(), menu(), menutype(), caption(), menucapoff(), workmenu(), state, dmgbit(), maskeddmgbit(), damagepreview
     ELSE  'User quit editbitset
      EXIT DO
     END IF
    LOOP
    'Bitsets have complicated effects
    state.need_update = YES
   CASE AtkElemBitAct
    'merge the two blocks of bitsets into the buffer
    FOR i = 0 TO 1
     'includes bits 5 - 20
     buffer(i) = recbuf(AtkDatBitsets + i)
    NEXT i
    FOR i = 0 TO 2
     'bits 80 - 127
     buffer(2 + i) = recbuf(AtkDatBitsets2 + 5 + i)
    NEXT i
    editbitset buffer(), 0, elementbit(), "attack_element_bitsets", remember_elmt_bit, , readattackname(recbuf()) & " elements"
    'split the buffer to the two bitset blocks
    FOR i = 0 TO 1
     recbuf(AtkDatBitsets + i) = buffer(i)
    NEXT i
    FOR i = 0 TO 2
     recbuf(AtkDatBitsets2 + 5 + i) = buffer(2 + i)
    NEXT i
   CASE AtkChainBits
    editbitset recbuf(), AtkDatChainBits, atk_chain_bitset_names(), "attack_chain_bitsets"
    state.need_update = YES
   CASE AtkElseChainBits
    editbitset recbuf(), AtkDatElseChainBits, atk_chain_bitset_names(), "attack_chain_bitsets"
    state.need_update = YES
   CASE AtkInsteadChainBits
    editbitset recbuf(), AtkDatInsteadChainBits, atk_chain_bitset_names(), "attack_chain_bitsets"
    state.need_update = YES
   CASE AtkChainBrowserAct
    saveattackdata recbuf(), recindex
    recindex = attack_chain_browser(recindex)
    atk_edit_load_attack recbuf(), recindex, caption(), AtkCapFailConds
    state.need_update = YES
   CASE AtkElementalFails TO AtkElementalFails + maxElements - 1
    DIM cond as AttackElementCondition
    DeSerAttackElementCond cond, recbuf(), menuoff(workmenu(state.pt))
    percent_cond_editor cond, -1000.0, 1000.0, 4, "Fail", " damage" + menu(workmenu(state.pt))  'Fail when ... damage from <elem>
    SerAttackElementCond cond, recbuf(), menuoff(workmenu(state.pt))
    state.need_update = YES
   CASE AtkWepHand0
    xy_position_on_sprite_slice weppreview, recbuf(AtkDatWepHand0X), recbuf(AtkDatWepHand0Y), "Weapon handle position", "xy_weapon_handle"
   CASE AtkWepHand1
    ChangeSpriteSlice weppreview, , , , 1
    xy_position_on_sprite_slice weppreview, recbuf(AtkDatWepHand1X), recbuf(AtkDatWepHand1Y), "Weapon handle position", "xy_weapon_handle"
    ChangeSpriteSlice weppreview, , , , 0
   CASE AtkBaseAtk
    recbuf(AtkDatBaseAtk) = browse_base_attack_stat(recbuf(AtkDatBaseAtk))
    state.need_update = YES
   CASE AtkDamageColor
    SELECT CASE twochoice("Damage Color Override", "Use Default", "Pick a specific color")
     CASE 0: recbuf(AtkDatDamageColor) = 0
     CASE 1: recbuf(AtkDatDamageColor) = color_browser_256(recbuf(AtkDatDamageColor))
    END SELECT
    state.need_update = YES
   CASE AtkAlignToTarget
    saveattackdata recbuf(), recindex
    attack_alignment_editor recindex, recbuf(AtkDatXOffset), recbuf(AtkDatYOffset), recbuf(AtkDatHorizAlign), recbuf(AtkDatVertAlign)
    state.need_update = YES
   CASE AtkEffectsAct
    atk_edit_pushptr state, laststate, menudepth
    setactivemenu workmenu(), effectsMenu(), state
    helpkey = "attack_effects"
   CASE AtkMiscAct
    atk_edit_pushptr state, laststate, menudepth
    setactivemenu workmenu(), miscMenu(), state
    helpkey = "attack_misc"
  END SELECT
 END IF

 IF keyval(scAlt) = 0 OR isStringField(menutype(workmenu(state.pt))) THEN 'not pressing ALT, or not allowed to
  IF editflexmenu(state, workmenu(state.pt), menutype(), menuoff(), menulimits(), recbuf(), caption(), min(), max()) THEN
   state.need_update = YES
  END IF
 END IF

 IF flexmenu_handle_crossrefs(state, workmenu(state.pt), menutype(), menuoff(), recindex, recbuf(), YES) THEN
  'Reload this attack in case it was changed in recursive call to the editor (in fact, this record might be deleted!)
  recindex = small(recindex, gen(genMaxAttack))
  atk_edit_load_attack recbuf(), recindex, caption(), AtkCapFailConds
  show_name_ticks = 23
  state.need_update = YES
 END IF

 IF state.need_update THEN
  IF helpkey = "attack_appearance" THEN
   attack_editor_build_appearance_menu recbuf(), workmenu(), state
  END IF
  IF helpkey = "attack_sounds" THEN
   attack_editor_build_sounds_menu recbuf(), workmenu(), state
  END IF
  IF helpkey = "attack_damage" THEN
   attack_editor_build_damage_menu recbuf(), menu(), menutype(), caption(), menucapoff(), workmenu(), state, dmgbit(), maskeddmgbit(), damagepreview
  END IF
  '--in case new attacks/enemies/items have been added
  max(AtkLimChainTo) = gen(genMaxAttack) + 1
  max(AtkLimTransmogEnemy) = gen(genMaxEnemy) + 1
  max(AtkLimSpawnEnemy) = gen(genMaxEnemy) + 1
  max(AtkLimItem) = gen(genMaxItem) + 1
  max(AtkLimSfx) = gen(genMaxSFX) + 1
  max(AtkLimSfxOrDefault) = gen(genMaxSFX) + 1
  '--in case chain mode has changed
  update_attack_editor_for_chain recbuf(AtkDatChainMode),        menu(AtkChainVal1),        max(AtkLimChainVal1),        min(AtkLimChainVal1),        menutype(AtkChainVal1),        menu(AtkChainVal2),        max(AtkLimChainVal2),        min(AtkLimChainVal2),        menutype(AtkChainVal2),        recbuf(AtkDatChainRate),        recbuf(AtkDatChainVal1)
  update_attack_editor_for_chain recbuf(AtkDatElseChainMode),    menu(AtkElseChainVal1),    max(AtkLimElseChainVal1),    min(AtkLimElseChainVal1),    menutype(AtkElseChainVal1),    menu(AtkElseChainVal2),    max(AtkLimElseChainVal2),    min(AtkLimElseChainVal2),    menutype(AtkElseChainVal2),    recbuf(AtkDatElseChainRate),    recbuf(AtkDatElseChainVal1)
  update_attack_editor_for_chain recbuf(AtkDatInsteadChainMode), menu(AtkInsteadChainVal1), max(AtkLimInsteadChainVal1), min(AtkLimInsteadChainVal1), menutype(AtkInsteadChainVal1), menu(AtkInsteadChainVal2), max(AtkLimInsteadChainVal2), min(AtkLimInsteadChainVal2), menutype(AtkInsteadChainVal2), recbuf(AtkDatInsteadChainRate), recbuf(AtkDatInsteadChainVal1)
  '--re-enforce bounds, as they might have just changed
  enforceflexbounds menuoff(), menutype(), menulimits(), recbuf(), min(), max()
  '--fix caption attack caption duration
  caption(menucapoff(AtkCapTime) - 1) = "ticks (" & seconds_estimate(recbuf(AtkDatCapTime)) & " sec)"
  updateflexmenu state.pt, dispmenu(), workmenu(), state.last, menu(), menutype(), menuoff(), menulimits(), recbuf(), caption(), max(), recindex, menucapoff()
  flexmenu_update_selectable workmenu(), menutype(), selectable()
  '--update the picture and palette preview
  ChangeSpriteSlice preview, sprTypeAttack, recbuf(AtkDatPic), recbuf(AtkDatPal)
  DrawSlice preview_box, dpage   'FIXME: pre-call DrawSlice to work around CoverChildren lag
  '--update the weapon picture and palette preview
  IF recbuf(AtkDatWepPic) = 0 THEN
   weppreview->visible = NO
  ELSE
   weppreview->visible = YES
   ChangeSpriteSlice weppreview, sprTypeWeapon, recbuf(AtkDatWepPic) - 1, recbuf(AtkDatWepPal)
  END IF
  '--done updating
  state.need_update = NO
 END IF

 clearpage dpage
 IF drawpreview THEN
  atk_edit_preview recbuf(AtkDatAnimPattern), preview
  DrawSlice preview_box, dpage
 END IF

 'Damage preview, blank on most menus.
 'It really can get 13 lines long! *shudder*
 wrapprint damagepreview, pMenuX, 81, uilook(eduiNote), dpage, , , fontPlain

 'Cost preview
 IF helpkey = "attack_cost" THEN
  DIM tmp_atk as AttackData
  tmp_atk.mp_cost = recbuf(AtkDatMpCost)
  tmp_atk.hp_cost = recbuf(AtkDatHpCost)
  tmp_atk.money_cost = recbuf(AtkDatMoneyCost)
  FOR i as integer = 0 TO 2
   tmp_atk.item(i).id = recbuf(AtkDatItem + i * 2)
   tmp_atk.item(i).number = recbuf(AtkDatItemCost + i * 2)
  NEXT i
  DIM cost_caption as string = attack_cost_info(tmp_atk, 0, 99, 99)
  ' This preview indicates that only the right-most 30 characters fit on the screen;
  ' the rest are shown dark.
  edgeprint cost_caption, pRight, pBottom, uilook(uiDisabledItem), dpage
  edgeprint RIGHT(cost_caption, 30), pRight, pBottom, uilook(uiText), dpage
 END IF

 DIM nowindex as integer = workmenu(state.pt)
 edgeprint flexmenu_tooltip(menutype(nowindex), recbuf(menuoff(nowindex))), pInfoX, pInfoY, uilook(uiDisabledItem), dpage

 standardmenu dispmenu(), state, , , dpage, menuopts
 IF keyval(scAlt) > 0 OR show_name_ticks > 0 THEN 'holding ALT or just tab-flipped, show ID and name
   show_name_ticks = large(0, show_name_ticks - 1)
   tmpstr = readattackname(recbuf()) & " " & recindex
   textcolor uilook(uiText), uilook(uiHighlight)
   printstr tmpstr, pRight, 0, dpage
 END IF

 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP

'--save what we were last working on
saveattackdata recbuf(), recindex

resetsfx
DeleteSlice @preview_box

remember_recindex = recindex
RETURN recindex

END FUNCTION

'Returns YES if a new record was added, or NO if cancelled.
'When YES, gen(genMaxAttack) gets updated, and recbuf() will be populated with
'blank or cloned record, and unsaved! Previous contents are discarded.
'TODO: convert to generic_add_new
FUNCTION atk_edit_add_new (recbuf() as integer, preview_box as Slice Ptr) as bool
  DIM attack as AttackData
  DIM menu(2) as string
  DIM attacktocopy as integer = 0
  DIM preview as Slice ptr = preview_box->FirstChild
  DIM state as MenuState
  state.last = UBOUND(menu)
  state.autosize = YES
  state.pt = 1
  state.size = 3

  state.need_update = YES
  setkeys
  DO
    setwait 55
    setkeys
    IF keyval(ccCancel) > 1 THEN setkeys : RETURN NO  'cancel
    IF keyval(scF1) > 1 THEN show_help "attack_new"
    usemenu state
    IF state.pt = 2 THEN
      IF intgrabber(attacktocopy, 0, gen(genMaxAttack)) THEN state.need_update = YES
    END IF
    IF state.need_update THEN
      state.need_update = NO
      loadattackdata recbuf(), attacktocopy
      convertattackdata recbuf(), attack
      ChangeSpriteSlice preview, sprTypeAttack, recbuf(AtkDatPic), recbuf(AtkDatPal)
      DrawSlice preview_box, dpage   'FIXME: pre-call DrawSlice to work around CoverChildren lag
      menu(0) = "Cancel"
      menu(1) = "New Blank Attack"
      menu(2) = "Copy of Attack " & attacktocopy
    END IF
    IF keyval(scF6) > 1 THEN slice_editor preview_box, SL_COLLECT_EDITOR
    IF enter_space_click(state) THEN
      setkeys
      SELECT CASE state.pt
        CASE 0 ' cancel
          RETURN NO
        CASE 1 ' blank
          gen(genMaxAttack) += 1
          initattackdata recbuf()
          RETURN YES
        CASE 2 ' copy
          gen(genMaxAttack) += 1
          RETURN YES
      END SELECT
    END IF

    clearpage vpage
    standardmenu menu(), state, 20, 20, vpage
    IF state.pt = 2 THEN
      textcolor uilook(uiMenuItem), 0
      printstr " Name: " & attack.name, 20, 48, vpage
      printstr RIGHT(" Description: " & attack.description, 40), 20, 56, vpage
      atk_edit_preview recbuf(AtkDatAnimPattern), preview
      DrawSlice preview_box, vpage
    END IF
    setvispage vpage
    dowait
  LOOP
END FUNCTION

SUB atk_edit_merge_bitsets(recbuf() as integer, tempbuf() as integer)
  'merge the two blocks of bitsets into the buffer
  DIM i as integer
  FOR i = 0 TO 3
    tempbuf(i) = recbuf(AtkDatBitsets + i)
  NEXT i
  FOR i = 0 TO 7
    tempbuf(4 + i) = recbuf(AtkDatBitsets2 + i)
  NEXT i
END SUB

SUB atk_edit_split_bitsets(recbuf() as integer, tempbuf() as integer)
  'split the buffer to the two bitset blocks
  DIM i as integer
  FOR i = 0 TO 3
    recbuf(AtkDatBitsets + i) = tempbuf(i)
  NEXT i
  FOR i = 0 TO 7
    recbuf(AtkDatBitsets2 + i) = tempbuf(4 + i)
  NEXT i
END SUB

SUB atk_edit_load_attack(recbuf() as integer, attackid as integer, caption() as string, AtkCapFailConds as integer)
 loadattackdata recbuf(), attackid
 update_attack_editor_for_fail_conds recbuf(), caption(), AtkCapFailConds
END SUB

'There's another overload of this in common.rbas
FUNCTION readattackname(recbuf() as integer) as string
 RETURN readbadbinstring(recbuf(), AtkDatName, 10, 1)
END FUNCTION

'Regenerate captions for elemental failure conditions
SUB update_attack_editor_for_fail_conds(recbuf() as integer, caption() as string, byval AtkCapFailConds as integer)
 DIM cond as AttackElementCondition
 FOR i as integer = 0 TO 63 'maxElements - 1
  DeSerAttackElementCond cond, recbuf(), 121 + i * 3
  caption(AtkCapFailConds + i * 2 + 1) = format_percent_cond(cond, " [No Condition]")
 NEXT
END SUB

SUB attack_editor_build_appearance_menu(recbuf() as integer, workmenu() as integer, state as MenuState)
  FOR i as integer = 2 TO UBOUND(workmenu)
   workmenu(i) = AtkBlankMenuItem
  NEXT
  workmenu(0) = AtkBackAct
  workmenu(1) = AtkPic
  workmenu(2) = AtkPal
  workmenu(3) = AtkAnimAttack
  workmenu(4) = AtkAnimPattern
  workmenu(5) = AtkAnimAttacker
  workmenu(6) = AtkAlignToTarget
  workmenu(7) = AtkDelay
  workmenu(8) = AtkTurnDelay
  workmenu(9) = AtkDramaticPause
  workmenu(10) = AtkCaption
  workmenu(11) = AtkCapTime
  workmenu(12) = AtkCaptDelay
  workmenu(13) = AtkDamageColor
  'Be careful when adding new menu items here. See that more are sometimes apended below
  state.last = 13

  DIM anim as integer = recbuf(AtkDatAnimAttacker)
  IF     anim = atkrAnimStrike _
  ORELSE anim = atkrAnimDashIn _
  ORELSE anim = atkrAnimSpinStrike _
  ORELSE anim = atkrAnimTeleport _
  ORELSE anim = atkrAnimStandingStrike _
  THEN
   workmenu(15) = AtkWepPic
   state.last = 15
   IF recbuf(AtkDatWepPic) > 0 THEN
    workmenu(16) = AtkWepPal
    workmenu(17) = AtkWepHand0
    workmenu(18) = AtkWepHand1
    state.last = 18
   END IF
  END IF
   
  state.top = 0
  state.need_update = YES
END SUB

SUB attack_editor_build_sounds_menu(recbuf() as integer, workmenu() as integer, state as MenuState)
  FOR i as integer = 2 TO UBOUND(workmenu)
   workmenu(i) = AtkBlankMenuItem
  NEXT
  workmenu(0) = AtkBackAct
  workmenu(1) = AtkSoundEffect
  'workmenu(2) = AtkMissSoundEffect
  'workmenu(3) = AtkFailSoundEffect
  'workmenu(4) = AtkStealFailSoundEffect
  workmenu(2) = AtkLearnSoundEffect
  state.last = 2
  state.top = 0
  state.need_update = YES
END SUB

'Wherein we show how to avoid the limitations of flexmenu by avoiding its use
SUB attack_editor_build_damage_menu(recbuf() as integer, menu() as string, menutype() as integer, caption() as string, menucapoff() as integer, workmenu() as integer, state as MenuState, dmgbit() as string, maskeddmgbit() as string, preview as string)
  DIM i as integer
  DIM attack as AttackData
  convertattackdata(recbuf(), attack)
  DIM targetstat as string = caption(menucapoff(AtkTargStat) + attack.targ_stat)
  DIM iselemental as bool = NO
  DIM target_is_register as bool = NO
  DIM percentage_attack as bool = NO

  IF attack.targ_stat > statLast AND attack.targ_stat <= statLastRegister THEN target_is_register = YES
  IF attack.damage_math = 5 OR attack.damage_math = 6 THEN percentage_attack = YES

  FOR i = 0 TO gen(genNumElements) - 1
    IF attack.elemental_damage(i) THEN iselemental = YES
  NEXT

  ' Blank the menu
  FOR i as integer = 2 TO UBOUND(workmenu)
   workmenu(i) = AtkBlankMenuItem
  NEXT
  state.top = 0
  state.last = 23
  state.need_update = YES
  preview = ""

  ' By default, all bitsets shown. We'll blank ones to hide later
  FOR i = 0 TO UBOUND(dmgbit)
    maskeddmgbit(i) = dmgbit(i)
  NEXT


  ' Start building
  workmenu(0) = AtkBackAct   'Previous
  workmenu(1) = AtkHitX      'Number of hits
  workmenu(2) = AtkDamageEq  'Damage equation
  DIM nextslot as integer = 3

  ' "Attacks ignore extra hits stat" gen bitset
  IF prefbit(29) THEN
    maskeddmgbit(49) = ""  'Ignore attacker's extra hits
  END IF

  IF prefbit(29) = NO AND attack.ignore_extra_hits = NO THEN
    preview += "Hits " & attack.hits & " to " & attack.hits & " + attacker " + statnames(statHitX) + !" times\n"
  ELSE
    IF attack.hits = 1 THEN
      preview += !"Hits 1 time\n"
    ELSE
      preview += "Hits " & attack.hits & !" times\n"
    END IF
  END IF

  IF target_is_register THEN
    'Register stats are always capped to max
    maskeddmgbit(58) = ""  'Allow Cure to exceed maximum
  END IF

  IF attack.targ_stat <> statRegen AND attack.targ_stat <> statPoison THEN
    maskeddmgbit(88) = ""  'Healing poison causes regen, and vice versa
  END IF

  'If Damage Math is No Damage
  '(Note that this also disables nearly all aiming and failure logic!)
  IF attack.damage_math = 4 THEN
    'Nada... and only one of the bitsets apply

    workmenu(nextslot) = AtkDamageBitAct   'Damage bitsets menu
    nextslot += 1

    FOR i = 0 TO UBOUND(maskeddmgbit)
      IF i <> 49 THEN maskeddmgbit(i) = ""  'Ignore attacker's extra hits
    NEXT

    'Also "Do not display damage" has no effect, but that's not in this menu

  ELSE
    'Doing damage

    DIM setvalue as bool = NO  'Setting target stat directly to a value (percentage of something)
    DIM elemental_modifiers as bool = iselemental  'absorbable due to elements?

    'A normal attack (not percentage-based)
    IF attack.damage_math <= 3 THEN
      workmenu(nextslot) = AtkBaseAtk      'Base attack value/stat
      nextslot += 1
      IF attack.damage_math <> 3 THEN
        'Not pure damage
        workmenu(nextslot) = AtkBaseDef    'Base defense stat
        nextslot += 1
      END IF

      menu(AtkExtraDamage) = "Extra Damage:"
      menutype(AtkExtraDamage) = 17  'value%

      preview += "${LM48}DMG = "

      DIM as string amult, dmult, astat, dstat

      astat = caption(menucapoff(AtkBaseAtk) + attack.base_atk_stat)
      dstat = caption(menucapoff(AtkBaseDef) + attack.base_def_stat)
      IF attack.base_def_stat = 0 THEN  'Default
        IF attack.base_atk_stat = 1 THEN  'Magic attack
          dstat = statnames(statWill)
        ELSE
          dstat = statnames(statDef)
        END IF
      END IF

      IF attack.damage_math = 0 THEN amult = "" : dmult = "0.5 * "  'Normal
      IF attack.damage_math = 1 THEN amult = "0.8 * " : dmult = "0.1 * "  'Blunt
      IF attack.damage_math = 2 THEN amult = "1.3 * " : dmult = ""  'Sharp

      DIM show_extra_damage as integer = (attack.extra_damage <> 0)

      IF attack.damage_math = 3 THEN  'Pure damage
        'Some special case simplifications
        IF attack.base_atk_stat = 4 THEN
          '0 to 999
          preview += "Random(0 to " & INT(9.99 * (100 + attack.extra_damage)) & ")"
          show_extra_damage = NO
        ELSEIF attack.base_atk_stat = 5 THEN
          '100
          preview += STR(100 + attack.extra_damage)
          show_extra_damage = NO
        ELSE
          preview += astat
        END IF
      ELSE
        preview += "(" + amult + astat + " - " + dmult + dstat + ")"
      END IF

      IF show_extra_damage THEN
        preview += " * " & (attack.extra_damage + 100) & "%"
      END IF
      IF iselemental THEN
        preview += " * Elemental Bonuses"
      END IF

      IF attack.do_not_randomize = NO THEN
        preview += !"\nDMG = DMG +/- 20%"
      END IF

      'If the attack is actually spreadable (Spread or Optional Spread)
      IF attack.targ_set = 1 OR attack.targ_set = 2 THEN
        IF attack.divide_spread_damage THEN preview += " / Num Targets"
      ELSE
        maskeddmgbit(1) = ""  'Divide spread damage
      END IF

      '--mask bitsets which have no effect
      maskeddmgbit(69) = "" '% based attacks damage instead of set


    ELSEIF attack.damage_math = 5 OR attack.damage_math = 6 THEN
      '%-based attacks. Two big alternative damage formulae!

      elemental_modifiers = NO

      DIM as string tempcap

      preview += "${LM48}"

      IF attack.percent_damage_not_set = NO THEN
        setvalue = YES

        '--percentage damage shows target stat
        tempcap = caption(menucapoff(AtkTargStat) + recbuf(AtkDatTargStat)) + " = " & (100 + attack.extra_damage) & "%"
        caption(menucapoff(AtkDamageEq) + 5) = tempcap + " of Maximum"
        caption(menucapoff(AtkDamageEq) + 6) = tempcap + " of Current"

        IF attack.show_damage_without_inflicting THEN
          'Ugh, special case it to show damage instead
          'preview += "DMG = "  + caption(menucapoff(AtkDamageEq) + attack.damage_math) + " - Current " + targetstat
          IF attack.damage_math = 5 THEN tempcap = "maximum " + targetstat
          IF attack.damage_math = 6 THEN tempcap = "current " + targetstat
          preview += "DMG = current " + targetstat + " - " & (100 + attack.extra_damage) & "% of " + tempcap
        ELSE
          preview += "Target " + caption(menucapoff(AtkDamageEq) + attack.damage_math)
        END IF

        '--mask bitsets which have no effect
        maskeddmgbit(0) = ""  'Cure instead of harm
        maskeddmgbit(83) = "" 'Don't allow damage to exceed target stat
        'Enemy's "Harmed by cure" bitset also does nothing

      ELSE
        tempcap = (100 + attack.extra_damage) & "%"
        caption(menucapoff(AtkDamageEq) + 5) = tempcap + " of Maximum"
        caption(menucapoff(AtkDamageEq) + 6) = tempcap + " of Current"

        preview += "DMG = " + caption(menucapoff(AtkDamageEq) + attack.damage_math) + " " + targetstat

      END IF

      menu(AtkExtraDamage) = "Percentage:"
      menutype(AtkExtraDamage) = 22  '(100 + value)%

      '--mask bitsets which have no effect
      maskeddmgbit(1) = ""   'Divide spread damage
      maskeddmgbit(61) = ""  'Do not randomize
      maskeddmgbit(62) = ""  'Damage can be zero
      
    ELSE
      showbug "Impossible damage math setting"
    END IF

    'Add rest of menu items
    IF attack.show_damage_without_inflicting = NO OR percentage_attack THEN
      'If not inflicting, only need to select target stat if it affects damage
      workmenu(nextslot) = AtkTargStat     'Target stat
      nextslot += 1
    END IF
    workmenu(nextslot) = AtkExtraDamage  'Extra damage %
    nextslot += 1
    workmenu(nextslot) = AtkDamageBitAct 'Damage bitsets menu
    nextslot += 1

    'If this bit is set, then damage caps and "Allow cure to exceed maximum" and absorbing
    'don't take effect, but "Do not exceed target stat" and min 1 damage still do
    IF attack.show_damage_without_inflicting = YES THEN
      maskeddmgbit(2)  = ""  'Absorb Damage
      maskeddmgbit(57) = ""  'Reset target stat to max before hit
      maskeddmgbit(58) = ""  'Allow Cure to exceed maximum
      maskeddmgbit(88) = ""  'Healing poison causes regen, and vice versa
    END IF

    IF attack.show_damage_without_inflicting = NO AND setvalue = NO AND gen(genDamageCap) > 0 THEN
      'Both damage caps takes effect
      IF attack.damage_can_be_zero THEN
        preview += !"\nDMG = limit(DMG, 0 to " & gen(genDamageCap) & ")"
      ELSE
        preview += !"\nDMG = limit(DMG, 1 to " & gen(genDamageCap) & ")"
      END IF
    ELSEIF percentage_attack = NO THEN
      'Cap damage below
      IF attack.damage_can_be_zero = NO THEN
        preview += !"\nIf DMG <= 0 then DMG = 1"
      ELSE
        preview += !"\nIf DMG < 0 then DMG = 0"
      END IF
    END IF

    DIM might_otherwise_exceed_max as bool = NO  'Could "allow cure to exceed max" be needed for THE TARGET

    'Check whether (and say when) this attack might cure the target
    IF elemental_modifiers THEN
      '(setvalue is NO)
      IF attack.cure_instead_of_harm THEN
        preview += !"\nNegate DMG if target absorbs element or not `Harmed by cure'"
      ELSE
        preview += !"\nNegate DMG if target absorbs element"
      END IF
      might_otherwise_exceed_max = YES
    ELSEIF attack.cure_instead_of_harm AND setvalue = NO THEN
      'AKA damage-not-set percentage-based attack
      preview += !"\nNegate DMG if target not `Harmed by cure'"
      might_otherwise_exceed_max = YES
    ELSEIF setvalue = YES AND attack.extra_damage > 0 THEN
      might_otherwise_exceed_max = YES
    END IF

    IF setvalue = NO AND attack.do_not_exceed_targ_stat THEN
      preview += !"\nDMG = limit(DMG, -target lost " + targetstat + " to target " + targetstat + ")"

      'In this case, we can never cure the target, but we ONLY hide "Allow Cure to exceed maximum"
      'if we are not absorbing, because it still affects the attacker's target stat!
      IF attack.absorb_damage = NO THEN
        maskeddmgbit(58) = ""  'Allow Cure to exceed maximum
      END IF
      might_otherwise_exceed_max = NO
    END IF

    IF attack.show_damage_without_inflicting = NO THEN
      IF setvalue THEN
        'Special case, "Target stat = ..." line already added
        IF attack.absorb_damage THEN
          preview += !"\nAttacker " + targetstat + " -= change to target's " + targetstat
        END IF
      ELSE

        IF attack.reset_targ_stat_before_hit THEN
          preview += !"\nTarget " + targetstat + " = Max " + targetstat + " - DMG"
        ELSE
          preview += !"\nTarget " + targetstat + " -= DMG"
        END IF
        IF attack.absorb_damage THEN
          preview += !"\nAttacker " + targetstat + " += DMG"
        END IF
      END IF

      IF attack.poison_is_negative_regen AND (attack.targ_stat = statPoison OR attack.targ_stat = statRegen) THEN
        'Healing poison causes regen, and vice versa
        DIM negatedstat as integer = IIF(attack.targ_stat = statPoison, statRegen, statPoison)
        DIM negatedstatname as string = caption(menucapoff(AtkTargStat) + negatedstat)
        preview += !"\nIf Target/Attacker " + targetstat + " > Max"
        preview += !"\nthen Target/Attacker " & negatedstatname & " -= amount above Max"
      END IF

      IF attack.allow_cure_to_exceed_maximum = NO AND target_is_register = NO THEN
        'Might the target stat be capped?
        'Don't bother stating this for registers, as they are always capped (and the preview gets way too long)
        IF might_otherwise_exceed_max THEN
          'preview += !"\nIf Target " + targetstat + " > Maximum then " + targetstat + " = Maximum"
          preview += !"\nLimit Target " + targetstat + " to <= Max"
        END IF
        IF attack.absorb_damage THEN
          'preview += !"\nIf Attacker " + targetstat + " > Maximum then " + targetstat + " = Maximum"
          preview += !"\nLimit Attacker " + targetstat + " to <= Max"
        END IF
      END IF
    END IF
  END IF

  state.pt = small(state.pt, nextslot - 1)

END SUB

SUB atk_edit_preview(byval pattern as integer, sl as Slice Ptr)
 STATIC anim0 as integer
 STATIC anim1 as integer
 anim0 = anim0 + 1
 IF anim0 > 3 THEN
  anim0 = 0
  IF pattern = 0 THEN anim1 = anim1 + 1: IF anim1 > 2 THEN anim1 = 0
  IF pattern = 1 THEN anim1 = anim1 - 1: IF anim1 < 0 THEN anim1 = 2
  IF pattern = 2 THEN anim1 = anim1 + 1: IF anim1 > 2 THEN anim1 = -1
  IF pattern = 3 THEN anim1 = randint(3)
 END IF
 ChangeSpriteSlice sl, , , ,ABS(anim1)
END SUB


'--------------------------- Nearly Generic Flexmenu Stuff ---------------------


SUB atk_edit_backptr(workmenu() as integer, mainMenu() as integer, state as MenuState, laststate as menustate, byref menudepth as integer)
 setactivemenu workmenu(), mainMenu(), state
 menudepth = 0
 state.pt = laststate.pt
 state.top = laststate.top
 state.need_update = YES
END SUB

SUB atk_edit_pushptr(state as MenuState, laststate as MenuState, byref menudepth as integer)
 laststate.pt = state.pt
 laststate.top = state.top
 menudepth = 1
END SUB

SUB flexmenu_update_selectable(workmenu() as integer, menutype() as integer, selectable() as bool)
 REDIM selectable(UBOUND(workmenu))
 FOR i as integer = 0 TO UBOUND(workmenu)
  selectable(i) = menutype(workmenu(i)) <> 18  'skippable
 NEXT
END SUB

'Handles attempt to enter the attack or enemy browser or editor by hitting Enter/etc/+/Insert/etc on a menu item.
'Returns true if need to update state.
FUNCTION flexmenu_handle_crossrefs (state as MenuState, nowindex as integer, menutype() as integer, menuoff() as integer, recindex as integer, recbuf() as integer, is_attack_editor as bool) as bool

 'Early out tests
 IF enter_or_add_new(state) = NO THEN RETURN NO
 SELECT CASE menutype(nowindex)
  CASE 7, 9 'attack, enemy
  CASE ELSE
   RETURN NO
 END SELECT

 IF is_attack_editor THEN
  saveattackdata recbuf(), recindex
 ELSE
  saveenemydata recbuf(), recindex
 END IF

 DIM ret as bool
 DIM byref dat as integer = recbuf(menuoff(nowindex))
 'The reason for using intgrab=NO below is so that we can filter out when Alt is held down.
 'But it results in needing to press Delete twice to delete an enemy/attack
 SELECT CASE menutype(nowindex)
  CASE 7 'dat is attack + 1
   ret = attackgrabber(dat, state, 1, , NO)  'intgrab=NO
  CASE 9 'dat is enemy + 1
   ret = enemygrabber(dat, state, 1, , NO)  'intgrab=NO
 END SELECT

 ' If we entered the attack/enemy editor recursively recbuf() may now stale, so
 ' reload it and re-write dat.
 ' When we return YES recbuf() will be reloaded again, so we need to save our changes too!
 IF ret THEN
  DIM newdat as integer = dat
  IF is_attack_editor THEN
   loadattackdata recbuf(), recindex
   dat = newdat
   saveattackdata recbuf(), recindex
  ELSE
   loadenemydata recbuf(), recindex
   dat = newdat
   saveenemydata recbuf(), recindex
  END IF
 END IF

 RETURN ret
END FUNCTION

'Helper for menutypes 6000-6999 and 7000-7999 which works out readbit/setbit arguments
SUB flexmenu_bitset_word_and_bit(menutype_item as integer, menuoff_item as integer, byref wordnum as integer, byref bitnum as integer)
 IF menutype_item >= 7000 THEN
  bitnum = menutype_item - 7000
  IF bitnum < 16*4 THEN
   wordnum = AtkDatBitsets
  ELSE
   wordnum = AtkDatBitsets2
   bitnum -= 16*4
  END IF
 ELSE
  wordnum = menuoff_item
  bitnum = menutype_item - 6000
 END If
END SUB

FUNCTION editflexmenu (state as MenuState, nowindex as integer, menutype() as integer, menuoff() as integer, menulimits() as integer, datablock() as integer, caption() as string, mintable() as integer, maxtable() as integer) as bool
'--Calls intgrabber/strgrabber etc, as appropriate for the selected data field.
'--returns true if data has changed, false it not

'nowindex is the index into the menu data of the currently selected menuitem
'menutype() holds the type of each menu element.
'           0=int
'           1=action (usually triggering a different menu)
'           2=tag condition, including special tags
'           3=string(bybyte)
'           4=badly stored string(by word)
'           5=chooser (not connected with data)
'           6=extra badly stored string(by word with gap)
'           7=attack number (offset!)
'           8=item number (not offset)
'           9=enemy name (offset)
'           10=item number (offset!)
'           11=sound effect or none (offset)
'           12=defaultable positive int >=0 is int, -1 is "default"
'           13=Default zero int >0 is int, 0 is "default"  (see also type 26)
'           14=sound effect + 1 (0=default, -1=none)
'           15=UNUSED
'           16=stat (numbered the same way as BattleStatsSingle.sta())
'           17=int with a % sign after it
'           18=skipper (caption which is skipped by the cursor)
'           19=ticks (with seconds estimate)
'           20=Else-Chain Rate hack (clumsy hack to force myself to do this elegantly in editedit --James)
'           21=set tag, excluding special tags
'           22=(int+100) with a % sign after it
'           23=color, or 0 for default
'           24=turn-based attack delay
'           25=counterattack provoke setting (captioned, with default for 0)
'           26=Defaultable non-negative int: >0 is int offset by 1, 0 is "default"
'           27=sound effect or default or none (offset)
'           1000-1999=postcaptioned int (caption-start-offset=n-1000)
'                     (be careful about negatives!)
'           2000-2999=caption-only int (caption-start-offset=n-1000)
'                     (be careful about negatives!)
'           3000-3999=multi-state (uses caption index -1 too!)
'           4000-4999=percent_cond_grabber, where caption(n-4000) holds
'                     the default string (no condition), and caption(n-4000+1) is
'                     the repr string needed by percent_cond_grabber.
'                     Limits not yet supported.
'                     (The condition is stored in 3 consecutive INTs.)
'           5000-5999=percent_grabber (single floats), where caption(n-4000) holds
'                     the repr string needed by percent_grabber.
'                     Limits not yet supported.
'                     (The single is stored in 2 consecutive INTs.)
'           6000-6999=bitset, either NO/YES or captioned
'                     The bit number, in the bitarray starting at offset in menuoff is type - 6000
'                     If menucapoff() is non-zero it's used instead of NO/YES
'           7000-7999=attack bitset, either NO/YES or captioned
'                     The attack bit number is type - 7000, menuoff() ignored.
'                     If the item title starts with '!', the bit is inverted.
'                     If menucapoff() is non-zero it's used instead of NO/YES
'           8000-8999=value of stat 8000+statnum
'menuoff() is the offsets into the data block where each menu data is stored
'menulimits() is the offsets into the mintable() and maxtable() arrays
'datablock() holds the actual data
'mintable() is minimum integer values
'maxtable() is maximum int values and string limits

DIM changed as bool = NO
DIM s as string

SELECT CASE menutype(nowindex)
 CASE 0, 8, 12 TO 17, 19, 20, 23, 24, 25, 3000 TO 3999, 8000 TO 8999' integers
  changed = intgrabber(datablock(menuoff(nowindex)), mintable(menulimits(nowindex)), maxtable(menulimits(nowindex)))
 CASE 1000 TO 2999' captioned integers
  changed = intgrabber(datablock(menuoff(nowindex)), mintable(menulimits(nowindex)), maxtable(menulimits(nowindex)))
  IF enter_space_click(state) THEN
   DIM old_dat as integer = datablock(menuoff(nowindex))
   DIM flexb as FlexmenuCaptionBrowser
   flexb.set_list_from_flexmenu caption(), menutype(nowindex), mintable(menulimits(nowindex)), maxtable(menulimits(nowindex))
   datablock(menuoff(nowindex)) = flexb.browse(datablock(menuoff(nowindex)))
   changed = (old_dat <> datablock(menuoff(nowindex)))
  END IF
 CASE 7, 9 TO 11, 26, 27 'offset integers
  changed = zintgrabber(datablock(menuoff(nowindex)), mintable(menulimits(nowindex)) - 1, maxtable(menulimits(nowindex)) - 1)
 CASE 22 '(int+100)%
  DIM temp as integer = datablock(menuoff(nowindex)) + 100
  changed = intgrabber(temp, mintable(menulimits(nowindex)) + 100, maxtable(menulimits(nowindex)) + 100)
  datablock(menuoff(nowindex)) = temp - 100
 CASE 2' tag condition
  changed = tag_grabber(datablock(menuoff(nowindex)), state)
 CASE 21' set tag (non-special)
  changed = tag_set_grabber(datablock(menuoff(nowindex)), state)
 CASE 3' string
  s = readbinstring(datablock(), menuoff(nowindex), maxtable(menulimits(nowindex)))
  IF strgrabber(s, maxtable(menulimits(nowindex))) THEN changed = YES
  writebinstring s, datablock(), menuoff(nowindex), maxtable(menulimits(nowindex))
 CASE 4' badly stored string
  s = readbadbinstring(datablock(), menuoff(nowindex), maxtable(menulimits(nowindex)), 0)
  IF strgrabber(s, maxtable(menulimits(nowindex))) THEN changed = YES
  writebadbinstring s, datablock(), menuoff(nowindex), maxtable(menulimits(nowindex)), 0
 CASE 6' extra badly stored string
  s = readbadbinstring(datablock(), menuoff(nowindex), maxtable(menulimits(nowindex)), 1)
  IF strgrabber(s, maxtable(menulimits(nowindex))) THEN changed = YES
  writebadbinstring s, datablock(), menuoff(nowindex), maxtable(menulimits(nowindex)), 1
 CASE 4000 TO 4999' elemental condition
  DIM cond as AttackElementCondition
  DIM capnum as integer = menutype(nowindex) - 4000
  DeSerAttackElementCond cond, datablock(), menuoff(nowindex)
  'modifies caption(capnum + 1)
  changed = percent_cond_grabber(cond, caption(capnum + 1), caption(capnum), -1000.0, 1000.0)
  'debug "cond_grab: ch=" & changed & " type = " & cond.type & " val = " & cond.value &  " off = " & menuoff(nowindex) & " cap = " & caption(capnum + 1)
  SerAttackElementCond cond, datablock(), menuoff(nowindex)
 CASE 5000 TO 5999' single, as percent
  DIM value as single
  DIM capnum as integer = menutype(nowindex) - 5000
  value = DeSerSingle(datablock(), menuoff(nowindex))
  'modifies caption(capnum)
  changed = percent_grabber(value, caption(capnum), -1000.0, 1000.0)
  SerSingle(datablock(), menuoff(nowindex), value)
 CASE 6000 TO 6999, 7000 TO 7999' bitset or attack bitset
  DIM as integer wordnum, bitnum
  flexmenu_bitset_word_and_bit menutype(nowindex), menuoff(nowindex), wordnum, bitnum
  changed = bitsetgrabber(datablock(), wordnum, bitnum, state)
END SELECT

'--browse & preview sound effects
IF (menutype(nowindex) = 11 ORELSE menutype(nowindex) = 27) THEN
 IF keyval(scP) > 1 THEN 
  IF datablock(menuoff(nowindex)) > 0 THEN
   playsfx datablock(menuoff(nowindex)) - 1
  END IF
 END IF
 IF enter_space_click(state) THEN
  DIM should_pick as integer = 1
  IF menutype(nowindex) = 27 THEN
   should_pick = twochoice("Use default sound?", "Use hit sound", "Pick a specific sound", 1, -1)
   IF should_pick = 0 THEN
    datablock(menuoff(nowindex)) = 0
    changed = YES
   END IF
  END IF
  IF should_pick = 1 THEN
   DIM old_sfx as integer = datablock(menuoff(nowindex))
   DIM sfx as integer = sfx_picker_or_none(old_sfx)
   IF sfx <> old_sfx THEN
    datablock(menuoff(nowindex)) = sfx
    changed = YES
   END IF
  END IF
 END IF
END IF

RETURN changed

END FUNCTION

SUB enforceflexbounds (menuoff() as integer, menutype() as integer, menulimits() as integer, recbuf() as integer, min() as integer, max() as integer)

FOR i as integer = 0 TO UBOUND(menuoff)
 SELECT CASE menutype(i)
  CASE 0, 8, 12 TO 17, 19, 20, 22, 23, 24, 25, 1000 TO 3999
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

SUB setactivemenu (workmenu() as integer, newmenu() as integer, byref state as MenuState)
 DIM i as integer
 FOR i = 0 TO UBOUND(newmenu)
  workmenu(i) = newmenu(i)
 NEXT i
 state.pt = 0
 state.top = 0
 state.last = UBOUND(newmenu)
 state.need_update = YES
END SUB

SUB updateflexmenu (mpointer as integer, nowmenu() as string, nowdat() as integer, size as integer, menu() as string, menutype() as integer, menuoff() as integer, menulimits() as integer, datablock() as integer, caption() as string, maxtable() as integer, recindex as integer, menucapoff() as integer)

'--generates a nowmenu subset from generic menu data

'nowmenu() contains the results. a menu ready to use with standardmenu
'nowdat() is a list of the indexes of which menu elements are currently on display
'size is the index of the last element in nowdat() and nowmenu()
'menu() holds all the available captions. They may contain $$ to indicate where
'       the generated text should be inserted, otherwise it is appended.
'menutype() holds the type of each menu element.
'           0=int
'           1=action (usually triggering a different menu)
'           2=tag condition, including special tags
'           3=string(bybyte)
'           4=badly stored string(by word)
'           5=record chooser (not connected with data)
'           6=extra badly stored string(by word with gap)
'           7=attack number (offset)
'           8=item number (not offset)
'           9=enemy name (offset)
'           10=item name (offset)
'           11=sound effect or none (offset)
'           12=defaultable positive int >=0 is int, -1 is "default"
'           13=Default zero int >0 is int, 0 is "default"  (see also type 26)
'           14=sound effect + 1 (0=default, -1=none)
'           15=UNUSED
'           16=stat (numbered the same way as BattleStatsSingle.sta())
'           17=int with a % sign after it
'           18=skipper (caption which is skipped by the cursor)
'           19=ticks (with seconds estimate)
'           20=Else-Chain Rate hack (clumsy hack to force myself to do this elegantly in editedit --James)
'           21=set tag, excluding special tags
'           22=(int+100) with a % sign after it
'           23=color, or 0 for default
'           24=turn-based attack delay
'           25=counterattack provoke setting (captioned, with default for 0)
'           26=Defaultable non-negative int: >0 is int offset by 1, 0 is "default"
'           27=sound effect or default or none (offset)
'           1000-1999=postcaptioned int (caption-start-offset=n-1000)
'                     (be careful about negatives!)
'           2000-2999=caption-only int (caption-start-offset=n-2000)
'                     (be careful about negatives!)
'           3000-3999=Multi-state (0 and negatives are caption-only,
'                                  positive is postcaptioned. Captions are
'                                  numbered bass-ackwards )
'           4000-4999=percent_cond_grabber, where caption(n-4000) holds
'                     the default string (no condition), and caption(n-4000+1) is
'                     the repr string needed by percent_cond_grabber.
'                     Limits not yet supported.
'                     (The condition is stored in 3 consecutive INTs.)
'           5000-5999=percent_grabber (single floats), where caption(n-4000) holds
'                     the repr string needed by percent_grabber.
'                     Limits not yet supported.
'                     (The single is stored in 2 consecutive INTs.)
'           6000-6999=bitset, either NO/YES or captioned
'                     The bit number, in the bitarray starting at offset in menuoff is type - 6000
'                     If menucapoff() is non-zero it's used instead of NO/YES
'           7000-7999=attack bitset, either NO/YES or captioned
'                     The attack bit number is type - 7000, menuoff() ignored.
'                     If menucapoff() is non-zero it's used instead of NO/YES
'           8000-8999=value of stat 8000+statnum
'menuoff() tells us what index to look for the data for this menu item
'menulimits() is the offset to look in maxtable() for limits
'datablock() the actual data the menu represents
'caption() available captions for postcaptioned ints
'maxtable() used here only for max string lengths

DIM maxl as integer
DIM capnum as integer
DIM dat as integer
DIM i as integer
FOR i = 0 TO size
 DIM nospace as bool = NO
 DIM datatext as string
 dat = datablock(menuoff(nowdat(i)))
 nowmenu(i) = menu(nowdat(i))
 SELECT CASE menutype(nowdat(i))
  CASE 0 '--int
   datatext = STR(dat)
  CASE 2 '--tag condition, including specials
   datatext = tag_condition_caption(dat, "", "NONE")
  CASE 3 '--goodstring
   maxl = maxtable(menulimits(nowdat(i)))
   datatext = readbinstring(datablock(), menuoff(nowdat(i)), maxl)
   nospace = YES
  CASE 4 '--badstring
   maxl = maxtable(menulimits(nowdat(i)))
   datatext = readbadbinstring(datablock(), menuoff(nowdat(i)), maxl, 0)
   nospace = YES
  CASE 5 '--record index
   'Special, $$ text replacement not available
   nowmenu(i) = CHR(27) & nowmenu(i) & " " & recindex & CHR(26)
  CASE 6 '--extrabadstring
   maxl = maxtable(menulimits(nowdat(i)))
   datatext = readbadbinstring(datablock(), menuoff(nowdat(i)), maxl, 1)
   nospace = YES
  CASE 7 '--attack number
   IF dat <= 0 THEN
    datatext = "None"
   ELSE
    datatext = (dat - 1) & " " + readattackname(dat - 1)
   END IF
  CASE 8 '--item number
   datatext = load_item_name(dat, 0, 1)
  CASE 9 '--enemy number
   IF dat <= 0 THEN
    datatext = "None"
   ELSE
    datatext = (dat - 1) & " " + readenemyname(dat - 1)
   END IF
  CASE 10 '--item number, offset
    datatext = load_item_name(dat, 0, 0)
  CASE 11 '--sound effect number, offset
    IF dat <= 0 THEN
      datatext = "None"
    ELSE
      datatext = (dat - 1) & " (" + getsfxname(dat - 1) + ")"
    END IF
  CASE 12 '--defaultable positive int
    datatext = defaultint(dat)
  CASE 13 '--zero default int
    datatext = zero_default(dat)
  CASE 14 '--sound effect number + 1 (0=default, -1=none)
    IF dat = 0 THEN
      datatext = "Default"
    ELSEIF dat < 0 THEN
      datatext = "None"
    ELSE
      datatext = (dat - 1) & " (" + getsfxname(dat - 1) + ")"
    END IF
  CASE 15 '--UNUSED
  CASE 16 '--stat
    datatext = battle_statnames(dat)
  CASE 17 '--int%
   datatext = dat & "%"
  CASE 18 '--skipper
   '--no change to caption
  CASE 19 '--ticks
   datatext = dat & " ticks (" & seconds_estimate(dat) & " sec)"
  CASE 20 '--Else-chain rate (FIXME: it is a terrible hack to hardcode 13 here)
   datatext = dat & "%"
   'AtkDatChainRate = 13
   IF dat > 0 ANDALSO datablock(13) > 0 THEN
    datatext = datatext & strprintf(" (effectively %.2g%%)", (1. - datablock(13) / 100) * dat) 
   END IF
  CASE 21 '--set tag, not including specials
   datatext = tag_set_caption(dat, "")
  CASE 22 '--(int+100)%
   datatext = (dat + 100) & "%"
  CASE 23 '--color 0=default >=1 is palette index (color 0 not available)
   datatext = zero_default(dat)
  CASE 24 '--turn-based attack delay
   datatext = "Happen " & ABS(dat) & " attacks " & IIF(dat < 0, "earlier", "later") & " than normal"
  CASE 25 '--counterattack provoke setting
   capnum = menucapoff(AtkCounterProvoke)
   datatext = caption(capnum + dat)
   IF dat = provokeDefault THEN datatext &= " (" & caption(capnum + gen(genDefCounterProvoke)) & ")"
  CASE 26 '--0=default, >0 is int offset by 1
   IF dat = 0 THEN datatext = "Default" ELSE datatext = STR(dat - 1)
  CASE 27 '--sound effect number, offset
    IF dat <= -1 THEN
      datatext = "None"
    ELSEIF dat = 0 THEN
      datatext = "(Same as Hit sound)"
    ELSE
      datatext = (dat - 1) & " (" + getsfxname(dat - 1) + ")"
    END IF
  CASE 1000 TO 1999 '--captioned int
   capnum = menutype(nowdat(i)) - 1000
   datatext = dat & " " & caption(capnum + dat)
  CASE 2000 TO 2999 '--caption-only int
   capnum = menutype(nowdat(i)) - 2000
   datatext = caption(capnum + dat)
  CASE 3000 TO 3999 '--multistate
   capnum = menutype(nowdat(i)) - 3000
   IF dat > 0 THEN
    datatext = dat & " " & caption(capnum - 1)
   ELSE
    datatext = caption(capnum + ABS(dat))
   END IF
  CASE 4000 TO 4999 '--percent_cond_grabber
   capnum = menutype(nowdat(i)) - 4000
   datatext = caption(capnum + 1)
   nospace = YES
  CASE 5000 TO 5999 '--percent_grabber
   capnum = menutype(nowdat(i)) - 5000
   datatext = caption(capnum)
  CASE 6000 TO 6999, 7000 TO 7999 '--bitset or attack bitset
   DIM as integer wordnum, bitnum, thebit
   flexmenu_bitset_word_and_bit menutype(nowdat(i)), menuoff(nowdat(i)), wordnum, bitnum
   thebit = readbit(datablock(), wordnum, bitnum)
   IF LEFT(nowmenu(i), 1) = "!" THEN  'Invert bit
    nowmenu(i) = MID(nowmenu(i), 2)
    thebit XOR= 1
   END IF
   IF menucapoff(nowdat(i)) THEN
    datatext = caption(menucapoff(nowdat(i) + thebit))
   ELSE
    datatext = yesorno(thebit)
   END IF
  CASE 8000 TO 8999 '--value of stat 8000+statnum
   datatext = STR(dat)
   IF menutype(nowdat(i)) - 8000 = statSpeed THEN '--speed (shows battle turn time estimate)
    IF gen(genBattleMode) = 0 THEN  'active-time
     datatext &= " (" & speed_estimate(dat) & ")"
    END IF
   END IF

 END SELECT
 IF replacestr(nowmenu(i), "$$", datatext) = 0 THEN
  'No replacements made
  IF nospace = NO AND nowmenu(i) <> "" THEN nowmenu(i) += " "
  nowmenu(i) += datatext
 END IF
NEXT i
END SUB

FUNCTION isStringField(byval mnu as integer) as bool
  IF mnu = 3 OR mnu = 4 OR mnu = 6 THEN RETURN YES
  RETURN NO
END FUNCTION

'Message to show at the bottom of the screen. Only for things not specific to enemy or attack editor.
FUNCTION flexmenu_tooltip(menutype as integer, datvalue as integer) as string
 SELECT CASE menutype
  CASE 7, 9  'attack (offset)
   RETURN THINGGRABBER_TOOLTIP
  CASE 8000 TO 8999  'value of stat 8000+statnum
   IF menutype <> 8000 + statSpeed THEN  'Skip Speed, since that's already shown right in the menu item (see updateflexmenu)
    RETURN stat_value_caption(menutype - 8000, datvalue)
   END IF

 END SELECT
END FUNCTION

'------------------------------------------------------------------------------

'Returns which "base attack stat" was selected. base_num is the initial/default value.
FUNCTION browse_base_attack_stat(byval base_num as integer) as integer

 IF base_num = 1 THEN base_num = 12 'redundant attacker magic entry
 IF base_num = 2 THEN base_num = 6 'redundant attacker HP entry

 DIM hstate as MenuState
 hstate.last = 4
 DIM state(4) as MenuState
 DIM menu(4) as MenuDef

 append_menu_item(menu(0), "Default (" & statnames(statAtk) & ")")
 menu(0).last->extra(0) = 0
 append_menu_item(menu(0), "100")
 menu(0).last->extra(0) = 5
 append_menu_item(menu(0), "100 * number of targets")
 menu(0).last->extra(0) = 59
 append_menu_item(menu(0), "Random 0-999")
 menu(0).last->extra(0) = 4
 append_menu_item(menu(0), "Last damage by attacker")
 menu(0).last->extra(0) = 18
 append_menu_item(menu(0), "Last damage to attacker")
 menu(0).last->extra(0) = 19
 append_menu_item(menu(0), "Last damage to target")
 menu(0).last->extra(0) = 20
 append_menu_item(menu(0), "Last cure to attacker")
 menu(0).last->extra(0) = 21
 append_menu_item(menu(0), "Last cure to target")
 menu(0).last->extra(0) = 22
 append_menu_item(menu(0), "Lost HP (attacker)")
 menu(0).last->extra(0) = 3
 append_menu_item(menu(0), "Lost MP (attacker)")
 menu(0).last->extra(0) = 60
 append_menu_item(menu(0), "Lost HP (target)")
 menu(0).last->extra(0) = 62
 append_menu_item(menu(0), "Lost MP (target)")
 menu(0).last->extra(0) = 61

 FOR i as integer = 0 TO 11
  append_menu_item(menu(1), statnames(i) & " (attacker)")
  menu(1).last->extra(0) = 6 + i
  append_menu_item(menu(2), statnames(i) & " (target)")
  menu(2).last->extra(0) = 23 + i
  append_menu_item(menu(3), "Max " & statnames(i) & " (attacker)")
  menu(3).last->extra(0) = 35 + i
  append_menu_item(menu(4), "Max " & statnames(i) & " (target)")
  menu(4).last->extra(0) = 47 + i
 NEXT i
 
 FOR i as integer = 0 TO 4
  FOR j as integer = 0 TO menu(i).numitems - 1
   state(i).active = NO
   IF menu(i).items[j]->extra(0) = base_num THEN
    state(i).active = YES
    state(i).pt = j
    hstate.pt = i
   END IF
  NEXT j
  WITH menu(i)
   .textalign = alignLeft
   .alignhoriz = alignLeft
   .alignvert = alignTop
   .anchorhoriz = alignLeft
   .anchorvert = alignTop
   .offset.y = 5
   .maxrows = 22
   .bordersize = -4
   IF i = 0 THEN
    .offset.x = 4
   ELSE
    .offset.x = menu(i-1).offset.x + menu(i-1).rect.wide + 8
   END IF
  END WITH
  init_menu_state state(i), menu(i)
  ' Draw the menu to calculate its width and position
  draw_menu menu(i), state(i), vpage
 NEXT i

 ' Pre-scroll the menus so that the selected one is fully visible
 DIM shiftx as integer
 WITH menu(hstate.pt).rect
  IF .x < 0 THEN
   shiftx = .x
  ELSE
   shiftx = large(0, .x + .wide - vpages(vpage)->w)
  END IF
 END WITH
 FOR i as integer = 0 TO 4
  menu(i).offset.x -= shiftx
 NEXT i

 DIM oldpt as integer
 DIM result as integer

 setkeys
 DO
  setwait 33
  setkeys

  IF keyval(ccCancel) > 1 THEN
   result = base_num
   EXIT DO
  END IF

  IF keyval(scF1) > 1 THEN show_help "attack_browse_base_stat"

  IF enter_space_click(hstate) THEN
   result = menu(hstate.pt).items[state(hstate.pt).pt]->extra(0)
   EXIT DO
  END IF
  
  oldpt = hstate.pt
  IF usemenu(hstate, ccLeft, ccRight) THEN
   state(hstate.pt).pt = small(state(oldpt).pt, state(hstate.pt).last)
  END IF
  FOR i as integer = 0 TO 4
   state(i).active = (i = hstate.pt)
   usemenu state(i)
  NEXT i

  IF menu(hstate.pt).rect.x < 0 THEN
   FOR i as integer = 0 TO 4
    menu(i).offset.x += 32
   NEXT i
  ELSEIF menu(hstate.pt).rect.x + menu(hstate.pt).rect.wide > vpages(vpage)->w THEN
   FOR i as integer = 0 TO 4
    menu(i).offset.x -= 32
   NEXT i
  END IF

  clearpage vpage
  FOR i as integer = 0 TO 4
   draw_menu menu(i), state(i), vpage
  NEXT i  
  setvispage vpage
  dowait
 LOOP
 
 setkeys
 RETURN result
END FUNCTION

SUB attack_alignment_editor (byval attack_id as integer, byref xoff as integer, byref yoff as integer, byref halign as integer, byref valign as integer)

 DIM attack as AttackData
 loadattackdata attack, attack_id

 DIM menu(6) as string
 menu(0) = "Previous Menu..."

 DIM halign_cap(-1 TO 1) as string
 halign_cap(-1) = "Left"
 halign_cap(0) = "Center"
 halign_cap(1) = "Right"
 DIM valign_cap(-1 TO 1) as string
 valign_cap(-1) = "Top"
 valign_cap(0) = "Center"
 valign_cap(1) = "Bottom"

 DIM state as MenuState
 state.pt = 0
 state.last = UBOUND(menu)
 state.size = 22
 state.need_update = YES

 DIM previewpos as XYZTriple
 
 STATIC enemy_id as integer
 enemy_id = small(enemy_id, gen(genMaxEnemy))
 DIM enemy as EnemyDef
 
 DIM reverse as integer = 0

 DIM preview_box as Slice Ptr
 preview_box = NewSliceOfType(slRectangle)
 ChangeRectangleSlice preview_box, , uilook(uiDisabledItem), uilook(uiMenuItem), , transOpaque
 '--Align the box in the bottom right
 WITH *preview_box
  .X = -8
  .Y = -8
  .Width = 120
  .Height = 120 
  .PaddingTop = 1
  .PaddingBottom = 1
  .PaddingLeft = 1
  .PaddingRight = 1
  .AnchorHoriz = alignRight
  .AlignHoriz = alignRight
  .AnchorVert = alignBottom
  .AlignVert = alignBottom
 END WITH
 
 DIM targ_spr as Slice Ptr
 targ_spr = NewSliceOfType(slSprite, preview_box)

 DIM atk_spr as Slice Ptr
 atk_spr = NewSliceOfType(slSprite, preview_box)

 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF state.need_update THEN
   state.need_update = NO
   menu(1) = "Attack X Offset: " & xoff & IIF(reverse <> 0, "(mirrored)", "")
   menu(2) = "Attack Y Offset: " & yoff
   menu(3) = "Horizontal Alignment: " & safe_caption(halign_cap(), halign, "alignment") & IIF(reverse <> 0, "(mirrored)", "")
   menu(4) = "Vertical Alignment: " & safe_caption(valign_cap(), valign, "alignment")
   menu(5) = "Preview on Enemy: " & enemy_id & " " & readenemyname(enemy_id)
   menu(6) = "Attack done by " & IIF(reverse <> 0, "Enemy", "Hero")
   attack.targ_offset_x = xoff
   attack.targ_offset_y = yoff
   attack.targ_halign = halign
   attack.targ_valign = valign
   loadenemydata enemy, enemy_id
   ChangeSpriteSlice targ_spr, sprTypeSmallEnemy + enemy.size, enemy.pic, enemy.pal
   targ_spr->pos = (preview_box->size - targ_spr->size) \ 2
   previewpos = attack_placement_over_targetpos(attack, TYPE<XYZTriple>(targ_spr->x, targ_spr->y, 0), targ_spr->size, NO, reverse)
   atk_spr->X = previewpos.x
   atk_spr->Y = previewpos.y - previewpos.z
   ChangeSpriteSlice atk_spr, sprTypeAttack, attack.picture, attack.pal, , IIF(attack.unreversable_picture, 0, reverse)
  END IF
  
  usemenu state
  IF keyval(ccCancel) > 1 THEN
   EXIT DO
  END IF
  IF keyval(scF1) > 1 THEN show_help "attack_alignment_editor"
  
  'Activate
  IF enter_space_click(state) THEN
   SELECT CASE state.pt
    CASE 0: EXIT DO
    CASE 5:
     enemy_id = enemy_picker(enemy_id)
     state.need_update = YES
    CASE 6:
     reverse XOR= 1
     state.need_update = YES
   END SELECT
  END IF

  'Typing
  SELECT CASE state.pt
   CASE 1:
    IF intgrabber(xoff, -9999, 9999) THEN state.need_update = YES
   CASE 2:
    IF intgrabber(yoff, -9999, 9999) THEN state.need_update = YES
   CASE 3:
    IF intgrabber(halign, -1, 1) THEN state.need_update = YES
   CASE 4:
    IF intgrabber(valign, -1, 1) THEN state.need_update = YES
   CASE 5:
    IF intgrabber(enemy_id, 0, gen(genMaxEnemy)) THEN state.need_update = YES
   CASE 6:
    IF intgrabber(reverse, 0, 1) THEN state.need_update = YES
  END SELECT

  clearpage vpage
  'Paint preview
  atk_edit_preview attack.anim_pattern, atk_spr
  DrawSlice preview_box, vpage
  IF prefbit(36) THEN
   wrapprint """Old attack positioning"" bitset is on, which disables attack alignment", pInfoX, pInfoY, uilook(uiText), vpage
  END IF
  'Show menu
  standardmenu menu(), state, , , vpage
  setvispage vpage
  dowait
 LOOP

 DeleteSlice @preview_box
 
END SUB

'---------------------------- Attack chain browser -----------------------------


SUB update_attack_editor_for_chain (byval mode as integer, byref caption1 as string, byref max1 as integer, byref min1 as integer, byref menutype1 as integer, byref caption2 as string, byref max2 as integer, byref min2 as integer, byref menutype2 as integer, rate as integer, stat as integer)
 SELECT CASE mode
  CASE 0 '--no special condition
   'Set limits to maximally permissive to preserve existing values
   caption1 = ""
   max1 = 32767
   min1 = -32768
   menutype1 = 18'skipper
   caption2 = ""
   max2 = 32767
   min2 = -32768
   menutype2 = 18'skipper
  CASE 1 '--tagcheck
   caption1 = "    if Tag:"
   max1 = max_tag()
   min1 = -max_tag()
   menutype1 = 2
   caption2 = "    and Tag:"
   max2 = max_tag()
   min2 = -max_tag()
   menutype2 = 2
  CASE 2 TO 19 ' stat checks
   SELECT CASE mode
    CASE 2 TO 5, 16: caption1 = "    if attacker"
    CASE 6 TO 9: caption1 = "    if any target's"
    CASE 10 TO 15: caption1 = "    if all targets'"
    CASE 17: caption1 = "    Stat: attacker's"
    CASE 18: caption1 = "    Stat: max any target's"
    CASE 19: caption1 = "    Stat: min all targets'"
   END SELECT
   max1 = 15
   min1 = 0
   menutype1 = 16 'stat
   SELECT CASE mode
    CASE 2,6,10
     caption2 = "    is >"
     max2 = 32767
     min2 = -32767
     menutype2 = 0
    CASE 3,7,11
     caption2 = "    is <"
     max2 = 32767
     min2 = -32767
     menutype2 = 0
    CASE 4,8,12
     caption2 = "    is >"
     max2 = 32767
     min2 = 0
     menutype2 = 17 'int%
    CASE 5,9,13
     caption2 = "    is <"
     max2 = 32767
     min2 = 0
     menutype2 = 17 'int%
    CASE 14
     caption2 = "    is > attacker"
     max2 = 15
     min2 = 0
     menutype2 = 16 ' stat
    CASE 15
     caption2 = "    is < attacker"
     max2 = 15
     min2 = 0
     menutype2 = 16 ' stat
    CASE 16
     caption2 = "    is < attacker"
     max2 = 15
     min2 = 0
     menutype2 = 16 ' stat
    CASE 17, 18, 19
     'DIM statname as string = LEFT(battle_statnames(stat), 8)
     'caption2 = "    from 0% at " & statname & "=0 to " & rate & "% at " & statname & " >="
     caption2 = "    from 0% at stat=0 to " & rate & "% at stat>=$$"
     max2 = 32767
     min2 = 1
     menutype2 = 0
   END SELECT
 END SELECT
END SUB

FUNCTION attack_chain_browser (byval start_attack as integer) as integer
 DIM state as AttackChainBrowserState
 DIM selected as integer = start_attack

 v_new state.chainfrom
 v_new state.chainto

 DO
  '--Init

  state.root = NewSliceOfType(slContainer)
  state.root->Fill = YES

  state.lbox = NewSliceOfType(slLayout, state.root)
  state.lbox->Width = 80
  state.lbox->LayoutData->secondary_padding = 6

  state.rbox = NewSliceOfType(slLayout, state.root)
  state.rbox->Width = 80
  state.rbox->AlignHoriz = 2
  state.rbox->AnchorHoriz = 2
  state.rbox->LayoutData->secondary_padding = 6

  init_attack_chain_screen selected, state

  state.column = 1
  state.refresh = YES
  state.focused = state.current

  state.before.pt = 0
  state.before.top = 0
  state.after.pt = 0
  state.after.top = 0
 
  setkeys
  DO
   setwait 55
   setkeys

   'TODO: switch to a plankmenu & scroll slice instead
   state.before.size = large(1, (vpages(vpage)->h + 6) \ 56) - 1
   state.after.size = state.before.size

   IF keyval(ccCancel) > 1 THEN
    state.done = YES
    EXIT DO
   END IF
   IF keyval(scF1) > 1 THEN show_help "attack_chain_browse"
   IF keyval(scF6) > 1 THEN slice_editor state.root, SL_COLLECT_EDITOR

   IF enter_or_space() THEN
    IF state.focused <> 0 THEN
     IF state.focused->extra(0) >= 0 THEN  '"(None)" isn't selected
      IF state.column = 1 THEN state.done = YES
      selected = state.focused->extra(0)
      EXIT DO
     END IF
    END IF
   END IF

   IF keyval(ccLeft) > 1 THEN loopvar(state.column, 0, 2, -1) : state.refresh = YES
   IF keyval(ccRight) > 1 THEN loopvar(state.column, 0, 2, 1) : state.refresh = YES
   SELECT CASE state.column
    CASE 0: IF usemenu(state.before) THEN state.refresh = YES
    CASE 1: 
    CASE 2: IF usemenu(state.after) THEN state.refresh = YES
   END SELECT
   
   IF state.refresh THEN
    state.refresh = NO
    attack_preview_slice_defocus state.focused
    state.focused = NULL
    SELECT CASE state.column
     CASE 0:
      IF state.before.pt >= 0 THEN state.focused = state.chainfrom[state.before.pt]
     CASE 1:
      state.focused = state.current
     CASE 2:
      IF state.after.pt >= 0 THEN state.focused = state.chainto[state.after.pt]
    END SELECT
    attack_preview_slice_focus state.focused
    state.lbox->Y = state.before.top * -56
   END IF
 
   clearpage dpage
   DrawSlice state.root, dpage
 
   SWAP vpage, dpage
   setvispage vpage
   dowait
  LOOP
  
  DeleteSlice @state.root
  IF state.done THEN EXIT DO
 LOOP

 v_free state.chainfrom
 v_free state.chainto

 RETURN selected
END FUNCTION

SUB init_attack_chain_screen(byval attack_id as integer, state as AttackChainBrowserState)
 DIM atk as AttackData
 loadattackdata atk, attack_id
 
 state.current = create_attack_preview_slice("", attack_id, state.root)
 state.current->AnchorHoriz = 1
 state.current->AlignHoriz = 1
 state.current->Y = 6

 v_resize state.chainto, 0
 IF atk.instead.atk_id > 0 THEN
  v_append state.chainto, create_attack_preview_slice("Instead", atk.instead.atk_id - 1, state.rbox)
 END IF
 IF atk.chain.atk_id > 0 THEN
  v_append state.chainto, create_attack_preview_slice("Regular", atk.chain.atk_id - 1, state.rbox)
 END IF
 IF atk.elsechain.atk_id > 0 THEN
  v_append state.chainto, create_attack_preview_slice("Else", atk.elsechain.atk_id - 1, state.rbox)
 END IF

 IF v_len(state.chainto) = 0 THEN
  v_append state.chainto, create_attack_preview_dummy_slice("(None)", state.rbox)
 END IF

 state.after.last = v_len(state.chainto) - 1

 '--now search for attacks that chain to this one
 v_resize state.chainfrom, 0
 FOR i as integer = 0 TO gen(genMaxAttack)
  loadattackdata atk, i
  IF atk.chain.atk_id - 1 = attack_id THEN
   v_append state.chainfrom, create_attack_preview_slice("Regular", i, state.lbox)
  END IF
  IF atk.elsechain.atk_id - 1 = attack_id THEN
   v_append state.chainfrom, create_attack_preview_slice("Else", i, state.lbox)
  END IF
  IF atk.instead.atk_id - 1 = attack_id THEN
   v_append state.chainfrom, create_attack_preview_slice("Instead", i, state.lbox)
  END IF
 NEXT i

 IF v_len(state.chainfrom) = 0 THEN
  v_append state.chainfrom, create_attack_preview_dummy_slice("(None)", state.lbox)
 END IF

 state.before.last = v_len(state.chainfrom) - 1
END SUB

FUNCTION create_attack_preview_dummy_slice(caption as string, byval parent as Slice Ptr) as Slice Ptr
 DIM box as Slice ptr = NewSliceOfType(slRectangle, parent)
 box->Width = 80
 box->Height = 10
 ChangeRectangleSlice box, 0
 ChangeRectangleSlice box, , , , -1

 DIM numsl as Slice Ptr = NewSliceOfType(slText, box)
 ChangeTextSlice numsl, caption, , YES
 numsl->AnchorHoriz = 1
 numsl->AlignHoriz = 1

 box->extra(0) = -1  'Not an attack
 RETURN box
END FUNCTION

FUNCTION create_attack_preview_slice(caption as string, byval attack_id as integer, byval parent as Slice Ptr) as Slice Ptr
 DIM atk as AttackData
 loadattackdata atk, attack_id
 
 DIM box as Slice Ptr = NewSliceOfType(slRectangle, parent)
 box->Width = 80
 box->Height = 50
 ChangeRectangleSlice box, 0
 ChangeRectangleSlice box, , , , -1

 DIM spr as Slice Ptr = NewSliceOfType(slSprite, box)
 ChangeSpriteSlice spr, sprTypeAttack, atk.picture, atk.pal, 2
 spr->AnchorHoriz = 1
 spr->AlignHoriz = 1
 spr->AnchorVert = 2
 spr->AlignVert = 2

 DIM numsl as Slice Ptr = NewSliceOfType(slText, box)
 ChangeTextSlice numsl, STR(attack_id), , YES
 numsl->AnchorHoriz = 1
 numsl->AlignHoriz = 1
 
 DIM namesl as Slice Ptr = NewSliceOfType(slText, box)
 ChangeTextSlice namesl, atk.name, , YES
 namesl->AnchorHoriz = 1
 namesl->AlignHoriz = 1
 namesl->Y = 10

 DIM capsl as Slice Ptr = NewSliceOfType(slText, box)
 ChangeTextSlice capsl, caption, , YES
 capsl->AnchorHoriz = 1
 capsl->AlignHoriz = 1
 capsl->AnchorVert = 2
 capsl->AlignVert = 2

 '--Save attack_id in the extra data
 box->extra(0) = attack_id
 RETURN box
END FUNCTION

SUB attack_preview_slice_focus(byval sl as Slice Ptr)
 IF sl = 0 THEN EXIT SUB
 ChangeRectangleSlice sl, , , , 0
 DIM ch as Slice Ptr = sl->FirstChild
 WHILE ch
  IF ch->SliceType = slText THEN
   ChangeTextSlice ch, , uilook(uiSelectedItem)
  END IF
  ch = ch->NextSibling
 WEND
END SUB

SUB attack_preview_slice_defocus(byval sl as Slice Ptr)
 IF sl = 0 THEN EXIT SUB
 ChangeRectangleSlice sl, , , , -1
 DIM ch as Slice Ptr = sl->FirstChild
 WHILE ch
  IF ch->SliceType = slText THEN
   ChangeTextSlice ch, , uilook(uiText)
  END IF
  ch = ch->NextSibling
 WEND
END SUB
