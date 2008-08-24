#IFNDEF BATTLE_UDTS_BI
#DEFINE BATTLE_UDTS_BI

'This file contains UDTs that only get used in battle mode,
'so as to prevent them from cluttering up the global udts.bi file

TYPE BattleSprite
  name AS STRING
  basex AS INTEGER
  basey AS INTEGER
  x AS INTEGER
  y AS INTEGER
  z AS INTEGER
  w AS INTEGER
  h AS INTEGER
  d AS INTEGER
  xmov AS INTEGER
  ymov AS INTEGER
  zmov AS INTEGER
  xspeed AS INTEGER
  yspeed AS INTEGER
  zspeed AS INTEGER
  vis AS INTEGER
  hero_untargetable AS INTEGER
  enemy_untargetable AS INTEGER
  death_unneeded AS INTEGER
  dissolve AS INTEGER
  flee AS INTEGER
  attack_succeeded AS INTEGER
  sprites as Frame ptr 'the graphic set
  sprite_num AS INTEGER 'how many frames
  frame AS INTEGER 'the current frame
  pal as palette16 ptr 'yeah
  deathtype AS INTEGER 'for enemies (0 = default, otherwise is type + 1)
  deathtime AS INTEGER '0 = default, otherwise is time + 1
  death_sfx AS INTEGER '0 = default, -1 = none, >0 = sfx ID + 1
  ready AS INTEGER  ' YES if the hero or enemy can have a turn, NO if they are not ready yet
  attack AS INTEGER ' ID number +1 of the attack that this hero or enemy is going to do next
  t(12) AS INTEGER  ' Currently selected target slots. -1 means no target. Targets must be sorted to the beginning if the list changes 
  revenge AS INTEGER ' ID of last hero or enemy who damaged this hero or enemy, or -1 for none
  revengemask(11) AS INTEGER  'YES for each hero or enemy who has damaged this hero/enemy at least once, otherwise NO
  last_targs(11) AS INTEGER   'YES for each target previously hit by this hero/enemy, otherwise NO
  stored_targs(11) AS INTEGER 'YES for each stored target for ths hero/enemy, otherwise NO
END TYPE

UNION BattleStatsSingle
  TYPE
    hp AS INTEGER   '0
    mp AS INTEGER   '1
    str AS INTEGER  '2
    acc AS INTEGER  '3
    def AS INTEGER  '4
    dog AS INTEGER  '5
    mag AS INTEGER  '6
    wil AS INTEGER  '7
    spd AS INTEGER  '8
    ctr AS INTEGER  '9
    foc AS INTEGER  '10
    hits AS INTEGER '11
    poison AS INTEGER
    regen AS INTEGER
    stun AS INTEGER
    mute AS INTEGER
  END TYPE
  sta(15) AS INTEGER
End UNION

TYPE BattleStats
  cur AS BattleStatsSingle
  max AS BattleStatsSingle
END TYPE

'This type stores the state of the currently animating attack
TYPE AttackState
 '--Elementals are stored in AttackState just for the benefit of elemental spawning
 non_elemental AS INTEGER 'YES or NO
 elemental(7) AS INTEGER  'YES or NO for each element
END TYPE

'This type stores the state of target selection.
TYPE TargettingState
  pointer AS INTEGER      'Slot number of the currently selected (but not yet chosen) target slot
  hit_dead AS INTEGER     'YES if this is a "Life" spell, or NO for all other attacks
  mask(11) AS INTEGER     'For the currently targetting hero, a list of 1/0 values indicating
                          'which targets are valid for the currently targetting attack
  selected(11) AS INTEGER 'For the currently targetting hero, a list of 1/0 values indicating
                          ' which targets from .mask() are currently selected.
END TYPE

'This type stores the state of the battle engine, for example,
'who's turn it is, what each character is doing, and targetting information
TYPE BattleState
 acting AS INTEGER     'Hero or enemy who is currently taking their turn to act
 hero_turn AS INTEGER  'Hero currently selecting an attack
 enemy_turn AS INTEGER 'Enemy currently selecting an attack
 next_hero AS INTEGER  'counter that controls which ready hero will get their turn next
 next_enemy AS INTEGER 'counter that controls which ready enemy will get their turn next
 menu_mode AS INTEGER  'batMENUHERO batMENUSPELL or batMENUITEM
 targ AS TargettingState
 atk AS AttackState
END TYPE
CONST batMENUHERO = 0
CONST batMENUSPELL = 1
CONST batMENUITEM = 2

'This type stores the visual state of the victory display
TYPE VictoryState
 state AS INTEGER 'vicSTATENAME or 0 for none
 box AS INTEGER   'NO when not displaying a box, YES when displaying a box
 showlearn AS INTEGER 'NO when not showing spell learning, YES when already showing a learned spell
 learnwho AS INTEGER 'battle slot of hero currently displaying learned spells
 learnlist AS INTEGER 'spell list of hero currently displaying learned spells
 learnslot AS INTEGER 'spell list slot of hero currently displaying learned spells
 item_name AS STRING 'name of currently displaying found item or "" for none
 found_index AS INTEGER 'index into the found() array that lists items found in this battle
 gold_caption AS STRING
 exp_caption AS STRING
 item_caption AS STRING
 plural_item_caption AS STRING
 gold_name AS STRING
 exp_name AS STRING
 level_up_caption AS STRING
 levels_up_caption AS STRING
 learned_caption AS STRING
END TYPE
CONST vicGOLDEXP = 1
CONST vicLEVELUP = 2
CONST vicSPELLS  = 3
CONST vicITEMS   = 4
'negative are non-displaying exit states
CONST vicEXITDELAY = -1
CONST vicEXIT    = -2

'This type is just used by RewardState
TYPE RewardsStateItem
 id AS INTEGER
 num AS INTEGER
END TYPE

'This type controls the state of rewards gathered in the current battle
TYPE RewardsState
 plunder AS INTEGER
 exper AS INTEGER
 found(16) AS RewardsStateItem
END TYPE

#ENDIF