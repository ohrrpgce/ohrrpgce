#IFNDEF BATTLE_UDTS_BI
#DEFINE BATTLE_UDTS_BI

'This file contains UDTs that only get used in battle mode,
'so as to prevent them from cluttering up the global udts.bi file

UNION BattleStatsSingle
  'See also HeroStatsSingle '-- the two of these can probably be unified eventually
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

TYPE BattleHeroMenu
  menu AS INTEGER 'Same as bmenu(hero,slot)
  caption AS STRING
  atk AS INTEGER 'Attack ID (or -1 for none)
  spell_list AS INTEGER ' index of spell list (or -10 for items or -1 for none)
END TYPE

TYPE HarmText 'FIXME: when battle display is converted to slices, this can go away entirely
 text AS STRING
 col AS INTEGER
 pos AS XYPair
 ticks AS INTEGER
END TYPE

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
  '--stats
  stat AS BattleStats
  '--level-mp
  consume_lmp AS INTEGER '0 for no LMP consumption, >= 1 to indicate which level of MP should be deducted
                         'FIXME: consume_lmp would probably be better as a member of the AttackState or AttackQueue later on.
  '--misc
  dissolve AS INTEGER
  flee AS INTEGER ' used to indicate when a sprite animates running away (not to be confused with BattleState.flee)
  attack_succeeded AS INTEGER
  sprites as Frame ptr 'the graphic set
  sprite_num AS INTEGER 'how many frames
  frame AS INTEGER 'the current frame
  walk AS INTEGER 'used by heroes when animating walking
  anim_pattern AS INTEGER 'used by attack sprites
  anim_index AS INTEGER 'used by attack sprites
  pal as palette16 ptr 'yeah
  deathtype AS INTEGER 'for enemies (0 = default, otherwise is type + 1)
  deathtime AS INTEGER '0 = default, otherwise is time + 1
  death_sfx AS INTEGER '0 = default, -1 = none, >0 = sfx ID + 1
  revengeharm AS INTEGER 'The last damage dealt TO this hero or enemy
  thankvengecure AS INTEGER 'The cure damage undealt TO this hero or enemy (as a positive number!)
  repeatharm AS INTEGER 'The last damage dealy BY this hero or enemy
  cursorpos AS XYPair
  menu(5)   AS BattleHeroMenu 'Only applies to heroes. blank for enemies
  menu_size AS INTEGER 'actually the index of the last used element in .menu()
  harm AS HarmText
  '--affliction state
  poison_repeat AS INTEGER
  regen_repeat AS INTEGER
  '--Turn-taking
  ready_meter AS INTEGER '0-1000, fills based on speed. When 1000, set .ready=YES
  ready  AS INTEGER  ' YES if the hero or enemy can have a turn, NO if they are not ready yet
  attack AS INTEGER  ' ID number +1 of the attack that this hero or enemy is going to do next
  delay AS INTEGER   ' Number of ticks before prepared attack animates (counts down)
  '--Targetting
  t(12)               AS INTEGER 'Currently selected target slots. -1 means no target. Targets must be sorted to the beginning if the list changes 
  revenge             AS INTEGER 'ID of last hero or enemy who damaged this hero or enemy, or -1 for none
  thankvenge          AS INTEGER 'ID of last hero or enemy who cured this hero or enemy, or -1 for none
  revengemask(11)     AS INTEGER 'YES for each hero or enemy who has damaged this hero/enemy at least once, otherwise NO
  thankvengemask(11)  AS INTEGER 'YES for each hero or enemy who has cured this hero/enemy at least once, otherwise NO
  last_targs(11)      AS INTEGER 'YES for each target previously hit by this hero/enemy, otherwise NO
  stored_targs(11)    AS INTEGER 'YES for each stored target for ths hero/enemy, otherwise NO
  keep_dead_targs(11) AS INTEGER 'YES to mark targets of attacks that can target the dead (used in sorting of .t)
  stored_targs_can_be_dead AS INTEGER
  '--Bitsets
  weak(7)        AS INTEGER 'YES/NO for weakness to each element
  strong(7)      AS INTEGER 'YES/NO for strength to each element
  absorb(7)      AS INTEGER 'YES/NO to absorb each element
  enemytype(7)   AS INTEGER 'YES/NO for membership in each enemy type
  harmed_by_cure AS INTEGER 'YES/NO
  mp_idiot       AS INTEGER 'YES/NO for turn loss when using MP-consuming attacks after MP runs out
  is_boss        AS INTEGER 'YES/NO
  unescapable    AS INTEGER 'YES/NO
  die_without_boss    AS INTEGER 'YES/NO
  flee_instead_of_die AS INTEGER 'YES/NO
  enemy_untargetable  AS INTEGER 'YES/NO
  hero_untargetable   AS INTEGER 'YES/NO
  death_unneeded AS INTEGER 'YES/NO
  never_flinch   AS INTEGER 'YES/NO
  ignore_for_alone    AS INTEGER 'YES/NO
  '--
  enemy AS EnemyDef '--only populated if this slot holds an enemy
  '--
  lifemeter AS INTEGER 'FIXME: this can be replaced by the width property of the lifemeter slice
                       'when lifemeters have been converted to slices
END TYPE

'This type stores the state of the currently animating attack
TYPE AttackState
 id AS INTEGER            'Attack ID of the current attack or -1 for none.
                          ' only set when the attack delay is over, and
                          ' cleared when animation finishes
 was_id AS INTEGER        'Attack ID of the animating attack. Cleared after fulldeathcheck is finished
 '--Elementals are stored in AttackState just for the benefit of elemental spawning
 non_elemental AS INTEGER 'YES or NO
 elemental(7) AS INTEGER  'YES or NO for each element
 has_consumed_costs AS INTEGER 'YES or NO, prevents multi-hit attacks from consuming MP more than once
END TYPE

'This type stores the state of target selection.
TYPE TargettingState
  mode AS INTEGER         'targNONE targSETUP targMANUAL targAUTO
  pointer AS INTEGER      'Slot number of the currently selected (but not yet chosen) target slot
  hit_dead AS INTEGER     'YES if this is a "Life" spell, or NO for all other attacks
  mask(11) AS INTEGER     'For the currently targetting hero, a list of 1/0 values indicating
                          'which targets are valid for the currently targetting attack
  selected(11) AS INTEGER 'For the currently targetting hero, a list of 1/0 values indicating
                          ' which targets from .mask() are currently selected.
  opt_spread AS INTEGER   '0 no, 1 allowed, 2 spread
  interactive AS INTEGER  'YES or NO for current attack
  roulette AS INTEGER     'YES or NO for current attack
  force_first AS INTEGER  'YES or NO for current attack
END TYPE
'.mode > 0 means hero picking a target
CONST targNONE   = 0 'means hero not currently picking a target
CONST targSETUP  = 1 'means targetting needs set-up
CONST targMANUAL = 2 'means normal manual targetting
CONST targAUTO   = 3 'means autotargeting

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

'These handle the state of the currently displaying spell menu
TYPE SpellMenuItem
 name AS STRING
 desc AS STRING
 cost AS STRING
 atk_id AS INTEGER
 enable AS INTEGER 'YES or NO
END TYPE
TYPE SpellMenuState
 slot(23) AS SpellMenuItem
END TYPE

'--These patterns are used for attack animation frame oscillation
TYPE AttackAnimationPattern
 frame(10) AS INTEGER
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
 death_mode AS INTEGER 'deathNOBODY deathENEMIES deathHEROES
 targ AS TargettingState
 atk AS AttackState
 pt AS INTEGER         'menu cursor for hero menu
 listslot AS INTEGER   'currently active hero spell list slot
 sptr AS INTEGER       'menu cursor for hero spell list menu
 item AS MenuState     'menu cursor for items menu
 item_desc AS STRING   'description of currently selected item in item menu
 caption AS STRING     'currently displaying caption
 caption_time AS INTEGER 'time for the currently displaying caption
 caption_delay AS INTEGER 'delay before current caption displays
 anim_ready AS INTEGER 'YES or NO indicating whether the current animation has bene generated
 animpat(3) AS AttackAnimationPattern = _
        { ({0,0,1,1,2,2,-1}), _
          ({2,2,1,1,0,0,-1}), _
          ({0,0,1,1,2,2,1,1,0,0,-1}), _
          ({-1,-1}) }
 curbg AS INTEGER      'Current background
 wait_frames AS INTEGER 'used by the attack animation
 level_mp_caption AS STRING
 cannot_run_caption AS STRING
 cancel_spell_caption AS STRING
 flee AS INTEGER 'used by the crappy running system, not to be confused with BattleSprite.flee
 away AS INTEGER 'used by the crappy running system.
 alert_ticks AS INTEGER
 alert AS STRING
 tog AS INTEGER 'alternates 0,1,0,1 tick by tick
 laststun AS DOUBLE
 vic AS VictoryState
 rew AS RewardsState
 spell AS SpellMenuState
 inv_scroll AS MenuState
 inv_scroll_rect AS RectType
 iuse(inventoryMax / 16) AS INTEGER 'bitsets for whether items can be used by the current hero
END TYPE

CONST batMENUHERO = 0
CONST batMENUSPELL = 1
CONST batMENUITEM = 2
'--Used by the .deathmode member
CONST deathNOBODY  = 0
CONST deathENEMIES = 1
CONST deathHEROES  = 2

TYPE AttackQueue
 used     AS INTEGER 'YES when used, NO when recycleable
 attack   AS INTEGER 'attack ID number
 attacker AS INTEGER 'slot number of attacker
 t(12)    AS INTEGER 'Targeted slots, -1 for empty, or slot number.
 blocking AS INTEGER 'YES for normal, NO for nonblocking
END TYPE

#ENDIF
