#IFNDEF BATTLE_UDTS_BI
#DEFINE BATTLE_UDTS_BI

'This file contains UDTs that only get used in battle mode,
'so as to prevent them from cluttering up the global udts.bi file

UNION BattleStatsSingle
  'See also Stats '-- the two of these can probably be unified eventually
  TYPE
    hp as integer   '0
    mp as integer   '1
    str as integer  '2
    acc as integer  '3
    def as integer  '4
    dog as integer  '5
    mag as integer  '6
    wil as integer  '7
    spd as integer  '8
    ctr as integer  '9
    foc as integer  '10
    hits as integer '11
    poison as integer
    regen as integer
    stun as integer
    mute as integer
  END TYPE
  sta(15) as integer
End UNION

TYPE BattleStats
  cur as BattleStatsSingle
  max as BattleStatsSingle
END TYPE

TYPE HarmText 'FIXME: when battle display is converted to slices, this can go away entirely
 text as string
 col as integer
 pos as XYPair
 ticks as integer
END TYPE

'Names for hero battle sprite frames
CONST frameSTAND = 0
CONST frameVICTORYB = 0
CONST frameSTEP = 1
CONST frameLAND = 2
CONST frameVICTORYA = 2
CONST frameATTACKA = 2
CONST frameATTACKB = 3
CONST frameCAST = 4
CONST frameJUMP = 4
CONST frameHURT = 5
CONST frameWEAK = 6
CONST frameDEAD = 7

TYPE BattleSprite
  name as string
  basex as integer
  basey as integer
  x as integer
  y as integer
  z as integer
  w as integer
  h as integer
  d as integer
  xmov as integer
  ymov as integer
  zmov as integer
  xspeed as integer
  yspeed as integer
  zspeed as integer
  vis as integer
  '--stats
  stat as BattleStats
  elementaldmg(maxElements - 1) as single
  '--level-mp
  consume_lmp as integer '0 for no LMP consumption, >= 1 to indicate which level of MP should be deducted
                         'FIXME: consume_lmp would probably be better as a member of the AttackState or AttackQueue later on.
  '--item consumption
  consume_item as integer ' -1 means no item consumed, >=0 indicates which inventory slot will be used (NOT item ID)
                         'FIXME: consume_item would probably be better as a member of the AttackState or AttackQueue later on.
  '--battle menu
  batmenu as MenuDef
  menust as MenuState
  '--misc
  dissolve as integer
  flee as integer ' used to indicate when a sprite animates running away (not to be confused with BattleState.flee)
  attack_succeeded as integer
  sprites as Frame ptr 'the graphic set
  sprite_num as integer 'how many frames
  frame as integer 'the current frame (if a hero, one of the frame* constants)
  walk as integer 'used by heroes when animating walking
  anim_pattern as integer 'used by attack sprites
  anim_index as integer 'used by attack sprites
  pal as palette16 ptr 'yeah
  deathtype as integer 'for enemies (0 = default, otherwise is type + 1)
  deathtime as integer '0 = default, otherwise is time + 1
  death_sfx as integer '0 = default, -1 = none, >0 = sfx ID + 1
  revengeharm as integer 'The last damage dealt TO this hero or enemy
  thankvengecure as integer 'The cure damage undealt TO this hero or enemy (as a positive number!)
  repeatharm as integer 'The last damage dealy BY this hero or enemy
  cursorpos as XYPair
  harm as HarmText
  hand(1) as XYPair ' For weapons = handle pos. For heroes, intended as hand position but not used yet
  '--used only for turnTURN mode
  initiative_order as integer
  no_attack_this_turn as integer
  '--used only for turnACTIVE mode
  active_turn_num as integer
  '--affliction state
  poison_repeat as integer
  regen_repeat as integer
  '--Turn-taking
  ready_meter as integer '0-1000, fills based on speed. When 1000, set .ready=YES
  ready  as integer  ' YES if the hero or enemy can have a turn, NO if they are not ready yet
  attack as integer  ' ID number +1 of the attack that this hero or enemy is going to do next
  '--Targetting
  revenge             as integer 'ID of last hero or enemy who damaged this hero or enemy, or -1 for none
  thankvenge          as integer 'ID of last hero or enemy who cured this hero or enemy, or -1 for none
  revengemask(11)     as integer 'YES for each hero or enemy who has damaged this hero/enemy at least once, otherwise NO
  thankvengemask(11)  as integer 'YES for each hero or enemy who has cured this hero/enemy at least once, otherwise NO
  last_targs(11)      as integer 'YES for each target previously hit by this hero/enemy, otherwise NO
  stored_targs(11)    as integer 'YES for each stored target for ths hero/enemy, otherwise NO
  stored_targs_can_be_dead as integer
  '--Bitsets
  harmed_by_cure as integer 'YES/NO
  mp_idiot       as integer 'YES/NO for turn loss when using MP-consuming attacks after MP runs out
  is_boss        as integer 'YES/NO
  unescapable    as integer 'YES/NO
  die_without_boss    as integer 'YES/NO
  flee_instead_of_die as integer 'YES/NO
  enemy_untargetable  as integer 'YES/NO
  hero_untargetable   as integer 'YES/NO
  death_unneeded as integer 'YES/NO
  never_flinch   as integer 'YES/NO
  ignore_for_alone    as integer 'YES/NO
  '--counterattacking
  elem_counter_attack(maxElements - 1) as integer
  stat_counter_attack(11) as integer
  '--
  enemy as EnemyDef '--only populated if this slot holds an enemy
  '--
  lifemeter as integer 'FIXME: this can be replaced by the width property of the lifemeter slice
                        'when lifemeters have been converted to slices
  bequesting as integer ' YES/NO if true, this character is doing a final attack before
                         ' they die. Death is delayed until there are no more queued attacks
                         ' for this attacker. A bequesting character cannot be targeted by new
                         ' attacks, and does not take any more normal turns. If the bequested
                         ' attack is a self-targeting cure attack, the attacker's death can be
                         ' cancelled.
END TYPE

'This type stores the state of the currently animating attack
TYPE AttackState
 id as integer            'Attack ID of the current attack or -1 for none.
                          ' only set when the attack delay is over, and
                          ' cleared when animation finishes
 was_id as integer        'Attack ID of the animating attack. Cleared after fulldeathcheck is finished
 '--Elementals are stored in AttackState just for the benefit of elemental spawning
 non_elemental as integer 'YES or NO
 elemental(maxElements - 1) as integer  'YES or NO for each element
 has_consumed_costs as integer 'YES or NO, prevents multi-hit attacks from consuming MP more than once
END TYPE

'This type stores the state of target selection.
TYPE TargettingState
  mode as integer         'targNONE targSETUP targMANUAL targAUTO
  pointer as integer      'Slot number of the currently selected (but not yet chosen) target slot
  hit_dead as integer     'YES if this is a "Life" spell, or NO for all other attacks
  mask(11) as integer     'For the currently targetting hero, a list of 1/0 values indicating
                          'which targets are valid for the currently targetting attack
  selected(11) as integer 'For the currently targetting hero, a list of 1/0 values indicating
                          ' which targets from .mask() are currently selected.
  opt_spread as integer   '0 no, 1 allowed, 2 spread
  interactive as integer  'YES or NO for current attack
  roulette as integer     'YES or NO for current attack
  force_first as integer  'YES or NO for current attack
  atk as AttackData        'Loaded in setup_targeting()
                           'and should only be trusted while mode=targMANUAL
END TYPE
'.mode > 0 means hero picking a target
CONST targNONE   = 0 'means hero not currently picking a target
CONST targSETUP  = 1 'means targetting needs set-up
CONST targMANUAL = 2 'means normal manual targetting
CONST targAUTO   = 3 'means autotargeting

'This type stores the visual state of the victory display
TYPE VictoryState
 state as integer 'vicSTATENAME or 0 for none
 box as integer   'NO when not displaying a box, YES when displaying a box
 showlearn as integer 'NO when not showing spell learning, YES when already showing a learned spell
 learnwho as integer 'battle slot of hero currently displaying learned spells
 learnlist as integer 'spell list of hero currently displaying learned spells
 learnslot as integer 'spell list slot of hero currently displaying learned spells
 item_name as string 'name of currently displaying found item or "" for none
 found_index as integer 'index into the found() array that lists items found in this battle
 gold_caption as string
 exp_caption as string
 item_caption as string
 plural_item_caption as string
 gold_name as string
 exp_name as string
 level_up_caption as string
 levels_up_caption as string
 learned_caption as string
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
 id as integer
 num as integer
END TYPE

'This type controls the state of rewards gathered in the current battle
TYPE RewardsState
 plunder as integer
 exper as integer
 found(16) as RewardsStateItem
END TYPE

'These handle the state of the currently displaying spell menu
TYPE SpellMenuItem
 name as string
 desc as string
 cost as string
 atk_id as integer
 enable as integer 'YES or NO
END TYPE
TYPE SpellMenuState
 slot(23) as SpellMenuItem
END TYPE

'--These patterns are used for attack animation frame oscillation
TYPE AttackAnimationPattern
 frame(10) as integer
END TYPE

CONST turnACTIVE = 0 'Take turns when asynchonous ready-meters fill
CONST turnTURN = 1 'Everyone takes turns together
TYPE TurnManager
 mode as integer = turnACTIVE
 '--stuff used by all modes
 '--stuff used only by turnACTIVE
 '--stuff used only by turnTURN
 choosing_attacks as integer 'YES/NO
 number as integer ' number of the current turn (merely debugging information)
 reverse as integer 'YES/NO
END TYPE

'This type stores the state of the battle engine, for example,
'who's turn it is, what each character is doing, and targetting information
TYPE BattleState
 turn as TurnManager
 ticks as integer      'number of ticks since the battle was initialized
 acting as integer     'Hero or enemy who is currently taking their turn to act
                       '(contains garbage if noone is acting... indicated by bat.atk.id < 0?)
 hero_turn as integer  '(MOVEME) Hero currently selecting an attack
 enemy_turn as integer '(MOVEME) Enemy currently selecting an attack
 next_hero as integer  '(MOVEME) counter that controls which ready hero will get their turn next
 next_enemy as integer 'counter that controls which ready enemy will get their turn next
 menu_mode as integer  'batMENUHERO batMENUSPELL or batMENUITEM
 death_mode as integer 'deathNOBODY deathENEMIES deathHEROES
 have_written_stats as integer 'writestats has been called. Don't do it again!
 targ as TargettingState
 atk as AttackState
 listslot as integer   'currently active hero spell list slot
 sptr as integer       'menu cursor for hero spell list menu
 item as MenuState     'menu cursor for items menu
 item_desc as string   'description of currently selected item in item menu
 caption as string     'currently displaying caption
 caption_time as integer 'time for the currently displaying caption
 caption_delay as integer 'delay before current caption displays
 anim_ready as integer 'YES or NO indicating whether the current animation has ben generated
 anim_t(11) as integer 'targets for currently animating attack
 anim_blocking_delay as integer 'whether the currently animating attack had a blocking delay (to make chains inherit non-blocking)
 animpat(3) as AttackAnimationPattern = _
        { ({0,0,1,1,2,2,-1}), _
          ({2,2,1,1,0,0,-1}), _
          ({0,0,1,1,2,2,1,1,0,0,-1}), _
          ({-1,-1}) }
 curbg as integer      'Current background
 bg_tick as integer    'Number of ticks since last background frame change
 wait_frames as integer 'used by the attack animation
 level_mp_caption as string
 cannot_run_caption as string
 cancel_spell_caption as string
 flee as integer 'used by the crappy running system, not to be confused with BattleSprite.flee
 away as integer 'used by the crappy running system.
 alert_ticks as integer
 alert as string
 tog as integer 'alternates 0,1,0,1 tick by tick
 laststun as integer
 vic as VictoryState
 rew as RewardsState
 spell as SpellMenuState
 inv_scroll as MenuState
 inv_scroll_rect as RectType
 iuse(inventoryMax / 16) as integer 'bitsets for whether items can be used by the current hero
 test_view_mode as integer 'used for debugging new display stuff with F12
 test_future as integer    'used for debugging new display stuff with F12
END TYPE

'--Used by the .menu_mode member
CONST batMENUHERO = 0 
CONST batMENUSPELL = 1
CONST batMENUITEM = 2
'--Used by the .deathmode member
CONST deathNOBODY  = 0
CONST deathENEMIES = 1
CONST deathHEROES  = 2
'--used by the .t member of the menu items in the .batmenu member
CONST batmenu_ATTACK = -1000
CONST batmenu_SPELLS = -1001
CONST batmenu_ITEMS  = -1002

TYPE AttackQueue
 used     as integer 'YES when used, NO when recycleable
 attack   as integer 'attack ID number
 attacker as integer 'slot number of attacker
 t(11)    as integer 'Targeted slots, -1 for empty, or slot number.
 blocking as integer 'YES to block attacker from acting, NO for nonblocking
 delay    as integer 'Number of ticks that should pass before this attack happens
 turn_delay as integer 'Number of turns to wait before the tick delay begins.
 dont_retarget as integer
END TYPE

#ENDIF
