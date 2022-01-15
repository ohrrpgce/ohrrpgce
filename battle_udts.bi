'OHRRPGCE GAME - Battle datastructures
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#IFNDEF BATTLE_UDTS_BI
#DEFINE BATTLE_UDTS_BI

'This file contains UDTs that only get used in battle mode,
'so as to prevent them from cluttering up the global udts.bi file

#include "slices.bi"

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
    focus as integer'10
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

TYPE BattleSprite
  name as string

  '--Sprites/slices
  sl as Slice ptr
  sprite as Slice ptr  'The sprite component of the slice

  DECLARE PROPERTY x() as integer
  DECLARE PROPERTY x(val as integer)
  DECLARE PROPERTY y() as integer
  DECLARE PROPERTY y(val as integer)
  DECLARE PROPERTY z() as integer
  DECLARE PROPERTY z(val as integer)
  DECLARE PROPERTY pos() byref as XYPair
  DECLARE PROPERTY w() as integer
  DECLARE PROPERTY w(val as integer)
  DECLARE PROPERTY h() as integer
  DECLARE PROPERTY h(val as integer)
  DECLARE PROPERTY size() byref as XYPair
  DECLARE PROPERTY frame() as integer
  DECLARE PROPERTY frame(fr as integer)

  basepos as XYPair
  d as integer
  xmov as integer
  ymov as integer
  zmov as integer
  xspeed as integer
  yspeed as integer
  zspeed as integer
  vis as bool   'Roughly, but not exactly, visibility:
                'For combatants: Is alive (or dying interruptted) and visible (didn't jump).
                '  Gets set to YES while an enemy is performing an on-death attack.
                '  Gets set to NO while it's dissolving or fleeing after death
                '  If true, treated as a valid target for attacks that can't hit dead.
                'For other BattleSprites: is visible
  hidden as bool ' For combatants, has been hidden by an attacker animation like "Jump" or "Run and Hide"
                 ' or any attack with "always hides attacker" bitset. Not targetable until unhidden
                 ' if true, excluded from all targetting except for attacks with attacker animation
                 ' "Land" or "Unhide" or any attack with the "always unhide attacker" bit

  '--stats
  stat as BattleStats
  elementaldmg(maxElements - 1) as single
  '--level-mp
  consume_lmp as integer '-1 for no LMP consumption, >= 0 to indicate which level of MP should be deducted
                         'FIXME: consume_lmp would probably be better as a member of the AttackState or AttackQueue later on.
  '--item consumption (when using an item; this is not part of attack costs)
  consume_item as integer ' -1 means no item consumed, >=0 indicates which inventory slot will be used (NOT item ID)
                         'FIXME: consume_item would probably be better as a member of the AttackState or AttackQueue later on.
  '--battle menu
  batmenu as MenuDef
  menust as MenuState
  '--misc
  dissolve as integer      'Ticks left in death dissolving animation (for enemies).
                           'Set to 1 for dead heroes, to make them visible and use death frame, and does not count down (yuck)
  dissolve_appear as integer 'Counts ticks *up* to appeartime while enemy appears
  fleeing as bool          'Sprite is animating running away (not to be confused with BattleState.flee)
  attack_succeeded as bool
  walk as integer 'used by heroes when animating walking
  anim_pattern as integer 'used by attack sprites
  anim_index as integer 'used by attack sprites
  deathtype as integer 'for enemies: dissolve animation type
  _deathtime as integer '0 = default, otherwise is time
  DECLARE FUNCTION deathtime() as integer  '_deathtime with default
  appeartype as integer '-1 means appear instantly, >= 0 is dissolve animation type
  _appeartime as integer '0 = default, otherwise is time
  DECLARE FUNCTION appeartime() as integer  '_appeartime with default
  death_sfx as integer '0 = default, -1 = none, >0 = sfx ID + 1
  revengeharm as integer 'The last damage dealt TO this hero or enemy
  thankvengecure as integer 'The cure damage undealt TO this hero or enemy (as a positive number!)
  repeatharm as integer 'The last damage dealt BY this hero or enemy
  cursorpos as XYPair  'Cursor position measured relative to center of top edge
  harm as HarmText
  hand(1) as XYPair ' For weapons = handle pos. For heroes, intended as hand position but not used yet (it's read directly from HeroState)
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
  revenge             as integer 'slot of last hero or enemy who damaged this hero or enemy, or -1 for none
  thankvenge          as integer 'slot of last hero or enemy who cured this hero or enemy, or -1 for none
  revengemask(11)     as integer 'YES for each hero or enemy who has damaged this hero/enemy at least once, otherwise NO
  thankvengemask(11)  as integer 'YES for each hero or enemy who has cured this hero/enemy at least once, otherwise NO
  counter_target      as integer 'slot of the last attacker to target this hero/enemy with any attack, -1 for none
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
  give_rewards_even_if_alive as bool
  '--counterattacking
  elem_counter_attack(maxElements - 1) as integer
  non_elem_counter_attack as integer
  stat_counter_attack(statLast) as integer
  '--
  enemy as EnemyDef '--only populated if this slot holds an enemy
  '--
  lifemeter as double    'Width of the (filled part of the) HP meter in pixels
  mpmeter as double      'Width of the (filled part of the) MP meter in pixels
  bequesting as bool     ' YES/NO if true, this character is triggering a final attack before
                         ' they die. Death is delayed until the bequest attack happens.
                         ' A bequesting character cannot be targeted by new
                         ' attacks (except self-targeting), and does not take any more normal turns. 
  self_bequesting as bool ' Only for self-targeted bequest attacks. Reset when the attack ends
                         'If the bequested attack is a self-targeting cure attack, or a
                         'transmogrify attack the attacker's death can be cancelled.
END TYPE

'This type stores the state of the currently animating attack
TYPE AttackState
 id as integer            'Attack ID of the current attack or -1 for none.
                          ' only set when the attack delay is over, and
                          ' cleared when animation finishes
 was_id as integer        'Attack ID of the animating attack. Cleared after fulldeathcheck is finished
 '--Elementals are stored in AttackState just for the benefit of elemental spawning
 non_elemental as bool
 elemental(maxElements - 1) as bool
 has_consumed_costs as bool  'Prevents multi-hit attacks from consuming MP more than once
END TYPE

'For TargettingState.mode
ENUM TargetMode
 targNONE   = 0 'means hero not currently picking a target
 targSETUP  = 1 'means targetting needs set-up
 targMANUAL = 2 'means normal manual targetting
 targAUTO   = 3 'means autotargeting
END ENUM

'This type stores the state of target selection.
TYPE TargettingState
  mode as TargetMode      '<> targNONE means hero picking a target
  pointer as integer      'Slot number of the currently selected (but not yet chosen) target slot
  hit_dead as bool        'YES if this is a "Life" spell, or NO for all other attacks
  mask(11) as bool        'For the currently targetting hero, indicates
                          'which targets are valid for the currently targetting attack
  selected(11) as bool    'For the currently targetting hero, indicates
                          ' which targets from .mask() are currently selected.
  opt_spread as integer   '0 no, 1 allowed, 2 spread
  interactive as bool     'for current attack
  roulette as bool        'for current attack
  force_first as bool     'for current attack
  atk as AttackData        'Loaded in setup_targeting()
                           'and should only be trusted while mode=targMANUAL
  hover as integer         'target that the mouse is hovering over or -1 if none
  mouse_optional_spread as bool 'Internal state for toggling optional spread with a drag
  must_hover_valid_target as bool 'Quit back to the menu if not currently hovering a valid target (for quick-targetting attacks directly from the menu)
END TYPE

'For VictoryState.state
ENUM VictoryStateEnum
 vicNONE = 0     'Victory hasn't happened
 vicGOLDEXP = 1
 vicLEVELUP = 2
 vicSPELLS  = 3
 vicITEMS   = 4
 'negative are non-displaying exit states
 vicEXITDELAY = -1
 vicEXIT    = -2
END ENUM

'This type stores the visual state of the victory display
TYPE VictoryState
 state as VictoryStateEnum
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
 exp_name as string
 level_up_caption as string
 levels_up_caption as string
 learned_caption as string
 display_ticks as integer ' Number of ticks that each victory screen has been displayed
END TYPE

'--Used by BattleState.death_mode
ENUM DeathMode
 deathNOBODY  = 0
 deathENEMIES = 1
 deathHEROES  = 2
END ENUM

'--Used by BattleState.menu_mode
ENUM BattleMenuMode
 batMENUHERO = 0   'Also used when no hero is acting
 batMENUSPELL = 1
 batMENUITEM = 2
END ENUM

'--used by the .t member of the menu items in the .batmenu member
ENUM BattleMenuItemType
 batmenu_ATTACK = mtypeLAST + 1
 batmenu_SPELLS
 batmenu_ITEMS
END ENUM

'This type is just used by RewardState
TYPE RewardsStateItem
 id as integer    'Not offset
 num as integer   'num = 0 indcates slot not used
END TYPE

'The rewards gathered in the current battle
TYPE RewardsState
 plunder as integer
 exper as integer
 found(16) as RewardsStateItem

 DECLARE SUB add_item(itemid as integer, count as integer = 1)
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
 choosing_attacks as bool
 number as integer ' number of the current turn (merely debugging information)
 reverse as bool   ' bat.next_hero loops backwards, used to cancel and return to previous hero
END TYPE

'This type stores the state of the battle engine, for example,
'who's turn it is, what each character is doing, and targetting information
TYPE BattleState
 root_sl as Slice ptr
 battlefield_sl as Slice ptr  'BattleSprite slices are parented to this

 turn as TurnManager
 ticks as integer      'number of ticks since the battle was initialized
 acting as integer     'Hero or enemy who is currently taking their turn to act
                       '(contains garbage if noone is acting... indicated by bat.atk.id < 0?)
 hero_turn as integer  '(MOVEME) Hero currently selecting an attack
 enemy_turn as integer '(MOVEME) Enemy currently selecting an attack
 next_hero as integer  '(MOVEME) counter that controls which ready hero will get their turn next
 next_enemy as integer 'counter that controls which ready enemy will get their turn next
 menu_mode as BattleMenuMode
 death_mode as DeathMode
 targ as TargettingState
 atk as AttackState
 listslot as integer   'currently active hero spell list slot
 sptr as integer       'menu cursor for hero spell list menu
 sptr_hover as integer 'menu cursor mouse hover for the spell list
 item as MenuState     'menu cursor for items menu
 item_drag_top as integer 'used for right-drag scrolling the item menu
 item_desc as string   'description of currently selected item in item menu
 caption as string          'Currently displaying caption
 caption_time as integer    'Remaining ticks before the caption is removed (0 if no caption)
 caption_delay as integer   'Remaining ticks to delay before .caption is actually made visible
 anim_ready as bool         'Indicates whether the current animation has been generated
 anim_t(11) as integer 'targets for currently animating attack
 anim_blocking_delay as integer 'whether the currently animating attack had a blocking delay (to make chains inherit non-blocking)
 animpat(3) as AttackAnimationPattern = _
        { ({0,0,1,1,2,2,-1}), _
          ({2,2,1,1,0,0,-1}), _
          ({0,0,1,1,2,2,1,1,0,0,-1}), _
          ({-1,-1}) }
 backdrop_sl as Slice ptr
 curbg as integer      'Current background
 bg_tick as integer    'Number of ticks since last background frame change
 wait_frames as integer 'used by the attack animation
 level_mp_caption as string
 cannot_run_caption as string
 cancel_spell_caption as string
 flee as integer            'Used by the crappy running system, not to be confused with BattleSprite.fleeing
 away as integer            'Used by the crappy running system.
 mouse_running as integer   'Counts the number of ticks the right mouse button has been held for the crappy running system
 alert_ticks as integer     'Number of ticks remaining to display .alert
 alert as string            'Separate message from caption, used only for 'CANNOT RUN'
 tog as integer             'Alternates 0,1,0,1 tick by tick
 laststun as integer
 vic as VictoryState
 rew as RewardsState
 spell as SpellMenuState
 inv_scroll as MenuState
 inv_scroll_rect as RectType
 iuse(inventoryMax / 16) as integer 'bitsets for whether items can be used by the current hero
 show_info_mode as integer '0=nothing, 1=show_enemy_meters, 2=display_attack_queue
 'The following don't do anything right now, but are handy to leave in
 test_view_mode as integer 'used for debugging new display stuff with F12
 test_future as integer    'used for debugging new display stuff with F12
END TYPE


TYPE AttackQueue
 used     as bool    'YES when used, NO when recycleable (TODO: use a vector instead)
 attack   as integer 'attack ID number
 attacker as integer 'slot number of attacker
 t(11)    as integer 'Targeted slots, -1 for empty, or slot number.
 blocking as bool    'Whether blocks attacker from acting
 delay    as integer 'turnACTIVE: Number of ticks that should pass before this attack happens
                     'turnTURN: sort key for ordering queued attacks
 turn_delay as integer 'Number of turns to wait before the .delay begins.
                       '(Can be negative in turnTURN, if didn't happen during intended round)
 dont_retarget as bool
END TYPE

#ENDIF
