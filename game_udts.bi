#IFNDEF GAME_UDTS_BI
#DEFINE GAME_UDTS_BI

#INCLUDE "slices.bi"

'This file contains UDTs that only get used in game mode, and not in custom,
'so as to prevent them from cluttering up the global udts.bi file

TYPE MapModeState
  id       as integer
  lastmap  as integer 'ID of the last loaded map
  same     as integer 'YES/NO flag that indicates when your are moving through a same-map door
  name     as string
  door(99) as Door
  doorlinks(199) as doorlink
END TYPE

UNION HeroStatsSingle
  'See also BattleStatsSingle '-- the two of these can probably be unified eventually
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
  END TYPE
  sta(11) as integer
End UNION

TYPE HeroStats
  cur as HeroStatsSingle
  max as HeroStatsSingle
END TYPE

TYPE HeroState
 stat as HeroStats
 lev as integer
 lev_gain as integer
 exp_cur as integer
 exp_next as integer
 wep_pic as integer
 wep_pal as integer
 battle_pic as integer
 battle_pal as integer
 def_wep as integer '-default weapon
 pic as integer
 pal as integer
 rename_on_status as integer 'Renameable in status menu
 elementals(maxElements - 1) as SINGLE
 hand_pos(1) as XYPair
END TYPE

TYPE HeroWalkabout
  sl as Slice Ptr
  xgo as integer
  ygo as integer
  wtog as integer
  speed as integer
  curzones as integer vector
  'x, y, z, dir are in catx(), caty(), catz(), catd() instead *grumble*
END TYPE

TYPE ScriptLoggingState
  enabled as integer
  filename as string
  tick as integer                    'Number of times interpret has been called
  last_logged as integer             'scrat index
  output_flag as integer             'Set to true after script_log_out called
  last_wait_msg as string
  wait_msg_repeats as integer        'Number of ticks in a row with identical last_wait_msg
END TYPE

TYPE GameState
  map as MapModeState
  wonbattle as integer               'Indicates the status of the last battle, 1 for victory 0 for running away
  remembermusic as integer           'Remembers music from before battle starts
  random_battle_countdown as integer
  stock(99, 49) as integer           'Keeps track of available inventory at each shop
  foe_freq(254) as integer           'A cache of the fight frequency for each formation set
  walk_through_walls as integer      'used for F11 cheat mode
  mouse_enabled as integer           'initmouse called
  hero(40) as HeroState
  debug_showtags as integer
  debug_npc_info as integer
  autorungame as integer
  need_fade_in as integer
  fade_in_delay as integer
  current_master_palette as integer  'Modified by "load palette" command
  showtext as string                 'Used for map names and other alerts
  showtext_ticks as integer          'Remaining number of ticks
  getinputtext_enabled as integer
  script_log as ScriptLoggingState
END TYPE

TYPE TextBoxState
  id             as integer 'ID Number of the current box or -1 for none
  box            as TextBox '--Contains the data about the content of the textbox
  showing        as integer 'YES or NO
  fully_shown    as integer 'YES or NO. All lines have been displayed, player is allowed to advance
  choice_cursor  as integer
  remember_music as integer
  show_lines     as integer 'Number of lines currently on display
  sayer          as integer 'The NPC reference who triggered this textbox as a positive number, or -1 for none
  old_dir        as integer 'For NPCs that return to their previos direction after speaking
  portrait       as GraphicPair
  sl             as Slice Ptr
END TYPE

TYPE EquippableList
  count as integer
  offset(inventoryMax) as integer 'Index into the inventory, or -1 for nothing
END TYPE

TYPE EquipMenuState
  mode                as integer '0=picking slot 1=picking equipment to put in slot
  who                 as integer
  slot                as integer
  eq_cursor           as MenuState
  default_weapon      as integer 'item ID + 1
  default_weapon_name as string
  unequip_caption     as string
  eq(4)               as EquippableList
  stat_bonus(11)      as integer 'Cache stat bonuses for currently selected equippable
END TYPE

TYPE ItemsMenuState
  cursor as integer
  sel    as integer
  top    as integer
  info   as string
  re_use as integer
  trigger_box as integer
  quit   as integer
  scroll as MenuState
  scrollrect as RectType
  refresh as integer
  page as integer
  rect as RectType
  tog as integer
  special(-3 TO 0) as string 'upper bound should be -1, had to change due to FB bug #2898546
END TYPE

TYPE SpellsMenuSlot
  id      as integer    'attack id (or -1 for none)
  name    as string
  desc    as string
  cost    as string
  can_use as integer
  targt   as integer
  tstat   as integer
END TYPE

TYPE SpellsMenuList
  name       as string
  menu_index as integer 'maps to index in spells() global
  magic_type as integer
END TYPE

TYPE SpellsMenuState
  hero      as integer
  listnum   as integer
  last      as integer 'last occupied slot in .lists()
  lists(4)  as SpellsMenuList
  spell(24) as SpellsMenuSlot
  quit      as integer
  cursor    as integer
  mset      as integer
  re_use    as integer
  page      as integer
  tog       as integer
  cancel_menu_caption as string
  has_none_caption as string
END TYPE

TYPE LumpReloadState
  mode      as integer  'one of the loadmode constants in const.bi
  dirty     as integer  'whether a script has modified this for the current map
  changed   as integer  'whether modified by Custom and not reloaded
  hash      as integer  'used to store file_hash of last version loaded
END TYPE

TYPE LumpReloadOptions
  gmap     as LumpReloadState  '.hash not (can't) be used
  maptiles as LumpReloadState
  passmap  as LumpReloadState
  foemap   as LumpReloadState  '.dirty ignored: can't be modified
  zonemap  as LumpReloadState
  npcl     as LumpReloadState  '.dirty ignored: nearly always dirty
  npcd     as LumpReloadState

  hsp      as LumpReloadState  '.hash, .mode, .dirty ignored
END TYPE

#ENDIF
