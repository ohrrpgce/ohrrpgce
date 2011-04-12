#IFNDEF GAME_UDTS_BI
#DEFINE GAME_UDTS_BI

#INCLUDE "slices.bi"

'This file contains UDTs that only get used in game mode, and not in custom,
'so as to prevent them from cluttering up the global udts.bi file

TYPE MapModeState
  id       AS INTEGER
  lastmap  AS INTEGER 'ID of the last loaded map
  same     AS INTEGER 'YES/NO flag that indicates when your are moving through a same-map door
  showname AS INTEGER
  name     AS STRING
  door(99) AS Door
  doorlinks(199) AS doorlink
END TYPE

UNION HeroStatsSingle
  'See also BattleStatsSingle '-- the two of these can probably be unified eventually
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
  END TYPE
  sta(11) AS INTEGER
End UNION

TYPE HeroStats
  cur AS HeroStatsSingle
  max AS HeroStatsSingle
END TYPE

TYPE HeroState
 stat AS HeroStats
 lev AS INTEGER
 lev_gain AS INTEGER
 wep_pic AS INTEGER
 wep_pal AS INTEGER
 battle_pic AS INTEGER
 battle_pal AS INTEGER
 def_wep AS INTEGER '-default weapon
 pic AS INTEGER
 pal AS INTEGER
 rename_on_status AS INTEGER 'Renameable in status menu
 elementals(maxElements - 1) AS SINGLE
END TYPE

TYPE GameState
  map AS MapModeState
  wonbattle AS INTEGER 'Indicates the status of the last battle, 1 for victory 0 for running away
  remembermusic AS INTEGER 'Remembers music from before battle starts
  random_battle_countdown AS INTEGER
  stock(99, 49) AS INTEGER 'Keeps track of available inventory at each shop
  foe_freq(254) AS INTEGER 'A cache of the fight frequency for each formation set
  walk_through_walls AS INTEGER 'used for F11 cheat mode
  mouse_enabled AS INTEGER 'initmouse called
  hero(40) AS HeroState
END TYPE

TYPE TextBoxState
  id             AS INTEGER 'ID Number of the current box or -1 for none
  box            AS TextBox '--Contains the data about the content of the textbox
  showing        AS INTEGER 'YES or NO
  fully_shown    AS INTEGER 'YES or NO. All lines have been displayed, player is allowed to advance
  choice_cursor  AS INTEGER
  remember_music AS INTEGER
  show_lines     AS INTEGER 'Number of lines currently on display
  sayer          AS INTEGER 'The NPC reference who triggered this textbox as a positive number, or -1 for none
  old_dir        AS INTEGER 'For NPCs that return to their previos direction after speaking
  portrait       AS GraphicPair
  sl             AS Slice Ptr
END TYPE

TYPE EquippableList
  count AS INTEGER
  offset(inventoryMax) AS INTEGER 'Index into the inventory, or -1 for nothing
END TYPE

TYPE EquipMenuState
  mode                AS INTEGER '0=picking slot 1=picking equipment to put in slot
  who                 AS INTEGER
  slot                AS INTEGER
  eq_cursor           AS MenuState
  default_weapon      AS INTEGER 'item ID + 1
  default_weapon_name AS STRING
  unequip_caption     AS STRING
  eq(4)               AS EquippableList
  stat_bonus(11)      AS INTEGER 'Cache stat bonuses for currently selected equippable
END TYPE

TYPE ItemsMenuState
  cursor AS INTEGER
  sel    AS INTEGER
  top    AS INTEGER
  info   AS STRING
  re_use AS INTEGER
  trigger_box AS INTEGER
  quit   AS INTEGER
  scroll AS MenuState
  scrollrect AS RectType
  refresh AS INTEGER
  page AS INTEGER
  rect AS RectType
  tog AS INTEGER
  special(-3 TO 0) AS STRING 'upper bound should be -1, had to change due to FB bug #2898546
END TYPE

TYPE SpellsMenuSlot
  id      AS INTEGER    'attack id (or -1 for none)
  name    AS STRING
  desc    AS STRING
  cost    AS STRING
  can_use AS INTEGER
  targt   AS INTEGER
  tstat   AS INTEGER
END TYPE

TYPE SpellsMenuList
  name       AS STRING
  menu_index AS INTEGER 'maps to index in spells() global
  magic_type AS INTEGER
END TYPE

TYPE SpellsMenuState
  hero      AS INTEGER
  listnum   AS INTEGER
  last      AS INTEGER 'last occupied slot in .lists()
  lists(4)  AS SpellsMenuList
  spell(24) AS SpellsMenuSlot
  quit      AS INTEGER
  cursor    AS INTEGER
  mset      AS INTEGER
  re_use    AS INTEGER
  page      AS INTEGER
  tog       AS INTEGER
  cancel_menu_caption AS STRING
  has_none_caption AS STRING
END TYPE

#ENDIF
