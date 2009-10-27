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

TYPE GameState
  map AS MapModeState
  wonbattle AS INTEGER 'Indicates the status of the last battle, 1 for victory 0 for running away
  remembermusic AS INTEGER 'Remembers music from before battle starts
  random_battle_countdown AS INTEGER
  stock(99, 49) AS INTEGER 'Keeps track of available inventory at each shop
  foe_freq(254) AS INTEGER 'A cache of the fight frequency for each formation set
  walk_through_walls AS INTEGER 'used for F11 cheat mode
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


#ENDIF
