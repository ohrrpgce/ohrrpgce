'OHRRPGCE GAME - UDTs for game state (not game data)
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'
'This file contains UDTs that only get used in game mode, and not in custom,
'so as to prevent them from cluttering up the global udts.bi file

#IFNDEF GAME_UDTS_BI
#DEFINE GAME_UDTS_BI

#INCLUDE "slices.bi"
#INCLUDE "const.bi"

TYPE SliceTableType
  root as Slice Ptr
  maproot as Slice Ptr
  maplayer(maplayerMax) as Slice Ptr
  obsoleteoverhead as Slice Ptr
  MapOverlay as Slice Ptr
  Backdrop as Slice Ptr
  Walkabout as Slice Ptr
  HeroLayer as Slice Ptr
  NPCLayer as Slice Ptr
  scriptsprite as Slice Ptr
  textbox as Slice Ptr
  menu as Slice Ptr
  scriptstring as Slice Ptr
  reserve as Slice Ptr
END TYPE

TYPE MapModeState
  id       as integer
  lastmap  as integer 'ID of the last loaded map
  same     as bool    'flag that indicates when you are moving through a same-map door
  name     as string
  door(maxDoorsPerMap) as Door
END TYPE

TYPE HeroStats
  base as Stats  'Without equipment, caps, (or in future buffs) applied
  cur as Stats
  max as Stats
END TYPE

TYPE EquipSlot
  id as integer = -1    'Item ID, or -1 if nothing equipped
END TYPE

TYPE HeroState
 id as integer = -1 ' set to -1 for empty slots
 name as string
 locked as bool
 stat as HeroStats
 spells(3, 23) as integer   '4 spell lists: 0 for none, or attack ID + 1
 levelmp(maxMPLevel) as integer 'FF1-style level MP
 equip(4) as EquipSlot      'Equipment
 lev as integer
 lev_gain as integer        'Number of levels gained in the last battle or XP-giving script command
 learnmask((4 * 24 - 1) \ 16) as integer  'Spells learnt in the last battle: 2D array of bits
                                          '(Not affected by script commands)
 exp_cur as integer
 exp_next as integer
 battle_pic as integer
 battle_pal as integer
 exp_mult as double
 def_wep as integer         'default weapon; item ID + 1, 0 is meaningless?
 sl as Slice ptr            'Also copied into herow()
 pic as integer
 pal as integer
 portrait_pic as integer
 portrait_pal as integer
 rename_on_status as bool     'Renameable in status menu. Not saved.
 elementals(maxElements - 1) as single
 hand_pos(1) as XYPair
 hand_pos_overridden as bool  'YES if hand_pos has been set by a script, so should be saved and not reloaded
END TYPE

TYPE HeroWalkabout
  party_slot as integer  'The gam.hero() index of this hero, or -1 if empty
  sl as Slice ptr        'Alias to gam.hero(.party_slot).sl, or NULL. Even if sl is parented
                         'to the Reserve layer, it isn't NULL.
  UNION
    TYPE
      xgo as integer
      ygo as integer
    END TYPE
    xygo as XYPair
  END UNION
  wtog as integer
  speed as integer
  curzones as integer vector = 0

  'x, y, z, dir are not located here! They are in the cats() array of type CaterpillarHistory
  'See the accessor functions herox() heroy() heroz() and herodir()

  Declare Destructor()  'Free curzones
END TYPE

TYPE HeroSliceContext EXTENDS SliceContext
  DECLARE VIRTUAL FUNCTION description() as string
  slot as integer
END TYPE

TYPE NPCSliceContext EXTENDS SliceContext
  DECLARE VIRTUAL FUNCTION description() as string
  npcindex as NPCIndex
END TYPE

TYPE ScriptLoggingState
  enabled as bool
  filename as string
  tick as integer                    'Number of times interpret has been called
  last_logged as integer             'scrat index
  output_flag as bool                'Set to true after script_log_out called
  last_wait_msg as string
  wait_msg_repeats as integer        'Number of ticks in a row with identical last_wait_msg
  last_script_childtime as double    'Seconds of child run time by the last watched script to finish
END TYPE

'Set by script commands (or other triggers) to signal that they want something to be done after leaving the
'script interpreter (used to be called wantdoor, etc).
'These actions are delayed for historical/backcompat reasons; see interpret().
TYPE ScriptWantSignals
  box as integer      'Textbox, or 0 for none (can't ask to show box 0)
  door as integer     'Door number +1
  door_fadescreen as bool 'When a door is triggered: whether to fade the screen
  battle as integer   'Formation number +1
  teleport as bool    'Map num has changed, call preparemap()
  usenpc as integer   'NPC instance +1
  rungame as string   'Path to game to load, or ""
  loadgame as integer 'Save slot +1. Must be a used (valid) save slot.
  loadgame_prefix as string 'Used to distinguish quickload slot. Normally ""
  resetgame as bool   'Called "reset game"
  dont_quit_to_loadmenu as bool 'If no title screen, don't quit to the load menu.

  script_args(any) as integer  'Arguments to newgame/loadgame
END TYPE

'Used when automatically updating the visibility of the virtual gamepad
'(Android only)
TYPE VirtualGamePadState
  in_battle as bool = NO
  script_hide_virtual_gamepad as bool
  script_show_virtual_gamepad as bool
  script_wants_keys as integer = 0
  being_shown as bool
END TYPE

ENUM HeroPathingMode
 NONE
 NPC
 POS
END ENUM

TYPE HeroPathing
  mode as HeroPathingMode 'Set if pathfinding is happening
  dest_pos as XYPair 'Used when mode = HeroPathingMode.POS
  dest_npc as NPCIndex 'Target NPC used when mode = HeroPathingMode.NPC
  on_map as integer ' the map ID where the pathing was started
  stop_after_stillticks as bool 'Cancel pathing if stuck for this many ticks
  stop_when_npc_reached as bool 'Cancel pathing as soon as the hero catches up the the NPC it is following
  'The following are normally only used for the leader's built-in pathfinding
  by_user as bool'  set to YES for built-in pathfinding, NO for scripted
  queued_menu as bool ' Set to YES when a menu opening keypress has been queued
  dest_display_sl as Slice Ptr
END TYPE

'Holds part of a STF record, enough to tell whether the record has been edited significantly
TYPE OriginalStock
  thingtype as integer = -1          '-1 if uninitialised, 0 for items, 1 for heroes
  thingid as integer                 'ID of the item or hero
  stock as integer                   'Same values as STF 'In Stock' field!: -1 for infinite, >= 0 is finite amount
                                     '(Note this differs from encoding of gam.stock())
END TYPE

TYPE GameState
  map as MapModeState
  wonbattle as bool                  'Indicates the status of the last battle (won as opposed to dying or running or 'force exit')
  remembermusic as integer           'Remembers music from before battle starts
  music_change_delay as integer      'Number of ticks to wait before changing music; <= 0 if no change pending
  delayed_music as integer           'Song number to change to, -1 to stop music
  random_battle_countdown as integer
  stock(any, any) as integer         'Keeps track of available inventory at each shop (shop, stockidx)
                                     'must REDIM gam.stock(gen(genMaxShop), 49) after gen() is loaded.
                                     'Each entry is either -1 (infinite stock), 0 (not loaded; will be loaded
                                     'when the shop is visited unless STF is too short)
                                     'or remainingstock+1 if > 0
  original_stock(any, any) as OriginalStock 'Saves info about the STF entry at the time that an element of stock()
                                     'is initialised, so we can tell if the STF entry has been edited since and
                                     'the stock() element should be reset.
  foe_freq(254) as integer           'A cache of the fight frequency for each formation set
  walk_through_walls as bool         'used for F11 cheat mode
  mouse_enabled as bool              'Mouse clicks trigger onkeypress script (initmouse called/mouse movement on)
  click_keys as bool                 'Mouse clicks count for anykey, usekey, etc (cache of /mouse/click_keys)
  hero(sizeParty - 1) as HeroState
  debug_npc_info as integer          'NPC debug display. 0: off, 1: IDs/refs, 2: also show obstructions
  debug_textbox_info as bool         'Textbox debug display
  debug_camera_pan as bool           'Arrow keys pan the camera, and camera mode is ignored
  debug_disable_foemap as bool       'Disable random battles

  'The following are mutually exclusive
  debug_scripts as integer           '0: off, 1: show running scripts, 2: pause and enter debugger
  debug_showtags as integer          '0: off, 1: small display, 2: full-length display

  paused as bool                     'Pause the game (only in map mode; this is a debug key)
  autorungame as bool                'Game was autorun, not selected from RPG browser
  return_to_browser as bool          'Return to browser when quitting the game
  ingame as bool                     'Have loaded a game and done initialisations. True from titlescreen onwards
  started_by_run_game as bool        'True if this game was started with "run game"
  shared_fullscreen_setting as bool  'If true, preserve current fullscreen state when loading a game.
  fullscreen_config_file as string   '.ini file in which to save "gfx.fullscreen". Usually game_config_file
  need_fade_in as bool
  fade_in_delay as integer
  fade_in_script_overridable as bool 'If true, the fade in can be cancelled by a fadescreenout command
  need_fade_page as bool             'Use fadetopage instead of setvispage; used in 32-bit mode to fade between master palettes
  current_master_palette as integer  'gen(genMasterPal) or last call to "load palette" command
  showtext as string                 'Used for map names and other alerts
  showtext_ticks as integer          'Remaining number of ticks
  showstring as string               'The text shown by showvalue/showstring commands
  getinputtext_enabled as bool
  timer_offset as double             'Amount to shift TIMER by, used only by the milliseconds command
  script_log as ScriptLoggingState
  want as ScriptWantSignals
  quit as bool                       'Quit back to titlescreen
  pad as VirtualGamePadState
  non_elemental_elements(maxElements - 1) as bool 'Loaded by load_non_elemental_elements()
  hero_pathing(3) as HeroPathing
  stillticks(3) as integer           'keeps track of how long a hero has been standing still
  pathing_click_start as double
END TYPE

'Note that .showing, .fully_shown, .sayer need to be always correct even if no box is up
TYPE TextBoxState
  id             as integer = -1 'ID Number of the current box or -1 for none
  box            as TextBox 'Contains the data about the content of the textbox
  showing        as bool
  fully_shown    as bool    'All lines have been displayed, player is allowed to advance. False if no textbox
  choicestate    as MenuState 'Choicebox menu control
  remember_music as integer
  show_lines     as integer 'Number of lines currently on display
  sayer          as NPCIndex = -1 'The NPC instance that triggered this textbox if >= 0, or -1 for none
  old_dir        as integer 'For NPCs that return to their previos direction after speaking
  sl             as Slice Ptr

  Declare Destructor()
END TYPE

TYPE EquippableList
  count as integer
  offset(inventoryMax) as integer 'Index into the inventory, or -1 for nothing
END TYPE

TYPE EquipMenuState
  mode                as integer '0=picking slot 1=picking equipment to put in slot
  quit                as bool
  allow_switch        as bool
  menu(6)             as string
  who                 as integer
  slot                as integer
  eq_cursor           as MenuState
  default_weapon_name as string
  unequip_caption     as string
  eq(4)               as EquippableList
  stat_bonus(11)      as integer 'Cache stat bonuses for currently selected equippable
  stat_total_bonus(11) as integer 'Total stat bonuses for all hero equip after equipping selected equippable
  hero                as HeroDef
  item_info           as string
END TYPE

TYPE SpellsMenuSlot
  id      as integer    'attack id (or -1 for none)
  name    as string
  desc    as string
  cost    as string
  can_use as bool       'Useable OOB
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
  quit      as bool
  cursor    as integer
  mset      as integer
  re_use    as bool
  page      as integer
  tog       as bool
  cancel_menu_caption as string
  has_none_caption as string
END TYPE

TYPE ShopBuyState
  shop_id        as integer
  shoptype       as integer
  stuff_list     as NodePtr
  selected       as NodePtr
  curslot        as integer
  root_sl        as Slice Ptr
  money_sl       as Slice Ptr
  info_sl        as Slice Ptr
  hero_box       as Slice Ptr
  party_box(3)    as Slice Ptr
  party_sl(3)    as Slice Ptr
  price_box      as Slice Ptr
  price_sl       as Slice Ptr
  alert_box      as Slice Ptr
  alert_sl       as Slice Ptr
  alert_ticks    as integer
  hire_box       as Slice Ptr
  hire_sl        as Slice Ptr
  portrait_box   as Slice Ptr
  portrait_sl    as Slice Ptr
  tog            as integer
  walk           as integer
  info           as MenuDef
  info_st        as MenuState
END TYPE

TYPE OrderTeamState
  party as MenuState
  reserve as MenuState
  info as string
  swapme as integer
  charsize as XYPair 'not in pixels
  swindex(40) as integer
  swname(40) as string
  show_reserve as bool
  page as integer
  do_pick as bool
  do_quit as bool
END TYPE

TYPE LumpReloadState
  mode      as integer  'one of the loadmode constants in const.bi
  dirty     as bool     'whether a script has modified this (referring to current map, if applicable)
  changed   as bool     'whether modified by Custom and not reloaded
  hash      as ulongint 'used to store file_hash64 of last version loaded
END TYPE

TYPE LumpReloadOptions
  gmap     as LumpReloadState  '.hash not (can't) be used
  maptiles as LumpReloadState
  passmap  as LumpReloadState
  foemap   as LumpReloadState  '.dirty ignored: can't be modified
  zonemap  as LumpReloadState
  npcl     as LumpReloadState  '.dirty ignored: nearly always dirty
  npcd     as LumpReloadState

  hsp      as LumpReloadState  '.hash, .dirty ignored
END TYPE

TYPE VirtualKeyboardScreenState
 sl as Slice Ptr
 result as string
 max_length as integer = -1
 prompt as string
 done as bool = NO
 entry_sl as Slice Ptr
 select_sl as Slice Ptr
 shift as bool
 symbols as bool
 arr as any ptr vector
 pushed_sl as Slice Ptr
 drag_off as bool
END TYPE

TYPE DebugMenuDef
 menu as string vector    ' List of menu item names. NULL if we're checking key combos instead
 selected_item as string  ' If this is set, find the menu item with this name instead of building the menu.
 Declare Sub start_building_menu()
 Declare Destructor()
 Declare Function def(combining_scancode as integer = 0, scancode as integer = 0, menuitem as zstring ptr = @"") as bool
END TYPE

TYPE CaterpillarHistory
 UNION
  TYPE
   x as integer
   y as integer
  END TYPE
  pos as XYPair
 END UNION
 z as integer
 d as DirNum
END TYPE


#ENDIF
