#IFNDEF UDTS_BI
#DEFINE UDTS_BI

#INCLUDE "const.bi"
#INCLUDE "util.bi"
#INCLUDE "reload.bi"
#INCLUDE "allmodex.bi"

'Forward declarations
TYPE SliceFwd as Slice

ENUM 'CompType
  compNone
  compEq  ' =
  compNe  ' <>
  compLt  ' <
  compLe  ' <=
  compGt  ' >
  compGe  ' >=
  compTag '
  compLAST = compTag
END ENUM
TYPE CompType as integer

'A check on the value of a tag or global variable. When type == compTag, then
'.tag is positive to check that the tag is on, and negative to check it is off,
'and .tag = 0 is equivalent to type == compNone.
'The meaning of compNone is context specific: it might be either Always or Never.
TYPE Condition
  UNION
    varnum as integer  ' global
    tag as integer
  END UNION
  comp as CompType
  value as integer  'Not used when type == compTag
  'The following only used only in Custom and never saved
  editstate as ubyte
  lastinput as ubyte
END TYPE

TYPE MenuSet
  menufile as string
  itemfile as string
END TYPE

TYPE BasicMenuItem
  text as string    'This is the caption actually displayed (unlike MenuDefItem.caption)
  'In MenuDefItems the following aren't saved.
  col as integer    'Text color. 0=use default, >0 is color index, <0 is a UI color
                    'Has no effect when the menu item is selected and flashing
  disabled_col as integer  'Text color if disabled. Meaning of values same as 'col'.
  bgcol as integer  'Text bg color. Only supported by standardmenu, not draw_menu.
  'In MenuDefItems the following aren't saved. They are set at run-time (eg. in update_menu_items)
  unselectable as bool 'Menu cursor skips over this item
  disabled as bool  'Appear greyed out and disable activation
                    '(For in-game user menus, set based on .tag1/.tag2 and type/subtype)
END TYPE

DECLARE_VECTOR_OF_TYPE(BasicMenuItem, BasicMenuItem)

MAKETYPE_DoubleList(MenuDefItem)
MAKETYPE_DListItem(MenuDefItem)

TYPE MenuDefItem EXTENDS BasicMenuItem
  handle    as integer
  caption   as string  ' This is the caption as set in the menu editor/set menu item caption
  trueorder as DListItem(MenuDefItem) ' contains next, prev
  t         as integer ' Item type; of type MenuItemType in-game, except in battles. You're free to assign own meaning.
  sub_t     as integer ' Sub-type. Free to assign own meaning. See const.bi.
  tag1      as integer
  tag2      as integer
  settag    as integer
  togtag    as integer
  extra(2)  as integer
  hide_if_disabled  as bool
  close_when_activated as bool
  skip_close_script as bool
  dataptr   as any ptr  'Use this with caution!

  'Not hidden. (Doesn't say whether currently on-screen!)
  DECLARE FUNCTION visible() as bool
END TYPE

DECLARE_VECTOR_OF_TYPE(MenuDefItem, MenuDefItem)

TYPE MenuDef
  record    as integer = -1
  handle    as integer
  name      as string
  boxstyle  as integer
  textcolor as integer  'Default. 0=use uiMenuItem, >0 is color index, <0 is a UI color
  disabled_textcolor as integer  'Default. 0=use uiDisabledItem, >0 is color index, <0 is a UI color
  maxrows   as integer
  edit_mode as bool   'Never hide disabled items, allow selection of unselectable items
  items     as MenuDefItem Ptr Ptr
  'adds first, last, numitems, itemlist members
  INHERITAS_DoubleList(MenuDefItem, itemlist) 'True order of menu items, ignoring sort-invisible-items-to-end
  translucent      as bool ' Bitset 0
  no_scrollbar     as bool ' Bitset 1
  allow_gameplay   as bool ' Bitset 2
  suspend_player   as bool ' Bitset 3
  no_box           as bool ' Bitset 4
  no_close         as bool ' Bitset 5
  no_controls      as bool ' Bitset 6
  prevent_main_menu as bool ' Bitset 7
  advance_textbox  as bool ' Bitset 8
  highlight_selection as bool 'Bitset 9: Draw a rectangle behind the item while selected
  remember_selection as bool 'Bitset 10: .pt is remember when closing
  rect      as RectType
  offset    as XYPair
  anchorhoriz as AlignType = alignCenter  'Relative to self
  anchorvert as AlignType = alignCenter   'Relative to self
  alignhoriz as AlignType = alignCenter   'Relative to screen. NOT saved or exposed to users
  alignvert as AlignType = alignCenter    'Relative to screen. NOT saved or exposed to users
  textalign as AlignType = alignCenter    'Text alignment
  withtags as bool          'Enable text markup. NOT saved or exposed to users

  min_chars as integer
  max_chars as integer
  bordersize as integer  'Pixels added to default border padding (which is 8).
  itemspacing as integer 'pixels added to the default item spacing.
                          'negative shrinks, positive grows. This
                          'alters the vertical size of the whole menu!
  on_close  as integer   'Script trigger
  esc_menu  as integer   'Cancel button close action: 0=just close, >0 is menu ID + 1 to open in-place
  age as integer         'This is incremented in draw_menu and should normally be the age of the menu in ticks

  Declare Constructor()
  Declare Destructor()
END TYPE

TYPE MenuState
  active    as bool = YES
  pt        as integer 'currently selected item (.first - 1 if none, eg. menu is empty)
  hover     as integer 'item mouse is hovering over, or .first - 1 if none. Set even if .active=NO
  top       as integer 'scroll position for long lists
  first     as integer 'first element (usually zero)
  last      as integer 'last element (.first - 1 if menu is empty)
  size      as integer 'number of elements to display at a time - 1
  need_update as bool  'menu needs some kind of update
  tog       as bool    'For flashing cursor
  autosize as bool = NO 'Set this to true, and usemenu will
                                 'auto-upate the menu size to fill the
                                 'screen vertically
  autosize_ignore_pixels as integer = 0 ' Often you only want
                                 ' *most* of the screen to be filled
  autosize_ignore_lines as integer = 0
  '--These are populated when the menu is drawn by standardmenu. They should and can
  '--only be set manually when standardmenu isn't used.
  has_been_drawn as bool
  rect as RectType
  spacing as integer    'Height of each line, in pixels. Set MenuOptions.itemspacing to adjust.
  select_by_mouse_release as bool ' This is important for menus where you need to be able to
                                  ' focus an item and pick an item with separate clicks
  drag_start_top as integer 'used internally bu mouse_drag_menu()

  DECLARE FUNCTION empty() as bool
  DECLARE FUNCTION pt_valid() as bool
  DECLARE FUNCTION would_have_scrollbar() as bool
END TYPE

'A set of rendering options which can be passed to standardmenu.
'This partially overlaps MenuDef, which is the reason it wasn't added to MenuState:
'MenuState is used for drawing MenuDefs too, which would be confusing
TYPE MenuOptions
  edged as bool
  highlight as bool       'Display a uiHighlight-colored rectangle behind the selected item
  bgfuzz as bool          'Draw a fuzzyrect behind the text of each menu item
  normal_col as integer   'Default color/UIcol for items without .col. Defaults to uilook(uiMenuItem)
  disabled_col as integer 'Default color/UIcol for items without .disabled_col. Defaults to uilook(uiDisabledItem)

  calc_size as bool       'Set rect according to widest menu item and num items, otherwise
                          'stretches to right & bottom screen edges (which might be smaller!)
  wide as integer = 9999  'Width in pixels (but doesn't extend past screen edge).
                          'If calc_width is true, then this is the min width
                          '(so you MUST override the default of 9999!!)
  itemspacing as integer  'Pixels added to the default item spacing.
                          'negative shrinks, positive grows. This
                          'alters the vertical size of the whole menu!
  showright as bool       'Always show the right-most portion of text if too long,
                          'instead of only doing so when selected.
  nevershowright as bool  'Don't show right-most portion, even if selected
  scrollbar as bool       'Draw a scrollbar (boxstyle 0); position given by x, y, .wide and MenuState.size
  fullscreen_scrollbar as bool 'Draw a scrollbar at the right edge of the screen
END TYPE

'For when a string array is too crude, but a MenuDef is overkill
TYPE SimpleMenuItem EXTENDS BasicMenuItem
  dat as integer  'For your own use
END TYPE

DECLARE_VECTOR_OF_TYPE(SimpleMenuItem, SimpleMenuItem)

'This type abstracts menu data (either a string array or a BasicMenuItem-derived vector)
'and a search method for select_by_typing.
'(The menu length must not change during the lifetime of this object!)
TYPE MenuSearcher
 'Get the text of a menu item
 'getfunc as function(this as MenuSearcher, byval index as integer) as string
 'Search text for a query and return position in the text of a match, or 0 for no match
 findfunc as function(this as MenuSearcher, itemtext as string, query as string) as integer

 'Exactly one of the following is non-null
 menu_array as string ptr
 menu_vector as BasicMenuItem vector

 excludeword as string      'used only by MenuSearcher_find_word_boundary

 declare Constructor(menu() as string)
 declare Constructor(menu_vector as BasicMenuItem vector)
 declare function text(byval index as integer) as string
 declare function selectable(byval index as integer) as bool
END TYPE

TYPE SelectTypeState
 query as string              'String to search for
 buffer as string             'Text the user has entered
 last_input_time as double    'TIMER at last input

 'Used by highlight_menu_typing_selection
 query_at as integer          'Offset in text of current menu item of match, or 0
 highlight_at as integer      'Offset of match/no match text highlighting. Remembers last non-zero value of query_at.
 'highlight_pt as integer      'Menu index (line) where the highlighting is. Is different from state.pt only if unselectable
 remember_pt as integer       'Remember last state.pt value
END TYPE

CONST maxNPCDataField = 18  'Highest valid argument to Get/SetNPCD and read/alterNPC commands
'Because Get/SetNPCD are used to load/save .N lumps, this will change when binN changes

ENUM PathfindingObstructionMode
  obmodeDefault = 0
  obmodeNPCsObstruct = 1
  obmodeNPCsIgnored = 2
  obmodeLAST = 2
END ENUM

'Value passable to any NPC script command:
'Either an NPC ID (>= 0) or an NPC reference (< 0). No 'invalid' value.
TYPE NPCScriptref as integer

'Index in npcs() NPCType array, -1 for none/invalid.
TYPE NPCTypeID as integer

'Index in npc() NPCInst array, -1 for none/invalid.
TYPE NPCIndex as integer

'Warning: when editing NPCType, update Get/SetNPCD,
'readnpc, alternpc, plotscr.hsd constants, and plotdict.xml
'Note that instances of this type are copied in edit_npc (w/ default copy constructor)
TYPE NPCType
  picture as integer     '+0
  palette as integer = -1 '+1
  movetype as integer    '+2
  speed as integer = 4   '+3  real speed, not value in .d
  textbox as integer     '+4
  facetype as integer    '+5
  item as integer        '+6
  pushtype as integer    '+7
  activation as integer  '+8
  tag1 as integer        '+9   appear only if
  tag2 as integer        '+10  appear only if 2
  usetag as integer      '+11  onetime use bit or 0 for none
  script as integer      '+12  script trigger
  scriptarg as integer   '+13  argument to the script
  vehicle as integer     '+14
  defaultzone as integer '+15
  defaultwallzone as integer '+16
  ignore_passmap as integer '+17  (0 or 1) Can walk through walls (but not zones or map edges)
  pathfinding_obstruction_mode as PathfindingObstructionMode '+18 (only matters if .movetype=15)
END TYPE

ENUM NPCOverrideMove
 NONE
 NPC
 POS
END ENUM

TYPE PathfinderOverride
  override as NPCOverrideMove 'Set if normal movement has been overridden by pathfinding

  cooldown as integer 'Ticks; used to prevent stuck pathfinding NPCs from re-pathing too fast

  dest_pos as XYPair 'Used when pathfinding overrides normal movement (only used for NPCOverrideMove.POS)
  dest_npc as NPCIndex 'Target NPC used when pathfinding overrides normal movement (only used for NPCOverrideMove.NPC)
  stop_when_npc_reached as bool
  stop_after_stillticks as integer
END TYPE

'Don't forget to update CleanNPCL and load/save_npc_instances when adding to this
TYPE NPCInst
  sl as SliceFwd Ptr
  '--stored values
  UNION
    TYPE
      x as integer  'npcl+0
      y as integer  'npcl+300
    END TYPE
    pos as XYPair
  END UNION
  z as integer      '            Does not include foot offset
  id as integer     'npcl+600    0 if unused, ID + 1 for normal NPCs, -ID - 1 for hidden NPCs
                    '  (So NOT an NPCTypeID)
                    '  NOTE: NPCs with invalidly high ID numbers (on incompletely loaded maps)
                    '        are automatically hidden (and obviously unsafe to unhide)
  UNION
    TYPE
      xgo as integer'npcl+1500   warning: positive to go LEFT, negative RIGHT
      ygo as integer'npcl+1800   warning: positive to go UP, negative to go RIGHT
                    ' These backwards-seeming values are an artifact of a time 20 years ago
                    ' when James typed - when he should have typed +
    END TYPE
    xygo as XYPair
  END UNION
  dir as DirNum     'npcl+900
  frame as integer  'npcl+1200   0-3: ticks in walk animation loop (2 ticks per frame)
  extra(2) as integer
  ignore_walls as bool  'ignore passmap, zones and map edges (but not NPC/hero obstructions)
  not_obstruction as bool 'can walk through npcs+heroes and vice-versa
  suspend_use as bool   'not activatable 
  suspend_ai as bool    'ignore movetype (effectively 'stand still')

  '' The following aren't saved. (Should they be?)

  curzones as integer vector  'sorted vector of zones this NPC is currently inside

  stillticks as integer 'counts the number of ticks that an npc has spent holding still
  pathover as PathfinderOverride
  follow_walls_waiting as bool  '"Follow Walls Stop For Others" only: is stopped for an NPC/hero
END TYPE

TYPE InventSlot
  used as bool          'use this to check if empty, not num!

  'following fields should not be used if used = NO
  id as integer	        'read ID number starting at 0, not +1!!
  num as integer
  text as string        'text field which shows up in inventory, blank if empty
  sortorder as integer  'contains garbage nearly always: only used in items_menu_autosort
END TYPE

TYPE PlotTimer
  count as integer
  speed as integer
  ticks as integer
  trigger as integer
  flags as integer  '1=critical 2=battle 4=menu 8+=undefined
  st as integer     'string, but both str and string are reserved
  finished_tick as integer  'tick at which this last triggered; 0 if never
END TYPE

TYPE Plotstring
  s as string
  X as integer
  Y as integer
  col as integer    '0-255, or -1 to default to uiText
  bgcol as integer
  bits as integer   '1=visible, 2=edgeless text
END TYPE

Type HSHeader
  valid as bool
  hspeak_version as string    'Usually 3 characters, but minor version is missing for versions before 3H
  hsp_format as integer
  script_format as integer    'Same as the .HSZ format number, or 0 if not known
  max_function_id as integer  'Maximum ID of a used function, or 0 if not known
  plotscr_version as string   'empty if not known
End Type

TYPE TriggerData
  name as string
  id as integer
END TYPE

TYPE ScriptData
  'Script attributes
  id as integer         'id number of script  (set to 0 to mark as unused slot)
                        'The id number is negated if this is a stale unreloaded script and
                        'shouldn't be used for new scripts.
  hash as ulongint      'file_hash64 of the .hsx/.hsz file
  ptr as integer ptr    'pointer to script commands
  scrformat as integer  'hsz file format version number
  headerlen as integer  'header length in bytes
  size as integer       'size of script data, in 4 byte words (in-memory size, not on-disk size)
  vars as integer       'local variable (including arguments) count, not including nonlocals
  nonlocals as integer  'number of nonlocal variables (sum of locals in ancestor scripts)
  args as integer       'number of arguments
  strtable as integer   'pointer to string table (offset from start of script data in 4 byte ints)
  nestdepth as integer  'Number of scripts/subscripts this is nested in, 0 for scripts
  parent as integer     'ID of parent script or 0 if not a subscript

  'Book keeping
  trigger_type as string 'The type of the last trigger (eg "new game") of this script, or blank
                        '(A script is considered to be a fibre root if it has a non-blank trigger_type)
  refcount as integer   'number of ScriptInst pointing to this data
  lastuse as uinteger
  'For script profiling. The following are filled in and used only if SCRIPTPROFILE is defined.
  calls_in_stack as integer 'Number of times this script appears in the call chain for the current
                        'executing fibre. Needed to account child running time when recursing.
  numcalls as integer   'Total number of times this script has been started (since loading)
  laststart as double   'Timer when the first instance of this script in the currently executing
                        'fibre was started. If not used in the current fibre, is garbage.
                        'Used for tracking time spent in self and children.
  totaltime as double   'time spent in here, in seconds. If currently executing, subtract start time.
  childtime as double   'time spent in here and all descendents, in seconds
  entered as integer    'number of times entered (started/switched to)
  'End profiling.

  next as ScriptData ptr 'next in linked list, for hashtable
  backptr as ScriptData ptr ptr 'pointer to pointer pointing to this, in script(), or a .next pointer
                        'not your usual double linked list, because head of list is a script() element
END TYPE

'Used by the old interpreter
TYPE OldScriptFrame
  heap as integer       'position of start of the local vars in the heap() buffer for a script
END TYPE

'State of an executing script, used by the old interpreter
TYPE OldScriptState
  scr as ScriptData ptr 'script in script() hashtable (duplicated from ScriptInst)
  scrdata as integer ptr 'convenience pointer to .scr->ptr
  frames(maxScriptNesting) as OldScriptFrame   'frame(0) points to local variables, others to variables of ancestor scripts
  heapend as integer    'one-past-end of offsets on the heap used by this script (starts at frame(0).heap)
  stackbase as integer  'position where this script's stack data starts in scrst
  state as integer      'current interpreter statemachine state; negated if suspended by another fibre
  ptr as integer        'the execution pointer (in int32's from the start of the script data)
  ret as integer        'the scripts current return value
  curargn as integer    'current arg for current statement
  depth as integer      'stack depth of current script
  id as integer         'id number of current script (duplicated from ScriptInst)
END TYPE

ENUM WaitTypeEnum
  waitingOnNothing = 0  'The script isn't waiting (but might be suspended; check OldScriptState.state)
  waitingOnCmd          'A command in the script triggered a wait (ScriptInst.curvalue says which)
  waitingOnTick         'The script was externally made to wait; ScriptInst.waitarg gives the number of ticks
END ENUM

'Externally visible state of an executing script, used outside the interpreter
TYPE ScriptInst
  scr as ScriptData ptr 'script in script() hashtable
  waiting as WaitTypeEnum  'Whether the script is waiting
  waitarg as integer    'wait state argument 1
  waitarg2 as integer   'wait state argument 2
  watched as bool       'true for scripts which are being logged (fibre roots only)
  started as bool       'used only if watched is true: whether the script has started
  id as integer         'id number of script

  'these 3 items are only updated when the script interpreter is left. While inside
  'the script interpreter (command handlers) use the curcmd (ScriptCommand ptr) global.
  'These are also updated when a script is stopped (either suspended, or interpretloop is left).
  curkind as integer    'kind of current statement
  curvalue as integer   'value/id of current statement
  curargc as integer    'number of args for current statement  (used only in old script debugger...)
END TYPE

TYPE ScriptFibre

  id as integer         'Triggers pre-decoded
  scripttype as string
  trigger_loc as string 'More information about how it was triggered
  double_trigger_check as bool  'Whether to prevent double triggering
  log_line as string    'Debugging aid: Comprised from scripttype, arg names and values and trigger_loc
  argc as integer       'The number of args passed
  args(3) as integer
END TYPE

DECLARE_VECTOR_OF_TYPE(ScriptFibre ptr, ScriptFibre_ptr)

'Node of an .hsz script abstract syntax tree
TYPE ScriptCommand
  kind as integer
  value as integer
  argc as integer
  args(0) as integer
END TYPE

TYPE TilemapInfo
  UNION
    TYPE
      wide as integer
      high as integer
    END TYPE
    size as XYPair
  END UNION
  layers as integer

  err as string      'Error message if the file is bad, otherwise blank
  DECLARE SUB seterr(filename as string, errmsg as string)
END TYPE

ENUM MapEdgeModeEnum
  mapEdgeCrop = 0
  mapEdgeWrap = 1
  mapEdgeDefaultTile = 2
END ENUM

'WARNING: don't add strings to this
Type ZoneHashedSegment
  IDmap(14) as ushort
End Type

'Data associated with an individual zone, other than the tiles themselves
Type ZoneInfo
  id as integer
  name as string
  hidden as bool  'used in the map editor only, not saved
  numtiles as integer  'number of set tiles
  extra(2) as integer
End Type

'A set of zones
'See loading.rbas for documentation.
'*** Requires construction + destruction ***
Type ZoneMap
  numzones as integer
  zones as ZoneInfo ptr
  UNION
    TYPE
      wide as integer       'width in tiles
      high as integer       'height in tiles
    END TYPE
    size as XYPair
  END UNION
  wide_segments as integer  'width in 4x4 tile segments
  high_segments as integer  'height in 4x4 tile segments
  bitmap as ushort ptr      'array dimensions are [high][wide]
  zoneIDmap as ushort ptr   'array dimensions are [high_segments][wide_segments][16]
  extraID_hash as HashTable 'table of ZoneHashedSegments for overcrowded tiles, indexed by (x SHL 16) + y
End Type

Type Door
  pos as XYPair         ' In tiles
  exists as bool        ' False if this door ID isn't used
End Type

Type DoorLink
  source as integer
  dest as integer
  dest_map as integer
  tag1 as integer
  tag2 as integer
End Type

' All the data for a map. Currently only used in the map editor.
TYPE MapData
  id as integer
  name as string
  UNION
    TYPE
      wide as integer        'Map size in tiles
      high as integer
    END TYPE
    size as XYPair
  END UNION

  tiles(any) as TileMap
  pass as TileMap
  foemap as TileMap
  zmap as ZoneMap
  gmap(any) as integer
  door(maxDoorsPerMap) as Door
  doorlink(199) as DoorLink
  npc_def(any) as NPCType
  npc(299) as NPCInst

  Declare Constructor ()
  Declare Destructor ()

  Declare Sub load (map_id as integer)
  Declare Sub load_for_minimap(map_id as integer)
END TYPE

Union Stats
       Type
               hp as integer
               mp as integer
               str as integer
               acc as integer
               def as integer
               dog as integer
               mag as integer
               wil as integer
               spd as integer
               ctr as integer
               focus as integer
               hits as integer
       End Type
       sta(11) as integer
End Union

Enum AttackTagConditionEnum
	atktagOnUse = 1
	atktagOnHit = 2
	atktagOnMiss = 3
	atktagOnKill = 4
End Enum

Type AttackDataTag
	tag as integer
	condition as AttackTagConditionEnum
	tagcheck as integer
End Type

Type AttackDataItem
	id as integer 'ID + 1
	number as integer 'positive to consume, negative to acquire
End Type

Type AttackDataChain
	atk_id as integer 'ID+1, 0=None
	rate as integer
	mode as integer
	val1 as integer
	val2 as integer
	'--bitsets
	must_know as bool
	no_delay as bool
	nonblocking as bool
	dont_retarget as bool
	invert_condition as bool
End Type

Type AttackElementCondition
	comp as integer  'a CompType with extra bits set:
	                 '16: Invert condition, 32: Miss instead of fail,
	                 '64: Multiply threshold value by random(0.0, 1.0)
	value as single  'Threshold
End Type


Enum CounterProkeEnum
	provokeDefault = 0       ' Use genDefCounterProvoke
	provokeAlways = 1        ' Always trigger counter attacks
	provokeNever = 2         ' Never trigger counter attacks
	provokeHit = 3           ' Only if the attack hit (didn't miss or fail)
	provokeFail = 4          ' Only if the attack failed
	provokeMiss = 5          ' Only if the attack missed
	provokeDidntHit = 6      ' Only if the attack missed or failed
	provokeDidntFail = 7     ' Only if the attack didn't fail
	provokeDidntMiss = 8     ' Only if the attack didn't miss
	provokeLAST = 8
End Enum

Enum AttackAnimation
	atkAnimNormal = 0
	atkAnimProjectile = 1
	atkAnimReverseProjectile = 2
	atkAnimDrop = 3
	atkAnimRing = 4
	atkAnimWave = 5
	atkAnimScatter = 6
	atkAnimSequentialProjectile = 7
	atkAnimMeteor = 8
	atkAnimDriveby = 9
	atkAnimNull  = 10
End Enum

Enum AttackerAnimation
	atkrAnimStrike = 0     ' (shows weapon)
	atkrAnimCast = 1
	atkrAnimDashIn = 2     ' (shows weapon)
	atkrAnimSpinStrike = 3 ' (shows weapon)
	atkrAnimJump = 4
	atkrAnimLand = 5
	atkrAnimNull = 6
	atkrAnimStandingCast = 7
	atkrAnimTeleport = 8   ' (shows weapon)
	atkrAnimStandingStrike = 9 ' (shows weapon)
End Enum

Enum TransmogStatsRule
	transmogKeepCurrent = 0
	transmogUseNewMax = 1
	transmogKeepCurrentPercent = 2
	transmogKeepCurrentCropMax = 3
End Enum

'Attack transmogrification settings
Type TransmogData
	enemy as integer         ' ID, -1 means no transmogrification
	hp_rule as TransmogStatsRule
	other_stats_rule as TransmogStatsRule
	rewards_rule as integer  ' 0 (don't give) or 1 (give)
End Type

Type AttackData
	id as integer 'should only be set when loaded!
	name as string
	description as string
	picture as integer
	pal as integer
	override_wep_pic as bool 'YES if wep_picture and wep_pal are to be used.
	wep_picture as integer
	wep_pal as integer
	wep_handle(1) as XYPair 'Handle offset ONLY applies when override_wep_pic is YES. Ignored when the hero's equipped weapon is used
	anim_pattern as integer
	targ_class as integer
	targ_set as integer
	damage_math as integer
	aim_math as integer
	base_atk_stat as integer
	base_def_stat as integer
	mp_cost as integer
	hp_cost as integer
	money_cost as integer
	extra_damage as integer
	attacker_anim as AttackerAnimation
	attack_anim as AttackAnimation
	attack_delay as integer   'In active-battle mode: ticks to delay (non-negative)
	                          'In turn-based mode: number of attack queue slots (attacks) to advance/delay
	                          '(can be negative, to advance)
	turn_delay as integer 'in turns
	dramatic_pause as integer 'in ticks
	hits as integer
	targ_stat as integer
	prefer_targ as integer
	prefer_targ_stat as integer
	caption_time as integer
	caption as string
	caption_delay as integer
	tagset(1) as AttackDataTag
	item(2) as AttackDataItem
	elemental_fail_conds(maxElements - 1) as AttackElementCondition
	sound_effect as integer ' ID + 1
	chain as AttackDataChain
	elsechain as AttackDataChain
	instead as AttackDataChain
	learn_sound_effect as integer ' ID + 1
	transmog as TransmogData
	base_acc_stat as integer
	base_dog_stat as integer
	acc_mult as single
	dog_mult as single
	atk_mult as single
	def_mult as single
	aim_extra as single
	randomization as integer
	absorb_rate as single
	damage_color as integer '0 is default, >=1 is palette index (color zero not available)
	'----Bitsets----
	cure_instead_of_harm as bool
	divide_spread_damage as bool
	absorb_damage as bool
	unreversable_picture as bool
	can_steal_item as bool
	elemental_damage(maxElements - 1) as bool
	cannot_target_enemy_slot(7) as bool
	cannot_target_hero_slot(3) as bool
	ignore_extra_hits as bool
	erase_rewards as bool
	show_damage_without_inflicting as bool
	store_targ as bool
	delete_stored_targs as bool
	automatic_targ as bool
	show_name as bool
	do_not_display_damage as bool
	reset_targ_stat_before_hit as bool
	allow_cure_to_exceed_maximum as bool
	useable_outside_battle as bool
	obsolete_damage_mp as bool
	do_not_randomize as bool       'Obsolete, replaced by .randomization
	damage_can_be_zero as bool
	force_run as bool
	force_victory as bool
	force_battle_exit as bool
	mutable as bool
	fail_if_targ_poison as bool
	fail_if_targ_regen as bool
	fail_if_targ_stun as bool
	fail_if_targ_mute as bool
	percent_damage_not_set as bool
	check_costs_as_weapon as bool  'FIXME: broken/misnamed, affects all battle menu attacks
	no_chain_on_failure as bool
	reset_poison as bool
	reset_regen as bool
	reset_stun as bool
	reset_mute as bool
	cancel_targets_attack as bool
	not_cancellable_by_attacks as bool
	no_spawn_on_attack as bool
	no_spawn_on_kill as bool
	check_costs_as_item as bool
	recheck_costs_after_delay as bool
	targ_does_not_flinch as bool
	do_not_exceed_targ_stat as bool
	nonblocking as bool
	counterattack_provoke as CounterProkeEnum
	never_trigger_elemental_counterattacks as bool
	poison_is_negative_regen as bool
End Type

'An item in a hero's spell list definition (actual spell lists in HeroState.spells() array of integers)
Type SpellList
	attack as integer   'attack id+1, 0 for unused
	learned as integer  '0 if learnable from an item, or level+1 if learnt at level
End Type

Enum TagRangeCheckKind
	level = 1
End Enum

Type TagRangeCheck
	kind as TagRangeCheckKind
	tag as integer
	min as integer
	max as integer
End Type
DECLARE_VECTOR_OF_TYPE(TagRangeCheck, TagRangeCheck) 'DEFINE_ in common.rbas

Type HeroDef
	name as string
	sprite as integer
	sprite_pal as integer
	walk_sprite as integer
	walk_sprite_pal as integer
	portrait as integer
	portrait_pal as integer
	def_level as integer
	exp_mult as double
	def_weapon as integer    'Not offset by 1, unlike HeroState.def_wep!
	Lev0 as Stats
	LevMax as Stats
	spell_lists(3,23) as SpellList
	elementals(maxElements - 1) as single
	elem_counter_attack(maxElements - 1) as integer 'id+1, 0=none
	non_elem_counter_attack as integer 'id+1, 0=none
	stat_counter_attack(11) as integer 'id+1, 0=none
	bits(2) as integer
	list_name(3) as string
	list_type(3) as integer
	have_tag as integer
	alive_tag as integer
	leader_tag as integer
	active_tag as integer
	checks(any) as TagRangeCheck
	max_name_len as integer
	hand_pos(1) as XYPair
	reld as Reload.NodePtr

	Declare Constructor (id as integer = -1)
	Declare Destructor ()
End Type

'This caches the tags needed by evalherotag/tag_is_special
TYPE HeroTagsCache
	Declare Constructor ()
	Declare Destructor ()
	have_tag as integer
	alive_tag as integer
	leader_tag as integer
	active_tag as integer
	checks as TagRangeCheck vector
END TYPE

'This is part of the UDT for items, which hasn't been written yet
'It caches the tags needed by evalitemtag/tag_is_special
TYPE ItemTagsCache
	have_tag         as integer
	in_inventory_tag as integer
	is_equipped_tag  as integer
	is_actively_equipped_tag as integer
END TYPE

TYPE EnemyStealDef
  thievability as integer
  item as integer
  item_rate as integer
  rare_item as integer
  rare_item_rate as integer
END TYPE

TYPE EnemyRewardDef
  gold as long
  exper as long
  item as integer
  item_rate as integer
  rare_item as integer
  rare_item_rate as integer
END TYPE

TYPE EnemySpawnDef
  how_many as integer
  on_death as integer                       'id+1, 0=none
  non_elemental_death as integer            'id+1, 0=none
  when_alone as integer                     'id+1, 0=none
  non_elemental_hit as integer              'id+1, 0=none
  elemental_hit(maxElements - 1) as integer 'id+1, 0=none
  all_elements_on_hit as bool
END TYPE

TYPE EnemyDef
  name as string
  steal as EnemyStealDef
  reward as EnemyRewardDef
  dissolve as integer
  dissolve_length as integer
  dissolve_in as integer
  dissolve_in_length as integer
  death_sound as integer ' id+1, 0=default, -1=none
  cursor_offset as XYPair
  pic as integer
  pal as integer
  size as integer
  stat as Stats
  elementals(maxElements - 1) as single
  spawn as EnemySpawnDef
  regular_ai(4) as integer     'id+1, 0=none
  desperation_ai(4) as integer 'id+1, 0=none
  alone_ai(4) as integer       'id+1, 0=none
  elem_counter_attack(maxElements - 1) as integer 'id+1, 0=none
  non_elem_counter_attack as integer 'id+1, 0=none
  stat_counter_attack(11) as integer 'id+1, 0=none
  bequest_attack as integer    'id+1, 0=none
  '--bitsets
  harmed_by_cure as bool
  mp_idiot       as bool  'for turn loss when using MP-consuming attacks after MP runs out
  is_boss        as bool
  unescapable    as bool
  die_without_boss    as bool
  flee_instead_of_die as bool
  enemy_untargetable  as bool
  hero_untargetable   as bool
  death_unneeded as bool
  never_flinch   as bool
  ignore_for_alone    as bool

  Declare Constructor()
END TYPE

TYPE FormationSlot
  id as integer = -1  '-1: none
  pos as XYPair
END TYPE

TYPE Formation
  slots(7) as FormationSlot
  music as integer = -1         '-1: none, -2: same as map
  background as integer
  background_frames as integer = 1  'always >= 1 (no animation if == 1)
  background_ticks as integer
  victory_tag as integer        '0: none, 1+: tag number
  death_action as integer       '-1: continue game, 0: gameover
  hero_form as integer

  Declare Destructor()  'Does nothing
END TYPE

TYPE FormationSet
  frequency as integer
  tag as integer                'required tag
  formations(19) as integer     '-1: unused
END TYPE

TYPE HeroFormation
  slots(3) as FormationSlot
END TYPE

ENUM 'PortraitTypeEnum
 portraitNONE = 0
 portraitSPRITESET = 1    'Spriteset ID
 portraitPARTYRANK = 2    'Hero by caterpillar rank
 portraitPARTYSLOT = 3    'Hero by party slot
 portraitHEROID = 4       'Hero by ID
 portraitLAST = 4
END ENUM
TYPE PortraitTypeEnum as integer

TYPE TextBox
  text(any) as string

  'Conditionals
  instead_tag as integer
  instead     as integer
  settag_tag  as integer
  settag1     as integer
  settag2     as integer
  battle_tag  as integer
  battle      as integer
  shop_tag    as integer
  shop        as integer
  hero_tag    as integer
  hero_addrem as integer
  hero_swap   as integer
  hero_lock   as integer
  game_tag    as integer
  game_delete as integer
  game_save   as integer
  game_load   as integer
  after_tag   as integer
  after       as integer
  money_tag   as integer
  money       as integer
  door_tag    as integer
  door        as integer
  item_tag    as integer
  item        as integer
  menu_tag    as integer
  menu        as integer

  'Choicebox
  choice_enabled as bool
  choice(1)   as string
  choice_tag(1) as integer

  'Appearance
  no_box      as bool
  opaque      as bool
  vertical_offset as integer ' in 4-pixel increments
  shrink      as integer = -1 ' in 4-pixel increments, -1 is "Auto"
  textcolor   as integer     ' 0=default
  boxstyle    as integer
  backdrop    as integer     ' +1
  backdrop_trans as bool

  portrait_box  as integer
  portrait_type as PortraitTypeEnum
  portrait_id   as integer
  portrait_pal  as integer = -1
  portrait_pos  as XYPair

  restore_music as bool
  music         as integer   ' +1, 0=none, -1=silence
  sound_effect  as integer   ' +1, 0 means none
  stop_sound_after as bool
  line_sound    as integer   ' +1, 0=default, -1=none
END TYPE

TYPE VehicleData
  name           as string
  speed          as integer
  random_battles as integer
  use_button     as integer
  menu_button    as integer
  riding_tag     as integer
  on_mount       as integer
  on_dismount    as integer
  override_walls as integer
  blocked_by       as integer
  mount_from     as integer
  dismount_to    as integer
  elevation      as integer
  '--bitsets
  pass_walls     as bool
  pass_npcs      as bool
  enable_npc_activation as bool
  enable_door_use       as bool
  do_not_hide_leader    as bool
  do_not_hide_party     as bool
  dismount_ahead        as bool
  pass_walls_while_dismounting as bool
  disable_flying_shadow as bool
END TYPE

TYPE VehicleState
  active    as bool 'Is mounting/in/dismounting. If this is false, the rest is garbage
  id        as integer = -1 'Vehicle definition id that is loaded into .dat
  dat       as VehicleData
  npc       as NPCIndex
  old_speed as integer 'hero speed before mount
  mounting  as bool
  rising    as bool
  falling   as bool
  init_dismount as bool
  trigger_cleanup as bool
  ahead     as bool
END TYPE

TYPE SpriteSize
 name as string
 size as XYPair            'Default size
 fixed_size as bool        'Sizes other than the default not allowed
 frames as integer
 directions as integer
 genmax as integer         'Offset in gen() where max record index is stored
 genmax_offset as integer  'if gen() actually stores num instead of max, this is -1
 DECLARE FUNCTION lastrec() as integer  'Max valid record number
END TYPE

TYPE DistribState
  pkgname as string
  gamename as string
  author as string
  email as string
  website as string
  description as string
  more_description as string
  license as string
  copyright_year as string
END TYPE

'Holds the contents of a RELOAD node like general_reld."editor_version" or rsav."client_version"
'Normally you should test against .branch_revision instead of .revision.
TYPE EngineVersion
  recorded as bool           'False for old .rpg files without version info (before r5761, Apr 9 2013)
  name as string             'Expected to be 'OHRRPGCE'
  long_version as string     'Complete version and build string
  branch_name as string      '"wip" or release name
  revision as integer        'SVN revision, or 0 if unknown.
                             'You should normally test against .branch_revision instead!
  branch_revision as integer 'SVN revision at which the release was branched, equal to .revision for "wip"
END TYPE

'--Describes a set of OHR scancodes used for menu movement,
'--including confirm and cancel. This is mainly intended for
'--facilitating multiplayer gamepad support by allowing a
'--menu-type interface to be navigated only by one player
TYPE ArrowSet
  U as integer
  R as integer
  D as integer
  L as integer
  confirm as integer  'ENTER
  confirm2 as integer 'SPACE
  confirm3 as integer 'CTRL
  cancel as integer   'ESC
  cancel2 as integer  'ALT
END TYPE

TYPE BoxStyle
 edgecol as integer
 bgcol as integer
 border as integer '0 for none, >= 1 is border sprite id -1
END TYPE

#ENDIF
