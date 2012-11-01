#IFNDEF UDTS_BI
#DEFINE UDTS_BI

#INCLUDE "const.bi"
#INCLUDE "util.bi"

'Forward declarations
TYPE SliceFwd AS Slice

UNION XYPair
  TYPE
   x AS INTEGER
   y AS INTEGER
  END TYPE
  TYPE
   w AS INTEGER
   h AS INTEGER
  END TYPE
  n(1) AS INTEGER
END UNION

'TYPE RectType
'  x AS INTEGER
'  y AS INTEGER
'  wide AS INTEGER
'  high AS INTEGER
'END TYPE

UNION RectType
  TYPE
    x AS INTEGER
    y AS INTEGER
    wide AS INTEGER
    high AS INTEGER
  END TYPE
  TYPE
    p1 AS XYPair
    p2 AS XYPair
  END TYPE
END UNION

TYPE MouseInfo
  x AS INTEGER
  y AS INTEGER
  clicks AS INTEGER  'Button down event since last tick; MouseButton array (see scancodes.bi)
  buttons AS INTEGER 'Buttons currently down OR clicked; MouseButton array
  wheel AS INTEGER   'Wheel movement since last tick; NOT SUPPORTED ON ALL BACKENDS
  dragging AS INTEGER 'MouseButton array, but only one button at once can be dragged.
                      'A dragged button is one held down for at least 2 ticks. 
                      'So on the first tick, you see click=button=true, dragging=false
                      'And on the subsequent ticks, you see dragging=button=true, click=false
  clickstart AS XYPair 'Mouse position at start of click/drag (Note: no backend currently
                       'supports reporting the position of click, so currently equal to .x/.y)
END TYPE

ENUM CompType
  compNone
  compEq  ' =
  compNe  ' <>
  compLt  ' <
  compLe  ' <=
  compGt  ' >
  compGe  ' >=
  compTag ' 
END ENUM

'A check on the value of a tag or global variable. When type == compTag, then
'.tag is positive to check that the tag is on, and negative to check it is off,
'and .tag = 0 is equivalent to type == compNone.
'The meaning of compNone is context specific: it might be either Always or Never.
TYPE Condition
  UNION
    varnum as integer  ' global
    tag as integer
  END UNION
  type as CompType
  value as integer  'Not used when type == compTag
  'The following only used only in Custom and never saved
  editstate as ubyte
  lastinput as ubyte
END TYPE

'WARNING: don't add strings to this
TYPE Palette16
	col(15) as ubyte 'indices into the master palette
	refcount as integer 'private
END TYPE

TYPE SpriteCacheEntryFwd as SpriteCacheEntry
TYPE SpriteSetFwd as SpriteSet

'sprites use this
'don't forget to update definition in blit.c when changing this!!
'WARNING: don't add strings to this
type Frame
	w as integer
	h as integer
	pitch as integer     'pixel (x,y) is at .image[.x + .pitch * .y]; mask and image pitch are the same!
	image as ubyte ptr
	mask as ubyte ptr
	refcount as integer  'see frame_unload in particular for documentation
	arraylen as integer  'how many frames were contiguously allocated in this frame array
	base as Frame ptr    'the Frame which actually owns this memory
	cacheentry as SpriteCacheEntryFwd ptr
	cached:1 as integer  '(not set for views onto cached sprites)
	arrayelem:1 as integer  'not the first frame in a frame array
	isview:1 as integer

	'used only by frames in a SpriteSet, for now
	offset as XYPair
	sprset as SpriteSetFwd ptr  'if not NULL, this Frame array is part of a SpriteSet which
                                    'will need to be freed at the same time
end type

ENUM AnimOpType
	animOpWait	'(ticks)
	animOpFrame	'(framenum)
	animOpSetOffset	'(x,y)
	animOpRelOffset	'(x,y)
END ENUM

TYPE AnimationOp
	type as AnimOpType
	arg1 as integer		
	arg2 as integer		
END TYPE

TYPE Animation
	name as string
	numitems as integer
	ops as AnimationOp ptr
END TYPE

'in effect, inherits from Frame
TYPE SpriteSet
	numanimations as integer
	animations as Animation ptr
	numframes as integer  'redundant to frames->arraylen
	frames as Frame ptr
	'uses refcount from frames
END TYPE

'A REAL sprite; This is basically a SpriteSet object with state
'Not refcounted. I don't currently see a reason that it should be (each the state of a single object)
'WARNING: don't add strings to this
TYPE SpriteState
	set as SpriteSet ptr
	curframe as Frame ptr  'convenience ptr to set->frames[.frame_id]
	pal as Palette16 ptr
	frame_id as integer
	anim as Animation ptr
	anim_step as integer
	anim_wait as integer
	anim_loop as integer '-1:infinite, 0<:number of times to play after current
	offset as XYPair
END TYPE

TYPE GraphicPair
	sprite as frame ptr
	pal as palette16 ptr
END TYPE

TYPE ClipState
	whichframe as Frame ptr
	clipl as integer
	clipr as integer
	clipt as integer
	clipb as integer
END TYPE

TYPE MenuSet
  menufile AS STRING
  itemfile AS STRING
END TYPE

TYPE BasicMenuItem
  text AS STRING
  col AS INTEGER
  bgcol AS INTEGER
  unselectable AS INTEGER
  disabled AS INTEGER  'Appear greyed out. Any other meaning of this is up to the user
END TYPE

DECLARE_VECTOR_OF_TYPE(BasicMenuItem, BasicMenuItem)

MAKETYPE_DoubleList(MenuDefItem)
MAKETYPE_DListItem(MenuDefItem)

TYPE MenuDefItem  'EXTENDS BasicMenuItem
  'members copied from BasicMenuItem
  text AS STRING  ' This is the caption actually displayed
  col AS INTEGER
  bgcol AS INTEGER  ' Not used
  unselectable AS INTEGER  ' Not used (yet)
  disabled AS INTEGER ' set at run-time based on .tag1 and .tag2

  'Other members
  handle    AS INTEGER
  caption   AS STRING  ' This is the caption as set in the menu editor/set menu item caption
  trueorder AS DListItem(MenuDefItem) ' contains next, prev
  t         AS INTEGER
  sub_t     AS INTEGER
  tag1      AS INTEGER
  tag2      AS INTEGER
  settag    AS INTEGER
  togtag    AS INTEGER
  extra(2)  AS INTEGER
  hide_if_disabled  AS INTEGER ' Bitset
  close_if_selected AS INTEGER ' Bitset
  skip_close_script AS INTEGER ' Bitset
  dataptr   AS ANY PTR 'Use this with caution!
END TYPE

DECLARE_VECTOR_OF_TYPE(MenuDefItem, MenuDefItem)

'*** Requires construction (with ClearMenuData or LoadMenuData) + destruction (with DeleteMenuItems) ***
TYPE MenuDef
  record    AS INTEGER
  handle    AS INTEGER
  name      AS STRING
  boxstyle  AS INTEGER
  textcolor AS INTEGER
  maxrows   AS INTEGER
  edit_mode AS INTEGER 'Never hide disabled items, allow selection of unselectable items
  items     AS MenuDefItem Ptr Ptr
  'adds first, last, numitems, itemlist members
  INHERITAS_DoubleList(MenuDefItem, itemlist) 'True order of menu items, ignoring sort-invisible-items-to-end
  translucent      AS INTEGER ' Bitset 0
  no_scrollbar     AS INTEGER ' Bitset 1
  allow_gameplay   AS INTEGER ' Bitset 2
  suspend_player   AS INTEGER ' Bitset 3
  no_box           AS INTEGER ' Bitset 4
  no_close         AS INTEGER ' Bitset 5
  no_controls      AS INTEGER ' Bitset 6
  prevent_main_menu AS INTEGER ' Bitset 7
  advance_textbox  AS INTEGER ' Bitset 8
  rect      AS RectType
  offset    AS XYPair
  anchor    AS XYPair
  align     AS INTEGER
  min_chars AS INTEGER
  max_chars AS INTEGER
  bordersize AS INTEGER
  on_close  AS INTEGER 'script trigger
  esc_menu  AS INTEGER
END TYPE

TYPE MenuState
  active    AS INTEGER 'boolean
  pt        AS INTEGER 'currently selected item (.first - 1 if menu is empty)
  top       AS INTEGER 'scroll position for long lists
  first     AS INTEGER 'first element (usually zero)
  last      AS INTEGER 'last element (.first - 1 if menu is empty)
  size      AS INTEGER 'number of elements to display at a time - 1
  need_update AS INTEGER 'menu needs some kind of update
  tog       AS INTEGER ' For flashing cursor
END TYPE

'For when a string array is too crude, but a MenuDef is overkill
TYPE SimpleMenuItem  'EXTENDS BasicMenuItem
  'members copied from BasicMenuItem
  text AS STRING
  col AS INTEGER
  bgcol AS INTEGER
  unselectable AS INTEGER
  disabled AS INTEGER  'Appear greyed out. Other meaning of this depend on use 

  'other members
  dat AS INTEGER  'For your own use
END TYPE

DECLARE_VECTOR_OF_TYPE(SimpleMenuItem, SimpleMenuItem)

'Warning: when editing NPCType, consider updating read/write_npc_int and serialization disasters
'including updating readnpc, alternpc, plotscr.hsd constants, and plotdict.xml
TYPE NPCType
  picture as integer     '+0
  palette as integer     '+1
  movetype as integer    '+2
  speed as integer       '+3  real speed, not value in .d
  textbox as integer     '+4
  facetype as integer    '+5
  item as integer        '+6
  pushtype as integer    '+7
  activation as integer  '+8
  tag1 as integer        '+9   appear only if
  tag2 as integer        '+10  appear only if 2
  usetag as integer      '+11  onetime use bit or 0 for none
  script as integer      '+12
  scriptarg as integer   '+13
  vehicle as integer     '+14
  defaultzone as integer '+15
  defaultwallzone as integer '+16
  sprite as frame ptr
  pal as palette16 ptr
END TYPE

'Don't forget to update CleanNPCL and savegame.bas when adding to this
TYPE NPCInst
  sl AS SliceFwd Ptr
  '--stored values
  x as integer      'npcl+0
  y as integer      'npcl+300
  id as integer     'npcl+600    0 if unused, ID + 1 for normal NPCs, -ID - 1 for hidden NPCs
                    '  NOTE: NPCs with invalidly high ID numbers (on incompletely loaded maps)
                    '        are automatically hidden (and obviously unsafe to unhide)
  xgo as integer    'npcl+1500   warning: positive to go LEFT, negative RIGHT
  ygo as integer    'npcl+1800   reversed as above
  dir as integer    'npcl+900
  frame as integer  'npcl+1200   0-3: ticks in walk animation loop (2 ticks per frame)
  extra(2) as integer
  ignore_walls as integer  'ignore passmap
  not_obstruction as integer 'can walk through npcs+heroes and vice-versa
  suspend_use as integer   'not activatable 
  suspend_ai as integer    'ignore movetype (effectively 'stand still')
  curzones as integer vector  'sorted vector of zones this NPC is currently inside
END TYPE

TYPE InventSlot
  used as integer	'use this to check if empty, not num!

  'following fields should not be used if used = 0
  id as integer		'read ID number starting at 0, not +1!!
  num as integer
  text as string	'text field which shows up in inventory, blank if empty
  sortorder as integer  'contains garbage nearly always: only used in items_menu_autosort
END TYPE

TYPE PlotTimer
  count as integer
  speed as integer
  ticks as integer
  trigger as integer
  flags as integer '1=critical 2=battle 4=menu 8+=undefined
  st as integer 'string, but both str and string are reserved
  pause as integer '0 unpaused, -1 paused
  finished_tick as integer  'tick at which this last triggered; 0 if never
END TYPE

TYPE Plotstring
  s as string
  X as integer
  Y as integer
  col as integer
  bgcol as integer
  bits as integer
END TYPE

Type HSHeader
  valid as integer
  hspeak_version as string
  hsp_format as integer
  script_format as integer    'Same as the .HSZ format number, or 0 if not known
  max_function_id as integer  'Maximum ID of a used function, or 0 if not known
  plotscr_version as string   'empty if not known
End Type

TYPE TriggerData
  name as string
  id as integer
END TYPE

'WARNING: don't add strings to this
TYPE ScriptData
  id as integer         'id number of script  (set to 0 to mark as unused slot)
  refcount as integer   'number of ScriptInst pointing to this data
  totaluse as integer   'total number of times this script has been requested since loading
  lastuse as integer
  totaltime as double   'time spent in here, in seconds. Used only if SCRIPTPROFILE is defined
  entered as integer    'number of times entered. Used only if SCRIPTPROFILE is defined
  ptr as integer ptr    'pointer to script commands
  size as integer       'size of script data, in 4 byte words
  vars as integer       'variable (including arguments) count
  args as integer       'number of arguments
  strtable as integer   'pointer to string table (offset from start of script data in long ints)

  next as ScriptData ptr 'next in linked list, for hashtable
  backptr as ScriptData ptr ptr 'pointer to pointer pointing to this, in script(), or a .next pointer
                        'not your usual double linked list, because head of list is a script() element
END TYPE

TYPE ScriptInst
  scr as ScriptData ptr 'script in script() hashtable
  scrdata as integer ptr 'convenience pointer to scr->ptr
  heap as integer       'position of the script's local vars in the buffer
  stackbase as integer  'position where this script's stack data starts in scrst
  state as integer      'what the script is doing right now
  ptr as integer        'the execution pointer (in int32's from the start of the script data)
  ret as integer        'the scripts current return value
  curargn as integer    'current arg for current statement
  depth as integer      'stack depth of current script
  id as integer         'id number of current script
  waitarg as integer    'wait state argument 1
  waitarg2 as integer   'wait state argument 2
  watched as integer    'true for scripts which are being logged
  started as integer    'used only if watched is true: whether the script has started

  'these 3 items are only current/correct for inactive scripts. The active script's current
  'command is pointed to by the curcmd (ScriptCommand ptr) global, and copied here
  'when a script is stopped (either suspended, or interpretloop is left)
  curkind as integer    'kind of current statement
  curvalue as integer   'value of current statement
  curargc as integer    'number of args for current statement
END TYPE

TYPE QueuedScript
  id as integer         'Triggers pre-decoded
  scripttype as string
  trigger_loc as string 'More information about how it was triggered
  double_trigger_check as integer
  log_line as string    'Debugging aid: Comprised from scripttype, arg names and values and trigger_loc
  argc as integer       'The number of args passed
  args(3) as integer
END TYPE

TYPE ScriptCommand
  kind as integer
  value as integer
  argc as integer
  args(999999) as integer
END TYPE

UNION RGBcolor
	as uinteger col
		TYPE
			as ubyte b, g, r, a
		END TYPE
END UNION

Type TileAnimState
  cycle AS INTEGER
  pt AS INTEGER
  skip AS INTEGER
END Type

Type TilesetData
  num as integer
  spr as Frame ptr
  anim(1) as TileAnimState
  tastuf(40) as integer
End Type

'*** Requires construction + destruction ***
Type TileMap
  wide as integer
  high as integer
  data as ubyte ptr
  layernum as integer
End Type

TYPE TilemapInfo
  wide as integer
  high as integer
  layers as integer
END TYPE

'WARNING: don't add strings to this
Type ZoneHashedSegment
  IDmap(14) as ushort
  hashed as HashedItem
End Type

'Data associated with an individual zone, other than the tiles themselves
Type ZoneInfo
  id as integer
  name as string
  hidden as integer  'used in the map editor only, not saved
  numtiles as integer  'number of set tiles
  extra(2) as integer
End Type

'A set of zones
'See loading.bas for documentation.
'*** Requires construction + destruction ***
Type ZoneMap
  numzones as integer
  zones as ZoneInfo ptr
  wide as integer  'width in tiles
  high as integer  'height in tiles
  wide_segments as integer  'width in 4x4 tile segments
  high_segments as integer  'height in 4x4 tile segments
  bitmap as ushort ptr      'array dimensions are [high][wide]
  zoneIDmap as ushort ptr   'array dimensions are [high_segments][wide_segments][16]
  extraID_hash as HashTable 'table of ZoneHashedSegments for overcrowded tiles, indexed by (x SHL 16) + y
End Type

Type FontChar
	offset as integer  'offset into spr->image
	offx as byte   'pixel offsets
	offy as byte
	w as byte      'size of sprite
	h as byte
End Type

'WARNING: don't add strings to this
Type FontLayer
	spr as Frame ptr
	refcount as integer
	chdata(255) as FontChar
End Type

Type Font
	initialised as integer
	layers(1) as FontLayer ptr	'single layer fonts should use sprite(1) only
	w(255) as integer	'width of each character
	h as integer		'height of a line
	offset as XYPair	'added to coordinates when printing
	cols as integer		'number of used colours, not including colour 0 (transparency), so at most <s>255</s> 15
	pal as Palette16 ptr '(Default) palette template to use, or NULL if this font is unpaletted (foreground colour only)
	pal_id as integer   'id of pal. Not used by render_text
	outline_col as integer 'palette entry which should be filled with uiOutline, or -1 for none
End Type

'text_layout_dimensions returns this struct
Type StringSize
	h as integer  'Height (in pixels)
	w as integer  'Greatest width of any line
	endchar as integer  'For when maxlines is specified: one character past last line
	lastw as integer  'Width of last line fragment
	lasth as integer  'Height of last line fragment
	lines as integer  'Number of lines (always at least 1)   FIXME:  not true
	finalfont as Font ptr
End Type

Type StringCharPos
	charnum as integer  'offset in string; equal to len(text) if off the end
	exacthit as integer  'actually on this character, or just the nearest (eg. off end of line)
	x as integer  'position is in screen coordinates
	y as integer
	'w as integer  'Size of the selected character (do we really need this?)
	h as integer
	lineh as integer  'height of containing line fragment
End Type

Type PrintStrStatePtr as PrintStrState ptr

Type Door
	as integer x, y
	as integer bits(0)
End Type

Type DoorLink
	as integer source, dest, dest_map, tag1, tag2
End Type

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
               foc as integer
               hits as integer
       End Type
       sta(11) as integer
End Union

Type AttackDataTag
	tag as integer
	condition as integer
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
	must_know as integer
	no_delay as integer
	nonblocking as integer
	dont_retarget as integer
End Type

Type AttackElementCondition
	type as CompType  'Only compNone, compLt, compGt supported!
	value as single
End Type

Type AttackData
	id as integer 'should only be set when loaded!
	name as string
	description as string
	picture as integer
	pal as integer
	override_wep_pic as integer 'YES if wep_picture and wep_pal are to be used.
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
	attacker_anim as integer
	attack_anim as integer
	attack_delay as integer 'in ticks
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
	transmog_enemy as integer ' ID +1, 0 no change
	transmog_hp as integer
	transmog_stats as integer
	'----Bitsets----
	cure_instead_of_harm as integer
	divide_spread_damage as integer
	absorb_damage as integer
	unreversable_picture as integer
	can_steal_item as integer
	elemental_damage(maxElements - 1) as integer
	cannot_target_enemy_slot(7) as integer
	cannot_target_hero_slot(3) as integer
	ignore_extra_hits as integer
	erase_rewards as integer
	show_damage_without_inflicting as integer
	store_targ as integer
	delete_stored_targ as integer
	automatic_targ as integer
	show_name as integer
	do_not_display_damage as integer
	reset_targ_stat_before_hit as integer
	allow_cure_to_exceed_maximum as integer
	useable_outside_battle as integer
	obsolete_damage_mp as integer
	do_not_randomize as integer
	damage_can_be_zero as integer
	force_run as integer
	force_victory as integer
	force_battle_exit as integer
	mutable as integer
	fail_if_targ_poison as integer
	fail_if_targ_regen as integer
	fail_if_targ_stun as integer
	fail_if_targ_mute as integer
	percent_damage_not_set as integer
	check_costs_as_weapon as integer
	no_chain_on_failure as integer
	reset_poison as integer
	reset_regen as integer
	reset_stun as integer
	reset_mute as integer
	cancel_targets_attack as integer
	not_cancellable_by_attacks as integer
	no_spawn_on_attack as integer
	no_spawn_on_kill as integer
	check_costs_as_item as integer
	recheck_costs_after_delay as integer
	targ_does_not_flinch as integer
	do_not_exceed_targ_stat as integer
	nonblocking as integer
End Type

Type SpellList
	attack as integer
	learned as integer
End Type

Type HeroDef
	name as string
	sprite as integer
	sprite_pal as integer
	walk_sprite as integer
	walk_sprite_pal as integer
	portrait as integer
	portrait_pal as integer
	def_level as integer
	def_weapon as integer
	Lev0 as stats
	LevMax as stats
	spell_lists(3,23) as SpellList
	elementals(maxElements - 1) as single
	bits(2) as integer
	list_name(3) as string
	list_type(3) as integer
	have_tag as integer
	alive_tag as integer
	leader_tag as integer
	active_tag as integer
	max_name_len as integer
	hand_pos(1) as XYPair
End Type

'This caches the tags needed by evalherotag/tag_is_special
TYPE HeroTagsCache
	have_tag as integer
	alive_tag as integer
	leader_tag as integer
	active_tag as integer
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
  on_death as integer 'id+1, 0=none
  non_elemental_death as integer 'id+1, 0=none
  when_alone as integer 'id+1, 0=none
  non_elemental_hit as integer 'id+1, 0=none
  elemental_hit(maxElements - 1) as integer 'id+1, 0=none
END TYPE

TYPE EnemyDef
  name as string
  steal as EnemyStealDef
  reward as EnemyRewardDef
  dissolve as integer
  dissolve_length as integer
  death_sound as integer ' id+1, 0=default, -1=none
  cursor_offset as XYPair
  pic as integer
  pal as integer
  size as integer
  stat as stats
  elementals(maxElements - 1) as single
  spawn as EnemySpawnDef
  regular_ai(4) as integer 'id+1, 0=none
  desperation_ai(4) as integer 'id+1, 0=none
  alone_ai(4) as integer 'id+1, 0=none
  elem_counter_attack(maxElements - 1) as integer 'id+1, 0=none
  stat_counter_attack(11) as integer 'id+1, 0=none
  bequest_attack as integer 'id+1, 0=none
  '--bitsets
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
END TYPE

TYPE FormationSlot
  id as integer    '-1: none
  pos as XYPair
END TYPE

TYPE Formation
  slots(7) as FormationSlot
  music as integer              '-1: none, -2: same as map
  background as integer
  background_frames as integer  'always >= 1
  background_ticks as integer
END TYPE

TYPE FormationSet
  frequency as integer
  tag as integer                'required tag
  formations(19) as integer     '-1: unused
END TYPE

TYPE TextBox
  text(7) AS STRING
  instead_tag AS INTEGER
  instead     AS INTEGER
  settag_tag  AS INTEGER
  settag1     AS INTEGER
  settag2     AS INTEGER
  battle_tag  AS INTEGER
  battle      AS INTEGER
  shop_tag    AS INTEGER
  shop        AS INTEGER
  hero_tag    AS INTEGER
  hero_addrem AS INTEGER
  hero_swap   AS INTEGER
  hero_lock   AS INTEGER
  after_tag   AS INTEGER
  after       AS INTEGER
  money_tag   AS INTEGER
  money       AS INTEGER
  door_tag    AS INTEGER
  door        AS INTEGER
  item_tag    AS INTEGER
  item        AS INTEGER
  menu_tag    AS INTEGER
  menu        AS INTEGER
  choice_enabled AS INTEGER
  no_box      AS INTEGER
  opaque      AS INTEGER
  restore_music AS INTEGER
  choice(1)   AS STRING
  choice_tag(1) AS INTEGER
  vertical_offset AS INTEGER ' in 4-pixel increments
  shrink      AS INTEGER     ' in 4-pixel increments, -1 is "Auto"
  textcolor   AS INTEGER     ' 0=default
  boxstyle    AS INTEGER
  backdrop    AS INTEGER     ' +1
  music       AS INTEGER     ' +1
  portrait_box  AS INTEGER
  portrait_type AS INTEGER
  portrait_id   AS INTEGER
  portrait_pal  AS INTEGER
  portrait_pos  AS XYPair
  sound_effect AS INTEGER    ' +1, 0 means none
  stop_sound_after AS INTEGER 'bitset
END TYPE

TYPE VehicleData
  name           AS STRING
  speed          AS INTEGER
  random_battles AS INTEGER
  use_button     AS INTEGER
  menu_button    AS INTEGER
  riding_tag     AS INTEGER
  on_mount       AS INTEGER
  on_dismount    AS INTEGER
  override_walls AS INTEGER
  blocked_by       AS INTEGER
  mount_from     AS INTEGER
  dismount_to    AS INTEGER
  elevation      AS INTEGER
  '--bitsets
  pass_walls     AS INTEGER
  pass_npcs      AS INTEGER
  enable_npc_activation AS INTEGER
  enable_door_use       AS INTEGER
  do_not_hide_leader    AS INTEGER
  do_not_hide_party     AS INTEGER
  dismount_ahead        AS INTEGER
  pass_walls_while_dismounting AS INTEGER
  disable_flying_shadow AS INTEGER
END TYPE

TYPE VehicleState
  active    AS INTEGER 'Is mounting/in/dismounting. If this is false, the rest is garbage
  id        AS INTEGER 'vehicle defintion id that is loaded into .dat
  dat       AS VehicleData
  npc       AS INTEGER 'npc reference number
  old_speed AS INTEGER 'hero speed before mount
  mounting  AS INTEGER '0
  rising    AS INTEGER '1
  falling   AS INTEGER '2
  init_dismount AS INTEGER '3
  trigger_cleanup AS INTEGER '4
  ahead     AS INTEGER '5
END TYPE

TYPE PlotSprite
 used as integer
 sprite as Frame Ptr
 pal as Palette16 Ptr
 spr_type as integer 'hero, walkabout, etc...
 spr_num as integer
 frame as integer
 frames as integer
 visible as integer
 x as integer
 y as integer
END TYPE

TYPE SpriteSize
 name AS STRING
 size AS XYPair
 frames AS INTEGER
 genmax AS INTEGER 'Offset in gen() where max record index is stored
 genmax_offset AS INTEGER 'if gen() actually stores num instead of max, this is -1
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

#ENDIF
