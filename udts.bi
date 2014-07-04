#IFNDEF UDTS_BI
#DEFINE UDTS_BI

#INCLUDE "const.bi"
#INCLUDE "util.bi"
#INCLUDE "reload.bi"

'Forward declarations
TYPE SliceFwd as Slice

TYPE MouseInfo
  x as integer
  y as integer
  moved as bool      'Whether mouse has moved since last readmouse call.
  movedtick as bool  'Whether mouse has moved since the last setkeys call
  clicks as integer  'Button down event since last readmouse call; MouseButton bitvector (see scancodes.bi)
  clickstick as integer 'Button down since the last setkeys call
  buttons as integer 'Buttons currently down OR clicked; MouseButton bitvector
  wheel as integer   'Wheel movement since last tick; NOT SUPPORTED ON ALL BACKENDS
  dragging as integer 'MouseButton bitvector, but only one button at once can be dragged.
                      'A dragged button is one held down for at least 2 ticks. 
                      'So on the first tick, you see click=button=true, dragging=false
                      'And on the subsequent ticks, you see dragging=button=true, click=false
  clickstart as XYPair 'Mouse position at start of click/drag (Note: no backend currently
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

Enum SpriteType
 sprTypeInvalid = -1
 sprTypeHero = 0
 sprTypeSmallEnemy = 1
 sprTypeMediumEnemy = 2
 sprTypeLargeEnemy = 3
 sprTypeWalkabout = 4
 sprTypeWeapon = 5
 sprTypeAttack = 6
 sprTypeBoxBorder = 7
 sprTypePortrait = 8
 sprTypeLastPT = 8
 sprTypeMXS = 9    'Can't change this! Saved in .slice files
 sprTypeLast = 9   'Last sprite type selectable in slice editor
 sprTypeFrame = 10  'A sprite not loaded from file, but from a Frame. Free to change this later (never saved)
End Enum

'WARNING: don't add strings to this
TYPE Palette16
	col(15) as ubyte 'indices into the master palette
	refcount as int32 'private
END TYPE

TYPE SpriteCacheEntryFwd as SpriteCacheEntry
TYPE SpriteSetFwd as SpriteSet

'An 8 bit, single frame of a sprite.
'Don't forget to update definition in blit.c when changing this!!
'As a rather ugly hack (TODO: remove), arrays of Frames are sometimes (for sprite sets) allocated contiguously,
'with each having pointers to separate .image and .mask buffers. All will initially have .refcount = 1,
'.arraylen set to the length of the array, and all but first will have .arrayelem ON.
'WARNING: don't add strings to this
type Frame
	w as int32
	h as int32
	pitch as int32     'pixel (x,y) is at .image[.x + .pitch * .y]; mask and image pitch are the same!
	image as ubyte ptr
	mask as ubyte ptr
	refcount as int32  'see frame_unload in particular for documentation
	arraylen as int32  'how many frames were contiguously allocated in this frame array
	base as Frame ptr    'if a view, the Frame which actually owns this memory
	cacheentry as SpriteCacheEntryFwd ptr
	cached:1 as int32  '(not set for views onto cached sprites)
	arrayelem:1 as int32  'not the first frame in a frame array
	isview:1 as int32

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
	anim_loop as integer  '-1:infinite, 0<:number of times to play after current
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
  menufile as string
  itemfile as string
END TYPE

TYPE BasicMenuItem
  text as string
  col as integer
  bgcol as integer
  unselectable as bool
  disabled as bool  'Appear greyed out. Any other meaning of this is up to the user
END TYPE

DECLARE_VECTOR_OF_TYPE(BasicMenuItem, BasicMenuItem)

MAKETYPE_DoubleList(MenuDefItem)
MAKETYPE_DListItem(MenuDefItem)

TYPE MenuDefItem  'EXTENDS BasicMenuItem
  'members copied from BasicMenuItem
  text as string  ' This is the caption actually displayed
  col as integer  ' used to manually override the color of the menu item (has no effect when the menu item is selected and flashing)
  bgcol as integer  ' Not used
  unselectable as bool  ' Not used (yet)
  disabled as bool  ' set at run-time based on .tag1 and .tag2

  'Other members
  handle    as integer
  caption   as string  ' This is the caption as set in the menu editor/set menu item caption
  trueorder as DListItem(MenuDefItem) ' contains next, prev
  t         as integer
  sub_t     as integer
  tag1      as integer
  tag2      as integer
  settag    as integer
  togtag    as integer
  extra(2)  as integer
  hide_if_disabled  as bool
  close_if_selected as bool
  skip_close_script as bool
  dataptr   as any ptr  'Use this with caution!
END TYPE

DECLARE_VECTOR_OF_TYPE(MenuDefItem, MenuDefItem)

'*** Requires construction (with ClearMenuData or LoadMenuData) ***
TYPE MenuDef
  record    as integer
  handle    as integer
  name      as string
  boxstyle  as integer
  textcolor as integer
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
  highlight_selection as bool 'Bitset 9
  rect      as RectType
  offset    as XYPair
  anchor    as XYPair
  align     as integer
  min_chars as integer
  max_chars as integer
  bordersize as integer
  itemspacing as integer 'pixels added to the default item spacing.
                          'negative shrinks, positive grows. This
                          'alters the vertical size of the whole menu!
  on_close  as integer 'script trigger
  esc_menu  as integer
  Declare Destructor () 'defined in menus.bas
END TYPE

TYPE MenuState
  active    as bool = YES
  pt        as integer 'currently selected item (.first - 1 if menu is empty)
  top       as integer 'scroll position for long lists
  first     as integer 'first element (usually zero)
  last      as integer 'last element (.first - 1 if menu is empty)
  size      as integer 'number of elements to display at a time - 1
  need_update as bool  'menu needs some kind of update
  tog       as bool    'For flashing cursor
  '--populated when the menu is drawn
  has_been_drawn as bool
  rect as RectType
  spacing as integer
END TYPE

'A set of rendering options which can be passed to standardmenu.
'This partially overlaps MenuDef, which is the reason it wasn't added to MenuState:
'MenuState is used for drawing MenuDefs too, which would be confusing
TYPE MenuOptions
  edged as bool
  wide as integer = 9999
  highlight as bool
  showright as bool       'Always show the right-most portion of text if too long,
                          'instead of only doing so when selected.
  scrollbar as bool       'Draw a scrollbar (boxstyle 0); position given by x, y, .wide and MenuState.size
  fullscreen_scrollbar as bool 'Draw a scrollbar at the right edge of the screen
END TYPE

'For when a string array is too crude, but a MenuDef is overkill
TYPE SimpleMenuItem  'EXTENDS BasicMenuItem
  'members copied from BasicMenuItem
  text as string
  col as integer
  bgcol as integer
  unselectable as bool
  disabled as bool  'Appear greyed out. Other meaning of this depend on use 

  'other members
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

'Don't forget to update CleanNPCL and load/save_npc_locations when adding to this
TYPE NPCInst
  sl as SliceFwd Ptr
  '--stored values
  x as integer      'npcl+0
  y as integer      'npcl+300
  z as integer      '            Does not include foot offset
  id as integer     'npcl+600    0 if unused, ID + 1 for normal NPCs, -ID - 1 for hidden NPCs
                    '  NOTE: NPCs with invalidly high ID numbers (on incompletely loaded maps)
                    '        are automatically hidden (and obviously unsafe to unhide)
  xgo as integer    'npcl+1500   warning: positive to go LEFT, negative RIGHT
  ygo as integer    'npcl+1800   reversed as above
  dir as integer    'npcl+900
  frame as integer  'npcl+1200   0-3: ticks in walk animation loop (2 ticks per frame)
  extra(2) as integer
  ignore_walls as bool  'ignore passmap
  not_obstruction as bool 'can walk through npcs+heroes and vice-versa
  suspend_use as bool   'not activatable 
  suspend_ai as bool    'ignore movetype (effectively 'stand still')
  curzones as integer vector  'sorted vector of zones this NPC is currently inside
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
  pause as bool
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
  'Script attributes
  id as integer         'id number of script  (set to 0 to mark as unused slot)
                        'The id number is negated if this is a stale unreloaded script and
                        'shouldn't be used for new scripts.
  ptr as integer ptr    'pointer to script commands
  size as integer       'size of script data, in 4 byte words
  vars as integer       'local variable (including arguments) count, not including nonlocals
  nonlocals as integer  'number of nonlocal variables
  args as integer       'number of arguments
  strtable as integer   'pointer to string table (offset from start of script data in long ints)
  nestdepth as integer  'nesting depth, 0 for scripts
  parent as integer     'ID of parent or 0 for none

  'Book keeping
  refcount as integer   'number of ScriptInst pointing to this data
  totaluse as integer   'total number of times this script has been requested since loading
  lastuse as integer
  totaltime as double   'time spent in here, in seconds. Used only if SCRIPTPROFILE is defined
  entered as integer    'number of times entered. Used only if SCRIPTPROFILE is defined

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
  state as integer      'what the script is doing right now
  ptr as integer        'the execution pointer (in int32's from the start of the script data)
  ret as integer        'the scripts current return value
  curargn as integer    'current arg for current statement
  depth as integer      'stack depth of current script
  id as integer         'id number of current script (duplicated from ScriptInst)
END TYPE

ENUM WaitTypeEnum
  waitingOnNothing = 0  'The script isn't waiting
  waitingOnCmd          'A command in the script triggered a wait (ScriptInst.curvalue says which)
  waitingOnTick         'The script was externally made to wait; ScriptInst.waitarg gives the number of ticks
END ENUM

'Externally visible state of an executing script, used outside the interpreter
TYPE ScriptInst
  scr as ScriptData ptr 'script in script() hashtable
  waiting as WaitTypeEnum  'Whether the script is waiting
  waitarg as integer    'wait state argument 1
  waitarg2 as integer   'wait state argument 2
  watched as bool       'true for scripts which are being logged
  started as bool       'used only if watched is true: whether the script has started
  id as integer         'id number of script

  'these 3 items are only updated when the script interpreter is left. While inside
  'the script interpreter (command handlers) use the curcmd (ScriptCommand ptr) global.
  'These are also updated when a script is stopped (either suspended, or interpretloop is left).
  curkind as integer    'kind of current statement
  curvalue as integer   'value/id of current statement
  curargc as integer    'number of args for current statement  (used only in old script debugger...)
END TYPE

TYPE QueuedScript
  id as integer         'Triggers pre-decoded
  scripttype as string
  trigger_loc as string 'More information about how it was triggered
  double_trigger_check as bool  'Whether to prevent double triggering
  log_line as string    'Debugging aid: Comprised from scripttype, arg names and values and trigger_loc
  argc as integer       'The number of args passed
  args(3) as integer
END TYPE

TYPE ScriptCommand
  kind as integer
  value as integer
  argc as integer
  args(0) as integer
END TYPE

UNION RGBcolor
  as uint32 col
  TYPE
    as ubyte b, g, r, a
  END TYPE
END UNION

Type TileAnimState
  cycle as integer 'Current tile offset (tile to show)
  pt as integer    'Step number of the next step in the animation
  skip as integer  'Number of ticks left in current wait
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
	initialised as bool
	layers(1) as FontLayer ptr	'single layer fonts should use sprite(1) only
	w(255) as integer	'width of each character
	h as integer		'height of a line
	offset as XYPair	'added to coordinates when printing
	cols as integer		'number of used colours, not including colour 0 (transparency), so at most <s>255</s> 15
	pal as Palette16 ptr    '(Default) palette template to use, or NULL if this font is unpaletted (foreground colour only)
	pal_id as integer       'id of pal. Not used by render_text
	outline_col as integer  'palette entry which should be filled with uiOutline, or -1 for none
End Type

'text_layout_dimensions returns this struct
Type StringSize
	h as integer         'Height (in pixels)
	w as integer         'Greatest width of any line
	endchar as integer   'For when maxlines is specified: one character past last line
	lastw as integer     'Width of last line fragment
	lasth as integer     'Height of last line fragment
	lines as integer     'Number of lines (always at least 1)   FIXME:  not true
	finalfont as Font ptr
End Type

Type StringCharPos
	charnum as integer   'offset in string; equal to len(text) if off the end
	exacthit as bool     'whether actually on this character, or just the nearest (eg. off end of line)
	x as integer         'position is in screen coordinates
	y as integer
	'w as integer        'Size of the selected character (do we really need this?)
	h as integer
	lineh as integer     'height of containing line fragment
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
               focus as integer
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
	must_know as bool
	no_delay as bool
	nonblocking as bool
	dont_retarget as bool
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
	attacker_anim as integer
	attack_anim as integer
	attack_delay as integer 'in ticks
	turn_delay as integer 'in turns
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
	delete_stored_targ as bool
	automatic_targ as bool
	show_name as bool
	do_not_display_damage as bool
	reset_targ_stat_before_hit as bool
	allow_cure_to_exceed_maximum as bool
	useable_outside_battle as bool
	obsolete_damage_mp as bool
	do_not_randomize as bool
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
	check_costs_as_weapon as bool
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
	never_trigger_elemental_counterattacks as bool
	poison_is_negative_regen as bool
End Type

'An item in a hero's spell list definition (actual spell lists in spell() array of integers)
Type SpellList
	attack as integer   'attack id+1, 0 for unused
	learned as integer  '0 if learnable from an item, or level+1 if learnt at level
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
	reld as Reload.NodePtr
	
	Declare Destructor ()  'defined in common.bas
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
  on_death as integer                       'id+1, 0=none
  non_elemental_death as integer            'id+1, 0=none
  when_alone as integer                     'id+1, 0=none
  non_elemental_hit as integer              'id+1, 0=none
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
  regular_ai(4) as integer     'id+1, 0=none
  desperation_ai(4) as integer 'id+1, 0=none
  alone_ai(4) as integer       'id+1, 0=none
  elem_counter_attack(maxElements - 1) as integer 'id+1, 0=none
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
END TYPE

TYPE FormationSlot
  id as integer    '-1: none
  pos as XYPair
END TYPE

TYPE Formation
  slots(7) as FormationSlot
  music as integer              '-1: none, -2: same as map
  background as integer
  background_frames as integer  'always >= 1 (no animation if == 1)
  background_ticks as integer
END TYPE

TYPE FormationSet
  frequency as integer
  tag as integer                'required tag
  formations(19) as integer     '-1: unused
END TYPE

TYPE TextBox
  text(7) as string
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
  choice_enabled as bool
  no_box      as bool
  opaque      as bool
  restore_music as bool
  choice(1)   as string
  choice_tag(1) as integer
  vertical_offset as integer ' in 4-pixel increments
  shrink      as integer     ' in 4-pixel increments, -1 is "Auto"
  textcolor   as integer     ' 0=default
  boxstyle    as integer
  backdrop    as integer     ' +1
  music       as integer     ' +1
  portrait_box  as integer
  portrait_type as integer
  portrait_id   as integer
  portrait_pal  as integer
  portrait_pos  as XYPair
  sound_effect as integer    ' +1, 0 means none
  stop_sound_after as bool
  backdrop_trans as bool
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
  id        as integer 'vehicle defintion id that is loaded into .dat
  dat       as VehicleData
  npc       as integer 'npc reference number
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
 size as XYPair
 frames as integer
 directions as integer
 genmax as integer         'Offset in gen() where max record index is stored
 genmax_offset as integer  'if gen() actually stores num instead of max, this is -1
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
