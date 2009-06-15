#IFNDEF UDTS_BI
#DEFINE UDTS_BI

#INCLUDE "const.bi"

UNION XYPair
  TYPE
   x AS INTEGER
   y AS INTEGER
  END TYPE
  n(1) AS INTEGER
END UNION

TYPE RectType
  x AS INTEGER
  y AS INTEGER
  wide AS INTEGER
  high AS INTEGER
END TYPE

TYPE Palette16
	col(15) as ubyte 'indicies into the master palette
	refcount as integer 'private
END TYPE

'sprites use this
type Frame
	w as integer
	h as integer
	image as ubyte ptr
	mask as ubyte ptr
	refcount as integer
	cache as string
end type

TYPE GraphicPair
	sprite as frame ptr
	pal as palette16 ptr
END TYPE

TYPE MenuSet
  menufile  AS STRING
  itemfile AS STRING
END TYPE

TYPE MenuDefItem
  handle    AS INTEGER
  exists    AS INTEGER
  disabled  AS INTEGER ' set at run-time based on .tag1 and .tag2
  member    AS INTEGER
  caption   AS STRING
  sortorder AS INTEGER
  t         AS INTEGER
  sub_t     AS INTEGER
  tag1      AS INTEGER
  tag2      AS INTEGER
  settag    AS INTEGER
  togtag    AS INTEGER
  extra(2)  AS INTEGER
  hide_if_disabled  AS INTEGER ' Bitset
  close_if_selected AS INTEGER ' Bitset
END TYPE

TYPE MenuDef
  record    AS INTEGER
  handle    AS INTEGER
  name      AS STRING
  boxstyle  AS INTEGER
  textcolor AS INTEGER
  maxrows   AS INTEGER
  edit_mode AS INTEGER 'Never hide disabled items, allow selection of unselectable items
  items(20) AS MenuDefItem
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
END TYPE

TYPE MenuState
  active    AS INTEGER
  pt        AS INTEGER 'currently selected item
  top       AS INTEGER 'scroll position for long lists
  first     AS INTEGER 'first element (usually zero)
  last      AS INTEGER 'last element
  size      AS INTEGER 'number of elements to display at a time (actually index of last to display relative to top, so "size"-1)
  need_update AS INTEGER 'menu needs some kind of update
  tog       AS INTEGER ' For flashing cursor
END TYPE

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
  usetag as integer      '+11
  script as integer      '+12
  scriptarg as integer   '+13
  vehicle as integer     '+14
  sprite as frame ptr
  pal as palette16 ptr
END TYPE

TYPE NPCInst
  x as integer      'npcl+0
  y as integer      'npcl+300
  xgo as integer    'npcl+1500   warning: positive to go LEFT, negative RIGHT
  ygo as integer    'npcl+1800   reversed as above
  id as integer     'npcl+600
  dir as integer    'npcl+900
  frame as integer  'npcl+1200
  extra(2) as integer
END TYPE

TYPE InventSlot
  used as integer	'use this to check if empty, not num!

  'following fields should not be used if used = 0
  id as integer		'absolute, not +1!!
  num as integer
  text as string	'text field which shows up in inventory, blank if empty
END TYPE

TYPE PlotTimer
  count as integer
  speed as integer
  ticks as integer
  trigger as integer
  flags as integer
  st as integer 'string, but both str and string are reserved
  pause as integer '0 unpaused, -1 paused
END TYPE

TYPE Plotstring
  s as string
  X as integer
  Y as integer
  col as integer
  bgcol as integer
  bits as integer
END TYPE

TYPE ScriptData
  id as integer         'id number of script  (set to 0 to mark as unused slot)
  refcount as integer   'number of ScriptInst pointing to this data
  totaluse as integer   'total number of times this script has been requested since loading
  lastuse as integer
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
  state as integer      'what the script is doing right now
  ptr as integer        'the execution pointer (in int32's from the start of the script data)
  ret as integer        'the scripts current return value
  curargn as integer    'current arg for current statement
  depth as integer      'stack depth of current script
  id as integer         'id number of current script
  waitarg as integer    'wait state argument

  'these 3 items are only current/correct for inactive scripts. The active script's current
  'command is pointed to by the curcmd (ScriptCommand ptr) global, and copied here
  'when a script is stopped (either suspended, or interpretloop is left)
  curkind as integer    'kind of current statement
  curvalue as integer   'value of current statement
  curargc as integer    'number of args for current statement
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
  refcount as integer  'exists (and unused) in spr too, but using that one would be tacky
  spr as Frame ptr     '(uncached) could be a Frame, but sprite_delete doesn't like that
  anim(1) as TileAnimState
  tastuf(40) as integer
End Type

Type FontChar
	offset as integer  'offset into spr.image
	offx as byte   'pixel offsets
	offy as byte
	w as byte      'size of sprite
	h as byte
End Type

'spr has a refcount, which we use
'Pretend I inherited from Frame here
Type FontLayer
	spr as Frame
	chdata(255) as FontChar
End Type

Type Font
	sprite(1) as FontLayer ptr	'single layer fonts should use sprite(1) only
	w(255) as integer	'width of each character
	h as integer		'height of a line
	offset as XYPair	'added to coordinates when printing
	cols as integer		'number of used colours, not including colour 0 (transparency)
End Type

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
	number as integer 'positive to consume, negative to aquire
End Type

Type AttackData
	name as string
	description as string
	picture as integer
	pal as integer
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
	chain_to as integer 'ID +1
	chain_rate as integer
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
	sound_effect as integer ' ID + 1
	'----Bitsets----
	cure_instead_of_harm as integer
	divide_spread_damage as integer
	absorb_damage as integer
	unreversable_picture as integer
	can_steal_item as integer
	elemental_damage(7) as integer
	monster_type_bonus(7) as integer
	fail_vs_elemental(7) as integer
	fail_vs_monster_type(7) as integer
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
	Lev99 as stats
	spell_lists(3,23) as SpellList
	bits(2) as integer
	list_name(3) as string
	list_type(3) as integer
	have_tag as integer
	alive_tag as integer
	leader_tag as integer
	active_tag as integer
	max_name_len as integer
	hand_a_x as integer
	hand_a_y as integer
	hand_b_x as integer
	hand_b_y as integer
End Type

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
END TYPE

TYPE MouseArea
  x AS INTEGER
  y AS INTEGER
  w AS INTEGER
  h AS INTEGER
  hidecursor AS INTEGER
END TYPE

TYPE ToolInfoType
  name AS STRING
  icon AS STRING
  shortcut AS INTEGER
  cursor AS INTEGER
  areanum AS INTEGER
END TYPE

'yuck. FB has multidimensional arrays, why doesn't it let us utilise 'em? would like to write
'DIM defaults(2,160)
'loadpasdefaults defaults(i), foo
TYPE DefArray
 a(160) AS INTEGER  '161 elements required
END TYPE

'it's not possible to include utils.bi in here, because of compat.bi
#ifndef UTIL_BI
TYPE Stack
  pos as integer ptr
  bottom as integer ptr
  size as integer
END TYPE
#endif

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

TYPE SpriteSize 'FIXME: there are a lot of places this could be used where it isn't yet. See standard_sprite_load()
 name AS STRING
 size AS XYPair
 frames AS INTEGER
 genmax AS INTEGER 'Offset in gen() where max record index is stored
END TYPE

#ENDIF
