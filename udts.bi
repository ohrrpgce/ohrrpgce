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
end type

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
  extra1 as integer
  extra2 as integer
END TYPE

TYPE InventSlot
  used as integer	'use this to check if empty, not num!

  'following fields should not be used if used = 0
  id as integer		'absolute, not +1!!
  num as integer
  text as string	'text field which shows up in inventory, blank if empty
END TYPE

TYPE BattleSprite
  basex AS INTEGER
  basey AS INTEGER
  x AS INTEGER
  y AS INTEGER
  z AS INTEGER
  w AS INTEGER
  h AS INTEGER
  d AS INTEGER
  xmov AS INTEGER
  ymov AS INTEGER
  zmov AS INTEGER
  xspeed AS INTEGER
  yspeed AS INTEGER
  zspeed AS INTEGER
  vis AS INTEGER
  hero_untargetable AS INTEGER
  enemy_untargetable AS INTEGER
  dissolve AS INTEGER
  attack_succeeded AS INTEGER
  sprites as Frame ptr 'the graphic set
  sprite_num AS INTEGER 'how many frames
  frame AS INTEGER 'the current frame
  pal as palette16 ptr 'yeah
  deathtype AS INTEGER 'for enemies (0 = default, otherwise is type + 1)
  deathtime AS INTEGER '0 = default, otherwise is time + 1
  death_sfx AS INTEGER '0 = default, -1 = none, >0 = sfx ID + 1
END TYPE

UNION BattleStatsSingle
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
    poison AS INTEGER
    regen AS INTEGER
    stun AS INTEGER
    mute AS INTEGER
  END TYPE
  sta(15) AS INTEGER
End UNION

TYPE BattleStats
  cur AS BattleStatsSingle
  max AS BattleStatsSingle
END TYPE

TYPE Timer
  count as integer
  speed as integer
  ticks as integer
  trigger as integer
  flags as integer
  st as integer 'string, but both str and string are reserved
END TYPE

TYPE Plotstring
  s as string
  X as integer
  Y as integer
  col as integer
  bgcol as integer
  bits as integer
END TYPE

TYPE ScriptInst
  scrnum as integer     'slot number in script() array
  scrdata as integer ptr 'convenience pointer to script(.scrnum).ptr
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

TYPE ScriptData
  id as integer         'id number of script  (set to 0 to mark as unused slot)
  refcount as integer   'number of ScriptInst pointing to this data
  totaluse as integer   'total number of times this script has been requested since loading
  lastuse as integer
  ptr as integer ptr    'pointer to allocated memory
  size as integer       'amount the script takes up in the buffer
  vars as integer       'variable (including arguments) count
  args as integer       'number of arguments
  strtable as integer   'pointer to string table (offset from start of script data in long ints)
END TYPE

TYPE ScriptCommand
  kind as integer
  value as integer
  argc as integer
  args(999) as integer
END TYPE

UNION RGBcolor
	as uinteger col
		TYPE
			as ubyte b, g, r, a
		END TYPE
END UNION

TYPE TileCloneBuffer
 exists AS INTEGER
 buf(19,19) AS UBYTE
 size AS XYPair
END TYPE

TYPE TileEditState
 x AS INTEGER
 y AS INTEGER
 tilex AS INTEGER
 tiley AS INTEGER
 gotmouse AS INTEGER
 drawcursor AS INTEGER
 tool AS INTEGER
 curcolor AS INTEGER
 hidemouse AS INTEGER
 airsize AS INTEGER
 mist AS INTEGER
 undo AS INTEGER
 allowundo AS INTEGER
 zone AS INTEGER
 justpainted AS INTEGER
 hold AS INTEGER
 hox AS INTEGER
 hoy AS INTEGER
 cutfrom AS INTEGER
 canpaste AS INTEGER
 delay AS INTEGER
 fixmouse AS INTEGER
 readjust AS INTEGER
 adjustpos AS XYPair
END TYPE

TYPE SpriteEditState
 '--sprite set state
 framenum AS INTEGER
 
 '--sprite editor state
 x AS INTEGER
 y AS INTEGER
 lastpos AS XYPair
 zonenum AS INTEGER
 zone AS XYPair
 zonecursor AS INTEGER
 gotmouse AS INTEGER
 drawcursor AS INTEGER
 tool AS INTEGER
 curcolor AS INTEGER
 hidemouse AS INTEGER
 airsize AS INTEGER
 mist AS INTEGER
 hold AS INTEGER
 holdpos AS XYPair
 radius AS INTEGER
 squish AS XYPair '--For non-round elipses. Not implemented yet
 undodepth AS INTEGER
 undoslot AS INTEGER
 undomax AS INTEGER
 fixmouse AS INTEGER
 movespeed AS INTEGER
 readjust AS INTEGER
 adjustpos AS XYPair
END TYPE

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
  choice_enabled AS INTEGER
  no_box      AS INTEGER
  translucent AS INTEGER
  restore_music AS INTEGER
  choice(1)   AS STRING
  choice_tag  AS INTEGER
  vertical_offset AS INTEGER ' in 4-pixel increments
  box_shrink  AS INTEGER     ' in 4-pixel increments
  textcolor   AS INTEGER     ' 0=default
  boxstyle    AS INTEGER
  backdrop    AS INTEGER     ' +1
  music       AS INTEGER     ' +1
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

'Documentation of veh() in game, which is different from the VEH lump
'0 is true (-1) if in/mounting/dismounting a vehicle
'1-4 unused
'5 is the npc ref of the vehicle
'6 contains (a second set of) bitsets describing what the vehicle is doing
'veh(6)==0 is checked to see if something vehicle related is happening
''bit 0 scrambling/mounting
''bit 1 rising
''bit 2 falling
''bit 3 initiate dismount
''bit 4 clear - set to clean up to officially end vehicle use
''bit 5 ahead - set while getting off (dismount ahead only)
'7 remembers the speed of the leader 
'8-21 are copied from VEH


#ENDIF
