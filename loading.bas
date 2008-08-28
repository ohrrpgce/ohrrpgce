'OHRRPGCE GAME&CUSTOM - Routines for loading data
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#include "udts.bi"
#include "compat.bi"
#include "const.bi"
#include "common.bi"
#include "loading.bi"
#include "allmodex.bi"

option explicit

SUB LoadNPCD(file as string, dat() as NPCType)
  DIM i AS INTEGER, j AS INTEGER, f AS INTEGER
  f = FREEFILE
  OPEN file FOR BINARY AS #f
  SEEK #f, 8

  FOR i = 0 TO npcdMax
    FOR j = 0 TO 14
      SetNPCD(dat(i), j, ReadShort(f, -1))
    NEXT
  NEXT

  CLOSE #f

  FOR i = 0 TO npcdMax
    IF dat(i).speed = 3 THEN dat(i).speed = 10
  NEXT i
END SUB

'Prefer write_npc_int instead in the future, as it lacks pointer thoughtcrime
SUB SetNPCD(npcd AS NPCType, offset AS INTEGER, value AS INTEGER)
  IF offset >= 0 and offset <= 14 THEN
    (@npcd.picture)[offset] = value
  ELSE
    debug "Attempt to write NPC data out-of-range. offset=" + STR$(offset) + " value=" + STR$(value)
  END IF
END SUB

'Prefer read_npc_int instead in the future, as it lacks pointer thoughtcrime
FUNCTION GetNPCD(npcd AS NPCType, offset AS INTEGER) AS INTEGER
  IF offset >= 0 and offset <= 14 THEN
    RETURN (@npcd.picture)[offset]
  ELSE
    debug "Attempt to read NPC data out-of-range. offset=" + STR$(offset)
  END IF
END FUNCTION

SUB CleanNPCD(dat() as NPCType)
  DIM i AS INTEGER, j AS INTEGER

  FOR i = 0 TO npcdMax
    FOR j = 0 TO 14
      SetNPCD(dat(i), j, 0)
    NEXT
  NEXT
END SUB

SUB LoadNPCL(file as string, dat() as NPCInst, num as integer)
  DIM i AS INTEGER, f AS INTEGER
  REDIM dat(num - 1) as NPCInst
  f = FREEFILE
  OPEN file FOR BINARY AS #f
  seek #f,8
  FOR i = 0 to num - 1
    dat(i).x = ReadShort(f,-1) * 20
  NEXT
  FOR i = 0 to num - 1
    dat(i).y = (ReadShort(f,-1) - 1) * 20
  NEXT
  FOR i = 0 to num - 1
    dat(i).id = ReadShort(f,-1)
  NEXT
  FOR i = 0 to num - 1
    dat(i).dir = ReadShort(f,-1)
  NEXT
  FOR i = 0 to num - 1
    dat(i).frame = ReadShort(f,-1)
  NEXT
  FOR i = 0 TO num - 1
    dat(i).xgo = 0
    dat(i).ygo = 0
  NEXT
  CLOSE #f
END SUB

SUB SerNPCL(npc() as NPCInst, z, buffer(), num as integer, xoffset as integer, yoffset as integer)
  DIM i as integer
  FOR i = 0 to num - 1
    buffer(z) = npc(i).x - xoffset: z = z + 1
  NEXT
  FOR i = 0 to num - 1
    buffer(z) = npc(i).y - yoffset : z = z + 1
  NEXT
  FOR i = 0 to num - 1
    buffer(z) = npc(i).id: z = z + 1
  NEXT
  FOR i = 0 to num - 1
    buffer(z) = npc(i).dir: z = z + 1
  NEXT
  FOR i = 0 to num - 1
    buffer(z) = npc(i).frame: z = z + 1
  NEXT
  FOR i = 0 to num - 1
    buffer(z) = npc(i).xgo: z = z + 1
  NEXT
  FOR i = 0 to num - 1
    buffer(z) = npc(i).ygo: z = z + 1
  NEXT
END SUB

SUB DeserNPCL(npc() as NPCInst, z, buffer(), num as integer, xoffset as integer, yoffset as integer)
  DIM i as integer
  FOR i = 0 to num - 1
    npc(i).x = buffer(z) + xoffset: z = z + 1
  NEXT
  FOR i = 0 to num - 1
    npc(i).y = buffer(z) + yoffset: z = z + 1
  NEXT
  FOR i = 0 to num - 1
    npc(i).id = buffer(z): z = z + 1
  NEXT
  FOR i = 0 to num - 1
    npc(i).dir = buffer(z): z = z + 1
  NEXT
  FOR i = 0 to num - 1
    npc(i).frame = buffer(z): z = z + 1
  NEXT
  FOR i = 0 to num - 1
    npc(i).xgo = buffer(z): z = z + 1
  NEXT
  FOR i = 0 to num - 1
    npc(i).ygo = buffer(z): z = z + 1
  NEXT
END SUB

SUB CleanNPCL(dat() as NPCInst, num as integer)
  DIM i as integer
  FOR i = 0 to num - 1
    dat(i).x = 0
    dat(i).y = 0
    dat(i).id = 0
    dat(i).dir = 0
    dat(i).frame = 0
    dat(i).xgo = 0
    dat(i).ygo = 0
  NEXT
END SUB

SUB SerInventory8Bit(invent() as InventSlot, z, buf())
  DIM i as integer, j as integer
  buf(z) = 1 'Instruct new versions of game to ignore all this junk and use the 16-bit data instead
  '...but go ahead and write the 8-bit data so that loading a new SAV in an old version of game
  '   will not result in a nuked inventory
  z += 3 ' disregard some jibba jabba
  FOR i = 0 to inventoryMax
    IF invent(i).used THEN
      buf(z) = (invent(i).num AND 255) shl 8 OR ((invent(i).id + 1) AND 255)
    ELSE
      buf(z) = 0
    END IF
    z += 1
  NEXT
  z += 2  'slots 198 and 199 not useable
  z += 3 * 12
  FOR i = 0 to inventoryMax
    IF invent(i).used = 0 THEN invent(i).text = SPACE$(11)
    'unfortunately, this isn't exactly the badbinstring format
    FOR j = 0 TO 11
     'actually max length is 11, last byte always wasted
      IF j < LEN(invent(i).text) THEN buf(z) = invent(i).text[j] ELSE buf(z) = 0
      z += 1
    NEXT
  NEXT
  z += 2 * 12
END SUB

SUB DeserInventory8Bit(invent() as InventSlot, z, buf())
  DIM i as integer, j as integer, temp as string
  z += 3
  FOR i = 0 TO inventoryMax
    invent(i).num = buf(z) shr 8
    invent(i).id = (buf(z) and 255) - 1
    invent(i).used = invent(i).id >= 0
    z += 1
  NEXT
  z += 2
  z += 3 * 12
  FOR i = 0 TO inventoryMax
    temp = ""
    FOR j = 0 TO 11
      IF buf(z) > 0 AND buf(z) <= 255 THEN temp = temp + CHR$(buf(z))
      z += 1
    NEXT j
    '--Don't bother actually using the stored string. it is rebuilt later with rebuild_inventory_captions()
    'invent(i).text = temp$
  NEXT
  z += 2 * 12
END SUB

SUB CleanInventory(invent() as InventSlot)
  DIM i as integer
  FOR i = 0 TO inventoryMax
    invent(i).used = 0
    invent(i).text = SPACE$(11)
  NEXT
END SUB

SUB SaveInventory16bit(invent() AS InventSlot, BYREF z AS INTEGER, buf())
  DIM i AS INTEGER
  FOR i = 0 TO inventoryMax
    WITH invent(i)
      IF .used THEN
        buf(z) = .id
        buf(z+1) = .num
      ELSE
        buf(z) = -1
        buf(z+1) = 0
      END IF
    END WITH
    z += 2
  NEXT i
END SUB

SUB LoadInventory16Bit(invent() AS InventSlot, BYREF z AS INTEGER, buf())
  DIM i AS INTEGER
  FOR i = 0 TO inventoryMax
    WITH invent(i)
      .num = buf(z+1)
      IF .num > 0 THEN
        .used = YES
        .id = buf(z)
      ELSE
        'empty slot
        .used = NO
        .id = buf(z)
        .num = 0
      END IF
    END WITH
    z += 2
  NEXT i
END SUB

SUB LoadTiledata(filename as string, array() as integer, byval numlayers as integer, byref wide as integer, byref high as integer)
  'resize array and attempt to read numlayers of tile data, if that many are not present, default to 1 layer (blank out the rest)
  DIM AS INTEGER fh, i
  fh = FREEFILE
  OPEN filename$ FOR BINARY AS #fh
  SEEK #fh, 8
  wide = Readshort(fh, -1)
  high = ReadShort(fh, -1)
  REDIM array(1 + (numlayers * wide * high + 1) \ 2)
  IF LOF(fh) < 7 + 4 + numlayers * wide * high THEN numlayers = 1
  DIM temparray(1 + (numlayers * wide * high + 1) \ 2) AS SHORT
  GET #fh, 8, temparray()    'handles odd bytes
  FOR i = 0 TO UBOUND(temparray)
   array(i) = temparray(i)
  NEXT
  CLOSE #fh
END SUB

SUB SaveTiledata(filename as string, array() as integer, byval numlayers as integer)
  DIM AS INTEGER wide, high
  wide = array(0)
  high = array(1)
  xbsave filename, array(), 4 + numlayers * wide * high
END SUB

SUB CleanTiledata(array() as integer, wide as integer, high as integer, numlayers as integer)
  'aka AllocateTiledata
  REDIM array(1 + (numlayers * wide * high + 1) \ 2)
  array(0) = wide
  array(1) = high
END SUB

SUB DeserDoorLinks(filename as string, array() as doorlink)
	dim as integer hasheader = -1, f, i
	'when we strip the header, we can check for its presence here

	if not fileisreadable(filename) then
		debug "couldn't load " & filename
		exit sub
	end if
	
	f = freefile
	open filename for binary as #f
	
	
	if hasheader then 
		dim stupid(6) as ubyte
		get #f,, stupid()
	end if
		
	for i = 0 to 199
		array(i).source = ReadShort(f)
	next
	for i = 0 to 199
		array(i).dest = ReadShort(f)
	next
	for i = 0 to 199
		array(i).dest_map = ReadShort(f)
	next
	for i = 0 to 199
		array(i).tag1 = ReadShort(f)
	next
	for i = 0 to 199
		array(i).tag2 = ReadShort(f)
	next
	
	close #f
End SUB

Sub SerDoorLinks(filename as string, array() as doorlink, withhead as integer = -1)
	dim as integer f = freefile, i
	
	if not fileiswriteable(filename) then exit sub
	
	safekill(filename)
	
	open filename for binary as #f
	
	if withhead then
		dim stupid as ubyte = 253
		put #f, , stupid
		writeshort f, -1, -26215 '&h9999, signed
		writeshort f, -1, 0
		writeshort f, -1, 2000
	end if
	
	
	for i = 0 to 199
		WriteShort f, -1, array(i).source
	next
	for i = 0 to 199
		WriteShort f, -1, array(i).dest
	next
	for i = 0 to 199
		WriteShort f, -1, array(i).dest_map
	next
	for i = 0 to 199
		WriteShort f, -1, array(i).tag1
	next
	for i = 0 to 199
		WriteShort f, -1, array(i).tag2
	next
	
	close #f
end sub

sub CleanDoorLinks(array() as doorlink)
	dim i as integer
	for i = lbound(array) to ubound(array)
		array(i).source = -1
		array(i).dest = 0
		array(i).dest_map = 0
		array(i).tag1 = 0
		array(i).tag2 = 0
	next
end sub

Sub DeSerDoors(filename as string, array() as door, record as integer)
	if not fileisreadable(filename) then exit sub
	
	dim as integer f = freefile, i
	
	open filename for binary as #f
	
	seek #f, record * 600 + 1
	
	for i = 0 to 99
		array(i).x = readshort(f)
	next
	for i = 0 to 99
		array(i).y = readshort(f)
	next
	for i = 0 to 99
		array(i).bits(0) = readshort(f)
	next
	
	close #f
End Sub

Sub SerDoors(filename as string, array() as door, record as integer)
	if not fileiswriteable(filename) then exit sub
	dim as integer f = freefile, i
	
	open filename for binary as #f
	
	seek #f, record * 600 + 1
	
	for i = 0 to 99
		writeshort f, -1, array(i).x
	next
	for i = 0 to 99
		writeshort f, -1, array(i).y
	next
	for i = 0 to 99
		writeshort f, -1, array(i).bits(0)
	next
	
	close #f
	
End Sub

Sub CleanDoors(array() as door)
	dim i as integer
	for i = lbound(array) to ubound(array)
		array(i).x = 0
		array(i).y = 0
		array(i).bits(0) = 0
	next
end sub

'loads a standard block of stats from a file handle.
Sub LoadStats(fh as integer, sta as stats ptr)
	dim i as integer
	if sta = 0 then exit sub
	
	with *sta
		for i = 0 to 11
			.sta(i) = readShort(fh)
		next i
	end with
	
end sub

'saves a stat block to a file handle
Sub SaveStats(fh as integer, sta as stats ptr)
	dim i as integer
	if sta = 0 then exit sub
	
	with *sta
		for i = 0 to 11
			writeShort(fh, -1, .sta(i))
		next i
	end with
	
end sub

'this differs from the above because it loads two interleaved blocks of stats,
'such as those found in the hero definitions.
Sub LoadStats2(fh as integer, lev0 as stats ptr, lev99 as stats ptr)
	dim as integer i
	if lev0 = 0 or lev99 = 0 then exit sub
	for i = 0 to 11
		lev0->sta(i) = readShort(fh)
		lev99->sta(i) = readShort(fh)
	next i
end sub

'save interleaved stat blocks
Sub SaveStats2(fh as integer, lev0 as stats ptr, lev99 as stats ptr)
	if lev0 = 0 or lev99 = 0 then exit sub
	dim as integer i
	for i = 0 to 11
		writeShort(fh,-1,lev0->sta(i))
		writeShort(fh,-1,lev99->sta(i))
	next i
end sub

Sub DeSerHeroDef(filename as string, hero as herodef ptr, record as integer)
	if record < 0 or record > gen(genMaxHero) then debug "DeSerHeroDef: fail on record:" & record : exit sub
	if not fileisreadable(filename) or hero = 0 then exit sub
	
	dim as integer f = freefile, i, j
	
	open filename for binary as #f
	
	seek #f, record * 636 + 1
	
	'begin (this makes the baby jesus cry :'( )
	with *hero
		.name							= readvstr(f, 16)
		.sprite						= readshort(f)
		.sprite_pal				= readshort(f)
		.walk_sprite			= readshort(f)
		.walk_sprite_pal	= readshort(f)
		.def_level				= readshort(f)
		.def_weapon				= readshort(f)
		LoadStats2(f, @.Lev0, @.Lev99)
		'get #f,, .spell_lists()
		for i = 0 to 3
			for j = 0 to 23 'have to do it this way in case FB reads arrays the wrong way
				.spell_lists(i,j).attack = readshort(f)
				.spell_lists(i,j).learned = readshort(f)
			next
		next
		readshort(f) 'unused
		for i = 0 to 2
			.bits(i) = readShort(f)
		next
		for i = 0 to 3
			.list_name(i) = ReadVStr(f,10)
		next
		readshort(f) 'unused
		for i = 0 to 3
			.list_type(i) = readshort(f)
		next
		.have_tag = readshort(f)
		.alive_tag = readshort(f)
		.leader_tag = readshort(f)
		.active_tag = readshort(f)
		.max_name_len = readshort(f)
		.hand_a_x = readshort(f)
		.hand_a_y = readshort(f)
		.hand_b_x = readshort(f)
		.hand_b_y = readshort(f)
		'16 more unused bytes
		
	end with
	
	close #f
end sub

Sub SerHeroDef(filename as string, hero as herodef ptr, record as integer)
	if not fileiswriteable(filename) or hero = 0 then exit sub
	
	dim as integer f = freefile, i, j
	
	open filename for binary as #f
	
	seek #f, record * 636 + 1
	
	'begin (this makes the baby jesus cry :'( )
	with *hero
		writevstr(f,16,.name)
		writeshort(f,-1,.sprite)
		writeshort(f,-1,.sprite_pal)
		writeshort(f,-1,.walk_sprite)
		writeshort(f,-1,.walk_sprite_pal)
		writeshort(f,-1,.def_level)
		writeshort(f,-1,.def_weapon)
		SaveStats2(f, @.Lev0, @.Lev99)
		'get #f,, .spell_lists()
		for i = 0 to 3
			for j = 0 to 23 'have to do it this way in case FB reads arrays the wrong way
				writeshort(f,-1,.spell_lists(i,j).attack)
				writeshort(f,-1,.spell_lists(i,j).learned)
			next
		next
		writeshort(f,-1,0) 'unused
		for i = 0 to 2
			writeshort(f,-1,.bits(i))
		next
		for i = 0 to 3
			WriteVStr(f,10, .list_name(i))
		next
		writeshort(f,-1,0) 'unused
		for i = 0 to 3
			writeshort(f,-1,.list_type(i))
		next
		writeshort(f,-1,.have_tag)
		writeshort(f,-1,.alive_tag)
		writeshort(f,-1,.leader_tag)
		writeshort(f,-1,.active_tag)
		writeshort(f,-1,.max_name_len)
		writeshort(f,-1,.hand_a_x)
		writeshort(f,-1,.hand_a_y)
		writeshort(f,-1,.hand_b_x)
		writeshort(f,-1,.hand_b_y)
		'16 more unused bytes
		
	end with
	
	close #f
end sub

SUB ClearMenuData(dat AS MenuDef)
 DIM bits(0) AS INTEGER
 WITH dat
  .record = -1
  .handle = 0
  .name = ""
  .boxstyle = 0
  .textcolor = 0
  .maxrows = 0
  .offset.x = 0
  .offset.y = 0
  .anchor.x = 0
  .anchor.y = 0
  .align = 0
  .min_chars = 0
  .max_chars = 0
  .bordersize = 0
  ClearMenuItems dat
 END WITH
 bits(0) = 0
 MenuBitsFromArray dat, bits()
END SUB

SUB ClearMenuItems(menu AS MenuDef)
 DIM i AS INTEGER
 WITH menu
  FOR i = 0 TO UBOUND(.items)
   ClearMenuItem(.items(i))
  NEXT i
 END WITH
END SUB

SUB ClearMenuItem(mi AS MenuDefItem)
 DIM bits(0) AS INTEGER
 DIM i AS INTEGER
 WITH mi
  .exists = 0
  .member = 0
  .caption = ""
  .sortorder = 0
  .t = 0
  .sub_t = 0
  .tag1 = 0
  .tag2 = 0
  .settag = 0
  .togtag = 0
  FOR i = 0 TO 2
   .extra(i) = 0
  NEXT i
 END WITH
 bits(0) = 0
 MenuItemBitsFromArray mi, bits()
END SUB

SUB LoadMenuData(menu_set AS MenuSet, dat AS MenuDef, record AS INTEGER, ignore_items AS INTEGER=NO)
 DIM f AS INTEGER
 DIM bits(0) AS INTEGER
 IF record > gen(genMaxMenu) OR record < 0 THEN
  ClearMenuData dat
  EXIT SUB
 END IF
 f = FREEFILE
 OPEN menu_set.menufile FOR BINARY AS #f
 SEEK #f, record * getbinsize(binMENUS) + 1
 WITH dat
  .record = record
  .name = ReadByteStr(f, 20)
  .boxstyle = ReadShort(f)
  .textcolor = ReadShort(f)
  .maxrows = ReadShort(f)
  bits(0) = ReadShort(f)
  MenuBitsFromArray dat, bits()
  .offset.x = ReadShort(f)
  .offset.y = ReadShort(f)
  .anchor.x = ReadShort(f)
  .anchor.y = ReadShort(f)
  .align = ReadShort(f)
  .min_chars = ReadShort(f)
  .max_chars = ReadShort(f)
  .bordersize = ReadShort(f)
 END WITH
 CLOSE #f
 IF ignore_items = NO THEN 'This is disableable for performance when all you care about loading is the menu's name
  LoadMenuItems menu_set, dat.items(), record
 END IF
END SUB

SUB LoadMenuItems(menu_set AS MenuSet, mi() AS MenuDefItem, record AS INTEGER)
 DIM i AS INTEGER
 DIM f AS INTEGER
 DIM member AS INTEGER
 DIM elem AS INTEGER = 0
 DIM actual_record_count AS INTEGER = 0

 FOR i = 0 TO UBOUND(mi)
  ClearMenuItem mi(i)
 NEXT i

 f = FREEFILE
 OPEN menu_set.itemfile FOR BINARY AS #f
 actual_record_count = LOF(f) / getbinsize(binMENUITEM)
 IF actual_record_count <> gen(genMaxMenuItem) + 1 THEN
  debug "menuitem.bin record count sanity check failed " & gen(genMaxMenuItem) & "->" & actual_record_count - 1
  gen(genMaxMenuItem) = actual_record_count - 1
 END IF
 FOR i = 0 TO gen(genMaxMenuItem)
  SEEK #f, i * getbinsize(binMENUITEM) + 1
  member = ReadShort(f)
  IF member = record + 1 THEN
   LoadMenuItem f, mi(elem), i
   elem = elem + 1
   IF elem > UBOUND(mi) THEN EXIT FOR
  END IF
 NEXT i
 CLOSE #f
 SortMenuItems mi()
END SUB

SUB LoadMenuItem(f AS INTEGER, BYREF mi AS MenuDefItem, record AS INTEGER)
 DIM i AS INTEGER
 DIM bits(0) AS INTEGER
 SEEK #f, record * getbinsize(binMENUITEM) + 1
 WITH mi
  .member = ReadShort(f)
  .exists = (.member >= 0)
  .caption = ReadByteStr(f, 38)
  .sortorder = ReadShort(f)
  .t = ReadShort(f)
  .sub_t = ReadShort(f)
  .tag1 = ReadShort(f)
  .tag2 = ReadShort(f)
  .settag = ReadShort(f)
  .togtag = ReadShort(f)
  bits(0) = ReadShort(f)
  FOR i = 0 TO 2
   .extra(i) = ReadShort(f)
  NEXT i
 END WITH
 MenuItemBitsFromArray mi, bits()
END SUB

SUB SaveMenuData(menu_set AS MenuSet, dat AS MenuDef, record AS INTEGER)
 DIM f AS INTEGER
 DIM bits(0) AS INTEGER
 f = FREEFILE
 OPEN menu_set.menufile FOR BINARY AS #f
 SEEK #f, record * getbinsize(binMENUS) + 1
 WITH dat
  WriteByteStr(f, 20, .name)
  WriteShort(f, -1, .boxstyle)
  WriteShort(f, -1, .textcolor)
  WriteShort(f, -1, .maxrows)
  MenuBitsToArray dat, bits()
  WriteShort(f, -1, bits(0))
  WriteShort(f, -1, .offset.x)
  WriteShort(f, -1, .offset.y)
  WriteShort(f, -1, .anchor.x)
  WriteShort(f, -1, .anchor.y)
  WriteShort(f, -1, .align)
  WriteShort(f, -1, .min_chars)
  WriteShort(f, -1, .max_chars)
  WriteShort(f, -1, .bordersize)
 END WITH
 CLOSE #f
 DIM i AS INTEGER
 SaveMenuItems menu_set, dat.items(), record
END SUB

SUB SaveMenuItems(menu_set AS MenuSet, mi() AS MenuDefItem, record AS INTEGER)
 DIM i AS INTEGER
 DIM f AS INTEGER
 DIM member AS INTEGER
 DIM elem AS INTEGER = 0
 DIM blankmi AS MenuDefItem

 FOR i = 0 TO UBOUND(mi)
  'Force all items to use the correct member and sortorder
  WITH mi(i)
   IF .exists THEN
    .member = record + 1
    .sortorder = i
   END IF
  END WITH
 NEXT i
 
 f = FREEFILE
 OPEN menu_set.itemfile FOR BINARY AS #f
 'Loop through each record and orphan all old entries for this menu
 FOR i = 0 TO gen(genMaxMenuItem)
  SEEK #f, i * getbinsize(binMENUITEM) + 1
  member = ReadShort(f)
  IF member = record + 1 THEN
   SaveMenuItem f, blankmi, i
  END IF
 NEXT i
 'Loop through each record, writing new values into orphan slots
 FOR i = 0 TO gen(genMaxMenuItem)
  SEEK #f, i * getbinsize(binMENUITEM) + 1
  member = ReadShort(f)
  IF member = 0 THEN
   SaveMenuItem f, mi(elem), i
   elem = elem + 1
   IF elem > UBOUND(mi) THEN EXIT FOR
  END IF
 NEXT i
 DO WHILE elem <= UBOUND(mi)
  'More items need to be written, append them
  IF mi(elem).exists THEN
   gen(genMaxMenuItem) += 1
   SaveMenuItem f, mi(elem), gen(genMaxMenuItem)
  END IF
  elem += 1
 LOOP
 CLOSE #f
END SUB

SUB SaveMenuItem(f AS INTEGER, mi AS MenuDefItem, record AS INTEGER)
 DIM i AS INTEGER
 DIM bits(0) AS INTEGER
 SEEK #f, record * getbinsize(binMENUITEM) + 1
 WITH mi
  WriteShort(f, -1, .member)
  WriteByteStr(f, 38, .caption)
  WriteShort(f, -1, .sortorder)
  WriteShort(f, -1, .t)
  WriteShort(f, -1, .sub_t)
  WriteShort(f, -1, .tag1)
  WriteShort(f, -1, .tag2)
  WriteShort(f, -1, .settag)
  WriteShort(f, -1, .togtag)
  MenuItemBitsToArray mi, bits()
  WriteShort(f, -1, bits(0))
  FOR i = 0 TO 2
   WriteShort(f, -1, .extra(i))
  NEXT i
 END WITH
END SUB

SUB MenuBitsToArray (menu AS MenuDef, bits() AS INTEGER)
 bits(0) = 0
 WITH menu
  setbit bits(), 0, 0, .translucent
  setbit bits(), 0, 1, .no_scrollbar
  setbit bits(), 0, 2, .allow_gameplay
  setbit bits(), 0, 3, .suspend_player
  setbit bits(), 0, 4, .no_box
  setbit bits(), 0, 5, .no_close
  setbit bits(), 0, 6, .no_controls
  setbit bits(), 0, 7, .prevent_main_menu
 END WITH
END SUB

SUB MenuBitsFromArray (menu AS MenuDef, bits() AS INTEGER)
 WITH menu
  .translucent    = (readbit(bits(), 0, 0) <> 0)
  .no_scrollbar   = (readbit(bits(), 0, 1) <> 0)
  .allow_gameplay = (readbit(bits(), 0, 2) <> 0)
  .suspend_player = (readbit(bits(), 0, 3) <> 0)
  .no_box         = (readbit(bits(), 0, 4) <> 0)
  .no_close       = (readbit(bits(), 0, 5) <> 0)
  .no_controls    = (readbit(bits(), 0, 6) <> 0)
  .prevent_main_menu = (readbit(bits(), 0, 7) <> 0)
 END WITH
END SUB

SUB MenuItemBitsToArray (mi AS MenuDefItem, bits() AS INTEGER)
 bits(0) = 0
 WITH mi
  setbit bits(), 0, 0, .hide_if_disabled
  setbit bits(), 0, 1, .close_if_selected
 END WITH
END SUB

SUB MenuItemBitsFromArray (mi AS MenuDefItem, bits() AS INTEGER)
 WITH mi
  .hide_if_disabled = (readbit(bits(), 0, 0) <> 0)
  .close_if_selected = (readbit(bits(), 0, 1) <> 0)
 END WITH
END SUB

SUB SortMenuItems(mi() AS MenuDefItem)
 DIM AS INTEGER i, j, lowest, found
 FOR i = 0 TO UBOUND(mi)
  lowest = 32767
  found = -1
  FOR j = i TO UBOUND(mi)
   WITH mi(j)
    IF .sortorder < lowest AND .exists AND (NOT (.disabled AND .hide_if_disabled)) THEN
     lowest = .sortorder
     found = j
    END IF
   END WITH
  NEXT j
  IF found >= 0 THEN
   SWAP mi(i), mi(found)
  END IF
 NEXT i
END SUB

SUB LoadVehicle (file AS STRING, veh(), vehname$, record AS INTEGER)
 setpicstuf veh(), 80, -1
 loadset file, record, 0
 vehname$ = STRING$(bound(veh(0) AND 255, 0, 15), 0)
 array2str veh(), 1, vehname$
END SUB

SUB SaveVehicle (file AS STRING, veh(), vehname$, record AS INTEGER)
 veh(0) = bound(LEN(vehname$), 0, 15)
 str2array vehname$, veh(), 1
 setpicstuf veh(), 80, -1
 storeset file, record, 0
END SUB

SUB OldDefaultUIColors (colarray() AS INTEGER)
 'Default UI for Classic OHR master palette
 'for upgrading old games that lak an uilook.bin file
 DIM uidef(uiColors) = {0,7,8,14,15,6,7,1,2,18,21,35,37,15,240,10,14,240, _
        18,28,34,44,50,60,66,76,82,92,98,108,114,124,130,140, _
        146,156,162,172,178,188,194,204,210,220,226,236,242,252}
 DIM i AS INTEGER
 FOR i = 0 TO uiColors
  colarray(i) = uidef(i)
 NEXT
END SUB

SUB DefaultUIColors (colarray() AS INTEGER)
 'Default UI for NeoTA's new Master palette
 'for the filepicker before loading a game.
 DIM uidef(uiColors) = {0,144,80,110,240,102,144,244,215,242,67,212, _
                        215,240,0,220,110,0,242,40,211,221,83,90,182, _
                        173,100,159,115,60,132,156,98,105,195,204,70, _
                        66,217,210,87,82,108,232,54,116,48,160}
 DIM i AS INTEGER
 FOR i = 0 TO uiColors
  colarray(i) = uidef(i)
 NEXT
 'DIM hexstring AS STRING
 'FOR i = 0 TO uiColors
 ' hexstring += "&h" & hex(master(uidef(i)).col, 6) & ","
 'NEXT
 'debug "defaults: " & hexstring
END SUB

SUB GuessDefaultUIColors (colarray() AS INTEGER)
 DIM AS INTEGER fixeddefaults(uiColors) = {&h000000,&hA19CB0,&h4F595A,&hFFFC62,&hFFFFFF,&h8E6B00,&hA19CB0,_
       &h003B95,&h228B22,&h001D48,&h153289,&h154C15,&h228B22,&hFFFFFF,&h000000,&h6BEB61,&hFFFC62,&h000000,_
       &h001D48,&h8084D0,&h123D12,&h98FA90,&h500000,&hFF7F7F,&h4F7A54,&hD3F88E,&h5E4600,&hF1EA89,&h471747,_
       &hDF90FF,&h76352C,&hD3A560,&h2D2200,&hD7A100,&h4D3836,&hF6D2B6,&h2179D3,&h0E2059,&h3CB23A,&h0E300E,_
       &hBF0000,&h340000,&hFFDD30,&hCD8316,&h8236AC,&h5F1F5F,&h2F342E,&hBAABC1}
 DIM i AS INTEGER
 DIM temp AS RGBcolor
 FOR i = 0 TO uiColors
  temp.col = fixeddefaults(i)
  colarray(i) = nearcolor(master(), temp.r, temp.g, temp.b)
 NEXT
END SUB

SUB LoadUIColors (colarray() AS INTEGER, palnum AS INTEGER=-1)
 'load ui colors from data lump
 DIM filename AS STRING
 filename = workingdir & SLASH & "uicolors.bin"

 IF palnum < 0 OR palnum > gen(genMaxMasterPal) OR NOT isfile(filename) THEN
  DefaultUIColors colarray()
  EXIT SUB
 END IF

 DIM i AS INTEGER
 DIM f AS INTEGER
 f = FREEFILE
 OPEN filename FOR BINARY AS #f
 SEEK #f, palnum * getbinsize(binUICOLORS) + 1
 FOR i = 0 TO uiColors
  colarray(i) = ReadShort(f)
 NEXT i
 CLOSE f
END SUB

SUB SaveUIColors (colarray() AS INTEGER, palnum AS INTEGER)
 DIM filename AS STRING
 filename = workingdir & SLASH & "uicolors.bin"

 IF palnum < 0 OR palnum > gen(genMaxMasterPal) THEN
  debug "SaveUIColors: attempt to save colors out of range " & palnum
  EXIT SUB
 END IF

 DIM i AS INTEGER
 DIM f AS INTEGER
 f = FREEFILE
 OPEN filename FOR BINARY AS #f
 SEEK #f, palnum * getbinsize(binUICOLORS) + 1
 FOR i = 0 TO uiColors
  WriteShort f, -1, colarray(i)
 NEXT i
 CLOSE f
END SUB

SUB LoadTextBox (BYREF box AS TextBox, boxbuf() AS INTEGER, record AS INTEGER)
 IF UBOUND(boxbuf) < dimbinsize(binSAY) THEN debug "LoadTextBox: boxbuf too small:" & UBOUND(boxbuf) : EXIT SUB
 IF record < 0 OR record > gen(genMaxTextBox) THEN debug "LoadTextBox: invalid record: " & record : EXIT SUB

 DIM filename AS STRING
 filename = game & ".say"

 DIM i AS INTEGER
 DIM f AS INTEGER
 f = FREEFILE
 OPEN filename FOR BINARY ACCESS READ AS #f
 SEEK #f, record * getbinsize(binSAY) + 1
 FOR i = 0 TO 199
  boxbuf(i) = ReadShort(f)
 NEXT i
 CLOSE #f

 '--Populate TextBox object
 WITH box
  '--Load lines of text
  FOR i = 0 TO 7
   .text(i) = STRING(38, 0)
   array2str boxbuf(), i * 38, .text(i)
   .text(i) = RTRIM(.text(i), CHR(0)) '--Trim off any trailing ASCII zeroes
  NEXT i
  '--Gather conditional data
  '--transpose conditional data from its dumb-as-toast non-int-aligned location
  DIM condtemp AS STRING
  DIM condbuf(20) AS INTEGER
  condtemp = STRING(42, 0)
  array2str boxbuf(), 305, condtemp
  str2array condtemp, condbuf(), 0
  '--Get conditional data
  .instead_tag = bound(condbuf(0), -999, 999)
  .instead     = bound(condbuf(1), -32767, gen(genMaxTextbox))
  .settag_tag  = bound(condbuf(2), -999, 999)
  .settag1     = bound(condbuf(3), -999, 999)
  .settag2     = bound(condbuf(4), -999, 999)
  .battle_tag  = bound(condbuf(5), -999, 999)
  .battle      = bound(condbuf(6), 0, gen(genMaxFormation))
  .shop_tag    = bound(condbuf(7), -999, 999)
  .shop        = bound(condbuf(8), -32000, gen(genMaxShop) + 1)
  .hero_tag    = bound(condbuf(9), -999, 999)
  .hero_addrem = bound(condbuf(10), -99, 99)
  .after_tag   = bound(condbuf(11), -999, 999)
  .after       = bound(condbuf(12), -32767, gen(genMaxTextbox))
  .money_tag   = bound(condbuf(13), -999, 999)
  .money       = bound(condbuf(14), -32000, 32000)
  .door_tag    = bound(condbuf(15), -999, 999)
  .door        = bound(condbuf(16), 0, 199)
  .item_tag    = bound(condbuf(17), -999, 999)
  .item        = bound(condbuf(18), -gen(genMaxItem) - 1, gen(genMaxItem) + 1)
  .hero_swap   = bound(condbuf(19), -99, 99)
  .hero_lock   = bound(condbuf(20), -99, 99)
  '--Get box bitsets
  .choice_enabled = xreadbit(boxbuf(), 0, 174)
  .no_box         = xreadbit(boxbuf(), 1, 174)
  .opaque         = xreadbit(boxbuf(), 2, 174)
  .restore_music  = xreadbit(boxbuf(), 3, 174)
  '--Get choicebox data
  FOR i = 0 TO 1
   .choice(i) = STRING(15, 0)
   array2str boxbuf(), 349 + (i * 18), .choice(i)
   .choice(i) = RTRIM(.choice(i), CHR(0)) 'Trim off trailing ASCII zeroes
   .choice_tag(i) = boxbuf(182 + (i * 9))
  NEXT i
  '--Get box appearance
  .vertical_offset = boxbuf(193)
  .shrink          = boxbuf(194)
  .textcolor       = boxbuf(195) ' 0=default
  .boxstyle        = boxbuf(196)
  .backdrop        = boxbuf(197) ' +1
  .music           = boxbuf(198) ' +1
 END WITH
END SUB

SUB SaveTextBox (BYREF box AS TextBox, boxbuf() AS INTEGER, record AS INTEGER)
 IF UBOUND(boxbuf) < dimbinsize(binSAY) THEN debug "SaveTextBox: boxbuf too small:" & UBOUND(boxbuf) : EXIT SUB
 IF record < 0 OR record > gen(genMaxTextBox) THEN debug "SaveTextBox: invalid record: " & record : EXIT SUB

 DIM filename AS STRING
 filename = game & ".say"

 DIM i AS INTEGER

 'FIXME: not all elements are saved here yet. They will be added as direct boxbuf() access is phased out
 WITH box
  '--Transcribe lines of text into the buffer
  FOR i = 0 TO 7
   WHILE LEN(.text(i)) < 38: .text(i) = .text(i) & CHR(0): WEND
   str2array .text(i), boxbuf(), i * 38
  NEXT i
  '--Transcribe conditional data
  DIM condbuf(20) AS INTEGER
  condbuf(0) = .instead_tag
  condbuf(1) = .instead
  condbuf(2) = .settag_tag
  condbuf(3) = .settag1
  condbuf(4) = .settag2
  condbuf(5) = .battle_tag
  condbuf(6) = .battle
  condbuf(7) = .shop_tag
  condbuf(8) = .shop
  condbuf(9) = .hero_tag
  condbuf(10) = .hero_addrem
  condbuf(11) = .after_tag
  condbuf(12) = .after
  condbuf(13) = .money_tag
  condbuf(14) = .money
  condbuf(15) = .door_tag
  condbuf(16) = .door
  condbuf(17) = .item_tag
  condbuf(18) = .item
  condbuf(19) = .hero_swap
  condbuf(20) = .hero_lock
  DIM condtemp AS STRING
  condtemp = STRING(42, 0)
  array2str condbuf(), 0, condtemp
  str2array condtemp, boxbuf(), 305
  
 END WITH

 DIM f AS INTEGER
 f = FREEFILE
 OPEN filename FOR BINARY AS #f
 SEEK #f, record * getbinsize(binSAY) + 1
 FOR i = 0 TO 199
  WriteShort f, -1, boxbuf(i)
 NEXT i
 CLOSE #f
END SUB
