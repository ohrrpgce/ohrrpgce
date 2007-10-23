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

SUB SetNPCD(npcd AS NPCType, offset AS INTEGER, value AS INTEGER)
  IF offset >= 0 and offset <= 14 THEN
    (@npcd.picture)[offset] = value
  ELSE
    debug "Attempt to write NPC data out-of-range. offset=" + STR$(offset) + " value=" + STR$(value)
  END IF
END SUB

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

SUB SerInventory(invent() as InventSlot, z, buf())
  DIM i as integer, j as integer
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

SUB DeserInventory(invent() as InventSlot, z, buf())
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
    invent(i).text = temp$
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
		array(i).source = 0
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
 DIM i AS INTEGER
 WITH dat
  .name = ""
  .boxstyle = 0
  .textcolor = 0
  .maxrows = 0
  FOR i = 0 TO UBOUND(.items)
   ClearMenuItem(.items(i))
  NEXT i
 END WITH
END SUB

SUB ClearMenuItem(mi AS MenuDefItem)
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
  .bits = 0
 END WITH
END SUB

SUB LoadMenuData(menusfile AS STRING, menuitemfile AS STRING, dat AS MenuDef, record AS INTEGER)
 DIM f AS INTEGER
 IF record > gen(genMaxMenu) THEN
  ClearMenuData dat
  EXIT SUB
 END IF
 f = FREEFILE
 OPEN menusfile FOR BINARY AS #f
 SEEK #f, record * getbinsize(binMENUS) + 1
 WITH dat
  .name = ReadVStr(f, 20)
  .boxstyle = ReadShort(f)
  .textcolor = ReadShort(f)
  .maxrows = ReadShort(f)
 END WITH
 CLOSE #f
 LoadMenuItems menuitemfile, dat.items(), record
END SUB

SUB LoadMenuItems(menuitemfile AS STRING, mi() AS MenuDefItem, record AS INTEGER)
 DIM i AS INTEGER
 DIM f AS INTEGER
 DIM member AS INTEGER
 DIM elem AS INTEGER = 0
 f = FREEFILE
 OPEN menuitemfile FOR BINARY AS #f
 FOR i = 0 TO gen(genMaxMenuItem)
  SEEK #f, i * getbinsize(binMENUITEM) + 1
  member = ReadShort(f) - 1
  IF member = record THEN
   WITH mi(elem)
    .exists = (member >= 0)
    .member = member
    .caption = ReadVStr(f, 38)
    .sortorder = ReadShort(f)
    .t = ReadShort(f)
    .sub_t = ReadShort(f)
    .tag1 = ReadShort(f)
    .tag2 = ReadShort(f)
    .settag = ReadShort(f)
    .togtag = ReadShort(f)
    .bits = ReadShort(f)
   END WITH
   elem = elem + 1
   IF elem > UBOUND(mi) THEN EXIT FOR
  END IF
 NEXT i
 CLOSE #f
 SortMenuItems mi()
END SUB

SUB SaveMenuData(menusfile AS STRING, menuitemfile AS STRING, dat AS MenuDef, record AS INTEGER)
 DIM f AS INTEGER
 f = FREEFILE
 OPEN menusfile FOR BINARY AS #f
 SEEK #f, record * getbinsize(binMENUS) + 1
 WITH dat
  WriteVStr(f, 20, .name)
  WriteShort(f, -1, .boxstyle)
  WriteShort(f, -1, .textcolor)
  WriteShort(f, -1, .maxrows)
 END WITH
 CLOSE #f
 DIM i AS INTEGER
 SaveMenuItems menuitemfile, dat.items(), record
END SUB

SUB SaveMenuItems(menuitemfile AS STRING, mi() AS MenuDefItem, record AS INTEGER)
 DIM i AS INTEGER
 DIM f AS INTEGER
 DIM member AS INTEGER
 DIM elem AS INTEGER = 0
 DIM blankmi AS MenuDefItem
 f = FREEFILE
 OPEN menuitemfile FOR BINARY AS #f
 'Loop through each record and orphan all old entries for this menu
 FOR i = 0 TO gen(genMaxMenuItem)
  SEEK #f, i * getbinsize(binMENUITEM) + 1
  member = ReadShort(f) - 1
  IF member = record THEN
   SaveMenuItem f, blankmi, i
  END IF
 NEXT i
 'Loop through each record, writing new values into orphan slots
 FOR i = 0 TO gen(genMaxMenuItem)
  SEEK #f, i * getbinsize(binMENUITEM) + 1
  member = ReadShort(f) - 1
  IF member = -1 THEN
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
 SEEK #f, record * getbinsize(binMENUITEM) + 1
 WITH mi
  WriteShort(f, -1, .member)
  WriteVStr(f, 38, .caption)
  WriteShort(f, -1, .sortorder)
  WriteShort(f, -1, .t)
  WriteShort(f, -1, .sub_t)
  WriteShort(f, -1, .tag1)
  WriteShort(f, -1, .tag2)
  WriteShort(f, -1, .settag)
  WriteShort(f, -1, .togtag)
  WriteShort(f, -1, .bits)
 END WITH
END SUB

SUB SortMenuItems(mi() AS MenuDefItem)
 DIM AS INTEGER i, j, lowest, found
 FOR i = 0 TO UBOUND(mi)
  lowest = 32767
  found = -1
  FOR j = i TO UBOUND(mi)
   IF mi(j).sortorder < lowest AND mi(j).exists THEN
    lowest = mi(j).sortorder
    found = j
   END IF
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
