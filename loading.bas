'OHRRPGCE GAME&CUSTOM - Routines for loading data
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#include "compat.bi"
#include "udts.bi"
#include "const.bi"
#include "common.bi"
#include "loading.bi"
#include "allmodex.bi"

option explicit

DECLARE SUB LoadMenuItems(menu_set AS MenuSet, dat AS MenuDef, record AS INTEGER)
DECLARE SUB LoadMenuItem(f AS INTEGER, items() AS MenuDefItem ptr, record AS INTEGER)
DECLARE SUB SaveMenuItems(menu_set AS MenuSet, dat AS MenuDef, record AS INTEGER)
DECLARE SUB SaveMenuItem(f AS INTEGER, mi AS MenuDefItem, record AS INTEGER, menunum AS INTEGER, itemnum AS INTEGER)

'There are two versions of LoadNPCD:
'LoadNPCD(file, dat()):
'  (Normal version) dat is variable length (already initialised!!), redimmed to the correct length by LoadNPCD
'LoadNPCD_fixedlen(file, dat(), arraylen):
'  dat is a fixed length (0 TO max_npc_def), and the REAL length is returned in arraylen

PRIVATE SUB LoadNPCD_internal (file as string, dat() as NPCType, byref arraylen as integer, byval resize as integer)
  DIM as integer i, j, f

  'It's still up to you to actually load the new sprites, but "do not pass
  'arrays holding frame pointers" would be a tricky catch
  FOR i = 0 TO UBOUND(dat)
   WITH dat(i)
    IF .sprite THEN frame_unload @.sprite
    IF .pal THEN palette16_unload @.pal
   END WITH
  NEXT i

  f = FREEFILE
  OPEN file FOR BINARY ACCESS READ AS #f
  SEEK #f, 8

  arraylen = (LOF(f) - 7) \ getbinsize(binN)
  IF resize THEN REDIM dat(arraylen - 1)
  DIM as integer recordlen = getbinsize(binN) \ 2
  DIM as integer buf(recordlen - 1)

  FOR i = 0 TO arraylen - 1
    loadrecord buf(), f, recordlen
    FOR j = 0 TO recordlen - 1
      SetNPCD(dat(i), j, buf(j))
    NEXT
    IF dat(i).speed = 3 THEN dat(i).speed = 10
  NEXT

  CLOSE #f
END SUB

SUB LoadNPCD_fixedlen(file as string, dat() as NPCType, byref arraylen as integer)
  IF UBOUND(dat) <> max_npc_defs - 1 THEN
    fatalerror "Programmer error! LoadNPCD: dat() length " & UBOUND(dat)
  END IF
  LoadNPCD_internal(file, dat(), arraylen, NO)
END SUB

SUB LoadNPCD(file as string, dat() as NPCType)
  LoadNPCD_internal(file, dat(), 0, YES)  'dummy arraylen
END SUB

'As for LoadNPCD, there are two versions of SaveNPCD. See comment above

SUB SaveNPCD_fixedlen(file as string, dat() as NPCType, byval arraylen as integer)
  DIM AS INTEGER i, j, f

  'We want to truncate the file to the right length
  safekill file

  f = FREEFILE
  OPEN file FOR BINARY AS #f
  SEEK #f, 8

  DIM as integer recordlen = getbinsize(binN) \ 2
  DIM as integer buf(recordlen - 1)

  FOR i = 0 TO arraylen - 1
    FOR j = 0 TO recordlen - 1
      IF j = 3 AND dat(i).speed = 10 THEN
        '--Special case for speed = 10 (gets stored as 3)
        buf(j) = 3
      ELSE
        buf(j) = GetNPCD(dat(i), j)
      END IF
    NEXT
    storerecord buf(), f, recordlen
  NEXT

  CLOSE #f
END SUB

SUB SaveNPCD(file as string, dat() as NPCType)
  SaveNPCD_fixedlen(file, dat(), UBOUND(dat) + 1)
END SUB

'Prefer write_npc_int instead in the future, as it lacks pointer thoughtcrime
SUB SetNPCD(npcd AS NPCType, offset AS INTEGER, value AS INTEGER)
  STATIC maxoffset AS INTEGER = -1
  IF maxoffset = -1 THEN maxoffset = (curbinsize(binN) \ 2) - 1

  IF offset >= 0 AND offset <= maxoffset THEN
    (@npcd.picture)[offset] = value
  ELSE
    debug "Attempt to write NPC data out-of-range. offset=" + STR$(offset) + " value=" + STR$(value)
  END IF
END SUB

'Prefer read_npc_int instead in the future, as it lacks pointer thoughtcrime
FUNCTION GetNPCD(npcd AS NPCType, offset AS INTEGER) AS INTEGER
  STATIC maxoffset AS INTEGER = -1
  IF maxoffset = -1 THEN maxoffset = (curbinsize(binN) \ 2) - 1

  IF offset >= 0 AND offset <= maxoffset THEN
    RETURN (@npcd.picture)[offset]
  ELSE
    debug "Attempt to read NPC data out-of-range. offset=" & offset
  END IF
END FUNCTION

SUB CleanNPCDefinition(dat as NPCType)
  WITH dat
   .picture    = 0
   .palette    = -1 'Default palette
   .movetype   = 0
   .speed      = 0
   .textbox    = 0
   .facetype   = 0
   .item       = 0
   .pushtype   = 0
   .activation = 0
   .tag1       = 0
   .tag2       = 0
   .usetag     = 0
   .script     = 0
   .scriptarg  = 0
   .vehicle    = 0
   IF .sprite THEN frame_unload @.sprite
   IF .pal THEN palette16_unload @.pal
  END WITH
END SUB

SUB CleanNPCD(dat() as NPCType)
  FOR i AS INTEGER = 0 TO UBOUND(dat)
    CleanNPCDefinition dat(i)
  NEXT
END SUB

SUB LoadNPCL(file as string, dat() as NPCInst)
  DIM i AS INTEGER
  DIM f AS INTEGER
  f = FREEFILE
  OPEN file FOR BINARY AS #f
  SEEK #f,8
  FOR i = 0 to 299
    dat(i).x = ReadShort(f,-1) * 20
  NEXT
  FOR i = 0 to 299
    dat(i).y = (ReadShort(f,-1) - 1) * 20
  NEXT
  FOR i = 0 to 299
    dat(i).id = ReadShort(f,-1)
  NEXT
  FOR i = 0 to 299
    dat(i).dir = ReadShort(f,-1)
  NEXT
  FOR i = 0 to 299
    dat(i).frame = ReadShort(f,-1)
  NEXT
  FOR i = 0 TO 299
    dat(i).xgo = 0
    dat(i).ygo = 0
  NEXT
  CLOSE #f
END SUB

SUB SaveNPCL(file as string, dat() as NPCInst)
  DIM i AS INTEGER
  DIM f AS INTEGER
  f = FREEFILE
  OPEN file FOR BINARY AS #f
  SEEK #f, 8
  FOR i = 0 to 299
    WriteShort f, -1, dat(i).x / 20
  NEXT
  FOR i = 0 to 299
    WriteShort f, -1, dat(i).y / 20 + 1
  NEXT
  FOR i = 0 to 299
    WriteShort f, -1, dat(i).id
  NEXT
  FOR i = 0 to 299
    WriteShort f, -1, dat(i).dir
  NEXT
  FOR i = 0 to 299
    WriteShort f, -1, dat(i).frame
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

SUB CleanNPCL(dat() as NPCInst, byval num as integer=-1)
  'Num is the count of elements to erase, or -1 to autodetect the size of the array
  DIM i as integer
  IF num = -1 THEN num = UBOUND(dat) + 1
  FOR i = 0 to num - 1
   WITH dat(i)
    .x = 0
    .y = 0
    .id = 0
    .dir = 0
    .frame = 0
    .xgo = 0
    .ygo = 0
    .extra(0) = 0
    .extra(1) = 0
    .extra(2) = 0
   END WITH
  NEXT
END SUB

SUB SerInventory8Bit(invent() as InventSlot, z, buf())
  DIM i as integer, j as integer
  buf(z) = 1 'Instruct new versions of game to ignore all this junk and use the 16-bit data instead
  '...but go ahead and write the 8-bit data so that loading a new SAV in an old version of game
  '   will not result in a nuked inventory
  z += 3 ' disregard some jibba jabba
  FOR i = 0 to 197 ' hard code old inventoryMax
    IF invent(i).used THEN
      buf(z) = (invent(i).num AND 255) shl 8 OR ((invent(i).id + 1) AND 255)
    ELSE
      buf(z) = 0
    END IF
    z += 1
  NEXT
  z += 2  'slots 198 and 199 not useable
  z += 3 * 12
  FOR i = 0 to 197 ' hard code old inventoryMax
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
  FOR i = 0 TO 197 ' hard code old inventoryMax
    invent(i).num = buf(z) shr 8
    invent(i).id = (buf(z) and 255) - 1
    invent(i).used = invent(i).id >= 0
    z += 1
  NEXT
  z += 2
  z += 3 * 12
  FOR i = 0 TO 197 ' hard code old inventoryMax
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

SUB SaveInventory16bit(invent() AS InventSlot, BYREF z AS INTEGER, buf() AS INTEGER, BYVAL first AS INTEGER=0, BYVAL last AS INTEGER=-1)
  IF last = -1 THEN last = UBOUND(invent)
  DIM i AS INTEGER
  FOR i = first TO small(inventoryMax, last)
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

SUB LoadInventory16Bit(invent() AS InventSlot, BYREF z AS INTEGER, buf() AS INTEGER, BYVAL first AS INTEGER=0, BYVAL last AS INTEGER=-1)
  IF last = -1 THEN last = UBOUND(invent)
  DIM i AS INTEGER
  FOR i = first TO small(inventoryMax, last)
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

SUB UnloadTilemap(map as TileMap)
  DEALLOCATE map.data
  map.data = NULL
END SUB

SUB UnloadTilemaps(layers() as TileMap)
  FOR i as integer = 0 TO UBOUND(layers)
    DEALLOCATE layers(i).data
    layers(i).data = NULL
  NEXT
END SUB

SUB LoadTilemap(map as TileMap, filename as string)
  IF map.data THEN DEALLOCATE map.data

  DIM AS INTEGER fh
  fh = FREEFILE
  OPEN filename FOR BINARY AS #fh
  map.wide = bound(readshort(fh, 8), 16, 32678)
  map.high = bound(readshort(fh, 10), 10, 32678)
  map.layernum = 0
  IF map.wide * map.high + 11 <> LOF(fh) THEN
    'PROBLEM: early versions always saved 32000 bytes of tile data (ie, 32011 total)!
    'Because of bug 829, tilemaps with bad lengths are common; better not to spam this message
    'debug "tilemap " & filename & " (" & map.wide & "x" & map.high & ") bad length or size; " & LOF(fh) & " bytes"
    'show the user their garbled mess, always interesting
  END IF
  map.data = ALLOCATE(map.wide * map.high)
  GET #fh, 12, *map.data, map.wide * map.high
  CLOSE #fh
END SUB

'FIXME: early versions always saved 32000 bytes of tile data (ie, 32011 total)!
'This will cause extra spurious map layers to be loaded!
SUB LoadTilemaps(layers() as TileMap, filename as string)
  DIM AS INTEGER fh, numlayers, i, wide, high
  FOR i = 0 TO UBOUND(layers)
    IF layers(i).data THEN DEALLOCATE layers(i).data
  NEXT

  fh = FREEFILE
  OPEN filename FOR BINARY AS #fh
  wide = bound(readshort(fh, 8), 16, 32678)
  high = bound(readshort(fh, 10), 10, 32678)
  numlayers = (LOF(fh) - 11) \ (wide * high)
  IF numlayers > maplayerMax + 1 OR numlayers * wide * high + 11 <> LOF(fh) THEN
    'Because of bug 829, tilemaps with bad lengths are common; better not to spam this message
    'debug "tilemap " & filename & " (" & wide & "x" & high & ") bad length or size; " & LOF(fh) & " bytes"
    'show the user their garbled mess, always interesting
    numlayers = bound(numlayers, 1, maplayerMax + 1)
  END IF
  REDIM layers(numlayers - 1)
  SEEK fh, 12
  FOR i = 0 TO numlayers - 1
    WITH layers(i)
      .data = ALLOCATE(wide * high)
      .wide = wide
      .high = high
      .layernum = i
      GET #fh, , *.data, wide * high
    END WITH
  NEXT
  CLOSE #fh
END SUB

SUB SaveTilemap(tmap as TileMap, filename as string)
  DIM fh as integer
  safekill filename
  fh = FREEFILE
  OPEN filename FOR BINARY AS #fh
  writeshort fh, 8, tmap.wide
  writeshort fh, 10, tmap.high
  PUT #fh, 12, *tmap.data, tmap.wide * tmap.high
  CLOSE #fh
END SUB

SUB SaveTilemaps(tmaps() as TileMap, filename as string)
  DIM fh as integer
  safekill filename
  fh = FREEFILE
  OPEN filename FOR BINARY AS #fh
  writeshort fh, 8, tmaps(0).wide
  writeshort fh, 10, tmaps(0).high
  SEEK #fh, 12
  FOR i as integer = 0 TO UBOUND(tmaps(0))
    PUT #fh, , *tmaps(i).data, tmaps(i).wide * tmaps(i).high
  NEXT
  CLOSE #fh
END SUB

SUB CleanTilemap(map as TileMap, BYVAL wide as integer, BYVAL high as integer, BYVAL layernum as integer = 0)
  'two purposes: allocate a new tilemap, or blank an existing one
  UnloadTilemap(map)
  map.wide = wide
  map.high = high
  map.data = CALLOCATE(wide * high)
  map.layernum = layernum
END SUB

SUB CleanTilemaps(layers() as TileMap, BYVAL wide as integer, BYVAL high as integer, BYVAL numlayers as integer)
  'two purposes: allocate a new tilemap, or blank an existing one
  UnloadTilemaps layers()
  REDIM layers(numlayers - 1)
  FOR i as integer = 0 TO numlayers - 1
    WITH layers(i)
      .wide = wide
      .high = high
      .data = CALLOCATE(wide * high)
      .layernum = i
    END WITH
  NEXT
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
		.portrait = readshort(f)
		for i = 0 to 2
			.bits(i) = readShort(f)
		next
		for i = 0 to 3
			.list_name(i) = ReadVStr(f,10)
		next
		.portrait_pal = readshort(f)
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
		writeshort(f,-1,.portrait)
		for i = 0 to 2
			writeshort(f,-1,.bits(i))
		next
		for i = 0 to 3
			WriteVStr(f,10, .list_name(i))
		next
		writeshort(f,-1,.portrait_pal)
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

'This initialises a menu if it has not been already
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
  IF .items THEN
   DeleteMenuItems dat
  ELSE
   dlist_construct .itemlist, OFFSETOF(MenuDefItem, trueorder)
  END IF
 END WITH
 bits(0) = 0
 MenuBitsFromArray dat, bits()
END SUB

SUB DeleteMenuItems(menu AS MenuDef)
 DIM i AS INTEGER
 WITH menu
  FOR i = 0 TO .numitems - 1
   dlist_remove menu.itemlist, .items[i]
   DELETE .items[i]
  NEXT i
  DEALLOCATE(.items)
  .items = NULL
 END WITH
END SUB

'This is not used anywhere currently. This has nothing to do with deleting a menu item!
SUB ClearMenuItem(mi AS MenuDefItem)
 DIM bits(0) AS INTEGER
 DIM i AS INTEGER
 WITH mi
  .caption = ""
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
  .on_close = ReadShort(f)
  .esc_menu = ReadShort(f)
  IF .items THEN
   DeleteMenuItems dat
  ELSE
   dlist_construct .itemlist, OFFSETOF(MenuDefItem, trueorder)
  END IF
 END WITH
 CLOSE #f
 IF ignore_items = NO THEN 'This is disableable for performance when all you care about loading is the menu's name
  LoadMenuItems menu_set, dat, record
 END IF
END SUB

SUB LoadMenuItems(menu_set AS MenuSet, menu AS MenuDef, record AS INTEGER)
 DIM i AS INTEGER
 DIM f AS INTEGER
 DIM member AS INTEGER
 DIM actual_record_count AS INTEGER = 0
 'The items may appear out-of-order in menuitem.bin, so rather than just append them as
 'we find the, first we store them in this temp array:
 REDIM itemarray(0) AS MenuDefItem ptr

 f = FREEFILE
 OPEN menu_set.itemfile FOR BINARY AS #f
 'FIXME: this shouldn't be here, it's covered in upgrade() (but commented out currently)
 actual_record_count = LOF(f) / getbinsize(binMENUITEM)
 IF actual_record_count <> gen(genMaxMenuItem) + 1 THEN
  debug "menuitem.bin record count sanity check failed " & gen(genMaxMenuItem) & "->" & actual_record_count - 1
  gen(genMaxMenuItem) = actual_record_count - 1
 END IF
 FOR i = 0 TO gen(genMaxMenuItem)
  SEEK #f, i * getbinsize(binMENUITEM) + 1
  member = ReadShort(f)
  IF member = record + 1 THEN
   LoadMenuItem f, itemarray(), i
  END IF
 NEXT i
 CLOSE #f

 'build the item list
 FOR i = 0 TO UBOUND(itemarray)
  IF itemarray(i) <> NULL THEN
   dlist_append(menu.itemlist, itemarray(i))
  ELSE
   'can't create a zero length FB array
   IF UBOUND(itemarray) <> 0 THEN
    debug "menu " & record & " item " & i & " could not be found in " & menu_set.itemfile
   END IF
  END IF
 NEXT
 'build the items[] array
 SortMenuItems menu
END SUB

SUB LoadMenuItem(f AS INTEGER, items() AS MenuDefItem ptr, record AS INTEGER)
 DIM i AS INTEGER
 DIM bits(0) AS INTEGER
 DIM mi AS MenuDefItem ptr
 DIM itemnum AS INTEGER
 mi = NEW MenuDefItem
 SEEK #f, record * getbinsize(binMENUITEM) + 1
 WITH *mi
  ReadShort(f) 'throw away member
  .caption = ReadByteStr(f, 38)
  itemnum = ReadShort(f)
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
 IF itemnum > UBOUND(items) THEN REDIM PRESERVE items(itemnum)
 items(itemnum) = mi
 MenuItemBitsFromArray *mi, bits()
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
  WriteShort(f, -1, .on_close)
  WriteShort(f, -1, .esc_menu)
 END WITH
 CLOSE #f
 SaveMenuItems menu_set, dat, record
END SUB

SUB SaveMenuItems(menu_set AS MenuSet, menu AS MenuDef, record AS INTEGER)
 DIM i AS INTEGER
 DIM f AS INTEGER
 DIM member AS INTEGER
 DIM elem AS INTEGER = 0
 DIM mi AS MenuDefItem ptr
 DIM blankmi AS MenuDefItem
 
 f = FREEFILE
 OPEN menu_set.itemfile FOR BINARY AS #f
 'Loop through each record and orphan all old entries for this menu
 FOR i = 0 TO gen(genMaxMenuItem)
  SEEK #f, i * getbinsize(binMENUITEM) + 1
  member = ReadShort(f)
  IF member = record + 1 THEN
   SaveMenuItem f, blankmi, i, -1, 0
  END IF
 NEXT i
 'Loop through each record, writing new values into orphan slots
 mi = menu.first
 FOR i = 0 TO gen(genMaxMenuItem)
  SEEK #f, i * getbinsize(binMENUITEM) + 1
  member = ReadShort(f)
  IF member = 0 THEN
   IF mi = NULL THEN EXIT FOR
   SaveMenuItem f, *mi, i, record, elem
   elem = elem + 1
   mi = mi->trueorder.next
  END IF
 NEXT i
 DO WHILE mi
  'More items need to be written, append them
  gen(genMaxMenuItem) += 1
  SaveMenuItem f, *mi, gen(genMaxMenuItem), record, elem
  elem += 1
  mi = mi->trueorder.next
 LOOP
 CLOSE #f
END SUB

SUB SaveMenuItem(f AS INTEGER, mi AS MenuDefItem, record AS INTEGER, menunum AS INTEGER, itemnum AS INTEGER)
 DIM i AS INTEGER
 DIM bits(0) AS INTEGER
 SEEK #f, record * getbinsize(binMENUITEM) + 1
 WITH mi
  WriteShort(f, -1, menunum + 1)
  WriteByteStr(f, 38, .caption)
  WriteShort(f, -1, itemnum)
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
  setbit bits(), 0, 8, .advance_textbox
 END WITH
END SUB

SUB MenuBitsFromArray (menu AS MenuDef, bits() AS INTEGER)
 WITH menu
  .translucent    = xreadbit(bits(), 0)
  .no_scrollbar   = xreadbit(bits(), 1)
  .allow_gameplay = xreadbit(bits(), 2)
  .suspend_player = xreadbit(bits(), 3)
  .no_box         = xreadbit(bits(), 4)
  .no_close       = xreadbit(bits(), 5)
  .no_controls    = xreadbit(bits(), 6)
  .prevent_main_menu = xreadbit(bits(), 7)
  .advance_textbox   = xreadbit(bits(), 8)
 END WITH
END SUB

SUB MenuItemBitsToArray (mi AS MenuDefItem, bits() AS INTEGER)
 bits(0) = 0
 WITH mi
  setbit bits(), 0, 0, .hide_if_disabled
  setbit bits(), 0, 1, .close_if_selected
  setbit bits(), 0, 2, .skip_close_script
 END WITH
END SUB

SUB MenuItemBitsFromArray (mi AS MenuDefItem, bits() AS INTEGER)
 WITH mi
  .hide_if_disabled  = xreadbit(bits(), 0)
  .close_if_selected = xreadbit(bits(), 1)
  .skip_close_script = xreadbit(bits(), 2)
 END WITH
END SUB

'recreate a menu's items[] array, which sorts visible items to the top
SUB SortMenuItems(menu AS MenuDef)
 DIM AS INTEGER i, j, lowest, found
 DIM mi AS MenuDefItem ptr
 IF menu.numitems = 0 THEN
  DEALLOCATE(menu.items)
  menu.items = NULL
  EXIT SUB
 END IF
 menu.items = REALLOCATE(menu.items, SIZEOF(any ptr) * menu.numitems)
 'stick all visible items in .items[]
 i = 0
 mi = menu.first
 WHILE mi
  IF (mi->disabled AND mi->hide_if_disabled) = 0 THEN
   menu.items[i] = mi
   i += 1
  END IF
  mi = mi->trueorder.next
 WEND
 'append all invisible items
 mi = menu.first
 WHILE mi
  IF mi->disabled AND mi->hide_if_disabled THEN
   menu.items[i] = mi
   i += 1
  END IF
  mi = mi->trueorder.next
 WEND
END SUB

SUB LoadVehicle (file AS STRING, vehicle AS VehicleData, record AS INTEGER)
  DIM buf(39)
  LoadVehicle file, buf(), vehicle.name, record
  WITH vehicle
    .speed          = buf(8)
    .random_battles = buf(11)
    .use_button     = buf(12)
    .menu_button    = buf(13)
    .riding_tag     = buf(14)
    .on_mount       = buf(15)
    .on_dismount    = buf(16)
    .override_walls = buf(17)
    .blocked_by     = buf(18)
    .mount_from     = buf(19)
    .dismount_to    = buf(20)
    .elevation      = buf(21)
    .pass_walls            = xreadbit(buf(), 0, 9)
    .pass_npcs             = xreadbit(buf(), 1, 9)
    .enable_npc_activation = xreadbit(buf(), 2, 9)
    .enable_door_use       = xreadbit(buf(), 3, 9)
    .do_not_hide_leader    = xreadbit(buf(), 4, 9)
    .do_not_hide_party     = xreadbit(buf(), 5, 9)
    .dismount_ahead        = xreadbit(buf(), 6, 9)
    .pass_walls_while_dismounting = xreadbit(buf(), 7, 9)
    .disable_flying_shadow        = xreadbit(buf(), 8, 9)
  END WITH
END SUB

SUB LoadVehicle (file AS STRING, veh(), vehname$, record AS INTEGER)
 loadrecord veh(), file, 40, record
 vehname$ = STRING$(bound(veh(0) AND 255, 0, 15), 0)
 array2str veh(), 1, vehname$
END SUB

SUB SaveVehicle (file AS STRING, vehicle AS VehicleData, record AS INTEGER)
  DIM buf(39)
  WITH vehicle
    buf(39) = .speed
    buf(11) = .random_battles
    buf(12) = .use_button
    buf(13) = .menu_button
    buf(14) = .riding_tag
    buf(15) = .on_mount
    buf(16) = .on_dismount
    buf(17) = .override_walls
    buf(18) = .blocked_by
    buf(19) = .mount_from
    buf(20) = .dismount_to
    buf(21) = .elevation
    setbit buf(), 9, 0, .pass_walls
    setbit buf(), 9, 1, .pass_npcs
    setbit buf(), 9, 2, .enable_npc_activation
    setbit buf(), 9, 3, .enable_door_use
    setbit buf(), 9, 4, .do_not_hide_leader
    setbit buf(), 9, 5, .do_not_hide_party
    setbit buf(), 9, 6, .dismount_ahead
    setbit buf(), 9, 7, .pass_walls_while_dismounting
    setbit buf(), 9, 8, .disable_flying_shadow
  END WITH
  SaveVehicle file, buf(), vehicle.name, record
END SUB

SUB SaveVehicle (file AS STRING, veh(), vehname$, record AS INTEGER)
 veh(0) = bound(LEN(vehname$), 0, 15)
 str2array vehname$, veh(), 1
 storerecord veh(), file, 40, record
END SUB

SUB ClearVehicle (vehicle AS VehicleData)
  WITH vehicle
    .speed          = 0
    .random_battles = 0
    .use_button     = 0
    .menu_button    = 0
    .riding_tag     = 0
    .on_mount       = 0
    .on_dismount    = 0
    .override_walls = 0
    .blocked_by     = 0
    .mount_from     = 0
    .dismount_to    = 0
    .elevation      = 0
    .pass_walls            = NO
    .pass_npcs             = NO
    .enable_npc_activation = NO
    .enable_door_use       = NO
    .do_not_hide_leader    = NO
    .do_not_hide_party     = NO
    .dismount_ahead        = NO
    .pass_walls_while_dismounting = NO
    .disable_flying_shadow        = NO
  END WITH
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
 FOR i = 0 TO 47
  temp.col = fixeddefaults(i)
  colarray(i) = nearcolor(master(), temp.r, temp.g, temp.b)
 NEXT
 FOR i = 48 TO 62
  'Box border pictures default to zero (none)
  colarray(i) = 0
 NEXT i
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

SUB LoadTextBox (BYREF box AS TextBox, record AS INTEGER)
 DIM boxbuf(dimbinsize(binSAY)) AS INTEGER
 IF record < 0 OR record > gen(genMaxTextBox) THEN
  debug "LoadTextBox: invalid record: " & record
  IF record <> 0 THEN LoadTextBox box, 0
  EXIT SUB
 END IF

 DIM filename AS STRING
 filename = game & ".say"
 loadrecord boxbuf(), filename, getbinsize(binSAY) / 2, record

 DIM i AS INTEGER

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
  .menu_tag    = bound(boxbuf(192), -999, 999)
  .menu        = bound(boxbuf(199), 0 ,gen(genMaxMenu))
  '--Get box bitsets
  .choice_enabled = xreadbit(boxbuf(), 0, 174)
  .no_box         = xreadbit(boxbuf(), 1, 174)
  .opaque         = xreadbit(boxbuf(), 2, 174)
  .restore_music  = xreadbit(boxbuf(), 3, 174)
  .portrait_box   = xreadbit(boxbuf(), 4, 174)
  .stop_sound_after = xreadbit(boxbuf(), 5, 174)
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
  .sound_effect    = boxbuf(205) ' +1
  '--Get portrait data
  .portrait_type   = boxbuf(200)
  .portrait_id     = boxbuf(201)
  .portrait_pal    = boxbuf(202)
  .portrait_pos.x  = boxbuf(203)
  .portrait_pos.y  = boxbuf(204)
 END WITH
END SUB

SUB SaveTextBox (BYREF box AS TextBox, record AS INTEGER)
 DIM boxbuf(dimbinsize(binSAY))
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
  boxbuf(192) = .menu_tag
  boxbuf(199) = .menu
  '--Save bitsets
  setbit boxbuf(), 174, 0, .choice_enabled
  setbit boxbuf(), 174, 1, .no_box
  setbit boxbuf(), 174, 2, .opaque
  setbit boxbuf(), 174, 3, .restore_music
  setbit boxbuf(), 174, 4, .portrait_box
  setbit boxbuf(), 174, 5, .stop_sound_after
  setbit boxbuf(), 174, 6, NO 'Unused
  setbit boxbuf(), 174, 7, NO 'Unused
  '--Transcribe choice text
  FOR i = 0 TO 1
   WHILE LEN(.choice(i)) < 15: .choice(i) = .choice(i) & CHR(0): WEND
   str2array .choice(i), boxbuf(), 349 + (i * 18)
   'Also save choice tags
   boxbuf(182 + (i * 9)) = .choice_tag(i)
  NEXT i
  '--Save box appearance
  boxbuf(193) = .vertical_offset
  boxbuf(194) = .shrink
  boxbuf(195) = .textcolor ' 0=default
  boxbuf(196) = .boxstyle
  boxbuf(197) = .backdrop  ' +1
  boxbuf(198) = .music     ' +1
  boxbuf(205) = .sound_effect ' +1
  '--Save portrait data
  boxbuf(200) = .portrait_type
  boxbuf(201) = .portrait_id
  boxbuf(202) = .portrait_pal
  boxbuf(203) = .portrait_pos.x
  boxbuf(204) = .portrait_pos.y
 END WITH

 storerecord boxbuf(), filename, getbinsize(binSAY) / 2, record
END SUB

SUB ClearTextBox (BYREF box AS TextBox)
 DIM i AS INTEGER
 '--Erase members of TextBox object
 WITH box
  '--Load lines of text
  FOR i = 0 TO 7
   .text(i) = ""
  NEXT i
  '--Erase conditional data
  .instead_tag = 0
  .instead     = 0
  .settag_tag  = 0
  .settag1     = 0
  .settag2     = 0
  .battle_tag  = 0
  .battle      = 0
  .shop_tag    = 0
  .shop        = 0
  .hero_tag    = 0
  .hero_addrem = 0
  .after_tag   = 0
  .after       = 0
  .money_tag   = 0
  .money       = 0
  .door_tag    = 0
  .door        = 0
  .item_tag    = 0
  .item        = 0
  .hero_swap   = 0
  .hero_lock   = 0
  .menu_tag    = 0
  .menu        = 0
  '--Clear box bitsets
  .choice_enabled = NO
  .no_box         = NO
  .opaque         = NO
  .restore_music  = NO
  .portrait_box   = NO
  '--Clear choicebox data
  FOR i = 0 TO 1
   .choice(i) = ""
   .choice_tag(i) = 0
  NEXT i
  '--Clear box appearance
  .vertical_offset = 0
  .shrink          = -1
  .textcolor       = 0
  .boxstyle        = 0
  .backdrop        = 0
  .music           = 0
  '--Clear character portrait
  .portrait_type = 0
  .portrait_id = 0
  .portrait_pal = -1
  .portrait_pos.x = 0
  .portrait_pos.y = 0
 END WITH
END SUB

SUB loadoldattackdata (array(), index)
 loadrecord array(), game & ".dt6", 40, index
END SUB

SUB saveoldattackdata (array(), index)
 storerecord array(), game & ".dt6", 40, index
END SUB

SUB loadnewattackdata (array(), index)
 DIM size AS INTEGER = getbinsize(binATTACK) \ 2
 IF size > 0 THEN
  loadrecord array(), workingdir + SLASH + "attack.bin", size, index
 END IF
END SUB

SUB savenewattackdata (array(), index)
 DIM size AS INTEGER = curbinsize(binATTACK) \ 2
 IF size > 0 THEN
  storerecord array(), workingdir + SLASH + "attack.bin", size, index
 END IF
END SUB

SUB loadattackdata (array() AS INTEGER, BYVAL index AS INTEGER)
 loadoldattackdata array(), index
 DIM size AS INTEGER = getbinsize(binATTACK) \ 2 'size of record in RPG file
 IF size > 0 THEN
  DIM buf(size) AS INTEGER
  loadnewattackdata buf(), index
  FOR i AS INTEGER = 0 TO size
   array(40 + i) = buf(i)
  NEXT i
 END IF
END SUB

SUB loadattackchain (BYREF ch AS AttackDataChain, buf() AS INTEGER, BYVAL id_offset AS INTEGER, BYVAL rate_offset AS INTEGER, BYVAL mode_offset AS INTEGER, BYVAL val1_offset AS INTEGER, BYVAL val2_offset AS INTEGER, BYVAL bits_offset AS INTEGER)
 ch.atk_id = buf(id_offset)
 ch.rate = buf(rate_offset)
 ch.mode = buf(mode_offset)
 ch.val1 = buf(val1_offset)
 ch.val2 = buf(val2_offset)
 ch.must_know  = xreadbit(buf(), 0, bits_offset)
 ch.no_delay   = xreadbit(buf(), 1, bits_offset)
END SUB

SUB loadattackdata (BYREF atkdat AS AttackData, BYVAL index AS INTEGER)
 DIM buf(40 + dimbinsize(binATTACK)) AS INTEGER
 loadattackdata buf(), index
 convertattackdata buf(), atkdat
END SUB

SUB convertattackdata(buf() AS INTEGER, BYREF atkdat AS AttackData)
 WITH atkdat
  .name = readbadbinstring(buf(), 24, 10, 1)
  .description = readbinstring(buf(), 73, 38)
  .picture = buf(0)
  .pal = buf(1)
  .anim_pattern = buf(2)
  .targ_class = buf(3)
  .targ_set = buf(4)
  .damage_math = buf(5)
  .aim_math = buf(6)
  .base_atk_stat = buf(7)
  .base_def_stat = buf(58)
  .mp_cost = buf(8)
  .hp_cost = buf(9)
  .money_cost = buf(10)
  .extra_damage = buf(11)
  .attacker_anim = buf(14)
  .attack_anim = buf(15)
  .attack_delay = buf(16)
  .hits = buf(17)
  .targ_stat = buf(18)
  .prefer_targ = buf(19)
  .prefer_targ_stat = buf(100)
  .caption_time = buf(36)
  .caption = readbinstring$(buf(), 37, 38)
  .caption_delay = buf(57)
  FOR i AS INTEGER = 0 TO 1
   WITH .tagset(i)
    .tag = buf(59 + i*3)
    .condition = buf(60 + i*3)
    .tagcheck = buf(61 + i*3)
   END WITH
  NEXT i
  FOR i AS INTEGER = 0 TO 2
   WITH .item(i)
    .id = buf(93 + i*2)
    .number = buf(94 + i*2)
   END WITH
  NEXT i
  .sound_effect = buf(99)
  .learn_sound_effect = buf(117)
  .transmog_enemy = buf(118)
  .transmog_hp = buf(119)
  .transmog_stats = buf(120)
  '----Chaining----
  loadattackchain .chain, buf(), 12, 13, 101, 102, 103, 104
  loadattackchain .elsechain, buf(), 105, 107, 106, 108, 109, 110
  loadattackchain .instead, buf(), 111, 113, 112, 114, 115, 116
  '----Bitsets----
  .cure_instead_of_harm           = xreadbit(buf(), 0, 20)
  .divide_spread_damage           = xreadbit(buf(), 1, 20)
  .absorb_damage                  = xreadbit(buf(), 2, 20)
  .unreversable_picture           = xreadbit(buf(), 3, 20)
  .can_steal_item                 = xreadbit(buf(), 4, 20)
  FOR i AS INTEGER = 0 TO 7
   .elemental_damage(i)           = xreadbit(buf(), 5+i, 20)
   .monster_type_bonus(i)         = xreadbit(buf(), 13+i, 20)
   .fail_vs_elemental(i)          = xreadbit(buf(), 21+i, 20)
   .fail_vs_monster_type(i)       = xreadbit(buf(), 29+i, 20)
  NEXT i
  FOR i AS INTEGER = 0 TO 7
   .cannot_target_enemy_slot(i)   = xreadbit(buf(), 37+i, 20)
  NEXT i
  FOR i AS INTEGER = 0 TO 3
   .cannot_target_hero_slot(i)    = xreadbit(buf(), 45+i, 20)
  NEXT i
  .ignore_extra_hits              = xreadbit(buf(), 49, 20)
  .erase_rewards                  = xreadbit(buf(), 50, 20)
  .show_damage_without_inflicting = xreadbit(buf(), 51, 20)
  .store_targ                     = xreadbit(buf(), 52, 20)
  .delete_stored_targ             = xreadbit(buf(), 53, 20)
  .automatic_targ                 = xreadbit(buf(), 54, 20)
  .show_name                      = xreadbit(buf(), 55, 20)
  .do_not_display_damage          = xreadbit(buf(), 56, 20)
  .reset_targ_stat_before_hit     = xreadbit(buf(), 57, 20)
  .allow_cure_to_exceed_maximum   = xreadbit(buf(), 58, 20)
  .useable_outside_battle         = xreadbit(buf(), 59, 20)
  .obsolete_damage_mp             = xreadbit(buf(), 60, 20)
  .do_not_randomize               = xreadbit(buf(), 61, 20)
  .damage_can_be_zero             = xreadbit(buf(), 62, 20)
  .force_run                      = xreadbit(buf(), 63, 20)
  .mutable                        = xreadbit(buf(), 0, 65)
  .fail_if_targ_poison            = xreadbit(buf(), 1, 65)
  .fail_if_targ_regen             = xreadbit(buf(), 2, 65)
  .fail_if_targ_stun              = xreadbit(buf(), 3, 65)
  .fail_if_targ_mute              = xreadbit(buf(), 4, 65)
  .percent_damage_not_set         = xreadbit(buf(), 5, 65)
  .check_costs_as_weapon          = xreadbit(buf(), 6, 65)
  .no_chain_on_failure            = xreadbit(buf(), 7, 65)
  .reset_poison                   = xreadbit(buf(), 8, 65)
  .reset_regen                    = xreadbit(buf(), 9, 65)
  .reset_stun                     = xreadbit(buf(), 10, 65)
  .reset_mute                     = xreadbit(buf(), 11, 65)
  .cancel_targets_attack          = xreadbit(buf(), 12, 65)
  .not_cancellable_by_attacks     = xreadbit(buf(), 13, 65)
  .no_spawn_on_attack             = xreadbit(buf(), 14, 65)
  .no_spawn_on_kill               = xreadbit(buf(), 15, 65)
  .check_costs_as_item            = xreadbit(buf(), 16, 65)
  .recheck_costs_after_delay      = xreadbit(buf(), 17, 65)
  .targ_does_not_flinch           = xreadbit(buf(), 18, 65)
  .do_not_exceed_targ_stat        = xreadbit(buf(), 19, 65)
 END WITH
END SUB

SUB saveattackdata (array(), index)
 saveoldattackdata array(), index
 DIM size AS INTEGER = curbinsize(binATTACK) / 2 'size of record supported by engine
 IF size > 0 THEN
  DIM buf(size) AS INTEGER
  FOR i AS INTEGER = 0 TO size
   buf(i) = array(40 + i)
  NEXT i
  savenewattackdata buf(), index
 END IF
END SUB

SUB loadtanim (n AS INTEGER, tastuf() AS INTEGER)
 setpicstuf tastuf(), 80, -1
 loadset game & ".tap", n, 0
END SUB

SUB savetanim (n AS INTEGER, tastuf() AS INTEGER)
 setpicstuf tastuf(), 80, -1
 storeset game & ".tap", n, 0
END SUB

SUB getpal16 (array() AS INTEGER, aoffset AS INTEGER, foffset AS INTEGER, autotype AS INTEGER=-1, sprite AS INTEGER=0)
DIM buf(8) AS INTEGER
DIM defaultpal AS INTEGER
DIM i AS INTEGER

loadrecord buf(), game & ".pal", 8, 0
IF buf(0) = 4444 THEN '--check magic number
 IF buf(1) >= foffset AND foffset >= 0 THEN
  'palette is available
  loadrecord buf(), game & ".pal", 8, 1 + foffset
  FOR i = 0 TO 7
   array(aoffset * 8 + i) = buf(i)
  NEXT i
  EXIT SUB
 ELSEIF foffset = -1 THEN
  'load a default palette
  IF autotype >= 0 THEN
   defaultpal = getdefaultpal(autotype, sprite)
   IF defaultpal > -1 THEN
    'Recursive
    getpal16 array(), aoffset, defaultpal
    EXIT SUB
   END IF
  END IF
 END IF
 'palette is out of range, return blank
 FOR i = 0 TO 7
  array(aoffset * 8 + i) = 0
 NEXT i
ELSE '--magic number not found, palette is still in BSAVE format
 DIM xbuf(100 * 8)
 xbload game + ".pal", xbuf(), "16-color palletes missing from " + game
 FOR i = 0 TO 7
  array(aoffset * 8 + i) = xbuf(foffset * 8 + i)
 NEXT i
END IF

END SUB

SUB storepal16 (array() AS INTEGER, aoffset AS INTEGER, foffset AS INTEGER)
DIM buf(8) AS INTEGER

DIM f AS STRING = game & ".pal"
loadrecord buf(), f, 8, 0

IF buf(0) <> 4444 THEN
 fatalerror "16-color palette file may be corrupt"
END IF

DIM last AS INTEGER = buf(1)
DIM i AS INTEGER

IF foffset > last THEN
 '--blank out palettes before extending file
 FOR i = last + 1 TO foffset
  flusharray buf(), 8, 0
  storerecord buf(), f, 8, 1 + i
 NEXT i
 '--update header
 buf(0) = 4444
 buf(1) = foffset
 storerecord buf(), f, 8, 0
END IF

IF foffset >= 0 THEN '--never write a negative file offset
 'copy palette to buffer
 FOR i = 0 TO 7
  buf(i) = array(aoffset * 8 + i)
 NEXT i
 'write palette
 storerecord buf(), f, 8, 1 + foffset
END IF

Palette16_update_cache f, foffset
END SUB

SUB loaditemdata (array() AS INTEGER, index AS INTEGER)
 flusharray array(), 99, 0
 IF index > gen(genMaxItem) THEN debug "loaditemdata:" & index & " out of range" : EXIT SUB
 IF loadrecord(array(), game & ".itm", 100, index) = 0 THEN debug "loaditemdata:" & index & " loadrecord failed" : EXIT SUB
END SUB

SUB saveitemdata (array() AS INTEGER, index AS INTEGER)
 storerecord array(), game & ".itm", 100, index
END SUB

SUB loadenemydata (array() AS INTEGER, index AS INTEGER, altfile AS INTEGER = 0)
 DIM filename AS STRING
 IF altfile THEN
  filename = tmpdir & "dt1.tmp"
 ELSE
  filename = game & ".dt1"
 END IF
 loadrecord array(), filename, 160, index
END SUB

SUB loadenemydata (enemy AS EnemyDef, index AS INTEGER, altfile AS INTEGER = 0)
 DIM buf(159) AS INTEGER
 loadenemydata buf(), index, altfile
 WITH enemy
  .name = readbadbinstring(buf(), 0, 16)
  .steal.thievability = buf(17)
  .steal.item = buf(18)
  .steal.item_rate = buf(19)
  .steal.rare_item = buf(20)
  .steal.rare_item_rate = buf(21)
  .dissolve = buf(22)
  .dissolve_length = buf(23)
  .death_sound = buf(24)
  .cursor_offset.x = buf(25)
  .cursor_offset.y = buf(26)
  .pic = buf(53)
  .pal = buf(54)
  .size = buf(55)
  .reward.gold = buf(56)
  .reward.exper = buf(57)
  .reward.item = buf(58)
  .reward.item_rate = buf(59)
  .reward.rare_item = buf(60)
  .reward.rare_item_rate = buf(61)
  FOR i AS INTEGER = 0 TO UBOUND(.stat.sta)
   .stat.sta(i) = buf(62 + i)
  NEXT i
  
  '--bitsets
  FOR i AS INTEGER = 0 TO 7
   .weak(i) = xreadbit(buf(), 0 + i, 74)
   .strong(i) = xreadbit(buf(), 8 + i, 74)
   .absorb(i) = xreadbit(buf(), 16 + i, 74)
   .enemytype(i) = xreadbit(buf(), 24 + i, 74)
  NEXT i
  .harmed_by_cure      = xreadbit(buf(), 54, 74)
  .mp_idiot            = xreadbit(buf(), 55, 74)
  .is_boss             = xreadbit(buf(), 56, 74)
  .unescapable         = xreadbit(buf(), 57, 74)
  .die_without_boss    = xreadbit(buf(), 58, 74)
  .flee_instead_of_die = xreadbit(buf(), 59, 74)
  .enemy_untargetable  = xreadbit(buf(), 60, 74)
  .hero_untargetable   = xreadbit(buf(), 61, 74)
  .death_unneeded      = xreadbit(buf(), 62, 74)
  .never_flinch        = xreadbit(buf(), 63, 74)
  .ignore_for_alone    = xreadbit(buf(), 64, 74)
  
  '--spawning
  .spawn.on_death = buf(79)
  .spawn.non_elemental_death = buf(80)
  .spawn.when_alone = buf(81)
  .spawn.non_elemental_hit = buf(82)
  FOR i AS INTEGER = 0 TO 7
   .spawn.elemental_hit(i) = buf(83 + i)
  NEXT i
  .spawn.how_many = buf(91)
  
  '--attacks
  FOR i AS INTEGER = 0 TO 4
   .regular_ai(i) = buf(92 + i)
   .desperation_ai(i) = buf(97 + i)
   .alone_ai(i) = buf(102 + i)
  NEXT i
  
  '--not used!
  FOR i AS INTEGER = 0 TO 7
   .counter_attack(i) = buf(107 + i)
  NEXT i
  
 END WITH
END SUB

SUB saveenemydata (array() AS INTEGER, index AS INTEGER, altfile AS INTEGER = 0)
 DIM filename AS STRING
 IF altfile THEN
  filename = tmpdir & "dt1.tmp"
 ELSE
  filename = game & ".dt1"
 END IF
 storerecord array(), filename, 160, index
END SUB

SUB saveenemydata (enemy AS EnemyDef, index AS INTEGER, altfile AS INTEGER = 0)
 DIM buf(159) AS INTEGER
 WITH enemy
  buf(0) = LEN(.name)
  FOR i AS INTEGER = 1 TO LEN(.name)
   buf(i) = ASC(MID(.name, i, 1))
  NEXT i
  buf(17) = .steal.thievability
  buf(18) = .steal.item
  buf(19) = .steal.item_rate
  buf(20) = .steal.rare_item
  buf(21) = .steal.rare_item_rate
  buf(22) = .dissolve
  buf(23) = .dissolve_length
  buf(24) = .death_sound
  buf(25) = .cursor_offset.x
  buf(26) = .cursor_offset.y
  buf(53) = .pic
  buf(54) = .pal
  buf(55) = .size
  buf(56) = .reward.gold
  buf(57) = .reward.exper
  buf(58) = .reward.item
  buf(59) = .reward.item_rate
  buf(60) = .reward.rare_item
  buf(61) = .reward.rare_item_rate
  FOR i AS INTEGER = 0 TO UBOUND(.stat.sta)
   buf(62 + i) = .stat.sta(i)
  NEXT i
  
  '--bitsets
  FOR i AS INTEGER = 0 TO 7
   setbit buf(), 74, 0 + i, .weak(i)
   setbit buf(), 74, 8 + i, .strong(i)
   setbit buf(), 74, 16 + i, .absorb(i)
   setbit buf(), 74, 24 + i, .enemytype(i)
  NEXT i
  setbit buf(), 74, 54, .harmed_by_cure
  setbit buf(), 74, 55, .mp_idiot
  setbit buf(), 74, 56, .is_boss
  setbit buf(), 74, 57, .unescapable
  setbit buf(), 74, 58, .die_without_boss
  setbit buf(), 74, 59, .flee_instead_of_die
  setbit buf(), 74, 60, .enemy_untargetable
  setbit buf(), 74, 61, .hero_untargetable
  setbit buf(), 74, 62, .death_unneeded
  setbit buf(), 74, 63, .never_flinch
  setbit buf(), 74, 64, .ignore_for_alone
  
  '--spawning
  buf(79) = .spawn.on_death
  buf(80) = .spawn.non_elemental_death
  buf(81) = .spawn.when_alone
  buf(82) = .spawn.non_elemental_hit
  FOR i AS INTEGER = 0 TO 7
   buf(83 + i) = .spawn.elemental_hit(i)
  NEXT i
  buf(91) = .spawn.how_many
  
  '--attacks
  FOR i AS INTEGER = 0 TO 4
   buf(92 + i) = .regular_ai(i)
   buf(97 + i) = .desperation_ai(i)
   buf(102 + i) = .alone_ai(i)
  NEXT i
  
  '--not used!
  FOR i AS INTEGER = 0 TO 7
   buf(107 + i) = .counter_attack(i)
  NEXT i
  
 END WITH

 saveenemydata buf(), index, altfile
END SUB

SUB loadherodata (hero as herodef ptr, index as integer)
 deserherodef game & ".dt0", hero, index
END SUB

SUB saveherodata (hero as herodef ptr, index as integer)
 serherodef game & ".dt0", hero, index
END SUB

SUB save_string_list(array() AS STRING, filename AS STRING)

 DIM fh AS INTEGER = FREEFILE
 OPEN filename FOR BINARY ACCESS WRITE AS #fh

 DIM s AS STRING
 
 FOR i AS INTEGER = 0 TO UBOUND(array)
  s = escape_nonprintable_ascii(array(i)) & CHR(10)
  PUT #fh, , s
 NEXT i
 
 CLOSE #fh

END SUB

SUB load_string_list(array() AS STRING, filename AS STRING)

 DIM i AS INTEGER = 0

 IF isfile(filename) THEN

  DIM fh AS INTEGER = FREEFILE
  OPEN filename FOR INPUT AS #fh

  DIM s AS STRING
 
  DO WHILE NOT EOF(fh)
   '--get the next line
   LINE INPUT #fh, s
   '--if the array is not big enough to hold the new line, make it bigger
   IF i > UBOUND(array) THEN
    REDIM PRESERVE array(i) AS STRING
   END IF
   '--store the string in the array
   array(i) = decode_backslash_codes(s)
   '--ready for the next line
   i += 1
  LOOP
 
  CLOSE #fh

 ELSE
  '--file does not exist, but still act like we read a single (empty) line
  i += 1
 END IF
 
 '--resize the array to fit the number of lines loaded
 REDIM PRESERVE array(i - 1) AS STRING
 
END SUB

FUNCTION load_map_pos_save_offset(BYVAL mapnum AS INTEGER) AS XYPair
 DIM offset AS XYPair
 DIM gmaptmp(dimbinsize(binMAP))
 loadrecord gmaptmp(), game & ".map", getbinsize(binMAP) / 2, mapnum
 offset.x = gmaptmp(20)
 offset.y = gmaptmp(21)
 RETURN offset
END FUNCTION
