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

SUB DeserDoors(filename as string, array() as door)
	dim as integer hasheader = 0, f, i
	'when we strip the header, we can check for its presence here
	if not fileisreadable(filename) then exit sub
	open filename for binary as #f
	
	redim array(199)
	
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

Sub SerDoors(filename as string, array() as door, withhead as integer = 1)
	dim as integer f, i
	
	if not fileiswriteable(filename) then exit sub
	
	safekill(filename)
	
	open filename for binary as #1
	
	if withhead then
		dim stupid(6) as ubyte
		put #f, , stupid()
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
	
end sub