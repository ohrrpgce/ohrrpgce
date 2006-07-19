TYPE NPCInst
  x as integer      'npcl+0   
  y as integer      'npcl+300 
  xgo as integer    'npcl+1500
  ygo as integer    'npcl+1800
  id as integer     'npcl+600 
  dir as integer    'npcl+900 
  frame as integer  'npcl+1200
  extra1 as integer
  extra2 as integer
END TYPE

TYPE InventSlot
  used : 1 as byte	'use this to check if empty, not num!
  text as string	'text field which shows up in inventory, blank if empty

  'following fields should not be used if used = 0
  id as integer		'absolute, not +1!! 
  num as integer
END TYPE

'Loading subs
DECLARE SUB LoadNPCL(file as string, dat() as NPCInst, num as integer)

'Serialization
DECLARE SUB SerNPCL(dat() as NPCInst, z, buf(), num as integer)
DECLARE SUB SerInventory(invent() as InventSlot, z, buf())

'DeSerialization
DECLARE SUB DeserNPCL(dat() as NPCInst, z, buf(), num as integer)
DECLARE SUB DeserInventory(invent() as InventSlot, z, buf())

'Cleaning
DECLARE SUB CleanNPCL(dat() as NPCInst, num as integer)
DECLARE SUB CleanInventory(invent() as InventSlot)
