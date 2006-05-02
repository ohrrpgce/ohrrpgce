TYPE NPCInst
  x as integer
  y as integer
  xgo as integer
  ygo as integer
  id as integer
  dir as integer
  frame as integer
END TYPE

'npcl+0
'npcl+300
'npcl+1500
'npcl+1800
'npcl+600
'npcl+900
'npcl+1200

'Loading subs
DECLARE SUB LoadNPCL(file as string,dat() as NPCInst, num as integer)

'Serialization
DECLARE SUB SerNPCL(dat() as NPCInst, z, buf(), num as integer)

'DeSerialization
DECLARE SUB DeserNPCL(dat() as NPCInst, z, buf(), num as integer)

'Cleaning
DECLARE SUB CleanNPCL(dat() as NPCInst, num as integer)