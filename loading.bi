'OHRRPGCE - loading.bi
'(C) Copyright 1997-2006 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#IFNDEF LOADING_BI
#DEFINE LOADING_BI

#include "reload.bi"
#include "reloadext.bi"

USING RELOAD
USING RELOAD.EXT

'*** NOTE ***
'As documented here, some types require initialisation, done by Clean*/Clear* (same thing) or Load*,
'and destruction, done by Unload*/Delete* (same thing). (ARRRGH! Can't we get anything straight??)
'Clean*/Clear* SUBs can also blank out already-initialised data; for objects that don't require
'initialisation that is all they do.

declare sub SerSingle (buf() as integer, byval index as integer, byval sing as single)
declare function DeSerSingle (buf() as integer, byval index as integer) as single

'Sprites are not loaded by these functions; can use CleanNPCD to free them if you load them
declare sub LoadNPCD(file as string, dat() as NPCType)
declare sub LoadNPCD_fixedlen(file as string, dat() as NPCType, byref arraylen as integer)
declare sub SaveNPCD(file as string, dat() as NPCType)
declare sub SaveNPCD_fixedlen(file as string, dat() as NPCType, byval arraylen as integer)
declare sub setnpcd(npcd as npctype, byval offset as integer, byval value as integer)
declare function getnpcd(npcd as npctype, byval offset as integer) as integer
declare sub CleanNPCDefinition(dat as NPCType)
declare sub CleanNPCD(dat() as NPCType)

declare sub LoadNPCL(file as string, dat() as npcinst)
declare sub SaveNPCL(file as string, dat() as npcinst)
declare sub DeserNPCL(npc() as npcinst, byref z as integer, buffer() as integer, byval num as integer, byval xoffset as integer, byval yoffset as integer)
declare sub CleanNPCInst(inst as NPCInst)
declare sub CleanNPCL(dat() as npcinst)

declare Sub SaveInventory16bit(invent() as InventSlot, byref z as integer, buf() as integer, byval first as integer=0, byval last as integer=-1)
declare Sub LoadInventory16Bit(invent() as InventSlot, byref z as integer, buf() as integer, byval first as integer=0, byval last as integer=-1)
declare sub serinventory8bit(invent() as inventslot, byref z as integer, buf() as integer)
declare sub deserinventory8bit(invent() as inventslot, byref z as integer, buf() as integer)
declare sub cleaninventory(invent() as inventslot)

'*** Requires construction + destruction ***
declare sub UnloadTilemap(map as TileMap)
declare sub UnloadTilemaps(layers() as TileMap)
declare sub LoadTilemap(map as TileMap, filename as string)
declare sub LoadTilemaps(layers() as TileMap, filename as string)
declare sub SaveTilemap(tmap as TileMap, filename as string)
declare sub SaveTilemaps(tmaps() as TileMap, filename as string)
declare sub CleanTilemap(map as TileMap, byval wide as integer, byval high as integer, byval layernum as integer = 0)
declare sub CleanTilemaps(layers() as TileMap, byval wide as integer, byval high as integer, byval numlayers as integer)
declare function GetTilemapInfo(filename as string, info as TilemapInfo) as integer
declare sub MergeTileMap(mine as TileMap, theirs_file as string, base_file as string)
declare sub MergeTileMaps(mine() as TileMap, theirs_file as string, base_file as string)


'*** Requires construction + destruction ***
declare sub CleanZoneMap(zmap as ZoneMap, byval wide as integer, byval high as integer)
declare sub DeleteZoneMap(zmap as ZoneMap)
declare function SetZoneTile(zmap as ZoneMap, byval id as integer, byval x as integer, byval y as integer) as integer
declare sub UnsetZoneTile(zmap as ZoneMap, byval id as integer, byval x as integer, byval y as integer)
declare function CheckZoneAtTile(zmap as ZoneMap, byval id as integer, byval x as integer, byval y as integer) as integer
declare sub GetZonesAtTile OVERLOAD (zmap as ZoneMap, zones() as integer, byval x as integer, byval y as integer)
declare function GetZonesAtTile OVERLOAD (zmap as ZoneMap, byval x as integer, byval y as integer) as integer vector
declare function GetZoneInfo(zmap as ZoneMap, byval id as integer) as ZoneInfo ptr
declare sub DebugZoneMap(zmap as ZoneMap, byval x as integer = -1, byval y as integer = -1)
declare sub ZoneToTileMap(zmap as ZoneMap, tmap as TileMap, byval id as integer, byval bitnum as integer)
declare sub SaveZoneMap(zmap as ZoneMap, filename as string, rsrect as RectType ptr = NULL)
declare sub LoadZoneMap(zmap as ZoneMap, filename as string)

declare SUB DeserDoorLinks(filename as string, array() as doorlink)
declare Sub SerDoorLinks(filename as string, array() as doorlink, byval withhead as integer = 1)
declare sub CleanDoorLinks(array() as doorlink)
declare Sub DeSerDoors(filename as string, array() as door, byval record as integer)
declare Sub SerDoors(filename as string, array() as door, byval record as integer)
declare Sub CleanDoors(array() as door)

declare Sub LoadStats(byval fh as integer, sta as stats ptr)
declare Sub SaveStats(byval fh as integer, sta as stats ptr)
declare Sub LoadStats2(byval fh as integer, lev0 as stats ptr, levMax as stats ptr)
declare Sub SaveStats2(byval fh as integer, lev0 as stats ptr, levMax as stats ptr)

declare Sub DeSerHeroDef(filename as string, hero as herodef ptr, byval record as integer)
declare Sub SerHeroDef(filename as string, hero as herodef ptr, byval record as integer)
declare sub loadherodata (hero as herodef ptr, byval index as integer)
declare sub saveherodata (hero as herodef ptr, byval index as integer)
declare sub ClearHeroData (hero as HeroDef)
declare function GetHeroHandPos(byval hero_id as integer, byval which_frame as integer, byval isy as integer) as integer

declare Sub LoadVehicle OVERLOAD (file as string, vehicle as VehicleData, byval record as integer)
declare Sub LoadVehicle OVERLOAD (file as string, veh() as integer, vehname as string, byval record as integer)
declare Sub SaveVehicle OVERLOAD (file as string, veh() as integer, vehname as string, byval record as integer)
declare Sub SaveVehicle OVERLOAD (file as string, vehicle as VehicleData, byval record as integer)
declare Sub ClearVehicle (vehicle as VehicleData)

declare Sub SaveUIColors (colarray() as integer, byval palnum as integer)
declare Sub LoadUIColors (colarray() as integer, byval palnum as integer=-1)
declare Sub DefaultUIColors (colarray() as integer)
declare Sub OldDefaultUIColors (colarray() as integer)
declare Sub GuessDefaultUIColors (colarray() as integer)

declare Sub LoadTextBox (byref box as TextBox, byval record as integer)
declare Sub SaveTextBox (byref box as TextBox, byval record as integer)
declare Sub ClearTextBox (byref box as TextBox)

DECLARE SUB loadoldattackelementalfail (byref cond as AttackElementCondition, buf() as integer, byval element as integer)
DECLARE SUB loadoldattackdata (array() as integer, byval index as integer)
DECLARE SUB saveoldattackdata (array() as integer, byval index as integer)
DECLARE SUB loadnewattackdata (array() as integer, byval index as integer)
DECLARE SUB savenewattackdata (array() as integer, byval index as integer)
DECLARE SUB loadattackdata OVERLOAD (array() as integer, byval index as integer)
DECLARE SUB loadattackdata OVERLOAD (byref atkdat as AttackData, byval index as integer)
DECLARE SUB SerAttackElementCond (cond as AttackElementCondition, buf() as integer, byval index as integer)
DECLARE SUB DeSerAttackElementCond (byref cond as AttackElementCondition, buf() as integer, byval index as integer)
DECLARE SUB convertattackdata(buf() as integer, byref atkdat as AttackData)
DECLARE SUB saveattackdata (array() as integer, byval index as integer)

DECLARE SUB loadtanim (byval n as integer, tastuf() as integer)
DECLARE SUB savetanim (byval n as integer, tastuf() as integer)

DECLARE SUB getpal16 (array() as integer, byval aoffset as integer, byval foffset as integer, byval autotype as integer=-1, byval sprite as integer=0)
DECLARE SUB storepal16 (array() as integer, byval aoffset as integer, byval foffset as integer)

DECLARE SUB loaditemdata (array() as integer, byval index as integer)
DECLARE SUB saveitemdata (array() as integer, byval index as integer)
DECLARE FUNCTION LoadOldItemElemental (itembuf() as integer, byval element as integer) as SINGLE
DECLARE SUB LoadItemElementals (byval index as integer, itemresists() as single)

DECLARE FUNCTION backcompat_element_dmg (byval weak as integer, byval strong as integer, byval absorb as integer) as double
DECLARE FUNCTION loadoldenemyresist (array() as integer, byval element as integer) as SINGLE
DECLARE SUB clearenemydata OVERLOAD (enemy as EnemyDef)
DECLARE SUB clearenemydata OVERLOAD (buf() as integer)
DECLARE SUB loadenemydata OVERLOAD (array() as integer, byval index as integer, byval altfile as integer = 0)
DECLARE SUB loadenemydata OVERLOAD (enemy as EnemyDef, byval index as integer, byval altfile as integer = 0)
DECLARE SUB saveenemydata OVERLOAD (array() as integer, byval index as integer, byval altfile as integer = 0)
DECLARE SUB saveenemydata OVERLOAD (enemy as EnemyDef, byval index as integer, byval altfile as integer = 0)

DECLARE SUB ClearFormation (form as Formation)
DECLARE SUB LoadFormation OVERLOAD (form as Formation, byval index as integer)
DECLARE SUB LoadFormation OVERLOAD (form as Formation, filename as string, byval index as integer)
DECLARE SUB SaveFormation OVERLOAD (form as Formation, byval index as integer)
DECLARE SUB SaveFormation OVERLOAD (form as Formation, filename as string, byval index as integer)
'index is formation set number, starting from 1!
DECLARE SUB LoadFormationSet (formset as FormationSet, byval index as integer)
DECLARE SUB SaveFormationSet (formset as FormationSet, byval index as integer)

DECLARE SUB load_hsp_header(filename as string, header as HSHeader)
DECLARE SUB load_lookup1_bin(table() as TriggerData)

DECLARE SUB save_string_list(array() as string, filename as string)
DECLARE SUB load_string_list(array() as string, filename as string)

DECLARE FUNCTION load_map_pos_save_offset(byval mapnum as integer) as XYPair

DECLARE SUB save_npc_locations OVERLOAD (filename as string, npc() as NPCInst)
DECLARE SUB save_npc_locations OVERLOAD (byval npcs_node as NodePtr, npc() as NPCInst)
DECLARE SUB save_npc_loc OVERLOAD (byval parent as NodePtr, byval index as integer, npc as NPCInst)
DECLARE SUB save_npc_loc OVERLOAD (byval parent as NodePtr, byval index as integer, npc as NPCInst, map_offset as XYPair)

DECLARE SUB load_npc_locations OVERLOAD (filename as string, npc() as NPCInst)
DECLARE SUB load_npc_locations OVERLOAD (byval npcs_node as NodePtr, npc() as NPCInst)
DECLARE SUB load_npc_loc OVERLOAD (byval n as NodePtr, npc as NPCInst)
DECLARE SUB load_npc_loc OVERLOAD (byval n as NodePtr, npc as NPCInst, map_offset as XYPair)

DECLARE FUNCTION load_gamename (filename as string="") as string
DECLARE FUNCTION load_aboutline (filename as string="") as string
DECLARE SUB save_gamename (s as string, filename as string="")
DECLARE SUB save_aboutline (s as string, filename as string="")

DECLARE SUB load_distrib_state OVERLOAD (byref distinfo as DistribState)
DECLARE SUB load_distrib_state OVERLOAD (byref distinfo as DistribState, filename as string)
DECLARE SUB load_distrib_state OVERLOAD (byref distinfo as DistribState, byval node as Reload.NodePtr)
DECLARE SUB save_distrib_state OVERLOAD (byref distinfo as DistribState)
DECLARE SUB save_distrib_state OVERLOAD (byref distinfo as DistribState, filename as string)
DECLARE SUB save_distrib_state OVERLOAD (byref distinfo as DistribState, byval node as Reload.NodePtr)

DECLARE SUB save_hero_as_reload(hero as HeroDef ptr, byval parent as NodePtr)
DECLARE SUB load_hero_from_reload(hero as HeroDef ptr, byval parent as NodePtr)

DECLARE FUNCTION WriteXYPairNode (byval parent as NodePtr, nodename as string, pair as XYPair) as NodePtr
DECLARE FUNCTION WritePicPalNode (byval parent as NodePtr, nodename as string, byval pic as integer, byval pal as integer=-1) as NodePtr
DECLARE FUNCTION WriteStatsNode (byval parent as NodePtr, nodename as string, statobj as Stats) as NodePtr

DECLARE SUB ReadStatsNode (byval stats as NodePtr, statobj as Stats)

#ENDIF
