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
declare sub LoadNPCD_fixedlen(file as string, dat() as NPCType, BYREF arraylen as integer)
declare sub SaveNPCD(file as string, dat() as NPCType)
declare sub SaveNPCD_fixedlen(file as string, dat() as NPCType, BYVAL arraylen as integer)
declare sub setnpcd(npcd as npctype, offset as integer, value as integer)
declare function getnpcd(npcd as npctype, offset as integer) as integer
declare sub CleanNPCDefinition(dat as NPCType)
declare sub CleanNPCD(dat() as NPCType)

declare sub LoadNPCL(file as string, dat() as npcinst)
declare sub SaveNPCL(file as string, dat() as npcinst)
declare sub DeserNPCL(npc() as npcinst, z as integer, buffer() as integer, num as integer, xoffset as integer, yoffset as integer)
declare sub CleanNPCInst(inst as NPCInst)
declare sub CleanNPCL(dat() as npcinst, byval num as integer=-1)

declare Sub SaveInventory16bit(invent() AS InventSlot, BYREF z AS INTEGER, buf() AS INTEGER, BYVAL first AS INTEGER=0, BYVAL last AS INTEGER=-1)
declare Sub LoadInventory16Bit(invent() AS InventSlot, BYREF z AS INTEGER, buf() AS INTEGER, BYVAL first AS INTEGER=0, BYVAL last AS INTEGER=-1)
declare sub serinventory8bit(invent() as inventslot, z as integer, buf() as integer)
declare sub deserinventory8bit(invent() as inventslot, z as integer, buf() as integer)
declare sub cleaninventory(invent() as inventslot)

'*** Requires construction + destruction ***
declare sub UnloadTilemap(map as TileMap)
declare sub UnloadTilemaps(layers() as TileMap)
declare sub LoadTilemap(map as TileMap, filename as string)
declare sub LoadTilemaps(layers() as TileMap, filename as string)
declare sub SaveTilemap(tmap as TileMap, filename as string)
declare sub SaveTilemaps(tmaps() as TileMap, filename as string)
declare sub CleanTilemap(map as TileMap, BYVAL wide as integer, BYVAL high as integer, BYVAL layernum as integer = 0)
declare sub CleanTilemaps(layers() as TileMap, BYVAL wide as integer, BYVAL high as integer, BYVAL numlayers as integer)
declare function GetTilemapInfo(filename as string, info as TilemapInfo) as integer

'*** Requires construction + destruction ***
declare sub CleanZoneMap(zmap as ZoneMap, BYVAL wide as integer, BYVAL high as integer)
declare sub DeleteZoneMap(zmap as ZoneMap)
declare function SetZoneTile(zmap as ZoneMap, BYVAL id as integer, BYVAL x as integer, BYVAL y as integer) as integer
declare sub UnsetZoneTile(zmap as ZoneMap, BYVAL id as integer, BYVAL x as integer, BYVAL y as integer)
declare function CheckZoneAtTile(zmap as ZoneMap, BYVAL id as integer, BYVAL x as integer, BYVAL y as integer) as integer
declare sub GetZonesAtTile(zmap as ZoneMap, zones() as integer, BYVAL x as integer, BYVAL y as integer)
declare function GetZoneInfo(zmap as ZoneMap, BYVAL id as integer) as ZoneInfo ptr
declare sub DebugZoneMap(zmap as ZoneMap, BYVAL x as integer = -1, BYVAL y as integer = -1)
declare sub ZoneToTileMap(zmap as ZoneMap, tmap as TileMap, BYVAL id as integer, BYVAL bitnum as integer)
declare sub SaveZoneMap(zmap as ZoneMap, filename as string, rsrect as RectType ptr = NULL)
declare sub LoadZoneMap(zmap as ZoneMap, filename as string)

declare SUB DeserDoorLinks(filename as string, array() as doorlink)
declare Sub SerDoorLinks(filename as string, array() as doorlink, withhead as integer = 1)
declare sub CleanDoorLinks(array() as doorlink)
declare Sub DeSerDoors(filename as string, array() as door, record as integer)
declare Sub SerDoors(filename as string, array() as door, record as integer)
declare Sub CleanDoors(array() as door)

declare Sub LoadStats(fh as integer, sta as stats ptr)
declare Sub SaveStats(fh as integer, sta as stats ptr)
declare Sub LoadStats2(fh as integer, lev0 as stats ptr, levMax as stats ptr)
declare Sub SaveStats2(fh as integer, lev0 as stats ptr, levMax as stats ptr)

declare Sub DeSerHeroDef(filename as string, hero as herodef ptr, record as integer)
declare Sub SerHeroDef(filename as string, hero as herodef ptr, record as integer)
declare sub loadherodata (hero as herodef ptr, index as integer)
declare sub saveherodata (hero as herodef ptr, index as integer)

declare Sub LoadVehicle OVERLOAD (file AS STRING, vehicle AS VehicleData, record AS INTEGER)
declare Sub LoadVehicle OVERLOAD (file AS STRING, veh() as integer, vehname as string, record AS INTEGER)
declare Sub SaveVehicle OVERLOAD (file AS STRING, veh() as integer, vehname as string, record AS INTEGER)
declare Sub SaveVehicle OVERLOAD (file AS STRING, vehicle AS VehicleData, record AS INTEGER)
declare Sub ClearVehicle (vehicle AS VehicleData)

declare Sub SaveUIColors (colarray() AS INTEGER, palnum AS INTEGER)
declare Sub LoadUIColors (colarray() AS INTEGER, palnum AS INTEGER=-1)
declare Sub DefaultUIColors (colarray() AS INTEGER)
declare Sub OldDefaultUIColors (colarray() AS INTEGER)
declare Sub GuessDefaultUIColors (colarray() AS INTEGER)

declare Sub LoadTextBox (BYREF box AS TextBox, record AS INTEGER)
declare Sub SaveTextBox (BYREF box AS TextBox, record AS INTEGER)
declare Sub ClearTextBox (BYREF box AS TextBox)

DECLARE SUB loadoldattackelementalfail (BYREF cond as AttackElementCondition, buf() as integer, BYVAL element as integer)
DECLARE SUB loadoldattackdata (array() as integer, index as integer)
DECLARE SUB saveoldattackdata (array() as integer, index as integer)
DECLARE SUB loadnewattackdata (array() as integer, index as integer)
DECLARE SUB savenewattackdata (array() as integer, index as integer)
DECLARE SUB loadattackdata OVERLOAD (array() as integer, BYVAL index as integer)
DECLARE SUB loadattackdata OVERLOAD (BYREF atkdat as AttackData, BYVAL index as integer)
DECLARE SUB SerAttackElementCond (cond as AttackElementCondition, buf() as integer, BYVAL index as integer)
DECLARE SUB DeSerAttackElementCond (BYREF cond as AttackElementCondition, buf() as integer, BYVAL index as integer)
DECLARE SUB convertattackdata(buf() AS INTEGER, BYREF atkdat AS AttackData)
DECLARE SUB saveattackdata (array() as integer, index as integer)

DECLARE SUB loadtanim (n as integer, tastuf() as integer)
DECLARE SUB savetanim (n as integer, tastuf() as integer)

DECLARE SUB getpal16 (array() as integer, aoffset as integer, foffset as integer, autotype as integer=-1, sprite as integer=0)
DECLARE SUB storepal16 (array() as integer, aoffset as integer, foffset as integer)

DECLARE SUB loaditemdata (array() as integer, index as integer)
DECLARE SUB saveitemdata (array() as integer, index as integer)
DECLARE FUNCTION LoadOldItemElemental (itembuf() AS INTEGER, BYVAL element AS INTEGER) AS SINGLE
DECLARE SUB LoadItemElementals (BYVAL index as integer, itemresists() as single)

DECLARE FUNCTION backcompat_element_dmg (BYVAL weak as integer, BYVAL strong as integer, BYVAL absorb as integer) as double
DECLARE FUNCTION loadoldenemyresist (array() AS INTEGER, BYVAL element AS INTEGER) AS SINGLE
DECLARE SUB clearenemydata OVERLOAD (enemy AS EnemyDef)
DECLARE SUB clearenemydata OVERLOAD (buf() AS INTEGER)
DECLARE SUB loadenemydata OVERLOAD (array() as integer, index as integer, altfile as integer = 0)
DECLARE SUB loadenemydata OVERLOAD (enemy AS EnemyDef, index AS INTEGER, altfile AS INTEGER = 0)
DECLARE SUB saveenemydata OVERLOAD (array() as integer, index as integer, altfile as integer = 0)
DECLARE SUB saveenemydata OVERLOAD (enemy AS EnemyDef, index as integer, altfile as integer = 0)

DECLARE SUB save_string_list(array() AS STRING, filename AS STRING)
DECLARE SUB load_string_list(array() AS STRING, filename AS STRING)

DECLARE FUNCTION load_map_pos_save_offset(BYVAL mapnum AS INTEGER) AS XYPair

DECLARE SUB save_npc_locations OVERLOAD (filename AS STRING, npc() AS NPCInst)
DECLARE SUB save_npc_locations OVERLOAD (BYVAL npcs_node AS NodePtr, npc() AS NPCInst)
DECLARE SUB save_npc_loc OVERLOAD (BYVAL parent AS NodePtr, BYVAL index AS integer, npc AS NPCInst)
DECLARE SUB save_npc_loc OVERLOAD (BYVAL parent AS NodePtr, BYVAL index AS integer, npc AS NPCInst, map_offset AS XYPair)

DECLARE SUB load_npc_locations OVERLOAD (filename AS STRING, npc() AS NPCInst)
DECLARE SUB load_npc_locations OVERLOAD (BYVAL npcs_node AS NodePtr, npc() AS NPCInst)
DECLARE SUB load_npc_loc OVERLOAD (BYVAL n AS NodePtr, npc AS NPCInst)
DECLARE SUB load_npc_loc OVERLOAD (BYVAL n AS NodePtr, npc AS NPCInst, map_offset AS XYPair)

#ENDIF
