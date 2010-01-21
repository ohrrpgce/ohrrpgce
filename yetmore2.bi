#ifndef YETMORE2_BI
#define YETMORE2_BI

DECLARE SUB cathero
DECLARE FUNCTION cropmovement (x as integer, y as integer, xgo as integer, ygo as integer) as integer
DECLARE SUB defaultc
DECLARE SUB drawnpcs
DECLARE SUB forcedismount (catd() as integer)
DECLARE FUNCTION framewalkabout (x as integer, y as integer, framex as integer, framey as integer, mapwide as integer, maphigh as integer, wrapmode as integer) as integer
DECLARE SUB initgamedefaults
DECLARE SUB innRestore (stat() as integer)
DECLARE SUB setmapxy
DECLARE SUB limitcamera (BYREF x AS INTEGER, BYREF y AS INTEGER)
DECLARE SUB showplotstrings
DECLARE FUNCTION strgrabber (s as string, maxl as integer) AS INTEGER
DECLARE SUB makebackups
DECLARE SUB correctbackdrop
DECLARE SUB cleanuptemp
DECLARE FUNCTION checkfordeath (stat() as integer) as integer
DECLARE SUB aheadxy (x as integer, y as integer, direction as integer, distance as integer)
DECLARE SUB exitprogram (needfade as integer)
DECLARE SUB keyboardsetup
DECLARE SUB verquit
DECLARE FUNCTION titlescr as integer
DECLARE SUB reloadnpc (stat() as integer)
DECLARE FUNCTION mapstatetemp(mapnum as integer, prefix as string) as string
DECLARE SUB savemapstate_gmap(mapnum as integer, prefix as string)
DECLARE SUB savemapstate_npcl(mapnum as integer, prefix as string)
DECLARE SUB savemapstate_npcd(mapnum as integer, prefix as string)
DECLARE SUB savemapstate_tilemap(mapnum as integer, prefix as string)
DECLARE SUB savemapstate_passmap(mapnum as integer, prefix as string)
DECLARE SUB savemapstate (mapnum as integer, savemask as integer = 255, prefix as string)
DECLARE SUB loadmapstate_gmap (mapnum as integer, prefix as string, dontfallback as integer = 0)
DECLARE SUB loadmapstate_npcl (mapnum as integer, prefix as string, dontfallback as integer = 0)
DECLARE SUB loadmapstate_npcd (mapnum as integer, prefix as string, dontfallback as integer = 0)
DECLARE SUB loadmapstate_tilemap (mapnum as integer, prefix as string, dontfallback as integer = 0)
DECLARE SUB loadmapstate_passmap (mapnum as integer, prefix as string, dontfallback as integer = 0)
DECLARE SUB loadmapstate (mapnum as integer, loadmask as integer, prefix as string, dontfallback as integer = 0)
DECLARE SUB deletemapstate (mapnum as integer, killmask as integer, prefix as string)
DECLARE SUB deletetemps
DECLARE FUNCTION decodetrigger (trigger as integer, trigtype as integer) as integer
DECLARE SUB debug_npcs ()
DECLARE SUB npc_debug_display ()
DECLARE FUNCTION game_setoption(opt as string, arg as string) as integer
DECLARE FUNCTION compatpage() as integer

#endif
