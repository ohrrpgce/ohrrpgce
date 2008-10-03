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
DECLARE FUNCTION isonscreen (x as integer, y as integer) as integer
DECLARE SUB setmapxy
DECLARE SUB limitcamera 
DECLARE SUB setScriptArg (arg as integer, value as integer)
DECLARE SUB showplotstrings
DECLARE FUNCTION str2int (stri as string) as integer
DECLARE FUNCTION str2lng (stri as string) as long
DECLARE FUNCTION strgrabber (s as string, maxl as integer) AS INTEGER
DECLARE SUB makebackups
DECLARE SUB correctbackdrop
DECLARE SUB cleanuptemp
DECLARE FUNCTION checkfordeath (stat()) as integer
DECLARE SUB aheadxy (x, y, direction, distance)
DECLARE SUB exitprogram (needfade)
DECLARE SUB keyboardsetup
DECLARE SUB verquit
DECLARE FUNCTION titlescr as integer
DECLARE SUB reloadnpc (stat())
DECLARE FUNCTION mapstatetemp(mapnum, prefix as string) as string
DECLARE SUB savemapstate_gmap(mapnum, prefix as string)
DECLARE SUB savemapstate_npcl(mapnum, prefix as string)
DECLARE SUB savemapstate_npcd(mapnum, prefix as string)
DECLARE SUB savemapstate_tilemap(mapnum, prefix as string)
DECLARE SUB savemapstate_passmap(mapnum, prefix as string)
DECLARE SUB savemapstate (mapnum, savemask = 255, prefix as string)
DECLARE SUB loadmapstate_gmap (mapnum, prefix as string, dontfallback = 0)
DECLARE SUB loadmapstate_npcl (mapnum, prefix as string, dontfallback = 0)
DECLARE SUB loadmapstate_npcd (mapnum, prefix as string, dontfallback = 0)
DECLARE SUB loadmapstate_tilemap (mapnum, prefix as string, dontfallback = 0)
DECLARE SUB loadmapstate_passmap (mapnum, prefix as string, dontfallback = 0)
DECLARE SUB loadmapstate (mapnum, loadmask, prefix as string, dontfallback = 0)
DECLARE SUB deletemapstate (mapnum, killmask, prefix as string)
DECLARE SUB deletetemps
DECLARE FUNCTION decodetrigger (trigger, trigtype) as integer
DECLARE SUB killallscripts
DECLARE SUB resetinterpreter
DECLARE SUB reloadscript (si as ScriptInst, updatestats as integer = -1)
DECLARE SUB debug_npcs ()
DECLARE SUB npc_debug_display ()

#endif