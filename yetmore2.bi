#ifndef YETMORE2_BI
#define YETMORE2_BI

#include "reload.bi"

DECLARE FUNCTION cropmovement (x as integer, y as integer, xgo as integer, ygo as integer) as integer
DECLARE SUB defaultc
DECLARE SUB forcedismount (catd() as integer)
DECLARE FUNCTION framewalkabout (x as integer, y as integer, framex as integer, framey as integer, mapwide as integer, maphigh as integer, wrapmode as integer) as integer
DECLARE SUB initgamedefaults
DECLARE SUB innRestore ()
DECLARE SUB setmapxy
DECLARE SUB limitcamera (BYREF x AS INTEGER, BYREF y AS INTEGER)
DECLARE SUB showplotstrings
DECLARE SUB makebackups
DECLARE SUB correctbackdrop
DECLARE SUB cleanuptemp
DECLARE FUNCTION checkfordeath () as integer
DECLARE SUB aheadxy (x as integer, y as integer, direction as integer, distance as integer)
DECLARE SUB exitprogram (BYVAL needfade as integer, BYVAL errorout as integer = NO)
DECLARE SUB keyboardsetup
DECLARE SUB verquit
DECLARE FUNCTION titlescr as integer
DECLARE SUB reloadnpc ()
DECLARE FUNCTION mapstatetemp(mapnum as integer, prefix as string) as string
DECLARE SUB savemapstate_gmap(mapnum as integer, prefix as string)
DECLARE SUB savemapstate_npcl OVERLOAD (mapnum as integer, prefix as string)
DECLARE SUB savemapstate_npcl OVERLOAD (BYVAL node AS Reload.NodePtr)
DECLARE SUB savemapstate_npcd(mapnum as integer, prefix as string)
DECLARE SUB savemapstate_tilemap(mapnum as integer, prefix as string)
DECLARE SUB savemapstate_passmap(mapnum as integer, prefix as string)
DECLARE SUB savemapstate_zonemap(mapnum as integer, prefix as string)
DECLARE SUB savemapstate (mapnum as integer, savemask as integer = 255, prefix as string)
DECLARE SUB loadmapstate_gmap (mapnum as integer, prefix as string, dontfallback as integer = 0)
DECLARE SUB loadmapstate_npcl (mapnum as integer, prefix as string, dontfallback as integer = 0)
DECLARE SUB loadmapstate_npcd (mapnum as integer, prefix as string, dontfallback as integer = 0)
DECLARE SUB loadmapstate_tilemap (mapnum as integer, prefix as string, dontfallback as integer = 0)
DECLARE SUB loadmapstate_passmap (mapnum as integer, prefix as string, dontfallback as integer = 0)
DECLARE SUB loadmapstate_zonemap (mapnum as integer, prefix as string, dontfallback as integer = 0)
DECLARE SUB loadmapstate (mapnum as integer, loadmask as integer, prefix as string, dontfallback as integer = 0)
DECLARE SUB deletemapstate (mapnum as integer, killmask as integer, prefix as string)
DECLARE SUB deletetemps
DECLARE FUNCTION decodetrigger (trigger as integer, trigtype as integer) as integer
DECLARE SUB debug_npcs ()
DECLARE SUB npc_debug_display ()
DECLARE FUNCTION game_setoption(opt as string, arg as string) as integer
DECLARE SUB handshake_with_master ()
DECLARE SUB show_wrong_spawned_version_error ()
DECLARE SUB check_game_custom_versions_match ()
DECLARE SUB receive_file_updates ()
DECLARE SUB try_to_reload_files_onmap ()
DECLARE FUNCTION compatpage() as integer
DECLARE SUB load_fset_frequencies ()


#endif
