#ifndef YETMORE2_BI
#define YETMORE2_BI

#include "reload.bi"

DECLARE FUNCTION cropmovement (byref x as integer, byref y as integer, byref xgo as integer, byref ygo as integer) as integer
DECLARE SUB defaultc
DECLARE SUB forcedismount (catd() as integer)
DECLARE FUNCTION framewalkabout (byval x as integer, byval y as integer, byref framex as integer, byref framey as integer, byval mapwide as integer, byval maphigh as integer, byval wrapmode as integer) as integer
DECLARE SUB initgamedefaults
DECLARE SUB innRestore ()
DECLARE SUB setmapxy
DECLARE SUB limitcamera (byref x as integer, byref y as integer)
DECLARE SUB showplotstrings
DECLARE SUB makebackups
DECLARE SUB make_map_backups
DECLARE SUB update_backdrop_slice
DECLARE SUB cleanuptemp
DECLARE FUNCTION checkfordeath () as integer
DECLARE SUB aheadxy (byref x as integer, byref y as integer, byval direction as integer, byval distance as integer)
DECLARE SUB exitprogram (byval need_fade_out as bool = NO, byval errorout as integer = 0)
DECLARE SUB keyboardsetup
DECLARE SUB verify_quit
DECLARE FUNCTION titlescreen () as integer
DECLARE SUB reloadnpc ()
DECLARE FUNCTION mapstatetemp(byval mapnum as integer, prefix as string) as string
DECLARE SUB savemapstate_gmap(byval mapnum as integer, prefix as string)
DECLARE SUB savemapstate_npcl OVERLOAD (byval mapnum as integer, prefix as string)
'DECLARE SUB savemapstate_npcl OVERLOAD (byval node as Reload.NodePtr)   'where did this come from?
DECLARE SUB savemapstate_npcd(byval mapnum as integer, prefix as string)
DECLARE SUB savemapstate_tilemap(byval mapnum as integer, prefix as string)
DECLARE SUB savemapstate_passmap(byval mapnum as integer, prefix as string)
DECLARE SUB savemapstate_zonemap(byval mapnum as integer, prefix as string)
DECLARE SUB savemapstate (byval mapnum as integer, byval savemask as integer = 255, prefix as string)
DECLARE SUB loadmapstate_gmap (byval mapnum as integer, prefix as string, byval dontfallback as integer = 0)
DECLARE SUB loadmapstate_npcl (byval mapnum as integer, prefix as string, byval dontfallback as integer = 0)
DECLARE SUB loadmapstate_npcd (byval mapnum as integer, prefix as string, byval dontfallback as integer = 0)
DECLARE SUB loadmapstate_tilemap (byval mapnum as integer, prefix as string, byval dontfallback as integer = 0)
DECLARE SUB loadmapstate_passmap (byval mapnum as integer, prefix as string, byval dontfallback as integer = 0)
DECLARE SUB loadmapstate_zonemap (byval mapnum as integer, prefix as string, byval dontfallback as integer = 0)
DECLARE SUB loadmapstate (byval mapnum as integer, byval loadmask as integer, prefix as string, byval dontfallback as integer = 0)
DECLARE SUB deletemapstate (byval mapnum as integer, byval killmask as integer, prefix as string)
DECLARE FUNCTION gmap_index_affects_tiles(byval index as integer) as integer
DECLARE SUB reloadmap_gmap_no_tilesets()
DECLARE SUB reloadmap_npcd()
DECLARE SUB reloadmap_npcl(byval merge as integer)
DECLARE SUB reloadmap_tilemap_and_tilesets(byval merge as integer)
DECLARE SUB reloadmap_passmap(byval merge as integer)
DECLARE SUB reloadmap_foemap()
DECLARE SUB reloadmap_zonemap()
DECLARE SUB deletetemps
DECLARE SUB debug_npcs ()
DECLARE SUB npc_debug_display ()
DECLARE FUNCTION game_setoption(opt as string, arg as string) as integer
DECLARE SUB handshake_with_master ()
DECLARE SUB show_wrong_spawned_version_error ()
DECLARE SUB check_game_custom_versions_match ()
DECLARE SUB receive_file_updates ()
DECLARE SUB try_to_reload_files_onmap ()
DECLARE SUB try_reload_lumps_anywhere ()
DECLARE FUNCTION compatpage() as integer
DECLARE SUB live_preview_menu ()


#endif
