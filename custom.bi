'OHRRPGCE - Editors and functions defined throughout Custom (not in custom.bas)
'Many modules would only have a couple of functions (editors) in their header files,
'so those functions are declared here instead. See customsubs.bi for util functions.
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#ifndef CUSTOM_BI
#define CUSTOM_BI

#include "config.bi"
#include "udts.bi"
#include "custom_udts.bi"

DECLARE SUB importbmp (f as string, cap as string, byref count as integer, sprtype as SpriteType)
DECLARE SUB vehicles ()
DECLARE SUB scriptman ()
DECLARE SUB map_picker ()
DECLARE SUB spriteset_editor (xw as integer, yw as integer, byref sets as integer, perset as integer, info() as string, fileset as SpriteType, fullset as bool = NO, cursor_start as integer = 0, cursor_top as integer = 0)
DECLARE SUB new_spriteset_editor()
DECLARE SUB backdrop_browser ()
DECLARE SUB importsong ()
DECLARE SUB importsfx ()
DECLARE SUB gendata ()
DECLARE SUB item_editor ()
DECLARE SUB formation_editor ()
DECLARE SUB enemydata ()
DECLARE SUB hero_editor ()
DECLARE SUB text_box_editor ()
DECLARE SUB maptile ()
DECLARE SUB compile_andor_import_scripts (f as string, quickimport as bool = NO)
DECLARE SUB reimport_previous_scripts ()
DECLARE SUB write_session_info ()

DECLARE SUB load_npc_graphics(npc_def() as NPCType, npc_img() as GraphicPair)
DECLARE SUB npcdef (st as MapEditState)
DECLARE SUB tile_anim_draw_range(tastuf() as integer, byval taset as integer, byval page as integer)

DECLARE SUB frame_draw_with_background (src as Frame ptr, pal as Palette16 ptr = NULL, x as integer, y as integer, scale as integer = 1, bgcolor as bgType, byref chequer_scroll as integer, dest as Frame ptr)
DECLARE FUNCTION bgcolor_caption(bgcolor as bgType) as string

#endif