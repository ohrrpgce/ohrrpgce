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
DECLARE SUB spriteset_editor (byval xw as integer, byval yw as integer, byref sets as integer, byval perset as integer, info() as string, byval zoom as integer, byval fileset as integer, byval fullset as integer=NO, byval cursor_start as integer=0, byval cursor_top as integer=0)
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

DECLARE SUB mapedit_update_npc_graphics(st as MapEditState, npc_img() as GraphicPair)

#endif