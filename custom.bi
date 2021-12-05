'OHRRPGCE - Editors and functions defined throughout Custom (not in custom.bas)
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

'Many modules would only have a couple of functions (editors) in their header files,
'so those functions are declared here instead. See customsubs.bi for util functions.

#ifndef CUSTOM_BI
#define CUSTOM_BI

#include "config.bi"
#include "udts.bi"
#include "custom_udts.bi"

' record: which attack to show. If -1, default. If >= max, ask to add a new record,
' (and exit and return -1 if cancelled). Can also return -1 if not reentrant.
' Otherwise, returns the object number that was selected/last edited.
TYPE FnEditor as FUNCTION(record as integer = -1) as integer

' FnEditor editors
DECLARE FUNCTION enemy_editor (recindex as integer = -1) as integer
DECLARE FUNCTION enemy_picker (recindex as integer = -1) as integer
DECLARE FUNCTION enemy_picker_or_none (recindex as integer = -1) as integer
DECLARE FUNCTION attack_editor (recindex as integer = -1) as integer
DECLARE FUNCTION attack_picker (recindex as integer = -1) as integer
DECLARE FUNCTION attack_picker_or_none (recindex as integer = -1) as integer
DECLARE FUNCTION hero_picker (recindex as integer = -1) as integer
DECLARE FUNCTION hero_picker_or_none (recindex as integer = -1) as integer
DECLARE FUNCTION text_box_editor (whichbox as integer = -1) as integer
DECLARE FUNCTION textbox_picker (recindex as integer = -1) as integer
DECLARE FUNCTION textbox_picker_or_none (recindex as integer = -1, skip_zero as bool = NO) as integer
DECLARE FUNCTION item_picker (recindex as integer = -1) as integer
DECLARE FUNCTION item_picker_or_none (recindex as integer = -1) as integer
DECLARE FUNCTION shop_picker (recindex as integer = -1) as integer
DECLARE FUNCTION shop_picker_or_none (recindex as integer = -1) as integer
DECLARE FUNCTION sfx_picker (recindex as integer = -1) as integer
DECLARE FUNCTION sfx_picker_or_none (recindex as integer = -1) as integer
DECLARE FUNCTION song_picker (recindex as integer = -1) as integer
DECLARE FUNCTION song_picker_or_none (recindex as integer = -1) as integer
DECLARE FUNCTION formation_picker (recindex as integer = -1) as integer
DECLARE FUNCTION formation_picker_or_none (recindex as integer = -1) as integer


DECLARE SUB importmxs ()
DECLARE FUNCTION importmasterpal OVERLOAD (filename as string = "", palnum as integer) as bool
DECLARE SUB importmasterpal OVERLOAD (newmaster() as RGBcolor, palnum as integer)
DECLARE SUB vehicle_editor ()
DECLARE SUB script_management ()
DECLARE SUB map_picker ()
DECLARE SUB mapeditor (byval mapnum as integer)
DECLARE SUB edit_npc (npcdata as NPCType, gmap() as integer, zmap as ZoneMap)
DECLARE SUB maptile ()
DECLARE SUB spriteset_editor(sprtype as SpriteType)
DECLARE SUB backdrop_browser ()
DECLARE SUB song_editor_main()
DECLARE FUNCTION importsong (byval songnum as integer) as integer
DECLARE SUB sfx_editor_main()
DECLARE FUNCTION importsfx (byval sfxnum as integer) as integer
DECLARE SUB generalmusicsfxmenu ()
DECLARE SUB general_data_editor ()
DECLARE SUB global_text_strings_editor ()
DECLARE SUB item_editor ()
DECLARE FUNCTION individual_item_editor(item_id as integer) as integer
DECLARE SUB formation_editor ()
DECLARE FUNCTION individual_formation_editor (form_id as integer = -1) as integer
DECLARE FUNCTION formation_set_editor (set_id as integer = -1) as integer
DECLARE SUB hero_editor_main ()
DECLARE FUNCTION hero_editor (hero_id as integer) as integer
DECLARE SUB attack_editor_main ()
DECLARE SUB enemy_editor_main ()
DECLARE SUB foemap_stats_menu (foemap as TileMap, title as string)
DECLARE SUB shop_editor_main()
DECLARE SUB textbox_editor_main()
DECLARE SUB menu_editor ()
DECLARE SUB font_editor (font() as integer)
DECLARE SUB font_test_menu ()
DECLARE SUB resolution_menu ()
DECLARE SUB translations_menu ()
DECLARE SUB general_scripts_menu ()
DECLARE SUB script_error_mode_menu ()

DECLARE FUNCTION export_translations (fname as string) as bool
DECLARE FUNCTION compile_andor_import_scripts (f as string, quickimport as bool = NO) as bool
DECLARE SUB reimport_previous_scripts ()
DECLARE SUB write_session_info ()

DECLARE SUB common_elementals_editor(elementals() as single, helpfile as string, byval showsign as integer = 0)
DECLARE SUB tile_anim_draw_range(tastuf() as integer, byval taset as integer, byval page as integer)

DECLARE SUB frame_draw_with_background (src as Frame ptr, pal as Palette16 ptr = NULL, x as integer, y as integer, bgcolor as bgType, byref chequer_scroll as integer, dest as Frame ptr, opts as DrawOptions = def_drawoptions)
DECLARE FUNCTION bgcolor_caption(bgcolor as bgType) as string

DECLARE SUB Custom_volume_menu
DECLARE SUB Custom_global_menu
DECLARE SUB global_Custom_controls ()

DECLARE FUNCTION scriptbrowse (byref trigger as integer, byval triggertype as integer, scrtype as string, allow_default as bool = NO, default as integer = 0) as string
DECLARE FUNCTION scrintgrabber (byref n as integer, byval min as integer, byval max as integer, byval less as KBScancode=ccLeft, byval more as KBScancode=ccRight, byval scriptside as integer, byval triggertype as integer) as bool
DECLARE FUNCTION get_hspeak_version(hspeak_path as string) as string

DECLARE FUNCTION prompt_for_scancode () as KBScancode

'Note: Custom-specific global variables are in cglobals.bi

#endif
