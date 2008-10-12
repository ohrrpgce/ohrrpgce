'OHRRPGCE - Some Custom common code
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#ifndef CUSTOMSUBS_BI
#define CUSTOMSUBS_BI

#include "const.bi"

DECLARE FUNCTION tag_grabber (BYREF n AS INTEGER, min AS INTEGER=-999, max AS INTEGER=999) AS INTEGER
DECLARE FUNCTION tagnames (starttag AS INTEGER=0, picktag AS INTEGER=NO) AS INTEGER
DECLARE FUNCTION strgrabber (s AS STRING, maxl AS INTEGER) AS INTEGER
DECLARE FUNCTION charpicker() AS STRING
DECLARE SUB ui_color_editor(palnum AS INTEGER)
DECLARE SUB make_ui_color_editor_menu(m() AS STRING, colors() AS INTEGER)
DECLARE FUNCTION int_from_xy(pos AS XYPair, wide AS INTEGER, high AS INTEGER) AS INTEGER
DECLARE FUNCTION xy_from_int(n AS INTEGER, wide AS INTEGER, high AS INTEGER) AS XYPair
DECLARE FUNCTION color_browser_256(start_color AS INTEGER=0) AS INTEGER
DECLARE FUNCTION yesno(capt AS STRING, defaultval AS INTEGER=YES, escval AS INTEGER=NO) AS INTEGER
DECLARE FUNCTION pick_ogg_quality(BYREF quality AS INTEGER) AS INTEGER
DECLARE FUNCTION needaddset (BYREF pt AS INTEGER, BYREF check AS INTEGER, what AS STRING) AS INTEGER
DECLARE SUB keyboardsetup ()
DECLARE FUNCTION load_vehicle_name(vehID AS INTEGER) AS STRING
DECLARE FUNCTION load_item_name (it AS INTEGER, hidden AS INTEGER, offbyone AS INTEGER) AS STRING
DECLARE FUNCTION textbox_preview_line OVERLOAD (boxnum AS INTEGER) AS STRING
DECLARE FUNCTION textbox_preview_line OVERLOAD (box AS TextBox) AS STRING
DECLARE SUB onetimetog(BYREF tagnum AS INTEGER)
DECLARE SUB edit_npc (npcid AS INTEGER, npc() AS INTEGER)
DECLARE FUNCTION pal16browse (BYVAL curpal AS INTEGER, BYVAL picset AS INTEGER, BYVAL picnum AS INTEGER, BYVAL picframes AS INTEGER, BYVAL picw AS INTEGER, BYVAL pich AS INTEGER) AS INTEGER
DECLARE FUNCTION step_estimate(freq AS INTEGER, low AS INTEGER, high AS INTEGER, infix AS STRING="-", suffix AS STRING= "", zero AS STRING="never") AS STRING
DECLARE FUNCTION speed_estimate(speed AS INTEGER, suffix AS STRING=" seconds", zero AS STRING="infinity") AS STRING
DECLARE SUB load_text_box_portrait (BYREF box AS TextBox, BYREF gfx AS GraphicPair)
DECLARE FUNCTION fixfilename (s AS STRING) AS STRING
DECLARE FUNCTION inputfilename (query AS STRING, ext AS STRING, default AS STRING="", check_for_existing AS INTEGER=YES) AS STRING
DECLARE FUNCTION export_textboxes (filename AS STRING, metadata() AS INTEGER) AS INTEGER
DECLARE FUNCTION import_textboxes (filename AS STRING, BYREF warn AS STRING) AS INTEGER
DECLARE FUNCTION askwhatmetadata (metadata() AS INTEGER, metadatalabels() AS STRING) AS INTEGER
DECLARE FUNCTION str2bool(q AS STRING, default AS INTEGER = NO, invert AS INTEGER = NO) AS INTEGER
DECLARE SUB xy_position_on_sprite (spr AS GraphicPair, BYREF x AS INTEGER, BYREF y AS INTEGER, BYVAL frame AS INTEGER, BYVAL wide AS INTEGER, byval high AS INTEGER, caption AS STRING)
DECLARE SUB edit_menu_bits (menu AS MenuDef)
DECLARE SUB edit_menu_item_bits (mi AS MenuDefItem)
DECLARE SUB reposition_menu (menu AS MenuDef, mstate AS MenuState)
DECLARE SUB reposition_anchor (menu AS MenuDef, mstate AS MenuState)
DECLARE FUNCTION tag_toggle_caption(n AS INTEGER, prefix AS STRING="Toggle tag") AS STRING
DECLARE SUB editbitset (array() AS INTEGER, BYVAL wof AS INTEGER, BYVAL last AS INTEGER, names() AS STRING)
DECLARE FUNCTION scriptbrowse (BYREF trigger AS INTEGER, BYVAL triggertype AS INTEGER, scrtype AS STRING) AS STRING
DECLARE FUNCTION scrintgrabber (BYREF n AS INTEGER, BYVAL min AS INTEGER, BYVAL max AS INTEGER, BYVAL less AS INTEGER=75, BYVAL more AS INTEGER=77, BYVAL scriptside AS INTEGER, BYVAL triggertype AS INTEGER) AS INTEGER

#endif
