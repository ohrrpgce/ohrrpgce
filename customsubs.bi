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
DECLARE SUB ui_color_editor()
DECLARE SUB make_ui_color_editor_menu(m() AS STRING, colors() AS INTEGER)
DECLARE FUNCTION int_from_xy(pos AS XYPair, wide AS INTEGER, high AS INTEGER) AS INTEGER
DECLARE FUNCTION xy_from_int(n AS INTEGER, wide AS INTEGER, high AS INTEGER) AS XYPair
DECLARE FUNCTION color_browser_256(start_color AS INTEGER=0) AS INTEGER
DECLARE FUNCTION yesno(capt AS STRING, defaultval AS INTEGER=YES, escval AS INTEGER=NO) AS INTEGER
DECLARE FUNCTION pick_ogg_quality(BYREF quality AS INTEGER) AS INTEGER
DECLARE FUNCTION needaddset (BYREF pt AS INTEGER, BYREF check AS INTEGER, what AS STRING) AS INTEGER
DECLARE SUB cycletile (tanim_state() AS TileAnimState, tastuf() AS INTEGER)

#endif