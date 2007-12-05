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

#endif