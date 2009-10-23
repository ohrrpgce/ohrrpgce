'OHRRPGCE - Some Custom menu code
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#ifndef FLEXMENU_BI
#define FLEXMENU_BI

#include "udts.bi"

DECLARE SUB menu_editor ()
DECLARE SUB update_menu_editor_menu(record, edmenu AS MenuDef, menu AS MenuDef)
DECLARE SUB update_detail_menu(detail AS MenuDef, mi AS MenuDefItem)
DECLARE SUB menu_editor_keys (state AS MenuState, mstate AS MenuState, menudata AS MenuDef, record, menu_set AS MenuSet)
DECLARE SUB menu_editor_menu_keys (mstate AS MenuState, dstate AS MenuState, menudata AS MenuDef, record AS INTEGER)
DECLARE SUB menu_editor_detail_keys(dstate AS MenuState, mstate AS MenuState, detail AS MenuDef, mi AS MenuDefItem)

DECLARE SUB setactivemenu (workmenu(), newmenu(), BYREF state AS MenuState)
DECLARE SUB flexmenu_skipper (BYREF state AS MenuState, workmenu(), menutype())

DECLARE SUB atk_edit_preview(BYVAL pattern AS INTEGER, sl AS Slice Ptr)
DECLARE SUB atk_edit_pushptr(state AS MenuState, laststate AS MenuState, BYREF menudepth AS INTEGER)
DECLARE SUB atk_edit_backptr(workmenu() AS INTEGER, mainMenu() AS INTEGER, state AS MenuState, laststate AS menustate, BYREF menudepth AS INTEGER)

DECLARE FUNCTION editflexmenu (nowindex AS INTEGER, menutype() AS INTEGER, menuoff() AS INTEGER, menulimits() AS INTEGER, datablock() AS INTEGER, mintable() AS INTEGER, maxtable() AS INTEGER) AS INTEGER
DECLARE SUB updateflexmenu (mpointer AS INTEGER, nowmenu() AS STRING, nowdat() AS INTEGER, size AS INTEGER, menu() AS STRING, menutype() AS INTEGER, menuoff() AS INTEGER, menulimits() AS INTEGER, datablock() AS INTEGER, caption() AS STRING, maxtable() AS INTEGER, recindex AS INTEGER)
DECLARE SUB enforceflexbounds (menuoff() AS INTEGER, menutype() AS INTEGER, menulimits() AS INTEGER, recbuf() AS INTEGER, min() AS INTEGER, max() AS INTEGER)
DECLARE SUB addcaption (caption() AS STRING, indexer AS INTEGER, cap AS STRING)
DECLARE SUB attackdata ()
DECLARE FUNCTION isStringField(mnu AS INTEGER)


#ENDIF
