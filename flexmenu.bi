'OHRRPGCE - Some Custom menu code
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#ifndef FLEXMENU_BI
#define FLEXMENU_BI

#include "udts.bi"

DECLARE SUB menu_editor ()
DECLARE SUB update_menu_editor_menu(byval record as integer, edmenu as MenuDef, menu as MenuDef)
DECLARE SUB update_detail_menu(detail as MenuDef, mi as MenuDefItem)
DECLARE SUB menu_editor_keys (state as MenuState, mstate as MenuState, menudata as MenuDef, byref record as integer, menu_set as MenuSet)
DECLARE SUB menu_editor_menu_keys (mstate as MenuState, dstate as MenuState, menudata as MenuDef, byval record as integer)
DECLARE SUB menu_editor_detail_keys(dstate as MenuState, mstate as MenuState, detail as MenuDef, mi as MenuDefItem)

DECLARE SUB setactivemenu (workmenu() as integer, newmenu() as integer, byref state as MenuState)

DECLARE SUB atk_edit_preview(byval pattern as integer, sl as Slice Ptr)
DECLARE SUB atk_edit_pushptr(state as MenuState, laststate as MenuState, byref menudepth as integer)
DECLARE SUB atk_edit_backptr(workmenu() as integer, mainMenu() as integer, state as MenuState, laststate as menustate, byref menudepth as integer)

DECLARE FUNCTION editflexmenu (nowindex as integer, menutype() as integer, menuoff() as integer, menulimits() as integer, datablock() as integer, caption() as string, mintable() as integer, maxtable() as integer) as integer
DECLARE SUB updateflexmenu (mpointer as integer, nowmenu() as string, nowdat() as integer, size as integer, menu() as string, menutype() as integer, menuoff() as integer, menulimits() as integer, datablock() as integer, caption() as string, maxtable() as integer, recindex as integer)
DECLARE SUB flexmenu_update_selectable (workmenu() as integer, menutype() as integer, selectable() as bool)
DECLARE SUB enforceflexbounds (menuoff() as integer, menutype() as integer, menulimits() as integer, recbuf() as integer, min() as integer, max() as integer)
DECLARE SUB addcaption (caption() as string, byref indexer as integer, cap as string)
DECLARE SUB attackdata ()
DECLARE FUNCTION isStringField(byval mnu as integer) as integer


#ENDIF
