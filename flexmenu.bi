'OHRRPGCE - flexmenu system, used by attack and enemy editors
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#ifndef FLEXMENU_BI
#define FLEXMENU_BI

#include "udts.bi"

DECLARE SUB setactivemenu (workmenu() as integer, newmenu() as integer, byref state as MenuState)
DECLARE FUNCTION editflexmenu (state as MenuState, nowindex as integer, menutype() as integer, menuoff() as integer, menulimits() as integer, datablock() as integer, caption() as string, mintable() as integer, maxtable() as integer) as bool
DECLARE SUB updateflexmenu (mpointer as integer, nowmenu() as string, nowdat() as integer, size as integer, menu() as string, menutype() as integer, menuoff() as integer, menulimits() as integer, datablock() as integer, caption() as string, maxtable() as integer, recindex as integer, menucapoff() as integer)
DECLARE SUB flexmenu_update_selectable (workmenu() as integer, menutype() as integer, selectable() as bool)
DECLARE FUNCTION flexmenu_handle_crossrefs (state as MenuState, nowindex as integer, menutype() as integer, menuoff() as integer, recindex as integer, recbuf() as integer, is_attack_editor as bool) as bool
DECLARE SUB enforceflexbounds (menuoff() as integer, menutype() as integer, menulimits() as integer, recbuf() as integer, min() as integer, max() as integer)
DECLARE SUB addcaption (caption() as string, byref indexer as integer, cap as string)
DECLARE FUNCTION isStringField(byval mnu as integer) as bool
DECLARE FUNCTION flexmenu_tooltip(menutype as integer, datvalue as integer) as string


#ENDIF
