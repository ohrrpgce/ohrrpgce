#ifndef MORESUBS_BI
#define MORESUBS_BI

#include "gfx.bi"

DECLARE SUB addhero (byval who as integer, byval slot as integer, byval forcelevel as integer=-1)
DECLARE FUNCTION averagelev () as integer
DECLARE SUB calibrate

DECLARE FUNCTION consumeitem (byval invslot as integer) as bool
DECLARE FUNCTION countitem (byval item_id as integer) as integer
DECLARE SUB getitem (byval item_id as integer, byval num as integer=1)
DECLARE SUB delitem (byval item_id as integer, byval amount as integer=1)
DECLARE FUNCTION room_for_item (byval itemid as integer, byval num as integer = 1) as bool

DECLARE SUB doswap (byval s as integer, byval d as integer)
DECLARE SUB update_textbox ()
DECLARE SUB party_change_updates ()
DECLARE SUB evalherotags ()
DECLARE SUB evalitemtags ()
DECLARE FUNCTION findhero (byval who as integer, byval first as integer, byval last as integer, byval direction as integer) as integer
DECLARE SUB hero_swap_menu (byval reserve_too as bool)
DECLARE SUB settag OVERLOAD (byval tagnum as integer, byval value as integer = 4444)
DECLARE SUB settag OVERLOAD (tagbits() as integer, byval tagnum as integer, byval value as integer = 4444)
DECLARE FUNCTION istag OVERLOAD (byval num as integer, byval zero as integer=NO) as integer
DECLARE FUNCTION istag OVERLOAD (tagbits() as integer, byval num as integer, byval zero as integer=NO) as integer
DECLARE SUB loaddoor (byval map as integer)
DECLARE SUB minimap (byval x as integer, byval y as integer)
DECLARE FUNCTION teleporttool () as integer
DECLARE FUNCTION onwho (caption as string, byval alone as integer) as integer
DECLARE SUB readjoysettings
DECLARE SUB renamehero (byval who as integer, byval escapable as integer)
DECLARE SUB resetgame ()
DECLARE SUB get_max_levelmp (ret() as integer, byval hero_level as integer)
DECLARE SUB resetlmp (byval slot as integer, byval hero_level as integer)
DECLARE SUB reset_game_state ()
DECLARE SUB reset_map_state (map as MapModeState)

DECLARE FUNCTION settingstring (searchee as string, setting as string, result as string) as integer
DECLARE SUB shop (byval id as integer)
DECLARE FUNCTION useinn (byval price as integer, byval holdscreen as integer) as integer
DECLARE SUB tagdisplay
DECLARE SUB writejoysettings
DECLARE FUNCTION herocount (byval last as integer = 3) as integer
DECLARE FUNCTION caterpillar_size () as integer

DECLARE FUNCTION gamepadmap_from_reload(gamepad as NodePtr, byval use_dpad as bool=NO) as GamePadMap
DECLARE FUNCTION use_touch_textboxes() as bool
DECLARE FUNCTION should_disable_virtual_gamepad() as bool
DECLARE FUNCTION should_hide_virtual_gamepad_when_suspendplayer() as bool
DECLARE SUB remap_virtual_gamepad(nodename as string, byval in_battle as bool=NO)

DECLARE FUNCTION default_margin() as integer
DECLARE FUNCTION default_margin_for_game() as integer

#endif
