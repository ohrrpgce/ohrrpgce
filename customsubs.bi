'OHRRPGCE - Some Custom common code
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#ifndef CUSTOMSUBS_BI
#define CUSTOMSUBS_BI

#include "const.bi"
#include "slices.bi"
#include "custom_udts.bi"

TYPE FnScriptVisitor as function (byref trig as integer, description as string, caption as string) as bool

DECLARE FUNCTION tag_grabber (byref n as integer, state as MenuState, allowspecial as bool=YES, always_choice as bool=NO, allowneg as bool=YES) as bool
DECLARE FUNCTION tag_id_grabber (byref n as integer, state as MenuState) as bool
DECLARE FUNCTION tag_set_grabber (byref n as integer, state as MenuState) as bool
DECLARE FUNCTION tags_menu (byval starttag as integer=0, byval picktag as bool=NO, byval allowspecial as bool=YES, showsign as bool=NO, byval always_choice as bool=NO) as integer
DECLARE FUNCTION tag_toggle_caption(byval n as integer, prefix as string="Toggle tag", byval allowspecial as bool=NO) as string
DECLARE FUNCTION tag_set_caption(byval n as integer, prefix as string="Set Tag", byval allowspecial as bool=NO) as string
DECLARE FUNCTION tag_choice_caption(byval n as integer, prefix as string="", byval allowspecial as bool=NO) as string
DECLARE FUNCTION tag_condition_caption(byval n as integer, prefix as string="Tag", zerocap as string, onecap as string="Never", negonecap as string="Always") as string
DECLARE FUNCTION describe_two_tag_condition(prefix as string, truetext as string, falsetext as string, byval zerovalue as bool, byval tag1 as integer, byval tag2 as integer) as string

DECLARE FUNCTION cond_grabber (cond as Condition, default as bool = NO, alwaysedit as bool, st as MenuState) as bool
DECLARE FUNCTION condition_string (cond as Condition, byval selected as integer, default as string = "Always", byval wide as integer = 40) as string

DECLARE FUNCTION charpicker() as string

DECLARE FUNCTION format_percent_cond(cond as AttackElementCondition, default as string, byval decimalplaces as integer = 4) as string
DECLARE FUNCTION percent_cond_grabber(byref cond as AttackElementCondition, byref repr as string, default as string, byval min as double, byval max as double, byval decimalplaces as integer = 4, ret_if_repr_changed as bool = YES) as bool
DECLARE SUB percent_cond_editor (cond as AttackElementCondition, byval min as double, byval max as double, byval decimalplaces as integer = 4, do_what as string = "...", percent_of_what as string = "")

CONST THINGGRABBER_TOOLTIP = "Ctrl-Enter/Click to edit, +/Insert to add new"

DECLARE FUNCTION enter_or_add_new(state as MenuState) as bool
DECLARE FUNCTION attackgrabber (byref datum as integer, state as MenuState, offset as integer = 0, min as integer = 0, intgrab as bool = YES) as bool
DECLARE FUNCTION enemygrabber (byref datum as integer, state as MenuState, offset as integer = 0, min as integer = 0, intgrab as bool = YES) as bool
DECLARE FUNCTION textboxgrabber (byref datum as integer, state as MenuState, offset as integer = 0, min as integer = 0, intgrab as bool = YES) as integer

DECLARE SUB ui_color_editor(palnum as integer)
DECLARE SUB make_ui_color_editor_menu(m() as string, colors() as integer)
DECLARE SUB ui_boxstyle_editor(palnum as integer)
DECLARE SUB make_ui_boxstyle_editor_menu(m() as string, boxes() as BoxStyle)

TYPE FnRecordName as FUNCTION(idx as integer) as string

DECLARE FUNCTION generic_add_new (what as string, maxindex as integer, getname as FnRecordName = NULL, previewer as RecordPreviewer ptr = NULL, helpkey as string = "") as integer
DECLARE FUNCTION needaddset (byref pt as integer, byref check as integer, what as string) as bool
DECLARE FUNCTION intgrabber_with_addset (byref pt as integer, byval min as integer, byval max as integer, byval maxmax as integer=32767, what as string, byval less as KBScancode=ccLeft, byval more as KBScancode=ccRight) as bool

DECLARE FUNCTION load_vehicle_name(vehID as integer) as string
DECLARE FUNCTION load_item_name (it as integer, hidden as integer, offbyone as integer) as string
DECLARE SUB onetimetog(byref tagnum as integer)

DECLARE FUNCTION pal16browse OVERLOAD (curpal as integer, sprite as Frame ptr, show_default as bool=NO) as integer
DECLARE FUNCTION pal16browse OVERLOAD (curpal as integer, picset as SpriteType, picnum as integer, show_default as bool=NO) as integer

DECLARE FUNCTION formset_step_estimate(freq as integer, suffix as string=" steps") as string
DECLARE FUNCTION formset_freq_estimate(freq as integer) as double
DECLARE FUNCTION speed_estimate(speed as integer) as string
DECLARE FUNCTION seconds_estimate(ticks as integer) as string
DECLARE FUNCTION stat_value_caption(statnum as integer, statval as integer) as string

DECLARE SUB load_text_box_portrait (byref box as TextBox, byref gfx as GraphicPair)
DECLARE SUB draw_crosshairs(pos as XYPair, length as integer = 1, zoom as integer = 1, col as integer, page as integer)
DECLARE SUB xy_position_on_slice (sl as Slice Ptr, byref x as integer, byref y as integer, caption as string, helpkey as string)
DECLARE SUB xy_position_on_sprite_slice (sl as Slice Ptr, byref x as integer, byref y as integer, caption as string, helpkey as string)
DECLARE SUB xy_position_on_sprite (spr as GraphicPair, byref x as integer, byref y as integer, byval frame as integer, caption as string, helpkey as string)
DECLARE FUNCTION sublist (s() as string, helpkey as string="", byval x as integer=0, byval y as integer=0, byval page as integer=-1) as integer
DECLARE SUB get_menu_hotkeys (menu() as string, byval menumax as integer, menukeys() as string, excludewords as string = "")
DECLARE FUNCTION experience_chart (byval expcurve as double=0.2) as double
DECLARE SUB stat_growth_chart ()
DECLARE SUB spawn_game_menu(gdb as bool = NO, valgrind as bool = NO)

DECLARE FUNCTION write_rpg_or_rpgdir (lumpsdir as string, filetolump as string) as bool
DECLARE SUB move_unwriteable_rpg (filetolump as string)
DECLARE SUB save_edit_time ()
DECLARE FUNCTION save_current_game(byval genDebugMode_override as integer=-1) as bool
DECLARE FUNCTION automatic_backup (rpgfile as string) as bool

DECLARE SUB check_used_onetime_npcs(bits() as integer)

DECLARE SUB menu_of_reorderable_nodes(st as MenuState, menu as MenuDef)
DECLARE FUNCTION reorderable_node(byval node as NodePtr) as integer

DECLARE FUNCTION edit_purchase_enumgrabber(byval node as NodePtr, enumstr() as string) as bool
DECLARE FUNCTION edit_purchase_enumbrowse(caption as string, byval node as NodePtr, enumstr() as string, helpkey as string) as bool

DECLARE FUNCTION npc_preview_text(byref npc as NPCType) as string

DECLARE FUNCTION custom_setoption(opt as string, arg as string) as integer

#endif
