'OHRRPGCE - Some Custom common code
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#ifndef CUSTOMSUBS_BI
#define CUSTOMSUBS_BI

#include "const.bi"
#include "slices.bi"
#include "custom_udts.bi"

TYPE FnScriptVisitor as function (byref trig as integer, description as string, caption as string) as integer

DECLARE FUNCTION tag_grabber (byref n as integer, byval min as integer=-99999, byval max as integer=99999, byval allowspecial as integer=YES) as integer
DECLARE FUNCTION tags_menu (byval starttag as integer=0, byval picktag as integer=NO, byval allowspecial as integer=YES) as integer
DECLARE FUNCTION tag_toggle_caption(byval n as integer, prefix as string="Toggle tag", byval allowspecial as integer=NO) as string
DECLARE FUNCTION tag_set_caption(byval n as integer, prefix as string="Set Tag", byval allowspecial as integer=NO) as string
DECLARE FUNCTION tag_choice_caption(byval n as integer, prefix as string="", byval allowspecial as integer=NO) as string
DECLARE FUNCTION tag_condition_caption(byval n as integer, prefix as string="Tag", zerocap as string, onecap as string="Never", negonecap as string="Always") as string
DECLARE FUNCTION explain_two_tag_condition(prefix as string, truetext as string, falsetext as string, byval zerovalue as integer, byval tag1 as integer, byval tag2 as integer) as string

DECLARE FUNCTION cond_grabber (cond as Condition, byval default as integer = 0, byval alwaysedit as integer) as integer
DECLARE FUNCTION condition_string (cond as Condition, byval selected as integer, default as string = "Always", byval wide as integer = 40) as string

DECLARE FUNCTION charpicker() as string

DECLARE FUNCTION format_percent_cond(byref cond as AttackElementCondition, default as string, byval decimalplaces as integer = 4) as string
DECLARE FUNCTION percent_cond_grabber(byref cond as AttackElementCondition, repr as string, default as string, byval min as double, byval max as double, byval decimalplaces as integer = 4) as integer
DECLARE SUB percent_cond_editor (cond as AttackElementCondition, byval min as double, byval max as double, byval decimalplaces as integer = 4, do_what as string = "...", percent_of_what as string = "")

DECLARE SUB ui_color_editor(palnum as integer)
DECLARE SUB make_ui_color_editor_menu(m() as string, colors() as integer)
DECLARE SUB ui_boxstyle_editor(palnum as integer)
DECLARE SUB make_ui_boxstyle_editor_menu(m() as string, boxes() as BoxStyle)

DECLARE FUNCTION pick_ogg_quality(byref quality as integer) as integer
DECLARE FUNCTION needaddset (byref pt as integer, byref check as integer, what as string) as integer
DECLARE FUNCTION intgrabber_with_addset (byref pt as integer, byval min as integer, byval max as integer, byval maxmax as integer=32767, what as string, byval less as integer=scLeft, byval more as integer=scRight) as integer
DECLARE SUB keyboardsetup ()
DECLARE FUNCTION load_vehicle_name(vehID as integer) as string
DECLARE FUNCTION load_item_name (it as integer, hidden as integer, offbyone as integer) as string
DECLARE FUNCTION textbox_preview_line OVERLOAD (boxnum as integer) as string
DECLARE FUNCTION textbox_preview_line OVERLOAD (box as TextBox) as string
DECLARE SUB onetimetog(byref tagnum as integer)
DECLARE SUB edit_npc (npcdata as NPCType, gmap() as integer, zmap as ZoneMap)
DECLARE FUNCTION pal16browse (byval curpal as integer, byval picset as integer, byval picnum as integer) as integer
DECLARE FUNCTION step_estimate(freq as integer, low as integer, high as integer, infix as string="-", suffix as string= "", zero as string="never") as string
DECLARE FUNCTION speed_estimate(speed as integer, suffix as string=" seconds", zero as string="infinity") as string
DECLARE FUNCTION seconds_estimate(ticks as integer) as string
DECLARE SUB load_text_box_portrait (byref box as TextBox, byref gfx as GraphicPair)
DECLARE FUNCTION export_textboxes (filename as string, metadata() as integer) as integer
DECLARE FUNCTION import_textboxes (filename as string, byref warn as string) as integer
DECLARE FUNCTION askwhatmetadata (metadata() as integer, metadatalabels() as string) as integer
DECLARE FUNCTION str2bool(q as string, default as integer = NO, invert as integer = NO) as integer
DECLARE SUB xy_position_on_slice (sl as Slice Ptr, byref x as integer, byref y as integer, caption as string, helpkey as string)
DECLARE SUB xy_position_on_sprite (spr as GraphicPair, byref x as integer, byref y as integer, byval frame as integer, byval wide as integer, byval high as integer, caption as string, helpkey as string)
DECLARE SUB edit_menu_bits (menu as MenuDef)
DECLARE SUB edit_menu_item_bits (mi as MenuDefItem)
DECLARE SUB reposition_menu (menu as MenuDef, mstate as MenuState)
DECLARE SUB reposition_anchor (menu as MenuDef, mstate as MenuState)
DECLARE FUNCTION editbitset (array() as integer, byval wof as integer, byval last as integer, names() as string, helpkey as string="editbitset", byref remem_pt as integer = -2, byval immediate_quit as integer = NO) as integer
DECLARE FUNCTION scriptbrowse_string (byref trigger as integer, byval triggertype as integer, scrtype as string) as string
DECLARE SUB scriptbrowse (byref trigger as integer, byval triggertype as integer, scrtype as string)
DECLARE FUNCTION scrintgrabber (byref n as integer, byval min as integer, byval max as integer, byval less as integer=75, byval more as integer=77, byval scriptside as integer, byval triggertype as integer) as integer
DECLARE SUB visit_scripts(byval visit as FnScriptVisitor)
DECLARE SUB gather_script_usage(list() as string, byval id as integer, byval trigger as integer=0, byref meter as integer, byval meter_times as integer=1, box_instead_cache() as integer, box_after_cache() as integer, box_preview_cache() as string)
DECLARE SUB script_usage_list ()
DECLARE SUB script_broken_trigger_list()
DECLARE SUB autofix_broken_old_scripts()
DECLARE FUNCTION sublist (s() as string, helpkey as string="", byval x as integer=0, byval y as integer=0, byval page as integer=-1) as integer
DECLARE SUB edit_global_text_strings()
DECLARE SUB writeglobalstring (index as integer, s as string, maxlen as integer)
DECLARE FUNCTION safe_caption(caption_array() as string, byval index as integer, description as string) as string
DECLARE SUB update_attack_editor_for_chain (byval mode as integer, byref caption1 as string, byref max1 as integer, byref min1 as integer, byref menutype1 as integer, byref caption2 as string, byref max2 as integer, byref min2 as integer, byref menutype2 as integer)
DECLARE FUNCTION attack_chain_browser (byval start_attack as integer) as integer
DECLARE FUNCTION create_attack_preview_slice(caption as string, byval attack_id as integer, byval parent as Slice Ptr) as Slice Ptr
DECLARE SUB init_attack_chain_screen(byval attack_id as integer, state as AttackChainBrowserState)
DECLARE SUB attack_preview_slice_focus(byval sl as Slice Ptr)
DECLARE SUB attack_preview_slice_defocus(byval sl as Slice Ptr)
DECLARE FUNCTION find_free_attack_preview_slot(slots() as Slice Ptr) as integer
DECLARE SUB position_chain_preview_boxes(sl_list() as Slice ptr, st as MenuState)
DECLARE SUB fontedit (font() as integer)
DECLARE SUB fontedit_export_font(font() as integer)
DECLARE SUB fontedit_import_font(font() as integer)
DECLARE SUB cropafter (byval index as integer, byref limit as integer, byval flushafter as integer, lump as string, byval bytes as integer, byval prompt as integer=YES)
DECLARE FUNCTION numbertail (s as string) as string
DECLARE SUB get_menu_hotkeys (menu() as string, byval menumax as integer, menukeys() as string, excludewords as string = "")
DECLARE SUB experience_chart ()
DECLARE SUB stat_growth_chart ()
DECLARE SUB spawn_game_menu ()

DECLARE FUNCTION write_rpg_or_rpgdir (lumpsdir as string, filetolump as string) as bool
DECLARE SUB move_unwriteable_rpg (filetolump as string)
DECLARE FUNCTION save_current_game(byval genDebugMode_override as integer=-1) as bool
DECLARE SUB automatic_backup (rpgfile as string)

DECLARE SUB check_used_onetime_npcs(bits() as integer)

DECLARE SUB menu_of_reorderable_nodes(st as MenuState, menu as MenuDef)
DECLARE FUNCTION reorderable_node(byval node as NodePtr) as integer

DECLARE SUB edit_platform_options ()
DECLARE FUNCTION prompt_for_scancode () as integer
DECLARE FUNCTION scancode_to_name(byval sc as integer) as string
DECLARE SUB edit_purchase_options ()
DECLARE SUB edit_purchase_details (byval prod as NodePtr)

DECLARE SUB edit_savegame_options ()

DECLARE SUB resolution_menu (secret_options as bool)

#endif
