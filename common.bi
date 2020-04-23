
'OHRRPGCE - Some Custom/Game common code
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#ifndef COMMON_BI
#define COMMON_BI

#include "util.bi"
#include "udts.bi"
#include "browse.bi"
#include "menus.bi"
#include "const.bi"
#include "misc.bi"
#include "reload.bi"
#include "common_base.bi"
#include "common_menus.bi"

USING RELOAD

DECLARE FUNCTION common_setoption(opt as string, arg as string) as integer

DECLARE SUB fadein ()
DECLARE SUB fadeout OVERLOAD (palidx as integer)
DECLARE SUB fadeout OVERLOAD (byval red as integer, byval green as integer, byval blue as integer)

DECLARE SUB ensure_normal_palette ()
DECLARE SUB restore_previous_palette ()
DECLARE SUB push_and_reset_gfxio_state ()
DECLARE SUB pop_gfxio_state ()

DECLARE SUB start_new_debug (title as string)
DECLARE SUB end_debug ()
DECLARE SUB debug_reload (nod as Node ptr)
'Other debug and error reporting functions are declared in common_base.bi

DECLARE FUNCTION filesize (file as string) as string

DECLARE SUB writebinstring OVERLOAD (savestr as string, array() as integer, byval offset as integer, byval maxlen as integer)
DECLARE SUB writebinstring OVERLOAD (savestr as string, array() as short, byval offset as integer, byval maxlen as integer)
DECLARE SUB writebadbinstring (savestr as string, array() as integer, byval offset as integer, byval maxlen as integer, byval skipword as integer=0)
DECLARE FUNCTION readbinstring OVERLOAD (array() as integer, byval offset as integer, byval maxlen as integer) as string
DECLARE FUNCTION readbinstring OVERLOAD (array() as short, byval offset as integer, byval maxlen as integer) as string
DECLARE FUNCTION readbadbinstring (array() as integer, byval offset as integer, byval maxlen as integer, byval skipword as integer=0) as string
DECLARE FUNCTION read32bitstring (strptr as integer ptr) as string
DECLARE FUNCTION readbadgenericname (byval index as integer, filename as string, byval recsize as integer, byval offset as integer, byval size as integer, byval skip as integer = 0, byval expectexists as bool=YES) as string

ENUM RectTransTypes
 transUndef = -1
 transOpaque = 0
 transFuzzy = 1
 transHollow = 2
 transBlend = 3
 transLAST = 3
END ENUM

ENUM RectBorderTypes
 borderUndef = -3
 borderNone = -2
 borderLine = -1
 'N=0-14: usually means use box N style's border
 '(but also used by edgebox_rawborder to mean use raw box border sprite N)
END ENUM

DECLARE FUNCTION lookup_box_border(border as RectBorderTypes) as RectBorderTypes

'----- Styled boxes
DECLARE SUB centerfuz (x as RelPos, y as RelPos, w as RelPos, h as RelPos, boxstyle_plus1 as integer, page as integer)
DECLARE SUB centerbox (x as RelPos=rCenter, y as RelPos=rCenter, w as RelPos, h as RelPos, boxstyle_plus1 as integer, page as integer)
DECLARE SUB center_edgeboxstyle (x as RelPos=rCenter, y as RelPos=rCenter, w as RelPos, h as RelPos, boxstyle as integer, page as integer, fuzzy as bool=NO, suppress_borders as bool=NO)
DECLARE SUB edgeboxstyle OVERLOAD (rect as RectType, boxstyle as integer, page as integer, fuzzy as bool=NO, suppress_borders as bool=NO)
DECLARE SUB edgeboxstyle OVERLOAD (x as RelPos, y as RelPos, w as RelPos, h as RelPos, boxstyle as integer, page as integer, fuzzy as bool=NO, suppress_borders as bool=NO)

'----- Unstyled boxes
DECLARE SUB edgebox (x as RelPos, y as RelPos, w as RelPos, h as RelPos, col as integer, bordercol as integer, page as integer, trans as RectTransTypes=transOpaque, border as RectBorderTypes=borderLine)

DECLARE SUB draw_box_back (fr as Frame ptr, pos as XYPair, size as XYPair, col as integer, trans as RectTransTypes, fuzzfactor as integer=50, fuzz_stationary as bool=NO, fuzz_zoom as integer=1)
DECLARE SUB draw_box_border (fr as Frame ptr, pos as XYPair, size as XYPair, bordercol as integer, borderindex as RectBorderTypes, trans as RectTransTypes = transHollow)

DECLARE FUNCTION decodetrigger (trigger as integer, showerr as bool = YES) as integer
DECLARE FUNCTION trigger_or_default(trigger as integer, default as integer) as integer
DECLARE FUNCTION scriptname (byval num as integer) as string
DECLARE FUNCTION scriptname_default(id_or_trigger as integer, default_trigger as integer) as string

DECLARE Function seconds2str(byval sec as integer, f as string = " %m: %S") as string

DECLARE SUB loaddefaultpals (byval fileset as SpriteType, poffset() as integer)
DECLARE SUB savedefaultpals (byval fileset as SpriteType, poffset() as integer, maxset as integer)
DECLARE SUB guessdefaultpals (byval fileset as SpriteType, poffset() as integer)
DECLARE FUNCTION getdefaultpal(byval fileset as SpriteType, byval index as integer) as integer
DECLARE FUNCTION abs_pal_num(byval num as integer, byval sprtype as SpriteType, byval spr as integer) as integer

DECLARE SUB clear_fixbits_cache ()
DECLARE FUNCTION getfixbit(byval bitnum as integer) as integer
DECLARE SUB setfixbit(byval bitnum as integer, byval bitval as integer)
DECLARE SUB clear_binsize_cache ()
DECLARE SUB setbinsize (byval id as integer, byval size as integer)
DECLARE FUNCTION curbinsize (byval id as integer) as integer
DECLARE FUNCTION defbinsize (byval id as integer) as integer
DECLARE FUNCTION getbinsize (byval id as integer) as integer
DECLARE FUNCTION dimbinsize (byval id as integer) as integer

DECLARE FUNCTION readarchinym (gamedir as string, sourcefile as string) as string
DECLARE FUNCTION maplumpname (byval map as integer, oldext as string) as string

DECLARE FUNCTION shorten_to_left (text as string, byval wide as integer) as string
DECLARE FUNCTION shorten_to_right (text as string, byval wide as integer) as string

DECLARE SUB loadpalette(pal() as RGBcolor, byval palnum as integer)
DECLARE SUB savepalette(pal() as RGBcolor, byval palnum as integer)
DECLARE SUB convertpalette(oldpal() as integer, newpal() as RGBcolor)
DECLARE SUB unconvertpalette()

DECLARE FUNCTION rgb_to_string(col as RGBcolor) as string
DECLARE FUNCTION string_to_rgb(text as string, byref col as RGBcolor) as bool
DECLARE FUNCTION string_to_color(text as string, default as integer = -1) as integer

ENUM MinimapAlgorithmEnum
 minimapScaled   'Proper scaling. Returns a 32 bit Frame; auto-fallback to minimapScaledQuant in 8-bit mode.
 minimapScatter  'Original, noisy minimap algorithm - randomly pick a color
 minimapMajority 'Approximately selects the most common color for each pixel
 minimapScaledQuant 'minimapScaled but quantised to an 8 bit Frame.
 minimapLAST = 3
END ENUM

DECLARE_VECTOR_OF_TYPE(TileMap ptr, TileMap_ptr)
DECLARE_VECTOR_OF_TYPE(TilesetData ptr, TilesetData_ptr)

TYPE MinimapGenerator
 DECLARE CONSTRUCTOR (tiles_arr() as TileMap, tilesets_arr() as TilesetData ptr, pmapptr as TileMap ptr = NULL, zoom_in as integer = -1, algorithm as MinimapAlgorithmEnum = minimapScaled)
 DECLARE DESTRUCTOR()
 DECLARE FUNCTION run(runtime as double) as Frame ptr
 DECLARE FUNCTION finished() as bool

 zoom as integer
 minimap as Frame Ptr

 PRIVATE:
 algorithm as MinimapAlgorithmEnum
 pmapptr as TileMap ptr
 tiles as TileMap ptr vector
 tilesets as TilesetData ptr vector
 prng_state as uinteger
 composed_tile as Frame Ptr
 nextrow as integer
END TYPE

DECLARE FUNCTION minimap_zoom_amount(mapsize as XYPair, margin as XYPair = XY(0,0)) as integer
DECLARE FUNCTION createminimap OVERLOAD (tiles() as TileMap, tilesets() as TilesetData ptr, pmapptr as TileMap ptr = NULL, byref zoom as integer = -1, algorithm as MinimapAlgorithmEnum = minimapScaled) as Frame ptr
DECLARE FUNCTION createminimap OVERLOAD (layer as TileMap, tileset as TilesetData ptr, byref zoom as integer = -1, algorithm as MinimapAlgorithmEnum = minimapScaled) as Frame ptr
DECLARE SUB animatetilesets (tilesets() as TilesetData ptr)
DECLARE SUB cycletile (tanim_state() as TileAnimState, tastuf() as integer)
DECLARE SUB loadtilesetdata (tilesets() as TilesetData ptr, byval layer as integer, byval tilesetnum as integer)
DECLARE SUB reloadtileanimations (tilesets() as TilesetData ptr, gmap() as integer)
DECLARE SUB unloadtilesetdata (byref tileset as TilesetData ptr)
DECLARE FUNCTION layer_tileset_index(byval layer as integer) as integer
DECLARE FUNCTION gmap_index_affects_tiles(byval index as integer) as bool
DECLARE SUB loadmaptilesets (tilesets() as TilesetData ptr, gmap() as integer, resetanimations as bool = YES)
DECLARE SUB unloadmaptilesets (tilesets() as TilesetData ptr)
DECLARE SUB set_map_edge_draw_mode(gmap() as integer, wrap_layers_over_edge_of_crop_maps as bool = NO)

DECLARE FUNCTION finddatafile(filename as string, error_if_missing as bool = YES) as string
DECLARE FUNCTION finddatadir(dirname as string, error_if_missing as bool = YES) as string
DECLARE FUNCTION get_data_dir() as string
DECLARE FUNCTION filename_relative_to_datadir(filename as string) as string

DECLARE SUB updaterecordlength (lumpf as string, byval bindex as integer, byval headersize as integer = 0, byval repeating as bool = NO)

DECLARE SUB writepassword (pass as string)
DECLARE FUNCTION checkpassword (pass as string) as integer
DECLARE FUNCTION getpassword () as string

DECLARE SUB upgrade (show_messages as bool)
DECLARE SUB future_rpg_warning ()
DECLARE SUB rpg_sanity_checks ()
DECLARE SUB fix_sprite_record_count(sprtype as SpriteType)
DECLARE SUB fix_recordless_lump(lumpname as string, record_byte_size as integer, byval header_bytes as integer=0)
DECLARE SUB fix_record_count(byref last_rec_index as integer, record_byte_size as integer, lumpname as string, info as string, skip_header_bytes as integer=0, count_offset as integer=0, errlvl as ErrorLevelEnum=errError)

DECLARE SUB loadglobalstrings ()
DECLARE FUNCTION readglobalstring (byval index as integer, default as zstring ptr, byval maxlen as integer=10) as string

DECLARE FUNCTION price_string (cost as integer) as string
DECLARE FUNCTION money_name () as string

DECLARE SUB load_default_master_palette (master_palette() as RGBColor)
DECLARE SUB dump_integer_array_as_hex (arraydim as string, byval start as uinteger ptr, byval _ubound as integer, byval nibbles as integer = 8)

DECLARE FUNCTION readattackname (byval index as integer) as string
DECLARE FUNCTION readattackcaption (byval index as integer) as string
DECLARE FUNCTION readenemyname (byval index as integer) as string
DECLARE FUNCTION readitemname (byval index as integer) as string
DECLARE FUNCTION readitemdescription (byval index as integer) as string
DECLARE FUNCTION readshopname (byval shopnum as integer) as string
DECLARE FUNCTION getsongname (byval num as integer, byval prefixnum as bool = NO) as string
DECLARE FUNCTION getsfxname (byval num as integer) as string
DECLARE FUNCTION getheroname (hero_id as integer, use_default as bool = YES) as string
DECLARE FUNCTION getmapname (byval m as integer) as string
DECLARE SUB getstatnames(statnames() as string)
DECLARE FUNCTION battle_statnames(statnum as integer) as string
DECLARE SUB getelementnames(elmtnames() as string)

DECLARE FUNCTION getdisplayname (default as string) as string

DECLARE SUB playsongnum (byval songnum as integer)

DECLARE FUNCTION open_document (url as string) as bool
#DEFINE open_url open_document
DECLARE FUNCTION spawn_and_wait (app as string, args as string, expect_exitcode_0 as bool = YES) as string
DECLARE FUNCTION get_support_dir () as string
DECLARE FUNCTION find_helper_app (appname as zstring ptr, try_install as bool=NO, download_url as zstring ptr=@"") as string
DECLARE FUNCTION find_windows_helper_app (appname as string, try_install as bool=NO, download_url as string="") as string
DECLARE FUNCTION install_windows_helper_app (appname as string, download_url as string="") as string
DECLARE FUNCTION download_file (url as string, dest as string, forcefilename as string="") as bool
DECLARE FUNCTION missing_helper_message (appname as string) as string

DECLARE FUNCTION stredit (s as string, byref insert as integer, byval maxl as integer = 9999999, byval numlines as integer = 1, byval line_length as integer = 1) as integer
'strgrabber has separate versions in customsubs.bas and yetmore2.bas
DECLARE FUNCTION strgrabber (s as string, maxl as integer = 9999999) as bool
DECLARE SUB handle_text_copy_paste (byref text as string, byref clip as string)

'When to accept mouse wheel input
ENUM WheelHandlingEnum
  wheelNever
  wheelAlways
  wheelRightButton  'When the right mouse button is down
END ENUM

DECLARE SUB reset_menu_edit_state ()

DECLARE FUNCTION keygrabber (byref n as integer, min as integer, max as integer, less as KBScancode=ccLeft, more as KBScancode=ccRight) as bool
DECLARE FUNCTION intgrabber OVERLOAD (byref n as integer, min as integer, max as integer, less as KBScancode=ccLeft, more as KBScancode=ccRight, returninput as bool=NO, use_clipboard as bool=YES, autoclamp as bool=YES, scrollwheel as WheelHandlingEnum=wheelRightButton) as bool
DECLARE FUNCTION intgrabber OVERLOAD (byref n as longint, min as longint, max as longint, less as KBScancode=ccLeft, more as KBScancode=ccRight, returninput as bool=NO, use_clipboard as bool=YES, autoclamp as bool=YES, scrollwheel as WheelHandlingEnum=wheelRightButton) as bool
DECLARE FUNCTION zintgrabber (byref n as integer, min as integer, max as integer, less as KBScancode=ccLeft, more as KBScancode=ccRight) as bool
DECLARE FUNCTION xintgrabber (byref n as integer, pmin as integer, pmax as integer, nmin as integer=1, nmax as integer=1, less as KBScancode=ccLeft, more as KBScancode=ccRight) as integer

DECLARE SUB console_reset (top as integer = 0, bottom as integer = 0)
DECLARE SUB console_show_message (s as string)
DECLARE SUB console_append_message (s as string)

DECLARE FUNCTION hilite (what as string, col as integer = -1) as string
DECLARE FUNCTION ticklite(what as string, col as integer = -1) as string
DECLARE FUNCTION fgtag(col as integer, text as string = "") as string
DECLARE FUNCTION bgtag(col as integer, text as string = "") as string

DECLARE FUNCTION basic_textbox (msg as zstring ptr, col as integer = -1, page as integer, ypos as RelPos = pCentered, width as RelPos = -1, shrink as bool = NO, suppress_borders as bool = NO) as integer
DECLARE FUNCTION notification (msg as zstring ptr, shrink as bool = NO) as KBScancode
DECLARE SUB pop_warning(msg as zstring ptr, byval autoquit as bool = NO)
DECLARE FUNCTION multichoice(capt as zstring ptr, choices() as string, defaultval as integer=0, escval as integer=-1, helpkey as zstring ptr=@"", centerlines as bool=YES, extra_message as zstring ptr=@"") as integer
DECLARE FUNCTION twochoice(capt as zstring ptr, strA as zstring ptr=@"Yes", strB as zstring ptr=@"No", byval defaultval as integer=0, byval escval as integer=-1, helpkey as zstring ptr=@"") as integer
DECLARE FUNCTION yesno(capt as zstring ptr, byval defaultval as bool=YES, byval escval as bool=NO) as bool
DECLARE FUNCTION confirmed_copy (srcfile as string, destfile as string) as bool
DECLARE FUNCTION confirmed_copydirectory(src as string, dest as string) as bool
DECLARE FUNCTION os_shell_move(src as string, dest as string) as bool

DECLARE SUB create_default_menu(menu as MenuDef, add_sfx_volume as bool = YES)
DECLARE SUB create_volume_menu(menu as MenuDef)

DECLARE FUNCTION bound_arg(n as integer, min as integer, max as integer, argname as zstring ptr, context as zstring ptr = NULL, errlvl as scriptErrEnum = serrBound) as bool
DECLARE SUB reporterr(msg as zstring ptr, errlvl as scriptErrEnum = serrBadOp, context as zstring ptr = NULL)

DECLARE FUNCTION load_tag_name (byval index as integer) as string
DECLARE SUB save_tag_name (tagname as string, byval index as integer)

DECLARE SUB load_special_tag_caches()
DECLARE FUNCTION tag_is_autoset(byval tag_id as integer) as bool
DECLARE FUNCTION describe_tag_autoset_places(byval tag_id as integer) as string
DECLARE FUNCTION onoroff (byval n as integer) as string
DECLARE FUNCTION describe_tag_condition(tag as integer, zerocap as string, maxwidth as integer = 320) as string
DECLARE FUNCTION yesorno (byval n as integer, yes_cap as zstring ptr=@"YES", no_cap as zstring ptr=@"NO") as string

DECLARE FUNCTION format_float (byval float as double, byval sigfigs as integer = 5) as string

DECLARE FUNCTION format_percent OVERLOAD (byref float as double, byval sigfigs as integer = 5) as string
DECLARE FUNCTION format_percent OVERLOAD (byref float as single, byval sigfigs as integer = 5) as string
DECLARE FUNCTION percent_grabber OVERLOAD (byref float as double, byref repr as string = "", min as double, max as double, decimalplaces as integer = 4, ret_if_repr_changed as bool = YES) as bool
DECLARE FUNCTION percent_grabber OVERLOAD (byref float as single, byref repr as string = "", min as double, max as double, decimalplaces as integer = 4, ret_if_repr_changed as bool = YES) as bool

DECLARE FUNCTION bitgrabber (byref bitsets as integer, whichbit as integer, byref state as MenuState) as bool
DECLARE FUNCTION bitsetgrabber (bitwords() as integer, wordnum as integer, bitnum as integer, byref state as MenuState) as bool
DECLARE FUNCTION boolgrabber (byref thebool as bool, byref state as MenuState) as bool

DECLARE FUNCTION menu_click (state as MenuState) as bool
DECLARE FUNCTION menu_click_outside(m as MenuDef) as bool
DECLARE FUNCTION menu_right_click_close(m as MenuDef) as bool
DECLARE FUNCTION click_dismiss () as bool
DECLARE FUNCTION enter_space_click (state as MenuState) as bool
DECLARE FUNCTION enter_or_space () as bool
DECLARE FUNCTION toggle_item (state as MenuState) as bool
DECLARE FUNCTION cropafter_keycombo(index_selected as bool = NO) as bool
DECLARE FUNCTION copy_keychord () as bool
DECLARE FUNCTION paste_keychord () as bool
DECLARE FUNCTION find_next_or_prev_keychord () as integer

DECLARE FUNCTION prefbit(bitnum as integer) as bool
DECLARE SUB setprefbit(bitnum as integer, newval as bool = YES)

DECLARE FUNCTION text_box_last_line(byref box as TextBox) as integer
DECLARE FUNCTION get_text_box_height(byref box as TextBox) as integer
DECLARE FUNCTION last_inv_slot() as integer

DECLARE FUNCTION utf8_to_OHR(utf8string as ustring) as string
DECLARE FUNCTION decode_backslash_codes(s as string, context as string = "", byref show_warnings as bool = NO) as string
DECLARE FUNCTION escape_nonprintable_ascii(s as string) as string
DECLARE FUNCTION remove_nonprintable_ascii(s as string, replacement as string = "") as string
DECLARE FUNCTION sanitize_script_identifier (ident as string, allow_whitespace as bool = YES) as string

DECLARE FUNCTION inputfilename (query as zstring ptr, ext as zstring ptr, byref directory as string, helpkey as zstring ptr, default as zstring ptr=@"", allow_overwrite as bool=YES) as string
DECLARE FUNCTION prompt_for_string (byref retstring as string, caption as string, limit as integer = 40) as bool

DECLARE SUB set_app_dir()
DECLARE FUNCTION get_home_dir() as string
DECLARE FUNCTION get_settings_dir () as string
DECLARE FUNCTION get_documents_dir() as string
DECLARE FUNCTION get_help_dir(helpfile as string="") as string
DECLARE FUNCTION load_help_file(helpkey as string) as string
DECLARE SUB save_help_file(helpkey as string, text as string)

DECLARE SUB show_help(helpkey as zstring ptr)
DECLARE FUNCTION multiline_string_editor(s as string, helpkey as string="", prompt_to_save as bool = YES) as string
DECLARE SUB export_string_to_file(s as string)
DECLARE SUB import_string_from_file(s as string)

DECLARE FUNCTION int_from_xy(pos as XYPair, byval wide as integer, byval high as integer) as integer
DECLARE FUNCTION xy_from_int(byval n as integer, byval wide as integer, byval high as integer) as XYPair

DECLARE FUNCTION color_browser_256(byval start_color as integer=0) as integer

'Sprite loading convenience functions
DECLARE SUB load_sprite_and_pal (byref img as GraphicPair, byval spritetype as SpriteType, byval index as integer, byval palnum as integer=-1)
DECLARE SUB unload_sprite_and_pal (byref img as GraphicPair)

DECLARE FUNCTION exptolevel (byval level as integer, byval curve as double = 0.2) as integer
DECLARE FUNCTION total_exp_to_level (byval level as integer, byval curve as double = 0.2) as integer
DECLARE FUNCTION current_max_level() as integer
DECLARE FUNCTION atlevel (byval lev as integer, byval a0 as integer, byval aMax as integer) as integer
DECLARE FUNCTION atlevel_quadratic (byval lev as double, byval a0 as double, byval aMax as double, byval midpercent as double) as double

DECLARE FUNCTION max_tag() as integer

DECLARE FUNCTION ideal_ticks_per_second() as double
DECLARE FUNCTION wtog_ticks() as integer
DECLARE FUNCTION max_wtog() as integer
DECLARE FUNCTION wtog_to_frame(wtog as integer) as integer

DECLARE SUB cleanup_global_reload_doc ()
DECLARE FUNCTION get_reload_copy (byval n as NodePtr) as NodePtr
DECLARE FUNCTION get_reload_empty (nodename as string = "") as NodePtr

DECLARE SUB upgrade_hero_battle_menu_item(bmenu as NodePtr)
DECLARE FUNCTION add_hero_battle_menu_item(byval parent as NodePtr, kind as string, byval value as integer = 0) as NodePtr
DECLARE FUNCTION should_hide_hero_stat OVERLOAD (hero as HeroDef, byval statnum as integer) as bool
DECLARE FUNCTION should_hide_hero_stat OVERLOAD (byval hero_id as integer, byval statnum as integer) as bool

DECLARE FUNCTION find_on_word_boundary_excluding(haystack as string, needle as string, excludeword as string) as integer
DECLARE FUNCTION find_on_word_boundary(haystack as string, needle as string) as integer

DECLARE FUNCTION str_rect(s as string, byval x as integer, byval y as integer) as RectType

DECLARE FUNCTION gamepad_virtual_keyboard OVERLOAD (default_str as string, max_length as integer=-1, byval multi_player as integer=-1) as string
DECLARE FUNCTION gamepad_virtual_keyboard OVERLOAD (arr as ArrowSet, default_str as string, max_length as integer=-1) as string

DECLARE FUNCTION default_arrowset() as ArrowSet
DECLARE FUNCTION arrowset_for_multiplayer_gamepad(byval player as integer) as ArrowSet
DECLARE FUNCTION arrowset_from_reload(gamepad as NodePtr) as ArrowSet
DECLARE FUNCTION keyval_arrowset_up(arr as ArrowSet) as bool
DECLARE FUNCTION keyval_arrowset_right(arr as ArrowSet) as bool
DECLARE FUNCTION keyval_arrowset_down(arr as ArrowSet) as bool
DECLARE FUNCTION keyval_arrowset_left(arr as ArrowSet) as bool
DECLARE FUNCTION keyval_arrowset_confirm(arr as ArrowSet) as bool
DECLARE FUNCTION keyval_arrowset_cancel(arr as ArrowSet) as bool

DECLARE FUNCTION dissolve_type_caption(n as integer) as string
DECLARE FUNCTION appear_type_caption(n as integer) as string

TYPE FnNoArgsBool as FUNCTION () as bool
DECLARE FUNCTION progress_spinner (byval exit_condition_func as FnNoArgsBool, caption as string, byval timeout_seconds as double) as bool

DECLARE FUNCTION default_gen_bool(nodepath as string) as bool
DECLARE FUNCTION get_gen_bool(nodepath as zstring ptr) as bool
DECLARE SUB set_gen_bool(nodepath as zstring ptr, byval v as bool)
DECLARE SUB toggle_gen_bool(nodepath as zstring ptr)

DECLARE FUNCTION default_gen_int(nodepath as string) as integer
DECLARE FUNCTION gen_int_limits(nodepath as string) as XYPair
DECLARE FUNCTION enforce_gen_int_limits(nodepath as string, byval v as integer) as integer
DECLARE FUNCTION get_gen_int(nodepath as zstring ptr) as integer
DECLARE SUB set_gen_int(nodepath as zstring ptr, byval v as integer)
DECLARE FUNCTION gen_intgrabber(nodepath as string) as bool

DECLARE FUNCTION get_gen_str(nodepath as zstring ptr, default as zstring ptr = @"") as string

DECLARE SUB set_global_config_file()
DECLARE FUNCTION read_ini_prefixed_str (filename as string, prefixed_key as string, default as string="") as string
DECLARE FUNCTION read_config_str (key as zstring ptr, default as zstring ptr=@"") as string
DECLARE FUNCTION read_config_int (key as zstring ptr, default as integer=0) as integer
DECLARE FUNCTION read_config_bool (key as zstring ptr, default as bool=NO) as bool
DECLARE SUB write_config OVERLOAD (key as zstring ptr, value as string)
DECLARE SUB write_config OVERLOAD (key as zstring ptr, value as integer)

'Global variables  (See also common_base.bi)
EXTERN sourcerpg as string
EXTERN game as string
EXTERN game_unique_id as string
EXTERN context_string as string
EXTERN running_as_slave as bool
EXTERN uilook() as integer
EXTERN boxlook() as BoxStyle
EXTERN as integer vpage, dpage
EXTERN buffer() as integer
EXTERN fadestate as bool
EXTERN master() as RGBcolor
EXTERN current_font() as integer
EXTERN gen() as integer
EXTERN gen_reld_doc as DocPtr
EXTERN fmvol as integer
EXTERN sprite_sizes() as SpriteSize
EXTERN statnames() as string
EXTERN documents_dir as string
EXTERN settings_dir as string
EXTERN log_dir as string
EXTERN app_dir as string
EXTERN app_resources_dir as string
EXTERN global_config_file as string
EXTERN config_prefix as string
EXTERN global_reload_doc as DocPtr
EXTERN herotags() as HeroTagsCache
EXTERN itemtags() as ItemDefTags
EXTERN lookup1_bin_cache() as TriggerData
EXTERN script_names() as IntStrPair
EXTERN debug_to_console as bool
EXTERN remember_debug_messages as bool
EXTERN num_logged_errors as integer

#endif
