
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
DECLARE SUB fadeout (byval red as integer, byval green as integer, byval blue as integer)
DECLARE SUB ensure_normal_palette ()
DECLARE SUB restore_previous_palette ()

DECLARE SUB start_new_debug (title as string)
DECLARE SUB end_debug ()

DECLARE FUNCTION filesize (file as string) as string

DECLARE SUB writebinstring OVERLOAD (savestr as string, array() as integer, byval offset as integer, byval maxlen as integer)
DECLARE SUB writebinstring OVERLOAD (savestr as string, array() as short, byval offset as integer, byval maxlen as integer)
DECLARE SUB writebadbinstring (savestr as string, array() as integer, byval offset as integer, byval maxlen as integer, byval skipword as integer=0)
DECLARE FUNCTION readbinstring OVERLOAD (array() as integer, byval offset as integer, byval maxlen as integer) as string
DECLARE FUNCTION readbinstring OVERLOAD (array() as short, byval offset as integer, byval maxlen as integer) as string
DECLARE FUNCTION readbadbinstring (array() as integer, byval offset as integer, byval maxlen as integer, byval skipword as integer=0) as string
DECLARE FUNCTION read32bitstring (strptr as integer ptr) as string
DECLARE FUNCTION readbadgenericname (byval index as integer, filename as string, byval recsize as integer, byval offset as integer, byval size as integer, byval skip as integer = 0) as string

ENUM RectTransTypes
 transUndef = -1
 transOpaque = 0
 transFuzzy
 transHollow
END ENUM

ENUM RectBorderTypes
 borderUndef = -3
 borderNone = -2
 borderLine = -1
 'N=0-14: usually means use box N style's border
 '(but also used by edgebox_rawborder to mean use raw box border sprite N)
END ENUM

'----- Styled boxes
DECLARE SUB centerfuz (x as RelPos, y as RelPos, w as RelPos, h as RelPos, boxstyle_plus1 as integer, page as integer)
DECLARE SUB centerbox (x as RelPos=rCenter, y as RelPos=rCenter, w as RelPos, h as RelPos, boxstyle_plus1 as integer, page as integer)
DECLARE SUB center_edgeboxstyle (x as RelPos=rCenter, y as RelPos=rCenter, w as RelPos, h as RelPos, boxstyle as integer, page as integer, fuzzy as bool=NO, suppress_borders as bool=NO)
DECLARE SUB edgeboxstyle OVERLOAD (rect as RectType, boxstyle as integer, page as integer, fuzzy as bool=NO, suppress_borders as bool=NO)
DECLARE SUB edgeboxstyle OVERLOAD (x as RelPos, y as RelPos, w as RelPos, h as RelPos, boxstyle as integer, page as integer, fuzzy as bool=NO, suppress_borders as bool=NO)

'----- Unstyled boxes
DECLARE SUB edgebox OVERLOAD (x as RelPos, y as RelPos, w as RelPos, h as RelPos, col as integer, bordercol as integer, page as integer, trans as RectTransTypes=transOpaque, border as RectBorderTypes=borderLine, fuzzfactor as integer=50, relcoords as bool=YES)
DECLARE SUB edgebox OVERLOAD (x as RelPos, y as RelPos, w as RelPos, h as RelPos, col as integer, bordercol as integer, fr as Frame Ptr, trans as RectTransTypes=transOpaque, border as RectBorderTypes=borderLine, fuzzfactor as integer=50, relcoords as bool=YES)
DECLARE SUB edgebox_rawborder (x as RelPos, y as RelPos, w as RelPos, h as RelPos, col as integer, bordercol as integer, fr as Frame Ptr, trans as RectTransTypes=transOpaque, borderindex as RectBorderTypes=borderLine, fuzzfactor as integer=50, relcoords as bool=YES)

DECLARE FUNCTION decodetrigger (byval trigger as integer) as integer
DECLARE FUNCTION scriptname (byval num as integer) as string

DECLARE Function seconds2str(byval sec as integer, f as string = " %m: %S") as string

DECLARE SUB loaddefaultpals (byval fileset as SpriteType, poffset() as integer, byval sets as integer)
DECLARE SUB savedefaultpals (byval fileset as SpriteType, poffset() as integer, byval sets as integer)
DECLARE SUB guessdefaultpals (byval fileset as SpriteType, poffset() as integer, byval sets as integer)
DECLARE FUNCTION getdefaultpal(byval fileset as SpriteType, byval index as integer) as integer
DECLARE FUNCTION abs_pal_num(byval num as integer, byval sprtype as SpriteType, byval spr as integer) as integer

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

DECLARE SUB poke8bit (array16() as integer, byval index as integer, byval val8 as integer)
DECLARE FUNCTION peek8bit (array16() as integer, byval index as integer) as integer

DECLARE SUB loadpalette(pal() as RGBcolor, byval palnum as integer)
DECLARE SUB savepalette(pal() as RGBcolor, byval palnum as integer)
DECLARE SUB convertpalette(oldpal() as integer, newpal() as RGBcolor)
DECLARE SUB unconvertpalette()

DECLARE FUNCTION createminimap OVERLOAD (map() as TileMap, tilesets() as TilesetData ptr, pmapptr as TileMap ptr = NULL, byref zoom as integer = -1) as Frame ptr
DECLARE FUNCTION createminimap OVERLOAD (layer as TileMap, tileset as TilesetData ptr, byref zoom as integer = -1) as Frame ptr
DECLARE SUB animatetilesets (tilesets() as TilesetData ptr)
DECLARE SUB cycletile (tanim_state() as TileAnimState, tastuf() as integer)
DECLARE SUB loadtilesetdata (tilesets() as TilesetData ptr, byval layer as integer, byval tilesetnum as integer)
DECLARE SUB reloadtileanimations (tilesets() as TilesetData ptr, gmap() as integer)
DECLARE SUB unloadtilesetdata (byref tileset as TilesetData ptr)
DECLARE FUNCTION layer_tileset_index(byval layer as integer) as integer
DECLARE SUB loadmaptilesets (tilesets() as TilesetData ptr, gmap() as integer, resetanimations as bool = YES)
DECLARE SUB unloadmaptilesets (tilesets() as TilesetData ptr)
DECLARE SUB set_map_edge_draw_mode(gmap() as integer, wrap_layers_over_edge_of_crop_maps as bool = NO)

DECLARE FUNCTION finddatafile(filename as string, error_if_missing as bool = YES) as string
DECLARE FUNCTION finddatadir(dirname as string, error_if_missing as bool = YES) as string
DECLARE FUNCTION get_data_dir() as string
DECLARE FUNCTION filename_relative_to_datadir(filename as string) as string
DECLARE SUB updaterecordlength (lumpf as string, byval bindex as integer, byval headersize as integer = 0, byval repeating as integer = NO)
DECLARE SUB clamp_value (byref value as integer, byval min as integer, byval max as integer, argname as string)

DECLARE SUB writepassword (pass as string)
DECLARE FUNCTION checkpassword (pass as string) as integer
DECLARE FUNCTION getpassword () as string

DECLARE SUB upgrade (show_messages as bool)
DECLARE SUB future_rpg_warning ()
DECLARE SUB rpg_sanity_checks ()
DECLARE SUB fix_sprite_record_count(byval pt_num as integer)
DECLARE SUB fix_recordless_lump(lumpname as string, byref record_byte_size as integer, byval header_bytes as integer=0)
DECLARE SUB fix_record_count(byref last_rec_index as integer, byref record_byte_size as integer, lumpname as string, info as string, byval skip_header_bytes as integer=0, byval count_offset as integer=0)
DECLARE SUB loadglobalstrings ()
DECLARE FUNCTION readglobalstring (byval index as integer, default as string, byval maxlen as integer=10) as string
DECLARE SUB load_default_master_palette (master_palette() as RGBColor)
DECLARE SUB dump_integer_array_as_hex (arraydim as string, byval start as uinteger ptr, byval _ubound as integer, byval nibbles as integer = 8)

DECLARE FUNCTION readattackname (byval index as integer) as string
DECLARE FUNCTION readattackcaption (byval index as integer) as string
DECLARE FUNCTION readenemyname (byval index as integer) as string
DECLARE FUNCTION readitemname (byval index as integer) as string
DECLARE FUNCTION readitemdescription (byval index as integer) as string
DECLARE FUNCTION readshopname (byval shopnum as integer) as string
DECLARE FUNCTION getsongname (byval num as integer, byval prefixnum as integer = 0) as string
DECLARE FUNCTION getsfxname (byval num as integer) as string
DECLARE FUNCTION getheroname (byval hero_id as integer) as string
DECLARE FUNCTION getmapname (byval m as integer) as string
DECLARE SUB getstatnames(statnames() as string)
DECLARE SUB getelementnames(elmtnames() as string)

DECLARE FUNCTION getdisplayname (default as string) as string

DECLARE SUB playsongnum (byval songnum as integer)

DECLARE FUNCTION open_url (url as string) as bool
DECLARE FUNCTION spawn_and_wait (app as string, args as string) as string
DECLARE FUNCTION find_support_dir () as string
DECLARE FUNCTION find_helper_app (appname as string, try_install as integer=NO, download_url as string="") as string
DECLARE FUNCTION find_windows_helper_app (appname as string, try_install as integer=NO, download_url as string="") as string
DECLARE FUNCTION download_file (url as string, dest as string, forcefilename as string="") as integer
DECLARE FUNCTION missing_helper_message (appname as string) as string
DECLARE FUNCTION find_madplay () as string
DECLARE FUNCTION find_oggenc () as string
DECLARE FUNCTION can_convert_mp3 () as integer
DECLARE FUNCTION can_convert_wav () as integer
DECLARE FUNCTION mp3_to_ogg (in_file as string, out_file as string, byval quality as integer = 4) as string
DECLARE FUNCTION mp3_to_wav (in_file as string, out_file as string) as string
DECLARE FUNCTION wav_to_ogg (in_file as string, out_file as string, byval quality as integer = 4) as string

DECLARE FUNCTION stredit (s as string, byref insert as integer, byval maxl as integer, byval numlines as integer = 1, byval line_length as integer = 1) as integer
'strgrabber has separate versions in customsubs.bas and yetmore2.bas
DECLARE FUNCTION strgrabber (s as string, maxl as integer) as bool

DECLARE FUNCTION keygrabber (byref n as integer, min as integer, max as integer, less as integer=scLeft, more as integer=scRight) as bool
DECLARE FUNCTION intgrabber OVERLOAD (byref n as integer, min as integer, max as integer, less as integer=scLeft, more as integer=scRight, returninput as bool=NO, use_clipboard as bool=YES, autoclamp as bool=YES) as bool
DECLARE FUNCTION intgrabber OVERLOAD (byref n as longint, min as longint, max as longint, less as integer=scLeft, more as integer=scRight, returninput as bool=NO, use_clipboard as bool=YES, autoclamp as bool=YES) as bool
DECLARE FUNCTION zintgrabber (byref n as integer, min as integer, max as integer, less as integer=scLeft, more as integer=scRight) as bool
DECLARE FUNCTION xintgrabber (byref n as integer, pmin as integer, pmax as integer, nmin as integer=1, nmax as integer=1, less as integer=scLeft, more as integer=scRight) as integer

DECLARE SUB reset_console (byval top as integer = 0, byval h as integer = 200, byval c as integer = -1)
DECLARE SUB show_message (s as string)
DECLARE SUB append_message (s as string)

DECLARE FUNCTION hilite (what as string, col as integer = -1) as string
DECLARE FUNCTION ticklite(what as string, col as integer = -1) as string
DECLARE FUNCTION fgtag(col as integer, text as string = "") as string
DECLARE FUNCTION bgtag(col as integer, text as string = "") as string

DECLARE SUB basic_textbox (msg as string, col as integer = -1, page as integer, ypos as RelPos = pCentered, width as RelPos = -1, shrink as bool = NO)
DECLARE FUNCTION notification (msg as string) as integer
DECLARE SUB visible_debug (s as string, errlvl as errorLevelEnum = errDebug)
DECLARE SUB pop_warning(s as string, byval autoquit as integer = NO)
DECLARE FUNCTION multichoice(capt as string, choices() as string, defaultval as integer=0, escval as integer=-1, helpkey as string="", centerlines as bool=YES, extra_message as string = "") as integer
DECLARE FUNCTION twochoice(capt as string, strA as string="Yes", strB as string="No", byval defaultval as integer=0, byval escval as integer=-1, helpkey as string="") as integer
DECLARE FUNCTION yesno(capt as string, byval defaultval as integer=YES, byval escval as integer=NO) as integer
DECLARE FUNCTION confirmed_copy (srcfile as string, destfile as string) as integer
DECLARE FUNCTION confirmed_copydirectory(src as string, dest as string) as integer
DECLARE FUNCTION os_shell_move(src as string, dest as string) as integer

DECLARE SUB create_default_menu(menu as MenuDef, add_sfx_volume as bool = YES)
DECLARE SUB create_volume_menu(menu as MenuDef)

DECLARE FUNCTION bound_arg(byval n as integer, byval min as integer, byval max as integer, argname as zstring ptr, context as zstring ptr = NULL, byval fromscript as integer=YES, byval errlvl as scriptErrEnum = serrBound) as integer
DECLARE SUB reporterr(msg as string, byval errlvl as scriptErrEnum = serrBadOp)

DECLARE FUNCTION load_tag_name (byval index as integer) as string
DECLARE SUB save_tag_name (tagname as string, byval index as integer)

DECLARE SUB load_special_tag_caches()
DECLARE FUNCTION tag_is_autoset(byval tag_id as integer) as bool
DECLARE FUNCTION describe_tag_autoset_places(byval tag_id as integer) as string
DECLARE FUNCTION onoroff (byval n as integer) as string
DECLARE FUNCTION describe_tag_condition(tag as integer, zerocap as string, maxwidth as integer = 320) as string
DECLARE FUNCTION yesorno (byval n as integer, yes_cap as string="YES", no_cap as string="NO") as string

DECLARE FUNCTION format_percent (byval float as double, byval sigfigs as integer = 5) as string
DECLARE FUNCTION percent_grabber OVERLOAD (byref float as double, repr as string, byval min as double, byval max as double, byval decimalplaces as integer = 4) as integer
DECLARE FUNCTION percent_grabber OVERLOAD (byref float as single, repr as string, byval min as double, byval max as double, byval decimalplaces as integer = 4) as integer
DECLARE FUNCTION bitgrabber (byref bitsets as integer, whichbit as integer, byref state as MenuState) as bool

DECLARE FUNCTION menu_click (state as MenuState) as bool
DECLARE FUNCTION menu_click_outside(m as MenuDef) as bool
DECLARE FUNCTION click_dismiss () as bool
DECLARE FUNCTION enter_space_click (state as MenuState) as bool
DECLARE FUNCTION enter_or_space () as bool
DECLARE FUNCTION toggle_item (state as MenuState) as bool
DECLARE FUNCTION copy_keychord () as bool
DECLARE FUNCTION paste_keychord () as bool

DECLARE FUNCTION xreadbit (bitarray() as integer, byval bitoffset as integer, byval intoffset as integer=0) as bool

DECLARE FUNCTION get_text_box_height(byref box as TextBox) as integer
DECLARE FUNCTION last_inv_slot() as integer

DECLARE FUNCTION utf8_to_OHR(utf8string as string) as string
DECLARE FUNCTION decode_backslash_codes(s as string, context as string = "", byref show_warnings as bool = NO) as string
DECLARE FUNCTION escape_nonprintable_ascii(s as string) as string
DECLARE FUNCTION remove_nonprintable_ascii(s as string, replacement as string = "") as string
DECLARE FUNCTION sanitize_script_identifier (ident as string, allow_whitespace as bool = YES) as string

DECLARE FUNCTION inputfilename (query as string, ext as string, byref directory as string, helpkey as string, default as string="", allow_overwrite as bool=YES) as string
DECLARE FUNCTION prompt_for_string (byref s as string, caption as string, byval limit as integer=NO) as integer

DECLARE FUNCTION get_home_dir() as string
DECLARE FUNCTION get_settings_dir () as string
DECLARE FUNCTION get_documents_dir() as string
DECLARE FUNCTION get_help_dir(helpfile as string="") as string
DECLARE FUNCTION load_help_file(helpkey as string) as string
DECLARE SUB save_help_file(helpkey as string, text as string)

DECLARE SUB show_help(helpkey as string)
DECLARE FUNCTION multiline_string_editor(s as string, helpkey as string="", prompt_to_save as bool = YES) as string
DECLARE SUB export_string_to_file(s as string)
DECLARE SUB import_string_from_file(s as string)

DECLARE FUNCTION int_from_xy(pos as XYPair, byval wide as integer, byval high as integer) as integer
DECLARE FUNCTION xy_from_int(byval n as integer, byval wide as integer, byval high as integer) as XYPair

DECLARE FUNCTION color_browser_256(byval start_color as integer=0) as integer

'Sprite loading convenience functions
DECLARE SUB load_sprite_and_pal (byref img as GraphicPair, byval spritetype as SpriteType, byval index as integer, byval palnum as integer=-1)
DECLARE SUB unload_sprite_and_pal (byref img as GraphicPair)

DECLARE FUNCTION exptolevel (byval level as integer) as integer
DECLARE FUNCTION total_exp_to_level (byval level as integer) as integer
DECLARE FUNCTION current_max_level() as integer
DECLARE FUNCTION atlevel (byval lev as integer, byval a0 as integer, byval aMax as integer) as integer
DECLARE FUNCTION atlevel_quadratic (byval lev as double, byval a0 as double, byval aMax as double, byval midpercent as double) as double

DECLARE FUNCTION max_tag() as integer

DECLARE FUNCTION ideal_ticks_per_second() as double

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

TYPE FnNoArgsBool as FUNCTION () as bool
DECLARE SUB progress_spinner (exit_condition_func as FnNoArgsBool, caption as string, byval timeout_seconds as double)

DECLARE FUNCTION default_gen_bool(nodepath as string) as bool
DECLARE FUNCTION get_gen_bool(nodepath as string) as bool
DECLARE SUB set_gen_bool(nodepath as string, byval v as bool)
DECLARE SUB toggle_gen_bool(nodepath as string)

DECLARE FUNCTION default_gen_int(nodepath as string) as integer
DECLARE FUNCTION gen_int_limits(nodepath as string) as XYPair
DECLARE FUNCTION enforce_gen_int_limits(nodepath as string, byval v as integer) as integer
DECLARE FUNCTION get_gen_int(nodepath as string) as integer
DECLARE SUB set_gen_int(nodepath as string, byval v as integer)
DECLARE FUNCTION gen_intgrabber(nodepath as string) as bool

DECLARE SUB set_global_config_file()
DECLARE FUNCTION read_config_str (key as string, default as string="") as string
DECLARE FUNCTION read_config_int (key as string, default as integer=0) as integer

'Global variables  (See also common_base.bi)
EXTERN sourcerpg as string
EXTERN as string game, exename
EXTERN game_unique_id as string
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
EXTERN negative_zero as integer
EXTERN global_reload_doc as DocPtr
EXTERN herotags() as HeroTagsCache
EXTERN itemtags() as ItemTagsCache
EXTERN lookup1_bin_cache() as TriggerData
EXTERN script_names() as IntStrPair
EXTERN debug_to_console as bool
EXTERN remember_debug_messages as bool

#endif
