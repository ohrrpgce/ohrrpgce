
'OHRRPGCE - Some Custom/Game common code
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#ifndef COMMON_BI
#define COMMON_BI

#include "util.bi"
#include "udts.bi"
#include "music.bi"
#include "browse.bi"
#include "const.bi"
#include "misc.bi"  'for nulzstr

#ifdef COMMON_BASE_BI
#error Include at most one of common.bi, common_base.bi
#endif

DECLARE FUNCTION common_setoption(opt as string, arg as string) as integer

DECLARE SUB fadein ()
DECLARE SUB fadeout (red as integer, green as integer, blue as integer)
DECLARE FUNCTION usemenu OVERLOAD (pt as integer, top as integer, first as integer, last as integer, size as integer, deckey as integer = scUp, inckey as integer = scDown) as integer
DECLARE FUNCTION usemenu OVERLOAD (state as MenuState, deckey as integer = scUp, inckey as integer = scDown) as integer
DECLARE FUNCTION usemenu OVERLOAD (state as MenuState, enabled() as integer, BYVAL deckey as integer = scUp, BYVAL inckey as integer = scDown) as integer
DECLARE FUNCTION usemenu OVERLOAD (state as MenuState, menudata() as SimpleMenu, BYVAL deckey as integer = scUp, BYVAL inckey as integer = scDown) as integer
DECLARE FUNCTION scrollmenu (state AS MenuState, BYVAL deckey as integer = scUp, BYVAL inckey as integer = scDown) as integer
DECLARE SUB standardmenu OVERLOAD (menu() as string, state as MenuState, x as integer, y as integer, page as integer, edge as integer=NO, hidecursor as integer=NO, wide AS INTEGER=999, highlight AS INTEGER=NO, toggle AS INTEGER=YES)
DECLARE SUB standardmenu OVERLOAD (menu() AS STRING, state AS MenuState, shaded() AS INTEGER, x AS INTEGER, y AS INTEGER, page AS INTEGER, edge AS INTEGER=NO, hidecursor AS INTEGER=NO, wide AS INTEGER=999, highlight AS INTEGER=NO, toggle AS INTEGER=YES)
DECLARE SUB standardmenu OVERLOAD (menu() as string, size as integer, vis as integer, pt as integer, top as integer, x as integer, y as integer, page as integer, edge as integer=NO, wide AS INTEGER=999, highlight AS INTEGER=NO, shaded AS INTEGER PTR=NULL, toggle AS INTEGER=YES)
DECLARE SUB clamp_menu_state (BYREF state AS MenuState)
DECLARE SUB start_new_debug ()
DECLARE SUB end_debug ()
DECLARE SUB debug (s as string)
DECLARE SUB debugc CDECL ALIAS "debugc" (BYVAL s as zstring ptr, BYVAL errorlevel as integer)
DECLARE SUB debuginfo (s as string)
DECLARE FUNCTION soundfile (sfxnum as integer) as string
DECLARE FUNCTION filesize (file as string) as string

DECLARE FUNCTION acquiretempdir () as string

DECLARE SUB writebinstring OVERLOAD (savestr as string, array() as integer, offset as integer, maxlen as integer)
DECLARE SUB writebinstring OVERLOAD (savestr as string, array() as short, offset as integer, maxlen as integer)
DECLARE SUB writebadbinstring (savestr as string, array() as integer, offset as integer, maxlen as integer, skipword as integer=0)
DECLARE FUNCTION readbinstring OVERLOAD (array() as integer, offset as integer, maxlen as integer) as string
DECLARE FUNCTION readbinstring OVERLOAD (array() as short, offset as integer, maxlen as integer) as string
DECLARE FUNCTION readbadbinstring (array() as integer, offset as integer, maxlen as integer, skipword as integer=0) as string
DECLARE FUNCTION read32bitstring overload (array() as integer, offset as integer) as string
DECLARE FUNCTION read32bitstring overload (strptr as integer ptr) as string
DECLARE FUNCTION readbadgenericname (index as integer, filename as string, recsize as integer, offset as integer, size as integer, skip as integer = 0) as string

ENUM RectTransTypes
 transUndef = -1
 transOpaque = 0
 transFuzzy
 transHollow
END ENUM

DECLARE SUB centerfuz (x as integer, y as integer, w as integer, h as integer, c as integer, p as integer)
DECLARE SUB centerbox (x as integer, y as integer, w as integer, h as integer, c as integer, p as integer)
DECLARE SUB edgeboxstyle OVERLOAD (rect as RectType, BYVAL boxstyle, BYVAL p, BYVAL fuzzy=NO, BYVAL supress_borders=NO)
DECLARE SUB edgeboxstyle OVERLOAD (BYVAL x as integer, BYVAL y as integer, BYVAL w as integer, BYVAL h as integer, BYVAL boxstyle as integer, BYVAL p as integer, BYVAL fuzzy as integer=NO, BYVAL supress_borders as integer=NO)
DECLARE SUB center_edgeboxstyle (x as integer, y as integer, w as integer, h as integer, boxstyle as integer, p as integer, fuzzy as integer=NO, supress_borders as integer=NO)
DECLARE SUB edgebox OVERLOAD (x as integer, y as integer, w as integer, h as integer, col as integer, bordercol as integer, p as integer, trans as RectTransTypes=transOpaque, border as integer=-1, fuzzfactor as integer=50)
DECLARE SUB edgebox OVERLOAD (x, y, w, h, col, bordercol, BYVAL fr AS Frame Ptr, trans AS RectTransTypes=transOpaque, border=-1, fuzzfactor as integer=50)
DECLARE FUNCTION isbit (bb() as INTEGER, BYVAL w as INTEGER, BYVAL b as INTEGER) as INTEGER
DECLARE FUNCTION scriptname (num as integer, trigger as integer = 0) as string
DECLARE Function seconds2str(byval sec as integer, f as string = " %m: %S") as string

DECLARE SUB loaddefaultpals (fileset AS INTEGER, poffset() AS INTEGER, sets AS INTEGER)
DECLARE SUB savedefaultpals (fileset AS INTEGER, poffset() AS INTEGER, sets AS INTEGER)
DECLARE SUB guessdefaultpals (fileset AS INTEGER, poffset() AS INTEGER, sets AS INTEGER)
DECLARE FUNCTION getdefaultpal(fileset as integer, index as integer) as integer
DECLARE FUNCTION abs_pal_num(byval num as integer, byval sprtype as integer, byval spr as integer) as integer

DECLARE FUNCTION getfixbit(BYVAL bitnum AS INTEGER) AS INTEGER
DECLARE SUB setfixbit(BYVAL bitnum AS INTEGER, BYVAL bitval AS INTEGER)
DECLARE SUB setbinsize (id as integer, size as integer)
DECLARE FUNCTION curbinsize (id as integer) as integer
DECLARE FUNCTION defbinsize (id as integer) as integer
DECLARE FUNCTION getbinsize (id as integer) as integer
DECLARE FUNCTION dimbinsize (id as integer) as integer
DECLARE FUNCTION readarchinym (gamedir as string, sourcefile as string) as string
DECLARE FUNCTION maplumpname (map as integer, oldext as string) as string

DECLARE FUNCTION xstring (s as string, x as integer) as integer
DECLARE FUNCTION defaultint (n AS INTEGER, default_caption AS STRING="default") AS STRING
DECLARE FUNCTION caption_or_int (n AS INTEGER, captions() AS STRING) AS STRING
DECLARE SUB poke8bit (array16() as integer, index as integer, val8 as integer)
DECLARE FUNCTION peek8bit (array16() as integer, index as integer) as integer

DECLARE SUB loadpalette(pal() as RGBcolor, palnum as integer)
DECLARE SUB savepalette(pal() as RGBcolor, palnum as integer)
DECLARE SUB convertpalette(oldpal() as integer, newpal() as RGBcolor)

DECLARE FUNCTION createminimap OVERLOAD (map() AS TileMap, tilesets() AS TilesetData ptr, BYREF zoom AS INTEGER = -1) AS Frame PTR
DECLARE FUNCTION createminimap OVERLOAD (layer AS TileMap, tileset AS TilesetData ptr, BYREF zoom AS INTEGER = -1) AS Frame PTR
DECLARE SUB animatetilesets (tilesets() AS TilesetData ptr)
DECLARE SUB cycletile (tanim_state() AS TileAnimState, tastuf() AS INTEGER)
DECLARE SUB loadtilesetdata (tilesets() AS TilesetData ptr, BYVAL layer AS INTEGER, BYVAL tilesetnum AS INTEGER, BYVAL lockstep AS INTEGER = YES)
DECLARE SUB unloadtilesetdata (BYREF tileset AS TilesetData ptr)
DECLARE FUNCTION layer_tileset_index(BYVAL layer AS INTEGER) AS INTEGER
DECLARE SUB loadmaptilesets (tilesets() AS TilesetData ptr, gmap() AS INTEGER, BYVAL resetanimations as integer = YES)
DECLARE SUB unloadmaptilesets (tilesets() AS TilesetData ptr)
DECLARE FUNCTION finddatafile(filename as string) as string
DECLARE FUNCTION finddatadir(dirname AS STRING) AS STRING
DECLARE SUB updaterecordlength (lumpf as string, byval bindex as integer, byval headersize as integer = 0, byval repeating as integer = NO)
DECLARE SUB clamp_value (BYREF value as integer, BYVAL min as integer, BYVAL max as integer, argname as string)

DECLARE SUB writepassword (pass as string)
DECLARE FUNCTION checkpassword (pass as string) as integer
DECLARE FUNCTION getpassword () as string

DECLARE SUB upgrade (font() as integer)
DECLARE SUB future_rpg_warning ()
DECLARE SUB rpg_sanity_checks ()
DECLARE SUB fix_sprite_record_count(BYVAL pt_num AS INTEGER)
DECLARE SUB fix_record_count(BYREF last_rec_index AS INTEGER, BYREF record_byte_size AS INTEGER, lumpname AS STRING, info AS STRING, skip_header_bytes AS INTEGER=0, count_offset AS INTEGER=0)
DECLARE SUB loadglobalstrings ()
DECLARE FUNCTION readglobalstring (index as integer, default as string, maxlen as integer=10) as string
DECLARE SUB load_default_master_palette (master_palette() AS RGBColor)
DECLARE SUB dump_master_palette_as_hex (master_palette() AS RGBColor)

DECLARE FUNCTION readattackname (index as integer) as string
DECLARE FUNCTION readenemyname (index as integer) as string
DECLARE FUNCTION readitemname (index as integer) as string
DECLARE FUNCTION readitemdescription (byval index as integer) as string
DECLARE FUNCTION readshopname (shopnum as integer) as string
DECLARE FUNCTION getsongname (num AS INTEGER, prefixnum AS INTEGER = 0) as string
DECLARE FUNCTION getsfxname (num AS INTEGER) as string
DECLARE FUNCTION getheroname (hero_id AS INTEGER) AS STRING
DECLARE FUNCTION getmenuname(record AS INTEGER) AS STRING
DECLARE FUNCTION getmapname (m as integer) as string
DECLARE SUB getstatnames(statnames() AS STRING)
DECLARE SUB getelementnames(elmtnames() AS STRING)

DECLARE FUNCTION getdisplayname (default as string) as string

DECLARE SUB playsongnum (songnum as integer)

DECLARE FUNCTION spawn_and_wait (app AS STRING, args AS STRING) as string
DECLARE FUNCTION find_helper_app (appname AS STRING) AS STRING
DECLARE FUNCTION missing_helper_message (appname AS STRING) AS STRING
DECLARE FUNCTION find_madplay () AS STRING
DECLARE FUNCTION find_oggenc () AS STRING
DECLARE FUNCTION can_convert_mp3 () AS INTEGER
DECLARE FUNCTION can_convert_wav () AS INTEGER
DECLARE FUNCTION mp3_to_ogg (in_file AS STRING, out_file AS STRING, quality AS INTEGER = 5) AS STRING
DECLARE FUNCTION mp3_to_wav (in_file AS STRING, out_file AS STRING) AS STRING
DECLARE FUNCTION wav_to_ogg (in_file AS STRING, out_file AS STRING, quality AS INTEGER = 5) AS STRING

DECLARE FUNCTION intgrabber OVERLOAD (BYREF n AS INTEGER, BYVAL min AS INTEGER, BYVAL max AS INTEGER, BYVAL less AS INTEGER=scLeft, BYVAL more AS INTEGER=scRight, BYVAL returninput AS INTEGER=NO, BYVAL use_clipboard AS INTEGER=YES, BYVAL autoclamp AS INTEGER=YES) AS INTEGER
DECLARE FUNCTION intgrabber OVERLOAD (BYREF n AS LONGINT, BYVAL min AS LONGINT, BYVAL max AS LONGINT, BYVAL less AS INTEGER=scLeft, BYVAL more AS INTEGER=scRight, BYVAL returninput AS INTEGER=NO, BYVAL use_clipboard AS INTEGER=YES, BYVAL autoclamp AS INTEGER=YES) AS INTEGER
DECLARE FUNCTION zintgrabber (n AS INTEGER, min AS INTEGER, max AS INTEGER, less AS INTEGER=75, more AS INTEGER=77) AS INTEGER
DECLARE FUNCTION xintgrabber (n AS INTEGER, pmin AS INTEGER, pmax AS INTEGER, nmin AS INTEGER=1, nmax AS INTEGER=1, less AS INTEGER=scLeft, more AS INTEGER=scRight) AS INTEGER

DECLARE SUB reset_console (top AS INTEGER = 0, h AS INTEGER = 200, c AS INTEGER = 0)
DECLARE SUB show_message (s AS STRING)
DECLARE SUB append_message (s AS STRING)

DECLARE SUB basic_textbox (msg as string, BYVAL col as integer, BYVAL page as integer)
DECLARE SUB notification (msg AS STRING)
DECLARE SUB visible_debug (s as string)
DECLARE SUB showerror (msg as string, byval isfatal as integer = NO)
DECLARE SUB fatalerror (msg as string)
DECLARE SUB pop_warning(s AS STRING)
DECLARE FUNCTION multichoice(capt AS STRING, choices() AS STRING, defaultval AS INTEGER=0, escval AS INTEGER=-1, helpkey AS STRING="") AS INTEGER
DECLARE FUNCTION twochoice(capt AS STRING, strA AS STRING="Yes", strB AS STRING="No", defaultval AS INTEGER=0, escval AS INTEGER=-1, helpkey AS STRING="") AS INTEGER
DECLARE FUNCTION yesno(capt AS STRING, BYVAL defaultval AS INTEGER=YES, escval AS INTEGER=NO) AS INTEGER

DECLARE SUB position_menu (menu AS MenuDef, page AS INTEGER)
DECLARE SUB draw_menu (menu AS MenuDef, state AS MenuState, page AS INTEGER)
DECLARE SUB init_menu_state OVERLOAD (BYREF state AS MenuState, menu AS MenuDef)
DECLARE SUB init_menu_state OVERLOAD (BYREF state AS MenuState, menu() AS SimpleMenu, BYVAL pickenabled AS INTEGER = YES)
DECLARE FUNCTION count_menu_items (menu AS MenuDef) as integer
DECLARE FUNCTION find_empty_menu_item (menu AS MenuDef) as integer
DECLARE FUNCTION get_menu_item_caption (mi AS MenuDefItem, menu AS MenuDef) AS STRING
DECLARE FUNCTION get_special_menu_caption(subtype AS INTEGER, edit_mode AS INTEGER= NO) AS STRING
DECLARE SUB create_default_menu(menu AS MenuDef)
DECLARE FUNCTION anchor_point(anchor AS INTEGER, size AS INTEGER) AS INTEGER
DECLARE FUNCTION read_menu_int (menu AS MenuDef, intoffset AS INTEGER) as integer
DECLARE SUB write_menu_int (menu AS MenuDef, intoffset AS INTEGER, n AS INTEGER)
DECLARE FUNCTION read_menu_item_int (mi AS MenuDefItem, intoffset AS INTEGER) as integer
DECLARE SUB write_menu_item_int (mi AS MenuDefItem, intoffset AS INTEGER, n AS INTEGER)
DECLARE SUB position_menu_item (menu AS MenuDef, cap AS STRING, i AS INTEGER, BYREF where AS XYPair)
DECLARE FUNCTION append_menu_item(BYREF menu AS MenuDef, caption AS STRING, t AS INTEGER=0, sub_t AS INTEGER=0) as integer
DECLARE SUB remove_menu_item OVERLOAD(BYREF menu AS MenuDef, BYVAL mi AS MenuDefItem ptr)
DECLARE SUB remove_menu_item OVERLOAD(BYREF menu AS MenuDef, BYVAL mislot AS INTEGER)
DECLARE SUB swap_menu_items(BYREF menu1 AS MenuDef, BYVAL mislot1 AS INTEGER, BYREF menu2 AS MenuDef, BYVAL mislot2 AS INTEGER)
DECLARE SUB append_simplemenu_item (menu() as SimpleMenu, caption as string, BYVAL enabled as integer = YES, BYVAL col as integer = -1, BYVAL dat as integer = 0, BYVAL where as integer = -1)

DECLARE FUNCTION bound_arg(n AS INTEGER, min AS INTEGER, max AS INTEGER, argname AS ZSTRING PTR, context AS ZSTRING PTR=nulzstr, fromscript AS INTEGER=YES, errlvl AS INTEGER = 4) AS INTEGER
DECLARE SUB reporterr(msg AS STRING, errlvl AS INTEGER = 5)

DECLARE FUNCTION load_tag_name (index AS INTEGER) AS STRING
DECLARE SUB save_tag_name (tagname AS STRING, index AS INTEGER)
DECLARE FUNCTION tag_condition_caption(n AS INTEGER, prefix AS STRING="Tag", zerocap AS STRING="", onecap AS STRING="", negonecap AS STRING="") AS STRING
DECLARE FUNCTION tag_set_caption(n AS INTEGER, prefix AS STRING="Set Tag") AS STRING
DECLARE FUNCTION onoroff (n AS INTEGER) AS STRING
DECLARE FUNCTION yesorno (n AS INTEGER, yes_cap AS STRING="YES", no_cap AS STRING="NO") AS STRING
DECLARE FUNCTION format_percent (BYVAL float AS DOUBLE, BYVAL sigfigs AS INTEGER = 5) AS STRING

DECLARE FUNCTION enter_or_space () AS INTEGER
DECLARE FUNCTION copy_keychord () AS INTEGER
DECLARE FUNCTION paste_keychord () AS INTEGER

DECLARE SUB write_npc_int (npcdata AS NPCType, intoffset AS INTEGER, n AS INTEGER)
DECLARE FUNCTION read_npc_int (npcdata AS NPCType, intoffset AS INTEGER) AS INTEGER

DECLARE FUNCTION xreadbit (bitarray() AS INTEGER, bitoffset AS INTEGER, intoffset AS INTEGER=0) AS INTEGER

DECLARE SUB draw_scrollbar OVERLOAD (state AS MenuState, rect AS RectType, count AS INTEGER, boxstyle AS INTEGER=0, page AS INTEGER)
DECLARE SUB draw_scrollbar OVERLOAD (state AS MenuState, rect AS RectType, boxstyle AS INTEGER=0, page AS INTEGER)
DECLARE SUB draw_scrollbar OVERLOAD (state AS MenuState, menu AS MenuDef, page AS INTEGER)
DECLARE SUB draw_fullscreen_scrollbar(state AS MenuState, boxstyle AS INTEGER=0, page AS INTEGER)

DECLARE FUNCTION get_text_box_height(BYREF box AS TextBox) AS INTEGER
DECLARE FUNCTION last_inv_slot() AS INTEGER

DECLARE FUNCTION decode_backslash_codes(s AS STRING) AS STRING
DECLARE FUNCTION escape_nonprintable_ascii(s AS STRING) AS STRING
DECLARE FUNCTION fixfilename (s AS STRING) AS STRING

DECLARE FUNCTION inputfilename (query AS STRING, ext AS STRING, directory AS STRING, helpkey AS STRING, default AS STRING="", BYVAL allow_overwrite AS INTEGER=YES) AS STRING

DECLARE SUB set_homedir()
DECLARE FUNCTION get_help_dir(helpfile AS STRING="") AS STRING
DECLARE FUNCTION load_help_file(helpkey AS STRING) AS STRING
DECLARE SUB save_help_file(helpkey AS STRING, text AS STRING)

DECLARE SUB show_help(helpkey AS STRING)
DECLARE FUNCTION multiline_string_editor(s AS STRING, helpkey AS STRING="") AS STRING

DECLARE FUNCTION int_from_xy(pos AS XYPair, BYVAL wide AS INTEGER, BYVAL high AS INTEGER) AS INTEGER
DECLARE FUNCTION xy_from_int(BYVAL n AS INTEGER, BYVAL wide AS INTEGER, BYVAL high AS INTEGER) AS XYPair

DECLARE FUNCTION color_browser_256(BYVAL start_color AS INTEGER=0) AS INTEGER

'These were added from other, less-appropriate places
DECLARE FUNCTION filenum(n AS INTEGER) AS STRING

'Sprite loading convenience functions
DECLARE SUB load_sprite_and_pal (BYREF img AS GraphicPair, BYVAL spritetype, BYVAL index AS INTEGER, BYVAL palnum AS INTEGER=-1)
DECLARE SUB unload_sprite_and_pal (BYREF img AS GraphicPair)

'strgrabber has separate versions in customsubs.bas and yetmore2.bas
DECLARE FUNCTION strgrabber (s AS STRING, BYVAL maxl AS INTEGER) AS INTEGER


'Global variables
EXTERN sourcerpg as string
EXTERN as string game, tmpdir, exename, workingdir, homedir
EXTERN uilook() as integer
EXTERN as integer vpage, dpage
EXTERN buffer() as integer
EXTERN fadestate as integer
EXTERN master() as RGBcolor
EXTERN gen() as integer
EXTERN fmvol as integer
EXTERN sprite_sizes() AS SpriteSize
EXTERN statnames() as string
EXTERN cmdline_args() as string
EXTERN log_dir as string
EXTERN orig_dir as string
EXTERN data_dir as string
#ifdef IS_CUSTOM
 EXTERN cleanup_on_error as integer
#endif

#endif
