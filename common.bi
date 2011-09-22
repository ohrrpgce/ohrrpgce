
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
#include "menus.bi"
#include "const.bi"
#include "misc.bi"  'for nulzstr

#ifdef COMMON_BASE_BI
#error Include at most one of common.bi, common_base.bi
#endif

DECLARE FUNCTION common_setoption(opt as string, arg as string) as integer

DECLARE SUB fadein ()
DECLARE SUB fadeout (byval red as integer, byval green as integer, byval blue as integer)
DECLARE SUB start_new_debug ()
DECLARE SUB end_debug ()
DECLARE SUB debug (s as string)
DECLARE SUB debugc CDECL ALIAS "debugc" (byval s as zstring ptr, byval errorlevel as integer)
DECLARE SUB debuginfo (s as string)
DECLARE FUNCTION soundfile (byval sfxnum as integer) as string
DECLARE FUNCTION filesize (file as string) as string

DECLARE FUNCTION acquiretempdir () as string

DECLARE SUB writebinstring OVERLOAD (savestr as string, array() as integer, offset as integer, maxlen as integer)
DECLARE SUB writebinstring OVERLOAD (savestr as string, array() as short, offset as integer, maxlen as integer)
DECLARE SUB writebadbinstring (savestr as string, array() as integer, offset as integer, maxlen as integer, skipword as integer=0)
DECLARE FUNCTION readbinstring OVERLOAD (array() as integer, offset as integer, maxlen as integer) as string
DECLARE FUNCTION readbinstring OVERLOAD (array() as short, offset as integer, maxlen as integer) as string
DECLARE FUNCTION readbadbinstring (array() as integer, offset as integer, maxlen as integer, skipword as integer=0) as string
DECLARE FUNCTION read32bitstring overload (array() as integer, byval offset as integer) as string
DECLARE FUNCTION read32bitstring overload (strptr as integer ptr) as string
DECLARE FUNCTION readbadgenericname (byval index as integer, filename as string, byval recsize as integer, byval offset as integer, byval size as integer, byval skip as integer = 0) as string

ENUM RectTransTypes
 transUndef = -1
 transOpaque = 0
 transFuzzy
 transHollow
END ENUM

DECLARE SUB centerfuz (byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval c as integer, byval p as integer)
DECLARE SUB centerbox (byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval c as integer, byval p as integer)
DECLARE SUB edgeboxstyle OVERLOAD (byref rect as RectType, byval boxstyle as integer, byval p as integer, byval fuzzy as integer=NO, byval supress_borders as integer=NO)
DECLARE SUB edgeboxstyle OVERLOAD (byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval boxstyle as integer, byval p as integer, byval fuzzy as integer=NO, byval supress_borders as integer=NO)
DECLARE SUB center_edgeboxstyle (byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval boxstyle as integer, byval p as integer, byval fuzzy as integer=NO, byval supress_borders as integer=NO)
DECLARE SUB edgebox OVERLOAD (byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval col as integer, byval bordercol as integer, byval p as integer, byval trans as RectTransTypes=transOpaque, byval border as integer=-1, byval fuzzfactor as integer=50)
DECLARE SUB edgebox OVERLOAD (byval x as integer, byval y as integer, byval w as integer, byval h as integer, byval col as integer, byval bordercol as integer, byval fr as Frame Ptr, byval trans as RectTransTypes=transOpaque, byval border as integer=-1, byval fuzzfactor as integer=50)
DECLARE FUNCTION isbit (bb() as integer, byval w as integer, byval b as integer) as integer
DECLARE FUNCTION scriptname (num as integer, trigger as integer = 0) as string
DECLARE Function seconds2str(byval sec as integer, f as string = " %m: %S") as string

DECLARE SUB loaddefaultpals (fileset as integer, poffset() as integer, sets as integer)
DECLARE SUB savedefaultpals (fileset as integer, poffset() as integer, sets as integer)
DECLARE SUB guessdefaultpals (fileset as integer, poffset() as integer, sets as integer)
DECLARE FUNCTION getdefaultpal(fileset as integer, index as integer) as integer
DECLARE FUNCTION abs_pal_num(byval num as integer, byval sprtype as integer, byval spr as integer) as integer

DECLARE FUNCTION getfixbit(byval bitnum as integer) as integer
DECLARE SUB setfixbit(byval bitnum as integer, byval bitval as integer)
DECLARE SUB setbinsize (id as integer, size as integer)
DECLARE FUNCTION curbinsize (id as integer) as integer
DECLARE FUNCTION defbinsize (id as integer) as integer
DECLARE FUNCTION getbinsize (id as integer) as integer
DECLARE FUNCTION dimbinsize (id as integer) as integer
DECLARE FUNCTION readarchinym (gamedir as string, sourcefile as string) as string
DECLARE FUNCTION maplumpname (map as integer, oldext as string) as string

DECLARE FUNCTION xstring (s as string, x as integer) as integer
DECLARE FUNCTION defaultint (n as integer, default_caption as STRING="default") as STRING
DECLARE FUNCTION caption_or_int (n as integer, captions() as STRING) as STRING
DECLARE SUB poke8bit (array16() as integer, index as integer, val8 as integer)
DECLARE FUNCTION peek8bit (array16() as integer, index as integer) as integer

DECLARE SUB loadpalette(pal() as RGBcolor, byval palnum as integer)
DECLARE SUB savepalette(pal() as RGBcolor, byval palnum as integer)
DECLARE SUB convertpalette(oldpal() as integer, newpal() as RGBcolor)

DECLARE FUNCTION createminimap OVERLOAD (map() as TileMap, tilesets() as TilesetData ptr, byref zoom as integer = -1) as Frame PTR
DECLARE FUNCTION createminimap OVERLOAD (layer as TileMap, tileset as TilesetData ptr, byref zoom as integer = -1) as Frame PTR
DECLARE SUB animatetilesets (tilesets() as TilesetData ptr)
DECLARE SUB cycletile (tanim_state() as TileAnimState, tastuf() as integer)
DECLARE SUB loadtilesetdata (tilesets() as TilesetData ptr, byval layer as integer, byval tilesetnum as integer, byval lockstep as integer = YES)
DECLARE SUB reloadtileanimations (tilesets() as TilesetData ptr, gmap() as integer)
DECLARE SUB unloadtilesetdata (byref tileset as TilesetData ptr)
DECLARE FUNCTION layer_tileset_index(byval layer as integer) as integer
DECLARE SUB loadmaptilesets (tilesets() as TilesetData ptr, gmap() as integer, byval resetanimations as integer = YES)
DECLARE SUB unloadmaptilesets (tilesets() as TilesetData ptr)
DECLARE FUNCTION finddatafile(filename as string) as string
DECLARE FUNCTION finddatadir(dirname as STRING) as STRING
DECLARE SUB updaterecordlength (lumpf as string, byval bindex as integer, byval headersize as integer = 0, byval repeating as integer = NO)
DECLARE SUB clamp_value (byref value as integer, byval min as integer, byval max as integer, argname as string)

DECLARE SUB writepassword (pass as string)
DECLARE FUNCTION checkpassword (pass as string) as integer
DECLARE FUNCTION getpassword () as string

DECLARE SUB upgrade ()
DECLARE SUB future_rpg_warning ()
DECLARE SUB rpg_sanity_checks ()
DECLARE SUB fix_sprite_record_count(byval pt_num as integer)
DECLARE SUB fix_record_count(byref last_rec_index as integer, byref record_byte_size as integer, lumpname as STRING, info as STRING, skip_header_bytes as integer=0, count_offset as integer=0)
DECLARE SUB loadglobalstrings ()
DECLARE FUNCTION readglobalstring (index as integer, default as string, maxlen as integer=10) as string
DECLARE SUB load_default_master_palette (master_palette() as RGBColor)
DECLARE SUB dump_master_palette_as_hex (master_palette() as RGBColor)

DECLARE FUNCTION readattackname (byval index as integer) as string
DECLARE FUNCTION readenemyname (byval index as integer) as string
DECLARE FUNCTION readitemname (byval index as integer) as string
DECLARE FUNCTION readitemdescription (byval index as integer) as string
DECLARE FUNCTION readshopname (byval shopnum as integer) as string
DECLARE FUNCTION getsongname (byval num as integer, byval prefixnum as integer = 0) as string
DECLARE FUNCTION getsfxname (byval num as integer) as string
DECLARE FUNCTION getheroname (byval hero_id as integer) as string
DECLARE FUNCTION getmapname (byval m as integer) as string
DECLARE SUB getstatnames(statnames() as STRING)
DECLARE SUB getelementnames(elmtnames() as STRING)

DECLARE FUNCTION getdisplayname (default as string) as string

DECLARE SUB playsongnum (byval songnum as integer)

DECLARE FUNCTION spawn_and_wait (app as STRING, args as STRING) as string
DECLARE FUNCTION find_helper_app (appname as STRING) as STRING
DECLARE FUNCTION missing_helper_message (appname as STRING) as STRING
DECLARE FUNCTION find_madplay () as STRING
DECLARE FUNCTION find_oggenc () as STRING
DECLARE FUNCTION can_convert_mp3 () as integer
DECLARE FUNCTION can_convert_wav () as integer
DECLARE FUNCTION mp3_to_ogg (in_file as STRING, out_file as STRING, quality as integer = 5) as STRING
DECLARE FUNCTION mp3_to_wav (in_file as STRING, out_file as STRING) as STRING
DECLARE FUNCTION wav_to_ogg (in_file as STRING, out_file as STRING, quality as integer = 5) as STRING

DECLARE FUNCTION intgrabber OVERLOAD (byref n as integer, byval min as integer, byval max as integer, byval less as integer=scLeft, byval more as integer=scRight, byval returninput as integer=NO, byval use_clipboard as integer=YES, byval autoclamp as integer=YES) as integer
DECLARE FUNCTION intgrabber OVERLOAD (byref n as LONGINT, byval min as LONGINT, byval max as LONGINT, byval less as integer=scLeft, byval more as integer=scRight, byval returninput as integer=NO, byval use_clipboard as integer=YES, byval autoclamp as integer=YES) as integer
DECLARE FUNCTION zintgrabber (n as integer, min as integer, max as integer, less as integer=75, more as integer=77) as integer
DECLARE FUNCTION xintgrabber (n as integer, pmin as integer, pmax as integer, nmin as integer=1, nmax as integer=1, less as integer=scLeft, more as integer=scRight) as integer

DECLARE SUB reset_console (top as integer = 0, h as integer = 200, c as integer = -1)
DECLARE SUB show_message (s as STRING)
DECLARE SUB append_message (s as STRING)

DECLARE SUB basic_textbox (msg as string, byval col as integer, byval page as integer)
DECLARE SUB notification (msg as STRING)
DECLARE SUB visible_debug (s as string)
DECLARE SUB showerror (msg as string, byval isfatal as integer = NO)
DECLARE SUB fatalerror (msg as string)
DECLARE SUB pop_warning(s as STRING)
DECLARE FUNCTION multichoice(capt as STRING, choices() as STRING, defaultval as integer=0, escval as integer=-1, helpkey as STRING="") as integer
DECLARE FUNCTION twochoice(capt as STRING, strA as STRING="Yes", strB as STRING="No", defaultval as integer=0, escval as integer=-1, helpkey as STRING="") as integer
DECLARE FUNCTION yesno(capt as STRING, byval defaultval as integer=YES, escval as integer=NO) as integer

DECLARE SUB create_default_menu(menu as MenuDef)

DECLARE FUNCTION bound_arg(n as integer, min as integer, max as integer, argname as ZSTRING PTR, context as ZSTRING PTR=nulzstr, fromscript as integer=YES, errlvl as integer = 4) as integer
DECLARE SUB reporterr(msg as STRING, errlvl as integer = 5)

DECLARE FUNCTION load_tag_name (index as integer) as STRING
DECLARE SUB save_tag_name (tagname as STRING, index as integer)
DECLARE FUNCTION tag_condition_caption(n as integer, prefix as STRING="Tag", zerocap as STRING="", onecap as STRING="", negonecap as STRING="") as STRING
DECLARE FUNCTION tag_set_caption(n as integer, prefix as STRING="Set Tag") as STRING
DECLARE FUNCTION onoroff (n as integer) as STRING
DECLARE FUNCTION yesorno (n as integer, yes_cap as STRING="YES", no_cap as STRING="NO") as STRING
DECLARE FUNCTION format_percent (byval float as DOUBLE, byval sigfigs as integer = 5) as STRING

DECLARE FUNCTION enter_or_space () as integer
DECLARE FUNCTION copy_keychord () as integer
DECLARE FUNCTION paste_keychord () as integer

DECLARE SUB write_npc_int (npcdata as NPCType, intoffset as integer, n as integer)
DECLARE FUNCTION read_npc_int (npcdata as NPCType, intoffset as integer) as integer

DECLARE FUNCTION xreadbit (bitarray() as integer, bitoffset as integer, intoffset as integer=0) as integer

DECLARE FUNCTION get_text_box_height(byref box as TextBox) as integer
DECLARE FUNCTION last_inv_slot() as integer

DECLARE FUNCTION decode_backslash_codes(s as STRING) as STRING
DECLARE FUNCTION escape_nonprintable_ascii(s as STRING) as STRING
DECLARE FUNCTION fixfilename (s as STRING) as STRING

DECLARE FUNCTION inputfilename (query as STRING, ext as STRING, directory as STRING, helpkey as STRING, default as STRING="", byval allow_overwrite as integer=YES) as STRING
DECLARE FUNCTION prompt_for_string (byref s as STRING, caption as STRING, byval limit as integer=NO) as integer

DECLARE SUB set_homedir()
DECLARE FUNCTION get_help_dir(helpfile as STRING="") as STRING
DECLARE FUNCTION load_help_file(helpkey as STRING) as STRING
DECLARE SUB save_help_file(helpkey as STRING, text as STRING)

DECLARE SUB show_help(helpkey as STRING)
DECLARE FUNCTION multiline_string_editor(s as STRING, helpkey as STRING="") as STRING

DECLARE FUNCTION int_from_xy(pos as XYPair, byval wide as integer, byval high as integer) as integer
DECLARE FUNCTION xy_from_int(byval n as integer, byval wide as integer, byval high as integer) as XYPair

DECLARE FUNCTION color_browser_256(byval start_color as integer=0) as integer

'These were added from other, less-appropriate places
DECLARE FUNCTION filenum(n as integer) as STRING

'Sprite loading convenience functions
DECLARE SUB load_sprite_and_pal (byref img as GraphicPair, byval spritetype as integer, byval index as integer, byval palnum as integer=-1)
DECLARE SUB unload_sprite_and_pal (byref img as GraphicPair)

'strgrabber has separate versions in customsubs.bas and yetmore2.bas
DECLARE FUNCTION strgrabber (s as STRING, byval maxl as integer) as integer

DECLARE FUNCTION exptolevel (level as integer) as integer
DECLARE FUNCTION total_exp_to_level (level as integer) as integer
DECLARE FUNCTION current_max_level() as integer
DECLARE FUNCTION atlevel (lev as integer, a0 as integer, aMax as integer) as integer
DECLARE FUNCTION atlevel_quadratic (byval lev as double, byval a0 as double, byval aMax as double, byval midpercent as double) as double

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
EXTERN sprite_sizes() as SpriteSize
EXTERN statnames() as string
EXTERN cmdline_args() as string
EXTERN log_dir as string
EXTERN orig_dir as string
EXTERN data_dir as string
EXTERN negative_zero as integer
#ifdef IS_CUSTOM
 EXTERN cleanup_on_error as integer
#endif

#endif
