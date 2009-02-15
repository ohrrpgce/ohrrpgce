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

DECLARE SUB edgeprint (s as string, x as integer, y as integer, c as integer, p as integer)
DECLARE SUB fadein ()
DECLARE SUB fadeout (red as integer, green as integer, blue as integer)
DECLARE FUNCTION usemenu OVERLOAD (pt as integer, top as integer, first as integer, last as integer, size as integer) as integer
DECLARE FUNCTION usemenu OVERLOAD (state AS MenuState) as integer
DECLARE FUNCTION usemenu OVERLOAD (state AS MenuState, enabled() AS INTEGER) as integer
DECLARE SUB standardmenu OVERLOAD (menu() as string, state AS MenuState, x as integer, y as integer, page as integer, edge as integer=NO, hidecursor as integer=NO)
DECLARE SUB standardmenu OVERLOAD (menu() as string, size as integer, vis as integer, pt as integer, top as integer, x as integer, y as integer, page as integer, edge as integer=NO)
DECLARE SUB debug (s as string)
DECLARE SUB visible_debug (s as string)
DECLARE FUNCTION soundfile (sfxnum as integer) as string
DECLARE SUB safekill (f as string)
DECLARE FUNCTION getfixbit(bitnum AS INTEGER) AS INTEGER
DECLARE SUB setfixbit(bitnum AS INTEGER, bitval AS INTEGER)
DECLARE FUNCTION aquiretempdir () as string
DECLARE SUB writebinstring (savestr as string, array() as integer, offset as integer, maxlen as integer)
DECLARE SUB writebadbinstring (savestr as string, array() as integer, offset as integer, maxlen as integer, skipword as integer=0)
DECLARE FUNCTION readbinstring (array() as integer, offset as integer, maxlen as integer) as string
DECLARE FUNCTION readbadbinstring (array() as integer, offset as integer, maxlen as integer, skipword as integer=0) as string
DECLARE FUNCTION read32bitstring overload (array() as integer, offset as integer) as string
DECLARE FUNCTION read32bitstring overload (strptr as integer ptr) as string
DECLARE FUNCTION readbadgenericname (index as integer, filename as string, recsize as integer, offset as integer, size as integer, skip as integer) as string
DECLARE SUB copylump(package as string, lump as string, dest as string, ignoremissing AS INTEGER = 0)
DECLARE SUB centerfuz (x as integer, y as integer, w as integer, h as integer, c as integer, p as integer)
DECLARE SUB centerbox (x as integer, y as integer, w as integer, h as integer, c as integer, p as integer)
DECLARE SUB edgeboxstyle (x as integer, y as integer, w as integer, h as integer, boxstyle as integer, p as integer, fuzzy as integer=NO, supress_borders as integer=NO)
DECLARE SUB center_edgeboxstyle (x as integer, y as integer, w as integer, h as integer, boxstyle as integer, p as integer, fuzzy as integer=NO, supress_borders as integer=NO)
DECLARE SUB edgebox (x as integer, y as integer, w as integer, h as integer, col as integer, bordercol as integer, p as integer, fuzzy as integer=NO, border as integer=-1)
DECLARE SUB emptybox (x as integer, y as integer, w as integer, h as integer, col as integer, thick as integer, p as integer)
DECLARE FUNCTION isbit (bb() as INTEGER, BYVAL w as INTEGER, BYVAL b as INTEGER) as INTEGER
DECLARE FUNCTION scriptname (num as integer, trigger as integer = 0) as string
DECLARE Function seconds2str(byval sec as integer, byval f as string = " %m: %S") as string
DECLARE SUB loaddefaultpals (fileset AS INTEGER, poffset() AS INTEGER, sets AS INTEGER)
DECLARE SUB savedefaultpals (fileset AS INTEGER, poffset() AS INTEGER, sets AS INTEGER)
DECLARE SUB guessdefaultpals (fileset AS INTEGER, poffset() AS INTEGER, sets AS INTEGER)
DECLARE FUNCTION getdefaultpal(fileset as integer, index as integer) as integer
DECLARE SUB flusharray (array(), BYVAL size AS INTEGER=-1, BYVAL value AS INTEGER=0)
DECLARE SUB setbinsize (id as integer, size as integer)
DECLARE FUNCTION curbinsize (id as integer) as integer
DECLARE FUNCTION defbinsize (id as integer) as integer
DECLARE FUNCTION getbinsize (id as integer) as integer
DECLARE FUNCTION dimbinsize (id as integer) as integer
DECLARE SUB loadherodata (hero as herodef ptr, index as integer)
DECLARE SUB saveherodata (hero as herodef ptr, index as integer)
DECLARE SUB loadenemydata (array() as integer, index as integer, altfile as integer = 0)
DECLARE SUB saveenemydata (array() as integer, index as integer, altfile as integer = 0)
DECLARE SUB loaditemdata (array() as integer, index as integer)
DECLARE SUB saveitemdata (array() as integer, index as integer)
DECLARE FUNCTION maplumpname (map as integer, oldext as string) as string
DECLARE SUB getpal16 (array() as integer, aoffset as integer, foffset as integer, autotype as integer=-1, sprite as integer=0)
DECLARE SUB storepal16 (array() as integer, aoffset as integer, foffset as integer)
DECLARE SUB fatalerror (e as string)
DECLARE FUNCTION xstring (s as string, x as integer) as integer
DECLARE FUNCTION defaultint (n AS INTEGER, default_caption AS STRING="default") AS STRING
DECLARE SUB poke8bit (array16() as integer, index as integer, val8 as integer)
DECLARE FUNCTION peek8bit (array16() as integer, index as integer) as integer
DECLARE SUB loadpalette(pal() as RGBcolor, palnum as integer)
DECLARE SUB savepalette(pal() as RGBcolor, palnum as integer)
DECLARE SUB convertpalette(oldpal() as integer, newpal() as RGBcolor)
DECLARE FUNCTION getmapname (m as integer) as string
DECLARE FUNCTION createminimap (map() AS INTEGER, tilesets() AS TilesetData ptr, BYREF zoom AS INTEGER = -1) AS Frame PTR
DECLARE SUB loadtanim (n as integer, tastuf() as integer)
DECLARE SUB savetanim (n as integer, tastuf() as integer)
DECLARE SUB animatetilesets (tilesets() AS TilesetData ptr)
DECLARE SUB cycletile (tanim_state() AS TileAnimState, tastuf() AS INTEGER)
DECLARE SUB loadtilesetdata OVERLOAD (BYREF tileset AS TilesetData ptr, BYVAL tilesetnum AS INTEGER)
DECLARE SUB loadtilesetdata OVERLOAD (tilesets() AS TilesetData ptr, BYVAL layer AS INTEGER, BYVAL tilesetnum AS INTEGER)
DECLARE SUB unloadtilesetdata (BYREF tileset AS TilesetData ptr)
DECLARE SUB loadmaptilesets (tilesets() AS TilesetData ptr, gmap() AS INTEGER, BYVAL resetanimations as integer = YES)
DECLARE SUB unloadmaptilesets (tilesets() AS TilesetData ptr)
DECLARE SUB writescatter (s as string, lhold as integer, start as integer)
DECLARE SUB readscatter (s as string, lhold as integer, start as integer)
DECLARE FUNCTION finddatafile(filename as string) as string
DECLARE SUB updaterecordlength (lumpf as string, bindex AS INTEGER)
DECLARE SUB writepassword (p as string)
DECLARE FUNCTION readpassword () as string
DECLARE SUB upgrade (font() as integer)
DECLARE FUNCTION readglobalstring (index as integer, default as string, maxlen as integer) as string
DECLARE SUB load_default_master_palette (master_palette() AS RGBColor)
DECLARE SUB dump_master_palette_as_hex (master_palette() AS RGBColor)

DECLARE FUNCTION readattackname (index as integer) as string
DECLARE FUNCTION readenemyname (index as integer) as string
DECLARE FUNCTION readitemname (index as integer) as string
DECLARE FUNCTION readshopname (shopnum as integer) as string
DECLARE FUNCTION getsongname  (num AS INTEGER, prefixnum AS INTEGER = 0)as string
DECLARE FUNCTION getsfxname (num AS INTEGER) as string

DECLARE SUB playsongnum (songnum as integer)

DECLARE FUNCTION find_helper_app (appname AS STRING) AS STRING
DECLARE FUNCTION find_madplay () AS STRING
DECLARE FUNCTION find_oggenc () AS STRING
DECLARE FUNCTION can_convert_mp3 () AS INTEGER
DECLARE FUNCTION can_convert_wav () AS INTEGER
DECLARE SUB mp3_to_ogg (in_file AS STRING, out_file AS STRING, quality AS INTEGER = 5)
DECLARE SUB mp3_to_wav (in_file AS STRING, out_file AS STRING)
DECLARE SUB wav_to_ogg (in_file AS STRING, out_file AS STRING, quality AS INTEGER = 5)

DECLARE FUNCTION intgrabber (n AS INTEGER, min AS INTEGER, max AS INTEGER, less AS INTEGER=75, more AS INTEGER=77) AS INTEGER
DECLARE FUNCTION zintgrabber (n AS INTEGER, min AS INTEGER, max AS INTEGER, less AS INTEGER=75, more AS INTEGER=77) AS INTEGER
DECLARE FUNCTION xintgrabber (n AS INTEGER, pmin AS INTEGER, pmax AS INTEGER, nmin AS INTEGER, nmax AS INTEGER, less AS INTEGER=75, more AS INTEGER=77) AS INTEGER
DECLARE SUB reset_console (top AS INTEGER = 0, bottom AS INTEGER = 199, c AS INTEGER = 0)
DECLARE SUB show_message (s AS STRING)
DECLARE SUB append_message (s AS STRING)

DECLARE SUB position_menu (menu AS MenuDef)
DECLARE SUB draw_menu (menu AS MenuDef, state AS MenuState, page AS INTEGER)
DECLARE SUB init_menu_state (BYREF state AS MenuState, menu AS MenuDef)
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

DECLARE FUNCTION bound_arg(n AS INTEGER, min AS INTEGER, max AS INTEGER, cmd AS STRING, argname AS STRING) AS INTEGER

DECLARE FUNCTION load_tag_name (index AS INTEGER) AS STRING
DECLARE SUB save_tag_name (tagname AS STRING, index AS INTEGER)
DECLARE FUNCTION tag_condition_caption(n AS INTEGER, prefix AS STRING="Tag", zerocap AS STRING="", onecap AS STRING="", negonecap AS STRING="") AS STRING
DECLARE FUNCTION tag_set_caption(n AS INTEGER, prefix AS STRING="Set Tag") AS STRING
DECLARE FUNCTION onoroff (n AS INTEGER) AS STRING
DECLARE FUNCTION yesorno (n AS INTEGER, yes_cap AS STRING="YES", no_cap AS STRING="NO") AS STRING

DECLARE FUNCTION enter_or_space () AS INTEGER

DECLARE SUB write_npc_int (npcdata AS NPCType, intoffset AS INTEGER, n AS INTEGER)
DECLARE FUNCTION read_npc_int (npcdata AS NPCType, intoffset AS INTEGER) AS INTEGER

DECLARE FUNCTION xreadbit (bitarray() AS INTEGER, bitoffset AS INTEGER, intoffset AS INTEGER=0) AS INTEGER
DECLARE FUNCTION getheroname (hero_id AS INTEGER) AS STRING
DECLARE FUNCTION getmenuname(record AS INTEGER) AS STRING

DECLARE SUB draw_scrollbar OVERLOAD (state AS MenuState, rect AS RectType, count AS INTEGER, boxstyle AS INTEGER=0, page AS INTEGER)
DECLARE SUB draw_scrollbar OVERLOAD (state AS MenuState, rect AS RectType, boxstyle AS INTEGER=0, page AS INTEGER)
DECLARE SUB draw_scrollbar OVERLOAD (state AS MenuState, menu AS MenuDef, page AS INTEGER)
DECLARE SUB draw_fullscreen_scrollbar(state AS MenuState, boxstyle AS INTEGER=0, page AS INTEGER)

DECLARE FUNCTION range (number AS INTEGER, percent AS INTEGER) AS INTEGER
DECLARE FUNCTION rpad (s AS STRING, pad_char AS STRING, size AS INTEGER) AS STRING
DECLARE FUNCTION str2int (stri AS STRING, default AS INTEGER=0) AS INTEGER

DECLARE SUB load_box_border_cache()
DECLARE SUB clear_box_border_cache()

DECLARE SUB notification (show_msg AS STRING)

DECLARE FUNCTION get_text_box_height(BYREF box AS TextBox) AS INTEGER
DECLARE FUNCTION last_inv_slot() AS INTEGER

DECLARE FUNCTION decode_backslash_codes(s AS STRING) AS STRING
DECLARE FUNCTION escape_nonprintable_ascii(s AS STRING) AS STRING

'Sprite loading convenience functions
DECLARE FUNCTION standard_sprite_load (BYVAL spritetype AS INTEGER, BYVAL index AS INTEGER) AS Frame PTR
DECLARE FUNCTION standard_pal16_load (BYVAL palnum AS INTEGER = -1, BYVAL spritetype AS INTEGER, BYVAL index AS INTEGER) AS Palette16 PTR
DECLARE SUB load_sprite_and_pal (BYREF img AS GraphicPair, BYVAL spritetype, BYVAL index AS INTEGER, BYVAL palnum AS INTEGER=-1)
DECLARE SUB unload_sprite_and_pal (BYREF img AS GraphicPair)

'Global variables
EXTERN as string game, tmpdir, exename, workingdir
EXTERN uilook() as integer
EXTERN as integer vpage, dpage
EXTERN buffer() as integer
EXTERN fadestate as integer
EXTERN master() as RGBcolor
EXTERN keyv() as integer
EXTERN gen() as integer
EXTERN fmvol as integer
EXTERN sprite_sizes() AS SpriteSize

#ENDIF
