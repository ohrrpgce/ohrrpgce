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

DECLARE SUB edgeprint (s$, x%, y%, c%, p%)
DECLARE SUB fadein ()
DECLARE SUB fadeout (red%, green%, blue%)
DECLARE FUNCTION usemenu OVERLOAD (pt, top, first, last, size)
DECLARE FUNCTION usemenu OVERLOAD (state AS MenuState)
DECLARE SUB standardmenu OVERLOAD (menu$(), state AS MenuState, x, y, page, edge=NO, hidecursor=NO)
DECLARE SUB standardmenu OVERLOAD (menu$(), size, vis, pt, top, x, y, page, edge=NO)
DECLARE SUB debug (s$)
DECLARE SUB visible_debug (s$)
DECLARE FUNCTION soundfile$ (sfxnum%)
DECLARE SUB safekill (f$)
DECLARE FUNCTION getfixbit(bitnum AS INTEGER) AS INTEGER
DECLARE SUB setfixbit(bitnum AS INTEGER, bitval AS INTEGER)
DECLARE FUNCTION aquiretempdir$ ()
DECLARE SUB writebinstring (savestr$, array%(), offset%, maxlen%)
DECLARE SUB writebadbinstring (savestr$, array(), offset, maxlen, skipword=0)
DECLARE FUNCTION readbinstring$ (array%(), offset%, maxlen%)
DECLARE FUNCTION readbadbinstring$ (array(), offset, maxlen, skipword=0)
DECLARE FUNCTION read32bitstring$ overload (array%(), offset%)
DECLARE FUNCTION read32bitstring$ overload (strptr as integer ptr)
DECLARE FUNCTION readbadgenericname$ (index%, filename$, recsize%, offset%, size%, skip%)
DECLARE SUB copylump(package$, lump$, dest$, ignoremissing AS INTEGER = 0)
DECLARE SUB centerfuz (x%, y%, w%, h%, c%, p%)
DECLARE SUB centerbox (x%, y%, w%, h%, c%, p%)
DECLARE SUB edgeboxstyle (x, y, w, h, boxstyle, p, fuzzy=NO)
DECLARE SUB edgebox (x, y, w, h, col, bordercol, p, fuzzy=NO)
DECLARE SUB emptybox (x, y, w, h, col, thick, p)
DECLARE FUNCTION isbit (bb() as INTEGER, BYVAL w as INTEGER, BYVAL b as INTEGER) as INTEGER
DECLARE FUNCTION scriptname$ (num%, trigger% = 0)
DECLARE Function seconds2str(byval sec as integer, byval f as string = "%m:%S") as string
DECLARE SUB loaddefaultpals (fileset AS INTEGER, poffset() AS INTEGER, sets AS INTEGER)
DECLARE SUB savedefaultpals (fileset AS INTEGER, poffset() AS INTEGER, sets AS INTEGER)
DECLARE SUB guessdefaultpals (fileset AS INTEGER, poffset() AS INTEGER, sets AS INTEGER)
DECLARE FUNCTION getdefaultpal(fileset, index)
DECLARE SUB flusharray (array%(), size%, value%)
DECLARE SUB setbinsize (id, size)
DECLARE FUNCTION curbinsize (id)
DECLARE FUNCTION defbinsize (id)
DECLARE FUNCTION getbinsize (id)
DECLARE FUNCTION dimbinsize (id)
DECLARE SUB loadherodata (hero as herodef ptr, index)
DECLARE SUB saveherodata (hero as herodef ptr, index)
DECLARE SUB loadenemydata (array(), index, altfile = 0)
DECLARE SUB saveenemydata (array(), index, altfile = 0)
DECLARE SUB loaditemdata (array(), index)
DECLARE SUB saveitemdata (array(), index)
DECLARE SUB loadoldattackdata (array(), index)
DECLARE SUB saveoldattackdata (array(), index)
DECLARE SUB loadnewattackdata (array(), index)
DECLARE SUB savenewattackdata (array(), index)
DECLARE SUB loadattackdata (array(), index)
DECLARE SUB saveattackdata (array(), index)
DECLARE FUNCTION maplumpname$ (map, oldext$)
DECLARE SUB getpal16 (array(), aoffset, foffset, autotype=-1, sprite=0)
DECLARE SUB storepal16 (array(), aoffset, foffset)
DECLARE SUB fatalerror (e$)
DECLARE FUNCTION xstring (s$, x)
DECLARE FUNCTION defaultint$ (n)
DECLARE SUB poke8bit (array16(), index, val8)
DECLARE FUNCTION peek8bit (array16(), index)
DECLARE SUB loadpalette(pal() as RGBcolor, palnum%)
DECLARE SUB savepalette(pal() as RGBcolor, palnum%)
DECLARE SUB convertpalette(oldpal%(), newpal() as RGBcolor)
DECLARE FUNCTION getmapname$ (m)
DECLARE FUNCTION createminimap (array() AS INTEGER, map() AS INTEGER, tastuf() AS INTEGER, tilesetpage AS INTEGER, zoom AS INTEGER = -1) AS INTEGER
DECLARE SUB loadtanim (n, tastuf())
DECLARE SUB savetanim (n, tastuf())
DECLARE SUB animatetilesets (tilesets() AS TilesetData ptr)
DECLARE SUB cycletile (tanim_state() AS TileAnimState, tastuf() AS INTEGER)
DECLARE SUB loadtilesetdata OVERLOAD (BYREF tileset AS TilesetData ptr, BYVAL tilesetnum AS INTEGER)
DECLARE SUB loadtilesetdata OVERLOAD (tilesets() AS TilesetData ptr, BYVAL layer AS INTEGER, BYVAL tilesetnum AS INTEGER)
DECLARE SUB unloadtilesetdata (BYREF tileset AS TilesetData ptr)
DECLARE SUB loadmaptilesets (tilesets() AS TilesetData ptr, gmap() AS INTEGER)
DECLARE SUB unloadmaptilesets (tilesets() AS TilesetData ptr)
DECLARE SUB writescatter (s$, lhold%, start%)
DECLARE SUB readscatter (s$, lhold%, start%)
DECLARE FUNCTION finddatafile$(filename$)
DECLARE SUB updaterecordlength (lumpf$, bindex AS INTEGER)
DECLARE SUB writepassword (p$)
DECLARE FUNCTION readpassword$ ()
DECLARE SUB upgrade (font%())
DECLARE FUNCTION readglobalstring$ (index, default$, maxlen)
DECLARE SUB load_default_master_palette (master_palette() AS RGBColor)
DECLARE SUB dump_master_palette_as_hex (master_palette() AS RGBColor)

DECLARE FUNCTION readattackname$ (index%)
DECLARE FUNCTION readenemyname$ (index%)
DECLARE FUNCTION readitemname$ (index%)
DECLARE FUNCTION readshopname$ (shopnum%)
DECLARE FUNCTION getsongname$ (num AS INTEGER, prefixnum AS INTEGER = 0)
DECLARE FUNCTION getsfxname$ (num AS INTEGER)

DECLARE SUB playsongnum (songnum%)

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
DECLARE SUB upgrade_message (s AS STRING)
DECLARE SUB show_message (s AS STRING)

DECLARE SUB position_menu (menu AS MenuDef)
DECLARE SUB draw_menu (menu AS MenuDef, state AS MenuState, page AS INTEGER)
DECLARE SUB init_menu_state (BYREF state AS MenuState, menu AS MenuDef)
DECLARE FUNCTION count_menu_items (menu AS MenuDef)
DECLARE FUNCTION find_empty_menu_item (menu AS MenuDef)
DECLARE FUNCTION get_menu_item_caption (mi AS MenuDefItem, menu AS MenuDef) AS STRING
DECLARE FUNCTION get_special_menu_caption(subtype AS INTEGER, edit_mode AS INTEGER= NO) AS STRING
DECLARE SUB create_default_menu(menu AS MenuDef)
DECLARE FUNCTION anchor_point(anchor AS INTEGER, size AS INTEGER) AS INTEGER
DECLARE FUNCTION read_menu_int (menu AS MenuDef, intoffset AS INTEGER)
DECLARE SUB write_menu_int (menu AS MenuDef, intoffset AS INTEGER, n AS INTEGER)
DECLARE FUNCTION read_menu_item_int (mi AS MenuDefItem, intoffset AS INTEGER)
DECLARE SUB write_menu_item_int (mi AS MenuDefItem, intoffset AS INTEGER, n AS INTEGER)
DECLARE SUB position_menu_item (menu AS MenuDef, cap AS STRING, i AS INTEGER, BYREF where AS XYPair)
DECLARE FUNCTION append_menu_item(BYREF menu AS MenuDef, caption AS STRING, t AS INTEGER=0, sub_t AS INTEGER=0)

DECLARE FUNCTION bound_arg(n AS INTEGER, min AS INTEGER, max AS INTEGER, cmd AS STRING, argname AS STRING) AS INTEGER

DECLARE FUNCTION load_tag_name (index AS INTEGER) AS STRING
DECLARE SUB save_tag_name (tagname AS STRING, index AS INTEGER)
DECLARE FUNCTION tag_condition_caption(n AS INTEGER, prefix AS STRING="Tag", zerocap AS STRING="", onecap AS STRING="", negonecap AS STRING="") AS STRING
DECLARE FUNCTION tag_set_caption(n AS INTEGER, prefix AS STRING="Set Tag") AS STRING
DECLARE FUNCTION onoroff (n AS INTEGER) AS STRING

DECLARE FUNCTION enter_or_space () AS INTEGER

DECLARE SUB write_npc_int (npcdata AS NPCType, intoffset AS INTEGER, n AS INTEGER)
DECLARE FUNCTION read_npc_int (npcdata AS NPCType, intoffset AS INTEGER) AS INTEGER

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

#ENDIF
