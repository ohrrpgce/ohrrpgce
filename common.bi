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

DECLARE SUB edgeprint (s$, x%, y%, c%, p%)
DECLARE SUB fadein ()
DECLARE SUB fadeout (red%, green%, blue%)
DECLARE FUNCTION usemenu OVERLOAD (pt, top, first, last, size)
DECLARE FUNCTION usemenu OVERLOAD (state AS MenuState)
DECLARE SUB debug (s$)
DECLARE SUB visible_debug (s$)
DECLARE FUNCTION soundfile$ (sfxnum%)
DECLARE SUB getui(colarray(), palnum = -1)
DECLARE SUB safekill (f$)
DECLARE FUNCTION getfixbit(bitnum AS INTEGER) AS INTEGER
DECLARE SUB setfixbit(bitnum AS INTEGER, bitval AS INTEGER)
DECLARE FUNCTION aquiretempdir$ ()
DECLARE SUB writebinstring (savestr$, array%(), offset%, maxlen%)
DECLARE SUB writebadbinstring (savestr$, array%(), offset%, maxlen%, skipword%)
DECLARE FUNCTION readbinstring$ (array%(), offset%, maxlen%)
DECLARE FUNCTION readbadbinstring$ (array%(), offset%, maxlen%, skipword%)
DECLARE FUNCTION read32bitstring$ overload (array%(), offset%)
DECLARE FUNCTION read32bitstring$ overload (strptr as integer ptr)
DECLARE FUNCTION readbadgenericname$ (index%, filename$, recsize%, offset%, size%, skip%)
DECLARE SUB copylump(package$, lump$, dest$, ignoremissing AS INTEGER = 0)
DECLARE SUB centerfuz (x%, y%, w%, h%, c%, p%)
DECLARE SUB centerbox (x%, y%, w%, h%, c%, p%)
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
DECLARE SUB createminimap (array(), map(), tastuf(), tilesetpage, zoom)
DECLARE SUB loadtanim (n, tastuf())
DECLARE SUB savetanim (n, tastuf())
DECLARE SUB writescatter (s$, lhold%, start%)
DECLARE SUB readscatter (s$, lhold%, start%)
DECLARE FUNCTION finddatafile$(filename$)
DECLARE SUB updaterecordlength (lumpf$, bindex AS INTEGER)
DECLARE SUB writepassword (p$)
DECLARE FUNCTION readpassword$ ()
DECLARE SUB upgrade (font%())

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
DECLARE FUNCTION pick_ogg_quality(BYREF quality AS INTEGER) AS INTEGER

DECLARE FUNCTION intgrabber (n AS INTEGER, min AS INTEGER, max AS INTEGER, less AS INTEGER=75, more AS INTEGER=77) AS INTEGER
DECLARE FUNCTION zintgrabber (n AS INTEGER, min AS INTEGER, max AS INTEGER, less AS INTEGER=75, more AS INTEGER=77) AS INTEGER
DECLARE FUNCTION xintgrabber (n AS INTEGER, pmin AS INTEGER, pmax AS INTEGER, nmin AS INTEGER, nmax AS INTEGER, less AS INTEGER=75, more AS INTEGER=77) AS INTEGER
DECLARE SUB upgrade_message (s AS STRING)
DECLARE SUB show_message (s AS STRING)

'Global variables
EXTERN game$
EXTERN tmpdir$
EXTERN exename$
EXTERN uilook()
EXTERN vpage, dpage
EXTERN buffer()
EXTERN fadestate
EXTERN master() as RGBcolor
EXTERN workingdir$
EXTERN keyv()
EXTERN gen()

'Constants
CONST sizebinsize = 4

#ENDIF
