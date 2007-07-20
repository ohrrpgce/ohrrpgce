'OHRRPGCE - Some Custom/Game common code
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#ifndef COMMON_BI
#define COMMON_BI

#include "util.bi"
#include "udts.bi"

DECLARE FUNCTION browse$ (special, default$, fmask$, tmp$, needf = 0)
DECLARE SUB edgeprint (s$, x%, y%, c%, p%)
DECLARE SUB fadein ()
DECLARE SUB fadeout (red%, green%, blue%)
DECLARE FUNCTION usemenu (pt%, top%, first%, last%, size%)
DECLARE SUB debug (s$)
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
DECLARE SUB copylump(package$, lump$, dest$)
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
DECLARE SUB set_herostat_byindex(BYREF st AS Stats, index AS INTEGER, value AS INTEGER)
DECLARE FUNCTION get_herostat_byindex(BYREF st AS Stats, index AS INTEGER)

'Global variables
EXTERN game$
EXTERN uilook()
EXTERN vpage, dpage
EXTERN buffer()
EXTERN version$
EXTERN fadestate
EXTERN master() as RGBcolor
EXTERN workingdir$
EXTERN keyv()
EXTERN gen()

'Constants
CONST sizebinsize = 4

#ENDIF