'OHRRPGCE - Some Custom/Game common code
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
' This file is for code that is shared between GAME and CUSTOM.
' Code that is not OHRRPGCE-specific that would be of general use
' to any FreeBasic program belongs in util.bas instead

#include "const.bi"
#include "compat.bi"
#include "allmodex.bi"

#include "udts.bi"
#include "scrconst.bi"
#include "uiconst.bi"
#include "common.bi"

#include "music.bi"
#include "loading.bi"

'Subs and functions only used here
DECLARE SUB setup_sprite_sizes ()

'keyv() is a global declared in common.bi
'It is populated in keyboardsetup() which has
'different implementations in custom and game
REDIM keyv(55, 3)

#IFDEF IS_GAME
DECLARE SUB embedtext (text$, limit=0)
DECLARE FUNCTION istag (num, zero) as integer
#ENDIF

'Allocate sprite size table
REDIM sprite_sizes(8) AS SpriteSize
setup_sprite_sizes

'a primitive system for printing messages that scroll
TYPE ConsoleData
 AS INTEGER x = 0, y = 0, top = 0, bottom = 199, c = 0
END TYPE
DIM SHARED console AS ConsoleData

'don't black out the screen to show upgrade messages if there aren't any
DIM SHARED upgrademessages AS INTEGER

'Allocate the Box Border Cache
DIM SHARED box_border_cache_loaded AS INTEGER = NO
DIM SHARED box_border_cache(14) AS GraphicPair

SUB edgeprint (s$, x, y, c, p)
textcolor uilook(uiOutline), 0
printstr s$, x, y + 1, p
printstr s$, x + 1, y, p
printstr s$, x + 2, y + 1, p
printstr s$, x + 1, y + 2, p
textcolor c, 0
printstr s$, x + 1, y + 1, p
END SUB

'fade in and out not actually used in custom
SUB fadein ()
fadestate = 1
fadetopal master()
END SUB

SUB fadeout (red, green, blue)
fadestate = 0
fadeto red, green, blue
END SUB

SUB safekill (f$)
IF isfile(f$) THEN KILL f$
END SUB

FUNCTION usemenu (state AS MenuState) as integer
 WITH state
  RETURN usemenu(.pt, .top, .first, .last, .size)
 END WITH
END FUNCTION

FUNCTION usemenu (pt, top, first, last, size) as integer

oldptr = pt
oldtop = top

IF keyval(72) > 1 THEN pt = loopvar(pt, first, last, -1) 'UP
IF keyval(80) > 1 THEN pt = loopvar(pt, first, last, 1)  'DOWN
IF keyval(73) > 1 THEN pt = large(pt - size, first)      'PGUP
IF keyval(81) > 1 THEN pt = small(pt + size, last)       'PGDN
IF keyval(71) > 1 THEN pt = first                         'HOME
IF keyval(79) > 1 THEN pt = last                          'END
top = bound(top, pt - size, pt)

IF oldptr = pt AND oldtop = top THEN
 usemenu = 0
ELSE
 usemenu = 1
END IF

END FUNCTION

FUNCTION usemenu (state AS MenuState, enabled() AS INTEGER) as integer
'a version for menus with unselectable items, skip items for which enabled = 0

WITH state
 oldptr = .pt
 oldtop = .top
 d = 0

 IF keyval(72) > 1 THEN d = -1   'UP
 IF keyval(80) > 1 THEN d = 1    'DOWN
 IF keyval(73) > 1 THEN            'PGUP
  .pt = large(.pt - .size, .first)
  WHILE enabled(.pt) = 0 AND .pt > 0 : .pt = loopvar(.pt, .first, .last, -1) : WEND
  IF enabled(.pt) = 0 THEN d = 1
 END IF
 IF keyval(81) > 1 THEN            'PGDN
  .pt = small(.pt + .size, .last)
  WHILE enabled(.pt) = 0 AND .pt < .last : .pt = loopvar(.pt, .first, .last, 1) : WEND
  IF enabled(.pt) = 0 THEN d = -1
 END IF
 IF keyval(71) > 1 THEN .pt = .last : d = 1    'HOME
 IF keyval(79) > 1 THEN .pt = .first : d = -1    'END

 IF d THEN 
  DO
   .top = bound(.top, .pt - .size, .pt)
   .pt = loopvar(.pt, .first, .last, d)
  LOOP WHILE enabled(.pt) = 0
 END IF
 .top = bound(.top, .pt - .size, .pt)

 IF oldptr = .pt AND oldtop = .top THEN
  usemenu = 0
 ELSE
  usemenu = 1
 END IF
END WITH
END FUNCTION


FUNCTION soundfile (sfxnum%) as string
	DIM as string sfxbase

	sfxbase = workingdir & SLASH & "sfx" & sfxnum%
	IF isfile(sfxbase & ".ogg") THEN
   RETURN sfxbase & ".ogg"
  ELSEIF isfile(sfxbase & ".mp3") THEN
   RETURN sfxbase & ".mp3"
  ELSEIF isfile(sfxbase & ".wav") THEN
   RETURN sfxbase & ".wav"
	ELSE
   RETURN ""
	END IF
END FUNCTION

SUB debug (s$)
 DIM filename$
 #IFDEF IS_GAME
   filename$ = "g_debug.txt"
 #ELSE
   filename$ = "c_debug.txt"
 #ENDIF
 fh = FREEFILE
 OPEN filename$ FOR APPEND AS #fh
 PRINT #fh, s$
 CLOSE #fh
END SUB

SUB visible_debug (s$)
 debug s$
 centerbox 160, 100, 300, 36, 3, vpage
 edgeprint s$, 15, 90, uilook(uiText), vpage
 edgeprint "Press any key...", 15, 100, uilook(uiMenuItem), vpage
 setvispage vpage 'refresh
 w = getkey
END SUB

FUNCTION getfixbit(bitnum AS INTEGER) AS INTEGER
 DIM f$
 f$ = workingdir + SLASH + "fixbits.bin"
 IF NOT isfile(f$) THEN RETURN 0
 DIM bits(1) as INTEGER
 setpicstuf bits(), 2, -1
 loadset f$, 0, 0
 RETURN readbit(bits(), 0, bitnum)
END FUNCTION

SUB setfixbit(bitnum AS INTEGER, bitval AS INTEGER)
 DIM f$
 f$ = workingdir + SLASH + "fixbits.bin"
 DIM bits(1) as INTEGER
 setpicstuf bits(), 2, -1
 IF isfile(f$) THEN
  loadset f$, 0, 0
 END IF
 setbit bits(), 0, bitnum, bitval
 storeset f$, 0, 0
END SUB

FUNCTION aquiretempdir () as string
#IFNDEF __FB_LINUX__
'Windows only behavior
tmp$ = environ$("TEMP")
IF NOT fileiswriteable(tmp$ & SLASH & "writetest.tmp") THEN tmp$ = environ("TMP")
IF NOT fileiswriteable(tmp$ & SLASH & "writetest.tmp") THEN tmp$ = exepath$
IF NOT fileiswriteable(tmp$ & SLASH & "writetest.tmp") THEN tmp$ = ""
IF NOT fileiswriteable(tmp$ & SLASH & "writetest.tmp") THEN debug "Unable to find any writable temp dir"
safekill tmp$ & SLASH & "writetest.tmp"
IF RIGHT$(tmp$, 1) <> SLASH THEN tmp$ = tmp$ & SLASH
tmp$ = tmp$ & "ohrrpgce"
#ELSE
'Linux only behavior
#IFDEF IS_CUSTOM
RETURN "" 'Custom doesn't use this sub anyway...
#ELSE
tmp$ = environ$("HOME")
tmp$ = tmp$ & SLASH & ".ohrrpgce"
IF NOT isdir(tmp$) THEN makedir(tmp$)
tmp$ = tmp$ & SLASH
#ENDIF
#ENDIF
d$ = DATE
t$ = TIME
tmp$ = tmp$ & MID$(d$,7,4) & MID$(d$,1,2) & MID$(d$,4,2) & MID$(t$,1,2) & MID$(t$,4,2) & MID$(t$,7,2) & "." & STR$(INT(RND * 1000)) & ".tmp"
tmp$ = tmp$ & SLASH
RETURN tmp$
END FUNCTION

SUB copylump(package$, lump$, dest$, ignoremissing AS INTEGER = 0)
if len(dest$) and right(dest$, 1) <> SLASH then dest$ = dest$ + SLASH
IF isdir(package$) THEN
 'unlumped folder
 IF ignoremissing THEN
  IF NOT isfile(package$ + SLASH + lump$) THEN EXIT SUB
 END IF
 copyfile package$ + SLASH + lump$, dest$ + lump$
ELSE
 'lumpfile
 unlumpfile package$, lump$, dest$
END IF
END SUB

'Backwards compatability wrapper
SUB centerbox (x, y, w, h, c, p)
 IF c < 0 OR c > 15 THEN debug "Warning: invalid box style " & c & " in centerbox"
 center_edgeboxstyle x, y, w, h, c - 1, p
END SUB

SUB emptybox (x, y, w, h, col, thick, p)
 rectangle x, y, w, thick, col, p
 IF h > 0 THEN
  rectangle x, y + h - thick, w, thick, col, p
 END IF
 rectangle x, y, thick, h, col, p
 IF w > 0 THEN
  rectangle x + w - thick, y, thick, h, col, p
 END IF
END SUB

SUB load_box_border_cache()
 DIM i AS INTEGER
 DIM index AS INTEGER
 FOR i = 0 TO 14
  WITH box_border_cache(i)
   IF .sprite THEN sprite_unload(@.sprite)
   IF .pal THEN palette16_unload(@.pal)
   index = uilook(uiTextBoxFrame + i) - 1
   IF index >= 0 THEN
    .sprite = sprite_load(game & ".pt7", index, 16, 16, 16)
    .pal = palette16_load(game + ".pal", -1, 7, index)
   END IF
  END WITH
 NEXT i
 box_border_cache_loaded = YES
END SUB

SUB clear_box_border_cache()
 DIM i AS INTEGER
 FOR i = 0 TO 14
  WITH box_border_cache(i)
   IF .sprite THEN sprite_unload(@.sprite)
   IF .pal THEN palette16_unload(@.pal)
  END WITH
 NEXT i
 box_border_cache_loaded = NO
END SUB

SUB center_edgeboxstyle (x, y, w, h, boxstyle, p, fuzzy=NO, supress_borders=NO)
 edgeboxstyle x - w / 2, y - h / 2, w, h, boxstyle, p, fuzzy, supress_borders
END SUB

SUB edgeboxstyle (x, y, w, h, boxstyle, p, fuzzy=NO, supress_borders=NO)
 IF boxstyle < 0 OR boxstyle > 14 THEN
  debug "edgeboxstyle: invalid boxstyle " & boxstyle
  EXIT SUB
 END IF
 DIM textcol   AS INTEGER = uilook(uiTextBox + 2 * boxstyle)
 DIM bordercol AS INTEGER = uilook(uiTextBox + 2 * boxstyle + 1)
 DIM borders AS INTEGER = boxstyle
 IF supress_borders THEN borders = -1
 edgebox x, y, w, h, textcol, bordercol, p, fuzzy, borders
END SUB

SUB edgebox (x, y, w, h, col, bordercol, p, fuzzy=NO, border=-1)
IF fuzzy THEN
 fuzzyrect x, y, w, h, col, p
ELSE
 rectangle x, y, w, h, col, p
END IF
'--Simple line border
'Top edge
rectangle x, y, w, 1, bordercol, p
'Bottom edge
IF h > 0 THEN
 rectangle x, y + h - 1, w, 1, bordercol, p
END IF
'Left Edge
rectangle x, y, 1, h, bordercol, p
'Right Edge
IF w > 0 THEN
 rectangle x + w - 1, y, 1, h, bordercol, p
END IF
IF border >= 0 and border <= 14 THEN
 '--Graphical Border
 IF box_border_cache_loaded = NO THEN load_box_border_cache
 DIM i AS INTEGER
 WITH box_border_cache(border)
  IF .sprite THEN ' Only proceed if a sprite is actually selected
   'Draw edges
   '--Top and bottom edges
   FOR i = x + 8 TO x + w - 24 STEP 16
    sprite_draw .sprite + 2, .pal, i, y - 8, 1, YES, p
    sprite_draw .sprite + 13, .pal, i, y + h - 8, 1, YES, p
   NEXT i
   '--Left and right edges
   FOR i = y + 8 TO y + h - 24 STEP 16
    sprite_draw .sprite + 7, .pal, x - 8, i, 1, YES, p
    sprite_draw .sprite + 8, .pal, x + w - 8, i, 1, YES, p
   NEXT i
   'Draw end-pieces
   IF w > 16 THEN
    '--Top end pieces
    sprite_draw .sprite + 3, .pal, x + w - 24, y - 8, 1, YES, p
    sprite_draw .sprite + 1, .pal, x + 8, y - 8, 1, YES, p
    '--Bottom end pieces
    sprite_draw .sprite + 14, .pal, x + w - 24, y + h - 8, 1, YES, p
    sprite_draw .sprite + 12, .pal, x + 8, y + h - 8, 1, YES, p
   END IF
   IF h > 16 THEN
    '--Left side end pieces
    sprite_draw .sprite + 9, .pal, x - 8, y + h - 24, 1, YES, p
    sprite_draw .sprite + 5, .pal, x - 8, y + 8, 1, YES, p
    '--Right side end pieces
    sprite_draw .sprite + 10, .pal, x + w - 8, y + h - 24, 1, YES, p
    sprite_draw .sprite + 6, .pal, x + w - 8, y + 8, 1, YES, p
   END IF
   'Draw corners
   sprite_draw .sprite, .pal, x - 8, y - 8, 1, YES, p
   sprite_draw .sprite + 4, .pal, x + w - 8, y - 8, 1, YES, p
   sprite_draw .sprite + 11, .pal, x - 8, y + h - 8, 1, YES, p
   sprite_draw .sprite + 15, .pal, x + w - 8, y + h - 8, 1, YES, p
  END IF
 END WITH
END IF
END SUB

SUB centerfuz (x, y, w, h, c, p)
 IF c < 0 OR c > 15 THEN debug "Warning: invalid box style " & c & " in centerbox"
 center_edgeboxstyle x, y, w, h, c - 1, p, YES
END SUB

FUNCTION readbinstring (array(), offset, maxlen) as string

result$ = ""
strlen = bound(array(offset), 0, maxlen)

i = 1
DO WHILE LEN(result$) < strlen
 '--get an int
 n = array(offset + i)
 i = i + 1

 '--append the lowbyte as a char
 result$ = result$ + CHR$(n AND &HFF)

 '--if we still care about the highbyte, append it as a char too
 IF LEN(result$) < strlen THEN
  result$ = result$ + CHR$((n SHR 8) AND &HFF)
 END IF

LOOP

return result$
END FUNCTION

SUB writebinstring (savestr$, array(), offset, maxlen)
s$ = savestr$

'--pad s$ to the right length
DO WHILE LEN(s$) < maxlen
 s$ = s$ + CHR$(0)
LOOP

'--if it is an odd number
IF (LEN(s$) AND 1) THEN
 s$ = s$ + CHR$(0)
END IF

'--write length (current not max)
array(offset) = LEN(savestr$)

FOR i = 1 TO LEN(s$) \ 2
 array(offset + i) = s$[2 * i - 2] OR (s$[2 * i - 1] SHL 8)
NEXT

END SUB

FUNCTION readbadbinstring (array(), offset, maxlen, skipword=0) as string
result$ = ""
strlen = bound(array(offset), 0, maxlen)

FOR i = 1 TO strlen
 '--read and int
 n = array(offset + skipword + i)
 '--if the int is a char use it.
 IF n >= 0 AND n <= 255 THEN
  '--take the low byte
  n = (n AND &HFF)
  '--use it
  result$ = result$ + CHR$(n)
 END IF
NEXT i

return result$
END FUNCTION

SUB writebadbinstring (savestr$, array(), offset, maxlen, skipword=0)

'--write current length
array(offset) = LEN(savestr$)

FOR i = 1 TO LEN(savestr$)
 array(offset + skipword + i) = savestr$[i - 1]
NEXT i

FOR i = LEN(savestr$) + 1 TO maxlen
 array(offset + skipword + i) = 0
NEXT i

END SUB

FUNCTION read32bitstring (array(), offset) as string
result$ = ""
word = array(offset + 1)
FOR i = 1 TO array(offset)
 result$ += CHR$(word AND 255)
 IF i MOD 4 = 0 THEN word = array(offset + i \ 4 + 1) ELSE word = word SHR 8
NEXT
return result$
END FUNCTION

FUNCTION read32bitstring (stringptr as integer ptr) as string
result$ = ""
word = stringptr[1]
FOR i = 1 TO stringptr[0]
 result$ += CHR$(word AND 255)
 IF i MOD 4 = 0 THEN word = stringptr[i \ 4 + 1] ELSE word = word SHR 8
NEXT
return result$
END FUNCTION

FUNCTION readbadgenericname (index, filename$, recsize, offset, size, skip) as string

'--clobbers buffer!

result$ = ""

IF index >= 0 THEN
 setpicstuf buffer(), recsize, -1
 loadset filename$, index, 0
 result$ = readbadbinstring$(buffer(), offset, size, skip)
END IF

return result$

END FUNCTION

FUNCTION isbit (bb() as INTEGER, BYVAL w as INTEGER, BYVAL b as INTEGER) as INTEGER
 IF readbit (bb(), w, b) THEN
  RETURN -1
 ELSE
  RETURN 0
 END IF
END FUNCTION

FUNCTION scriptname (num, trigger = 0) as string
#ifdef IS_GAME
 'remember script names!
 STATIC cachenum, cacheids(24), cachenames(24) as string, gamename$
 IF game <> gamename$ THEN
  gamename$ = game
  cachenum = 0
 END IF
 FOR i = 0 TO cachenum - 1
  IF cacheids(i) = num THEN RETURN cachenames(i)
 NEXT
#endif

DIM buf(19)
IF num >= 16384 AND trigger > 0 THEN
 IF loadrecord (buf(), workingdir + SLASH + "lookup" + STR$(trigger) + ".bin", 20, num - 16384) THEN
  sname$ = readbinstring(buf(), 1, 36)
  IF buf(0) THEN
   a$ = sname$
  ELSE
   a$ = "[" + sname$ + "]"
  END IF
  GOTO theend
 END IF
END IF

IF num THEN
 a$ = "[id " + STR$(num) + "]"
 fh = FREEFILE
 OPEN workingdir + SLASH + "plotscr.lst" FOR BINARY AS #fh
 WHILE loadrecord (buf(), fh, 20)
  IF buf(0) = num THEN
   a$ = STRING$(small(large(buf(1), 0), 38), " ")
   array2str buf(), 4, a$
   EXIT WHILE
  END IF
 WEND
 CLOSE fh
ELSE
 a$ = "[none]"
END IF

theend:
#ifdef IS_GAME
 IF cachenum = 25 THEN cachenum = 0
 cacheids(cachenum) = num
 cachenames(cachenum) = a$
 cachenum += 1
#endif
return a$
END FUNCTION

Function seconds2str(byval sec as integer, byval f as string = "%m:%S") as string
  dim ret as string
  dim as integer s, m, h
  s = sec mod 60
  m = (sec \ 60) mod 60
  h = (sec \ 3600) mod 60

  dim as integer i
  for i = 0 to len(f) - 1
    if f[i] = asc("%") then
      i+=1
      select case as const f[i]
        case asc("s")
          ret = ret & sec
        case asc("S")
          if s < 10 then ret = ret & "0"
          ret = ret & s
        case asc("m")
          ret = ret & (sec \ 60)
        case asc("M")
          if m < 10 then ret = ret & "0"
          ret = ret & m
        case asc("h")
          ret = ret & (sec \ 3600)
        case asc("H")
          if h < 10 then ret = ret & "0"
          ret = ret & h
        case asc("%")
          ret = ret & "%"
      end select
    else
      ret = ret & chr(f[i])
    end if
  next

  return ret
end function

FUNCTION getdefaultpal(fileset, index) as integer
 DIM v AS SHORT
 f$ = workingdir & SLASH & "defpal" & fileset & ".bin"
 IF isfile(f$) THEN
   fh = FREEFILE
   OPEN f$ FOR BINARY AS #fh
   GET #fh, 1 + index * 2, v
   CLOSE #fh
   RETURN v
 ELSE
  debug "Default palette file " & f$ & " does not exist"
 END IF
END FUNCTION

SUB loaddefaultpals(fileset, poffset(), sets)
 DIM v AS SHORT
 f$ = workingdir & SLASH & "defpal" & fileset & ".bin"
 IF isfile(f$) THEN
   fh = FREEFILE
   OPEN f$ FOR BINARY AS #fh
   FOR i = 0 to sets
    GET #fh, 1 + i * 2, v
    poffset(i) = v
   NEXT i
   CLOSE #fh
 ELSE
   guessdefaultpals fileset, poffset(), sets
 END IF
END SUB

SUB savedefaultpals(fileset, poffset(), sets)
 DIM v AS SHORT
 f$ = workingdir & SLASH & "defpal" & fileset & ".bin"
 fh = FREEFILE
 OPEN f$ FOR BINARY AS #fh
 FOR i = 0 to sets
  v = poffset(i)
  PUT #fh, 1 + i * 2, v
 NEXT i
 CLOSE #fh
END SUB

SUB guessdefaultpals(fileset, poffset(), sets)
 dim her as herodef
 
 flusharray poffset(), sets, 0
 SELECT CASE fileset
 CASE 0 'Heroes
  'REDIM buf(318)
  FOR j = 0 TO gen(genMaxHero) 'I reversed the loops, because it's more efficient  
   FOR i = 0 TO sets           'to do the file I/O in the outer loop
    loadherodata @her, j
    IF her.sprite = i THEN
     poffset(i) = her.sprite_pal
     EXIT FOR
    END IF
   NEXT
  NEXT
 CASE 1 TO 3 'Enemies
  REDIM buf(160)
  FOR i = 0 TO sets
   FOR j = 0 TO gen(genMaxEnemy)
    loadenemydata buf(), j
    IF buf(53) = i AND buf(55) + 1 = fileset THEN
     poffset(i) = buf(54)
     EXIT FOR
    END IF
   NEXT j
  NEXT i
 CASE 4 'Walkabouts
  'REDIM buf(318)
  REDIM npcbuf(1500)
  FOR i = 0 TO sets
   found = 0
   FOR j = 0 TO gen(genMaxHero)
    loadherodata @her, j
    
    IF her.walk_sprite = i THEN
     poffset(i) = her.walk_sprite_pal
     found = 1
     EXIT FOR
    END IF
   NEXT j
   IF found = 0 THEN
    FOR mapi = 0 TO gen(genMaxMap)
     xbload maplumpname$(mapi, "n"), npcbuf(), "npcstat lump " & mapi & " is missing"
     FOR j = 0 to max_npc_defs
      IF npcbuf(15 * j + 0) = i THEN
       poffset(i) = npcbuf(15 * j + 1)
       found = 1
       EXIT FOR
      END IF
     NEXT j
     IF found THEN EXIT FOR
    NEXT mapi
   END IF
  NEXT i
 CASE 5 'Weapons
  REDIM buf(99)
  FOR i = 0 TO sets
   FOR j = 0 TO gen(genMaxItem)
    loaditemdata buf(), j
    IF buf(49) = 1 AND buf(52) = i THEN
     poffset(i) = buf(53)
     EXIT FOR
    END IF
   NEXT j
  NEXT i
 CASE 6 'Attacks
  REDIM buf(40 + dimbinsize(binATTACK))
  FOR i = 0 TO sets
   FOR j = 0 TO gen(genMaxAttack)
    loadattackdata buf(), j
    IF buf(0) = i THEN
     poffset(i) = buf(1)
     EXIT FOR
    END IF
   NEXT j
  NEXT i
 CASE ELSE
  debug "Unknown sprite type: " & fileset
 END SELECT
END SUB

SUB flusharray (array(), BYVAL size AS INTEGER=-1, BYVAL value AS INTEGER=0)
'If size is -1, then flush the entire array
IF size = -1 THEN size = UBOUND(array)
FOR i = 0 TO size
 array(i) = value
NEXT i
END SUB

SUB loadherodata (hero as herodef ptr, index)
deserherodef game & ".dt0", hero, index
END SUB

SUB saveherodata (hero as herodef ptr, index)
serherodef game & ".dt0", hero, index
END SUB

SUB loadenemydata (array(), index, altfile = 0)
IF altfile THEN
 filename$ = tmpdir & "dt1.tmp"
ELSE
 filename$ = game & ".dt1"
END IF
loadrecord array(), filename$, 160, index
END SUB

SUB saveenemydata (array(), index, altfile = 0)
IF altfile THEN
 filename$ = tmpdir & "dt1.tmp"
ELSE
 filename$ = game & ".dt1"
END IF
storerecord array(), filename$, 160, index
END SUB

SUB loaditemdata (array(), index)
 flusharray array(), 99, 0
 IF index > gen(genMaxItem) THEN debug "loaditemdata:" & index & " out of range" : EXIT SUB
 IF loadrecord(array(), game & ".itm", 100, index) = 0 THEN debug "loaditemdata:" & index & " loadrecord failed" : EXIT SUB
END SUB

SUB saveitemdata (array(), index)
storerecord array(), game & ".itm", 100, index
END SUB

FUNCTION defbinsize (id) as integer
 'returns the default size in BYTES to use for getbinsize() when no BINSIZE data is available at all
 IF id = 0 THEN RETURN 0  'attack.bin
 IF id = 1 THEN RETURN 64 '.stf
 IF id = 2 THEN RETURN 0  'songdata.bin
 IF id = 3 THEN RETURN 0  'sfxdata.bin
 IF id = 4 THEN RETURN 40 '.map
 IF id = 5 THEN RETURN 0  'menus.bin
 IF id = 6 THEN RETURN 0  'menuitem.bin
 IF id = 7 THEN RETURN 0  'uicolors.bin
 IF id = 8 THEN RETURN 400 '.say
 RETURN 0
END FUNCTION

FUNCTION curbinsize (id) as integer
 'returns the correct size in BYTES for of the records for the version you are running
 IF id = 0 THEN RETURN 122 'attack.bin
 IF id = 1 THEN RETURN 84  '.stf
 IF id = 2 THEN RETURN 32  'songdata.bin
 IF id = 3 THEN RETURN 34  'sfxdata.bin
 IF id = 4 THEN RETURN 50  '.map
 IF id = 5 THEN RETURN 48  'menus.bin
 IF id = 6 THEN RETURN 64  'menuitem.bin
 IF id = 7 THEN RETURN 126 'uicolors.bin
 IF id = 8 THEN RETURN 410 '.say
 RETURN 0
END FUNCTION

FUNCTION getbinsize (id) as integer
'returns the current size in BYTES of the records in the specific binary file you are working with
IF isfile(workingdir + SLASH + "binsize.bin") THEN
 fbdim recordsize
 fh = FREEFILE
 OPEN workingdir + SLASH + "binsize.bin" FOR BINARY AS #fh
 IF LOF(fh) < 2 * id + 2 THEN
  getbinsize = defbinsize(id)
 ELSE
  GET #fh, 1 + id * 2, recordsize
  getbinsize = recordsize
 END IF
 CLOSE #fh
ELSE
 getbinsize = defbinsize(id)
END IF

END FUNCTION

'INTS, not bytes!
FUNCTION dimbinsize (id) as integer
 'curbinsize is size supported by current version of engine
 'getbinsize is size of data in RPG file
 dimbinsize = large(curbinsize(id), getbinsize(id)) / 2
END FUNCTION

SUB setbinsize (id, size)
fbdim size16
size16 = size
fh = FREEFILE
OPEN workingdir + SLASH + "binsize.bin" FOR BINARY AS #fh
PUT #fh, 1 + id * 2, size16
CLOSE #fh
END SUB

FUNCTION maplumpname (map, oldext$) as string
 IF map < 100 THEN
  return game & "." & oldext$ & RIGHT$("0" & map, 2)
 ELSE
  return workingdir & SLASH & map & "." & oldext$
 END IF
END FUNCTION

SUB getpal16 (array(), aoffset, foffset, autotype=-1, sprite=0)
DIM buf(8)

loadrecord buf(), game + ".pal", 8, 0
IF buf(0) = 4444 THEN '--check magic number
 IF buf(1) >= foffset AND foffset >= 0 THEN
  'palette is available
  loadrecord buf(), game + ".pal", 8, 1 + foffset
  FOR i = 0 TO 7
   array(aoffset * 8 + i) = buf(i)
  NEXT i
 ELSEIF foffset = -1 THEN
  'load a default palette
  IF autotype >= 0 THEN
   defaultpal = getdefaultpal(autotype, sprite)
   'Recursive
   getpal16 array(), aoffset, defaultpal
  END IF
 ELSE
  'palette is out of range, return blank
  FOR i = 0 TO 7
   array(aoffset * 8 + i) = 0
  NEXT i
 END IF
ELSE '--magic number not found, palette is still in BSAVE format
 DIM xbuf(100 * 8)
 xbload game + ".pal", xbuf(), "16-color palletes missing from " + game
 FOR i = 0 TO 7
  array(aoffset * 8 + i) = xbuf(foffset * 8 + i)
 NEXT i
END IF

END SUB

SUB storepal16 (array(), aoffset, foffset)
DIM buf(8)

f$ = game + ".pal"
loadrecord buf(), f$, 8, 0

IF buf(0) <> 4444 THEN
 fatalerror "16-color palette file may be corrupt"
END IF

last = buf(1)

IF foffset > last THEN
 '--blank out palettes before extending file
 FOR i = last + 1 TO foffset
  flusharray buf(), 8, 0
  storerecord buf(), f$, 8, 1 + i
 NEXT i
 '--update header
 buf(0) = 4444
 buf(1) = foffset
 storerecord buf(), f$, 8, 0
END IF

IF foffset >= 0 THEN '--never write a negative file offset
 'copy palette to buffer
 FOR i = 0 TO 7
  buf(i) = array(aoffset * 8 + i)
 NEXT i
 'write palette
 storerecord buf(), f$, 8, 1 + foffset
END IF

END SUB

SUB fatalerror (e$)
DIM ypos AS INTEGER
#IFDEF IS_GAME
debug "fatal error:" + e$
setvispage 0
centerbox 160, 100, 310, 180, 3, 0
ypos = 20
DO WHILE LEN(e$) > 38
 edgeprint LEFT(e$, 38), 8, ypos, uilook(uiText), 0
 e$ = MID(e$, 38)
 ypos += 10
LOOP
edgeprint e$, 8, ypos, uilook(uiText), 0
ypos += 15
edgeprint "Press ESC to cleanly close the program", 8, ypos, uilook(uiMenuItem), 0
edgeprint "or any other key to ignore the", 8, ypos + 10, uilook(uiMenuItem), 0
edgeprint "error and try to continue playing.", 8, ypos + 20, uilook(uiMenuItem), 0

setvispage 0 'refresh
w = getkey

IF w = 1 THEN
 closemusic
 restoremode
 PRINT e$
 SYSTEM
END IF
#ENDIF
#IFDEF IS_CUSTOM
debug "fatal error:" + e$
textcolor uilook(uiText), 0
clearpage 0

ypos = 0
DO WHILE LEN(e$) > 38
 edgeprint LEFT(e$, 38), 0, ypos, uilook(uiText), 0
 e$ = MID(e$, 38)
 ypos += 8
LOOP
printstr e$, 0, ypos, 0
ypos += 16
printstr "an error has occured. Press ESC to", 0, ypos, 0
printstr "close " + CUSTOMEXE + " or press any other", 0, ypos + 8, 0
printstr "key to try to continue. If the",             0, ypos + 16, 0
printstr "error keeps happening, send e-mail to",      0, ypos + 24, 0
printstr "ohrrpgce-crash@HamsterRepublic.com",         0, ypos + 32, 0
setvispage 0 'refresh

w = getkey

IF w = 1 THEN
 closemusic
 restoremode

 touchfile workingdir + SLASH + "__danger.tmp"

 PRINT "fatal error:"
 PRINT e$

 'borrowed this code from game.bas cos wildcard didn't work in FB
 findfiles workingdir + SLASH + ALLFILES, 0, "filelist.tmp", buffer()
 fh = FREEFILE
 OPEN "filelist.tmp" FOR INPUT AS #fh
 DO UNTIL EOF(fh)
  LINE INPUT #fh, filename$
  KILL workingdir + SLASH + filename$
 LOOP
 CLOSE #fh
 KILL "filelist.tmp"
 RMDIR workingdir

 SYSTEM
END IF
#ENDIF
END SUB

FUNCTION xstring (s$, x) as integer
 return small(large(x - LEN(s$) * 4, 0), 319 - LEN(s$) * 8)
END FUNCTION

FUNCTION defaultint (n AS INTEGER, default_caption AS STRING="default") AS STRING
 IF n = -1 THEN RETURN default_caption
 RETURN STR(n)
END FUNCTION

SUB poke8bit (array16(), index, val8)
 IF val8 <> (val8 AND &hFF) THEN
   debug "Warning: " & val8 & " is not an 8-bit number. Discarding bits: " & (val8 XOR &hFF)
   val8 = val8 AND &hFF
 END IF
 element = array16(index \ 2)
 lb = element AND &hFF
 hb = (element AND &hFF00) SHR 8
 IF index AND 1 THEN
  hb = val8
 ELSE
  lb = val8
 END IF
 element = lb OR (hb SHL 8)
 array16(index \ 2) = element
END SUB

FUNCTION peek8bit (array16(), index) as integer
 element = array16(index \ 2)
 IF index AND 1 THEN
  RETURN (element AND &hFF00) SHR 8
 ELSE
  RETURN element AND &hFF
 END IF
END FUNCTION

SUB loadpalette(pal() as RGBcolor, palnum)
IF palnum < 0 THEN
 debug "loadpalette: invalid palnum " & palnum
 palnum = 0
END IF
IF NOT isfile(workingdir + SLASH + "palettes.bin") THEN
 '.MAS fallback, palnum ignored because it doesn't matter
 DIM oldpalbuf(767)
 xbload game + ".mas", oldpalbuf(), "master palette missing from " + game
 convertpalette oldpalbuf(), pal()
ELSE
 DIM AS SHORT headsize, recsize
 DIM palbuf(767) as UBYTE

 fh = FREEFILE
 OPEN workingdir + SLASH + "palettes.bin" FOR BINARY AS #fh
 GET #fh, , headsize
 GET #fh, , recsize
 GET #fh, recsize * palnum + headsize + 1, palbuf()
 CLOSE #fh
 FOR i = 0 TO 255
  pal(i).r = palbuf(i * 3)
  pal(i).g = palbuf(i * 3 + 1)
  pal(i).b = palbuf(i * 3 + 2)
 NEXT
END IF
'Uncomment the line below if you want the palette in text format for updating load_default_master_palette
'dump_master_palette_as_hex pal()
END SUB

SUB savepalette(pal() as RGBcolor, palnum)
IF isfile(workingdir + SLASH + "palettes.bin") THEN
 DIM AS SHORT headsize, recsize

 fh = FREEFILE
 OPEN workingdir + SLASH + "palettes.bin" FOR BINARY AS #fh
 GET #fh, , headsize
 GET #fh, , recsize

 DIM palbuf(recsize - 1) as UBYTE
 FOR i = 0 TO 255
  palbuf(i * 3) = pal(i).r
  palbuf(i * 3 + 1) = pal(i).g
  palbuf(i * 3 + 2) = pal(i).b
 NEXT
 PUT #fh, recsize * palnum + headsize + 1, palbuf()
 CLOSE #fh
END IF
END SUB

SUB convertpalette(oldpal() as integer, newpal() as RGBcolor)
'takes a old QB style palette (as 768 ints), translates it to
'8 bits per component and writes it to the provided RGBcolor array
FOR i = 0 TO 255
 r = oldpal(i * 3)
 g = oldpal(i * 3 + 1)
 b = oldpal(i * 3 + 2)
 'newpal(i).r = r shl 2 or r shr 4
 'newpal(i).g = g shl 2 or g shr 4
 'newpal(i).b = b shl 2 or b shr 4
 newpal(i).r = iif(r, r shl 2 + 3, 0)   'Mapping as Neo suggested
 newpal(i).g = iif(g, g shl 2 + 3, 0)
 newpal(i).b = iif(b, b shl 2 + 3, 0)
NEXT
END SUB

FUNCTION getmapname (m) as string
DIM nameread(39)
loadrecord nameread(), game + ".mn", 40, m
a$ = STRING$(small((nameread(0) AND 255), 39), " ")
array2str nameread(), 1, a$
RETURN a$
END FUNCTION

FUNCTION createminimap (map() AS INTEGER, tilesets() AS TilesetData ptr, BYREF zoom AS INTEGER = -1) AS Frame PTR
 'if zoom is -1, calculate and store it

 IF zoom = -1 THEN
  'auto-detect best zoom
  zoom = bound(small(320 \ map(0), 200 \ map(1)), 1, 20)
 END IF

 DIM mini as Frame Ptr
 mini = Callocate(sizeof(Frame))
 mini->w = zoom * map(0)
 mini->h = zoom * map(1)
 mini->image = Allocate(zoom * map(0) * zoom * map(1))
 mini->mask = NULL
 mini->refcount = -1234
 'REDIM array(zoom * map(0) - 1, zoom * map(1) - 1) AS UBYTE
 DIM AS SINGLE fraction
 fraction = 20 / zoom

 setmapdata map(), map(), 20, 0
 FOR j = 0 TO zoom * map(1) - 1
  FOR i = 0 TO zoom * map(0) - 1
   tx = i \ zoom
   ty = j \ zoom
   x = INT(((i MOD zoom) + RND) * fraction)
   y = INT(((j MOD zoom) + RND) * fraction)
   'layers but not overhead tiles
   FOR k = 2 TO 0 STEP -1
    block = readmapblock(tx, ty, k)
    IF block = 0 AND k > 0 THEN CONTINUE FOR

    WITH *tilesets(k)
     IF block > 207 THEN block = (block - 48 + .tastuf(20) + .anim(1).cycle) MOD 160
     IF block > 159 THEN block = (block + .tastuf(0) + .anim(0).cycle) MOD 160
     DIM AS INTEGER temp = .spr->image[block * 400 + y * 20 + x]
     mini->image[i + j * mini->w] = temp 
     IF temp <> 0 THEN EXIT FOR
    END WITH
   NEXT
  NEXT
 NEXT

 RETURN mini
END FUNCTION

FUNCTION readattackname (index) as string
'--clobbers buffer!!!
 return readbadgenericname(index, game + ".dt6", 80, 24, 10, 1)
END FUNCTION

FUNCTION readenemyname (index) as string
 '--clobbers buffer!!!
 readenemyname = readbadgenericname(index, game + ".dt1", 320, 0, 16, 0)
END FUNCTION

FUNCTION readitemname (index) as string
 '--clobbers buffer!!!
 readitemname = readbadgenericname(index, game + ".itm", 200, 0, 8, 0)
END FUNCTION

FUNCTION readshopname (shopnum) as string
 'clobbers buffer!
 readshopname = readbadgenericname(shopnum, game + ".sho", 40, 0, 15, 0)
END FUNCTION

FUNCTION getsongname (num AS INTEGER, prefixnum AS INTEGER = 0) as string
DIM songd(dimbinsize(binSONGDATA)) AS INTEGER
DIM s AS STRING
IF num = -1 THEN RETURN "-none-"
s = ""
IF prefixnum THEN s = num & " "
setpicstuf songd(), curbinsize(2), -1
loadset workingdir + SLASH + "songdata.bin", num, 0
s = s & readbinstring$ (songd(), 0, 30)
RETURN s
END FUNCTION

FUNCTION getsfxname (num AS INTEGER) as string
DIM sfxd(dimbinsize(binSFXDATA))
setpicstuf sfxd(), curbinsize(3), -1
loadset workingdir + SLASH + "sfxdata.bin", num, 0
return readbinstring (sfxd(), 0, 30)
END FUNCTION

FUNCTION intgrabber (n AS INTEGER, min AS INTEGER, max AS INTEGER, less AS INTEGER=75, more AS INTEGER=77) AS INTEGER
STATIC clip
old = n

IF more <> 0 AND keyval(more) > 1 THEN
 n = loopvar(n, min, max, 1)
ELSEIF less <> 0 AND keyval(less) > 1 THEN
 n = loopvar(n, min, max, -1)
ELSE
 s = SGN(n)
 n = ABS(n)
 IF keyval(14) > 1 THEN n \= 10
 FOR i = 1 TO 9
  IF keyval(i + 1) > 1 THEN n = n * 10 + i
 NEXT i
 IF keyval(11) > 1 THEN n *= 10
 IF min < 0 AND max > 0 THEN
  IF keyval(12) > 1 OR keyval(13) > 1 OR keyval(74) > 1 OR keyval(78) > 1 THEN s = s * -1
 END IF
 IF min < 0 AND (s < 0 OR max = 0) THEN n = -n
 'CLIPBOARD
 IF (keyval(29) > 0 AND keyval(82) > 1) OR ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(83) > 0) OR (keyval(29) > 0 AND keyval(46) > 1) THEN clip = n
 IF ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(82) > 1) OR (keyval(29) > 0 AND keyval(47) > 1) THEN n = clip
 n = large(min, n)
 n = small(max, n)
END IF

IF old = n THEN
 intgrabber = 0
ELSE
 intgrabber = 1
END IF

END FUNCTION

FUNCTION zintgrabber (n AS INTEGER, min AS INTEGER, max AS INTEGER, less AS INTEGER=75, more AS INTEGER=77) AS INTEGER
'--adjust for entries that are offset by +1
'--what a hack!
'--all entries <= 0 are special options not meant to be enumerated
'--supply the min & max as visible, not actual range for n
'--eg a menu with 'A' = -2, 'B' = -1, 'C' = 0, 'item 0 - item 99' = 1 - 100 would have min = -3, max = 99
old = n
temp = n - 1
'--must adjust to always be able to type in a number
IF temp < 0 THEN
 FOR i = 2 TO 11
  IF keyval(i) > 1 THEN temp = 0
 NEXT i
END IF
intgrabber temp, min, max, less, more
n = temp + 1
IF old = 1 AND keyval(14) > 1 THEN n = 0

IF old = n THEN
 zintgrabber = 0
ELSE
 zintgrabber = 1
END IF

END FUNCTION

FUNCTION xintgrabber (n AS INTEGER, pmin AS INTEGER, pmax AS INTEGER, nmin AS INTEGER, nmax AS INTEGER, less AS INTEGER=75, more AS INTEGER=77) AS INTEGER
'--a little bit of documentation required:
'--like zintgrabber, but for cases where positive values mean one thing, negatives
'--another, and 0 means none.

'nmin and nmax should be negative or 0. nmax should be less than nmin
'nmax can be 0 for no negative range
'nmin - nmax is the range of negative values
'eg. nmin = -1 nmax = -100: negatives indicate a number between 1 and 100
'pmin - pmax is position range, eg. 2 - 50

old = n
temp = n

'depending on n, align sequence to match displayed

IF old > 0 THEN
 temp = temp + pmin - 1
END IF

IF old < 0 THEN
 temp = temp + nmin + 1
END IF

'IF old = 0 THEN
'END IF


intgrabber temp, nmax, pmax, less, more

IF keyval(12) > 1 OR keyval(13) > 1 OR keyval(74) > 1 OR keyval(78) > 1 THEN negated = 1


IF old > 0 THEN
 IF temp >= pmin AND temp <= pmax THEN
  temp = temp - pmin + 1
 ELSE
  IF (temp >= 0 AND temp < pmin) OR (temp = -1 AND negated = 0) THEN
   'you've hit backspace or left or something
   temp = 0
  ELSE
   'you've hit minus or went off the far boundary
   temp = temp - nmin - 1
   'check the inverted value is in the other set
   IF temp > 0 THEN temp = 0
  END IF
 END IF
END IF

IF old < 0 THEN
 IF temp >= nmax AND temp <= nmin THEN
  temp = temp - nmin - 1
 ELSE
  IF (temp <= 0 AND temp > nmin) OR (temp = 1 AND negated = 0) THEN
   temp = 0
  ELSE
   temp = temp - pmin + 1
   IF temp < 0 THEN temp = 0
  END IF
 END IF
END IF

IF old = 0 THEN
 IF temp < 0 THEN temp = -1 'must have pressed left
 IF temp > 0 THEN
  IF temp < pmin OR keyval(more) > 1 THEN temp = 1 ELSE temp = temp - pmin + 1
 END IF
END IF

'backspace? goto none
IF temp = SGN(temp) AND keyval(14) > 1 THEN temp = 0

n = temp
IF old = n THEN
 xintgrabber = 0
ELSE
 xintgrabber = 1
END IF

END FUNCTION

SUB playsongnum (songnum%)
	DIM songbase$, songfile$

	songbase$ = workingdir & SLASH & "song" & songnum%
  songfile$ = ""
  
  IF isfile(songbase$ & ".mp3") THEN
    songfile$ = songbase$ & ".mp3"
  ELSEIF isfile(songbase$ & ".ogg") THEN
    songfile$ = songbase$ & ".ogg"
  ELSEIF isfile(songbase$ & ".mod") THEN
    songfile$ = songbase$ & ".mod"
  ELSEIF isfile(songbase$ & ".xm") THEN
    songfile$ = songbase$ & ".xm"
  ELSEIF isfile(songbase$ & ".s3m") THEN
    songfile$ = songbase$ & ".s3m"
  ELSEIF isfile(songbase$ & ".it") THEN
    songfile$ = songbase$ & ".it"
  ELSEIF isfile(songbase$ & ".mid") THEN
    songfile$ = songbase$ & ".mid"
  ELSEIF isfile(songbase$ & ".bam") THEN
    songfile$ = songbase$ & ".bam"
  ELSEIF isfile(game & "." & songnum%) THEN
    songfile$ = game & "." & songnum% ' old-style BAM naming scheme
  END IF

  if songfile$ = "" then exit sub
	loadsong songfile$
END SUB

FUNCTION find_madplay () AS STRING
 STATIC cached AS INTEGER = 0
 STATIC cached_app AS STRING
 IF cached THEN RETURN cached_app
 cached_app = find_helper_app("madplay")
 cached = -1
 RETURN cached_app
END FUNCTION

FUNCTION find_oggenc () AS STRING
 STATIC cached AS INTEGER = 0
 STATIC cached_app AS STRING
 IF cached THEN RETURN cached_app
 cached_app = find_helper_app("oggenc")
 IF cached_app = "" THEN cached_app = find_helper_app("oggenc2")
 cached = -1
 RETURN cached_app
END FUNCTION

FUNCTION find_helper_app (appname AS STRING) AS STRING
'Returns an empty string if the app is not found, or the full path if it is found
#IFDEF __FB_LINUX__
'--Find helper app on Linux
DIM AS INTEGER fh
DIM AS STRING tempfile
DIM AS STRING s
tempfile = tmpdir & "find_helper_app." & INT(RND * 10000) & ".tmp"
'Use the standard util "which" to search the path
SHELL "which " & appname & " > " & tempfile
IF NOT isfile(tempfile) THEN debug "find_helper_app(" & appname & ") failed" : RETURN ""
fh = FREEFILE
OPEN tempfile FOR INPUT AS #fh
LINE INPUT #fh, s
CLOSE #fh
KILL tempfile
s$ = TRIM(s)
RETURN s
#ELSE
'--Find helper app on Windows
DIM AS STRING exedir
exedir = trimfilename$(exename)
'First look in the support subdirectory
IF isfile(exedir & "support\" & appname$ & ".exe") THEN RETURN exedir & "support\" & appname$ & ".exe"
'Then look in the same folder as CUSTOM/GAME
IF isfile(exedir & appname$ & ".exe") THEN RETURN exedir & appname$ & ".exe"
RETURN ""
#ENDIF
END FUNCTION

FUNCTION can_convert_mp3 () AS INTEGER
 IF find_madplay() = "" THEN RETURN 0
 RETURN can_convert_wav()
END FUNCTION

FUNCTION can_convert_wav () AS INTEGER
 IF find_oggenc() = "" THEN RETURN 0
 RETURN -1 
END FUNCTION

SUB mp3_to_ogg (in_file AS STRING, out_file AS STRING, quality AS INTEGER = 4)
 DIM AS STRING tempwav
 tempwav = tmpdir & "temp." & INT(RND * 100000) & ".wav"
 mp3_to_wav(in_file, tempwav)
 wav_to_ogg(tempwav, out_file, quality)
 KILL tempwav
END SUB

SUB mp3_to_wav (in_file AS STRING, out_file AS STRING)
 DIM AS STRING app, args
 IF NOT isfile(in_file) THEN debug "mp3_to_wav: " & in_file & " does not exist" : EXIT SUB
 app = find_madplay()
 IF app = "" THEN debug "mp3_to_wav: failed to find madplay" : EXIT SUB
 args = " -o wave:""" & out_file & """ """ & in_file & """"
 SHELL app & args
 IF NOT isfile(out_file) THEN debug "mp3_to_wav: failed to create " & out_file : EXIT SUB
END SUB

SUB wav_to_ogg (in_file AS STRING, out_file AS STRING, quality AS INTEGER = 4)
 DIM AS STRING app, args
 IF NOT isfile(in_file) THEN debug "wav_to_ogg: " & in_file & " does not exist" : EXIT SUB
 app = find_oggenc()
 IF app = "" THEN debug "wav_to_mp3: failed to find oggenc" : EXIT SUB
 args = " -q " & quality & " -o """ & out_file & """ """ & in_file & """"
 SHELL app & args
 IF NOT isfile(out_file) THEN debug "wav_to_ogg: " & out_file & " does not exist" : EXIT SUB
END SUB

SUB upgrade_message (s AS STRING)
 IF NOT upgrademessages THEN
  upgrademessages = -1
  reset_console 20, 199
  upgrade_message "Auto-Updating obsolete RPG file"
 END IF
 debug "rpgfix:" & s
 show_message(s)
END SUB

SUB reset_console (top AS INTEGER = 0, bottom AS INTEGER = 199, c AS INTEGER = 0)
 WITH console
  .top = top
  .bottom = bottom
  .x = 0
  .y = top
  .c = c
  clearpage vpage, c, .top, .bottom
 END WITH
END SUB

SUB show_message (s AS STRING)
 WITH console
  IF .x > 0 THEN .x = 0 : .y += 8
  append_message s
 END WITH
END SUB

SUB append_message (s AS STRING)
 WITH console
  IF .x > 0 AND LEN(s) * 8 + .x > 320 THEN .x = 0 : .y += 8
  IF .y >= .bottom - 8 THEN
   'scroll page up 2 lines
   copypage vpage, vpage, .top + 16, .top, .bottom
   clearpage vpage, 0, .bottom - 15, .bottom
   .y -= 16
  END IF
  printstr s, .x, .y, vpage
  .x += LEN(s) * 8
  setvispage vpage
 END WITH
END SUB

SUB loadtanim (n, tastuf())
setpicstuf tastuf(), 80, -1
loadset game + ".tap", n, 0
END SUB

SUB savetanim (n, tastuf())
setpicstuf tastuf(), 80, -1
storeset game + ".tap", n, 0
END SUB

SUB animatetilesets (tilesets() AS TilesetData ptr)
 FOR i = 0 TO UBOUND(tilesets)
  FOR j = 0 TO i - 1
   IF tilesets(i) = tilesets(j) THEN CONTINUE FOR, FOR
  NEXT
  cycletile tilesets(i)->anim(), tilesets(i)->tastuf()
 NEXT
END SUB

SUB cycletile (tanim_state() AS TileAnimState, tastuf() AS INTEGER)
 DIM i AS INTEGER
 DIM notstuck AS INTEGER
 FOR i = 0 TO 1
#IFDEF IS_GAME
  IF istag(tastuf(1 + 20 * i), 0) THEN CONTINUE FOR
#ENDIF
  WITH tanim_state(i)
   .skip = large(.skip - 1, 0)
   IF .skip = 0 THEN
    notstuck = 10
    DO
     SELECT CASE tastuf(2 + 20 * i + .pt)
      CASE 0
       IF .pt <> 0 THEN .cycle = 0  'this is done for the tile animation plotscript commands
       .pt = 0
      CASE 1
       .cycle = .cycle - tastuf(11 + 20 * i + .pt) * 16
       .pt = loopvar(.pt, 0, 8, 1)
      CASE 2
       .cycle = .cycle + tastuf(11 + 20 * i + .pt) * 16
       .pt = loopvar(.pt, 0, 8, 1)
      CASE 3
       .cycle = .cycle + tastuf(11 + 20 * i + .pt)
       .pt = loopvar(.pt, 0, 8, 1)
      CASE 4
       .cycle = .cycle - tastuf(11 + 20 * i + .pt)
       .pt = loopvar(.pt, 0, 8, 1)
      CASE 5
       .skip = tastuf(11 + 20 * i + .pt)
       .pt = loopvar(.pt, 0, 8, 1)
#IFDEF IS_GAME
      CASE 6
       IF istag(tastuf(11 + 20 * i + .pt), 0) THEN
        .pt = loopvar(.pt, 0, 8, 1)
       ELSE
        .pt = 0
        .cycle = 0
       END IF
#ENDIF
      CASE ELSE
       .pt = loopvar(.pt, 0, 8, 1)
     END SELECT
     notstuck = large(notstuck - 1, 0)
    LOOP WHILE notstuck AND .skip = 0
   END IF
  END WITH
 NEXT i
END SUB

'Write old password format (backcompat only)
SUB writescatter (s$, lhold, start)
DIM stray(10)

s$ = LEFT$(s$, 20)
lhold = LEN(s$) * 8 - 1
str2array s$, stray(), 0

FOR i = 0 TO lhold
 trueb = readbit(stray(), 0, i)
 DO
  scatb = INT(RND * (16 + (i * 16)))
 LOOP UNTIL readbit(gen(), start - 1, scatb) = trueb
 gen(start + i) = scatb
NEXT i

FOR i = lhold + 1 TO 159
 gen(start + i) = INT(RND * 4444)
NEXT i
END SUB

'Read old password format (needed for backcompat upgrade)
SUB readscatter (s$, lhold, start)
DIM stray(10)
s$ = STRING$(20, "!")

FOR i = 0 TO lhold
 setbit stray(), 0, i, readbit(gen(), start - 1, gen(start + i))
NEXT i

array2str stray(), 0, s$
s$ = LEFT$(s$, INT((lhold + 1) / 8))

END SUB

FUNCTION finddatafile(filename$) as string
'Current dir
IF isfile(filename$) THEN RETURN filename$
'same folder as executable
IF isfile(exepath$ + SLASH + filename$) THEN RETURN exepath$ + SLASH + filename$
#IFDEF __FB_LINUX__
'~/.ohrrpgce/
IF isfile(tmpdir + SLASH + filename$) THEN RETURN tmpdir + SLASH + filename$
#IFDEF DATAFILES
IF isfile(DATAFILES + SLASH + filename$) THEN RETURN DATAFILES + SLASH + filename$
#ENDIF
#ENDIF
RETURN ""
END FUNCTION

SUB updaterecordlength (lumpf$, bindex AS INTEGER)
IF getbinsize(bindex) < curbinsize(bindex) THEN

 oldsize = getbinsize(bindex)
 newsize = curbinsize(bindex)

 upgrade_message trimpath$(lumpf$) & " record size = " & newsize

 IF oldsize > 0 THEN ' Only bother to do this for records of nonzero size

  tempf$ = tmpdir & "resize.tmp"

  flusharray buffer(), newsize / 2, 0

  ff = FREEFILE
  OPEN lumpf$ FOR BINARY AS #ff
  records = LOF(ff) / oldsize
  CLOSE #ff

  copyfile lumpf$, tempf$
  KILL lumpf$

  FOR i = 0 TO records - 1
   setpicstuf buffer(), oldsize, -1
   loadset tempf$, i, 0
   setpicstuf buffer(), newsize, -1
   storeset lumpf$, i, 0
  NEXT

  KILL tempf$
 END IF

 setbinsize bindex, newsize

END IF
END SUB

SUB writepassword (p$)

'-- set password version number (only if needed)
IF gen(genPassVersion) < 256 THEN gen(genPassVersion) = 256

'--pad the password with some silly obfuscating low-ascii chars
FOR i = 1 TO 17 - LEN(p$)
 IF INT(RND * 10) < 5 THEN
  p$ = p$ + CHR$(INT(RND * 30))
 ELSE
  p$ = CHR$(INT(RND * 30)) + p$
 END IF
NEXT i

'--apply a new ascii rotation / weak obfuscation number
gen(genPassRot) = INT(RND * 253) + 1
p$ = rotascii(p$, gen(genPassRot))

'--write the password into GEN
str2array p$, gen(), 14

END SUB

FUNCTION readpassword as string

'--read a 17-byte string from GEN at word offset 7
'--(Note that array2str uses the byte offset not the word offset)
s$ = STRING$(17, 0)
array2str gen(), 14, s$

'--reverse ascii rotation / weak obfuscation
s$ = rotascii(s$, gen(genPassRot) * -1)

'-- discard ascii chars lower than 32
p$ = ""
FOR i = 1 TO 17
 c$ = MID$(s$, i, 1)
 IF ASC(c$) >= 32 THEN p$ = p$ + c$
NEXT i

return p$

END FUNCTION

'======== FIXME: move this up as code gets cleaned up ===========
OPTION EXPLICIT

SUB upgrade (font())
DIM pal16(8)
DIM AS INTEGER i, j, o, p, y
DIM temp$, temp
DIM fh AS INTEGER
DIM pas$, rpas$

upgrademessages = 0

IF NOT fileiswriteable(workingdir + SLASH + "writetest.tmp") THEN
 upgrade_message workingdir & " not writable"
 upgrade_message "Making no attempt to upgrade"
 EXIT SUB
END IF
safekill workingdir + SLASH + "writetest.tmp"

IF gen(genVersion) = 0 THEN
 upgrade_message "Ancient Pre-1999 format (1)"
 gen(genVersion) = 1
 upgrade_message "Flushing New Text Data..."
 DIM box AS TextBox
 FOR o = 0 TO 999
  LoadTextBox box, o
  'Zero out the data members that contained random garbage before 1999
  WITH box
   .money_tag      = 0
   .money          = 0
   .door_tag       = 0
   .door           = 0
   .item_tag       = 0
   .item           = 0
   .choice_enabled = NO
   .no_box         = NO
   .opaque         = NO
   .restore_music  = NO
   .choice(0)      = ""
   .choice_tag(0)  = 0
   .choice(1)      = ""
   .choice_tag(1)  = 0
   .menu_tag       = 0
   .vertical_offset = 0
   .shrink         = 0
   .textcolor      = 0
   .boxstyle       = 0
   .backdrop       = 0
   .music          = 0
   .menu           = 0
  END WITH
  SaveTextBox box, o
 NEXT o
END IF
IF gen(genVersion) = 1 THEN
 upgrade_message "June 18 1999 format (2)"
 gen(genVersion) = 2
 upgrade_message "Updating Door Format..."
 FOR o = 0 TO 19
  IF isfile(game + ".dor") THEN xbload game + ".dor", buffer(), "No doors"
  FOR i = 0 TO 299
   buffer(i) = buffer(o * 300 + i)
  NEXT i
  setpicstuf buffer(), 600, -1
  storeset game + ".dox", o, 0
 NEXT o
 upgrade_message "Enforcing default font"
 getdefaultfont font()
 xbsave game + ".fnt", font(), 2048
 setfont font()
 upgrade_message "rpgfix:Making AniMaptiles Backward Compatible"
 FOR i = 0 TO 39
  buffer(i) = 0
 NEXT i
 FOR i = 0 TO 1
  o = i * 20
  buffer(0 + o) = 112
  buffer(1 + o) = 0
  '--wait 3--
  buffer(2 + o + 0) = 5
  buffer(11 + o + 0) = 3
  '--right 1--
  buffer(2 + o + 1) = 3
  buffer(11 + o + 1) = 1
  '--wait 3--
  buffer(2 + o + 2) = 5
  buffer(11 + o + 2) = 3
  '--left 1--
  buffer(2 + o + 3) = 4
  buffer(11 + o + 3) = 1
 NEXT i
 FOR i = 0 TO 14
  savetanim i, buffer()
 NEXT i
 DIM tx AS INTEGER, ty AS INTEGER
 FOR i = 0 TO gen(genMaxMap)
  upgrade_message " map " & i
  XBLOAD maplumpname$(i, "t"), buffer(), "Map not loaded"
  setmapdata buffer(), buffer(), 0, 0
  FOR tx = 0 TO buffer(0)
   FOR ty = 0 TO buffer(1)
    IF readmapblock(tx, ty, 0) = 158 THEN setmapblock tx, ty, 0, 206
   NEXT ty
  NEXT tx
  xbsave maplumpname$(i, "t"), buffer(), buffer(0) * buffer(1) + 4
 NEXT i
END IF
'---VERSION 3---
IF gen(genVersion) = 2 THEN
 upgrade_message "July 8 1999 format (3)"
 gen(genVersion) = 3
 '-get old-old password
 rpas$ = ""
 FOR i = 1 TO gen(genPW1Length)
  IF gen(4 + i) >= 0 AND gen(4 + i) <= 255 THEN rpas$ = rpas$ + CHR$(loopvar(gen(4 + i), 0, 255, gen(genPW1Offset) * -1))
 NEXT i

 '-SET (obsolete) SCATTERTABLE BASE
 gen(genScatterTableHead) = INT(RND * 15) + 1
 '-WRITE PASSWORD INTO (obsolete) SCATTERTABLE
 gen(genPW2Offset) = INT(RND * 250) + 1
 rpas$ = rotascii(rpas$, gen(genPW2Offset))
 '--write old password (will be upgraded again later in this same routine)
 writescatter rpas$, gen(genPW2Length), 200
 '-REPLACE OLD-OLD PASSWORD
 pas$ = rotascii("ufxx|twi%|fx%rt{ji", -5)
 gen(genPW1Length) = LEN(pas$)
 gen(genPW1Offset) = INT(RND * 250) + 1
 FOR i = 1 TO gen(genPW1Length)
  temp = ASC(MID$(pas$, i, 1))
  gen(4 + i) = loopvar(temp, 0, 255, gen(genPW1Offset))
 NEXT i
 upgrade_message "Put record count defaults in GEN..."
 gen(genMaxHeroPic)   = 40
 gen(genMaxEnemy1Pic) = 149
 gen(genMaxEnemy2Pic) = 79
 gen(genMaxEnemy3Pic) = 29
 gen(genMaxNPCPic)    = 119
 gen(genMaxWeaponPic) = 149
 gen(genMaxAttackPic) = 99
 gen(genMaxTile)      = 14
 gen(genMaxAttack)    = 200
 gen(genMaxHero)      = 59
 gen(genMaxEnemy)     = 500
 gen(genMaxFormation) = 1000
 gen(genMaxPal)       = 99
 gen(genMaxTextbox)   = 999
END IF
'--VERSION 4--
IF gen(genVersion) = 3 THEN
 upgrade_message "Sept 15 2000 format (4)"
 gen(genVersion) = 4
 upgrade_message "Clearing New Attack Bitsets..."
 setpicstuf buffer(), 80, -1
 FOR o = 0 TO gen(genMaxAttack)
  loadoldattackdata buffer(), o
  buffer(18) = 0
  IF readbit(buffer(), 20, 60) THEN buffer(18) = 1
  setbit buffer(), 20, 2, 0
  FOR i = 21 TO 58
   setbit buffer(), 20, i, 0
  NEXT i
  FOR i = 60 TO 63
   setbit buffer(), 20, i, 0
  NEXT i
  saveoldattackdata buffer(), o
 NEXT o
 setbit gen(), 101, 6, 0 'no hide readymeter
 setbit gen(), 101, 7, 0 'no hide health meter
END IF
'--VERSION 5--
IF gen(genVersion) = 4 THEN
 upgrade_message "March 31 2001 format (5)"
 gen(genVersion) = 5
 upgrade_message "Upgrading 16-color Palette Format..."
 setpicstuf pal16(), 16, -1
 xbload game + ".pal", buffer(), "16-color palletes missing from " + game
 KILL game + ".pal"
 '--find last used palette
 DIM last AS INTEGER = 99
 DIM foundpal AS INTEGER = 0
 FOR j = 99 TO 0 STEP -1
  FOR i = 0 TO 7
   IF buffer(j * 8 + i) <> 0 THEN
    last = j
    foundpal = 1
    EXIT FOR
   END IF
  NEXT i
  IF foundpal THEN EXIT FOR
 NEXT j
 upgrade_message "Last used palette is " & last
 '--write header
 pal16(0) = 4444
 pal16(1) = last
 FOR i = 2 TO 7
  pal16(i) = 0
 NEXT i
 storeset game + ".pal", 0, 0
 '--convert palettes
 FOR j = 0 TO last
  FOR i = 0 TO 7
   pal16(i) = buffer(j * 8 + i)
  NEXT i
  storeset game + ".pal", 1 + j, 0
 NEXT j
END IF
'--VERSION 6--
IF gen(genVersion) = 5 THEN
 upgrade_message "Serendipity format (6)"
 'Shop stuff and song name formats changed, MIDI music added
 'Sub version info also added
 'Clear battle formation animation data
 FOR i = 0 TO gen(genMaxFormation)
  setpicstuf buffer(), 80, -1
  loadset game + ".for", i, 0
  buffer(34) = 0
  buffer(35) = 0
  storeset game + ".for", i, 0
 NEXT i
 gen(genVersion) = 6
END IF
'--VERSION 7--
IF gen(genVersion) = 6 THEN
 'This version number change provides backcompat warnings when loading
 'ypsiliform or newer RPG files on xocolatl+ or older versions of game.exe
 'All the work of actually changing the format is done below with fixbits
 'The main purpose for this change is because games that use > 36 NPC defs
 'will crash versions of game.exe that did not support more than that.
 gen(genVersion) = 7
END IF

IF NOT isfile(workingdir + SLASH + "archinym.lmp") THEN
 upgrade_message "generate default archinym.lmp"
 '--create archinym information lump
 fh = FREEFILE
 OPEN workingdir + SLASH + "archinym.lmp" FOR OUTPUT AS #fh
 PRINT #fh, RIGHT$(game, LEN(game) - LEN(workingdir + SLASH))
 PRINT #fh, version$ + "(previous)"
 CLOSE #fh
END IF

IF NOT isfile(game + ".veh") THEN
 upgrade_message "add vehicle data"
 '--make sure vehicle lump is present
 DIM template$ = finddatafile("ohrrpgce.new")
 IF template$ <> "" THEN
  unlumpfile(template$, "ohrrpgce.veh", tmpdir)
  copyfile tmpdir & SLASH & "ohrrpgce.veh", game & ".veh"
  safekill tmpdir & SLASH & "ohrrpgce.veh"
  gen(genMaxVehicle) = 2
 END IF
END IF

'--make sure binsize.bin is full. why are we doing this? as lumps are upgraded
'--and binsize is extended, records in binsize which are meant to default
'--because they don't exist take on the value 0 instead
FOR i = 0 TO sizebinsize
 setbinsize i, getbinsize(i)
NEXT

IF NOT isfile(workingdir + SLASH + "attack.bin") THEN
 upgrade_message "Init extended attack data..."
 setbinsize 0, curbinsize(0)
 FOR i = 0 TO gen(genMaxAttack)
  savenewattackdata buffer(), i
 NEXT i

 '--and while we are at it, clear the old death-string from enemies
 upgrade_message "Re-init recycled enemy data..."
 FOR i = 0 TO gen(genMaxEnemy)
  loadenemydata buffer(), i
  FOR j = 17 TO 52
   buffer(j) = 0
  NEXT j
  saveenemydata buffer(), i
 NEXT i
END IF

IF NOT isfile(workingdir + SLASH + "songdata.bin") THEN
 upgrade_message "Upgrading Song Name format..."
 DIM song$(99)
 fh = FREEFILE
 OPEN game + ".sng" FOR BINARY AS #fh
 temp = LOF(fh)
 CLOSE #fh
 IF temp > 0 THEN
  fh = FREEFILE
  OPEN game + ".sng" FOR INPUT AS #fh
  FOR i = 0 TO 99
   INPUT #fh, song$(i)
  NEXT i
  CLOSE #fh
 END IF

 FOR i = 99 TO 1 STEP -1
  '-- check for midis as well 'cause some people might use a WIP custom or whatnot
  IF song$(i) <> "" OR isfile(game + "." + STR$(i)) OR isfile(workingdir + SLASH + "song" + STR$(i) + ".mid") THEN
   gen(genMaxSong) = i
   EXIT FOR
  END IF
 NEXT

 flusharray buffer(), curbinsize(2) / 2, 0
 setbinsize 2, curbinsize(2)
 setpicstuf buffer(), curbinsize(2), -1
 FOR i = 0 TO gen(genMaxSong)
  writebinstring song$(i), buffer(), 0, 30
  storeset workingdir + SLASH + "songdata.bin", i, 0
 NEXT
 ERASE song$
END IF

IF NOT isfile(workingdir + SLASH + "palettes.bin") THEN
 DIM AS SHORT headsize = 4, recsize = 768
 upgrade_message "Upgrading Master Palette format..."
 loadpalette master(), 0
 fh = FREEFILE
 OPEN workingdir + SLASH + "palettes.bin" FOR BINARY AS #fh
 PUT #fh, , headsize
 PUT #fh, , recsize
 CLOSE #fh
 savepalette master(), 0
END IF

'--check variable record size lumps and reoutput them if records have been extended
'--all of the files below should exist, be non zero length and have non zero record size by this point
updaterecordlength workingdir + SLASH + "attack.bin", binATTACK
updaterecordlength game + ".stf", binSTF
updaterecordlength workingdir + SLASH + "songdata.bin", binSONGDATA
updaterecordlength workingdir + SLASH + "sfxdata.bin", binSFXDATA
updaterecordlength game + ".map", binMAP
updaterecordlength workingdir + SLASH + "menus.bin", binMENUS
updaterecordlength workingdir + SLASH + "menuitem.bin", binMENUITEM
IF NOT isfile(workingdir + SLASH + "menuitem.bin") THEN
 upgrade_message "Creating default menu file..."
 DIM menu_set AS MenuSet
 menu_set.menufile = workingdir + SLASH + "menus.bin"
 menu_set.itemfile = workingdir + SLASH + "menuitem.bin"
 DIM menu AS MenuDef
 create_default_menu menu
 SaveMenuData menu_set, menu, 0
END IF
updaterecordlength workingdir + SLASH + "uicolors.bin", binUICOLORS
updaterecordlength game & ".say", binSAY

'sanity-check gen(genMaxMasterPal)
gen(genMaxMasterPal) = large(gen(genMaxMasterPal), 0)

'--give each palette a default ui color set
DIM ff AS INTEGER = FREEFILE
OPEN workingdir + SLASH + "uicolors.bin" FOR BINARY AS #ff
DIM uirecords AS INTEGER = LOF(ff) \ getbinsize(binUICOLORS)
CLOSE #ff
IF uirecords < gen(genMaxMasterPal) + 1 THEN
 upgrade_message "Adding default UI colors..."
 DIM defaultcols(uiColors)
 OldDefaultUIColors defaultcols()
 FOR i = uirecords TO gen(genMaxMasterPal)
  SaveUIColors defaultcols(), i
 NEXT
END IF

'--update to new (3rd) password format
IF gen(genPassVersion) < 256 THEN
 gen(genPassVersion) = 256
 IF gen(genPW2Length) = -1 THEN
  '--no password, write a blank one
  pas$ = ""
 ELSE
  '--read the old scattertable
  readscatter pas$, gen(genPW2Length), 200
  pas$ = rotascii(pas$, gen(genPW2Offset) * -1)
 END IF
 writepassword pas$
END IF

'Zero out new attack item cost (ammunition) data
IF getfixbit(fixAttackitems) = 0 THEN
  upgrade_message "Zero new ammunition data..."
  setfixbit(fixAttackitems, 1)
  fh = freefile
  OPEN workingdir + SLASH + "attack.bin" FOR BINARY AS #FH
  REDIM dat(curbinsize(0)/2 - 1) AS SHORT
  p = 1
  FOR i = 0 to gen(genMaxAttack)

    GET #fh,p,dat()
    FOR y = 53 TO 59
      dat(y) = 0
    NEXT

    PUT #fh,p,dat()
    p+=curbinsize(0)
  NEXT
  CLOSE #fh
END IF

IF getfixbit(fixWeapPoints) = 0 THEN
 upgrade_message "Reset hero hand points..."
 DO
  setfixbit(fixWeapPoints, 1)
  fh = freefile
  OPEN game + ".dt0" FOR BINARY AS #fh
  REDIM dat(317) AS SHORT
  p = 1
  FOR i = 0 to gen(genMaxHero)
   GET #fh,,dat()
   IF dat(297) <> 0 OR dat(298) <> 0 OR dat(299) <> 0 OR dat(300) <> 0 THEN
    close #fh
    EXIT DO 'they already use hand points, abort!
   END IF
  NEXT
  
  FOR i = 0 to gen(genMaxHero)
   GET #fh,p,dat()
   dat(297) = 24
   dat(299) = -20
   PUT #fh,p,dat()
   p+=318
  NEXT
  close #fh
  EXIT DO
 LOOP
END IF

IF getfixbit(fixStunCancelTarg) = 0 THEN
 upgrade_message "Target disabling old stun attacks..."
 setfixbit(fixStunCancelTarg, 1)
 REDIM dat(40 + dimbinsize(binATTACK)) AS INTEGER
 FOR i = 0 to gen(genMaxAttack)
  loadattackdata dat(), i
  IF dat(18) = 14 THEN '--Target stat is stun register
   IF readbit(dat(), 20, 0) THEN CONTINUE FOR '--cure instead of harm
   IF dat(5) = 5 OR dat(5) = 6 THEN '--set to percentage
    IF dat(11) >= 0 THEN CONTINUE FOR'-- set to >= 100%
   END IF
   'Turn on the disable target attack bit
   setbit dat(), 65, 12, YES
   saveattackdata dat(), i
  END IF
 NEXT
END IF

IF getfixbit(fixDefaultDissolve) = 0 THEN
 upgrade_message "Initializing default enemy fade..."
 setfixbit(fixDefaultDissolve, 1)
 gen(genEnemyDissolve) = 0
END IF

IF getfixbit(fixDefaultDissolveEnemy) = 0 THEN
 upgrade_message "Initializing default enemy fade (per enemy)..."
 setfixbit(fixDefaultDissolveEnemy, 1)
 REDIM dat(160)
 FOR i = 0 to gen(genMaxEnemy)
  loadenemydata dat(), i
  dat(22) = 0
  dat(23) = 0
  saveenemydata dat(), i
 NEXT
END IF

IF getfixbit(fixPushNPCBugCompat) = 0 THEN
 upgrade_message "Enabling Simulate pushable NPC bug bitset..."
 setfixbit(fixPushNPCBugCompat, 1)
 setbit gen(), genBits2, 0, 1 ' For backcompat
END IF

IF getfixbit(fixDefaultMaxItem) = 0 THEN
 upgrade_message "Store max item number in GEN..."
 setfixbit(fixDefaultMaxItem, 1)
 gen(genMaxItem) = 254
END IF

IF getfixbit(fixBlankDoorLinks) = 0 THEN
 upgrade_message "Disable redundant blank door links..."
 setfixbit(fixBlankDoorLinks, 1)
 DIM doorlink_temp(199) AS DoorLink
 DIM found_first AS INTEGER
 FOR i = 0 TO gen(genMaxMap)
  deserDoorLinks maplumpname(i, "d"), doorlink_temp()
  found_first = NO
  FOR j = 0 TO UBOUND(doorlink_temp)
   WITH doorlink_temp(j)
    IF .source = 0 AND .tag1 = 0 AND .tag2 = 0 THEN
     IF found_first = NO THEN
      'Ignore the first "always" link for door 0
      found_first = YES
     ELSE
      IF .dest = 0 AND .dest_map = 0 THEN
       .source = -1 ' Mark redundant all-zero links as unused
      END IF
     END IF
    END IF
   END WITH
  NEXT j
  serDoorLinks maplumpname(i, "d"), doorlink_temp()
 NEXT i
END IF

IF getfixbit(fixShopSounds) = 0 THEN
 upgrade_message "Set default soundeffects..."
 setfixbit(fixShopSounds, 1)
 gen(genItemLearnSFX) = gen(genAcceptSFX)
 gen(genCantLearnSFX) = gen(genCancelSFX)
 gen(genBuySFX) = gen(genAcceptSFX)
 gen(genHireSFX) = gen(genAcceptSFX)
 gen(genSellSFX) = gen(genAcceptSFX)
 gen(genCantBuySFX) = gen(genCancelSFX)
 gen(genCantSellSFX) = gen(genCancelSFX)
END IF

IF getfixbit(fixExtendedNPCs) = 0 THEN
 upgrade_message "Initialize extended NPC data..."
 setfixbit(fixExtendedNPCs, 1)
 DIM npctemp(99) AS NPCType
 FOR i = 0 TO gen(genMaxMap)
  LoadNPCD maplumpname$(i, "n"), npctemp()
  FOR j = 36 TO 99
   ' These are the garbage data left over from somewhere in the late 90's when
   ' James decided to make the .N lumps big enough to hold 100 NPC definitions
   ' even though there was only enough memory available for 36 NPC sprites at a time
   WITH npctemp(j)
    .picture    = 0
    .palette    = -1 'Default palette
    .movetype   = 0
    .speed      = 0
    .textbox    = 0
    .facetype   = 0
    .item       = 0
    .pushtype   = 0
    .activation = 0
    .tag1       = 0
    .tag2       = 0
    .usetag     = 0
    .script     = 0
    .scriptarg  = 0
    .vehicle    = 0
   END WITH
  NEXT j
  SaveNPCD maplumpname$(i, "n"), npctemp()
 NEXT i
END IF

IF getfixbit(fixHeroPortrait) = 0 THEN
 upgrade_message "Initialize hero portrait data..."
 setfixbit(fixHeroPortrait, 1)
 DIM her AS HeroDef
 FOR i = 0 TO gen(genMaxHero)
  loadherodata @her, i
  her.portrait = -1 'Disable
  her.portrait_pal = -1 'Default
  saveherodata @her, i
 NEXT i
END IF

IF getfixbit(fixTextBoxPortrait) = 0 THEN
 upgrade_message "Initialize text box portrait data..."
 setfixbit(fixTextBoxPortrait, 1)
 DIM box AS TextBox
 FOR i = 0 TO gen(genMaxTextBox)
  LoadTextBox box, i
  box.portrait_pal = -1 'Default palette
  SaveTextBox box, i
 NEXT i
END IF

'Save changes to GEN lump (important when exiting to the title screen and loading a SAV)
xbsave game + ".gen", gen(), 1000

'wow! this is quite a big and ugly routine!
END SUB

SUB standardmenu (menu$(), state AS MenuState, x, y, page, edge=NO, hidecursor=NO)
 DIM p AS INTEGER
 WITH state
  p = .pt
  IF hidecursor THEN p = .first - 1
  standardmenu menu$(), .last, .size, p, .top, x, y, page, edge
 END WITH
END SUB

SUB standardmenu (menu$(), size, vis, pt, top, x, y, page, edge=NO)
STATIC tog
DIM i AS INTEGER
DIM col AS INTEGER

tog = tog XOR 1

FOR i = top TO top + vis
 IF i <= size THEN
  IF edge THEN
   col = uilook(uiMenuItem)
   IF pt = i THEN col = uilook(uiSelectedItem + tog)
   edgeprint menu$(i), x + 0, y + (i - top) * 8, col, page
  ELSE
   textcolor uilook(uiMenuItem), 0
   IF pt = i THEN textcolor uilook(uiSelectedItem + tog), 0
   printstr menu$(i), x + 0, y + (i - top) * 8, page
  END IF
 END IF
NEXT i

END SUB

SUB draw_menu (menu AS MenuDef, state AS MenuState, page AS INTEGER)
 DIM i AS INTEGER
 DIM elem AS INTEGER
 DIM cap AS STRING
 DIM col AS INTEGER
 DIM where AS XYPair

 position_menu menu
 
 WITH menu.rect
  IF menu.no_box = NO THEN
   edgeboxstyle .x, .y, .wide, .high, menu.boxstyle, page, menu.translucent
  END IF
  IF menu.no_scrollbar = NO THEN
   draw_scrollbar state, menu, page
  END IF
 END WITH

 state.tog = state.tog XOR 1

 FOR i = 0 TO state.size
  elem = state.top + i
  IF elem <= UBOUND(menu.items) AND elem >= LBOUND(menu.items) THEN
   col = menu.textcolor
   IF col = 0 THEN col = uilook(uiMenuItem)
   IF state.pt = elem AND state.active THEN col = uilook(uiSelectedItem + state.tog)
   WITH menu.items(elem)
    IF .disabled THEN
     col = uilook(uiDisabledItem)
     IF state.pt = elem AND state.active THEN col = uilook(uiSelectedDisabled + state.tog)
    END IF
    IF .exists AND (NOT (.disabled AND .hide_if_disabled)) THEN
     cap = get_menu_item_caption(menu.items(elem), menu)
     position_menu_item menu, cap, i, where
     IF .t = 1 AND .sub_t = 11 THEN ' volume meter
      edgeboxstyle where.x, where.y, fmvol * 3, 10, menu.boxstyle, page, NO, YES
     END IF
     edgeprint cap, where.x, where.y, col, page
    ELSE
     IF menu.edit_mode = YES THEN
      cap = "[NEW MENU ITEM]"
      position_menu_item menu, cap, i, where
      edgeprint cap, where.x, where.y, col, page
     END IF
     EXIT FOR ' Give up after we find the first non-existant item (which will always be sorted to the end)
    END IF
   END WITH
  END IF
 NEXT i
 
END SUB

SUB position_menu_item (menu AS MenuDef, cap AS STRING, i AS INTEGER, BYREF where AS XYPair)
 DIM bord AS INTEGER
 bord = 8 + menu.bordersize
 WITH menu.rect
  SELECT CASE menu.align
   CASE -1
    where.x = .x + bord
   CASE 0
    where.x = .x + .wide / 2 - LEN(cap) * 4
   CASE 1
    where.x = .x + .wide - bord - LEN(cap) * 8
  END SELECT
  where.y = .y + bord + (i * 10)
 END WITH
END SUB

SUB position_menu (menu AS MenuDef)
 DIM i AS INTEGER
 DIM cap AS STRING
 DIM bord AS INTEGER
 bord = 8 + menu.bordersize

 menu.rect.wide = bord * 2
 menu.rect.high = bord * 2

 FOR i = 0 TO UBOUND(menu.items)
  WITH menu.items(i)
   IF .exists THEN
    cap = get_menu_item_caption(menu.items(i), menu)
    menu.rect.wide = large(menu.rect.wide, LEN(cap) * 8 + bord * 2)
    IF .disabled AND .hide_if_disabled THEN CONTINUE FOR 'hidden matter for auto-width but not auto-height
    menu.rect.high = menu.rect.high + 10
   END IF
  END WITH
 NEXT i
 IF menu.edit_mode = YES THEN
  menu.rect.high = menu.rect.high + 10
 END IF
 '--enforce min width
 menu.rect.wide = large(menu.rect.wide, menu.min_chars * 8 + bord * 2)
 '--enforce screen boundaries
 menu.rect.wide = small(menu.rect.wide, 320)
 menu.rect.high = small(menu.rect.high, 200)
 IF menu.maxrows > 0 THEN menu.rect.high = small(menu.rect.high, menu.maxrows * 10 + bord * 2)

 WITH menu
  .rect.x = 160 - anchor_point(.anchor.x, .rect.wide) + menu.offset.x
  .rect.y = 100 - anchor_point(.anchor.y, .rect.high) + menu.offset.y
 END WITH
END SUB

FUNCTION anchor_point(anchor AS INTEGER, size AS INTEGER) AS INTEGER
 SELECT CASE anchor
  CASE -1
   RETURN 0
  CASE 0
   RETURN size \ 2
  CASE 1
   RETURN size
 END SELECT
END FUNCTION

SUB init_menu_state (BYREF state AS MenuState, menu AS MenuDef)
 state.last = count_menu_items(menu) - 1
 state.size = menu.maxrows - 1
 IF state.size = -1 THEN state.size = 20
 state.pt = bound(state.pt, 0, state.last)
 state.top = bound(state.top, 0, large(state.last - state.size, 0))
END SUB

FUNCTION find_empty_menu_item (menu AS MenuDef) as integer
 DIM i AS INTEGER
 FOR i = 0 TO UBOUND(menu.items)
  WITH menu.items(i)
   IF .exists = NO THEN RETURN i
  END WITH
 NEXT i
 RETURN -1
END FUNCTION

FUNCTION count_menu_items (menu AS MenuDef) as integer
 DIM i AS INTEGER
 DIM count AS INTEGER = 0
 FOR i = 0 TO UBOUND(menu.items)
  WITH menu.items(i)
   IF .disabled AND .hide_if_disabled THEN CONTINUE FOR
   IF .exists THEN
    count += 1
   END IF
  END WITH
 NEXT i
 IF menu.edit_mode = YES THEN count += 1
 RETURN count
END FUNCTION

FUNCTION readglobalstring (index, default$, maxlen) as string
DIM fh AS INTEGER = FREEFILE
OPEN game + ".stt" FOR BINARY AS #fh

DIM namelen AS UBYTE
GET #fh, 1 + index * 11, namelen
IF maxlen < namelen THEN namelen = maxlen

DIM result AS STRING
IF index * 11 + namelen + 1 > LOF(fh) THEN
 result = default$
ELSEIF namelen > 0 THEN
 result = STRING$(namelen, 0)
 GET #fh, index * 11 + 2, result
ELSE
 result = ""
END IF

CLOSE #fh

RETURN result
END FUNCTION

FUNCTION get_menu_item_caption (mi AS MenuDefItem, menu AS MenuDef) AS STRING
 DIM cap AS STRING
 DIM menutemp AS MenuDef
 cap = mi.caption
 IF LEN(cap) = 0 THEN
  'No caption, use the default
  SELECT CASE mi.t
   CASE 1 ' special screen
    cap = get_special_menu_caption(mi.sub_t, menu.edit_mode)
   CASE 2 ' another menu
    '--Loading the target menu name here may be inadvisable for performance reasons.
    '--this routine gets called for evert    
    'LoadMenuData menu_set, menutemp, mi.sub_t, YES
    'cap = menutemp.name
    cap = "Menu " & mi.sub_t
   CASE 3 ' Text Box
    cap = "Text Box " & mi.sub_t
   CASE 4 ' Run Script
    cap = scriptname$(mi.sub_t, plottrigger)
  END SELECT
 END IF
 IF menu.edit_mode = YES AND LEN(TRIM(cap)) = 0 THEN cap = "[BLANK]" 
 #IFDEF IS_GAME
  embedtext cap
 #ENDIF
 IF menu.max_chars > 0 THEN ' Crop overlength
  IF menu.align = 1 THEN ' right align
   cap = RIGHT(cap, menu.max_chars)
  ELSE ' left and center align
   cap = LEFT(cap, menu.max_chars)
  END IF
 END IF
 RETURN cap
END FUNCTION

FUNCTION get_special_menu_caption(subtype AS INTEGER, edit_mode AS INTEGER= NO) AS STRING
 DIM cap AS STRING
 SELECT CASE subtype
  CASE 0: cap = readglobalstring$(60, "Items", 10)
  CASE 1: cap = readglobalstring$(61, "Spells", 10)
  CASE 2: cap = readglobalstring$(62, "Status", 10)
  CASE 3: cap = readglobalstring$(63, "Equip", 10)
  CASE 4: cap = readglobalstring$(64, "Order", 10)
  CASE 5: cap = readglobalstring$(65, "Team", 10)
  CASE 6
   IF readbit(gen(), genBits, 5) THEN
    cap = readglobalstring$(65, "Team", 10)
   ELSE
    cap = readglobalstring$(64, "Order", 10)
   END IF
   IF edit_mode = YES THEN cap = cap & " [general bitset]"
  CASE 7,12:
   cap = readglobalstring$(68, "Map", 10)
   IF subtype = 7 AND edit_mode = YES THEN cap = cap & " [if allowed by map]"
  CASE 8,13:
   cap = readglobalstring$(66, "Save", 10)
   IF subtype = 8 AND edit_mode = YES THEN cap = cap & " [if allowed by map]"
  CASE 9: cap = "Load" ' FIXME: Needs a global text string
  CASE 10: cap = readglobalstring$(67, "Quit", 10)
  CASE 11: cap = readglobalstring$(69, "Volume", 10)
 END SELECT
 RETURN cap
END FUNCTION

SUB create_default_menu(menu AS MenuDef)
 DIM i AS INTEGER
 FOR i = 0 TO 3
  WITH menu.items(i)
   .exists = YES
   .t = 1
   .sub_t = i ' item, spell, status, equip
  END WITH
 NEXT i
 WITH menu.items(4)
  .exists = YES
  .t = 1
  .sub_t = 6 ' Order/Status menu
 END WITH
 WITH menu.items(5)
  .exists = YES
  .t = 1
  .sub_t = 7 ' map
  .hide_if_disabled = YES
 END WITH
 WITH menu.items(6)
  .exists = YES
  .t = 1
  .sub_t = 8 ' save
  .hide_if_disabled = YES
 END WITH
 FOR i = 0 TO 1
  WITH menu.items(7 + i)
   .exists = YES
   .t = 1
   .sub_t = 10 + i ' quit, volume
  END WITH
 NEXT i
 menu.translucent = YES
 menu.min_chars = 14
END SUB

FUNCTION read_menu_int (menu AS MenuDef, intoffset AS INTEGER) as integer
 '--This function allows read access to integers in a menu for the plotscripting interface
 '--intoffset is the integer offset, same as appears in the MENUS.BIN lump documentation
 DIM bits(0) AS INTEGER
 WITH menu
  SELECT CASE intoffset
   CASE 12: RETURN .boxstyle
   CASE 13: RETURN .textcolor
   CASE 14: RETURN .maxrows
   CASE 15:
    MenuBitsToArray menu, bits()
    RETURN bits(0)
   CASE 16: RETURN .offset.x
   CASE 17: RETURN .offset.y
   CASE 18: RETURN .anchor.x
   CASE 19: RETURN .anchor.y
   CASE 20: RETURN .align
   CASE 21: RETURN .min_chars
   CASE 22: RETURN .max_chars
   CASE 23: RETURN .bordersize
   CASE ELSE
    debug "read_menu_int: " & intoffset & " is an invalid integer offset"
  END SELECT
 END WITH
 RETURN 0
END FUNCTION

SUB write_menu_int (menu AS MenuDef, intoffset AS INTEGER, n AS INTEGER)
 '--This sub allows write access to integers in a menu for the plotscripting interface
 '--intoffset is the integer offset, same as appears in the MENUS.BIN lump documentation
 DIM bits(0) AS INTEGER
 WITH menu
  SELECT CASE intoffset
   CASE 12: .boxstyle = n
   CASE 13: .textcolor = n
   CASE 14: .maxrows = n
   CASE 15:
    bits(0) = n
    MenuBitsFromArray menu, bits()
   CASE 16: .offset.x = n
   CASE 17: .offset.y = n
   CASE 18: .anchor.x = n
   CASE 19: .anchor.y = n
   CASE 20: .align = n
   CASE 21: .min_chars = n
   CASE 22: .max_chars = n
   CASE 23: .bordersize = n
   CASE ELSE
    debug "write_menu_int: " & intoffset & " is an invalid integer offset"
  END SELECT
 END WITH
END SUB

FUNCTION read_menu_item_int (mi AS MenuDefItem, intoffset AS INTEGER) as integer
 '--This function allows read access to integers in a menu item for the plotscripting interface
 '--intoffset is the integer offset, same as appears in the MENUITEM.BIN lump documentation
 DIM bits(0) AS INTEGER
 WITH mi
  SELECT CASE intoffset
   CASE 22: RETURN .t
   CASE 23: RETURN .sub_t
   CASE 24: RETURN .tag1
   CASE 25: RETURN .tag2
   CASE 26: RETURN .settag
   CASE 27: RETURN .togtag
   CASE 28:
    MenuItemBitsToArray mi, bits()
    RETURN bits(0)
   CASE 29 TO 31: RETURN .extra(intoffset - 29)
   CASE ELSE
    debug "read_menu_item_int: " & intoffset & " is an invalid integer offset"
  END SELECT
 END WITH
 RETURN 0
END FUNCTION

SUB write_menu_item_int (mi AS MenuDefItem, intoffset AS INTEGER, n AS INTEGER)
 '--This sub allows write access to integers in a menu item for the plotscripting interface
 '--intoffset is the integer offset, same as appears in the MENUITEM.BIN lump documentation
 DIM bits(0) AS INTEGER
 WITH mi
  SELECT CASE intoffset
   CASE 22: .t = n
   CASE 23: .sub_t = n
   CASE 24: .tag1 = n
   CASE 25: .tag2 = n
   CASE 26: .settag = n
   CASE 27: .togtag = n
   CASE 28:
    bits(0) = n
    MenuItemBitsFromArray mi, bits()
   CASE 29 TO 31: .extra(intoffset - 29) = n
   CASE ELSE
    debug "write_menu_item_int: " & intoffset & " is an invalid integer offset"
  END SELECT
 END WITH
END SUB

FUNCTION bound_arg(n AS INTEGER, min AS INTEGER, max AS INTEGER, cmd AS STRING, argname AS STRING) AS INTEGER
 IF n < min OR n > max THEN
  debug cmd & ": invalid " & argname & " " & n
  RETURN NO
 END IF
 RETURN YES
END FUNCTION

FUNCTION tag_set_caption(n AS INTEGER, prefix AS STRING="Set Tag") AS STRING
 RETURN tag_condition_caption(n, prefix, "N/A", "Unchangeable", "Unchangeable")
END FUNCTION

FUNCTION tag_condition_caption(n AS INTEGER, prefix AS STRING="Tag", zerocap AS STRING="", onecap AS STRING="", negonecap AS STRING="") AS STRING
 DIM s AS STRING
 DIM cap AS STRING
 s = prefix
 IF LEN(s) > 0 THEN s = s & " "
 s = s & ABS(n) & "=" & onoroff(n)
 cap = load_tag_name(n)
 IF n = 0 AND LEN(zerocap) > 0 THEN cap = zerocap
 IF n = 1 AND LEN(onecap) > 0 THEN cap = onecap
 IF n = -1 AND LEN(negonecap) > 0 THEN cap = negonecap
 cap = TRIM(cap)
 IF LEN(cap) > 0 THEN s = s & " (" & cap & ")"
 RETURN s
END FUNCTION

FUNCTION onoroff (n AS INTEGER) AS STRING
 IF n >= 0 THEN RETURN "ON"
 RETURN "OFF"
END FUNCTION

FUNCTION yesorno (n AS INTEGER, yes_cap AS STRING="YES", no_cap AS STRING="NO") AS STRING
 IF n THEN RETURN yes_cap
 RETURN no_cap
END FUNCTION

FUNCTION load_tag_name (index AS INTEGER) AS STRING
 IF index = 0 THEN RETURN ""
 IF index = 1 THEN RETURN "Never"
 IF index = -1 THEN RETURN "Always"
 DIM buf(20)
 setpicstuf buf(), 42, -1
 loadset game + ".tmn", ABS(index), 0
 RETURN readbadbinstring$(buf(), 0, 20)
END FUNCTION

SUB save_tag_name (tagname AS STRING, index AS INTEGER)
 DIM buf(20)
 setpicstuf buf(), 42, -1
 writebadbinstring tagname$, buf(), 0, 20
 storeset game + ".tmn", index, 0
END SUB

SUB dump_master_palette_as_hex (master_palette() AS RGBColor)
 DIM i AS INTEGER
 DIM hexstring AS STRING = " DIM colorcodes(255) AS INTEGER = {"
 FOR i = 0 to 255
  hexstring = hexstring & "&h" & hex(master_palette(i).col, 6)
  IF i <> 255 THEN hexstring = hexstring & ","
  IF LEN(hexstring) > 88 THEN
   hexstring = hexstring & "_"
   debug hexstring
   hexstring = "        "
  END IF
 NEXT i
 hexstring = hexstring & "}"
 debug hexstring
END SUB

SUB load_default_master_palette (master_palette() AS RGBColor)
 'To regenerate this if the default master palette changes, use dump_master_palette_as_hex
 DIM colorcodes(255) AS INTEGER = {&h000000,&h232222,&h312F2B,&h3F3B34,&h4C483C,&h5D5747,_
        &h716A54,&h857C61,&h9A8F6D,&hAFA277,&hC4B581,&hD8C68B,&hEAD694,&hFDBC3B,&hFC9D47,_
        &hFA7D53,&h0D0F0D,&h121111,&h2A2426,&h41323B,&h583D51,&h6F456D,&h81577B,&h916684,_
        &hA2778D,&hB28997,&hC39CA3,&hD3B0B0,&hE3C9C4,&hEEDCD6,&hF4E7E4,&hFAF3F3,&h1F221E,_
        &h0C0E1C,&h1C203E,&h2A305E,&h39407D,&h495198,&h5962B1,&h6975C4,&h8084D0,&h9793DD,_
        &hAEA2EA,&hC1B8F1,&hD3CEF7,&hE2DFFC,&hECEBFF,&hF6F6FF,&h2F342E,&h15091C,&h2B1239,_
        &h411B56,&h562473,&h6C2D90,&h8236AC,&h9740C9,&hAD49E6,&hC154FF,&hCB68FF,&hD57CFF,_
        &hDF90FF,&hE9A4FF,&hF2B8FF,&hFCCCFF,&h40463E,&h060E27,&h0E2059,&h153289,&h1B45AE,_
        &h1E5DC0,&h2179D3,&h2294DD,&h24B0E6,&h25CEF0,&h27EEF9,&h3DFFFA,&h75FFFB,&hA3FFFC,_
        &hC4FFFD,&hE4FFFE,&h4F595A,&h170000,&h340000,&h500000,&h6B0000,&h870000,&hA30000,_
        &hBF0000,&hDC2D2D,&hFA5F5F,&hFF7F7F,&hFF9D9D,&hFFB9B9,&hFFD0D0,&hFFE1E1,&hFFF1F1,_
        &h5F6B75,&h140F00,&h2D2200,&h463400,&h5E4600,&h765800,&h8E6B00,&hA67D00,&hBF8F00,_
        &hD7A100,&hEFB300,&hFFC70D,&hFFDD30,&hFFEF4D,&hFFFC62,&hFFFFB4,&h707D8F,&h140614,_
        &h2E0F2E,&h471747,&h5F1F5F,&h782878,&h913091,&hAB3CA2,&hC74AB0,&hE358BE,&hFF67CC,_
        &hFF8AD8,&hFFACE3,&hFFC7EC,&hFFDBF3,&hFFEFF9,&h898CA0,&h1B0904,&h3D150A,&h5A2419,_
        &h76352C,&h91463F,&hAC5752,&hBF6666,&hD17579,&hE4848C,&hF693A0,&hFFA8B5,&hFFC2CB,_
        &hFFD7DD,&hFFE7EA,&hFFF6F7,&hA19CB0,&h080B0E,&h121921,&h1B2733,&h363B45,&h4D484B,_
        &h61504B,&h75584B,&h89614B,&h9E694B,&hB1774F,&hC38C56,&hD3A560,&hDFC171,&hE8D67D,_
        &hF1EA89,&hBAABC1,&h091207,&h162911,&h20411C,&h285B2A,&h45692A,&h647729,&h7B8639,_
        &h90964E,&hA2A860,&hAFBE6C,&hBDD379,&hC9E784,&hD3F88E,&hDFFFA7,&hF1FFD8,&hCCBDD0,_
        &h0F0F0F,&h232221,&h363331,&h494D3C,&h436443,&h4F7A54,&h5A8F67,&h64A57D,&h6DBA96,_
        &h76CFB1,&h7DE5D0,&h84F9F1,&hA4FFFB,&hC4FFFC,&hE5FFFE,&hDAD0DD,&h161010,&h322524,_
        &h4D3836,&h6A4C44,&h836052,&h9A7360,&hAF846C,&hC29478,&hD4A484,&hE1B494,&hEDC2A2,_
        &hF6D2B6,&hF9E2CF,&hFBECE1,&hFDF7F2,&hE6E0E8,&h0B230B,&h0E300E,&h123D12,&h154C15,_
        &h196119,&h1E771E,&h228B22,&h379F37,&h3CB23A,&h44C53D,&h65D95D,&h6BEB61,&h98FA90,_
        &hCCFFCA,&hE5FFE9,&hEFEBF0,&h180B09,&h371916,&h52281E,&h6B3824,&h84492A,&h9E5A30,_
        &hB56E24,&hCD8316,&hDF9814,&hE6AD33,&hECC253,&hF3D773,&hF7E69A,&hFAEFBE,&hFCF7E2,_
        &hFFFFFF,&h000000,&h001D48,&h002C6F,&h003B95,&h004BBC,&h005AE2,&h076DFF,&h258BFF,_
        &h43A9FF,&h61C7FF,&h85D6FF,&hA8E2FF,&hC5EBFF,&hDAF2FF,&hEEF9FF}
 DIM i AS INTEGER
 FOR i = 0 TO 255
  master_palette(i).col = colorcodes(i)
 NEXT i
END SUB

FUNCTION enter_or_space () AS INTEGER
 RETURN keyval(28) > 1 OR keyval(57) > 1
END FUNCTION

FUNCTION append_menu_item(BYREF menu AS MenuDef, caption AS STRING, t AS INTEGER=0, sub_t AS INTEGER=0) as integer
 DIM i AS INTEGER
 FOR i = 0 TO UBOUND(menu.items)
  WITH menu.items(i)
   IF .exists = NO THEN
    .exists = YES
    .caption = caption
    .t = t
    .sub_t = sub_t
    RETURN i
   END IF
  END WITH
 NEXT i
 debug "No room to append """ & caption & """ to menu " & menu.name 
 RETURN -1 'failure
END FUNCTION

SUB write_npc_int (npcdata AS NPCType, intoffset AS INTEGER, n AS INTEGER)
 '--intoffset is the integer offset, same as appears in the .N lump documentation
 WITH npcdata
  SELECT CASE intoffset
   CASE 0: .picture = n
   CASE 1: .palette = n
   CASE 2: .movetype = n
   CASE 3: .speed = n
   CASE 4: .textbox = n
   CASE 5: .facetype = n
   CASE 6: .item = n
   CASE 7: .pushtype = n
   CASE 8: .activation = n
   CASE 9: .tag1 = n
   CASE 10: .tag2 = n
   CASE 11: .usetag = n
   CASE 12: .script = n
   CASE 13: .scriptarg = n
   CASE 14: .vehicle = n
   CASE ELSE
    debug "write_npc_int: " & intoffset & " is an invalid integer offset"
  END SELECT
 END WITH
END SUB

FUNCTION read_npc_int (npcdata AS NPCType, intoffset AS INTEGER) AS INTEGER
 '--intoffset is the integer offset, same as appears in the .N lump documentation
 WITH npcdata
  SELECT CASE intoffset
   CASE 0: RETURN .picture
   CASE 1: RETURN .palette
   CASE 2: RETURN .movetype
   CASE 3: RETURN .speed
   CASE 4: RETURN .textbox
   CASE 5: RETURN .facetype
   CASE 6: RETURN .item
   CASE 7: RETURN .pushtype
   CASE 8: RETURN .activation
   CASE 9: RETURN .tag1
   CASE 10: RETURN .tag2
   CASE 11: RETURN .usetag
   CASE 12: RETURN .script
   CASE 13: RETURN .scriptarg
   CASE 14: RETURN .vehicle
   CASE ELSE
    debug "read_npc_int: " & intoffset & " is an invalid integer offset"
  END SELECT
 END WITH
 RETURN 0
END FUNCTION

SUB unloadtilesetdata (BYREF tileset AS TilesetData ptr)
 IF tileset <> NULL THEN
  tileset->refcount -= 1
  IF tileset->refcount = 0 THEN
   'debug "unloading tileset " & tileset->num
   unloadtileset tileset->spr
   Deallocate(tileset)
  END IF
  tileset = NULL
 END IF
END SUB

SUB maptilesetsprint (tilesets() AS TilesetData ptr)
 DIM i AS INTEGER
 FOR i = 0 TO UBOUND(tilesets)
  IF tilesets(i) = NULL THEN
   debug i & ": NULL"
  ELSE 
   debug i & ": " & tilesets(i)->num & "  ref: " & tilesets(i)->refcount
  END IF
 NEXT
END SUB

SUB loadtilesetdata (BYREF tileset AS TilesetData ptr, BYVAL tilesetnum AS INTEGER)
'tileset may already be loaded
'note that tile animation data is NOT reset if the old tileset was the same	

 IF tileset <> NULL THEN
  IF tileset->num = tilesetnum THEN EXIT SUB
  unloadtilesetdata tileset
 END IF

 tileset = Callocate(sizeof(TilesetData))

 WITH *tileset
  .num = tilesetnum
  .refcount = 1
  'debug "loading tileset " & tilesetnum

  dim page = allocatepage
  loadpage game + ".til", tilesetnum, page
  loadtileset .spr, page
  freepage page
  loadtanim tilesetnum, .tastuf()
  DIM i AS INTEGER
  FOR i = 0 TO 1
   WITH .anim(i)
    .cycle = 0
    .pt = 0
    .skip = 0
   END WITH
  NEXT
 END WITH
END SUB

SUB loadtilesetdata (tilesets() AS TilesetData ptr, BYVAL layer AS INTEGER, BYVAL tilesetnum AS INTEGER)
 DIM i AS INTEGER
 FOR i = 0 TO UBOUND(tilesets)
  IF i <> layer AND tilesets(i) <> NULL THEN
   IF tilesets(i)->num = tilesetnum THEN

    unloadtilesetdata tilesets(layer)
    tilesets(layer) = tilesets(i)
    tilesets(layer)->refcount += 1
    EXIT SUB
   END IF
  END IF
 NEXT

 loadtilesetdata tilesets(layer), tilesetnum
END SUB

SUB loadmaptilesets (tilesets() AS TilesetData ptr, gmap() AS INTEGER, BYVAL resetanimations AS INTEGER = YES)
'tilesets() may contain already loaded tilesets. In this case, we can reuse them
'but this mechanism is crude, tilesets might be freed then reloaded (code to fix commented)

' REDIM tilesets(0 TO 2 * UBOUND(tilesets) + 1)
' FOR i = 0 TO 2
'  IF tilesets(i) <> NULL THEN tilesets(UBOUND(tilesets) - i) = tilesets(i): tilesets(i)->refcount += 1
' NEXT

 DIM AS INTEGER i, j
 DIM tileset AS INTEGER

 FOR i = 0 TO 2
  IF gmap(22 + i) <> 0 THEN
   tileset = gmap(22 + i) - 1
  ELSE
   tileset = gmap(0)
  END IF

  loadtilesetdata tilesets(), i, tileset
 NEXT

 FOR i = 0 TO 2
'  unloadtilesetdata tilesets(UBOUND(tilesets - i))
  IF resetanimations THEN
   FOR j = 0 TO 1
    WITH tilesets(i)->anim(j)
     .cycle = 0
     .pt = 0
     .skip = 0
    END WITH
   NEXT
  END IF
 NEXT
' REDIM PRESERVE tilesets(UBOUND(tilesets) \ 2)
END SUB

SUB unloadmaptilesets (tilesets() AS TilesetData ptr)
 DIM i AS INTEGER
 FOR i = 0 TO UBOUND(tilesets)
  unloadtilesetdata tilesets(i)
 NEXT
END SUB

FUNCTION xreadbit (bitarray() AS INTEGER, bitoffset AS INTEGER, intoffset AS INTEGER=0) AS INTEGER
 'This is a wrapper for readbit that returns YES/NO and accepts a default arg of zero for the integer offset
 RETURN readbit(bitarray(), intoffset, bitoffset) <> 0 
END FUNCTION

FUNCTION getheroname (hero_id AS INTEGER) AS STRING
 DIM her AS HeroDef
 IF hero_id >= 0 THEN
  loadherodata @her, hero_id
  RETURN her.name
 END IF
 RETURN ""
END FUNCTION

FUNCTION getmenuname(record AS INTEGER) AS STRING
 DIM menu_set AS MenuSet
 menu_set.menufile = workingdir + SLASH + "menus.bin"
 menu_set.itemfile = workingdir + SLASH + "menuitem.bin"
 DIM menu AS MenuDef
 LoadMenuData menu_set, menu, record, NO
 RETURN menu.name
END FUNCTION

SUB draw_scrollbar(state AS MenuState, menu AS MenuDef, page AS INTEGER)
 draw_scrollbar state, menu.rect, count_menu_items(menu) - 1, menu.boxstyle, page
END SUB

SUB draw_scrollbar(state AS MenuState, rect AS RectType, boxstyle AS INTEGER=0, page AS INTEGER)
 DIM count AS INTEGER = state.last - state.first
 draw_scrollbar state, rect, count, boxstyle, page
END SUB

SUB draw_scrollbar(state AS MenuState, rect AS RectType, count AS INTEGER, boxstyle AS INTEGER=0, page AS INTEGER)
 IF (state.top > state.first OR (count-1) >= state.size) AND count > 0 THEN
  IF count > 0 THEN
   DIM sbar AS RectType
   DIM slider AS RectType
   sbar.x = rect.x + rect.wide - 6
   sbar.y = rect.y + 2
   sbar.wide = 4
   sbar.high = rect.high - 4
   WITH sbar
    slider.y = ((.high * 200) / (count) * (state.top - state.first)) / 200
    slider.high = ((.high * 200) / (count) * state.size) / 200
    rectangle .x, .y, .wide, .high, uilook(uiBackground), page
    rectangle .x, .y + slider.y, .wide, slider.high, uilook(uiTextBox + boxstyle * 2 + 1), page
   END WITH
  END IF
 END IF
END SUB

SUB draw_fullscreen_scrollbar(state AS MenuState, boxstyle AS INTEGER=0, page AS INTEGER)
 DIM rect AS RectType
 rect.wide = 320
 rect.high = 200
 draw_scrollbar state, rect, boxstyle, page
END SUB

FUNCTION range (number AS INTEGER, percent AS INTEGER) AS INTEGER
 DIM a AS INTEGER
 a = (number / 100) * percent
 RETURN number + INT(RND * (a * 2)) - a
END FUNCTION

FUNCTION rpad (s AS STRING, pad_char AS STRING, size AS INTEGER) AS STRING
 DIM result AS STRING
 result = LEFT(s, size)
 WHILE LEN(result) < size: result = result & pad_char: WEND
 RETURN result
END FUNCTION

SUB notification (show_msg AS STRING)
 DIM msg AS STRING = show_msg
 DIM ypos AS INTEGER
 DIM high AS INTEGER = 16

 'Find the height
 DO WHILE LEN(msg) > 38
  msg = MID(msg, 39)
  high += 8
 LOOP

 msg = show_msg
 
 edgeprint msg, 8, ypos, uilook(uiText), vpage

 edgeboxstyle 4, 4, 312, high, 2, vpage

 ypos = 8
 DO WHILE LEN(msg) > 38
  edgeprint LEFT(msg, 38), 8, ypos, uilook(uiText), vpage
  msg = MID(msg, 39)
  ypos += 8
 LOOP
 edgeprint msg, 8, ypos, uilook(uiText), vpage

 setvispage vpage 'refresh
 waitforanykey
END SUB

FUNCTION get_text_box_height(BYREF box AS TextBox) AS INTEGER
 IF box.shrink >= 0 THEN RETURN 88 - box.shrink * 4
 FOR i AS INTEGER = UBOUND(box.text) TO 0 STEP -1
  IF LEN(TRIM(box.text(i))) > 0 THEN
   DIM vsize AS INTEGER = 20 + i * 10
   IF vsize < 32 AND vsize > 24 THEN RETURN 32
   IF vsize <= 24 THEN RETURN 16
   RETURN vsize
  END IF
 NEXT i
 RETURN 88
END FUNCTION

FUNCTION last_inv_slot() AS INTEGER
 IF gen(genMaxInventory) = 0 THEN RETURN inventoryMax
 RETURN large(INT((gen(genMaxInventory) + 1) / 3), 1) * 3 -1
END FUNCTION

SUB setup_sprite_sizes ()
 'Populates the global sprite_sizes
 WITH sprite_sizes(0)
  .name = "Hero"
  .size.x = 32
  .size.y = 40
  .frames = 8
  .genmax = genMaxHeroPic
 END WITH
 WITH sprite_sizes(1)
  .name = "Small Enemy"
  .size.x = 34
  .size.y = 34
  .frames = 1
  .genmax = genMaxEnemy1Pic
 END WITH
 WITH sprite_sizes(2)
  .name = "Medium Enemy"
  .size.x = 50
  .size.y = 50
  .frames = 1
  .genmax = genMaxEnemy2Pic
 END WITH
 WITH sprite_sizes(3)
  .name = "Large Enemy"
  .size.x = 80
  .size.y = 80
  .frames = 1
  .genmax = genMaxEnemy3Pic
 END WITH
 WITH sprite_sizes(4)
  .name = "Walkabout"
  .size.x = 20
  .size.y = 20
  .frames = 8
  .genmax = genMaxNPCPic
 END WITH
 WITH sprite_sizes(5)
  .name = "Weapon"
  .size.x = 24
  .size.y = 24
  .frames = 2
  .genmax = genMaxWeaponPic
 END WITH
 WITH sprite_sizes(6)
  .name = "Attack"
  .size.x = 50
  .size.y = 50
  .frames = 3
  .genmax = genMaxAttackPic
 END WITH
 WITH sprite_sizes(7)
  .name = "Box Border"
  .size.x = 16
  .size.y = 16
  .frames = 16
  .genmax = genMaxBoxBorder
 END WITH
 WITH sprite_sizes(8)
  .name = "Portrait"
  .size.x = 50
  .size.y = 50
  .frames = 1
  .genmax = genMaxPortrait
 END WITH
END SUB

FUNCTION standard_sprite_load (BYVAL spritetype AS INTEGER, BYVAL index AS INTEGER) AS Frame PTR
 WITH sprite_sizes(spritetype)
  RETURN sprite_load(game & ".pt" & spritetype, index, .frames, .size.x, .size.y)
 END WITH
END FUNCTION

FUNCTION standard_pal16_load (BYVAL palnum AS INTEGER = -1, BYVAL spritetype AS INTEGER, BYVAL index AS INTEGER) AS Palette16 PTR
 RETURN palette16_load(game & ".pal", palnum, spritetype, index)
END FUNCTION

SUB load_sprite_and_pal (BYREF img AS GraphicPair, BYVAL spritetype, BYVAL index AS INTEGER, BYVAL palnum AS INTEGER=-1)
 unload_sprite_and_pal img
 img.sprite = standard_sprite_load(spritetype, index)
 img.pal    = standard_pal16_load(palnum, spritetype, index)
END SUB

SUB unload_sprite_and_pal (BYREF img AS GraphicPair)
 IF img.sprite THEN sprite_unload @img.sprite
 IF img.pal    THEN palette16_unload @img.pal
END SUB

FUNCTION str2int (stri as string, default as integer=0) as integer
 'Use this in contrast to QuickBasic's VALINT
 DIM n AS INTEGER = 0
 DIM s AS STRING = LTRIM(stri)
 IF s = "" THEN RETURN default
 DIM sign AS INTEGER = 1

 DIM ch AS STRING
 DIM c AS INTEGER
 FOR i AS INTEGER = 1 TO LEN(s)
  ch = MID(s, i, 1)
  IF ch = "-" AND i = 1 THEN
   sign = -1
   CONTINUE FOR
  END IF
  c = ASC(ch) - 48
  IF c >= 0 AND c <= 9 THEN
   n = n * 10 + (c * sign)
  ELSE
   RETURN default
  END IF
 NEXT i

 RETURN n

END FUNCTION
