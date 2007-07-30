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
#include "uiconst.bi"
#include "common.bi"

#include "music.bi"
#include "loading.bi

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

SUB getui (colarray(), palnum)
'load ui colors from data lump
'(lump not finalised, just set defaults for now)

DIM uidef(uiColors) = {0,7,8,14,15,6,7,1,2,18,21,35,37,15,240,10,14,240, _
        18,28,34,44,50,60,66,76,82,92,98,108,114,124,130,140, _
        146,156,162,172,178,188,194,204,210,220,226,236,242,252}

FOR i=0 TO uiColors
 colarray(i) = uidef(i)
NEXT
END SUB

SUB safekill (f$)
IF isfile(f$) THEN KILL f$
END SUB

FUNCTION usemenu (pt, top, first, last, size)

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

FUNCTION soundfile$ (sfxnum%)
	DIM as string sfxbase

	sfxbase = workingdir$ & SLASH & "sfx" & sfxnum%
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

FUNCTION getfixbit(bitnum AS INTEGER) AS INTEGER
 DIM f$
 f$ = workingdir$ + SLASH + "fixbits.bin"
 IF NOT isfile(f$) THEN RETURN 0
 DIM bits(1) as INTEGER
 setpicstuf bits(), 2, -1
 loadset f$, 0, 0
 RETURN readbit(bits(), 0, bitnum)
END FUNCTION

SUB setfixbit(bitnum AS INTEGER, bitval AS INTEGER)
 DIM f$
 f$ = workingdir$ + SLASH + "fixbits.bin"
 DIM bits(1) as INTEGER
 setpicstuf bits(), 2, -1
 IF isfile(f$) THEN
  loadset f$, 0, 0
 END IF
 setbit bits(), 0, bitnum, bitval
 storeset f$, 0, 0
END SUB

FUNCTION aquiretempdir$ ()
#IFNDEF __FB_LINUX__
'Windows only behavior
t$ = environ$("TEMP")
IF NOT isdir(t$) THEN t$ = environ("TMP")
IF NOT isdir(t$) THEN
 '--fall back to working dir if all else fails
 t$ = exepath$
END IF
IF RIGHT$(t$, 1) <> SLASH THEN t$ = t$ + SLASH
RETURN t$
#ELSE
'Linux only behavior
#IFDEF IS_CUSTOM
RETURN ""
#ELSE
h$ = environ$("HOME")
o$ = h$ + "/.ohrrpgce"
IF NOT isdir(o$) THEN makedir(o$)
d$ = DATE
t$ = TIME
tmp$ = o$ + "/" + MID$(d$,7,4) + MID$(d$,1,2) + MID$(d$,4,2) + MID$(t$,1,2) + MID$(t$,4,2) + MID$(t$,7,2) + "." + STR$(INT(RND * 1000)) + ".tmp"
tmp$ = tmp$ + "/"
RETURN tmp$
#ENDIF
#ENDIF
END FUNCTION

SUB copylump(package$, lump$, dest$)
IF isdir(package$) THEN
 'unlumped folder
 copyfile package$ + SLASH + lump$, dest$ + SLASH + lump$, buffer()
ELSE
 'lumpfile
 unlumpfile package$, lump$, dest$ + SLASH
END IF
END SUB

SUB centerbox (x, y, w, h, c, p)
tbc = uiTextBox + (2 * (c - 1))
rectangle x - INT(w * .5), y - INT(h * .5), w, h, uilook(tbc), p
rectangle x - INT(w * .5), y - INT(h * .5), w, 1, uilook(tbc + 1), p
rectangle x - INT(w * .5), y + (h - INT(h * .5)), w, 1, uilook(tbc + 1), p
rectangle x - INT(w * .5), y - INT(h * .5), 1, h, uilook(tbc + 1), p
rectangle x + (w - INT(w * .5)), y - INT(h * .5), 1, h + 1, uilook(tbc + 1), p
END SUB

SUB centerfuz (x, y, w, h, c, p)
tbc = uiTextBox + (2 * (c - 1))
fuzzyrect x - INT(w * .5), y - INT(h * .5), w, h, uilook(tbc), p
rectangle x - INT(w * .5), y - INT(h * .5), w, 1, uilook(tbc + 1), p
rectangle x - INT(w * .5), y + (h - INT(h * .5)), w, 1, uilook(tbc + 1), p
rectangle x - INT(w * .5), y - INT(h * .5), 1, h, uilook(tbc + 1), p
rectangle x + (w - INT(w * .5)), y - INT(h * .5), 1, h + 1, uilook(tbc + 1), p
END SUB

FUNCTION readbinstring$ (array(), offset, maxlen)

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

readbinstring$ = result$
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

FUNCTION readbadbinstring$ (array(), offset, maxlen, skipword)
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

readbadbinstring$ = result$
END FUNCTION

SUB writebadbinstring (savestr$, array(), offset, maxlen, skipword)

'--write current length
array(offset) = LEN(savestr$)

FOR i = 1 TO LEN(savestr$)
 array(offset + skipword + i) = savestr$[i - 1]
NEXT i

FOR i = LEN(savestr$) + 1 TO maxlen
 array(offset + skipword + i) = 0
NEXT i

END SUB

FUNCTION read32bitstring$ (array(), offset)
result$ = ""
word = array(offset + 1)
FOR i = 1 TO array(offset)
 result$ += CHR$(word AND 255)
 IF i MOD 4 = 0 THEN word = array(offset + i \ 4 + 1) ELSE word = word SHR 8
NEXT
read32bitstring$ = result$
END FUNCTION

FUNCTION read32bitstring$ (stringptr as integer ptr)
result$ = ""
word = stringptr[1]
FOR i = 1 TO stringptr[0]
 result$ += CHR$(word AND 255)
 IF i MOD 4 = 0 THEN word = stringptr[i \ 4 + 1] ELSE word = word SHR 8
NEXT
read32bitstring$ = result$
END FUNCTION

FUNCTION readbadgenericname$ (index, filename$, recsize, offset, size, skip)

'--clobbers buffer!

result$ = ""

IF index >= 0 THEN
 setpicstuf buffer(), recsize, -1
 loadset filename$, index, 0
 result$ = readbadbinstring$(buffer(), offset, size, skip)
END IF

readbadgenericname = result$

END FUNCTION

FUNCTION isbit (bb() as INTEGER, BYVAL w as INTEGER, BYVAL b as INTEGER) as INTEGER
 IF readbit (bb(), w, b) THEN
  RETURN -1
 ELSE
  RETURN 0
 END IF
END FUNCTION

FUNCTION scriptname$ (num, trigger = 0)
#ifdef IS_GAME
 'remember script names!
 STATIC cachenum, cacheids(24), cachenames$(24), gamename$
 IF game$ <> gamename$ THEN
  gamename$ = game$
  cachenum = 0
 END IF
 FOR i = 0 TO cachenum - 1
  IF cacheids(i) = num THEN RETURN cachenames$(i)
 NEXT
#endif

DIM buf(19)
IF num >= 16384 AND trigger > 0 THEN
 IF loadrecord (buf(), workingdir$ + SLASH + "lookup" + STR$(trigger) + ".bin", 20, num - 16384) THEN
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
 OPEN workingdir$ + SLASH + "plotscr.lst" FOR BINARY AS #fh
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
scriptname$ = a$
#ifdef IS_GAME
 IF cachenum = 25 THEN cachenum = 0
 cacheids(cachenum) = num
 cachenames$(cachenum) = a$
 cachenum += 1
#endif
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

FUNCTION getdefaultpal(fileset, index)
 DIM v AS SHORT
 f$ = workingdir$ & SLASH & "defpal" & fileset & ".bin"
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
 f$ = workingdir$ & SLASH & "defpal" & fileset & ".bin"
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
 f$ = workingdir$ & SLASH & "defpal" & fileset & ".bin"
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
     FOR j = 0 to 35
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
  REDIM buf(100)
  FOR i = 0 TO sets
   FOR j = 0 TO 254
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

SUB flusharray (array(), size, value)
FOR i = 0 TO size
 array(i) = value
NEXT i
END SUB

SUB loadherodata (hero as herodef ptr, index)
deserherodef game$ & ".dt0", hero, index
END SUB

SUB saveherodata (hero as herodef ptr, index)
serherodef game$ & ".dt0", hero, index
END SUB

SUB loadenemydata (array(), index, altfile = 0)
IF altfile THEN
 filename$ = workingdir$ + SLASH + "dt1.tmp"
ELSE
 filename$ = game$ & ".dt1"
END IF
loadrecord array(), filename$, 160, index
END SUB

SUB saveenemydata (array(), index, altfile = 0)
IF altfile THEN
 filename$ = workingdir$ + SLASH + "dt1.tmp"
ELSE
 filename$ = game$ & ".dt1"
END IF
storerecord array(), filename$, 160, index
END SUB

SUB loaditemdata (array(), index)
loadrecord array(), game$ & ".itm", 100, index
END SUB

SUB saveitemdata (array(), index)
storerecord array(), game$ & ".itm", 100, index
END SUB

SUB loadoldattackdata (array(), index)
loadrecord array(), game$ & ".dt6", 40, index
END SUB

SUB saveoldattackdata (array(), index)
storerecord array(), game$ & ".dt6", 40, index
END SUB

SUB loadnewattackdata (array(), index)
size = getbinsize(binATTACK) \ 2
IF size > 0 THEN
 loadrecord array(), workingdir$ + SLASH + "attack.bin", size, index
END IF
END SUB

SUB savenewattackdata (array(), index)
size = curbinsize(binATTACK) \ 2
IF size > 0 THEN
 storerecord array(), workingdir$ + SLASH + "attack.bin", size, index
END IF
END SUB

SUB loadattackdata (array(), index)
loadoldattackdata array(), index
size = getbinsize(binATTACK) \ 2 'size of record in RPG file
IF size > 0 THEN
 DIM buf(size)
 loadnewattackdata buf(), index
 FOR i = 0 TO size
  array(40 + i) = buf(i)
 NEXT i
END IF
END SUB

SUB saveattackdata (array(), index)
saveoldattackdata array(), index
size = curbinsize(binATTACK) / 2 'size of record supported by engine
IF size > 0 THEN
 DIM buf(size)
 FOR i = 0 TO size
  buf(i) = array(40 + i)
 NEXT i
 savenewattackdata buf(), index
END IF
END SUB

FUNCTION defbinsize (id)
 IF id = 0 THEN RETURN 0  'attack.bin
 IF id = 1 THEN RETURN 64 '.stf
 IF id = 2 THEN RETURN 0  'songdata.bin
 IF id = 3 THEN RETURN 0  'sfxdata.bin
 IF id = 4 THEN RETURN 40 '.map
 RETURN 0
END FUNCTION

FUNCTION curbinsize (id)
 IF id = 0 THEN RETURN 120 'attack.bin
 IF id = 1 THEN RETURN 84  '.stf
 IF id = 2 THEN RETURN 32  'songdata.bin
 IF id = 3 THEN RETURN 34  'sfxdata.bin
 IF id = 4 THEN RETURN 44  '.map
 RETURN 0
END FUNCTION

FUNCTION getbinsize (id)

IF isfile(workingdir$ + SLASH + "binsize.bin") THEN
 fbdim recordsize
 fh = FREEFILE
 OPEN workingdir$ + SLASH + "binsize.bin" FOR BINARY AS #fh
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
FUNCTION dimbinsize (id)
 'curbinsize is size supported by current version of engine
 'getbinsize is size of data in RPG file
 dimbinsize = large(curbinsize(id), getbinsize(id)) / 2
END FUNCTION

SUB setbinsize (id, size)
fbdim size16
size16 = size
fh = FREEFILE
OPEN workingdir$ + SLASH + "binsize.bin" FOR BINARY AS #fh
PUT #fh, 1 + id * 2, size16
CLOSE #fh
END SUB

FUNCTION maplumpname$ (map, oldext$)
 IF map < 100 THEN
  maplumpname$ = game$ & "." & oldext$ & RIGHT$("0" & map, 2)
 ELSE
  maplumpname$ = workingdir$ & SLASH & map & "." & oldext$
 END IF
END FUNCTION

SUB getpal16 (array(), aoffset, foffset, autotype=-1, sprite=0)
DIM buf(8)

loadrecord buf(), game$ + ".pal", 8, 0
IF buf(0) = 4444 THEN '--check magic number
 IF buf(1) >= foffset AND foffset >= 0 THEN
  'palette is available
  loadrecord buf(), game$ + ".pal", 8, 1 + foffset
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
 xbload game$ + ".pal", xbuf(), "16-color palletes missing from " + game$
 FOR i = 0 TO 7
  array(aoffset * 8 + i) = xbuf(foffset * 8 + i)
 NEXT i
END IF

END SUB

SUB storepal16 (array(), aoffset, foffset)
DIM buf(8)

f$ = game$ + ".pal"
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
#IFDEF IS_GAME
setvispage 0
centerbox 160, 100, 300, 180, 3, 0
edgeprint e$, xstring(e$, 160), 20, uilook(uiText), 0
edgeprint "Press ESC to cleanly close the program", 15, 40, uilook(uiMenuItem), 0
edgeprint "or any other key to ignore the", 15, 50, uilook(uiMenuItem), 0
edgeprint "error and try to continue playing.", 15, 60, uilook(uiMenuItem), 0

setvispage 0 'refresh
w = getkey

IF w = 1 THEN
 closemusic
 restoremode
 PRINT e$
 SYSTEM
END IF
#ENDIF
#IFDEF IS_GAME
debug "fatal error:" + e$
textcolor 15, 0
FOR i = 0 TO 1
 clearpage i
 printstr e$, 0, 0, i
 printstr "an error has occured. Press ESC to", 0, 16, i
 printstr "close " + CUSTOMEXE + " or press any other", 0, 24, i
 printstr "key to try to continue. If the", 0, 32, i
 printstr "error keeps happening, send e-mail to", 0, 40, i
 printstr "ohrrpgce-crash@HamsterRepublic.com", 0, 48, i
NEXT i
setvispage 0 'refresh

w = getkey

IF w = 1 THEN
 restoremode

 touchfile workingdir$ + SLASH + "__danger.tmp"

 PRINT "fatal error:"
 PRINT e$

 'borrowed this code from game.bas cos wildcard didn't work in FB
 findfiles workingdir$ + SLASH + ALLFILES, 0, "filelist.tmp", buffer()
 fh = FREEFILE
 OPEN "filelist.tmp" FOR INPUT AS #fh
 DO UNTIL EOF(fh)
  LINE INPUT #fh, filename$
  KILL workingdir$ + SLASH + filename$
 LOOP
 CLOSE #fh
 KILL "filelist.tmp"
 RMDIR workingdir$

 SYSTEM
END IF
#ENDIF
END SUB

FUNCTION xstring (s$, x)
xstring = small(large(x - LEN(s$) * 4, 0), 319 - LEN(s$) * 8)
END FUNCTION

FUNCTION defaultint$ (n)
IF n = -1 THEN RETURN " default"
RETURN XSTR$(n)
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

FUNCTION peek8bit (array16(), index)
 element = array16(index \ 2)
 IF index AND 1 THEN
  RETURN (element AND &hFF00) SHR 8
 ELSE
  RETURN element AND &hFF
 END IF
END FUNCTION

SUB loadpalette(pal() as RGBcolor, palnum)
IF NOT isfile(workingdir$ + SLASH + "palettes.bin") THEN
 '.MAS fallback, palnum ignored because it doesn't matter
 DIM oldpalbuf(767)
 xbload game$ + ".mas", oldpalbuf(), "master palette missing from " + game$
 convertpalette oldpalbuf(), pal()
ELSE
 DIM AS SHORT headsize, recsize
 DIM palbuf(767) as UBYTE

 fh = FREEFILE
 OPEN workingdir$ + SLASH + "palettes.bin" FOR BINARY AS #fh
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
END SUB

SUB savepalette(pal() as RGBcolor, palnum)
IF isfile(workingdir$ + SLASH + "palettes.bin") THEN
 DIM AS SHORT headsize, recsize

 fh = FREEFILE
 OPEN workingdir$ + SLASH + "palettes.bin" FOR BINARY AS #fh
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

FUNCTION getmapname$ (m)
DIM nameread(39)
loadrecord nameread(), game$ + ".mn", 40, m
a$ = STRING$(small((nameread(0) AND 255), 39), " ")
array2str nameread(), 1, a$
RETURN a$
END FUNCTION

SUB createminimap (array(), map(), tastuf(), tilesetpage, zoom)
 'we don't have any sprite struct, so redim array() (dynamic array) and pass back the pixel data

 REDIM array(zoom * map(0) - 1, zoom * map(1) - 1)
 DIM AS SINGLE fraction, rand
 fraction = 20 / zoom
 rand = RND

 setmapdata map(), map(), 20, 0
 FOR i = 0 TO zoom * map(0) - 1
  FOR j = 0 TO zoom * map(1) - 1
   tx = i \ zoom
   ty = j \ zoom
   x = INT(((i MOD zoom) + rand) * fraction)
   y = INT(((j MOD zoom) + rand) * fraction)
   'over the top layer support
   FOR k = 2 TO 0 STEP -1
    block = readmapblock(tx, ty, k)
    IF block = 0 AND k > 0 THEN CONTINUE FOR
    IF block > 207 THEN block = (block - 207) + tastuf(20)
    IF block > 159 THEN block = (block - 159) + tastuf(0)
    mx = (block MOD 16) * 20
    my = (block \ 16) * 20
    array(i,j) = readpixel(mx + x, my + y, tilesetpage)
    IF array(i,j) <> 0 THEN EXIT FOR
   NEXT
  NEXT
 NEXT

END SUB

FUNCTION readattackname$ (index)
'--clobbers buffer!!!
readattackname$ = readbadgenericname$(index, game$ + ".dt6", 80, 24, 10, 1)
END FUNCTION

FUNCTION readenemyname$ (index)
'--clobbers buffer!!!
readenemyname$ = readbadgenericname$(index, game$ + ".dt1", 320, 0, 16, 0)
END FUNCTION

FUNCTION readitemname$ (index)
'--clobbers buffer!!!
readitemname$ = readbadgenericname$(index, game$ + ".itm", 200, 0, 8, 0)
END FUNCTION

FUNCTION readshopname$ (shopnum)
'clobbers buffer!
readshopname$ = readbadgenericname$(shopnum, game$ + ".sho", 40, 0, 15, 0)
END FUNCTION

FUNCTION getsongname$ (num AS INTEGER, prefixnum AS INTEGER = 0)
DIM songd(dimbinsize(2)) AS INTEGER
DIM s AS STRING
IF num = -1 THEN RETURN "-none-"
s = ""
IF prefixnum THEN s = num & " "
setpicstuf songd(), curbinsize(2), -1
loadset workingdir$ + SLASH + "songdata.bin", num, 0
s = s & readbinstring$ (songd(), 0, 30)
RETURN s
END FUNCTION

FUNCTION getsfxname$ (num AS INTEGER)
DIM sfxd(dimbinsize(3))
setpicstuf sfxd(), curbinsize(3), -1
loadset workingdir$ + SLASH + "sfxdata.bin", num, 0
getsfxname$ = readbinstring$ (sfxd(), 0, 30)
END FUNCTION

FUNCTION intgrabber (n AS INTEGER, min AS INTEGER, max AS INTEGER, less AS INTEGER, more AS INTEGER) AS INTEGER
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
 IF (keyval(29) > 0 AND keyval(82) > 1) OR ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(83)) OR (keyval(29) > 0 AND keyval(46) > 1) THEN clip = n
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

SUB playsongnum (songnum%)
	DIM songbase$, songfile$

	songbase$ = workingdir$ & SLASH & "song" & songnum%
  songfile$ = ""
  IF isfile(game$ & "." & songnum%) THEN
    songfile$ = game$ & "." & songnum% ' old-style BAM naming scheme
  ELSEIF isfile(songbase$ & ".mid") THEN
    songfile$ = songbase$ & ".mid"
  ELSEIF isfile(songbase$ & ".mp3") THEN
    songfile$ = songbase$ & ".mp3"
  ELSEIF isfile(songbase$ & ".ogg") THEN
    songfile$ = songbase$ & ".ogg"
  ELSEIF isfile(songbase$ & ".mod") THEN
    songfile$ = songbase$ & ".mod"
  ELSEIF isfile(songbase$ & ".xm") THEN
    songfile$ = songbase$ & ".xm"
  ELSEIF isfile(songbase$ & ".s3m") THEN
    songfile$ = songbase$ & ".s3m"
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
tempfile = tmpdir$ & "find_helper_app." & INT(RND * 10000) & ".tmp"
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
exedir = trimfilename$(exename$)
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
 tempwav = tmpdir$ & "temp." & INT(RND * 100000) & ".wav"
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

FUNCTION pick_ogg_quality() AS INTEGER
 STATIC q AS INTEGER = 4
 DIM i AS INTEGER
 clearpage dpage
 clearpage vpage
 setkeys
 DO
  setwait 80
  setkeys
  IF keyval(28) > 1 OR keyval(57) > 1 THEN EXIT DO
  intgrabber (q, -1, 10, 75, 77)
  centerbox 160, 100, 300, 40, 4, dpage
  edgeprint "Pick Ogg quality level (" & q & ")", 64, 86, uilook(uiText), dpage
  FOR i = 0 TO q + 1
   rectangle 30 + 21 * i, 100, 20, 16, uilook(uiText), dpage
  NEXT i
  swap vpage, dpage
  setvispage vpage
  dowait
 LOOP
 RETURN q
END FUNCTION
