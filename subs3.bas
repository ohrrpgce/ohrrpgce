'OHRRPGCE CUSTOM - Even more misc unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z

#include "const.bi"

'basic subs and functions
DECLARE FUNCTION str2lng& (stri$)
DECLARE FUNCTION str2int% (stri$)
DECLARE FUNCTION filenum$ (n%)
DECLARE SUB writeconstant (filehandle%, num%, names$, unique$(), prefix$)
DECLARE SUB writeglobalstring (index%, s$, maxlen%)
DECLARE SUB textfatalerror (e$)
DECLARE FUNCTION numbertail$ (s$)
DECLARE SUB cropafter (index%, limit%, flushafter%, lump$, bytes%, prompt%)
DECLARE FUNCTION isunique% (s$, u$(), r%)
DECLARE FUNCTION exclude$ (s$, x$)
DECLARE FUNCTION exclusive$ (s$, x$)
DECLARE FUNCTION needaddset (pt%, check%, what$)
DECLARE SUB cycletile (cycle%(), tastuf%(), pt%(), skip%())
DECLARE SUB testanimpattern (tastuf%(), taset%)
DECLARE SUB editbitset (array%(), wof%, last%, names$())
DECLARE SUB formation ()
DECLARE SUB enemydata ()
DECLARE SUB herodata ()
DECLARE SUB attackdata ()
DECLARE SUB getnames (stat$(), max%)
DECLARE SUB statname ()
DECLARE SUB textage ()
DECLARE FUNCTION sublist% (num%, s$())
DECLARE SUB maptile (font%())
DECLARE FUNCTION strgrabber (s$, maxl) AS INTEGER
DECLARE SUB fixfilename (s$)
DECLARE FUNCTION inputfilename$ (query$, ext$, default$ = "")
DECLARE FUNCTION scrintgrabber (n%, BYVAL min%, BYVAL max%, BYVAL less%, BYVAL more%, scriptside%, triggertype%)
DECLARE FUNCTION tagnames (starttag AS INTEGER=0, picktag AS INTEGER=NO) AS INTEGER

#include "compat.bi"
#include "allmodex.bi"
#include "common.bi"
#include "cglobals.bi"

#include "scrconst.bi"

REM $STATIC
' SUB editbitset (array(), wof, last, names$())

' '---DIM AND INIT---
' pt = -1
' top = -1

' '---MAIN LOOP---
' setkeys
' DO
'  setwait timing(), 80
'  setkeys
'  tog = tog XOR 1
'  IF keyval(1) > 1 THEN EXIT DO
'  usemenu pt, top, -1, last, 24
'  IF pt >= 0 THEN
'   IF keyval(75) > 1 OR keyval(51) > 1 THEN setbit array(), wof, pt, 0
'   IF keyval(77) > 1 OR keyval(52) > 1 THEN setbit array(), wof, pt, 1
'   IF keyval(57) > 1 OR keyval(28) > 1 THEN setbit array(), wof, pt, readbit(array(), wof, pt) XOR 1
'  ELSE
'   IF keyval(28) > 1 OR keyval(57) > 1 THEN EXIT DO
'  END IF
'  FOR i = top TO small(top + 24, last)
'   c = 8 - readbit(array(), wof, i)
'   IF pt = i THEN c = (8 * readbit(array(), wof, i)) + 6 + tog
'   textcolor c, 0
'   IF i >= 0 THEN
'    printstr names$(i), 8, (i - top) * 8, dpage
'   ELSE
'    IF c = 8 THEN c = 7
'    textcolor c, 0
'    printstr "Previous Menu", 8, (i - top) * 8, dpage
'   END IF
'  NEXT i
'  ' printstr XSTR$(pt) + XSTR$(top) + XSTR$(last), 160, 0, dpage
'  SWAP vpage, dpage
'  setvispage vpage
'  clearpage dpage
'  dowait
' LOOP
' '---TERMINATE---

' END SUB

'This new bitset() will build its own menu of bits, and thus hide blank bitsets
SUB editbitset (array(), wof, last, names$())

'---DIM AND INIT---
pt = -1
top = -1

dim menu$(-1 to last), bits(-1 to last), count

pt = 0
FOR i = 0 to last
 IF names$(i) <> "" THEN
  menu$(pt) = names$(i)
  bits(pt) = i
  pt = pt + 1
 END IF
NEXT

count = pt
pt = -1

'---MAIN LOOP---
setkeys
DO
 setwait timing(), 80
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 usemenu pt, top, -1, count-1, 24
 IF pt >= 0 THEN
  IF keyval(75) > 1 OR keyval(51) > 1 THEN setbit array(), wof, bits(pt), 0
  IF keyval(77) > 1 OR keyval(52) > 1 THEN setbit array(), wof, bits(pt), 1
  IF keyval(57) > 1 OR keyval(28) > 1 THEN setbit array(), wof, bits(pt), readbit(array(), wof, bits(pt)) XOR 1
 ELSE
  IF keyval(28) > 1 OR keyval(57) > 1 THEN EXIT DO
 END IF
 FOR i = top TO small(top + 24, count-1)
  c = 8 - readbit(array(), wof, bits(i))
  IF pt = i THEN c = (8 * readbit(array(), wof, bits(i))) + 6 + tog
  textcolor c, 0
  IF i >= 0 THEN
   printstr menu$(i), 8, (i - top) * 8, dpage
  ELSE
   IF c = 8 THEN c = 7
   textcolor c, 0
   printstr "Previous Menu", 8, (i - top) * 8, dpage
  END IF
 NEXT i
 ' printstr XSTR$(pt) + XSTR$(top) + XSTR$(last), 160, 0, dpage
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
'---TERMINATE---

END SUB

FUNCTION exclude$ (s$, x$)
outf$ = ""
FOR i = 1 TO LEN(s$)
 ok = -1
 FOR j = 1 TO LEN(x$)
  IF MID$(s$, i, 1) = MID$(x$, j, 1) THEN ok = 0
 NEXT j
 IF ok THEN outf$ = outf$ + MID$(s$, i, 1)
NEXT i
exclude$ = outf$
END FUNCTION

FUNCTION exclusive$ (s$, x$)
outf$ = ""
FOR i = 1 TO LEN(s$)
 ok = 0
 FOR j = 1 TO LEN(x$)
  IF MID$(s$, i, 1) = MID$(x$, j, 1) THEN ok = 1
 NEXT j
 IF ok THEN outf$ = outf$ + MID$(s$, i, 1)
NEXT i
exclusive$ = outf$
END FUNCTION

FUNCTION filesize$ (file$)
'returns size of a file in formatted string
IF isfile(file$) THEN
 ff = FREEFILE
 OPEN file$ FOR BINARY AS #ff
 size = LOF(ff)
 CLOSE #ff

 units$ = " B"
 split = 0
 IF size > 1024 THEN split = 1 : units$ = " KB"
 IF size > 1048576 THEN split = 1 : size = size / 1024 : units$ = " MB"
 fsize$ = STR$(size)
 IF split <> 0 THEN
  size = size / 102.4
  fsize$ = STR$(size \ 10)
  IF size < 1000 THEN fsize$ = fsize$ + "." + STR$(size MOD 10)
 END IF
 filesize$ = fsize$ + units$
ELSE
 filesize$ = "N/A"
END IF
END FUNCTION

FUNCTION inputfilename$ (query$, ext$, default$)
filename$ = default$
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN inputfilename$ = "": EXIT DO
 strgrabber filename$, 40
 fixfilename filename$
 IF keyval(28) > 1 THEN
  filename$ = TRIM$(filename$)
  IF isfile(filename$ + ext$) AND filename$ <> "" THEN alert$ = filename$ + ext$ + " already exists": alert = 30: filename$ = ""
  IF filename$ <> "" THEN inputfilename$ = filename$: EXIT DO
 END IF
 textcolor 15, 0
 printstr query$, 160 - LEN(query$) * 4, 20, dpage
 IF alert > 0 THEN printstr alert$, 160 - LEN(alert$) * 4, 40, dpage: alert = alert - 1
 textcolor 14 + tog, 1
 printstr filename$, 160 - (LEN(filename$) + LEN(ext$)) * 4 , 30, dpage
 textcolor 15, 1
 printstr ext$, 160 + (LEN(filename$) - LEN(ext$)) * 4 , 30, dpage
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
END FUNCTION

FUNCTION numbertail$ (s$)

DIM n AS LONG

IF s$ = "" THEN
 outf$ = "BLANK"
ELSE
 a = ASC(RIGHT$(s$, 1))
 IF a < 48 OR a > 57 THEN
  outf$ = s$ + "2"
 ELSE
  a$ = s$
  b$ = ""
  DO WHILE ASC(RIGHT$(a$, 1)) >= 48 AND ASC(RIGHT$(a$, 1)) <= 57
   b$ = RIGHT$(a$, 1) + b$
   a$ = LEFT$(a$, LEN(a$) - 1)
   IF LEN(a$) = 0 THEN EXIT DO
  LOOP
  IF LEN(b$) > 9 THEN b$ = "0"
  n = str2int(b$)
  n = n + 1
  outf$ = a$ + STR$(n)
 END IF
END IF

numbertail$ = outf$

END FUNCTION

SUB seekscript (temp, seekdir, triggertype)
'temp = -1 means scroll to last script
'returns 0 when scrolled past first script, -1 when went past last

 DIM buf(19), plotids(gen(43))
 recordsloaded = 0
 screxists = 0

 fh = FREEFILE
 OPEN workingdir$ + SLASH + "lookup" + STR$(triggertype) + ".bin" FOR BINARY AS #fh
 triggernum = LOF(fh) \ 40
 IF temp = -1 THEN temp = triggernum + 16384

 DO
  temp += seekdir
  IF temp > gen(43) AND temp < 16384 THEN
   IF seekdir > 0 THEN
    temp = 16384
   ELSEIF triggertype = plottrigger THEN
    temp = gen(43)
   ELSE
    temp = 0
   END IF
  END IF
  IF temp <= 0 THEN EXIT DO
  IF temp >= triggernum + 16384 THEN
   temp = -1
   EXIT DO
  END IF
  'check script exists, else keep looking
  IF temp < 16384 AND triggertype = plottrigger THEN
   IF plotids(temp) THEN
    screxists = -1
   ELSE
    WHILE recordsloaded < gen(40)
     loadrecord buf(), workingdir$ + SLASH + "plotscr.lst", 20, recordsloaded
     recordsloaded += 1
     IF buf(0) = temp THEN screxists = -1: EXIT WHILE
     IF buf(0) <= gen(43) THEN plotids(buf(0)) = -1
    WEND
   END IF
  END IF
  IF temp >= 16384 THEN
   loadrecord buf(), fh, 20, temp - 16384
   IF buf(0) THEN screxists = -1
  END IF
  IF screxists THEN EXIT DO
 LOOP

 CLOSE fh
END SUB

FUNCTION scrintgrabber (n, BYVAL min, BYVAL max, BYVAL less, BYVAL more, scriptside, triggertype)
'script side is 1 or -1: on which side of zero are the scripts
'min or max on side of scripts is ignored

temp = n
IF scriptside < 0 THEN
 temp = -n
 SWAP less, more
 min = -min
 max = -max
 SWAP min, max
END IF

seekdir = 0
IF keyval(more) > 1 THEN
 seekdir = 1
ELSEIF keyval(less) > 1 THEN
 seekdir = -1
END IF

IF seekdir THEN
 scriptscroll = 0
 IF temp = min AND seekdir = -1 THEN
  temp = -1
  scriptscroll = -1
 ELSEIF (temp = 0 AND seekdir = 1) OR temp > 0 THEN
  scriptscroll = -1
 END IF
 IF scriptscroll THEN
  'scroll through scripts
  seekscript temp, seekdir, triggertype
  IF temp = -1 THEN temp = min
 ELSE
  'regular scroll
  temp += seekdir
 END IF
ELSE
 IF (temp > 0 AND temp < 16384) OR (temp = 0 AND scriptside = 1) THEN
  'if a number is entered, don't seek to the next script, allow "[id]" to display instead
  IF intgrabber(temp, 0, 16383, 0, 0) THEN
   'if temp starts off greater than gen(43) then don't disturb it
   temp = small(temp, gen(43))
  END IF
 ELSEIF temp < 0 OR (temp = 0 AND scriptside = -1) THEN
  intgrabber(temp, min, 0, 0, 0)
 END IF
END IF

IF keyval(83) > 1 THEN temp = 0
IF keyval(12) > 1 OR keyval(74) > 1 THEN temp = bound(-temp, min, gen(43))

temp = temp * SGN(scriptside)
scrintgrabber = (temp <> n)
n = temp
END FUNCTION

FUNCTION str2int (stri$)

n = 0
s$ = LTRIM$(stri$)
sign = 1

FOR i = 1 TO LEN(s$)
 c$ = MID$(s$, i, 1)
 IF c$ = "-" AND i = 1 THEN sign = -1
 c = ASC(c$) - 48
 IF c >= 0 AND c <= 9 THEN
  n = n * 10 + (c * sign)
 END IF
NEXT i

str2int = n

END FUNCTION

FUNCTION str2lng& (stri$)

n& = 0
s$ = LTRIM$(stri$)
sign = 1

FOR i = 1 TO LEN(s$)
 c$ = MID$(s$, i, 1)
 IF c$ = "-" AND i = 1 THEN sign = -1
 c = ASC(c$) - 48
 IF c >= 0 AND c <= 9 THEN
  n& = n& * 10 + (c * sign)
 END IF
NEXT i

str2lng& = n&

END FUNCTION

FUNCTION tag_grabber (BYREF n AS INTEGER) AS INTEGER
 IF intgrabber(n, -999, 999) THEN RETURN YES
 IF keyval(28) > 1 OR keyval(57) > 1 THEN
  DIM browse_tag AS INTEGER
  browse_tag = tagnames(n, YES)
  IF browse_tag >= 2 THEN
   n = browse_tag
   RETURN YES
  END IF
 END IF
 RETURN NO
END FUNCTION

FUNCTION tagnames (starttag AS INTEGER=0, picktag AS INTEGER=NO) AS INTEGER
DIM state AS MenuState
DIM thisname AS STRING
IF gen(genMaxTagname) < 1 THEN gen(genMaxTagname) = 1
DIM menu(gen(genMaxTagname)) AS STRING
IF picktag THEN
 menu(0) = "Cancel"
ELSE
 menu(0) = "Previous Menu"
END IF
DIM i AS INTEGER
FOR i = 2 TO gen(genMaxTagname) + 1
 'Load all tag names plus the first blank name
 menu(i - 1) = "Tag " & i & ":" & load_tag_name(i)
NEXT i

clearpage 0
clearpage 1

DIM tagsign AS INTEGER
tagsign = SGN(starttag)
IF tagsign = 0 THEN tagsign = 1

state.size = 24
state.last = gen(genMaxTagname)

state.pt = 0
IF ABS(starttag) >= 2 THEN state.pt = small(ABS(starttag) - 1, gen(genMaxTagName))
IF state.pt >= 1 THEN thisname = load_tag_name(state.pt + 1)

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 IF usemenu(state) THEN
  IF state.pt >= 1 AND state.pt <= gen(genMaxTagName) THEN
   thisname = load_tag_name(state.pt + 1)
  ELSE
   thisname = ""
  END IF
 END IF
 IF state.pt = 0 AND (keyval(57) > 1 OR keyval(28) > 1) THEN EXIT DO
 IF state.pt > 0 AND state.pt <= gen(genMaxTagName) THEN
  IF picktag THEN
   IF keyval(28) > 1 THEN
    RETURN (state.pt + 1) * tagsign
   END IF
  END IF
  IF strgrabber(thisname, 20) THEN
   save_tag_name thisname, state.pt + 1
   menu(state.pt) = "Tag " & state.pt + 1 & ":" & thisname
   IF state.pt = gen(genMaxTagName) THEN
    IF gen(genMaxTagName) < 999 THEN
     gen(genMaxTagName) += 1
     REDIM PRESERVE menu(gen(genMaxTagName)) AS STRING
     menu(gen(genMaxTagName)) = "Tag " & gen(genMaxTagName) + 1 & ":"
     state.last += 1
    END IF
   END IF
  END IF
 END IF

 standardmenu menu(), state, 0, 0, dpage

 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

RETURN 0
END FUNCTION

SUB textfatalerror (e$)

debug "fatal error:" + e$

touchfile workingdir$ + SLASH + "__danger.tmp"

PRINT "fatal error:"
PRINT e$

'borrowed this code from game.bas cos wildcard didn't work
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

END SUB

SUB writeglobalstring (index, s$, maxlen)

fh = FREEFILE

OPEN game$ + ".stt" FOR BINARY AS #fh

a$ = CHR$(small(LEN(s$), small(maxlen, 255)))
PUT #fh, 1 + index * 11, a$
a$ = LEFT$(s$, small(maxlen, 255))
PUT #fh, 2 + index * 11, a$

CLOSE #fh

END SUB
