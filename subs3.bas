'OHRRPGCE CUSTOM - Even more misc unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE FUNCTION str2lng& (stri$)
DECLARE FUNCTION str2int% (stri$)
DECLARE FUNCTION readshopname$ (shopnum%)
DECLARE SUB flusharray (array%(), size%, value%)
DECLARE FUNCTION filenum$ (n%)
DECLARE SUB writeconstant (filehandle%, num%, names$, unique$(), prefix$)
DECLARE SUB safekill (f$)
DECLARE SUB touchfile (f$)
DECLARE SUB romfontchar (font%(), char%)
DECLARE SUB standardmenu (menu$(), size%, vis%, pt%, top%, x%, y%, page%, edge%)
DECLARE FUNCTION readitemname$ (index%)
DECLARE FUNCTION readattackname$ (index%)
DECLARE SUB writeglobalstring (index%, s$, maxlen%)
DECLARE FUNCTION readglobalstring$ (index%, default$, maxlen%)
DECLARE FUNCTION getShortName$ (filename$)
DECLARE FUNCTION getLongName$ (filename$)
DECLARE SUB textfatalerror (e$)
DECLARE SUB xbload (f$, array%(), e$)
DECLARE SUB fatalerror (e$)
DECLARE FUNCTION scriptname$ (num%, f$)
DECLARE FUNCTION unlumpone% (lumpfile$, onelump$, asfile$)
DECLARE FUNCTION getmapname$ (m%)
DECLARE FUNCTION numbertail$ (s$)
DECLARE SUB cropafter (index%, limit%, flushafter%, lump$, bytes%, prompt%)
DECLARE FUNCTION isunique% (s$, u$(), r%)
DECLARE FUNCTION loadname$ (length%, offset%)
DECLARE SUB exportnames (gamedir$, song$())
DECLARE FUNCTION exclude$ (s$, x$)
DECLARE FUNCTION exclusive$ (s$, x$)
DECLARE FUNCTION needaddset (pt%, check%, what$)
DECLARE FUNCTION browse$ (special, default$, fmask$, tmp$)
DECLARE SUB cycletile (cycle%(), tastuf%(), pt%(), skip%())
DECLARE SUB testanimpattern (tastuf%(), taset%)
DECLARE FUNCTION usemenu (pt%, top%, first%, last%, size%)
DECLARE FUNCTION heroname$ (num%, cond%(), a%())
DECLARE FUNCTION bound% (n%, lowest%, highest%)
DECLARE FUNCTION onoroff$ (n%)
DECLARE FUNCTION intstr$ (n%)
DECLARE FUNCTION lmnemonic$ (index%)
DECLARE FUNCTION rotascii$ (s$, o%)
DECLARE SUB debug (s$)
DECLARE SUB bitset (array%(), wof%, last%, names$())
DECLARE FUNCTION usemenu (pt%, top%, first%, last%, size%)
DECLARE SUB edgeprint (s$, x%, y%, c%, p%)
DECLARE SUB formation (song$())
DECLARE SUB enemydata ()
DECLARE SUB herodata ()
DECLARE SUB attackdata ()
DECLARE SUB getnames (stat$(), max%)
DECLARE SUB statname ()
DECLARE SUB textage (song$())
DECLARE FUNCTION sublist% (num%, s$())
DECLARE SUB maptile (master%(), font%())
DECLARE FUNCTION small% (n1%, n2%)
DECLARE FUNCTION large% (n1%, n2%)
DECLARE FUNCTION loopvar% (var%, min%, max%, inc%)
DECLARE FUNCTION intgrabber (n%, min%, max%, less%, more%)
DECLARE SUB strgrabber (s$, maxl%)
DECLARE SUB smnemonic (tagname$, index%)
DECLARE SUB setbinsize (id%, size%)
DECLARE FUNCTION getbinsize% (id%)
DECLARE SUB fixfilename (s$)
DECLARE FUNCTION inputfilename$ (query$, ext$)

'$INCLUDE: 'allmodex.bi'
'$INCLUDE: 'cglobals.bi'

'$INCLUDE: 'const.bi'

REM $STATIC
' SUB bitset (array(), wof, last, names$())

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
'  dummy = usemenu(pt, top, -1, last, 24)
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
'  ' printstr STR$(pt) + STR$(top) + STR$(last), 160, 0, dpage
'  SWAP vpage, dpage
'  setvispage vpage
'  clearpage dpage
'  dowait
' LOOP
' '---TERMINATE---

' END SUB

'This new bitset() will build its own menu of bits, and thus hide blank bitsets
SUB bitset (array(), wof, last, names$())

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
 dummy = usemenu(pt, top, -1, count-1, 24)
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
 ' printstr STR$(pt) + STR$(top) + STR$(last), 160, 0, dpage
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
'---TERMINATE---

END SUB

FUNCTION bound (n, lowest, highest)
bound = n
IF n < lowest THEN bound = lowest
IF n > highest THEN bound = highest
END FUNCTION

SUB debug (s$)
OPEN "c_debug.txt" FOR APPEND AS #3
PRINT #3, s$
CLOSE #3
END SUB

SUB drawmini (high, wide, cursor(), page, tastuf())

clearpage vpage
FOR i = 0 TO high
 FOR o = 0 TO wide
  block = readmapblock(o, i)
  IF block > 207 THEN block = (block - 207) + tastuf(20)
  IF block > 159 THEN block = (block - 159) + tastuf(0)
  mx = block - (INT(block / 16) * 16)
  my = INT(block / 16)
  loadsprite cursor(), 0, (INT(RND * 7) + 7) + (mx * 20), (INT(RND * 7) + 7) + (my * 20), 1, 1, 3
  stosprite cursor(), 0, o, i, page
 NEXT o
NEXT i

END SUB

FUNCTION exclude$ (s$, x$)
out$ = ""
FOR i = 1 TO LEN(s$)
 ok = -1
 FOR j = 1 TO LEN(x$)
  IF MID$(s$, i, 1) = MID$(x$, j, 1) THEN ok = 0
 NEXT j
 IF ok THEN out$ = out$ + MID$(s$, i, 1)
NEXT i
exclude$ = out$
END FUNCTION

FUNCTION exclusive$ (s$, x$)
out$ = ""
FOR i = 1 TO LEN(s$)
 ok = 0
 FOR j = 1 TO LEN(x$)
  IF MID$(s$, i, 1) = MID$(x$, j, 1) THEN ok = 1
 NEXT j
 IF ok THEN out$ = out$ + MID$(s$, i, 1)
NEXT i
exclusive$ = out$
END FUNCTION

FUNCTION filesize$ (file$)
'returns size of a file in formatted string
IF isfile(file$ + CHR$(0)) THEN
 ff = FREEFILE
 OPEN file$ FOR BINARY AS #ff
 size = LOF(ff)
 CLOSE #ff

 units$ = " B"
 IF size > 1024 THEN split = 1 : units$ = " KB"
 IF size > 1048576 THEN split = 1 : size = size / 1024 : units$ = " MB"
 fsize$ = intstr$(size)
 IF split THEN
  size = size / 102.4
  fsize$ = intstr$(size \ 10)
  IF size < 1000 THEN fsize$ = fsize$ + "." + intstr$(size MOD 10)
 END IF
 filesize$ = fsize$ + units$
ELSE
 filesize$ = "N/A"
END IF
END FUNCTION

SUB flusharray (array(), size, value)
FOR i = 0 TO size
 array(i) = value
NEXT i
END SUB

FUNCTION getLongName$ (filename$)
'--given a filename, returns its longname.
'  it will always return the filename only, without the path
'  even though it can accept a fully qualified filename as input

'--has a bug that prevents it from returning files that are longer
'  than 260 chars including pathname

failed = 0
result$ = ""
length = LongNameLength(filename$ + CHR$(0))
IF length = -1 THEN
 '--failed to get any name at all
 failed = -1
ELSE
 a$ = STRING$(length, 0)
 getstring a$
 FOR i = LEN(a$) TO 1 STEP -1
  IF MID$(a$, i, 1) = "\" OR MID$(a$, i, 1) = ":" THEN EXIT FOR
  IF MID$(a$, i, 1) <> CHR$(0) THEN
   result$ = MID$(a$, i, 1) + result$
  END IF
 NEXT i
 IF result$ = "" THEN
  '--never return a null result!
  failed = -1
 END IF
END IF
IF failed THEN
 '--failed, return input (minus path)
 FOR i = LEN(filename$) TO 1 STEP -1
  IF MID$(filename$, i, 1) = "\" OR MID$(filename$, i, 1) = ":" THEN EXIT FOR
  result$ = MID$(filename$, i, 1) + result$
 NEXT i
END IF
getLongName$ = result$
END FUNCTION

FUNCTION inputfilename$ (query$, ext$)
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN inputfilename$ = "": EXIT DO
 strgrabber file$, 8
 fixfilename file$
 IF keyval(28) > 1 THEN
  IF isfile(file$ + ext$ + CHR$(0)) AND file$ <> "" THEN alert$ = file$ + ext$ + " already exists": alert = 30: file$ = ""
  IF file$ <> "" THEN inputfilename$ = file$: EXIT DO
 END IF
 textcolor 15, 0
 printstr query$, 160 - LEN(query$) * 4, 20, dpage
 IF alert > 0 THEN printstr alert$, 160 - LEN(alert$) * 4, 40, dpage: alert = alert - 1
 textcolor 14 + tog, 1
 printstr file$, 160 - LEN(game$) * 4, 30, dpage
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
END FUNCTION

FUNCTION intstr$ (n)
IF n < 0 THEN
 intstr$ = STR$(n)
ELSE
 intstr$ = RIGHT$(STR$(n), LEN(STR$(n)) - 1)
END IF
END FUNCTION

FUNCTION numbertail$ (s$)

DIM n AS LONG

IF s$ = "" THEN
 out$ = "BLANK"
ELSE
 a = ASC(RIGHT$(s$, 1))
 IF a < 48 OR a > 57 THEN
  out$ = s$ + "2"
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
  out$ = a$ + LTRIM$(STR$(n))
 END IF
END IF

numbertail$ = out$

END FUNCTION

FUNCTION readglobalstring$ (index, default$, maxlen)

fh = FREEFILE
OPEN game$ + ".stt" FOR BINARY AS #fh

a$ = CHR$(0)
GET #fh, 1 + index * 11, a$
namelen = 0: IF a$ <> "" THEN namelen = ASC(a$)

IF index * 11 + i > LOF(fh) THEN
 result$ = default$
ELSE
 result$ = STRING$(small(namelen, maxlen), CHR$(0))
 GET #fh, 2 + index * 11, result$
END IF

CLOSE #fh

readglobalstring = result$
END FUNCTION

FUNCTION rotascii$ (s$, o)

temp$ = ""

FOR i = 1 TO LEN(s$)
 temp$ = temp$ + CHR$(loopvar(ASC(MID$(s$, i, 1)), 0, 255, o))
NEXT i

rotascii$ = temp$

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

SUB tagnames
DIM menu$(2)
clearpage 0
clearpage 1

IF general(56) < 1 THEN general(56) = 1
pt = 2
csr = 0
menu$(0) = "Previous Menu"
tagname$ = lmnemonic$(pt)

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 dummy = usemenu(csr, 0, 0, 2, 24)
 IF csr = 0 AND (keyval(57) > 1 OR keyval(28) > 1) THEN EXIT DO
 IF csr = 1 THEN
  oldptr = pt
  IF intgrabber(pt, 0, small(general(56) + 1, 999), 75, 77) THEN
   IF pt > general(56) THEN general(56) = pt
   smnemonic tagname$, oldptr
   tagname$ = lmnemonic$(pt)
  END IF
 END IF
 IF csr = 2 THEN
  strgrabber tagname$, 20
  IF keyval(28) > 1 THEN
   smnemonic tagname$, pt
   pt = small(pt + 1, 999)
   tagname$ = lmnemonic$(pt)
  END IF
 END IF
 menu$(1) = "Tag" + STR$(pt)
 menu$(2) = "Name:" + tagname$
 
 standardmenu menu$(), 2, 22, csr, 0, 0, 0, dpage, 0
 
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
smnemonic tagname$, pt

END SUB

SUB textfatalerror (e$)

debug "fatal error:" + e$

touchfile workingdir$ + "\__danger.tmp"

PRINT "fatal error:"
PRINT e$

KILL workingdir$ + "\*.*"
RMDIR workingdir$

SYSTEM

END SUB

FUNCTION unlumpone (lumpfile$, onelump$, asfile$)
unlumpone = 0

IF NOT isdir("unlump1.tmp" + CHR$(0)) THEN MKDIR "unlump1.tmp"
CALL unlump(lumpfile$ + CHR$(0), "unlump1.tmp\", buffer())

IF isfile("unlump1.tmp\" + onelump$ + CHR$(0)) THEN
 copyfile "unlump1.tmp\" + onelump$ + CHR$(0), asfile$ + CHR$(0), buffer()
 unlumpone = -1
END IF

touchfile "unlump1.tmp\nothing.tmp"

KILL "unlump1.tmp\*.*"
RMDIR "unlump1.tmp"

END FUNCTION

SUB updaterecordlength (lumpf$, bindex)
IF getbinsize(bindex) < curbinsize(bindex) THEN
 printstr "Upgrading " + lumpf$ + " to new record size...", 0, 10, vpage

 tempf$ = workingdir$ + "\" + "resize.tmp"

 oldsize = getbinsize(bindex)
 newsize = curbinsize(bindex)

 flusharray buffer(), newsize / 2, 0

 ff = FREEFILE
 OPEN lumpf$ FOR BINARY AS #ff
 records = LOF(ff) / oldsize
 CLOSE #ff

 copyfile lumpf$ + CHR$(0), tempf$ + CHR$(0), buffer()
 KILL lumpf$

 FOR i = 0 TO records - 1
  setpicstuf buffer(), oldsize, -1
  loadset tempf$ + CHR$(0), i, 0
  setpicstuf buffer(), newsize, -1
  storeset lumpf$ + CHR$(0), i, 0
 NEXT

 KILL tempf$

 setbinsize bindex, newsize

END IF
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

SUB xbload (f$, array(), e$)

IF isfile(f$ + CHR$(0)) THEN
 handle = FREEFILE
 OPEN f$ FOR BINARY AS #handle
 bytes = LOF(handle)
 CLOSE #handle
 IF bytes THEN
  OPEN f$ FOR BINARY AS #handle
  a$ = " "
  GET #handle, 1, a$
  CLOSE #handle
  IF a$ = CHR$(253) THEN
   DEF SEG = VARSEG(array(0)): BLOAD f$, VARPTR(array(0))
  ELSE
   fatalerror e$ + "(unbloadable)"
  END IF
 ELSE
  fatalerror e$ + "(zero byte)"
 END IF
ELSE
 fatalerror e$
END IF

END SUB

