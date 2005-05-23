'NOTATE - BAM Sheet-Music Editor
'(C) Copyright 1997 James Paige, Brian Fisher, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'
'$DYNAMIC
DEFINT A-Z
'Basic Sub
DECLARE FUNCTION small (n1, n2)
DECLARE FUNCTION large (n1, n2)
'General Mode-X Stuff
DECLARE SUB setmodex ()
DECLARE SUB copypage (BYVAL page1, BYVAL page2)
DECLARE SUB setvispage (BYVAL page)
DECLARE SUB clearpage (BYVAL page)
'Mode-X Boxes N Pixels
DECLARE SUB rectangle (BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL C, BYVAL p)
DECLARE SUB putpixel (BYVAL x, BYVAL y, BYVAL C, BYVAL p)
DECLARE FUNCTION readpixel (BYVAL x, BYVAL y, BYVAL p)
'Palette stuff
DECLARE SUB fadeto (palbuff(), BYVAL red, BYVAL green, BYVAL blue)
DECLARE SUB fadetopal (pal(), palbuff())
DECLARE SUB setpal (pal())
'Mode-X Text
DECLARE SUB setfont (f())
DECLARE SUB printstr (s$, BYVAL x, BYVAL y, BYVAL p)
DECLARE SUB textcolor (BYVAL f, BYVAL b)
'Spiffo Timing
DECLARE SUB setwait (b(), BYVAL t)
DECLARE SUB dowait ()
'Copy file
DECLARE SUB copyfile (s$, d$, b())
'Adlib musica
DECLARE SUB resetfm ()
DECLARE SUB fmkeyon (BYVAL v, BYVAL n)
DECLARE SUB fmkeyoff (BYVAL v)
DECLARE SUB getvoice (BYVAL v, BYVAL io, f$, b())
DECLARE SUB setvoice (BYVAL v, b())
'Keyhandling
DECLARE SUB setkeys ()
DECLARE FUNCTION Keyseg ()
DECLARE FUNCTION keyoff ()
DECLARE FUNCTION keyval (BYVAL a)
DECLARE FUNCTION getkey ()
TYPE Regtype
 ax AS INTEGER
 bx AS INTEGER
 cx AS INTEGER
 dx AS INTEGER
 bp AS INTEGER
 si AS INTEGER
 di AS INTEGER
 flags AS INTEGER
 ds AS INTEGER
 es AS INTEGER
END TYPE
DIM SHARED regs AS Regtype
regs.ax = &H3509: CALL interruptx(&H21, regs, regs)
off9 = regs.bx: seg9 = regs.es
DIM font(1024), master(767), buffer(16384), timing(4), mouse(4)
DIM sline(128), note$(6), n(150, 9), l(150, 9), nlen(7), nlen$(7), col(8), ny(127), noff(8), inst$(127), cinst$(8), cinst(8), kp$(50), tag(15), jump(99), j2tag(99), jid(99)
setmodex
resetfm
OPEN "gm.ibk" FOR BINARY AS #1
FOR i = 0 TO 127
 inst$(i) = STRING$(9, 0)
 GET #1, 2053 + i * 9, inst$(i)
NEXT i
CLOSE #1
FOR i = 0 TO 8
 getvoice i, 0, "ibank.ibk" + CHR$(0), buffer()
 cinst$(i) = inst$(0)
NEXT i
DEF SEG = VARSEG(font(0)): BLOAD "notate.fnt", VARPTR(font(0))
setfont font()
DEF SEG = VARSEG(master(0)): BLOAD "notate.mas", VARPTR(master(0))
setpal master()
GOSUB switchon
RANDOMIZE TIMER
vpage = 0: dpage = 1: vptr = 60: nl = 3: file$ = "template.not": sharp$(1) = "sharp": mes = 32: f$ = ""
FOR i = 0 TO 9
READ o
sline(o) = 1
NEXT i
FOR i = 0 TO 6
READ note$(i)
NEXT i
FOR i = 0 TO 7
READ nlen(i)
NEXT i
FOR i = 0 TO 7
READ nlen$(i)
NEXT i
FOR i = 0 TO 8
READ col(i)
NEXT i
FOR i = 0 TO 127
READ ny(i)
NEXT i
FOR i = 2 TO 50
READ kp$(i)
NEXT i
rectangle 0, 0, 320, 40, 7, 2
FOR i = 43 TO 77
 IF sline(i) = 1 THEN rectangle 10, 200 - (ny(i) * 2), 300, 1, 8, 2
NEXT i
OPEN file$ FOR BINARY AS #1
FOR i = 0 TO 8
 a$ = " "
 GET #1, 32770 + i, a$
 cinst(i) = ASC(a$)
 cinst$(i) = inst$(cinst(i))
 getvoice i, cinst(i), "ibank.ibk" + CHR$(0), buffer()
NEXT i
GET #1, 32770 + 9, last
FOR i = 1 TO 15
 GET #1, 32770 + 11 + i * 2, tag(i)
NEXT i
FOR i = 0 TO 99
 GET #1, 32770 + 45 + (i * 6 + 0), jump(i)
 GET #1, 32770 + 45 + (i * 6 + 2), jid(i)
 GET #1, 32770 + 45 + (i * 6 + 4), j2tag(i)
NEXT i
CLOSE #1
GOSUB visnotes

setkeys:
DO
setwait timing(), 62
setkeys
tog = tog XOR 1
FOR i = 0 TO 8
 IF noff(i) > 0 THEN noff(i) = noff(i) - 1: IF noff(i) = 0 THEN fmkeyoff i
NEXT i
GOSUB keyinput
GOSUB display
IF quit = 1 THEN GOTO finis
dowait
LOOP

playscr:
rectangle 0, 0, 320, 40, 7, vpage
textcolor 240, 0
printstr "Playing...", 1, 1, vpage
dowait
FOR i = 0 TO 150
 setwait timing(), 62
 FOR o = 0 TO 8
  IF (n(i, o) AND 128) = 128 THEN fmkeyoff o
  IF (n(i, o) AND 127) > 0 THEN fmkeyon o, (n(i, o) AND 127)
 NEXT o
 dowait
 IF i + left > last THEN i = 150
NEXT i
FOR i = 0 TO 8
 fmkeyoff i
NEXT i
RETURN

keyinput:
IF keyval(1) > 1 THEN quit = 1
IF keyval(75) > 0 AND ptr > 0 THEN ptr = ptr - 1: IF left > ptr THEN GOSUB writeall: left = left - 1: GOSUB visnotes
IF keyval(77) > 0 AND ptr < 32767 THEN ptr = ptr + 1: IF left + 150 < ptr THEN GOSUB writeall: left = left + 1: GOSUB visnotes
IF keyval(73) > 1 THEN ptr = large(ptr - mes, 0): IF left > ptr THEN GOSUB writeall: left = large(left - mes, 0): GOSUB visnotes
IF keyval(81) > 1 THEN ptr = small(ptr + mes, 32767): IF left + 150 < ptr THEN GOSUB writeall: left = small(left + mes, 32767): GOSUB visnotes
IF keyval(72) > 0 AND vptr < 127 THEN
 vptr = vptr + 1: IF ny(vptr) = -1 THEN vptr = vptr + 1
 noten = noten + 1
 IF noten > 6 THEN noten = 0
END IF
IF keyval(80) > 0 AND vptr > 2 THEN
 vptr = vptr - 1: IF ny(vptr) = -1 THEN vptr = vptr - 1
 noten = noten - 1
 IF noten < 0 THEN noten = 6
END IF
IF keyval(51) > 1 THEN nl = nl - 1: IF nl < 0 THEN nl = 7
IF keyval(52) > 1 THEN nl = nl + 1: IF nl > 7 THEN nl = 0
IF keyval(45) > 1 AND keyval(29) > 0 THEN GOSUB clearall
FOR i = 2 TO 10
 IF keyval(i) > 1 THEN ins = i - 2
NEXT i
sharp = 0: IF keyval(56) > 0 AND noten <> 2 AND noten <> 6 THEN sharp = 1
dot! = 1: IF keyval(29) > 0 THEN dot! = 1.5
IF keyval(57) > 1 THEN GOSUB setnote
IF keyval(83) > 1 AND keyval(29) = 0 THEN FOR o = vptr TO vptr + 1: GOSUB delnote: NEXT o: GOSUB visnotes
IF keyval(83) > 1 AND keyval(29) > 0 THEN FOR o = 1 TO 127: GOSUB delnote: NEXT o: GOSUB visnotes
IF keyval(15) > 1 THEN
 IF mes = 32 THEN mes = 24: GOTO no15
 IF mes = 24 THEN mes = 32: GOTO no15
END IF
no15:
IF keyval(28) > 1 THEN GOSUB playscr
IF (keyval(12) > 1 OR keyval(74) > 1) AND cinst(ins) > 0 THEN
 cinst(ins) = cinst(ins) - 1
 cinst$(ins) = inst$(cinst(ins))
 getvoice ins, cinst(ins), "ibank.ibk" + CHR$(0), buffer()
 fmkeyon ins, vptr + sharp: noff(ins) = nlen(nl)
END IF
IF (keyval(13) > 1 OR keyval(78) > 1) AND cinst(ins) < 127 THEN
 cinst(ins) = cinst(ins) + 1
 cinst$(ins) = inst$(cinst(ins))
 getvoice ins, cinst(ins), "ibank.ibk" + CHR$(0), buffer()
 fmkeyon ins, vptr + sharp: noff(ins) = nlen(nl)
END IF
IF keyval(60) > 1 THEN GOSUB savesong: GOSUB visnotes
IF keyval(61) > 1 THEN GOSUB readsong: GOSUB visnotes
IF keyval(62) > 1 THEN GOSUB parsesong: GOSUB visnotes
IF keyval(43) > 1 THEN last = ptr
IF keyval(20) > 1 THEN GOSUB puttag
IF keyval(46) > 1 THEN GOSUB killtag
IF keyval(38) > 1 THEN GOSUB loopage
RETURN

loopage:
jn = 1: jp = 0
jmenu$(0) = "Place Loop *" + STR$(jn)
jmenu$(1) = "Place Infinite Loop"
jmenu$(2) = "Place Gosub"
jmenu$(3) = "Place Return"
copypage vpage, dpage
textcolor 240, 0
rectangle 0, 0, 320, 40, 7, dpage
noloop = 0: j = 99
FOR i = 99 TO 0 STEP -1
 IF jump(i) = 0 THEN j = i
NEXT i
setkeys
DO
 setwait timing(), 62
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN noloop = 2
 IF keyval(72) > 1 THEN jp = jp - 1: IF jp < 0 THEN jp = 3
 IF keyval(80) > 1 THEN jp = jp + 1: IF jp > 3 THEN jp = 0
 IF keyval(28) > 1 OR keyval(57) > 1 THEN noloop = 1
 IF jp = 0 THEN
  IF keyval(75) > 1 THEN jn = large(jn - 1, 1)
  IF keyval(77) > 1 THEN jn = small(jn + 1, 253)
 END IF
 jmenu$(0) = "Place Finite Loop:" + STR$(jn)
 FOR i = 0 TO 3
  textcolor 240, 0
  IF jp = i THEN textcolor 15 + (tog * 225), 0
  printstr jmenu$(i), 1, 1 + i * 8, dpage
 NEXT i
 copypage dpage, vpage
 rectangle 0, 0, 320, 40, 7, dpage
 dowait
LOOP UNTIL noloop > 0
IF noloop = 2 THEN RETURN
jump(j) = ptr
jid(j) = 0
j2tag(j) = 0
IF jp = 0 THEN jid(j) = jn: jmenu$(0) = "Jump" + STR$(jn) + " Times to Tag:"
IF jp = 1 THEN jid(j) = 254: jmenu$(0) = "Loop Infinately to Tag:"
IF jp = 2 THEN jid(j) = 255: jmenu$(0) = "Gosub to Tag:"
IF jp = 3 THEN jid(j) = -1: RETURN
textcolor 240, 0
t$ = "0"
noloop = 0
setkeys
DO
 setwait timing(), 62
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN noloop = 2
 FOR i = 2 TO 11
  IF keyval(i) > 1 AND LEN(t$) < 2 THEN t$ = t$ + kp$(i)
 NEXT i
 IF keyval(14) > 1 AND LEN(t$) > 0 THEN t$ = LEFT$(t$, LEN(t$) - 1)
 IF keyval(28) > 1 AND LEN(t$) > 0 THEN
  IF VAL(t$) > -1 AND VAL(t$) < 16 THEN noloop = 1
  IF VAL(t$) < 0 OR VAL(t$) > 15 THEN t$ = ""
 END IF
 printstr jmenu$(0) + t$, 1, 1, dpage
 copypage dpage, vpage
 rectangle 0, 0, 320, 40, 7, dpage
 dowait
LOOP UNTIL noloop > 0
IF noloop = 2 THEN jump(j) = 0: RETURN
FOR i = 0 TO 99
IF j2tag(i) = VAL(t$) THEN jump(i) = 0
NEXT i
j2tag(j) = VAL(t$)
RETURN

puttag:
copypage vpage, dpage
textcolor 240, 0
rectangle 0, 0, 320, 40, 7, dpage
noloop = 0: t$ = ""
FOR i = 15 TO 1 STEP -1
 IF tag(i) = 0 THEN t$ = RIGHT$(STR$(i), LEN(STR$(i)) - 1)
NEXT i
setkeys
DO
 setwait timing(), 62
 setkeys
 IF keyval(1) > 1 THEN noloop = 2
 FOR i = 2 TO 11
  IF keyval(i) > 1 AND LEN(t$) < 2 THEN t$ = t$ + kp$(i)
 NEXT i
 IF keyval(14) > 1 AND LEN(t$) > 0 THEN t$ = LEFT$(t$, LEN(t$) - 1)
 IF keyval(28) > 1 AND LEN(t$) > 0 THEN
  IF VAL(t$) > 0 AND VAL(t$) < 16 THEN noloop = 1
  IF VAL(t$) < 1 OR VAL(t$) > 15 THEN t$ = ""
 END IF
 IF keyval(28) > 1 AND LEN(t$) = 0 THEN noloop = 2
 printstr "Place tag of number:", 1, 1, dpage
 printstr t$, 1, 9, dpage
 copypage dpage, vpage
 rectangle 0, 0, 320, 40, 7, dpage
 dowait
LOOP UNTIL noloop > 0
IF noloop = 2 THEN RETURN
tag(VAL(t$)) = ptr
RETURN

killtag:
copypage vpage, dpage
textcolor 240, 0
rectangle 0, 0, 320, 40, 7, dpage
noloop = 0: t$ = ""
setkeys
DO
 setwait timing(), 62
 setkeys
 IF keyval(1) > 1 THEN noloop = 2
 FOR i = 2 TO 11
  IF keyval(i) > 1 AND LEN(t$) < 2 THEN t$ = t$ + kp$(i)
 NEXT i
 IF keyval(14) > 1 AND LEN(t$) > 0 THEN t$ = LEFT$(t$, LEN(t$) - 1)
 IF keyval(28) > 1 AND LEN(t$) > 0 THEN
  IF VAL(t$) > 0 AND VAL(t$) < 16 THEN noloop = 1
  IF VAL(t$) < 1 OR VAL(t$) > 15 THEN t$ = ""
 END IF
 IF keyval(28) > 1 AND LEN(t$) = 0 THEN noloop = 2
 printstr "Clear tag of number:", 1, 1, dpage
 printstr t$, 1, 9, dpage
 copypage dpage, vpage
 rectangle 0, 0, 320, 40, 7, dpage
 dowait
LOOP UNTIL noloop > 0
IF noloop = 2 THEN RETURN
tag(VAL(t$)) = 0
RETURN

parsesong:
copypage vpage, dpage
textcolor 240, 0
rectangle 0, 0, 320, 40, 7, dpage
noloop = 0
setkeys
DO
 setwait timing(), 62
 setkeys
 IF keyval(1) > 1 THEN noloop = 2
 FOR i = 2 TO 50
  IF keyval(i) > 1 AND LEN(f$) < 8 AND kp$(i) <> "" THEN f$ = f$ + kp$(i)
 NEXT i
 IF keyval(14) > 1 AND LEN(f$) > 0 THEN f$ = LEFT$(f$, LEN(f$) - 1)
 IF keyval(28) > 1 AND LEN(f$) > 0 THEN noloop = 1
 IF keyval(28) > 1 AND LEN(f$) = 0 THEN noloop = 2
 printstr "Create BAM file of name:", 1, 1, dpage
 printstr f$, 1, 9, dpage
 copypage dpage, vpage
 rectangle 0, 0, 320, 40, 7, dpage
 dowait
LOOP UNTIL noloop > 0
IF noloop = 2 THEN RETURN
OPEN f$ + ".bam" FOR BINARY AS #1
IF LOF(1) > 0 THEN
 CLOSE #1
 KILL f$ + ".bam"
 OPEN f$ + ".bam" FOR BINARY AS #1
END IF
a$ = "CBMF"
PUT #1, 1, a$
OPEN "ibank.ibk" FOR BINARY AS #2
FOR i = 0 TO 8
 a$ = CHR$(48 + i)
 PUT #1, 5 + i * 12, a$
 a$ = STRING$(11, 0)
 GET #2, 5 + cinst(i) * 16, a$
 PUT #1, 6 + i * 12, a$
NEXT i
CLOSE #2
a$ = CHR$(80)
PUT #1, 113, a$
p& = 114
OPEN file$ FOR BINARY AS #2
d = 0
FOR i = 0 TO last
 FOR o = 0 TO 8
  a$ = CHR$(0)
  GET #2, 1 + (i * 9) + o, a$
  b = ASC(a$)
  IF (b AND 128) = 128 THEN
   IF d > 0 THEN
    a$ = CHR$(128 + (d - 1)): d = 0
    PUT #1, p&, a$
    p& = p& + 1
   END IF
   a$ = CHR$(32 + o)
   PUT #1, p&, a$
   p& = p& + 1
  END IF
 NEXT o
 FOR o = 0 TO 99
  IF jump(o) = i AND jump(o) > 0 THEN
   IF jid(o) > 0 THEN
    a$ = CHR$(96 + j2tag(o)) + CHR$(jid(o))
    PUT #1, p&, a$
    p& = p& + 2
   END IF
   IF jid(o) = -1 THEN
    a$ = CHR$(112)
    PUT #1, p&, a$
    p& = p& + 1
   END IF
  END IF
 NEXT o
 FOR o = 1 TO 15
  IF tag(o) = i AND tag(o) > 0 THEN
   a$ = CHR$(80 + o)
   PUT #1, p&, a$
   p& = p& + 1
  END IF
 NEXT o
 FOR o = 0 TO 8
  a$ = CHR$(0)
  GET #2, 1 + (i * 9) + o, a$
  b = ASC(a$)
  IF (b AND 127) > 0 THEN
   IF d > 0 THEN
    a$ = CHR$(128 + (d - 1)): d = 0
    PUT #1, p&, a$
    p& = p& + 1
   END IF
   a$ = CHR$(16 + o)
   PUT #1, p&, a$
   p& = p& + 1
   a$ = CHR$(b AND 127)
   PUT #1, p&, a$
   p& = p& + 1
  END IF
 NEXT o
 d = d + 1
 IF d > 127 THEN
  a$ = CHR$(128 + (d - 1)): d = 0
  PUT #1, p&, a$
  p& = p& + 1
 END IF
NEXT i
IF d > 0 THEN
 a$ = CHR$(128 + (d - 1)): d = 0
 PUT #1, p&, a$
 p& = p& + 1
END IF
CLOSE #2
a$ = CHR$(96 + 0) + CHR$(254) + CHR$(0)
PUT #1, p&, a$
CLOSE #1
RETURN

savesong:   
copypage vpage, dpage
textcolor 240, 0
rectangle 0, 0, 320, 40, 7, dpage
noloop = 0: f$ = ""
setkeys
DO
 setwait timing(), 62
 setkeys
 IF keyval(1) > 1 THEN noloop = 2
 FOR i = 2 TO 50
  IF keyval(i) > 1 AND LEN(f$) < 8 AND kp$(i) <> "" THEN f$ = f$ + kp$(i)
 NEXT i
 IF keyval(14) > 1 AND LEN(f$) > 0 THEN f$ = LEFT$(f$, LEN(f$) - 1)
 IF keyval(28) > 1 AND LEN(f$) > 0 THEN noloop = 1
 IF keyval(28) > 1 AND LEN(f$) = 0 THEN noloop = 2
 printstr "Save as:", 1, 1, dpage
 printstr f$, 1, 9, dpage
 copypage dpage, vpage
 rectangle 0, 0, 320, 40, 7, dpage
 dowait
LOOP UNTIL noloop > 0
IF noloop = 2 THEN RETURN
OPEN file$ FOR BINARY AS #1
FOR i = 0 TO 8
 a$ = CHR$(cinst(i))
 PUT #1, 32770 + i, a$
NEXT i
PUT #1, 32770 + 9, last
FOR i = 1 TO 15
 PUT #1, 32770 + 11 + i * 2, tag(i)
NEXT i
FOR i = 0 TO 99
 PUT #1, 32770 + 45 + (i * 6 + 0), jump(i)
 PUT #1, 32770 + 45 + (i * 6 + 2), jid(i)
 PUT #1, 32770 + 45 + (i * 6 + 4), j2tag(i)
NEXT i
CLOSE #1
copyfile file$ + CHR$(0), f$ + ".not" + CHR$(0), buffer()
RETURN

readsong:
copypage vpage, dpage
textcolor 240, 0
rectangle 0, 0, 320, 40, 7, dpage
noloop = 0: f$ = ""
setkeys
DO
 setwait timing(), 62
 setkeys
 IF keyval(1) > 1 THEN noloop = 2
 FOR i = 2 TO 50
  IF keyval(i) > 1 AND LEN(f$) < 8 AND kp$(i) <> "" THEN f$ = f$ + kp$(i)
 NEXT i
 IF keyval(14) > 1 AND LEN(f$) > 0 THEN f$ = LEFT$(f$, LEN(f$) - 1)
 IF keyval(28) > 1 AND LEN(f$) > 0 THEN noloop = 1
 IF keyval(28) > 1 AND LEN(f$) = 0 THEN noloop = 2
 printstr "Load file:", 1, 1, dpage
 printstr f$, 1, 9, dpage
 copypage dpage, vpage
 rectangle 0, 0, 320, 40, 7, dpage
 dowait
LOOP UNTIL noloop > 0
IF noloop = 2 THEN RETURN
copyfile f$ + ".not" + CHR$(0), file$ + CHR$(0), buffer()
OPEN file$ FOR BINARY AS #1
FOR i = 0 TO 8
 a$ = " "
 GET #1, 32770 + i, a$
 cinst(i) = ASC(a$)
 cinst$(i) = inst$(cinst(i))
 getvoice i, cinst(i), "ibank.ibk" + CHR$(0), buffer()
NEXT i
GET #1, 32770 + 9, last
FOR i = 1 TO 15
 GET #1, 32770 + 11 + i * 2, tag(i)
NEXT i
FOR i = 0 TO 99
 GET #1, 32770 + 45 + (i * 6 + 0), jump(i)
 GET #1, 32770 + 45 + (i * 6 + 2), jid(i)
 GET #1, 32770 + 45 + (i * 6 + 4), j2tag(i)
NEXT i
CLOSE #1
left = 0: ptr = 0
RETURN

delnote:
kiln = 0
OPEN file$ FOR BINARY AS #1
FOR i = 0 TO 8
 a$ = " "
 GET #1, 1 + (ptr * 9) + i, a$
 b = ASC(a$)
 IF o = (b AND 127) THEN
  kiln = 1
  a$ = " "
  GET #1, 1 + (ptr * 9) + i, a$
  b = ASC(a$)
  n(ptr - left, i) = 0
  IF (b AND 128) = 128 THEN n(ptr - left, i) = (n(ptr - left, i) OR 128)
  a$ = CHR$(n(ptr - left, i))
  PUT #1, 1 + (ptr * 9) + i, a$
  a$ = " "
  GET #1, 1 + ((ptr + l(ptr - left, i)) * 9) + i, a$
  b = (ASC(a$) AND 127)
  a$ = CHR$(b)
  PUT #1, 1 + ((ptr + l(ptr - left, i)) * 9) + i, a$
  j = ptr + l(ptr - left, i): noloop = 0
  DO
   j = j - 1
   GET #1, 1 + (j * 9) + i, a$
   b = ASC(a$)
   IF (b AND 127) > 0 THEN noloop = 1
   IF (b AND 128) = 128 THEN noloop = 2
   IF j < 1 OR j < ptr + l(ptr - left, i) - 127 THEN noloop = 2
  LOOP UNTIL noloop > 0
  IF noloop = 1 THEN
   GET #1, 1 + ((ptr + l(ptr - left, i)) * 9) + i, a$
   b = (ASC(a$) OR 128)
   a$ = CHR$(b)
   PUT #1, 1 + ((ptr + l(ptr - left, i)) * 9) + i, a$
  END IF
  l(ptr - left, i) = 0
 END IF
NEXT i
CLOSE #1
IF kiln = 1 THEN RETURN
FOR i = 1 TO 15
 IF tag(i) = ptr AND ptr > 0 THEN tag(i) = 0
NEXT i
FOR i = 0 TO 99
 IF jump(i) = ptr AND ptr > 0 THEN jump(i) = 0
NEXT i
RETURN

setnote:
fmkeyon ins, vptr + sharp: noff(ins) = nlen(nl)
OPEN file$ FOR BINARY AS #1
a$ = " "
GET #1, 1 + (ptr * 9) + ins, a$
b = ASC(a$)
n(ptr - left, ins) = vptr + sharp
l(ptr - left, ins) = nlen(nl) * dot!
IF (b AND 128) = 128 THEN n(ptr - left, ins) = (n(ptr - left, ins) OR 128)
a$ = CHR$(n(ptr - left, ins))
PUT #1, 1 + (ptr * 9) + ins, a$
FOR i = ptr + 1 TO (ptr + (nlen(nl) * dot!)) - 1
 a$ = CHR$(0)
 PUT #1, 1 + (i * 9) + ins, a$
NEXT i
a$ = " "
GET #1, 1 + ((ptr + (nlen(nl) * dot!)) * 9) + ins, a$
b = (ASC(a$) OR 128)
a$ = CHR$(b)
PUT #1, 1 + ((ptr + (nlen(nl) * dot!)) * 9) + ins, a$
CLOSE #1
IF ptr + (nlen(nl) * dot!) > last THEN last = ptr + (nlen(nl) * dot!) + 2
 GOSUB visnotes
RETURN

visnotes:
a$ = " "
OPEN file$ FOR BINARY AS #1
FOR i = left TO left + 150
 FOR o = 0 TO 8
  GET #1, 1 + (i * 9) + o, a$
  n(i - left, o) = ASC(a$)
  l(i - left, o) = 0
  IF n(i - left, o) > 0 THEN
   j = i: noloop = 0
   DO
    j = j + 1
    GET #1, 1 + (j * 9) + o, a$
    b = ASC(a$)
    IF (b AND 128) = 128 THEN noloop = 1
    IF j - i > 128 THEN noloop = 2
   LOOP UNTIL noloop > 0
   IF noloop = 1 THEN
    l(i - left, o) = j - i
   END IF
  END IF
 NEXT o
NEXT i
CLOSE #1
RETURN

writeall:
OPEN file$ FOR BINARY AS #1
FOR i = left TO left + 150
FOR o = 0 TO 8
a$ = CHR$(n(i - left, o))
PUT #1, 1 + (i * 9) + o, a$
NEXT o
NEXT i
CLOSE #1
RETURN

clearall:
FOR i = 0 TO 8
 getvoice i, 0, "ibank.ibk" + CHR$(0), buffer()
 cinst$(i) = inst$(0)
 cinst(i) = 0
NEXT i
FOR i = 1 TO 15
tag(i) = 0
NEXT i
FOR i = 0 TO 99
jump(i) = 0
NEXT i
a$ = STRING$(100, 0)
OPEN file$ FOR BINARY AS #1
FOR i& = 0 TO 32767 STEP 100
PUT #1, 1 + i&, a$
NEXT i&
CLOSE #1
FOR i = 0 TO 150
FOR o = 0 TO 8
n(i, o) = 0
l(i, o) = 0
NEXT o
NEXT i
ptr = 0
left = 0
last = 0
RETURN

display:
textcolor 240, 0
printstr "Offset:" + STR$(ptr), 1, 1, dpage
printstr nlen$(nl) + " note - " + note$(noten) + sharp$(sharp), 1, 9, dpage
rectangle 1, 17, (nlen(nl) * 2) * dot!, 2, col(ins) - (4 * sharp), dpage
printstr "Voice:" + cinst$(ins), 1, 20, dpage
FOR i = left TO left + 150
 IF INT(i / 8) * 8 = (i / 8) * 8 THEN
  rectangle 2 * (i - left) + 10, 110, 1, 16, 8, dpage
  rectangle 2 * (i - left) + 10, 134, 1, 16, 8, dpage
 END IF
 IF INT(i / mes) * mes = (i / mes) * mes THEN rectangle 2 * (i - left) + 10, 108, 1, 45, 8, dpage
 IF i = last THEN rectangle 2 * (i - left) + 10, 40, 2, 160, 15, dpage
 FOR o = 1 TO 15
  IF tag(o) = i AND i > 0 THEN rectangle 2 * (i - left) + 10, 40, 1, 160, 1, dpage: : textcolor 9, 0: printstr STR$(o), 2 * (i - left) + 2, 33 + (o * 8), dpage
 NEXT o
 FOR o = 0 TO 99
  IF jump(o) = i AND i > 0 THEN
   j$ = "L" + RIGHT$(STR$(jid(o)), LEN(STR$(jid(o))) - 1): jj$ = STR$(j2tag(o))
   IF jid(o) = -1 THEN j$ = "RET": jj$ = ""
   IF jid(o) = 254 THEN j$ = "Lþ"
   IF jid(o) = 255 THEN j$ = "GOS"
   rectangle 2 * (i - left) + 10, 40, 1, 160, 2, dpage: : textcolor 10, 0: printstr j$, 2 * (i - left) + 10, 41, dpage: printstr jj$, 2 * (i - left) + 2, 49, dpage
  END IF
 NEXT o
NEXT i
FOR i = 0 TO 150
 FOR o = 0 TO 8
  flat = 0: IF ny(n(i, o) AND 127) = -1 THEN flat = 1
  IF (n(i, o) AND 127) > 0 THEN rectangle 10 + i * 2, 200 - (ny((n(i, o) AND 127) - flat)) * 2, l(i, o) * 2, 2, col(o) - (4 * flat), dpage: rectangle 10 + i * 2, 200 - (ny((n(i, o) AND 127) - flat)) * 2, 1, 2, col(o) + 2 - (4 * flat), dpage
 NEXT o
NEXT i
rectangle 2 * (ptr - left) + 10, 40, 1, 160, 4 + tog * 8, dpage
rectangle 2 * (ptr - left) + 10, 200 - (ny(vptr) * 2) - 1, 3, 1, 4 + tog * 8, dpage
rectangle 2 * (ptr - left) + 10, 200 - (ny(vptr) * 2) + 2, 3, 1, 4 + tog * 8, dpage
SWAP vpage, dpage
setvispage vpage
copypage 2, dpage
RETURN

finis:
OPEN file$ FOR BINARY AS #1
FOR i = 0 TO 8
 a$ = CHR$(cinst(i))
 PUT #1, 32770 + i, a$
NEXT i
PUT #1, 32770 + 9, last
FOR i = 1 TO 15
 PUT #1, 32770 + 11 + i * 2, tag(i)
NEXT i
FOR i = 0 TO 99
 PUT #1, 32770 + 45 + (i * 6 + 0), jump(i)
 PUT #1, 32770 + 45 + (i * 6 + 2), jid(i)
 PUT #1, 32770 + 45 + (i * 6 + 4), j2tag(i)
NEXT i
CLOSE #1
GOSUB shutoff
SCREEN 13
SYSTEM

switchon:
regs.ax = &H2509: regs.ds = Keyseg: regs.dx = keyoff
CALL interruptx(&H21, regs, regs)
RETURN

shutoff:
regs.ax = &H2509: regs.ds = seg9: regs.dx = off9
CALL interruptx(&H21, regs, regs)
RETURN

DATA 43,47,50,53,57,64,67,71,74,77
DATA C,D,E,F,G,A,B
DATA 1,2,4,8,16,32,64,128
DATA Thirtysecond,Sixteenth,Eighth,Quarter,Half,Whole,Double,Quadriple
DATA 29,45,61,77,93,109,125,141,157

'    C  C# D  D# E  F  F# G  G# A  A# B
DATA  0,-1, 1,-1, 2, 3,-1, 4,-1, 5,-1, 6
DATA  7,-1, 8,-1, 9,10,-1,11,-1,12,-1,13
DATA 14,-1,15,-1,16,17,-1,18,-1,19,-1,20
DATA 21,-1,22,-1,23,24,-1,25,-1,26,-1,27
DATA 28,-1,29,-1,30,31,-1,32,-1,33,-1,34
DATA 35,-1,36,-1,37,38,-1,39,-1,40,-1,41
DATA 42,-1,43,-1,44,45,-1,46,-1,47,-1,48
DATA 49,-1,50,-1,51,52,-1,53,-1,54,-1,55
DATA 56,-1,57,-1,58,59,-1,60,-1,61,-1,62
DATA 63,-1,64,-1,65,66,-1,67,-1,68,-1,69
DATA 70,-1,71,-1,72,73,-1,74

DATA 1,2,3,4,5,6,7,8,9,0,_,,,,Q,W,E,R,T,Y,U,I,O,P,,,,,A,S,D,F,G,H,J,K,L,,,"~",,,Z,X,C,V,B,N,M

REM $STATIC
FUNCTION large (n1, n2)
large = n1
IF n2 > n1 THEN large = n2
END FUNCTION

FUNCTION small (n1, n2)
small = n1
IF n2 < n1 THEN small = n2
END FUNCTION

