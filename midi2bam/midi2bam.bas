'MIDI2BAM - Music File Format Converter
'(C) Copyright 1997 James Paige, Brian Fisher, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
' 
'$DYNAMIC
DEFINT A-Z
'Basic Sub
DECLARE FUNCTION getfour& (p&)
DECLARE SUB sortstrings (st$(), num)
'General Mode-X Stuff
DECLARE SUB setmodex ()
DECLARE SUB restoremode
DECLARE SUB copypage (BYVAL page1, BYVAL page2)
DECLARE SUB setvispage (BYVAL page)
DECLARE SUB clearpage (BYVAL page)
'Page stuff
DECLARE SUB setdiskpages (buf(), BYVAL l, BYVAL b)
DECLARE SUB storepage (fil$, BYVAL i, BYVAL p)
DECLARE SUB loadpage (fil$, BYVAL i, BYVAL p)
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
'Adlib musica
DECLARE SUB setupmusic (b())
DECLARE SUB closemusic ()
DECLARE SUB resetfm ()
DECLARE SUB fmkeyon (BYVAL v, BYVAL n)
DECLARE SUB fmkeyoff (BYVAL v)
DECLARE SUB getvoice (BYVAL v, BYVAL io, f$, b())
DECLARE SUB setvoice (BYVAL v, b())
DECLARE FUNCTION getfmvol ()
DECLARE SUB setfmvol (BYVAL vol)
'Keyhandling
DECLARE SUB setkeys ()
DECLARE FUNCTION Keyseg ()
DECLARE FUNCTION keyoff ()
DECLARE FUNCTION keyval (BYVAL a)
DECLARE FUNCTION getkey ()
'filestuf
DECLARE SUB findfiles (n$, BYVAL m, o$, b())
DECLARE SUB getpath (p$)
DECLARE FUNCTION pathlength ()
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
DIM font(1024), master(800), buf(16384), timing(1), mouse(4)
DIM inst$(127), note(9), value(9), chan(15), ins(9)
DIM keymap(127), keyson(127), skey$(55)
DIM song(32000), rec(16000), tagp(16), rep(16), inst(6), track(6)
DIM bamuse(15), vinst(15, 6)
DIM dirs$(256), bam$(256)
DIM dispx(15), dispn(15), dispt(15), dispi(15), dispc(15), dispfi(15)
DIM midc(15), midi(15), midn(15)
DIM mtrk&(15), mtrkd&(15), mtrks&(15), mtrkl&(15), mtrke(15), mtrkend(15), mtrki(15)
DIM offn(32), offc(32), offs(32), onn(32), onc(32), ont(32)
DIM inson(15), vins(15)
dtempo& = 439560
midistep = 16
smartmode = 0
disptype = 2

inson(9) = -1
songl = 0
songp = 0
playsong = 0
setdiskpages buf(), 200, 0
lowk = 0: highk = 8
inst = 0
fadeto buf(), 0, 0, 0
setmodex
setpal master()
oldfm = getfmvol
fmvol = 15
temp = pathlength
path$ = STRING$(temp, 0)
getpath path$
fmask$ = "*.mid" + CHR$(0)
GOSUB getdir
resetfm
FOR i = 2 TO 53
 READ skey$(i)
NEXT i
ibank$ = "ibank.ibk"
OPEN ibank$ FOR BINARY AS #1
FOR i = 0 TO 127
 inst$(i) = STRING$(9, 0)
 GET #1, 2053 + i * 9, inst$(i)
NEXT i
CLOSE #1
DEF SEG = VARSEG(font(0)): BLOAD "text.fnt", VARPTR(font(0))
setfont font()
DEF SEG = VARSEG(master(0)): BLOAD "palette.sto", VARPTR(master(0))
GOSUB switchon
RANDOMIZE TIMER
vpage = 0: dpage = 1: delay = 56
showkeys = 0
setfmvol fmvol
GOSUB setdefault
GOSUB setins

setvispage 0
fadetopal master(), buf()
exitprog = 0
WHILE exitprog = 0
setwait timing(), delay
copypage 2, dpage
setkeys
IF keyval(1) > 1 THEN IF bamlook <> 0 THEN bamlook = 0 ELSE exitprog = 1
IF keyval(57) > 1 THEN disptype = disptype + 1: IF disptype > 3 THEN disptype = 0
IF keyval(74) > 1 AND fmvol > 1 THEN fmvol = fmvol - 1: setfmvol fmvol
IF keyval(75) > 1 AND ipnt > 0 THEN ipnt = ipnt - 1
IF keyval(77) > 1 AND ipnt < 16 THEN ipnt = ipnt + 1
IF keyval(2) > 1 THEN IF inson(ipnt) = -1 THEN inson(ipnt) = 0 ELSE inson(ipnt) = -1
IF keyval(3) > 1 THEN smartmode = smartmode XOR 1
IF keyval(78) > 1 AND fmvol < 15 THEN fmvol = fmvol + 1: setfmvol fmvol
IF keyval(62) > 1 THEN GOSUB newsong
IF keyval(63) > 1 THEN IF playsong = 1 THEN playsong = 0: GOSUB stopsong ELSE playsong = 1: songp = 0
IF keyval(61) > 1 AND bamlook = 0 THEN
 GOSUB getdir: bamlook = 1
END IF
IF keyval(60) > 1 AND bamlook = 0 THEN bamlook = -1
IF playsong = 1 AND songl > 0 THEN GOSUB playtick
IF bamlook > 0 THEN GOSUB findbam
IF bamlook = -1 THEN GOSUB savebam
IF bamlook = 0 THEN GOSUB display
FOR i = 0 TO 15
IF ipnt = i THEN textcolor 14, 12 + i * 16 ELSE textcolor 15, 12 + i * 16
IF inson(i) = -1 THEN printstr "X", i * 20 + 8, 180, dpage ELSE printstr "O", i * 20 + 8, 180, dpage
NEXT i

textcolor 15, 0
printstr STR$(smartmode), 240, 170, dpage
FOR i = 1 TO fmvol
 rectangle 250 + i * 4, 170, 3, 10, 10, dpage
NEXT i
SWAP vpage, dpage
setvispage vpage
dowait
WEND
GOTO quit

writedelay:
IF recd > 0 THEN
 rec(recp) = recd - 1 + 128
 recp = recp + 1
 recd = 0
END IF
RETURN

playtick:
textcolor 14, 4: printstr "PLAY", 62, 172, dpage
IF waitt > 0 THEN waitt = waitt - 1
WHILE waitt = 0
 IF songp > songl THEN songp = 0
 i = song(songp)
 temp = INT(i / 16)
 songp = songp + 1
 SELECT CASE temp
  CASE 0
   songp = 0
   waitt = 1
  CASE 1
   j = song(songp + 2)
   k = song(songp)
   songp = songp + 3
   i = i AND 15
   chan(i) = k
   IF i < 10 AND inson(k) <> -1 THEN
    value(i) = -1
    note(i) = j
    fmkeyon i, j
   END IF
  CASE 2
   i = i AND 15
   IF i < 10 THEN
    value(i) = 0
    fmkeyoff i
    IF recsong THEN
     GOSUB writedelay
     rec(recp) = 32 + i
     recp = recp + 1
    END IF
   END IF
  CASE 3
   i = i AND 15
   songp = songp + 1
   FOR j = 0 TO 5
    inst(j) = song(songp)
    IF bamuse(i) = 1 THEN vinst(i, j) = song(songp)
    songp = songp + 1
   NEXT j
   IF bamuse(i) = 1 AND i < 10 THEN setvoice i, inst()
  CASE 5
   i = i AND 15
   tagp(i) = songp
  CASE 6
   i = i AND 15
   j = song(songp)
   songp = songp + 1
   IF j = 255 THEN retp = songp: songp = tagp(i)
   IF j = 254 THEN songp = tagp(i)
   IF j < 254 THEN
    IF rep(i) = 1 THEN
     rep(i) = 0
    ELSEIF rep(i) = 0 THEN
     rep(i) = j + 1
     songp = tagp(i)
    ELSE
     rep(i) = rep(i) - 1
     songp = tagp(i)
    END IF
   END IF
  CASE 7
   IF retp <> 0 THEN songp = retp: retp = 0
  CASE IS > 7
   waitt = (i AND 127) + 1
 END SELECT
WEND
RETURN

getdir:
IF RIGHT$(path$, 1) = CHR$(0) THEN path$ = LEFT$(path$, LEN(path$) - 1)
findfiles path$ + fmask$ + CHR$(0), 32, "files.lst" + CHR$(0), buf()
fpoint = 0: fstart = 0: dpoint = 0: dstart = 0: temp = 0
OPEN "files.lst" FOR INPUT AS #1
WHILE EOF(1) = 0 AND temp < 256
 INPUT #1, bam$(temp)
 temp = temp + 1
WEND
CLOSE #1
sortstrings bam$(), temp
fmatch = temp
bam$(temp) = ""
findfiles path$ + "*.*" + CHR$(0), 16, "files.lst" + CHR$(0), buf()
temp = 0
OPEN "files.lst" FOR INPUT AS #1
WHILE EOF(1) = 0 AND temp < 256
 INPUT #1, dirs$(temp)
 IF dirs$(temp) <> "." THEN temp = temp + 1
WEND
CLOSE #1
sortstrings dirs$(), temp
dmatch = temp
dirs$(temp) = ""
RETURN

savebam:
textcolor 14, 1: printstr saven$, 100, 20, dpage
IF keyval(0) = 14 THEN
 IF LEN(saven$) > 0 THEN saven$ = LEFT$(saven$, LEN(saven$) - 1)
ELSEIF keyval(0) = 28 AND saven$ <> "" THEN
 i = 1
 WHILE i < LEN(saven$) AND MID$(saven$, i, 1) <> "."
  i = i + 1
 WEND
 IF MID$(saven$, i, 1) <> "." THEN saven$ = saven$ + ".bam"
 ON ERROR GOTO badfile
 OPEN saven$ FOR BINARY AS #1
 IF LOF(1) > 0 THEN
  CLOSE #1
  KILL saven$
  OPEN saven$ FOR BINARY AS #1
 END IF
 tmp$ = "CBMF"
 PUT #1, 1, tmp$
 i = 0
 k = 5
 bytes = 0
 endsong = 0
 WHILE i < songl
  temp = song(i) AND 255
  tmp$ = CHR$(temp)
  PUT #1, k, tmp$
  k = k + 1
  i = i + 1
  IF bytes = 0 THEN
   IF (temp AND 240) = 16 THEN
    i = i + 2
    bytes = 1
   END IF
   IF (temp AND 240) = 48 THEN
    i = i + 1
    FOR j = 1 TO 6
     PUT #1, k, song(i)
     k = k + 2
     i = i + 1
    NEXT j
    k = k - 1
   END IF
   IF (temp AND 240) = 96 THEN bytes = 1
   IF temp = 96 AND song(i) = 254 THEN endsong = 1
  ELSE
   bytes = bytes - 1
  END IF
 WEND
 IF songl > 2 THEN
 IF endsong = 0 THEN
  temp$ = CHR$(96) + CHR$(254) + CHR$(0)
  PUT #1, k, temp$
 END IF
 END IF
 CLOSE #1
badfile:
 saven$ = ""
 ON ERROR GOTO 0
 bamlook = 0
ELSEIF keyval(0) < 54 THEN
 IF LEN(saven$) < 12 AND skey$(keyval(0)) <> " " THEN saven$ = saven$ + skey$(keyval(0))
END IF
RETURN

findbam:
temp = 0
rectangle 7, 9, 202, 58, 9, dpage
rectangle 8, 10, 200, 56, 1, dpage
rectangle 7, 69, 306, 10, 9, dpage
rectangle 8, 70, 304, 8, 1, dpage
rectangle 215, 11, 98, 50, 9, dpage
rectangle 216, 12, 96, 48, 1, dpage
textcolor 14, 1: printstr RIGHT$(path$, 38), 8, 70, dpage
WHILE temp + fstart < fmatch AND temp < 14
 IF temp + fstart = fpoint AND bamlook = 1 THEN textcolor 14, 9 ELSE textcolor 15, 1
 printstr bam$(temp + fstart), 8 + 104 * (temp AND 1), 10 + INT(temp / 2) * 8, dpage
 temp = temp + 1
WEND
temp = 0
WHILE temp + dstart < dmatch AND temp < 6
 IF temp + dstart = dpoint AND bamlook = 2 THEN textcolor 14, 9 ELSE textcolor 15, 1
 printstr dirs$(temp + dstart), 216, temp * 8 + 12, dpage
 temp = temp + 1
WEND
IF fmatch = 0 THEN bamlook = 2
IF bamlook = 1 THEN
 IF keyval(15) > 1 THEN bamlook = 2
 IF keyval(75) > 1 OR keyval(77) > 1 THEN fpoint = fpoint XOR 1
 IF keyval(72) > 1 AND fpoint > 1 THEN
  fpoint = fpoint - 2
  IF fpoint < fstart THEN fstart = fpoint AND (NOT 1)
 END IF
 IF keyval(80) > 1 THEN
  fpoint = fpoint + 2
  IF fpoint >= fmatch THEN fpoint = fmatch - 1
  IF fpoint - fstart > 13 THEN fstart = fstart + 2
 END IF
 IF keyval(28) > 1 THEN songn$ = path$ + bam$(fpoint): GOSUB loadsong: bamlook = 0
ELSE
 IF keyval(75) > 1 OR keyval(77) > 1 OR keyval(15) > 1 THEN bamlook = 1
 IF keyval(72) > 1 AND dpoint > 0 THEN
  dpoint = dpoint - 1
  IF dpoint < dstart THEN dstart = dpoint
 END IF
 IF keyval(80) > 1 THEN
  dpoint = dpoint + 1
  IF dpoint >= dmatch THEN dpoint = dmatch - 1
  IF dpoint - dstart > 5 THEN dstart = dstart + 1
 END IF
 IF keyval(28) > 1 THEN
  IF dirs$(dpoint) = ".." THEN
   IF LEN(path$) > 0 THEN
    IF MID$(path$, LEN(path$) - 2, 1) = "." THEN
     path$ = path$ + dirs$(dpoint) + "\"
    ELSE
     temp = LEN(path$) - 1
     WHILE MID$(path$, temp, 1) <> "\"
      temp = temp - 1
     WEND
     path$ = LEFT$(path$, temp)
    END IF
   ELSE
    path$ = path$ + dirs$(dpoint) + "\"
   END IF
  ELSE
   path$ = path$ + dirs$(dpoint) + "\"
  END IF
  GOSUB getdir: bamlook = 1
 END IF
END IF
RETURN

loadsong:
OPEN songn$ FOR BINARY AS #1
tag$ = "CBMF"
GET #1, 1, tag$
IF tag$ = "CBMF" THEN
 GOSUB newsong
 ptr = 5
 bytes = 0
 waitt = 0
 WHILE EOF(1) = 0
  GET #1, ptr, temp
  ptr = ptr + 1
  IF temp < 0 THEN temp = 32768 + temp
  temp = temp AND 255
  song(songl) = temp
  songl = songl + 1
  IF bytes = 0 THEN
   IF (temp AND 240) = 16 THEN bytes = 1: bamuse(temp AND 15) = 1
   IF (temp AND 240) = 48 THEN
    FOR i = 1 TO 6
     GET #1, ptr, j
     song(songl) = j
     songl = songl + 1
     ptr = ptr + 2
    NEXT i
    ptr = ptr - 1
   END IF
   IF (temp AND 240) = 96 THEN bytes = 1
  ELSE
   bytes = bytes - 1
  END IF
 WEND
ELSEIF tag$ = "MThd" THEN
 GOSUB newsong
 FOR i = 0 TO 15
  midc(i) = -1:  midi(i) = -1: midn(i) = -1: mtrk&(i) = 1: mtrkd&(i) = 4000000: mtrkend(i) = -1: mtrki(i) = 1
 NEXT i
 pntr& = getfour(5)
 pntr& = pntr& + 9
 GET #1, 12, tracks
 textcolor 14, 5
 printstr STR$(tracks), 80, 170, vpage
 tperq = 0
 GET #1, 13, temp
 IF temp < 0 THEN temp = temp + 32768
 temp = temp AND 255
 tperq = temp * 256
 GET #1, 14, temp
 IF temp < 0 THEN temp = temp + 32768
 temp = temp AND 255
 tperq = tperq + temp
 tempo& = dtempo&
 midistep = CINT(55000 / (tempo& / tperq))
 textcolor 15, 4
 printstr STR$(midistep), 0, 0, vpage
 printstr STR$(tempo&), 100, 0, vpage
 printstr STR$(tperq), 200, 0, vpage
 tag$ = "MTrk"
 i = 0
 IF tracks > 15 THEN tracks = 15
 WHILE i < tracks AND tag$ = "MTrk"
  GET #1, pntr&, tag$
  pntr& = pntr& + 4
  leng& = getfour(pntr&)
  mtrkl&(i) = leng&
  pntr& = pntr& + 4
  mtrk&(i) = pntr&
  mtrks&(i) = pntr&
  pntr& = pntr& + leng&
  mtrkd&(i) = 0
  delay& = 0
  DO
   delay& = delay& * 128
   GET #1, mtrk&(i), temp
   mtrk&(i) = mtrk&(i) + 1
   IF temp < 0 THEN temp = 32768 + temp
   temp = temp AND 255
   delay& = delay& + (temp AND 127)
  LOOP UNTIL (temp AND 128) = 0 OR (delay& > 1600000)
  mtrkd&(i) = delay&
  i = i + 1
 WEND
 tracks = i
 j = 0
 nextv = 0
 lastd& = 0
 midid& = 0
 WHILE j < tracks AND songl < 31900
 noff = 0: non = 0
 FOR i = 0 TO tracks - 1
  WHILE mtrkd&(i) <= midid& AND mtrkend(i) < 2 AND songl < 31900
   textcolor 14, 3: printstr STR$(j), 50, 180, vpage
   setkeys
   IF keyval(1) > 1 THEN mtrkend(i) = 2: j = tracks: setkeys
   GET #1, mtrk&(i), temp
   IF temp < 0 THEN temp = 32768 + temp
   temp = temp AND 255
   IF (temp AND 128) = 128 THEN
    mtrk&(i) = mtrk&(i) + 1
    mtrke(i) = temp
   END IF
   SELECT CASE INT(mtrke(i) / 16)
    CASE 8
     channel = mtrke(i) AND 15
     IF inson(channel) <> -1 THEN
      GET #1, mtrk&(i), temp
      IF temp < 0 THEN temp = 32768 + temp
      temp = temp AND 127
      offn(noff) = temp: offc(noff) = channel: offs(noff) = 0
      noff = noff + 1
     END IF
     mtrk&(i) = mtrk&(i) + 2
    CASE 9
     channel = mtrke(i) AND 15
     IF inson(channel) <> -1 THEN
      GET #1, mtrk&(i) + 1, temp
      IF temp < 0 THEN temp = 32768 + temp
      temp = temp AND 127
      IF temp = 0 THEN
       GET #1, mtrk&(i), temp
       IF temp < 0 THEN temp = 32768 + temp
       temp = temp AND 127
       offn(noff) = temp: offc(noff) = channel: offs(noff) = 0
       noff = noff + 1
      ELSE
       IF smartmode = 1 THEN
        voic = 0
        WHILE ((midc(voic) <> channel OR midn(voic) < 0) AND voic < 9)
         voic = voic + 1
        WEND
        IF voic < 9 THEN
         offn(noff) = midn(voic): offc(noff) = midc(voic): noff = noff + 1: offs(noff) = 0
        END IF
       END IF
       GET #1, mtrk&(i), temp
       IF temp < 0 THEN temp = 32768 + temp
       temp = temp AND 127
       onc(non) = channel: onn(non) = temp: ont(non) = i
       non = non + 1
      END IF
     END IF
     mtrk&(i) = mtrk&(i) + 2
    CASE 10
     mtrk&(i) = mtrk&(i) + 2
    CASE 11
     mtrk&(i) = mtrk&(i) + 2
    CASE 12
     channel = mtrke(i) AND 15
     IF inson(channel) <> -1 THEN
      GET #1, mtrk&(i), temp
      IF temp < 0 THEN temp = 32768 + temp
      temp = temp AND 127
      mtrki(channel) = temp
     END IF
     mtrk&(i) = mtrk&(i) + 1
    CASE 13
     mtrk&(i) = mtrk&(i) + 1
    CASE 14
     mtrk&(i) = mtrk&(i) + 2
    CASE 15
     GET #1, mtrk&(i), temp
     IF temp < 0 THEN temp = 32768 + temp
     temp = temp AND 255
     mtrk&(i) = mtrk&(i) + 1
     IF temp = 81 THEN
      tempo& = 0
      FOR k = 1 TO 3
       GET #1, mtrk&(i) + k, temp
       IF temp < 0 THEN temp = temp + 32768
       temp = temp AND 255
       tempo& = tempo& * 256
       tempo& = tempo& + temp
      NEXT k
      midistep = CINT(55000 / (tempo& / tperq))
      textcolor 15, 4
      printstr STR$(midistep), 0, 0, vpage
      printstr STR$(tempo&), 100, 0, vpage
     END IF
     GET #1, mtrk&(i), temp
     IF temp < 0 THEN temp = 32768 + temp
     temp = temp AND 255
     mtrk&(i) = mtrk&(i) + temp + 1
    CASE ELSE
     mtrk&(i) = mtrk&(i) + 1
     mtrkd&(i) = 1
   END SELECT
   textcolor 254, 15
   printstr STR$(mtrks&(i)), 10, 20 + 8 * i, vpage
   printstr STR$(mtrks&(i) + mtrkl&(i)), 100, 20 + 8 * i, vpage
   printstr STR$(mtrk&(i)), 200, 20 + 8 * i, vpage
   printstr STR$(mtrkend(i)), 300, 20 + 8 * i, vpage

   IF mtrk&(i) - mtrks&(i) >= mtrkl&(i) THEN mtrk&(i) = mtrks&(i): IF mtrkend(i) = 0 THEN mtrkend(i) = 1: j = j + 1 ELSE IF mtrkend(i) = -1 THEN mtrkend(i) = 2: j = j + 1
   delay& = 0
   DO
    delay& = delay& * 128
    GET #1, mtrk&(i), temp
    mtrk&(i) = mtrk&(i) + 1
    IF temp < 0 THEN temp = 32768 + temp
    temp = temp AND 255
    delay& = delay& + (temp AND 127)
   LOOP UNTIL (temp AND 128) = 0 OR delay& > 1600000
   mtrkd&(i) = delay& + mtrkd&(i)
  WEND
  IF mtrkend(i) = -1 THEN mtrkend(i) = 0
 NEXT i
'       TURN OFF VOICES
 FOR i = 0 TO noff - 1
  voic = 0
  WHILE (midn(voic) <> offn(i) OR midc(voic) <> offc(i)) AND voic < 9
   voic = voic + 1
  WEND
  IF voic > 8 THEN
   offs(i) = 1
  ELSE
   midn(voic) = -1
   GOSUB mididelay
   song(songl) = 2 * 16 + voic
   songl = songl + 1
  END IF
 NEXT i
'       TURN ON VOICES
 FOR i = 0 TO non - 1
  voic = 0
  WHILE (midi(voic) <> mtrki(onc(i)) OR midn(voic) > -1) AND voic < 9
   voic = voic + 1
  WEND
  IF voic > 8 THEN
   voic = 0
   WHILE (midn(voic) > -1 OR midi(voic) > -1) AND voic < 9
    voic = voic + 1
   WEND
   IF voic > 8 THEN
    voic = 0
    WHILE midn(voic) > -1 AND voic < 9
     voic = voic + 1
    WEND
   END IF
   IF voic < 9 THEN
    bamuse(voic) = 1
    midi(voic) = mtrki(onc(i))
    GOSUB mididelay
    getvoice 0, midi(voic), ibank$ + CHR$(0), inst()
    song(songl) = voic + 48
    songl = songl + 1
    song(songl) = midi(voic)
    songl = songl + 1
    FOR temp2 = 0 TO 5
     song(songl) = inst(temp2)
     songl = songl + 1
    NEXT temp2
   END IF
  END IF
  IF voic < 9 THEN
   GOSUB mididelay
   midn(voic) = onn(i)
   midc(voic) = onc(i)
   song(songl) = 16 + voic
   songl = songl + 1
   song(songl) = onc(i)
   songl = songl + 1
   song(songl) = ont(i)
   songl = songl + 1
   song(songl) = onn(i)
   songl = songl + 1
  END IF
 NEXT i
'       ADVANCE DELAY
 midid& = midid& + midistep
'       TURN OFF DELAYED STOPS
 FOR i = 0 TO noff - 1
  IF offs(i) = 1 THEN
   offs(i) = 0
   voic = 0
   WHILE (midn(voic) <> offn(i) OR midc(voic) <> offc(i)) AND voic < 9
    voic = voic + 1
   WEND
   IF voic < 9 THEN
    midn(voic) = -1
    GOSUB mididelay
    song(songl) = 2 * 16 + voic
    songl = songl + 1
   END IF
  END IF
 NEXT i
 textcolor 14, 1: printstr STR$(songl), 140, 100, vpage
 WEND
END IF
CLOSE #1
GOSUB setins
RETURN

mididelay:
IF midid& > lastd& AND INT((midid& - lastd&) / midistep) > 0 THEN
 song(songl) = INT((midid& - lastd&) / midistep) - 1 + 128
 songl = songl + 1
 lastd& = midid&
END IF
RETURN

display:
j = songp
FOR i = 0 TO 8
 IF disptype = 2 THEN textcolor 15, 12 + chan(i) * 16 ELSE textcolor 15, i + 1
 dispx(i) = 0
 dispn(i) = 0
 dispt(i) = 0
 dispc(i) = 0
 dispi(i) = dispfi(i)
 IF dispfi(i) >= 0 THEN printstr inst$(dispfi(i)), INT(i / 3) * 100, 140 + 10 * (i - INT(i / 3) * 3), dpage
NEXT i
first = 0
i = 10 + waitt
WHILE i < 310
 k = song(j)
 temp = INT(k / 16)
 j = j + 1
 SELECT CASE temp
  CASE 0
   i = 320
  CASE 1
   k = k AND 15
   dispc(k) = song(j)
   j = j + 1
   dispt(k) = song(j)
   j = j + 1
   l = song(j)
   j = j + 1
   dispx(k) = i
   dispn(k) = l
  CASE 2
   k = k AND 15
   IF dispn(k) <= 127 AND dispn(k) > 0 AND i > dispx(k) THEN
    SELECT CASE disptype
    CASE 0
     rectangle dispx(k), 130 - dispn(k), i - dispx(k), 1, k + 1, dpage
    CASE 1
     rectangle dispx(k), 130 - dispn(k), i - dispx(k), 1, 12 + dispt(k) * 16, dpage
    CASE 2
     rectangle dispx(k), 130 - dispn(k), i - dispx(k), 1, 12 + dispc(k) * 16, dpage
    CASE 3
     rectangle dispx(k), 130 - dispn(k), i - dispx(k), 1, 12 + (dispi(k) AND 15) * 16, dpage
    END SELECT
    dispn(k) = 0
   END IF
  CASE 3
   k = k AND 15
   dispi(k) = song(j)
   IF first = 0 THEN dispfi(k) = song(j)
   j = j + 7
   rectangle i - 1, 132, 3, 2, 12 + k * 16, dpage
  CASE 5
   k = k AND 15
   rectangle i, 132, 1, 2, 15, dpage
   textcolor 15 + k * 16, 0
   printstr STR$(k), i - 8, 132, dpage
  CASE 6
   k = k AND 15
   j = j + 1
   textcolor 12 + k * 16, 0
   printstr STR$(k) + CHR$(14), i - 8, 132, dpage
  CASE 7
   printstr CHR$(15), i - 4, 132, dpage
  CASE IS > 7
   i = i + (k AND 127) + 1
   first = 1
 END SELECT
 IF j >= songl THEN i = 320
WEND
RETURN

transfer:
songp = 0
songl = 0
FOR i = 0 TO 8
 song(songl) = i + 48
 songl = songl + 1
 FOR temp = 0 TO 5
  song(songl) = vinst(i, temp)
  songl = songl + 1
 NEXT temp
NEXT i
playsong = 0
byte = 0
FOR i = 0 TO 8
 bamuse(i) = 0
NEXT i
recl = recp - 1
FOR i = 0 TO recl
 temp = rec(i)
 IF (temp AND 240) = 16 THEN
  bamuse(temp AND 15) = 1
  i = i + 1
 END IF
 IF (temp AND 240) = 96 THEN i = i + 1
 IF (temp AND 240) = 80 THEN i = i + 6
NEXT i
FOR i = 0 TO recl
 song(i + songl) = rec(i)
NEXT i
songl = songl + recl
RETURN

newsong:
songl = 0
GOSUB stopsong
FOR i = 0 TO 8
 bamuse(i) = 0
NEXT i
GOSUB setins
RETURN

stopsong:
songp = 0
FOR i = 0 TO 8
 IF bamuse(i) = 1 AND value(i) < 0 THEN fmkeyoff i: value(i) = 0
NEXT i
FOR i = 0 TO 15
 tagp(i) = 0: rep(i) = 0
NEXT i
RETURN

setdefault:
FOR i = 0 TO 127
keymap(i) = 0
NEXT i
RESTORE defaultkey
FOR i = 36 TO 73
READ keymap(i)
NEXT i
RETURN

setins:
FOR temp = 0 TO 8
 dispfi(temp) = -1
NEXT temp
RETURN

quit:
setfmvol oldfm
fadeto buf(), 0, 0, 0
GOSUB shutoff
resetfm
restoremode
SYSTEM

switchon:
regs.ax = &H2509: regs.ds = Keyseg: regs.dx = keyoff
CALL interruptx(&H21, regs, regs)
RETURN

shutoff:
regs.ax = &H2509: regs.ds = seg9: regs.dx = off9
CALL interruptx(&H21, regs, regs)
RETURN

DATA 1,2,3,4,5,6,7,8,9,0,-,=,"","",q,w,e,r,t,y,u,i,o,p,[,],"","",a,s,d,f,g,h,j,k,l,";","'",`,"",\,z,x,c,v,b,n,m,",",".","/"

keydata:
DATA 0,0,5,3,8,2,13,3,16,1,24,0,29,3,32,2,37,3,40,2,45,3,48,1

defaultkey:
'starts at 36
DATA 44,31,45,32,46,47,34,48,35,49,36,50,51,38,52,39,53
'starts at 53
DATA 16,3,17,4,18,5,19,20,7,21,8,22,23,10,24,11,25,12,26,27,14
'ends at 73

REM $STATIC
FUNCTION getfour& (p&)
v& = 0
FOR i = 0 TO 3
GET #1, p& + i, temp
IF temp < 0 THEN temp = temp + 32768
v& = v& * 256
v& = v& + (temp AND 255)
NEXT i
getfour& = v&
END FUNCTION

SUB sortstrings (st$(), num)
DIM ns$(num), nl(num), howmany(44)
FOR lnum = 8 TO 1 STEP -1
FOR i = 0 TO 44
 howmany(i) = 0
NEXT i
FOR i = 0 TO num - 1
 IF (lnum AND 1) = 0 THEN temp = LEN(st$(i)) ELSE temp = LEN(ns$(i))
 IF temp < lnum THEN
  nl(i) = 0
 ELSE
  IF (lnum AND 1) = 0 THEN temp = ASC(MID$(st$(i), lnum, 1)) ELSE temp = ASC(MID$(ns$(i), lnum, 1))
  IF temp > 47 AND temp < 91 THEN
   nl(i) = temp - 47
  ELSEIF temp = 46 THEN
   nl(i) = 0
  ELSE
   nl(i) = 44
  END IF
 END IF
 howmany(nl(i)) = howmany(nl(i)) + 1
NEXT i
FOR i = 1 TO 44
 howmany(i) = howmany(i) + howmany(i - 1)
NEXT i
IF (lnum AND 1) = 0 THEN
 FOR i = num - 1 TO 0 STEP -1
  howmany(nl(i)) = howmany(nl(i)) - 1
  ns$(howmany(nl(i))) = st$(i)
 NEXT i
ELSE
 FOR i = num - 1 TO 0 STEP -1
  howmany(nl(i)) = howmany(nl(i)) - 1
  st$(howmany(nl(i))) = ns$(i)
 NEXT i
END IF
NEXT lnum
END SUB

