DECLARE SUB standardmenu (menu$(), size%, vis%, ptr%, top%, x%, y%, dpage%)
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE SUB xbload (f$, array%(), e$)
DECLARE FUNCTION scriptname$ (num%, f$, gen%(), buffer%())
DECLARE FUNCTION needaddset (ptr%, check%, what$, vpage%, dpage%, timing%())
DECLARE SUB cropafter (index%, limit%, flushafter%, lump$, buffer%(), bytes%, game$, timing%())
DECLARE SUB herotags (a%(), timing%(), vpage%, dpage%, game$)
DECLARE SUB cycletile (cycle%(), tastuf%(), ptr%(), skip%())
DECLARE SUB testanimpattern (tastuf%(), taset%, timing%(), vpage%, dpage%, buffer%())
DECLARE FUNCTION usemenu (ptr%, top%, first%, last%, size%)
DECLARE FUNCTION heroname$ (num%, cond%(), a%(), game$)
DECLARE FUNCTION bound% (n%, lowest%, highest%)
DECLARE FUNCTION onoroff$ (n%)
DECLARE FUNCTION intstr$ (n%)
DECLARE FUNCTION lmnemonic$ (index%, game$)
DECLARE FUNCTION rotascii$ (s$, o%)
DECLARE SUB debug (s$)
DECLARE SUB bitset (array%(), wof%, last%, name$(), timing%(), vpage%, dpage%)
DECLARE FUNCTION usemenu (ptr%, top%, first%, last%, size%)
DECLARE SUB edgeprint (s$, x%, y%, c%, p%)
DECLARE SUB formation (game$, timing%(), general%(), pal%(), buffer%(), song$())
DECLARE SUB importbmp (buffer%(), game$, timing%(), general%(), keyv%(), master%())
DECLARE SUB enemydata (game$, timing%(), general%(), pal%(), buffer%(), keyv%(), con$())
DECLARE SUB herodata (game$, timing%(), general%(), pal%(), buffer%(), keyv%(), con$())
DECLARE SUB attackdata (game$, atkdat$(), atklim%(), timing%(), general%(), pal%(), buffer%(), con$(), keyv%())
DECLARE SUB getnames (game$, stat$(), max%)
DECLARE SUB statname (game$, timing%(), general%(), name$(), keyv%())
DECLARE SUB textage (game$, timing%(), general%(), keyv(), buffer(), song$())
DECLARE FUNCTION sublist% (num%, s$(), timing%())
DECLARE SUB maptile (game$, master%(), buffer%(), timing%(), font%(), general())
DECLARE FUNCTION small% (n1%, n2%)
DECLARE FUNCTION large% (n1%, n2%)
DECLARE FUNCTION loopvar% (var%, min%, max%, inc%)
'assembly subs and functions
DECLARE SUB setmodeX ()
DECLARE SUB setpicstuf (buf(), BYVAL b, BYVAL p)
DECLARE SUB loadset (fil$, BYVAL i, BYVAL l)
DECLARE SUB storeset (fil$, BYVAL i, BYVAL l)
DECLARE SUB copypage (BYVAL page1, BYVAL page2)
DECLARE SUB setvispage (BYVAL page)
DECLARE SUB drawsprite (pic(), BYVAL picoff, pal(), BYVAL po, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB wardsprite (pic(), BYVAL picoff, pal(), BYVAL po, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB getsprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL page)
DECLARE SUB loadsprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL page)
DECLARE SUB stosprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB bigsprite (pic(), pal(), BYVAL p, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB hugesprite (pic(), pal(), BYVAL p, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB setdiskpages (buf(), BYVAL h, BYVAL l)
DECLARE SUB loadpage (fil$, BYVAL i, BYVAL p)
DECLARE SUB storepage (fil$, BYVAL i, BYVAL p)
DECLARE SUB bitmap2page (temp(), bmp$, BYVAL p)
DECLARE SUB setpal (pal())
DECLARE SUB clearpage (BYVAL page)
DECLARE SUB setkeys ()
DECLARE SUB setfont (f())
DECLARE SUB printstr (s$, BYVAL x, BYVAL y, BYVAL p)
DECLARE SUB textcolor (BYVAL f, BYVAL b)
DECLARE SUB setitup (fil$, buff(), tbuff(), BYVAL p)
DECLARE FUNCTION resetdsp
DECLARE SUB playsnd (BYVAL n, BYVAL f)
DECLARE SUB closefile
DECLARE SUB rectangle (BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL c, BYVAL p)
DECLARE SUB setwait (b(), BYVAL t)
DECLARE SUB dowait ()
DECLARE SUB setmapdata (array(), pas(), BYVAL t, BYVAL b)
DECLARE SUB setmapblock (BYVAL x, BYVAL y, BYVAL v)
DECLARE FUNCTION readmapblock (BYVAL x, BYVAL y)
DECLARE SUB drawmap (BYVAL x, BYVAL y, BYVAL t, BYVAL p)
DECLARE SUB setanim (BYVAL cycle1, BYVAL cycle2)
DECLARE FUNCTION readpixel (BYVAL x, BYVAL y, BYVAL p)
DECLARE SUB setbit (b(), BYVAL w, BYVAL b, BYVAL v)
DECLARE FUNCTION readbit (b(), BYVAL w, BYVAL b)
DECLARE FUNCTION Keyseg ()
DECLARE FUNCTION keyoff ()
DECLARE FUNCTION keyval (BYVAL a)
DECLARE FUNCTION getkey ()
DECLARE SUB copyfile (s$, d$, buf())
DECLARE SUB findfiles (fmask$, BYVAL attrib, outfile$, buf())
DECLARE SUB setupmusic (mbuf())
DECLARE SUB closemusic ()
DECLARE SUB stopsong ()
DECLARE SUB resumesong ()
DECLARE SUB resetfm ()
DECLARE SUB loadsong (f$)
'DECLARE SUB fademusic (BYVAL vol)
DECLARE FUNCTION getfmvol ()
DECLARE SUB setfmvol (BYVAL vol)
DECLARE SUB array2str (arr(), BYVAL o, s$)
DECLARE SUB str2array (s$, arr(), BYVAL o)
DECLARE FUNCTION intgrabber (n%, min%, max%, less%, more%)
DECLARE SUB strgrabber (s$, maxl%, keyv%())

REM $STATIC
SUB attackdata (game$, atkdat$(), atklim(), timing(), general(), pal(), buffer(), con$(), keyv())
DIM name$(100), a(40), b(40), extra$(20), info$(10, 10), atkbit$(-1 TO 60)
xbload game$ + ".pal", pal(), "could not find 16-color palette"
vpage = 0: dpage = 1: max = 32
clearpage 0
clearpage 1
clearpage 2
clearpage 3
OPEN game$ + ".stt" FOR BINARY AS #1
getnames game$, name$(), max
CLOSE #1
info$(0, 0) = "Cycle Forward"
info$(1, 0) = "Cycle Back"
info$(2, 0) = "Oscillate"
info$(3, 0) = "Random"
info$(0, 1) = "Strike"
info$(1, 1) = "Cast"
info$(2, 1) = "Dash In"
info$(3, 1) = "SpinStrike"
info$(4, 1) = "Jump (chain to Land)"
info$(5, 1) = "Land"
info$(6, 1) = "Null"
info$(7, 1) = "Standing Cast"
info$(8, 1) = "Teleport"
info$(0, 2) = "Normal"
info$(1, 2) = "Projectile"
info$(2, 2) = "Reverse Projectile"
info$(3, 2) = "Drop"
info$(4, 2) = "Ring"
info$(5, 2) = "Wave"
info$(6, 2) = "Scatter"
info$(7, 2) = "Sequential Projectile"
info$(8, 2) = "Meteor"
info$(9, 2) = "Driveby"
info$(10, 2) = "Null"
info$(0, 3) = "Normal: " + name$(3) + "*4 ~ " + name$(6)
info$(1, 3) = "Poor: " + name$(3) + "*2 ~ " + name$(6)
info$(2, 3) = "Bad: " + name$(3) + " ~ " + name$(6)
info$(3, 3) = "Magic: Never Misses"
info$(4, 3) = "Magic: " + name$(29) + " ~ " + name$(30) + "*1.5"
info$(0, 4) = name$(2)
info$(1, 4) = name$(29)
info$(2, 4) = name$(0)
info$(3, 4) = "Lost " + name$(0)
info$(4, 4) = "Random"
info$(5, 4) = "100"
info$(0, 5) = name$(5)
info$(1, 5) = name$(30)
info$(2, 5) = name$(5)
info$(3, 5) = name$(5)
info$(4, 5) = name$(5)
info$(5, 5) = name$(5)
info$(0, 7) = "Enemy"
info$(1, 7) = "Ally"
info$(2, 7) = "Self"
info$(3, 7) = "All"
info$(4, 7) = "Ally (Including Dead)"
info$(5, 7) = "Ally Not Self"
info$(0, 8) = "Focused"
info$(1, 8) = "Spread"
info$(2, 8) = "Optional Spread"
info$(3, 8) = "Random Focus"
info$(4, 8) = "First Target"
atkbit$(0) = "Always Cure"
atkbit$(1) = "Divide Spread Damage"
atkbit$(2) = "Bounceable"
atkbit$(3) = "Unreversable Picture"
atkbit$(4) = "Steal Item"
FOR i = 0 TO 7
atkbit$(i + 5) = name$(i + 17) + " Damage"
NEXT i
FOR i = 0 TO 7
atkbit$(i + 13) = "Bonus vs " + name$(i + 9)
NEXT i
FOR i = 0 TO 18
atkbit$((i * 2) + 21) = "Set " + con$(i)
atkbit$((i * 2) + 22) = "Unset " + con$(i)
NEXT i
atkbit$(59) = "Useable Outside of Battle"
atkbit$(60) = "Damage " + name$(1)
GOSUB thisattack

setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN GOTO doneattack
IF keyval(29) > 0 AND keyval(14) THEN cropafter ptr, general(34), 0, ".dt6", buffer(), 80, game$, timing()
IF (keyval(57) > 1 OR keyval(28) > 1) AND csr = -2 THEN GOTO doneattack
IF (keyval(57) > 1 OR keyval(28) > 1) AND csr = 18 THEN bitset a(), 20, 60, atkbit$(), timing(), vpage, dpage
IF keyval(72) > 1 AND csr > -2 THEN csr = csr - 1
IF keyval(80) > 1 AND csr < 19 THEN csr = csr + 1
IF csr = 19 THEN
 strgrabber nam$, 10, keyv()
END IF
IF csr >= 0 AND csr < 18 THEN
 atklim(0, 1) = general(32)
 atklim(12, 1) = general(34) + 1
 IF intgrabber(a(csr), atklim(csr, 0), atklim(csr, 1), 75, 77) THEN
  IF csr = 0 THEN
   setpicstuf buffer(), 3750, 2
   loadset game$ + ".pt6" + CHR$(0), a(0), 0
  END IF
  IF csr = 12 THEN GOSUB chainnm
 END IF
END IF
IF csr = -1 THEN '---SELECT A DIFFERENT ENEMY
 remptr = ptr
 IF intgrabber(ptr, 0, general(34), 51, 52) THEN
  SWAP ptr, remptr
  GOSUB saveattack
  SWAP ptr, remptr
  GOSUB thisattack
 END IF
 IF keyval(75) > 1 AND ptr > 0 THEN GOSUB saveattack: ptr = ptr - 1: GOSUB thisattack
 IF keyval(77) > 1 AND ptr < 32767 THEN
  GOSUB saveattack
  ptr = ptr + 1
  IF needaddset(ptr, general(34), "attack", vpage, dpage, timing()) THEN GOSUB clearattack
  GOSUB thisattack
 END IF
END IF'--DONE SELECTING DIFFERENT ENEMY
FOR i = 0 TO 17
extra$(i) = intstr$(a(i))
NEXT i
extra$(2) = info$(a(2), 0)
extra$(3) = info$(a(3), 7)
extra$(4) = info$(a(4), 8)
extra$(14) = info$(a(14), 1)
extra$(15) = info$(a(15), 2)
extra$(6) = info$(a(6), 3)
extra$(7) = info$(a(7), 4)
extra$(12) = chain$
info$(0, 6) = "Normal: " + info$(a(7), 4) + " - " + info$(a(7), 5) + "*.5"
info$(1, 6) = "Blunt: " + info$(a(7), 4) + "*.8 - " + info$(a(7), 5) + "*.1"
info$(2, 6) = "Sharp: " + info$(a(7), 4) + "*1.3 - " + info$(a(7), 5)
info$(3, 6) = "Pure: " + info$(a(7), 4)
info$(4, 6) = "No Damage"
extra$(5) = info$(a(5), 6)
anim0 = anim0 + 1
IF anim0 > 3 THEN
 anim0 = 0
 IF a(2) = 0 THEN anim1 = anim1 + 1: IF anim1 > 2 THEN anim1 = 0
 IF a(2) = 1 THEN anim1 = anim1 - 1: IF anim1 < 0 THEN anim1 = 2
 IF a(2) = 2 THEN anim1 = anim1 + 1: IF anim1 > 2 THEN anim1 = -1
 IF a(2) = 3 THEN anim1 = INT(RND * 3)
END IF
extra$(19) = nam$
rectangle 256, 145, 52, 52, 7, dpage
rectangle 257, 146, 50, 50, 8, dpage
loadsprite buffer(), 0, 1250 * ABS(anim1), 0, 50, 50, 2
drawsprite buffer(), 0, pal(), a(1) * 16, 257, 146, dpage
textcolor 7, 0: IF csr = -2 THEN textcolor 14 + tog, 0
printstr "Return to Main Menu", 0, 0, dpage
textcolor 7, 0: IF csr = -1 THEN textcolor 14 + tog, 0
printstr CHR$(27) + "Attack" + STR$(ptr) + CHR$(26), 0, 8, dpage
FOR i = 0 TO 19
textcolor 7, 0: IF csr = i THEN textcolor 14 + tog, 0
printstr atkdat$(i) + " " + extra$(i), 0, 16 + (8 * i), dpage
NEXT i
SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

thisattack:
setpicstuf a(), 80, -1
loadset game$ + ".dt6" + CHR$(0), ptr, 0
FOR i = 0 TO 18
a(i) = small(large(a(i), atklim(i, 0)), atklim(i, 1))
NEXT i
nam$ = ""
a(24) = small(large(a(24), 0), 10)
FOR i = 26 TO 25 + a(24)
nam$ = nam$ + CHR$(a(i))
NEXT i
setpicstuf buffer(), 3750, 2
loadset game$ + ".pt6" + CHR$(0), a(0), 0
GOSUB chainnm
RETURN

chainnm:
chain$ = ""
IF a(12) = 0 THEN RETURN
setpicstuf b(), 80, -1
loadset game$ + ".dt6" + CHR$(0), a(12) - 1, 0
FOR i = 26 TO 25 + b(24)
chain$ = chain$ + CHR$(b(i))
NEXT i
RETURN

saveattack:
a(24) = LEN(nam$)
FOR i = 26 TO 25 + a(24)
a(i) = ASC(MID$(nam$, i - 25, 1))
NEXT i
setpicstuf a(), 80, -1
storeset game$ + ".dt6" + CHR$(0), ptr, 0
RETURN

clearattack:
 FOR i = 0 TO 40
  a(i) = 0
 NEXT i
 setpicstuf a(), 80, -1
 storeset game$ + ".dt6" + CHR$(0), ptr, 0
RETURN

doneattack:
GOSUB saveattack
clearpage 0
clearpage 1
clearpage 2
clearpage 3
END SUB

SUB edgeprint (s$, x, y, c, p)
textcolor 240, 0
printstr s$, x, y + 1, p
printstr s$, x + 1, y, p
printstr s$, x + 2, y + 1, p
printstr s$, x + 1, y + 2, p
textcolor c, 0
printstr s$, x + 1, y + 1, p
END SUB

SUB enemydata (game$, timing(), general(), pal(), buffer(), keyv(), con$())
DIM name$(100), a(160), b(160), menu$(8), bmenu$(40), max(40), min(40), nof(12), attack$(24), ebit$(-1 TO 65), sp$(13)
xbload game$ + ".pal", pal(), "could not find 16-color palette"
vpage = 0: dpage = 1: max = 32
nof(0) = 0: nof(1) = 1: nof(2) = 2: nof(3) = 3: nof(4) = 5: nof(5) = 6: nof(6) = 29: nof(7) = 30: nof(8) = 8: nof(9) = 7: nof(10) = 31: nof(11) = 4
clearpage 0
clearpage 1
clearpage 2
clearpage 3
OPEN game$ + ".stt" FOR BINARY AS #1
getnames game$, name$(), max
CLOSE #1
csr = 1: ptr = 0
FOR i = 0 TO 7
ebit$(0 + i) = "Weak to " + name$(17 + i)
NEXT i
FOR i = 0 TO 7
ebit$(8 + i) = "Strong to " + name$(17 + i)
NEXT i
FOR i = 0 TO 7
ebit$(16 + i) = "Absorb " + name$(17 + i)
NEXT i
FOR i = 0 TO 7
ebit$(24 + i) = name$(9 + i)
NEXT i
FOR i = 0 TO 10
ebit$(32 + i) = "Immune to " + con$(i)
NEXT i
FOR i = 0 TO 10
ebit$(43 + i) = "Always aflicted by " + con$(i)
NEXT i
ebit$(54) = "Harmed by Cure"
ebit$(55) = "MP Idiot"
ebit$(56) = "Boss"
ebit$(57) = "Unescapable"
ebit$(58) = "Die Without Boss"
ebit$(59) = "Flee instead of Die"
ebit$(60) = "Untargetable by Enemies"
ebit$(61) = "Untargetable by Heros"
menu$(0) = "Return to Main Menu"
menu$(1) = CHR$(27) + "Pick Enemy" + STR$(ptr) + CHR$(26)
menu$(2) = "Name:"
menu$(3) = "Death:"
menu$(4) = "General Data..."
menu$(5) = "Edit Stats..."
menu$(6) = "Bitsets..."
menu$(7) = "Ally Spawning..."
menu$(8) = "Edit Attacks..."
GOSUB thisfoe

setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN GOTO donefoe
IF keyval(29) > 0 AND keyval(14) THEN cropafter ptr, general(36), 0, ".dt1", buffer(), 320, game$, timing()
IF keyval(72) > 1 AND csr > 0 THEN csr = csr - 1
IF keyval(80) > 1 AND csr < 8 THEN csr = csr + 1
IF keyval(57) > 1 OR keyval(28) > 1 THEN
 IF csr = 0 THEN GOTO donefoe
 IF csr = 4 THEN GOSUB egeneral
 IF csr = 5 THEN GOSUB foestat
 IF csr = 6 THEN bitset a(), 74, 61, ebit$(), timing(), vpage, dpage
 IF csr = 7 THEN GOSUB aspawn
 IF csr = 8 THEN GOSUB eattack
END IF
IF csr = 1 THEN
 remptr = ptr
 IF intgrabber(ptr, 0, general(36), 51, 52) THEN
  SWAP ptr, remptr
  GOSUB lastfoe
  SWAP ptr, remptr
  GOSUB thisfoe
 END IF
 IF keyval(75) > 1 AND ptr > 0 THEN GOSUB lastfoe: ptr = ptr - 1: GOSUB thisfoe
 IF keyval(77) > 1 AND ptr < 32767 THEN
  GOSUB lastfoe
  ptr = ptr + 1
  IF needaddset(ptr, general(36), "enemy", vpage, dpage, timing()) THEN GOSUB clearfoe
  GOSUB thisfoe
 END IF
END IF
IF csr = 2 THEN
 strgrabber nam$, 16, keyv()
 menu$(2) = "Name:" + nam$
END IF
IF csr = 3 THEN
 strgrabber death$, 34, keyv()
 menu$(3) = "Death:" + death$
END IF

standardmenu menu$(), 8, 22, csr, 0, 0, 0, dpage
'FOR i = 0 TO 8
' textcolor 7, 0
' IF csr = i THEN textcolor 14 + tog, 0
' printstr menu$(i), 0, i * 8, dpage
'NEXT i

loadsprite buffer(), 0, 0, 0, w, w, 2
wardsprite buffer(), 0, pal(), 16 * a(54), 260 - w * .5, 30, dpage
SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

eattack:
bctr = 0
offset = 92
FOR o = 1 TO 24
GOSUB eatkname
NEXT o
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN RETURN
IF keyval(72) > 1 AND bctr > 0 THEN bctr = bctr - 1
IF keyval(80) > 1 AND bctr < 24 THEN bctr = bctr + 1
IF bctr > 0 THEN
 IF intgrabber(a(offset + (bctr - 1)), 0, general(34) + 1, 75, 77) THEN o = bctr: GOSUB eatkname
END IF
IF keyval(57) > 1 OR keyval(28) > 1 THEN
 IF bctr = 0 THEN RETURN
END IF
FOR i = 1 TO 5
 bmenu$(i) = "Standard:" + attack$(i)
NEXT i
FOR i = 6 TO 10
 bmenu$(i) = "Desparation:" + attack$(i)
NEXT i
FOR i = 11 TO 15
 bmenu$(i) = "Alone:" + attack$(i)
NEXT i
bmenu$(16) = "Counter Physical:" + attack$(16)
FOR i = 17 TO 24
 bmenu$(i) = "Counter " + name$(i) + ":" + attack$(i)
NEXT i
textcolor 7, 0: IF bctr = 0 THEN textcolor 14 + tog, 0
printstr "Previous Menu", 0, 0, dpage

standardmenu bmenu$(), 24, 24, bctr, 1, 0, 8, dpage
'FOR i = 1 TO 24
' textcolor 7, 0: IF bctr = i THEN textcolor 14 + tog, 0
' printstr bmenu$(i), 0, 8 * i, dpage
'NEXT i

SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

eatkname:
IF a(offset + (o - 1)) = 0 THEN attack$(o) = "NONE": RETURN
attack$(o) = ""
setpicstuf b(), 80, -1
loadset game$ + ".dt6" + CHR$(0), a(offset + (o - 1)) - 1, 0
FOR j = 26 TO 25 + b(24)
attack$(o) = attack$(o) + CHR$(b(j))
NEXT j
RETURN

aspawn:
bmenu$(0) = "Previous Menu"
FOR csr2 = 1 TO 12
 max(csr2) = general(36) + 1
 GOSUB spawnn
NEXT csr2
max(13) = 7
csr2 = 0
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN RETURN
IF (keyval(57) > 1 OR keyval(28) > 1) AND csr2 = 0 THEN RETURN
IF keyval(72) > 1 THEN csr2 = large(0, csr2 - 1)
IF keyval(80) > 1 THEN csr2 = small(13, csr2 + 1)
IF csr2 > 0 THEN
 dummy = intgrabber(a(78 + csr2), 0, max(csr2), 75, 77)
 IF csr2 < 13 THEN GOSUB spawnn
END IF
bmenu$(1) = "On Death: " + sp$(1)
bmenu$(2) = "On Physical Death: " + sp$(2)
bmenu$(3) = "When Alone: " + sp$(3)
bmenu$(4) = "On Physical hit: " + sp$(4)
FOR i = 5 TO 12
bmenu$(i) = "On " + name$(17 + (i - 5)) + " hit: " + sp$(i)
NEXT i
bmenu$(13) = "Number to Spawn:" + STR$(a(91))

standardmenu bmenu$(), 13, 22, csr2, 0, 0, 0, dpage
'FOR i = 0 TO 13
' textcolor 7, 0: IF csr2 = i THEN textcolor 14 + tog, 0
' printstr bmenu$(i), 0, i * 8, dpage
'NEXT i

SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

spawnn:
sp$(csr2) = ""
IF a(78 + csr2) = 0 THEN RETURN
setpicstuf b(), 320, -1
loadset game$ + ".dt1" + CHR$(0), a(78 + csr2) - 1, 0
FOR i = 1 TO b(0)
sp$(csr2) = sp$(csr2) + CHR$(b(i))
NEXT i
RETURN

foestat:
bmenu$(0) = "Previous Menu"
FOR i = 1 TO 12: min(i) = 0: max(i) = 999: NEXT
FOR i = 1 TO 2: max(i) = 32767: NEXT i
FOR i = 10 TO 11: max(i) = 100: NEXT i
max(12) = 10
GOSUB esmi
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN RETURN
IF keyval(72) > 1 THEN bctr = large(bctr - 1, 0)
IF keyval(80) > 1 THEN bctr = small(bctr + 1, 12)
IF (keyval(28) > 1 OR keyval(57) > 1) AND bctr = 0 THEN RETURN
IF bctr > 0 THEN
 dummy = intgrabber(a(61 + bctr), min(bctr), max(bctr), 75, 77)
 GOSUB esmi
END IF

standardmenu bmenu$(), 12, 22, bctr, 0, 8, 0, 0
'FOR i = 0 TO 12
' textcolor 7, 0
' IF i = bctr THEN textcolor 14 + tog, 0
' printstr bmenu$(i), 8, i * 8, dpage
'NEXT i

SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

esmi:
FOR i = 0 TO 11
bmenu$(i + 1) = name$(nof(i)) + STR$(a(i + 62))
NEXT i
RETURN

egeneral:
bctr = 0
bmenu$(0) = "Previous Menu"
min(1) = 0: max(1) = m
min(2) = 0: max(2) = 99
min(3) = 0: max(3) = 2
min(4) = 0: max(4) = 32767
min(5) = 0: max(5) = 32767
min(6) = 0: max(6) = 255
min(7) = 0: max(7) = 100
min(8) = 0: max(8) = 255
min(9) = 0: max(9) = 100
GOSUB iplunder
setkeys
DO
bmenu$(1) = "Picture:" + STR$(a(53))
bmenu$(2) = "Palette:" + STR$(a(54))
bmenu$(3) = "Size:" + STR$(a(55))
bmenu$(4) = "Gold:" + STR$(a(56))
bmenu$(5) = "Experience:" + STR$(a(57))
bmenu$(6) = "Item: " + inormal$
bmenu$(7) = "Item%:" + STR$(a(59))
bmenu$(8) = "Rare Item: " + irare$
bmenu$(9) = "Rare Item%:" + STR$(a(61))
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN RETURN
IF keyval(72) > 1 AND bctr > 0 THEN bctr = bctr - 1
IF keyval(80) > 1 AND bctr < 9 THEN bctr = bctr + 1
IF (keyval(28) > 1 OR keyval(57) > 1) AND bcsr = 0 THEN RETURN
IF bctr > 0 THEN
 temp = a(52 + bctr)
 IF intgrabber(a(52 + bctr), min(bctr), max(bctr), 75, 77) THEN
  IF bctr = 1 OR bctr = 3 THEN GOSUB foepics
  max(1) = m
  a(53) = small(a(53), m)
 END IF
 IF temp <> a(52 + bctr) THEN GOSUB foepics: GOSUB iplunder
END IF

standardmenu bmenu$(), 9, 22, bctr, 0, 0, 0, dpage
'FOR i = 0 TO 9
' textcolor 7, 0
' IF i = bctr THEN textcolor 14 + tog, 0
' printstr bmenu$(i), 8, i * 8, dpage
'NEXT i

loadsprite buffer(), 0, 0, 0, w, w, 2
wardsprite buffer(), 0, pal(), 16 * a(54), 260 - w * .5, 30, dpage
SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

iplunder:
setpicstuf buffer(), 200, -1
loadset game$ + ".itm" + CHR$(0), a(58), 0
inormal$ = ""
FOR o = 1 TO small(buffer(0), 12)
 IF buffer(o) < 256 AND buffer(o) > -1 THEN inormal$ = inormal$ + CHR$(buffer(o)) ELSE inormal$ = ""
NEXT o
setpicstuf buffer(), 200, -1
loadset game$ + ".itm" + CHR$(0), a(60), 0
irare$ = ""
FOR o = 1 TO small(buffer(0), 12)
 IF buffer(o) < 256 AND buffer(o) > -1 THEN irare$ = irare$ + CHR$(buffer(o)) ELSE irare$ = ""
NEXT o
RETURN

clearfoe:
FOR i = 0 TO 160
 a(i) = 0
NEXT i
setpicstuf a(), 320, -1
storeset game$ + ".dt1" + CHR$(0), ptr, 0
RETURN

lastfoe:
a(0) = LEN(nam$)
FOR i = 1 TO a(0)
a(i) = ASC(MID$(nam$, i, 1))
NEXT i
a(17) = LEN(death$)
FOR i = 1 TO a(17)
a(i + 17) = ASC(MID$(death$, i, 1))
NEXT i
setpicstuf a(), 320, -1
storeset game$ + ".dt1" + CHR$(0), ptr, 0
RETURN

thisfoe:
setpicstuf a(), 320, -1
loadset game$ + ".dt1" + CHR$(0), ptr, 0
nam$ = "": death$ = ""
IF a(0) > 16 THEN a(0) = 0
FOR i = 1 TO a(0)
nam$ = nam$ + CHR$(a(i))
NEXT i
IF a(17) > 36 THEN a(17) = 0
FOR i = 1 TO a(17)
death$ = death$ + CHR$(a(i + 17))
NEXT i
menu$(3) = "Death:" + death$
menu$(2) = "Name:" + nam$
menu$(1) = CHR$(27) + "Pick Enemy" + STR$(ptr) + CHR$(26)
GOSUB foepics
RETURN

foepics:
IF a(55) = 0 THEN s = 578: w = 34: f$ = ".pt1": m = general(27)
IF a(55) = 1 THEN s = 1250: w = 50: f$ = ".pt2": m = general(28)
IF a(55) = 2 THEN s = 3200: w = 80: f$ = ".pt3": m = general(29)
setpicstuf buffer(), s, 2
loadset game$ + f$ + CHR$(0), a(53), 0
RETURN

donefoe:
GOSUB lastfoe
dowait
clearpage 0
clearpage 1
clearpage 2
clearpage 3
END SUB

SUB formation (game$, timing(), general(), pal(), buffer(), song$())
DIM a(40), b(160), c(24), s(7), w(7), po(7), menu$(10), ename$(7), max(10), z(7), bmenu$(22)
xbload game$ + ".pal", pal(), "could not find 16-color palette"
vpage = 0: dpage = 1
clearpage 0
clearpage 1
clearpage 2
clearpage 3
setdiskpages buffer(), 200, 0

menu$(0) = "Return to Main Menu"
menu$(1) = "Edit Individual Formations..."
menu$(2) = "Construct Formation Sets..."
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN GOTO doneform
IF keyval(72) > 1 THEN csr = large(csr - 1, 0)
IF keyval(80) > 1 THEN csr = small(csr + 1, 2)
IF keyval(57) > 1 OR keyval(28) > 1 THEN
 IF csr = 0 THEN GOTO doneform
 IF csr = 1 THEN GOSUB editform
 IF csr = 2 THEN GOSUB formsets
END IF

standardmenu menu$(), 2, 22, csr, 0, 0, 0, dpage
'FOR i = 0 TO 2
' textcolor 7, 0
' IF i = csr THEN textcolor 14 + tog, 0
' printstr menu$(i), 0, i * 8, dpage
'NEXT i

SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

formsets:
bmenu$(0) = "Previous Menu"
GOSUB loadfset
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN GOSUB savefset: RETURN
IF keyval(72) > 1 THEN bcsr = large(bcsr - 1, 0)
IF keyval(80) > 1 THEN bcsr = small(bcsr + 1, 22)
IF keyval(28) > 1 OR keyval(57) > 1 THEN
 IF bcsr = 0 THEN GOSUB savefset: RETURN
END IF
IF bcsr = 1 THEN
 IF keyval(75) > 1 THEN GOSUB savefset: gptr = large(gptr - 1, 0): GOSUB loadfset
 IF keyval(77) > 1 THEN GOSUB savefset: gptr = small(gptr + 1, 255): GOSUB loadfset
END IF
IF bcsr = 2 THEN dummy = intgrabber(c(0), 0, 99, 75, 77)
IF bcsr > 2 THEN dummy = intgrabber(c(bcsr - 2), 0, general(37) + 1, 75, 77)
bmenu$(1) = CHR$(27) + "Formation Set" + STR$(gptr + 1) + CHR$(26)
bmenu$(2) = "Battle Frequency:" + STR$(c(0))
FOR i = 3 TO 22
 bmenu$(i) = "Formation" + STR$(c(i - 2) - 1)
 IF c(i - 2) = 0 THEN bmenu$(i) = "Empty"
NEXT i

standardmenu bmenu$(), 22, 22, bcsr, 0, 0, 0, dpage
'FOR i = 0 TO 22
' textcolor 7, 0
' IF i = bcsr THEN textcolor 14 + tog, 0
' printstr bmenu$(i), 0, i * 8, dpage
'NEXT i

SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

savefset:
setpicstuf c(), 50, -1
storeset game$ + ".efs" + CHR$(0), gptr, 0
RETURN

loadfset:
setpicstuf c(), 50, -1
loadset game$ + ".efs" + CHR$(0), gptr, 0
RETURN

editform:
menu$(3) = "Previous Menu"
max(0) = general(100) - 1
max(1) = 100
max(2) = 999
ptr = 0: csr2 = -5: csr3 = 0
GOSUB loadform
GOSUB formpics
setkeys
DO
setwait timing(), 90
setkeys
tog = tog XOR 1
flash = flash + 1: IF flash > 14 THEN flash = -13
IF csr3 = 1 THEN
 IF keyval(1) > 1 OR keyval(57) > 1 OR keyval(28) > 1 THEN setkeys: csr3 = 0
 IF keyval(56) > 0 THEN
  IF keyval(72) > 0 AND a(csr2 * 4 + 2) > 0 THEN a(csr2 * 4 + 2) = a(csr2 * 4 + 2) - 8
  IF keyval(80) > 0 AND a(csr2 * 4 + 2) < 199 - w(csr2) THEN a(csr2 * 4 + 2) = a(csr2 * 4 + 2) + 8
  IF keyval(75) > 0 AND a(csr2 * 4 + 1) > 0 THEN a(csr2 * 4 + 1) = a(csr2 * 4 + 1) - 8
  IF keyval(77) > 0 AND a(csr2 * 4 + 1) < 250 - w(csr2) THEN a(csr2 * 4 + 1) = a(csr2 * 4 + 1) + 8
 END IF
 IF keyval(56) = 0 THEN
  IF keyval(72) > 0 AND a(csr2 * 4 + 2) > 0 THEN a(csr2 * 4 + 2) = a(csr2 * 4 + 2) - 1
  IF keyval(80) > 0 AND a(csr2 * 4 + 2) < 199 - w(csr2) THEN a(csr2 * 4 + 2) = a(csr2 * 4 + 2) + 1
  IF keyval(75) > 0 AND a(csr2 * 4 + 1) > 0 THEN a(csr2 * 4 + 1) = a(csr2 * 4 + 1) - 1
  IF keyval(77) > 0 AND a(csr2 * 4 + 1) < 250 - w(csr2) THEN a(csr2 * 4 + 1) = a(csr2 * 4 + 1) + 1
 END IF
END IF
IF csr3 = 0 THEN
 IF keyval(1) > 1 THEN GOSUB saveform: RETURN
 IF keyval(29) > 0 AND keyval(14) THEN cropafter ptr, general(37), 0, ".for", buffer(), 80, game$, timing()
 IF keyval(72) > 1 THEN csr2 = large(csr2 - 1, -5)
 IF keyval(80) > 1 THEN csr2 = small(csr2 + 1, 7)
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  IF csr2 = -5 THEN GOSUB saveform: RETURN
  IF csr2 >= 0 THEN IF a(csr2 * 4 + 0) > 0 THEN csr3 = 1
 END IF
 IF csr2 > -4 AND csr2 < 0 THEN
  IF intgrabber(a(35 + csr2), 0, max(csr2 + 3), 75, 77) THEN GOSUB saveform: GOSUB loadform
 END IF
 IF csr2 = -4 THEN '---SELECT A DIFFERENT FORMATION
  remptr = ptr
  IF intgrabber(ptr, 0, general(37), 51, 52) THEN
   SWAP ptr, remptr
   GOSUB saveform
   SWAP ptr, remptr
   GOSUB loadform
   GOSUB formpics
  END IF
  IF keyval(75) > 1 AND ptr > 0 THEN GOSUB saveform: ptr = large(ptr - 1, 0): GOSUB loadform: GOSUB formpics
  IF keyval(77) > 1 AND ptr < 32767 THEN
   GOSUB saveform
   ptr = ptr + 1
   IF needaddset(ptr, general(37), "attack", vpage, dpage, timing()) THEN GOSUB clearformation
   GOSUB loadform
   GOSUB formpics
  END IF
 END IF'--DONE SELECTING DIFFERENT FORMATION
 IF keyval(75) > 1 THEN
  IF csr2 >= 0 THEN a(csr2 * 4 + 0) = large(a(csr2 * 4 + 0) - 1, 0): GOSUB formpics
 END IF
 IF keyval(77) > 1 THEN
  IF csr2 >= 0 THEN a(csr2 * 4 + 0) = small(a(csr2 * 4 + 0) + 1, general(36) + 1): GOSUB formpics
 END IF
END IF
GOSUB formsprite
FOR i = 0 TO 3
 rectangle 240 + i * 8, 75 + i * 22, 32, 40, 18 + i * 2, dpage
NEXT i
IF csr3 = 0 THEN
 menu$(4) = CHR$(27) + "formation" + STR$(ptr) + CHR$(26)
 menu$(5) = "Backdrop screen:" + STR$(a(32))
 menu$(6) = "Battle Music:"
 IF a(33) = 0 THEN menu$(6) = menu$(6) + " -none-" ELSE menu$(6) = menu$(6) + STR$(a(33)) + " " + song$(a(33) - 1)
 menu$(7) = "*Unused*:" + STR$(a(34))
 FOR i = 0 TO 4
  col = 7: IF csr2 + 5 = i THEN col = 14 + tog
  edgeprint menu$(i + 3), 1, 1 + (i * 10), col, dpage
 NEXT i
 FOR i = 0 TO 7
  col = 7: IF csr2 = i THEN col = 14 + tog
  edgeprint "Enemy:" + ename$(i), 1, 50 + (i * 10), col, dpage
 NEXT i
END IF
SWAP vpage, dpage
setvispage vpage
copypage 2, dpage
dowait
LOOP

formsprite:
FOR i = 0 TO 7
 z(i) = i
NEXT i
FOR i = 0 TO 6
 temp = 200
 FOR o = 7 TO i STEP -1
  IF a(z(o) * 4 + 2) < temp THEN temp = a(z(o) * 4 + 2): j = o
 NEXT o
 SWAP z(j), z(i)
NEXT i
FOR i = 0 TO 7
IF a(z(i) * 4 + 0) > 0 THEN
 loadsprite buffer(), 0, 0, z(i) * 10, w(z(i)), w(z(i)), 3
 drawsprite buffer(), 0, pal(), po(z(i)) * 16, a(z(i) * 4 + 1), a(z(i) * 4 + 2), dpage
 IF csr2 = z(i) THEN textcolor 176 + ABS(flash), 0: printstr CHR$(25), a(z(i) * 4 + 1) + (w(z(i)) * .5) - 4, a(z(i) * 4 + 2), dpage
END IF
NEXT i
RETURN

clearformation:
FOR i = 0 TO 40
 a(i) = 0
NEXT i
setpicstuf a(), 80, -1
storeset game$ + ".for" + CHR$(0), ptr, 0
RETURN

saveform:
setpicstuf a(), 80, -1
storeset game$ + ".for" + CHR$(0), ptr, 0
RETURN

loadform:
setpicstuf a(), 80, -1
loadset game$ + ".for" + CHR$(0), ptr, 0
loadpage game$ + ".mxs" + CHR$(0), a(32), 2
IF a(33) = 0 THEN a(33) = general(4)
RETURN

formpics:
FOR i = 0 TO 7
ename$(i) = "-EMPTY-"
IF a(i * 4 + 0) > 0 THEN
 setpicstuf b(), 320, -1
 loadset game$ + ".dt1" + CHR$(0), a(i * 4 + 0) - 1, 0
 ename$(i) = ""
 FOR o = 1 TO b(0)
 ename$(i) = ename$(i) + CHR$(b(o))
 NEXT o
 po(i) = b(54)
 IF b(55) = 0 THEN s(i) = 578: w(i) = 34: f$ = ".pt1"
 IF b(55) = 1 THEN s(i) = 1250: w(i) = 50: f$ = ".pt2"
 IF b(55) = 2 THEN s(i) = 3200: w(i) = 80: f$ = ".pt3"
 setpicstuf buffer(), s(i), 3
 loadset game$ + f$ + CHR$(0), b(53), i * 10
END IF
NEXT i
RETURN

doneform:
clearpage 0
clearpage 1
clearpage 2
clearpage 3
END SUB

SUB herodata (game$, timing(), general(), pal(), buffer(), keyv(), con$())
DIM name$(100), a(318), menu$(8), bmenu$(40), max(40), min(40), nof(12), attack$(24), b(40), option$(10), hbit$(-1 TO 45), hmenu$(4)
xbload game$ + ".pal", pal(), "could not find 16-color palette"
vpage = 0: dpage = 1: wd = 1: max = 32
nof(0) = 0: nof(1) = 1: nof(2) = 2: nof(3) = 3: nof(4) = 5: nof(5) = 6: nof(6) = 29: nof(7) = 30: nof(8) = 8: nof(9) = 7: nof(10) = 31: nof(11) = 4
clearpage 0
clearpage 1
clearpage 2
clearpage 3
OPEN game$ + ".stt" FOR BINARY AS #1
getnames game$, name$(), max
CLOSE #1
csr = 1
FOR i = 0 TO 7
hbit$(i) = "Weak to " + name$(17 + i)
hbit$(i + 8) = "Strong to " + name$(17 + i)
hbit$(i + 16) = "Absorb " + name$(17 + i)
NEXT i
FOR i = 0 TO 10
hbit$(24 + i) = "Immune to " + con$(i)
NEXT i
FOR i = 0 TO 7
hbit$(35 + i) = "Allways Effected by " + con$(11 + i)
NEXT i
menu$(0) = "Return to Main Menu"
menu$(1) = CHR$(27) + "Pick Hero" + STR$(ptr) + CHR$(26)
menu$(2) = "Name:"
menu$(3) = "Appearance..."
menu$(4) = "Edit Stats..."
menu$(5) = "Edit Spell Lists..."
menu$(6) = "Bitsets..."
menu$(7) = "Name Spell Lists..."
menu$(8) = "Hero Tags..."
GOSUB thishero

setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
GOSUB movesmall
IF keyval(1) > 1 THEN GOTO donehero
IF keyval(29) > 0 AND keyval(14) THEN
 cropafter ptr, general(35), -1, ".dt0", buffer(), 636, game$, timing()
END IF
IF keyval(72) > 1 AND csr > 0 THEN csr = csr - 1
IF keyval(80) > 1 AND csr < 8 THEN csr = csr + 1
IF keyval(57) > 1 OR keyval(28) > 1 THEN
 IF csr = 0 THEN GOTO donehero
 IF csr = 3 THEN GOSUB picnpal
 IF csr = 4 THEN GOSUB levstats
 IF csr = 5 THEN GOSUB speltypes
 IF csr = 6 THEN bitset a(), 240, 42, hbit$(), timing(), vpage, dpage
 IF csr = 7 THEN GOSUB heromenu
 IF csr = 8 THEN herotags a(), timing(), vpage, dpage, game$
END IF
IF csr = 1 THEN
 remptr = ptr
 IF intgrabber(ptr, 0, general(35), 51, 52) THEN
  SWAP ptr, remptr
  GOSUB lasthero
  SWAP ptr, remptr
  GOSUB thishero
 END IF
 IF keyval(75) > 1 AND ptr > 0 THEN GOSUB lasthero: ptr = ptr - 1: GOSUB thishero
 IF keyval(77) > 1 AND ptr < 59 THEN
  GOSUB lasthero
  ptr = ptr + 1
  IF needaddset(ptr, general(35), "hero", vpage, dpage, timing()) THEN GOSUB clearhero
  GOSUB thishero
 END IF
END IF
IF csr = 2 THEN
 strgrabber nam$, 16, keyv()
 menu$(2) = "Name:" + nam$
END IF

standardmenu menu$(), 8, 22, csr, 0, 0, 0, dpage
'FOR i = 0 TO 8
' textcolor 7, 0
' IF csr = i THEN textcolor 14 + tog, 0
' printstr menu$(i), 0, i * 8, dpage
'NEXT i

loadsprite buffer(), 0, 640 * tog, 0, 32, 40, 2
drawsprite buffer(), 0, pal(), 16 * a(18), 250, 25, dpage
loadsprite buffer(), 0, (wd * 400) + (200 * tog), 16, 20, 20, 2
drawsprite buffer(), 0, pal(), 16 * a(20), 230 + wx, 5 + wy, dpage
SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

heromenu:
bmenu$(0) = "Previous Menu": bctr = 0
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN RETURN
IF (keyval(57) > 1 OR keyval(28) > 1) AND bctr = 0 THEN RETURN
IF keyval(72) > 1 THEN bctr = large(0, bctr - 1)
IF keyval(80) > 1 THEN bctr = small(4, bctr + 1)
IF bctr > 0 THEN
 strgrabber hmenu$(bctr - 1), 10, keyv()
END IF
bmenu$(1) = "Spell List 1:" + hmenu$(0)
bmenu$(2) = "Spell List 2:" + hmenu$(1)
bmenu$(3) = "Spell List 3:" + hmenu$(2)
bmenu$(4) = "Spell List 4:" + hmenu$(3)

standardmenu bmenu$(), 4, 22, bctr, 0, 0, 0, dpage
'FOR i = 0 TO 4
' textcolor 7, 0: IF bctr = i THEN textcolor 14 + tog, 0
' printstr bmenu$(i), 0, i * 8, dpage
'NEXT i

SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

speltypes:
FOR i = 0 TO 3
 IF a(288 + i) > 10 OR a(288 + i) < 0 THEN a(288 + i) = 0
NEXT i
bctr = -1
option$(0) = "Spells (MP Based)"
option$(1) = "Spells (FF1 Style)"
option$(2) = "Random Effects"
option$(3) = "Item Consuming (not implemented)"
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN RETURN
IF keyval(72) > 1 THEN bctr = large(bctr - 1, -1)
IF keyval(80) > 1 THEN bctr = small(bctr + 1, 3)
IF bctr >= 0 THEN dummy = intgrabber(a(288 + bctr), 0, 2, 75, 77)
IF keyval(57) > 1 OR keyval(28) > 1 THEN
 IF bctr = -1 THEN RETURN
 IF bctr >= 0 AND bctr < 4 THEN temp = bctr: GOSUB spells: bctr = temp
END IF
textcolor 7, 0: IF bctr = -1 THEN textcolor 14 + tog, 0
printstr "Cancel", 0, 0, dpage
FOR i = 0 TO 3
 textcolor 7, 0: IF bctr = i THEN textcolor 14 + tog, 0
 printstr "Type" + STR$(i) + " Spells:" + option$(a(288 + i)), 0, 8 + i * 8, dpage
NEXT i
SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

picnpal:
bctr = 0
bmenu$(0) = "Previous Menu"
min(1) = 0: max(1) = general(26)
min(2) = 0: max(2) = 99
min(3) = 0: max(3) = general(30)
min(4) = 0: max(4) = 99
min(5) = -1: max(5) = 99
min(6) = 0: max(6) = 255
GOSUB itstrh
setkeys
DO
GOSUB genheromenu
setwait timing(), 100
setkeys
tog = tog XOR 1
GOSUB movesmall
IF keyval(1) > 1 THEN RETURN
IF keyval(72) > 1 AND bctr > 0 THEN bctr = bctr - 1
IF keyval(80) > 1 AND bctr < 6 THEN bctr = bctr + 1
IF (keyval(28) > 1 OR keyval(57) > 1) AND bcsr = 0 THEN RETURN
IF bctr > 0 THEN
 IF intgrabber(a(16 + bctr), min(bctr), max(bctr), 75, 77) THEN
  IF bctr = 1 OR bctr = 3 THEN GOSUB heropics
  IF bctr = 6 THEN GOSUB itstrh
 END IF
END IF

standardmenu bmenu$(), 6, 22, bctr, 0, 8, 0, dpage
'FOR i = 0 TO 6
' textcolor 7, 0
' IF i = bctr THEN textcolor 14 + tog, 0
' printstr bmenu$(i), 8, i * 8, dpage
'NEXT i

loadsprite buffer(), 0, 640 * tog, 0, 32, 40, 2
drawsprite buffer(), 0, pal(), 16 * a(18), 250, 25, dpage
loadsprite buffer(), 0, (wd * 400) + (200 * tog), 16, 20, 20, 2
drawsprite buffer(), 0, pal(), 16 * a(20), 230 + wx, 5 + wy, dpage
SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

genheromenu:
bmenu$(1) = "Battle Picture:" + STR$(a(17))
bmenu$(2) = "Battle Palette:" + STR$(a(18))
bmenu$(3) = "Walkabout Picture:" + STR$(a(19))
bmenu$(4) = "Walkabout Palette:" + STR$(a(20))
bmenu$(5) = "Base Level:" + STR$(a(21))
IF a(21) < 0 THEN bmenu$(5) = "Base Level: Party Average"
bmenu$(6) = "Default Weapon:" + it$
RETURN

levstats:
bctr = 0
bmenu$(0) = "Previous Menu"
FOR i = 1 TO 24: min(i) = 0: max(i) = 999: NEXT
FOR i = 1 TO 4: max(i) = 9999: NEXT i
FOR i = 19 TO 22: max(i) = 100: NEXT i
FOR i = 23 TO 24: max(i) = 10: NEXT i
GOSUB smi
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN RETURN
IF keyval(72) > 1 THEN bctr = large(bctr - 2, 0)
IF keyval(80) > 1 AND bctr > 0 THEN bctr = small(bctr + 2, 24)
IF keyval(80) > 1 AND bctr = 0 THEN bctr = bctr + 1
IF keyval(75) > 1 AND bctr > 0 THEN bctr = bctr - 1
IF keyval(77) > 1 AND bctr < 24 THEN bctr = bctr + 1
IF (keyval(28) > 1 OR keyval(57) > 1) AND bctr = 0 THEN RETURN
IF bctr > 0 THEN
 IF intgrabber(a(22 + bctr), min(bctr), max(bctr), 51, 52) THEN GOSUB smi
END IF
textcolor 7, 0
IF 0 = bctr THEN textcolor 14 + tog, 0
printstr bmenu$(0), 8, 0, dpage
textcolor 11, 0
printstr "LEVEL ZERO", 8, 12, dpage
printstr "LEVEL NINETY-NINE", 160, 12, dpage
FOR i = 0 TO 11
 textcolor 7, 0
 IF 1 + i * 2 = bctr THEN textcolor 14 + tog, 0
 printstr bmenu$(1 + i * 2), 8, 20 + i * 8, dpage
 textcolor 7, 0
 IF 2 + i * 2 = bctr THEN textcolor 14 + tog, 0
 printstr bmenu$(2 + i * 2), 160, 20 + i * 8, dpage
NEXT i
IF bctr > 0 THEN GOSUB graph
SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

spells:
bctr = 0
offset = 47 + (temp * 48)
FOR o = 1 TO 24
GOSUB atkname
NEXT o
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN RETURN
IF keyval(72) > 1 AND bctr > 0 THEN bctr = bctr - 1
IF keyval(80) > 1 AND bctr < 24 THEN bctr = bctr + 1
IF keyval(75) > 1 AND bctr > 0 AND a(offset + (bctr - 1) * 2) > 0 THEN a(offset + (bctr - 1) * 2) = a(offset + (bctr - 1) * 2) - 1: o = bctr: GOSUB atkname
IF keyval(77) > 1 AND bctr > 0 AND a(offset + (bctr - 1) * 2) < general(34) + 1 THEN a(offset + (bctr - 1) * 2) = a(offset + (bctr - 1) * 2) + 1: o = bctr: GOSUB atkname
IF bctr > 0 THEN dummy = intgrabber(a(offset + (bctr - 1) * 2 + 1), 0, 100, 51, 52)
IF keyval(57) > 1 OR keyval(28) > 1 THEN
 IF bctr = 0 THEN RETURN
END IF
textcolor 10, 0: printstr UCASE$(option$(a(288 + temp))), 300 - LEN(option$(a(288 + temp))) * 8, 0, dpage
textcolor 7, 0: IF bctr = 0 THEN textcolor 14 + tog, 0
printstr "Previous Menu", 8, 0, dpage
FOR i = 1 TO 24
 textcolor 7, 0: IF bctr = i THEN textcolor 14 + tog, 0
 temp$ = attack$(i)
 IF a(offset + (i - 1) * 2) > 0 THEN
  IF a(offset + (i - 1) * 2 + 1) = 0 THEN temp$ = temp$ + " (Learned from Item)"
  IF a(offset + (i - 1) * 2 + 1) > 0 THEN temp$ = temp$ + " (Learned at Level" + STR$(a(offset + (i - 1) * 2 + 1) - 1) + ")"
 END IF
 printstr temp$, 8, 8 * i, dpage
NEXT i
SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

atkname:
IF a(offset + (o - 1) * 2) = 0 THEN attack$(o) = "EMPTY": RETURN
attack$(o) = ""
setpicstuf b(), 80, -1
loadset game$ + ".dt6" + CHR$(0), a(offset + (o - 1) * 2) - 1, 0
FOR j = 26 TO 25 + b(24)
attack$(o) = attack$(o) + CHR$(b(j))
NEXT j
RETURN

graph:
o = INT((bctr - 1) / 2)
printstr name$(nof(o)), 310 - LEN(name$(nof(o))) * 8, 180, dpage
FOR i = 0 TO 99 STEP 4
ii = (.8 * i / 50) * i * ((a(24 + o * 2) - a(23 + o * 2)) / 100) + a(23 + o * 2)
ii = large(ii, 0)
j = (ii) * (100 / max(bctr))
rectangle 290 + (i / 4), 176 - j, 1, j + 1, 7, dpage
NEXT i
RETURN

smi:
FOR i = 0 TO 11
bmenu$(i * 2 + 1) = name$(nof(i)) + STR$(a(i * 2 + 23))
bmenu$(i * 2 + 2) = name$(nof(i)) + STR$(a(i * 2 + 24))
NEXT i
RETURN

movesmall:
wc = wc + 1: IF wc >= 15 THEN wc = 0: wd = wd + 1: IF wd > 3 THEN wd = 0
IF wd = 0 THEN wy = wy - 4
IF wd = 1 THEN wx = wx + 4
IF wd = 2 THEN wy = wy + 4
IF wd = 3 THEN wx = wx - 4
RETURN

clearhero:
FOR i = 0 TO 318
 a(i) = 0
NEXT i
setpicstuf a(), 636, -1
storeset game$ + ".dt0" + CHR$(0), ptr, 0
RETURN

lasthero:
a(0) = LEN(nam$)
FOR i = 1 TO a(0)
a(i) = ASC(MID$(nam$, i, 1))
NEXT i
FOR i = 0 TO 3
 a(243 + i * 11) = LEN(hmenu$(i))
 FOR o = 1 TO LEN(hmenu$(i))
  a(243 + (i * 11) + o) = ASC(MID$(hmenu$(i), o, 1))
 NEXT o
NEXT i
setpicstuf a(), 636, -1
storeset game$ + ".dt0" + CHR$(0), ptr, 0
RETURN

thishero:
setpicstuf a(), 636, -1
loadset game$ + ".dt0" + CHR$(0), ptr, 0
nam$ = ""
FOR i = 1 TO a(0)
nam$ = nam$ + CHR$(a(i))
NEXT i
FOR i = 0 TO 3
 hmenu$(i) = ""
 FOR o = 1 TO a(243 + i * 11)
  hmenu$(i) = hmenu$(i) + CHR$(a(243 + (i * 11) + o))
 NEXT o
NEXT i
menu$(2) = "Name:" + nam$
menu$(1) = CHR$(27) + "Pick Hero" + STR$(ptr) + CHR$(26)
GOSUB heropics
RETURN

heropics:
setpicstuf buffer(), 5120, 2
loadset game$ + ".pt0" + CHR$(0), a(17), 0
setpicstuf buffer(), 1600, 2
loadset game$ + ".pt4" + CHR$(0), a(19), 16
RETURN

itstrh:
setpicstuf buffer(), 200, -1
loadset game$ + ".itm" + CHR$(0), a(22), 0
it$ = ""
FOR o = 1 TO buffer(0)
 IF buffer(o) < 256 AND buffer(o) > -1 THEN it$ = it$ + CHR$(buffer(o)) ELSE it$ = ""
NEXT o
RETURN

donehero:
GOSUB lasthero
clearpage 0
clearpage 1
clearpage 2
clearpage 3

'0-318,636 bytes
'0       name length
'1-16    name content
'17      pic
'18      pal
'19      walkabout pic
'20      walkabout pal
'21      base level
'22      default weapon
'23-46   stats
'47-238  spell lists
'240-242 bitsets
'243-286 spell list names
'288-291 spell menu types
'292-295 hero tags

END SUB

SUB herotags (a(), timing(), vpage, dpage, game$)

DIM menu$(5)
menu$(0) = "Previous Menu"
menu$(1) = "have hero TAG"
menu$(2) = "is alive TAG"
menu$(3) = "is leader TAG"
menu$(4) = "is in party now TAG"

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN GOTO doneherotags
 IF keyval(72) > 1 THEN ptr = large(ptr - 1, 0)
 IF keyval(80) > 1 THEN ptr = small(ptr + 1, 4)
 SELECT CASE ptr
  CASE 0
   IF keyval(57) > 1 OR keyval(28) > 1 THEN GOTO doneherotags
  CASE ELSE
   dummy = intgrabber(a(291 + ptr), 0, 500, 75, 77)
 END SELECT
 FOR i = 0 TO 4
  a$ = ""
  IF i > 0 THEN
   a$ = STR$(a(291 + i)) + " (" + lmnemonic(a(291 + i), game$) + ")"
  END IF
  textcolor 7, 0
  IF ptr = i THEN textcolor 14 + tog, 0
  printstr menu$(i) + a$, 0, i * 8, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

doneherotags:
'292     have hero tag
'293     is alive tag
'294     is leader tag
'295     is in active party tag
END SUB

FUNCTION intgrabber (n, min, max, less, more)
 STATIC clip
 old = n
 IF keyval(more) > 1 THEN n = loopvar(n, min, max, 1): GOTO doneintgrabber
 IF keyval(less) > 1 THEN n = loopvar(n, min, max, -1): GOTO doneintgrabber
 s = SGN(n)
 n$ = intstr$(ABS(n))
 IF keyval(14) > 1 AND LEN(n$) > 0 THEN n$ = LEFT$(n$, LEN(n$) - 1)
 FOR i = 1 TO 9
  IF keyval(i + 1) > 1 THEN n$ = n$ + intstr$(i)
 NEXT i
 IF keyval(11) > 1 THEN n$ = n$ + "0"
 IF min < 0 THEN
  IF keyval(12) > 1 OR keyval(13) > 1 OR keyval(74) > 1 OR keyval(78) > 1 THEN s = s * -1
 END IF
 capper& = INT(VAL(n$))
 IF capper& > 32767 THEN capper& = 32767
 IF capper& < -32767 THEN capper& = -32767
 n = capper&
 IF s <> 0 THEN n = n * s
 'CLIPBOARD
 IF (keyval(29) > 0 AND keyval(82) > 1) OR ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(83)) OR (keyval(29) > 0 AND keyval(46) > 1) THEN clip = n
 IF ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(82) > 1) OR (keyval(29) > 0 AND keyval(47) > 1) THEN n = clip
 n = large(min, n)
 n = small(max, n)

doneintgrabber:

IF old = n THEN
 intgrabber = 0
ELSE
 intgrabber = 1
END IF

END FUNCTION

SUB itemdata (game$, timing(), general(), pal(), buffer(), keyv(), con$())
DIM name$(100), a(99), menu$(18), bmenu$(40), nof(12), b(40), ibit$(-1 TO 59), item$(-1 TO 255), eqst$(5), max(18), sbmax(11)
xbload game$ + ".pal", pal(), "could not find 16-color palette"
vpage = 0: dpage = 1: max = 32
nof(0) = 0: nof(1) = 1: nof(2) = 2: nof(3) = 3: nof(4) = 5: nof(5) = 6: nof(6) = 29: nof(7) = 30: nof(8) = 8: nof(9) = 7: nof(10) = 31: nof(11) = 4
clearpage 0
clearpage 1
clearpage 2
clearpage 3
OPEN game$ + ".stt" FOR BINARY AS #1
getnames game$, name$(), max
CLOSE #1
eqst$(0) = "NEVER EQUIPPED"
eqst$(1) = "Weapon"
FOR i = 0 TO 3
 eqst$(i + 2) = name$(25 + i)
NEXT i
FOR i = 0 TO 1
 sbmax(i) = 9999
NEXT i
FOR i = 2 TO 8
 sbmax(i) = 999
NEXT i
FOR i = 9 TO 10
 sbmax(i) = 100
NEXT i
sbmax(11) = 10

csr = 0: top = -1
GOSUB litemname
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN GOTO doneitem
IF keyval(29) > 0 AND keyval(14) THEN
 cropafter csr, 255, -1, ".itm", buffer(), 200, game$, timing()
 GOSUB litemname
END IF
IF keyval(72) > 1 THEN csr = large(csr - 1, -1): IF csr < top THEN top = top - 1
IF keyval(80) > 1 THEN csr = small(csr + 1, 255): IF csr > top + 23 THEN top = top + 1
IF keyval(57) > 1 OR keyval(28) > 1 THEN
 IF csr = -1 THEN GOTO doneitem
 GOSUB edititem
 setpicstuf a(), 200, -1
 storeset game$ + ".itm" + CHR$(0), csr, 0
 i = csr: GOSUB sitemname
END IF
FOR i = top TO top + 23
 textcolor 7, 0
 IF i = csr THEN textcolor 14 + tog, 0
 temp$ = STR$(i) + " " + item$(i)
 IF i < 0 THEN temp$ = "Return to Main Menu"
 printstr temp$, 0, (i - top) * 8, dpage
NEXT i
SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

edititem:
setpicstuf a(), 200, -1
loadset game$ + ".itm" + CHR$(0), csr, 0
a(9) = small(a(9), 35)
info$ = ""
FOR o = 10 TO 9 + a(9)
 info$ = info$ + CHR$(small(large(a(o), 0), 255))
NEXT o

menu$(0) = "Back to Item Menu"
menu$(16) = "Stat Bonuses..."
menu$(17) = "Bitmask..."
menu$(18) = "Who Can Equip?..."
max(3) = 32767
max(4) = general(34) + 1
max(5) = general(34) + 1
max(6) = 5
max(7) = general(34) + 1
max(8) = general(34) + 1
max(9) = general(31)
max(10) = 99
max(11) = 2
max(12) = 500
max(13) = 500
max(14) = 500
max(15) = 500

setpicstuf a(), 200, -1
loadset game$ + ".itm" + CHR$(0), csr, 0
GOSUB itemmenu

setpicstuf buffer(), 576, 2
loadset game$ + ".pt5" + CHR$(0), a(52), 0

setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN RETURN
IF keyval(72) > 1 THEN ptr = large(ptr - 1, 0)
IF keyval(80) > 1 THEN ptr = small(ptr + 1, 18)
IF keyval(28) > 1 OR keyval(57) > 1 THEN
 IF ptr = 0 THEN RETURN
 IF a(49) > 0 THEN
  IF ptr = 16 THEN GOSUB statbon: GOSUB itemmenu
  IF ptr = 17 THEN GOSUB ibitset: GOSUB itemmenu
  IF ptr = 18 THEN GOSUB equipbit: GOSUB itemmenu
 END IF
END IF
IF ptr >= 3 AND ptr <= 10 THEN
 min = 0: IF ptr = 8 THEN min = general(39) * -1
 IF intgrabber(a(46 + (ptr - 3)), min, max(ptr), 75, 77) THEN GOSUB itemmenu
END IF
IF ptr = 11 THEN
 IF intgrabber(a(73), 0, 2, 75, 77) THEN GOSUB itemmenu
END IF
IF ptr >= 12 AND ptr <= 15 THEN
 IF intgrabber(a(74 + (ptr - 12)), 0, max(ptr), 75, 77) THEN GOSUB itemmenu
END IF
IF ptr = 1 THEN
 strgrabber item$(csr), 8, keyv()
 menu$(1) = "Name:" + item$(csr)
END IF
IF ptr = 2 THEN
 strgrabber info$, 34, keyv()
 menu$(2) = "Info:" + info$
END IF
FOR i = 0 TO 18
 textcolor 7, 0
 IF ptr = i THEN textcolor 14 + tog, 0
 IF i >= 16 AND a(49) = 0 THEN
  textcolor 8, 0
  IF ptr = i THEN textcolor 6 + tog, 0
 END IF
 printstr menu$(i), 0, i * 8, dpage
NEXT i
IF a(49) = 1 THEN
 loadsprite buffer(), 0, 288, 0, 24, 24, 2
 drawsprite buffer(), 0, pal(), 16 * a(53), 280, 160, dpage
END IF
SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

itemmenu:
menu$(1) = "Name:" + item$(csr)
menu$(2) = "Info:" + info$
menu$(3) = "Value" + STR$(a(46))
n = 47: GOSUB itatkname
menu$(4) = "When used in battle- " + temp$
n = 48: GOSUB itatkname
menu$(5) = "When used as a Weapon- " + temp$
menu$(6) = "Equippable as- " + eqst$(large(small(a(49), 5), 0))
n = 50: GOSUB itatkname
menu$(7) = "Teach Spell- " + temp$
IF a(51) >= 0 THEN
 n = 51: GOSUB itatkname
 menu$(8) = "When used out of battle- " + temp$
ELSE
 menu$(8) = "When used out of battle- Text" + STR$(ABS(a(51)))
END IF
menu$(9) = "Weapon Picture" + STR$(a(52))
menu$(10) = "Weapon Palette" + STR$(a(53))
IF a(49) <> 1 THEN menu$(9) = "Weapon Picture N/A": menu$(10) = "Weapon Palette N/A"
menu$(11) = "Unlimited Use"
IF a(73) = 1 THEN menu$(11) = "Consumed By Use"
IF a(73) = 2 THEN menu$(11) = "Cannot be Sold/Dropped"
menu$(12) = "own item TAG" + STR$(a(74)) + " " + lmnemonic(a(74), game$)
menu$(13) = "is in inventory TAG" + STR$(a(75)) + " " + lmnemonic(a(75), game$)
menu$(14) = "is equipped TAG" + STR$(a(76)) + " " + lmnemonic(a(76), game$)
menu$(15) = "eqpt by active hero TAG" + STR$(a(77)) + " " + lmnemonic(a(77), game$)
IF ptr = 9 THEN
 setpicstuf buffer(), 576, 2
 loadset game$ + ".pt5" + CHR$(0), a(52), 0
END IF
RETURN

statbon:
ptr2 = 0
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN RETURN
IF keyval(72) > 1 THEN ptr2 = large(ptr2 - 1, -1)
IF keyval(80) > 1 THEN ptr2 = small(ptr2 + 1, 11)
IF keyval(28) > 1 OR keyval(57) > 1 THEN
 IF ptr2 = -1 THEN RETURN
END IF
IF ptr2 >= 0 THEN
 dummy = intgrabber(a(54 + ptr2), sbmax(ptr2) * -1, sbmax(ptr2), 75, 77)
END IF
textcolor 7, 0
IF ptr2 = -1 THEN textcolor 14 + tog, 0
printstr "Previous Menu", 0, 0, dpage
FOR i = 0 TO 11
 textcolor 7, 0
 IF ptr2 = i THEN textcolor 14 + tog, 0
 printstr name$(nof(i)) + " Bonus:" + STR$(a(54 + i)), 0, 8 + i * 8, dpage
NEXT i
SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

itatkname: 'n is the offset
temp$ = "NOTHING"
IF a(n) <= 0 THEN RETURN
temp$ = ""
setpicstuf buffer(), 80, -1
loadset game$ + ".dt6" + CHR$(0), a(n) - 1, 0
FOR i = 26 TO 25 + small(buffer(24), 40)
 temp$ = temp$ + CHR$(small(large(buffer(i), 0), 255))
NEXT i
RETURN

litemname:
setpicstuf a(), 200, -1
FOR i = 0 TO 255
 loadset game$ + ".itm" + CHR$(0), i, 0
 a(0) = large(small(a(0), 8), 0)
 item$(i) = ""
 FOR o = 1 TO a(0)
  item$(i) = item$(i) + CHR$(small(large(a(o), 0), 255))
 NEXT o
NEXT i
RETURN

sitemname:
setpicstuf a(), 200, -1
loadset game$ + ".itm" + CHR$(0), i, 0
a(0) = LEN(item$(i))
FOR o = 1 TO a(0)
 a(o) = ASC(MID$(item$(i), o, 1))
NEXT o
a(9) = LEN(info$)
FOR o = 10 TO 9 + a(9)
 a(o) = ASC(MID$(info$, o - 9, 1))
NEXT o
storeset game$ + ".itm" + CHR$(0), i, 0
RETURN

ibitset:
FOR i = 0 TO 7
ibit$(i) = "Weakness to " + name$(17 + i)
ibit$(i + 8) = "Strength to " + name$(17 + i)
ibit$(i + 16) = "Absorbs " + name$(17 + i)
NEXT i
FOR i = 0 TO 10
ibit$(24 + i) = "Immunity to " + con$(i)
NEXT i
FOR i = 0 TO 7
ibit$(35 + i) = "Permanent " + con$(11 + i)
NEXT i
bitset a(), 70, 42, ibit$(), timing(), vpage, dpage
RETURN

equipbit:
FOR i = 0 TO 59
 setpicstuf buffer(), 636, -1
 loadset game$ + ".dt0" + CHR$(0), i, 0
  ibit$(i) = ""
  FOR o = 1 TO buffer(0)
   ibit$(i) = ibit$(i) + CHR$(small(large(buffer(o), 0), 255))
  NEXT o
 ibit$(i) = "Equipable by " + ibit$(i)
NEXT i
csr2 = 0
top2 = -1
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN RETURN
IF (keyval(57) > 1 OR keyval(28) > 1) AND csr2 = -1 THEN RETURN
IF keyval(72) > 1 THEN csr2 = large(csr2 - 1, -1): IF csr2 < top2 THEN top2 = top2 - 1
IF keyval(80) > 1 THEN csr2 = small(csr2 + 1, 59): IF csr2 > top2 + 20 THEN top2 = top2 + 1
IF (keyval(57) > 1 OR keyval(28) > 1) AND csr2 >= 0 THEN setbit a(), 66, csr2, readbit(a(), 66, csr2) XOR 1
IF keyval(75) > 1 AND csr2 >= 0 THEN setbit a(), 66, csr2, 0
IF keyval(77) > 1 AND csr2 >= 0 THEN setbit a(), 66, csr2, 1
textcolor 10, 2: IF csr2 = -1 THEN textcolor 10 + 5 * tog, 2
IF top2 = -1 THEN printstr "Go Back", 0, 0, dpage
FOR i = top2 TO top2 + 20
textcolor 8 - readbit(a(), 66, i), 0: IF csr2 = i THEN textcolor 7 + (readbit(a(), 66, i) * 7) + tog, 0
printstr ibit$(i), 0, (i - top2) * 8, dpage
NEXT i
SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

doneitem:
clearpage 0
clearpage 1
clearpage 2
clearpage 3

'SHOP STUFF
'0       Name length
'1-8     Name
'9       description length
'10-45   description
'46      cash value
'47      attack when used in battle
'48      attack as weapon
'49      Equip style
'        0 never
'        1 weapon
'        2 shield
'        3 armor
'        4 head
'        5 ring/gauntlet
'50      Learn when used out of battle
'51      Attack when use oob
'52      Weapon picture
'53      weapon pal
'54      Hp bonus
'55      Mp bonus
'56      Str bonus
'57      Acc bonus
'58      Def bonus
'59      Dodge bonus
'60      Mag bonus
'61      Will bonus
'62      Speed bonus
'63      Counter bonus
'64      Focus bonus
'65      Xhits bonus
'66-69   equipability bitsets
'70-72   bitsetmask
'73      Consumed by use
'74      when have tag
'75      is in inventory
'76      is equiped tag
'77      is equiped by hero in active party

END SUB

FUNCTION large (n1, n2)
large = n1
IF n2 > n1 THEN large = n2
END FUNCTION

FUNCTION loopvar (var, min, max, inc)
a = var + inc
IF a > max THEN a = a - ((max - min) + 1): loopvar = a: EXIT FUNCTION
IF a < min THEN a = a + ((max - min) + 1): loopvar = a: EXIT FUNCTION
loopvar = a
END FUNCTION

SUB npcdef (ptr, buffer(), game$, pal(), timing(), general(), filenum$(), npc$(), unpc(), lnpc())
DIM npc(1500), mtype$(10), push$(7), stepi(5), info$(5, 1)
vpage = 0: dpage = 1
clearpage 0: clearpage 1
setvispage vpage
xbload game$ + ".pal", pal(), "could not find 16-color palette"
unpc(4) = general(39)'max text boxes
unpc(12) = general(43)'max scripts
unpc(14) = general(55) + 1'max vehicles
mtype$(0) = "Stand Still"
mtype$(1) = "Wander"
mtype$(2) = "Pace"
mtype$(3) = "Right Turns"
mtype$(4) = "Left Turns"
mtype$(5) = "Random Turns"
mtype$(6) = "Chase You"
mtype$(7) = "Avoid You"
push$(0) = " Off"
push$(1) = " Full"
push$(2) = " Horizontal"
push$(3) = " Vertical"
push$(4) = " Up only"
push$(5) = " Right Only"
push$(6) = " Down Only"
push$(7) = " Left Only"
stepi(0) = 0
stepi(1) = 1
stepi(2) = 2
stepi(3) = 10
stepi(4) = 4
stepi(5) = 5
info$(0, 0) = ":Use"
info$(1, 0) = ":Touch"
info$(2, 0) = ":Step On"
info$(0, 1) = " Change Direction"
info$(1, 1) = " Face Player"
info$(2, 1) = " Do Not Face Player"
unpc(0) = general(30)

npcpick:
cur = 0: top = 0
xbload game$ + ".n" + filenum$(ptr), npc(), "could not find npc definitions for map " + filenum$(ptr)
FOR i = 0 TO 35
GOSUB loadnpcpic
NEXT i
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN DEF SEG = VARSEG(npc(0)): BSAVE game$ + ".n" + filenum$(ptr), VARPTR(npc(0)), 3000: GOTO npcdone
IF keyval(72) > 1 AND cur > 0 THEN cur = cur - 1: IF cur < top THEN top = top - 1
IF keyval(80) > 1 AND cur < 35 THEN cur = cur + 1: IF cur > top + 7 THEN top = top + 1
IF (keyval(57) > 1 OR keyval(28) > 1) THEN GOSUB npcstats
FOR i = top TO top + 7
 textcolor 7, 0
 IF cur = i THEN textcolor 14 + tog, 0
 printstr STR$(i), 0, ((i - top) * 25), dpage
 loadsprite buffer(), 0, 800, 5 * i, 20, 20, 2
 drawsprite buffer(), 0, pal(), 16 * npc(i * 15 + 1), 32, ((i - top) * 25), dpage
NEXT
SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

loadnpcpic:
setpicstuf buffer(), 1600, 2
loadset game$ + ".pt4" + CHR$(0), npc(i * 15 + 0), 5 * i
RETURN

npcstats:
GOSUB itstr
GOSUB frstline
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF npc(cur * 15 + 2) = 1 THEN walk = walk + 1: IF walk > 3 THEN walk = 0
IF keyval(1) > 1 THEN RETURN
IF keyval(72) > 1 AND csr > -1 THEN csr = csr - 1
IF keyval(80) > 1 AND csr < 14 THEN csr = csr + 1
IF csr = 11 THEN
 IF keyval(75) > 1 OR keyval(77) > 1 OR keyval(57) > 1 OR keyval(28) > 1 THEN GOSUB onetimetog
END IF
IF (csr >= 1 AND csr < 11) OR csr > 11 THEN
 IF intgrabber(npc(cur * 15 + csr), lnpc(csr), unpc(csr), 75, 77) THEN
  IF csr = 6 THEN GOSUB itstr
  IF csr = 4 THEN GOSUB frstline
 END IF
END IF
IF csr = 0 THEN
 IF intgrabber(npc(cur * 15 + csr), lnpc(csr), unpc(csr), 75, 77) = 1 THEN
  i = cur
  GOSUB loadnpcpic
 END IF
END IF
IF (keyval(57) > 1 OR keyval(28) > 1) AND csr = -1 THEN RETURN
textcolor 7, 0
IF csr = -1 THEN textcolor 14 + tog, 0
printstr "Previous Menu", 0, 0, dpage
FOR i = 0 TO 14
textcolor 7, 0
 IF csr = i THEN textcolor 14 + tog, 0
 temp$ = STR$(npc(cur * 15 + i))
 SELECT CASE i
  CASE 2
   temp$ = " = " + mtype$(npc(cur * 15 + i))
  CASE 3
   temp$ = STR$(stepi(npc(cur * 15 + i)))
  CASE 5
   temp$ = info$(npc(cur * 15 + i), 1)
  CASE 6
   temp$ = " " + it$
  CASE 7
   temp$ = push$(npc(cur * 15 + i))
  CASE 8
   temp$ = info$(npc(cur * 15 + i), 0)
  CASE 9, 10
   IF npc(cur * 15 + i) THEN
    temp$ = STR$(ABS(npc(cur * 15 + i))) + " = " + onoroff$(npc(cur * 15 + i)) + " (" + lmnemonic$(ABS(npc(cur * 15 + i)), game$) + ")"
   ELSE
    temp$ = " 0 (N/A)"
   END IF
  CASE 11
   IF npc(cur * 15 + i) THEN temp$ = " Only Once (tag" + STR$(1000 + npc(cur * 15 + i)) + ")" ELSE temp$ = " Repeatedly"
  CASE 12 'script
   temp$ = scriptname$(npc(cur * 15 + i), "plotscr.lst", general(), buffer())
  CASE 13 'script arg
   IF npc(cur * 15 + 12) = 0 THEN temp$ = " N/A"
  CASE 14 'vehicle
   IF npc(cur * 15 + 14) <= 0 THEN
    temp$ = "No"
   ELSE
    setpicstuf buffer(), 80, -1
    loadset game$ + ".veh" + CHR$(0), npc(cur * 15 + 14) - 1, 0
    temp$ = STRING$(bound(buffer(0) AND 255, 0, 15), 0)
    array2str buffer(), 1, temp$
   END IF
 END SELECT
 printstr npc$(i) + temp$, 0, 8 + (8 * i), dpage
NEXT i
rectangle 9, 139, 22, 22, 15, dpage
rectangle 10, 140, 20, 20, 7, dpage
loadsprite buffer(), 0, 800 + (200 * INT(walk / 2)), 5 * cur, 20, 20, 2
drawsprite buffer(), 0, pal(), 16 * npc(cur * 15 + 1), 10, 140, dpage
a$ = "Appears if tag" + STR$(ABS(npc(cur * 15 + 9))) + " = " + onoroff$(npc(cur * 15 + 9)) + " and tag" + STR$(ABS(npc(cur * 15 + 10))) + " = " + onoroff$(npc(cur * 15 + 10))
IF npc(cur * 15 + 9) <> 0 AND npc(cur * 15 + 10) = 0 THEN a$ = "Appears if tag" + STR$(ABS(npc(cur * 15 + 9))) + " = " + onoroff$(npc(cur * 15 + 9))
IF npc(cur * 15 + 9) = 0 AND npc(cur * 15 + 10) <> 0 THEN a$ = "Appears if tag" + STR$(ABS(npc(cur * 15 + 10))) + " = " + onoroff$(npc(cur * 15 + 10))
IF npc(cur * 15 + 9) = 0 AND npc(cur * 15 + 10) = 0 THEN a$ = "Appears all the time"
textcolor 15, 0
printstr a$, 0, 190, dpage
textcolor 15, 1
printstr x$, 0, 170, dpage
SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

onetimetog:
IF npc(cur * 15 + 11) > 0 THEN
 setbit general(), 106, npc(cur * 15 + 11) - 1, 0
 npc(cur * 15 + 11) = 0: RETURN
END IF

i = 0
DO
 general(105) = loopvar(general(105), 0, 999, 1)
 i = i + 1: IF i > 1000 THEN RETURN
LOOP UNTIL readbit(general(), 106, general(105)) = 0
npc(cur * 15 + 11) = general(105) + 1
setbit general(), 106, general(105), 1
RETURN

itstr:
it$ = "NONE"
IF npc(cur * 15 + 6) = 0 THEN RETURN
setpicstuf buffer(), 200, -1
loadset game$ + ".itm" + CHR$(0), npc(cur * 15 + 6) - 1, 0
it$ = ""
FOR o = 1 TO small(buffer(0), 16)
 IF buffer(o) > 255 OR buffer(o) < 0 THEN buffer(o) = 0
 it$ = it$ + CHR$(buffer(o))
NEXT o
RETURN

frstline:
x$ = ""
IF npc(cur * 15 + 4) = 0 THEN RETURN
x$ = STRING$(38, 0)
setpicstuf buffer(), 400, -1
loadset game$ + ".say" + CHR$(0), npc(cur * 15 + 4), 0
array2str buffer(), 0, x$
RETURN

npcdone:
clearpage 0
clearpage 1
clearpage 2

'npc(i * 15 + 0) = picture
'npc(i * 15 + 1) = palette
'npc(i * 15 + 2) = move type
'npc(i * 15 + 3) = move speed
'npc(i * 15 + 4) = display text
'npc(i * 15 + 5) = when activated
'npc(i * 15 + 6) = give item
'npc(i * 15 + 7) = pushability
'npc(i * 15 + 8) = activation
'npc(i * 15 + 9) = appear if tag
'npc(i * 15 + 10) = appear if secondary tag
'npc(i * 15 + 11) = usable
'npc(i * 15 + 12) = run script
'npc(i * 15 + 13) = script argument
'npc(i * 15 + 14) = vehicle
'Picture,Palette,Move Type,Move Speed,Display Text,When Activated,"Give Item:"
'Pushability,Activation,Appear if Tag,Appear if Tag,Usable,*Unused*

END SUB

SUB readscatter (s$, lhold, array(), start)
DIM stray(10)
s$ = STRING$(20, "!")

FOR i = 0 TO lhold
 setbit stray(), 0, i, readbit(array(), start - 1, array(start + i))
NEXT i

array2str stray(), 0, s$
s$ = LEFT$(s$, INT((lhold + 1) / 8))

END SUB

FUNCTION small (n1, n2)
small = n1
IF n2 < n1 THEN small = n2
END FUNCTION

SUB strgrabber (s$, maxl, keyv())
 STATIC clip$
 IF keyval(14) > 1 AND LEN(s$) > 0 THEN s$ = LEFT$(s$, LEN(s$) - 1)
 shift = 0
 IF keyval(54) > 0 OR keyval(42) > 0 THEN shift = 1
 IF keyval(56) THEN shift = 2
 IF keyval(57) > 1 AND LEN(s$) < maxl THEN s$ = s$ + " "
 FOR i = 2 TO 53
  IF keyval(i) > 1 AND LEN(s$) < maxl AND keyv(i, shift) > 0 THEN s$ = s$ + CHR$(keyv(i, shift))
 NEXT i
 IF (keyval(29) > 0 AND keyval(82) > 1) OR ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(83)) OR (keyval(29) > 0 AND keyval(46) > 1) THEN clip$ = s$
 IF ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(82) > 1) OR (keyval(29) > 0 AND keyval(47) > 1) THEN s$ = LEFT$(clip$, maxl)
END SUB

SUB writescatter (s$, lhold, array(), start)
DIM stray(10)

s$ = LEFT$(s$, 20)
lhold = LEN(s$) * 8 - 1
str2array s$, stray(), 0

FOR i = 0 TO lhold
 trueb = readbit(stray(), 0, i)
 DO
  scatb = INT(RND * (16 + (i * 16)))
 LOOP UNTIL readbit(array(), start - 1, scatb) = trueb
 array(start + i) = scatb
NEXT i

FOR i = lhold + 1 TO 159
 array(start + i) = INT(RND * 4444)
NEXT i

END SUB

