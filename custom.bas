'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE SUB standardmenu (menu$(), size%, vis%, ptr%, top%, x%, y%, dpage%)
DECLARE SUB vehicles (game$, timing%(), general%(), buffer%(), keyv%())
DECLARE SUB verifyrpg (nn$(), game$, buffer%())
DECLARE SUB xbload (f$, array%(), e$)
DECLARE FUNCTION scriptname$ (num%, f$, gen%(), buffer%())
DECLARE FUNCTION getmapname$ (m%, game$, buffer%())
DECLARE FUNCTION numbertail$ (s$)
DECLARE SUB cropafter (index%, limit%, flushafter%, lump$, buffer%(), bytes%, game$, timing%())
DECLARE SUB scriptman (game$, gamedir$, vpage%, dpage%, timing%(), general(), buffer%(), song$())
DECLARE FUNCTION exclude$ (s$, x$)
DECLARE FUNCTION exclusive$ (s$, x$)
DECLARE SUB writescatter (s$, lhold%, array%(), start%)
DECLARE SUB readscatter (s$, lhold%, array%(), start%)
DECLARE SUB fontedit (font%(), game$, timing%(), vpage%, dpage%, keyv%(), buffer%())
DECLARE SUB savetanim (n%, tastuf%(), game$)
DECLARE SUB loadtanim (n%, tastuf%(), game$)
DECLARE SUB cycletile (cycle%(), tastuf%(), ptr%(), skip%())
DECLARE SUB testanimpattern (tastuf%(), taset%, timing%(), vpage%, dpage%, buffer%())
DECLARE FUNCTION usemenu (ptr%, top%, first%, last%, size%)
DECLARE FUNCTION heroname$ (num%, cond%(), a%(), game$)
DECLARE FUNCTION bound% (n%, lowest%, highest%)
DECLARE FUNCTION onoroff$ (n%)
DECLARE FUNCTION intstr$ (n%)
DECLARE FUNCTION lmnemonic$ (index%, game$)
DECLARE SUB smnemonic (tagname$, index%, game$)
DECLARE SUB tagnames (game$, timing%(), keyv%())
DECLARE SUB sizemar (array%(), buffer%(), wide%, high%, tempx%, tempy%, tempw%, temph%, yout%, page%)
DECLARE SUB drawmini (high%, wide%, cursor%(), page%, tastuf%())
DECLARE FUNCTION rotascii$ (s$, o%)
DECLARE SUB debug (s$)
DECLARE SUB mapmaker (game$, font%(), master%(), buffer%(), pal%(), timing%(), map%(), pass%(), emap%(), f$(), general%(), doors%(), link%(), npc%(), npcstat%(), song$(), filenum$(), npc$(), unpc%(), lnpc%(), keyv%())
DECLARE SUB npcdef (ptr%, buffer%(), game$, pal%(), timing%(), general%(), filenum$(), npc$(), unpc%(), lnpc%())
DECLARE SUB bitset (array%(), wof%, last%, name$(), timing%(), vpage%, dpage%)
DECLARE SUB upgrade (general%(), buffer%(), game$, f$(), font%())
DECLARE SUB sprite (buffer%(), xw%, yw%, pal%(), timing%(), game$, sets%, perset%, soff%, foff%, atatime%, info$(), size%, zoom%, file$, master%(), font%())
DECLARE FUNCTION needaddset (ptr%, check%, what$, vpage%, dpage%, timing%())
DECLARE SUB shopdata (game$, timing%(), buffer%(), keyv%(), general%())
DECLARE FUNCTION intgrabber (n%, min%, max%, less%, more%)
DECLARE SUB strgrabber (s$, maxl%, keyv%())
DECLARE SUB importsong (song$(), buffer%(), game$, timing%(), general%(), keyv%(), master())
DECLARE SUB edgeprint (s$, x%, y%, c%, p%)
DECLARE SUB gendata (game$, general%(), timing%(), buffer%(), song$(), keyv%())
DECLARE SUB itemdata (game$, timing%(), general%(), pal%(), buffer%(), keyv%(), con$())
DECLARE SUB formation (game$, timing%(), general%(), pal%(), buffer%(), song$())
DECLARE SUB importbmp (buffer%(), game$, timing%(), general%(), keyv%(), master%())
DECLARE SUB enemydata (game$, timing%(), general%(), pal%(), buffer%(), keyv%(), con$())
DECLARE SUB herodata (game$, timing%(), general%(), pal%(), buffer%(), keyv%(), con$())
DECLARE SUB attackdata (game$, atkdat$(), atklim%(), timing%(), general%(), pal%(), buffer%(), con$(), keyv%())
DECLARE SUB getnames (game$, stat$(), max%)
DECLARE SUB statname (game$, timing%(), general%(), name$(), keyv%())
DECLARE SUB textage (game$, timing%(), general%(), keyv(), buffer(), song$())
DECLARE FUNCTION sublist% (num%, s$(), timing%())
DECLARE SUB maptile (game$, master%(), buffer%(), timing%(), font(), general())
DECLARE FUNCTION small% (n1%, n2%)
DECLARE FUNCTION large% (n1%, n2%)
DECLARE FUNCTION loopvar% (var%, min%, max%, inc%)
'assembly subs and functions
DECLARE SUB setmodeX ()
DECLARE SUB restoremode ()
DECLARE SUB setpicstuf (buf(), BYVAL b, BYVAL p)
DECLARE SUB loadset (fil$, BYVAL i, BYVAL L)
DECLARE SUB storeset (fil$, BYVAL i, BYVAL L)
DECLARE SUB copypage (BYVAL page1, BYVAL page2)
DECLARE SUB setvispage (BYVAL page)
DECLARE SUB drawsprite (pic(), BYVAL picoff, pal(), BYVAL po, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB wardsprite (pic(), BYVAL picoff, pal(), BYVAL po, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB getsprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL page)
DECLARE SUB loadsprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL page)
DECLARE SUB stosprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB bigsprite (pic(), pal(), BYVAL p, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB hugesprite (pic(), pal(), BYVAL p, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB setdiskpages (buf(), BYVAL h, BYVAL L)
DECLARE SUB loadpage (fil$, BYVAL i, BYVAL p)
DECLARE SUB storepage (fil$, BYVAL i, BYVAL p)
DECLARE SUB bitmap2page (temp(), bmp$, BYVAL p)
DECLARE SUB loadbmp (f$, BYVAL x, BYVAL y, buf(), BYVAL p)
DECLARE SUB getbmppal (f$, mpal(), pal(), BYVAL o)
DECLARE FUNCTION bmpinfo (f$, dat())
DECLARE SUB setpal (pal())
DECLARE SUB clearpage (BYVAL page)
DECLARE SUB setfont (f())
DECLARE SUB printstr (s$, BYVAL x, BYVAL y, BYVAL p)
DECLARE SUB textcolor (BYVAL f, BYVAL b)
DECLARE SUB setitup (fil$, buff(), tbuff(), BYVAL p)
DECLARE FUNCTION resetdsp
DECLARE SUB playsnd (BYVAL n, BYVAL f)
DECLARE SUB closefile
DECLARE SUB rectangle (BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL c, BYVAL p)
DECLARE SUB drawline (BYVAL x1, BYVAL y1, BYVAL x2, BYVAL y2, BYVAL c, BYVAL p)
DECLARE SUB paintat (BYVAL x, BYVAL y, BYVAL c, BYVAL page, buf(), BYVAL max)
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
DECLARE SUB setkeys ()
DECLARE FUNCTION Keyseg ()
DECLARE FUNCTION keyoff ()
DECLARE FUNCTION keyval (BYVAL a)
DECLARE FUNCTION getkey ()
DECLARE SUB copyfile (s$, d$, buf())
DECLARE SUB findfiles (fmask$, BYVAL attrib, outfile$, buf())
DECLARE SUB lumpfiles (listf$, lump$, path$, buffer())
DECLARE SUB unlump (lump$, ulpath$, buffer())
DECLARE SUB unlumpfile (lump$, fmask$, path$, buf())
DECLARE SUB setupmusic (mbuf())
DECLARE SUB closemusic ()
DECLARE SUB stopsong ()
DECLARE SUB resumesong ()
DECLARE SUB resetfm ()
DECLARE SUB loadsong (f$)
'DECLARE SUB fademusic (BYVAL vol)
DECLARE FUNCTION getfmvol ()
DECLARE SUB setfmvol (BYVAL vol)
DECLARE FUNCTION setmouse (mbuf())
DECLARE SUB readmouse (mbuf())
DECLARE SUB movemouse (BYVAL x, BYVAL y)
DECLARE SUB array2str (arr(), BYVAL o, s$)
DECLARE SUB str2array (s$, arr(), BYVAL o)
DECLARE SUB getstring (path$)
DECLARE FUNCTION pathlength ()
DECLARE FUNCTION rpathlength ()
DECLARE FUNCTION envlength (e$)
DECLARE FUNCTION drivelist (drbuf())
DECLARE SUB setdrive (BYVAL drive)
DECLARE FUNCTION isfile (n$)
DECLARE FUNCTION isdir (dir$)
DECLARE FUNCTION isremovable (BYVAL d)
DECLARE FUNCTION isvirtual (BYVAL d)
DECLARE FUNCTION hasmedia (BYVAL d)

'--make stack bigger
CLEAR , , 2000

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

'version ID
version& = 20000303
version$ = "Incomplete O.H.R.RPG.C.E Editor"
IF COMMAND$ = "/V" THEN PRINT version$: PRINT "Version"; version&: SYSTEM

curdir$ = STRING$(pathlength, 0)
getstring curdir$
IF RIGHT$(curdir$, 1) = "\" AND LEN(curdir$) > 3 THEN curdir$ = LEFT$(curdir$, LEN(curdir$) - 1)
gamedir$ = STRING$(rpathlength, 0)
getstring gamedir$
IF RIGHT$(gamedir$, 1) = "\" AND LEN(gamedir$) > 3 THEN gamedir$ = LEFT$(gamedir$, LEN(gamedir$) - 1)
CHDIR gamedir$
setdrive ASC(UCASE$(LEFT$(gamedir$, 1))) - 65

DIM font(1024), master(767), buffer(16384), pal(1584), timing(4), joy(4), scroll(16002), pass(16002), emap(16002)
DIM menu$(22), option$(10), filenum$(99), general(500), npc$(15), unpc(15), lnpc(15), keyv(55, 2), stat$(60), doors(300), atkdat$(19), atklim(18, 1), con$(18), rpg$(255), hinfo$(7), einfo$(0), ainfo$(2), xinfo$(1), winfo$(7), link(1000), npc(1500),  _
npcstat(1500), song$(-1 TO 100)

GOSUB listmake

RANDOMIZE TIMER
setmodeX
xbload "ohrrpgce.mas", master(), "default master palette OHRRPGCE.MAS is missing"
xbload "ohrrpgce.fnt", font(), "default font OHRRPGCE.FNT is missing"
setpal master()
setfont font()
setdiskpages buffer(), 200, 0
GOSUB switchon
textcolor 15, 0
GOSUB readstuff
'voices = resetdsp
'setitup game$+".cbv" + CHR$(0),noise(), buffer(), 2

dpage = 1: vpage = 0: rate = 160: game$ = "": song$(-1) = "Return to Main Menu"
GOSUB chooserpg

GOSUB checkpass

clearpage 0
setvispage 0
textcolor 15, 0
printstr "UNLUMPING DATA: please wait.", 0, 0, 0

OPEN "working.tmp\__danger.tmp" FOR OUTPUT AS #1
 PRINT #1, "This file only exists if unlumping failed."
CLOSE #1

ERASE scroll, pass, emap
DIM lumpbuf(32767)
unlump game$ + ".rpg" + CHR$(0), "working.tmp\", lumpbuf()
ERASE lumpbuf
DIM scroll(16002), pass(16002), emap(16002)
verifyrpg filenum$(), "working.tmp\" + game$, buffer()

KILL "working.tmp\__danger.tmp"

game$ = "working.tmp\" + game$
IF NOT isfile(game$ + ".mas" + CHR$(0)) THEN copyfile "ohrrpgce.mas" + CHR$(0), game$ + ".mas" + CHR$(0), buffer()
DEF SEG = VARSEG(master(0)): BLOAD game$ + ".mas", VARPTR(master(0))
IF NOT isfile(game$ + ".fnt" + CHR$(0)) THEN copyfile "ohrrpgce.fnt" + CHR$(0), game$ + ".fnt" + CHR$(0), buffer()
DEF SEG = VARSEG(font(0)): BLOAD game$ + ".fnt", VARPTR(font(0))
xbload game$ + ".gen", general(), "general data is missing, RPG file corruption is likely"
upgrade general(), buffer(), game$, filenum$(), font()
GOSUB lsongstr
setfont font()
needf = 1

setkeys
GOSUB setmainmenu
DO:
setwait timing(), 90
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN
 SELECT CASE menumode
 CASE 0'--normal
  GOSUB relump
  IF quitnow > 0 THEN GOTO finis
 CASE 1'--graphics
  ptr = 0: menumode = 0: GOSUB setmainmenu
 END SELECT
END IF
IF keyval(72) > 1 THEN ptr = ptr - 1: IF ptr < 0 THEN ptr = mainmax
IF keyval(80) > 1 THEN ptr = ptr + 1: IF ptr > mainmax THEN ptr = 0
IF keyval(57) > 1 OR keyval(28) > 1 THEN
 SELECT CASE menumode
 CASE 0'--normal mode
  IF ptr = 0 THEN ptr = 0: menumode = 1: GOSUB setgraphicmenu
  IF ptr = 1 THEN mapmaker game$, font(), master(), buffer(), pal(), timing(), scroll(), pass(), emap(), filenum$(), general(), doors(), link(), npc(), npcstat(), song$(), filenum$(), npc$(), unpc(), lnpc(), keyv()
  IF ptr = 2 THEN statname game$, timing(), general(), stat$(), keyv()
  IF ptr = 3 THEN herodata game$, timing(), general(), pal(), buffer(), keyv(), con$()
  IF ptr = 4 THEN enemydata game$, timing(), general(), pal(), buffer(), keyv(), con$()
  IF ptr = 5 THEN attackdata game$, atkdat$(), atklim(), timing(), general(), pal(), buffer(), con$(), keyv()
  IF ptr = 6 THEN itemdata game$, timing(), general(), pal(), buffer(), keyv(), con$()
  IF ptr = 7 THEN shopdata game$, timing(), buffer(), keyv(), general()
  IF ptr = 8 THEN formation game$, timing(), general(), pal(), buffer(), song$()
  IF ptr = 9 THEN textage game$, timing(), general(), keyv(), buffer(), song$()
  IF ptr = 10 THEN vehicles game$, timing(), general(), buffer(), keyv()
  IF ptr = 11 THEN tagnames game$, timing(), keyv()
  IF ptr = 12 THEN importsong song$(), buffer(), game$, timing(), general(), keyv(), master()
  IF ptr = 13 THEN fontedit font(), game$, timing(), vpage, dpage, keyv(), buffer()
  IF ptr = 14 THEN gendata game$, general(), timing(), buffer(), song$(), keyv()
  IF ptr = 15 THEN scriptman game$, gamedir$, vpage, dpage, timing(), general(), buffer(), song$()
  IF ptr = 16 THEN
   GOSUB relump
   IF quitnow > 0 THEN GOTO finis
  END IF
 CASE 1'--graphics mode
  IF ptr = 0 THEN ptr = 0: menumode = 0: GOSUB setmainmenu
  IF ptr = 1 THEN maptile game$, master(), buffer(), timing(), font(), general()
  IF ptr = 2 THEN sprite buffer(), 20, 20, pal(), timing(), game$, general(30), 8, 5, 0, 7, winfo$(), 200, 4, ".pt4", master(), font()
  IF ptr = 3 THEN sprite buffer(), 32, 40, pal(), timing(), game$, general(26), 8, 16, 0, 3, hinfo$(), 640, 4, ".pt0", master(), font()
  IF ptr = 4 THEN sprite buffer(), 34, 34, pal(), timing(), game$, general(27), 1, 2, 0, 4, einfo$(), 578, 4, ".pt1", master(), font()
  IF ptr = 5 THEN sprite buffer(), 50, 50, pal(), timing(), game$, general(28), 1, 4, 1, 2, einfo$(), 1250, 2, ".pt2", master(), font()
  IF ptr = 6 THEN sprite buffer(), 80, 80, pal(), timing(), game$, general(29), 1, 10, 2, 1, einfo$(), 3200, 2, ".pt3", master(), font()
  IF ptr = 7 THEN sprite buffer(), 50, 50, pal(), timing(), game$, general(32), 3, 12, 0, 2, ainfo$(), 1250, 2, ".pt6", master(), font()
  IF ptr = 8 THEN sprite buffer(), 24, 24, pal(), timing(), game$, general(31), 2, 2, 0, 5, xinfo$(), 288, 4, ".pt5", master(), font()
  IF ptr = 9 THEN importbmp buffer(), game$, timing(), general(), keyv(), master()
 END SELECT
END IF

standardmenu menu$(), mainmax, 22, ptr, 0, 0, 0, dpage
'FOR i = 0 TO mainmax
' textcolor 7, 0
' IF ptr = i THEN textcolor 14 + tog, 0: printstr "-", 0, i * 8, dpage
' printstr menu$(i), 10, i * 8, dpage
'NEXT i

textcolor 6, 0
printstr version$ + STR$(version&), 0, 192, dpage

SWAP vpage, dpage
setvispage vpage
copypage 2, dpage
dowait
LOOP

setmainmenu:
mainmax = 16
menu$(0) = "Edit Graphics"
menu$(1) = "Edit Map Data"
menu$(2) = "Edit Stat Names"
menu$(3) = "Edit Hero Stats"
menu$(4) = "Edit Enemy Stats"
menu$(5) = "Edit Attacks"
menu$(6) = "Edit Items"
menu$(7) = "Edit Shops"
menu$(8) = "Edit Battle Formations"
menu$(9) = "Edit Text Boxes"
menu$(10) = "Edit Vehicles"
menu$(11) = "Edit Tag Names"
menu$(12) = "Import Music"
menu$(13) = "Edit Font"
menu$(14) = "Edit General Game Data"
menu$(15) = "Script Management"
menu$(16) = "Quit Editing"
RETURN

setgraphicmenu:
mainmax = 9
menu$(0) = "Back to the main menu"
menu$(1) = "Edit Maptiles"
menu$(2) = "Draw Walkabout Graphics"
menu$(3) = "Draw Hero Graphics"
menu$(4) = "Draw Small Enemy Graphics  34x34"
menu$(5) = "Draw Medium Enemy Graphics 50x50"
menu$(6) = "Draw Big Enemy Graphics    80x80"
menu$(7) = "Draw Attacks"
menu$(8) = "Draw Weapons"
menu$(9) = "Import Screens"
RETURN

chooserpg:

OPEN "rpg.lst" FOR INPUT AS #1
L = 2: csr = 2: top = 0
rpg$(0) = "CREATE NEW GAME"
rpg$(1) = "EXIT TO DOS"
DO
 IF L >= 255 THEN EXIT DO
 INPUT #1, temp$
 IF temp$ = "-END-" THEN EXIT DO
 rpg$(L) = LEFT$(temp$, LEN(temp$) - 4)
 L = L + 1
LOOP
CLOSE #1
L = L - 1

IF oldcrash = 1 THEN
 rpg$(0) = "RECOVER IT"
 rpg$(1) = "IGNORE IT"
 CALL findfiles("working.tmp\*.gen" + CHR$(0), 32, "crash.lst" + CHR$(0), buffer())
 OPEN "crash.lst" FOR INPUT AS #1
 DO WHILE NOT EOF(1)
  INPUT #1, a$
  a$ = LEFT$(a$, LEN(a$) - 4)
  GOSUB cleanup
 LOOP
 CLOSE #1
 KILL "crash.lst"
 clearpage 0
 setvispage 0
 textcolor 15, 0
 printstr "Run CUSTOM.EXE again.", 0, 0, 0
 w = getkey
 GOTO finis
END IF

setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN GOTO finis
IF keyval(72) > 1 THEN csr = large(csr - 1, 0): top = small(top, csr)
IF keyval(80) > 1 THEN csr = small(csr + 1, L): IF csr > top + 20 THEN top = top + 1
IF keyval(57) > 1 OR keyval(28) > 1 THEN
 IF csr = 0 THEN
  'IF game$ = "" THEN GOTO finis
  GOSUB gamestr
  IF game$ = "" GOTO nomakegame
  textcolor 6, 0
  printstr "Please Wait...", 0, 40, vpage
  printstr "Creating RPG File", 0, 50, vpage
  copyfile "ohrrpgce.new" + CHR$(0), game$ + ".rpg" + CHR$(0), buffer()
  printstr "Unlumping", 0, 60, vpage
  ERASE scroll, pass, emap
  DIM lumpbuf(32767)
  unlump game$ + ".rpg" + CHR$(0), "working.tmp\", lumpbuf()
  ERASE lumpbuf
  DIM scroll(16002), pass(16002), emap(16002)
  findfiles "working.tmp\ohrrpgce.*" + CHR$(0), 32, "temp.lst" + CHR$(0), buffer()
  printstr "Translumping", 0, 70, vpage
  OPEN "temp.lst" FOR INPUT AS #1
   'FOR i = 0 TO 19
   DO WHILE NOT EOF(1)
    a$ = ""
    INPUT #1, a$
    IF LEFT$(LCASE$(a$), 8) = "ohrrpgce" THEN
     a$ = RIGHT$(a$, LEN(a$) - 8)
     b$ = game$
    ELSE
     b$ = ""
    END IF
    copyfile "working.tmp\ohrrpgce" + a$ + CHR$(0), "working.tmp\" + b$ + a$ + CHR$(0), buffer()
   'NEXT i
   LOOP
  CLOSE #1
  KILL "working.tmp\ohrrpgce.*"
  printstr "Finalumping", 0, 80, vpage
  findfiles "working.tmp\" + game$ + ".*" + CHR$(0), 32, "temp.lst" + CHR$(0), buffer()
  ERASE scroll, pass, emap
  DIM lumpbuf(32767)
  lumpfiles "temp.lst" + CHR$(0), game$ + ".rpg" + CHR$(0), "working.tmp\", lumpbuf()
  ERASE lumpbuf
  DIM scroll(16002), pass(16002), emap(16002)
  RETURN
 END IF
 IF csr = 1 THEN GOTO finis
 game$ = rpg$(csr): RETURN
nomakegame:
END IF

standardmenu rpg$(), L, 22, csr, top, 0, 0, dpage
'FOR i = top TO top + 20
' textcolor 7, 0: IF csr = i THEN textcolor 14 + tog, 0
' printstr rpg$(i), 0, (i - top) * 8, dpage
'NEXT i

SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

gamestr:
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN game$ = "": RETURN
strgrabber game$, 8, keyv()
IF LEN(game$) > 0 THEN
 IF RIGHT$(game$, 1) = " " THEN game$ = LEFT$(game$, LEN(game$) - 1) + "_"
 IF ASC(RIGHT$(game$, 1)) < 48 THEN game$ = LEFT$(game$, LEN(game$) - 1)
 IF ASC(RIGHT$(game$, 1)) > 57 AND ASC(RIGHT$(game$, 1)) < 65 THEN game$ = LEFT$(game$, LEN(game$) - 1)
 IF ASC(RIGHT$(game$, 1)) > 90 AND ASC(RIGHT$(game$, 1)) < 94 THEN game$ = LEFT$(game$, LEN(game$) - 1)
 IF ASC(RIGHT$(game$, 1)) = 96 OR ASC(RIGHT$(game$, 1)) > 122 THEN game$ = LEFT$(game$, LEN(game$) - 1)
END IF
IF keyval(28) > 1 THEN
 FOR i = 2 TO L
  IF UCASE$(rpg$(i)) = UCASE$(game$) AND game$ <> "" THEN alert$ = game$ + " already exists": alert = 30: game$ = "": EXIT FOR
 NEXT i
 IF game$ <> "" THEN RETURN
END IF
textcolor 15, 0
printstr "Filename of New Game?", 160 - LEN("Filename of New Game?") * 4, 20, dpage
IF alert > 0 THEN printstr alert$, 160 - LEN(alert$) * 4, 40, dpage: alert = alert - 1
textcolor 14 + tog, 1
printstr game$, 160 - LEN(game$) * 4, 30, dpage
SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

cleanup:
temp = 0
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(72) > 1 AND temp = 1 THEN temp = 0
IF keyval(80) > 1 AND temp = 0 THEN temp = 1
IF keyval(57) > 1 OR keyval(28) > 1 THEN
 IF temp = 0 THEN
  IF isfile("working.tmp\__danger.tmp" + CHR$(0)) THEN
   textcolor 14, 4
   printstr "Data is corrupt, not safe to relump", 0, 100, vpage
   w = getkey
  ELSE '---END UNSAFE
   printstr "Saving as " + a$ + ".BAK", 0, 180, vpage
   printstr "LUMPING DATA: please wait...", 0, 190, vpage
   findfiles "working.tmp\" + a$ + ".*" + CHR$(0), 32, "temp.lst" + CHR$(0), buffer()
   ERASE scroll, pass, emap
   DIM lumpbuf(32767)
   lumpfiles "temp.lst" + CHR$(0), a$ + ".bak" + CHR$(0), "working.tmp\", lumpbuf()
   ERASE lumpbuf
   DIM scroll(16002), pass(16002), emap(16002)
   clearpage vpage
   printstr "the recovered data has been saved.", 0, 0, vpage
   printstr "if CUSTOM.EXE crashed last time you", 0, 8, vpage
   printstr "ran it and you lost work, you may", 0, 16, vpage
   printstr "be able to recover it. make a backup", 0, 24, vpage
   printstr "copy of " + a$ + ".RPG and then rename", 0, 32, vpage
   printstr a$ + ".BAK to " + a$ + ".RPG", 0, 40, vpage
   printstr "If you have questions, ask", 0, 56, vpage
   printstr "helpIcrashed@HamsterRepublic.com", 0, 64, vpage
   w = getkey
   RETURN
  END IF '---END RELUMP
 END IF
 IF temp = 1 THEN RETURN
END IF
textcolor 9, 0
printstr a$ + " was found unlumped", 0, 0, dpage
IF isfile("working.tmp\__danger.tmp" + CHR$(0)) THEN
 printstr "CUSTOM.EXE has crashed while unlumping", 0, 40, dpage
 printstr "The unlumped data cannot be safely", 0, 48, dpage
 printstr "recovered. Your last saved copy of", 0, 56, dpage
 printstr a$ + " has not been harmed.", 0, 64, dpage
END IF

standardmenu rpg$(), 1, 1, temp, 0, 0, 8, dpage
'FOR i = 0 TO 1
' textcolor 7, 0: IF temp = i THEN textcolor 14 + tog, 0
' printstr rpg$(i), 0, 8 + 8 * i, dpage
'NEXT i

SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

listmake:
CALL findfiles("*.rpg" + CHR$(0), 32, "rpg.lst" + CHR$(0), buffer())
OPEN "rpg.lst" FOR APPEND AS #1 LEN = 25
WRITE #1, "-END-"
CLOSE #1

IF NOT isdir("working.tmp" + CHR$(0)) THEN
 MKDIR "working.tmp"
ELSE
 oldcrash = 1
END IF
RETURN

relump:
DEF SEG = VARSEG(general(0)): BSAVE game$ + ".gen", VARPTR(general(0)), 1000
GOSUB ssongstr
rpg$(0) = "Continue Editing"
rpg$(1) = "Save Changes"
rpg$(2) = "Discard Changes"
quitnow = sublist(2, rpg$(), timing())
IF quitnow = 1 THEN
 GOSUB dorelump
END IF
IF quitnow = 2 THEN
 rpg$(0) = "I am sure I dont want to save."
 rpg$(1) = "I will save it after all."
 IF sublist(1, rpg$(), timing()) = 1 THEN GOSUB dorelump
END IF
setkeys
RETURN

dorelump:
 clearpage 0
 setvispage 0
 textcolor 15, 0
 printstr "LUMPING DATA: please wait.", 0, 0, 0
 findfiles game$ + ".*" + CHR$(0), 32, "temp.lst" + CHR$(0), buffer()
 fptr = FREEFILE
 OPEN "temp.lst" FOR APPEND AS #fptr
  a$ = "plotscr.lst" + CHR$(0)
  PRINT #fptr, a$
  IF isfile("working.tmp\browse.txt" + CHR$(0)) THEN
   a$ = "BROWSE.TXT" + CHR$(0)
   PRINT #fptr, a$
  END IF
 CLOSE #fptr
 '--verify that maps are not corrupt--
 verifyrpg filenum$(), game$, buffer()
 '---KILL BUFFERS, LUMP, REDEFINE BUFFERS---
 ERASE scroll, pass, emap
 DIM lumpbuf(32767)
 lumpfiles "temp.lst" + CHR$(0), RIGHT$(game$, LEN(game$) - 12) + ".rpg" + CHR$(0), "working.tmp\", lumpbuf()
 ERASE lumpbuf
 DIM scroll(16002), pass(16002), emap(16002)
RETURN

checkpass:
unlumpfile game$ + ".rpg" + CHR$(0), game$ + ".gen", "working.tmp\", buffer()
xbload "working.tmp\" + game$ + ".gen", general(), "general data is missing, RPG file corruption is likely"
IF general(94) = -1 THEN RETURN
'----load password-----
readscatter rpas$, general(94), general(), 200
rpas$ = rotascii(rpas$, general(93) * -1)
'-----get inputed password-----
pas$ = ""
clearpage 0
clearpage 1
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(28) > 1 THEN
 IF pas$ = rpas$ OR pas$ = rotascii$("GtGz%_f%MfRzXf", -5) THEN RETURN ELSE GOTO finis
END IF
strgrabber pas$, 17, keyv()
textcolor 15, 0
printstr "This game requires a password to edit", 0, 0, dpage
printstr " Type it in and press ENTER", 0, 9, dpage
textcolor 14 + tog, 1
printstr pas$, 0, 20, dpage
SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

finis:
OPEN "working.tmp\kill.me" FOR OUTPUT AS #1
 PRINT #1, "this file shouldnt be inside your RPG file"
CLOSE #1
KILL "working.tmp\*.*"
RMDIR "working.tmp"
IF isfile("rpg.lst" + CHR$(0)) THEN KILL "rpg.lst"
IF isfile("temp.lst" + CHR$(0)) THEN KILL "temp.lst"
GOSUB shutoff
'closefile
CHDIR curdir$
'setdrive ASC(UCASE$(LEFT$(curdir$, 1))) - 64
CLEAR
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

readstuff:
RESTORE menuitems
FOR i = 0 TO 7
READ winfo$(i)
NEXT i
FOR i = 0 TO 7
READ hinfo$(i)
NEXT i
FOR i = 0 TO 0
READ einfo$(i)
NEXT i
FOR i = 0 TO 1
READ xinfo$(i)
NEXT i
FOR i = 0 TO 2
READ ainfo$(i)
NEXT i
'FOR i = 0 TO 19
'READ filenum$(i)
'NEXT i
o = 0
FOR i = 48 TO 57
 FOR j = 48 TO 57
  filenum$(o) = CHR$(i) + CHR$(j)
  o = o + 1
 NEXT j
NEXT i
FOR i = 0 TO 14
READ npc$(i)
NEXT i
FOR i = 0 TO 14
READ unpc(i)
NEXT i
FOR i = 0 TO 14
READ lnpc(i)
NEXT i

FOR o = 0 TO 2
 FOR i = 2 TO 53
  READ temp$
  IF temp$ <> "" THEN keyv(i, o) = ASC(temp$) ELSE keyv(i, o) = 0
 NEXT i
NEXT o

keyv(40, 1) = 34
READ temp
FOR i = 0 TO temp
READ stat$(i)
NEXT i
FOR i = 0 TO 19
READ atkdat$(i)
NEXT i
FOR o = 0 TO 1
FOR i = 0 TO 18
READ atklim(i, o)
NEXT i
NEXT o
FOR i = 0 TO 18
READ con$(i)
NEXT i
RETURN

ssongstr:
OPEN game$ + ".sng" FOR OUTPUT AS #1
FOR i = 0 TO 99
 WRITE #1, song$(i)
NEXT i
CLOSE #1
RETURN

lsongstr:
OPEN game$ + ".sng" FOR BINARY AS #1
temp& = LOF(1)
CLOSE #1
IF temp& = 0 THEN RETURN
OPEN game$ + ".sng" FOR INPUT AS #1
FOR i = 0 TO 99
 INPUT #1, song$(i)
NEXT i
CLOSE #1
RETURN

menuitems:
DATA "Up A","Up B","Right A","Right B","Down A","Down B","Left A","Left B"
DATA "Standing","Stepping","Attack A","Attack B","Cast/Use","Hurt","Weak","Dead"
DATA "Enemy (facing right)"
DATA "Frame 1","Frame 2"
DATA "First Frame","Middle Frame","Last Frame"
DATA Picture,Palette,Move Type,Move Speed,Display Text,When Activated,"Give Item:",Pushability,Activation,Appear if Tag,Appear if Tag,Usable,"Run Script: ", Script Argument,"Vehicle: "
DATA 119,99,7,5,-1,2,255,7,2,500,500,1,0,32767,0
DATA 0,0,0,0,0,0,0,0,0,-500,-500,0,0,-32767,0

DATA 1,2,3,4,5,6,7,8,9,0,-,=,"","",q,w,e,r,t,y,u,i,o,p,[,],"","",a,s,d,f,g,h,j,k,l,";","'",`,"",\,z,x,c,v,b,n,m,",",".","/"
DATA !,@,#,$,%,^,&,*,(,),_,+,"","",Q,W,E,R,T,Y,U,I,O,P,{,},"","",A,S,D,F,G,H,J,K,L,":"," ",~,"",|,Z,X,C,V,B,N,M,"<",">","?"
DATA "‚","ƒ","„","…","†","‡","ˆ","‰","Š","‹","Œ","","","","Ž","","","‘","’","“","”","•","–","—","˜","™","","","š","›","œ","","ž","Ÿ"," ","¡","¢","£","¤","¥","","¦","§","¨","©","ª","«","¬","­","®","¯","°"

DATA 32
DATA Health Points,Spell Points,Attack Power,Accuracy,Hit X,Blocking Power,Dodge Rate,Counter Rate,Speed,Enemy Type 1,Enemy Type 2,Enemy Type 3,Enemy Type 4,Enemy Type 5,Enemy Type 6,Enemy Type 7,Enemy Type 8
DATA Elemental 1,Elemental 2,Elemental 3,Elemental 4,Elemental 5,Elemental 6,Elemental 7,Elemental 8,Armor 1,Armor 2, Armor 3,Armor 4,Spell Skill,Spell Block,"Spell cost %","Money"

DATA Picture,Palette,"Animation:","Target Class:","Target Setting:",Damage,Aim,"Base Stat:",Mp Cost,Hp Cost,Money Cost,Extra Damage,"Chain to:",Chain%,"Attacker Motion:","Attack Motion:",Delay,Number of Hits,Edit Bitsets...,"Name="
DATA 0,0,0,0,0,0,0,0,-999,-9999,-32767,-100,0,0,0,0,0,1,0
DATA 99,90,3,5,4,4,4,5,999,9999,32767,1000,300,100,8,10,1000,20,0

DATA Sleep,Stone,Poison,Plague,Blind,Mute,Insane,Slow,Weak,Soft,Curse,Power,Accuracy,Hyper,Shell,Dodge,Fast,Wall,Blessing

'---GENERIC LOOP HEAD---
'setkeys
'DO
'setwait timing(), 100
'setkeys
'tog = tog XOR 1
'IF keyval(1) > 1 THEN GOTO donewhatever

'---GENERIC LOOP TAIL---
'SWAP vpage, dpage
'setvispage vpage
'copypage 3, dpage
'dowait
'LOOP

'---DOCUMENTATION OF GENERAL DATA---
'* denotes obsolete fields
'0        number of maps
'1-20    *tilesetassignments
'1        title screen
'2        title music
'3        victory music
'4        default battle music
'5-25    *passcode
'26       max hero graphics 40
'27       max small enemy graphics 149
'28       max med enemy graphics 79
'29       max large graphics 29
'30       max npc graphics 119
'31       max weapon graphics 149
'32       max attack graphics 99
'33       max tilesets 14
'34       max attack definitions 200
'35       max hero definitions 59
'36       max enemy definitions 500
'37       max formations 1000
'38       max palettes 99
'39       max text boxes 999
'40       total available plotscripts
'41       new-game plotscript
'42       game-over plotscript
'43       highest numbered plotscript
'44       suspendstuff bits
'45       cameramode
'46       cameraarg1
'47       cameraarg2
'48       cameraarg3
'49       cameraarg4
'50       script backdrop
'51       days of play
'52       hours of play
'53       minutes of play
'54       seconds of play
'55       max vehicle types

'93       new passcode offset
'94       new passcode length
'95       RPG file format version ID
'96       starting gold
'97       last shop
'98      *old passcode offset
'99      *old passcode length
'100      last screen
'101      general bitsets
'102      starting X
'103      starting Y
'104      starting Map
'105      one-time-NPC indexer
'106-170  one-time-NPC placeholders
'199      start of password scattertable
'200-359  password mess

REM $STATIC
SUB mapmaker (game$, font(), master(), buffer(), pal(), timing(), map(), pass(), emap(), f$(), general(), doors(), link(), npc(), npcstat(), song$(), filenum$(), npc$(), unpc(), lnpc(), keyv())
DIM menubar(82), cursor(600), c1(200), c2(200), c4(200), c8(200), mode$(12), list$(12), temp$(12), ulim(4), llim(4), menu$(-1 TO 5), gmap(20), gd$(-1 TO 20), gdmax(20), gdmin(20), destdoor(300), tastuf(40), cycle(1), cycptr(1), cycskip(1), sampmap(2 _
), cursorpal(15)

xbload game$ + ".pal", pal(), "cant find 16-color palettes"
textcolor 15, 0

temp$ = ""
FOR i = 0 TO 15: temp$ = temp$ + CHR$(i): NEXT i
str2array temp$, cursorpal(), 0

'--create cursor
clearpage 2
rectangle 0, 0, 20, 20, 15, 2
rectangle 1, 1, 18, 18, 0, 2
rectangle 2, 2, 16, 16, 7, 2
rectangle 3, 3, 14, 14, 0, 2
getsprite cursor(), 200, 0, 0, 20, 20, 2
clearpage 2
rectangle 0, 0, 20, 20, 15, 2
rectangle 1, 1, 18, 18, 0, 2
rectangle 3, 3, 14, 14, 7, 2
rectangle 4, 4, 12, 12, 0, 2
getsprite cursor(), 400, 0, 0, 20, 20, 2

mode$(0) = "Picture Mode"
mode$(1) = "Passability Mode"
mode$(2) = "Door Placement Mode"
mode$(3) = "NPC Placement Mode"
mode$(4) = "Foe Mapping Mode"
vpage = 0: dpage = 1
menubar(0) = 160: menubar(1) = 1
sampmap(0) = 1
sampmap(1) = 1
GOSUB loadmenu

maptop = 0
GOSUB updatemapname
setkeys
DO
setwait timing(), 120
setkeys
IF keyval(1) > 1 THEN GOTO donemapping
IF keyval(72) > 1 AND ptr > -2 THEN ptr = ptr - 1: GOSUB updatemapname: IF ptr < maptop - 2 THEN maptop = maptop - 1
IF keyval(80) > 1 AND ptr < general(0) THEN ptr = ptr + 1: GOSUB updatemapname: IF ptr > maptop + 16 THEN maptop = maptop + 1
IF (keyval(57) > 1 OR keyval(28) > 1) AND ptr = -1 THEN GOSUB addmap
IF (keyval(57) > 1 OR keyval(28) > 1) AND ptr = -2 THEN GOTO donemapping
IF (keyval(57) > 1 OR keyval(28) > 1) AND ptr > -1 THEN GOSUB loadmap: GOSUB whattodo
tog = tog XOR 1
FOR i = maptop TO small(general(0) + 2, maptop + 18)
 textcolor 7, 0
 IF ptr = i - 2 THEN textcolor 14 + tog, 0
 IF i = 0 THEN printstr "Return to Main Menu", 0, (i - maptop) * 8, dpage
 IF i = 1 THEN
  printstr "Add a New Map", 0, (i - maptop) * 8, dpage
 END IF
 IF i > 1 THEN printstr "Map " + f$(i - 2), 0, (i - maptop) * 8, dpage
NEXT i
textcolor 15, 0
printstr mapname$, 0, 192, dpage
SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

updatemapname:
 IF ptr < 0 THEN
  mapname$ = ""
 ELSE
  mapname$ = getmapname$(ptr, game$, buffer())
 END IF
RETURN

whattodo:
x = 0: y = 0: mapx = 0: mapy = 0
list$(0) = "Return to Map Menu"
list$(1) = "Resize Map..."
list$(2) = "Edit NPCs..."
list$(3) = "Edit General Map Data..."
list$(4) = "Erase Map Data"
list$(5) = "Link Doors..."
list$(6) = "Edit Tilemap..."
list$(7) = "Edit Wallmap..."
list$(8) = "Place Doors..."
list$(9) = "Place NPCs..."
list$(10) = "Edit Foemap..."
list$(11) = "Map name:"
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN GOSUB savemap: RETURN
IF keyval(72) > 1 AND csr > 0 THEN csr = csr - 1
IF keyval(80) > 1 AND csr < 11 THEN csr = csr + 1
IF keyval(28) > 1 OR keyval(57) > 1 THEN
 IF csr = 0 THEN GOSUB savemap: RETURN
 IF csr = 1 THEN GOSUB sizemap
 IF csr = 2 THEN
  npcdef ptr, buffer(), game$, pal(), timing(), general(), filenum$(), npc$(), unpc(), lnpc()
  xbload game$ + ".n" + f$(ptr), npcstat(), "NPCstat lump has dissapeared!"
 END IF
 IF csr = 3 THEN
  GOSUB gmapdata
  loadpage game$ + ".til" + CHR$(0), gmap(0), 3
 END IF
 IF csr = 4 THEN GOSUB delmap
 IF csr = 5 THEN GOSUB linkdoor
 IF csr > 5 AND csr < 11 THEN editmode = csr - 6: GOSUB mapping
END IF
IF csr = 11 THEN strgrabber mapname$, 39, keyv()
list$(11) = "Map name:" + mapname$
IF LEN(list$(11)) > 40 THEN list$(11) = mapname$

standardmenu list$(), 11, 11, csr, 0, 0, 0, dpage
'FOR i = 0 TO 11
' textcolor 7, 0
' IF csr = i THEN textcolor 14 + tog, 0
' printstr list$(i), 0, 8 * i, dpage
'NEXT i

SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP
                            
gmapdata:
gd = 0
gd$(-1) = "Previous Menu"
gd$(0) = "Map Tileset:"
gd$(1) = "Ambient Music:"
gd$(2) = "Minimap Available:"
gd$(3) = "Save Anywhere:"
gd$(4) = "Display Map Name:"
gd$(5) = "Map Edge Mode:"
gd$(6) = "Default Edge Tile:"
gd$(7) = "Autorun Script: "
gd$(8) = "Script Argument:"
gd$(9) = "Harm-Tile Damage:"
gd$(10) = "Harm-Tile Flash:"
gd$(11) = "Foot Offset:"
gdmax(0) = general(33):  gdmin(0) = 0
gdmax(1) = 100:          gdmin(1) = 0
gdmax(2) = 1:            gdmin(2) = 0
gdmax(3) = 1:            gdmin(3) = 0
gdmax(4) = 255:          gdmin(4) = 0
gdmax(5) = 2:            gdmin(5) = 0
gdmax(6) = 255:          gdmin(6) = 0
gdmax(7) = general(43):  gdmin(7) = 0
gdmax(8) = 32767:        gdmin(8) = -32767
gdmax(9) = 32767:        gdmin(9) = -32767
gdmax(10) = 255:         gdmin(10) = 0
gdmax(11) = 20:          gdmin(11) = -20
FOR i = 0 TO 11
 gmap(i) = bound(gmap(i), gdmin(i), gdmax(i))
NEXT i
scr$ = scriptname$(gmap(7), "plotscr.lst", general(), buffer())
setkeys
DO
setwait timing(), 120
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN RETURN
IF keyval(72) > 1 AND gd > -1 THEN gd = gd - 1
IF keyval(80) > 1 AND gd < 11 THEN gd = gd + 1
IF gd = -1 THEN
 IF keyval(57) > 1 OR keyval(28) > 1 THEN RETURN
END IF
IF gd > -1 THEN
 IF intgrabber(gmap(gd), gdmin(gd), gdmax(gd), 75, 77) THEN
  IF gd = 7 THEN
   scr$ = scriptname$(gmap(7), "plotscr.lst", general(), buffer())
  END IF
 END IF
END IF
FOR i = -1 TO 11
 temp$ = ""
 SELECT CASE i
 CASE 0, 9
  temp$ = STR$(gmap(i))
 CASE 1
  IF gmap(1) = 0 THEN temp$ = " -none-" ELSE temp$ = STR$(gmap(1) - 1) + " " + song$(gmap(1) - 1)
 CASE 2, 3
  IF gmap(i) = 0 THEN temp$ = " NO" ELSE temp$ = " YES"
 CASE 4
  IF gmap(i) = 0 THEN temp$ = " NO" ELSE temp$ = STR$(gmap(i)) + " ticks"
 CASE 5
  SELECT CASE gmap(i)
  CASE 0
   temp$ = " Crop"
  CASE 1
   temp$ = " Wrap"
  CASE 2
   temp$ = " use default edge tile"
  END SELECT
 CASE 6
  IF gmap(5) = 2 THEN
   temp$ = STR$(gmap(i))
  ELSE
   temp$ = " N/A"
  END IF
 CASE 7
  temp$ = scr$
 CASE 8
  IF gmap(7) = 0 THEN
   temp$ = " N/A"
  ELSE
   temp$ = STR$(gmap(i))
  END IF
 CASE 10
  IF gmap(i) = 0 THEN
   temp$ = " none"
  ELSE
   temp$ = STR$(gmap(i))
  END IF
 CASE 11
  SELECT CASE gmap(i)
  CASE 0
   temp$ = " none"
  CASE IS < 0
   temp$ = " up" + STR$(ABS(gmap(i))) + " pixels"
  CASE IS > 0
   temp$ = " down" + STR$(gmap(i)) + " pixels"
  END SELECT
 END SELECT
 textcolor 7, 0
 IF i = gd THEN textcolor 14 + tog, 0
 printstr gd$(i) + temp$, 0, 8 + (8 * i), dpage
 IF i = 10 THEN rectangle 4 + (8 * LEN(gd$(i) + temp$)), 8 + (8 * i), 8, 8, gmap(i), dpage
NEXT i
IF gmap(5) = 2 THEN
 '--show default edge tile
 setmapdata sampmap(), sampmap(), 180, 0
 setmapblock 0, 0, gmap(6)
 drawmap 0, -180, 0, dpage
 rectangle 20, 180, 300, 20, 240, dpage
END IF
SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

mapping:
clearpage 2
'--load NPC graphics--
FOR i = 0 TO 35
 setpicstuf buffer(), 1600, 2
 loadset game$ + ".pt4" + CHR$(0), npcstat(i * 15 + 0), 5 * i
NEXT i

setkeys
DO
setwait timing(), 120
setkeys
IF keyval(59) > 1 THEN
 editmode = 0
END IF
IF keyval(60) > 1 THEN
 editmode = 1
END IF
IF keyval(61) > 1 THEN
 editmode = 2
END IF
IF keyval(62) > 1 THEN
 editmode = 3
END IF
IF keyval(63) > 1 THEN
 editmode = 4
END IF
IF keyval(1) > 1 THEN RETURN
IF keyval(15) > 1 THEN tiny = tiny XOR 1
SELECT CASE editmode
'---TILEMODE------
CASE 0
 setmapdata map(), pass(), 20, 0
 IF keyval(33) > 1 AND keyval(29) > 0 THEN
  FOR i = 0 TO 14
   FOR o = 0 TO 8
    setmapblock INT(mapx / 20) + i, INT(mapy / 20) + o, pic
   NEXT o
  NEXT i
  setmapdata map(), pass(), 20, 0
 END IF
 IF keyval(41) > 1 THEN GOSUB minimap
 IF keyval(28) > 1 THEN GOSUB pickblock
 IF keyval(57) > 0 THEN setmapblock x, y, pic
 IF keyval(58) > 1 THEN pic = readmapblock(x, y): menu = pic: by = INT(pic / 16): bx = pic - (by * 16)
 IF keyval(2) > 1 THEN
  old = readmapblock(x, y)
  IF old > 159 THEN
   new = (old - 160) + tastuf(0)
  ELSE
   IF old >= tastuf(0) AND old < tastuf(0) + 48 THEN
    new = 160 + (old - tastuf(0))
   END IF
  END IF
  IF keyval(29) = 0 THEN
   setmapblock x, y, new
  ELSE
   FOR tx = 0 TO map(0)
    FOR ty = 0 TO map(1)
     IF readmapblock(tx, ty) = old THEN setmapblock tx, ty, new
    NEXT ty
   NEXT tx
  END IF
 END IF
 IF keyval(3) > 1 THEN
  old = readmapblock(x, y)
  IF old > 207 THEN
   new = (old - 208) + tastuf(20)
  ELSE
   IF old >= tastuf(20) AND old < tastuf(20) + 48 THEN
    new = 208 + (old - tastuf(20))
   END IF
  END IF
  IF keyval(29) = 0 THEN
   setmapblock x, y, new
  ELSE
   FOR tx = 0 TO map(0)
    FOR ty = 0 TO map(1)
     IF readmapblock(tx, ty) = old THEN setmapblock tx, ty, new
    NEXT ty
   NEXT tx
  END IF
 END IF
 IF keyval(51) > 0 AND pic > 0 THEN
  pic = pic - 1: bx = bx - 1
  IF bx < 0 THEN bx = 15: by = by - 1
  IF pic - menu < 0 AND pic + menu > 0 THEN menu = menu - 1
 END IF
 IF keyval(52) > 0 AND pic < 159 THEN
  pic = pic + 1: bx = bx + 1
  IF bx > 15 THEN bx = 0: by = by + 1
  IF pic - menu > 14 AND menu < 159 THEN menu = menu + 1
 END IF
'---PASSMODE-------
CASE 1
 setmapdata pass(), pass(), 20, 0
 over = readmapblock(x, y)
 IF keyval(57) > 1 AND (over AND 15) = 0 THEN setmapblock x, y, 15
 IF keyval(57) > 1 AND (over AND 15) = 15 THEN setmapblock x, y, 0
 IF keyval(57) > 1 AND (over AND 15) > 0 AND (over AND 15) < 15 THEN setmapblock x, y, 0
 IF keyval(29) > 0 THEN
  IF keyval(72) > 1 THEN setmapblock x, y, (over XOR 1)
  IF keyval(77) > 1 THEN setmapblock x, y, (over XOR 2)
  IF keyval(80) > 1 THEN setmapblock x, y, (over XOR 4)
  IF keyval(75) > 1 THEN setmapblock x, y, (over XOR 8)
 END IF
 IF keyval(30) > 1 THEN setmapblock x, y, (over XOR 16) 'vehicle A
 IF keyval(48) > 1 THEN setmapblock x, y, (over XOR 32) 'vehicle B
 IF keyval(35) > 1 THEN setmapblock x, y, (over XOR 64) 'harm tile
 IF keyval(24) > 1 THEN setmapblock x, y, (over XOR 128)'overhead
'---DOORMODE-----
CASE 2
 IF keyval(57) > 1 THEN
  temp = 0
  FOR i = 0 TO 99
   IF doors(i) = x AND doors(i + 100) = y + 1 AND doors(i + 200) = 1 THEN temp = 1: doors(i + 200) = 0
  NEXT
  IF temp = 0 THEN
   temp = -1
   FOR i = 99 TO 0 STEP -1
    IF doors(i + 200) = 0 THEN temp = i
   NEXT
   IF temp >= 0 THEN doors(0 + temp) = x: doors(100 + temp) = y + 1: doors(200 + temp) = 1
  END IF
 END IF
'---NPCMODE------
CASE 3
 IF keyval(83) > 1 THEN
  FOR i = 0 TO 299
  IF npc(i + 600) > 0 THEN
   IF npc(i + 0) = x AND npc(i + 300) = y + 1 THEN npc(i + 600) = 0
  END IF
  NEXT i
 END IF
 nd = -1
 IF keyval(72) > 1 THEN nd = 0
 IF keyval(77) > 1 THEN nd = 1
 IF keyval(80) > 1 THEN nd = 2
 IF keyval(75) > 1 THEN nd = 3
 IF keyval(57) > 1 OR (keyval(29) > 0 AND nd > -1) THEN
  temp = 0
  IF nd = -1 THEN
   FOR i = 0 TO 299
   IF npc(i + 600) > 0 THEN
    IF npc(i + 0) = x AND npc(i + 300) = y + 1 THEN npc(i + 600) = 0: temp = 1
   END IF
   NEXT i
  END IF
  IF nd = -1 THEN nd = 2
  IF temp = 0 THEN
   temp = -1
   FOR i = 299 TO 0 STEP -1
   IF npc(i + 600) = 0 THEN temp = i
   NEXT i
   IF temp >= 0 THEN npc(temp + 0) = x: npc(temp + 300) = y + 1: npc(temp + 600) = nptr + 1: npc(temp + 900) = nd
  END IF
 END IF
 IF keyval(51) > 0 THEN nptr = nptr - 1: IF nptr < 0 THEN nptr = 35
 IF keyval(52) > 0 THEN nptr = nptr + 1: IF nptr > 35 THEN nptr = 0
'---FOEMODE--------
CASE 4
 IF keyval(51) > 0 THEN foe = loopvar(foe, 0, 255, -1)
 IF keyval(52) > 0 THEN foe = loopvar(foe, 0, 255, 1)
 IF keyval(57) > 0 THEN setmapdata emap(), pass(), 20, 0: setmapblock x, y, foe: setmapdata map(), pass(), 20, 0
 IF keyval(33) > 1 AND keyval(29) > 0 THEN
  setmapdata emap(), pass(), 20, 0
  FOR i = 0 TO 14
   FOR o = 0 TO 8
    setmapblock INT(mapx / 20) + i, INT(mapy / 20) + o, foe
   NEXT o
  NEXT i
  setmapdata map(), pass(), 20, 0
 END IF
 IF keyval(58) > 1 THEN foe = readmapblock(x, y): menu = pic: by = INT(pic / 15): bx = pic - (by * 16)
'--done input-modes-------
END SELECT

setmapdata map(), pass(), 20, 0

'--general purpose controls----
IF keyval(56) = 0 AND keyval(29) = 0 THEN
 IF keyval(72) > 0 THEN y = large(y - 1, 0): IF y < INT(mapy / 20) AND mapy > 0 THEN mapy = mapy - 20
 IF keyval(80) > 0 THEN y = small(y + 1, high - 1): IF y > INT(mapy / 20) + 8 AND mapy < (high * 20) - 180 THEN mapy = mapy + 20
 IF keyval(75) > 0 THEN x = large(x - 1, 0): IF x < INT(mapx / 20) AND mapx > 0 THEN mapx = mapx - 20
 IF keyval(77) > 0 THEN x = small(x + 1, wide - 1): IF x > INT(mapx / 20) + 14 AND mapx < (wide * 20) - 300 THEN mapx = mapx + 20
END IF
IF keyval(56) > 0 AND keyval(29) = 0 THEN
 IF keyval(72) > 0 AND mapy > 0 THEN mapy = mapy - 20: y = y - 1
 IF keyval(80) > 0 AND mapy < (high * 20) - 180 THEN mapy = mapy + 20: y = y + 1
 IF keyval(75) > 0 AND mapx > 0 THEN mapx = mapx - 20: x = x - 1
 IF keyval(77) > 0 AND mapx < ((wide + 1) * 20) - 320 THEN mapx = mapx + 20: x = x + 1
END IF
tog = tog XOR 1
flash = loopvar(flash, 0, 3, 1)

'--draw menubar
IF editmode = 0 THEN
 setmapdata menubar(), pass(), 0, 180
 drawmap menu * 20, 0, 0, dpage
ELSE
 rectangle 0, 0, 320, 20, 0, dpage
END IF

'--draw map
setmapdata map(), pass(), 20, 0
setanim tastuf(0) + cycle(0), tastuf(20) + cycle(1)
cycletile cycle(), tastuf(), cycptr(), cycskip()
drawmap mapx, mapy - 20, 0, dpage

'--show passmode overlay
IF editmode = 1 THEN
  setmapdata pass(), pass(), 20, 0
  FOR o = 0 TO 8
   FOR i = 0 TO 15
    over = readmapblock(INT(mapx / 20) + i, INT(mapy / 20) + o)
    IF (over AND 1) THEN rectangle i * 20, o * 20 + 20, 20, 3, 7 + tog, dpage
    IF (over AND 2) THEN rectangle i * 20 + 17, o * 20 + 20, 3, 20, 7 + tog, dpage
    IF (over AND 4) THEN rectangle i * 20, o * 20 + 37, 20, 3, 7 + tog, dpage
    IF (over AND 8) THEN rectangle i * 20, o * 20 + 20, 3, 20, 7 + tog, dpage
    textcolor 14 + tog, 0
    IF (over AND 16) THEN printstr "A", i * 20, o * 20 + 20, dpage
    IF (over AND 32) THEN printstr "B", i * 20 + 10, o * 20 + 20, dpage
    IF (over AND 64) THEN printstr "H", i * 20, o * 20 + 30, dpage
    IF (over AND 128) THEN printstr "O", i * 20 + 10, o * 20 + 30, dpage
   NEXT i
  NEXT o
END IF

'--door display--
IF editmode = 2 THEN
 textcolor 240, 0
 FOR i = 0 TO 99
  IF doors(i) >= INT(mapx / 20) AND doors(i) < INT(mapx / 20) + 16 AND doors(i + 100) > INT(mapy / 20) AND doors(i + 100) <= INT(mapy / 20) + 9 AND doors(i + 200) = 1 THEN
   rectangle doors(i) * 20 - mapx, doors(i + 100) * 20 - mapy, 20, 20, 15 - tog, dpage
   printstr intstr$(i), doors(i) * 20 - mapx + 10 - (4 * (LEN(STR$(i)) - 1)), doors(i + 100) * 20 - mapy + 6, dpage
  END IF
 NEXT
END IF

'--npc display--
IF editmode = 3 THEN
 walk = walk + 1: IF walk > 3 THEN walk = 0
 FOR i = 0 TO 299
 IF npc(i + 600) > 0 THEN
  IF npc(i + 0) >= INT(mapx / 20) AND npc(i + 0) < INT(mapx / 20) + 16 AND npc(i + 300) > INT(mapy / 20) AND npc(i + 300) <= INT(mapy / 20) + 9 THEN
   loadsprite cursor(), 0, 400 * npc(i + 900) + (200 * INT(walk / 2)), 5 * (npc(i + 600) - 1), 20, 20, 2
   drawsprite cursor(), 0, pal(), 16 * npcstat((npc(i + 600) - 1) * 15 + 1), npc(i) * 20 - mapx, npc(i + 300) * 20 - mapy, dpage
   textcolor 14 + tog, 0
   temp$ = intstr$(npc(i + 600) - 1)
   printstr temp$, npc(i) * 20 - mapx, npc(i + 300) * 20 - mapy + 8, dpage
  END IF
 END IF
 NEXT
END IF

'--position finder--
IF tiny = 1 THEN rectangle 0, 20, wide, high, 1, dpage: rectangle mapx / 20, (mapy / 20) + 20, 15, 9, 10, dpage

'--normal cursor--
IF editmode <> 3 THEN
 drawsprite cursor(), 200 * (1 + tog), cursorpal(), 0, (x * 20) - mapx, (y * 20) - mapy + 20, dpage
 IF editmode = 0 THEN drawsprite cursor(), 200 * (1 + tog), cursorpal(), 0, ((pic - menu) * 20), 0, dpage
END IF

'--npc placement cursor--
IF editmode = 3 THEN
 loadsprite cursor(), 0, (walk * 400), nptr * 5, 20, 20, 2
 drawsprite cursor(), 0, pal(), 16 * (npcstat(nptr * 15 + 1)), (x * 20) - mapx, (y * 20) - mapy + 20, dpage
 textcolor 14 + tog, 0
 temp$ = intstr$(nptr)
 printstr temp$, (x * 20) - mapx, (y * 20) - mapy + 28, dpage
END IF

'--show foemap--
IF editmode = 4 THEN
 setmapdata emap(), pass(), 20, 0
 textcolor 14 + tog, 0
 FOR i = 0 TO 14
  FOR o = 0 TO 8
   temp = readmapblock(INT(mapx / 20) + i, INT(mapy / 20) + o)
   IF temp > 0 THEN printstr intstr$(temp), i * 20 + 10 - (4 * (LEN(STR$(temp)) - 1)), o * 20 + 26, dpage
  NEXT o
 NEXT i
END IF

textcolor 15, 0
printstr "X" + STR$(x) + "   Y" + STR$(y), 0, 192, dpage
setmapdata map(), pass(), 20, 0
rectangle 300, 0, 20, 200, 0, dpage
rectangle 0, 19, 320, 1, 15, dpage
textcolor 15, 0
printstr mode$(editmode), 0, 24, dpage
IF editmode = 4 THEN textcolor 15, 1: printstr "Formation Set:" + STR$(foe), 0, 16, dpage
SWAP vpage, dpage
setvispage vpage
dowait
LOOP

pickblock:
setkeys
DO
setwait timing(), 120
setkeys
IF keyval(28) > 1 OR keyval(1) > 1 THEN menu = pic: RETURN
IF keyval(72) > 0 AND by > 0 THEN by = by - 1: pic = pic - 16
IF keyval(80) > 0 AND by < 9 THEN by = by + 1: pic = pic + 16
IF keyval(75) > 0 AND bx > 0 THEN bx = bx - 1: pic = pic - 1
IF keyval(77) > 0 AND bx < 15 THEN bx = bx + 1: pic = pic + 1
IF keyval(51) > 0 AND pic > 0 THEN pic = pic - 1: bx = bx - 1: IF bx < 0 THEN bx = 15: by = by - 1
IF keyval(52) > 0 AND pic < 159 THEN pic = pic + 1: bx = bx + 1: IF bx > 15 THEN bx = 0: by = by + 1
tog = tog XOR 1
loadsprite cursor(), 0, 0, 0, 20, 20, 2
drawsprite cursor(), 200 * (1 + tog), cursorpal(), 0, bx * 20, by * 20, dpage
copypage dpage, vpage
copypage 3, dpage
dowait
LOOP

sizemap:
clearpage 2
tempx = 0: tempy = 0
tempw = wide: temph = high
setmapdata map(), pass(), 20, 0
drawmini high, wide, cursor(), 2, tastuf()
setkeys
DO
setwait timing(), 100
setkeys
IF keyval(1) > 1 THEN RETURN
IF keyval(28) > 1 THEN GOSUB dosizemap: RETURN
IF keyval(29) THEN
 IF keyval(72) > 0 THEN tempy = tempy - (1 + (keyval(56) * 8)): tempy = large(tempy, 0)
 IF keyval(80) > 0 THEN tempy = tempy + (1 + (keyval(56) * 8)): tempy = small(tempy, high - temph)
 IF keyval(75) > 0 THEN tempx = tempx - (1 + (keyval(56) * 8)): tempx = large(tempx, 0)
 IF keyval(77) > 0 THEN tempx = tempx + (1 + (keyval(56) * 8)): tempx = small(tempx, wide - tempw)
 tempx = large(tempx, 0)
 tempy = large(tempy, 0)
ELSE
 IF keyval(72) > 0 THEN temph = temph - (1 + (keyval(56) * 8)): temph = large(temph, 10)
 IF keyval(80) > 0 THEN temph = temph + (1 + (keyval(56) * 8)): temph = small(temph, 32000): WHILE temph * tempw > 32000: tempw = tempw - 1: WEND
 IF keyval(75) > 0 THEN tempw = tempw - (1 + (keyval(56) * 8)): tempw = large(tempw, 16)
 IF keyval(77) > 0 THEN tempw = tempw + (1 + (keyval(56) * 8)): tempw = small(tempw, 32000): WHILE temph * tempw > 32000: temph = temph - 1: WEND
 th& = temph
 tw& = tempw
 WHILE th& * tw& >= 32000
  temph = large(temph - 1, 10)
  tempw = large(tempw - 1, 16)
  th& = temph
  tw& = tempw
 WEND
END IF
edgeprint "width" + STR$(wide) + CHR$(26) + intstr$(tempw), 1, 1, 7, dpage
edgeprint "height" + STR$(high) + CHR$(26) + intstr$(temph), 1, 11, 7, dpage
edgeprint "area" + STR$(wide * high) + CHR$(26) + intstr$(temph * tempw), 1, 21, 7, dpage
rectangle tempx, tempy, tempw, 1, 14 + tog, dpage
rectangle tempx, tempy, 1, temph, 14 + tog, dpage
rectangle tempx, tempy + temph, tempw, 1, 14 + tog, dpage
rectangle tempx + tempw, tempy, 1, temph, 14 + tog, dpage
copypage dpage, vpage
copypage 2, dpage
dowait
LOOP

dosizemap:
clearpage 0
clearpage 1
yout = 0
edgeprint "TILEMAP", 0, yout * 10, 15, vpage: yout = yout + 1
sizemar map(), buffer(), wide, high, tempx, tempy, tempw, temph, yout, vpage
edgeprint "PASSMAP", 0, yout * 10, 15, vpage: yout = yout + 1
sizemar pass(), buffer(), wide, high, tempx, tempy, tempw, temph, yout, vpage
edgeprint "FOEMAP", 0, yout * 10, 15, vpage: yout = yout + 1
sizemar emap(), buffer(), wide, high, tempx, tempy, tempw, temph, yout, vpage
setmapdata map(), pass(), 20, 0
wide = map(0): high = map(1)
edgeprint "Aligning and truncating doors", 0, yout * 10, 15, vpage: yout = yout + 1
FOR i = 0 TO 99
 doors(i) = doors(i) - tempx
 doors(i + 100) = doors(i + 100) - tempy
 IF doors(i) < 0 OR doors(i + 100) < 0 OR doors(i) > wide OR doors(i + 100) > high THEN
  doors(i + 200) = 0
 END IF
NEXT
edgeprint "Aligning and truncating NPCs", 0, yout * 10, 15, vpage: yout = yout + 1
FOR i = 0 TO 299
 npc(i + 0) = npc(i + 0) - tempx
 npc(i + 300) = npc(i + 300) - tempy
 IF npc(i + 0) < 0 OR npc(i + 300) < 0 OR npc(i + 0) > wide OR npc(i + 300) > high THEN
  npc(i + 600) = 0
 END IF
NEXT i

GOSUB verifymap
RETURN

minimap:
clearpage vpage
setmapdata map(), pass(), 20, 0
drawmini high, wide, cursor(), vpage, tastuf()
printstr "Press Any Key", 0, 180, vpage
w = getkey
RETURN

delmap:
setvispage vpage
temp$(0) = "Do Not Delete"
temp$(1) = "Delete Map"
yesno = sublist(1, temp$(), timing())
IF yesno = 1 THEN
 printstr "Please Wait...", 0, 40, vpage
 map(0) = 32: map(1) = 20
 pass(0) = 32: pass(1) = 20
 emap(0) = 32: emap(1) = 20
 FOR i = 2 TO 16002
  rectangle INT(i * .02), 180, 2, 10, 15, vpage
  map(i) = 0
  pass(i) = 0
  emap(i) = 0
 NEXT i
 '---FLUSH DOOR LINKS---
 FOR i = 0 TO 1000
  link(i) = 0
 NEXT i
 '---FLUSH NPC LOCATIONS---
 FOR i = 0 TO 900
  npc(i) = 0
 NEXT i
 '---FLUSH DOOR LOCATIONS---
 FOR i = 0 TO 99
  doors(i) = 0
  doors(i + 100) = 0
  doors(i + 200) = 0
 NEXT
 DEF SEG = VARSEG(map(0)): BSAVE game$ + ".t" + f$(ptr), VARPTR(map(0)), map(0) * map(1) + 4
 DEF SEG = VARSEG(pass(0)): BSAVE game$ + ".p" + f$(ptr), VARPTR(pass(0)), pass(0) * pass(1) + 4
 DEF SEG = VARSEG(emap(0)): BSAVE game$ + ".e" + f$(ptr), VARPTR(emap(0)), emap(0) * emap(1) + 4
 DEF SEG = VARSEG(link(0)): BSAVE game$ + ".d" + f$(ptr), VARPTR(link(0)), 2000
 DEF SEG = VARSEG(npc(0)): BSAVE game$ + ".l" + f$(ptr), VARPTR(npc(0)), 3000
 setpicstuf doors(), 600, -1
 storeset game$ + ".dox" + CHR$(0), ptr, 0
END IF
RETURN

addmap:
IF general(0) >= 99 THEN RETURN
general(0) = general(0) + 1
FOR i = 0 TO 16002
 rectangle INT(i * .02), 180, 2, 10, 15, vpage
 map(i) = 0
 pass(i) = 0
NEXT i
FOR i = 0 TO 1000
 link(i) = 0
NEXT i
FOR i = 0 TO 900
 npc(i) = 0
NEXT i
FOR i = 0 TO 1500
 npcstat(i) = 0
NEXT i
FOR i = 0 TO 99
 doors(i) = 0
 doors(i + 100) = 0
 doors(i + 200) = 0
NEXT
map(0) = 64: map(1) = 64
pass(0) = 64: pass(1) = 64
emap(0) = 64: emap(1) = 64
DEF SEG = VARSEG(map(0)): BSAVE game$ + ".t" + f$(general(0)), VARPTR(map(0)), map(0) * map(1) + 4
DEF SEG = VARSEG(pass(0)): BSAVE game$ + ".p" + f$(general(0)), VARPTR(pass(0)), pass(0) * pass(1) + 4
DEF SEG = VARSEG(emap(0)): BSAVE game$ + ".e" + f$(general(0)), VARPTR(emap(0)), emap(0) * emap(1) + 4
DEF SEG = VARSEG(link(0)): BSAVE game$ + ".d" + f$(general(0)), VARPTR(link(0)), 2000
DEF SEG = VARSEG(npcstat(0)): BSAVE game$ + ".n" + f$(general(0)), VARPTR(npcstat(0)), 3000
DEF SEG = VARSEG(npc(0)): BSAVE game$ + ".l" + f$(general(0)), VARPTR(npc(0)), 3000
setpicstuf doors(), 600, -1
storeset game$ + ".dox" + CHR$(0), general(0), 0
'--setup map name
buffer(0) = 0
setpicstuf buffer(), 80, -1
storeset game$ + ".mn" + CHR$(0), general(0), 0
RETURN

savemap:
setpicstuf gmap(), 40, -1
storeset game$ + ".map" + CHR$(0), ptr, 0
DEF SEG = VARSEG(map(0)): BSAVE game$ + ".t" + f$(ptr), VARPTR(map(0)), map(0) * map(1) + 4
DEF SEG = VARSEG(pass(0)): BSAVE game$ + ".p" + f$(ptr), VARPTR(pass(0)), pass(0) * pass(1) + 4
DEF SEG = VARSEG(emap(0)): BSAVE game$ + ".e" + f$(ptr), VARPTR(emap(0)), emap(0) * emap(1) + 4
DEF SEG = VARSEG(npc(0)): BSAVE game$ + ".l" + f$(ptr), VARPTR(npc(0)), 3000
DEF SEG = VARSEG(link(0)): BSAVE game$ + ".d" + f$(ptr), VARPTR(link(0)), 2000
setpicstuf doors(), 600, -1
storeset game$ + ".dox" + CHR$(0), ptr, 0
'--save map name
buffer(0) = LEN(mapname$)
str2array LEFT$(mapname$, 39), buffer(), 1
setpicstuf buffer(), 80, -1
storeset game$ + ".mn" + CHR$(0), ptr, 0
RETURN

loadmap:
setpicstuf gmap(), 40, -1
loadset game$ + ".map" + CHR$(0), ptr, 0
loadpage game$ + ".til" + CHR$(0), gmap(0), 3
loadtanim gmap(0), tastuf(), game$
FOR i = 0 TO 1
 cycle(i) = 0
 cycptr(i) = 0
 cycskip(i) = 0
NEXT i
xbload game$ + ".t" + f$(ptr), map(), "tilemap lump is missing!"
xbload game$ + ".p" + f$(ptr), pass(), "passmap lump is missing!"
xbload game$ + ".e" + f$(ptr), emap(), "foemap lump is missing!"
xbload game$ + ".l" + f$(ptr), npc(), "npclocation lump is missing!"
xbload game$ + ".n" + f$(ptr), npcstat(), "npcstat lump is missing!"
xbload game$ + ".d" + f$(ptr), link(), "doorlink lump is missing!"
setpicstuf doors(), 600, -1
loadset game$ + ".dox" + CHR$(0), ptr, 0
wide = map(0): high = map(1)
mapname$ = getmapname$(ptr, game$, buffer())
GOSUB verifymap
RETURN

verifymap:
IF map(0) <> pass(0) OR map(0) <> emap(0) OR map(1) <> pass(1) OR map(1) <> emap(1) THEN
 '--Map's X and Y do not match
 clearpage vpage
 j = 0
 textcolor 15, 0
 printstr "Map" + f$(ptr) + ":" + mapname$, 0, j * 8, vpage: j = j + 1
 j = j + 1
 printstr "this map seems to be corrupted", 0, j * 8, vpage: j = j + 1
 j = j + 1
 printstr " TileMap" + STR$(map(0)) + "*" + LTRIM$(STR$(map(1))) + " tiles", 0, j * 8, vpage: j = j + 1
 printstr " WallMap" + STR$(pass(0)) + "*" + LTRIM$(STR$(pass(1))) + " tiles", 0, j * 8, vpage: j = j + 1
 printstr " FoeMap" + STR$(emap(0)) + "*" + LTRIM$(STR$(emap(1))) + " tiles", 0, j * 8, vpage: j = j + 1
 j = j + 1
 printstr "What is the correct size?", 0, j * 8, vpage: j = j + 1
 DO
  textcolor 14, 240
  setkeys
  DO
   setwait timing(), 100
   setkeys
   IF keyval(1) > 1 OR keyval(28) > 1 THEN EXIT DO
   dummy = intgrabber(wide, 0, 9999, 75, 77)
   printstr "Width:" + STR$(wide) + "   ", 0, j * 8, vpage
   dowait
  LOOP
  j = j + 1
  setkeys
  DO
   setwait timing(), 100
   setkeys
   IF keyval(1) > 1 OR keyval(28) > 1 THEN EXIT DO
   dummy = intgrabber(high, 0, 9999, 75, 77)
   printstr "Height:" + STR$(high) + "    ", 0, j * 8, vpage
   dowait
  LOOP
  textcolor 15, 0
  IF wide * high < 32000 AND wide > 0 AND high > 0 THEN EXIT DO
  j = j - 2
  printstr "What is the correct size? (bad size!)", 0, j * 8, vpage: j = j + 1
 LOOP
 map(0) = wide: map(1) = high
 pass(0) = wide: pass(1) = high
 emap(0) = wide: emap(1) = high
 j = j + 2
 printstr "please report this error to", 0, j * 8, vpage: j = j + 1
 printstr "ohrrpgce@HamsterRepublic.com", 0, j * 8, vpage: j = j + 1
 w = getkey
END IF
RETURN

loadmenu:
setmapdata menubar(), pass(), 180, 0
FOR i = 0 TO 159
setmapblock i, 0, i
NEXT
RETURN

linkdoor:
GOSUB savemap
ulim(0) = 99: llim(0) = 0
ulim(1) = 99: llim(1) = 0
ulim(2) = general(0): llim(2) = 0
ulim(3) = 500: llim(3) = -500
ulim(4) = 500: llim(4) = -500
ttop = 0: cur = 0
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN DEF SEG = VARSEG(link(0)): BSAVE game$ + ".d" + f$(ptr), VARPTR(link(0)), 2000: RETURN
IF keyval(72) > 1 AND cur > 0 THEN cur = cur - 1: IF cur < ttop THEN ttop = ttop - 1
IF keyval(80) > 1 AND cur < 199 THEN cur = cur + 1: IF cur > ttop + 10 THEN ttop = ttop + 1
IF keyval(28) > 1 OR keyval(57) > 1 THEN GOSUB seedoors
'IF keyval(51) > 0 AND link(cur + (cur2 * 200)) > llim(cur2) THEN link(cur + (cur2 * 200)) = link(cur + (cur2 * 200)) - 1
'IF keyval(52) > 0 AND link(cur + (cur2 * 200)) < ulim(cur2) THEN link(cur + (cur2 * 200)) = link(cur + (cur2 * 200)) + 1
'FOR i = 0 TO 4
' textcolor 7, 0
' IF cur2 = i THEN textcolor 10 + (tog * 5), 0
' printstr STR$(link(cur + (i * 200))), i * 50, 0, dpage
'NEXT i
FOR i = ttop TO ttop + 10
 textcolor 7, 0
 IF cur = i THEN textcolor 14 + tog, 0
 a$ = "Door" + STR$(link(i)) + " leads to door" + STR$(link(i + 200)) + " on map" + STR$(link(i + 400))
 printstr a$, 0, 2 + (i - ttop) * 16, dpage
 a$ = "  only if tag" + STR$(ABS(link(i + 600))) + " =" + STR$(SGN(SGN(link(i + 600)) + 1)) + " and tag" + STR$(ABS(link(i + 800))) + " =" + STR$(SGN(SGN(link(i + 800)) + 1))
 IF link(i + 600) = 0 AND link(i + 800) <> 0 THEN a$ = "  only if tag" + STR$(ABS(link(i + 800))) + " =" + STR$(SGN(SGN(link(i + 800)) + 1))
 IF link(i + 600) <> 0 AND link(i + 800) = 0 THEN a$ = "  only if tag" + STR$(ABS(link(i + 600))) + " =" + STR$(SGN(SGN(link(i + 600)) + 1))
 IF link(i + 600) = 0 AND link(i + 800) = 0 THEN a$ = "  all the time"
 printstr a$, 0, 10 + (i - ttop) * 16, dpage
NEXT i
SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

seedoors:
DEF SEG = VARSEG(map(0)): BSAVE game$ + ".t" + f$(ptr), VARPTR(map(0)), map(0) * map(1) + 4
menu$(-1) = "Go Back"
menu$(0) = "Entrance Door"
menu$(1) = "Exit Door"
menu$(2) = "Exit Map"
menu$(3) = "Require Tag"
menu$(4) = "Require Tag"
cur2 = -1
outmap$ = getmapname$(link(cur + 400), game$, buffer())
GOSUB showldoor
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF sdwait > 0 THEN
 sdwait = sdwait - 1
 IF sdwait = 0 THEN GOSUB showldoor
END IF
IF keyval(1) > 1 THEN RETURN
IF keyval(72) > 1 THEN cur2 = cur2 - 1: IF cur2 < -1 THEN cur2 = 4
IF keyval(80) > 1 THEN cur2 = cur2 + 1: IF cur2 > 4 THEN cur2 = -1
IF cur2 >= 0 THEN
 IF intgrabber(link(cur + (cur2 * 200)), llim(cur2), ulim(cur2), 75, 77) THEN sdwait = 3: outmap$ = getmapname$(link(cur + 400), game$, buffer())
ELSE
 IF keyval(28) > 1 OR keyval(57) > 1 THEN RETURN
END IF
rectangle 0, 100, 320, 2, 1 + tog, dpage
FOR i = -1 TO 4
 temp$ = ""
 IF i >= 0 AND i <= 2 THEN temp$ = STR$(link(cur + (i * 200)))
 IF i > 2 THEN
  IF link(cur + (i * 200)) THEN
   temp$ = STR$(ABS(link(cur + (i * 200)))) + " = " + onoroff$(link(cur + (i * 200))) + " (" + lmnemonic$(ABS(link(cur + (i * 200))), game$) + ")"
  ELSE
   temp$ = " 0 [N/A]"
  END IF
 END IF
 col = 7: IF cur2 = i THEN col = 14 + tog
 edgeprint menu$(i) + temp$, 1, 1 + (i + 1) * 10, col, dpage
NEXT i
edgeprint "ENTER", 275, 0, 15, dpage
edgeprint "EXIT", 283, 190, 15, dpage
edgeprint outmap$, 0, 190, 15, dpage
SWAP vpage, dpage
setvispage vpage
copypage 2, dpage
dowait
LOOP

showldoor:
clearpage 2
setmapdata map(), pass(), 0, 101
IF doors(link(cur + (0 * 200)) + 200) = 1 THEN
 dmx = doors(link(cur + (0 * 200))) * 20 - 150
 dmy = doors(link(cur + (0 * 200)) + 100) * 20 - 65
 dmx = small(large(dmx, 0), map(0) * 20 - 320)
 dmy = small(large(dmy, 0), map(1) * 20 - 100)
 drawmap dmx, dmy, 0, 2
 rectangle doors(link(cur + (0 * 200))) * 20 - dmx, doors(link(cur + (0 * 200)) + 100) * 20 - dmy - 20, 20, 20, 240, 2
 rectangle 1 + doors(link(cur + (0 * 200))) * 20 - dmx, 1 + doors(link(cur + (0 * 200)) + 100) * 20 - dmy - 20, 18, 18, 7, 2
 textcolor 240, 0
 temp$ = STR$(link(cur + (0 * 200)))
 printstr RIGHT$(temp$, LEN(temp$) - 1), doors(link(cur + (0 * 200))) * 20 - dmx + 10 - (4 * LEN(temp$)), doors(link(cur + (0 * 200)) + 100) * 20 - dmy - 14, 2
END IF
'-----------------EXIT DOOR
setpicstuf destdoor(), 600, -1
loadset game$ + ".dox" + CHR$(0), link(cur + (2 * 200)), 0
xbload game$ + ".t" + f$(link(cur + (2 * 200))), map(), "Could not find map" + f$(link(cur + (2 * 200)))
setpicstuf buffer(), 40, -1
loadset game$ + ".map" + CHR$(0), link(cur + (2 * 200)), 0
loadpage game$ + ".til" + CHR$(0), buffer(0), 3
setmapdata map(), pass(), 101, 0
IF destdoor(link(cur + (1 * 200)) + 200) = 1 THEN
 dmx = destdoor(link(cur + (1 * 200))) * 20 - 150
 dmy = destdoor(link(cur + (1 * 200)) + 100) * 20 - 65
 dmx = small(large(dmx, 0), map(0) * 20 - 320)
 dmy = small(large(dmy, 0), map(1) * 20 - 100)
 drawmap dmx, dmy - 100, 0, 2
 rectangle destdoor(link(cur + (1 * 200))) * 20 - dmx, destdoor(link(cur + (1 * 200)) + 100) * 20 - dmy + 80, 20, 20, 240, 2
 rectangle 1 + destdoor(link(cur + (1 * 200))) * 20 - dmx, 1 + destdoor(link(cur + (1 * 200)) + 100) * 20 - dmy + 80, 18, 18, 7, 2
 textcolor 240, 0
 temp$ = STR$(link(cur + (1 * 200)))
 printstr RIGHT$(temp$, LEN(temp$) - 1), destdoor(link(cur + (1 * 200))) * 20 - dmx + 10 - (4 * LEN(temp$)), destdoor(link(cur + (1 * 200)) + 100) * 20 - dmy + 86, 2
END IF
'-----------------RESET DATA
loadpage game$ + ".til" + CHR$(0), gmap(0), 3
xbload game$ + ".t" + f$(ptr), map(), "Tilemap lump disappeared!"
RETURN

donemapping:
clearpage 0
clearpage 1
clearpage 2
clearpage 3

'----
'gmap(20)
'0=tileset
'1=ambient music
'2=minimap
'3=save anywhere
'4=show map name
'5=map edge mode
'6=default edge tile
'7=autorun script
'8=script argument
'9=harm tile damage
'10=harm tile flash
'11=default foot-offset

'tiles
'1   north
'2   east
'4   south
'8   west
'16  vehicle A
'32  vehicle B
'64  harm tile
'128 overhead

END SUB

FUNCTION needaddset (ptr, check, what$, vpage, dpage, timing())
needaddset = 0
IF ptr > check THEN
 setkeys
 DO
  setwait timing(), 100
  setkeys
  tog = tog XOR 1
  IF keyval(1) > 1 THEN ptr = ptr - 1: EXIT DO
  IF keyval(72) > 1 OR keyval(80) > 1 OR keyval(75) > 1 OR keyval(77) > 1 THEN csr = csr XOR 1
  IF keyval(57) > 1 OR keyval(28) > 1 THEN
   IF csr = 0 THEN
    check = check + 1
    needaddset = -1
   ELSE
    ptr = ptr - 1
   END IF
   setkeys
   EXIT FUNCTION
  END IF
  rectangle 80, 10, 160, 40, 1, dpage
  edgeprint "Add new " + what$ + "?", 160 - LEN("Add new " + what$ + "?") * 4, 20, 15, dpage
  textcolor 7, 1: IF csr = 0 THEN textcolor 14 + tog, 9
  printstr " YES ", 120, 30, dpage
  textcolor 7, 1: IF csr = 1 THEN textcolor 14 + tog, 9
  printstr " NO ", 164, 30, dpage
  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
END IF
END FUNCTION

SUB shopdata (game$, timing(), buffer(), keyv(), general())
DIM name$(100), a(20), b(32), menu$(24), smenu$(24), max(24), min(24), sbit$(-1 TO 10), stf$(16)

vpage = 0: dpage = 1: max = 32: ptr = 0: it$ = "-NONE-"
sbit$(0) = "Buy"
sbit$(1) = "Sell"
sbit$(2) = "Hire"
sbit$(3) = "Inn"
sbit$(4) = "Equip"
sbit$(5) = "Save"
sbit$(6) = "Map"
sbit$(7) = "Team"
smenu$(0) = "Previous Menu"
max(3) = 2
min(5) = -1
max(5) = 99
FOR i = 6 TO 9
 min(i) = -500: max(i) = 500
NEXT i
min(10) = -32767
max(10) = 32767
max(11) = 256
min(13) = -32767
max(13) = 32767
max(14) = 256
stf$(0) = "Item"
stf$(1) = "Hero"
stf$(2) = "Script"
stf$(3) = "Normal"
stf$(4) = "Aquire Inventory"
stf$(5) = "Increment Inventory"
stf$(6) = "Refuse to Buy"
stf$(7) = "In Stock: Infinite"
stf$(8) = "In Stock: None"
clearpage 0
clearpage 1
clearpage 2
clearpage 3
OPEN game$ + ".stt" FOR BINARY AS #1
getnames game$, name$(), max
CLOSE #1

GOSUB lshopset
GOSUB menugen
li = 6
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN GOTO doneshop
IF keyval(29) > 0 AND keyval(14) THEN cropafter ptr, general(97), 0, ".sho", buffer(), 40, game$, timing(): GOSUB menugen
IF keyval(72) > 1 THEN csr = large(0, csr - 1)
IF keyval(80) > 1 THEN csr = small(li, csr + 1)
IF csr = 1 THEN
 IF keyval(75) > 1 AND ptr > 0 THEN GOSUB sshopset: ptr = ptr - 1: GOSUB lshopset
 IF keyval(77) > 1 AND ptr < 32767 THEN
  GOSUB sshopset
  ptr = ptr + 1
  IF needaddset(ptr, general(97), "Shop", vpage, dpage, timing()) THEN
   FOR i = 0 TO 19: a(i) = 0: NEXT i
   setpicstuf a(), 40, -1
   storeset game$ + ".sho" + CHR$(0), ptr, 0
  END IF
  GOSUB lshopset
 END IF
END IF
IF csr = 2 THEN
 strgrabber sn$, 16, keyv()
 GOSUB menuup
END IF
IF keyval(28) > 1 OR keyval(57) > 1 THEN
 IF csr = 0 THEN GOTO doneshop
 IF csr = 3 AND havestuf THEN GOSUB shopstuf: GOSUB sstuf
 IF csr = 4 THEN bitset a(), 17, 7, sbit$(), timing(), vpage, dpage: GOSUB menuup
END IF
IF csr = 5 THEN
 IF intgrabber(a(18), 0, 32767, 75, 77) THEN GOSUB menuup
END IF
IF csr = 6 THEN
 IF intgrabber(a(19), 0, general(43), 75, 77) THEN GOSUB menuup
END IF
FOR i = 0 TO li
 c = 7: IF i = csr THEN c = 14 + tog
 IF i = 3 AND havestuf = 0 THEN
  c = 8: IF i = csr THEN c = 6 + tog
 END IF
 textcolor c, 0
 printstr menu$(i), 0, i * 8, dpage
NEXT i
SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

menugen:
menu$(0) = "Return to Main Menu"
menu$(3) = "Edit Available Stuff..."
menu$(4) = "Edit Shop Bitsets..."
GOSUB menuup
RETURN

lshopset:
setpicstuf a(), 40, -1
loadset game$ + ".sho" + CHR$(0), ptr, 0
sn$ = ""
FOR i = 1 TO small(a(0), 15)
sn$ = sn$ + CHR$(a(i))
NEXT i
GOSUB menuup
RETURN

sshopset:
a(16) = small(a(16), 49)
a(0) = LEN(sn$)
FOR i = 1 TO small(a(0), 15)
 a(i) = ASC(MID$(sn$, i, 1))
NEXT i
setpicstuf a(), 40, -1
storeset game$ + ".sho" + CHR$(0), ptr, 0
RETURN

menuup:
menu$(1) = CHR$(27) + " Shop" + STR$(ptr) + " of" + STR$(general(97)) + CHR$(26)
menu$(2) = "Name:" + sn$
menu$(5) = "Inn Price:" + STR$(a(18))
IF readbit(a(), 17, 3) = 0 THEN menu$(5) = "Inn Price: N/A"
menu$(6) = "Inn Script: " + scriptname$(a(19), "plotscr.lst", general(), buffer())
IF readbit(a(), 17, 0) OR readbit(a(), 17, 1) OR readbit(a(), 17, 2) THEN havestuf = 1 ELSE havestuf = 0
RETURN

shopstuf:
thing = 0
tcsr = 0
last = 2
GOSUB lstuf
GOSUB othertype
GOSUB stufmenu
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN RETURN
 IF tcsr = 0 THEN IF keyval(57) > 1 OR keyval(28) > 1 THEN RETURN
 IF keyval(72) > 1 THEN tcsr = large(0, tcsr - 1)
 IF keyval(80) > 1 THEN tcsr = small(last, tcsr + 1)
 IF tcsr = 1 THEN
  IF keyval(75) > 1 AND thing > 0 THEN GOSUB sstuf: thing = thing - 1: GOSUB lstuf: GOSUB itstrsh
  IF keyval(77) > 1 AND thing < 49 THEN
   GOSUB sstuf
   thing = thing + 1
   IF needaddset(thing, a(16), "Shop Thing", vpage, dpage, timing()) THEN
    FOR i = 0 TO 31: b(i) = 0: NEXT i
    setpicstuf b(), 64, -1
    storeset game$ + ".stf" + CHR$(0), ptr * 50 + thing, 0
   END IF
   GOSUB lstuf
   GOSUB itstrsh
  END IF
 END IF
 IF tcsr = 2 THEN strgrabber thing$, 16, keyv()
 IF tcsr > 2 THEN
  IF b(17) = 1 THEN
    '--using a hero
    min(12) = -1
    max(12) = 99
  ELSE
    '--not a hero
    min(12) = 0: max(12) = 3
  END IF
  IF intgrabber(b(17 + tcsr - 3), min(tcsr), max(tcsr), 75, 77) THEN
   IF tcsr = 3 OR tcsr = 4 THEN GOSUB othertype: GOSUB setdefault
   IF tcsr = 11 OR tcsr = 14 THEN GOSUB itstrsh
   IF (b(26) < 0 OR b(26) > 3) AND b(17) <> 1 THEN b(26) = 0
  END IF
 END IF
 GOSUB stufmenu

standardmenu smenu$(), last, 22, tcsr, 0, 0, 0, dpage
' FOR i = 0 TO last
'  textcolor 7, 0: IF tcsr = i THEN textcolor 14 + tog, 0
'  printstr smenu$(i), 0, i * 8, dpage
' NEXT i

 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

othertype:
SELECT CASE b(17)
 CASE 0
  last = 14
  max(4) = 255: IF b(18) > 255 THEN b(18) = 0
  max(12) = 3
 CASE 1
  last = 12
  max(4) = 59: IF b(18) > 59 THEN b(18) = 0
  max(12) = 99
 CASE 2
  last = 11
  max(4) = 999
END SELECT
GOSUB itstrsh
RETURN

setdefault:
IF b(17) = 0 THEN
 setpicstuf buffer(), 200, -1
 loadset game$ + ".itm" + CHR$(0), b(18), 0
 thing$ = ""
 FOR o = 1 TO small(buffer(0), 10)
  IF buffer(o) < 256 AND buffer(o) > -1 THEN thing$ = thing$ + CHR$(buffer(o)) ELSE thing$ = ""
 NEXT o
 b(24) = buffer(46)
 b(27) = INT(buffer(46) / 2)
END IF
IF b(17) = 1 THEN
 thing$ = ""
 setpicstuf buffer(), 636, -1
 loadset game$ + ".dt0" + CHR$(0), b(18), 0
 FOR i = 1 TO small(buffer(0), 16)
  thing$ = thing$ + CHR$(buffer(i))
 NEXT i
END IF
IF b(17) = 2 THEN thing$ = "Unsupported"
RETURN

stufmenu:
smenu$(1) = CHR$(27) + "Shop Thing" + STR$(thing) + " of" + STR$(a(16)) + CHR$(26)
smenu$(2) = "Name: " + thing$
smenu$(3) = "Type:" + STR$(b(17)) + "-" + stf$(bound(b(17), 0, 2))
smenu$(4) = "Number:" + STR$(b(18)) + " " + nit$
IF b(19) > 0 THEN
 smenu$(5) = "In Stock:" + STR$(b(19))
ELSE
 smenu$(5) = stf$(8 + bound(b(19), -1, 0))
END IF
smenu$(6) = "Buy Require Tag" + STR$(ABS(b(20))) + " =" + STR$(SGN(SGN(b(20)) + 1)) + " (" + lmnemonic$(ABS(b(20)), game$) + ")"
IF b(20) = 1 THEN smenu$(6) = smenu$(6) + "[Never]"
IF b(20) = -1 THEN smenu$(6) = smenu$(6) + "[Always]"
IF b(20) = 0 THEN smenu$(6) = "[No Tag Check]"
smenu$(7) = "Sell Require Tag" + STR$(ABS(b(21))) + " =" + STR$(SGN(SGN(b(21)) + 1)) + " (" + lmnemonic$(ABS(b(21)), game$) + ")"
IF b(21) = 1 THEN smenu$(7) = smenu$(7) + "[Never]"
IF b(21) = -1 THEN smenu$(7) = smenu$(7) + "[Always]"
IF b(21) = 0 THEN smenu$(7) = "[No Tag Check]"
smenu$(8) = "Buy Set Tag" + STR$(ABS(b(22))) + " =" + STR$(SGN(SGN(b(22)) + 1)) + " (" + lmnemonic$(ABS(b(22)), game$) + ")"
IF b(22) = 1 THEN smenu$(8) = smenu$(8) + "[Unalterable]"
IF b(22) = -1 THEN smenu$(8) = smenu$(8) + "[Unalterable]"
IF b(22) = 0 THEN smenu$(8) = "[No Tag Set]"
smenu$(9) = "Sell Set Tag" + STR$(ABS(b(23))) + " =" + STR$(SGN(SGN(b(23)) + 1)) + " (" + lmnemonic$(ABS(b(23)), game$) + ")"
IF b(23) = 1 THEN smenu$(9) = smenu$(9) + "[Unalterable]"
IF b(23) = -1 THEN smenu$(9) = smenu$(9) + "[Unalterable]"
IF b(23) = 0 THEN smenu$(9) = "[No Tag Set]"
smenu$(10) = name$(32) + " Price:" + STR$(b(24))
smenu$(11) = "Must Trade:" + it$
IF b(17) = 0 THEN
 smenu$(12) = "Sell type: " + stf$(bound(b(26), 0, 3) + 3)
 smenu$(13) = "Sell Price:" + STR$(b(27))
 smenu$(14) = "Trade In For: " + trit$
END IF
IF b(17) = 1 THEN
 smenu$(12) = "Experience Level:" + STR$(b(26))
END IF
RETURN

lstuf:
setpicstuf b(), 64, -1
loadset game$ + ".stf" + CHR$(0), ptr * 50 + thing, 0
thing$ = ""
FOR i = 1 TO bound(b(0), 0, 16)
 thing$ = thing$ + CHR$(bound(b(i), 0, 255))
NEXT i
'---check for invalid data
IF b(17) < 0 OR b(17) > 2 THEN b(17) = 0
IF b(19) < -1 THEN b(19) = 0
IF (b(26) < 0 OR b(26) > 3) AND b(17) <> 1 THEN b(26) = 0
GOSUB menuup
RETURN

sstuf:
b(0) = LEN(thing$)
FOR i = 1 TO small(b(0), 16)
 b(i) = ASC(MID$(thing$, i, 1))
NEXT i
setpicstuf b(), 64, -1
storeset game$ + ".stf" + CHR$(0), ptr * 50 + thing, 0
RETURN

itstrsh:
IF b(25) = 0 THEN it$ = "-NONE-": GOTO ittrade
setpicstuf buffer(), 200, -1
loadset game$ + ".itm" + CHR$(0), b(25) - 1, 0
it$ = ""
FOR o = 1 TO small(buffer(0), 10)
 IF buffer(o) < 256 AND buffer(o) > -1 THEN it$ = it$ + CHR$(buffer(o)) ELSE it$ = ""
NEXT o
ittrade:
IF b(28) = 0 THEN trit$ = "-NONE-": RETURN
setpicstuf buffer(), 200, -1
loadset game$ + ".itm" + CHR$(0), b(28) - 1, 0
trit$ = ""
FOR o = 1 TO small(buffer(0), 10)
 IF buffer(o) < 256 AND buffer(o) > -1 THEN trit$ = trit$ + CHR$(buffer(o)) ELSE trit$ = ""
NEXT o
RETURN

doneshop:
GOSUB sshopset
clearpage 0
clearpage 1
clearpage 2
clearpage 3

'shopdata

'0    shop name length
'1-15 shop name
'16   stuff count
'17   bitsets
'18   inn price
'19   inn script


END SUB

SUB sizemar (array(), buffer(), wide, high, tempx, tempy, tempw, temph, yout, page)
'---FLUSH BUFFER---
edgeprint "Flushing buffer...", 0, yout * 10, 15, page: yout = yout + 1
buffer(0) = tempw
buffer(1) = temph
FOR i = 2 TO 16002
 buffer(i) = 0
NEXT i
'---WRITE RESIZED MAP INTO BUFFER
edgeprint "Resizing Map...", 0, yout * 10, 15, page: yout = yout + 1
FOR i = tempy TO tempy + temph
 FOR o = tempx TO tempx + tempw
  setmapdata array(), array(), 20, 0
  IF o < wide AND i < high THEN
   j = readmapblock(o, i)
  ELSE
   j = 0
  END IF
  setmapdata buffer(), buffer(), 20, 0
  setmapblock o - tempx, i - tempy, j
 NEXT o
NEXT i
'---MOVE BUFFER TO ARRAY---
edgeprint "Refreshing from buffer...", 0, yout * 10, 15, page: yout = yout + 1
FOR i = 0 TO 16002
 array(i) = buffer(i)
NEXT i

END SUB

SUB smnemonic (tagname$, index, game$)
DIM buf(20)
setpicstuf buf(), 42, -1

buf(0) = LEN(tagname$)
FOR i = 1 TO small(buf(0), 20)
 buf(i) = ASC(MID$(tagname$, i, 1))
NEXT i

storeset game$ + ".tmn" + CHR$(0), index, 0

END SUB

SUB statname (game$, timing(), general(), name$(), keyv())
DIM stat$(60)
vpage = 0: dpage = 1: max = 32
clearpage 0
clearpage 1
OPEN game$ + ".stt" FOR BINARY AS #1
getnames game$, stat$(), max

setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN GOTO donename
dummy = usemenu(ptr, top, 0, max, 22)
'IF keyval(72) > 1 AND ptr > 0 THEN ptr = ptr - 1: IF ptr < top THEN top = top - 1
'IF keyval(80) > 1 AND ptr < max THEN ptr = ptr + 1: IF ptr > top + 21 THEN top = top + 1
IF keyval(28) > 1 THEN GOSUB typestat

standardmenu name$(), max, 22, ptr, top, 0, 0, dpage
standardmenu stat$(), max, 22, ptr, top, 160, 0, dpage
'FOR i = top TO top + 21
' textcolor 7, 0
' IF i = ptr THEN textcolor 14 + tog, 0
' printstr name$(i), 0, (i - top) * 8, dpage
' printstr stat$(i), 160, (i - top) * 8, dpage
'NEXT i

'textcolor 15, 0: printstr STR$(ptr), 0, 192, dpage
SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

typestat:
setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 OR keyval(28) > 1 THEN RETURN
strgrabber stat$(ptr), 10, keyv()

standardmenu name$(), max, 22, ptr, top, 0, 0, dpage
standardmenu stat$(), max, 22, ptr, top, 160, 0, dpage
textcolor 15, 1
printstr stat$(ptr), 160, (ptr - top) * 8, dpage
'FOR i = top TO top + 15
' textcolor 7, 0
' IF i = ptr THEN textcolor 14 + tog, 0
' printstr name$(i), 0, (i - top) * 8, dpage
' IF i = ptr THEN textcolor 15, 1
' printstr stat$(i), 160, (i - top) * 8, dpage
'NEXT i

SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

donename:
FOR i = 0 TO max
temp$ = CHR$(LEN(stat$(i)))
PUT #1, 1 + (11 * i), temp$
PUT #1, 1 + (11 * i) + 1, stat$(i)
NEXT i
clearpage 0
clearpage 1
CLOSE #1
END SUB

FUNCTION sublist (num, s$(), timing())
dpage = 1: vpage = 0
clearpage 1: clearpage 0

setkeys
DO
 setwait timing(), 90
 setkeys
dummy = usemenu(ptr, 0, 0, num, 22)
' IF keyval(72) > 1 THEN ptr = large(ptr - 1, 0)
' IF keyval(80) > 1 THEN ptr = small(ptr + 1, num)
 IF keyval(57) > 1 OR keyval(28) > 1 THEN sublist = ptr: GOTO donelist
 tog = tog XOR 1

standardmenu s$(), num, 22, ptr, top, 0, 0, dpage
' FOR i = 0 TO num
'  textcolor 7, 0
'  IF ptr = i THEN textcolor 14 + tog, 0
'  printstr s$(i), 10, 8 * i, dpage
' NEXT i

 SWAP dpage, vpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP UNTIL keyval(1) > 1
sublist = -1
donelist:
clearpage 0
clearpage 1
END FUNCTION

SUB tagnames (game$, timing(), keyv())
DIM menu$(2)
vpage = 0: dpage = 1
clearpage 0
clearpage 1

ptr = 2
csr = 0
menu$(0) = "Previous Menu"
tagname$ = lmnemonic$(ptr, game$)

setkeys
DO
setwait timing(), 100
setkeys
tog = tog XOR 1
IF keyval(1) > 1 THEN GOTO donetagname
IF keyval(72) > 1 THEN csr = large(csr - 1, 0)
IF keyval(80) > 1 THEN csr = small(csr + 1, 2)
IF csr = 0 AND (keyval(57) > 1 OR keyval(28) > 1) THEN GOTO donetagname
IF csr = 1 THEN
 IF keyval(75) > 1 THEN
  smnemonic tagname$, ptr, game$
  ptr = large(ptr - 1, 0)
  tagname$ = lmnemonic$(ptr, game$)
 END IF
 IF keyval(77) > 1 THEN
  smnemonic tagname$, ptr, game$
  ptr = small(ptr + 1, 500)
  tagname$ = lmnemonic$(ptr, game$)
 END IF
END IF
IF csr = 2 THEN
 strgrabber tagname$, 20, keyv()
 IF keyval(28) > 1 THEN
  smnemonic tagname$, ptr, game$
  ptr = small(ptr + 1, 500)
  tagname$ = lmnemonic$(ptr, game$)
 END IF
END IF
menu$(1) = "Tag" + STR$(ptr)
menu$(2) = "Name:" + tagname$

standardmenu menu$(), 2, 22, csr, 0, 0, 0, dpage
'FOR i = 0 TO 2
' textcolor 7, 0
' IF csr = i THEN textcolor 14 + tog, 0
' printstr menu$(i), 0, i * 8, dpage
'NEXT i

SWAP vpage, dpage
setvispage vpage
clearpage dpage
dowait
LOOP

donetagname:
smnemonic tagname$, ptr, game$
END SUB

SUB upgrade (general(), buffer(), game$, f$(), font())

IF general(95) = 0 THEN
 general(95) = 1
 clearpage vpage
 printstr "Flushing New Text Data...", 0, 0, vpage
 setpicstuf buffer(), 400, -1
 FOR o = 0 TO 999
  loadset game$ + ".say" + CHR$(0), o, 0
  temp$ = STRING$(68, 0)
  str2array temp$, buffer(), 331
  storeset game$ + ".say" + CHR$(0), o, 0
 NEXT o
END IF
IF general(95) = 1 THEN
 general(95) = 2
 clearpage vpage
 printstr "Updating Door Format...", 0, 0, vpage
 FOR o = 0 TO 19
  IF isfile(game$ + ".dor" + CHR$(0)) THEN DEF SEG = VARSEG(buffer(0)): BLOAD game$ + ".dor", VARPTR(buffer(0))
  FOR i = 0 TO 299
   buffer(i) = buffer(o * 300 + i)
  NEXT i
  setpicstuf buffer(), 600, -1
  storeset game$ + ".dox" + CHR$(0), o, 0
 NEXT o
 printstr "Enforcing default font", 0, 16, vpage
 copyfile "ohrrpgce.fnt" + CHR$(0), game$ + ".fnt" + CHR$(0), buffer()
 DEF SEG = VARSEG(font(0)): BLOAD game$ + ".fnt", VARPTR(font(0))
 setfont font()
 printstr "Making AniMaptiles Backward Compatable", 0, 16, vpage
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
  savetanim n, buffer(), game$
 NEXT i
 FOR i = 0 TO general(0)
  printstr " map" + STR$(i), 16, 24 + i * 8, vpage
  DEF SEG = VARSEG(buffer(0)): BLOAD game$ + ".t" + f$(i), VARPTR(buffer(0))
  setmapdata buffer(), buffer(), 0, 0
  FOR tx = 0 TO buffer(0)
   FOR ty = 0 TO buffer(1)
    IF readmapblock(tx, ty) = 158 THEN setmapblock tx, ty, 206
   NEXT ty
  NEXT tx
  DEF SEG = VARSEG(buffer(0)): BSAVE game$ + ".t" + f$(i), VARPTR(buffer(0)), buffer(0) * buffer(1) + 4
 NEXT i
END IF
'---VERSION 3---
IF general(95) = 2 THEN
 general(95) = 3
 clearpage vpage
 '-GET OLD PASSWORD
 rpas$ = ""
 FOR i = 1 TO general(99)
  IF general(4 + i) >= 0 AND general(4 + i) <= 255 THEN rpas$ = rpas$ + CHR$(loopvar(general(4 + i), 0, 255, general(98) * -1))
 NEXT i
 '-SET SCATTERTABLE BASE
 general(199) = INT(RND * 15) + 1
 '-WRITE PASSWORD INTO SCATTERTABLE
 general(93) = INT(RND * 250) + 1
 rpas$ = rotascii(rpas$, general(93))
 writescatter rpas$, general(94), general(), 200
 '-REPLACE OLD PASSWORD
 pas$ = rotascii("ufxx|twi%|fx%rt{ji", -5)
 general(99) = LEN(pas$)
 general(98) = INT(RND * 250) + 1
 FOR i = 1 TO general(99)
  temp = ASC(MID$(pas$, i, 1))
  general(4 + i) = loopvar(temp, 0, 255, general(98))
 NEXT i
 printstr "Data Scaling Shtuff...", 0, 0, vpage
 general(26) = 40
 general(27) = 149
 general(28) = 79
 general(29) = 29
 general(30) = 119
 general(31) = 149
 general(32) = 99
 general(33) = 14
 general(34) = 200
 general(35) = 59
 general(36) = 500
 general(37) = 1000
 general(38) = 99
 general(39) = 999
END IF


'wow! this is quite a big and ugly routine!
END SUB

