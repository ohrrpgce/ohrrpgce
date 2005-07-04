'OHRRPGCE GAME - Even more various unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z
DECLARE FUNCTION rpad$ (s$, pad$, size%)
DECLARE FUNCTION isonscreen% (x%, y%)
DECLARE FUNCTION framewalkabout% (x%, y%, framex%, framey%, mapwide%, maphigh%, wrapmode%)
DECLARE SUB setScriptArg (arg%, value%)
DECLARE FUNCTION cropPlotStr% (s$)
DECLARE SUB wrapaheadxy (x%, y%, direction%, distance%, mapwide%, maphigh%, wrapmode%)
DECLARE SUB aheadxy (x%, y%, direction%, distance%)
DECLARE SUB wrapxy (x%, y%, wide%, high%)
DECLARE SUB loadSayToBuffer (say%)
DECLARE SUB safekill (f$)
DECLARE SUB touchfile (f$)
DECLARE SUB keyhandleroff ()
DECLARE FUNCTION partybyrank% (slot%)
DECLARE FUNCTION herobyrank% (slot%)
DECLARE FUNCTION rankincaterpillar% (heroid%)
DECLARE SUB embedtext (text$, limit%)
DECLARE SUB renamehero (who%)
DECLARE SUB fadeout (red%, green%, blue%, force%)
DECLARE SUB fadein (force%)
'basic subs and functions
DECLARE FUNCTION vehiclestuff% (disx%, disy%, foep%)
DECLARE FUNCTION trylearn% (who%, atk%, learntype%)
DECLARE SUB correctbackdrop (gmap%())
DECLARE FUNCTION gethighbyte% (n%)
DECLARE FUNCTION readbadbinstring$ (array%(), offset%, maxlen%, skipword%)
DECLARE FUNCTION readbinstring$ (array%(), offset%, maxlen%)
DECLARE SUB wrappedsong (songnumber%)
DECLARE SUB flusharray (array%(), size%, value%)
DECLARE SUB delitem (it%)
DECLARE FUNCTION readglobalstring$ (index%, default$, maxlen%)
DECLARE FUNCTION getnpcref% (seekid%, offset%)
DECLARE SUB suspendresume (id%)
DECLARE SUB scriptwatcher (page%)
DECLARE SUB onkeyscript (scriptnum%)
DECLARE SUB waitcommands (id%)
DECLARE SUB getpal16 (array%(), aoffset%, foffset%)
DECLARE SUB greyscalepal ()
DECLARE SUB tweakpalette ()
DECLARE SUB vishero (stat%())
DECLARE SUB forceparty (stat%())
DECLARE SUB scriptdump (s$)
DECLARE FUNCTION vehpass% (n%, tile%, default%)
DECLARE FUNCTION dignum$ (n%, dig%)
DECLARE FUNCTION readfoemap% (x%, y%, wide%, high%, fh%)
DECLARE FUNCTION playtime$ (d%, h%, m%)
DECLARE FUNCTION functiondone% ()
DECLARE FUNCTION functionread% ()
DECLARE FUNCTION averagelev% (stat%())
DECLARE FUNCTION countitem% (it%)
DECLARE SUB xbload (f$, array%(), e$)
DECLARE SUB fatalerror (e$)
DECLARE FUNCTION movdivis% (xygo%)
DECLARE FUNCTION onwho% (w$, alone)
DECLARE SUB minimap (scroll%(), mx%, my%, gmap%(), x%, y%, tastuf%())
DECLARE SUB heroswap (iAll%, stat%())
DECLARE FUNCTION shoption (inn%, price%, needf%, stat%())
DECLARE SUB savegame (slot%, map%, foep%, stat%(), stock%())
DECLARE FUNCTION runscript% (n%, index%, newcall%, er$)
DECLARE SUB scripterr (e$)
DECLARE FUNCTION unlumpone% (lumpfile$, onelump$, asfile$)
DECLARE SUB itstr (i%)
DECLARE FUNCTION findhero% (who%, f%, l%, d%)
DECLARE FUNCTION howmanyh% (f%, l%)
DECLARE FUNCTION consumeitem% (index%)
DECLARE FUNCTION istag% (num%, zero%)
DECLARE FUNCTION bound% (n%, lowest%, highest%)
DECLARE FUNCTION usemenu% (ptr%, top%, first%, last%, size%)
DECLARE SUB debug (s$)
DECLARE FUNCTION browse$ (fmask$, needf%)
DECLARE SUB doswap (s%, d%, stat%())
DECLARE SUB control ()
DECLARE FUNCTION pickload%
DECLARE FUNCTION picksave%
DECLARE SUB equip (ptr%, stat%())
DECLARE FUNCTION items% (stat%())
DECLARE SUB getitem (getit%)
DECLARE SUB oobcure (w%, t%, atk%, spred%, stat%())
DECLARE SUB spells (ptr%, stat%())
DECLARE SUB status (ptr%, stat%())
DECLARE SUB getnames (stat$())
DECLARE SUB centerfuz (x%, y%, w%, h%, c%, p%)
DECLARE SUB centerbox (x%, y%, w%, h%, c%, p%)
DECLARE SUB resetlmp (slot%, lev%)
DECLARE SUB loadfoe (i%, formdata%(), es%(), x%(), y%(), p%(), v%(), w%(), h%(), ext$(), bits%(), stat%(), ebits%(), batname$())
DECLARE FUNCTION inflict (w%, t%, stat%(), x%(), y%(), w%(), h%(), harm$(), hc%(), hx%(), hy%(), atk%(), tcount%, die%(), bits%())
DECLARE FUNCTION battle (form%, fatal%, exstat%())
DECLARE SUB addhero (who%, slot%, stat%())
DECLARE SUB edgeprint (s$, x%, y%, c%, p%)
DECLARE FUNCTION atlevel% (now%, a0%, a99%)
DECLARE FUNCTION range% (n%, r%)
DECLARE FUNCTION small% (n1%, n2%)
DECLARE FUNCTION large% (n1%, n2%)
DECLARE FUNCTION loopvar% (var%, min%, max%, inc%)
DECLARE FUNCTION xstring% (s$, x%)
DECLARE SUB snapshot ()
DECLARE FUNCTION checksaveslot (slot%)
DECLARE FUNCTION readitemname$ (itemnum%)
DECLARE FUNCTION readglobalstring$ (index%, default$, maxlen%)
DECLARE FUNCTION readatkname$ (id%)
DECLARE SUB getmapname (mapname$, m%)
DECLARE SUB defaultc ()

'$INCLUDE: 'allmodex.bi'
'$INCLUDE: 'gglobals.bi'
'$INCLUDE: 'sglobals.bi'

'$INCLUDE: 'const.bi'
'$INCLUDE: 'scrconst.bi'

REM $STATIC
FUNCTION framewalkabout (x, y, framex, framey, mapwide, maphigh, wrapmode)
'Given an X and a Y returns true if a walkabout at that
'spot would be on-screen, and false if off-screen.
'Also checks wraparound map, and sets framex and framey
'to the position on screen where the walkabout should
'be drawn (relative to the top-left corner of the screen,
'not the top left corner of the map)

'--by default, assume we will not draw the walkaout
yesdraw = 0

IF isonscreen(x, y) THEN
 '--walkabout is on-screen
 yesdraw = 1
 framex = x - mapx
 framey = y - mapy
ELSE
 IF wrapmode = 1 THEN
  '--in wrap-mode
  '--I hope this checking isn't too slow!
  DO '--just so I can exit do
   IF isonscreen(x - mapwide, y) THEN
    '--off-left
    yesdraw = 1
    framex = (x - mapwide) - mapx
    framey = y - mapy
    EXIT DO
   END IF
   IF isonscreen(x + mapwide, y) THEN
    '--off-right
    yesdraw = 1
    framex = (x + mapwide) - mapx
    framey = y - mapy
    EXIT DO
   END IF
   IF isonscreen(x, y - maphigh) THEN
    '--off-top
    yesdraw = 1
    framex = x - mapx
    framey = (y - maphigh) - mapy
    EXIT DO
   END IF
   IF isonscreen(x, y + maphigh) THEN
    '--off-bottom
    yesdraw = 1
    framex = x - mapx
    framey = (y + maphigh) - mapy
    EXIT DO
   END IF
   IF isonscreen(x - mapwide, y - maphigh) THEN
    '--off-top-left
    yesdraw = 1
    framex = (x - mapwide) - mapx
    framey = (y - maphigh) - mapy
    EXIT DO
   END IF
   IF isonscreen(x + mapwide, y - maphigh) THEN
    '--off-top-right
    yesdraw = 1
    framex = (x + mapwide) - mapx
    framey = (y - maphigh) - mapy
    EXIT DO
   END IF
   IF isonscreen(x - mapwide, y + maphigh) THEN
    '--off-bottom-left
    yesdraw = 1
    framex = (x - mapwide) - mapx
    framey = (y + maphigh) - mapy
    EXIT DO
   END IF
   IF isonscreen(x + mapwide, y + maphigh) THEN
    '--off-bottom-right
    yesdraw = 1
    framex = (x + mapwide) - mapx
    framey = (y + maphigh) - mapy
    EXIT DO
   END IF
   EXIT DO
  LOOP
 END IF
END IF

framewalkabout = yesdraw
END FUNCTION

SUB initgamedefaults

lastsaveslot = 0

'--items
item$(-3) = readglobalstring$(35, "DONE", 10)
item$(-2) = readglobalstring$(36, "AUTOSORT", 10)
item$(-1) = readglobalstring$(37, "TRASH", 10)
FOR i = -3 TO -1
 item(i) = 1
 item$(i) = rpad$(item$(i), " ", 11)
NEXT i
FOR i = 0 TO 199: item$(i) = "           ": NEXT i

'--money
gold& = gen(96)

'--hero's speed
FOR i = 0 TO 3
 herospeed(i) = 4
NEXT i

'--hero's position
FOR i = 0 TO 15
 catx(i) = gen(102) * 20
 caty(i) = gen(103) * 20
 catd(i) = 2
NEXT i

END SUB

SUB innRestore (stat())

FOR i = 0 TO 3
 IF hero(i) > 0 THEN '--hero exists
  IF stat(i, 0, 0) <= 0 AND readbit(gen(), 101, 4) THEN
   '--hero is dead and inn-revive is disabled
  ELSE
   '--normal revive
   stat(i, 0, 0) = stat(i, 1, 0)
   stat(i, 0, 1) = stat(i, 1, 1)
   resetlmp i, stat(i, 0, 12)
  END IF
 END IF
NEXT i

END SUB

FUNCTION isonscreen (x, y)
IF x >= mapx - 20 AND x <= mapx + 340 AND y >= mapy - 20 AND y <= mapy + 200 THEN
 isonscreen = -1
ELSE
 isonscreen = 0
END IF

'17*20=340
'10*20=200

END FUNCTION

SUB safekill (f$)
IF isfile(f$ + CHR$(0)) THEN KILL f$
END SUB

SUB setScriptArg (arg, value)
 IF scrat(nowscript, scrargs) > arg THEN
  heap(scrat(nowscript, scrheap) + arg) = value
 END IF
END SUB

SUB showplotstrings

FOR i = 0 TO 31
 '-- for each string
 IF readbit(plotstrBits(), i, 0) THEN
  '-- only display visible strings
  IF readbit(plotstrBits(), i, 1) THEN
    '-- flat text
    textcolor plotstrCol(i), plotstrBGCol(i)
    printstr plotstring$(i), plotstrX(i), plotstrY(i), dpage
  ELSE
    '-- with outline
    edgeprint plotstring$(i), plotstrX(i), plotstrY(i), plotstrCol(i), dpage
  END IF
 END IF
NEXT i

END SUB

SUB strgrabber (s$, maxl)
STATIC clip$

'--BACKSPACE support
IF keyval(14) > 1 AND LEN(s$) > 0 THEN s$ = LEFT$(s$, LEN(s$) - 1)

'--SHIFT support
shift = 0
IF keyval(54) > 0 OR keyval(42) > 0 THEN shift = 1

'--adding chars
IF LEN(s$) < maxl THEN

 '--SPACE support
 IF keyval(57) > 1 THEN
   s$ = s$ + " "
 ELSE
  '--all other keys
  FOR i = 2 TO 53
   IF keyval(i) > 1 AND keyv(i, shift) > 0 THEN
    s$ = s$ + CHR$(keyv(i, shift))
    EXIT FOR
   END IF
  NEXT i
 END IF

END IF

END SUB

SUB touchfile (f$)

fh = FREEFILE
OPEN f$ FOR BINARY AS #fh
CLOSE #fh

END SUB

SUB defaultc
 RESTORE ctrldata
 FOR i = 0 TO 12
  READ csetup(i)
 NEXT i
 FOR i = 9 TO 12
  READ joy(i)
 NEXT i
 EXIT SUB

ctrldata:
 DATA 72,80,75,77,57,28,29,1,56,1,15,36,51
 DATA 150,650,150,650
END SUB
