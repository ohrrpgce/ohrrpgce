'OHRRPGCE GAME - Even more various unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z
DECLARE SUB verquit ()
DECLARE SUB playtimer ()
DECLARE SUB quitcleanup ()
DECLARE FUNCTION str2int% (stri$)
DECLARE FUNCTION str2lng& (stri$)
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
DECLARE SUB correctbackdrop ()
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
DECLARE FUNCTION readfoemap% (x%, y%, wide%, high%, fh%)
DECLARE FUNCTION playtime$ (d%, h%, m%)
DECLARE FUNCTION functiondone% ()
DECLARE FUNCTION functionread% ()
DECLARE FUNCTION averagelev% (stat%())
DECLARE FUNCTION countitem% (it%)
DECLARE SUB fatalerror (e$)
DECLARE FUNCTION movdivis% (xygo%)
DECLARE FUNCTION onwho% (w$, alone)
DECLARE SUB minimap (mx%, my%, x%, y%, tastuf%())
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
DECLARE FUNCTION usemenu% (pt%, top%, first%, last%, size%)
DECLARE SUB debug (s$)
DECLARE FUNCTION browse$ (fmask$, needf%)
DECLARE SUB doswap (s%, d%, stat%())
DECLARE SUB control ()
DECLARE SUB equip (pt%, stat%())
DECLARE FUNCTION items% (stat%())
DECLARE SUB getitem (getit%)
DECLARE SUB oobcure (w%, t%, atk%, spred%, stat%())
DECLARE SUB spells (pt%, stat%())
DECLARE SUB status (pt%, stat%())
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
DECLARE FUNCTION readatkname$ (id%)
DECLARE SUB getmapname (mapname$, m%)
DECLARE SUB defaultc ()
DECLARE SUB loadsay (choosep%, say%, sayer%, showsay%, say$(), saytag%(), choose$(), chtag%(), saybit%(), sayenh%())
DECLARE FUNCTION maplumpname$ (map, oldext$)
DECLARE SUB cathero ()

'$INCLUDE: 'compat.bi'
'$INCLUDE: 'allmodex.bi'
'$INCLUDE: 'gglobals.bi'
'$INCLUDE: 'const.bi'
'$INCLUDE: 'scrconst.bi'
101
DATA 72,80,75,77,57,28,29,1,56,1,15,36,51
DATA 150,650,150,650

REM $STATIC
SUB cathero

DIM zsort(3)

'--if riding a vehicle and not mounting and not hiding leader and not hiding party then exit
IF veh(0) AND readbit(veh(), 6, 0) = 0 AND readbit(veh(), 6, 4) = 0 AND readbit(veh(), 6, 5) = 0 AND readbit(veh(), 9, 4) = 0 AND readbit(veh(), 9, 5) = 0 THEN EXIT SUB

IF readbit(gen(), 101, 1) = 1 AND (veh(0) = 0 OR readbit(veh(), 9, 4) = 0) THEN
 '--caterpillar party (normal)
 '--this should Y-sort
 catermask(0) = 0
 FOR i = 0 TO 3
  FOR o = 0 TO 3
   IF readbit(catermask(), 0, o) = 0 THEN j = o
  NEXT o
  FOR o = 0 TO 3
   IF caty(o * 5) - mapy < caty(j * 5) - mapy AND readbit(catermask(), 0, o) = 0 THEN
    j = o
   END IF
  NEXT o
  zsort(i) = j
  setbit catermask(), 0, j, 1
 NEXT i
 FOR i = 0 TO 3
  IF framewalkabout(catx(zsort(i) * 5), caty(zsort(i) * 5) + gmap(11), framex, framey, scroll(0) * 20, scroll(1) * 20, gmap(5)) THEN
   loadsprite buffer(), 0, 200 * ((catd(zsort(i) * 5) * 2) + INT(wtog(zsort(i)) / 2)), zsort(i) * 5, 20, 20, 2
   drawsprite buffer(), 0, pal16(), zsort(i) * 16, framex, framey - catz(zsort(i) * 5), dpage
  END IF
 NEXT i
ELSE
 '--non-caterpillar party, vehicle no-hide-leader (or backcompat pref)
 loadsprite buffer(), 0, 200 * ((catd(0) * 2) + INT(wtog(0) / 2)), 0, 20, 20, 2
 drawsprite buffer(), 0, pal16(), 0, catx(0) - mapx, (caty(0) - mapy) - catz(0) + gmap(11), dpage
END IF

END SUB

SUB defaultc
 RESTORE 101
 FOR i = 0 TO 12
  READ csetup(i)
 NEXT i
 FOR i = 9 TO 12
  READ joy(i)
 NEXT i
 EXIT SUB

ctrldata:


END SUB

SUB forcedismount (choosep, say, sayer, showsay, say$(), saytag(), choose$(), chtag(), saybit(), sayenh(), catd(), foep)
IF veh(0) THEN
 '--clear vehicle on loading new map--
 IF readbit(veh(), 9, 6) AND readbit(veh(), 9, 7) = 0 THEN
  '--dismount-ahead is true, dismount-passwalls is false
  SELECT CASE catd(0)
   CASE 0
    ygo(0) = 20
   CASE 1
    xgo(0) = -20
   CASE 2
    ygo(0) = -20
   CASE 3
    xgo(0) = 20
  END SELECT
 END IF
 IF veh(16) > 0 THEN
  say = veh(16)
  loadsay choosep, say, sayer, showsay, say$(), saytag(), choose$(), chtag(), saybit(), sayenh()
 END IF
 IF veh(16) < 0 THEN
  rsr = runscript(ABS(veh(16)), nowscript + 1, -1, "dismount")
 END IF
 IF veh(14) > 1 THEN setbit tag(), 0, veh(14), 0
 herospeed(0) = veh(7)
 IF herospeed(0) = 3 THEN herospeed(0) = 10
 FOR i = 0 TO 21
  veh(i) = 0
 NEXT i
 FOR i = 1 TO 15
  catx(i) = catx(0)
  caty(i) = caty(0)
 NEXT i
 foep = range(100, 60)
END IF
END SUB

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

FUNCTION maplumpname$ (map, oldext$)
 IF map < 100 THEN
  maplumpname$ = game$ + "." + oldext$ + RIGHT$("0" + LTRIM$(STR$(map)), 2)
 ELSE
  maplumpname$ = workingdir$ + SLASH + LTRIM$(STR$(map)) + "." + oldext$
 END IF
END FUNCTION

SUB safekill (f$)
IF isfile(f$ + CHR$(0)) THEN KILL f$
END SUB

SUB setScriptArg (arg, value)
 IF scrat(nowscript, scrargs) > arg THEN
  heap(scrat(nowscript, scrheap) + arg) = value
 END IF
END SUB

SUB setusermenu (menu$(), mt, mi())

menu$(0) = readglobalstring$(67, "Quit", 10)
menu$(1) = readglobalstring$(62, "Status", 10)
menu$(2) = readglobalstring$(68, "Map", 10)
menu$(3) = readglobalstring$(61, "Spells", 10)
menu$(4) = readglobalstring$(60, "Items", 10)
menu$(5) = readglobalstring$(63, "Equip", 10)
menu$(6) = readglobalstring$(66, "Save", 10)
menu$(7) = readglobalstring$(69, "Volume", 10)
menu$(8) = readglobalstring$(64, "Order", 10)
IF readbit(gen(), 101, 5) THEN menu$(8) = readglobalstring$(65, "Team", 10)
'THIS STUFF WILL CHANGE LATER...yes it will!
mt = 8
mi(0) = 4
mi(1) = 3
mi(2) = 1
mi(3) = 5
mi(4) = 8
mi(5) = 2
mi(6) = 6
mi(7) = 0
mi(8) = 7

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

SUB checklumpmod
 IF readbit(lumpmod(),0,0) THEN
  unlumpfile sourcerpg$ + CHR$(0), "*.dt1", workingdir$ + SLASH, buffer()
  setbit lumpmod(),0,0,0
 END IF

END SUB

SUB correctbackdrop

IF gen(58) THEN
 '--restore text box backdrop
 loadpage game$ + ".mxs" + CHR$(0), gen(58) - 1, 3
 EXIT SUB
END IF

IF gen(50) THEN
 '--restore script backdrop
 loadpage game$ + ".mxs" + CHR$(0), gen(50) - 1, 3
 EXIT SUB
END IF

loadpage game$ + ".til" + CHR$(0), gmap(0), 3

END SUB

SUB cleanuptemp
KILL workingdir$ + SLASH + "lockfile.tmp"
findfiles workingdir$ + SLASH + "*.*" + CHR$(0), 0, tmpdir$ + "filelist.tmp" + CHR$(0), buffer()
fh = FREEFILE
OPEN tmpdir$ + "filelist.tmp" FOR INPUT AS #fh
DO UNTIL EOF(fh)
 INPUT #fh, filename$
 filename$ = LCASE$(filename$)
 IF LEFT$(filename$, 3) = "vdm" AND RIGHT$(filename$, 4) = ".tmp" THEN
  '-- mysterious locked file, leave it alone
 ELSE
  '-- delte file
 END IF
 KILL workingdir$ + SLASH + filename$
LOOP
CLOSE #fh
KILL tmpdir$ + "filelist.tmp"

END SUB

FUNCTION checkfordeath (stat())
checkfordeath = 0' --default alive

o = 0
FOR i = 0 TO 3 '--for each slot
 IF hero(i) > 0 THEN '--if hero exists
  o = o + 1
  IF stat(i, 0, 0) <= 0 AND stat(i, 1, 0) > 0 THEN o = o - 1
 END IF
NEXT i
IF o = 0 THEN checkfordeath = 1

END FUNCTION

SUB aheadxy (x, y, direction, distance)
'--alters the input X and Y, moving them "ahead" by distance in direction

IF direction = 0 THEN y = y - distance
IF direction = 1 THEN x = x + distance
IF direction = 2 THEN y = y + distance
IF direction = 3 THEN x = x - distance

END SUB

SUB exitprogram (needfade)

'DEBUG debug "Exiting Program"
'DEBUG debug "fade music"
fademusic 0
'DEBUG debug "fade screen"
IF needfade THEN fadeout 0, 0, 0, -1
quitcleanup
'DEBUG debug "Restore Old Graphics Mode"
restoremode
'DEBUG debug "Terminate NOW (boom!)"
SYSTEM

END SUB

SUB keyboardsetup
RESTORE keyconst
keyconst:
FOR o = 0 TO 1
 FOR i = 2 TO 53
  READ temp$
  IF temp$ <> "" THEN keyv(i, o) = ASC(temp$) ELSE keyv(i, o) = 0
 NEXT i
NEXT o
keyv(40, 1) = 34
DATA "1","2","3","4","5","6","7","8","9","0","-","=","","","q","w","e","r","t","y","u","i","o","p","[","]","","","a","s","d","f","g","h","j","k","l",";","'","`","","\","z","x","c","v","b","n","m",",",".","/"
DATA "!","@","#","$","%","^","&","*","(",")","_","+","","","Q","W","E","R","T","Y","U","I","O","P","{","}","","","A","S","D","F","G","H","J","K","L",":"," ","~","","|","Z","X","C","V","B","N","M","<",">","?"
END SUB

SUB verquit
 copypage dpage, vpage
 quitprompt$ = readglobalstring$(55, "Quit Playing?", 20)
 quityes$ = readglobalstring$(57, "Yes", 10)
 quitno$ = readglobalstring$(58, "No", 10)
 dd = 2
 ptr2 = 0
 setkeys
 DO
  setwait timing(), speedcontrol
  setkeys
  tog = tog XOR 1
  playtimer
  control
  wtog(0) = loopvar(wtog(0), 0, 3, 1)
  IF carray(5) > 1 THEN abortg = 0: setkeys: flusharray carray(),7,0: EXIT DO
  IF (carray(4) > 1 AND ABS(ptr2) > 20) OR ABS(ptr2) > 50 THEN
   IF ptr2 < 0 THEN abortg = 1: fadeout 0, 0, 0, 0
   setkeys
   flusharray carray(), 7, 0
   EXIT SUB
  END IF
  IF carray(2) > 0 THEN ptr2 = ptr2 - 5: dd = 3
  IF carray(3) > 0 THEN ptr2 = ptr2 + 5: dd = 1
  centerbox 160, 95, 200, 42, 15, dpage
  loadsprite buffer(), 0, 200 * ((dd * 2) + INT(wtog(0) / 2)), 0 * 5, 20, 20, 2
  drawsprite buffer(), 0, pal16(), 0, 150 + (ptr2), 90, dpage
  edgeprint quitprompt$, xstring(quitprompt$, 160), 80, 15, dpage
  col = 7: IF ptr2 < -20 THEN col = 10 + tog * 5
  edgeprint quityes$, 70, 96, col, dpage
  col = 7: IF ptr2 > 20 THEN col = 10 + tog * 5
  edgeprint quitno$, 256 - LEN(quitno$) * 8, 96, col, dpage
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB 