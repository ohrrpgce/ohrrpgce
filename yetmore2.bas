'OHRRPGCE GAME - Even more various unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z

#include "compat.bi"
#include "allmodex.bi"
#include "common.bi"
#include "gglobals.bi"
#include "const.bi"
#include "scrconst.bi"
#include "uiconst.bi"
#include "loading.bi"

'basic subs and functions
DECLARE SUB verquit ()
DECLARE SUB playtimer ()
DECLARE FUNCTION str2int% (stri$)
DECLARE FUNCTION str2lng& (stri$)
DECLARE FUNCTION rpad$ (s$, pad$, size%)
DECLARE FUNCTION isonscreen% (x%, y%)
DECLARE FUNCTION framewalkabout% (x%, y%, framex%, framey%, mapwide%, maphigh%, wrapmode%)
DECLARE SUB setScriptArg (arg%, value%)
DECLARE SUB wrapaheadxy (x%, y%, direction%, distance%, mapwide%, maphigh%, wrapmode%)
DECLARE SUB aheadxy (x%, y%, direction%, distance%)
DECLARE SUB wrapxy (x%, y%, wide%, high%)
DECLARE SUB keyhandleroff ()
DECLARE FUNCTION partybyrank% (slot%)
DECLARE FUNCTION herobyrank% (slot%)
DECLARE FUNCTION rankincaterpillar% (heroid%)
DECLARE SUB renamehero (who%)
DECLARE FUNCTION trylearn% (who%, atk%, learntype%)
DECLARE SUB correctbackdrop ()
DECLARE FUNCTION gethighbyte% (n%)
DECLARE SUB wrappedsong (songnumber%)
DECLARE SUB delitem (it%, num%)
DECLARE FUNCTION getnpcref% (seekid%, offset%)
DECLARE SUB suspendresume (id%)
DECLARE SUB scriptwatcher (page%)
DECLARE SUB onkeyscript (scriptnum%)
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
DECLARE FUNCTION movdivis% (xygo%)
DECLARE FUNCTION onwho% (w$, alone)
DECLARE SUB heroswap (iAll%, stat%())
DECLARE SUB savegame (slot%, foep%, stat%(), stock%())
DECLARE FUNCTION runscript% (n%, index%, newcall%, er$, trigger%)
DECLARE SUB scripterr (e$)
DECLARE SUB itstr (i%)
DECLARE FUNCTION findhero% (who%, f%, l%, d%)
DECLARE FUNCTION howmanyh% (f%, l%)
DECLARE FUNCTION consumeitem% (index%)
DECLARE FUNCTION istag% (num%, zero%)
DECLARE SUB doswap (s%, d%, stat%())
DECLARE SUB control ()
DECLARE SUB equip (pt%, stat%())
DECLARE FUNCTION items% (stat%())
DECLARE SUB spells (pt%, stat%())
DECLARE SUB status (pt%, stat%())
DECLARE SUB getnames (stat$())
DECLARE SUB resetlmp (slot%, lev%)
DECLARE FUNCTION battle (form%, fatal%, exstat%())
DECLARE FUNCTION atlevel% (now%, a0%, a99%)
DECLARE FUNCTION range% (n%, r%)
DECLARE SUB snapshot ()
DECLARE FUNCTION checksaveslot (slot%)
DECLARE SUB defaultc ()
DECLARE SUB loadsay (BYREF txt AS TextBoxState, box_id AS INTEGER)
DECLARE SUB cathero ()
DECLARE SUB readjoysettings ()
DECLARE SUB loadmap_gmap(mapnum%)
DECLARE SUB loadmap_npcl(mapnum%)
DECLARE SUB loadmap_npcd(mapnum%)
DECLARE SUB loadmap_tilemap(mapnum%)
DECLARE SUB loadmap_passmap(mapnum%)
DECLARE SUB loadmaplumps (mapnum%, loadmask%)
DECLARE FUNCTION mapstatetemp$(mapnum, prefix$)
DECLARE SUB savemapstate_gmap(mapnum%, prefix$)
DECLARE SUB savemapstate_npcl(mapnum%, prefix$)
DECLARE SUB savemapstate_npcd(mapnum%, prefix$)
DECLARE SUB savemapstate_tilemap(mapnum%, prefix$)
DECLARE SUB savemapstate_passmap(mapnum%, prefix$)
DECLARE SUB freescripts (mem%)
DECLARE FUNCTION loadscript% (n%)
DECLARE SUB limitcamera ()
DECLARE SUB cropposition (BYREF x, BYREF y, unitsize)
DECLARE SUB npcplot ()

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
 IF framewalkabout(catx(0), caty(0) + gmap(11), framex, framey, scroll(0) * 20, scroll(1) * 20, gmap(5)) THEN
  loadsprite buffer(), 0, 200 * ((catd(0) * 2) + INT(wtog(0) / 2)), 0, 20, 20, 2
  drawsprite buffer(), 0, pal16(), 0, framex, framey - catz(0), dpage
 END IF
END IF

END SUB

FUNCTION cropmovement (x, y, xgo, ygo)
 'crops movement at edge of map, or wraps
 'returns true if ran into wall at edge
 cropmovement = 0
 IF gmap(5) = 1 THEN
  '--wrap walking
  IF x < 0 THEN x = x + scroll(0) * 20
  IF x >= scroll(0) * 20 THEN x = x - scroll(0) * 20
  IF y < 0 THEN y = y + scroll(1) * 20
  IF y >= scroll(1) * 20 THEN y = y - scroll(1) * 20
 ELSE
  '--crop walking
  IF x < 0 THEN x = 0: xgo = 0: cropmovement = 1
  IF x > (scroll(0) - 1) * 20 THEN x = (scroll(0) - 1) * 20: xgo = 0: cropmovement = 1
  IF y < 0 THEN y = 0: ygo = 0: cropmovement = 1
  IF y > (scroll(1) - 1) * 20 THEN y = (scroll(1) - 1) * 20: ygo = 0: cropmovement = 1
 END IF
END FUNCTION

SUB defaultc
 DIM cconst(12) = {72,80,75,77,57,28,29,1,56,1,15,36,51}
 DIM joyconst(3) = {150,650,150,650}

 FOR i = 0 TO 12
  csetup(i) = cconst(i)
 NEXT i
 FOR i = 9 TO 12
  joy(i) = joyconst(i - 9)
 NEXT i
 EXIT SUB
END SUB

SUB drawnpcs
 FOR i = 0 TO 299 '-- for each NPC instance
  IF npc(i).id > 0 THEN '-- if visible
   o = npc(i).id - 1
   z = 0
   drawnpcX = 0
   drawnpcY = 0
   IF framewalkabout(npc(i).x, npc(i).y + gmap(11), drawnpcX, drawnpcY, scroll(0) * 20, scroll(1) * 20, gmap(5)) THEN
    IF veh(0) AND veh(5) = i THEN z = catz(0) '--special vehicle magic
    IF z AND readbit(veh(), 9, 8) = 0 THEN '--shadow
     rectangle npc(i).x - mapx + 6, npc(i).y - mapy + gmap(11) + 13, 8, 5, uilook(uiShadow), dpage
     rectangle npc(i).x - mapx + 5, npc(i).y - mapy + gmap(11) + 14, 10, 3, uilook(uiShadow), dpage
    END IF
    sprite_draw npcs(o).sprite + (2 * npc(i).dir) + npc(i).frame \ 2, npcs(o).pal, drawnpcX, drawnpcY - z, 1, -1, dpage
    'edgeprint STR$(i), drawnpcX, drawnpcY + gmap(11) - z, uilook(uiText), dpage
   END IF
  END IF
 NEXT i
END SUB

SUB forcedismount (BYREF txt AS TextBoxState, catd(), foep)
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
  loadsay txt, veh(16)
 END IF
 IF veh(16) < 0 THEN
  rsr = runscript(ABS(veh(16)), nowscript + 1, -1, "dismount", plottrigger)
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
CleanInventory inventory()

'--money
gold = gen(genStartMoney)

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

SUB setmapxy
SELECT CASE gen(cameramode)
 CASE herocam
  mapx = catx(gen(cameraArg)) - 150
  mapy = caty(gen(cameraArg)) - 90
 CASE npccam
  mapx = npc(gen(cameraArg)).x - 150
  mapy = npc(gen(cameraArg)).y - 90
 CASE pancam ' 1=dir, 2=ticks, 3=step
  IF gen(cameraArg2) > 0 THEN
   aheadxy mapx, mapy, gen(cameraArg), gen(cameraArg3)
   gen(cameraArg2) -= 1
  END IF
  IF gen(cameraArg2) <= 0 THEN gen(cameramode) = stopcam
 CASE focuscam ' 1=x, 2=y, 3=x step, 4=y step
  temp = gen(cameraArg) - mapx
  IF ABS(temp) <= gen(cameraArg3) THEN
   gen(cameraArg3) = 0
   mapx = gen(cameraArg)
  ELSE
   mapx += SGN(temp) * gen(cameraArg3)
  END IF
  temp = gen(cameraArg2) - mapy
  IF ABS(temp) <= gen(cameraArg4) THEN
   gen(cameraArg4) = 0
   mapy = gen(cameraArg2)
  ELSE
   mapy += SGN(temp) * gen(cameraArg4)
  END IF
  limitcamera
  IF gen(cameraArg3) = 0 AND gen(cameraArg4) = 0 THEN gen(cameramode) = stopcam
END SELECT
limitcamera
END SUB

SUB limitcamera 
IF gmap(5) = 0 THEN
 'when cropping the camera to the map, stop camera movements that attempt to go over the edge
 oldmapx = mapx
 oldmapy = mapy
 mapx = bound(mapx, 0, scroll(0) * 20 - 320)
 mapy = bound(mapy, 0, scroll(1) * 20 - 200)
 IF oldmapx <> mapx THEN
  IF gen(cameramode) = pancam THEN gen(cameramode) = stopcam
  IF gen(cameramode) = focuscam THEN gen(cameraArg3) = 0
 END IF
 IF oldmapy <> mapy THEN
  IF gen(cameramode) = pancam THEN gen(cameramode) = stopcam
  IF gen(cameramode) = focuscam THEN gen(cameraArg4) = 0
 END IF
END IF
IF gmap(5) = 1 THEN
 'Wrap the camera according to the center, not the top-left
 mapx += 160
 mapy += 160
 wrapxy mapx, mapy, scroll(0) * 20, scroll(1) * 20
 mapx -= 160
 mapy -= 160
END IF
END SUB

SUB setScriptArg (arg, value)
 IF script(scrat(nowscript).scrnum).args > arg THEN
  heap(scrat(nowscript).heap + arg) = value
 END IF
END SUB

SUB showplotstrings

FOR i = 0 TO 31
 '-- for each string
 IF plotstr(i).bits AND 1 THEN
  '-- only display visible strings
  IF plotstr(i).bits AND 2 THEN
    '-- flat text
    textcolor plotstr(i).Col, plotstr(i).BGCol
    printstr plotstr(i).s, plotstr(i).X, plotstr(i).Y, dpage
  ELSE
    '-- with outline
    edgeprint plotstr(i).s, plotstr(i).X, plotstr(i).Y, plotstr(i).Col, dpage
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

FUNCTION strgrabber (s$, maxl) AS INTEGER
STATIC clip$

DIM old$
old$ = s$

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

'Return true of the string has changed
RETURN (s$ <> old$)

END FUNCTION

SUB makebackups
 'what is this for? Since some lumps can be modified at run time, we need to keep a
 'backup copy, and then only edit the copy. The original is never used directly.
 'enemy data
 copyfile game + ".dt1", tmpdir & "dt1.tmp"
 'formation data
 copyfile game + ".for", tmpdir & "for.tmp"
 'if you add lump-modding commands, you better well add them here >:(
END SUB

SUB correctbackdrop

IF gen(58) THEN
 '--restore text box backdrop
 loadpage game + ".mxs", gen(58) - 1, 3
 EXIT SUB
END IF

IF gen(50) THEN
 '--restore script backdrop
 loadpage game + ".mxs", gen(50) - 1, 3
 EXIT SUB
END IF

'loadpage game + ".til", gmap(0), 3

END SUB

SUB cleanuptemp
 findfiles workingdir + SLASH + ALLFILES, 0, tmpdir + "filelist.tmp", buffer()
 fh = FREEFILE
  OPEN tmpdir + "filelist.tmp" FOR INPUT AS #fh
  DO UNTIL EOF(fh)
   LINE INPUT #fh, filename$
   IF usepreunlump = 0 THEN
    'normally delete everything
    KILL workingdir + SLASH + filename$
   ELSE
    'but for preunlumped games only delete specific files
    ext$ = justextension$(filename$)
    IF ext$ = "tmp" OR ext$ = "bmd" THEN
     KILL workingdir + SLASH + filename$
    END IF
   END IF
  LOOP
  CLOSE #fh

  KILL tmpdir + "filelist.tmp"

  findfiles tmpdir + ALLFILES, 0, tmpdir + "filelist.tmp", buffer()
  fh = FREEFILE
  OPEN tmpdir + "filelist.tmp" FOR INPUT AS #fh
  DO UNTIL EOF(fh)
   LINE INPUT #fh, filename$
   IF filename$ = "filelist.tmp" THEN CONTINUE DO ' skip this, deal with it later
   IF NOT isdir(tmpdir & filename$) THEN
    KILL tmpdir & filename$
   END IF
  LOOP
  CLOSE #fh

  KILL tmpdir + "filelist.tmp"
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
IF needfade THEN fadeout 0, 0, 0

'DEBUG debug "Cleanup Routine"
'--open files
'DEBUG debug "Close foemap handle"
CLOSE #foemaph

'--script stack
'DEBUG debug "Release script stack"
releasestack
destroystack(scrst)

'--reset audio
closemusic
'DEBUG debug "Restore original FM volume"
setfmvol fmvol

'--working files
'DEBUG debug "Kill working files"
cleanuptemp
RMDIR tmpdir + "playing.tmp"
RMDIR tmpdir
'DEBUG debug "Remove working directory"
IF usepreunlump = 0 THEN RMDIR workingdir

'DEBUG debug "Restore Old Graphics Mode"
restoremode
'DEBUG debug "Terminate NOW (boom!)"
SYSTEM

END SUB

SUB keyboardsetup
'There is a different implementation of this in customsubs for CUSTOM
DIM keyconst(103) as string = {"1","2","3","4","5","6","7","8","9","0","-","=","","","q","w","e","r","t","y","u","i","o","p","[","]","","","a","s","d","f","g","h","j","k","l",";","'","`","","\","z","x","c","v","b","n","m",",",".","/", _
"!","@","#","$","%","^","&","*","(",")","_","+","","","Q","W","E","R","T","Y","U","I","O","P","{","}","","","A","S","D","F","G","H","J","K","L",":"," ","~","","|","Z","X","C","V","B","N","M","<",">","?"}

FOR o = 0 TO 1
 FOR i = 2 TO 53
  temp$ = keyconst$((i - 2) + o * 52)
  IF temp$ <> "" THEN keyv(i, o) = ASC(temp$) ELSE keyv(i, o) = 0
 NEXT i
NEXT o
keyv(40, 1) = 34

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
  setwait speedcontrol
  setkeys
  tog = tog XOR 1
  playtimer
  control
  wtog(0) = loopvar(wtog(0), 0, 3, 1)
  IF carray(5) > 1 THEN abortg = 0: setkeys: flusharray carray(),7,0: EXIT DO
  IF (carray(4) > 1 AND ABS(ptr2) > 20) OR ABS(ptr2) > 50 THEN
   IF ptr2 < 0 THEN abortg = 1: fadeout 0, 0, 0
   setkeys
   flusharray carray(), 7, 0
   EXIT SUB
  END IF
  IF carray(2) > 0 THEN ptr2 = ptr2 - 5: dd = 3
  IF carray(3) > 0 THEN ptr2 = ptr2 + 5: dd = 1
  centerbox 160, 95, 200, 42, 15, dpage
  loadsprite buffer(), 0, 200 * ((dd * 2) + INT(wtog(0) / 2)), 0 * 5, 20, 20, 2
  drawsprite buffer(), 0, pal16(), 0, 150 + (ptr2), 90, dpage
  edgeprint quitprompt$, xstring(quitprompt$, 160), 80, uilook(uiText), dpage
  col = uilook(uiMenuItem): IF ptr2 < -20 THEN col = uilook(uiSelectedItem + tog) '10 + tog * 5
  edgeprint quityes$, 70, 96, col, dpage
  col = uilook(uiMenuItem): IF ptr2 > 20 THEN col = uilook(uiSelectedItem + tog) '10 + tog * 5
  edgeprint quitno$, 256 - LEN(quitno$) * 8, 96, col, dpage
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB

FUNCTION titlescr
titlescr = -1 ' default return true for success
loadpage game + ".mxs", gen(genTitle), 3
needf = 2
IF gen(genTitleMus) > 0 THEN wrappedsong gen(genTitleMus) - 1
fademusic fmvol
setkeys
DO
 setwait speedcontrol
 setkeys
 control
 IF carray(5) > 1 THEN
  titlescr = 0 ' return false for cancel
  EXIT DO
 END IF
 IF carray(4) > 1 OR carray(5) > 1 THEN EXIT DO
 FOR i = 2 TO 88
  IF keyval(i) > 1 THEN
   EXIT DO
  END IF
 NEXT i
 FOR i = 0 TO 1
  gotj(i) = readjoy(joy(), i)
  IF gotj(i) THEN
   IF joy(2) = 0 OR joy(3) = 0 THEN
    joy(2) = -1: joy(3) = -1
    readjoysettings
    joy(2) = -1: joy(3) = -1
    EXIT DO
   ELSE
    gotj(i) = 0
   END IF
  END IF
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 IF needf = 1 THEN
  needf = 0
  fademusic fmvol
  fadein
 END IF
 IF needf > 1 THEN needf = needf - 1
 dowait
LOOP
END FUNCTION

SUB reloadnpc (stat())
vishero stat()
FOR i = 0 TO npcdMax
 setpicstuf buffer(), 1600, 2
 with npcs(i)
  if .sprite then sprite_unload(@.sprite)
  if .pal then palette16_unload(@.pal)
  .sprite = sprite_load(game + ".pt4", .picture, 8, 20, 20)
  .pal = palette16_load(game + ".pal", .palette, 4, .picture)
 end with
NEXT i
END SUB

FUNCTION mapstatetemp$(mapnum, prefix$)
 RETURN tmpdir & prefix$ & mapnum
END FUNCTION

SUB savemapstate_gmap(mapnum, prefix$)
 fh = FREEFILE
 OPEN mapstatetemp$(mapnum, prefix$) + "_map.tmp" FOR BINARY AS #fh
 PUT #fh, , gmap()
 CLOSE #fh
END SUB

SUB savemapstate_npcl(mapnum, prefix$)
 fh = FREEFILE
 OPEN mapstatetemp$(mapnum, prefix$) + "_l.tmp" FOR BINARY AS #fh
 PUT #fh, , npc()
 CLOSE #fh
END SUB

SUB savemapstate_npcd(mapnum, prefix$)
 fh = FREEFILE
 OPEN mapstatetemp$(mapnum, prefix$) + "_n.tmp" FOR BINARY AS #fh
 'PUT #fh, , npcs()
 dim i as integer
 for i = lbound(npcs) to ubound(npcs)
  with npcs(i)
   put #fh, ,.picture
   put #fh, ,.palette
   put #fh, ,.movetype
   put #fh, ,.speed
   put #fh, ,.textbox
   put #fh, ,.facetype
   put #fh, ,.item
   put #fh, ,.pushtype
   put #fh, ,.activation
   put #fh, ,.tag1
   put #fh, ,.tag2
   put #fh, ,.usetag
   put #fh, ,.script
   put #fh, ,.scriptarg
   put #fh, ,.vehicle
  end with
 next
 CLOSE #fh
END SUB

SUB savemapstate_tilemap(mapnum, prefix$)
 savetiledata mapstatetemp$(mapnum, prefix$) + "_t.tmp", scroll(), 3
END SUB

SUB savemapstate_passmap(mapnum, prefix$)
 savetiledata mapstatetemp$(mapnum, prefix$) + "_p.tmp", pass()
END SUB

SUB savemapstate (mapnum, savemask = 255, prefix$)
fh = FREEFILE
IF savemask AND 1 THEN
 savemapstate_gmap mapnum, prefix$
END IF
IF savemask AND 2 THEN
 savemapstate_npcl mapnum, prefix$
END IF
IF savemask AND 4 THEN
 savemapstate_npcd mapnum, prefix$
END IF
IF savemask AND 8 THEN
 savemapstate_tilemap mapnum, prefix$
END IF
IF savemask AND 16 THEN
 savemapstate_passmap mapnum, prefix$
END IF
END SUB

SUB loadmapstate_gmap (mapnum, prefix$, dontfallback = 0)
 fh = FREEFILE
 filebase$ = mapstatetemp$(mapnum, prefix$)
 IF NOT isfile(filebase$ + "_map.tmp") THEN
  IF dontfallback = 0 THEN loadmap_gmap mapnum
  EXIT SUB
 END IF
 OPEN filebase$ + "_map.tmp" FOR BINARY AS #fh
 GET #fh, , gmap()
 CLOSE #fh

 loadmaptilesets tilesets(), gmap()
 correctbackdrop
 SELECT CASE gmap(5) '--outer edge wrapping
  CASE 0, 1'--crop edges or wrap
   setoutside -1
  CASE 2
   setoutside gmap(6)
 END SELECT
END SUB

SUB loadmapstate_npcl (mapnum, prefix$, dontfallback = 0)
 fh = FREEFILE
 filebase$ = mapstatetemp$(mapnum, prefix$)
 IF NOT isfile(filebase$ + "_l.tmp") THEN
  IF dontfallback = 0 THEN loadmap_npcl mapnum
  EXIT SUB
 END IF
 OPEN filebase$ + "_l.tmp" FOR BINARY AS #fh
 GET #fh, , npc()
 CLOSE #fh

 'Evaluate whether NPCs should appear or disappear based on tags
 npcplot
END SUB

SUB loadmapstate_npcd (mapnum, prefix$, dontfallback = 0)
 fh = FREEFILE
 filebase$ = mapstatetemp$(mapnum, prefix$)
 IF NOT isfile(filebase$ + "_n.tmp") THEN
  IF dontfallback = 0 THEN loadmap_npcd mapnum
  EXIT SUB
 END IF
 OPEN filebase$ + "_n.tmp" FOR BINARY AS #fh
 for i = lbound(npcs) to ubound(npcs)
  with npcs(i)
   get #fh, ,.picture
   get #fh, ,.palette
   get #fh, ,.movetype
   get #fh, ,.speed
   get #fh, ,.textbox
   get #fh, ,.facetype
   get #fh, ,.item
   get #fh, ,.pushtype
   get #fh, ,.activation
   get #fh, ,.tag1
   get #fh, ,.tag2
   get #fh, ,.usetag
   get #fh, ,.script
   get #fh, ,.scriptarg
   get #fh, ,.vehicle
  end with
 next
 CLOSE #fh

 'Evaluate whether NPCs should appear or disappear based on tags
 npcplot
 'load NPC graphics
 reloadnpc stat()
END SUB

SUB loadmapstate_tilemap (mapnum, prefix$, dontfallback = 0)
 filebase$ = mapstatetemp$(mapnum, prefix$)
 IF NOT isfile(filebase$ + "_t.tmp") THEN
  IF dontfallback = 0 THEN loadmap_tilemap mapnum
 ELSE
  DIM AS SHORT mapsize(1), propersize(1)
  fh = FREEFILE
  OPEN maplumpname$(mapnum, "t") FOR BINARY AS #fh
  GET #fh, 8, propersize()
  CLOSE #fh
  OPEN filebase$ + "_t.tmp" FOR BINARY AS #fh
  GET #fh, 8, mapsize()
  CLOSE #fh
  IF mapsize(0) = propersize(0) AND mapsize(1) = propersize(1) THEN
   loadtiledata filebase$ + "_t.tmp", scroll(), 3

   '--as soon as we know the dimensions of the map, enforce hero position boundaries
   cropposition catx(0), caty(0), 20

  ELSE
   IF dontfallback = 0 THEN loadmap_tilemap mapnum
  END IF
 END IF
END SUB

SUB loadmapstate_passmap (mapnum, prefix$, dontfallback = 0)
 filebase$ = mapstatetemp$(mapnum, prefix$)
 IF NOT isfile(filebase$ + "_p.tmp") THEN
  IF dontfallback = 0 THEN loadmap_passmap mapnum
 ELSE
  DIM AS SHORT mapsize(1), propersize(1)
  fh = FREEFILE
  OPEN maplumpname$(mapnum, "p") FOR BINARY AS #fh
  GET #fh, 8, propersize()
  CLOSE #fh
  OPEN filebase$ + "_p.tmp" FOR BINARY AS #fh
  GET #fh, 8, mapsize()
  CLOSE #fh
  IF mapsize(0) = propersize(0) AND mapsize(1) = propersize(1) THEN
   loadtiledata filebase$ + "_p.tmp", pass()
  ELSE
   IF dontfallback = 0 THEN loadmap_passmap mapnum
  END IF
 END IF
END SUB

SUB loadmapstate (mapnum, loadmask, prefix$, dontfallback = 0)
IF loadmask AND 1 THEN
 loadmapstate_gmap mapnum, prefix$, dontfallback
END IF
IF loadmask AND 2 THEN
 loadmapstate_npcl mapnum, prefix$, dontfallback
END IF
IF loadmask AND 4 THEN
 loadmapstate_npcd mapnum, prefix$, dontfallback
END IF
IF loadmask AND 8 THEN
 loadmapstate_tilemap mapnum, prefix$, dontfallback
END IF
IF loadmask AND 16 THEN
 loadmapstate_passmap mapnum, prefix$, dontfallback
END IF
END SUB

SUB deletemapstate (mapnum, killmask, prefix$)
filebase$ = mapstatetemp(mapnum, "map")
IF killmask AND 1 THEN safekill filebase$ + "_map.tmp"
IF killmask AND 2 THEN safekill filebase$ + "_l.tmp"
IF killmask AND 4 THEN safekill filebase$ + "_n.tmp"
IF killmask AND 8 THEN safekill filebase$ + "_t.tmp"
IF killmask AND 16 THEN safekill filebase$ + "_p.tmp"
END SUB

SUB deletetemps
'deletes game-state temporary files when exiting back to the titlescreen

 findfiles tmpdir + ALLFILES, 0, tmpdir + "filelist.tmp", buffer()
 fh = FREEFILE
 OPEN tmpdir + "filelist.tmp" FOR INPUT AS #fh
 DO UNTIL EOF(fh)
  LINE INPUT #fh, filename$
  filename$ = LCASE$(filename$)
  IF RIGHT$(filename$,4) = ".tmp" AND (LEFT$(filename$,3) = "map" OR LEFT$(filename$,5) = "state") THEN
   KILL tmpdir + filename$
  END IF
 LOOP
 CLOSE #fh

 KILL tmpdir + "filelist.tmp"
END SUB

FUNCTION decodetrigger (trigger, trigtype)
 DIM buf(19)
 'debug "decoding " + STR$(trigger) + " type " + STR$(trigtype)
 decodetrigger = trigger  'default
 IF trigger >= 16384 THEN
  fname$ = workingdir + SLASH + "lookup" + STR$(trigtype) + ".bin"
  IF isfile(fname$) THEN
   IF loadrecord (buf(), fname$, 20, trigger - 16384) THEN
    decodetrigger = buf(0)
    IF buf(0) = 0 THEN
     scripterr "Script " + readbinstring(buf(), 1, 36) + " is not imported"
    END IF
   END IF
  END IF
 END IF
END FUNCTION

SUB killallscripts
'this kills all running scripts.
'for use in cases of massive errors, quiting to titlescreen or loading a game.

 FOR i = nowscript TO 0 STEP -1 
  IF scrat(i).scrnum > -1 THEN script(scrat(i).scrnum).refcount -= 1
 NEXT
 nowscript = -1

 destroystack(scrst)  'temp
 createstack(scrst)

END SUB

SUB resetinterpreter
'unload all scripts and wipe interpreter state. use when quitting the game.

 killallscripts

 freescripts(0)
END SUB

SUB reloadscript (si as ScriptInst, updatestats)
 IF si.scrnum = -1 THEN
  si.scrnum = loadscript(si.id)
  IF si.scrnum = -1 THEN killallscripts: EXIT SUB
  script(si.scrnum).refcount += 1
  IF updatestats THEN script(si.scrnum).totaluse += 1
 END IF
 IF updatestats THEN 
  scriptctr += 1
  script(si.scrnum).lastuse = scriptctr
 END IF
END SUB

SUB debug_npcs ()
 debug "NPC types:"
 FOR i AS INTEGER = 0 TO 35
  debug " ID " & i & ": pic=" & npcs(i).picture & " pal=" & npcs(i).palette
 NEXT
 debug "NPC instances:"
 FOR i AS INTEGER = 0 TO 299
  WITH npc(i)
   IF .id <> 0 THEN
    DIM AS INTEGER drawX, drawY
    IF framewalkabout(npc(i).x, npc(i).y + gmap(11), drawX, drawY, scroll(0) * 20, scroll(1) * 20, gmap(5)) THEN
     debug " " & i & ": ID=" & SGN(.id) * (ABS(.id) - 1) & " x=" & .x & " y=" & .y & " screenx=" & drawX & " screeny=" & drawY
    ELSE
     debug " " & i & ": ID=" & SGN(.id) * (ABS(.id) - 1) & " x=" & .x & " y=" & .y
    END IF
   END IF
  END WITH
 NEXT
END SUB

SUB npc_debug_display ()
 DIM temp AS STRING
 STATIC tog
 tog = tog XOR 1
 FOR i AS INTEGER = 0 TO 299
  WITH npc(i)
   IF .id <> 0 THEN
    DIM AS INTEGER drawX, drawY
    IF framewalkabout(npc(i).x, npc(i).y + gmap(11), drawX, drawY, scroll(0) * 20, scroll(1) * 20, gmap(5)) THEN
     textcolor uilook(uiText), 0
     'the numbers can overlap quite badly, try to squeeze them in
     temp = STR$(SGN(.id) * (ABS(.id) - 1))
     printstr MID$(temp, 1, 1), drawX, drawY + 4, dpage
     printstr MID$(temp, 2, 1), drawX + 7, drawY + 4, dpage
     printstr MID$(temp, 3, 1), drawX + 14, drawY + 4, dpage
     textcolor uilook(uiDescription), 0
     temp = STR$(i + 1)
     printstr MID$(temp, 1, 1), drawX, drawY + 12, dpage
     printstr MID$(temp, 2, 1), drawX + 7, drawY + 12, dpage
     printstr MID$(temp, 3, 1), drawX + 14, drawY + 12, dpage
    END IF
   END IF
  END WITH
 NEXT
END SUB