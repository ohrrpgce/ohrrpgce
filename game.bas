'OHRRPGCE GAME - Main module
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE SUB verquit ()
DECLARE SUB keyboardsetup ()
DECLARE SUB cathero ()
DECLARE SUB setScriptArg (arg%, value%)
DECLARE SUB showplotstrings ()
DECLARE SUB innRestore (stat%())
DECLARE SUB exitprogram (needfade%)
DECLARE SUB quitcleanup ()
DECLARE FUNCTION wrappass (x%, y%, xgo%, ygo%, isveh%)
DECLARE SUB wrapaheadxy (x%, y%, direction%, distance%, unitsize%)
DECLARE SUB aheadxy (x%, y%, direction%, distance%)
DECLARE SUB wrapxy (x%, y%, wide%, high%)
DECLARE FUNCTION framewalkabout% (x%, y%, framex%, framey%, mapwide%, maphigh%, wrapmode%)
DECLARE SUB initgamedefaults ()
DECLARE SUB templockexplain ()
DECLARE SUB cleanuptemp ()
DECLARE FUNCTION getfilelist% (wildcard$)
DECLARE SUB scriptadvanced (id%)
DECLARE FUNCTION vehiclestuff% (disx%, disy%, foep%, vehedge%)
DECLARE SUB touchfile (f$)
DECLARE FUNCTION checkfordeath (stat())
DECLARE SUB loadsay (choosep%, say%, sayer%, showsay%, say$(), saytag%(), choose$(), chtag%(), saybit%(), sayenh%())
DECLARE SUB correctbackdrop ()
DECLARE SUB unequip (who%, where%, defwep%, stat%(), resetdw%)
DECLARE FUNCTION isonscreen% (x%, y%)
DECLARE SUB readjoysettings ()
DECLARE FUNCTION settingstring% (searchee$, setting$, result$)
DECLARE SUB interpolatecat ()
DECLARE SUB setdebugpan ()
DECLARE SUB writescriptvar (id%, newval%)
DECLARE FUNCTION readscriptvar% (id%)
DECLARE FUNCTION gethighbyte% (n%)
DECLARE SUB wrappedsong (songnumber%)
DECLARE SUB scriptmisc (id%)
DECLARE SUB scriptcam (id%)
DECLARE SUB scriptnpc (id%)
DECLARE SUB scriptstat (id%, stat%())
DECLARE FUNCTION rpad$ (s$, pad$, size%)
DECLARE FUNCTION readglobalstring$ (index%, default$, maxlen%)
DECLARE FUNCTION getnpcref% (seekid%, offset%)
DECLARE SUB suspendresume (id%)
DECLARE SUB scriptwatcher (page%)
DECLARE SUB onkeyscript (scriptnum%)
DECLARE SUB waitcommands (id%)
DECLARE SUB getpal16 (array%(), aoffset%, foffset%)
DECLARE SUB scriptpalette (id%)
DECLARE SUB greyscalepal ()
DECLARE SUB tweakpalette ()
DECLARE SUB arslhero (saytag%(), stat%())
DECLARE SUB forceparty (stat%())
DECLARE SUB doequip (toequip%, who%, where%, defwep%, stat%())
DECLARE SUB scriptdump (s$)
DECLARE SUB getitem (getit%, num%)
DECLARE SUB doihavebits ()
DECLARE SUB npcplot ()
DECLARE SUB vishero (stat%())
DECLARE SUB reloadnpc (stat%())
DECLARE FUNCTION vehpass% (n%, tile%, default%)
DECLARE SUB initgame ()
DECLARE FUNCTION readfoemap% (x%, y%, wide%, high%, fh%)
DECLARE SUB playtimer ()
DECLARE FUNCTION functiondone% ()
DECLARE FUNCTION functionread% ()
DECLARE SUB subreturn ()
DECLARE SUB subdoarg ()
DECLARE SUB unwindtodo (levels%)
DECLARE SUB resetgame (map%, foep%, stat%(), stock%(), showsay%, scriptout$, sayenh%())
DECLARE FUNCTION countitem% (it%)
DECLARE SUB scriptmath ()
DECLARE SUB fatalerror (e$)
DECLARE FUNCTION movdivis% (xygo%)
DECLARE SUB scripterr (e$)
DECLARE SUB calibrate ()
DECLARE FUNCTION runscript% (n%, index%, newcall%, er$)
DECLARE SUB getmapname (mapname$, M%)
DECLARE FUNCTION istag% (num%, zero%)
DECLARE SUB evalitemtag ()
DECLARE SUB evalherotag (stat%())
DECLARE SUB tagdisplay ()
DECLARE SUB rpgversion (v%)
DECLARE SUB cycletile (cycle%(), tastuf%(), pt%(), skip%())
DECLARE SUB loadtanim (n%, tastuf%())
DECLARE SUB loaddoor (map%, door%())
DECLARE SUB reinitnpc (remember%, map%)
DECLARE FUNCTION findhero% (who%, f%, l%, d%)
DECLARE SUB doswap (s%, d%, stat%())
DECLARE FUNCTION howmanyh% (f%, l%)
DECLARE SUB heroswap (iAll%, stat%())
DECLARE SUB patcharray (array%(), n$, max%)
DECLARE SUB drawsay (saybit%(), sayenh%(), say$(), showsay%, choose$(), choosep%)
DECLARE SUB shop (id%, needf%, stock%(), stat%(), map%, foep%, mx%, my%, tastuf%())
DECLARE SUB minimap (x%, y%, tastuf%())
DECLARE FUNCTION onwho% (w$, alone)
DECLARE FUNCTION shoption (inn%, price%, needf%, stat%())
DECLARE SUB itstr (i%)
DECLARE SUB control ()
DECLARE FUNCTION picksave% (load%)
DECLARE SUB savegame (slot%, map%, foep%, stat%(), stock())
DECLARE SUB loadgame (slot%, map%, foep%, stat%(), stock())
DECLARE SUB equip (pt%, stat%())
DECLARE FUNCTION items% (stat%())
DECLARE SUB delitem (it%, num%)
DECLARE SUB oobcure (w%, t%, atk%, spred%, stat%())
DECLARE SUB spells (pt%, stat%())
DECLARE SUB status (pt%, stat%())
DECLARE SUB getnames (stat$())
DECLARE SUB resetlmp (slot%, lev%)
DECLARE SUB loadfoe (i%, formdata%(), es%(), x%(), y%(), p%(), v%(), w%(), h%(), ext$(), bits%(), stat%(), ebits%(), batname$())
DECLARE FUNCTION inflict (w%, t%, stat%(), x%(), y%(), w%(), h%(), harm$(), hc%(), hx%(), hy%(), atk%(), tcount%, die%(), bits%())
DECLARE FUNCTION battle (form%, fatal%, exstat%())
DECLARE SUB addhero (who%, slot%, stat%())
DECLARE FUNCTION atlevel% (now%, a0%, a99%)
DECLARE FUNCTION range% (n%, r%)
DECLARE FUNCTION xstring% (s$, x%)
DECLARE SUB snapshot ()
DECLARE FUNCTION checksaveslot (slot%)
DECLARE SUB defaultc ()
DECLARE SUB forcedismount (choosep, say, sayer, showsay, say$(), saytag(), choose$(), chtag(), saybit(), sayenh(), catd(), foep)
DECLARE SUB setusermenu (menu$(), mt%, mi%())
DECLARE FUNCTION maplumpname$ (map, oldext$)
DECLARE SUB makebackups
DECLARE SUB setmapxy ()
DECLARE SUB drawnpcs ()
DECLARE FUNCTION wrapcollision (xa%, ya%, xgoa%, ygoa%, xb%, yb%, xgob%, ygob%)
DECLARE FUNCTION cropmovement (x%, y%, xgo%, ygo%)
DECLARE FUNCTION wraptouch (x1%, y1%, x2%, y2%, distance%)
DECLARE FUNCTION titlescr% ()

'---INCLUDE FILES---
'$INCLUDE: 'compat.bi'
'$INCLUDE: 'allmodex.bi'
'$INCLUDE: 'common.bi' 
'$INCLUDE: 'gglobals.bi'
'$INCLUDE: 'const.bi'
'$INCLUDE: 'scrconst.bi'
'$INCLUDE: 'uiconst.bi'

'DEBUG debug "started debug session "+date$+" "+time$

REMEMBERSTATE


'---GET TEMP DIR---
tmpdir$ = aquiretempdir$
commandlineargs

'DEBUG debug "Thestart"
thestart:

'$INCLUDE: 'gver.txt'

'DEBUG debug "dim (almost) everything"

DIM font(1024), master(767), buffer(16384), pal16(448), timing(4), joy(14), music(16384)
DIM door(206), gen(104), npcs(1500), saytag(21), tag(127), hero(40), stat(40, 1, 16), bmenu(40, 5), spell(40, 3, 23), lmp(40, 7), foef(254), menu$(20), exlev&(40, 1), names$(40), mi(10), gotj(2), veh(21)
DIM eqstuf(40, 4), gmap(20), csetup(20), carray(20), stock(99, 49), choose$(1), chtag(1), saybit(0), sayenh(6), catx(15), caty(15), catz(15), catd(15), xgo(3), ygo(3), herospeed(3), wtog(3), say$(7), hmask(3),  _
tastuf(40), cycle(1), cycptr(1), cycskip(1), herobits(59, 3), itembits(255, 3)
DIM mapname$, catermask(0), nativehbits(40, 4), keyv(55, 1)
DIM script(4096), heap(2048), global(1024), astack(512), scrat(128, 14), retvals(32), plotstring$(31), plotstrX(31), plotstrY(31), plotstrCol(31), plotstrBGCol(31), plotstrBits(31)
DIM uilook(uiColors)
DIM inventory(inventoryMax) as InventSlot
DIM npc(300) as NPCInst
DIM didgo(0 TO 3)
'all global variables have to be dimmed to be used with EXTERN
DIM mapx, mapy, vpage, dpage, fadestate, fmvol, speedcontrol, tmpdir$, usepreunlump, lockfile, gold, lastsaveslot, abortg, exename$, sourcerpg$, foemaph, presentsong, framex, framey, game$, workingdir$, version$ 
DIM nowscript, scriptret, nextscroff

'--stuff we used to DIM here, but have defered to later
'DIM scroll(16002), pass(16002)

'DEBUG debug "dim binsize arrays"
'$INCLUDE: 'binsize.bi'

'DEBUG debug "setup directories"

'---Get work dir and exe name---
tmpdir$ = aquiretempdir$
workingdir$ = tmpdir$ + "playing.tmp"
exename$ = trimextension$(trimpath$(COMMAND$(0)))


'DEBUG debug "create working.tmp"

'---If workingdir$ does not already exist, it must be created---
IF isdir(workingdir$) THEN
 'DEBUG debug workingdir$+" already exists"
 touchfile workingdir$ + SLASH + "delete.tmp"
 'DEBUG debug "erasing "+workingdir$+"\"+ALLFILES
 ON ERROR GOTO tempDirErr
 cleanuptemp
 ON ERROR GOTO 0
ELSE
 makedir workingdir$
END IF

'-- Init joysticks
FOR i = 0 TO 1
 gotj(i) = readjoy(joy(), i)
NEXT i

dpage = 1: vpage = 0
speedcontrol = 80
presentsong = -1
gen(60) = 0'--leave joystick calibration enabled
'fpstimer! = TIMER

'DEBUG debug "randomize timer"

RANDOMIZE TIMER
FOR i = 1 TO 15
 master(i * 3 + 0) = SGN(i AND 4) * 32 + SGN(i AND 8) * 16
 master(i * 3 + 1) = SGN(i AND 2) * 32 + SGN(i AND 8) * 16
 master(i * 3 + 2) = SGN(i AND 1) * 32 + SGN(i AND 8) * 16
NEXT i

'DEBUG debug "load font"

getdefaultfont font()
'get default ui colours
getui ""

'DEBUG debug "set mode-X"
setmodex
setwindowtitle "O.H.R.RPG.C.E"

'DEBUG debug "init error-handler"
ON ERROR GOTO modeXerr

'DEBUG debug "set diskpages"
setdiskpages buffer(), 200, 0

'DEBUG debug "apply font"
setfont font()

keyboardsetup

textcolor uilook(uiText), 0
FOR i = 0 TO 31
 plotstrCol(i) = uilook(uiText)
NEXT i

'DEBUG debug "init sound"
setupmusic music()

setupsound
'resetfm
'setfmvol 7
fmvol = getfmvol
setfmvol 0

'DEBUG debug "set up default controls"
defaultc

'---IF A VALID RPG FILE WAS SPECIFIED ON THE COMMAND LINE, RUN IT, ELSE BROWSE---
'---ALSO CHECKS FOR GAME.EXE RENAMING
'DEBUG debug "enable autorunning"
autorungame = 0
a$ = getcommandline
IF NOT linux THEN
  IF MID$(a$, 2, 1) <> ":" THEN a$ = curdir$ + SLASH + a$
END IF
IF LCASE$(RIGHT$(a$, 4)) = ".rpg" AND isfile(a$) THEN
 sourcerpg$ = a$
 autorungame = 1
ELSEIF LEN(a$) AND isdir(a$) THEN 'perhaps it's an unlumped folder?
 'check for essentials
 IF isfile(a$ + SLASH + "archinym.lmp") THEN 'ok, accept it
  autorungame = 1
  usepreunlump = 1
  sourcerpg$ = a$
  workingdir$ = a$
 END IF
ELSE
 IF LCASE$(exename$) <> "game" THEN
  IF isfile(exepath + SLASH + exename$ + ".rpg") THEN
   sourcerpg$ = exepath + SLASH + exename$ + ".rpg"
   autorungame = 1
  END IF
 END IF
END IF
IF autorungame = 0 THEN
 'DEBUG debug "browse for RPG"
 sourcerpg$ = browse$(7, "", "*.rpg", tmpdir$, 1)
 IF sourcerpg$ = "" THEN exitprogram 0
 IF isdir(sourcerpg$) THEN
  usepreunlump = 1
  workingdir$ = sourcerpg$
 END IF 
END IF

'--open a lockfile in the working directory to notify other instances
'--of GAME.EXE that it is taken.
lockfile = FREEFILE
OPEN workingdir$ + SLASH + "lockfile.tmp" FOR BINARY AS #lockfile

IF autorungame = 0 THEN
 rectangle 4, 3, 312, 14, 9, vpage
 rectangle 5, 4, 310, 12, 1, vpage
END IF

edgeprint "Loading...", xstring("Loading...", 160), 6, uilook(uiText), vpage
setvispage vpage 'refresh
'DEBUG debug "unlumping "+sourcerpg$

'---GAME SELECTED, PREPARING TO PLAY---
DIM lumpbuf(16383)
IF usepreunlump = 0 THEN 
 unlump sourcerpg$, workingdir$ + SLASH, lumpbuf()
END IF

initgame '--set game$

makebackups 'make a few backup lumps

unlump game$ + ".hsp", workingdir$ + SLASH, lumpbuf()
ERASE lumpbuf

'DEBUG debug "dim big stuff *after* unlumping"
DIM scroll(16002), pass(16002)

fadeout 0, 0, 0, -1
needf = 1

xbload game$ + ".mas", master(), "master palette missing from " + game$
xbload game$ + ".fnt", font(), "font missing from " + game$
xbload game$ + ".gen", buffer(), "general data missing from " + game$
FOR i = 0 TO 104
 gen(i) = buffer(i)
NEXT i

rpgversion gen(genVersion)

'Get UI colours
getui workingdir$ + SLASH + "uilook.bin"

setfont font()
setpicstuf buffer(), 50, -1
FOR i = 0 TO 254
 loadset game$ + ".efs", i, 0
 foef(i) = buffer(0)
NEXT i
j = 0

beginplay:

initgamedefaults
fatal = 0: abortg = 0
foep = range(100, 60)
map = gen(104)

makebackups 'make a few backup lumps

nowscript = -1
nextscroff = 0
depth = 0
releasestack
setupstack astack(), 1024, workingdir$ + SLASH + "stack.tmp"

temp = -1
IF readbit(gen(), genBits, 11) = 0 THEN
 IF titlescr = 0 THEN GOTO resetg
 IF readbit(gen(), genBits, 12) = 0 THEN temp = picksave(1)
ELSE
 IF readbit(gen(), genBits, 12) = 0 THEN
  IF gen(2) > 0 THEN wrappedsong gen(2) - 1
  fademusic fmvol
  clearpage 3
  temp = picksave(2)
 END IF
END IF
'DEBUG debug "picked save slot"+XSTR$(temp)
fademusic 0
stopsong
fadeout 0, 0, 0, -1
IF temp = -2 THEN GOTO resetg
IF temp >= 0 THEN
 GOSUB doloadgame
ELSE
 clearpage 0
 clearpage 1
 addhero 1, 0, stat()
 IF gen(41) > 0 THEN
  rsr = runscript(gen(41), nowscript + 1, -1, "newgame")
 END IF
END IF
ERASE scroll, pass
GOSUB preparemap
doihavebits
evalherotag stat()
needf = 1: ng = 1

'DEBUG debug "pre-call movement"
setmapdata pass(), pass(), 0, 0
GOSUB movement
setkeys
DO
 'DEBUG debug "top of master loop"
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 'DEBUG debug "read controls"
 control
 IF gmap(15) THEN onkeyscript gmap(15)
 'DEBUG debug "enter script interpreter"
 GOSUB interpret
 'DEBUG debug "increment timers"
 playtimer
 'DEBUG debug "keyboard handling"
 IF carray(5) > 1 AND showsay = 0 AND needf = 0 AND readbit(gen(), 44, suspendplayer) = 0 AND veh(0) = 0 AND xgo(0) = 0 AND ygo(0) = 0 THEN
  GOSUB usermenu
  evalitemtag
  npcplot
 END IF
 IF showsay = 0 AND needf = 0 AND readbit(gen(), 44, suspendplayer) = 0 AND veh(6) = 0 THEN
  IF xgo(0) = 0 AND ygo(0) = 0 THEN
   DO
    IF carray(0) > 0 THEN ygo(0) = 20: catd(0) = 0: EXIT DO
    IF carray(1) > 0 THEN ygo(0) = -20: catd(0) = 2: EXIT DO
    IF carray(2) > 0 THEN xgo(0) = 20: catd(0) = 3: EXIT DO
    IF carray(3) > 0 THEN xgo(0) = -20: catd(0) = 1: EXIT DO
    IF carray(4) > 1 AND veh(0) = 0 THEN
     auto = 0
     GOSUB usething
    END IF
    EXIT DO
   LOOP
  END IF
 END IF
 'debug "before nextsay:"
 IF carray(4) > 1 AND showsay = 1 AND readbit(gen(), 44, suspendboxadvance) = 0 THEN
  GOSUB nextsay
 END IF
 'debug "after nextsay:"
 IF veh(0) THEN
  'DEBUG debug "evaluate vehicles"
  setmapdata pass(), pass(), 0, 0
  pasx = INT(catx(0) / 20)
  pasy = INT(caty(0) / 20)
  vehedge = 0
  IF readbit(veh(), 9, 6) AND readbit(veh(), 9, 7) THEN
   '--dismount-ahead is true, dismount-passwalls is true
   SELECT CASE catd(0)
    CASE 0
     pasy = pasy - 1
     IF pasy < 0 THEN pasy = (scroll(1) - 1) : vehedge = 1
    CASE 1
     pasx = pasx + 1
     IF pasx > (scroll(0) - 1) THEN pasx = 0 : vehedge = 1
    CASE 2
     pasy = pasy + 1
     IF pasy > (scroll(1) - 1) THEN pasy = 0 : vehedge = 1
    CASE 3
     pasx = pasx - 1
     IF pasx < 0 THEN pasx = (scroll(0) - 1) : vehedge = 1
   END SELECT
  END IF
  tmp = vehiclestuff(pasx, pasy, foep, vehedge)
  SELECT CASE tmp
   CASE IS < 0
    rsr = runscript(ABS(tmp), nowscript + 1, -1, "vehicle")
   CASE 1
    GOSUB usermenu
    evalherotag stat()
    evalitemtag
    npcplot
   CASE IS > 1
    say = tmp - 1
    loadsay choosep, say, sayer, showsay, say$(), saytag(), choose$(), chtag(), saybit(), sayenh()
  END SELECT
 END IF
 IF showsay = 1 THEN
  IF carray(0) > 1 THEN choosep = 0
  IF carray(1) > 1 THEN choosep = 1
 END IF
 'DEBUG debug "setmapdata pass"
 setmapdata pass(), pass(), 0, 0
 'DEBUG debug "hero movement"
 GOSUB movement
 'DEBUG debug "NPC movement"
 GOSUB movenpc
 IF readbit(gen(), 101, 8) = 0 THEN
  '--debugging keys
  'DEBUG debug "evaluate debugging keys"
  IF keyval(41) > 0 THEN
   IF keyval(59) > 1 THEN
    catx(0) = (catx(0) \ 20) * 20
    caty(0) = (caty(0) \ 20) * 20
    xgo(0) = 0
    ygo(0) = 0
   END IF
  ELSE
   IF keyval(59) > 1 AND showsay = 0 THEN minimap catx(0), caty(0), tastuf()
  END IF
  IF keyval(60) > 1 AND showsay = 0 THEN
   savegame 32, map, foep, stat(), stock()
  END IF
  IF keyval(61) > 1 AND showsay = 0 THEN
   wantloadgame = 33
  END IF
  IF keyval(62) > 1 THEN showtags = showtags XOR 1
  IF keyval(63) > 1 THEN
   SELECT CASE gen(cameramode)
    CASE herocam
     IF gen(cameraArg) < 15 THEN
      gen(cameraArg) = gen(cameraArg) + 5
     ELSE
      gen(cameraArg) = 0
     END IF
    CASE ELSE
     gen(cameramode) = herocam
     gen(cameraArg) = 0
   END SELECT
  END IF
  IF keyval(64) > 0 AND (gen(cameramode) <> pancam OR gen(cameraArg2) = 0) THEN
   '--only permit movement when not already panning
   IF keyval(72) > 0 THEN
    gen(cameraArg) = 0 'north
    setdebugpan
   END IF
   IF keyval(77) > 0 THEN
    gen(cameraArg) = 1 'east
    setdebugpan
   END IF
   IF keyval(80) > 0 THEN
    gen(cameraArg) = 2 'south
    setdebugpan
   END IF
   IF keyval(75) > 0 THEN
    gen(cameraArg) = 3 'west
    setdebugpan
   END IF
  END IF
  IF keyval(65) > 1 THEN
   showmapname = 15
   IF readbit(gen(), 101, 9) = 0 THEN
    mapname$ = "levelup bug enabled"
    setbit gen(), 101, 9, 1
   ELSE
    mapname$ = "levelup bug disabled"
    setbit gen(), 101, 9, 0
   END IF
  END IF
  IF keyval(66) > 1 THEN patcharray gen(), "gen", 104
  IF keyval(67) > 1 THEN patcharray gmap(), "gmap", 20
  IF keyval(68) > 1 THEN scrwatch = loopvar(scrwatch, 0, 2, 1)
  IF keyval(87) > 1 THEN ghost = ghost XOR 1
  IF keyval(29) > 0 THEN
   IF keyval(74) > 1 THEN speedcontrol = large(speedcontrol - 1, 10): scriptout$ = XSTR$(speedcontrol)
   IF keyval(78) > 1 THEN speedcontrol = small(speedcontrol + 1, 160): scriptout$ = XSTR$(speedcontrol)
  END IF
 END IF
 IF wantloadgame > 0 THEN
  'DEBUG debug "loading game slot" + XSTR$(wantloadgame - 1)
  temp = wantloadgame - 1
  wantloadgame = 0
  resetgame map, foep, stat(), stock(), showsay, scriptout$, sayenh()
  initgamedefaults
  nowscript = -1
  nextscroff = 0
  releasestack
  setupstack astack(), 1024, workingdir$ + SLASH + "stack.tmp"
  fademusic 0
  stopsong
  fadeout 0, 0, 0, -1
  needf = 1: ng = 1
  GOSUB doloadgame
  ERASE scroll, pass
  GOSUB preparemap
 END IF
 'DEBUG debug "random enemies"
 IF foep = 0 AND readbit(gen(), 44, suspendrandomenemies) = 0 AND (veh(0) = 0 OR veh(11) > -1) THEN
  temp = readfoemap(INT(catx(0) / 20), INT(caty(0) / 20), scroll(0), scroll(1), foemaph)
  IF veh(0) AND veh(11) > 0 THEN temp = veh(11)
  IF temp > 0 THEN
   setpicstuf buffer(), 50, -1
   loadset game$ + ".efs", temp - 1, 0
   FOR i = 0 TO INT(RND * range(19, 27))
    foenext = loopvar(foenext, 0, 19, 1)
    breakout = 0
    DO WHILE buffer(1 + foenext) = 0
     breakout = breakout + 1
     IF breakout > 40 THEN EXIT FOR
     foenext = loopvar(foenext, 0, 19, 1)
    LOOP
   NEXT i
   batform = buffer(1 + foenext) - 1
   IF gmap(13) <= 0 THEN
    '--normal battle
    fatal = 0
    ERASE scroll, pass
    wonbattle = battle(batform, fatal, stat())
    afterbat = 1
    GOSUB preparemap
    needf = 2
   ELSE
    rsr = runscript(gmap(13), nowscript + 1, -1, "rand-battle")
    IF rsr = 1 THEN
     setScriptArg 0, batform
     setScriptArg 1, temp
    END IF
   END IF
   foep = range(100, 60)
  END IF
 END IF
 'DEBUG debug "check for death"
 IF fatal = 1 THEN
  '--this is what happens when you die in battle
  showsay = 0
  IF gen(42) > 0 THEN
   rsr = runscript(gen(42), nowscript + 1, -1, "death")
   IF rsr = 1 THEN
    fatal = 0
    needf = 2
   END IF
  ELSE
   fadeout 63, 0, 0, 0
  END IF
 END IF
 GOSUB displayall
 IF fatal = 1 OR abortg > 0 THEN
  resetgame map, foep, stat(), stock(), showsay, scriptout$, sayenh()
  'if skip loadmenu and title bits set, quit
  IF readbit(gen(), genBits, 11) AND (readbit(gen(), genBits, 12) OR abortg = 2) THEN GOTO resetg ELSE GOTO beginplay
 END IF
 'DEBUG debug "swap video pages"
 SWAP vpage, dpage
 setvispage vpage
 'DEBUG debug "fade in"
 'DEBUG debug "needf"+XSTR$(needf)
 IF needf = 1 AND fatal = 0 THEN
  needf = 0
  fademusic fmvol
  fadein 0
  setkeys
 END IF
 IF needf > 1 THEN needf = needf - 1
 'DEBUG debug "tail of main loop"
 dowait
LOOP

doloadgame:
loadgame temp, map, foep, stat(), stock()
afterload = -1
IF gen(57) > 0 THEN
 rsr = runscript(gen(57), nowscript + 1, -1, "loadgame")
 IF rsr = 1 THEN
  '--pass save slot as argument
  IF temp = 32 THEN temp = -1 'quickload slot
  setScriptArg 0, temp
 END IF
END IF
samemap = -1
RETRACE

displayall:
'DEBUG debug "display"
IF gen(58) = 0 AND gen(50) = 0 THEN
 '---NORMAL DISPLAY---
 'DEBUG debug "normal display"
 setmapdata scroll(), pass(), 0, 0
 setanim tastuf(0) + cycle(0), tastuf(20) + cycle(1)
 cycletile cycle(), tastuf(), cycptr(), cycskip()
 'DEBUG debug "drawmap"
 overlay = 1
 IF readbit(gen(), 44, suspendoverlay) THEN overlay = 0
 drawmap mapx, mapy, overlay, dpage
 'DEBUG debug "draw npcs and heroes"
 IF gmap(16) = 1 THEN
  cathero
  drawnpcs
 ELSE
  drawnpcs
  cathero
 END IF
 'DEBUG debug "drawoverhead"
 IF readbit(gen(), 44, suspendoverlay) = 0 THEN drawmap mapx, mapy, 2, dpage
ELSE '---END NORMAL DISPLAY---
 'DEBUG debug "backdrop display"
 copypage 3, dpage
END IF '---END BACKDROP DISPLAY---
'DEBUG debug "text box"
IF showsay > 0 THEN drawsay saybit(), sayenh(), say$(), showsay, choose$(), choosep
'DEBUG debug "map name"
IF showmapname > 0 AND gmap(4) >= showmapname THEN
 showmapname = showmapname - 1: edgeprint mapname$, xstring(mapname$, 160), 180, uilook(uiText), dpage
ELSE
 showmapname = 0
END IF
'--FPS
'framecount = framecount + 1
'IF fpstimer! + 1 < TIMER THEN
'  scriptout$ = "FPS" + XSTR$(framecount)
'  fpstimer! = TIMER
'  framecount = 0
'END IF
edgeprint scriptout$, 0, 190, uilook(uiText), dpage
showplotstrings
IF showtags > 0 THEN tagdisplay
IF scrwatch THEN scriptwatcher dpage
RETRACE

usermenu:
setusermenu menu$(), mt, mi()
IF gmap(2) = 0 THEN
 '--minimap not available
 o = 0
 FOR i = 0 TO mt
  IF mi(i) = 2 THEN o = 1: SWAP mi(i), mi(i + 1)
 NEXT i
 IF o = 1 THEN mt = mt - 1
END IF
IF gmap(3) = 0 THEN
 '--save not available
 o = 0
 FOR i = 0 TO mt
  IF mi(i) = 6 THEN o = 1: SWAP mi(i), mi(i + 1)
 NEXT i
 IF o = 1 THEN mt = mt - 1
END IF
csr = 0: pt = 0
setkeys
DO
 setwait timing(), speedcontrol
 setkeys
 tog = tog XOR 1
 playtimer
 control
 GOSUB displayall
 IF carray(5) > 1 OR abortg > 0 THEN
  EXIT DO
 END IF
 IF carray(0) > 1 THEN pt = loopvar(pt, 0, mt, -1)
 IF carray(1) > 1 THEN pt = loopvar(pt, 0, mt, 1)
 IF mi(pt) = 7 THEN
  '--volume control
  IF carray(2) > 1 THEN fmvol = large(fmvol - 1, 0): setfmvol fmvol
  IF carray(3) > 1 THEN fmvol = small(fmvol + 1, 15): setfmvol fmvol
 END IF
 IF carray(4) > 1 THEN
  IF mi(pt) = 4 THEN
   say = items(stat())
   IF say THEN
    '--player has used an item that calls a text box--
    IF say > 0 THEN
     loadsay choosep, say, sayer, showsay, say$(), saytag(), choose$(), chtag(), saybit(), sayenh()
    END IF
    EXIT DO
   END IF
  END IF
  IF mi(pt) = 1 THEN
   w = onwho(readglobalstring$(104, "Whose Status?", 20), 0)
   IF w >= 0 THEN
    status w, stat()
   END IF
  END IF
  IF mi(pt) = 3 THEN
   w = onwho(readglobalstring$(106, "Whose Spells?", 20), 0)
   IF w >= 0 THEN
    spells w, stat()
   END IF
  END IF
  IF mi(pt) = 6 THEN
   temp = picksave(0)
   IF temp >= 0 THEN savegame temp, map, foep, stat(), stock()
   reloadnpc stat()
  END IF
  IF mi(pt) = 5 THEN
   w = onwho(readglobalstring$(108, "Equip Whom?", 20), 0)
   IF w >= 0 THEN
    equip w, stat()
   END IF
  END IF
  IF mi(pt) = 2 THEN minimap catx(0), caty(0), tastuf()
  IF mi(pt) = 8 THEN
   heroswap readbit(gen(), 101, 5), stat()
  END IF
  IF mi(pt) = 0 THEN verquit
  '---After all sub-menus are done, re-evaluate the hero/item tags
  '---that way if you revive a hero, kill a hero swap out... whatever
  evalherotag stat()
  evalitemtag
 END IF
 centerfuz 160, 100, 120, (mt + 2) * 10, 1, dpage
 FOR i = 0 TO mt
  col = uilook(uiMenuItem)
  IF mi(i) = 7 AND fmvol THEN centerbox 160, 110 - ((mt + 2) * 10) * .5 + (i * 10), fmvol * 6, 10, 1, dpage
  IF pt = i THEN col = uilook(uiSelectedItem + tog)
  edgeprint menu$(mi(i)), xstring(menu$(mi(i)), 160), 106 - ((mt + 2) * 10) * .5 + (i * 10), col, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 'copypage 3, dpage
 dowait
LOOP
setkeys
FOR i = 0 TO 7: carray(i) = 0: NEXT i
fatal = checkfordeath(stat())
RETRACE

usething:
IF auto = 0 THEN
 ux = catx(0)
 uy = caty(0)
 wrapaheadxy ux, uy, catd(0), 20, 20
END IF
IF sayer < 0 THEN
  IF auto <> 2 THEN 'find the NPC to trigger the hard way
   sayer = -1
   j = -1
   DO
    j = j + 1
    IF j > 299 THEN RETRACE
    'would <= 19 do?
    'LOOP UNTIL ABS(npcl(j) - ux) < 16 AND ABS(npcl(j + 300) - uy) < 16 AND npcl(j + 600) > 0 AND (j <> veh(5) OR veh(0) = 0)
    IF npc(j).id > 0 AND (j <> veh(5) OR veh(0) = 0) THEN 'A
     dim nx,ny,nd
     nx = npc(j).x
     ny = npc(j).y
     nd = npc(j).dir
     IF (nx = ux AND ny = uy) THEN 'not moving NPCs
      EXIT DO
     ELSEIF nx MOD 20 <> 0 XOR ny mod 20 <> 0 THEN 'they're moving (i.e. misaligned)
      '--first check the tile the NPC is stepping into
      IF npc(j).ygo > 0 THEN ' up
       wrapaheadxy nx, ny, 0, ABS(npc(j).ygo), 20
      END IF
      IF npc(j).xgo < 0 THEN ' right
       wrapaheadxy nx, ny, 1, ABS(npc(j).xgo), 20
      END IF
      IF npc(j).ygo < 0 THEN ' down
       wrapaheadxy nx, ny, 2, ABS(npc(j).ygo), 20
      END IF
      IF npc(j).xgo > 0 THEN ' left
       wrapaheadxy nx, ny, 3, ABS(npc(j).xgo), 20
      END IF
      '--uncommenting the line below provides a helpful rectangle that shows the activation tile of an NPC
      'rectangle nx - mapx, ny - mapy, 20,20, 1, vpage : setvispage vpage 
      IF (nx = ux AND ny = uy) THEN 'check for activation
       EXIT DO
      END IF
      '--also check the tile the NPC is leaving
      nx = nx + SGN(npc(j).xgo) * 20
      ny = ny + SGN(npc(j).ygo) * 20
      '--uncommenting the line below provides a helpful rectangle that shows the activation tile of an NPC
      'rectangle nx - mapx, ny - mapy, 20,20, 4, vpage : setvispage vpage 
      IF (nx = ux AND ny = uy) THEN 'check for activation
       '--if activating an NPC that has just walked past us, cause it to back up
       npc(j).xgo = SGN(npc(j).xgo * -1) * (20 - ABS(npc(j).xgo))
       npc(j).ygo = SGN(npc(j).ygo * -1) * (20 - ABS(npc(j).ygo))
       EXIT DO
      END IF
     END IF
    END IF
   LOOP
   'UNTIL wraptouch(npcl(j), npcl(j + 300), ux, uy, 15) AND npcl(j + 600) > 0 AND (j <> veh(5) OR veh(0) = 0)
   sayer = j
  END IF
END IF
IF sayer >= 0 THEN
 '--Step-on NPCs cannot be used
 IF auto = 0 AND npcs((npc(sayer).id - 1) * 15 + 8) = 2 THEN sayer = -1 : RETRACE
 getit = npcs((npc(sayer).id - 1) * 15 + 6)
 IF getit THEN getitem getit, 1
 '---DIRECTION CHANGING-----------------------
 IF npcs((npc(sayer).id - 1) * 15 + 5) < 2 THEN
  recalld = npc(sayer).dir
  npc(sayer).dir = catd(0)
  npc(sayer).dir = loopvar(npc(sayer).dir, 0, 3, 1): npc(sayer).dir = loopvar(npc(sayer).dir, 0, 3, 1)
 END IF
 IF npcs((npc(sayer).id - 1) * 15 + 11) > 0 THEN
  '--One-time-use tag
  setbit tag(), 0, 1000 + npcs((npc(sayer).id - 1) * 15 + 11), 1
 END IF
 IF npcs((npc(sayer).id - 1) * 15 + 12) > 0 THEN
  '--summon a script directly from an NPC
  rsr = runscript(npcs((npc(sayer).id - 1) * 15 + 12), nowscript + 1, -1, "NPC")
  IF rsr = 1 THEN
   setScriptArg 0, npcs((npc(sayer).id - 1) * 15 + 13)
   setScriptArg 1, (sayer + 1) * -1 'reference
  END IF
 END IF
 vehuse = npcs((npc(sayer).id - 1) * 15 + 14)
 IF vehuse THEN '---activate a vehicle---
  setpicstuf buffer(), 80, -1
  loadset game$ + ".veh", vehuse - 1, 0
  setmapdata pass(), pass(), 0, 0
  IF vehpass(buffer(19), readmapblock(catx(0) \ 20, caty(0) \ 20), -1) THEN
   '--check mounting permissions first
   FOR i = 0 TO 7: veh(i) = 0: NEXT i
   FOR i = 8 TO 21: veh(i) = buffer(i): NEXT i
   veh(0) = -1
   veh(5) = sayer
   veh(7) = herospeed(0)
   herospeed(0) = 10
   setbit veh(), 6, 0, 1 '--trigger mounting sequence
   IF veh(14) > 1 THEN setbit tag(), 0, veh(14), 1
  END IF
 END IF
 say = npcs((npc(sayer).id - 1) * 15 + 4)
 SELECT CASE say
  CASE 0
   sayer = -1
  CASE IS > 0
   loadsay choosep, say, sayer, showsay, say$(), saytag(), choose$(), chtag(), saybit(), sayenh()
 END SELECT
 evalherotag stat()
 evalitemtag
 IF say = 0 THEN
  'reinitnpc 1, map
  npcplot
 END IF
END IF
RETRACE

nextsay:
IF sayenh(4) > 0 THEN
 '--backdrop needs resetting
 gen(58) = 0
 correctbackdrop
END IF
'---IF MADE A CHOICE---
IF readbit(saybit(), 0, 0) THEN
 IF ABS(chtag(choosep)) > 1 THEN setbit tag(), 0, ABS(chtag(choosep)), SGN(SGN(chtag(choosep)) + 1)
END IF
'---RESET MUSIC----
IF readbit(saybit(), 0, 3) THEN
 IF gmap(1) > 0 THEN
  wrappedsong gmap(1) - 1
 ELSE
  stopsong
 END IF
END IF
'---GAIN/LOSE CASH-----
IF istag(saytag(13), 0) THEN
 gold& = gold& + saytag(14)
 IF gold& > 2000000000 THEN gold& = 2000000000
 IF gold& < 0 THEN gold& = 0
END IF
'---SPAWN BATTLE--------
IF istag(saytag(5), 0) THEN
 fatal = 0
 ERASE scroll, pass
 wonbattle = battle(saytag(6), fatal, stat())
 afterbat = 1
 GOSUB preparemap
 foep = range(100, 60)
 needf = 1
END IF
'---GAIN/LOSE ITEM--------
IF istag(saytag(17), 0) THEN
 IF saytag(18) > 0 THEN getitem saytag(18), 1
 IF saytag(18) < 0 THEN delitem ABS(saytag(18)), 1
END IF
'---SHOP/INN/SAVE/ETC------------
IF istag(saytag(7), 0) THEN
 copypage vpage, 3
 IF saytag(8) > 0 THEN
  shop saytag(8) - 1, needf, stock(), stat(), map, foep, mx, my, tastuf()
  reloadnpc stat()
 END IF
 inn = 0
 IF saytag(8) < 0 THEN
  IF shoption(inn, ABS(saytag(8)), needf, stat()) THEN
   fadeout 0, 0, 20, 0
   needf = 1
  END IF
 END IF
 IF saytag(8) <= 0 AND inn = 0 THEN
  innRestore stat()
 END IF
 vishero stat()
 loadpage game$ + ".til", gmap(0), 3
END IF
'---ADD/REMOVE/SWAP/LOCK HERO-----------------
IF istag(saytag(9), 0) THEN arslhero saytag(), stat()
'---FORCE DOOR------
IF istag(saytag(15), 0) THEN
 dforce = saytag(16) + 1
 GOSUB opendoor
 IF needf = 0 THEN
  temp = readfoemap(INT(catx(0) / 20), INT(caty(0) / 20), scroll(0), scroll(1), foemaph)
  IF veh(0) AND veh(11) > 0 THEN temp = veh(11)
  IF temp > 0 THEN foep = large(foep - foef(temp - 1), 0)
  setmapdata scroll(), pass(), 0, 0
 END IF
 setmapxy
END IF
'---JUMP TO NEXT TEXT BOX--------
IF istag(saytag(11), 0) THEN
 IF saytag(12) < 0 THEN
  rsr = runscript(ABS(saytag(12)), nowscript + 1, -1, "textbox")
 ELSE
  say = saytag(12)
  loadsay choosep, say, sayer, showsay, say$(), saytag(), choose$(), chtag(), saybit(), sayenh()
  RETRACE
 END IF
END IF
evalherotag stat()
evalitemtag
'---DONE EVALUATING CONDITIONALS--------
'reinitnpc 1, map
vishero stat()
npcplot
IF sayer >= 0 THEN
 IF npc(sayer).id > 0 THEN
  IF npcs((npc(sayer).id - 1) * 15 + 5) = 1 THEN
   npc(sayer).dir = recalld
  END IF
 END IF
END IF
IF sayenh(4) > 0 THEN
 gen(58) = 0
 correctbackdrop
END IF
FOR i = 0 TO 6
 sayenh(i) = 0
NEXT i
showsay = 0
sayer = -1
setkeys
FOR i = 0 TO 7: carray(i) = 0: NEXT i
RETRACE

movement:
FOR whoi = 0 TO 3
 thisherotilex = INT(catx(whoi * 5) / 20)
 thisherotiley = INT(caty(whoi * 5) / 20)
 '--if if aligned in at least one direction and passibility is enabled ... and some vehicle stuff ...
 IF (movdivis(xgo(whoi)) OR movdivis(ygo(whoi))) AND ghost = 0 AND readbit(veh(), 9, 0) = 0 AND vehpass(veh(17), readmapblock(thisherotilex, thisherotiley), 0) = 0 THEN
  IF readbit(gen(), 44, suspendherowalls) = 0 AND veh(6) = 0 THEN
   '--this only happens if herowalls is on
   '--wrapping passability
   dummy = wrappass(thisherotilex, thisherotiley, xgo(whoi), ygo(whoi), veh(0))
  END IF
  IF readbit(gen(), 44, suspendobstruction) = 0 AND veh(6) = 0 THEN
   '--this only happens if obstruction is on
   FOR i = 0 TO 299
    IF npc(i).id > 0 THEN '---NPC EXISTS---
     IF npcs((npc(i).id - 1) * 15 + 8) < 2 THEN '---NPC IS AN OBSTRUCTION---
      IF wrapcollision (npc(i).x, npc(i).y, npc(i).xgo, npc(i).ygo, catx(whoi * 5), caty(whoi * 5), xgo(whoi), ygo(whoi)) THEN
       xgo(whoi) = 0: ygo(whoi) = 0
       id = (npc(i).id - 1)
       '--push the NPC
       IF npcs(id * 15 + 7) > 0 AND npc(i).xgo = 0 AND npc(i).ygo = 0 THEN
        temp = npcs(id * 15 + 7)
        IF catd(whoi) = 0 AND (temp = 1 OR temp = 2 OR temp = 4) THEN npc(i).ygo = 20
        IF catd(whoi) = 2 AND (temp = 1 OR temp = 2 OR temp = 6) THEN npc(i).ygo = -20
        IF catd(whoi) = 3 AND (temp = 1 OR temp = 3 OR temp = 7) THEN npc(i).xgo = 20
        IF catd(whoi) = 1 AND (temp = 1 OR temp = 3 OR temp = 5) THEN npc(i).xgo = -20
       END IF
       IF npcs(id * 15 + 8) = 1 AND whoi = 0 THEN
        IF wraptouch(npc(i).x, npc(i).y, catx(0), caty(0), 20) THEN
         ux = npc(i).x
         uy = npc(i).y
         auto = 1
         sayer = i
         GOSUB usething
        END IF
       END IF '---autoactivate
      END IF ' ---NPC IS IN THE WAY
     END IF ' ---NPC IS AN OBSTRUCTION
    END IF '---NPC EXISTS
   NEXT i
  END IF
 END IF'--this only gets run when starting a movement to a new tile
NEXT whoi
'--if the leader moved last time, and catapillar is enabled then make others trail
IF readbit(gen(), 44, suspendcatapillar) = 0 THEN
 IF xgo(0) OR ygo(0) THEN
  FOR i = 15 TO 1 STEP -1
   catx(i) = catx(i - 1)
   caty(i) = caty(i - 1)
   catd(i) = catd(i - 1)
  NEXT i
  FOR whoi = 0 TO 3
   wtog(whoi) = loopvar(wtog(whoi), 0, 3, 1)
  NEXT whoi
 END IF
ELSE
 FOR whoi = 0 TO 3
  IF xgo(whoi) OR ygo(whoi) THEN wtog(whoi) = loopvar(wtog(whoi), 0, 3, 1)
 NEXT whoi
END IF
FOR whoi = 0 TO 3
 didgo(whoi) = 0
 IF xgo(whoi) OR ygo(whoi) THEN
  '--this actualy updates the heros coordinates
  IF xgo(whoi) > 0 THEN xgo(whoi) = xgo(whoi) - herospeed(whoi): catx(whoi * 5) = catx(whoi * 5) - herospeed(whoi): didgo(whoi) = 1
  IF xgo(whoi) < 0 THEN xgo(whoi) = xgo(whoi) + herospeed(whoi): catx(whoi * 5) = catx(whoi * 5) + herospeed(whoi): didgo(whoi) = 1
  IF ygo(whoi) > 0 THEN ygo(whoi) = ygo(whoi) - herospeed(whoi): caty(whoi * 5) = caty(whoi * 5) - herospeed(whoi): didgo(whoi) = 1
  IF ygo(whoi) < 0 THEN ygo(whoi) = ygo(whoi) + herospeed(whoi): caty(whoi * 5) = caty(whoi * 5) + herospeed(whoi): didgo(whoi) = 1
 END IF

 o = whoi
 '--if catapillar is not suspended, only the leader's motion matters
 IF readbit(gen(), 44, suspendcatapillar) = 0 THEN o = 0

 '--leader always checks harm tiles, allies only if caterpillar is enabled
 IF whoi = 0 OR readbit(gen(), 101, 1) = 1 THEN
  '--Stuff that should only happen when you finish moving
  IF didgo(o) = 1 AND xgo(o) = 0 AND ygo(o) = 0 THEN
   '---check for harm tile
   p = readmapblock(catx(whoi * 5) \ 20, caty(whoi * 5) \ 20)
   IF (p AND 64) THEN
    o = -1
    FOR i = 0 TO whoi
     o = o + 1
     WHILE hero(o) = 0 AND o < 4: o = o + 1: WEND
    NEXT i
    IF o < 4 THEN
     stat(o, 0, 0) = bound(stat(o, 0, 0) - gmap(9), 0, stat(o, 1, 0))
     IF gmap(10) THEN
      rectangle 0, 0, 320, 200, gmap(10), vpage
      setvispage vpage
     END IF 
    END IF
    '--check for death
    fatal = checkfordeath(stat())
   END IF
  END IF
 END IF
 dummy = cropmovement(catx(whoi * 5), caty(whoi * 5), xgo(whoi), ygo(whoi))
NEXT whoi
'--only the leader may activate NPCs
IF (xgo(0) MOD 20 = 0) AND (ygo(0) MOD 20 = 0) AND (didgo(0) = 1 OR ng = 1) THEN
 '--finished a step
 ng = 0
 IF readbit(gen(), 44, suspendobstruction) = 0 THEN
  '--this only happens if obstruction is on
  FOR i = 0 TO 299
   IF npc(i).id > 0 THEN '---NPC EXISTS---
    IF veh(0) = 0 OR (readbit(veh(), 9, 2) AND veh(5) <> i) THEN
     IF npcs((npc(i).id - 1) * 15 + 8) = 2 THEN '---NPC IS PASSABLE---
      IF npc(i).x = catx(0) AND npc(i).y = caty(0) THEN '---YOU ARE ON NPC---
       ux = npc(i).x
       uy = npc(i).y
       auto = 1
       sayer = i
       GOSUB usething
      END IF'---YOU ARE ON NPC---
     END IF ' ---NPC IS PASSABLE---
    END IF '--veh okay
   END IF '---NPC EXISTS
  NEXT i
 END IF
 GOSUB opendoor
 IF needf = 0 THEN
  temp = readfoemap(catx(0) \ 20, caty(0) \ 20, scroll(0), scroll(1), foemaph)
  IF veh(0) AND veh(11) > 0 THEN temp = veh(11)
  IF temp > 0 THEN foep = large(foep - foef(temp - 1), 0)
  setmapdata scroll(), pass(), 0, 0
 END IF
 IF gmap(14) > 0 THEN
  rsr = runscript(gmap(14), nowscript + 1, -1, "eachstep")
  IF rsr = 1 THEN
   setScriptArg 0, catx(0) \ 20
   setScriptArg 1, caty(0) \ 20
   setScriptArg 2, catd(0)
  END IF
 END IF
END IF
setmapxy
RETRACE

movenpc:
FOR o = 0 TO 299
 IF npc(o).id > 0 THEN
  id = (npc(o).id - 1)
  '--if this is the active vehicle
  IF veh(0) AND veh(5) = o THEN
   '-- if we are not scrambling clearing or aheading
   IF readbit(veh(), 6, 0) = 0 AND readbit(veh(), 6, 4) = 0 AND readbit(veh(), 6, 5) = 0 THEN
    '--match vehicle to main hero
    npc(o).x = catx(0)
    npc(o).y = caty(0)
    npc(o).dir = catd(0)
    npc(o).frame = wtog(0)
   END IF
  ELSE
   movetype = npcs(id * 15 + 2)
   speedset = npcs(id * 15 + 3)
   IF movetype > 0 AND (speedset > 0 OR movetype = 8) AND sayer <> o AND readbit(gen(), 44, suspendnpcs) = 0 THEN
    IF npc(o).xgo = 0 AND npc(o).ygo = 0 THEN
     'RANDOM WANDER---
     IF movetype = 1 THEN
      rand = 25
      IF wraptouch(npc(o).x, npc(o).y, catx(0), caty(0), 20) THEN rand = 5
      IF INT(RND * 100) < rand THEN
       temp = INT(RND * 4)
       npc(o).dir = temp
       IF temp = 0 THEN npc(o).ygo = 20
       IF temp = 2 THEN npc(o).ygo = -20
       IF temp = 3 THEN npc(o).xgo = 20
       IF temp = 1 THEN npc(o).xgo = -20
      END IF
     END IF '---RANDOM WANDER
     'ASSORTED PACING---
     IF movetype > 1 AND movetype < 6 THEN
      IF npc(o).dir = 0 THEN npc(o).ygo = 20
      IF npc(o).dir = 2 THEN npc(o).ygo = -20
      IF npc(o).dir = 3 THEN npc(o).xgo = 20
      IF npc(o).dir = 1 THEN npc(o).xgo = -20
     END IF '---ASSORTED PACING
     'CHASE/FLEE---
     IF movetype > 5 AND movetype < 8 THEN
      rand = 100
      IF INT(RND * 100) < rand THEN
       IF INT(RND * 100) < 50 THEN
	IF caty(0) < npc(o).y THEN temp = 0
	IF caty(0) > npc(o).y THEN temp = 2
        IF gmap(5) = 1 AND caty(0) - scroll(1) * 10 > npc(o).y THEN temp = 0
        IF gmap(5) = 1 AND caty(0) + scroll(1) * 10 < npc(o).y THEN temp = 2
	IF caty(0) = npc(o).y THEN temp = INT(RND * 4)
       ELSE
        IF catx(0) < npc(o).x THEN temp = 3
        IF catx(0) > npc(o).x THEN temp = 1
        IF gmap(5) = 1 AND catx(0) - scroll(0) * 10 > npc(o).x THEN temp = 3
        IF gmap(5) = 1 AND catx(0) + scroll(0) * 10 < npc(o).x THEN temp = 1
        IF catx(0) = npc(o).x THEN temp = INT(RND * 4)
       END IF
       IF movetype = 7 THEN temp = loopvar(temp, 0, 3, 2)
       npc(o).dir = temp
       IF temp = 0 THEN npc(o).ygo = 20
       IF temp = 2 THEN npc(o).ygo = -20
       IF temp = 3 THEN npc(o).xgo = 20
       IF temp = 1 THEN npc(o).xgo = -20
      END IF
     END IF '---CHASE/FLEE
     'WALK IN PLACE---
     IF movetype = 8 THEN
      npc(o).frame = loopvar(npc(o).frame, 0, 3, 1) 
     END IF '---WALK IN PLACE
    END IF
   END IF
  END IF
  IF npc(o).xgo <> 0 OR npc(o).ygo <> 0 THEN GOSUB movenpcgo
 END IF
NEXT o
RETRACE

movenpcgo:
setmapdata pass(), pass(), 0, 0
npc(o).frame = loopvar(npc(o).frame, 0, 3, 1)
IF movdivis(npc(o).xgo) OR movdivis(npc(o).ygo) THEN
 IF readbit(gen(), 44, suspendnpcwalls) = 0 THEN
  '--this only happens if NPC walls on
  IF wrappass(npc(o).x \ 20, npc(o).y \ 20, npc(o).xgo, npc(o).ygo, 0) THEN
   npc(o).xgo = 0
   npc(o).ygo = 0
   GOSUB hitwall
   GOTO nogo
  END IF
 END IF
 IF readbit(gen(), 44, suspendobstruction) = 0 THEN
  '--this only happens if obstruction is on
  FOR i = 0 TO 299
   IF npc(i).id > 0 AND o <> i THEN
    IF wrapcollision (npc(i).x, npc(i).y, npc(i).xgo, npc(i).ygo, npc(o).x, npc(o).y, npc(o).xgo, npc(o).ygo) THEN
     npc(o).xgo = 0
     npc(o).ygo = 0
     GOSUB hitwall
     GOTO nogo
    END IF
   END IF
  NEXT i
  '---CHECK THAT NPC IS OBSTRUCTABLE-----
  IF npc(o).id > 0 THEN
   IF npcs((npc(o).id - 1) * 15 + 8) < 2 THEN
    IF wrapcollision (npc(o).x, npc(o).y, npc(o).xgo, npc(o).ygo, catx(0), caty(0), xgo(0), ygo(0)) THEN
     npc(o).xgo = 0
     npc(o).ygo = 0
     '--a 0-3 tick delay before pacing enemies bounce off hero
     IF npc(o).frame = 3 THEN
      GOSUB hitwall
      GOTO nogo
     END IF
    END IF
   END IF
  END IF
 END IF
END IF
IF npcs(id * 15 + 3) THEN
 '--change x,y and decrement wantgo by speed
 IF npc(o).xgo > 0 THEN npc(o).xgo = npc(o).xgo - npcs(id * 15 + 3): npc(o).x = npc(o).x - npcs(id * 15 + 3)
 IF npc(o).xgo < 0 THEN npc(o).xgo = npc(o).xgo + npcs(id * 15 + 3): npc(o).x = npc(o).x + npcs(id * 15 + 3)
 IF npc(o).ygo > 0 THEN npc(o).ygo = npc(o).ygo - npcs(id * 15 + 3): npc(o).y = npc(o).y - npcs(id * 15 + 3)
 IF npc(o).ygo < 0 THEN npc(o).ygo = npc(o).ygo + npcs(id * 15 + 3): npc(o).y = npc(o).y + npcs(id * 15 + 3)
ELSE
 '--no speed, kill wantgo
 npc(o).xgo = 0
 npc(o).ygo = 0
END IF
IF cropmovement(npc(o).x, npc(o).y, npc(o).xgo, npc(o).ygo) THEN GOSUB hitwall
nogo:
IF npcs(id * 15 + 8) = 1 AND showsay = 0 THEN
 IF wraptouch(npc(o).x, npc(o).y, catx(0), caty(0), 20) THEN
  ux = npc(o).x
  uy = npc(o).y
  auto = 1
  sayer = o
  GOSUB usething
 END IF
END IF
RETRACE

hitwall:
IF npcs(id * 15 + 2) = 2 THEN npc(o).dir = loopvar(npc(o).dir, 0, 3, 2)
IF npcs(id * 15 + 2) = 3 THEN npc(o).dir = loopvar(npc(o).dir, 0, 3, 1)
IF npcs(id * 15 + 2) = 4 THEN npc(o).dir = loopvar(npc(o).dir, 0, 3, -1)
IF npcs(id * 15 + 2) = 5 THEN npc(o).dir = INT(RND * 4)
RETRACE

opendoor:
IF veh(0) AND readbit(veh(), 9, 3) = 0 AND dforce = 0 THEN RETRACE
FOR doori = 0 TO 99
 IF readbit(door(), 200, doori) THEN
  IF (door(doori) = INT(catx(0) / 20) AND door(doori + 100) = INT(caty(0) / 20) + 1) OR dforce - 1 = doori THEN
   dforce = 0
   GOSUB thrudoor
   EXIT FOR
  END IF
 END IF
NEXT doori
RETRACE

thrudoor:
samemap = 0
oldmap = map
'--load link data into buffer() -- Take care not to clobber it!
xbload maplumpname$(map, "d"), buffer(), "Oh no! Map" + STR$(map) + " doorlinks missing"
FOR o = 0 TO 199
 IF doori = buffer(o) THEN
  'PLOT CHECKING FOR DOORS
  bad = 1
  IF istag(buffer(o + 800), -1) AND istag(buffer(o + 600), -1) THEN bad = 0
  IF bad = 0 THEN
   map = buffer(o + 400)
   destdoor = buffer(o + 200)
   '--buffer() gets clobbered here, but thats okay because we are done with it
   loaddoor map, door()
   catx(0) = door(destdoor) * 20
   caty(0) = (door(destdoor + 100) - 1) * 20
   fadeout 0, 0, 0, 0
   needf = 2
   ERASE scroll, pass
   afterbat = 0
   IF oldmap = map THEN samemap = -1
   GOSUB preparemap
   foep = range(100, 60)
   EXIT FOR
  END IF
 END IF
NEXT o
RETRACE

preparemap:
'DEBUG debug "in preparemap"
'--[!] here I should only DIM what is needed, chu ne?
DIM scroll(16002), pass(16002)
setpicstuf gmap(), 40, -1
loadset game$ + ".map", map, 0
getmapname mapname$, map
loadtanim gmap(0), tastuf()
FOR i = 0 TO 1
 cycle(i) = 0
 cycptr(i) = 0
 cycskip(i) = 0
NEXT i
xbloadmap maplumpname$(map, "t"), scroll(), "Oh no! Map" + STR$(map) + " tilemap is missing"
xbloadmap maplumpname$(map, "p"), pass(), "Oh no! Map" + STR$(map) + " passabilitymap is missing"
IF isfile(maplumpname$(map, "e")) THEN
 CLOSE #foemaph
 foemaph = FREEFILE
 OPEN maplumpname$(map, "e") FOR BINARY AS #foemaph
ELSE
 fatalerror "Oh no! Map" + STR$(map) + " foemap is missing"
END IF
loaddoor map, door()
IF afterbat = 0 THEN
 showmapname = gmap(4)
 'xbload maplumpname$(map, "l"), npcl(), "Oh no! Map" + STR$(map) + " NPC locations are missing"
 LoadNPCL maplumpname$(map, "l"), npc(), 300
 xbload maplumpname$(map, "n"), npcs(), "Oh no! Map" + STR$(map) + " NPC definitions are missing"
 FOR i = 0 TO 299
  npc(i).x = npc(i).x * 20        
  npc(i).y = (npc(i).y - 1) * 20 
  npc(i).xgo = 0                  
  npc(i).ygo = 0                  
 NEXT
END IF
npcplot
IF afterbat = 0 AND samemap = 0 THEN
 forcedismount choosep, say, sayer, showsay, say$(), saytag(), choose$(), chtag(), saybit(), sayenh(), catd(), foep
END IF
IF afterbat = 0 AND afterload = 0 THEN
 FOR i = 0 TO 15
  catx(i) = catx(0)
  caty(i) = caty(0)
  catd(i) = catd(0)
  catz(i) = 0
 NEXT i
END IF
IF afterload THEN
 interpolatecat
 xgo(0) = 0
 ygo(0) = 0
 herospeed(0) = 4
END IF
IF veh(0) AND samemap THEN
 FOR i = 0 TO 3
  catz(i) = veh(21)
 NEXT i
 herospeed(0) = veh(8)
 IF herospeed(0) = 3 THEN herospeed(0) = 10
END IF
reloadnpc stat()
FOR i = 0 TO 35
 IF npcs(i * 15 + 3) = 3 THEN npcs(i * 15 + 3) = 10
NEXT i
correctbackdrop
SELECT CASE gmap(5) '--outer edge wrapping
 CASE 0, 1'--crop edges or wrap
  setoutside -1
 CASE 2
  setoutside gmap(6)
END SELECT
sayer = -1
IF readbit(gen(), 44, suspendambientmusic) = 0 THEN
 IF gmap(1) = 0 THEN
  stopsong
 ELSE
  wrappedsong gmap(1) - 1
 END IF
END IF
evalherotag stat()
evalitemtag
IF afterbat = 0 THEN
 IF gmap(7) > 0 THEN
  rsr = runscript(gmap(7), nowscript + 1, -1, "map")
  IF rsr = 1 THEN
   setScriptArg 0, gmap(8)
  END IF
 END IF
ELSE
 IF gmap(12) > 0 THEN
  rsr = runscript(gmap(12), nowscript + 1, -1, "afterbattle")
  IF rsr = 1 THEN
   '--afterbattle script gets one arg telling if you won or ran
   setScriptArg 0, wonbattle
  END IF
 END IF
END IF
afterbat = 0
samemap = 0
afterload = 0
'DEBUG debug "end of preparemap"
RETRACE

resetg:
IF autorungame THEN exitprogram (NOT abortg)
fademusic 0
fadeout 0, 0, 0, -1
closemusic
closesound
'closefile
setfmvol fmvol
restoremode
RETRIEVESTATE
GOTO thestart

tempDirErr:
templockexplain
ON ERROR GOTO 0
SYSTEM

modeXerr:
'--get back to text mode
restoremode
quitcleanup
crashexplain
'--crash and print the error
PRINT "Error code"; ERR
ON ERROR GOTO 0
exitprogram 0

'--this is what we have dimed for scripts
'--script(4096), heap(2048), global(1024), astack(1024), scrat(128, 14), nowscript
interpret:
IF scrwatch THEN scriptwatcher vpage
IF nowscript >= 0 THEN
 SELECT CASE scrat(nowscript, scrstate)
  CASE IS < stnone
   scripterr "illegally suspended script"
   scrat(nowscript, scrstate) = ABS(scrat(nowscript, scrstate))
  CASE stnone
   scripterr "script" + XSTR$(nowscript) + " became stateless"
  CASE stwait
   '--evaluate wait conditions
   SELECT CASE scrat(nowscript, curvalue)
    CASE 15, 16, 35, 61'--use door, teleport to map, use NPC, want battle
     scrat(nowscript, scrstate) = streturn
    CASE 1'--wait number of ticks
     scrat(nowscript, curwaitarg) = scrat(nowscript, curwaitarg) - 1
     IF scrat(nowscript, curwaitarg) < 1 THEN
      scrat(nowscript, scrstate) = streturn
     END IF
    CASE 2'--wait for all
     n = 0
     FOR i = 0 TO 3
      IF xgo(i) <> 0 OR ygo(i) <> 0 THEN n = 1
     NEXT i
     IF readbit(gen(), 44, suspendnpcs) = 1 THEN
      FOR i = 0 TO 299
       IF npc(i).xgo <> 0 OR npc(i).ygo <> 0 THEN n = 1
       EXIT FOR
      NEXT i
     END IF
     IF gen(cameramode) = pancam OR gen(cameramode) = focuscam THEN n = 1
     IF n = 0 THEN
      scrat(nowscript, scrstate) = streturn
     END IF
    CASE 3'--wait for hero
     IF scrat(nowscript, curwaitarg) < 0 OR scrat(nowscript, curwaitarg) > 3 THEN
      scripterr "waiting for nonexistant hero" + XSTR$(scrat(nowscript, curwaitarg))
      scrat(nowscript, scrstate) = streturn
     ELSE
      IF xgo(scrat(nowscript, curwaitarg)) = 0 AND ygo(scrat(nowscript, curwaitarg)) = 0 THEN
       scrat(nowscript, scrstate) = streturn
      END IF
     END IF
    CASE 4'--wait for NPC
     npcref = getnpcref(scrat(nowscript, curwaitarg), 0)
     IF npcref >= 0 THEN
      IF npc(npcref).xgo = 0 AND npc(npcref).ygo = 0 THEN
       scrat(nowscript, scrstate) = streturn
      END IF
     ELSE
      '--no reference found, why wait for a non-existant npc?
      scrat(nowscript, scrstate) = streturn
     END IF
    CASE 9'--wait for key
     IF scrat(nowscript, curwaitarg) >= 0 AND scrat(nowscript, curwaitarg) <= 5 THEN
      IF carray(scrat(nowscript, curwaitarg)) > 1 THEN
       scrat(nowscript, scrstate) = streturn
      END IF
     ELSE
      FOR i = 0 TO 5
       IF carray(i) > 1 THEN
        scriptret = csetup(i)
        scrat(nowscript, scrstate) = streturn
       END IF
      NEXT i
      FOR i = 1 TO 127
       IF keyval(i) > 1 THEN
        scriptret = i
        scrat(nowscript, scrstate) = streturn
       END IF
      NEXT i
     END IF
    CASE 244'--wait for scancode
     IF keyval(scrat(nowscript, curwaitarg)) > 1 THEN
      scrat(nowscript, scrstate) = streturn
     END IF
    CASE 42'--wait for camera
     IF gen(cameramode) <> pancam AND gen(cameramode) <> focuscam THEN scrat(nowscript, scrstate) = streturn
    CASE 59'--wait for text box
     IF showsay = 0 OR readbit(gen(), 44, suspendboxadvance) = 1 THEN
      scrat(nowscript, scrstate) = streturn
     END IF
    CASE 73, 234'--game over, quit from loadmenu
    CASE ELSE
     scripterr "illegal wait substate" + XSTR$(scrat(nowscript, curvalue))
     scrat(nowscript, scrstate) = streturn
   END SELECT
   IF scrat(nowscript, scrstate) = streturn THEN
    '--this allows us to resume the script without losing a game cycle
    wantimmediate = -1
   END IF
  CASE ELSE
   '--interpret script
   GOSUB interpretloop
 END SELECT
 IF wantimmediate = -1 THEN
  '--wow! I hope this doesnt screw things up!
  wantimmediate = 0
  GOTO interpret
 END IF
END IF
'--do spawned text boxes, battles, etc.
IF wantbox > 0 THEN
 say = wantbox
 loadsay choosep, say, sayer, showsay, say$(), saytag(), choose$(), chtag(), saybit(), sayenh()
 wantbox = 0
END IF
IF wantdoor > 0 THEN
 dforce = wantdoor
 wantdoor = 0
 GOSUB opendoor
 IF needf = 0 THEN
  temp = readfoemap(INT(catx(0) / 20), INT(caty(0) / 20), scroll(0), scroll(1), foemaph)
  IF veh(0) AND veh(11) > 0 THEN temp = veh(11)
  IF temp > 0 THEN foep = large(foep - foef(temp - 1), 0)
  setmapdata scroll(), pass(), 0, 0
 END IF
 setmapxy
END IF
IF wantbattle > 0 THEN
 fatal = 0
 ERASE scroll, pass
 wonbattle = battle(wantbattle - 1, fatal, stat())
 scriptret = wonbattle
 wantbattle = 0
 afterbat = 1
 GOSUB preparemap
 foep = range(100, 60)
 needf = 3
 setkeys
END IF
IF wantteleport > 0 THEN
 wantteleport = 0
 ERASE scroll, pass
 afterbat = 0
 GOSUB preparemap
 foep = range(100, 60)
END IF
IF wantusenpc > 0 THEN
 sayer = wantusenpc - 1
 wantusenpc = 0
 auto = 2
 GOSUB usething
END IF
RETRACE

interpretloop:
DO
 'scriptdump "interpretloop"
 IF scrwatch = 2 THEN
  scriptwatcher vpage
  IF keyval(1) > 1 THEN scrwatch = 0
 END IF
 SELECT CASE scrat(nowscript, scrstate)
  CASE stwait'---begin waiting for something
   EXIT DO
  CASE stdoarg'---do argument
   subdoarg
  CASE stread'---read statement
   '--FIRST STATE
   '--sets stnext for a function, or streturn for others
   IF functionread THEN EXIT DO
  CASE streturn'---return
   '--sets stdone if done with entire script, stnext otherwise
   subreturn
  CASE stnext'---check if all args are done
   IF scrat(nowscript, curargn) >= scrat(nowscript, curargc) THEN
    '--pop return values of each arg
    '--evaluate function, math, script, whatever
    '--scriptret would be set here, pushed at return
    SELECT CASE scrat(nowscript, curkind)
     CASE tystop
      scripterr "stnext encountered noop" + XSTR$(scrat(nowscript, curvalue)) + " at" + XSTR$(scrat(nowscript, scrptr)) + " in" + XSTR$(nowscript): nowscript = -1: EXIT DO
     CASE tymath, tyfunct
      '--complete math and functions, nice and easy.
      FOR i = scrat(nowscript, curargc) - 1 TO 0 STEP -1
       retvals(i) = popw
      NEXT i
      GOSUB sfunctions
      '--unless you have switched to wait mode, return
      IF scrat(nowscript, scrstate) = stnext THEN scrat(nowscript, scrstate) = streturn'---return
     CASE tyflow
      '--finish flow control? tricky!
      SELECT CASE scrat(nowscript, curvalue)
       CASE flowwhile'--repeat or terminate while
	SELECT CASE scrat(nowscript, curargn)
	 CASE 2
	  '--if a while statement finishes normally (argn is 2) then it repeats.
	  dummy = popw
          dummy = popw
	  scrat(nowscript, curargn) = 0
	 CASE ELSE
	  scripterr "while fell out of bounds, landed on" + XSTR$(scrat(nowscript, curargn)): nowscript = -1: EXIT DO
	END SELECT
       CASE flowfor'--repeat or terminate for
	SELECT CASE scrat(nowscript, curargn)
	 CASE 5
	  '--normal for termination means repeat
	  dummy = popw
	  GOSUB incrementflow
	  scrat(nowscript, curargn) = 4
	 CASE ELSE
	  scripterr "for fell out of bounds, landed on" + XSTR$(scrat(nowscript, curargn)): nowscript = -1: EXIT DO
	END SELECT
       CASE flowreturn
	scrat(nowscript, scrret) = popw
	scrat(nowscript, scrstate) = streturn'---return
       CASE flowbreak
        r = popw
        unwindtodo(r)
        '--for and while need to be broken
        IF scrat(nowscript, curkind) = tyflow AND (scrat(nowscript, curvalue) = flowfor OR scrat(nowscript, curvalue) = flowwhile) THEN
         GOSUB dumpandreturn
        END IF
       CASE flowcontinue
        r = popw
        unwindtodo(r)
        IF scrat(nowscript, curkind) = tyflow AND scrat(nowscript, curvalue) = flowswitch THEN
         '--set state to 2
         dummy = popw
         dummy = popw
         pushw 2
         pushw 9999
        ELSEIF NOT (scrat(nowscript, curkind) = tyflow AND (scrat(nowscript, curvalue) = flowfor OR scrat(nowscript, curvalue) = flowwhile)) THEN
         '--if this do isn't a for's or while's, then just repeat it, discarding the returned value
         dummy = popw
         scrat(nowscript, curargn) = scrat(nowscript, curargn) - 1
        END IF
       CASE flowexit
        unwindtodo(9999)
       CASE flowexitreturn
        scrat(nowscript, scrret) = popw
        unwindtodo(9999)
       CASE flowswitch
        tmpcase = popw
        tmpstate = popw
        tmpvar = popw
        scriptret = 0
        scrat(nowscript, scrstate) = streturn
       CASE ELSE
	'--do, then, etc... terminate normally
	scriptret = -1
	GOSUB dumpandreturn
      END SELECT
      'scrat(nowscript, scrstate) = streturn'---return
     CASE tyscript
      rsr = runscript(scrat(nowscript, curvalue), nowscript + 1, 0, "indirect")
      IF rsr = 1 THEN
       '--fill heap with return values
       FOR i = scrat(nowscript - 1, curargc) - 1 TO 0 STEP -1
	setScriptArg i, popw
       NEXT i
      END IF
      IF rsr = 0 THEN
       scrat(nowscript, scrstate) = streturn'---return
      END IF
     CASE ELSE
      scripterr "illegal kind" + XSTR$(scrat(nowscript, curkind)) + XSTR$(scrat(nowscript, curvalue)) + " in stnext": nowscript = -1: EXIT DO
    END SELECT
   ELSE
    '--flow control is special, for all else, do next arg
    SELECT CASE scrat(nowscript, curkind)
     CASE tyflow
      SELECT CASE scrat(nowscript, curvalue)
       CASE flowif'--we got an if!
	SELECT CASE scrat(nowscript, curargn)
	 CASE 0
	  scrat(nowscript, scrstate) = stdoarg'---call conditional
	 CASE 1
	  r = popw
	  pushw r
	  IF r THEN
	   scrat(nowscript, scrstate) = stdoarg'---call then block
	  ELSE
	   scrat(nowscript, curargn) = 2
	   '--if-else needs one extra thing on the stack to account for the then that didnt get used.
	   pushw 0
	   scrat(nowscript, scrstate) = stdoarg'---call else block
	  END IF
	 CASE 2
          '--finished then but not at end of argument list: skip else
          GOSUB dumpandreturn
	 CASE ELSE
	  scripterr "if statement overstepped bounds"
	END SELECT
       CASE flowwhile'--we got a while!
	SELECT CASE scrat(nowscript, curargn)
	 CASE 0
	  scrat(nowscript, scrstate) = stdoarg'---call condition
	 CASE 1
	  r = popw
	  IF r THEN
	   scrat(nowscript, scrstate) = stdoarg'---call do block
           '--number of words on stack should equal argn (for simplicity when unwinding stack)
           pushw 0
	  ELSE
           '--break while: no args to pop
	   scriptret = 0
           scrat(nowscript, scrstate) = streturn'---return
	  END IF
	 CASE ELSE
	  scripterr "while statement has jumped the curb"
	END SELECT
       CASE flowfor'--we got a for!
	SELECT CASE scrat(nowscript, curargn)
	 '--argn 0 is var
	 '--argn 1 is start
	 '--argn 2 is end
	 '--argn 3 is step
	 '--argn 4 is do block
	 '--argn 5 is repeat (normal termination)
	 CASE 0, 1, 3
	  '--get var, start, and later step
	  scrat(nowscript, scrstate) = stdoarg
	 CASE 2
	  '--set variable to start val before getting end
	  tmpstart = popw
	  tmpvar = popw
	  pushw tmpvar
	  pushw tmpstart

	  '--update for counter
	  writescriptvar tmpvar, tmpstart

	  '---now get end value
	  scrat(nowscript, scrstate) = stdoarg
	 CASE 4
	  tmpstep = popw
	  tmpend = popw
	  tmpstart = popw
	  tmpvar = popw
	  tmpnow = readscriptvar(tmpvar)
	  IF (tmpnow > tmpend AND tmpstep > 0) OR (tmpnow < tmpend AND tmpstep < 0) THEN
           '--breakout
	   scriptret = 0
           scrat(nowscript, scrstate) = streturn'---return
	  ELSE
	   pushw tmpvar
	   pushw tmpstart
	   pushw tmpend
	   pushw tmpstep
	   scrat(nowscript, scrstate) = stdoarg'---execute the do block
	  END IF
	 CASE ELSE
	  scripterr "for statement is being difficult"
	END SELECT
       CASE flowswitch
        IF scrat(nowscript, curargn) = 0 THEN
         '--get expression to match
         scrat(nowscript, scrstate) = stdoarg
        ELSEIF scrat(nowscript, curargn) = 1 THEN
         '--set up state - push a 0: not fallen in
         '--assume first statement is a case, run it
         pushw 0
         scrat(nowscript, scrstate) = stdoarg
        ELSE
         tmpcase = popw
         tmpstate = popw
         doseek = 0 ' whether or not to search argument list for something to execute
         IF tmpstate = 0 THEN
          '--not fallen in
          tmpvar = popw
          IF tmpcase = tmpvar THEN
           tmpstate = 1
          END IF
          pushw tmpvar
          doseek = 1 '---search for a case
         ELSEIF tmpstate = 2 THEN
          '--continue encountered, fall back in
          tmpstate = 1
          doseek = 1
         ELSE
          '--after successfully running a do block, pop off matching value and exit
          tmpvar = popw
          scriptret = 0
          scrat(nowscript, scrstate) = streturn'---return
         END IF

         WHILE doseek
          tmpkind = script(scrat(nowscript, scroff) + script(scrat(nowscript, scroff) + scrat(nowscript, scrptr) + 3 + scrat(nowscript, curargn)))

          IF (tmpstate = 1 AND tmpkind = tyflow) OR (tmpstate = 0 AND (tmpkind <> tyflow OR scrat(nowscript, curargn) = scrat(nowscript, curargc) - 1)) THEN
           '--fall into a do, execute a case, or run default (last arg)
           scrat(nowscript, scrstate) = stdoarg
           pushw tmpstate
           EXIT WHILE
          END IF
          IF scrat(nowscript, curargn) >= scrat(nowscript, curargc) THEN
           tmpvar = popw
           scriptret = 0
           scrat(nowscript, scrstate) = streturn'---return
           EXIT WHILE
          END IF
          scrat(nowscript, curargn) = scrat(nowscript, curargn) + 1
         WEND
        END IF
       CASE ELSE
	scrat(nowscript, scrstate) = stdoarg'---call argument
      END SELECT
     CASE ELSE
      scrat(nowscript, scrstate) = stdoarg'---call argument
    END SELECT
   END IF
  CASE stdone'---script terminates
   '--if resuming a supended script, restore its state (normally stwait)
   '--if returning a value to a calling script, set streturn
   '--if no scripts left, break the loop
   SELECT CASE functiondone
    CASE 1
     EXIT DO
    CASE 2
     wantimmediate = -1
   END SELECT
 END SELECT
LOOP
RETRACE

incrementflow:
tmpstep = popw
tmpend = popw
tmpstart = popw
tmpvar = popw
pushw tmpvar
pushw tmpstart
pushw tmpend
pushw tmpstep
writescriptvar tmpvar, readscriptvar(tmpvar) + tmpstep
RETRACE

dumpandreturn:
FOR i = scrat(nowscript, curargn) - 1 TO 0 STEP -1
 dummy = popw
NEXT i
scriptret = 0
scrat(nowscript, scrstate) = streturn'---return
RETRACE

'---DO THE ACTUAL EFFECTS OF MATH AND FUNCTIONS----
sfunctions:
scriptret = 0
SELECT CASE scrat(nowscript, curkind)
 '---MATH----------------------------------------------------------------------
 CASE tymath
  scriptmath
  '---FUNCTIONS-----------------------------------------------------------------
 CASE tyfunct
  'the only commands that belong at the top level are the ones that need
  'access to main-module top-level global variables or GOSUBs
  SELECT CASE scrat(nowscript, curvalue)
   CASE 11'--Show Text Box (box)
    wantbox = retvals(0)
   CASE 15'--use door
    wantdoor = retvals(0) + 1
    scrat(nowscript, curwaitarg) = 0
    scrat(nowscript, scrstate) = stwait
   CASE 16'--fight formation
    IF retvals(0) <= gen(37) THEN
     wantbattle = retvals(0) + 1
     scrat(nowscript, curwaitarg) = 0
     scrat(nowscript, scrstate) = stwait
    ELSE
     scriptret = -1
    END IF
   CASE 23'--unequip
    IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
     i = retvals(0)
     unequip i, bound(retvals(1) - 1, 0, 4), stat(i, 0, 16), stat(), 1
    END IF
   CASE 24'--force equip
    IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
     i = retvals(0)
     unequip i, bound(retvals(1) - 1, 0, 4), stat(i, 0, 16), stat(), 0
     doequip bound(retvals(2), 0, 255) + 1, i, bound(retvals(1) - 1, 0, 4), stat(i, 0, 16), stat()
    END IF
   CASE 32'--show backdrop
    gen(50) = bound(retvals(0) + 1, 0, gen(100))
    correctbackdrop
   CASE 33'--show map
    gen(50) = 0
    correctbackdrop
   CASE 34'--dismount vehicle
    forcedismount choosep, say, sayer, showsay, say$(), saytag(), choose$(), chtag(), saybit(), sayenh(), catd(), foep
   CASE 35'--use NPC
    npcref = getnpcref(retvals(0), 0)
    IF npcref >= 0 THEN
     wantusenpc = npcref + 1
     scrat(nowscript, curwaitarg) = 0
     scrat(nowscript, scrstate) = stwait
    END IF
   CASE 37'--use shop
    IF retvals(0) >= 0 THEN
     shop retvals(0), needf, stock(), stat(), map, foep, mx, my, tastuf()
     reloadnpc stat()
     vishero stat()
     loadpage game$ + ".til", gmap(0), 3
    END IF
   CASE 55'--get default weapon
    IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
     scriptret = stat(retvals(0), 0, 16) - 1
    ELSE
     scriptret = 0
    END IF
   CASE 56'--set default weapon
    IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
     '--identify new default weapon
     newdfw = bound(retvals(1), 0, 255) + 1
     '--remember old default weapon
     olddfw = stat(retvals(0), 0, 16)
     '--remeber currently equipped weapon
     cureqw = eqstuf(retvals(0), 0)
     '--change default
     stat(retvals(0), 0, 16) = newdfw
     '--blank weapon
     unequip retvals(0), 0, olddfw, stat(), 0
     IF cureqw <> olddfw THEN
      '--if previously using a weapon, re-equip old weapon
      doequip cureqw, retvals(0), 0, newdfw, stat()
     ELSE
      '--otherwize equip new default weapon
      doequip newdfw, retvals(0), 0, newdfw, stat()
     END IF
    END IF
   CASE 61'--teleport to map
    map = bound(retvals(0), 0, gen(0))
    FOR i = 0 TO 3
     catx(i) = retvals(1) * 20
     caty(i) = retvals(2) * 20
    NEXT i
    wantteleport = 1
    scrat(nowscript, curwaitarg) = 0
    scrat(nowscript, scrstate) = stwait
   CASE 63, 169'--resume random enemies
    setbit gen(), 44, suspendrandomenemies, 0
    foep = range(100, 60)
   CASE 73'--game over
    abortg = 1
    scrat(nowscript, scrstate) = stwait
   CASE 77'--show value
    scriptout$ = STR$(retvals(0))
   CASE 78'--alter NPC
    IF retvals(1) >= 0 AND retvals(1) <= 14 THEN
     IF retvals(0) < 0 THEN retvals(0) = (npc(abs(retvals(0) + 1)).id - 1)
     IF retvals(0) >= 0 AND retvals(0) <= 35 THEN
      npcs(retvals(0) * 15 + retvals(1)) = retvals(2)
      IF retvals(1) = 0 THEN
       setpicstuf buffer(), 1600, 2
       loadset game$ + ".pt4", retvals(2), 20 + (5 * retvals(0))
      END IF
      IF retvals(1) = 1 THEN getpal16 pal16(), 4 + retvals(0), retvals(2)
     END IF
    END IF
   CASE 79'--show no value
    scriptout$ = ""
   CASE 80'--current map
    scriptret = map
   CASE 86'--advance text box
    GOSUB nextsay
   CASE 97'--read map block
    setmapdata scroll(), pass(), 0, 0
    scriptret = readmapblock(bound(retvals(0), 0, scroll(0)), bound(retvals(1), 0, scroll(1)))
   CASE 98'--write map block
    setmapdata scroll(), pass(), 0, 0
    setmapblock bound(retvals(0), 0, scroll(0)), bound(retvals(1), 0, scroll(1)), bound(retvals(2), 0, 255)
   CASE 99'--read pass block
    setmapdata pass(), pass(), 0, 0
    scriptret = readmapblock(bound(retvals(0), 0, pass(0)), bound(retvals(1), 0, pass(1)))
   CASE 100'--write pass block
    setmapdata pass(), pass(), 0, 0
    setmapblock bound(retvals(0), 0, pass(0)), bound(retvals(1), 0, pass(1)), bound(retvals(2), 0, 255)
   CASE 144'--load tileset
    IF retvals(0) >= 0 THEN
     o = retvals(0)
    ELSE
     o = gmap(0)
    END IF
    loadpage game$ + ".til", o, 3
    loadtanim o, tastuf()
    FOR i = 0 TO 1
     cycle(i) = 0
     cycptr(i) = 0
     cycskip(i) = 0
    NEXT i
   CASE 151'--show mini map
    minimap catx(0), caty(0), tastuf()
   CASE 153'--items menu
    wantbox = items(stat())
   CASE 155, 170'--save menu
    'ID 155 is a backcompat hack
    scriptret = picksave(0) + 1
    IF scriptret > 0 AND (retvals(0) OR scrat(nowscript, curvalue) = 155) THEN
     savegame scriptret - 1, map, foep, stat(), stock()
    END IF
    reloadnpc stat()
   CASE 166'--save in slot
    IF retvals(0) >= 1 AND retvals(0) <= 32 THEN
     savegame retvals(0) - 1, map, foep, stat(), stock()
    END IF
   CASE 167'--last save slot
    scriptret = lastsaveslot
   CASE 174'--loadfromslot
    IF retvals(0) >= 1 AND retvals(0) <= 32 THEN
     IF checksaveslot(retvals(0)) = 3 THEN
      wantloadgame = retvals(0)
      scrat(nowscript, scrstate) = stwait
     END IF
    END IF
   CASE 210'--show string
    IF retvals(0) >= 0 AND retvals(0) <= 31 THEN
     scriptout$ = plotstring$(retvals(0))
    END IF
   CASE 234'--load menu
    scriptret = picksave(1) + 1
    reloadnpc stat()
    IF retvals(0) THEN
     IF scriptret = -1 THEN
      abortg = 2  'don't go straight back to loadmenu!
      scrat(nowscript, scrstate) = stwait
      fadeout 0, 0, 0, 0
     ELSEIF scriptret > 0 THEN
      wantloadgame = scriptret
      scrat(nowscript, scrstate) = stwait
     END IF
    END IF
   CASE ELSE '--try all the scripts implemented in subs
    scriptnpc scrat(nowscript, curvalue)
    scriptmisc scrat(nowscript, curvalue)
    scriptadvanced scrat(nowscript, curvalue)
    scriptstat scrat(nowscript, curvalue), stat()
    '---------
  END SELECT
END SELECT
RETRACE
