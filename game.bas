'OHRRPGCE GAME - Main module
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z

#include "udts.bi"

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
DECLARE FUNCTION vehiclestuff% (disx%, disy%, foep%, vehedge%, showsay%)
DECLARE FUNCTION checkfordeath (stat())
DECLARE SUB loadsay (choosep%, say%, sayer%, showsay%, remembermusic%, say$(), saytag%(), choose$(), chtag%(), saybit%(), sayenh%())
DECLARE SUB correctbackdrop ()
DECLARE SUB unequip (who%, where%, defwep%, stat%(), resetdw%)
DECLARE FUNCTION isonscreen% (x%, y%)
DECLARE SUB readjoysettings ()
DECLARE FUNCTION settingstring% (searchee$, setting$, result$)
DECLARE SUB interpolatecat ()
DECLARE SUB setdebugpan ()
DECLARE SUB writescriptvar (BYVAL id%, BYVAL newval%)
DECLARE FUNCTION readscriptvar% (id%)
DECLARE FUNCTION gethighbyte% (n%)
DECLARE SUB wrappedsong (songnumber%)
DECLARE SUB stopsong ()
DECLARE SUB scriptmisc (id%)
DECLARE SUB scriptcam (id%)
DECLARE SUB scriptnpc (id%)
DECLARE SUB scriptstat (id%, stat%())
DECLARE FUNCTION rpad$ (s$, pad$, size%)
DECLARE FUNCTION getnpcref% (seekid%, offset%)
DECLARE SUB suspendresume (id%)
DECLARE SUB scriptwatcher (mode%, drawloop%)
DECLARE SUB breakpoint (mode%, callspot%)
DECLARE SUB onkeyscript (scriptnum%)
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
DECLARE FUNCTION movdivis% (xygo%)
DECLARE SUB scripterr (e$)
DECLARE SUB calibrate ()
DECLARE FUNCTION runscript% (n%, index%, newcall%, er$, trigger%)
DECLARE FUNCTION istag% (num%, zero%)
DECLARE SUB evalitemtag ()
DECLARE SUB evalherotag (stat%())
DECLARE SUB tagdisplay ()
DECLARE SUB rpgversion (v%)
DECLARE SUB cycletile (cycle%(), tastuf%(), pt%(), skip%())
DECLARE SUB loaddoor (map%, door() as door)
DECLARE FUNCTION findhero% (who%, f%, l%, d%)
DECLARE SUB doswap (s%, d%, stat%())
DECLARE FUNCTION howmanyh% (f%, l%)
DECLARE SUB heroswap (iAll%, stat%())
DECLARE SUB patcharray (array%(), n$, max%)
DECLARE SUB drawsay (saybit%(), sayenh%(), say$(), showsay%, choose$(), choosep%)
DECLARE SUB shop (id%, needf%, stock%(), stat%(), map%, foep%, tastuf%())
DECLARE SUB minimap (x%, y%, tastuf%())
DECLARE FUNCTION onwho% (w$, alone)
DECLARE FUNCTION useinn (inn%, price%, needf%, stat%(), holdscreen() AS UBYTE)
DECLARE SUB itstr (i%)
DECLARE SUB control ()
DECLARE FUNCTION picksave% (load%)
DECLARE SUB savegame (slot%, map%, foep%, stat%(), stock())
DECLARE SUB loadgame (slot%, map%, foep%, stat%(), stock())
DECLARE SUB equip (pt%, stat%())
DECLARE FUNCTION items% (stat%())
DECLARE SUB delitem (it%, num%)
DECLARE SUB spells (pt%, stat%())
DECLARE SUB status (pt%, stat%())
DECLARE SUB getnames (stat$())
DECLARE SUB resetlmp (slot%, lev%)
DECLARE FUNCTION battle (form%, fatal%, exstat%())
DECLARE SUB addhero (who, slot, stat(), forcelevel=-1)
DECLARE FUNCTION atlevel% (now%, a0%, a99%)
DECLARE FUNCTION range% (n%, r%)
DECLARE SUB snapshot ()
DECLARE FUNCTION checksaveslot (slot%)
DECLARE SUB defaultc ()
DECLARE SUB forcedismount (choosep, say, sayer, showsay, remembermusic, say$(), saytag(), choose$(), chtag(), saybit(), sayenh(), catd(), foep)
DECLARE SUB makebackups
DECLARE SUB setmapxy ()
DECLARE SUB drawnpcs ()
DECLARE FUNCTION wrapcollision (xa%, ya%, xgoa%, ygoa%, xb%, yb%, xgob%, ygob%)
DECLARE FUNCTION cropmovement (x%, y%, xgo%, ygo%)
DECLARE FUNCTION wraptouch (x1%, y1%, x2%, y2%, distance%)
DECLARE FUNCTION titlescr% ()
DECLARE SUB loadmap_gmap(mapnum%)
DECLARE SUB loadmap_npcl(mapnum%)
DECLARE SUB loadmap_npcd(mapnum%)
DECLARE SUB loadmap_tilemap(mapnum%)
DECLARE SUB loadmap_passmap(mapnum%)
DECLARE SUB loadmaplumps (mapnum%, loadmask%)
DECLARE SUB savemapstate_gmap(mapnum%, prefix$)
DECLARE SUB savemapstate_npcl(mapnum%, prefix$)
DECLARE SUB savemapstate_npcd(mapnum%, prefix$)
DECLARE SUB savemapstate_tilemap(mapnum%, prefix$)
DECLARE SUB savemapstate_passmap(mapnum%, prefix$)
DECLARE SUB savemapstate (mapnum%, savemask%, prefix$)
DECLARE SUB loadmapstate_gmap (mapnum%, prefix$, dontfallback% = 0)
DECLARE SUB loadmapstate_npcl (mapnum%, prefix$, dontfallback% = 0)
DECLARE SUB loadmapstate_npcd (mapnum%, prefix$, dontfallback% = 0)
DECLARE SUB loadmapstate_tilemap (mapnum%, prefix$, dontfallback% = 0)
DECLARE SUB loadmapstate_passmap (mapnum%, prefix$, dontfallback% = 0)
DECLARE SUB loadmapstate (mapnum%, loadmask%, prefix$, dontfallback% = 0)
DECLARE SUB deletemapstate (mapnum%, killmask%, prefix$)
DECLARE SUB deletetemps ()
DECLARE FUNCTION scriptstate$ (targetscript% = -1)
DECLARE Sub MenuSound(byval s as integer)
DECLARE SUB LoadGen
DECLARE SUB dotimer(byval l as integer)
DECLARE function dotimerbattle() as integer
DECLARE function dotimermenu() as integer
DECLARE sub dotimerafterbattle()
DECLARE FUNCTION loadscript% (n%)
DECLARE SUB resetinterpreter ()
DECLARE SUB killallscripts ()
DECLARE SUB reloadscript (index, updatestats = -1)
DECLARE FUNCTION count_sav(filename AS STRING) AS INTEGER
DECLARE SUB cropposition (BYREF x, BYREF y, unitsize)
DECLARE FUNCTION add_menu (record AS INTEGER, allow_duplicate AS INTEGER=NO) AS INTEGER
DECLARE SUB remove_menu (slot AS INTEGER)
DECLARE SUB bring_menu_forward (slot AS INTEGER)
DECLARE FUNCTION menus_allow_gameplay () AS INTEGER
DECLARE FUNCTION menus_allow_player () AS INTEGER
DECLARE FUNCTION allowed_to_open_main_menu () AS INTEGER
DECLARE SUB player_menu_keys (BYREF menu_text_box AS INTEGER, BYREF wantloadgame AS INTEGER, stat(), catx(), caty(), tastuf(), map, foep, stock())
DECLARE FUNCTION getdisplayname$ (default$)
DECLARE SUB check_menu_tags ()
DECLARE FUNCTION game_usemenu (state AS MenuState)
DECLARE FUNCTION bound_item(itemID AS INTEGER, cmd AS STRING) AS INTEGER
DECLARE FUNCTION bound_hero_party(who AS INTEGER, cmd AS STRING, minimum AS INTEGER=0) AS INTEGER
DECLARE FUNCTION bound_menuslot(menuslot AS INTEGER, cmd AS STRING) AS INTEGER
DECLARE FUNCTION bound_menuslot_and_mislot(menuslot AS INTEGER, mislot AS INTEGER, cmd AS STRING) AS INTEGER
DECLARE FUNCTION bound_plotstr(n AS INTEGER, cmd AS STRING) AS INTEGER
DECLARE FUNCTION find_menu_id (id AS INTEGER) AS INTEGER
DECLARE FUNCTION find_menu_handle(menu_handle) AS INTEGER
DECLARE FUNCTION find_menu_item_handle_in_menuslot (handle AS INTEGER, menuslot AS INTEGER) AS INTEGER
DECLARE FUNCTION find_menu_item_handle(handle AS INTEGER, BYREF found_in_menuslot) AS INTEGER
DECLARE FUNCTION assign_menu_item_handle (BYREF mi AS MenuDefItem) AS INTEGER
DECLARE FUNCTION assign_menu_handles (BYREF menu AS MenuDef) AS INTEGER
DECLARE FUNCTION menu_item_handle_by_slot(menuslot AS INTEGER, mislot AS INTEGER, visible_only AS INTEGER=YES) AS INTEGER
DECLARE FUNCTION find_menu_item_slot_by_string(menuslot AS INTEGER, s AS STRING, mislot AS INTEGER=0, visible_only AS INTEGER=YES) AS INTEGER

'---INCLUDE FILES---
#include "compat.bi"
#include "allmodex.bi"
#include "common.bi"
#include "gglobals.bi"
#include "const.bi"
#include "scrconst.bi"
#include "uiconst.bi"
#include "loading.bi"

DECLARE FUNCTION outside_battle_cure (atk AS INTEGER, target AS INTEGER, attacker AS INTEGER, stat() AS INTEGER, spread AS INTEGER) AS INTEGER

'DEBUG debug "started debug session "+date$+" "+time$

REMEMBERSTATE

'DEBUG debug "randomize timer"
RANDOMIZE TIMER

processcommandline

'---get temp dir---
tmpdir = aquiretempdir$

'DEBUG debug "dim (almost) everything"

'$dynamic

'Mixed global and module variables
DIM font(1024), buffer(16384), pal16(448), timing(4), music(16384)
DIM gen(360), saytag(21), tag(127), hero(40), bmenu(40, 5), spell(40, 3, 23), lmp(40, 7), foef(254), exlev(40, 1) AS LONG, names(40), veh(21)
DIM eqstuf(40, 4), stock(99, 49), choose$(1), chtag(1), saybit(0), sayenh(6), catx(15), caty(15), catz(15), catd(15), xgo(3), ygo(3), herospeed(3), wtog(3), say$(7), hmask(3), herobits(59, 3), itembits(maxMaxItems, 3)
DIM mapname$, catermask(0), nativehbits(40, 4)

dim map_draw_mode as integer = -1

'Old Menu data
DIM menu$(9), mi(9)
'New Menu Data
DIM menu_set AS MenuSet
DIM menus(0) AS MenuDef 'This is an array because it will eventually be a stack of heirarchial menus
DIM mstates(0) AS MenuState
DIM topmenu AS INTEGER = -1

dim door(99) as door, doorlinks(199) as doorlink

'shared module variables
DIM SHARED cycle(1), cycptr(1), cycskip(1), tastuf(40), stat(40, 1, 16)

'global variables
DIM scroll(), pass()
DIM master(255) as RGBcolor
DIM uilook(uiColors)
DIM inventory(inventoryMax) as InventSlot
DIM npcs(npcdMax) as NPCType
DIM npc(300) as NPCInst
DIM didgo(0 TO 3)
DIM shared mapx, mapy, vpage, dpage, fadestate, fmvol, speedcontrol, usepreunlump, lastsaveslot, abortg, foemaph, presentsong, framex, framey
DIM AS STRING tmpdir, exename, game, sourcerpg, savefile, workingdir
DIM gold AS LONG
DIM prefsdir as string
DIM timers(15) as timer
DIM fatal

DIM keyv(55, 1), csetup(20), carray(20)
DIM mouse(3)
DIM joy(14), gotj(2)

DIM backcompat_sound_slot_mode
DIM backcompat_sound_slots(7)

DIM nowscript, scriptret, scriptctr, numloadedscr, totalscrmem
DIM heap(2048), global(1024), retvals(32)
DIM scrat(128) as ScriptInst
DIM script(128) as ScriptData
DIM plotstr(31) as Plotstring
DIM scrst as Stack

'DEBUG debug "Thestart"
DO 'This is a big loop that encloses the entire program. The loop is only reached when resetting the game

'DEBUG debug "setup directories"

'---get work dir and exe name---
IF NOT isdir(tmpdir) THEN makedir tmpdir
workingdir = tmpdir + "playing.tmp"
exename = trimextension$(trimpath$(COMMAND$(0)))

'DEBUG debug "create playing.tmp"
'---If workingdir does not already exist, it must be created---
IF isdir(workingdir) THEN
 'DEBUG debug workingdir+" already exists"
 'DEBUG debug "erasing "+workingdir+"\"+ALLFILES
 cleanuptemp
ELSE
 makedir workingdir
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

FOR i = 1 TO 15
 master(i).r = SGN(i AND 4) * 168 + SGN(i AND 8) * 87
 master(i).g = SGN(i AND 2) * 168 + SGN(i AND 8) * 87
 master(i).b = SGN(i AND 1) * 168 + SGN(i AND 8) * 87
NEXT i
'get default ui colours
LoadUIColors uilook()

'DEBUG debug "load font"
getdefaultfont font()

'DEBUG debug "set mode-X"
setmodex
setwindowtitle "O.H.R.RPG.C.E"

'DEBUG debug "apply font"
setfont font()

keyboardsetup

textcolor uilook(uiText), 0
FOR i = 0 TO 31
 plotstr(i).Col = uilook(uiText)
NEXT i

'DEBUG debug "init sound"
setupmusic
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
usepreunlump = 0
FOR i = 1 TO commandlineargcount
 a$ = commandlinearg(i)

#IFNDEF __FB_LINUX__
 IF MID$(a$, 2, 1) <> ":" THEN a$ = curdir$ + SLASH + a$
#ENDIF
 IF LCASE$(RIGHT$(a$, 4)) = ".rpg" AND isfile(a$) THEN
  sourcerpg = a$
  autorungame = 1
  EXIT FOR
 ELSEIF isdir(a$) THEN 'perhaps it's an unlumped folder?
  'check for essentials
  IF isfile(a$ + SLASH + "archinym.lmp") THEN 'ok, accept it
   autorungame = 1
   usepreunlump = 1
   sourcerpg = a$
   workingdir = a$
  END IF
  EXIT FOR
 END IF
NEXT

IF autorungame = 0 THEN
 IF LCASE$(exename) <> "game" THEN
  IF isfile(exepath + SLASH + exename + ".rpg") THEN
   sourcerpg = exepath + SLASH + exename + ".rpg"
   autorungame = 1
  END IF
 END IF
END IF
IF autorungame = 0 THEN
 'DEBUG debug "browse for RPG"
 sourcerpg = browse$(7, "", "*.rpg", tmpdir, 1)
 IF sourcerpg = "" THEN exitprogram 0
 IF isdir(sourcerpg) THEN
  usepreunlump = 1
  workingdir = sourcerpg
 END IF
END IF

'-- set up prefs dir
#IFDEF __FB_LINUX__
'This is important on linux in case you are playing an rpg file installed in /usr/share/games
prefsdir = ENVIRON$("HOME") + SLASH + ".ohrrpgce" + SLASH + trimextension$(trimpath$(sourcerpg))
IF NOT isdir(prefsdir) THEN makedir prefsdir
#ELSE
'This is not used anywhere yet in the Windows version
prefsdir = ENVIRON$("APPDATA") + SLASH + "OHRRPGCE" + SLASH + trimextension$(trimpath$(sourcerpg))
#ENDIF

'--set up savegame file
savefile = trimextension$(sourcerpg) + ".sav"
#IFDEF __FB_LINUX__
IF NOT fileisreadable(savefile) THEN
 savefile = prefsdir + SLASH + trimpath$(savefile)
END IF
#ENDIF

IF autorungame = 0 THEN
 rectangle 4, 3, 312, 14, 9, vpage
 rectangle 5, 4, 310, 12, 1, vpage
END IF

edgeprint "Loading...", xstring("Loading...", 160), 6, uilook(uiText), vpage
setvispage vpage 'refresh

debug "Playing game " & trimpath$(sourcerpg) & " (" & getdisplayname$(" ") & ") " & DATE & " " & TIME

'---GAME SELECTED, PREPARING TO PLAY---
DIM lumpbuf(16383)
IF usepreunlump = 0 THEN
 unlump sourcerpg, workingdir + SLASH, lumpbuf()
END IF

dim gmap(dimbinsize(4)) 'this must be declared here, after the binsize file exists!

initgame '--set game

xbload game + ".fnt", font(), "font missing from " + game
LoadGEN
'--upgrade obsolete RPG files (if possible)
upgrade font()

makebackups 'make a few backup lumps

if isfile(game + ".hsp") then unlump game + ".hsp", tmpdir, lumpbuf()
ERASE lumpbuf

fadeout 0, 0, 0
needf = 1

rpgversion gen(genVersion)

setfont font()
setpicstuf buffer(), 50, -1
FOR i = 0 TO 254
 loadset game + ".efs", i, 0
 foef(i) = buffer(0)
NEXT i
j = 0

nowscript = -1
numloadedscr = 0
totalscrmem = 0
depth = 0
resetinterpreter
'the old stack used only inbattle
releasestack
setupstack

'beginplay
DO' This loop encloses the playable game for a specific RPG file

loadpalette master(), gen(genMasterPal)
LoadUIColors uilook(), gen(genMasterPal)

initgamedefaults
fatal = 0: abortg = 0
foep = range(100, 60)
map = gen(104)
lastmap = -1
choosep = 0
sayer = 0
remembermusic = -1
scrwatch = 0
menu_set.menufile = workingdir & SLASH & "menus.bin"
menu_set.itemfile = workingdir & SLASH & "menuitem.bin"

makebackups 'make a few backup lumps

wantbox = 0
wantdoor = 0
wantbattle = 0
wantteleport = 0
wantusenpc = 0
wantloadgame = 0
showsay = 0

temp = -1
IF readbit(gen(), genBits, 11) = 0 THEN
 IF titlescr = 0 THEN EXIT DO'resetg
 IF readbit(gen(), genBits, 12) = 0 THEN temp = picksave(1)
ELSE
 readjoysettings
 IF readbit(gen(), genBits, 12) = 0 THEN
  IF gen(genTitleMus) > 0 THEN wrappedsong gen(genTitleMus) - 1
  fademusic fmvol
  clearpage 3
  temp = picksave(2)
 END IF
END IF
'DEBUG debug "picked save slot"+XSTR$(temp)
fademusic 0
stopsong
fadeout 0, 0, 0
IF temp = -2 THEN EXIT DO 'resetg
IF temp >= 0 THEN
 GOSUB doloadgame
ELSE
 clearpage 0
 clearpage 1
 addhero 1, 0, stat()
 IF gen(41) > 0 THEN
  rsr = runscript(gen(41), nowscript + 1, -1, "newgame", plottrigger)
 END IF
END IF

GOSUB preparemap
doihavebits
evalherotag stat()
needf = 1
force_npc_check = YES
DIM menu_text_box AS INTEGER = 0

'DEBUG debug "pre-call movement"
setmapdata pass(), pass(), 0, 0
GOSUB movement
setkeys
DO
 'DEBUG debug "top of master loop"
 setwait timing(), speedcontrol
 setkeys
 readmouse mouse()  'setmouse() is optional
 tog = tog XOR 1
 'DEBUG debug "increment play timers"
 playtimer
 'DEBUG debug "read controls"
 control
 'debug "menu key handling:"
 check_menu_tags
 FOR i = 0 TO topmenu
  IF mstates(i).need_update THEN
   mstates(i).need_update = NO
   init_menu_state mstates(i), menus(i)
  END IF
 NEXT i
 player_menu_keys menu_text_box, wantloadgame, stat(), catx(), caty(), tastuf(), map, foep, stock()
 IF menu_text_box > 0 THEN
  '--player has triggered a text box from the menu--
  say = menu_text_box
  loadsay choosep, say, sayer, showsay, remembermusic, say$(), saytag(), choose$(), chtag(), saybit(), sayenh()
 END IF
 'debug "after menu key handling:"
 IF menus_allow_gameplay() THEN
 IF gmap(15) THEN onkeyscript gmap(15)
 'breakpoint : called after keypress script is run, but don't get called by wantimmediate
 IF scrwatch > 1 THEN breakpoint scrwatch, 4
 'DEBUG debug "enter script interpreter"
 GOSUB interpret
 'DEBUG debug "increment script timers"
 dotimer(0)
 'DEBUG debug "keyboard handling"
 IF carray(5) > 1 AND showsay = 0 AND needf = 0 AND readbit(gen(), 44, suspendplayer) = 0 AND veh(0) = 0 AND xgo(0) = 0 AND ygo(0) = 0 THEN
  IF allowed_to_open_main_menu() THEN
   add_menu 0
   menusound gen(genAcceptSFX)
   evalitemtag
   npcplot
  END IF
 END IF
 IF showsay = 0 AND needf = 0 AND readbit(gen(), 44, suspendplayer) = 0 AND veh(6) = 0 AND menus_allow_player() THEN
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
  tmp = vehiclestuff(pasx, pasy, foep, vehedge, showsay)
  SELECT CASE tmp
   CASE IS < 0
    rsr = runscript(ABS(tmp), nowscript + 1, -1, "vehicle", plottrigger)
   CASE 1
    add_menu 0
    menusound gen(genAcceptSFX)
    evalherotag stat()
    evalitemtag
    npcplot
   CASE IS > 1
    say = tmp - 1
    loadsay choosep, say, sayer, showsay, remembermusic, say$(), saytag(), choose$(), chtag(), saybit(), sayenh()
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
  IF keyval(62) > 1 THEN showtags = showtags XOR 1: scrwatch = 0 
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
  IF keyval(64) > 0 AND gen(cameramode) <> pancam THEN
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
  IF keyval(68) > 1 THEN scrwatch = loopvar(scrwatch, 0, 2, 1): showtags = 0
  IF keyval(87) > 1 THEN ghost = ghost XOR 1
  IF keyval(29) > 0 THEN
   IF keyval(74) > 1 THEN speedcontrol = large(speedcontrol - 1, 10): scriptout$ = XSTR$(speedcontrol)
   IF keyval(78) > 1 THEN speedcontrol = small(speedcontrol + 1, 160): scriptout$ = XSTR$(speedcontrol)
  END IF
  IF keyval(29) > 0 AND keyval(32) THEN scriptout$ = scriptstate$
  IF keyval(41) > 1 then map_draw_mode = not map_draw_mode
 END IF
 IF wantloadgame > 0 THEN
  'DEBUG debug "loading game slot" + XSTR$(wantloadgame - 1)
  temp = wantloadgame - 1
  wantloadgame = 0
  resetgame map, foep, stat(), stock(), showsay, scriptout$, sayenh()
  initgamedefaults
  fademusic 0
  stopsong
  fadeout 0, 0, 0
  needf = 1
  force_npc_check = YES
  lastmap = -1
  GOSUB doloadgame
  GOSUB preparemap
 END IF
 'DEBUG debug "random enemies"
 IF foep = 0 AND readbit(gen(), 44, suspendrandomenemies) = 0 AND (veh(0) = 0 OR veh(11) > -1) THEN
  temp = readfoemap(INT(catx(0) / 20), INT(caty(0) / 20), scroll(0), scroll(1), foemaph)
  IF veh(0) AND veh(11) > 0 THEN temp = veh(11)
  IF temp > 0 THEN
   setpicstuf buffer(), 50, -1
   loadset game + ".efs", temp - 1, 0
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
    remembermusic = presentsong
    wonbattle = battle(batform, fatal, stat())
    dotimerafterbattle
    afterbat = 1
    GOSUB preparemap
    needf = 2
   ELSE
    rsr = runscript(gmap(13), nowscript + 1, -1, "rand-battle", plottrigger)
    IF rsr = 1 THEN
     setScriptArg 0, batform
     setScriptArg 1, temp
    END IF
   END IF
   foep = range(100, 60)
  END IF
 END IF
 'DEBUG debug "check for death (fatal = " & fatal & ")"
 IF fatal = 1 THEN
  '--this is what happens when you die in battle
  showsay = 0
  IF gen(42) > 0 THEN
   rsr = runscript(gen(42), nowscript + 1, -1, "death", plottrigger)
   IF rsr = 1 THEN
    fatal = 0
    needf = 2
   END IF
  ELSE
   fadeout 255, 0, 0
  END IF
 END IF
 END IF' end menus_allow_gameplay
 GOSUB displayall
 IF fatal = 1 OR abortg > 0 THEN
  resetgame map, foep, stat(), stock(), showsay, scriptout$, sayenh()
  'if skip loadmenu and title bits set, quit
  IF (readbit(gen(), genBits, 11)) AND (readbit(gen(), genBits, 12) OR abortg = 2 OR count_sav(savefile) = 0) THEN
   EXIT DO, DO ' To game select screen (quit the gameplay and RPG file loops, allowing the program loop to cycle)
  ELSE
   EXIT DO ' To title screen (quit the gameplay loop and allow the RPG file loop to cycle)
  END IF
 END IF
 'DEBUG debug "swap video pages"
 SWAP vpage, dpage
 setvispage vpage
 'DEBUG debug "fade in"
 'DEBUG debug "needf"+XSTR$(needf)
 IF needf = 1 AND fatal = 0 THEN
  needf = 0
  fademusic fmvol
  fadein
  setkeys
 END IF
 IF needf > 1 THEN needf = needf - 1
 'DEBUG debug "tail of main loop"
 dowait
LOOP

LOOP ' This is the end of the DO that encloses a specific RPG file

'resetg
IF autorungame THEN exitprogram (NOT abortg)
resetinterpreter
cleanuptemp
fademusic 0
fadeout 0, 0, 0
closemusic
closesound
'closefile
setfmvol fmvol
restoremode
RETRIEVESTATE
LOOP ' This is the end of the DO that encloses the entire program.

doloadgame:
loadgame temp, map, foep, stat(), stock()
afterload = -1
IF gen(57) > 0 THEN
 rsr = runscript(gen(57), nowscript + 1, -1, "loadgame", plottrigger)
 IF rsr = 1 THEN
  '--pass save slot as argument
  IF temp = 32 THEN temp = -1 'quickload slot
  setScriptArg 0, temp
 END IF
END IF
samemap = -1
RETRACE

displayall:
IF gen(58) = 0 AND gen(50) = 0 THEN
 '---NORMAL DISPLAY---
 'DEBUG debug "normal display"
 setmapdata scroll(), pass(), 0, 0
 setanim tastuf(0) + cycle(0), tastuf(20) + cycle(1)
 cycletile cycle(), tastuf(), cycptr(), cycskip()
 'DEBUG debug "drawmap"
 overlay = 1
 IF readbit(gen(), 44, suspendoverlay) THEN overlay = 0
 drawmap mapx, mapy, 0, overlay, dpage, 0
 if readbit(gmap(), 19, 0) then drawmap mapx, mapy, 1, 0, dpage, 1
 'DEBUG debug "draw npcs and heroes"
 IF gmap(16) = 1 THEN
  cathero
  drawnpcs
 ELSE
  drawnpcs
  cathero
 END IF
 'DEBUG debug "drawoverhead"
 if readbit(gmap(), 19, 1) then drawmap mapx, mapy, 2, 0, dpage, 1
 IF readbit(gen(), 44, suspendoverlay) = 0 THEN drawmap mapx, mapy, 0, 2, dpage
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
FOR i = 0 TO topmenu
 draw_menu menus(i), mstates(i), dpage
NEXT i
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
IF scrwatch THEN scriptwatcher scrwatch, -1
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
     nx = npc(j).x
     ny = npc(j).y
     nd = npc(j).dir
     IF (nx = ux AND ny = uy) THEN 'not moving NPCs
      EXIT DO
     ELSEIF nx MOD 20 <> 0 XOR ny mod 20 <> 0 THEN 'they're moving (i.e. misaligned)
      '--first check the tile the NPC is stepping into
      nx -= npc(j).xgo
      ny -= npc(j).ygo
      cropposition nx, ny, 20
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
 IF auto = 0 AND npcs(npc(sayer).id - 1).activation = 2 THEN sayer = -1 : RETRACE
 getit = npcs(npc(sayer).id - 1).item
 IF getit THEN getitem getit, 1
 '---DIRECTION CHANGING-----------------------
 recalld = -1
 IF auto <> 2 AND npcs(npc(sayer).id - 1).facetype < 2 THEN
  recalld = npc(sayer).dir
  npc(sayer).dir = catd(0)
  npc(sayer).dir = loopvar(npc(sayer).dir, 0, 3, 1): npc(sayer).dir = loopvar(npc(sayer).dir, 0, 3, 1)
 END IF
 IF npcs(npc(sayer).id - 1).usetag > 0 THEN
  '--One-time-use tag
  setbit tag(), 0, 1000 + npcs(npc(sayer).id - 1).usetag, 1
 END IF
 IF npcs(npc(sayer).id - 1).script > 0 THEN
  '--summon a script directly from an NPC
  rsr = runscript(npcs(npc(sayer).id - 1).script, nowscript + 1, -1, "NPC", plottrigger)
  IF rsr = 1 THEN
   setScriptArg 0, npcs(npc(sayer).id - 1).scriptarg
   setScriptArg 1, (sayer + 1) * -1 'reference
  END IF
 END IF
 vehuse = npcs(npc(sayer).id - 1).vechicle
 IF vehuse THEN '---activate a vehicle---
  setpicstuf buffer(), 80, -1
  loadset game + ".veh", vehuse - 1, 0
  setmapdata pass(), pass(), 0, 0
  IF vehpass(buffer(19), readmapblock(catx(0) \ 20, caty(0) \ 20, 0), -1) THEN
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
 say = npcs(npc(sayer).id - 1).textbox
 SELECT CASE say
  CASE 0
   sayer = -1
  CASE IS > 0
   loadsay choosep, say, sayer, showsay, remembermusic, say$(), saytag(), choose$(), chtag(), saybit(), sayenh()
 END SELECT
 evalherotag stat()
 evalitemtag
 IF say = 0 THEN
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
 ELSEIF gmap(1) = 0 THEN
  stopsong
 ELSE
  IF remembermusic > -1 THEN wrappedsong remembermusic ELSE stopsong
 END IF
END IF
'---GAIN/LOSE CASH-----
IF istag(saytag(13), 0) THEN
 gold = gold + saytag(14)
 IF gold > 2000000000 THEN gold = 2000000000
 IF gold < 0 THEN gold = 0
END IF
'---SPAWN BATTLE--------
IF istag(saytag(5), 0) THEN
 fatal = 0
 remembermusic = presentsong
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
 IF saytag(8) > 0 THEN
  shop saytag(8) - 1, needf, stock(), stat(), map, foep, tastuf()
  reloadnpc stat()
 END IF
 inn = 0
 IF saytag(8) < 0 THEN
  DIM holdscreen(DIMSCREENPAGE) AS UBYTE
  '--Preserve background for display beneath the top-level shop menu
  copypage vpage, holdscreen()
  IF useinn(inn, ABS(saytag(8)), needf, stat(), holdscreen()) THEN
   fadeout 0, 0, 80
   needf = 1
  END IF
  ERASE holdscreen
 END IF
 IF saytag(8) <= 0 AND inn = 0 THEN
  innRestore stat()
 END IF
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
  rsr = runscript(ABS(saytag(12)), nowscript + 1, -1, "textbox", plottrigger)
 ELSE
  say = saytag(12)
  loadsay choosep, say, sayer, showsay, remembermusic, say$(), saytag(), choose$(), chtag(), saybit(), sayenh()
  RETRACE
 END IF
END IF
evalherotag stat()
evalitemtag
'---DONE EVALUATING CONDITIONALS--------
vishero stat()
npcplot
IF sayer >= 0 AND recalld <> -1 THEN
 IF npc(sayer).id > 0 THEN
  IF npcs(npc(sayer).id - 1).facetype = 1 THEN
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
'note: xgo and ygo are offset of current position from destination, eg +ve xgo means go left 
FOR whoi = 0 TO 3
 thisherotilex = INT(catx(whoi * 5) / 20)
 thisherotiley = INT(caty(whoi * 5) / 20)
 '--if if aligned in at least one direction and passibility is enabled ... and some vehicle stuff ...
 IF (movdivis(xgo(whoi)) OR movdivis(ygo(whoi))) AND ghost = 0 AND readbit(veh(), 9, 0) = 0 AND vehpass(veh(17), readmapblock(thisherotilex, thisherotiley, 0), 0) = 0 THEN
  IF readbit(gen(), 44, suspendherowalls) = 0 AND veh(6) = 0 THEN
   '--this only happens if herowalls is on
   '--wrapping passability
   wrappass thisherotilex, thisherotiley, xgo(whoi), ygo(whoi), veh(0)
  END IF
  IF readbit(gen(), 44, suspendobstruction) = 0 AND veh(6) = 0 THEN
   '--this only happens if obstruction is on
   FOR i = 0 TO 299
    IF npc(i).id > 0 THEN '---NPC EXISTS---
     IF npcs(npc(i).id - 1).activation < 2 THEN '---NPC IS AN OBSTRUCTION---
      IF wrapcollision (npc(i).x, npc(i).y, npc(i).xgo, npc(i).ygo, catx(whoi * 5), caty(whoi * 5), xgo(whoi), ygo(whoi)) THEN
       xgo(whoi) = 0: ygo(whoi) = 0
       id = (npc(i).id - 1)
       '--push the NPC
       pushtype = npcs(id).pushtype
       IF pushtype > 0 AND npc(i).xgo = 0 AND npc(i).ygo = 0 THEN
        IF catd(whoi) = 0 AND (pushtype = 1 OR pushtype = 2 OR pushtype = 4) THEN npc(i).ygo = 20
        IF catd(whoi) = 2 AND (pushtype = 1 OR pushtype = 2 OR pushtype = 6) THEN npc(i).ygo = -20
        IF catd(whoi) = 3 AND (pushtype = 1 OR pushtype = 3 OR pushtype = 7) THEN npc(i).xgo = 20
        IF catd(whoi) = 1 AND (pushtype = 1 OR pushtype = 3 OR pushtype = 5) THEN npc(i).xgo = -20
        IF readbit(gen(), genBits2, 0) = 0 THEN ' Only do this if the backcompat bitset is off
         FOR o = 0 TO 299 ' check to make sure no other NPCs are blocking this one
          IF i = o THEN CONTINUE FOR
          IF wrapcollision (npc(i).x, npc(i).y, npc(i).xgo, npc(i).ygo, npc(o).x, npc(o).y, npc(o).xgo, npc(o).ygo) THEN
           npc(i).xgo = 0
           npc(i).ygo = 0
           EXIT FOR
          END IF
         NEXT o
        END IF
       END IF
       IF npcs(id).activation = 1 AND whoi = 0 THEN
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
   p = readmapblock(catx(whoi * 5) \ 20, caty(whoi * 5) \ 20, 0)
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
 cropmovement catx(whoi * 5), caty(whoi * 5), xgo(whoi), ygo(whoi)
NEXT whoi
'--only the leader may activate NPCs
IF (xgo(0) MOD 20 = 0) AND (ygo(0) MOD 20 = 0) AND (didgo(0) = 1 OR force_npc_check = YES) THEN
 '--finished a step
 force_npc_check = NO
 IF readbit(gen(), 44, suspendobstruction) = 0 THEN
  '--this only happens if obstruction is on
  FOR i = 0 TO 299
   IF npc(i).id > 0 THEN '---NPC EXISTS---
    IF veh(0) = 0 OR (readbit(veh(), 9, 2) AND veh(5) <> i) THEN
     IF npcs(npc(i).id - 1).activation = 2 THEN '---NPC IS PASSABLE---
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
 IF didgo(0) = 1 THEN 'only check doors if the hero really moved, not just if force_npc_check = YES
  GOSUB opendoor
 END IF
 IF needf = 0 THEN
  temp = readfoemap(catx(0) \ 20, caty(0) \ 20, scroll(0), scroll(1), foemaph)
  IF veh(0) AND veh(11) > 0 THEN temp = veh(11)
  IF temp > 0 THEN foep = large(foep - foef(temp - 1), 0)
  setmapdata scroll(), pass(), 0, 0
 END IF
 IF gmap(14) > 0 THEN
  rsr = runscript(gmap(14), nowscript + 1, -1, "eachstep", plottrigger)
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
   movetype = npcs(id).movetype
   speedset = npcs(id).speed
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
   IF npcs(npc(o).id - 1).activation < 2 THEN
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
IF npcs(id).speed THEN
 '--change x,y and decrement wantgo by speed
 IF npc(o).xgo > 0 THEN npc(o).xgo = npc(o).xgo - npcs(id).speed: npc(o).x = npc(o).x - npcs(id).speed
 IF npc(o).xgo < 0 THEN npc(o).xgo = npc(o).xgo + npcs(id).speed: npc(o).x = npc(o).x + npcs(id).speed
 IF npc(o).ygo > 0 THEN npc(o).ygo = npc(o).ygo - npcs(id).speed: npc(o).y = npc(o).y - npcs(id).speed
 IF npc(o).ygo < 0 THEN npc(o).ygo = npc(o).ygo + npcs(id).speed: npc(o).y = npc(o).y + npcs(id).speed
ELSE
 '--no speed, kill wantgo
 npc(o).xgo = 0
 npc(o).ygo = 0
END IF
IF cropmovement(npc(o).x, npc(o).y, npc(o).xgo, npc(o).ygo) THEN GOSUB hitwall
nogo:
IF npcs(id).activation = 1 AND showsay = 0 THEN
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
IF npcs(id).movetype = 2 THEN npc(o).dir = loopvar(npc(o).dir, 0, 3, 2)
IF npcs(id).movetype = 3 THEN npc(o).dir = loopvar(npc(o).dir, 0, 3, 1)
IF npcs(id).movetype = 4 THEN npc(o).dir = loopvar(npc(o).dir, 0, 3, -1)
IF npcs(id).movetype = 5 THEN npc(o).dir = INT(RND * 4)
RETRACE

opendoor:
IF veh(0) AND readbit(veh(), 9, 3) = 0 AND dforce = 0 THEN RETRACE
FOR doori = 0 TO 99
 IF readbit(door(doori).bits(),0,0) THEN
  IF (door(doori).x = INT(catx(0) / 20) AND door(doori).y = INT(caty(0) / 20)+1) OR dforce - 1 = doori THEN
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
'xbload maplumpname$(map, "d"), buffer(), "Oh no! Map" + STR$(map) + " doorlinks missing"
deserdoorlinks(maplumpname(map,"d"), doorlinks())

FOR o = 0 TO 199
 with doorlinks(o)
 IF doori = .source THEN
  'PLOT CHECKING FOR DOORS
  bad = 1
  IF istag(.tag1, -1) AND istag(.tag2, -1) THEN bad = 0
  IF bad = 0 THEN
   map = .dest_map
   destdoor = .dest
   deserdoors game + ".dox", door(), map
   catx(0) = door(destdoor).x * 20
   caty(0) = (door(destdoor).y - 1) * 20
   fadeout 0, 0, 0
   needf = 2
   afterbat = 0
   IF oldmap = map THEN samemap = -1
   GOSUB preparemap
   foep = range(100, 60)
   EXIT FOR
  END IF
 END IF
 end with
NEXT o
RETRACE

preparemap:
'DEBUG debug "in preparemap"
'save data from old map
IF lastmap > -1 THEN
 IF gmap(17) = 1 THEN
  savemapstate_npcd lastmap, "map"
  savemapstate_npcl lastmap, "map"
 END IF
 IF gmap(18) = 1 THEN
  savemapstate_tilemap lastmap, "map"
  savemapstate_passmap lastmap, "map"
 END IF
END IF
lastmap = map

'load gmap
loadmapstate_gmap map, "map"

'Play map music
IF readbit(gen(), 44, suspendambientmusic) = 0 THEN
 IF gmap(1) > 0 THEN
  wrappedsong gmap(1) - 1
 ELSEIF gmap(1) = 0 THEN
  stopsong
 ELSEIF gmap(1) = -1 AND afterbat THEN
  IF remembermusic > -1 THEN wrappedsong remembermusic ELSE stopsong
 END IF
END IF

mapname$ = getmapname$(map)

IF gmap(18) < 2 THEN
 loadmapstate_tilemap map, "map"
 loadmapstate_passmap map, "map"
ELSE
 loadmap_tilemap map
 loadmap_passmap map
END IF

'--as soon as we know the dimentions of the map, enforce hero position boundaries
cropposition catx(0), caty(0), 20

IF afterbat = 0 THEN
 showmapname = gmap(4)
 IF gmap(17) < 2 THEN
  loadmapstate_npcd map, "map"
  loadmapstate_npcl map, "map"
 ELSE
  loadmap_npcd map
  loadmap_npcl map
 END IF
END IF
'load NPC graphics
reloadnpc stat()
'Evaluate whether NPCs should appear or disappear based on tags
npcplot

IF isfile(maplumpname$(map, "e")) THEN
 CLOSE #foemaph
 foemaph = FREEFILE
 OPEN maplumpname$(map, "e") FOR BINARY AS #foemaph
ELSE
 fatalerror "Oh no! Map" + STR$(map) + " foemap is missing"
END IF
loaddoor map, door()

IF afterbat = 0 AND samemap = 0 THEN
 forcedismount choosep, say, sayer, showsay, remembermusic, say$(), saytag(), choose$(), chtag(), saybit(), sayenh(), catd(), foep
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
sayer = -1

evalherotag stat()
evalitemtag
IF afterbat = 0 THEN
 IF gmap(7) > 0 THEN
  rsr = runscript(gmap(7), nowscript + 1, -1, "map", plottrigger)
  IF rsr = 1 THEN
   setScriptArg 0, gmap(8)
  END IF
 END IF
ELSE
 IF gmap(12) > 0 THEN
  rsr = runscript(gmap(12), nowscript + 1, -1, "afterbattle", plottrigger)
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

'--this is what we have dimed for scripts
'--script(4096), heap(2048), global(1024), scrat(128), nowscript
interpret:
IF nowscript >= 0 THEN
 SELECT CASE scrat(nowscript).state
  CASE IS < stnone
   scripterr "illegally suspended script"
   scrat(nowscript).state = ABS(scrat(nowscript).state)
  CASE stnone
   scripterr "script" + XSTR$(nowscript) + " became stateless"
  CASE stwait
   '--evaluate wait conditions
   SELECT CASE scrat(nowscript).curvalue
    CASE 15, 35, 61'--use door, use NPC, teleport to map
     scrat(nowscript).state = streturn
    CASE 16'--fight formation
     scriptret = wonbattle
     scrat(nowscript).state = streturn
    CASE 1'--wait number of ticks
     scrat(nowscript).waitarg = scrat(nowscript).waitarg - 1
     IF scrat(nowscript).waitarg < 1 THEN
      scrat(nowscript).state = streturn
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
      scrat(nowscript).state = streturn
     END IF
    CASE 3'--wait for hero
     IF scrat(nowscript).waitarg < 0 OR scrat(nowscript).waitarg > 3 THEN
      scripterr "waiting for nonexistant hero" + XSTR$(scrat(nowscript).waitarg)
      scrat(nowscript).state = streturn
     ELSE
      IF xgo(scrat(nowscript).waitarg) = 0 AND ygo(scrat(nowscript).waitarg) = 0 THEN
       scrat(nowscript).state = streturn
      END IF
     END IF
    CASE 4'--wait for NPC
     npcref = getnpcref(scrat(nowscript).waitarg, 0)
     IF npcref >= 0 THEN
      IF npc(npcref).xgo = 0 AND npc(npcref).ygo = 0 THEN
       scrat(nowscript).state = streturn
      END IF
     ELSE
      '--no reference found, why wait for a non-existant npc?
      scrat(nowscript).state = streturn
     END IF
    CASE 9'--wait for key
     IF scrat(nowscript).waitarg >= 0 AND scrat(nowscript).waitarg <= 5 THEN
      IF carray(scrat(nowscript).waitarg) > 1 THEN
       scrat(nowscript).state = streturn
      END IF
     ELSE
      FOR i = 0 TO 5
       IF carray(i) > 1 THEN
        scriptret = csetup(i)
        scrat(nowscript).state = streturn
       END IF
      NEXT i
      FOR i = 1 TO 127
       IF keyval(i) > 1 THEN
        scriptret = i
        scrat(nowscript).state = streturn
       END IF
      NEXT i
     END IF
    CASE 244'--wait for scancode
     IF keyval(scrat(nowscript).waitarg) > 1 THEN
      scrat(nowscript).state = streturn
     END IF
    CASE 42'--wait for camera
     IF gen(cameramode) <> pancam AND gen(cameramode) <> focuscam THEN scrat(nowscript).state = streturn
    CASE 59'--wait for text box
     IF showsay = 0 OR readbit(gen(), 44, suspendboxadvance) = 1 THEN
      scrat(nowscript).state = streturn
     END IF
    CASE 73, 234'--game over, quit from loadmenu
    CASE ELSE
     scripterr "illegal wait substate" + XSTR$(scrat(nowscript).curvalue)
     scrat(nowscript).state = streturn
   END SELECT
   IF scrat(nowscript).state = streturn THEN
    '--this allows us to resume the script without losing a game cycle
    wantimmediate = -1
   END IF
  CASE ELSE
   '--interpret script
   GOSUB interpretloop
 END SELECT
 IF wantimmediate = -2 THEN
  IF nowscript < 0 THEN
   debug "wantimmediate ended on nowscript = -1"
  ELSE
   debug "wantimmediate would have skipped wait on command " & scrat(nowscript).curvalue & " in " & scriptname$(scrat(nowscript).id) & ", state = " & scrat(nowscript).state
   debug "needf = " & needf
  END IF
  wantimmediate = 0 'change to -1 to reenable bug
 END IF
 IF wantimmediate = -1 THEN
  '--wow! I hope this doesnt screw things up!
  wantimmediate = 0
  GOTO interpret
 END IF
END IF
'--do spawned text boxes, battles, etc.
IF wantbox > 0 THEN
 say = wantbox
 loadsay choosep, say, sayer, showsay, remembermusic, say$(), saytag(), choose$(), chtag(), saybit(), sayenh()
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
 remembermusic = presentsong
 wonbattle = battle(wantbattle - 1, fatal, stat())
 wantbattle = 0
 afterbat = 1
 GOSUB preparemap
 foep = range(100, 60)
 needf = 3
 setkeys
END IF
IF wantteleport > 0 THEN
 wantteleport = 0
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
DIM tmpstate, tmpcase  'tmpstart, tmpend, tmpstep, tmpnow, tmpvar
reloadscript(nowscript)
DO
 SELECT CASE scrat(nowscript).state
  CASE stwait'---begin waiting for something
   EXIT DO
  CASE stdoarg'---do argument
   subdoarg
  CASE stread'---read statement
   '--FIRST STATE
   '--sets stnext for a function, or streturn for others
   IF functionread THEN EXIT DO
   IF scrwatch AND breakstread THEN breakpoint scrwatch, 3
  CASE streturn'---return
   '--sets stdone if done with entire script, stnext otherwise
   subreturn
  CASE stnext'---check if all args are done
   IF scrwatch AND breakstnext THEN breakpoint scrwatch, 1
   IF scrat(nowscript).curargn >= scrat(nowscript).curargc THEN
    '--pop return values of each arg
    '--evaluate function, math, script, whatever
    '--scriptret would be set here, pushed at return
    SELECT CASE scrat(nowscript).curkind
     CASE tystop
      scripterr "stnext encountered noop " & scrat(nowscript).curvalue & " at " & scrat(nowscript).ptr & " in " & nowscript
      killallscripts
      EXIT DO
     CASE tymath, tyfunct
      '--complete math and functions, nice and easy.
      FOR i = scrat(nowscript).curargc - 1 TO 0 STEP -1
       pops(scrst, retvals(i))
      NEXT i
      GOSUB sfunctions
      '--unless you have switched to wait mode, return
      IF scrat(nowscript).state = stnext THEN scrat(nowscript).state = streturn'---return
     CASE tyflow
      '--finish flow control? tricky!
      SELECT CASE scrat(nowscript).curvalue
       CASE flowwhile'--repeat or terminate while
        SELECT CASE scrat(nowscript).curargn
         CASE 2
          '--if a while statement finishes normally (argn is 2) then it repeats.
          scrst.pos -= 2
          scrat(nowscript).curargn = 0
         CASE ELSE
          scripterr "while fell out of bounds, landed on " & scrat(nowscript).curargn
          killallscripts
          EXIT DO
        END SELECT
       CASE flowfor'--repeat or terminate for
        SELECT CASE scrat(nowscript).curargn
         CASE 5
          '--normal for termination means repeat
          scrst.pos -= 1
          GOSUB incrementflow
          scrat(nowscript).curargn = 4
         CASE ELSE
          scripterr "for fell out of bounds, landed on " & scrat(nowscript).curargn
          killallscripts
          EXIT DO
        END SELECT
       CASE flowreturn
        pops(scrst, scrat(nowscript).ret)
        scrat(nowscript).state = streturn'---return
       CASE flowbreak
        pops(scrst, temp)
        unwindtodo(temp)
        '--for and while need to be broken
        IF scrat(nowscript).curkind = tyflow AND (scrat(nowscript).curvalue = flowfor OR scrat(nowscript).curvalue = flowwhile) THEN
         GOSUB dumpandreturn
        END IF
       CASE flowcontinue
        pops(scrst, temp)
        unwindtodo(temp)
        IF scrat(nowscript).curkind = tyflow AND scrat(nowscript).curvalue = flowswitch THEN
         '--set state to 2
         scrst.pos -= 2
         pushs(scrst, 2)
         pushs(scrst, 0) '-- dummy value
        ELSEIF NOT (scrat(nowscript).curkind = tyflow AND (scrat(nowscript).curvalue = flowfor OR scrat(nowscript).curvalue = flowwhile)) THEN
         '--if this do isn't a for's or while's, then just repeat it, discarding the returned value
         scrst.pos -= 1
         scrat(nowscript).curargn = scrat(nowscript).curargn - 1
        END IF
       CASE flowexit
        unwindtodo(9999)
       CASE flowexitreturn
        pops(scrst, scrat(nowscript).ret)
        unwindtodo(9999)
       CASE flowswitch
        scrst.pos -= 3
        scriptret = 0
        scrat(nowscript).state = streturn
       CASE ELSE
        '--do, then, etc... terminate normally
        scriptret = -1
        GOSUB dumpandreturn
      END SELECT
      'scrat(nowscript).state = streturn'---return
     CASE tyscript
      rsr = runscript(scrat(nowscript).curvalue, nowscript + 1, 0, "indirect", 0)
      IF rsr = 1 THEN
       '--fill heap with return values
       FOR i = scrat(nowscript - 1).curargc - 1 TO 0 STEP -1
        pops(scrst, temp)
        setScriptArg i, temp
       NEXT i
      END IF
      IF rsr = 0 THEN
       scrat(nowscript).state = streturn'---return
      END IF
     CASE ELSE
      scripterr "illegal kind " & scrat(nowscript).curkind & " " & scrat(nowscript).curvalue & " in stnext"
      killallscripts
      EXIT DO
    END SELECT
   ELSE
    IF scrat(nowscript).curargn = 0 THEN
     '--always need to execute the first argument
     scrat(nowscript).state = stdoarg
    ELSE 
     '--flow control and logical math are special, for all else, do next arg
     SELECT CASE scrat(nowscript).curkind
      CASE tymath
       SELECT CASE scrat(nowscript).curvalue
        CASE 20'--logand
         IF reads(scrst, 0) THEN
          scrat(nowscript).state = stdoarg'---call 2nd argument
         ELSE
          '--shortcut evaluate to false
          scriptret = 0
          '--pop all args
          scrst.pos -= scrat(nowscript).curargn
          scrat(nowscript).state = streturn'---return
         END IF
        CASE 21'--logor
         IF reads(scrst, 0) THEN
          '--shortcut evaluate to true
          scriptret = 1
          '--pop all args
          scrst.pos -= scrat(nowscript).curargn
          scrat(nowscript).state = streturn'---return
         ELSE
          scrat(nowscript).state = stdoarg'---call 2nd argument
         END IF
        CASE ELSE
         scrat(nowscript).state = stdoarg'---call argument
       END SELECT
      CASE tyflow
       SELECT CASE scrat(nowscript).curvalue
        CASE flowif'--we got an if!
         SELECT CASE scrat(nowscript).curargn
          CASE 0
           scrat(nowscript).state = stdoarg'---call conditional
          CASE 1
           IF reads(scrst, 0) THEN
            'scrst.pos -= 1
            scrat(nowscript).state = stdoarg'---call then block
           ELSE
            scrat(nowscript).curargn = 2
            '--if-else needs one extra thing on the stack to account for the then that didnt get used.
            pushs(scrst, 0)
            scrat(nowscript).state = stdoarg'---call else block
           END IF
          CASE 2
           '--finished then but not at end of argument list: skip else
           GOSUB dumpandreturn
          CASE ELSE
           scripterr "if statement overstepped bounds"
         END SELECT
        CASE flowwhile'--we got a while!
         SELECT CASE scrat(nowscript).curargn
          CASE 0
           scrat(nowscript).state = stdoarg'---call condition
          CASE 1
           IF reads(scrst, 0) THEN
            scrat(nowscript).state = stdoarg'---call do block
            '--don't pop: number of words on stack should equal argn (for simplicity when unwinding stack)
           ELSE
            '--break while
            scrst.pos -= 1
            scriptret = 0
            scrat(nowscript).state = streturn'---return
           END IF
          CASE ELSE
          scripterr "while statement has jumped the curb"
         END SELECT
        CASE flowfor'--we got a for!
         SELECT CASE scrat(nowscript).curargn
          '--argn 0 is var
          '--argn 1 is start
          '--argn 2 is end
          '--argn 3 is step
          '--argn 4 is do block
          '--argn 5 is repeat (normal termination)
          CASE 0, 1, 3
           '--get var, start, and later step
           scrat(nowscript).state = stdoarg
          CASE 2
           '--set variable to start val before getting end
           writescriptvar reads(scrst, -1), reads(scrst, 0)
           '---now get end value
           scrat(nowscript).state = stdoarg
          CASE 4
           IF scrwatch AND breakloopbrch THEN breakpoint scrwatch, 5
           tmpstep = reads(scrst, 0)
           tmpend = reads(scrst, -1)
           tmpstart = reads(scrst, -2)
           tmpvar = reads(scrst, -3)
           tmpnow = readscriptvar(tmpvar)
           IF (tmpnow > tmpend AND tmpstep > 0) OR (tmpnow < tmpend AND tmpstep < 0) THEN
            '--breakout
            scrst.pos -= 4
            scriptret = 0
            scrat(nowscript).state = streturn'---return
           ELSE
            scrat(nowscript).state = stdoarg'---execute the do block
           END IF
          CASE ELSE
           scripterr "for statement is being difficult"
         END SELECT
        CASE flowswitch
         IF scrat(nowscript).curargn = 0 THEN
          '--get expression to match
          scrat(nowscript).state = stdoarg
         ELSEIF scrat(nowscript).curargn = 1 THEN
          '--set up state - push a 0: not fallen in
          '--assume first statement is a case, run it
          pushs(scrst, 0)
          scrat(nowscript).state = stdoarg
         ELSE
          pops(scrst, tmpcase)
          pops(scrst, tmpstate)
          doseek = 0 ' whether or not to search argument list for something to execute
          IF tmpstate = 0 THEN
           '--not fallen in, check tmpvar
           IF tmpcase = reads(scrst, 0) THEN 
            tmpstate = 1
           END IF
           doseek = 1 '--search for a case or do
          ELSEIF tmpstate = 1 THEN
           '--after successfully running a do block, pop off matching value and exit
           scrst.pos -= 1
           scriptret = 0
           scrat(nowscript).state = streturn'---return
          ELSEIF tmpstate = 2 THEN
           '--continue encountered, fall back in
           tmpstate = 1
           doseek = 1 '--search for a do
          END IF

          DIM tmpptr as integer ptr
          IF doseek THEN tmpptr = script(scrat(nowscript).scrnum).ptr
          WHILE doseek
           tmpkind = tmpptr[tmpptr[scrat(nowscript).curargn + scrat(nowscript).ptr + 3]]

           IF (tmpstate = 1 AND tmpkind = tyflow) OR (tmpstate = 0 AND (tmpkind <> tyflow OR scrat(nowscript).curargn = scrat(nowscript).curargc - 1)) THEN
            '--fall into a do, execute a case, or run default (last arg)
            scrat(nowscript).state = stdoarg
            pushs(scrst, tmpstate)
            EXIT WHILE
           END IF
           IF scrat(nowscript).curargn >= scrat(nowscript).curargc THEN
            scrst.pos -= 1
            scriptret = 0
            scrat(nowscript).state = streturn'---return
            EXIT WHILE
           END IF
           scrat(nowscript).curargn = scrat(nowscript).curargn + 1
          WEND
         END IF
        CASE ELSE
         scrat(nowscript).state = stdoarg'---call argument
       END SELECT
      CASE ELSE
       scrat(nowscript).state = stdoarg'---call argument
     END SELECT
    END IF
   END IF
  CASE stdone'---script terminates
   '--if resuming a supended script, restore its state (normally stwait)
   '--if returning a value to a calling script, set streturn
   '--if no scripts left, break the loop
   SELECT CASE functiondone
    CASE 1
     EXIT DO
    CASE 2
     IF scrat(nowscript).state <> stwait THEN
      debug "WANTIMMEDIATE BUG"
      debug scriptname$(scrat(nowscript + 1).id) & " terminated, setting wantimmediate on " & scriptname$(scrat(nowscript).id)
      wantimmediate = -2
     ELSE
      wantimmediate = -1
     END IF
   END SELECT
   IF scrwatch AND breakstnext THEN breakpoint scrwatch, 2
 END SELECT
LOOP
RETRACE

incrementflow:
tmpvar = reads(scrst, -3)
writescriptvar tmpvar, readscriptvar(tmpvar) + reads(scrst, 0)
RETRACE

dumpandreturn:
scrst.pos -= scrat(nowscript).curargn
scriptret = 0
scrat(nowscript).state = streturn'---return
RETRACE

'---DO THE ACTUAL EFFECTS OF MATH AND FUNCTIONS----
sfunctions:
DIM menuslot AS INTEGER = 0
DIM mislot AS INTEGER = 0
scriptret = 0
SELECT CASE AS CONST scrat(nowscript).curkind
 '---MATH----------------------------------------------------------------------
 CASE tymath
  scriptmath
  '---FUNCTIONS-----------------------------------------------------------------
 CASE tyfunct
  'the only commands that belong at the top level are the ones that need
  'access to main-module top-level global variables or GOSUBs
  SELECT CASE scrat(nowscript).curvalue
   CASE 11'--Show Text Box (box)
    wantbox = retvals(0)
   CASE 15'--use door
    wantdoor = retvals(0) + 1
    scrat(nowscript).waitarg = 0
    scrat(nowscript).state = stwait
   CASE 16'--fight formation
    IF retvals(0) >= 0 AND retvals(0) <= gen(37) THEN
     wantbattle = retvals(0) + 1
     scrat(nowscript).waitarg = 0
     scrat(nowscript).state = stwait
    ELSE
     scriptret = -1
    END IF
   CASE 23'--unequip
    IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
     i = retvals(0)
     unequip i, bound(retvals(1) - 1, 0, 4), stat(i, 0, 16), stat(), 1
    END IF
    evalitemtag
   CASE 24'--force equip
    IF bound_hero_party(retvals(0), "force equip") THEN
     i = retvals(0)
     IF bound_item(retvals(2), "force equip") THEN
      unequip i, bound(retvals(1) - 1, 0, 4), stat(i, 0, 16), stat(), 0
      doequip retvals(2) + 1, i, bound(retvals(1) - 1, 0, 4), stat(i, 0, 16), stat()
     END IF
    END IF
    evalitemtag
   CASE 32'--show backdrop
    gen(50) = bound(retvals(0) + 1, 0, gen(100))
    correctbackdrop
   CASE 33'--show map
    gen(50) = 0
    correctbackdrop
   CASE 34'--dismount vehicle
    forcedismount choosep, say, sayer, showsay, remembermusic, say$(), saytag(), choose$(), chtag(), saybit(), sayenh(), catd(), foep
   CASE 35'--use NPC
    npcref = getnpcref(retvals(0), 0)
    IF npcref >= 0 THEN
     wantusenpc = npcref + 1
     scrat(nowscript).waitarg = 0
     scrat(nowscript).state = stwait
    END IF
   CASE 37'--use shop
    IF retvals(0) >= 0 AND retvals(0) <= gen(genMaxShop) THEN
     shop retvals(0), needf, stock(), stat(), map, foep, tastuf()
     reloadnpc stat()
     loadpage game + ".til", gmap(0), 3
    END IF
   CASE 55'--get default weapon
    IF retvals(0) >= 0 AND retvals(0) <= 40 THEN
     scriptret = stat(retvals(0), 0, 16) - 1
    ELSE
     scriptret = 0
    END IF
   CASE 56'--set default weapon
    IF bound_hero_party(retvals(0), "set default weapon") THEN
     IF bound_item(retvals(1), "set default weapon") THEN
      '--identify new default weapon
      newdfw = retvals(1) + 1
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
    END IF
   CASE 61'--teleport to map
    IF retvals(0) >= 0 AND retvals(0) <= gen(0) THEN
     map = retvals(0)
     FOR i = 0 TO 3
      catx(i) = retvals(1) * 20
      caty(i) = retvals(2) * 20
     NEXT i
     wantteleport = 1
     scrat(nowscript).waitarg = 0
     scrat(nowscript).state = stwait
    END IF
   CASE 63, 169'--resume random enemies
    setbit gen(), 44, suspendrandomenemies, 0
    foep = range(100, 60)
   CASE 73'--game over
    abortg = 1
    scrat(nowscript).state = stwait
   CASE 77'--show value
    scriptout$ = STR$(retvals(0))
   CASE 78'--alter NPC
    IF retvals(1) >= 0 AND retvals(1) <= 14 THEN
     IF retvals(0) < 0 AND retvals(0) >= -300 THEN retvals(0) = ABS(npc(ABS(retvals(0) + 1)).id) - 1
     IF retvals(0) >= 0 AND retvals(0) <= npcdMax THEN
      writesafe = 1
      IF retvals(1) = 0 THEN
       IF retvals(2) < 0 OR retvals(2) > gen(genMaxNPCPic) THEN
        writesafe = 0
       ELSE
        setpicstuf buffer(), 1600, 2
        loadset game + ".pt4", retvals(2), 20 + (5 * retvals(0))
        if npcs(retvals(0)).sprite then sprite_unload(@npcs(retvals(0)).sprite)
        npcs(retvals(0)).sprite = sprite_load(game + ".pt4", retvals(2), 8, 20, 20)
       END IF
      END IF
      IF retvals(1) = 1 THEN
       getpal16 pal16(), 4 + retvals(0), retvals(2), 4, npcs(retvals(0)).picture
       if npcs(retvals(0)).pal then palette16_unload(@npcs(retvals(0)).pal)
       npcs(retvals(0)).pal = palette16_load(game + ".pal", retvals(2), 4, npcs(retvals(0)).picture)
      END IF
      IF writesafe THEN SetNPCD(npcs(retvals(0)), retvals(1), retvals(2))
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
    IF scrat(nowscript).curargc = 2 THEN retvals(2) = 0
    scriptret = readmapblock(bound(retvals(0), 0, scroll(0)-1), bound(retvals(1), 0, scroll(1)-1), bound(retvals(2), 0, 2))
   CASE 98'--write map block
    IF scrat(nowscript).curargc = 3 THEN retvals(3) = 0
    setmapdata scroll(), pass(), 0, 0
    setmapblock bound(retvals(0), 0, scroll(0)-1), bound(retvals(1), 0, scroll(1)-1), bound(retvals(3),0,2), bound(retvals(2), 0, 255)
   CASE 99'--read pass block
    setmapdata scroll(), pass(), 0, 0
    scriptret = readpassblock(bound(retvals(0), 0, pass(0)-1), bound(retvals(1), 0, pass(1)-1))
   CASE 100'--write pass block
    setmapdata scroll(), pass(), 0, 0
    setpassblock bound(retvals(0), 0, pass(0)-1), bound(retvals(1), 0, pass(1)-1), bound(retvals(2), 0, 255)
   CASE 144'--load tileset
    IF retvals(0) <= gen(genMaxTile) THEN
     IF retvals(0) < 0 THEN
      retvals(0) = gmap(0)
     END IF
     loadpage game + ".til", retvals(0), 3
     loadtanim retvals(0), tastuf()
     FOR i = 0 TO 1
      cycle(i) = 0
      cycptr(i) = 0
      cycskip(i) = 0
     NEXT i
    END IF
   CASE 151'--show mini map
    minimap catx(0), caty(0), tastuf()
   CASE 153'--items menu
    wantbox = items(stat())
   CASE 155, 170'--save menu
    'ID 155 is a backcompat hack
    scriptret = picksave(0) + 1
    IF scriptret > 0 AND (retvals(0) OR scrat(nowscript).curvalue = 155) THEN
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
      scrat(nowscript).state = stwait
     END IF
    END IF
   CASE 210'--show string
    IF retvals(0) >= 0 AND retvals(0) <= 31 THEN
     scriptout$ = plotstr(retvals(0)).s
    END IF
   CASE 234'--load menu
    scriptret = picksave(1) + 1
    reloadnpc stat()
    IF retvals(0) THEN
     IF scriptret = -1 THEN
      abortg = 2  'don't go straight back to loadmenu!
      scrat(nowscript).state = stwait
      fadeout 0, 0, 0
     ELSEIF scriptret > 0 THEN
      wantloadgame = scriptret
      scrat(nowscript).state = stwait
     END IF
    END IF
   CASE 245'--save map state
    IF retvals(1) > -1 AND retvals(1) <= 31 THEN
     savemapstate retvals(1), retvals(0), "state"
    ELSEIF retvals(1) = -1 THEN
     savemapstate map, retvals(0), "map"
    END IF
   CASE 246'--load map state
    IF retvals(1) > -1 AND retvals(1) <= 31 THEN
     loadmapstate retvals(1), retvals(0), "state", -1
    ELSEIF retvals(1) = -1 THEN
     loadmapstate map, retvals(0), "map"
    END IF
   CASE 247'--reset map state
    loadmaplumps map, retvals(0)
   CASE 248'--delete map state
    deletemapstate map, retvals(0), "map"
   CASE 253'--settileanimationoffset
    IF retvals(0) = 0 OR retvals(0) = 1 THEN
     cycle(retvals(0)) = retvals(1) MOD 160
    END IF
   CASE 254'--gettileanimationoffset
    IF retvals(0) = 0 OR retvals(0) = 1 THEN
     scriptret = cycle(retvals(0))
    END IF
   CASE 255'--animationstarttile
    IF retvals(0) < 160 THEN
     scriptret = retvals(0)
    ELSEIF retvals(0) >= 160 AND retvals(0) < 256 THEN
     scriptret = tastuf(((retvals(0) - 160) \ 48) * 20) + (retvals(0) - 160) MOD 48
    END IF
   CASE 258'--checkherowall
    IF retvals(0) >= 0 AND retvals(0) <= 3 THEN
     tempxgo = 0
     tempygo = 0
     IF retvals(1) = 0 THEN tempygo = 20
     IF retvals(1) = 1 THEN tempxgo = -20
     IF retvals(1) = 2 THEN tempygo = -20
     IF retvals(1) = 3 THEN tempxgo = 20
     scriptret = wrappass(catx(retvals(0) * 5) \ 20, catx(retvals(0) * 5) \ 20, tempxgo, tempygo, 0)
    END IF
   CASE 259'--checkNPCwall
    npcref = getnpcref(retvals(0), 0)
    tempxgo = 0
    tempygo = 0
    IF retvals(1) = 0 THEN tempygo = 20
    IF retvals(1) = 1 THEN tempxgo = -20
    IF retvals(1) = 2 THEN tempygo = -20
    IF retvals(1) = 3 THEN tempxgo = 20
    scriptret = wrappass(npc(npcref).x \ 20, npc(npcref).y \ 20, tempxgo, tempygo, 0)
   CASE 267'--main menu
    add_menu 0
    evalherotag stat()
    evalitemtag
    npcplot
   CASE 274'--open menu
    IF bound_arg(retvals(0), 0, gen(genMaxMenu), "open menu", "menu ID") THEN
     scriptret = add_menu(retvals(0), (retvals(1) <> 0))
    END IF
   CASE 275'--read menu int
    menuslot = find_menu_handle(retvals(0))
    IF bound_menuslot(menuslot, "read menu int") THEN
     scriptret = read_menu_int(menus(menuslot), retvals(1))
    END IF
   CASE 276'--write menu int
    menuslot = find_menu_handle(retvals(0))
    IF bound_menuslot(menuslot, "write menu int") THEN
     write_menu_int(menus(menuslot), retvals(1), retvals(2))
    END IF
   CASE 277'--read menu item int
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF bound_menuslot_and_mislot(menuslot, mislot, "read menu item int") THEN
     WITH menus(menuslot)
      IF .items(mislot).exists THEN scriptret = read_menu_item_int(.items(mislot), retvals(1))
     END WITH
    END IF
   CASE 278'--write menu item int
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF bound_menuslot_and_mislot(menuslot, mislot, "write menu item int") THEN
     WITH menus(menuslot)
      IF .items(mislot).exists THEN write_menu_item_int(.items(mislot), retvals(1), retvals(2))
     END WITH
    END IF
   CASE 279'--create menu
    scriptret = add_menu(-1)
    menus(topmenu).allow_gameplay = YES
   CASE 280'--close menu
    menuslot = find_menu_handle(retvals(0))
    IF bound_menuslot(menuslot, "close menu") THEN
     remove_menu menuslot
    END IF
   CASE 281'--top menu
    IF topmenu >= 0 THEN
     scriptret = menus(topmenu).handle
    END IF
   CASE 282'--bring menu forward
    menuslot = find_menu_handle(retvals(0))
    IF bound_menuslot(menuslot, "bring menu forward") THEN
     bring_menu_forward menuslot
    END IF
   CASE 283'--add menu item
    menuslot = find_menu_handle(retvals(0))
    IF bound_menuslot(menuslot, "add menu item") THEN
     i = append_menu_item(menus(menuslot), "")
     IF i >= 0 THEN
      scriptret = assign_menu_item_handle(menus(menuslot).items(i))
      mstates(menuslot).need_update = YES
     ELSE
      debug "add menu item: failed. menu " & menuslot & " is full"
     END IF
    END IF
   CASE 284'--delete menu item
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF bound_menuslot_and_mislot(menuslot, mislot, "delete menu item") THEN
     WITH menus(menuslot)
      ClearMenuItem .items(mislot)
      SortMenuItems .items()
     END WITH
     mstates(menuslot).need_update = YES
    END IF
   CASE 285'--get menu item caption
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF bound_menuslot_and_mislot(menuslot, mislot, "get menu item caption") THEN
     IF bound_plotstr(retvals(1), "get menu item caption") THEN
      plotstr(retvals(1)).s = get_menu_item_caption(menus(menuslot).items(mislot), menus(menuslot))
     END IF
    END IF
   CASE 286'--set menu item caption
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF bound_menuslot_and_mislot(menuslot, mislot, "set menu item caption") THEN
     IF bound_plotstr(retvals(1), "get menu item caption") THEN
      menus(menuslot).items(mislot).caption = plotstr(retvals(1)).s
     END IF
    END IF
   CASE 287'--get level mp
    IF bound_hero_party(retvals(0), "get level mp") THEN
     IF bound_arg(retvals(1), 0, 7, "get level mp", "mp level") THEN
      scriptret = lmp(retvals(0), retvals(1))
     END IF
    END IF
   CASE 288'--set level mp
    IF bound_hero_party(retvals(0), "set level mp") THEN
     IF bound_arg(retvals(1), 0, 7, "set level mp", "mp level") THEN
      lmp(retvals(0), retvals(1)) = retvals(2)
     END IF
    END IF
   CASE 289'--bottom menu
    IF topmenu >= 0 THEN
     scriptret = menus(0).handle
    END IF
   CASE 290'--previous menu
    menuslot = find_menu_handle(retvals(0))
    IF bound_menuslot(menuslot, "previous menu") THEN
     menuslot = menuslot - 1
     IF menuslot >= 0 THEN
      scriptret = menus(menuslot).handle
     END IF
    END IF
   CASE 291'--next menu
    menuslot = find_menu_handle(retvals(0))
    IF bound_menuslot(menuslot, "next menu") THEN
     menuslot = menuslot + 1
     IF menuslot <= topmenu THEN
      scriptret = menus(menuslot).handle
     END IF
    END IF
   CASE 292'--menu item by slot
    menuslot = find_menu_handle(retvals(0))
    IF bound_menuslot(menuslot, "menu item by slot") THEN
     scriptret = menu_item_handle_by_slot(menuslot, retvals(1), retvals(2)<>0)
    END IF
   CASE 293'--previous menu item
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF bound_menuslot_and_mislot(menuslot, mislot, "previous menu item") THEN
     scriptret = menu_item_handle_by_slot(menuslot, mislot - 1, retvals(1)<>0)
    END IF
   CASE 294'--next menu item
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF bound_menuslot_and_mislot(menuslot, mislot, "next menu item") THEN
     scriptret = menu_item_handle_by_slot(menuslot, mislot - 1, retvals(1)<>0)
    END IF
   CASE 295'--selected menu item
    menuslot = find_menu_handle(retvals(0))
    IF bound_menuslot(menuslot, "selected menu item") THEN
     scriptret = menu_item_handle_by_slot(menuslot, mstates(menuslot).pt)
    END IF
   CASE 296'--select menu item
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF bound_menuslot_and_mislot(menuslot, mislot, "select menu item") THEN
     mstates(menuslot).pt = menu_item_handle_by_slot(menuslot, mislot)
     mstates(menuslot).need_update = YES
    END IF
   CASE 297'--parent menu
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF bound_menuslot_and_mislot(menuslot, mislot, "parent menu") THEN
     scriptret = menus(menuslot).handle
    END IF
   CASE 298'--get menu ID
    menuslot = find_menu_handle(retvals(0))
    IF bound_menuslot(menuslot, "get menu ID") THEN
     scriptret = menus(menuslot).record
    END IF
   CASE 299'--swap menu items
    DIM AS INTEGER menuslot2, mislot2
    mislot = find_menu_item_handle(retvals(0), menuslot)
    mislot2 = find_menu_item_handle(retvals(1), menuslot2)
    IF bound_menuslot_and_mislot(menuslot, mislot, "swap menu items") THEN
     IF bound_menuslot_and_mislot(menuslot2, mislot2, "swap menu items") THEN
      SWAP menus(menuslot).items(mislot), menus(menuslot2).items(mislot2)
      mstates(menuslot).need_update = YES
      mstates(menuslot2).need_update = YES
     END IF
    END IF
   CASE 300'--find menu item caption
    IF bound_plotstr(retvals(1), "find menu item caption") THEN
     menuslot = find_menu_handle(retvals(0))
     DIM start_slot AS INTEGER
     IF retvals(3) = 0 THEN
      start_slot = 0
     ELSE
      start_slot = find_menu_item_handle_in_menuslot(retvals(3), menuslot) + 1
     END IF
     IF bound_menuslot_and_mislot(menuslot, start_slot, "find menu item caption") THEN
      mislot = find_menu_item_slot_by_string(menuslot, plotstr(retvals(1)).s, start_slot, (retvals(2) <> 0))
      IF mislot >= 0 THEN scriptret = menus(menuslot).items(mislot).handle
     END IF
    END IF
   CASE 301'--find menu ID
    IF bound_arg(retvals(0), 0, gen(genMaxMenu), "find menu ID", "menu ID") THEN
     menuslot = find_menu_id(retvals(0))
     IF menuslot >= 0 THEN
      scriptret = menus(menuslot).handle
     END IF
    END IF
   CASE 302'--menu is open
    menuslot = find_menu_handle(retvals(0))
    IF menuslot = -1 THEN
     scriptret = 0
    ELSE
     scriptret = 1
    END IF
   CASE 303'--menu item slot
    mislot = find_menu_item_handle(retvals(0), menuslot)
    IF bound_menuslot_and_mislot(menuslot, mislot, "menu item slot") THEN
     scriptret = mislot
    END IF
   CASE 304'--outside battle cure
    IF bound_arg(retvals(0), 0, gen(genMaxAttack), "outside battle cure", "attack ID") THEN
     IF bound_hero_party(retvals(1), "outside battle cure") THEN
      IF bound_hero_party(retvals(2), "outside battle cure", -1) THEN
       scriptret = ABS(outside_battle_cure(retvals(0), retvals(1), retvals(2), stat(), 0))
      END IF
     END IF
    END IF
   CASE ELSE '--try all the scripts implemented in subs
    scriptnpc scrat(nowscript).curvalue
    scriptmisc scrat(nowscript).curvalue
    scriptadvanced scrat(nowscript).curvalue
    scriptstat scrat(nowscript).curvalue, stat()
    '---------
  END SELECT
END SELECT
RETRACE

FUNCTION bound_item(itemID AS INTEGER, cmd AS STRING) AS INTEGER
 RETURN bound_arg(itemID, 0, gen(genMaxItem), cmd, "item ID")
END FUNCTION

FUNCTION bound_hero_party(who AS INTEGER, cmd AS STRING, minimum AS INTEGER=0) AS INTEGER
 RETURN bound_arg(who, minimum, 40, cmd, "hero party slot")
END FUNCTION

FUNCTION bound_menuslot(menuslot AS INTEGER, cmd AS STRING) AS INTEGER
 RETURN bound_arg(menuslot, 0, topmenu, cmd, "menu handle")
END FUNCTION

FUNCTION bound_menuslot_and_mislot(menuslot AS INTEGER, mislot AS INTEGER, cmd AS STRING) AS INTEGER
 IF bound_menuslot(menuslot, cmd) THEN
  RETURN bound_arg(mislot, 0, UBOUND(menus(menuslot).items), cmd, "menu item handle")
 END IF
 RETURN NO
END FUNCTION

FUNCTION bound_plotstr(n AS INTEGER, cmd AS STRING) AS INTEGER
 RETURN bound_arg(n, 0, UBOUND(plotstr), cmd, "string ID")
END FUNCTION

SUB loadmap_gmap(mapnum)
 loadrecord gmap(), game + ".map", getbinsize(4) / 2, mapnum
 loadpage game + ".til", gmap(0), 3
 loadtanim gmap(0), tastuf()
 FOR i = 0 TO 1
  cycle(i) = 0
  cycptr(i) = 0
  cycskip(i) = 0
 NEXT i
 correctbackdrop
 SELECT CASE gmap(5) '--outer edge wrapping
  CASE 0, 1'--crop edges or wrap
   setoutside -1
  CASE 2
   setoutside gmap(6)
 END SELECT
END SUB

SUB loadmap_npcl(mapnum)
 LoadNPCL maplumpname$(mapnum, "l"), npc(), 300
END SUB

SUB loadmap_npcd(mapnum)
 LoadNPCD maplumpname$(mapnum, "n"), npcs()
END SUB

SUB loadmap_tilemap(mapnum)
 LoadTileData maplumpname$(mapnum, "t"), scroll(), 3
END SUB

SUB loadmap_passmap(mapnum)
 LoadTileData maplumpname$(mapnum, "p"), pass(), 1
END SUB

SUB loadmaplumps (mapnum, loadmask)
 'loads some, but not all the lumps needed for each map
 IF loadmask AND 1 THEN
  loadmap_gmap mapnum
 END IF
 IF loadmask AND 2 THEN
  loadmap_npcl mapnum
 END IF
 IF loadmask AND 4 THEN
  loadmap_npcd mapnum
 END IF
 IF loadmask AND 8 THEN
  loadmap_tilemap mapnum
 END IF
 IF loadmask AND 16 THEN
  loadmap_passmap mapnum
 END IF
 IF loadmask AND (2 OR 4) THEN
  npcplot
 END IF
END SUB

Sub MenuSound(byval s as integer)
  if s then stopsfx s-1:playsfx s-1, 0
End Sub

SUB LoadGen
  dim as integer genlen, ff
  dim as short s

  if not isfile(game + ".gen") then fatalerror("general data missing from " & game): exit sub

  genlen = 0
  ff = freefile

  OPEN game + ".gen" FOR BINARY AS #ff
  SEEK #ff, 8

  DO UNTIL EOF(ff) OR genlen > UBOUND(gen)
    get #ff, , s
    gen(genlen) = s
    genlen += 1
  LOOP
  genlen -= 1
END SUB

SUB dotimer(byval l as integer)
  dim i as integer
  for i = 0 to 15
    with timers(i)
      if .speed > 0 then
        if ((l = 1) AND (.flags AND 2 = 0)) OR ((l = 2) AND (.flags AND 4 = 0)) then continue for 'not supposed to run here
        'debug "updating timer #" & i

        if .st > 0 then
          if plotstr(.st - 1).s = "" then plotstr(.st - 1).s = seconds2str(.count)
        end if

        .ticks += 1
        if .ticks >= .speed then
          .count -= 1
          .ticks = 0
          if .st > 0 and .count >= 0 then plotstr(.st - 1).s = seconds2str(.count)
          if .count < 0 then
            .speed *= -1
            .speed -= 1
            'do something
            if l = 0 then 'on the field
              if .trigger = -2 then 'game over
                fatal = 1
                abortg = 1

                exit sub
              end if

              if .trigger = -1 then 'undefined, shouldn't happen
              end if

              if .trigger > -1 then 'plotscript
                rsr = runscript(.trigger, nowscript + 1, -1, "timer", 0)
                IF rsr = 1 THEN
                  setScriptArg 0, i
                END IF
              end if
            end if
          end if
        end if
      end if
    end with
  next
end sub

function dotimerbattle() as integer
  dotimer 1  'no sense duplicating code

  dim i as integer
  for i = 0 to 15
    with timers(i)
      if .speed < 0 then 'normally, not valid. but, if a timer expired in battle, this will be -ve, -1
        if .flags AND 1 then return -1
      end if
    end with
  next
  return 0
end function

function dotimermenu() as integer
  dotimer 2  'no sense duplicating code

  dim i as integer
  for i = 0 to 15
    with timers(i)
      if .speed < 0 then 'normally, not valid. but, if a timer expired in the menu, this will be -ve, -1
        if .flags AND 1 then return -1
      end if
    end with
  next
  return 0
end function

Sub dotimerafterbattle()
  dim i as integer
  for i = 0 to 15
    with timers(i)
      if .speed < 0 then 'normally, not valid. but, if a timer expired in battle, this will be -ve, -1
        .speed *= -1
        .speed -= 1

        .count = 0
        .ticks = .speed
      end if
    end with
  next

end sub

FUNCTION count_sav(filename AS STRING) AS INTEGER
 DIM i AS INTEGER
 DIM n AS INTEGER
 DIM savver AS INTEGER
 n = 0
 setpicstuf buffer(), 30000, -1
 FOR i = 0 TO 3
  loadset filename, i * 2, 0
  savver = buffer(0)
  IF savver = 3 THEN n += 1
 NEXT i
 RETURN n
END FUNCTION

FUNCTION add_menu (record AS INTEGER, allow_duplicate AS INTEGER=NO) AS INTEGER
 IF record > = 0 AND allow_duplicate = NO THEN
  'If adding a non-blank menu, first check if the requested menu is already open
  DIM menuslot AS INTEGER
  menuslot = find_menu_id(record)
  IF menuslot >= 0 THEN
   'the requested menu is already open, just bring it to the top
   bring_menu_forward menuslot
   RETURN menus(topmenu).handle
  END IF
 END IF
 'Load the menu into a new menu slot
 topmenu = topmenu + 1
 IF topmenu > UBOUND(menus) THEN
  REDIM PRESERVE menus(topmenu) AS MenuDef
  REDIM PRESERVE mstates(topmenu) AS MenuState
 END IF
 mstates(topmenu).top = 0
 mstates(topmenu).pt = 0
 IF record = -1 THEN
  ClearMenuData menus(topmenu)
 ELSE
  LoadMenuData menu_set, menus(topmenu), record
 END IF
 init_menu_state mstates(topmenu), menus(topmenu)
 mstates(topmenu).active = YES
 check_menu_tags
 RETURN assign_menu_handles(menus(topmenu))
END FUNCTION

SUB remove_menu (slot AS INTEGER)
 IF slot < 0 OR slot > UBOUND(menus) THEN debug "remove_menu: invalid slot " & slot : EXIT SUB
 bring_menu_forward slot
 ClearMenuData menus(topmenu)
 topmenu = topmenu - 1
 IF topmenu >=0 THEN
  REDIM PRESERVE menus(topmenu) AS MenuDef
  REDIM PRESERVE mstates(topmenu) AS MenuState
  mstates(topmenu).active = YES
 END IF
END SUB

SUB bring_menu_forward (slot AS INTEGER)
 DIM i AS INTEGER
 IF slot < 0 OR slot > UBOUND(menus) THEN debug "bring_menu_forward: invalid slot " & slot : EXIT SUB
 FOR i = slot TO topmenu - 1
  SWAP menus(i), menus(i + 1)
  SWAP mstates(i), mstates(i + 1)
 NEXT i
END SUB

FUNCTION menus_allow_gameplay () AS INTEGER
 IF topmenu < 0 THEN RETURN YES
 RETURN menus(topmenu).allow_gameplay
END FUNCTION

FUNCTION menus_allow_player () AS INTEGER
 IF topmenu < 0 THEN RETURN YES
 RETURN menus(topmenu).suspend_player = NO
END FUNCTION

SUB player_menu_keys (BYREF menu_text_box AS INTEGER, BYREF wantloadgame AS INTEGER, stat(), catx(), caty(), tastuf(), map, foep, stock())
 DIM slot AS INTEGER
 DIM activated AS INTEGER
 DIM menu_handle AS INTEGER
 menu_text_box = 0
 IF topmenu >= 0 THEN
  IF menus(topmenu).no_controls = YES THEN EXIT SUB
  menu_handle = menus(topmenu).handle 'store handle for later use
  IF game_usemenu(mstates(topmenu)) THEN
   menusound gen(genCursorSFX)
  END IF
  IF carray(5) > 1 AND menus(topmenu).no_close = NO THEN
   carray(5) = 0
   setkeys ' Forget keypress that closed the menu
   remove_menu topmenu
   menusound gen(genCancelSFX)
   fatal = checkfordeath(stat())
   EXIT SUB
  END IF
  activated = NO
  WITH menus(topmenu).items(mstates(topmenu).pt)
   IF .disabled THEN EXIT SUB
   IF carray(4) > 1 THEN
    activated = YES
    SELECT CASE .t
     CASE 0 ' Label
      SELECT CASE .sub_t
       CASE 0 'Selectable
       CASE 1 'Unselectable
        activated = NO
      END SELECT
     CASE 1 ' Special
      SELECT CASE .sub_t
       CASE 0 ' item
        menu_text_box = items(stat())
        IF menu_text_box > 0 THEN
         remove_menu topmenu
         EXIT SUB
        END IF
       CASE 1 ' spell
        slot = onwho(readglobalstring$(106, "Whose Spells?", 20), 0)
        IF slot >= 0 THEN spells slot, stat()
       CASE 2 ' status
        slot = onwho(readglobalstring$(104, "Whose Status?", 20), 0)
        IF slot >= 0 THEN status slot, stat()
       CASE 3 ' equip
        slot = onwho(readglobalstring$(108, "Equip Whom?", 20), 0)
        IF slot >= 0 THEN equip slot, stat()
       CASE 4 ' order
        heroswap 0, stat()
       CASE 5 ' team
        heroswap 1, stat()
       CASE 6 ' order/team
        heroswap readbit(gen(), 101, 5), stat()
       CASE 7,12 ' map
        minimap catx(0), caty(0), tastuf()
       CASE 8,13 ' save
        slot = picksave(0)
        IF slot >= 0 THEN savegame slot, map, foep, stat(), stock()
        reloadnpc stat()
       CASE 9 ' load
        slot = picksave(1)
        reloadnpc stat()
        IF slot >= 0 THEN
         wantloadgame = slot + 1
         FOR i = topmenu TO 0 STEP -1
          remove_menu i
         NEXT i
         EXIT SUB
        END IF
       CASE 10 ' quit
        menusound gen(genAcceptSFX)
        verquit
       CASE 11 ' volume
        activated = NO
      END SELECT
     CASE 2 ' Menu
      mstates(topmenu).active = NO
      add_menu .sub_t
     CASE 3 ' Text box
      menu_text_box = .sub_t
     CASE 4 ' Run Script
      rsr = runscript(.sub_t, nowscript + 1, YES, "menuitem", plottrigger)
      IF rsr = 1 THEN
       setScriptArg 0, .handle
      END IF
    END SELECT
   END IF
   IF .t = 1 AND .sub_t = 11 THEN '--volume
    IF carray(2) > 1 THEN fmvol = large(fmvol - 1, 0): setfmvol fmvol
    IF carray(3) > 1 THEN fmvol = small(fmvol + 1, 15): setfmvol fmvol
   END IF
   IF activated THEN
    IF .settag > 1 THEN setbit tag(), 0, .settag, YES
    IF .settag < -1 THEN setbit tag(), 0, ABS(.settag), NO
    IF .togtag > 1 THEN setbit tag(), 0, .togtag, (readbit(tag(), 0, .togtag) XOR 1)
    IF .close_if_selected THEN
     remove_menu find_menu_handle(menu_handle)
     carray(4) = 0
     setkeys '--Discard the  keypress that triggered the menu item that closed the menu
     EXIT SUB
    END IF
   END IF
  END WITH
 END IF
END SUB

SUB check_menu_tags ()
 DIM i AS INTEGER
 DIM j AS INTEGER
 DIM old AS INTEGER
 DIM changed AS INTEGER
 DIM remember AS INTEGER
 FOR j = 0 TO topmenu
  WITH menus(j)
   changed = NO
   FOR i = 0 TO UBOUND(.items)
    WITH .items(i)
     old = .disabled
     .disabled = NO
     IF NOT (istag(.tag1, YES) AND istag(.tag2, YES)) THEN .disabled = YES
     IF .t = 0 AND .sub_t = 1 THEN .disabled = YES
     IF .t = 1 AND .sub_t = 7 AND gmap(2) = 0 THEN .disabled = YES 'Minimap disabled on this map
     IF .t = 1 AND .sub_t = 8 AND gmap(3) = 0 THEN .disabled = YES 'Save anywhere disabled on this map
     IF old <> .disabled THEN changed = YES
    END WITH
   NEXT i
   IF changed = YES THEN
    remember = .items(mstates(j).pt).sortorder
    SortMenuItems .items()
    FOR i = 0 TO UBOUND(.items)
     WITH .items(i)
      IF .exists = NO THEN EXIT FOR
      IF .disabled AND .hide_if_disabled THEN EXIT FOR
      IF remember = .sortorder THEN
        mstates(j).pt = i
        EXIT FOR
      END IF
     END WITH
    NEXT i
    mstates(j).need_update = YES
   END IF
  END WITH
 NEXT j
END SUB

FUNCTION game_usemenu (state AS MenuState)
 DIM oldptr AS INTEGER
 DIM oldtop AS INTEGER

 WITH state
  oldptr = .pt
  oldtop = .top

  IF carray(0) > 1 THEN .pt = loopvar(.pt, .first, .last, -1) 'UP
  IF carray(1) > 1 THEN .pt = loopvar(.pt, .first, .last, 1)  'DOWN
  IF keyval(73) > 1 THEN .pt = large(.pt - .size, .first)     'PGUP
  IF keyval(81) > 1 THEN .pt = small(.pt + .size, .last)      'PGDN
  IF keyval(71) > 1 THEN .pt = .first                         'HOME
  IF keyval(79) > 1 THEN .pt = .last                          'END
  .top = bound(.top, .pt - .size, .pt)

  IF oldptr <> .pt OR oldtop <> .top THEN RETURN YES
  RETURN NO
 END WITH
END FUNCTION

FUNCTION find_menu_id (id AS INTEGER) AS INTEGER
 DIM i AS INTEGER
 FOR i = topmenu TO 0 STEP -1
  IF menus(i).record = id THEN
   RETURN i 'return slot
  END IF
 NEXT i
 RETURN -1 ' Not found
END FUNCTION

FUNCTION find_menu_handle (handle) AS INTEGER
 DIM i AS INTEGER
 FOR i = 0 TO topmenu
  IF menus(i).handle = handle THEN RETURN i 'return slot
 NEXT i
 RETURN -1 ' Not found
END FUNCTION

FUNCTION find_menu_item_handle_in_menuslot (handle AS INTEGER, menuslot AS INTEGER) AS INTEGER
 DIM mislot AS INTEGER
 WITH menus(menuslot)
  FOR mislot = 0 TO UBOUND(.items)
   WITH .items(mislot)
    IF .exists AND .handle = handle THEN
     RETURN mislot
    END IF
   END WITH
  NEXT mislot
 END WITH
 RETURN -1 ' Not found
END FUNCTION

FUNCTION find_menu_item_handle (handle AS INTEGER, BYREF found_in_menuslot) AS INTEGER
 DIM menuslot AS INTEGER
 DIM mislot AS INTEGER
 DIM found AS INTEGER
 FOR menuslot = 0 TO topmenu
  found = find_menu_item_handle_in_menuslot(handle, menuslot)
  IF found >= 0 THEN
   found_in_menuslot = menuslot
   RETURN found
  END IF
 NEXT menuslot
 found_in_menuslot = -1
 RETURN -1 ' Not found
END FUNCTION

FUNCTION assign_menu_item_handle (BYREF mi AS MenuDefItem) AS INTEGER
 STATIC new_handle = 0
 IF mi.exists THEN
  new_handle = new_handle + 1
  mi.handle = new_handle
  RETURN new_handle
 END IF
 RETURN 0
END FUNCTION

FUNCTION assign_menu_handles (BYREF menu AS MenuDef) AS INTEGER
 DIM i AS INTEGER
 STATIC new_handle = 0
 new_handle = new_handle + 1
 menus(topmenu).handle = new_handle
 FOR i = 0 TO UBOUND(menu.items)
  assign_menu_item_handle menu.items(i)
 NEXT i
 RETURN new_handle
END FUNCTION

FUNCTION menu_item_handle_by_slot(menuslot AS INTEGER, mislot AS INTEGER, visible_only AS INTEGER=YES) AS INTEGER
 IF menuslot >= 0 AND menuslot <= topmenu THEN
  WITH menus(menuslot)
   IF mislot >= 0 AND mislot <= UBOUND(.items) THEN
    WITH .items(mislot)
     IF .exists = NO THEN RETURN 0
     IF visible_only AND .disabled AND .hide_if_disabled THEN RETURN 0
     RETURN .handle
    END WITH
   END IF
  END WITH
 END IF
 RETURN 0
END FUNCTION

FUNCTION find_menu_item_slot_by_string(menuslot AS INTEGER, s AS STRING, mislot AS INTEGER=0, visible_only AS INTEGER=YES) AS INTEGER
 DIM cap AS STRING
 WITH menus(menuslot)
  FOR i = mislot TO UBOUND(.items)
   WITH .items(i)
    IF .exists = NO THEN CONTINUE FOR
    IF visible_only AND .disabled AND .hide_if_disabled THEN CONTINUE FOR
    cap = get_menu_item_caption(menus(menuslot).items(i), menus(menuslot))
    IF cap = s THEN
     RETURN i
    END IF
   END WITH
  NEXT i
 END WITH
 RETURN -1 ' not found
END FUNCTION

FUNCTION allowed_to_open_main_menu () AS INTEGER
 DIM i AS INTEGER
 IF find_menu_id(0) >= 0 THEN RETURN NO 'Already open
 FOR i = topmenu TO 0 STEP -1
  IF menus(i).prevent_main_menu = YES THEN RETURN NO
 NEXT i
 RETURN YES
END FUNCTION
