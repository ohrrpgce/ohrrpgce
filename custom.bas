'OHRRPGCE CUSTOM - Main module
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z
'basic subs and functions
DECLARE SUB writepassword (p$)
DECLARE FUNCTION readpassword$ ()
DECLARE SUB fixfilename (s$)
DECLARE FUNCTION filenum$ (n%)
DECLARE SUB touchfile (f$)
DECLARE SUB standardmenu (menu$(), size%, vis%, pt%, top%, x%, y%, page%, edge%)
DECLARE FUNCTION readenemyname$ (index%)
DECLARE FUNCTION zintgrabber% (n%, min%, max%, less%, more%)
DECLARE FUNCTION readitemname$ (index%)
DECLARE SUB setbinsize (id%, size%)
DECLARE FUNCTION getbinsize% (id%)
DECLARE SUB flusharray (array%(), size%, value%)
DECLARE FUNCTION readattackname$ (index%)
DECLARE SUB writeglobalstring (index%, s$, maxlen%)
DECLARE FUNCTION readglobalstring$ (index%, default$, maxlen%)
DECLARE SUB importbmp (f$, cap$, count%)
DECLARE SUB getpal16 (array%(), aoffset%, foffset%)
DECLARE SUB upgrade (font%())
DECLARE SUB loadpasdefaults (array%(), tilesetnum%)
DECLARE SUB textxbload (f$, array%(), e$)
DECLARE SUB fixorder (f$)
DECLARE FUNCTION unlumpone% (lumpfile$, onelump$, asfile$)
DECLARE SUB vehicles ()
DECLARE SUB verifyrpg ()
DECLARE FUNCTION scriptname$ (num%, f$)
DECLARE FUNCTION getmapname$ (m%)
DECLARE FUNCTION numbertail$ (s$)
DECLARE SUB cropafter (index%, limit%, flushafter%, lump$, bytes%, prompt%)
DECLARE SUB scriptman ()
DECLARE FUNCTION exclude$ (s$, x$)
DECLARE FUNCTION exclusive$ (s$, x$)
DECLARE SUB writescatter (s$, lhold%, start%)
DECLARE SUB readscatter (s$, lhold%, start%)
DECLARE SUB fontedit (font%(), gamedir$)
DECLARE SUB savetanim (n%, tastuf%())
DECLARE SUB loadtanim (n%, tastuf%())
DECLARE SUB cycletile (cycle%(), tastuf%(), pt%(), skip%())
DECLARE SUB testanimpattern (tastuf%(), taset%)
DECLARE FUNCTION heroname$ (num%, cond%(), a%())
DECLARE FUNCTION onoroff$ (n%)
DECLARE FUNCTION lmnemonic$ (index%)
DECLARE SUB smnemonic (tagname$, index%)
DECLARE SUB tagnames ()
DECLARE SUB sizemar (array%(), wide%, high%, tempx%, tempy%, tempw%, temph%, yout%, page%)
DECLARE SUB drawmini (high%, wide%, cursor%(), page%, tastuf%())
DECLARE FUNCTION rotascii$ (s$, o%)
DECLARE SUB mapmaker (font%(), map%(), pass%(), emap%(), doors%(), link%(), npcn%(), npcstat%())
DECLARE SUB npcdef (npcn%(), pt%)
DECLARE SUB editbitset (array%(), wof%, last%, names$())
DECLARE SUB sprite (xw%, yw%, sets%, perset%, soff%, foff%, atatime%, info$(), size%, zoom%, file$, font%())
DECLARE FUNCTION needaddset (pt%, check%, what$)
DECLARE SUB shopdata ()
DECLARE FUNCTION intgrabber (n%, min%, max%, less%, more%)
DECLARE SUB strgrabber (s$, maxl%)
DECLARE SUB importsong ()
DECLARE SUB importsfx ()
DECLARE SUB gendata ()
DECLARE SUB itemdata ()
DECLARE SUB formation ()
DECLARE SUB enemydata ()
DECLARE SUB herodata ()
DECLARE SUB attackdata ()
DECLARE SUB getnames (stat$(), max%)
DECLARE SUB statname ()
DECLARE SUB textage ()
DECLARE SUB editmenus ()
DECLARE FUNCTION sublist% (num%, s$())
DECLARE SUB maptile (font())
DECLARE FUNCTION maplumpname$ (map, oldext$)
DECLARE SUB updaterecordlength (lumpf$, bindex%)
DECLARE FUNCTION itemstr$(it%,hiden%,offbyone%)
DECLARE FUNCTION inputfilename$ (query$, ext$)
DECLARE FUNCTION newRPGfile (template$, newrpg$)
DECLARE SUB dolumpfiles (filetolump$)
DECLARE FUNCTION readarchinym$ ()

'$INCLUDE: 'compat.bi'
'$INCLUDE: 'allmodex.bi'
'$INCLUDE: 'common.bi' 
'$INCLUDE: 'cglobals.bi'

'$INCLUDE: 'const.bi'
'$INCLUDE: 'uiconst.bi'

workingdir$ = "working.tmp"

'version ID
'$INCLUDE: 'cver.txt'
'PRINT isn't going to work in FB Allegro
IF (LCASE$(COMMAND$) = "/v" AND NOT LINUX) OR LCASE$(COMMAND$) = "-v" THEN PRINT version$: SYSTEM
commandlineargs

gamedir$ = exepath$
CHDIR gamedir$

DIM font(1024), master(767), buffer(16384), timing(4), joy(4), scroll(16002), pass(16002), emap(16002)
DIM menu$(22), general(360), keyv(55, 3), doors(300), rpg$(255), hinfo$(7), einfo$(0), ainfo$(2), xinfo$(1), winfo$(7), link(1000), npcn(1500), npcstat(1500), spriteclip(1600), uilook(uiColors)
'more global variables
DIM game$, gamefile$, unsafefile$, insert
DIM vpage, dpage, fadestate, workingdir$, version$ 

'--DIM binsize arrays
'$INCLUDE: 'binsize.bi'

RANDOMIZE TIMER
textxbload "ohrrpgce.mas", master(), "default master palette ohrrpgce.mas is missing"
textxbload "ohrrpgce.fnt", font(), "default font ohrrpgce.fnt is missing"
getui ""
setmodex
ON ERROR GOTO modeXerr
setpal master()
setfont font()
setdiskpages buffer(), 200, 0
textcolor 15, 0
GOSUB readstuff

dpage = 1: vpage = 0: Rate = 160
game$ = ""
gamefile$ = ""

GOSUB makeworkingdir
cmdline$ = getcommandline
IF cmdline$ <> "" THEN
 IF isfile(cmdline$) OR isdir(cmdline$) THEN 
  gamefile$ = cmdline$
  game$ = trimextension$(trimpath$(gamefile$))
 ELSE
  centerbox 160, 40, 300, 50, 3, 0
  edgeprint "File not found:", 15, 30, uilook(uiText), 0
  edgeprint RIGHT$(cmdline$,35), 15, 40, uilook(uiText), 0
  setvispage 0
  w = getkey
 END IF
END IF
IF game$ = "" THEN
 GOSUB chooserpg
END IF
setwindowtitle "OHRRPGCE - " + gamefile$

GOSUB checkpass

clearpage 0
setvispage 0
textcolor 15, 0
printstr "UNLUMPING DATA: please wait.", 0, 0, 0

touchfile workingdir$ + SLASH + "__danger.tmp"

IF isdir(gamefile$) THEN
 'work on an unlumped RPG file
 findfiles gamefile$ + SLASH + ALLFILES, 0, "filelist.tmp", buffer()
 fh = FREEFILE
 OPEN "filelist.tmp" FOR INPUT AS #fh
 DO UNTIL EOF(fh)
  LINE INPUT #fh, filename$
  copyfile gamefile$ + SLASH + filename$, workingdir$ + SLASH + filename$, buffer()
 LOOP
 CLOSE #fh
 KILL "filelist.tmp"
ELSE 
 ERASE scroll, pass, emap
 DIM lumpbuf(16383)
 unlump gamefile$, workingdir$ + SLASH, lumpbuf()
 ERASE lumpbuf
 DIM scroll(16002), pass(16002), emap(16002)
END IF
game$ = workingdir$ + SLASH + game$
verifyrpg
safekill workingdir$ + SLASH + "__danger.tmp"

IF NOT isfile(game$ + ".mas") THEN copyfile "ohrrpgce.mas", game$ + ".mas", buffer()
xbload game$ + ".mas", master(), "Master palette not found"
setpal master()
getui workingdir$ + SLASH + "uilook.bin"
IF NOT isfile(game$ + ".fnt") THEN copyfile "ohrrpgce.fnt", game$ + ".fnt", buffer()
xbload game$ + ".fnt", font(), "Font not loaded"
'--loadgen, upgrade, resave
xbload game$ + ".gen", general(), "general data is missing, RPG file corruption is likely"
upgrade font()
xbsave game$ + ".gen", general(), 1000
setfont font()
needf = 1

menumode = 0
pt = 0
mainmax = 0
quitnow = 0

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
    IF quitnow > 1 THEN GOTO finis
   CASE 1'--graphics
    pt = 0: menumode = 0: GOSUB setmainmenu
  END SELECT
 END IF
 dummy = usemenu(pt, 0, 0, mainmax, 24)
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  SELECT CASE menumode
   CASE 0'--normal mode
    IF pt = 0 THEN pt = 0: menumode = 1: GOSUB setgraphicmenu
    IF pt = 1 THEN mapmaker font(), scroll(), pass(), emap(), doors(), link(), npcn(), npcstat()
    IF pt = 2 THEN statname
    IF pt = 3 THEN herodata
    IF pt = 4 THEN enemydata
    IF pt = 5 THEN attackdata
    IF pt = 6 THEN itemdata
    IF pt = 7 THEN shopdata
    IF pt = 8 THEN formation
    IF pt = 9 THEN textage
    'if pt = 10 then editmenus
    IF pt = 10 THEN vehicles
    IF pt = 11 THEN tagnames
    IF pt = 12 THEN importsong
    IF pt = 13 THEN importsfx
    IF pt = 14 THEN fontedit font(), gamedir$
    IF pt = 15 THEN gendata
    IF pt = 16 THEN scriptman
    IF pt = 17 THEN
     GOSUB relump
     IF quitnow > 1 THEN GOTO finis
    END IF
   CASE 1'--graphics mode
    IF pt = 0 THEN pt = 0: menumode = 0: GOSUB setmainmenu
    IF pt = 1 THEN maptile font()
    IF pt = 2 THEN sprite 20, 20, general(30), 8, 5, 0, 7, winfo$(), 200, 4, ".pt4", font()
    IF pt = 3 THEN sprite 32, 40, general(26), 8, 16, 0, 3, hinfo$(), 640, 4, ".pt0", font()
    IF pt = 4 THEN sprite 34, 34, general(27), 1, 2, 0, 4, einfo$(), 578, 4, ".pt1", font()
    IF pt = 5 THEN sprite 50, 50, general(28), 1, 4, 1, 2, einfo$(), 1250, 2, ".pt2", font()
    IF pt = 6 THEN sprite 80, 80, general(29), 1, 10, 2, 1, einfo$(), 3200, 2, ".pt3", font()
    IF pt = 7 THEN sprite 50, 50, general(32), 3, 12, 0, 2, ainfo$(), 1250, 2, ".pt6", font()
    IF pt = 8 THEN sprite 24, 24, general(31), 2, 2, 0, 5, xinfo$(), 288, 4, ".pt5", font()
    IF pt = 9 THEN importbmp ".mxs", "screen", general(100)
    IF pt = 10 THEN
     general(33) = general(33) + 1
     importbmp ".til", "tileset", general(33)
     general(33) = general(33) - 1
    END IF
  END SELECT
  '--always resave the .GEN lump after any menu
  xbsave game$ + ".gen", general(), 1000
 END IF

 standardmenu menu$(), mainmax, 22, pt, 0, 0, 0, dpage, 0

 textcolor 6, 0
 printstr version$, 0, 192, dpage

 SWAP vpage, dpage
 setvispage vpage
 copypage 2, dpage
 dowait
LOOP

setmainmenu:
mainmax = 17
menu$(0) = "Edit Graphics"
menu$(1) = "Edit Map Data"
menu$(2) = "Edit Global Text Strings"
menu$(3) = "Edit Hero Stats"
menu$(4) = "Edit Enemy Stats"
menu$(5) = "Edit Attacks"
menu$(6) = "Edit Items"
menu$(7) = "Edit Shops"
menu$(8) = "Edit Battle Formations"
menu$(9) = "Edit Text Boxes"
'menu$(10) = "Edit Menus"
menu$(10) = "Edit Vehicles"
menu$(11) = "Edit Tag Names"
menu$(12) = "Import Music"
menu$(13) = "Import Sound Effects"
menu$(14) = "Edit Font"
menu$(15) = "Edit General Game Data"
menu$(16) = "Script Management"
menu$(17) = "Quit Editing"
RETRACE

setgraphicmenu:
mainmax = 10
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
menu$(10) = "Import Full Maptile Sets"
RETRACE

chooserpg:
fh = FREEFILE
OPEN "rpg.lst" FOR INPUT AS #fh
last = 2: csr = 1: top = 0
rpg$(0) = "CREATE NEW GAME"
rpg$(1) = "LOAD EXISTING GAME"
rpg$(2) = "EXIT PROGRAM"

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN GOTO finis
 dummy = usemenu(csr, top, 0, last, 20)
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  IF csr = 0 THEN
   game$ = inputfilename$("Filename of New Game?", ".rpg")
   IF game$ <> "" THEN
     IF NOT newRPGfile("ohrrpgce.new", game$ + ".rpg") THEN GOTO finis
     gamefile$ = game$ + ".rpg"
     EXIT DO
   END IF
  ELSEIF csr = 1 THEN
   gamefile$ = browse$(7, "", "*.rpg", aquiretempdir$, 0)
   game$ = trimextension$(trimpath$(gamefile$))
   IF game$ <> "" THEN EXIT DO
  ELSEIF csr = 2 THEN
   GOTO finis
  END IF
 END IF

 standardmenu rpg$(), last, 22, csr, top, 0, 0, dpage, 0

 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
RETRACE

cleanup:
rpg$(0) = "RECOVER IT"
rpg$(1) = "ERASE IT"
rpg$(2) = "DO NOTHING"
temp = 0
a$ = "recovered"
i = 0
DO WHILE isfile("recovered" + STR$(i) + ".bak")
 i = i + 1
LOOP
a$ = a$ + STR$(i)
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 dummy = usemenu(temp, 0, 0, 2, 2)
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  IF temp = 0 THEN
   IF isfile(workingdir$ + SLASH + "__danger.tmp") THEN
    textcolor 14, 4
    printstr "Data is corrupt, not safe to relump", 0, 100, vpage
    setvispage vpage 'refresh
    w = getkey
   ELSE '---END UNSAFE
    printstr "Saving as " + a$ + ".bak", 0, 180, vpage
    printstr "LUMPING DATA: please wait...", 0, 190, vpage
    setvispage vpage 'refresh
    '--re-lump recovered files as BAK file
    dolumpfiles a$ + ".bak"
    clearpage vpage
    printstr "the recovered data has been saved.", 0, 0, vpage
    printstr "if " + CUSTOMEXE + " crashed last time you", 0, 8, vpage
    printstr "ran it and you lost work, you may", 0, 16, vpage
    printstr "be able to recover it. Make a backup", 0, 24, vpage
    printstr "copy of your RPG and then rename", 0, 32, vpage
    printstr a$ + ".bak to gamename.rpg", 0, 40, vpage
    printstr "If you have questions, ask", 0, 56, vpage
    printstr "ohrrpgce-crash@HamsterRepublic.com", 0, 64, vpage
    setvispage vpage 'refresh
    w = getkey
    RETRACE
   END IF '---END RELUMP
  END IF
  IF temp = 1 THEN RETRACE
  IF temp = 2 THEN nocleanup = 1: RETRACE
 END IF
 textcolor 9, 0
 printstr "A game was found unlumped", 0, 0, dpage
 printstr "This may mean that " + CUSTOMEXE + " crashed", 0, 40, dpage
 printstr "last time you used it, or it may mean", 0, 48, dpage
 printstr "that another copy of " + CUSTOMEXE + " is", 0, 56, dpage
 printstr "already running in the background.", 0, 64, dpage

 standardmenu rpg$(), 2, 2, temp, 0, 0, 8, dpage, 0

 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP'a$

makeworkingdir:
IF NOT isdir(workingdir$) THEN
 makedir workingdir$
ELSE
 'check for locked working directory
 ON ERROR GOTO tempDirErr
 safekill (workingdir$ + SLASH + "lockfile.tmp")
 ON ERROR GOTO 0
 'Recover from an old crash
 GOSUB cleanup
 clearpage 0
 setvispage 0
 textcolor 15, 0
 printstr "Run " + CUSTOMEXE + " again.", 0, 0, 0
 setvispage 0 'refresh
 w = getkey
 GOTO finis
END IF
'--open a lockfile to notify other instances of custom that the working
'--dir is in use
lockfile = FREEFILE
OPEN workingdir$ + SLASH + "lockfile.tmp" FOR BINARY AS #lockfile
RETRACE

relump:
xbsave game$ + ".gen", general(), 1000
rpg$(0) = "Continue editing"
rpg$(1) = "Save changes and continue editing"
rpg$(2) = "Save changes and quit"
rpg$(3) = "Discard changes and quit"
quitnow = sublist(3, rpg$())
IF quitnow = 1 OR quitnow = 2 THEN
 GOSUB dorelump
END IF
IF quitnow = 3 THEN
 rpg$(0) = "I changed my mind! Don't quit!"
 rpg$(1) = "I am sure I don't want to save."
 IF sublist(1, rpg$()) <= 0 THEN quitnow = 0
END IF
setkeys
RETRACE

dorelump:
clearpage 0
setvispage 0
textcolor 15, 0
printstr "LUMPING DATA: please wait.", 0, 0, 0
setvispage 0 'refresh
'--verify that maps are not corrupt--
verifyrpg
'--lump data to SAVE rpg file
dolumpfiles gamefile$
RETRACE

checkpass:
copylump gamefile$, "archinym.lmp", workingdir$
'--set game$ according to the archinym
game$ = readarchinym()
copylump gamefile$, game$ + ".gen", workingdir$
xbload workingdir$ + SLASH + game$ + ".gen", general(), "general data is missing, RPG file corruption is likely"
'----load password-----
IF general(5) >= 256 THEN
 '--new format password
 rpas$ = readpassword$
ELSE
 '--old scattertable format
 IF general(94) = -1 THEN RETRACE 'this is stupid
 readscatter rpas$, general(94), 200
 rpas$ = rotascii(rpas$, general(93) * -1)
END IF
'--if password is unset, do not prompt
IF rpas$ = "" THEN RETRACE
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
  '--check password
  IF pas$ = rpas$ THEN 
   RETRACE 
  ELSE 
   GOTO finis
  END IF
 END IF
 strgrabber pas$, 17
 textcolor 15, 0
 printstr "This game requires a password to edit", 0, 0, dpage
 printstr " Type it in and press ENTER", 0, 9, dpage
 textcolor 14 + tog, 1
 printstr STRING$(LEN(pas$), "*"), 0, 20, dpage
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

tempDirErr:
PRINT "Either " + CUSTOMEXE + " is already running in the background, or it"
PRINT "terminated incorrectly last time it was run, and was unable to clean up"
PRINT "its temporary files. The operating system is denying access to the"
PRINT "files in " + workingdir$
PRINT
PRINT "Error code"; ERR
ON ERROR GOTO 0
SYSTEM

modeXerr:
restoremode
IF LEN(unsafefile$) THEN
 PRINT unsafefile$
 GOSUB cleanupfiles
END IF
crashexplain
PRINT "Game:"; gamedir$ + SLASH + gamefile$
'--crash and print the error
ON ERROR GOTO 0

finis:
GOSUB cleanupfiles
setvispage 0
clearpage 0
textcolor 15, 0
printstr "Don't forget to keep backup copies of", 0, 0, 0
printstr "your work! You never know when an", 0, 8, 0
printstr "unknown bug or a hard-drive crash or", 0, 16, 0
printstr "a little brother might delete your", 0, 24, 0
printstr "files!", 0, 32, 0
setvispage 0 'force a refresh
w = getkey
'closefile
CHDIR curdir$
restoremode
SYSTEM

cleanupfiles:
CLOSE #lockfile
IF nocleanup = 0 THEN
 touchfile workingdir$ + SLASH + "kill.tmp"
 'borrowed this code from game.bas cos wildcard didn't work in FB
 findfiles workingdir$ + SLASH + ALLFILES, 0, "filelist.tmp", buffer()
 fh = FREEFILE
 OPEN "filelist.tmp" FOR INPUT AS #fh
 DO UNTIL EOF(fh)
  LINE INPUT #fh, filename$
  KILL workingdir$ + SLASH + filename$
 LOOP
 CLOSE #fh
 KILL "filelist.tmp"
 RMDIR workingdir$
END IF
safekill "rpg.lst"
safekill "temp.lst"
RETRACE

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

FOR o = 0 TO 3
 FOR i = 2 TO 53
  READ temp$
  IF temp$ <> "" THEN keyv(i, o) = ASC(temp$) ELSE keyv(i, o) = 0
 NEXT i
NEXT o

keyv(40, 1) = 34

RETRACE

menuitems:
DATA "Up A","Up B","Right A","Right B","Down A","Down B","Left A","Left B"
DATA "Standing","Stepping","Attack A","Attack B","Cast/Use","Hurt","Weak","Dead"
DATA "Enemy (facing right)"
DATA "Frame 1","Frame 2"
DATA "First Frame","Middle Frame","Last Frame"

DATA "1","2","3","4","5","6","7","8","9","0","-","=","","","q","w","e","r","t","y","u","i","o","p","[","]","","","a","s","d","f","g","h","j","k","l",";","'","`","","\","z","x","c","v","b","n","m",",",".","/"
DATA "!","@","#","$","%","^","&","*","(",")","_","+","","","Q","W","E","R","T","Y","U","I","O","P","{","}","","","A","S","D","F","G","H","J","K","L",":"," ","~","","|","Z","X","C","V","B","N","M","<",">","?"
DATA "‚","ƒ","„","…","†","‡","ˆ","‰","Š","‹","Œ","","","","Ž","","","‘","’","“","”","•","–","—","˜","™","","","š","›","œ","","ž","Ÿ"," ","¡","¢","£","¤","¥","","¦","§","¨","©","ª","«","¬","­","®","¯","°"
DATA "±","²","³","´","µ","¶","·","¸","¹","º","»","¼","","","½","¾","¿","À","Á","Â","Ã","Ä","Å","Æ","Ç","È","","","É","Ê","Ë","Ì","Í","Î","Ï","Ð","Ñ","Ò","Ó","Ô","","Õ","Ö","×","Ø","Ù","Ú","Û","Ü","Ý","Þ","ß"

'DATA Picture,Palette,"Animation:","Target Class:","Target Setting:",Damage,Aim,"Base Stat:","","",Money Cost,"Extra Damage%","Chain to:",Chain%,"Attacker Motion:","Attack Motion:",Delay,Number of Hits,"Target Stat:",Edit Bitsets...,"Name="
'DATA 0,0,0,0,0,0,0,0,-999,-9999,-32767,-100,0,0,0,0,0,1,0
'DATA 99,32767,3,9,4,6,4,20,999,9999,32767,1000,300,100,8,10,1000,20,11

'DATA Sleep,Stone,Poison,Plague,Blind,Mute,Insane,Slow,Weak,Soft,Curse,Power,Accuracy,Hyper,Shell,Dodge,Fast,Wall,Blessing

'---GENERIC LOOP HEAD---
'setkeys
'DO
'setwait timing(), 100
'setkeys
'tog = tog XOR 1
'IF keyval(1) > 1 THEN EXIT DO

'---GENERIC LOOP TAIL---
'SWAP vpage, dpage
'setvispage vpage
'copypage 3, dpage
'dowait
'LOOP

'---For documentation of general data see http://hamsterrepublic.com/ohrrpgce/index.php/GEN.html

REM $STATIC
FUNCTION filenum$ (n)
 IF n < 100 THEN
  filenum$ = RIGHT$("00" + STR$(n), 2)
 ELSE
  filenum$ = STR$(n)
 END IF
END FUNCTION

SUB fixfilename (s$)
result$ = ""
FOR i = 1 TO LEN(s$)
 char$ = MID$(s$, i, 1)
 ascii = ASC(char$)
 SELECT CASE ascii
  CASE 32, 48 TO 57, 65 TO 90, 97 TO 122, 95, 126, 45
   result$ = result$ + char$
 END SELECT
NEXT
s$ = result$
END SUB

SUB fixorder (f$)

copyfile f$, "fixorder.tmp", buffer()

ofh = FREEFILE
OPEN f$ FOR OUTPUT AS #ofh

ifh = FREEFILE
OPEN "fixorder.tmp" FOR INPUT AS #ifh

'--first output the archinym.lmp and browse.txt files
WHILE NOT EOF(ifh)
 LINE INPUT #ifh, a$
 b$ = LCASE$(a$)
 IF b$ = "archinym.lmp" OR b$ = "browse.txt" THEN
  PRINT #ofh, a$
 END IF
WEND

'--close and re-open
CLOSE #ifh
OPEN "fixorder.tmp" FOR INPUT AS #ifh

'--output the other files, excluding illegal files
WHILE NOT EOF(ifh)
 LINE INPUT #ifh, a$
 b$ = LCASE$(a$)
 SELECT CASE b$
  CASE "archinym.lmp", "browse.txt", "scripts.txt", "hs"
   '--do nothing
  CASE ELSE
   '--check extenstion
   c$ = RIGHT$(b$, 4)
   SELECT CASE c$
    CASE ".tmp", ".hsx", ".hsz"
     '--do nothing
    CASE ELSE
     '--output all other names
     PRINT #ofh, a$
   END SELECT
 END SELECT
WEND
CLOSE #ifh

CLOSE #ofh

safekill "fixorder.tmp"

END SUB

SUB fontedit (font(), gamedir$)

STATIC default$

DIM f(255), copybuf(4), menu$(3)

menu$(0) = "Previous Menu"
menu$(1) = "Edit Font..."
menu$(2) = "Import Font..."
menu$(3) = "Export Font..."

last = -1
FOR i = 32 TO 255
 last = last + 1
 f(last) = i
NEXT i

mode = -1

menuptr = 0
top = 0

linesize = 14
pt = -1 * linesize

setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 SELECT CASE mode
  CASE -1
   IF keyval(1) > 1 THEN EXIT DO
   dummy = usemenu(menuptr, 0, 0, 3, 22)
   IF keyval(57) > 1 OR keyval(28) > 1 THEN
    IF menuptr = 0 THEN EXIT DO
    IF menuptr = 1 THEN mode = 0
    IF menuptr = 2 THEN GOSUB importfont
    IF menuptr = 3 THEN GOSUB exportfont
   END IF
  CASE 0
   IF keyval(1) > 1 THEN mode = -1
   IF keyval(72) > 1 THEN pt = large(pt - linesize, -1 * linesize)
   IF keyval(80) > 1 THEN pt = small(pt + linesize, last)
   IF keyval(75) > 1 THEN pt = large(pt - 1, 0)
   IF keyval(77) > 1 THEN pt = small(pt + 1, last)
   IF keyval(57) > 1 OR keyval(28) > 1 THEN
    IF pt < 0 THEN
     mode = -1
    ELSE
     mode = 1
     x = 0: y = 0
    END IF
   END IF
   IF keyval(29) > 0 AND keyval(19) > 1 THEN romfontchar font(), pt
  CASE 1
   IF keyval(1) > 1 OR keyval(28) > 1 THEN mode = 0
   IF keyval(72) > 1 THEN y = loopvar(y, 0, 7, -1)
   IF keyval(80) > 1 THEN y = loopvar(y, 0, 7, 1)
   IF keyval(75) > 1 THEN x = loopvar(x, 0, 7, -1)
   IF keyval(77) > 1 THEN x = loopvar(x, 0, 7, 1)
   IF keyval(57) > 1 THEN
    setbit font(), 0, (f(pt) * 8 + x) * 8 + y, (readbit(font(), 0, (f(pt) * 8 + x) * 8 + y) XOR 1)
   END IF
 END SELECT
 IF mode >= 0 THEN
  '--copy and paste support
  IF (keyval(29) > 0 AND keyval(82) > 1) OR ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(83)) OR (keyval(29) > 0 AND keyval(46) > 1) THEN GOSUB copychar
  IF ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(82) > 1) OR (keyval(29) > 0 AND keyval(47) > 1) THEN GOSUB pastechar
 END IF

 IF mode = -1 THEN
  standardmenu menu$(), 3, 22, menuptr, 0, 0, 0, dpage, 0
 END IF

 IF mode >= 0 THEN
  xoff = 8: yoff = 8
  FOR i = 0 TO last
   textcolor 7, 8
   IF pt >= 0 THEN
    IF mode = 0 THEN
     IF (i MOD linesize) = (pt MOD linesize) OR (i \ linesize) = (pt \ linesize) THEN textcolor 7, 1
    END IF
    IF pt = i THEN textcolor 14 + tog, 0
   END IF
   printstr CHR$(f(i)), xoff + (i MOD linesize) * 9, yoff + (i \ linesize) * 9, dpage
  NEXT i
  textcolor 7, 0
  IF pt < 0 THEN textcolor 14 + tog, 0
  printstr menu$(0), 8, 0, dpage

  IF pt >= 0 THEN
   xoff = 150
   yoff = 4
   rectangle xoff, yoff, 160, 160, 8, dpage
   FOR i = 0 TO 7
    FOR j = 0 TO 7
     z = readbit(font(), 0, (f(pt) * 8 + i) * 8 + j)
     IF z THEN rectangle xoff + i * 20, yoff + j * 20, 20, 20, 7, dpage
    NEXT j
   NEXT i
   IF mode = 1 THEN rectangle xoff + x * 20, yoff + y * 20, 20, 20, 2 + 8 * readbit(font(), 0, (f(pt) * 8 + x) * 8 + y), dpage
   textcolor 15, 0
   printstr "ASCII" + XSTR$(f(pt)), 20, 190, dpage
   IF f(pt) < 32 THEN
    printstr "RESERVED", 120, 190, dpage
   ELSE
    FOR i = 2 TO 53
     IF f(pt) = keyv(i, 2) THEN printstr "ALT+" + UCASE$(CHR$(keyv(i, 0))), 120, 190, dpage
     IF f(pt) = keyv(i, 3) THEN printstr "ALT+SHIFT+" + UCASE$(CHR$(keyv(i, 0))), 120, 190, dpage
    NEXT i
    IF f(pt) = 32 THEN printstr "SPACE", 120, 190, dpage
   END IF
  END IF
 END IF

 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
GOSUB savefont
EXIT SUB

loadfont:
xbload game$ + ".fnt", font(), "Can't load font"
setfont font()
RETRACE

savefont:
xbsave game$ + ".fnt", font(), 2048
RETRACE

copychar:
FOR i = 0 TO 63
 setbit copybuf(), 0, i, readbit(font(), 0, f(pt) * 64 + i)
NEXT i
RETRACE

pastechar:
FOR i = 0 TO 63
 setbit font(), 0, f(pt) * 64 + i, readbit(copybuf(), 0, i)
NEXT i
RETRACE

importfont:
newfont$ = browse$(0, default$, "*.ohf", "")
IF newfont$ <> "" THEN
 copyfile newfont$, game$ + ".fnt", buffer()

 '--never overwrite 0 thru 31
 FOR i = 0 TO 2047
  setbit buffer(), 0, i, readbit(font(), 0, i)
 NEXT i

 GOSUB loadfont

 FOR i = 0 TO 2047
  setbit font(), 0, i, readbit(buffer(), 0, i)
 NEXT i
 menuptr = 1
 mode = 0
END IF
RETRACE

exportfont:
setkeys
newfont$ = "newfont"
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO

 old$ = newfont$
 strgrabber newfont$, 8
 IF old$ <> newfont$ THEN
  '--make sure only legal DOS filename chars are used
  lastchar = ASC(UCASE$(RIGHT$("_" + newfont$, 1)))
  SELECT CASE lastchar
   CASE 48 TO 57, 65 TO 90, 95, 126
    '--legal chars
   CASE 32
    '--replace space with underscore
    newfont$ = old$ + "_"
   CASE ELSE
    '--illegal chars
    newfont$ = old$
  END SELECT
 END IF

 IF keyval(28) > 1 THEN
  GOSUB savefont
  copyfile game$ + ".fnt", gamedir$ + SLASH + newfont$ + ".ohf", buffer()
  EXIT DO
 END IF

 textcolor 7, 0
 printstr "Input a filename to save to", 0, 0, dpage
 printstr "[" + gamedir$ + SLASH + newfont$ + ".ohf]", 0, 8, dpage
 textcolor 14 + tog, 1
 printstr newfont$, 0, 16, dpage

 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
RETRACE

END SUB

FUNCTION needaddset (pt, check, what$)
needaddset = 0
csr = 0
IF pt > check THEN
 setkeys
 DO
  setwait timing(), 100
  setkeys
  tog = tog XOR 1
  IF keyval(1) > 1 THEN pt = pt - 1: EXIT DO
  IF keyval(72) > 1 OR keyval(80) > 1 OR keyval(75) > 1 OR keyval(77) > 1 THEN csr = csr XOR 1
  IF keyval(57) > 1 OR keyval(28) > 1 THEN
   IF csr = 0 THEN
    check = check + 1
    needaddset = -1
   ELSE
    pt = pt - 1
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

FUNCTION readpassword$

'--read a 17-byte string from GEN at word offset 7
'--(Note that array2str uses the byte offset not the word offset)
s$ = STRING$(17, 0)
array2str general(), 14, s$

'--reverse ascii rotation / weak obfuscation
s$ = rotascii(s$, general(6) * -1)

'-- discard ascii chars lower than 32
p$ = ""
FOR i = 1 TO 17
 c$ = MID$(s$, i, 1)
 IF ASC(c$) >= 32 THEN p$ = p$ + c$
NEXT i

readpassword$ = p$

END FUNCTION

SUB shopdata
DIM names$(32), a(20), b(curbinsize(1) / 2), menu$(24), smenu$(24), max(24), min(24), sbit$(-1 TO 10), stf$(16), tradestf$(3)

maxcount = 32: pt = 0: it$ = "-NONE-": sn$ = ""
havestuf = 0
sbit$(0) = "Buy"
sbit$(1) = "Sell"
sbit$(2) = "Hire"
sbit$(3) = "Inn"
sbit$(4) = "Equip"
sbit$(5) = "Save"
sbit$(6) = "Map"
sbit$(7) = "Team"
smenu$(0) = "Previous Menu"
max(3) = 1
min(5) = -1
max(5) = 99
FOR i = 6 TO 9
 min(i) = -999: max(i) = 999
NEXT i
min(10) = -32767
max(10) = 32767
'max(11) = 255
'min(11) = -1
'min(13) = -32767
'max(13) = 32767
'max(14) = 255
'min(14) = -1
FOR i = 11 TO 17 STEP 2
 max(i) = 255
 min(i) = -1
 max(i + 1) = 99
 min(i + 1) = 1
NEXT

min(20) = -32767
max(20) = 32767
max(21) = 255
min(21) = -1
max(22) = 99
min(22) = 1
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
getnames names$(), maxcount

GOSUB lshopset
GOSUB menugen
li = 6
csr = 0
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 IF keyval(29) > 0 AND keyval(14) THEN cropafter pt, general(97), 0, game$ + ".sho", 40, 1: GOSUB menugen
 dummy = usemenu(csr, 0, 0, li, 24)
 IF csr = 1 THEN
  IF keyval(75) > 1 AND pt > 0 THEN
   GOSUB sshopset
   pt = pt - 1
   GOSUB lshopset
  END IF
  IF keyval(77) > 1 AND pt < 32767 THEN
   GOSUB sshopset
   pt = pt + 1
   IF needaddset(pt, general(97), "Shop") THEN
    flusharray a(), 19, 0
    setpicstuf a(), 40, -1
    storeset game$ + ".sho", pt, 0
   END IF
   GOSUB lshopset
  END IF
 END IF
 IF csr = 2 THEN
  strgrabber sn$, 15
  GOSUB menuup
 END IF
 IF keyval(28) > 1 OR keyval(57) > 1 THEN
  IF csr = 0 THEN EXIT DO
  IF csr = 3 AND havestuf THEN
   GOSUB shopstuf
   GOSUB sstuf
  END IF
  IF csr = 4 THEN editbitset a(), 17, 7, sbit$(): GOSUB menuup
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
GOSUB sshopset
clearpage 0
clearpage 1
clearpage 2
clearpage 3
EXIT SUB

menugen:
menu$(0) = "Return to Main Menu"
menu$(3) = "Edit Available Stuff..."
menu$(4) = "Edit Shop Bitsets..."
GOSUB menuup
RETRACE

lshopset:
setpicstuf a(), 40, -1
loadset game$ + ".sho", pt, 0
sn$ = ""
FOR i = 1 TO small(a(0), 15)
 sn$ = sn$ + CHR$(a(i))
NEXT i
GOSUB menuup
RETRACE

sshopset:
a(16) = small(a(16), 49)
a(0) = LEN(sn$)
FOR i = 1 TO small(a(0), 15)
 a(i) = ASC(MID$(sn$, i, 1))
NEXT i
setpicstuf a(), 40, -1
storeset game$ + ".sho", pt, 0
RETRACE

menuup:
menu$(1) = CHR$(27) + " Shop" + XSTR$(pt) + " of" + XSTR$(general(97)) + CHR$(26)
menu$(2) = "Name:" + sn$
menu$(5) = "Inn Price:" + XSTR$(a(18))
IF readbit(a(), 17, 3) = 0 THEN menu$(5) = "Inn Price: N/A"
menu$(6) = "Inn Script: " + scriptname$(a(19), "plotscr.lst")
IF readbit(a(), 17, 0) OR readbit(a(), 17, 1) OR readbit(a(), 17, 2) THEN havestuf = 1 ELSE havestuf = 0
RETRACE

shopstuf:
thing = 0
thing$ = ""
tcsr = 0
last = 2
GOSUB lstuf
GOSUB othertype
GOSUB itstrsh
GOSUB stufmenu
setkeys
DO
 setwait timing(), 100
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN RETRACE
 IF tcsr = 0 THEN IF keyval(57) > 1 OR keyval(28) > 1 THEN RETRACE
 'IF keyval(72) > 1 THEN tcsr = large(0, tcsr - 1)
 'IF keyval(80) > 1 THEN tcsr = small(last, tcsr + 1)
 dummy = usemenu(tcsr, 0, 0, last, 24)
 IF tcsr = 1 THEN
  IF keyval(75) > 1 AND thing > 0 THEN 
   GOSUB sstuf
   thing = thing - 1
   GOSUB lstuf
   GOSUB itstrsh
  END IF
  IF keyval(77) > 1 AND thing < 49 THEN
   GOSUB sstuf
   thing = thing + 1
   IF needaddset(thing, a(16), "Shop Thing") THEN
    flusharray b(), getbinsize(1) / 2 - 1, 0
    setpicstuf b(), getbinsize(1), -1
    storeset game$ + ".stf", pt * 50 + thing, 0
   END IF
   GOSUB lstuf
   GOSUB itstrsh
  END IF
 END IF
 IF tcsr = 2 THEN strgrabber thing$, 16
 IF tcsr > 2 THEN
  IF b(17) = 1 THEN
   '--using a hero
   min(19) = -1
   max(19) = 99
  ELSE
   '--not a hero
   min(19) = 0: max(19) = 3
  END IF
  SELECT CASE tcsr
   CASE 11 '--must trade in item 1 type
    IF zintgrabber(b(25), min(tcsr), max(tcsr), 75, 77) THEN GOSUB itstrsh
   CASE 13, 15, 17 '--must trade in item 2+ types
    IF zintgrabber(b(18 + tcsr), min(tcsr), max(tcsr), 75, 77) THEN GOSUB itstrsh
   CASE 12, 14, 16, 18 '--trade in item amounts
    b(18 + tcsr) = b(18 + tcsr) + 1
    dummy = intgrabber(b(18 + tcsr), min(tcsr), max(tcsr), 75, 77)
    b(18 + tcsr) = b(18 + tcsr) - 1
   CASE 19, 20 '--sell type, price
    dummy = intgrabber(b(7 + tcsr), min(tcsr), max(tcsr), 75, 77)
    IF (b(26) < 0 OR b(26) > 3) AND b(17) <> 1 THEN b(26) = 0
   CASE 21 '--trade in for
    IF zintgrabber(b(7 + tcsr), min(tcsr), max(tcsr), 75, 77) THEN GOSUB itstrsh
   CASE 22 '--trade in for amount
    b(7 + tcsr) = b(7 + tcsr) + 1
    dummy = intgrabber(b(7 + tcsr), min(tcsr), max(tcsr), 75, 77)
    b(7 + tcsr) = b(7 + tcsr) - 1
   CASE ELSE
    IF intgrabber(b(17 + tcsr - 3), min(tcsr), max(tcsr), 75, 77) THEN
     IF tcsr = 3 OR tcsr = 4 THEN
      GOSUB othertype
      GOSUB setdefault
     END IF
    END IF
  END SELECT
 END IF
 GOSUB othertype
 GOSUB stufmenu

 standardmenu smenu$(), last, 22, tcsr, 0, 0, 0, dpage, 0

 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

othertype:
SELECT CASE b(17)
 CASE 0
  last = 22
  max(4) = 254: IF b(18) > 255 THEN b(18) = 0
  max(19) = 3
 CASE 1
  last = 19
  max(4) = 59: IF b(18) > 59 THEN b(18) = 0
  max(19) = 99
 CASE 2
  last = 18
  max(4) = 999
END SELECT
'GOSUB itstrsh
RETRACE

setdefault:
IF b(17) = 0 THEN
'  setpicstuf buffer(), 200, -1
'  loadset game$ + ".itm", b(18), 0
'  thing$ = readbadbinstring$(buffer(), 0, 8, 0)
 'thing$ = ""
 'FOR o = 1 TO small(buffer(0), 10)
 ' IF buffer(o) < 256 AND buffer(o) > -1 THEN thing$ = thing$ + CHR$(buffer(o)) ELSE thing$ = ""
 'NEXT o
 thing$ = itemstr$(b(18),1,1)
 b(24) = buffer(46)
 b(27) = INT(buffer(46) / 2)
END IF
IF b(17) = 1 THEN
 thing$ = ""
 setpicstuf buffer(), 636, -1
 loadset game$ + ".dt0", b(18), 0
 FOR i = 1 TO small(buffer(0), 16)
  thing$ = thing$ + CHR$(buffer(i))
 NEXT i
END IF
IF b(17) = 2 THEN thing$ = "Unsupported"
RETRACE

stufmenu:
smenu$(1) = CHR$(27) + "Shop Thing" + XSTR$(thing) + " of" + XSTR$(a(16)) + CHR$(26)
smenu$(2) = "Name: " + thing$
smenu$(3) = "Type:" + XSTR$(b(17)) + "-" + stf$(bound(b(17), 0, 2))
smenu$(4) = "Number:" + XSTR$(b(18)) + " " + nit$
IF b(19) > 0 THEN
 smenu$(5) = "In Stock:" + XSTR$(b(19))
ELSE
 smenu$(5) = stf$(8 + bound(b(19), -1, 0))
END IF
smenu$(6) = "Buy Require Tag" + XSTR$(ABS(b(20))) + " =" + XSTR$(SGN(SGN(b(20)) + 1)) + " (" + lmnemonic$(ABS(b(20))) + ")"
IF b(20) = 1 THEN smenu$(6) = smenu$(6) + "[Never]"
IF b(20) = -1 THEN smenu$(6) = smenu$(6) + "[Always]"
IF b(20) = 0 THEN smenu$(6) = "[No Tag Check]"
smenu$(7) = "Sell Require Tag" + XSTR$(ABS(b(21))) + " =" + XSTR$(SGN(SGN(b(21)) + 1)) + " (" + lmnemonic$(ABS(b(21))) + ")"
IF b(21) = 1 THEN smenu$(7) = smenu$(7) + "[Never]"
IF b(21) = -1 THEN smenu$(7) = smenu$(7) + "[Always]"
IF b(21) = 0 THEN smenu$(7) = "[No Tag Check]"
smenu$(8) = "Buy Set Tag" + XSTR$(ABS(b(22))) + " =" + XSTR$(SGN(SGN(b(22)) + 1)) + " (" + lmnemonic$(ABS(b(22))) + ")"
IF b(22) = 1 THEN smenu$(8) = smenu$(8) + "[Unalterable]"
IF b(22) = -1 THEN smenu$(8) = smenu$(8) + "[Unalterable]"
IF b(22) = 0 THEN smenu$(8) = "[No Tag Set]"
smenu$(9) = "Sell Set Tag" + XSTR$(ABS(b(23))) + " =" + XSTR$(SGN(SGN(b(23)) + 1)) + " (" + lmnemonic$(ABS(b(23))) + ")"
IF b(23) = 1 THEN smenu$(9) = smenu$(9) + "[Unalterable]"
IF b(23) = -1 THEN smenu$(9) = smenu$(9) + "[Unalterable]"
IF b(23) = 0 THEN smenu$(9) = "[No Tag Set]"
smenu$(10) = names$(32) + " Price:" + XSTR$(b(24))
smenu$(11) = "Must Trade in" + XSTR$(b(30) + 1) + " of:" + tradestf$(0)
smenu$(12) = " (Change Amount)" 
smenu$(13) = "Must Trade in" + XSTR$(b(32) + 1) + " of:" + tradestf$(1)
smenu$(14) = " (Change Amount)" 
smenu$(15) = "Must Trade in" + XSTR$(b(34) + 1) + " of:" + tradestf$(2)
smenu$(16) = " (Change Amount)" 
smenu$(17) = "Must Trade in" + XSTR$(b(36) + 1) + " of:" + tradestf$(3)
smenu$(18) = " (Change Amount)" 
IF b(17) = 0 THEN
 smenu$(19) = "Sell type: " + stf$(bound(b(26), 0, 3) + 3)
 smenu$(20) = "Sell Price:" + XSTR$(b(27))
 smenu$(21) = "Trade in for" + XSTR$(b(29) + 1) + " of:" + trit$
 smenu$(22) = " (Change Amount)"
ELSE
 smenu$(19) = "Experience Level:" + XSTR$(b(26))
END IF
'--mutate menu for item/hero
RETRACE

lstuf:
flusharray b(), curbinsize(1) / 2, 0
setpicstuf b(), getbinsize(1), -1
loadset game$ + ".stf", pt * 50 + thing, 0
thing$ = readbadbinstring$(b(), 0, 16, 0)
'thing$ = ""
'FOR i = 1 TO bound(b(0), 0, 16)
' thing$ = thing$ + CHR$(bound(b(i), 0, 255))
'NEXT i
'---check for invalid data
IF b(17) < 0 OR b(17) > 2 THEN b(17) = 0
IF b(19) < -1 THEN b(19) = 0
IF (b(26) < 0 OR b(26) > 3) AND b(17) <> 1 THEN b(26) = 0
'--WIP Serendipity custom builds didn't flush shop records when upgrading properly
FOR i = 32 TO 42
 b(i) = large(b(i), 0)
NEXT
'--didn't see what this is for
'GOSUB menuup
RETRACE

sstuf:
b(0) = LEN(thing$)
FOR i = 1 TO small(b(0), 16)
 b(i) = ASC(MID$(thing$, i, 1))
NEXT i
setpicstuf b(), getbinsize(1), -1
storeset game$ + ".stf", pt * 50 + thing, 0
RETRACE

itstrsh:
tradestf$(0) = itemstr$(b(25),0,0)
tradestf$(1) = itemstr$(b(31),0,0)
tradestf$(2) = itemstr$(b(33),0,0)
tradestf$(3) = itemstr$(b(35),0,0)
trit$ = itemstr$(b(28),0,0)
RETRACE

END SUB

SUB smnemonic (tagname$, index)
DIM buf(20)
setpicstuf buf(), 42, -1

buf(0) = LEN(tagname$)
FOR i = 1 TO small(buf(0), 20)
 buf(i) = ASC(MID$(tagname$, i, 1))
NEXT i

storeset game$ + ".tmn", index, 0

END SUB

SUB touchfile (f$)

fh = FREEFILE
OPEN f$ FOR BINARY AS #fh
CLOSE #fh

END SUB

SUB upgrade (font())

DIM pal16(8)


IF general(genVersion) = 0 THEN
 general(genVersion) = 1
 clearpage vpage
 printstr "Flushing New Text Data...", 0, 0, vpage
 setvispage vpage 'refresh
 setpicstuf buffer(), 400, -1
 FOR o = 0 TO 999
  loadset game$ + ".say", o, 0
  temp$ = STRING$(68, 0)
  str2array temp$, buffer(), 331
  storeset game$ + ".say", o, 0
 NEXT o
END IF
IF general(genVersion) = 1 THEN
 general(genVersion) = 2
 clearpage vpage
 printstr "Updating Door Format...", 0, 0, vpage
 setvispage vpage 'refresh
 FOR o = 0 TO 19
  IF isfile(game$ + ".dor") THEN xbload game$ + ".dor", buffer(), "No doors"
  FOR i = 0 TO 299
   buffer(i) = buffer(o * 300 + i)
  NEXT i
  setpicstuf buffer(), 600, -1
  storeset game$ + ".dox", o, 0
 NEXT o
 printstr "Enforcing default font", 0, 16, vpage
 setvispage vpage 'refresh
 copyfile "ohrrpgce.fnt", game$ + ".fnt", buffer()
 xbload game$ + ".fnt", font(), "Font not loaded"
 setfont font()
 printstr "Making AniMaptiles Backward Compatable", 0, 16, vpage
 setvispage vpage 'refresh
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
  savetanim i, buffer()
 NEXT i
 FOR i = 0 TO general(0)
  printstr " map" + XSTR$(i), 16, 24 + i * 8, vpage
  XBLOAD maplumpname$(i, "t"), buffer(), "Map not loaded"
  setmapdata buffer(), buffer(), 0, 0
  FOR tx = 0 TO buffer(0)
   FOR ty = 0 TO buffer(1)
    IF readmapblock(tx, ty) = 158 THEN setmapblock tx, ty, 206
   NEXT ty
  NEXT tx
  xbsave maplumpname$(i, "t"), buffer(), buffer(0) * buffer(1) + 4
 NEXT i
END IF
'---VERSION 3---
IF general(genVersion) = 2 THEN
 general(genVersion) = 3
 clearpage vpage
 '-get old-old password
 rpas$ = ""
 FOR i = 1 TO general(99)
  IF general(4 + i) >= 0 AND general(4 + i) <= 255 THEN rpas$ = rpas$ + CHR$(loopvar(general(4 + i), 0, 255, general(98) * -1))
 NEXT i
 '-SET (obsolete) SCATTERTABLE BASE
 general(199) = INT(RND * 15) + 1
 '-WRITE PASSWORD INTO (obsolete) SCATTERTABLE
 general(93) = INT(RND * 250) + 1
 rpas$ = rotascii(rpas$, general(93))
 '--write old password (will be upgraded again later in this same routine)
 writescatter rpas$, general(94), 200
 '-REPLACE OLD-OLD PASSWORD
 pas$ = rotascii("ufxx|twi%|fx%rt{ji", -5)
 general(99) = LEN(pas$)
 general(98) = INT(RND * 250) + 1
 FOR i = 1 TO general(99)
  temp = ASC(MID$(pas$, i, 1))
  general(4 + i) = loopvar(temp, 0, 255, general(98))
 NEXT i
 printstr "Data Scaling Shtuff...", 0, 0, vpage
 setvispage vpage 'refresh
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
'--VERSION 4--
IF general(genVersion) = 3 THEN
 general(genVersion) = 4
 clearpage vpage
 printstr "Clearing New Attack Bitsets...", 0, 0, vpage
 setvispage vpage 'refresh
 setpicstuf buffer(), 80, -1
 FOR o = 0 TO general(34)
  loadset game$ + ".dt6", o, 0
  buffer(18) = 0
  IF readbit(buffer(), 20, 60) THEN buffer(18) = 1
  setbit buffer(), 20, 2, 0
  FOR i = 21 TO 58
   setbit buffer(), 20, i, 0
  NEXT i
  FOR i = 60 TO 63
   setbit buffer(), 20, i, 0
  NEXT i
  storeset game$ + ".dt6", o, 0
 NEXT o
 setbit general(), 101, 6, 0 'no hide readymeter
 setbit general(), 101, 7, 0 'no hide health meter
END IF
'--VERSION 5--
IF general(genVersion) = 4 THEN
 general(genVersion) = 5
 clearpage vpage
 printstr "Upgrading 16-color Palette Format...", 0, 0, vpage
 setvispage vpage 'refresh
 setpicstuf pal16(), 16, -1
 xbload game$ + ".pal", buffer(), "16-color palletes missing from " + game$
 KILL game$ + ".pal"
 '--find last used palette
 last = 99
 foundpal = 0
 FOR j = 99 TO 0 STEP -1
  FOR i = 0 TO 7
   IF buffer(j * 8 + i) <> 0 THEN
    last = j
    foundpal = 1
    EXIT FOR
   END IF
  NEXT i
  IF foundpal THEN EXIT FOR
 NEXT j
 printstr "Last used palette is" + XSTR$(last), 0, 8, vpage
 setvispage vpage 'refresh
 '--write header
 pal16(0) = 4444
 pal16(1) = last
 FOR i = 2 TO 7
  pal16(i) = 0
 NEXT i
 storeset game$ + ".pal", 0, 0
 '--convert palettes
 FOR j = 0 TO last
  FOR i = 0 TO 7
   pal16(i) = buffer(j * 8 + i)
  NEXT i
  storeset game$ + ".pal", 1 + j, 0
 NEXT j
END IF
'--VERSION 6--
IF general(genVersion) = 5 THEN
 'Shop stuff and song name formats changed, MIDI music added
 'Sub version info also added
 general(genVersion) = 6
END IF 


IF NOT isfile(workingdir$ + SLASH + "archinym.lmp") THEN
 '--create archinym information lump
 fh = FREEFILE
 OPEN workingdir$ + SLASH + "archinym.lmp" FOR OUTPUT AS #fh
 PRINT #fh, RIGHT$(game$, LEN(game$) - LEN(workingdir$ + SLASH))
 PRINT #fh, version$ + "(previous)"
 CLOSE #fh
END IF

IF NOT isfile(game$ + ".veh") THEN
 '--make sure vehicle lump is present
 IF isfile("ohrrpgce.new") THEN
  IF unlumpone("ohrrpgce.new", "ohrrpgce.veh", game$ + ".veh") THEN
   general(55) = 2
  END IF
 END IF
END IF

'--make sure binsize.bin is full. why are we doing this? as lumps are upgraded
'--and binsize is extended, records in binsize which are meant to default
'--because they don't exist take on the value 0 instead
FOR i = 0 TO sizebinsize
 setbinsize i, getbinsize(i)
NEXT

IF NOT isfile(workingdir$ + SLASH + "attack.bin") THEN
 clearpage vpage
 printstr "Init extended attack data...", 0, 0, vpage
 setvispage vpage 'refresh
 flusharray buffer(), curbinsize(0) / 2, 0
 setbinsize 0, curbinsize(0)
 setpicstuf buffer(), curbinsize(0), -1
 FOR i = 0 TO general(34)
  storeset workingdir$ + SLASH + "attack.bin", i, 0
 NEXT i

 '--and while we are at it, clear the old death-string from enemies
 printstr "Re-init recycled enemy data...", 0, 10, vpage
 setvispage vpage 'refresh
 setpicstuf buffer(), 320, -1
 FOR i = 0 TO general(36)
  loadset game$ + ".dt1", i, 0
  FOR j = 17 TO 52
   buffer(j) = 0
  NEXT j
  storeset game$ + ".dt1", i, 0
 NEXT i
END IF

IF NOT isfile(workingdir$ + SLASH + "songdata.bin") THEN
 printstr "Upgrading Song Name format...", 0, 0, vpage
 setvispage vpage 'refresh
 DIM song$(99)
 fh = FREEFILE
 OPEN game$ + ".sng" FOR BINARY AS #fh
 temp& = LOF(fh)
 CLOSE #fh
 IF temp& > 0 THEN
  fh = FREEFILE
  OPEN game$ + ".sng" FOR INPUT AS #fh
  FOR i = 0 TO 99
   INPUT #fh, song$(i)
  NEXT i
  CLOSE #fh
 END IF
 
 FOR i = 99 TO 1 STEP -1
  '-- check for midis as well 'cause some people might use a WIP custom or whatnot
  IF song$(i) <> "" OR isfile(game$ + "." + STR$(i)) OR isfile(workingdir$ + SLASH + "song" + STR$(i) + ".mid") THEN
   general(genMaxSong) = i
   EXIT FOR
  END IF
 NEXT

 flusharray buffer(), curbinsize(2) / 2, 0
 setbinsize 2, curbinsize(2)
 setpicstuf buffer(), curbinsize(2), -1
 FOR i = 0 TO general(genMaxSong)
  writebinstring song$(i), buffer(), 0, 30
  storeset workingdir$ + SLASH + "songdata.bin", i, 0
 NEXT
 ERASE song$
END IF



'--check variable record size lumps and reoutput them if records have been extended
'--all of the files below should exist, be non zero length and have non zero record size by this point
updaterecordlength workingdir$ + SLASH + "attack.bin", 0
updaterecordlength game$ + ".stf", 1
updaterecordlength workingdir$ + SLASH + "songdata.bin", 2

'--update to new (3rd) password format
IF general(5) < 256 THEN
 general(5) = 256
 IF general(94) = -1 THEN
  '--no password, write a blank one
  pas$ = ""
 ELSE
  '--read the old scattertable
  readscatter pas$, general(94), 200
  pas$ = rotascii(pas$, general(93) * -1)
 END IF
 writepassword pas$
END IF

'Zero out new attack item cost (ammunition) data
IF getfixbit(fixAttackitems) = 0 THEN
  setfixbit(fixAttackitems, 1)
  fh = freefile
  OPEN workingdir$ + SLASH + "attack.bin" FOR BINARY AS #FH
  REDIM dat(curbinsize(0)/2 - 1) AS SHORT
  p = 1
  FOR i = 0 to general(genMaxAttack)
    
    GET #fh,p,dat()
    FOR y = 53 TO 59
      dat(y) = 0
    NEXT
    
    PUT #fh,p,dat()
    p+=curbinsize(0)
  NEXT
  CLOSE #fh
END IF

'wow! this is quite a big and ugly routine!
END SUB

SUB writepassword (p$)

'-- set password version number (only if needed)
IF general(5) < 256 THEN general(5) = 256

'--pad the password with some silly obfuscating low-ascii chars
FOR i = 1 TO 17 - LEN(p$)
 IF INT(RND * 10) < 5 THEN
  p$ = p$ + CHR$(INT(RND * 30))
 ELSE
  p$ = CHR$(INT(RND * 30)) + p$
 END IF
NEXT i

'--apply a new ascii rotation / weak obfuscation number
general(6) = INT(RND * 253) + 1
p$ = rotascii(p$, general(6))

'--write the password into GEN
str2array p$, general(), 14

END SUB

FUNCTION newRPGfile (template$, newrpg$)
 newRPGfile = 0 ' default return value 0 means failure
 IF newrpg$ = "" THEN EXIT FUNCTION
 textcolor 6, 0
 printstr "Please Wait...", 0, 40, vpage
 printstr "Creating RPG File", 0, 50, vpage
 IF NOT isfile(template$) THEN
  printstr "Error: " + template$ + " not found", 0, 60, vpage
  printstr "Press Enter to quit", 0, 70, vpage
  w = getkey
  EXIT FUNCTION
 END IF
 copyfile template$, newrpg$, buffer()
 printstr "Unlumping", 0, 60, vpage
 setvispage vpage 'refresh
 DIM lumpbuf(16383)
 unlump newrpg$, workingdir$ + SLASH, lumpbuf()
 ERASE lumpbuf
 '--create archinym information lump
 fh = FREEFILE
 OPEN workingdir$ + SLASH + "archinym.lmp" FOR OUTPUT AS #fh
 PRINT #fh, "ohrrpgce"
 PRINT #fh, version$
 CLOSE #fh
 printstr "Finalumping", 0, 80, vpage
 setvispage vpage 'refresh
 '--re-lump files as NEW rpg file
 dolumpfiles newrpg$
 newRPGfile = -1 'return true for success
END FUNCTION

SUB dolumpfiles (filetolump$)
'--build the list of files to lump
findfiles workingdir$ + SLASH + ALLFILES, 0, "temp.lst", buffer()
fixorder "temp.lst"
IF isdir(filetolump$) THEN
 '---copy changed files back to source rpgdir---
 fh = FREEFILE
 OPEN "temp.lst" FOR INPUT AS #fh
 unsafefile$ = "RPG lump copy failed!"
 DO UNTIL EOF(fh)
  LINE INPUT #fh, filename$
  safekill filetolump$ + SLASH + filename$
  copyfile workingdir$ + SLASH + filename$, filetolump$ + SLASH + filename$, buffer()
 LOOP
 CLOSE #fh
 safekill "temp.lst"
 unsafefile$ = ""
ELSE
 '---relump data into lumpfile package---
 DIM lumpbuf(16383)
 unsafefile$ = "RPG lumping failed!"
 lumpfiles "temp.lst", filetolump$, workingdir$ + SLASH, lumpbuf()
 safekill "temp.lst"
 unsafefile$ = ""
 ERASE lumpbuf
END IF
END SUB

FUNCTION readarchinym$ ()
 IF isfile(workingdir$ + SLASH + "archinym.lmp") THEN
  fh = FREEFILE
  OPEN workingdir$ + SLASH + "archinym.lmp" FOR INPUT AS #fh
  LINE INPUT #fh, a$
  CLOSE #fh
  a$ = LCASE$(a$)
  readarchinym$ = a$
 ELSE
  ' for backwards compatability with ancient games that lack archinym.lmp
  debug workingdir$ + SLASH + "archinym.lmp" + " unreadable, using " + LCASE$(trimpath$(game$)) + " instead"
  readarchinym$ = LCASE$(trimpath$(game$))
 END IF 
END FUNCTION
