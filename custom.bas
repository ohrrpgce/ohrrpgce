'OHRRPGCE CUSTOM - Main module
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z

#include "udts.bi"

'basic subs and functions
DECLARE SUB fixfilename (s$)
DECLARE FUNCTION filenum$ (n%)
DECLARE SUB standardmenu (menu$(), size%, vis%, pt%, top%, x%, y%, page%, edge%)
DECLARE SUB writeglobalstring (index%, s$, maxlen%)
DECLARE FUNCTION readglobalstring$ (index%, default$, maxlen%)
DECLARE SUB importbmp (f$, cap$, count%)
DECLARE SUB loadpasdefaults (array%(), tilesetnum%)
DECLARE SUB textxbload (f$, array%(), e$)
DECLARE SUB fixorder (f$)
DECLARE SUB vehicles ()
DECLARE SUB verifyrpg ()
DECLARE FUNCTION numbertail$ (s$)
DECLARE SUB cropafter (index%, limit%, flushafter%, lump$, bytes%, prompt%)
DECLARE SUB scriptman ()
DECLARE FUNCTION exclude$ (s$, x$)
DECLARE FUNCTION exclusive$ (s$, x$)
DECLARE SUB fontedit (font%())
DECLARE SUB cycletile (cycle%(), tastuf%(), pt%(), skip%())
DECLARE SUB testanimpattern (tastuf%(), taset%)
DECLARE FUNCTION onoroff$ (n%)
DECLARE FUNCTION lmnemonic$ (index%)
DECLARE SUB smnemonic (tagname$, index%)
DECLARE SUB tagnames ()
DECLARE SUB sizemar (array%(), wide%, high%, tempx%, tempy%, tempw%, temph%, yout%, page%)
DECLARE SUB drawmini (high%, wide%, cursor%(), page%, tastuf%())
DECLARE SUB mapmaker (font%(), npcn%(), npcstat%())
DECLARE SUB npcdef (npcn%(), pt%)
DECLARE SUB editbitset (array%(), wof%, last%, names$())
DECLARE SUB sprite (xw%, yw%, sets%, perset%, soff%, foff%, atatime%, info$(), size%, zoom%, fileset%, font%())
DECLARE FUNCTION needaddset (pt%, check%, what$)
DECLARE SUB shopdata ()
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
DECLARE SUB menu_editor ()
DECLARE FUNCTION sublist% (num%, s$())
DECLARE SUB maptile (font())
DECLARE FUNCTION itemstr$(it%,hiden%,offbyone%)
DECLARE FUNCTION inputfilename$ (query$, ext$, default$ = "")
DECLARE FUNCTION newRPGfile (template$, newrpg$)
DECLARE SUB dolumpfiles (filetolump$)
DECLARE FUNCTION readarchinym$ ()
DECLARE SUB importscripts (f$)
DECLARE FUNCTION scriptbrowse$ (trigger%, triggertype%, scrtype$)
DECLARE FUNCTION scrintgrabber (n%, BYVAL min%, BYVAL max%, BYVAL less%, BYVAL more%, scriptside%, triggertype%)
DECLARE SUB move_unwritable_rpg(BYREF filetolump$)

#include "compat.bi"
#include "allmodex.bi"
#include "common.bi"
#include "cglobals.bi"

#include "const.bi"
#include "uiconst.bi"
#include "scrconst.bi"

DIM exename$
exename$ = trimextension$(trimpath$(COMMAND$(0)))

DIM tmpdir$
DIM homedir$
'why do we use different temp dirs in game and custom?
#IFDEF __FB_LINUX__
homedir$ = ENVIRON$("HOME")
tmpdir$ = homedir$ + SLASH + ".ohrrpgce" + SLASH
IF NOT isdir(tmpdir$) THEN makedir tmpdir$
#ELSE
'Custom on Windows works in the current dir
homedir$ = ENVIRON$("USERPROFILE") & SLASH & "My Documents" 'Is My Documents called something else for non-English versions of Windows?
tmpdir$ = exepath$ + SLASH
#ENDIF

IF fileiswriteable(exepath$ + SLASH + "writetest.tmp") THEN
 'When CUSTOM is installed read-write, work in CUSTOM's folder
 safekill exepath$ + SLASH + "writetest.tmp"
 CHDIR exepath$ 'Note that exepath$ is a FreeBasic builtin, and not derived from the above exename$
ELSE
 'If CUSTOM is installed read-only, use your home dir as the default
 CHDIR homedir$
 #IFNDEF __FB_LINUX__
  'under Windows, need a new tmpdir$ too
  tmpdir$ = homedir$ + SLASH
 #ENDIF
END IF

workingdir$ = tmpdir$ & "working.tmp"

processcommandline

DIM font(1024), buffer(16384), timing(4), joy(4)
DIM menu$(22), gen(360), keyv(55, 3), rpg$(255), hinfo$(7), einfo$(0), ainfo$(2), xinfo$(1), winfo$(7), npcn(1500), npcstat(1500), uilook(uiColors)
DIM master(255) as RGBcolor
'more global variables
DIM game$, gamefile$, insert, activepalette
DIM vpage, dpage, fadestate, workingdir$


dim shared trit as string 'to fix an undefined variable error

RANDOMIZE TIMER

palfile$ = finddatafile("ohrrpgce.mas")
IF palfile$ <> "" THEN
 textxbload palfile$, buffer(), "default master palette ohrrpgce.mas is missing"
 convertpalette buffer(), master()
ELSE
 FOR i = 1 TO 15
  master(i).r = SGN(i AND 4) * 168 + SGN(i AND 8) * 87
  master(i).g = SGN(i AND 2) * 168 + SGN(i AND 8) * 87
  master(i).b = SGN(i AND 1) * 168 + SGN(i AND 8) * 87
 NEXT i
END IF
getui uilook()

getdefaultfont font()

setmodex
setpal master()
setfont font()
setdiskpages buffer(), 200, 0
textcolor 15, 0
GOSUB readstuff

dpage = 1: vpage = 0: Rate = 160
game$ = ""
gamefile$ = ""
hsfile$ = ""

GOSUB makeworkingdir
FOR i = 1 TO commandlineargcount
 cmdline$ = commandlinearg$(i)

 IF isfile(cmdline$) = 0 AND isdir(cmdline$) = 0 THEN
  centerbox 160, 40, 300, 50, 3, 0
  edgeprint "File not found:", 15, 30, uilook(uiText), 0
  edgeprint RIGHT$(cmdline$,35), 15, 40, uilook(uiText), 0
  setvispage 0
  w = getkey
  CONTINUE FOR
 END IF
 IF LCASE$(justextension$(cmdline$)) = "hs" AND isfile(cmdline$) THEN
  hsfile$ = cmdline$
  CONTINUE FOR
 END IF

 IF (LCASE$(justextension$(cmdline$)) = "rpg" AND isfile(cmdline$)) OR isdir(cmdline$) THEN
  gamefile$ = cmdline$
  game$ = trimextension$(trimpath$(gamefile$))
 END IF
NEXT
IF game$ = "" THEN
 hsfile$ = ""
 GOSUB chooserpg
END IF
setwindowtitle "OHRRPGCE - " + gamefile$

GOSUB checkpass

clearpage 0
setvispage 0
textcolor 15, 0
printstr "UNLUMPING DATA: please wait.", 0, 0, 0
setvispage 0

touchfile workingdir$ + SLASH + "__danger.tmp"

IF isdir(gamefile$) THEN
 'work on an unlumped RPG file
 findfiles gamefile$ + SLASH + ALLFILES, 0, "filelist.tmp", buffer()
 fh = FREEFILE
 OPEN "filelist.tmp" FOR INPUT AS #fh
 DO UNTIL EOF(fh)
  LINE INPUT #fh, filename$
  copyfile gamefile$ + SLASH + filename$, workingdir$ + SLASH + filename$
 LOOP
 CLOSE #fh
 KILL "filelist.tmp"
ELSE
 DIM lumpbuf(16383)
 unlump gamefile$, workingdir$ + SLASH, lumpbuf()
 ERASE lumpbuf
END IF
game$ = workingdir$ + SLASH + game$
verifyrpg
safekill workingdir$ + SLASH + "__danger.tmp"

IF hsfile$ <> "" THEN GOTO hsimport

IF NOT isfile(game$ + ".mas") AND NOT isfile(workingdir$ + SLASH + "palettes.bin") THEN 
 palfile$ = finddatafile("ohrrpgce.mas")
 IF palfile$ = "" THEN fatalerror "RPG master palette and ohrrpgce.mas missing"
 copyfile palfile$, game$ + ".mas"
END IF
IF NOT isfile(game$ + ".fnt") THEN
 getdefaultfont font()
 xbsave game$ + ".fnt", font(), 2048
END IF
xbload game$ + ".fnt", font(), "Font not loaded"
'--loadgen, upgrade, resave
xbload game$ + ".gen", gen(), "general data is missing, RPG file corruption is likely"
activepalette = gen(genMasterPal)
loadpalette master(), activepalette
setpal master()
getui uilook(), activepalette
upgrade font()
setfont font()

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
 usemenu pt, 0, 0, mainmax, 24
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  SELECT CASE menumode
   CASE 0'--normal mode
    IF pt = 0 THEN pt = 0: menumode = 1: GOSUB setgraphicmenu
    IF pt = 1 THEN mapmaker font(), npcn(), npcstat()
    IF pt = 2 THEN statname
    IF pt = 3 THEN herodata
    IF pt = 4 THEN enemydata
    IF pt = 5 THEN attackdata
    IF pt = 6 THEN itemdata
    IF pt = 7 THEN shopdata
    IF pt = 8 THEN formation
    IF pt = 9 THEN textage
    if pt = 10 then menu_editor
    IF pt = 11 THEN vehicles
    IF pt = 12 THEN tagnames
    IF pt = 13 THEN importsong
    IF pt = 14 THEN importsfx
    IF pt = 15 THEN fontedit font()
    IF pt = 16 THEN gendata
    IF pt = 17 THEN scriptman
    IF pt = 18 THEN
     GOSUB relump
     IF quitnow > 1 THEN GOTO finis
    END IF
   CASE 1'--graphics mode
    IF pt = 0 THEN pt = 0: menumode = 0: GOSUB setmainmenu
    IF pt = 1 THEN maptile font()
    IF pt = 2 THEN sprite 20, 20, gen(genMaxNPCPic),    8, 5, 0, 7, winfo$(), 200, 4, 4, font()
    IF pt = 3 THEN sprite 32, 40, gen(genMaxHeroPic),   8, 16, 0, 3, hinfo$(), 640, 4, 0, font()
    IF pt = 4 THEN sprite 34, 34, gen(genMaxEnemy1Pic), 1, 2, 0, 4, einfo$(), 578, 4, 1, font()
    IF pt = 5 THEN sprite 50, 50, gen(genMaxEnemy2Pic), 1, 4, 1, 2, einfo$(), 1250, 2, 2, font()
    IF pt = 6 THEN sprite 80, 80, gen(genMaxEnemy3Pic), 1, 10, 2, 1, einfo$(), 3200, 2, 3, font()
    IF pt = 7 THEN sprite 50, 50, gen(genMaxAttackPic), 3, 12, 0, 2, ainfo$(), 1250, 2, 6, font()
    IF pt = 8 THEN sprite 24, 24, gen(genMaxWeaponPic), 2, 2, 0, 5, xinfo$(), 288, 4, 5, font()
    IF pt = 9 THEN importbmp ".mxs", "screen", gen(100)
    IF pt = 10 THEN
     gen(33) = gen(33) + 1
     importbmp ".til", "tileset", gen(33)
     gen(33) = gen(33) - 1
    END IF
  END SELECT
  '--always resave the .GEN lump after any menu
  xbsave game$ + ".gen", gen(), 1000
 END IF

 standardmenu menu$(), mainmax, 22, pt, 0, 0, 0, dpage, 0

 textcolor 6, 0
 printstr version_code$, 0, 184, dpage
 printstr version_build$, 0, 192, dpage

 SWAP vpage, dpage
 setvispage vpage
 copypage 2, dpage
 dowait
LOOP

setmainmenu:
mainmax = 18
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
menu$(10) = "Edit Menus"
menu$(11) = "Edit Vehicles"
menu$(12) = "Edit Tag Names"
menu$(13) = "Import Music"
menu$(14) = "Import Sound Effects"
menu$(15) = "Edit Font"
menu$(16) = "Edit General Game Data"
menu$(17) = "Script Management"
menu$(18) = "Quit Editing"
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
menu$(9) = "Import/Export Screens"
menu$(10) = "Import/Export Full Maptile Sets"
RETRACE

chooserpg:
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
 usemenu csr, top, 0, last, 20
 IF keyval(57) > 1 OR keyval(28) > 1 THEN
  IF csr = 0 THEN
   game$ = inputfilename$("Filename of New Game?", ".rpg")
   IF game$ <> "" THEN
     IF NOT newRPGfile(finddatafile("ohrrpgce.new"), game$ + ".rpg") THEN GOTO finis
     gamefile$ = game$ + ".rpg"
     EXIT DO
   END IF
  ELSEIF csr = 1 THEN
   gamefile$ = browse$(7, "", "*.rpg", tmpdir$, 0)
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
 usemenu temp, 0, 0, 2, 2
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
RETRACE

relump:
xbsave game$ + ".gen", gen(), 1000
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
copylump gamefile$, "archinym.lmp", workingdir$, -1
'--set game$ according to the archinym
game$ = readarchinym()
copylump gamefile$, game$ + ".gen", workingdir$
xbload workingdir$ + SLASH + game$ + ".gen", gen(), "general data is missing, RPG file corruption is likely"
'----load password-----
IF gen(5) >= 256 THEN
 '--new format password
 rpas$ = readpassword$
ELSE
 '--old scattertable format
 IF gen(94) = -1 THEN RETRACE 'this is stupid
 readscatter rpas$, gen(94), 200
 rpas$ = rotascii(rpas$, gen(93) * -1)
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

hsimport:
xbload game$ + ".gen", gen(), "general data is missing, RPG file corruption is likely"
upgrade font() 'needed because it has not already happened because we are doing command-line import
importscripts hsfile$
xbsave game$ + ".gen", gen(), 1000
GOSUB dorelump
GOSUB cleanupfiles
CHDIR curdir$
restoremode
SYSTEM

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
END

cleanupfiles:
IF nocleanup = 0 THEN
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

copyfile f$, "fixorder.tmp"

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
  CASE "archinym.lmp", "browse.txt"
   '--do nothing
  CASE ELSE
   '--check extenstion
   c$ = RIGHT$(b$, 4)
   SELECT CASE c$
    CASE ".tmp"
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

SUB fontedit (font())

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
   usemenu menuptr, 0, 0, 3, 22
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
 copyfile newfont$, game$ + ".fnt"

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
  copyfile game$ + ".fnt", newfont$ + ".ohf"
  EXIT DO
 END IF

 textcolor 7, 0
 printstr "Input a filename to save to", 0, 0, dpage
 printstr "[" + newfont$ + ".ohf]", 0, 8, dpage
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
boxwidth = large(160, LEN(what$) * 8 + 88)
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
  rectangle 160 - boxwidth \ 2, 10, boxwidth, 40, 1, dpage
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

SUB shopdata
DIM names$(32), a(20), b(curbinsize(1) / 2), menu$(24), smenu$(24), max(24), min(24), sbit$(-1 TO 10), stf$(16), tradestf$(3)
DIM her AS HeroDef ' only used in getdefaultthingname

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
 IF keyval(29) > 0 AND keyval(14) THEN cropafter pt, gen(97), 0, game$ + ".sho", 40, 1: GOSUB menugen
 usemenu csr, 0, 0, li, 24
 IF csr = 1 THEN
  IF pt = gen(97) AND keyval(77) > 1 THEN
   GOSUB sshopset
   pt = pt + 1
   IF needaddset(pt, gen(genMaxShop), "Shop") THEN
    flusharray a(), 19, 0
    setpicstuf a(), 40, -1
    storeset game$ + ".sho", pt, 0
   END IF
   GOSUB lshopset
  END IF
  newpt = pt
  IF intgrabber(newpt, 0, gen(genMaxShop)) THEN
   GOSUB sshopset
   pt = newpt
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
  IF csr = 6 THEN
   menu$(6) = "Inn Script: " + scriptbrowse$(a(19), plottrigger, "Inn Plotscript")
  END IF
 END IF
 IF csr = 5 THEN
  IF intgrabber(a(18), 0, 32767) THEN GOSUB menuup
 END IF
 IF csr = 6 THEN
  IF scrintgrabber(a(19), 0, 0, 75, 77, 1, plottrigger) THEN GOSUB menuup
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
menu$(1) = CHR$(27) + " Shop" + XSTR$(pt) + " of" + XSTR$(gen(97)) + CHR$(26)
menu$(2) = "Name:" + sn$
menu$(5) = "Inn Price:" + XSTR$(a(18))
IF readbit(a(), 17, 3) = 0 THEN menu$(5) = "Inn Price: N/A"
menu$(6) = "Inn Script: " + scriptname$(a(19), plottrigger)
IF readbit(a(), 17, 0) OR readbit(a(), 17, 1) OR readbit(a(), 17, 2) THEN havestuf = 1 ELSE havestuf = 0
RETRACE

shopstuf:
thing = 0
defaultthing$ = ""
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
 usemenu tcsr, 0, 0, last, 24
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
    IF zintgrabber(b(25), min(tcsr), max(tcsr)) THEN GOSUB itstrsh
   CASE 13, 15, 17 '--must trade in item 2+ types
    IF zintgrabber(b(18 + tcsr), min(tcsr), max(tcsr)) THEN GOSUB itstrsh
   CASE 12, 14, 16, 18 '--trade in item amounts
    b(18 + tcsr) = b(18 + tcsr) + 1
    intgrabber(b(18 + tcsr), min(tcsr), max(tcsr))
    b(18 + tcsr) = b(18 + tcsr) - 1
   CASE 19, 20 '--sell type, price
    intgrabber(b(7 + tcsr), min(tcsr), max(tcsr))
    IF (b(26) < 0 OR b(26) > 3) AND b(17) <> 1 THEN b(26) = 0
   CASE 21 '--trade in for
    IF zintgrabber(b(7 + tcsr), min(tcsr), max(tcsr)) THEN GOSUB itstrsh
   CASE 22 '--trade in for amount
    b(7 + tcsr) = b(7 + tcsr) + 1
    intgrabber(b(7 + tcsr), min(tcsr), max(tcsr))
    b(7 + tcsr) = b(7 + tcsr) - 1
   CASE ELSE
    IF intgrabber(b(17 + tcsr - 3), min(tcsr), max(tcsr)) THEN
     IF tcsr = 3 OR tcsr = 4 THEN
      GOSUB othertype
      GOSUB getdefaultthingname
      thing$ = defaultthing$
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
RETRACE

getdefaultthingname:
'"DIM her AS HeroDef" is only used here but appears at the top of shopdata sub to avoid branch crossing warning
IF b(17) = 0 THEN
 defaultthing$ = itemstr$(b(18),1,1)
 b(24) = buffer(46)
 b(27) = INT(buffer(46) / 2)
END IF
IF b(17) = 1 THEN
 defaultthing$ = ""
 loadherodata @her, b(18)
 defaultthing$ = defaultthing$ + her.name
END IF
IF b(17) = 2 THEN defaultthing$ = "Unsupported"
RETRACE

stufmenu:
smenu$(1) = CHR$(27) + "Shop Thing" + XSTR$(thing) + " of" + XSTR$(a(16)) + CHR$(26)
smenu$(2) = "Name: " + thing$
smenu$(3) = "Type:" + XSTR$(b(17)) + "-" + stf$(bound(b(17), 0, 2))
smenu$(4) = "Number:" + XSTR$(b(18)) + " " + defaultthing$
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
'---check for invalid data
IF b(17) < 0 OR b(17) > 2 THEN b(17) = 0
IF b(19) < -1 THEN b(19) = 0
IF (b(26) < 0 OR b(26) > 3) AND b(17) <> 1 THEN b(26) = 0
'--WIP Serendipity custom builds didn't flush shop records when upgrading properly
FOR i = 32 TO 42
 b(i) = large(b(i), 0)
NEXT
GOSUB getdefaultthingname
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

FUNCTION newRPGfile (template$, newrpg$)
 newRPGfile = 0 ' default return value 0 means failure
 IF newrpg$ = "" THEN EXIT FUNCTION
 textcolor 6, 0
 printstr "Please Wait...", 0, 40, vpage
 printstr "Creating RPG File", 0, 50, vpage
 setvispage vpage
 IF NOT isfile(template$) THEN
  printstr "Error: ohrrpgce.new not found", 0, 60, vpage
  printstr "Press Enter to quit", 0, 70, vpage
 setvispage vpage
  w = getkey
  EXIT FUNCTION
 END IF
 copyfile template$, newrpg$
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
 IF NOT fileiswriteable(filetolump$ & SLASH & "archinym.lmp") THEN
  move_unwritable_rpg filetolump$
  makedir filetolump$
 END IF
 fh = FREEFILE
 OPEN "temp.lst" FOR INPUT AS #fh
 DO UNTIL EOF(fh)
  LINE INPUT #fh, filename$
  safekill filetolump$ + SLASH + filename$
  copyfile workingdir$ + SLASH + filename$, filetolump$ + SLASH + filename$
 LOOP
 CLOSE #fh
 safekill "temp.lst"
ELSE
 '---relump data into lumpfile package---
 IF NOT fileiswriteable(filetolump$) THEN
  move_unwritable_rpg filetolump$
 END IF
 DIM lumpbuf(16383)
 lumpfiles "temp.lst", filetolump$, workingdir$ + SLASH, lumpbuf()
 safekill "temp.lst"
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

SUB move_unwritable_rpg(BYREF filetolump$)
 clearpage vpage
 centerbox 160, 100, 310, 50, 3, vpage
 edgeprint filetolump$, 10, 80, 15, vpage
 edgeprint "is not writable. Saving to:", 10, 90, 15, vpage
 filetolump$ = homedir$ & SLASH & trimpath$(filetolump$)
 edgeprint filetolump$, 10, 100, 15, vpage
 edgeprint "[Press Any Key]", 10, 110, 15, vpage
 setvispage vpage
 w = getkey
END SUB
