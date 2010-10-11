'OHRRPGCE CUSTOM - Main module
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
DEFINT A-Z

#include "compat.bi"
#include "udts.bi"
#include "const.bi"

'basic subs and functions
DECLARE SUB importbmp (f AS STRING, cap AS STRING, count AS INTEGER)

'the below functions require cleaning
DECLARE SUB vehicles ()
DECLARE SUB verifyrpg ()
DECLARE SUB scriptman ()
DECLARE SUB map_picker ()
DECLARE SUB sprite (xw, yw, sets, perset, soff, info$(), zoom, fileset, font(), fullset AS INTEGER=NO, cursor_start AS INTEGER=0, cursor_top AS INTEGER=0)
DECLARE SUB shopdata ()
DECLARE SUB importsong ()
DECLARE SUB importsfx ()
DECLARE SUB gendata ()
DECLARE SUB itemdata ()
DECLARE SUB formation ()
DECLARE SUB enemydata ()
DECLARE SUB herodata ()
DECLARE SUB attackdata ()
DECLARE SUB text_box_editor ()
DECLARE SUB menu_editor ()
DECLARE SUB maptile (font())
DECLARE FUNCTION newRPGfile (templatefile$, newrpg$)
DECLARE SUB dolumpfiles (filetolump$)
DECLARE FUNCTION readarchinym$ ()
DECLARE SUB importscripts (f$)
DECLARE SUB move_unwritable_rpg(BYREF filetolump$)

#include "allmodex.bi"
#include "common.bi"
#include "loading.bi"
#include "customsubs.bi"
#include "slices.bi"
#include "cglobals.bi"
#include "uiconst.bi"
#include "scrconst.bi"
#include "sliceedit.bi"
#include "reloadedit.bi"

DIM exename as string
exename = trimextension$(trimpath$(COMMAND$(0)))

DIM tmpdir as string
DIM homedir as string
'why do we use different temp dirs in game and custom?
set_homedir

#IFDEF __FB_DARWIN__
 'Bundled apps have starting current directory equal to the location of the bundle, but exepath points inside
 IF RIGHT(exepath, 19) = ".app/Contents/MacOS" THEN
  data_dir = parentdir(exepath, 1) + "Resources"
 END IF
#ENDIF

orig_dir = CURDIR()

'temporarily set current directory, will be changed to game directory later if writable
IF fileiswriteable(exepath$ + SLASH + "writetest.tmp") THEN
 'When CUSTOM is installed read-write, work in CUSTOM's folder
 safekill exepath$ + SLASH + "writetest.tmp"
 CHDIR exepath$ 'Note that exepath$ is a FreeBasic builtin, and not derived from the above exename
ELSE
 'If CUSTOM is installed read-only, use your home dir as the default
 CHDIR homedir
END IF

#IFDEF __UNIX__
 tmpdir = homedir + SLASH + ".ohrrpgce" + SLASH
 IF NOT isdir(tmpdir) THEN makedir tmpdir
#ELSE
 'Custom on Windows works in the current dir
 tmpdir = CURDIR + SLASH
#ENDIF

start_new_debug
debuginfo long_version & build_info 
debuginfo DATE & " " & TIME

dim workingdir as string
workingdir = tmpdir & "working.tmp"

processcommandline

REDIM gen(360)
REDIM buffer(16384)
REDIM master(255) as RGBcolor
REDIM uilook(uiColors)
DIM font(1024), joy(4)
DIM statnames() AS STRING
DIM menu(22) AS STRING
DIM chooserpg_menu(2) AS STRING
DIM cleanup_menu(2) AS STRING
DIM quit_menu(3) AS STRING
DIM quit_confirm(1) AS STRING
'more global variables
DIM game as string, sourcerpg as string, activepalette
DIM vpage, dpage, fadestate
DIM fmvol

RANDOMIZE TIMER, 3 ' mersenne twister

load_default_master_palette master()
LoadUIColors uilook()

getdefaultfont font()

setmodex
setpal master()
setfont font()
textcolor uilook(uiText), 0

'hinfo$(7), einfo$(0), ainfo$(2), xinfo$(1), winfo$(7)

DIM walkabout_frame_captions(7) AS STRING = {"Up A","Up B","Right A","Right B","Down A","Down B","Left A","Left B"}
DIM hero_frame_captions(7) AS STRING = {"Standing","Stepping","Attack A","Attack B","Cast/Use","Hurt","Weak","Dead"}
DIM enemy_frame_captions(0) AS STRING = {"Enemy (facing right)"}
DIM weapon_frame_captions(1) AS STRING = {"Frame 1","Frame 2"}
DIM attack_frame_captions(2) AS STRING = {"First Frame","Middle Frame","Last Frame"}
DIM box_border_captions(15) AS STRING = {"Top Left Corner","Top Edge Left","Top Edge","Top Edge Right","Top Right Corner","Left Edge Top","Right Edge Top","Left Edge","Right Edge","Left Edge Bottom","Right Edge Bottom","Bottom Left Corner","Bottom Edge Left","Bottom Edge","Bottom Edge Right","Bottom Right Corner"}
DIM portrait_captions(0) AS STRING = {"Character Portrait"}

keyboardsetup

dpage = 1: vpage = 0
game = ""
sourcerpg = ""
hsfile$ = ""

GOSUB makeworkingdir
FOR i = 1 TO UBOUND(cmdline_args)
 cmdline$ = cmdline_args(i)

 IF isfile(cmdline$) = 0 AND isdir(cmdline$) = 0 THEN
  centerbox 160, 40, 300, 50, 3, 0
  edgeprint "File not found/invalid option:", 15, 30, uilook(uiText), 0
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
  sourcerpg = cmdline$
  game = trimextension$(trimpath$(sourcerpg))
 END IF
NEXT
IF game = "" THEN
 hsfile$ = ""
 GOSUB chooserpg
END IF

end_debug

#IFDEF __FB_WIN32__
 IF MID$(sourcerpg, 2, 1) <> ":" THEN sourcerpg = curdir$ + SLASH + sourcerpg
#ELSE
 IF MID$(sourcerpg, 1, 1) <> SLASH THEN sourcerpg = curdir$ + SLASH + sourcerpg
#ENDIF
a$ = trimfilename(sourcerpg)
IF a$ <> "" ANDALSO fileiswriteable(a$ + SLASH + "writetest.tmp") THEN
 safekill a$ + SLASH + "writetest.tmp"
 CHDIR a$
END IF
'otherwise, keep current directory as it was, net effect: it is the same as in Game

start_new_debug
debuginfo long_version & build_info
debuginfo "Runtime info: " & gfxbackendinfo & "  " & musicbackendinfo & "  " & systeminfo
debuginfo "Editing game " & trimpath(sourcerpg) & " (" & getdisplayname(" ") & ") " & DATE & " " & TIME

setwindowtitle "OHRRPGCE - " + sourcerpg

GOSUB checkpass

clearpage vpage
textcolor uilook(uiText), 0
printstr "UNLUMPING DATA: please wait.", 0, 0, vpage
setvispage vpage

touchfile workingdir + SLASH + "__danger.tmp"

IF isdir(sourcerpg) THEN
 'work on an unlumped RPG file
 findfiles sourcerpg + SLASH + ALLFILES, 0, "filelist.tmp"
 fh = FREEFILE
 OPEN "filelist.tmp" FOR INPUT AS #fh
 DO UNTIL EOF(fh)
  LINE INPUT #fh, filename$
  filecopy sourcerpg + SLASH + filename$, workingdir + SLASH + filename$
 LOOP
 CLOSE #fh
 KILL "filelist.tmp"
ELSE
 unlump sourcerpg, workingdir + SLASH
END IF
game = workingdir + SLASH + game
verifyrpg
safekill workingdir + SLASH + "__danger.tmp"

IF hsfile$ <> "" THEN GOTO hsimport

setupmusic
fmvol = getfmvol

IF NOT isfile(game + ".mas") AND NOT isfile(workingdir + SLASH + "palettes.bin") THEN
 debug "Warning: " & game & ".mas does not exist (which should never happen)"
END IF
IF NOT isfile(game + ".fnt") THEN
 getdefaultfont font()
 xbsave game + ".fnt", font(), 2048
END IF
xbload game + ".fnt", font(), "Font not loaded"
'--loadgen, upgrade, resave
xbload game + ".gen", gen(), "general data is missing, RPG file corruption is likely"

'Load palette and uicolors to display messages while upgrading
load_default_master_palette master()
setpal master()
'get default ui colours
LoadUIColors uilook()

'upgrade obsolete RPG files
upgrade font()

'Load palette and uicolors again (because the upgrade routine may have changed them)
activepalette = gen(genMasterPal)
loadpalette master(), activepalette
setpal master()
LoadUIColors uilook(), activepalette

setfont font()
rpgversion gen(genVersion)

loadglobalstrings
getstatnames statnames()

menumode = 0
pt = 0
mainmax = 0
quitnow = 0

setkeys
GOSUB setmainmenu
DO:
 setwait 55
 setkeys
 tog = tog XOR 1
 IF keyval(scEsc) > 1 THEN
  SELECT CASE menumode
   CASE 0'--normal
    GOSUB relump
    IF quitnow > 1 THEN GOTO finis
   CASE 1'--graphics
    pt = 0: menumode = 0: GOSUB setmainmenu
  END SELECT
 END IF
 IF keyval(scF1) > 1 THEN
  SELECT CASE menumode
   CASE 0'--normal mode
    show_help "main"
   CASE 1'--normal mode
    show_help "gfxmain"
  END SELECT
 END IF
 IF keyval(scCTRL) > 0 AND keyval(scR) > 1 THEN
  reload_editor
 END IF
 usemenu pt, 0, 0, mainmax, 24
 IF enter_or_space() THEN
  SELECT CASE menumode
   CASE 0'--normal mode
    IF pt = 0 THEN pt = 0: menumode = 1: GOSUB setgraphicmenu
    IF pt = 1 THEN map_picker
    IF pt = 2 THEN edit_global_text_strings
    IF pt = 3 THEN herodata
    IF pt = 4 THEN enemydata
    IF pt = 5 THEN attackdata
    IF pt = 6 THEN itemdata
    IF pt = 7 THEN shopdata
    IF pt = 8 THEN formation
    IF pt = 9 THEN text_box_editor
    if pt = 10 THEN menu_editor
    IF pt = 11 THEN vehicles
    IF pt = 12 THEN tagnames
    IF pt = 13 THEN importsong
    IF pt = 14 THEN importsfx
    IF pt = 15 THEN fontedit font()
    IF pt = 16 THEN gendata
    IF pt = 17 THEN scriptman
    IF pt = 18 THEN slice_editor
    IF pt = 19 THEN
     GOSUB relump
     IF quitnow > 1 THEN GOTO finis
    END IF
   CASE 1'--graphics mode
    IF pt = 0 THEN pt = 0: menumode = 0: GOSUB setmainmenu
    IF pt = 1 THEN maptile font()
    IF pt = 2 THEN sprite 20, 20, gen(genMaxNPCPic),    8, 5, walkabout_frame_captions(),  4, 4, font()
    IF pt = 3 THEN sprite 32, 40, gen(genMaxHeroPic),   8, 16, hero_frame_captions(), 4, 0, font()
    IF pt = 4 THEN sprite 34, 34, gen(genMaxEnemy1Pic), 1, 2, enemy_frame_captions(), 4, 1, font()
    IF pt = 5 THEN sprite 50, 50, gen(genMaxEnemy2Pic), 1, 4, enemy_frame_captions(), 2, 2, font()
    IF pt = 6 THEN sprite 80, 80, gen(genMaxEnemy3Pic), 1, 10, enemy_frame_captions(), 2, 3, font()
    IF pt = 7 THEN sprite 50, 50, gen(genMaxAttackPic), 3, 12, attack_frame_captions(), 2, 6, font()
    IF pt = 8 THEN sprite 24, 24, gen(genMaxWeaponPic), 2, 2, weapon_frame_captions(), 4, 5, font()
    IF pt = 9 THEN
     sprite 16, 16, gen(genMaxBoxBorder), 16, 7, box_border_captions(), 4, 7, font()
    END IF
    IF pt = 10 THEN sprite 50, 50, gen(genMaxPortrait), 1, 4, portrait_captions(), 2, 8, font()
    IF pt = 11 THEN importbmp ".mxs", "screen", gen(genNumBackdrops)
    IF pt = 12 THEN
     gen(genMaxTile) = gen(genMaxTile) + 1
     importbmp ".til", "tileset", gen(genMaxTile)
     gen(genMaxTile) = gen(genMaxTile) - 1
     tileset_empty_cache
    END IF
    IF pt = 13 THEN ui_color_editor(activepalette)
  END SELECT
  '--always resave the .GEN lump after any menu
  xbsave game + ".gen", gen(), 1000
 END IF

 clearpage dpage
 standardmenu menu(), mainmax, 22, pt, 0, 0, 0, dpage, 0

 textcolor uilook(uiSelectedDisabled), 0
 printstr version_code$, 0, 176, dpage
 printstr version_build$, 0, 184, dpage
 textcolor uilook(uiText), 0
 printstr "Press F1 for help on any menu!", 0, 192, dpage

 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP

setmainmenu:
mainmax = 19
menu(0) = "Edit Graphics"
menu(1) = "Edit Map Data"
menu(2) = "Edit Global Text Strings"
menu(3) = "Edit Hero Stats"
menu(4) = "Edit Enemy Stats"
menu(5) = "Edit Attacks"
menu(6) = "Edit Items"
menu(7) = "Edit Shops"
menu(8) = "Edit Battle Formations"
menu(9) = "Edit Text Boxes"
menu(10) = "Edit Menus"
menu(11) = "Edit Vehicles"
menu(12) = "Edit Tag Names"
menu(13) = "Import Music"
menu(14) = "Import Sound Effects"
menu(15) = "Edit Font"
menu(16) = "Edit General Game Data"
menu(17) = "Script Management"
menu(18) = "Edit Slice Collections"
menu(19) = "Quit Editing"
RETRACE

setgraphicmenu:
mainmax = 13
menu(0) = "Back to the main menu"
menu(1) = "Edit Maptiles"
menu(2) = "Draw Walkabout Graphics"
menu(3) = "Draw Hero Graphics"
menu(4) = "Draw Small Enemy Graphics  34x34"
menu(5) = "Draw Medium Enemy Graphics 50x50"
menu(6) = "Draw Big Enemy Graphics    80x80"
menu(7) = "Draw Attacks"
menu(8) = "Draw Weapons"
menu(9) = "Draw Box Edges"
menu(10) = "Draw Portrait Graphics"
menu(11) = "Import/Export Screens"
menu(12) = "Import/Export Full Maptile Sets"
menu(13) = "Change User-Interface Colors"
RETRACE

chooserpg:
last = 2: csr = 1: top = 0
chooserpg_menu(0) = "CREATE NEW GAME"
chooserpg_menu(1) = "LOAD EXISTING GAME"
chooserpg_menu(2) = "EXIT PROGRAM"

setkeys
DO
 setwait 55
 setkeys
 tog = tog XOR 1
 IF keyval(scEsc) > 1 THEN GOTO finis
 usemenu csr, top, 0, last, 20
 IF enter_or_space() THEN
  IF csr = 0 THEN
   game = inputfilename("Filename of New Game?", ".rpg", CURDIR, "input_file_new_game", , NO)
   IF game <> "" THEN
     IF NOT newRPGfile(finddatafile("ohrrpgce.new"), game + ".rpg") THEN GOTO finis
     sourcerpg = game + ".rpg"
     game = trimpath$(game)
     EXIT DO
   END IF
  ELSEIF csr = 1 THEN
   sourcerpg = browse$(7, "", "*.rpg", tmpdir, 0, "browse_rpg")
   game = trimextension$(trimpath$(sourcerpg))
   IF game <> "" THEN EXIT DO
  ELSEIF csr = 2 THEN
   GOTO finis
  END IF
 END IF

 clearpage dpage
 standardmenu chooserpg_menu(), last, 22, csr, top, 0, 0, dpage, 0

 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
RETRACE

cleanup:
cleanup_menu(0) = "DO NOTHING"
cleanup_menu(1) = "RECOVER IT"
cleanup_menu(2) = "ERASE IT"
clean_choice = 0
a$ = "recovered"
i = 0
DO WHILE isfile("recovered" + STR$(i) + ".bak")
 i = i + 1
LOOP
a$ = a$ + STR$(i)
setkeys
DO
 setwait 55
 setkeys
 tog = tog XOR 1
 usemenu clean_choice, 0, 0, 2, 2
 IF enter_or_space() THEN
  IF clean_choice = 1 THEN
   IF isfile(workingdir + SLASH + "__danger.tmp") THEN
    textcolor uilook(uiSelectedItem), uilook(uiHighlight) 'FIXME: new uilook for warning text colors?
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
  IF clean_choice = 2 THEN RETRACE
  IF clean_choice = 0 THEN nocleanup = 1: RETRACE
 END IF
 textcolor uilook(uiSelectedDisabled), 0
 printstr "A game was found unlumped", 0, 0, dpage
 printstr "This may mean that " + CUSTOMEXE + " crashed", 0, 40, dpage
 printstr "last time you used it, or it may mean", 0, 48, dpage
 printstr "that another copy of " + CUSTOMEXE + " is", 0, 56, dpage
 printstr "already running in the background.", 0, 64, dpage

 standardmenu cleanup_menu(), 2, 2, clean_choice, 0, 0, 8, dpage, 0

 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

makeworkingdir:
IF NOT isdir(workingdir) THEN
 makedir workingdir
ELSE
 'Recover from an old crash
 GOSUB cleanup
 clearpage 0
 setvispage 0
 textcolor uilook(uiText), 0
 printstr "Run " + CUSTOMEXE + " again.", 0, 0, 0
 setvispage 0 'refresh
 w = getkey
 GOTO finis
END IF
RETRACE

relump:
xbsave game + ".gen", gen(), 1000
quit_menu(0) = "Continue editing"
quit_menu(1) = "Save changes and continue editing"
quit_menu(2) = "Save changes and quit"
quit_menu(3) = "Discard changes and quit"
clearkey(-1) 'stop firing esc's, if the user hit esc+pgup+pgdown
quitnow = sublist(quit_menu(), "quit_and_save")
IF keyval(-1) THEN '2nd quit request? Right away!
 a$ = trimextension(sourcerpg)
 i = 0
 DO
  lumpfile$ = a$ & ".rpg_" & i & ".bak"
  i += 1
 LOOP WHILE isfile(lumpfile$)
 clearpage 0
 printstr "Saving as " + lumpfile$, 0, 0, 0
 printstr "LUMPING DATA: please wait...", 0, 10, 0
 setvispage 0
 dolumpfiles lumpfile$
 quitnow = 4 'no special meaning
 RETRACE
END IF
IF quitnow = 1 OR quitnow = 2 THEN
 GOSUB dorelump
END IF
IF quitnow = 3 THEN
 quit_confirm(0) = "I changed my mind! Don't quit!"
 quit_confirm(1) = "I am sure I don't want to save."
 IF sublist(quit_confirm()) <= 0 THEN quitnow = 0
END IF
setkeys
RETRACE

dorelump:
clearpage 0
setvispage 0
textcolor uilook(uiText), 0
printstr "LUMPING DATA: please wait.", 0, 0, 0
setvispage 0 'refresh
'--verify that maps are not corrupt--
verifyrpg
'--lump data to SAVE rpg file
dolumpfiles sourcerpg
RETRACE

checkpass:
copylump sourcerpg, "archinym.lmp", workingdir, -1
'--set game according to the archinym
game = readarchinym()
copylump sourcerpg, game + ".gen", workingdir
xbload workingdir + SLASH + game + ".gen", gen(), "general data is missing, RPG file corruption is likely"
'----load password-----
IF gen(genPassVersion) >= 256 THEN
 '--new format password
 rpas$ = readpassword$
ELSE
 '--old scattertable format
 IF gen(genPW2Length) = -1 THEN RETRACE 'this is stupid
 readscatter rpas$, gen(genPW2Length), 200
 rpas$ = rotascii(rpas$, gen(genPW2Offset) * -1)
END IF
'--if password is unset, do not prompt
IF rpas$ = "" THEN RETRACE
'-----get inputed password-----
pas$ = ""
setkeys
DO
 setwait 55
 setkeys
 tog = tog XOR 1
 IF keyval(scEnter) > 1 THEN
  '--check password
  IF pas$ = rpas$ THEN
   RETRACE
  ELSE
   GOTO finis
  END IF
 END IF
 strgrabber pas$, 17
 clearpage dpage
 textcolor uilook(uiText), 0
 printstr "This game requires a password to edit", 0, 0, dpage
 printstr " Type it in and press ENTER", 0, 9, dpage
 textcolor uilook(uiSelectedItem + tog), 1
 printstr STRING$(LEN(pas$), "*"), 0, 20, dpage
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP

hsimport:
debuginfo "Importing scripts from " & hsfile$
xbload game + ".gen", gen(), "general data is missing, RPG file corruption is likely"
upgrade font() 'needed because it has not already happened because we are doing command-line import
importscripts with_orig_path(hsfile$)
xbsave game + ".gen", gen(), 1000
GOSUB dorelump
GOSUB cleanupfiles
end_debug
restoremode
SYSTEM

finis:
closemusic
'catch sprite leaks
sprite_empty_cache
palette16_empty_cache
GOSUB cleanupfiles
IF keyval(-1) = 0 THEN
 clearpage vpage
 pop_warning "Don't forget to keep backup copies of your work! You never know when an unknown bug or a hard-drive crash or a little brother might delete your files!"
END IF
end_debug
restoremode
END

cleanupfiles:
IF nocleanup = 0 THEN killdir workingdir
safekill "temp.lst"
RETRACE

'---GENERIC LOOP HEAD---
'setkeys
'DO
'setwait timing(), 100
'setkeys
'tog = tog XOR 1
'IF keyval(scESC) > 1 THEN EXIT DO
'IF keyval(scF1) > 1 THEN show_help "helpkey"

'---GENERIC LOOP TAIL---
'SWAP vpage, dpage
'setvispage vpage
'copypage 3, dpage
'dowait
'LOOP

'---For documentation of general data see http://hamsterrepublic.com/ohrrpgce/index.php/GEN.html

REM $STATIC

SUB fixorder (f$)

filecopy f$, "fixorder.tmp"

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

SUB shopdata
DIM a(20), b(curbinsize(binSTF) / 2), menu(24) AS STRING, smenu(24) AS STRING, max(24), min(24), sbit(-1 TO 10) AS STRING, stf(16) AS STRING, tradestf(3) AS STRING
DIM her AS HeroDef' Used to get hero name for default stuff name
DIM item_tmp(99) ' This is only used for loading the default buy/sell price for items
DIM sn AS STRING = "", trit AS STRING = ""


maxcount = 32: pt = 0
havestuf = 0
sbit(0) = "Buy"
sbit(1) = "Sell"
sbit(2) = "Hire"
sbit(3) = "Inn"
sbit(4) = "Equip"
sbit(5) = "Save"
sbit(6) = "Map"
sbit(7) = "Team"
smenu(0) = "Previous Menu"
max(3) = 1
min(5) = -1
max(5) = 99
FOR i = 6 TO 9
 min(i) = -999: max(i) = 999
NEXT i
min(10) = -32767
max(10) = 32767
FOR i = 11 TO 17 STEP 2
 max(i) = gen(genMaxItem)
 min(i) = -1
 max(i + 1) = 99
 min(i + 1) = 1
NEXT

min(20) = -32767
max(20) = 32767
max(21) = gen(genMaxItem)
min(21) = -1
max(22) = 99
min(22) = 1
stf(0) = "Item"
stf(1) = "Hero"
stf(2) = "Script"
stf(3) = "Normal"
stf(4) = "Aquire Inventory"
stf(5) = "Increment Inventory"
stf(6) = "Refuse to Buy"
stf(7) = "In Stock: Infinite"
stf(8) = "In Stock: None"

GOSUB lshopset
GOSUB menugen
li = 6
csr = 0
setkeys
DO
 setwait 55
 setkeys
 tog = tog XOR 1
 IF keyval(scEsc) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help "shop_main"
 IF keyval(scCtrl) > 0 AND keyval(scBackspace) > 0 THEN cropafter pt, gen(genMaxShop), 0, game + ".sho", 40: GOSUB menugen
 usemenu csr, 0, 0, li, 24
 IF csr = 1 THEN
  IF pt = gen(genMaxShop) AND keyval(scRight) > 1 THEN
   GOSUB sshopset
   pt = pt + 1
   IF gen(genMaxShop) < 99 THEN
    '--only allow adding shops up to 99
    'FIXME: This is because of the limitation on remembering shop stock in the SAV format
    '       when the SAV format has changed, this limit can easily be lifted.
    IF needaddset(pt, gen(genMaxShop), "Shop") THEN
     '--Create a new shop record
     flusharray a(), 19, 0
     setpicstuf a(), 40, -1
     storeset game + ".sho", pt, 0
     '--create a new shop stuff record
     flusharray b(), getbinsize(binSTF) / 2 - 1, 0
     setpicstuf b(), getbinsize(binSTF), -1
     b(19) = -1 ' When adding new stuff, default in-stock to infinite
     storeset game + ".stf", pt * 50 + 0, 0
    END IF
    GOSUB lshopset
   END IF
  END IF
  newpt = pt
  IF intgrabber(newpt, 0, gen(genMaxShop)) THEN
   GOSUB sshopset
   pt = newpt
   GOSUB lshopset
  END IF
 END IF
 IF csr = 2 THEN
  strgrabber sn, 15
  GOSUB menuup
 END IF
 IF enter_or_space() THEN
  IF csr = 0 THEN EXIT DO
  IF csr = 3 AND havestuf THEN
   GOSUB shopstuf
   GOSUB sstuf
  END IF
  IF csr = 4 THEN editbitset a(), 17, 7, sbit(): GOSUB menuup
  IF csr = 6 THEN
   menu(6) = "Inn Script: " & scriptbrowse_string(a(19), plottrigger, "Inn Plotscript")
  END IF
 END IF
 IF csr = 5 THEN
  IF intgrabber(a(18), 0, 32767) THEN GOSUB menuup
 END IF
 IF csr = 6 THEN
  IF scrintgrabber(a(19), 0, 0, scLeft, scRight, 1, plottrigger) THEN GOSUB menuup
 END IF
 clearpage dpage
 FOR i = 0 TO li
  c = uilook(uiMenuItem): IF i = csr THEN c = uilook(uiSelectedItem + tog)
  IF i = 3 AND havestuf = 0 THEN
   c = uilook(uiDisabledItem): IF i = csr THEN c = uilook(uiSelectedDisabled + tog)
  END IF
  textcolor c, 0
  printstr menu(i), 0, i * 8, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
GOSUB sshopset
EXIT SUB

menugen:
menu(0) = "Return to Main Menu"
menu(3) = "Edit Available Stuff..."
menu(4) = "Select Shop Menu Items..."
GOSUB menuup
RETRACE

lshopset:
setpicstuf a(), 40, -1
loadset game + ".sho", pt, 0
sn = ""
FOR i = 1 TO small(a(0), 15)
 sn = sn + CHR$(a(i))
NEXT i
GOSUB menuup
RETRACE

sshopset:
a(16) = small(a(16), 49)
a(0) = LEN(sn)
FOR i = 1 TO small(a(0), 15)
 a(i) = ASC(MID$(sn, i, 1))
NEXT i
setpicstuf a(), 40, -1
storeset game + ".sho", pt, 0
RETRACE

menuup:
menu(1) = CHR(27) & " Shop " & pt & " of " & gen(genMaxShop) & CHR(26)
menu(2) = "Name: " & sn
menu(5) = "Inn Price: " & a(18)
IF readbit(a(), 17, 3) = 0 THEN menu(5) = "Inn Price: N/A"
menu(6) = "Inn Script: " & scriptname$(a(19), plottrigger)
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
 setwait 55
 setkeys
 tog = tog XOR 1
 IF keyval(scEsc) > 1 THEN RETRACE
 IF keyval(scF1) > 1 THEN show_help "shop_stuff"
 IF tcsr = 0 THEN IF enter_or_space() THEN RETRACE
 usemenu tcsr, 0, 0, last, 24
 IF tcsr = 1 THEN
  IF keyval(scLeft) > 1 AND thing > 0 THEN
   GOSUB sstuf
   thing = thing - 1
   GOSUB lstuf
   GOSUB itstrsh
  END IF
  IF keyval(scRight) > 1 AND thing < 49 THEN
   GOSUB sstuf
   thing = thing + 1
   IF needaddset(thing, a(16), "Shop Thing") THEN
    flusharray b(), getbinsize(binSTF) / 2 - 1, 0
    setpicstuf b(), getbinsize(binSTF), -1
    b(19) = -1 ' When adding new stuff, default in-stock to infinite
    storeset game + ".stf", pt * 50 + thing, 0
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
   CASE 6 TO 9 '--tags
    tag_grabber b(17 + tcsr - 3)
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
      '--Re-load default names and default prices
      SELECT CASE b(17)
       CASE 0' This is an item
        thing$ = load_item_name(b(18),1,1)
        loaditemdata item_tmp(), b(18)
        b(24) = item_tmp(46) ' default buy price
        b(27) = item_tmp(46) \ 2 ' default sell price
       CASE 1
        loadherodata @her, b(18)
        thing$ = her.name
        b(24) = 0 ' default buy price
        b(27) = 0 ' default sell price
       CASE ELSE
        thing$ = "Unsupported"
      END SELECT
     END IF
    END IF
  END SELECT
 END IF
 GOSUB othertype
 GOSUB stufmenu

 clearpage dpage
 standardmenu smenu(), last, 22, tcsr, 0, 0, 0, dpage, 0

 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP

othertype:
SELECT CASE b(17)
 CASE 0 ' Is an item
  last = 22
  max(4) = gen(genMaxItem): IF b(18) > max(4) THEN b(18) = 0
  max(19) = 3 ' Item sell-type
 CASE 1 ' Is a hero
  last = 19
  max(4) = gen(genMaxHero): IF b(18) > gen(genMaxHero) THEN b(18) = 0
  max(19) = 99 ' Hero experience level
 CASE 2 ' Is ... something else?
  last = 18
  max(4) = 999
END SELECT
RETRACE

stufmenu:
smenu(1) = CHR(27) & "Shop Thing " & thing & " of " & a(16) & CHR(26)
smenu(2) = "Name: " & thing$
smenu(3) = "Type: " & b(17) & "-" & stf(bound(b(17), 0, 2))
smenu(4) = "Number: " & b(18) & " " & defaultthing$
IF b(19) > 0 THEN
 smenu(5) = "In Stock: " & b(19)
ELSE
 smenu(5) = stf(8 + bound(b(19), -1, 0))
END IF
smenu(6) = tag_condition_caption(b(20), "Buy Require Tag", "No Tag Check")
smenu(7) = tag_condition_caption(b(21), "Sell Require Tag", "No Tag Check")
smenu(8) = tag_condition_caption(b(22), "Buy Set Tag", "No Tag Set", "Unalterable", "Unalterable")
smenu(9) = tag_condition_caption(b(23), "Sell Set Tag", "No Tag Set", "Unalterable", "Unalterable")
smenu(10) = readglobalstring(32, "Money") & " Price: " & b(24)
smenu(11) = "Must Trade in " & (b(30) + 1) & " of: " & tradestf(0)
smenu(12) = " (Change Amount)"
smenu(13) = "Must Trade in " & (b(32) + 1) & " of: " & tradestf(1)
smenu(14) = " (Change Amount)"
smenu(15) = "Must Trade in " & (b(34) + 1) & " of: " & tradestf(2)
smenu(16) = " (Change Amount)"
smenu(17) = "Must Trade in " & (b(36) + 1) & " of: " & tradestf(3)
smenu(18) = " (Change Amount)"
IF b(17) = 0 THEN
 smenu(19) = "Sell type: " & stf(bound(b(26), 0, 3) + 3)
 smenu(20) = "Sell Price: " & b(27)
 smenu(21) = "Trade in for " & (b(29) + 1) & " of: " & trit$
 smenu(22) = " (Change Amount)"
ELSE
 smenu(19) = "Experience Level: "
 IF b(26) = -1 THEN
  smenu(19) = smenu(19) & "default"
 ELSE
  smenu(19) = smenu(19) & b(26)
 END IF
END IF
'--mutate menu for item/hero
RETRACE

lstuf:
flusharray b(), curbinsize(binSTF) / 2, 0
setpicstuf b(), getbinsize(binSTF), -1
loadset game + ".stf", pt * 50 + thing, 0
thing$ = readbadbinstring$(b(), 0, 16, 0)
'---check for invalid data
IF b(17) < 0 OR b(17) > 2 THEN b(17) = 0
IF b(19) < -1 THEN b(19) = 0
IF (b(26) < 0 OR b(26) > 3) AND b(17) <> 1 THEN b(26) = 0
'--WIP Serendipity custom builds didn't flush shop records when upgrading properly
FOR i = 32 TO 42
 b(i) = large(b(i), 0)
NEXT
RETRACE

sstuf:
b(0) = LEN(thing$)
FOR i = 1 TO small(b(0), 16)
 b(i) = ASC(MID$(thing$, i, 1))
NEXT i
setpicstuf b(), getbinsize(binSTF), -1
storeset game + ".stf", pt * 50 + thing, 0
RETRACE

itstrsh:
tradestf(0) = load_item_name(b(25),0,0)
tradestf(1) = load_item_name(b(31),0,0)
tradestf(2) = load_item_name(b(33),0,0)
tradestf(3) = load_item_name(b(35),0,0)
trit$ = load_item_name(b(28),0,0)
RETRACE

END SUB

FUNCTION newRPGfile (templatefile$, newrpg$)
 newRPGfile = 0 ' default return value 0 means failure
 IF newrpg$ = "" THEN EXIT FUNCTION
 textcolor uilook(uiSelectedDisabled), 0
 printstr "Please Wait...", 0, 40, vpage
 printstr "Creating RPG File", 0, 50, vpage
 setvispage vpage
 IF NOT isfile(templatefile$) THEN
  printstr "Error: ohrrpgce.new not found", 0, 60, vpage
  printstr "Press Enter to quit", 0, 70, vpage
 setvispage vpage
  w = getkey
  EXIT FUNCTION
 END IF
 filecopy templatefile$, newrpg$
 printstr "Unlumping", 0, 60, vpage
 setvispage vpage 'refresh
 unlump newrpg$, workingdir + SLASH
 '--create archinym information lump
 fh = FREEFILE
 OPEN workingdir + SLASH + "archinym.lmp" FOR OUTPUT AS #fh
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
findfiles workingdir + SLASH + ALLFILES, 0, "temp.lst"
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
  filecopy workingdir + SLASH + filename$, filetolump$ + SLASH + filename$
  'FIXME: move file?
 LOOP
 CLOSE #fh
 safekill "temp.lst"
ELSE
 '---relump data into lumpfile package---
 IF NOT fileiswriteable(filetolump$) THEN
  move_unwritable_rpg filetolump$
 END IF
 lumpfiles "temp.lst", filetolump$, workingdir + SLASH
 safekill "temp.lst"
END IF
END SUB

FUNCTION readarchinym$ ()
 IF isfile(workingdir + SLASH + "archinym.lmp") THEN
  fh = FREEFILE
  OPEN workingdir + SLASH + "archinym.lmp" FOR INPUT AS #fh
  LINE INPUT #fh, a$
  CLOSE #fh
  a$ = LCASE$(a$)
  readarchinym$ = a$
 ELSE
  ' for backwards compatability with ancient games that lack archinym.lmp
  debug workingdir + SLASH + "archinym.lmp" + " unreadable, using " + LCASE$(trimpath$(game)) + " instead"
  readarchinym$ = LCASE$(trimpath$(game))
 END IF
END FUNCTION

SUB move_unwritable_rpg(BYREF filetolump$)
 clearpage vpage
 centerbox 160, 100, 310, 50, 3, vpage
 edgeprint filetolump$, 10, 80, uilook(uiText), vpage
 edgeprint "is not writable. Saving to:", 10, 90, uilook(uiText), vpage
 filetolump$ = homedir & SLASH & trimpath$(filetolump$)
 edgeprint filetolump$, 10, 100, uilook(uiText), vpage
 edgeprint "[Press Any Key]", 10, 110, uilook(uiText), vpage
 setvispage vpage
 w = getkey
END SUB

'=======================================================================
'FIXME: move this up as code gets cleaned up!  (Hah!)
OPTION EXPLICIT
