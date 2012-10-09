'OHRRPGCE - File browser
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#ifdef LANG_DEPRECATED
 #define __langtok #lang
 __langtok "deprecated"
 OPTION STATIC
 OPTION EXPLICIT
#endif

#include "config.bi"
#include "ver.txt"
#include "const.bi"
#include "allmodex.bi"
#include "common.bi"
#include "loading.bi"
#include "reload.bi"
#include "os.bi"

Enum BrowseEntryKind
	bkDrive = 0       'Windows only
	bkParentDir = 1
	bkSubDir = 2
	bkSelectable = 3
	bkRoot = 4
	bkSpecial = 5      'Not used
	bkUnselectable = 6
End Enum

Type BrowseMenuEntry
	kind as BrowseEntryKind
	filename as string
	caption as string
	about as string
End type

Type BrowseMenuState
	nowdir as string
	tmp as string
	treeptr as integer
	treetop as integer
	treesize as integer   'last entry
	viewsize as integer
	special as integer
	ranalready as integer
	meter as integer
	drivesshown as integer  'number of drive entries (plus 1 for refresh)
	alert as string
	mashead as string
	paledithead as string
	showHidden as integer
	getdrivenames as integer
	changed as integer
	fmask as string
	snd as integer
End Type

'Subs and functions only used locally
DECLARE SUB build_listing(tree() as BrowseMenuEntry, byref br as BrowseMenuState)
DECLARE SUB draw_browse_meter(br as BrowseMenuState)
DECLARE SUB browse_hover(tree() as BrowseMenuEntry, byref br as BrowseMenuState)
DECLARE SUB browse_hover_file(tree() as BrowseMenuEntry, byref br as BrowseMenuState)
DECLARE SUB browse_add_files(wildcard as string, byval filetype as integer, byref br as BrowseMenuState, tree() as BrowseMenuEntry)
DECLARE FUNCTION validmusicfile (file as string, byval typemask as integer) as integer
DECLARE FUNCTION browse_sanity_check_reload(filename as string, info as string) as integer
DECLARE FUNCTION check_for_plotscr_inclusion(filename as string) as integer

FUNCTION browse (byval special as integer, default as string, fmask as string, tmp as string, byref needf as integer, helpkey as string) as string
STATIC remember as string
DIM ret as string

DIM br as BrowseMenuState
br.tmp = tmp
br.special = special
br.fmask = fmask
br.snd = -1

'special=0   no preview
'special=1   just BAM
'special=2   2 or 16 color BMP
'special=3   background
'special=4   master palette (*.mas, 8 bit *.bmp, 16x16 24 bit *.bmp) (fmask is ignored)
'special=5   any supported music (currently *.bam, *.mid, *.ogg, *.mp3, *.mod, *.xm, *.it, *.s3m formats)  (fmask is ignored)
'special=6   any supported SFX (currently *.ogg, *.wav, *.mp3) (fmask is ignored)
'special=7   RPG files
'special=8   RELOAD files
'special=9   script files (.hs, .hss)
'special=10  2, 16 or 256 colour BMP, any size (temporary, used by font_test_menu only)

br.mashead = CHR(253) & CHR(13) & CHR(158) & CHR(0) & CHR(0) & CHR(0) & CHR(6)
br.paledithead = CHR(253) & CHR(217) & CHR(158) & CHR(0) & CHR(0) & CHR(7) & CHR(6)

'tree().kind contains the type of each object in the menu
REDIM tree(255) as BrowseMenuEntry
DIM catfg(6) as integer, catbg(6) as integer

br.showHidden = 0

'FIXME: do we need another uilook() constant for these "blue" directories instead of uilook(uiTextbox + 1)?
catfg(0) = uilook(uiMenuItem)   : catbg(0) = uilook(uiHighlight)    'selectable drives (none on unix systems)
catfg(1) = uilook(uiTextbox + 1): catbg(1) = uilook(uiDisabledItem) 'directories
catfg(2) = uilook(uiTextbox + 1): catbg(2) = uilook(uiBackground)   'subdirectories
catfg(3) = uilook(uiMenuItem)   : catbg(3) = uilook(uiBackground)   'files
catfg(4) = uilook(uiTextbox + 1): catbg(4) = uilook(uiDisabledItem) 'root of current drive
catfg(5) = uilook(uiTextBox + 3): catbg(5) = uilook(uiDisabledItem) 'special (never used???)
catfg(6) = uilook(uiDisabledItem): catbg(6) = uilook(uiBackground)  'disabled

IF needf = 1 THEN
 DIM temppal(255) as RGBcolor
 FOR i as integer = 0 TO 255
  temppal(i).r = 0
  temppal(i).g = 0
  temppal(i).b = 0
 NEXT i
 setpal temppal()
END IF

'Load a variant of the default font, misc/browser font.ohf, which has both Latin-1
'characters and the old (c), etc, characters from the original fonts
DIM browser_font(1023) as integer
getbrowserfont browser_font()
setfont browser_font()

'remember/default may be either empty or a file (if one was selected last call), or directory (if not)
DIM startfile as string
IF remember = "" THEN remember = curdir & SLASH
IF default = "" THEN
 default = remember
END IF
startfile = default
br.nowdir = trimfilename(startfile) & SLASH
IF RIGHT(startfile, 1) = SLASH THEN
 startfile = ""
ELSE
 startfile = trimpath(startfile)
END IF

IF br.special = 7 THEN br.viewsize = 16 ELSE br.viewsize = 17

br.treeptr = 0
br.treetop = 0
br.treesize = 0
br.drivesshown = 0
br.getdrivenames = 0  'whether to fetch names of all drives, on if hit F5

br.ranalready = 0
build_listing tree(), br

IF LEN(startfile) THEN
 FOR i as integer = 0 TO br.treesize
  IF tree(i).filename = startfile THEN br.treeptr = i
 NEXT
END IF

br.changed = 0
IF br.alert = "" THEN br.changed = 1  'Don't clobber alert

setkeys YES
DO
 setwait 55
 setkeys YES
 IF keyval(scEsc) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help helpkey
 IF usemenu(br.treeptr, br.treetop, 0, br.treesize, br.viewsize) OR br.changed THEN
  br.alert = ""
  br.changed = 0
  browse_hover tree(), br
 END IF
 IF enter_or_space() THEN
  br.alert = ""
  br.changed = 1
  IF br.special = 1 OR br.special = 5 THEN music_stop
  SELECT CASE tree(br.treeptr).kind
   CASE bkDrive
    'this could take a while...
    rectangle 5, 32 + br.viewsize * 9, 310, 12, uilook(uiTextbox + 0), vpage
    edgeprint "Reading...", 8, 34 + br.viewsize * 9, uilook(uiText), vpage
    setvispage vpage
    IF hasmedia(tree(br.treeptr).filename) THEN
     br.nowdir = tree(br.treeptr).filename
     build_listing tree(), br
    ELSE
     br.alert = "No media"
     br.changed = 0
    END IF
   CASE bkParentDir, bkRoot
    br.nowdir = ""
    FOR i as integer = br.drivesshown TO br.treeptr
     br.nowdir = br.nowdir + tree(i).filename
    NEXT i
    build_listing tree(), br
   CASE bkSubDir
    br.nowdir = br.nowdir + tree(br.treeptr).filename + SLASH
    build_listing tree(), br
   CASE bkSelectable
    ret = br.nowdir + tree(br.treeptr).filename
    EXIT DO
  END SELECT
 END IF
 IF keyval(scCtrl) THEN
  'Ctrl + H for hidden
  IF keyval(scH) > 1 THEN
   br.showHidden XOR= YES
   build_listing tree(), br
  END IF
 ELSE
  'find by letter
  DIM intext as string = LCASE(LEFT(getinputtext, 1))
  IF LEN(intext) > 0 THEN
   FOR j as integer = 1 TO br.treesize
    DIM mappedj as integer
    mappedj = (j + br.treeptr) MOD (br.treesize + 1)
    IF (tree(mappedj).kind = bkParentDir OR tree(mappedj).kind = bkSubDir OR tree(mappedj).kind = bkSelectable) THEN
     IF LCASE(LEFT(tree(mappedj).caption, 1)) = intext THEN br.treeptr = mappedj: EXIT FOR
    END IF
   NEXT
  END IF
 END IF
 IF keyval(scF5) > 1 THEN  'F5
  'refresh
  br.drivesshown = 0
  br.getdrivenames = 1
  build_listing tree(), br
  br.changed = 1
 END IF
 IF keyval(scBackspace) > 1 THEN 'backspace
  'go up a directory
  FOR i as integer = LEN(br.nowdir) - 1 TO 1 STEP -1
   IF br.nowdir[i - 1] = ASC(SLASH) THEN br.nowdir = LEFT(br.nowdir, i) : EXIT FOR
  NEXT
  build_listing tree(), br
  br.changed = 1
 END IF

 '--Draw screen
 clearpage dpage
 edgeboxstyle 4, 3, 312, 14, 0, dpage, NO, YES
 IF br.special = 7 AND tree(br.treeptr).kind = bkSelectable THEN
  'Selected item is an RPG
  edgeprint shorten_to_left(br.nowdir + tree(br.treeptr).filename, 304), 8, 6, uilook(uiText), dpage
 ELSE
  edgeprint br.nowdir, 8, 6, uilook(uiText), dpage
 END IF
 edgeboxstyle 4, 31 + br.viewsize * 9, 312, 14, 0, dpage, NO, YES
 edgeprint br.alert, 8, 34 + br.viewsize * 9, uilook(uiText), dpage
 IF br.special = 7 THEN
  rectangle 0, 190, 320, 10, uilook(uiDisabledItem), dpage
  edgeprint version & " " & gfxbackend & "/" & musicbackend, 8, 190, uilook(uiMenuItem), dpage
  textcolor uilook(uiText), 0
 END IF
 textcolor uilook(uiText), 0
 printstr ">", 0, 20 + (br.treeptr - br.treetop) * 9, dpage
 FOR i as integer = br.treetop TO small(br.treetop + br.viewsize, br.treesize)
  textcolor catfg(tree(i).kind), catbg(tree(i).kind)
  DIM a as string = tree(i).caption
  IF LEN(a) < 38 AND catbg(tree(i).kind) > 0 THEN a = a + STRING(38 - LEN(a), " ")
  printstr a, 10, 20 + (i - br.treetop) * 9, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 IF needf = 1 THEN fadein: setkeys
 IF needf THEN needf = needf - 1
 dowait
LOOP

setfont current_font()
IF LEN(ret) THEN
 default = ret
ELSE
 default = br.nowdir
END IF
remember = default
music_stop
IF br.snd >= 0 THEN sound_stop(br.snd, -1) : UnloadSound(br.snd)
RETURN ret

END FUNCTION

SUB browse_hover(tree() as BrowseMenuEntry, byref br as BrowseMenuState)
 SELECT CASE tree(br.treeptr).kind
  CASE bkDrive
   br.alert = "Drive"
  CASE bkParentDir
   br.alert = "Directory"
  CASE bkSubDir
   br.alert = "Subdirectory"
  CASE bkRoot
   br.alert = "Root"
  CASE bkSelectable, bkUnselectable
   browse_hover_file tree(), br
 END SELECT
END SUB

SUB browse_hover_file(tree() as BrowseMenuEntry, byref br as BrowseMenuState)
 DIM bmpd as BitmapInfoHeader
 SELECT CASE br.special
  CASE 1 'music bam only (is this still used?)
   music_stop
   IF tree(br.treeptr).kind = bkSelectable OR tree(br.treeptr).kind = bkUnselectable THEN
    IF validmusicfile(br.nowdir + tree(br.treeptr).filename, FORMAT_BAM) THEN
     loadsong br.nowdir + tree(br.treeptr).filename
    ELSE
     br.alert = tree(br.treeptr).filename + " is not a valid BAM file"
    END IF
   END IF
  CASE 5 'music
   music_stop
   br.alert = tree(br.treeptr).about
   IF validmusicfile(br.nowdir + tree(br.treeptr).filename, PREVIEWABLE_MUSIC_FORMAT) THEN
    loadsong br.nowdir + tree(br.treeptr).filename
   ELSEIF getmusictype(br.nowdir + tree(br.treeptr).filename) = FORMAT_MP3 THEN
    br.alert = "Cannot preview MP3, try importing"
   END IF
  CASE 6 'sfx
   br.alert = tree(br.treeptr).about
   IF br.snd > -1 THEN
    sound_stop(br.snd,-1)
    UnloadSound(br.snd)
    br.snd = -1
   END IF
   IF tree(br.treeptr).kind <> bkUnselectable THEN
    'not disabled because of size
    IF validmusicfile(br.nowdir + tree(br.treeptr).filename, PREVIEWABLE_FX_FORMAT) THEN
     br.snd = LoadSound(br.nowdir + tree(br.treeptr).filename)
     sound_play(br.snd, 0, -1)
    ELSEIF getmusictype(br.nowdir + tree(br.treeptr).filename) = FORMAT_MP3 THEN
     br.alert = "Cannot preview MP3, try importing"
    END IF
   END IF
  CASE 9 'scripts
   IF LCASE(justextension(tree(br.treeptr).filename)) = "hs" THEN
    br.alert = "Compiled HamsterSpeak scripts"
   ELSE
    br.alert = "HamsterSpeak scripts"
   END IF
  CASE ELSE
   br.alert = tree(br.treeptr).about
 END SELECT
END SUB

'Returns true if the BMP looks good
FUNCTION browse_check_bmp(byref br as BrowseMenuState, tree() as BrowseMenuEntry, byref bmpd as BitmapInfoHeader) as integer
 DIM support as integer = bmpinfo(br.nowdir + tree(br.treesize).filename, bmpd)
 IF support = 2 THEN
  tree(br.treesize).about = bmpd.biWidth & "*" & bmpd.biHeight & " pixels, " & bmpd.biBitCount & "-bit color"
  RETURN YES
 ELSEIF support = 1 THEN
  tree(br.treesize).about = "Unsupported BMP file"
 ELSE
  tree(br.treesize).about = "Invalid BMP file"
 END IF
 tree(br.treesize).kind = bkUnselectable
 RETURN NO
END FUNCTION

SUB browse_add_files(wildcard as string, byval filetype as integer, byref br as BrowseMenuState, tree() as BrowseMenuEntry)
DIM bmpd as BitmapInfoHeader
DIM tempbuf(79) as integer
DIM filename as string

DIM filelist() as string
findfiles br.nowdir, wildcard, filetype, br.showhidden, filelist()

FOR i as integer = 0 TO UBOUND(filelist)
 br.treesize = br.treesize + 1
 IF br.treesize = UBOUND(tree) THEN REDIM PRESERVE tree(UBOUND(tree) + 256)
 tree(br.treesize).kind = bkSelectable
 tree(br.treesize).filename = filelist(i)
 filename = br.nowdir & tree(br.treesize).filename
 '---music files
 IF br.special = 1 OR br.special = 5 THEN
  IF validmusicfile(filename, VALID_MUSIC_FORMAT) = 0 THEN
   tree(br.treesize).kind = bkUnselectable
   tree(br.treesize).about = "Not a valid music file"
  END IF
 END IF
 IF br.special = 6 THEN
  IF validmusicfile(filename, VALID_FX_FORMAT) = 0 THEN
   tree(br.treesize).kind = bkUnselectable
   tree(br.treesize).about = "Not a valid sound effect file"
  ELSEIF FILELEN(filename) > 500 * 1024 AND LCASE(justextension(filename)) <> "wav" THEN
   tree(br.treesize).kind = bkUnselectable
   tree(br.treesize).about = "File is too large (limit 500kB)"
  END IF
 END IF
 '---1- and 4-bit BMP browsing
 IF br.special = 2 THEN
  IF browse_check_bmp(br, tree(), bmpd) THEN
   IF bmpd.biBitCount > 4 OR bmpd.biWidth > 320 OR bmpd.biHeight > 200 THEN
    tree(br.treesize).kind = bkUnselectable
   END IF
  END IF
 END IF
 '---320x200x24/8bit BMP files
 IF br.special = 3 THEN
  IF browse_check_bmp(br, tree(), bmpd) THEN
   IF (bmpd.biBitCount <> 24 AND bmpd.biBitCount <> 8) OR bmpd.biWidth <> 320 OR bmpd.biHeight <> 200 THEN
    tree(br.treesize).kind = bkUnselectable
   END IF
  END IF
 END IF
 '---1/4/8 bit BMP files
 IF br.special = 10 THEN
  IF browse_check_bmp(br, tree(), bmpd) THEN
   IF bmpd.biBitCount > 8 THEN
    tree(br.treesize).kind = bkUnselectable
   END IF
  END IF
 END IF
 '--master palettes
 IF br.special = 4 THEN
  IF LCASE(justextension(filename)) = "mas" THEN
   DIM masfh as integer = FREEFILE
   OPEN filename FOR BINARY as #masfh
   DIM a as string = "       "
   GET #masfh, 1, a
   CLOSE #masfh
   SELECT CASE a
    CASE br.mashead
     tree(br.treesize).about = "MAS format"
    CASE br.paledithead
     tree(br.treesize).about = "MAS format (PalEdit)"
    CASE ELSE
     tree(br.treesize).about = "Not a valid MAS file"
     tree(br.treesize).kind = bkUnselectable
   END SELECT
  ELSE  'BMP as a master palette
   IF browse_check_bmp(br, tree(), bmpd) THEN
    IF bmpd.biBitCount = 8 THEN
     'Don't care about the dimensions
     tree(br.treesize).about = bmpd.biBitCount & "-bit color BMP"
    ELSEIF (bmpd.biBitCount = 24 AND (bmpd.biWidth = 16 AND bmpd.biHeight = 16)) = 0 THEN
     tree(br.treesize).kind = bkUnselectable
    END IF
   END IF
  END IF
 END IF
 '--RPG files
 IF br.special = 7 THEN
  copylump filename, "browse.txt", br.tmp, -1
  tree(br.treesize).caption = load_gamename(br.tmp & "browse.txt")
  tree(br.treesize).about = load_aboutline(br.tmp & "browse.txt")
  safekill br.tmp & "browse.txt"
  IF tree(br.treesize).caption = "" THEN tree(br.treesize).caption = tree(br.treesize).filename
 END IF
 '--RELOAD files
 IF br.special = 8 THEN
  IF browse_sanity_check_reload(filename, tree(br.treesize).about) = NO THEN
   tree(br.treesize).kind = bkUnselectable 'grey out bad ones
  END IF
 END IF
 '--script files
 IF br.special = 9 THEN
  IF wildcard = "*.txt" THEN
   IF NOT check_for_plotscr_inclusion(filename) THEN
    'Don't display .txt files unless they include plotscr.hsd
    br.treesize = br.treesize - 1
   END IF
  END IF
 END IF
 draw_browse_meter br
NEXT

END SUB

FUNCTION browse_sanity_check_reload(filename as string, info as string) as integer
 'info argument will be modified
 DIM header as string = "    "
 DIM fh as integer = FREEFILE
 OPEN filename FOR BINARY ACCESS READ as #fh
  GET #fh, 1, header
 CLOSE #fh
 IF header <> "RELD" THEN info = "Has no RELOAD file header." : RETURN NO
 DIM doc as Reload.Docptr
 doc = Reload.LoadDocument(filename)
 IF doc = 0 THEN info = "Reload document not loadable." : RETURN NO
 DIM node as Reload.Nodeptr
 node = Reload.DocumentRoot(doc)
 IF node = 0 THEN
  Reload.FreeDocument(doc)
  info = "Reload document has broken root node"
  RETURN NO
 END IF
 SELECT CASE Reload.NodeName(node)
  CASE "rsav": info = "OHRRPGCE Save-game"
  CASE "editor": info = "OHRRPGCE editor definition file"
  CASE "":
   IF RIGHT(filename, 6) = ".slice" ORELSE LEFT(trimpath(filename), 10) = "slicetree_" THEN
    info = "Saved slice collection"
   ELSE
    info = "RELOAD document"
   END IF
  CASE ELSE
   info = "RELOAD document (" & Reload.NodeName(node) & ")"
 END SELECT
 Reload.FreeDocument(doc)
 RETURN YES
END FUNCTION

SUB draw_browse_meter(br as BrowseMenuState)
IF br.treesize AND 15 THEN EXIT SUB
WITH br
 IF .ranalready THEN
  .meter = small(.meter + 1, 308)
  rectangle 5 + .meter, 33 + .viewsize * 9, 2, 5, uilook(uiTextbox + 1), vpage
  setvispage vpage 'refresh
 END IF
END WITH
END SUB

FUNCTION validmusicfile (file as string, byval typemask as integer) as integer
'-- actually, doesn't need to be a music file, but only multi-filetype imported data right now
	DIM as string hdmask, realhd
	DIM as integer musfh, chk
	chk = getmusictype(file)

	if (chk AND typemask) = 0 then return 0

	SELECT CASE chk
	CASE FORMAT_BAM
		hdmask = "    "
		realhd = "CBMF"
	CASE FORMAT_MIDI
		hdmask = "    "
		realhd = "MThd"
	CASE FORMAT_XM
		hdmask = "                 "
		realhd = "Extended Module: "
	END SELECT

	if LEN(hdmask) then
		musfh = FREEFILE
		OPEN file FOR BINARY as #musfh
		GET #musfh, 1, hdmask
		CLOSE #musfh
		IF hdmask <> realhd THEN return 0
	end if

	return 1
END FUNCTION

SUB build_listing(tree() as BrowseMenuEntry, byref br as BrowseMenuState)
 'for progress meter
 IF br.ranalready THEN rectangle 5, 32 + br.viewsize * 9, 310, 12, uilook(uiTextbox + 0), vpage
 br.meter = 0

 'erase old list
 IF br.getdrivenames THEN br.treesize = -1 ELSE br.treesize = br.drivesshown - 1
 FOR i as integer = br.treesize + 1 TO UBOUND(tree)
  tree(i).filename = ""
  tree(i).caption = ""
  tree(i).about = ""
  tree(i).kind = 0
 NEXT i
 
#IFDEF __FB_WIN32__
 '--Drive list
 IF br.drivesshown = 0 THEN
  '--Refresh drives option
  'br.treesize += 1
  'tree(br.treesize).filename = ""
  'tree(br.treesize).caption = "Refresh drives list"
  'tree(br.treesize).kind = bkSpecial

  DIM drive(26) as string
  DIM drivetotal as integer = drivelist(drive())
  FOR i as integer = 0 TO drivetotal - 1
   br.treesize += 1
   tree(br.treesize).filename = drive(i)
   tree(br.treesize).caption = drive(i)
   tree(br.treesize).kind = bkDrive
   IF br.getdrivenames THEN
    tree(br.treesize).caption += " " + drivelabel(drive(i))
   END IF
   draw_browse_meter br

  NEXT i
  'could add My Documents to drives list here
 END IF
#ENDIF
 br.drivesshown = br.treesize + 1
 br.getdrivenames = 0

 IF br.nowdir = "" THEN
 ELSE
  DIM a as string = br.nowdir
  DIM b as string

  '--Current drive
  br.treesize += 1
  b = MID(a, 1, INSTR(a, SLASH))
  tree(br.treesize).filename = b
  tree(br.treesize).kind = bkRoot
#IFDEF __FB_WIN32__
  IF hasmedia(b) = 0 THEN
   'Somebody pulled out the disk
   br.changed = 0
   br.alert = "Disk not readable"
   br.treesize -= 1
   br.treeptr = 0
   br.treetop = 0
   br.nowdir = ""
   EXIT SUB
  END IF
  FOR i as integer = 0 TO br.drivesshown - 1
   IF tree(i).filename = b THEN
    DIM tmpname as string = drivelabel(b)
    IF LEN(tmpname) THEN tree(i).caption = b + " " + tmpname
    tree(br.treesize).caption = tree(i).caption
    EXIT FOR
   END IF
  NEXT
#ENDIF
  a = MID(a, INSTR(a, SLASH) + 1)
  '--Directories
  b = ""
  DO UNTIL a = ""
   b = b + LEFT(a, 1)
   a = RIGHT(a, LEN(a) - 1)
   IF RIGHT(b, 1) = SLASH THEN
#IFDEF __FB_WIN32__
    'Special handling of My Documents in Windows
    IF b = "My Documents\" OR b = "MYDOCU~1\" THEN
     FOR i as integer = br.treesize TO br.drivesshown STEP -1
      b = tree(i).filename + b
     NEXT i
     br.treesize = br.drivesshown - 1
     tree(br.treesize + 1).caption = "My Documents\"
    END IF
#ENDIF
    br.treesize = br.treesize + 1
    IF br.treesize = UBOUND(tree) THEN REDIM PRESERVE tree(UBOUND(tree) + 256)
    tree(br.treesize).filename = b
    tree(br.treesize).kind = bkParentDir
    b = ""
   END IF
  LOOP
  '---FIND ALL SUB-DIRECTORIES IN THE CURRENT DIRECTORY---
  DIM filelist() as string
  findfiles br.nowdir, ALLFILES, fileTypeDirectory, br.showhidden, filelist()
  FOR i as integer = 0 TO UBOUND(filelist)
   br.treesize = br.treesize + 1
   IF br.treesize = UBOUND(tree) THEN REDIM PRESERVE tree(UBOUND(tree) + 256)
   tree(br.treesize).kind = bkSubDir
   tree(br.treesize).filename = filelist(i)
   DIM extension as string = justextension(filelist(i))
   IF tree(br.treesize).filename = "." OR tree(br.treesize).filename = ".." OR RIGHT(tree(br.treesize).filename, 4) = ".tmp" THEN br.treesize = br.treesize - 1
   IF br.special = 7 THEN ' Special handling in RPG mode
    IF LCASE(extension) = "rpgdir" THEN br.treesize = br.treesize - 1
   END IF
   IF br.special <> 8 THEN
    '--hide any .saves folders when browsing (except in RELOAD special mode)
    IF LCASE(extension) = "saves" THEN br.treesize = br.treesize - 1
   END IF
#IFDEF __FB_DARWIN__
   IF extension = "app" THEN br.treesize = br.treesize - 1
#ENDIF
   draw_browse_meter br
  NEXT
  '---FIND ALL FILES IN FILEMASK---
  DIM filetype as integer = fileTypeFile
  IF br.special = 4 THEN
   browse_add_files "*.mas", filetype, br, tree()
   browse_add_files "*.bmp", filetype, br, tree()
  ELSEIF br.special = 5 THEN' background music
   '--disregard fmask. one call per extension
   browse_add_files "*.bam", filetype, br, tree()
   browse_add_files "*.mid", filetype, br, tree()
   browse_add_files "*.xm", filetype, br, tree()
   browse_add_files "*.it", filetype, br, tree()
   browse_add_files "*.mod", filetype, br, tree()
   browse_add_files "*.s3m", filetype, br, tree()
   browse_add_files "*.ogg", filetype, br, tree()
   browse_add_files "*.mp3", filetype, br, tree()
  ELSEIF br.special = 6 THEN ' sound effects
   '--disregard fmask. one call per extension
   browse_add_files "*.wav", filetype, br, tree()
   browse_add_files "*.ogg", filetype, br, tree()
   browse_add_files "*.mp3", filetype, br, tree()
  ELSEIF br.special = 7 THEN
   'Call once for RPG files once for rpgdirs
   browse_add_files br.fmask, filetype, br, tree()
   browse_add_files "*.rpgdir", fileTypeDirectory, br, tree()
  ELSEIF br.special = 8 THEN
   browse_add_files "*.reld", filetype, br, tree()
   browse_add_files "*.reload", filetype, br, tree()
   browse_add_files "*.slice", filetype, br, tree()
   browse_add_files "*.rsav", filetype, br, tree()
   browse_add_files "*.editor", filetype, br, tree()
  ELSEIF br.special = 9 THEN
   browse_add_files "*.hs", filetype, br, tree()
   browse_add_files "*.hss", filetype, br, tree()
   browse_add_files "*.txt", filetype, br, tree()
  ELSE
   browse_add_files br.fmask, filetype, br, tree()
  END IF
 END IF

 '--set display
 FOR i as integer = 0 TO br.treesize
  IF LEN(tree(i).caption) = 0 THEN
   tree(i).caption = tree(i).filename
  END IF
 NEXT

 DIM sortstart as integer = br.treesize
 FOR k as integer = 0 TO br.treesize
  WITH tree(k)
   IF .kind = bkSubDir OR .kind = bkSelectable OR .kind = bkUnselectable THEN sortstart = k: EXIT FOR
  END WITH
 NEXT

 '--alphabetize
 FOR i as integer = sortstart TO br.treesize - 1
  FOR j as integer = br.treesize TO i + 1 STEP -1
   DIM k as integer = 0
   DIM chara as integer
   DIM charb as integer
   DO
    chara = tolower(tree(i).caption[k])
    charb = tolower(tree(j).caption[k])
    IF chara < charb THEN
     EXIT DO
    ELSEIF chara > charb THEN
     SWAP tree(i), tree(j)
     EXIT DO
    END IF
    k += 1
   LOOP WHILE chara OR charb
  NEXT
 NEXT

 '--sort by type
 FOR o as integer = br.treesize TO sortstart + 1 STEP -1
  FOR i as integer = sortstart + 1 TO o
   IF tree(i).kind < tree(i - 1).kind THEN
    SWAP tree(i), tree(i - 1)
   END IF
  NEXT
 NEXT

 '--set cursor
 br.treeptr = 0
 br.treetop = 0
 FOR i as integer = br.drivesshown TO br.treesize
  'look for first selectable item
  IF tree(i).kind = bkSelectable THEN br.treeptr = i: EXIT FOR
  'second preference is first subdirectory
  IF tree(i).kind = bkSubDir AND tree(br.treeptr).kind <> bkSubDir THEN br.treeptr = i
  'final preference is current (bottommost) directory
  IF tree(i).kind = bkParentDir OR tree(i).kind = bkRoot THEN br.treeptr = i
 NEXT i
 br.treetop = small(br.treeptr - 2, br.treesize - br.viewsize)
 br.treetop = large(br.treetop, 0)

 '--don't display progress bar overtop of previous menu
 br.ranalready = 1

END SUB

FUNCTION check_for_plotscr_inclusion(filename as string) as integer
 'This script is a hack to allow people who name their scripts .txt to use the import
 'feature without cluttering the browse interface with non-plotscript .txt files
 'Note that scanscripts.py uses completely different autodetection method
 DIM result as integer = NO
 
 DIM fh as integer = FREEFILE
 OPEN filename FOR INPUT AS #fh
 DIM s as string
 FOR i as integer = 0 TO 29 'Only bother to check the first 30 lines
  INPUT #fh, s
  IF INSTR(LTRIM(LCASE(s)), "include") = 1 ANDALSO INSTR(LCASE(s), "plotscr.hsd") > 0 THEN
   result = YES
   EXIT FOR
  END IF
 NEXT i
 CLOSE #fh
 
 RETURN result
END FUNCTION
