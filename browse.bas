'OHRRPGCE - File browser
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#include "config.bi"
#include "const.bi"
#include "allmodex.bi"
#include "common.bi"
#include "reload.bi"
#include "os.bi"

OPTION EXPLICIT

Type BrowseMenuEntry
	kind as integer
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
DECLARE SUB build_listing(tree() AS BrowseMenuEntry, BYREF br AS BrowseMenuState)
DECLARE SUB draw_browse_meter(br AS BrowseMenuState)
DECLARE SUB browse_hover(tree() AS BrowseMenuEntry, BYREF br AS BrowseMenuState)
DECLARE SUB browse_add_files(wildcard$, BYVAL filetype AS INTEGER, BYREF br AS BrowseMenuState, tree() AS BrowseMenuEntry)
DECLARE FUNCTION validmusicfile (file$, BYVAL typemask as integer)
DECLARE FUNCTION browse_sanity_check_reload(filename AS STRING, info AS STRING) AS INTEGER

FUNCTION browse (special, default$, fmask AS STRING, tmp$, needf, helpkey as string) as string
STATIC remember as string
browse = ""

DIM br AS BrowseMenuState
br.tmp = tmp$
br.special = special
br.fmask = fmask
br.snd = -1

'special=0   no preview
'special=1   just BAM
'special=2   16 color BMP
'special=3   background
'special=4   master palette (*.mas, 8 bit *.bmp, 16x16 24 bit *.bmp) (fmask is ignored)
'special=5   any supported music (currently *.bam, *.mid, *.ogg, *.mp3, *.mod, *.xm, *.it, *.s3m formats)  (fmask is ignored)
'special=6   any supported SFX (currently *.ogg, *.wav, *.mp3) (fmask is ignored)
'special=7   RPG files
'special=8   RELOAD files
br.mashead = CHR(253) & CHR(13) & CHR(158) & CHR(0) & CHR(0) & CHR(0) & CHR(6)
br.paledithead = CHR(253) & CHR(217) & CHR(158) & CHR(0) & CHR(0) & CHR(7) & CHR(6)

REDIM tree(255) AS BrowseMenuEntry
DIM catfg(6) as integer, catbg(6) as integer

'tree().kind contains the type of each object in the menu
'0 = Drive (Windows only)
'1 = Parent Directory
'2 = Subdirectory
'3 = Selectable item
'4 = Root
'5 = Special (not used)
'6 = Unselectable item

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
 FOR i AS INTEGER = 0 TO 255
  temppal(i).r = 0
  temppal(i).g = 0
  temppal(i).b = 0
 NEXT i
 setpal temppal()
END IF

IF remember = "" THEN remember = curdir$ + SLASH
IF default$ = "" THEN
 br.nowdir = remember
ELSE
 br.nowdir = default$
END IF

IF br.special = 7 THEN br.viewsize = 16 ELSE br.viewsize = 17

br.treeptr = 0
br.treetop = 0
br.treesize = 0
br.drivesshown = 0
br.getdrivenames = 0  'whether to fetch names of all drives, on if hit F5

br.ranalready = 0
build_listing tree(), br

br.changed = 0
IF br.alert = "" THEN br.changed = 1

setkeys
DO
 setwait 55
 setkeys
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
  IF br.special = 1 OR br.special = 5 THEN pausesong
  SELECT CASE tree(br.treeptr).kind
   CASE 0
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
   CASE 1, 4
    br.nowdir = ""
    FOR i AS INTEGER = br.drivesshown TO br.treeptr
     br.nowdir = br.nowdir + tree(i).filename
    NEXT i
    build_listing tree(), br
   CASE 2
    br.nowdir = br.nowdir + tree(br.treeptr).filename + SLASH
    build_listing tree(), br
   CASE 3
    browse = br.nowdir + tree(br.treeptr).filename
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
  DIM intext as string = LEFT(getinputtext, 1)
  IF LEN(intext) > 0 THEN
   FOR j AS INTEGER = 1 TO br.treesize
    DIM mappedj AS INTEGER
    mappedj = (j + br.treeptr) MOD (br.treesize + 1)
    IF (tree(mappedj).kind = 1 OR tree(mappedj).kind = 2 OR tree(mappedj).kind = 3) THEN
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
  FOR i AS INTEGER = LEN(br.nowdir) - 1 TO 1 STEP -1
   IF br.nowdir[i - 1] = ASC(SLASH) THEN br.nowdir = LEFT$(br.nowdir, i) : EXIT FOR
  NEXT
  build_listing tree(), br
  br.changed = 1
 END IF
 '--Draw screen
 clearpage dpage
 edgeboxstyle 4, 3, 312, 14, 0, dpage, NO, YES
 edgeprint br.nowdir, 8, 6, uilook(uiText), dpage
 edgeboxstyle 4, 31 + br.viewsize * 9, 312, 14, 0, dpage, NO, YES
 edgeprint br.alert, 8, 34 + br.viewsize * 9, uilook(uiText), dpage
 IF br.special = 7 THEN
  rectangle 0, 190, 320, 10, uilook(uiDisabledItem), dpage
  edgeprint version$ + " " + gfxbackend + "/" + musicbackend, 8, 190, uilook(uiMenuItem), dpage
  textcolor uilook(uiText), 0
 END IF
 textcolor uilook(uiText), 0
 printstr ">", 0, 20 + (br.treeptr - br.treetop) * 9, dpage
 FOR i AS INTEGER = br.treetop TO small(br.treetop + br.viewsize, br.treesize)
  textcolor catfg(tree(i).kind), catbg(tree(i).kind)
  DIM a AS STRING = tree(i).caption
  IF LEN(a) < 38 AND catbg(tree(i).kind) > 0 THEN a = a + STRING(38 - LEN(a), " ")
  printstr a, 10, 20 + (i - br.treetop) * 9, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 IF needf = 1 THEN fadein: setkeys
 IF needf THEN needf = needf - 1
 dowait
LOOP
IF default$ = "" THEN
 remember = br.nowdir
ELSE
 default$ = br.nowdir
END IF
pausesong:if br.snd >= 0 then sound_stop(br.snd, -1): UnloadSound(br.snd)

END FUNCTION

SUB browse_hover(tree() AS BrowseMenuEntry, BYREF br AS BrowseMenuState)
 DIM bmpd as BitmapInfoHeader
 SELECT CASE br.special
  CASE 1 'music bam only (is this still used?)
   pausesong
   IF tree(br.treeptr).kind = 3 OR tree(br.treeptr).kind = 6 THEN
    IF validmusicfile(br.nowdir + tree(br.treeptr).filename, FORMAT_BAM) THEN
     loadsong br.nowdir + tree(br.treeptr).filename
    ELSE
     br.alert = tree(br.treeptr).filename + " is not a valid BAM file"
    END IF
   END IF
  CASE 2, 3 'bitmaps
   IF bmpinfo(br.nowdir + tree(br.treeptr).filename, bmpd) THEN
    br.alert = bmpd.biWidth & "*" & bmpd.biHeight & " pixels, " & bmpd.biBitCount & "-bit color"
   END IF
  CASE 4 'palettes
   IF tree(br.treeptr).kind = 3 OR tree(br.treeptr).kind = 6 THEN
    DIM masfh AS INTEGER = FREEFILE
    OPEN br.nowdir + tree(br.treeptr).filename FOR BINARY AS #masfh
    IF LCASE$(justextension$(tree(br.treeptr).filename)) = "mas" THEN
     DIM a AS STRING = "       "
     GET #masfh, 1, a
     CLOSE #masfh
     SELECT CASE a
      CASE br.mashead
       br.alert = "MAS format"
      CASE br.paledithead
       br.alert = "MAS format (PalEdit)"
      CASE ELSE
      br.alert = "Not a valid MAS file"
     END SELECT
    ELSE
     '.bmp file
     IF bmpinfo(br.nowdir + tree(br.treeptr).filename, bmpd) THEN
      IF bmpd.biBitCount = 24 THEN
       br.alert = bmpd.biWidth & "*" & bmpd.biHeight & " pixels, " & bmpd.biBitCount & "-bit color"
      ELSE
       br.alert = bmpd.biBitCount & "-bit color BMP"
      END IF
     END IF
    END IF
   END IF
  CASE 5 'music
   pausesong
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
   IF tree(br.treeptr).kind <> 6 THEN
    'not disabled because of size
    IF validmusicfile(br.nowdir + tree(br.treeptr).filename, PREVIEWABLE_FX_FORMAT) THEN
     br.snd = LoadSound(br.nowdir + tree(br.treeptr).filename)
     sound_play(br.snd, 0, -1)
    ELSEIF getmusictype(br.nowdir + tree(br.treeptr).filename) = FORMAT_MP3 THEN
     br.alert = "Cannot preview MP3, try importing"
    END IF
   END IF
  CASE 7 'rpg
   br.alert = tree(br.treeptr).about
  CASE 8 'reload
   br.alert = tree(br.treeptr).about
 END SELECT
 IF tree(br.treeptr).kind = 0 THEN br.alert = "Drive"
 IF tree(br.treeptr).kind = 1 THEN br.alert = "Directory"
 IF tree(br.treeptr).kind = 2 THEN br.alert = "Subdirectory"
 IF tree(br.treeptr).kind = 4 THEN br.alert = "Root"
END SUB

SUB browse_add_files(wildcard$, BYVAL filetype AS INTEGER, BYREF br AS BrowseMenuState, tree() AS BrowseMenuEntry)
DIM bmpd AS BitmapInfoHeader
DIM tempbuf(79)
DIM filename AS STRING

DIM filelist() AS STRING
findfiles br.nowdir, wildcard$, filetype, br.showhidden, filelist()

FOR i AS INTEGER = 0 TO UBOUND(filelist)
 br.treesize = br.treesize + 1
 IF br.treesize = UBOUND(tree) THEN REDIM PRESERVE tree(UBOUND(tree) + 256)
 tree(br.treesize).kind = 3
 tree(br.treesize).filename = filelist(i)
 filename = br.nowdir & tree(br.treesize).filename
 '---music files
 IF br.special = 1 OR br.special = 5 THEN
  IF validmusicfile(filename, VALID_MUSIC_FORMAT) = 0 THEN
   tree(br.treesize).kind = 6
   tree(br.treesize).about = "Not a valid music file"
  END IF
 END IF
 IF br.special = 6 THEN
  IF validmusicfile(filename, VALID_FX_FORMAT) = 0 THEN
   tree(br.treesize).kind = 6
   tree(br.treesize).about = "Not a valid sound effect file"
  ELSEIF FILELEN(filename) > 500 * 1024 AND justextension(filename) <> "wav" THEN
   tree(br.treesize).kind = 6
   tree(br.treesize).about = "File is too large (limit 500kB)"
  END IF
 END IF
 '---4-bit BMP browsing
 IF br.special = 2 THEN
  IF bmpinfo(filename, bmpd) THEN
   IF bmpd.biBitCount <> 4 OR bmpd.biWidth > 320 OR bmpd.biHeight > 200 THEN
    tree(br.treesize).kind = 6
   END IF
  ELSE
   br.treesize = br.treesize - 1
  END IF
 END IF
 '---320x200x24/8bit BMP files
 IF br.special = 3 THEN
  IF bmpinfo(filename, bmpd) THEN
   IF (bmpd.biBitCount <> 24 AND bmpd.biBitCount <> 8) OR bmpd.biWidth <> 320 OR bmpd.biHeight <> 200 THEN
    tree(br.treesize).kind = 6
   END IF
  ELSE
   br.treesize = br.treesize - 1
  END IF
 END IF
 '--master palettes  (why isn't this up there?)
 IF br.special = 4 THEN
  IF LCASE$(justextension$(filename)) = "mas" THEN
   DIM masfh AS INTEGER = FREEFILE
   OPEN filename FOR BINARY AS #masfh
   DIM a AS STRING = "       "
   GET #masfh, 1, a
   CLOSE #masfh
   IF a <> br.mashead AND a <> br.paledithead THEN
    tree(br.treesize).kind = 6
   END IF
  ELSE  'BMP as a master palette
   IF bmpinfo(filename, bmpd) THEN
    IF (bmpd.biBitCount = 8 OR bmpd.biBitCount = 24 AND (bmpd.biWidth = 16 AND bmpd.biHeight = 16)) = 0 THEN tree(br.treesize).kind = 6
   ELSE
    br.treesize = br.treesize - 1
   END IF
  END IF
 END IF
 '--RPG files
 IF br.special = 7 THEN
  copylump filename, "browse.txt", br.tmp, -1
  IF loadrecord(tempbuf(), br.tmp + "browse.txt", 40, , NO) THEN
   tree(br.treesize).caption = readbinstring(tempbuf(), 0, 38)
   tree(br.treesize).about = readbinstring(tempbuf(), 20, 38)
   safekill br.tmp + "browse.txt"
   IF LEN(tree(br.treesize).caption) = 0 THEN tree(br.treesize).caption = tree(br.treesize).filename
  ELSE
   tree(br.treesize).about = ""
   tree(br.treesize).caption = tree(br.treesize).filename
  END IF
 END IF
 '--RELOAD files
 IF br.special = 8 THEN
  IF browse_sanity_check_reload(filename, tree(br.treesize).about) = NO THEN
   tree(br.treesize).kind = 6 'grey out bad ones
  END IF
 END IF
 draw_browse_meter br
NEXT

END SUB

FUNCTION browse_sanity_check_reload(filename AS STRING, info AS STRING) AS INTEGER
 'info argument will be modified
 DIM header AS STRING = "    "
 DIM fh AS INTEGER = FREEFILE
 OPEN filename FOR BINARY ACCESS READ AS #fh
  GET #fh, 1, header
 CLOSE #fh
 IF header <> "RELD" THEN info = "Has no RELOAD file header." : RETURN NO
 DIM doc AS Reload.Docptr
 doc = Reload.LoadDocument(filename)
 IF doc = 0 THEN info = "Reload document not loadable." : RETURN NO
 DIM node AS Reload.Nodeptr
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

SUB draw_browse_meter(br AS BrowseMenuState)
IF br.treesize AND 15 THEN EXIT SUB
WITH br
 IF .ranalready THEN
  .meter = small(.meter + 1, 308)
  rectangle 5 + .meter, 33 + .viewsize * 9, 2, 5, uilook(uiTextbox + 1), vpage
  setvispage vpage 'refresh
 END IF
END WITH
END SUB

FUNCTION validmusicfile (file AS STRING, BYVAL typemask AS INTEGER)
'-- actually, doesn't need to be a music file, but only multi-filetype imported data right now
	DIM AS STRING hdmask, realhd
	DIM AS INTEGER musfh, chk
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
		OPEN file FOR BINARY AS #musfh
		GET #musfh, 1, hdmask
		CLOSE #musfh
		IF hdmask <> realhd THEN return 0
	end if

	return 1
END FUNCTION

SUB build_listing(tree() AS BrowseMenuEntry, BYREF br AS BrowseMenuState)
 'for progress meter
 IF br.ranalready THEN rectangle 5, 32 + br.viewsize * 9, 310, 12, uilook(uiTextbox + 0), vpage
 br.meter = 0

 'erase old list
 IF br.getdrivenames THEN br.treesize = -1 ELSE br.treesize = br.drivesshown - 1
 FOR i AS INTEGER = br.treesize + 1 TO UBOUND(tree)
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
  'tree(br.treesize).kind = 5

  DIM drive(26) AS STRING
  DIM drivetotal AS INTEGER = drivelist(drive())
  FOR i AS INTEGER = 0 TO drivetotal - 1
   br.treesize += 1
   tree(br.treesize).filename = drive(i)
   tree(br.treesize).caption = drive(i)
   tree(br.treesize).kind = 0
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
  DIM a AS STRING = br.nowdir
  DIM b AS STRING

  '--Current drive
  br.treesize += 1
  b = MID(a, 1, INSTR(a, SLASH))
  tree(br.treesize).filename = b
  tree(br.treesize).kind = 4
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
  FOR i AS INTEGER = 0 TO br.drivesshown - 1
   IF tree(i).filename = b THEN
    DIM tmpname AS STRING = drivelabel(b)
    IF LEN(tmpname) THEN tree(i).caption = b + " " + tmpname
    tree(br.treesize).caption = tree(i).caption
    EXIT FOR
   END IF
  NEXT
#ENDIF
  a = MID$(a, INSTR$(a, SLASH) + 1)
  '--Directories
  b = ""
  DO UNTIL a = ""
   b = b + LEFT$(a, 1)
   a = RIGHT$(a, LEN(a) - 1)
   IF RIGHT$(b, 1) = SLASH THEN
#IFDEF __FB_WIN32__
    'Special handling of My Documents in Windows
    IF b = "My Documents\" OR b = "MYDOCU~1\" THEN
     FOR i AS INTEGER = br.treesize TO br.drivesshown STEP -1
      b = tree(i).filename + b
     NEXT i
     br.treesize = br.drivesshown - 1
     tree(br.treesize + 1).caption = "My Documents\"
    END IF
#ENDIF
    br.treesize = br.treesize + 1
    IF br.treesize = UBOUND(tree) THEN REDIM PRESERVE tree(UBOUND(tree) + 256)
    tree(br.treesize).filename = b
    tree(br.treesize).kind = 1
    b = ""
   END IF
  LOOP
  '---FIND ALL SUB-DIRECTORIES IN THE CURRENT DIRECTORY---
  DIM filelist() AS STRING
  findfiles br.nowdir, ALLFILES, fileTypeDirectory, br.showhidden, filelist()
  FOR i AS INTEGER = 0 TO UBOUND(filelist)
   br.treesize = br.treesize + 1
   IF br.treesize = UBOUND(tree) THEN REDIM PRESERVE tree(UBOUND(tree) + 256)
   tree(br.treesize).kind = 2
   tree(br.treesize).filename = filelist(i)
   IF tree(br.treesize).filename = "." OR tree(br.treesize).filename = ".." OR RIGHT$(tree(br.treesize).filename, 4) = ".tmp" THEN br.treesize = br.treesize - 1
   IF br.special = 7 THEN ' Special handling in RPG mode
    IF justextension$(tree(br.treesize).filename) = "rpgdir" THEN br.treesize = br.treesize - 1
   END IF
   IF br.special <> 8 THEN
    '--hide any .saves folders when browsing (except in RELOAD special mode)
    IF justextension$(tree(br.treesize).filename) = "saves" THEN br.treesize = br.treesize - 1
   END IF
#IFDEF __FB_DARWIN__
   IF justextension$(tree(br.treesize).filename) = "app" THEN br.treesize = br.treesize - 1
#ENDIF
   draw_browse_meter br
  NEXT
  '---FIND ALL FILES IN FILEMASK---
  DIM filetype AS INTEGER = fileTypeFile
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
  ELSE
   browse_add_files br.fmask, filetype, br, tree()
  END IF
 END IF

 '--set display
 FOR i AS INTEGER = 0 TO br.treesize
  IF LEN(tree(i).caption) = 0 THEN
   tree(i).caption = tree(i).filename
  END IF
 NEXT

 DIM sortstart AS INTEGER = br.treesize
 FOR k AS INTEGER = 0 TO br.treesize
  WITH tree(k)
   IF .kind = 2 OR .kind = 3 OR .kind = 6 THEN sortstart = k: EXIT FOR
  END WITH
 NEXT

 '--alphabetize
 FOR i AS INTEGER = sortstart TO br.treesize - 1
  FOR j AS INTEGER = br.treesize TO i + 1 STEP -1
   DIM k AS INTEGER = 0
   DIM chara AS INTEGER
   DIM charb AS INTEGER
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
 FOR o AS INTEGER = br.treesize TO sortstart + 1 STEP -1
  FOR i AS INTEGER = sortstart + 1 TO o
   IF tree(i).kind < tree(i - 1).kind THEN
    SWAP tree(i), tree(i - 1)
   END IF
  NEXT
 NEXT

 '--set cursor
 br.treeptr = 0
 br.treetop = 0
 FOR i AS INTEGER = br.drivesshown TO br.treesize
  'look for first selectable item
  IF tree(i).kind = 3 THEN br.treeptr = i: EXIT FOR
  'second preference is first subdirectory
  IF tree(i).kind = 2 AND tree(br.treeptr).kind <> 2 THEN br.treeptr = i
  'final preference is current (bottommost) directory
  IF tree(i).kind = 1 OR tree(i).kind = 4 THEN br.treeptr = i
 NEXT i
 br.treetop = small(br.treeptr - 2, br.treesize - br.viewsize)
 br.treetop = large(br.treetop, 0)

 '--don't display progress bar overtop of previous menu
 br.ranalready = 1

END SUB
