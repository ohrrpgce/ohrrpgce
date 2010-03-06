'OHRRPGCE - File browser
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#include "const.bi"
#include "compat.bi"
#include "allmodex.bi"
#include "common.bi"

#IFDEF IS_CUSTOM
'this is used for showing help
#include "customsubs.bi"
#ENDIF

Type BrowseMenuEntry
	kind as integer
	filename as string
	caption as string
	about as string
End type

Type BrowseMenuState
	nowdir as string
	tmp as string
	treesize as integer   'last entry
	viewsize as integer
	special as integer
	ranalready as integer
	meter as integer
	drivesshown as integer  'number of drive entries (plus 1 for refresh)
End Type

'Subs and functions only used locally
DECLARE SUB draw_browse_meter(br AS BrowseMenuState)
DECLARE SUB browse_add_files(wildcard$, attrib AS INTEGER, BYREF br AS BrowseMenuState, tree() AS BrowseMenuEntry)
DECLARE FUNCTION validmusicfile (file$, as integer = FORMAT_BAM AND FORMAT_MIDI)
DECLARE FUNCTION show_mp3_info() AS STRING

FUNCTION browse (special, default$, fmask$, tmp$, needf, helpkey as string) as string
STATIC remember as string
browse = ""

DIM br AS BrowseMenuState
br.tmp = tmp$
br.special = special

'special=0   no preview
'special=1   just BAM
'special=2   16 color BMP
'special=3   background
'special=4   master palette (*.mas, 8 bit *.bmp, 16x16 24 bit *.bmp) (fmask$ is ignored)
'special=5   any supported music (currently *.bam, *.mid, *.ogg, *.mp3, *.mod, *.xm, *.it, *.s3m formats)  (fmask$ is ignored)
'special=6   any supported SFX (currently *.ogg, *.wav, *.mp3) (fmask$ is ignored)
'special=7   RPG files
mashead$ = CHR$(253) + CHR$(13) + CHR$(158) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(6)
paledithead$ = CHR$(253) + CHR$(217) + CHR$(158) + CHR$(0) + CHR$(0) + CHR$(7) + CHR$(6)

REDIM tree(255) AS BrowseMenuEntry
DIM drive(26) as string, catfg(6) as integer, catbg(6) as integer, f as integer = -1
DIM bmpd as BitmapInfoHeader

'tree().kind contains the type of each object in the menu
'0 = Drive (Windows only)
'1 = Parent Directory
'2 = Subdirectory
'3 = Selectable item
'4 = Root
'5 = Special (not used)
'6 = Unselectable item

showHidden = 0

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
 FOR i = 0 TO 255
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

treeptr = 0
treetop = 0
br.treesize = 0
br.drivesshown = 0
getdrivenames = 0  'whether to fetch names of all drives, on if hit F5

br.ranalready = 0
GOSUB context

changed = 1

setkeys
DO
 setwait 55
 setkeys
 tog = tog XOR 1
 IF keyval(scEsc) > 1 THEN EXIT DO
#IFDEF IS_CUSTOM
 IF keyval(scF1) > 1 THEN show_help helpkey
#ENDIF
 IF usemenu(treeptr, treetop, 0, br.treesize, br.viewsize) OR changed THEN
  alert$ = ""
  changed = 0
  GOSUB hover
 END IF
 IF enter_or_space() THEN
  alert$ = ""
  changed = 1
  IF br.special = 1 OR br.special = 5 THEN pausesong
  SELECT CASE tree(treeptr).kind
   CASE 0
    'this could take a while...
    rectangle 5, 32 + br.viewsize * 9, 310, 12, uilook(uiTextbox + 0), vpage
    edgeprint "Reading...", 8, 34 + br.viewsize * 9, uilook(uiText), vpage
    setvispage vpage
    IF hasmedia(tree(treeptr).filename) THEN
     br.nowdir = tree(treeptr).filename
     GOSUB context
    ELSE
     alert$ = "No media"
     changed = 0
    END IF
   CASE 1, 4
    br.nowdir = ""
    FOR i = br.drivesshown TO treeptr
     br.nowdir = br.nowdir + tree(i).filename
    NEXT i
    GOSUB context
   CASE 2
    br.nowdir = br.nowdir + tree(treeptr).filename + SLASH
    GOSUB context
   CASE 3
    browse = br.nowdir + tree(treeptr).filename
    EXIT DO
  END SELECT
 END IF
 IF keyval(scCtrl) THEN
  'Ctrl + H for hidden
  IF keyval(scH) > 1 THEN
   showHidden = showHidden XOR attribHidden
   GOSUB context
  END IF
 ELSE
  'find by letter
  FOR i = 2 TO 53
   IF keyval(i) > 1 AND keyv(i, 0) > 0 THEN
    FOR j = 1 TO br.treesize
     mappedj = (j + treeptr) MOD (br.treesize + 1)
     tempstr$ = LCASE$(tree(mappedj).caption)
     IF (tree(mappedj).kind = 1 OR tree(mappedj).kind = 2 OR tree(mappedj).kind = 3) AND tempstr$[0] = keyv(i, 0) THEN treeptr = mappedj: EXIT FOR
    NEXT
    EXIT FOR
   END IF
  NEXT i
 END IF
 IF keyval(scF5) > 1 THEN  'F5
  'refresh
  br.drivesshown = 0
  getdrivenames = 1
  GOSUB context
  changed = 1
 END IF
 IF keyval(scBackspace) > 1 THEN 'backspace
  'go up a directory
  FOR i = LEN(br.nowdir) - 1 TO 1 STEP -1
   IF br.nowdir[i - 1] = ASC(SLASH) THEN br.nowdir = LEFT$(br.nowdir, i) : EXIT FOR
  NEXT
  GOSUB context
  changed = 1
 END IF
 edgeboxstyle 4, 3, 312, 14, 0, dpage, NO, YES
 edgeprint br.nowdir, 8, 6, uilook(uiText), dpage
 edgeboxstyle 4, 31 + br.viewsize * 9, 312, 14, 0, dpage, NO, YES
 edgeprint alert$, 8, 34 + br.viewsize * 9, uilook(uiText), dpage
 IF br.special = 7 THEN
  rectangle 0, 190, 320, 10, uilook(uiDisabledItem), dpage
  edgeprint version$ + " " + gfxbackend + "/" + musicbackend, 8, 190, uilook(uiMenuItem), dpage
  textcolor uilook(uiText), 0
 END IF
 textcolor uilook(uiText), 0
 printstr ">", 0, 20 + (treeptr - treetop) * 9, dpage
 FOR i = treetop TO small(treetop + br.viewsize, br.treesize)
  textcolor catfg(tree(i).kind), catbg(tree(i).kind)
  a$ = tree(i).caption
  IF LEN(a$) < 38 AND catbg(tree(i).kind) > 0 THEN a$ = a$ + STRING$(38 - LEN(a$), " ")
  printstr a$, 10, 20 + (i - treetop) * 9, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 IF needf = 1 THEN fadein: setkeys
 IF needf THEN needf = needf - 1
 dowait
LOOP
IF default$ = "" THEN
 remember = br.nowdir
ELSE
 default$ = br.nowdir
END IF
pausesong:if f >= 0 then sound_stop(f, -1): UnloadSound(f)
EXIT FUNCTION

hover:
SELECT CASE br.special
 CASE 1
  pausesong
  IF tree(treeptr).kind = 3 OR tree(treeptr).kind = 6 THEN
   IF validmusicfile(br.nowdir + tree(treeptr).filename, FORMAT_BAM) THEN
    loadsong br.nowdir + tree(treeptr).filename
   ELSE
    alert$ = tree(treeptr).filename + " is not a valid BAM file"
   END IF
  END IF
 CASE 2, 3
  IF bmpinfo(br.nowdir + tree(treeptr).filename, bmpd) THEN
   alert$ = bmpd.biWidth & "*" & bmpd.biHeight & " pixels, " & bmpd.biBitCount & "-bit color"
  END IF
 CASE 4
  IF tree(treeptr).kind = 3 OR tree(treeptr).kind = 6 THEN
   masfh = FREEFILE
   OPEN br.nowdir + tree(treeptr).filename FOR BINARY AS #masfh
   IF LCASE$(justextension$(tree(treeptr).filename)) = "mas" THEN
    a$ = "       "
    GET #masfh, 1, a$
    CLOSE #masfh
    SELECT CASE a$
     CASE mashead$
      alert$ = "MAS format"
     CASE paledithead$
      alert$ = "MAS format (PalEdit)"
     CASE ELSE
     alert$ = "Not a valid MAS file"
    END SELECT
   ELSE
    '.bmp file
    IF bmpinfo(br.nowdir + tree(treeptr).filename, bmpd) THEN
     IF bmpd.biBitCount = 24 THEN
      alert$ = bmpd.biWidth & "*" & bmpd.biHeight & " pixels, " & bmpd.biBitCount & "-bit color"
     ELSE
      alert$ = bmpd.biBitCount & "-bit color BMP"
     END IF
    END IF
   END IF
  END IF
 CASE 5
  pausesong
  alert$ = tree(treeptr).about
  IF validmusicfile(br.nowdir + tree(treeptr).filename, PREVIEWABLE_MUSIC_FORMAT) THEN
   loadsong br.nowdir + tree(treeptr).filename
  ELSEIF getmusictype(br.nowdir + tree(treeptr).filename) = FORMAT_MP3 THEN
   alert$ = show_mp3_info()
  END IF
 CASE 6
  alert$ = tree(treeptr).about
  IF f > -1 THEN
   sound_stop(f,-1)
   UnloadSound(f)
   f = -1
  END IF
  IF tree(treeptr).kind <> 6 THEN
   'not disabled because of size
   IF validmusicfile(br.nowdir + tree(treeptr).filename, PREVIEWABLE_FX_FORMAT) THEN
    f = LoadSound(br.nowdir + tree(treeptr).filename)
    sound_play(f, 0, -1)
   ELSEIF getmusictype(br.nowdir + tree(treeptr).filename) = FORMAT_MP3 THEN
    alert$ = show_mp3_info()
   END IF
  END IF
 CASE 7
  alert$ = tree(treeptr).about
END SELECT
IF tree(treeptr).kind = 0 THEN alert$ = "Drive"
IF tree(treeptr).kind = 1 THEN alert$ = "Directory"
IF tree(treeptr).kind = 2 THEN alert$ = "Subdirectory"
IF tree(treeptr).kind = 4 THEN alert$ = "Root"
RETRACE

context:
'for progress meter
IF br.ranalready THEN rectangle 5, 32 + br.viewsize * 9, 310, 12, uilook(uiTextbox + 0), vpage
br.meter = 0

'erase old list
IF getdrivenames THEN br.treesize = -1 ELSE br.treesize = br.drivesshown - 1
FOR i = br.treesize + 1 TO UBOUND(tree)
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

  drivetotal = drivelist(drive())
  FOR i = 0 TO drivetotal - 1
   br.treesize += 1
   tree(br.treesize).filename = drive(i)
   tree(br.treesize).caption = drive(i)
   tree(br.treesize).kind = 0
   IF getdrivenames THEN
'    IF isremovable(drive(i)) THEN
'     tree(br.treesize).caption += " (removable)"
'    ELSE
     tree(br.treesize).caption += " " + drivelabel$(drive(i))
'    END IF
   END IF
   draw_browse_meter br

  NEXT i
  'could add My Documents to drives list here
 END IF
#ENDIF
br.drivesshown = br.treesize + 1
getdrivenames = 0

IF br.nowdir = "" THEN
ELSE
 a$ = br.nowdir

 '--Current drive
 br.treesize += 1
 b$ = MID$(a$, 1, INSTR(a$, SLASH))
 tree(br.treesize).filename = b$
 tree(br.treesize).kind = 4
#IFDEF __FB_WIN32__
 IF hasmedia(b$) = 0 THEN
  'Somebody pulled out the disk
  changed = 0
  alert$ = "Disk not readable"
  br.treesize -= 1
  treeptr = 0
  treetop = 0
  br.nowdir = ""
  RETRACE
 END IF
 FOR i = 0 TO br.drivesshown - 1
  IF tree(i).filename = b$ THEN
   tmpname$ = drivelabel$(b$)
   IF LEN(tmpname$) THEN tree(i).caption = b$ + " " + tmpname$
   tree(br.treesize).caption = tree(i).caption
   EXIT FOR
  END IF
 NEXT
#ENDIF
 a$ = MID$(a$, INSTR$(a$, SLASH) + 1)
 '--Directories
 b$ = ""
 DO UNTIL a$ = ""
  b$ = b$ + LEFT$(a$, 1)
  a$ = RIGHT$(a$, LEN(a$) - 1)
  IF RIGHT$(b$, 1) = SLASH THEN
#IFDEF __FB_WIN32__
   'Special handling of My Documents in Windows
   IF b$ = "My Documents\" OR b$ = "MYDOCU~1\" THEN
    FOR i = br.treesize TO br.drivesshown STEP -1
     b$ = tree(i).filename + b$
    NEXT i
    br.treesize = br.drivesshown - 1
    tree(br.treesize + 1).caption = "My Documents\"
   END IF
#ENDIF
   br.treesize = br.treesize + 1
   IF br.treesize = UBOUND(tree) THEN REDIM PRESERVE tree(UBOUND(tree) + 256)
   tree(br.treesize).filename = b$
   tree(br.treesize).kind = 1
   b$ = ""
  END IF
 LOOP
 '---FIND ALL SUB-DIRECTORIES IN THE CURRENT DIRECTORY---
 findfiles br.nowdir + ALLFILES, 16, br.tmp + "hrbrowse.tmp"
 fh = FREEFILE
 OPEN br.tmp + "hrbrowse.tmp" FOR INPUT AS #fh
 DO UNTIL EOF(fh)
  br.treesize = br.treesize + 1
  IF br.treesize = UBOUND(tree) THEN REDIM PRESERVE tree(UBOUND(tree) + 256)
  tree(br.treesize).kind = 2
  LINE INPUT #fh, tree(br.treesize).filename
  IF tree(br.treesize).filename = "." OR tree(br.treesize).filename = ".." OR RIGHT$(tree(br.treesize).filename, 4) = ".tmp" THEN br.treesize = br.treesize - 1
  IF br.special = 7 THEN ' Special handling in RPG mode
   IF justextension$(tree(br.treesize).filename) = "rpgdir" THEN br.treesize = br.treesize - 1
  END IF
  draw_browse_meter br
 LOOP
 CLOSE #fh
 safekill br.tmp + "hrbrowse.tmp"
 '---FIND ALL FILES IN FILEMASK---
 attrib = attribAlmostAll OR showHidden
 IF br.special = 4 THEN
  browse_add_files "*.mas", attrib, br, tree()
  browse_add_files "*.bmp", attrib, br, tree()
 ELSEIF br.special = 5 THEN' background music
  '--disregard fmask$. one call per extension
  browse_add_files "*.bam", attrib, br, tree()
  browse_add_files "*.mid", attrib, br, tree()
  browse_add_files "*.xm", attrib, br, tree()
  browse_add_files "*.it", attrib, br, tree()
  browse_add_files "*.mod", attrib, br, tree()
  browse_add_files "*.s3m", attrib, br, tree()
  browse_add_files "*.ogg", attrib, br, tree()
  browse_add_files "*.mp3", attrib, br, tree()
 ELSEIF br.special = 6 THEN ' sound effects
  '--disregard fmask$. one call per extension
  browse_add_files "*.wav", attrib, br, tree()
  browse_add_files "*.ogg", attrib, br, tree()
  browse_add_files "*.mp3", attrib, br, tree()
 ELSEIF br.special = 7 THEN
  'Call once for RPG files once for rpgdirs
  browse_add_files fmask$, attrib, br, tree()
  browse_add_files "*.rpgdir", 16, br, tree()
 ELSE
  browse_add_files fmask$, attrib, br, tree()
 END IF
END IF

'--set display
FOR i = 0 TO br.treesize
 IF LEN(tree(i).caption) = 0 THEN
  tree(i).caption = tree(i).filename
 END IF
NEXT

sortstart = br.treesize
FOR k = 0 TO br.treesize
 WITH tree(k)
  IF .kind = 2 OR .kind = 3 OR .kind = 6 THEN sortstart = k: EXIT FOR
 END WITH
NEXT

'--alphabetize
FOR i = sortstart TO br.treesize - 1
 FOR j = br.treesize TO i + 1 STEP -1
  k = 0
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
FOR o = br.treesize TO sortstart + 1 STEP -1
 FOR i = sortstart + 1 TO o
  IF tree(i).kind < tree(i - 1).kind THEN
   SWAP tree(i), tree(i - 1)
  END IF
 NEXT
NEXT

'--set cursor
treeptr = 0
treetop = 0
FOR i = br.drivesshown TO br.treesize
 'look for first selectable item
 IF tree(i).kind = 3 THEN treeptr = i: EXIT FOR
 'second preference is first subdirectory
 IF tree(i).kind = 2 AND tree(treeptr).kind <> 2 THEN treeptr = i
 'final preference is current (bottommost) directory
 IF tree(i).kind = 1 OR tree(i).kind = 4 THEN treeptr = i
NEXT i
treetop = bound(treetop, treeptr - (br.viewsize + 2), treeptr)

'--don't display progress bar overtop of previous menu
br.ranalready = 1

RETRACE

END FUNCTION

SUB browse_add_files(wildcard$, attrib AS INTEGER, BYREF br AS BrowseMenuState, tree() AS BrowseMenuEntry)
DIM bmpd AS BitmapInfoHeader
DIM tempbuf(79)
DIM f AS STRING
mashead$ = CHR$(253) + CHR$(13) + CHR$(158) + CHR$(0) + CHR$(0) + CHR$(0) + CHR$(6)
paledithead$ = CHR$(253) + CHR$(217) + CHR$(158) + CHR$(0) + CHR$(0) + CHR$(7) + CHR$(6)

DIM filelist AS STRING
filelist = br.tmp + "hrbrowse.tmp"
findfiles br.nowdir + anycase$(wildcard$), attrib, filelist

fh = FREEFILE
OPEN filelist FOR INPUT AS #fh
DO UNTIL EOF(fh)
 br.treesize = br.treesize + 1
 IF br.treesize = UBOUND(tree) THEN REDIM PRESERVE tree(UBOUND(tree) + 256)
 tree(br.treesize).kind = 3
 LINE INPUT #fh, tree(br.treesize).filename
 f = br.nowdir & tree(br.treesize).filename
 '---music files
 IF br.special = 1 OR br.special = 5 THEN
  IF validmusicfile(f, VALID_MUSIC_FORMAT) = 0 THEN
   tree(br.treesize).kind = 6
   tree(br.treesize).about = "Not a valid music file"
  END IF
 END IF
 IF br.special = 6 THEN
  IF validmusicfile(f, VALID_FX_FORMAT) = 0 THEN
   tree(br.treesize).kind = 6
   tree(br.treesize).about = "Not a valid sound effect file"
  ELSEIF FILELEN(f) > 500 * 1024 AND justextension(tree(br.treesize).filename) <> "wav" THEN
   tree(br.treesize).kind = 6
   tree(br.treesize).about = "File is too large (limit 500kB)"
  END IF
 END IF
 '---4-bit BMP browsing
 IF br.special = 2 THEN
  IF bmpinfo(f, bmpd) THEN
   IF bmpd.biBitCount <> 4 OR bmpd.biWidth > 320 OR bmpd.biHeight > 200 THEN
    tree(br.treesize).kind = 6
   END IF
  ELSE
   br.treesize = br.treesize - 1
  END IF
 END IF
 '---320x200x24/8bit BMP files
 IF br.special = 3 THEN
  IF bmpinfo(f, bmpd) THEN
   IF (bmpd.biBitCount <> 24 AND bmpd.biBitCount <> 8) OR bmpd.biWidth <> 320 OR bmpd.biHeight <> 200 THEN
    tree(br.treesize).kind = 6
   END IF
  ELSE
   br.treesize = br.treesize - 1
  END IF
 END IF
 '--master palettes  (why isn't this up there?)
 IF br.special = 4 THEN
  IF LCASE$(justextension$(tree(br.treesize).filename)) = "mas" THEN
   masfh = FREEFILE
   OPEN f FOR BINARY AS #masfh
   a$ = "       "
   GET #masfh, 1, a$
   CLOSE #masfh
   IF a$ <> mashead$ AND a$ <> paledithead$ THEN
    tree(br.treesize).kind = 6
   END IF
  ELSE
   IF bmpinfo(f, bmpd) THEN
    IF (bmpd.biBitCount = 8 OR bmpd.biBitCount = 24 AND (bmpd.biWidth = 16 AND bmpd.biHeight = 16)) = 0 THEN tree(br.treesize).kind = 6
   ELSE
    br.treesize = br.treesize - 1
   END IF
  END IF
 END IF
 '--RPG files
 IF br.special = 7 THEN
  copylump f, "browse.txt", br.tmp, -1
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
 draw_browse_meter br
LOOP
CLOSE #fh
safekill filelist

END SUB

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

FUNCTION validmusicfile (file AS STRING, types = FORMAT_BAM AND FORMAT_MIDI)
'-- actually, doesn't need to be a music file, but only multi-filetype imported data right now
	DIM AS STRING ext, a, realhd
	DIM AS INTEGER musfh, v, chk
	ext = lcase(justextension(file))
	chk = getmusictype(file)

	if (chk AND types) = 0 then return 0

	SELECT CASE chk
	CASE FORMAT_BAM
		a = "    "
		realhd = "CBMF"
		v = 1
	CASE FORMAT_MIDI
		a = "    "
		realhd = "MThd"
		v = 1
	CASE FORMAT_XM
		a =      "                 "
		realhd = "Extended Module: "
		v = 1
	CASE FORMAT_MP3
		return can_convert_mp3()
	END SELECT

	if v then
		musfh = FREEFILE
		OPEN file FOR BINARY AS #musfh
		GET #musfh, 1, a
		CLOSE #musfh
		IF a <> realhd THEN return 0
	end if

	return 1
END FUNCTION

FUNCTION show_mp3_info() AS STRING
 IF can_convert_mp3() THEN
  RETURN "Cannot preview MP3, try importing"
 ELSE
  RETURN "madplay & oggenc required. See README"
 END IF
END FUNCTION
