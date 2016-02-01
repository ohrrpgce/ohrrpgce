'OHRRPGCE - File browser
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

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
	mstate as MenuState
	special as integer
	ranalready as bool
	meter as integer
	drivesshown as integer  'number of drive entries (plus 1 for "refresh" option, if uncommented)
	alert as string
	mashead as string
	paledithead as string
	showHidden as bool
	getdrivenames as bool   'Poll drive names on Windows? (can be slow)
	fmask as string
	snd as integer          'Slot of currently playing sound, or -1
End Type

'Subs and functions only used locally
DECLARE SUB build_listing(tree() as BrowseMenuEntry, byref br as BrowseMenuState)
DECLARE SUB draw_browse_meter(br as BrowseMenuState)
DECLARE SUB browse_hover(tree() as BrowseMenuEntry, byref br as BrowseMenuState)
DECLARE SUB browse_hover_file(tree() as BrowseMenuEntry, byref br as BrowseMenuState)
DECLARE SUB browse_add_files(wildcard as string, byval filetype as integer, byref br as BrowseMenuState, tree() as BrowseMenuEntry)
DECLARE FUNCTION validmusicfile (file as string, byval typemask as integer) as integer
DECLARE FUNCTION browse_sanity_check_reload(filename as string, info as string) as integer
DECLARE FUNCTION check_for_plotscr_inclusion(filename as string) as bool

FUNCTION browse (byval special as integer, default as string, fmask as string, tmp as string, byref needf as integer, helpkey as string) as string
STATIC remember as string
DIM ret as string

DIM selectst as SelectTypeState
DIM br as BrowseMenuState
br.tmp = tmp
br.special = special
br.fmask = fmask
br.snd = -1

'special=0   no preview
'special=1   just BAM
'special=2   any BMP (sprite import)
'special=3   320x200 background
'special=4   master palette (*.mas, 8 bit *.bmp, 16x16 24/32 bit *.bmp) (fmask is ignored)
'special=5   any supported music (currently *.bam, *.mid, *.ogg, *.mp3, *.mod, *.xm, *.it, *.s3m formats)  (fmask is ignored)
'special=6   any supported SFX (currently *.ogg, *.wav, *.mp3) (fmask is ignored)
'special=7   RPG files
'special=8   RELOAD files
'special=9   script files (.hs, .hss)
'special=10  2, 16 or 256 colour BMP, any size (used by font_test_menu only)
'special=11  Browse for a folder
'special=12  tilemaps (fmask is ignored)

br.mashead = CHR(253) & CHR(13) & CHR(158) & CHR(0) & CHR(0) & CHR(0) & CHR(6)
br.paledithead = CHR(253) & CHR(217) & CHR(158) & CHR(0) & CHR(0) & CHR(7) & CHR(6)

'tree().kind contains the type of each object in the menu
REDIM tree(255) as BrowseMenuEntry
DIM catfg(6) as integer, catbg(6) as integer

br.showHidden = NO

'FIXME: do we need another uilook() constant for these "blue" directories
' instead of boxlook(0).edgecol ?
catfg(0) = uilook(uiMenuItem)   : catbg(0) = uilook(uiHighlight)    'selectable drives (none on unix systems)
catfg(1) = boxlook(0).edgecol   : catbg(1) = uilook(uiDisabledItem) 'directories
catfg(2) = boxlook(0).edgecol   : catbg(2) = uilook(uiBackground)   'subdirectories
catfg(3) = uilook(uiMenuItem)   : catbg(3) = uilook(uiBackground)   'files
catfg(4) = boxlook(0).edgecol   : catbg(4) = uilook(uiDisabledItem) 'root of current drive
catfg(5) = boxlook(1).edgecol   : catbg(5) = uilook(uiDisabledItem) 'special (never used???)
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
default = simplify_path(default)
IF isdir(default) THEN
 br.nowdir = default & SLASH
 startfile = ""
ELSE
 br.nowdir = trimfilename(default) & SLASH
 startfile = trimpath(default)
END IF

IF br.special = 7 THEN br.mstate.size = 16 ELSE br.mstate.size = 17

br.mstate.pt = 0
br.mstate.top = 0
br.mstate.last = 0
br.drivesshown = 0
br.getdrivenames = NO

br.ranalready = NO
build_listing tree(), br

IF LEN(startfile) THEN
 FOR i as integer = 0 TO br.mstate.last
  IF tree(i).filename = startfile THEN br.mstate.pt = i
 NEXT
END IF

'br.mstate.need_update used only to indicate hover text (.alert) needs update
br.mstate.need_update = NO
IF br.alert = "" THEN br.mstate.need_update = YES  'Don't clobber possible alert from build_listing

setkeys YES
DO
 setwait 55
 setkeys YES
 IF keyval(scEsc) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help helpkey
 IF usemenu(br.mstate) THEN br.mstate.need_update = YES
 IF keyval(scSpace) > 0 AND LEN(selectst.query) > 0 THEN
  'While typing a string, space doesn't cause selection
 ELSEIF enter_space_click(br.mstate) THEN
  br.alert = ""
  br.mstate.need_update = YES
  IF br.special = 1 OR br.special = 5 THEN music_stop
  SELECT CASE tree(br.mstate.pt).kind
   CASE bkDrive
    'this could take a while...
    rectangle 5, 32 + br.mstate.size * 9, 310, 12, boxlook(0).edgecol, vpage
    edgeprint "Reading...", 8, 34 + br.mstate.size * 9, uilook(uiText), vpage
    setvispage vpage
    IF hasmedia(tree(br.mstate.pt).filename) THEN
     br.nowdir = tree(br.mstate.pt).filename
     build_listing tree(), br
    ELSE
     br.alert = "No media"
     br.mstate.need_update = NO
    END IF
   CASE bkParentDir, bkRoot
    br.nowdir = ""
    FOR i as integer = br.drivesshown TO br.mstate.pt
     br.nowdir = br.nowdir + tree(i).filename
    NEXT i
    build_listing tree(), br
   CASE bkSubDir
    br.nowdir = br.nowdir + tree(br.mstate.pt).filename + SLASH
    build_listing tree(), br
   CASE bkSelectable
    ret = br.nowdir + tree(br.mstate.pt).filename
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
  IF select_by_typing(selectst) THEN
   DIM index as integer = br.mstate.pt
   IF LEN(selectst.query) = 1 THEN index = loopvar(index, 0, br.mstate.last)
   FOR ctr as integer = 0 TO br.mstate.last
    'IF (tree(index).kind = bkParentDir OR tree(index).kind = bkSubDir OR tree(index).kind = bkSelectable) THEN
     'Search both display name (preferentially) and filename
     selectst.query_at = find_on_word_boundary(LCASE(tree(index).caption), selectst.query)
     IF selectst.query_at = 0 THEN
      IF INSTR(LCASE(tree(index).filename), selectst.query) = 1 THEN selectst.query_at = -1  'invisible match
     END IF
     IF selectst.query_at THEN
      br.mstate.pt = index
      selectst.remember_pt = index
      br.mstate.need_update = YES
      EXIT FOR
     END IF
    'END IF
    index = loopvar(index, 0, br.mstate.last)
   NEXT
  END IF
 END IF
 IF keyval(scF5) > 1 THEN
  'refresh
  br.drivesshown = 0
  br.getdrivenames = 1
  build_listing tree(), br
  br.mstate.need_update = YES
 END IF
 IF keyval(scBackspace) > 1 THEN
  'Go up a directory
  br.nowdir = parentdir(br.nowdir)
  build_listing tree(), br
  br.mstate.need_update = YES
 END IF
 IF br.mstate.need_update THEN
  br.alert = ""
  br.mstate.need_update = NO
  browse_hover tree(), br
 END IF

 '--Draw screen
 clearpage dpage
 edgeboxstyle 4, 3, 312, 14, 0, dpage, NO, YES
 IF br.special = 7 AND tree(br.mstate.pt).kind = bkSelectable THEN
  'Selected item is an RPG
  edgeprint shorten_to_left(br.nowdir + tree(br.mstate.pt).filename, 304), 8, 6, uilook(uiText), dpage
 ELSE
  edgeprint br.nowdir, 8, 6, uilook(uiText), dpage
 END IF
 edgeboxstyle 4, 31 + br.mstate.size * 9, 312, 14, 0, dpage, NO, YES
 edgeprint br.alert, 8, 34 + br.mstate.size * 9, uilook(uiText), dpage
 IF br.special = 7 THEN
  rectangle 0, 190, 320, 10, uilook(uiDisabledItem), dpage
  edgeprint version & " " & gfxbackend & "/" & musicbackend, 8, 190, uilook(uiMenuItem), dpage
  textcolor uilook(uiText), 0
 END IF
 textcolor uilook(uiText), 0
 printstr ">", 0, 20 + (br.mstate.pt - br.mstate.top) * 9, dpage
 'This mess here because this menu doesn't use a standard MenuState
 IF selectst.remember_pt <> br.mstate.pt THEN select_clear selectst
 selectst.remember_pt = br.mstate.pt

 WITH br.mstate
  .has_been_drawn = YES
  .rect.x = 10
  .rect.y = 20
  .rect.wide = get_resolution_w()
  .rect.high = get_resolution_h()
  .spacing = 9
 END WITH
 FOR i as integer = br.mstate.top TO small(br.mstate.top + br.mstate.size, br.mstate.last)
  textcolor catfg(tree(i).kind), catbg(tree(i).kind)
  DIM caption as string = tree(i).caption
  IF LEN(caption) < 38 AND catbg(tree(i).kind) > 0 THEN caption += STRING(38 - LEN(caption), " ")
  IF i = br.mstate.pt THEN caption = highlight_menu_typing_selection_string(caption, selectst)
  printstr caption, 10, 20 + (i - br.mstate.top) * 9, dpage, YES
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
 SELECT CASE tree(br.mstate.pt).kind
  CASE bkDrive
   br.alert = "Drive"
  CASE bkParentDir
   br.alert = "Directory"
  CASE bkSubDir
   br.alert = "Subdirectory"
  CASE bkRoot
   br.alert = "Root directory"
  CASE bkSelectable, bkUnselectable
   browse_hover_file tree(), br
 END SELECT
END SUB

SUB browse_hover_file(tree() as BrowseMenuEntry, byref br as BrowseMenuState)
 SELECT CASE br.special
  CASE 1 'music bam only (is this still used?)
   music_stop
   IF tree(br.mstate.pt).kind = bkSelectable OR tree(br.mstate.pt).kind = bkUnselectable THEN
    IF validmusicfile(br.nowdir + tree(br.mstate.pt).filename, FORMAT_BAM) THEN
     loadsong br.nowdir + tree(br.mstate.pt).filename
    ELSE
     br.alert = tree(br.mstate.pt).filename + " is not a valid BAM file"
    END IF
   END IF
  CASE 5 'music
   music_stop
   br.alert = tree(br.mstate.pt).about
   IF validmusicfile(br.nowdir + tree(br.mstate.pt).filename, PREVIEWABLE_MUSIC_FORMAT) THEN
    loadsong br.nowdir + tree(br.mstate.pt).filename
   ELSEIF getmusictype(br.nowdir + tree(br.mstate.pt).filename) = FORMAT_MP3 THEN
    br.alert = "Cannot preview MP3, try importing"
   END IF
  CASE 6 'sfx
   br.alert = tree(br.mstate.pt).about
   IF br.snd > -1 THEN
    sound_stop(br.snd,-1)
    UnloadSound(br.snd)
    br.snd = -1
   END IF
   IF tree(br.mstate.pt).kind <> bkUnselectable THEN
    'not disabled because of size
    IF validmusicfile(br.nowdir + tree(br.mstate.pt).filename, PREVIEWABLE_FX_FORMAT) THEN
     br.snd = LoadSound(br.nowdir + tree(br.mstate.pt).filename)
     sound_play(br.snd, 0, -1)
    ELSEIF getmusictype(br.nowdir + tree(br.mstate.pt).filename) = FORMAT_MP3 THEN
     br.alert = "Cannot preview MP3, try importing"
    END IF
   END IF
  CASE 9 'scripts
   IF LCASE(justextension(tree(br.mstate.pt).filename)) = "hs" THEN
    br.alert = "Compiled HamsterSpeak scripts"
   ELSE
    br.alert = "HamsterSpeak scripts"
   END IF
  CASE ELSE
   br.alert = tree(br.mstate.pt).about
 END SELECT
END SUB

'Returns true if the BMP looks good
FUNCTION browse_check_bmp(byref br as BrowseMenuState, tree() as BrowseMenuEntry, byref bmpd as BitmapV3InfoHeader) as integer
 DIM support as integer = bmpinfo(br.nowdir + tree(br.mstate.last).filename, bmpd)
 IF support = 2 THEN
  tree(br.mstate.last).about = bmpd.biWidth & "*" & bmpd.biHeight & " pixels, " & bmpd.biBitCount & "-bit color"
  RETURN YES
 ELSEIF support = 1 THEN
  tree(br.mstate.last).about = "Unsupported BMP file"
 ELSE
  tree(br.mstate.last).about = "Invalid BMP file"
 END IF
 tree(br.mstate.last).kind = bkUnselectable
 RETURN NO
END FUNCTION

SUB browse_add_files(wildcard as string, byval filetype as integer, byref br as BrowseMenuState, tree() as BrowseMenuEntry)
DIM bmpd as BitmapV3InfoHeader
DIM tempbuf(79) as integer
DIM filename as string

DIM filelist() as string
findfiles br.nowdir, wildcard, filetype, br.showhidden, filelist()

FOR i as integer = 0 TO UBOUND(filelist)
 br.mstate.last = br.mstate.last + 1
 IF br.mstate.last = UBOUND(tree) THEN REDIM PRESERVE tree(UBOUND(tree) + 256)
 tree(br.mstate.last).kind = bkSelectable
 tree(br.mstate.last).filename = filelist(i)
 filename = br.nowdir & tree(br.mstate.last).filename
 '---music files
 IF br.special = 1 OR br.special = 5 THEN
  IF validmusicfile(filename, VALID_MUSIC_FORMAT) = 0 THEN
   tree(br.mstate.last).kind = bkUnselectable
   tree(br.mstate.last).about = "Not a valid music file"
  END IF
 END IF
 IF br.special = 6 THEN
  IF validmusicfile(filename, VALID_FX_FORMAT) = 0 THEN
   tree(br.mstate.last).kind = bkUnselectable
   tree(br.mstate.last).about = "Not a valid sound effect file"
  ELSEIF FILELEN(filename) > 500 * 1024 AND LCASE(justextension(filename)) <> "wav" THEN
   tree(br.mstate.last).kind = bkUnselectable
   tree(br.mstate.last).about = "File is too large (limit 500kB)"
  END IF
 END IF
 '---Any BMP
 IF br.special = 2 THEN
  IF browse_check_bmp(br, tree(), bmpd) = NO THEN
   tree(br.mstate.last).kind = bkUnselectable
  END IF
 END IF
 '---320x200 BMP files (any supported bitdepth)
 IF br.special = 3 THEN
  IF browse_check_bmp(br, tree(), bmpd) = NO OR bmpd.biWidth <> 320 OR bmpd.biHeight <> 200 THEN
   tree(br.mstate.last).kind = bkUnselectable
  END IF
 END IF
 '---1/4/8 bit BMP files (fonts)
 IF br.special = 10 THEN
  IF browse_check_bmp(br, tree(), bmpd) = NO OR bmpd.biBitCount > 8 THEN
   tree(br.mstate.last).kind = bkUnselectable
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
     tree(br.mstate.last).about = "MAS format"
    CASE br.paledithead
     tree(br.mstate.last).about = "MAS format (PalEdit)"
    CASE ELSE
     tree(br.mstate.last).about = "Not a valid MAS file"
     tree(br.mstate.last).kind = bkUnselectable
   END SELECT
  ELSE  'BMP as a master palette
   IF browse_check_bmp(br, tree(), bmpd) = NO THEN
    tree(br.mstate.last).kind = bkUnselectable
   ELSE
    IF bmpd.biBitCount <= 8 THEN
     'Don't care about the dimensions
     tree(br.mstate.last).about = bmpd.biBitCount & "-bit color BMP"
    ELSEIF bmpd.biBitCount >= 24 AND (bmpd.biWidth = 16 AND bmpd.biHeight = 16) THEN
     'ok
    ELSE
     tree(br.mstate.last).kind = bkUnselectable
    END IF
   END IF
  END IF
 END IF
 '--RPG files
 IF br.special = 7 THEN
  copylump filename, "browse.txt", br.tmp, YES
  tree(br.mstate.last).caption = load_gamename(br.tmp & "browse.txt")
  tree(br.mstate.last).about = load_aboutline(br.tmp & "browse.txt")
  safekill br.tmp & "browse.txt"
  IF tree(br.mstate.last).caption = "" THEN tree(br.mstate.last).caption = tree(br.mstate.last).filename
 END IF
 '--RELOAD files
 IF br.special = 8 THEN
  IF browse_sanity_check_reload(filename, tree(br.mstate.last).about) = NO THEN
   tree(br.mstate.last).kind = bkUnselectable 'grey out bad ones
  END IF
 END IF
 '--script files
 IF br.special = 9 THEN
  IF wildcard = "*.txt" THEN
   IF NOT check_for_plotscr_inclusion(filename) THEN
    'Don't display .txt files unless they include plotscr.hsd
    br.mstate.last = br.mstate.last - 1
   END IF
  END IF
 END IF
 '--tilemaps
 IF br.special = 12 THEN
  DIM info as TilemapInfo
  IF GetTilemapInfo(filename, info) = NO THEN
   tree(br.mstate.last).kind = bkUnselectable
  END IF
  tree(br.mstate.last).about = "Size " & info.wide & "x" & info.high & " tilemap with " & info.layers & " layers"
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
IF br.mstate.last AND 15 THEN EXIT SUB
WITH br
 IF .ranalready THEN
  .meter = small(.meter + 1, 308)
  rectangle 5 + .meter, 33 + .mstate.size * 9, 2, 5, boxlook(0).bgcol, vpage
  setvispage vpage 'refresh
 END IF
END WITH
END SUB

FUNCTION validmusicfile (file as string, byval typemask as integer) as integer
 '-- actually, doesn't need to be a music file, but only multi-filetype imported data right now
 DIM as string hdmask, realhd
 DIM as integer musfh, chk
 chk = getmusictype(file)

 IF (chk AND typemask) = 0 THEN return 0

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
 'Other supported module formats are missing, but I don't see any point adding them
 END SELECT

 IF LEN(hdmask) THEN
  musfh = FREEFILE
  OPEN file FOR BINARY as #musfh
  GET #musfh, 1, hdmask
  CLOSE #musfh
  IF hdmask <> realhd THEN return 0
 END IF

 RETURN 1
END FUNCTION

SUB build_listing(tree() as BrowseMenuEntry, byref br as BrowseMenuState)
 'for progress meter
 IF br.ranalready THEN rectangle 5, 32 + br.mstate.size * 9, 310, 12, boxlook(0).edgecol, vpage
 br.meter = 0

 'erase old list
 IF br.getdrivenames THEN br.mstate.last = -1 ELSE br.mstate.last = br.drivesshown - 1
 FOR i as integer = br.mstate.last + 1 TO UBOUND(tree)
  tree(i).filename = ""
  tree(i).caption = ""
  tree(i).about = ""
  tree(i).kind = 0
 NEXT i
 
#IFDEF __FB_WIN32__
 '--Drive list
 IF br.drivesshown = 0 THEN
  '--Refresh drives option
  'br.mstate.last += 1
  'tree(br.mstate.last).filename = ""
  'tree(br.mstate.last).caption = "Refresh drives list"
  'tree(br.mstate.last).kind = bkSpecial

  DIM drive(26) as string
  DIM drivetotal as integer = drivelist(drive())
  FOR i as integer = 0 TO drivetotal - 1
   br.mstate.last += 1
   tree(br.mstate.last).filename = drive(i)
   tree(br.mstate.last).caption = drive(i)
   tree(br.mstate.last).kind = bkDrive
   IF br.getdrivenames THEN
    tree(br.mstate.last).caption += " " + drivelabel(drive(i))
   END IF
   draw_browse_meter br

  NEXT i
  'could add My Documents to drives list here
 END IF
#ENDIF
 br.drivesshown = br.mstate.last + 1
 br.getdrivenames = NO

 IF br.nowdir = "" THEN
 ELSE
  DIM b as string

  '--Current drive
  br.mstate.last += 1
  b = MID(br.nowdir, 1, INSTR(br.nowdir, SLASH))
  tree(br.mstate.last).filename = b
  tree(br.mstate.last).kind = bkRoot
#IFDEF __FB_WIN32__
  IF hasmedia(b) = 0 THEN
   'Somebody pulled out the disk
   br.mstate.need_update = NO
   br.alert = "Disk not readable"
   br.mstate.last -= 1
   br.mstate.pt = 0
   br.mstate.top = 0
   br.nowdir = ""
   EXIT SUB
  END IF
  FOR i as integer = 0 TO br.drivesshown - 1
   IF tree(i).filename = b THEN
    DIM tmpname as string = drivelabel(b)
    IF LEN(tmpname) THEN tree(i).caption = b + " " + tmpname
    tree(br.mstate.last).caption = tree(i).caption
    EXIT FOR
   END IF
  NEXT
#ENDIF
  '-- Add parent directory entries
  DIM path_right as string
  DIM path_left as string
  ' Remove c:\ or root /
  path_left = ""
  path_right = trim_path_root(br.nowdir)
#IFDEF __FB_WIN32__
  ' Special handling of user home dir (I think this is pretty pointless):
  ' Show username\ as the topmost parent directory, rather than c:\, Users\, username\
  ' This works on all OSes, only do this on Windows, because it's weird
  DIM home_dir as string = get_home_dir()
  IF RIGHT(home_dir, 1) <> SLASH THEN home_dir &= SLASH
  IF LEN(home_dir) > 0 AND INSTR(br.nowdir, home_dir) = 1 THEN
   br.mstate.last = br.mstate.last + 1
   IF br.mstate.last = UBOUND(tree) THEN REDIM PRESERVE tree(UBOUND(tree) + 256)
   tree(br.mstate.last).filename = trim_path_root(home_dir)
   tree(br.mstate.last).kind = bkParentDir
   tree(br.mstate.last).caption = trimpath(home_dir) & SLASH
   path_right = MID(br.nowdir, LEN(home_dir) + 1)
  END IF
#ENDIF
  ' Add parent directories.
  DO UNTIL path_right = ""
   path_left &= LEFT(path_right, 1)
   path_right = MID(path_right, 2)
   IF RIGHT(path_left, 1) = SLASH THEN
    br.mstate.last = br.mstate.last + 1
    IF br.mstate.last = UBOUND(tree) THEN REDIM PRESERVE tree(UBOUND(tree) + 256)
    tree(br.mstate.last).filename = path_left
    tree(br.mstate.last).kind = bkParentDir
    path_left = ""
   END IF
  LOOP
  '---FIND ALL SUB-DIRECTORIES IN THE CURRENT DIRECTORY---
  DIM filelist() as string
  findfiles br.nowdir, ALLFILES, fileTypeDirectory, br.showhidden, filelist()
  FOR i as integer = 0 TO UBOUND(filelist)
   br.mstate.last = br.mstate.last + 1
   IF br.mstate.last = UBOUND(tree) THEN REDIM PRESERVE tree(UBOUND(tree) + 256)
   tree(br.mstate.last).kind = bkSubDir
   tree(br.mstate.last).filename = filelist(i)
   DIM extension as string = justextension(filelist(i))
   IF tree(br.mstate.last).filename = "." OR tree(br.mstate.last).filename = ".." OR RIGHT(tree(br.mstate.last).filename, 4) = ".tmp" THEN br.mstate.last = br.mstate.last - 1
   IF br.special = 7 THEN ' Special handling in RPG mode
    IF LCASE(extension) = "rpgdir" THEN br.mstate.last = br.mstate.last - 1
   END IF
   IF br.special <> 8 THEN
    '--hide any .saves folders when browsing (except in RELOAD special mode)
    IF LCASE(extension) = "saves" THEN br.mstate.last = br.mstate.last - 1
   END IF
#IFDEF __FB_DARWIN__
   IF extension = "app" THEN br.mstate.last = br.mstate.last - 1
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
  ELSEIF br.special = 11 THEN
   IF diriswriteable(br.nowdir) THEN
    br.mstate.last = br.mstate.last + 1
    IF br.mstate.last = UBOUND(tree) THEN REDIM PRESERVE tree(UBOUND(tree) + 256)
    tree(br.mstate.last).kind = bkSelectable
    tree(br.mstate.last).filename = ""
    tree(br.mstate.last).caption = "Select this folder"
    tree(br.mstate.last).about = br.nowdir
   END IF
  ELSE
   browse_add_files br.fmask, filetype, br, tree()
  END IF
 END IF

 '--set display
 FOR i as integer = 0 TO br.mstate.last
  IF LEN(tree(i).caption) = 0 THEN
   tree(i).caption = tree(i).filename
  END IF
 NEXT

 DIM sortstart as integer = br.mstate.last
 FOR k as integer = 0 TO br.mstate.last
  WITH tree(k)
   IF .kind = bkSubDir OR .kind = bkSelectable OR .kind = bkUnselectable THEN sortstart = k: EXIT FOR
  END WITH
 NEXT

 '--alphabetize
 FOR i as integer = sortstart TO br.mstate.last - 1
  FOR j as integer = br.mstate.last TO i + 1 STEP -1
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
 FOR o as integer = br.mstate.last TO sortstart + 1 STEP -1
  FOR i as integer = sortstart + 1 TO o
   IF tree(i).kind < tree(i - 1).kind THEN
    SWAP tree(i), tree(i - 1)
   END IF
  NEXT
 NEXT

 '--set cursor
 br.mstate.pt = 0
 br.mstate.top = 0
 FOR i as integer = br.drivesshown TO br.mstate.last
  'look for first selectable item
  IF tree(i).kind = bkSelectable THEN br.mstate.pt = i: EXIT FOR
  'second preference is first subdirectory
  IF tree(i).kind = bkSubDir AND tree(br.mstate.pt).kind <> bkSubDir THEN br.mstate.pt = i
  'final preference is current (bottommost) directory
  IF tree(i).kind = bkParentDir OR tree(i).kind = bkRoot THEN br.mstate.pt = i
 NEXT i
 br.mstate.top = small(br.mstate.pt - 2, br.mstate.last - br.mstate.size)
 br.mstate.top = large(br.mstate.top, 0)

 '--don't display progress bar overtop of previous menu
 br.ranalready = YES

END SUB

FUNCTION check_for_plotscr_inclusion(filename as string) as bool
 'This script is a hack to allow people who name their scripts .txt to use the import
 'feature without cluttering the browse interface with non-plotscript .txt files
 'Note that scanscripts.py uses a completely different autodetection method
 DIM result as bool = NO

 'We check whether these form the beginning of any line near the top of the file
 DIM indicators(...) as string = { _
  "include,plotscr.hsd", "script,", "plotscript,", "globalvariable", "defineconstant" _
 }

 DIM fh as integer = FREEFILE
 IF OPEN(filename FOR INPUT AS #fh) THEN RETURN NO
 DIM s as string
 FOR i as integer = 0 TO 49 'Only bother to check the first 50 lines
  LINE INPUT #fh, s
  s = exclude(LCASE(s), !" \t")
  FOR j as integer = 0 TO UBOUND(indicators)
   IF MID(s, 1, LEN(indicators(j))) = indicators(j) THEN
    result = YES
    EXIT FOR, FOR
   END IF
  NEXT
 NEXT i
 CLOSE #fh
 
 RETURN result
END FUNCTION
