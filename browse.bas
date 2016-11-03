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
	filename as string         'Actual filename, in whatever encoding the OS uses
	decoded_filename as string 'Filename in Latin-1
	caption as string          'How the entry is shown
	about as string            'Description to show at bottom when selected
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
DECLARE SUB append_tree_record(byref br as BrowseMenuState, tree() as BrowseMenuEntry)
DECLARE SUB build_listing(tree() as BrowseMenuEntry, byref br as BrowseMenuState)
DECLARE SUB draw_browse_meter(br as BrowseMenuState)
DECLARE SUB browse_hover(tree() as BrowseMenuEntry, byref br as BrowseMenuState)
DECLARE SUB browse_hover_file(tree() as BrowseMenuEntry, byref br as BrowseMenuState)
DECLARE SUB browse_add_files(wildcard as string, byval filetype as integer, byref br as BrowseMenuState, tree() as BrowseMenuEntry)
DECLARE FUNCTION valid_audio_file (filepath as string, byval typemask as integer) as bool
DECLARE FUNCTION browse_get_reload_info(filepath as string, info as string) as bool
DECLARE FUNCTION check_for_plotscr_inclusion(filepath as string) as bool

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
 ' An entirely black palette
 DIM temppal(255) as RGBcolor
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
     br.nowdir += tree(i).filename
    NEXT i
    build_listing tree(), br
   CASE bkSubDir
    br.nowdir += tree(br.mstate.pt).filename + SLASH
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
      IF INSTR(LCASE(tree(index).decoded_filename), selectst.query) = 1 THEN selectst.query_at = -1  'invisible match
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
  edgeprint shorten_to_left(decode_filename(br.nowdir + tree(br.mstate.pt).filename), 304), 8, 6, uilook(uiText), dpage
 ELSE
  edgeprint decode_filename(br.nowdir), 8, 6, uilook(uiText), dpage
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
 IF needf THEN needf -= 1
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

' Set br.alert according to the selected browser entry, and preview audio
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

' Set br.alert according to the selected file, and preview audio
SUB browse_hover_file(tree() as BrowseMenuEntry, byref br as BrowseMenuState)
 WITH tree(br.mstate.pt)
  DIM as string filepath = br.nowdir & .filename
  SELECT CASE br.special
   CASE 1 'music bam only (is this still used?)
    music_stop
    IF .kind = bkSelectable OR .kind = bkUnselectable THEN
     IF valid_audio_file(filepath, FORMAT_BAM) THEN
      loadsong filepath
     ELSE
      br.alert = .decoded_filename + " is not a valid BAM file"
     END IF
    END IF
   CASE 5 'music
    music_stop
    br.alert = .about
    IF valid_audio_file(filepath, PREVIEWABLE_MUSIC_FORMAT) THEN
     loadsong filepath
    ELSEIF getmusictype(filepath) = FORMAT_MP3 THEN
     br.alert = "Cannot preview MP3, try importing"
    END IF
   CASE 6 'sfx
    br.alert = .about
    IF br.snd > -1 THEN
     sound_stop(br.snd,-1)
     UnloadSound(br.snd)
     br.snd = -1
    END IF
    IF .kind <> bkUnselectable THEN
     'not disabled because of size
     IF valid_audio_file(filepath, PREVIEWABLE_FX_FORMAT) THEN
      br.snd = LoadSound(filepath)
      sound_play(br.snd, 0, -1)
     ELSEIF getmusictype(filepath) = FORMAT_MP3 THEN
      br.alert = "Cannot preview MP3, try importing"
     END IF
    END IF
   CASE 9 'scripts
    IF LCASE(justextension(.filename)) = "hs" THEN
     br.alert = "Compiled HamsterSpeak scripts"
    ELSE
     br.alert = "HamsterSpeak scripts"
    END IF
   CASE ELSE
    br.alert = .about
  END SELECT
 END WITH
END SUB

'Returns true if the BMP looks good; set .about
FUNCTION browse_check_bmp(byref br as BrowseMenuState, tree() as BrowseMenuEntry, byref bmpd as BitmapV3InfoHeader) as integer
 WITH tree(br.mstate.last)
  DIM support as integer = bmpinfo(br.nowdir + .filename, bmpd)
  IF support = 2 THEN
   .about = bmpd.biWidth & "*" & bmpd.biHeight & " pixels, " & bmpd.biBitCount & "-bit color"
   RETURN YES
  ELSEIF support = 1 THEN
   .about = "Unsupported BMP file"
  ELSE
   .about = "Invalid BMP file"
  END IF
  .kind = bkUnselectable
 END WITH
 RETURN NO
END FUNCTION

SUB append_tree_record(byref br as BrowseMenuState, tree() as BrowseMenuEntry)
 br.mstate.last += 1
 IF br.mstate.last = UBOUND(tree) THEN REDIM PRESERVE tree(UBOUND(tree) + 256)
END SUB

SUB browse_add_files(wildcard as string, byval filetype as integer, byref br as BrowseMenuState, tree() as BrowseMenuEntry)
 DIM bmpd as BitmapV3InfoHeader
 DIM tempbuf(79) as integer
 DIM filepath as string

 DIM filelist() as string
 findfiles br.nowdir, wildcard, filetype, br.showhidden, filelist()

 FOR i as integer = 0 TO UBOUND(filelist)
  append_tree_record br, tree()
  WITH tree(br.mstate.last)
   .kind = bkSelectable
   .filename = filelist(i)
   filepath = br.nowdir & .filename
   '---music files
   IF br.special = 1 OR br.special = 5 THEN
    IF valid_audio_file(filepath, VALID_MUSIC_FORMAT) = NO THEN
     .kind = bkUnselectable
     .about = "Not a valid music file"
    END IF
   END IF
   IF br.special = 6 THEN
    IF valid_audio_file(filepath, VALID_FX_FORMAT) = NO THEN
     .kind = bkUnselectable
     .about = "Not a valid sound effect file"
    ELSEIF FILELEN(filepath) > 500 * 1024 AND LCASE(justextension(filepath)) <> "wav" THEN
     .kind = bkUnselectable
     .about = "File is too large (limit 500kB)"
    END IF
   END IF
   '---Any BMP
   IF br.special = 2 THEN
    IF browse_check_bmp(br, tree(), bmpd) = NO THEN
     .kind = bkUnselectable
    END IF
   END IF
   '---320x200 BMP files (any supported bitdepth)
   IF br.special = 3 THEN
    IF browse_check_bmp(br, tree(), bmpd) = NO OR bmpd.biWidth <> 320 OR bmpd.biHeight <> 200 THEN
     .kind = bkUnselectable
    END IF
   END IF
   '---1/4/8 bit BMP files (fonts)
   IF br.special = 10 THEN
    IF browse_check_bmp(br, tree(), bmpd) = NO OR bmpd.biBitCount > 8 THEN
     .kind = bkUnselectable
    END IF
   END IF
   '--master palettes
   IF br.special = 4 THEN
    IF LCASE(justextension(filepath)) = "mas" THEN
     DIM masfh as integer = FREEFILE
     OPENFILE(filepath, FOR_BINARY, masfh)
     DIM a as string = "       "
     GET #masfh, 1, a
     CLOSE #masfh
     SELECT CASE a
      CASE br.mashead
       .about = "MAS format"
      CASE br.paledithead
       .about = "MAS format (PalEdit)"
      CASE ELSE
       .about = "Not a valid MAS file"
       .kind = bkUnselectable
     END SELECT
    ELSE  'BMP as a master palette
     IF browse_check_bmp(br, tree(), bmpd) = NO THEN
      .kind = bkUnselectable
     ELSE
      IF bmpd.biBitCount <= 8 THEN
       'Don't care about the dimensions
       .about = bmpd.biBitCount & "-bit color BMP"
      ELSEIF bmpd.biBitCount >= 24 AND (bmpd.biWidth = 16 AND bmpd.biHeight = 16) THEN
       'ok
      ELSE
       .kind = bkUnselectable
      END IF
     END IF
    END IF
   END IF
   '--RPG files
   IF br.special = 7 THEN
    copylump filepath, "browse.txt", br.tmp, YES
    .caption = load_gamename(br.tmp & "browse.txt")
    .about = load_aboutline(br.tmp & "browse.txt")
    safekill br.tmp & "browse.txt"
   END IF
   '--RELOAD files
   IF br.special = 8 THEN
    IF browse_get_reload_info(filepath, .about) = NO THEN
     .kind = bkUnselectable 'grey out bad ones
    END IF
   END IF
   '--script files
   IF br.special = 9 THEN
    IF wildcard = "*.txt" THEN
     IF NOT check_for_plotscr_inclusion(filepath) THEN
      'Don't display .txt files unless they include plotscr.hsd
      br.mstate.last -= 1
      ' WARNING: WITH pointer now invalid
     END IF
    END IF
   END IF
   '--tilemaps
   IF br.special = 12 THEN
    DIM info as TilemapInfo
    IF GetTilemapInfo(filepath, info) = NO THEN
     .kind = bkUnselectable
    END IF
    .about = "Size " & info.wide & "x" & info.high & " tilemap with " & info.layers & " layers"
   END IF
  END WITH
  draw_browse_meter br
 NEXT
END SUB

' Check whether valid RELOAD file (return true), and modify info argument
FUNCTION browse_get_reload_info(filepath as string, info as string) as bool
 DIM header as string = "    "
 DIM fh as integer = FREEFILE
 OPENFILE(filepath, FOR_BINARY + ACCESS_READ, fh)
  GET #fh, 1, header
 CLOSE #fh
 IF header <> "RELD" THEN info = "Has no RELOAD file header." : RETURN NO
 DIM doc as Reload.Docptr
 doc = Reload.LoadDocument(filepath)
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
   IF RIGHT(filepath, 6) = ".slice" ORELSE LEFT(trimpath(filepath), 10) = "slicetree_" THEN
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

' Check whether file type is one of the ones specified in the mask
FUNCTION valid_audio_file (filepath as string, byval typemask as integer) as bool
 DIM as string hdmask, realhd
 DIM as integer musfh, chk
 chk = getmusictype(filepath)

 IF (chk AND typemask) = 0 THEN return NO

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
  OPENFILE(filepath, FOR_BINARY, musfh)
  GET #musfh, 1, hdmask
  CLOSE #musfh
  IF hdmask <> realhd THEN return NO
 END IF

 RETURN YES
END FUNCTION

SUB build_listing(tree() as BrowseMenuEntry, byref br as BrowseMenuState)
 'for progress meter
 IF br.ranalready THEN rectangle 5, 32 + br.mstate.size * 9, 310, 12, boxlook(0).edgecol, vpage
 br.meter = 0

 'erase old list
 IF br.getdrivenames THEN br.mstate.last = -1 ELSE br.mstate.last = br.drivesshown - 1
 FOR i as integer = br.mstate.last + 1 TO UBOUND(tree)
  tree(i) = BrowseMenuEntry()
 NEXT i
 
#IFDEF __FB_WIN32__
 '--Drive list
 IF br.drivesshown = 0 THEN
  '--Refresh drives option
  'append_tree_record br, tree()
  'tree(br.mstate.last).filename = ""
  'tree(br.mstate.last).caption = "Refresh drives list"
  'tree(br.mstate.last).kind = bkSpecial

  DIM drive(26) as string
  DIM drivetotal as integer = drivelist(drive())
  FOR i as integer = 0 TO drivetotal - 1
   append_tree_record br, tree()
   WITH tree(br.mstate.last)
    .filename = drive(i)
    .caption = drive(i)
    IF br.getdrivenames THEN
     .caption += " " + drivelabel(drive(i))
    END IF
    .kind = bkDrive
   END WITH
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
  append_tree_record br, tree()
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
   append_tree_record br, tree()
   WITH tree(br.mstate.last)
    .filename = trim_path_root(home_dir)
    .kind = bkParentDir
    .caption = trimpath(home_dir) & SLASH
   END WITH
   path_right = MID(br.nowdir, LEN(home_dir) + 1)
  END IF
#ENDIF
  ' Add parent directories.
  DO UNTIL path_right = ""
   path_left &= LEFT(path_right, 1)
   path_right = MID(path_right, 2)
   IF RIGHT(path_left, 1) = SLASH THEN
    append_tree_record br, tree()
    WITH tree(br.mstate.last)
     .filename = path_left
     .kind = bkParentDir
    END WITH
    path_left = ""
   END IF
  LOOP

  '---FIND ALL SUB-DIRECTORIES IN THE CURRENT DIRECTORY---
  DIM filelist() as string
  findfiles br.nowdir, ALLFILES, fileTypeDirectory, br.showhidden, filelist()
  FOR i as integer = 0 TO UBOUND(filelist)
   append_tree_record br, tree()
   WITH tree(br.mstate.last)
    .kind = bkSubDir
    .filename = filelist(i)
    IF .filename = "." OR .filename = ".." OR RIGHT(LCASE(.filename), 4) = ".tmp" THEN br.mstate.last -= 1
   END WITH
   DIM extension as string = justextension(filelist(i))
   IF br.special = 7 THEN ' Special handling in RPG mode
    IF LCASE(extension) = "rpgdir" THEN br.mstate.last -= 1
   END IF
   IF br.special <> 8 THEN
    '--hide any .saves folders when browsing (except in RELOAD special mode)
    IF LCASE(extension) = "saves" THEN br.mstate.last -= 1
   END IF
#IFDEF __FB_DARWIN__
   IF extension = "app" THEN br.mstate.last -= 1
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
    append_tree_record br, tree()
    WITH tree(br.mstate.last)
     .kind = bkSelectable
     .filename = ""
     .caption = "Select this folder"
     .about = decode_filename(br.nowdir)
    END WITH
   END IF
  ELSE
   browse_add_files br.fmask, filetype, br, tree()
  END IF
 END IF

 '--set display
 FOR i as integer = 0 TO br.mstate.last
  WITH tree(i)
   .decoded_filename = decode_filename(.filename)
   IF LEN(.caption) = 0 THEN
    .caption = .decoded_filename
   END IF
  END WITH
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

FUNCTION check_for_plotscr_inclusion(filepath as string) as bool
 'This script is a hack to allow people who name their scripts .txt to use the import
 'feature without cluttering the browse interface with non-plotscript .txt files
 'Note that scanscripts.py uses a completely different autodetection method
 DIM result as bool = NO

 'We check whether these form the beginning of any line near the top of the file
 DIM indicators(...) as string = { _
  "include,plotscr.hsd", "script,", "plotscript,", "globalvariable", "defineconstant" _
 }

 DIM fh as integer = FREEFILE
 IF OPENFILE(filepath, FOR_INPUT, fh) THEN RETURN NO
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
