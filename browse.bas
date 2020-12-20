'OHRRPGCE - File browser
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#include "config.bi"
#include "ver.txt"
#include "const.bi"
#include "allmodex.bi"
#include "common.bi"
#include "slices.bi"
#include "loading.bi"
#include "reload.bi"
#include "os.bi"

Type FilePreviewerPtr as FilePreviewer ptr

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
	mstate as MenuState
	filetype as BrowseFileType
	ranalready as bool
	meter as integer
	drivesshown as integer  'number of drive entries (plus 1 for "refresh" option, if uncommented)
	alert as string
	engine_version_shown as bool  'Show the engine version at the bottom of the screen
	showHidden as bool
	getdrivenames as bool   'Poll drive names on Windows? (can be slow)
	fmask as string
        previewer as FilePreviewerPtr
	preview_panel_size as XYPair 'Size of the preview area
End Type

Type FilePreviewer Extends Object
	Declare Destructor()
	Declare Virtual Sub load_preview(filepath as string, br as BrowseMenuState)
	Declare Virtual Sub unload_preview()
	Declare Virtual Sub draw_preview()
	Declare Virtual Sub update_layout(br as BrowseMenuState)
End Type

Type ImagePreviewer Extends FilePreviewer
	Declare Virtual Sub load_preview(filepath as string, br as BrowseMenuState)
	Declare Virtual Sub draw_preview()
	Declare Virtual Sub unload_preview()
	Declare Virtual Sub update_layout(br as BrowseMenuState)
        Declare Sub toggle_remapping()

	image_preview as Frame ptr   'Preview of currently selected image, or NULL
	image_preview_remap as bool  'Show preview remapped to master() rather than original color
	preview_footer as string     'Info message
End Type

Type SfxPreviewer Extends FilePreviewer
	Declare Virtual Sub load_preview(filepath as string, br as BrowseMenuState)
	Declare Virtual Sub unload_preview()

	snd as integer = -1          'Slot of currently playing sound, or -1
End Type

Type MusicPreviewer Extends FilePreviewer
	Declare Virtual Sub load_preview(filepath as string, br as BrowseMenuState)
	Declare Virtual Sub unload_preview()
End Type


'Subs and functions only used locally
DECLARE SUB append_tree_record(byref br as BrowseMenuState, tree() as BrowseMenuEntry)
DECLARE SUB build_listing(tree() as BrowseMenuEntry, byref br as BrowseMenuState)
DECLARE SUB draw_browse_meter(br as BrowseMenuState)
DECLARE SUB browse_calc_menusize(byref br as BrowseMenuState)
DECLARE SUB browse_update_layout(byref br as BrowseMenuState, tree() as BrowseMenuEntry)
DECLARE SUB browse_hover(tree() as BrowseMenuEntry, byref br as BrowseMenuState)
DECLARE SUB browse_add_files(wildcard as string, byval filetype as integer, byref br as BrowseMenuState, tree() as BrowseMenuEntry)
DECLARE FUNCTION legal_audio_file (filepath as string, byval typemask as integer) as bool
DECLARE FUNCTION browse_get_reload_info(filepath as string, info as string) as bool
DECLARE FUNCTION check_is_scripts_file(filepath as string) as bool

DIM SHARED remember as string

SUB set_browse_default(default as string)
 remember = default
END SUB

' Returns an absolute path, or "" if the user cancelled.
' filetype: Specifies which files are valid (including default allowed file extensions)
'          and how to preview them.
' default: initially selected file or a directory. Gets set to the file selected,
'          or if canceled, the last directory we were in.
'          If this is "", then the initial directory is the directory from the
'          last time browse() was called, anywhere.
' fmask:   A mask like "*.bmp" or "key.der" overriding the defaults for filetype; use only
'          if the defaults aren't right (e.g. only "*.slice" rather than all RELOAD files).
'          Mandatory for filetype=browseAny.
' needf:   whether to fade screen in
FUNCTION browse (filetype as BrowseFileType = browseAny, byref default as string = "", fmask as string = "", helpkey as string = "", needf as bool = NO) as string
DIM ret as string

DIM selectst as SelectTypeState
DIM br as BrowseMenuState
br.filetype = filetype
br.fmask = fmask
br.engine_version_shown = (filetype = browseRPG)
br.showHidden = NO

SELECT CASE br.filetype
 CASE browseMusic
  br.previewer = NEW MusicPreviewer
 CASE browseSfx
  br.previewer = NEW SfxPreviewer
 CASE browseImage, browsePalettedImage, browseTileset, browseMasterPal
  'Any kind of image
  br.previewer = NEW ImagePreviewer
END SELECT

'tree().kind contains the type of each object in the menu
REDIM tree(255) as BrowseMenuEntry
DIM catfg(6) as integer, catbg(6) as integer

DIM defaultcol as integer = findrgb(128, 132, 208)  'blue
catfg(0) = uilook(uiMenuItem)   : catbg(0) = uilook(uiHighlight)    'selectable drives (none on unix systems)
catfg(1) = defaultcol           : catbg(1) = uilook(uiDisabledItem) 'directories
catfg(2) = defaultcol           : catbg(2) = uilook(uiBackground)   'subdirectories
catfg(3) = uilook(uiMenuItem)   : catbg(3) = uilook(uiBackground)   'files
catfg(4) = defaultcol           : catbg(4) = uilook(uiDisabledItem) 'root of current drive
catfg(5) = findrgb(200,200,128) : catbg(5) = uilook(uiDisabledItem) 'special (never used)
catfg(6) = uilook(uiDisabledItem): catbg(6) = uilook(uiBackground)  'disabled

IF needf THEN
 fadeout 0, 0, 0, 0  'fadems=0: fade immediately to black
END IF

'--Preserve pages 2 and 3 because the tilemap editor still uses them
'  and they are going to be clobbered by bitdepth changing.
DIM holdpage2 as Frame Ptr = frame_duplicate(vpages(2))
DIM holdpage3 as Frame Ptr = frame_duplicate(vpages(3))

DIM vpages_were_32bit as bool = vpages_are_32bit()
switch_to_32bit_vpages

'Load a variant of the default font, misc/browser font.ohf, which has both Latin-1
'characters and the old (c), etc, characters from the original fonts
DIM browser_font(1023) as integer
getbrowserfont browser_font()
setfont browser_font()

DIM prev_mouse_vis as CursorVisibility = getcursorvisibility()
showmousecursor
force_use_mouse += 1

'remember/default may be either empty or a file (if one was selected last call), or directory (if not)
DIM startfile as string
IF remember = "" THEN remember = CURDIR & SLASH
IF default = "" THEN
 default = remember
END IF
default = simplify_path(absolute_path(default))
IF isdir(default) AND justextension(default) <> "rpgdir" THEN
 br.nowdir = default & SLASH
 startfile = ""
ELSE
 br.nowdir = trimfilename(default) & SLASH
 startfile = trimpath(default)
END IF

browse_calc_menusize br

br.mstate.pt = 0
br.mstate.top = 0
br.mstate.last = 0
DIM _menuopts as MenuOptions  'Not used! Only for calc_menustate_size
_menuopts.edged = YES
'This is just for mouse support
calc_menustate_size br.mstate, _menuopts, 10, 20

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
 browse_update_layout br, tree()

 IF keyval(ccCancel) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help helpkey
 IF usemenu(br.mstate) THEN br.mstate.need_update = YES
 mouse_drag_menu(br.mstate)
 IF keyval(scSpace) > 0 AND LEN(selectst.query) > 0 THEN
  'While typing a string, space doesn't cause selection
 ELSEIF enter_space_click(br.mstate) THEN
  br.alert = ""
  br.mstate.need_update = YES
  SELECT CASE tree(br.mstate.pt).kind
   CASE bkDrive
    'this could take a while...
    rectangle 5, 32 + br.mstate.size * 9, 310, 12, boxlook(0).bgcol, vpage
    edgeprint "Reading...", 8, 34 + br.mstate.size * 9, uilook(uiText), vpage
    setvispage vpage, NO
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
  'Ctrl + P to switch between paletted/unpaletted previewing
  IF (*br.previewer IS ImagePreviewer) ANDALSO keyval(scP) > 1 THEN
   CAST(ImagePreviewer ptr, br.previewer)->toggle_remapping()
   br.mstate.need_update = YES
  END IF
 ELSE
  IF select_by_typing(selectst) THEN
   DIM index as integer = br.mstate.pt
   IF LEN(selectst.query) = 1 THEN loopvar index, 0, br.mstate.last
   FOR ctr as integer = 0 TO br.mstate.last
    'IF (tree(index).kind = bkParentDir OR tree(index).kind = bkSubDir OR tree(index).kind = bkSelectable) THEN
     'Search both display name (preferentially) and filename
     selectst.query_at = find_on_word_boundary_excluding(LCASE(tree(index).caption), selectst.query, "the")
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
    loopvar index, 0, br.mstate.last
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
 DIM title as string
 IF br.filetype = browseRPG AND tree(br.mstate.pt).kind = bkSelectable THEN
  'Selected item is an RPG
  title = br.nowdir + tree(br.mstate.pt).filename
 ELSE
  title = br.nowdir
 END IF
 title = text_right(decode_filename(title), 304, YES, NO)  'ellipsis=YES, withtags=NO
 edgeprint title, 8, 6, uilook(uiText), dpage
 textcolor uilook(uiText), 0
 printstr ">", 0, 20 + (br.mstate.pt - br.mstate.top) * 9, dpage
 'This mess here because this menu doesn't use a standard MenuState
 IF selectst.remember_pt <> br.mstate.pt THEN select_clear selectst
 selectst.remember_pt = br.mstate.pt

 WITH br.mstate
  .has_been_drawn = YES
  .rect.x = 10
  .rect.y = 20
 END WITH
 FOR i as integer = br.mstate.top TO small(br.mstate.top + br.mstate.size, br.mstate.last)
  DIM fgcol as integer = catfg(tree(i).kind)
  IF i = br.mstate.hover THEN fgcol = uilook(uiMouseHoverItem)
  textcolor fgcol, catbg(tree(i).kind)
  DIM caption as string = tree(i).caption
  IF LEN(caption) < 38 AND catbg(tree(i).kind) > 0 THEN caption += STRING(38 - LEN(caption), " ")
  IF i = br.mstate.pt THEN caption = highlight_menu_typing_selection_string(caption, selectst)
  printstr caption, 10, 20 + (i - br.mstate.top) * 9, dpage, YES
 NEXT i

 'Preview info or image
 IF br.previewer THEN br.previewer->draw_preview()

 'The info line at the bottom, and maybe the engine version
 edgeboxstyle 4, 31 + br.mstate.size * 9, 312, 14, 0, dpage, NO, YES
 edgeprint br.alert, 8, 34 + br.mstate.size * 9, uilook(uiText), dpage
 IF br.engine_version_shown THEN
  rectangle 0, pBottom, 320, 10, uilook(uiDisabledItem), dpage
  edgeprint short_version & " " & gfxbackend & "/" & musicbackend, 8, pBottom + 1, uilook(uiMenuItem), dpage
  textcolor uilook(uiText), 0
 END IF

 SWAP vpage, dpage
 setvispage vpage
 IF needf THEN
  'FIXME: the browser now runs at 32-bitdepth, which doesn't support screen fading yet
  'fadein
  'setkeys
  setpal master()
  needf = NO
 END IF
 dowait
LOOP

setcursorvisibility(prev_mouse_vis)
force_use_mouse -= 1
setfont current_font()
IF vpages_were_32bit = NO THEN
 switch_to_8bit_vpages
END IF

IF LEN(ret) THEN
 default = ret
ELSE
 default = br.nowdir
END IF
remember = default

DELETE br.previewer

'Restore held copies of page 2 and 3 (because the tileset editor still needs them)
' and they were clobbered by bit depth changing
frame_draw holdpage2, , 0, 0, NO, vpages(2)
frame_unload @holdpage2
frame_draw holdpage3, , 0, 0, NO, vpages(3)
frame_unload @holdpage3

setkeys
RETURN ret

END FUNCTION

SUB browse_calc_menusize(byref br as BrowseMenuState)
 br.mstate.rect.size = XY(320, get_resolution().h)
 DIM margin as integer = 37 + IIF(br.engine_version_shown, 10, 0)
 br.mstate.size = (get_resolution().h - margin) \ 9 - 1
 br.preview_panel_size = get_resolution() - XY(322, 2)
END SUB

'Update the menu layout (currently just the image preview) if the window was resized
SUB browse_update_layout(byref br as BrowseMenuState, tree() as BrowseMenuEntry)
 IF UpdateScreenSlice() THEN
  browse_calc_menusize br
  IF br.previewer THEN br.previewer->update_layout(br)
 END IF
END SUB



'==============================================================================

DESTRUCTOR FilePreviewer()
 unload_preview
END DESTRUCTOR

SUB FilePreviewer.load_preview(filepath as string, br as BrowseMenuState)
END SUB

SUB FilePreviewer.unload_preview()
END SUB

SUB FilePreviewer.draw_preview()
END SUB

SUB FilePreviewer.update_layout(br as BrowseMenuState)
END SUB

'----------------------------------------

SUB MusicPreviewer.load_preview(filepath as string, br as BrowseMenuState)
 unload_preview
 IF legal_audio_file(filepath, music_supported_formats()) THEN
  loadsong filepath
 ELSE
  br.alert = "Cannot preview this file type"
 END IF
END SUB

SUB MusicPreviewer.unload_preview()
 music_stop
END SUB

'----------------------------------------

SUB SfxPreviewer.load_preview(filepath as string, br as BrowseMenuState)
 unload_preview
 'not disabled because of size
 IF legal_audio_file(filepath, sound_supported_formats()) THEN
  snd = sound_load(filepath)
  IF snd > -1 THEN sound_play(snd, 0, get_global_sfx_volume)
 ELSE
  br.alert = "Cannot preview this file type"
 END IF
END SUB

SUB SfxPreviewer.unload_preview()
 IF snd >= 0 THEN
  sound_stop(snd)
  sound_unload(snd)
 END IF
 snd = -1
END SUB

'----------------------------------------

SUB ImagePreviewer.load_preview(filepath as string, br as BrowseMenuState)
 unload_preview
 IF NOT br.preview_panel_size > 0 THEN EXIT SUB 'There's no space to display it
 DIM ratio as double = 1.0
 DIM starttime as double = TIMER
 IF image_preview_remap = NO THEN
  ' Load the image as a 32 bit Surface (necessary for scale_surface), then scale it
  DIM as Surface ptr temp = image_import_as_surface(filepath, YES)  'always_32bit=YES
  IF temp = NULL THEN EXIT SUB
  WITH br.preview_panel_size
   IF temp->width > 0 THEN ratio = small(1.0, small(.w / temp->width, .h / temp->height))
  END WITH
  surface_assign @temp, surface_scale(temp, temp->width * ratio, temp->height * ratio)
  image_preview = frame_with_surface(temp)
  gfx_surfaceDestroy(@temp)
 ELSE
  ' Scaling not implemented
  image_preview = image_import_as_frame_8bit(filepath, master(), NO)
 END IF
 preview_footer = CINT(100 * ratio) & "% scale"  '; load:" & CINT(1000 * (TIMER - starttime)) & "ms"
END SUB

SUB ImagePreviewer.draw_preview()
 IF image_preview THEN
  drawbox 320, 0, image_preview->w + 2, image_preview->h + 2, uilook(uiText), , dpage
  frame_draw image_preview, , 321, 1, NO, dpage
  edgeprint preview_footer, pRight, image_preview->h + 3, uilook(uiDisabledItem), dpage
 END IF
END SUB

SUB ImagePreviewer.unload_preview()
 frame_unload @image_preview
END SUB

SUB ImagePreviewer.update_layout(br as BrowseMenuState)
 'Redraw not needed when remapping, because the preview doesn't scale to window size
 IF image_preview ANDALSO image_preview_remap = NO THEN br.mstate.need_update = YES
END SUB

SUB ImagePreviewer.toggle_remapping()
 image_preview_remap XOR= YES
 IF image_preview_remap = NO THEN
  show_overlay_message "32-bit preview (original color)"
 ELSE
  show_overlay_message "8-bit preview (converted to master palette)"
 END IF
END SUB

'==============================================================================


' Set br.alert according to the selected browser entry, and load any preview
SUB browse_hover(tree() as BrowseMenuEntry, byref br as BrowseMenuState)
 IF br.previewer THEN br.previewer->unload_preview()

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
   WITH tree(br.mstate.pt)
    DIM as string filepath = br.nowdir & .filename
    br.alert = .about
    IF .kind = bkSelectable ANDALSO br.previewer THEN
     br.previewer->load_preview(filepath, br)
    END IF
   END WITH
 END SELECT
END SUB

'Returns true if the image looks good; set .about
FUNCTION browse_check_image(byref br as BrowseMenuState, tree() as BrowseMenuEntry, byref iminfo as ImageFileInfo) as bool
 WITH tree(br.mstate.last)
  iminfo = image_read_info(br.nowdir + .filename)
  .about = iminfo.info
  IF LEN(iminfo.error) THEN .about &= " (" & iminfo.error & ")"
  IF iminfo.supported THEN
   RETURN YES
  END IF
  .kind = bkUnselectable
  RETURN NO
 END WITH
END FUNCTION

SUB append_tree_record(byref br as BrowseMenuState, tree() as BrowseMenuEntry)
 br.mstate.last += 1
 IF br.mstate.last = UBOUND(tree) THEN REDIM PRESERVE tree(UBOUND(tree) + 256)
END SUB

SUB browse_add_files(wildcard as string, byval filetype as integer, byref br as BrowseMenuState, tree() as BrowseMenuEntry)
 DIM iminfo as ImageFileInfo
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
   IF br.filetype = browseMusic THEN
    IF legal_audio_file(filepath, VALID_MUSIC_FORMAT) = NO THEN
     .kind = bkUnselectable
     .about = "Not a valid music file"
    END IF
   END IF
   IF br.filetype = browseSfx THEN
    IF legal_audio_file(filepath, VALID_SFX_FORMAT) = NO THEN
     .kind = bkUnselectable
     .about = "Not a valid sound file"
    ELSEIF FILELEN(filepath) > 1024 * 1024 AND LCASE(justextension(filepath)) <> "wav" THEN
     .kind = bkUnselectable
     .about = "File is too large (limit 1MB)"
    END IF
   END IF
   '---320x200 images
   IF br.filetype = browseTileset THEN
    IF browse_check_image(br, tree(), iminfo) = NO OR iminfo.size.w <> 320 OR iminfo.size.h <> 200 THEN
     .kind = bkUnselectable
    END IF
   END IF
   '---paletted image (1/4/8 bit in the case of BMP), used for fonts and tilemaps
   IF br.filetype = browsePalettedImage THEN
    IF browse_check_image(br, tree(), iminfo) = NO THEN
     .kind = bkUnselectable
    ELSEIF iminfo.paletted = NO THEN
     .kind = bkUnselectable
     .about = "Not paletted: " & .about
    END IF
   END IF
   '---Any image
   IF br.filetype = browseImage THEN
    IF browse_check_image(br, tree(), iminfo) = NO THEN
     .kind = bkUnselectable
    END IF
   END IF
   '--master palettes
   IF br.filetype = browseMasterPal THEN
    IF LCASE(justextension(filepath)) = "mas" THEN
     DIM masfh as integer
     OPENFILE(filepath, FOR_BINARY + ACCESS_READ, masfh)
     DIM a as string = "       "
     GET #masfh, 1, a
     CLOSE #masfh
     SELECT CASE a
      CASE CHR(253) & CHR(13) & CHR(158) & CHR(0) & CHR(0) & CHR(0) & CHR(6)
       .about = "MAS format"
      CASE CHR(253) & CHR(217) & CHR(158) & CHR(0) & CHR(0) & CHR(7) & CHR(6)
       .about = "MAS format (PalEdit)"
      CASE ELSE
       .about = "Not a valid MAS file"
       .kind = bkUnselectable
     END SELECT
    ELSE  'An image file
     IF browse_check_image(br, tree(), iminfo) = NO THEN
      .kind = bkUnselectable
     ELSE
      IF iminfo.size.w = 16 AND iminfo.size.h = 16 THEN
       'A pixel per color (don't care about the bitdepth)
      ELSEIF iminfo.bpp <= 8 THEN
       'The palette is used. Don't care about the dimensions
       .about = iminfo.bpp & "-bit color " & iminfo.imagetype_name
      ELSE
       .kind = bkUnselectable
      END IF
     END IF
    END IF
   END IF
   '--RPG files/RPGDIR dirs
   IF br.filetype = browseRPG THEN
    copylump filepath, "browse.txt", tmpdir, YES
    .caption = load_gamename(tmpdir & "browse.txt")
    .about = load_aboutline(tmpdir & "browse.txt")
    safekill tmpdir & "browse.txt"
   END IF
   '--RELOAD files
   IF br.filetype = browseRELOAD THEN
    IF browse_get_reload_info(filepath, .about) = NO THEN
     .kind = bkUnselectable 'grey out bad ones
    END IF
   END IF
   '--script files
   IF br.filetype = browseScripts THEN
    DIM ext as string = LCASE(justextension(filepath))
    IF ext = "hs" THEN
     .about = "Compiled HamsterSpeak scripts"
    ELSE
     .about = "HamsterSpeak scripts"
    END IF
    IF ext = "txt" THEN
     ' Only add .txt files that seem to be HS scripts
     IF NOT check_is_scripts_file(filepath) THEN
      br.mstate.last -= 1
      ' WARNING: WITH pointer now invalid
     END IF
    END IF
   END IF
   '--.tilemaps
   IF br.filetype = browseTilemap THEN
    DIM info as TilemapInfo
    IF GetTilemapInfo(filepath, info) = NO THEN
     .about = info.err
     .kind = bkUnselectable
    END IF
    .about = "Size " & info.size.wh & " tilemap with " & info.layers & " layers"
   END IF
  END WITH
  draw_browse_meter br
 NEXT
END SUB

' Check whether valid RELOAD file (return true), and modify info argument
FUNCTION browse_get_reload_info(filepath as string, info as string) as bool
 DIM header as string = "    "
 DIM fh as integer
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
   rectangle 5 + .meter, 33 + .mstate.size * 9, 2, 5, boxlook(0).edgecol, vpage
   setvispage vpage 'refresh
  END IF
 END WITH
END SUB

' Check whether file type (truly) is one of the ones specified in the mask
FUNCTION legal_audio_file (filepath as string, byval typemask as integer) as bool
 DIM chk as MusicFormatEnum = getmusictype(filepath)
 IF (chk AND typemask) = 0 THEN RETURN NO
 RETURN valid_audio_file(filepath)
END FUNCTION

SUB build_listing(tree() as BrowseMenuEntry, byref br as BrowseMenuState)
 'for progress meter
 IF br.ranalready THEN rectangle 5, 32 + br.mstate.size * 9, 310, 12, boxlook(0).bgcol, vpage
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
  ' (Note Win 95/98/ME doesn't have a home dir)
  DIM home_dir as string = add_trailing_slash(get_home_dir())
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
   IF br.filetype = browseRPG THEN ' Special handling in RPG mode
    IF LCASE(extension) = "rpgdir" THEN br.mstate.last -= 1
   END IF
   IF br.filetype <> browseRELOAD THEN
    '--hide any .saves folders when browsing (except in RELOAD special mode)
    IF LCASE(extension) = "saves" THEN br.mstate.last -= 1
   END IF
#IFDEF __FB_DARWIN__
   IF extension = "app" THEN br.mstate.last -= 1
#ENDIF
   draw_browse_meter br
  NEXT

  '--Add all files matching the filename mask or type
  IF LEN(br.fmask) THEN
   browse_add_files br.fmask, fileTypeFile, br, tree()
  ELSE

   'If no fmask is given, use default file extensions
   SELECT CASE br.filetype
    CASE browseMusic
     '--disregard fmask. one call per extension
     browse_add_files "*.bam",     fileTypeFile, br, tree()
     browse_add_files "*.mid",     fileTypeFile, br, tree()
     browse_add_files "*.xm",      fileTypeFile, br, tree()
     browse_add_files "*.it",      fileTypeFile, br, tree()
     browse_add_files "*.mod",     fileTypeFile, br, tree()
     browse_add_files "*.s3m",     fileTypeFile, br, tree()
     browse_add_files "*.ogg",     fileTypeFile, br, tree()
     browse_add_files "*.mp3",     fileTypeFile, br, tree()
     browse_add_files "*.wav",     fileTypeFile, br, tree()
    CASE browseSfx
     '--disregard fmask. one call per extension
     browse_add_files "*.wav",     fileTypeFile, br, tree()
     browse_add_files "*.ogg",     fileTypeFile, br, tree()
     browse_add_files "*.mp3",     fileTypeFile, br, tree()
    CASE browseRPG
     browse_add_files "*.rpg",     fileTypeFile, br, tree()
     browse_add_files "*.rpgdir",  fileTypeDirectory, br, tree()
    CASE browseRELOAD
     browse_add_files "*.reld",    fileTypeFile, br, tree()
     browse_add_files "*.reload",  fileTypeFile, br, tree()
     browse_add_files "*.slice",   fileTypeFile, br, tree()
     browse_add_files "*.rsav",    fileTypeFile, br, tree()
     browse_add_files "*.editor",  fileTypeFile, br, tree()
     browse_add_files "*.rgfx",    fileTypeFile, br, tree()
    CASE browseTilemap
     browse_add_files "*.tilemap", fileTypeFile, br, tree()
    CASE browseScripts
     browse_add_files "*.hs",      fileTypeFile, br, tree()
     browse_add_files "*.hsp",     fileTypeFile, br, tree()
     browse_add_files "*.hss",     fileTypeFile, br, tree()
     browse_add_files "*.txt",     fileTypeFile, br, tree()
    CASE browseDir
     IF diriswriteable(br.nowdir) THEN
      append_tree_record br, tree()
      WITH tree(br.mstate.last)
       .kind = bkSelectable
       .filename = ""
       .caption = "Select this folder"
       .about = decode_filename(br.nowdir)
      END WITH
     END IF
    CASE browsePalettedImage
     browse_add_files "*.bmp",     fileTypeFile, br, tree()
     browse_add_files "*.png",     fileTypeFile, br, tree()
    CASE browseMasterPal
     browse_add_files "*.mas",     fileTypeFile, br, tree()
     browse_add_files "*.bmp",     fileTypeFile, br, tree()
     browse_add_files "*.png",     fileTypeFile, br, tree()
    CASE browseImage, browseTileset
     browse_add_files "*.bmp",     fileTypeFile, br, tree()
     browse_add_files "*.jpeg",    fileTypeFile, br, tree()
     browse_add_files "*.jpg",     fileTypeFile, br, tree()
     browse_add_files "*.png",     fileTypeFile, br, tree()
    CASE browseAny
     showbug "browse(): browseAny with missing fmask"
    CASE ELSE
     showbug "browse(): unknown file type " & br.filetype
   END SELECT
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
   IF numeric_string_compare(tree(i).caption, tree(j).caption, YES) > 0 THEN  'case_insen=YES
    SWAP tree(i), tree(j)
   END IF
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
 correct_menu_state br.mstate

 '--don't display progress bar overtop of previous menu
 br.ranalready = YES

END SUB

FUNCTION check_is_scripts_file(filepath as string) as bool
 'This script is a hack to allow people who name their scripts .txt to use the import
 'feature without cluttering the browse interface with non-plotscript .txt files
 'Note that scanscripts.py uses a completely different autodetection method
 DIM result as bool = NO

 'We check whether these form the beginning of any line near the top of the file
 DIM indicators(...) as string = { _
  "include,", "script,", "plotscript,", "globalvariable", "defineconstant" _
 }

 DIM fh as integer
 IF OPENFILE(filepath, FOR_INPUT, fh) THEN RETURN NO
 DIM s as string
 FOR i as integer = 0 TO 49 'Only bother to check the first 50 lines uncommented lines
  LINE INPUT #fh, s
  IF EOF(fh) THEN EXIT FOR
  s = exclude(LCASE(s), !" \t")
  IF LEFT(s, 1) = "#" OR LEN(s) = 0 THEN
   i -= 1  'Don't count commented/blank lines, since they may be many of them
   CONTINUE FOR
  END IF
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
