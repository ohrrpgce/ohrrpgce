'OHRRPGCE CUSTOM - Slice Collection Editor
'(C) Copyright 1997-2008 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'Except, this module isn't especially crappy. Yay!
'

#ifdef __FB_LANG__
  #if __FB_LANG__ <> "fb"
'$DYNAMIC
    Option Explicit
  #endif
#endif

#include "compat.bi"
#include "allmodex.bi"
#include "common.bi"
#include "slices.bi"
#include "custom_udts.bi"
#include "customsubs.bi"

'==============================================================================

TYPE SliceEditMenuItem
 s AS STRING
 handle AS Slice Ptr
END TYPE

TYPE SliceEditState
 collection_number AS INTEGER
 collection_group_number AS INTEGER
 collection_file AS STRING
END TYPE

'------------------------------------------------------------------------------

ENUM EditRuleMode
  erNone              'Used for labels and links
  erIntgrabber
  erStrgrabber
  erToggle
END ENUM

TYPE EditRule
  dataptr AS ANY PTR  'It scares the heck out of me that I think this is the best solution
  mode AS EditRuleMode
  lower AS INTEGER
  upper AS INTEGER
  group AS INTEGER    'Marks this rule as a member of a numbered group, the meaning of which is defined in the implementation
  helpkey AS STRING   'actually appended to "sliceedit_" to get the full helpkey
END TYPE

'==============================================================================

DIM SHARED editable_slice_types(4) AS SliceTypes
editable_slice_types(0) = SlContainer
editable_slice_types(1) = SlRectangle
editable_slice_types(2) = SlSprite
editable_slice_types(3) = SlText
editable_slice_types(4) = SlGrid

'==============================================================================

CONST slgrPICKTYPE = 1
CONST slgrPICKXY = 2
CONST slgrPICKWH = 4
CONST slgrPICKCOL = 8
CONST slgrUPDATESPRITE = 16
CONST slgrUPDATERECTCOL = 32
CONST slgrUPDATERECTSTYLE = 64

'==============================================================================

'Public functions (will put these in a bi file if there turns out to be more than 1)
DECLARE SUB slice_editor ()

'Functions that might go better in slices.bas ... we shall see
DECLARE FUNCTION SlicePositionString (sl AS Slice Ptr) AS STRING
DECLARE SUB DrawSliceAnts (BYVAL sl AS Slice Ptr, dpage AS INTEGER)

'Functions that use awkward adoption metaphors
DECLARE SUB SliceAdoptSister (BYVAL sl AS Slice Ptr)
DECLARE SUB AdjustSlicePosToNewParent (BYVAL sl AS Slice Ptr, BYVAL newparent AS Slice Ptr)
DECLARE SUB SliceAdoptNiece (BYVAL sl AS Slice Ptr)

'Functions only used locally
DECLARE SUB slice_editor_refresh (BYREF ses AS SliceEditState, BYREF state AS MenuState, menu() AS SliceEditMenuItem, edslice AS Slice Ptr, BYREF cursor_seek AS Slice Ptr)
DECLARE SUB slice_editor_refresh_delete (BYREF index AS INTEGER, menu() AS SliceEditMenuItem)
DECLARE SUB slice_editor_refresh_append (BYREF index AS INTEGER, menu() AS SliceEditMenuItem, caption AS STRING, sl AS Slice Ptr=0)
DECLARE SUB slice_editor_refresh_recurse (BYREF index AS INTEGER, menu() AS SliceEditMenuItem, BYREF indent AS INTEGER, sl AS Slice Ptr)
DECLARE SUB slice_edit_detail (sl AS Slice Ptr, rootsl AS Slice Ptr)
DECLARE SUB slice_edit_detail_refresh (BYREF state AS MenuState, menu() AS STRING, sl AS Slice Ptr, rules() AS EditRule)
DECLARE SUB slice_edit_detail_keys (BYREF state AS MenuState, sl AS Slice Ptr, rootsl AS Slice Ptr, rules() AS EditRule)
DECLARE SUB slice_editor_xy (BYREF x AS INTEGER, BYREF y AS INTEGER, BYVAL focussl AS Slice Ptr, BYVAL rootsl AS Slice Ptr)
DECLARE FUNCTION slice_editor_filename(BYREF ses AS SliceEditState) AS STRING
DECLARE SUB slice_editor_load(BYREF edslice AS Slice Ptr, filename AS STRING)
DECLARE SUB slice_editor_save(BYVAL edslice AS Slice Ptr, filename AS STRING)

'Functions that need to be aware of magic numbers for SliceType
DECLARE FUNCTION slice_edit_detail_browse_slicetype(BYREF slice_type AS SliceTypes) AS SliceTypes

'Slice EditRule convenience functions
DECLARE SUB sliceed_rule(rules() AS EditRule, helpkey AS String, mode AS EditRuleMode, BYVAL dataptr AS ANY PTR, BYVAL lower AS INTEGER=0, BYVAL upper AS INTEGER=0, BYVAL group AS INTEGER = 0)
DECLARE SUB sliceed_rule_tog(rules() AS EditRule, helpkey AS String, BYVAL dataptr AS INTEGER PTR, BYVAL group AS INTEGER=0)
DECLARE SUB sliceed_rule_none(rules() AS EditRule, helpkey AS String, BYVAL group AS INTEGER = 0)

'==============================================================================

DIM SHARED HorizCaptions(2) AS STRING
HorizCaptions(0) = "Left"
HorizCaptions(1) = "Center"
HorizCaptions(2) = "Right"
DIM SHARED VertCaptions(2) AS STRING
VertCaptions(0) = "Top"
VertCaptions(1) = "Center"
VertCaptions(2) = "Bottom"
DIM SHARED BorderCaptions(-2 TO -1) AS STRING
BorderCaptions(-2) = "None"
BorderCaptions(-1) = "Line"
DIM SHARED TransCaptions(0 TO 2) AS STRING
TransCaptions(0) = "Solid"
TransCaptions(1) = "Fuzzy"
TransCaptions(2) = "Hollow"

'==============================================================================

SUB slice_editor ()

 DIM ses AS SliceEditState

 DIM edslice AS Slice Ptr
 edslice = NewSlice
 WITH *edslice
  .Attach = slScreen
  .SliceType = slRoot
  .Fill = YES
 END WITH

 IF isfile(slice_editor_filename(ses)) THEN
  SliceLoadFromFile edslice, slice_editor_filename(ses)
 ELSE
  '--FIXME: this backcompat is of very low importance (probably only matters to James)
  '--and can be removed completely before Zenzizenzic
  SliceLoadFromFile edslice, workingdir & SLASH & "slicetree_0.reld"
  safekill workingdir & SLASH & "slicetree_0.reld"
 END IF

 DIM menu(0) AS SliceEditMenuItem
 DIM plainmenu(0) AS STRING 'FIXME: This is a hack because I didn't want to re-implement standardmenu right now

 DIM state AS MenuState
 WITH state
  .size = 21
  .need_update = YES
 END WITH
 DIM cursor_seek AS Slice Ptr = 0

 DIM slice_type AS SliceTypes
 DIM shift AS INTEGER

 DIM filename AS STRING
 
 DIM jump_to_collection AS INTEGER

 '--this early draw ensures that all the slices are updated before the loop starts
 DrawSlice edslice, dpage

 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(scEsc) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "sliceedit"

  IF state.need_update THEN
   slice_editor_refresh(ses, state, menu(), edslice, cursor_seek)
   REDIM plainmenu(state.last) AS STRING
   FOR i AS INTEGER = 0 TO UBOUND(plainmenu)
    plainmenu(i) = menu(i).s
   NEXT i
   state.need_update = NO
  END IF

  shift = (keyval(scLeftShift) > 0 OR keyval(scRightShift) > 0)
  IF enter_or_space() THEN
   IF state.pt = 0 THEN
    EXIT DO
   ELSE
    slice_edit_detail menu(state.pt).handle, edslice
    state.need_update = YES
   END IF 
  END IF
  IF state.pt = 1 THEN
   '--Browse collections
   jump_to_collection = ses.collection_number
   IF intgrabber(jump_to_collection, 0, 32767) THEN
    slice_editor_save edslice, slice_editor_filename(ses)
    ses.collection_number = jump_to_collection
    slice_editor_load edslice, slice_editor_filename(ses)
    state.need_update = YES
   END IF
  END IF
  IF keyval(scE) > 1 THEN
   filename = inputfilename("Export slice collection", ".slice", "", "input_filename_export_slices")
   IF filename <> "" THEN
    SliceSaveToFile edslice, filename & ".slice"
   END IF
  END IF
  IF keyval(scI) > 1 THEN
   filename = browse(0, "", "*.slice", "",, "browse_import_slices")
   IF filename <> "" THEN
    slice_editor_load edslice, filename
    state.need_update = YES
   END IF
  END IF
  IF keyval(scPlus) > 1 OR keyval(scNumpadPlus) THEN
   IF slice_edit_detail_browse_slicetype(slice_type) THEN
    IF state.pt > 1 THEN
     InsertSliceBefore menu(state.pt).handle, NewSliceOfType(slice_type)
    ELSE
     cursor_seek = NewSliceOfType(slice_type, edslice)
    END IF
    state.need_update = YES
   END IF
  END IF
  IF keyval(scDelete) > 1 THEN
   IF yesno("Delete this " & SliceTypeName(slice_type) & " slice?", NO) THEN
    slice_editor_refresh_delete state.pt, menu()
    state.need_update = YES
   END IF
  END IF
  IF state.pt > 0 THEN
   IF shift THEN
    IF menu(state.pt).handle THEN
     IF keyval(scUp) > 1 THEN
      SwapSiblingSlices menu(state.pt).handle, menu(state.pt).handle->PrevSibling
      cursor_seek = menu(state.pt).handle
      state.need_update = YES
     END IF
     IF keyval(scDown) > 1 AND state.pt < state.last THEN
      SwapSiblingSlices menu(state.pt).handle, menu(state.pt).handle->NextSibling
      cursor_seek = menu(state.pt).handle
      state.need_update = YES
     END IF
     IF keyval(scRight) > 1 THEN
      SliceAdoptSister menu(state.pt).handle
      cursor_seek = menu(state.pt).handle
      state.need_update = YES
     END IF
      If keyval(scLeft) > 1 THEN
      SliceAdoptNiece menu(state.pt).handle
      cursor_seek = menu(state.pt).handle
      state.need_update = YES
     END IF
    END IF
   END IF
  END IF
  IF NOT shift THEN
   IF keyval(scLeft) > 1 THEN
    IF menu(state.pt).handle THEN
     cursor_seek = (menu(state.pt).handle)->parent
     state.need_update = YES
    END IF
   END IF
  END IF
  IF state.need_update = NO THEN
   'Only do normal cursor movement when no updates are needed
   usemenu state
  END IF

  DrawSlice edslice, dpage
  IF state.pt > 0 THEN
   DrawSliceAnts menu(state.pt).handle, dpage
  END IF
  standardmenu plainmenu(), state, 0, 0, dpage, YES
  edgeprint "+ to add a slice. SHIFT+arrows to sort", 0, 190, uilook(uiText), dpage

  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait

  updatepagesize dpage
  WITH *edslice
   .Width = vpages(dpage)->w
   .Height = vpages(dpage)->h
  END WITH
 LOOP

 slice_editor_save edslice, slice_editor_filename(ses)
 
 DeleteSlice @edslice

END SUB

SUB slice_editor_load(BYREF edslice AS Slice Ptr, filename AS STRING)
 DeleteSlice @edslice
 edslice = NewSlice
 WITH *edslice
  .Attach = slScreen
  .SliceType = slRoot
  .Fill = YES
 END WITH
 IF isfile(filename) THEN
  SliceLoadFromFile edslice, filename
 END IF
END SUB

SUB slice_editor_save(BYVAL edslice AS Slice Ptr, filename AS STRING)
 IF edslice->NumChildren > 0 THEN
  '--save non-empty slice collections
  SliceSaveToFile edslice, filename
 ELSE
  '--erase empty slice collections
  safekill filename
 END IF
END SUB

SUB slice_edit_detail (sl AS Slice Ptr, rootsl AS Slice Ptr)

 IF sl = 0 THEN EXIT SUB

 DIM menu(0) AS STRING
 DIM rules(0) AS EditRule

 DIM state AS MenuState
 WITH state
  .size = 22
  .need_update = YES
 END WITH

 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(scEsc) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "sliceedit_" & rules(state.pt).helpkey

  IF state.need_update THEN
   slice_edit_detail_refresh state, menu(), sl, rules()
   state.need_update = NO
  END IF

  usemenu state
  IF state.pt = 0 AND enter_or_space() THEN EXIT DO
  slice_edit_detail_keys state, sl, rootsl, rules()
  
  DrawSlice rootsl, dpage
  DrawSliceAnts sl, dpage
  standardmenu menu(), state, 0, 0, dpage, YES

  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait

  updatepagesize dpage
  WITH *rootsl
   .Width = vpages(dpage)->w
   .Height = vpages(dpage)->h
  END WITH
 LOOP
END SUB

SUB slice_edit_detail_keys (BYREF state AS MenuState, sl AS Slice Ptr, rootsl AS Slice Ptr, rules() AS EditRule)
 DIM rule AS EditRule = rules(state.pt)
 SELECT CASE rule.mode
  CASE erIntgrabber
   DIM n AS INTEGER PTR = rule.dataptr
   IF intgrabber(*n, rule.lower, rule.upper) THEN
    state.need_update = YES
   END IF
  CASE erToggle
   DIM n AS INTEGER PTR = rule.dataptr
   IF intgrabber(*n, -1, 0) THEN
    state.need_update = YES
   END IF
   IF enter_or_space() THEN *n = NOT *n : state.need_update = YES
  CASE erStrgrabber
   DIM s AS STRING PTR = rule.dataptr
   IF strgrabber(*s, 256) THEN 'FIXME: this is a bad max length. Should there be a max?
    state.need_update = YES
   END IF
 END SELECT
 DIM switchtype AS INTEGER = NO
 IF rule.group AND slgrPICKTYPE THEN
  DIM slice_type AS SliceTypes = sl->SliceType
  DIM slice_type_num AS INTEGER = 0
  FOR i AS INTEGER = 0 TO UBOUND(editable_slice_types)
   IF slice_type = editable_slice_types(i) THEN slice_type_num = i
  NEXT i
  IF intgrabber(slice_type_num, 0, UBOUND(editable_slice_types)) THEN
   slice_type = editable_slice_types(slice_type_num)
   state.need_update = YES
   switchtype = YES
  END IF
  IF enter_or_space() THEN
   IF slice_edit_detail_browse_slicetype(slice_type) THEN
    state.need_update = YES
    switchtype = YES
   END IF
  END IF
  IF switchtype THEN
   ReplaceSliceType sl, NewSliceOfType(slice_type)
   switchtype = NO
  END IF
 END IF
 IF rule.group AND slgrPICKXY THEN
  IF enter_or_space() THEN
   slice_editor_xy sl->X, sl->Y, sl, rootsl
   state.need_update = YES
  END IF
 END IF
 IF rule.group AND slgrPICKWH THEN
  IF enter_or_space() THEN
   slice_editor_xy sl->Width, sl->Height, sl, rootsl
   state.need_update = YES
  END IF
 END IF
 IF rule.group AND slgrPICKCOL THEN
  IF enter_or_space() THEN
   DIM n AS INTEGER PTR = rule.dataptr
   *n = color_browser_256(*n)
   state.need_update = YES
  END IF
 END IF
 IF rule.group AND slgrUPDATESPRITE THEN
  IF state.need_update THEN
   'state.need_update is cleared at the top of the loop
   DIM dat AS SpriteSliceData Ptr
   dat = sl->SliceData
   dat->loaded = NO
   dat->record = small(dat->record, gen(sprite_sizes(dat->spritetype).genmax))
   dat->frame = small(dat->frame, sprite_sizes(dat->spritetype).frames - 1)
  END IF
 END IF
 IF rule.group AND slgrUPDATERECTCOL THEN
  IF state.need_update THEN
   DIM dat AS RectangleSliceData Ptr
   dat = sl->SliceData
   dat->style = -1
   dat->style_loaded = NO
  END IF
 END IF
 IF rule.group AND slgrUPDATERECTSTYLE THEN
  IF state.need_update THEN
   DIM dat AS RectangleSliceData Ptr
   dat = sl->SliceData
   dat->style_loaded = NO
  END IF
 END IF
 IF state.need_update AND sl->SliceType = slText THEN
  UpdateTextSlice sl
 END IF
END SUB

FUNCTION slice_editor_filename(BYREF ses AS SliceEditState) AS STRING
 RETURN workingdir & SLASH & "slicetree_" & ses.collection_group_number & "_" & ses.collection_number & ".reld"
END FUNCTION

SUB slice_editor_xy (BYREF x AS INTEGER, BYREF y AS INTEGER, BYVAL focussl AS Slice Ptr, BYVAL rootsl AS Slice Ptr)
 DIM shift AS INTEGER = 0
 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(scEsc) > 1 THEN EXIT DO
  IF enter_or_space() THEN EXIT DO
  shift = (keyval(scLeftShift) > 0 OR keyval(scRightShift) > 0)
  IF keyval(scUp)    > 0 THEN y -= 1 + 9 * ABS(shift)
  IF keyval(scRight) > 0 THEN x += 1 + 9 * ABS(shift)
  IF keyval(scDown)  > 0 THEN y += 1 + 9 * ABS(shift)
  IF keyval(scLeft)  > 0 THEN x -= 1 + 9 * ABS(shift)
  DrawSlice rootsl, dpage
  DrawSliceAnts focussl, dpage
  edgeprint "Arrow keys to edit, SHIFT for speed", 0, 190, uilook(uiText), dpage
  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
END SUB

SUB sliceed_rule(rules() AS EditRule, helpkey AS String, mode AS EditRuleMode, BYVAL dataptr AS ANY PTR, BYVAL lower AS INTEGER=0, BYVAL upper AS INTEGER=0, BYVAL group AS INTEGER = 0)
 DIM index AS INTEGER = UBOUND(rules) + 1
 REDIM PRESERVE rules(index) AS EditRule
 WITH rules(index)
  .dataptr = dataptr
  .mode = mode
  .lower = lower
  .upper = upper
  .group = group
  .helpkey = helpkey
 END WITH 
END SUB

SUB sliceed_rule_none(rules() AS EditRule, helpkey AS String, BYVAL group AS INTEGER = 0)
 sliceed_rule rules(), helpkey, erNone, 0, 0, 0, group
END SUB

SUB sliceed_rule_tog(rules() AS EditRule, helpkey AS String, BYVAL dataptr AS INTEGER PTR, BYVAL group AS INTEGER=0)
 sliceed_rule rules(), helpkey, erToggle, dataptr, -1, 0, group
END SUB

SUB slice_edit_detail_refresh (BYREF state AS MenuState, menu() AS STRING, sl AS Slice Ptr, rules() AS EditRule)
 REDIM menu(5)
 REDIM rules(0)
 rules(0).helpkey = "detail"
 menu(0) = "Previous Menu"
 WITH *sl
  menu(1) = "Slice type: " & SliceTypeName(sl)
  sliceed_rule_none rules(), "slicetype", slgrPICKTYPE
  menu(2) = "X: " & .X
  sliceed_rule rules(), "pos", erIntgrabber, @.X, -9999, 9999, slgrPICKXY
  menu(3) = "Y: " & .Y
  sliceed_rule rules(), "pos", erIntgrabber, @.Y, -9999, 9999, slgrPICKXY
  menu(4) = "Width: " & .Width
  sliceed_rule rules(), "size", erIntgrabber, @.Width, 0, 9999, slgrPICKWH
  menu(5) = "Height: " & .Height
  sliceed_rule rules(), "size", erIntgrabber, @.Height, 0, 9999, slgrPICKWH
  SELECT CASE .SliceType
   CASE slRectangle
    DIM dat AS RectangleSliceData Ptr
    dat = .SliceData
    str_array_append menu(), "Style: " & defaultint(dat->style, "None")
    sliceed_rule rules(), "rect_style", erIntgrabber, @(dat->style), -1, 14, slgrUPDATERECTSTYLE
    str_array_append menu(), "Background color: " & defaultint(dat->bgcol)
    sliceed_rule rules(), "rect_bg", erIntgrabber, @(dat->bgcol), 0, 255, (slgrUPDATERECTCOL OR slgrPICKCOL)
    str_array_append menu(), "Foreground color: " & defaultint(dat->fgcol)
    sliceed_rule rules(), "rect_fg", erIntgrabber, @(dat->fgcol), 0, 255, (slgrUPDATERECTCOL OR slgrPICKCOL)
    str_array_append menu(), "Border: " & caption_or_int(dat->border, BorderCaptions())
    sliceed_rule rules(), "rect_border", erIntgrabber, @(dat->border), -2, 14, slgrUPDATERECTCOL 
    str_array_append menu(), "Translucency: " & TransCaptions(dat->translucent)
    sliceed_rule rules(), "rect_trans", erIntgrabber, @(dat->translucent), 0, 2
   CASE slText
    DIM dat AS TextSliceData Ptr
    dat = .SliceData
    str_array_append menu(), "Text: " & dat->s
    sliceed_rule rules(), "text_text", erStrgrabber, @(dat->s), 0, 0
    str_array_append menu(), "Color: " & zero_default(dat->col)
    sliceed_rule rules(), "text_color", erIntgrabber, @(dat->col), 0, 255, slgrPICKCOL
    str_array_append menu(), "Outline: " & yesorno(dat->outline)
    sliceed_rule_tog rules(), "text_outline", @(dat->outline)
    str_array_append menu(), "Wrap: " & yesorno(dat->wrap)
    sliceed_rule_tog rules(), "text_wrap", @(dat->wrap)
    str_array_append menu(), "Background Color: " & zero_default(dat->bgcol)
    sliceed_rule rules(), "text_bg", erIntgrabber, @(dat->bgcol), 0, 255, slgrPICKCOL
   CASE slSprite
    DIM dat AS SpriteSliceData Ptr
    dat = .SliceData
    str_array_append menu(), "Sprite Type: " & sprite_sizes(dat->spritetype).name
    sliceed_rule rules(), "sprite_type", erIntgrabber, @(dat->spritetype), 0, 8, slgrUPDATESPRITE
    str_array_append menu(), "Sprite Number: " & dat->record
    sliceed_rule rules(), "sprite_rec", erIntgrabber, @(dat->record), 0, gen(sprite_sizes(dat->spritetype).genmax), slgrUPDATESPRITE
    str_array_append menu(), "Sprite Palette: " & defaultint(dat->pal)
    sliceed_rule rules(), "sprite_pal", erIntgrabber, @(dat->pal), -1, gen(genMaxPal), slgrUPDATESPRITE
    str_array_append menu(), "Sprite Frame: " & dat->frame
    sliceed_rule rules(), "sprite_frame", erIntgrabber, @(dat->frame), 0, sprite_sizes(dat->spritetype).frames - 1
    str_array_append menu(), "Flip horiz.: " & yesorno(dat->flipHoriz)
    sliceed_rule_tog rules(), "sprite_flip", @(dat->flipHoriz), slgrUPDATESPRITE
    str_array_append menu(), "Flip vert.: " & yesorno(dat->flipVert)
    sliceed_rule_tog rules(), "sprite_flip", @(dat->flipVert), slgrUPDATESPRITE
   CASE slGrid
    DIM dat AS GridSliceData Ptr
    dat = .SliceData
    str_array_append menu(), "Rows: " & dat->rows
    sliceed_rule rules(), "grid_rows", erIntgrabber, @(dat->rows), 0, 99 'FIXME: upper limit of 99 is totally arbitrary
    str_array_append menu(), "Columns: " & dat->cols
    sliceed_rule rules(), "grid_cols", erIntgrabber, @(dat->cols), 0, 99 'FIXME: upper limit of 99 is totally arbitrary
    str_array_append menu(), "Show Grid: " & yesorno(dat->show)
    sliceed_rule_tog rules(), "grid_show", @(dat->show)
  END SELECT
  str_array_append menu(), "Visible: " & yesorno(.Visible)
  sliceed_rule_tog rules(), "vis", @.Visible
  str_array_append menu(), "Fill Parent: " & yesorno(.Fill)
  sliceed_rule_tog rules(), "fill", @.Fill
  str_array_append menu(), "Clip Children: " & yesorno(.Clip)
  sliceed_rule_tog rules(), "clip", @.Clip
  IF .Fill = NO THEN
   str_array_append menu(), "Align horiz. with: " & HorizCaptions(.AlignHoriz)
   sliceed_rule rules(), "align", erIntgrabber, @.AlignHoriz, 0, 2
   str_array_append menu(), "Align vert. with: " & VertCaptions(.AlignVert)
   sliceed_rule rules(), "align", erIntgrabber, @.AlignVert, 0, 2
   str_array_append menu(), "Anchor horiz. on: " & HorizCaptions(.AnchorHoriz)
   sliceed_rule rules(), "anchor", erIntgrabber, @.AnchorHoriz, 0, 2
   str_array_append menu(), "Anchor vert. on: " & VertCaptions(.AnchorVert)
   sliceed_rule rules(), "anchor", erIntgrabber, @.AnchorVert, 0, 2
  END IF
  str_array_append menu(), "Padding Top: " & .PaddingTop
  sliceed_rule rules(), "padding", erIntgrabber, @.PaddingTop, -9999, 9999
  str_array_append menu(), "Padding Right: " & .PaddingRight
  sliceed_rule rules(), "padding", erIntgrabber, @.PaddingRight, -9999, 9999
  str_array_append menu(), "Padding Bottom: " & .PaddingBottom
  sliceed_rule rules(), "padding", erIntgrabber, @.PaddingBottom, -9999, 9999
  str_array_append menu(), "Padding Left: " & .PaddingLeft
  sliceed_rule rules(), "padding", erIntgrabber, @.PaddingLeft, -9999, 9999
 END WITH
  
 state.last = UBOUND(menu)
 state.pt = small(state.pt, state.last)
 state.top = small(state.top, state.pt)
END SUB

FUNCTION slice_edit_detail_browse_slicetype(BYREF slice_type AS SliceTypes) AS SliceTypes

 DIM state AS MenuState
 WITH state
  .last = UBOUND(editable_slice_types)
  .size = 22
 END WITH

 DIM menu(UBOUND(editable_slice_types)) AS STRING
 FOR i AS INTEGER = 0 TO UBOUND(menu)
  menu(i) = SliceTypeName(editable_slice_types(i))
  IF editable_slice_types(i) = slice_type THEN state.pt = i
 NEXT i

 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(scEsc) > 1 THEN RETURN NO
  IF keyval(scF1) > 1 THEN show_help "slicedit_browse_slicetype"

  usemenu state
  
  IF enter_or_space() THEN
   slice_type = editable_slice_types(state.pt)
   RETURN YES
  END IF
  
  standardmenu menu(), state, 0, 0, dpage
 
  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
 RETURN NO 
END FUNCTION

FUNCTION SlicePositionString (sl AS Slice Ptr) AS STRING
 'This shows the absolute screen position of a slice.
 WITH *sl
  RETURN .ScreenX & "," & .ScreenY & "(" & .Width & "x" & .Height & ")"
 END WITH
END FUNCTION

SUB slice_editor_refresh (BYREF ses AS SliceEditState, BYREF state AS MenuState, menu() AS SliceEditMenuItem, edslice AS Slice Ptr, BYREF cursor_seek AS Slice Ptr)
 FOR i AS INTEGER = 0 TO UBOUND(menu)
  menu(i).s = ""
 NEXT i
 DIM index AS INTEGER = 0

 DIM indent AS INTEGER = 0
 slice_editor_refresh_append index, menu(), "Previous Menu"
 slice_editor_refresh_append index, menu(), CHR(27) & " Slice Collection " & ses.collection_number & " " & CHR(26)
 slice_editor_refresh_recurse index, menu(), indent, edslice

 IF cursor_seek <> 0 THEN
  FOR i AS INTEGER = 0 TO index - 1
   IF menu(i).handle = cursor_seek THEN
    state.pt = i
    cursor_seek = 0
    EXIT FOR
   END IF
  NEXT i
 END IF
 
 WITH state
  .last = index - 1
  .pt = small(.pt, .last)
  .top = bound(.top, .pt - .size, .pt)
 END WITH
END SUB

SUB slice_editor_refresh_delete (BYREF index AS INTEGER, menu() AS SliceEditMenuItem)
 DeleteSlice @(menu(index).handle)
 FOR i AS INTEGER = index + 1 TO UBOUND(menu)
  SWAP menu(i), menu(i - 1)
 NEXT i
END SUB

SUB slice_editor_refresh_append (BYREF index AS INTEGER, menu() AS SliceEditMenuItem, caption AS STRING, sl AS Slice Ptr=0)
 IF index > UBOUND(menu) THEN
  REDIM PRESERVE menu(index + 10) AS SliceEditMenuItem
 END IF
 menu(index).s = caption
 menu(index).handle = sl
 index += 1
END SUB

SUB slice_editor_refresh_recurse (BYREF index AS INTEGER, menu() AS SliceEditMenuItem, BYREF indent AS INTEGER, sl AS Slice Ptr)
 WITH *sl
  DIM caption AS STRING
  caption = STRING(indent, " ")
  caption = caption & SliceTypeName(sl)
  caption = caption & " " & SlicePositionString(sl)
  IF .SliceType <> slRoot THEN
   slice_editor_refresh_append index, menu(), caption, sl
   indent += 1
  END IF
  'Now append the children
  DIM ch AS slice ptr = .FirstChild
  DO WHILE ch <> 0
   slice_editor_refresh_recurse index, menu(), indent, ch
   ch = ch->NextSibling
  LOOP
  IF .SliceType <> slRoot THEN
   indent -= 1
  END IF
 END WITH
END SUB

SUB SliceAdoptSister (BYVAL sl AS Slice Ptr)
 DIM newparent AS Slice Ptr = sl->PrevSibling
 IF newparent = 0 THEN EXIT SUB ' Eldest sibling can't be adopted
 '--Adopt self to elder sister's family
 SetSliceParent sl, newparent
 AdjustSlicePosToNewParent sl, newparent
END SUB

SUB SliceAdoptNiece (BYVAL sl AS Slice Ptr)
 DIM oldparent AS Slice Ptr = sl->Parent
 IF oldparent = 0 THEN EXIT SUB ' No parent
 DIM newparent AS Slice Ptr = sl->Parent->Parent
 IF newparent = 0 THEN EXIT SUB ' No grandparent
 'Adopt self to parent's family
 SetSliceParent sl, newparent
 'Make sure that the slice is the first sibling after its old parent
 SwapSiblingSlices sl, oldparent->NextSibling
 AdjustSlicePosToNewParent sl, newparent
END SUB

SUB AdjustSlicePosToNewParent (BYVAL sl AS Slice Ptr, BYVAL newparent AS Slice Ptr)
 '--Re-adjust ScreenX/ScreenY position for new parent
 IF newparent->SliceType = slGrid THEN
  '--except if the new parent is a grid. Then it would be silly to preserve Screen pos.
  sl->X = 0
  sl->Y = 0
  EXIT SUB
 END IF
 DIM oldpos AS XYPair
 oldpos.x = sl->ScreenX
 oldpos.y = sl->ScreenY
 RefreshSliceScreenPos sl
 DIM newpos AS XYPair
 newpos.x = sl->ScreenX
 newpos.y = sl->ScreenY
 sl->X += oldpos.x - newpos.x
 sl->Y += oldpos.y - newpos.y
END SUB

SUB DrawSliceAnts (BYVAL sl AS Slice Ptr, dpage AS INTEGER)
 STATIC ant AS INTEGER = 0
 IF sl = 0 THEN EXIT SUB
 DIM col AS INTEGER
 '--Draw verticals
 FOR i AS INTEGER = 0 TO large(ABS(sl->Height) - 1, 2)
  SELECT CASE (i + ant) MOD 3
   CASE 0: CONTINUE FOR
   CASE 1: col = uiLook(uiText)
   CASE 2: col = uiLook(uiBackground)
  END SELECT
  putpixel sl->ScreenX, sl->ScreenY + i, col, dpage
  putpixel sl->ScreenX + sl->Width - 1, sl->ScreenY + i, col, dpage
 NEXT i
 '--Draw horizontals
 FOR i AS INTEGER = 0 TO large(ABS(sl->Width) - 1, 2)
  SELECT CASE (i + ant) MOD 3
   CASE 0: CONTINUE FOR
   CASE 1: col = uiLook(uiText)
   CASE 2: col = uiLook(uiBackground)
  END SELECT
  putpixel sl->ScreenX + i, sl->ScreenY, col, dpage
  putpixel sl->ScreenX + i, sl->ScreenY + sl->Height - 1, col, dpage
 NEXT i
 '--Draw gridlines if this is a grid
 IF sl->SliceType = slGrid THEN
  DIM dat AS GridSliceData Ptr = sl->SliceData
  IF dat THEN
   DIM w AS INTEGER = sl->Width \ large(1, dat->cols)
   DIM h AS INTEGER = sl->Height \ large(1, dat->rows)
   '--draw verticals
   FOR i AS INTEGER = 1 TO dat->cols - 1
    FOR y AS INTEGER = 0 TO large(ABS(sl->Height) - 1, 2)
     SELECT CASE (y + ant) MOD 3
      CASE 0: CONTINUE FOR
      CASE 1: col = uiLook(uiText)
      CASE 2: col = uiLook(uiBackground)
     END SELECT
     putpixel sl->ScreenX + i * w, sl->ScreenY + y, col, dpage
    NEXT y
   NEXT i
   '--draw horizontals
   FOR i AS INTEGER = 1 TO dat->rows - 1
    FOR x AS INTEGER = 0 TO large(ABS(sl->Width) - 1, 2)
     SELECT CASE (x + ant) MOD 3
      CASE 0: CONTINUE FOR
      CASE 1: col = uiLook(uiText)
      CASE 2: col = uiLook(uiBackground)
     END SELECT
     putpixel sl->ScreenX + x, sl->ScreenY + i * h, col, dpage
    NEXT x
   NEXT i
  END IF
 END IF
 ant = loopvar(ant, 0, 2, 1)
END SUB

