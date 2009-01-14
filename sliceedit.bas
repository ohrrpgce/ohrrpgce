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

#include "allmodex.bi"
#include "common.bi"
#include "slices.bi"
#include "scancodes.bi"
#include "custom_udts.bi"
#include "customsubs.bi"

'==============================================================================

TYPE SliceEditMenuItem
 s AS STRING
 handle AS Slice Ptr
END TYPE

'==============================================================================

DIM SHARED editable_slice_types(2) AS SliceTypes
editable_slice_types(0) = SlRectangle
editable_slice_types(1) = SlSprite
editable_slice_types(2) = SlText

'==============================================================================

CONST slgrPICKTYPE = 1
CONST slgrPICKXY = 2
CONST slgrPICKWH = 3
CONST slgrPICKCOL = 4
CONST slgrUPDATESPRITE = 5

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
DECLARE SUB slice_editor_refresh (BYREF state AS MenuState, menu() AS SliceEditMenuItem, edslice AS Slice Ptr, BYREF cursor_seek AS Slice Ptr)
DECLARE SUB slice_editor_refresh_append (BYREF index AS INTEGER, menu() AS SliceEditMenuItem, caption AS STRING, sl AS Slice Ptr=0)
DECLARE SUB slice_editor_refresh_recurse (BYREF index AS INTEGER, menu() AS SliceEditMenuItem, BYREF indent AS INTEGER, sl AS Slice Ptr)
DECLARE SUB slice_edit_detail (sl AS Slice Ptr, rootsl AS Slice Ptr)
DECLARE SUB slice_edit_detail_refresh (BYREF state AS MenuState, menu() AS STRING, sl AS Slice Ptr, rules() AS EditRule)
DECLARE SUB slice_edit_detail_keys (BYREF state AS MenuState, sl AS Slice Ptr, rootsl AS Slice Ptr, rules() AS EditRule)
DECLARE SUB slice_editor_xy (BYREF x AS INTEGER, BYREF y AS INTEGER, BYVAL focussl AS Slice Ptr, BYVAL rootsl AS Slice Ptr)

'Functions that need to be aware of magic numbers for SliceType
DECLARE FUNCTION slice_edit_detail_browse_slicetype(BYREF slice_type AS SliceTypes) AS SliceTypes

'Slice EditRule convenience functions
DECLARE SUB sliceed_rule(rules() AS EditRule, mode AS EditRuleMode, BYVAL dataptr AS ANY PTR, BYVAL lower AS INTEGER=0, BYVAL upper AS INTEGER=0, BYVAL group AS INTEGER = 0)
DECLARE SUB sliceed_rule_tog(rules() AS EditRule, BYVAL dataptr AS INTEGER PTR, BYVAL group AS INTEGER=0)
DECLARE SUB sliceed_rule_none(rules() AS EditRule, BYVAL group AS INTEGER = 0)

'==============================================================================

DIM SHARED HorizCaptions(2) AS STRING
DIM SHARED VertCaptions(2) AS STRING
HorizCaptions(0) = "Left"
HorizCaptions(1) = "Center"
HorizCaptions(2) = "Right"
VertCaptions(0) = "Top"
VertCaptions(1) = "Center"
VertCaptions(2) = "Bottom"

'==============================================================================

SUB slice_editor ()

 DIM edslice AS Slice Ptr
 edslice = NewSlice
 WITH *edslice
  .Attach = slScreen
  .SliceType = slRoot
  .Width = 320
  .Height = 200
 END WITH

 DIM filename AS STRING
 filename = workingdir & SLASH & "slicetree_0.txt"
 IF isfile(filename) THEN
  DIM fr AS SliceFileRead
  OpenSliceFileRead fr, filename
  LoadSlice fr, edslice
  CloseSliceFileRead fr
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

  setkeys
 DO
  setwait 55
  setkeys
  IF keyval(scEsc) > 1 THEN EXIT DO

  IF state.need_update THEN
   slice_editor_refresh(state, menu(), edslice, cursor_seek)
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
  IF keyval(scPlus) > 1 OR keyval(scNumpadPlus) THEN
   IF slice_edit_detail_browse_slicetype(slice_type) THEN
    IF state.pt > 0 THEN
     InsertSiblingSlice menu(state.pt).handle, NewSliceOfType(slice_type)
    ELSE
     NewSliceOfType(slice_type, edslice)
    END IF
    state.need_update = YES
   END IF
  END IF
  IF state.pt > 0 THEN
   IF shift THEN
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
 LOOP

 DIM fw AS SliceFileWrite
 OpenSliceFileWrite fw, workingdir & SLASH & "slicetree_0.txt"
 SaveSlice fw, edslice
 CloseSliceFileWrite fw
 DeleteSlice @edslice

END SUB

SUB slice_edit_detail (sl AS Slice Ptr, rootsl AS Slice Ptr)

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
 SELECT CASE rule.group
  CASE slgrPICKTYPE:
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
  CASE slgrPICKXY:
   IF enter_or_space() THEN
    slice_editor_xy sl->X, sl->Y, sl, rootsl
    state.need_update = YES
   END IF
  CASE slgrPICKWH:
   IF enter_or_space() THEN
    slice_editor_xy sl->Width, sl->Height, sl, rootsl
    state.need_update = YES
   END IF
  CASE slgrPICKCOL:
   IF enter_or_space() THEN
    DIM n AS INTEGER PTR = rule.dataptr
    *n = color_browser_256(*n)
    state.need_update = YES
   END IF
  CASE slgrUPDATESPRITE
   IF state.need_update THEN
    'state.need_update is cleared at the top of the loop
    DIM dat AS SpriteSliceData Ptr
    dat = sl->SliceData
    dat->loaded = NO
    dat->record = small(dat->record, gen(sprite_sizes(dat->spritetype).genmax))
    dat->frame = small(dat->frame, sprite_sizes(dat->spritetype).frames - 1)
   END IF
 END SELECT
END SUB

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

SUB sliceed_rule(rules() AS EditRule, mode AS EditRuleMode, BYVAL dataptr AS ANY PTR, BYVAL lower AS INTEGER=0, BYVAL upper AS INTEGER=0, BYVAL group AS INTEGER = 0)
 DIM index AS INTEGER = UBOUND(rules) + 1
 REDIM PRESERVE rules(index) AS EditRule
 WITH rules(index)
  .dataptr = dataptr
  .mode = mode
  .lower = lower
  .upper = upper
  .group = group
 END WITH 
END SUB

SUB sliceed_rule_none(rules() AS EditRule, BYVAL group AS INTEGER = 0)
 sliceed_rule rules(), erNone, 0, 0, 0, group
END SUB

SUB sliceed_rule_tog(rules() AS EditRule, BYVAL dataptr AS INTEGER PTR, BYVAL group AS INTEGER=0)
 sliceed_rule rules(), erToggle, dataptr, -1, 0, group
END SUB

SUB slice_edit_detail_refresh (BYREF state AS MenuState, menu() AS STRING, sl AS Slice Ptr, rules() AS EditRule)
 REDIM menu(5)
 REDIM rules(0)
 menu(0) = "Previous Menu"
 WITH *sl
  menu(1) = "Slice type: " & SliceTypeName(sl)
  sliceed_rule_none rules(), slgrPICKTYPE
  menu(2) = "X: " & .X
  sliceed_rule rules(), erIntgrabber, @.X, -9999, 9999, slgrPICKXY
  menu(3) = "Y: " & .Y
  sliceed_rule rules(), erIntgrabber, @.Y, -9999, 9999, slgrPICKXY
  menu(4) = "Width: " & .Width
  sliceed_rule rules(), erIntgrabber, @.Width, 0, 9999, slgrPICKWH
  menu(5) = "Height: " & .Height
  sliceed_rule rules(), erIntgrabber, @.Height, 0, 9999, slgrPICKWH
  SELECT CASE .SliceType
   CASE slRectangle
    DIM dat AS RectangleSliceData Ptr
    dat = .SliceData
    string_array_grow_append menu(), "Background color: " & defaultint(dat->bgcol)
    sliceed_rule rules(), erIntgrabber, @(dat->bgcol), 0, 255, slgrPICKCOL
    string_array_grow_append menu(), "Foreground color: " & defaultint(dat->fgcol)
    sliceed_rule rules(), erIntgrabber, @(dat->fgcol), 0, 255, slgrPICKCOL
    string_array_grow_append menu(), "Transparent: " & yesorno(dat->transparent)
    sliceed_rule_tog rules(), @(dat->transparent)
    string_array_grow_append menu(), "Border: " & yesorno(dat->border)
    sliceed_rule_tog rules(), @(dat->border)
   CASE slText
    DIM dat AS TextSliceData Ptr
    dat = .SliceData
    string_array_grow_append menu(), "Text: " & dat->s
    sliceed_rule rules(), erStrgrabber, @(dat->s), 0, 0
    string_array_grow_append menu(), "Color: " & zero_default(dat->col)
    sliceed_rule rules(), erIntgrabber, @(dat->col), 0, 255, slgrPICKCOL
    string_array_grow_append menu(), "Outline: " & yesorno(dat->outline)
    sliceed_rule_tog rules(), @(dat->outline)
    string_array_grow_append menu(), "Wrap: " & yesorno(dat->wrap)
    sliceed_rule_tog rules(), @(dat->wrap)
   CASE slSprite
    DIM dat AS SpriteSliceData Ptr
    dat = .SliceData
    string_array_grow_append menu(), "Sprite Type: " & sprite_sizes(dat->spritetype).name
    sliceed_rule rules(), erIntgrabber, @(dat->spritetype), 0, 8, slgrUPDATESPRITE
    string_array_grow_append menu(), "Sprite Number: " & dat->record
    sliceed_rule rules(), erIntgrabber, @(dat->record), 0, gen(sprite_sizes(dat->spritetype).genmax), slgrUPDATESPRITE
    string_array_grow_append menu(), "Sprite Palette: " & defaultint(dat->pal)
    sliceed_rule rules(), erIntgrabber, @(dat->pal), -1, gen(genMaxPal), slgrUPDATESPRITE
    string_array_grow_append menu(), "Sprite Frame: " & dat->frame
    sliceed_rule rules(), erIntgrabber, @(dat->frame), 0, sprite_sizes(dat->spritetype).frames - 1
    string_array_grow_append menu(), "Flip horiz.: " & yesorno(dat->flipHoriz)
    sliceed_rule_tog rules(), @(dat->flipHoriz), slgrUPDATESPRITE
    string_array_grow_append menu(), "Flip vert.: " & yesorno(dat->flipVert)
    sliceed_rule_tog rules(), @(dat->flipVert), slgrUPDATESPRITE
  END SELECT
  string_array_grow_append menu(), "Visible: " & yesorno(.Visible)
  sliceed_rule_tog rules(), @.Visible
  string_array_grow_append menu(), "Fill Parent: " & yesorno(.Fill)
  sliceed_rule_tog rules(), @.Fill
  IF .Fill = NO THEN
   string_array_grow_append menu(), "Align horiz. with: " & HorizCaptions(.AlignHoriz)
   sliceed_rule rules(), erIntgrabber, @.AlignHoriz, 0, 2
   string_array_grow_append menu(), "Align vert. with: " & VertCaptions(.AlignVert)
   sliceed_rule rules(), erIntgrabber, @.AlignVert, 0, 2
   string_array_grow_append menu(), "Anchor horiz. on: " & HorizCaptions(.AnchorHoriz)
   sliceed_rule rules(), erIntgrabber, @.AnchorHoriz, 0, 2
   string_array_grow_append menu(), "Anchor vert. on: " & VertCaptions(.AnchorVert)
   sliceed_rule rules(), erIntgrabber, @.AnchorVert, 0, 2
  END IF
  string_array_grow_append menu(), "Padding Top: " & .PaddingTop
  sliceed_rule rules(), erIntgrabber, @.PaddingTop, -9999, 9999
  string_array_grow_append menu(), "Padding Right: " & .PaddingRight
  sliceed_rule rules(), erIntgrabber, @.PaddingRight, -9999, 9999
  string_array_grow_append menu(), "Padding Bottom: " & .PaddingBottom
  sliceed_rule rules(), erIntgrabber, @.PaddingBottom, -9999, 9999
  string_array_grow_append menu(), "Padding Left: " & .PaddingLeft
  sliceed_rule rules(), erIntgrabber, @.PaddingLeft, -9999, 9999
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

SUB slice_editor_refresh (BYREF state AS MenuState, menu() AS SliceEditMenuItem, edslice AS Slice Ptr, BYREF cursor_seek AS Slice Ptr)
 FOR i AS INTEGER = 0 TO UBOUND(menu)
  menu(i).s = ""
 NEXT i
 DIM index AS INTEGER = 0

 DIM indent AS INTEGER = 0
 slice_editor_refresh_append index, menu(), "Previous Menu"
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
 '--Re-adjust X/Y position for new parent
 DIM oldpos AS XYPair
 oldpos.x = sl->ScreenX
 oldpos.y = sl->ScreenY
 DIM newpos AS XYPair
 newpos.x = newparent->ScreenX + sl->X
 newpos.y = newparent->ScreenY + sl->Y
 sl->X += oldpos.x - newpos.x
 sl->Y += oldpos.y - newpos.y
END SUB

SUB DrawSliceAnts (BYVAL sl AS Slice Ptr, dpage AS INTEGER)
 STATIC ant AS INTEGER = 0
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
 ant = loopvar(ant, 0, 2, 1)
END SUB
