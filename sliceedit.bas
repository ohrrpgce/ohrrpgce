'OHRRPGCE CUSTOM - Slice Collection Editor
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
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

'==============================================================================

TYPE SliceEditMenuItem
 s AS STRING
 handle AS Slice Ptr
END TYPE

'==============================================================================

'Public functions (will put these in a bi file if there turns out to be more than 1)
DECLARE SUB slice_editor ()

'Functions that might go better in slices.bas ... we shall see
DECLARE FUNCTION SliceTypeName (sl AS Slice Ptr) AS STRING
DECLARE FUNCTION SlicePositionString (sl AS Slice Ptr) AS STRING

'Functions only used locally
DECLARE SUB slice_editor_refresh (BYREF state AS MenuState, menu() AS SliceEditMenuItem, edslice AS Slice Ptr)
DECLARE SUB slice_editor_refresh_append (BYREF index AS INTEGER, menu() AS SliceEditMenuItem, caption AS STRING, sl AS Slice Ptr=0)
DECLARE SUB slice_editor_refresh_recurse (BYREF index AS INTEGER, menu() AS SliceEditMenuItem, BYREF indent AS INTEGER, sl AS Slice Ptr)
DECLARE SUB slice_edit_detail (sl AS Slice Ptr, rootsl AS Slice Ptr)
DECLARE SUB slice_edit_detail_refresh (BYREF state AS MenuState, menu() AS STRING, sl AS Slice Ptr)
DECLARE SUB slice_edit_detail_keys (BYREF state AS MenuState, sl AS Slice Ptr)
DECLARE FUNCTION slice_type_as_number (slice_type AS SliceTypes) AS INTEGER
DECLARE FUNCTION new_slice_by_number (slice_type_number AS INTEGER) AS Slice Ptr

'==============================================================================

SUB slice_editor ()

 DIM edslice AS Slice Ptr
 edslice = NewSlice
 WITH *edslice
  .Attach = slScreen
  .SliceType = slRoot
 END WITH
 NewSlice(edslice)
 NewSlice(edslice)

 DIM menu(0) AS SliceEditMenuItem
 DIM plainmenu(0) AS STRING 'FIXME: This is a hack because I didn't want to re-implement standardmenu right now
 
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
   slice_editor_refresh(state, menu(), edslice)
   REDIM plainmenu(state.last) AS STRING
   FOR i AS INTEGER = 0 TO UBOUND(plainmenu)
    plainmenu(i) = menu(i).s
   NEXT i
   state.need_update = NO
  END IF

  usemenu state
  IF enter_or_space() THEN
   IF state.pt = 0 THEN
    EXIT DO
   ELSE
    slice_edit_detail menu(state.pt).handle, edslice
    state.need_update = YES
   END IF 
  END IF

  DrawSlice edslice, dpage
  standardmenu plainmenu(), state, 0, 0, dpage, YES

  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP

END SUB

SUB slice_edit_detail (sl AS Slice Ptr, rootsl AS Slice Ptr)

 DIM menu(0) AS STRING

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
   slice_edit_detail_refresh state, menu(), sl
  END IF

  usemenu state
  slice_edit_detail_keys state, sl
  
  DrawSlice rootsl, dpage
  standardmenu menu(), state, 0, 0, dpage

  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
END SUB

SUB slice_edit_detail_keys (BYREF state AS MenuState, sl AS Slice Ptr)
 WITH *sl
  SELECT CASE state.pt
   CASE 1:
    DIM slice_type AS INTEGER = slice_type_as_number(.SliceType)
    IF intgrabber(slice_type, 0, 7) THEN
     ReplaceSlice sl, new_slice_by_number(slice_type)
     state.need_update = YES
    END IF
   CASE 2: IF intgrabber(.X, -9999, 9999) THEN state.need_update = YES
   CASE 3: IF intgrabber(.Y, -9999, 9999) THEN state.need_update = YES
   CASE 4: IF intgrabber(.Width, 0, 9999) THEN state.need_update = YES
   CASE 5: IF intgrabber(.Height, 0, 9999) THEN state.need_update = YES
   CASE 6: IF intgrabber(.Visible, -1, 0) THEN state.need_update = YES
           IF enter_or_space() THEN .Visible = NOT .Visible : state.need_update = YES
  END SELECT
 END WITH
END SUB

SUB slice_edit_detail_refresh (BYREF state AS MenuState, menu() AS STRING, sl AS Slice Ptr)
 REDIM menu(6)
 menu(0) = "Previous Menu"
 WITH *sl
  menu(1) = "Slice type: " & SliceTypeName(sl)
  menu(2) = "X: " & .X
  menu(3) = "Y: " & .Y
  menu(4) = "Width: " & .Width
  menu(5) = "Height: " & .Height
  menu(6) = "Visible: " & yesorno(.Visible)
 END WITH
 state.last = UBOUND(menu)
 state.pt = small(state.pt, state.last)
 state.top = small(state.top, state.pt)
END SUB

FUNCTION SliceTypeName (sl AS Slice Ptr) AS STRING
 SELECT CASE sl->SliceType
  CASE slRoot:           RETURN "Root"
  CASE slSpecial:        RETURN "Special"
  CASE slRectangle:      RETURN "Rectangle"
  CASE slStyleRectangle: RETURN "Styled Rect"
  CASE slSprite:         RETURN "Sprite"
  CASE slText:           RETURN "Text"
  CASE slMenu:           RETURN "Menu"
  CASE slMenuItem:       RETURN "MenuItem"
 END SELECT
 RETURN "Unknown"
END FUNCTION

FUNCTION SlicePositionString (sl AS Slice Ptr) AS STRING
 'This shows the absolute screen position of a slice.
 WITH *sl
  RETURN .ScreenX & "," & .ScreenY & "(" & .Width & "x" & .Height & ")"
 END WITH
END FUNCTION

SUB slice_editor_refresh (BYREF state AS MenuState, menu() AS SliceEditMenuItem, edslice AS Slice Ptr)
 FOR i AS INTEGER = 0 TO UBOUND(menu)
  menu(i).s = ""
 NEXT i

 DIM index AS INTEGER = 0
 DIM indent AS INTEGER = 0
 slice_editor_refresh_append index, menu(), "Previous Menu"
 slice_editor_refresh_recurse index, menu(), indent, edslice
 WITH state
  .last = index - 1
  .pt = small(.pt, .last)
  .top = small(.top, .pt)
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

FUNCTION slice_type_as_number (slice_type AS SliceTypes) AS INTEGER
 SELECT CASE slice_type
  CASE slRoot:           RETURN 0
  CASE slSpecial:        RETURN 1
  CASE slRectangle:      RETURN 2
  CASE slStyleRectangle: RETURN 3
  CASE slSprite:         RETURN 4
  CASE slText:           RETURN 5
  CASE slMenu:           RETURN 6
  CASE slMenuItem:       RETURN 7
 END SELECT
END FUNCTION

FUNCTION new_slice_by_number (slice_type_number AS INTEGER) AS Slice Ptr
 SELECT CASE slice_type_number
  CASE 2: DIM dat AS RectangleSliceData
          RETURN NewRectangleSlice(0, dat)
  CASE 3: DIM dat AS StyleRectangleSliceData
          RETURN NewStyleRectangleSlice(0, dat)
  'CASE 4: DIM dat AS SpriteSliceData
  '        RETURN NewSpriteSlice(0, dat)
  CASE 5: DIM dat AS TextSliceData
          RETURN NewTextSlice(0, dat)
  CASE 6: DIM dat AS MenuSliceData
          RETURN NewMenuSlice(0, dat)
  CASE 7: DIM dat AS MenuItemSliceData
          RETURN NewMenuItemSlice(0, dat)
  CASE ELSE: RETURN NewSlice()
 END SELECT
END FUNCTION
