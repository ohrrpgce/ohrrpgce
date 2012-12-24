'OHRRPGCE CUSTOM - Editor Editor
'(C) Copyright 2010 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'Except, this module APOLOGISES FOR NOTHING!
'

#ifdef LANG_DEPRECATED
 #define __langtok #lang
 __langtok "deprecated"
 OPTION STATIC
 OPTION EXPLICIT
#endif

#include "config.bi"
#include "allmodex.bi"
#include "common.bi"
#include "slices.bi"
#include "customsubs.bi"
#include "loading.bi"
#include "reload.bi"
#include "reloadext.bi"

#include "editrunner.bi"

'just for debugging
#include "sliceedit.bi"

USING Reload
USING Reload.Ext

'-----------------------------------------------------------------------

'NOTE: these ought to be the same as the lookup codes defined
'in the widgets/widgets.rpg file, but that is just a convenience file
'and we don't currently enforce any such synchronization.
CONST eeslCaption = 1
CONST eeslValue = 2
CONST eeslPreview = 3
CONST eeslCondition = 4
CONST eeslCheck = 5

'-----------------------------------------------------------------------

TYPE EditorState
 root AS NodePtr
 need_update AS INTEGER
 root_sl AS Slice Ptr
 scroller AS Slice Ptr
 widget_state_doc AS DocPtr
 widget_state AS NodePtr
END TYPE

'-----------------------------------------------------------------------

DECLARE SUB edrun_init_sl (BYREF es AS EditorState)
DECLARE SUB edrun_update (BYREF es AS EditorState)
DECLARE FUNCTION edrun_create_widget_slice(BYVAL widget AS NodePtr) AS Slice Ptr
DECLARE SUB edrun_position_new_widget(BYREF es AS EditorState, BYVAL sl AS Slice Ptr)
DECLARE SUB edrun_populate_new_widget(BYVAL widget AS NodePtr, BYVAL sl AS Slice Ptr)

'-----------------------------------------------------------------------

SUB editor_runner(editor_definition_file AS STRING)
 IF NOT isfile(editor_definition_file) THEN
  pop_warning "file not found: " & editor_definition_file
  EXIT SUB
 END IF
 
 DIM doc AS DocPtr
 doc = LoadDocument(editor_definition_file, optNoDelay)
 
 DIM root AS NodePtr
 
 root = DocumentRoot(doc)
 editor_runner root
 FreeDocument(doc)
END SUB

SUB editor_runner(BYVAL root AS NodePtr)
 IF root = 0 THEN
  pop_warning "null root editor node"
  EXIT SUB
 END IF

 IF NodeName(root) <> "editor" THEN
  pop_warning "not an editor root node (" & NodeName(root) & ")"
  EXIT SUB
 END IF

 DIM es AS EditorState
 es.root = root
 es.need_update = YES
 es.widget_state_doc = CreateDocument()
 es.widget_state = DocumentRoot(es.widget_state_doc)
 
 edrun_init_sl es
 
 setkeys
 DO
  setwait 55
  setkeys

  IF es.need_update THEN
   edrun_update es
   es.need_update = NO
  END IF

  IF keyval(scESC) > 1 THEN EXIT DO
  
  IF keyval(scF6) THEN slice_editor es.root_sl

  clearpage dpage

  DrawSlice es.root_sl, dpage

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 
 FreeDocument es.widget_state_doc
 
END SUB

'-----------------------------------------------------------------------

SUB edrun_init_sl (BYREF es AS EditorState)
 DeleteSlice @es.root_sl
 es.scroller = 0
 FreeNode es.widget_state
 
 es.root_sl = NewSliceOfType(slContainer)
 es.root_sl->Fill = YES
 
 es.scroller = NewSliceOfType(slContainer, es.root_sl)
END SUB

SUB edrun_update (BYREF es AS EditorState)

 DIM widget_container AS NodePtr
 widget_container = NodeByPath(es.root, "/widgets")
 IF widget_container = 0 THEN
  pop_warning("Editor definition has no widget container node")
  EXIT SUB
 END IF

 DIM widget AS Nodeptr
 widget = FirstChild(widget_container, "widget")
 
 DIM sl AS Slice ptr
 DO WHILE widget
  sl = edrun_create_widget_slice(widget)
  debug "WIDGET:" & GetString(widget) & " " & GetChildNodeStr(widget, "caption")
  IF sl THEN
   edrun_position_new_widget(es, sl)
   edrun_populate_new_widget(widget, sl)
  END IF
  widget = NextSibling(widget, "widget")
 LOOP
 
END SUB

'-----------------------------------------------------------------------

FUNCTION edrun_create_widget_slice(BYVAL widget AS NodePtr) AS Slice Ptr
 IF widget = 0 THEN
  pop_warning("can't create slice for null widget!")
  RETURN 0
 END IF
 DIM sl AS Slice Ptr
 sl = NewSliceOfType(slSpecial)
 DIM kind AS STRING
 kind = GetString(widget)
 DIM dirname AS STRING
 dirname = finddatadir("widgets")
 IF dirname = "" THEN
  pop_warning("Can't find widget data dir!")
  RETURN 0
 END IF
 DIM filename AS STRING
 filename =  dirname & SLASH & kind & ".widget.slice"
 IF isfile(filename) THEN
  SliceLoadFromFile sl, filename
  IF sl THEN
  ELSE
   debuginfo "edrun_create_widget_slice: slice load failed"
  END IF
 ELSE
  debuginfo "edrun_create_widget_slice: no widget file for " & kind
 END IF
 RETURN sl
END FUNCTION

'-----------------------------------------------------------------------

SUB edrun_position_new_widget(BYREF es AS EditorState, BYVAL sl AS Slice Ptr)
 IF sl = 0 THEN pop_warning "edrun_position_new_widget: null sl": EXIT SUB

 sl->Fill = NO
 sl->Width = 0
 sl->Height = 0

 DIM after AS Slice Ptr
 after = LastChild(es.scroller)
 
 SetSliceParent sl, es.scroller
 IF after = 0 THEN
  'First widget, nothing to see here, move along folks!
  EXIT SUB
 END IF

 DIM capsl AS Slice Ptr
 capsl = LookupSlice(eeslCaption, after)
 
 IF capsl = 0 THEN
  debuginfo "edrun_position_new_widget: sanity fail. no caption slice found": EXIT SUB
 END IF

 sl->y = after->y + SliceEdgeY(capsl, 2)
 debug "sl->y = " & sl->y
 

END SUB

SUB edrun_populate_new_widget(BYVAL widget AS NodePtr, BYVAL sl AS Slice Ptr)
 DIM capsl AS Slice Ptr
 capsl = LookupSlice(eeslCaption, sl)
 DIM s AS STRING
 s = GetChildNodeStr(widget, "caption")
 debug "s=" & s
 ChangeTextSlice capsl, s
END SUB

