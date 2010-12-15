'OHRRPGCE CUSTOM - Editor Editor
'(C) Copyright 2010 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'Except, this module APOLOGISES FOR NOTHING!
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
#include "customsubs.bi"
#include "loading.bi"
#include "reload.bi"
#include "reloadext.bi"

#include "editrunner.bi"

USING Reload
USING Reload.Ext

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

DECLARE SUB edrun_update (BYREF es AS EditorState)

'-----------------------------------------------------------------------

SUB editor_runner(editor_definition_file AS STRING)
 IF NOT isfile(editor_definition_file) THEN
  pop_warning "file not found: " & editor_definition_file
  EXIT SUB
 END IF
 
 DIM doc AS DocPtr
 doc = LoadDocument(editor_definition_file)
 
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
 
 setkeys
 DO
  setwait 55
  setkeys

  IF es.need_update THEN
   edrun_update es
   es.need_update = NO
  END IF

  IF keyval(scESC) > 1 THEN EXIT DO

  clearpage dpage

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 
 FreeDocument es.widget_state_doc
 
END SUB

'-----------------------------------------------------------------------

SUB edrun_update (BYREF es AS EditorState)
 DeleteSlice @es.root_sl
 es.scroller = 0
 FreeNode es.widget_state
 
 es.root_sl = NewSliceOfType(slContainer)
 es.root_sl->Fill = YES
 
 es.scroller = NewSliceOfType(slContainer, es.root_sl)
 
 DIM widget_container AS NodePtr
 widget_container = NodeByPath(es.root, "/widgets")
 IF widget_container = 0 THEN
  pop_warning("Editor definition has no widget container node")
  EXIT SUB
 END IF
 
 DIM widget AS Nodeptr
 widget = FirstChild(widget_container, "widget")
 
 DO WHILE widget
  debug NodeName(widget)
  widget = NextSibling(widget, "widget")
 LOOP
 
END SUB

'-----------------------------------------------------------------------
