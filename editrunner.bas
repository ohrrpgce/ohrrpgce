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
END TYPE

'-----------------------------------------------------------------------

SUB editor_runner(editor_definition_file AS STRING)
 IF NOT isfile(editor_definition_file) THEN
  debuginfo "file not found: " & editor_definition_file
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
  debuginfo "null root editor node"
  EXIT SUB
 END IF

 IF NodeName(root) <> "editor" THEN
  debuginfo "not an editor root node (" & NodeName(root) & ")"
  EXIT SUB
 END IF

 DIM es AS EditorState
 es.root = root
 
 setkeys
 DO
  setwait 55
  setkeys

  IF keyval(scESC) > 1 THEN EXIT DO

  clearpage dpage

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 
END SUB

'-----------------------------------------------------------------------
