'OHRRPGCE CUSTOM - Reload Editing Tools
'(C) Copyright 2010 James Paige and Hamster Republic Productions
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
#include "customsubs.bi"
#include "loading.bi"
#include "reload.bi"

#include "reloadedit.bi"
'-----------------------------------------------------------------------

DECLARE SUB reload_editor_refresh (node AS Reload.Nodeptr, BYREF Menu AS MenuDef)
DECLARE FUNCTION reload_editor_node_string(node AS Reload.Nodeptr) AS STRING
DECLARE FUNCTION reload_editor_browse(BYREF doc AS Reload.Docptr, BYREF node AS Reload.NodePtr) AS INTEGER
DECLARE FUNCTION reload_editor_load(filename AS STRING, BYREF doc AS Reload.Docptr, BYREF node AS Reload.NodePtr) AS INTEGER
DECLARE SUB reload_editor_edit_node(mode AS INTEGER, mi AS MenuDefItem Ptr)
DECLARE FUNCTION reload_editor_edit_node_name(node AS Reload.Nodeptr) AS INTEGER

'-----------------------------------------------------------------------

DIM SHARED indent AS INTEGER

SUB reload_editor()
 indent = 0
 
 DIM doc AS Reload.Docptr
 doc = Reload.CreateDocument()
 DIM node AS Reload.Nodeptr
 Reload.SetRootNode(doc, node)

 DIM mode AS INTEGER = 0
 DIM mode_name(1) AS STRING
 mode_name(0) = "node names"
 mode_name(1) = "node values"

 DIM state AS MenuState
 state.pt = 0
 state.need_update = YES
 state.active = YES
 
 DIM menu AS MenuDef
 ClearMenuData menu
 menu.anchor.x = -1
 menu.anchor.y = -1
 menu.offset.x = -160
 menu.offset.y = -100
 menu.bordersize = -4
 menu.align = -1
 menu.maxrows = 18
 
 setkeys
 DO
  setwait 55
  setkeys

  IF state.need_update THEN
   DeleteMenuItems menu
   indent = 0
   reload_editor_refresh node, menu
   init_menu_state state, menu
   state.need_update = NO
  END IF
  
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scTAB) > 1 THEN mode = mode XOR 1
  IF keyval(scCTRL) > 0 THEN
   IF keyval(scR) > 1 THEN 
    IF reload_editor_browse(doc, node) THEN
     state.need_update = YES
    END IF
   END IF
  END IF
  
  IF state.pt >= 0 AND state.pt <= menu.numitems - 1 THEN
   reload_editor_edit_node (mode, menu.items[state.pt])
  END IF
  
  usemenu state
  
  clearpage dpage
  draw_menu menu, state, dpage
  edgeprint "F1=Help TAB=Mode (" & mode_name(mode) & ")", 0, 190, uilook(uiText), dpage

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP

 DeleteMenuItems menu
 Reload.FreeDocument(doc)
 
END SUB

SUB reload_editor_edit_node(mode AS INTEGER, mi AS MenuDefItem Ptr)
 IF mi = 0 THEN debug "reload_editor_edit_node: null mi": EXIT SUB
 DIM node AS Reload.NodePtr
 node = mi->dataptr
 IF node = 0 THEN debug "reload_editor_edit_node: mi has null node": EXIT SUB
 
 SELECT CASE mode
  CASE 0:
   IF reload_editor_edit_node_name(node) THEN
    mi->caption = STRING(mi->extra(0), " ") & reload_editor_node_string(node)
   END IF
  CASE 1:
 END SELECT
END SUB

FUNCTION reload_editor_edit_node_name(node AS Reload.Nodeptr) AS INTEGER
 IF node = 0 THEN debug "reload_editor_edit_node_name: null node": RETURN NO
 DIM s AS STRING
 s = Reload.NodeName(node)
 IF strgrabber(s, 40) THEN
  Reload.RenameNode(node, s)
  RETURN YES
 END IF
 RETURN NO
END FUNCTION

SUB reload_editor_refresh (node AS Reload.Nodeptr, BYREF Menu AS MenuDef)
 IF node = 0 THEN debug "reload_editor_refresh: null node" : EXIT SUB

 DIM s AS STRING
 s = STRING(indent, " ") & reload_editor_node_string(node)
 debug s
 
 DIM index AS INTEGER
 index = append_menu_item(menu, s)
 
 DIM mi AS MenuDefItem Ptr
 mi = menu.items[index]

 mi->dataptr = node
 mi->extra(0) = indent

 indent += 1 
 DIM chnode AS Reload.Nodeptr
 chnode = Reload.FirstChild(node)
 DO WHILE chnode
  reload_editor_refresh chnode, menu
  chnode = Reload.NextSibling(chnode)
 LOOP
 indent -= 1
END SUB

FUNCTION reload_editor_node_string(node AS Reload.Nodeptr) AS STRING
 IF node = 0 THEN debug "reload_editor_node_str: null node" : RETURN "<null ptr>"
 DIM s AS STRING = ""
 s &= Reload.NodeName(node)
 SELECT CASE Reload.NodeType(node)
  CASE Reload.rliNull:   s &= "()"
  CASE Reload.rliByte:   s &= "(byte) "   & Reload.GetInteger(node)
  CASE Reload.rliShort:  s &= "(short) "  & Reload.GetInteger(node)
  CASE Reload.rliInt:    s &= "(int) "    & Reload.GetInteger(node)
  CASE Reload.rliLong:   s &= "(long) "   & Reload.GetInteger(node)
  CASE Reload.rliFloat:  s &= "(float) "  & Reload.GetFloat(node)
  CASE Reload.rliString: s &= "(string) " & Reload.GetString(node)
 END SELECT
 RETURN s
END FUNCTION

FUNCTION reload_editor_browse(BYREF doc AS Reload.Docptr, BYREF node AS Reload.NodePtr) AS INTEGER
 DIM filename AS STRING
 filename = browse(0, "", "*.reld", "",, "browse_import_reload")
 RETURN reload_editor_load(filename, doc, node)
END FUNCTION

FUNCTION reload_editor_load(filename AS STRING, BYREF doc AS Reload.Docptr, BYREF node AS Reload.NodePtr) AS INTEGER
 Reload.FreeDocument doc
 doc = Reload.LoadDocument(filename)
 IF doc = 0 THEN debug "load '" & filename & "' failed: null doc": RETURN NO
 node = Reload.DocumentRoot(doc)
 IF node = 0 THEN debug "load '" & filename & "' failed: null root node": RETURN NO
 RETURN YES
END FUNCTION
