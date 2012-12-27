'OHRRPGCE CUSTOM - Reload Editing Tools
'(C) Copyright 2010 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'Except, this module isn't especially crappy. Yay!
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

#include "reloadedit.bi"

'-----------------------------------------------------------------------

TYPE ReloadEditorState
 indent AS INTEGER
 doc AS Reload.Docptr
 root AS Reload.Nodeptr
 mode AS INTEGER
 mode_name(1) AS STRING
 menu AS MenuDef
 state AS MenuState
 seeknode AS Reload.Nodeptr
 filename AS STRING
 clipboard AS Reload.NodePtr
 clipboard_is AS Reload.NodePtr 'only used for visual display of what you copied last
 changed AS INTEGER 'track whether or not changes that you might want to save have been made
END TYPE

'-----------------------------------------------------------------------

DECLARE SUB reload_editor_refresh (BYREF st AS ReloadEditorState, BYVAL node AS Reload.Nodeptr)
DECLARE FUNCTION reload_editor_node_string(BYREF st AS ReloadEditorState, BYVAL node AS Reload.Nodeptr) AS STRING
DECLARE FUNCTION reload_editor_browse(BYREF st AS ReloadEditorState) AS INTEGER
DECLARE SUB reload_editor_export(BYREF st AS ReloadEditorState)
DECLARE FUNCTION reload_editor_load(filename AS STRING, BYREF st AS ReloadEditorState) AS INTEGER
DECLARE SUB reload_editor_save(filename AS STRING, BYREF st AS ReloadEditorState)
DECLARE SUB reload_editor_edit_node(BYREF st AS ReloadEditorState, mi AS MenuDefItem Ptr)
DECLARE FUNCTION reload_editor_edit_node_name(BYVAL node AS Reload.Nodeptr) AS INTEGER
DECLARE FUNCTION reload_editor_edit_node_value(BYREF st AS ReloadEditorState, BYVAL node AS Reload.Nodeptr) AS INTEGER
DECLARE FUNCTION reload_editor_edit_node_type(BYVAL node AS Reload.Nodeptr) AS INTEGER
DECLARE SUB reload_editor_rearrange(BYREF st AS ReloadEditorState, mi AS MenuDefItem Ptr)
DECLARE SUB reload_editor_swap_node_left(BYVAL node AS Reload.Nodeptr)
DECLARE SUB reload_editor_swap_node_right(BYVAL node AS Reload.Nodeptr)
DECLARE SUB reload_editor_focus_node(BYREF st AS ReloadEditorState, BYVAL node AS Reload.Nodeptr)
DECLARE FUNCTION reload_editor_numeric_input_check() AS INTEGER
DECLARE FUNCTION reload_editor_okay_to_unload(BYREF st AS ReloadEditorState) AS INTEGER

'-----------------------------------------------------------------------

SUB reload_editor()
 DIM st AS ReloadEditorState
 
 st.changed = NO
 st.doc = Reload.CreateDocument()
 st.root = Reload.CreateNode(st.doc, "")
 Reload.SetRootNode(st.doc, st.root)

 st.mode_name(0) = "node values"
 st.mode_name(1) = "node names"

 st.state.pt = 0
 st.state.need_update = YES
 st.state.active = YES

 ClearMenuData st.menu
 WITH st.menu
  .anchor.x = -1
  .anchor.y = -1
  .offset.x = -160
  .offset.y = -100
  .bordersize = -4
  .align = -1
  .maxrows = 18
 END WITH
 
 setkeys YES
 DO
  setwait 55
  setkeys YES

  IF st.state.need_update THEN
   DeleteMenuItems st.menu
   st.indent = 0
   reload_editor_refresh st, st.root
   init_menu_state st.state, st.menu
   IF st.seeknode THEN
    reload_editor_focus_node st, st.seeknode
    st.seeknode = 0
   END IF
   st.state.need_update = NO
  END IF
  
  IF keyval(scESC) > 1 THEN
   IF reload_editor_okay_to_unload(st) THEN EXIT DO 
  END IF
  IF keyval(scF1) > 1 THEN show_help("reload_editor")
  IF keyval(scTAB) > 1 THEN st.mode = st.mode XOR 1
  IF keyval(scF3) > 1 THEN
   IF reload_editor_okay_to_unload(st) THEN
    IF reload_editor_browse(st) THEN
     setkeys YES
     st.state.need_update = YES
    END IF
   END IF
  END IF
  IF keyval(scF2) > 1 THEN
   reload_editor_export st
  END IF

  
  IF st.state.pt >= 0 AND st.state.pt <= st.menu.numitems - 1 THEN
   reload_editor_edit_node st, st.menu.items[st.state.pt]
   reload_editor_rearrange st, st.menu.items[st.state.pt]
  END IF
  
  IF keyval(scShift) = 0 THEN
   usemenu st.state
  END IF

  clearpage dpage
  draw_menu st.menu, st.state, dpage
  edgeprint "F1=Help TAB=Mode (" & st.mode_name(st.mode) & ") ", 0, 190, uilook(uiText), dpage

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP

 IF st.clipboard <> 0 THEN Reload.FreeNode(st.clipboard)
 Reload.FreeDocument(st.doc)
 
END SUB

SUB reload_editor_rearrange(BYREF st AS ReloadEditorState, mi AS MenuDefItem Ptr)
 DIM node AS Reload.Nodeptr
 node = mi->dataptr
 
 DIM changed AS INTEGER = NO
 
 IF keyval(scInsert) > 1 THEN
  DIM s AS STRING
  IF prompt_for_string(s, "name of new node") THEN
   DIM newnode AS Reload.Nodeptr
   newnode = Reload.CreateNode(st.doc, s)
   IF node = Reload.DocumentRoot(st.doc) THEN
    'root node can't have siblings!
    Reload.AddChild node, newnode
   ELSE
    Reload.AddSiblingAfter node, newnode
   END IF
   st.seeknode = newnode
   changed = YES
  END IF
 END IF
 
 IF keyval(scShift) > 0 THEN
  IF copy_keychord() THEN
   '--copy this node
   IF st.clipboard <> 0 THEN Reload.FreeNode(st.clipboard)
   st.clipboard = Reload.CloneNodeTree(node)
   st.clipboard_is = node
   changed = YES
  END IF
  IF paste_keychord() THEN
   '--paste this node
   IF st.clipboard <> 0 THEN
    Reload.AddSiblingAfter(node, Reload.CloneNodeTree(st.clipboard))
    IF Reload.NodeHasAncestor(node, st.clipboard_is) THEN st.clipboard_is = 0 'cosmetic importance only
    changed = YES
   END IF
  END IF
 END IF
 
 IF keyval(scShift) > 0 THEN
  IF keyval(scUP) > 1 THEN
   Reload.SwapNodePrev node
   st.seeknode = node
   changed = YES
  END IF
  IF keyval(scDOWN) > 1 THEN
   Reload.SwapNodeNext node
   st.seeknode = node
   changed = YES
  END IF
  IF keyval(scLEFT) > 1 THEN
   reload_editor_swap_node_left node
   st.seeknode = node
   changed = YES
  END IF
  IF keyval(scRIGHT) > 1 THEN
   reload_editor_swap_node_right node
   st.seeknode = node
   changed = YES
  END IF
 END IF
 
 IF keyval(scDelete) > 1 THEN
  IF node <> Reload.DocumentRoot(st.doc) THEN
   IF yesno("Delete this node?" & CHR(10) & reload_editor_node_string(st, node)) THEN
    Reload.FreeNode(node)
    changed = YES
   END IF
  END IF
 END IF
 
 IF changed THEN
  st.state.need_update = YES
  st.changed = YES
 END IF
END SUB

SUB reload_editor_swap_node_left(BYVAL node AS Reload.Nodeptr)
 IF node = 0 THEN EXIT SUB
 DIM parent AS Reload.NodePtr
 parent = Reload.NodeParent(node)
 IF parent = 0 THEN EXIT SUB
 Reload.AddSiblingAfter(parent, node)
END SUB

SUB reload_editor_swap_node_right(BYVAL node AS Reload.Nodeptr)
 IF node = 0 THEN EXIT SUB
 DIM sib AS Reload.NodePtr
 sib = Reload.PrevSibling(node)
 IF sib = 0 THEN EXIT SUB
 Reload.AddChild(sib, node)
END SUB

SUB reload_editor_edit_node(BYREF st AS ReloadEditorState, mi AS MenuDefItem Ptr)
 IF mi = 0 THEN debug "reload_editor_edit_node: null mi": EXIT SUB
 DIM node AS Reload.NodePtr
 node = mi->dataptr
 IF node = 0 THEN debug "reload_editor_edit_node: mi has null node": EXIT SUB

 DIM changed AS INTEGER = NO

 IF keyval(scCTRL) > 0 AND keyval(scShift) > 0 THEN EXIT SUB 'no typing while holding ctrl+shift!
  
 SELECT CASE st.mode
  CASE 0:
   IF reload_editor_edit_node_value(st, node) THEN changed = YES
  CASE 1:
   IF reload_editor_edit_node_name(node) THEN changed = YES
 END SELECT
 IF reload_editor_edit_node_type(node) THEN changed = YES

 IF changed THEN
  mi->caption = STRING(mi->extra(0), " ") & reload_editor_node_string(st, node)
  st.changed = YES
 END IF

END SUB

FUNCTION reload_editor_edit_node_name(BYVAL node AS Reload.Nodeptr) AS INTEGER
 IF node = 0 THEN debug "reload_editor_edit_node_name: null node": RETURN NO
 DIM s AS STRING
 s = Reload.NodeName(node)
 IF strgrabber(s, 40) THEN
  Reload.RenameNode(node, s)
  RETURN YES
 END IF
 RETURN NO
END FUNCTION

FUNCTION reload_editor_edit_node_value(BYREF st AS ReloadEditorState, BYVAL node AS Reload.Nodeptr) AS INTEGER
 IF node = 0 THEN debug "reload_editor_edit_node_value: null node": RETURN NO

 DIM nt AS Reload.NodeTypes = Reload.NodeType(node)
  
 IF nt = Reload.rltFloat THEN
  'debug "no floatgrabber exists yet"
 ELSEIF nt = Reload.rltInt ORELSE (nt = Reload.rltNull ANDALSO reload_editor_numeric_input_check()) THEN
  IF keyval(scShift) = 0 THEN
   DIM n AS INTEGER
   n = Reload.GetInteger(node)
   IF intgrabber(n, -2147483648, 2147483647) THEN
    Reload.SetContent(node, n)
    RETURN YES
   END IF
  END IF
  IF keyval(scBackspace) > 1 THEN
   IF Reload.GetInteger(node) = 0 THEN
    Reload.SetContent(node)
    RETURN YES
   END IF
  END IF
 ELSEIF nt = Reload.rltString ORELSE nt = Reload.rltNull THEN
  DIM s AS STRING
  s = Reload.GetString(node)
  IF nt <> Reload.rltNull AND keyval(scENTER) > 1 THEN
   s = multiline_string_editor(s, "reload_editor_multiline")
   Reload.SetContent(node, s)
   RETURN YES
  ELSE
   IF strgrabber(s, 40) THEN
    Reload.SetContent(node, s)
    RETURN YES
   END IF
  END IF
  IF keyval(scBackspace) > 1 THEN
   IF Reload.GetString(node) = "" THEN
    Reload.SetContent(node)
    RETURN YES
   END IF
  END IF
 ELSEIF nt <> Reload.rltNull THEN
  debug "invalid reload node type " & nt
 END IF
 RETURN NO
END FUNCTION

FUNCTION reload_editor_edit_node_type(BYVAL node AS Reload.Nodeptr) AS INTEGER
 IF node = 0 THEN debug "reload_editor_edit_node_type: null node" : RETURN NO
 IF keyval(scCTRL) > 0 THEN
  IF keyval(scI) > 1 THEN Reload.SetContent(node, Reload.GetInteger(node)) : RETURN YES
  IF keyval(scS) > 1 THEN Reload.SetContent(node, Reload.GetString(node)) : RETURN YES
  IF keyval(scF) > 1 THEN Reload.SetContent(node, Reload.GetFloat(node)) : RETURN YES
  IF keyval(scN) > 1 THEN Reload.SetContent(node) : RETURN YES
 END IF
 RETURN NO
END FUNCTION

SUB reload_editor_refresh (BYREF st AS ReloadEditorState, BYVAL node AS Reload.Nodeptr)
 IF node = 0 THEN EXIT SUB

 DIM s AS STRING
 s = STRING(st.indent, " ") & reload_editor_node_string(st, node)
 
 DIM index AS INTEGER
 index = append_menu_item(st.menu, s)
 
 DIM mi AS MenuDefItem Ptr
 mi = st.menu.items[index]

 mi->dataptr = node
 mi->extra(0) = st.indent

 st.indent += 1 
 DIM chnode AS Reload.Nodeptr
 chnode = Reload.FirstChild(node)
 DO WHILE chnode
  reload_editor_refresh st, chnode
  chnode = Reload.NextSibling(chnode)
 LOOP
 st.indent -= 1
END SUB

FUNCTION reload_editor_node_string(BYREF st AS ReloadEditorState ,BYVAL node AS Reload.Nodeptr) AS STRING
 IF node = 0 THEN debug "reload_editor_node_str: null node" : RETURN "<null ptr>"
 DIM s AS STRING = ""
 if node = st.clipboard_is OR Reload.NodeHasAncestor(node, st.clipboard_is) then s &= "*"
 s &= Reload.NodeName(node)
 SELECT CASE Reload.NodeType(node)
  CASE Reload.rltNull:   s &= "()"
  CASE Reload.rltInt:    s &= "(int) "    & Reload.GetInteger(node)
  CASE Reload.rltFloat:  s &= "(float) "  & Reload.GetFloat(node)
  CASE Reload.rltString: s &= "(str) " & Reload.GetString(node)
 END SELECT
 RETURN s
END FUNCTION

SUB reload_editor_focus_node(BYREF st AS ReloadEditorState, BYVAL node AS Reload.Nodeptr)
 DIM mi AS MenuDefItem Ptr
 DIM n AS Reload.Nodeptr
 FOR i AS INTEGER = 0 TO st.menu.numitems - 1
  mi = st.menu.items[i]
  n = mi->dataptr
  IF n = node THEN
   st.state.pt = i
   EXIT FOR
  END IF
 NEXT i
 WITH st.state
  .pt = small(.pt, .last)
  .top = bound(.top, .pt - .size, .pt)
 END WITH
END SUB

SUB reload_editor_export(BYREF st AS ReloadEditorState)
 DIM outfile AS STRING
 outfile = inputfilename("Export RELOAD document", "", "", "input_file_export_reload", st.filename)
 IF outfile <> "" THEN
  IF INSTR(outfile, ".") = 0 THEN outfile &= ".reld"
  reload_editor_save outfile, st
 END IF
END SUB

FUNCTION reload_editor_browse(BYREF st AS ReloadEditorState) AS INTEGER
 DIM filename AS STRING
 filename = browse(8, "", "", "",, "browse_import_reload")
 IF filename = "" THEN RETURN NO
 RETURN reload_editor_load(filename, st)
END FUNCTION

FUNCTION reload_editor_load(filename AS STRING, BYREF st AS ReloadEditorState) AS INTEGER
 st.filename = ""
 Reload.FreeDocument st.doc
 st.doc = Reload.LoadDocument(filename, optNoDelay)
 IF st.doc = 0 THEN debug "load '" & filename & "' failed: null doc": RETURN NO
 st.root = Reload.DocumentRoot(st.doc)
 IF st.root = 0 THEN debug "load '" & filename & "' failed: null root node": RETURN NO
 st.filename = trimpath(filename)
 st.changed = NO
 RETURN YES
END FUNCTION

SUB reload_editor_save(filename AS STRING, BYREF st AS ReloadEditorState)
 Reload.SerializeBin(filename, st.doc)
 st.filename = trimpath(filename)
 st.changed = NO
END SUB

FUNCTION reload_editor_numeric_input_check() AS INTEGER
 IF keyval(scShift) > 0 THEN RETURN NO
 FOR i AS INTEGER = sc1 TO sc0
  IF keyval(i) > 1 THEN RETURN YES
 NEXT i
 IF keyval(scMinus) > 1 OR keyval(scNumpadMinus) > 1 THEN RETURN YES
 RETURN NO
END FUNCTION

FUNCTION reload_editor_okay_to_unload(BYREF st AS ReloadEditorState) AS INTEGER
 IF st.changed = NO THEN RETURN YES
 DIM choice AS INTEGER
 'Prevent attempt to quit the program, stop and wait for response first
 DIM quitting as integer = keyval(-1)
 clearkey(-1)
 choice = twochoice("Save your changes before exiting?", "Yes, save", "No, discard")
 IF keyval(-1) THEN choice = 1  'Second attempt to close the program: discard
 SELECT CASE choice
  CASE -1: 'cancelled
   RETURN NO
  CASE 0: 'yes, save!
   reload_editor_export st
   'but only actually allow unload if the save was confirmed
   IF st.changed = NO THEN
    IF quitting THEN setquitflag
    RETURN YES
   END IF
   RETURN NO
  CASE 1: 'no discard!
   IF quitting THEN setquitflag
   RETURN YES
 END SELECT
 RETURN NO
END FUNCTION
