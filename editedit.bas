'OHRRPGCE CUSTOM - Editor Editor
'(C) Copyright 2010 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'Except, this module APOLOGISES FOR NOTHING!
'

#include "config.bi"
#include "allmodex.bi"
#include "common.bi"
#include "slices.bi"
#include "customsubs.bi"
#include "loading.bi"
#include "reload.bi"
#include "reloadext.bi"
#include "editrunner.bi"

#include "editedit.bi"

USING Reload
USING Reload.Ext

'-----------------------------------------------------------------------

TYPE EEState
 state as MenuState
 menu as MenuDef
 indent as integer
 doc as DocPtr
 root as NodePtr
 seek_widget as NodePtr
 clipboard as NodePtr
 clipboard_is as NodePtr
 filename as string
 changed as bool
END TYPE

TYPE WEStateF as WEState
TYPE WidgetRefreshSub as SUB(byref st as WEStateF, byval widget as Nodeptr)

TYPE WidgetCode
 refresh_callback as WidgetRefreshSub
END TYPE

TYPE WEState
 state as MenuState
 menu as MenuDef
 code as WidgetCode
 changed as bool
 exit_please as bool
END TYPE

CONST wedEXIT = -1
CONST wedSTRING = -2
CONST wedINT = -3
CONST wedBOOL = -4

'-----------------------------------------------------------------------

DECLARE SUB ee_create_new_editor_file(byref st as EEState)
DECLARE SUB ee_refresh OVERLOAD (byref st as EEState)
DECLARE SUB ee_refresh OVERLOAD (byref st as EEState, byval widget as NodePtr)
DECLARE FUNCTION ee_widget_string(byref st as EEState, byval widget as Nodeptr) as string
DECLARE SUB ee_focus_widget(byref st as EEState, byval widget as Nodeptr)
DECLARE SUB ee_export(byref st as EEState)
DECLARE FUNCTION ee_browse(byref st as EEState) as integer
DECLARE FUNCTION ee_load(filename as string, byref st as EEState) as integer
DECLARE SUB ee_save(filename as string, byref st as EEState)
DECLARE FUNCTION ee_okay_to_unload(byref st as EEState) as integer
DECLARE SUB ee_insertion(byref st as EEState, byval widget as Nodeptr)
DECLARE SUB ee_rearrange(byref st as EEState, mi as MenuDefItem Ptr)
DECLARE SUB ee_swap_widget_up(byval widget as Nodeptr)
DECLARE SUB ee_swap_widget_down(byval widget as Nodeptr)
DECLARE SUB ee_swap_widget_left(byval widget as Nodeptr)
DECLARE SUB ee_swap_widget_right(byval widget as Nodeptr)
DECLARE SUB ee_edit_menu_item(byref st as EEState, mi as MenuDefItem Ptr)
DECLARE FUNCTION ee_edit_widget(byref st as EEState, byval widget as NodePtr) as integer

DECLARE FUNCTION ee_prompt_for_widget_kind() as string
DECLARE FUNCTION ee_create_widget(byref st as EEState, kind as string) as NodePtr
DECLARE FUNCTION ee_container_check(byval cont as NodePtr) as bool
DECLARE FUNCTION ee_container_single_check(byval cont as NodePtr) as bool
DECLARE FUNCTION ee_count_children(byval cont as NodePtr) as integer
DECLARE FUNCTION ee_container_is_full(byval cont as NodePtr) as bool
DECLARE FUNCTION ee_parent_is_full(byval cont as NodePtr) as bool
DECLARE FUNCTION ee_widget_has_caption(byval widget as NodePtr) as integer
DECLARE FUNCTION ee_widget_has_data(byval widget as NodePtr) as integer

DECLARE FUNCTION widget_editor(byval widget as NodePtr) as integer
DECLARE SUB widget_editor_refresh(byref st as WEState, byval widget as NodePtr)
DECLARE SUB widget_editor_edit_menu_item(byref st as WEState, mi as MenuDefItem Ptr)
DECLARE FUNCTION widget_editor_edit_node(byref st as WEState, byval kind as integer, byval node as NodePtr) as bool

DECLARE SUB ee_get_widget_code(byref code as WidgetCode, byval widget as NodePtr)

'-----------------------------------------------------------------------

SUB editor_editor()
 DIM st as EEState
 
 st.changed = NO
 st.doc = CreateDocument()
 st.root = CreateNode(st.doc, "")
 SetRootNode(st.doc, st.root)
 ee_create_new_editor_file st
  
 st.state.pt = 0
 st.state.need_update = YES
 st.state.active = YES

 ClearMenuData st.menu
 WITH st.menu
  .anchor.x = -1
  .anchor.y = -1
  .offset.x = vpages(dpage)->w / 2 * -1
  .offset.y = vpages(dpage)->h / 2 * -1
  .bordersize = -4
  .align = -1
  .maxrows = vpages(dpage)->h / 10 - 2
  .min_chars = vpages(dpage)->w / 8 - 2
  .no_box = YES
 END WITH
 
 DIM oldsize as XYPair
 
 setkeys YES
 DO
  setwait 55
  setkeys YES

  IF oldsize.x <> vpages(dpage)->w ORELSE oldsize.y <> vpages(dpage)->h THEN 
   oldsize.x = vpages(dpage)->w
   oldsize.y = vpages(dpage)->h
   st.state.need_update = YES
  END IF
  IF st.state.need_update THEN
   WITH st.menu
    .offset.x = vpages(dpage)->w / 2 * -1
    .offset.y = vpages(dpage)->h / 2 * -1
    .maxrows = vpages(dpage)->h / 10 - 2
    .min_chars = vpages(dpage)->w / 8 - 2
   END WITH
   DeleteMenuItems st.menu
   st.indent = 0
   ee_refresh st
   init_menu_state st.state, st.menu
   IF st.seek_widget THEN
    ee_focus_widget st, st.seek_widget
    st.seek_widget = 0
   END IF
   st.state.need_update = NO
  END IF
  
  IF keyval(scESC) > 1 THEN
   IF ee_okay_to_unload(st) THEN EXIT DO 
  END IF
  IF keyval(scF1) > 1 THEN show_help("editor_editor")
  IF keyval(scF3) > 1 THEN
   IF ee_okay_to_unload(st) THEN
    IF ee_browse(st) THEN
     setkeys YES
     st.state.need_update = YES
    END IF
   END IF
  END IF
  IF keyval(scF2) > 1 THEN
   ee_export st
  END IF
  IF keyval(scF5) > 1 THEN
   editor_runner st.root
  END IF

  IF st.state.pt >= 0 AND st.state.pt <= st.menu.numitems - 1 THEN
   ee_edit_menu_item st, st.menu.items[st.state.pt]
   ee_rearrange st, st.menu.items[st.state.pt]
  ELSE
   ee_insertion st, 0
  END IF

  IF keyval(scShift) = 0 THEN
   usemenu st.state
  END IF

  clearpage dpage
  draw_menu st.menu, st.state, dpage
  edgeprint "F1=Help", 0, vpages(dpage)->h - 10, uilook(uiText), dpage

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP

 IF st.clipboard <> 0 THEN FreeNode(st.clipboard)
 FreeDocument(st.doc)
 
END SUB

'-----------------------------------------------------------------------

SUB ee_create_new_editor_file(byref st as EEState)
 RenameNode st.root, "editor"
 AppendChildNode st.root, "datafile"
 AppendChildNode st.root, "recordnode"
 AppendChildNode st.root, "enums"
 AppendChildNode st.root, "widgets"
END SUB

SUB ee_edit_menu_item(byref st as EEState, mi as MenuDefItem Ptr)
 IF mi = 0 THEN debug "ee_edit_menu_item: null mi": EXIT SUB
 DIM widget as NodePtr
 widget = mi->dataptr
 IF widget = 0 THEN debug "ee_edit_menu_item: mi has null widget node": EXIT SUB

 IF ee_edit_widget(st, widget) THEN
  mi->caption = STRING(mi->extra(0), " ") & ee_widget_string(st, widget)
  st.changed = YES
 END IF

END SUB

FUNCTION ee_edit_widget(byref st as EEState, byval widget as NodePtr) as integer
 IF widget = 0 THEN debug "ee_edit_widget: null widget" : RETURN NO

 DIM changed as integer = NO

 IF ee_widget_has_caption(widget) THEN
  DIM cap as string
  cap = GetChildNodeStr(widget, "caption")
  IF strgrabber(cap, 40) THEN
   IF cap = "" THEN
    SetChildNode(widget, "caption")
   ELSE
    SetChildNode(widget, "caption", cap)
   END IF
   changed = YES
  END IF
 END IF
 
 IF keyval(scEnter) > 1 THEN
  IF widget_editor(widget) THEN
   changed = YES
  END IF
 END IF
 
 RETURN changed
END FUNCTION

SUB ee_insertion(byref st as EEState, byval widget as Nodeptr)
 IF keyval(scInsert) > 1 THEN
  IF ee_parent_is_full(widget) THEN
   visible_debug "Parent container widget is full, and cannot hold any more child widgets"
   EXIT SUB
  END IF
  DIM kind as string
  kind = ee_prompt_for_widget_kind()
  IF kind <> "" THEN
   DIM newnode as Nodeptr
   newnode = ee_create_widget(st, kind)
   IF widget THEN
    AddSiblingAfter widget, newnode
   ELSE
    DIM node as Nodeptr
    node = NodeByPath(st.root, "/widgets")
    IF node = 0 THEN
     debuginfo "unable to find /widgets container node!"
     EXIT SUB
    END IF
    AddChild node, newnode
   END IF
   st.seek_widget = newnode
   st.changed = YES
   st.state.need_update = YES
  END IF
 END IF
END SUB

SUB ee_rearrange(byref st as EEState, mi as MenuDefItem Ptr)
 DIM widget as Nodeptr
 widget = mi->dataptr
 
 DIM changed as integer = NO

 ee_insertion st, widget

 IF keyval(scShift) > 0 THEN
  IF copy_keychord() THEN
   '--copy this widget
   IF st.clipboard <> 0 THEN FreeNode(st.clipboard)
   st.clipboard = CloneNodeTree(widget)
   st.clipboard_is = widget
   changed = YES
  END IF
  IF paste_keychord() THEN
   '--paste this widget
   IF st.clipboard <> 0 THEN
    AddSiblingAfter(widget, CloneNodeTree(st.clipboard))
    IF NodeHasAncestor(widget, st.clipboard_is) THEN st.clipboard_is = 0 'cosmetic importance only
    changed = YES
   END IF
  END IF
 END IF
 
 IF keyval(scShift) > 0 THEN
  IF keyval(scUP) > 1 THEN
   ee_swap_widget_up widget
   st.seek_widget = widget
   changed = YES
  END IF
  IF keyval(scDOWN) > 1 THEN
   ee_swap_widget_down widget
   st.seek_widget = widget
   changed = YES
  END IF
  IF keyval(scLEFT) > 1 THEN
   ee_swap_widget_left widget
   st.seek_widget = widget
   changed = YES
  END IF
  IF keyval(scRIGHT) > 1 THEN
   ee_swap_widget_right widget
   st.seek_widget = widget
   changed = YES
  END IF
 END IF
 
 IF keyval(scDelete) > 1 THEN
  IF yesno("Delete this widget?" & CHR(10) & ee_widget_string(st, widget)) THEN
   FreeNode(widget)
   changed = YES
  END IF
 END IF
 
 IF changed THEN
  st.state.need_update = YES
  st.changed = YES
 END IF
END SUB

SUB ee_swap_widget_up(byval widget as Nodeptr)
 IF widget = 0 THEN EXIT SUB
 DIM sib as NodePtr
 sib = PrevSibling(widget, "widget")
 IF sib = 0 THEN EXIT SUB
 SwapSiblingNodes(widget, sib)
END SUB

SUB ee_swap_widget_down(byval widget as Nodeptr)
 IF widget = 0 THEN EXIT SUB
 DIM sib as NodePtr
 sib = NextSibling(widget, "widget")
 IF sib = 0 THEN EXIT SUB
 SwapSiblingNodes(widget, sib)
END SUB

SUB ee_swap_widget_left(byval widget as Nodeptr)
 IF widget = 0 THEN EXIT SUB
 DIM parent as NodePtr
 parent = NodeParent(widget)
 IF parent = 0 THEN EXIT SUB
 AddSiblingAfter(parent, widget)
END SUB

SUB ee_swap_widget_right(byval widget as Nodeptr)
 IF widget = 0 THEN EXIT SUB
 DIM sib as NodePtr
 sib = PrevSibling(widget, "widget")
 IF sib = 0 THEN EXIT SUB
 IF ee_container_check(sib) = NO THEN EXIT SUB
 IF ee_container_is_full(sib) THEN EXIT SUB
 AddChild(sib, widget)
END SUB

SUB ee_refresh (byref st as EEState)
 DIM widgets_container as NodePtr
 widgets_container = NodeByPath(st.doc, "/widgets")
 IF widgets_container = 0 THEN EXIT SUB
 DIM widget as NodePtr
 widget = FirstChild(widgets_container, "widget")
 DO WHILE widget
  ee_refresh st, widget
  widget = NextSibling(widget, "widget")
 LOOP
END SUB

SUB ee_refresh (byref st as EEState, byval widget as NodePtr)
 IF widget = 0 THEN EXIT SUB

 IF widget = 0 THEN
  EXIT SUB
 END IF

 DIM s as string
 s = STRING(st.indent, " ") & ee_widget_string(st, widget)
 
 DIM index as integer
 index = append_menu_item(st.menu, s)
 
 DIM mi as MenuDefItem Ptr
 mi = st.menu.items[index]

 mi->dataptr = widget
 mi->extra(0) = st.indent

 st.indent += 1 
 DIM chnode as Nodeptr
 chnode = FirstChild(widget, "widget")
 DO WHILE chnode
  ee_refresh st, chnode
  chnode = NextSibling(chnode, "widget")
 LOOP
 st.indent -= 1
END SUB

FUNCTION ee_widget_string(byref st as EEState, byval widget as Nodeptr) as string
 IF widget = 0 THEN debug "ee_widget_string: null node" : RETURN "<null ptr>"
 DIM s as string = ""
 IF widget = st.clipboard_is OR NodeHasAncestor(widget, st.clipboard_is) then s &= "*"
 s &= "<" & GetString(widget) & ">" & GetChildNodeStr(widget, "caption", "")
 IF ee_widget_has_data(widget) THEN
  s &= " [" & GetChildNodeStr(widget, "data", "") & "]"
 END IF
 RETURN s
END FUNCTION

SUB ee_focus_widget(byref st as EEState, byval widget as Nodeptr)
 DIM mi as MenuDefItem Ptr
 DIM n as Nodeptr
 FOR i as integer = 0 TO st.menu.numitems - 1
  mi = st.menu.items[i]
  n = mi->dataptr
  IF n = widget THEN
   st.state.pt = i
   EXIT FOR
  END IF
 NEXT i
 WITH st.state
  .pt = small(.pt, .last)
  .top = bound(.top, .pt - .size, .pt)
 END WITH
END SUB

SUB ee_export(byref st as EEState)
 DIM outfile as string
 outfile = inputfilename("Export editor definition", "", "", "input_file_export_ee", st.filename)
 IF outfile <> "" THEN
  IF INSTR(outfile, ".") = 0 THEN outfile &= ".editor"
  ee_save outfile, st
 END IF
END SUB

FUNCTION ee_browse(byref st as EEState) as integer
 DIM filename as string
 filename = browse(0, "", "*.editor", "",, "browse_import_ee")
 IF filename = "" THEN RETURN NO
 RETURN ee_load(filename, st)
END FUNCTION

FUNCTION ee_load(filename as string, byref st as EEState) as integer
 st.filename = ""
 FreeDocument st.doc
 st.doc = LoadDocument(filename, optNoDelay)
 IF st.doc = 0 THEN debug "load '" & filename & "' failed: null doc": RETURN NO
 st.root = DocumentRoot(st.doc)
 IF st.root = 0 THEN debug "load '" & filename & "' failed: null root node": RETURN NO
 st.filename = trimpath(filename)
 st.changed = NO
 RETURN YES
END FUNCTION

SUB ee_save(filename as string, byref st as EEState)
 SerializeBin(filename, st.doc)
 st.filename = trimpath(filename)
 st.changed = NO
END SUB

FUNCTION ee_okay_to_unload(byref st as EEState) as integer
 IF st.changed = NO THEN RETURN YES
 DIM choice as integer
 'Prevent attempt to quit the program, stop and wait for response first
 DIM quitting as bool = getquitflag()
 setquitflag NO
 choice = twochoice("Save your changes before exiting?", "Yes, save", "No, discard")
 IF getquitflag() THEN choice = 1  'Second attempt to close the program: discard
 SELECT CASE choice
  CASE -1: 'cancelled
   RETURN NO
  CASE 0: 'yes, save!
   ee_export st
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

'-----------------------------------------------------------------------
'Update these to add a new widget:

FUNCTION ee_prompt_for_widget_kind() as string
 STATIC last_kind as integer = 0
 'The order widgets are listed here doesn't matter for anything other than the order they appear in the menu
 DIM w(...) as string = { _
  "int",_
  "string",_
  "label",_
  "bool",_
  "submenu",_
  "picture",_
  "item",_
  "attack",_
  "textbox",_
  "tag",_
  "tagcheck",_
  "equipslot",_
  "multi",_
  "array",_
  "maybe",_
  "exclusive"}
 DIM choice as integer
 choice = multichoice("Insert which kind of widget?", w(), last_kind, , "ee_prompt_for_widget_kind")
 IF choice = -1 THEN RETURN ""
 last_kind = choice
 RETURN w(choice)
END FUNCTION

FUNCTION ee_create_widget(byref st as EEState, kind as string) as NodePtr
 DIM widget as NodePtr
 widget = CreateNode(st.doc, "widget")
 SetContent(widget, kind)
 'We could add sub-nodes below if we wanted to
 SELECT CASE kind
  CASE "int":
  CASE "string":
  CASE "label":
  CASE "bool":
  CASE "submenu":
  CASE "picture":
  CASE "item":
  CASE "attack":
  CASE "textbox":
  CASE "tag":
  CASE "tagcheck":
  CASE "multi":
  CASE "array":
  CASE "maybe":
  CASE "exclusive":
  CASE "equipslot":
  CASE ELSE
   debug "Oops! Created a widget of kind """ & kind & """, but we have no idea what that is!"
 END SELECT
 RETURN widget
END FUNCTION

FUNCTION ee_container_check(byval cont as NodePtr) as bool
 IF cont = 0 THEN RETURN NO
 SELECT CASE GetString(cont)
  CASE "submenu": RETURN YES
  CASE "array": RETURN YES
  CASE "maybe": RETURN YES
  CASE "exclusive": RETURN YES
  CASE "multi": RETURN YES
 END SELECT
 RETURN NO
END FUNCTION

FUNCTION ee_container_single_check(byval cont as NodePtr) as bool
 'For containers that only hold one child widget
 IF cont = 0 THEN RETURN NO
 SELECT CASE GetString(cont)
  CASE "multi": RETURN YES
 END SELECT
 RETURN NO
END FUNCTION

FUNCTION ee_widget_has_caption(byval widget as NodePtr) as integer
 'True for widgets that use a caption node.
 IF widget = 0 THEN RETURN NO
 SELECT CASE GetString(widget)
  CASE "array": RETURN NO
  CASE "maybe": RETURN NO
 END SELECT
 RETURN YES
END FUNCTION

FUNCTION ee_widget_has_data(byval widget as NodePtr) as integer
 'True for widgets that use a data node
 IF widget = 0 THEN RETURN NO
 SELECT CASE GetString(widget)
  CASE "label": RETURN NO
 END SELECT
 RETURN YES
END FUNCTION

'-----------------------------------------------------------------------

FUNCTION ee_count_children(byval cont as NodePtr) as integer
 IF cont = 0 THEN RETURN 0
 DIM result as integer = 0
 DIM ch as NodePtr = FirstChild(cont, "widget")
 DO WHILE ch
  result += 1
  ch = NextSibling(ch, "widget")
 LOOP
 IF result > 0 THEN
  IF NOT ee_container_check(cont) THEN debug "ee_count_children: Found " & result & " child widgets on non-container: " & GetString(cont) 
  IF result > 1 ANDALSO ee_container_single_check(cont) THEN
   debug "ee_count_children: Found " & result & " child widgets on a container that only allows one child: " & GetString(cont)
  END IF
 END IF
 RETURN result
END FUNCTION

FUNCTION ee_container_is_full(byval cont as NodePtr) as bool
 IF cont = 0 THEN debug "ee_container_is_full: null cont" : RETURN YES
 IF NOT ee_container_check(cont) THEN
  debug "ee_container_is_full: " & GetString(cont) & " is not a container"
  RETURN YES
 END IF
 IF ee_container_single_check(cont) ANDALSO ee_count_children(cont) >= 1 THEN RETURN YES
 RETURN NO
END FUNCTION

FUNCTION ee_parent_is_full(byval cont as NodePtr) as bool
 IF cont = 0 THEN debug "ee_parent_is_full: null cont" : RETURN YES
 DIM parent as NodePtr = NodeParent(cont)
 IF parent = 0 THEN debug "ee_parent_is_full: no parent exits" : RETURN YES
 DIM pname as string = NodeName(parent)
 debug "ee_parent_is_full: parent name = """ & pname & """"
 IF pname <> "widget" THEN
  'Parent is not a widget
  IF pname = "widgets" THEN
   'Top level widget is never full
   RETURN NO
  END IF
  debug "ee_parent_is_full: unknown parent node name: """ & pname & """"
  RETURN YES
 END IF
 IF NOT ee_container_check(parent) THEN
  debug "ee_parent_is_full: parent is not really a container: """ & GetString(parent) & """"
  RETURN YES
 END IF
 RETURN ee_container_is_full(parent)
END FUNCTION

'-----------------------------------------------------------------------

FUNCTION widget_editor(byval widget as NodePtr) as integer

 DIM st as WEState
 st.changed = NO

 st.state.pt = 1
 st.state.need_update = YES
 st.state.active = YES

 ClearMenuData st.menu
 WITH st.menu
  .anchor.x = -1
  .anchor.y = -1
  .bordersize = -4
  .align = -1
  .no_box = YES
 END WITH
 
 DIM oldsize as XYPair
 
 ee_get_widget_code(st.code, widget)
 
 setkeys YES
 DO
  setwait 55
  setkeys YES

  IF oldsize.x <> vpages(dpage)->w ORELSE oldsize.y <> vpages(dpage)->h THEN 
   oldsize.x = vpages(dpage)->w
   oldsize.y = vpages(dpage)->h
   st.state.need_update = YES
  END IF
  IF st.state.need_update THEN
   st.state.need_update = NO
   WITH st.menu
    .offset.x = vpages(dpage)->w / 2 * -1
    .offset.y = vpages(dpage)->h / 2 * -1
    .maxrows = vpages(dpage)->h / 10 - 2
    .min_chars = vpages(dpage)->w / 8 - 2
   END WITH
   DeleteMenuItems st.menu
   widget_editor_refresh st, widget
   init_menu_state st.state, st.menu
  END IF
  
  IF keyval(scESC) > 1 ORELSE st.exit_please THEN
   EXIT DO 
  END IF
  IF keyval(scF1) > 1 THEN show_help("widget_editor")

  IF st.state.pt >= 0 AND st.state.pt <= st.menu.numitems - 1 THEN
   widget_editor_edit_menu_item st, st.menu.items[st.state.pt]
  END IF
  
  IF keyval(scShift) = 0 THEN
   usemenu st.state
  END IF

  clearpage dpage
  draw_menu st.menu, st.state, dpage
  edgeprint "F1=Help", 0, vpages(dpage)->h - 10, uilook(uiText), dpage

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP

 RETURN st.changed 
END FUNCTION

SUB widget_editor_edit_menu_item(byref st as WEState, mi as MenuDefItem Ptr)
 IF mi = 0 THEN debug "widget_editor_edit_menu_item: null mi": EXIT SUB
 DIM kind as integer = mi->t
 DIM node as NodePtr 
 node = mi->dataptr
 IF kind <> wedEXIT THEN
  IF node = 0 THEN debug "widget_editor_edit_menu_item: mi (" & mi->caption & ") has null node": EXIT SUB
 END IF

 IF widget_editor_edit_node(st, kind, node) THEN
  st.changed = YES
  'Something changed, signal to rebuild the menu strings
  st.state.need_update = YES
 END IF

END SUB

FUNCTION widget_editor_edit_node(byref st as WEState, byval kind as integer, byval node as NodePtr) as bool
 IF node = 0 THEN debug "widget_editor_edit_node: null node" : RETURN NO

 DIM changed as bool = NO

 SELECT CASE kind
  CASE wedEXIT:
   IF enter_or_space() THEN st.exit_please = YES
  CASE wedSTRING:
   DIM s as string = GetString(node)
   IF strgrabber(s, 1000000) THEN ' The 1 million character limit is totally arbitrary
    SetContent node, s
    changed = YES
   END IF
  CASE wedINT:
   DIM i as integer = GetInteger(node)
   IF intgrabber(i, -1000000, 1000000) THEN 'The +- 1 million limit is totally arbitrary
    SetContent node, i
    changed = YES
   END IF
  CASE wedBOOL:
   DIM b as integer = GetInteger(node) <> 0
   IF keyval(scLeft) > 1 ORELSE keyval(scRight) > 1 ORELSE enter_or_space() THEN
    SetContent node, (NOT b)
    changed = YES
   END IF
 END SELECT
  
 RETURN changed
END FUNCTION

'-----------------------------------------------------------------------

SUB wed_append_editable_string(byref st as WEState, caption as string, byval widget as NodePtr, sub_widget_name as string)
 DIM n as NodePtr = GetOrCreateChild(widget, sub_widget_name)
 DIM s as string = GetString(n)
 append_menu_item(st.menu, caption & ":" & s, wedSTRING, , n)
END SUB

SUB wed_append_zdefault_int(byref st as WEState, caption as string, byval widget as NodePtr, sub_widget_name as string)
 DIM n as NodePtr = GetOrCreateChild(widget, sub_widget_name)
 DIM i as integer = GetInteger(n)
 append_menu_item(st.menu, caption & ":" & zero_default(i), wedINT, , n)
END SUB

SUB wed_append_bool(byref st as WEState, caption as string, byval widget as NodePtr, sub_widget_name as string)
 DIM n as NodePtr = GetOrCreateChild(widget, sub_widget_name)
 DIM b as bool = GetInteger(n) <> 0
 append_menu_item(st.menu, caption & ":" & yesorno(b), wedBOOL, , n)
END SUB

SUB wed_append_zero_or_one(byref st as WEState, caption as string, byval widget as NodePtr, sub_widget_name as string)
 DIM n as NodePtr = GetOrCreateChild(widget, sub_widget_name)
 DIM i as integer = GetInteger(n)
 append_menu_item(st.menu, caption & ":" & i & iif_string(i < 0 ORELSE i > 1, " [should be 0 or 1!]", ""), wedINT, , n)
END SUB

SUB widget_editor_refresh(byref st as WEState, byval widget as NodePtr)
 DIM index as integer
 append_menu_item(st.menu, "Done Editing this Widget...", wedEXIT)
 IF ee_widget_has_caption(widget) THEN
  wed_append_editable_string(st, "Caption", widget, "caption")
 END IF
 IF ee_widget_has_data(widget) THEN
  wed_append_editable_string(st, "Data Node", widget, "data")
 END IF
 st.code.refresh_callback(st, widget)
END SUB

'-----------------------------------------------------------------------

SUB null_widget_refresh(byref st as WEState, byval widget as NodePtr)
 'for widgets that don't have any extra properties.
END SUB

SUB int_widget_refresh(byref st as WEState, byval widget as NodePtr)
 wed_append_zdefault_int(st, "Max", widget, "max")
 wed_append_zdefault_int(st, "Min", widget, "min")
 wed_append_editable_string(st, "Enum key", widget, "enum")
 wed_append_bool(st, "Optional", widget, "optional")
 wed_append_bool(st, "Zero Default", widget, "zerodefault")
 wed_append_bool(st, "-1 Default", widget, "neg1default")
END SUB

SUB picture_widget_refresh(byref st as WEState, byval widget as NodePtr)
 append_menu_item(st.menu, "Size Group:" & GetChildNodeInt(widget, "sizegroup"))
 append_menu_item(st.menu, "Save Size:" & yesorno(GetChildNodeBool(widget, "savesize")))
END SUB

SUB tagcheck_widget_refresh(byref st as WEState, byval widget as NodePtr)
 append_menu_item(st.menu, "Default Description:" & GetChildNodeStr(widget, "default"))
END SUB

SUB multi_widget_refresh(byref st as WEState, byval widget as NodePtr)
 wed_append_zero_or_one(st, "Minimum repetitions", widget, "min_nodes")
END SUB

SUB array_widget_refresh(byref st as WEState, byval widget as NodePtr)
 append_menu_item(st.menu, "Count:" & zero_default(GetChildNodeInt(widget, "count"), "variable length"))
 append_menu_item(st.menu, "Key:" & GetChildNodeStr(widget, "key"))
 append_menu_item(st.menu, "Enum:" & GetChildNodeStr(widget, "enum"))
END SUB

SUB maybe_widget_refresh(byref st as WEState, byval widget as NodePtr)
 append_menu_item(st.menu, "Hide:" & yesorno(GetChildNodeBool(widget, "hide")))
END SUB

'-----------------------------------------------------------------------
'#######################################################################

'--this is at the end of the file because I want to be lazy and not bother
'  with separate declares for each of the callbacks above.
SUB ee_get_widget_code(byref code as WidgetCode, byval widget as NodePtr)
 WITH code
  .refresh_callback = @null_widget_refresh

  IF widget = 0 THEN EXIT SUB
  DIM kind as string
  kind = GetString(widget)
  
  SELECT CASE kind
   CASE "int":
    .refresh_callback = @int_widget_refresh
   CASE "string":
   CASE "label":
   CASE "bool":
   CASE "picture":
    .refresh_callback = @picture_widget_refresh
   CASE "item":
   CASE "attack":
   CASE "tagcheck":
    .refresh_callback = @tagcheck_widget_refresh
   CASE "tag":
   CASE "multi":
    .refresh_callback = @multi_widget_refresh
   CASE "array":
    .refresh_callback = @array_widget_refresh
   CASE "maybe":
    .refresh_callback = @maybe_widget_refresh
  END SELECT
 END WITH
END SUB
