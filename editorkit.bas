'OHRRPGCE - EditorKit framework for creating editors
'(C) Copyright 1997-2023 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

' ==== EditorKit classes ====
' To create an editor with EditorKit, create a UDT Extending EditorKit, and
' implement sub define_items(). This will be called repeatedly for:
' processing: it's called every tick to handle editing of data fields
' activating: clicking or space/enter activating a menu item (actually the same as
'   the processing phase)
' refreshing: generate an array of items for display and navigation; called
'   whenever state.need_update is true (which define_items sets during processing)
'
' Then call .run(). But you probably want to load data or do other initialisation
' first, such as setting .helpkey.
'
' ==== Submenus ====
' Define submenus by branching on the value of `submenu` inside .define_items()
' and enter one by calling .enter_submenu() (which nests) or .switch_submenu().
' For a submenu-specific help page set helpkey in define_items (not set_helpkey()!)
'
' ==== Saving, loading, and multiple records ====
' If you overload the load() and save() methods they will be called when
' run() begins and right before it ends.
' To switch between multiple records you need to call setup_record_switching before
' .run() to define what the variable is that holds the current record number.
' load() and save() are assumed to access this variable (hence they take no args),
' and they are both called when switching records.
' Then call def_record_switcher in define_items() to add a <-Foo #-> line.
'
' ==== Adding menu items ====
' "Previous Menu" (customisable with prev_menu_text) is added automatically.
'
' To add a menu item, call from define_items():
' -spacer: a blank line
' -section or subsection: add a section header, which is unselectable. In future,
'  sections will be collapsible
' -defitem, or other def* function (which are convenience wrappers around defitem)
'
' Each defitem menu item is split into a "title" and a "caption" (either of
' which may be blank).  The title is the field description and should usually
' end in ':'; the caption presents the value. The title is set only by defitem/def*.
' The caption is set automatically to the value but you can replace it with
' set_caption or tell how to generate it with caption* functions.
'
' ==== State variables ====
' A menu item definition starting with defitem ends at the next defitem/spacer/etc
' call. defitem by itself just creates a menu item that does nothing.
'
' Inside the definition you can query some bool members:
' -selected: this menu item is the selected item
' -refresh: refreshing the menu
' -process: selected and should do per-tick logic, such as calling intgrabber
' -process_text: should be checked instead of process to decide whether to read text
'          input. (True even if Alt is held, but not when selecting-by-typing)
'          (Be sure to set using_strgrabber if you do.)
' -activate: selected and should be activated (if possible), e.g. enter a submenu.
' -delete_action: the user tried to delete this (Delete or possibly Backspace)
' -hover: mouse over this item
' -left_click: beginning of a left click/drag on this item. Use activate instead,
'          if you can which checks for button release.
' -right_click: beginning of a right click/drag.
' And a couple you can read/write:
' -edited: an edit_* call changed the item's value. You should set this manually if
'          you modify `value` manually.
' -state.needs_update: can also be set to indicate the menu needs refreshing
'
' So if you want to enter a submenu:
'     defitem "Edit details..."
'     if activate then edit_widget_details
' Or as a shortcut:
'     if defitem_act("Edit details...") then edit_widget_details
' Or if the submenu is defined within the same class:
'     if defitem_act("Edit details...") then enter_submenu "details"
'
' ==== Data ====
' Items can display and (optionally) edit a field of data, which could be an
' integer/string/etc passed byref, a RELOAD Node, ohrrpgce_config.ini setting,
' or general.reld setting. It works like so:
'
' -The datum is read into `value` (ints and bools), `valuestr` or `valuefloat`
'  by calling val_*, as_*, edit_*, edit_as_* or def*, and its source (eg. a 
'  Node) is recorded.
' -The value can be shifted with `offset_int`, or a bool inverted with
'  `invert_bool` or by prefixing the title with '!' (just like editbitset). Must
'  happen before editing or setting the caption.
' -edit_* methods will, `if process`, call intgrabber/etc to modify `value`/etc
'  and set `edited`. They also immediately call write_value for safety.
' -If `edited` is true, the value is written back; if you have custom editing
'  code (e.g. a *grabber call) that modifies value/etc you should set `edited`.
'  (This happens even during refresh, so it's OK to modify the value then.)
'
' You don't need to set the value with val_*/etc if you write custom editing code
' enclosed in `if process` and use set_caption for the caption.
'
' The families of available methods:
'
' -val_* to tell which value to edit. Examples:
'     val_int gen(genItemStackSize)  'Passing a value byref to record a ptr to it
'     val_node_int DocumentRoot(doc)  'A Node to edit (doesn't need to be byref)
'     'Uses NodeByPath to get a child, with default value "Weapon" if missing
'     val_node_str menunode, "/weapon/caption", "Weapon"
'     val_bitset bits(), 0, 35  'Starting from word 0, bit 35
'
' -as_* to tell what the value means, e.g. a tag check, enemy ID, or script
'  trigger - you won't use this for raw data. This just changes the default
'  caption (normally you use edit_as_* instead). Pass value/etc as the first
'  arg, e.g.
'     val_int rec(42)
'     as_enemy value
'  Which can also be written
'     as_enemy val_int(rec(42))
'  As a shortcut for byref data (val_int/bool/str/float) you can skip the val_*:
'     as_enemy rec(42)
'
' -edit_* to tell how to edit a value (if processing), e.g.:
'     val_node_int boxstyle_node
'     edit_int value, 0, 14   'Range 0 to 14
'  ...but as a shortcut you can skip the val_* (there are edit_X functions for
'  most val_X):
'     edit_node_int boxstyle_node, 0, 14
'  If you use an explicit val_* then he first arg to edit_* will be value/etc.
'
' -def*: As a further shortcut for simple values, you can use a def* method
'  which combines defitem and edit_*:
'     defitem "Default maximum item stack size:"
'     edit_int gen(genItemStackSize), 1, 99
'  can become:
'     defint "Default maximum item stack size:", gen(genItemStackSize), 1, 99
'  which is complete!
'
'  You can NOT write something like "defint "...", val_node_int(...), 0, 10"
'  because the menu item doesn't start until defitem is called.
'
' -edit_as_* for game data like tags or enemies, extends as_* with editing,
'  including bounds, entering browsers/submenus, etc. There's an edit_as_X for
'  every as_X. E.g.
'     edit_as_enemy rec(42)
'
' ==== "None" options and offset values ====
' Many edit_as_* methods take an Or_None flag to indicate -1 means None:
'     edit_as_enemy rec(42), Or_None
'
'  If you want 0 on-disk to be None and N > 0 to be record N-1 then use
'  offset_int to shift `value` from the on-disk value:
'     offset_int -1   'Can be called either before or after val_*
'     edit_as_enemy rec(42), Or_None
'  Alternatively:
'     edit_as_enemy offset_int(-1, rec(42)), Or_None
'  You can write it this equivalent way:
'     val_int rec(42)
'     value -= 1
'     edit_as_enemy value, Or_None
'     value += 1
'
' ==== Captions ====
' The caption defaults to the item's value if the title ends in ':'.  It can be
' set it with set_caption, or a caption* function such as `captions` for enum
' strings.  caption* methods (other than set_caption) must be called after
' value/valuestr/valuefloat is set!
'
' Example:
'     defint "Display '" & CHR(1) & "1' in inventory:", gen(genInventSlotx1Display), 0, 2
'     captions_list("always", "never", "only if stackable")
'
' ==== More examples ====
'
' You can call methods conditionally, as long as the same defitems are called during the
' processing and refreshing phases so the the menu item indices match.
' For example, in the formation editor, activation handling needs to overridden:
'     for slot as integer = 0 to ubound(form.slots)
'       defitem "Enemy:"
'       offset_int -1   '0 is None, 1+ is enemy ID+1
'       as_enemy form.slots(slot).id, Or_None   'Sets caption
'       if activate then
'         edited or= reposition_or_change_enemy_submenu(value)
'       else
'         edit_as_enemy value, Or_None   'Calling both as_enemy and edit_as_enemy is harmless
'       end if
'       if value = -1 then set_caption "Empty"  'Override default "None" caption
'     next

#include "config.bi"
#include "common.bi"
#include "reloadext.bi"
#include "editorkit.bi"
#include "loading.bi"
#include "custom.bi"
#include "customsubs.bi"
#include "sliceedit.bi"   'extra_data_editor
#include "thingbrowser.bi"

using Reload.Ext

DEFINE_VECTOR_OF_TYPE(SubmenuState, SubmenuState)


'===============================================================================
'                               Overridable methods

' define_items is abstract, must be overridden. Others are optional.

' Called when entering the menu and after switching records (the record id passed to
' setup_record_switching is modified before calling this).
' NOTE: you should set state.need_update = YES, as it possibly won't be automatically.
sub Editorkit.load()
end sub

' Called when exiting the menu and before switching records (the record id passed to
' setup_record_switching is modified after calling this).
sub EditorKit.save()
end sub

' Used by the Alt quick record switcher, normally the name of the record, or some other
' suitable preview text. Shouldn't include the id, it'll be appended.
' Defaults to name of the record type, e.g. "Textbox".
function EditorKit.get_record_name(id as integer) as string
	return record_type_name
end function

'===============================================================================
'                         ModularMenu hooks (internal)

sub EditorKit.update()
	' ModularMenu calls update() before any other hooks
	if initialised = NO then
		load()
		' On the first call store helpkey, as it may get clobbered
		if len(default_helpkey) = 0 then default_helpkey = base.helpkey
		initialised = YES
	end if

	want_exit = NO
	want_activate = NO

	run_phase(Phases.refreshing)
end sub

function EditorKit.each_tick() as bool
	base.helpkey = default_helpkey
	base.tooltip = ""
	want_exit = keyval(ccCancel) > 1
	want_submenu = "NO"
	want_activate = enter_space_click(state)
	record_id_grabber_called = NO

	run_phase(Phases.processing)

	'if enter_space_click(state) then
	'	activated_item = state.pt
	'	state.need_update or= run_phase(Phases.activating)
	'end if
	'if state.need_update = NO then
	'	run_phase(Phases.processing)
	'end if

	' Alt to change record.
	if record_id_ptr then
		' On string fields, Alt is for entering special characters.
		' This also means we have to do this after run_phase
		if keyval(scAlt) > 0 andalso not using_strgrabber then
			' Disable select-by-typing, although we don't use text input
			using_strgrabber = YES

			record_id_grabber
		end if
	end if

	if want_exit then
		if v_len(submenu_stack) then  'Exit submenu
			want_submenu = v_pop(submenu_stack)
		elseif try_exit() then  'Exit root menu
			save()
			return YES
		end if
		' Don't let ModularMenu handle it
		if keyval(ccCancel) > 1 then setkeys
	end if
	if want_submenu <> "NO" then
		apply_enter_submenu want_submenu
	end if

	'tooltip = v_str(submenu_stack) & " '" & submenu & "'"
	return NO
end function

sub EditorKit.draw_overlays()
	if record_id_ptr andalso keyval(scAlt) > 0 then
		var id = *record_id_ptr
		textcolor uilook(uiText), uilook(uiHighlight)
		printstr get_record_name(id) & " " & id, pRight, 0, vpage
	end if
end sub

'===============================================================================
'                                   Internal

constructor EditorKit()
	v_new submenu_stack
	saved_submenus.construct(8, type_table(SubmenuState), YES)
	saved_submenus.value_copy = NULL   'Delete but don't copy
end constructor

destructor EditorKit()
	v_free submenu_stack
end destructor

' Wrapper around define_items()
sub EditorKit.run_phase(which_phase as Phases)
	phase = which_phase
	cur_item_index = 0
	started_item = NO
	edited = NO

	refresh = (phase = Phases.refreshing)
	process = NO
	activate = NO

	if refresh then clear_menu

	defitem prev_menu_text
	if activate then want_exit = YES

	define_items()
	' End the final item
	finish_defitem
end sub

' Called after an item definition is finished
sub EditorKit.finish_defitem()
	if started_item = NO then exit sub

	if activate then
		' If you enter some editor and then exit it by hitting ESC/etc then we
		' need to ignore that cancel key or this menu will exit.
		' (Ideally would call setkeys regardless of how you exit the menu,
		' but it's not possible to tell that we entered one, and if you exit
		' it any other way there doesn't seem to be a possibility of
		' misinterpreting input.)
		if keyval(ccCancel) > 1 then setkeys
	end if

	if edited then
		state.need_update = YES
		write_value
		edited = NO
	end if

	if selected then
		if cur_item.helpkey <> "" and tooltip = "" then tooltip = "F1 for details"
	end if

	cur_item_index += 1

	if refresh then
		with cur_item
			dim as string text, title = .title, caption = .caption
			' If there's no caption, use the current data value as a default
			if len(caption) = 0 andalso ends_with(.title, ":") then
				select case .dtype
					case dtypeBool:   caption = iif(value, "YES", "NO")
					case dtypeInt:    caption = str(value)
					case dtypeFloat:  caption = str(valuefloat)
					case dtypeStr:    caption = valuestr
				end select
			end if

			if len(title) > 0 then
				title &= " "
			end if

			if .color then text = fgtag(ColorIndex(.color))
			text &= title & caption

			base.add_item .id, 0, text, (.unselectable = NO), NO, .disabled
		end with
	end if

	started_item = NO
end sub

' Get or create a SubmenuState
function EditorKit.get_submenu_state(name as string) as SubmenuState ptr
	dim ret as SubmenuState ptr
	ret = saved_submenus.get(name)
	if ret = null then
		ret = new SubmenuState
		saved_submenus.add(name, ret)
	end if
	return ret
end function

' Delayed switch/enter_submenu logic
sub EditorKit.apply_enter_submenu(name as string = "")
	state.need_update = YES

	' Save old state
	with *get_submenu_state(submenu)
		.pt = state.pt
		.top = state.top
	end with

	submenu = name

	' Restore previous state (blank if never visted)
	with *get_submenu_state(name)
		state.pt = .pt
		state.top = .top
	end with
end sub

' Helper for writeNodePath*
private function create_or_delete_default_node(cur_item as EditorKitItem, is_default as bool) as Node ptr
	with cur_item
		dim valnode as Node ptr
		if .delete_default andalso is_default then
			valnode = NodeByPath(.node, .path)
			if valnode then FreeNode valnode
		else
			return NodeByPath(.node, .path, YES)  'Create it
		end if
	end with
end function

' Write a modified value/valuestr/valuefloat back to where it was read from -- called when edited=YES
sub EditorKit.write_value()
	with cur_item
		if .writer = writerNone andalso .dtype = dtypeNone then
			' Happens if you use just defitem, do everything yourself,
			' but set the 'edited' flag, or if you call dont_write or delete_node.
			exit sub
		end if

		' Get the actual output integer value to write (no changes needed to string/float)
		dim outvalue as integer = value
		if .offset then
			assert(.dtype = dtypeInt)
			outvalue -= .offset
		end if
		if .inverted_bool then
			assert(.dtype = dtypeBool)
			outvalue xor= YES
		end if

		select case as const .writer
			case writerByte
				*.byte_ptr = outvalue
			case writerBoolean
				' FB booleans take value 0/-1 but are stored in
				' memory as a 0/1 byte (to match a C/C++ bool)
				*.byte_ptr = iif(outvalue, 1, 0)
			case writerBit
				if outvalue then
					*.int_ptr or= .whichbit
				else
					*.int_ptr and= not .whichbit
				end if
			case writerInt  'Includes bool
				*.int_ptr = outvalue
			case writerStr
				*.str_ptr = valuestr
			case writerDouble
				*.double_ptr = valuefloat
			case writerNodeInt
				SetContent(.node, outvalue)
			case writerNodeBool
				SetContent(.node, iif(outvalue, 1, 0))
			case writerNodeStr
				SetContent(.node, valuestr)
			case writerNodeFloat
				SetContent(.node, valuefloat)
			case writerNodePathInt, writerNodePathBool
				var valnode = create_or_delete_default_node(cur_item, outvalue = .writer_default_int)
				if .writer = writerNodePathBool then
					outvalue = iif(outvalue, 1, 0)
				end if
				if valnode then SetContent(valnode, outvalue)
			case writerNodePathStr
				var valnode = create_or_delete_default_node(cur_item, valuestr = .writer_default_str)
				if valnode then SetContent(valnode, valuestr)
			case writerNodePathFloat
				var valnode = create_or_delete_default_node(cur_item, valuefloat = .writer_default_float)
				if valnode then SetContent(valnode, valuefloat)
			case writerNodePathExists
				' If value is true, create it
				var valnode = NodeByPath(.node, .path, outvalue <> NO)
				if outvalue = NO andalso valnode then
					' If value is false, delete it
					FreeNode valnode
				end if
			case writerConfigBool
				write_config .path, yesorno(outvalue)

			case else  ' Including writerNone
				' We should have set both .dtype and .writer in val_*
				showbug "EditorKit: bad/missing writer"
		end select
	end with
end sub

'===============================================================================
'                             Editor setup routines

' max_record will only be modified if max_record_max is provided.
' max_record_max_ is the maximum number of records that can be added. Adding new records is
'   enabled by providing this value.
sub EditorKit.setup_record_switching(byref record_id as integer, min_record as integer = 0, byref max_record as integer, max_record_offset_ as integer = 0, record_type_name_ as string = "Record", max_record_max_ as integer = 0)
	record_id_ptr = @record_id
	min_record_id = min_record
	max_record_id_ptr = @max_record
	max_record_offset = max_record_offset_
	max_record_max = max_record_max_
	record_type_name = record_type_name_
end sub

/'
' Call this to allow deleting records from the end ("cropafter")
sub EditorKit.setup_cropafter(...)
	cropafter_lump = lump
	'Delete when selected, or Shift-Backspace everywhere
end sub
'/

'===============================================================================
'                          Other non-menu-item methods

' User wants to delete this item (e.g. Delete key)
' (In future, may cause a clickable delete icon to display)
function EditorKit.delete_action() as bool
	if process then
		' Most editing routines already listen for Backspace
		if can_use_strgrabber andalso cur_item.dtype = dtypeNone then
			if keyval(scBackspace) > 1 then return YES
		end if
		return keyval(scDelete) > 1
	end if
end function

' Call during 'process' or 'activate' to do a conditional exit from the menu.
' Exits the submenu, if any, otherwise, the try_exit() virtual method will be called,
' which can override the attempt to completely exit the menu.
sub EditorKit.exit_menu()
	want_exit = YES
end sub

' Enter a submenu, forming a stack of submenus, or if it's already on the stack,
' return to it and don't push the current menu.
' Regardless of whether a submenu is on the stack, its menu cursor is saved.
' The main visible effect of this is setting 'submenu'.
sub EditorKit.enter_submenu(name as string = "")
	if name = submenu then
		' This would mess up the stack
		'debug "noop enter_submenu(" & name & "), stack=" & v_str(submenu_stack)
		exit sub
	end if

	' If already stacked (open) go back to that stack frame, otherwise push onto stack
	dim idx as integer = v_find(submenu_stack, name)
	if idx > -1 then
		v_delete_slice(submenu_stack, idx, v_len(submenu_stack))
	else
		v_append(submenu_stack, submenu)
	end if

	' Delay changing state.pt, because it would mess up 'selected', 'activate', etc.
	want_submenu = name
end sub

' Enter a submenu, which becomes the root menu (exiting from it exits completely).
' Good for a tabbed menu.
sub EditorKit.switch_submenu(name as string = "")
	v_resize(submenu_stack, 0)
	want_submenu = name
end sub

sub EditorKit.switch_record(newid as integer)
	BUG_IF(record_id_ptr = 0, "Missing setup_record_switching")
	dim byref id as integer = *record_id_ptr
	save()
	id = newid
	load()
	state.need_update = YES  'load() is also meant to do this
end sub

/'
sub EditorKit.crop_after_record(...)
end sub
'/

' Allow typing in/editing the record ID number. Switches record and can also add new records if
' max_record_max was provided.
' Returns true if record changed.
function EditorKit.record_id_grabber() as bool
	' There are multiple ways to call this, only check once per tick
	if record_id_grabber_called then return NO
	record_id_grabber_called = YES

	BUG_IF(record_id_ptr = 0 orelse max_record_id_ptr = 0, "Missing setup_record_switching", NO)
	dim byref id as integer = *record_id_ptr
	dim byref maxvar as integer = *max_record_id_ptr
	dim maxid as integer = maxvar + max_record_offset
	dim newid as integer = id
	if max_record_max > 0 then
		intgrabber_with_addset(newid, 0, maxid, max_record_max, record_type_name)
		if newid > maxid then
			maxvar = newid - max_record_offset
		end if
	else
		intgrabber(newid, 0, maxid)
	end if
	if newid <> id then switch_record newid
	return newid <> id
end function

'===============================================================================
'                            Non-data menu item types

' Add a blank line
sub EditorKit.spacer()
	finish_defitem
	if refresh then base.add_spacer
	cur_item_index += 1
end sub

' Add an unselectable, highlighted & offset section header line, preceded by spacer
sub EditorKit.section(title as zstring ptr)
	finish_defitem
	' Doesn't count as an item
	if refresh then base.header " " & *title
	cur_item_index += 2
end sub

' Add an unselectable highlighted subsection header. No spacer in front
sub EditorKit.subsection(title as zstring ptr)
	finish_defitem
	' Doesn't count as an item
	if refresh then base.add_item , , *title, NO, YES
	cur_item_index += 1
end sub

sub EditorKit.def_record_switcher()
	BUG_IF(record_id_ptr = 0, "Missing setup_record_switching")
	dim id as integer = *record_id_ptr

	defitem chr(27) & record_type_name & " " & id & chr(26)
	if process then
		if record_id_grabber() then
			edited = YES
		end if
	end if

	/'
	'Not using delete_action() because don't allow Backspace
	if process andalso keyval(scDelete) > 1 then
		crop_after_record
	end if
	'/
end sub

'===============================================================================
'                            def* menu item functions

sub EditorKit.defitem(title as zstring ptr)
	finish_defitem
	started_item = YES

	' Set all the per-item state variables
	selected = (state.pt = cur_item_index)
	hover = (state.hover = cur_item_index)

	'? "defitem " & cur_item_index & " " & *title
	refresh = (phase = Phases.refreshing)
	var maybe_process = selected andalso (phase = Phases.processing)
	' Hold Alt to edit the record number, except on a text field, where Alt
	' can be used to input text.
	process = maybe_process andalso (keyval(scAlt) = 0)
	process_text = maybe_process andalso can_use_strgrabber

	'activate = selected andalso (phase = Phases.activating)
	activate = process andalso want_activate
	left_click = process andalso (readmouse.clicks and mouseLeft)
	right_click = process andalso (readmouse.clicks and mouseRight)

	if combo_key1 then
		if combo_key2 then
			if keyval(combo_key1) andalso keyval(combo_key2) > 1 then activate = YES
		else
			if (keyval(scCtrl) or keyval(scShift) or keyval(scAlt)) = 0 andalso keyval(combo_key1) > 1 then activate = YES
		end if
		' Can't wipe these in finish_defitem so have to do it here
		combo_key1 = scNone
		combo_key2 = scNone
	end if

	' Start new item
	'if refresh then
		cur_item.destructor()
		cur_item.constructor()
		cur_item.id = cur_item_index
		if title andalso title[0] = asc("!") then
			cur_item.inverted_bool = YES
			' No value is set yet, don't need to invert it
			cur_item.title = *(title + 1)
		else
			cur_item.title = *title
		end if
	'end if

	'return cur_item_index
end sub

function EditorKit.defitem_act(title as zstring ptr) as bool
	defitem title
        if activate then state.need_update = YES
	return activate
end function

' Add an unselectable line. You can still use set_caption or even val_*/as_* to set a default caption
sub EditorKit.defunselectable(title as zstring ptr, color as integer = -eduiNote-1)
	defitem title
	cur_item.unselectable = YES
	cur_item.color = color
end sub

sub EditorKit.defdisabled(title as zstring ptr)
	defitem title
	set_disabled
end sub

sub EditorKit.defint(title as zstring ptr, byref datum as integer, min as integer = 0, max as integer)
	defitem title
	edit_int datum, min, max
end sub

sub EditorKit.defbool(title as zstring ptr, byref datum as bool)
	defitem title
	edit_bool datum
end sub

sub EditorKit.defbool(title as zstring ptr, byref datum as boolean)
	defitem title
	edit_bool datum
end sub

sub EditorKit.defbitset(title as zstring ptr, bitwords() as integer, wordnum as integer = 0, bitnum as integer)
	defitem title
	edit_bitset bitwords(), wordnum, bitnum
end sub

sub EditorKit.defstr(title as zstring ptr, byref datum as string, maxlen as integer = 0)
	defitem title
	edit_str datum, maxlen
end sub

'===============================================================================
'                                    Captions

' Set the display value for the current item, overriding default conversion of the
' value to a string.
sub EditorKit.set_caption(caption as zstring ptr)
	if refresh then
		cur_item.caption = *caption
	end if
end sub

' Internal
sub EditorKit.wrap_caption(caption as string)
	if value = cur_item.default_value then
		set_caption "Default (" & caption & ")"
	else
		set_caption caption
	end if
end sub

sub EditorKit.caption_default_or_int(default_value as integer = 0, default_caption as zstring ptr = @"Default")
	if refresh then
		'cur_item.default_value = default_value  'Not used for anything
		'cur_item.default_eff_value = MIN_INT    'Unknown
		cur_item.caption = iif(value = default_value, *default_caption, str(value))
	end if
end sub

sub EditorKit.caption_default_or_str(default_caption as zstring ptr = @"[default]")
	if refresh then
		cur_item.caption = iif(len(valuestr), valuestr, *default_caption)
	end if
end sub

sub EditorKit.captions_bool(nocapt as zstring ptr, yescapt as zstring ptr)
	if refresh then
		cur_item.caption = *iif(value, yescapt, nocapt)
	end if
end sub

' Shows "Invalid <thing> ##" if the value is out of bounds
sub EditorKit.captions(captions_array() as string, invalid_thing as zstring ptr = @"value")
	if refresh then
		cur_item.caption = safe_caption(captions_array(), value, *invalid_thing)
	end if
end sub

' Due to FB bug sf#666 (fixed in 1.09) it's not possible to define an overload of
' captions() which takes a zstring ptr array.
sub EditorKit.captionsz(captions_array() as zstring ptr, invalid_thing as zstring ptr = @"value")
	if refresh then
		cur_item.caption = safe_captionz(captions_array(), value, *invalid_thing)
	end if
end sub

' Shows value as an int if it's out of bounds
sub EditorKit.captions_or_int(captions_array() as string)
	if refresh then
		cur_item.caption = caption_or_int(captions_array(), value)
	end if
end sub

'===============================================================================
'                           Other menu item attributes

' Makes this menu item unselectable, but does not change its colour
sub EditorKit.set_unselectable()
	cur_item.unselectable = YES
end sub

' Disables editing and other processing, and changes to the disabled color.
' That means it has to precede edit_* methods, e.g. replace "defstr X, Y" with
'  defitem X : set_disabled : edit_str Y
sub EditorKit.set_disabled()
	if edited then
		showbug "editorkit: set_disabled must precede edit_*"
	end if
	cur_item.disabled = YES
	cur_item.color = NO  'Override previous set_color
	activate = NO
	left_click = NO
	right_click = NO
	' TODO: really shouldn't change process, instead add something like
	' `editing`/`editing_text` that most things check instead.
	process = NO
	process_text = NO
end sub

' The id isn't used for anything currently
sub EditorKit.set_id(id as integer)
	cur_item.id = id
end sub

' The color is a master palette index or -uicol - 1 for a UI constant.
' Overrides color from a previous set_disabled().
sub EditorKit.set_color(color as integer)
	cur_item.color = color
end sub

' Set which help page is opened by F1 while the current menu item is selected
' Also sets a default tooltip "F1 for details" while selected
sub EditorKit.set_helpkey(key as zstring ptr)
	if process then
		base.helpkey = *key
		cur_item.helpkey = *key
	end if
end sub

' Set text that appears at the bottom of the screen while this item is selected
' (TODO: if the mouse moves, depend on mouse hover instead)
sub EditorKit.set_tooltip(text as zstring ptr)
	if selected then base.tooltip = *text
end sub

' Sets a combo key to activate the next defitem. If used with a single key, check keyval(key1) > 1.
' If used with two keys, the first is a modifier: keyval(key1) > 0 and keyval(key2) > 1.
' Note: unlike all other attributes this must precede the menu item! So that 'activate' can be set.
sub EditorKit.keycombo(key1 as KBScancode, key2 as KBScancode = scNone)
	combo_key1 = key1
	combo_key2 = key2
end sub

' Set the effective value for purposes of previewing and captioning when `value`
' is equal to `default_value`, for example:
'  Line sound: default (6 Blip sfx)
' (This default is distinct from the default value of a missing Node.)
' NOTE: unlike other caption functions, must be called before edit_as_*/as_*!
sub EditorKit.default_effective_value(default_value as integer, effective_value as integer)
	' False alarm if set_caption called otherwise, but disallowing that doesn't matter
	BUG_IF(len(cur_item.caption), "must be called before as_*/edit_as_*")
	cur_item.default_value = default_value
	cur_item.default_eff_value = effective_value
end sub

' Get the effective value (see default_effective_value)
function EditorKit.eff_value() as integer
	if value = cur_item.default_value then
		return cur_item.default_eff_value
	else
		return value
	end if
end function

' Call this after edit_str (or defstr or any other string type) to allow multiline editing.
' Call set_helpkey before this to make it use that helpkey inside.
function EditorKit.multiline_editable() as bool
	' Note that edit_str ensures Space won't activate
	if activate then
		valuestr = multiline_string_editor(valuestr, cur_item.helpkey, NO)  'prompt_to_save=NO
		edited = YES
		write_value
		return YES
	end if
end function

' Don't attempt to write back the value, e.g. because it's been written manually
' and value/valuestr/etc is stale.
sub EditorKit.dont_write()
	cur_item.dtype = dtypeNone
	cur_item.writer = writerNone
end sub


'===============================================================================
'                          val_* value definition functions

' Functions to tell which piece of data is associated with the menu item.
' These set value/valuestr/valuefloat, and record its dtype and the writer for it.
' These behave very similiarly to as_* functions, except those set the caption
' immediately if not already (val_* only set the default caption), so must be
' called after offset_int.

'------------------------------ Value modifiers --------------------------------

' Value modifiers can be called either before or after val_*, but must be called
' before edit_* or as_* or setting the caption!

' Cause `value` to be offset from the underlying data field.
sub EditorKit.offset_int(offset as integer)
	assert(cur_item.dtype = dtypeNone orelse cur_item.dtype = dtypeInt)
	assert(len(cur_item.caption) = 0)
	assert(edited = NO)
	cur_item.offset = offset
	' If val_* hasn't been called yet this has no effect because it'll be clobbered
	value += offset
end sub

' Convenience wrapper for one-line definitions like:
'   edit_as_enemy offset_int(1, rec(42)), Or_None
' But note you MUST NOT use this with defint!!
function EditorKit.offset_int(offset as integer, byref datum as integer) as integer
	offset_int offset
	return val_int(datum)
end function

' Invert the meaning of a bit from its underlying data field.
' As a shortcut you can prefix the menu item title with ! instead, like
'  defbitset "!Inns revive dead heroes", bits(), , 4
sub EditorKit.invert_bool()
	assert(cur_item.dtype = dtypeNone orelse cur_item.dtype = dtypeBool)
	assert(len(cur_item.caption) = 0)
	assert(edited = NO)
	cur_item.inverted_bool = YES
	' If val_* hasn't been called yet this has no effect because it'll be clobbered
	value xor= YES
end sub

' Convenience wrapper for one-line definitions like:
'   defitem "Translucent:"
'   edit_bool invert_bool(box.opaque)
' But note you MUST NOT use this with defbool!!
function EditorKit.invert_bool(byref datum as bool) as bool
	invert_bool
	return val_bool(datum)
end function

'------------------------------- Primitive types -------------------------------

' These primitive val_* functions apply data modifiers such as .offset and
' .invert_bool only if .dtype = dtypeNone, and set .writer only if it's
' writerNone. So other val_* functions should set .writer (if not None) but not
' .dtype, then call these primitives with 'value' to apply modifiers.
' Don't overwrite .writer if already set, because val_* functions can be called
' repeatedly, such as from inside edit_*.

function EditorKit.val_int(byref datum as integer) as integer
	value = datum
	with cur_item
		if .dtype = dtypeNone then
			' Need to make sure we only do this once!
			value += .offset
		end if
		.dtype = dtypeInt
		if .writer = writerNone then
			.writer = writerInt
			.int_ptr = @datum
		end if
	end with
	return value
end function

function EditorKit.val_bool(byref datum as bool) as bool
	value = (datum <> 0)
	with cur_item
		if .dtype = dtypeNone then
			' Need to make sure we only do this once!
			value xor= .inverted_bool
		end if
		.dtype = dtypeBool
		if .writer = writerNone then
			.writer = writerInt
			.int_ptr = @datum
		end if
	end with
	return value
end function

function EditorKit.val_bool(byref datum as boolean) as bool
	value = datum
	with cur_item
		if .dtype = dtypeNone then
			' Need to make sure we only do this once!
			value xor= .inverted_bool
		end if
		.dtype = dtypeBool
		if .writer = writerNone then
			.writer = writerBoolean
			.byte_ptr = @datum
		end if
	end with
	return value
end function

function EditorKit.val_bit(byref bits as integer, whichbit as integer) as bool
	value = (bits and whichbit) <> 0
	with cur_item
		if .dtype = dtypeNone then
			' Need to make sure we only do this once!
			value xor= .inverted_bool
		end if
		.dtype = dtypeBool
		if .writer = writerNone then
			.writer = writerBit
			.int_ptr = @bits
			.whichbit = whichbit
		end if
	end with
	return value
end function

function EditorKit.val_bitset(bitwords() as integer, wordnum as integer = 0, bitnum as integer) as bool
	' It's a safe bet bitwords() won't be redimmed
	'value = readbit(bitwords(), wordnum, bitnum) <> 0
	'... setbit bitwords(), wordnum, bitnum, value
	return val_bit(bitwords(wordnum + bitnum \ 16), 1 shl (bitnum mod 16))
end function

function EditorKit.val_str(byref datum as string) as string
	valuestr = datum
	with cur_item
		.dtype = dtypeStr
		if .writer = writerNone then
			.writer = writerStr
			.str_ptr = @datum
		end if
	end with
	return datum
end function

function EditorKit.val_float(byref datum as double) as double
	valuefloat = datum
	with cur_item
		.dtype = dtypeFloat
		if .writer = writerNone then
			.writer = writerDouble
			.double_ptr = @datum
		end if
	end with
	return datum
end function

'-------------------------------- Derived types --------------------------------

/'
' WARNING: options() will point to keys() strings! This is pretty dangerous so I'll comment it.
sub make_stringenum_array(options() as StringEnumOption, keys() as string)
	redim options(lbound(keys) to ubound(keys))
	for idx as integer = lbound(options) to ubound(options)
		options(idx).key = @keys(idx)
	next
end sub
'/

function find_enum_index(key as string, options() as StringEnumOption) as integer
	for idx as integer = lbound(options) to ubound(options)
		if *options(idx).key = key then
			return idx
		end if
	next
	return lbound(options) - 1
end function

' A string which takes one of a fixed set of allowed values, each of which may have a
' caption for display.
' If the string is blank but "" isn't an allowed value then it's initialised to options(0).key.
' lbound(options) can be a value other than 0.
function EditorKit.val_str_enum(byref datum as string, options() as StringEnumOption) as string
	val_str datum
	if refresh or process then
		' Ensure valuestr is valid and set caption
		dim index as integer = find_enum_index(valuestr, options())
		if index < lbound(options) then
			if len(valuestr) = 0 then
				' Apparently uninitialised, but "" is not one of the allowed
				' values, so initialise it to first option.
				' (Not using the default value for writerNodePathStr)
				if lbound(options) <= 0 andalso ubound(options) >= 0 then
					valuestr = *options(0).key
					index = 0
					edited = YES
				end if
			else
				' Invalid value, maybe from a future engine version. Don't touch it!
				if len(cur_item.caption) = 0 then
					set_caption "Unknown value: " & valuestr
				end if
			end if
		end if
		if len(cur_item.caption) = 0 andalso index >= lbound(options) then
			with options(index)
				set_caption iif(len(*.caption), .caption, .key)
			end with
		end if
	end if
	return valuestr
end function

'-------------------------------- RELOAD Nodes ---------------------------------

function EditorKit.val_node_int(node as Node ptr) as integer
	with cur_item
		if .writer = writerNone then
			.writer = writerNodeInt
			.node = node
		end if
	end with
	return val_int(GetInteger(node))  'Adds .offset
end function

' Note: `default` is the default value for a missing node, *before* adding any
' offset (so the default for `value` is `default + offset`)
function EditorKit.val_node_int(root as Node ptr, path as zstring ptr, default as integer = 0, delete_if_default_flag as EKFlags = 0) as integer
	' Wrap this in "if refresh or process or hover then"?
	with cur_item
		if .writer = writerNone then
			.writer = writerNodePathInt
			.node = root
			.path = *path
			.writer_default_int = default
			.delete_default = (delete_if_default_flag = Delete_If_Default)
		end if
	end with
	dim node as Node ptr = NodeByPath(root, path)
	value = iif(node, GetInteger(node), default)
	return val_int(value)  'Adds .offset
end function

' A Node written with value 0 or 1 (to match some existing RELOAD file formats)
function EditorKit.val_node_bool(node as Node ptr) as bool
	with cur_item
		if .writer = writerNone then
			.writer = writerNodeBool
			.node = node
		end if
	end with
	return val_bool(GetInteger(node))
end function

' Note: `default` is the default value for a missing node, *before* applying invert_bool
function EditorKit.val_node_bool(root as Node ptr, path as zstring ptr, default as bool = NO) as bool
	with cur_item
		if .writer = writerNone then
			.writer = writerNodePathBool
			.node = root
			.path = *path
			.delete_default = NO
		end if
	end with
	' Wrap this in "if refresh or process or hover then"?
	dim node as Node ptr = NodeByPath(root, path)
	value = iif(node, GetInteger(node), default)
	return val_bool(value)
end function

function EditorKit.val_node_str(node as Node ptr) as string
	with cur_item
		if .writer = writerNone then
			.writer = writerNodeStr
			.node = node
		end if
	end with
	return val_str(GetString(node))
end function

function EditorKit.val_node_str(root as Node ptr, path as zstring ptr, default as zstring ptr = @"", delete_if_default_flag as EKFlags = 0) as string
	with cur_item
		if process andalso .writer = writerNone then
			.writer = writerNodePathStr
			.node = root
			.path = *path
			.writer_default_str = *default
			.delete_default = (delete_if_default_flag = Delete_If_Default)
		end if
	end with
	dim node as Node ptr = NodeByPath(root, path)
	valuestr = iif(node, GetString(node), *default)
	return val_str(valuestr)
end function

function EditorKit.val_node_float(node as Node ptr) as double
	with cur_item
		if .writer = writerNone then
			.writer = writerNodeFloat
			.node = node
		end if
	end with
	return val_float(GetFloat(node))
end function

function EditorKit.val_node_float(root as Node ptr, path as zstring ptr, default as double = 0., delete_if_default_flag as EKFlags = 0) as double
	with cur_item
		if process andalso .writer = writerNone then
			.writer = writerNodePathFloat
			.node = root
			.path = *path
			.writer_default_float = default
			.delete_default = (delete_if_default_flag = Delete_If_Default)
		end if
	end with
	dim node as Node ptr = NodeByPath(root, path)
	return val_float(iif(node, GetFloat(node), default))
end function

function EditorKit.val_node_exists(root as Node ptr, path as zstring ptr) as bool
	with cur_item
		if process andalso .writer = writerNone then
			.writer = writerNodePathExists
			.node = root
			.path = *path
		end if
	end with
	return val_bool(NodeByPath(root, path) <> NULL)
end function

'------------------------- gen() and general.reld data -------------------------

' TODO

'-------------------------- .ini config file settings --------------------------

' Note: `default` is the default value for a missing setting, *before* applying invert_bool
function EditorKit.val_config_bool(path as zstring ptr, default as bool = NO) as bool
	' TODO: we ought to cache the config value and only read it when refreshing
	with cur_item
		if .writer = writerNone then
			' TODO: what about writing it with optional .edit prefix?
			.writer = writerConfigBool
			.path = *path
		end if
	end with
	return val_bool(read_config_bool(path, default))
end function

' TODO

'===============================================================================
'                                 Data editing

'------------------------------- Primitive types -------------------------------

function EditorKit.edit_int(byref datum as integer, min as integer, max as integer) as bool
	val_int datum
	if process then
		edited or= intgrabber(value, min, max)
		if edited then write_value
	end if
	return edited
end function

function EditorKit.edit_bool(byref datum as bool) as bool
	val_bool datum
	' Note: boolgrabber checks enter_space_click, and sets state.need_update
	if process orelse activate then
		' TODO: boolgrabber doesn't support Left/Right keys or anything else where inversion matters
		edited or= boolgrabber(value, state)
		if edited then write_value
	end if
	return edited
end function

function EditorKit.edit_bool(byref datum as boolean) as bool
	val_bool datum
	return edit_bool(value)
end function

' Editing a bit of an integer variable. (value will be true/YES or false/NO)
function EditorKit.edit_bit(byref bits as integer, whichbit as integer) as bool
	val_bit bits, whichbit
	' It's simpler to reuse edit_bool than to create a method that uses bitgrabber
	return edit_bool(value)
end function

' Editing a bit in an array of shorts. (value will be true/YES or false/NO)
function EditorKit.edit_bitset(bitwords() as integer, wordnum as integer = 0, bitnum as integer) as bool
	val_bitset bitwords(), wordnum, bitnum
	' It's simpler to reuse edit_bool than to create a method that uses bitsetgrabber
	return edit_bool(value)
end function

' See also multiline_editable()
function EditorKit.edit_str(byref datum as string, maxlen as integer = 0) as bool
	val_str datum
	if process_text then
		using_strgrabber = YES  'Disable select-by-typing
		edited or= strgrabber(valuestr, iif(maxlen, maxlen, 9999999))
		if edited then write_value
	end if
	if process then
		' Special case: SPACE should never activate a menu item that also takes text input
		' such as one that's multiline_editable.
		if keyval(scSpace) then activate = NO
	end if
	return edited
end function

'-------------------------------- Derived types --------------------------------

' For integers where values < 0 are special values and -1 is None. Like
' zintgrabber but not offset by 1: you have to use offset_int for that.
' When value < 0, you can type in a number as if value = 0.
' Backspace/Delete on value <= 0 goes to -1.
function EditorKit.edit_zint(byref datum as integer, min as integer, max as integer) as bool
	val_int datum
	if process then
		value += 1
		edited or= zintgrabber(value, min, max)
		value -= 1
		if edited then write_value
	end if
	return edited
end function

' Returns whether key was modified
function prompt_for_enum(byref key as string, prompt_text as string, options() as StringEnumOption, helpkey as string) as bool
	dim menu() as string
	dim start_idx as integer
	for idx as integer = 0 TO ubound(options)
		with options(idx)
			a_append menu(), iif(len(*.caption), .caption, .key)
			if key = *.key then start_idx = idx
		end with
	next
	dim choice as integer
	choice = popup_choice(prompt_text, menu(), start_idx, -1, helpkey)
	if choice = -1 then return NO
	key = *options(choice).key
	return YES
end function

' String enumerations: selection of a string value only from a set of allowed values.
' If the string is blank but "" isn't an allowed value then it's initialised to options(0).key.
' lbound(options) can be a value other than 0.
function EditorKit.edit_str_enum(byref datum as string, options() as StringEnumOption) as bool
	val_str_enum datum, options()
	if activate then
		edited or= prompt_for_enum(valuestr, "", options(), cur_item.helpkey)
	elseif process then
		dim index as integer = find_enum_index(valuestr, options())
		' If valuestr is not in options() then index = lbound - 1, and we preserve
		' it instead of clamping to lbound, but the user can still edit it.
		if intgrabber(index, small(index, lbound(options)), ubound(options)) then
			valuestr = *options(index).key
			edited = YES
		end if
	end if
	if edited then write_value
	return edited
end function

'-------------------------------- RELOAD Nodes ---------------------------------

function EditorKit.edit_node_int(node as Node ptr, min as integer = 0, max as integer) as bool
	val_node_int node
	return edit_int(value, min, max)
end function

' Note: `default` is the default value for a missing node, *before* applying offset_int
' Delete_If_Default flag: if passed, delete the node if it's set to the default value
function EditorKit.edit_node_int(root as Node ptr, path as zstring ptr, default as integer = 0, min as integer = 0, max as integer, delete_if_default_flag as EKFlags = 0) as bool
	val_node_int root, path, default, delete_if_default_flag
	return edit_int(value, min, max)
end function

function EditorKit.edit_node_bool(node as Node ptr) as bool
	val_node_bool node
	return edit_bool(value)
end function

' Note: `default` is the default value for a missing node, *before* applying invert_bool
function EditorKit.edit_node_bool(root as Node ptr, path as zstring ptr, default as integer = 0) as bool
	val_node_bool root, path, default
	return edit_bool(value)
end function

function EditorKit.edit_node_str(node as Node ptr, maxlen as integer = 0) as bool
	val_node_str node
	return edit_str(valuestr, maxlen)
end function

' If Delete_If_Default flag is passed, delete the node if it's set to the default value
function EditorKit.edit_node_str(root as Node ptr, path as zstring ptr, default as zstring ptr = @"", maxlen as integer = 0, delete_if_default_flag as EKFlags = 0) as bool
	val_node_str root, path, default, delete_if_default_flag
	return edit_str(valuestr, maxlen)
end function

' Toggle whether a node exists
function EditorKit.edit_node_exists(node as Node ptr, path as zstring ptr) as bool
	val_node_exists node, path
	return edit_bool(value)
end function

' Delete a Node if user presses delete/backspace, with optional prompt.
' If node is null then the node specified to the previous val/edit_node_* function is used.
' You can pass a Node ptr for non-
' If thingname is null then no prompt will be shown.
' Doesn't update value/valuestr/valuefloat.
sub EditorKit.deletable_node(node as Node ptr = NULL, thingname as zstring ptr = NULL)
	if delete_action() then
		if thingname = NULL orelse yesno("Really delete this " & *thingname & "?", NO, NO) then
			if node then
				if cur_item.node = null then
					cur_item.writer = writerNodeInt  'Dummy
					cur_item.node = node
				else
					assert(node = cur_item.node)
				end if
			end if
			delete_node
		end if
	end if
end sub

' Deletes the node specified by any val_node_* function, if it exists
sub EditorKit.delete_node()
	with cur_item
		select case as const .writer
			case writerNodeInt, writerNodeBool, writerNodeStr, writerNodeFloat
				FreeNode .node
			case writerNodePathInt, writerNodePathBool, writerNodePathStr, writerNodePathFloat, writerNodePathExists
				dim valnode as Node ptr
				valnode = NodeByPath(.node, .path)
				if valnode then FreeNode valnode
			case else
				showbug "EditorKit.delete_node: not a node datatype!"
		end select
		dont_write
	end with
	edited = YES
end sub

'----------------------------- general.reld data ------------------------------

' TODO

'-------------------------- .ini config file settings --------------------------

' Note: `default` is the default value for a missing setting, *before* applying invert_bool
function EditorKit.edit_config_bool(path as zstring ptr, default as bool = NO) as bool
	val_config_bool path, default
	return edit_bool(value)
end function

' TODO: Many more

'===============================================================================
'                     Game data type definitions & editing

' It's OK for edit_as_* functions to call as_* first, setting the caption before
' editing the value, because refreshing and processing are separate phases.

'------------------------------------ Tags -------------------------------------

' If you need more control over the captions, you can call tag_*_caption directly.
' allowspecial: if true, don't warn about picking autoset tags.

' Caption: "<prefix> #=ON/OFF (<tagname>)" where <tagname> is <zerocap> or
' "Never"/"Always" for tags 0/1.  You may want to pass zerocap="Always".
sub EditorKit.as_check_tag(byref datum as integer, prefix as zstring ptr = @"Tag", zerocap as zstring ptr = @"None")
	val_int datum
	if refresh andalso len(cur_item.caption) = 0 then
		wrap_caption tag_condition_caption(eff_value, *prefix, *zerocap)
	end if
end sub

' For a tag=on/off check.
function EditorKit.edit_as_check_tag(byref datum as integer, prefix as zstring ptr = @"Tag", zerocap as zstring ptr = @"None", allowneg as bool = YES) as bool
	as_check_tag datum, prefix, zerocap
	if process then
		edited or= tag_grabber(value, state, YES, NO, allowneg)  'allowspecial=YES, always_choice=NO
		if edited then write_value
	end if
	return edited
end function

' Caption: "<prefix> #=ON/OFF [AUTOSET] (<tagname>)" where <tagname> is
' "No tag set" or "Unchangeable" for tags 0/1.
sub EditorKit.as_set_tag(byref datum as integer, prefix as zstring ptr = @"Set tag", allowspecial as bool = NO)
	val_int datum
	if refresh andalso len(cur_item.caption) = 0 then
		wrap_caption tag_set_caption(eff_value, *prefix, allowspecial)
	end if
end sub

' For setting a tag or defining an autoset tag (for autosets use allowspecial=YES, allowneg=NO).
function EditorKit.edit_as_set_tag(byref datum as integer, prefix as zstring ptr = @"Set tag", allowspecial as bool = NO, allowneg as bool = YES) as bool
	as_set_tag datum, prefix, allowspecial
	if process then
		' With our default args, equivalent to tag_set_grabber
		edited or= tag_grabber(value, state, allowspecial, , allowneg)
		if edited then write_value
	end if
	return edited
end function

' Caption: "<prefix> # [AUTOSET] (<tagname>)" where <tagname> is "None" or
' "Unchangeable" for tags 0/1.
sub EditorKit.as_tag_id(byref datum as integer, prefix as zstring ptr = @"Tag", allowspecial as bool = NO)
	val_int datum
	if refresh andalso len(cur_item.caption) = 0 then
		wrap_caption tag_choice_caption(eff_value, *prefix, allowspecial)  'no zerocap arg
	end if
end sub

' For selecting a tag ID without negative values or "=ON/OFF" in the caption, e.g. for toggling a tag.
function EditorKit.edit_as_tag_id(byref datum as integer, prefix as zstring ptr = @"Tag", allowspecial as bool = NO) as bool
	as_tag_id datum, prefix, allowspecial
	if process then
		' This differs from tag_id_grabber, which may be misnamed
		edited or= tag_grabber(value, state, allowspecial, NO, NO)  'always_choice=NO, allowneg=NO
		if edited then write_value
	end if
	return edited
end function

'----------------------------------- Sprites -----------------------------------

sub EditorKit.as_spriteset(byref datum as integer, or_none_flag as EKFlags = 0)
	val_int datum
	if refresh andalso len(cur_item.caption) = 0 then
		var id = eff_value
		if id = -1 andalso (or_none_flag = Or_None) then
			wrap_caption "None"
		elseif id < 0 then
			wrap_caption "Invalid spriteset " & id
		end if
	end if
end sub

function EditorKit.edit_as_spriteset(byref datum as integer, spr_type as SpriteType, or_none_flag as EKFlags = 0) as bool
	BUG_IF(spr_type < 0 or spr_type > ubound(sprite_sizes), "Bad spr_type", NO)
	as_spriteset datum, or_none_flag
	if activate then
		dim spriteb as SpriteOfTypeBrowser
		value = spriteb.browse(value, or_none_flag = Or_None, spr_type)
		edited = YES
	else
		edit_zint value, iif(or_none_flag = Or_None, -1, 0), sprite_sizes(spr_type).lastrec
	end if
	if edited then write_value
	return edited
end function

' -1 means default
sub EditorKit.as_palette(byref datum as integer)
	val_int datum
	if refresh andalso len(cur_item.caption) = 0 then
		var id = eff_value
		if id = -1 then
			wrap_caption "Default"
		elseif id < 0 then
			wrap_caption "Invalid palette " & id
		end if
	end if
end sub

' Always allows -1 as default (there's no existing data field that doesn't allow a default palette)
function EditorKit.edit_as_palette(byref datum as integer, spr_type as SpriteType = sprTypeInvalid, spr_set as integer = 0) as bool
	BUG_IF(spr_type < 0 or spr_type > ubound(sprite_sizes), "Bad spr_type", NO)
	as_palette datum
	' Can't enter the browser without a spriteset to preview
	if activate andalso spr_type <> sprTypeInvalid then
		value = pal16browse(value, spr_type, spr_set, YES)  'show_default = YES
		edited = YES
	else
		edit_zint value, -1, gen(genMaxPal)
	end if
	if edited then write_value
	return edited
end function

'------------------------------------ Audio ------------------------------------

' -1 is Silence, but -2 often also has a special meaning which you need to set with set_caption
sub EditorKit.as_song(byref datum as integer)
	val_int datum
	if refresh andalso len(cur_item.caption) = 0 then
		var id = eff_value
		if id < 0 then  '-1
			wrap_caption "Silence"
		else
			wrap_caption id & " " & getsongname(id)
		end if
	end if
end sub

' Assumes min <= -1 (otherwise would need to call song_picker instead)
function EditorKit.edit_as_song(byref datum as integer, min as integer = -1, preview_audio_flag as EKFlags = 0) as bool
	as_song datum
	if activate then
		value = song_picker_or_none(value + 1) - 1  'Is offset by 1 from song_picker
		edited = YES
	else
		edit_zint value, min, gen(genMaxSong)
	end if
	if edited then
		if preview_audio_flag = Preview_Audio andalso eff_value >= 0 then
			' playsongnum doesn't stop the music if you play a nonexistent song.
			music_stop
			playsongnum eff_value
		else
			music_stop
		end if
	end if
	if edited then write_value
	return edited
end function

sub EditorKit.as_sfx(byref datum as integer)
	val_int datum
	if refresh andalso len(cur_item.caption) = 0 then
		var id = eff_value
		if id < 0 then  '-1
			wrap_caption "None"
		else
			wrap_caption id & " " & getsfxname(id)
		end if
	end if
end sub

' Assumes min <= -1 (otherwise would need to call sfx_picker instead)
function EditorKit.edit_as_sfx(byref datum as integer, min as integer = -1, preview_audio_flag as EKFlags = 0) as bool
	as_sfx datum
	if activate then
		value = sfx_picker_or_none(value + 1) - 1  'Is offset by 1 from sfx_picker
		edited = YES
	else
		edit_zint value, min, gen(genMaxSFX)
	end if
	if edited then
		if preview_audio_flag = Preview_Audio andalso eff_value >= 0 then
			resetsfx  'Stop previous sound
			playsfx eff_value
		else
			resetsfx
		end if
	end if
	if edited then write_value
	return edited
end function

'----------------------------------- Enemies -----------------------------------

' Or_None: -1 is None
sub EditorKit.as_enemy(byref datum as integer, or_none_flag as EKFlags = 0)
	val_int datum
	if refresh andalso len(cur_item.caption) = 0 then
		var id = eff_value
		if id = -1 andalso (or_none_flag = Or_None) then
			wrap_caption "None"
		elseif id < 0 then
			wrap_caption "Invalid enemy " & id
		else
			dim enemy as EnemyDef
			loadenemydata enemy, id
			wrap_caption id & " " & enemy.name
		end if
	end if
end sub

function EditorKit.edit_as_enemy(byref datum as integer, or_none_flag as EKFlags = 0) as bool
	as_enemy datum, or_none_flag
	if process then
		' TODO: offset and min args probably wrong
		edited or= enemygrabber(value, state, iif(or_none_flag = Or_None, 1, 0), 0)
		if edited then write_value
	end if
	return edited
end function

'------------------------------ Extra data vectors -----------------------------

'Adds a set of menu items for editing an extra data vector
sub EditorKit.edit_extra_data_vector(byref extravec as integer vector)
	dim length as integer = iif(extravec, v_len(extravec), 3)
	defitem "Length:"
	if edit_int(length, 0, maxExtraLength) then
		resize_extra extravec, length
	end if
	for i as integer = 0 to small(10, length) - 1
		defitem "extra " & i & ":"
		dim datum as integer = get_extra(extravec, i)
		if edit_int(datum, INT_MIN, INT_MAX) then
			set_extra extravec, i, datum
		end if
	next
	if length > 10 then
		defunselectable "..."
	end if
	if defitem_act("View/edit all extra data...") then
		extra_data_editor extravec
	end if
end sub
