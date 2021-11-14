'OHRRPGCE - EditorKit framework for creating editors
'(C) Copyright 1997-2021 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

' To create an editor with EditorKit, create a UDT Extending EditorKit, and
' implement sub define_items(). This will be called repeatedly for:
' refreshing: generate an array of items for display and navigation; called
'   whenever state.need_update is true (which define_items sets during processing)
' processing: define_items() is called every tick to handle editing of data fields
' activating: clicking or space/enter activating a menu item (actually the same as
'   the processing phase)
'
' Then call .run(). Loading, other initialisation, and saving are your own
' responsibility, for now.

' To add a menu item, call from define_items():
' -spacer: a blank line
' -section or subsection: add a section header, which is unselectable. In future,
'  sections will be collapsible
' -defitem, or other def* function (which are convenience wrappers around defitem)
'
' Each defitem menu item is split into a "title" and a "caption" (either of
' which may be blank).  The title is the field description and should usually
' end in ':'; the caption presents the value. The title is set only by defitem.
'
' A menu item definition starts with defitem and ends at the next defitem/spacer/
' etc. call. Inside the definition you can query some bool members:
' -selected: this menu item is the selected item
' -refresh: refreshing the menu
' -process: selected and should do per-tick logic, such as calling intgrabber
' -activate: selected and should be activated (if possible), e.g. enter a submenu.
' -hover: mouse over this item
'
' After defitem you can declare a datum that the menu item edits, using an
' edit_* method, eg:
'   defitem "Number of Hits:"
'   edit_int rec(17), 1, 20   'Range 1 to 20
' Or use is_* to display data without editing it.
' Or write some custom code to display "if refresh" and/or edit "if process".
'
' You can read the value/valuestr/valuefloat members after declaring a datum
' with edit_*/is_*. These are also used by the functions for setting the
' caption.  You don't need to set the value if you write custom editing code "if
' process" and use set_caption for the caption.
' The caption defaults to the item's value if the title ends in ':'.  Ite can be
' set it with set_caption, or a caption* function such as `captions` for enum
' strings.  caption* functions (other than set_caption) must be called after
' value/valuestr/valuefloat is set!

/' Examples
	'! prefix to invert the display of a bit/bool/bitset
	dim bits() as integer
	defbitset "!Enable debugging keys", bits(), , 8

	dim root as Node ptr
	dim somenode as Node ptr '= root."foo"."bar".ptr

	defitem "A RELOAD node:"
	edit_node_int somenode, 0, 1000

	defitem "A RELOAD node:"
	edit_nodepath_int root, "/foo/bar", 0, 100

        ' Not implemented
	'defitem "A RELOAD node:"
	'' A Node which gets deleted when equal to 0
	'edit_nodepath_opt_int root, "/foo/bar", 0, 100
'/

#include "config.bi"
#include "common.bi"
#include "reloadext.bi"
#include "editorkit.bi"
#include "customsubs.bi"

using Reload.Ext


'===============================================================================
'                         ModularMenu hooks (entry points)

sub EditorKit.update()
	' On the first call store helpkey, as it may get clobbered
	if len(default_helpkey) = 0 then default_helpkey = helpkey

	run_phase(Phases.refreshing)
end sub

function EditorKit.each_tick() as bool
	helpkey = default_helpkey
	want_exit = NO

	want_activate = enter_space_click(state)
	run_phase(Phases.processing)

	'if enter_space_click(state) then
	'	activated_item = state.pt
	'	state.need_update or= run_phase(Phases.activating)
	'end if
	'if state.need_update = NO then
	'	run_phase(Phases.processing)
	'end if

	if want_exit andalso try_exit() then return YES
	return NO
end function


'===============================================================================
'                                   Internal

constructor EditorKit()
	menuopts.itemspacing = 1
end constructor

' Wrapper around define_items()
sub EditorKit.run_phase(which_phase as Phases)
	phase = which_phase
	cur_item_index = 0
	started_item = NO

	refresh = (phase = Phases.refreshing)
	process = NO
	activate = NO

	if refresh then clear_menu

	defitem prev_menu_title
	if activate then want_exit = YES

	define_items()
	' End the final item
	finish_defitem
end sub

' Called after an item definition is finished
sub EditorKit.finish_defitem()
	if started_item = NO then exit sub

	if activate then
		' If you enter a submenu and then exit it by hitting ESC/etc then we
		' need to ignore that cancel key or this menu will exit.
		' (Ideally would call setkeys regardless of how you exit the submenu,
		' but it's not possible to tell that we entered one, and if you exit
		' it any other way there doesn't seem to be a possibility of
		' misinterpreting input.)
		if keyval(ccCancel) > 1 then setkeys
	end if

	cur_item_index += 1

	if refresh then
		with cur_item
			dim as string title = .title, caption = .caption
			' If there's no caption, use the current data value as a default
			if len(caption) = 0 andalso ends_with(.title, ":") then
				select case .kind
 					case edkindBool:     caption = yesorno(value)
 					case edkindInteger:  caption = str(value)
					case edkindFloat:    caption = str(valuefloat)
					case edkindString:   caption = valuestr
				end select
			end if

			if len(title) > 0 then
				title &= " "
			end if

			base.add_item .id, 0, title & caption, (.unselectable = NO)
		end with
	end if

	started_item = NO
end sub

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
	process = selected andalso (phase = Phases.processing)
	'activate = selected andalso (phase = Phases.activating)
	activate = selected andalso (phase = Phases.processing) andalso want_activate

	' Start new item
	'if refresh then
		cur_item.destructor()
		cur_item.constructor()
		cur_item.id = cur_item_index
		cur_item.title = *title
	'end if

	'return cur_item_index
end sub

function EditorKit.defitem_act(title as zstring ptr) as bool
	defitem title
        if activate then state.need_update = YES
	return activate
end function

' Add an unselectable line. You can still use set_caption or even as_int, etc.
sub EditorKit.defunselectable(title as zstring ptr)
	defitem title
	cur_item.unselectable = YES
end sub

sub EditorKit.defint(title as zstring ptr, byref datum as integer, min as integer = 0, max as integer)
	defitem title
	edit_int datum, min, max
end sub

sub EditorKit.defbool(title as zstring ptr, byref datum as bool)
	defitem title
	edit_bool datum
end sub

sub EditorKit.defbitset(title as zstring ptr, bitwords() as integer, wordnum as integer = 0, bitnum as integer)
	defitem title
	edit_bitset bitwords(), wordnum, bitnum
end sub

sub EditorKit.defstring(title as zstring ptr, byref datum as string, maxlen as integer = 0)
	defitem title
	edit_string datum, maxlen
end sub

sub EditorKit.defgen_int(title as zstring ptr, genidx as integer, min as integer = 0, max as integer)
	defitem title
	edit_gen_int genidx, min, max
end sub

'===============================================================================
'                                as_* functions

sub EditorKit.as_int(byref datum as integer)
	cur_item.kind = edkindInteger
	value = datum
end sub

sub EditorKit.as_bool(byref datum as bool)
	cur_item.kind = edkindBool
	value = datum
end sub

sub EditorKit.as_string(byref datum as string)
	cur_item.kind = edkindString
	valuestr = datum
end sub

sub EditorKit.as_float(byref datum as double)
	cur_item.kind = edkindFloat
	valuefloat = datum
end sub

'===============================================================================
'                           Data definition & editing

'------------------------------- Primitive types -------------------------------

function EditorKit.edit_int(byref datum as integer, min as integer, max as integer) as bool
	cur_item.kind = edkindInteger
	value = datum
	if process andalso intgrabber(datum, min, max) then
		value = datum
		state.need_update = YES
		return YES
	end if
end function

function EditorKit.edit_bool(byref datum as bool) as bool
	cur_item.kind = edkindBool
	dim invert as bool = (cur_item.title[0] = asc("!"))
	datum xor= invert  'Doesn't actually make any difference yet

	' Note: boolgrabber checks enter_space_click, and sets state.need_update
	dim ret as bool
	if process orelse activate then
		' TODO: boolgrabber doesn't support Left/Right keys or anything else where inversion matters
		if boolgrabber(datum, state) then
			state.need_update = YES
			ret = YES
		end if
	end if
	datum xor= invert
	value = datum
	return ret
end function

' Editing a bit of an integer variable. (value will be true/YES or false/NO)
function EditorKit.edit_bit(byref bits as integer, whichbit as integer) as bool
	'It's simpler to reuse edit_bool than to create a method that uses bitgrabber
	value = (bits and whichbit) <> 0
	if edit_bool(value) then
		bits xor= whichbit
		return YES
	end if
end function

' Editing a bit in an array of shorts. (value will be true/YES or false/NO)
function EditorKit.edit_bitset(bitwords() as integer, wordnum as integer = 0, bitnum as integer) as bool
	'It's simpler to reuse edit_bool than to create a method that uses bitsetgrabber
	value = readbit(bitwords(), wordnum, bitnum) <> 0
	if edit_bool(value) then
		setbit bitwords(), wordnum, bitnum, value
		return YES
	end if
end function

function EditorKit.edit_string(byref datum as string, maxlen as integer = 0) as bool
	cur_item.kind = edkindString
	if process andalso can_use_strgrabber then
		using_strgrabber = YES  'Disabled select-by-typing
		if strgrabber(datum, iif(maxlen, maxlen, 9999999)) then
			valuestr = datum
			state.need_update = YES
			return YES
		end if
	end if
	valuestr = datum
end function

'------------------------- gen() and general.reld data -------------------------

' An integer in gen()
function EditorKit.edit_gen_int(genidx as integer, min as integer = 0, max as integer) as bool
	return edit_int(gen(genidx), min, max)
end function

'-------------------------- .ini config file settings --------------------------

function EditorKit.edit_config_bool(path as zstring ptr, default as bool = NO) as bool
	' TODO: we ought to cache the config value and only read it when refreshing
	value = read_config_bool(path, default)
	if edit_bool(value) then
		write_config "thingbrowser.enable_top_level", yesorno(value)
		return YES
	end if
end function

'-------------------------------- RELOAD nodes ---------------------------------

' No default, because the Node ptr can't be NULL
function EditorKit.edit_node_int(node as Node ptr, min as integer = 0, max as integer) as bool
	value = GetInteger(node)
	if edit_int(value, min, max) then
		SetContent(node, value)
		return YES
	end if
end function

' No default, because the Node ptr can't be NULL
function EditorKit.edit_node_string(node as Node ptr, maxlen as integer = 0) as bool
	valuestr = GetString(node)
	if edit_string(valuestr) then
		SetContent(node, valuestr)
		return YES
	end if
end function

' delete_default: if true, delete the node if it's set to the default value
function EditorKit.edit_nodepath_int(root as Node ptr, path as zstring ptr, default as integer = 0, min as integer = 0, max as integer, delete_default as bool = NO) as bool
	dim node as Nodeptr = NodeByPath(root, path)
	value = iif(node, GetInteger(node), default)
	if edit_int(value, min, max) then
		if delete_default andalso value = default then
			if node then FreeNode node
		else
			if node = null then
				node = NodeByPath(root, path, YES)  'Create it
			end if
			SetContent(node, value)
		end if
		return YES
	end if
end function

' TODO: Many more

'===============================================================================
'                                    Captions

' Set the display value for the current item, overriding default conversion of the
' value to a string.
sub EditorKit.set_caption(caption as zstring ptr)
	if refresh then
		cur_item.caption = *caption
	end if
end sub

sub EditorKit.caption_default_or_int(default_value as integer = 0, default_caption as zstring ptr = @"default")
	if refresh then
		cur_item.caption = iif(value = default_value, *default_caption, str(value))
	end if
end sub

sub EditorKit.caption_default_or_string(default_caption as zstring ptr = @"[default]")
	if refresh then
		cur_item.caption = iif(len(valuestr), valuestr, *default_caption)
	end if
end sub

sub EditorKit.captions_bool(nocapt as zstring ptr, yescapt as zstring ptr)
	if refresh then
		cur_item.caption = iif(value, yescapt, nocapt)
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

' The id isn't used for anything currently
sub EditorKit.set_id(id as integer)
	cur_item.id = id
end sub

' Set which help page is opened by F1 while the current menu item is selected
sub EditorKit.set_helpkey(key as zstring ptr)
	if process then
		base.helpkey = *key
		'cur_item.helpkey = *key
	end if
end sub

' Set text that appears at the bottom of the screen while this item is selected
' (TODO: if the mouse moves, depend on mouse hover instead)
sub EditorKit.set_tooltip(text as zstring ptr)
	if refresh then
		if selected then base.tooltip = *text
	end if
end sub

'===============================================================================
'                               Other facilities

' Call during 'process' or 'activate' to do a conditional exit from the menu. The try_exit()
' virtual method will still be called, which can override it if you can implement it.
sub EditorKit.exit_menu()
	want_exit = YES
end sub
