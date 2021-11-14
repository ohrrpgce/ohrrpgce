'OHRRPGCE - EditorKit framework for creating editors
'(C) Copyright 1997-2021 James Paige, Ralph Versteegen, and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability

#ifndef EDITORKIT_BI
#define EDITORKIT_BI

#include "config.bi"
#include "common.bi"
#include "reload.bi"
#include "menus.bi"


enum EditorKitDataKind
	edkindNone
	edkindBool
	edkindInteger
	edkindFloat
	edkindString
	'TODO: check tag, set tag, scripts, textboxes, enemies, etc
end enum

type EditorKitItem
	kind as EditorKitDataKind
	id as integer        'Has no purpose
	title as string
	caption as string
	'helpkey as string
	unselectable as bool
end type

' See editorkit.bas for usage information
type EditorKit extends ModularMenu
	'---- Menu settings
	prev_menu_title as string = "Previous Menu"
	'And many others in ModularMenu, including:
	'helpkey as string       'Gets copied to default_helpkey
	'floating as bool        'Float on top of current screen contents
	'title as string         'Editor title, displayed at top of screen
	'menuopts as MenuOptions

	'---- State variables which can be accessed inside define_items().
	selected as bool         'The current menu item is selected
	hover as bool            'The current menu item has mouse-hover focus

	' refresh/process/activate tell the context in which the code for a menu item definition
	' is being run.
	' refresh is true for all menu items, while activate and process will be true for just one
	' (the currently selected one).
	' No more than one of refresh and process will be true at once.
	refresh as bool
	process as bool          'When called every tick to handle arbitrary input. Also the place
	' When `process' is true `activate' is also set to true if the item was clicked/activated.
	activate as bool

	' Holds the value of the datum currently being edited
	value as integer         'Includes bool/boolean
	valuefloat as double
	valuestr as string

	'---- The following is internal state you usually would not access
	enum Phases
		refreshing
		processing
		'activating
	end enum
	phase as Phases           'Whenever define_items() is called, this tells why
	want_activate as bool     'Cache enter_space_click() result
	want_exit as bool         'Called exit_menu()

	default_helpkey as string 'Default, if an item doesn't call set_helpkey.
	' Internal state to track the menu item currently being defined, while inside define_items()
	started_item as bool
	cur_item_index as integer
	cur_item as EditorKitItem

	declare constructor()

  private:
	declare sub update()
	declare function each_tick() as bool
	declare sub run_phase(which_phase as Phases)
	declare sub finish_defitem()

  public:
	' Subclasses should implement this method, nothing else is necessary.
	declare abstract sub define_items()
	' TODO: save, load methods

	declare sub spacer()
	declare sub section(title as zstring ptr)
	declare sub subsection(title as zstring ptr)

	declare sub defitem(title as zstring ptr)
	declare function defitem_act(title as zstring ptr) as bool
	declare sub defunselectable(title as zstring ptr)
	declare sub defint(title as zstring ptr, byref datum as integer, min as integer = 0, max as integer)
	declare sub defbool(title as zstring ptr, byref datum as bool)
	declare sub defbitset(title as zstring ptr, bitwords() as integer, wordnum as integer = 0, bitnum as integer)
	declare sub defstring(title as zstring ptr, byref datum as string, maxlen as integer = 0)
	declare sub defgen_int(title as zstring ptr, genidx as integer, min as integer = 0, max as integer)

	declare sub as_int(byref datum as integer)
	declare sub as_bool(byref datum as bool)
	declare sub as_string(byref datum as string)
	declare sub as_float(byref datum as double)

	declare function edit_int(byref datum as integer, min as integer, max as integer) as bool
	declare function edit_bool(byref datum as bool) as bool
	declare function edit_bit(byref bits as integer, whichbit as integer) as bool
	declare function edit_bitset(bitwords() as integer, wordnum as integer = 0, bitnum as integer) as bool
	declare function edit_string(byref datum as string, maxlen as integer = 0) as bool
	'declare function edit_float(byref datum as double, ...) as bool  'TODO

	declare function edit_gen_int(genidx as integer, min as integer = 0, max as integer) as bool

	declare function edit_config_bool(path as zstring ptr, default as bool = NO) as bool

	declare function edit_node_int(node as Reload.Node ptr, min as integer = 0, max as integer) as bool
	declare function edit_node_string(node as Reload.Node ptr, default as zstring ptr = @"", maxlen as integer = 0) as bool
	declare function edit_nodepath_int(root as Reload.Node ptr, path as zstring ptr, default as integer = 0, min as integer = 0, max as integer, delete_default as bool = YES) as bool

	declare sub set_caption(caption as zstring ptr)
	declare sub caption_default_or_int(default_value as integer = 0, default_caption as zstring ptr = @"default")
	declare sub caption_default_or_string(default_caption as zstring ptr = @"[default]")
	declare sub captions_bool(nocapt as zstring ptr, yescapt as zstring ptr)
	declare sub captions(captions_array() as string, invalid_thing as zstring ptr = @"value")
	declare sub captionsz(captions_array() as zstring ptr, invalid_thing as zstring ptr = @"value")
	declare sub captions_or_int(captions_array() as string)
	'declare sub captions_list(...)   'A macro, below
	'declare sub captions_list_or_int(...)   'A macro, below

	#macro captions_list(caption_strings...)
		if refresh then  'Creates a scope
			dim captionarray(...) as string = {caption_strings}
			captions captionarray()
		end if
	#endmacro
	#macro captions_list_or_int(caption_strings...)
		if refresh then  'Creates a scope
			dim captionarray(...) as string = {caption_strings}
			captions_or_int captionarray()
		end if
	#endmacro

	declare sub set_id(id as integer)
	declare sub set_helpkey(key as zstring ptr)
	declare sub set_tooltip(text as zstring ptr)

	declare sub exit_menu()

  private:
	' Disable a few ModularMenu methods so they can't be called directly; they wouldn't work.
	' NOTE: due to FB bug sf#948 if you attempt to call these you'll get the error
	' "error 255: Ambiguous symbol access, explicit scope resolution required"
	' Call section() instead of header()
	declare sub header()
	' Call spacer() instead of add_spacer()
	declare sub add_spacer()
	' Call defitem() or defunselectable() or subsection() instead of add_item()
	declare sub add_item()
end type

' Use the line number as a unique ID to identify the menu item, where needed
'#define defitem defitem_ __LINE__ ,
'#define defitem_act(args...) defitem_act_(__LINE__ ,  args)

#endif
