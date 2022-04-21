'OHRRPGCE - EditorKit framework for creating editors
'(C) Copyright 1997-2021 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#ifndef EDITORKIT_BI
#define EDITORKIT_BI

#include "config.bi"
#include "common.bi"
#include "reload.bi"
#include "menus.bi"

' QB relics
#undef defint
#undef defstr

' (Internal) Indicates which of value/valuestr/valuefloat is in use, and how to interpret it.
enum EditorKitDataType
	dtypeNone
	dtypeBool      'Includes boolean
	dtypeInt
	dtypeStr       'Includes string enumerations (*_str_enum)
	dtypeFloat
end enum

' (Internal) How to write value/valuestr/valuefloat back to the data source
enum EditorKitDataWriter
	writerNone
	writerByte
	writerBoolean
	writerBit
	writerInt      'Includes bool
	writerStr
	writerDouble
	writerNodeInt
	writerNodeBool
	writerNodeStr          'Includes string enumerations
	writerNodeFloat
	writerNodePathInt
	writerNodePathBool
	writerNodePathStr      'Includes string enumerations
	writerNodePathFloat
	writerNodePathExists
	writerConfigBool
end enum

' (Internal) Encapsulates most of the state of the current menu item, aside from
' the actual data value.  Probably should split up into the part we through
' away, and the part that could be useful to keep.
type EditorKitItem
	' Data:
	dtype as EditorKitDataType
	writer as EditorKitDataWriter
	union
		byte_ptr as byte ptr
		int_ptr as integer ptr
		str_ptr as string ptr
		double_ptr as double ptr
		node as Reload.Node ptr       'writerNode* only
	end union
	path as string         'writerNodePath* and writerConfig* only: path to node/setting
	whichbit as integer    'writerBit only: a bitmask
	offset as integer      'dtypeInt only: amount to subtract from value before writing
	inverted_bool as bool  'dtypeBool only: whether to invert value before writing

	delete_default as bool 'writerNodePath* only: delete node if equal to default
	' Default value of a missing node
	writer_default_int as integer  'writerNodePathInt only
	writer_default_str as string   'writerNodePathStr only
	writer_default_float as double 'writerNodePathFloat only

	default_value as integer = INT_MIN
	default_eff_value as integer   'What default_value is effectively equivalent to

	' Menu item:
	id as integer          'Has no purpose yet
	title as string
	caption as string
	helpkey as string
	unselectable as bool
end type

' An option of a string enumeration (val_str_enum)
type StringEnumOption
	' FB doesn't allow initialising strings in UDTs, so we use zstring ptrs
	key as zstring ptr         'The data field value. (Can be "")
	caption as zstring ptr     'Optional, defaults to key
	'description as zstring ptr
end type

' See editorkit.bas for usage information
type EditorKit extends ModularMenu
	'---- Menu settings
	prev_menu_text as string = "Previous Menu"
	' And many others in ModularMenu, including:
	'helpkey as string         'F1 page if not overridden for an item. Gets copied to default_helpkey
	'floating as bool          'Float on top of current screen contents
	'title as string           'Editor title, displayed at top of screen
	'menuopts as MenuOptions

	'---- State variables which can be accessed inside define_items().
	selected as bool           'The current menu item is selected
	hover as bool              'The current menu item has mouse-hover focus
	edited as bool             'The value has been modified and needs to be written back

	' refresh/process/activate tell the context in which the code for a menu
	' item definition is being run.
	' refresh is true for all menu items while refreshing, while activate
	' and process will be true for just one (the currently selected one).
	' No more than one of refresh and process will be true at once.
	refresh as bool
	process as bool            'When called every tick to handle arbitrary input and do editing
	process_text as bool       'Each-tick handling of text editing/input
	activate as bool           'If process, and the item was clicked/activated
	left_click as bool         'If process, and start of left-click on the item
	right_click as bool        'If process, and start of right-click on the item
	declare function delete_action() as bool

	' Holds the value of the datum currently being edited
	value as integer           'Includes bool/boolean data
	valuestr as string
	valuefloat as double

	declare function eff_value() as integer  'Effective val after applying default

	'---- The following is internal state you usually would not access

	enum Phases
		refreshing
		processing
		'activating
	end enum
	phase as Phases            'Whenever define_items() is called, this tells why
	want_activate as bool      'Cache enter_space_click() result
	want_exit as bool          'Called exit_menu()
	initialised as bool        'update() has been called at least once
	record_id_grabber_called as bool 'Ensure is only called once a tick

	default_helpkey as string  'Default, if an item doesn't call set_helpkey.
	' Internal state to track the menu item currently being defined, while inside define_items()
	started_item as bool
	cur_item_index as integer
	cur_item as EditorKitItem

	' For record switching
	record_id_ptr as integer ptr
	min_record_id as integer
	max_record_id_ptr as integer ptr
	max_record_offset as integer  'Added to *max_record_id_ptr
	max_record_max as integer  'The maximum allowed max_record_id

	record_type_name as string 'The type of data being edited, e.g. "Hero"

        '---- Editor setup routines (call before run())

	declare constructor()

	declare sub setup_record_switching(byref record_id as integer, min_record as integer = 0, byref max_record as integer, max_record_offset_ as integer = 0, record_type_name_ as string = "Record", max_record_max_ as integer = 0)

	' Inherited from ModularMenu
	'declare sub run()

  private:
	declare sub update()
	declare function each_tick() as bool
	declare sub draw_overlays()
	declare sub run_phase(which_phase as Phases)
	declare sub write_value()

  public:

        '---- Overridable methods

	' Subclasses should implement this method, nothing else is necessary.
	declare abstract sub define_items()

	declare virtual sub load()
	declare virtual sub save()
	declare virtual function get_record_name(id as integer) as string

	' And also ModularMenu methods such as draw_underlays, draw_overlays, try_exit

	enum EKFlags
		no_flags = 0
		Or_None
		Delete_If_Default
		Preview_Audio
	end enum

	'---- Other non-menu-item methods
	declare sub switch_record(newid as integer)
	declare sub exit_menu()
	declare function record_id_grabber() as bool

	'---- Non-data menu item types
	declare sub spacer()
	declare sub section(title as zstring ptr)
	declare sub subsection(title as zstring ptr)
	declare sub def_record_switcher()
	'No method for adding "Previous Menu", it's automatic

	'---- Adding data menu items
	declare sub defitem(title as zstring ptr)
	declare function defitem_act(title as zstring ptr) as bool
	declare sub defunselectable(title as zstring ptr)
	declare sub defint(title as zstring ptr, byref datum as integer, min as integer = 0, max as integer)
	declare sub defbool overload(title as zstring ptr, byref datum as bool)
	declare sub defbool overload(title as zstring ptr, byref datum as boolean)
	declare sub defbitset(title as zstring ptr, bitwords() as integer, wordnum as integer = 0, bitnum as integer)
	declare sub defstr(title as zstring ptr, byref datum as string, maxlen as integer = 0)

	declare sub finish_defitem()

	'---- Captions
	declare sub set_caption(caption as zstring ptr)
	declare sub caption_default_or_int(default_value as integer = 0, default_caption as zstring ptr = @"Default")
	declare sub caption_default_or_str(default_caption as zstring ptr = @"[default]")
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

	' Mostly internal
	declare sub wrap_caption(caption as string)

	'---- Other menu item attributes
	declare sub set_unselectable()
	declare sub set_id(id as integer)
	declare sub set_helpkey(key as zstring ptr)
	declare sub set_tooltip(text as zstring ptr)

	declare sub default_effective_value(default_value as integer, effective_value as integer)

	'---- Value defining methods (val_*)

	' Value modifiers
	declare sub offset_int overload(offset as integer)
	declare function offset_int overload(offset as integer, byref datum as integer) as integer
	declare sub invert_bool overload()
	declare function invert_bool overload(byref datum as bool) as bool

	' Primitive types
	declare function val_int(byref datum as integer) as integer
	declare function val_bool overload(byref datum as bool) as bool
	declare function val_bool overload(byref datum as boolean) as bool
	declare function val_bit(byref bits as integer, whichbit as integer) as bool
	declare function val_bitset(bitwords() as integer, wordnum as integer = 0, bitnum as integer) as bool
	declare function val_str(byref datum as string) as string
	declare function val_float(byref datum as double) as double

	' Derived types
	declare function val_str_enum(byref datum as string, options() as StringEnumOption) as string

	' RELOAD Nodes
	declare function val_node_int overload(node as Reload.Node ptr) as integer
	declare function val_node_int overload(root as Reload.Node ptr, path as zstring ptr, default as integer = 0, delete_if_default_flag as EKFlags = 0) as integer
	declare function val_node_bool overload(node as Reload.Node ptr) as bool
	declare function val_node_bool overload(root as Reload.Node ptr, path as zstring ptr, default as bool = NO) as bool
	declare function val_node_str overload(node as Reload.Node ptr) as string
	declare function val_node_str overload(root as Reload.Node ptr, path as zstring ptr, default as zstring ptr = @"", delete_if_default_flag as EKFlags = 0) as string
	declare function val_node_float overload(node as Reload.Node ptr) as double
	declare function val_node_float overload(root as Reload.Node ptr, path as zstring ptr, default as double = 0., delete_if_default_flag as EKFlags = 0) as double
	declare function val_node_exists(root as Reload.Node ptr, path as zstring ptr) as bool

	' .ini config file settings
	declare function val_config_bool(path as zstring ptr, default as bool = NO) as bool

	'---- Basic data editing (edit_*)

	' Primitive types
	declare function edit_int(byref datum as integer, min as integer, max as integer) as bool
	declare function edit_bool overload(byref datum as bool) as bool
	declare function edit_bool overload(byref datum as boolean) as bool
	declare function edit_bit(byref bits as integer, whichbit as integer) as bool
	declare function edit_bitset(bitwords() as integer, wordnum as integer = 0, bitnum as integer) as bool
	declare function edit_str(byref datum as string, maxlen as integer = 0) as bool
	'declare function edit_float(byref datum as double, ...) as bool  'TODO

	' Derived types
	declare function edit_zint(byref datum as integer, min as integer, max as integer) as bool
	declare function edit_str_enum(byref datum as string, options() as StringEnumOption) as bool

	' RELOAD Nodes
	declare function edit_node_int overload(node as Reload.Node ptr, min as integer = 0, max as integer) as bool
	declare function edit_node_int overload(root as Reload.Node ptr, path as zstring ptr, default as integer = 0, min as integer = 0, max as integer, delete_if_default_flag as EKFlags = 0) as bool
	declare function edit_node_bool overload(node as Reload.Node ptr) as bool
	declare function edit_node_bool overload(root as Reload.Node ptr, path as zstring ptr, default as integer = 0) as bool
	declare function edit_node_str overload(node as Reload.Node ptr, maxlen as integer = 0) as bool
	declare function edit_node_str overload(root as Reload.Node ptr, path as zstring ptr, default as zstring ptr = @"", maxlen as integer = 0, delete_if_default_flag as EKFlags = 0) as bool
	declare function edit_node_exists(node as Reload.Node ptr, path as zstring ptr) as bool

	declare sub delete_node()
	declare sub deletable_node(node as Node ptr = NULL, thingname as zstring ptr = NULL)

	' .ini config file settings
	declare function edit_config_bool(path as zstring ptr, default as bool = NO) as bool

	'---- Game data type definitions & editing (as_*, edit_as_*)

	' Tags
	declare sub as_check_tag(byref datum as integer, prefix as zstring ptr = @"Tag", zerocap as zstring ptr = @"None")
	declare function edit_as_check_tag(byref datum as integer, prefix as zstring ptr = @"Tag", zerocap as zstring ptr = @"None", allowneg as bool = YES) as bool
	declare sub as_set_tag(byref datum as integer, prefix as zstring ptr = @"Set tag", allowspecial as bool = NO)
	declare function edit_as_set_tag(byref datum as integer, prefix as zstring ptr = @"Set tag", allowspecial as bool = NO, allowneg as bool = YES) as bool
	declare sub as_tag_id(byref datum as integer, prefix as zstring ptr = @"Tag", allowspecial as bool = NO)
	declare function edit_as_tag_id(byref datum as integer, prefix as zstring ptr = @"Tag", allowspecial as bool = NO) as bool

	' Sprites
	declare sub as_spriteset(byref datum as integer, or_none_flag as EKFlags = 0)
	declare function edit_as_spriteset(byref datum as integer, spr_type as SpriteType, or_none_flag as EKFlags = 0) as bool
	declare sub as_palette(byref datum as integer)
	declare function edit_as_palette(byref datum as integer, spr_type as SpriteType = sprTypeInvalid, spr_set as integer = 0) as bool

	' Audio
	declare sub as_song(byref datum as integer)
	declare function edit_as_song(byref datum as integer, min as integer = -1, preview_audio_flag as EKFlags = 0) as bool
	declare sub as_sfx(byref datum as integer)
	declare function edit_as_sfx(byref datum as integer, min as integer = -1, preview_audio_flag as EKFlags = 0) as bool

	' Enemies
	declare sub as_enemy(byref id as integer, or_none_flag as EKFlags = 0)
	declare function edit_as_enemy(byref id as integer, or_none_flag as EKFlags = 0) as bool

	' Extra data vectors
	declare sub edit_extra_data_vector(byref extravec as integer vector)

  private:
	' Disable a few ModularMenu methods so they can't be called directly; they wouldn't work.
	' NOTE: due to FB bug sf#948 (fixed in FB 1.09) if you attempt to call these you'll get
	' the error "error 255: Ambiguous symbol access, explicit scope resolution required"
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
