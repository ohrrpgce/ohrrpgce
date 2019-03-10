#IFNDEF THINGBROWSER_BI
#DEFINE THINGBROWSER_BI

' pass the record number that the editor should edit.
' A record number greater than the current max record number will add a new record
' The return value is -1 for cancellation, or the last edited record number,
' which might differ from the record argument, particularly for editors that
' allow record-switching
' (This is similar to, but not exactly the same as FnEditor in custom.bi)
Type FnThingBrowserEditor as function(record as integer) as integer

Type ThingBrowser extends Object
 'Displays the browser, and retuns the selected result (or start_id if canceled)
 declare function browse(byref start_id as integer=0, byval or_none as bool=NO, editor_func as FnThingBrowserEditor=0, byval edit_by_default as bool=YES, byval skip_zero as bool=NO) as integer

 declare virtual sub build_thing_list()
 declare function check_plank_filter(byval sl as Slice Ptr) as bool
 declare sub loop_sprite_helper(byval plank as Slice Ptr, byval min as integer, byval max as integer, byval delay as integer=1)

 root as Slice ptr
 thinglist as Slice Ptr
 plank_size as XYPair 'This is calculated dynamically from the largest plank returned by create_thing_plank()

 helpkey as string
 index as integer
 or_none as bool
 skip_zero as bool 'Valid record IDs start from 1 instead of 0 (excluding None option)
 can_edit as bool 'gets turned on when editor_func is supplied to .browse()
 filter_text as string

 declare virtual function thing_kind_name() as string
 declare virtual function thing_kind_name_singular() as string
 declare virtual function init_helpkey() as string
 declare virtual function lowest_id() as integer
 declare virtual function highest_id() as integer
 declare virtual function highest_possible_id() as integer
 declare virtual function none_id() as integer

 'Any special initialisation needed. Called after root created but before build_thing_list()
 declare virtual sub enter_browser()

 'Any special cleanup needed. Called after the browser is left but before deleting root.
 declare virtual sub leave_browser()

 'the lookup code SL_PLANK_HOLDER will be automatically applied to whatever slice is returned.
 'Any slices with SL_PLANK_MENU_SELECTABLE should be created as children
 ' The thing id number will automatically be written into the plank's ->Extra(0) slot
 declare virtual function create_thing_plank(byval id as integer) as Slice ptr

 'This is called once each time the cursor selection moves, such as by keyboard or right-click
 declare virtual sub on_cursor_moved(byval id as integer, byval plank as Slice Ptr)

 'This is called once each tick for each plank, and can be used for animation, and similar
 declare virtual sub each_tick_each_plank(byval plank as Slice Ptr)
 
 'This is called once each tick for the currently selected cursor plank
 '(this is called second, after each_tick_each_plank())
 declare virtual sub each_tick_selected_plank(byval plank as Slice Ptr)

 'When a left click happens on a plank, this function must return YES for the plank to be selected 
 declare virtual function confirm_plank_click (byval plank as Slice Ptr) as bool
 
 'Initialize list of slice lookup codes that should be treated as sub-buttons
 declare virtual sub set_up_sub_buttons()
 sub_buttons(ANY) as integer
 
 'Handler for clicking on a sub-button
 declare virtual function on_sub_button_click(byval button_lookup as integer,byval id as integer, byval plank as Slice Ptr) as bool

 'If the plank is purely text based, just override this rather than .create_thing_plank()
 declare virtual function thing_text_for_id(byval id as integer) as string

 'Cropafter is for removing records after the current one
 declare virtual sub handle_cropafter(byval id as integer)

End Type

'-----------------------------------------------------------------------
'Data record browsers

Type ItemBrowser extends ThingBrowser
 declare virtual function thing_kind_name() as string
 declare virtual function init_helpkey() as string
 declare virtual function highest_id() as integer
 declare virtual function highest_possible_id() as integer
 declare virtual function thing_text_for_id(byval id as integer) as string
 declare virtual sub handle_cropafter(byval id as integer)
End Type

Type ShopBrowser extends ThingBrowser
 declare virtual function thing_kind_name() as string
 declare virtual function init_helpkey() as string
 declare virtual function highest_id() as integer
 declare virtual function highest_possible_id() as integer
 declare virtual function thing_text_for_id(byval id as integer) as string
End Type

Type ShopStuffBrowser extends ThingBrowser
 declare constructor (byval shop_id as integer)
 declare virtual function thing_kind_name() as string
 declare virtual function init_helpkey() as string
 declare virtual function highest_id() as integer
 declare virtual function highest_possible_id() as integer
 declare virtual function thing_text_for_id(byval id as integer) as string
 shop_id as integer
End Type

Type AttackBrowser extends ThingBrowser
 declare virtual function thing_kind_name() as string
 declare virtual function init_helpkey() as string
 declare virtual function highest_id() as integer
 declare virtual function highest_possible_id() as integer
 declare virtual function create_thing_plank(byval id as integer) as Slice ptr
 declare virtual sub handle_cropafter(byval id as integer)
 plank_template as Slice Ptr
End Type

Type EnemyBrowser extends ThingBrowser
 declare virtual function thing_kind_name() as string
 declare virtual function thing_kind_name_singular() as string
 declare virtual function init_helpkey() as string
 declare virtual function highest_id() as integer
 declare virtual function highest_possible_id() as integer
 declare virtual function create_thing_plank(byval id as integer) as Slice ptr
 declare virtual sub handle_cropafter(byval id as integer)
 plank_template as Slice Ptr
End Type

Type FormationBrowser extends ThingBrowser
 declare virtual function thing_kind_name() as string
 declare virtual function thing_kind_name_singular() as string
 declare virtual function init_helpkey() as string
 declare virtual function highest_id() as integer
 declare virtual function highest_possible_id() as integer
 declare virtual function thing_text_for_id(byval id as integer) as string
 declare virtual sub handle_cropafter(byval id as integer)
 plank_template as Slice Ptr
End Type

Type HeroBrowser extends ThingBrowser
 declare virtual function thing_kind_name() as string
 declare virtual function thing_kind_name_singular() as string
 declare virtual function init_helpkey() as string
 declare virtual function highest_id() as integer
 declare virtual function highest_possible_id() as integer
 declare virtual function create_thing_plank(byval id as integer) as Slice ptr
 declare virtual sub handle_cropafter(byval id as integer)
 plank_template as Slice Ptr
End Type

Type TextboxBrowser extends ThingBrowser
 declare virtual function thing_kind_name() as string
 declare virtual function thing_kind_name_singular() as string
 declare virtual function init_helpkey() as string
 declare virtual function highest_id() as integer
 declare virtual function highest_possible_id() as integer
 declare virtual function create_thing_plank(byval id as integer) as Slice ptr
 declare virtual sub handle_cropafter(byval id as integer)
 plank_template as Slice Ptr
End Type

'-----------------------------------------------------------------------
'Import resource browsers

Type SfxBrowser extends ThingBrowser
 declare virtual function thing_kind_name() as string
 declare virtual function init_helpkey() as string
 declare virtual function highest_id() as integer
 declare virtual function highest_possible_id() as integer
 declare virtual sub on_cursor_moved(byval id as integer, byval plank as Slice Ptr)
 declare virtual sub build_thing_list()
 declare virtual function create_thing_plank(byval id as integer) as Slice ptr
 plank_template as Slice Ptr
 declare virtual sub set_up_sub_buttons()
 declare virtual function on_sub_button_click(byval button_lookup as integer,byval id as integer, byval plank as Slice Ptr) as bool
 declare virtual sub each_tick_selected_plank(byval plank as Slice Ptr)
 imported_sfx(any) as bool
End Type

Type SongBrowser extends ThingBrowser
 declare virtual function thing_kind_name() as string
 declare virtual function init_helpkey() as string
 declare virtual function highest_id() as integer
 declare virtual function highest_possible_id() as integer
 declare virtual sub on_cursor_moved(byval id as integer, byval plank as Slice Ptr)
 declare virtual sub build_thing_list()
 declare virtual function create_thing_plank(byval id as integer) as Slice ptr
 plank_template as Slice Ptr
 declare virtual sub set_up_sub_buttons()
 declare virtual function on_sub_button_click(byval button_lookup as integer,byval id as integer, byval plank as Slice Ptr) as bool
 declare virtual sub each_tick_selected_plank(byval plank as Slice Ptr)
 imported_songs(any) as bool
End Type

'-----------------------------------------------------------------------
'Constant list browsers

Type ConstantListBrowser extends ThingBrowser
 declare virtual function thing_kind_name() as string
 declare virtual sub enter_browser()
 declare virtual function lowest_id() as integer
 declare virtual function highest_id() as integer
 declare virtual function thing_text_for_id(byval id as integer) as string
 declare virtual sub handle_cropafter(byval id as integer)
 list(any) as string
 longest as integer
End Type

Type ArrayBrowser extends ConstantListBrowser
 thing_name_override as string
 declare constructor (array() as string, thing_name as string="")
 declare virtual function thing_kind_name() as string
 declare sub set_list(array() as string)
End Type

Type FlexmenuCaptionBrowser extends ConstantListBrowser
 declare sub set_list_from_flexmenu(caption() as string, byval caption_code as integer, byval min as integer, byval max as integer)
End Type

'-----------------------------------------------------------------------
'Sprite browsers

Type SpriteBrowser extends ThingBrowser
 declare virtual function thing_kind_name() as string
 declare virtual function sprite_kind() as SpriteType
 declare virtual function sprite_frame() as integer
 declare virtual function create_thing_plank(byval id as integer) as Slice ptr
End Type

Type HeroSpriteBrowser extends SpriteBrowser
 declare virtual function highest_id() as integer
 declare virtual function sprite_kind() as SpriteType
 declare virtual sub each_tick_selected_plank(byval plank as Slice Ptr)
End Type

Type WalkaboutSpriteBrowser extends SpriteBrowser
 declare virtual function highest_id() as integer
 declare virtual function sprite_frame() as integer
 declare virtual function sprite_kind() as SpriteType
 declare virtual sub each_tick_selected_plank(byval plank as Slice Ptr)
End Type

Type PortraitSpriteBrowser extends SpriteBrowser
 declare virtual function highest_id() as integer
 declare virtual function sprite_kind() as SpriteType
End Type

Type EnemySpriteBrowser extends SpriteBrowser
 size_group as integer = -1
 declare virtual function highest_id() as integer
 declare virtual function sprite_kind() as SpriteType
End Type

Type AttackSpriteBrowser extends SpriteBrowser
 declare virtual function highest_id() as integer
 declare virtual function sprite_kind() as SpriteType
 declare virtual sub each_tick_each_plank(byval plank as Slice Ptr)
End Type

Type WeaponSpriteBrowser extends SpriteBrowser
 declare virtual function highest_id() as integer
 declare virtual function sprite_kind() as SpriteType
 declare virtual sub each_tick_selected_plank(byval plank as Slice Ptr)
End Type

Type BackdropSpriteBrowser extends SpriteBrowser
 declare virtual function highest_id() as integer
 declare virtual function sprite_kind() as SpriteType
 declare virtual function create_thing_plank(byval id as integer) as Slice ptr
 declare virtual sub enter_browser()
 declare virtual sub leave_browser()
End Type

Type BoxborderSpriteBrowser extends SpriteBrowser
 declare virtual function highest_id() as integer
 declare virtual function sprite_kind() as SpriteType
 declare virtual function create_thing_plank(byval id as integer) as Slice ptr
End Type

'This is not a real subclass of ThingBrowser, just sort of a duck-type
Type SpriteOfTypeBrowser extends Object
 declare function browse(byref start_id as integer=0, byval or_none as bool=NO, byval spr_type as spriteType) as integer
End Type

'-----------------------------------------------------------------------

#ENDIF
