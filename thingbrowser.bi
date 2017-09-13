#IFNDEF THINGBROWSER_BI
#DEFINE THINGBROWSER_BI

Type ThingBrowser extends Object
 'Displays the browser, and retuns the selected result (or start_id if canceled)
 declare function browse(byref start_id as integer=0, byval or_none as bool=NO) as integer
 declare sub build_thing_list()
 declare sub loop_sprite_helper(byval plank as Slice Ptr, byval min as integer, byval max as integer, byval delay as integer=1)

 root as Slice ptr
 plank_size as XYPair 'This is calculated dynamically from the largest plank returned by create_thing_plank()

 helpkey as string
 index as integer
 or_none as bool

 declare virtual function init_helpkey() as string
 declare virtual function lowest_id() as integer
 declare virtual function highest_id() as integer

 'Any special initialisation needed. Called after root created but before build_thing_list()
 declare virtual sub enter_browser()

 'Any special cleanup needed. Called after the browser is left but before deleting root.
 declare virtual sub leave_browser()

 'the lookup code SL_PLANK_HOLDER will be automatically applied to whatever slice is returned.
 'Any slices with SL_PLANK_MENU_SELECTABLE should be created as children
 ' The thing id number will automatically be written into the plank's ->Extra(0) slot
 declare virtual function create_thing_plank(byval id as integer) as Slice ptr

 'This is called once each tick for each plank, and can be used for animation, and similar
 declare virtual sub each_tick_each_plank(byval plank as Slice Ptr)
 
 'This is called once each tick for the currently selected cursor plank
 '(this is called second, after each_tick_each_plank())
 declare virtual sub each_tick_selected_plank(byval plank as Slice Ptr)

 'If the plank is purely text based, just override this rather than .create_thing_plank()
 declare virtual function thing_text_for_id(byval id as integer) as string

End Type

Type ItemBrowser extends ThingBrowser
 declare virtual function init_helpkey() as string
 declare virtual function highest_id() as integer
 declare virtual function thing_text_for_id(byval id as integer) as string
End Type

Type ShopBrowser extends ThingBrowser
 declare virtual function init_helpkey() as string
 declare virtual function highest_id() as integer
 declare virtual function thing_text_for_id(byval id as integer) as string
End Type

Type SpriteBrowser extends ThingBrowser
 declare virtual function sprite_kind() as integer
 declare virtual function sprite_frame() as integer
 declare virtual function create_thing_plank(byval id as integer) as Slice ptr
End Type

Type HeroSpriteBrowser extends SpriteBrowser
 declare virtual function highest_id() as integer
 declare virtual function sprite_kind() as integer
 declare virtual sub each_tick_selected_plank(byval plank as Slice Ptr)
End Type

Type WalkaboutSpriteBrowser extends SpriteBrowser
 declare virtual function highest_id() as integer
 declare virtual function sprite_frame() as integer
 declare virtual function sprite_kind() as integer
 declare virtual sub each_tick_selected_plank(byval plank as Slice Ptr)
End Type

Type PortraitSpriteBrowser extends SpriteBrowser
 declare virtual function highest_id() as integer
 declare virtual function sprite_kind() as integer
End Type

Type EnemySpriteBrowser extends SpriteBrowser
 size_group as integer = -1
 declare virtual function highest_id() as integer
 declare virtual function sprite_kind() as integer
End Type

Type AttackSpriteBrowser extends SpriteBrowser
 declare virtual function highest_id() as integer
 declare virtual function sprite_kind() as integer
 declare virtual sub each_tick_each_plank(byval plank as Slice Ptr)
End Type

Type WeaponSpriteBrowser extends SpriteBrowser
 declare virtual function highest_id() as integer
 declare virtual function sprite_kind() as integer
 declare virtual sub each_tick_selected_plank(byval plank as Slice Ptr)
End Type

Type BackdropSpriteBrowser extends SpriteBrowser
 declare virtual function highest_id() as integer
 declare virtual function sprite_kind() as integer
 declare virtual function create_thing_plank(byval id as integer) as Slice ptr
 declare virtual sub enter_browser()
 declare virtual sub leave_browser()
End Type

Type BoxborderSpriteBrowser extends SpriteBrowser
 declare virtual function highest_id() as integer
 declare virtual function sprite_kind() as integer
 'declare virtual function create_thing_plank(byval id as integer) as Slice ptr
End Type

Type SpriteOfTypeBrowser extends Object
 declare function browse(byref start_id as integer=0, byval or_none as bool=NO, byval spr_type as spriteType) as integer
End Type

#ENDIF
