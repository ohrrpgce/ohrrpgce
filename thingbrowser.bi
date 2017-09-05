#IFNDEF THINGBROWSER_BI
#DEFINE THINGBROWSER_BI

Type ThingBrowser extends Object
 'Displays the browser, and retuns the selected result (or start_id if canceled)
 declare function browse(byref start_id as integer=0) as integer
 declare sub build_thing_list()

 root as Slice ptr

 helpkey as string

 declare virtual function init_helpkey() as string
 declare virtual function lowest_id() as integer
 declare virtual function highest_id() as integer
End Type

Type ItemBrowser extends ThingBrowser
 declare virtual function init_helpkey() as string
 declare virtual function highest_id() as integer
End Type

#ENDIF
