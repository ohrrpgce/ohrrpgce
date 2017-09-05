#IFNDEF THINGBROWSER_BI
#DEFINE THINGBROWSER_BI

Type ThingBrowser extends Object
 'Displays the browser, and retuns the selected result (or start_id if canceled)
 declare function browse(byref start_id as integer=0) as integer

 holdscreen as integer
 helpkey as string

 declare virtual function init_helpkey() as string
 declare virtual sub build_thing_list()
End Type

Type ItemBrowser extends ThingBrowser
 declare virtual function init_helpkey() as string
 declare virtual sub build_thing_list()
End Type

#ENDIF
