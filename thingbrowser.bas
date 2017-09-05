'OHRRPGCE GAME & CUSTOM - Classes for browsing various kinds of things ant getting an ID number
'(C) Copyright 2017 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'Except, this module isn't especially crappy
'
#include "config.bi"
#include "allmodex.bi"
#include "common.bi"
#include "const.bi"
#include "uiconst.bi"
#include "reloadext.bi"
#include "slices.bi"

#include "thingbrowser.bi"

'-----------------------------------------------------------------------

Function ThingBrowser.browse(byref start_id as integer=0) as integer
 dim result as integer = start_id

 dim holdscreen as integer = allocatepage
 copypage vpage, holdscreen

 root = NewSliceOfType(slRoot)
 with *root
  .Fill = YES
  .paddingLeft = 8
  .paddingRight = 8
  .paddingTop = 8
  .paddingBottom = 8
 end with
 
 dim box as Slice ptr
 box = NewSliceOfType(slRectangle, root)
 WITH *box
  .Fill = YES
 END WITH
 ChangeRectangleSlice box, 1

 dim grid as Slice Ptr
 grid = NewSliceOfType(slGrid, root)
 WITH *grid
  .Fill = YES
  .Lookup = SL_PLANK_HOLDER
 END WITH

 build_thing_list()
 do
  setwait 55
  setkeys YES
  if keyval(scEsc) > 1 then
   'cancel out of the browser
   result = start_id
   exit do
  end if
  if len(helpkey) andalso keyval(scF1) > 1 then show_help helpkey

  copypage holdscreen, vpage
  DrawSlice root, vpage
  setvispage vpage
  dowait
 LOOP
 setkeys
 freepage holdscreen
 return result
End Function

Function ThingBrowser.init_helpkey() as string
 return ""
End Function


Sub ThingBrowser.build_thing_list()
 dim plank as slice ptr
 for id as integer = lowest_id() to highest_id()
  'plank = create_thing_plank(id)
 next id
End Sub

Function ThingBrowser.lowest_id() as integer
 return 0
End Function

Function ThingBrowser.highest_id() as integer
 return -1
End Function

'-----------------------------------------------------------------------

Function ItemBrowser.init_helpkey() as string
 return "item_browser"
End Function

Function ItemBrowser.highest_id() as integer
 return gen(genMaxItem)
End Function
