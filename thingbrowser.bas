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
 holdscreen = allocatepage
 copypage vpage, holdscreen
 
 dim result as integer = start_id

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

  edgeprint "Inside the thingbrowser", 0, 0, uilook(uiText), vpage
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
 debug "ThingBrowser.build_thing_list()"
End Sub

'-----------------------------------------------------------------------

Function ItemBrowser.init_helpkey() as string
 return "item_browser"
End Function

Sub ItemBrowser.build_thing_list()
 debug "ItemBrowser.build_thing_list()"
End Sub
