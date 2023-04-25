'OHRRPGCE CUSTOM - Shop Editor
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#include "config.bi"
#include "allmodex.bi"
#include "common.bi"
#include "loading.bi"
#include "const.bi"
#include "uiconst.bi"
#include "scrconst.bi"
#include "customsubs.bi"
#include "custom.bi"
#include "thingbrowser.bi"


DECLARE FUNCTION shop_editor (shop_id as integer) as integer
DECLARE SUB shop_stuff_editor_main (byval shop_id as integer)
DECLARE FUNCTION shop_stuff_edit (byval stuff_id as integer, byval shop_id as integer) as integer
DECLARE FUNCTION shop_stuff_edit_wrapper (byval stuff_id as integer) as integer 'uses shop_stuff_context_id
DECLARE SUB shop_save_stf (byval shop_id as integer, byref stuf as ShopStuffState, stufbuf() as integer)
DECLARE SUB shop_load_stf (byval shop_id as integer, byref stuf as ShopStuffState, stufbuf() as integer)
DECLARE SUB shop_swap_stf (shop_id as integer, thing_id1 as integer, thing_id2 as integer)
DECLARE SUB shop_init_stf (byval shop_id as integer, stuf as ShopStuffState, stufbuf() as integer)
DECLARE FUNCTION read_shop_stuff_count (byval shop_id as integer) as integer
DECLARE SUB write_shop_stuff_count (byval shop_id as integer, byval thing_last_id as integer)
DECLARE SUB update_shop_stuff_menu (byref stuf as ShopStuffState, stufbuf() as integer, byval thing_last_id as integer)
DECLARE SUB update_shop_stuff_type (byref stuf as ShopStuffState, stufbuf() as integer, byval reset_name_and_price as bool=NO)
DECLARE SUB shop_menu_update (byref shopst as ShopEditState, shopbuf() as integer)
DECLARE SUB shop_save (byref shopst as ShopEditState, shopbuf() as integer)
DECLARE SUB shop_load (byref shopst as ShopEditState, shopbuf() as integer)
DECLARE SUB shop_add_new (shopst as ShopEditState)

DIM SHARED shop_stuff_context_id as integer


SUB shop_editor_main()
 IF read_config_bool("thingbrowser.enable_top_level", YES) THEN
  DIM b as ShopBrowser
  b.browse(-1, , @shop_editor)
 ELSE
  shop_editor 0
 END IF
END SUB

FUNCTION shop_picker (recindex as integer = -1) as integer
 DIM b as ShopBrowser
 RETURN b.browse(recindex, , @shop_editor, NO)
END FUNCTION

FUNCTION shop_picker_or_none (recindex as integer = -1) as integer
 DIM b as ShopBrowser
 RETURN b.browse(recindex - 1, YES , @shop_editor, NO) + 1
END FUNCTION

FUNCTION shop_editor (shop_id as integer) as integer
 'shop_id is the default shop to start on. or > max to add a new one.
 'return value is the last id selected or -1 if cancelled adding a new
 DIM shopbuf(20) as integer

 DIM sbit(-1 TO 7) as string
 sbit(0) = "Buy"
 sbit(1) = "Sell"
 sbit(2) = "Hire"
 sbit(3) = "Inn"
 sbit(4) = "Equip"
 sbit(5) = "Save"
 sbit(6) = "Map"
 sbit(7) = "Team"

 DIM shopst as ShopEditState
 shopst.id = shop_id
 shopst.havestuf = NO

 IF shopst.id > gen(genMaxShop) THEN
  shop_add_new shopst
  IF shopst.id < shop_id THEN
   'Cancelled adding a new one
   RETURN -1
  END IF
 END IF


 shop_load shopst, shopbuf()
 shopst.st.last = 6
 shopst.st.size = 24

 DIM new_shop_id as integer
 
 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "shop_main"
  IF cropafter_keycombo(shopst.st.pt = 1) THEN cropafter shopst.id, gen(genMaxShop), game + ".sho", 40
  usemenu shopst.st
  IF shopst.st.pt = 1 THEN
   new_shop_id = shopst.id
   IF intgrabber_with_addset(new_shop_id, 0, gen(genMaxShop), maxMaxShop, "Shop") THEN
    shop_save shopst, shopbuf()
    shopst.id = new_shop_id
    IF shopst.id > gen(genMaxShop) THEN
     shop_add_new shopst
    END IF
    shop_load shopst, shopbuf()
   END IF
  END IF
  IF shopst.st.pt = 2 THEN
   strgrabber shopst.name, 15
   shopst.st.need_update = YES
  END IF
  IF enter_space_click(shopst.st) THEN
   IF shopst.st.pt = 0 THEN EXIT DO
   IF shopst.st.pt = 3 AND shopst.havestuf THEN
    shop_save shopst, shopbuf()
    shop_stuff_editor_main shopst.id
    shop_load shopst, shopbuf()
   END IF
   IF shopst.st.pt = 4 THEN editbitset shopbuf(), 17, sbit(), "shop_menu_items": shopst.st.need_update = YES
   IF shopst.st.pt = 6 THEN
    shopst.menu(6) = "Inn Script: " & scriptbrowse(shopbuf(19), plottrigger, "Inn Plotscript")
   END IF
  END IF
  IF shopst.st.pt = 5 THEN
   IF intgrabber(shopbuf(18), 0, 32767) THEN shopst.st.need_update = YES
  END IF
  IF shopst.st.pt = 6 THEN
   IF scrintgrabber(shopbuf(19), 0, 0, ccLeft, ccRight, 1, plottrigger) THEN shopst.st.need_update = YES
  END IF
  
  IF shopst.st.need_update THEN
   shopst.st.need_update = NO
   shop_menu_update shopst, shopbuf()
  END IF
  
  clearpage dpage
  standardmenu shopst.menu(), shopst.st, shopst.shaded(), , , dpage
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 shop_save shopst, shopbuf()
 RETURN shopst.id
END FUNCTION

SUB shop_load (byref shopst as ShopEditState, shopbuf() as integer)
 loadrecord shopbuf(), game & ".sho", 40 \ 2, shopst.id
 shopst.name = readbadbinstring(shopbuf(), 0, 15)
 shopst.st.need_update = YES
END SUB

SUB shop_save (byref shopst as ShopEditState, shopbuf() as integer)
 shopbuf(16) = small(shopbuf(16), 49)
 writebadbinstring shopst.name, shopbuf(), 0, 15
 storerecord shopbuf(), game & ".sho", 40 \ 2, shopst.id
END SUB

SUB shop_menu_update(byref shopst as ShopEditState, shopbuf() as integer)
 shopst.menu(0) = "Return to Previous Menu"
 shopst.menu(1) = CHR(27) & "Shop " & shopst.id & " of " & gen(genMaxShop) & CHR(26)
 shopst.menu(2) = "Name: " & shopst.name
 shopst.menu(3) = "Edit Available Stuff..."
 shopst.menu(4) = "Select Shop Menu Items..."
 shopst.menu(5) = "Inn Price: " & shopbuf(18)
 IF readbit(shopbuf(), 17, 3) = 0 THEN shopst.menu(5) = "Inn Price: N/A"
 shopst.menu(6) = "Inn Script: " & scriptname(shopbuf(19))
 IF readbit(shopbuf(), 17, 0) ORELSE readbit(shopbuf(), 17, 1) ORELSE readbit(shopbuf(), 17, 2) THEN
  shopst.havestuf = YES
  shopst.shaded(3) = NO
 ELSE
  shopst.havestuf = NO
  ' Grey out "Edit available stuff"
  shopst.shaded(3) = YES
 END IF
END SUB

'TODO: convert to generic_add_new
SUB shop_add_new (shopst as ShopEditState)
  DIM menu(2) as string
  DIM shoptocopy as integer = 0
  DIM state as MenuState
  state.last = UBOUND(menu)
  state.size = 24
  state.pt = 1

  state.need_update = YES
  setkeys
  DO
    setwait 55
    setkeys
    IF keyval(ccCancel) > 1 THEN  'cancel
      shopst.id -= 1
      EXIT DO
    END IF
    IF keyval(scF1) > 1 THEN show_help "shop_new"
    usemenu state
    IF state.pt = 2 THEN
      IF intgrabber(shoptocopy, 0, gen(genMaxShop)) THEN state.need_update = YES
    END IF
    IF state.need_update THEN
      state.need_update = NO
      menu(0) = "Cancel"
      menu(1) = "New Blank Shop"
      menu(2) = "Copy of Shop " & shoptocopy & " " & readshopname(shoptocopy) 'readbadbinstring(shopbuf(), 0, 15, 0)
    END IF
    IF enter_space_click(state) THEN
      DIM shopbuf(19) as integer
      DIM stufbuf(50 * curbinsize(binSTF) \ 2 - 1) as integer
      SELECT CASE state.pt
        CASE 0 ' cancel
          shopst.id -= 1
          EXIT DO
        CASE 1 ' blank
          gen(genMaxShop) += 1
          '--Create a new shop record
          flusharray shopbuf()
          '--Create a new shop stuff record
          flusharray stufbuf()
          'FIXME: load the name and price for first shop item
          stufbuf(19) = -1  'Default in-stock to infinite (first item only!)
        CASE 2 ' copy
          gen(genMaxShop) += 1
          loadrecord shopbuf(), game + ".sho", 20, shoptocopy
          loadrecord stufbuf(), game + ".stf", 50 * getbinsize(binSTF) \ 2, shoptocopy
      END SELECT
      storerecord shopbuf(), game + ".sho", 20, shopst.id
      'Save all 50 shop stock items at once
      storerecord stufbuf(), game + ".stf", 50 * getbinsize(binSTF) \ 2, shopst.id
      EXIT DO
    END IF

    clearpage vpage
    standardmenu menu(), state, 20, 20, vpage
    setvispage vpage
    dowait
  LOOP
END SUB

SUB shop_stuff_editor_main (byval shop_id as integer)
 IF read_config_bool("thingbrowser.enable_top_level", YES) THEN
  DIM b as ShopStuffBrowser = ShopStuffBrowser(shop_id)
  shop_stuff_context_id = shop_id
  b.browse(-1, , @shop_stuff_edit_wrapper)
 ELSE
  shop_stuff_edit_wrapper 0
 END IF
END SUB

FUNCTION shop_stuff_edit_wrapper (byval stuff_id as integer) as integer
 RETURN shop_stuff_edit(stuff_id, shop_stuff_context_id)
END FUNCTION

FUNCTION read_shop_stuff_count(byval shop_id as integer) as integer
 DIM shopbuf(20) as integer
 loadrecord shopbuf(), game & ".sho", 40 \ 2, shop_id
 RETURN shopbuf(16)
END FUNCTION

SUB write_shop_stuff_count(byval shop_id as integer, byval thing_last_id as integer)
 DIM shopbuf(20) as integer
 loadrecord shopbuf(), game & ".sho", 40 \ 2, shop_id
 shopbuf(16) = thing_last_id
 storerecord shopbuf(), game & ".sho", 40 \ 2, shop_id
END SUB

FUNCTION shop_stuff_edit (byval stuff_id as integer, byval shop_id as integer) as integer
 'stuff_id is the thing to start on, or > max to add a new one
 'Return value is the last thing selected, or -1 if adding a new one was cancelled
 DIM thing_last_id as integer = read_shop_stuff_count(shop_id)
 DIM show_stockidx as bool = NO

 DIM stuf as ShopStuffState
 stuf.thing = stuff_id
 stuf.thingname = ""

 stuf.st.pt = 0
 stuf.st.last = 2
 stuf.st.size = 24
 
 DIM stufbuf(curbinsize(binSTF) \ 2 - 1) as integer

 'If requested ID is greater than the last, add a new one
 IF stuf.thing > thing_last_id THEN
  IF stuf.thing > 49 THEN RETURN -1 'no more allowed
  thing_last_id = stuf.thing
  shop_init_stf shop_id, stuf, stufbuf()
 END IF

 shop_load_stf shop_id, stuf, stufbuf()
 
 update_shop_stuff_type stuf, stufbuf()
 update_shop_stuff_menu stuf, stufbuf(), thing_last_id
 
 setkeys YES
 DO
  setwait 55
  setkeys YES

  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "shop_stuff"
  IF keyval(scF6) > 1 THEN show_stockidx XOR= YES  'Debug key

  DIM used as bool = enter_space_click(stuf.st)
  IF stuf.st.pt = 0 ANDALSO used THEN EXIT DO
  IF used THEN
   'Save as a precaution because we might be about to browse
   shop_save_stf shop_id, stuf, stufbuf()
   write_shop_stuff_count shop_id, thing_last_id
  END IF
  SELECT CASE stuf.st.pt
   CASE 1 'browse shop stuff
    DIM newthing as integer = stuf.thing
    IF keyval(scShift) > 0 THEN
     ' While holding Shift, can swap a thing with the one before/after it.
     IF keyval(ccLeft) > 1 OR keyval(ccRight) > 1 THEN
      IF keyval(ccLeft) > 1 AND stuf.thing > 0 THEN newthing -= 1
      IF keyval(ccRight) > 1 AND stuf.thing < thing_last_id THEN newthing += 1
      shop_save_stf shop_id, stuf, stufbuf()
      shop_swap_stf shop_id, stuf.thing, newthing
      stuf.thing = newthing
      stuf.st.need_update = YES
     END IF
    ELSE
     IF intgrabber_with_addset(newthing, 0, thing_last_id, 49, "Shop Thing") THEN
      shop_save_stf shop_id, stuf, stufbuf()
      stuf.thing = newthing
      IF stuf.thing > thing_last_id THEN
       thing_last_id = stuf.thing
       shop_init_stf shop_id, stuf, stufbuf()
      END IF
      shop_load_stf shop_id, stuf, stufbuf()
      update_shop_stuff_type stuf, stufbuf()
      stuf.st.need_update = YES
     END IF
    END IF
   CASE 2 'name
    IF strgrabber(stuf.thingname, 16) THEN stuf.st.need_update = YES
   CASE 3 TO 4 'type and ID
    IF stuf.st.pt = 4 THEN
     IF used THEN
      DIM id_num as integer
      IF stufbuf(17) = 0 THEN '--an item
       id_num = item_picker(stufbuf(18))
      ELSEIF stufbuf(17) = 1 THEN '--a hero
       id_num = hero_picker(stufbuf(18))
      END IF
      shop_load_stf shop_id, stuf, stufbuf()
      stufbuf(18) = id_num
      thing_last_id = read_shop_stuff_count(shop_id)
      update_shop_stuff_type stuf, stufbuf(), YES
      stuf.st.need_update = YES
     END IF
    END IF
    IF intgrabber(stufbuf(17 + stuf.st.pt - 3), stuf.min(stuf.st.pt), stuf.max(stuf.st.pt)) THEN
     stuf.st.need_update = YES
     update_shop_stuff_type stuf, stufbuf(), YES
    END IF
   CASE 6 TO 7 '--condition tags
    IF tag_grabber(stufbuf(17 + stuf.st.pt - 3), stuf.st) THEN stuf.st.need_update = YES
   CASE 8 TO 9 '--set tags
    IF tag_set_grabber(stufbuf(17 + stuf.st.pt - 3), stuf.st) THEN stuf.st.need_update = YES
   CASE 11 '--must trade in item 1 type
    IF zintgrabber(stufbuf(25), stuf.min(stuf.st.pt), stuf.max(stuf.st.pt)) THEN stuf.st.need_update = YES
    IF used THEN
     DIM item_id as integer = item_picker_or_none(stufbuf(25))
     shop_load_stf shop_id, stuf, stufbuf()
     stufbuf(25) = item_id
     thing_last_id = read_shop_stuff_count(shop_id)
     update_shop_stuff_type stuf, stufbuf(), YES
     stuf.st.need_update = YES
    END IF
   CASE 13, 15, 17 '--must trade in item 2+ types
    IF zintgrabber(stufbuf(18 + stuf.st.pt), stuf.min(stuf.st.pt), stuf.max(stuf.st.pt)) THEN stuf.st.need_update = YES
    IF used THEN
     DIM item_id as integer = item_picker_or_none(stufbuf(18 + stuf.st.pt))
     shop_load_stf shop_id, stuf, stufbuf()
     stufbuf(18 + stuf.st.pt) = item_id
     thing_last_id = read_shop_stuff_count(shop_id)
     update_shop_stuff_type stuf, stufbuf(), YES
     stuf.st.need_update = YES
    END IF
   CASE 12, 14, 16, 18 '--trade in item amounts
    stufbuf(18 + stuf.st.pt) += 1
    IF intgrabber(stufbuf(18 + stuf.st.pt), stuf.min(stuf.st.pt), stuf.max(stuf.st.pt)) THEN stuf.st.need_update = YES
    stufbuf(18 + stuf.st.pt) -= 1
   CASE 19, 20 '--sell type, price
    IF intgrabber(stufbuf(7 + stuf.st.pt), stuf.min(stuf.st.pt), stuf.max(stuf.st.pt)) THEN stuf.st.need_update = YES
    IF (stufbuf(26) < 0 OR stufbuf(26) > 3) AND stufbuf(17) <> 1 THEN stufbuf(26) = 0
   CASE 21 '--trade in for
    IF zintgrabber(stufbuf(7 + stuf.st.pt), stuf.min(stuf.st.pt), stuf.max(stuf.st.pt)) THEN stuf.st.need_update = YES
    IF used THEN
     dim item_id as integer = item_picker_or_none(stufbuf(7 + stuf.st.pt))
     shop_load_stf shop_id, stuf, stufbuf()
     stufbuf(7 + stuf.st.pt) = item_id
     thing_last_id = read_shop_stuff_count(shop_id)
     update_shop_stuff_type stuf, stufbuf(), YES
     stuf.st.need_update = YES
    END IF
   CASE 22 '--trade in for amount
    stufbuf(7 + stuf.st.pt) += 1
    IF intgrabber(stufbuf(7 + stuf.st.pt), stuf.min(stuf.st.pt), stuf.max(stuf.st.pt)) THEN stuf.st.need_update = YES
    stufbuf(7 + stuf.st.pt) -= 1
   CASE ELSE
    IF intgrabber(stufbuf(17 + stuf.st.pt - 3), stuf.min(stuf.st.pt), stuf.max(stuf.st.pt)) THEN
     stuf.st.need_update = YES
    END IF
  END SELECT

  usemenu stuf.st

  IF stuf.st.need_update THEN
   update_shop_stuff_menu stuf, stufbuf(), thing_last_id
  END IF

  clearpage dpage
  standardmenu stuf.menu(), stuf.st, , , dpage

  IF stuf.st.pt = 1 THEN  'thing ID selection
   textcolor uilook(uiDisabledItem), 0
   printstr "SHIFT + Left/Right to reorder", pInfoRight, pInfoY, dpage
  END IF

  IF show_stockidx THEN edgeprint "Stockidx " & stufbuf(37) - 1, pInfoX, pInfoY, uilook(uiMenuItem), dpage

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP

 shop_save_stf shop_id, stuf, stufbuf()

 'Re-save the shop last thing id in case it changed
 write_shop_stuff_count shop_id, thing_last_id
 
 RETURN stuf.thing
 
END FUNCTION

SUB shop_init_stf(byval shop_id as integer, stuf as ShopStuffState, stufbuf() as integer)
 flusharray stufbuf(), dimbinsize(binSTF), 0
 stufbuf(19) = -1 ' When adding new stuff, default in-stock to infinite
 stufbuf(37) = 1 + stuf.thing  'Set stockidx to next unused stock slot
 update_shop_stuff_type stuf, stufbuf(), YES  ' load the name and price
 shop_save_stf shop_id, stuf, stufbuf()
END SUB

SUB update_shop_stuff_type(byref stuf as ShopStuffState, stufbuf() as integer, byval reset_name_and_price as bool=NO)
 '--Re-load default names and default prices
 '--also reloads all limits
 SELECT CASE stufbuf(17)
  CASE 0' This is an item
   DIM item_tmp(dimbinsize(binITM)) as integer
   loaditemdata item_tmp(), stufbuf(18)
   stuf.item_value = item_tmp(46)
   IF reset_name_and_price THEN
    stuf.thingname = load_item_name(stufbuf(18),1,1)
    stufbuf(24) = stuf.item_value ' default buy price
    stufbuf(27) = stuf.item_value \ 2 ' default sell price
   END IF
   stuf.st.last = 22
   stuf.max(4) = gen(genMaxItem)
   IF stufbuf(18) > stuf.max(4) THEN stufbuf(18) = 0
   stuf.min(19) = 0
   stuf.max(19) = 3 ' Item sell-type
  CASE 1
   DIM her as HeroDef
   IF reset_name_and_price THEN
    loadherodata her, stufbuf(18)
    stuf.thingname = her.name
    stufbuf(24) = 0 ' default buy price
    stufbuf(27) = 0 ' default sell price
   END IF
   stuf.item_value = 0
   stuf.st.last = 19
   stuf.max(4) = gen(genMaxHero)
   IF stufbuf(18) > gen(genMaxHero) THEN stufbuf(18) = 0
   stuf.min(19) = -1
   stuf.max(19) = gen(genMaxLevel) ' Hero experience level
  CASE ELSE
   'Type 2 was script which was never supported but was allowed for data entry in some ancient versions
   stuf.thingname = "Unsupported"
 END SELECT

 stuf.max(3) = 1
 stuf.min(5) = -1
 stuf.max(5) = 9999
 FOR i as integer = 6 TO 9
  stuf.min(i) = -max_tag()
  stuf.max(i) = max_tag()
 NEXT i
 stuf.min(10) = -32767
 stuf.max(10) = 32767
 FOR i as integer = 11 TO 17 STEP 2
  stuf.max(i) = gen(genMaxItem)
  stuf.min(i) = -1
  stuf.max(i + 1) = 999
  stuf.min(i + 1) = 1
 NEXT

 stuf.min(20) = -32767
 stuf.max(20) = 32767
 stuf.max(21) = gen(genMaxItem)
 stuf.min(21) = -1
 stuf.max(22) = 999
 stuf.min(22) = 1

END SUB

SUB update_shop_stuff_menu (byref stuf as ShopStuffState, stufbuf() as integer, byval thing_last_id as integer)

 stuf.menu(0) = "Previous Menu"
 ' This is inaccurate; if there are 11 things we write "X of 10".
 stuf.menu(1) = CHR(27) & "Shop Thing " & stuf.thing & " of " & thing_last_id & CHR(26)
 stuf.menu(2) = "Name: " & stuf.thingname

 DIM typename as string
 DIM default_name as string
 SELECT CASE stufbuf(17)
  CASE 0:
   typename = "Item"
   default_name = readitemname(stufbuf(18))
  CASE 1:
   typename = "Hero"
   default_name = getheroname(stufbuf(18)) 
  CASE ELSE: typename = "???"
 END SELECT
 stuf.menu(3) = "Type: " & typename
 stuf.menu(4) = typename & " ID: " & stufbuf(18) & " " & default_name
 
 SELECT CASE stufbuf(19)
  CASE IS > 0: stuf.menu(5) = "In Stock: " & stufbuf(19)
  CASE 0: stuf.menu(5) = "In Stock: None"
  CASE -1: stuf.menu(5) = "In Stock: Infinite"
  CASE ELSE: stuf.menu(5) = stufbuf(19) & " ???" 
 END SELECT

 stuf.menu(6) = tag_condition_caption(stufbuf(20), "Buy Require Tag", "Always")
 stuf.menu(7) = tag_condition_caption(stufbuf(21), "Sell Require Tag", "Always")
 stuf.menu(8) = tag_set_caption(stufbuf(22), "Buy Set Tag")
 stuf.menu(9) = tag_set_caption(stufbuf(23), "Sell Set Tag")
 stuf.menu(10) = "Cost: " & price_string(stufbuf(24))
 IF stufbuf(17) = 0 AND stuf.item_value THEN  'item
  stuf.menu(10) &= " (" & CINT(100.0 * stufbuf(24) / stuf.item_value) & "% of Value)"
 END IF
 stuf.menu(11) = "Must Trade in " & (stufbuf(30) + 1) & " of: " & load_item_name(stufbuf(25),0,0)
 stuf.menu(12) = " (Change Amount)"
 stuf.menu(13) = "Must Trade in " & (stufbuf(32) + 1) & " of: " & load_item_name(stufbuf(31),0,0)
 stuf.menu(14) = " (Change Amount)"
 stuf.menu(15) = "Must Trade in " & (stufbuf(34) + 1) & " of: " & load_item_name(stufbuf(33),0,0)
 stuf.menu(16) = " (Change Amount)"
 stuf.menu(17) = "Must Trade in " & (stufbuf(36) + 1) & " of: " & load_item_name(stufbuf(35),0,0)
 stuf.menu(18) = " (Change Amount)"

 IF stufbuf(17) = 0 THEN

  SELECT CASE stufbuf(26)
   CASE 0: stuf.menu(19) = "Sell type: Don't Change Stock"
   CASE 1: stuf.menu(19) = "Sell type: Acquire Infinite Stock"
   CASE 2:
    stuf.menu(19) = "Sell type: Increment Stock"
    IF stufbuf(19) = -1 THEN
     stuf.menu(19) = "Sell type: Inc Stock (does nothing)"
    END IF
   CASE 3: stuf.menu(19) = "Sell type: Refuse to Buy"
   CASE ELSE: stuf.menu(19) = "Sell type: " & stufbuf(26) & " ???"
  END SELECT

  stuf.menu(20) = "Sell for: " & price_string(stufbuf(27))
  IF stuf.item_value THEN
   stuf.menu(20) &= " (" & CINT(100.0 * stufbuf(27) / stuf.item_value) & "% of Value)"
  END IF
  stuf.menu(21) = "  and " & (stufbuf(29) + 1) & " of: " & load_item_name(stufbuf(28),0,0)
  stuf.menu(22) = " (Change Amount)"
 ELSE
  stuf.menu(19) = "Experience Level: "
  IF stufbuf(26) = -1 THEN
   stuf.menu(19) &= "default"
  ELSE
   stuf.menu(19) &= stufbuf(26)
  END IF
 END IF
 
 stuf.st.need_update = NO
END SUB

' Read the selected shop thing record from .stf, with error checking
SUB shop_load_stf (byval shop_id as integer, byref stuf as ShopStuffState, stufbuf() as integer)
 flusharray stufbuf(), dimbinsize(binSTF), 0
 loadrecord stufbuf(), game & ".stf", getbinsize(binSTF) \ 2, shop_id * 50 + stuf.thing
 stuf.thingname = readbadbinstring(stufbuf(), 0, 16, 0)
 '---check for invalid data
 IF stufbuf(17) < 0 OR stufbuf(17) > 2 THEN stufbuf(17) = 0
 IF stufbuf(19) < -1 THEN stufbuf(19) = 0
 IF (stufbuf(26) < 0 OR stufbuf(26) > 3) AND stufbuf(17) <> 1 THEN stufbuf(26) = 0
 '--WIP Serendipity custom builds didn't flush shop records when upgrading properly
 FOR i as integer = 32 TO 41
  stufbuf(i) = large(stufbuf(i), 0)
 NEXT
 '--Upgrades
 IF stufbuf(37) = 0 THEN stufbuf(37) = stuf.thing + 1  'Initialise stockidx
END SUB

' Write the selected shop thing record to .stf
SUB shop_save_stf (byval shop_id as integer, byref stuf as ShopStuffState, stufbuf() as integer)
 writebadbinstring stuf.thingname, stufbuf(), 0, 16
 storerecord stufbuf(), game & ".stf", getbinsize(binSTF) \ 2, shop_id * 50 + stuf.thing
END SUB

' Swap two shop thing records
SUB shop_swap_stf (shop_id as integer, thing_id1 as integer, thing_id2 as integer)
 DIM size as integer = getbinsize(binSTF) \ 2
 DIM as integer stufbuf1(size), stufbuf2(size)
 loadrecord stufbuf1(), game & ".stf", size, shop_id * 50 + thing_id1
 loadrecord stufbuf2(), game & ".stf", size, shop_id * 50 + thing_id2
 storerecord stufbuf1(), game & ".stf", size, shop_id * 50 + thing_id2
 storerecord stufbuf2(), game & ".stf", size, shop_id * 50 + thing_id1
END SUB
