'OHRRPGCE COMMON - Game/Custom shared menu code
'
'Please read LICENSE.txt for GNU GPL details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#include "config.bi"
#include "udts.bi"
#include "const.bi"
#include "scrconst.bi"
#include "allmodex.bi"
#include "common.bi"
#include "menus.bi"

#IFDEF IS_GAME
DECLARE SUB embedtext (text AS STRING, limit AS INTEGER = 0)
#ENDIF

'Local functions
DECLARE SUB LoadMenuItems(menu_set AS MenuSet, dat AS MenuDef, record AS INTEGER)
DECLARE SUB LoadMenuItem(f AS INTEGER, items() AS MenuDefItem ptr, record AS INTEGER)
DECLARE SUB SaveMenuItems(menu_set AS MenuSet, dat AS MenuDef, record AS INTEGER)
DECLARE SUB SaveMenuItem(f AS INTEGER, mi AS MenuDefItem, record AS INTEGER, menunum AS INTEGER, itemnum AS INTEGER)


'==========================================================================================
'                                 Generic MenuState Stuff
'==========================================================================================


'(Re-)initialise menu state, preserving .pt if valid
'pickenabled: move .pt to an enabled menu item. (Note that .enabled may mean either "greyed out"
'or "unselectable")
SUB init_menu_state (BYREF state AS MenuState, menu() AS SimpleMenu, BYVAL pickenabled AS INTEGER = YES)
 WITH state
  .first = 0
  .last = UBOUND(menu)
  IF .size <= 0 THEN .size = 20
  .pt = bound(.pt, .first, .last)  '.first <= .last
  IF pickenabled THEN
   IF menu(.pt).enabled = NO THEN
    .pt = -1  'explicitly -1 when nothing selectable
    FOR i AS INTEGER = 0 TO UBOUND(menu)
     IF menu(i).enabled THEN .pt = i: EXIT FOR
    NEXT
   END IF
   'Menus with unselectable items have lookahead, which these +1,-1
   'attempt to simulate. Not perfect, but prevents some flickering
   IF .pt <> -1 THEN .top = bound(.top, .pt - .size + 1, .pt - 1)
  END IF
  .top = bound(.top, 0, large(.last - .size, 0))
 END WITH
END SUB

SUB clamp_menu_state (BYREF state AS MenuState)
 WITH state
  IF .pt < .top THEN .top = .pt
  IF .pt > .top + .size THEN .top = large(.top, .top + .size)
 END WITH
END SUB

'Simple... and yet, more options than a regular menu item
'Can also insert instead of appending... bad name
SUB append_simplemenu_item (menu() as SimpleMenu, caption as string, BYVAL enabled as integer = YES, BYVAL col as integer = -1, BYVAL dat as integer = 0, BYVAL where as integer = -1)
 IF col = -1 THEN col = uilook(uiText)
 IF where = -1 THEN
  REDIM PRESERVE menu(LBOUND(menu) TO UBOUND(menu) + 1)
  where = UBOUND(menu)
 END IF
 WITH menu(where)
  .text = caption
  .enabled = enabled
  .col = col
  .dat = dat
 END WITH
END SUB

FUNCTION usemenu (state AS MenuState, deckey = scUp, inckey = scDown) as integer
 WITH state
  RETURN usemenu(.pt, .top, .first, .last, .size, deckey, inckey)
 END WITH
END FUNCTION

FUNCTION usemenu (pt, top, first, last, size, deckey = scUp, inckey = scDown) as integer

oldptr = pt
oldtop = top

IF keyval(deckey) > 1 THEN pt = loopvar(pt, first, last, -1)
IF keyval(inckey) > 1 THEN pt = loopvar(pt, first, last, 1)
IF keyval(scPageup) > 1 THEN pt = pt - size
IF keyval(scPagedown) > 1 THEN pt = pt + size
IF keyval(scHome) > 1 THEN pt = first
IF keyval(scEnd) > 1 THEN pt = last
pt = small(large(pt, first), last)  '=last when last<first, ie. menu empty
top = bound(top, pt - size, pt)

IF oldptr = pt AND oldtop = top THEN
 usemenu = 0
ELSE
 usemenu = 1
END IF

END FUNCTION

FUNCTION usemenu (state as MenuState, menudata() as SimpleMenu, BYVAL deckey as integer = scUp, BYVAL inckey as integer = scDown) as integer
'a version for menus with unselectable items, skip items for which menudata().enabled = 0

WITH state
 '.pt = -1 when the menu has no selectable items
 IF .pt = -1 THEN RETURN 0

 oldptr = .pt
 oldtop = .top
 d = 0
 moved_d = 0

 IF keyval(deckey) > 1 THEN d = -1
 IF keyval(inckey) > 1 THEN d = 1
 IF keyval(scPageup) > 1 THEN
  .pt = large(.pt - .size, .first)
  WHILE menudata(.pt).enabled = 0 AND .pt > .first : .pt = loopvar(.pt, .first, .last, -1) : WEND
  IF menudata(.pt).enabled = 0 THEN d = 1
  moved_d = -1
 END IF
 IF keyval(scPagedown) > 1 THEN
  .pt = small(.pt + .size, .last)
  WHILE menudata(.pt).enabled = 0 AND .pt < .last : .pt = loopvar(.pt, .first, .last, 1) : WEND
  IF menudata(.pt).enabled = 0 THEN d = -1
  moved_d = 1
 END IF
 IF keyval(scHome) > 1 THEN .pt = .last : d = 1
 IF keyval(scEnd) > 1 THEN .pt = .first : d = -1

 IF d THEN 
  moved_d = d
  DO
   .top = bound(.top, .pt - .size, .pt)
   .pt = loopvar(.pt, .first, .last, d)
  LOOP WHILE menudata(.pt).enabled = 0
 END IF

 IF moved_d THEN
  'we look ahead of the actual cursor, to bring unselectable items at the ends of the menu into view
  DIM lookahead AS INTEGER = .pt
  DO
   lookahead += moved_d
  LOOP WHILE bound(lookahead, .first, .last) = lookahead ANDALSO menudata(lookahead).enabled = 0
  lookahead = bound(lookahead, .first, .last)
  .top = bound(.top, lookahead - .size, lookahead)
 END IF
 .top = bound(.top, .pt - .size, .pt)

 IF oldptr = .pt AND oldtop = .top THEN
  usemenu = 0
 ELSE
  usemenu = 1
 END IF
END WITH
END FUNCTION

FUNCTION usemenu (state as MenuState, enabled() as INTEGER, BYVAL deckey as integer = scUp, BYVAL inckey as integer = scDown) as integer
'a version for menus with unselectable items, skip items for which enabled = 0

WITH state
 '.pt = -1 when the menu has no selectable items
 IF .pt = -1 THEN RETURN 0

 oldptr = .pt
 oldtop = .top
 d = 0
 moved_d = 0

 IF keyval(deckey) > 1 THEN d = -1
 IF keyval(inckey) > 1 THEN d = 1
 IF keyval(scPageup) > 1 THEN
  .pt = large(.pt - .size, .first)
  WHILE enabled(.pt) = 0 AND .pt > .first : .pt = loopvar(.pt, .first, .last, -1) : WEND
  IF enabled(.pt) = 0 THEN d = 1
  moved_d = -1
 END IF
 IF keyval(scPagedown) > 1 THEN
  .pt = small(.pt + .size, .last)
  WHILE enabled(.pt) = 0 AND .pt < .last : .pt = loopvar(.pt, .first, .last, 1) : WEND
  IF enabled(.pt) = 0 THEN d = -1
  moved_d = 1
 END IF
 IF keyval(scHome) > 1 THEN .pt = .last : d = 1
 IF keyval(scEnd) > 1 THEN .pt = .first : d = -1

 IF d THEN
  moved_d = d
  DO
   .top = bound(.top, .pt - .size, .pt)
   .pt = loopvar(.pt, .first, .last, d)
  LOOP WHILE enabled(.pt) = 0
 END IF

 IF moved_d THEN
  'we look ahead of the actual cursor, to bring unselectable items at the ends of the menu into view
  DIM lookahead AS INTEGER = .pt
  DO
   lookahead += moved_d
  LOOP WHILE bound(lookahead, .first, .last) = lookahead ANDALSO enabled(lookahead) = 0
  lookahead = bound(lookahead, .first, .last)
  .top = bound(.top, lookahead - .size, lookahead)
 END IF
 .top = bound(.top, .pt - .size, .pt)

 IF oldptr = .pt AND oldtop = .top THEN
  usemenu = 0
 ELSE
  usemenu = 1
 END IF
END WITH
END FUNCTION

'scrollmenu is like usemenu for menus where no menu item is selected:
'you just want to scroll a menu up and down (modifies .top; .pt is ignored).
FUNCTION scrollmenu (state AS MenuState, BYVAL deckey as integer = scUp, BYVAL inckey as integer = scDown) as integer
 WITH state
  DIM oldtop as integer = .top
  DIM lasttop as integer = large(.first, .last - .size)
  IF keyval(deckey) > 1 THEN .top = loopvar(.top, .first, lasttop, -1)
  IF keyval(inckey) > 1 THEN .top = loopvar(.top, .first, lasttop, 1)
  IF keyval(scPageup) > 1 THEN .top = large(.first, .top - .size)
  IF keyval(scPagedown) > 1 THEN .top = small(lasttop, .top + .size)
  IF keyval(scHome) > 1 THEN .top = .first
  IF keyval(scEnd) > 1 THEN .top = lasttop
  RETURN (.top <> oldtop)
 END WITH
END FUNCTION

SUB standardmenu (menu() AS STRING, state AS MenuState, x AS INTEGER, y AS INTEGER, page AS INTEGER, edge AS INTEGER=NO, hidecursor AS INTEGER=NO, wide AS INTEGER=999, highlight AS INTEGER=NO, toggle=YES)
 DIM p AS INTEGER
 WITH state
  p = .pt
  IF hidecursor THEN p = .first - 1
  standardmenu menu(), .last, .size, p, .top, x, y, page, edge, wide, highlight, NULL, toggle
 END WITH
END SUB

'Version which allows items to be greyed out/disabled/shaded
SUB standardmenu (menu() AS STRING, state AS MenuState, shaded() AS INTEGER, x AS INTEGER, y AS INTEGER, page AS INTEGER, edge AS INTEGER=NO, hidecursor AS INTEGER=NO, wide AS INTEGER=999, highlight AS INTEGER=NO, toggle=YES)
 DIM p AS INTEGER
 IF LBOUND(shaded) > LBOUND(menu) OR UBOUND(shaded) < UBOUND(menu) THEN fatalerror "standardmenu: shaded() too small"
 WITH state
  p = .pt
  IF hidecursor THEN p = .first - 1
  standardmenu menu(), .last, .size, p, .top, x, y, page, edge, wide, highlight, @shaded(0), toggle
 END WITH
END SUB

SUB standardmenu (menu() AS STRING, size, vis, pt, top, x, y, page, edge=NO, wide=999, highlight=NO, shaded AS INTEGER PTR=NULL, toggle=YES)
'the default for wide is 999 until I know whether it'd break anything to set it to 40
STATIC rememtog
DIM tog AS INTEGER
DIM i AS INTEGER
DIM col AS INTEGER
DIM text AS STRING

IF toggle THEN
 rememtog = rememtog XOR 1
 tog = rememtog
ELSE
 'This menu isn't the active one or we otherwise don't want it to animate
 tog = 0
END IF

FOR i = top TO top + vis
 IF i <= size THEN
  text = menu(i)
  IF pt = i THEN text = RIGHT(text, wide)
  IF pt = i AND highlight THEN
   DIM w AS INTEGER
   w = 8 * LEN(text)
   IF w = 0 THEN w = small(wide * 8, 320)
   rectangle x + 0, y + (i - top) * 8, w, 8, uilook(uiHighlight), page
  END IF
  IF shaded ANDALSO shaded[i] THEN
   col = uilook(uiDisabledItem)
   IF pt = i THEN col = uilook(uiSelectedDisabled + tog)
  ELSE
   col = uilook(uiMenuItem)
   IF pt = i THEN col = uilook(uiSelectedItem + tog)
  END IF
  IF edge THEN
   edgeprint text, x + 0, y + (i - top) * 8, col, page, YES
  ELSE
   textcolor col, 0
   printstr text, x + 0, y + (i - top) * 8, page, YES
  END IF
 END IF
NEXT i

END SUB


'==========================================================================================
'                                         MenuDef
'==========================================================================================


'This initialises a menu if it has not been already
SUB ClearMenuData(dat AS MenuDef)
 DIM bits(0) AS INTEGER
 WITH dat
  .record = -1
  .handle = 0
  .name = ""
  .boxstyle = 0
  .textcolor = 0
  .maxrows = 0
  .offset.x = 0
  .offset.y = 0
  .anchor.x = 0
  .anchor.y = 0
  .align = 0
  .min_chars = 0
  .max_chars = 0
  .bordersize = 0
  IF .items THEN
   DeleteMenuItems dat
  ELSE
   dlist_construct .itemlist, OFFSETOF(MenuDefItem, trueorder)
  END IF
 END WITH
 bits(0) = 0
 MenuBitsFromArray dat, bits()
END SUB

SUB DeleteMenuItems(menu AS MenuDef)
 DIM i AS INTEGER
 WITH menu
  FOR i = 0 TO .numitems - 1
   dlist_remove menu.itemlist, .items[i]
   DELETE .items[i]
  NEXT i
  DEALLOCATE(.items)
  .items = NULL
 END WITH
END SUB

'This is not used anywhere currently. This has nothing to do with deleting a menu item!
SUB ClearMenuItem(mi AS MenuDefItem)
 DIM bits(0) AS INTEGER
 DIM i AS INTEGER
 WITH mi
  .caption = ""
  .t = 0
  .sub_t = 0
  .tag1 = 0
  .tag2 = 0
  .settag = 0
  .togtag = 0
  FOR i = 0 TO 2
   .extra(i) = 0
  NEXT i
 END WITH
 bits(0) = 0
 MenuItemBitsFromArray mi, bits()
END SUB

'recreate a menu's items[] array, which sorts visible items to the top
SUB SortMenuItems(menu AS MenuDef)
 DIM AS INTEGER i, j, lowest, found
 DIM mi AS MenuDefItem ptr
 IF menu.numitems = 0 THEN
  DEALLOCATE(menu.items)
  menu.items = NULL
  EXIT SUB
 END IF
 menu.items = REALLOCATE(menu.items, SIZEOF(any ptr) * menu.numitems)
 'stick all visible items in .items[]
 i = 0
 mi = menu.first
 WHILE mi
  IF (mi->disabled AND mi->hide_if_disabled) = 0 THEN
   menu.items[i] = mi
   i += 1
  END IF
  mi = mi->trueorder.next
 WEND
 'append all invisible items
 mi = menu.first
 WHILE mi
  IF mi->disabled AND mi->hide_if_disabled THEN
   menu.items[i] = mi
   i += 1
  END IF
  mi = mi->trueorder.next
 WEND
END SUB

FUNCTION getmenuname(record AS INTEGER) AS STRING
 DIM as string ret
#IFDEF IS_GAME
 STATIC cache(32) as IntStrPair
 ret = search_string_cache(cache(), record, game)
 IF ret <> "" THEN RETURN ret
#ENDIF

 DIM menu_set AS MenuSet
 menu_set.menufile = workingdir + SLASH + "menus.bin"
 menu_set.itemfile = workingdir + SLASH + "menuitem.bin"
 DIM menu AS MenuDef
 LoadMenuData menu_set, menu, record, YES
 ret = menu.name
 ClearMenuData menu

#IFDEF IS_GAME
 add_string_cache cache(), record, ret
#ENDIF
 RETURN ret
END FUNCTION

'(Re-)initialise menu state, preserving .pt if valid
SUB init_menu_state (BYREF state AS MenuState, menu AS MenuDef)
 WITH state
  .first = 0
  .last = count_menu_items(menu) - 1
  .size = menu.maxrows - 1
  IF .size = -1 THEN .size = 17
  .pt = small(large(.pt, .first), .last)  'explicitly -1 when empty
  IF .pt <> -1 THEN .top = bound(.top, .pt - .size, .pt)
  .top = bound(.top, 0, large(.last - .size, 0))
 END WITH
END SUB

FUNCTION append_menu_item(BYREF menu AS MenuDef, caption AS STRING, t AS INTEGER=0, sub_t AS INTEGER=0) as integer
 DIM i AS INTEGER
 DIM item AS MenuDefItem ptr
 item = NEW MenuDefItem
 WITH *item
  .caption = caption
  .t = t
  .sub_t = sub_t
 END WITH

 dlist_append(menu.itemlist, item) 'updates .numitems

 'rather than call SortMenuItems, shuffle hidden items down a slot and insert new item
 menu.items = REALLOCATE(menu.items, menu.numitems * SIZEOF(any ptr))
 FOR i = menu.numitems - 2 TO 0 STEP -1  'last item in array is garbage
  IF menu.items[i]->disabled AND menu.items[i]->hide_if_disabled THEN
   SWAP menu.items[i], menu.items[i + 1]
  ELSE
   EXIT FOR
  END IF
 NEXT
 menu.items[i + 1] = item

 RETURN menu.numitems - 1
END FUNCTION

SUB remove_menu_item(BYREF menu AS MenuDef, BYVAL mi AS MenuDefItem ptr)
 dlist_remove menu.itemlist, mi
 DELETE mi
 'rebuild menu.items[]
 SortMenuItems menu
END SUB

SUB remove_menu_item(BYREF menu AS MenuDef, BYVAL mislot AS INTEGER)
 remove_menu_item menu, menu.items[mislot]
END SUB

SUB swap_menu_items(BYREF menu1 AS MenuDef, BYVAL mislot1 AS INTEGER, BYREF menu2 AS MenuDef, BYVAL mislot2 AS INTEGER)
 dlist_swap(menu1.itemlist, menu1.items[mislot1], menu2.itemlist, menu2.items[mislot2])
 SortMenuItems menu1
 SortMenuItems menu2
END SUB


'==========================================================================================
'                        Saving/Loading/(De)serializing MenuDefs
'==========================================================================================


SUB LoadMenuData(menu_set AS MenuSet, dat AS MenuDef, record AS INTEGER, ignore_items AS INTEGER=NO)
 DIM f AS INTEGER
 DIM bits(0) AS INTEGER
 IF record > gen(genMaxMenu) OR record < 0 THEN
  ClearMenuData dat
  EXIT SUB
 END IF
 f = FREEFILE
 OPEN menu_set.menufile FOR BINARY AS #f
 SEEK #f, record * getbinsize(binMENUS) + 1
 WITH dat
  .record = record
  .name = ReadByteStr(f, 20)
  .boxstyle = ReadShort(f)
  .textcolor = ReadShort(f)
  .maxrows = ReadShort(f)
  bits(0) = ReadShort(f)
  MenuBitsFromArray dat, bits()
  .offset.x = ReadShort(f)
  .offset.y = ReadShort(f)
  .anchor.x = ReadShort(f)
  .anchor.y = ReadShort(f)
  .align = ReadShort(f)
  .min_chars = ReadShort(f)
  .max_chars = ReadShort(f)
  .bordersize = ReadShort(f)
  .on_close = ReadShort(f)
  .esc_menu = ReadShort(f)
  IF .items THEN
   DeleteMenuItems dat
  ELSE
   dlist_construct .itemlist, OFFSETOF(MenuDefItem, trueorder)
  END IF
 END WITH
 CLOSE #f
 IF ignore_items = NO THEN 'This is disableable for performance when all you care about loading is the menu's name
  LoadMenuItems menu_set, dat, record
 END IF
END SUB

SUB LoadMenuItems(menu_set AS MenuSet, menu AS MenuDef, record AS INTEGER)
 DIM i AS INTEGER
 DIM f AS INTEGER
 DIM member AS INTEGER
 DIM actual_record_count AS INTEGER = 0
 'The items may appear out-of-order in menuitem.bin, so rather than just append them as
 'we find the, first we store them in this temp array:
 REDIM itemarray(0) AS MenuDefItem ptr

 f = FREEFILE
 OPEN menu_set.itemfile FOR BINARY AS #f
 'FIXME: this shouldn't be here, it's covered in upgrade() (but commented out currently)
 actual_record_count = LOF(f) / getbinsize(binMENUITEM)
 IF actual_record_count <> gen(genMaxMenuItem) + 1 THEN
  debug "menuitem.bin record count sanity check failed " & gen(genMaxMenuItem) & "->" & actual_record_count - 1
  gen(genMaxMenuItem) = actual_record_count - 1
 END IF
 FOR i = 0 TO gen(genMaxMenuItem)
  SEEK #f, i * getbinsize(binMENUITEM) + 1
  member = ReadShort(f)
  IF member = record + 1 THEN
   LoadMenuItem f, itemarray(), i
  END IF
 NEXT i
 CLOSE #f

 'build the item list
 FOR i = 0 TO UBOUND(itemarray)
  IF itemarray(i) <> NULL THEN
   dlist_append(menu.itemlist, itemarray(i))
  ELSE
   'can't create a zero length FB array
   IF UBOUND(itemarray) <> 0 THEN
    debug "menu " & record & " item " & i & " could not be found in " & menu_set.itemfile
   END IF
  END IF
 NEXT
 'build the items[] array
 SortMenuItems menu
END SUB

SUB LoadMenuItem(f AS INTEGER, items() AS MenuDefItem ptr, record AS INTEGER)
 DIM i AS INTEGER
 DIM bits(0) AS INTEGER
 DIM mi AS MenuDefItem ptr
 DIM itemnum AS INTEGER
 mi = NEW MenuDefItem
 SEEK #f, record * getbinsize(binMENUITEM) + 1
 WITH *mi
  ReadShort(f) 'throw away member
  .caption = ReadByteStr(f, 38)
  itemnum = ReadShort(f)
  .t = ReadShort(f)
  .sub_t = ReadShort(f)
  .tag1 = ReadShort(f)
  .tag2 = ReadShort(f)
  .settag = ReadShort(f)
  .togtag = ReadShort(f)
  bits(0) = ReadShort(f)
  FOR i = 0 TO 2
   .extra(i) = ReadShort(f)
  NEXT i
 END WITH
 IF itemnum > UBOUND(items) THEN REDIM PRESERVE items(itemnum)
 items(itemnum) = mi
 MenuItemBitsFromArray *mi, bits()
END SUB

SUB SaveMenuData(menu_set AS MenuSet, dat AS MenuDef, record AS INTEGER)
 DIM f AS INTEGER
 DIM bits(0) AS INTEGER
 f = FREEFILE
 OPEN menu_set.menufile FOR BINARY AS #f
 SEEK #f, record * getbinsize(binMENUS) + 1
 WITH dat
  WriteByteStr(f, 20, .name)
  WriteShort(f, -1, .boxstyle)
  WriteShort(f, -1, .textcolor)
  WriteShort(f, -1, .maxrows)
  MenuBitsToArray dat, bits()
  WriteShort(f, -1, bits(0))
  WriteShort(f, -1, .offset.x)
  WriteShort(f, -1, .offset.y)
  WriteShort(f, -1, .anchor.x)
  WriteShort(f, -1, .anchor.y)
  WriteShort(f, -1, .align)
  WriteShort(f, -1, .min_chars)
  WriteShort(f, -1, .max_chars)
  WriteShort(f, -1, .bordersize)
  WriteShort(f, -1, .on_close)
  WriteShort(f, -1, .esc_menu)
 END WITH
 CLOSE #f
 SaveMenuItems menu_set, dat, record
END SUB

SUB SaveMenuItems(menu_set AS MenuSet, menu AS MenuDef, record AS INTEGER)
 DIM i AS INTEGER
 DIM f AS INTEGER
 DIM member AS INTEGER
 DIM elem AS INTEGER = 0
 DIM mi AS MenuDefItem ptr
 DIM blankmi AS MenuDefItem
 
 f = FREEFILE
 OPEN menu_set.itemfile FOR BINARY AS #f
 'Loop through each record and orphan all old entries for this menu
 FOR i = 0 TO gen(genMaxMenuItem)
  SEEK #f, i * getbinsize(binMENUITEM) + 1
  member = ReadShort(f)
  IF member = record + 1 THEN
   SaveMenuItem f, blankmi, i, -1, 0
  END IF
 NEXT i
 'Loop through each record, writing new values into orphan slots
 mi = menu.first
 FOR i = 0 TO gen(genMaxMenuItem)
  SEEK #f, i * getbinsize(binMENUITEM) + 1
  member = ReadShort(f)
  IF member = 0 THEN
   IF mi = NULL THEN EXIT FOR
   SaveMenuItem f, *mi, i, record, elem
   elem = elem + 1
   mi = mi->trueorder.next
  END IF
 NEXT i
 DO WHILE mi
  'More items need to be written, append them
  gen(genMaxMenuItem) += 1
  SaveMenuItem f, *mi, gen(genMaxMenuItem), record, elem
  elem += 1
  mi = mi->trueorder.next
 LOOP
 CLOSE #f
END SUB

SUB SaveMenuItem(f AS INTEGER, mi AS MenuDefItem, record AS INTEGER, menunum AS INTEGER, itemnum AS INTEGER)
 DIM i AS INTEGER
 DIM bits(0) AS INTEGER
 SEEK #f, record * getbinsize(binMENUITEM) + 1
 WITH mi
  WriteShort(f, -1, menunum + 1)
  WriteByteStr(f, 38, .caption)
  WriteShort(f, -1, itemnum)
  WriteShort(f, -1, .t)
  WriteShort(f, -1, .sub_t)
  WriteShort(f, -1, .tag1)
  WriteShort(f, -1, .tag2)
  WriteShort(f, -1, .settag)
  WriteShort(f, -1, .togtag)
  MenuItemBitsToArray mi, bits()
  WriteShort(f, -1, bits(0))
  FOR i = 0 TO 2
   WriteShort(f, -1, .extra(i))
  NEXT i
 END WITH
END SUB

SUB MenuBitsToArray (menu AS MenuDef, bits() AS INTEGER)
 bits(0) = 0
 WITH menu
  setbit bits(), 0, 0, .translucent
  setbit bits(), 0, 1, .no_scrollbar
  setbit bits(), 0, 2, .allow_gameplay
  setbit bits(), 0, 3, .suspend_player
  setbit bits(), 0, 4, .no_box
  setbit bits(), 0, 5, .no_close
  setbit bits(), 0, 6, .no_controls
  setbit bits(), 0, 7, .prevent_main_menu
  setbit bits(), 0, 8, .advance_textbox
 END WITH
END SUB

SUB MenuBitsFromArray (menu AS MenuDef, bits() AS INTEGER)
 WITH menu
  .translucent    = xreadbit(bits(), 0)
  .no_scrollbar   = xreadbit(bits(), 1)
  .allow_gameplay = xreadbit(bits(), 2)
  .suspend_player = xreadbit(bits(), 3)
  .no_box         = xreadbit(bits(), 4)
  .no_close       = xreadbit(bits(), 5)
  .no_controls    = xreadbit(bits(), 6)
  .prevent_main_menu = xreadbit(bits(), 7)
  .advance_textbox   = xreadbit(bits(), 8)
 END WITH
END SUB

SUB MenuItemBitsToArray (mi AS MenuDefItem, bits() AS INTEGER)
 bits(0) = 0
 WITH mi
  setbit bits(), 0, 0, .hide_if_disabled
  setbit bits(), 0, 1, .close_if_selected
  setbit bits(), 0, 2, .skip_close_script
 END WITH
END SUB

SUB MenuItemBitsFromArray (mi AS MenuDefItem, bits() AS INTEGER)
 WITH mi
  .hide_if_disabled  = xreadbit(bits(), 0)
  .close_if_selected = xreadbit(bits(), 1)
  .skip_close_script = xreadbit(bits(), 2)
 END WITH
END SUB

FUNCTION read_menu_int (menu AS MenuDef, intoffset AS INTEGER) as integer
 '--This function allows read access to integers in a menu for the plotscripting interface
 '--intoffset is the integer offset, same as appears in the MENUS.BIN lump documentation
 DIM bits(0) AS INTEGER
 WITH menu
  SELECT CASE intoffset
   CASE 12: RETURN .boxstyle
   CASE 13: RETURN .textcolor
   CASE 14: RETURN .maxrows
   CASE 15:
    MenuBitsToArray menu, bits()
    RETURN bits(0)
   CASE 16: RETURN .offset.x
   CASE 17: RETURN .offset.y
   CASE 18: RETURN .anchor.x
   CASE 19: RETURN .anchor.y
   CASE 20: RETURN .align
   CASE 21: RETURN .min_chars
   CASE 22: RETURN .max_chars
   CASE 23: RETURN .bordersize
   CASE 24: RETURN .on_close
   CASE 25: RETURN .esc_menu
   CASE ELSE
    debug "read_menu_int: " & intoffset & " is an invalid integer offset"
  END SELECT
 END WITH
 RETURN 0
END FUNCTION

SUB write_menu_int (menu AS MenuDef, intoffset AS INTEGER, n AS INTEGER)
 '--This sub allows write access to integers in a menu for the plotscripting interface
 '--intoffset is the integer offset, same as appears in the MENUS.BIN lump documentation
 DIM bits(0) AS INTEGER
 WITH menu
  SELECT CASE intoffset
   CASE 12: .boxstyle = n
   CASE 13: .textcolor = n
   CASE 14: .maxrows = n
   CASE 15:
    bits(0) = n
    MenuBitsFromArray menu, bits()
   CASE 16: .offset.x = n
   CASE 17: .offset.y = n
   CASE 18: .anchor.x = n
   CASE 19: .anchor.y = n
   CASE 20: .align = n
   CASE 21: .min_chars = n
   CASE 22: .max_chars = n
   CASE 23: .bordersize = n
   CASE 24: .on_close = n
   CASE 25: .esc_menu = n
   CASE ELSE
    debug "write_menu_int: " & intoffset & " is an invalid integer offset"
  END SELECT
 END WITH
END SUB

FUNCTION read_menu_item_int (mi AS MenuDefItem, intoffset AS INTEGER) as integer
 '--This function allows read access to integers in a menu item for the plotscripting interface
 '--intoffset is the integer offset, same as appears in the MENUITEM.BIN lump documentation
 DIM bits(0) AS INTEGER
 WITH mi
  SELECT CASE intoffset
   CASE 22: RETURN .t
   CASE 23: RETURN .sub_t
   CASE 24: RETURN .tag1
   CASE 25: RETURN .tag2
   CASE 26: RETURN .settag
   CASE 27: RETURN .togtag
   CASE 28:
    MenuItemBitsToArray mi, bits()
    RETURN bits(0)
   CASE 29 TO 31: RETURN .extra(intoffset - 29)
   CASE ELSE
    debug "read_menu_item_int: " & intoffset & " is an invalid integer offset"
  END SELECT
 END WITH
 RETURN 0
END FUNCTION

SUB write_menu_item_int (mi AS MenuDefItem, intoffset AS INTEGER, n AS INTEGER)
 '--This sub allows write access to integers in a menu item for the plotscripting interface
 '--intoffset is the integer offset, same as appears in the MENUITEM.BIN lump documentation
 DIM bits(0) AS INTEGER
 WITH mi
  SELECT CASE intoffset
   CASE 22: .t = n
   CASE 23: .sub_t = n
   CASE 24: .tag1 = n
   CASE 25: .tag2 = n
   CASE 26: .settag = n
   CASE 27: .togtag = n
   CASE 28:
    bits(0) = n
    MenuItemBitsFromArray mi, bits()
   CASE 29 TO 31: .extra(intoffset - 29) = n
   CASE ELSE
    debug "write_menu_item_int: " & intoffset & " is an invalid integer offset"
  END SELECT
 END WITH
END SUB


'==========================================================================================
'                                    Drawing MenuDefs
'==========================================================================================


SUB draw_menu (menu AS MenuDef, state AS MenuState, page AS INTEGER)
 DIM i AS INTEGER
 DIM elem AS INTEGER
 DIM cap AS STRING
 DIM col AS INTEGER
 DIM where AS XYPair

 'we actually calculate each menu item caption twice: once also in position_menu
 position_menu menu, page
 
 WITH menu.rect
  IF menu.no_box = NO THEN
   edgeboxstyle .x, .y, .wide, .high, menu.boxstyle, page, menu.translucent
  END IF
  IF menu.no_scrollbar = NO THEN
   draw_scrollbar state, menu, page
  END IF
 END WITH

 state.tog = state.tog XOR 1

 FOR i = 0 TO state.size
  elem = state.top + i
  IF elem >= 0 AND elem < menu.numitems THEN
   col = menu.textcolor
   IF col = 0 THEN col = uilook(uiMenuItem)
   IF state.pt = elem AND state.active THEN col = uilook(uiSelectedItem + state.tog)
   WITH *menu.items[elem]
    IF .disabled THEN
     col = uilook(uiDisabledItem)
     IF state.pt = elem AND state.active THEN col = uilook(uiSelectedDisabled + state.tog)
    END IF
    IF NOT (.disabled AND .hide_if_disabled) THEN
     cap = get_menu_item_caption(*menu.items[elem], menu)
     position_menu_item menu, cap, i, where
     IF .t = 1 AND .sub_t = 11 THEN ' volume meter
      edgeboxstyle where.x, where.y, get_music_volume * 48, 10, menu.boxstyle, page, NO, YES
     END IF
     edgeprint cap, where.x, where.y, col, page
    END IF
   END WITH
  END IF
 NEXT i
 
END SUB

SUB position_menu_item (menu AS MenuDef, cap AS STRING, i AS INTEGER, BYREF where AS XYPair)
 DIM bord AS INTEGER
 bord = 8 + menu.bordersize
 WITH menu.rect
  SELECT CASE menu.align
   CASE -1
    where.x = .x + bord
   CASE 0
    where.x = .x + .wide / 2 - LEN(cap) * 4
   CASE 1
    where.x = .x + .wide - bord - LEN(cap) * 8
  END SELECT
  where.y = .y + bord + (i * 10)
 END WITH
END SUB

SUB position_menu (menu AS MenuDef, page AS INTEGER)
 DIM i AS INTEGER
 DIM cap AS STRING
 DIM bord AS INTEGER
 bord = 8 + menu.bordersize

 menu.rect.wide = bord * 2
 menu.rect.high = bord * 2

 FOR i = 0 TO menu.numitems - 1
  WITH *menu.items[i]
   cap = get_menu_item_caption(*menu.items[i], menu)
   menu.rect.wide = large(menu.rect.wide, LEN(cap) * 8 + bord * 2)
   IF .disabled AND .hide_if_disabled THEN CONTINUE FOR 'hidden matter for auto-width but not auto-height
   menu.rect.high = menu.rect.high + 10
  END WITH
 NEXT i
 '--enforce min width
 menu.rect.wide = large(menu.rect.wide, menu.min_chars * 8 + bord * 2)
 '--enforce screen boundaries
 menu.rect.wide = small(menu.rect.wide, vpages(page)->w)
 menu.rect.high = small(menu.rect.high, vpages(page)->h)
 IF menu.maxrows > 0 THEN menu.rect.high = small(menu.rect.high, menu.maxrows * 10 + bord * 2)

 WITH menu
  .rect.x = vpages(page)->w \ 2 - anchor_point(.anchor.x, .rect.wide) + menu.offset.x
  .rect.y = vpages(page)->h \ 2 - anchor_point(.anchor.y, .rect.high) + menu.offset.y
 END WITH
END SUB

FUNCTION anchor_point(anchor AS INTEGER, size AS INTEGER) AS INTEGER
 SELECT CASE anchor
  CASE -1
   RETURN 0
  CASE 0
   RETURN size \ 2
  CASE 1
   RETURN size
 END SELECT
END FUNCTION

FUNCTION count_menu_items (menu AS MenuDef) as integer
 DIM i AS INTEGER
 DIM count AS INTEGER = 0
 FOR i = 0 TO menu.numitems - 1
  WITH *menu.items[i]
   IF .disabled AND .hide_if_disabled THEN CONTINUE FOR
   count += 1
  END WITH
 NEXT i
 RETURN count
END FUNCTION

FUNCTION get_menu_item_caption (mi AS MenuDefItem, menu AS MenuDef) AS STRING
 DIM cap AS STRING
 cap = mi.caption
 IF LEN(cap) = 0 THEN
  'No caption, use the default
  SELECT CASE mi.t
   CASE 1 ' special screen
    cap = get_special_menu_caption(mi.sub_t, menu.edit_mode)
   CASE 2 ' another menu
    cap = getmenuname(mi.sub_t)
    IF cap = "" THEN cap = "Menu " & mi.sub_t
   CASE 3 ' Text Box
    cap = "Text Box " & mi.sub_t
   CASE 4 ' Run Script
    cap = scriptname(mi.sub_t, plottrigger)
  END SELECT
 END IF
 IF menu.edit_mode = YES AND LEN(TRIM(cap)) = 0 THEN cap = "[BLANK]" 
 #IFDEF IS_GAME
  embedtext cap
 #ENDIF
 IF menu.max_chars > 0 THEN ' Crop overlength
  IF menu.align = 1 THEN ' right align
   cap = RIGHT(cap, menu.max_chars)
  ELSE ' left and center align
   cap = LEFT(cap, menu.max_chars)
  END IF
 END IF
 RETURN cap
END FUNCTION

FUNCTION get_special_menu_caption(subtype AS INTEGER, edit_mode AS INTEGER= NO) AS STRING
 DIM cap AS STRING
 SELECT CASE subtype
  CASE 0: cap = readglobalstring(60, "Items", 10)
  CASE 1: cap = readglobalstring(61, "Spells", 10)
  CASE 2: cap = readglobalstring(62, "Status", 10)
  CASE 3: cap = readglobalstring(63, "Equip", 10)
  CASE 4: cap = readglobalstring(64, "Order", 10)
  CASE 5: cap = readglobalstring(65, "Team", 10)
  CASE 6
   IF readbit(gen(), genBits, 5) THEN
    cap = readglobalstring(65, "Team", 10)
   ELSE
    cap = readglobalstring(64, "Order", 10)
   END IF
   IF edit_mode = YES THEN cap = cap & " [general bitset]"
  CASE 7,12:
   cap = readglobalstring(68, "Map", 10)
   IF subtype = 7 AND edit_mode = YES THEN cap = cap & " [if allowed by map]"
  CASE 8,13:
   cap = readglobalstring(66, "Save", 10)
   IF subtype = 8 AND edit_mode = YES THEN cap = cap & " [if allowed by map]"
  CASE 9: cap = "Load" ' FIXME: Needs a global text string
  CASE 10: cap = readglobalstring(67, "Quit", 10)
  CASE 11: cap = readglobalstring(69, "Volume", 10)
 END SELECT
 RETURN cap
END FUNCTION


'==========================================================================================
'                                       Scrollbars!
'==========================================================================================


SUB draw_scrollbar(state AS MenuState, menu AS MenuDef, page AS INTEGER)
 draw_scrollbar state, menu.rect, menu.boxstyle, page
END SUB

SUB draw_scrollbar(state AS MenuState, rect AS RectType, boxstyle AS INTEGER=0, page AS INTEGER)
 DIM count AS INTEGER = state.last - state.first + 1
 draw_scrollbar state, rect, count, boxstyle, page
END SUB

'count being the number of (visible) menu items
SUB draw_scrollbar(state AS MenuState, rect AS RectType, count AS INTEGER, boxstyle AS INTEGER=0, page AS INTEGER)
 'recall state.size is off-by-1
 IF state.top > state.first OR count > (state.size + 1) THEN
  IF count > 0 THEN
   DIM sbar AS RectType
   DIM slider AS RectType
   sbar.x = rect.x + rect.wide - 6
   sbar.y = rect.y + 2
   sbar.wide = 4
   sbar.high = rect.high - 4
   WITH sbar
    slider.y = .high / count * (state.top - state.first)
    slider.high = .high / count * (state.size + 1)
    rectangle .x, .y, .wide, .high, uilook(uiBackground), page
    rectangle .x, .y + slider.y, .wide, slider.high, uilook(uiTextBox + boxstyle * 2 + 1), page
   END WITH
  END IF
 END IF
END SUB

SUB draw_fullscreen_scrollbar(state AS MenuState, boxstyle AS INTEGER=0, page AS INTEGER)
 DIM rect AS RectType
 rect.wide = 320
 rect.high = 200
 draw_scrollbar state, rect, boxstyle, page
END SUB
