'OHRRPGCE - Definitions of some menus which are common to Custom + Game
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#include "config.bi"
#include "allmodex.bi"
#include "udts.bi"
#include "uiconst.bi"
#include "common.bi"
#include "slices.bi"
#include "reload.bi"
#include "loading.bi"


'Globals

REDIM npc_movetypes(15) as string
npc_movetypes(0) = "Stand Still"
npc_movetypes(1) = "Wander"
npc_movetypes(2) = "Pace"
npc_movetypes(3) = "Right Turns"
npc_movetypes(4) = "Left Turns"
npc_movetypes(5) = "Random Turns"
npc_movetypes(6) = "Chase You (Meandering)"
npc_movetypes(7) = "Avoid You (Meandering)"
npc_movetypes(8) = "Walk In Place"
npc_movetypes(9) = "Chase You (Direct)"
npc_movetypes(10) = "Avoid You (Direct)"
npc_movetypes(11) = "Follow walls (Right)"
npc_movetypes(12) = "Follow walls (Left)"
npc_movetypes(13) = "Follow walls (R) stop for others"
npc_movetypes(14) = "Follow walls (L) stop for others"
npc_movetypes(15) = "Chase You (Pathfinding)"

REDIM npc_pushtypes(7) as string
npc_pushtypes(0) = "Off"
npc_pushtypes(1) = "Full"
npc_pushtypes(2) = "Vertical"
npc_pushtypes(3) = "Horizontal"
npc_pushtypes(4) = "Up only"
npc_pushtypes(5) = "Right Only"
npc_pushtypes(6) = "Down Only"
npc_pushtypes(7) = "Left Only"

REDIM npc_usetypes(2) as string
npc_usetypes(0) = "Use"
npc_usetypes(1) = "Touch"
npc_usetypes(2) = "Step On"

REDIM npc_facetypes(2) as string
npc_facetypes(0) = "Change Direction"
npc_facetypes(1) = "Face Player"
npc_facetypes(2) = "Do Not Face Player"


'==============================================================================

'This is a debug menu, which shows all bits (including hidden and unused) and their
'indices, in order of their index.  So it throws away the ordering in bitmenu(),
'and acts like editbitset with show_index=YES, show_all=YES.
'See editbitset for arg documentation.
FUNCTION edit_all_bits (array() as integer, wof as integer, bitmenu() as IntStrPair, numbits as integer, helpkey as string="editbitset", byref remem_pt as integer = -1, immediate_quit as bool = NO, title as string = "", prevmenu as string="Previous Menu") as EditBitsetResult

 REDIM fullbitmenu(numbits - 1) as IntStrPair
 FOR i as integer = 0 TO numbits - 1
  fullbitmenu(i).i = i
 NEXT

 FOR i as integer = 0 TO UBOUND(bitmenu)
  IF bitmenu(i).i >= 0 THEN
   fullbitmenu(bitmenu(i).i).s = bitmenu(i).s
  END IF
 NEXT

 RETURN editbitset(array(), wof, fullbitmenu(), helpkey, remem_pt, immediate_quit, title, prevmenu, YES, YES)
END FUNCTION

'Edit array of bits. The bits don't have to be consecutive, but they do have to be in ascending order.
'The bits corresponding to any blank entries in names(), or starting with '##' are hidden/skipped over,
'unless show_all is true.
'If a bit name starts with ! then the diplayed value of the bit is reversed.
'remem_pt is used to store the selected bit.
'If immediate_quit is true, then toggling a bit causes the menu to quit immediately (and return edbitPickedBit).
'Return value:
' edbitCancel    if user pressed ESC
' edbitBack      if used selected prevmenu (which is useful if prevmenu is something like "Done")
' edbitPickedBit if immediate_quit=YES and the user toggled a bit
FUNCTION editbitset (array() as integer, wof as integer, names() as string, helpkey as string="editbitset", byref remem_pt as integer = -1, immediate_quit as bool = NO, title as string = "", prevmenu as string="Previous Menu", show_index as bool = NO, show_all as bool = NO) as EditBitsetResult

 REDIM bitmenu(UBOUND(names)) as IntStrPair

 DIM nextbit as integer = 0
 FOR i as integer = 0 TO UBOUND(names)
  IF show_all ORELSE LEN(names(i)) THEN
   bitmenu(nextbit).s = names(i)
   bitmenu(nextbit).i = i
   nextbit += 1
  END IF
 NEXT
 IF nextbit THEN
  REDIM PRESERVE bitmenu(nextbit - 1)
 ELSE
  ERASE bitmenu
 END IF

 RETURN editbitset(array(), wof, bitmenu(), helpkey, remem_pt, immediate_quit, title, prevmenu, show_index, show_all)
END FUNCTION

'See above for documentation.
'This overload takes an array of bits to edit which allows bits be out of order,
'and to include unselectable section headings.
'The .i member of bits() is the bit number, which is -1 for unselectable menu items.
'This overload doesn't hide bits with blank names.  Bits with ## prefix are hidden
'unless show_hidden=YES.  If you want to see unused bits (like show_all=YES for the
'other editbitset overload), use edit_all_bits instead.
FUNCTION editbitset (array() as integer, wof as integer, bits() as IntStrPair, helpkey as string="editbitset", byref remem_pt as integer = -1, immediate_quit as bool = NO, title as string = "", prevmenu as string="Previous Menu", show_index as bool = NO, show_hidden as bool = NO) as EditBitsetResult

 DIM selectable(-1 TO UBOUND(bits)) as bool  'Oversized if there are any hidden bits
 selectable(-1) = YES

 'Build bitmenu(), a copy of bits(), with hidden items removed
 '(Note that bitmenu(-1) for prevmenu doesn't exist)
 DIM bitmenu() as IntStrPair
 FOR idx as integer = 0 TO UBOUND(bits)
  IF show_hidden ORELSE LEFT(bits(idx).s, 2) <> "##" THEN
   a_append bitmenu(), bits(idx).i, LTRIM(bits(idx).s, "#")
   selectable(UBOUND(bitmenu)) = (bits(idx).i >= 0)
  END IF
 NEXT

 DIM menupos as XYPair
 IF LEN(title) THEN menupos.y = 14

 DIM state as MenuState
 state.pt = remem_pt
 state.top = -1
 state.first = -1
 state.last = UBOUND(bitmenu)
 state.autosize = YES
 state.autosize_ignore_pixels = menupos.y
 calc_menustate_size state, MenuOptions(), 0, 0   'For autosize
 correct_menu_state state

 DIM ret as EditBitsetResult

 push_and_reset_gfxio_state
 DO
  setwait 55
  setkeys
  state.tog = state.tog XOR 1
  IF keyval(ccCancel) > 1 THEN ret = edbitCancelled : EXIT DO
  IF keyval(scF1) > 1 THEN show_help helpkey
  usemenu state, selectable()
  IF state.pt >= 0 ANDALSO selectable(state.pt) THEN
   DIM bitnum as integer = bitmenu(state.pt).i
   DIM inverted as integer = IIF(starts_with(bitmenu(state.pt).s, "!"), 1, 0)
   DIM newval as integer = -2
   IF keyval(scDelete) > 1 OR keyval(scBackspace) > 1 THEN newval = 0 XOR inverted
   IF keyval(ccLeft) > 1 OR keyval(scLeftCaret) > 1 THEN newval = 0 XOR inverted
   IF keyval(ccRight) > 1 OR keyval(scRightCaret) > 1 THEN newval = 1 XOR inverted
   IF enter_space_click(state) THEN newval = readbit(array(), wof, bitnum) XOR 1

   IF newval <> -2 THEN
    setbit array(), wof, bitnum, newval
    IF immediate_quit THEN ret = edbitPickedBit : EXIT DO
   END IF
  ELSE
   IF enter_space_click(state) THEN ret = edbitBack : EXIT DO
  END IF

  ' Draw
  clearpage dpage
  calc_menustate_size state, MenuOptions(), menupos.x, menupos.y  ' Recalcs .size, .rect, .spacing
  draw_fullscreen_scrollbar state, , dpage
  IF LEN(title) THEN edgeprint title, pCentered, menupos.y - 12, uilook(uiMenuItem), dpage
  FOR i as integer = state.top TO small(state.top + state.size, state.last)
   DIM drawat as XYPair = menupos
   drawat.x += 8 + IIF(state.pt = i, showRight, 0)
   drawat.y += (i - state.top) * state.spacing
   DIM biton as integer
   DIM col as integer
   DIM text as string = IIF(i = -1, prevmenu, bitmenu(i).s)
   IF i >= 0 ANDALSO selectable(i) THEN
    biton = readbit(array(), wof, bitmenu(i).i)
    IF LEFT(text, 1) = "!" THEN  'Inverted display
     biton XOR= 1
     text = MID(text, 2)
    END IF
    IF show_index THEN text = strprintf("%2d ", bitmenu(i).i) & text
    ellipse vpages(dpage), menupos.x + 4, drawat.y + 3, 3, uilook(uiDisabledItem), IIF(biton, uilook(uiSelectedItem), -1)
   ELSE
    biton = 1  'Don't show as disabled
    IF i > -1 THEN col = uilook(eduiHeading) 'Section heading: override text color
   END IF
   col = menu_item_color(state, i, biton = 0, selectable(i) = NO, col)
   textcolor col, 0
   printstr text, drawat.x, drawat.y, dpage
  NEXT i
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 pop_gfxio_state
 remem_pt = state.pt
 RETURN ret
END FUNCTION

'This is a wrapper around editbitset, but bools instead of packed bits.
FUNCTION editbools(bools() as bool, names() as string, helpkey as string = "editbitset", byref remem_pt as integer = -2, immediate_quit as bool = NO, title as string = "", prevmenu as string="Previous Menu") as EditBitsetResult
 DIM bitarray(0 TO UBOUND(bools) \ 16) as integer
 FOR i as integer = 0 TO UBOUND(bools)
  setbit bitarray(), 0, i, bools(i)
 NEXT

 DIM ret as EditBitsetResult
 ret = editbitset(bitarray(), 0, names(), helpkey, remem_pt, immediate_quit, title, prevmenu)

 FOR i as integer = 0 TO UBOUND(bools)
  bools(i) = xreadbit(bitarray(), i)
 NEXT
 RETURN ret
END FUNCTION

SUB gather_gen_bits(bittemp() as integer)
 bittemp(0) = gen(genBits)
 bittemp(1) = gen(genBits2)
 bittemp(2) = gen(genBits2+1)
 FOR i as integer = 0 TO 3
  bittemp(3 + i) = gen(genBits3 + i)
 NEXT
END SUB

SUB scatter_gen_bits(bittemp() as integer)
 gen(genBits) = bittemp(0)
 gen(genBits2) = bittemp(1)
 gen(genBits2+1) = bittemp(2)
 FOR i as integer = 0 TO 3
  gen(genBits3 + i) = bittemp(3 + i)
 NEXT
END SUB

SUB edit_global_bitsets OVERLOAD (bitname() as string, helpfile as string)
 DIM bittemp(6) as integer
 gather_gen_bits bittemp()
 editbitset bittemp(), 0, bitname(), helpfile
 scatter_gen_bits bittemp()
END SUB

SUB edit_global_bitsets OVERLOAD (bits() as IntStrPair, helpfile as string)
 DIM bittemp(6) as integer
 gather_gen_bits bittemp()
 editbitset bittemp(), 0, bits(), helpfile
 scatter_gen_bits bittemp()
END SUB

SUB edit_general_bitsets()
 REDIM bits() as IntStrPair
 a_append bits(), -1, ""
 a_append bits(), -1, " Hero Experience, Levels and Stats"
 a_append bits(), 2,  "!Restore HP on levelup"
 a_append bits(), 3,  "!Restore MP on levelup"
 a_append bits(), 4,  "!Inns revive dead heroes"
 a_append bits(), 31, "Don't reset max stats after OOB attack"
 a_append bits(), 30, "!Divide experience between heroes"
 a_append bits(), 19, "Dead heroes gain share of experience"
 a_append bits(), 43, "Cap minimum stats at zero"
 a_append bits(), 46, "!Negative-damage harmtiles can cure above max HP"
 a_append bits(), 29, "Ignore extra Hits stat"
 a_append bits(), 52, "Ignore MP~ stat"

 a_append bits(), -1, ""
 a_append bits(), -1, " Hero Party"
 a_append bits(), 1,  "Enable caterpillar party"
 a_append bits(), 41, "Keep caterpillar length the same when speed changes"
 a_append bits(), 42, "Heroes use Walk in Place animation while idle"
 a_append bits(), 5,  "Hero swapping always available"
 a_append bits(), 20, "Locked heroes can't be re-ordered"

 a_append bits(), -1, ""
 a_append bits(), -1, " Starting or Loading Games"
 a_append bits(), 11, "Skip title screen"
 a_append bits(), 12, "Skip load screen"
 a_append bits(), 40, "!Stop music before starting/loading game"
 a_append bits(), 44, "Hide empty save slots at the bottom of the save/load menus"

 a_append bits(), -1, ""
 a_append bits(), -1, " Misc"
 a_append bits(), 8,  "!Enable debugging keys"
 a_append bits(), 10, "Permit double-triggering of scripts"
 a_append bits(), 18, "!Save gameover/loadgame script IDs"
 a_append bits(), 15, "Default passability disabled by default"
 a_append bits(), 47, "!Map joystick controls to keyboard keys for scripts"
 a_append bits(), 53, "!Map joystick (left) stick to dpad"

 edit_global_bitsets bits(), "general_game_bitsets"
END SUB

SUB edit_backcompat_bitsets()
 DIM bitname(111) as string
 bitname(9) = "Simulate old levelup bonus-accretion bug"
 bitname(16) = "Simulate pushable NPC obstruction bug"
 bitname(24) = "!Disable better scancodes for scripts"
 bitname(25) = "Simulate old fail vs element resist bit"
 bitname(27) = "!Don't recreate map slices when changing maps"
 bitname(28) = "!Harm tiles ignore non-caterpillar heroes"
 bitname(32) = "!Limit maximum tags to 999"
 bitname(33) = "Simulate Bug #430 script wait skips"
 bitname(34) = "!showtextbox is delayed"
 bitname(36) = "Old attack positioning at bottom-left of target"
 bitname(37) = "Wrap map layers over edge of Crop maps"
 bitname(39) = "Draw Backdrop slice above Script Layer"
 IF gen(genBattleMode) = 1 THEN  'turn-based
  bitname(50) = "!Non-turn attack delays can also cause turn delays"
 END IF
 edit_global_bitsets bitname(), "share_general_game_backcompat_bitsets"
END SUB

'Battle System bitsets
SUB edit_battle_bitsets()
 REDIM bits() as IntStrPair
 a_append bits(), -1, ""
 a_append bits(), -1, " Battle Display Options"
 a_append bits(), 6,  "!Show ready-meter"
 a_append bits(), 7,  "!Show health meter"
 a_append bits(), 49, "Show MP meter"
 a_append bits(), 14, "Disable hero cursor"
 a_append bits(), 38, "Never show script timers during battles"

 a_append bits(), -1, ""
 a_append bits(), -1, " General Options"
 a_append bits(), 26, "0 damage when immune to attack elements"
 a_append bits(), 29, "Ignore extra Hits stat"
 a_append bits(), 52, "Ignore MP~ stat"

 a_append bits(), -1, ""
 IF gen(genBattleMode) = 0 THEN
  a_append bits(), -1, " Active-Time Battle Options"
  IF get_gen_bool("/mouse/mouse_battles") THEN
   a_append bits(), 17, "!Hold ESC key or right-click to run from battle"
  ELSE
   a_append bits(), 17, "!Hold ESC key to run from battle"
  END IF
  a_append bits(), 13, "Pause on all battle menus & targeting"
  a_append bits(), 0,  "Pause on Spells & Items menus"
  a_append bits(), 35, "Pause when targeting attacks"
  a_append bits(), 23, "Battle menus wait for attack animations"
  a_append bits(), 21, "Attack captions pause ready meters"
  a_append bits(), 22, "!Randomize initial ready meters"
 ELSE
  a_append bits(), -1, " Turn-based Battle Options"
  a_append bits(), 48, "!Press ESC to cancel/change a hero's attack"
  a_append bits(), 51, "Don't break Speed ties randomly"
 END IF

 edit_global_bitsets bits(), "general_battle_bitsets"
END SUB

SUB edit_mouse_options ()

 DIM menu as MenuDef
 
 DIM st as MenuState
 st.active = YES
 st.need_update = YES

 DIM t as integer
 DIM do_toggle as bool = NO
 REDIM enabled(0) as bool

 push_and_reset_gfxio_state
 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(scF1) > 1 THEN show_help "edit_mouse_options"
  IF keyval(ccCancel) > 1 THEN EXIT DO

  IF st.need_update THEN
   st.need_update = NO
   InitLikeStandardMenu menu
   append_menu_item menu, "Previous menu..."
   menu.last->t = 0
   append_menu_item menu, "Click on map to move the hero: " & yesorno(get_gen_bool("/mouse/move_hero"))
   menu.last->t = 1
   IF get_gen_bool("/mouse/move_hero") THEN
    append_menu_item menu, " Display destination: " & yesorno(get_gen_bool("/mouse/move_hero/display_dest"))
    menu.last->t = 2
    append_menu_item menu, " Cancel move on battles: " & yesorno(get_gen_bool("/mouse/move_hero/cancel_on_battle"))
    menu.last->t = 3
    append_menu_item menu, " Cancel move on textboxes: " & yesorno(get_gen_bool("/mouse/move_hero/cancel_on_textbox"))
    menu.last->t = 4
    append_menu_item menu, " Cancel move on menus: " & yesorno(get_gen_bool("/mouse/move_hero/cancel_on_menu"))
    menu.last->t = 5
    append_menu_item menu, " Max tiles to walk: " & zero_default(get_gen_int("/mouse/move_hero/max_path_length"), "0 (no limit)")
    menu.last->t = 6
   END IF
   append_menu_item menu, "Show cursor even in full-screen: " & yesorno(get_gen_bool("/mouse/show_cursor"))
   menu.last->t = 9
   append_menu_item menu, "Open main menu on right-click: " & yesorno(get_gen_bool("/mouse/menu_right_click"))
   menu.last->t = 10
   append_menu_item menu, "Click to advance text boxes: " & yesorno(get_gen_bool("/mouse/click_textboxes"))
   menu.last->t = 20
   append_menu_item menu, """any key"", etc, include mouse: " & yesorno(get_gen_bool("/mouse/click_keys"))
   menu.last->t = 13
   append_menu_item menu, "Mouse support on menus: " & yesorno(get_gen_bool("/mouse/mouse_menus"))
   menu.last->t = 11
   append_menu_item menu, "Mouse support in battles: " & yesorno(get_gen_bool("/mouse/mouse_battles"))
   menu.last->t = 12

   init_menu_state st, menu
  END IF

  usemenu st, menu

  t = menu.items[st.pt]->t

  do_toggle = NO
  
  IF enter_space_click(st) THEN
   SELECT CASE t
    CASE 0: EXIT DO
    CASE 6:
    CASE ELSE
     do_toggle = YES
   END SELECT
  END IF

  SELECT CASE t
   CASE 0: 'exit
   CASE 6:
    IF gen_intgrabber("/mouse/move_hero/max_path_length") THEN st.need_update = YES
   CASE ELSE
    IF keyval(ccLeft) > 1 ORELSE keyval(ccRight) > 1 THEN
     do_toggle = YES
    END IF
  END SELECT
  
  IF do_toggle THEN
   SELECT CASE t
    CASE 1:
     toggle_gen_bool("/mouse/move_hero")
     st.need_update = YES
    CASE 2:
     toggle_gen_bool("/mouse/move_hero/display_dest")
     st.need_update = YES
    CASE 3:
     toggle_gen_bool("/mouse/move_hero/cancel_on_battle")
     st.need_update = YES
    CASE 4:
     toggle_gen_bool("/mouse/move_hero/cancel_on_textbox")
     st.need_update = YES
    CASE 5:
     toggle_gen_bool("/mouse/move_hero/cancel_on_menu")
     st.need_update = YES
    CASE 9:
     toggle_gen_bool("/mouse/show_cursor")
     st.need_update = YES
    CASE 10:
     toggle_gen_bool("/mouse/menu_right_click")
     st.need_update = YES
    CASE 11:
     toggle_gen_bool("/mouse/mouse_menus")
     st.need_update = YES
    CASE 12:
     toggle_gen_bool("/mouse/mouse_battles")
     st.need_update = YES
    CASE 13:
     toggle_gen_bool("/mouse/click_keys")
     st.need_update = YES
    CASE 20:
     toggle_gen_bool("/mouse/click_textboxes")
     st.need_update = YES
   END SELECT
  END IF
  
  clearpage dpage
  
  draw_menu menu, st, dpage
  SWAP vpage, dpage
  setvispage vpage
  
  dowait
 LOOP
 pop_gfxio_state

 #IFNDEF IS_GAME
  ' Don't write changes, in case we're live-previewing or playing an .rpgdir
  write_general_reld()
 #ENDIF
END SUB
