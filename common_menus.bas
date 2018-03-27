'OHRRPGCE - Definitions of menus which are common to Custom + Game
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

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

'Edit array of bits. The bits don't have to be consecutive, but they do have to be in ascending order.
'The bits corresponding to any blank entries in names() are skipped over.
'if remem_pt is not -2 (initialise to -1) it is used to store the selected bit (index in names())
'If immediate_quit is true, then toggling a bit causes the menu to quit immediately and return YES (otherwise NO)
FUNCTION editbitset (array() as integer, byval wof as integer, byval last as integer, names() as string, helpkey as string="editbitset", byref remem_pt as integer = -2, byval immediate_quit as integer = NO) as integer

 '---DIM AND INIT---
 DIM state as MenuState

 DIM menu(-1 to last) as string
 DIM bits(-1 to last) as integer
 
 init_menu_state state, menu()
 state.top = -1
 state.pt = -1

 menu(-1) = "Previous Menu"

 DIM nextbit as integer = 0
 FOR i as integer = 0 to last
  IF names(i) <> "" THEN
   menu(nextbit) = names(i)
   bits(nextbit) = i
   IF remem_pt = i THEN state.pt = nextbit
   nextbit += 1
  END IF
 NEXT
 state.last = nextbit - 1
 state.autosize = YES

 DIM ret as integer = NO
 DIM col as integer

 '---MAIN LOOP---
 force_use_mouse += 1
 DIM prev_mouse_vis as CursorVisibility = getcursorvisibility()
 showmousecursor
 setkeys
 DO
  setwait 55
  setkeys
  state.tog = state.tog XOR 1
  IF keyval(scEsc) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help helpkey
  usemenu state
  IF state.pt >= 0 THEN
   IF keyval(scLeft) > 1 OR keyval(scComma) > 1 THEN
    setbit array(), wof, bits(state.pt), 0
    IF immediate_quit THEN ret = YES: EXIT DO
   END IF
   IF keyval(scRight) > 1 OR keyval(scPeriod) > 1 THEN
    setbit array(), wof, bits(state.pt), 1
    IF immediate_quit THEN ret = YES: EXIT DO
   END IF
   IF enter_space_click(state) THEN
    setbit array(), wof, bits(state.pt), readbit(array(), wof, bits(state.pt)) XOR 1
    IF immediate_quit THEN ret = YES: EXIT DO
   END IF
  ELSE
   IF enter_space_click(state) THEN EXIT DO
  END IF

  ' Draw
  clearpage dpage
  calc_menustate_size state, MenuOptions(), 0, 0  ' Recalcs .size, .rect, .spacing
  draw_fullscreen_scrollbar state, , dpage
  FOR i as integer = state.top TO small(state.top + state.size, state.last)
   IF i >= 0 THEN
    DIM biton as integer = readbit(array(), wof, bits(i))
    IF state.pt = i THEN
     col = uilook(IIF(biton, uiSelectedItem, uiSelectedDisabled) + state.tog)
    ELSEIF state.hover = i THEN
     col = uilook(uiMouseHoverItem)
    ELSE
     col = uilook(IIF(biton, uiMenuItem, uiDisabledItem))
    END IF
    ellipse vpages(dpage), 0 + 4, (i - state.top) * state.spacing + 3, 3, uilook(uiDisabledItem), IIF(biton, uilook(uiSelectedItem), -1)
   ELSE
    col = uilook(uiMenuItem)
    IF state.pt = i THEN
     col = uilook(uiSelectedItem + state.tog)
    ELSEIF state.hover = i THEN
     col = uilook(uiMouseHoverItem)
    END IF
   END IF
   textcolor col, 0
   DIM drawstr as string = " " & menu(i)
   printstr drawstr, IIF(state.pt = i, showRight, 0), (i - state.top) * state.spacing, dpage
  NEXT i
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 force_use_mouse -= 1
 setcursorvisibility(prev_mouse_vis)
 IF remem_pt <> -2 THEN
  IF state.pt = -1 THEN
   remem_pt = -1
  ELSE
   remem_pt = bits(state.pt)
  END IF
 END IF
 RETURN ret
END FUNCTION


SUB edit_global_bitsets(bitname() as string, helpfile as string)
 DIM bittemp(2) as integer
 bittemp(0) = gen(genBits)
 bittemp(1) = gen(genBits2)
 bittemp(2) = gen(genBits2+1)
 editbitset bittemp(), 0, UBOUND(bitname), bitname(), helpfile
 gen(genBits) = bittemp(0)
 gen(genBits2) = bittemp(1)
 gen(genBits2+1) = bittemp(2)
END SUB

SUB edit_general_bitsets()
 DIM bitname(47) as string
 bitname(1) = "Enable Caterpillar Party"
 bitname(2) = "Don't Restore HP on Levelup"
 bitname(3) = "Don't Restore MP on Levelup"
 bitname(4) = "Inns Don't Revive Dead Heroes"
 bitname(5) = "Hero Swapping Always Available"
 bitname(6) = "Hide Ready-meter in Battle"
 bitname(7) = "Hide Health-meter in Battle"
 bitname(8) = "Disable Debugging Keys"
 bitname(10) = "Permit double-triggering of scripts"
 bitname(11) = "Skip title screen"
 bitname(12) = "Skip load screen"
 bitname(14) = "Disable Hero's Battle Cursor"
 bitname(15) = "Default passability disabled by default"
 bitname(17) = "Disable ESC key running from battle"
 bitname(18) = "Don't save gameover/loadgame script IDs"
 bitname(19) = "Dead heroes gain share of experience"
 bitname(20) = "Locked heroes can't be re-ordered"
 bitname(22) = "Don't randomize battle ready meters"
 bitname(26) = "0 damage when immune to attack elements"
 bitname(29) = "Attacks will ignore extra hits stat"
 bitname(30) = "Don't divide experience between heroes"
 bitname(31) = "Don't reset max stats after OOB attack"
 bitname(38) = "Never show script timers during battles"
 bitname(40) = "Don't stop music when starting/loading game"
 bitname(41) = "Keep caterpillar length the same when speed changes"
 bitname(42) = "Heroes use Walk in Place animation while idle"
 bitname(43) = "Cap minimum stats at zero"
 bitname(44) = "Hide empty save slots at the bottom of the save/load menus"
 edit_global_bitsets bitname(), "general_game_bitsets"
END SUB

SUB edit_backcompat_bitsets()
 DIM bitname(47) as string
 bitname(9) = "Simulate Old Levelup bonus-accretion Bug"
 bitname(16) = "Simulate Pushable NPC obstruction bug"
 bitname(24) = "Enable better scancodes for scripts"
 bitname(25) = "Simulate old fail vs element resist bit"
 bitname(27) = "Recreate map slices when changing maps"
 bitname(28) = "Harm tiles harm non-caterpillar heroes"
 bitname(32) = "Don't limit maximum tags to 999"
 bitname(33) = "Simulate Bug #430 script wait skips"
 bitname(34) = "showtextbox happens immediately"
 bitname(36) = "Old attack positioning at bottom-left of target"
 bitname(37) = "Wrap map layers over edge of Crop maps"
 bitname(39) = "Draw Backdrop slice above Script layer"
 edit_global_bitsets bitname(), "share_general_game_backcompat_bitsets"
END SUB

SUB edit_active_time_battle_bitsets()
 DIM bitname(35) as string
 bitname(0) = "Pause on Spells & Items menus"
 bitname(13) = "Pause on all battle menus & targeting"
 bitname(21) = "Attack captions pause battle meters"
 bitname(23) = "Battle menus wait for attack animations"
 bitname(35) = "Pause when targeting attacks"
 edit_global_bitsets bitname(), "general_game_active_battle_bitsets"
END SUB

SUB edit_mouse_options ()

 DIM menu as MenuDef
 
 DIM st as MenuState
 st.active = YES
 st.need_update = YES

 DIM t as integer
 DIM do_toggle as bool = NO
 REDIM enabled(0) as bool

 force_use_mouse += 1
 DIM prev_mouse_vis as CursorVisibility = getcursorvisibility()
 showmousecursor
 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(scF1) > 1 THEN show_help "edit_mouse_options"
  IF keyval(scEsc) > 1 THEN EXIT DO

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
   append_menu_item menu, "Experimental/Unfinished Mouse Features"
   menu.last->disabled = YES
   menu.last->unselectable = YES
   append_menu_item menu, "Mouse support on menus: " & yesorno(get_gen_bool("/mouse/mouse_menus"))
   menu.last->t = 11
   append_menu_item menu, "Mouse support in battles: " & yesorno(get_gen_bool("/mouse/mouse_battles"))
   menu.last->t = 12
   init_menu_state st, menu
   REDIM enabled(menu.numitems - 1) as bool
   FOR i as integer = 0 TO UBOUND(enabled)
    WITH *menu.items[i]
     enabled(i) = NOT .unselectable
    END WITH
   NEXT i
  END IF

  usemenu st, enabled()

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
    IF keyval(scLeft) > 1 ORELSE keyval(scRight) > 1 THEN
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
 force_use_mouse -= 1
 setcursorvisibility(prev_mouse_vis)

 #IFNDEF IS_GAME
  ' Don't write changes, in case we're live-previewing or playing an .rpgdir
  write_general_reld()
 #ENDIF
END SUB
