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
 state.spacing = 8

 DIM ret as integer = NO
 DIM col as integer

 '---MAIN LOOP---
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
  clearpage dpage
  draw_fullscreen_scrollbar state, , dpage
  FOR i as integer = state.top TO small(state.top + state.size, state.last)
   IF i >= 0 THEN
    col = IIF(readbit(array(), wof, bits(i)), uilook(uiMenuItem), uilook(uiDisabledItem))
    IF state.pt = i THEN col = IIF(readbit(array(), wof, bits(i)), uilook(uiSelectedItem + state.tog), uilook(uiSelectedDisabled + state.tog))
   ELSE
    col = uilook(uiMenuItem)
    IF state.pt = i THEN col = uilook(uiSelectedItem + state.tog)
   END IF
   textcolor col, 0
   DIM drawstr as string = " " & menu(i)
   IF state.pt = i THEN drawstr = RIGHT(drawstr, 40)
   printstr drawstr, 0, (i - state.top) * 8, dpage
  NEXT i
  WITH state
   .has_been_drawn = YES
   .spacing = 8
   .rect.x = 0
   .rect.y = 0
   .rect.wide = get_resolution_w()
   .rect.high = small(get_resolution_h(), (.size + 1) * .spacing)
  END WITH
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
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
 bitname(23) = "Pause for attack animations"
 bitname(35) = "Pause when targeting attacks"
 edit_global_bitsets bitname(), "general_game_active_battle_bitsets"
END SUB
