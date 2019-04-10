'OHRRPGCE CUSTOM - Misc editors
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
#include "config.bi"
#include "string.bi"
#include "allmodex.bi"
#include "common.bi"
#include "bcommon.bi"
#include "customsubs.bi"
#include "cglobals.bi"
#include "custom.bi"
#include "thingbrowser.bi"

#include "const.bi"
#include "scrconst.bi"
#include "uiconst.bi"
#include "loading.bi"
#include "slices.bi"

'local subs and functions
DECLARE SUB generalscriptsmenu ()
DECLARE SUB script_error_mode_menu ()
DECLARE SUB masterpalettemenu ()
DECLARE SUB statcapsmenu ()
DECLARE SUB battleoptionsmenu ()
DECLARE SUB equipmergemenu ()
DECLARE SUB update_masterpalette_menu(menu() as string, shaded() as bool, palnum as integer)
DECLARE SUB inputpasw ()
DECLARE SUB nearestui (byval mimicpal as integer, newpal() as RGBcolor, newui() as integer, newbox() as BoxStyle)
DECLARE SUB remappalette (oldmaster() as RGBcolor, oldui() as integer, oldbox() as BoxStyle, newmaster() as RGBcolor, newui() as integer, newbox() as BoxStyle)


SUB vehicle_editor

DIM menu(15) as string
DIM veh(39) as integer
DIM min(39) as integer
DIM max(39) as integer
DIM offset(39) as integer
DIM vehbit() as IntStrPair
DIM tiletype(8) as string
DIM vehname as string = ""
DIM vehicle_id as integer = 0

DIM state as MenuState
state.size = 24
state.last = UBOUND(menu)
state.need_update = YES

a_append vehbit(), -1, ""
a_append vehbit(), -1, " Appearance"
a_append vehbit(), 4,  "Do not hide leader"
a_append vehbit(), 5,  "Do not hide party"
a_append vehbit(), 8,  "Disable flying shadow"
a_append vehbit(), -1, ""
a_append vehbit(), -1, " Movement"
a_append vehbit(), 0,  "Pass through walls"
a_append vehbit(), 1,  "Pass through NPCs"
a_append vehbit(), 9,  "Ignore harmtiles"
a_append vehbit(), 6,  "Dismount one space ahead"
a_append vehbit(), 7,  "Pass walls while dismounting"
a_append vehbit(), -1, ""
a_append vehbit(), -1, " Activation"
a_append vehbit(), 2,  "Enable NPC activation"
a_append vehbit(), 3,  "Enable door use"

tiletype(0) = "default"
tiletype(1) = "A"
tiletype(2) = "B"
tiletype(3) = "A and B"
tiletype(4) = "A or B"
tiletype(5) = "not A"
tiletype(6) = "not B"
tiletype(7) = "neither A nor B"
tiletype(8) = "everywhere"

min(3) = 0: max(3) = 5: offset(3) = 8             'speed
FOR i as integer = 0 TO 3
 min(5 + i) = 0: max(5 + i) = 8: offset(5 + i) = 17 + i
NEXT i
min(9) = -1: max(9) = 255: offset(9) = 11 'battles
min(10) = -2: max(10) = gen(genMaxRegularScript): offset(10) = 12 'use button
min(11) = -2: max(11) = gen(genMaxRegularScript): offset(11) = 13 'menu button
min(12) = -max_tag(): max(12) = max_tag(): offset(12) = 14 'tag
min(13) = gen(genMaxRegularScript) * -1: max(13) = gen(genMaxTextbox): offset(13) = 15'mount
min(14) = gen(genMaxRegularScript) * -1: max(14) = gen(genMaxTextbox): offset(14) = 16'dismount
min(15) = 0: max(15) = 99: offset(15) = 21'dismount

LoadVehicle game + ".veh", veh(), vehname, vehicle_id

setkeys YES
DO
 setwait 55
 setkeys YES
 IF state.need_update THEN
  state.need_update = NO
  menu(0) = "Previous Menu"
  menu(1) = "Vehicle " & vehicle_id
  menu(2) = "Name: " + vehname

  IF veh(offset(3)) = 3 THEN
   menu(3) = "Speed: 10"
  ELSE
   menu(3) = "Speed: " & veh(8)
  END IF

  menu(4) = "Vehicle Bitsets..." '9,10

  menu(5) = "Override walls: "
  menu(6) = "Blocked by: "
  menu(7) = "Mount from: "
  menu(8) = "Dismount to: "
  FOR i as integer = 0 TO 3
   menu(5 + i) = menu(5 + i) + tiletype(bound(veh(offset(5 + i)), 0, 8))
  NEXT i

  DIM tmp as string
  
  SELECT CASE veh(offset(9))
   CASE -1
    tmp = "disabled"
   CASE 0
    tmp = "enabled"
   CASE ELSE
    tmp = "formation set " & veh(offset(9))
  END SELECT
  menu(9) = "Random Battles: " + tmp '11

  FOR i as integer = 0 TO 1
   SELECT CASE veh(offset(10 + i))
    CASE -2
     tmp = "disabled"
    CASE -1
     tmp = "menu"
    CASE 0
     tmp = "dismount"
    CASE ELSE
     tmp = "script " + scriptname(ABS(veh(offset(10 + i))))
   END SELECT
   IF i = 0 THEN menu(10 + i) = "Use button: " + tmp'12
   IF i = 1 THEN menu(10 + i) = "Menu button: " + tmp'13
  NEXT i
  
  menu(12) = tag_set_caption(veh(offset(12)), "If riding set tag")
  
  SELECT CASE veh(offset(13))
   CASE 0
    tmp = "[script/textbox]"
   CASE IS < 0
    tmp = "run script " + scriptname(ABS(veh(offset(13))))
   CASE IS > 0
    tmp = "text box " & veh(offset(13))
  END SELECT
  menu(13) = "On Mount: " + tmp
  SELECT CASE veh(offset(14))
   CASE 0
    tmp = "[script/textbox]"
   CASE IS < 0
    tmp = "run script " + scriptname(ABS(veh(offset(14))))
   CASE IS > 0
    tmp = "text box " & veh(offset(14))
  END SELECT
  menu(14) = "On Dismount: " + tmp
  menu(15) = "Elevation: " & veh(offset(15)) & " pixels"
 END IF
 IF keyval(ccCancel) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help "vehicle_editor"
 usemenu state
 SELECT CASE state.pt
  CASE 0
   IF enter_space_click(state) THEN
    EXIT DO
   END IF
  CASE 1
   DIM savept as integer = vehicle_id
   IF intgrabber_with_addset(vehicle_id, 0, gen(genMaxVehicle), 32767, "vehicle") THEN
    SaveVehicle game + ".veh", veh(), vehname, savept
    IF vehicle_id > gen(genMaxVehicle) THEN  '--adding set
     gen(genMaxVehicle) = vehicle_id
     flusharray veh()
     vehname = ""
    ELSE
     LoadVehicle game + ".veh", veh(), vehname, vehicle_id
    END IF
    state.need_update = YES
   END IF
  CASE 2
   DIM oldname as string = vehname
   strgrabber vehname, 15
   IF oldname <> vehname THEN state.need_update = YES
  CASE 3, 5 TO 9, 15
   IF intgrabber(veh(offset(state.pt)), min(state.pt), max(state.pt)) THEN
    state.need_update = YES
   END IF
  CASE 12 '--tags
   IF tag_set_grabber(veh(offset(state.pt)), state) THEN
    state.need_update = YES
   END IF
  CASE 4
   IF enter_space_click(state) THEN
    editbitset veh(), 9, vehbit(), "vehicle_bitsets"
   END IF
  CASE 10, 11
   IF enter_space_click(state) THEN
    veh(offset(state.pt)) = large(0, veh(offset(state.pt)))
    scriptbrowse veh(offset(state.pt)), plottrigger, "vehicle plotscript"
    state.need_update = YES
   ELSEIF scrintgrabber(veh(offset(state.pt)), min(state.pt), max(state.pt), ccLeft, ccRight, 1, plottrigger) THEN
    state.need_update = YES
   END IF
  CASE 13, 14 '--mount and dismount actions
   IF veh(offset(state.pt)) > 0 ANDALSO textboxgrabber(veh(offset(state.pt)), state) THEN
    state.need_update = YES
   ELSEIF enter_space_click(state) THEN
    DIM temptrig as integer = large(0, -veh(offset(state.pt)))
    scriptbrowse temptrig, plottrigger, "vehicle plotscript"
    veh(offset(state.pt)) = -temptrig
    state.need_update = YES
   ELSEIF scrintgrabber(veh(offset(state.pt)), min(state.pt), max(state.pt), ccLeft, ccRight, -1, plottrigger) THEN
    state.need_update = YES
   END IF
 END SELECT
 clearpage dpage
 standardmenu menu(), state, 0, 0, dpage
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
SaveVehicle game + ".veh", veh(), vehname, vehicle_id

END SUB

SUB generalscriptsmenu ()
 CONST menusize = 13
 DIM menu(menusize) as string
 DIM menu_display(menusize) as string
 DIM selectable(menusize) as bool
 flusharray selectable(), , YES
 DIM scripttype(menusize) as string
 selectable(1) = NO
 selectable(2) = NO
 'Global script triggers:
 scripttype(3) = "New game"
 scripttype(4) = "Game over"
 scripttype(5) = "Load game"
 scripttype(6) = "Menu action"
 selectable(7) = NO
 selectable(8) = NO
 'Map default scripts:
 scripttype(9) = "Map autorun"
 scripttype(10) = "After battle"
 scripttype(11) = "Instead of battle"
 scripttype(12) = "Each-step"
 scripttype(13) = "On-keypress"

 DIM scriptgenoff(menusize) as integer = { _
     0, 0, 0, genNewGameScript, genGameoverScript, genLoadGameScript, genEscMenuScript, _
     0, 0, genDefMapAutorunScript, genDefAfterBattleScript, genDefInsteadOfBattleScript, _
     genDefEachStepScript, genDefOnKeypressScript _
 }

 DIM selectst as SelectTypeState
 DIM state as MenuState
 state.size = 24
 state.last = UBOUND(menu)

 setkeys YES
 DO
  setwait 55
  setkeys YES
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "global_scripts"
  usemenu state, selectable()
  IF state.pt = 0 THEN
   IF enter_space_click(state) THEN EXIT DO
  ELSEIF scriptgenoff(state.pt) = 0 THEN
   'This menu item is a header
  ELSE
   IF enter_space_click(state) THEN
    scriptbrowse(gen(scriptgenoff(state.pt)), plottrigger, scripttype(state.pt) + " script")
   ELSE
    scrintgrabber(gen(scriptgenoff(state.pt)), 0, 0, ccLeft, ccRight, 1, plottrigger)
   END IF
  END IF

  menu(0) = "Previous Menu"
  menu(2) = fgtag(uilook(eduiHeading)) + " Global script triggers"
  menu(8) = fgtag(uilook(eduiHeading)) + " Map default scripts"
  FOR i as integer = 1 TO menusize
   IF scriptgenoff(i) THEN
    menu(i) = scripttype(i) + ": " + scriptname(gen(scriptgenoff(i)))
   END IF
  NEXT

  IF select_by_typing(selectst, NO) THEN
   select_on_word_boundary menu(), selectst, state
  END IF

  clearpage dpage
  highlight_menu_typing_selection menu(), menu_display(), selectst, state
  standardmenu menu_display(), state, 0, 0, dpage
  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
END SUB

SUB script_error_mode_menu ()
 DIM menu(1) as string
 DIM menu_display(UBOUND(menu)) as string

 DIM selectst as SelectTypeState
 DIM state as MenuState
 state.size = 24
 state.last = UBOUND(menu)
 state.need_update = YES

 DIM root as Slice Ptr
 root = NewSliceOfType(slContainer)
 WITH *root
  .Fill = YES
 END WITH

 DIM box as Slice Ptr
 box = NewSliceOfType(slRectangle, root)
 WITH *box
  .Fill = YES
  .FillMode = sliceFillHoriz
  .height = 48
  .AnchorHoriz = alignCenter
  .AnchorVert = alignBottom
  .AlignHoriz = alignCenter
  .AlignVert = alignBottom
  .paddingLeft = 8
  .paddingRight = 8
  .paddingTop = 8
  .paddingBottom = 8
 END WITH
 ChangeRectangleSlice box, 1

 DIM infosl as Slice Ptr
 infosl = NewSliceofType(slText, box)
 infosl->Fill = YES
 ChangeTextSlice infosl, , , , YES

 setkeys YES
 DO
  setwait 55
  setkeys YES

  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "general_script_error"
   usemenu state

   IF enter_space_click(state) THEN
    SELECT CASE state.pt
     CASE 0
      EXIT DO
     CASE 1
      gen(genDebugMode) XOR= 1
      state.need_update = YES
    END SELECT
   END IF

   SELECT CASE state.pt
    CASE 1
     IF intgrabber(gen(genDebugMode), 0, 1) THEN
      state.need_update = YES
     END IF
   END SELECT

   IF state.need_update THEN
    state.need_update = NO
    menu(0) = "Previous Menu"
    menu(1) = "Script Error Display Mode: " & yesorno(gen(genDebugMode), "debug", "release")
    SELECT CASE gen(genDebugMode)
     CASE 0
      ChangeTextSlice infosl, "Release mode hides script errors and warnings. It is HIGHLY recommended that you use release mode when releasing your game. Please press F1 for more information."
     CASE 1
      ChangeTextSlice infosl, "Debug mode shows script errors and warnings. It is recommended while playtesting. It will automatically be turned OFF when you export your game from the ""Distribute Game"" menu. Please press F1 for more information."
    END SELECT
   END IF    
   box->Height = infosl->height + box->PaddingTop + box->PaddingBottom

   IF select_by_typing(selectst, NO) THEN
    select_on_word_boundary menu(), selectst, state
   END IF

   clearpage dpage
   DrawSlice root, dpage
   highlight_menu_typing_selection menu(), menu_display(), selectst, state
   standardmenu menu_display(), state, 0, 0, dpage 

   SWAP vpage, dpage
   setvispage vpage
   dowait
 LOOP
 DeleteSlice @root
END SUB


SUB export_master_palette ()
 DIM filename as string

 filename = inputfilename("Name of file to export to?", ".bmp", "", "input_file_export_masterpal")
 IF filename = "" THEN EXIT SUB
 filename &= ".bmp"

 DIM outsurf as Surface ptr
 gfx_surfaceCreate(16, 16, SF_32bit, SU_Staging, @outsurf)

 FOR i as integer = 0 TO 255
  outsurf->pColorData[i] = master(i)
 NEXT
 surface_export_bmp24(filename, outsurf)
 gfx_surfaceDestroy(@outsurf)
END SUB

SUB masterpalettemenu
DIM menu(9) as string
DIM menu_display(9) as string
DIM shaded(9) as bool
DIM oldpal as integer
DIM palnum as integer = activepalette
loadpalette master(), palnum
setpal master()
LoadUIColors uilook(), boxlook(), palnum

DIM selectst as SelectTypeState
DIM state as MenuState
state.size = 10
state.last = UBOUND(menu)
state.pt = 1

update_masterpalette_menu menu(), shaded(), palnum

setkeys YES
DO
 setwait 55
 setkeys YES

 IF keyval(ccCancel) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help "master_palette_menu"
 usemenu state

 oldpal = palnum
 IF keyval(ccRight) > 1 AND palnum = gen(genMaxMasterPal) THEN
  palnum += 1
  IF needaddset(palnum, gen(genMaxMasterPal), "Master Palette") THEN
   IF importmasterpal("", palnum) THEN
    setpal master()
    LoadUIColors uilook(), boxlook(), palnum
    state.need_update = YES     
   ELSE
    palnum -= 1
    gen(genMaxMasterPal) = palnum
   END IF
  END IF
  setkeys
 END IF
 IF state.pt = 1 THEN
  intgrabber(palnum, 0, gen(genMaxMasterPal))
 ELSE
  IF keyval(ccLeft) > 1 THEN palnum += gen(genMaxMasterPal)
  IF keyval(ccRight) > 1 THEN palnum += 1
  palnum = palnum MOD (gen(genMaxMasterPal) + 1)
 END IF
 IF palnum <> oldpal THEN
  loadpalette master(), palnum
  setpal master()
  LoadUIColors uilook(), boxlook(), palnum
  state.need_update = YES
 END IF

 IF enter_space_click(state) THEN
  SELECT CASE state.pt
  CASE 0
    EXIT DO
  CASE 2
    IF importmasterpal("", palnum) THEN
     setpal master()
     LoadUIColors uilook(), boxlook(), palnum
     state.need_update = YES
    END IF
  CASE 3
    export_master_palette
  CASE 4
    ui_color_editor palnum
  CASE 5
    nearestui activepalette, master(), uilook(), boxlook()
    SaveUIColors uilook(), boxlook(), palnum
  CASE 6
    LoadUIColors uilook(), boxlook(), activepalette
    SaveUIColors uilook(), boxlook(), palnum
  CASE 7
    gen(genMasterPal) = palnum
    ' Keep obsolete .MAS lump up to date
    unconvertpalette
    'Instant live-previewing
    xbsave game + ".gen", gen(), 1000
    state.need_update = YES
  CASE 8
    activepalette = palnum
    state.need_update = YES
  CASE 9
    ui_boxstyle_editor palnum
  END SELECT
 END IF

 IF state.need_update THEN
  state.need_update = NO
  update_masterpalette_menu menu(), shaded(), palnum
 END IF

 IF select_by_typing(selectst) THEN
  select_on_word_boundary menu(), selectst, state
 END IF

 'draw the menu
 clearpage dpage
 highlight_menu_typing_selection menu(), menu_display(), selectst, state
 standardmenu menu_display(), state, shaded(), 0, 0, dpage 

 DIM as integer colgridx = 33, colgridy = 82

 FOR i as integer = 0 TO 255
  rectangle colgridx + (i MOD 16) * 16, colgridy + (i \ 16) * 7, 16, 7, i, dpage
 NEXT
 IF state.pt = 4 ORELSE state.pt = 5 ORELSE state.pt = 6 THEN
  FOR i as integer = 0 TO uiColorLast
   drawbox colgridx + (uilook(i) MOD 16) * 16, colgridy + (uilook(i) \ 16) * 7, 16, 7, uilook(uiHighlight + state.tog), 1, dpage
  NEXT i
 END IF
 IF state.pt = 5 ORELSE state.pt = 6 ORELSE state.pt = 9 THEN
  FOR i as integer = 0 TO uiBoxLast
   drawbox colgridx + (boxlook(i).bgcol MOD 16) * 16, colgridy + (boxlook(i).bgcol \ 16) * 7, 16, 7, uilook(uiHighlight + state.tog), 1, dpage
   drawbox colgridx + (boxlook(i).edgecol MOD 16) * 16, colgridy + (boxlook(i).edgecol \ 16) * 7, 16, 7, uilook(uiHighlight + state.tog), 1, dpage
  NEXT i
 END IF

 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP

IF activepalette <> palnum THEN
 loadpalette master(), activepalette
 setpal master()
 LoadUIColors uilook(), boxlook(), activepalette
END IF

END SUB

SUB update_masterpalette_menu(menu() as string, shaded() as bool, palnum as integer)
 menu(0) = "Previous Menu"
 menu(1) = "<- Master Palette " & palnum & " ->"
 menu(2) = "Replace this Master Palette"
 menu(3) = "Export this palette"
 menu(4) = "Edit User Interface Colors..."
 menu(5) = "Nearest-match active palette's UI colors"
 menu(6) = "Copy active palette's UI data"
 IF palnum = gen(genMasterPal) THEN
  menu(7) = "Current default in-game Master Palette"
 ELSE
  menu(7) = "Set as in-game Master Palette"
 END IF
 IF palnum = activepalette THEN
  menu(8) = "Current active editing palette"
 ELSE
  menu(8) = "Set as active editing palette"
 END IF
 menu(9) = "Edit Box Styles..."

 FOR i as integer = 0 TO UBOUND(shaded)
  shaded(i) = NO
 NEXT
 IF palnum = activepalette THEN
  shaded(5) = YES
  shaded(6) = YES
  shaded(8) = YES
 END IF
 IF palnum = gen(genMasterPal) THEN
  shaded(7) = YES
 END IF
END SUB

'Returns true on success
FUNCTION importmasterpal (filename as string = "", palnum as integer) as bool
 STATIC default as string
 IF filename = "" THEN
  filename = browse(browseMasterPal, default, "", "browse_import_master_palette")
  IF filename = "" THEN RETURN NO
 END IF
 IF LCASE(justextension(filename)) = "mas" THEN
  xbload filename, buffer(), "MAS load error"
  convertpalette buffer(), master()
 ELSE
  DIM info as ImageFileInfo = image_read_info(filename)
  IF info.size = XY(16, 16) THEN
   palette_from_16x16_image filename, master()
  ELSE
   image_load_palette filename, master()
  END IF
 END IF
 'get a default set of ui colours - nearest match to the current
 nearestui activepalette, master(), uilook(), boxlook()

 IF palnum > gen(genMaxMasterPal) THEN gen(genMaxMasterPal) = palnum
 savepalette master(), palnum
 SaveUIColors uilook(), boxlook(), palnum
 RETURN YES
END FUNCTION

SUB nearestui (byval mimicpal as integer, newmaster() as RGBcolor, newui() as integer, newbox() as BoxStyle)
 'finds the nearest match newui() in newpal() to mimicpal's ui colours
 DIM referencepal(255) as RGBcolor
 DIM referenceui(uiColorLast) as integer
 DIM refboxstyle(uiBoxLast) as BoxStyle
 loadpalette referencepal(), mimicpal
 LoadUIColors referenceui(), refboxstyle(), mimicpal
 remappalette referencepal(), referenceui(), refboxstyle(), newmaster(), newui(), newbox()
END SUB

SUB remappalette (oldmaster() as RGBcolor, oldui() as integer, oldbox() as BoxStyle, newmaster() as RGBcolor, newui() as integer, newbox() as BoxStyle)
 'first the ui
 FOR i as integer = 0 TO UBOUND(oldui)
  WITH oldmaster(oldui(i))
   IF .col = newmaster(oldui(i)).col THEN
    newui(i) = oldui(i)
   ELSE
    newui(i) = nearcolor(newmaster(), .r, .g, .b)
   END IF
  END WITH
 NEXT
 'Then the boxstyles
 FOR i as integer = 0 TO UBOUND(oldbox)
  WITH oldmaster(oldbox(i).bgcol)
   IF .col = newmaster(oldbox(i).bgcol).col THEN
    newbox(i).bgcol = oldbox(i).bgcol
   ELSE
    newbox(i).bgcol = nearcolor(newmaster(), .r, .g, .b)
   END IF
  END WITH
  WITH oldmaster(oldbox(i).edgecol)
   IF .col = newmaster(oldbox(i).edgecol).col THEN
    newbox(i).edgecol = oldbox(i).edgecol
   ELSE
    newbox(i).edgecol = nearcolor(newmaster(), .r, .g, .b)
   END IF
  END WITH
 NEXT
END SUB

SUB inputpasw()
DIM tog as integer = 0
DIM oldpassword as integer = (checkpassword("") = 0)
DIM pas as string
setkeys YES
DO
 setwait 55
 setkeys YES
 tog = tog XOR 1
 IF keyval(ccCancel) > 1 THEN EXIT DO
 IF keyval(scEnter) > 1 THEN
'  IF oldpassword = NO THEN
 writepassword pas
  EXIT DO
 END IF
 IF keyval(scF1) > 1 THEN show_help "input_password"
 strgrabber pas, 17
 clearpage dpage
 textcolor uilook(uiMenuItem), 0
 printstr "You can require a password for this", 0, 0, dpage
 printstr "game to be opened in " + CUSTOMEXE, 0, 8, dpage
 printstr "This does not encrypt your file, and", 0, 16, dpage
 printstr "should not be considered as any security", 0, 24, dpage
 IF oldpassword THEN
  printstr "PASSWORD SET. NEW PASSWORD:", 30, 64, dpage
  IF LEN(pas) = 0 THEN printstr "(Hit Enter to remove)", 30, 94, dpage
 ELSE
  printstr "NO PASSWORD. NEW PASSWORD:", 30, 64, dpage
 END IF
 IF LEN(pas) THEN
  textcolor uilook(uiSelectedItem + tog), uilook(uiHighlight)
  printstr pas, 30, 74, dpage
 ELSE
  textcolor uilook(uiMenuItem), uilook(uiHighlight)
  printstr "(NONE)", 30, 74, dpage
 END IF
 SWAP vpage, dpage
 setvispage vpage
 dowait
LOOP
END SUB

SUB generate_battlesystem_menu(menu() as string, enabled() as bool, greyout() as bool)
 flusharray enabled(), UBOUND(enabled), YES
 flusharray greyout(), UBOUND(greyout), NO

 menu(0) = "Previous Menu"

 menu(1) = " Mechanics"
 enabled(1) = NO
 greyout(1) = YES

 menu(2) = "Battle Mode: "
 IF gen(genBattleMode) = 0 THEN
  menu(2) &= "Active-time"
 ELSE
  menu(2) &= "Turn-based"
 END IF
 menu(3) = "Battle preference bitsets..."
 menu(4) = "Number of Elements: " & gen(genNumElements)
 menu(5) = "Hero Elemental Resistance Calculation..."
 menu(6) = "Mark non-elemental elementals..."

 enabled(7) = NO
 menu(8) = " Stats and Experience"
 enabled(8) = NO
 greyout(8) = YES
 menu(9) = "Stat Caps..."
 menu(10) = "View Experience Chart..."
 menu(11) = "Experience given to heroes..."
 enabled(11) = NO
 menu(12) = " ...swapped-out and unlocked: " & gen(genUnlockedReserveXP) & "%"
 menu(13) = " ...swapped-out and locked: " & gen(genLockedReserveXP) & "%"
 '--Disabled because it is not ready yet
 'menu() = "Stat Growth Options..."

 menu(14) = "Hero Weak state below: " & gen(genHeroWeakHP) & "% " & statnames(statHP)
 menu(15) = "Enemy Weak state below: " & gen(genEnemyWeakHP) & "% " & statnames(statHP)

 enabled(16) = NO
 menu(17) = " Display"
 enabled(17) = NO
 greyout(17) = YES

 menu(18) = "Poison Indicator: " & gen(genPoisonChar) & " " & CHR(gen(genPoisonChar))
 menu(19) = "Stun Indicator: " & gen(genStunChar) & " " & CHR(gen(genStunChar))
 menu(20) = "Mute Indicator: " & gen(genMuteChar) & " " & CHR(gen(genMuteChar))
 menu(21) = "Regen Indicator: " & gen(genRegenChar) & " " & CHR(gen(genRegenChar))
 menu(22) = "Default Enemy Dissolve: " & dissolve_type_caption(gen(genEnemyDissolve))
 menu(23) = "Damage Display Time: " & gen(genDamageDisplayTicks) & " ticks (" & seconds_estimate(gen(genDamageDisplayTicks)) & " sec)"
 menu(24) = "Damage Display Rises: " & gen(genDamageDisplayRise) & " pixels"
 menu(25) = "Rewards Display: " & IIF(gen(genSkipBattleRewardsTicks) = 0, "Wait for keypress", gen(genSkipBattleRewardsTicks) & " ticks (" & seconds_estimate(gen(genSkipBattleRewardsTicks)) & " secs)")

 enabled(26) = NO
 menu(27) = " Other Defaults"
 enabled(27) = NO
 greyout(27) = YES

 menu(28) = "Attacks Provoke Counterattacks: " & counter_provoke_captions(gen(genDefCounterProvoke))
END SUB

SUB battleoptionsmenu ()
 CONST maxMenu = 28
 DIM menu(maxMenu) as string
 DIM menu_display(maxMenu) as string
 DIM min(maxMenu) as integer
 DIM max(maxMenu) as integer
 DIM index(maxMenu) as integer
 DIM enabled(maxMenu) as bool
 DIM greyout(maxMenu) as bool
 DIM selectst as SelectTypeState
 DIM state as MenuState
 WITH state
  .autosize = YES
  .last = maxMenu
  .need_update = YES
 END WITH
 DIM menuopts as MenuOptions
 menuopts.disabled_col = uilook(eduiHeading)  'For section headings

 'I think these things are here (and not upgrade) because we don't want to  force them on games
 IF gen(genPoisonChar) <= 0 THEN gen(genPoisonChar) = 161
 IF gen(genStunChar) <= 0 THEN gen(genStunChar) = 159
 IF gen(genMuteChar) <= 0 THEN gen(genMuteChar) = 163
 'Regen icon is newer, so let's not default it unexpectedly
 IF gen(genRegenChar) <= 0 THEN gen(genRegenChar) = 32
 
 IF gen(genBattleMode) < 0 ORELSE gen(genBattleMode) > 1 THEN
  visible_debug "WARNING: invalid gen(genBattleMode) " & gen(genBattleMode) & " resorting to active mode"
  gen(genBattleMode) = 0
 END IF
 
 index(2) = genBattleMode
 min(2) = 0
 max(2) = 1
 index(4) = genNumElements
 min(4) = 1
 max(4) = 64
 index(12) = genUnlockedReserveXP
 max(12) = 1000
 index(13) = genLockedReserveXP
 max(13) = 1000
 min(14) = 1
 max(14) = 100
 index(14) = genHeroWeakHP
 min(15) = 1
 max(15) = 100
 index(15) = genEnemyWeakHP
 index(18) = genPoisonChar
 index(19) = genStunChar
 index(20) = genMuteChar
 index(21) = genRegenChar
 FOR i as integer = 18 TO 21
  min(i) = 32
  max(i) = 255
 NEXT
 index(22) = genEnemyDissolve
 max(22) = dissolveTypeMax
 index(23) = genDamageDisplayTicks
 max(23) = 1000
 index(24) = genDamageDisplayRise
 max(24) = 1000
 min(24) = -1000
 index(25) = genSkipBattleRewardsTicks
 max(25) = 100000
 min(25) = 0
 index(28) = genDefCounterProvoke
 max(28) = provokeLAST
 min(28) = 1  ' Can't select 'Default'

 generate_battlesystem_menu menu(), enabled(), greyout()

 setkeys YES
 DO
  setwait 55
  setkeys YES

  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "battle_system_options"
  usemenu state, enabled()
  IF enter_space_click(state) THEN
   IF state.pt = 0 THEN EXIT DO
   IF state.pt = 3 THEN edit_battle_bitsets
   IF state.pt = 5 THEN equipmergemenu
   IF state.pt = 6 THEN mark_non_elemental_elementals
   IF state.pt = 9 THEN statcapsmenu
   IF state.pt = 10 THEN experience_chart
   IF min(state.pt) = 32 AND max(state.pt) = 255 THEN  'Character field
    DIM d as string = charpicker
    IF d <> "" THEN
     gen(index(state.pt)) = ASC(d)
     state.need_update = YES
    END IF
   END IF
  END IF
  IF index(state.pt) THEN
   IF intgrabber(gen(index(state.pt)), min(state.pt), max(state.pt)) THEN state.need_update = YES
  END IF

  IF state.need_update THEN
   generate_battlesystem_menu menu(), enabled(), greyout()
   state.need_update = NO
  END IF

  IF select_by_typing(selectst, NO) THEN
   select_on_word_boundary menu(), selectst, state
  END IF

  clearpage vpage
  draw_fullscreen_scrollbar state, , vpage
  highlight_menu_typing_selection menu(), menu_display(), selectst, state
  standardmenu menu_display(), state, greyout(), 0, 0, vpage, menuopts
  setvispage vpage
  dowait
 LOOP
END SUB

SUB statcapsmenu
 CONST maxMenu = 15
 DIM m(maxMenu) as string
 DIM menu_display(maxMenu) as string
 DIM max(maxMenu) as integer
 DIM index(maxMenu) as integer
 DIM selectst as SelectTypeState
 DIM state as MenuState
 state.last = maxMenu
 state.size = 24
 state.need_update = YES
 DIM i as integer

 index(1) = genDamageCap
 FOR i as integer = 2 TO 2 + statLast  '2 to 13
  index(i) = genStatCap + (i - 2)
 NEXT
 index(14) = genLevelCap
 index(15) = genMaxLevel

 max(1) = 32767  'Damage
 'Stat caps
 '(note: you can use setherostat to set stats to anything, even above 32767)
 FOR i as integer = 2 TO 2 + statLast
  max(i) = 32767
 NEXT
 max(14) = gen(genMaxLevel)  'Level cap is capped to Max Level
 max(15) = 99  'Max Level is capped to 99 ... FIXME: this could go higher!
 DO
  setwait 55
  setkeys YES

  IF keyval(ccCancel) > 1 OR (state.pt = 0 AND enter_space_click(state)) THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "stat_caps_menu"
  usemenu state
  IF state.pt > 0 THEN
   IF intgrabber(gen(index(state.pt)), 0, max(state.pt)) THEN state.need_update = YES
  END IF
  IF state.need_update THEN
   state.need_update = NO
   m(0) = "Previous Menu"
   m(1) = "Damage Cap: "
   IF gen(genDamageCap) = 0 THEN m(1) += "None" ELSE m(1) &= gen(genDamageCap)
   FOR i as integer = 0 TO statLast
    m(2 + i) = statnames(i) + " Cap: " _
               + IIF(gen(genStatCap + i), STR(gen(genStatCap + i)), "None")
   NEXT
   IF gen(genLevelCap) > gen(genMaxLevel) THEN gen(genLevelCap) = gen(genMaxLevel)
   max(14) = gen(genMaxLevel)
   m(14) = "Initial Level Cap: " & gen(genLevelCap)
   m(15) = "Maximum Level: " & gen(genMaxLevel)
  END IF

  IF select_by_typing(selectst, NO) THEN
   select_on_word_boundary m(), selectst, state
  END IF

  clearpage vpage
  highlight_menu_typing_selection m(), menu_display(), selectst, state
  standardmenu menu_display(), state, 0, 0, vpage
  setvispage vpage
  dowait
 LOOP
END SUB

FUNCTION merge_elementals_example(byval exampleno as integer, example() as single, byval formula as integer) as string
 DIM ret as string = "Ex" & exampleno
 FOR i as integer = 0 TO 3
  DIM temp as string
  IF i >= 1 AND i <= 2 AND formula = 2 THEN  'Show equipment as additive changes
   temp = format_percent(example(i) - 1.0, 3)
   IF LEFT(temp, 1) <> "-" THEN temp = "+" + temp
  ELSE
   temp = format_percent(example(i), 3)
  END IF
  ret += RIGHT("       " + temp, 9)
 NEXT
 RETURN ret
END FUNCTION

SUB generate_equipmerge_preview(byval formula as integer, menu() as string, greyed_out() as integer, ex9() as single)
 FOR i as integer = 1 TO 3
  greyed_out(i) = YES
 NEXT
 greyed_out(1 + gen(genEquipMergeFormula)) = NO
 FOR i as integer = 4 TO UBOUND(menu)
  menu(i) = ""
 NEXT

 DIM _NaN as single = 0.0f
 _NaN = 0.0f/_NaN

 DIM ex1(3) as single = {1, 1, 3, _NaN}
 DIM ex2(3) as single = {1, 2, 2, _NaN}
 DIM ex3(3) as single = {0, 0, 1, _NaN}
 DIM ex4(3) as single = {0.5, 1, 2, _NaN}
 DIM ex5(3) as single = {-1, 1.5, 2, _NaN}
 DIM ex6(3) as single = {-1, -1, -1, _NaN}
 DIM ex7(3) as single = {2, 0.5, 0.5, _NaN}
 DIM ex8(3) as single = {1, -1.2, -1.2, _NaN}
 ex9(3) = _NaN

 IF formula = -1 THEN
  menu(9) = "Select a formula to see examples"
 ELSE
  ex1(3) = equip_elemental_merge(ex1(), formula)
  ex2(3) = equip_elemental_merge(ex2(), formula)
  ex3(3) = equip_elemental_merge(ex3(), formula)
  ex4(3) = equip_elemental_merge(ex4(), formula)
  ex5(3) = equip_elemental_merge(ex5(), formula)
  ex6(3) = equip_elemental_merge(ex6(), formula)
  ex7(3) = equip_elemental_merge(ex7(), formula)
  ex8(3) = equip_elemental_merge(ex8(), formula)
  ex9(3) = equip_elemental_merge(ex9(), formula)

  menu(9) = "Examples:"
  menu(10) = "        Hero   Equip1   Equip2   Result"
  menu(11) = merge_elementals_example(1, ex1(), formula)
  menu(12) = merge_elementals_example(2, ex2(), formula)
  menu(13) = merge_elementals_example(3, ex3(), formula)
  menu(14) = merge_elementals_example(4, ex4(), formula)
  menu(15) = merge_elementals_example(5, ex5(), formula)
  menu(16) = merge_elementals_example(6, ex6(), formula)
  menu(17) = merge_elementals_example(7, ex7(), formula)
  menu(18) = merge_elementals_example(8, ex8(), formula)
  menu(19) = merge_elementals_example(9, ex9(), formula)

  IF formula = 2 THEN
   menu(21) = "(Equipment values are displayed"
   menu(22) = "differently when this is chosen)"
  END IF
 END IF
END SUB

SUB equipmergemenu
 DIM menu(22) as string
 DIM greyed_out(22) as integer
 DIM st as MenuState
 st.size = 24
 st.last = 3
 st.need_update = YES
 DIM tog as integer

 'Random example which changes on entering the menu
 DIM ex9(3) as single = {rando(), 3*rando()-1.5, 1+rando()}

 menu(0) = "Previous Menu"
 menu(1) = "Old awful formula (multiplication-like)"
 menu(2) = "Combine resistances by multiplication"
 menu(3) = "Combine resistances by addition"
 DO
  setwait 55
  setkeys
  tog XOR= 1
  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "equip_elemental_formula"
  IF enter_space_click(st) THEN
   IF st.pt = 0 THEN
    EXIT DO
   ELSE
    gen(genEquipMergeFormula) = st.pt - 1
    st.need_update = YES
   END IF
  END IF
  IF usemenu(st) THEN st.need_update = YES

  IF st.need_update THEN
   generate_equipmerge_preview st.pt - 1, menu(), greyed_out(), ex9()
   st.need_update = NO
  END IF

  clearpage vpage
  FOR i as integer = 0 TO UBOUND(menu)
   IF greyed_out(i) THEN
    textcolor uilook(uiDisabledItem), 0
    IF st.pt = i THEN textcolor uilook(uiSelectedDisabled + tog), 0
   ELSE
    textcolor uilook(uiMenuItem), 0
    IF st.pt = i THEN textcolor uilook(uiSelectedItem + tog), 0
   END IF
   printstr menu(i), 0, i * 8, vpage, YES
  NEXT i
  setvispage vpage
  dowait
 LOOP
END SUB

SUB startingdatamenu
 CONST maxMenu = 6
 DIM m(maxMenu) as string
 DIM menu_display(maxMenu) as string
 DIM max(maxMenu) as integer
 DIM index(maxMenu) as integer
 DIM selectst as SelectTypeState
 DIM state as MenuState
 state.last = maxMenu
 state.size = 24
 state.need_update = YES
 DIM as integer lastmap = -1

 index(1) = genStartX
 index(2) = genStartY
 index(3) = genStartMap
 max(3) = gen(genMaxMap)
 index(4) = genStartHero
 max(4) = gen(genMaxHero)
 index(5) = genStartMoney
 max(5) = 32767
 index(6) = genStartTextbox
 max(6) = gen(genMaxTextbox)
 DO
  setwait 55
  setkeys YES

  IF keyval(ccCancel) > 1 OR (state.pt = 0 AND enter_space_click(state)) THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "new_game_data"
  usemenu state
  IF state.pt = 6 THEN
   state.need_update OR= textboxgrabber(gen(index(state.pt)), state)
  ELSEIF state.pt > 0 THEN
   IF intgrabber(gen(index(state.pt)), 0, max(state.pt)) THEN state.need_update = YES
  END IF
  IF state.pt = 4 THEN
   IF enter_space_click(state) THEN
    gen(genStartHero) = hero_picker(gen(genStartHero))
    state.need_update = YES
   END IF
  END IF
  IF state.need_update THEN
   state.need_update = NO
   max(3) = gen(genMaxMap)
   max(4) = gen(genMaxHero)
   max(6) = gen(genMaxTextbox)
   IF lastmap <> gen(genStartMap) THEN
    DIM fh as integer
    OPENFILE(maplumpname(gen(genStartMap), "t"), FOR_BINARY, fh)
    SEEK #fh, 8
    max(1) = Readshort(fh, -1) - 1 'map width
    max(2) = ReadShort(fh, -1) - 1 'map height
    CLOSE #fh
    gen(genStartX) = small(gen(genStartX), max(1))
    gen(genStartY) = small(gen(genStartY), max(2))
    lastmap = gen(genStartMap)
   END IF

   m(0) = "Previous Menu"
   m(1) = "Starting X: " & gen(genStartX)
   m(2) = "Starting Y: " & gen(genStartY)
   m(3) = "Starting Map: " & gen(genStartMap) & " " & getmapname(gen(genStartMap))
   m(4) = "Starting Hero: " & gen(genStartHero) & " " & getheroname(gen(genStartHero))
   m(5) = "Starting Money: " & gen(genStartMoney)
   IF gen(genStartTextbox) = 0 THEN
    m(6) = "Starting Textbox: None"
   ELSE
    m(6) = "Starting Textbox: " & gen(genStartTextbox) & " " & textbox_preview_line(gen(genStartTextbox), vpages(vpage)->w - 60)
   END IF
  END IF

  IF select_by_typing(selectst, NO) THEN
   select_on_word_boundary m(), selectst, state
  END IF

  clearpage vpage
  IF state.pt = 6 THEN edgeprint THINGGRABBER_TOOLTIP, 0, pBottom, uilook(uiDisabledItem), vpage
  highlight_menu_typing_selection m(), menu_display(), selectst, state
  standardmenu menu_display(), state, 0, 0, vpage
  setvispage vpage
  dowait
 LOOP
END SUB


TYPE GeneralSettingsMenu EXTENDS ModularMenu
 index(any) as integer    'gen() index, or -1
 min(any) as integer
 max(any) as integer

 longname as string
 aboutline as string

 DECLARE SUB add_item(subtype as integer = 0, id as integer = -1, text as string = "", canselect as bool = YES, heading as bool = NO)
 DECLARE SUB gen_int(genidx as integer, minvalue as integer, maxvalue as integer)
 DECLARE SUB update()
 DECLARE SUB update_edit_time()
END TYPE

SUB GeneralSettingsMenu.add_item(subtype as integer = 0, id as integer = -1, text as string = "", canselect as bool = YES, heading as bool = NO)
 BASE.add_item subtype, id, text, canselect, heading
 a_append index(), 0
 a_append min(), 0
 a_append max(), 0
END SUB

'Applies to last add_item()
SUB GeneralSettingsMenu.gen_int(genidx as integer, minvalue as integer, maxvalue as integer)
 DIM i as integer = UBOUND(index)
 index(i) = genidx
 min(i) = minvalue
 max(i) = maxvalue
END SUB


SUB GeneralSettingsMenu.update()
 DIM tmp as string

 clear_menu()
 ERASE index
 ERASE min
 ERASE max

 add_item 0, ,  "Return to Main Menu"

 header " Game Title & Info"
 add_item 1, ,  "Long name: " + this.longname
 add_item 2, ,  "About line: " + this.aboutline
 add_item 3, ,  "Title Screen..."

 '-------------------------
 header " Major Settings"
 add_item 4, ,  "New Games..."
 add_item 5, ,  "Saved Games..."
 add_item 8, ,  "Battle System..."
 add_item 6, ,  "Preference Bitsets..."
 add_item 7, ,  "Backwards-Compatibility..."

 '-------------------------
 header " Controls"
 add_item 16, , "Mouse Options..."
 add_item 15, , "Platform-Specific Controls..."

 '-------------------------
 header " Scripts"
 add_item 9, ,  "Special Plotscripts..."
 add_item 10, , "Error Display..."

 '-------------------------
 header " Graphics"
 add_item 12, , "Master Palettes..."
 add_item 14, , "Window-Size Options..."

 DIM fps as string
 '16ms and 33ms are special-cased to be exactly 60/30fps
 IF gen(genMillisecPerFrame) = 16 THEN
  fps = "60"
 ELSEIF gen(genMillisecPerFrame) = 33 THEN
  fps = "30"
 ELSE
  fps = FORMAT(small(60., 1000 / gen(genMillisecPerFrame)), ".#")
 END IF
 add_item , , "Framerate: " & fps & " frames/sec (" & gen(genMillisecPerFrame) & "ms/frame)"
 gen_int genMillisecPerFrame, 16, 200

 tmp = "Minimap style: "
 SELECT CASE gen(genMinimapAlgorithm)
  CASE minimapScaled :   tmp &= "Smoothly scaled down"
  CASE minimapScatter :  tmp &= "Pick random color"
  CASE minimapMajority : tmp &= "Pick most common color"
 END SELECT
 add_item , , tmp
 gen_int genMinimapAlgorithm, 0, minimapLAST

 tmp = "Camera following a hero/NPC centers on: "
 SELECT CASE gen(genCameraOnWalkaboutFocus)
  CASE 0 : tmp &= "tile"
  CASE 1 : tmp &= "sprite"
  CASE 2 : tmp &= "sprite minus Z"
 END SELECT
 add_item , , tmp
 gen_int genCameraOnWalkaboutFocus, 0, 2

 '-------------------------
 header " Audio"
 add_item 11, , "Global Music and Sound Effects..."
 add_item , , "Initial music volume: " & gen(genMusicVolume) & "%"
 gen_int genMusicVolume, 0, 100
 add_item , , "Initial sound effects volume: " & gen(genSFXVolume) & "%"
 gen_int genSFXVolume, 0, 100

 '-------------------------
 header " Inventory"
 IF gen(genMaxInventory) = 0 THEN
  add_item 20, , "Inventory size: Default (" & (last_inv_slot() \ 3) + 1 & " rows)"
 ELSE
  add_item 20, , "Inventory size: " & (last_inv_slot() \ 3) + 1 & " rows, " & gen(genMaxInventory) + 1 & " slots"
 END IF
 gen_int genMaxInventory, 0, (inventoryMax + 1) \ 3

 tmp = "Inventory autosort: "
 SELECT CASE gen(genAutosortScheme)
  CASE 0: tmp &= "by item type/uses"
  CASE 1: tmp &= "by whether usable"
  CASE 2: tmp &= "alphabetically"
  CASE 3: tmp &= "by item ID number"
  CASE 4: tmp &= "no reordering"
 END SELECT
 add_item , , tmp
 gen_int genAutosortScheme, 0, 4

 add_item , , "Default maximum item stack size: " & gen(genItemStackSize)
 gen_int genItemStackSize, 1, 99

 tmp = "Display '" & CHR(1) & "1' in inventory: "  'CHR(1) is the x symbol
 SELECT CASE gen(genInventSlotx1Display)
  CASE 0: tmp &= "always"
  CASE 1: tmp &= "never"
  CASE 2: tmp &= "only if stackable"
 END SELECT
 add_item , , tmp
 gen_int genInventSlotx1Display, 0, 2

 '-------------------------
 header " Misc"
 add_item 17, , "In-App Purchases... (experimental)"
 add_item 13, , "Password For Editing..."

 header " Stats"
 add_item  , , "Time spent editing...", NO
 add_item 18, , " this session:", NO
 add_item 19, , " in total:", NO
 DIM created as double = GetChildNodeFloat(get_general_reld, "created", 0.)
 IF created <> 0. THEN
  add_item , , "Game created " & FORMAT(created, "yyyy mmm dd hh:mm"), NO
 END IF

 'Next free ID number: 21

END SUB

'Update the edit_time display
SUB GeneralSettingsMenu.update_edit_time()
 this.menu(a_find(this.itemtypes(), 18)) = " this session: " & format_duration(active_seconds, 0)
 'Round edit_time to integer so it ticks in sync with 'This session'
 DIM total_edit_time as double = INT(GetChildNodeFloat(get_general_reld, "edit_time")) + active_seconds
 DIM total_text as string = " in total: "
 '"created" was added at the same time as "edit_time", so if it's missing then the total is inaccurate.
 IF GetChildNodeExists(get_general_reld, "created") = NO THEN total_text &= "at least "
 total_text &= format_duration(total_edit_time, 0)
 this.menu(a_find(this.itemtypes(), 19)) = total_text
END SUB

SUB general_data_editor ()
 STATIC shown_framerate_warning as bool = NO

 'make sure genMaxInventory is a multiple of 3 (other valyes possible in older versions and Fufluns nightlies)
 IF gen(genMaxInventory) THEN gen(genMaxInventory) = last_inv_slot()

 DIM genmenu as GeneralSettingsMenu
 genmenu.aboutline = load_aboutline()
 genmenu.longname = load_gamename()
 genmenu.update()

 DIM selectst as SelectTypeState
 DIM state as MenuState
 WITH state
  .autosize = YES
  .autosize_ignore_pixels = 4
  .last = UBOUND(genmenu.menu)
 END WITH
 DIM menuopts as MenuOptions
 menuopts.disabled_col = uilook(eduiHeading)
 menuopts.itemspacing = 1
 calc_menustate_size state, menuopts, 4, 4, vpage  'Avoid scrollbar length glitch

 setkeys YES
 DO
  setwait 55
  setkeys YES

  IF keyval(ccCancel) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "general_game_data"
  usemenu state, genmenu.selectable()

  IF enter_space_click(state) THEN
   SELECT CASE genmenu.itemtypes(state.pt)
    CASE 0:  EXIT DO
    CASE 3
     DIM backdropb as BackdropSpriteBrowser
     gen(genTitle) = backdropb.browse(gen(genTitle))
    CASE 4:  startingdatamenu
    CASE 5:  edit_savegame_options
    CASE 6:  edit_general_bitsets
    CASE 7:  edit_backcompat_bitsets
    CASE 8:  battleoptionsmenu
    CASE 9:  generalscriptsmenu
    CASE 10: script_error_mode_menu
    CASE 11: generalmusicsfxmenu
    CASE 12: masterpalettemenu
    CASE 13: inputpasw
    CASE 14: resolution_menu
    CASE 15: edit_platform_controls
    CASE 16: edit_mouse_options
    CASE 17: edit_purchase_options
   END SELECT
  END IF

  DIM enable_strgrabber as bool = NO

  SELECT CASE genmenu.itemtypes(state.pt)
   CASE 1  'Long name
    IF LEN(selectst.query) = 0 THEN
     enable_strgrabber = YES
     state.need_update OR= strgrabber(genmenu.longname, 38)
    END IF
   CASE 2  'About line
    IF LEN(selectst.query) = 0 THEN
     enable_strgrabber = YES
     state.need_update OR= strgrabber(genmenu.aboutline, 38)
    END IF
   CASE 20 'genMaxInventory
    DIM as integer temp = (gen(genMaxInventory) + 1) \ 3
    IF intgrabber(temp, genmenu.min(state.pt), genmenu.max(state.pt)) THEN
     gen(genMaxInventory) = temp * 3 - 1
     IF temp = 0 THEN gen(genMaxInventory) = 0
     state.need_update = YES
    END IF
   CASE ELSE
    WITH genmenu
     IF .index(state.pt) ANDALSO intgrabber(gen(.index(state.pt)), .min(state.pt), .max(state.pt)) THEN
      state.need_update = YES
      IF .index(state.pt) = genMillisecPerFrame ANDALSO shown_framerate_warning = NO THEN
       show_help "framerate_warning"
       shown_framerate_warning = YES
      END IF
     END IF
    END WITH
  END SELECT

  IF state.need_update THEN
   genmenu.update()
   state.last = UBOUND(genmenu.menu)
   state.need_update = NO
  END IF
  genmenu.update_edit_time()

  IF enable_strgrabber = NO ANDALSO select_by_typing(selectst, NO) THEN
   select_on_word_boundary genmenu.menu(), selectst, state
  END IF

  clearpage dpage
  draw_fullscreen_scrollbar state, , dpage
  DIM menu_display(UBOUND(genmenu.menu)) as string
  highlight_menu_typing_selection genmenu.menu(), menu_display(), selectst, state
  standardmenu menu_display(), state, genmenu.shaded(), 4, 4, dpage, menuopts

  SWAP vpage, dpage
  setvispage vpage
  dowait
 LOOP
 
 '--write long name and about line
 save_gamename genmenu.longname
 save_aboutline genmenu.aboutline

 '--Also use the in-game setting for previewing stuff in Custom
 set_music_volume 0.01 * gen(genMusicVolume)
 set_global_sfx_volume 0.01 * gen(genSFXVolume)
END SUB
