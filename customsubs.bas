'OHRRPGCE - Custom common code
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
' This file is for general purpose code use by CUSTOM but not by GAME.

#include "compat.bi"
#include "allmodex.bi"
#include "common.bi"
#include "loading.bi"
#include "const.bi"
#include "scrconst.bi"
#include "cglobals.bi"
#include "scancodes.bi"
#include "reload.bi"
#include "slices.bi"

#include "customsubs.bi"

'Subs and functions only used here
DECLARE SUB import_textboxes_warn (BYREF warn AS STRING, s AS STRING)
DECLARE SUB seekscript (BYREF temp AS INTEGER, BYVAL seekdir AS INTEGER, BYVAL triggertype AS INTEGER)

OPTION EXPLICIT

FUNCTION tag_grabber (BYREF n AS INTEGER, min AS INTEGER=-999, max AS INTEGER=999) AS INTEGER
 IF intgrabber(n, min, max) THEN RETURN YES
 IF enter_or_space() THEN
  DIM browse_tag AS INTEGER
  browse_tag = tagnames(n, YES)
  IF browse_tag >= 2 OR browse_tag <= 2 THEN
   n = browse_tag
   RETURN YES
  END IF
 END IF
 RETURN NO
END FUNCTION

FUNCTION tagnames (starttag AS INTEGER=0, picktag AS INTEGER=NO) AS INTEGER
 DIM state AS MenuState
 DIM thisname AS STRING
 DIM remembertag AS INTEGER = starttag
 IF gen(genMaxTagname) < 1 THEN gen(genMaxTagname) = 1
 DIM menu(gen(genMaxTagname)) AS STRING
 IF picktag THEN
  menu(0) = "Cancel"
 ELSE
  menu(0) = "Previous Menu"
 END IF
 DIM i AS INTEGER
 FOR i = 2 TO gen(genMaxTagname) + 1
  'Load all tag names plus the first blank name
  menu(i - 1) = "Tag " & i & ":" & load_tag_name(i)
 NEXT i

 clearpage 0
 clearpage 1

 DIM tagsign AS INTEGER
 tagsign = SGN(starttag)
 IF tagsign = 0 THEN tagsign = 1

 state.size = 24
 state.last = gen(genMaxTagname)

 state.pt = 0
 IF ABS(starttag) >= 2 THEN state.pt = small(ABS(starttag) - 1, gen(genMaxTagName))
 IF state.pt >= 1 THEN thisname = load_tag_name(state.pt + 1)

 DIM tog AS INTEGER = 0
 setkeys
 DO
  setwait 55
  setkeys
  tog = tog XOR 1
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "tagnames"
  IF usemenu(state) THEN
   IF state.pt >= 1 AND state.pt <= gen(genMaxTagName) THEN
    thisname = load_tag_name(state.pt + 1)
   ELSE
    thisname = ""
   END IF
  END IF
  IF state.pt = 0 AND enter_or_space() THEN EXIT DO
  IF state.pt > 0 AND state.pt <= gen(genMaxTagName) THEN
   IF picktag THEN
    IF keyval(28) > 1 THEN
     RETURN (state.pt + 1) * tagsign
    END IF
   END IF
   IF strgrabber(thisname, 20) THEN
    save_tag_name thisname, state.pt + 1
    menu(state.pt) = "Tag " & state.pt + 1 & ":" & thisname
    IF state.pt = gen(genMaxTagName) THEN
     IF gen(genMaxTagName) < 999 THEN
      gen(genMaxTagName) += 1
      REDIM PRESERVE menu(gen(genMaxTagName)) AS STRING
      menu(gen(genMaxTagName)) = "Tag " & gen(genMaxTagName) + 1 & ":"
      state.last += 1
     END IF
    END IF
   END IF
  END IF

  draw_fullscreen_scrollbar state, ,dpage
  standardmenu menu(), state, 0, 0, dpage

  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP

 RETURN remembertag
END FUNCTION

FUNCTION strgrabber (s AS STRING, maxl AS INTEGER) AS INTEGER
STATIC clip AS STRING
DIM shift AS INTEGER
DIM i AS INTEGER

DIM old AS STRING
old = s

'--BACKSPACE support
IF keyval(14) > 1 AND LEN(s) > 0 THEN s = LEFT(s, LEN(s) - 1)

'--copy support
IF (keyval(29) > 0 AND keyval(82) > 1) OR ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(83) > 0) OR (keyval(29) > 0 AND keyval(46) > 1) THEN clip = s

'--paste support
IF ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(82) > 1) OR (keyval(29) > 0 AND keyval(47) > 1) THEN s = LEFT(clip, maxl)

'--SHIFT support
shift = 0
IF keyval(54) > 0 OR keyval(42) > 0 THEN shift = 1

'--ALT support
IF keyval(56) THEN shift = shift + 2

'--adding chars
IF LEN(s) < maxl THEN

 IF keyval(57) > 1 THEN
  IF keyval(29) = 0 THEN
   '--SPACE support
   s = s + " "
  ELSE
   '--charlist support
   s = s + charpicker
  END IF
 ELSE
  IF keyval(29) = 0 THEN
   '--all other keys
   FOR i = 2 TO 53
    IF keyval(i) > 1 AND keyv(i, shift) > 0 THEN
     s = s + CHR(keyv(i, shift))
     EXIT FOR
    END IF
   NEXT i
  END IF
 END IF

END IF

'Return true of the string has changed
RETURN (s <> old)

END FUNCTION

FUNCTION charpicker() AS STRING

STATIC pt

DIM i AS INTEGER
DIM f(255)
DIM last AS INTEGER = -1
DIM linesize AS INTEGER
DIM offset AS XYPair

FOR i = 32 TO 255
 last = last + 1
 f(last) = i
NEXT i

linesize = 16
offset.x = 160 - (linesize * 9) \ 2
offset.y = 100 - ((last \ linesize) * 9) \ 2

DIM tog AS INTEGER = 0
setkeys
DO
 setwait 55
 setkeys
 tog = tog XOR 1
 IF keyval(scESC) > 1 THEN RETURN ""
 IF keyval(scF1) > 1 THEN show_help "charpicker"

 IF keyval(72) > 1 THEN pt = large(pt - linesize, 0)
 IF keyval(80) > 1 THEN pt = small(pt + linesize, last)
 IF keyval(75) > 1 THEN pt = large(pt - 1, 0)
 IF keyval(77) > 1 THEN pt = small(pt + 1, last)

 IF enter_or_space() THEN RETURN CHR(f(pt))

 FOR i = 0 TO last
  textcolor uilook(uiMenuItem), uilook(uiDisabledItem)
  IF (i MOD linesize) = (pt MOD linesize) OR (i \ linesize) = (pt \ linesize) THEN textcolor uilook(uiMenuItem), uilook(uiHighlight)
  IF pt = i THEN textcolor uilook(uiSelectedItem + tog), 0
  printstr CHR(f(i)), offset.x + (i MOD linesize) * 9, offset.y + (i \ linesize) * 9, dpage
 NEXT i

 textcolor uilook(uiMenuItem), 0
 printstr "ASCII " & f(pt), 78, 190, dpage
 FOR i = 2 TO 53
  IF f(pt) = keyv(i, 2) THEN printstr "ALT+" + UCASE$(CHR$(keyv(i, 0))), 178, 190, dpage
  IF f(pt) = keyv(i, 3) THEN printstr "ALT+SHIFT+" + UCASE$(CHR$(keyv(i, 0))), 178, 190, dpage
 NEXT i
 IF f(pt) = 32 THEN printstr "SPACE", 178, 190, dpage

 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

END FUNCTION

SUB ui_color_editor(palnum AS INTEGER)
 DIM i AS INTEGER
 DIM index AS INTEGER
 DIM default_colors(uiColors) AS INTEGER

 DIM sample_menu AS MenuDef
 WITH sample_menu
  .anchor.x = 1
  .anchor.y = -1
  .offset.x = 156
  .offset.y = -96
 END WITH
 append_menu_item sample_menu, "Sample"
 append_menu_item sample_menu, "Example"
 i = append_menu_item(sample_menu, "Disabled")
 sample_menu.items(i).disabled = YES
 
 DIM sample_state AS MenuState
 sample_state.active = YES
 init_menu_state sample_state, sample_menu

 GuessDefaultUIColors default_colors()

 LoadUIColors uilook(), palnum

 DIM color_menu(uiColors + 1) AS STRING
 make_ui_color_editor_menu color_menu(), uilook()

 DIM state AS MenuState
 state.size = 22
 state.last = UBOUND(color_menu)

 DIM tog AS INTEGER = 0
 setkeys
 DO
  setwait 55
  setkeys
  tog = tog XOR 1
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "ui_color_editor"
  usemenu state

  index = state.pt - 1

  IF enter_or_space() THEN
   IF state.pt = 0 THEN
    EXIT DO
   ELSEIF state.pt < uiTextBoxFrame THEN
    'Color browser
    uilook(index) = color_browser_256(uilook(index))
    make_ui_color_editor_menu color_menu(), uilook() 
   END IF
  END IF

  SELECT CASE index
   CASE 0 TO 47
    IF intgrabber(uilook(index), 0, 255) THEN
     make_ui_color_editor_menu color_menu(), uilook()
    END IF
   CASE 48 TO 62
    IF zintgrabber(uilook(index), -1, gen(genMaxBoxBorder)) THEN
     make_ui_color_editor_menu color_menu(), uilook()
     clear_box_border_cache
    END IF
  END SELECT

  IF keyval(29) > 0 AND keyval(32) > 1 THEN ' Ctrl+D
   uilook(index) = default_colors(index)
   make_ui_color_editor_menu color_menu(), uilook()
  END IF

  '--update sample according to what you have highlighted
  sample_menu.boxstyle = 0
  sample_state.pt = 0
  SELECT CASE index
   CASE 5,6 ' selected disabled
    sample_state.pt = 2
   CASE 18 TO 47
    sample_menu.boxstyle = INT((state.pt - 19) / 2)
   CASE 48 TO 62
    sample_menu.boxstyle = index - 48
  END SELECT

  draw_menu sample_menu, sample_state, dpage
  standardmenu color_menu(), state, 10, 0, dpage
  FOR i = state.top TO state.top + state.size
   IF i > 0  AND i <= 48 THEN
    rectangle 0, 8 * (i - state.top), 8, 8, uilook(i - 1), dpage
   END IF
  NEXT i
  edgeprint "Ctrl+D to revert to default", 100, 190, uilook(uiText), dpage

  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
 SaveUIColors uilook(), palnum
END SUB

SUB make_ui_color_editor_menu(m() AS STRING, colors() AS INTEGER)
 DIM cap(17) AS STRING = {"Background", "Menu item", "Disabled item", _
     "Selected item (A)", "Selected item (B)", "Selected disabled item (A)", _
      "Selected disabled item (B)", "Hilight (A)", "Hilight (B)", "Time bar", _
      "Time bar (full)", "Health bar", "Health bar (flash)", "Default Text", _
      "Text outline", "Spell description", "Total money", "Vehicle shadow"}
 DIM i AS INTEGER
 m(0) = "Previous Menu"
 FOR i = 0 TO 17
  m(1 + i) = cap(i) & ": " & colors(i)
 NEXT i
 FOR i = 0 TO 14
  m(19 + i*2) = "Box style " & i & " color:  " & colors(18 + i*2)
  m(19 + i*2 + 1) = "Box style " & i & " border: " & colors(18 + i*2 + 1)
  m(49 + i) = "Box style " & i & " border image: " & zero_default(colors(48 + i), "none", -1)
 NEXT i
END SUB

FUNCTION color_browser_256(start_color AS INTEGER=0) AS INTEGER
 DIM i AS INTEGER
 DIM tog AS INTEGER = 0
 DIM spot AS XYPair
 DIM cursor AS XYPair
 cursor = xy_from_int(start_color, 16, 16)
 setkeys
 DO
  setwait 55
  setkeys
  tog = (tog + 1) MOD 256
  IF keyval(scESC) > 1 THEN RETURN start_color
  IF keyval(scF1) > 1 THEN show_help "color_browser"

  IF enter_or_space() THEN RETURN int_from_xy(cursor, 16, 16)

  IF keyval(72) > 1 THEN cursor.y = loopvar(cursor.y, 0, 15, -1)
  IF keyval(80) > 1 THEN cursor.y = loopvar(cursor.y, 0, 15, 1)
  IF keyval(75) > 1 THEN cursor.x = loopvar(cursor.x, 0, 15, -1)
  IF keyval(77) > 1 THEN cursor.x = loopvar(cursor.x, 0, 15, 1)

  FOR i = 0 TO 255
   spot = xy_from_int(i, 16, 16)
   IF spot.x = cursor.x AND spot.y = cursor.y THEN
    edgebox 64 + spot.x * 12 , 0 + spot.y * 12 , 12, 12, i, tog, dpage
   ELSE
    rectangle 64 + spot.x * 12 , 0 + spot.y * 12 , 12, 12, i, dpage
   END IF
  NEXT i

  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
END FUNCTION

FUNCTION xy_from_int(n AS INTEGER, wide AS INTEGER, high AS INTEGER) AS XYPair
 DIM pair AS XYPair
 pair.x = n MOD wide
 pair.y = small(INT(n / wide), high - 1)
 RETURN pair
END FUNCTION

FUNCTION int_from_xy(pair AS XYPair, wide AS INTEGER, high AS INTEGER) AS INTEGER
 RETURN bound(pair.y * wide + pair.x, 0, wide * high - 1)
END FUNCTION

FUNCTION pick_ogg_quality(BYREF quality AS INTEGER) AS INTEGER
 STATIC q AS INTEGER = 4
 DIM i AS INTEGER
 clearpage dpage
 clearpage vpage
 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(scESC) > 1 THEN RETURN -1   'cancel
  IF keyval(scF1) > 1 THEN show_help "pick_ogg_quality"
  IF enter_or_space() THEN EXIT DO
  intgrabber (q, -1, 10)
  centerbox 160, 100, 300, 40, 4, dpage
  edgeprint "Pick Ogg quality level (" & q & ")", 64, 86, uilook(uiText), dpage
  FOR i = 0 TO q + 1
   rectangle 30 + 21 * i, 100, 20, 16, uilook(uiText), dpage
  NEXT i
  swap vpage, dpage
  setvispage vpage
  dowait
 LOOP
 quality = q
 RETURN 0
END FUNCTION

FUNCTION yesno(capt AS STRING, BYVAL defaultval AS INTEGER=YES, escval AS INTEGER=NO) AS INTEGER
 IF defaultval = YES THEN
  defaultval = 0
 ELSEIF defaultval = NO THEN
  defaultval = 1
 END IF
 DIM result AS INTEGER
 result = twochoice(capt, "Yes", "No", defaultval, escval)
 IF result = 0 THEN RETURN YES
 IF result = 1 THEN RETURN NO
END FUNCTION

FUNCTION twochoice(capt AS STRING, strA AS STRING="Yes", strB AS STRING="No", defaultval AS INTEGER=YES, escval AS INTEGER=NO) AS INTEGER
 DIM state AS MenuState
 DIM menu AS MenuDef
 DIM result AS INTEGER

 append_menu_item menu, strA
 append_menu_item menu, strB

 state.active = YES
 init_menu_state state, menu
 state.pt = defaultval

 'Keep whatever was on the screen already as a background
 copypage vpage, dpage
 
 setkeys
 DO
  setwait 55
  setkeys

  IF keyval(1) > 1 THEN
   result = escval
   state.active = NO
  END IF

  IF enter_or_space() THEN
   result = state.pt
   state.active = NO
  END IF

  IF state.active = NO THEN EXIT DO
  
  usemenu state

  centerbox 160, 70, small(16 + LEN(capt) * 8, 320), 16, 2, 0
  edgeprint capt, xstring(capt, 160), 65, uilook(uiMenuItem), 0
  draw_menu menu, state, 0
  setvispage 0
  copypage 1, 0
  dowait
 LOOP
 setkeys

 RETURN result
END FUNCTION

FUNCTION needaddset (BYREF pt AS INTEGER, BYREF check AS INTEGER, what AS STRING) AS INTEGER
 IF pt <= check THEN RETURN NO
 IF yesno("Add new " & what & "?") THEN
  check += 1
  RETURN YES
 ELSE
  pt -= 1
 END IF
 RETURN NO
END FUNCTION

SUB keyboardsetup ()
 'There is a different implementation of this in yetmore2 for GAME
 DIM keyconst(207) AS STRING = {"1","2","3","4","5","6","7","8","9","0","-","=","","","q","w","e","r","t","y","u","i","o","p","[","]","","","a","s","d","f","g","h","j","k","l",";","'","`","","\","z","x","c","v","b","n","m",",",".","/", _
  "!","@","#","$","%","^","&","*","(",")","_","+","","","Q","W","E","R","T","Y","U","I","O","P","{","}","","","A","S","D","F","G","H","J","K","L",":"," ","~","","|","Z","X","C","V","B","N","M","<",">","?", _
  "�","�","�","�","�","�","�","�","�","�","�","�","","","�","�","�","�","�","�","�","�","�","�","�","�","","","�","�","�","�","�","�","�","�","�","�","�","�","","�","�","�","�","�","�","�","�","�","�","�", _
  "�","�","�","�","�","�","�","�","�","�","�","�","","","�","�","�","�","�","�","�","�","�","�","�","�","","","�","�","�","�","�","�","�","�","�","�","�","�","","�","�","�","�","�","�","�","�","�","�","�"}
 DIM temp AS STRING
 DIM AS INTEGER j, i
 FOR j = 0 TO 3
  FOR i = 2 TO 53
   temp = keyconst((i - 2) + j * 52)
   IF temp <> "" THEN keyv(i, j) = ASC(temp) ELSE keyv(i, j) = 0
  NEXT i
 NEXT j
 keyv(40, 1) = 34
END SUB

SUB edit_npc (BYREF npcdata AS NPCType)
 DIM i AS INTEGER

 DIM itemname AS STRING
 DIM boxpreview AS STRING
 DIM scrname AS STRING
 DIM vehiclename AS STRING
 DIM caption AS STRING
 DIM appearstring AS STRING

 DIM walk AS INTEGER = 0
 DIM tog AS INTEGER = 0

 DIM unpc(15) AS INTEGER, lnpc(15) AS INTEGER
 DIM menucaption(15) AS STRING, movetype(10) AS STRING, pushtype(7) AS STRING, usetype(5, 1) AS STRING

 DIM state AS MenuState
 state.size = 24
 state.first = -1
 state.last = 14
 state.top = -1
 state.pt = -1

 unpc(0) = gen(genMaxNPCPic)
 unpc(1) = 32767
 unpc(2) = 8
 unpc(3) = 5
 unpc(4) = -1
 unpc(5) = 2
 unpc(6) = gen(genMaxItem) + 1
 unpc(7) = 7
 unpc(8) = 2
 unpc(9) = 999
 unpc(10) = 999
 unpc(11) = 1
 unpc(12) = 0
 unpc(13) = 32767
 unpc(14) = 0
 FOR i = 0 TO 14
  lnpc(i) = 0
 NEXT i
 lnpc(1) = -1
 lnpc(9) = -999
 lnpc(10) = -999
 lnpc(13) = -32767
 unpc(4) = gen(genMaxTextbox)       'max text boxes
 unpc(12) = gen(genMaxRegularScript)'max scripts
 unpc(14) = gen(genMaxVehicle) + 1  'max vehicles

 menucaption(0) = "Picture"
 menucaption(1) = "Palette"
 menucaption(2) = "Move Type"
 menucaption(3) = "Move Speed"
 menucaption(4) = "Display Text"
 menucaption(5) = "When Activated"
 menucaption(6) = "Give Item:"
 menucaption(7) = "Pushability"
 menucaption(8) = "Activation: "
 menucaption(9) = "Appear if Tag"
 menucaption(10) = "Appear if Tag"
 menucaption(11) = "Usable"
 menucaption(12) = "Run Script: "
 menucaption(13) = "Script Argument"
 menucaption(14) = "Vehicle: "
 movetype(0) = "Stand Still"
 movetype(1) = "Wander"
 movetype(2) = "Pace"
 movetype(3) = "Right Turns"
 movetype(4) = "Left Turns"
 movetype(5) = "Random Turns"
 movetype(6) = "Chase You"
 movetype(7) = "Avoid You"
 movetype(8) = "Walk In Place"
 pushtype(0) = " Off"
 pushtype(1) = " Full"
 pushtype(2) = " Vertical"
 pushtype(3) = " Horizontal"
 pushtype(4) = " Up only"
 pushtype(5) = " Right Only"
 pushtype(6) = " Down Only"
 pushtype(7) = " Left Only"
 usetype(0, 0) = "Use"
 usetype(1, 0) = "Touch"
 usetype(2, 0) = "Step On"
 usetype(0, 1) = " Change Direction"
 usetype(1, 1) = " Face Player"
 usetype(2, 1) = " Do Not Face Player"

 npcdata.sprite = sprite_load(game & ".pt4", npcdata.picture, 8, 20, 20)
 npcdata.pal = palette16_load(game & ".pal", npcdata.palette, 4, npcdata.picture)

 itemname = load_item_name(npcdata.item, 0, 0)
 boxpreview = textbox_preview_line(npcdata.textbox)
 scrname = scriptname$(npcdata.script, plottrigger)
 vehiclename = load_vehicle_name(npcdata.vehicle - 1)

 setkeys
 DO
  setwait 55
  setkeys
  tog = tog XOR 1
  IF npcdata.movetype > 0 THEN walk = walk + 1: IF walk > 3 THEN walk = 0
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "edit_npc"
  usemenu state
  SELECT CASE state.pt
   CASE 0'--picture
    IF intgrabber(npcdata.picture, lnpc(state.pt), unpc(state.pt)) THEN
     sprite_unload @npcdata.sprite
     palette16_unload @npcdata.pal
     npcdata.sprite = sprite_load(game & ".pt4", npcdata.picture, 8, 20, 20)
     npcdata.pal = palette16_load(game & ".pal", npcdata.palette, 4, npcdata.picture)
    END IF
   CASE 1'--palette
    IF intgrabber(npcdata.palette, lnpc(state.pt), unpc(state.pt)) THEN
     palette16_unload @npcdata.pal
     npcdata.pal = palette16_load(game & ".pal", npcdata.palette, 4, npcdata.picture)
    END IF
    IF enter_or_space() THEN
     npcdata.palette = pal16browse(npcdata.palette, 4, npcdata.picture, 8, 20, 20)
     palette16_unload @npcdata.pal
     npcdata.pal = palette16_load(game & ".pal", npcdata.palette, 4, npcdata.picture)
    END IF
   CASE 2
    intgrabber(npcdata.movetype, lnpc(state.pt), unpc(state.pt))
   CASE 3
    'yuck.
    IF npcdata.speed = 10 THEN npcdata.speed = 3
    intgrabber(npcdata.speed, lnpc(state.pt), unpc(state.pt))
    IF npcdata.speed = 3 THEN npcdata.speed = 10
   CASE 4
    IF intgrabber(npcdata.textbox, lnpc(state.pt), unpc(state.pt)) THEN
     boxpreview = textbox_preview_line(npcdata.textbox)
    END IF
   CASE 5
    intgrabber(npcdata.facetype, lnpc(state.pt), unpc(state.pt))
   CASE 6
    IF intgrabber(npcdata.item, lnpc(state.pt), unpc(state.pt)) THEN
     itemname = load_item_name(npcdata.item, 0, 0)
    END IF
   CASE 7
    intgrabber(npcdata.pushtype, lnpc(state.pt), unpc(state.pt))
   CASE 8
    intgrabber(npcdata.activation, lnpc(state.pt), unpc(state.pt))
   CASE 9'--tag conditionals
    tag_grabber npcdata.tag1
   CASE 10'--tag conditionals
    tag_grabber npcdata.tag2
   CASE 11'--one-time-use tag
    IF keyval(75) > 1 OR keyval(77) > 1 OR enter_or_space() THEN
     onetimetog npcdata.usetag
    END IF
   CASE 12'--script
    IF enter_or_space() THEN
     scrname = scriptbrowse$(npcdata.script, plottrigger, "NPC use plotscript")
    ELSEIF scrintgrabber(npcdata.script, 0, 0, 75, 77, 1, plottrigger) THEN
     scrname = scriptname$(npcdata.script, plottrigger)
    END IF
   CASE 13
    intgrabber(npcdata.scriptarg, lnpc(state.pt), unpc(state.pt))
   CASE 14
    IF intgrabber(npcdata.vehicle, lnpc(state.pt), unpc(state.pt)) THEN
     vehiclename = load_vehicle_name(npcdata.vehicle - 1)
    END IF
   CASE -1' previous menu
    IF enter_or_space() THEN EXIT DO
  END SELECT
  textcolor uilook(uiMenuItem), 0
  IF state.pt = -1 THEN textcolor uilook(uiSelectedItem + tog), 0
  printstr "Previous Menu", 0, 0, dpage
  FOR i = 0 TO 14
   textcolor uilook(uiMenuItem), 0
   IF state.pt = i THEN textcolor uilook(uiSelectedItem + tog), 0
   caption = " " & read_npc_int(npcdata, i)
   SELECT CASE i
    CASE 1
     caption = " " & defaultint$(npcdata.palette)
    CASE 2
     caption = " = " & movetype(npcdata.movetype)
    CASE 3
     caption = " " & npcdata.speed
    CASE 5
     caption = usetype(npcdata.facetype, 1)
    CASE 6
     caption = " " & itemname
    CASE 7
     caption = pushtype(npcdata.pushtype)
    CASE 8
     caption = usetype(npcdata.activation, 0)
    CASE 9
     IF npcdata.tag1 THEN
      caption = " " & ABS(npcdata.tag1) & " = " & onoroff$(npcdata.tag1) & " (" & load_tag_name(ABS(npcdata.tag1)) & ")"
     ELSE
      caption = " 0 (N/A)"
     END IF
    CASE 10
     IF npcdata.tag2 THEN
      caption = " " & ABS(npcdata.tag2) & " = " & onoroff$(npcdata.tag2) & " (" & load_tag_name(ABS(npcdata.tag2)) & ")"
     ELSE
      caption = " 0 (N/A)"
     END IF
    CASE 11
     IF npcdata.usetag THEN caption = " Only Once (tag " & (1000 + npcdata.usetag) & ")" ELSE caption = " Repeatedly"
    CASE 12 'script
     caption = scrname
    CASE 13 'script arg
     IF npcdata.script = 0 THEN caption = " N/A"
    CASE 14 'vehicle
     IF npcdata.vehicle <= 0 THEN
      caption = "No"
     ELSE
      caption = vehiclename
     END IF
   END SELECT
   printstr menucaption(i) + caption, 0, 8 + (8 * i), dpage
  NEXT i
  edgebox 9, 139, 22, 22, uilook(uiDisabledItem), uilook(uiText), dpage
  sprite_draw npcdata.sprite + 4 + (walk \ 2), npcdata.pal, 10, 140, 1, YES, dpage
  appearstring = "Appears if tag " & ABS(npcdata.tag1) & " = " & onoroff$(npcdata.tag1) & " and tag " & ABS(npcdata.tag2) & " = " & onoroff$(npcdata.tag2)
  IF npcdata.tag1 <> 0 AND npcdata.tag2 = 0 THEN appearstring = "Appears if tag " & ABS(npcdata.tag1) & " = " & onoroff$(npcdata.tag1)
  IF npcdata.tag1 = 0 AND npcdata.tag2 <> 0 THEN appearstring = "Appears if tag " & ABS(npcdata.tag2) & " = " & onoroff$(npcdata.tag2)
  IF npcdata.tag1 = 0 AND npcdata.tag2 = 0 THEN appearstring = "Appears all the time"
  textcolor uilook(uiSelectedItem2), 0
  printstr appearstring, 0, 190, dpage
  textcolor uilook(uiSelectedItem2), uiLook(uiHighlight)
  printstr boxpreview, 0, 170, dpage
  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP

 sprite_unload @npcdata.sprite
 palette16_unload @npcdata.pal
END SUB

FUNCTION load_vehicle_name(vehID AS INTEGER) AS STRING
 IF vehID < 0 OR vehID > gen(genMaxVehicle) THEN RETURN ""
 DIM vehname AS STRING
 DIM vehbuffer(40) AS INTEGER
 setpicstuf vehbuffer(), 80, -1
 loadset game & ".veh", vehID, 0
 vehname = STRING(bound(vehbuffer(0) AND 255, 0, 15), 0)
 array2str vehbuffer(), 1, vehname
 RETURN vehname
END FUNCTION

FUNCTION load_item_name (it AS INTEGER, hidden AS INTEGER, offbyone AS INTEGER) AS STRING
 'it - the item number
 'hidden - whether to *not* prefix the item number
 'offbyone - whether it is the item number (1), or the itemnumber + 1 (0)
 IF it <= 0 AND offbyone = NO THEN RETURN "NONE"
 DIM itn AS INTEGER
 IF offbyone THEN itn = it ELSE itn = it - 1
 DIM buf(99) AS INTEGER
 loaditemdata buf(), itn
 DIM result AS STRING
 result = readbadbinstring$(buf(), 0, 8, 0)
 IF hidden = 0 THEN result = itn & " " & result
 RETURN result
END FUNCTION

FUNCTION textbox_preview_line(boxnum AS INTEGER) AS STRING
 IF boxnum <= 0 OR boxnum > gen(genMaxTextBox) THEN RETURN ""
 DIM box AS TextBox
 LoadTextBox box, boxnum
 RETURN textbox_preview_line(box)
END FUNCTION

FUNCTION textbox_preview_line(box AS TextBox) AS STRING
 DIM s AS STRING
 DIM i AS INTEGER
 FOR i = 0 TO 7
  s= TRIM(box.text(i))
  IF LEN(s) > 0 THEN RETURN s 
 NEXT i
 RETURN "" 
END FUNCTION

SUB onetimetog(BYREF tagnum AS INTEGER)
 IF tagnum > 0 THEN
  setbit gen(), 106, tagnum - 1, 0
  tagnum = 0
  EXIT SUB
 END IF
 DIM i AS INTEGER = 0
 DO
  gen(105) = loopvar(gen(105), 0, 999, 1)
  i = i + 1: IF i > 1000 THEN EXIT SUB 'Revisit this later
 LOOP UNTIL readbit(gen(), 106, gen(105)) = 0
 tagnum = gen(105) + 1
 setbit gen(), 106, gen(105), 1
END SUB

FUNCTION pal16browse (BYVAL curpal AS INTEGER, BYVAL picset AS INTEGER, BYVAL picnum AS INTEGER, BYVAL picframes AS INTEGER, BYVAL picw AS INTEGER, BYVAL pich AS INTEGER) AS INTEGER

 DIM sprite(9) AS Frame PTR
 DIM pal16(9) AS Palette16 PTR

 DIM AS INTEGER i, o, j, k
 DIM c AS INTEGER

 DIM state AS MenuState
 state.need_update = YES
 state.pt = large(curpal, 0)
 state.top = curpal - 1
 state.first = -1
 state.size = 9

 clearpage dpage

 '--get last pal
 setpicstuf buffer(), 16, -1
 loadset game + ".pal", 0, 0
 state.last = buffer(1) + 1
 o = 0
 FOR i = state.last TO 0 STEP -1
  loadset game + ".pal", 1 + i, 0
  FOR j = 0 TO 7
   IF buffer(j) <> 0 THEN o = 1: EXIT FOR
  NEXT j
  IF o = 1 THEN EXIT FOR
  state.last = i + 1
 NEXT i

 state.top = bound(state.top, state.first, large(state.last - state.size, state.first))

 setkeys
 DO
  setwait 55
  setkeys
  state.tog = state.tog XOR 1
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "pal16browse"
  IF usemenu(state) THEN state.need_update = YES
  IF intgrabber(state.pt, state.first, state.last, 51, 52) THEN
   state.need_update = YES
  END IF
  IF enter_or_space() THEN
   IF state.pt >= 0 THEN curpal = state.pt
   EXIT DO
  END IF

  IF state.need_update THEN
   state.need_update = NO
   state.top = bound(state.top, state.pt - state.size, state.pt)
   state.top = bound(state.top, state.first, large(state.last - state.size, state.first))
   FOR i = 0 TO 9
    sprite_unload @sprite(i)
    palette16_unload @pal16(i)
    sprite(i) = sprite_load(game & ".pt" & picset, picnum, picframes, picw, pich)
    pal16(i) = palette16_load(game + ".pal", state.top + i, picset, picnum)
   NEXT i
  END IF

  FOR i = 0 TO 9
   textcolor uilook(uiMenuItem), 0
   IF state.top + i = state.pt THEN textcolor uilook(uiSelectedItem + state.tog), 0
   SELECT CASE state.top + i
    CASE IS >= 0
     o = LEN(" " & (state.top + i)) * 8
     IF state.top + i = state.pt THEN
      edgebox o - 1, 1 + i * 20, 114, 18, uilook(uiBackground), uilook(uiMenuitem), dpage
     END IF
     FOR j = 0 TO 15
      IF pal16(i) THEN
       c = pal16(i)->col(j)
       rectangle o + j * 7, 2 + i * 20, 5, 16, c, dpage
      END IF
     NEXT j
     IF state.top + i <> state.pt THEN
      IF pal16(i) THEN
       FOR k = 0 TO picframes - 1
        sprite_draw sprite(i) + k, pal16(i), o + 140 + (k * picw), i * 20 - (pich \ 2 - 10), 1, YES, dpage
       NEXT k
      END IF
     END IF
     printstr "" & (state.top + i), 4, 5 + i * 20, dpage
    CASE ELSE
     printstr "Cancel", 4, 5 + i * 20, dpage
   END SELECT
  NEXT i
  IF state.pt >= 0 THEN '--write current pic on top
   i = state.pt - state.top
   o = LEN(" " & state.pt) * 8
   IF pal16(i) THEN
    FOR k = 0 TO picframes - 1
     sprite_draw sprite(i) + k, pal16(i), o + 130 + (k * picw), i * 20 - (pich \ 2 - 10), 1, YES, dpage
    NEXT k
   END IF
  END IF
 
  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP

 FOR i = 0 TO 9
  sprite_unload @sprite(i)
  palette16_unload @pal16(i)
 NEXT
 RETURN curpal
END FUNCTION

FUNCTION step_estimate(freq AS INTEGER, low AS INTEGER, high AS INTEGER, infix AS STRING="-", suffix AS STRING= "", zero AS STRING="never") AS STRING
 IF freq = 0 THEN RETURN zero
 DIM low_est  AS INTEGER = INT(low / freq)
 DIM high_est AS INTEGER = INT(high / freq)
 RETURN low_est & infix & high_est & suffix
END FUNCTION

FUNCTION speed_estimate(speed AS INTEGER, suffix AS STRING=" seconds", zero AS STRING="infinity") AS STRING
 IF speed = 0 THEN RETURN zero
 DIM ticks AS INTEGER = INT(1000 / speed)
 DIM result AS STRING
 result = STR(INT(ticks * 10 \ 18) / 10)
 'Special case for dumb floating point math freak-outs
 WHILE INSTR(result, ".") AND RIGHT(result, 2) = "99"
  result = LEFT(result, LEN(result) - 1)
 WEND
 RETURN result & suffix
END FUNCTION

SUB load_text_box_portrait (BYREF box AS TextBox, BYREF gfx AS GraphicPair)
 'WARNING: There is another version of this in yetmore.bas
 'If you update this here, make sure to update that one too!
 DIM img_id AS INTEGER = -1
 DIM pal_id AS INTEGER = -1
 DIM her AS HeroDef
 WITH gfx
  IF .sprite THEN sprite_unload @.sprite
  IF .pal    THEN palette16_unload @.pal
  SELECT CASE box.portrait_type
   CASE 1' Fixed ID number
    img_id = box.portrait_id
    pal_id = box.portrait_pal
   CASE 2' Hero by caterpillar
    'In custom, no party exists, so preview using the first hero
    loadherodata @her, 0
    img_id = her.portrait
    pal_id = her.portrait_pal
   CASE 3' Hero by party slot
    'In custom, no party exists, so preview using the first hero
    loadherodata @her, 0
    img_id = her.portrait
    pal_id = her.portrait_pal
  END SELECT
  IF img_id >= 0 THEN
   .sprite = sprite_load(game & ".pt8", img_id, 1, 50, 50)
   .pal    = palette16_load(game & ".pal", pal_id, 8, img_id)
  END IF
 END WITH
END SUB

FUNCTION fixfilename (s AS STRING) AS STRING
 'Makes sure that a string cannot contain any chars unsafe for filenames
 DIM result AS STRING = ""
 DIM ch AS STRING
 DIM ascii AS INTEGER
 DIM i AS INTEGER
 FOR i = 1 TO LEN(s)
  ch = MID(s, i, 1)
  ascii = ASC(ch)
  SELECT CASE ascii
   CASE 32, 48 TO 57, 65 TO 90, 97 TO 122, 95, 126, 45
    result = result & ch
  END SELECT
 NEXT i
 RETURN result
END FUNCTION

FUNCTION inputfilename (query AS STRING, ext AS STRING, default AS STRING="", check_for_existing AS INTEGER=YES) AS STRING
 DIM filename AS STRING = default
 DIM alert AS STRING
 DIM alert_time AS INTEGER = 0
 DIM tog AS INTEGER
 setkeys
 DO
  setwait 55
  setkeys
  tog = tog XOR 1
  IF keyval(1) > 1 THEN RETURN ""
  strgrabber filename, 40
  filename = fixfilename(filename)
  IF keyval(28) > 1 THEN
   filename = TRIM(filename)
   IF check_for_existing AND isfile(filename & ext) AND filename <> "" THEN
    alert = filename & ext & " already exists"
    alert_time = 30
   ELSE
    IF filename <> "" THEN RETURN filename
   END IF
  END IF
  textcolor uilook(uiText), 0
  printstr query, 160 - LEN(query) * 4, 20, dpage
  IF alert_time > 0 THEN printstr alert, 160 - LEN(alert) * 4, 40, dpage: alert_time =- 1
  textcolor uilook(uiSelectedItem + tog), 1
  printstr filename, 160 - LEN(filename & ext) * 4 , 30, dpage
  textcolor uilook(uiText), uilook(uiHighlight)
  printstr ext, 160 + (LEN(filename) - LEN(ext)) * 4 , 30, dpage
  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
END FUNCTION

FUNCTION askwhatmetadata (metadata() AS INTEGER, metadatalabels() AS STRING) AS INTEGER
 DIM tog AS INTEGER
 
 DIM state AS MenuState
 state.size = UBOUND(metadata) + 1
 state.first = -1
 state.last = UBOUND(metadata)
 state.top = -1
 state.pt = -1
 
 setkeys
 DO
  setwait 55
  setkeys
  usemenu state
  tog = tog XOR 1
  IF keyval(scESC) > 1 THEN RETURN NO
  IF keyval(scF1) > 1 THEN show_help "textbox_export_askwhatmetadata"
  
  IF enter_or_space() THEN
   IF state.pt = -1 THEN RETURN YES
   IF metadata(state.pt) = NO THEN metadata(state.pt) = YES ELSE metadata(state.pt) = NO
  END IF
  
  textcolor uilook(uiText), 0
  printstr "Choose what metadata to include:", 4, 4, dpage
  
  IF state.pt <> -1 THEN textcolor uilook(uiText), 0 ELSE textcolor uilook(uiSelectedItem + tog), 1
  printstr "Done", 4, 4 + 9, dpage
  FOR i AS INTEGER = 0 TO UBOUND(metadatalabels)
   IF state.pt = i THEN
    IF metadata(i) = YES THEN textcolor uilook(uiSelectedItem + tog), 1 ELSE textcolor uilook(uiSelectedDisabled), 1
   ELSE
    IF metadata(i) = YES THEN textcolor uilook(uiText), 0 ELSE textcolor uilook(uiDisabledItem), 0
   END IF
   printstr metadatalabels(i), 4, 4 + 18 + i * 9, dpage
  NEXT
  
  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
END FUNCTION

FUNCTION export_textboxes (filename AS STRING, metadata() AS INTEGER) AS INTEGER
 DIM fh AS INTEGER = FREEFILE
 IF OPEN(filename FOR OUTPUT AS #fh) THEN debug "export_textboxes: Failed to open " & filename : RETURN NO
 DIM box AS TextBox
 DIM blank AS INTEGER
 DIM AS INTEGER i, j, k
 FOR i = 0 TO gen(genMaxTextBox)
  LoadTextBox box, i
  '--Write the header guide
  PRINT #fh, "======================================"
  '--Write the box number and metadata
  PRINT #fh, "Box " & i
    
  IF metadata(1) THEN '--box conditionals
   IF box.instead_tag <> 0 THEN
    PRINT #fh, "Instead Tag: " & box.instead_tag & " (" & escape_nonprintable_ascii(tag_condition_caption(box.instead_tag, , "Impossible", "Never", "Always")) & ")"
    PRINT #fh, "Instead Box: " & box.instead;
    IF box.instead < 0 THEN
     PRINT #fh, " (Plotscript " & scriptname$(box.instead * -1, plottrigger) & ")"
    ELSE
     PRINT #fh, " (Textbox)"
    END IF
   END IF
   IF box.after_tag <> 0 THEN
    PRINT #fh, "Next Tag: " & box.after_tag & " (" & escape_nonprintable_ascii(tag_condition_caption(box.after_tag, , "Impossible", "Never", "Always")) & ")"
    PRINT #fh, "Next Box: " & box.after;
    IF box.after < 0 THEN
     PRINT #fh, " (Plotscript " & scriptname$(box.after * -1, plottrigger) & ")"
    ELSE
     PRINT #fh, " (Textbox)"
    END IF
   END IF
   
   IF box.settag_tag <> 0 THEN
    PRINT #fh, "Set Tag: " & box.settag_tag & " (" & escape_nonprintable_ascii(tag_condition_caption(box.settag_tag, , "Impossible", "Never", "Always")) & ")"
    IF box.settag1 <> 0 THEN PRINT #fh, "Set Tag 1: " & box.settag1 & " (" & escape_nonprintable_ascii(tag_condition_caption(box.settag1, , "Impossible", "Never", "Always")) & ")"
    IF box.settag2 <> 0 THEN PRINT #fh, "Set Tag 2: " & box.settag2 & " (" & escape_nonprintable_ascii(tag_condition_caption(box.settag2, , "Impossible", "Never", "Always")) & ")"
   END IF
   IF box.battle_tag <> 0 THEN
    PRINT #fh, "Battle Tag: " & box.battle_tag & " (" & escape_nonprintable_ascii(tag_condition_caption(box.battle_tag, , "Impossible", "Never", "Always")) & ")"
    PRINT #fh, "Battle: " & box.battle
   END IF
   IF box.shop_tag <> 0 THEN
    PRINT #fh, "Shop Tag: " & box.shop_tag & " (" & escape_nonprintable_ascii(tag_condition_caption(box.shop_tag, , "Impossible", "Never", "Always")) & ")"
    PRINT #fh, "Shop: " & box.shop;
    if(box.shop = 0) THEN PRINT #fh, " (Restore HP/MP)"
    if(box.shop < 0) THEN PRINT #fh, " (Inn for $" & (box.shop * -1) & ")"
    if(box.shop > 0) THEN PRINT #fh, " (" & escape_nonprintable_ascii(readshopname$(box.shop - 1)) & ")"
   END IF
   IF box.hero_tag <> 0 THEN
    PRINT #fh, "Hero Tag: " & box.hero_tag & " (" & escape_nonprintable_ascii(tag_condition_caption(box.hero_tag, , "Impossible", "Never", "Always")) & ")"
    
    IF box.hero_addrem <> 0 THEN
     PRINT #fh, "Hero Add: " & box.hero_addrem;
     IF box.hero_addrem < 0 THEN
      PRINT #fh, " (Remove " & escape_nonprintable_ascii(getheroname((box.hero_addrem * -1) - 1)) & ")"
     ELSE
      PRINT #fh, " (Add " & escape_nonprintable_ascii(getheroname(box.hero_addrem - 1)) & ")"
     END IF
    END IF
    
    IF box.hero_swap <> 0 THEN
     PRINT #fh, "Hero Swap: " & box.hero_swap;
     IF box.hero_swap < 0 THEN
      PRINT #fh, " (Swap Out " & escape_nonprintable_ascii(getheroname((box.hero_swap * -1) - 1)) & ")"
     ELSE
      PRINT #fh, " (Swap In " & escape_nonprintable_ascii(getheroname(box.hero_swap - 1)) & ")"
     END IF
    END IF
    
    IF box.hero_lock <> 0 THEN
     PRINT #fh, "Hero Lock: " & box.hero_lock;
     IF box.hero_lock < 0 THEN
      PRINT #fh, " (Lock " & escape_nonprintable_ascii(getheroname((box.hero_lock * -1) - 1)) & ")"
     ELSE
      PRINT #fh, " (Unlock " & escape_nonprintable_ascii(getheroname(box.hero_lock - 1)) & ")"
     END IF
    END IF
    
   END IF
   
   IF box.money_tag <> 0 THEN
    PRINT #fh, "Money Tag: " & box.money_tag & " (" & escape_nonprintable_ascii(tag_condition_caption(box.money_tag, , "Impossible", "Never", "Always")) & ")"
    PRINT #fh, "Money: " & box.money
   END IF
   
   IF box.door_tag <> 0 THEN
    PRINT #fh, "Door Tag: " & box.door_tag & " (" & escape_nonprintable_ascii(tag_condition_caption(box.door_tag, , "Impossible", "Never", "Always")) & ")"
    PRINT #fh, "Door: " & box.door
   END IF
   
   IF box.item_tag <> 0 THEN
    PRINT #fh, "Item Tag: " & box.item_tag & " (" & escape_nonprintable_ascii(tag_condition_caption(box.item_tag, , "Impossible", "Never", "Always")) & ")"
    PRINT #fh, "Item: " & box.item;
    IF box.item < 0 THEN
     PRINT #fh, " (Remove " & escape_nonprintable_ascii(readitemname$((box.item * -1) - 1)) & ")"
    ELSE
     PRINT #fh, " (Add " & escape_nonprintable_ascii(readitemname$(box.item - 1)) & ")"
    END IF
   END IF
  END IF
  
  IF box.menu_tag <> 0 THEN
    PRINT #fh, "Menu Tag: " & box.menu_tag & " (" & escape_nonprintable_ascii(tag_condition_caption(box.menu_tag, , "Impossible", "Never", "Always")) & ")"
    PRINT #fh, "Menu: " & box.menu
   END IF
   
  IF metadata(2) THEN '--choices
   IF box.choice_enabled THEN
    PRINT #fh, "Choice Enabled: YES"
    PRINT #fh, "Choice 1: " & escape_nonprintable_ascii(box.choice(0))
    PRINT #fh, "Choice 1 Tag: " & box.choice_tag(0) & " (" & escape_nonprintable_ascii(tag_condition_caption(box.choice_tag(0), , "Do Nothing", "Never", "Always")) & ")"
    PRINT #fh, "Choice 2: " & escape_nonprintable_ascii(box.choice(1))
    PRINT #fh, "Choice 2 Tag: " & box.choice_tag(1) & " (" & escape_nonprintable_ascii(tag_condition_caption(box.choice_tag(1), , "Do Nothing", "Never", "Always")) & ")"
    
   END IF
  END IF
  
  IF metadata(3) THEN '--box appearance
   IF box.shrink = -1 THEN
    PRINT #fh, "Size: auto"
   ELSE
    PRINT #fh, "Size: " & (21 - box.shrink)
   END IF
   PRINT #fh, "Position: " & box.vertical_offset
   PRINT #fh, "Text Color: " & box.textcolor '--AARGH.
   PRINT #fh, "Border Color: " & box.boxstyle '--AARGH AGAIN.
   PRINT #fh, "Backdrop: " & box.backdrop
   IF box.music > 0 THEN
    PRINT #fh, "Music: " & box.music & " (" & escape_nonprintable_ascii(getsongname$(box.music - 1)) & ")"
   ELSE
    PRINT #fh, "Music: " & box.music & " (None)"
   END IF
   PRINT #fh, "Restore Music: " & yesorno(box.restore_music)
   PRINT #fh, "Show Box: " & yesorno(NOT box.no_box) '--argh, double negatives
   PRINT #fh, "Translucent: " & yesorno(NOT box.opaque) '--  "       "      "
   
   IF box.portrait_box <> NO OR box.portrait_type <> 0 THEN
    PRINT #fh, "Portrait Box: " & yesorno(box.portrait_box)
   END IF
   IF box.portrait_type <> 0 THEN
    PRINT #fh, "Portrait Type: " & box.portrait_type
    PRINT #fh, "Portrait ID: " & box.portrait_id
    IF box.portrait_pal <> -1 THEN PRINT #fh, "Portrait Palette: " & box.portrait_pal
    PRINT #fh, "Portrait X: " & box.portrait_pos.X
    PRINT #fh, "Portrait Y: " & box.portrait_pos.Y
   END IF
  END IF
  
  
  
  IF metadata(0) THEN '--box text
   '--Write the separator
   PRINT #fh, "--------------------------------------"
   blank = 0
   FOR j = 0 TO 7
    IF box.text(j) = "" THEN
     blank += 1
    ELSE
     FOR k = 1 TO blank
      PRINT #fh, ""
     NEXT k
     blank = 0
     PRINT #fh, escape_nonprintable_ascii(box.text(j))
    END IF
   NEXT j
  END IF
 NEXT i
 CLOSE #fh
 RETURN YES
END FUNCTION

SUB import_textboxes_warn (BYREF warn AS STRING, s AS STRING)
 debug "import_textboxes: " & s
 IF warn <> "" THEN warn = warn & " "
 warn = warn & s
END SUB

FUNCTION import_textboxes (filename AS STRING, BYREF warn AS STRING) AS INTEGER
 DIM fh AS INTEGER = FREEFILE
 IF OPEN(filename FOR INPUT AS #fh) THEN
  import_textboxes_warn warn, "Failed to open """ & filename & """."
  RETURN NO
 END IF
 DIM warn_length AS INTEGER = 0
 DIM warn_skip AS INTEGER = 0
 DIM warn_append AS INTEGER = 0
 DIM box AS TextBox
 DIM index AS INTEGER = 0
 DIM getindex AS INTEGER = 0 
 DIM mode AS INTEGER = 0
 DIM s AS STRING
 DIM firstline AS INTEGER = YES
 DIM line_number AS INTEGER = 0
 DIM boxlines AS INTEGER = 0
 DIM i AS INTEGER
 DO WHILE NOT EOF(fh)
  line_number += 1
  LINE INPUT #1, s
  s = decode_backslash_codes(s)
  IF firstline THEN
   IF s <> STRING(38, "=") THEN
    import_textboxes_warn warn, filename & " is not a valid text box file. Expected header row, found """ & s & """."
    CLOSE #fh
    RETURN NO
   END IF
   firstline = NO
   CONTINUE DO
  END IF
  SELECT CASE mode
   CASE 0 '--Seek box number
    IF LEFT(s, 4) = "Box " THEN
     getindex = VALINT(MID(s, 5))
     IF getindex > index THEN
      warn_skip += 1
      debug "import_textboxes: line " & line_number & ": box ID " & index & " is not in the txt file"
     END IF
     IF getindex < index THEN
      debug "import_textboxes: line " & line_number & ": box ID numbers out-of-order. Expected " & index & ", but found " & getindex
     END IF
     index = getindex
     LoadTextBox box, index
     boxlines = 0
     mode = 1
    ELSE
     import_textboxes_warn warn, "line " & line_number & ": expected Box # but found """ & s & """."
     CLOSE #fh
     RETURN NO
    END IF
   CASE 1 '--Seek divider
    IF s = STRING(38, "-") THEN
     mode = 2
    ELSEIF s = STRING(38, "=") THEN '--no text
     IF index > gen(genMaxTextbox) THEN
      warn_append += index - gen(genMaxTextbox)
      gen(genMaxTextbox) = index
     END IF
     SaveTextBox box, index
     index += 1
     mode = 0
     boxlines = 0
    ELSE
     IF INSTR(s, ":") THEN '--metadata, probably
      dim t as string, v as string
      t = LCASE(LEFT(s, instr(s, ":") - 1))
      v = TRIM(MID(s, instr(s, ":") + 1))
      SELECT CASE t
       CASE "size"
        IF LCASE(v) = "auto" THEN
         box.shrink = -1
        ELSEIF VALINT(v) > 21 THEN
         debug "Box size too large, capping"
         box.shrink = 0
        ELSE
         box.shrink = 21 - VALINT(v)
        END IF
       CASE "portrait box"
        box.portrait_box = str2bool(v, NO)
       CASE "portrait type"
        box.portrait_type = VALINT(v)
       CASE "portrait id"
        box.portrait_id = VALINT(v)
       CASE "portrait x"
        box.portrait_pos.x = VALINT(v)
       CASE "portrait y"
        box.portrait_pos.y = VALINT(v)
       CASE "portrait palette"
        box.portrait_pal = VALINT(v)
       CASE "instead tag"
        box.instead_tag = VALINT(v)
       CASE "instead box"
        box.instead = VALINT(v)
       CASE "set tag"
        box.settag_tag = VALINT(v)
       CASE "set tag 1"
        box.settag1 = VALINT(v)
       CASE "set tag 2"
        box.settag2 = VALINT(v)
       CASE "battle tag"
        box.battle_tag = VALINT(v)
       CASE "battle"
        box.battle = VALINT(v)
       CASE "shop tag"
        box.shop_tag = VALINT(v)
       CASE "shop"
        box.shop = VALINT(v)
       CASE "item tag"
        box.item_tag = VALINT(v)
       CASE "item"
        box.item = VALINT(v)
       CASE "money tag"
        box.money_tag = VALINT(v)
       CASE "money"
        box.money = VALINT(v)
       CASE "door tag"
        box.door_tag = VALINT(v)
       CASE "door"
        box.door = VALINT(v)
       CASE "hero tag"
        box.hero_tag = VALINT(v)
       CASE "hero add"
        box.hero_addrem = VALINT(v)
       CASE "hero swap"
        box.hero_swap = VALINT(v)
       CASE "hero lock"
        box.hero_lock = VALINT(v)
       CASE "menu tag"
        box.menu_tag = VALINT(v)
       CASE "menu"
        box.menu_tag = VALINT(v)
       CASE "next tag"
        box.after_tag = VALINT(v)
       CASE "next box"
        box.after = VALINT(v)
       CASE "choice enabled"
        box.choice_enabled = str2bool(v)
       CASE "choice 1"
        box.choice(0) = TRIM(v)
       CASE "choice 2"
        box.choice(1) = TRIM(v)
       CASE "choice 1 tag"
        box.choice_tag(0) = VALINT(v)
       CASE "choice 2 tag"
        box.choice_tag(1) = VALINT(v)
       CASE "position"
        box.vertical_offset = VALINT(v)
       CASE "text color"
        box.textcolor = VALINT(v)
       CASE "border color"
        box.boxstyle = VALINT(v)
       CASE "backdrop"
        box.backdrop = VALINT(v)
       CASE "music"
        box.music = VALINT(v)
       CASE "restore music"
        box.restore_music = str2bool(v)
       CASE "show box"
        box.no_box = str2bool(v,,YES)
       CASE "translucent"
        box.opaque = str2bool(v,,YES)
        
       CASE ELSE
        import_textboxes_warn warn, "line " & line_number & ": expected divider line but found """ & s & """."
        CLOSE #fh
        RETURN NO
      END SELECT
     END IF
    END IF
   CASE 2 '--Text lines
    IF s = STRING(38, "=") THEN
     FOR i = boxlines TO 7
      box.text(i) = ""
     NEXT i
     IF index > gen(genMaxTextbox) THEN
      warn_append += index - gen(genMaxTextbox)
      gen(genMaxTextbox) = index
     END IF
     SaveTextBox box, index
     index += 1
     boxlines = 0
     mode = 0
    ELSE
     IF boxlines >= 8 THEN
      import_textboxes_warn warn, "line " & line_number & ": too many lines in box " & index & ". Overflowed with """ & s & """."
      CLOSE #fh
      RETURN NO
     END IF
     IF LEN(s) > 38 THEN '--this should be down here
      warn_length += 1
      debug "import_textboxes: line " & line_number & ": line too long (" & LEN(s) & ")"
      s = LEFT(s, 38)
     END IF
     box.text(boxlines) = s
     boxlines += 1
    END IF
  END SELECT
 LOOP
 IF mode = 2 THEN'--Save the last box
  FOR i = boxlines TO 7
   box.text(i) = ""
  NEXT i
  IF index > gen(genMaxTextbox) THEN
   warn_append += index - gen(genMaxTextbox)
   gen(genMaxTextbox) = index
  END IF
  SaveTextBox box, index
 ELSEIF mode = 0 THEN '--this... is not good
  import_textboxes_warn warn, "line " & line_number & ": txt file ended unexpectedly."
  CLOSE #fh
  RETURN NO
 END IF
 IF warn_length > 0 THEN import_textboxes_warn warn, warn_length & " lines were too long."
 IF warn_skip > 0   THEN import_textboxes_warn warn, warn_skip & " box ID numbers were not in the txt file."
 IF warn_append > 0 THEN import_textboxes_warn warn, warn_append & " new boxes were appended."
 CLOSE #fh
 RETURN YES
END FUNCTION

FUNCTION str2bool(q AS STRING, default AS INTEGER = NO, invert AS INTEGER = NO) AS INTEGER
 IF LCASE(LEFT(TRIM(q), 3)) = "yes" THEN
  IF invert THEN RETURN NO ELSE RETURN YES
 END IF
 IF LCASE(LEFT(TRIM(q), 2)) = "no" THEN
  IF invert THEN RETURN YES ELSE RETURN NO
 END IF
 RETURN default
END FUNCTION

SUB xy_position_on_sprite (spr AS GraphicPair, BYREF x AS INTEGER, BYREF y AS INTEGER, BYVAL frame AS INTEGER, BYVAL wide AS INTEGER, byval high AS INTEGER, caption AS STRING)
 DIM col AS INTEGER
 DIM tog AS INTEGER
 setkeys
 DO
  setwait 55
  setkeys
  tog = tog XOR 1

  IF keyval(scEsc) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "xy_position_on_sprite"
  IF enter_or_space() THEN EXIT DO
  IF keyval(scLeft) > 0  THEN x -= 1
  IF keyval(scRight) > 0 THEN x += 1
  IF keyval(scUp) > 0    THEN y -= 1
  IF keyval(scDown) > 0  THEN y += 1

  emptybox 160 - wide, 100 - high, wide * 2, high * 2, uilook(uiSelectedDisabled), 1, dpage
  sprite_draw spr.sprite + frame, spr.pal, 160 - wide, 100 - high, 2,, dpage
  col = uilook(uiBackground)
  IF tog = 0 THEN col = uilook(uiSelectedItem)
  rectangle 160 - wide + x * 2 - 2, 100 - high + y * 2, 2, 2, col, dpage
  rectangle 160 - wide + x * 2 + 2, 100 - high + y * 2, 2, 2, col, dpage
  rectangle 160 - wide + x * 2, 100 - high + y * 2 - 2, 2, 2, col, dpage
  rectangle 160 - wide + x * 2, 100 - high + y * 2 + 2, 2, 2, col, dpage

  edgeprint caption, xstring(caption, 160), 0, uilook(uiText), dpage
  edgeprint "Position point and press Enter or SPACE", 0, 190, uilook(uiText), dpage

  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
END SUB

SUB edit_menu_bits (menu AS MenuDef)
 DIM bitname(8) AS STRING
 DIM bits(0) AS INTEGER
 
 bitname(0) = "Translucent box"
 bitname(1) = "Never show scrollbar"
 bitname(2) = "Allow gameplay & scripts"
 bitname(3) = "Suspend player even if gameplay allowed"
 bitname(4) = "No box"
 bitname(5) = "Cancel button doesn't close menu"
 bitname(6) = "No player control of menu"
 bitname(7) = "Prevent main menu activation"
 bitname(8) = "Advance text box when menu closes"

 MenuBitsToArray menu, bits()
 editbitset bits(), 0, UBOUND(bitname), bitname()
 MenuBitsFromArray menu, bits()  
END SUB

SUB edit_menu_item_bits (mi AS MenuDefItem)
 DIM bitname(1) AS STRING
 DIM bits(0) AS INTEGER
 
 bitname(0) = "Hide if disabled"
 bitname(1) = "Close menu if selected"

 MenuItemBitsToArray mi, bits()
 editbitset bits(), 0, UBOUND(bitname), bitname()
 MenuItemBitsFromArray mi, bits()  
END SUB

SUB reposition_menu (menu AS MenuDef, mstate AS MenuState)
 DIM shift AS INTEGER

 setkeys
 DO
  setwait 55
  setkeys
 
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "reposition_menu"
  
  shift = ABS(keyval(42) > 0 OR keyval(54) > 0)
  WITH menu.offset
   IF keyval(72) > 1 THEN .y -= 1 + 9 * shift
   IF keyval(80) > 1 THEN .y += 1 + 9 * shift
   IF keyval(75) > 1 THEN .x -= 1 + 9 * shift
   IF keyval(77) > 1 THEN .x += 1 + 9 * shift
  END WITH
 
  draw_menu menu, mstate, dpage
  edgeprint "Offset=" & menu.offset.x & "," & menu.offset.y, 0, 0, uilook(uiDisabledItem), dpage
  edgeprint "Arrows to re-position, ESC to exit", 0, 191, uilook(uiDisabledItem), dpage
  
  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
END SUB

SUB reposition_anchor (menu AS MenuDef, mstate AS MenuState)
 DIM tog AS INTEGER = 0
 DIM x AS INTEGER
 DIM y AS INTEGER
 setkeys
 DO
  setwait 55
  setkeys
  tog = tog XOR 1
 
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "reposition_anchor"
  
  WITH menu.anchor
   IF keyval(72) > 1 THEN .y = bound(.y - 1, -1, 1)
   IF keyval(80) > 1 THEN .y = bound(.y + 1, -1, 1)
   IF keyval(75) > 1 THEN .x = bound(.x - 1, -1, 1)
   IF keyval(77) > 1 THEN .x = bound(.x + 1, -1, 1)
  END WITH
 
  draw_menu menu, mstate, dpage
  WITH menu
   x = .rect.x - 2 + anchor_point(.anchor.x, .rect.wide)
   y = .rect.y - 2 + anchor_point(.anchor.y, .rect.high)
   edgebox x, y, 5, 5, 2 + tog, dpage, NO 
  END WITH
  edgeprint "Arrows to re-position, ESC to exit", 0, 191, uilook(uiDisabledItem), dpage
  
  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
END SUB

FUNCTION tag_toggle_caption(n AS INTEGER, prefix AS STRING="Toggle tag") AS STRING
 DIM s AS STRING
 s = prefix
 IF LEN(s) > 0 THEN s = s & " "
 s = s & ABS(n)
 SELECT CASE n
  CASE 0: s = s & " (N/A)"
  CASE 1, -1: s = s & " (unchangeable)"
  CASE IS > 1: s = s & " (" & load_tag_name(n) & ")"
 END SELECT
 RETURN s
END FUNCTION

SUB editbitset (array() AS INTEGER, BYVAL wof AS INTEGER, BYVAL last AS INTEGER, names() AS STRING)

 '---DIM AND INIT---
 DIM state AS MenuState
 WITH state
  .pt = -1
  .top = -1
  .first = -1
  .last = last
  .size = 24
 END WITH

 DIM menu(-1 to last) AS STRING
 DIM bits(-1 to last) AS INTEGER

 menu(-1) = "Previous Menu"

 DIM nextbit AS INTEGER = 0
 FOR i AS INTEGER = 0 to last
  IF names(i) <> "" THEN
   menu(nextbit) = names(i)
   bits(nextbit) = i
   nextbit += 1
  END IF
 NEXT
 state.last = nextbit - 1

 DIM col AS INTEGER

 '---MAIN LOOP---
 setkeys
 DO
  setwait 55
  setkeys
  state.tog = state.tog XOR 1
  IF keyval(scEsc) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "editbitset"
  usemenu state
  IF state.pt >= 0 THEN
   IF keyval(scLeft) > 1 OR keyval(scComma) > 1 THEN setbit array(), wof, bits(state.pt), 0
   IF keyval(scRight) > 1 OR keyval(scPeriod) > 1 THEN setbit array(), wof, bits(state.pt), 1
   IF enter_or_space() THEN setbit array(), wof, bits(state.pt), readbit(array(), wof, bits(state.pt)) XOR 1
  ELSE
   IF enter_or_space() THEN EXIT DO
  END IF
  draw_fullscreen_scrollbar state, , dpage
  FOR i AS INTEGER = state.top TO small(state.top + state.size, state.last)
   IF i >= 0 THEN
    col = IIF(readbit(array(), wof, bits(i)), uilook(uiMenuItem), uilook(uiDisabledItem))
    IF state.pt = i THEN col = IIF(readbit(array(), wof, bits(i)), uilook(uiSelectedItem + state.tog), uilook(uiSelectedDisabled + state.tog))
   ELSE
    col = uilook(uiMenuItem)
    IF state.pt = i THEN col = uilook(uiSelectedItem + state.tog)
   END IF
   textcolor col, 0
   printstr menu(i), 8, (i - state.top) * 8, dpage
  NEXT i
  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
END SUB

FUNCTION scriptbrowse (BYREF trigger AS INTEGER, BYVAL triggertype AS INTEGER, scrtype AS STRING) AS STRING
 DIM localbuf(20)
 REDIM scriptnames(0) AS STRING, scriptids(0)
 DIM numberedlast AS INTEGER = 0
 DIM firstscript AS INTEGER = 0
 DIM scriptmax AS INTEGER = 0
 
 DIM chara AS INTEGER
 DIM charb AS INTEGER
 
 DIM fh AS INTEGER
 DIM i AS INTEGER
 DIM j AS INTEGER

 DIM tempstr AS STRING
  tempstr = scriptname(trigger, triggertype)
 IF tempstr <> "[none]" AND LEFT$(tempstr, 1) = "[" THEN firstscript = 2 ELSE firstscript = 1

 IF triggertype = 1 THEN
  'plotscripts
  fh = FREEFILE
  OPEN workingdir + SLASH + "plotscr.lst" FOR BINARY AS #fh
  'numberedlast = firstscript + LOF(fh) \ 40 - 1
  numberedlast = firstscript + gen(40) - 1

  REDIM scriptnames(numberedlast) AS STRING, scriptids(numberedlast)

  i = firstscript
  FOR j AS INTEGER = firstscript TO numberedlast
   loadrecord localbuf(), fh, 20
   IF localbuf(0) < 16384 THEN
    scriptids(i) = localbuf(0)
    scriptnames(i) = STR$(localbuf(0)) + " " + readbinstring(localbuf(), 1, 36)
    i += 1
   END IF
  NEXT
  numberedlast = i - 1

  CLOSE #fh
 END IF

 fh = FREEFILE
 OPEN workingdir + SLASH + "lookup" + STR$(triggertype) + ".bin" FOR BINARY AS #fh
 scriptmax = numberedlast + LOF(fh) \ 40

 IF scriptmax < firstscript THEN
  RETURN "[no scripts]"
 END IF

 ' 0 to firstscript - 1 are special options (none, current script)
 ' firstscript to numberedlast are oldstyle numbered scripts
 ' numberedlast + 1 to scriptmax are newstyle trigger scripts
 REDIM PRESERVE scriptnames(scriptmax), scriptids(scriptmax)
 scriptnames(0) = "[none]"
 scriptids(0) = 0
 IF firstscript = 2 THEN
  scriptnames(1) = tempstr
  scriptids(1) = trigger
 END IF

 i = numberedlast + 1
 FOR j AS INTEGER = numberedlast + 1 TO scriptmax
  loadrecord localbuf(), fh, 20
  IF localbuf(0) <> 0 THEN
   scriptids(i) = 16384 + j - (numberedlast + 1)
   scriptnames(i) = readbinstring(localbuf(), 1, 36)
   i += 1
  END IF
 NEXT
 scriptmax = i - 1

 CLOSE #fh

 'insertion sort numbered scripts by id
 FOR i = firstscript + 1 TO numberedlast
  FOR j AS INTEGER = i - 1 TO firstscript STEP -1
   IF scriptids(j + 1) < scriptids(j) THEN
    SWAP scriptids(j + 1), scriptids(j)
    SWAP scriptnames(j + 1), scriptnames(j)
   ELSE
    EXIT FOR
   END IF
  NEXT
 NEXT

 'sort trigger scripts by name
 FOR i = numberedlast + 1 TO scriptmax - 1
  FOR j AS INTEGER = scriptmax TO i + 1 STEP -1
   FOR k AS INTEGER = 0 TO small(LEN(scriptnames(i)), LEN(scriptnames(j)))
    chara = ASC(LCASE$(CHR$(scriptnames(i)[k])))
    charb = ASC(LCASE$(CHR$(scriptnames(j)[k])))
    IF chara < charb THEN
     EXIT FOR
    ELSEIF chara > charb THEN
     SWAP scriptids(i), scriptids(j)
     SWAP scriptnames(i), scriptnames(j)
     EXIT FOR
     END IF
   NEXT
  NEXT
 NEXT

 DIM state AS MenuState
 WITH state
  .pt = 0
  .last = scriptmax
  .size = 22
 END WITH

 IF firstscript = 2 THEN
  state.pt = 1
 ELSE
  FOR i = 1 TO scriptmax
   IF trigger = scriptids(i) THEN state.pt = i: EXIT FOR
  NEXT
 END IF
 state.top = large(0, small(state.pt - 10, scriptmax - 21))
 DIM id AS INTEGER = scriptids(state.pt)
 DIM iddisplay AS INTEGER = 0
 clearpage 0
 clearpage 1
 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(scESC) > 1 THEN
   RETURN tempstr
  END IF
  IF keyval(scF1) > 1 THEN show_help "scriptbrowse"
  IF enter_or_space() THEN EXIT DO
  IF scriptids(state.pt) < 16384 THEN
   IF intgrabber(id, 0, 16383) THEN
    iddisplay = -1
    FOR i = 0 TO numberedlast
     IF id = scriptids(i) THEN state.pt = i
    NEXT
   END IF
  END IF
  IF usemenu(state) THEN
   IF scriptids(state.pt) < 16384 THEN
    id = scriptids(state.pt)
   ELSE
    id = 0
    iddisplay = 0
   END IF
  END IF
  FOR i = 12 TO 53
   IF keyval(i) > 1 AND keyv(i, 0) > 0 THEN
    j = state.pt + 1
    FOR ctr AS INTEGER = numberedlast + 1 TO scriptmax
     IF j > scriptmax THEN j = numberedlast + 1
     tempstr$ = LCASE$(scriptnames(j))
     IF tempstr$[0] = keyv(i, 0) THEN state.pt = j: EXIT FOR
     j += 1
    NEXT
    EXIT FOR
   END IF
  NEXT i

  draw_fullscreen_scrollbar state, , dpage
  textcolor uilook(uiText), 0
  printstr "Pick a " + scrtype$, 0, 0, dpage
  standardmenu scriptnames(), state, 8, 10, dpage, 0
  IF iddisplay THEN
   textcolor uilook(uiMenuItem), uilook(uiHighlight)
   printstr STR$(id), 8, 190, dpage
  END IF

  SWAP dpage, vpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
 clearpage 0
 clearpage 1

 trigger = scriptids(state.pt)
 IF scriptids(state.pt) < 16384 THEN
  RETURN MID(scriptnames(state.pt), INSTR(scriptnames(state.pt), " ") + 1)
 ELSE
  RETURN scriptnames(state.pt)
 END IF

END FUNCTION

FUNCTION scrintgrabber (BYREF n AS INTEGER, BYVAL min AS INTEGER, BYVAL max AS INTEGER, BYVAL less AS INTEGER=75, BYVAL more AS INTEGER=77, BYVAL scriptside AS INTEGER, BYVAL triggertype AS INTEGER) AS INTEGER
 'script side is 1 or -1: on which side of zero are the scripts
 'min or max on side of scripts is ignored

 DIM temp AS INTEGER = n
 IF scriptside < 0 THEN
  temp = -n
  SWAP less, more
  min = -min
  max = -max
  SWAP min, max
 END IF

 DIM seekdir AS INTEGER = 0
 IF keyval(more) > 1 THEN
  seekdir = 1
 ELSEIF keyval(less) > 1 THEN
  seekdir = -1
 END IF

 DIM scriptscroll AS INTEGER = NO
 IF seekdir <> 0 THEN
  scriptscroll = NO
  IF temp = min AND seekdir = -1 THEN
   temp = -1
   scriptscroll = YES
  ELSEIF (temp = 0 AND seekdir = 1) OR temp > 0 THEN
   scriptscroll = YES
  END IF
  IF scriptscroll THEN
   'scroll through scripts
   seekscript temp, seekdir, triggertype
   IF temp = -1 THEN temp = min
  ELSE
   'regular scroll
   temp += seekdir
  END IF
 ELSE
  IF (temp > 0 AND temp < 16384) OR (temp = 0 AND scriptside = 1) THEN
   'if a number is entered, don't seek to the next script, allow "[id]" to display instead
   IF intgrabber(temp, 0, 16383, 0, 0) THEN
    'if temp starts off greater than gen(genMaxRegularScript) then don't disturb it
    temp = small(temp, gen(genMaxRegularScript))
   END IF
  ELSEIF temp < 0 OR (temp = 0 AND scriptside = -1) THEN
   intgrabber(temp, min, 0, 0, 0)
  END IF
 END IF

 IF keyval(scDelete) > 1 THEN temp = 0
 IF keyval(scMinus) > 1 OR keyval(scNumpadMinus) > 1 THEN temp = bound(-temp, min, gen(genMaxRegularScript))

 temp = temp * SGN(scriptside)
 scrintgrabber = (temp <> n) ' Returns true if BYREF n has changed
 n = temp
END FUNCTION

SUB seekscript (BYREF temp AS INTEGER, BYVAL seekdir AS INTEGER, BYVAL triggertype AS INTEGER)
 'temp = -1 means scroll to last script
 'returns 0 when scrolled past first script, -1 when went past last

 DIM buf(19), plotids(gen(43))
 DIM recordsloaded AS INTEGER = 0
 DIM screxists AS INTEGER = 0

 DIM fh AS INTEGER = FREEFILE
 OPEN workingdir & SLASH & "lookup" & triggertype & ".bin" FOR BINARY AS #fh
 DIM triggernum AS INTEGER = LOF(fh) \ 40
 IF temp = -1 THEN temp = triggernum + 16384

 DO
  temp += seekdir
  IF temp > gen(43) AND temp < 16384 THEN
   IF seekdir > 0 THEN
    temp = 16384
   ELSEIF triggertype = plottrigger THEN
    temp = gen(43)
   ELSE
    temp = 0
   END IF
  END IF
  IF temp <= 0 THEN EXIT DO
  IF temp >= triggernum + 16384 THEN
   temp = -1
   EXIT DO
  END IF
  'check script exists, else keep looking
  IF temp < 16384 AND triggertype = plottrigger THEN
   IF plotids(temp) THEN
    screxists = -1
   ELSE
    WHILE recordsloaded < gen(40)
     loadrecord buf(), workingdir + SLASH + "plotscr.lst", 20, recordsloaded
     recordsloaded += 1
     IF buf(0) = temp THEN screxists = -1: EXIT WHILE
     IF buf(0) <= gen(43) THEN plotids(buf(0)) = -1
    WEND
   END IF
  END IF
  IF temp >= 16384 THEN
   loadrecord buf(), fh, 20, temp - 16384
   IF buf(0) THEN screxists = -1
  END IF
  IF screxists THEN EXIT DO
 LOOP

 CLOSE fh
END SUB

FUNCTION load_help_file(helpkey AS STRING) AS STRING
 IF isdir(exepath & SLASH & "ohrhelp") THEN
  DIM helpfile AS STRING
  helpfile = exepath & SLASH & "ohrhelp" & SLASH & helpkey & ".txt"
  IF isfile(helpfile) THEN
   DIM fh AS INTEGER = FREEFILE
   OPEN helpfile FOR INPUT ACCESS READ AS #fh
   DIM helptext AS STRING = ""
   DIM s AS STRING
   DO WHILE NOT EOF(fh)
    LINE INPUT #fh, s
    helptext = helptext & s & CHR(10)
   LOOP
   CLOSE #fh
   RETURN helptext
  END IF
 END IF
 RETURN "No help found for """ & helpkey & """"
END FUNCTION

SUB save_help_file(helpkey AS STRING, text AS STRING)
 IF NOT isdir(exepath & SLASH & "ohrhelp") THEN
  MKDIR exepath & SLASH & "ohrhelp"
 END IF
 IF isdir(exepath & SLASH & "ohrhelp") THEN
  DIM helpfile AS STRING
  helpfile = exepath & SLASH & "ohrhelp" & SLASH & helpkey & ".txt"
  IF fileiswriteable(helpfile) THEN
   DIM fh AS INTEGER = FREEFILE
   OPEN helpfile FOR OUTPUT ACCESS READ WRITE AS #fh
   DIM trimmed_text AS STRING
   trimmed_text = RTRIM(text, ANY " " & CHR(13) & CHR(10))
   PRINT #fh, trimmed_text
   CLOSE #fh
  ELSE
   debug "help file """ & helpfile & """ is not writeable."
  END IF
 ELSE
  debug "Can't create help dir """ & exepath & SLASH & "ohrhelp"""
 END IF
END SUB

SUB show_help(helpkey AS STRING)
 DIM help_str AS STRING
 help_str = load_help_file(helpkey)
 
 '--Construct the help UI (This will be hella easier later when the Slice Editor can save/load)
 DIM help_root AS Slice Ptr
 help_root = NewSliceOfType(slRoot)
 WITH *help_root
  .Y = 200
  .Fill = NO
 END WITH
 DIM help_outer_box AS Slice Ptr
 help_outer_box = NewSliceOfType(slContainer, help_root)
 WITH *help_outer_box
  .paddingTop = 4
  .paddingBottom = 4
  .paddingLeft = 4
  .paddingRight = 4
  .Fill = Yes
 END WITH
 DIM help_box AS Slice Ptr
 help_box = NewSliceOfType(slRectangle, help_outer_box)
 WITH *help_box
  .paddingTop = 8
  .paddingBottom = 8
  .paddingLeft = 8
  .paddingRight = 8
  .Fill = YES
  ChangeRectangleSlice help_box, 1
 END WITH
 DIM help_text AS Slice Ptr
 help_text = NewSliceOfType(slText, help_box)
 WITH *help_text
  .Fill = YES
  ChangeTextSlice help_text, help_str, , , YES
 END WITH
 DIM animate AS Slice Ptr
 animate = help_root

 '--Preserve whatever screen was already showing as a background
 DIM holdscreen AS INTEGER
 holdscreen = allocatepage
 copypage vpage, holdscreen

 DIM dat AS TextSliceData Ptr
 dat = help_text->SliceData
 dat->line_limit = 18

 DIM editing AS INTEGER = NO
 DIM deadkeys AS INTEGER = 25
 DIM scrollbar_state AS MenuState
 scrollbar_state.size = 17

 '--Now loop displaying help
 setkeys
 DO
  setwait 17, 70
  setkeys
  
  IF editing THEN  
   stredit(dat->s, 2048, YES, INT(help_text->Width / 8)) '2048 chars is totally arbitrary and maybe not a good limit
   dat->insert = insert '--copy the global stredit() insert point
  END IF

  IF deadkeys = 0 THEN 
   IF keyval(scESC) > 1 THEN EXIT DO
   IF keyval(scE) > 1 THEN
    editing = YES
    dat->show_insert = YES
    dat->insert = insert '--copy the global stredit() insert point
    ChangeRectangleSlice help_box, , uilook(uiBackground), , 0
   END IF
   IF keyval(scF1) and helpkey <> "helphelp" THEN
    show_help "helphelp"
   END IF
   IF keyval(scUp) > 1 THEN dat->first_line = large(0, dat->first_line - 1)
   IF keyval(scDown) > 1 THEN dat->first_line = small(large(0, dat->line_count - dat->line_limit), dat->first_line + 1)
  END IF
  deadkeys = large(deadkeys -1, 0)

  'Animate the arrival of the help screen
  animate->Y = large(animate->Y - 20, 0)

  DrawSlice help_root, dpage
  
  WITH scrollbar_state
   .top = dat->first_line
   .last = dat->line_count - 1
  END WITH
  draw_fullscreen_scrollbar scrollbar_state, , dpage

  SWAP vpage, dpage
  setvispage vpage
  copypage holdscreen, dpage
  dowait
 LOOP

 '--If there are any changes to the help screen, offer to save them
 IF help_str <> dat->s THEN
  IF yesno("Save changes to help for """ & helpkey & """?", YES) THEN
   save_help_file helpkey, dat->s
  END IF
 END IF

 '--Animate the removal of the help screen
 DO
  setkeys
  setwait 17, 70
  animate->Y = animate->Y + 20
  IF animate->Y > 200 THEN EXIT DO
  DrawSlice help_root, dpage
  SWAP vpage, dpage
  setvispage vpage
  copypage holdscreen, dpage
  dowait
 LOOP
  
 freepage holdscreen
 DeleteSlice @help_root
END SUB

SUB draw_gather_script_usage_meter (BYVAL meter AS INTEGER, BYVAL meter_times AS INTEGER=1) 
 DIM AS INTEGER max, size
 max = (8 + gen(genMaxTextBox) + gen(genMaxMap) + gen(genMaxVehicle) + gen(genMaxShop) + gen(genMaxMenu)) * meter_times
 size = 320 / max * meter
 edgebox 0, 188, size, 12, uilook(uiTimeBarFull), uilook(uiTimeBar), vpage
 edgeprint "searching for plotscript usage", 1, 189, uilook(uiText), vpage
 setvispage vpage
END SUB

SUB gather_script_usage(list() AS STRING, BYVAL id AS INTEGER, BYVAL trigger AS INTEGER=0, BYREF meter AS INTEGER, BYVAL meter_times AS INTEGER=1, box_instead_cache() AS INTEGER, box_after_cache() AS INTEGER, box_preview_cache() AS STRING)
 '--Global scripts
 IF gen(genNewGameScript) = id THEN str_array_append list(), "  new game script"
 IF gen(genGameoverScript) = id THEN str_array_append list(), "  game over script"
 IF gen(genLoadGameScript) = id THEN str_array_append list(), "  load game script"
 meter += 3
 draw_gather_script_usage_meter meter, meter_times 
 DIM AS INTEGER i, j

 '--Text box scripts
 FOR i = 0 TO gen(genMaxTextbox)
  IF box_instead_cache(i) = id THEN str_array_append list(), "  box " & i & " (instead) " & box_preview_cache(i)
  IF box_after_cache(i) = id THEN str_array_append list(), "  box " & i & " (after) " & box_preview_cache(i)
  meter += 1
  IF meter MOD 64 = 0 THEN draw_gather_script_usage_meter meter, meter_times 
 NEXT i
 
 '--Map scripts and NPC scripts
 DIM gmaptmp(dimbinsize(binMAP))
 DIM npctmp(max_npc_defs) AS NPCType
 FOR i = 0 TO gen(genMaxMap)
  loadrecord gmaptmp(), game & ".map", dimbinsize(binMAP), i
  IF gmaptmp(7) = id THEN str_array_append list(), "  map " & i & " autorun" 
  IF gmaptmp(12) = id THEN str_array_append list(), "  map " & i & " after-battle" 
  IF gmaptmp(13) = id THEN str_array_append list(), "  map " & i & " instead-of-battle" 
  IF gmaptmp(14) = id THEN str_array_append list(), "  map " & i & " each-step" 
  IF gmaptmp(15) = id THEN str_array_append list(), "  map " & i & " on-keypress" 
  'loop through NPC's
  LoadNPCD maplumpname(i, "n"), npctmp()
  FOR j = 0 TO max_npc_defs
   IF npctmp(j).script = id THEN str_array_append list(), "  map " & i & " NPC " & j
  NEXT j
  meter += 1
  IF meter MOD 64 = 0 THEN draw_gather_script_usage_meter meter, meter_times 
 NEXT i
 
 '--vehicle scripts
 DIM vehtmp(39) AS INTEGER
 DIM vehname AS STRING
 FOR i = 0 TO gen(genMaxVehicle)
  LoadVehicle  game & ".veh", vehtmp(), vehname, i
  IF vehtmp(12) = id THEN str_array_append list(), "  use button vehicle " & i & " """ & vehname & """"
  IF vehtmp(13) = id THEN str_array_append list(), "  menu button vehicle " & i & " """ & vehname & """"
  IF -vehtmp(15) = id THEN str_array_append list(), "  on mount vehicle " & i & " """ & vehname & """"
  IF -vehtmp(16) = id THEN str_array_append list(), "  on dismount vehicle " & i & " """ & vehname & """"
  meter += 1
  IF meter MOD 64 = 0 THEN draw_gather_script_usage_meter meter, meter_times 
 NEXT i
 
 '--shop scripts
 DIM shoptmp(19)
 DIM shopname AS STRING
 FOR i = 0 TO gen(genMaxShop)
  loadrecord shoptmp(), game & ".sho", 20, i
  shopname = readbadbinstring(shoptmp(), 0, 15)
  IF shoptmp(19) = id THEN str_array_append list(), "  shop inn " & i & " """ & shopname & """"
  meter += 1
  IF meter MOD 64 = 0 THEN draw_gather_script_usage_meter meter, meter_times 
 NEXT i
 
 '--menu scripts
 DIM menu_set AS MenuSet
 menu_set.menufile = workingdir + SLASH + "menus.bin"
 menu_set.itemfile = workingdir + SLASH + "menuitem.bin"
 DIM menutmp AS MenuDef
 FOR i = 0 TO gen(genMaxMenu)
  LoadMenuData menu_set, menutmp, i
  FOR j = 0 TO UBOUND(menutmp.items)
   WITH menutmp.items(j)
    IF .exists THEN
     IF .t = 4 THEN
      IF .sub_t = id THEN str_array_append list(), "  menu " & i & " item " & j & " """ & .caption & """"
     END IF
    END IF
   END WITH
  NEXT j
  meter += 1
  IF meter MOD 64 = 0 THEN draw_gather_script_usage_meter meter, meter_times 
 NEXT i

END SUB

SUB script_usage_list ()
 REDIM list(0) AS STRING
 DIM buf(20) AS INTEGER
 DIM id AS INTEGER
 DIM s AS STRING
 DIM fh AS INTEGER
 DIM meter AS INTEGER = 0
 DIM meter_times AS INTEGER = 0

 list(0) = "back to previous menu..."

 'Get script count for progress meter
 ' (yeah, I know we loop through the all the scripts twice.
 ' that is okay because gather_script_usage is the really slow part)
 fh = FREEFILE
 OPEN workingdir & SLASH & "plotscr.lst" FOR BINARY ACCESS READ AS #fh
 FOR i AS INTEGER = 0 TO gen(genMaxPlotscript)
  loadrecord buf(), fh, 20, i
  id = buf(0)
  IF id <= 16383 THEN
   meter_times += 1
  END IF
 NEXT i
 CLOSE #fh
 fh = FREEFILE
 OPEN workingdir & SLASH & "lookup1.bin" FOR BINARY ACCESS READ AS #fh
 DIM plotscript_count AS INTEGER = (LOF(fh) \ 40) - 1
 FOR i AS INTEGER = 0 TO plotscript_count
  loadrecord buf(), fh, 20, i
  id = buf(0)
  IF id <> 0 THEN
   meter_times += 1
  END IF
 NEXT i
 CLOSE #fh

 draw_gather_script_usage_meter meter, meter_times

 '--Pre-cache text box script triggers for performance purposes
 DIM box_instead_cache(gen(genMaxTextbox)) AS INTEGER
 DIM box_after_cache(gen(genMaxTextbox)) AS INTEGER
 DIM box_preview_cache(gen(genMaxTextbox)) AS STRING
 DIM box AS TextBox
 FOR i AS INTEGER = 0 TO gen(genMaxTextbox)
  LoadTextBox box, i
  box_instead_cache(i) = -box.instead
  box_after_cache(i) = -box.after
  IF box_instead_cache(i) > 0 OR box_after_cache(i) > 0 THEN
   '--Don't bother loading the preview line unless there is a script trigger
   box_preview_cache(i) = textbox_preview_line(box)
  END IF
 NEXT i

 'Loop through old-style non-autonumbered scripts
 fh = FREEFILE
 OPEN workingdir & SLASH & "plotscr.lst" FOR BINARY ACCESS READ AS #fh
 FOR i AS INTEGER = 0 TO gen(genMaxPlotscript)
  loadrecord buf(), fh, 20, i
  id = buf(0)
  IF id <= 16383 THEN
   s = id & ":" & readbinstring(buf(), 1, 38)
   str_array_append list(), s
   gather_script_usage list(), id, 0, meter, meter_times, box_instead_cache(), box_after_cache(), box_preview_cache()
  END IF
 NEXT i
 CLOSE #fh

 'Loop through new-style plotscripts
 fh = FREEFILE
 OPEN workingdir & SLASH & "lookup1.bin" FOR BINARY ACCESS READ AS #fh
 FOR i AS INTEGER = 0 TO plotscript_count
  loadrecord buf(), fh, 20, i
  id = buf(0)
  s = readbinstring(buf(), 1, 38)
  IF id <> 0 THEN
   str_array_append list(), s
   gather_script_usage list(), i + 16384, plottrigger, meter, meter_times, box_instead_cache(), box_after_cache(), box_preview_cache()
  END IF
 NEXT i
 CLOSE #fh

 DIM state AS MenuState
 state.size = 24
 state.last = UBOUND(list)
 
 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "script_usage_list"
  IF enter_or_space() THEN
   IF state.pt = 0 THEN EXIT DO
  END IF
  usemenu state

  draw_fullscreen_scrollbar state, , dpage 
  standardmenu list(), state, 0, 0, dpage

  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
 
END SUB

'--A similar function exists in yetmore2.bas for game. it differs only in error-reporting
FUNCTION decodetrigger (trigger as integer, trigtype as integer) as integer
 DIM buf(19) AS INTEGER
 DIM fname AS STRING
 IF trigger >= 16384 THEN
  fname = workingdir & SLASH & "lookup" & trigtype & ".bin"
  IF isfile(fname) THEN
   IF loadrecord (buf(), fname$, 20, trigger - 16384) THEN
    RETURN buf(0)
   ELSE
    debug "decodetrigger: record " & (trigger - 16384) & " could not be loaded"
   END IF
  ELSE
   debug "decodetrigger: file " & fname & " could not be found"
  END IF
 ELSE
  '--this is an old-style script
  RETURN trigger
 END IF
END FUNCTION

SUB check_broken_script_trigger(list() AS STRING, BYVAL trig AS INTEGER, description AS STRING, caption AS STRING = "")
 IF trig <= 0 THEN EXIT SUB ' No script trigger
 '--decode script trigger
 DIM id AS INTEGER
 id = decodetrigger(trig, plottrigger)
 '--Check for missing new-style script
 IF id = 0 THEN
  str_array_append list(), description & " " & scriptname(trig, plottrigger) & " missing" & caption 
  EXIT SUB
 END IF
 '--now check for missing old-style scripts
 IF id < 16384 THEN
  DIM buf(19)
  DIM fh AS INTEGER = FREEFILE
  OPEN workingdir & SLASH & "plotscr.lst" FOR BINARY ACCESS READ AS #fh
  DIM found AS INTEGER = NO
  FOR i AS INTEGER = 0 TO gen(genMaxPlotScript)
   loadrecord buf(), fh, 20, i
   IF buf(0) = id THEN
    found = YES
    EXIT FOR '-- Found it okay!
   END IF
  NEXT i
  CLOSE #fh
  IF found = NO THEN
   str_array_append list(), description & " ID " & id & " missing" & caption
  END IF
 END IF
END SUB

SUB script_broken_trigger_list()
 REDIM list(0) AS STRING

 list(0) = "back to previous menu..."

 '--global scripts
 check_broken_script_trigger list(), gen(genNewGameScript), "new game"
 check_broken_script_trigger list(), gen(genLoadGameScript), "load game"
 check_broken_script_trigger list(), gen(genGameoverScript), "game over"

 DIM AS INTEGER i, j

 '--Text box scripts
 DIM box AS TextBox
 FOR i AS INTEGER = 0 TO gen(genMaxTextbox)
  LoadTextBox box, i
  check_broken_script_trigger list(), -box.instead, "box " & i & " (instead)", ":" & textbox_preview_line(box)
  check_broken_script_trigger list(), -box.after, "box " & i & " (after)", ":" & textbox_preview_line(box)
 NEXT i
 
 '--Map scripts and NPC scripts
 DIM gmaptmp(dimbinsize(binMAP))
 DIM npctmp(max_npc_defs) AS NPCType
 FOR i = 0 TO gen(genMaxMap)
  loadrecord gmaptmp(), game & ".map", dimbinsize(binMAP), i
  check_broken_script_trigger list(), gmaptmp(7), "map " & i & " autorun"
  check_broken_script_trigger list(), gmaptmp(12), "map " & i & " after-battle"
  check_broken_script_trigger list(), gmaptmp(13), "map " & i & " instead-of-battle"
  check_broken_script_trigger list(), gmaptmp(14), "map " & i & " each-step"
  check_broken_script_trigger list(), gmaptmp(15), "map " & i & " on-keypress"
  'loop through NPC's
  LoadNPCD maplumpname(i, "n"), npctmp()
  FOR j = 0 TO max_npc_defs
   check_broken_script_trigger list(), npctmp(j).script, "map " & i & " NPC " & j
  NEXT j
 NEXT i
 
 '--vehicle scripts
 DIM vehtmp(39) AS INTEGER
 DIM vehname AS STRING
 FOR i = 0 TO gen(genMaxVehicle)
  LoadVehicle game & ".veh", vehtmp(), vehname, i
  check_broken_script_trigger list(), vehtmp(12), "use button veh " & i, " """ & vehname & """"
  check_broken_script_trigger list(), vehtmp(13), "menu button veh " & i, " """ & vehname & """"
  check_broken_script_trigger list(), -vehtmp(15), "mount vehicle " & i, " """ & vehname & """"
  check_broken_script_trigger list(), -vehtmp(16), "dismount vehicle " & i,  " """ & vehname & """"
 NEXT i
 
 '--shop scripts
 DIM shoptmp(19)
 DIM shopname AS STRING
 FOR i = 0 TO gen(genMaxShop)
  loadrecord shoptmp(), game & ".sho", 20, i
  shopname = readbadbinstring(shoptmp(), 0, 15)
  check_broken_script_trigger list(), shoptmp(19), "show inn " & i, " """ & shopname & """"
 NEXT i
 
 '--menu scripts
 DIM menu_set AS MenuSet
 menu_set.menufile = workingdir + SLASH + "menus.bin"
 menu_set.itemfile = workingdir + SLASH + "menuitem.bin"
 DIM menutmp AS MenuDef
 FOR i = 0 TO gen(genMaxMenu)
  LoadMenuData menu_set, menutmp, i
  FOR j = 0 TO UBOUND(menutmp.items)
   WITH menutmp.items(j)
    IF .exists THEN
     IF .t = 4 THEN
      check_broken_script_trigger list(), .sub_t, "menu " & i & " item " & j & " """ & .caption & """"
     END IF
    END IF
   END WITH
  NEXT j
 NEXT i

 IF UBOUND(list) = 0 THEN
  str_array_append list(), "No broken triggers found!"
 END IF

 DIM state AS MenuState
 state.size = 24
 state.last = UBOUND(list)

 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "script_broken_trigger_list"
  IF enter_or_space() THEN
   IF state.pt = 0 THEN EXIT DO
  END IF
  usemenu state

  draw_fullscreen_scrollbar state, , dpage 
  standardmenu list(), state, 0, 0, dpage

  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
 
END SUB

FUNCTION autofix_old_script(BYREF id AS INTEGER) AS INTEGER
 '--returns true if a fix has occured
 IF id = 0 THEN RETURN NO ' not a trigger
 IF id >= 16384 THEN RETURN NO 'New-style script
 DIM buf(19) AS INTEGER
 DIM fh AS INTEGER
 
 fh = FREEFILE
 OPEN workingdir & SLASH & "plotscr.lst" FOR BINARY ACCESS READ AS #fh
 FOR i AS INTEGER = 0 TO gen(genMaxPlotScript)
  loadrecord buf(), fh, 20, i
  IF buf(0) = id THEN
   CLOSE #fh
   RETURN NO 'Found okay
  END IF
 NEXT i
 CLOSE #fh
 
 DIM found_name AS STRING = ""
 
 fh = FREEFILE
 OPEN tmpdir & "plotscr.lst.tmp" FOR BINARY ACCESS READ AS #fh
 FOR i AS INTEGER = 0 TO (LOF(fh) \ 40) - 1
  loadrecord buf(), fh, 20, i
  IF buf(0) = id THEN '--Yay! found it in the old file!
   found_name = readbinstring(buf(), 1, 38)
   EXIT FOR
  END IF
 NEXT i
 CLOSE #fh
 
 IF found_name = "" THEN RETURN NO '--broken but unfixable (no old name)

 fh = FREEFILE
 OPEN workingdir & SLASH & "lookup1.bin" FOR BINARY ACCESS READ AS #fh
 FOR i AS INTEGER = 0 TO (LOF(fh) \ 40) - 1
  loadrecord buf(), fh, 20, i
  IF found_name = readbinstring(buf(), 1, 38) THEN '--Yay! found it in the new file!
   id = 16384 + i
   CLOSE #fh
   RETURN YES '--fixed it, report a change!
  END IF
 NEXT i
 CLOSE #fh 

 RETURN NO '--broken but unfixable (no matching new name)
 
END FUNCTION

SUB autofix_broken_old_scripts()

 '--sanity test
 IF NOT isfile(tmpdir & "plotscr.lst.tmp") THEN
  debug "can't autofix broken old scripts, can't find: " & tmpdir & "plotscr.lst.tmp"
 END IF

 '--global scripts
 autofix_old_script gen(genNewGameScript)
 autofix_old_script gen(genLoadGameScript)
 autofix_old_script gen(genGameoverScript)

 DIM AS INTEGER i, j, idtmp, resave

 '--Text box scripts
 DIM box AS TextBox
 FOR i AS INTEGER = 0 TO gen(genMaxTextbox)
  LoadTextBox box, i
  resave = NO
  IF box.instead < 0 THEN
   idtmp = -box.instead
   IF autofix_old_script(idtmp) THEN resave = YES
   box.instead = -idtmp
  END IF
  IF box.after < 0 THEN
   idtmp = -box.after
   IF autofix_old_script(idtmp) THEN resave = YES
   box.after = -idtmp
  END IF
  IF resave THEN
   SaveTextBox box, i
  END IF
 NEXT i
 
 '--Map scripts and NPC scripts
 DIM gmaptmp(dimbinsize(binMAP))
 DIM npctmp(max_npc_defs) AS NPCType
 FOR i = 0 TO gen(genMaxMap)
  resave = NO
  loadrecord gmaptmp(), game & ".map", dimbinsize(binMAP), i
  IF autofix_old_script(gmaptmp(7)) THEN resave = YES 'autorun
  IF autofix_old_script(gmaptmp(12)) THEN resave = YES 'after-battle
  IF autofix_old_script(gmaptmp(13)) THEN resave = YES 'instead-of-battle
  IF autofix_old_script(gmaptmp(14)) THEN resave = YES 'each-step
  IF autofix_old_script(gmaptmp(15)) THEN resave = YES 'on-keypress
  IF resave THEN
   storerecord gmaptmp(), game & ".map", dimbinsize(binMAP), i
  END IF
  'loop through NPC's
  LoadNPCD maplumpname(i, "n"), npctmp()
  resave = NO
  FOR j = 0 TO max_npc_defs
   IF autofix_old_script(npctmp(j).script) THEN resave = YES
  NEXT j
  IF resave THEN
   SaveNPCD maplumpname(i, "n"), npctmp()
  END IF
 NEXT i
 
 '--vehicle scripts
 DIM vehtmp(39) AS INTEGER
 DIM vehname AS STRING
 FOR i = 0 TO gen(genMaxVehicle)
  resave = NO
  LoadVehicle game & ".veh", vehtmp(), vehname, i
  IF vehtmp(12) > 0 THEN
   IF autofix_old_script(vehtmp(12)) THEN resave = YES 'use button vehicle
  END IF
  IF vehtmp(13) > 0 THEN
   IF autofix_old_script(vehtmp(13)) THEN resave = YES 'menu button vehicle
  END IF
  IF vehtmp(15) < 0 THEN
   idtmp = -vehtmp(15)
   IF autofix_old_script(idtmp) THEN resave = YES 'mount vehicle
   vehtmp(15) = -idtmp
  END IF
  IF vehtmp(16) < 0 THEN
   idtmp = -vehtmp(16)
   IF autofix_old_script(idtmp) THEN resave = YES 'dismount vehicle
   vehtmp(16) = -idtmp
  END IF
  IF resave THEN
   SaveVehicle game & ".veh", vehtmp(), vehname, i
  END IF
 NEXT i
 
 '--shop scripts
 DIM shoptmp(19)
 FOR i = 0 TO gen(genMaxShop)
  loadrecord shoptmp(), game & ".sho", 20, i
  IF autofix_old_script(shoptmp(19)) THEN
   storerecord shoptmp(), game & ".sho", 20, i
  END IF
 NEXT i
 
 '--menu scripts
 DIM menu_set AS MenuSet
 menu_set.menufile = workingdir + SLASH + "menus.bin"
 menu_set.itemfile = workingdir + SLASH + "menuitem.bin"
 DIM menutmp AS MenuDef
 FOR i = 0 TO gen(genMaxMenu)
  resave = NO
  LoadMenuData menu_set, menutmp, i
  FOR j = 0 TO UBOUND(menutmp.items)
   WITH menutmp.items(j)
    IF .exists THEN
     IF .t = 4 THEN
      IF autofix_old_script(.sub_t) THEN resave = YES
     END IF
    END IF
   END WITH
  NEXT j
  IF resave THEN
   SaveMenuData menu_set, menutmp, i
  END IF
 NEXT i

END SUB

SUB stredit (s AS STRING, BYVAL maxl AS INTEGER, BYVAL multiline AS INTEGER=NO, BYVAL wrapchars AS INTEGER=1)
 'insert is declared EXTERN in cglobals.bi and DIMed in custom.bas
 
 STATIC clip AS STRING

 '--copy support
 IF (keyval(scCtrl) > 0 AND keyval(scInsert) > 1) OR ((keyval(scLeftShift) > 0 OR keyval(scRightShift) > 0) AND keyval(scDelete) > 0) OR (keyval(scCtrl) > 0 AND keyval(scC) > 1) THEN clip = s

 '--paste support
 IF ((keyval(scLeftShift) > 0 OR keyval(scRightShift) > 0) AND keyval(scInsert) > 1) OR (keyval(scCtrl) > 0 AND keyval(scV) > 1) THEN s = LEFT(clip, maxl)

 '--insert cursor movement
 IF keyval(scCtrl) = 0 THEN 'not CTRL
  IF keyval(scLeft) > 1 THEN insert = large(0, insert - 1)
  IF keyval(scRight) > 1 THEN insert = small(LEN(s), insert + 1)
 ELSE 'CTRL
  IF keyval(scleft) > 1 THEN insert = 0
  IF keyval(scRight) > 1 THEN insert = LEN(s)
 END IF

 '--up and down arrow keys
 IF multiline THEN
  IF keyval(scUp) > 1 OR keyval(scDown) > 1 THEN
   DIM wrapped AS STRING
   wrapped = wordwrap(s, large(1, wrapchars))
   DIM lines() AS STRING
   split(wrapped, lines())
   DIM count AS INTEGER = 0
   DIM found_insert AS INTEGER = -1
   DIM line_chars AS INTEGER
   FOR i AS INTEGER = 0 TO UBOUND(lines)
    IF count + LEN(lines(i)) >= insert THEN
     found_insert = i
     line_chars = insert - count
     EXIT FOR
    END IF
    count += LEN(lines(i)) + 1
   NEXT i
   IF found_insert >= 0 THEN
    IF keyval(scUp) > 1 AND found_insert > 0 THEN
     insert = 0
     FOR i AS INTEGER = 0 TO found_insert - 2
      insert += LEN(lines(i)) + 1
     NEXT i
     insert += small(line_chars, LEN(lines(found_insert - 1)))
    END IF
    IF keyval(scDown) > 1 AND found_insert < UBOUND(lines) THEN
     insert = 0
     FOR i AS INTEGER = 0 TO found_insert
      insert += LEN(lines(i)) + 1
     NEXT i
     insert += small(line_chars, LEN(lines(found_insert + 1)))
    END IF
   END IF
   '--end of special handling for up and down arrows
  END IF
  '--Home and end keys
  IF keyval(scHome) > 1 THEN
   DO WHILE insert > 0
    insert -= 1
    IF MID(s, 1 + insert, 1) = CHR(10) THEN insert += 1 : EXIT DO
   LOOP
  END IF
  IF keyval(scEnd) > 1 THEN
   DO WHILE insert < LEN(s)
    IF MID(s, 1 + insert, 1) = CHR(10) THEN EXIT DO
    insert += 1
   LOOP
  END IF
  '--end of special keys that only work in multiline mode
 END IF


 IF insert < 0 THEN insert = LEN(s)
 insert = bound(insert, 0, LEN(s))

 DIM pre AS STRING = LEFT(s, insert)
 DIM post AS STRING = RIGHT(s, LEN(s) - insert)

 '--BACKSPACE support
 IF keyval(scBackspace) > 1 AND LEN(pre) > 0 THEN
  pre = LEFT(pre, LEN(pre) - 1)
  insert = large(0, insert - 1)
 END IF

 '--DEL support
 IF keyval(scDelete) > 1 AND LEN(post) > 0 THEN post = RIGHT(post, LEN(post) - 1)

 '--SHIFT support
 DIM shift AS INTEGER = 0
 IF keyval(scRightShift) > 0 OR keyval(scLeftShift) > 0 THEN shift = 1

 '--ALT support
 IF keyval(scAlt) THEN shift += 2

 '--adding chars
 IF LEN(pre) + LEN(post) < maxl THEN
  DIM L AS INTEGER = LEN(pre)
  IF keyval(scSpace) > 1 THEN
   IF keyval(scCTRL) = 0 THEN
    '--SPACE support
    pre = pre & " "
   ELSE
    '--charlist support
    pre = pre & charpicker()
   END IF
  ELSEIF multiline AND keyval(scEnter) > 1 THEN
   pre = pre & CHR(10)
  ELSE
   IF keyval(scCtrl) = 0 THEN
    '--all other keys
    FOR i AS INTEGER = 2 TO 53
     IF keyval(i) > 1 AND keyv(i, shift) > 0 THEN
      pre = pre & CHR(keyv(i, shift))
      EXIT FOR
     END IF
    NEXT i
   END IF
  END IF
  IF LEN(pre) > L THEN insert += 1
 END IF

 s = pre & post

END SUB

FUNCTION sublist (s() AS STRING, helpkey AS STRING="") AS INTEGER
 clearpage 0
 clearpage 1
 DIM state AS MenuState
 state.pt = 0
 state.last = UBOUND(s)
 state.size = 22
 
 setkeys
 DO
  setwait 55
  setkeys
  usemenu state
  IF keyval(scESC) > 1 THEN
   sublist = -1
   EXIT DO
  END IF
  IF keyval(scF1) > 1 AND helpkey <> "" THEN show_help helpkey
  IF enter_or_space() THEN
   sublist = state.pt
   EXIT DO
  END IF
  standardmenu s(), state, 0, 0, dpage
  SWAP dpage, vpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
 clearpage 0
 clearpage 1
END FUNCTION
