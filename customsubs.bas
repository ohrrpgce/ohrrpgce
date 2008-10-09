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

#include "customsubs.bi"

'Subs and functions defined elsewhere
DECLARE FUNCTION scriptbrowse$ (trigger%, triggertype%, scrtype$)
DECLARE FUNCTION scrintgrabber (n%, BYVAL min%, BYVAL max%, BYVAL less%, BYVAL more%, scriptside%, triggertype%)

'Subs and functions only used here
DECLARE SUB import_textboxes_warn (BYREF warn AS STRING, s AS STRING)

OPTION EXPLICIT

FUNCTION tag_grabber (BYREF n AS INTEGER, min AS INTEGER=-999, max AS INTEGER=999) AS INTEGER
 IF intgrabber(n, min, max) THEN RETURN YES
 IF enter_or_space() THEN
  DIM browse_tag AS INTEGER
  browse_tag = tagnames(n, YES)
  IF browse_tag >= 2 THEN
   n = browse_tag
   RETURN YES
  END IF
 END IF
 RETURN NO
END FUNCTION

FUNCTION tagnames (starttag AS INTEGER=0, picktag AS INTEGER=NO) AS INTEGER
DIM state AS MenuState
DIM thisname AS STRING
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
 IF keyval(1) > 1 THEN EXIT DO
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

 standardmenu menu(), state, 0, 0, dpage

 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

RETURN 0
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
 IF keyval(1) > 1 THEN RETURN ""

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
  IF keyval(1) > 1 THEN EXIT DO
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
  IF keyval(1) > 1 THEN RETURN start_color

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
  IF keyval(1) > 1 THEN RETURN -1   'cancel
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

FUNCTION yesno(capt AS STRING, defaultval AS INTEGER=YES, escval AS INTEGER=NO) AS INTEGER
 DIM state AS MenuState
 DIM menu AS MenuDef
 DIM result AS INTEGER

 append_menu_item menu, "Yes"
 append_menu_item menu, "No"

 state.active = YES
 init_menu_state state, menu
 IF defaultval = YES THEN state.pt = 0
 IF defaultval = NO  THEN state.pt = 1 

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
   IF state.pt = 0 THEN result = YES
   IF state.pt = 1 THEN result = NO
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
  "Ç","É","Ñ","Ö","Ü","á","à","â","ä","ã","å","ç","","","é","è","ê","ë","í","ì","î","ï","ñ","ó","ò","ô","","","ö","õ","ú","ù","û","ü","†","°","¢","£","§","•","","¶","ß","®","©","™","´","¨","≠","Æ","Ø","∞", _
  "±","≤","≥","¥","µ","∂","∑","∏","π","∫","ª","º","","","Ω","æ","ø","¿","¡","¬","√","ƒ","≈","∆","«","»","","","…"," ","À","Ã","Õ","Œ","œ","–","—","“","”","‘","","’","÷","◊","ÿ","Ÿ","⁄","€","‹","›","ﬁ","ﬂ"}
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

SUB edit_npc (npcid AS INTEGER, npc() AS INTEGER)
 DIM i AS INTEGER

 DIM npcdata AS NPCtype
 FOR i = 0 to 14
  write_npc_int npcdata, i, npc(npcid * 15 + i)
 NEXT i

 DIM itemname AS STRING
 DIM boxpreview AS STRING
 DIM scrname AS STRING
 DIM vehiclename AS STRING
 DIM caption AS STRING
 DIM appearstring AS STRING

 DIM walk AS INTEGER = 0
 DIM tog AS INTEGER = 0

 DIM unpc(15) AS INTEGER, lnpc(15) AS INTEGER
 DIM menucaption(15) AS STRING, movetype(10) AS STRING, pushtype(7) AS STRING, stepi(5), usetype(5, 1) AS STRING

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
 stepi(0) = 0
 stepi(1) = 1
 stepi(2) = 2
 stepi(3) = 10
 stepi(4) = 4
 stepi(5) = 5
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
  IF keyval(1) > 1 THEN EXIT DO
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
    intgrabber(npcdata.speed, lnpc(state.pt), unpc(state.pt))
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
     caption = " " & stepi(npcdata.speed)
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

 FOR i = 0 to 14
  npc(npcid * 15 + i) = read_npc_int(npcdata, i)
 NEXT i

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
  IF keyval(1) > 1 THEN EXIT DO
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
  IF keyval(1) > 1 THEN RETURN NO
  
  IF keyval(28) > 1 THEN
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
    PRINT #fh, "Instead Tag: " & box.instead_tag & " (" & tag_condition_caption(box.instead_tag, , "Impossible", "Never", "Always") & ")"
    PRINT #fh, "Instead Box: " & box.instead;
    IF box.instead < 0 THEN
     PRINT #fh, " (Plotscript " & scriptname$(box.instead * -1, plottrigger) & ")"
    ELSE
     PRINT #fh, " (Textbox)"
    END IF
   END IF
   IF box.after_tag <> 0 THEN
    PRINT #fh, "Next Tag: " & box.after_tag & " (" & tag_condition_caption(box.after_tag, , "Impossible", "Never", "Always") & ")"
    PRINT #fh, "Next Box: " & box.after;
    IF box.after < 0 THEN
     PRINT #fh, " (Plotscript " & scriptname$(box.after * -1, plottrigger) & ")"
    ELSE
     PRINT #fh, " (Textbox)"
    END IF
   END IF
   
   IF box.settag_tag <> 0 THEN
    PRINT #fh, "Set Tag: " & box.settag_tag & " (" & tag_condition_caption(box.settag_tag, , "Impossible", "Never", "Always") & ")"
    IF box.settag1 <> 0 THEN PRINT #fh, "Set Tag 1: " & box.settag1 & " (" & tag_condition_caption(box.settag1, , "Impossible", "Never", "Always") & ")"
    IF box.settag2 <> 0 THEN PRINT #fh, "Set Tag 2: " & box.settag2 & " (" & tag_condition_caption(box.settag2, , "Impossible", "Never", "Always") & ")"
   END IF
   IF box.battle_tag <> 0 THEN
    PRINT #fh, "Battle Tag: " & box.battle_tag & " (" & tag_condition_caption(box.battle_tag, , "Impossible", "Never", "Always") & ")"
    PRINT #fh, "Battle: " & box.battle
   END IF
   IF box.shop_tag <> 0 THEN
    PRINT #fh, "Shop Tag: " & box.shop_tag & " (" & tag_condition_caption(box.shop_tag, , "Impossible", "Never", "Always") & ")"
    PRINT #fh, "Shop: " & box.shop;
    if(box.shop = 0) THEN PRINT #fh, " (Restore HP/MP)"
    if(box.shop < 0) THEN PRINT #fh, " (Inn for $" & (box.shop * -1) & ")"
    if(box.shop > 0) THEN PRINT #fh, " (" & readshopname$(box.shop - 1) & ")"
   END IF
   IF box.hero_tag <> 0 THEN
    PRINT #fh, "Hero Tag: " & box.hero_tag & " (" & tag_condition_caption(box.hero_tag, , "Impossible", "Never", "Always") & ")"
    
    IF box.hero_addrem <> 0 THEN
     PRINT #fh, "Hero Add: " & box.hero_addrem;
     IF box.hero_addrem < 0 THEN
      PRINT #fh, " (Remove " & getheroname((box.hero_addrem * -1) - 1) & ")"
     ELSE
      PRINT #fh, " (Add " & getheroname(box.hero_addrem - 1) & ")"
     END IF
    END IF
    
    IF box.hero_swap <> 0 THEN
     PRINT #fh, "Hero Swap: " & box.hero_swap;
     IF box.hero_swap < 0 THEN
      PRINT #fh, " (Swap Out " & getheroname((box.hero_swap * -1) - 1) & ")"
     ELSE
      PRINT #fh, " (Swap In " & getheroname(box.hero_swap - 1) & ")"
     END IF
    END IF
    
    IF box.hero_lock <> 0 THEN
     PRINT #fh, "Hero Lock: " & box.hero_lock;
     IF box.hero_lock < 0 THEN
      PRINT #fh, " (Lock " & getheroname((box.hero_lock * -1) - 1) & ")"
     ELSE
      PRINT #fh, " (Unlock " & getheroname(box.hero_lock - 1) & ")"
     END IF
    END IF
    
   END IF
   
   IF box.money_tag <> 0 THEN
    PRINT #fh, "Money Tag: " & box.money_tag & " (" & tag_condition_caption(box.money_tag, , "Impossible", "Never", "Always") & ")"
    PRINT #fh, "Money: " & box.money
   END IF
   
   IF box.door_tag <> 0 THEN
    PRINT #fh, "Door Tag: " & box.door_tag & " (" & tag_condition_caption(box.door_tag, , "Impossible", "Never", "Always") & ")"
    PRINT #fh, "Door: " & box.door
   END IF
   
   IF box.item_tag <> 0 THEN
    PRINT #fh, "Item Tag: " & box.item_tag & " (" & tag_condition_caption(box.item_tag, , "Impossible", "Never", "Always") & ")"
    PRINT #fh, "Item: " & box.item;
    IF box.item < 0 THEN
     PRINT #fh, " (Remove " & readitemname$((box.item * -1) - 1) & ")"
    ELSE
     PRINT #fh, " (Add " & readitemname$(box.item - 1) & ")"
    END IF
   END IF
  END IF
  
  IF box.menu_tag <> 0 THEN
    PRINT #fh, "Menu Tag: " & box.menu_tag & " (" & tag_condition_caption(box.menu_tag, , "Impossible", "Never", "Always") & ")"
    PRINT #fh, "Menu: " & box.menu
   END IF
   
  IF metadata(2) THEN '--choices
   IF box.choice_enabled THEN
    PRINT #fh, "Choice Enabled: YES"
    PRINT #fh, "Choice 1: " & box.choice(0)
    PRINT #fh, "Choice 1 Tag: " & box.choice_tag(0) & " (" & tag_condition_caption(box.choice_tag(0), , "Do Nothing", "Never", "Always") & ")"
    PRINT #fh, "Choice 2: " & box.choice(1)
    PRINT #fh, "Choice 2 Tag: " & box.choice_tag(1) & " (" & tag_condition_caption(box.choice_tag(1), , "Do Nothing", "Never", "Always") & ")"
    
   END IF
  END IF
  
  IF metadata(3) THEN '--box appearance
   PRINT #fh, "Size: " & (21 - box.shrink)
   PRINT #fh, "Position: " & box.vertical_offset
   PRINT #fh, "Text Color: " & box.textcolor '--AARGH.
   PRINT #fh, "Border Color: " & box.boxstyle '--AARGH AGAIN.
   PRINT #fh, "Backdrop: " & box.backdrop
   IF box.music > 0 THEN
    PRINT #fh, "Music: " & box.music & " (" & getsongname$(box.music - 1) & ")"
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
     PRINT #fh, box.text(j)
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
        if VALINT(v) > 21 THEN
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
        box.door_tag = VALINT(v)
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
 
  IF keyval(1) > 1 THEN EXIT DO
  
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
 
  IF keyval(1) > 1 THEN EXIT DO
  
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
  usemenu state
  IF state.pt >= 0 THEN
   IF keyval(scLeft) > 1 OR keyval(scComma) > 1 THEN setbit array(), wof, bits(state.pt), 0
   IF keyval(scRight) > 1 OR keyval(scPeriod) > 1 THEN setbit array(), wof, bits(state.pt), 1
   IF enter_or_space() THEN setbit array(), wof, bits(state.pt), readbit(array(), wof, bits(state.pt)) XOR 1
  ELSE
   IF enter_or_space() THEN EXIT DO
  END IF
  draw_fullscreen_scrollbar state, UBOUND(menu) + 1, 0, dpage
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
