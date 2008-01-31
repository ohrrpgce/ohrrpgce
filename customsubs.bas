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

#include "customsubs.bi"

'Subs and functions defined elsewhere
DECLARE FUNCTION scriptbrowse$ (trigger%, triggertype%, scrtype$)
DECLARE FUNCTION scrintgrabber (n%, BYVAL min%, BYVAL max%, BYVAL less%, BYVAL more%, scriptside%, triggertype%)
DECLARE FUNCTION pal16browse% (curpal%, usepic%, picx%, picy%, picw%, pich%, picpage%)

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
 setwait 100
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
IF (keyval(29) > 0 AND keyval(82) > 1) OR ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(83)) OR (keyval(29) > 0 AND keyval(46) > 1) THEN clip = s

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
 setwait 100
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

 DefaultUIColors default_colors()

 LoadUIColors uilook(), palnum

 DIM color_menu(uiColors + 1) AS STRING
 make_ui_color_editor_menu color_menu(), uilook()

 DIM state AS MenuState
 state.size = 22
 state.last = UBOUND(color_menu)

 DIM tog AS INTEGER = 0
 setkeys
 DO
  setwait 100
  setkeys
  tog = tog XOR 1
  IF keyval(1) > 1 THEN EXIT DO
  usemenu state

  index = state.pt - 1

  IF enter_or_space() THEN
   IF state.pt = 0 THEN EXIT DO
   uilook(index) = color_browser_256(uilook(index))
   make_ui_color_editor_menu color_menu(), uilook() 
  END IF

  IF state.pt > 0 THEN
   IF intgrabber(uilook(index), 0, 255) THEN
    make_ui_color_editor_menu color_menu(), uilook()
   END IF
  END IF

  IF keyval(29) > 0 AND keyval(32) > 1 THEN ' Ctrl+D
   uilook(index) = default_colors(index)
   make_ui_color_editor_menu color_menu(), uilook()
  END IF

  '--update sample according to what you have highlighted
  sample_menu.boxstyle = 0
  sample_state.pt = 0
  SELECT CASE state.pt - 1
   CASE 5,6 ' selected disabled
    sample_state.pt = 2
   CASE 18 TO 47
    sample_menu.boxstyle = INT((state.pt - 19) / 2)
  END SELECT

  draw_menu sample_menu, sample_state, dpage
  standardmenu color_menu(), state, 10, 0, dpage
  FOR i = state.top TO state.top + state.size
   IF i > 0 THEN
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
  m(19 + i*2) = "Box style " & i & " color:" & colors(18 + i*2)
  m(19 + i*2 + 1) = "Box style " & i & " border:" & colors(18 + i*2 + 1)
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
  setwait 100
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
  setwait 80
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
 DIM holdscreen(DIMSCREENPAGE) AS UBYTE
 copypage vpage, dpage
 copypage vpage, holdscreen()
 
 setkeys
 DO
  setwait 100
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

  centerbox 160, 70, small(16 + LEN(capt) * 8, 320), 16, uilook(uiHighlight), dpage
  edgeprint capt, xstring(capt, 160), 65, uilook(uiMenuItem), dpage
  draw_menu menu, state, dpage
  SWAP vpage, dpage
  setvispage vpage
  copypage holdscreen(), dpage
  dowait
 LOOP

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

SUB cycletile (tanim_state() AS TileAnimState, tastuf() AS INTEGER)
 'Note that although this is almost the same as the cycletile in game, it does not handle tags
 DIM i AS INTEGER
 DIM notstuck AS INTEGER
 FOR i = 0 TO 1
  WITH tanim_state(i)
   .skip = large(.skip - 1, 0)
   IF .skip = 0 THEN
    notstuck = 10
    DO
     SELECT CASE tastuf(2 + 20 * i + .pt)
      CASE 0
       .pt = 0
       .cycle = 0
      CASE 1
       .cycle = .cycle - tastuf(11 + 20 * i + .pt) * 16
       .pt = loopvar(.pt, 0, 8, 1)
      CASE 2
       .cycle = .cycle + tastuf(11 + 20 * i + .pt) * 16
       .pt = loopvar(.pt, 0, 8, 1)
      CASE 3
       .cycle = .cycle + tastuf(11 + 20 * i + .pt)
       .pt = loopvar(.pt, 0, 8, 1)
      CASE 4
       .cycle = .cycle - tastuf(11 + 20 * i + .pt)
       .pt = loopvar(.pt, 0, 8, 1)
      CASE 5
       .skip = tastuf(11 + 20 * i + .pt)
       .pt = loopvar(.pt, 0, 8, 1)
      CASE ELSE
       .pt = loopvar(.pt, 0, 8, 1)
     END SELECT
     notstuck = large(notstuck - 1, 0)
    LOOP WHILE notstuck AND .skip = 0
   END IF
  END WITH
 NEXT i
END SUB

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
 DIM spritebuf(800) AS INTEGER
 DIM pal16(288) AS INTEGER
 
 DIM itemname AS STRING
 DIM boxpreview AS STRING
 DIM scrname AS STRING
 DIM vehiclename AS STRING
 DIM caption AS STRING
 DIM appearstring AS STRING

 DIM i AS INTEGER
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

 setpicstuf spritebuf(), 1600, 2
 loadset game & ".pt4", npc(npcid * 15 + 0), 5 * npcid
 getpal16 pal16(), npcid, npc(npcid * 15 + 1), 4, npc(npcid * 15 + 0)

 itemname = load_item_name(npc(npcid * 15 + 6), 0, 0)
 boxpreview = textbox_preview_line(npc(npcid * 15 + 4))
 scrname = scriptname$(npc(npcid * 15 + 12), plottrigger)
 vehiclename = load_vehicle_name(npc(npcid * 15 + 14) - 1)

 setkeys
 DO
  setwait 100
  setkeys
  tog = tog XOR 1
  IF npc(npcid * 15 + 2) > 0 THEN walk = walk + 1: IF walk > 3 THEN walk = 0
  IF keyval(1) > 1 THEN EXIT DO
  usemenu state
  SELECT CASE state.pt
   CASE 12'--script
    IF enter_or_space() THEN
     scrname = scriptbrowse$(npc(npcid * 15 + 12), plottrigger, "NPC use plotscript")
    ELSEIF scrintgrabber(npc(npcid * 15 + 12), 0, 0, 75, 77, 1, plottrigger) THEN
     scrname = scriptname$(npc(npcid * 15 + 12), plottrigger)
    END IF
   CASE 11'--one-time-use tag
    IF keyval(75) > 1 OR keyval(77) > 1 OR enter_or_space() THEN
     onetimetog npc(npcid * 15 + 11)
    END IF
   CASE 2 TO 8, IS > 12'--simple integers
    IF intgrabber(npc(npcid * 15 + state.pt), lnpc(state.pt), unpc(state.pt)) THEN
     IF state.pt = 6 THEN itemname = load_item_name(npc(npcid * 15 + 6), 0, 0)
     IF state.pt = 4 THEN boxpreview = textbox_preview_line(npc(npcid * 15 + 4))
     IF state.pt = 14 THEN vehiclename = load_vehicle_name(npc(npcid * 15 + 14) - 1)
    END IF
   CASE 9, 10'--tag conditionals
    tag_grabber npc(npcid * 15 + state.pt)
   CASE 1'--palette
    IF intgrabber(npc(npcid * 15 + state.pt), lnpc(state.pt), unpc(state.pt)) THEN
     getpal16 pal16(), npcid, npc(npcid * 15 + 1), 4, npc(npcid * 15 + 0)
    END IF
    IF enter_or_space() THEN
     npc(npcid * 15 + state.pt) = pal16browse(npc(npcid * 15 + state.pt), 8, 0, 5 * npcid, 20, 20, 2)
     getpal16 pal16(), npcid, npc(npcid * 15 + 1), 4, npc(npcid * 15 + 0) 
    END IF
   CASE 0'--picture
    IF intgrabber(npc(npcid * 15 + state.pt), lnpc(state.pt), unpc(state.pt)) = 1 THEN
     setpicstuf spritebuf(), 1600, 2
     loadset game & ".pt4", npc(npcid * 15 + 0), 5 * npcid
     getpal16 pal16(), npcid, npc(npcid * 15 + 1), 4, npc(npcid * 15 + 0)
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
   caption = " " & npc(npcid * 15 + i)
   SELECT CASE i
    CASE 1
     caption = " " & defaultint$(npc(npcid * 15 + i))
    CASE 2
     caption = " = " & movetype(npc(npcid * 15 + i))
    CASE 3
     caption = " " & stepi(npc(npcid * 15 + i))
    CASE 5
     caption = usetype(npc(npcid * 15 + i), 1)
    CASE 6
     caption = " " & itemname
    CASE 7
     caption = pushtype(npc(npcid * 15 + i))
    CASE 8
     caption = usetype(npc(npcid * 15 + i), 0)
    CASE 9, 10
     IF npc(npcid * 15 + i) THEN
      caption = " " & ABS(npc(npcid * 15 + i)) & " = " & onoroff$(npc(npcid * 15 + i)) & " (" & load_tag_name(ABS(npc(npcid * 15 + i))) & ")"
     ELSE
      caption = " 0 (N/A)"
     END IF
    CASE 11
     IF npc(npcid * 15 + i) THEN caption = " Only Once (tag " & (1000 + npc(npcid * 15 + i)) & ")" ELSE caption = " Repeatedly"
    CASE 12 'script
     caption = scrname
    CASE 13 'script arg
     IF npc(npcid * 15 + 12) = 0 THEN caption = " N/A"
    CASE 14 'vehicle
     IF npc(npcid * 15 + 14) <= 0 THEN
      caption = "No"
     ELSE
      caption = vehiclename
     END IF
   END SELECT
   printstr menucaption(i) + caption, 0, 8 + (8 * i), dpage
  NEXT i
  edgebox 9, 139, 22, 22, uilook(uiDisabledItem), uilook(uiText), dpage
  loadsprite spritebuf(), 0, 800 + (200 * INT(walk / 2)), 5 * npcid, 20, 20, 2
  drawsprite spritebuf(), 0, pal16(), 16 * npcid, 10, 140, dpage
  appearstring = "Appears if tag " & ABS(npc(npcid * 15 + 9)) & " = " & onoroff$(npc(npcid * 15 + 9)) & " and tag " & ABS(npc(npcid * 15 + 10)) & " = " & onoroff$(npc(npcid * 15 + 10))
  IF npc(npcid * 15 + 9) <> 0 AND npc(npcid * 15 + 10) = 0 THEN appearstring = "Appears if tag " & ABS(npc(npcid * 15 + 9)) & " = " & onoroff$(npc(npcid * 15 + 9))
  IF npc(npcid * 15 + 9) = 0 AND npc(npcid * 15 + 10) <> 0 THEN appearstring = "Appears if tag " & ABS(npc(npcid * 15 + 10)) & " = " & onoroff$(npc(npcid * 15 + 10))
  IF npc(npcid * 15 + 9) = 0 AND npc(npcid * 15 + 10) = 0 THEN appearstring = "Appears all the time"
  textcolor uilook(uiSelectedItem2), 0
  printstr appearstring, 0, 190, dpage
  textcolor uilook(uiSelectedItem2), uiLook(uiHighlight)
  printstr boxpreview, 0, 170, dpage
  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
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
 DIM boxbuf(dimbinsize(binSAY))
 LoadTextBox boxbuf(), boxnum
 DIM s AS STRING
 DIM i AS INTEGER
 FOR i = 0 TO 7
  s = STRING$(38, 0)
  array2str boxbuf(), i * 38, s
  s = TRIM(s)
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
