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
#include "cglobals.bi"

#include "customsubs.bi"

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
  textcolor 7, 8
  IF (i MOD linesize) = (pt MOD linesize) OR (i \ linesize) = (pt \ linesize) THEN textcolor 7, 1
  IF pt = i THEN textcolor 14 + tog, 0
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
