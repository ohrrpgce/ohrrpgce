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
    IF keyval(scEnter) > 1 THEN
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
IF keyval(scBackspace) > 1 AND LEN(s) > 0 THEN s = LEFT(s, LEN(s) - 1)

'--copy support
IF (keyval(scCtrl) > 0 AND keyval(scInsert) > 1) OR ((keyval(scLeftShift) > 0 OR keyval(scRightShift) > 0) AND keyval(scDelete) > 0) OR (keyval(scCtrl) > 0 AND keyval(scC) > 1) THEN clip = s

'--paste support
IF ((keyval(scLeftShift) > 0 OR keyval(scRightShift) > 0) AND keyval(scInsert) > 1) OR (keyval(scCtrl) > 0 AND keyval(scV) > 1) THEN s = LEFT(clip, maxl)

'--SHIFT support
shift = 0
IF keyval(scRightShift) > 0 OR keyval(scLeftShift) > 0 THEN shift = 1

'--ALT support
IF keyval(scAlt) THEN shift = shift + 2

'--adding chars
IF LEN(s) < maxl THEN

 IF keyval(scSpace) > 1 THEN
  IF keyval(scCtrl) = 0 THEN
   '--SPACE support
   s = s + " "
  ELSE
   '--charlist support
   s = s + charpicker
  END IF
 ELSE
  IF keyval(scCtrl) = 0 THEN
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

 IF keyval(scUp) > 1 THEN pt = large(pt - linesize, 0)
 IF keyval(scDown) > 1 THEN pt = small(pt + linesize, last)
 IF keyval(scLeft) > 1 THEN pt = large(pt - 1, 0)
 IF keyval(scRight) > 1 THEN pt = small(pt + 1, last)

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
 ClearMenuData sample_menu
 WITH sample_menu
  .anchor.x = 1
  .anchor.y = -1
  .offset.x = 156
  .offset.y = -96
 END WITH
 append_menu_item sample_menu, "Sample"
 append_menu_item sample_menu, "Example"
 append_menu_item sample_menu, "Disabled"
 sample_menu.last->disabled = YES
 
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

  IF index >= 0 THEN
   IF keyval(scCtrl) > 0 AND keyval(scD) > 1 THEN ' Ctrl+D
    uilook(index) = default_colors(index)
    make_ui_color_editor_menu color_menu(), uilook()
   END IF
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

  IF keyval(scUp) > 1 THEN cursor.y = loopvar(cursor.y, 0, 15, -1)
  IF keyval(scDown) > 1 THEN cursor.y = loopvar(cursor.y, 0, 15, 1)
  IF keyval(scLeft) > 1 THEN cursor.x = loopvar(cursor.x, 0, 15, -1)
  IF keyval(scRight) > 1 THEN cursor.x = loopvar(cursor.x, 0, 15, 1)

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

'Asks a yes-or-no pop-up question.
'(Not to be confused with yesorno(), which returns a yes/no string)
FUNCTION yesno(capt AS STRING, BYVAL defaultval AS INTEGER=YES, escval AS INTEGER=NO) AS INTEGER
 IF defaultval THEN defaultval = 0 ELSE defaultval = 1
 IF escval THEN escval = 0 ELSE escval = 1
 DIM result AS INTEGER
 result = twochoice(capt, "Yes", "No", defaultval, escval)
 IF result = 0 THEN RETURN YES
 IF result = 1 THEN RETURN NO
END FUNCTION

FUNCTION twochoice(capt AS STRING, strA AS STRING="Yes", strB AS STRING="No", defaultval AS INTEGER=0, escval AS INTEGER=-1, helpkey AS STRING="") AS INTEGER
 DIM state AS MenuState
 DIM menu AS MenuDef
 ClearMenuData menu
 DIM result AS INTEGER
 DIM captlines() AS STRING
 DIM wide AS INTEGER

 split(wordwrap(capt, 37), captlines())
 FOR i AS INTEGER = 0 TO UBOUND(captlines)
  wide = large(wide, LEN(captlines(i)))
 NEXT

 append_menu_item menu, strA
 append_menu_item menu, strB

 state.active = YES
 init_menu_state state, menu
 state.pt = defaultval
 menu.offset.Y = 5 * UBOUND(captlines)

 'Keep whatever was on the screen already as a background
 copypage vpage, dpage
 
 setkeys
 DO
  setwait 55
  setkeys

  IF keyval(scEsc) > 1 THEN
   result = escval
   state.active = NO
  END IF

  IF keyval(scF1) > 1 ANDALSO LEN(helpkey) > 0 THEN
   show_help helpkey
  END IF

  IF enter_or_space() THEN
   result = state.pt
   state.active = NO
  END IF

  IF state.active = NO THEN EXIT DO
  
  usemenu state

  centerbox 160, 70, 16 + wide * 8, 16 + 10 * UBOUND(captlines), 2, 0
  FOR i AS INTEGER = 0 TO UBOUND(captlines)
   edgeprint captlines(i), xstring(captlines(i), 160), 65 - 5 * UBOUND(captlines) + i * 10, uilook(uiMenuItem), 0
  NEXT
  draw_menu menu, state, 0
  IF LEN(helpkey) > 0 THEN
   edgeprint "F1 Help", 0, 190, uilook(uiMenuItem), dpage
  END IF
  setvispage 0
  copypage 1, 0
  dowait
 LOOP
 setkeys

 RETURN result
END FUNCTION

SUB pop_warning(s AS STRING)
 
 '--Construct the warning UI (This will be hella easier later when the Slice Editor can save/load)
 DIM root AS Slice Ptr
 root = NewSliceOfType(slRoot)
 WITH *root
  .Y = 200
  .Fill = NO
 END WITH
 DIM outer_box AS Slice Ptr
 outer_box = NewSliceOfType(slContainer, root)
 WITH *outer_box
  .paddingTop = 20
  .paddingBottom = 20
  .paddingLeft = 20
  .paddingRight = 20
  .Fill = Yes
 END WITH
 DIM inner_box AS Slice Ptr
 inner_box = NewSliceOfType(slRectangle, outer_box)
 WITH *inner_box
  .paddingTop = 8
  .paddingBottom = 8
  .paddingLeft = 8
  .paddingRight = 8
  .Fill = YES
  ChangeRectangleSlice inner_box, 2
 END WITH
 DIM text_area AS Slice Ptr
 text_area = NewSliceOfType(slText, inner_box)
 WITH *text_area
  .Fill = YES
  ChangeTextSlice text_area, s, , , YES
 END WITH
 DIM animate AS Slice Ptr
 animate = root

 '--Preserve whatever screen was already showing as a background
 DIM holdscreen AS INTEGER
 holdscreen = allocatepage
 copypage vpage, holdscreen
 copypage vpage, dpage

 DIM dat AS TextSliceData Ptr
 dat = text_area->SliceData
 dat->line_limit = 15

 DIM deadkeys AS INTEGER = 25
 DIM cursor_line AS INTEGER = 0
 DIM scrollbar_state AS MenuState
 scrollbar_state.size = 16

 '--Now loop displaying text
 setkeys
 DO
  setwait 17, 70
  setkeys
  
  IF deadkeys = 0 THEN 
   IF keyval(scESC) > 1 OR enter_or_space() THEN EXIT DO
   IF keyval(scUp) > 1 THEN dat->first_line -= 1
   IF keyval(scDown) > 1 THEN dat->first_line += 1
   dat->first_line = bound(dat->first_line, 0, large(0, dat->line_count - dat->line_limit))
  END IF
  deadkeys = large(deadkeys -1, 0)

  'Animate the arrival of the pop-up
  animate->Y = large(animate->Y - 20, 0)

  DrawSlice root, dpage
  
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

 '--Animate the removal of the help screen
 DO
  setkeys
  setwait 17, 70
  animate->Y = animate->Y + 20
  IF animate->Y > 200 THEN EXIT DO
  DrawSlice root, dpage
  SWAP vpage, dpage
  setvispage vpage
  copypage holdscreen, dpage
  dowait
 LOOP
  
 freepage holdscreen
 DeleteSlice @root
END SUB

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
 '
 'WARNING!: if your text editor converts these to UTF, it will break the
 'font editor and the ALT+letter ALT+SHIFT+letter feature of string entry!
 DIM keyconst(207) AS STRING = {"1","2","3","4","5","6","7","8","9","0","-","=","","","q","w","e","r","t","y","u","i","o","p","[","]","","","a","s","d","f","g","h","j","k","l",";","'","`","","\","z","x","c","v","b","n","m",",",".","/", _
  "!","@","#","$","%","^","&","*","(",")","_","+","","","Q","W","E","R","T","Y","U","I","O","P","{","}","","","A","S","D","F","G","H","J","K","L",":"," ","~","","|","Z","X","C","V","B","N","M","<",">","?", _
  !"\130",!"\131",!"\132",!"\133",!"\134",!"\135",!"\136",!"\137",!"\138",!"\139",!"\140",!"\141","","",!"\142",!"\143",!"\144",!"\145",!"\146",!"\147",!"\148",!"\149",!"\150",!"\151",!"\152",!"\153","","",!"\154",!"\155",!"\156",!"\157",!"\158",!"\159",!"\160",!"\161",!"\162",!"\163",!"\164",!"\165","",!"\166",!"\167",!"\168",!"\169",!"\170",!"\171",!"\172",!"\173",!"\174",!"\175",!"\176", _
  !"\177",!"\178",!"\179",!"\180",!"\181",!"\182",!"\183",!"\184",!"\185",!"\186",!"\187",!"\188","","",!"\189",!"\190",!"\191",!"\192",!"\193",!"\194",!"\195",!"\196",!"\197",!"\198",!"\199",!"\200","","",!"\201",!"\202",!"\203",!"\204",!"\205",!"\206",!"\207",!"\208",!"\209",!"\210",!"\211",!"\212","",!"\213",!"\214",!"\215",!"\216",!"\217",!"\218",!"\219",!"\220",!"\221",!"\222",!"\223"}
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
 DIM menucaption(15) AS STRING

 DIM state AS MenuState
 state.size = 24
 state.first = -1
 state.last = 14
 state.top = -1
 state.pt = -1

 'lower and upper data limits
 unpc(0) = gen(genMaxNPCPic)
 unpc(1) = 32767
 unpc(2) = 8
 unpc(3) = 5
 unpc(4) = gen(genMaxTextbox)       'max text boxes
 unpc(5) = 2
 unpc(6) = gen(genMaxItem) + 1
 unpc(7) = 7
 unpc(8) = 2
 unpc(9) = 999
 unpc(10) = 999
 unpc(11) = 1
 unpc(12) = gen(genMaxRegularScript)'max scripts
 unpc(13) = 32767
 unpc(14) = gen(genMaxVehicle) + 1  'max vehicles

 FOR i = 0 TO 14
  lnpc(i) = 0
 NEXT i
 lnpc(1) = -1
 lnpc(9) = -999
 lnpc(10) = -999
 lnpc(13) = -32767

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
 DIM movetype(8) AS STRING
 movetype(0) = "Stand Still"
 movetype(1) = "Wander"
 movetype(2) = "Pace"
 movetype(3) = "Right Turns"
 movetype(4) = "Left Turns"
 movetype(5) = "Random Turns"
 movetype(6) = "Chase You"
 movetype(7) = "Avoid You"
 movetype(8) = "Walk In Place"
 DIM pushtype(7) AS STRING
 pushtype(0) = "Off"
 pushtype(1) = "Full"
 pushtype(2) = "Vertical"
 pushtype(3) = "Horizontal"
 pushtype(4) = "Up only"
 pushtype(5) = "Right Only"
 pushtype(6) = "Down Only"
 pushtype(7) = "Left Only"
 DIM usetype(2) AS STRING
 usetype(0) = "Use"
 usetype(1) = "Touch"
 usetype(2) = "Step On"
 DIM facetype(2) AS STRING
 facetype(0) = "Change Direction"
 facetype(1) = "Face Player"
 facetype(2) = "Do Not Face Player"

 npcdata.sprite = frame_load(4, npcdata.picture)
 npcdata.pal = palette16_load(npcdata.palette, 4, npcdata.picture)

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
     frame_unload @npcdata.sprite
     palette16_unload @npcdata.pal
     npcdata.sprite = frame_load(4, npcdata.picture)
     npcdata.pal = palette16_load(npcdata.palette, 4, npcdata.picture)
    END IF
   CASE 1'--palette
    IF intgrabber(npcdata.palette, lnpc(state.pt), unpc(state.pt)) THEN
     palette16_unload @npcdata.pal
     npcdata.pal = palette16_load(npcdata.palette, 4, npcdata.picture)
    END IF
    IF enter_or_space() THEN
     npcdata.palette = pal16browse(npcdata.palette, 4, npcdata.picture)
     palette16_unload @npcdata.pal
     npcdata.pal = palette16_load(npcdata.palette, 4, npcdata.picture)
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
    IF keyval(scLeft) > 1 OR keyval(scRight) > 1 OR enter_or_space() THEN
     onetimetog npcdata.usetag
    END IF
   CASE 12'--script
    IF enter_or_space() THEN
     scrname = scriptbrowse_string(npcdata.script, plottrigger, "NPC use plotscript")
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
     caption = " = " & safe_caption(movetype(), npcdata.movetype, "movetype")
    CASE 3
     caption = " " & npcdata.speed
    CASE 4
     caption = " " & zero_default(npcdata.textbox, "[None]")
    CASE 5
     caption = " " & safe_caption(facetype(), npcdata.facetype, "facetype")
    CASE 6
     caption = " " & itemname
    CASE 7
     caption = " " & safe_caption(pushtype(), npcdata.pushtype, "pushtype")
    CASE 8
     caption = safe_caption(usetype(), npcdata.activation, "usetype")
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
  frame_draw npcdata.sprite + 4 + (walk \ 2), npcdata.pal, 10, 140, 1, YES, dpage
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

 frame_unload @npcdata.sprite
 palette16_unload @npcdata.pal
END SUB

FUNCTION load_vehicle_name(vehID AS INTEGER) AS STRING
 IF vehID < 0 OR vehID > gen(genMaxVehicle) THEN RETURN ""
 DIM vehicle AS VehicleData
 LoadVehicle game + ".veh", vehicle, vehID
 RETURN vehicle.name
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
  gen(genOneTimeNPC) = loopvar(gen(genOneTimeNPC), 0, 999, 1)
  i = i + 1: IF i > 1000 THEN EXIT SUB 'Revisit this later
 LOOP UNTIL readbit(gen(), 106, gen(genOneTimeNPC)) = 0
 tagnum = gen(genOneTimeNPC) + 1
 setbit gen(), 106, gen(genOneTimeNPC), 1
END SUB

FUNCTION pal16browse (BYVAL curpal AS INTEGER, BYVAL picset AS INTEGER, BYVAL picnum AS INTEGER) AS INTEGER

 DIM buf(7) AS INTEGER
 DIM sprite(9) AS Frame PTR
 DIM pal16(9) AS Palette16 PTR

 DIM AS INTEGER i, o, j, k
 DIM c AS INTEGER

 DIM state AS MenuState
 state.need_update = YES
 state.top = curpal - 1
 state.first = -1
 state.size = 9

 '--get last pal
 loadrecord buf(), game + ".pal", 8, 0
 state.last = buf(1) + 1
 FOR i = state.last TO 0 STEP -1
  state.last = i
  loadrecord buf(), game + ".pal", 8, 1 + i
  FOR j = 0 TO 7
   IF buf(j) <> 0 THEN EXIT FOR, FOR
  NEXT j
 NEXT i

 state.pt = bound(curpal, 0, state.last)
 state.top = bound(state.top, state.first, large(state.last - state.size, state.first))

 clearpage dpage
 'reset repeat rate, needed because called from sprite editor (argh), the caller resets its own repeatrate
 setkeyrepeat
 setkeys
 DO
  setwait 55
  setkeys
  state.tog = state.tog XOR 1
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "pal16browse"
  IF usemenu(state) THEN state.need_update = YES
  IF enter_or_space() THEN
   IF state.pt >= 0 THEN curpal = state.pt
   EXIT DO
  END IF

  IF state.need_update THEN
   state.need_update = NO
   FOR i = 0 TO 9
    frame_unload @sprite(i)
    palette16_unload @pal16(i)
    sprite(i) = frame_load(picset, picnum)
    IF state.top + i <= gen(genMaxPal) THEN pal16(i) = palette16_load(state.top + i, picset, picnum)
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
       WITH sprite_sizes(picset)
        FOR k = 0 TO .frames - 1
         frame_draw sprite(i) + k, pal16(i), o + 140 + (k * .size.x), i * 20 - (.size.y \ 2 - 10), 1, YES, dpage
        NEXT k
       END WITH
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
    WITH sprite_sizes(picset)
     FOR k = 0 TO .frames - 1
      frame_draw sprite(i) + k, pal16(i), o + 130 + (k * .size.x), i * 20 - (.size.y \ 2 - 10), 1, YES, dpage
     NEXT k
    END WITH
   END IF
  END IF
 
  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP

 FOR i = 0 TO 9
  frame_unload @sprite(i)
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

FUNCTION seconds_estimate(ticks AS INTEGER) AS STRING
 DIM sec AS SINGLE
 sec = ticks * (1 / 18.2)
 RETURN STR(INT(sec * 10) / 10)
END FUNCTION

SUB load_text_box_portrait (BYREF box AS TextBox, BYREF gfx AS GraphicPair)
 'WARNING: There is another version of this in yetmore.bas
 'If you update this here, make sure to update that one too!
 DIM img_id AS INTEGER = -1
 DIM pal_id AS INTEGER = -1
 DIM her AS HeroDef
 WITH gfx
  IF .sprite THEN frame_unload @.sprite
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
   .sprite = frame_load(8, img_id)
   .pal    = palette16_load(pal_id, 8, img_id)
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
   CASE 32, 48 TO 57, 65 TO 90, 97 TO 122, 95, 126, 45  '[ 0-9A-Za-z_~-]
    result = result & ch
  END SELECT
 NEXT i
 RETURN result
END FUNCTION

FUNCTION inputfilename (query AS STRING, ext AS STRING, directory AS STRING, helpkey AS STRING, default AS STRING="", allow_overwrite AS INTEGER=YES) AS STRING
 DIM filename AS STRING = default
 DIM tog AS INTEGER
 IF directory = "" THEN directory = CURDIR
 setkeys
 DO
  setwait 55
  setkeys
  tog = tog XOR 1
  IF keyval(scEsc) > 1 THEN RETURN ""
  IF keyval(scF1) > 1 THEN show_help helpkey
  strgrabber filename, 40
  filename = fixfilename(filename)
  IF keyval(scEnter) > 1 THEN
   filename = TRIM(filename)
   IF filename <> "" THEN
    IF isfile(directory + SLASH + filename + ext) THEN
     If allow_overwrite THEN
      IF yesno("File already exists, overwrite?") THEN RETURN directory + SLASH + filename
     ELSE
      notification filename & ext & " already exists"
     END IF
    ELSE
     RETURN directory + SLASH + filename
    END IF
   END IF
  END IF
  textcolor uilook(uiText), 0
  printstr query, 160 - LEN(query) * 4, 20, dpage
  printstr "Output directory: ", 160 - 18 * 4, 35, dpage
  printstr directory, xstring(directory, 160), 45, dpage
  textcolor uilook(uiSelectedItem + tog), 1
  printstr filename, 160 - LEN(filename & ext) * 4 , 60, dpage
  textcolor uilook(uiText), uilook(uiHighlight)
  printstr ext, 160 + (LEN(filename) - LEN(ext)) * 4 , 60, dpage
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
    PRINT #fh, "Music: " & box.music & " (" & escape_nonprintable_ascii(getsongname(box.music - 1)) & ")"
   ELSE
    PRINT #fh, "Music: " & box.music & " (None)"
   END IF
   PRINT #fh, "Restore Music: " & yesorno(box.restore_music)
   IF box.sound_effect > 0 THEN
    PRINT #fh, "Sound Effect: " & box.sound_effect & " (" & escape_nonprintable_ascii(getsfxname(box.sound_effect - 1)) & ")"
   ELSE
    PRINT #fh, "Sound Effect: " & box.sound_effect & " (None)"
   END IF
   PRINT #fh, "Stop Sound After Box: " & yesorno(box.stop_sound_after)
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
       CASE "sound effect"
        box.sound_effect = VALINT(v)
       CASE "stop sound after box"
        box.stop_sound_after = str2bool(v)
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

SUB xy_position_on_slice (sl AS Slice Ptr, BYREF x AS INTEGER, BYREF y AS INTEGER, caption AS STRING, helpkey AS STRING)
 DIM col AS INTEGER
 DIM tog AS INTEGER
 DIM root AS Slice Ptr
 
 setkeys
 DO
  setwait 55
  setkeys
  tog = tog XOR 1

  IF keyval(scEsc) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help helpkey
  IF enter_or_space() THEN EXIT DO
  IF keyval(scLeft) > 0  THEN x -= 1
  IF keyval(scRight) > 0 THEN x += 1
  IF keyval(scUp) > 0    THEN y -= 1
  IF keyval(scDown) > 0  THEN y += 1

  DrawSlice sl, dpage
  col = uilook(uiBackground)
  IF tog = 0 THEN col = uilook(uiSelectedItem)
  rectangle sl->ScreenX + x - 2, sl->ScreenY + y, 2, 2, col, dpage
  rectangle sl->ScreenX + x + 2, sl->ScreenY + y, 2, 2, col, dpage
  rectangle sl->ScreenX + x, sl->ScreenY + y - 2, 2, 2, col, dpage
  rectangle sl->ScreenX + x, sl->ScreenY + y + 2, 2, 2, col, dpage

  edgeprint caption, xstring(caption, 160), 0, uilook(uiText), dpage
  edgeprint "Position point and press Enter or SPACE", 0, 190, uilook(uiText), dpage

  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
END SUB

SUB xy_position_on_sprite (spr AS GraphicPair, BYREF x AS INTEGER, BYREF y AS INTEGER, BYVAL frame AS INTEGER, BYVAL wide AS INTEGER, byval high AS INTEGER, caption AS STRING, helpkey AS STRING)
 DIM col AS INTEGER
 DIM tog AS INTEGER
 setkeys
 DO
  setwait 55
  setkeys
  tog = tog XOR 1

  IF keyval(scEsc) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help helpkey
  IF enter_or_space() THEN EXIT DO
  IF keyval(scLeft) > 0  THEN x -= 1
  IF keyval(scRight) > 0 THEN x += 1
  IF keyval(scUp) > 0    THEN y -= 1
  IF keyval(scDown) > 0  THEN y += 1

  emptybox 160 - wide, 100 - high, wide * 2, high * 2, uilook(uiSelectedDisabled), 1, dpage
  frame_draw spr.sprite + frame, spr.pal, 160 - wide, 100 - high, 2,, dpage
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
 editbitset bits(), 0, UBOUND(bitname), bitname(), "menu_editor_bitsets"
 MenuBitsFromArray menu, bits()  
END SUB

SUB edit_menu_item_bits (mi AS MenuDefItem)
 DIM bitname(1) AS STRING
 DIM bits(0) AS INTEGER
 
 bitname(0) = "Hide if disabled"
 bitname(1) = "Close menu if selected"

 MenuItemBitsToArray mi, bits()
 editbitset bits(), 0, UBOUND(bitname), bitname(), "menu_editor_item_bitsets"
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
  
  shift = ABS(keyval(scLeftShift) > 0 OR keyval(scRightShift) > 0)
  WITH menu.offset
   IF keyval(scUp) > 1 THEN .y -= 1 + 9 * shift
   IF keyval(scDown) > 1 THEN .y += 1 + 9 * shift
   IF keyval(scLeft) > 1 THEN .x -= 1 + 9 * shift
   IF keyval(scRight) > 1 THEN .x += 1 + 9 * shift
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
   IF keyval(scUp) > 1 THEN .y = bound(.y - 1, -1, 1)
   IF keyval(scDown) > 1 THEN .y = bound(.y + 1, -1, 1)
   IF keyval(scLeft) > 1 THEN .x = bound(.x - 1, -1, 1)
   IF keyval(scRight) > 1 THEN .x = bound(.x + 1, -1, 1)
  END WITH
 
  draw_menu menu, mstate, dpage
  WITH menu
   x = .rect.x - 2 + anchor_point(.anchor.x, .rect.wide)
   y = .rect.y - 2 + anchor_point(.anchor.y, .rect.high)
   rectangle x, y, 5, 5, 2 + tog, dpage 
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

SUB editbitset (array() AS INTEGER, BYVAL wof AS INTEGER, BYVAL last AS INTEGER, names() AS STRING, helpkey AS STRING="editbitset")

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
  IF keyval(scF1) > 1 THEN show_help helpkey
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

SUB scriptbrowse (BYREF trigger AS INTEGER, BYVAL triggertype AS INTEGER, scrtype AS STRING)
 'For when you don't care about the return value of scriptbrowse_string()
 DIM s AS STRING
 s = scriptbrowse_string(trigger, triggertype, scrtype)
END SUB

FUNCTION scriptbrowse_string (BYREF trigger AS INTEGER, BYVAL triggertype AS INTEGER, scrtype AS STRING) AS STRING
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
  numberedlast = firstscript + gen(genNumPlotscripts) - 1

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

 DIM buf(19), plotids(gen(genMaxRegularScript))
 DIM recordsloaded AS INTEGER = 0
 DIM screxists AS INTEGER = 0

 DIM fh AS INTEGER = FREEFILE
 OPEN workingdir & SLASH & "lookup" & triggertype & ".bin" FOR BINARY AS #fh
 DIM triggernum AS INTEGER = LOF(fh) \ 40
 IF temp = -1 THEN temp = triggernum + 16384

 DO
  temp += seekdir
  IF temp > gen(genMaxRegularScript) AND temp < 16384 THEN
   IF seekdir > 0 THEN
    temp = 16384
   ELSEIF triggertype = plottrigger THEN
    temp = gen(genMaxRegularScript)
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
    WHILE recordsloaded < gen(genNumPlotscripts)
     loadrecord buf(), workingdir + SLASH + "plotscr.lst", 20, recordsloaded
     recordsloaded += 1
     IF buf(0) = temp THEN screxists = -1: EXIT WHILE
     IF buf(0) <= gen(genMaxRegularScript) THEN plotids(buf(0)) = -1
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
 copypage vpage, dpage

 DIM dat AS TextSliceData Ptr
 dat = help_text->SliceData
 dat->line_limit = 18

 DIM editing AS INTEGER = NO
 DIM deadkeys AS INTEGER = 25
 DIM cursor_line AS INTEGER = 0
 DIM scrollbar_state AS MenuState
 scrollbar_state.size = 17

 '--Now loop displaying help
 setkeys
 DO
  setwait 17, 70
  setkeys
  
  IF editing THEN  
   cursor_line = stredit(dat->s, 32767, dat->line_limit, INT(help_text->Width / 8))
   'The limit of 32767 chars is totally arbitrary and maybe not a good limit
   dat->insert = insert '--copy the global stredit() insert point
  END IF

  IF deadkeys = 0 THEN 
   IF keyval(scESC) > 1 THEN EXIT DO
   IF keyval(scE) > 1 THEN
    IF fileiswriteable(get_help_dir() & SLASH & helpkey & ".txt") THEN
     editing = YES
     dat->show_insert = YES
     dat->insert = insert '--copy the global stredit() insert point
     ChangeRectangleSlice help_box, , uilook(uiBackground), , 0
    ELSE
     pop_warning "Your """ & get_help_dir() & """ folder is not writeable. Try making a copy of it at """ & homedir & SLASH & "ohrhelp"""
    END IF
   END IF
   IF keyval(scF1) and helpkey <> "helphelp" THEN
    show_help "helphelp"
   END IF
   IF editing THEN
    dat->first_line = small(dat->first_line, cursor_line - 1)
    dat->first_line = large(dat->first_line, cursor_line - (dat->line_limit - 2))
   ELSE
    '--not editing, just browsing
    IF keyval(scUp) > 1 THEN dat->first_line -= 1
    IF keyval(scDown) > 1 THEN dat->first_line += 1
    IF keyval(scPageUp) > 1 THEN dat->first_line -= dat->line_limit - 1
    IF keyval(scPageDown) > 1 THEN dat->first_line += dat->line_limit - 1
    IF keyval(scHome) > 1 THEN dat->first_line = 0
    IF keyval(scEnd) > 1 THEN dat->first_line = dat->line_count
   END IF
   dat->first_line = bound(dat->first_line, 0, large(0, dat->line_count - dat->line_limit))
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
  IF yesno("Save changes to help for """ & helpkey & """?", YES, YES) THEN
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

'--For each script trigger datum in the game, call visitor (whether or not there
'--is a script set there; however fields which specify either a script or
'--something else, eg. either a script or a textbox, may be skipped)
SUB visit_scripts(byval visitor as FnScriptVisitor)
 DIM AS INTEGER i, j, idtmp, resave

 '--global scripts
 visitor(gen(genNewGameScript), "new game", "")
 visitor(gen(genLoadGameScript), "load game", "")
 visitor(gen(genGameoverScript), "game over", "")

 '--Text box scripts
 DIM box AS TextBox
 FOR i AS INTEGER = 0 TO gen(genMaxTextbox)
  LoadTextBox box, i
  resave = NO
  IF box.instead < 0 THEN
   idtmp = -box.instead
   resave OR= visitor(idtmp, "box " & i & " (instead)", textbox_preview_line(box))
   box.instead = -idtmp
  END IF
  IF box.after < 0 THEN
   idtmp = -box.after
   resave OR= visitor(idtmp, "box " & i & " (after)", textbox_preview_line(box))
   box.after = -idtmp
  END IF
  IF resave THEN
   SaveTextBox box, i
  END IF
 NEXT i
 
 '--Map scripts and NPC scripts
 DIM gmaptmp(dimbinsize(binMAP))
 REDIM npctmp(0) AS NPCType
 FOR i = 0 TO gen(genMaxMap)
  resave = NO
  loadrecord gmaptmp(), game & ".map", dimbinsize(binMAP), i
  resave OR= visitor(gmaptmp(7), "map " & i & " autorun", "")
  resave OR= visitor(gmaptmp(12), "map " & i & " after-battle", "")
  resave OR= visitor(gmaptmp(13), "map " & i & " instead-of-battle", "")
  resave OR= visitor(gmaptmp(14), "map " & i & " each-step", "")
  resave OR= visitor(gmaptmp(15), "map " & i & " on-keypress", "")
  IF resave THEN
   storerecord gmaptmp(), game & ".map", dimbinsize(binMAP), i
  END IF
  'loop through NPC's
  LoadNPCD maplumpname(i, "n"), npctmp()
  resave = NO
  FOR j = 0 TO UBOUND(npctmp)
   resave OR= visitor(npctmp(j).script, "map " & i & " NPC " & j, "")
  NEXT j
  IF resave THEN
   SaveNPCD maplumpname(i, "n"), npctmp()
  END IF
 NEXT i
 
 '--vehicle scripts
 DIM vehicle AS VehicleData
 FOR i = 0 TO gen(genMaxVehicle)
  resave = NO
  LoadVehicle game & ".veh", vehicle, i
  IF vehicle.use_button > 0 THEN
   resave OR= visitor(vehicle.use_button, "use button veh " & i, """" & vehicle.name & """")
  END IF
  IF vehicle.menu_button > 0 THEN
   resave OR= visitor(vehicle.menu_button, "menu button veh " & i, """" & vehicle.name & """")
  END IF
  IF vehicle.on_mount < 0 THEN
   idtmp = -(vehicle.on_mount)
   resave OR= visitor(idtmp, "mount vehicle " & i, """" & vehicle.name & """")
   vehicle.on_mount = -idtmp
  END IF
  IF vehicle.on_dismount < 0 THEN
   idtmp = -(vehicle.on_dismount)
   resave OR= visitor(idtmp, "dismount vehicle " & i,  """" & vehicle.name & """")
   vehicle.on_dismount = -idtmp
  END IF
  IF resave THEN
   SaveVehicle game & ".veh", vehicle, i
  END IF
 NEXT i
 
 '--shop scripts
 DIM shoptmp(19)
 DIM shopname AS STRING
 FOR i = 0 TO gen(genMaxShop)
  loadrecord shoptmp(), game & ".sho", 20, i
  shopname = readbadbinstring(shoptmp(), 0, 15)
  IF visitor(shoptmp(19), "show inn " & i, """" & shopname & """") THEN
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
  FOR j = 0 TO menutmp.numitems - 1
   WITH *menutmp.items[j]
    IF .t = 4 THEN
     resave OR= visitor(.sub_t, "menu " & i & " item " & j, """" & .caption & """")
    END IF
   END WITH
  NEXT j
  IF resave THEN
   SaveMenuData menu_set, menutmp, i
  END IF
 NEXT i

END SUB

'For script_usage_list and script_usage_visitor
DIM SHARED plotscript_order() AS INTEGER
DIM SHARED script_usage_menu() AS IntStrPair

PRIVATE FUNCTION script_usage_visitor(byref trig as integer, description as string, caption as string) as integer
 IF trig = 0 THEN RETURN NO
 '--See script_usage_list about rank calculation
 DIM rank AS INTEGER = trig
 IF trig >= 16384 THEN rank = 100000 + plotscript_order(trig - 16384)
 intstr_array_append script_usage_menu(), rank, "  " & description & " " & caption
 RETURN NO  'trig not modified
END FUNCTION

SUB script_usage_list ()
 DIM buf(20) AS INTEGER
 DIM id AS INTEGER
 DIM s AS STRING
 DIM fh AS INTEGER
 DIM i AS INTEGER
 'DIM t AS DOUBLE = TIMER

 'Build script_usage_menu, which is an list of menu items, initially out of order.
 'The integer in each pair is used to sort the menu items into the right order:
 'items for old-style scripts have rank = id
 'all plotscripts are ordered by name and given rank = 100000 + alphabetic rank
 'Start by adding all the script names to script_usage_menu (so that they'll
 'appear first when we do a stable sort), then add script instances.

 REDIM script_usage_menu(0)
 script_usage_menu(0).i = -1
 script_usage_menu(0).s = "back to previous menu..."

 'Loop through old-style non-autonumbered scripts
 fh = FREEFILE
 OPEN workingdir & SLASH & "plotscr.lst" FOR BINARY AS #fh
 FOR i AS INTEGER = 0 TO gen(genNumPlotscripts) - 1
  loadrecord buf(), fh, 20, i
  id = buf(0)
  IF id <= 16383 THEN
   s = id & ":" & readbinstring(buf(), 1, 38)
   intstr_array_append script_usage_menu(), id, s
  END IF
 NEXT i
 CLOSE #fh

 'Loop through new-style plotscripts

 'First, a detour: determine the alphabetic rank of each plotscript
 fh = FREEFILE
 OPEN workingdir & SLASH & "lookup1.bin" FOR BINARY AS #fh
 REDIM plotscripts(0) AS STRING
 WHILE loadrecord(buf(), fh, 20)
  s = readbinstring(buf(), 1, 38)
  str_array_append plotscripts(), s
 WEND

 'Have to skip if no plotscripts
 IF UBOUND(plotscripts) > 0 THEN
  'We must skip plotscripts(0)
  REDIM plotscript_order(UBOUND(plotscripts) - 1)
  qsort_strings_indices plotscript_order(), @plotscripts(1), UBOUND(plotscripts), sizeof(string)
  invert_permutation plotscript_order()

  'OK, now that we can calculate ranks, we can add new-style scripts
  SEEK #fh, 1
  i = 0
  WHILE loadrecord(buf(), fh, 20)
   id = buf(0)
   IF id <> 0 THEN
    s = readbinstring(buf(), 1, 38)
    intstr_array_append script_usage_menu(), 100000 + plotscript_order(i), s
   END IF
   i += 1
  WEND 
 END IF
 CLOSE #fh

 'add script instances to script_usage_menu
 visit_scripts @script_usage_visitor

 'sort, and build menu() (for standardmenu)
 DIM indices(UBOUND(script_usage_menu)) AS INTEGER
 REDIM menu(UBOUND(script_usage_menu)) AS STRING
 sort_integers_indices indices(), @script_usage_menu(0).i, UBOUND(script_usage_menu) + 1, sizeof(IntStrPair)

 DIM currentscript AS INTEGER = -1
 DIM j AS INTEGER = 0
 FOR i AS INTEGER = 0 TO UBOUND(script_usage_menu)
  WITH script_usage_menu(indices(i))
   IF MID(.s, 1, 1) = " " THEN
    'script trigger
    'Do not add triggers which are missing their scripts; those go in the other menu
    IF .i <> currentscript THEN CONTINUE FOR
   END IF
   menu(j) = .s
   j += 1
   currentscript = .i
  END WITH
 NEXT
 REDIM PRESERVE menu(j - 1)

 'Free memory
 REDIM plotscript_order(0)
 REDIM script_usage_menu(0)

 'debug "script usage in " & ((TIMER - t) * 1000) & "ms"

 DIM state AS MenuState
 state.size = 24
 state.last = UBOUND(menu)
 
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
  standardmenu menu(), state, 0, 0, dpage

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
  IF loadrecord (buf(), fname$, 20, trigger - 16384) THEN
   RETURN buf(0)
  ELSE
   debug "decodetrigger: record " & (trigger - 16384) & " could not be loaded"
  END IF
 ELSE
  '--this is an old-style script
  RETURN trigger
 END IF
END FUNCTION

'--This could be used in more places; makes sense to load plotscr.lst into a global
DIM SHARED script_ids_list() AS INTEGER

SUB load_script_ids_list()
 REDIM script_ids_list(large(0, gen(genNumPlotscripts) - 1))
 DIM buf(19) AS INTEGER
 DIM fh AS INTEGER
 fh = FREEFILE
 OPEN workingdir & SLASH & "plotscr.lst" FOR BINARY AS #fh
 FOR i AS INTEGER = 0 TO gen(genNumPlotscripts) - 1
  loadrecord buf(), fh, 20, i
  script_ids_list(i) = buf(0)
 NEXT i
 CLOSE #fh
END SUB

'--For script_broken_trigger_list and check_broken_script_trigger
DIM SHARED missing_script_trigger_list() AS STRING

PRIVATE FUNCTION check_broken_script_trigger(byref trig as integer, description as string, caption as string) as integer
 IF trig <= 0 THEN RETURN NO ' No script trigger
 '--decode script trigger
 DIM id AS INTEGER
 id = decodetrigger(trig, plottrigger)
 '--Check for missing new-style script
 IF id = 0 THEN
  str_array_append missing_script_trigger_list(), description & " " & scriptname(trig, plottrigger) & " missing. " & caption 
 ELSEIF id < 16384 THEN
  '--now check for missing old-style scripts
  IF int_array_find(script_ids_list(), id) <> -1 THEN RETURN NO 'Found okay

  str_array_append missing_script_trigger_list(), description & " ID " & id & " missing. " & caption
 END IF
 RETURN NO
END FUNCTION

SUB script_broken_trigger_list()
 'Cache plotscr.lst
 load_script_ids_list

 REDIM missing_script_trigger_list(0) AS STRING
 missing_script_trigger_list(0) = "back to previous menu..."

 visit_scripts @check_broken_script_trigger

 IF UBOUND(missing_script_trigger_list) = 0 THEN
  str_array_append missing_script_trigger_list(), "No broken triggers found!"
 END IF

 DIM state AS MenuState
 state.size = 24
 state.last = UBOUND(missing_script_trigger_list)

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
  standardmenu missing_script_trigger_list(), state, 0, 0, dpage

  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP 
 'Free memory
 REDIM missing_script_trigger_list(0)
END SUB

FUNCTION autofix_old_script_visitor(byref id as integer, description as string, caption as string) as integer
 '--returns true if a fix has occured
 IF id = 0 THEN RETURN NO ' not a trigger
 IF id >= 16384 THEN RETURN NO 'New-style script
 IF int_array_find(script_ids_list(), id) <> -1 THEN RETURN NO 'Found okay

 DIM buf(19) AS INTEGER
 DIM fh AS INTEGER
  
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
 OPEN workingdir & SLASH & "lookup1.bin" FOR BINARY AS #fh
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
  EXIT SUB
 END IF

 'Cache plotscr.lst
 load_script_ids_list()

 visit_scripts @autofix_old_script_visitor
END SUB

FUNCTION stredit (s AS STRING, BYVAL maxl AS INTEGER, BYVAL numlines AS INTEGER=1, BYVAL wrapchars AS INTEGER=1) AS INTEGER
 'Return value is the line that the cursor is on, or 0 if numlines=1
 stredit = 0
 
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
  IF keyval(scLeft) > 1 THEN 'move by word
   IF insert > 0 THEN 'searching from position -1 searches from the end
    insert = INSTRREV(s, ANY !" \n", insert - 1)  'different argument order: the FB devs, they are so funny
   END IF
  END IF
  IF keyval(scRight) > 1 THEN
   insert = INSTR(insert + 1, s, ANY !" \n")
   IF insert = 0 THEN insert = LEN(s)
  END IF
  IF keyval(scHome) > 1 THEN insert = 0
  IF keyval(scEnd) > 1 THEN insert = LEN(s)
 END IF

 '--up and down arrow keys
 IF numlines > 1 THEN
  DIM wrapped AS STRING
  wrapped = wordwrap(s, large(1, wrapchars))
  DIM lines() AS STRING
  split(wrapped, lines())
  DIM count AS INTEGER = 0
  DIM found_insert AS INTEGER = -1
  DIM line_chars AS INTEGER
  DIM move_lines AS INTEGER = 0
  FOR i AS INTEGER = 0 TO UBOUND(lines)
   IF count + LEN(lines(i)) >= insert THEN
    found_insert = i
    line_chars = insert - count
    EXIT FOR
   END IF
   count += LEN(lines(i)) + 1
  NEXT i
  IF found_insert >= 0 THEN
   '--set return value
   stredit = found_insert
   IF keyval(scUp) > 1 THEN move_lines = -1
   IF keyval(scDown) > 1 THEN move_lines = 1
   IF keyval(scPageUp) > 1 THEN move_lines = -(numlines - 2)
   IF keyval(scPageDown) > 1 THEN move_lines = numlines - 2
   IF move_lines THEN
    found_insert = bound(found_insert + move_lines, 0, UBOUND(lines) - 1)
    insert = 0
    FOR i AS INTEGER = 0 TO found_insert - 1
     insert += LEN(lines(i)) + 1
    NEXT i
    insert += small(line_chars, LEN(lines(found_insert)))
    '--set return value
    stredit = found_insert
   END IF
   '--end of special handling for up and down motion
  END IF
  '--Home and end keys: go to previous/next newline,
  '--unless Ctrl is pressed, which is handled above
  IF keyval(scCtrl) = 0 THEN
   IF keyval(scHome) > 1 THEN
    IF insert > 0 THEN 'searching from position -1 searches from the end
     insert = INSTRREV(s, CHR(10), insert - 1)
    END IF
   END IF
   IF keyval(scEnd) > 1 THEN
    insert = INSTR(insert + 1, s, CHR(10))
    IF insert = 0 THEN insert = LEN(s)
   END IF
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
  ELSEIF numlines > 1 AND keyval(scEnter) > 1 THEN
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
 
END FUNCTION

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

SUB edit_global_text_strings()
 DIM max AS INTEGER = 123
 DIM text(-1 TO max) AS STRING
 DIM names(-1 TO max) AS STRING
 DIM maxlen(max) AS INTEGER
 DIM search AS STRING = ""
 DIM found AS INTEGER = NO
 DIM state AS MenuState
 DIM rect AS RectType
 rect.wide = 320
 rect.high = 192

 clearpage 0
 clearpage 1

 FOR i AS INTEGER = 0 TO max
  SELECT CASE i
   CASE 55, 74 TO 76, 78, 80 TO 86, 88 TO 92, 97 TO 98, 106 TO 115, 123
    maxlen(i) = 20
   CASE 39, 40
    maxlen(i) = 8
   CASE 94 TO 96, 105
    maxlen(i) = 30
   CASE ELSE
    maxlen(i) = 10
  END SELECT
 NEXT i

 '--load current names

 names(-1) = "Back to Previous Menu" : text(-1) = ""
 names(0) = "Health Points":            text(0) = readglobalstring(0, "HP", 10)
 names(1) = "Spell Points":             text(1) = readglobalstring(1, "MP", 10)
 names(2) = "Attack Power":             text(2) = readglobalstring(2, "Attack", 10)
 names(3) = "Accuracy":                 text(3) = readglobalstring(3, "Accuracy", 10)
 names(4) = "Extra Hits":               text(4) = readglobalstring(4, "Hits", 10)
 names(5) = "Blocking Power":           text(5) = readglobalstring(5, "Blocking", 10)
 names(6) = "Dodge Rate":               text(6) = readglobalstring(6, "Dodge", 10)
 names(7) = "Counter Rate":             text(7) = readglobalstring(7, "Counter", 10)
 names(8) = "Speed":                    text(8) = readglobalstring(8, "Speed", 10)
 FOR i AS INTEGER = 1 TO 8
  names(8 + i) = "Enemy Type " & i:     text(8 + i) = readglobalstring(8 + i, "EnemyType" & i, 10)
  names(16 + i) = "Elemental " & i:     text(16 + i) = readglobalstring(16 + i, "Elemental" & i, 10)
 NEXT i
 FOR i AS INTEGER = 1 TO 4
  names(24 + i) = "Armor " & i:         text(24 + i) = readglobalstring(24 + i, "Armor " & i, 10)
 NEXT i
 names(29) = "Spell Skill":             text(29) = readglobalstring(29, "SpellSkill", 10)
 names(30) = "Spell Block":             text(30) = readglobalstring(30, "SpellBlock", 10)
 names(31) = "Spell cost %":            text(31) = readglobalstring(31, "SpellCost%", 10)
 names(32) = "Money":                   text(32) = readglobalstring(32, "Money", 10)
 names(33) = "Experience":              text(33) = readglobalstring(33, "Experience", 10)
 names(34) = "Battle Item Menu":        text(34) = readglobalstring(34, "Item", 10)
 names(35) = "Exit Item Menu":          text(35) = readglobalstring(35, "DONE", 10)
 names(36) = "Sort Item Menu":          text(36) = readglobalstring(36, "AUTOSORT", 10)
 names(37) = "Drop Item":               text(37) = readglobalstring(37, "TRASH", 10)
 names(38) = "Weapon":                  text(38) = readglobalstring(38, "Weapon", 10)
 names(39) = "Unequip All":             text(39) = readglobalstring(39, "-REMOVE-", 10)
 names(40) = "Exit Equip":              text(40) = readglobalstring(40, "-EXIT-", 10)
 names(41) = "Drop Prompt":             text(41) = readglobalstring(41, "Discard", 10)
 names(42) = "Negative Drop Prefix":    text(42) = readglobalstring(42, "Cannot", 10)
 names(43) = "Level":                   text(43) = readglobalstring(43, "Level", 10)
 names(44) = "Overwrite Save Yes":      text(44) = readglobalstring(44, "Yes", 10)
 names(45) = "Overwrite Save No":       text(45) = readglobalstring(45, "No", 10)
 names(46) = "Exit Spell List Menu":    text(46) = readglobalstring(46, "EXIT", 10)
 names(47) = "(exp) for next (level)":  text(47) = readglobalstring(47, "for next", 10)
 names(48) = "Remove Hero from Team":   text(48) = readglobalstring(48, "REMOVE", 10)
 names(49) = "Pay at Inn":              text(49) = readglobalstring(49, "Pay", 10)
 names(50) = "Cancel Inn":              text(50) = readglobalstring(50, "Cancel", 10)
 names(51) = "Cancel Spell Menu":       text(51) = readglobalstring(51, "(CANCEL)", 10)
 names(52) = "New Game":                text(52) = readglobalstring(52, "NEW GAME", 10)
 names(53) = "Exit Game":               text(53) = readglobalstring(53, "EXIT", 10)
 names(54) = "Pause":                   text(54) = readglobalstring(54, "PAUSE", 10)
 names(55) = "Quit Playing Prompt":     text(55) = readglobalstring(55, "Quit Playing?", 20)
 names(56) = "Quit Playing Yes":        text(56) = readglobalstring(57, "Yes", 10)
 names(57) = "Quit Playing No":         text(57) = readglobalstring(58, "No", 10)
 names(58) = "Cancel Save":             text(58) = readglobalstring(59, "CANCEL", 10)
 names(59) = "Menu: Items":             text(59) = readglobalstring(60, "Items", 10)
 names(60) = "Menu: Spells":            text(60) = readglobalstring(61, "Spells", 10)
 names(61) = "Menu: Status":            text(61) = readglobalstring(62, "Status", 10)
 names(62) = "Menu: Equip":             text(62) = readglobalstring(63, "Equip", 10)
 names(63) = "Menu: Order":             text(63) = readglobalstring(64, "Order", 10)
 names(64) = "Menu: Team":              text(64) = readglobalstring(65, "Team", 10)
 names(65) = "Menu: Save":              text(65) = readglobalstring(66, "Save", 10)
 names(66) = "Menu: Quit":              text(66) = readglobalstring(67, "Quit", 10)
 names(67) = "Menu: Minimap":           text(67) = readglobalstring(68, "Map", 10)
 names(68) = "Volume Control":          text(68) = readglobalstring(69, "Volume", 10)
 names(69) = "Shop Menu: Buy":          text(69) = readglobalstring(70, "Buy", 10)
 names(70) = "Shop Menu: Sell":         text(70) = readglobalstring(71, "Sell", 10)
 names(71) = "Shop Menu: Inn":          text(71) = readglobalstring(72, "Inn", 10)
 names(72) = "Shop Menu: Hire":         text(72) = readglobalstring(73, "Hire", 10)
 names(73) = "Shop Menu: Exit":         text(73) = readglobalstring(74, "Exit", 10)
 names(74) = "Unsellable item warning": text(74) = readglobalstring(75, "CANNOT SELL", 20)
 names(75) = "Sell value prefix":       text(75) = readglobalstring(77, "Worth", 20)
 names(76) = "Sell trade prefix":       text(76) = readglobalstring(79, "Trade for", 20)
 names(77) = "($) and a (item)":        text(77) = readglobalstring(81, "and a", 10)
 names(78) = "Worthless item warning":  text(78) = readglobalstring(82, "Worth Nothing", 20)
 names(79) = "Sell alert":              text(79) = readglobalstring(84, "Sold", 10)
 names(80) = "Buy trade prefix":        text(80) = readglobalstring(85, "Trade for", 20)
 names(81) = "Hire price prefix":       text(81) = readglobalstring(87, "Joins for", 20)
 names(82) = "Cannot buy prefix":       text(82) = readglobalstring(89, "Cannot Afford", 20)
 names(83) = "Cannot hire prefix":      text(83) = readglobalstring(91, "Cannot Hire", 20)
 names(84) = "Buy alert":               text(84) = readglobalstring(93, "Purchased", 20)
 names(85) = "Hire alert (suffix)":     text(85) = readglobalstring(95, "Joined!", 20)
 names(86) = "(#) in stock":            text(86) = readglobalstring(97, "in stock", 20)
 names(87) = "Equipability prefix":     text(87) = readglobalstring(99, "Equip:", 10)
 names(88) = "Party full warning":      text(88) = readglobalstring(100, "No Room In Party", 20)
 names(89) = "Replace Save Prompt":     text(89) = readglobalstring(102, "Replace Old Data?", 20)
 names(90) = "Status Prompt":           text(90) = readglobalstring(104, "Who's Status?", 20)
 names(91) = "Spells Prompt":           text(91) = readglobalstring(106, "Who's Spells?", 20)
 names(92) = "Equip Prompt":            text(92) = readglobalstring(108, "Equip Who?", 20)
 names(93) = "Equip Nothing (unequip)": text(93) = readglobalstring(110, "Nothing", 10)
 names(94) = "Nothing to Steal":        text(94) = readglobalstring(111, "Has Nothing", 30)
 names(95) = "Steal Failure":           text(95) = readglobalstring(114, "Cannot Steal", 30)
 names(96) = "Stole (itemname)":        text(96) = readglobalstring(117, "Stole", 30)
 names(97) = "When an Attack Misses":   text(97) = readglobalstring(120, "miss", 20)
 names(98) = "When a Spell Fails":      text(98) = readglobalstring(122, "fail", 20)
 names(99) = "(hero) learned (spell)":  text(99) = readglobalstring(124, "learned", 10)
 names(100) = "Found (gold)":           text(100) = readglobalstring(125, "Found", 10)
 names(101) = "Gained (experience)":    text(101) = readglobalstring(126, "Gained", 10)
 names(102) = "Weak to (elemental)":    text(102) = readglobalstring(127, "Weak to", 10)
 names(103) = "Strong to (elemental)":  text(103) = readglobalstring(128, "Strong to", 10)
 names(104) = "Absorbs (elemental)":    text(104) = readglobalstring(129, "Absorbs", 10)
 names(105) = "No Elemental Effects":   text(105) = readglobalstring(130, "No Elemental Effects", 30)
 names(106) = "(hero) has no spells":   text(106) = readglobalstring(133, "has no spells", 20)
 names(107) = "Plotscript: pick hero":  text(107) = readglobalstring(135, "Which Hero?", 20)
 names(108) = "Hero name prompt":       text(108) = readglobalstring(137, "Name the Hero", 20)
 names(109) = "Found a (item)":         text(109) = readglobalstring(139, "Found a", 20)
 names(110) = "Found (number) (items)": text(110) = readglobalstring(141, "Found", 20)
 names(111) = "THE INN COSTS (# gold)": text(111) = readglobalstring(143, "THE INN COSTS", 20)
 names(112) = "You have (# gold)":      text(112) = readglobalstring(145, "You have", 20)
 names(113) = "CANNOT RUN!":            text(113) = readglobalstring(147, "CANNOT RUN!", 20)
 names(114) = "Level up for (hero)":    text(114) = readglobalstring(149, "Level up for", 20)
 names(115) = "(#) levels for (hero)":  text(115) = readglobalstring(151, "levels for", 20)
 names(116) = "($) and (number) (item)":text(116) = readglobalstring(153, "and", 10)
 names(117) = "day":                    text(117) = readglobalstring(154, "day", 10)
 names(118) = "days":                   text(118) = readglobalstring(155, "days", 10)
 names(119) = "hour":                   text(119) = readglobalstring(156, "hour", 10)
 names(120) = "hours":                  text(120) = readglobalstring(157, "hours", 10)
 names(121) = "minute":                 text(121) = readglobalstring(158, "minute", 10)
 names(122) = "minutes":                text(122) = readglobalstring(159, "minutes", 10)
 names(123) = "minutes":                text(123) = readglobalstring(160, "Level MP", 20)

 'names() = "":      text() = readglobalstring(, "", 10)
 'NOTE: if you add global strings here, be sure to update the limit-checking on
 'the implementation of the "get global string" plotscripting command

 state.top = -1
 state.pt = -1
 state.first = -1
 state.last = max
 state.size = 22
 setkeys
 DO
  setwait 55
  setkeys
  IF keyval(scESC) > 1 THEN EXIT DO
  IF keyval(scF1) > 1 THEN show_help "edit_global_strings"
  IF keyval(scCTRL) > 0 AND keyval(scS) > 1 THEN
   IF prompt_for_string(search, "Search (descriptions & values)") THEN
    found = NO
    FOR i AS INTEGER = state.pt + 1 TO state.last
     IF INSTR(LCASE(names(i)), LCASE(search)) OR INSTR(LCASE(text(i)), LCASE(search)) THEN
      state.pt = i
      clamp_menu_state state
      found = YES
      EXIT FOR
     END IF
    NEXT i
    IF NOT found THEN '--Not found below, wrap to above
     FOR i AS INTEGER = 0 TO state.pt - 1
      IF INSTR(LCASE(names(i)), LCASE(search)) OR INSTR(LCASE(text(i)), LCASE(search)) THEN
       state.pt = i
       clamp_menu_state state
       found = YES
       EXIT FOR
      END IF
     NEXT i
    END IF
   END IF
  END IF
  usemenu state
  IF state.pt = -1 THEN
   IF enter_or_space() THEN EXIT DO
  ELSE
   strgrabber text(state.pt), maxlen(state.pt)
  END IF
 
  standardmenu names(), state, 0, 0, dpage
  standardmenu text(), state, 232, 0, dpage
  draw_scrollbar state, rect, , dpage
  edgeprint "CTRL+S Search", 0, 191, uilook(uiDisabledItem), dpage
  IF state.pt >= 0 THEN
   edgeboxstyle 160 - (maxlen(state.pt) * 4), 191, 8 * maxlen(state.pt) + 4, 8, 0, dpage, transOpaque, YES
   edgeprint text(state.pt), 162 - (maxlen(state.pt) * 4), 191, uilook(uiText), dpage
  END IF
  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
 DIM j AS INTEGER = 0
 FOR i AS INTEGER = 0 TO max
  writeglobalstring j, text(i), maxlen(i)
  j = j + 1 + (maxlen(i) \ 11)
 NEXT i
 getstatnames statnames()
 clearpage 0
 clearpage 1
END SUB

SUB writeglobalstring (index AS INTEGER, s AS STRING, maxlen AS INTEGER)
 DIM fh AS INTEGER = FREEFILE
 OPEN game & ".stt" FOR BINARY AS #fh
 DIM ch AS STRING
 ch = CHR(small(LEN(s), small(maxlen, 255)))
 PUT #fh, 1 + index * 11, ch
 ch = LEFT(s, small(maxlen, 255))
 PUT #fh, 2 + index * 11, ch
 CLOSE #fh
 loadglobalstrings
END SUB

FUNCTION prompt_for_string (BYREF s AS STRING, caption AS STRING, BYVAL limit AS INTEGER=NO) AS INTEGER
 '--Construct the prompt UI. FIXME: redo this when the Slice Editor can save/load)
 DIM root AS Slice Ptr
 root = NewSliceOfType(slRoot)
 root->Fill = YES
 DIM outer_box AS Slice Ptr
 outer_box = NewSliceOfType(slRectangle, root)
 WITH *outer_box
  .AnchorHoriz = 1
  .AnchorVert = 1
  .AlignHoriz = 1
  .AlignVert = 1
  .paddingTop = 16
  .paddingBottom = 16
  .paddingLeft = 16
  .paddingRight = 16
  .Width = 300
  .Height = 64
 END WITH
 ChangeRectangleSlice outer_box, 1
 DIM caption_area AS Slice Ptr
 caption_area = NewSliceOfType(slText, outer_box)
 ChangeTextSlice caption_area, caption, uilook(uiText)
 DIM inner_box AS Slice Ptr
 inner_box = NewSliceOfType(slContainer, outer_box)
 WITH *inner_box
  .paddingTop = 16
  .Fill = YES
 END WITH
 DIM text_border_box As Slice Ptr
 text_border_box = NewSliceOfType(slRectangle, inner_box)
 WITH *text_border_box
  .paddingTop = 2
  .paddingBottom = 2
  .paddingLeft = 2
  .paddingRight = 2
  .Fill = YES
 END WITH
 ChangeRectangleSlice text_border_box, , uilook(uiOutline), uilook(uiText)
 DIM text_area AS Slice Ptr
 text_area = NewSliceOfType(slText, text_border_box)
 WITH *text_area
  .Fill = YES
 END WITH
 ChangeTextSlice text_area, s, uilook(uiMenuItem), , , uilook(uiOutline) 

 '--Preserve whatever screen was already showing as a background
 DIM holdscreen AS INTEGER
 holdscreen = allocatepage
 copypage vpage, holdscreen

 DIM dat AS TextSliceData Ptr
 dat = text_area->SliceData

 IF limit = NO THEN limit = 40

 '--Now loop while editing string
 setkeys
 DO
  setwait 17, 70
  setkeys
  
  IF keyval(scESC) > 1 THEN
   prompt_for_string = NO
   EXIT DO
  END IF
  IF keyval(scEnter) > 1 THEN
   prompt_for_string = YES
   s = dat->s
   EXIT DO
  END IF
  strgrabber dat->s, limit

  DrawSlice root, dpage

  SWAP vpage, dpage
  setvispage vpage
  copypage holdscreen, dpage
  dowait
 LOOP
 
 setkeys
 freepage holdscreen
 DeleteSlice @root
END FUNCTION

FUNCTION safe_caption(caption_array() AS STRING, BYVAL index AS INTEGER, description AS STRING) AS STRING
 IF index >= LBOUND(caption_array) AND index <= UBOUND(caption_array) THEN
  RETURN caption_array(index)
 ELSE
  RETURN "Invalid " & description & " " & index
 END IF
END FUNCTION

SUB update_attack_editor_for_chain (BYVAL mode AS INTEGER, BYREF caption1 AS STRING, BYREF max1 AS INTEGER, BYREF min1 AS INTEGER, BYREF menutype1 AS INTEGER, BYREF caption2 AS STRING, BYREF max2 AS INTEGER, BYREF min2 AS INTEGER, BYREF menutype2 AS INTEGER)
 SELECT CASE mode
  CASE 0 '--no special condition
   caption1 = ""
   max1 = 32000
   min1 = -32000
   menutype1 = 18'skipper
   caption2 = ""
   max2 = 32000
   min2 = -32000
   menutype2 = 18'skipper
  CASE 1 '--tagcheck
   caption1 = "  if Tag:"
   max1 = 1000
   min1 = -1000
   menutype1 = 2
   caption2 = "  and Tag:"
   max2 = 1000
   min2 = -1000
   menutype2 = 2
  CASE 2 TO 5
   caption1 = "  if attacker"
   max1 = 15
   min1 = 0
   menutype1 = 16 'stat
   SELECT CASE mode
    CASE 2
     caption2 = "  is >"
     max2 = 32000
     min2 = -32000
     menutype2 = 0
    CASE 3
     caption2 = "  is <"
     max2 = 32000
     min2 = -32000
     menutype2 = 0
    CASE 4
     caption2 = "  is >"
     max2 = 100
     min2 = 0
     menutype2 = 17 'int%
    CASE 5
     caption2 = "  is <"
     max2 = 100
     min2 = 0
     menutype2 = 17 'int%
   END SELECT
 END SELECT
END SUB

FUNCTION attack_chain_browser (BYVAL start_attack AS INTEGER) AS INTEGER
 DIM state AS AttackChainBrowserState
 DIM selected AS INTEGER = start_attack
 
 state.before.size = 2
 state.after.size = 2
 
 DO
  '--Init

  FOR i AS INTEGER = 0 TO UBOUND(state.chainto)
   state.chainto(i) = 0
  NEXT i

  FOR i AS INTEGER = 0 TO UBOUND(state.chainfrom)
   state.chainfrom(i) = 0
  NEXT i
  
  state.root = NewSliceOfType(slRoot)

  state.lbox = NewSliceOfType(slContainer, state.root)
  state.lbox->Width = 80

  state.rbox = NewSliceOfType(slContainer, state.root)
  state.rbox->Width = 80
  state.rbox->AlignHoriz = 2
  state.rbox->AnchorHoriz = 2

  init_attack_chain_screen selected, state
 
  state.column = 1
  state.refresh = YES
  state.focused = state.current

  state.before.pt = 0
  state.before.top = 0
  state.after.pt = 0
  state.after.top = 0
 
  setkeys
  DO
   setwait 55
   setkeys

   IF keyval(scESC) > 1 THEN
    state.done = YES
    EXIT DO
   END IF
   IF keyval(scF1) > 1 THEN show_help "attack_chain_browse"

   IF enter_or_space() THEN
    IF state.focused <> 0 THEN
     IF state.column = 1 THEN state.done = YES
     selected = state.focused->extra(0)
     EXIT DO
    END IF
   END IF

   IF keyval(scLeft) > 1 THEN state.column = loopvar(state.column, 0, 2, -1) : state.refresh = YES
   IF keyval(scRight) > 1 THEN state.column = loopvar(state.column, 0, 2, 1) : state.refresh = YES
   SELECT CASE state.column
    CASE 0: IF usemenu(state.before) THEN state.refresh = YES
    CASE 1: 
    CASE 2: IF usemenu(state.after) THEN state.refresh = YES
   END SELECT
   
   IF state.refresh THEN
    state.refresh = NO
    attack_preview_slice_defocus state.focused
    SELECT CASE state.column
     CASE 0: state.focused = state.chainfrom(state.before.pt)
     CASE 1: state.focused = state.current
     CASE 2: state.focused = state.chainto(state.after.pt)
    END SELECT
    attack_preview_slice_focus state.focused
    state.lbox->Y = state.before.top * -56
   END IF
 
   DrawSlice state.root, dpage
 
   SWAP vpage, dpage
   setvispage vpage
   clearpage dpage
   dowait
  LOOP
  
  DeleteSlice @(state.root)
  IF state.done THEN EXIT DO
 LOOP
 
 RETURN selected
END FUNCTION

FUNCTION find_free_attack_preview_slot(slots() AS Slice Ptr) AS INTEGER
 FOR i AS INTEGER = 0 TO UBOUND(slots)
  IF slots(i) = 0 THEN RETURN i
 NEXT i
 'Oops! Can't hold any more 'FIXME: if/when FreeBasic supports resizeable arrays in types, use them here
 RETURN -1
END FUNCTION

SUB init_attack_chain_screen(BYVAL attack_id AS INTEGER, state AS AttackChainBrowserState)
 DIM atk AS AttackData
 loadattackdata atk, attack_id
 
 state.current = create_attack_preview_slice("", attack_id, state.root)
 state.current->AnchorHoriz = 1
 state.current->AlignHoriz = 1
 state.current->Y = 6
 
 DIM slot AS INTEGER
 IF atk.instead.atk_id > 0 THEN
  slot = find_free_attack_preview_slot(state.chainto())
  IF slot >= 0 THEN
   state.chainto(slot) = create_attack_preview_slice("Instead", atk.instead.atk_id - 1, state.rbox)
  END IF
 END IF
 IF atk.chain.atk_id > 0 THEN
  slot = find_free_attack_preview_slot(state.chainto())
  IF slot >= 0 THEN
   state.chainto(slot) = create_attack_preview_slice("Regular", atk.chain.atk_id - 1, state.rbox)
  END IF
 END IF
 IF atk.elsechain.atk_id > 0 THEN
  slot = find_free_attack_preview_slot(state.chainto())
  IF slot >= 0 THEN
   state.chainto(slot) = create_attack_preview_slice("Else", atk.elsechain.atk_id - 1, state.rbox)
  END IF
 END IF
 
 position_chain_preview_boxes(state.chainto(), state.after)

 '--now search for attacks that chain to this one
 FOR i AS INTEGER = 0 TO gen(genMaxAttack)
  loadattackdata atk, i
  IF atk.chain.atk_id - 1 = attack_id THEN
   slot = find_free_attack_preview_slot(state.chainfrom())
   IF slot = -1 THEN EXIT FOR 'give up when out of space
   state.chainfrom(slot) = create_attack_preview_slice("Regular", i, state.lbox)
  END IF
  IF atk.elsechain.atk_id - 1 = attack_id THEN
   slot = find_free_attack_preview_slot(state.chainfrom())
   IF slot = -1 THEN EXIT FOR 'give up when out of space
   state.chainfrom(slot) = create_attack_preview_slice("Else", i, state.lbox)
  END IF
  IF atk.instead.atk_id - 1 = attack_id THEN
   slot = find_free_attack_preview_slot(state.chainfrom())
   IF slot = -1 THEN EXIT FOR 'give up when out of space
   state.chainfrom(slot) = create_attack_preview_slice("Instead", i, state.lbox)
  END IF
 NEXT i

 position_chain_preview_boxes(state.chainfrom(), state.before)

END SUB

SUB position_chain_preview_boxes(sl_list() AS Slice ptr, st AS MenuState)
 st.last = -1
 DIM y AS INTEGER = 6
 FOR i AS INTEGER = 0 TO UBOUND(sl_list)
  IF sl_list(i) <> 0 THEN
   WITH *(sl_list(i))
    .Y = y
    y += .Height + 6
   END WITH
   st.last += 1
  END IF
 NEXT i
 IF st.last = -1 THEN st.last = 0
END SUB

FUNCTION create_attack_preview_slice(caption AS STRING, BYVAL attack_id AS INTEGER, BYVAL parent AS Slice Ptr) AS Slice Ptr
 DIM atk AS AttackData
 loadattackdata atk, attack_id
 
 DIM box AS Slice Ptr = NewSliceOfType(slRectangle, parent)
 box->Width = 80
 box->Height = 50
 ChangeRectangleSlice box, 0
 ChangeRectangleSlice box, , , , -1

 DIM spr AS Slice Ptr = NewSliceOfType(slSprite, box)
 ChangeSpriteSlice spr, 6, atk.picture, atk.pal, 2
 spr->AnchorHoriz = 1
 spr->AlignHoriz = 1
 spr->AnchorVert = 2
 spr->AlignVert = 2

 DIM numsl AS Slice Ptr = NewSliceOfType(slText, box)
 ChangeTextSlice numsl, STR(attack_id), , YES
 numsl->AnchorHoriz = 1
 numsl->AlignHoriz = 1
 
 DIM namesl AS Slice Ptr = NewSliceOfType(slText, box)
 ChangeTextSlice namesl, atk.name, , YES
 namesl->AnchorHoriz = 1
 namesl->AlignHoriz = 1
 namesl->Y = 10

 DIM capsl AS Slice Ptr = NewSliceOfType(slText, box)
 ChangeTextSlice capsl, caption, , -1
 capsl->AnchorHoriz = 1
 capsl->AlignHoriz = 1
 capsl->AnchorVert = 2
 capsl->AlignVert = 2

 '--Save attack_id in the extra data
 box->extra(0) = attack_id
 RETURN box
END FUNCTION

SUB attack_preview_slice_focus(BYVAL sl AS Slice Ptr)
 IF sl = 0 THEN EXIT SUB
 ChangeRectangleSlice sl, , , , 0
END SUB

SUB attack_preview_slice_defocus(BYVAL sl AS Slice Ptr)
 IF sl = 0 THEN EXIT SUB
 ChangeRectangleSlice sl, , , , -1
END SUB

SUB fontedit (font() AS INTEGER)

 DIM f(255) AS INTEGER
 DIM copybuf(4) AS INTEGER
 DIM menu(3) AS STRING

 menu(0) = "Previous Menu"
 menu(1) = "Edit Font..."
 menu(2) = "Import Font..."
 menu(3) = "Export Font..."

 DIM i AS INTEGER

 DIM last AS INTEGER = -1
 FOR i = 32 TO 255
  last += 1
  f(last) = i
 NEXT i

 DIM mode AS INTEGER = -1

 'This state is used for the menu, not the charpicker
 DIM state AS MenuState
 WITH state
  .pt = 0
  .top = 0
  .last = UBOUND(menu)
  .size = 22
 END WITH

 DIM linesize AS INTEGER = 14
 DIM pt AS INTEGER = -1 * linesize

 DIM x AS INTEGER
 DIM y AS INTEGER
 
 DIM xoff AS INTEGER
 DIM yoff AS INTEGER
 
 DIM c AS INTEGER

 setkeys
 DO
  setwait 55
  setkeys
  state.tog = state.tog XOR 1
  IF keyval(scF1) > 1 THEN show_help "fontedit"
  SELECT CASE mode
   CASE -1
    IF keyval(scEsc) > 1 THEN EXIT DO
    usemenu state
    IF enter_or_space() THEN
     IF state.pt = 0 THEN EXIT DO
     IF state.pt = 1 THEN mode = 0
     IF state.pt = 2 THEN
      fontedit_import_font font()
      state.pt = 1
      mode = 0
     END IF
     IF state.pt = 3 THEN fontedit_export_font font()
    END IF
   CASE 0
    IF keyval(scEsc) > 1 THEN mode = -1
    IF keyval(scUp) > 1 THEN pt = large(pt - linesize, -1 * linesize)
    IF keyval(scDown) > 1 THEN pt = small(pt + linesize, last)
    IF keyval(scLeft) > 1 THEN pt = large(pt - 1, 0)
    IF keyval(scRight) > 1 THEN pt = small(pt + 1, last)
    IF enter_or_space() THEN
     IF pt < 0 THEN
      mode = -1
     ELSE
      mode = 1
      x = 0
      y = 0
     END IF
    END IF
    IF keyval(scCtrl) > 0 AND keyval(scR) > 1 THEN romfontchar font(), pt
   CASE 1
    IF keyval(scEsc) > 1 OR keyval(scEnter) > 1 THEN mode = 0
    IF keyval(scUp) > 1 THEN y = loopvar(y, 0, 7, -1)
    IF keyval(scDown) > 1 THEN y = loopvar(y, 0, 7, 1)
    IF keyval(scLeft) > 1 THEN x = loopvar(x, 0, 7, -1)
    IF keyval(scRight) > 1 THEN x = loopvar(x, 0, 7, 1)
    IF keyval(scSpace) > 1 THEN
     setbit font(), 0, (f(pt) * 8 + x) * 8 + y, (readbit(font(), 0, (f(pt) * 8 + x) * 8 + y) XOR 1)
     setfont font()
    END IF
  END SELECT
  IF mode >= 0 THEN
   '--copy and paste support
   IF (keyval(scCtrl) > 0 AND keyval(scInsert) > 1) OR ((keyval(scLeftShift) > 0 OR keyval(scRightShift) > 0) AND keyval(scDelete) > 0) OR (keyval(scCtrl) > 0 AND keyval(scC) > 1) THEN
    FOR i = 0 TO 63
     setbit copybuf(), 0, i, readbit(font(), 0, f(pt) * 64 + i)
    NEXT i
   END IF
   IF ((keyval(scLeftShift) > 0 OR keyval(scRightShift) > 0) AND keyval(scInsert) > 1) OR (keyval(scCtrl) > 0 AND keyval(scV) > 1) THEN
    FOR i = 0 TO 63
     setbit font(), 0, f(pt) * 64 + i, readbit(copybuf(), 0, i)
    NEXT i
    setfont font()
   END IF
  END IF

  IF mode = -1 THEN
   standardmenu menu(), state, 0, 0, dpage
  END IF

  IF mode >= 0 THEN
   xoff = 8
   yoff = 8
   FOR i = 0 TO last
    textcolor uilook(uiMenuItem), uilook(uiDisabledItem)
    IF pt >= 0 THEN
     IF mode = 0 THEN
      IF (i MOD linesize) = (pt MOD linesize) OR (i \ linesize) = (pt \ linesize) THEN textcolor uilook(uiMenuItem), uilook(uiHighlight)
     END IF
     IF pt = i THEN textcolor uilook(uiSelectedItem + state.tog), 0
    END IF
    printstr CHR(f(i)), xoff + (i MOD linesize) * 9, yoff + (i \ linesize) * 9, dpage
   NEXT i
   textcolor uilook(uiMenuItem), 0
   IF pt < 0 THEN textcolor uilook(uiSelectedItem + state.tog), 0
   printstr menu(0), 8, 0, dpage

   IF pt >= 0 THEN
    xoff = 150
    yoff = 4
    rectangle xoff, yoff, 160, 160, uilook(uiDisabledItem), dpage
    FOR i = 0 TO 7
     FOR j AS INTEGER = 0 TO 7
      IF readbit(font(), 0, (f(pt) * 8 + i) * 8 + j) THEN
       rectangle xoff + i * 20, yoff + j * 20, 20, 20, uilook(uiMenuItem), dpage
      END IF
     NEXT j
    NEXT i
    IF mode = 1 THEN
     IF readbit(font(), 0, (f(pt) * 8 + x) * 8 + y) THEN
      c = uilook(uiSelectedItem2)
     ELSE
      c = uilook(uiSelectedDisabled)
     END IF
     rectangle xoff + x * 20, yoff + y * 20, 20, 20, c, dpage
    END IF
    textcolor uilook(uiText), 0
    printstr "ASCII " & f(pt), 20, 190, dpage
    IF f(pt) < 32 THEN
     printstr "RESERVED", 120, 190, dpage
    ELSE
     FOR i = 2 TO 53
      IF f(pt) = keyv(i, 2) THEN printstr "ALT+" + UCASE(CHR(keyv(i, 0))), 120, 190, dpage
      IF f(pt) = keyv(i, 3) THEN printstr "ALT+SHIFT+" + UCASE(CHR(keyv(i, 0))), 120, 190, dpage
     NEXT i
     IF f(pt) = 32 THEN printstr "SPACE", 120, 190, dpage
    END IF
   END IF
  END IF

  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP
 
 xbsave game + ".fnt", font(), 2048
EXIT SUB

END SUB

SUB fontedit_export_font(font() AS INTEGER)

 DIM newfont AS STRING = "newfont"
 newfont = inputfilename("Input a filename to save to", ".ohf", "", "input_file_export_font") 

 IF newfont <> "" THEN
  xbsave game & ".fnt", font(), 2048
  filecopy game & ".fnt", newfont & ".ohf"
 END IF

END SUB

SUB fontedit_import_font(font() AS INTEGER)

 STATIC default AS STRING
 DIM newfont AS STRING = browse(0, default, "*.ohf", "", , "browse_font")
 
 IF newfont <> "" THEN
  filecopy newfont, game & ".fnt"

  DIM i AS INTEGER
  DIM font_tmp(1024) AS INTEGER

  '--never overwrite 0 thru 31
  FOR i = 0 TO 2047
   setbit font_tmp(), 0, i, readbit(font(), 0, i)
  NEXT i

  '--Reload the font
  xbload game + ".fnt", font(), "Can't load font"
  setfont font()

  '--write back the old 0-31 values
  FOR i = 0 TO 2047
   setbit font(), 0, i, readbit(font_tmp(), 0, i)
  NEXT i
  
 END IF
END SUB

SUB cropafter (BYVAL index AS INTEGER, BYREF limit AS INTEGER, BYVAL flushafter AS INTEGER, lump AS STRING, BYVAL bytes AS INTEGER, BYVAL prompt AS INTEGER=YES)
 'flushafter -1 = flush records
 'flushafter 0 = trim file
 DIM i as integer

 IF prompt THEN
  DIM menu(1) as string
  menu(0) = "No do not delete anything"
  menu(1) = "Yes, delete all records after this one"
  IF sublist(menu(), "cropafter") < 1 THEN
   setkeys
   EXIT SUB
  ELSE
   setkeys
  END IF
 END IF

 DIM buf(bytes \ 2 - 1) AS INTEGER
 FOR i = 0 TO index
  loadrecord buf(), lump, bytes \ 2, i
  storerecord buf(), tmpdir & "_cropped.tmp", bytes \ 2, i
 NEXT i
 IF flushafter THEN
  'FIXME: this flushafter hack only exists for the .DT0 lump,
  ' out of fear that some code with read hero data past the end of the file.
  ' after cleanup of all hero code has confurmed this fear is unfounded, we can
  ' eliminate this hack entirely
  flusharray buf()
  FOR i = index + 1 TO limit
   storerecord buf(), tmpdir & "_cropped.tmp", bytes \ 2, i
  NEXT i
 END IF
 limit = index

 filecopy tmpdir & "_cropped.tmp", lump
 safekill tmpdir & "_cropped.tmp"
END SUB

FUNCTION numbertail (s AS STRING) AS STRING
 DIM n AS INTEGER

 IF s = "" THEN RETURN "BLANK"

 FOR i AS INTEGER = 1 TO LEN(s)
  IF is_int(MID(s, i)) THEN
   n = str2int(MID(s, i)) + 1
   RETURN LEFT(s, i - 1) & n
  END IF
 NEXT
 RETURN s + "2"  
END FUNCTION
