'OHRRPGCE CUSTOM - Mostly drawing-related routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'!$DYNAMIC
DEFINT A-Z

#include "udts.bi"
#include "custom_udts.bi"

'basic subs and functions
DECLARE SUB picktiletoedit (tmode%, pagenum%, mapfile$)
DECLARE SUB editmaptile (ts AS TileEditState, mover(), mouse(), area() AS MouseArea)
DECLARE SUB tilecut (ts AS TileEditState, mouse(), area() AS MouseArea)
DECLARE SUB refreshtileedit (mover%(), state AS TileEditState)
DECLARE SUB writeundoblock (mover%(), state AS TileEditState)
DECLARE SUB readundoblock (mover%(), state AS TileEditState)
DECLARE SUB fliptile (mover%(), ts AS TileEditState)
DECLARE SUB scrolltile (mover%(), ts AS TileEditState)
DECLARE SUB clicktile (mover(), ts AS TileEditState, mouseclick, BYREF clone AS TileCloneBuffer)
DECLARE SUB tilecopy (cutnpaste%(), ts AS TileEditState)
DECLARE SUB tilepaste (cutnpaste%(), ts AS TileEditState)
DECLARE SUB tiletranspaste (cutnpaste%(), ts AS TileEditState)
DECLARE FUNCTION filenum$ (n%)
DECLARE SUB copymapblock (buf%(), sx%, sy%, sp%, dx%, dy%, dp%)
DECLARE SUB changepal (palval%, palchange%, workpal%(), aindex%)
DECLARE SUB loadpasdefaults (array%(), tilesetnum%)
DECLARE SUB savepasdefaults (array%(), tilesetnum%)
DECLARE SUB airbrush (x%, y%, d%, m%, c%, p%)
DECLARE SUB ellipse (x%, y%, radius%, c%, p%, squish1%, squish2%)
DECLARE SUB cropafter (index%, limit%, flushafter%, lump$, bytes%, prompt%)
DECLARE SUB testanimpattern (tastuf%(), taset%)
DECLARE SUB setanimpattern (tastuf%(), taset%)
DECLARE FUNCTION mouseover% (mouse%(), BYREF zox, BYREF zoy, BYREF zcsr, area() AS MouseArea)
DECLARE SUB formation ()
DECLARE SUB enemydata ()
DECLARE SUB herodata ()
DECLARE SUB attackdata (atkdat$(), atklim%())
DECLARE SUB getnames (stat$(), max%)
DECLARE SUB statname ()
DECLARE SUB maptile (font())
DECLARE SUB importmasterpal (f$, palnum%)

'Local SUBs and FUNCTIONS
DECLARE SUB spriteedit_load_what_you_see(spritefile AS STRING, j, top, sets, ss AS SpriteEditState, soff, placer(), workpal(), poffset())
DECLARE SUB spriteedit_save_what_you_see(spritefile AS STRING, j, top, sets, ss AS SpriteEditState, soff, placer(), workpal(), poffset())
DECLARE SUB init_sprite_zones(area() AS MouseArea, ss AS SpriteEditState)
DECLARE SUB spriteedit_display(BYREF ss AS SpriteEditState, BYREF ss_save AS SpriteEditStatic, state AS MenuState, placer(), workpal(), poffset(), info$(), toolinfo() AS ToolInfoType, area() AS MouseArea, mouse())
DECLARE SUB spriteedit_import16(BYREF ss AS SpriteEditState, BYREF ss_save AS SpriteEditStatic, BYREF state AS MenuState, placer() AS INTEGER, workpal() AS INTEGER, poffset() AS INTEGER, info() AS STRING, toolinfo() AS ToolInfoType, area() AS MouseArea, mouse())

#include "scancodes.bi"
#include "compat.bi"
#include "allmodex.bi"
#include "common.bi"
#include "loading.bi"
#include "customsubs.bi"
#include "cglobals.bi"

#include "const.bi"

REM $STATIC
SUB airbrush (x, y, d, m, c, p)
'airbrush thanks to Ironhoof (Russel Hamrick)

'AirBrush this rutine works VERY well parameters as fallows:
' AIRBRUSH x , y , diameter , mist_amount , color , page
' diameter sets the width & hight by square radius
' mist_amount sets how many pixels to place i put 100 and it ran fast so
' it works EXCELLENTLY with a mouse on the DTE =)

FOR count = 1 TO RND * m
 x2 = RND * d
 y2 = RND * d
 r = d \ 2
 x3 = x - r
 y3 = y - r
 IF ABS((x3 + x2) - x) ^ 2 + ABS((y3 + y2) - y) ^ 2 < r ^ 2 THEN
  putpixel x3 + x2, y3 + y2, c, p
 END IF
NEXT

END SUB

SUB changepal (palval, palchange, workpal(), aindex)

storepal16 workpal(), aindex, palval
palval = bound(palval + palchange, 0, 32767)
getpal16 workpal(), aindex, palval

END SUB

SUB copymapblock (buf(), sx, sy, sp, dx, dy, dp)

'buf() is a 20-byte array

FOR i = 0 TO 19
 loadsprite buf(), 0, sx, sy + i, 40, 1, sp
 stosprite buf(), 0, dx, dy + i, dp
NEXT i

END SUB

SUB ellipse (x, y, radius, c, p, squish1, squish2)
'ellipse thanks to Ironhoof (Russel Hamrick)

'works mostly like the normal QB circle but with
'more useful features
' ELLIPSE x , y , radius , color , page , vertical pull , horizontal pull
'the vertical pull & horizontal pull should be in decimals or whole
'numbers. when both numbers are large it shrinks the circle to fit
'the screen like so ellipse 10,10,25,7,0,25,40 will make the ellispe
'smaller. but if its smaller number is 1 or 0 (same) and the other large 0, 25 it will only bend not shrink the ellipse.

r = radius
b = squish1
b2 = squish2
lx# = 0
ly# = 0

IF b = 0 THEN b = 1
IF b2 = 0 THEN b2 = 1
'IF b > b2 THEN r = r * b
'IF b < b2 THEN r = r * b2
t = -45
DO
 a# = (3.141593 * t) / 180
 xi# = COS(a#)
 yi# = SIN(a#)
 x2# = x - xi# * r / b
 y2# = y - yi# * r / b2
 IF x2# < 0 THEN x2# = 0
 IF x2# > 319 THEN x2# = 319
 IF y2# < 0 THEN y2# = 0
 IF y2# > 199 THEN y2# = 199
 IF lx# = 0 AND ly# = 0 THEN lx# = x2#: ly# = y2#
 drawline x2#, y2#, lx#, ly#, c, p
 lx# = x2#
 ly# = y2#
 t = t + 4:
 IF t > 360 THEN EXIT DO
LOOP

END SUB

SUB importbmp (f$, cap$, count)
STATIC default$
DIM palmapping(255), bmpd(4)
DIM pmask(255) as RGBcolor, temppal(255) as RGBcolor
DIM menu$(6), submenu$(2)
DIM mstate as MenuState
mstate.size = 24
mstate.last = UBOUND(menu$)
menu$(0) = "Return to Main Menu"
menu$(1) = CHR$(27) + "Browse 0" + CHR$(26)
menu$(2) = "Replace current " + cap$
menu$(3) = "Append a new " + cap$
menu$(4) = "Disable palette colors for import"
menu$(5) = "Export " + cap$ + " as BMP"
menu$(6) = "Full screen view"
submenu$(0) = "Import with current Master Palette"
submenu$(1) = "Import with new Master Palette"
submenu$(2) = "Do not remap colours"

pt = 0 'backdrop number

IF count = 0 THEN count = 1
clearpage 0
clearpage 1
clearpage 2
clearpage 3
loadpalette pmask(), activepalette
loadpage game + f$, pt, 2

setkeys
DO
 setwait 55
 setkeys
 IF keyval(29) > 0 AND keyval(14) > 1 THEN
  this = count - 1
  cropafter pt, this, 3, game + f$, -1, 1
  count = this + 1
 END IF
 IF keyval(scESC) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help "importbmp"
 usemenu mstate
 IF intgrabber(pt, 0, count - 1) THEN
  menu$(1) = CHR$(27) + "Browse" + XSTR$(pt) + CHR$(26)
  loadpage game + f$, pt, 2
 END IF
 IF enter_or_space() THEN
  IF mstate.pt = 0 THEN EXIT DO
  IF mstate.pt = 2 THEN
   srcbmp$ = browse$(3, default$, "*.bmp", "")
   IF srcbmp$ <> "" THEN
    GOSUB bimport
   END IF
   loadpage game + f$, pt, 2
  END IF
  IF mstate.pt = 3 AND count < 32767 THEN
   srcbmp$ = browse$(3, default$, "*.bmp", "")
   IF srcbmp$ <> "" THEN
    oldpt = pt
    pt = count
    GOSUB bimport
    IF pt = count THEN pt = oldpt 'cancelled
   END IF
   menu$(1) = CHR$(27) + "Browse" + XSTR$(pt) + CHR$(26)
   loadpage game + f$, pt, 2
  END IF
  IF mstate.pt = 4 THEN GOSUB disable
  IF mstate.pt = 5 THEN
   outfile$ = inputfilename("Name of file to export to?", ".bmp", trimpath$(game) & " " & cap$ & pt)
   IF outfile$ <> "" THEN screenshot outfile$ & ".bmp", 2, master()
  END IF
 END IF
 IF mstate.pt <> 6 THEN
  standardmenu menu$(), mstate, 0, 0, dpage, -1
 END IF
 SWAP vpage, dpage
 setvispage vpage
 copypage 2, dpage
 dowait
LOOP
clearpage 0
clearpage 1
clearpage 2
clearpage 3
EXIT SUB

disable:
csr2 = 0
setpal pmask()
setkeys
DO
 setwait 55
 setkeys
 tog = tog XOR 1
 IF keyval(scESC) > 1 THEN setpal master(): RETRACE
 IF keyval(scF1) > 1 THEN show_help "importbmp_disable"
 IF csr2 = 0 THEN
  IF enter_or_space() THEN setpal master(): RETRACE
  IF keyval(80) > 1 THEN csr2 = 1: cy = -1
 END IF
 IF csr2 = 1 THEN
  IF keyval(75) > 1 THEN cx = large(cx - 1, 0)
  IF keyval(77) > 1 THEN cx = small(cx + 1, 15)
  IF keyval(80) > 1 THEN cy = small(cy + 1, 15)
  IF keyval(72) > 1 THEN cy = cy - 1: IF cy < 0 THEN cy = 0: csr2 = 0
  IF enter_or_space() THEN
   pmask(cy * 16 + cx).r xor= master(cy * 16 + cx).r
   pmask(cy * 16 + cx).g xor= master(cy * 16 + cx).g
   pmask(cy * 16 + cx).b xor= master(cy * 16 + cx).b
   setpal pmask()
  END IF
 END IF
 textcolor uilook(uiMenuItem), 0: IF csr2 = 0 THEN textcolor uilook(uiSelectedItem + tog), 0
 printstr "Previous Menu", 0, 0, dpage
 IF csr2 = 1 THEN rectangle 0 + cx * 10, 8 + cy * 10, 10, 10, uilook(uiSelectedItem + tog), dpage
 FOR i = 0 TO 15
  FOR o = 0 TO 15
   rectangle 1 + o * 10, 9 + i * 10, 8, 8, i * 16 + o, dpage
  NEXT o
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage 2, dpage
 dowait
LOOP

bimport:
bmpinfo(srcbmp$, bmpd())
paloption = 2
IF bmpd(0) = 8 THEN
 loadbmppal srcbmp$, temppal()
 IF memcmp(@temppal(0), @master(0), 256 * sizeof(RGBcolor)) <> 0 THEN
  paloption = sublist(submenu$(), "importbmp_palette")
  IF paloption = -1 THEN RETRACE
  IF paloption = 1 THEN
   importmasterpal srcbmp$, gen(genMaxMasterPal) + 1
   activepalette = gen(genMaxMasterPal)
   setpal master()
   LoadUIColors uilook(), activepalette
  END IF
 END IF
END IF
bitmap2page pmask(), srcbmp$, 3
IF paloption = 0 THEN
 convertbmppal srcbmp$, pmask(), palmapping(), 0
 FOR y = 0 TO 199
  FOR x = 0 TO 319
   putpixel x, y, palmapping(readpixel(x, y, 3)), 3
  NEXT
 NEXT
END IF
storepage game + f$, pt, 3
IF pt >= count THEN count = pt + 1
loadpalette pmask(), activepalette
RETRACE

END SUB

SUB loadpasdefaults (array(), tilesetnum)
flusharray array(), 160, 0
'--load defaults from tile set defaults file
setpicstuf array(), 322, -1
loadset workingdir + SLASH + "defpass.bin", tilesetnum, 0
'--enforce magic number and filesize
IF array(160) <> 4444 THEN
 flusharray array(), 160, 0
END IF
END SUB

SUB savepasdefaults (array(), tilesetnum)
'--set magic number
array(160) = 4444
'--write defaults into tile set defaults file
setpicstuf array(), 322, -1
storeset workingdir + SLASH + "defpass.bin", tilesetnum, 0
END SUB

SUB maptile (font())
DIM menu$(10), tastuf(40)

mapfile$ = game + ".til"

bnum = 0
tmode = 0
pagenum = -1
top = -1
taptr = 0

setkeys
DO
 setwait 55
 setkeys
 tog = tog XOR 1
 IF keyval(scESC) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help "maptile_pickset"
 IF keyval(29) > 0 AND keyval(14) > 1 AND pagenum > -1 THEN
  cropafter pagenum, gen(33), 3, game + ".til", -1, 1
 END IF
 IF keyval(80) > 1 AND pagenum = gen(33) AND gen(33) < 32767 THEN
  pagenum = pagenum + 1
  IF needaddset(pagenum, gen(33), "tile set") THEN
   WHILE pagenum > top + 20: top = top + 1: WEND
   clearpage 3
   storepage mapfile$, pagenum, 3
  END IF
 END IF
 IF usemenu(pagenum, top, -1, gen(33), 20) THEN
  IF pagenum = -1 THEN clearpage 3 ELSE loadpage mapfile$, pagenum, 3
 END IF
 IF enter_or_space() AND pagenum = -1 THEN EXIT DO
 IF enter_or_space() AND pagenum > -1 THEN GOSUB tilemode
 FOR i = top TO small(top + 20, gen(33))
  c = uilook(uiMenuItem)
  IF pagenum = i THEN c = uilook(uiSelectedItem + tog)
  IF i < 0 THEN
   edgeprint "Return to Main Menu", 10, 8 + (i - top) * 8, c, dpage
  ELSE
   edgeprint "Tile Set" + XSTR$(i), 10, 8 + (i - top) * 8, c, dpage
  END IF
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 dowait
LOOP
clearpage 3
clearpage 2
clearpage 1
clearpage 0
EXIT SUB

tilemode:
GOSUB tilemodemenu
setkeys
DO
 setwait 55
 setkeys
 tog = tog XOR 1
 IF keyval(scESC) > 1 THEN tmode = 0: RETRACE
 IF keyval(scF1) > 1 THEN show_help "maptile_tilemode"
 usemenu tmode, 0, 0, 5, 24
 IF enter_or_space() THEN
  SELECT CASE tmode
   CASE 0, 1, 2, 3
    picktiletoedit tmode, pagenum, mapfile$
   CASE 4
    GOSUB tileanim
    setkeys
    GOSUB tilemodemenu
   CASE 5
    tmode = 0
    RETRACE
  END SELECT
 END IF
 FOR i = 0 TO 5
  c = uilook(uiMenuItem)
  IF tmode = i THEN c = uilook(uiSelectedItem + tog)
  edgeprint menu$(i), 10, 8 * (i + 1), c, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 dowait
LOOP
tilemodemenu:
menu$(0) = "Draw Tiles"
menu$(1) = "Cut Tiles from Tilesets"
menu$(2) = "Cut Tiles from Backdrops"
menu$(3) = "Set Default Passability"
menu$(4) = "Define Tile Animation"
menu$(5) = "Cancel"
RETRACE

tileanim:
taset = 0
loadtanim pagenum, tastuf()
GOSUB utamenu
menu$(0) = "Previous Menu"
menu$(2) = "Set Animation Range"
menu$(3) = "Set Animation Pattern"
menu$(5) = "Test Animations"
setkeys
DO
 setwait 55
 setkeys
 tog = tog XOR 1
 IF keyval(scESC) > 1 THEN savetanim pagenum, tastuf(): RETRACE
 IF keyval(scF1) > 1 THEN show_help "maptile_tileanim"
 IF usemenu(taptr, 0, 0, 5, 5) THEN GOSUB utamenu
 IF taptr = 1 THEN
  IF intgrabber(taset, 0, 1) THEN GOSUB utamenu
 END IF
 IF taptr = 4 THEN
  IF tag_grabber(tastuf(1 + 20 * taset)) THEN GOSUB utamenu
 END IF
 IF enter_or_space() THEN
  IF taptr = 0 THEN savetanim pagenum, tastuf(): RETRACE
  IF taptr = 2 THEN GOSUB setanimrange
  IF taptr = 3 THEN setanimpattern tastuf(), taset
  IF taptr = 5 THEN testanimpattern tastuf(), taset
  
 END IF
 FOR i = 0 TO 5
  textcolor uilook(uiMenuItem), uilook(uiOutline)
  IF taptr = i THEN textcolor uilook(uiSelectedItem + tog), uilook(uiOutline)
  printstr menu$(i), 10, 8 * (i + 1), dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP

utamenu:
menu$(1) = CHR$(27) + "Animation set" + XSTR$(taset) + CHR$(26)
menu$(4) = tag_condition_caption(tastuf(1 + 20 * taset), "Disable if Tag", "No tag check")
RETRACE

setanimrange:
setkeys
DO
 setwait 55
 setkeys
 tog = tog XOR 1
 IF keyval(scESC) > 1 OR enter_or_space() THEN RETRACE
 IF keyval(scF1) > 1 THEN show_help "maptile_setanimrange"
 IF keyval(72) > 1 THEN tastuf(0 + 20 * taset) = large(tastuf(0 + 20 * taset) - 16, 0)
 IF keyval(80) > 1 THEN tastuf(0 + 20 * taset) = small(tastuf(0 + 20 * taset) + 16, 112)
 IF keyval(75) > 1 THEN tastuf(0 + 20 * taset) = large(tastuf(0 + 20 * taset) - 1, 0)
 IF keyval(77) > 1 THEN tastuf(0 + 20 * taset) = small(tastuf(0 + 20 * taset) + 1, 112)
 GOSUB drawanimrange
 SWAP vpage, dpage
 setvispage vpage
 copypage 3, dpage
 dowait
LOOP

drawanimrange:
x = 0: y = 0
FOR i = 0 TO 159
 IF i < tastuf(0 + 20 * taset) OR i > tastuf(0 + 20 * taset) + 47 THEN
  fuzzyrect x * 20, y * 20, 20, 20, uilook(uiText), dpage
 END IF
 x = x + 1: IF x > 15 THEN x = 0: y = y + 1
NEXT i
RETRACE

END SUB

FUNCTION mouseover (mouse(), BYREF zox, BYREF zoy, BYREF zcsr, area() AS MouseArea)

FOR i = 22 TO 0 STEP -1
 IF area(i).w <> 0 AND area(i).h <> 0 THEN
  IF mouse(0) >= area(i).x AND mouse(0) < area(i).x + area(i).w THEN
   IF mouse(1) >= area(i).y AND mouse(1) < area(i).y + area(i).h THEN
    zox = mouse(0) - area(i).x
    zoy = mouse(1) - area(i).y
    zcsr = area(i).hidecursor
    mouseover = i + 1
    EXIT FUNCTION
   END IF 'Y OKAY---
  END IF 'X OKAY---
 END IF 'VALID ZONE---
NEXT i

END FUNCTION

SUB setanimpattern (tastuf(), taset)
DIM menu$(12), stuff$(7), llim(7), ulim(7)
menu$(0) = "Previous Menu"
stuff$(0) = "end of animation"
stuff$(1) = "up"
stuff$(2) = "down"
stuff$(3) = "right"
stuff$(4) = "left"
stuff$(5) = "wait"
stuff$(6) = "if tag do rest"
stuff$(7) = "unknown command"
FOR i = 1 TO 2
 llim(i) = 0
 ulim(i) = 9
NEXT i
FOR i = 3 TO 4
 llim(i) = 0
 ulim(i) = 159
NEXT i
llim(5) = 0
ulim(5) = 32767
llim(6) = -999
ulim(6) = 999

GOSUB refreshmenu
pt = 0
ptr2 = 0
context = 0
index = 0
setkeys
DO
 setwait 55
 setkeys
 tog = tog XOR 1
 IF keyval(scF1) > 1 THEN show_help "maptile_setanimpattern"
 SELECT CASE context
  CASE 0 '---PICK A STATEMENT---
   IF keyval(scESC) > 1 THEN EXIT DO
   IF usemenu(pt, 0, 0, 9, 9) THEN GOSUB refreshmenu
   IF enter_or_space() THEN
    IF pt = 0 THEN
     EXIT DO
    ELSE
     context = 1
    END IF
   END IF
  CASE 1 '---EDIT THAT STATEMENT---
   IF keyval(scESC) > 1 THEN context = 0
   usemenu ptr2, 0, 0, 1, 1
   index = bound(pt - 1, 0, 8) + 20 * taset
   IF ptr2 = 0 THEN
    IF intgrabber(tastuf(2 + index), 0, 6) THEN GOSUB refreshmenu
    IF enter_or_space() THEN context = 0
   END IF
   IF ptr2 = 1 THEN
    IF tastuf(2 + index) = 6 THEN
     IF tag_grabber(tastuf(11 + index)) THEN GOSUB refreshmenu
    ELSE
     IF intgrabber(tastuf(11 + index), llim(tastuf(2 + index)), ulim(tastuf(2 + index))) THEN GOSUB refreshmenu
     IF enter_or_space() THEN context = 0
    END IF
   END IF
 END SELECT
 FOR i = 0 TO 9
  textcolor uilook(uiMenuItem), 0
  IF i = pt THEN
   textcolor uilook(uiSelectedItem + tog), 0
  END IF
  IF context = 1 THEN textcolor uilook(uiDisabledItem), 0
  printstr menu$(i), 0, i * 8, dpage
 NEXT i
 IF pt > 0 THEN
  FOR i = 0 TO 1
   textcolor uilook(uiMenuItem), 0
   IF context = 1 AND i = ptr2 THEN
    textcolor uilook(uiSelectedItem + tog), 0
   END IF
   IF context = 0 THEN textcolor uilook(uiDisabledItem), 0
   printstr menu$(10 + i), 0, 100 + i * 8, dpage
  NEXT i
 END IF 'pt > 1
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
GOSUB forcebounds
EXIT SUB

refreshmenu:
GOSUB forcebounds
FOR i = 1 TO 9
 menu$(i) = "-"
NEXT i
menu$(10) = ""
FOR i = 0 TO 8
 a = bound(tastuf((2 + i) + 20 * taset), 0, 7)
 b = tastuf((11 + i) + 20 * taset)
 menu$(i + 1) = stuff$(a)
 IF a = 0 THEN EXIT FOR
 IF a > 0 AND a < 6 THEN menu$(i + 1) = menu$(i + 1) + XSTR$(b)
 IF a = 6 THEN menu$(i + 1) = menu$(i + 1) & " (" & load_tag_name(b) & ")"
NEXT i
IF i = 8 THEN menu$(10) = "end of animation"
menu$(10) = "Action=" + stuff$(bound(tastuf(2 + bound(pt - 1, 0, 8) + 20 * taset), 0, 7))
menu$(11) = "Value="
this = tastuf(11 + bound(pt - 1, 0, 8) + 20 * taset)
SELECT CASE tastuf(2 + bound(pt - 1, 0, 8) + 20 * taset)
 CASE 1 TO 4
  menu$(11) = menu$(11) + STR$(this) + " Tiles"
 CASE 5
  menu$(11) = menu$(11) + STR$(this) + " Ticks"
 CASE 6
  menu$(11) = menu$(11) + tag_condition_caption(this)
 CASE ELSE
  menu$(11) = menu$(11) + "N/A"
END SELECT
RETRACE

forcebounds:
FOR i = 0 TO 8
 j = bound(i, 0, 8) + 20 * taset
 tastuf(2 + j) = bound(tastuf(2 + j), 0, 7)
 tastuf(11 + j) = bound(tastuf(11 + j), llim(tastuf(2 + j)), ulim(tastuf(2 + j)))
NEXT i
RETRACE

END SUB

SUB testanimpattern (tastuf(), taset)

DIM sample(7)
DIM tanim_state(1) AS TileAnimState
DIM tileset as Frame ptr = NULL

clearpage vpage
clearpage dpage

loadtileset tileset, 3

sample(0) = 3
sample(1) = 3
buffer(0) = 16
buffer(1) = 3
setmapdata buffer(), buffer(), 10, 130
FOR i = 0 TO 47
 y = INT(i / 16)
 x = i - y * 16
 setmapblock x, y, 0, tastuf(20 * taset) + i
NEXT i

GOSUB setupsample

setkeys
DO
 setwait 55
 setkeys
 tog = tog XOR 1
 
 IF keyval(scESC) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN show_help "maptile_testanimpattern"
 IF keyval(72) > 1 THEN csr = loopvar(csr, 0, 47, -16): GOSUB setupsample
 IF keyval(80) > 1 THEN csr = loopvar(csr, 0, 47, 16): GOSUB setupsample
 IF keyval(75) > 1 THEN csr = loopvar(csr, 0, 47, -1): GOSUB setupsample
 IF keyval(77) > 1 THEN csr = loopvar(csr, 0, 47, 1): GOSUB setupsample
 SWAP vpage, dpage
 setvispage vpage
 '--draw available animating tiles--
 setmapdata buffer(), buffer(), 10, 130
 drawmap 0, -10, 0, 0, tileset, dpage
 '--draw sample--
 setmapdata sample(), sample(), 100, 40
 setanim tastuf(0) + tanim_state(0).cycle, tastuf(20) + tanim_state(1).cycle
 cycletile tanim_state(), tastuf()
 drawmap -130, -100, 0, 0, tileset, dpage
 '--Draw cursor--
 y = INT(csr / 16)
 x = csr - y * 16
 rectangle 20 * x, 10 + 20 * y, 20, 1, uilook(uiSelectedItem + tog), dpage
 rectangle 20 * x, 10 + 20 * y, 1, 20, uilook(uiSelectedItem + tog), dpage
 rectangle 20 * x, 29 + 20 * y, 20, 1, uilook(uiSelectedItem + tog), dpage
 rectangle 20 * x + 19, 10 + 20 * y, 1, 20, uilook(uiSelectedItem + tog), dpage
 
 dowait
LOOP
unloadtileset tileset
EXIT SUB

setupsample:
setmapdata sample(), sample(), 100, 70
FOR i = 0 TO 8
 y = INT(i / 3)
 x = i - y * 3
 setmapblock x, y, 0, 160 + (taset * 48) + csr
NEXT i
RETRACE

END SUB

SUB picktiletoedit (tmode, pagenum, mapfile$)
STATIC cutnpaste(19, 19), oldpaste
DIM ts AS TileEditState, mover(12), mouse(4), area(22) AS MouseArea
ts.gotmouse = setmouse(mouse())
ts.canpaste = oldpaste
ts.drawcursor = 1
ts.airsize = 5
ts.mist = 4
area(0).x = 60
area(0).y = 0
area(0).w = 200
area(0).h = 160
area(0).hidecursor = YES
area(1).x = 0
area(1).y = 160
area(1).w = 320
area(1).h = 32
'TOOLS (more at 21,22)
FOR i = 0 TO 5
 area(2 + i).x = 4 + i * 9
 area(2 + i).y = 32
 area(2 + i).w = 8
 area(2 + i).h = 8
NEXT i
FOR i = 0 TO 3
 area(12 + i).x = 4 + i * 9
 area(12 + i).y = 42
 area(12 + i).w = 8
 area(12 + i).h = 8
NEXT i
area(10).x = 8
area(10).y = 190
area(10).w = 32
area(10).h = 10
area(11).x = 280
area(11).y = 190
area(11).w = 32
area(11).h = 10
'LESS AIRBRUSH AREA
area(16).x = 12
area(16).y = 60
area(16).w = 8
area(16).h = 8
area(16).hidecursor = NO
'LESS AIRBRUSH MIST
area(17).x = 12
area(17).y = 76
area(17).w = 8
area(17).h = 8
area(17).hidecursor = NO
'MORE AIRBRUSH AREA
area(18).x = 36
area(18).y = 60
area(18).w = 8
area(18).h = 8
area(18).hidecursor = NO
'MORE AIRBRUSH MIST
area(19).x = 36
area(19).y = 76
area(19).w = 8
area(19).h = 8
area(19).hidecursor = NO
FOR i = 0 TO 1
 area(21 + i).x = 40 + i * 9
 area(21 + i).y = 42
 area(21 + i).w = 8
 area(21 + i).h = 8
NEXT i
DIM pastogkey(7), defaults(160), bitmenu$(10)
IF tmode = 3 THEN
 pastogkey(0) = 72
 pastogkey(1) = 77
 pastogkey(2) = 80
 pastogkey(3) = 75
 pastogkey(4) = 30
 pastogkey(5) = 48
 pastogkey(6) = 35
 pastogkey(7) = 24
 loadpasdefaults defaults(), pagenum
 bitmenu$(0) = "Impassable to the North"
 bitmenu$(1) = "Impassable to the East"
 bitmenu$(2) = "Impassable to the South"
 bitmenu$(3) = "Impassable to the West"
 bitmenu$(4) = "A-type vehicle Tile"
 bitmenu$(5) = "B-type vehicle Tile"
 bitmenu$(6) = "Harm Tile"
 bitmenu$(7) = "Overhead Tile"
END IF    

loadpage mapfile$, pagenum, 3
'pick block to draw/import/default
bnum = 0
setkeys
DO
 setwait 55
 setkeys
 copypage 3, dpage
 IF ts.gotmouse THEN
  readmouse mouse()
 END IF
 IF keyval(scESC) > 1 THEN storepage mapfile$, pagenum, 3: EXIT DO
 IF keyval(scF1) > 1 THEN
  IF tmode = 3 THEN
   show_help "default_passability"
  ELSE
   show_help "picktiletoedit"
  END IF
 END IF
 IF tmode <> 3 OR keyval(29) = 0 THEN
  IF slowkey(75, 2) THEN IF bnum > 0 THEN bnum = bnum - 1: IF ts.gotmouse THEN mouse(0) = mouse(0) - 20: movemouse mouse(0), mouse(1)
  IF slowkey(77, 2) THEN IF bnum < 159 THEN bnum = bnum + 1: IF ts.gotmouse THEN mouse(0) = mouse(0) + 20: movemouse mouse(0), mouse(1)
  IF slowkey(72, 2) THEN IF bnum > 15 THEN bnum = bnum - 16: IF ts.gotmouse THEN mouse(1) = mouse(1) - 20: movemouse mouse(0), mouse(1)
  IF slowkey(80, 2) THEN IF bnum < 144 THEN bnum = bnum + 16: IF ts.gotmouse THEN mouse(1) = mouse(1) + 20: movemouse mouse(0), mouse(1)
 END IF
 IF ts.gotmouse THEN
  bnum = INT(mouse(1) / 20) * 16 + INT(mouse(0) / 20)
 END IF
 IF tmode = 3 THEN
  '--pass mode shortcuts
  FOR i = 0 TO 7
   IF keyval(29) > 0 OR i > 3 THEN
    IF keyval(pastogkey(i)) > 1 THEN
     setbit defaults(), bnum, i, readbit(defaults(), bnum, i) XOR 1
    END IF
   END IF
  NEXT i
 END IF
 IF (keyval(29) > 0 AND keyval(82) > 1) OR ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(83) > 0) OR (keyval(29) > 0 AND keyval(46) > 1) THEN tilecopy cutnpaste(), ts
 IF ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(82) > 1) OR (keyval(29) > 0 AND keyval(47) > 1) THEN tilepaste cutnpaste(), ts
 IF (keyval(29) > 0 AND keyval(20) > 1) THEN tiletranspaste cutnpaste(), ts
 ts.tilex = bnum AND 15
 ts.tiley = INT(bnum / 16)
 IF enter_or_space() OR mouse(3) > 0 THEN
  setkeys
  IF tmode = 0 THEN
   editmaptile ts, mover(), mouse(), area()
  END IF
  IF tmode = 1 THEN
   ts.cuttileset = YES
   ts.cutfrom = small(ts.cutfrom, gen(genMaxTile))
   tilecut ts, mouse(), area()
  END IF 
  IF tmode = 2 THEN
   ts.cuttileset = NO
   ts.cutfrom = small(ts.cutfrom, gen(genMaxBackdrop) - 1)
   tilecut ts, mouse(), area()
  END IF 
  IF tmode = 3 THEN
   editbitset defaults(), bnum, 7, bitmenu$()
  END IF
 END IF
 tog = tog XOR 1
 IF tmode = 3 THEN
  FOR o = 0 TO 9
   FOR i = 0 TO 15
    IF (defaults(i + o * 16) AND 1) THEN rectangle i * 20, o * 20, 20, 3, uilook(uiMenuItem + tog), dpage
    IF (defaults(i + o * 16) AND 2) THEN rectangle i * 20 + 17, o * 20, 3, 20, uilook(uiMenuItem + tog), dpage
    IF (defaults(i + o * 16) AND 4) THEN rectangle i * 20, o * 20 + 17, 20, 3, uilook(uiMenuItem + tog), dpage
    IF (defaults(i + o * 16) AND 8) THEN rectangle i * 20, o * 20, 3, 20, uilook(uiMenuItem + tog), dpage
    textcolor uilook(uiSelectedItem + tog), 0
    IF (defaults(i + o * 16) AND 16) THEN printstr "A", i * 20, o * 20, dpage
    IF (defaults(i + o * 16) AND 32) THEN printstr "B", i * 20 + 10, o * 20, dpage
    IF (defaults(i + o * 16) AND 64) THEN printstr "H", i * 20, o * 20 + 10, dpage
    IF (defaults(i + o * 16) AND 128) THEN printstr "O", i * 20 + 10, o * 20 + 10, dpage
   NEXT i
  NEXT o
 END IF
 rectangle ts.tilex * 20 + 7, ts.tiley * 20 + 7, 6, 6, IIF(tog, uilook(uiBackground), uilook(uiText)), dpage
 IF ts.gotmouse THEN
  IF tog THEN
   textcolor uilook(uiText), 0
  ELSE
   textcolor uilook(uiDescription), 0
  END IF
  printstr CHR$(2), small(large(mouse(0) - 2, 0), 311), small(large(mouse(1) - 2, 0), 191), dpage
 END IF
 SWAP dpage, vpage
 setvispage vpage
 dowait
LOOP
IF tmode = 3 THEN
 savepasdefaults defaults(), pagenum
END IF
oldpaste = ts.canpaste
END SUB

SUB refreshtileedit (mover(), state AS TileEditState)
copymapblock mover(), state.tilex * 20, state.tiley * 20, 3, 280, 10 + (state.undo * 21), 2
rectangle 59, 0, 202, 161, uilook(uiText), 2
FOR i = 0 TO 19
 FOR j = 0 TO 19
  rectangle 60 + i * 10, j * 8, 10, 8, readpixel(state.tilex * 20 + i, state.tiley * 20 + j, 3), 2
 NEXT j
NEXT i
END SUB

SUB writeundoblock (mover(), state AS TileEditState)
rectangle 270, 16 + (state.undo * 21), 8, 8, 0, 2
state.undo = loopvar(state.undo, 0, 5, 1)
copymapblock mover(), state.tilex * 20, state.tiley * 20, 3, 280, 10 + (state.undo * 21), 2
textcolor uilook(uiMenuItem), 0
printstr ">", 270, 16 + (state.undo * 21), 2
state.allowundo = 1
END SUB

SUB readundoblock (mover(), state AS TileEditState)
FOR j = 0 TO 5
 rectangle 270, 16 + (j * 21), 8, 8, 0, 2
NEXT j
copymapblock mover(), 280, 10 + (state.undo * 21), 2, state.tilex * 20, state.tiley * 20, 3
textcolor uilook(uiMenuItem), 0
printstr ">", 270, 16 + (state.undo * 21), 2
refreshtileedit mover(), state
END SUB

SUB editmaptile (ts AS TileEditState, mover(), mouse(), area() AS MouseArea)
STATIC clone AS TileCloneBuffer
DIM spot AS XYPair

DIM toolinfo(7) AS ToolInfoType
WITH toolinfo(0)
 .name = "Draw"
 .icon = CHR(3)
 .shortcut = 32
 .cursor = 0
 .areanum = 2
END WITH
WITH toolinfo(1)
 .name = "Box"
 .icon = CHR(4)
 .shortcut = 48
 .cursor = 1
 .areanum = 3
END WITH
WITH toolinfo(2)
 .name = "Line"
 .icon = CHR(5)
 .shortcut = 38
 .cursor = 2
 .areanum = 4
END WITH
WITH toolinfo(3)
 .name = "Fill"
 .icon = "F"
 .shortcut = 33
 .cursor = 3
 .areanum = 5
END WITH
WITH toolinfo(4)
 .name = "Oval"
 .icon = "O"
 .shortcut = 24
 .cursor = 2
 .areanum = 6
END WITH
WITH toolinfo(5)
 .name = "Air"
 .icon = "A"
 .shortcut = 30
 .cursor = 3
 .areanum = 7
END WITH
WITH toolinfo(6)
 .name = "Mark"
 .icon = "M"
 .shortcut = 50
 .cursor = 3
 .areanum = 21
END WITH
WITH toolinfo(7)
 .name = "Clone"
 .icon = "C"
 .shortcut = 46
 .cursor = 3
 .areanum = 22
END WITH

tog = 0
tick = 0
ts.justpainted = 0
ts.undo = 0
ts.allowundo = 0
ts.delay = 3
zox = ts.x * 10 + 5
zoy = ts.y * 8 + 4
mouse(0) = area(0).x + zox
mouse(1) = area(0).y + zoy
movemouse mouse(0), mouse(1)
clearpage 2
FOR i = 0 TO 5
 edgebox 279, 9 + (i * 21), 22, 22, uilook(uiBackground), uilook(uiMenuItem), 2
NEXT i
refreshtileedit mover(), ts
textcolor uilook(uiMenuItem), 0
printstr ">", 270, 16 + (ts.undo * 21), 2
FOR j = 0 TO 7
 FOR i = 0 TO 15
  rectangle i * 10, j * 4 + 160, 10, 4, j * 16 + i, 2
  rectangle i * 10 + 160, j * 4 + 160, 10, 4, j * 16 + i + 128, 2
 NEXT i
NEXT j
'---EDIT BLOCK---
setkeyrepeat 25, 5
setkeys
DO
 setwait 17, 70
 setkeys
 IF ts.gotmouse THEN
  readmouse mouse()
  zcsr = 0
  ts.zone = mouseover(mouse(), zox, zoy, zcsr, area())
 END IF

 ts.delay = large(ts.delay - 1, 0)
 ts.justpainted = large(ts.justpainted - 1, 0)
 IF keyval(1) > 1 THEN
  IF ts.hold = YES THEN
   ts.hold = NO
  ELSE
   EXIT DO
  END IF
 END IF
 IF keyval(scF1) > 1 THEN show_help "editmaptile"
 IF keyval(56) = 0 THEN
  ts.fixmouse = NO
  IF slowkey(75, 6) THEN ts.x = large(ts.x - 1, 0): ts.fixmouse = YES
  IF slowkey(77, 6) THEN ts.x = small(ts.x + 1, 19): ts.fixmouse = YES
  IF slowkey(72, 6) THEN ts.y = large(ts.y - 1, 0): ts.fixmouse = YES
  IF slowkey(80, 6) THEN ts.y = small(ts.y + 1, 19): ts.fixmouse = YES
  IF ts.fixmouse AND ts.zone = 1 THEN
   zox = ts.x * 10 + 5
   zoy = ts.y * 8 + 4
   mouse(0) = area(0).x + zox
   mouse(1) = area(0).y + zoy
   movemouse mouse(0), mouse(1)
  END IF
 END IF 
 '---KEYBOARD SHORTCUTS FOR TOOLS------------
 FOR i = 0 TO 7
  IF keyval(toolinfo(i).shortcut) > 1 THEN
   ts.tool = i
   ts.hold = NO
   ts.drawcursor = toolinfo(i).cursor + 1
  END IF
 NEXT i
 '----------
 IF keyval(51) > 1 OR (keyval(56) > 0 AND keyval(75) > 1) THEN
  ts.curcolor = (ts.curcolor + 255) MOD 256
  IF ts.curcolor MOD 16 = 15 THEN ts.curcolor = (ts.curcolor + 144) MOD 256
 END IF
 IF keyval(52) > 1 OR (keyval(56) > 0 AND keyval(77) > 1) THEN
  ts.curcolor += 1
  IF ts.curcolor MOD 16 = 0 THEN ts.curcolor = (ts.curcolor + 112) MOD 256
 END IF
 IF keyval(56) > 0 AND keyval(72) > 1 THEN ts.curcolor = (ts.curcolor + 240) MOD 256
 IF keyval(56) > 0 AND keyval(80) > 1 THEN ts.curcolor = (ts.curcolor + 16) MOD 256
 IF keyval(41) > 1 THEN ts.hidemouse = ts.hidemouse XOR 1
 IF keyval(29) > 0 AND keyval(44) > 1 AND ts.allowundo THEN
  ts.undo = loopvar(ts.undo, 0, 5, -1)
  readundoblock mover(), ts
 END IF
 IF keyval(57) > 0 THEN clicktile mover(), ts, mouse(3), clone
 IF keyval(28) > 1 THEN ts.curcolor = readpixel(ts.tilex * 20 + ts.x, ts.tiley * 20 + ts.y, 3)
 IF keyval(scCapslock) > 0 THEN scrolltile mover(), ts
 SELECT CASE ts.zone   
 CASE 1
  ts.x = INT(zox / 10)
  ts.y = INT(zoy / 8)
  IF ts.tool = clone_tool THEN
   ' For clone brush tool, enter/right-click moves the handle point
   IF ts.readjust THEN
    IF keyval(28) = 0 AND mouse(2) = 0 THEN ' click or key release
     ts.readjust = NO
     ts.hox += (ts.x - ts.adjustpos.x)
     ts.hoy += (ts.y - ts.adjustpos.y)
     ts.adjustpos.x = 0
     ts.adjustpos.y = 0
    END IF
   ELSE
    IF (keyval(28) AND 5) OR mouse(2) = 2 THEN
     ts.readjust = YES
     ts.adjustpos.x = ts.x
     ts.adjustpos.y = ts.y
    END IF
   END IF
  ELSE
   'for all other tools, pick a color
   IF mouse(2) = 2 THEN
    ts.curcolor = readpixel(ts.tilex * 20 + ts.x, ts.tiley * 20 + ts.y, 3)
   END IF
  END IF
  IF mouse(2) = 1 THEN clicktile mover(), ts, mouse(3), clone
 CASE 2
  IF mouse(2) > 0 AND mouse(3) = 1 THEN
   ts.curcolor = ((zoy \ 4) * 16) + ((zox MOD 160) \ 10) + (zox \ 160) * 128
  END IF
 CASE 13 TO 16
  IF mouse(3) = 1 THEN fliptile mover(), ts
 END SELECT
 FOR i = 0 TO 7
  IF toolinfo(i).areanum = ts.zone - 1 THEN
   IF mouse(3) = 1 THEN
    ts.tool = i
    ts.drawcursor = toolinfo(ts.tool).cursor + 1
   END IF
  END IF
 NEXT i
 '--mouse over undo
 IF mouse(0) >= 280 AND mouse(0) < 300 THEN
  FOR i = 0 TO 5
   IF mouse(1) >= (10 + (i * 21)) AND mouse(1) < (30 + (i * 21)) THEN
    IF mouse(3) = 1 AND ts.allowundo THEN
     ts.undo = i
     readundoblock mover(), ts
    END IF
   END IF
  NEXT i
 END IF
 IF ts.tool = airbrush_tool THEN '--adjust airbrush
  IF mouse(3) = 1 OR mouse(2) = 1 THEN
   IF ts.zone = 17 THEN ts.airsize = large(ts.airsize - tick, 1)
   IF ts.zone = 19 THEN ts.airsize = small(ts.airsize + tick, 30)
   IF ts.zone = 18 THEN ts.mist = large(ts.mist - tick, 1)
   IF ts.zone = 20 THEN ts.mist = small(ts.mist + tick, 99)
  END IF
  IF keyval(12) > 1 OR keyval(74) > 1 THEN
   IF keyval(29) > 0 THEN
    ts.mist = large(ts.mist - 1, 1)
   ELSE
    ts.airsize = large(ts.airsize - 1, 1)
   END IF
  END IF
  IF keyval(13) > 1 OR keyval(78) > 1 THEN
   IF keyval(29) > 0 THEN
    ts.mist = small(ts.mist + 1, 99)
   ELSE
    ts.airsize = small(ts.airsize + 1, 80)
   END IF
  END IF
 END IF
 IF keyval(scBackspace) > 1 OR keyval(scLeftBracket) > 1 OR keyval(scRightBracket) > 1 THEN fliptile mover(), ts
 cy = (ts.curcolor \ 16) MOD 8
 cx = (ts.curcolor AND 15) + (ts.curcolor \ 128) * 16
 rectangle cx * 10 + 4, cy * 4 + 162, 3, 1, IIF(tog, uilook(uiBackground), uilook(uiText)), dpage
 rectangle 60 + ts.x * 10, ts.y * 8, 10, 8, readpixel(ts.tilex * 20 + ts.x, ts.tiley * 20 + ts.y, 3), dpage
 rectangle ts.x * 10 + 64, ts.y * 8 + 3, 3, 2, IIF(tog, uilook(uiBackground), uilook(uiText)), dpage
 IF ts.tool = airbrush_tool THEN
  ellipse 64 + ts.x * 10, 3 + ts.y * 8, (ts.airsize * 9) / 2, ts.curcolor, dpage, 0, 0
 END IF
 IF ts.tool = clone_tool AND tog = 0 THEN
  IF clone.exists = YES THEN
   FOR i = 0 TO clone.size.y - 1
    FOR j = 0 TO clone.size.x - 1
     spot.x = ts.x - ts.hox + j
     spot.y = ts.y - ts.hoy + i
     IF ts.readjust = YES THEN
      spot.x -= (ts.x - ts.adjustpos.x)
      spot.y -= (ts.y - ts.adjustpos.y)
     END IF
     IF spot.x >= 0 AND spot.x <= 19 AND spot.y >= 0 AND spot.y <= 19 AND clone.buf(j, i) > 0 THEN
      rectangle 60 + spot.x * 10, 0 + spot.y * 8, 10, 8, clone.buf(j, i), dpage
     END IF
    NEXT j
   NEXT i
  END IF
 END IF
 IF ts.hold = YES THEN
  SELECT CASE ts.tool
   CASE box_tool
    rectangle 60 + small(ts.x, ts.hox) * 10, small(ts.y, ts.hoy) * 8, (ABS(ts.x - ts.hox) + 1) * 10, (ABS(ts.y - ts.hoy) + 1) * 8, ts.curcolor, dpage
   CASE line_tool
    drawline 65 + ts.x * 10, 4 + ts.y * 8, 65 + ts.hox * 10, 4 + ts.hoy * 8, ts.curcolor, dpage
   CASE oval_tool
    radius = large(ABS(ts.hox - ts.x), ABS(ts.hoy - ts.y)) * 9
    ellipse 65 + ts.hox * 10, 4 + ts.hoy * 8, radius, ts.curcolor, dpage, 0, 0
   CASE mark_tool
    IF tog = 0 THEN emptybox 60 + small(ts.x, ts.hox) * 10, small(ts.y, ts.hoy) * 8, (ABS(ts.x - ts.hox) + 1) * 10, (ABS(ts.y - ts.hoy) + 1) * 8, INT(RND * 10), 10, dpage
  END SELECT
 END IF
 textcolor uilook(uiText), uilook(uiHighlight)
 printstr toolinfo(ts.tool).name, 8, 8, dpage
 printstr "Tool", 8, 16, dpage
 printstr "Undo", 274, 1, dpage
 FOR i = 0 TO 7
  fgcol = uilook(uiMenuItem): bgcol = uilook(uiDisabledItem)
  IF ts.tool = i THEN fgcol = uilook(uiText): bgcol = uilook(uiMenuItem)
  IF ts.zone - 1 = toolinfo(i).areanum THEN bgcol = uilook(uiSelectedDisabled)
  textcolor fgcol, bgcol
  printstr toolinfo(i).icon, area(toolinfo(i).areanum).x, area(toolinfo(i).areanum).y, dpage
 NEXT i
 FOR i = 0 TO 3
  textcolor uilook(uiMenuItem), uilook(uiDisabledItem): IF ts.zone = 13 + i THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
  printstr CHR$(7 + i), 4 + i * 9, 42, dpage
 NEXT i
 IF ts.tool = airbrush_tool THEN
  textcolor uilook(uiMenuItem), 0
  printstr "SIZE", 12, 52, dpage
  printstr XSTR$(ts.airsize), 12, 60, dpage
  printstr "MIST", 12, 68, dpage
  printstr XSTR$(ts.mist), 12, 76, dpage
  textcolor uilook(uiMenuItem), uilook(uiDisabledItem): IF ts.zone = 17 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
  printstr CHR$(27), 12, 60, dpage
  textcolor uilook(uiMenuItem), uilook(uiDisabledItem): IF ts.zone = 18 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
  printstr CHR$(27), 12, 76, dpage
  textcolor uilook(uiMenuItem), uilook(uiDisabledItem): IF ts.zone = 19 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
  printstr CHR$(26), 36, 60, dpage
  textcolor uilook(uiMenuItem), uilook(uiDisabledItem): IF ts.zone = 20 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
  printstr CHR$(26), 36, 76, dpage
 END IF
 IF ts.gotmouse THEN
  c = zcsr
  IF c = -1 THEN
   c = ts.drawcursor
   IF ts.hidemouse THEN c = -2
  END IF
  IF tog THEN
   textcolor uilook(uiText), 0
  ELSE
   textcolor uilook(uiDescription), 0
  END IF
  printstr CHR$(2 + c), small(large(mouse(0) - 2, 0), 311), small(large(mouse(1) - 2, 0), 191), dpage
 END IF
 SWAP dpage, vpage
 setvispage vpage
 copypage 2, dpage
 tick = 0
 IF dowait THEN tick = 1: tog = tog XOR 1
LOOP
setkeyrepeat
IF ts.gotmouse THEN
 movemouse ts.tilex * 20 + 10, ts.tiley * 20 + 10
END IF
END SUB

SUB clicktile (mover(), ts AS TileEditState, mouseclick, BYREF clone AS TileCloneBuffer)
DIM spot AS XYPair

IF ts.delay > 0 THEN EXIT SUB
SELECT CASE ts.tool
 CASE draw_tool
  IF ts.justpainted = 0 THEN writeundoblock mover(), ts
  ts.justpainted = 3
  putpixel 280 + ts.x, 10 + (ts.undo * 21) + ts.y, ts.curcolor, 2
  rectangle ts.tilex * 20 + ts.x, ts.tiley * 20 + ts.y, 1, 1, ts.curcolor, 3
  rectangle 60 + ts.x * 10, ts.y * 8, 10, 8, ts.curcolor, 2
 CASE box_tool
  IF mouseclick > 0 OR keyval(57) > 1 THEN
   IF ts.hold = YES THEN
    writeundoblock mover(), ts
    rectangle small(ts.tilex * 20 + ts.x, ts.tilex * 20 + ts.hox), small(ts.tiley * 20 + ts.y, ts.tiley * 20 + ts.hoy), ABS(ts.x - ts.hox) + 1, ABS(ts.y - ts.hoy) + 1, ts.curcolor, 3
    refreshtileedit mover(), ts
    ts.hold = NO
   ELSE
    ts.hold = YES
    ts.hox = ts.x
    ts.hoy = ts.y
   END IF
  END IF
 CASE line_tool
  IF mouseclick > 0 OR keyval(57) > 1 THEN
   IF ts.hold = YES THEN
    writeundoblock mover(), ts
    drawline ts.tilex * 20 + ts.x, ts.tiley * 20 + ts.y, ts.tilex * 20 + ts.hox, ts.tiley * 20 + ts.hoy, ts.curcolor, 3
    refreshtileedit mover(), ts
    ts.hold = NO
   ELSE
    ts.hold = YES
    ts.hox = ts.x
    ts.hoy = ts.y
   END IF
  END IF
 CASE fill_tool
  IF mouseclick > 0 OR keyval(57) > 1 THEN
   writeundoblock mover(), ts
   rectangle 0, 0, 22, 22, ts.curcolor, dpage
   FOR i = 0 TO 19
    FOR j = 0 TO 19
     putpixel 1 + i, 1 + j, readpixel(ts.tilex * 20 + i, ts.tiley * 20 + j, 3), dpage
    NEXT j
   NEXT i
   paintat 1 + ts.x, 1 + ts.y, ts.curcolor, dpage, buffer(), 16384
   FOR i = 0 TO 19
    FOR j = 0 TO 19
     putpixel ts.tilex * 20 + i, ts.tiley * 20 + j, readpixel(1 + i, 1 + j, dpage), 3
    NEXT j
   NEXT i
   refreshtileedit mover(), ts
   rectangle 0, 0, 22, 22, uilook(uiBackground), dpage
  END IF
 CASE oval_tool
  IF mouseclick > 0 OR keyval(57) > 1 THEN
   IF ts.hold = YES THEN
    writeundoblock mover(), ts
    radius = large(ABS(ts.hox - ts.x), ABS(ts.hoy - ts.y))
    rectangle 0, 0, 22, 22, uilook(uiText), dpage
    FOR i = 0 TO 19
     FOR j = 0 TO 19
      putpixel 1 + i, 1 + j, readpixel(ts.tilex * 20 + i, ts.tiley * 20 + j, 3), dpage
     NEXT j
    NEXT i
    ellipse 1 + ts.hox, 1 + ts.hoy, radius, ts.curcolor, dpage, 0, 0
    FOR i = 0 TO 19
     FOR j = 0 TO 19
      putpixel ts.tilex * 20 + i, ts.tiley * 20 + j, readpixel(1 + i, 1 + j, dpage), 3
     NEXT j
    NEXT i
    refreshtileedit mover(), ts
    rectangle 0, 0, 22, 22, uilook(uiBackground), dpage
    ts.hold = NO
   ELSE
    ts.hold = YES
    ts.hox = ts.x
    ts.hoy = ts.y
   END IF
  END IF
 CASE airbrush_tool
  IF ts.justpainted = 0 THEN writeundoblock mover(), ts
  ts.justpainted = 3
  rectangle 19, 119, 22, 22, uilook(uiText), dpage
  FOR i = 0 TO 19
   FOR j = 0 TO 19
    putpixel 20 + i, 120 + j, readpixel(ts.tilex * 20 + i, ts.tiley * 20 + j, 3), dpage
   NEXT j
  NEXT i
  airbrush 20 + ts.x, 120 + ts.y, ts.airsize, ts.mist, ts.curcolor, dpage
  FOR i = 0 TO 19
   FOR j = 0 TO 19
    putpixel ts.tilex * 20 + i, ts.tiley * 20 + j, readpixel(20 + i, 120 + j, dpage), 3
   NEXT j
  NEXT i
  refreshtileedit mover(), ts
 CASE mark_tool
  IF mouseclick > 0 OR keyval(57) > 1 THEN
   IF ts.hold = YES THEN
    clone.size.x = ABS(ts.x - ts.hox) + 1
    clone.size.y = ABS(ts.y - ts.hoy) + 1
    FOR i = 0 TO clone.size.y - 1
     FOR j = 0 TO clone.size.x - 1
      clone.buf(j, i) = readpixel(small(ts.tilex * 20 + ts.x, ts.tilex * 20 + ts.hox) + j, small(ts.tiley * 20 + ts.y, ts.tiley * 20 + ts.hoy) + i, 3)
     NEXT j
    NEXT i
    ts.hox = clone.size.x \ 2
    ts.hoy = clone.size.y \ 2
    ts.readjust = NO
    ts.adjustpos.x = 0
    ts.adjustpos.y = 0
    clone.exists = YES
    refreshtileedit mover(), ts
    ts.hold = NO
    ts.tool = clone_tool ' auto-select the clone tool after marking
   ELSE
    ts.hold = YES
    ts.hox = ts.x
    ts.hoy = ts.y
   END IF
  END IF
 CASE clone_tool
 IF mouseclick > 0 OR keyval(57) > 1 THEN
  IF ts.justpainted = 0 THEN writeundoblock mover(), ts
  ts.justpainted = 3
  IF clone.exists = YES THEN
   FOR i = 0 TO clone.size.y - 1
    FOR j = 0 TO clone.size.x - 1
     spot.x = ts.x - ts.hox + j + ts.adjustpos.x
     spot.y = ts.y - ts.hoy + i + ts.adjustpos.y
     IF spot.x >= 0 AND spot.x <= 19 AND spot.y >= 0 AND spot.y <= 19 AND clone.buf(j, i) > 0 THEN
      putpixel ts.tilex * 20 + spot.x, ts.tiley * 20 + spot.y, clone.buf(j, i), 3
     END IF
    NEXT j
   NEXT i
   refreshtileedit mover(), ts
  ELSE
   'if no clone buffer, switch to mark tool
   ts.tool = mark_tool
   ts.hold = YES
   ts.hox = ts.x
   ts.hoy = ts.y
  END IF
 END IF
END SELECT
END SUB

SUB scrolltile (mover(), ts AS TileEditState)
rectangle 0, 0, 20, 20, uilook(uiBackground), dpage
shiftx = 0: shifty = 0
IF slowkey(72, 6) THEN shifty = -1
IF slowkey(80, 6) THEN shifty = 1
IF slowkey(75, 6) THEN shiftx = -1
IF slowkey(77, 6) THEN shiftx = 1
FOR i = 0 TO 19
 FOR j = 0 TO 19
  tempx = (i + shiftx + 20) MOD 20
  tempy = (j + shifty + 20) MOD 20
  putpixel tempx, tempy, readpixel(ts.tilex * 20 + i, ts.tiley * 20 + j, 3), dpage
 NEXT j
NEXT i
FOR i = 0 TO 19
 FOR j = 0 TO 19
  putpixel ts.tilex * 20 + i, ts.tiley * 20 + j, readpixel(i, j, dpage), 3
 NEXT j
NEXT i
refreshtileedit mover(), ts
rectangle 0, 0, 20, 20, uilook(uiBackground), dpage
END SUB

SUB fliptile (mover(), ts AS TileEditState)
writeundoblock mover(), ts
rectangle 0, 0, 20, 20, uilook(uiBackground), dpage
flipx = 0: flipy = 0
IF (ts.zone = 13 OR ts.zone = 16) OR keyval(scLeftBracket) > 1 OR (keyval(scBackspace) > 1 AND keyval(scCtrl) = 0) THEN flipx = 19
IF ts.zone = 14 OR ts.zone = 15 OR keyval(scRightBracket) > 1 OR (keyval(scBackspace) > 1 AND keyval(scCtrl) > 0) THEN flipy = 19
FOR i = 0 TO 19
 FOR j = 0 TO 19
  tempx = ABS(i - flipx)
  tempy = ABS(j - flipy)
  IF (ts.zone = 15 OR ts.zone = 16) OR (keyval(26) > 1 OR keyval(27) > 1) THEN SWAP tempx, tempy
  putpixel tempx, tempy, readpixel(ts.tilex * 20 + i, ts.tiley * 20 + j, 3), dpage
 NEXT j
NEXT i
FOR i = 0 TO 19
 FOR j = 0 TO 19
  putpixel ts.tilex * 20 + i, ts.tiley * 20 + j, readpixel(i, j, dpage), 3
 NEXT j
NEXT i
refreshtileedit mover(), ts
rectangle 0, 0, 20, 20, uilook(uiBackground), dpage
END SUB

SUB tilecut (ts AS TileEditState, mouse(), area() AS MouseArea)
IF ts.gotmouse THEN
 movemouse ts.x, ts.y
END IF
ts.delay = 3
previewticks = 0
IF ts.cuttileset THEN loadpage game + ".til", ts.cutfrom, 2 ELSE loadpage game + ".mxs", ts.cutfrom, 2
setkeys
DO
 setwait 110
 setkeys
 tog = tog XOR 1
 ts.delay = large(ts.delay - 1, 0)
 IF ts.gotmouse THEN
  readmouse mouse()
  zcsr = 0
  ts.zone = mouseover(mouse(), 0, 0, zcsr, area())
  IF keyval(42) OR keyval(54) THEN 'shift
   ts.x = mouse(0) - mouse(0) MOD 20
   ts.y = mouse(1) - mouse(1) MOD 20
  ELSE
   ts.x = small(mouse(0), 300)
   ts.y = small(mouse(1), 180)
  END IF
 END IF
 IF keyval(scESC) > 1 THEN
  EXIT DO
 END IF
 IF keyval(scF1) > 1 THEN show_help "tilecut"
 inc = 1: IF keyval(56) > 0 THEN inc = 20
 IF keyval(72) AND 5 THEN ts.y = large(ts.y - inc, 0): IF ts.gotmouse THEN movemouse ts.x, ts.y
 IF keyval(80) AND 5 THEN ts.y = small(ts.y + inc, 180): IF ts.gotmouse THEN movemouse ts.x, ts.y
 IF keyval(75) AND 5 THEN ts.x = large(ts.x - inc, 0): IF ts.gotmouse THEN movemouse ts.x, ts.y
 IF keyval(77) AND 5 THEN ts.x = small(ts.x + inc, 300): IF ts.gotmouse THEN movemouse ts.x, ts.y
 IF enter_or_space() OR (mouse(3) > 0 AND ts.zone < 11) THEN
  IF ts.delay = 0 THEN
   FOR i = 0 TO 19
    FOR j = 0 TO 19
     putpixel ts.tilex * 20 + i, ts.tiley * 20 + j, readpixel(ts.x + i, ts.y + j, 2), 3
    NEXT j
   NEXT i
   IF keyval(28) > 1 OR (mouse(3) AND 4) THEN 'enter or middle click
    ts.tiley = (ts.tiley + (ts.tilex + 1) \ 16) MOD 10
    ts.tilex = (ts.tilex + 1) AND 15
    ts.x += 20
    IF ts.x > 300 THEN
     ts.x = 0
     ts.y += 20
     IF ts.y > 180 THEN ts.y = 0
    END IF
    IF ts.gotmouse THEN movemouse ts.x, ts.y
    previewticks = 6
   ELSE
    EXIT DO
   END IF
  END IF
 END IF
 '---PICK BACKGROUND PAGE------
 oldcut = ts.cutfrom
 IF ts.cuttileset THEN maxset = gen(genMaxTile) ELSE maxset = gen(genMaxBackdrop) - 1
 intgrabber ts.cutfrom, 0, maxset, 51, 52
 IF ts.zone = 11 AND mouse(3) > 0 THEN ts.cutfrom = loopvar(ts.cutfrom, 0, maxset, -1)
 IF ts.zone = 12 AND mouse(3) > 0 THEN ts.cutfrom = loopvar(ts.cutfrom, 0, maxset, 1)
 IF oldcut <> ts.cutfrom THEN
  IF ts.cuttileset THEN loadpage game + ".til", ts.cutfrom, 2 ELSE loadpage game + ".mxs", ts.cutfrom, 2
 END IF
 '----
 IF previewticks THEN
  previewy = bound(ts.tiley * 20 - 20, 0, 140)
  IF ts.y < 100 THEN
   'tileset preview at bottom of screen
   copypage 2, dpage, 0, 0, 139
   copypage 3, dpage, previewy, 141, 199
   rectangle 0, 139, 320, 2, uilook(uiSelectedItem + tog), dpage
   drawbox ts.tilex * 20, 141 + ts.tiley * 20 - previewy, 20, 20, uilook(uiSelectedItem + tog), dpage
  ELSE
   copypage 3, dpage, bound(ts.tiley * 20 - 20, 0, 140), 0, 59
   copypage 2, dpage, 59, 59, 199
   rectangle 0, 59, 320, 2, uilook(uiSelectedItem + tog), dpage
   drawbox ts.tilex * 20, ts.tiley * 20 - previewy, 20, 20, uilook(uiSelectedItem + tog), dpage
  END IF
  previewticks -= 1
 ELSE
  copypage 2, dpage
 END IF

 drawbox ts.x, ts.y, 20, 20, iif(tog, uilook(uiText), uilook(uiDescription)), dpage
 textcolor uilook(uiMenuItem + tog), 1
 IF ts.zone = 11 THEN textcolor uilook(uiSelectedItem + tog), uilook(uiHighlight)
 printstr "Prev", 8, 190, dpage
 textcolor uilook(uiMenuItem + tog), 1
 IF ts.zone = 12 THEN textcolor uilook(uiSelectedItem + tog), uilook(uiHighlight)
 printstr "Next", 280, 190, dpage
 textcolor uilook(uiText), uilook(uiHighlight)
 temp$ = XSTR$(ts.cutfrom) + " "
 printstr temp$, 160 - LEN(temp$) * 4, 190, dpage
 IF ts.gotmouse THEN
  IF tog THEN
   textcolor uilook(uiText), 0
  ELSE
   textcolor uilook(uiDescription), 0
  END IF
  printstr CHR$(2), small(large(mouse(0) - 2, 0), 311), small(large(mouse(1) - 2, 0), 191), dpage
 END IF
 SWAP dpage, vpage
 setvispage vpage
 dowait
LOOP
IF ts.gotmouse THEN
 movemouse ts.tilex * 20 + 10, ts.tiley * 20 + 10
END IF
END SUB

SUB tilecopy (cutnpaste(), ts AS TileEditState)
FOR i = 0 TO 19
 FOR j = 0 TO 19
  cutnpaste(i, j) = readpixel(ts.tilex * 20 + i, ts.tiley * 20 + j, 3)
 NEXT j
NEXT i
ts.canpaste = 1
END SUB

SUB tilepaste (cutnpaste(), ts AS TileEditState)
IF ts.canpaste THEN
 FOR i = 0 TO 19
  FOR j = 0 TO 19
   putpixel ts.tilex * 20 + i, ts.tiley * 20 + j, cutnpaste(i, j), 3
  NEXT j
 NEXT i
END IF 
END SUB

SUB tiletranspaste (cutnpaste(), ts AS TileEditState)
IF ts.canpaste THEN
 FOR i = 0 TO 19
  FOR j = 0 TO 19
   IF cutnpaste(i, j) THEN putpixel ts.tilex * 20 + i, ts.tiley * 20 + j, cutnpaste(i, j), 3
  NEXT j
 NEXT i
END IF 
END SUB

OPTION EXPLICIT '======== FIXME: move this up as code gets cleaned up =====================

SUB sprite (xw, yw, sets, perset, soff, atatime, info$(), zoom, fileset, font(), fullset AS INTEGER=NO, cursor_start AS INTEGER=0, cursor_top AS INTEGER=0)
STATIC spriteclip(2 + (32 * 40 * 8) \ 4) 'Because this is static we have to make it the max possible sprite-set size, which is the hero sprite set
STATIC clippedpal, clippedw, clippedh, paste
STATIC ss_save AS SpriteEditStatic

DIM mouse(4)

DIM ss AS SpriteEditState
WITH ss
 .framenum = 0
 .wide = xw
 .high = yw
 .perset = perset
 .size = .wide * .high / 2
 .setsize = .size * .perset
 .zoom = zoom
 '--Editor
 .x = 0
 .y = 0
 .zone.x = 0
 .zone.y = 0
 .hold = NO
 .gotmouse = setmouse(mouse())
 .drawcursor = 1
 .tool = draw_tool
 .airsize = 5
 .mist = 4
 .palindex = 1
 .previewpos.x = 319 - .wide
 .previewpos.y = 119
END WITH

DIM area(22) AS MouseArea
init_sprite_zones area(), ss

DIM sprites(atatime) AS Frame
DIM nulpal(8), placer(2 + (xw * yw * perset) \ 4), pclip(8)
DIM toolinfo(7) AS ToolInfoType
DIM workpal(8 * (atatime + 1))
REDIM poffset(large(sets, atatime))
DIM AS INTEGER do_paste = 0
DIM AS INTEGER paste_transparent = 0
DIM AS INTEGER debug_palettes = 0
DIM AS INTEGER tick = 0
'FOR Loop counters
DIM AS INTEGER i, j, o

DIM spritefile AS STRING
spritefile = game & ".pt" & fileset

DIM state AS MenuState
WITH state
 .pt = cursor_start
 .top = cursor_top
END WITH

WITH toolinfo(0)
 .name = "Draw"
 .icon = CHR(3)
 .shortcut = 32
 .cursor = 0
 .areanum = 6
END WITH
WITH toolinfo(1)
 .name = "Box"
 .icon = CHR(4)
 .shortcut = 48
 .cursor = 1
 .areanum = 7
END WITH
WITH toolinfo(2)
 .name = "Line"
 .icon = CHR(5)
 .shortcut = 38
 .cursor = 2
 .areanum = 8
END WITH
WITH toolinfo(3)
 .name = "Fill"
 .icon = "F"
 .shortcut = 33
 .cursor = 3
 .areanum = 9
END WITH
WITH toolinfo(4)
 .name = "Oval"
 .icon = "O"
 .shortcut = 24
 .cursor = 2
 .areanum = 10
END WITH
WITH toolinfo(5)
 .name = "Air"
 .icon = "A"
 .shortcut = 30
 .cursor = 3
 .areanum = 11
END WITH
WITH toolinfo(6)
 .name = "Mark"
 .icon = "M"
 .shortcut = 50
 .cursor = 2
 .areanum = 21
END WITH
WITH toolinfo(7)
 .name = "Clone"
 .icon = "C"
 .shortcut = 46
 .cursor = 3
 .areanum = 22
END WITH
FOR i = 0 TO 15
 poke8bit nulpal(), i, i
NEXT i
loaddefaultpals fileset, poffset(), sets
GOSUB loadalluc

setkeys
DO
 setwait 55
 setkeys
 state.tog = state.tog XOR 1
 IF keyval(scESC) > 1 THEN EXIT DO
 IF keyval(scF1) > 1 THEN
  IF fullset THEN
   show_help "sprite_pickset_fullset"
  ELSE
   show_help "sprite_pickset"
  END IF
 END IF
 IF keyval(29) > 0 AND keyval(14) > 1 THEN
  GOSUB savealluc
  cropafter state.pt, sets, 0, spritefile, ss.setsize, 1
  clearpage 3
  GOSUB loadalluc
 END IF
 IF enter_or_space() THEN
  GOSUB savealluc
  GOSUB spriteage
  GOSUB loadalluc
 END IF
 IF keyval(scCtrl) > 0 AND keyval(scF) > 1 THEN
  IF fullset = NO AND perset > 1 THEN
   GOSUB savealluc
   sprite xw * perset, yw, sets, 1, soff, atatime, info$(), 1, fileset, font(), YES, state.pt, state.top
   GOSUB loadalluc
  END IF
 END IF
 IF keyval(73) > 1 THEN
  GOSUB savealluc
  state.pt = large(state.pt - atatime, 0)
  state.top = state.pt
  GOSUB loadalluc
 END IF
 IF keyval(81) > 1 THEN
  GOSUB savealluc
  state.top = large(small(state.pt, sets - atatime), 0)
  state.pt = small(state.pt + atatime, sets)
  GOSUB loadalluc
 END IF
 IF keyval(71) > 1 THEN
  GOSUB savealluc
  state.pt = 0
  state.top = 0
  GOSUB loadalluc
 END IF
 IF keyval(79) > 1 THEN
  GOSUB savealluc
  state.pt = sets
  state.top = large(small(state.pt, sets - atatime), 0)
  GOSUB loadalluc
 END IF
 IF keyval(72) > 1 THEN
  state.pt = large(state.pt - 1, 0)
  IF state.pt < state.top THEN
   GOSUB savealluc
   state.top = state.pt
   GOSUB loadalluc
  END IF
 END IF
 IF keyval(80) > 1 AND state.pt < 32767 THEN
  state.pt = state.pt + 1
  IF needaddset(state.pt, sets, "graphics") THEN
   '--Add a new blank sprite set
   setpicstuf buffer(), ss.setsize, -1
   FOR i = 0 TO ss.setsize / 2
    buffer(i) = 0
   NEXT i
   storeset spritefile, state.pt, 0
   '-- re-size the array that stores the default palette offset
   REDIM PRESERVE poffset(large(sets, atatime))
   '--add a new blank default palette
   poffset(state.pt) = 0
   GOSUB loadalluc
  END IF
  IF state.pt > state.top + atatime THEN
   GOSUB savealluc
   state.top = state.top + 1
   GOSUB loadalluc
  END IF
 END IF
 IF keyval(75) > 1 THEN ss.framenum = large(ss.framenum - 1, 0)
 IF keyval(77) > 1 THEN ss.framenum = small(ss.framenum + 1, ss.perset - 1)
 IF keyval(26) > 1 THEN
  changepal poffset(state.pt), -1, workpal(), state.pt - state.top
 END IF
 IF keyval(27) > 1 THEN
  changepal poffset(state.pt), 1, workpal(), state.pt - state.top
 END IF
 '--copying
 IF (keyval(29) > 0 AND keyval(82) > 1) OR ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(83) > 0) OR (keyval(29) > 0 AND keyval(46) > 1) THEN 
  loadsprite spriteclip(), 0, ss.framenum * ss.size, soff * (state.pt - state.top), ss.wide, ss.high, 3
  paste = 1
  clippedw = ss.wide
  clippedh = ss.high
 END IF
 '--pasting
 do_paste = 0
 IF (((keyval(42) > 0 OR keyval(54) > 0) AND keyval(82) > 1) OR (keyval(29) > 0 AND keyval(47) > 1)) AND paste = 1 THEN
  do_paste = -1
  paste_transparent = 0
 END IF
 IF (keyval(29) > 0 AND keyval(20) > 1) AND paste = 1 THEN
  do_paste = -1
  paste_transparent = -1
 END IF
 IF do_paste THEN
  do_paste = 0
  loadsprite placer(), 0, ss.framenum * ss.size, soff * (state.pt - state.top), ss.wide, ss.high, 3
  rectangle 0, 0, ss.wide, ss.high, 0, dpage
  drawsprite placer(), 0, nulpal(), 0, 0, 0, dpage
  IF NOT paste_transparent THEN
   rectangle 0, 0, clippedw, clippedh, 0, dpage
  END IF
  drawsprite spriteclip(), 0, nulpal(), 0, 0, 0, dpage
  getsprite placer(), 0, 0, 0, ss.wide, ss.high, dpage
  stosprite placer(), 0, ss.framenum * ss.size, soff * (state.pt - state.top), 3
  spriteedit_save_what_you_see(spritefile, state.pt, state.top, sets, ss, soff, placer(), workpal(), poffset())
 END IF
 IF keyval(59) > 1 THEN
  debug_palettes = debug_palettes XOR 1
 END IF
 'draw sprite sets
 rectangle 0, 0, 320, 200, uilook(uiDisabledItem), dpage
 rectangle 4 + (ss.framenum * (ss.wide + 1)), (state.pt - state.top) * (ss.high + 5), ss.wide + 2, ss.high + 2, uilook(uiText), dpage
 FOR i = state.top TO small(state.top + atatime, sets)
  FOR o = 0 TO ss.perset - 1
   rectangle 5 + (o * (ss.wide + 1)), 1 + ((i - state.top) * (ss.high + 5)), ss.wide, ss.high, 0, dpage
   loadsprite placer(), 0, ss.size * o, soff * (i - state.top), ss.wide, ss.high, 3
   drawsprite placer(), 0, workpal(), (i - state.top) * 16, 5 + (o * (ss.wide + 1)), 1 + ((i - state.top) * (ss.high + 5)), dpage
  NEXT o
 NEXT i
 textcolor uilook(uiMenuItem), 0
 printstr "Palette " & poffset(state.pt), 320 - (LEN("Palette " & poffset(state.pt)) * 8), 0, dpage
 FOR i = 0 TO 15
  rectangle 271 + i * 3, 8, 3, 8, peek8bit(workpal(), (state.pt - state.top) * 16 + i), dpage
 NEXT i
 IF debug_palettes THEN
   FOR j = 0 TO atatime
     FOR i = 0 TO 15
      rectangle 271 + i * 3, 40 + j * 5, 3, 4, peek8bit(workpal(), j * 16 + i), dpage
     NEXT i
   NEXT j
 END IF
 printstr "Set " & state.pt, 320 - (LEN("Set " & state.pt) * 8), 16, dpage
 printstr info$(ss.framenum), 320 - (LEN(info$(ss.framenum)) * 8), 24, dpage
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
changepal poffset(state.pt), 0, workpal(), state.pt - state.top
GOSUB savealluc
savedefaultpals fileset, poffset(), sets
clearpage 0
clearpage 1
clearpage 2
clearpage 3
EXIT SUB

spriteage:
ss.delay = 10
ss.lastpos.x = -1
ss.lastpos.y = -1
ss.undodepth = 0
ss.undoslot = 0
ss.undomax = (32000 \ ss.size) - 1
GOSUB spedbak
loadsprite placer(), 0, ss.framenum * ss.size, soff * (state.pt - state.top), ss.wide, ss.high, 3
setkeyrepeat 25, 5
setkeys
DO
 setwait 17, 70
 setkeys
 IF ss.gotmouse THEN
  readmouse mouse()
  ss.zonecursor = 0
  ss.zonenum = mouseover(mouse(), ss.zone.x, ss.zone.y, ss.zonecursor, area())
 END IF
 IF keyval(scESC) > 1 THEN
  IF ss.hold = YES THEN
   GOSUB resettool
  ELSE
   stosprite placer(), 0, ss.framenum * ss.size, soff * (state.pt - state.top), 3
   GOSUB resettool
   EXIT DO
  END IF
 END IF
 IF keyval(scF1) > 1 THEN show_help "sprite_editor"
 IF ss.delay = 0 THEN
  GOSUB sprctrl
 END IF
 ss.delay = large(ss.delay - 1, 0)
 copypage 2, dpage  'moved this here to cover up residue on dpage (which was there before I got here!)
 spriteedit_display ss, ss_save, state, placer(), workpal(), poffset(), info$(), toolinfo(), area(), mouse()
 SWAP vpage, dpage
 setvispage vpage
 'blank the sprite area
 rectangle ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, 0, dpage
 tick = 0
 IF dowait THEN tick = 1: state.tog = state.tog XOR 1
LOOP
setkeyrepeat
j = state.pt
spriteedit_save_what_you_see(spritefile, j, state.top, sets, ss, soff, placer(), workpal(), poffset())
changepal poffset(state.pt), 0, workpal(), state.pt - state.top
RETRACE

sprctrl:
IF mouse(2) = 0 AND keyval(57) = 0 THEN
 ss.lastpos.x = -1
 ss.lastpos.y = -1
END IF
IF keyval(4) > 1 THEN setvispage 3: waitforanykey
IF keyval(41) > 1 THEN ss.hidemouse = ss.hidemouse XOR 1
IF keyval(51) > 1 AND ss.palindex > 0 THEN ss.palindex -= 1
IF keyval(52) > 1 AND ss.palindex < 15 THEN ss.palindex += 1
IF ss.zonenum = 2 THEN
 IF mouse(3) > 0 THEN ss.palindex = small(INT(ss.zone.x / 4), 15)
END IF
IF keyval(26) > 1 OR (ss.zonenum = 5 AND mouse(3) > 0) THEN
 changepal poffset(state.pt), -1, workpal(), state.pt - state.top
END IF
IF keyval(27) > 1 OR (ss.zonenum = 6 AND mouse(3) > 0) THEN
 changepal poffset(state.pt), 1, workpal(), state.pt - state.top
END IF
IF keyval(25) > 1 OR (ss.zonenum = 19 AND mouse(3) > 0) THEN '--call palette browser
 '--write changes so far
 stosprite placer(), 0, ss.framenum * ss.size, soff * (state.pt - state.top), 3
 '--save current palette
 storepal16 workpal(), state.pt - state.top, poffset(state.pt)
 poffset(state.pt) = pal16browse(poffset(state.pt), fileset, state.pt, ss.perset, ss.wide, ss.high)
 getpal16 workpal(), state.pt - state.top, poffset(state.pt)
END IF
'--UNDO
IF (keyval(29) > 0 AND keyval(44) > 1) OR (ss.zonenum = 20 AND mouse(3) > 0) THEN GOSUB readundospr
'--COPY (CTRL+INS,SHIFT+DEL,CTRL+C)
IF (keyval(29) > 0 AND keyval(82) > 1) OR ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(83) > 0) OR (keyval(29) > 0 AND keyval(46) > 1) THEN
 clippedw = ss.wide
 clippedh = ss.high
 stosprite placer(), 0, ss.framenum * ss.size, soff * (state.pt - state.top), 3
 loadsprite spriteclip(), 0, ss.framenum * ss.size, soff * (state.pt - state.top), ss.wide, ss.high, 3
 paste = 1
END IF
'--PASTE (SHIFT+INS,CTRL+V)
IF (((keyval(42) > 0 OR keyval(54) > 0) AND keyval(82) > 1) OR (keyval(29) > 0 AND keyval(47) > 1)) AND paste = 1 THEN
 rectangle 0, 0, ss.wide, ss.high, 0, dpage
 drawsprite placer(), 0, nulpal(), 0, 0, 0, dpage
 drawsprite spriteclip(), 0, nulpal(), 0, 0, 0, dpage, 0
 getsprite placer(), 0, 0, 0, ss.wide, ss.high, dpage
END IF
'--TRANSPARENT PASTE (CTRL+T)
IF (keyval(29) > 0 AND keyval(20) > 1) AND paste = 1 THEN
 rectangle 0, 0, ss.wide, ss.high, 0, dpage
 drawsprite placer(), 0, nulpal(), 0, 0, 0, dpage
 drawsprite spriteclip(), 0, nulpal(), 0, 0, 0, dpage
 getsprite placer(), 0, 0, 0, ss.wide, ss.high, dpage
END IF
'--COPY PALETTE (ALT+C)
IF keyval(56) > 0 AND keyval(46) > 1 THEN
 FOR i = 0 TO 7
  pclip(i) = workpal(i + (state.pt - state.top) * 8)
 NEXT
 clippedpal = 1
END IF
'--PASTE PALETTE (ALT+V)
IF keyval(56) > 0 AND keyval(47) > 1 THEN
 IF clippedpal THEN
  FOR i = 0 TO 8
   workpal(i + (state.pt - state.top) * 8) = pclip(i)
  NEXT
 END IF
END IF
ss.curcolor = peek8bit(workpal(), (state.pt - state.top) * 16 + ss.palindex)
IF keyval(56) > 0 THEN
 IF keyval(72) > 1 AND ss.curcolor > 15 THEN ss.curcolor -= 16
 IF keyval(80) > 1 AND ss.curcolor < 240 THEN ss.curcolor += 16
 IF keyval(75) > 1 AND ss.curcolor > 0 THEN ss.curcolor -= 1
 IF keyval(77) > 1 AND ss.curcolor < 255 THEN ss.curcolor += 1
END IF
IF mouse(3) = 1 AND ss.zonenum = 3 THEN
 ss.curcolor = INT(INT(ss.zone.y / 6) * 16) + INT(ss.zone.x / 4)
END IF
poke8bit workpal(), (state.pt - state.top) * 16 + ss.palindex, ss.curcolor
IF keyval(56) = 0 THEN
 WITH ss
  .fixmouse = NO
  IF slowkey(72, 6) THEN .y = large(0, .y - 1):      .fixmouse = YES
  IF slowkey(80, 6) THEN .y = small(ss.high - 1, .y + 1): .fixmouse = YES
  IF slowkey(75, 6) THEN .x = large(0, .x - 1):      .fixmouse = YES
  IF slowkey(77, 6) THEN .x = small(ss.wide - 1, .x + 1): .fixmouse = YES
 END WITH
 IF ss.fixmouse THEN
  IF ss.zonenum = 1 THEN
   ss.zone.x = ss.x * ss.zoom + INT(ss.zoom / 2)
   ss.zone.y = ss.y * ss.zoom + INT(ss.zoom / 2)
   mouse(0) = area(0).x + ss.zone.x 
   mouse(1) = area(0).y + ss.zone.y
   movemouse mouse(0), mouse(1)
  END IF 
  IF ss.zonenum = 14 THEN
   ss.zone.x = ss.x
   ss.zone.y = ss.y
   mouse(0) = area(13).x + ss.zone.x 
   mouse(1) = area(13).y + ss.zone.y
   movemouse mouse(0), mouse(1)
  END IF
 END IF
END IF
IF ss.zonenum = 1 THEN
 ss.x = INT(ss.zone.x / ss.zoom)
 ss.y = INT(ss.zone.y / ss.zoom)
END IF
IF ss.tool = airbrush_tool THEN '--adjust airbrush
 IF mouse(3) = 1 OR mouse(2) = 1 THEN
  IF ss.zonenum = 15 THEN ss.airsize = large(ss.airsize - tick, 1)
  IF ss.zonenum = 17 THEN ss.airsize = small(ss.airsize + tick, 80)
  IF ss.zonenum = 16 THEN ss.mist = large(ss.mist - tick, 1)
  IF ss.zonenum = 18 THEN ss.mist = small(ss.mist + tick, 99)
 END IF
 IF keyval(12) > 1 OR keyval(74) > 1 THEN
  IF keyval(29) > 0 THEN
   ss.mist = large(ss.mist - 1, 1)
  ELSE
   ss.airsize = large(ss.airsize - 1, 1)
  END IF
 END IF
 IF keyval(13) > 1 OR keyval(78) > 1 THEN
  IF keyval(29) > 0 THEN
   ss.mist = small(ss.mist + 1, 99)
  ELSE
   ss.airsize = small(ss.airsize + 1, 80)
  END IF
 END IF
END IF
IF ss.zonenum = 14 THEN
 ss.x = ss.zone.x
 ss.y = ss.zone.y
END IF
IF ((ss.zonenum = 1 OR ss.zonenum = 14) AND (mouse(3) = 1 OR mouse(2) = 1)) OR keyval(57) > 0 THEN
 SELECT CASE ss.tool
  CASE draw_tool
   GOSUB putdot
  CASE box_tool
   IF mouse(3) > 0 OR keyval(57) > 1 THEN
    IF ss.hold THEN
     ss.hold = NO: GOSUB drawsquare
    ELSE
     ss.hold = YES
     ss.holdpos.x = ss.x
     ss.holdpos.y = ss.y
    END IF
   END IF
  CASE line_tool
   IF mouse(3) > 0 OR keyval(57) > 1 THEN
    IF ss.hold = YES THEN
     ss.hold = NO
     GOSUB straitline
    ELSE
     ss.hold = YES
     ss.holdpos.x = ss.x
     ss.holdpos.y = ss.y
    END IF
   END IF
  CASE fill_tool
   IF mouse(3) > 0 OR keyval(57) > 1 THEN
    GOSUB floodfill
   END IF
  CASE oval_tool
   IF mouse(3) > 0 OR keyval(57) > 1 THEN
    IF ss.hold = NO THEN
     '--start oval
     ss.holdpos.x = ss.x
     ss.holdpos.y = ss.y
     ss.squish.x = 0
     ss.squish.y = 0
     ss.radius = 0
     ss.hold = YES
    ELSE
     GOSUB drawoval
     ss.hold = NO
    END IF
   END IF
  CASE airbrush_tool
   GOSUB sprayspot
  CASE mark_tool
   IF mouse(3) > 0 OR keyval(57) > 1 THEN
    IF ss.hold THEN
     ss.hold = NO
     drawsprite placer(), 0, nulpal(), 0, ss.previewpos.x, ss.previewpos.y, dpage
     getsprite ss_save.clonebuf(), 0, ss.previewpos.x + small(ss.x, ss.holdpos.x), ss.previewpos.y + small(ss.y, ss.holdpos.y), ABS(ss.x - ss.holdpos.x) + 1, ABS(ss.y - ss.holdpos.y) + 1, dpage
     ss.holdpos.x = ss_save.clonebuf(0) \ 2
     ss.holdpos.y = ss_save.clonebuf(1) \ 2
     ss_save.clonemarked = YES
     ss.tool = clone_tool ' auto-select the clone tool after marking
    ELSE
     ss.hold = YES
     ss.holdpos.x = ss.x
     ss.holdpos.y = ss.y
    END IF
   END IF
  CASE clone_tool
   IF mouse(3) > 0 OR keyval(57) > 1 THEN
    IF ss_save.clonemarked THEN
     IF ss.lastpos.x = -1 AND ss.lastpos.y = -1 THEN
      GOSUB writeundospr
     END IF
     drawsprite placer(), 0, nulpal(), 0, ss.previewpos.x, ss.previewpos.y, dpage
     drawsprite ss_save.clonebuf(), 0, nulpal(), 0, ss.previewpos.x + ss.x - ss.holdpos.x, ss.previewpos.y + ss.y - ss.holdpos.y, dpage
     getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
     ss.lastpos.x = ss.x
     ss.lastpos.y = ss.y
    ELSE
     ss.tool = mark_tool ' select selection tool if clone is not available
     ss.hold = YES
     ss.holdpos.x = ss.x
     ss.holdpos.y = ss.y
    END IF
   END IF
 END SELECT
END IF
IF ss.hold = YES AND ss.tool = oval_tool THEN
 ss.radius = large(ABS(ss.x - ss.holdpos.x), ABS(ss.y - ss.holdpos.y))
END IF
FOR i = 0 TO 7
 IF (mouse(3) > 0 AND ss.zonenum = toolinfo(i).areanum + 1) OR keyval(toolinfo(i).shortcut) > 1 THEN
  ss.tool = i
  GOSUB resettool
  ss.drawcursor = toolinfo(i).cursor + 1
 END IF
NEXT i
IF ss.tool = clone_tool THEN
 ' For clone brush tool, enter/right-click moves the handle point
 IF ss.readjust THEN
  IF keyval(28) = 0 AND mouse(2) = 0 THEN ' click or key release
   ss.readjust = NO
   ss.holdpos.x += (ss.x - ss.adjustpos.x)
   ss.holdpos.y += (ss.y - ss.adjustpos.y)
   ss.adjustpos.x = 0
   ss.adjustpos.y = 0
  END IF
 ELSE
  IF (keyval(28) AND 5) OR mouse(2) = 2 THEN
   ss.readjust = YES
   ss.adjustpos.x = ss.x
   ss.adjustpos.y = ss.y
  END IF
 END IF
ELSE
 ' For all other tools, pick a color
 IF keyval(28) > 1 OR (ss.zonenum = 1 AND mouse(2) = 2) THEN
  drawsprite placer(), 0, nulpal(), 0, ss.previewpos.x, ss.previewpos.y, dpage
  ss.palindex = readpixel(ss.previewpos.x + ss.x, ss.previewpos.y + ss.y, dpage)
 END IF
END IF
IF keyval(14) > 1 OR (ss.zonenum = 4 AND mouse(3) > 0) THEN wardsprite placer(), 0, nulpal(), 0, ss.previewpos.x, ss.previewpos.y, dpage: getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
IF keyval(scCapslock) > 0 THEN
 IF slowkey(72, 6) THEN
  rectangle ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, 0, dpage
  drawsprite placer(), 0, nulpal(), 0, ss.previewpos.x, ss.previewpos.y - 1, dpage
  getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
 END IF
 IF slowkey(80, 6) THEN
  rectangle ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, 0, dpage
  drawsprite placer(), 0, nulpal(), 0, ss.previewpos.x, ss.previewpos.y + 1, dpage
  getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
 END IF
 IF slowkey(75, 6) THEN
  rectangle ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, 0, dpage
  drawsprite placer(), 0, nulpal(), 0, ss.previewpos.x - 1, ss.previewpos.y, dpage
  getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
 END IF
 IF slowkey(77, 6) THEN
  rectangle ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, 0, dpage
  drawsprite placer(), 0, nulpal(), 0, ss.previewpos.x + 1, ss.previewpos.y, dpage
  getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
 END IF
END IF
IF keyval(23) > 1 OR (ss.zonenum = 13 AND mouse(3) > 0) THEN
 spriteedit_import16 ss, ss_save, state, placer(), workpal(), poffset(), info$(), toolinfo(), area(), mouse()
 GOSUB spedbak
 setkeyrepeat 25, 5
END IF
RETRACE

resettool:
ss.hold = NO
ss.readjust = NO
ss.adjustpos.x = 0
ss.adjustpos.y = 0
RETRACE

spedbak:
clearpage 2
rectangle 3, 0, ss.wide * ss.zoom + 2, ss.high * ss.zoom + 2, uilook(uiText), 2
rectangle 4, 1, ss.wide * ss.zoom, ss.high * ss.zoom, 0, 2
rectangle 246, 109, 67, 8, uilook(uiText), 2
rectangle 247, 110, 65, 6, uilook(uiBackground), 2
rectangle ss.previewpos.x - 1, ss.previewpos.y - 1, ss.wide + 2, ss.high + 2, uilook(uiText), 2
rectangle ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, 0, 2
area(0).w = ss.wide * ss.zoom
area(0).h = ss.high * ss.zoom
area(13).w = ss.wide
area(13).h = ss.high
RETRACE

floodfill:
GOSUB writeundospr
rectangle ss.previewpos.x - 1, ss.previewpos.y - 1, ss.wide + 2, ss.high + 2, uilook(uiHighlight), dpage
rectangle ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, 0, dpage
drawsprite placer(), 0, nulpal(), 0, ss.previewpos.x, ss.previewpos.y, dpage
paintat ss.previewpos.x + ss.x, ss.previewpos.y + ss.y, ss.palindex, dpage, buffer(), 16384
getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
RETRACE

sprayspot:
IF ss.lastpos.x = -1 AND ss.lastpos.y = -1 THEN GOSUB writeundospr
drawsprite placer(), 0, nulpal(), 0, ss.previewpos.x, ss.previewpos.y, dpage
airbrush ss.previewpos.x + ss.x, ss.previewpos.y + ss.y, ss.airsize, ss.mist, ss.palindex, dpage
getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
ss.lastpos.x = ss.x
ss.lastpos.y = ss.y
RETRACE

writeundospr:
stosprite placer(), 0, ss.undoslot * ss.size, 100, 3
ss.undoslot = loopvar(ss.undoslot, 0, ss.undomax, 1)
ss.undodepth = small(ss.undodepth + 1, ss.undomax + 1)
RETRACE

readundospr:
IF ss.undodepth > 0 THEN
 ss.undodepth = ss.undodepth - 1
 ss.undoslot = loopvar(ss.undoslot, 0, ss.undomax, -1)
 loadsprite placer(), 0, ss.undoslot * ss.size, 100, ss.wide, ss.high, 3
END IF
RETRACE

putdot:
drawsprite placer(), 0, nulpal(), 0, ss.previewpos.x, ss.previewpos.y, dpage
IF ss.lastpos.x = -1 AND ss.lastpos.y = -1 THEN
 GOSUB writeundospr
 putpixel ss.previewpos.x + ss.x, ss.previewpos.y + ss.y, ss.palindex, dpage
ELSE
 drawline ss.previewpos.x + ss.x, ss.previewpos.y + ss.y, ss.previewpos.x + ss.lastpos.x, ss.previewpos.y + ss.lastpos.y, ss.palindex, dpage
END IF
getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
ss.lastpos.x = ss.x
ss.lastpos.y = ss.y
RETRACE

drawoval:
GOSUB writeundospr
drawsprite placer(), 0, nulpal(), 0, ss.previewpos.x, ss.previewpos.y, dpage
ellipse ss.previewpos.x + ss.holdpos.x, ss.previewpos.y + ss.holdpos.y, ss.radius, ss.palindex, dpage, ss.squish.x, ss.squish.y
getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
RETRACE

drawsquare:
GOSUB writeundospr
drawsprite placer(), 0, nulpal(), 0, ss.previewpos.x, ss.previewpos.y, dpage
rectangle ss.previewpos.x + small(ss.x, ss.holdpos.x), ss.previewpos.y + small(ss.y, ss.holdpos.y), ABS(ss.x - ss.holdpos.x) + 1, ABS(ss.y - ss.holdpos.y) + 1, ss.palindex, dpage
getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
RETRACE

straitline:
GOSUB writeundospr
drawsprite placer(), 0, nulpal(), 0, ss.previewpos.x, ss.previewpos.y, dpage
drawline ss.previewpos.x + ss.x, ss.previewpos.y + ss.y, ss.previewpos.x + ss.holdpos.x, ss.previewpos.y + ss.holdpos.y, ss.palindex, dpage
getsprite placer(), 0, ss.previewpos.x, ss.previewpos.y, ss.wide, ss.high, dpage
RETRACE

savealluc:
FOR j = state.top TO state.top + atatime
 spriteedit_save_what_you_see(spritefile, j, state.top, sets, ss, soff, placer(), workpal(), poffset()) 
NEXT j
RETRACE

loadalluc:
FOR j = state.top TO state.top + atatime
 spriteedit_load_what_you_see(spritefile, j, state.top, sets, ss, soff, placer(), workpal(), poffset())
NEXT
RETRACE

END SUB '----END of sprite()

SUB spriteedit_display(BYREF ss AS SpriteEditState, BYREF ss_save AS SpriteEditStatic, state AS MenuState, placer(), workpal(), poffset(), info$(), toolinfo() AS ToolInfoType, area() AS MouseArea, mouse())
 ss.curcolor = peek8bit(workpal(), ss.palindex + (state.pt - state.top) * 16)
 rectangle 247 + ((ss.curcolor - (INT(ss.curcolor / 16) * 16)) * 4), 0 + (INT(ss.curcolor / 16) * 6), 5, 7, uilook(uiText), dpage
 DIM AS INTEGER i, o
 FOR i = 0 TO 15
  FOR o = 0 TO 15
   rectangle 248 + (i * 4), 1 + (o * 6), 3, 5, o * 16 + i, dpage
  NEXT o
 NEXT i
 textcolor uilook(uiText), uilook(uiDisabledItem): IF ss.zonenum = 5 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
 printstr CHR(27), 248, 100, dpage
 textcolor uilook(uiText), uilook(uiDisabledItem): IF ss.zonenum = 6 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
 printstr CHR(26), 304, 100, dpage
 textcolor uilook(uiText), 0
 printstr LEFT(" Pal", 4 - (LEN(STR(poffset(state.pt))) - 3)) & poffset(state.pt), 248, 100, dpage
 rectangle 247 + (ss.palindex * 4), 110, 5, 7, uilook(uiText), dpage
 FOR i = 0 TO 15
  rectangle 248 + (i * 4), 111, 3, 5, peek8bit(workpal(), i + (state.pt - state.top) * 16), dpage
 NEXT
 drawspritex placer(), 0, workpal(), (state.pt - state.top) * 16, 4, 1, dpage, ss.zoom, NO
 ss.curcolor = peek8bit(workpal(), ss.palindex + (state.pt - state.top) * 16)
 IF ss.hold = YES AND ss.tool = box_tool THEN
  rectangle 4 + small(ss.x, ss.holdpos.x) * ss.zoom, 1 + small(ss.y, ss.holdpos.y) * ss.zoom, (ABS(ss.x - ss.holdpos.x) + 1) * ss.zoom, (ABS(ss.y - ss.holdpos.y) + 1) * ss.zoom, ss.curcolor, dpage
  rectangle 4 + ss.holdpos.x * ss.zoom, 1 + ss.holdpos.y * ss.zoom, ss.zoom, ss.zoom, IIF(state.tog, uilook(uiBackground), uilook(uiText)), dpage
 END IF
 rectangle 4 + (ss.x * ss.zoom), 1 + (ss.y * ss.zoom), ss.zoom, ss.zoom, IIF(state.tog, uilook(uiBackground), uilook(uiText)), dpage
 drawsprite placer(), 0, workpal(), (state.pt - state.top) * 16, ss.previewpos.x, ss.previewpos.y, dpage, 0
 IF ss.hold = YES AND ss.tool = box_tool THEN
  rectangle ss.previewpos.x + small(ss.x, ss.holdpos.x), ss.previewpos.y + small(ss.y, ss.holdpos.y), ABS(ss.x - ss.holdpos.x) + 1, ABS(ss.y - ss.holdpos.y) + 1, ss.curcolor, dpage
  putpixel ss.previewpos.x + ss.holdpos.x, ss.previewpos.y + ss.holdpos.y, state.tog * 15, dpage
 END IF
 IF ss.hold = YES AND ss.tool = line_tool THEN
  drawline ss.previewpos.x + ss.x, ss.previewpos.y + ss.y, ss.previewpos.x + ss.holdpos.x, ss.previewpos.y + ss.holdpos.y, ss.curcolor, dpage
  drawline 5 + (ss.x * ss.zoom), 2 + (ss.y * ss.zoom), 5 + (ss.holdpos.x * ss.zoom), 2 + (ss.holdpos.y * ss.zoom), ss.curcolor, dpage
 END IF
 IF ss.hold = YES AND ss.tool = oval_tool THEN
  ellipse ss.previewpos.x + ss.holdpos.x, ss.previewpos.y + ss.holdpos.y, ss.radius, ss.curcolor, dpage, ss.squish.x, ss.squish.y
  ellipse 5 + (ss.holdpos.x * ss.zoom), 2 + (ss.holdpos.y * ss.zoom), ss.radius * ss.zoom, ss.curcolor, dpage, ss.squish.x, ss.squish.y
 END IF
 IF ss.tool = airbrush_tool THEN
  ellipse ss.previewpos.x + ss.x, ss.previewpos.y + ss.y, ss.airsize / 2, ss.curcolor, dpage, 0, 0
  ellipse 5 + (ss.x * ss.zoom), 2 + (ss.y * ss.zoom), (ss.airsize / 2) * ss.zoom, ss.curcolor, dpage, 0, 0
 END IF
 IF ss.hold = YES AND ss.tool = mark_tool AND state.tog = 0 THEN
  ss.curcolor = INT(RND * 255) ' Random color when marking a clone region
  emptybox 4 + small(ss.x, ss.holdpos.x) * ss.zoom, 1 + small(ss.y, ss.holdpos.y) * ss.zoom, (ABS(ss.x - ss.holdpos.x) + 1) * ss.zoom, (ABS(ss.y - ss.holdpos.y) + 1) * ss.zoom, ss.curcolor, ss.zoom, dpage
  emptybox ss.previewpos.x + small(ss.x, ss.holdpos.x), ss.previewpos.y + small(ss.y, ss.holdpos.y), ABS(ss.x - ss.holdpos.x) + 1, ABS(ss.y - ss.holdpos.y) + 1, ss.curcolor, 1, dpage
 END IF
 DIM temppos AS XYPair
 IF ss.tool = clone_tool AND ss_save.clonemarked = YES AND state.tog = 0 THEN
  temppos.x = ss.x - ss.holdpos.x
  temppos.y = ss.y - ss.holdpos.y
  IF ss.readjust THEN
   temppos.x += (ss.adjustpos.x - ss.x)
   temppos.y += (ss.adjustpos.y - ss.y)
  END IF
 IF ss.zoom = 4 THEN hugesprite ss_save.clonebuf(), workpal(), (state.pt - state.top) * 16, 4 + temppos.x * ss.zoom, 1 + temppos.y * ss.zoom, dpage
 IF ss.zoom = 2 THEN bigsprite ss_save.clonebuf(), workpal(), (state.pt - state.top) * 16, 4 + temppos.x * ss.zoom, 1 + temppos.y * ss.zoom, dpage
  drawsprite ss_save.clonebuf(), 0, workpal(), (state.pt - state.top) * 16, ss.previewpos.x + temppos.x, ss.previewpos.y + temppos.y, dpage
 END IF
 putpixel ss.previewpos.x + ss.x, ss.previewpos.y + ss.y, state.tog * 15, dpage
 textcolor uilook(uiMenuItem), 0
 printstr info$(ss.framenum), 0, 182, dpage
 printstr "Tool:" & toolinfo(ss.tool).name, 0, 190, dpage
 DIM bgcol AS INTEGER
 DIM fgcol AS INTEGER
 FOR i = 0 TO 7
  fgcol = uilook(uiMenuItem)
  bgcol = uilook(uiDisabledItem)
  IF ss.tool = i THEN
   fgcol = uilook(uiText)
   bgcol = uilook(uiMenuItem)
  END IF
  IF ss.zonenum = toolinfo(i).areanum + 1 THEN bgcol = uilook(uiSelectedDisabled)
  textcolor fgcol, bgcol
  printstr toolinfo(i).icon, area(toolinfo(i).areanum).x, area(toolinfo(i).areanum).y, dpage
 NEXT i
 textcolor uilook(uiMenuItem), uilook(uiDisabledItem)
 IF ss.zonenum = 4 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
 printstr CHR(7), 182, 190, dpage
 textcolor uilook(uiMenuitem), uilook(uiDisabledItem)
 IF ss.zonenum = 13 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
 printstr "I", 194, 190, dpage
 IF ss.undodepth = 0 THEN
  textcolor uilook(uiBackground), uilook(uiDisabledItem)
 ELSE
  textcolor uilook(uiMenuItem), uilook(uiDisabledItem)
 END IF
 IF ss.zonenum = 20 AND ss.undodepth > 0 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
 printstr "UNDO", 170, 182, dpage
 IF ss.tool = airbrush_tool THEN
  textcolor uilook(uiMenuItem), 0
  printstr "SIZE" & ss.airsize, 218, 182, dpage
  printstr "MIST" & ss.mist, 218, 190, dpage
  textcolor uilook(uiMenuItem), uilook(uiDisabledItem)
  IF ss.zonenum = 15 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
  printstr CHR(27), 210, 182, dpage
  textcolor uilook(uiMenuItem), uilook(uiDisabledItem)
  IF ss.zonenum = 16 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
  printstr CHR(27), 210, 190, dpage
  textcolor uilook(uiMenuItem), uilook(uiDisabledItem)
  IF ss.zonenum = 17 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
  printstr CHR(26), 266, 182, dpage
  textcolor uilook(uiMenuItem), uilook(uiDisabledItem)
  IF ss.zonenum = 18 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
  printstr CHR(26), 266, 190, dpage
 END IF
 IF ss.gotmouse THEN
  IF ss.zonecursor = -1 THEN
   IF ss.hidemouse THEN ss.zonecursor = -2 ELSE ss.zonecursor = ss.drawcursor
  END IF
  IF state.tog THEN
   textcolor uilook(uiText), 0
  ELSE
   textcolor uilook(uiDescription), 0
  END IF
  printstr CHR(2 + ss.zonecursor), small(large(mouse(0) - 2, 0), 311), small(large(mouse(1) - 2, 0), 191), dpage
 END IF
END SUB

SUB init_sprite_zones(area() AS MouseArea, ss AS SpriteEditState)
 DIM i AS INTEGER
 'DRAWING ZONE
 area(0).x = 4
 area(0).y = 1
 area(0).hidecursor = YES
 'PALETTE ZONE
 area(1).x = 248
 area(1).y = 111
 area(1).w = 64
 area(1).h = 6
 area(1).hidecursor = NO
 'MASTER PAL ZONE
 area(2).x = 248
 area(2).y = 1
 area(2).w = 64
 area(2).h = 96
 area(2).hidecursor = NO
 'FLIP BUTTON
 area(3).x = 182
 area(3).y = 190
 area(3).w = 8
 area(3).h = 10
 area(3).hidecursor = NO
 'PREV PAL BUTTON
 area(4).x = 248
 area(4).y = 100
 area(4).w = 8
 area(4).h = 8
 area(4).hidecursor = NO
 'NEXT PAL BUTTON
 area(5).x = 304
 area(5).y = 100
 area(5).w = 8
 area(5).h = 8
 area(5).hidecursor = NO
 'TOOL BUTTONS (more below at 21,22)
 FOR i = 0 TO 5
  area(6 + i).x = 80 + i * 10
  area(6 + i).y = 190
  area(6 + i).w = 8
  area(6 + i).h = 10
  area(6 + i).hidecursor = NO
 NEXT i
 'IMPORT BUTTON
 area(12).x = 194
 area(12).y = 190
 area(12).w = 8
 area(12).h = 10
 area(12).hidecursor = NO
 'SMALL DRAWING AREA
 area(13).x = ss.previewpos.x
 area(13).y = ss.previewpos.y
 area(13).hidecursor = YES
 'LESS AIRBRUSH AREA
 area(14).x = 210
 area(14).y = 182
 area(14).w = 8
 area(14).h = 8
 area(14).hidecursor = NO
 'LESS AIRBRUSH MIST
 area(15).x = 210
 area(15).y = 190
 area(15).w = 8
 area(15).h = 8
 area(15).hidecursor = NO
 'MORE AIRBRUSH AREA
 area(16).x = 266
 area(16).y = 182
 area(16).w = 8
 area(16).h = 8
 area(16).hidecursor = NO
 'MORE AIRBRUSH MIST
 area(17).x = 266
 area(17).y = 190
 area(17).w = 8
 area(17).h = 8
 area(17).hidecursor = NO
 'PALETTE NUMBER
 area(18).x = 256
 area(18).y = 100
 area(18).w = 48
 area(18).h = 8
 area(18).hidecursor = NO
 'UNDO BUTTON
 area(19).x = 170
 area(19).y = 182
 area(19).w = 32
 area(19).h = 8
 area(19).hidecursor = NO
 FOR i = 0 TO 1
  area(21 + i).x = 140 + i * 10
  area(21 + i).y = 190
  area(21 + i).w = 8
  area(21 + i).h = 10
  area(21 + i).hidecursor = NO
 NEXT i
END SUB

'======== FIXME: move this up as code gets cleaned up ===========
OPTION EXPLICIT

SUB spriteedit_load_what_you_see(spritefile AS STRING, j, top, sets, ss AS SpriteEditState, soff, placer(), workpal(), poffset())
 DIM i AS INTEGER
 getpal16 workpal(), j - top, poffset(j)
 IF j <= sets THEN
  setpicstuf buffer(), ss.setsize, 2
  loadset spritefile, large(j, 0), 0
  FOR i = 0 TO (ss.perset - 1)
   loadsprite placer(), 0, ss.size * i, 0, ss.wide, ss.high, 2
   stosprite placer(), 0, ss.size * i, soff * (j - top), 3
  NEXT i
 END IF
END SUB

SUB spriteedit_save_what_you_see(spritefile AS STRING, j, top, sets, ss AS SpriteEditState, soff, placer(), workpal(), poffset())
 DIM i AS INTEGER
 IF j <= sets THEN
  setpicstuf buffer(), ss.setsize, 2
  FOR i = 0 TO (ss.perset - 1)
   loadsprite placer(), 0, ss.size * i, soff * (j - top), ss.wide, ss.high, 3
   stosprite placer(), 0, ss.size * i, 0, 2
  NEXT i
  storeset spritefile, large(j, 0), 0
 END IF
END SUB

SUB spriteedit_import16(BYREF ss AS SpriteEditState, BYREF ss_save AS SpriteEditStatic, BYREF state AS MenuState, placer() AS INTEGER, workpal() AS INTEGER, poffset() AS INTEGER, info() AS STRING, toolinfo() AS ToolInfoType, area() AS MouseArea, mouse())
 DIM palstate AS MenuState 
 DIM coltemp AS INTEGER
 DIM srcbmp AS STRING
 DIM pickpos AS XYPair
 DIM picksize AS XYPair
 DIM pixelval AS INTEGER
 DIM movespeed AS INTEGER
 
 STATIC default AS STRING
 
 setkeyrepeat
 srcbmp = browse(2, default, "*.bmp", "")
 IF srcbmp = "" THEN EXIT SUB
 '--------------------
 'DECIDE ABOUT PALETTE
 DIM pmenu(2) AS STRING
 pmenu(0) = "Overwrite Current Palette"
 pmenu(1) = "Import Without Palette"
 pmenu(2) = "Cancel Import"
 
 palstate.pt = 0
 palstate.last = 2
 setkeys
 DO
  setwait 110
  setkeys
  palstate.tog = palstate.tog XOR 1
  IF keyval(scESC) > 1 THEN EXIT SUB
  IF keyval(scF1) > 1 THEN show_help "sprite_import16"
  IF keyval(scLeft) > 1 OR keyval(scLeftBracket) > 1 THEN
   changepal poffset(state.pt), -1, workpal(), state.pt - state.top
  END IF
  IF keyval(scRight) > 1 OR keyval(scRightBracket) > 1 THEN
   changepal poffset(state.pt), 1, workpal(), state.pt - state.top
  END IF
  usemenu palstate
  IF enter_or_space() THEN
   IF palstate.pt = 2 THEN EXIT SUB
   EXIT DO
  END IF
  spriteedit_display ss, ss_save, state, placer(), workpal(), poffset(), info(), toolinfo(), area(), mouse()
  rectangle 4, 156, 208, 32, uilook(uiDisabledItem), dpage
  FOR i AS INTEGER = 0 TO 2
   coltemp = uilook(uiMenuItem): IF i = palstate.pt THEN coltemp = uilook(uiSelectedItem + palstate.tog)
   edgeprint pmenu(i), 8, 160 + (i * 8), coltemp, dpage
  NEXT i
  SWAP vpage, dpage
  setvispage vpage
  copypage 2, dpage
  dowait
 LOOP

 '---------------------
 'PICK BACKGROUND COLOR
 
 clearpage dpage
 DIM holdscreen AS INTEGER
 holdscreen = allocatepage

 'This loads the BMP onto a temporary screen page
 loadbmp srcbmp, 0, 0, holdscreen

 'Temporaraly update the palette. This will be done again after the transparent color is chosen
 DIM temppal(7)
 '--first copy the old palette in the current slot
 FOR i AS INTEGER = 0 To 7
  temppal(i) = workpal((state.pt - state.top) * 8 + i)
 NEXT i
 IF palstate.pt = 0 THEN
  convertbmppal srcbmp, master(), temppal(), 0
 END IF

 DIM temp_placer(2 + (ss.wide * ss.high * ss.perset) \ 4) AS INTEGER
 pickpos.x = 0
 pickpos.y = 0
 DIM bmpd(40) AS INTEGER 'Temp buffer for holding BMP header
 bmpinfo srcbmp, bmpd()
 '--set up cursor
 picksize.x = small(320, small(bmpd(1), ss.wide))
 picksize.y = small(200, small(bmpd(2), ss.high))
 
 setkeys
 DO
  setwait 110
  setkeys
  palstate.tog = palstate.tog XOR 1
  IF keyval(scESC) > 1 THEN 
  '--Cancel
   freepage holdscreen
   EXIT SUB
  END IF
  IF keyval(scF1) > 1 THEN show_help "sprite_import16_pickbackground"
  IF keyval(scALT) THEN movespeed = 9 ELSE movespeed = 1
  IF keyval(scUp) > 0 THEN pickpos.y = large(pickpos.y - movespeed, 0)
  IF keyval(scDown) > 0 THEN pickpos.y = small(pickpos.y + movespeed, picksize.y - 1)
  IF keyval(scleft) > 0 THEN pickpos.x = large(pickpos.x - movespeed, 0)
  IF keyval(scRight) > 0 THEN pickpos.x = small(pickpos.x + movespeed, picksize.x - 1)
  IF enter_or_space() THEN EXIT DO
  '--Draw a preview of the sprite
  getsprite temp_placer(), 0, 0, 0, ss.wide, ss.high, holdscreen
  drawspritex temp_placer(), 0, temppal(), 0, 0, 0, dpage, ss.zoom
  '--Draw the pixel cursor
  rectangle pickpos.x * ss.zoom, pickpos.y * ss.zoom, ss.zoom, ss.zoom, uilook(uiSelectedDisabled + palstate.tog), dpage
  edgeprint "Pick Background Color", 0, 190, uilook(uiMenuItem), dpage
  SWAP vpage, dpage
  setvispage vpage
  clearpage dpage
  dowait
 LOOP

 '--picked a transparent pixel
 pixelval = readpixel(pickpos.x, pickpos.y, holdscreen)
 '--swap the transparent pixels to 0
 FOR i AS INTEGER = 0 TO picksize.x - 1
  FOR o AS INTEGER = 0 TO picksize.y - 1
   IF readpixel(i, o, holdscreen) = pixelval THEN
    putpixel i, o, 0, holdscreen
   ELSE
    IF readpixel(i, o, holdscreen) = 0 THEN
     putpixel i, o, pixelval, holdscreen
    END IF
   END IF
  NEXT o
 NEXT i
 '--swap the transparent palette entry to 0
 IF palstate.pt = 0 THEN
  convertbmppal srcbmp, master(), workpal(), 8 * (state.pt - state.top)
  'swap black with the transparent color
  poke8bit workpal(), pixelval + (state.pt - state.top) * 16, peek8bit(workpal(), 0 + (state.pt - state.top) * 16)
  poke8bit workpal(), 0 + (state.pt - state.top) * 16, 0
 END IF
 '--read the sprite
 getsprite placer(), 0, 0, 0, ss.wide, ss.high, holdscreen
 '--free the temp screen page
 freepage holdscreen
END SUB
