'OHRRPGCE CUSTOM - Mostly drawing-related routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z

#include "udts.bi"

'basic subs and functions
DECLARE SUB picktiletoedit (tmode%, pagenum%, mapfile$)
DECLARE SUB editmaptile (ts AS TileEditState, mover(), mouse(), area() AS MouseArea, toolinfo() AS ToolInfoType)
DECLARE SUB tilecut (ts AS TileEditState, mouse(), area() AS MouseArea)
DECLARE SUB refreshtileedit (mover%(), state AS TileEditState)
DECLARE SUB writeundoblock (mover%(), state AS TileEditState)
DECLARE SUB readundoblock (mover%(), state AS TileEditState)
DECLARE SUB fliptile (mover%(), ts AS TileEditState)
DECLARE SUB scrolltile (mover%(), ts AS TileEditState)
DECLARE SUB clicktile (mover%(), ts AS TileEditState, mouseclick%)
DECLARE SUB tilecopy (cutnpaste%(), ts AS TileEditState)
DECLARE SUB tilepaste (cutnpaste%(), ts AS TileEditState)
DECLARE SUB tiletranspaste (cutnpaste%(), ts AS TileEditState)
DECLARE SUB fixfilename (s$)
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
DECLARE SUB editbitset (array%(), wof%, last%, name() AS STRING)
DECLARE FUNCTION mouseover% (mouse%(), zox%, zoy%, zcsr%, area() AS MouseArea)
DECLARE SUB formation ()
DECLARE SUB enemydata ()
DECLARE SUB herodata ()
DECLARE SUB attackdata (atkdat$(), atklim%())
DECLARE SUB getnames (stat$(), max%)
DECLARE SUB statname ()
DECLARE FUNCTION sublist% (num%, s$())
DECLARE SUB maptile (font())
DECLARE SUB importmasterpal (f$, palnum%)
DECLARE FUNCTION inputfilename$ (query$, ext$, default$ = "")
declare sub loadwuc(spritefile$, j, top, sets, xw,yw, soff, perset, size,placer(), workpal(), poffset())
declare sub savewuc(spritefile$, j, top, sets, xw,yw, soff, perset, size,placer(), workpal(), poffset())

#include "compat.bi"
#include "allmodex.bi"
#include "common.bi"
#include "loading.bi"
#include "customsubs.bi"
#include "cglobals.bi"

#include "const.bi"

dim shared col as integer

REM $STATIC
SUB airbrush (x, y, d, m, c, p)
'airbrush thanks to Ironhoof (Russel Hamrick)

'AirBrush this rutine works VERY well parameters as fallows:
' AIRBRUSH x , y , diameter , mist_amount , color , page
' diameter sets the width & hight by square radius
' mist_amount sets how many pixels to place i put 100 and it ran fast so
' it works EXCELLENTLY with a mouse on the DTE =)

FOR count = 1 TO m
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
DIM menu$(10), submenu$(2), palmapping(255), bmpd(4)
DIM pmask(255) as RGBcolor, temppal(255) as RGBcolor
csr = 0
pt = 0

IF count = 0 THEN count = 1
clearpage 0
clearpage 1
clearpage 2
clearpage 3
menu$(0) = "Return to Main Menu"
menu$(1) = CHR$(27) + "Browse" + XSTR$(pt) + CHR$(26)
menu$(2) = "Replace current " + cap$
menu$(3) = "Append a new " + cap$
menu$(4) = "Disable palette colors for import"
menu$(5) = "Export current " + cap$ + " as BMP"
submenu$(0) = "Import with current Master Palette"
submenu$(1) = "Import with new Master Palette"
submenu$(2) = "Do not remap colours"
loadpalette pmask(), activepalette
loadpage game + f$, pt, 2

setkeys
DO
 setwait 55
 setkeys
 tog = tog XOR 1
 IF keyval(29) > 0 AND keyval(14) > 1 THEN
  this = count - 1
  cropafter pt, this, 3, game + f$, -1, 1
  count = this + 1
 END IF
 IF keyval(1) > 1 THEN EXIT DO
 usemenu csr, 0, 0, 5, 24
 IF csr = 1 THEN
  IF intgrabber(pt, 0, count - 1) THEN
   menu$(1) = CHR$(27) + "Browse" + XSTR$(pt) + CHR$(26)
   loadpage game + f$, pt, 2
  END IF
 END IF
 IF enter_or_space() THEN
  IF csr = 0 THEN EXIT DO
  IF csr = 2 THEN
   srcbmp$ = browse$(3, default$, "*.bmp", "")
   IF srcbmp$ <> "" THEN
    GOSUB bimport
   END IF
   loadpage game + f$, pt, 2
  END IF
  IF csr = 3 AND count < 32767 THEN
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
  IF csr = 4 THEN GOSUB disable
  IF csr = 5 THEN
   outfile$ = inputfilename$("Name of file to export to?", ".bmp", trimpath$(game) & " " & cap$ & pt)
   IF outfile$ <> "" THEN screenshot outfile$ & ".bmp", 2, master()
  END IF
 END IF
 FOR i = 0 TO 5
  col = uilook(uiMenuItem): IF i = csr THEN col = uilook(uiSelectedItem + tog)
  edgeprint menu$(i), 1, 1 + 10 * i, col, dpage
 NEXT i
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
 IF keyval(1) > 1 THEN setpal master(): RETRACE
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
  paloption = sublist(2, submenu$())
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
 IF keyval(1) > 1 THEN EXIT DO
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
 IF keyval(1) > 1 THEN tmode = 0: RETRACE
 usemenu tmode, 0, 0, 4, 24
 IF enter_or_space() THEN
  SELECT CASE tmode
   CASE 0, 1, 2
    picktiletoedit tmode, pagenum, mapfile$
   CASE 3
    GOSUB tileanim
    setkeys
    GOSUB tilemodemenu
   CASE 4
    tmode = 0
    RETRACE
  END SELECT
 END IF
 FOR i = 0 TO 4
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
menu$(1) = "Cut Tiles"
menu$(2) = "Set Default Passability"
menu$(3) = "Define Tile Animation"
menu$(4) = "Cancel"
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
 IF keyval(1) > 1 THEN savetanim pagenum, tastuf(): RETRACE
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
 IF keyval(1) > 1 OR enter_or_space() THEN RETRACE
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

FUNCTION mouseover (mouse(), zox, zoy, zcsr, area() AS MouseArea)

FOR i = 20 TO 0 STEP -1
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
 SELECT CASE context
  CASE 0 '---PICK A STATEMENT---
   IF keyval(1) > 1 THEN EXIT DO
   IF usemenu(pt, 0, 0, 9, 9) THEN GOSUB refreshmenu
   IF enter_or_space() THEN
    IF pt = 0 THEN
     EXIT DO
    ELSE
     context = 1
    END IF
   END IF
  CASE 1 '---EDIT THAT STATEMENT---
   IF keyval(1) > 1 THEN context = 0
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

SUB sprite (xw, yw, sets, perset, soff, foff, atatime, info$(), size, zoom, fileset, font())
STATIC default$, spriteclip(1600), clippedpal, clippedw, clippedh, paste
DIM nulpal(8), placer(1602), pclip(8), pmenu$(3), bmpd(40), mouse(4), area(20) AS MouseArea
DIM toolinfo(5) AS ToolInfoType
DIM workpal(8 * (atatime + 1))
DIM poffset(large(sets, atatime))
DIM AS INTEGER do_paste = 0
DIM AS INTEGER paste_transparent = 0
spritefile$ = game & ".pt" & fileset

gotm = setmouse(mouse())
GOSUB initmarea
tool = 0
airsize = 5
mist = 10
pt = 0
icsr = 0
itop = 0
dcsr = 1
x = 0: y = 0
zox = 0: zoy = 0
debug_palettes = 0
pmenu$(0) = "Overwrite Current Palette"
pmenu$(1) = "Import Without Palette"
pmenu$(2) = "Cancel Import"
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
FOR i = 0 TO 15
 poke8bit nulpal(), i, i
NEXT i
loaddefaultpals fileset, poffset(), sets
GOSUB loadalluc

setkeys
DO
 setwait 55
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN EXIT DO
 IF keyval(29) > 0 AND keyval(14) > 1 THEN
  GOSUB savealluc
  cropafter pt, sets, 0, spritefile$, size * perset, 1
  clearpage 3
  GOSUB loadalluc
 END IF
 IF enter_or_space() THEN
  GOSUB savealluc
  GOSUB spriteage
  GOSUB loadalluc
 END IF
 IF keyval(73) > 1 THEN
  GOSUB savealluc
  pt = large(pt - atatime, 0)
  top = pt
  GOSUB loadalluc
 END IF
 IF keyval(81) > 1 THEN
  GOSUB savealluc
  top = large(small(pt, sets - atatime), 0)
  pt = small(pt + atatime, sets)
  GOSUB loadalluc
 END IF
 IF keyval(71) > 1 THEN
  GOSUB savealluc
  pt = 0
  top = 0
  GOSUB loadalluc
 END IF
 IF keyval(79) > 1 THEN
  GOSUB savealluc
  pt = sets
  top = large(small(pt, sets - atatime), 0)
  GOSUB loadalluc
 END IF
 IF keyval(72) > 1 THEN
  pt = large(pt - 1, 0)
  IF pt < top THEN
   GOSUB savealluc
   top = pt
   GOSUB loadalluc
  END IF
 END IF
 IF keyval(80) > 1 AND pt < 32767 THEN
  pt = pt + 1
  IF needaddset(pt, sets, "graphics") THEN
   '--Add a new blank sprite set
   setpicstuf buffer(), size * perset, -1
   FOR i = 0 TO (size * perset) / 2
    buffer(i) = 0
   NEXT i
   storeset spritefile$, pt, 0
   '-- re-size the array that stores the default palette offset
   REDIM PRESERVE poffset(large(sets, atatime))
   '--add a new blank default palette
   poffset(pt) = 0
   GOSUB loadalluc
  END IF
  IF pt > top + atatime THEN
   GOSUB savealluc
   top = top + 1
   GOSUB loadalluc
  END IF
 END IF
 IF keyval(75) > 1 THEN num = large(num - 1, 0)
 IF keyval(77) > 1 THEN num = small(num + 1, perset - 1)
 IF keyval(26) > 1 THEN
  changepal poffset(pt), -1, workpal(), pt - top
 END IF
 IF keyval(27) > 1 THEN
  changepal poffset(pt), 1, workpal(), pt - top
 END IF
 '--copying
 IF (keyval(29) > 0 AND keyval(82) > 1) OR ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(83) > 0) OR (keyval(29) > 0 AND keyval(46) > 1) THEN 
  loadsprite spriteclip(), 0, num * size, soff * (pt - top), xw, yw, 3
  paste = 1
  clippedw = xw
  clippedh = yw
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
  loadsprite placer(), 0, num * size, soff * (pt - top), xw, yw, 3
  rectangle 0, 0, xw, yw, 0, dpage
  drawsprite placer(), 0, nulpal(), 0, 0, 0, dpage
  IF NOT paste_transparent THEN
   rectangle 0, 0, clippedw, clippedh, 0, dpage
  END IF
  drawsprite spriteclip(), 0, nulpal(), 0, 0, 0, dpage
  getsprite placer(), 0, 0, 0, xw, yw, dpage
  stosprite placer(), 0, num * size, soff * (pt - top), 3
  savewuc(spritefile$, pt, top, sets, xw,yw, soff, perset, size, placer(), workpal(), poffset())
 END IF
 IF keyval(59) > 1 THEN
  debug_palettes = debug_palettes XOR 1
 END IF
 GOSUB choose
 textcolor uilook(uiMenuItem), 0
 printstr "Palette" + XSTR$(poffset(pt)), 320 - (LEN("Palette" + XSTR$(poffset(pt))) * 8), 0, dpage
 FOR i = 0 TO 15
  rectangle 271 + i * 3, 8, 3, 8, peek8bit(workpal(), (pt - top) * 16 + i), dpage
 NEXT i
 IF debug_palettes THEN
   FOR j = 0 TO atatime
     FOR i = 0 TO 15
      rectangle 271 + i * 3, 40 + j * 5, 3, 4, peek8bit(workpal(), j * 16 + i), dpage
     NEXT i
   NEXT j
 END IF
 printstr "Set" + XSTR$(pt), 320 - (LEN("Set" + XSTR$(pt)) * 8), 16, dpage
 printstr info$(num), 320 - (LEN(info$(num)) * 8), 24, dpage
 SWAP vpage, dpage
 setvispage vpage
 clearpage dpage
 dowait
LOOP
changepal poffset(pt), 0, workpal(), pt - top
GOSUB savealluc
savedefaultpals fileset, poffset(), sets
clearpage 0
clearpage 1
clearpage 2
clearpage 3
EXIT SUB

choose:
rectangle 0, 0, 320, 200, uilook(uiDisabledItem), dpage
rectangle 4 + (num * (xw + 1)), (pt - top) * (yw + 5), xw + 2, yw + 2, uilook(uiText), dpage
FOR i = top TO small(top + atatime, sets)
 picslot = i - top
 FOR o = 0 TO perset - 1
  rectangle 5 + (o * (xw + 1)), 1 + (picslot * (yw + 5)), xw, yw, 0, dpage
  loadsprite placer(), 0, size * o, soff * picslot, xw, yw, 3
  drawsprite placer(), 0, workpal(), (i - top) * 16, 5 + (o * (xw + 1)), 1 + (picslot * (yw + 5)), dpage
 NEXT o
NEXT i
RETRACE

resettool:
box = 0
drl = 0
ovalstep = 0
RETRACE

spriteage:
undodepth = 0
undoptr = 0
undomax = (32000 \ size) - 1
GOSUB spedbak
loadsprite placer(), 0, num * size, soff * (pt - top), xw, yw, 3
setkeys
DO
 setwait 90
 setkeys
 IF gotm THEN
  readmouse mouse()
  zcsr = 0
  zone = mouseover(mouse(), zox, zoy, zcsr, area())
 END IF
 IF keyval(1) > 1 THEN
  IF box OR drl OR ovalstep THEN
   GOSUB resettool
  ELSE
   stosprite placer(), 0, num * size, soff * (pt - top), 3
   GOSUB resettool
   EXIT DO
  END IF
 END IF
 GOSUB sprctrl
 tog = tog XOR 1
 copypage 2, dpage  'moved this here to cover up residue on dpage (which was there before I got here!)
 GOSUB spritescreen
 SWAP vpage, dpage
 setvispage vpage
 'blank the sprite area
 rectangle 239, 119, xw, yw, 0, dpage
 dowait
LOOP
j = pt
savewuc(spritefile$, j, top, sets, xw,yw, soff, perset, size,placer(), workpal(), poffset())
changepal poffset(pt), 0, workpal(), pt - top
RETRACE

sprctrl:
IF mouse(2) = 0 THEN
 oldx = -1
 oldy = -1
END IF
IF keyval(4) > 1 THEN setvispage 3: w = getkey
IF keyval(41) > 1 THEN hideptr = hideptr XOR 1
IF keyval(51) > 1 AND col > 0 THEN col = col - 1
IF keyval(52) > 1 AND col < 15 THEN col = col + 1
IF zone = 2 THEN
 IF mouse(3) > 0 THEN col = small(INT(zox / 4), 15)
END IF
IF keyval(26) > 1 OR (zone = 5 AND mouse(3) > 0) THEN
 changepal poffset(pt), -1, workpal(), pt - top
END IF
IF keyval(27) > 1 OR (zone = 6 AND mouse(3) > 0) THEN
 changepal poffset(pt), 1, workpal(), pt - top
END IF
IF keyval(25) > 1 OR (zone = 19 AND mouse(3) > 0) THEN '--call palette browser
 '--write changes so far
 stosprite placer(), 0, num * size, soff * (pt - top), 3
 '--save current palette
 storepal16 workpal(), pt - top, poffset(pt)
 poffset(pt) = pal16browse(poffset(pt), fileset, pt, perset, xw, yw)
 getpal16 workpal(), pt - top, poffset(pt)
END IF
'--UNDO
IF (keyval(29) > 0 AND keyval(44) > 1) OR (zone = 20 AND mouse(3) > 0) THEN GOSUB readundospr
'--COPY (CTRL+INS,SHIFT+DEL,CTRL+C)
IF (keyval(29) > 0 AND keyval(82) > 1) OR ((keyval(42) > 0 OR keyval(54) > 0) AND keyval(83) > 0) OR (keyval(29) > 0 AND keyval(46) > 1) THEN
 clippedw = xw
 clippedh = yw
 stosprite placer(), 0, num * size, soff * (pt - top), 3
 loadsprite spriteclip(), 0, num * size, soff * (pt - top), xw, yw, 3
 paste = 1
END IF
'--PASTE (SHIFT+INS,CTRL+V)
IF (((keyval(42) > 0 OR keyval(54) > 0) AND keyval(82) > 1) OR (keyval(29) > 0 AND keyval(47) > 1)) AND paste = 1 THEN
 rectangle 0, 0, xw, yw, 0, dpage
 drawsprite placer(), 0, nulpal(), 0, 0, 0, dpage
 rectangle x, y, clippedw, clippedh, 0, dpage
 drawsprite spriteclip(), 0, nulpal(), 0, x, y, dpage
 getsprite placer(), 0, 0, 0, xw, yw, dpage
END IF
'--TRANSPARENT PASTE (CTRL+T)
IF (keyval(29) > 0 AND keyval(20) > 1) AND paste = 1 THEN
 rectangle 0, 0, xw, yw, 0, dpage
 drawsprite placer(), 0, nulpal(), 0, 0, 0, dpage
 drawsprite spriteclip(), 0, nulpal(), 0, x, y, dpage
 getsprite placer(), 0, 0, 0, xw, yw, dpage
END IF
'--COPY PALETTE (ALT+C)
IF keyval(56) > 0 AND keyval(46) > 1 THEN
 FOR i = 0 TO 7
  pclip(i) = workpal(i + (pt - top) * 8)
 NEXT
 clippedpal = 1
END IF
'--PASTE PALETTE (ALT+V)
IF keyval(56) > 0 AND keyval(47) > 1 THEN
 IF clippedpal THEN
  FOR i = 0 TO 8
   workpal(i + (pt - top) * 8) = pclip(i)
  NEXT
 END IF
END IF
curcol = peek8bit(workpal(), (pt - top) * 16 + col)
IF keyval(56) > 0 THEN
 IF keyval(72) > 0 AND curcol > 15 THEN curcol -= 16
 IF keyval(80) > 0 AND curcol < 240 THEN curcol += 16
 IF keyval(75) > 0 AND curcol > 0 THEN curcol -= 1
 IF keyval(77) > 0 AND curcol < 255 THEN curcol += 1
END IF
IF mouse(3) = 1 AND zone = 3 THEN 'AND col > 0 THEN
 curcol = INT(INT(zoy / 6) * 16) + INT(zox / 4)
END IF
poke8bit workpal(), (pt - top) * 16 + col, curcol
IF keyval(56) = 0 THEN
 fixmouse = 0
 IF keyval(72) AND 5 THEN y = large(0, y - 1): fixmouse = 1
 IF keyval(80) AND 5 THEN y = small(yw - 1, y + 1): fixmouse = 1
 IF keyval(75) AND 5 THEN x = large(0, x - 1): fixmouse = 1
 IF keyval(77) AND 5 THEN x = small(xw - 1, x + 1): fixmouse = 1
 IF fixmouse THEN
  IF zone = 1 THEN
   zox = x * zoom + INT(zoom / 2)
   zoy = y * zoom + INT(zoom / 2)
   mouse(0) = area(0).x + zox 
   mouse(1) = area(0).y + zoy
   movemouse mouse(0), mouse(1)
  END IF 
  IF zone = 14 THEN
   zox = x
   zoy = y
   mouse(0) = area(13).y + zox 
   mouse(1) = area(13).y + zoy
   movemouse mouse(0), mouse(1)
  END IF
 END IF
END IF
IF zone = 1 THEN
 x = INT(zox / zoom)
 y = INT(zoy / zoom)
END IF
IF tool = 5 THEN '--adjust airbrush
 IF mouse(3) = 1 OR mouse(2) = 1 THEN
  IF zone = 15 THEN airsize = large(airsize - 1, 1)
  IF zone = 17 THEN airsize = small(airsize + 1, 80)
  IF zone = 16 THEN mist = large(mist - 1, 1)
  IF zone = 18 THEN mist = small(mist + 1, 99)
 END IF
 IF keyval(12) > 1 OR keyval(74) > 1 THEN
  IF keyval(29) > 0 THEN
   mist = large(mist - 1, 1)
  ELSE
   airsize = large(airsize - 1, 1)
  END IF
 END IF
 IF keyval(13) > 1 OR keyval(78) > 1 THEN
  IF keyval(29) > 0 THEN
   mist = small(mist + 1, 99)
  ELSE
   airsize = small(airsize + 1, 80)
  END IF
 END IF
END IF
IF zone = 14 THEN
 x = zox
 y = zoy
END IF
IF ((zone = 1 OR zone = 14) AND (mouse(3) = 1 OR mouse(2) = 1)) OR keyval(57) > 0 THEN
 SELECT CASE tool
  CASE 0'---Draw
   GOSUB putdot
  CASE 1'---Box
   IF mouse(3) > 0 OR keyval(57) > 1 THEN
    IF box THEN
     box = 0: GOSUB drawsquare
    ELSE
     box = 1: bx = x: by = y
    END IF
   END IF
  CASE 2'---Line
   IF mouse(3) > 0 OR keyval(57) > 1 THEN
    IF drl THEN
     drl = 0: GOSUB straitline
    ELSE
     drl = 1: bx = x: by = y
    END IF
   END IF
  CASE 3'---Fill
   IF mouse(3) > 0 OR keyval(57) > 1 THEN
    GOSUB floodfill
   END IF
  CASE 4'---Oval
   IF mouse(3) > 0 OR keyval(57) > 1 THEN
    SELECT CASE ovalstep
     CASE 0'--start oval
      bx = x: by = y
      squishx = 0: squishy = 0
      radius = 0
      ovalstep = 1
     CASE 1'--draw the oval
      GOSUB drawoval
      ovalstep = 0
    END SELECT
   END IF
  CASE 5'---Spray
   GOSUB sprayspot
 END SELECT
END IF
IF ovalstep = 1 THEN
 radius = large(ABS(x - bx), ABS(y - by))
END IF
FOR i = 0 TO 5
 IF (mouse(3) > 0 AND zone = 7 + i) OR keyval(toolinfo(i).shortcut) > 1 THEN
  tool = i
  GOSUB resettool
  dcsr = toolinfo(i).cursor + 1
 END IF
NEXT i
IF keyval(28) > 1 OR (zone = 1 AND mouse(2) = 2) THEN
 drawsprite placer(), 0, nulpal(), 0, 239, 119, dpage: col = readpixel(239 + x, 119 + y, dpage)
END IF
IF keyval(14) > 1 OR (zone = 4 AND mouse(3) > 0) THEN wardsprite placer(), 0, nulpal(), 0, 239, 119, dpage: getsprite placer(), 0, 239, 119, xw, yw, dpage
IF keyval(58) > 0 THEN
 IF keyval(72) > 0 THEN rectangle 239, 119, xw, yw, 0, dpage: drawsprite placer(), 0, nulpal(), 0, 239, 118, dpage: getsprite placer(), 0, 239, 119, xw, yw, dpage
 IF keyval(80) > 0 THEN rectangle 239, 119, xw, yw, 0, dpage: drawsprite placer(), 0, nulpal(), 0, 239, 120, dpage: getsprite placer(), 0, 239, 119, xw, yw, dpage
 IF keyval(75) > 0 THEN rectangle 239, 119, xw, yw, 0, dpage: drawsprite placer(), 0, nulpal(), 0, 238, 119, dpage: getsprite placer(), 0, 239, 119, xw, yw, dpage
 IF keyval(77) > 0 THEN rectangle 239, 119, xw, yw, 0, dpage: drawsprite placer(), 0, nulpal(), 0, 240, 119, dpage: getsprite placer(), 0, 239, 119, xw, yw, dpage
END IF
IF keyval(23) > 1 OR (zone = 13 AND mouse(3) > 0) THEN GOSUB import16
RETRACE

spedbak:
clearpage 2
rectangle 3, 0, xw * zoom + 2, yw * zoom + 2, uilook(uiText), 2
rectangle 4, 1, xw * zoom, yw * zoom, 0, 2
rectangle 245, 109, 67, 8, uilook(uiText), 2
rectangle 246, 110, 65, 6, 0, 2
rectangle 238, 118, xw + 2, yw + 2, uilook(uiText), 2
rectangle 239, 119, xw, yw, 0, 2
area(0).w = xw * zoom
area(0).h = yw * zoom
area(13).w = xw
area(13).h = yw
RETRACE

import16:
srcbmp$ = browse$(2, default$, "*.bmp", "")
IF srcbmp$ = "" THEN RETRACE
'--------------------
'DECIDE ABOUT PALETTE
pcsr = 0
setkeys
DO
 setwait 110
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN RETRACE
 IF keyval(75) > 1 OR keyval(26) > 1 THEN
  changepal poffset(pt), -1, workpal(), pt - top
 END IF
 IF keyval(77) > 1 OR keyval(27) > 1 THEN
  changepal poffset(pt), 1, workpal(), pt - top
 END IF
 usemenu pcsr, 0, 0, 2, 24
 IF enter_or_space() THEN
  IF pcsr = 2 THEN RETRACE
  EXIT DO
 END IF
 GOSUB spritescreen
 rectangle 4, 156, 208, 32, uilook(uiDisabledItem), dpage
 FOR i = 0 TO 2
  c = uilook(uiMenuItem): IF i = pcsr THEN c = uilook(uiSelectedItem + tog)
  edgeprint pmenu$(i), 8, 160 + (i * 8), c, dpage
 NEXT i
 SWAP vpage, dpage
 setvispage vpage
 copypage 2, dpage
 dowait
LOOP

clearpage dpage
clearpage 2

loadbmp srcbmp$, 1, 1, 2

'---------------------
'PICK BACKGROUND COLOR
gx = 1
gy = 1
temp = bmpinfo(srcbmp$, bmpd())
edjx = small(320, bmpd(1))
edjy = small(200, bmpd(2))
setkeys
DO
 setwait 110
 setkeys
 tog = tog XOR 1
 IF keyval(1) > 1 THEN 
 '--Cancel
  GOSUB spedbak
  RETRACE
 END IF
 IF keyval(56) THEN movespeed = 9 ELSE movespeed = 1
 IF keyval(72) > 0 THEN gy = large(gy - movespeed, 1)
 IF keyval(80) > 0 THEN gy = small(gy + movespeed, edjy)
 IF keyval(75) > 0 THEN gx = large(gx - movespeed, 1)
 IF keyval(77) > 0 THEN gx = small(gx + movespeed, edjx)
 IF enter_or_space() THEN EXIT DO
 putpixel gx, gy, 15 + tog, dpage
 edgeprint "Pick Background Color", 0, 190, 7, dpage
 SWAP vpage, dpage
 setvispage vpage
 copypage 2, dpage
 dowait
LOOP

'--picked a transparent pixel
temp = readpixel(gx, gy, 2)
'--swap the transparent pixels to 0
FOR i = 1 TO edjx
 FOR o = 1 TO edjy
  IF readpixel(i, o, 2) = temp THEN
   putpixel i, o, 0, 2
  ELSE
   IF readpixel(i, o, 2) = 0 THEN
    putpixel i, o, temp, 2
   END IF
  END IF
 NEXT o
NEXT i
'--swap the transparent palette entry to 0
IF pcsr = 0 THEN
 convertbmppal srcbmp$, master(), workpal(), 8 * (pt - top)
 'swap black with the transparent color
 poke8bit workpal(), temp + (pt - top) * 16, peek8bit(workpal(), 0 + (pt - top) * 16)
 poke8bit workpal(), 0 + (pt - top) * 16, 0
END IF
'--read the sprite
getsprite placer(), 0, 1, 1, xw, yw, 2
GOSUB spedbak
RETRACE

floodfill:
GOSUB writeundospr
rectangle 238, 118, xw + 2, yw + 2, uilook(uiHighlight), dpage
rectangle 239, 119, xw, yw, 0, dpage
drawsprite placer(), 0, nulpal(), 0, 239, 119, dpage
paintat 239 + x, 119 + y, col, dpage, buffer(), 16384
getsprite placer(), 0, 239, 119, xw, yw, dpage
RETRACE

sprayspot:
IF oldx = -1 AND oldy = -1 THEN GOSUB writeundospr
drawsprite placer(), 0, nulpal(), 0, 239, 119, dpage
airbrush 239 + x, 119 + y, airsize, mist, col, dpage
getsprite placer(), 0, 239, 119, xw, yw, dpage
oldx = x
oldy = y
RETRACE

writeundospr:
stosprite placer(), 0, undoptr * size, 100, 3
undoptr = loopvar(undoptr, 0, undomax, 1)
undodepth = small(undodepth + 1, undomax + 1)
RETRACE

readundospr:
IF undodepth > 0 THEN
 undodepth = undodepth - 1
 undoptr = loopvar(undoptr, 0, undomax, -1)
 loadsprite placer(), 0, undoptr * size, 100, xw, yw, 3
END IF
RETRACE

putdot:
drawsprite placer(), 0, nulpal(), 0, 239, 119, dpage
IF oldx = -1 AND oldy = -1 THEN
 GOSUB writeundospr
 putpixel 239 + x, 119 + y, col, dpage
ELSE
 drawline 239 + x, 119 + y, 239 + oldx, 119 + oldy, col, dpage
END IF
getsprite placer(), 0, 239, 119, xw, yw, dpage
oldx = x
oldy = y
RETRACE

drawoval:
GOSUB writeundospr
drawsprite placer(), 0, nulpal(), 0, 239, 119, dpage
ellipse 239 + bx, 119 + by, radius, col, dpage, squishx, squishy
getsprite placer(), 0, 239, 119, xw, yw, dpage
RETRACE

drawsquare:
GOSUB writeundospr
drawsprite placer(), 0, nulpal(), 0, 239, 119, dpage
rectangle 239 + small(x, bx), 119 + small(y, by), ABS(x - bx) + 1, ABS(y - by) + 1, col, dpage
getsprite placer(), 0, 239, 119, xw, yw, dpage
RETRACE

straitline:
GOSUB writeundospr
drawsprite placer(), 0, nulpal(), 0, 239, 119, dpage
drawline 239 + x, 119 + y, 239 + bx, 119 + by, col, dpage
getsprite placer(), 0, 239, 119, xw, yw, dpage
RETRACE

spritescreen:
curcol = peek8bit(workpal(), col + (pt - top) * 16)
rectangle 247 + ((curcol - (INT(curcol / 16) * 16)) * 4), 0 + (INT(curcol / 16) * 6), 5, 7, uilook(uiText), dpage
FOR i = 0 TO 15
 FOR o = 0 TO 15
  rectangle 248 + (i * 4), 1 + (o * 6), 3, 5, o * 16 + i, dpage
 NEXT o
NEXT i
textcolor uilook(uiText), uilook(uiDisabledItem): IF zone = 5 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
printstr CHR$(27), 248, 100, dpage
textcolor uilook(uiText), uilook(uiDisabledItem): IF zone = 6 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
printstr CHR$(26), 304, 100, dpage
textcolor uilook(uiText), 0
printstr LEFT$(" Pal", 4 - (LEN(XSTR$(poffset(pt))) - 3)) + XSTR$(poffset(pt)), 248, 100, dpage
rectangle 247 + (col * 4), 110, 5, 7, uilook(uiText), dpage
FOR i = 0 TO 15
 rectangle 248 + (i * 4), 111, 3, 5, peek8bit(workpal(), i + (pt - top) * 16), dpage
NEXT
IF zoom = 4 THEN hugesprite placer(), workpal(), (pt - top) * 16, 4, 1, dpage, 0
IF zoom = 2 THEN bigsprite placer(), workpal(), (pt - top) * 16, 4, 1, dpage, 0
curcol = peek8bit(workpal(), col + (pt - top) * 16)
IF box = 1 THEN
 rectangle 4 + small(x, bx) * zoom, 1 + small(y, by) * zoom, (ABS(x - bx) + 1) * zoom, (ABS(y - by) + 1) * zoom, curcol, dpage
 rectangle 4 + bx * zoom, 1 + by * zoom, zoom, zoom, IIF(tog, uilook(uiBackground), uilook(uiText)), dpage
END IF
rectangle 4 + (x * zoom), 1 + (y * zoom), zoom, zoom, IIF(tog, uilook(uiBackground), uilook(uiText)), dpage
drawsprite placer(), 0, workpal(), (pt - top) * 16, 239, 119, dpage, 0
IF box = 1 THEN
 rectangle 239 + small(x, bx), 119 + small(y, by), ABS(x - bx) + 1, ABS(y - by) + 1, curcol, dpage
 putpixel 239 + bx, 119 + by, tog * 15, dpage
END IF
IF drl = 1 THEN
 drawline 239 + x, 119 + y, 239 + bx, 119 + by, curcol, dpage
 drawline 5 + (x * zoom), 2 + (y * zoom), 5 + (bx * zoom), 2 + (by * zoom), curcol, dpage
END IF
IF ovalstep > 0 THEN
 ellipse 239 + bx, 119 + by, radius, curcol, dpage, squishx, squishy
 ellipse 5 + (bx * zoom), 2 + (by * zoom), radius * zoom, curcol, dpage, squishx, squishy
END IF
IF tool = 5 THEN
 ellipse 239 + x, 119 + y, airsize / 2, curcol, dpage, 0, 0
 ellipse 5 + (x * zoom), 2 + (y * zoom), (airsize / 2) * zoom, curcol, dpage, 0, 0
END IF
putpixel 239 + x, 119 + y, tog * 15, dpage
textcolor uilook(uiMenuItem), 0
printstr info$(num), 0, 182, dpage
printstr "Tool:" & toolinfo(tool).name, 0, 190, dpage
FOR i = 0 TO 5
 t1 = uilook(uiMenuItem): t2 = uilook(uiDisabledItem)
 IF tool = i THEN t1 = uilook(uiText): t2 = uilook(uiMenuItem)
 IF zone - 7 = i THEN t2 = uilook(uiSelectedDisabled)
 textcolor t1, t2
 printstr toolinfo(i).icon, area(toolinfo(i).areanum).x, area(toolinfo(i).areanum).y, dpage
NEXT i
textcolor uilook(uiMenuItem), uilook(uiDisabledItem): IF zone = 4 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
printstr CHR$(7), 182, 190, dpage
textcolor uilook(uiMenuitem), uilook(uiDisabledItem): IF zone = 13 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
printstr "I", 194, 190, dpage
IF undodepth = 0 THEN
 textcolor uilook(uiBackground), uilook(uiDisabledItem)
ELSE
 textcolor uilook(uiMenuItem), uilook(uiDisabledItem)
END IF
IF zone = 20 AND undodepth > 0 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
printstr "UNDO", 170, 182, dpage
IF tool = 5 THEN
 textcolor uilook(uiMenuItem), 0
 printstr "SIZE" + STR$(airsize), 218, 182, dpage
 printstr "MIST" + STR$(mist), 218, 190, dpage
 textcolor uilook(uiMenuItem), uilook(uiDisabledItem): IF zone = 15 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
 printstr CHR$(27), 210, 182, dpage
 textcolor uilook(uiMenuItem), uilook(uiDisabledItem): IF zone = 16 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
 printstr CHR$(27), 210, 190, dpage
 textcolor uilook(uiMenuItem), uilook(uiDisabledItem): IF zone = 17 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
 printstr CHR$(26), 266, 182, dpage
 textcolor uilook(uiMenuItem), uilook(uiDisabledItem): IF zone = 18 THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
 printstr CHR$(26), 266, 190, dpage
END IF
IF gotm THEN
 c = zcsr
 IF c = -1 THEN
  IF hideptr THEN c = -2 ELSE c = dcsr
 END IF
 IF tog THEN
  textcolor uilook(uiText), 0
 ELSE
  textcolor uilook(uiDescription), 0
 END IF
 printstr CHR$(2 + c), small(large(mouse(0) - 2, 0), 311), small(large(mouse(1) - 2, 0), 191), dpage
END IF
RETRACE

initmarea:
'0 x
'1 y
'2 width
'3 height
'4 cursor
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
'TOOL BUTTONS
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
area(13).x = 239
area(13).y = 119
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
RETRACE

savealluc:
FOR j = top TO top + atatime
	savewuc(spritefile$, j, top, sets, xw,yw, soff, perset, size,placer(), workpal(), poffset()) 
NEXT j
RETRACE

loadalluc:
FOR j = top TO top + atatime
 loadwuc(spritefile$, j, top, sets, xw,yw, soff, perset, size,placer(), workpal(), poffset())
NEXT
RETRACE

END SUB

SUB testanimpattern (tastuf(), taset)

DIM sample(7)
DIM tanim_state(1) AS TileAnimState

clearpage vpage
clearpage dpage

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
 
 IF keyval(1) > 1 THEN EXIT DO
 IF keyval(72) > 1 THEN csr = loopvar(csr, 0, 47, -16): GOSUB setupsample
 IF keyval(80) > 1 THEN csr = loopvar(csr, 0, 47, 16): GOSUB setupsample
 IF keyval(75) > 1 THEN csr = loopvar(csr, 0, 47, -1): GOSUB setupsample
 IF keyval(77) > 1 THEN csr = loopvar(csr, 0, 47, 1): GOSUB setupsample
 SWAP vpage, dpage
 setvispage vpage
 '--draw available animating tiles--
 setmapdata buffer(), buffer(), 10, 130
 drawmap 0, -10, 0, 0, dpage
 '--draw sample--
 setmapdata sample(), sample(), 100, 40
 setanim tastuf(0) + tanim_state(0).cycle, tastuf(20) + tanim_state(1).cycle
 cycletile tanim_state(), tastuf()
 drawmap -130, -100, 0, 0, dpage
 '--Draw cursor--
 y = INT(csr / 16)
 x = csr - y * 16
 rectangle 20 * x, 10 + 20 * y, 20, 1, uilook(uiSelectedItem + tog), dpage
 rectangle 20 * x, 10 + 20 * y, 1, 20, uilook(uiSelectedItem + tog), dpage
 rectangle 20 * x, 29 + 20 * y, 20, 1, uilook(uiSelectedItem + tog), dpage
 rectangle 20 * x + 19, 10 + 20 * y, 1, 20, uilook(uiSelectedItem + tog), dpage
 
 dowait
LOOP
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
DIM ts AS TileEditState, mover(12), mouse(4), area(20) AS MouseArea
DIM toolinfo(5) AS ToolInfoType
ts.gotmouse = setmouse(mouse())
ts.canpaste = oldpaste
ts.drawcursor = 1
ts.airsize = 5
ts.mist = 10
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
area(0).x = 60
area(0).y = 0
area(0).w = 200
area(0).h = 160
area(0).hidecursor = YES
area(1).x = 0
area(1).y = 160
area(1).w = 320
area(1).h = 32
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
DIM pastogkey(7), defaults(160), bitmenu$(10)
IF tmode = 2 THEN
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
 setwait 110
 setkeys
 copypage 3, dpage
 IF ts.gotmouse THEN
  readmouse mouse()
 END IF
 IF keyval(1) > 1 THEN storepage mapfile$, pagenum, 3: EXIT DO
 IF tmode <> 2 OR keyval(29) = 0 THEN
  IF keyval(75) AND 5 THEN IF bnum > 0 THEN bnum = bnum - 1: IF ts.gotmouse THEN mouse(0) = mouse(0) - 20: movemouse mouse(0), mouse(1)
  IF keyval(77) AND 5 THEN IF bnum < 159 THEN bnum = bnum + 1: IF ts.gotmouse THEN mouse(0) = mouse(0) + 20: movemouse mouse(0), mouse(1)
  IF keyval(72) AND 5 THEN IF bnum > 15 THEN bnum = bnum - 16: IF ts.gotmouse THEN mouse(1) = mouse(1) - 20: movemouse mouse(0), mouse(1)
  IF keyval(80) AND 5 THEN IF bnum < 144 THEN bnum = bnum + 16: IF ts.gotmouse THEN mouse(1) = mouse(1) + 20: movemouse mouse(0), mouse(1)
 END IF
 IF ts.gotmouse THEN
  bnum = INT(mouse(1) / 20) * 16 + INT(mouse(0) / 20)
 END IF
 IF tmode = 2 THEN
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
   editmaptile ts, mover(), mouse(), area(), toolinfo()
  END IF
  IF tmode = 1 THEN
   tilecut ts, mouse(), area()
  END IF 
  IF tmode = 2 THEN
   editbitset defaults(), bnum, 7, bitmenu$()
  END IF
 END IF
 tog = tog XOR 1
 IF tmode = 2 THEN
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
IF tmode = 2 THEN
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

SUB editmaptile (ts AS TileEditState, mover(), mouse(), area() AS MouseArea, toolinfo() AS ToolInfoType)
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
setkeys
DO
 setwait 90
 setkeys
 IF ts.gotmouse THEN
  readmouse mouse()
  zcsr = 0
  ts.zone = mouseover(mouse(), zox, zoy, zcsr, area())
 END IF
 tog = tog XOR 1
 ts.delay = large(ts.delay - 1, 0)
 ts.justpainted = large(ts.justpainted - 1, 0)
 IF keyval(1) > 1 THEN
  IF ts.hold THEN
   ts.hold = 0
  ELSE
   EXIT DO
  END IF
 END IF
 IF keyval(56) = 0 THEN
  fixmouse = 0
  IF keyval(75) AND 5 THEN ts.x = large(ts.x - 1, 0): fixmouse = 1
  IF keyval(77) AND 5 THEN ts.x = small(ts.x + 1, 19): fixmouse = 1
  IF keyval(72) AND 5 THEN ts.y = large(ts.y - 1, 0): fixmouse = 1
  IF keyval(80) AND 5 THEN ts.y = small(ts.y + 1, 19): fixmouse = 1
  IF fixmouse THEN
   zox = ts.x * 10 + 5
   zoy = ts.y * 8 + 4
   mouse(0) = area(0).x + zox
   mouse(1) = area(0).y + zoy
   movemouse mouse(0), mouse(1)
  END IF
 END IF 
 '---KEYBOARD SHORTCUTS FOR TOOLS------------
 FOR i = 0 TO 5
  IF keyval(toolinfo(i).shortcut) > 1 THEN ts.tool = i: ts.hold = 0: ts.drawcursor = toolinfo(i).cursor + 1
 NEXT i
 '----------
 IF keyval(51) > 1 OR (keyval(56) > 0 AND keyval(75) > 0) THEN
  ts.curcolor = (ts.curcolor + 255) MOD 256
  IF ts.curcolor MOD 16 = 15 THEN ts.curcolor = (ts.curcolor + 144) MOD 256
 END IF
 IF keyval(52) > 1 OR (keyval(56) > 0 AND keyval(77) > 0) THEN
  ts.curcolor += 1
  IF ts.curcolor MOD 16 = 0 THEN ts.curcolor = (ts.curcolor + 112) MOD 256
 END IF
 IF keyval(56) > 0 AND keyval(72) > 0 THEN ts.curcolor = (ts.curcolor + 240) MOD 256
 IF keyval(56) > 0 AND keyval(80) > 0 THEN ts.curcolor = (ts.curcolor + 16) MOD 256
 IF keyval(41) > 1 THEN ts.hidemouse = ts.hidemouse XOR 1
 IF keyval(29) > 0 AND keyval(44) > 1 AND ts.allowundo THEN
  ts.undo = loopvar(ts.undo, 0, 5, -1)
  readundoblock mover(), ts
 END IF
 IF keyval(57) > 0 THEN clicktile mover(), ts, mouse(3)
 IF keyval(28) > 1 THEN ts.curcolor = readpixel(ts.tilex * 20 + ts.x, ts.tiley * 20 + ts.y, 3)
 IF keyval(58) > 0 THEN scrolltile mover(), ts
 IF ts.gotmouse THEN
  SELECT CASE ts.zone   
  CASE 1
   ts.x = INT(zox / 10)
   ts.y = INT(zoy / 8)
   IF mouse(2) = 2 THEN ts.curcolor = readpixel(ts.tilex * 20 + ts.x, ts.tiley * 20 + ts.y, 3)
   IF mouse(2) = 1 THEN clicktile mover(), ts, mouse(3)
  CASE 2
   IF mouse(2) > 0 AND mouse(3) = 1 THEN
    ts.curcolor = ((zoy \ 4) * 16) + ((zox MOD 160) \ 10) + (zox \ 160) * 128
   END IF
  CASE 3 TO 8
   IF mouse(3) = 1 THEN
    ts.tool = ts.zone - 3
    ts.drawcursor = toolinfo(ts.tool).cursor + 1
    ts.hold = 0
   END IF
  CASE 13 TO 16
   IF mouse(3) = 1 THEN fliptile mover(), ts
  END SELECT 
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
 END IF
 IF ts.tool = 5 THEN '--adjust airbrush
  IF mouse(3) = 1 OR mouse(2) = 1 THEN
   IF ts.zone = 17 THEN ts.airsize = large(ts.airsize - 1, 1)
   IF ts.zone = 19 THEN ts.airsize = small(ts.airsize + 1, 30)
   IF ts.zone = 18 THEN ts.mist = large(ts.mist - 1, 1)
   IF ts.zone = 20 THEN ts.mist = small(ts.mist + 1, 99)
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
 IF keyval(14) > 1 OR keyval(26) > 1 OR keyval(27) > 1 THEN fliptile mover(), ts
 cy = (ts.curcolor \ 16) MOD 8
 cx = (ts.curcolor AND 15) + (ts.curcolor \ 128) * 16
 rectangle cx * 10 + 4, cy * 4 + 162, 3, 1, IIF(tog, uilook(uiBackground), uilook(uiText)), dpage
 rectangle 60 + ts.x * 10, ts.y * 8, 10, 8, readpixel(ts.tilex * 20 + ts.x, ts.tiley * 20 + ts.y, 3), dpage
 rectangle ts.x * 10 + 64, ts.y * 8 + 3, 3, 2, IIF(tog, uilook(uiBackground), uilook(uiText)), dpage
 IF ts.tool = 5 THEN
  ellipse 64 + ts.x * 10, 3 + ts.y * 8, (ts.airsize * 9) / 2, ts.curcolor, dpage, 0, 0
 END IF
 SELECT CASE ts.hold
  CASE 1
   rectangle 60 + small(ts.x, ts.hox) * 10, small(ts.y, ts.hoy) * 8, (ABS(ts.x - ts.hox) + 1) * 10, (ABS(ts.y - ts.hoy) + 1) * 8, ts.curcolor, dpage
  CASE 2
   drawline 65 + ts.x * 10, 4 + ts.y * 8, 65 + ts.hox * 10, 4 + ts.hoy * 8, ts.curcolor, dpage
  CASE 3
   radius = large(ABS(ts.hox - ts.x), ABS(ts.hoy - ts.y)) * 9
   ellipse 65 + ts.hox * 10, 4 + ts.hoy * 8, radius, ts.curcolor, dpage, 0, 0
 END SELECT
 textcolor uilook(uiText), uilook(uiHighlight)
 printstr toolinfo(ts.tool).name, 8, 8, dpage
 printstr "Tool", 8, 16, dpage
 printstr "Undo", 274, 1, dpage
 FOR i = 0 TO 5
  t1 = uilook(uiMenuItem): t2 = uilook(uiDisabledItem)
  IF ts.tool = i THEN t1 = uilook(uiText): t2 = uilook(uiMenuItem)
  IF ts.zone - 3 = i THEN t2 = uilook(uiSelectedDisabled)
  textcolor t1, t2
  printstr toolinfo(i).icon, area(toolinfo(i).areanum).x, area(toolinfo(i).areanum).y, dpage
 NEXT i
 FOR i = 0 TO 3
  textcolor uilook(uiMenuItem), uilook(uiDisabledItem): IF ts.zone = 13 + i THEN textcolor uilook(uiText), uilook(uiSelectedDisabled)
  printstr CHR$(7 + i), 4 + i * 9, 42, dpage
 NEXT i
 IF ts.tool = 5 THEN
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
 dowait
LOOP
IF ts.gotmouse THEN
 movemouse ts.tilex * 20 + 10, ts.tiley * 20 + 10
END IF
END SUB

SUB clicktile (mover(), ts AS TileEditState, mouseclick)
IF ts.delay > 0 THEN EXIT SUB
SELECT CASE ts.tool
 CASE 0'---DRAW
  IF ts.justpainted = 0 THEN writeundoblock mover(), ts
  ts.justpainted = 3
  putpixel 280 + ts.x, 10 + (ts.undo * 21) + ts.y, ts.curcolor, 2
  rectangle ts.tilex * 20 + ts.x, ts.tiley * 20 + ts.y, 1, 1, ts.curcolor, 3
  rectangle 60 + ts.x * 10, ts.y * 8, 10, 8, ts.curcolor, 2
 CASE 1'---BOX
  IF mouseclick > 0 OR keyval(57) > 1 THEN
   IF ts.hold = 1 THEN
    writeundoblock mover(), ts
    rectangle small(ts.tilex * 20 + ts.x, ts.tilex * 20 + ts.hox), small(ts.tiley * 20 + ts.y, ts.tiley * 20 + ts.hoy), ABS(ts.x - ts.hox) + 1, ABS(ts.y - ts.hoy) + 1, ts.curcolor, 3
    refreshtileedit mover(), ts
    ts.hold = 0
   ELSE
    ts.hold = 1
    ts.hox = ts.x
    ts.hoy = ts.y
   END IF
  END IF
 CASE 2'---LINE
  IF mouseclick > 0 OR keyval(57) > 1 THEN
   IF ts.hold = 2 THEN
    writeundoblock mover(), ts
    drawline ts.tilex * 20 + ts.x, ts.tiley * 20 + ts.y, ts.tilex * 20 + ts.hox, ts.tiley * 20 + ts.hoy, ts.curcolor, 3
    refreshtileedit mover(), ts
    ts.hold = 0
   ELSE
    ts.hold = 2
    ts.hox = ts.x
    ts.hoy = ts.y
   END IF
  END IF
 CASE 3'---FILL
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
 CASE 4'---OVAL
  IF mouseclick > 0 OR keyval(57) > 1 THEN
   IF ts.hold = 3 THEN
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
    ts.hold = 0
   ELSE
    ts.hold = 3
    ts.hox = ts.x
    ts.hoy = ts.y
   END IF
  END IF
 CASE 5'---AIR
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
END SELECT
END SUB

SUB scrolltile (mover(), ts AS TileEditState)
rectangle 0, 0, 20, 20, uilook(uiBackground), dpage
shiftx = 0: shifty = 0
IF keyval(72) > 0 THEN shifty = -1
IF keyval(80) > 0 THEN shifty = 1
IF keyval(75) > 0 THEN shiftx = -1
IF keyval(77) > 0 THEN shiftx = 1
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
IF (ts.zone = 13 OR ts.zone = 16) OR keyval(26) > 1 OR (keyval(14) > 1 AND keyval(29) = 0) THEN flipx = 19
IF ts.zone = 14 OR ts.zone = 15 OR keyval(27) > 1 OR (keyval(14) > 1 AND keyval(29) > 0) THEN flipy = 19
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
clearpage 2
loadpage game + ".mxs", ts.cutfrom, 2
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
  ts.x = small(mouse(0), 300)
  ts.y = small(mouse(1), 180)
 END IF
 IF keyval(1) > 1 THEN
  EXIT DO
 END IF
 inc = 1: IF keyval(56) > 0 THEN inc = 20
 IF keyval(72) AND 5 THEN ts.y = large(ts.y - inc, 0): IF ts.gotmouse THEN movemouse ts.x, ts.y
 IF keyval(80) AND 5 THEN ts.y = small(ts.y + inc, 180): IF ts.gotmouse THEN movemouse ts.x, ts.y
 IF keyval(75) AND 5 THEN ts.x = large(ts.x - inc, 0): IF ts.gotmouse THEN movemouse ts.x, ts.y
 IF keyval(77) AND 5 THEN ts.x = small(ts.x + inc, 300): IF ts.gotmouse THEN movemouse ts.x, ts.y
 IF enter_or_space() OR (mouse(3) > 0 AND ts.zone < 11) THEN
  IF ts.delay = 0 THEN
   setkeys
   FOR i = 0 TO 19
    FOR j = 0 TO 19
     putpixel ts.tilex * 20 + i, ts.tiley * 20 + j, readpixel(ts.x + i, ts.y + j, 2), 3
    NEXT j
   NEXT i
   EXIT DO
  END IF
 END IF
 '---PICK BACKGROUND PAGE------
 oldcut = ts.cutfrom
 intgrabber ts.cutfrom, 0, gen(genMaxBackdrop) - 1, 51, 52
 IF ts.zone = 11 AND mouse(3) > 0 THEN ts.cutfrom = loopvar(ts.cutfrom, 0, gen(genMaxBackdrop) - 1, -1)
 IF ts.zone = 12 AND mouse(3) > 0 THEN ts.cutfrom = loopvar(ts.cutfrom, 0, gen(genMaxBackdrop) - 1, 1)
 IF oldcut <> ts.cutfrom THEN loadpage game + ".mxs", ts.cutfrom, 2
 '----
 drawline ts.x, ts.y, ts.x + 19, ts.y, 10 + tog * 5, dpage
 drawline ts.x, ts.y, ts.x, ts.y + 19, 10 + tog * 5, dpage
 drawline ts.x + 19, ts.y + 19, ts.x + 19, ts.y, 10 + tog * 5, dpage
 drawline ts.x + 19, ts.y + 19, ts.x, ts.y + 19, 10 + tog * 5, dpage
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
 copypage 2, dpage
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

sub loadwuc(spritefile$, j, top, sets, xw,yw, soff, perset, size,placer(), workpal(), poffset())
getpal16 workpal(), j - top, poffset(j)
IF j <= sets THEN
 setpicstuf buffer(), size * perset, 2
 loadset spritefile$, large(j, 0), 0
 FOR o = 0 TO (perset - 1)
  loadsprite placer(), 0, size * o, 0, xw, yw, 2
  stosprite placer(), 0, size * o, soff * (j - top), 3
 NEXT o
END IF

end sub

sub savewuc(spritefile$, j, top, sets, xw,yw, soff, perset, size,placer(), workpal(), poffset())
IF j <= sets THEN
 setpicstuf buffer(), size * perset, 2
 FOR o = 0 TO (perset - 1)
  loadsprite placer(), 0, size * o, soff * (j - top), xw, yw, 3
  stosprite placer(), 0, size * o, 0, 2
 NEXT o
 storeset spritefile$, large(j, 0), 0
END IF

end sub
