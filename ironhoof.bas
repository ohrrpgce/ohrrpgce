'OHRRPGCE CUSTOM - Ironhoof's donated code
'(C) Copyright 2001 Russel Hamrick
' TODO: Resolve licencing permission!!!
'
'$DYNAMIC
DEFINT A-Z
'---------------------------------------------------------------------------
'DECLARE SUB Nprint (a, x, y, p)

DECLARE SUB setmodeX ()
DECLARE SUB restoremode ()
DECLARE SUB setvispage (BYVAL pagenumber%)
DECLARE SUB clearpage (BYVAL pagenumber%)
DECLARE SUB copypage (BYVAL sourcepage%, BYVAL destpage%)
DECLARE SUB setfont (font%())
DECLARE SUB textcolor (BYVAL fg%, BYVAL bg%)
DECLARE SUB printstr (text$, BYVAL x%, BYVAL y%, BYVAL page%)
DECLARE SUB putpixel (BYVAL x%, BYVAL y%, BYVAL col%, BYVAL page%)
DECLARE FUNCTION readpixel% (BYVAL x%, BYVAL y%, BYVAL page%)
DECLARE SUB rectangle (BYVAL x%, BYVAL y%, BYVAL w%, BYVAL h%, BYVAL col%, BYVAL page%)
DECLARE SUB fuzzyrect (BYVAL x%, BYVAL y%, BYVAL w%, BYVAL h%, BYVAL col%, BYVAL page%)
DECLARE SUB drawline (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL col%, BYVAL page%)
DECLARE SUB paintat (BYVAL x%, BYVAL y%, BYVAL col%, BYVAL page%, buffer%(), BYVAL bufsize%)
DECLARE SUB drawsprite (pic(), BYVAL picoff, pal(), BYVAL po, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB wardsprite (pic(), BYVAL picoff, pal(), BYVAL po, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB getsprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL page)
DECLARE SUB loadsprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL w, BYVAL h, BYVAL page)
DECLARE SUB stosprite (pic(), BYVAL picoff, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB bigsprite (pic(), pal(), BYVAL p, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB hugesprite (pic(), pal(), BYVAL p, BYVAL x, BYVAL y, BYVAL page)
DECLARE SUB setdiskpages (buf(), BYVAL h, BYVAL L)
DECLARE SUB loadpage (fil$, BYVAL i, BYVAL p)
DECLARE SUB storepage (fil$, BYVAL i, BYVAL p)
DECLARE SUB bitmap2page (temp(), bmp$, BYVAL p)
DECLARE SUB loadbmp (F$, BYVAL x, BYVAL y, buf(), BYVAL p)
DECLARE SUB getbmppal (F$, mpal(), pal(), BYVAL o)
DECLARE FUNCTION bmpinfo (F$, dat())
DECLARE SUB setupmusic (mbuf())
DECLARE SUB closemusic ()
DECLARE SUB stopsong ()
DECLARE SUB resumesong ()
DECLARE SUB resetfm ()
DECLARE SUB loadsong (F$)
DECLARE SUB fademusic (BYVAL vol)
DECLARE FUNCTION getfmvol ()
DECLARE SUB setfmvol (BYVAL vol)
'---------------------------------------------------------------------------
'DECLARE SUB QUAKE (L#)
'Quake runs from 0 to 32000, 0 wont even show while 32000 makes a perfect quake

DECLARE SUB ffbox (x, y, x2, y2, c, c2, p)
'it works just like rectangle
'but like the normal QB line command either x and y can be any corner
'FFBOX x , y , x2 , y2 , boarder color , back color , page

DECLARE SUB ellipse (x, y, radius, c, p, squish1, squish2)
'works mostly like the normal QB circle but with
'more usefull features
' ELLIPSE x , y , radius , color , page , vertical pull , horizontal pull
'the vertical pull & horizontal pull should be in decimals or whole
'numbers. when both numbers are large it shrinks the circle to fit
'the screen like so ellipse 10,10,25,7,0,25,40 will make the ellispe
'smaller. but if its smaller number is 1 or 0 (same) and the other large 0, 25 it will only bend not shrink the ellipse.

DECLARE SUB airbrush (x, y, d, m, c, p)
'AirBrush this rutine works VERY well parameters as fallows:
' AIRBRUSH x , y , diameter , mist_amount , color , page
' diameter sets the width & hight by square radius
' mist_amount sets how many pixels to place i put 100 and it ran fast so
' it works EXCELLENTLY with a mouse on the DTE =)
'---------------------------------------------------------------------------

'setmodeX
'setvispage 0
'DIM font(1024)
'
'DEF SEG = VARSEG(font(0)): BLOAD "ohrrpgce.fnt", VARPTR(font(0))
'textcolor 7, 0
'setfont font()
'a = 0: y = 0
'
'ellipse 150, 100, 30, 3, 0, 3, 0
'ellipse 150, 100, 30, 4, 0, 0, 3
'
'DO WHILE y < 3  'loop the quake 3 times for a sorta long rumble
'QUAKE 32000
'y = y + 1: LOOP
'
'DO                             'Make a bunch of paint puffs
'a$ = INKEY$: IF a$ <> "" THEN GOTO done2:
'x# = RND * 319: y# = RND * 199: c# = RND * 15
'AirBrush x#, y#, 20, 55, c#, 0
'LOOP
'done2:
'QUAKE 32000
'clearpage 0
'clearpage 0
'fuzzyrect 0, 0, 319, 199, 1, 0
'ffbox 0, 0, 150, 100, 15, 1, 0
'printstr "AINT IT SPIFFISH?", 5, 5, 0
'x$ = INPUT$(2)

'$INCLUDE: 'cglobals.bi'

REM $STATIC
SUB airbrush (x, y, d, m, c, p)

DO WHILE count < m
x2 = RND * d
y2 = RND * d
r = d \ 2
x3 = x - r
y3 = y - r
IF ABS((x3 + x2) - x) ^ 2 + ABS((y3 + y2) - y) ^ 2 < r ^ 2 THEN
 putpixel x3 + x2, y3 + y2, c, p
END IF
count = count + 1: LOOP

END SUB

SUB ellipse (x, y, radius, c, p, squish1, squish2)

r = radius
b = squish1
b2 = squish2

IF b = 0 THEN b = 1
IF b2 = 0 THEN b2 = 1
'IF b > b2 THEN r = r * b
'IF b < b2 THEN r = r * b2
t = -45
weave:
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
lx# = x2#: ly# = y2#
t = t + 4: IF t > 360 THEN  ELSE GOTO weave:

END SUB

SUB ffbox (x, y, x2, y2, c, c2, p)
IF x > x2 THEN SWAP x2, x
IF y > y2 THEN SWAP y2, y
rectangle x + 1, y, x2 - 2, y2, c, p
rectangle x, y + 1, x2, y2 - 2, c, p
rectangle x + 2, y + 2, x2 - 4, y2 - 4, c2, p
END SUB

