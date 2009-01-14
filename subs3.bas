'OHRRPGCE CUSTOM - Even more misc unsorted routines
'(C) Copyright 1997-2005 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
'$DYNAMIC
DEFINT A-Z

#include "const.bi"

'basic subs and functions
DECLARE FUNCTION str2lng& (stri$)
DECLARE FUNCTION filenum$ (n%)
DECLARE SUB writeconstant (filehandle%, num%, names AS STRING, unique$(), prefix$)
DECLARE SUB writeglobalstring (index%, s$, maxlen%)
DECLARE FUNCTION numbertail$ (s$)
DECLARE SUB cropafter (index%, limit%, flushafter%, lump$, bytes%, prompt%)
DECLARE FUNCTION isunique% (s$, u$(), r%)
DECLARE FUNCTION exclude$ (s$, x$)
DECLARE FUNCTION exclusive$ (s$, x$)
DECLARE SUB testanimpattern (tastuf%(), taset%)
DECLARE SUB formation ()
DECLARE SUB enemydata ()
DECLARE SUB herodata ()
DECLARE SUB attackdata ()
DECLARE SUB getnames (stat$(), max%)
DECLARE SUB statname ()
DECLARE SUB textage ()
DECLARE FUNCTION sublist% (num%, s$())
DECLARE SUB maptile (font%())

#include "compat.bi"
#include "allmodex.bi"
#include "common.bi"
#include "customsubs.bi"
#include "cglobals.bi"

#include "scrconst.bi"

REM $STATIC

FUNCTION exclude$ (s$, x$)
outf$ = ""
FOR i = 1 TO LEN(s$)
 ok = -1
 FOR j = 1 TO LEN(x$)
  IF MID$(s$, i, 1) = MID$(x$, j, 1) THEN ok = 0
 NEXT j
 IF ok THEN outf$ = outf$ + MID$(s$, i, 1)
NEXT i
exclude$ = outf$
END FUNCTION

FUNCTION exclusive$ (s$, x$)
outf$ = ""
FOR i = 1 TO LEN(s$)
 ok = 0
 FOR j = 1 TO LEN(x$)
  IF MID$(s$, i, 1) = MID$(x$, j, 1) THEN ok = 1
 NEXT j
 IF ok THEN outf$ = outf$ + MID$(s$, i, 1)
NEXT i
exclusive$ = outf$
END FUNCTION

FUNCTION filesize$ (file$)
'returns size of a file in formatted string
IF isfile(file$) THEN
 ff = FREEFILE
 OPEN file$ FOR BINARY AS #ff
 size = LOF(ff)
 CLOSE #ff

 units$ = " B"
 spl = 0
 IF size > 1024 THEN spl = 1 : units$ = " KB"
 IF size > 1048576 THEN spl = 1 : size = size / 1024 : units$ = " MB"
 fsize$ = STR$(size)
 IF spl <> 0 THEN
  size = size / 102.4
  fsize$ = STR$(size \ 10)
  IF size < 1000 THEN fsize$ = fsize$ + "." + STR$(size MOD 10)
 END IF
 filesize$ = fsize$ + units$
ELSE
 filesize$ = "N/A"
END IF
END FUNCTION

FUNCTION numbertail$ (s$)

DIM n AS LONG

IF s$ = "" THEN
 outf$ = "BLANK"
ELSE
 a = ASC(RIGHT$(s$, 1))
 IF a < 48 OR a > 57 THEN
  outf$ = s$ + "2"
 ELSE
  a$ = s$
  b$ = ""
  DO WHILE ASC(RIGHT$(a$, 1)) >= 48 AND ASC(RIGHT$(a$, 1)) <= 57
   b$ = RIGHT$(a$, 1) + b$
   a$ = LEFT$(a$, LEN(a$) - 1)
   IF LEN(a$) = 0 THEN EXIT DO
  LOOP
  IF LEN(b$) > 9 THEN b$ = "0"
  n = str2int(b$)
  n = n + 1
  outf$ = a$ + STR$(n)
 END IF
END IF

numbertail$ = outf$

END FUNCTION

FUNCTION str2lng& (stri$)

n& = 0
s$ = LTRIM$(stri$)
sign = 1

FOR i = 1 TO LEN(s$)
 c$ = MID$(s$, i, 1)
 IF c$ = "-" AND i = 1 THEN sign = -1
 c = ASC(c$) - 48
 IF c >= 0 AND c <= 9 THEN
  n& = n& * 10 + (c * sign)
 END IF
NEXT i

str2lng& = n&

END FUNCTION

SUB writeglobalstring (index, s$, maxlen)

fh = FREEFILE

OPEN game + ".stt" FOR BINARY AS #fh

a$ = CHR$(small(LEN(s$), small(maxlen, 255)))
PUT #fh, 1 + index * 11, a$
a$ = LEFT$(s$, small(maxlen, 255))
PUT #fh, 2 + index * 11, a$

CLOSE #fh

END SUB
