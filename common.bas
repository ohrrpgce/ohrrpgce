'OHRRPGCE - Some Custom/Game common code
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

'$INCLUDE: 'compat.bi'
'$INCLUDE: 'allmodex.bi'

'$INCLUDE: 'uiconst.bi'

COMMON SHARED uilook()



SUB getui (f$)
'load ui colors from data lump
'(lump not finalised, just set defaults for now)

RESTORE defaultui
FOR i=0 TO uiColors
 READ col%
 uilook(i) = col%
NEXT

'The QB editor moves this data to the top, but QB still compiles fine
'with it here.
defaultui:
DATA 0,7,8,14,15,6,7,1,2,18,21,35,37,15,240,10,14
DATA 18,28,34,44,50,60,66,76,82,92,98,108,114,124,130,140
DATA 146,156,162,172,178,188,194,204,210,220,226,236,242,252 

END SUB

FUNCTION trimpath$ (filename$)
'return the filename without path
FOR i = LEN(filename$) TO 1 STEP -1
 IF MID$(filename$, i, 1) = SLASH THEN i += 1 : EXIT FOR
NEXT
trimpath$ = MID$(filename$, i)
END FUNCTION
