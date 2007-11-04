'OHRRPGCE - Some utility code
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
' This file contains utility subs and functions which would be useful for
' any FreeBasic program. Nothing in here can depend on Allmodex, nor on any
' gfx or music backend, nor on any other part of the OHR

#include "compat.bi"
#include "util.bi"

FUNCTION bound (n, lowest, highest)
bound = n
IF n < lowest THEN bound = lowest
IF n > highest THEN bound = highest
END FUNCTION

FUNCTION large (n1, n2)
large = n1
IF n2 > n1 THEN large = n2
END FUNCTION

FUNCTION loopvar (value, min, max, inc)
a = value + inc
IF a > max THEN a = a - ((max - min) + 1): loopvar = a: EXIT FUNCTION
IF a < min THEN a = a + ((max - min) + 1): loopvar = a: EXIT FUNCTION
loopvar = a
END FUNCTION

FUNCTION small (n1, n2)
small = n1
IF n2 < n1 THEN small = n2
END FUNCTION

FUNCTION trimpath$ (filename$)
'return the filename without path
dim i as integer
for i = 0 to len(filename$) -1 
	if filename$[i] = asc("\") or filename$[i] = asc("/") then filename$[i] = asc(SLASH)
next
IF INSTR(filename$,SLASH) = 0 THEN RETURN filename$
FOR i = LEN(filename$) TO 1 STEP -1
 IF MID$(filename$, i, 1) = SLASH THEN i += 1 : EXIT FOR
NEXT
RETURN MID$(filename$, i)
END FUNCTION

FUNCTION trimfilename$ (filename$)
'return the path without the filename
dim i as integer
for i = 0 to len(filename$) -1 
	if filename$[i] = asc("\") or filename$[i] = asc("/") then filename$[i] = asc(SLASH)
next
IF INSTR(filename$,SLASH) = 0 THEN RETURN ""
FOR i = LEN(filename$) TO 1 STEP -1
 IF MID$(filename$, i, 1) = SLASH THEN i -= 1 : EXIT FOR
NEXT
RETURN MID$(filename$, 1, i)
END FUNCTION

FUNCTION trimextension$ (filename$)
'return the filename without extension
IF INSTR(filename$,".") = 0 THEN RETURN filename$
FOR i = LEN(filename$) TO 1 STEP -1
 IF MID$(filename$, i, 1) = "." THEN i -= 1 : EXIT FOR
NEXT
RETURN MID$(filename$, 1, i)
END FUNCTION

FUNCTION justextension$ (filename$)
'return only the extension (everything after the *last* period)
FOR i = LEN(filename$) TO 1 STEP -1
 char$ = MID$(filename$, i, 1)
 IF char$ = "." THEN RETURN RIGHT$(filename$, LEN(filename$) - i)
 IF char$ = SLASH THEN RETURN ""
NEXT
RETURN ""
END FUNCTION

FUNCTION anycase$ (filename$)
 'make a filename case-insensitive
#IFDEF __FB_LINUX__
 DIM ascii AS INTEGER
 result$ = ""
 FOR i = 1 TO LEN(filename$)
  ascii = ASC(MID$(filename$, i, 1))
  IF ascii >= 65 AND ascii <= 90 THEN
   result$ = result$ + "[" + CHR$(ascii) + CHR$(ascii + 32) + "]"
  ELSEIF ascii >= 97 AND ascii <= 122 THEN
   result$ = result$ + "[" + CHR$(ascii - 32) + CHR$(ascii) + "]"
  ELSE
   result$ = result$ + CHR$(ascii)
  END IF
 NEXT i
 RETURN result$
#ELSE
 'Windows filenames are always case-insenstitive
 RETURN filename$
#ENDIF
END FUNCTION

SUB touchfile (filename$)
fh = FREEFILE
OPEN filename$ FOR BINARY AS #fh
CLOSE #fh
END SUB

FUNCTION rotascii$ (s$, o)
temp$ = ""
FOR i = 1 TO LEN(s$)
 temp$ = temp$ + CHR$(loopvar(ASC(MID$(s$, i, 1)), 0, 255, o))
NEXT i
rotascii$ = temp$
END FUNCTION

FUNCTION escape_string(s AS STRING, chars AS STRING) AS STRING
 DIM i AS INTEGER
 DIM c AS STRING
 DIM result AS STRING
 result = ""
 FOR i = 1 to LEN(s)
  c = MID$(s, i, 1)
  IF INSTR(chars, c) THEN
   result = result & "\"
  END IF
  result = result & c
 NEXT i
 RETURN result
END FUNCTION

SUB createstack (st as Stack)
  WITH st
    .bottom = allocate(512 * sizeof(integer))
    IF .bottom = 0 THEN
      'oh dear
      'debug "Not enough memory for stack"
      EXIT SUB
    END IF
    .pos = .bottom
    .size = 512
  END WITH
END SUB

SUB destroystack (st as Stack)
  IF st.size > 0 THEN
    deallocate st.bottom
    st.size = -1
  END IF
END SUB

SUB checkoverflow (st as Stack, byval amount as integer = 1)
  WITH st
    IF .pos - .bottom + amount >= .size THEN
      DIM newptr as integer ptr
      newptr = reallocate(.bottom, .size + 512 * sizeof(integer))
      IF newptr = 0 THEN
        'debug "stack: out of memory"
        .bottom = 0
        EXIT SUB
      END IF
      .size += 512
      .pos += newptr - .bottom
      .bottom = newptr
    END IF
  END WITH
END SUB

'read an int from the stack relative to current position (eg -1 is last word pushed - off should be negative)
FUNCTION reads (st as Stack, BYVAL off as integer) as integer
 IF st.pos - off >= st.bottom THEN
  reads = st.pos[off]
 END IF
END FUNCTION
