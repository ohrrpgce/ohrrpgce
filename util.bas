'OHRRPGCE - Some utility code
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
' This file contains utility subs and functions which would be useful for
' any FreeBasic program. Nothing in here can depend on Allmodex, nor on any
' gfx or music backend, nor on any other part of the OHR

CONST STACK_SIZE_INC = 512 ' in integers

#include "compat.bi"
#include "util.bi"

#if __FB_LANG__ <> "fb"
OPTION EXPLICIT
#endif

'DECLARE SUB debug (str$)

FUNCTION bound (BYVAL n as integer, BYVAL lowest as integer, BYVAL highest as integer) as integer
bound = n
IF n < lowest THEN bound = lowest
IF n > highest THEN bound = highest
END FUNCTION

FUNCTION bound (BYVAL n AS DOUBLE, BYVAL lowest AS DOUBLE, BYVAL highest AS DOUBLE) AS DOUBLE
bound = n
IF n < lowest THEN bound = lowest
IF n > highest THEN bound = highest
END FUNCTION

FUNCTION large (BYVAL n1 as integer, BYVAL n2 as integer) as integer
large = n1
IF n2 > n1 THEN large = n2
END FUNCTION

FUNCTION loopvar (BYVAL value as integer, BYVAL min as integer, BYVAL max as integer, BYVAL inc as integer) as integer
dim as integer a = value + inc
IF a > max THEN loopvar = a - ((max - min) + 1): EXIT FUNCTION
IF a < min THEN loopvar = a + ((max - min) + 1): EXIT FUNCTION
loopvar = a
END FUNCTION

FUNCTION small (BYVAL n1 as integer, BYVAL n2 as integer) as integer
small = n1
IF n2 < n1 THEN small = n2
END FUNCTION

FUNCTION trimpath(filename as string) as string
'return the file/directory name without path
dim i as integer
for i = 0 to len(filename) -1 
	if filename[i] = asc("\") or filename[i] = asc("/") then filename[i] = asc(SLASH)
next
IF filename <> "" ANDALSO filename[LEN(filename) - 1] = asc(SLASH) THEN
 filename = MID(filename, 1, LEN(filename) - 1)
END IF
IF INSTR(filename,SLASH) = 0 THEN RETURN filename
FOR i = LEN(filename) TO 1 STEP -1
 IF MID(filename, i, 1) = SLASH THEN i += 1 : EXIT FOR
NEXT
RETURN MID(filename, i)
END FUNCTION

FUNCTION trimfilename (filename as string) as string
'return the path without the filename
dim i as integer
for i = 0 to len(filename) -1 
	if filename[i] = asc("\") or filename[i] = asc("/") then filename[i] = asc(SLASH)
next
IF INSTR(filename,SLASH) = 0 THEN RETURN ""
FOR i = LEN(filename) TO 1 STEP -1
 IF MID(filename, i, 1) = SLASH THEN i -= 1 : EXIT FOR
NEXT
RETURN MID(filename, 1, i)
END FUNCTION

FUNCTION trimextension (filename as string) as string
'return the filename without extension
dim as integer i
IF INSTR(filename,".") = 0 THEN RETURN filename
FOR i = LEN(filename) TO 1 STEP -1
 IF MID(filename, i, 1) = "." THEN i -= 1 : EXIT FOR
NEXT
RETURN MID(filename, 1, i)
END FUNCTION

FUNCTION justextension (filename as string) as string
'return only the extension (everything after the *last* period)
FOR i as integer = LEN(filename) TO 1 STEP -1
 dim as string char = MID(filename, i, 1)
 IF char = "." THEN RETURN RIGHT(filename, LEN(filename) - i)
 IF char = SLASH THEN RETURN ""
NEXT
RETURN ""
END FUNCTION

FUNCTION anycase (filename as string) as string
 'make a filename case-insensitive
#IFDEF __FB_LINUX__
 DIM ascii AS INTEGER
 dim as string result = ""
 FOR i as integer = 1 TO LEN(filename)
  ascii = ASC(MID(filename, i, 1))
  IF ascii >= 65 AND ascii <= 90 THEN
   result = result + "[" + CHR(ascii) + CHR(ascii + 32) + "]"
  ELSEIF ascii >= 97 AND ascii <= 122 THEN
   result = result + "[" + CHR(ascii - 32) + CHR(ascii) + "]"
  ELSE
   result = result + CHR(ascii)
  END IF
 NEXT i
 RETURN result
#ELSE
 'Windows filenames are always case-insenstitive
 RETURN filename
#ENDIF
END FUNCTION

SUB touchfile (filename as string)
dim as integer fh = FREEFILE
OPEN filename FOR BINARY AS #fh
CLOSE #fh
END SUB

FUNCTION rotascii (s as string, o as integer) as string
 dim as string temp = ""
 FOR i as integer = 1 TO LEN(s)
  temp = temp + CHR(loopvar(ASC(MID(s, i, 1)), 0, 255, o))
 NEXT i
 RETURN temp
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
    .size = STACK_SIZE_INC - 4
    .bottom = allocate(STACK_SIZE_INC * sizeof(integer))
    IF .bottom = 0 THEN
      'oh dear
      'debug "Not enough memory for stack"
      EXIT SUB
    END IF
    .pos = .bottom
  END WITH
END SUB

SUB destroystack (st as Stack)
  IF st.bottom <> 0 THEN
    deallocate st.bottom
    st.size = -1
  END IF
END SUB

SUB checkoverflow (st as Stack, byval amount as integer = 1)
  WITH st
    IF .pos - .bottom + amount >= .size THEN
      .size += STACK_SIZE_INC
      IF .size > STACK_SIZE_INC * 4 THEN .size += STACK_SIZE_INC
      'debug "new stack size = " & .size & " * 4  pos = " & (.pos - .bottom) & " amount = " & amount
      'debug "nowscript = " & nowscript & " " & scrat(nowscript).id & " " & scriptname$(scrat(nowscript).id) 

      DIM newptr as integer ptr
      newptr = reallocate(.bottom, .size * sizeof(integer))
      IF newptr = 0 THEN
        'debug "stack: out of memory"
        EXIT SUB
      END IF
      .pos += newptr - .bottom
      .bottom = newptr
    END IF
  END WITH
END SUB

FUNCTION sign_string(n AS INTEGER, neg_str AS STRING, zero_str AS STRING, pos_str AS STRING) AS STRING
 IF n < 0 THEN RETURN neg_str
 IF n > 0 THEN RETURN pos_str
 RETURN zero_str
END FUNCTION

FUNCTION zero_default(n as integer, zerocaption AS STRING="default", displayoffset AS INTEGER = 0) AS STRING
 IF n = 0 THEN RETURN zerocaption
 RETURN "" & (n + displayoffset)
END FUNCTION

Function wordwrap(Byval z as string, byval wid as integer, byval sep as string) as string
 dim as string ret, in
 in = z
 if len(in) <= wid then return in
 
 dim as integer i, j
 do
  for i = 1 to small(wid + 1, len(in))
   if mid(in, i, 1) = sep then
    ret &= left(in, i - 1) & sep
    in = mid(in, i + 1)
    continue do
   end if
  next
  
  if i > len(in) then
   ret &= in
   in = ""
   exit do
  end if
  
  
  
  for j = i - 1 to 1 step -1
   if mid(in, j, 1) = " " then
    'bingo!
    ret &= left(in, j - 1) & sep
    in = mid(in, j + 1)
    continue do
   end if
  next
  if j = 0 then 'words too long, we need to cut it off
   ret &= left(in, wid) & sep
   in = mid(in, wid + 1)
  end if
 loop while in <> ""
 
 return ret
 
end function

sub split(byval z as string, ret() as string, sep as string = chr(10))
 redim ret(0)
 dim as integer i = 0, i2 = 1, j = 0
 dim as string in = z
 i = instr(i2, in, sep)
 if i = 0 then
  ret(0) = in
  exit sub
 end if
 do
  redim preserve ret(j) 
  if i = 0 then 
   ret(j) = mid(in, i2)
   exit do
  else
   ret(j) = mid(in, i2, i - i2)
  end if
  i2 = i + 1
  i = instr(i2, in, sep)
  j+=1
 loop
end sub

function textwidth(byval z as string) as integer
 dim lines() as string
 split(z, lines())
 dim ret as integer = 0
 for i as integer = 0 to ubound(lines)
  if len(lines(i)) > ret then ret = len(lines(i))
 next
 return ret * 8
end function

SUB str_array_append (array() AS STRING, s AS STRING)
 REDIM PRESERVE array(UBOUND(array) + 1) AS STRING
 array(UBOUND(array)) = s
END SUB
