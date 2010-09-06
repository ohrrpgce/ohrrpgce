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

#if defined(IS_GAME) OR defined(IS_CUSTOM)
 DECLARE SUB debug (s as string)
#else
 PRIVATE SUB debug (s as string)
  PRINT s
 END SUB
#endif

 '------------- Other -------------

FUNCTION bitcount (BYVAL v as unsigned integer) as integer
  'From the "Software Optimization Guide for AMD Athlon 64 and Opteron Processors". Thanks, AMD!
  v = v - ((v SHR 1) AND &h55555555)
  v = (v AND &h33333333) + ((v SHR 2) AND &h33333333)
  RETURN ((v + (v SHR 4) AND &hF0F0F0F) * &h1010101) SHR 24
END FUNCTION

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

FUNCTION range (number AS INTEGER, percent AS INTEGER) AS INTEGER
 DIM a AS INTEGER
 a = (number / 100) * percent
 RETURN number + INT(RND * (a * 2)) - a
END FUNCTION

FUNCTION rpad (s AS STRING, pad_char AS STRING, size AS INTEGER) AS STRING
 DIM result AS STRING
 result = LEFT(s, size)
 WHILE LEN(result) < size: result = result & pad_char: WEND
 RETURN result
END FUNCTION

'Like INSTR, but return the n-th match
'Returns 0 if not found
FUNCTION Instr_nth (BYVAL start AS INTEGER, s AS STRING, substring AS STRING, BYVAL nth AS INTEGER = 1) AS INTEGER
 DIM temp AS INTEGER = start - 1
 IF nth < 1 THEN RETURN 0
 FOR n AS INTEGER = 1 TO nth
  temp = INSTR(temp + 1, s, substring)
  IF temp = 0 THEN RETURN 0
 NEXT
 RETURN temp
END FUNCTION

'Like INSTR without start point, but return the n-th match
'Returns 0 if not found
FUNCTION Instr_nth (s AS STRING, substring AS STRING, BYVAL nth AS INTEGER = 1) AS INTEGER
 RETURN Instr_nth(1, s, substring, nth)
END FUNCTION

FUNCTION is_int (s AS STRING) AS INTEGER
 'Even stricter than str2int (doesn't accept "00")
 DIM n AS INTEGER = VALINT(s)
 RETURN (n <> 0 ANDALSO n <> VALINT(s + "1")) ORELSE s = "0"
END FUNCTION

FUNCTION str2int (stri as string, default as integer=0) as integer
 'Use this in contrast to QuickBasic's VALINT.
 'it is stricter, and returns a default on failure
 DIM n AS INTEGER = 0
 DIM s AS STRING = LTRIM(stri)
 IF s = "" THEN RETURN default
 DIM sign AS INTEGER = 1

 DIM ch AS STRING
 DIM c AS INTEGER
 FOR i AS INTEGER = 1 TO LEN(s)
  ch = MID(s, i, 1)
  IF ch = "-" AND i = 1 THEN
   sign = -1
   CONTINUE FOR
  END IF
  c = ASC(ch) - 48
  IF c >= 0 AND c <= 9 THEN
   n = n * 10 + (c * sign)
  ELSE
   RETURN default
  END IF
 NEXT i

 RETURN n
END FUNCTION

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

FUNCTION exclude (s as string, x as string) as string
 DIM outf AS STRING = ""
 DIM ok AS INTEGER
 FOR i AS INTEGER = 1 TO LEN(s)
  ok = -1
  FOR j AS INTEGER = 1 TO LEN(x)
   IF MID(s, i, 1) = MID(x, j, 1) THEN ok = 0
  NEXT j
  IF ok THEN outf &= MID(s, i, 1)
 NEXT i
 RETURN outf
END FUNCTION

FUNCTION exclusive (s as string, x as string) as string
 DIM outf AS STRING = ""
 DIM ok AS INTEGER
 FOR i AS INTEGER = 1 TO LEN(s)
  ok = 0
  FOR j AS INTEGER = 1 TO LEN(x)
   IF MID(s, i, 1) = MID(x, j, 1) THEN ok = -1
  NEXT j
  IF ok THEN outf &= MID(s, i, 1)
 NEXT i
 RETURN outf
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

FUNCTION iif_string(byval condition as integer, s1 as string, s2 as string) as string
 IF condition THEN RETURN s1 ELSE RETURN s2
END FUNCTION

'returns a copy of the string with separators inserted; use together with split()
Function wordwrap(z as string, byval wid as integer, sep as string = chr(10)) as string
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

'Splits a line at the separators; use together with wordwrap() to do wrapping
sub split(in as string, ret() as string, sep as string = chr(10))
 redim ret(0)
 dim as integer i = 0, i2 = 1, j = 0
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

function textwidth(z as string) as integer
 dim lines() as string
 split(z, lines())
 dim ret as integer = 0
 for i as integer = 0 to ubound(lines)
  if len(lines(i)) > ret then ret = len(lines(i))
 next
 return ret * 8
end function

SUB flusharray (array() AS INTEGER, BYVAL size AS INTEGER=-1, BYVAL value AS INTEGER=0)
 'If size is -1, then flush the entire array
 IF size = -1 THEN size = UBOUND(array)
 FOR i AS INTEGER = LBOUND(array) TO size
  array(i) = value
 NEXT i
END SUB

SUB str_array_append (array() AS STRING, s AS STRING)
 REDIM PRESERVE array(LBOUND(array) TO UBOUND(array) + 1) AS STRING
 array(UBOUND(array)) = s
END SUB

SUB int_array_append (array() AS INTEGER, BYVAL k AS INTEGER)
 REDIM PRESERVE array(LBOUND(array) TO UBOUND(array) + 1) AS INTEGER
 array(UBOUND(array)) = k
END SUB

SUB intstr_array_append (array() AS IntStrPair, BYVAL k AS INTEGER, s AS STRING)
 REDIM PRESERVE array(LBOUND(array) TO UBOUND(array) + 1)
 array(UBOUND(array)).i = k
 array(UBOUND(array)).s = s
END SUB

FUNCTION int_array_find (array() AS INTEGER, BYVAL value AS INTEGER) AS INTEGER
 FOR i AS INTEGER = LBOUND(array) TO UBOUND(array)
  IF array(i) = value THEN RETURN i
 NEXT
 RETURN -1
END FUNCTION

'Resize a dynamic int array, removing all occurrences of k
SUB int_array_remove (array() as integer, byval k as integer)
 DIM i AS INTEGER = LBOUND(array)
 WHILE i <= UBOUND(array)
  IF array(i) = k THEN
   'Shuffle down
   FOR j AS INTEGER = i TO UBOUND(array) - 1
    array(j) = array(j + 1)
   NEXT
   IF UBOUND(array) > LBOUND(array) THEN REDIM PRESERVE array(LBOUND(array) TO UBOUND(array) - 1)
  END IF
  i += 1
 WEND
END SUB

'I've compared the speed of the following two. For random integers, the quicksort is faster
'for arrays over length about 80. For arrays which are 90% sorted appended with 10% random data,
'the cut off is about 600 (insertion sort did ~5x better on nearly-sort data at the 600 mark)

'Returns, in indices() (assumed to already have been dimmed large enough), indices for
'visiting the data (an array of some kind of struct containing an integer) in ascending order.
'start points to the integer in the first element, stride is the size of an array element, in integers
'Insertion sort. Running time is O(n^2). Much faster on nearly-sorted lists. STABLE
SUB sort_integers_indices(indices() as integer, BYVAL start as integer ptr, BYVAL number as integer, BYVAL stride as integer)
 IF number = 0 THEN number = UBOUND(indices) + 1
 DIM keys(number - 1) as integer
 DIM as integer i, temp
 FOR i = 0 TO number - 1
  keys(i) = *start
  start = CAST(integer ptr, CAST(byte ptr, start) + stride) 'yuck
 NEXT

 indices(0) = 0
 FOR j as integer = 1 TO number - 1
  temp = keys(j)
  FOR i = j - 1 TO 0 STEP -1
   IF keys(i) <= temp THEN EXIT FOR
   keys(i + 1) = keys(i)
   indices(i + 1) = indices(i)
  NEXT
  keys(i + 1) = temp
  indices(i + 1) = j
 NEXT
END SUB

TYPE FnQsortCompare as FUNCTION CDECL (BYVAL as any ptr, BYVAL as any ptr) as integer

FUNCTION integer_compare CDECL (BYVAL a as integer ptr ptr, BYVAL b as integer ptr ptr) as integer
 IF **a < **b THEN RETURN -1
 IF **a > **b THEN RETURN 1
 'implicitly RETURN 0 (it's faster to omit the RETURN :-)
END FUNCTION

'a string ptr is a pointer to a FB string descriptor
FUNCTION string_compare CDECL (BYVAL a as string ptr ptr, BYVAL b as string ptr ptr) as integer
 'This is equivalent, but the code below can be adapted for case insensitive compare (and is faster)
 'RETURN fb_StrCompare( **a, -1, **b, -1)

 DIM as integer ret = 0, somenull = 0
 'Ah, brings back happy memories of C hacking, doesn'it?
 IF @((**a)[0]) = 0 THEN ret -= 1: somenull = 1
 IF @((**b)[0]) = 0 THEN ret += 1: somenull = 1
 IF somenull THEN RETURN ret

 DIM k AS INTEGER = 0
 DIM chara as ubyte
 DIM charb as ubyte
 DO
  chara = (**a)[k]
  charb = (**b)[k]
  IF chara < charb THEN
   RETURN -1
  ELSEIF chara > charb THEN
   RETURN 1
  END IF
  k += 1
 LOOP WHILE chara OR charb
 RETURN 0
END FUNCTION

'CRT Quicksort. Running time is *usually* O(n*log(n)). NOT STABLE
'See sort_integer_indices.
PRIVATE SUB qsort_indices(indices() as integer, BYVAL start as any ptr, BYVAL number as integer, BYVAL stride as integer, BYVAL compare_fn as FnQsortCompare)
 IF number = 0 THEN number = UBOUND(indices) + 1

 DIM keys(number - 1) as any ptr
 DIM i as integer
 FOR i = 0 TO number - 1
  keys(i) = start + stride * i
 NEXT

 qsort(@keys(0), number, sizeof(any ptr), compare_fn)

 FOR i = 0 TO number - 1
  indices(i) = CAST(integer, keys(i) - start) \ stride
 NEXT
END SUB

SUB qsort_integers_indices(indices() as integer, BYVAL start as integer ptr, BYVAL number as integer, BYVAL stride as integer)
 qsort_indices indices(), start, number, stride, CAST(FnQsortCompare, @integer_compare)
END SUB

SUB qsort_strings_indices(indices() as integer, BYVAL start as string ptr, BYVAL number as integer, BYVAL stride as integer)
 qsort_indices indices(), start, number, stride, CAST(FnQsortCompare, @string_compare)
END SUB

'Invert a permutation such as that returned by sort_integers_indices;
'indices() should contain the integers 0 to UBOUND(indices)
SUB invert_permutation(indices() as integer)
 DIM inverse(UBOUND(indices)) as integer
 FOR i as integer = 0 TO UBOUND(indices)
  inverse(indices(i)) = i
 NEXT
 'Copy back
 memcpy(@indices(0), @inverse(0), sizeof(integer) * (UBOUND(indices) + 1))
END SUB

'These cache functions store a 'resetter' string, which causes search_string_cache
'to automatically empty the cache when its value changes (eg, different game).
'Note that you can resize the cache arrays as you want at any time.
FUNCTION search_string_cache (cache() as IntStrPair, byval key as integer, resetter as string) as string
 IF cache(0).s <> resetter THEN
  cache(0).s = resetter
  cache(0).i = 0  'used to loop through the indices when writing
  
  FOR i as integer = 1 TO UBOUND(cache)
   cache(i).i = -1099999876
   cache(i).s = ""
  NEXT
 END IF

 FOR i as integer = 1 TO UBOUND(cache)
  IF cache(i).i = key THEN RETURN cache(i).s
 NEXT
END FUNCTION

SUB add_string_cache (cache() as IntStrPair, byval key as integer, value as string)
 DIM i as integer
 FOR i = 1 TO UBOUND(cache)
  IF cache(i).i = -1099999876 THEN
   cache(i).i = key
   cache(i).s = value
   EXIT SUB
  END IF
 NEXT
 'overwrite an existing entry, in a loop
 i = 1 + (cache(0).i MOD UBOUND(cache))
 cache(i).i = key
 cache(i).s = value
 cache(0).i = i
END SUB

SUB remove_string_cache (cache() as IntStrPair, byval key as integer)
 FOR i as integer = 1 TO UBOUND(cache)
  IF cache(i).i = key THEN
   cache(i).i = -1099999876
   cache(i).s = ""
   EXIT SUB
  END IF
 NEXT
END SUB

#define ROT(a,b) ((a shl b) or (a shr (32 - b)))

'Fairly fast (in original C) string hash, ported from from fb2c++ (as strihash,
'original was case insensitive) which I wrote and tested myself
FUNCTION strhash(byval strp as zstring ptr, byval leng as integer) as unsigned integer
 DIM as unsigned integer hash = &hbaad1dea

 IF (leng and 3) = 3 THEN
  hash xor= *strp shl 16
  strp += 1
 END IF
 IF (leng and 3) >= 2 THEN
  hash xor= *strp shl 8
  strp += 1
 END IF
 IF (leng and 3) >= 1 THEN
  hash xor= *strp
  strp += 1
  hash = (hash shl 5) - hash
  hash xor= ROT(hash, 19)
 END IF

 leng \= 4
 WHILE leng
  hash += *cast(unsigned integer ptr, strp)
  strp += 4
  hash = (hash shl 5) - hash  ' * 31
  hash xor= ROT(hash, 19)
  leng -= 1
 WEND
 'No need to be too thorough, will get rehashed if needed anyway
 hash += ROT(hash, 2)
 hash xor= ROT(hash, 27)
 hash += ROT(hash, 16)
 RETURN hash
END FUNCTION

FUNCTION strhash(hstr as string) as unsigned integer
 RETURN strhash(hstr, len(hstr))
END FUNCTION


'------------- File Functions -------------

FUNCTION trimpath(filename as string) as string
  'return the file/directory name without path
  dim i as integer
  for i = 0 to len(filename) - 1 
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

FUNCTION is_absolute_path (sDir as string) as integer
#IFDEF __UNIX__
  if left(sDir, 1) = "/" then return -1
#ELSE
  dim first as string = lcase(left(sDir, 1))
  if first = "\" then return -1
  if first >= "a" andalso first <= "z" andalso mid(sDir, 2, 2) = ":\" then return -1
#ENDIF
  return 0
END FUNCTION

FUNCTION anycase (filename as string) as string
  'make a filename case-insensitive
#IFDEF __FB_WIN32__
  'Windows filenames are always case-insenstitive
  RETURN filename
#ELSE
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
#ENDIF
END FUNCTION

SUB touchfile (filename as string)
  dim as integer fh = FREEFILE
  OPEN filename FOR BINARY AS #fh
  CLOSE #fh
END SUB

SUB findfiles (fmask AS STRING, BYVAL attrib AS INTEGER, outfile AS STRING)
  ' attrib 0: all files 'cept folders, attrib 16: folders only
#ifdef __UNIX__
  'this is pretty hacky, but works around the lack of DOS-style attributes, and the apparent uselessness of DIR
  DIM AS STRING grep, shellout
  shellout = "/tmp/ohrrpgce-findfiles-" + STR(RND * 10000) + ".tmp"
  grep = "-v '/$'"
  IF attrib AND 16 THEN grep = "'/$'"
  DIM i AS INTEGER
  FOR i = LEN(fmask) TO 1 STEP -1
    IF MID(fmask, i, 1) = CHR(34) THEN fmask = LEFT(fmask, i - 1) + "\" + CHR(34) + RIGHT(fmask, LEN(fmask) - i)
  NEXT i
  i = INSTR(fmask, "*")
  IF i THEN
    fmask = CHR(34) + LEFT(fmask, i - 1) + CHR(34) + RIGHT(fmask, LEN(fmask) - i + 1)
  ELSE
    fmask = CHR(34) + fmask + CHR(34)
  END IF
  SHELL "ls -d1p " + fmask + " 2>/dev/null |grep "+ grep + ">" + shellout + " 2>&1"
  DIM AS INTEGER f1, f2
  f1 = FreeFile
  OPEN shellout FOR INPUT AS #f1
  f2 = FreeFile
  OPEN outfile FOR OUTPUT AS #f2
  DIM s AS STRING
  DO UNTIL EOF(f1)
    LINE INPUT #f1, s
    IF s = "/dev/" OR s = "/proc/" OR s = "/sys/" THEN CONTINUE DO
    IF RIGHT(s, 1) = "/" THEN s = LEFT(s, LEN(s) - 1)
    DO WHILE INSTR(s, "/")
      s = RIGHT(s, LEN(s) - INSTR(s, "/"))
    LOOP
    PRINT #f2, s
  LOOP
  CLOSE #f1
  CLOSE #f2
  KILL shellout
#else
  DIM a AS STRING, i AS INTEGER, folder AS STRING
  if attrib = 0 then attrib = 255 xor 16
  if attrib = 16 then attrib = 55 '*sigh*
  FOR i = LEN(fmask) TO 1 STEP -1
    IF MID(fmask, i, 1) = "\" THEN folder = MID(fmask, 1, i): EXIT FOR
  NEXT

  DIM AS INTEGER tempf, realf
  tempf = FreeFile
  a = DIR(fmask, attrib)
  if a = "" then
    'create an empty file
    OPEN outfile FOR OUTPUT AS #tempf
    close #tempf
    exit sub
  end if
  OPEN outfile + ".tmp" FOR OUTPUT AS #tempf
  DO UNTIL a = ""
    PRINT #tempf, a
    a = DIR '("", attrib)
  LOOP
  CLOSE #tempf
  OPEN outfile + ".tmp" FOR INPUT AS #tempf
  realf = FREEFILE
  OPEN outfile FOR OUTPUT AS #realf
  DO UNTIL EOF(tempf)
  LINE INPUT #tempf, a
  IF attrib = 55 THEN
    'alright, we want directories, but DIR is too broken to give them to us
    'files with attribute 0 appear in the list, so single those out
    IF DIR(folder + a, 55) <> "" AND DIR(folder + a, 39) = "" THEN PRINT #realf, a
  ELSE
    PRINT #realf, a
  END IF
  LOOP
  CLOSE #tempf
  CLOSE #realf
  KILL outfile + ".tmp"
#endif
END SUB

SUB killdir(directory as string)
  dim fh as integer
  dim filename as string
  findfiles directory + SLASH + ALLFILES, 0, "filelist.tmp"
  fh = FREEFILE
  OPEN "filelist.tmp" FOR INPUT AS #fh
  DO UNTIL EOF(fh)
    LINE INPUT #fh, filename
    KILL directory + SLASH + filename
  LOOP
  CLOSE #fh
  KILL "filelist.tmp"
  RMDIR directory
  IF isdir(directory) THEN
    'debug "Failed to delete directory " & directory
  END IF
END SUB

SUB safekill (f as string)
  IF isfile(f) THEN KILL f
END SUB

FUNCTION fileisreadable(f as string) as integer
  dim fh as integer, err_code as integer
  fh = freefile
  err_code = open(f for binary access read as #fh)
  if err_code = 2 then
    ''debug f & " unreadable (ignored)"
    return 0
  elseif err_code <> 0 then
    'debug "Error " & err_code & " reading " & f   
    return 0
  end if
  close #fh
  return -1
END FUNCTION

FUNCTION fileiswriteable(f as string) as integer
  dim fh as integer
  fh = freefile
  if open (f for binary access read write as #fh) = 2 then
    ''debug f & " unreadable (ignored)"
    return 0 
  end if
  close #fh
  return -1
END FUNCTION

FUNCTION diriswriteable(d as string) as integer
  dim testfile as string = d & SLASH & "__testwrite_" & INT(RND * 100000) & ".tmp"
  if fileiswriteable(testfile) then
    kill testfile
    return -1
  end if
  return 0
END FUNCTION

FUNCTION isfile (n as string) as integer
  ' directories don't count as files
  ' this is a simple wrapper for fileisreadable
  if n = "" then return 0
  return fileisreadable(n)
END FUNCTION

FUNCTION isdir (sDir as string) as integer
#IFDEF __UNIX__
  'Special hack for broken Linux dir() behavior
  sDir = escape_string(sDir, """`\$")
  isdir = SHELL("[ -d """ + sDir + """ ]") = 0
#ELSE
  'Windows just uses dir
  dim ret as integer = dir(sDir, 55) <> "" AND dir(sDir, 39) = ""
  return ret
#ENDIF
END FUNCTION


'--------- Doubly Linked List ---------

#define DLFOLLOW(someptr)  cast(DListItem(Any) ptr, cast(byte ptr, someptr) + this.memberoffset)

SUB dlist_construct (byref this as DoubleList(Any), byval itemoffset as integer)
  this.numitems = 0
  this.first = NULL
  this.last = NULL
  this.memberoffset = itemoffset
END SUB

'NULL as beforeitem inserts at end
SUB dlist_insertat (byref this as DoubleList(Any), byval beforeitem as any ptr, byval newitem as any ptr)
  dim litem as DListItem(Any) ptr = DLFOLLOW(newitem)

  litem->next = beforeitem

  if beforeitem = NULL then
    litem->prev = this.last
    this.last = newitem
  else
    dim bitem as DListItem(Any) ptr = DLFOLLOW(beforeitem)
    litem->prev = bitem->prev
    bitem->prev = newitem
  end if

  if litem->prev then
    DLFOLLOW(litem->prev)->next = newitem
  else
    this.first = newitem
  end if

  this.numitems += 1
END SUB

SUB dlist_remove (byref this as DoubleList(Any), byval item as any ptr)
  dim litem as DListItem(Any) ptr = DLFOLLOW(item)

  'check whether item isn't the member of a list
  if litem->next = NULL andalso item <> this.last then exit sub

  if litem->prev then
    DLFOLLOW(litem->prev)->next = litem->next
  else
    this.first = litem->next
  end if
  if litem->next then
    DLFOLLOW(litem->next)->prev = litem->prev
  else
    this.last = litem->prev
  end if
  litem->next = NULL
  litem->prev = NULL

  this.numitems -= 1
END SUB

SUB dlist_swap (byref this as DoubleList(Any), byval item1 as any ptr, byref that as DoubleList(Any), byval item2 as any ptr)
  'dlist_insertat can't move items from one list to another
  if item1 = item2 then exit sub
  dim dest2 as any ptr = DLFOLLOW(item1)->next
  dlist_remove(this, item1)
  if dest2 = item2 then
    'items are arranged like  -> item1 -> item2 ->
    dlist_insertat(that, DLFOLLOW(item2)->next, item1)
  else
    dlist_insertat(that, item2, item1)
    dlist_remove(that, item2)
    dlist_insertat(this, dest2, item2)
  end if
END SUB

FUNCTION dlist_find (byref this as DoubleList(Any), byval item as any ptr) as integer
  dim n as integer = 1
  dim lit as any ptr = this.first
  while lit
    if lit = item then return n
    n += 1
    lit = DLFOLLOW(lit)->next
  wend
  return 0
END FUNCTION

FUNCTION dlist_walk (byref this as DoubleList(Any), byval item as any ptr, byval n as integer) as any ptr
  if item = NULL then item = this.first
  while n > 0 andalso item
    item = DLFOLLOW(item)->next
    n -= 1
  wend
  while n < 0 andalso item
    item = DLFOLLOW(item)->prev
    n += 1
  wend
  return item
END FUNCTION

/'
SUB dlist_print (byref this as DoubleList(Any))
  dim ptt as any ptr = this.first
  debug "numitems=" & this.numitems & " first=" & hex(ptt) & " last=" & hex(this.last) & " items:"
  while ptt
    debug " 0x" & hex(ptt) & " n:0x" & hex(DLFOLLOW(ptt)->next) & " p:0x" & hex(DLFOLLOW(ptt)->prev) '& " " & get_menu_item_caption(*ptt, menudata)
    ptt = DLFOLLOW(ptt)->next
  wend
END SUB
'/

'------------- Hash Table -------------

#define HTCASTUSERPTR(someptr)  cast(any ptr, cast(byte ptr, someptr) - this.memberoffset)
#define HTCASTITEMPTR(someptr)  cast(HashedItem ptr, cast(byte ptr, someptr) + this.memberoffset)

SUB hash_construct(byref this as HashTable, byval itemoffset as integer, byval tablesize as integer = 256)
  this.numitems = 0
  this.tablesize = tablesize
  this.table = callocate(sizeof(any ptr) * this.tablesize)
  this.comparefunc = NULL
  this.memberoffset = itemoffset
END SUB

SUB hash_destruct(byref this as HashTable)
  deallocate(this.table)
  this.table = NULL
END SUB

SUB hash_add(byref this as HashTable, byval item as any ptr)
  dim bucket as HashedItem ptr ptr
  dim it as HashedItem ptr = HTCASTITEMPTR(item)
  
  bucket = @this.table[it->hash mod this.tablesize]
  it->_prevp = bucket
  it->_next = *bucket
  if *bucket then
    it->_next->_prevp = @it->_next
  end if
  *bucket = it

  this.numitems += 1
END SUB

SUB hash_remove(byref this as HashTable, byval item as any ptr)
  IF item = NULL THEN EXIT SUB

  dim it as HashedItem ptr = HTCASTITEMPTR(item)

  *(it->_prevp) = it->_next
  IF it->_next THEN
    it->_next->_prevp = it->_prevp
  END IF
  it->_next = NULL
  it->_prevp = NULL
  this.numitems -= 1
END SUB

FUNCTION hash_find(byref this as HashTable, byval hash as integer, byval key as any ptr = NULL) as any ptr
  dim bucket as HashedItem ptr ptr
  dim it as HashedItem ptr
  
  it = this.table[hash mod this.tablesize]
  while it
    if it->hash = hash then
      dim ret as any ptr = HTCASTUSERPTR(it)
      if key andalso this.comparefunc then
        if this.comparefunc(ret, key) then
          return ret
        end if
      else
        return ret
      end if
    end if
    it = it->_next
  wend
  return NULL
END FUNCTION

FUNCTION hash_iter(byref this as HashTable, byref state as integer, byref item as any ptr) as any ptr
  dim it as HashedItem ptr = NULL
  if item then
    it = HTCASTITEMPTR(item)->_next
  end if

  while it = NULL
    if state >= this.tablesize then return NULL
    it = this.table[state]
    state += 1
  wend
 
  item = HTCASTUSERPTR(it)
  return item
END FUNCTION
