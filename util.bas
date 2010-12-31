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
#include "cutil.bi"

#if __FB_LANG__ <> "fb"
OPTION EXPLICIT
#endif

#if defined(IS_GAME) OR defined(IS_CUSTOM)
 DECLARE SUB debug (s as string)
 DECLARE SUB debuginfo (s as string)
 DECLARE SUB fatalerror (s as string)
#else
 PRIVATE SUB debug (s as string)
  PRINT s
 END SUB
 PRIVATE SUB debuginfo (s as string)
  PRINT s
 END SUB
 PRIVATE SUB fatalerror (s as string)
  PRINT s
  SYSTEM
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

FUNCTION bound (BYVAL n as longint, BYVAL lowest as longint, BYVAL highest as longint) as longint
 bound = n
 IF n < lowest THEN bound = lowest
 IF n > highest THEN bound = highest
END FUNCTION

FUNCTION bound (BYVAL n AS DOUBLE, BYVAL lowest AS DOUBLE, BYVAL highest AS DOUBLE) AS DOUBLE
 bound = n
 IF n < lowest THEN bound = lowest
 IF n > highest THEN bound = highest
END FUNCTION

FUNCTION in_bound (BYVAL n as integer, BYVAL lowest as integer, BYVAL highest as integer) as integer
 RETURN (n >= lowest) AND (n <= highest)
END FUNCTION

FUNCTION large (BYVAL n1 as integer, BYVAL n2 as integer) as integer
 large = n1
 IF n2 > n1 THEN large = n2
END FUNCTION

FUNCTION large (BYVAL n1 as longint, BYVAL n2 as longint) as longint
 large = n1
 IF n2 > n1 THEN large = n2
END FUNCTION

FUNCTION large (BYVAL n1 as double, BYVAL n2 as double) as double
 IF n2 > n1 THEN RETURN n2 ELSE RETURN n1
END FUNCTION

FUNCTION loopvar (BYVAL value as integer, BYVAL min as integer, BYVAL max as integer, BYVAL inc as integer) as integer
 DIM as integer a = value + inc
 IF a > max THEN RETURN a - ((max - min) + 1)
 IF a < min THEN RETURN a + ((max - min) + 1)
 RETURN a
END FUNCTION

FUNCTION loopvar (BYVAL value as longint, BYVAL min as longint, BYVAL max as longint, BYVAL inc as longint) as longint
 DIM as longint a = value + inc
 IF a > max THEN RETURN a - ((max - min) + 1)
 IF a < min THEN RETURN a + ((max - min) + 1)
 RETURN a
END FUNCTION

FUNCTION small (BYVAL n1 as integer, BYVAL n2 as integer) as integer
 small = n1
 IF n2 < n1 THEN small = n2
END FUNCTION

FUNCTION small (BYVAL n1 as longint, BYVAL n2 as longint) as longint
 small = n1
 IF n2 < n1 THEN small = n2
END FUNCTION

FUNCTION small (BYVAL n1 as double, BYVAL n2 as double) as double
 IF n2 < n1 THEN RETURN n2 ELSE RETURN n1
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
  c = MID(s, i, 1)
  IF INSTR(chars, c) THEN
   result = result & "\"
  END IF
  result = result & c
 NEXT i
 RETURN result
END FUNCTION

'Replace occurrences of a substring. Modifies 'buffer'!
'Returns the number of replacements done. Inserted text is not eligible for further replacements.
'Optionally limit the number of times to do with replacement by passing maxtimes; no limit if < 0
FUNCTION replacestr (buffer as string, replacewhat as string, withwhat as string, byval maxtimes as integer = -1) as integer
 DIM pt as integer
 DIM count as integer
 DIM start as integer = 1

 WHILE maxtimes < 0 OR count < maxtimes
  pt = INSTR(start, buffer, replacewhat)
  IF pt = 0 THEN RETURN count
  buffer = MID(buffer, 1, pt - 1) + withwhat + MID(buffer, pt + LEN(replacewhat))
  start = pt + LEN(withwhat)
  count += 1
 WEND
 RETURN count
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

'------------- Stack -------------

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
      'debug "nowscript = " & nowscript & " " & scrat(nowscript).id & " " & scriptname(scrat(nowscript).id) 

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

'------------- Old allmodex stack  -------------

dim shared stackbottom as ubyte ptr
dim shared stackptr as ubyte ptr
dim shared stacksize as integer = -1

SUB setupstack ()
	stackbottom = callocate(32768)
	if (stackbottom = 0) then
		'oh dear
		debug "Not enough memory for stack"
		exit sub
	end if
	stackptr = stackbottom
	stacksize = 32768
end SUB

SUB pushw (BYVAL word as integer)
	if stackptr - stackbottom > stacksize - 2 then
		dim newptr as ubyte ptr
		newptr = reallocate(stackbottom, stacksize + 32768)
		if newptr = 0 then
			debug "stack: out of memory"
			exit sub
		end if
		stacksize += 32768
		stackptr += newptr - stackbottom
		stackbottom = newptr
	end if
	*cast(short ptr, stackptr) = word
	stackptr += 2
end SUB

FUNCTION popw () as integer
	dim pw as short

	if (stackptr >= stackbottom + 2) then
		stackptr -= 2
		pw = *cast(short ptr, stackptr)
	else
		pw = 0
		debug "underflow"
	end if

	popw = pw
end FUNCTION

SUB pushdw (BYVAL dword as integer)
	if stackptr - stackbottom > stacksize - 4 then
		dim newptr as ubyte ptr
		newptr = reallocate(stackbottom, stacksize + 32768)
		if newptr = 0 then
			debug "stack: out of memory"
			exit sub
		end if
		stacksize += 32768
		stackptr += newptr - stackbottom
		stackbottom = newptr
	end if
	*cast(integer ptr, stackptr) = dword
	stackptr += 4
end SUB

FUNCTION popdw () as integer
	dim pdw as integer

	if (stackptr >= stackbottom - 4) then
		stackptr -= 4
		pdw = *cast(integer ptr, stackptr)
	else
		pdw = 0
		debug "underflow"
	end if

	popdw = pdw
end FUNCTION

SUB releasestack ()
	if stacksize > 0 then
		deallocate stackbottom
		stacksize = -1
	end if
end SUB

FUNCTION stackpos () as integer
	stackpos = stackptr - stackbottom
end FUNCTION

'read an int from the stack relative to current position (eg -1 is last word pushed - off should be negative)
FUNCTION readstackdw (BYVAL off as integer) as integer
	if stackptr + off * 4 >= stackbottom then
		readstackdw = *cptr(integer ptr, stackptr + off * 4)
	end if
END FUNCTION

'------------- End allmodex stack -------------

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

FUNCTION str_array_findcasei (array() AS STRING, value AS STRING) AS INTEGER
 DIM valuei AS STRING = LCASE(value)
 FOR i AS INTEGER = LBOUND(array) TO UBOUND(array)
  IF LCASE(array(i)) = value THEN RETURN i
 NEXT
 RETURN -1
END FUNCTION

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
  'Return the file/directory name without path, and without trailing slash
  'Eg. "a/b/c/" -> "c"
  DIM i as integer
  DIM retend as integer = LEN(filename)
  DIM ch as byte
  IF retend > 0 THEN
    ch = filename[retend - 1]
    IF ch = asc("/") OR ch = asc("\") THEN retend -= 1
  END IF
  FOR i = retend TO 1 STEP -1
    IF filename[i - 1] = asc("\") OR filename[i - 1] = asc("/") THEN
      RETURN MID(filename, i + 1, retend - (i + 1) + 1)
    END IF
  NEXT
  RETURN MID(filename, 1, retend)
END FUNCTION

FUNCTION trimfilename (filename as string) as string
  'Return the path without the filename, and without trailing slash
  'NOT the complement to trimpath:
  'Eg. "a/b/c/" -> "a/b/c"
  DIM i as integer
  DIM ret as string = filename
  FOR i = 0 TO LEN(ret) - 1 
    IF (ret[i] = asc("\")) OR (ret[i] = asc("/")) THEN ret[i] = asc(SLASH)
  NEXT
  RETURN MID(ret, 1, large(0, INSTRREV(ret, SLASH) - 1))
END FUNCTION

FUNCTION trimextension (filename as string) as string
  'Return the filename (including path) without extension
  'Periods at the beginning of file/folder names are not counted as beginning an extension
  DIM at as integer = INSTRREV(filename, ".")
  DIM at2 as integer = large(INSTRREV(filename, "\"), INSTRREV(filename, "/"))
  IF at >= at2 + 2 THEN
    RETURN MID(filename, 1, at - 1)
  ELSE
    RETURN filename
  END IF
END FUNCTION

FUNCTION justextension (filename as string) as string
  'Return only the extension (everything after the *last* period)
  'Periods at the beginning of file/folder names are not counted as beginning an extension
  DIM at as integer = INSTRREV(filename, ".")
  DIM at2 as integer = large(INSTRREV(filename, "\"), INSTRREV(filename, "/"))
  IF at >= at2 + 2 THEN
    RETURN MID(filename, at + 1)
  ELSE
    RETURN ""
  END IF
END FUNCTION

FUNCTION is_absolute_path (sDir as string) as integer
#IFDEF __UNIX__
  IF left(sDir, 1) = "/" THEN RETURN -1
#ELSE
  DIM first as string = LCASE(LEFT(sDir, 1))
  IF first = "\" THEN RETURN -1
  IF first >= "a" andalso first <= "z" andalso MID(sDir, 2, 2) = ":\" THEN RETURN -1
#ENDIF
  RETURN 0
END FUNCTION

'Make a path absolute. See also with_orig_dir in compat.bas
FUNCTION absolute_path(pathname as string) as string
  IF NOT is_absolute_path(pathname) THEN RETURN CURDIR & SLASH & pathname
  RETURN pathname
END FUNCTION

'Go up a number of directories.
'pathname is interpreted as a directory even if missing the final slash!
'Warning, don't actually rely on . and .. being properly handled
FUNCTION parentdir (pathname as string, BYVAL upamount as integer = 1) as string
  DIM as integer temp, retlen = LEN(pathname)
  WHILE upamount > 0
    WHILE retlen > 0 ANDALSO pathname[retlen - 1] = ASC(SLASH) : retlen -= 1 : WEND
    temp = INSTRREV(pathname, SLASH, retlen)
    IF temp = 0 THEN EXIT WHILE
    retlen = temp
    IF MID(pathname, retlen + 1, 3) = ".." + SLASH THEN
      upamount += 1
    ELSEIF MID(pathname, retlen + 1, 2) = "." + SLASH THEN
    ELSE
      upamount -= 1
    END IF
  WEND
  RETURN MID(pathname, 1, retlen)
END FUNCTION

FUNCTION anycase (filename as string) as string
  'create a case-insensitive regex from a filename
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

'Finds files in a directory, writing them into an array without their path
'filelist() must be resizeable; it'll be resized so that LBOUND = -1, with files, if any, in filelist(0) up
'By default, find all files in directory, otherwise namemask is a case-insensitive filename mask
'filetype is one of fileTypeFile, fileTypeDirectory
SUB findfiles (directory AS STRING, namemask AS STRING = "", BYVAL filetype AS INTEGER = fileTypeFile, BYVAL findhidden AS INTEGER = 0, filelist() AS STRING)
  DIM AS STRING searchdir = directory
  IF RIGHT(searchdir, 1) <> SLASH THEN searchdir += SLASH
  DIM AS STRING nmask = anycase(namemask)
  IF LEN(nmask) = 0 THEN nmask = ALLFILES
  REDIM filelist(-1 TO -1)

#ifdef __UNIX__
  'this is super hacky, but works around the apparent uselessness of DIR
  DIM AS STRING grep, shellout
  shellout = "/tmp/ohrrpgce-findfiles-" + STR(RND * 10000) + ".tmp"
  grep = "-v '/$'"
  IF filetype = fileTypeDirectory THEN grep = "'/$'"
  searchdir = """" + escape_string(searchdir, """`\$") + """"
  IF findhidden THEN
    searchdir = searchdir + nmask + " " + searchdir + "." + nmask
  ELSE
    searchdir = searchdir + nmask
  END IF

  SHELL "ls -d1p " + searchdir + " 2>/dev/null |grep "+ grep + ">" + shellout + " 2>&1"
  DIM AS INTEGER f1
  f1 = FreeFile
  OPEN shellout FOR INPUT AS #f1
  DIM filename AS STRING
  DO UNTIL EOF(f1)
    LINE INPUT #f1, filename
    IF RIGHT(filename, 3) = "/./" ORELSE RIGHT(filename, 4) = "/../" _
         ORELSE filename = "/dev/" ORELSE filename = "/proc/" ORELSE filename = "/sys/" THEN CONTINUE DO
    str_array_append filelist(), trimpath(filename)
  LOOP
  CLOSE #f1
  KILL shellout

#else
  DIM foundfile AS STRING
  DIM attrib AS INTEGER
  /'---DOS directory attributes
  CONST attribReadOnly = 1
  CONST attribHidden = 2
  CONST attribSystem = 4
  CONST attribDirectory = 16
  CONST attribArchive = 32
  CONST attribReserved = 192 '64 OR 128
  CONST attribAlmostAll = 237 ' All except directory and hidden
  '/
  IF filetype = fileTypeDirectory THEN
    attrib = 53
  ELSE
    attrib = (253 XOR 16)
  END IF
  IF findhidden THEN attrib += 2
  foundfile = DIR(searchdir + nmask, attrib)
  IF foundfile = "" THEN EXIT SUB
  REDIM tempfilelist(-1 TO -1) AS STRING
  DO UNTIL foundfile = ""
    str_array_append tempfilelist(), foundfile
    foundfile = DIR '("", attrib)
  LOOP
  FOR i AS INTEGER = 0 TO UBOUND(tempfilelist)
    foundfile = tempfilelist(i)
    IF filetype = fileTypeDirectory THEN
      'alright, we want directories, but DIR is too broken to give them to us
      'files with attribute 0 appear in the list, so single those out
      IF DIR(searchdir + foundfile, 55) = "" OR DIR(searchdir + foundfile, 39) <> "" THEN CONTINUE FOR
    END IF
    str_array_append filelist(), foundfile
  NEXT
#endif
END SUB

SUB killdir(directory as string)
  DIM filelist() as string
  findfiles directory, ALLFILES, fileTypeFile, -1, filelist()
  FOR i as integer = 0 TO UBOUND(filelist)
    KILL directory + SLASH + filelist(i)
  NEXT
  IF RMDIR(directory) THEN
    'errno would get overwritten while building the error message
    DIM err_string AS STRING = *get_sys_err_string()
    debug "Could not rmdir(" & directory & "): " & err_string
  END IF
'  IF isdir(directory) THEN
'    debug "Failed to delete directory " & directory
'  END IF
END SUB

'Returns zero on success
FUNCTION makedir (directory as string) as integer
  IF isdir(directory) THEN
    debuginfo "makedir: " & directory & " already exists"
    RETURN 0
  END IF
  IF MKDIR(directory) THEN
    'errno would get overwritten while building the error message
    DIM err_string AS STRING = *get_sys_err_string()
    'The heck? On Windows at least, MKDIR throws this false error
    IF err_string <> "File exists" THEN
     debug "Could not mkdir(" & directory & "): " & err_string
     RETURN 1
    END IF
  END IF
#ifdef __FB_LINUX__
  ' work around broken file permissions in dirs created by linux version
  ' MKDIR creates with mode 644, should create with mode 755
  SHELL "chmod +x """ + directory + """"
#endif
  RETURN 0
END FUNCTION

SUB safekill (filename as string)
  IF isfile(filename) THEN
   'KILL is a thin wrapper around C's remove(), however by calling it directly we can get a textual error message
   IF remove(strptr(filename)) THEN
    DIM err_string AS STRING = *get_sys_err_string()
    debug "Could not remove(" & filename & "): " & err_string

    'NOTE: on Windows, even if deletion fails because the file is open, the file will be marked
    'to be deleted once everyone closes it. Also, it will no longer be possible to open it.
    'On Unix, you can unlink a file even when someone else has it open.
   END IF
  END IF
END SUB

FUNCTION fileisreadable(filename as string) as integer
  dim fh as integer, err_code as integer
  fh = freefile
  err_code = open(filename for binary access read as #fh)
  if err_code = 2 then
    ''debug f & " unreadable (ignored)"
    return 0
  elseif err_code <> 0 then
    'debug "Error " & err_code & " reading " & filename
    return 0
  end if
  close #fh
  return -1
END FUNCTION

FUNCTION fileiswriteable(filename as string) as integer
  dim fh as integer
  fh = freefile
  if open (filename for binary access read write as #fh) = 2 then
    ''debug filename & " unreadable (ignored)"
    return 0 
  end if
  close #fh
  return -1
END FUNCTION

FUNCTION diriswriteable(d as string) as integer
  dim testfile as string = d & SLASH & "__testwrite_" & INT(RND * 100000) & ".tmp"
  if fileiswriteable(testfile) then
    safekill testfile
    return -1
  end if
  return 0
END FUNCTION

FUNCTION isfile (filename as string) as integer
  ' directories don't count as files
  ' this is a simple wrapper for fileisreadable
  if filename = "" then return 0
  return fileisreadable(filename)
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
  this.numitems = 0
  this.tablesize = 0
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


'------------- Old allmodex stuff -------------

SUB array2str (arr() as integer, byval o as integer, dest as string)
'String dest is already filled out with spaces to the requisite size
'o is the offset in bytes from the start of the buffer
'the buffer will be packed 2 bytes to an int, for compatibility, even
'though FB ints are 4 bytes long  ** leave like this? not really wise
	dim i as integer
	dim bi as integer
	dim bp as integer ptr
	dim toggle as integer

	bp = @arr(0)
	bi = o \ 2 'offset is in bytes
	toggle = o mod 2

	for i = 0 to len(dest) - 1
		if toggle = 0 then
			dest[i] = bp[bi] and &hff
			toggle = 1
		else
			dest[i] = (bp[bi] and &hff00) shr 8
			toggle = 0
			bi = bi + 1
		end if
	next

END SUB

SUB str2array (src as string, arr() as integer, byval o as integer)
'strangely enough, this does the opposite of the above
	dim i as integer
	dim bi as integer
	dim bp as integer ptr
	dim toggle as integer

	bp = @arr(0)
	bi = o \ 2 'offset is in bytes
	toggle = o mod 2

	'debug "String is " + str(len(src)) + " chars"
	for i = 0 to len(src) - 1
		if toggle = 0 then
			bp[bi] = src[i] and &hff
			toggle = 1
		else
			bp[bi] = (bp[bi] and &hff) or (src[i] shl 8)
			'check sign
			if (bp[bi] and &h8000) > 0 then
				bp[bi] = bp[bi] or &hffff0000 'make -ve
			end if
			toggle = 0
			bi = bi + 1
		end if
	next
end SUB

SUB xbload (filename as string, array() as integer, errmsg as string)
	IF isfile(filename) THEN
		dim ff as integer, byt as ubyte, seg as short, offset as short, length as short
		dim ilength as integer
		dim i as integer
		
		ff = FreeFile
		OPEN filename FOR BINARY AS #ff
		GET #ff,, byt 'Magic number, always 253
		IF byt <> 253 THEN fatalerror errmsg
		GET #ff,, seg 'Segment, no use anymore
		GET #ff,, offset 'Offset into the array, not used now
		GET #ff,, length 'Length
		'length is in bytes, so divide by 2, and subtract 1 because 0-based
		ilength = (length / 2) - 1

		dim buf(ilength) as short

		GET #ff,, buf()
		CLOSE #ff

		for i = 0 to small(ilength, ubound(array))
			array(i) = buf(i)
		next i

	ELSE
		fatalerror errmsg
	END IF
END SUB

SUB xbsave (filename as string, array() as integer, bsize as integer)
	dim ff as integer, byt as UByte, seg AS uShort, offset AS Short, length AS Short
	dim ilength as integer
	dim i as integer
	dim needbyte as integer
	
	seg = &h9999
	offset = 0
	'Because we're working with a short array, but the data is in bytes
	'we need to check if there's an odd size, and therefore a spare byte
	'we'll need to add at the end.
	ilength = (bsize \ 2) - 1	'will lose an odd byte in the division
	needbyte = bsize mod 2		'write an extra byte at the end?
	length = bsize	'bsize is in bytes
	byt = 253

	'copy array to shorts
	DIM buf(ilength) as short
	for i = 0 to small(ilength, ubound(array))
		buf(i) = array(i)
	next

	ff = FreeFile
	OPEN filename FOR BINARY ACCESS write AS #ff
	PUT #ff, , byt				'Magic number
	PUT #ff, , seg				'segment - obsolete
	PUT #ff, , offset			'offset - obsolete
	PUT #ff, , length			'size in bytes

	PUT #ff,, buf()
	if needbyte = 1 then
		i = small(ilength + 1, ubound(array)) 'don't overflow
		byt = array(i) and &hff
		put #ff, , byt
	end if
	CLOSE #ff
END SUB

SUB setbit (bb() as integer, BYVAL w as integer, BYVAL b as integer, BYVAL v as integer)
	dim mask as uinteger
	dim woff as integer
	dim wb as integer

	woff = w + (b \ 16)
	wb = b mod 16

	if woff > ubound(bb) then
		debug "setbit overflow: ub " & ubound(bb) & ", w " & w & ", b " & b & ", v " & v
		exit sub
	end if

	mask = 1 shl wb
	if v then
		bb(woff) = bb(woff) or mask
	else
		mask = not mask
		bb(woff) = bb(woff) and mask
	end if
end SUB

FUNCTION readbit (bb() as integer, BYVAL w as integer, BYVAL b as integer)  as integer
	dim mask as uinteger
	dim woff as integer
	dim wb as integer

	woff = w + (b \ 16)
	if woff > ubound(bb) then
		debug "readbit overflow: ub " & ubound(bb) & ", w " & w & ", b " & b
		return 0
	end if
	wb = b mod 16

	mask = 1 shl wb

	if (bb(woff) and mask) then
		readbit = 1
	else
		readbit = 0
	end if
end FUNCTION
