'OHRRPGCE - Some utility code
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)
'
' This file contains utility subs and functions which would be useful for
' any FreeBasic program. Nothing in here can depend on Allmodex, nor on any
' gfx or music backend, nor on any other part of the OHR

CONST STACK_SIZE_INC = 512 ' in integers

#include "file.bi"   'FB header
#include "datetime.bi" 'FB header
#include "config.bi"
#include "util.bi"
#include "cutil.bi"
#include "os.bi"
#include "common_base.bi"
#ifdef __FB_MAIN__
#include "testing.bi"
#endif

'#ifdef __FB_ANDROID__
'#define DEBUG_FILE_IO
'#endif


Type FBSTRING as string
'Resize a FB string
Declare Function fb_hStrRealloc Alias "fb_hStrRealloc" (byval s as FBSTRING ptr, byval size as integer, byval preserve as integer) as FBSTRING ptr
'Resize a FB string or allocate a new one, and mark it temporary (equals SPEED)
Declare Function fb_hStrAllocTemp Alias "fb_hStrAllocTemp" (byval s as FBSTRING ptr, byval size as integer) as FBSTRING ptr



'It is very important for this to be populated _before_ any calls to CHDIR
DIM orig_dir as string

DIM tmpdir as string

'This is needed for mbstowcs. Placing it here seems like the simplest way to ensure it's run in all utilities
init_runtime


'------------- Math operations -------------

FUNCTION bitcount (byval v as unsigned integer) as integer
  'From the "Software Optimization Guide for AMD Athlon 64 and Opteron Processors". Thanks, AMD!
  v = v - ((v SHR 1) AND &h55555555)
  v = (v AND &h33333333) + ((v SHR 2) AND &h33333333)
  RETURN ((v + (v SHR 4) AND &hF0F0F0F) * &h1010101) SHR 24
END FUNCTION

FUNCTION ceiling (byval n as integer) as integer
 RETURN INT(n * -1) * -1
END FUNCTION

FUNCTION bound (byval n as integer, byval lowest as integer, byval highest as integer) as integer
 bound = n
 IF n < lowest THEN bound = lowest
 IF n > highest THEN bound = highest
END FUNCTION

FUNCTION bound (byval n as longint, byval lowest as longint, byval highest as longint) as longint
 bound = n
 IF n < lowest THEN bound = lowest
 IF n > highest THEN bound = highest
END FUNCTION

FUNCTION bound (byval n as double, byval lowest as double, byval highest as double) as double
 bound = n
 IF n < lowest THEN bound = lowest
 IF n > highest THEN bound = highest
END FUNCTION

FUNCTION in_bound (byval n as integer, byval lowest as integer, byval highest as integer) as integer
 RETURN (n >= lowest) AND (n <= highest)
END FUNCTION

FUNCTION large (byval n1 as integer, byval n2 as integer) as integer
 large = n1
 IF n2 > n1 THEN large = n2
END FUNCTION

FUNCTION large (byval n1 as longint, byval n2 as longint) as longint
 large = n1
 IF n2 > n1 THEN large = n2
END FUNCTION

FUNCTION large (byval n1 as double, byval n2 as double) as double
 IF n2 > n1 THEN RETURN n2 ELSE RETURN n1
END FUNCTION

FUNCTION loopvar (byval value as integer, byval min as integer, byval max as integer, byval inc as integer = 1) as integer
 RETURN POSMOD((value + inc) - min, (max - min) + 1) + min
END FUNCTION

FUNCTION loopvar (byval value as longint, byval min as longint, byval max as longint, byval inc as longint = 1) as longint
 RETURN POSMOD((value + inc) - min, (max - min) + 1) + min
END FUNCTION

FUNCTION small (byval n1 as integer, byval n2 as integer) as integer
 small = n1
 IF n2 < n1 THEN small = n2
END FUNCTION

FUNCTION small (byval n1 as longint, byval n2 as longint) as longint
 small = n1
 IF n2 < n1 THEN small = n2
END FUNCTION

FUNCTION small (byval n1 as double, byval n2 as double) as double
 IF n2 < n1 THEN RETURN n2 ELSE RETURN n1
END FUNCTION

'Find dimensions of a rect, given opposite corners 
SUB corners_to_rect (p1 as XYPair, p2 as XYPair, result as RectType)
 IF p1.x < p2.x THEN result.x = p1.x ELSE result.x = p2.x
 result.wide = ABS(p1.x - p2.x)
 IF p1.y < p2.y THEN result.y = p1.y ELSE result.y = p2.y
 result.high = ABS(p1.y - p2.y)
END SUB

'As above, but include the corner positions inside the rect (for use when coordinates measure on a discrete grid)
SUB corners_to_rect_inclusive (p1 as XYPair, p2 as XYPair, result as RectType)
 corners_to_rect p1, p2, result
 result.wide += 1
 result.high += 1
END SUB

FUNCTION rect_collide_point (r as RectType, p as XYPair) as bool
 RETURN p.x >= r.x ANDALSO p.y >= r.y ANDALSO p.x < r.x + r.wide ANDALSO p.y < r.y + r.high
END FUNCTION

FUNCTION rect_collide_point (r as RectType, byval x as integer, byval y as integer) as bool
 RETURN x >= r.x ANDALSO y >= r.y ANDALSO x < r.x + r.wide ANDALSO y < r.y + r.high
END FUNCTION

FUNCTION rect_collide_point_vertical_chunk (r as RectType, p as XYPair, chunk_spacing as integer) as integer
 'Divide a rect into vertical chunks (like a menu) and return the
 'index of the one the point collides with. Returns -1 if none collide
 IF rect_collide_point(r, p) THEN
  RETURN int((p.y - r.y) / chunk_spacing)
 END IF
 RETURN -1
END FUNCTION

FUNCTION rect_collide_point_vertical_chunk (r as RectType, byval x as integer, byval y as integer, chunk_spacing as integer) as integer
 'Divide a rect into vertical chunks (like a menu) and return the
 'index of the one the point collides with. Returns -1 if none collide
 DIM p as XYPair
 p.x = x
 p.y = y
 RETURN rect_collide_point_vertical_chunk(r, p, chunk_spacing)
END FUNCTION

FUNCTION rando () as double
 'STATIC count as integer = 0
 'This is a simple wrapper for RND to facilitate debugging
 DIM n as double = RND
 'debug count & " RND=" & n
 'count += 1
 RETURN n
END FUNCTION

FUNCTION randint (byval limit as integer) as integer
 'Returns a random integer >=0 and < limit
 RETURN INT(rando() * limit)
END FUNCTION

'Simple low quality psuedo random number generator with exposed state. Same speed as RND.
'Initialise prng_state to a seed before first call.
'Returns a float in the range [0.0, 1.0) but no where near a full double of precision.
FUNCTION simple_rand (byref prng_state as uinteger) as double
 prng_state = (prng_state * 1103515245 + 12345)
 RETURN CAST(double, prng_state) * (1.0 / &hffffffffU)
END FUNCTION

'Simple low quality psuedo random number generator. Initialise prng_state to a seed before first call.
'Returns an integer in the range 0 to limit - 1. limit should be <= 2^20
FUNCTION simple_randint (byref prng_state as uinteger, byval limit as integer) as uinteger
 prng_state = (prng_state * 1103515245 + 12345)
 RETURN CINT((CAST(longint, prng_state) * limit) SHR 32)
END FUNCTION

'simple_rand simple test: create a bmp
/'
 DIM timestart as double = TIMER
 DIM tframe as frame ptr = frame_new(256, 256)
 DIM pstate as unsigned integer = 0
 FOR yy as integer = 0 to 255
  FOR xx as integer = 0 to 255
   putpixel tframe, xx, yy, simple_randint(pstate, 16) * 15
   'putpixel tframe, xx, yy, CINT(simple_rand(pstate) * 16) * 15
  NEXT
 NEXT
 frame_export_bmp8 "randtest.bmp", tframe, master()
 debug "testframe in " & (TIMER - timestart) * 1000 & "ms"
 frame_unload @tframe
'/

FUNCTION range (number as integer, percent as integer) as integer
 DIM a as integer
 a = (number / 100) * percent
 RETURN number + randint((a * 2)) - a
END FUNCTION

FUNCTION isnan (byval value as double) as integer
 RETURN value <> value
END FUNCTION

FUNCTION isnan (byval value as single) as integer
 RETURN value <> value
END FUNCTION

FUNCTION isfinite (byval value as double) as integer
 RETURN DBL_MAX >= value AND value >= -DBL_MAX
END FUNCTION

FUNCTION isfinite (byval value as single) as integer
 RETURN FLT_MAX >= value AND value >= -FLT_MAX
END FUNCTION

'A fuzzy equivalent to 'iif(value >= low+high/2, 1.0, 0.0)'
'Swap low,high to reverse the comparison
FUNCTION fuzzythreshold (byval value as double, byval low as double, byval high as double) as double
 IF low > high THEN
  low = -low
  high = -high
  value = -value
 END IF
 IF value <= low THEN
  RETURN 0.0
 ELSEIF value >= high THEN
  RETURN 1.0
 ELSE
  RETURN (value - low) / (high - low)
 END IF
END FUNCTION


'---------------- String operations --------------

'FB will usually produce a NULL ptr when converting an empty string to a zstring ptr,
'which is not acceptable in C
FUNCTION cstring (s as string) as zstring ptr
 DIM ret as zstring ptr = strptr(s)
 IF ret = NULL THEN RETURN strptr("")
 RETURN ret
END FUNCTION

'FB's string assignment will always do a strlen on zstring arguments, (even if you call fb_StrAssign
'manually with the right length!) so if the source is a binary blob containing null bytes then
'this function should be used instead.
FUNCTION blob_to_string (byval str_ptr as zstring ptr, byval str_len as integer) as string
 DIM ret as string
 'Consider this a testcase for "Can you trust TMC to implement a FreeBASIC compiler?"
 'OK, OK, you could just do ret = SPACE(str_len), but this is faster
 fb_hStrAllocTemp(@ret, str_len)
 memcpy(@ret[0], str_ptr, str_len)
 ret[str_len] = 0
 return ret
END FUNCTION

FUNCTION rpad (s as string, pad_char as string, size as integer) as string
 DIM temp as string
 temp = LEFT(s, size)
 RETURN temp & STRING(size - LEN(temp), pad_char)
END FUNCTION

'Like INSTR, but return the n-th match
'Returns 0 if not found
FUNCTION Instr_nth (byval start as integer, s as string, substring as string, byval nth as integer = 1) as integer
 DIM temp as integer = start - 1
 IF nth < 1 THEN RETURN 0
 FOR n as integer = 1 TO nth
  temp = INSTR(temp + 1, s, substring)
  IF temp = 0 THEN RETURN 0
 NEXT
 RETURN temp
END FUNCTION

'Like INSTR without start point, but return the n-th match
'Returns 0 if not found
FUNCTION Instr_nth (s as string, substring as string, byval nth as integer = 1) as integer
 RETURN Instr_nth(1, s, substring, nth)
END FUNCTION

'Returns the number of characters at the start of two strings that are equal
FUNCTION length_matching(s1 as string, s2 as string) as integer
 DIM as ubyte ptr p1 = @s1[0], p2 = @s2[0]
 DIM as integer ret = 0
 WHILE *p1 AND *p2
  IF *p1 <> *p2 THEN RETURN ret
  p1 += 1
  p2 += 1
  ret += 1
 WEND
 RETURN ret
END FUNCTION

FUNCTION is_int (s as string) as integer
 'Even stricter than str2int (doesn't accept "00")
 DIM n as integer = VALINT(s)
 RETURN (n <> 0 ANDALSO n <> VALINT(s + "1")) ORELSE s = "0"
END FUNCTION

FUNCTION str2int (stri as string, default as integer=0) as integer
 'Use this in contrast to QuickBasic's VALINT.
 'it is stricter, and returns a default on failure
 DIM n as integer = 0
 DIM s as string = LTRIM(stri)
 IF s = "" THEN RETURN default
 DIM sign as integer = 1

 DIM ch as string
 DIM c as integer
 FOR i as integer = 1 TO LEN(s)
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

FUNCTION escape_string(s as string, chars as string) as string
 DIM i as integer
 DIM c as string
 DIM result as string
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
 DIM ret as string = ""
 FOR i as integer = 1 TO LEN(s)
  DIM tmp as string = MID(s, i, 1)
  IF INSTR(x, tmp) = 0 THEN
   ret &= tmp
  END IF
 NEXT i
 RETURN ret
END FUNCTION

FUNCTION exclusive (s as string, x as string) as string
 DIM ret as string = ""
 FOR i as integer = 1 TO LEN(s)
  DIM tmp as string = MID(s, i, 1)
  IF INSTR(x, tmp) THEN
   ret &= tmp
  END IF
 NEXT i
 RETURN ret
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

SUB setstackposition (st as Stack, byval position as integer)
  IF position < 0 OR position > stackposition(st) THEN
    fatalerror "setstackposition invalid, " & position
  END IF
  st.pos = st.bottom + position
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

SUB pushw (byval word as integer)
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

SUB pushdw (byval dword as integer)
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
FUNCTION readstackdw (byval off as integer) as integer
	if stackptr + off * 4 >= stackbottom then
		readstackdw = *cptr(integer ptr, stackptr + off * 4)
	end if
END FUNCTION

'------------- End allmodex stack -------------

FUNCTION sign_string(n as integer, neg_str as string, zero_str as string, pos_str as string) as string
 IF n < 0 THEN RETURN neg_str
 IF n > 0 THEN RETURN pos_str
 RETURN zero_str
END FUNCTION

FUNCTION zero_default(n as integer, zerocaption as string="default", displayoffset as integer = 0) as string
 IF n = 0 THEN RETURN zerocaption
 RETURN "" & (n + displayoffset)
END FUNCTION

'FIXME: Can be replaced by IIF in most recent FB version, once we switch to it
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

'Splits text at the separators; use together with wordwrap() to do wrapping
'sep must be length 1. ret() must be resizeable. If in == "", then ret() is redimmed to length 1.
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

FUNCTION days_since_datestr (datestr as string) as integer
 'Returns the number of days since a date given as a string in the format YYYY-MM-DD
 IF LEN(datestr) <> 10 THEN
  debug "days_since_datestr: bad datestr " & datestr
  RETURN 0
 END IF
 DIM y as integer = str2int(MID(datestr, 1, 4))
 DIM m as integer = str2int(MID(datestr, 6, 2))
 DIM d as integer = str2int(MID(datestr, 9, 2))
 RETURN NOW - DateSerial(y, m, d)
END FUNCTION

SUB flusharray (array() as integer, byval size as integer=-1, byval value as integer=0)
 'If size is -1, then flush the entire array
 IF size = -1 THEN size = UBOUND(array)
 FOR i as integer = LBOUND(array) TO size
  array(i) = value
 NEXT i
END SUB

SUB str_array_append (array() as string, s as string)
 REDIM PRESERVE array(LBOUND(array) TO UBOUND(array) + 1) as string
 array(UBOUND(array)) = s
END SUB

FUNCTION str_array_findcasei (array() as string, value as string) as integer
 DIM valuei as string = LCASE(value)
 FOR i as integer = LBOUND(array) TO UBOUND(array)
  IF LCASE(array(i)) = value THEN RETURN i
 NEXT
 RETURN -1
END FUNCTION

SUB int_array_append (array() as integer, byval k as integer)
 REDIM PRESERVE array(LBOUND(array) TO UBOUND(array) + 1) as integer
 array(UBOUND(array)) = k
END SUB

SUB intstr_array_append (array() as IntStrPair, byval k as integer, s as string)
 REDIM PRESERVE array(LBOUND(array) TO UBOUND(array) + 1)
 array(UBOUND(array)).i = k
 array(UBOUND(array)).s = s
END SUB

FUNCTION int_array_find (array() as integer, byval value as integer) as integer
 FOR i as integer = LBOUND(array) TO UBOUND(array)
  IF array(i) = value THEN RETURN i
 NEXT
 RETURN -1
END FUNCTION

'Resize a dynamic int array, removing all occurrences of k
SUB int_array_remove (array() as integer, byval k as integer)
 DIM i as integer = LBOUND(array)
 WHILE i <= UBOUND(array)
  IF array(i) = k THEN
   'Shuffle down
   FOR j as integer = i TO UBOUND(array) - 1
    array(j) = array(j + 1)
   NEXT
   IF UBOUND(array) > LBOUND(array) THEN REDIM PRESERVE array(LBOUND(array) TO UBOUND(array) - 1)
  END IF
  i += 1
 WEND
END SUB

SUB int_array_copy (fromarray() as integer, toarray() as integer)
 DIM as integer low = LBOUND(fromarray), high = UBOUND(fromarray)
 REDIM toarray(low TO high)
 memcpy @toarray(low), @fromarray(low), sizeof(integer) * (high - low + 1)
END SUB

'I've compared the speed of the following two. For random integers, the quicksort is faster
'for arrays over length about 80. For arrays which are 90% sorted appended with 10% random data,
'the cut off is about 600 (insertion sort did ~5x better on nearly-sort data at the 600 mark)

'Returns, in indices() (assumed to already have been dimmed large enough), indices for
'visiting the data (an array of some kind of struct containing an integer) in ascending order.
'start points to the integer in the first element, stride is the size of an array element, in integers
'Insertion sort. Running time is O(n^2). Much faster on nearly-sorted lists. STABLE
SUB sort_integers_indices(indices() as integer, byval start as integer ptr, byval number as integer, byval stride as integer)
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

FUNCTION integer_compare CDECL (byval a as integer ptr, byval b as integer ptr) as integer
 IF *a < *b THEN RETURN -1
 IF *a > *b THEN RETURN 1
 'implicitly RETURN 0 (it's faster to omit the RETURN :-)
END FUNCTION

FUNCTION integerptr_compare CDECL (byval a as integer ptr ptr, byval b as integer ptr ptr) as integer
 IF **a < **b THEN RETURN -1
 IF **a > **b THEN RETURN 1
 'implicitly RETURN 0 (it's faster to omit the RETURN :-)
END FUNCTION

'a string ptr is a pointer to a FB string descriptor
FUNCTION string_compare CDECL (byval a as string ptr, byval b as string ptr) as integer
 'This is equivalent, but the code below can be adapted for case insensitive compare (and is faster (what, how?!))
 'RETURN fb_StrCompare( *a, -1, *b, -1)

 DIM as integer ret = 0, somenull = 0
 'Ah, brings back happy memories of C hacking, doesn'it?
 IF @((*a)[0]) = 0 THEN ret -= 1: somenull = 1
 IF @((*b)[0]) = 0 THEN ret += 1: somenull = 1
 IF somenull THEN RETURN ret

 DIM k as integer = 0
 DIM chara as ubyte
 DIM charb as ubyte
 DO
  chara = (*a)[k]
  charb = (*b)[k]
  IF chara < charb THEN
   RETURN -1
  ELSEIF chara > charb THEN
   RETURN 1
  END IF
  k += 1
 LOOP WHILE chara OR charb
 RETURN 0
END FUNCTION

FUNCTION stringptr_compare CDECL (byval a as string ptr ptr, byval b as string ptr ptr) as integer
 RETURN string_compare(*a, *b)
END FUNCTION

'CRT Quicksort. Running time is *usually* O(n*log(n)). NOT STABLE
'See sort_integer_indices.
PRIVATE SUB qsort_indices(indices() as integer, byval start as any ptr, byval number as integer, byval stride as integer, byval compare_fn as FnCompare)
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

SUB qsort_integers_indices(indices() as integer, byval start as integer ptr, byval number as integer, byval stride as integer)
 qsort_indices indices(), start, number, stride, CAST(FnCompare, @integerptr_compare)
END SUB

SUB qsort_strings_indices(indices() as integer, byval start as string ptr, byval number as integer, byval stride as integer)
 qsort_indices indices(), start, number, stride, CAST(FnCompare, @stringptr_compare)
END SUB

'Invert a (possibly partial) permutation such as that returned by sort_integers_indices;
'indices() should normally contain the integers 0 to UBOUND(inverse),
'result stored in inverse().
'If an integer x between 0 and UBOUND(inverse) is missing from indices(),
'then inverse(x) contains garbage. You may want to clear inverse() first.
SUB invert_permutation(indices() as integer, inverse() as integer)
 FOR i as integer = 0 TO UBOUND(indices)
  DIM index as integer = indices(i)
  IF index >= 0 AND index <= UBOUND(inverse) THEN
   inverse(index) = i
  END IF
 NEXT
END SUB

SUB invert_permutation(indices() as integer)
 DIM inverse(UBOUND(indices)) as integer
 invert_permutation indices(), inverse()
 'Copy back
 memcpy(@indices(0), @inverse(0), sizeof(integer) * (UBOUND(indices) + 1))
END SUB

'These cache functions store a 'resetter' string, which causes search_string_cache
'to automatically empty the cache when its value changes (eg, different game).
'Normally you would just use the game_unique_id global for this.
'Note that you can resize the cache arrays (in either direction!) as you want at any time.
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

FUNCTION strhash(hstr as string) as unsigned integer
 RETURN stringhash(cptr(ubyte ptr, strptr(hstr)), len(hstr))
END FUNCTION


'------------- File Functions -------------


'Uses strhash, which is pretty fast despite being FB. I get 56MB/s on my netbook.
'Please not do depend on the algorithm not changing.
FUNCTION hash_file(filename as string) as unsigned integer
  DIM fh as integer = FREEFILE
  IF OPEN(filename FOR BINARY as #fh) THEN
    debug "hash_file: couldn't open " & filename
    RETURN 0
  END IF
  DIM size as integer = LOF(fh)
  DIM hash as unsigned integer = size
  hash += hash SHL 8
  DIM buf(4095) as ubyte
  WHILE size > 0
    DIM readamnt as integer
    fgetiob fh, , @buf(0), 4096, @readamnt
    IF readamnt < size AND readamnt <> 4096 THEN
      debug "hash_file: fgetiob failed!"
      RETURN 0
    END IF
    hash xor= stringhash(@buf(0), readamnt)
    hash += ROT(hash, 5)
    size -= 4096
  WEND
  CLOSE #fh
  RETURN hash
END FUNCTION

'Change / to \ in paths on Windows
FUNCTION normalize_path(filename as string) as string
  DIM ret as string = filename
#IFDEF __FB_WIN32__
  FOR i as integer = 0 TO LEN(ret) - 1 
    IF ispathsep(ret[i]) THEN ret[i] = asc(SLASH)
  NEXT
#ENDIF
  RETURN ret
END FUNCTION

FUNCTION trim_trailing_slashes(filename as string) as string
  DIM retend as integer = LEN(filename)
  WHILE retend > 0
    DIM ch as byte = filename[retend - 1]
    IF ispathsep(ch) THEN
      retend -= 1
    ELSE
      EXIT WHILE
    END IF
  WEND
  RETURN MID(filename, 1, retend)
END FUNCTION

FUNCTION trimpath(filename as string) as string
  'Return the file/directory name without path, and without trailing slash
  'See testcases below
  DIM temp as string = trim_trailing_slashes(filename)
  FOR i as integer = LEN(temp) TO 1 STEP -1
    IF ispathsep(temp[i - 1]) THEN
      RETURN MID(temp, i + 1, LEN(temp) - (i + 1) + 1)
    END IF
  NEXT
  RETURN temp
END FUNCTION

#IFDEF __FB_MAIN__
#DEFINE testtrimp(path, expected) testEqual(trimpath(path), normalize_path(expected))

startTest(trimpath)
  testtrimp("a/b/cat//", "cat")
  testtrimp("a/b/cat/",  "cat")
  testtrimp("a/b/cat",   "cat")
  testtrimp("cat/",      "cat")
  testtrimp("/cat/",     "cat")
  testtrimp("/",         ""   )
  testtrimp("",          ""   )
endTest
#ENDIF

FUNCTION trimfilename (filename as string) as string
  'Trim the last component of a path (which may be a directory rather than file!)
  'Return path without trailing slash. See testcases.
  'This is the complement to trimpath
  DIM ret as string = trim_trailing_slashes(normalize_path(filename))
  ret = MID(ret, 1, large(0, INSTRREV(ret, SLASH) - 1))
  IF is_absolute_path(filename) AND is_absolute_path(ret) = NO THEN
    RETURN get_path_root(filename)
  END IF
  return ret
END FUNCTION

#IFDEF __FB_MAIN__
#DEFINE testtrimf(path, expected) testEqual(trimfilename(path), normalize_path(expected))

startTest(trimfilename)
  testtrimf("a/b/cat//", "a/b")
  testtrimf("a/b/cat/",  "a/b")
  testtrimf("a/b/cat",   "a/b")
  testtrimf("cat/",      "")
  testtrimf("/cat/",     "/")
  #IFDEF __FB_WIN32__
    testtrimf("c:/vak",          "c:\")
    testtrimf("c:\vak/",         "c:\")
    testtrimf("c:\",             "c:\")
  #ENDIF
endTest
#ENDIF

FUNCTION trimextension (filename as string) as string
  'Return the filename (including path) without extension
  'Periods at the beginning of file/folder names are not counted as beginning an extension
  DIM at as integer = INSTRREV(filename, ".")
  DIM at2 as integer = INSTRREV(filename, "/")
#IFDEF __FB_WIN32__
  at2 = large(at2, INSTRREV(filename, "\"))
#ENDIF
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
  DIM at2 as integer = INSTRREV(filename, "/")
#IFDEF __FB_WIN32__
  at2 = large(at2, INSTRREV(filename, "\"))
#ENDIF
  IF at >= at2 + 2 THEN
    RETURN MID(filename, at + 1)
  ELSE
    RETURN ""
  END IF
END FUNCTION

'If a path is absolute return the root directory: / on Unix, X:/ or X:\ or / or \ on Windows
'Otherwise return ""
'FIXME: should handle network paths on Windows too
FUNCTION get_path_root (pathname as string) as string
#IFDEF __FB_WIN32__
  DIM first as string = LCASE(LEFT(pathname, 1))
  DIM temp as string = MID(pathname, 2, 2)
  IF first >= "a" ANDALSO first <= "z" ANDALSO (temp = ":\" OR temp = ":/") THEN
    RETURN MID(pathname, 1, 3)
  END IF
#ENDIF
  IF LEN(pathname) ANDALSO ispathsep(pathname[0]) THEN RETURN MID(pathname, 1, 1)
  RETURN ""
END FUNCTION

FUNCTION is_absolute_path (sDir as string) as integer
  RETURN LEN(get_path_root(sDir)) <> 0
END FUNCTION

'Make a path absolute. See also absolute_with_orig_path
FUNCTION absolute_path(pathname as string) as string
  IF NOT is_absolute_path(pathname) THEN RETURN CURDIR & SLASH & pathname
  RETURN pathname
END FUNCTION

FUNCTION absolute_with_orig_path(file_or_dir as string, byval add_slash as integer = NO) as string
  DIM d as string = file_or_dir
  IF NOT is_absolute_path(d) THEN d = orig_dir & SLASH & d
  IF add_slash AND RIGHT(d, 1) <> SLASH THEN d = d & SLASH
  RETURN d
END FUNCTION

'Remove redundant ../, ./, // in a path. Handles both relative and absolute paths
'Result has normalised slashes and no trailing slash (unless it's the root /).
'See testcases below
FUNCTION simplify_path(sDir as string) as string
  DIM piecesarray() as string
  DIM pieces as string vector
  DIM pathname as string = normalize_path(sDir)
  DIM isabsolute as integer = is_absolute_path(pathname)
  'remove drive letter
  DIM ret as string = get_path_root(pathname)
  'Trim everything except the final slash of the root
  IF LEN(ret) THEN
   pathname = MID(pathname, LEN(ret))
  END IF

  split pathname, piecesarray(), SLASH
  array_to_vector pieces, piecesarray()
  DIM i as integer = 0
  DIM leading_updots as integer = 0  'The number of "../"s at the start
  WHILE i < v_len(pieces)
    IF pieces[i] = "" OR pieces[i] = "." THEN
      v_delete_slice pieces, i, i+1
    ELSEIF pieces[i] = ".." THEN
      IF i = 0 ANDALSO isabsolute THEN
        'Can't go up in the root directory
        v_delete_slice pieces, i, i+1
      ELSEIF i > leading_updots THEN
        v_delete_slice pieces, i-1, i+1
        i -= 1
      ELSE
        leading_updots += 1
        i += 1
      END IF
    ELSE
      i += 1
    END IF
  WEND
  FOR i = 0 TO v_len(pieces) - 1
    IF i <> 0 THEN ret += SLASH
    ret += pieces[i]
  NEXT
  v_free pieces
  IF ret = "" THEN ret = "."   'so that appending a slash is safe
  RETURN ret
END FUNCTION

#IFDEF __FB_MAIN__
#DEFINE testsimplify(path, expected) testEqual(simplify_path(path), normalize_path(expected))

startTest(simplify_path)
  testsimplify("testcases",         "testcases")
  testsimplify(".././../foo/",      "../../foo")
  testsimplify(".././a/../../foo/", "../../foo")
  testsimplify("/..",   "/")
  testsimplify("",      ".")
  testsimplify(".",     ".")
  testsimplify("/../.", "/")
  testsimplify("./../../../../a",   "../../../../a")
  testsimplify("//.//../a/../c/b/../d", "/c/d")
  '? simplify_path(absolute_path(".."))
  '? "curdir=" & curdir
  '? "reallysimplify('" & path & "')=" + simplify_path_further(absolute_path("a/b/.svn"), curdir)
endTest
#ENDIF

'Make a path relative if it's below 'fromwhere' (which is a path, not a file)
'It would be possible to also possibly return something starting with some ../'s, but it's more trouble
FUNCTION simplify_path_further(pathname as string, fromwhere as string) as string
  DIM path as string = simplify_path(pathname)
  DIM source as string = simplify_path(fromwhere)
  IF RIGHT(source, 1) <> SLASH THEN source += SLASH  'need a slash so we don't match foo/ and foo.rpgdir/
  IF is_absolute_path(path) THEN
#IFDEF __FB_WIN32__
    DIM matchlen as integer = length_matching(LCASE(source), LCASE(path))
#ELSE
    DIM matchlen as integer = length_matching(source, path)
#ENDIF
    IF matchlen = LEN(source) THEN
      IF matchlen >= LEN(path) THEN
        'they are equal  (>= for the extra slash on source)
        RETURN "."
      ELSE   
        RETURN MID(path, matchlen + 1)
      END IF
    END IF

    'DIM driveletter as string = get_driveletter(path)
    'IF get_driveletter(source) <> driveletter THEN RETURN path
    'matchlen = instrrev(path, SLASH, matchlen)
    'path = MID(path, matchlen + 1)
  END IF
  RETURN path
END FUNCTION

'Go up a number of directories. Simplifies and normalises.
'pathname is interpreted as a directory even if missing the final slash!
FUNCTION parentdir (path as string, byval upamount as integer = 1) as string
  DIM pathname as string = path + SLASH
  FOR i as integer = 0 TO upamount - 1
   pathname += ".." + SLASH
  NEXT
  DIM ret as string = simplify_path(pathname)
  IF RIGHT(ret, 1) <> SLASH THEN ret += SLASH
  RETURN ret
END FUNCTION

FUNCTION anycase (filename as string) as string
  'create a case-insensitive regex from a filename
#IFDEF __FB_WIN32__
  'Windows filenames are always case-insenstitive
  RETURN filename
#ELSE
  DIM ascii as integer
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

FUNCTION escape_filename (filename as string) as string
  'This is intended for escaping filenames for use in shells
#IFDEF __UNIX__
  'Don't escape '
  RETURN """" & escape_string(filename, """`\$|<>&#;()") & """"
#ELSE
  'Note " is not allowed in filenames
  RETURN """" & filename & """"
#ENDIF
END FUNCTION

FUNCTION escape_filenamec CDECL (byval filename as zstring ptr) as zstring ptr
  DIM ret as string = escape_filename(*filename)
  DIM retz as zstring ptr = malloc(LEN(ret) + 1)
  strcpy retz, cstring(ret)
  RETURN retz
END FUNCTION

'This is a replacement for SHELL. It needs to be used on Windows if the executable was escaped (so contains quotes)
SUB safe_shell (cmd as string)
#IFDEF __FB_WIN32__
  'SHELL wraps system() which calls cmd.exe (or command.com on older OSes)
  'cmd.exe will remove the first and last quotes from the string and leave the rest.
  'Therefore, there need to be two quotes at the beginning of the string!
  SHELL """" & cmd & """"
#ELSE
  SHELL cmd
#ENDIF
END SUB

SUB touchfile (filename as string)
  dim as integer fh = FREEFILE
  OPEN filename FOR BINARY as #fh
  CLOSE #fh
END SUB

'Increases (never decreases) the length of a file by appending NUL bytes as required.
'Writing off the end of a file writes garbage between the new data and the end of the old file.
'Use this function to extend the file first.
SUB extendfile (byval fh as integer, byval length as integer)
 DIM curlen as integer = LOF(fh)
 IF curlen < length THEN
  DIM oldpos as integer = SEEK(fh)
  DIM buf(length - curlen - 1) as ubyte
  PUT #fh, curlen + 1, buf()
  SEEK #fh, oldpos
 END IF
END SUB

#ifdef __UNIX__

'Attempt to decode a filename (using native encoding) and then convert down to Latin-1 encoding
PRIVATE FUNCTION decode_filename(filename as string) as string
  DIM length as integer
  DIM unicode as wstring ptr

/'
  'Always assume UTF8 -- doesn't work in practise
  length = utf8_length(strptr(filename))
  IF length < 0 THEN
    debuginfo "decode_filename(" & filename & ") failed, " & length
    RETURN filename
  END IF
  unicode = utf8_decode(strptr(filename), @length)
  IF unicode = NULL THEN RETURN filename  'Shouldn't happen
'/
#ifdef __FB_ANDROID__
  'Android NDK doesn't support mbstowcs or non-C locales (only exposed to Java apps)
  'FIXME: Not sure what to do...
  RETURN filename
#else
  length = mbstowcs(NULL, STRPTR(filename), 0)
  IF length = -1 THEN
    debuginfo "decode_filename(" & filename & ") failed"
    RETURN filename   'not valid UTF-8 (Note: we continue on valid ASCII)
  END IF
  unicode = allocate(SIZEOF(wstring) * (length + 1))
  mbstowcs(unicode, STRPTR(filename), length + 1)

  'DIM ret as string = SPACE(length)
  'wstring_to_latin1(unicode, strptr(ret), length + 1)
  DIM ret as string = *unicode
  'debug "decode_filename(" & filename & ") = " & ret
  deallocate unicode
  RETURN ret
#endif

END FUNCTION

#elseif defined(__FB_WIN32__)

'Convert Windows-1252 to Latin-1 by removing the extra characters
FUNCTION decode_filename(filename as string) as string
  DIM ret as string = SPACE(LEN(filename))
  FOR i as integer = 0 TO LEN(filename) - 1
    IF filename[i] >= 127 AND filename[i] <= 160 THEN
      ret[i] = ASC("?")
    ELSE
      ret[i] = filename[i]
    END IF
  NEXT
  RETURN ret
END FUNCTION

#endif

'Finds files in a directory, writing them into an array without their path
'filelist() must be resizeable; it'll be resized so that LBOUND = -1, with files, if any, in filelist(0) up
'By default, find all files in directory, otherwise namemask is a case-insensitive filename mask
'filetype is one of fileTypeFile, fileTypeDirectory
SUB findfiles (directory as string, namemask as string = "", byval filetype as integer = fileTypeFile, byval findhidden as integer = 0, filelist() as string)
  DIM as string searchdir = directory
  IF RIGHT(searchdir, 1) <> SLASH THEN searchdir += SLASH
  DIM as string nmask = anycase(namemask)
  IF LEN(nmask) = 0 THEN nmask = ALLFILES
  REDIM filelist(-1 TO -1)
#ifdef DEBUG_FILE_IO
  debuginfo "findfiles(directory = " & directory & ", namemask = " & namemask & ", " _
            & IIF_string(filetype = fileTypeFile, "fileTypeFile", "fileTypeDirectory") & ", findhidden = " & findhidden & ")"
#endif

#ifdef __UNIX__
  DIM filenames as string vector

  IF filetype = fileTypeDirectory THEN
    v_move filenames, list_subdirs(searchdir, nmask, findhidden)
  ELSE
    v_move filenames, list_files(searchdir, nmask, findhidden)
  END IF
        
  FOR i as integer = 0 TO v_len(filenames) - 1
    IF filetype = fileTypeDirectory THEN
     'Filter out some Linux dirs that we should not be browsing around in.
     IF filenames[i] = "dev" ORELSE filenames[i] = "proc" ORELSE filenames[i] = "sys" THEN CONTINUE FOR
     'Maybe we should filter out some other dirs on Mac and on Android?
    END IF
    str_array_append filelist(), decode_filename(filenames[i])
  NEXT

  v_free filenames

#else
  'On Windows, non-unicode-enabled programs automatically get their filenames downconverted to Windows-1252,
  'so we only restrict further, to Latin-1.
  'However, once we want to support more than just Latin-1 filenames, we will have to rewrite
  'this properly, using winapi calls, because FB's DIR has no support.

  DIM foundfile as string
  DIM attrib as integer
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
  REDIM tempfilelist(-1 TO -1) as string
  DO UNTIL foundfile = ""
    str_array_append tempfilelist(), foundfile
    foundfile = DIR '("", attrib)
  LOOP
  FOR i as integer = 0 TO UBOUND(tempfilelist)
    foundfile = tempfilelist(i)
    IF foundfile = "." ORELSE foundfile = ".." THEN CONTINUE FOR
    IF filetype = fileTypeDirectory THEN
      'alright, we want directories, but DIR is too broken to give them to us
      'files with attribute 0 appear in the list, so single those out
      IF DIR(searchdir + foundfile, 55) = "" OR DIR(searchdir + foundfile, 39) <> "" THEN CONTINUE FOR
    END IF
    str_array_append filelist(), decode_filename(foundfile)
  NEXT
#endif
END SUB

SUB writeablecopyfile(src as string, dest as string)
 copyfile src, dest
 setwriteable dest
END SUB

'Copy files in one directory to another (ignores directories)
SUB copyfiles(src as string, dest as string, byval copyhidden as integer = 0)
 DIM filelist() as string
 findfiles src, ALLFILES, fileTypeFile, copyhidden, filelist()
 FOR i as integer = 0 TO UBOUND(filelist)
  writeablecopyfile src + SLASH + filelist(i), dest + SLASH + filelist(i)
 NEXT
END SUB

FUNCTION copydirectory (src as string, dest as string, byval copyhidden as integer = -1) as string
 'Recursively copy directory src to directory dest. Dest should not already exist
 'returns "" on success, or an error string on failure. Failure might leave behind a partial copy.
 IF isdir(dest) THEN RETURN "copydirectory: Destination """ & dest & """ must not already exist"
 
 '--create the dest directory
 IF makedir(dest) <> 0 THEN RETURN "copydirectory: Couldn't create """ & dest & """"

 '--copy all the files
 DIM filelist() as string
 findfiles src, ALLFILES, fileTypeFile, copyhidden, filelist()
 FOR i as integer = 0 TO UBOUND(filelist)
  writeablecopyfile src & SLASH & filelist(i), dest & SLASH & filelist(i)
  IF NOT isfile(dest & SLASH & filelist(i)) THEN
   RETURN "copydirectory: Couldn't copy file """ & dest & SLASH & filelist(i) & """"
  END IF
 NEXT i

 '--recursively copy all the subdirectories
 DIM result as string = ""
 DIM dirlist() as string
 findfiles src, ALLFILES, fileTypeDirectory, copyhidden, dirlist()
 FOR i as integer = 0 TO UBOUND(dirlist)
  IF dirlist(i) = "." ORELSE dirlist(i) = ".." THEN CONTINUE FOR
  result = copydirectory(src & SLASH & dirlist(i), dest & SLASH & dirlist(i), copyhidden)
  IF result <> "" THEN RETURN result
 NEXT i
 
 RETURN ""
 
END FUNCTION

SUB killdir(directory as string, recurse as integer=0)
  DIM filelist() as string
  findfiles directory, ALLFILES, fileTypeFile, -1, filelist()
  FOR i as integer = 0 TO UBOUND(filelist)
    safekill directory + SLASH + filelist(i)
  NEXT
#ifdef DEBUG_FILE_IO
  debuginfo "killdir(" & directory & ", recurse = " & recurse & ")"
#endif
  IF recurse THEN
   DIM dirlist() as string
   findfiles directory, ALLFILES, fileTypeDirectory, -1, dirlist()
   FOR i as integer = 0 TO UBOUND(dirlist)
    IF dirlist(i) = "." ORELSE dirlist(i) = ".." THEN CONTINUE FOR
    'debuginfo "recurse to " & directory & SLASH & dirlist(i)
    killdir directory & SLASH & dirlist(i), -1
   NEXT i
  END IF
  IF RMDIR(directory) THEN
    'errno would get overwritten while building the error message
    DIM err_string as string = *get_sys_err_string()
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
#ifdef DEBUG_FILE_IO
  debuginfo "makedir(" & directory & ")"
#endif
  IF MKDIR(directory) THEN
    'errno would get overwritten while building the error message
    DIM err_string as string = *get_sys_err_string()
    'The heck? On Windows at least, MKDIR throws this false error
#ifdef __FB_WIN32__
    IF err_string = "File exists" THEN RETURN 0
#endif
    debug "Could not mkdir(" & directory & "): " & err_string
    RETURN 1
  END IF
#ifdef __FB_LINUX__
  ' work around broken file permissions in dirs created by linux version
  ' MKDIR creates with mode 644, should create with mode 755
  SHELL "chmod +x " + escape_filename(directory)
#endif
  RETURN 0
END FUNCTION

SUB safekill (filename as string)
  DIM exists as bool = isfile(filename)
#ifdef DEBUG_FILE_IO
  debuginfo "safekill(" & filename & ") exists = " & exists
#endif
  IF exists THEN
   'KILL is a thin wrapper around C's remove(), however by calling it directly we can get a textual error message
   IF remove(strptr(filename)) THEN
    DIM err_string as string = *get_sys_err_string()
    debug "Could not remove(" & filename & "): " & err_string

    'NOTE: on Windows, even if deletion fails because the file is open, the file will be marked
    'to be deleted once everyone closes it. Also, it will no longer be possible to open it.
    'On Unix, you can unlink a file even when someone else has it open.
   END IF
  END IF
END SUB

'FIXME/NOTE: On Unix this can not move between different filesystems, so only use between "nearby" locations!
'NOTE: An alternative function is os_shell_move 
'Returns zero on success 
FUNCTION local_file_move(frompath as string, topath as string) as integer
  'FB's NAME is translated directly to a rename() call, so is no better
  IF rename(frompath, topath) THEN
    DIM err_string as string = *get_sys_err_string()  'errno would get overwritten while building the error message
    debug "rename(" & frompath & ", " & topath & ") failed (dest exists=" & isfile(topath) & ") Reason: " & err_string
    RETURN 1
  END IF
  RETURN 0
END FUNCTION

FUNCTION fileisreadable(filename as string) as integer
  dim ret as bool = NO
  dim fh as integer, err_code as integer
  fh = freefile
  err_code = open(filename for binary access read as #fh)
  if err_code = 2 then
    'Doesn't exist
  elseif err_code <> 0 then
    debuginfo "fileisreadable: Error " & err_code & " reading " & filename
  else
    close #fh
    ret = YES
  end if
#ifdef DEBUG_FILE_IO
  debuginfo "fileisreadable(" & filename & ") = " & ret
#endif
  return ret
END FUNCTION

FUNCTION fileiswriteable(filename as string) as integer
  dim ret as bool = NO
  dim fh as integer
  fh = freefile
  if open (filename for binary access read write as #fh) = 0 then
    close #fh
    ret = YES
  end if
#ifdef DEBUG_FILE_IO
  debuginfo "fileiswriteable(" & filename & ") = " & ret
#endif
  return ret
END FUNCTION

FUNCTION diriswriteable(d as string) as integer
  if isfile(d + SLASH + "archinym.lmp") then
   'Kludge to detect an rpgdir full of unwriteable files: on Windows you don't seem
   'able to mark a folder read-only, instead it makes the contents read-only.
    if fileiswriteable(d + SLASH + "archinym.lmp") = 0 then return 0
  end if
  dim testfile as string = d & SLASH & "__testwrite_" & randint(100000) & ".tmp"
  if fileiswriteable(testfile) then
    safekill testfile
    return -1
  end if
  return 0
END FUNCTION

FUNCTION isfile (filename as string) as integer
  ' directories don't count as files
  ' FIXME: Returns true if passed a directory on Linux.
  ' this is a simple wrapper for fileisreadable
  if filename = "" then return 0
  return fileisreadable(filename)
END FUNCTION

FUNCTION isdir (sDir as string) as integer
  dim ret as bool
#IFDEF __FB_ANDROID__
  '[ does not work in Android 2.2. I don't know how reliable this is
  ret = SHELL("ls " + escape_filename(sDir) + "/") = 0
#ELSEIF DEFINED(__UNIX__)
  'Special hack for broken Linux dir() behavior
  '(FIXME: is DIR still broken? Should investigate)
  ret = SHELL("[ -d " + escape_filename(sDir) + " ]") = 0
#ELSE
  'Windows just uses dir (ugh)
  'Have to remove trailing slash, otherwise dir always returns nothing
  dim temp as string = rtrim(sdir, any "\/")
  ret = dir(temp, 55) <> "" AND dir(temp, 39) = ""
#ENDIF
#IFDEF DEBUG_FILE_IO
  debuginfo "isdir(" & sDir & ") = " & ret
#ENDIF
  return ret
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
  dim n as integer = 0
  dim lit as any ptr = this.first
  while lit
    if lit = item then return n
    n += 1
    lit = DLFOLLOW(lit)->next
  wend
  return -1
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

FUNCTION hash_find(byref this as HashTable, byval hash as unsigned integer, byval key as any ptr = NULL) as any ptr
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
		IF OPEN(filename FOR BINARY ACCESS READ as #ff) THEN
			fatalerror errmsg
		END IF
		GET #ff,, byt 'Magic number, always 253
		IF byt <> 253 THEN
			CLOSE #ff
			fatalerror errmsg & " (bad header)"  'file may also be zero length
		END IF
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
	dim ff as integer, byt as UByte, seg as uShort, offset as Short, length as Short
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
	OPEN filename FOR BINARY ACCESS write as #ff  'Truncate
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

SUB setbit (bb() as integer, byval w as integer, byval b as integer, byval v as integer)
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

FUNCTION readbit (bb() as integer, byval w as integer, byval b as integer)  as integer
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

'Prehaps doesn't belong here because scancodes are OHR-specific. However, OHR
'scancodes are 95% the same as FB scancodes
FUNCTION scancodename (byval k as integer) as string
 'static scancodenames(...) as string * 14 = { ... }
 #INCLUDE "scancodenames.bi"

 IF k >= lbound(scancodenames) and k <= ubound(scancodenames) THEN
  IF scancodenames(k) <> "" THEN return scancodenames(k)
 END IF
 return "scancode" & k
END FUNCTION

FUNCTION special_char_sanitize(s as string) as string
 'This is a datalossy function.
 'Remove special characters from an OHR string to make it 7-bit ASCII safe.
 'Also translates the old OHR and the new Latin-1 copyright chars to (C)
 DIM result as string = ""
 FOR i as integer = 0 TO LEN(s) - 1
  SELECT CASE s[i]
   CASE 32 TO 126:
    result &= CHR(s[i])
   CASE 134, 169:
    result &= "(C)"
  END SELECT
 NEXT i
 RETURN result
END FUNCTION

FUNCTION starts_with(s as string, prefix as string) as integer
 'Return YES if the string begins with a specific prefix
 RETURN MID(s, 1, LEN(prefix)) = prefix
END FUNCTION

FUNCTION ends_with(s as string, suffix as string) as integer
 'Return YES if the string ends with a specific prefix
 RETURN RIGHT(s, LEN(suffix)) = suffix
END FUNCTION

FUNCTION byte_size_of_file(filename as string) as integer
 DIM fh as integer = FREEFILE
 OPEN filename for binary access read as #fh
 byte_size_of_file = LOF(fh)
 CLOSE #fh
END FUNCTION

FUNCTION count_directory_size(directory as string) as integer
 '--Count the bytes in all the files in a directory and all subdirectories.
 '--This doesn't consider the space taken by the directories themselves,
 '--nor does it consider blocksize or any other filesystem details.
 DIM bytes as integer = 0
 DIM filelist() as string
 
 '--First cound files
 findfiles directory, ALLFILES, fileTypeFile, -1, filelist()
 FOR i as integer = 0 TO UBOUND(filelist)
  bytes += byte_size_of_file(directory & SLASH & filelist(i))
 NEXT
 
 '--Then count subdirectories
 findfiles directory, ALLFILES, fileTypeDirectory, -1, filelist()
 FOR i as integer = 0 TO UBOUND(filelist)
  bytes += count_directory_size(directory & SLASH & filelist(i))
 NEXT
 
 RETURN bytes
END FUNCTION

FUNCTION string_from_first_line_of_file (filename as string) as string
 'Read the first line of a text file and return it as a string.
 'ignore/removes any line-ending chars
 DIM fh as integer = FREEFILE
 DIM result as string
 OPEN filename for input as #fh
 LINE INPUT #fh, result
 CLOSE #fh
 RETURN result
END FUNCTION

FUNCTION string_from_file (filename as string) as string
 'Read an entire file as a string.
 'convert the line endings to LF only
 DIM fh as integer = FREEFILE
 DIM result as string = ""
 DIM s as string
 OPEN filename for input as #fh
 DO WHILE NOT EOF(fh)
  LINE INPUT #fh, s
  s = RTRIM(s)
  result &= s & CHR(10)
 LOOP
 CLOSE #fh
 RETURN result
END FUNCTION

SUB string_to_file (string_to_write as string, filename as string)
 'Write a string to a text file using native line endings
 DIM s as string = string_to_write
 replacestr string_to_write, !"\n", LINE_END
 DIM fh as integer = FREEFILE
 OPEN filename FOR BINARY AS #FH
 PUT #fh, , s
 CLOSE #fh
END SUB

'Read each line of a file into a string array
SUB lines_from_file(strarray() as string, filename as string)
 REDIM strarray(-1 TO -1)
 DIM as integer fh
 fh = FREEFILE
 IF OPEN(filename FOR INPUT as #fh) THEN
  debug "Could not open " & filename
  EXIT SUB
 END IF
 DO UNTIL EOF(fh)
  DIM text as string
  LINE INPUT #fh, text
  str_array_append strarray(), text
 LOOP
 CLOSE #fh
END SUB

'Write an array of strings to a file, one-per-line.
'unix line endings will automatically be added
SUB lines_to_file(strarray() as string, filename as string)
 DIM fh as integer
 fh = FREEFILE
 IF OPEN(filename FOR BINARY ACCESS WRITE as #fh) THEN
  debug "Could not open " & filename & " for writing"
  EXIT SUB
 END IF
 FOR i as integer = 0 TO UBOUND(strarray)
  PUT #fh, , strarray(i) & !"\n"
 NEXT i
 CLOSE #fh
END SUB

'Note: Custom doesn't use this function
SUB set_tmpdir ()
 DIM tmp as string
 #IFDEF __FB_WIN32__
  'Windows only behavior
  tmp = environ("TEMP")
  IF NOT diriswriteable(tmp) THEN tmp = environ("TMP")
  IF NOT diriswriteable(tmp) THEN tmp = exepath
  IF NOT diriswriteable(tmp) THEN tmp = CURDIR
  IF NOT diriswriteable(tmp) THEN fatalerror "Unable to find any writable temp dir"
 #ELSEIF DEFINED(__FB_ANDROID__)
  'SDL sets initial directory to .../com.hamsterrepublic.ohrrpgce.game/files
  tmp = orig_dir
 #ELSE
  'Unix only behavior
  tmp = environ("HOME") + SLASH + ".ohrrpgce"
 #ENDIF
 IF NOT isdir(tmp) THEN
  IF makedir(tmp) <> 0 THEN fatalerror "Temp directory " & tmp & " missing and unable to create it"
 END IF
 IF RIGHT(tmp, 1) <> SLASH THEN tmp = tmp & SLASH
 DIM as string d = DATE, t = TIME
 tmp += "ohrrpgce" & MID(d,7,4) & MID(d,1,2) & MID(d,4,2) & MID(t,1,2) & MID(t,4,2) & MID(t,7,2) & "." & randint(1000) & ".tmp" & SLASH
 tmpdir = tmp
END SUB

SUB write_ini_value (ini_filename as string, key as string, value as integer)
 REDIM ini(0) as string
 lines_from_file ini(), ini_filename
 write_ini_value ini(), key, value
 lines_to_file ini(), ini_filename
END SUB

SUB write_ini_value (ini() as string, key as string, value as integer)
 'Key is case insensitive but case preservative
 'If the key is matched more than once, all copies of it will be changed
 IF LEN(key) = 0 THEN
  debug "Can't write empty key to ini file"
  EXIT SUB
 END IF
 DIM found as bool = NO
 FOR i as integer = 0 TO UBOUND(ini)
  IF ini_key_match(key, ini(i)) THEN
   ini(i) = key & " = " & value
   found = YES
  END IF
 NEXT i
 IF NOT found THEN
  REDIM ini(UBOUND(ini)+1) as string
  ini(UBOUND(ini)) = key & " = " & value
 END IF
END SUB

FUNCTION read_ini_int (ini_filename as string, key as string, byval default as integer=0) as integer
 REDIM ini(0) as string
 lines_from_file ini(), ini_filename
 RETURN read_ini_int(ini(), key, default)
END FUNCTION

FUNCTION read_ini_int (ini() as string, key as string, byval default as integer=0) as integer
 IF LEN(key) = 0 THEN
  debug "Can't read empty key from ini file"
  RETURN default
 END IF
 FOR i as integer = 0 TO UBOUND(ini)
  IF ini_key_match(key, ini(i)) THEN
   RETURN ini_value_int(ini(i), default)
  END IF
 NEXT i
 RETURN default
END FUNCTION

FUNCTION ini_key_match(key as string, s as string) as bool
 DIM comp as string
 comp = LCASE(key & "=")
 IF LCASE(LEFT(s, LEN(comp))) = comp THEN RETURN YES
 comp = LCASE(key & " =")
 IF LCASE(LEFT(s, LEN(comp))) = comp THEN RETURN YES
 RETURN NO
END FUNCTION

FUNCTION ini_value_int (s as string, byval default as integer=0) as integer
 'Extracts and returns an integer from the value portion of an ini line.
 'Does NOT check the key. You should use ini_key_match() before calling this
 'default is returned if there is an error parsing the line
 DIM eqpos as integer = INSTR(s, "=")
 IF eqpos = 0 THEN
  'no equal sign found
  RETURN default
 END IF
 DIM tail as string = MID(s, eqpos + 1)
 RETURN str2int(tail, -1)
END FUNCTION
