'OHRRPGCE - util.bi
'(C) Copyright 1997-2006 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#IFNDEF UTIL_BI
#DEFINE UTIL_BI

#IFNDEF NULL
#DEFINE NULL 0
#ENDIF

TYPE bool as integer
' Temporary, until we can declare bool as an int32
TYPE bool32 as long

'Even though long and integer are the same size on 32 bit platforms,
'fbc considers them different types and throws warnings!
#IFDEF __FB_64BIT__
  #IFNDEF int32
    TYPE int32 as long
  #ENDIF
  #IFNDEF uint32
    TYPE uint32 as ulong
  #ENDIF
#ELSE
  #IFNDEF int32
    TYPE int32 as integer
  #ENDIF
  #IFNDEF uint32
    TYPE uint32 as uinteger
  #ENDIF
#ENDIF

'---For some crazy reason TRUE and FALSE don't work well as const even though they are not reserved
CONST YES = -1
CONST NO = 0

#include "vector.bi"
#include "crt/stddef.bi"


'----------------------------------------------------------------------
'                           Macro utilities


#DEFINE _CONCAT(a,b) a##b
#DEFINE TEMPLNAME(a,b) a##__##b
#DEFINE MACROSTART #MACRO

#MACRO TEMPLATE_BASE(name, typename)
  'Internal junk. See TEMPLATE for documentation

  #MACRO INHERIT_##name(T)
    name##_MEMBERS(T)
  #ENDMACRO

  'Gimmick, ignore. Like a TEMPLATE_GENERIC version of INHERIT
  #MACRO INHERITAS_##name(T, whatalias)
    UNION
      TYPE
        name##_MEMBERS(T)
      END TYPE
      whatalias as TEMPLNAME(name, ANY)
    END UNION
  #ENDMACRO

  #DEFINE name(T) _CONCAT(name##__, T)

  #UNDEF CUR_TEMPL_DEFN
  #DEFINE CUR_TEMPL_DEFN name

  MACROSTART name##_MEMBERS(typename)
#ENDMACRO

#MACRO TEMPLATE(name, typename)
  /'
   ' Workaround for lack of templates. Single type argument (could add more). Usage example:

     TEMPLATE(MyTemplate, T)
       foo as T
     #ENDMACRO  'yes, weird, can't do anything about it

     'instantiate MyTemplate<integer>
     MAKETYPE_MyTemplate(integer)

   ' Instantiated templates need to be declared with MAKETYPE_* before use. Then
   ' MyTemplate(integer) is the instance (compare MyTemplate<int> in C++).
   ' You can embed as unnamed nested types with INHERIT_*; MAKETYPE_* is not required.
   '/

  #MACRO MAKETYPE_##name(T)
    'you can declare the same alias multiple times.
    TYPE _CONCAT(T,Fwd) as T

    TYPE TEMPLNAME(name, T)
      name##_MEMBERS(_CONCAT(T,Fwd))
    END TYPE
  #ENDMACRO

  TEMPLATE_BASE(name, typename)
#ENDMACRO

#MACRO TEMPLATE_GENERIC(name, typename)
  /'
   ' This is the same as TEMPLATE, but assumes that Any is a valid substitution for T,
   ' as MAKETYPE creates a union with a member 'generic' for easy casting to TypeName(Any),
   ' intended for passing to generic functions. Templates really need OO to patch up the
   ' problems they introduce.

   ' You should insert 'ENDGENERIC' after the '#ENDMACRO'
   '/

  #MACRO MAKETYPE_##name(T)
    'you can declare the same alias multiple times.
    TYPE _CONCAT(T,Fwd) as T

    UNION TEMPLNAME(name, T)
      TYPE
        name##_MEMBERS(_CONCAT(T,Fwd))
      END TYPE
      generic as TEMPLNAME(name, ANY)
    END UNION
  #ENDMACRO

  TEMPLATE_BASE(name, typename)
#ENDMACRO

#DEFINE ENDGENERIC _
  /'This is just MAKETYPE_##name(ANY), but that would be recursive, says FB'/ _
  TYPE TEMPLNAME(CUR_TEMPL_DEFN, ANY) : _
    _CONCAT(CUR_TEMPL_DEFN,_MEMBERS)(ANY) : _
  END TYPE


'----------------------------------------------------------------------
'                             Data types


'Static variable-length arrays need to be redimmed before first use.
'This does that, while leaving the array alone if already initialised,
'in a way that should work in any FB version, assuming 1 dimension and LBOUND 0.
'Since FB 0.90, UBOUND(array) returns -1 if the array is uninitialised
#define initialize_static_dynamic_array(array) REDIM PRESERVE array(large(0, UBOUND(array)))

TYPE IntStrPair
  i as integer
  s as string
END TYPE

declare function str_array_findcasei (array() as string, value as string) as integer
declare function int_array_find (array() as integer, byval value as integer) as integer
declare sub array_shuffle_to_end overload(array() as integer, which as integer)
declare sub array_shuffle_to_end overload(array() as string, which as integer)

'These act on *dynamically typed* destination arrays only! Static arrays will segfault!
declare sub str_array_append (array() as string, s as string)
declare sub int_array_append (array() as integer, byval k as integer)
declare sub intstr_array_append (array() as IntStrPair, byval k as integer, s as string)
declare sub int_array_remove (array() as integer, byval k as integer)
declare sub int_array_copy (fromarray() as integer, toarray() as integer)

/'
declare sub int_array_exclude (() as integer, array() as integer)
declare sub int_array_sort (dest() as integer, src() as integer)
'/


'--------------- Stack ----------------


'New stack
TYPE Stack
  pos as integer ptr
  bottom as integer ptr
  size as integer
END TYPE

declare sub createstack (st as Stack)
declare sub destroystack (st as Stack)
declare sub checkoverflow (st as Stack, byval amount as integer = 1)
declare sub setstackposition (st as Stack, byval position as integer)
#define stackposition(stack)          ((stack).pos - (stack).bottom)
#define pushstack(stack, datum)       *(stack).pos = (datum) : (stack).pos += 1
#define popstack(stack, var)          (stack).pos -= 1 : (var) = *(stack).pos
'read from a stack offset from the last push (eg. 0 is last int pushed, -1 is below that)
#define readstack(stack, off)         stack.pos[(off) - 1]
#define checkunderflow(stack, amount) ((stack).pos - (amount) < (stack).bottom)

'Old allmodex stack (FIXME: get rid of this)
declare sub setupstack ()
declare sub pushw (byval word as integer)
declare function popw () as integer
declare sub pushdw (byval word as integer)
declare function popdw () as integer
declare sub releasestack ()
declare function stackpos () as integer
declare function readstackdw (byval off as integer) as integer
declare function readstackw (byval off as integer) as integer



'------------ String Cache ------------

declare function search_string_cache (cache() as IntStrPair, byval key as integer, resetter as string = CHR(234)) as string
declare sub add_string_cache (cache() as IntStrPair, byval key as integer, value as string)
declare sub remove_string_cache (cache() as IntStrPair, byval key as integer)


'--------- Doubly Linked List ---------

'doubly linked list header
TEMPLATE_GENERIC(DoubleList, T)
  numitems as integer
  first as T ptr
  last as T ptr
  memberoffset as integer '= OFFSETOF(T, DListItem)
#ENDMACRO
ENDGENERIC

'doubly linked list item
'WARNING: don't add strings to this
TEMPLATE_GENERIC(DListItem, T)
  next as T ptr
  prev as T ptr
#ENDMACRO
ENDGENERIC

'DList function 'item' arguments are pointers to objects containing DListItem instances.
'You have to provide the offset of the DListItem as itemoffset to dlist_construct.
'Pass
declare sub dlist_construct (byref this as DoubleList(Any), byval itemoffset as integer)

'NULL as beforeitem inserts at end
'newitem must not already be a member of a list!
declare sub dlist_insertat (byref this as DoubleList(Any), byval beforeitem as any ptr, byval newitem as any ptr)

#define dlist_append(this, newitem) dlist_insertat((this), NULL, (newitem))

declare sub dlist_remove (byref this as DoubleList(Any), byval item as any ptr)

'swap the positions of two items, already in (possibly different) lists
declare sub dlist_swap (byref this as DoubleList(Any), byval item1 as any ptr, byref that as DoubleList(Any), byval item2 as any ptr)

'returns 0-based index of item in the list, or -1 if not found
declare function dlist_find (byref this as DoubleList(Any), byval item as any ptr) as integer

'Move along a list n spaces: positive is forward, negative backwards. Returns NULL past either end
declare function dlist_walk (byref this as DoubleList(Any), byval startitem as any ptr, byval n as integer) as any ptr

'the nth item in a list, counting from 0. NULL past end
#define dlist_nth(this, n) dlist_walk((this), NULL, n)

'declare sub dlist_print (byref this as DoubleList(Any))

'------------- Hash Table -------------

'WARNING: don't add strings to this
TYPE HashedItem
  hash as unsigned integer
  'these are internal
  _next as HashedItem ptr       
  _prevp as HashedItem ptr ptr  'pointer to either a _next pointer to this, or table entry
END TYPE
'(notice that unlike DListItem, the HashedItem next/prev point to HashedItem rather than containing objects)

'if we had classes, then this would work well as a template, but it's pointless at the moment
TYPE HashTable
  numitems as integer
  tablesize as unsigned integer
  table as any ptr ptr
  'arguments to comparefunc are (byval as TypeContainingHashedItem ptr, byval as KeyType ptr)
  comparefunc as FnCompare
  memberoffset as integer
END TYPE

'a HashTable compares items by their hash values. If two different keys might hash to the same value, you should
'set the 'comparefunc' function pointer in the HashTable, and pass 'key' to hash_find

declare sub hash_construct(byref this as HashTable, byval itemoffset as integer, byval tablesize as integer = 256)
declare sub hash_destruct(byref this as HashTable)

'Pass an object containing HashedItem member with .hash already set
declare sub hash_add(byref this as HashTable, byval item as any ptr)
declare sub hash_remove(byref this as HashTable, byval item as any ptr)
declare function hash_find(byref this as HashTable, byval hash as unsigned integer, byval key as any ptr = NULL) as any ptr

'to iterate over a hash table, dim state as integer = 0 and object pointer = NULL and
'pass to hash_iter until item = NULL. Returns item.
declare function hash_iter(byref this as HashTable, byref state as integer, byref item as any ptr) as any ptr


'----------------------------------------------------------------------
'                          File Functions

'Constants for findfiles
CONST fileTypeDirectory = 0
CONST fileTypeFile = 1

declare function hash_file(filename as string) as unsigned integer
declare function normalize_path (filename as string) as string
declare function simplify_path (pathname as string) as string
declare function simplify_path_further (pathname as string, fromwhere as string) as string
declare function trim_trailing_slashes (filename as string) as string
declare function trimpath (filename as string) as string
declare function trimfilename (filename as string) as string
declare function trimextension (filename as string) as string
declare function justextension (filename as string) as string
declare function get_path_root (pathname as string) as string
declare function is_absolute_path (sDir as string) as integer
declare function absolute_path (pathname as string) as string
declare function absolute_with_orig_path (file_or_dir as string, byval add_slash as integer = NO) as string
declare function parentdir (pathname as string, byval upamount as integer = 1) as string
declare function anycase (filename as string) as string
declare function escape_filename (filename as string) as string
declare function escape_filenamec cdecl alias "escape_filenamec" (byval filename as zstring ptr) as zstring ptr
declare sub touchfile (filename as string)
declare sub extendfile (byval fh as integer, byval length as integer)
declare sub findfiles (directory as string, namemask as string = "", byval filetype as integer = fileTypeFile, byval findhidden as integer = 0, filelist() as string)
declare sub writeablecopyfile (src as string, dest as string)
declare sub copyfiles (src as string, dest as string, byval copyhidden as integer = 0)
declare function copydirectory (src as string, dest as string, byval copyhidden as integer = -1) as string
declare sub killdir (directory as string, recurse as integer=0)
declare function makedir (directory as string) as integer
declare function safekill (filename as string) as bool
declare function local_file_move(frompath as string, topath as string) as integer
declare function fileisreadable(f as string) as integer
declare function fileiswriteable(f as string) as integer
declare function diriswriteable(d as string) as integer
declare function isfile (n as string) as integer
declare function isdir (sdir as string) as integer
declare function count_directory_size(directory as string) as integer
declare function byte_size_of_file(filename as string) as integer
declare function string_from_first_line_of_file (filename as string) as string
declare function string_from_file (filename as string) as string
declare sub string_to_file (string_to_write as string, filename as string)
declare sub lines_from_file (strarray() as string, filename as string)
declare sub lines_to_file(strarray() as string, filename as string)
declare sub set_tmpdir ()

'slight hackery to get more versatile read function
'(fbc internally defines these functions differently from their actual prototypes,
' leading to conflicts with -gen gcc, so use the fbc prototype)

#IF __FB_VERSION__ < "0.91"
declare function fget alias "fb_FileGet" ( byval fnum as integer, byval pos as uinteger = 0, byval dst as any ptr, byval bytes as integer ) as integer
declare function fput alias "fb_FilePut" ( byval fnum as integer, byval pos as uinteger = 0, byval src as any ptr, byval bytes as integer ) as integer
#ELSE
declare function fget alias "fb_FileGet" ( byval fnum as long, byval pos as long = 0, byval dst as any ptr, byval bytes as size_t ) as long
declare function fput alias "fb_FilePut" ( byval fnum as long, byval pos as long = 0, byval src as any ptr, byval bytes as size_t ) as long
#ENDIF
declare function fgetiob alias "fb_FileGetIOB" ( byval fnum as long, byval pos as long = 0, byval dst as any ptr, byval bytes as size_t, byval bytesread as size_t ptr ) as long


'----------------------------------------------------------------------
'                              Shell

declare function safe_shell (cmd as string) as integer
declare function run_and_get_output(cmd as string, output as string) as integer


'----------------------------------------------------------------------
'                              Math


'crt/float.bi is missing these in FB 0.90.1 and earlier

#IF __FB_VERSION__ < "0.91"
  ' Maximum double
  'FIXME: This is the true value, but produces an assembler error!
  '#define DBL_MAX 1.7976931348623157e+308
  #define DBL_MAX 1.7976931348623154e+308
  ' Maximum single
  #define FLT_MAX 3.40282347e+38F
#ENDIF

UNION XYPair
  TYPE
   x as integer
   y as integer
  END TYPE
  TYPE
   w as integer
   h as integer
  END TYPE
  n(1) as integer
END UNION

UNION RectType
  TYPE
    x as integer
    y as integer
    wide as integer
    high as integer
  END TYPE
  TYPE
    topleft as XYPair
    size as XYPair
  END TYPE
END UNION

'Specify opposite points instead of width and height
TYPE RectPoints
  p1 as XYPair
  p2 as XYPair
END TYPE


declare function bitcount (byval v as unsigned integer) as integer
declare function ceiling (byval n as integer) as integer
declare function bound overload (byval n as integer, byval lowest as integer, byval highest as integer) as integer
declare function bound overload (byval n as longint, byval lowest as longint, byval highest as longint) as longint
declare function bound overload (byval n as double, byval lowest as double, byval highest as double) as double
declare function in_bound (byval n as integer, byval lowest as integer, byval highest as integer) as integer
declare function large overload (byval n1 as integer, byval n2 as integer) as integer
declare function large overload (byval n1 as longint, byval n2 as longint) as longint
declare function large overload (byval n1 as double, byval n2 as double) as double
declare function loopvar overload (byval var as integer, byval min as integer, byval max as integer, byval inc as integer = 1) as integer
declare function loopvar overload (byval value as longint, byval min as longint, byval max as longint, byval inc as longint = 1) as longint
declare function small overload (byval n1 as integer, byval n2 as integer) as integer
declare function small overload (byval n1 as longint, byval n2 as longint) as longint
declare function small overload (byval n1 as double, byval n2 as double) as double
declare sub corners_to_rect (p1 as XYPair, p2 as XYPair, result as RectType)
declare sub corners_to_rect_inclusive (p1 as XYPair, p2 as XYPair, result as RectType)
declare function rect_collide_point overload (r as RectType, p as XYPair) as bool
declare function rect_collide_point overload (r as RectType, byval x as integer, byval y as integer) as bool
declare function rect_collide_point_vertical_chunk overload (r as RectType, p as XYPair, chunk_spacing as integer) as integer
declare function rect_collide_point_vertical_chunk overload (r as RectType, byval x as integer, byval y as integer, chunk_spacing as integer) as integer
declare function rando () as double
declare function randint (byval limit as integer) as integer
declare function range (number as integer, percent as integer) as integer
declare function isnan overload (byval value as double) as integer
declare function isnan overload (byval value as single) as integer
declare function isfinite overload (byval value as double) as integer
declare function isfinite overload (byval value as single) as integer
declare function fuzzythreshold (byval value as double, byval low as double, byval high as double) as double
declare function simple_rand (byref prng_state as uinteger) as double
declare function simple_randint (byref prng_state as uinteger, byval upperbound as integer) as uinteger

'Euclidean modulo (always positive)
#DEFINE POSMOD(dividend, divisor) ((((dividend) MOD (divisor)) + (divisor)) MOD (divisor))

#DEFINE ROT(a,b) ((a shl b) or (a shr (32 - b)))


'----------------------------------------------------------------------
'                         String functions


declare function cstring (s as string) as zstring ptr
declare function blob_to_string (byval str_ptr as zstring ptr, byval str_len as integer) as string
declare function rpad (s as string, pad_char as string, size as integer) as string
declare function lpad (s as string, pad_char as string, size as integer) as string
declare function instr_nth overload (byval start as integer, s as string, substring as string, byval nth as integer) as integer
declare function instr_nth overload (s as string, substring as string, byval nth as integer) as integer
declare function length_matching (s1 as string, s2 as string) as integer
declare function is_int(s as string) as integer
declare function str2int (stri as string, default as integer=0) as integer
declare function rotascii (s as string, o as integer) as string
declare function escape_string(s as string, chars as string) as string
declare function replacestr (buffer as string, replacewhat as string, withwhat as string, byval maxtimes as integer = -1) as integer
declare function exclude (s as string, x as string) as string
declare function exclusive (s as string, x as string) as string
declare function scancodename (byval k as integer) as string
declare function special_char_sanitize(s as string) as string
declare function sign_string(n as integer, neg_str as string, zero_str as string, pos_str as string) as string
declare function iif_string(byval condition as integer, s1 as string, s2 as string) as string
declare function zero_default(n as integer, zerocaption as string="default", displayoffset as integer = 0) as string
declare function blank_default(s as string, blankcaption as string="default") as string
declare Function wordwrap(z as string, byval width as integer, sep as string = chr(10)) as string

declare sub split(in as string, ret() as string, sep as string = chr(10))
declare sub split_line_positions(original_text as string, lines() as string, line_starts() as integer, sep as string = chr(10))


'----------------------------------------------------------------------
'                       Commandline processing


' A function to handle commandline options, e.g. gfx_setoption
type FnSetOption as function(opt as string, arg as string) as integer

declare function commandline_flag(opt as string) as bool
declare sub processcommandline(cmdline_args() as string, opt_handler as FnSetOption, args_file as string = "")


'----------------------------------------------------------------------
'                        ini file read/write

declare function read_ini_int overload (ini() as string, key as string, byval default as integer=0) as integer
declare function read_ini_int overload (ini_filename as string, key as string, byval default as integer=0) as integer
declare sub write_ini_value overload (ini() as string, key as string, value as integer)
declare sub write_ini_value overload (ini_filename as string, key as string, value as integer)
declare function ini_key_match(key as string, s as string) as bool
declare function ini_value_int (s as string, byval default as integer=0) as integer


'----------------------------------------------------------------------
'                              Other


declare function days_since_datestr(datestr as string) as integer
declare sub flusharray (array() as integer, byval size as integer=-1, byval value as integer=0)
declare sub sort_integers_indices(indices() as integer, byval start as integer ptr, byval number as integer = 0, byval stride as integer = SIZEOF(integer))
declare sub qsort_integers_indices(indices() as integer, byval start as integer ptr, byval number as integer, byval stride as integer)
declare sub qsort_strings_indices(indices() as integer, byval start as string ptr, byval number as integer, byval stride as integer)
declare function integer_compare cdecl (byval a as integer ptr, byval b as integer ptr) as long
declare function string_compare cdecl (byval a as string ptr, byval b as string ptr) as long
declare sub invert_permutation overload (indices() as integer, inverse() as integer)
declare sub invert_permutation overload (indices() as integer)

declare function strhash (hstr as string) as unsigned integer
declare function starts_with(s as string, prefix as string) as integer
declare function ends_with(s as string, suffix as string) as integer

declare function string_index_in_array(s as string, a() as string, notfound as integer=-1) as integer

#macro debug_if_slow(starttime, seconds, extrainfo)
  IF TIMER > starttime + seconds THEN
    debug __FUNCTION__ "(" & extrainfo & ") took " & CINT((TIMER - starttime) * 1000) & "ms"
  END IF
#endmacro

'----------------------------------------------------------------------
'                        Old allmodex functions


DECLARE SUB xbload (f as string, array() as integer, e as string)
DECLARE SUB xbsave (f as string, array() as integer, bsize as integer)
DECLARE SUB setbit (b() as integer, byval w as integer, byval b as integer, byval v as integer)
DECLARE FUNCTION readbit (b() as integer, byval w as integer, byval b as integer) as integer
DECLARE SUB array2str (arr() as integer, byval o as integer, s as string)
DECLARE SUB str2array (s as string, arr() as integer, byval o as integer)


'----------------------------------------------------------------------
'              Globals (think twice before adding more)


EXTERN tmpdir as string
'Ideally would not be in this module
EXTERN orig_dir as string


#ENDIF
