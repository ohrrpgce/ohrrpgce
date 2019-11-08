'OHRRPGCE - util.bi
'(C) Copyright 1997-2006 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#IFNDEF UTIL_BI
#DEFINE UTIL_BI

#include "config.bi"
#include "crt/stddef.bi"
#include "file.bi"   'FB header, for FILELEN
#include "cutil.bi"  'To make strprintf always available

#include "lib/sha1.bi"
#include "os.bi"
#include "vector.bi"
#include "const.bi"  'For DirNum

'#ifdef __FB_ANDROID__
'#define DEBUG_FILE_IO
'#endif


declare sub lowlevel_init()
declare sub setup_fb_error_handler()
declare sub remove_fb_error_handler()
extern "C"
  declare sub fb_error_hook (message as const zstring ptr)
end extern

'Ensure that all executables call lowlevel_init at the top of main().  Want this
'in main() so COMMAND is initialised and the module constructor ordering doesn't
'matter.
#IFDEF __FB_MAIN__
  lowlevel_init
#ENDIF

'----------------------------------------------------------------------
'                           Macro utilities

#define cvar(v, x) cast(typeof(v), x)
#define canyptr(x) cast(any ptr, cast(intptr_t, x))
#define cintptr32(x) cast(integer, cast(intptr_t, x))  'Cast a ptr to a 32 bit int

#macro EXIT_MSG_IF(condition, errlvl, message, retwhat...)
	if condition then
		debugc errlvl, __FUNCTION__ ": " & message
		return retwhat  'If retwhat isn't given, just "return"
	end if
#endmacro

'Log an error message and exit (with optional return value) if 'condition' is true
#define FAIL_IF(condition, message, retwhat...)  EXIT_MSG_IF(condition, errError, message, retwhat)
'Show an error message and exit (with optional return value)  if 'condition' is true
#define ERROR_IF(condition, message, retwhat...) EXIT_MSG_IF(condition, errShowError, message, retwhat)
#define BUG_IF(condition, message, retwhat...) EXIT_MSG_IF(condition, errShowBug, message, retwhat)

'Used to dereference a ptr only if not NULL... a bit yuck
'E.g. "IF_PTR(ptr_to_foo)->widgets += 1"
#define IF_PTR(arg)  if arg then arg


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


TYPE IntStrPair
  i as integer
  s as string
END TYPE

declare function a_find overload (array() as string,  value as string,  notfound as integer = -1) as integer
declare function a_findcasei (array() as string,  value as string,  notfound as integer = -1) as integer
declare function a_find overload (array() as integer, value as integer, notfound as integer = -1) as integer
declare function a_find overload (array() as IntStrPair, value as integer, notfound as integer = -1) as integer
declare function a_find overload (array() as IntStrPair, value as string, notfound as integer = -1) as integer
declare sub a_shuffle_to_end overload (array() as integer, which as integer)
declare sub a_shuffle_to_end overload (array() as string, which as integer)

'These act on *dynamic length* destination arrays only! Static arrays will segfault!
declare sub a_append overload (array() as string,  value as zstring ptr)
declare sub a_append overload (array() as integer, value as integer)
declare sub a_append overload (array() as IntStrPair, byval k as integer, s as zstring ptr)
declare sub a_insert overload (array() as string,  pos as integer, value as string)
declare sub a_insert overload (array() as integer, pos as integer, value as integer)
declare sub a_pop overload (array() as string,  which as integer = -&h7FFFFFFF)
declare sub a_pop overload (array() as integer, which as integer = -&h7FFFFFFF)
declare function a_remove overload (array() as string,  value as string)  as integer
declare function a_remove overload (array() as integer, value as integer) as integer
declare sub a_copy overload (fromarray() as integer, toarray() as integer)
declare sub a_copy overload (fromarray() as string,  toarray() as string)

/'
declare sub a_exclude (() as integer, array() as integer)
declare sub a_sort (dest() as integer, src() as integer)
'/


' This macro removes the i-th element from a 1-D array by shuffling it to the end
' and redimming. Has to be a macro since FB doesn't have templates,
' and can't be named a_remove as that clashes.
#MACRO a_any_remove(array, which)
  FOR _aidx as integer = which TO UBOUND(array) - 1
    SWAP array(_aidx), array(_aidx + 1)
  NEXT
  REDIM PRESERVE array(LBOUND(array) TO UBOUND(array) - 1) 'FB now supports zero-length arrays
#ENDMACRO


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

'Old allmodex stack (FIXME: get rid of this, can be directly replaced with the above)
declare sub setupstack ()
declare sub pushdw (byval word as integer)
declare function popdw () as integer
declare sub releasestack ()
declare function stackpos () as integer
declare function readstackdw (byval off as integer) as integer


'------------ String Cache ------------

'See also the intstr_* functions.
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

'----------------------------------------------------------------------
'                             HashTable

Type HashBucketItem
  hash as integer
  key as any ptr              'If the table uses integer keys, 'key' is NULL
  Union
    value as any ptr
    value_int as integer
  end Union
end Type

DECLARE_VECTOR_OF_TYPE(HashBucketItem, HashBucketItem)

'This is a multimap (it allows duplicate keys).
'Keys and values can be either integers or pointers to any type.
'The table optionally will make copies of keys or values instead of just treating them
'as opaque pointers - to do that you need to either use the construct() overload which provides type
'information, or manually set some of the key_* or value_* members.
'Use DECLARE_VECTOR_OF_TYPE() and DEFINE_CUSTOM_VECTOR_TYPE() to declare/create a TypeTable for a UDT.
'(But note that the default hash function for a UDT just hashes its contents, which will not work
'if the compiler adds padding to the UDT! And it might not be what you want if the UDT contains pointers,
'including strings.)
'You need to ensure that you don't mix different key/value types in the same table, and that you use the
'correct overloads, because in most cases that isn't checked.

Type HashTable
  'Public members: (you may wish to manually set some function ptrs instead of providing TypeTables)
  numitems as integer
  key_compare as FnCompare    'If NULL, items are compared by hash value only, and keys should be NULL.
                              'Arguments to comparefunc are (byval as KeyType ptr, byval as KeyType ptr)
  key_hash as FnHash
  key_copy as FnCopy          'May be NULL
  key_delete as FnDelete      'May be NULL
  key_length as integer       'May be 0. Needed only if key_hash is NULL and not using integer keys
  key_is_integer as bool      'Whether key_type is integer, rather than using ptrs as keys
  value_copy as FnCopy        'May be NULL
  value_delete as FnDelete    'May be NULL
  value_is_string as bool     'value_type is type_table(string)
  value_is_zstring as bool    'value_type is type_table(zstring)

  'Internal members:
  tablesize as uinteger       'Length of the 'table' array
  table as HashBucketItem vector ptr  'An array of hash table buckets, each a vector or NULL


  'Use this constructor if either keys and values are just integers or opaque ptrs,
  'or if you want to set the .key_* and .value_* members yourself.
  'E.g. set tbl.value_delete = @DEALLOCATE to free values when they are removed.
  'tablesize should be manually adjusted to something suitable, because it does not grow automatically.
  'If there are N items in the table, then time to lookup a key will be on average N/tablesize.
  declare sub construct(tablesize as integer = 31)

  'Construct a HashTable with information about the types, if you want to store non-opaque pointers.
  'Pass a type info struct like 'type_table(T)' if you want the key/value to be a T ptr.
  'For example you should pass type_table(string) or type_table(zstring) to store FB or C strings;
  'do NOT use type_table(zstring_ptr)!
  'type_table(integer) as key or value is a special case, as integers are stored directly instead of
  'pointers to integers. type_table(any_ptr) (not type_table(any)) is also a special case, and means
  'an opaque pointer.
  'If you want the keys or values to be copied with NEW and freed with DELETE when added/removed
  'from the table, pass copy_and_delete_{keys,values} = YES. (Hint: you probably want that for keys
  'and values which are 'string's. Otherwise, you are responsible for allocating and deleting them.
  'These args have no effect when key/value_type is integer, and must not be used with any_ptr.
  declare sub construct(tablesize as integer = 31, key_type as TypeTable, copy_and_delete_keys as bool, value_type as TypeTable, copy_and_delete_values as bool)

  'Frees all memory. construct() can be called afterwards, with any types.
  'Optional, since the automatic destructor calls destruct().
  declare sub destruct()
  declare destructor()

  'Whether construct() has been called
  declare function constructed() as bool

  'Remove and everything in the table and call the dtors, if provided
  declare sub clear()

  'Provide either a hash, or both a key and its hash. Both key and value may be NULL.
  'However if the value is NULL you can't distinguish between NULL values and keys that aren't present!
  'NOTE: if the key already exists, it will be duplicated! Use set() instead to overwrite.
  declare sub add(hash as integer, value as any ptr, _key as any ptr = NULL)  'Ignore _key
  declare sub add(hash as integer, value as integer)
  declare sub add(key as any ptr, value as any ptr)
  declare sub add(key as any ptr, value as integer)

  'Change the value of (the first instance of) a key, or add it if it's not in the table yet.
  declare sub set(hash as integer, value as any ptr, _key as any ptr = NULL)  'Ignore _key
  declare sub set(hash as integer, value as integer)
  declare sub set(key as any ptr, value as any ptr)
  declare sub set(key as any ptr, value as integer)

  'Returns the value for (the first instance of) a key, or default if not present
  declare function get(hash as integer, default as any ptr = NULL, _key as any ptr = NULL) as any ptr  'Ignore _key
  declare function get(key as any ptr, default as any ptr = 0) as any ptr
  'Convenience functions, which cast the return value of .get()
  declare function get_int(hash as integer, default as integer = 0) as integer
  declare function get_int(key as any ptr, default as integer = 0) as integer
  declare function get_str(hash as integer, default as zstring ptr = @"", _key as any ptr = NULL) as string  'Ignore _key
  declare function get_str(key as any ptr, default as zstring ptr = @"") as string

  'Remove (the first instance of) an item and call key/value dtors, if provided.
  'Returns YES if it was found, NO otherwise
  declare function remove(hash as integer, _key as any ptr = NULL) as bool  'Ignore _key
  declare function remove(key as any ptr) as bool

  'To iterate over a hash table, dim state as uinteger = 0 and prev_value = NULL and
  'pass to iter until value = NULL. Returns values, and optionally keys ('key' set byref).
  'Adding items to the table while iterating is OK; they may or may not get iterated over. Removing items while
  'iterating is OK, provided that prev_value isn't removed and that values are unique.
  declare function iter(byref state as uinteger, prev_value as any ptr, byref key as any ptr = NULL) as any ptr

  'Returns either an integer vector (if this.key_is_integer) or else an any ptr vector
  '(you should store the result in an appropriate variable!)
  'Unsorted. Keys are not copied, so pointers will become invalid if you free the key
  '(e.g. remove from the table if it's set to delete keys)
  declare function keys() as any ptr vector

  'Implemented, but I don't think this is needed.
  'declare function hashes() as integer vector

  'Unsorted contents of the table. Only a shallow copy of key and value ptrs, so
  'make sure they haven't been deleted yet and you don't double-free!  You must
  'free the vector.
  'Duplicate keys appear in the order they were added.
  declare function items() as HashBucketItem vector

  'Like .items(), but sorted either using key_compare, or by hash.
  'Duplicate keys do NOT appear in the order they were added.
  'Not thread-safe!
  declare function items_sorted() as HashBucketItem vector

  'For internal use, mostly. Get the hash of a key ptr.
  declare function hash_key(key as any ptr) as integer
end Type


'This convenience class is a HashTable with 'string' key, and has method overloads for that.
Type StrHashTable Extends HashTable
  'REMEMBER: almost certainly you want copy_and_delete_values=YES if the value type is string
  declare sub construct(tablesize as integer = 31, value_type as TypeTable = type_table(any_ptr), copy_and_delete_values as bool)
  declare sub add(key as string, value as any ptr)
  declare sub add(key as string, value as integer)
  declare sub add(key as string, value as string)
  declare sub set(key as string, value as any ptr)
  declare sub set(key as string, value as integer)
  declare sub set(key as string, value as string)
  declare function get(key as string, default as any ptr = NULL) as any ptr
  declare function get_int(key as string, default as integer = 0) as integer
  declare function get_str(key as string, default as zstring ptr = @"") as string
  declare function remove(key as string) as bool
end Type


'----------------------------------------------------------------------
'                         Hash Functions

declare sub file_hash_SHA1 overload (filename as string, result_out as SHA160 ptr)
declare sub file_hash_SHA1 overload (fh as integer, result_out as SHA160 ptr)
declare function file_hash64 overload (filename as string) as ulongint
declare function file_hash64 overload (fh as integer) as ulongint
declare function SHA1_to_string(hash as SHA160) as string
declare function strhash (hstr as string) as unsigned integer

'----------------------------------------------------------------------
'                      Path and File Functions

declare function join_path (path1 as string, path2 as string) as string
declare function normalize_path (filename as string) as string
declare function simplify_path (pathname as string) as string
declare function simplify_path_further (pathname as string, fromwhere as string = "") as string
declare function paths_equal(path1 as string, path2 as string) as bool
declare function add_trailing_slash (dirname as string) as string
declare function trim_trailing_slashes (filename as string) as string
declare function trimpath (filename as string) as string
declare function trimfilename (filename as string) as string
declare function trimextension (filename as string) as string
declare function justextension (filename as string) as string
declare function get_path_root (pathname as string) as string
declare function trim_path_root (pathname as string) as string
declare function is_absolute_path (sDir as string) as bool
declare function is_possibly_absolute_path (sDir as string) as bool
declare function absolute_path (pathname as string) as string
declare function absolute_with_orig_path (file_or_dir as string, byval add_slash as bool = NO) as string
declare function parentdir (pathname as string, byval upamount as integer = 1) as string
declare function anycase (filename as string) as string
declare function escape_filename (filename as string) as string
declare function escape_filenamec cdecl alias "escape_filenamec" (byval filename as zstring ptr) as zstring ptr
declare function fixfilename (filename as string) as string
declare function url_hostname (url as string) as string
declare function decode_filename (filename as string) as string
declare sub touchfile (filename as string)
declare sub extendfile (byval fh as integer, byval length as integer)
declare sub findfiles (directory as string, namemask as string = "", filetype as FileTypeEnum = fileTypeFile, findhidden as bool = NO, filelist() as string)
declare function find_file_portably (path as string) as string
declare function find_file_anycase (path as string, file_type as FileTypeEnum = fileTypeFile) as string
declare function writeablecopyfile (src as string, dest as string) as bool
declare sub copyfiles (src as string, dest as string, copyhidden as bool = NO, lowercase as bool = NO)
declare function copydirectory (src as string, dest as string, byval copyhidden as bool = YES) as string
declare sub killdir (directory as string, recurse as bool = NO)
declare function makedir (directory as string) as integer
declare function safekill (filename as string) as bool
declare function killfile (filename as string) as bool
declare function local_file_move(frompath as string, topath as string) as bool
declare function fileisreadable(filename as string) as bool
declare function fileiswriteable(filename as string) as bool
declare function diriswriteable(filename as string) as bool
declare function isfile(filename as string) as bool
declare function real_isfile(filename as string) as bool
declare function is_not_file(filename as string) as bool
declare function isdir (filename as string) as bool
declare function count_directory_size(directory as string) as integer

declare function read_file (filename as string) as string
declare function string_from_first_line_of_file (filename as string) as string
declare function string_from_file (filename as string) as string
declare sub string_to_file (string_to_write as string, filename as string)
declare function lines_from_file (strarray() as string, filename as string, expect_exists as bool = YES) as bool
declare function lines_to_file(strarray() as string, filename as string, lineending as string = !"\n") as bool

declare function get_tmpdir () as string

'Slight hackery to get more versatile read function
'Earlier versions of fbc like 0.90 internally defined these functions differently from their actual prototypes,
'leading to conflicts with -gen gcc, so use the fbc prototype.
'TODO: The only reason to support -gen gcc in FB 0.90 is because the android branch of our FB fork is currently based on it.

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

declare function safe_shell (cmd as string, timeout as double = 5., log_it as bool = YES) as integer
declare function run_and_get_output(cmd as string, byref stdout_s as string, byref stderr_s as string = "", log_it as bool = YES) as integer


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

' Don't use this directly.
TYPE XYSimple
  x as integer
  y as integer
END TYPE

UNION XYPair
  TYPE
   x as integer
   y as integer
  END TYPE
  TYPE
   w as integer
   h as integer
  END TYPE
  TYPE
   wide as integer
   high as integer
  END TYPE
  n(1) as integer
  xy as XYSimple  'This is to allow LET(posx,posy) = pos.xy
  wh as XYSimple

  DECLARE OPERATOR += (rhs as XYPair)
  DECLARE OPERATOR CAST () as string
  DECLARE OPERATOR LET (value as integer)
END UNION
DECLARE OPERATOR =  (lhs as XYPair, rhs as XYPair) as bool
DECLARE OPERATOR =  (lhs as XYPair, rhs as integer) as bool
DECLARE OPERATOR <> (lhs as XYPair, rhs as XYPair) as bool
DECLARE OPERATOR <> (lhs as XYPair, rhs as integer) as bool
DECLARE OPERATOR <  (lhs as XYPair, rhs as XYPair) as bool
DECLARE OPERATOR <  (lhs as XYPair, rhs as integer) as bool
DECLARE OPERATOR <= (lhs as XYPair, rhs as XYPair) as bool
DECLARE OPERATOR <= (lhs as XYPair, rhs as integer) as bool
DECLARE OPERATOR >  (lhs as XYPair, rhs as XYPair) as bool
DECLARE OPERATOR >  (lhs as XYPair, rhs as integer) as bool
DECLARE OPERATOR >= (lhs as XYPair, rhs as XYPair) as bool
DECLARE OPERATOR >= (lhs as XYPair, rhs as integer) as bool
DECLARE OPERATOR + (lhs as XYPair, rhs as XYPair) as XYPair
DECLARE OPERATOR + (lhs as XYPair, rhs as integer) as XYPair
DECLARE OPERATOR - (lhs as XYPair, rhs as XYPair) as XYPair
DECLARE OPERATOR - (lhs as XYPair, rhs as integer) as XYPair
DECLARE OPERATOR * (lhs as XYPair, rhs as XYPair) as XYPair
DECLARE OPERATOR * (lhs as XYPair, rhs as integer) as XYPair
DECLARE OPERATOR * (lhs as XYPair, rhs as double) as XYPair
DECLARE OPERATOR \ (lhs as XYPair, rhs as XYPair) as XYPair
DECLARE OPERATOR \ (lhs as XYPair, rhs as integer) as XYPair
DECLARE OPERATOR / (lhs as XYPair, rhs as XYPair) as XYPair
DECLARE OPERATOR / (lhs as XYPair, rhs as double) as XYPair
DECLARE OPERATOR ABS (lhs as XYPair) as XYPair
DECLARE OPERATOR MOD (lhs as XYPair, rhs as XYPair) as XYPair
DECLARE OPERATOR MOD (lhs as XYPair, rhs as integer) as XYPair
DECLARE OPERATOR - (lhs as XYPair) as XYPair

#DEFINE XY(x, y) TYPE<XYPair>(x, y)

'This allows us to create vectors of XYPair using vector.bas
DECLARE_VECTOR_OF_TYPE(XYPair, XYPair)

DECLARE FUNCTION xypair_direction (v as XYPair, byval axis as integer, byval default as DirNum = dirNone) as DirNum
DECLARE FUNCTION xypair_to_direction (v as XYPair) as DirNum
DECLARE SUB xypair_move (v as XYPair, byval direction as integer, byval amount as integer = 1)
DECLARE FUNCTION xypair_manhattan_distance(v1 as XYPair, v2 as XYPair) as integer
DECLARE FUNCTION xypair_distance_squared(v1 as XYPair, v2 as XYPair) as integer

DECLARE FUNCTION dirX(dirn as DirNum, dist as integer = 1) as integer
DECLARE FUNCTION dirY(dirn as DirNum, dist as integer = 1) as integer

UNION XYZTriple
  TYPE
   x as integer
   y as integer
   z as integer
  END TYPE
  TYPE
   w as integer
   h as integer
   depth as integer
  END TYPE
  n(2) as integer
END UNION

UNION RectType
  TYPE
    x as integer
    y as integer
    UNION
      TYPE
        wide as integer
        high as integer
      END TYPE
      TYPE
        w as integer
        h as integer
      END TYPE
    END UNION
  END TYPE
  TYPE
    topleft as XYPair
    size as XYPair
  END TYPE
  TYPE
    xy as XYPair
    wh as XYPair
  END TYPE

  DECLARE OPERATOR CAST () as string
END UNION

#DEFINE XYWH(x, y, w, h)  TYPE<RectType>(x, y, w, h)   'Just TYPE will nearly always do too.
#DEFINE XY_WH(xy, wh)  TYPE<RectType>((xy).x, (xy).y, (wh).w, (wh).h)

DECLARE OPERATOR = (lhs as RectType, rhs as RectType) as bool
DECLARE OPERATOR <> (lhs as RectType, rhs as RectType) as bool
DECLARE OPERATOR + (lhs as RectType, rhs as XYPair) as RectType
DECLARE OPERATOR - (lhs as RectType, rhs as XYPair) as RectType
DECLARE OPERATOR * (lhs as RectType, rhs as integer) as RectType

'Specify opposite points instead of width and height
TYPE RectPoints
  p1 as XYPair
  p2 as XYPair
END TYPE

'Used for menu and slice anchor points and slice align points
'Not to be confused with the rCenter, ancCenter, etc, constants!
Enum AlignType
  alignLeft = 0
  alignTop = 0
  alignMiddle = 1
  alignCenter = 1
  alignNone = 1   'Only used by slice clamp options
  alignRight = 2
  alignBottom = 2
End Enum

CONST _rFactor = 10100000
' Max amount that can be added to/subtracted from an r* or anc* constant
CONST _rMargin = 50000

' Relative coordinates, used by relative_pos() and various functions
' such as edgebox, printstr, rectangle.
' Not to be confused with the alignCenter, etc, constants! These
' are not usable as slice or menu positions.
' You can add together at most one r*, one anc* and one show* constant...
' with the exception that you can assume rCenter + rCenter = rRight, and rLeft = 0.
' Note that INT_MAX, 99999, 999999, 9999999 (other large values close to power of 10)
' get passed through unchanged, without being interpreted as having any flags.

' r* constants say which edge of the screen this RelPos position is relative to.
CONST rLeft =   0
CONST rTop =    0
CONST rCenter = _rFactor * 3
CONST rMiddle = _rFactor * 3
CONST rHalf   = _rFactor * 3  ' Use this as an object size to mean "half the size of the dest page"
CONST rRight =  _rFactor * 6
CONST rBottom = _rFactor * 6
CONST rWidth =  _rFactor * 6  ' Use this as an object width to mean "width of the dest page"
CONST rHeight = _rFactor * 6  ' Use this as an object height to mean "height of the dest page"
' anc* constants say which edge of the object this RelPos gives the position of
CONST ancTop =    0
CONST ancLeft =   0
CONST ancCenter = _rFactor * 1
CONST ancMiddle = _rFactor * 1
CONST ancBottom = _rFactor * 2
CONST ancRight =  _rFactor * 2
' show* constants shift if over the screen edge, so the left-most or right-most part of the object is visible
CONST showLeft =  _rFactor * 9
CONST showTop  =  _rFactor * 9
CONST showRight = _rFactor * 18
CONST showBottom = _rFactor * 18
' placements; used especially for printstr, etc.
CONST pLeft =          rLeft   + ancLeft  ' =0
CONST pTop =           rTop    + ancTop   ' =0
CONST pCentered =      rCenter + ancCenter
CONST pCenteredLeft =  rCenter + ancCenter + showLeft  'AKA xstring
CONST pCenteredRight = rCenter + ancCenter + showRight 'AKA xstringright
CONST pBottom =        rBottom + ancBottom
CONST pRight =         rRight  + ancRight

' Type of a relative coordinate, use this to indicate whether a function supports them!
TYPE RelPos as integer

declare function relative_pos (pos as RelPos, pagewidth as integer, objwidth as integer = 0) as integer
declare sub RelPos_decode (pos as RelPos, byref offset as integer, byref align as AlignType, byref anchor as AlignType, byref show as AlignType)

declare function bitcount (byval v as unsigned integer) as integer
declare function ceiling (byval n as integer) as integer
declare function bound overload (byval n as integer, byval lowest as integer, byval highest as integer) as integer
declare function bound overload (byval n as longint, byval lowest as longint, byval highest as longint) as longint
declare function bound overload (byval n as double, byval lowest as double, byval highest as double) as double
declare function in_bound (byval n as integer, byval lowest as integer, byval highest as integer) as integer
declare sub clamp_value (byref value as integer, byval min as integer, byval max as integer, argname as string)
declare function large overload (byval n1 as integer, byval n2 as integer) as integer
declare function large overload (byval n1 as longint, byval n2 as longint) as longint
declare function large overload (byval n1 as double, byval n2 as double) as double
declare sub loopvar overload (byref value as integer, min as integer, max as integer, inc as integer = 1)
declare sub loopvar overload (byref value as longint, min as longint, max as longint, inc as longint = 1)
declare function small overload (byval n1 as integer, byval n2 as integer) as integer
declare function small overload (byval n1 as longint, byval n2 as longint) as longint
declare function small overload (byval n1 as double, byval n2 as double) as double
declare sub corners_to_rect (p1 as XYPair, p2 as XYPair, result as RectType)
declare sub corners_to_rect_inclusive (p1 as XYPair, p2 as XYPair, result as RectType)
declare function rect_collide_point (r as RectType, p as XYPair) as bool
declare function rect_collide_rect (r1 as RectType, r2 as RectType) as bool
declare function rect_collide_point_vertical_chunk (r as RectType, p as XYPair, chunk_spacing as integer) as integer
declare sub reseed_prng (seed as double)
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

Enum clipDir
 clipNone
 clipLeft
 clipRight
End Enum

type FnReplacement as function(original as string, arg as any ptr) as string

declare function cstring (s as string) as zstring ptr
declare function copy_zstring (str_ptr as zstring ptr) as zstring ptr
declare function blob_to_string (byval str_ptr as zstring ptr, byval str_len as integer) as string
declare function utf8_to_latin1(utf8string as ustring) as string
declare function latin1_to_utf8(s as string) as ustring

declare function rpad (s as string, pad_char as zstring ptr = @" ", size as integer, clip as clipDir = clipNone) as string
declare function lpad (s as string, pad_char as zstring ptr = @" ", size as integer, clip as clipDir = clipNone) as string
declare function rlpad (s as string, pad_char as zstring ptr = @" ", pad_right as integer, pad_left as integer, clip as clipDir = clipNone) as string
declare function instr_nth overload (byval start as integer, s as string, substring as string, byval nth as integer) as integer
declare function instr_nth overload (s as string, substring as string, byval nth as integer) as integer
declare function length_matching (s1 as string, s2 as string) as integer
declare function skip_over (text as string, byref idx as integer, tok as zstring ptr, maxskips as integer = -1) as integer
declare function parse_int (stri as zstring ptr, ret as integer ptr=NULL, strict as bool=NO) as bool
declare function str2int (stri as zstring ptr, default as integer=0, strict as bool=NO) as integer
declare function split_str_int(z as zstring ptr, byref action as string, byref arg as integer) as bool
declare function str2bool(q as string, default as integer = NO) as integer
declare function rotascii (s as string, o as integer) as string
declare function titlecase(word as string) as string
declare function escape_string(s as string, chars as string) as string
declare function replacestr overload (byref buffer as string, replacewhat as string, replacefunc as FnReplacement, arg as any ptr, maxtimes as integer = -1, caseinsensitive as bool = NO) as integer
declare function replacestr overload (byref buffer as string, replacewhat as string, withwhat as string, maxtimes as integer = -1, caseinsensitive as bool = NO) as integer
declare function normalize_newlines (buffer as string, newline as string = LINE_END) as string
declare function exclude (s as string, x as string) as string
declare function exclusive (s as string, x as string) as string
declare function scancodename (byval k as KBScancode) as string
declare function special_char_sanitize(s as string) as string

declare function sign_string (n as integer, neg_str as string, zero_str as string, pos_str as string) as string
declare function zero_default (n as integer, default_caption as string="default") as string
declare function defaultint (n as integer, default_caption as string="default", default_value as integer=-1) as string
declare function blank_default (s as string, blankcaption as string="[default]") as string
declare function caption_or_int (captions() as string, n as integer) as string
declare function safe_caption (caption_array() as string, index as integer, description as string = "value") as string
declare function safe_captionz (caption_array() as zstring ptr, index as integer, description as string = "value") as string

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


declare function read_ini_str overload (ini() as string, key as string, default as string="") as string
declare function read_ini_str overload (ini_filename as string, key as string, default as string="") as string
declare function read_ini_int overload (ini_filename as string, key as string, default as integer=0) as integer
declare function read_ini_double overload (ini_filename as string, key as string, default as double=0.) as double
declare sub write_ini_value overload (ini() as string, key as string, value as string)
declare sub write_ini_value overload (ini_filename as string, key as string, value as string)
declare sub write_ini_value overload (ini_filename as string, key as string, value as integer)
declare sub write_ini_value overload (ini_filename as string, key as string, value as double)
declare function ini_key_match(text as string, key as string, byref value as string = "") as bool


'----------------------------------------------------------------------
'                              Other


declare function days_since_datestr(datestr as string) as integer
declare function format_duration(length as double, decimal_places as integer = 1) as string

declare sub flusharray (array() as integer, byval size as integer=-1, byval value as integer=0)
declare sub sort_integers_indices(indices() as integer, byval start as integer ptr, byval number as integer = 0, byval stride as integer = SIZEOF(integer))
declare sub qsort_integers_indices(indices() as integer, byval start as integer ptr, byval number as integer, byval stride as integer)
declare sub qsort_strings_indices(indices() as integer, byval start as string ptr, byval number as integer, byval stride as integer)
declare function integer_compare cdecl (byval a as integer ptr, byval b as integer ptr) as long
declare function string_compare cdecl (a as string, b as string) as integer
declare function numeric_string_compare cdecl (a as string, b as string, case_insen as bool = NO) as integer
declare sub invert_permutation overload (indices() as integer, inverse() as integer)
declare sub invert_permutation overload (indices() as integer)

declare function starts_with(s as string, prefix as string) as bool
declare function ends_with(s as string, suffix as string) as bool

declare function readkey () as string

#macro debug_if_slow(starttime, seconds, extrainfo)
  IF TIMER > starttime + seconds THEN _
    debuginfo __FUNCTION__ "(" & extrainfo & ") took " & CINT((TIMER - starttime) * 1000) & "ms"
#endmacro

'----------------------------------------------------------------------
'                        Old allmodex functions


DECLARE SUB xbload (f as string, array() as integer, e as string)
DECLARE SUB xbsave (f as string, array() as integer, bsize as integer)

DECLARE SUB setbitmask (byref bitsets as integer, bitmask as integer, value as bool)
DECLARE SUB setbit (bitwords() as integer, wordnum as integer, bitnum as integer, value as bool)
DECLARE FUNCTION readbit (bitwords() as integer, wordnum as integer = 0, bitnum as integer) as integer
DECLARE FUNCTION xreadbit (bitwords() as integer, bitnum as integer, wordoffset as integer=0) as bool

DECLARE SUB array2str (arr() as integer, byval o as integer, s as string)
DECLARE SUB str2array (s as string, arr() as integer, byval o as integer)


'----------------------------------------------------------------------
'              Globals (think twice before adding more)


EXTERN tmpdir as string
'Ideally would not be in this module
EXTERN orig_dir as string
EXTERN exename as string
EXTERN "C"
EXTERN program_start_timer as double
END EXTERN

#ENDIF
