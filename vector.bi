'OHRRPGCE - Vectors
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
'
'Arrays in FB are a disaster. This is a completely separate array implementation,
'written in C (see array.c).  These arrays are called vectors (as in C++) to avoid
'confusion. They are resizeable 1-D arrays of homogenous type. Each has a header,
'which contains a pointer to a 'type table'.
'
'See http://rpg.hamsterrepublic.com/ohrrpgce/Vectors

#IFNDEF VECTOR_BI
#DEFINE VECTOR_BI

#include "config.bi"

'For each type from which you want to be able to form vectors from, add a 
'DECLARE_VECTOR_OF_TYPE line somewhere (probably a header, make it this one
'only if the type is a primitive one visible here), and a DEFINE_VECTOR_OF_TYPE
'or DEFINE_VECTOR_OF_CLASS or DEFINE_CUSTOM_VECTOR_TYPE line in some module
'(vector.bas for anything declared here). If it is a vector of vectors,
'instead use DEFINE_VECTOR_VECTOR_OF


''''''''''''''''''''''''''''' Type Declarations ''''''''''''''''''''''''''''''''


#DEFINE vector ptr

TYPE FnCtor as sub cdecl (byval as any ptr)
TYPE FnCopyCtor as sub cdecl (byval dest as any ptr, byval src as any ptr)
TYPE FnDtor as sub cdecl (byval as any ptr)
TYPE FnCopy as function cdecl (byval as any ptr) as any ptr  'Allocate and initialise a copy
TYPE FnDelete as sub cdecl (byval as any ptr)  'Destruct and delete
TYPE FnStr as function cdecl (byval as any ptr) as string
TYPE FnCompare as function cdecl (byval as const any ptr, byval as const any ptr) as int32
TYPE FnHash as function cdecl (byval as any ptr) as uinteger

'Not used
ENUM 'PassConvention
  PASS_BYVAL
  PASS_BYREF
  PASS_ZSTRING
END ENUM
TYPE PassConvention as uint32

TYPE TypeTable
  element_len as uint32
  passtype as PassConvention  'Not used
  ctor as FnCtor
  copyctor as FnCopyCtor
  dtor as FnDtor
  copy as FnCopy
  _delete as FnDelete
  comp as FnCompare
  inequal as FnCompare
  hash as FnHash
  tostr as FnStr
  name as zstring ptr  'For debugging
END TYPE

#DEFINE type_table(TID) type_tbl_##TID

''' array.c declarations (internal, do no use!)

extern "C"

'This one function in array.c is special: it needs to be passed a TypeTable which
'depends on the overload. Therefore, we wrap it.
declare sub array_new (byref this as any vector, byval length as int32, byval reserve as int32, byval tbl as TypeTable ptr)

'Undocumented, if you need this, you're probably doing something wrong
declare function array_is_temp (byval this as any vector) as boolint

'Needed for v_append_once wrapper
declare function array_append (byref this as any vector, byval value as any ptr) as any vector
declare function array_find (byval this as any vector, byval value as any ptr) as int32

'Current FB -gen -gcc bug: can't have multiple prototypes aliased to the same function.
'So need to declare a single prototype for the vector functions, and use macros to point at them.
'Where the return value is T ptr or T vector it's possible to set the correct return type only
'by requiring parentheses around arguments.
'Functions which take a literal as argument need special treatment; they have wrapper functions
'in DECLARE_VECTOR_OF_TYPE.
#IF __FB_GCC__
  declare sub array_free (byref this as any vector)
  declare sub array_assign (byref dest as any vector, byref src as any vector)
  declare sub array_assign_d (byref dest as any vector, byref src as any vector)
  declare function array_temp (byval this as any vector) as any vector
  declare function array_length (byval this as any vector) as int32
  declare sub array_resize (byref this as any vector, byval len as int32)
  declare function array_expand (byref this as any vector, byval amount as int32 = 1) as any ptr
  declare sub array_shrink (byref this as any vector, byval amount as int32 = 1)
  declare function array_index (byval this as any vector, byval index as int32) as any ptr
  declare function array_end (byval this as any vector) as any ptr
  declare function array_type (byval this as any vector) as TypeTable ptr
  declare function array_extend (byref this as any vector, byref append as any vector) as any vector
  declare function array_extend_d (byref this as any vector, byref append as any vector) as any vector
  declare function array_sort (byval this as any vector, byval compfunc as FnCompare = 0) as any vector
  declare function array_reverse (byref this as any vector) as any vector
  declare function array_equal (byval lhs as any vector, byval rhs as any vector) as boolint
  declare function array_inequal (byref lhs as any vector, byref rhs as any vector) as boolint
  declare function array_insert (byref this as any vector, byval pos as int32, byval value as any ptr) as any vector
  declare function array_remove (byref this as any vector, byval value as any ptr) as int32
  declare function array_delete_slice (byref this as any vector, byval from as int32, byval to as int32) as any vector
  declare sub array_heappop (byref this as any vector, byval compfunc as FnCompare = 0)
  declare function array_heappush (byref this as any vector, byval value as any ptr, byval compfunc as FnCompare = 0) as int32

  'Some of the following are commented out because they're defined below as wrapper functions
  #DEFINE v_free array_free
  #DEFINE v_copy array_assign
  #DEFINE v_move array_assign_d
  #DEFINE v_ret(this) cast(typeof(this), array_temp(this))
  #DEFINE v_len array_length
  #DEFINE v_resize array_resize
  '#DEFINE v_expand(this, amount) cast(typeof(this), array_expand(this, amount))
  #DEFINE v_shrink array_shrink
  #DEFINE v_at(this, index) cast(typeof(this), array_index(this, index))
  #DEFINE v_end(this) cast(typeof(this), array_end(this))
  #DEFINE v_type array_type
  '#DEFINE v_append(this, value) cast(typeof(this), array_append(this, value))
  #DEFINE v_extend(this, append) cast(typeof(this), array_extend(this, append))
  #DEFINE v_extend_d(this, append) cast(typeof(this), array_extend_d(this, append))
  #DEFINE v_sort array_sort
  '#DEFINE v_sort(this, compfunc) cast(typeof(this), array_sort(this, compfunc))
  #DEFINE v_reverse array_reverse
  '#DEFINE v_reverse(this) cast(typeof(this), array_reverse(this))
  #DEFINE v_equal array_equal
  #DEFINE v_inequal array_inequal
  '#DEFINE v_find array_find
  '#DEFINE v_insert array_insert
  '#DEFINE v_remove array_remove
  #DEFINE v_delete_slice array_delete_slice
  '#DEFINE v_delete_slice(this, from, to) cast(typeof(this), array_delete_slice(this, from, to))
  #DEFINE v_heappop array_heappop
  '#DEFINE v_heappush array_heappush

#ENDIF
end extern

#define v_last(array)  (v_end(array)[-1])

'''''''''''''''''''''''''''''' Declaration macro '''''''''''''''''''''''''''''''


'Declare overloaded versions of each vector function
#MACRO DECLARE_VECTOR_OF_TYPE(T, TID)
  DECLARE_VECTOR_OF_TYPE_NOPOP(T, TID)

  'Pops and returns element at 'index', by default the last. Throws a fatal error if out of range.
  private function v_pop overload (byref this as T vector, byval index as integer = -1) as T
    if index = -1 then index = v_len(this) - 1
    dim ret as T = *v_at(this, index)
    v_delete_slice(this, index, index + 1)
    return ret
  end function
#ENDMACRO

'Use this instead of DECLARE_VECTOR_OF_TYPE if can't return a value of type 'T' (e.g. zstring,
'or if a necessary copy-constructor is missing)
#MACRO DECLARE_VECTOR_OF_TYPE_NOPOP(T, TID)

  extern "C"
    extern type_table(TID) as TypeTable
  end extern

  'Deletes any existing vector in 'this', creates a new vector.
  '(this should be either NULL or an existing array of the same type)
  'Special case: this is a FB wrapper function
  'reserve is the number of extra elements to reserve memory for. It is only an optimisation
  private sub v_new overload (byref this as T vector, byval length as int32 = 0, byval reserve as int32 = 2)
    array_new(this, length, reserve, @type_table(TID))
  end sub

#IF __FB_GCC__ = 0
  extern "c"

  'Deletes vector (if non-NULL), sets variable to NULL
  declare sub v_free overload alias "array_free" (byref this as T vector)

  'Deletes dest if it exists
  declare sub v_copy overload alias "array_assign" (byref dest as T vector, byref src as T vector)

  'Deletes dest if it exists; src is left equal to NULL. Importantly, unsets temporary flag.
  'Convention: always use this to assign a vector returned by a function to variable
  declare sub v_move overload alias "array_assign_d" (byref dest as T vector, byref src as T vector)

  'Marks a vector as temporary
  'Convention: always mark anything you return from a function as temporary!
  declare function v_ret overload alias "array_temp" (byval this as T vector) as T vector

  declare function v_len overload alias "array_length" (byval this as T vector) as int32

  'Changes the length of a vector. Elements are deleted or constructed as needed
  declare sub v_resize overload alias "array_resize" (byref this as T vector, byval len as int32)

  'Increases the number of elements of a vector. Returns a pointer to the first new element created
  declare function v_expand overload alias "array_expand" (byref this as T vector, byval amount as int32 = 1) as T ptr

  'Reduces the number of elements of a vector.
  declare sub v_shrink overload alias "array_shrink" (byref this as T vector, byval amount as int32 = 1)

  'Return pointer to some element. Throws a fatal error if out of range. Useful for polymorphism.
  declare function v_at overload alias "array_index" (byval this as T vector, byval index as int32) as T ptr

  'Returns pointer 1 past end, useful for indexing from the end
  declare function v_end overload alias "array_end" (byval this as T vector) as T ptr

  'The last element of the array; invalid if length is zero
  '#define v_last(array)  (v_end(array)[-1])

  'Returns type table
  declare function v_type overload alias "array_type" (byval this as T vector) as TypeTable ptr

  'Returns 'this'
  declare function v_append overload alias "array_append" (byref this as T vector, byref value as T) as T vector

  'If not already in the vector, append value and return true
  declare function v_append_once overload (byref this as T vector, byref value as T) as bool

  'Concatenate two vectors. Returns 'this'
  'Note: brackets around the argument list aren't optional: gengcc builds will break
  declare function v_extend overload alias "array_extend" (byref this as T vector, byref append as T vector) as T vector

  'Concatenate two vectors, but destroy the second in the process: much faster. Returns 'this'.
  'Note: brackets around the argument list aren't optional: gengcc builds will break
  declare function v_extend_d overload alias "array_extend_d" (byref this as T vector, byref append as T vector) as T vector

  'Sort (Quicksort: non-stable) into ascending order. Returns 'this'.
  'You can override the type's default compare func.
  declare function v_sort overload alias "array_sort" (byval this as T vector, byval compfunc as FnCompare = 0) as T vector

  'Reverse elements. Returns 'this'
  declare function v_reverse overload alias "array_reverse" (byref this as T vector) as T vector

  'Are all elements equal? If the type has no comparison functions defined, does raw memcmp. Returns YES/NO
  declare function v_equal overload alias "array_equal" (byval lhs as T vector, byval rhs as T vector) as boolint

  'Are any elements inequal? If the type has no comparison functions defined, does raw memcmp. Returns YES/NO
  'Please ignore the byrefs: they are there only to match the FnCompare signature
  declare function v_inequal overload alias "array_inequal" (byref lhs as T vector, byref rhs as T vector) as boolint

  'Returns the index of the first element equal to 'item', or -1 if not found
  declare function v_find overload alias "array_find" (byval this as T vector, byref value as T) as int32

  'Insert an element at some position. Returns 'this'
  declare function v_insert overload alias "array_insert" (byref this as T vector, byval position as int32, byref value as T) as T vector

  'Remove the first instance of value. No error or warning if it isn't found.
  'Returns the index of the item if it was found, or -1 if not
  'Use v_delete_slice if you want to remove an item at a certain index
  declare function v_remove overload alias "array_remove" (byref this as T vector, byref value as T) as int32

  'Delete the range [from, to). Returns 'this'
  declare function v_delete_slice overload alias "array_delete_slice" (byref this as T vector, byval from as int32, byval to as int32) as T vector

  'The following act on heaps, which are vectors ordered according to a tree structure, with (one of) the
  'minimum element at this[0]. They are an efficient way to implement priority queues.
  'Construct a heap by starting from an empty vector and calling v_heappush
  'repeatedly, or just use v_sort.

  'Remove and delete the smallest element (read it yourself before popping) from a heap.
  'You can override the type's default compare func.
  declare sub v_heappop overload alias "array_heappop" (byref this as T vector, byval compfunc as FnCompare = 0)

  'Add a new value to a heap. Returns the index at which it was placed.
  'You can override the type's default compare func.
  declare function v_heappush overload alias "array_heappush" (byref this as T vector, byref value as T, byval compfunc as FnCompare = 0) as int32

  end extern

#ELSE  'IF __FB_GCC__

  'Can't use the "byref as T" trick to get fbc to pass as pointer to an object
  'when using #defines instead of aliases (see the #IF __FB_GCC__ block above)
  'and I'm not aware of any other way to get a pointer for an arbitrary type in a macro.
  '(E.g. @/varptr don't work for integers inside a macro)
  '(There's another reason to not use #defines: it might require writing the call with brackets.)
  'So use wrapper functions instead (luckily GCC can optimise these away)

  'Wrap just for the default argument
  private function v_expand overload (byref this as T vector, byval amount as int32 = 1) as T ptr
    return cast(T ptr, array_expand(this, amount))
  end function

  private function v_append overload (byref this as T vector, byref value as T) as T vector
    return cast(T vector, array_append(this, @value))
  end function

  private function v_find overload (byval this as T vector, byref value as T) as int32
    return array_find(this, @value)
  end function

  private function v_insert overload (byref this as T vector, byval position as int32, byref value as T) as T vector
    return cast(T vector, array_insert(this, position, @value))
  end function

  private function v_remove overload (byref this as T vector, byref value as T) as int32
    return array_remove(this, @value)
  end function

  private function v_heappush overload (byref this as T vector, byref value as T, byval compfunc as FnCompare = 0) as int32
    return array_heappush(this, @value, compfunc)
  end function

#ENDIF

  extern "C"
  private function v_append_once overload (byref this as T vector, byref value as T) as bool
    if array_find(this, @value) = -1 then
      array_append(this, @value)
      return YES
    end if
  end function
  end extern

#ENDMACRO


'Accepts any type of vector, and returns a string representation, eg [3, 4]. Understands nested vectors.
'Doesn't actually modify vec, ignore the byref. (Implemented in vector.bas)
DECLARE FUNCTION v_str CDECL (byref vec as any vector) as string


'This stuff is commented out until we switch to a version of FB with variadic macros
/'
#MACRO array_of(arg0, arg1)
'Special case for strings, as the type of a string literal is actually zstring
# IF typeof(arg0) = zstring
   array_create(type_table(string), arg0, arg1)
# ELSE
   array_create(type_table(typeof(arg0)), arg0, arg1)
# ENDIF
#ENDMACRO

declare function cdecl array_create(byval tbl as typeTable, ...)
'/


'''''''''''''''''''''''''''''' Definition macros '''''''''''''''''''''''''''''''


'For UDTs not having a [copy] constructor or destructor (which those containing strings have).
'T is a type, and TID is T with spaces replaced with underscores.
#MACRO DEFINE_VECTOR_OF_TYPE(T, TID)

  private sub TID##_copyconstr_func cdecl (byval p1 as T ptr, byval p2 as T ptr)
    '(Only works for simple types not containing strings, because p1 contains garbage)
    *p1 = *p2
  end sub

  DEFINE_VECTOR_OF_TYPE_COMMON(T, TID, @TID##_copyconstr_func, NULL)
#ENDMACRO


'For UDTs having a destructor or copy constructor (eg containing strings), but no constructor.
'If it has a constructor, you need DEFINE_CUSTOM_VECTOR_TYPE, or more likely, you should store ptrs instead!
'T is a type, and TID is T with spaces replaced with underscores.
#MACRO DEFINE_VECTOR_OF_CLASS(T, TID)

  'Note this is the copy constructor, NOT the assignment operator, so p1 contains garbage
  'and so *p1 = *p2 won't work
  private sub TID##_copyctor_func cdecl (byval p1 as T ptr, byval p2 as T ptr)
    '(Does not work for POD UDTs not containing strings: FB does not generate
    ' copy constructors)
    p1->constructor(*p2)
  end sub

  private sub TID##_dtor_func cdecl (byval p as T ptr)
    '(Only works for UDTs, not primitive types)
    'FB acts very strangely wrt destructors... if a UDT does not actually need destructing,
    'FB will still generate destructor calls, but if you try to call ->destructor() on such
    'a pointer, it seems to screw up and stop generating assembly
    p->destructor()
  end sub

  DEFINE_VECTOR_OF_TYPE_COMMON(T, TID, @TID##_copyctor_func, @TID##_dtor_func)
#ENDMACRO

#MACRO DEFINE_VECTOR_OF_TYPE_COMMON(T, TID, COPY_FUNC, DELETE_FUNC)
  'Dumb default (FB doesn't provide default comparison methods for UDTs)
  private function TID##_compare_func cdecl (byval p1 as T ptr, byval p2 as T ptr) as int32
    return memcmp(p1, p2, sizeof(T))   
  end function

  DEFINE_CUSTOM_VECTOR_TYPE(T, TID, NULL, COPY_FUNC, DELETE_FUNC, @TID##_compare_func, NULL, NULL, NULL)
#ENDMACRO

'Creates TypeTable for T, allowing use of 'T vector' and type_table(T)
'If you want to customise one of the methods, you can use this instead of DEFINE_VECTOR_OF_TYPE.
'You don't need to provide EQUAL_FUNC if you give COMPARE_FUNC unless you have special reason
'(doubles). You don't need COMPARE_FUNC if you don't want to sort an vector. You can give NULL
'for both if you don't want to search an vector for an element.
'
'WARNING: Don't forget to use CDECL!!! No warning will be given!
'
#MACRO DEFINE_CUSTOM_VECTOR_TYPE(T, TID, CTOR_FUNC, COPYCTOR_FUNC, DTOR_FUNC, COMPARE_FUNC, INEQUAL_FUNC, HASH_FUNC, STR_FUNC)

  private function TID##_copy cdecl (byval p as T ptr) as T ptr
    'This works regardless of whether the UDT has a copy-constructor or not.
    '(You might get an 'Invalid data types' error here due to https://sourceforge.net/p/fbc/bugs/894/)
    return new T(*p)
  end function

  private sub TID##_delete cdecl (byval p as T ptr)
    delete p
  end sub

  DEFINE_TYPE_TABLE(T, TID, CTOR_FUNC, COPYCTOR_FUNC, DTOR_FUNC, COMPARE_FUNC, INEQUAL_FUNC, HASH_FUNC, STR_FUNC, @TID##_copy, @TID##_delete)
#ENDMACRO


#MACRO DEFINE_TYPE_TABLE(T, TID, CTOR_FUNC, COPYCTOR_FUNC, DTOR_FUNC, COMPARE_FUNC, INEQUAL_FUNC, HASH_FUNC, STR_FUNC, COPY_FUNC, DELETE_FUNC)
  DIM type_table(TID) as TypeTable
  'FB doesn't let you use function addresses in initialisers, considered non-constant
  type_table(TID) = Type( _
     sizeof(T),                     /'element_len'/          _
     PASS_BYVAL,                    /'passtype'/             _
     cast(FnCtor, CTOR_FUNC),       /'ctor'/                 _
     cast(FnCopyCtor, COPYCTOR_FUNC),/'copyctor'/            _
     cast(FnDtor, DTOR_FUNC),       /'dtor'/                 _
     cast(FnCopy, COPY_FUNC),       /'copy'/                 _
     cast(FnDelete, DELETE_FUNC),   /'_delete'/              _
     cast(FnCompare, COMPARE_FUNC), /'comp'/                 _
     cast(FnCompare, INEQUAL_FUNC), /'inequal'/              _
     cast(FnHash, HASH_FUNC),       /'hash'/                 _
     cast(FnStr, STR_FUNC),         /'tostr'/                _
     @#T                            /'name'/                 _
  )
#ENDMACRO

'Defines T vector vector (which means the base type is 'T vector')
#MACRO DEFINE_VECTOR_VECTOR_OF(T, TID)
  sub TID##_vector_ctor CDECL (byref this as T vector)
    this = NULL
    array_new this, 0, 0, @type_table(TID)
  end sub

  sub TID##_vector_copyctor CDECL (byref this as T vector, byref that as T vector)
    this = NULL
    v_copy this, that
  end sub

  DEFINE_CUSTOM_VECTOR_TYPE(T vector, TID##_vector, @TID##_vector_ctor, @TID##_vector_copyctor, @v_free, NULL, @v_inequal, NULL, @v_str)
#ENDMACRO


'''''''''''''''''''''''''' Common type declarations ''''''''''''''''''''''''''''


'Declarations of common/primitive TypeTables. Definitions are in vector.bas 

DECLARE_VECTOR_OF_TYPE(integer, integer)
DECLARE_VECTOR_OF_TYPE(double, double)
DECLARE_VECTOR_OF_TYPE(string, string)
DECLARE_VECTOR_OF_TYPE(zstring ptr, zstring_ptr)  'Warning, no deleting, copying or initialisation done (strings assumed to be static)
DECLARE_VECTOR_OF_TYPE_NOPOP(zstring, zstring)  'Warning, no deleting, copying or initialisation done (strings assumed to be static)
DECLARE_VECTOR_OF_TYPE(any ptr, any_ptr)  'Pointers aren't freed when the vector is

DECLARE_VECTOR_OF_TYPE(integer vector, integer_vector)

'Special: can contain any kind of vector, but you can't do anything with the elements directly
'without casting them to some vector type
'DECLARE_VECTOR_OF_TYPE(any vector, any_vector)
'DECLARE_VECTOR_OF_TYPE(any, any)

'Utility functions

DECLARE FUNCTION intvec_sum(byval vec as integer vector) as integer

'These only accept arrays with LBOUND 0 or -1, the -1th element is ignored
DECLARE SUB array_to_vector OVERLOAD (byref vec as integer vector, array() as integer)
DECLARE SUB array_to_vector OVERLOAD (byref vec as string vector, array() as string)

'These require dynamic arrays, of course. If the array is zero length, the array will be -1 TO -1
DECLARE SUB vector_to_array OVERLOAD (array() as integer, byval vec as integer vector)
DECLARE SUB vector_to_array OVERLOAD (array() as string, byval vec as string vector)


#ENDIF
