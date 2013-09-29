'OHRRPGCE - Type tables for vectors
'
'Please read LICENSE.txt for GNU GPL details and disclaimer of liability
'
'Type definitions for the basic builtin types. Place other DEFINE_VECTOR_OF_TYPE macros in another
'module (somewhere where your type definitions are actually available, they aren't here)

#include "config.bi"
#include "util.bi"
#include "common_base.bi"

'Note: has default FB calling convention (FBCALL)
DECLARE FUNCTION fb_StrAssignEx alias "fb_StrAssignEx" (byval dst as any ptr, byval dst_size as integer, byval src as any ptr, byval src_size as integer, byval fill_rem as integer, byval is_init as integer) as any ptr



'Unfortunately fb_StrDelete is not cdecl
SUB string_dtor cdecl (byval arg as string ptr)
  fb_StrDelete(*arg)
END SUB

'Strings don't have constructors or other operator methods, so call libfb directly
SUB string_copyctor cdecl (byval p1 as string ptr, byval p2 as string ptr)
  fb_StrAssignEx(p1, -1, p2, -1, 0, 1)
END SUB

FUNCTION zstring_compare CDECL (byval a as zstring ptr ptr, byval b as zstring ptr ptr) as integer
  RETURN strcmp(*a, *b)
END FUNCTION

FUNCTION double_compare CDECL (byval a as double ptr, byval b as double ptr) as integer
  IF *a < *b THEN RETURN -1
  IF *a > *b THEN RETURN 1
  'implicitly RETURN 0 (it's faster to omit the RETURN :-)
END FUNCTION

'Doubles are tricky. You usually don't really want to use exactly equality to test whether
'two doubles are equal, only to use it for sorting.
FUNCTION double_inequal CDECL (byval a as double ptr, byval b as double ptr) as integer
  'tol = MAX(ABS(*a), ABS(*b), 1.0)
  DIM tol as double = ABS(*a)
  DIM temp as double = ABS(*b)
  IF temp > tol THEN tol = temp
  IF 1.0 > tol THEN tol = 1.0
  RETURN (ABS(*a - *b) <= 1E-15 * tol)
END FUNCTION

FUNCTION string_str CDECL (byval this as string ptr) as string
  RETURN """" + *this + """"
END FUNCTION

FUNCTION zstring_str CDECL (byval this as zstring ptr ptr) as string
  RETURN """" + **this + """"
END FUNCTION

FUNCTION integer_str CDECL (byval this as integer ptr) as string
  RETURN STR(*this)
END FUNCTION

FUNCTION ptr_str CDECL (byval this as any ptr ptr) as string
  RETURN "0x" + HEX(cast(unsigned integer, *this))
END FUNCTION

FUNCTION double_str CDECL (byval this as double ptr) as string
  RETURN STR(*this)
END FUNCTION


' Non-UDT types each require special treatment
'DEFINE_CUSTOM_VECTOR_TYPE(T,          TID,         CTOR_FUNC, COPY_FUNC,        DELETE_FUNC,  COMPARE_FUNC,     INEQUAL_FUNC,    STR_FUNC)

DEFINE_CUSTOM_VECTOR_TYPE(integer,     integer,     NULL,      NULL,             NULL,         @integer_compare, NULL,            @integer_str)
DEFINE_CUSTOM_VECTOR_TYPE(double,      double,      NULL,      NULL,             NULL,         @double_compare,  @double_inequal, @double_str)
DEFINE_CUSTOM_VECTOR_TYPE(string,      string,      NULL,      @string_copyctor, @string_dtor, @string_compare,  NULL,            @string_str)
DEFINE_CUSTOM_VECTOR_TYPE(zstring ptr, zstring_ptr, NULL,      NULL,             NULL,         @zstring_compare, NULL,            @zstring_str)

DEFINE_CUSTOM_VECTOR_TYPE(any ptr,     any_ptr,     NULL,      NULL,             NULL,         @integer_compare, NULL,            @ptr_str)
'Note: v_copy might change (free) the src if it is temp. An 'any vector' should never contain temps
'DEFINE_CUSTOM_VECTOR_TYPE(any vector, any_vector,  NULL,      @v_copy,          @v_free,      NULL,             NULL,            NULL)

DEFINE_VECTOR_VECTOR_OF(integer, integer)  'integer vector vector

'Utility Functions

FUNCTION intvec_sum(byval vec as integer vector) as integer
  DIM sum as integer = 0
  FOR i as integer = 0 TO v_len(vec) - 1
    sum += vec[i]
  NEXT
  RETURN sum
END FUNCTION

'Not byref because it modifies vec, but simply to fit the signature...
'ought to take a vector ptr, but that's unpleas
FUNCTION v_str CDECL (byref vec as any vector) as string
  IF vec = NULL THEN RETURN ""
  DIM ret as string = "["
  'Cast so that we can call a set of overloaded functions (aside from v_new, all
  'overloads are actually identical)
  DIM vec_ as integer vector = cast(integer vector, vec)
  DIM tbl as TypeTable ptr = v_type(vec_)
  FOR i as integer = 0 TO v_len(vec_) - 1
    IF i <> 0 THEN ret += ", "
    DIM p as any ptr = vec + i * tbl->element_len
    IF tbl->tostr THEN
      ret += tbl->tostr(p)
    ELSE
      ret += "<" + *tbl->name + ">"
    END IF
  NEXT
  RETURN ret + "]"
END FUNCTION

SUB vector_to_array OVERLOAD (array() as integer, byval vec as integer vector)
  IF vec = NULL THEN
    debug "vector_to_array: uninitialised vector is suspicious"
    REDIM array(-1 TO -1)
    EXIT SUB
  ELSEIF v_len(vec) = 0 THEN
    REDIM array(-1 TO -1)
    EXIT SUB
  END IF
  REDIM array(0 TO v_len(vec) - 1)
  FOR i as integer = 0 TO v_len(vec) - 1
    array(i) = vec[i]
  NEXT
END SUB

SUB vector_to_array OVERLOAD (array() as string, byval vec as string vector)
  IF vec = NULL THEN
    debug "vector_to_array: uninitialised vector is suspicious"
    REDIM array(-1 TO -1)
    EXIT SUB
  ELSEIF v_len(vec) = 0 THEN
    REDIM array(-1 TO -1)
    EXIT SUB
  END IF
  REDIM array(0 TO v_len(vec) - 1)
  FOR i as integer = 0 TO v_len(vec) - 1
    array(i) = vec[i]
  NEXT
END SUB

SUB array_to_vector OVERLOAD (byref vec as integer vector, array() as integer)
  IF LBOUND(array) < -1 OR LBOUND(array) > 0 THEN
    showerror "array_to_vector: bad array size " & LBOUND(array) & " TO " & UBOUND(array)
    v_new vec
    EXIT SUB
  END IF
  v_new vec, UBOUND(array) + 1
  FOR i as integer = 0 TO v_len(vec) - 1
    vec[i] = array(i)
  NEXT
END SUB

SUB array_to_vector OVERLOAD (byref vec as string vector, array() as string)
  IF LBOUND(array) < -1 OR LBOUND(array) > 0 THEN
    showerror "array_to_vector: bad array size " & LBOUND(array) & " TO " & UBOUND(array)
    v_new vec
    EXIT SUB
  END IF
  v_new vec, UBOUND(array) + 1
  FOR i as integer = 0 TO v_len(vec) - 1
    vec[i] = array(i)
  NEXT
END SUB
