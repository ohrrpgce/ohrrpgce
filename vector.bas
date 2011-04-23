'OHRRPGCE - Type tables for vectors
'
'Please read LICENSE.txt for GNU GPL details and disclaimer of liability
'
'Type definitions for the basic builtin types. Place other DEFINE_VECTOR_OF_TYPE macros in another
'module (somewhere where your type definitions are actually available, they aren't here)


#include "util.bi"

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

FUNCTION double_compare CDECL (BYVAL a as double ptr, BYVAL b as double ptr) as integer
  IF *a < *b THEN RETURN -1
  IF *a > *b THEN RETURN 1
  'implicitly RETURN 0 (it's faster to omit the RETURN :-)
END FUNCTION

'Doubles are tricky. You usually don't really want to use exactly equality to test whether
'two doubles are equal, only to use it for sorting.
FUNCTION double_inequal CDECL (BYVAL a as double ptr, BYVAL b as double ptr) as integer
  'tol = MAX(ABS(*a), ABS(*b), 1.0)
  DIM tol as double = ABS(*a)
  DIM temp as double = ABS(*b)
  IF temp > tol THEN tol = temp
  IF 1.0 > tol THEN tol = 1.0
  RETURN (ABS(*a - *b) <= 1E-15 * tol)
END FUNCTION

FUNCTION string_str CDECL (BYVAL this as string ptr) as string
  RETURN """" + *this + """"
END FUNCTION

FUNCTION integer_str CDECL (BYVAL this as integer ptr) as string
  RETURN STR(*this)
END FUNCTION

FUNCTION double_str CDECL (BYVAL this as double ptr) as string
  RETURN STR(*this)
END FUNCTION


' Non-UDT types each require special treatment
'DEFINE_CUSTOM_VECTOR_TYPE(T,        TID,       CTOR_FUNC, COPY_FUNC,        DELETE_FUNC,  COMPARE_FUNC,     INEQUAL_FUNC, STR_FUNC)

DEFINE_CUSTOM_VECTOR_TYPE(integer,   integer,   NULL,      NULL,             NULL,         @integer_compare, NULL,     @integer_str)
DEFINE_CUSTOM_VECTOR_TYPE(double,    double,    NULL,      NULL,             NULL,         @double_compare,  @double_inequal, @double_str)
DEFINE_CUSTOM_VECTOR_TYPE(string,    string,    NULL,      @string_copyctor, @string_dtor, @string_compare,  NULL,     @string_str)

'DEFINE_CUSTOM_VECTOR_TYPE(any ptr,   any_ptr,   NULL,      NULL,             NULL,         @integer_compare, NULL,    NULL)
'Note: v_copy might change (free) the src if it is temp. An 'any vector' should never contain temps
'DEFINE_CUSTOM_VECTOR_TYPE(any vector, any_vector, NULL,      @v_copy,          @v_free,      NULL,             NULL,   NULL)

DEFINE_VECTOR_VECTOR_OF(integer, integer)  'integer vector vector

'Utility Functions

FUNCTION intvec_sum(BYVAL vec as integer vector) as integer
  DIM sum as integer = 0
  FOR i as integer = 0 TO v_len(vec) - 1
    sum += vec[i]
  NEXT
  RETURN sum
END FUNCTION

FUNCTION v_str(BYVAL vec as any vector) as string
  IF vec = NULL THEN RETURN ""
  DIM ret as string = "["
  'Cast so that we can call a set of overloaded functions (aside from v_new, all
  'overloads are actually identical)
  DIM vec_ as integer vector = cast(integer vector, vec)
  DIM tbl as TypeTable ptr = v_type(vec_)
  FOR i as integer = 0 TO v_len(vec_) - 1
    IF i <> 0 THEN ret += ", "
    DIM p as any ptr = vec + i * tbl->element_len
    'IF RIGHT(*tbl->name, 7) = " vector" THEN
    IF tbl->dtor = @v_free THEN
      'It's a vector vector, can recurse!
      ret += v_str(*cast(any vector ptr, p))
    ELSEIF tbl->tostr THEN
      ret += tbl->tostr(p)
    ELSE
      ret += "<" + *tbl->name + ">"
    END IF
  NEXT
  RETURN ret + "]"
END FUNCTION
