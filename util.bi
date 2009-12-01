'OHRRPGCE - util.bi
'(C) Copyright 1997-2006 James Paige and Hamster Republic Productions
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'See README.txt for code docs and apologies for crappyness of this code ;)

#IFNDEF UTIL_BI
#DEFINE UTIL_BI

#IFNDEF NULL
#DEFINE NULL 0
#ENDIF


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


declare sub str_array_append (array() as string, s as string)
declare sub int_array_append (array() as integer, k as integer)
declare function int_array_find (array() as integer, value as integer) as integer


TYPE Stack
  pos as integer ptr
  bottom as integer ptr
  size as integer
END TYPE

declare sub createstack (st as Stack)
declare sub destroystack (st as Stack)
declare sub checkoverflow (st as Stack, byval amount as integer = 1)
#define pushs(stack, datum) *(stack).pos = (datum) : (stack).pos += 1
#define pops(stack, var) (stack).pos -= 1 : (var) = *(stack).pos
'read from a stack offset from the last push (eg. 0 is last int pushed, -1 is previous)
#define reads(stack, off) stack.pos[(off) - 1]
#define checkunderflow(stack, amount) ((stack).pos - (amount) < (stack).bottom)


TYPE IntStrPair
  i as integer
  s as string
END TYPE

declare function search_string_cache (cache() as IntStrPair, byval key as integer, resetter as string = CHR(234)) as string
declare sub add_string_cache (cache() as IntStrPair, byval key as integer, value as string)
declare sub remove_string_cache (cache() as IntStrPair, byval key as integer)


'doubly linked list header
TEMPLATE_GENERIC(DoubleList, T)
  numitems as integer
  first as T ptr
  last as T ptr
  memberoffset as integer
#ENDMACRO
ENDGENERIC

'doubly linked list item
TEMPLATE_GENERIC(DListItem, T)
  next as T ptr
  prev as T ptr
#ENDMACRO
ENDGENERIC

'DList functions 'item' arguments are pointer to objects containing DListItem instances.
'You have to provide the offset of the the DListItem as itemoffset to dlist_construct.
'Pass
declare sub dlist_construct (byref this as DoubleList(Any), byval itemoffset as integer)

'NULL as beforeitem inserts at end
declare sub dlist_insertat (byref this as DoubleList(Any), byval beforeitem as any ptr, byval newitem as any ptr)

#define dlist_append(this, newitem) dlist_insertat((this), NULL, (newitem))

declare sub dlist_remove (byref this as DoubleList(Any), byval item as any ptr)

'returns 1-based index of item in the list, or 0 if not found
declare function dlist_find (byref this as DoubleList(Any), byval item as any ptr) as integer


'----------------------------------------------------------------------
'                              Other


declare function bound overload (byval n AS INTEGER, byval lowest AS INTEGER, byval highest AS INTEGER) AS INTEGER
declare function bound overload (byval n as double, byval lowest as double, byval highest as double) as double
declare function large (byval n1 AS INTEGER, byval n2 AS INTEGER) AS INTEGER
declare function loopvar (byval var AS INTEGER, byval min AS INTEGER, byval max AS INTEGER, byval inc AS INTEGER) AS INTEGER
declare function small (byval n1 AS INTEGER, byval n2 AS INTEGER) AS INTEGER
declare function range (number as integer, percent as integer) as integer
declare function rpad (s as string, pad_char as string, size as integer) as string
declare function is_int(s as string) as integer
declare function str2int (stri as string, default as integer=0) as integer
declare function trimpath (filename as string) as string
declare function trimfilename (filename as string) as string
declare function trimextension (filename as string) as string
declare function justextension (filename as string) as string
declare function anycase (filename as string) as string
declare sub touchfile (filename as string)
declare function rotascii (s as string, o as integer) as string
declare function escape_string(s as string, chars as string) as string
declare function exclude (s as string, x as string) as string
declare function exclusive (s as string, x as string) as string
declare function sign_string(n as integer, neg_str as string, zero_str as string, pos_str as string) as string
declare function zero_default(n as integer, zerocaption AS STRING="default", displayoffset AS INTEGER = 0) as string
declare Function wordwrap(Byval inp as string, byval width as integer, byval sep as string = chr(10)) as string
declare sub split(byval in as string, ret() as string, sep as string = chr(10))
declare function textwidth(byval z as string) as integer
declare sub sort_integers_indices(indices() as integer, byval start as integer ptr, byval number as integer = 0, byval stride as integer = SIZEOF(integer))
declare function strhash overload (byval strp as zstring ptr, byval leng as integer) as unsigned integer
declare function strhash overload (hstr as string) as unsigned integer

#ENDIF
