/* OHRRPGCE - typed array (vector) datatype
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
 * Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
 */

//#include "fb/fb_stub.h"
struct FBSTRING;

typedef void (*FnCtor)(void *);
typedef void (*FnCopyCtor)(void *, void *);  // second arg ought to be const, but in practice isn't (AnyVector)
typedef void (*FnDtor)(void *);
typedef void *(*FnCopy)(void *);  // Allocate and initialise a copy
typedef void (*FnDelete)(void *);  // Destruct and delete
typedef int (*FnCompare)(const void *, const void *);
typedef unsigned int (*FnHash)(const void *);
typedef struct FBSTRING *(*FnStr)(const void *);

typedef struct _typetable {
	unsigned int element_len;
	// Explains the default passing type (NOT USED)
	// Note: strings are an exception
	enum PassConvention { PASS_BYVAL, PASS_BYREF, PASS_ZSTRING } passtype;
	FnCtor ctor;
	FnCopyCtor copyctor;
	FnDtor dtor;
	FnCopy copy;
	FnDelete delete;
	FnCompare comp;
	FnCompare inequal;
	FnHash hash;
	FnStr tostr;
	char *name;
} typetable;

#define type_table(T) type_tbl_##T

// Define array_t as a pointer to some type, rather than as void* which
// leads to almost zero type checking.
typedef struct _dummy_ {int a;} *array_t;



// Typetables which can be passed to array_new. Remember to use the FB name of a type, not the C name
// These typetables are defined in vector.bas

                                                     // C name of the element type:
extern typetable type_table(integer);                // int
extern typetable type_table(double);                 // double
extern typetable type_table(string);                 // FBSTRING
extern typetable type_table(zstring_ptr);            // char*  (assumed to be static strings!)
extern typetable type_table(any_ptr);                // void*
extern typetable type_table(any_vector);             // array_t (vector of void* vectors)
extern typetable type_table(integer_vector);         // array_t (vector of int vectors)

// Typetables for UDTs

extern typetable type_table(MapEditUndoTile);        // MAPEDITUNDOTILE
extern typetable type_table(MapEditUndoTile_vector); // array_t (vector of MAPEDITUNDOTILE)
extern typetable type_table(BasicMenuItem);          // BASICMENUITEM
extern typetable type_table(MenuDefItem);            // MENUDEFITEM
extern typetable type_table(SimpleMenuItem);         // SIMPLEMENUITEM
// And more... these don't belong here anyway


void array_new(array_t *array, int len, int reserve, typetable *tbl);
void array_free(array_t *array);
array_t array_append(array_t *array, void *value);
array_t array_extend_d(array_t *dest, array_t *src);
array_t array_extend(array_t *dest, array_t *src);
void array_assign(array_t *dest, array_t *src);
void array_assign_d(array_t *dest, array_t *src);
void array_resize(array_t *array, int len);
void *array_expand(array_t *array, int amount);
int array_length(array_t array);
array_t array_end(array_t array);
void *array_index(array_t array, int n);
typetable *array_type(array_t array);
array_t array_temp(array_t array);
boolint array_is_temp(array_t array);
array_t array_sort(array_t array, FnCompare compare);
boolint array_equal(array_t lhs, array_t rhs);
boolint array_inequal(array_t *lhs, array_t *rhs);
int array_find(array_t array, void *value);
array_t array_insert(array_t *array, int pos, void *value);
array_t array_delete_slice(array_t *array, int from, int to);
int array_remove(array_t *array, void *value);
array_t array_reverse(array_t *array);
void array_heappop(array_t *array, FnCompare compare);
int array_heappush(array_t *array, void *value, FnCompare compare);
