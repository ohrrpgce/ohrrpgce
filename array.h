//#include "fb/fb_stub.h"
struct FBSTRING;

typedef void (*FnCtor)(void *);
typedef int (*FnCompare)(const void *, const void *);
typedef void (*FnCopy)(void *, void *);  // second arg ought to be const, but in practice isn't (AnyVector)
typedef struct FBSTRING *(*FnStr)(const void *);

typedef struct _typetable {
	unsigned int element_len;
	// Explains the default passing type
	// Note: strings are an exception
	enum PassConvention { PASS_BYVAL, PASS_BYREF, PASS_ZSTRING } passtype;
	FnCtor ctor;
	FnCopy copyctor;
	FnCtor dtor;
	FnCompare comp;
	FnCompare inequal;
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
extern typetable type_table(integer);                // intptr_t, not int
extern typetable type_table(double);                 // double
extern typetable type_table(string);                 // FBSTRING*
extern typetable type_table(zstring_ptr);            // char*  (assumed to be static strings!)
extern typetable type_table(any_ptr);                // void*
extern typetable type_table(any_vector);             // array_t (vector of void* vectors)
extern typetable type_table(integer_vector);         // array_t (vector of intptr_t vectors)

// Typetables for UDTs

extern typetable type_table(MapEditUndoTile);        // MAPEDITUNDOTILE
extern typetable type_table(MapEditUndoTile_vector); // array_t (vector of MAPEDITUNDOTILE)
extern typetable type_table(BasicMenuItem);          // BASICMENUITEM
extern typetable type_table(MenuDefItem);            // MENUDEFITEM
extern typetable type_table(SimpleMenuItem);         // SIMPLEMENUITEM


void array_new(array_t *array, int len, typetable *tbl);
void array_free(array_t *array);
array_t array_append(array_t *array, void *value);
array_t array_extend_d(array_t *dest, array_t *src);
array_t array_extend(array_t *dest, array_t *src);
void array_assign(array_t *dest, array_t *src);
void array_assign_d(array_t *dest, array_t *src);
void array_resize(array_t *array, unsigned int len);
void *array_expand(array_t *array, unsigned int amount);
int array_length(array_t array);
array_t array_end(array_t array);
void *array_index(array_t array, int n);
typetable *array_type(array_t array);
array_t array_temp(array_t array);
int array_is_temp(array_t array);
array_t array_sort(array_t array, FnCompare compare);
int array_equal(array_t lhs, array_t rhs);
int array_inequal(array_t *lhs, array_t *rhs);
int array_find(array_t array, void *value);
array_t array_insert(array_t *array, int pos, void *value);
array_t array_delete_slice(array_t *array, int from, int to);
int array_remove(array_t *array, void *value);
array_t array_reverse(array_t *array);
