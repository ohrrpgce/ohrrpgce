/* OHRRPGCE - Replacement for FB arrays
 * Copyright 2010. Please read LICENSE.txt for GNU GPL details and disclaimer of liability
 */

#include <stdarg.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#ifdef _WIN32
#include <malloc.h>
#else
#include <alloca.h>
#endif
#include "common.h"
#include "array.h"


typedef struct _array_header {
	typetable *typetbl;
	unsigned int len:31;
	unsigned int temp:1;
} array_header;


void (*debug_hook)(enum ErrorLevel errorlevel, const char *msg) = debugc;

// This is for the benefit of testing tools (vectortest)
void set_debug_hook(void (*new_debug_hook)(enum ErrorLevel errorlevel, const char *msg)) {
	if (new_debug_hook)
		debug_hook = new_debug_hook;
	else
		debug_hook = debugc;
}

void _throw_error(enum ErrorLevel errorlevel, const char *srcfile, int linenum, const char *msg, ...) {
	va_list vl;
	va_start(vl, msg);
	char buf[256];
	buf[255] = '\0';
	int emitted = 0;
	if (srcfile)
		emitted = snprintf(buf, 255, "On line %d in %s: ", linenum, srcfile);
	vsnprintf(buf + emitted, 255 - emitted, msg, vl);
	va_end(vl);
	debug_hook(errorlevel, buf);
	/*
	if (errorlevel >= 5) {
		// Ah, what the heck, shouldn't run, but I already wrote it (NULLs indicate no RESUME support)
		void (*handler)() = fb_ErrorThrowAt(linenum, srcfile, NULL, NULL);
		handler();
	}
	*/
}


/* ifdef VALGRIND_ARRAYS, then two bytes before the start of an array hold an id
   which points to an entry in header_table. Otherwise the array_header is placed
   before the array. Defining VALGRIND_ARRAYS means that accessing an array with
   sizeof(datatype) > 2 will be off the end of the allocated memory block,
   allowing valgrind to catch it. However it is slower and only allows
   up to 65536 arrays to be used at once. */

#ifndef VALGRIND_ARRAYS

#define ARRAY_OVERHEAD sizeof(array_header)

static inline array_header *get_header(array_t array) {
	if (!array)
		return NULL;
	return (array_header *)array - 1;
}

#else // defined(VALGRIND_ARRAYS)

#define ARRAY_OVERHEAD sizeof(short)

array_header **header_table;

static short alloc_header() {
	if (!header_table) {
		header_table = calloc(65536 * sizeof(void *), 1);
	}
	for (int i = 0; i < 65536; i++) {
		if (!header_table[i]) {
			header_table[i] = malloc(sizeof(array_header));
			return i;
		}
	}
	debug(errFatal, "header_table is full: 65536 arrays are allocated. Compile without -DVALGRIND_ARRAYS");
}

static void free_header(short id) {
	if (!header_table[id])
		throw_error("alloc_header: header_table[%d] already free", id);
	free(header_table[id]);
	header_table[id] = NULL;
}

static inline array_header *get_header(array_t array) {
	if (!array)
		return NULL;
	return header_table[((short *)array)[-1]];
}


#endif

// Given pointer to allocated memory, produce array_t handle
static inline array_t get_array_ptr(void *mem) {
	return (array_t)((char *)mem + ARRAY_OVERHEAD);
}

// Get the pointer to the allocated block of memory an array sits in
static inline void *get_mem_ptr(array_t array) {
	return (char *)array - ARRAY_OVERHEAD;
}

static inline typetable *get_type(array_t array) {
	if (!array)
		return NULL;
	return get_header(array)->typetbl;
}

static inline unsigned int length(array_t array) {
	if (!array)
		return 0;
	return get_header(array)->len;
}

static inline void *nth_elem(array_t array, int n) {
	typetable *tytbl = get_type(array);
	return (char *)array + tytbl->element_len * n;
}





// Lowest-level alloc routine. Does not destruct/construct elements
static array_t mem_alloc(typetable *typetbl, unsigned int len) {
	void *mem = malloc(ARRAY_OVERHEAD + len * typetbl->element_len);
	if (!mem)
		debug(errFatal, "out of memory");
	array_t array = get_array_ptr(mem);
#ifdef VALGRIND_ARRAYS
	*(short *)mem = alloc_header();
#endif
	array_header *header = get_header(array);
	header->typetbl = typetbl;
	header->len = len;
	header->temp = 0;
	return array;
}

// Lowest-level free routine. Does not destruct/construct elements
static inline void mem_free(array_t array) {
	if (!array)
		return;
#ifdef VALGRIND_ARRAYS
	free_header(*(short *)get_mem_ptr(array));
#endif
	free(get_mem_ptr(array));
}

static array_t mem_resize(array_t array, unsigned int len) warn_unused_result;

// Lowest-level resize routine. Does not destruct/construct elements
static array_t mem_resize(array_t array, unsigned int len) {
	void *mem = get_mem_ptr(array);
	array_header *header = get_header(array);
	unsigned int arraysize = ARRAY_OVERHEAD + len * header->typetbl->element_len;
#if 1
	void *newmem = realloc(mem, arraysize);
#else
	unsigned int oldsize = ARRAY_OVERHEAD + header->len * header->typetbl->element_len;
	if (oldsize > arraysize)
		oldsize = arraysize;
	void *newmem = malloc(arraysize);
	memset(newmem, 255, arraysize);
	memcpy(newmem, mem, oldsize);
	memset(mem, 255, oldsize);
	free(mem);
#endif
	if (!newmem)
		debug(errFatal, "out of memory");
#ifndef VALGRIND_ARRAYS
	header = newmem;
#endif
	header->len = len;
	return get_array_ptr(newmem);
}




/*
array_t array_create_temp(struct typetable *typetbl, int len, ...) {
	va_list va;
	va_start(va, len);
	array_t array = mem_alloc(typetbl, len);
	FnCopy copier = typetbl->copy;
	//FIXME
	va_end(va);
	return array;
}
*/

/*
void resize_and_init() {

	int oldsize = array->size;
	int oldlen = dim->ubound + 1;

	redim_1D();

	if (len > oldlen) {
		char *elmt = &array->data[oldlen * element_len];
		if (ctor == NULL) {
			memset(elmt, 0, array->size - oldsize);
		} else {
			for (int i = len - oldlen; i; --i) {
				array_func_ctor_arg(elmt);
				elmt += element_len;
			}
		}
	}
}
*/


// Call destructor on elements [from, to)
// Note: does not set array length or resize memory
static void delete_elements(array_t array, unsigned int from, unsigned int to) {
	typetable *typetbl = get_header(array)->typetbl;

	if (typetbl->dtor) {
		// Destruct in reverse order, just like libfb
		// (that only matters when this is used to resize or delete an array)
		void *elmt = (char *)array + (to - 1) * typetbl->element_len;
		for (int num2delete = to - from; num2delete > 0; --num2delete) {
			typetbl->dtor(elmt);
			elmt -= typetbl->element_len;
		}
	}
}

static void copy_elements(void *dest, void *src, unsigned int len, typetable *tytbl) {
	if (tytbl->copyctor) {
		for (int i = 0; i < len; ++i) {
			tytbl->copyctor(dest, src);
			src += tytbl->element_len;
			dest += tytbl->element_len;
		}
	}
	else
		memcpy(dest, src, tytbl->element_len * len);
}

static void init_elements(void *dest, unsigned int len, typetable *tytbl) {
	if (tytbl->ctor) {
		for (int i = 0; i < len; ++i) {
			tytbl->ctor(dest);
			dest += tytbl->element_len;
		}
	}
	else
		memset(dest, 0, tytbl->element_len * len);
}



//////////////////////////////////////// Public Functions //////////////////////////////////////////

// With a couple exceptions, NO NULL POINTERS ALLOWED
// key: A - NULL allowed | W - warning | E - error


// (E)
array_t array_append(array_t *array, void *value) {
	if (!*array)
		throw_error("array_append: array uninitialised");

	typetable *tytbl = get_type(*array);
	unsigned int len = length(*array);

	if (value >= (void *)*array && value < (void *)nth_elem(*array, len)) {
		// Special logic: you're appending an element of an array to itself, but what
		// if realloc moves the array? Rather than throw an error, be benevolent.
		void *temp = alloca(tytbl->element_len);
		memcpy(temp, value, tytbl->element_len);
		value = temp;
	}

	*array = mem_resize(*array, len + 1);

	void *elmt = nth_elem(*array, len);
	if (tytbl->copyctor) {
		tytbl->copyctor(elmt, value);
	} else {
		memcpy(elmt, value, tytbl->element_len);
	}
	return *array;
}


// Destructive
// (E, A)
array_t array_extend_d(array_t *dest, array_t *src) {
	if (!*src)
		return *dest;  // All done!

	if (!*dest) {
		throw_error("array_extend_d: dest array not initialised");
		/*
		debuginfo("array_extend_d: dest array not initialised");
		*dest = *src;
		*src = NULL;
		return *dest;
		// Voilà!
		*/
	}

	if (*dest == *src)
		throw_error("array_extend_d: trying to destructively extend array onto itself!");

	typetable *tytbl = get_type(*dest);
	unsigned int len = length(*dest);
	unsigned int len2 = length(*src);
	if (get_type(*src) != tytbl)
		throw_error("array_extend_d: these arrays have different types! %s and %s", tytbl->name, get_type(*src)->name);

	*dest = mem_resize(*dest, len + len2);

	memcpy((char *)*dest + tytbl->element_len * len, *src, tytbl->element_len * len2);

	mem_free(*src);
	*src = NULL;
	return *dest;
}

// (E, E)
array_t array_extend(array_t *dest, array_t *src) {
	if (!*dest || !*src)
		throw_error("array_extend: array uninitialised");

	if (get_header(*src)->temp)
		return array_extend_d(dest, src);

	typetable *tytbl = get_type(*dest);
	unsigned int len = length(*dest);
	unsigned int len2 = length(*src);

	*dest = mem_resize(*dest, len + len2);
	//If *dest == *src, then we are still OK

	void *from = *src, *to = nth_elem(*dest, len); //(char *)*dest + (tytbl->element_len * len);
	copy_elements(to, from, len2, tytbl);
	return *dest;
}

// Performs a copy
// (A, W)
void array_assign(array_t *dest, array_t *src) {
	if (*dest) {
		// Delete old dest array
		delete_elements(*dest, 0, length(*dest));
		mem_free(*dest);
		*dest = NULL;
	}

	if (!*src) {
		debuginfo("array_assign: NULL src");
		return;
	}

	array_header *srch = get_header(*src);
	if (srch->temp) {
		srch->temp = 0;
		*dest = *src;
		*src = NULL;  //Safety
		return;
	}

	// Copy required
	array_t newa = mem_alloc(srch->typetbl, srch->len);
	copy_elements(newa, *src, srch->len, srch->typetbl);
	*dest = newa;
}

// Moves an array from the second variable to the first
// (A, W)
void array_assign_d(array_t *dest, array_t *src) {
	if (*dest) {
		// Delete old dest array
		delete_elements(*dest, 0, length(*dest));
		mem_free(*dest);
		*dest = NULL;
	}

	if (!*src) {
		debuginfo("array_assign: NULL src");
		return;
	}

	get_header(*src)->temp = 0;
	*dest = *src;
	*src = NULL;  //Safety
}

// (A)
void array_new(array_t *array, int len, typetable *tbl) {
	if (*array)
		array_free(array);
	*array = mem_alloc(tbl, len);
	init_elements(*array, len, tbl);
}

// (A)
void array_free(array_t *array) {
	if (*array) {
		delete_elements(*array, 0, length(*array));
		mem_free(*array);
		*array = NULL;
	}
}

// (E)
void array_resize(array_t *array, unsigned int len) {
	if (!*array) {
		throw_error("array_resize: array uninitialised");
	}

	typetable *tytbl = get_type(*array);
	unsigned int oldlen = length(*array);

	if (len < oldlen)
		delete_elements(*array, len, oldlen);

	*array = mem_resize(*array, len);

	if (len > oldlen)
		init_elements((char *)*array + tytbl->element_len * oldlen, len - oldlen, tytbl);
}

// (E)
void *array_expand(array_t *array, unsigned int amount) {
	unsigned int oldlen = length(*array);
	array_resize(array, oldlen + amount);
	return nth_elem(*array, oldlen);
}

// (A)
int array_length(array_t array) {
	return length(array);
}

// (A)
array_t array_end(array_t array) {
	if (!array)
		return NULL;
	return (array_t)((char *)array + get_type(array)->element_len * length(array));
}

// (E)
// Note that n is signed for more informative error messages
void *array_index(array_t array, int n) {
	if (!array)
		throw_error("array_index: array uninitialised");
	if (n < 0 || n >= length(array)) {
		debug(errPromptBug, "array_index: out of bounds array access, index %d in length %d array of %s", n, length(array), get_type(array)->name);
		return NULL;
	}
	return nth_elem(array, n);
}

// (A)
typetable *array_type(array_t array) {
	return get_type(array);
}

// (E)
array_t array_temp(array_t array) {
	if (!array)
		throw_error("array_temp: array uninitialised");

	get_header(array)->temp = 1;
	return array;
}

// (E)
int array_is_temp(array_t array) {
	if (!array)
		throw_error("array_is_temp: array uninitialised");
	return get_header(array)->temp;
}

// (E)
array_t array_sort(array_t array, FnCompare compare) {
	if (!array)
		throw_error("array_sort: array uninitialised");
	typetable *tytbl = get_type(array);
	if (!compare) {
		compare = tytbl->comp;
		if (!compare)
			throw_error("array_sort: no comparison function for %s", tytbl->name);
	}
	qsort(array, length(array), tytbl->element_len, compare);
	return array;
}

// Returns -1 for true
int array_equal(array_t lhs, array_t rhs) {
	if (!lhs || !rhs)
		throw_error("array_equal: array uninitialised");

	int len = length(lhs);
	if (length(rhs) != len)
		return 0;
	typetable *tytbl = get_type(lhs);	
	if (get_type(rhs) != tytbl)
		return 0;   // Don't throw an error
	FnCompare comp = tytbl->inequal;
	if (!comp)
		comp = tytbl->comp;
	if (comp) {
		char *e1 = (char *)lhs, *e2 = (char *)rhs;
		for (int i = 0; i < len; ++i) {
			if (comp(e1, e2))
				return 0;
			e1 += tytbl->element_len;
			e2 += tytbl->element_len;
		}
		return -1;
	} else
		return memcmp(lhs, rhs, tytbl->element_len * len) ? 0 : -1;
}

// (E)
int array_inequal(array_t *lhs, array_t *rhs) {
	return array_equal(*lhs, *rhs) ? 0 : -1;
}

// (E)
int array_find(array_t array, void *value) {
	if (!array)
		throw_error("array_find: array uninitialised");

	typetable *tytbl = get_type(array);
	unsigned int len = length(array);

	FnCompare comp = tytbl->inequal;
	if (!comp)
		comp = tytbl->comp;
	if (!comp)
		throw_error("array_find: no comparison function for %s", tytbl->name);

	char *elem = (char *)array;
	for (int i = 0; i < len; ++i) {
		if (comp(elem, value) == 0)
			return i;
		elem += tytbl->element_len;
	}
	return -1;
}

// (E)
array_t array_insert(array_t *array, int pos, void *value) {
	if (!*array)
		throw_error("array_insert: array uninitialised");

	typetable *tytbl = get_type(*array);
	unsigned int len = length(*array);

	if (pos < 0 || pos > len) {
		debug(errPromptBug, "array_insert: tried to insert at position %d of array of length %d", pos, len);
		return *array;
	}

	if (value >= (void *)*array && value < (void *)nth_elem(*array, len)) {
		// Special logic: you're inserting an element array[i] of an array into itself, but
		// realloc could move the array or maybe i >= pos
		void *temp = alloca(tytbl->element_len);
		memcpy(temp, value, tytbl->element_len);
		value = temp;
	}

	*array = mem_resize(*array, len + 1);

	// Move back end of array
	void *elmt = nth_elem(*array, pos);
	if (pos < len)
		memmove(elmt + tytbl->element_len, elmt, (len - pos) * tytbl->element_len);

	if (tytbl->copyctor) {
		tytbl->copyctor(elmt, value);
	} else {
		memcpy(elmt, value, tytbl->element_len);
	}

	return *array;
}

// Delete the elements in the range [from, to)
// (E)
array_t array_delete_slice(array_t *array, int from, int to) {
	if (!*array)
		throw_error("array_delete_slice: array uninitialised");

	typetable *tytbl = get_type(*array);
	unsigned int len = length(*array);

	if (from < 0 || to > len || from > to) {
		debug(errPromptBug, "array_delete_slice: invalid slice [%d, %d) of array of length %d", from, to, len);
		return *array;
	}
	if (from == to)
		return *array;

	delete_elements(*array, from, to);

	// Move forward end of array
	void *from_ptr = nth_elem(*array, from);
	void *to_ptr = nth_elem(*array, to);
	memmove(from_ptr, to_ptr, (len - to) * tytbl->element_len);

	return *array = mem_resize(*array, len - (to - from));
}

// (E)
int array_remove(array_t *array, void *value) {
	int pos = array_find(*array, value);
	if (pos == -1)
		return -1;
	array_delete_slice(array, pos, pos + 1);
	return pos;
}

// (E)
array_t array_reverse(array_t *array) {
	if (!*array)
		throw_error("array_reverse: array uninitialised");

	typetable *tytbl = get_type(*array);
	unsigned int len = length(*array);

	array_t newmem = mem_alloc(tytbl, len);
	char *dest = (char *)newmem, *src = nth_elem(*array, len - 1);

	for (int i = len; i; i--) {
		memcpy(dest, src, tytbl->element_len);
		dest += tytbl->element_len;
		src -= tytbl->element_len;
	}

	mem_free(*array);
	return *array = newmem;
}
