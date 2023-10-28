/* OHRRPGCE - Generic vector/array library
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
 * Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>

#include "errorlog.h"
#include "array.h"


typedef struct _array_header {
	typetable *typetbl;
	// Unsigned bitfields, to avoid needing sign extension
	unsigned len:31;
	unsigned temp:1;
	int allocated;  // Amount of memory allocated, in number of elements
} array_header;

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

array_header **header_table = NULL;

// Returns an index in header_table containing the header pointer
static unsigned short alloc_header() {
	if (!header_table) {
		header_table = calloc(65536 * sizeof(array_header *), 1);
	}
	for (int i = 0; i < 65536; i++) {
		if (!header_table[i]) {
			header_table[i] = malloc(sizeof(array_header));
			return i;
		}
	}
	debug(errFatalError, "header_table is full: 65536 arrays are allocated. Compile without -DVALGRIND_ARRAYS");
	abort();
}

static void free_header(short id) {
	if (!header_table[id])
		throw_error("alloc_header: header_table[%d] already free", id);
	free(header_table[id]);
	header_table[id] = NULL;
}

//static inline
array_header *get_header(array_t array) {
	if (!array)
		return NULL;
	return header_table[((unsigned short *)array)[-1]];
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

static inline int length(array_t array) {
	if (!array)
		return 0;
	return get_header(array)->len;
}

static inline void *nth_elem(array_t array, int n) {
	typetable *tytbl = get_type(array);
	return (char *)array + tytbl->element_len * n;
}

#ifdef has_overflow_builtins
# define smul_overflow __builtin_smul_overflow
# define sadd_overflow __builtin_sadd_overflow
#else

// In modern GCC and clang can use __builtin_smul_overflow and
// __builtin_sadd_overflow instead.
// Set *res = a*b, or return true if overflows
static bool smul_overflow(int a, int b, int *res) {
	*res = a * b;  // technically undefined behaviour, don't care
	if (b && *res / b != a)
		return true;
	return false;
}
// Set *res = a+b, or return true if overflows
static bool sadd_overflow(int a, int b, int *res) {
	if ((a > 0 && b > INT_MAX - a) ||
	    (a < 0 && b < INT_MIN - a))
		return true;
	*res = a + b;
	return false;
}

#endif  /* !has_overflow_builtins */

// Lowest-level alloc routine. Does not destruct/construct elements
// len: length of the array
// alloc: allocate memory for this many element (must be >= len)
static array_t mem_alloc(typetable *typetbl, int len, int alloc) {
	if (alloc < len || len < 0)
		debug(errFatalError, "mem_alloc: alloc == %d < len == %d", alloc, len);
	int arraysize;
	if (smul_overflow(alloc, typetbl->element_len, &arraysize) ||
	    sadd_overflow(arraysize, ARRAY_OVERHEAD, &arraysize))
		debug(errFatalError, "mem_alloc: overflow; vector alloc=%d", alloc);
	//printf("alloc arraysize %d len %d alloc %d ellen %d\n", arraysize, len, alloc, typetbl->element_len);
	void *mem = malloc(arraysize);
	if (!mem)
		debug(errFatalError, "mem_alloc: out of memory");
	array_t array = get_array_ptr(mem);
#ifdef VALGRIND_ARRAYS
	*(unsigned short *)mem = alloc_header();
#endif
	array_header *header = get_header(array);
	header->typetbl = typetbl;
	header->temp = 0;
	header->len = len;
	header->allocated = alloc;
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

// For debugging: Whether to move the data to a new allocation every single time
// it's resized to check that everyone correctly handles this.
#define FORCE_REALLOC 0

static array_t mem_resize(array_t array, int len) warn_if_result_unused;

// Lowest-level resize routine. Does not destruct/construct elements
static array_t mem_resize(array_t array, int len) {
	void *mem = get_mem_ptr(array);
	array_header *header = get_header(array);

	// Decide whether to grow or shrink memory
	int alloclen;  // Amount to allocate
	if (len > header->allocated)
		alloclen = len + len / 2 + 3;
	else if (len + 1 < header->allocated / 4)
		alloclen = len;
#if !FORCE_REALLOC
	else {
		header->len = len;
		return array;
	}
#else
	alloclen = len;
#endif

	int arraysize;
	if (smul_overflow(alloclen, header->typetbl->element_len, &arraysize) ||
	    sadd_overflow(arraysize, ARRAY_OVERHEAD, &arraysize))
		debug(errFatalError, "mem_resize: overflow; vector len=%d", len);
#if !FORCE_REALLOC
	//printf("realloc to arraysize %d len %d alloc %d\n", arraysize, len, alloclen);
	void *newmem = realloc(mem, arraysize);
#else
	// For debugging: Allocate new memory
	int oldsize = ARRAY_OVERHEAD + header->len * header->typetbl->element_len;
	if (oldsize > arraysize)
		oldsize = arraysize;
	void *newmem = malloc(arraysize);
	memset(newmem, 255, arraysize);
	memcpy(newmem, mem, oldsize);
	memset(mem, 255, oldsize);
	free(mem);
#endif
	if (!newmem)
		debug(errFatalError, "out of memory");
#ifndef VALGRIND_ARRAYS
	header = newmem;
#endif
	header->len = len;
	header->allocated = alloclen;
	return get_array_ptr(newmem);
}




/*
array_t array_create_temp(struct typetable *typetbl, int len, ...) {
	va_list va;
	va_start(va, len);
	array_t array = mem_alloc(typetbl, len, len);
	FnCopyCtor copier = typetbl->copyctor;
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
static void delete_elements(array_t array, int from, int to) {
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

static void copy_elements(void *dest, void *src, int len, typetable *tytbl) {
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

static void init_elements(void *dest, int len, typetable *tytbl) {
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
	int len = length(*array);

	void *moved_from = NULL;
	if (value >= (void *)*array && value < (void *)nth_elem(*array, len)) {
		// Special logic: you're appending an element of an array to itself, but what
		// if realloc moves the array? Rather than throw an error, be benevolent.
		moved_from = *array;
	}

	*array = mem_resize(*array, len + 1);

	if (moved_from)
		value += (void*)*array - moved_from;

	void *elmt = nth_elem(*array, len);
	copy_elements(elmt, value, 1, tytbl);
	return *array;
}


// Destructive
// (E, A)
array_t array_extend_d(array_t *dest, array_t *src) {
	if (!*dest) {
		throw_error("array_extend_d: dest array not initialised");
		/*
		debug(errBug, "array_extend_d: dest array not initialised");
		*dest = *src;
		*src = NULL;
		return *dest;
		// Voilà!
		*/
	}
	if (!*src)
		return *dest;  // All done!

	if (*dest == *src)
		throw_error("array_extend_d: trying to destructively extend array onto itself!");

	typetable *tytbl = get_type(*dest);
	int len = length(*dest);
	int len2 = length(*src);
	if (get_type(*src) != tytbl)
		throw_error("array_extend_d: these arrays have different types! %s and %s", tytbl->name, get_type(*src)->name);

	*dest = mem_resize(*dest, len + len2);

	memcpy(nth_elem(*dest, len), *src, tytbl->element_len * len2);

	mem_free(*src);
	*src = NULL;
	return *dest;
}

// (E, A)
array_t array_extend(array_t *dest, array_t *src) {
	if (!*dest)
		throw_error("array_extend: array uninitialised");
	if (!*src)
		return *dest;

	if (get_header(*src)->temp)
		return array_extend_d(dest, src);

	typetable *tytbl = get_type(*dest);
	int len = length(*dest);
	int len2 = length(*src);

	*dest = mem_resize(*dest, len + len2);
	//If *dest == *src, then we are still OK

	void *from = *src, *to = nth_elem(*dest, len);
	copy_elements(to, from, len2, tytbl);
	return *dest;
}

// Performs a copy
// (A, W)
void array_assign(array_t *dest, array_t *src) {
	if (*dest) {
		// Delete old dest array
		// TODO: it's not necessary to free the old memory
		delete_elements(*dest, 0, length(*dest));
		mem_free(*dest);
		*dest = NULL;
	}

	if (!*src) {
		debug(errBug, "array_assign: NULL src");
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
	array_t newa = mem_alloc(srch->typetbl, srch->len, srch->len);
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
		debug(errBug, "array_assign: NULL src");
		return;
	}

	get_header(*src)->temp = 0;
	*dest = *src;
	*src = NULL;  //Safety
}

// (A)
void array_new(array_t *array, int len, int reserve, typetable *tbl) {
	if (len < 0) {
		throw_error("array_new: invalid length %d", len);
	}
	if (reserve < 0) {
		throw_error("array_new: invalid reserve %d", reserve);
	}
	if (tbl->element_len <= 0) {
		throw_error("array_new: invalid element_len %d", tbl->element_len);
	}
	if (*array)
		array_free(array);
	*array = mem_alloc(tbl, len, len + reserve);
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
void array_resize(array_t *array, int len) {
	if (!*array) {
		throw_error("array_resize: array uninitialised");
	}
	if (len < 0) {
		throw_error("array_resize: invalid length %d", len);
	}

	typetable *tytbl = get_type(*array);
	int oldlen = length(*array);

	if (len < oldlen)
		delete_elements(*array, len, oldlen);

	*array = mem_resize(*array, len);

	if (len > oldlen)
		init_elements((char *)*array + tytbl->element_len * oldlen, len - oldlen, tytbl);
}

// (E)
void *array_expand(array_t *array, int amount) {
	if (amount < 0) {
		// The returned pointer would be off the end of the array
		throw_error("array_expand: invalid amount %d. Use array_shrink instead", amount);
	}

	int oldlen = length(*array);
	array_resize(array, oldlen + amount);
	return nth_elem(*array, oldlen);
}

// (E)
// Reduce length. Don't care if you pass a negative amount, which expands the array.
void array_shrink(array_t *array, int amount) {
	array_resize(array, length(*array) - amount);
}

// (A)
int array_length(array_t array) {
	return length(array);
}

// (A)
array_t array_end(array_t array) {
	if (!array)
		return NULL;
	return nth_elem(array, length(array));
}

// (E)
// Note that n is signed for more informative error messages
void *array_index(array_t array, int n) {
	if (!array)
		throw_error("array_index: array uninitialised");
	if (n < 0 || n >= length(array)) {
		debug(errShowBug, "array_index: out of bounds array access, index %d in length %d array of %s", n, length(array), get_type(array)->name);
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
boolint array_is_temp(array_t array) {
	if (!array)
		throw_error("array_is_temp: array uninitialised");
	return get_header(array)->temp ? -1 : 0;
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
boolint array_equal(array_t lhs, array_t rhs) {
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
boolint array_inequal(array_t *lhs, array_t *rhs) {
	return array_equal(*lhs, *rhs) ? 0 : -1;
}

// (E)
int array_find(array_t array, void *value) {
	if (!array)
		throw_error("array_find: array uninitialised");

	typetable *tytbl = get_type(array);
	int len = length(array);

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
	int len = length(*array);

	if (pos < 0 || pos > len) {
		debug(errShowBug, "array_insert: tried to insert at position %d of array of length %d", pos, len);
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

	copy_elements(elmt, value, 1, tytbl);
	return *array;
}

// Delete the elements in the range [from, to)
// (E)
array_t array_delete_slice(array_t *array, int from, int to) {
	if (!*array)
		throw_error("array_delete_slice: array uninitialised");

	typetable *tytbl = get_type(*array);
	int len = length(*array);

	// Cast, and checking to < 0, is just to silence an annoying warning due to a gcc bug
	if (from < 0 || to < 0 || to > len || (unsigned int)from > (unsigned int)to) {
		debug(errShowBug, "array_delete_slice: invalid slice [%d, %d) of array of length %d", from, to, len);
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
	int len = length(*array);

	array_t newmem = mem_alloc(tytbl, len, len);
	char *dest = (char *)newmem, *src = nth_elem(*array, len - 1);

	for (int i = len; i; i--) {
		memcpy(dest, src, tytbl->element_len);
		dest += tytbl->element_len;
		src -= tytbl->element_len;
	}

	mem_free(*array);
	return *array = newmem;
}

/****************************** Priority Queue *******************************/
// Binary heap priority queue to retrieve minimum element

#define LEFT_CHILD(idx) (2*idx+1)
#define PARENT(idx) ((idx-1)/2)

// (E)
// Remove and delete the smallest element (read it yourself before popping)
// and preserve the heap invariant.
// compare is optional.
void array_heappop(array_t *array, FnCompare compare)
{
	if (!array)
		throw_error("array_heappop: array uninitialised");

	typetable *tytbl = get_type(*array);
	int len = length(*array);
	if (len == 0)
		throw_error("array_heappop: pop from empty queue");
	if (!compare) {
		compare = tytbl->comp;
		if (!compare)
			throw_error("array_heappop: no comparison function defined for %s", tytbl->name);
	}

	// Delete root element
	delete_elements(*array, 0, 1);

	// We pop off the last element and look where to put it (don't resize yet)
	int hole = 0;
	void *to_insert = nth_elem(*array, len - 1);
	len--;

	if (len) {
		// Bubble up the hole from root until finding a spot to put to_insert
		while (LEFT_CHILD(hole) < len) {
			// Find the smallest child
			int left_child = LEFT_CHILD(hole), right_child = left_child + 1;
			int smallest = left_child;
			if (right_child < len && compare(nth_elem(*array, right_child), nth_elem(*array, left_child)) <= 0)
				smallest = right_child;

			if (compare(to_insert, nth_elem(*array, smallest)) <= 0)
				break;  // at least as small as both children
			memcpy(nth_elem(*array, hole), nth_elem(*array, smallest), tytbl->element_len);
			hole = smallest;
		}
		memcpy(nth_elem(*array, hole), to_insert, tytbl->element_len);
	}

	// Now update the array length
	*array = mem_resize(*array, len);
}

// (E)
// Add a new value to a heap; returns its location
// compare is optional
int array_heappush(array_t *array, void *value, FnCompare compare)
{
	if (!array)
		throw_error("array_heappush: array uninitialised");

	typetable *tytbl = get_type(*array);
	int len = length(*array);
	if (!compare) {
		compare = tytbl->comp;
		if (!compare)
			throw_error("array_heappush: no comparison function defined for %s", tytbl->name);
	}

	void *moved_from = NULL;
	if (value >= (void *)*array && value < (void *)nth_elem(*array, len)) {
		// Special logic: you're push a copy of an element of an array, but what
		// if realloc moves the array? Rather than throw an error, be benevolent.
		moved_from = *array;
	}

	*array = mem_resize(*array, len + 1);

	if (moved_from)
		value += (void*)*array - moved_from;

	// Start from end and bubble down the value until its parent isn't larger
	int hole = len;  // where to place
	while (hole > 0) {
		int parent = PARENT(hole);
		if (compare(nth_elem(*array, parent), value) <= 0)
			break;
		memcpy(nth_elem(*array, hole), nth_elem(*array, parent), tytbl->element_len);
		hole = parent;
	}
	// Place *value
	copy_elements(nth_elem(*array, hole), value, 1, tytbl);
	return hole;
}
