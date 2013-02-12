#include "config.bi"
#include "util.bi"
#include "const.bi"

'''''''''''''''''''''''''''''''' Testing Stuff '''''''''''''''''''''''''''''''''

#include "testing.bi"

#macro assertVector(vec, repr)
	Scope
		dim temp as string = v_str(vec)
		if temp <> repr then
			print
			print "Expected " #vec " = " repr
			print "In fact  " #vec " = " + temp
			errorfile = __FILE__
			errorpos = __LINE__
			return 1
		end if
	End Scope
#endmacro

extern "C"
	type FnDebugHook as sub (byval errorlevel as ErrorLevelEnum, byval msg as zstring ptr)
	declare sub set_debug_hook (byval new_debug_hook as FnDebugHook)
end extern


'''''''''''''''''''''''''''''''''''' Types '''''''''''''''''''''''''''''''''''''


type TypeA
	int1 as integer
	d1 as double
end type

DECLARE_VECTOR_OF_TYPE(TypeA, TypeA)
DEFINE_VECTOR_OF_TYPE(TypeA, TypeA)

type TypeB
	int1 as integer
	str1 as string
	str2 as string
end type

DECLARE_VECTOR_OF_TYPE(TypeB, TypeB)
DEFINE_VECTOR_OF_CLASS(TypeB, TypeB)

type VecMap
	name as string
	vec as integer vector
end type

dim shared as integer VecMap_counter = 0

sub VecMap_construct cdecl (byref this as VecMap)
	this.constructor()
	v_new this.vec
	'? __FUNCTION__
	VecMap_counter += 1
end sub

sub VecMap_set (byref this as VecMap, nam as string, byval foo as integer, byval bar as integer = 0)
	v_resize this.vec, 2
	this.vec[0] = foo
	this.vec[1] = bar
	this.name = nam
	'? __FUNCTION__
end sub

sub VecMap_copy cdecl (byref this as VecMap, byref that as VecMap)
	this.constructor()
	this.name = that.name
	v_copy this.vec, that.vec
	'? __FUNCTION__
	VecMap_counter += 1
end sub

sub VecMap_destruct cdecl (byref this as VecMap)
	v_free this.vec
	this.destructor()
	'? __FUNCTION__
	VecMap_counter -= 1
end sub

function VecMap_compare cdecl (byref lhs as VecMap, byref rhs as VecMap) as integer
	return string_compare(@lhs.name, @rhs.name)
end function

function VecMap_string cdecl (byref this as VecMap) as string
	return this.name + ":" + v_str(this.vec)
end function

DECLARE_VECTOR_OF_TYPE(VecMap, VecMap)
DEFINE_CUSTOM_VECTOR_TYPE(VecMap, VecMap, @VecMap_construct, @VecMap_copy, @VecMap_destruct, @VecMap_compare, NULL, @VecMap_string)


'''''''''''''''''''''''''''''''''''' Tests '''''''''''''''''''''''''''''''''''''


startTest(emptyInteger)
	dim arr as integer vector
	v_new arr
	if v_len(arr) <> 0 then fail
	if v_type(arr) <> @type_table(integer) then fail
	v_new arr, 10000
	if v_len(arr) <> 10000 then fail
	for i as integer = 0 to 9999
		if arr[i] <> 0 then fail
		if *v_at(arr, i) <> 0 then fail
	next
	v_free arr
	if arr <> NULL then fail
endTest

startTest(emptyString)
	dim arr as string vector
	v_new arr
	if v_len(arr) <> 0 then fail
	if v_type(arr) <> @type_table(string) then fail
	v_new arr, 10000
	if v_len(arr) <> 10000 then fail
	for i as integer = 0 to 9999
		if arr[i] <> "" then fail 
	next
	v_free arr
	if arr <> NULL then fail
endTest

startTest(appendInteger)
	dim arr as integer vector
	v_new arr
	v_append arr, 42
	v_append arr, -2
	if v_len(arr) <> 2 then fail
	if @arr[1] <> @(v_end(arr)[-1]) then fail
	if arr[0] <> 42 then fail
	if arr[1] <> -2 then fail
	v_free arr
	if arr <> NULL then fail
endTest

startTest(appendString)
	dim arr as string vector
	v_new arr
	v_append arr, ""
	v_append arr, "foo"
	if v_len(arr) <> 2 then fail
	if arr <> v_end(arr)-2 then fail
	if arr[0] <> "" then fail
	if arr[1] <> "foo" then fail
	v_free arr
	if arr <> NULL then fail
endTest

startTest(findInsertDeleteInteger)
	dim arr as integer vector
	v_new arr
	v_insert arr, 0, 100
	v_insert arr, 1, 101
	v_insert arr, 0, 98
	v_insert arr, 1, 99
	if v_find(arr, 101) <> 3 then fail
	if v_find(arr, 102) <> -1 then fail
	v_expand arr, 2
	arr[4] = 102
	arr[5] = 104
	v_insert arr, 5, 103
	assertVector(arr, "[98, 99, 100, 101, 102, 103, 104]")
	if v_remove(arr, 102) <> 4 then fail
	if v_remove(arr, 105) <> -1 then fail
	assertVector(arr, "[98, 99, 100, 101, 103, 104]")
	v_delete_slice arr, 0, 2
	assertVector(arr, "[100, 101, 103, 104]")
	v_delete_slice arr, 0, 0
	v_delete_slice arr, 4, 4
	assertVector(arr, "[100, 101, 103, 104]")
	v_delete_slice arr, 2, 3
	assertVector(arr, "[100, 101, 104]")
	v_delete_slice arr, 0, v_len(arr)
	assertVector(arr, "[]")
	v_free arr
endTest

startTest(modifyStrings)
	dim arr as string vector
	v_new arr
	v_append arr, ""
	v_append arr, "com"
	dim temp as string = "pound"
	arr[0] += "blarg"
	if "blarg" <> arr[0] then fail
	arr[1] += temp
	if arr[1] <> "compound" then fail
	arr[0] = 42 & arr[1] + " " + arr[0]
	if arr[0] <> "42compound blarg" then fail
	v_append arr, temp + temp
	if arr[2] <> "poundpound" then fail
	if v_len(arr) <> 3 then fail
	v_free arr
endTest

startTest(integerResizing)
	dim arr as integer vector
	v_new arr
	for i as integer = 0 to 40
		v_append arr, 800 + i
	next
	v_resize arr, 23
	if v_len(arr) <> 23 then fail
	v_resize arr, 50
	if v_len(arr) <> 50 then fail
	for i as integer = 0 to 49
		if arr[i] <> iif(i < 23, 800 + i, 0) then fail
		if arr[i] <> *v_at(arr, i) then fail
	next
	v_resize arr, 0
	if v_len(arr) <> 0 then fail
	v_free arr
endTest

startTest(stringResizing)
	dim arr as string vector
	v_new arr
	for i as integer = 0 to 40
		v_append arr, "800" & i
	next
	v_resize arr, 23
	if v_len(arr) <> 23 then fail
	v_resize arr, 50
	if v_len(arr) <> 50 then fail
	for i as integer = 0 to 49
		if arr[i] <> iif_string(i < 23, "800" & i, "") then fail
	next
	v_resize arr, 0
	if v_len(arr) <> 0 then fail
	v_free arr
endTest

startTest(simpleTypeA)
	dim arr as TypeA vector
	v_new arr
	if v_len(arr) <> 0 then fail
	if v_type(arr) <> @type_table(TypeA) then fail
	v_free arr
endTest

startTest(appendTypeA)
	dim arr as TypeA vector
	v_new arr
	'TypeA contains no strings, so you can do this
	'(<TypeA> required, v_append ambiguously overloaded)
	v_append arr, Type<TypeA>(16, 3.14159265358979323)
	'and this
	v_resize arr, v_len(arr) + 1
	arr[1] = Type(17, 1.23)
	'But can't do this:
	'v_append arr, Type<TypeA>()
	dim temp as TypeA ptr
	temp = v_expand(arr)
	if temp <> @arr[2] then fail

	if arr[0].int1 <> 16 then fail
	if arr[0].d1 <> 3.14159265358979323 then fail
	if arr[1].int1 <> 17 then fail
	if arr[1].d1 <> 1.23 then fail
	v_free arr
endTest

startTest(simpleTypeB)
	dim arr as TypeB vector
	v_new arr
	if v_len(arr) <> 0 then fail
	if v_type(arr) <> @type_table(TypeB) then fail
	v_free arr
endTest

startTest(appendTypeB)
	dim arr as TypeB vector
	v_new arr
	'TypeB contains strings. You can't create a temporary with initial elements
	v_append arr, Type<TypeB>()
	with arr[0]
		.str1 = "a"
		.str2 = "b"
		.int1 = 7
	end with
	'Another style
	v_resize arr, v_len(arr) + 1
	with v_end(arr)[-1]
		.str1 = "c"
		.str2 = ""
		.int1 = -1
	end with
	'Here's a simpler way to do the above
	with *v_expand(arr)
		.str1 = "asdf"
		.int1 = 111111111
	end with
	'This is also safe (v_append has a special check for self-appending; normally this
	'would be a subtle error)
	v_append arr, arr[1]
	if arr[0].str1 <> "a" then fail
	if arr[0].str2 <> "b" then fail
	if arr[0].int1 <> 7 then fail
	if arr[2].str1 <> "asdf" then fail
	if arr[2].str2 <> "" then fail
	if arr[2].int1 <> 111111111 then fail
	if arr[3].str1 <> "c" then fail
	if arr[3].str2 <> "" then fail
	if arr[3].int1 <> -1 then fail
	v_free arr
endTest

startTest(ptrArray)
	dim as any ptr vector arr
	dim as integer a,b,c
	v_new arr, 5
	arr[1] = @a
	arr[2] = @b
	arr[3] = @c
	v_append arr, arr[2]
	'?v_str(arr)
	b = 2313
	if 2313 <> *cast(integer ptr, arr[5]) then fail
	v_free arr
endTest

startTest(copyEmptyIntegerArray)
	dim as integer vector a1, a2
	v_new a1
	v_copy a2, a1
	if v_len(a1) <> 0 then fail
	if v_len(a2) <> 0 then fail
	v_free a1
	v_free a2
endTest

startTest(copyStringArray)
	dim as string vector a1, a2
	v_new a1
	v_append a1, "foo"
	v_copy a2, a1
	if v_len(a1) <> 1 then fail
	if v_len(a2) <> 1 then fail
	if a1[0] <> "foo" then fail
	a1[0] = ""
	if a2[0] <> "foo" then fail
	v_free a1
	v_free a2
endTest

startTest(moveStringArray)
	dim as string vector a1, a2
	v_new a1
	v_append a1, "foo"
	v_move a2, a1
	if v_len(a2) <> 1 then fail
	if a2[0] <> "foo" then fail
	if a1 <> NULL then fail
	v_free a2
endTest

startTest(copyTempStringArray)
	dim as string vector a1, a2, temp
	v_new a1
	v_append a1, "foo"
	temp = v_ret(a1)
	if temp <> a1 then fail
	'Note: must pass a1, not 'v_ret(a1)' to v_copy, otherwise it can't wipe a1
	v_copy a2, a1
	if v_len(a2) <> 1 then fail
	if a1 <> NULL then fail
	if a2[0] <> "foo" then fail
	v_free a2
endTest

startTest(moveTempStringArray)
	dim as string vector a1, a2, temp
	v_new a1
	v_append a1, "foo"
	'fact that a1 is temp should be completely ignored
	v_ret(a1)
	v_move a2, a1
	if v_len(a2) <> 1 then fail
	if a1 <> NULL then fail
	if a2[0] <> "foo" then fail
	v_free a2
endTest

startTest(appendToTempArray)
	dim as double vector a1, a2
	v_new a1
	v_ret(a1)
	v_append a1, 13.37
	if v_len(a1) <> 1 then fail
	v_resize a1, 2
	a1[1] = a1[0] * 3.1415
	'should still be temporary, because a1 never appeared as a 'src' argument
	if array_is_temp(a1) = 0 then fail
	v_copy a2, a1
	if a1 <> NULL then fail
	if array_is_temp(a2) then fail
	if v_len(a2) <> 2 then fail
	if a2[1] <> 42.001855 then fail
	v_free a2
endTest

startTest(stringReverse)
	dim arr as string vector
	v_new arr
	v_reverse arr
	assertVector(arr, "[]")
	v_append arr, "foo"
	v_reverse arr
	assertVector(arr, "[""foo""]")
	v_insert arr, 0, "bar"
	v_reverse arr
	assertVector(arr, "[""foo"", ""bar""]")
	v_free arr
endTest

startTest(intReverseSort)
	dim as integer vector arr, arr2
	v_new arr, 10000
	dim as integer sum, i
	for i = 0 to 9999
		arr[i] = rnd * 10000
		sum += arr[i]
	next
	v_sort arr
	for i = 0 to 9998
		if arr[i] > arr[i+1] then fail
		sum -= arr[i]
	next
	sum -= arr[9999]
	if sum <> 0 then fail
	v_copy arr2, arr
	v_reverse arr2
	for i = 0 to 9999
		if arr[i] <> arr2[9999 - i] then fail
	next
	v_free arr
	v_free arr2
endTest

startTest(stringSort)
	dim arr as string vector
	v_new arr, 400
	dim as integer sum, i, j
	for i = 0 to 399
		for j = 0 to 2
			arr[i] += chr(asc("a") + rnd * 25)
		next
		if rnd > 0.5 then arr[i] = ucase(arr[i])
	next
	v_sort arr
	for i = 0 to 398
		'print arr[i],
		if string_compare(@arr[i], @arr[i+1]) > 0 then fail
	next
	v_free arr
endTest

startTest(intArrayCompatibility)
	dim i as integer
	redim array(10) as integer
	dim vec as integer vector
	for i = 0 to 10
		array(i) = i
	next
	array_to_vector vec, array()
	if v_len(vec) <> 11 then fail
	for i = 0 to 10
		if vec[i] <> i then fail
	next
	v_resize vec, 5
	v_reverse vec
	vector_to_array array(), vec
	if lbound(array) <> 0 then fail
	if ubound(array) <> 4 then fail
	for i = 0 to 4
		if array(i) <> 4 - i then fail
	next
	redim array(-1 to -1)
	array_to_vector vec, array()
	if v_len(vec) <> 0 then fail
	redim array(2 to 3)
	vector_to_array array(), vec
	if ubound(array) >= 0 then fail
	v_free vec
endTest

startTest(stringArrayCompatibility)
	dim i as integer
	redim array(-1 to 1) as string
	dim vec as string vector
	array(0) = "index0"
	array(1) = "index1"
	array_to_vector vec, array()
	if v_len(vec) <> 2 then fail
	for i = 0 to 1
		if vec[i] <> "index" & i then fail
	next
	v_resize vec, 5
	redim array(-10 to 10)
	vector_to_array array(), vec
	if lbound(array) <> 0 then fail
	if ubound(array) <> 4 then fail
	if array(1) <> "index1" then fail
	if array(2) <> "" then fail
	v_free vec
endTest

function v_range(byval n as integer) as integer vector
	dim ret as integer vector
	v_new ret, n
	for i as integer = 0 to n - 1
		ret[i] = i
	next
	return v_ret(ret)
end function

startTest(returnVector)
	dim arr as integer vector
	v_copy arr, v_range(5)  'or v_move
	assertVector(arr, "[0, 1, 2, 3, 4]")
	'Test address-of return value
	v_extend_d(arr, v_range(1))   'unneeded _d form
	assertVector(arr, "[0, 1, 2, 3, 4, 0]")
	v_free arr
endTest

startTest(functionalStyle)
	dim arr as integer vector
	v_move arr, v_extend(v_reverse(v_sort(v_append(v_range(5), -100))), v_range(2))
	assertVector(arr, "[4, 3, 2, 1, 0, -100, 0, 1]")
	v_free arr
endTest

startTest(intVectorVectorCtors)
	'Test ctor and copyctor
	dim as integer i
	dim arr as integer vector vector
	v_new arr, 10
	for i = 0 to 9
		if arr[i] = NULL then fail
		if v_len(arr[i]) <> 0 then fail
	next
	v_append arr[9], 998
	v_append arr[9], 999
	dim tmp as integer vector = *v_expand(arr)
	if tmp <> arr[10] then fail
	if v_len(tmp) <> 0 then fail
	v_extend(arr[10], arr[9])   'NOTE: passing tmp as first argument is not allowed!
	arr[9][0] = 997
	assertVector(arr, "[[], [], [], [], [], [], [], [], [], [997, 999], [998, 999]]")
	v_free arr 
endTest

startTest(intVectorVectorAppending)
	dim arr as integer vector vector
	dim tmp as integer vector
	v_new arr
	v_new tmp, 3
	tmp[2] = 1001
	v_append arr, tmp
	tmp[1] = 1010
	v_append arr, tmp
	v_append arr, arr[0]
	arr[2][2] = 1100
	v_new tmp, 1
	tmp[0] = 55
	v_append arr, tmp
	v_free tmp
	dim sums as integer vector
	v_new sums
	for i as integer = 0 to v_len(arr) - 1
		'if i <> 3 andalso v_len(arr[i]) <> 4 then fail
		v_sort arr[i]
		v_append sums, intvec_sum(arr[i])
	next
	assertVector(arr, "[[0, 0, 1001], [0, 1001, 1010], [0, 0, 1100], [55]]")
	if v_len(sums) <> 4 then fail
	if intvec_sum(sums) <> 1001 + 1001 + 1010 + 1100 + 55 then fail
	v_free arr
	v_free sums
endTest

startTest(intVectorVectorCompare)
	dim as integer vector vector arr, arr2
	dim tmp as integer vector
	v_new arr
	v_new arr2
	if v_inequal(arr, arr2) then fail
	v_new tmp, 3
	tmp[1] = 1000
	tmp[2] = 1001
	v_append arr, tmp
	arr[0][0] = 10
	tmp[0] = 10
	v_append arr2, tmp
	assertVector(arr, "[[10, 1000, 1001]]")
	if v_equal(arr, arr2) = 0 then fail
	if v_equal(arr[0], arr2[0]) = 0 then fail
	v_append arr, tmp
	if v_equal(arr, arr2) then fail
	v_append arr2, tmp
	if v_inequal(arr, arr2) then fail
	v_free arr
	v_free arr2
endTest

'First we need to create 'string vector vector' overloads/typetable
DECLARE_VECTOR_OF_TYPE(string vector, string_vector)
DEFINE_VECTOR_VECTOR_OF(string, string)

'string vector vector vector
DECLARE_VECTOR_OF_TYPE(string vector vector, string_vector_vector)
DEFINE_VECTOR_VECTOR_OF(string vector, string_vector)

startTest(stringVectorVectorVector)
	'Test ctor and copyctor
	dim as integer i
	dim arr as string vector vector vector
	v_new arr, 1                                  'arr := [[]]
	if arr[0] = NULL then fail
	if v_len(arr[0]) <> 0 then fail
	v_resize arr[0], 2                            'arr := [[[], []]]
	if v_len(arr[0][1]) <> 0 then fail
	v_append arr[0][1], "~"                       'arr := [[[], ["~"]]]
 	if v_len(arr[0][1]) <> 1 then fail
	if arr[0][1][0] <> "~" then fail
	v_resize arr, 3                               'arr := [[[], ["~"]], [], []]
	v_append arr[1], arr[0][1]                    'arr := [[[], ["~"]], [["~"]], []]
	if @arr[0][1][0] = @arr[1][0][0] then fail
	v_append arr[1][0], "/.."                     'arr := [[[], ["~"]], [["~", "/.."]], []]
	assertVector(arr, "[[[], [""~""]], [[""~"", ""/..""]], []]")
	v_new arr[2], 1                               'arr := [[[], ["~"]], [["~", "/.."]], [[]]]
	v_new arr[2][0], 1                            'arr := [[[], ["~"]], [["~", "/.."]], [[""]]]
	arr[0][1][0] += arr[1][0][1]                  'arr := [[[], ["~/.."]], [["~", "/.."]], [[""]]]
	assertVector(arr, "[[[], [""~/..""]], [[""~"", ""/..""]], [[""""]]]")
	v_resize arr[1], 0                            'arr := [[[], ["~/.."]], [], [[""]]]
	assertVector(arr, "[[[], [""~/..""]], [], [[""""]]]")
	v_free arr 
endTest

startTest(VecMap)
	dim as VecMap vector arr, arr2
	v_new arr, 2
	VecMap_set(arr[1], "N", 0, -1)

	dim temp as VecMap
	VecMap_construct(temp)
	VecMap_set(temp, "E", 1, 0)
	v_append arr, temp
	VecMap_destruct temp

	v_extend(arr, arr)
	assertVector(arr, "[:[], N:[0, -1], E:[1, 0], :[], N:[0, -1], E:[1, 0]]")
	v_resize arr, v_len(arr) - 2
	assertVector(arr, "[:[], N:[0, -1], E:[1, 0], :[]]")
	v_copy arr2, arr
	if v_str(arr2) <> v_str(arr) then fail
	v_extend_d(arr, arr2)
	'if arr2 <> NULL then fail
	assertVector(arr, "[:[], N:[0, -1], E:[1, 0], :[], :[], N:[0, -1], E:[1, 0], :[]]")
	v_free arr
	v_free arr2
	'? VecMap_counter
	if VecMap_counter <> 0 then fail
endTest

'The following should cause valgrind to display 3 'Invalid read' errors, if
'compiled with 'scons valgrind=1 vectortest' (and only two without valgrind=1)
startTest(valgrind)
	dim arr as integer vector
	dim dummy as integer
	v_new arr
	dummy = arr[-1]
	dummy = arr[0]
	v_append arr, 42
	dummy = arr[1]
	v_free arr
endTest


'Most error conditions in array.c raise a errFatalBug error, but bounds checking
'raises errPromptBug errors

dim shared num_errors as integer = 0

sub error_counter cdecl (byval errorlevel as ErrorLevelEnum, byval msg as zstring ptr)
	if errorlevel > errPromptBug then
		print "unexpected error (errlvl=" & errorlevel & "): " & *msg
		end 1
	end if
	num_errors += 1
end sub

startTest(testBoundsChecking)
	num_errors = 0
	set_debug_hook(@error_counter)

	dim arr as integer vector
	v_new arr, 2
	v_at(arr, 0)
	if num_errors then fail
	v_at(arr, 1)
	if num_errors then fail

	v_at(arr, 2)
	if num_errors <> 1 then fail
	v_at(arr, -1)
	if num_errors <> 2 then fail

	set_debug_hook(NULL)
	v_free arr
endTest
