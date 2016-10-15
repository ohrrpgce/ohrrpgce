
#include "config.bi"
#include "string.bi"
#include "const.bi"

extern "C"
	type FnDebugHook as sub (byval errorlevel as ErrorLevelEnum, byval msg as zstring ptr)
	declare sub set_debug_hook (byval new_debug_hook as FnDebugHook)
end extern

TYPE testPtr as function() as integer

extern pauseTime as double
extern errorpos as integer
extern errorfile as string
dim pauseTime as double
dim errorpos as integer
dim errorfile as string

Randomize 42

sub doTest(t as string, byval theTest as testPtr)
	static num as integer = 0
	
	num += 1
	
	print "Test #" & num & ": " & t & "... ";
	
	dim as double start, finish, diff
	dim as integer ret
	
	pauseTime = 0
	
	start = timer
	
	ret = theTest()
	
	finish = timer - pauseTime
	
	diff = finish - start
	
	do while diff < 0
		diff += 86400
	loop
	
	'diff *= 1000000
	
	if ret > 0 then
		print "FAIL (on line " & errorpos & " in " & errorfile & ")"
		end num
	elseif ret = 0 then
		print "Pass"
	else
		print "SKIP"
	end if
	
	if(diff < 1) then
		diff *= 1000
		if(diff < 10) then
			diff *= 1000
			print "Took " & int(diff) & !" \u03BCs "
		elseif diff < 100 then
			print "Took " & format(diff, "0.0") & " ms "
		else
			print "Took " & int(diff) & " ms "
		end if
	else
		print "Took " & format(diff, "0.00") & " s "
	end if
	
end sub

#define pass return 0
#define fail errorfile = __FILE__ : errorpos = __LINE__ : return 1
#define skip return -1

#macro startTest(t)
	Declare Function t##_TEST() as integer
	doTest(#t, @t##_TEST)
	function t##_TEST() as integer
#endmacro
#define endTest pass : end Function

#macro testEqual(exp1, exp2)
	Scope
		var temp1 = exp1, temp2 = exp2
		if temp1 <> temp2 then
			print
			print "Expected " #exp1 " = " #exp2
			print "Actually " #exp1 " = " & temp1
			print "         " #exp2 " = " & temp2
			errorfile = __FILE__
			errorpos = __LINE__
			return 1
		end if
	End Scope
#endmacro

function ask(q as string) as integer
	dim ret as string, r as integer
	
	dim as double s, f, d
	
	s = timer
	
	q = q & " (y/n)"
	
	again:
	print q
	ret = input(1)
	
	if lcase(ret) <> "y" and lcase(ret) <> "n" then goto again
	
	r = lcase(ret) = "y"
	
	f = timer
	
	d = s - f
	
	do while d < 0
		d += 86400
	loop
	
	pauseTime += d
	
	return r
end function
