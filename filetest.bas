' These tests focus on OPENFILE and other basic functions in filelayer.cpp and util.bas,
' but even OPENFILE tests are not comprehensive (e.g. don't actually test the messaging,
' mechanism, or ACCESS_READ_WRITE, or ACCESS_ANY, etc)

#include "testing.bi"
#include "lumpfile.bi"
#include "util.bi"

dim shared fh as integer
dim shared num_errors as integer = 0

startTest(OPEN)
	fh = freefile
	' Need to be in a writable dir
	if open("_writetest.tmp" access write as fh) then fail
	if close(fh) then fail
endTest

' Hook everything
function openhook_filter(filename as string, writable as boolint, writes_allowed as boolint) as FilterActionEnum
	? "openhook_filter(" & filename & ", " & writable & ", " & writes_allowed & ")"
	if writes_allowed = NO and writable then
		? "(disallowed)"
		num_errors += 1
		return FilterActionEnum.deny
	end if
	return FilterActionEnum.hook
end function

' Allow writing to hooked files
set_OPEN_hook @openhook_filter, YES, NULL

startTest(explicitReadNonexistentFiles)
	if openfile("_nonexistent_file.tmp", for_binary + access_read, fh) = 0 then
		? "Opening nonexistent file should have failed"
		fail
	end if
endTest

startTest(cantCloseInvalidFiles)
	if close(fh) = 0 then fail
endTest

startTest(writeToFile)
	' Opening a file without explicitly asking for read or write results
	' in a writable file handle.
	if openfile("_testfile.tmp", for_binary, fh) then fail
	print #fh, "hello"
	if close(fh) then fail
endTest

startTest(readFile)
	if openfile("_testfile.tmp", for_binary + access_read, fh) then fail

	dim line_in as string
	input #fh, line_in
	close fh
	'? "reading back: " & line_in
	if line_in <> "hello" then fail
endTest

startTest(fileisreadable)
	if fileisreadable("_testfile.tmp") = NO then fail
endTest

'This exercises opening files for write and deleting them
startTest(diriswriteable)
	if diriswriteable("/tmp/doesnt/exist/surely") then fail
	' We already checked curdir is writable
	if diriswriteable(".") = NO then fail
endTest

sub error_counter cdecl (byval errorlevel as ErrorLevelEnum, byval msg as zstring ptr)
	? "(error reported: " & *msg & ")"
	if errorlevel > errPromptBug then
		? "unexpected error (errlvl=" & errorlevel & "): " & *msg
		end 1
	end if
	num_errors += 1
end sub

set_debug_hook(@error_counter)

' Now disallow writes
set_OPEN_hook @openhook_filter, NO, NULL

startTest(openForWriteFails)
	if openfile("_testfile.tmp", for_binary + access_write, fh) = 0 then
		? "should have failed"
		fail
	end if
	if num_errors <> 1 then fail
endTest

' Opening a file without explicitly asking for read or write now results
' in a read-only open.
startTest(implicitlyReadonly)
	num_errors = 0
	' Can open existing files
	if openfile("_testfile.tmp", for_binary, fh) then fail
	' They're opened for reading only
	print #fh, "something"
	if num_errors < 1 then fail  ' A single print can cause multiple write errors
	' A write-error does not close the file
	if close(fh) then fail

	' Can't open non-existing files, that requires opening for writing
	num_errors = 0
	if openfile("_nonexistent_file.tmp", for_binary, fh) = 0 then fail
	' NOTE: We don't get an error message printed here, because the hook filter doesn't
	' know that the file doesn't exist; instead FB returns 'file not found'
endTest

startTest(killFile)
	if killfile("_testfile.tmp") = NO then fail
	if killfile("_writetest.tmp") = NO then fail
	if fileisreadable("_testfile.tmp") then fail
endTest

clear_OPEN_hook

startTest(canWriteAgain)
	num_errors = 0
	if openfile("_testfile.tmp", for_binary, fh) then fail
	print #fh, "something else"
	if close(fh) then fail
	if killfile("_testfile.tmp") = NO then fail
	if num_errors <> 0 then fail
endTest


? "All tests passed."
