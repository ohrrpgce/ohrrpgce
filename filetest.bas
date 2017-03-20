' These tests focus on OPENFILE and other basic functions in filelayer.cpp and util.bas,
' but even OPENFILE tests are not comprehensive (e.g. don't actually test the messaging,
' mechanism, or ACCESS_READ_WRITE, or ACCESS_ANY, etc)

#include "testing.bi"
#include "lumpfile.bi"
#include "util.bi"

dim shared fh as integer
dim shared num_errors as integer = 0

#define DBG(x)
'#define DBG(x) ?x

startTest(OPEN)
	fh = freefile
	' Need to be in a writable dir
	if open("_writetest.tmp" access write as fh) then fail
	if close(fh) then fail
endTest

' Hook everything
function openhook_filter(filename as string, writable as boolint, writes_allowed as boolint) as FilterActionEnum
	DBG("openhook_filter(" & filename & ", " & writable & ", " & writes_allowed & ")")
	if writes_allowed = NO and writable then
		DBG("(disallowed)")
		num_errors += 1
		return FilterActionEnum.deny
	end if
	return FilterActionEnum.hook
end function

' Allow writing to hooked files
set_OPEN_hook @openhook_filter, YES, NULL

startTest(explicitReadNonexistentFiles)
	' Cleanup from previous failure
	safekill "_nonexistent_file.tmp"

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

startTest(makeReadOnly)
	' setwriteable only implemented on Windows
	#ifdef __FB_WIN32__
		' Cleanup previous run
		if isfile("_testreadonly.tmp") then
			setwriteable("_testreadonly.tmp", YES)
			safekill("_testreadonly.tmp")
		end if

		if openfile("_testreadonly.tmp", for_binary, fh) then fail
		if close(fh) then fail
		if setwriteable("_testreadonly.tmp", NO) = NO then fail
	#endif
endTest

startTest(get_file_type)
	if get_file_type("") <> fileTypeDirectory then fail  ' "" is the current directory
	if get_file_type("_testfile.tmp") <> fileTypeFile then fail
	if get_file_type(curdir & SLASH & "_testfile.tmp") <> fileTypeFile then fail
	if get_file_type("_nonexistent_file.tmp") <> fileTypeNonexistent then fail
	if get_file_type("_nonexistent_file.tmp") <> fileTypeNonexistent then fail  ' Didn't create it
	if get_file_type("_nonexistent_file.tmp" SLASH) <> fileTypeNonexistent then fail
	if get_file_type(curdir) <> fileTypeDirectory then fail
	if get_file_type(curdir & SLASH & "..") <> fileTypeDirectory then fail
	if get_file_type("/foo/bar/") <> fileTypeNonexistent then fail  ' Not a valid path
	' Will print an error message on Unix (is a file, not a dir)
	if get_file_type("_testfile.tmp" SLASH "file") <> fileTypeNonexistent then fail
	' Read-only and special files/dirs
	#ifdef __FB_UNIX__
		if get_file_type("/bin/sh") <> fileTypeFile then fail
		if get_file_type("/etc/sudoers") <> fileTypeFile then fail
		if get_file_type("/bin/") <> fileTypeDirectory then fail
		if get_file_type("/dev/tty") <> fileTypeOther then fail
	#elseif defined(__FB_WIN32__)
		if get_file_type("_testreadonly.tmp") <> fileTypeFile then fail
		if get_file_type("C:\windows\") <> fileTypeDirectory then fail   'Note: not actually readonly
		' Oops, this doesn't actually work, I guess GetFileAttributes can't be used for devices
		'if get_file_type("\\.\C:") <> fileTypeOther then fail  ' Drive device
	#endif
endTest

startTest(fileisreadable)
	if fileisreadable("") then fail
	if fileisreadable("_testfile.tmp") = NO then fail
	if fileisreadable("_nonexistent_file.tmp") then fail
	if fileisreadable(CURDIR) then fail   ' OPENing a directory works on Linux
	if fileisreadable("_nonexistent_dir.tmp" & SLASH) then fail
	' Read-only and unreadable files
	#ifdef __FB_UNIX__
		if fileisreadable("/bin/sh") = NO then fail
		if fileisreadable("/etc/sudoers") then fail
	#elseif defined(__FB_WIN32__)
		if fileisreadable("_testreadonly.tmp") = NO then fail
		' Don't have an easy example of an unreadable file
	#endif

	' isfile is just an alias for fileisreadable, so should behave the same
	#ifdef __FB_UNIX__
		if isfile("/etc/sudoers") then fail
	#elseif defined(__FB_WIN32__)
		' Don't have an easy example of an unreadable file
	#endif
endTest

startTest(real_isfile)
	if real_isfile("") then fail
	if real_isfile("_testfile.tmp") = NO then fail
	if real_isfile("_nonexistent_file.tmp") then fail
	if real_isfile(CURDIR) then fail   ' OPENing a directory works on Linux
	if real_isfile("_nonexistent_dir.tmp" & SLASH) then fail
	' Read-only and unreadable files
	#ifdef __FB_UNIX__
		if fileisreadable("/bin/sh") = NO then fail
		if fileisreadable("/etc/sudoers") then fail
	#elseif defined(__FB_WIN32__)
		if fileisreadable("_testreadonly.tmp") = NO then fail
		' Don't have an easy example of an unreadable file
	#endif
endTest

startTest(fileiswriteable)
	if fileiswriteable("") then fail
	if fileiswriteable("_testfile.tmp") = NO then fail
	if fileiswriteable("_nonexistent_file.tmp") = NO then fail
	if isfile("_nonexistent_file.tmp") then fail   ' Should not have created it
	if fileiswriteable(CURDIR) then fail
	' A read-only file
	#ifdef __FB_UNIX__
		if fileiswriteable("/bin/sh") then fail
	#elseif defined(__FB_WIN32__)
		if fileiswriteable("_testreadonly.tmp") then fail
	#endif
endTest

'This exercises opening files for write and deleting them
startTest(diriswriteable)
	if diriswriteable("_testfile.tmp") then fail   ' Will print two error messages on Unix
	if diriswriteable("/tmp/doesnt/exist/surely") then fail
	' We already checked curdir is writable
	if diriswriteable(".") = NO then fail
	if diriswriteable("") = NO then fail
	if diriswriteable("_nonexistent_file.tmp") then fail  ' Shouldn't work if not created yet
	if diriswriteable("_nonexistent_file.tmp" SLASH) then fail
	if get_file_type("_nonexistent_file.tmp") <> fileTypeNonexistent then fail  ' Shouldn't have created
	' A read-only directory
	#ifdef __FB_UNIX__
		if diriswriteable("/bin/") then fail
	#elseif defined(__FB_WIN32__)
		' On NTFS under Windows, the readonly attribute on a folder does nothing, have to use ACLs
		' or a different filesystem to prevent new files!
		'if diriswriteable("C:\windows") then fail
	#endif
endTest

startTest(isdir)
	if isdir("") = NO then fail  'Current directory
	if isdir("_testfile.tmp") then fail
	if isdir("/tmp/doesnt/exist/surely") then fail
	if isdir(".") = NO then fail
	if isdir(absolute_path(CURDIR)) = NO then fail
	if isdir("_nonexistent_file.tmp") then fail
	if isdir("_nonexistent_file.tmp" SLASH) then fail
	if get_file_type("_nonexistent_file.tmp") <> fileTypeNonexistent then fail  ' Shouldn't have created
	' Read-only directories
	#ifdef __FB_UNIX__
		if isdir("/bin/") = NO then fail
		if isdir("/") = NO then fail
	#elseif defined(__FB_WIN32__)
		' Under Windows, these aren't readonly, don't have any examples of read-only directories!
		if isdir("C:\windows") = NO then fail
		if isdir("C:\") = NO then fail
	#endif
endTest

startTest(makeWritable)
	' setwriteable only implemented on Windows
	#ifdef __FB_WIN32__
		if setwriteable("_testreadonly.tmp", YES) = NO then fail
		if fileisreadable("_testreadonly.tmp") = NO then fail
		if safekill("_testreadonly.tmp") = NO then fail
	#endif
endTest

sub error_counter cdecl (byval errorlevel as ErrorLevelEnum, byval msg as zstring ptr)
	DBG("(error reported: " & *msg & ")")
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
