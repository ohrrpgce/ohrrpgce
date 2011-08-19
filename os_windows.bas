'OHHRPGCE COMMON - Windows versions of OS-specific routines
'Please read LICENSE.txt for GNU GPL License details and disclaimer of liability

#include "os.bi"
#include "config.bi"
#include "crt/limits.bi"
#include "crt/stdio.bi"
#include "common.bi"
#include "allmodex.bi"
#include "util.bi"
#include "const.bi"
include_windows_bi()

option explicit


'FormatMessage is such an awfully complex function
function get_windows_error () as string
	dim errcode as integer = GetLastError()
	dim strbuf as string * 256
	FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, NULL, errcode, 0, strptr(strbuf), 255, NULL)
	return strbuf
end function

extern "C"

'==========================================================================================
'                                       Filesystem
'==========================================================================================


function drivelist (drives() as string) as integer
	dim drivebuf as zstring * 1000
	dim drivebptr as zstring ptr
	dim as integer zslen, i

	zslen = GetLogicalDriveStrings(999, drivebuf)

	drivebptr = @drivebuf
	while drivebptr < @drivebuf + zslen
		drives(i) = *drivebptr
		drivebptr += len(drives(i)) + 1
		i += 1
	wend

	drivelist = i
end function

function drivelabel (drive as string) as string
	dim tmpname as zstring * 256
	if GetVolumeInformation(drive, tmpname, 255, NULL, NULL, NULL, NULL, 0) = 0 then
		drivelabel = "<not ready>"
	else
		drivelabel = tmpname
	end if
end function

function isremovable (drive as string) as integer
	isremovable = GetDriveType(drive) = DRIVE_REMOVABLE
end function

function hasmedia (drive as string) as integer
	hasmedia = GetVolumeInformation(drive, NULL, 0, NULL, NULL, NULL, NULL, 0)
end function

sub setwriteable (fname as string)
	dim attr as integer = GetFileAttributes(strptr(fname))
	if attr = INVALID_FILE_ATTRIBUTES then
		dim errstr as string = get_windows_error()
		debug "GetFileAttributes(" & fname & ") failed: " & errstr
		exit sub
	end if
	attr = attr and not FILE_ATTRIBUTE_READONLY
	'attr = attr or FILE_ATTRIBUTE_TEMPORARY  'Try to avoid writing to harddisk
	if SetFileAttributes(strptr(fname), attr) = 0 then
		dim errstr as string = get_windows_error()
		debug "SetFileAttributes(" & fname & ") failed: " & errstr
	end if
end sub


'Advisory locking (actually mandatory on Windows).

private function get_file_handle (byval fh as CFILE_ptr) as integer
	return get_osfhandle(fileno(fh))
end function
	
function lock_file_for_write (byval fh as CFILE_ptr, byval timeout_ms as integer, byval flag as integer, funcname as string) as integer
	dim fhandle as integer = get_file_handle(fh)
	dim timeout as integer = GetTickCount() + timeout_ms
	dim overlapped as LPOVERLAPPED
	overlapped.hEvent = 0
	overlapped.offset = 0  'specify beginning of file
	overlapped.offsetHigh = 0
	do
		if (LockFile(fhandle, OR LOCKFILE_FAIL_IMMEDIATELY, 0, &hffffffff, 0, @overlapped))
			return YES
		end if
		if GetLastError() <> ERROR_IO_PENDING then
			dim errstr as string = get_windows_error()
			debug funcname & ": LockFile() failed: " & errstr
			return NO
		end if
		Sleep(0)
	while GetTickCount() < timeout
	debug funcname & ": timed out"
	return NO
end function

function lock_file_for_write (byval fh as CFILE_ptr, byval timeout_ms as integer) as integer
	return lock_file_base(fh, timeout_ms, LOCKFILE_EXCLUSIVE_LOCK, "lock_file_for_write")
end function

function lock_file_for_read (byval fh as CFILE_ptr, byval timeout_ms as integer) as integer
	return lock_file_base(fh, timeout_ms, 0, "lock_file_for_read")
end function

sub unlock_file (byval fh as CFILE_ptr)
	UnLockFile(get_file_handle(fh), 0, 0, &hffffffff, 0)
end sub


'==========================================================================================
'                               Inter-process communication
'==========================================================================================


'WRITEME: NOT IMPLEMENTED

function channel_open_read (name as string, byval result as IPCChannel ptr) as integer
	return 0
end function

function channel_open_write (name as string, byval result as IPCChannel ptr) as integer
	return 0
end function

sub channel_close (byval channel as IPCChannel ptr)
end function

function channel_write (byval channel as IPCChannel, byval buf as byte ptr, byval buflen as integer) as integer
	return 0
end function

function channel_input_line (byval channel as IPCChannel, output as string) as integer         
	return 0
end function


'==========================================================================================
'                                       Processes
'==========================================================================================


'Returns 0 on failure.
'If successful, you should call cleanup_process with the handle after you don't need it any longer.
'This is currently designed for running console applications. Could be
'generalised in future as needed.
function open_console_process (program as string, args as string) as ProcessHandle
	dim argstemp as string = args
	dim flags as integer = 0
	dim sinfo as STARTUPINFO
	sinfo.cb = sizeof(STARTUPINFO)
	'The following console-specific stuff is what prevents bug 826 from occurring
	sinfo.dwFlags = STARTF_USESHOWWINDOW OR STARTF_USEPOSITION
	sinfo.wShowWindow = 4 'SW_SHOWNOACTIVATE  'Don't activate window, but do show (not defined, probably we excluded too much of windows.bi)
	sinfo.dwX = 5  'Try to move the window out of the way so that it doesn't cover our window
	sinfo.dwY = 5

	dim pinfop as ProcessHandle = Callocate(sizeof(PROCESS_INFORMATION))
	if CreateProcess(strptr(program), strptr(argstemp), NULL, NULL, 0, flags, NULL, NULL, @sinfo, pinfop) = 0 then
		dim errstr as string = get_windows_error()
		debug "CreateProcess(" & program & ", " & args & ") failed: " & errstr
		Deallocate(pinfop)
		return 0
	else
		return pinfop
	end if
end function

'If exitcode is nonnull and the process exited, the exit code will be placed in it
function process_running (byval process as ProcessHandle, byval exitcode as integer ptr = NULL) as integer
	if process = NULL then return NO
	dim waitret as integer = WaitForSingleObject(process->hProcess, 0)
        if waitret = WAIT_FAILED then
		dim errstr as string = get_windows_error()
		debug "process_running failed: " & errstr
		return NO
	end if
	if exitcode <> NULL and waitret = 0 then
		if GetExitCodeProcess(process->hProcess, exitcode) = 0 then
			debuginfo "GetExitCodeProcess failed: " & get_windows_error()
		end if
	end if
	return (waitret = WAIT_TIMEOUT)
end function

sub kill_process (byval process as ProcessHandle)
	if process = NULL then exit sub
	'Isn't there some way to signal the process to quit? This kills it immediately.
	if TerminateProcess(process->hProcess, 1) = 0 then
		debug "TerminateProcess failed: " & get_windows_error()
	end if

	'And now we wait for the process to die: it might have files open that we want to delete.
	'Amazingly, if we don't do this and instead just wait for a couple seconds when we try
	'to delete files the process had open they're still open and we can't. However, waiting
	'for the process to die takes just a millisecond or two! Something ain't right.

	dim waitret as integer = WaitForSingleObject(process->hProcess, 500)  'wait up to 500ms
        if waitret <> 0 then
		dim errstr as string
		if waitret = WAIT_FAILED then errstr = get_windows_error()
		debug "couldn't wait for process to quit: " & waitret & " " & errstr
	end if
end sub

'Cleans up resources associated with a ProcessHandle
sub cleanup_process (byval process as ProcessHandle ptr)
	if process = NULL orelse *process = NULL then exit sub
	CloseHandle((*process)->hProcess)
	CloseHandle((*process)->hThread)
	Deallocate(*process)
	*process = NULL
end sub

end extern