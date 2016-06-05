'OHHRPGCE COMMON - Windows versions of OS-specific routines
'Please read LICENSE.txt for GNU GPL License details and disclaimer of liability

#include "config.bi"
include_windows_bi()
#include once "win/psapi.bi"
'#include "win/shellapi.bi"
'#include "win/objbase.bi"
#define MSG MSG_  'Workaround an #undef in include_windows_bi()
#include "win/shlobj.bi"
#undef this
#include "os.bi"
#include "crt/string.bi"
#include "crt/limits.bi"
#include "crt/stdio.bi"
#include "crt/io.bi"
#include "common.bi"
#include "allmodex.bi"
#include "util.bi"
#include "const.bi"

'''''' Extra winapi defines

' Missing from FB 0.23 headers
#ifndef GetProcessImageFileName
	extern "Windows"
	#ifdef UNICODE
		declare function GetProcessImageFileName alias "GetProcessImageFileNameW" (byval hProcess as HANDLE, byval lpImageFileName as LPWSTR, byval nSize as DWORD) as DWORD
	#else
		declare function GetProcessImageFileName alias "GetProcessImageFileNameA" (byval hProcess as HANDLE, byval lpImageFileName as LPSTR, byval nSize as DWORD) as DWORD
	#endif
	end extern
#endif

'We #undef'd copyfile
#ifdef UNICODE
	declare function CopyFile_ alias "CopyFileW" (byval as LPCWSTR, byval as LPCWSTR, byval as BOOL) as BOOL
#else
	declare function CopyFile_ alias "CopyFileA" (byval as LPCSTR, byval as LPCSTR, byval as BOOL) as BOOL
#endif


'==========================================================================================
'                                Utility/general functions
'==========================================================================================

extern "C"
'FormatMessage is such an awfully complex function
function get_windows_error (byval errcode as integer) as string
	dim strbuf as string * 256
	FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, NULL, errcode, 0, strptr(strbuf), 255, NULL)
	return strbuf
end function
end extern

#define error_string get_windows_error(GetLastError())

private function get_file_handle (byval fh as CFILE_ptr) as HANDLE
	return cast(HANDLE, _get_osfhandle(_fileno(fh)))
end function

private function file_handle_to_readable_FILE (byval fhandle as HANDLE, funcname as string) as FILE ptr
	dim fd as integer = _open_osfhandle(cast(integer, fhandle), 0)
	if fd = -1 then
		debug funcname + ": _open_osfhandle failed"
		CloseHandle(fhandle)
		return NULL
	end if

	dim fh as FILE ptr = _fdopen(fd, "r")
	if fh = NULL then
		debug funcname + ": _fdopen failed"
		_close(fd)
		return NULL
	end if
	return fh
end function

'Returns true only on Windows 95, 98 and ME
function is_windows_9x () as bool
	dim verinfo as OSVERSIONINFO
	verinfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO)
	if GetVersionEx(@verinfo) then
		return verinfo.dwPlatformId <= 1
	else
		return NO  'simply most likely
	end if
end function

'Note: this returns Windows 8 on Windows 8.1 and 10, because GetVersionEx lies to preserve compatibility!
'To fix that need to include a manifest: https://msdn.microsoft.com/en-us/library/windows/desktop/dn481241%28v=vs.85%29.aspx
function get_windows_version () as string
	dim ret as string
	dim verinfo as OSVERSIONINFO
	verinfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO)
	if GetVersionEx(@verinfo) then
		ret = "Windows " & verinfo.dwMajorVersion & "." & verinfo.dwMinorVersion & "." & verinfo.dwBuildNumber
		select case verinfo.dwPlatformId * 10000 + verinfo.dwMajorVersion * 100 + verinfo.dwMinorVersion
			case 10400:  ret += " (95)"
			case 10410:  ret += " (98)"
			case 10490:  ret += " (ME)"
			case 20000 to 20499:  ret += " (NT)"
			case 20500:  ret += " (2000)"
			case 20501:  ret += " (XP)"
			case 20502:  ret += " (XP x64/Server 2003)"
			case 20600:  ret += " (Vista/Server 2008)"
			case 20601:  ret += " (7/Server 2008 R2)"
			case 20602:  ret += " (8/Server 2012 or later)"
			'case 20603:  ret += " (8.1/Server 2012 R2)"
			'case 21000:  ret += " (10/Server 2016)"
		end select
		ret += " " + verinfo.szCSDVersion
	end if
	return ret
end function


extern "C"

'Currently Android only
sub external_log (msg as string)
end sub

function memory_usage() as integer
	return 0
end function

function memory_usage_string() as string
	return ""
end function


'==========================================================================================
'                                       Filesystem
'==========================================================================================

function list_files (searchdir as string, nmask as string, byval showhidden as bool) as string vector
	'This function is only used on unix! see os_unix.c
	dim ret as string vector
	v_new ret
	return v_ret(ret)
end function

function list_subdirs (searchdir as string, nmask as string, byval showhidden as bool) as string vector
	'This function is only used on unix! see os_unix.c
	dim ret as string vector
	v_new ret
	return v_ret(ret)
end function

function os_get_documents_dir() as string
	dim buf as string * MAX_PATH
	' This is a very deprecated function; SHGetFolderPath is slightly more modern but apparently Win 2000+ only.
        ' Doesn't set an error code!
	if SHGetSpecialFolderPath(0, strptr(buf), CSIDL_PERSONAL, 0) then  'Documents
		if diriswriteable(buf) then
			return buf
		else
			debug "Can't write to CSIDL_PERSONAL directory " & buf
		end if
	end if
	if SHGetSpecialFolderPath(0, strptr(buf), CSIDL_DESKTOPDIRECTORY, 0) then  'Desktop
		if diriswriteable(buf) then
			return buf
		else
			debug "Can't write to CSIDL_DESKTOPDIRECTORY " & buf
		end if
	end if

	' On older systems (if SHGetSpecialFolderPath is not available) the following fallback should be used,
	' however the .exe would probably simple fail to load there.
	' (Incorrect in non-english versions of Windows)
	dim ret as string
	ret = environ("USERPROFILE") & SLASH & "Documents"  ' Vista and later
	if not isdir(ret) then
		ret = environ("USERPROFILE") & SLASH & "My Documents"  ' XP and earlier
	end if
	return ret
end function

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

'True on success
function setwriteable (fname as string) as bool
	dim attr as integer = GetFileAttributes(strptr(fname))
	if attr = INVALID_FILE_ATTRIBUTES then
		dim errstr as string = error_string
		debug "GetFileAttributes(" & fname & ") failed: " & errstr
		return NO
	end if
	attr = attr and not FILE_ATTRIBUTE_READONLY
	'attr = attr or FILE_ATTRIBUTE_TEMPORARY  'Try to avoid writing to harddisk
	if SetFileAttributes(strptr(fname), attr) = 0 then
		dim errstr as string = error_string
		debug "SetFileAttributes(" & fname & ") failed: " & errstr
		return NO
	end if
	return YES
end function

'A file copy function which deals safely with the case where the file is open already (why do we need that?)
'Returns true on success.
function copy_file_replacing(byval source as zstring ptr, byval destination as zstring ptr) as bool
	'Replacing an open file does not work on Windows.

	'Overwrites existing files
	if CopyFile_(source, destination, 0) = 0 then
		dim errstr as string = error_string
		debugc errError, "copy_file_replacing(" & *source & "," & *destination & ") failed: " & errstr
		return NO
	end if
	return YES
end function


'==========================================================================================
'                                    Advisory locking
'==========================================================================================
' (Actually mandatory on Windows)

	
private function lock_file_base (byval fh as CFILE_ptr, byval timeout_ms as integer, byval flag as integer, funcname as string) as integer
	dim fhandle as HANDLE = get_file_handle(fh)
	dim timeout as integer = GetTickCount() + timeout_ms
	dim overlappedop as OVERLAPPED
	overlappedop.hEvent = 0
	overlappedop.offset = 0  'specify beginning of file
	overlappedop.offsetHigh = 0
	do
		if (LockFileEx(fhandle, LOCKFILE_FAIL_IMMEDIATELY, 0, &hffffffff, 0, @overlappedop)) then
			return YES
		end if
		if GetLastError() <> ERROR_IO_PENDING then
			dim errstr as string = error_string
			debug funcname & ": LockFile() failed: " & errstr
			return NO
		end if
		Sleep(0)
	loop while GetTickCount() < timeout
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

function test_locked (filename as string, byval writable as integer) as integer
	'Not bothering to implement this; used for debugging only
	return 0
end function


'==========================================================================================
'                               Inter-process communication
'==========================================================================================


type NamedPipeInfo
  fh as HANDLE         'Write end of the pipe. Used for writing
  readfh as HANDLE     'Read end of the pipe. Not used. Probably equal to fh
  cfile as FILE ptr    'stdio FILE wrapper around read end of the pipe. Used for reading only
  available as integer   'Total amount seen on readfh
  readamount as integer  'Total amount read from cfile
  hasconnected as integer
  overlappedop as OVERLAPPED
end type


declare sub channel_delete (byval channel as NamedPipeInfo ptr)

function channel_open_server (byref channel as NamedPipeInfo ptr, chan_name as string) as integer
	if channel <> NULL then debug "channel_open_server: forgot to close" : channel_close(channel)

	dim pipeh as HANDLE
	'asynchronous named pipe with 4096 byte read & write buffers
	pipeh = CreateNamedPipe(strptr(chan_name), PIPE_ACCESS_DUPLEX OR FILE_FLAG_OVERLAPPED, _
	                        PIPE_TYPE_BYTE OR PIPE_READMODE_BYTE, 1, 4096, 4096, 0, NULL)
	if pipeh = -1 then
		dim errstr as string = error_string
		debug "Could not open IPC channel: " + errstr
		return NO
	end if

	dim pipeinfo as NamedPipeInfo ptr
	pipeinfo = New NamedPipeInfo
	pipeinfo->fh = pipeh
	pipeinfo->readfh = pipeh

	'create a "manual-reset event object", required for ConnectNamedPipe
	dim event as HANDLE
	event = CreateEvent(NULL, 1, 0, NULL)
	pipeinfo->overlappedop.hEvent = event

	'Start listening for connection (technically possible for a client to connect as soon as
	'CreateNamedPipe is called)
	ConnectNamedPipe(pipeh, @pipeinfo->overlappedop)
	dim errcode as integer = GetLastError()
	if errcode = ERROR_PIPE_CONNECTED then
		pipeinfo->hasconnected = YES
	elseif errcode <> ERROR_IO_PENDING then
		dim errstr as string = error_string
		debug "ConnectNamedPipe error: " + errstr
		channel_delete(pipeinfo)
		return NO
	end if

	dim cfile as FILE ptr
	cfile = file_handle_to_readable_FILE(pipeh, "channel_open_server")
	if cfile = NULL then
		channel_delete(pipeinfo)
		return NO
	end if
	pipeinfo->cfile = cfile

	channel = pipeinfo
	return YES
end function

'Wait for a client connection; return true on success
function channel_wait_for_client_connection (byref channel as NamedPipeInfo ptr, byval timeout_ms as integer) as integer
	dim startt as double = TIMER

	if channel->hasconnected = NO then
		dim res as integer
		res = WaitForSingleObject(channel->overlappedop.hEvent, timeout_ms)
		if res = WAIT_TIMEOUT then
			debug "timeout while waiting for channel connection"
			return NO
		elseif res = WAIT_OBJECT_0 then
			channel->hasconnected = YES
		else
			dim errstr as string = error_string
			debug "error waiting for channel connection: " + errstr
			return NO
		end if
		debuginfo "Channel connection received (after " & CINT(1000 * (TIMER - startt)) & "ms)"
	end if
	return YES
end function

'Returns true on success
function channel_open_client (byref channel as NamedPipeInfo ptr, chan_name as string) as integer
	if channel <> NULL then debug "channel_open_client: forgot to close" : channel_close(channel)

	dim pipeh as HANDLE
	pipeh = CreateFile(strptr(chan_name), GENERIC_READ OR GENERIC_WRITE, 0, NULL, _
	                   OPEN_EXISTING, 0, NULL)
	if pipeh = -1 then
		dim errstr as string = error_string
		debug "channel_open_client: could not open: " + errstr
		return NO
	end if

	'This is a hack; see channel_read_input_line
	dim cfile as FILE ptr
	cfile = file_handle_to_readable_FILE(pipeh, "channel_open_client")
	if cfile = NULL then
		CloseHandle(pipeh)
		return NO
	end if

	dim pipeinfo as NamedPipeInfo ptr
	pipeinfo = New NamedPipeInfo
	pipeinfo->fh = pipeh
	pipeinfo->readfh = pipeh
	pipeinfo->cfile = cfile
	channel = pipeinfo
	return YES
end function

private sub channel_delete (byval channel as NamedPipeInfo ptr)
	with *channel
		if .cfile then
			fclose(.cfile)  'Closes .readfh too
			if .readfh = .fh then .fh = NULL
			.readfh = NULL
		end if
		if .fh then CloseHandle(.fh)
		if .overlappedop.hEvent then CloseHandle(.overlappedop.hEvent)
	end with
	Delete channel
end sub

sub channel_close (byref channel as NamedPipeInfo ptr)
	if channel = NULL then exit sub
	channel_delete(channel)
	channel = NULL
end sub

'Returns true on success
function channel_write (byref channel as NamedPipeInfo ptr, byval buf as any ptr, byval buflen as integer) as integer
	if channel = NULL then return NO

	dim as integer res, written
	'Technically am meant to pass an OVERLAPPED pointer to WriteFile, but this seems to work
	res = WriteFile(channel->fh, buf, buflen, @written, NULL)
	if res = 0 or written < buflen then
		'should actually check errno instead; hope this works
		dim errstr as string = error_string
		debuginfo "channel_write error (closing) (wrote " & written & " of " & buflen & "): " & errstr
		channel_close(channel)
		return NO
	end if
	'debuginfo "channel_write: " & written & " of " & buflen & " " & error_string
	return YES
end function

'Returns true on success
'Automatically appends a newline.
function channel_write_line (byref channel as NamedPipeInfo ptr, buf as string) as integer
	'Temporarily replace NULL byte with a newline
	buf[LEN(buf)] = 10
	dim ret as integer = channel_write(channel, @buf[0], LEN(buf) + 1)
	buf[LEN(buf)] = 0
	return ret
end function

'Read until the next newline (result in line_in) and return true, or return false if nothing to read
function channel_input_line (byref channel as NamedPipeInfo ptr, line_in as string) as integer
	line_in = ""
	if channel = NULL then return NO

	'This is a hack because I'm too lazy to do my own buffering:
	'I wrapped the pipe in a C stdio FILE, but use PeekNamedPipe to figure out how much data is left
	'in total in the pipe's buffer and the FILE buffer; do not call fgets unless it's positive,
	'otherwise it will block.
	if channel->readamount >= channel->available then
		'recheck whether more data is available
		dim bytesbuffered as integer
		if PeekNamedPipe(channel->readfh, NULL, 0, NULL, @bytesbuffered, NULL) = 0 then
			dim errstr as string = error_string
			debuginfo "PeekNamedPipe error (closing) : " + errstr
			channel_close(channel)
			return 0
		end if
		channel->available += bytesbuffered
		'debuginfo "read new data " & bytesbuffered
		if channel->readamount >= channel->available then
			return 0
		end if
	end if

	dim buf(511) as ubyte
	dim res as ubyte ptr
	do
		res = fgets(@buf(0), 512, channel->cfile)
		if res = NULL then
			dim errstr as string = error_string
			debuginfo "pipe read error (closing): " + errstr  'should actually check errno instead; hope this works
			channel_close(channel)
			return 0
		end if
		channel->readamount += strlen(@buf(0))
		res = strrchr(@buf(0), 10)
		if res <> NULL then *res = 0  'strip newline
		'debuginfo "read '" & *cast(zstring ptr, @buf(0)) & "'"
		line_in += *cast(zstring ptr, @buf(0))
		if buf(0) = 0 or res <> NULL then
			return 1
		end if
	loop
end function


'==========================================================================================
'                                       Processes
'==========================================================================================


'Returns 0 on failure.
'If successful, you should call cleanup_process with the handle after you don't need it any longer.
'program is an unescaped path. Any paths in the arguments should be escaped
'This is for gui processes
function open_process (program as string, args as string) as ProcessHandle
	dim argstemp as string = escape_filename(program) + " " + args
	dim flags as integer = 0
	dim sinfo as STARTUPINFO
	sinfo.cb = sizeof(STARTUPINFO)
	dim pinfop as ProcessHandle = Callocate(sizeof(PROCESS_INFORMATION))
	if CreateProcess(strptr(program), strptr(argstemp), NULL, NULL, 0, flags, NULL, NULL, @sinfo, pinfop) = 0 then
		dim errstr as string = error_string
		debug "CreateProcess(" & program & ", " & args & ") failed: " & errstr
		Deallocate(pinfop)
		return 0
	else
		return pinfop
	end if
end function

'Run a (hidden) commandline program and open a pipe which writes to its stdin & reads from stdout
'Returns 0 on failure.
'If successful, you should call cleanup_process with the handle after you don't need it any longer.
function open_piped_process (program as string, args as string, byval iopipe as NamedPipeInfo ptr ptr) as ProcessHandle
	dim argstemp as string = escape_filename(program) + " " + args
	dim flags as integer = 0
	dim pinfop as ProcessHandle  'PROCESS_INFORMATION ptr
	dim sinfo as STARTUPINFO
	dim pipeinfo as NamedPipeInfo ptr

	if *iopipe then
		debug "Error: open_piped_process found open IPCChannel argument"
		channel_close *iopipe
	end if

	dim pipename as string
	pipename = "\\.\pipe\AnonPipe." & (100000 * rando())

	dim as NamedPipeInfo ptr serverpipe, clientpipe

	if channel_open_server(serverpipe, pipename) = NO then
		debug "open_piped_process failed."
		goto error_out
	end if

	if channel_open_client(clientpipe, pipename) = NO then
		debug "open_piped_process failed."
		goto error_out
	end if

	'Make this pipe handle inheritable
	if SetHandleInformation(clientpipe->fh, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT) = 0 then
		dim errstr as string = error_string
		debug "SetHandleInformation failure: " & errstr
		goto error_out
	end if

	sinfo.cb = sizeof(STARTUPINFO)
	'sinfo.hStdError = clientpipe->fh
	sinfo.hStdOutput = clientpipe->fh
	sinfo.hStdInput = clientpipe->fh
	sinfo.dwFlags or= STARTF_USESTDHANDLES 'OR STARTF_USESHOWWINDOW
	'(Apparently this flag doesn't work unless you also set standard input and output handle)
	flags or= CREATE_NO_WINDOW

	pinfop = Callocate(sizeof(PROCESS_INFORMATION))
	if CreateProcess(strptr(program), strptr(argstemp), NULL, NULL, 1, flags, NULL, NULL, @sinfo, pinfop) = 0 then
		dim errstr as string = error_string
		debug "CreateProcess(" & program & ", " & args & ") failed: " & errstr
		goto error_out
	end if

	'Get rid of unneeded handle
	channel_close(clientpipe)
	clientpipe = NULL

	*iopipe = serverpipe

	return pinfop

 error_out:
	if clientpipe then channel_close(clientpipe)
	if serverpipe then channel_close(serverpipe)
	if pinfop then Deallocate(pinfop)
	return 0

end function

'Returns 0 on failure.
'If successful, you should call cleanup_process with the handle after you don't need it any longer.
'This is currently designed for running console applications. Could be
'generalised in future as needed.
function open_console_process (program as string, args as string) as ProcessHandle
	dim argstemp as string = escape_filename(program) + " " + args
	dim flags as integer = 0
	dim sinfo as STARTUPINFO
	sinfo.cb = sizeof(STARTUPINFO)
	'The following console-specific stuff is what prevents bug 826 from occurring
	sinfo.dwFlags = STARTF_USESHOWWINDOW OR STARTF_USEPOSITION
	'sinfo.wShowWindow = 4 'SW_SHOWNOACTIVATE  'Don't activate window, but do show (not defined, probably we excluded too much of windows.bi)
	sinfo.wShowWindow = 1 'SW_SHOWNORMAL  'Show and activate windows
	sinfo.dwX = 5  'Try to move the window out of the way so that it doesn't cover our window
	sinfo.dwY = 5

	dim pinfop as ProcessHandle = Callocate(sizeof(PROCESS_INFORMATION))
	if CreateProcess(strptr(program), strptr(argstemp), NULL, NULL, 0, flags, NULL, NULL, @sinfo, pinfop) = 0 then
		dim errstr as string = error_string
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
		dim errstr as string = error_string
		debug "process_running failed: " & errstr
		return NO
	end if
	if exitcode <> NULL and waitret = 0 then
		if GetExitCodeProcess(process->hProcess, exitcode) = 0 then
			dim errstr as string = error_string
			debuginfo "GetExitCodeProcess failed: " & errstr
		end if
	end if
	return (waitret = WAIT_TIMEOUT)
end function

sub kill_process (byval process as ProcessHandle)
	if process = NULL then exit sub
	'Isn't there some way to signal the process to quit? This kills it immediately.
	if TerminateProcess(process->hProcess, 1) = 0 then
		dim errstr as string = error_string
		debug "TerminateProcess failed: " & errstr
	end if

	'And now we wait for the process to die: it might have files open that we want to delete.
	'Amazingly, if we don't do this and instead just wait for a couple seconds when we try
	'to delete files the process had open they're still open and we can't. However, waiting
	'for the process to die takes just a millisecond or two! Something ain't right.

	dim waitret as integer = WaitForSingleObject(process->hProcess, 500)  'wait up to 500ms
	if waitret <> 0 then
		dim errstr as string
		if waitret = WAIT_FAILED then errstr = error_string
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

function get_process_id () as integer
	return GetCurrentProcessId()
end function

'Returns full path to a process given its PID in device form, e.g.
'\Device\HarddiskVolume1\OHRRPGCE\custom.exe
'or "" if it doesn't exist or we don't have permission.
function get_process_path (pid as integer) as string
	dim proc as HANDLE
	proc = OpenProcess(PROCESS_QUERY_INFORMATION, FALSE, pid)
	if proc = NULL then
		dim errcode as integer = GetLastError()
		' OpenProcess sets "Invalid parameter" error if the pid doesn't exist
		if errcode <> ERROR_INVALID_PARAMETER then
			debug "get_process_path: OpenProcess(pid=" & pid & ") err " & errcode & " " & get_windows_error(errcode)
		end if
		return ""
	end if
	dim ret as zstring * 256
	'QueryFullProcessImageName, which returns a normal filename instead of device form, is Win Vista+.
	if GetProcessImageFileName(proc, ret, 256) = 0 then
		dim errcode as integer = GetLastError()
		debug "get_process_path: GetProcessImageFileName err " & errcode & " " & get_windows_error(errcode)
	end if
	CloseHandle(proc)
	return ret
end function


/'
'Opens a file (or URL) with default handler.
'If successful returns "", otherwise returns an error message.
function open_document (filename as string) as string
	'Initialise COM; may be necessary. May be called multiple times
	'as long as the args are the same.
	CoInitializeEx(NULL, COINIT_APARTMENTTHREADED | COINIT_DISABLE_OLE1DDE)
	dim info as SHELLEXECUTEINFO
	info.cbSize = SIZEOF(SHELLEXECUTEINFO)
	'Probably unneeded. Waits for the 'execute operation' to complete (does that
	'mean better error catching?). Needed when called from background thread.
	info.fmask = SEE_MASK_NOASYNC
	info.lpVerb = @"open"
	info.lpFile = filename
	info.nShow = SW_SHOWNORMAL
	if ShellExecuteEx(@info) = 0 then
		return error_string
	end if
	return ""
end function
'/

end extern
