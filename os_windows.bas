'OHHRPGCE COMMON - Windows versions of OS-specific routines
'Please read LICENSE.txt for GNU GPL License details and disclaimer of liability

#include "config.bi"
include_windows_bi()
' This define is needed in C code to compile binaries on Win7+ that use certain
' psapi.dll functions so they will work on older Windows.
' It's not currently used by FB's headers, but might in future.
#define PSAPI_VERSION 1
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
#include "common_base.bi"
#include "util.bi"
#include "const.bi"
#include "ver.txt"

'Try to load CrashRpt*.dll on startup.
'You should build with pdb=1, and keep the generated .pdb files.
'The scons debug=... setting doesn't matter; debug=0 to strip all symbols is fine,
'but debug=2/3 will add -exx for more error checking.
#define WITH_CRASHRPT

'You should compile with gengcc=1, or debug info will be garbage. Source lines will be
'included with scons debug>=1
#define WITH_EXCHNDL

'Only has an effect if WITH_EXCHNDL defined: Try to load exchndl.dll at startup,
'instead of being statically linked.
#define DYNAMIC_EXCHNDL

#if defined(WITH_EXCHNDL)
	#if defined(DYNAMIC_EXCHNDL)
		dim shared ExcHndlSetLogFileNameA as function(as zstring ptr) as boolean
	#else
		#include "win32/exchndl.bi"  'Our win32 directory
	#endif
#endif

#if defined(WITH_CRASHRPT)
	'This API version is not defined by CrashRpt, it's something we define to allow different
	'copies of the engine which use incompatible versions of CrashRpt to coexist.
	CONST CURRENT_CRASHRPT_API = 1

	extern "C"
		'This function is in os_windows2.c
		declare function crashrpt_setup(libpath as zstring ptr, appname as zstring ptr, version as zstring ptr, buildstring as zstring ptr, logfile1 as zstring ptr, logfile2 as zstring ptr, add_screenshot as boolint) as boolint
	end extern
#endif


dim shared crash_reportfile as string
dim shared continue_after_exception as bool = NO
dim shared want_exception_messagebox as bool = YES
dim shared main_thread_id as integer

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
	static cached as integer = -2   'I did not bother to test whether a cache is needed
	if cached <> -2 then return cached
	dim verinfo as OSVERSIONINFO
	verinfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO)
	if GetVersionEx(@verinfo) then
		cached = verinfo.dwPlatformId <= 1
	else
		cached = NO  'simply most likely
	end if
	return cached
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

sub os_init ()
	main_thread_id = GetCurrentThreadId()
end sub

'Currently Android only
sub external_log (msg as zstring ptr)
end sub

#macro GET_MEMORY_INFO(memctrs, on_error)
	' This requires psapi.dll
	if GetProcessMemoryInfo(GetCurrentProcess(), @memctrs, sizeof(memctrs)) = 0 then
		debug "GetProcessMemoryInfo failed: " & error_string
		return on_error
	end if
#endmacro

' Return an approximation of the total amount of memory allocated by this process, in bytes:
' the amount of space reserved in the pagefile, plus unpageable memory. Does not include
' memory mapped files (like the .exe itself), but those are probably constant.
' Often also more than the actual amount of memory used, especially when using gfx_directx
function memory_usage() as integer
	dim memctrs as PROCESS_MEMORY_COUNTERS
	GET_MEMORY_INFO(memctrs, 0)
	return memctrs.PagefileUsage
end function

function memory_usage_string() as string
	dim memctrs as PROCESS_MEMORY_COUNTERS
	if GetProcessMemoryInfo(GetCurrentProcess(), @memctrs, sizeof(memctrs)) = 0 then
		debug "GetProcessMemoryInfo failed: " & error_string
		return ""
	end if
	GET_MEMORY_INFO(memctrs, "")
	return "workingset=" & memctrs.WorkingSetSize & " peak workingset=" & memctrs.PeakWorkingSetSize _
	       & " commit=" & memctrs.PagefileUsage & " peak commit=" & memctrs.PeakPagefileUsage _
	       & " nonpaged=" & memctrs.QuotaNonPagedPoolUsage
end function


'==========================================================================================
'                                   Exception Handling
'==========================================================================================

extern "windows"
'A default last-resort exception handler, used only if we haven't loaded an
'exception handling library.
function exceptFilterMessageBox(pExceptionInfo as PEXCEPTION_POINTERS) as clong
	if want_exception_messagebox then
		'Avoid calling FB string routines
		dim msgbuf as string * 301
		if len(crash_reportfile) then
			'This only happens if we loaded exchndl.dll
			snprintf(strptr(msgbuf), 300, _
				 !"The engine has crashed! Sorry :(\n\n" _
				 !"A crash report has been written to\n%s\n\n" _
				 !"Please email it and g_debug.txt or c_debug.txt\n" _
				 !"to ohrrpgce-crash@HamsterRepublic.com\n" _
				 "with a description of what you were doing.", _
				 strptr(crash_reportfile))
		else
			snprintf(strptr(msgbuf), 300, _
				 !"The engine has crashed! Sorry :(\n\n" _
				 !"Can't generate a stacktrace, as exchndl.dll isn't present.\n\n" _
				 !"Please email g_debug.txt or c_debug.txt to\n" _
				 !"ohrrpgce-crash@HamsterRepublic.com\n" _
				 "with a description of what you were doing.")
		end if
		MessageBoxA(NULL, strptr(msgbuf), "OHRRPGCE Error", MB_OK or MB_ICONERROR)
	end if

	if continue_after_exception then
		return &hffffffff  '== EXCEPTION_CONTINUE_EXECUTION, which is not declared in FB's headers
	else
		'Stop, don't show the default "program has stopped responding" message.
		return 1  '== EXCEPTION_EXECUTE_HANDLER
	end if
end function
end extern

#ifdef WITH_CRASHRPT

'Load the crashrpt dll, return whether successful
function try_load_crashrpt_at(crashrpt_dll as string) as bool
	if real_isfile(crashrpt_dll) = NO then return NO

	'The utilities (programs other than Game and Custom) set app_name to NULL rather than override it
	dim appname as zstring ptr = iif(app_name <> NULL, app_name, strptr(exename))
	dim add_screenshot as bool = NO '(app_name <> NULL)
	early_debuginfo "Loading " & crashrpt_dll
	if crashrpt_setup(strptr(crashrpt_dll), appname, short_version, long_version, _
			  app_log_filename, app_archive_filename, add_screenshot) then
		'early_debuginfo "CrashRpt handler installed"
		return YES
	end if
	'On failure, crashrpt_setup logs an error itself
	return NO
end function

function find_and_load_crashrpt() as bool
	dim dll_loc_file as string = ENVIRON("APPDATA") & "\OHRRPGCE\crashrpt_loc_api" & CURRENT_CRASHRPT_API & ".txt"

	'First check the support directory.
	'Can't call find_helper_app since common.rbas isn't linked by all utilities,
	'and want to minimise the amount of code run before installing the handler anyway.
	dim crashrpt_dll as string = EXEPATH & "\support\CrashRpt1403.dll"
	if try_load_crashrpt_at(crashrpt_dll) then
		'debuginfo "Caching at " &  dll_loc_file
		'Success: write the location of the dll to a file so that game.exe can
		'find the copy distributed with Custom even when running from somewhere else.
		'Always update the file so that it's not stale.
		'We might be pointing it to an older .dll version than it had, but it's too much
		'trouble to check for that, and doesn't matter.
		dim fh as integer = FREEFILE
		if open(dll_loc_file for output as fh) = 0 then
			print #fh, crashrpt_dll
			close #fh
		end if

		return YES
	end if

	'Then try to read the dll location from the dll_loc_file.
	dim fh as integer = FREEFILE
	if open(dll_loc_file for input as fh) = 0 then
		line input #fh, crashrpt_dll
		close #fh

		if try_load_crashrpt_at(crashrpt_dll) then
			return YES
		end if
	end if

	early_debuginfo "Couldn't find crashrpt.dll"
	return NO
end function

#endif

'Installs one or two of three different possible handlers for unhandled exceptions.
sub setup_exception_handler()
	'Install a default exception handler to show a useful message on a crash.
	'If we're using crashrpt:
	'CrashRpt will handle the crash - if we successfully found and loaded it -
	'and this exception handler won't run.
	'If we're using exchndl:
	'exchndl will call any preexisting exception handler after its own runs.
	'(Note: this won't work if libexchndl.a is statically linked)
        SetUnhandledExceptionFilter(@exceptFilterMessageBox)

#if defined(WITH_CRASHRPT)
	'Load CrashRpt, which connects to CrashSender.exe, and out-of-process exception
	'handler (meaning, it spawns a separate process to report the crash, so it can work
	'even if there's severe memory/state corruption).
	if find_and_load_crashrpt() then exit sub
#endif

#if defined(WITH_EXCHNDL)
	' Load the DrMingw embeddable exception handler, if available

#if defined(DYNAMIC_EXCHNDL)
	' To dynamically link to exchndl.dll

	dim dll as string
	dll = exepath & "\exchndl.dll"
	if real_isfile(dll) = NO then
		early_debuginfo "exchndl.dll not found"
		exit sub
	end if
	early_debuginfo "Loading " & dll
	dim handle as any ptr
	handle = dylibload(dll)
	if handle = NULL then
		debug "exchndl.dll load failed! lasterr: " & error_string
		exit sub
	end if
	ExcHndlSetLogFileNameA = dylibsymbol(handle, "ExcHndlSetLogFileNameA")
	if ExcHndlSetLogFileNameA = NULL then
		debug "ExcHndlSetLogFileNameA missing"
		exit sub
	end if
#else
	' If statically linked to libexchndl.a/exchndl.dll
	ExcHndlInit()
#endif
	crash_reportfile = trimextension(exename) + "-crash-report.txt"
	early_debuginfo "exchndl will log to " & crash_reportfile
	ExcHndlSetLogFileNameA(strptr(crash_reportfile))

#endif
end sub

'Called from within fatalerror. Will popup an "Engine crashed" messagebox if show_message true.
sub save_backtrace(show_message as bool = YES)
	#if defined(WITH_EXCHNDL)
		if len(crash_reportfile) = 0 then exit sub  'Don't have exchndl
	#endif
	debug "Saving backtrace"
	want_exception_messagebox = show_message
	continue_after_exception = YES
	'To continue past DebugBreak (interrupt_self) you need to advance the instruction
	'pointer in the exception handler. RaiseException doesn't have that complication.
	'interrupt_self
	RaiseException(EXCEPTION_BREAKPOINT, 0, 0, NULL)
	want_exception_messagebox = YES
	continue_after_exception = NO
	debug "Done!"
end sub

' A breakpoint
sub interrupt_self ()
	DebugBreak
end sub


'==========================================================================================
'                                       Filesystem
'==========================================================================================

function get_file_type (fname as string) as FileTypeEnum
	if len(fname) = 0 then return fileTypeDirectory
	dim res as DWORD = GetFileAttributes(strptr(fname))
	if res = INVALID_FILE_ATTRIBUTES then
		dim errc as integer = GetLastError()
		' Path not found if the parent directory isn't valid either
		if errc = ERROR_FILE_NOT_FOUND or errc = ERROR_PATH_NOT_FOUND then
			return fileTypeNonexistent
		else
			' Returns an error for folders which are network shares (but not subdirs thereof)
			dim errstr as string = error_string
			debug "get_file_type(" & fname & "): " & errc & " " & errstr
			return fileTypeError
		end if
	elseif res and FILE_ATTRIBUTE_DIRECTORY then
		return fileTypeDirectory
	elseif res and FILE_ATTRIBUTE_DEVICE then
		' Note: This doesn't actually happen when opening a device like \\.\C:. An error occurs instead.
		return fileTypeOther
	else
		return fileTypeFile
	end if
end function

/'  Start of an alternative impl that can detect devices.
function get_file_type2 (fname as string) as bool
	dim hdl as HANDLE
	hdl = CreateFile(strptr(fname), 0, FILE_SHARE_READ + FILE_SHARE_WRITE + FILE_SHARE_DELETE, NULL, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL)
	if hdl = INVALID_HANDLE_VALUE then
		? error_string
		return NO
	end if
	CloseHandle hdl
	return YES
end function
'/

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
function setwriteable (fname as string, towhat as bool) as bool
	dim attr as integer = GetFileAttributes(strptr(fname))
	if attr = INVALID_FILE_ATTRIBUTES then
		dim errstr as string = error_string
		debug "GetFileAttributes(" & fname & ") failed: " & errstr
		return NO
	end if
	if towhat then
		attr and= not FILE_ATTRIBUTE_READONLY
	else
		attr or= FILE_ATTRIBUTE_READONLY
	end if
	'attr = attr or FILE_ATTRIBUTE_TEMPORARY  'Try to avoid writing to harddisk
	if SetFileAttributes(strptr(fname), attr) = 0 then
		dim errstr as string = error_string
		debug "SetFileAttributes(" & fname & ", " & towhat & ") failed: " & errstr
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
		debugerror "copy_file_replacing(" & *source & "," & *destination & ") failed: " & errstr
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


'Try to launch a program asynchronously, searching for it in the standard search paths
'(including windows/system32, current directory, and PATH).
'Returns 0 on failure.
'If successful, you should call cleanup_process with the handle after you don't need it any longer.
'program is an unescaped path. Any paths in the arguments should be escaped.
'Allows only killing or waiting for the program, not communicating with it unless that is
'done some other way (e.g. using channels)
'graphical: if true, launch a graphical process (displays a console for commandline programs on
'           Windows, does nothing on Unix).
'waitable is true if you want process_cleanup to wait for the command to finish (ignored on
'           Windows: always waitable)
function open_process (program as string, args as string, waitable as boolint, graphical as boolint) as ProcessHandle
	dim argstemp as string = escape_filename(program) + " " + args
	dim flags as integer = 0
	dim sinfo as STARTUPINFO
	sinfo.cb = sizeof(STARTUPINFO)
	if graphical = NO then
		'I wrote originally that
		'"Apparently CREATE_NO_WINDOW doesn't work unless you also set standard input and output handle"
		'however now I can't reproduce that problem (on Windows XP).
		'However, passing STARTF_USESTDHANDLES causes pipe redirection (>, 2>)
		'to not work when passed to cmd.exe as a commandline, breaking run_and_get_output.
		'I can't understand why cmd.exe should care!
		'sinfo.dwFlags or= STARTF_USESTDHANDLES 'OR STARTF_USESHOWWINDOW
		'FIXME: On Windows 9X (when using command.com), this doesn't work, and a window always pops up.
		flags or= CREATE_NO_WINDOW
	end if

	dim pinfop as ProcessHandle = Callocate(sizeof(PROCESS_INFORMATION))
	'Passing NULL as lpApplicationName causes the first quote-delimited
	'token in argstemp to be used, and to search for the program in standard
	'paths. If lpApplicationName is provided then searching doesn't happen.
	if CreateProcess(NULL, strptr(argstemp), NULL, NULL, 0, flags, NULL, NULL, @sinfo, pinfop) = 0 then
		dim errstr as string = error_string
		debug "CreateProcess(" & program & ", " & args & ") failed: " & errstr
		Deallocate(pinfop)
		return 0
	else
		return pinfop
	end if
end function

'Untested?
'Run a (hidden) commandline program asynchronously and open a pipe which writes
'to its stdin & reads from stdout, searching for it in the standard search paths.
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
	'FIXME: not sure whether to use this flag (see open_process), since this function is untested
	sinfo.dwFlags or= STARTF_USESTDHANDLES 'OR STARTF_USESHOWWINDOW
	'(Apparently this flag doesn't work unless you also set standard input and output handle)
	flags or= CREATE_NO_WINDOW

	pinfop = Callocate(sizeof(PROCESS_INFORMATION))
	'Passing NULL as lpApplicationName causes the first quote-delimited
	'token in argstemp to be used, and to search for the program in standard
	'paths. If lpApplicationName is provided then searching doesn't happen.
	if CreateProcess(NULL, strptr(argstemp), NULL, NULL, 1, flags, NULL, NULL, @sinfo, pinfop) = 0 then
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
'This is currently designed for asynchronously running console applications.
'On Windows it displays a visible console window, on Unix it doesn't.
'Could be generalised in future as needed.
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
private function _process_running (process as ProcessHandle, exitcode as integer ptr = NULL, timeoutms as integer) as boolint
	if process = NULL then return NO
	dim waitret as integer = WaitForSingleObject(process->hProcess, timeoutms)
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

'If exitcode is nonnull and the process exited, the exit code will be placed in it
function process_running (process as ProcessHandle, exitcode as integer ptr = NULL) as boolint
	return _process_running(process, exitcode, 0)
end function

'Wait for and cleanup the process, returns exitcode, or -2 if the process had to be killed
function wait_for_process (process as ProcessHandle ptr, timeoutms as integer = 4000) as integer
	dim exitcode as integer
	if _process_running(*process, @exitcode, timeoutms) then
		kill_process *process
		exitcode = -2
	end if
	cleanup_process process
	return exitcode
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
'This function is used primarily to determine whether a process is still running
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

	if len(ret) then
		'If a process crashes or is killed (but not if it closes normally), Windows apparently keeps some
		'information about the dead PID and continues to return its image path.
		'So check the exitcode to prevent misidentifying a PID as still running.
		dim exitcode as DWORD
		if GetExitCodeProcess(proc, @exitcode) = 0 then
			dim errcode as integer = GetLastError()
			debug "get_process_path: GetExitCodeProcess err " & errcode & " " & get_windows_error(errcode)
		end if
		if exitcode <> STILL_ACTIVE then
			debuginfo "pid " & pid & " image " & ret & " may have crashed, exitcode " & exitcode
			ret = ""
		end if
	end if

	CloseHandle(proc)
	return ret
end function

'Opens a file (or URL, starting with a protocol like http://) with default handler.
'If successful returns "", otherwise returns an error message (in practice only returns error messages for
'files, you never get an error message for an invalid or malformed URL)
function open_document (filename as string) as string
	'Initialise COM; may be necessary. May be called multiple times
	'as long as the args are the same.
	CoInitializeEx(NULL, COINIT_APARTMENTTHREADED OR COINIT_DISABLE_OLE1DDE)
	dim info as SHELLEXECUTEINFO
	info.cbSize = SIZEOF(SHELLEXECUTEINFO)
	'SEE_MASK_NOASYNC probably unneeded. Waits for the 'execute operation' to complete (does that
	'mean better error catching?). Needed when called from background thread.
	info.fmask = SEE_MASK_NOASYNC
	' Verb: Use the default action for this URL/filename. Can instead explicitly pass @"open", but that
	' apparently may not work if the web browser failed to register as supporting "open"
	info.lpVerb = @""
	info.lpFile = STRPTR(filename)
	info.nShow = SW_SHOWNORMAL
	if ShellExecuteEx(@info) = 0 then
		return error_string
	end if
	return ""
end function


'==========================================================================================
'                                       Threading
'==========================================================================================

function on_main_thread () as bool
	return GetCurrentThreadId() = main_thread_id
end function

function tls_alloc_key() as TLSKey
	dim key as DWORD = TlsAlloc()
	if key = 0 then
		debugerror "TlsAlloc failed: " & get_windows_error(GetLastError())
	end if
	return cast(TLSKey, key)
end function

sub tls_free_key(key as TLSKey)
	TlsFree(cast(DWORD, key))
end sub

function tls_get(key as TLSKey) as any ptr
	return TlsGetValue(cast(DWORD, key))
end function

sub tls_set(key as TLSKey, value as any ptr)
	TlsSetValue(cast(DWORD, key), value)
end sub


end extern
