'OHRRPGCE - Windows versions of OS-specific routines
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#include "config.bi"
#include "os.bi"

'#include "crt/string.bi"
'#include "crt/limits.bi"
'#include "crt/stdio.bi"
'#include "common_base.bi"
'#include "util.bi"
'#include "const.bi"


'==========================================================================================
'                                Utility/general functions
'==========================================================================================

extern "C"

'Returns true only on Windows 95, 98 and ME
function is_windows_9x () as bool
	return NO
end function

'If running under wine return its version string (e.g. "8.4"), otherwise NULL
function get_wine_version () as zstring ptr
	return NULL
end function

function get_windows_version () as string
	return ""
end function

function get_windows_runtime_info () as string
	return "unknown"
end function

/'
sub os_init ()
end sub

sub external_log (msg as const zstring ptr)
end sub

sub os_open_logfile (path as const zstring ptr)
end sub

sub os_close_logfile ()
end sub

'/

'This is only used on Win32, otherwise it would be better not to have it here in os_minimal.bas
sub error_message_box(msg as const zstring ptr)
end sub

' Return an approximation of the total amount of memory allocated by this process, in bytes:
' the amount of space reserved in the pagefile, plus unpageable memory. Does not include
' memory mapped files (like the .exe itself), but those are probably constant.
' Often also more than the actual amount of memory used, especially when using gfx_directx
function memory_usage() as integer
	return 0
end function

function memory_usage_string() as string
	return ""
end function

' Like FB's dylibload except it doesn't load the library if it isn't already.
' The ".dll" suffix on the name is optional and it can include a path.
' Use with FB's dylibsymbol and dylibfree.
function dylib_noload(libname as const zstring ptr) as any ptr
	return dylibload(*libname)
end function

'==========================================================================================
'                                   Exception Handling
'==========================================================================================

/'
'' Implemented in the os_* module.

function setup_exception_handler() as boolint
	return YES
end function

function save_backtrace(show_message as bool = YES) as boolint
	return NO
end function

function send_bug_report (msg as const zstring ptr) as boolint
	return NO
end function
'/

' A breakpoint
sub interrupt_self ()
end sub


'==========================================================================================
'                                       Filesystem
'==========================================================================================

'' Other file functions are implemented in the os_* module.

function drivelist (drives() as string) as integer
	return 0
end function

function drivelabel (drive as string) as string
	return ""
end function

function isremovable (drive as string) as integer
	return 0
end function

function hasmedia (drive as string) as integer
	return 0
end function

'True on success
function setwriteable (fname as string, towhat as bool) as bool
	return YES
end function


'==========================================================================================
'                                    Advisory locking
'==========================================================================================

function lock_file_for_write (fh as CFILE_ptr, filename as zstring ptr, timeout_ms as integer) as integer
	return NO
end function

function lock_file_for_read (fh as CFILE_ptr, filename as zstring ptr, timeout_ms as integer) as integer
	return NO
end function

sub unlock_file (byval fh as CFILE_ptr)
end sub

function test_locked (filename as string, byval writable as integer) as integer
	return 0
end function


'==========================================================================================
'                               Inter-process communication
'==========================================================================================

/'
'' These functions are not called at all in MINIMAL_OS builds.

declare sub channel_delete (byval channel as NamedPipeInfo ptr)

function channel_open_server (byref channel as NamedPipeInfo ptr, chan_name as string) as integer
	return NO
end function

function channel_wait_for_client_connection (byref channel as NamedPipeInfo ptr, byval timeout_ms as integer) as integer
	return NO
end function

function channel_open_client (byref channel as NamedPipeInfo ptr, chan_name as string) as integer
	return NO
end function

sub channel_close (byref channel as NamedPipeInfo ptr)
end sub

function channel_write (byref channel as NamedPipeInfo ptr, byval buf as any ptr, byval buflen as integer) as integer
	return NO
end function

function channel_write_line (byref channel as NamedPipeInfo ptr, buf as string) as integer
	return NO
end function

function channel_input_line (byref channel as NamedPipeInfo ptr, line_in as string) as integer
	return NO
end function

function file_ready_to_read(fileno as integer) as boolean
	return false
end function

'/

'==========================================================================================
'                                       Processes
'==========================================================================================

/'
'' These functions are not called at all in MINIMAL_OS builds.

function open_process (program as string, args as string, waitable as boolint, show_output as boolint) as ProcessHandle
	debug "open_process unsupported"
	return 0
end function

function open_piped_process (program as string, args as string, byval iopipe as NamedPipeInfo ptr ptr) as ProcessHandle
	debug "open_piped_process unsupported"
	return 0
end function

function open_console_process (program as string, args as string) as ProcessHandle
	debug "open_console_process unsupported"
	return 0
end function

function process_running (process as ProcessHandle, exitcode as integer ptr = NULL) as boolint
	return NO
end function

function wait_for_process (process as ProcessHandle ptr, timeoutms as integer = 4000) as integer
	return -1
end function

sub kill_process (byval process as ProcessHandle)
end sub

sub cleanup_process (byval process as ProcessHandle ptr)
end sub

'/

function get_process_id () as integer
	return 1
end function

'Returns full path to a process given its PID
'or "" if it doesn't exist, or "<unknown>" if it can't be determined.
'This function is used only to determine whether a process is still running; its meaning is OS-specific.
function get_process_name (pid as integer) as string
	return "<unknown>"
end function

'Opens a file (or URL, starting with a protocol like http://) with default handler.
'If successful returns "", otherwise returns an error message
function os_open_document (filename as string) as string
	return "Not supported by this port"
end function


'==========================================================================================
'                                       Threading
'==========================================================================================

'' MINIMAL_OS builds are always NO_TLS

' function on_main_thread () as bool
' 	return false
' end function


'==========================================================================================
'                                           WM
'==========================================================================================

sub os_get_screen_size(wide as integer ptr, high as integer ptr)
	*wide = 0
	*high = 0
end sub

end extern
