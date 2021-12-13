'OHRRPGCE - Header for OS-specific routines
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
'
'Implementations are in os_unix.c, os_unix2.bas, os_unix_wm.bas, os_windows.bas, os_windows2.c, os_sockets.c
'Documentation is sadly to be found duplicated in those files, with many differences,
'so check both Unix and Windows implementations before use

#ifndef OS_BI
#define OS_BI

#include "vector.bi"

extern "C"

declare function is_windows_9x () as bool
#ifdef __FB_WIN32__
declare function get_windows_version () as string
declare function get_windows_runtime_info () as string
declare function win_error_str(errcode as integer = -1) as zstring ptr
#endif

declare sub os_init ()

declare sub external_log (msg as const zstring ptr)

declare function memory_usage() as integer
declare function memory_usage_string() as string

declare function setup_exception_handler () as boolint
declare function save_backtrace (show_message as bool = YES) as boolint
declare function send_bug_report (msg as const zstring ptr) as boolint
declare sub interrupt_self ()
declare sub os_open_logfile (path as const zstring ptr)
declare sub os_close_logfile ()

'Actually in filelayer.cpp
declare function copyfile(source as string, destination as string) as boolint
declare function renamefile(source as string, destination as string) as boolint

declare function copy_file_replacing(source as zstring ptr, destination as zstring ptr) as boolint
declare function os_rename(source as zstring ptr, destination as zstring ptr) as boolint

enum FileTypeEnum
  fileTypeNonexistent ' Doesn't exist (or parent directory doesn't exist)
  fileTypeFile        ' Regular file or a symlink to one
  fileTypeDirectory   ' Directory or a symlink to one
  fileTypeOther       ' A device, fifo, or other special file type
  fileTypeError       ' Something unreadable (including broken symlinks)

  fileTypeFileOrDir = 10 ' Special argument to findfiles ONLY. NOT a file type!
end enum

declare function get_file_type (fname as string) as FileTypeEnum

declare function list_files_or_subdirs (searchdir as string, nmask as string, showhidden as bool, whichtype as integer) as string vector
declare function list_files (searchdir as string, nmask as string, byval showhidden as bool) as string vector
declare function list_subdirs (searchdir as string, nmask as string, byval showhidden as bool) as string vector

#ifdef __FB_WIN32__
  declare function os_get_documents_dir() as string
#endif
declare function drivelist (d() as string) as integer
declare function drivelabel (drive as string) as string
declare function isremovable (drive as string) as integer
declare function hasmedia (drive as string) as integer

declare function setwriteable (fname as string, towhat as bool) as bool

'C FILE* type. Can be retrieved with FILEATTR from a FB filehandle
type CFILE_ptr as any ptr

'Advisory locking (actually mandatory on Windows).
declare function lock_file_for_write (fh as CFILE_ptr, filename as zstring ptr, timeout_ms as integer) as integer
declare function lock_file_for_read (fh as CFILE_ptr, filename as zstring ptr, timeout_ms as integer) as integer
declare sub unlock_file (byval fh as CFILE_ptr)
declare function test_locked (filename as string, byval writable as integer) as integer

#ifdef __FB_WIN32__

type NamedPipeInfoFwd as NamedPipeInfo

type ProcessHandle as PROCESS_INFORMATION ptr
type IPCChannel as NamedPipeInfoFwd ptr

declare function os_open_document (filename as string) as string

#else

type ProcessInfo
  waitable as boolint
  file as FILE ptr
  pid as integer
end type
type ProcessHandle as ProcessInfo ptr
type IPCChannel as FILE ptr

declare function checked_system (cmdline as zstring ptr) as integer

#endif

'IPCChannel functions will automatically close an IPCChannel and set it equal to NULL if there is an error

'declare function channel_pick_name (byval id as zstring ptr, byval tempdir as zstring ptr, byval rpg as zstring ptr) as string
declare function channel_open_client (byref channel as IPCChannel, chan_name as string) as integer
declare function channel_open_server (byref channel as IPCChannel, chan_name as string) as integer
declare sub channel_close (byref channel as IPCChannel)
declare function channel_wait_for_client_connection (byref channel as IPCChannel, byval timeout_ms as integer) as integer
declare function channel_write (byref channel as IPCChannel, byval buf as any ptr, byval buflen as integer) as integer
declare function channel_write_line (byref channel as IPCChannel, buf as string) as integer
declare function channel_input_line (byref channel as IPCChannel, line_in as string) as integer

'Networking

type HTTPRequest
	failed as boolint
	failmsg as zstring ptr       ' When failed is true, a string containing the error message
	started as boolint           ' HTTP_request called, cleanup needed
	response as ubyte ptr        ' Response with the header stripped
	response_len as integer      ' Length of response, NOT response_buf
	response_buf as zstring ptr  ' Response with the header
	status as integer            ' HTTP status, 200 for success
	status_string as zstring ptr ' Returned from the server, may be anything
	_dummy as any ptr
end type

'In os_sockets.c
declare sub HTTP_Request_init(req as HTTPRequest ptr)
declare sub HTTP_Request_destroy(req as HTTPRequest ptr)
declare function HTTP_request(req as HTTPRequest ptr, url as const zstring ptr, verb as const zstring ptr, content_type as const zstring ptr = NULL, data as const ubyte ptr = NULL, datalen as integer = 0) as boolint

'In networkutil.bi
declare sub POST_add_text_part(byref buffer as string, name as string, contents as string)
declare sub POST_add_file_part(byref buffer as string, name as string, filename as string, content_type as string, contents as string)
declare sub POST_add_final_part(byref buffer as string)
declare function multipart_POST_request(req as HTTPRequest ptr, url as string, buffer as string) as boolint

'Threads

declare function on_main_thread () as bool

type TLSKey as intptr_t
declare function tls_alloc_key() as TLSKey
declare sub tls_free_key(key as TLSKey)
declare function tls_get(key as TLSKey) as any ptr
declare sub tls_set(key as TLSKey, value as any ptr)

'Processes

declare function open_process (program as string, args as string, waitable as boolint, show_output as boolint) as ProcessHandle
declare function open_piped_process (program as string, args as string, byval iopipe as IPCChannel ptr) as ProcessHandle
' run_process_and_get_output is Unix only
declare function run_process_and_get_output(program as string, args as string, outdata as string) as integer
declare function open_console_process (program as string, args as string) as ProcessHandle
declare function process_running (byval process as ProcessHandle, byval exitcode as integer ptr = NULL) as boolint
declare function wait_for_process (process as ProcessHandle ptr, timeoutms as integer = 4000) as integer
declare sub kill_process (byval process as ProcessHandle)
declare sub cleanup_process (byval process as ProcessHandle ptr)

declare function get_process_id () as integer
declare function get_process_path (pid as integer) as string

'WM and screen

'Only implemented for X11 and Windows, sets to 0 otherwise or on error
'NOTE: call get_screen_size instead of this.
declare sub os_get_screen_size(wide as integer ptr, high as integer ptr)

#ifdef USE_X11
declare sub set_X11_error_handlers()
#endif

end extern

#endif
