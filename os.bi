'OHRRPGCE COMMON - Header for OS-specific routines
'Please read LICENSE.txt for GNU GPL License details and disclaimer of liability
'
'Implementations are in os_unix.c, os_windows.bas and os_windows2.c
'Documentation is sadly to be found duplicated in those files, with many differences,
'so check both Unix and Windows implementations before use

#ifndef OS_BI
#define OS_BI

#include "util.bi"

#ifdef __FB_WIN32__
declare function is_windows_9x () as bool
declare function get_windows_version () as string
#endif

extern "C"

declare sub init_runtime ()

declare sub external_log (msg as string)

declare function memory_usage() as integer
declare function memory_usage_string() as string

'Actually in filelayer.cpp
declare function copyfile(source as string, destination as string) as integer

declare function list_files (searchdir as string, nmask as string, byval showhidden as bool) as string vector
declare function list_subdirs (searchdir as string, nmask as string, byval showhidden as bool) as string vector

declare function drivelist (d() as string) as integer
declare function drivelabel (drive as string) as string
declare function isremovable (drive as string) as integer
declare function hasmedia (drive as string) as integer

declare sub setwriteable (fname as string)

'C FILE* type. Can be retrieved with FILEATTR from a FB filehandle
type CFILE_ptr as any ptr

'Advisory locking (actually mandatory on Windows).
declare function lock_file_for_write (byval fh as CFILE_ptr, byval timeout_ms as integer) as integer
declare function lock_file_for_read (byval fh as CFILE_ptr, byval timeout_ms as integer) as integer
declare sub unlock_file (byval fh as CFILE_ptr)
declare function test_locked (filename as string, byval writable as integer) as integer

#ifdef __FB_WIN32__

type NamedPipeInfoFwd as NamedPipeInfo

type ProcessHandle as PROCESS_INFORMATION ptr
'type IPCChannel as HANDLE
'#define NULL_CHANNEL INVALID_HANDLE_VALUE
type IPCChannel as NamedPipeInfoFwd ptr
#define NULL_CHANNEL NULL

declare function open_document (filename as string) as string

#else

type ProcessHandle as any ptr  'actually FILE*
type IPCChannel as any ptr   'actually FILE*
#define NULL_CHANNEL NULL

#endif

'IPCChannel functions will automatically close an IPCChannel and set it equal to NULL_CHANNEL if there is an error

'declare function channel_pick_name (byval id as zstring ptr, byval tempdir as zstring ptr, byval rpg as zstring ptr) as string
declare function channel_open_client (byref channel as IPCChannel, chan_name as string) as integer
declare function channel_open_server (byref channel as IPCChannel, chan_name as string) as integer
declare sub channel_close (byref channel as IPCChannel)
declare function channel_wait_for_client_connection (byref channel as IPCChannel, byval timeout_ms as integer) as integer
declare function channel_write (byref channel as IPCChannel, byval buf as any ptr, byval buflen as integer) as integer
declare function channel_write_line (byref channel as IPCChannel, buf as string) as integer
declare function channel_input_line (byref channel as IPCChannel, line_in as string) as integer


declare function open_process (program as string, args as string) as ProcessHandle
declare function open_piped_process (program as string, args as string, byval iopipe as IPCChannel ptr) as ProcessHandle
declare function open_console_process (program as string, args as string) as ProcessHandle
declare function process_running (byval process as ProcessHandle, byval exitcode as integer ptr = NULL) as integer
declare sub kill_process (byval process as ProcessHandle)
declare sub cleanup_process (byval process as ProcessHandle ptr)

declare function get_process_id () as integer


'Only implemented for X11 and Windows, sets to 0 otherwise or on error
'NOTE: call get_screen_size instead of this.
declare sub os_get_screen_size(wide as integer ptr, high as integer ptr)

end extern

#endif
