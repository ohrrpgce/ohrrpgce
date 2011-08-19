'OHHRPGCE COMMON - Header for OS-specific routines
'Please read LICENSE.txt for GNU GPL License details and disclaimer of liability

#ifndef OS_BI
#define OS_BI

#include "util.bi"

extern "C"

'Actually in filelayer.cpp
declare function copyfile(source as string, destination as string) as integer

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
type ProcessHandle as PROCESS_INFORMATION ptr
type IPCChannel as integer   'dummy type
#define NULL_CHANNEL 0
#else
type ProcessHandle as integer  'dummy type
type IPCChannel as any ptr   'actually FILE*
#define NULL_CHANNEL NULL
#endif

declare function channel_open_read (chan_name as string, byval result as IPCChannel ptr) as integer
declare function channel_open_write (chan_name as string, byval result as IPCChannel ptr) as integer
declare sub channel_close (byval channel as IPCChannel ptr)
declare function channel_write (byval channel as IPCChannel, byval buf as byte ptr, byval buflen as integer) as integer
declare function channel_input_line (byval channel as IPCChannel, line_in as string) as integer


declare function open_console_process (program as string, args as string) as ProcessHandle
declare function process_running (byval process as ProcessHandle, byval exitcode as integer ptr = NULL) as integer
declare sub kill_process (byval process as ProcessHandle)
declare sub cleanup_process (byval process as ProcessHandle ptr)

end extern

#endif
