'OHHRPGCE COMMON - Header for OS-specific routines
'Please read LICENSE.txt for GNU GPL License details and disclaimer of liability

#ifndef OS_BI
#define OS_BI

#include "util.bi"

declare function drivelist (d() as string) as integer
declare function drivelabel (drive as string) as string
declare function isremovable (drive as string) as integer
declare function hasmedia (drive as string) as integer

#ifdef __FB_WIN32__
type ProcessHandle as PROCESS_INFORMATION ptr
#else
'dummy type
type ProcessHandle as integer
#endif

declare function open_console_process (program as string, args as string) as ProcessHandle
declare function process_running (byval process as ProcessHandle, byval exitcode as integer ptr = NULL) as integer
declare sub kill_process (byval process as ProcessHandle)
declare sub cleanup_process (byval process as ProcessHandle ptr)

#endif
