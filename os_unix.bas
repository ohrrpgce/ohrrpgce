'OHHRPGCE COMMON - Generic Unix versions of OS-specific routines
'Please read LICENSE.txt for GNU GPL License details and disclaimer of liability

#include "os.bi"
#include "compat.bi"
#include "crt/limits.bi"
#include "common.bi"
#include "allmodex.bi"
#include "util.bi"
#include "const.bi"

option explicit


'==========================================================================================
'                                       Filesystem
'==========================================================================================


function drivelist (drives() as string) as integer
	' on Unix there is only one drive, the root /
	drivelist = 0
end function

function drivelabel (drive as string) as string
	drivelabel = ""
end function

function isremovable (drive as string) as integer
	isremovable = 0
end function

function hasmedia (drive as string) as integer
	hasmedia = 0
end function

sub setwriteable (fname as string)
	'Not written because I don't know whether it's actually needed: does
	'filecopy on Unix also copy file permissions?
end sub


'==========================================================================================
'                                       Processes
'==========================================================================================


'Returns 0 on failure.
'If successful, you should call cleanup_process with the handle after you don't need it any longer.
'This is currently designed for running console applications. Could be
'generalised in future as needed.
function open_console_process (program as string, args as string) as ProcessHandle
	'Unimplemented and not yet used
	return 0
end function

'If exitcode is nonnull and the process exited, the exit code will be placed in it
function process_running (byval process as ProcessHandle, byval exitcode as integer ptr = NULL) as integer
	'Unimplemented and not yet used
	return NO
end function

sub kill_process (byval process as ProcessHandle)
	'Unimplemented and not yet used
end sub

'Cleans up resources associated with a ProcessHandle
sub cleanup_process (byval process as ProcessHandle ptr)
	'Unimplemented and not yet used
end sub
