'OHRRPGCE COMMON - Generic Unix versions of OS-specific routines
'Please read LICENSE.txt for GNU GPL License details and disclaimer of liability
'
'This module is for Unix-specific functions that are more easily implemented
'in FB than in C.
'(But in fact almost all such code is in util.bas or common.rbas rather than here)

#include "config.bi"
#include "os.bi"

' This may return either the name that the process was called with, which may have a full, relative, or no path,
' or just return the full path to the executable. Not necessarily equal to COMMAND(0).
' Returns "" if invalid or don't have permission.
function get_process_path (pid as integer) as string
	dim cmdname as string
#ifdef __FB_LINUX__
	' With GNU ps, "-o command" and "-o cmd" return the name and arguments it was called with,
	' and "-o comm" is just the first 15 characters of the command name after stripping the path.
	' It appears to be impossible to get the non-truncated command name and path without also getting
	' the args and other post-processing to process stuff like "kdeinit4: ksysguard [kdeinit]"
	' Therefore better to use the linux-specific procfs.
	'run_and_get_output("ps -p " & pid & " -o comm=", cmdname)
	run_and_get_output("readlink /proc/" & pid & "/exe", cmdname)
#else
	' On OSX (BSD) "-o comm" returns the name the command was called with (which may or may not include a path),
	' "-o command" adds the arguments, and "-o cmd" does not work.
	run_and_get_output("ps -p " & pid & " -o command=", cmdname)
#endif
	return rtrim(cmdname, !"\n")
end function
