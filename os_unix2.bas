'OHRRPGCE - Generic Unix versions of OS-specific routines
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
'
'This module is for Unix-specific functions that are more easily implemented
'in FB than in C.
'(But in fact almost all such code is in util.bas or common.rbas rather than here)

#include "config.bi"
#include "os.bi"
#include "util.bi"

extern "C"

' This may return either the name that the process was called with, which may have a full, relative, or no path,
' or just return the full path to the executable. Not necessarily equal to COMMAND(0).
' This function is used only to determine whether a process is still running; its meaning is OS-specific.
' Returns "" if invalid or don't have permission.
' (Should return "<unknown>" if the pid exists but we can't get the path)
function get_process_name (pid as integer) as string
#if defined(MINIMAL_OS)
	return ""
#else
	dim cmdname as string
#if defined(__GNU_LINUX__)
	' With GNU ps, "-o command" and "-o cmd" return the name and arguments it was called with,
	' and "-o comm" is just the first 15 characters of the command name after stripping the path.
	' It appears to be impossible to get the non-truncated command name and path without also getting
	' the args and other post-processing to process stuff like "kdeinit4: ksysguard [kdeinit]"
	' The alternative, reading /proc/$pid/exe (linux-specific) changes if the exe is moved or deleted,
        ' which makes it unreliable for checking if the same pid and exe pair are still running.
	run_and_get_output("ps -p " & pid & " -o comm=", cmdname)
	'run_and_get_output("readlink /proc/" & pid & "/exe", cmdname)
#elseif  defined(__FB_ANDROID__)
	' On Android 4.4.2, ps doesn't support -o comm= option, and the output looks like
	'USER     PID   PPID  VSIZE  RSS   PRIO  NICE  RTPRI SCHED   WCHAN    PC         NAME
	'u0_a115   9481  159   380324 27624 20    0     0     0     ffffffff 00000000 S com.hamsterrepublic.ohrrpgce.custom
	run_and_get_output("ps -p " & pid, cmdname)
	dim where as integer = instrrev(cmdname, " ")
	if where then
		cmdname = mid(cmdname, where + 1)
	else
		cmdname = "<unknown>"
	end if
#else
	' On OSX (BSD) "-o comm" returns the name the command was called with (which may or may not include a path),
	' "-o command" adds the arguments, and "-o cmd" does not work.
	run_and_get_output("ps -p " & pid & " -o command=", cmdname)
#endif
	return rtrim(cmdname, !"\n")
#endif
end function

end extern
