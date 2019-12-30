'OHRRPGCE - Common code for utilities
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'
'This module is for code to be linked into utilities, but not Game and Custom.
'These are replacements for common.rbas functions in a non-graphical environment.

#include "config.bi"
#include "common_base.bi"
#include "util.bi"

DIM workingdir as string

DIM cleanup_function as sub ()

DIM app_name as zstring ptr = NULL  'NULL, so exename is used instead
DIM app_log_filename as zstring ptr = NULL
DIM app_archive_filename as zstring ptr = NULL

SUB debug (msg as const zstring ptr)
  print *msg
END SUB

SUB debuginfo (msg as const zstring ptr)
  print *msg
END SUB

SUB early_debuginfo (msg as const zstring ptr)
  'Don't want to print startup stuff like setup_exception_handler
  'print s
END SUB

EXTERN "C"

SUB onetime_debug (errorlevel as errorLevelEnum = errDebug, msg as const zstring ptr)
  STATIC silenced_messages() as string
  IF a_find(silenced_messages(), *CAST(zstring ptr, msg)) <> -1 THEN EXIT SUB
  debugc_internal NULL, errorlevel, msg
  a_append silenced_messages(), *msg
END SUB

SUB debugc_internal (callsite as any ptr, errorlevel as errorLevelEnum, s as const zstring ptr)
  IF errorlevel >= errFatalError THEN fatalerror s
  IF errorlevel = errBug OR errorlevel = errShowBug OR errorlevel = errFatalBug THEN print "(BUG) ",
  IF errorlevel >= errError THEN print "ERROR: ",
  print *s
END SUB

SUB showerror_internal (callsite as any ptr, msg as const zstring ptr, isfatal as bool = NO, isbug as bool = NO)
 IF isfatal THEN
  fatalerror msg
 ELSE
  print *msg
 END IF
END SUB

END EXTERN

SUB debugerror (msg as const zstring ptr)
  debugc errError, msg
END SUB

SUB visible_debug (msg as const zstring ptr)
  debugc errShowDebug, msg
END SUB

SUB fatalerror (msg as const zstring ptr)
  IF LEN(*msg) THEN print "ERROR: " + *msg
  IF cleanup_function THEN cleanup_function()
  SYSTEM 1
END SUB

SUB fatalbug (msg as const zstring ptr)
  debugc errFatalBug, msg
END SUB

' SUB showbug (msg as const zstring ptr)
'   debugc errShowBug, msg
' END SUB
