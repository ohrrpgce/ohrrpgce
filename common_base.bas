'OHRRPGCE - Common code for utilities
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'
'This module is for code to be linked into utilities, but not Game and Custom.
'These are replacements for common.rbas functions in a non-graphical environment.

#include "config.bi"
#include "common_base.bi"
#include "file.bi"

DIM workingdir as string

DIM cleanup_function as sub ()

SUB debug (s as string)
  print s
END SUB

SUB debuginfo (s as string)
  print s
END SUB

SUB debugc cdecl alias "debugc" (byval errorlevel as errorLevelEnum, byval s as zstring ptr)
  IF errorlevel >= errFatal THEN fatalerror *s
  IF errorlevel = errBug OR errorlevel = errPromptBug OR errorlevel = errFatalBug THEN print "(BUG) ",
  IF errorlevel >= errError THEN print "ERROR: ",
  print *s
END SUB

SUB showerror (msg as string, isfatal as bool = NO, isbug as bool = NO)
 IF isfatal THEN
  fatalerror msg
 ELSE
  print msg
 END IF
END SUB

SUB visible_debug (msg as string, errlvl as errorLevelEnum = errDebug)
 debugc errlvl, msg
 'notification msg + !"\nPress any key..."
END SUB

SUB fatalerror (e as string)
  IF e <> "" THEN print "ERROR: " + e
  IF cleanup_function THEN cleanup_function()
  SYSTEM 1
END SUB
