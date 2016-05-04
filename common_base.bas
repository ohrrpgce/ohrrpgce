'OHRRPGCE - Common code for utilities
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'
'This module is for code to be linked into utilities, but not Game and Custom.
'Currently mostly replacements for common.bas functions.

#include "common_base.bi"
#include "file.bi"

EXTERN workingdir as string
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

SUB showerror (msg as string, byval isfatal as integer = 0)
 IF isfatal THEN
  fatalerror msg
 ELSE
  print msg
 END IF
END SUB

SUB fatalerror (e as string)
  IF e <> "" THEN print "ERROR: " + e
  IF cleanup_function THEN cleanup_function()
  SYSTEM 1
END SUB

FUNCTION readkey () as string
  DO
    DIM w as string = INKEY
    IF w <> "" THEN RETURN w
  LOOP
END FUNCTION

FUNCTION rightafter (s as string, d as string) as string
  DIM result as string
  FOR i as integer = LEN(s) TO 1 STEP -1
   IF MID(s, i, 1) = d THEN
    RETURN result
   END IF
   result += MID(s, i, 1)
  NEXT i
  RETURN ""
END FUNCTION
