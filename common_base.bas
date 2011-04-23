'OHRRPGCE - Common code for utilities
'
'Please read LICENSE.txt for GPL License details and disclaimer of liability
'
'This module is for code to be linked into utilities, but not Game and Custom.
'Currently mostly replacements for common.bas functions.

#include "common_base.bi"

DIM cleanup_function as sub ()

SUB debug (s as string)
  print s
END SUB

SUB debuginfo (s as string)
  print s
END SUB

SUB debugc cdecl alias "debugc" (byval s as zstring ptr, byval errorlevel as integer)
  IF errorlevel >= 4 THEN fatalerror *s
  IF errorlevel = 3 THEN print "ERROR: ",
  print *s
END SUB

SUB fatalerror (e as string)
  IF e <> "" THEN print "ERROR: " + e
  IF cleanup_function THEN cleanup_function()
  SYSTEM
END SUB

FUNCTION readkey () as string
  DO
    DIM w as string = INKEY
    IF w <> "" THEN RETURN w
  LOOP
END FUNCTION

FUNCTION rightafter (s as string, d as string) as string
  DIM result as string
  FOR i = LEN(s) TO 1 STEP -1
   IF MID(s, i, 1) = d THEN
    RETURN result
   END IF
   result += MID(s, i, 1)
  NEXT i
  RETURN ""
END FUNCTION
