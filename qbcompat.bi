'' Compatibility functions/definitions for OHRRPGCE
' This file is specific to either FreeBasic or QuickBasic and
' defines the necessary functions to maintain compatibility
' between the two

'' QuickBasic version

DECLARE SUB getdefaultfont (font() AS INTEGER)
DECLARE SUB xbload (f$, array%(), e$)
DECLARE SUB crashexplain ()
DECLARE SUB fbdim (v%)
DECLARE SUB playsongnum (songnum%)
DECLARE FUNCTION canplay (file$)
DECLARE SUB defseg (vbl AS INTEGER)
DECLARE SUB xbsave (f$, array%(), bsize%)
DECLARE SUB togglewindowed ()
DECLARE SUB storecommandline (temppath AS STRING)
DECLARE FUNCTION getcommandline$ (temppath AS STRING)
DECLARE SUB romfontchar (font%(), char%)
DECLARE SUB makedir (dirname$)
DECLARE SUB setwindowtitle (title AS STRING)

CONST SLASH = "\"
CONST CUSTOMEXE = "CUSTOMQB.EXE"
CONST LINUX = 0
CONST ALLFILES = "*.*"