'' Compatibility functions/definitions for OHRRPGCE
' This file is specific to either FreeBasic or QuickBasic and
' defines the necessary functions to maintain compatibility
' between the two

'' QuickBasic version

DECLARE SUB getdefaultfont (font() as integer)
DECLARE SUB xbload (f$, array%(), e$)
DECLARE SUB xbsave (f$, array%(), bsize%)
''only used in game.bas, maybe don't declare here?
DECLARE SUB crashexplain ()
DECLARE SUB fbdim (v%)
