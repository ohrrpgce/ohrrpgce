'OHRRPGCE - Common Game/Custom commandline arg processing
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#IFNDEF CMDLINE_BI
#DEFINE CMDLINE_BI

DECLARE FUNCTION gamecustom_setoption(opt as string, arg as string) as integer

DECLARE SUB hook_after_attach_to_Custom(success as bool)

''''Globals

' Globals telling whether certain commandline arguments have been seen.
EXTERN overrode_default_zoom as bool
EXTERN overrode_default_fullscreen as bool
EXTERN overrode_log_dir as bool

#ENDIF
