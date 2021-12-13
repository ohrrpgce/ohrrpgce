'OHRRPGCE - Odd header/module left over from the QuickBasic to FreeBASIC move
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
'
'FIXME: move this crud elsewhere. Does NOT correspond to misc.h and miscc.c!

#IFNDEF MISC_BI
#DEFINE MISC_BI

#INCLUDE "util.bi"

DECLARE SUB display_help_string (help as string)

DECLARE SUB getdefaultfont (font() as integer)
DECLARE SUB getbrowserfont (font() as integer)

DECLARE FUNCTION gamecustom_setoption(opt as string, arg as string) as integer

DECLARE SUB hook_after_attach_to_Custom(success as bool)

DECLARE FUNCTION ReadShort overload (byval fh as integer, byval p as long=-1) as short
DECLARE FUNCTION ReadShort overload (filename as string, byval p as integer) as short
DECLARE Sub WriteShort overload (byval fh as integer, byval p as long, byval v as integer)
DECLARE Sub WriteShort overload (byval fh as integer, byval p as long, byval v as short)
DECLARE Sub WriteShort overload (filename as string, byval p as integer, byval v as integer)
DECLARE FUNCTION ReadVStr(byval fh as integer, byval maxlen as integer) as string
DECLARE Sub WriteVStr(byval fh as integer, byval maxlen as integer, s as string)
DECLARE SUB WriteByte(byval fh as integer, byval v as ubyte, byval p as long=-1)
DECLARE FUNCTION ReadByte(byval fh as integer, byval p as long=-1) as ubyte
DECLARE SUB WriteByteStr(byval fh as integer, byval maxlen as integer, s as string)
DECLARE FUNCTION ReadByteStr(byval fh as integer, byval maxlen as integer) as string

''''Globals

' Globals telling whether certain commandline arguments have been seen.
EXTERN overrode_default_zoom as bool
EXTERN overrode_default_fullscreen as bool
EXTERN overrode_log_dir as bool

#ENDIF
