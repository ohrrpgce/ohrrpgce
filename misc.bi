'OHRRPGCE Common - Odd header/module left over from the QuickBasic to FreeBASIC move
'FIXME: move this crud elsewhere

#IFNDEF MISC_BI
#DEFINE MISC_BI

#INCLUDE "util.bi"

declare function xstr (byval x as integer) as string

DECLARE SUB display_help_string (help as string)

DECLARE SUB getdefaultfont (font() as integer)
DECLARE SUB getbrowserfont (font() as integer)

DECLARE FUNCTION gamecustom_setoption(opt as string, arg as string) as integer

DECLARE SUB crashexplain ()
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
