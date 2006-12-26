'' Compatibility functions/definitions for OHRRPGCE
' This file is specific to either FreeBasic or QuickBasic and
' defines the necessary functions to maintain compatibility
' between the two

'' FreeBasic version

#IFNDEF COMPAT_BI
#DEFINE COMPAT_BI

#include "music.bi"

option nokeyword getkey
option nokeyword setmouse

#ifndef DEMACRO
#ifndef DONESTR
#define fbdim dim as short
#define DONESTR
#endif

'included only for $inclib?
#include once "crt.bi"
'#include "crt/setjmp.bi"
' setjmp.bi is incorrect
type crt_jmp_buf:dummy(63) as byte:end type
#ifdef __FB_WIN32__
declare function setjmp cdecl alias "_setjmp" (byval as any ptr) as integer
#else
declare function setjmp cdecl alias "setjmp" (byval as any ptr) as integer
#endif
declare sub longjmp cdecl alias "longjmp" (byval as any ptr, byval as integer)
#undef rand
#undef abort
#undef bound
#undef strlen

extern gosubbuf() as crt_jmp_buf
extern gosubptr as integer
option nokeyword gosub
#define gosub if setjmp(@gosubbuf(gosubptr)) then gosubptr-=1 else gosubptr+=1:goto
#define retrace longjmp(@gosubbuf(gosubptr-1),1)
#define retrievestate gosubptr=localgosubptr
#define rememberstate localgosubptr=gosubptr

'#DEFINE CLEAROBJ(OBJ) memset(@(OBJ),0,LEN(OBJ))
'#DEFINE COPYOBJ(TO,FROM) memcpy(@(TO),@(FROM),LEN(FROM))

#endif

#ifdef __FB_LINUX__
#define LINUX -1
#define SLASH "/"
#define CUSTOMEXE "ohrrpgce-custom"
#define ALLFILES "*"
#else
#define LINUX 0
#define SLASH "\"
#define CUSTOMEXE "CUSTOM.EXE"
#define ALLFILES "*.*"
#endif
#define ISDOS 0

declare function xstr$ overload (x as integer)
declare function xstr$ (x as short)
declare function xstr$ (x as single)
declare function xstr$ (x as double)
declare function intstr$ (x as integer)

DECLARE SUB getdefaultfont (font() as integer)
DECLARE SUB xbload (f$, array%(), e$)
DECLARE SUB xbsave (f$, array%(), bsize%)
DECLARE SUB crashexplain ()
declare sub togglewindowed()
declare sub processcommandline()
DECLARE function commandlineargcount() as integer
DECLARE function commandlinearg(argnum as integer) as string
DECLARE FUNCTION validmusicfile (file$, as integer = FORMAT_BAM AND FORMAT_MIDI)
DECLARE FUNCTION getmusictype (file$)
DECLARE SUB playsongnum (songnum%)
DECLARE SUB romfontchar (font%(), char%)
DECLARE SUB makedir (dirname$)
DECLARE SUB setwindowtitle (title as string)
DECLARE FUNCTION ReadShort(fh as integer,p as long) as short
DECLARE Sub WriteShort(fh as integer,p as long, v as integer)

#ENDIF
