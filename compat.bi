'' Compatibility functions/definitions for OHRRPGCE
' This file is specific to either FreeBasic or QuickBasic and
' defines the necessary functions to maintain compatibility
' between the two

'' FreeBasic version

#IFNDEF COMPAT_BI
#DEFINE COMPAT_BI

'version strings
#IFDEF IS_GAME
#INCLUDE "gver.txt"
#ENDIF
#IFDEF IS_CUSTOM
#INCLUDE "cver.txt"
#ENDIF

#IF __FB_DEBUG__
 #DEFINE _GSTR & " -g"
#ELSE
 #DEFINE _GSTR
#ENDIF
#IF __FB_ERR__
 #DEFINE _ESTR & " -exx"
#ELSE
 #DEFINE _ESTR
#ENDIF
#IFDEF __FB_LINUX__
 #DEFINE _PSTR & " Linux"
#ELSEIF defined(__FB_WIN32__)
 #DEFINE _PSTR & " Win32"
#ELSE
 #DEFINE _PSTR & " Other"
#ENDIF
CONST build_info$ = "" _GSTR _ESTR _PSTR


option nokeyword getkey
option nokeyword setmouse

'included only for $inclib?
#include once "crt.bi"
#undef rand
#undef abort
#undef bound
#undef strlen

#ifndef DEMACRO
#ifndef DONESTR
#define fbdim dim as short
#define DONESTR
#endif

#if  __FB_VERSION__ = "0.15"
'use native gosubs

#define retrace return
#define retrievestate
#define rememberstate
#define crt_jmp_buf byte

#elseif 1
'use nearly-as-fast assembly version (one extra jump)

option nokeyword gosub
#define gosub _gosub_beta(__LINE__,__FUNCTION_NQ__)
'the "if 0 then" is used to place a label after the goto
#define _gosub_beta(a,b) asm : call gosub_##b##_line_##a end asm : if 0 then asm : gosub_##b##_line_##a: end asm : goto
#define retrace asm ret
#define retrievestate
#define rememberstate
#define crt_jmp_buf byte

#else  'choose GOSUB workaround

'alternative to above blocks, use this code on non x86 platforms
'use a setjmp/longjmp kludge

'#include "crt/setjmp.bi"
' setjmp.bi is incorrect
type crt_jmp_buf:dummy(63) as byte:end type
#ifdef __FB_WIN32__
declare function setjmp cdecl alias "_setjmp" (byval as any ptr) as integer
#else
declare function setjmp cdecl alias "setjmp" (byval as any ptr) as integer
#endif
declare sub longjmp cdecl alias "longjmp" (byval as any ptr, byval as integer)

extern gosubbuf() as crt_jmp_buf
extern gosubptr as integer
option nokeyword gosub
#define gosub if setjmp(@gosubbuf(gosubptr)) then gosubptr-=1 else gosubptr+=1:goto
#define retrace longjmp(@gosubbuf(gosubptr-1),1)
#define retrievestate gosubptr=localgosubptr
#define rememberstate localgosubptr=gosubptr
#endif

'#DEFINE CLEAROBJ(OBJ) memset(@(OBJ),0,LEN(OBJ))
'#DEFINE COPYOBJ(TO,FROM) memcpy(@(TO),@(FROM),LEN(FROM))

#endif  'choose GOSUB workaround

#ifdef __FB_LINUX__
#define LINUX -1
#define SLASH "/"
#define LINE_END CHR(10)
#define CUSTOMEXE "ohrrpgce-custom"
#define ALLFILES "*"
#else
#define LINUX 0
#define SLASH "\"
#define LINE_END CHR(13) & CHR(10)
#define CUSTOMEXE "CUSTOM.EXE"
#define ALLFILES "*.*"
#endif
#define ISDOS 0

declare function xstr$ overload (x as integer)
declare function xstr$ (x as short)
declare function xstr$ (x as single)
declare function xstr$ (x as double)
declare function xstr$ (x as long)
declare function intstr$ (x as integer)

DECLARE SUB getdefaultfont (font() as integer)
DECLARE SUB xbload (f$, array%(), e$)
DECLARE SUB xbsave (f$, array%(), bsize%)
DECLARE SUB crashexplain ()
declare sub togglewindowed()
declare sub processcommandline()
DECLARE function commandlineargcount() as integer
DECLARE function commandlinearg(argnum as integer) as string
DECLARE SUB romfontchar (font%(), char%)
DECLARE SUB makedir (dirname$)
DECLARE SUB setwindowtitle (title as string)
DECLARE FUNCTION ReadShort overload (fh as integer,p as long=-1) as short
DECLARE FUNCTION ReadShort overload (filename as string, p as integer) as short
DECLARE Sub WriteShort overload (fh as integer,p as long, v as integer)
DECLARE Sub WriteShort overload (fh as integer,p as long, v as short)
DECLARE Sub WriteShort overload (filename as string, p as integer, v as integer)
DECLARE FUNCTION ReadVStr(fh as integer, le as integer) as string
DECLARE Sub WriteVStr(fh as integer, le as integer, s as string)
DECLARE SUB WriteByte(fh as integer,v as ubyte, p as long=-1)
DECLARE FUNCTION ReadByte(fh as integer,p as long=-1) as ubyte
DECLARE SUB WriteByteStr(fh as integer, le as integer, s as string)
DECLARE FUNCTION ReadByteStr(fh as integer, le as integer) as string

#ENDIF
