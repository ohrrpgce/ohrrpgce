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
#IFDEF SCRIPTPROFILE
 #DEFINE _SSTR & " script_profiling"
#ELSE
 #DEFINE _SSTR
#ENDIF
CONST build_info as string = "" _GSTR _ESTR _SSTR _PSTR

EXTERN as string gfxbackend, musicbackend
EXTERN as string gfxbackendinfo, musicbackendinfo

#undef getkey

'included only for $inclib?
#include once "crt.bi"
#undef rand
#undef abort
#undef bound
#undef strlen

'it was too awful (collision-wise) to include all of windows.bi
#macro include_windows_bi()
'# include "windows.bi"
# ifndef windows_bi_included
#  define windows_bi_included
#  undef point
#  define _X86_
#  include "win/windef.bi"
#  include "win/winbase.bi"
#  undef max
#  undef min
#  undef getcommandline
#  undef copyfile
#  undef istag
# endif
#endmacro

#if  __FB_VERSION__ = "0.15"
'use native gosubs

#define retrace return
#define retrievestate
#define rememberstate
#define crt_jmp_buf byte

#elseif 1
'use nearly-as-fast assembly version (one extra jump)

#undef gosub
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
#endif  'choose GOSUB workaround

'#DEFINE CLEAROBJ(OBJ) memset(@(OBJ),0,LEN(OBJ))
'#DEFINE COPYOBJ(TO,FROM) memcpy(@(TO),@(FROM),LEN(FROM))

#ifdef __FB_LINUX__
#define LINUX -1
#define SLASH "/"
#define LINE_END !"\n"
#define CUSTOMEXE "ohrrpgce-custom"
#define ALLFILES "*"
#else
#define LINUX 0
#define SLASH "\"
#define LINE_END !"\r\n"
#define CUSTOMEXE "CUSTOM.EXE"
#define ALLFILES "*.*"
#endif

'Warning: you may not nest TIMER_STOP/START calls!

'under windows, TIMER uses QueryPerformanceCounter, under unix it uses gettimeofday
#ifdef ACCURATETIMER
 'use a timer which counts CPU time spent by this process (preferably thread) only
 #ifdef __FB_WIN32__
  'only available on win 2000 or later
  include_windows_bi()
  #if defined(GetThreadTimes)
   #define timer_variables  as FILETIME ptr atimer_s, atimer_e
   extern timer_variables
   #define TIMER_START(a)  GetThreadTimes(GetCurrentThread, NULL, NULL, NULL, @atimer_s)
   #define TIMER_STOP(a)  GetThreadTimes(GetCurrentThread, NULL, NULL, NULL, @atimer_e): a += (atimer_e.dwLowDateTime - atimer_s.dwLowDateTime) * 0.0000001
  #else
   #print GetThreadTimes not available
  #endif
 #else
  'assume anything else is a unix
  'options: clock, times, clock_gettime (with CLOCK_THREAD_CPUTIME_ID) which apparently counts in clock ticks (1ms)
  #define timer_variables as timespec atimer_s, atimer_e
  extern timer_variables
  #define TIMER_START(a)  clock_gettime(CLOCK_THREAD_CPUTIME_ID, @atimer_s)
  #define TIMER_STOP(a)  clock_gettime(CLOCK_THREAD_CPUTIME_ID, @atimer_e): a += (atimer_e.tv_nsec - atimer_s.tv_nsec) * 0.000000001
 #endif
#endif
#ifndef TIMER_START
 #define TIMER_START(a) a -= TIMER
 #define TIMER_STOP(a)  a += TIMER
#endif

declare function xstr (x as integer) as string

DECLARE SUB display_help_string (help as string)
DECLARE SUB getdefaultfont (font() as integer)
DECLARE SUB xbload (f as string, array() as integer, e as string)
DECLARE SUB xbsave (f as string, array() as integer, bsize as integer)
DECLARE SUB crashexplain ()
DECLARE function with_orig_path(dir_name as string, add_slash as integer=0) as string
declare sub processcommandline()
DECLARE SUB romfontchar (font() as integer, char as integer)
DECLARE SUB makedir (dirname as string)
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

'not really a global: just an empty string, since you can't specify zstring ptr default arguments directly
EXTERN nulzstr as zstring ptr

#ENDIF
