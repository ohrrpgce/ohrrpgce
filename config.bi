'OHRRPGCE Common - Configuration/platform specific/important macros
'This file is (should be) included everywhere, and is a dumping ground for macros and other global declarations

#IFNDEF CONFIG_BI
#DEFINE CONFIG_BI


'====================================== Build string ======================================

#IF __FB_DEBUG__
 #DEFINE _GSTR " -g"
#ELSE
 #DEFINE _GSTR
#ENDIF
#IF __FB_ERR__
 #DEFINE _ESTR " -exx"
#ELSE
 #DEFINE _ESTR
#ENDIF
#IF __FB_GCC__
 #DEFINE _GENSTR " -gen gcc"
#ELSE
 #DEFINE _GENSTR
#ENDIF
#IF     defined( __FB_ANDROID__)
 #DEFINE _PSTR " Android"
 #DEFINE __UNIX__
#ELSEIF defined( __FB_LINUX__)
 #DEFINE _PSTR " Linux"
 #DEFINE __UNIX__
#ELSEIF defined(__FB_FREEBSD__)
 #DEFINE _PSTR " FreeBSD"
 #DEFINE __UNIX__
#ELSEIF defined(__FB_NETBSD__)
 #DEFINE _PSTR " NetBSD"
 #DEFINE __UNIX__
#ELSEIF defined(__FB_OPENBSD__)
 #DEFINE _PSTR " OpenBSD"
 #DEFINE __UNIX__
#ELSEIF defined(__FB_DARWIN__)
 #DEFINE _PSTR " Mac OS X/Darwin"
 #DEFINE __UNIX__
#ELSEIF defined(__FB_WIN32__)
 #DEFINE _PSTR " Win32"
#ELSEIF defined(__FB_DOS__)
 #DEFINE _PSTR " DOS"
#ELSE
 #DEFINE _PSTR " Unknown Platform"
#ENDIF
#IFDEF __FB_64BIT__
 #DEFINE _BSTR " 64-bit"
#ELSE
 #DEFINE _BSTR " 32-bit"
#ENDIF
#IFDEF SCRIPTPROFILE
 #DEFINE _SSTR " script_profiling"
#ELSE
 #DEFINE _SSTR
#ENDIF
CONST build_info as string = "" _GSTR _ESTR _GENSTR _SSTR _PSTR _BSTR


'==================================== OS-specific defines =================================

#IFDEF __FB_ANDROID__
 #DEFINE LOWMEM
#ENDIF

'__FB_UNIX__ is only in FB 0.21 onwards (I think)
'In FB 0.90+ it's either defined or not (can't be used in #IF), while in earlier FBs
'it's always defined, either to 0 or -1 (can't be used in #IFDEF). HATEHATEHATE
#IFNDEF __UNIX__
 #IF __FB_VERSION__ < "0.90"
  #IF __FB_UNIX__
   'STRANGE: For some reason I have to add a comment here or FreeBasic 0.24-pre doesn't compile it
   #DEFINE __UNIX__
  #ENDIF
 #ELSE
  #IFDEF __FB_UNIX__
   #DEFINE __UNIX__
  #ENDIF
 #ENDIF
#ENDIF

#IFDEF __UNIX__
 'FB's headers check for __FB_LINUX__
 '(because they are actually headers from some GNU/Linux distrib, other platforms not officially supported)
 #DEFINE __FB_LINUX__
#ENDIF

#IFDEF __UNIX__
 #IFNDEF __FB_DARWIN__
  #IFNDEF __FB_ANDROID__
   #DEFINE __X11__
  #ENDIF
 #ENDIF
#ENDIF

#ifdef __UNIX__
#define SLASH "/"
#define ispathsep(character) (character = ASC("/"))
#define LINE_END !"\n"
#define CUSTOMEXE "ohrrpgce-custom"
#define GAMEEXE "ohrrpgce-game"
#define DOTEXE ""
#define ALLFILES "*"
#else
#define SLASH "\"
#define ispathsep(character) (character = ASC("/") OR character = ASC("\"))
#define LINE_END !"\r\n"
#define CUSTOMEXE "custom.exe"
#define GAMEEXE "game.exe"
#define DOTEXE ".exe"
#define ALLFILES "*.*"
#endif

'---For some crazy reason TRUE and FALSE don't work well as const even though they are not reserved
CONST YES = -1
CONST NO = 0

#IFNDEF NULL
#DEFINE NULL 0
#ENDIF


'================================= 32/64 bit differences ==================================


' We put a few declarations in a namespace so that they aren't lost after including
' windows.bi and #undefing. If more include_windows_bi() problems occur we can get
' around them by moving more stuff into this namespace.
NAMESPACE OHR

' TODO: FB 1.04+ has a boolean type, which we ignore for now
' (it's 1 bit in size and compatible with C/C++ bool)
#IFDEF __FB_64BIT__
  TYPE bool as long  '32 bit
#ELSE
  'Tip: Change this to 'long' to cause warnings for inconsistent usage of bool vs integer
  TYPE bool as integer
#ENDIF

' I will use boolint in declarations of C/C++ functions where we would like to use
' bool (C/C++) or boolean (FB), but shouldn't, to support FB pre-1.04. So instead,
' use boolint on both sides, to show intention but prevent accidental C/C++ bool usage.
TYPE boolint as long  '32 bit

'Even though long and integer are the same size on 32 bit platforms,
'fbc considers them different types and throws warnings!
'This is because they get mangled to C long and int types respectively.
'Likewise, integer and longint could be different on 64 bit. See crt/long.bi.
#IFDEF __FB_64BIT__
  #IFNDEF int32
    TYPE int32 as long
  #ENDIF
  #IFNDEF uint32
    TYPE uint32 as ulong
  #ENDIF
  TYPE int64 as integer
  TYPE uint64 as uinteger
  #IFNDEF ssize_t
    TYPE ssize_t as integer
  #ENDIF
#ELSE
  #IFNDEF int32
    TYPE int32 as integer
  #ENDIF
  #IFNDEF uint32
    TYPE uint32 as uinteger
  #ENDIF
  TYPE int64 as longint
  TYPE uint64 as ulongint
  #IFNDEF ssize_t
    TYPE ssize_t as integer
  #ENDIF
#ENDIF

END NAMESPACE

USING OHR

TYPE fb_integer as integer
TYPE fb_uinteger as uinteger

' Use of the following two macros may be needed when including
' certain external headers. Most FB headers have no or almost no
' instances of 'integer'. Strangely there are a few random occurrences.
' To be safe, put 'use_native_integer' before and 'use_32bit_integer'
' after an 'unclean' include.

#MACRO use_native_integer()
# IFDEF __FB_64BIT__
#  UNDEF integer
#  UNDEF uinteger
   TYPE integer as fb_integer
   TYPE uinteger as fb_uinteger
# ENDIF
#ENDMACRO

#MACRO use_32bit_integer()
# IFDEF __FB_64BIT__
#  UNDEF integer
#  UNDEF uinteger
   TYPE integer as int32
   TYPE uinteger as uint32
# ENDIF
#ENDMACRO

'included only for $inclib?
#include once "crt.bi"
#include once "crt/limits.bi"
#undef rand
#undef bound
'Need to include these before redefining the size of 'integer'
#include "crt/stddef.bi"
#include "crt/sys/types.bi"
#ifndef intptr_t
 ' Old FB headers
 TYPE intptr_t as size_t
#endif

use_32bit_integer()


'======================================== windows.bi ======================================

' include_windows_bi() MUST be used after config.bi is included but before anything else!
#macro include_windows_bi()
# ifndef windows_bi_included
#  define windows_bi_included
#  define _X86_
   use_native_integer()
#  include once "windows.bi"
' Almost everywhere, the following two headers are enough
' #  include once "win/windef.bi"
' #  include once "win/winbase.bi"
' ' The following two .bi's are in order to undef iswindow so can include SDL.bi, which includes windows.bi
' #  include once "win/wingdi.bi"
' #  include once "win/winuser.bi"
   use_32bit_integer()
#  undef max
#  undef min
#  undef default_palette
#  undef sound_playing
#  undef copyfile
#  undef istag
#  undef ignore
#  undef iswindow
#  undef rectangle
#  undef ellipse
#  undef color_menu
#  undef openfile
   'Needed in music_native2.bas
   type MSG_ as MSG
   const TRANSPARENT_ = TRANSPARENT
#  undef msg
#  undef this
#  undef font
#  undef opaque
#  undef transparent
#  undef bool
# endif
#endmacro


'==================================== TIMER_START/STOP ====================================

'Warning: you may not nest TIMER_STOP/START calls!

'under windows, TIMER uses QueryPerformanceCounter, under unix it uses gettimeofday
#ifdef ACCURATETIMER
 'use a timer which counts CPU time spent by this process (preferably thread) only
 #ifdef __FB_WIN32__
  'only available on win 2000 or later
  include_windows_bi()
  #if defined(GetThreadTimes)
   #define timer_variables  as FILETIME ptr atimer_s, atimer_e, atimer_temp
   extern timer_variables
   #define READ_TIMER(a)  GetThreadTimes(GetCurrentThread, NULL, NULL, NULL, @atimer_temp): a = atimer_temp.dwLowDateTime * 0.0000001
   #define TIMER_START(a)  GetThreadTimes(GetCurrentThread, NULL, NULL, NULL, @atimer_s)
   #define TIMER_STOP(a)  GetThreadTimes(GetCurrentThread, NULL, NULL, NULL, @atimer_e): a += (atimer_e.dwLowDateTime - atimer_s.dwLowDateTime) * 0.0000001
  #else
   #print GetThreadTimes not available; don't define ACCURATETIMER
  #endif
 #else
  'assume anything else is a unix
  'options: clock, times, clock_gettime (with CLOCK_THREAD_CPUTIME_ID) which apparently counts in clock ticks (1ms)
  #define timer_variables as timespec atimer_s, atimer_e, atimer_temp
  extern timer_variables
  #define READ_TIMER(a)  clock_gettime(CLOCK_THREAD_CPUTIME_ID, @atimer_temp): a = atimer_temp.tv_nsec * 0.000000001
  #define TIMER_START(a)  clock_gettime(CLOCK_THREAD_CPUTIME_ID, @atimer_s)
  #define TIMER_STOP(a)  clock_gettime(CLOCK_THREAD_CPUTIME_ID, @atimer_e): a += (atimer_e.tv_nsec - atimer_s.tv_nsec) * 0.000000001
 #endif
#endif
#ifndef TIMER_START
 #define READ_TIMER(a)   a = TIMER
 #define TIMER_START(a) a -= TIMER
 #define TIMER_STOP(a)  a += TIMER
#endif


'====================================== GOSUB Hack ========================================

#if __FB_GCC__ = 0
'use nearly-as-fast assembly version (one extra jump)

#undef gosub
#define gosub _gosub_beta(__LINE__,__FUNCTION_NQ__)
'The "if 0 then" is used to place a label after the goto (the goto label occurs afterwards)
'We subtract 12 from esp to perserve 16-byte stack alignment, in case it is required
'(eg Linux i386/x86_64)
#macro _gosub_beta(a,b)
  asm
    sub esp, 12
    call gosub_##b##_line_##a
    add esp, 12
  end asm
  if 0 then asm : gosub_##b##_line_##a: end asm : goto
#endmacro
#define retrace asm ret
#define crt_jmp_buf byte

#else  'choose GOSUB workaround

'alternative to above blocks, use this code on non x86 platforms
'use a setjmp/longjmp kludge

'#include "crt/setjmp.bi"
' setjmp.bi is incorrect. Actual size is 148 bytes on 64 bit OSX, so be conservative
type crt_jmp_buf:dummy(255) as byte:end type
#ifdef __FB_WIN32__
declare function setjmp cdecl alias "_setjmp" (byval as any ptr) as integer
#else
declare function setjmp cdecl alias "setjmp" (byval as any ptr) as integer
#endif
declare sub longjmp cdecl alias "longjmp" (byval as any ptr, byval as integer)

extern gosubbuf(31) as crt_jmp_buf
extern gosubptr as integer
'option nokeyword gosub
#undef gosub
#define gosub if setjmp(@gosubbuf(gosubptr)) then gosubptr-=1 else gosubptr+=1:goto
#define retrace longjmp(@gosubbuf(gosubptr-1),1)

#endif  'choose GOSUB workaround

#ENDIF
