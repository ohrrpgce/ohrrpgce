'OHRRPGCE Common - Configuration/platform specific/important macros
'This file is (should be) included everywhere, and is a dumping ground for macros and other global declarations

#IFNDEF CONFIG_BI
#DEFINE CONFIG_BI

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

EXTERN wantpollingthread as integer
EXTERN as string gfxbackend, musicbackend
EXTERN as string gfxbackendinfo, musicbackendinfo, systeminfo

#undef getkey

'included only for $inclib?
#include once "crt.bi"
#include once "crt/limits.bi"
#undef rand
#undef abort
#undef bound

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
#  undef ignore
# endif
#endmacro

'GOSUB hack

#if __FB_GCC__ = 0
'use nearly-as-fast assembly version (one extra jump)

#undef gosub
#define gosub _gosub_beta(__LINE__,__FUNCTION_NQ__)
'the "if 0 then" is used to place a label after the goto
#define _gosub_beta(a,b) asm : call gosub_##b##_line_##a end asm : if 0 then asm : gosub_##b##_line_##a: end asm : goto
#define retrace asm ret
#define crt_jmp_buf byte

#else  'choose GOSUB workaround

#ifdef __FB_DARWIN__
 #error "setjmp GOSUB hack not supported on Mac"
#endif

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

'#DEFINE CLEAROBJ(OBJ) memset(@(OBJ),0,LEN(OBJ))
'#DEFINE COPYOBJ(TO,FROM) memcpy(@(TO),@(FROM),LEN(FROM))

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

#ENDIF
