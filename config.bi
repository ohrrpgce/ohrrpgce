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

' We put a few declarations in a namespace so that they aren't lost after including
' windows.bi and #undefing. If more include_windows_bi() problems occur we can get
' around them by moving more stuff into this namespace.
NAMESPACE OHR

#IFNDEF NULL
#DEFINE NULL 0
#ENDIF

' TODO: FB 1.04+ has a boolean type, which we ignore for now
' (it's 1 bit in size and compatible with C/C++ bool)
#IFDEF __FB_64BIT__
  TYPE bool as long  '32 bit
#ELSE
  'Tip: Change this to 'long' to warnings for inconsistent usage of bool vs integer
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


'---For some crazy reason TRUE and FALSE don't work well as const even though they are not reserved
CONST YES = -1
CONST NO = 0

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
# UNDEF integer
# UNDEF uinteger
  TYPE integer as fb_integer
  TYPE uinteger as fb_uinteger
#ENDMACRO

#MACRO use_32bit_integer()
# UNDEF integer
# UNDEF uinteger
  TYPE integer as int32
  TYPE uinteger as uint32
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

#macro include_windows_bi()
# ifndef windows_bi_included
#  define windows_bi_included
#  undef point
#  undef copyfile
#  undef iswindow
#  undef rectangle
#  undef ellipse
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

'''''''''''GOSUB hack

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
