'OHRRPGCE Common - Odd header/module left over from the QuickBasic to FreeBASIC move
'FIXME: move this crud elsewhere

#IFNDEF MISC_BI
#DEFINE MISC_BI

#INCLUDE "util.bi"

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
   #print GetThreadTimes not available; don't define ACCURATETIMER
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

'not really a global: just an empty string, since you can't specify zstring ptr default arguments directly
EXTERN nulzstr as zstring ptr

EXTERN overrode_default_zoom as bool
EXTERN overrode_default_fullscreen as bool

#ENDIF
