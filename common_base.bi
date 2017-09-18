'OHRRPGCE - Some Custom/Game common code
'
' This header can be included by Game, Custom, or any utility,
' and contains functions which have two implementations: in
' common.rbas (for Game and Custom, and in common_base.bas (all else).

#ifndef COMMON_BASE_BI
#define COMMON_BASE_BI

#include "config.bi"
#include "const.bi"

declare sub debug (s as string)
declare sub debuginfo (s as string)
declare sub fatalerror (s as string)
declare sub showerror (msg as string, byval isfatal as bool = 0)
declare sub visible_debug (s as string, errlvl as errorLevelEnum = errDebug)
declare sub debugc cdecl alias "debugc" (byval errorlevel as errorLevelEnum, byval s as zstring ptr)

'Called by fatalerror
extern cleanup_function as sub ()

'Global variables
EXTERN workingdir as string

#endif