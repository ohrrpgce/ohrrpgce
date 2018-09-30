'OHRRPGCE - Some Custom/Game common code
'
' This header can be included by Game, Custom, or any utility,
' and contains functions which have two implementations: in
' common.rbas (for Game and Custom, and in common_base.bas (all else).

#ifndef COMMON_BASE_BI
#define COMMON_BASE_BI

#include "config.bi"
#include "const.bi"

declare sub debug (msg as zstring ptr)
declare sub early_debuginfo (msg as zstring ptr)
declare sub debuginfo (msg as zstring ptr)
declare sub fatalerror (msg as zstring ptr)
declare sub showerror (msg as zstring ptr, isfatal as bool = NO, isbug as bool = NO)
declare sub visible_debug (msg as zstring ptr, errlvl as errorLevelEnum = errDebug)
declare sub debugc cdecl alias "debugc" (errorlevel as errorLevelEnum, msg as zstring ptr)

'Called by fatalerror
extern cleanup_function as sub ()

'Global variables
EXTERN workingdir as string

#endif
