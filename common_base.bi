'OHRRPGCE - Some Custom/Game common code
'
' This header can be included by Game, Custom, or any utility,
' and contains functions which have two implementations: in
' common.rbas (for Game and Custom, and in common_base.bas (all else).

#ifndef COMMON_BASE_BI
#define COMMON_BASE_BI

#include "config.bi"
#include "const.bi"

declare sub debug (msg as const zstring ptr)
declare sub early_debuginfo (msg as const zstring ptr)
declare sub debuginfo (msg as const zstring ptr)
declare sub debugerror (msg as const zstring ptr)
declare sub fatalerror (msg as const zstring ptr)
declare sub fatalbug (msg as const zstring ptr)
declare sub visible_debug (msg as const zstring ptr)

extern "C"
declare sub onetime_debug (errorlevel as errorLevelEnum = errDebug, msg as const zstring ptr)

declare sub showerror_internal (callsite as any ptr, msg as const zstring ptr, isfatal as bool = NO, isbug as bool = NO)
declare sub debugc_internal (callsite as any ptr, errorlevel as errorLevelEnum, msg as const zstring ptr)

'In miscc.c
declare sub showbug (msg as const zstring ptr)
declare sub showerror (msg as const zstring ptr, isfatal as bool = NO, isbug as bool = NO)
declare sub debugc (errorlevel as errorLevelEnum, msg as const zstring ptr)
end extern

'Called by fatalerror
extern cleanup_function as sub ()

'Global variables
EXTERN workingdir as string
EXTERN app_name as zstring ptr
EXTERN app_log_filename as zstring ptr
EXTERN app_archive_filename as zstring ptr

#endif
