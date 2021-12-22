'OHRRPGCE - Misc functions written in C
'(C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
'Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#ifndef MISCC_BI
#define MISCC_BI

extern "C"

' For size_t
#include "crt/stddef.bi"

declare function get_sys_err_string() as zstring ptr

declare function stringhash(byval strp as zstring ptr, byval leng as integer) as unsigned integer

declare sub strip_carriage_returns(byval text as zstring ptr)

declare function strprintf (byval fmtstr as zstring ptr, ...) as string

declare sub init_crt ()

declare sub disable_extended_precision ()

declare function hook_fb_End() as boolint
declare sub unhook_fb_End()

end extern

#endif
