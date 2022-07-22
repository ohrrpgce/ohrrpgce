/**
 * This file has no copyright assigned and is placed in the Public Domain.
 * This file is part of the mingw-w64 runtime package.
 * No warranty is given.
 *
 * KLUDGE: this file is from mingw-w64-crt 7.0.0.  It is a temporary fix for
 * mingw-w64 6.0.0 (currently used on the official build machine) not supporting
 * Win 95-2000 (bug #1241) though it seems to compile without problem in later
 * mingw-w64 versions. See
 * https://github.com/ohrrpgce/ohrrpgce/issues/1241
 * https://github.com/mirror/mingw-w64/commit/ebf4bf9d2a9
 */

#include <_mingw.h>

extern int* __MINGW_IMP_SYMBOL(__mb_cur_max);

int __cdecl ___mb_cur_max_func(void);
int __cdecl ___mb_cur_max_func(void)
{
    return *__MINGW_IMP_SYMBOL(__mb_cur_max);
}

typedef int __cdecl (*_f___mb_cur_max_func)(void);
_f___mb_cur_max_func __MINGW_IMP_SYMBOL(___mb_cur_max_func) = ___mb_cur_max_func;
