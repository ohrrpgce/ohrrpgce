/* OHRRPGCE - Misc functions written in C
 * (C) Copyright 1997-2021 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
 * Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
 */

#ifndef MISCC_H
#define MISCC_H

#include "config.h"

#ifdef __cplusplus
extern "C" {
#endif

// Note: error/logging functions in miscc.c are declared instead in errorlog.h

void init_fbstring(FBSTRING *fbstr, const char *cstr);
void init_fbstring_copy(FBSTRING *fbstr, FBSTRING *src);
void set_fbstring(FBSTRING *fbstr, const char *cstr);
FBSTRING *return_fbstring(FBSTRING *fbstr);
FBSTRING *empty_fbstring();
void delete_fbstring(FBSTRING *str);

uint32_t stringhash(const unsigned char *strp, int length);

void strip_carriage_returns(char *text);

FBSTRING *strprintf (const char *fmtstr, ...);

void init_crt();

void disable_extended_precision();

boolint hook_fb_End();
void unhook_fb_End();

#ifdef __cplusplus
}
#endif

#endif
