/* OHRRPGCE - Logging and error reporting functions
 * (C) Copyright 1997-2021 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
 * Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
 */

#ifndef ERRORLOG_H
#define ERRORLOG_H

#include "config.h"
#include "miscc.h"

#ifdef __cplusplus
extern "C" {
#endif

// Constants for debugc mirrored from const.bi
// NOTE: const.bi MUST be updated when this is changed!
enum ErrorLevel {
	errInfo = 1,   //Informational spam (doesn't cause g/c_debug.txt to be kept)
	errShowInfo,   //Show and call debuginfo
	errDebug,      //Log a message and preserve g/c_debug.txt
	errShowDebug,  //Show and call debug. Minor error, but want to keep user well informed
	errError,      //Something is wrong, but it's not necessarily due to a bug. Only log it.
	errBug,        //Engine bug detected; log but don't interrupt the program. Usually would use errShowBug instead.
	errShowError,  //Something is wrong and continuing might be dubious, but it's not necessarily due to a bug.
	               //Show error and possibly prompt the user whether they want to quit (doesn't return) or continue
	errShowBug,    //As above, but indicates that it's an engine bug
	errFatalError, //Does not return!
	errFatalBug,   //Does not return!
	errDie,        //Exit immediately without attempting to show a message (especially for gfx backend errors)
};


//// in util.bas

void fb_error_hook(const char *message, boolint interrupt_signal);

// Escape a filename for use in a shell in a way suitable for this OS.
// Returns a malloc'd string buffer
char *escape_filenamec (const char *filename);

extern double program_start_timer;

//// in common_base.bas / common.rbas

void early_debuginfo(const char *msg);
void onetime_debug(enum ErrorLevel errorlevel, const char *msg);
void debugc_internal(void *callsite, enum ErrorLevel errorlevel, const char *msg);
void showerror_internal(void *callsite, const char *msg, boolint isfatal, boolint isbug);

extern char *app_name;
extern char *app_log_filename;
extern char *app_archive_filename;

//// in miscc.c (NOTE: debugc, _throw_error also defined separately in gfx_directx.cpp)

void debugc(enum ErrorLevel errorlevel, const char *msg);
void showbug(const char *msg);
void showerror(const char *msg, boolint isfatal, boolint isbug);
void _throw_error(enum ErrorLevel errorlevel, const char *srcfile, int linenum, const char *msg, ...) format_chk(4);
extern void (*debug_hook)(enum ErrorLevel errorlevel, const char *msg);
void set_debug_hook(void (*new_debug_hook)(enum ErrorLevel errorlevel, const char *msg));

#define debug(errorlevel, ...) _throw_error(errorlevel, NULL, 0, __VA_ARGS__)
#define debuginfo(...) _throw_error(errInfo, NULL, 0, __VA_ARGS__)
#define throw_error(...) _throw_error(errFatalBug, __FILE__, __LINE__, __VA_ARGS__)
#define fatal_error(...) _throw_error(errFatalError, __FILE__, __LINE__, __VA_ARGS__)

//// in libfb.a

void _noreturn (*fb_ErrorThrowAt(int line_num, const char *mod_name, void *res_label, void *resnext_label))(void);

#ifdef __cplusplus
}
#endif

#endif
