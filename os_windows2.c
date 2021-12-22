/* OHRRPGCE - Windows-specific routines which require C implementations
 * (C) Copyright 1997-2020 James Paige, Ralph Versteegen, and the OHRRPGCE Developers
 * Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
 *
 * Most low-level Windows code is in os_windows.bas, but some is easier to write in C.
 */

//fb_stub.h MUST be included first, to ensure fb_off_t is 64 bit
#include "fb/fb_stub.h"

#include <windows.h>
#include <inttypes.h>  // for PRId64

#include "os.h"
#include "errorlog.h"
#include "lumpfile.h"

#define _CRASHRPT_NO_WRAPPERS  //Exclude C++ wrapper classes
#include "CrashRpt.h"  // win32/include/CrashRpt.h

FBCALL double fb_Timer();

struct {
	CRASHRPTAPI(int) (*crInstallA)(__in PCR_INSTALL_INFOA pInfo);
	CRASHRPTAPI(int) (*crAddFile2A)(LPCSTR pszFile, LPCSTR pszDestFile, LPCSTR pszDesc, DWORD dwFlags);
	CRASHRPTAPI(int) (*crAddPropertyA)(LPCSTR pszPropName, LPCSTR pszPropValue);
	CRASHRPTAPI(int) (*crAddScreenshot2)(DWORD dwFlags, int nJpegQuality);
	CRASHRPTAPI(int) (*crGetLastErrorMsgA)(LPSTR pszBuffer, UINT uBuffSize);
	CRASHRPTAPI(int) (*crSetCrashCallbackA)(PFNCRASHCALLBACKA pfnCallbackFunc, LPVOID lpParam);
	CRASHRPTAPI(int) (*crGenerateErrorReport)(__in_opt CR_EXCEPTION_INFO* pExceptionInfo);
} crpt;


// (This could have been written in os_windows.bas and there's no special reason it isn't)
void os_get_screen_size(int *wide, int *high) {
	//*wide = *high = 0;
	// This gets the size of the primary monitor
	*wide = GetSystemMetrics(SM_CXSCREEN);
	*high = GetSystemMetrics(SM_CYSCREEN);
	debug(errInfo, "get_screen_size: true screen size %dx%d", *wide, *high);

	// This retrieves the size of the 'work area' on the primary monitor,
	// which is the part of the screen not obscured by taskbar and similar toolbars
	RECT rect;
	if (!SystemParametersInfo(SPI_GETWORKAREA, 0, &rect, 0)) {
		debug(errError, "get_screen_size failed: %s", win_error_str(GetLastError()));
		return;
	}
	*wide = rect.right - rect.left;
	*high = rect.bottom - rect.top;
}

// Returns -1 if can't determine
long long get_filesize(const char *filename) {
	long long ret = -1;

	HANDLE file = CreateFile(filename, GENERIC_READ, FILE_SHARE_READ | FILE_SHARE_WRITE, NULL,
				 OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, NULL);
	if (file && file != INVALID_HANDLE_VALUE) {
		DWORD highword;
		// GetFileSizeEx not available on Win9x :(
		ret = GetFileSize(file, &highword);
		if (ret == 0xffffffff && GetLastError() != NO_ERROR)
			ret = -1;
		else
			ret |= (long long)highword << 32;
		CloseHandle(file);
	}
	return ret;
}

//-----------------------------------------------------------------------------

// Called by crashrpt after a crash has occurred. MUST NOT perform any memory allocation,
// e.g. manipulation of FB strings. winapi calls may be OK (crashrpt has to use some anyway),
// but use sparingly and avoid global state.
// If we get a crash in here, there will be no crash report generated.
// An alternative way to attach data to the report without risking crashing and losing
// the report is to attach a file with CR_AF_TAKE_ORIGINAL_FILE instead of CR_AF_MAKE_FILE_COPY,
// and then write to the file in the second callback, after CrashSender.exe has started.
int CALLBACK crashrpt_callback(CR_CRASH_CALLBACK_INFOA* pInfo) {
	// We could dynamically decide whether to terminate the program here,
	// by setting pInfo->bContinueExecution = TRUE;

	char buf[50], buf2[MAX_PATH + 80];

	// Add current timestamp
	snprintf(buf, 50, "%.7f", fb_Timer() - program_start_timer);
	crpt.crAddPropertyA("Crash time", buf);

	// List, and attach, recently used files
	const char *fname;
	double opentime;
	for (int idx = 0; ; idx++) {
		if (!read_recent_files_list(idx, &fname, &opentime)) break;

		long long fsize = get_filesize(fname);
		const char *note = "";
		if (fsize < 60000) {
			crpt.crAddFile2A(fname, NULL, "Recently opened file", CR_AF_MAKE_FILE_COPY |
					 CR_AF_FILE_MUST_EXIST | CR_AF_ALLOW_DELETE);
		} else {
			note = "omitting=toobig";
		}
		snprintf(buf, 50, "Recent file %d", idx);
		snprintf(buf2, MAX_PATH + 80, "opened=%.7f size=%" PRId64 " %s path=%s", opentime, fsize, note, fname);
		crpt.crAddPropertyA(buf, buf2);
	}

	// Proceed with crash report generation.
	return CR_CB_DODEFAULT;
}

#define lookup_sym(lib, strct, sym) \
	if (!(strct.sym = (void*)GetProcAddress((HINSTANCE)lib, #sym))) { \
		debuginfo("Couldn't load %s: %s", #sym, win_error_str(GetLastError())); \
		FreeLibrary((HINSTANCE)lib); \
		return 0; \
	}

// Returns success
boolint crashrpt_setup(const char *libpath, const char *appname, const char *version, const char *buildstring, const char *branchstring, const char *logfile1, const char *logfile2, boolint add_screenshot) {

	// First, have to find the dll
	void *lib = LoadLibrary(libpath);
	if (!lib) {
		debuginfo("LoadLibrary(%s) failed: %s", libpath, win_error_str(GetLastError()));
		return 0;
	}

	lookup_sym(lib, crpt, crInstallA)
	lookup_sym(lib, crpt, crAddFile2A)
	lookup_sym(lib, crpt, crAddPropertyA)
	lookup_sym(lib, crpt, crAddScreenshot2)
	lookup_sym(lib, crpt, crGetLastErrorMsgA)
	lookup_sym(lib, crpt, crSetCrashCallbackA)
	lookup_sym(lib, crpt, crGenerateErrorReport)

	CR_INSTALL_INFOA info;
	memset(&info, 0, sizeof(CR_INSTALL_INFOA));
	info.cb = sizeof(CR_INSTALL_INFOA);
	info.pszAppName = appname;
	info.pszAppVersion = version;
	info.pszEmailSubject = "Crash Report";
	info.pszEmailTo = "ohrrpgce-crash@HamsterRepublic.com";
	info.pszUrl = "http://rpg.hamsterrepublic.com/crashrpt/";
	info.uPriorities[CR_HTTP] = 3;  // Try first
	info.uPriorities[CR_SMTP] = 0;  // Don't bother, will be blocked by DreamHost, and cause a delay
	info.uPriorities[CR_SMAPI] = 1; // Try last: Simple MAPI using user's mail client
	// Install all available exception handlers
	info.dwFlags |= CR_INST_ALL_POSSIBLE_HANDLERS;
	info.dwFlags |= CR_INST_APP_RESTART;
	// This flag gives users the option to send later, queuing until the next run
	info.dwFlags |= CR_INST_SEND_QUEUED_REPORTS;
	info.dwFlags |= CR_INST_ALLOW_ATTACH_MORE_FILES;
	// Show user email and description fields immediately instead of starting hidden
	//info.dwFlags |= CR_INST_SHOW_ADDITIONAL_INFO_FIELDS;
	info.dwFlags |= CR_INST_AUTO_THREAD_HANDLERS;
	info.pszRestartCmdLine = "";
	info.uMiniDumpType = MiniDumpWithHandleData;

	// Install crash reporting
	if (crpt.crInstallA(&info)) {
		char szErrorMsg[512] = {0};
		crpt.crGetLastErrorMsgA(szErrorMsg, 512);
		debug(errError, "Installing crashrpt failed: %s", szErrorMsg);
		debug(errError, "continue...");
		debug(errError, "...");
		return NO;
	}

	crpt.crAddFile2A(logfile1, NULL, "Log File", CR_AF_MAKE_FILE_COPY | CR_AF_MISSING_FILE_OK | CR_AF_ALLOW_DELETE);
	crpt.crAddFile2A(logfile2, NULL, "Log File", CR_AF_MAKE_FILE_COPY | CR_AF_MISSING_FILE_OK | CR_AF_ALLOW_DELETE);

	if (add_screenshot)
		crpt.crAddScreenshot2(CR_AS_MAIN_WINDOW | CR_AS_ALLOW_DELETE, 0);
	crpt.crAddPropertyA("build", buildstring);
	crpt.crAddPropertyA("branch", branchstring);

	crpt.crSetCrashCallbackA(&crashrpt_callback, NULL);

	return YES;
}

boolint crashrpt_send_report(const char *errmsg) {
	crpt.crAddPropertyA("error", errmsg);
	CR_EXCEPTION_INFO ei;
	memset(&ei, 0, sizeof(CR_EXCEPTION_INFO));
	ei.cb = sizeof(CR_EXCEPTION_INFO);
	ei.exctype = CR_CPP_SIGABRT;
	ei.bManual = TRUE;
	if (crpt.crGenerateErrorReport(&ei)) {
		char szErrorMsg[512] = {0};
		crpt.crGetLastErrorMsgA(szErrorMsg, 512);
		debug(errError, "crGenerateErrorReport failed: %s", szErrorMsg);
		return NO;
	}
	return YES;
}
