#pragma once

'Translated from the original with:
' fbfrog -define CALLBACK __stdcall -define WINAPI __stdcall -target windows include/CrashRpt.h
'with manual TODO fixes.
'See the original header for the documentation.

#include once "windows.bi"
#include once "dbghelp.bi"

extern "Windows"

#define _CRASHRPT_H_
#define __reserved
#define __in
#define __in_opt
#define __out_ecount_z(x)
#define CRASHRPT_EXTERNC
#define CRASHRPTAPI(rettype) CRASHRPT_EXTERNC rettype WINAPI
const CRASHRPT_VER = 1403
type LPGETLOGFILE as function(byval lpvState as LPVOID) as BOOL
const CR_SEH_EXCEPTION = 0
const CR_CPP_TERMINATE_CALL = 1
const CR_CPP_UNEXPECTED_CALL = 2
const CR_CPP_PURE_CALL = 3
const CR_CPP_NEW_OPERATOR_ERROR = 4
const CR_CPP_SECURITY_ERROR = 5
const CR_CPP_INVALID_PARAMETER = 6
const CR_CPP_SIGABRT = 7
const CR_CPP_SIGFPE = 8
const CR_CPP_SIGILL = 9
const CR_CPP_SIGINT = 10
const CR_CPP_SIGSEGV = 11
const CR_CPP_SIGTERM = 12

type tagCR_EXCEPTION_INFO
	cb as WORD
	pexcptrs as PEXCEPTION_POINTERS
	exctype as long
	code as DWORD
	fpe_subcode as ulong
	expression as const wstring ptr
	function as const wstring ptr
	file as const wstring ptr
	line as ulong
	bManual as BOOL
	hSenderProcess as HANDLE
end type

type CR_EXCEPTION_INFO as tagCR_EXCEPTION_INFO
type PCR_EXCEPTION_INFO as CR_EXCEPTION_INFO ptr
const CR_CB_STAGE_PREPARE = 10
const CR_CB_STAGE_FINISH = 20

type tagCR_CRASH_CALLBACK_INFOW
	cb as WORD
	nStage as long
	pszErrorReportFolder as LPCWSTR
	pExceptionInfo as CR_EXCEPTION_INFO ptr
	pUserParam as LPVOID
	bContinueExecution as BOOL
end type

type CR_CRASH_CALLBACK_INFOW as tagCR_CRASH_CALLBACK_INFOW

type tagCR_CRASH_CALLBACK_INFOA
	cb as WORD
	nStage as long
	pszErrorReportFolder as LPCSTR
	pExceptionInfo as CR_EXCEPTION_INFO ptr
	pUserParam as LPVOID
	bContinueExecution as BOOL
end type

type CR_CRASH_CALLBACK_INFOA as tagCR_CRASH_CALLBACK_INFOA
type CR_CRASH_CALLBACK_INFO as CR_CRASH_CALLBACK_INFOA
const CR_CB_CANCEL = 0
const CR_CB_DODEFAULT = 1
const CR_CB_NOTIFY_NEXT_STAGE = 2

type PFNCRASHCALLBACKW as function(byval pInfo as CR_CRASH_CALLBACK_INFOW ptr) as long
type PFNCRASHCALLBACKA as function(byval pInfo as CR_CRASH_CALLBACK_INFOA ptr) as long
type PFNCRASHCALLBACK as PFNCRASHCALLBACKA

declare function crSetCrashCallbackW(byval pfnCallbackFunc as PFNCRASHCALLBACKW, byval lpParam as LPVOID) as long
declare function crSetCrashCallbackA(byval pfnCallbackFunc as PFNCRASHCALLBACKA, byval lpParam as LPVOID) as long
declare function crSetCrashCallback alias "crSetCrashCallbackA"(byval pfnCallbackFunc as PFNCRASHCALLBACKA, byval lpParam as LPVOID) as long

const CR_HTTP = 0
const CR_SMTP = 1
const CR_SMAPI = 2
#define CR_NEGATIVE_PRIORITY (UINT - 1)
const CR_INST_STRUCTURED_EXCEPTION_HANDLER = &h1
const CR_INST_SEH_EXCEPTION_HANDLER = &h1
const CR_INST_TERMINATE_HANDLER = &h2
const CR_INST_UNEXPECTED_HANDLER = &h4
const CR_INST_PURE_CALL_HANDLER = &h8
const CR_INST_NEW_OPERATOR_ERROR_HANDLER = &h10
const CR_INST_SECURITY_ERROR_HANDLER = &h20
const CR_INST_INVALID_PARAMETER_HANDLER = &h40
const CR_INST_SIGABRT_HANDLER = &h80
const CR_INST_SIGFPE_HANDLER = &h100
const CR_INST_SIGILL_HANDLER = &h200
const CR_INST_SIGINT_HANDLER = &h400
const CR_INST_SIGSEGV_HANDLER = &h800
const CR_INST_SIGTERM_HANDLER = &h1000
const CR_INST_ALL_POSSIBLE_HANDLERS = &h1FFF
const CR_INST_CRT_EXCEPTION_HANDLERS = &h1FFE
const CR_INST_NO_GUI = &h2000
const CR_INST_HTTP_BINARY_ENCODING = &h4000
const CR_INST_DONT_SEND_REPORT = &h8000
const CR_INST_APP_RESTART = &h10000
const CR_INST_NO_MINIDUMP = &h20000
const CR_INST_SEND_QUEUED_REPORTS = &h40000
const CR_INST_STORE_ZIP_ARCHIVES = &h80000
const CR_INST_SEND_MANDATORY = &h100000
const CR_INST_SHOW_ADDITIONAL_INFO_FIELDS = &h200000
const CR_INST_ALLOW_ATTACH_MORE_FILES = &h400000
const CR_INST_AUTO_THREAD_HANDLERS = &h800000

type tagCR_INSTALL_INFOW
	cb as WORD
	pszAppName as LPCWSTR
	pszAppVersion as LPCWSTR
	pszEmailTo as LPCWSTR
	pszEmailSubject as LPCWSTR
	pszUrl as LPCWSTR
	pszCrashSenderPath as LPCWSTR
	pfnCrashCallback as LPGETLOGFILE
	uPriorities(0 to 4) as UINT
	dwFlags as DWORD
	pszPrivacyPolicyURL as LPCWSTR
	pszDebugHelpDLL as LPCWSTR
	uMiniDumpType as MINIDUMP_TYPE
	pszErrorReportSaveDir as LPCWSTR
	pszRestartCmdLine as LPCWSTR
	pszLangFilePath as LPCWSTR
	pszEmailText as LPCWSTR
	pszSmtpProxy as LPCWSTR
	pszCustomSenderIcon as LPCWSTR
	pszSmtpLogin as LPCWSTR
	pszSmtpPassword as LPCWSTR
	nRestartTimeout as long
end type

type CR_INSTALL_INFOW as tagCR_INSTALL_INFOW
type PCR_INSTALL_INFOW as CR_INSTALL_INFOW ptr

type tagCR_INSTALL_INFOA
	cb as WORD
	pszAppName as LPCSTR
	pszAppVersion as LPCSTR
	pszEmailTo as LPCSTR
	pszEmailSubject as LPCSTR
	pszUrl as LPCSTR
	pszCrashSenderPath as LPCSTR
	pfnCrashCallback as LPGETLOGFILE
	uPriorities(0 to 4) as UINT
	dwFlags as DWORD
	pszPrivacyPolicyURL as LPCSTR
	pszDebugHelpDLL as LPCSTR
	uMiniDumpType as MINIDUMP_TYPE
	pszErrorReportSaveDir as LPCSTR
	pszRestartCmdLine as LPCSTR
	pszLangFilePath as LPCSTR
	pszEmailText as LPCSTR
	pszSmtpProxy as LPCSTR
	pszCustomSenderIcon as LPCSTR
	pszSmtpLogin as LPCSTR
	pszSmtpPassword as LPCSTR
	nRestartTimeout as long
end type

type CR_INSTALL_INFOA as tagCR_INSTALL_INFOA
type PCR_INSTALL_INFOA as CR_INSTALL_INFOA ptr
type CR_INSTALL_INFO as CR_INSTALL_INFOA
type PCR_INSTALL_INFO as PCR_INSTALL_INFOA

declare function crInstallW(byval pInfo as PCR_INSTALL_INFOW) as long
declare function crInstallA(byval pInfo as PCR_INSTALL_INFOA) as long
declare function crInstall alias "crInstallA"(byval pInfo as PCR_INSTALL_INFOA) as long
declare function crUninstall() as long
declare function crInstallToCurrentThread2(byval dwFlags as DWORD) as long
declare function crUninstallFromCurrentThread() as long

const CR_AF_TAKE_ORIGINAL_FILE = 0
const CR_AF_MAKE_FILE_COPY = 1
const CR_AF_FILE_MUST_EXIST = 0
const CR_AF_MISSING_FILE_OK = 2
const CR_AF_ALLOW_DELETE = 4

declare function crAddFile2W(byval pszFile as LPCWSTR, byval pszDestFile as LPCWSTR, byval pszDesc as LPCWSTR, byval dwFlags as DWORD) as long
declare function crAddFile2A(byval pszFile as LPCSTR, byval pszDestFile as LPCSTR, byval pszDesc as LPCSTR, byval dwFlags as DWORD) as long
declare function crAddFile2 alias "crAddFile2A"(byval pszFile as LPCSTR, byval pszDestFile as LPCSTR, byval pszDesc as LPCSTR, byval dwFlags as DWORD) as long

const CR_AS_VIRTUAL_SCREEN = 0
const CR_AS_MAIN_WINDOW = 1
const CR_AS_PROCESS_WINDOWS = 2
const CR_AS_GRAYSCALE_IMAGE = 4
const CR_AS_USE_JPEG_FORMAT = 8
const CR_AS_ALLOW_DELETE = 16
declare function crAddScreenshot(byval dwFlags as DWORD) as long
declare function crAddScreenshot2(byval dwFlags as DWORD, byval nJpegQuality as long) as long
const CR_AV_VIRTUAL_SCREEN = 0
const CR_AV_MAIN_WINDOW = 1
const CR_AV_PROCESS_WINDOWS = 2
const CR_AV_QUALITY_LOW = 0
const CR_AV_QUALITY_GOOD = 4
const CR_AV_QUALITY_BEST = 8
const CR_AV_NO_GUI = 16
const CR_AV_ALLOW_DELETE = 32

declare function crAddVideo(byval dwFlags as DWORD, byval nDuration as long, byval nFrameInterval as long, byval pDesiredFrameSize as PSIZE, byval hWndParent as HWND) as long
declare function crAddPropertyW(byval pszPropName as LPCWSTR, byval pszPropValue as LPCWSTR) as long
declare function crAddPropertyA(byval pszPropName as LPCSTR, byval pszPropValue as LPCSTR) as long
declare function crAddProperty alias "crAddPropertyA"(byval pszPropName as LPCSTR, byval pszPropValue as LPCSTR) as long
const CR_AR_ALLOW_DELETE = &h1
declare function crAddRegKeyW(byval pszRegKey as LPCWSTR, byval pszDstFileName as LPCWSTR, byval dwFlags as DWORD) as long
declare function crAddRegKeyA(byval pszRegKey as LPCSTR, byval pszDstFileName as LPCSTR, byval dwFlags as DWORD) as long
declare function crAddRegKey alias "crAddRegKeyA"(byval pszRegKey as LPCSTR, byval pszDstFileName as LPCSTR, byval dwFlags as DWORD) as long
declare function crGenerateErrorReport(byval pExceptionInfo as CR_EXCEPTION_INFO ptr) as long
declare function crExceptionFilter(byval code as ulong, byval ep as _EXCEPTION_POINTERS ptr) as long

const CR_NONCONTINUABLE_EXCEPTION = 32
const CR_THROW = 33
const CR_STACK_OVERFLOW = 34
declare function crEmulateCrash(byval ExceptionType as ulong) as long

declare function crGetLastErrorMsgW(byval pszBuffer as LPWSTR, byval uBuffSize as UINT) as long
declare function crGetLastErrorMsgA(byval pszBuffer as LPSTR, byval uBuffSize as UINT) as long
declare function crGetLastErrorMsg alias "crGetLastErrorMsgA"(byval pszBuffer as LPSTR, byval uBuffSize as UINT) as long

end extern
