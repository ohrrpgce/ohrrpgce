//TESTAPP determines whether the test app is running (TRUE/FALSE)
#ifndef TESTAPP
#define TESTAPP 0

//included libraries for TESTAPP
#if TESTAPP
#include <fstream>
#include <tchar.h>
#include <string>
#endif

//resource loading from module
#if TESTAPP
#define MODULENAME NULL
#else
#define MODULENAME TEXT("gfx_directx.dll")
#define ISOLATION_AWARE_ENABLED  1
#endif //TESTAPP

//used in testing hresult's
#define TEST_HR(hresult, errorCode)  if(S_OK != hresult) return Report(errorCode);

//used in logging data
#if TESTAPP
#define TEST_LOG_DECLARE(logVarName)  std::ofstream logVarName
#define TEST_LOG_INIT(logVarName, strFile)  \
	logVarName.open(strFile, std::ios::out | std::ios::app); \
	TCHAR tmpBuffer_logInit_1001[32]; \
	GetTimeFormat(LOCALE_SYSTEM_DEFAULT, 0, 0, 0, tmpBuffer_logInit_1001, 32); \
	logVarName << tmpBuffer_logInit_1001
#define TEST_LOG(logVarName, strCommand, hResult, strMessage)  \
	logVarName << TEXT(strCommand) << TEXT(": ") << (int)hResult \
	<< TEXT("\r\n\t") << TEXT(strMessage) << TEXT("\r\n\r\n")
#define TEST_LOG_CLOSE(logVarName)  logVarName.close()
#define TEST_ONLY_BLOCK(codeBlock) codeBlock
#else
#define TEST_LOG_DECLARE(logVarName) logVarName
#define TEST_LOG_INIT(logVarName, strFile) logVarName; strFile
#define TEST_LOG(logVarName, strCommand, hResult, strMessage) logVarName; strCommand; hResult; strMessage
#define TEST_LOG_CLOSE(logVarName) logVarName
#define TEST_ONLY_BLOCK(codeBlock)
#endif //logging data

#endif //TESTAPP

