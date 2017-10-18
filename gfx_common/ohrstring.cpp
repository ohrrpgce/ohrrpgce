#include "../config.h"
#include <windows.h>
#include <string>
#include <cstdlib>
#include "ohrstring.hpp"

using namespace std;

// Codepage for OHR-native string format (currently Latin-1, in future UTF8)
int OHRCodePage = 1252;
//int OHRCodePage = CP_UTF8;

// Convert UTF-16 to the given codepage (1252 for Latin-1, or CP_UTF8, or OHRCodePage)
// The result must be free()'d
char *WstringToMBstring(const wchar_t *wstr, int codepage) {
	int outlen = WideCharToMultiByte(codepage, 0, wstr, -1, NULL, 0, NULL, NULL);
	if (!outlen)
		return (char*)calloc(1, 1);
	char *buf = (char*)malloc(outlen);  //outlen includes the NUL
	WideCharToMultiByte(codepage, 0, wstr, -1, buf, outlen, NULL, NULL);
	return buf;
}

// Convert encoding (1252 for Latin-1, or CP_UTF8, or OHRCodePage) to UTF-16
// The result must be free()'d
string WstringToOHR(const wchar_t *wstr) {
	char *buf = WstringToMBstring(wstr, OHRCodePage);
	string ret = string(buf);
	free(buf);
	return ret;
}

wchar_t *MBstringToWstring(const char *str, int codepage) {
	int outlen = MultiByteToWideChar(codepage, 0, str, -1, NULL, 0);
	if (!outlen)
		return (wchar_t*)calloc(1, sizeof(wchar_t));
	wchar_t *buf = (wchar_t*)malloc(outlen * sizeof(wchar_t));
	MultiByteToWideChar(codepage, 0, str, -1, buf, outlen);
	return buf;
}

// Convert OHR native encoding (Latin-1 or UTF-8) to UTF-16
wstring OHRToWstring(const char *str) {
	wchar_t *buf = MBstringToWstring(str, OHRCodePage);
	wstring ret = wstring(buf);
	free(buf);
	return ret;
}
