#include <windows.h>
#include <string>
#include <cstdlib>
#include "ohrstring.h"

using namespace std;

// Codepage for OHR-native string format (currently Latin-1, in future UTF8)
int OHRCodePage = 1252;
//int OHRCodePage = CP_UTF8;

// Convert UTF-16 to whatever the OHR native encoding is (Latin-1 or UTF-8)
string WstringToOHR(const wchar_t *wstr) {
	int outlen = WideCharToMultiByte(OHRCodePage, 0, wstr, -1, NULL, 0, NULL, NULL);
	if (!outlen)
		return string();
	char *buf = (char*)alloca(outlen);
        WideCharToMultiByte(OHRCodePage, 0, wstr, -1, buf, outlen, NULL, NULL);
        outlen--;  // Remove trailing nul
	return string(buf, outlen);
}

// Convert OHR native encoding (Latin-1 or UTF-8) to UTF-16
wstring OHRToWstring(const char *str) {
	int outlen = MultiByteToWideChar(OHRCodePage, 0, str, -1, NULL, 0);
	if (!outlen)
		return wstring();
	wchar_t *buf = (wchar_t*)alloca(outlen * sizeof(wchar_t));
	MultiByteToWideChar(OHRCodePage, 0, str, -1, buf, outlen);
                outlen--;  // Remove trailing nul
	return wstring(buf, outlen);
}
