// Windows-specific routines for wchar_t <-> OHR's encoding

#ifndef OHRSTRING_H
#define OHRSTRING_H

// Codepage for OHR-native string format (currently Latin-1, in future UTF8)
extern int OHRCodePage;

#ifdef __cplusplus
extern "C" {
#endif

// Convert UTF-16 to the given codepage (1252 for Latin-1, or CP_UTF8, or OHRCodePage)
// The result must be free()'d
char *WstringToMBstring(const wchar_t *wstr, int codepage);

// Convert encoding (1252 for Latin-1, or CP_UTF8, or OHRCodePage) to UTF-16
// The result must be free()'d
wchar_t *MBstringToWstring(const char *str, int codepage);

#ifdef __cplusplus
}

#include <string>

// Convert UTF-16 to whatever the OHR native encoding is (Latin-1 or UTF-8)
std::string WstringToOHR(const wchar_t*);

// Convert OHR native encoding (Latin-1 or UTF-8) to UTF-16
std::wstring OHRToWstring(const char *str);


// if you have an ASCII string, then don't call TstringToOHR
#ifdef _UNICODE
// _UNICODE controls TCHAR, UNICODE controls winapi *W/*A selection
// gfx_directx.dll is compiled with UNICODE, and everything else isn't!

inline std::string TstringToOHR(const wchar_t *tstr) {
	return WstringToOHR(tstr);
}

inline std::wstring OHRToTstring(const char *str) {
	return OHRToWstring(str);
}

#else

//Not bothering to implement these, please don't use Tstring/TCHAR.
//inline std::string TstringToOHR(const char *str) {}
//inline std::wstring OHRToTstring(const char *str) {}

#endif

#endif

#endif
