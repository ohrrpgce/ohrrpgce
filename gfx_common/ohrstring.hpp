// Windows-specific.

#ifndef OHRSTRING_H
#define OHRSTRING_H

// Codepage for OHR-native string format (currently Latin-1, in future UTF8)
extern int OHRCodePage;

// Convert UTF-16 to whatever the OHR native encoding is (Latin-1 or UTF-8)
std::string WstringToOHR(const wchar_t*);

// Convert OHR native encoding (Latin-1 or UTF-8) to UTF-16
std::wstring OHRToWstring(const char *str);

// if you have an ASCII string, then don't call TstringToOHR
#ifdef _UNICODE
// _UNICODE controls TCHAR, UNICODE controls winapi *W/*A selection

inline std::string TstringToOHR(const wchar_t *tstr) {
	return WstringToOHR(tstr);
}

inline std::wstring OHRToTstring(const char *str) {
	return OHRToWstring(str);
}

#else

#error _UNICODE must be #defined; ANSI encoding is not supported.

// No point implementing these, we will never compile without UNICODE
//inline std::string TstringToOHR(const char *str) {}
//inline std::wstring OHRToTstring(const char *str) {}

#endif

#endif
