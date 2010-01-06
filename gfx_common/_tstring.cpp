#include "_tstring.h"

tstring::tstring() : _tstring()
{
}

tstring::tstring(const W_CHAR *uniString) : _tstring()
{
	T_CHAR buffer[256] = T_TEXT("");
	assign(WcharToTchar(buffer, 256, uniString, 0));
}

tstring::tstring(const A_CHAR *ansiString) : _tstring()
{
	T_CHAR buffer[256] = T_TEXT("");
	assign(CharToTchar(buffer, 256, ansiString, 0));
}

//converts char to tchar; if nSrcSize == 0, the length of the source string is taken
T_CHAR* CharToTchar(T_CHAR* szDest, unsigned int nDestSize, const A_CHAR* szSrc, unsigned int nSrcSize)
{
#ifdef _UNICODE
	size_t n = 0;
	::mbstowcs_s(&n, szDest, nDestSize, szSrc, (nSrcSize != 0 ? nSrcSize : ::strlen(szSrc)));
#else
	::strcpy_s(szDest, nDestSize, szSrc);
#endif
	return szDest;
}

//converts wchar_t to tchar; if nSrcSize == 0, the length of the source string is taken
T_CHAR* WcharToTchar(T_CHAR* szDest, unsigned int nDestSize, const W_CHAR* szSrc, unsigned int nSrcSize)
{
#ifdef _UNICODE
	::wcscpy_s(szDest, nDestSize, szSrc);
#else
	size_t n = 0;
	::wcstombs_s(&n, szDest, nDestSize, szSrc, (nSrcSize != 0 ? nSrcSize : ::wcslen(szSrc)));
#endif
	return szDest;
}

//converts tchar to wchar_t; if nSrcSize == 0, the length of the source string is taken
W_CHAR* TcharToWchar(W_CHAR* szDest, unsigned int nDestSize, const T_CHAR* szSrc, unsigned int nSrcSize)
{
#ifdef _UNICODE
	::wcscpy_s(szDest, nDestSize, szSrc);
#else
	size_t n = 0;
	::mbstowcs_s(&n, szDest, nDestSize, szSrc, (nSrcSize != 0 ? nSrcSize : ::strlen(szSrc)));
#endif
	return szDest;
}

//converts tchar to char; if nSrcSize == 0, the length of the source string is taken
A_CHAR* TcharToChar(A_CHAR* szDest, unsigned int nDestSize, const T_CHAR* szSrc, unsigned int nSrcSize)
{
#ifdef _UNICODE
	size_t n = 0;
	::wcstombs_s(&n, szDest, nDestSize, szSrc, (nSrcSize != 0 ? nSrcSize : ::wcslen(szSrc)));
#else
	::strcpy_s(szDest, nDestSize, szSrc);
#endif
	return szDest;
}
