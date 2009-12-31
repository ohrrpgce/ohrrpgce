#include "_tstring.h"

//converts char to tchar; if nSrcSize == 0, the length of the source string is taken
TCHAR* CharToTchar(TCHAR* szDest, UINT nDestSize, const CHAR* szSrc, UINT nSrcSize)
{
#ifdef _UNICODE
	::MultiByteToWideChar(CP_UTF8, 0, szSrc, (nSrcSize != 0 ? nSrcSize : ::strlen(szSrc)), szDest, nDestSize);
#else
	::strcpy_s(szDest, nDestSize, szSrc);
#endif
	return szDest;
}

//converts wchar_t to tchar; if nSrcSize == 0, the length of the source string is taken
TCHAR* WcharToTchar(TCHAR* szDest, UINT nDestSize, const WCHAR* szSrc, UINT nSrcSize)
{
#ifdef _UNICODE
	::wcscpy_s(szDest, nDestSize, szSrc);
#else
	::WideCharToMultiByte(CP_UTF8, 0, szSrc, (nSrcSize != 0 ? nSrcSize : ::wcslen(szSrc)), szDest, nDestSize, 0, FALSE);
#endif
	return szDest;
}

//converts tchar to wchar_t; if nSrcSize == 0, the length of the source string is taken
WCHAR* TcharToWchar(WCHAR* szDest, UINT nDestSize, const TCHAR* szSrc, UINT nSrcSize)
{
#ifdef _UNICODE
	::wcscpy_s(szDest, nDestSize, szSrc);
#else
	::MultiByteToWideChar(CP_UTF8, 0, szSrc, (nSrcSize != 0 ? nSrcSize : ::strlen(szSrc)), szDest, nDestSize);
#endif
	return szDest;
}

//converts tchar to char; if nSrcSize == 0, the length of the source string is taken
CHAR* TcharToChar(CHAR* szDest, UINT nDestSize, const TCHAR* szSrc, UINT nSrcSize)
{
#ifdef _UNICODE
	::WideCharToMultiByte(CP_UTF8, 0, szSrc, (nSrcSize != 0 ? nSrcSize : ::wcslen(szSrc)), szDest, nDestSize, 0, FALSE);
#else
	::strcpy_s(szDest, nDestSize, szSrc);
#endif
	return szDest;
}
