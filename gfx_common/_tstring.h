//_tstring.h
//by Jay Tennant 12/8/09
//typedef's a string template of type TCHAR

#ifndef _TSTRING_H
#define _TSTRING_H

#include <windows.h>
#include <string>
#include <tchar.h>

typedef std::basic_string<TCHAR> _tstring;

class tstring : public _tstring
{
public:
	tstring();
	tstring(wchar_t* uniString);
	tstring(char* ansiString);
	virtual ~tstring();
};

//converts char to tchar; if nSrcSize == 0, the length of the source string is taken
TCHAR* CharToTchar(TCHAR* szDest, UINT nDestSize, const CHAR* szSrc, UINT nSrcSize);

//converts wchar_t to tchar; if nSrcSize == 0, the length of the source string is taken
TCHAR* WcharToTchar(TCHAR* szDest, UINT nDestSize, const WCHAR* szSrc, UINT nSrcSize);

//converts tchar to wchar_t; if nSrcSize == 0, the length of the source string is taken
WCHAR* TcharToWchar(WCHAR* szDest, UINT nDestSize, const TCHAR* szSrc, UINT nSrcSize);

//converts tchar to char; if nSrcSize == 0, the length of the source string is taken
CHAR* TcharToChar(CHAR* szDest, UINT nDestSize, const TCHAR* szSrc, UINT nSrcSize);

#endif