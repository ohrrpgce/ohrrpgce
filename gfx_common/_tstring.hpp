//_tstring.h
//(C) Copyright 2010 Jay Tennant and the OHRRPGCE Developers
//Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
//
//typedef's a string template of type T_CHAR
// Windows specific. This is redundant to, but a different implementation from,
// gfx_directx\Tstring.h

#ifndef _TSTRING_H
#define _TSTRING_H

#include <string>

typedef wchar_t W_CHAR;
typedef char A_CHAR;

#ifdef _UNICODE
typedef W_CHAR T_CHAR;
#define T_TEXT(quote) L##quote
#else
typedef A_CHAR T_CHAR;
#define T_TEXT(quote) quote
#endif

typedef std::basic_string<T_CHAR> _tstring;

class tstring : public _tstring
{
public:
	tstring();
	tstring(const W_CHAR* uniString);
	tstring(const A_CHAR* ansiString);
};

//converts char to tchar; if nSrcSize == 0, the length of the source string is taken
T_CHAR* CharToTchar(T_CHAR* szDest, unsigned int nDestSize, const A_CHAR* szSrc, unsigned int nSrcSize);

//converts wchar_t to tchar; if nSrcSize == 0, the length of the source string is taken
T_CHAR* WcharToTchar(T_CHAR* szDest, unsigned int nDestSize, const W_CHAR* szSrc, unsigned int nSrcSize);

//converts tchar to wchar_t; if nSrcSize == 0, the length of the source string is taken
W_CHAR* TcharToWchar(W_CHAR* szDest, unsigned int nDestSize, const T_CHAR* szSrc, unsigned int nSrcSize);

//converts tchar to char; if nSrcSize == 0, the length of the source string is taken
A_CHAR* TcharToChar(A_CHAR* szDest, unsigned int nDestSize, const T_CHAR* szSrc, unsigned int nSrcSize);

#endif
