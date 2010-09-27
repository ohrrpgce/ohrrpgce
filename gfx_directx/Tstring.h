//Tstring.h
//by Jay Tennant 12/8/09; updated 1/26/10; updated 9/7/10
//typedef's a string template of type TCHAR; thread safe

#ifndef TSTRING_H
#define TSTRING_H

#include <string>

#ifdef _UNICODE
typedef std::basic_string<wchar_t> _Tstring;
#define JTEXT(quote) L##quote
#else
typedef std::basic_string<char> _Tstring;
#define JTEXT(quote) quote
#endif //_UNICODE

template <class T_StrDest, class T_StrSrc>
T_StrDest* StringToString(T_StrDest* szDest, unsigned int nDestSize, const T_StrSrc* szSrc)
{
	if(szDest == 0 || szSrc == 0)
		return szDest;
	if(sizeof(T_StrDest) == sizeof(T_StrSrc))
	{
		if(sizeof(T_StrDest) == sizeof(char))
			::strcpy_s((char*)szDest, nDestSize, (const char*)szSrc);
		else
			::wcscpy_s((wchar_t*)szDest, nDestSize, (const wchar_t*)szSrc);
	}
	else if(sizeof(T_StrDest) > sizeof(T_StrSrc))
	{
		size_t n = 0;
		::mbstowcs_s(&n, (wchar_t*)szDest, nDestSize, (const char*)szSrc, ::strlen((const char*)szSrc));
	}
	else
	{
		size_t n = 0;
		::wcstombs_s(&n, (char*)szDest, nDestSize, (const wchar_t*)szSrc, ::wcslen((const wchar_t*)szSrc));
	}
	return szDest;
}

class Tstring : public _Tstring
{
public:
	Tstring() : _Tstring() {}
	Tstring(const wchar_t* uniString) : _Tstring()
	{
		unsigned int nLength = 0;
		if(uniString)
			nLength = ::wcslen(uniString);
		if(nLength)
		{
#ifdef _UNICODE
			assign(uniString);
#else
			char *pBuffer = new char[nLength + 1]; //+1 for null terminator
			::memset((void*)pBuffer, 0, sizeof(pBuffer[0]) * (nLength + 1));
			if(pBuffer)
			{
				assign(StringToString(pBuffer, nLength + 1, uniString));
				delete [] pBuffer;
			}
#endif
		}
	}

	Tstring(const char* ansiString)
	{
		unsigned int nLength = 0;
		if(ansiString)
			nLength = ::strlen(ansiString);
		if(nLength)
		{
#ifdef _UNICODE
			wchar_t *pBuffer = new wchar_t[nLength + 1]; //+1 for null terminator
			::memset((void*)pBuffer, 0, sizeof(pBuffer[0]) * (nLength + 1));
			if(pBuffer)
			{
				assign(StringToString(pBuffer, nLength + 1, ansiString));
				delete [] pBuffer;
			}
#else
			assign(ansiString);
#endif
		}
	}
};

#endif //TSTRING_H