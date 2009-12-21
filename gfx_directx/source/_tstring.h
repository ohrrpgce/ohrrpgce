//_tstring.h
//by Jay Tennant 12/8/09
//typedef's a string template of type TCHAR

#ifndef _TSTRING_H
#define _TSTRING_H

#include <windows.h>
#include <string>

typedef std::basic_string<TCHAR> tstring;

#endif