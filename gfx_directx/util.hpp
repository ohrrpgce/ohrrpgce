//(C) Copyright 2009-2017 Jay Tennant and the OHRRPGCE Developers
//Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.

#pragma once

#include <windows.h>

// A SIZE literal
inline SIZE SZ(int x, int y)
{
	SIZE ret;
	ret.cx = x;
	ret.cy = y;
	return ret;
}
