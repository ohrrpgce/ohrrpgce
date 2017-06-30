
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
