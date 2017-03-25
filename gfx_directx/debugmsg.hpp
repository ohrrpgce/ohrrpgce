#pragma once

#include <winerror.h>
#include "../errorlevel.h"

extern "C" void debugc(ErrorLevel errlvl, const char* szMessage, ...);

namespace gfx
{
    const char *LastErrorString();
    const char *HRESULTString(HRESULT hresult);
}
