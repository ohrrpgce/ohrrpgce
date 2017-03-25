#pragma once

#include "winerror.h"
#include "../errorlevel.h"

namespace gfx
{
    void Debug(ErrorLevel errlvl, const char* szMessage, ...);

    const char *LastErrorString();
    const char *HRESULTString(HRESULT hresult);
}
