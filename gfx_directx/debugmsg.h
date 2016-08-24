#pragma once

#include "../errorlevel.h"

namespace gfx
{
    void Debug(ErrorLevel errlvl, const char* szMessage, ...);
}
