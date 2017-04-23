#pragma once

#include <winerror.h>
#include "../errorlevel.h"

// gfx_directx can be linked to modules from the main engine, which is why
// this is provided in the global namespace
extern "C" void debugc(ErrorLevel errlvl, const char* szMessage, ...);

extern bool input_debug;

#define INPUTDEBUG(...)   if (input_debug) debugc(errInfo, __VA_ARGS__);

namespace gfx
{
    const char *LastErrorString();
    const char *HRESULTString(HRESULT hresult);
}
