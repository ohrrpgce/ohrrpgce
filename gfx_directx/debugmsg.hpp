#pragma once

#include <winerror.h>
#include "../errorlevel.h"

// gfx_directx can be linked to modules from the main engine, which is why
// this is provided in the global namespace: we provide an implementation
// of debugc for any such code that's ususally linked against common.rbas.
extern "C" void debugc(ErrorLevel errlvl, const char* szMessage);

extern bool input_debug;

#define INPUTDEBUG(...)   if (input_debug) debug(errInfo, __VA_ARGS__);

namespace gfx
{
    // The actual debug() function uses in gfx_directx
    void debug(ErrorLevel errlvl, const char* szMessage, ...);
    const char *LastErrorString();
    const char *HRESULTString(HRESULT hresult);
}
