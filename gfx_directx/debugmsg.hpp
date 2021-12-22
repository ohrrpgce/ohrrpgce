#pragma once

#include <winerror.h>

// gfx_directx can be linked to modules from the main engine, so we provide
// implementations of debugc and _throw_error
// errorlog.h defines debug(), debuginfo(), throw_error() and fatal_error() as
// macros which call _throw_error.
#include "../errorlog.h"

extern bool input_debug;

#define INPUTDEBUG(...)   if (input_debug) debug(errInfo, __VA_ARGS__);

namespace gfx
{
    const char *LastErrorString();
    const char *HRESULTString(HRESULT hresult);
}
