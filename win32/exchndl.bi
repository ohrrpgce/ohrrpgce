#pragma once

extern "Windows"
declare sub ExcHndlInit()
declare function ExcHndlSetLogFileNameA(szLogFileName as const zstring ptr) as boolean
end extern
