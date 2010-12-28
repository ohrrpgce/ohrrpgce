//CBackend.h
//by Jay Tennant 12/27/10
//implements IBackend

#pragma once

#include "BackendDebugger.h"

class CBackend : public IBackend
{
public:
	using IBackend::m_hook;
};

extern CBackend g_Debugger;