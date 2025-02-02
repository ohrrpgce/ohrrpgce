//CBackend.h
//(C) Copyright 2010 Jay Tennant and the OHRRPGCE Developers
//Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
//
//implements IBackend

#pragma once

#include "BackendDebugger.hpp"

class CBackend : public IBackend
{
public:
	using IBackend::m_hook;
};

extern CBackend g_Debugger;
