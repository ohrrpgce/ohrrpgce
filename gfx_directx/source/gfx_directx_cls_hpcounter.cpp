#include "gfx_directx_cls_hpcounter.h"
using namespace gfx;

bool HPCounter::s_bInitialized = false;
LARGE_INTEGER HPCounter::s_frequency;

void HPCounter::Initialize()
{
	if(s_bInitialized)
		return;
	s_bInitialized = true;
	::QueryPerformanceFrequency(&s_frequency);
}

LARGE_INTEGER HPCounter::GetFrequency()
{
	return s_frequency;
}

HPCounter::HPCounter()
{
	Initialize();
	Update();
}

HPCounter::~HPCounter()
{
}

void HPCounter::Update()
{
	m_time = GetTime();
}

UINT HPCounter::GetTimeDifferential()
{
	UINT dt = (1000 * (GetTime().QuadPart - m_time.QuadPart)) / s_frequency.QuadPart;
	return dt;
}

UINT HPCounter::GetTimeDifferentialHiRes()
{
	UINT dt = (1000000L * (GetTime().QuadPart - m_time.QuadPart)) / s_frequency.QuadPart;
	return dt;
}

LARGE_INTEGER HPCounter::GetTime()
{
	::QueryPerformanceCounter(&m_timeDelta);
	return m_timeDelta;
}