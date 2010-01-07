#include "gfx_osmouse.h"
using namespace gfx;

OSMouse::OSMouse() : m_bInitialized(false), m_bOHRMouseActive(false), m_bFullscreen(false), m_bHidden(false), m_nPushCount(0)
{
}

OSMouse::~OSMouse()
{
}

void OSMouse::Push_State()
{
	m_nPushCount++;
	if(!m_bInitialized)
		return;
	if(m_bHidden)
		::SDL_ShowCursor(1);
	m_bHidden = false;
	::SDL_WM_GrabInput(SDL_GRAB_OFF);
}

void OSMouse::Pop_State()
{
	if(m_nPushCount != 0)
		m_nPushCount--;
	if(m_nPushCount == 0)
		UpdateClientRect();
}

void OSMouse::OHRMouseActive(bool bActive)
{
	m_bOHRMouseActive = bActive;
	UpdateClientRect();
}

void OSMouse::Fullscreen(bool bFullscreen)
{
	m_bFullscreen = bFullscreen;
	UpdateClientRect();
}

void OSMouse::UpdateClientRect()
{
	if(!m_bInitialized)
		return;
	if(m_nPushCount > 0)
		return;
	if(m_bOHRMouseActive || m_bFullscreen)
	{//it is active already
		::SDL_WM_GrabInput(SDL_GRAB_ON);
		if(!m_bHidden)
			::SDL_ShowCursor(0);
		m_bHidden = true;
	}
	else
	{
		::SDL_WM_GrabInput(SDL_GRAB_OFF);
		if(m_bHidden)
			::SDL_ShowCursor(1);
		m_bHidden = false;
	}
}

bool OSMouse::IsOHRMouseActive()
{
	return m_bOHRMouseActive;
}

bool OSMouse::IsFullscreen()
{
	return m_bFullscreen;
}

unsigned int OSMouse::GetPushCount()
{
	return m_nPushCount;
}