#include "gfx_directx_cls_osmouse.h"
using namespace gfx;

OSMouse::OSMouse() : m_hWnd(NULL), m_bInitialized(false), m_bOHRMouseActive(false), m_bFullscreen(false), m_bHidden(false),m_nPushCount(0)
{
	RECT z = {0};
	m_rClip = z;
	m_rPrevClip = z;
}

OSMouse::~OSMouse()
{
	Free();
}

int OSMouse::Initialize(HWND hWnd)
{
	m_hWnd = hWnd;
	::GetClipCursor(&m_rPrevClip);
	m_bInitialized = true;
	UpdateClientRect();
	return 0;
}

void OSMouse::Free()
{
	if(!m_bInitialized)
		return;
	m_bInitialized = false;
	::ClipCursor(&m_rPrevClip);
	if(m_nPushCount == 0)
		::ShowCursor(m_bHidden ? TRUE : FALSE);
	m_hWnd = NULL;
}

void OSMouse::Push_State()
{
	m_nPushCount++;
	if(!m_bInitialized)
		return;
	if(m_bHidden)
		::ShowCursor(TRUE);
	m_bHidden = false;
	::ClipCursor(&m_rPrevClip);
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
	RECT r = {0};
	::GetClientRect(m_hWnd, &r);
	POINT p = {r.left, r.top};
	::ClientToScreen(m_hWnd, &p);
	m_rClip.left = p.x;
	m_rClip.top = p.y;
	p.x = r.right;
	p.y = r.bottom;
	::ClientToScreen(m_hWnd, &p);
	m_rClip.right = p.x;
	m_rClip.bottom = p.y;
	if(m_bOHRMouseActive || m_bFullscreen)
	{//it is active already
		::ClipCursor(&m_rClip);
		if(!m_bHidden)
			::ShowCursor(FALSE);
		m_bHidden = true;
	}
	else
	{
		::ClipCursor(&m_rPrevClip);
		if(m_bHidden)
			::ShowCursor(TRUE);
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

UINT OSMouse::GetPushCount()
{
	return m_nPushCount;
}