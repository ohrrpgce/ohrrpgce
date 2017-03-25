#include "Window.h"
using namespace gfx;

Window::Window()
{
	m_pfnEventFilter = 0;
	m_pfnEventProc = 0;
}

Window::~Window()
{
}

void Window::PumpMessages()
{
	if(m_pfnEventProc == 0)
		return;
	static SDL_Event ev;
	while(SDL_PollEvent(&ev))
		m_pfnEventProc(&ev);
}

void Window::SetWindowTitle(const tstring &szWindowTitle)
{
	char buffer[256] = "";
	TcharToChar(buffer, 256, szWindowTitle.c_str(), 0);
	::SDL_WM_SetCaption(buffer, buffer);
}

void Window::SetEventFilter(SDL_EventFilter pfnEventFilter)
{
	m_pfnEventFilter = pfnEventFilter;
	if(m_pfnEventFilter != 0)
		::SDL_SetEventFilter(m_pfnEventFilter);
}

void Window::SetEventProc(gfx::SDL_EventProc pfnEventProc)
{
	m_pfnEventProc = pfnEventProc;
}

void Window::PushEvent(SDL_Event *pSdlEvent)
{
	if(0 != ::SDL_PushEvent(pSdlEvent)) //event queue may be full
	{
		PumpMessages();
		::SDL_PushEvent(pSdlEvent); //try, try again
	}
}
