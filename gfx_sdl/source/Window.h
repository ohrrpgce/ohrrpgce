//Window.h
//started 1/5/10
//manages sdl window interface

#ifndef GFX_WINDOW_H
#define GFX_WINDOW_H

#include "SDL.h"
#include "_tstring.h"

namespace gfx
{
	typedef int (*SDL_EventProc)(const SDL_Event *pEvent);

	class Window
	{
	protected:
		SDL_EventFilter m_pfnEventFilter;
		SDL_EventProc m_pfnEventProc;
	public:
		Window();
		virtual ~Window();

		void PumpMessages();
		void SetWindowTitle(const tstring& szWindowTitle);
		void SetEventFilter(SDL_EventFilter pfnEventFilter);
		void SetEventProc(SDL_EventProc pfnEventProc);
		void PushEvent(SDL_Event* pSdlEvent); //all events pushed bypasses event filter
	};
}

#endif
