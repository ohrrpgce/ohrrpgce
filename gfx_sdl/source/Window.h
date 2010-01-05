//Window.h
//started 1/5/10
//manages sdl window interface

#ifndef WINDOW_H
#define WINDOW_H

#include "SDL.h"
#include "_tstring.h"

#include "Graphics.h"
#include "Input.h"

namespace gfx
{
	class Window
	{
	protected:
	public:
		Window();
		virtual ~Window();

		int PumpMessages();
		void SetWindowTitle(const tstring& szWindowTitle);
		void SetEventFilter(SDL_EventFilter* pFilter);
		void PushEvent(const SDL_Event* pSdlEvent);

		void SetInput(gfx::Input *pInput);
		void SetGraphics(gfx::SDL *pGraphics);
	};
}

#endif