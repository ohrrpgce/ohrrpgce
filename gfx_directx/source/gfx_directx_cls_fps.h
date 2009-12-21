//gfx_directx_cls_fps.h
//by Jay Tennant 12/7/09
//manages fps counter and draws it to the screen

#ifndef GFX_DIRECTX_CLS_FPS_H
#define GFX_DIRECTX_CLS_FPS_H

#include "gfx_directx_cls_font.h"
#include "gfx_directx_cls_hpcounter.h"
#include <string>

namespace gfx
{
	class FPSDisplay
	{
	protected:
		Font m_font;
		HPCounter m_counter;
		UINT m_nFrameCount;
		UINT m_nFramePerSecond;
	public:
		FPSDisplay();
		virtual ~FPSDisplay();

		int Initialize(HWND hWnd, IDirect3DDevice9* d3ddev);
		void IncrementFrameCount(); //increments frame draw count by 1; also tests if time differential has passed 1 second
		void DrawFps(); //draws the frame/second count

		void OnLostDevice();
		void OnResetDevice();
	};
}

#endif