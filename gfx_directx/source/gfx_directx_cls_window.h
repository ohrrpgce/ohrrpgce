//gfx_directx_cls_window.h
//by Jay Tennant 10/30/09
//window wrapper

#ifndef GFX_DIRECTX_CLS_WINDOW_H
#define GFX_DIRECTX_CLS_WINDOW_H

#include "gfx_directx_TESTAPPconst.h"
#include <windows.h>

namespace gfx
{
	LRESULT CALLBACK WndProc(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam);

	class Window
	{
	protected:
		HINSTANCE m_hInst;
		HWND m_hWnd;
		MSG m_msg;
		RECT m_rWindow;
		bool m_bRunning;
	public:
		Window();
		virtual ~Window();

		int Initialize(HINSTANCE hInstance, const TCHAR* szIconResource, WNDPROC lpfnWndProc);
		void Shutdown();
		int PumpMessages();
		void SetWindowTitle(const TCHAR* strTitle);
		void SetClientSize(int width, int height);
		void SetWindowSize(int width, int height); //does not call AdjustWindowRect()
		void SetWindowPosition(int left, int top);
		void CenterWindow(); //centers window on the screen

		HINSTANCE GetAppHandle();
		HWND GetWindowHandle();
		RECT GetWindowSize();
		SIZE GetClientSize();
		int PostWindowMessage(UINT msg, WPARAM wParam, LPARAM lParam); //non-blocking window message call
		int SendWindowMessage(UINT msg, WPARAM wParam, LPARAM lParam); //blocking window message call
	};
}

#endif