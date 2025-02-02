//window.h
//(C) Copyright 2009-2017 Jay Tennant and the OHRRPGCE Developers
//Dual licensed under the GNU GPL v2+ and MIT Licenses. Read LICENSE.txt for terms and disclaimer of liability.
//
//window wrapper

#pragma once

#include <windows.h>

namespace gfx
{
	LRESULT CALLBACK WndProc(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam);

	// Note: a lot of the window handling is actually in D3D. Window doesn't even
	// know whether we're currently windowed or fullscreen!!
	class Window
	{
	protected:
		HINSTANCE m_hInst;
		HWND m_hWnd;
		MSG m_msg;
		RECT m_rWindow;  // Area of the whole window
		bool m_bRunning;
	public:
		Window();
		~Window();

		HRESULT initialize(HINSTANCE hInstance, const TCHAR* szIconResource, WNDPROC lpfnWndProc, SIZE size);
		void shutdown();
		void pumpMessages();
		void setWindowTitle(const TCHAR* strTitle);
		void setClientSize(int width, int height);
		void setWindowSize(int width, int height); //does not call AdjustWindowRect()
		void setWindowPosition(int left, int top);
		void centerWindow(); //centers window on the screen
		void showWindow(); //shows the window
		void hideWindow(); //hides the window

		HINSTANCE getAppHandle();
		HWND getWindowHandle();
		RECT getWindowRect();  // Position of the whole window
		SIZE getClientSize();
		BOOL postWindowMessage(UINT msg, WPARAM wParam, LPARAM lParam); //non-blocking window message call
		BOOL sendWindowMessage(UINT msg, WPARAM wParam, LPARAM lParam); //blocking window message call
	};
}
