//window.h
//by Jay Tennant 10/30/09; updated 4/21/11
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
