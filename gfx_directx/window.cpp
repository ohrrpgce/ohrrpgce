#include "window.h"
using namespace gfx;

LRESULT CALLBACK gfx::WndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	switch(msg)
	{
	case WM_DESTROY:
		{
			::PostQuitMessage(0);
		} break;
	default:
		return ::DefWindowProc(hWnd, msg, wParam, lParam);
	}
	return 0;
}

Window::Window() : m_hInst(NULL), m_hWnd(NULL), m_bRunning(false)
{
	::ZeroMemory(&m_rWindow, sizeof(m_rWindow));
}

Window::~Window()
{
	shutdown();
}

HRESULT Window::initialize(HINSTANCE hInstance, const TCHAR* szIconResource, WNDPROC lpfnWndProc)
{
	m_hInst = hInstance;
	WNDCLASSEX wc = {0};
	LOGBRUSH lb = {BS_SOLID, RGB(0,0,0)};

	wc.cbSize			= sizeof(wc);
	wc.hbrBackground	= ::CreateBrushIndirect(&lb);//(HBRUSH)COLOR_BACKGROUND;
	wc.hCursor			= ::LoadCursor(0, IDC_ARROW);
	if(szIconResource != NULL)
	{
		wc.hIcon		= ::LoadIcon(::GetModuleHandle(NULL), szIconResource);
		wc.hIconSm		= ::LoadIcon(::GetModuleHandle(NULL), szIconResource);
	}
	else
	{
		wc.hIcon		= ::LoadIcon(0, IDI_APPLICATION);
		wc.hIconSm		= ::LoadIcon(0, IDI_APPLICATION);
	}
	wc.hInstance		= m_hInst;
	if(lpfnWndProc == NULL)
		wc.lpfnWndProc	= (WNDPROC)gfx::WndProc;
	else
		wc.lpfnWndProc	= lpfnWndProc;
	wc.lpszClassName	= TEXT("gfx_directx window class");
	wc.style			= CS_HREDRAW | CS_VREDRAW;

	if(!::RegisterClassEx(&wc))
		return HRESULT_FROM_WIN32(GetLastError());
	m_rWindow.right = 800;
	m_rWindow.bottom = 600;
	// Compute the size of the window from the size of the client area
	::AdjustWindowRectEx(&m_rWindow, WS_OVERLAPPEDWINDOW, 0, 0);
	m_hWnd = ::CreateWindowEx(0, TEXT("gfx_directx window class"), TEXT(""), WS_OVERLAPPEDWINDOW, 
							  m_rWindow.left, m_rWindow.top, 
							  m_rWindow.right - m_rWindow.left, m_rWindow.bottom - m_rWindow.top, 
							  0, 0, m_hInst, 0);
	if(m_hWnd == NULL)
		return HRESULT_FROM_WIN32(GetLastError());
	m_bRunning = true;

	//::ShowWindow(m_hWnd, 1);
	//CenterWindow();
	//::UpdateWindow(m_hWnd);
	return S_OK;
}

void Window::shutdown()
{
	::DestroyWindow(m_hWnd);
	::UnregisterClass(TEXT("gfx_directx window class"), m_hInst);
}

void Window::pumpMessages()
{
	if(!m_bRunning)
		return;
	if(::PeekMessage(&m_msg, 0, 0, 0, PM_REMOVE))
	{
		::TranslateMessage(&m_msg);
		::DispatchMessage(&m_msg);
		if(m_msg.message == WM_QUIT)
			m_bRunning = false;
	}
}

void Window::setWindowTitle(const TCHAR *strTitle)
{
	if(!strTitle)
		return;
	::SetWindowText(m_hWnd, strTitle);
}

void Window::setClientSize(int width, int height)
{
	m_rWindow.right = m_rWindow.left + width;
	m_rWindow.bottom = m_rWindow.top + height;
	::AdjustWindowRectEx(&m_rWindow, WS_OVERLAPPEDWINDOW, 0, 0);
	::SetWindowPos(m_hWnd, HWND_NOTOPMOST, 0, 0, m_rWindow.right - m_rWindow.left, m_rWindow.bottom - m_rWindow.top, SWP_NOMOVE);
	::GetWindowRect(m_hWnd, &m_rWindow);
}

void Window::setWindowSize(int width, int height)
{
	::SetWindowPos(m_hWnd, HWND_NOTOPMOST, 0, 0, width, height, SWP_NOMOVE);
	::GetWindowRect(m_hWnd, &m_rWindow);
}

void Window::setWindowPosition(int left, int top)
{
	::SetWindowPos(m_hWnd, HWND_NOTOPMOST, left, top, 0, 0, SWP_NOSIZE);
	::GetWindowRect(m_hWnd, &m_rWindow);
}

void Window::centerWindow()
{
	HWND hParent = ::GetParent(m_hWnd);
	if(hParent == NULL)
		hParent = ::GetDesktopWindow();
	RECT rScreen;
	::GetWindowRect(hParent, &rScreen);
	LONG left, top;
	left = ((rScreen.right - rScreen.left) / 2) - ((m_rWindow.right - m_rWindow.left) / 2);
	top = ((rScreen.bottom - rScreen.top) / 2) - ((m_rWindow.bottom - m_rWindow.top) / 2);
	setWindowPosition(left, top);
}

void Window::showWindow()
{
	::ShowWindow(m_hWnd, SW_SHOW);
}

void Window::hideWindow()
{
	::ShowWindow(m_hWnd, SW_HIDE);
}

HINSTANCE Window::getAppHandle()
{
	return m_hInst;
}

HWND Window::getWindowHandle()
{
	return m_hWnd;
}

RECT Window::getWindowRect()
{
	return m_rWindow;
}

SIZE Window::getClientSize()
{
	RECT rClient;
	::GetClientRect(m_hWnd, &rClient);
	SIZE ret = {rClient.right, rClient.bottom};
	return ret;
}

BOOL Window::postWindowMessage(UINT msg, WPARAM wParam, LPARAM lParam)
{
	return ::PostMessage(m_hWnd, msg, wParam, lParam);
}

BOOL Window::sendWindowMessage(UINT msg, WPARAM wParam, LPARAM lParam)
{
	return ::SendMessage(m_hWnd, msg, wParam, lParam);
}
