#include "gfx_directx_cls_window.h"
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
	Shutdown();
}

int Window::Initialize(HINSTANCE hInstance, const TCHAR* szIconResource, WNDPROC lpfnWndProc)
{
	m_hInst = hInstance;
	WNDCLASSEX wc = {0};
	wc.cbSize			= sizeof(wc);
	wc.hbrBackground	= (HBRUSH)COLOR_BACKGROUND;
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
	wc.lpszClassName	= TEXT("gfx_directx_cls_window class");
	wc.style			= CS_HREDRAW | CS_VREDRAW;

	if(!::RegisterClassEx(&wc))
		return -1;
	m_rWindow.right = 800;
	m_rWindow.bottom = 600;
	::AdjustWindowRectEx(&m_rWindow, WS_OVERLAPPEDWINDOW, 0, 0);
	m_hWnd = ::CreateWindowEx(0, TEXT("gfx_directx_cls_window class"), TEXT(""), WS_OVERLAPPEDWINDOW, 
							  m_rWindow.left, m_rWindow.top, 
							  m_rWindow.right - m_rWindow.left, m_rWindow.bottom - m_rWindow.top, 
							  0, 0, m_hInst, 0);
	if(m_hWnd == NULL)
		return -1;
	m_bRunning = true;

	//::ShowWindow(m_hWnd, 1);
	//CenterWindow();
	//::UpdateWindow(m_hWnd);
	return 0;
}

void Window::Shutdown()
{
	::DestroyWindow(m_hWnd);
	::UnregisterClass(TEXT("gfx_directx_cls_window class"), m_hInst);
}

int Window::PumpMessages()
{
	if(!m_bRunning)
		return -1;
	if(::PeekMessage(&m_msg, 0, 0, 0, PM_REMOVE))
	{
		::TranslateMessage(&m_msg);
		::DispatchMessage(&m_msg);
		if(m_msg.message == WM_QUIT)
			m_bRunning = false;
	}
	return 0;
}

void Window::SetWindowTitle(const TCHAR *strTitle)
{
	if(!strTitle)
		return;
	::SetWindowText(m_hWnd, strTitle);
}

void Window::SetClientSize(int width, int height)
{
	m_rWindow.right = m_rWindow.left + width;
	m_rWindow.bottom = m_rWindow.top + height;
	::AdjustWindowRectEx(&m_rWindow, WS_OVERLAPPEDWINDOW, 0, 0);
	::SetWindowPos(m_hWnd, HWND_NOTOPMOST, 0, 0, m_rWindow.right - m_rWindow.left, m_rWindow.bottom - m_rWindow.top, SWP_NOMOVE);
	::GetWindowRect(m_hWnd, &m_rWindow);
}

void Window::SetWindowSize(int width, int height)
{
	::SetWindowPos(m_hWnd, HWND_NOTOPMOST, 0, 0, width, height, SWP_NOMOVE);
	::GetWindowRect(m_hWnd, &m_rWindow);
}

void Window::SetWindowPosition(int left, int top)
{
	::SetWindowPos(m_hWnd, HWND_NOTOPMOST, left, top, 0, 0, SWP_NOSIZE);
	::GetWindowRect(m_hWnd, &m_rWindow);
}

void Window::CenterWindow()
{
	HWND hParent = ::GetParent(m_hWnd);
	if(hParent == NULL)
		hParent = ::GetDesktopWindow();
	RECT rScreen;
	::GetWindowRect(hParent, &rScreen);
	LONG left, top;
	left = ((rScreen.right - rScreen.left) / 2) - ((m_rWindow.right - m_rWindow.left) / 2);
	top = ((rScreen.bottom - rScreen.top) / 2) - ((m_rWindow.bottom - m_rWindow.top) / 2);
	SetWindowPosition(left, top);
}

void Window::ShowWindow()
{
	::ShowWindow(m_hWnd, SW_SHOW);
}

void Window::HideWindow()
{
	::ShowWindow(m_hWnd, SW_HIDE);
}

HINSTANCE Window::GetAppHandle()
{
	return m_hInst;
}

HWND Window::GetWindowHandle()
{
	return m_hWnd;
}

RECT Window::GetWindowSize()
{
	return m_rWindow;
}

SIZE Window::GetClientSize()
{
	RECT rClient;
	::GetClientRect(m_hWnd, &rClient);
	SIZE ret = {rClient.right, rClient.bottom};
	return ret;
}

int Window::PostWindowMessage(UINT msg, WPARAM wParam, LPARAM lParam)
{
	return ::PostMessage(m_hWnd, msg, wParam, lParam);
}

int Window::SendWindowMessage(UINT msg, WPARAM wParam, LPARAM lParam)
{
	return ::SendMessage(m_hWnd, msg, wParam, lParam);
}