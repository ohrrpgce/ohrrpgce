#include "window.hpp"
#include "debugmsg.hpp"
using namespace gfx;

/* Window is largely subservient to the D3D class, which also
   does a lot of window handling (e.g. fullscreening is done by
   Reset()'ing the DirectX state, which indirectly causes the window
   state to change).
   Window doesn't even know whether we're currently windowed or fullscreen!!
   Also, before the backend is initialised, g_Window shouldn't be accessed,
   while g_DirectX collects parameters to use for startup.
*/

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
}

Window::~Window()
{
	shutdown();
}

HRESULT Window::initialize(HINSTANCE hInstance, const TCHAR* szIconResource, WNDPROC lpfnWndProc, SIZE size)
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

	m_rWindow.left = 0;
	m_rWindow.top = 0;
	m_rWindow.right = size.cx;
	m_rWindow.bottom = size.cy;
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

// This changes the size of the window, used when we are windowed
// (Also called before initialisation)
void Window::setClientSize(int width, int height)
{
	INPUTDEBUG("setClientSize %d*%d", width, height);
	m_rWindow.right = m_rWindow.left + width;
	m_rWindow.bottom = m_rWindow.top + height;
	// Compute window rect from client area rect
	::AdjustWindowRectEx(&m_rWindow, WS_OVERLAPPEDWINDOW, 0, 0);
	INPUTDEBUG("  adjusted to [SetWindowPos] %d*%d", m_rWindow.right - m_rWindow.left, m_rWindow.bottom - m_rWindow.top);
	if(!m_bRunning)
		return;
	// Generates a WM_SIZE, which causes g_DirectX and g_Mouse to be updated
	::SetWindowPos(m_hWnd, HWND_NOTOPMOST, 0, 0, m_rWindow.right - m_rWindow.left, m_rWindow.bottom - m_rWindow.top, SWP_NOMOVE);
	::GetWindowRect(m_hWnd, &m_rWindow);
}

// This changes the size of the window, used when we are fullscreen
// (Also called before initialisation)
void Window::setWindowSize(int width, int height)
{
	INPUTDEBUG("setWindowSize [SetWindowPos] %d*%d", width, height);
	if(!m_bRunning)
		return;
	// Generates a WM_SIZE, which causes g_DirectX and g_Mouse to be updated
	::SetWindowPos(m_hWnd, HWND_NOTOPMOST, 0, 0, width, height, SWP_NOMOVE);
	::GetWindowRect(m_hWnd, &m_rWindow);
}

void Window::setWindowPosition(int left, int top)
{
	if(!m_bRunning)
		return;
	::SetWindowPos(m_hWnd, HWND_NOTOPMOST, left, top, 0, 0, SWP_NOSIZE);
	::GetWindowRect(m_hWnd, &m_rWindow);
}

void Window::centerWindow()
{
	if(!m_bRunning)
		return;
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
