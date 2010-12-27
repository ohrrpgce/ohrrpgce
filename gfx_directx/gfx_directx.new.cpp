#include "gfx_msg.h"
#include "gfx_directx.h"
#include "gfx_directx_cls_window.h"
#include "gfx_directx_cls_d3d.h"
#include "gfx_directx_cls_keyboard.h"
#include "gfx_directx_cls_mouse.h"
#include "gfx_directx_cls_joystick.h"
#include "gfx_directx_version.h"
using namespace gfx;

#include "resource.h"

//use common controls available on different windows OS'
#if defined _M_IX86
#pragma comment(linker,"/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='x86' publicKeyToken='6595b64144ccf1df' language='*'\"")
#elif defined _M_IA64
#pragma comment(linker,"/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='ia64' publicKeyToken='6595b64144ccf1df' language='*'\"")
#elif defined _M_X64
#pragma comment(linker,"/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='amd64' publicKeyToken='6595b64144ccf1df' language='*'\"")
#else
#pragma comment(linker,"/manifestdependency:\"type='win32' name='Microsoft.Windows.Common-Controls' version='6.0.0.0' processorArchitecture='*' publicKeyToken='6595b64144ccf1df' language='*'\"")
#endif

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Version 1.0 interfaces

void OnCriticalError(const char* szError) {}
void SendDebugString(const char* szMessage) {}

DFI_IMPLEMENT_CDECL(int, gfx_init, void (__cdecl *terminate_signal_handler)(void) , const char* windowicon, char* info_buffer, int info_buffer_size)
{
	GFX_INIT gfxInit = {sizeof(GFX_INIT), "DirectX Backend", windowicon, terminate_signal_handler, OnCriticalError, SendDebugString};
	if(gfx_Initialize(&gfxInit) == 0)
	{
		if(info_buffer != NULL && info_buffer_size > 16)
			strcpy(info_buffer, "Backend failed!");
		return 0;
	}
	if(info_buffer != NULL && info_buffer_size > 18)
		strcpy(info_buffer, "Backend success!");
	return 1;
}

DFI_IMPLEMENT_CDECL(void, gfx_close)
{
	gfx_Shutdown();
}

DFI_IMPLEMENT_CDECL(int, gfx_getversion)
{
	return gfx_GetVersion();
}

DFI_IMPLEMENT_CDECL(void, gfx_showpage, unsigned char *raw, int w, int h)
{
	gfx_Present(raw, w, h, 0);
	gfx_PumpMessages();
}

DFI_IMPLEMENT_CDECL(void, gfx_setpal, unsigned int *pal)
{
	gfx_Present(0, 0, 0, pal);
	gfx_PumpMessages();
}

DFI_IMPLEMENT_CDECL(int, gfx_screenshot, const char* fname)
{
	return gfx_ScreenShot(fname);
}

DFI_IMPLEMENT_CDECL(void, gfx_setwindowed, int iswindow)
{
	gfx_SendMessage(OM_GFX_SETWINDOWED, iswindow, 0);
}

DFI_IMPLEMENT_CDECL(void, gfx_windowtitle, const char* title)
{
	gfx_SetWindowTitle(title);
}

DFI_IMPLEMENT_CDECL(WindowState*, gfx_getwindowstate)
{//there isn't an equivalent message in the new backend
	return 0;
}

DFI_IMPLEMENT_CDECL(int, gfx_setoption, const char* opt, const char* arg)
{
	if(!opt || !arg)
		return 0;
	if(::strcmp(opt, "w") == 0 || ::strcmp(opt, "width") == 0)
		gfx_SendMessage(OM_GFX_SETWIDTH, ::atoi(arg), 0);
	else if(::strcmp(opt, "h") == 0 || ::strcmp(opt, "height") == 0)
		gfx_SendMessage(OM_GFX_SETHEIGHT, ::atoi(arg), 0);
	else if(::strcmp(opt, "f") == 0 || ::strcmp(opt, "fullscreen") == 0)
	{
		if(*arg == '0')
			gfx_SendMessage(OM_GFX_SETWINDOWED, 0, 0);
		else
			gfx_SendMessage(OM_GFX_SETWINDOWED, 1, 0);
	}
	else if(::strcmp(opt, "v") == 0 || ::strcmp(opt, "vsync") == 0)
	{
		if(*arg == '0')
			gfx_SendMessage(OM_GFX_SETVSYNC, 0, 0);
		else
			gfx_SendMessage(OM_GFX_SETVSYNC, 1, 0);
	}
	else if(::strcmp(opt, "a") == 0 || ::strcmp(opt, "aspect") == 0)
	{
		if(*arg == '0')
			gfx_SendMessage(OM_GFX_SETARP, 0, 0);
		else
			gfx_SendMessage(OM_GFX_SETARP, 1, 0);
	}
	else if(::strcmp(opt, "s") == 0 || ::strcmp(opt, "smooth") == 0)
	{
		if(*arg == '0')
			gfx_SendMessage(OM_GFX_SETSMOOTH, 0, 0);
		else
			gfx_SendMessage(OM_GFX_SETSMOOTH, 1, 0);
	}
	else if(::strcmp(opt, "ss") == 0 || ::strcmp(opt, "screenshot") == 0)
	{
		if(*arg == 'j')
			gfx_SendMessage(OM_GFX_SETSSFORMAT, 1, 0);
		else if(*arg == 'b')
			gfx_SendMessage(OM_GFX_SETSSFORMAT, 2, 0);
		else if(*arg == 'p')
			gfx_SendMessage(OM_GFX_SETSSFORMAT, 3, 0);
		else if(*arg == 'd')
			gfx_SendMessage(OM_GFX_SETSSFORMAT, 4, 0);
		else
			gfx_SendMessage(OM_GFX_SETSSFORMAT, 0, 0);
	}
	else
		return 0;
	return 2;
}

DFI_IMPLEMENT_CDECL(const char*, gfx_describe_options)
{
	return "-w -width [x]  sets the width of the client area\n" \
		"-h -height [x]  sets the height of the client area\n" \
		"-f -fullscreen [0* | 1]  toggles fullscreen on startup\n" \
		"    the above may NOT be called before width and height\n" \
		"-v -vsync [0 | 1*]  toggles vsync\n" \
		"-a -aspect [0 | 1*]  toggles aspect ratio preservation\n" \
		"-s -smooth [0* | 1]  toggles smooth linear interpolation of display\n" \
		"-ss -screenshot [jpg | bmp | png* | dds | ohr]\n" \
		"     the above sets the screen shot format";
}

DFI_IMPLEMENT_CDECL(void, io_init)
{//there isn't an equivalent message in the new backend
}

DFI_IMPLEMENT_CDECL(void, io_pollkeyevents)
{
	gfx_Present(0,0,0,0);
	gfx_PumpMessages();
}

DFI_IMPLEMENT_CDECL(void, io_updatekeys, int *keybd)
{
	gfx_GetKeyboard(keybd);
}

DFI_IMPLEMENT_CDECL(int, io_setmousevisibility, int visible)
{
	if(visible == 0)
		gfx_HideCursor();
	else
		gfx_ShowCursor();
	return 1;
}

DFI_IMPLEMENT_CDECL(void, io_getmouse, int& mx, int& my, int& mwheel, int& mbuttons)
{
	gfx_GetMouse(mx, my, mwheel, mbuttons);
}

DFI_IMPLEMENT_CDECL(void, io_setmouse, int x, int y)
{
	gfx_SetMouse(x, y);
}

DFI_IMPLEMENT_CDECL(void, io_mouserect, int xmin, int xmax, int ymin, int ymax)
{
	gfx_ClipCursor(xmin, ymin, xmax, ymax);
}

DFI_IMPLEMENT_CDECL(int, io_readjoysane, int joynum, int& button, int& x, int& y)
{
	return gfx_GetJoystick(joynum, x, y, button);
}





/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Version 2.0 interfaces

Window g_Window;
HWND g_hWndDlg;
D3D g_DirectX;
Keyboard g_Keyboard;
Mouse2 g_Mouse;
Joystick g_Joystick;

void DebugString(const char* szMessage) { MessageBoxA(NULL, szMessage, "Debug Message", MB_OK); }

struct gfx_BackendState
{
	Tstring szWindowTitle;
	Tstring szWindowIcon;
	void (__cdecl *PostTerminateSignal)(void);
	void (__cdecl *OnCriticalError)(const char* szError);
	void (__cdecl *SendDebugString)(const char* szMessage);
	bool bClosing; //flagged when shutting down
	Tstring szHelpText;
} g_State;

LRESULT CALLBACK OHRWndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
BOOL CALLBACK OHROptionsDlgModeless(HWND hWndDlg, UINT msg, WPARAM wParam, LPARAM lParam);
SIZE CalculateNativeResolutionMultiple(UINT width, UINT height, UINT targetWidth = 320, UINT targetHeight = 200);
bool IsNativeResolutionMultiple(UINT width, UINT height, UINT targetWidth = 320, UINT targetHeight = 200);

void LoadHelpText()
{
	HMODULE hMod = GetModuleHandle(MODULENAME);
	if(hMod == NULL) return;
	HRSRC hRsrc = FindResource(hMod, TEXT("HelpFile"), RT_RCDATA);
	if(hRsrc == NULL) return;
	HGLOBAL hData = LoadResource(hMod, hRsrc);
	if(hData == NULL) return;
	char* pText = (char*)LockResource(hData);
	if(pText == NULL) return;
	g_State.szHelpText = pText;
}

DFI_IMPLEMENT_CDECL(int, gfx_Initialize, const GFX_INIT *pCreationData)
{
	if(pCreationData == NULL)
		return FALSE;
	if(pCreationData->nSize < sizeof(GFX_INIT))
		return FALSE;

	CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);
	LoadHelpText();

	TCHAR buffer[256] = TEXT("");
	g_State.szWindowIcon = StringToString(buffer, 256, pCreationData->szWindowIcon);
	g_State.PostTerminateSignal = pCreationData->PostTerminateSignal;
	g_State.OnCriticalError = pCreationData->OnCriticalError;
	g_State.SendDebugString = /*DebugString;*/pCreationData->SendDebugString;

	if(g_State.PostTerminateSignal == NULL || g_State.OnCriticalError == NULL || g_State.SendDebugString == NULL)
		return FALSE;

	g_State.SendDebugString("gfx_directx: Initializing...");

	if(S_OK != g_Window.Initialize(::GetModuleHandle(MODULENAME), 
								   (pCreationData->szWindowIcon ? g_State.szWindowIcon.c_str() : NULL), 
								   (WNDPROC)OHRWndProc))
	{
		g_State.SendDebugString("gfx_directx: Failed at window initialization! Fallback...");
		return FALSE;
	}

	g_State.SendDebugString("gfx_directx: Window Intialized!");

	if( FAILED(g_DirectX.Initialize(&g_Window, MODULENAME)) )
	{
		g_Window.Shutdown();
		gfx_PumpMessages();
		g_State.SendDebugString("gfx_directx: Failed at d3d initialization! Fallback...");
		return FALSE;
	}

	g_State.SendDebugString("gfx_directx: D3D Initialized!");

	if(FAILED(g_Joystick.Initialize( g_Window.GetAppHandle(), g_Window.GetWindowHandle() )))
		g_State.SendDebugString("gfx_directx: Failed to support joysticks!");
	else
		g_State.SendDebugString("gfx_directx: Joysticks supported!");

	gfx_SetWindowTitle(pCreationData->szInitWindowTitle);

	g_Window.SetClientSize(640, 400);
	g_Window.CenterWindow();
	g_Window.ShowWindow();

	g_State.SendDebugString("gfx_directx: Initialization success!");
	return TRUE;
}

DFI_IMPLEMENT_CDECL(void, gfx_Shutdown)
{
	g_State.SendDebugString("gfx_directx: Closing backend...");
	g_Joystick.Shutdown();
	g_DirectX.Shutdown();
	g_Window.Shutdown();
	gfx_PumpMessages();
	g_State.SendDebugString("gfx_directx: Close complete!");
	CoUninitialize();
}

DFI_IMPLEMENT_CDECL(int, gfx_SendMessage, unsigned int msg, unsigned int dwParam, void *pvParam)
{
	switch(msg)
	{
	case OM_GFX_SETWIDTH:
		g_Window.SetClientSize(dwParam, g_Window.GetClientSize().cy);
		break;
	case OM_GFX_SETHEIGHT:
		g_Window.SetClientSize(g_Window.GetClientSize().cx, dwParam);
		break;
	case OM_GFX_SETCLIENTAREA:
		g_Window.SetClientSize(dwParam, (int)pvParam);
		break;
	case OM_GFX_SETLEFT:
		g_Window.SetWindowPosition(dwParam, g_Window.GetWindowSize().top);
		break;
	case OM_GFX_SETTOP:
		g_Window.SetWindowPosition(g_Window.GetWindowSize().left, dwParam);
		break;
	case OM_GFX_SETPOSITION:
		g_Window.SetWindowPosition(dwParam, (int)pvParam);
		break;
	case OM_GFX_SETARP:
		g_DirectX.SetAspectRatioPreservation(dwParam != FALSE);
		break;
	case OM_GFX_SETWINDOWED:
		g_DirectX.SetViewFullscreen(dwParam == FALSE);
		break;
	case OM_GFX_SETSMOOTH:
		g_DirectX.SetSmooth(dwParam != FALSE);
		break;
	case OM_GFX_SETVSYNC:
		g_DirectX.SetVsyncEnabled(dwParam == FALSE);
		break;
	case OM_GFX_SETSSFORMAT:
		switch(dwParam)
		{
		case 0:
			g_DirectX.SetImageFileFormat(D3DXIFF_FORCE_DWORD);
			break;
		case 1:
			g_DirectX.SetImageFileFormat(D3DXIFF_JPG);
			break;
		case 2:
			g_DirectX.SetImageFileFormat(D3DXIFF_BMP);
			break;
		case 3:
			g_DirectX.SetImageFileFormat(D3DXIFF_PNG);
			break;
		case 4:
			g_DirectX.SetImageFileFormat(D3DXIFF_DDS);
			break;
		default:
			g_DirectX.SetImageFileFormat(D3DXIFF_FORCE_DWORD);
			break;
		}
		break;

	case OM_GFX_GETWIDTH:
		return g_Window.GetClientSize().cx;
	case OM_GFX_GETHEIGHT:
		return g_Window.GetClientSize().cy;
	case OM_GFX_GETCLIENTAREA:
		return (g_Window.GetClientSize().cx << 16) | g_Window.GetClientSize().cy;
	case OM_GFX_GETLEFT:
		return g_Window.GetWindowSize().left;
	case OM_GFX_GETTOP:
		return g_Window.GetWindowSize().top;
	case OM_GFX_GETPOSITION:
		return (g_Window.GetWindowSize().left << 16) | g_Window.GetWindowSize().top;
	case OM_GFX_GETARP:
		return g_DirectX.IsAspectRatioPreserved() ? TRUE : FALSE;
	case OM_GFX_GETWINDOWED:
		return g_DirectX.IsViewFullscreen() ? FALSE : TRUE;
	case OM_GFX_GETSMOOTH:
		return g_DirectX.IsSmooth() ? TRUE : FALSE;
	case OM_GFX_GETVSYNC:
		return g_DirectX.IsVsyncEnabled() ? TRUE : FALSE;
	case OM_GFX_GETSSFORMAT:
		switch(g_DirectX.GetImageFileFormat())
		{
		case D3DXIFF_JPG:
			return 1;
		case D3DXIFF_BMP:
			return 2;
		case D3DXIFF_PNG:
			return 3;
		case D3DXIFF_DDS:
			return 4;
		default:
			return 0;
		}
	default:
		return FALSE;
	}
	return TRUE;
}

DFI_IMPLEMENT_CDECL(int, gfx_GetVersion)
{
	return DX_VERSION_MAJOR;
}

DFI_IMPLEMENT_CDECL(void, gfx_Present, unsigned char *pSurface, int nWidth, int nHeight, unsigned int *pPalette)
{
	//if(pPalette)
	//	g_DirectX.SetPalette(&Palette<UINT>(pPalette, 256));
	//g_DirectX.ShowPage(pSurface, nWidth, nHeight);
	g_DirectX.Present(pSurface, nWidth, nHeight, &Palette<UINT>(pPalette, 256));
}

DFI_IMPLEMENT_CDECL(int, gfx_ScreenShot, const char *szFileName)
{
	if(!g_DirectX.IsScreenShotsActive())
		return FALSE;
	if(szFileName == NULL)
		return FALSE;
	TCHAR buffer[256] = TEXT("");
	StringToString(buffer, 256, szFileName);
	switch(g_DirectX.GetImageFileFormat())
	{
	case D3DXIFF_JPG:
		::_tcscat(buffer, TEXT(".jpg"));
		break;
	case D3DXIFF_BMP:
		::_tcscat(buffer, TEXT(".bmp"));
		break;
	case D3DXIFF_PNG:
		::_tcscat(buffer, TEXT(".png"));
		break;
	case D3DXIFF_DDS:
		::_tcscat(buffer, TEXT(".dds"));
		break;
	default:
		return FALSE;
	}
	if(S_OK != g_DirectX.ScreenShot(buffer))
		return FALSE;
	return TRUE;
}

DFI_IMPLEMENT_CDECL(void, gfx_PumpMessages)
{
	static MSG msg;
	while(::PeekMessage(&msg, 0, 0, 0, PM_REMOVE))
	{
		if(!::IsWindow(g_hWndDlg) || !::IsDialogMessage(g_hWndDlg, &msg))
		{
			::TranslateMessage(&msg);
			::DispatchMessage(&msg);
		}
	}
	g_Joystick.Poll();
	//g_Keyboard.Poll();
}

DFI_IMPLEMENT_CDECL(void, gfx_SetWindowTitle, const char *szTitle)
{
	TCHAR buffer[256] = TEXT("");
	if(szTitle == NULL)
		g_State.szWindowTitle = TEXT("");
	else
	{
		g_State.szWindowTitle = StringToString(buffer, 256, szTitle);
	}
	Tstring szTemp = g_State.szWindowTitle;
	if(g_Mouse.IsClippedCursor() && g_Mouse.IsInputLive())
		szTemp += TEXT(" Press 'Scroll Lock' to free mouse");
	else if(g_Mouse.IsClippedCursor() && !g_Mouse.IsInputLive())
		szTemp += TEXT(" Press 'Scroll Lock' to lock mouse");
	g_Window.SetWindowTitle(szTemp.c_str());
}

DFI_IMPLEMENT_CDECL(const char*, gfx_GetWindowTitle)
{
	char buffer[256] = "";
	return StringToString(buffer, 256, g_State.szWindowTitle.c_str());
}

DFI_IMPLEMENT_CDECL(void, gfx_ShowCursor)
{
	//g_State.bOhrMouseRequest = false;
	g_Mouse.SetCursorVisibility(gfx::Mouse2::CV_SHOW);
	g_Mouse.SetInputState(gfx::Mouse2::IS_DEAD);
}

DFI_IMPLEMENT_CDECL(void, gfx_HideCursor)
{
	//g_State.bOhrMouseRequest = true;
	g_Mouse.SetCursorVisibility(gfx::Mouse2::CV_HIDE);
	g_Mouse.SetInputState(gfx::Mouse2::IS_LIVE);
}

DFI_IMPLEMENT_CDECL(void, gfx_ClipCursor, int left, int top, int right, int bottom)
{
	if(left == -1 && top == -1 && right == -1 && bottom == -1)
		g_Mouse.SetClipState(gfx::Mouse2::CS_OFF);
	else
	{
		RECT r = {left, top, right, bottom};
		g_Mouse.SetClippingRect(&r);
		g_Mouse.SetClipState(gfx::Mouse2::CS_ON);
	}
	char buffer[256] = "";
	gfx_SetWindowTitle(StringToString(buffer, 256, g_State.szWindowTitle.c_str()));
}

DFI_IMPLEMENT_CDECL(int, gfx_GetKeyboard, int *pKeyboard)
{
	if(pKeyboard == NULL)
		return FALSE;
	g_Keyboard.GetOHRScans(pKeyboard);
	return TRUE;
}

DFI_IMPLEMENT_CDECL(int, gfx_GetMouse, int& x, int& y, int& wheel, int& buttons)
{
	x = g_Mouse.GetCursorPos().x;
	y = g_Mouse.GetCursorPos().y;
	wheel = g_Mouse.GetWheel();
	buttons = g_Mouse.GetButtonState().GetData();
	return TRUE;
}

DFI_IMPLEMENT_CDECL(int, gfx_SetMouse, int x, int y)
{
	DWORD xPos, yPos;
	RECT rClient, rDesktop;
	GetClientRect(g_Window.GetWindowHandle(), &rClient);
	POINT pos = {rClient.right * x/320.0f, rClient.bottom * y/200.0f};
	
	ClientToScreen(g_Window.GetWindowHandle(), &pos);
	GetWindowRect(GetDesktopWindow(), &rDesktop);
	
	xPos = 1 + (DWORD)((float)pos.x / (float)rDesktop.right * 65535.0f);
	yPos = 1 + (DWORD)((float)pos.y / (float)rDesktop.bottom * 65535.0f);

	mouse_event(MOUSEEVENTF_ABSOLUTE | MOUSEEVENTF_MOVE, xPos, yPos, 0, NULL);

	return TRUE;
}

DFI_IMPLEMENT_CDECL(int, gfx_GetJoystick, int nDevice, int& x, int& y, int& buttons)
{
	return g_Joystick.GetState(nDevice, buttons, x, y);
}

DFI_IMPLEMENT_CDECL(int, gfx_SetJoystick, int nDevice, int x, int y)
{
	return FALSE; //no support
}

DFI_IMPLEMENT_CDECL(int, gfx_GetJoystickCount)
{
	return g_Joystick.GetJoystickCount();
}

SIZE CalculateNativeResolutionMultiple(UINT width, UINT height, UINT targetWidth, UINT targetHeight)
{
	SIZE ret = {targetWidth, targetHeight};
	UINT targetArea = targetWidth * targetHeight;
	UINT area = width * height;
	UINT i = 1;
	while(area > targetArea * i * i)
		i++;
	if(i != 1)
	{
		float midArea = targetArea * (i-.5f) * (i-.5f);
		if(area < (UINT)midArea)
		{
			ret.cx = targetWidth * (i-1);
			ret.cy = targetHeight * (i-1);
		}
		else
		{
			ret.cx = targetWidth * i;
			ret.cy = targetHeight * i;
		}
	}
	return ret;
}

bool IsNativeResolutionMultiple(UINT width, UINT height, UINT targetWidth, UINT targetHeight)
{
	if(width * height < targetWidth * targetHeight)
		return false;
	UINT i = 1;
	while(width > targetWidth * i)
		i++;
	if(width != targetWidth * i)
		return false;
	if(height != targetHeight * i)
		return false;
	return true;
}

LRESULT CALLBACK OHRWndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	const UINT_PTR ID_MENU_OPTIONS = 101;
	static BOOL bSizing = FALSE;
	
	if(g_Mouse.ProcessMessage(hWnd, msg, wParam, lParam))
		return 0;
	g_Keyboard.ProcessMessage(hWnd, msg, wParam, lParam);

	switch(msg)
	{
	case WM_KEYDOWN:
		{
			switch(wParam)
			{
			case VK_SCROLL:
				{
					if(!(lParam & 0x40000000)) //key was not pressed before
					{
						if(g_Mouse.IsInputLive())
						{
							//g_Mouse.PushState();
							//g_Mouse.SetInputState(gfx::Mouse::IS_DEAD);
							g_Mouse.PushState(gfx::Mouse2::IS_DEAD);
						}
						else
						{
							g_Mouse.PopState();
							char buffer[256] = "";
							gfx_SetWindowTitle(StringToString(buffer, 256, g_State.szWindowTitle.c_str()));
						}
					}
				} break;
			default:
				return ::DefWindowProc(hWnd, msg, wParam, lParam);
			}
		} break;
	case WM_SYSCOMMAND:
		{
			switch(wParam)
			{
			case ID_MENU_OPTIONS:
				{//run dialog
					if(::IsWindow(g_hWndDlg))
						return 0;
					g_hWndDlg = ::CreateDialog(::GetModuleHandle(MODULENAME), TEXT("DialogOptions"), hWnd, (DLGPROC)OHROptionsDlgModeless);
					::ShowWindow(g_hWndDlg, SW_SHOWNORMAL);
					::UpdateWindow(g_hWndDlg);
				} break;
			case SC_KEYMENU:
				{
				} break;
			default:
				{
					return ::DefWindowProc(hWnd, msg, wParam, lParam);
				}
			}
		} break;
	case WM_SYSCHAR:
		{
			switch(wParam)
			{
			case VK_RETURN: //alt-enter
				{
					if(!(lParam & 0x40000000)) //key was not pressed before
					{
						while(IsZoomed(hWnd) || IsIconic(hWnd)) //if maximized or minimized
							ShowWindow(hWnd, SW_RESTORE);
						g_DirectX.SetViewFullscreen(!g_DirectX.IsViewFullscreen());
						g_Mouse.SetVideoMode(g_DirectX.IsViewFullscreen() ? gfx::Mouse2::VM_FULLSCREEN : gfx::Mouse2::VM_WINDOWED);
					}
				} break;
			default:
				return ::DefWindowProc(hWnd, msg, wParam, lParam);
			}
		} break;
	case WM_ACTIVATE:
		{
			if(LOWORD(wParam) == WA_INACTIVE)
			{
				//g_Mouse.PushState();
				//g_Mouse.SetInputState(gfx::Mouse::IS_DEAD);
				//g_Mouse.SetVideoMode(gfx::Mouse::VM_WINDOWED);
				g_Mouse.SetVideoMode(gfx::Mouse2::VM_WINDOWED);
				g_Mouse.PushState(gfx::Mouse2::IS_DEAD);
			}
			else
			{
				g_Mouse.SetVideoMode(g_DirectX.IsViewFullscreen() ? gfx::Mouse2::VM_FULLSCREEN : gfx::Mouse2::VM_WINDOWED);
				g_Mouse.PopState();
			}
			return ::DefWindowProc(hWnd, msg, wParam, lParam);
		} break;
	case WM_MOVE:
		{
			g_Mouse.UpdateClippingRect();
		} break;
	case WM_SIZE:
		{
			bSizing = FALSE;
			::DefWindowProc(hWnd, msg, wParam, lParam);
			if(wParam == SIZE_MINIMIZED)
			{
				//g_Mouse.PushState();
				//g_Mouse.SetInputState(gfx::Mouse::IS_DEAD);
				//g_Mouse.SetVideoMode(gfx::Mouse::VM_WINDOWED);
				g_Mouse.SetVideoMode(gfx::Mouse2::VM_WINDOWED);
				g_Mouse.PushState(gfx::Mouse2::IS_DEAD);
			}
			else
			{
				RECT r = {0,0, LOWORD(lParam), HIWORD(lParam)};
				g_DirectX.SetResolution(&r);
				g_Mouse.SetVideoMode(g_DirectX.IsViewFullscreen() ? gfx::Mouse2::VM_FULLSCREEN : gfx::Mouse2::VM_WINDOWED);
				g_Mouse.UpdateClippingRect();
				g_Mouse.PopState();
			}
		} break;
	case WM_SIZING:
		{
			if(!bSizing)
			{
				bSizing = TRUE;
				g_Mouse.PushState(gfx::Mouse2::IS_DEAD);
			}
			if(!g_DirectX.IsViewFullscreen())
			{
				RECT rWindowTest = {0,0,400,400};
				SIZE sPadding = {0,0};
				::AdjustWindowRectEx(&rWindowTest, WS_OVERLAPPEDWINDOW, FALSE, 0);
				sPadding.cx = rWindowTest.right - rWindowTest.left - 400;
				sPadding.cy = rWindowTest.bottom - rWindowTest.top - 400;

				switch(wParam)
				{
				case WMSZ_BOTTOMLEFT:
					{
						RECT r = *(RECT*)lParam;
						if(!IsNativeResolutionMultiple(r.right - r.left - sPadding.cx, r.bottom - r.top - sPadding.cy))
						{
							SIZE resolution = CalculateNativeResolutionMultiple(r.right - r.left - sPadding.cx, r.bottom - r.top - sPadding.cy);
							r.bottom = r.top + (resolution.cy + sPadding.cy);
							r.left = r.right - (resolution.cx + sPadding.cx);
							*(RECT*)lParam = r;
						}
					} break;
				case WMSZ_BOTTOMRIGHT:
					{
						RECT r = *(RECT*)lParam;
						if(!IsNativeResolutionMultiple(r.right - r.left - sPadding.cx, r.bottom - r.top - sPadding.cy))
						{
							SIZE resolution = CalculateNativeResolutionMultiple(r.right - r.left - sPadding.cx, r.bottom - r.top - sPadding.cy);
							r.bottom = r.top + (resolution.cy + sPadding.cy);
							r.right = r.left + (resolution.cx + sPadding.cx);
							*(RECT*)lParam = r;
						}
					} break;
				case WMSZ_TOPLEFT:
					{
						RECT r = *(RECT*)lParam;
						if(!IsNativeResolutionMultiple(r.right - r.left - sPadding.cx, r.bottom - r.top - sPadding.cy))
						{
							SIZE resolution = CalculateNativeResolutionMultiple(r.right - r.left - sPadding.cx, r.bottom - r.top - sPadding.cy);
							r.top = r.bottom - (resolution.cy + sPadding.cy);
							r.left = r.right - (resolution.cx + sPadding.cx);
							*(RECT*)lParam = r;
						}
					} break;
				case WMSZ_TOPRIGHT:
					{
						RECT r = *(RECT*)lParam;
						if(!IsNativeResolutionMultiple(r.right - r.left - sPadding.cx, r.bottom - r.top - sPadding.cy))
						{
							SIZE resolution = CalculateNativeResolutionMultiple(r.right - r.left - sPadding.cx, r.bottom - r.top - sPadding.cy);
							r.top = r.bottom - (resolution.cy + sPadding.cy);
							r.right = r.left + (resolution.cx + sPadding.cx);
							*(RECT*)lParam = r;
						}
					} break;
				}
			}
			::DefWindowProc(hWnd, msg, wParam, lParam);
		} break;
	case WM_POWERBROADCAST:
		{
			switch(wParam)
			{
			case PBT_APMRESUMESUSPEND:
			case PBT_APMRESUMECRITICAL:
			case PBT_APMRESUMEAUTOMATIC:
				{
					g_DirectX.Initialize(&g_Window, MODULENAME);
					g_Joystick.Initialize(g_Window.GetAppHandle(), g_Window.GetWindowHandle());
					g_Mouse.SetVideoMode(gfx::Mouse2::VM_WINDOWED);
					g_Mouse.UpdateClippingRect();
				} break;
			case PBT_APMSUSPEND:
				{
					g_DirectX.SetViewFullscreen(FALSE);
					g_DirectX.Shutdown();
					g_Joystick.Shutdown();
				} break;
			default:
				return ::DefWindowProc(hWnd, msg, wParam, lParam);
			}
			return TRUE;
		} break;
	case WM_CLOSE:
		{
			if(g_State.bClosing)
				return ::DefWindowProc(hWnd, msg, wParam, lParam);
			//g_Mouse.PushState();
			//g_Mouse.SetInputState(gfx::Mouse::IS_DEAD);
			//g_Mouse.SetVideoMode(gfx::Mouse::VM_WINDOWED);
			g_Mouse.SetVideoMode(gfx::Mouse2::VM_WINDOWED);
			g_Mouse.SetInputState(gfx::Mouse2::IS_DEAD);
			g_State.PostTerminateSignal();
			g_Mouse.SetVideoMode(g_DirectX.IsViewFullscreen() ? gfx::Mouse2::VM_FULLSCREEN : gfx::Mouse2::VM_WINDOWED);
			g_Mouse.PopState();
		} break;
	case WM_CREATE:
		{
			HMENU hSysMenu = ::GetSystemMenu(hWnd, false);
			::InsertMenu(hSysMenu, 0, MF_BYPOSITION | MF_STRING, ID_MENU_OPTIONS, TEXT("Options"));
			::InsertMenu(hSysMenu, 1, MF_BYPOSITION | MF_SEPARATOR, 0, 0);
		}//intentionally fall-through
	default:
		return ::DefWindowProc(hWnd, msg, wParam, lParam);
	}
	return 0;
}

BOOL CALLBACK OHROptionsDlgModeless(HWND hWndDlg, UINT msg, WPARAM wParam, LPARAM lParam)
{
	static BOOL bVsyncEnabled, bSmoothEnabled, bARPEnabled;

	switch(msg)
	{
	case WM_COMMAND:
		{
			switch(LOWORD(wParam))
			{//control id
			case IDC_OPTIONS_EnableVsync:
				{
					g_DirectX.SetVsyncEnabled(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnableVsync));
				} break;
			case IDC_OPTIONS_EnableSmooth:
				{
					g_DirectX.SetSmooth(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnableSmooth));
				} break;
			case IDC_OPTIONS_EnablePreserveAspectRatio:
				{
					g_DirectX.SetAspectRatioPreservation(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnablePreserveAspectRatio));
				} break;
			case IDC_OPTIONS_SetDefaults:
				{//sets defaults
					::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnableVsync, BST_CHECKED);
					::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnableSmooth, BST_UNCHECKED);
					::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnablePreserveAspectRatio, BST_CHECKED);

					g_DirectX.SetVsyncEnabled(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnableVsync));
					g_DirectX.SetSmooth(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnableSmooth));
					g_DirectX.SetAspectRatioPreservation(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnablePreserveAspectRatio));

					if(g_DirectX.IsScreenShotsActive())
					{
						::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_JPG, BST_UNCHECKED);
						::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_BMP, BST_UNCHECKED);
						::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_PNG, BST_CHECKED);
						::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_DDS, BST_UNCHECKED);
						::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_OHR, BST_UNCHECKED);
					}
					return TRUE;
				} break;
			case IDC_OPTIONS_RefreshJoysticks:
				{
					g_Joystick.RefreshEnumeration();
					TCHAR strInfoBuffer[128] = TEXT("");
					::_stprintf(strInfoBuffer, TEXT("Refresh Joysticks - Count: %d"), g_Joystick.GetJoystickCount());
					::SendDlgItemMessage(hWndDlg, IDC_OPTIONS_RefreshJoysticks, WM_SETTEXT, 0, (LPARAM)strInfoBuffer);
				} break;
			case IDOK:
				{//apply all changes and return
					g_DirectX.SetVsyncEnabled(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnableVsync));
					g_DirectX.SetSmooth(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnableSmooth));
					g_DirectX.SetAspectRatioPreservation(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnablePreserveAspectRatio));
					if(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_ScrnShotFormats_JPG))
						g_DirectX.SetImageFileFormat(D3DXIFF_JPG);
					else if(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_ScrnShotFormats_BMP))
						g_DirectX.SetImageFileFormat(D3DXIFF_BMP);
					else if(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_ScrnShotFormats_PNG))
						g_DirectX.SetImageFileFormat(D3DXIFF_PNG);
					else if(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_ScrnShotFormats_DDS))
						g_DirectX.SetImageFileFormat(D3DXIFF_DDS);
					else
						g_DirectX.SetImageFileFormat(D3DXIFF_FORCE_DWORD);
					g_Mouse.PopState();
					::DestroyWindow(hWndDlg);
					g_hWndDlg = NULL;
				} break;
			case IDCANCEL:
				{//revert all changes and return
					g_DirectX.SetVsyncEnabled(bVsyncEnabled);
					g_DirectX.SetSmooth(bSmoothEnabled);
					g_DirectX.SetAspectRatioPreservation(bARPEnabled);
					g_Mouse.PopState();
					::DestroyWindow(hWndDlg);
					g_hWndDlg = NULL;
				} break;
			default:
				return FALSE;
			}
		} break;
	case WM_INITDIALOG:
		{
			//g_Mouse.PushState();
			//g_Mouse.SetVideoMode(gfx::Mouse::VM_WINDOWED);
			//g_Mouse.SetInputState(gfx::Mouse::IS_DEAD);
			g_Mouse.PushState(gfx::Mouse2::IS_DEAD);

			bVsyncEnabled = g_DirectX.IsVsyncEnabled() ? TRUE : FALSE;
			bSmoothEnabled = g_DirectX.IsSmooth() ? TRUE : FALSE;
			bARPEnabled = g_DirectX.IsAspectRatioPreserved() ? TRUE : FALSE;

			::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnableVsync, (g_DirectX.IsVsyncEnabled() ? BST_CHECKED : BST_UNCHECKED));
			::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnableSmooth, (g_DirectX.IsSmooth() ? BST_CHECKED : BST_UNCHECKED));
			::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnablePreserveAspectRatio, (g_DirectX.IsAspectRatioPreserved() ? BST_CHECKED : BST_UNCHECKED));
			::SendDlgItemMessage(hWndDlg, IDC_OPTIONS_Status, WM_SETTEXT, 0, (LPARAM)g_State.szHelpText.c_str());
			TCHAR strInfoBuffer[128] = TEXT("");
			::_stprintf(strInfoBuffer, TEXT("DirectX Backend version: %d.%d.%d\r\nhttp://www.hamsterrepublic.com"), DX_VERSION_MAJOR, DX_VERSION_MINOR, DX_VERSION_BUILD);
			::SendDlgItemMessage(hWndDlg, IDC_OPTIONS_Info, WM_SETTEXT, 0, (LPARAM)strInfoBuffer);
			::_stprintf(strInfoBuffer, TEXT("Refresh Joysticks - Count: %d"), g_Joystick.GetJoystickCount());
			::SendDlgItemMessage(hWndDlg, IDC_OPTIONS_RefreshJoysticks, WM_SETTEXT, 0, (LPARAM)strInfoBuffer);
			if(g_DirectX.IsScreenShotsActive())
			{
				switch(g_DirectX.GetImageFileFormat())
				{
				case D3DXIFF_JPG:
					::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_JPG, BST_CHECKED);
					break;
				case D3DXIFF_BMP:
					::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_BMP, BST_CHECKED);
					break;
				case D3DXIFF_PNG:
					::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_PNG, BST_CHECKED);
					break;
				case D3DXIFF_DDS:
					::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_DDS, BST_CHECKED);
					break;
				default:
					::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_OHR, BST_CHECKED);
				}
			}
			else
			{
				::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_OHR, BST_CHECKED);
				::EnableWindow(::GetDlgItem(hWndDlg, IDC_OPTIONS_ScrnShotFormats_JPG), FALSE);
				::EnableWindow(::GetDlgItem(hWndDlg, IDC_OPTIONS_ScrnShotFormats_BMP), FALSE);
				::EnableWindow(::GetDlgItem(hWndDlg, IDC_OPTIONS_ScrnShotFormats_PNG), FALSE);
				::EnableWindow(::GetDlgItem(hWndDlg, IDC_OPTIONS_ScrnShotFormats_DDS), FALSE);
				::EnableWindow(::GetDlgItem(hWndDlg, IDC_OPTIONS_ScrnShotFormats_OHR), FALSE);
			}
		} break;
	default:
		return FALSE;
	}
	return TRUE;
}
