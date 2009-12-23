#include "gfx_directx_TESTAPPconst.h"
#if !TESTAPP
#define ISOLATION_AWARE_ENABLED  1
#endif
#include "gfx_directx.h"
#include "gfx_directx_cls_window.h"
#include "gfx_directx_cls.h"
#include "gfx_directx_cls_dinput.h"
#include "gfx_directx_cls_hpcounter.h"
#include "gfx_directx_cls_osmouse.h"
using namespace gfx;
#include "di2fb_scancodes.h"
#include "resource.h"

#define DX_BUILD_DIRECTX_DYNAMIC FALSE //can only be TRUE or FALSE, or 1 or 0 respectively
#define DX_BUILD_MSVC_DYNAMIC FALSE //can only be TRUE or FALSE, or 1 or 0 respectively
#define DX_VERSION_MAJOR 0x1
#define DX_VERSION_MINOR 0x2
#define DX_VERSION_BUILD ((DX_BUILD_MSVC_DYNAMIC << 1) + DX_BUILD_DIRECTX_DYNAMIC)
#define DX_BACKEND_VERSION ((DX_VERSION_MAJOR << 16) + (DX_VERSION_MINOR << 8) + DX_VERSION_BUILD)

#pragma comment(lib, "d3d9.lib")
#pragma comment(lib, "d3dx9.lib")
#pragma comment(lib, "dinput8.lib")
#pragma comment(lib, "dxguid.lib")

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

//resource loading from module
#if TESTAPP
#define MODULENAME NULL
#else
#define MODULENAME TEXT("gfx_directx.dll")
#endif //TESTAPP

Window g_Window;
HWND g_hWndDlg;
DirectX g_DirectX;
DirectInput g_DirectInput;
HPCounter g_Time;
OSMouse g_OSMouse;
unsigned char *g_pRaw;
SIZE g_pRawSize;

struct gfx_BackendState
{
	tstring strWindowTitle;
	bool bClosing; //flagged when shutting down
	bool bBlockOhrMouseInput; //toggles ohr mouse control
	WindowState winState;
	void (__cdecl *post_terminate_signal)(void);
} g_State;

LRESULT CALLBACK OHRGameWndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
//INT_PTR CALLBACK OHRGameOptionsDlg(HWND hWndDlg, UINT msg, WPARAM wParam, LPARAM lParam);
BOOL CALLBACK OHRGameOptionsDlgModeless(HWND hWndDlg, UINT msg, WPARAM wParam, LPARAM lParam);
void PumpMessages();

int gfx_init(void (__cdecl *terminate_signal_handler)(void) , const char* windowicon, char* info_buffer, int info_buffer_size)
{
	g_State.post_terminate_signal = terminate_signal_handler;
	TCHAR szWindowIconRsrc[256] = TEXT("");
	if(windowicon != NULL)
#ifdef _UNICODE
		::MultiByteToWideChar(CP_UTF8, 0, windowicon, ::strlen(windowicon), szWindowIconRsrc, 256);
#else
		::strcpy_s<256>(szWindowIconRsrc, windowicon);
#endif
	if(S_OK != g_Window.Initialize(::GetModuleHandle(MODULENAME), (windowicon ? szWindowIconRsrc : NULL), (WNDPROC)OHRGameWndProc))
		return FALSE;
	if(DX_OK != g_DirectX.Initialize(&g_Window, MODULENAME, false))
		return FALSE;
	g_Window.SetClientSize(960, 600);
	g_Window.SetWindowTitle(TEXT("DirectX Backend"));
	g_Window.CenterWindow();
	g_OSMouse.Initialize(g_Window.GetWindowHandle());
	TEST_ONLY_BLOCK(::MessageBox(0, g_DirectX.GetLastErrorMessage(), TEXT("Debug Initialize"), MB_OK););
	if(S_OK != g_DirectInput.Initialize(&g_Window))
		return FALSE;
	io_mouserect(-1,-1,-1,-1);
	return TRUE;
}

void gfx_close()
{
#if TESTAPP
	::MessageBox(0, g_DirectX.GetLastErrorMessage(), TEXT("Debug Close"), MB_OK);
#endif
	g_State.bClosing = true;
	g_DirectX.Shutdown();
	g_Window.Shutdown(0);
	while(g_Window.PumpMessages() == S_OK);
}

int gfx_getversion()
{
	return DX_VERSION_MAJOR;
}

void gfx_showpage(unsigned char *raw, int w, int h)
{
	//g_Time.Update();

	g_pRaw = raw;
	g_pRawSize.cx = w;
	g_pRawSize.cy = h;
	if(DX_OK != g_DirectX.ShowPage(raw, w, h))
		for(int i = 1; i < 10; i++)
			::Beep(250 * i, 10);

	//g_Window.PumpMessages();
	PumpMessages();
//#if TESTAPP
//	if(g_DirectX.IsVsyncEnabled())
//#endif
//		while(g_Time.GetTimeDifferential() < 6)
//			//g_Window.PumpMessages();
//			PumpMessages();
	//g_Time.Update();
}

void gfx_setpal(unsigned int *pal)
{
	g_DirectX.SetPalette(&Palette<UINT>(pal, 256)); //this is where the palette size is determined
	gfx_showpage(g_pRaw, g_pRawSize.cx, g_pRawSize.cy); //necessary because allmodex assumes the screen is updated here, which is a bad assumption
}

int gfx_screenshot(const char* fname)
{
	if(g_DirectX.GetImageFileFormat() == D3DXIFF_FORCE_DWORD) //unused value means ohr will handle the screen shot
		return FALSE;
	TCHAR buffer[256] = TEXT("");
	if(fname == NULL)
		return FALSE;
	int nLength = ::strlen(fname);
	D3DXIMAGE_FILEFORMAT format = g_DirectX.GetImageFileFormat();
#ifdef _UNICODE
	::MultiByteToWideChar(CP_UTF8, 0, fname, nLength, buffer, 256);
#else
	::strcpy_s<256>(buffer, fname);
#endif
	if(format == D3DXIFF_JPG)
		::_tcscat_s<256>(buffer, TEXT(".jpg"));
	else if(format == D3DXIFF_BMP)
		::_tcscat_s<256>(buffer, TEXT(".bmp"));
	else if(format == D3DXIFF_PNG)
		::_tcscat_s<256>(buffer, TEXT(".png"));
	else if(format == D3DXIFF_DDS)
		::_tcscat_s<256>(buffer, TEXT(".dds"));
	else if(format == D3DXIFF_HDR)
		::_tcscat_s<256>(buffer, TEXT(".hdr"));
	else if(format == D3DXIFF_DIB)
		::_tcscat_s<256>(buffer, TEXT(".dib"));
	else if(format == D3DXIFF_PFM)
		::_tcscat_s<256>(buffer, TEXT(".pfm"));
	if(DX_OK == g_DirectX.ScreenShot(buffer))
		return TRUE;
	return FALSE;
}

void gfx_setwindowed(int iswindow)
{
	g_DirectX.SetView(iswindow != FALSE);
	g_OSMouse.Fullscreen(g_DirectX.IsViewFullscreen());
}

void gfx_windowtitle(const char* title)
{
	TCHAR buffer[256] = TEXT("");
	if(title == NULL)
		return;
#ifdef _UNICODE
	::MultiByteToWideChar(CP_UTF8, 0, title, ::strlen(title), buffer, 256);
#else
	::strcpy_s<256>(buffer, title);
#endif
	g_State.strWindowTitle = buffer;
	tstring strTitle = g_State.strWindowTitle;
	if(!g_State.bBlockOhrMouseInput && g_OSMouse.IsOHRMouseActive())
		strTitle += TEXT(" Press 'Scroll Lock' to free mouse");
	else if(g_State.bBlockOhrMouseInput && g_OSMouse.IsOHRMouseActive())
		strTitle += TEXT(" Press 'Scroll Lock' to lock mouse");
	g_Window.SetWindowTitle(strTitle.c_str());
}

WindowState* gfx_getwindowstate()
{
	return &g_State.winState;
}

int gfx_setoption(const char* opt, const char* arg)
{
	if(!opt || !arg)
		return 0;
	if(::strcmp(opt, "w") == 0 || ::strcmp(opt, "width") == 0)
	{
		g_Window.SetClientSize(::atoi(arg), g_Window.GetClientSize().cy);
	}
	else if(::strcmp(opt, "h") == 0 || ::strcmp(opt, "height") == 0)
	{
		g_Window.SetClientSize(g_Window.GetClientSize().cx, ::atoi(arg));
	}
	else if(::strcmp(opt, "f") == 0 || ::strcmp(opt, "fullscreen") == 0)
	{
		if(*arg == '0')
			g_DirectX.SetView(true);
		else
			g_DirectX.SetView(false);
	}
	else if(::strcmp(opt, "fps") == 0)
	{
		if(*arg == '0')
			g_DirectX.SetFps(false);
		else
			g_DirectX.SetFps(true);
	}
	else if(::strcmp(opt, "v") == 0 || ::strcmp(opt, "vsync") == 0)
	{
		if(*arg == '0')
			g_DirectX.SetVSync(false);
		else
			g_DirectX.SetVSync(true);
	}
	else if(::strcmp(opt, "a") == 0 || ::strcmp(opt, "aspect") == 0)
	{
		if(*arg == '0')
			g_DirectX.SetAspectRatioPreservation(false);
		else
			g_DirectX.SetAspectRatioPreservation(true);
	}
	else if(::strcmp(opt, "s") == 0 || ::strcmp(opt, "smooth") == 0)
	{
		if(*arg == '0')
			g_DirectX.SetSmooth(false);
		else
			g_DirectX.SetSmooth(true);
	}
	else if(::strcmp(opt, "ss") == 0 || ::strcmp(opt, "screenshot") == 0)
	{
		if(*arg == 'j')
			g_DirectX.SetImageFileFormat(D3DXIFF_JPG);
		else if(*arg == 'b')
			g_DirectX.SetImageFileFormat(D3DXIFF_BMP);
		else if(*arg == 'p')
			g_DirectX.SetImageFileFormat(D3DXIFF_PNG);
		else if(*arg == 'd')
			g_DirectX.SetImageFileFormat(D3DXIFF_DDS);
		else
			g_DirectX.SetImageFileFormat(D3DXIFF_FORCE_DWORD);
	}
	else
		return 0;
	return 2;
}

const char* gfx_describe_options()
{
	return "-w -width [x]  sets the width of the client area\n" \
		"-h -height [x]  sets the height of the client area\n" \
		"-f -fullscreen [0* | 1]  toggles fullscreen on startup\n" \
		"    the above may NOT be called before width and height\n" \
		"-fps [0* | 1]  toggles the fps display\n" \
		"-v -vsync [0 | 1*]  toggles vsync\n" \
		"-a -aspect [0 | 1*]  toggles aspect ratio preservation\n" \
		"-s -smooth [0* | 1]  toggles smooth linear interpolation of display\n" \
		"-ss -screenshot [jpg* | bmp | png | dds | ohr]\n" \
		"     the above sets the screen shot format";
}

void io_init()
{
}

void io_pollkeyevents()
{
	g_DirectInput.PollKeyboard();
	if(!g_State.bBlockOhrMouseInput)
		g_DirectInput.PollMouse();
}

//void io_waitprocessing()
//{
//	PumpMessages();
//}

//void io_keybits(int* keybdarray)
//{//? why is this function here?
//	io_updatekeys(keybdarray);
//}

void io_updatekeys(int *keybd)
{
	io_pollkeyevents();
	BYTE *pKeys = g_DirectInput.GetKeyboardState();
	if(pKeys == NULL)
		return;
	for(UINT  i = 0; i < 256; i++)
		if(pKeys[i] & 0x80)
			keybd[di2fb(i)] |= 8; //di2fb() converts di scancode to fb
}

//void io_mousebits(int& mx, int& my, int& mwheel, int& mbuttons, int& mclicks)
//{//? why is this function here?
//	io_getmouse(mx, my, mwheel, mbuttons);
//}

int io_setmousevisibility(int visible)
{
	g_OSMouse.OHRMouseActive(visible == FALSE);
	char szTitle[256] = "";
#ifdef _UNICODE
	::WideCharToMultiByte(CP_UTF8, 0, g_State.strWindowTitle.c_str(), g_State.strWindowTitle.length(), szTitle, sizeof(szTitle), 0, 0);
#else
	::strcpy_s<256>(szTitle, g_State.strWindowTitle.c_str());
#endif
	gfx_windowtitle(szTitle);
	return 1;
}

void io_getmouse(int& mx, int& my, int& mwheel, int& mbuttons)
{
	mx = g_DirectInput.GetMouseX();
	my = g_DirectInput.GetMouseY();
	mwheel = g_DirectInput.GetMouseWheel();
	mbuttons = 0;
	mbuttons |= (g_DirectInput.IsMouseLButtonDown() ? 0x1 : 0);
	mbuttons |= (g_DirectInput.IsMouseRButtonDown() ? 0x2 : 0);
	mbuttons |= (g_DirectInput.IsMouseMButtonDown() ? 0x4 : 0);
}

void io_setmouse(int x, int y)
{
	g_DirectInput.SetMouseX(x);
	g_DirectInput.SetMouseY(y);
}

void io_mouserect(int xmin, int xmax, int ymin, int ymax)
{
	if(xmin == -1 && xmax == -1 && ymin == -1 && ymax == -1)
	{
		//if(!g_State.bBlockOhrMouseInput)
		//{
		//	g_State.bBlockOhrMouseInput = true;
		//	g_OSMouse.Push_State();
		//}
		g_DirectInput.ClipMouseMovement(0, 0, 319, 199);
	}
	else
	{
		//if(g_State.bBlockOhrMouseInput)
		//{
		//	g_State.bBlockOhrMouseInput = false;
		//	g_OSMouse.Pop_State();
		//}
		g_DirectInput.ClipMouseMovement(xmin, ymin, xmax, ymax);
	}
}

int io_readjoysane(int, int&, int&, int&)
{
	return 0;
}

void PumpMessages()
{
	MSG msg;
	while(::PeekMessage(&msg, 0, 0, 0, PM_REMOVE))
	{
		if(!::IsWindow(g_hWndDlg) || !::IsDialogMessage(g_hWndDlg, &msg))
		{
			::TranslateMessage(&msg);
			::DispatchMessage(&msg);
		}
	}
}

LRESULT CALLBACK OHRGameWndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
{
	const UINT_PTR ID_MENU_OPTIONS = 101;
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
						g_State.bBlockOhrMouseInput = !g_State.bBlockOhrMouseInput;
						if(g_State.bBlockOhrMouseInput)
							g_OSMouse.Push_State();
						else
							g_OSMouse.Pop_State();
						char szTitle[256] = "";
#ifdef _UNICODE
						::WideCharToMultiByte(CP_UTF8, 0, g_State.strWindowTitle.c_str(), g_State.strWindowTitle.length(), szTitle, sizeof(szTitle), 0, 0);
#else
						::strcpy_s<256>(szTitle, g_State.strWindowTitle.c_str());
#endif
						gfx_windowtitle(szTitle);
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
					//g_OSMouse.Push_State();
					//::DialogBox(::GetModuleHandle(MODULENAME), TEXT("DialogOptions"), hWnd, (DLGPROC)OHRGameOptionsDlg);
					g_hWndDlg = ::CreateDialog(::GetModuleHandle(MODULENAME), TEXT("DialogOptions"), hWnd, (DLGPROC)OHRGameOptionsDlgModeless);
					::ShowWindow(g_hWndDlg, SW_SHOWNORMAL);
					::UpdateWindow(g_hWndDlg);
					//g_OSMouse.Pop_State();
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
						gfx_setwindowed(g_DirectX.IsViewFullscreen() ? TRUE : FALSE);
				} break;
			default:
				return ::DefWindowProc(hWnd, msg, wParam, lParam);
			}
		} break;
TEST_ONLY_BLOCK(
	case WM_CHAR:
		{
			if(wParam == 'D' || wParam == 'd')
				::MessageBox(0, g_DirectX.GetCapsMessage(), TEXT("Debug D3D9 Capabilities"), MB_OK);
			else if(wParam == 'L' || wParam == 'l')
				::MessageBox(0, g_DirectX.GetLastErrorMessage(), TEXT("Debug D3D9 Last Error"), MB_OK);
			return ::DefWindowProc(hWnd, msg, wParam, lParam);
		} break;
	); //TEST_ONLY_BLOCK
	case WM_ACTIVATE:
		{
			if(LOWORD(wParam) == WA_INACTIVE)
			{
				g_State.winState.focused = FALSE;
				g_OSMouse.Push_State();
			}
			else
			{
				g_State.winState.focused = TRUE;
				g_OSMouse.Pop_State();
			}
			return ::DefWindowProc(hWnd, msg, wParam, lParam);
		} break;
	case WM_SIZE:
		{
			LRESULT ret = ::DefWindowProc(hWnd, msg, wParam, lParam);
			if(wParam == SIZE_MINIMIZED)
			{
				g_OSMouse.Push_State();
				return ret;
			}
			g_DirectX.SetResolution(LOWORD(lParam), HIWORD(lParam));
			g_OSMouse.Pop_State();
			return ret;
		} break;
	case WM_CLOSE:
		{
			if(g_State.bClosing)
				return ::DefWindowProc(hWnd, msg, wParam, lParam);
			g_OSMouse.Push_State();
			if(!g_State.post_terminate_signal)
				::MessageBox(0, TEXT("OHR Engine improperly notified backend of termination sequence. Ignoring Close"), TEXT("Error"), MB_OK | MB_ICONEXCLAMATION);
			else //if(IDYES == ::MessageBox(0, TEXT("Are you sure you want to quit?"), TEXT("Close"), MB_YESNO))
				g_State.post_terminate_signal();
			g_OSMouse.Pop_State();
		} break;
	//case WM_DESTROY: //not necessary anymore
	//	{
	//		if(g_State.post_terminate_signal)
	//			g_State.post_terminate_signal();
	//	} break;
	case WM_CREATE:
		{
			HMENU hSysMenu = ::GetSystemMenu(hWnd, false);
			::InsertMenu(hSysMenu, 0, MF_BYPOSITION | MF_STRING, ID_MENU_OPTIONS, TEXT("Options"));
			::InsertMenu(hSysMenu, 1, MF_BYPOSITION | MF_SEPARATOR, 0, 0);
			g_State.winState.focused = TRUE;
			g_State.winState.minimised = FALSE;
		}//intentionally fall-through
	default:
		return ::DefWindowProc(hWnd, msg, wParam, lParam);
	}
	return 0;
}

//this function supersceded by OHRGameOptionsDlgModeless(), with modeless dialog box instead of modal
//INT_PTR CALLBACK OHRGameOptionsDlg(HWND hWndDlg, UINT msg, WPARAM wParam, LPARAM lParam)
//{
//	switch(msg)
//	{
//	case WM_COMMAND:
//		{
//			switch(LOWORD(wParam))
//			{//control id
//			case IDC_OPTIONS_SetDefaults:
//				{//sets defaults
//					::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnableFps, BST_UNCHECKED);
//					::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnableVsync, BST_CHECKED);
//					::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnableSmooth, BST_UNCHECKED);
//					::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnablePreserveAspectRatio, BST_CHECKED);
//					::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_JPG, BST_CHECKED);
//					::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_BMP, BST_UNCHECKED);
//					::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_PNG, BST_UNCHECKED);
//					::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_DDS, BST_UNCHECKED);
//					::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_OHR, BST_UNCHECKED);
//				} break;
//			case IDOK:
//				{//apply all changes and return
//					g_OSMouse.Pop_State();
//					g_DirectX.SetFps(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnableFps));
//					g_DirectX.SetVSync(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnableVsync));
//					g_DirectX.SetSmooth(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnableSmooth));
//					g_DirectX.SetAspectRatioPreservation(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnablePreserveAspectRatio));
//					if(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_ScrnShotFormats_JPG))
//						g_DirectX.SetImageFileFormat(D3DXIFF_JPG);
//					else if(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_ScrnShotFormats_BMP))
//						g_DirectX.SetImageFileFormat(D3DXIFF_BMP);
//					else if(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_ScrnShotFormats_PNG))
//						g_DirectX.SetImageFileFormat(D3DXIFF_PNG);
//					else if(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_ScrnShotFormats_DDS))
//						g_DirectX.SetImageFileFormat(D3DXIFF_DDS);
//					else
//						g_DirectX.SetImageFileFormat(D3DXIFF_FORCE_DWORD);
//					::EndDialog(hWndDlg, 0);
//				} break;
//			case IDCANCEL:
//				{//ignore all changes and return
//					g_OSMouse.Pop_State();
//					::EndDialog(hWndDlg, 0);
//				} break;
//			}
//		} break;
//	case WM_INITDIALOG:
//		{
//			g_OSMouse.Push_State();
//			::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnableFps, (g_DirectX.IsFpsEnabled() ? BST_CHECKED : BST_UNCHECKED));
//			::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnableVsync, (g_DirectX.IsVsyncEnabled() ? BST_CHECKED : BST_UNCHECKED));
//			::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnableSmooth, (g_DirectX.IsSmooth() ? BST_CHECKED : BST_UNCHECKED));
//			::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnablePreserveAspectRatio, (g_DirectX.IsAspectRatioPreserved() ? BST_CHECKED : BST_UNCHECKED));
//			::SendDlgItemMessage(hWndDlg, IDC_OPTIONS_Status, WM_SETTEXT, 0, (LPARAM)g_DirectX.GetLastErrorMessage());
//			TCHAR strInfoBuffer[128] = TEXT("");
//			::_stprintf_s<128>(strInfoBuffer, TEXT("DirectX Backend version: %d.%d.%d\r\nhttp://www.hamsterrepublic.com"), DX_VERSION_MAJOR, DX_VERSION_MINOR, DX_VERSION_BUILD);
//			::SendDlgItemMessage(hWndDlg, IDC_OPTIONS_Info, WM_SETTEXT, 0, (LPARAM)strInfoBuffer);
//			switch(g_DirectX.GetImageFileFormat())
//			{
//			case D3DXIFF_JPG:
//				::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_JPG, BST_CHECKED);
//				break;
//			case D3DXIFF_BMP:
//				::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_BMP, BST_CHECKED);
//				break;
//			case D3DXIFF_PNG:
//				::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_PNG, BST_CHECKED);
//				break;
//			case D3DXIFF_DDS:
//				::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_DDS, BST_CHECKED);
//				break;
//			default:
//				::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_OHR, BST_CHECKED);
//			}
//		} break;
//	default:
//		return FALSE;
//	}
//	return TRUE;
//}

BOOL CALLBACK OHRGameOptionsDlgModeless(HWND hWndDlg, UINT msg, WPARAM wParam, LPARAM lParam)
{
	switch(msg)
	{
	case WM_COMMAND:
		{
			switch(LOWORD(wParam))
			{//control id
			case IDC_OPTIONS_SetDefaults:
				{//sets defaults
					::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnableFps, BST_UNCHECKED);
					::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnableVsync, BST_CHECKED);
					::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnableSmooth, BST_UNCHECKED);
					::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnablePreserveAspectRatio, BST_CHECKED);
					::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_JPG, BST_CHECKED);
					::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_BMP, BST_UNCHECKED);
					::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_PNG, BST_UNCHECKED);
					::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_DDS, BST_UNCHECKED);
					::CheckDlgButton(hWndDlg, IDC_OPTIONS_ScrnShotFormats_OHR, BST_UNCHECKED);
					return TRUE;
				} break;
			case IDOK:
				{//apply all changes and return
					g_DirectX.SetFps(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnableFps));
					g_DirectX.SetVSync(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnableVsync));
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
				} //fallthrough on purpose
			case IDCANCEL:
				{//ignore all changes and return
					g_OSMouse.Pop_State();
					::DestroyWindow(hWndDlg);
					g_hWndDlg = NULL;
				} break;
			default:
				return FALSE;
			}
		} break;
	case WM_INITDIALOG:
		{
			g_OSMouse.Push_State();
			::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnableFps, (g_DirectX.IsFpsEnabled() ? BST_CHECKED : BST_UNCHECKED));
			::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnableVsync, (g_DirectX.IsVsyncEnabled() ? BST_CHECKED : BST_UNCHECKED));
			::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnableSmooth, (g_DirectX.IsSmooth() ? BST_CHECKED : BST_UNCHECKED));
			::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnablePreserveAspectRatio, (g_DirectX.IsAspectRatioPreserved() ? BST_CHECKED : BST_UNCHECKED));
			::SendDlgItemMessage(hWndDlg, IDC_OPTIONS_Status, WM_SETTEXT, 0, (LPARAM)g_DirectX.GetLastErrorMessage());
			TCHAR strInfoBuffer[128] = TEXT("");
			::_stprintf_s<128>(strInfoBuffer, TEXT("DirectX Backend version: %d.%d.%d\r\nhttp://www.hamsterrepublic.com"), DX_VERSION_MAJOR, DX_VERSION_MINOR, DX_VERSION_BUILD);
			::SendDlgItemMessage(hWndDlg, IDC_OPTIONS_Info, WM_SETTEXT, 0, (LPARAM)strInfoBuffer);
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
		} break;
	default:
		return FALSE;
	}
	return TRUE;
}