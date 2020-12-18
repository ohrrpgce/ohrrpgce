#define ISOLATION_AWARE_ENABLED 1

#include "../config.h"
#include "../gfx.h"
#include "debugmsg.hpp"
#include "window.hpp"
#include "d3d.hpp"
#include "keyboard.hpp"
#include "mouse.hpp"
#include "joystick.hpp"
#include "version.h"
#include "util.hpp"
#include "../os.h"
#include "../lib/SDL/SDL_windowsclipboard.h"
#include <dbt.h>

using namespace gfx;

#include "resource.h"

#define MODULENAME TEXT("gfx_directx.dll")

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

bool input_debug = false;   // --input-debug flag

// Note: g_DirectX is responsible for all the important state of the
// backend, such as the current fullscreen state, window size, resolution,
// not g_Window.
struct gfx_BackendState
{
	Tstring szWindowTitle;
	Tstring szWindowIcon;
	void (__cdecl *DebugMsg)(ErrorLevel errlvl, const char* szMessage);
	FnEventHandler PostEvent;  //Send a message to the engine
	bool bClosing; //flagged when shutting down
	Tstring szHelpText;
	BOOL bDisableSysMsg;
} g_State;

void DefaultDebugMsg(ErrorLevel errlvl, const char* szMessage) {
	//MessageBoxA(NULL, szMessage, "Debug Message", MB_OK);
}

// For informative messages use errInfo
void ::_throw_error(ErrorLevel errlvl, const char *srcfile, int linenum, const char* szMessage, ...)
{
	if (g_State.DebugMsg)
	{
		va_list vl;
		va_start(vl, szMessage);
		const int BUFLEN = 512;
		char buf[BUFLEN];
		strcpy_s(buf, BUFLEN, "gfx_directx: ");
		int emitted = strlen(buf);
		if (srcfile)
			emitted += snprintf(buf + emitted, BUFLEN - emitted, "On line %d in %s: ", linenum, srcfile);
		if (emitted < BUFLEN)
			vsnprintf(buf + emitted, BUFLEN - emitted, szMessage, vl);
		va_end(vl);
		g_State.DebugMsg(errlvl, buf);
	}
}

void ::debugc(ErrorLevel errlvl, const char* szMessage)
{
	debug(errlvl, szMessage);
}

bool Win8OrLater()
{
	OSVERSIONINFO verinfo = {0};
	verinfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	if (GetVersionEx(&verinfo))
		return verinfo.dwPlatformId * 10000 + verinfo.dwMajorVersion * 100 + verinfo.dwMinorVersion >= 20602;
	return false;
}

// Try to translate an HRESULT into a string
const char *gfx::HRESULTString(HRESULT hresult)
{
	const int BUFLEN = 256;
	static char buf[BUFLEN];
	int len = sprintf_s(buf, BUFLEN, "ret=0x%08x ", hresult);

	// Check whether this is a win32 error code
	// Update: Starting in Windows 8, FormatMessage can now print DirectX errors.
	// Before Win8, you had to use DXGetErrorString and link DXERR.LIB, but
	// DXERR.LIB has been removed from the SDK, so won't attempt that.
	// Look up error codes in the DX headers.
	// HRESULTs with FACILITY_ITF (0x8004xxxx) in particular are library/interface specific, not universal.
	// FormatMessage returns wrong error pre-Win8 if hresult is a DirectX error code.
	// DirectX uses HRESULTs with a factility code of _FACD3D (0x876), apparently renamed _FACDD in DirectX11
	if (HRESULT_FACILITY(hresult) == FACILITY_WIN32 || hresult == 0 || Win8OrLater())
	{
		strcat_s(buf, BUFLEN, win_error_str(HRESULT_CODE(hresult)));
	}
	return buf;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Version 1.0 interfaces
D3D g_DirectX;
Window g_Window;


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
	gfx_PresentOld(raw, w, h, 0);
	gfx_PumpMessages();
}

DFI_IMPLEMENT_CDECL(int, gfx_present, Surface* pSurfaceIn, ::RGBPalette* pPalette)
{
	if(pSurfaceIn->format == SF_32bit)
		g_DirectX.present32( pSurfaceIn->pColorData, pSurfaceIn->width, pSurfaceIn->height );
	else
		// FIXME: this crashes if gfx_setpal hasn't been called yet
		g_DirectX.present( pSurfaceIn->pPaletteData, pSurfaceIn->width, pSurfaceIn->height, &gfx::Palette<UINT>(&pPalette->col[0].col, 256) );
	gfx_PumpMessages();

	return 0;
}

DFI_IMPLEMENT_CDECL(void, gfx_setpal, unsigned int *pal)
{
	gfx_PresentOld(0, 0, 0, pal);
	gfx_PumpMessages();
}

DFI_IMPLEMENT_CDECL(int, gfx_screenshot, const char* fname)
{
	return gfx_ScreenShot(fname);
}

// FIXME: this doesn't work when -fullscreen passed on commandline, because
// its recieved before gfx_Initialize
DFI_IMPLEMENT_CDECL(void, gfx_setwindowed, int iswindow)
{
	debug(errInfo, "setwindowed(%d)", iswindow);
	gfx_SendMessage(OM_GFX_SETWINDOWED, iswindow, 0);
}

DFI_IMPLEMENT_CDECL(void, gfx_windowtitle, const char* title)
{
	gfx_SetWindowTitle(title);
}

DFI_IMPLEMENT_CDECL(WindowState*, gfx_getwindowstate)
{
	static WindowState winstate;
	winstate.structsize = WINDOWSTATE_SZ;
	gfx_GetWindowState(0, &winstate);
	return &winstate;
}

DFI_IMPLEMENT_CDECL(int, gfx_setoption, const char* opt, const char* arg)
{
	if(!opt || !arg)
		return 0;
	INPUTDEBUG("gfx_setoption(%s, %s)", opt, arg);
	if(::strcmp(opt, "width") == 0)
		{
			gfx_SendMessage(OM_GFX_SETWIDTH, ::atoi(arg), 0);
		}
	else if(::strcmp(opt, "height") == 0)
		{
			gfx_SendMessage(OM_GFX_SETHEIGHT, ::atoi(arg), 0);
		}
	else if(::strcmp(opt, "z") == 0 || ::strcmp(opt, "zoom") == 0)
		{
			int zoom = ::atoi(arg);
			gfx_SendMessage(OM_GFX_SETCLIENTAREA, 320 * zoom, (void*)(200 * zoom));
		}
	else if(::strcmp(opt, "f") == 0 || ::strcmp(opt, "fullscreen") == 0)
	{
		// NOTE: gfx_directx usually never recieves -f, it's handled in backends.bas
		if(*arg == '0')
			gfx_SendMessage(OM_GFX_SETWINDOWED, 0, 0);
		else
			gfx_SendMessage(OM_GFX_SETWINDOWED, 1, 0);
	}
	else if(::strcmp(opt, "vsync") == 0)
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
	else if(::strcmp(opt, "input-debug") == 0)
	{
		input_debug = true;
		return 1;
	}
	else
		return 0;
	return 2;
}

DFI_IMPLEMENT_CDECL(const char*, gfx_describe_options)
{
	return  "   -width [x]       Sets the width of the window (client area)\n" \
		"   -height [x]      Sets the height of the window (client area)\n" \
		"-z -zoom [1...16]   Scale screen to 1,2, ... up to 16x native size (2x default)\n" \
		"                    The above may NOT be called after -fullscreen\n" \
		"   -vsync [0|1*]    Toggles vsync\n" \
		"-a -aspect [0|1*]   Toggles aspect ratio preservation\n" \
		"-s -smooth [0*|1]   Toggles smooth linear interpolation of display\n" \
		"-ss -screenshot [jpg | bmp | png* | dds | ohr]\n" \
		"                    Sets the screen shot format";
}

DFI_IMPLEMENT_CDECL(void, io_init)
{//there isn't an equivalent message in the new backend
}

DFI_IMPLEMENT_CDECL(void, io_pollkeyevents)
{
	gfx_PresentOld(0,0,0,0);
	gfx_PumpMessages();
}

DFI_IMPLEMENT_CDECL(void, io_keybits, int *keybd)
{
	gfx_GetKeyboard(keybd);
}

DFI_IMPLEMENT_CDECL(void, io_textinput, wchar_t *buffer, int bufferLen)
{
	gfx_GetText(buffer, bufferLen);
}

DFI_IMPLEMENT_CDECL(char*, io_get_clipboard_text)
{
	return WIN_GetClipboardText(g_Window.getWindowHandle());
}

DFI_IMPLEMENT_CDECL(void, io_set_clipboard_text, const char *text)
{
	WIN_SetClipboardText(g_Window.getWindowHandle(), text);
}

DFI_IMPLEMENT_CDECL(int, io_setmousevisibility, CursorVisibility visibility)
{
	gfx_SetCursorVisibility(visibility);
	return 1;
}

DFI_IMPLEMENT_CDECL(void, io_getmouse, int* mx, int* my, int* mwheel, int* mbuttons)
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

DFI_IMPLEMENT_CDECL(int, io_readjoysane, int joynum, unsigned int* button, int* x, int* y)
{
	return gfx_GetJoystick(joynum, x, y, button);
}





/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//Version 2.0 interfaces

HWND g_hWndDlg;
Keyboard g_Keyboard;
Mouse g_Mouse;
Joystick g_Joystick;


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

// Used to receive DebugMsg before gfx_setoption calls (which happen before gfx_Initialize)
DFI_IMPLEMENT_CDECL(void, gfx_setdebugfunc, FnDebug DebugMsg)
{
	g_State.DebugMsg = DebugMsg;
}

DFI_IMPLEMENT_CDECL(int, gfx_Initialize, const GfxInitData *pCreationData)
{
	if(pCreationData == NULL)
		return FALSE;
	if(pCreationData->structsize < GFXINITDATA_SZ)
		return FALSE;

	CoInitializeEx(NULL, COINIT_APARTMENTTHREADED);
	LoadHelpText();

	TCHAR buffer[256] = TEXT("");
	g_State.szWindowIcon = StringToString(buffer, 256, pCreationData->windowicon);
	g_State.DebugMsg = pCreationData->DebugMsg;
	g_State.PostEvent = pCreationData->PostEvent;

	if(g_State.PostEvent == NULL || g_State.DebugMsg == NULL) {
		debug(errError, "Required GfxInitData callbacks missing!");
		return FALSE;
	}
	debug(errInfo, "gfx_Initialize()...");

	HRESULT hr;
	if(FAILED(hr = g_Window.initialize(::GetModuleHandle(MODULENAME), 
	                                   (pCreationData->windowicon ? g_State.szWindowIcon.c_str() : NULL), 
	                                   (WNDPROC)OHRWndProc, g_DirectX.getWindowedSize())))
	{
		debug(errError, "Window initialization failed! %s", HRESULTString(hr));
		return FALSE;
	}

	if( FAILED(g_DirectX.initialize(&g_Window)) )
	{
		g_Window.shutdown();
		gfx_PumpMessages();
		debug(errError, "Failed at d3d initialization!");
		return FALSE;
	}

	if(FAILED(g_Joystick.initialize( g_Window.getAppHandle(), g_Window.getWindowHandle() )))
		debug(errError, "Joystick support failed!");
	else
	{
		debug(errInfo, "Joysticks supported.");

		/* Ask for more detailed WM_DEVICECHANGE messages (WinXP+)
		DEV_BROADCAST_DEVICEINTERFACE notificationFilter = {0};
		notificationFilter.dbcc_devicetype = DBT_DEVTYP_DEVICEINTERFACE;
		notificationFilter.dbcc_size = sizeof(notificationFilter);
		if(!RegisterDeviceNotification( g_Window.getWindowHandle(), &notificationFilter,
						DEVICE_NOTIFY_WINDOW_HANDLE | DEVICE_NOTIFY_ALL_INTERFACE_CLASSES))
			debug(errError, "RegisterDeviceNotification failed");
		*/
	}

	g_Mouse.initialize(&g_DirectX);

	gfx_SetWindowTitle(pCreationData->windowtitle);

	// At this point, the window is still hidden
	g_Window.showWindow();

	debug(errInfo, "Initialization success");
	return TRUE;
}

DFI_IMPLEMENT_CDECL(void, gfx_Shutdown)
{
	debug(errInfo, "gfx_Shutdown()...");
	g_Joystick.shutdown();
	g_DirectX.shutdown();
	g_Window.shutdown();
	gfx_PumpMessages();
	debug(errInfo, "Shutdown complete");
	CoUninitialize();
}

DFI_IMPLEMENT_CDECL(int, gfx_SendMessage, unsigned int msg, unsigned int dwParam, void *pvParam)
{
	debug(errInfo, "gfx_SendMessage %d %d %d", msg, dwParam, (int)pvParam);
	switch(msg)
	{
	case OM_GFX_SETWIDTH:
		g_DirectX.setWindowedSize(SZ(dwParam, g_DirectX.getWindowedSize().cy));
		break;
	case OM_GFX_SETHEIGHT:
		g_DirectX.setWindowedSize(SZ(g_DirectX.getWindowedSize().cx, dwParam));
		break;
	case OM_GFX_SETCLIENTAREA:
		g_DirectX.setWindowedSize(SZ(dwParam, (int)pvParam));
		break;
	case OM_GFX_SETLEFT:
		g_Window.setWindowPosition(dwParam, g_Window.getWindowRect().top);
		break;
	case OM_GFX_SETTOP:
		g_Window.setWindowPosition(g_Window.getWindowRect().left, dwParam);
		break;
	case OM_GFX_SETPOSITION:
		g_Window.setWindowPosition(dwParam, (int)pvParam);
		break;
	case OM_GFX_SETARP:
		g_DirectX.setAspectRatioPreservation(dwParam != FALSE);
		break;
	case OM_GFX_SETWINDOWED:
		g_DirectX.setViewFullscreen(dwParam == FALSE);
		break;
	case OM_GFX_SETSMOOTH:
		g_DirectX.setSmooth(dwParam != FALSE);
		break;
	case OM_GFX_SETVSYNC:
		g_DirectX.setVsyncEnabled(dwParam == FALSE);
		break;
	case OM_GFX_SETSSFORMAT:
		switch(dwParam)
		{
		case 0:
			g_DirectX.setImageFileFormat(D3DXIFF_FORCE_DWORD);
			break;
		case 1:
			g_DirectX.setImageFileFormat(D3DXIFF_JPG);
			break;
		case 2:
			g_DirectX.setImageFileFormat(D3DXIFF_BMP);
			break;
		case 3:
			g_DirectX.setImageFileFormat(D3DXIFF_PNG);
			break;
		case 4:
			g_DirectX.setImageFileFormat(D3DXIFF_DDS);
			break;
		default:
			g_DirectX.setImageFileFormat(D3DXIFF_FORCE_DWORD);
			break;
		}
		break;

	case OM_GFX_GETWIDTH:
		return g_Window.getClientSize().cx;
	case OM_GFX_GETHEIGHT:
		return g_Window.getClientSize().cy;
	case OM_GFX_GETCLIENTAREA:
		return (g_Window.getClientSize().cx << 16) | g_Window.getClientSize().cy;
	case OM_GFX_GETLEFT:
		return g_Window.getWindowRect().left;
	case OM_GFX_GETTOP:
		return g_Window.getWindowRect().top;
	case OM_GFX_GETPOSITION:
		return (g_Window.getWindowRect().left << 16) | g_Window.getWindowRect().top;
	case OM_GFX_GETARP:
		return g_DirectX.isAspectRatioPreserved() ? TRUE : FALSE;
	case OM_GFX_GETWINDOWED:
		return g_DirectX.isViewFullscreen() ? FALSE : TRUE;
	case OM_GFX_GETSMOOTH:
		return g_DirectX.isSmooth() ? TRUE : FALSE;
	case OM_GFX_GETVSYNC:
		return g_DirectX.isVsyncEnabled() ? TRUE : FALSE;
	case OM_GFX_GETSSFORMAT:
		switch(g_DirectX.getImageFileFormat())
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
	return DX_API_VERSION;
}

DFI_IMPLEMENT_CDECL(void, gfx_PresentOld, unsigned char *pSurface, int nWidth, int nHeight, unsigned int *pPalette)
{
	if(pPalette)
		// FIXME: this crashes if gfx_setpal hasn't been called yet
		g_DirectX.present(pSurface, nWidth, nHeight, &gfx::Palette<UINT>(pPalette, 256));
	else
		g_DirectX.present(pSurface, nWidth, nHeight, NULL);
}

DFI_IMPLEMENT_CDECL(int, gfx_ScreenShot, const char *szFileName)
{
	if(!g_DirectX.isScreenShotsActive())
		return FALSE;
	if(szFileName == NULL)
		return FALSE;
	TCHAR buffer[256] = TEXT("");
	StringToString(buffer, 256, szFileName);
	switch(g_DirectX.getImageFileFormat())
	{
	case D3DXIFF_JPG:
		::_tcscat_s(buffer, 256, TEXT(".jpg"));
		break;
	case D3DXIFF_BMP:
		::_tcscat_s(buffer, 256, TEXT(".bmp"));
		break;
	case D3DXIFF_PNG:
		::_tcscat_s(buffer, 256, TEXT(".png"));
		break;
	case D3DXIFF_DDS:
		::_tcscat_s(buffer, 256, TEXT(".dds"));
		break;
	default:
		return FALSE;
	}
	if( FAILED(g_DirectX.screenShot(buffer)) )
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
	//g_Joystick.poll();
	//g_Keyboard.poll();
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
	if(g_Mouse.isClippedCursor() && g_Mouse.isInputLive())
		szTemp += TEXT(" Press 'Scroll Lock' to free mouse");
	else if(g_Mouse.isClippedCursor() && !g_Mouse.isInputLive())
		szTemp += TEXT(" Press 'Scroll Lock' to lock mouse");
	g_Window.setWindowTitle(szTemp.c_str());
}

DFI_IMPLEMENT_CDECL(const char*, gfx_GetWindowTitle)
{
	static char buffer[256] = "";
	return StringToString(buffer, 256, g_State.szWindowTitle.c_str());
}

DFI_IMPLEMENT_CDECL(void, gfx_GetWindowState, int nID, WindowState *pState)
{
	// pState->structsize always >= 2
	// This doesn't detect when the Alt+Tab Task Switcher window is actually in the foreground.
	// gfx_sdl has the same problem on Windows, so the engine needs to work around this anyway.
	HWND hActive = GetForegroundWindow();
	pState->focused = (hActive == g_Window.getWindowHandle()) ? -1 : 0;
	pState->minimised = IsIconic(g_Window.getWindowHandle()) ? -1 : 0;
	if (pState->structsize >= 4)
		pState->fullscreen = g_DirectX.isViewFullscreen() ? -1 : 0;
	if (pState->structsize >= 6)
		pState->mouse_over = g_Mouse.isCursorOverClient() ? -1 : 0;
	if (pState->structsize >= 7)
	{
		SIZE sz = g_Window.getClientSize();
		pState->windowsize.w = sz.cx;
		pState->windowsize.h = sz.cy;
        }
        if (pState->structsize >= 8)
        {
		SIZE res = g_DirectX.getResolution();
		pState->zoom = min(res.cx / 320, res.cy / 200);
	}
	pState->structsize = min(pState->structsize, WINDOWSTATE_SZ);
}

DFI_IMPLEMENT_CDECL(void, gfx_SetCursorVisibility, CursorVisibility visibility)
{
	g_Mouse.setCursorVisibility(visibility);
}

DFI_IMPLEMENT_CDECL(void, gfx_ClipCursor, int left, int top, int right, int bottom)
{
	if(left == -1 && top == -1 && right == -1 && bottom == -1)
		g_Mouse.setClipState(gfx::Mouse::CS_OFF);
	else
	{
		RECT r = {left, top, right, bottom};
		g_Mouse.setClippingRect(&r);
		g_Mouse.setClipState(gfx::Mouse::CS_ON);
	}
	char buffer[256] = "";
	gfx_SetWindowTitle(StringToString(buffer, 256, g_State.szWindowTitle.c_str()));
}

DFI_IMPLEMENT_CDECL(int, gfx_GetKeyboard, int *pKeyboard)
{
	if(pKeyboard == NULL)
		return FALSE;
	g_Keyboard.getOHRScans(pKeyboard);
	return TRUE;
}

// Disable wcsncpy (and _stprintf) warning. Silly VC++, wcscpy_s is NOT a replacement for wcsncpy!
#pragma warning(disable:4996)

DFI_IMPLEMENT_CDECL(void, gfx_GetText, wchar_t *buffer, int bufferLen)
{
	int len = g_Keyboard.getText().length();
	if (len > bufferLen - 1)
		len = bufferLen - 1;
	wcsncpy(buffer, g_Keyboard.getText().c_str(), len);
	g_Keyboard.clearText();
}

DFI_IMPLEMENT_CDECL(int, gfx_GetMouse, int* x, int* y, int* wheel, int* buttons)
{
	*x = g_Mouse.getCursorPos().x;
	*y = g_Mouse.getCursorPos().y;
	*wheel = g_Mouse.getWheel();
	*buttons = g_Mouse.getButtonState().getData();
	return TRUE;
}

DFI_IMPLEMENT_CDECL(int, gfx_SetMouse, int x, int y)
{
	return g_Mouse.setPosition(x, y);
}

DFI_IMPLEMENT_CDECL(int, gfx_GetJoystick, int nDevice, int* x, int* y, unsigned int* buttons)
{
	g_Joystick.poll();
	return g_Joystick.getState(nDevice, *buttons, *x, *y);
}

DFI_IMPLEMENT_CDECL(int, gfx_SetJoystick, int nDevice, int x, int y)
{
	return FALSE; //no support
}

DFI_IMPLEMENT_CDECL(int, gfx_GetJoystickCount)
{
	return g_Joystick.getJoystickCount();
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
	
	if(g_Mouse.processMessage(hWnd, msg, wParam, lParam))
		return 0;
	g_Keyboard.processMessage(hWnd, msg, wParam, lParam);

	switch(msg)
	{
	case WM_KEYDOWN:
		{
			switch(wParam)
			{
			case VK_SCROLL:  // scroll lock: disable mouse input and clipping
				{
					// Technically this is wrong: assumes pushState only called with IS_DEAD
					// (currently true))
					if(!(lParam & 0x40000000)) //key was not pressed before
					{
						if(g_Mouse.isInputLive())
						{
							g_Mouse.pushState(gfx::Mouse::IS_DEAD);
						}
						else
						{
							g_Mouse.popState();
							char buffer[256] = "";
							gfx_SetWindowTitle(StringToString(buffer, 256, g_State.szWindowTitle.c_str()));
						}
					}
				} break;
			default:
				return ::DefWindowProc(hWnd, msg, wParam, lParam);
			}
		} break;
	case WM_SYSKEYDOWN:
	case WM_SYSKEYUP:
		{
			if(!g_State.bDisableSysMsg || wParam == VK_RETURN)
				return ::DefWindowProc(hWnd, msg, wParam, lParam);
		} break;
	case WM_SYSCOMMAND:
		{
			switch(wParam)
			{
			case ID_MENU_OPTIONS:
				{// 'Options' dialog
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
					return DefWindowProc(hWnd, msg, wParam, lParam);
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
						g_DirectX.setViewFullscreen(!g_DirectX.isViewFullscreen());
						g_Mouse.setVideoMode(g_DirectX.isViewFullscreen() ? gfx::Mouse::VM_FULLSCREEN : gfx::Mouse::VM_WINDOWED);
						g_State.PostEvent(eventFullscreened, g_DirectX.isViewFullscreen(), 0);
						if(g_DirectX.isViewFullscreen())
						{
							// Make window topmost
							//SetForegroundWindow(hWnd);
							//LockSetForegroundWindow(LSFW_LOCK);
							SetWindowPos(hWnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);
						}
						else
						{
							// Make window no longer topmost
							//LockSetForegroundWindow(LSFW_UNLOCK);
							SetWindowPos(hWnd, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);
						}
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
				g_Mouse.setVideoMode(gfx::Mouse::VM_WINDOWED);
				g_Mouse.pushState(gfx::Mouse::IS_DEAD);
				if(g_DirectX.isViewFullscreen())
				{
					// Make window no longer topmost
					//LockSetForegroundWindow(LSFW_UNLOCK);
					SetWindowPos(hWnd, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE | SWP_NOMOVE | SWP_NOSIZE);
					ShowWindow(hWnd, SW_MINIMIZE);
		 		}
			}
			else
			{
				g_Mouse.setVideoMode(g_DirectX.isViewFullscreen() ? gfx::Mouse::VM_FULLSCREEN : gfx::Mouse::VM_WINDOWED);
				g_Mouse.popState();
				if(g_DirectX.isViewFullscreen())
				{
					// Make window topmost
					//SetForegroundWindow(hWnd);
					//LockSetForegroundWindow(LSFW_LOCK);
					ShowWindow(hWnd, SW_RESTORE);
					SetWindowPos(hWnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE | SWP_NOSIZE);
				}
			}
			return ::DefWindowProc(hWnd, msg, wParam, lParam);
		} break;
	case WM_ENTERSIZEMOVE:
		{
			g_Mouse.pushState(gfx::Mouse::IS_DEAD);
		} break;
	case WM_EXITSIZEMOVE:
		{
			g_Mouse.popState();
		} break;
	case WM_MOVE:
		{
			g_Mouse.updateClippingRect();
		} break;
	case WM_SIZE:
		{
			// The window size changed, whether for external reason, or because Window::setClientSize/setWindowSize was called
			INPUTDEBUG("WM_SIZE: %d*%d", LOWORD(lParam), HIWORD(lParam));
			::DefWindowProc(hWnd, msg, wParam, lParam);
			if(wParam == SIZE_MINIMIZED)
			{
				g_Mouse.setVideoMode(gfx::Mouse::VM_WINDOWED);
				g_Mouse.pushState(gfx::Mouse::IS_DEAD);
			}
			else
			{
				// r is the new size of the client area
				SIZE r = {LOWORD(lParam), HIWORD(lParam)};
				g_DirectX.setResolution(r);
				g_Mouse.setVideoMode(g_DirectX.isViewFullscreen() ? gfx::Mouse::VM_FULLSCREEN : gfx::Mouse::VM_WINDOWED);
				g_Mouse.updateClippingRect();
				g_Mouse.popState();
			}
		} break;
	case WM_SIZING:
		{
			RECT r = *(RECT*)lParam;  // The drag rectangle
			INPUTDEBUG("WM_SIZING drag size: %d*%d", r.right - r.left, r.bottom - r.top);

			// The window is being resized by the user dragging an edge/corner
			// If they're dragging by a corner, cause the size to snap to nearest multiple of native.
			if(!g_DirectX.isViewFullscreen())
			{
				// Find out how much padding is needed to get the window size from the client area size
				RECT rWindowTest = {0,0,400,400};
				SIZE sPadding = {0,0};
				::AdjustWindowRectEx(&rWindowTest, WS_OVERLAPPEDWINDOW, FALSE, 0);
				sPadding.cx = rWindowTest.right - rWindowTest.left - 400;
				sPadding.cy = rWindowTest.bottom - rWindowTest.top - 400;

				switch(wParam)
				{
				case WMSZ_BOTTOMLEFT:
				case WMSZ_BOTTOMRIGHT:
				case WMSZ_TOPLEFT:
				case WMSZ_TOPRIGHT:
					{
						if(!IsNativeResolutionMultiple(r.right - r.left - sPadding.cx, r.bottom - r.top - sPadding.cy))
						{
							SIZE resolution = CalculateNativeResolutionMultiple(r.right - r.left - sPadding.cx, r.bottom - r.top - sPadding.cy);

							if(wParam == WMSZ_BOTTOMLEFT || wParam == WMSZ_TOPLEFT)
								r.left = r.right - (resolution.cx + sPadding.cx);
							else
								r.right = r.left + (resolution.cx + sPadding.cx);

							if(wParam == WMSZ_BOTTOMLEFT || wParam == WMSZ_BOTTOMRIGHT)
								r.bottom = r.top + (resolution.cy + sPadding.cy);
							else
								r.top = r.bottom - (resolution.cy + sPadding.cy);

							*(RECT*)lParam = r;
						}
					} break;
				}
			}
			::DefWindowProc(hWnd, msg, wParam, lParam);
		} break;
	case WM_NCCALCSIZE:
		{
			// This WM message is only intercepted for debugging.
			// If wParam = 0 then lParam is a RECT*, otherwise it is a NCCALCSIZE_PARAMS*;
			// either way the first sizeof(RECT) bytes is the proposed window rectangle,
			// and it get filled with the corresponding window client area.
			RECT *r = (RECT*)lParam;
			INPUTDEBUG("WM_NCCALCSIZE(%d): window rect size %d*%d", wParam, r->right - r->left, r->bottom - r->top);
			LRESULT ret = ::DefWindowProc(hWnd, msg, wParam, lParam);
			INPUTDEBUG("              ->ret=%x, client rect size %d*%d", ret, r->right - r->left, r->bottom - r->top);
			return ret;
		} break;
	case WM_POWERBROADCAST:
		{
			switch(wParam)
			{
			case PBT_APMRESUMESUSPEND:
			case PBT_APMRESUMECRITICAL:
			case PBT_APMRESUMEAUTOMATIC:
				{
					g_DirectX.initialize(&g_Window);
					g_Joystick.initialize(g_Window.getAppHandle(), g_Window.getWindowHandle());
					g_Mouse.setVideoMode(gfx::Mouse::VM_WINDOWED);
					g_Mouse.updateClippingRect();
				} break;
			case PBT_APMSUSPEND:
				{
					g_DirectX.setViewFullscreen(FALSE);
					g_DirectX.shutdown();
					g_Joystick.shutdown();
				} break;
			default:
				return ::DefWindowProc(hWnd, msg, wParam, lParam);
			}
			return TRUE;
		} break;
	case WM_DEVICECHANGE:
		{
			switch(wParam)
			{
			case DBT_DEVICEARRIVAL:
			case DBT_DEVICEREMOVECOMPLETE:
				// DBT_DEVNODES_CHANGED is the only message sent if RegisterDeviceNotification
				// hasn't been called; a bunch of these messages get sent at once
			case DBT_DEVNODES_CHANGED:
				g_Joystick.delayedRefreshEnumeration();
				break;
			}
			return TRUE;
		} break;
	case WM_CLOSE:
		{
			if(g_State.bClosing)
				return ::DefWindowProc(hWnd, msg, wParam, lParam);
			g_Mouse.setVideoMode(gfx::Mouse::VM_WINDOWED);
			g_Mouse.setInputState(gfx::Mouse::IS_DEAD);
			g_State.PostEvent(eventTerminate, 0, 0);
			g_Mouse.setVideoMode(g_DirectX.isViewFullscreen() ? gfx::Mouse::VM_FULLSCREEN : gfx::Mouse::VM_WINDOWED);
			g_Mouse.popState();
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
	static BOOL bVsyncEnabled, bSmoothEnabled, bARPEnabled, bSysMsgDisabled;

	switch(msg)
	{
	case WM_COMMAND:
		{
			switch(LOWORD(wParam))
			{//control id
			case IDC_OPTIONS_EnableVsync:
				{
					g_DirectX.setVsyncEnabled(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnableVsync));
				} break;
			case IDC_OPTIONS_EnableSmooth:
				{
					g_DirectX.setSmooth(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnableSmooth));
				} break;
			case IDC_OPTIONS_EnablePreserveAspectRatio:
				{
					g_DirectX.setAspectRatioPreservation(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnablePreserveAspectRatio));
				} break;
			case IDC_OPTIONS_DisableSystemMessages:
				{
					g_State.bDisableSysMsg = (BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_DisableSystemMessages));
				} break;
			case IDC_OPTIONS_SetDefaults:
				{//sets defaults
					::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnableVsync, BST_CHECKED);
					::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnableSmooth, BST_UNCHECKED);
					::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnablePreserveAspectRatio, BST_CHECKED);
					::CheckDlgButton(hWndDlg, IDC_OPTIONS_DisableSystemMessages, BST_UNCHECKED);

					g_DirectX.setVsyncEnabled(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnableVsync));
					g_DirectX.setSmooth(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnableSmooth));
					g_DirectX.setAspectRatioPreservation(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnablePreserveAspectRatio));
					g_State.bDisableSysMsg = (BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_DisableSystemMessages));

					if(g_DirectX.isScreenShotsActive())
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
					g_Joystick.refreshEnumeration();
					TCHAR strInfoBuffer[128] = TEXT("");
					::_stprintf_s(strInfoBuffer, 128, TEXT("Re-scan For Joysticks - Count: %d"), g_Joystick.getJoystickCount());
					::SendDlgItemMessage(hWndDlg, IDC_OPTIONS_RefreshJoysticks, WM_SETTEXT, 0, (LPARAM)strInfoBuffer);
				} break;
			case IDOK:
				{//apply all changes and return
					g_DirectX.setVsyncEnabled(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnableVsync));
					g_DirectX.setSmooth(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnableSmooth));
					g_DirectX.setAspectRatioPreservation(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_EnablePreserveAspectRatio));
					g_State.bDisableSysMsg = (BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_DisableSystemMessages));
					if(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_ScrnShotFormats_JPG))
						g_DirectX.setImageFileFormat(D3DXIFF_JPG);
					else if(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_ScrnShotFormats_BMP))
						g_DirectX.setImageFileFormat(D3DXIFF_BMP);
					else if(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_ScrnShotFormats_PNG))
						g_DirectX.setImageFileFormat(D3DXIFF_PNG);
					else if(BST_CHECKED == ::IsDlgButtonChecked(hWndDlg, IDC_OPTIONS_ScrnShotFormats_DDS))
						g_DirectX.setImageFileFormat(D3DXIFF_DDS);
					else
						g_DirectX.setImageFileFormat(D3DXIFF_FORCE_DWORD);
					g_Mouse.popState();
					::DestroyWindow(hWndDlg);
					g_hWndDlg = NULL;
				} break;
			case IDCANCEL:
				{//revert all changes and return
					g_DirectX.setVsyncEnabled(bVsyncEnabled);
					g_DirectX.setSmooth(bSmoothEnabled);
					g_DirectX.setAspectRatioPreservation(bARPEnabled);
					g_State.bDisableSysMsg = bSysMsgDisabled;
					g_Mouse.popState();
					::DestroyWindow(hWndDlg);
					g_hWndDlg = NULL;
				} break;
			default:
				return FALSE;
			}
		} break;
	case WM_INITDIALOG:
		{
			g_Mouse.pushState(gfx::Mouse::IS_DEAD);

			bVsyncEnabled = g_DirectX.isVsyncEnabled() ? TRUE : FALSE;
			bSmoothEnabled = g_DirectX.isSmooth() ? TRUE : FALSE;
			bARPEnabled = g_DirectX.isAspectRatioPreserved() ? TRUE : FALSE;
			bSysMsgDisabled = g_State.bDisableSysMsg;

			::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnableVsync, (g_DirectX.isVsyncEnabled() ? BST_CHECKED : BST_UNCHECKED));
			::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnableSmooth, (g_DirectX.isSmooth() ? BST_CHECKED : BST_UNCHECKED));
			::CheckDlgButton(hWndDlg, IDC_OPTIONS_EnablePreserveAspectRatio, (g_DirectX.isAspectRatioPreserved() ? BST_CHECKED : BST_UNCHECKED));
			::CheckDlgButton(hWndDlg, IDC_OPTIONS_DisableSystemMessages, (g_State.bDisableSysMsg ? BST_CHECKED : BST_UNCHECKED));
			::SendDlgItemMessage(hWndDlg, IDC_OPTIONS_Status, WM_SETTEXT, 0, (LPARAM)g_State.szHelpText.c_str());
			TCHAR strInfoBuffer[128] = TEXT("");
			::_stprintf_s(strInfoBuffer, 128, TEXT("DirectX Backend version: %d.%d.%d API v%d\r\nhttps://hamsterrepublic.com/ohrrpgce"), DX_VERSION_MAJOR, DX_VERSION_MINOR, DX_VERSION_BUILD, DX_API_VERSION);
			::SendDlgItemMessage(hWndDlg, IDC_OPTIONS_Info, WM_SETTEXT, 0, (LPARAM)strInfoBuffer);
			::_stprintf_s(strInfoBuffer, 128, TEXT("Re-scan For Joysticks - Count: %d"), g_Joystick.getJoystickCount());
			::SendDlgItemMessage(hWndDlg, IDC_OPTIONS_RefreshJoysticks, WM_SETTEXT, 0, (LPARAM)strInfoBuffer);
			if(g_DirectX.isScreenShotsActive())
			{
				switch(g_DirectX.getImageFileFormat())
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
